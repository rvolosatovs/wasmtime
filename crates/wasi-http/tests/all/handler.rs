//! Reproducers for `ProxyHandler` behavior when the caller of `handle` drops
//! its future before the guest has produced a response.

use http_body_util::{BodyExt, Empty, Full, StreamBody};
use hyper::body::{Body, Bytes};
use std::pin::{Pin, pin};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::task::{Context, Poll};
use std::time::{Duration, Instant};
use test_programs_artifacts::*;
use tokio::sync::mpsc::{self, UnboundedReceiver, UnboundedSender};
use tokio::time::{Sleep, timeout};
use wasmtime::component::{Component, Linker, ResourceTable, TaskGroupId};
use wasmtime::{Engine, Result, Store, StoreContextMut};
use wasmtime_wasi::{WasiCtx, WasiCtxView, WasiView};
use wasmtime_wasi_http::handler::{
    HandlerState, Instance, ProxyHandler, ProxyPre, Request, ShouldAccept, WorkerExpiration,
    WorkerState, WorkerStatus,
};
use wasmtime_wasi_http::{WasiBody, WasiHttpCtx, WasiHttpCtxView, WasiHttpView};

struct Data {
    wasi: WasiCtx,
    http: WasiHttpCtx,
    table: ResourceTable,
}

impl WasiView for Data {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

impl WasiHttpView for Data {
    fn http(&mut self) -> WasiHttpCtxView<'_> {
        WasiHttpCtxView {
            ctx: &mut self.http,
            table: &mut self.table,
            hooks: Default::default(),
        }
    }
}

/// Worker lifecycle events: `on_request_start` and `drop` notifications.
struct Events {
    started: UnboundedReceiver<()>,
    dropped: UnboundedReceiver<Result<(), String>>,
}

/// Expires a worker `request_timeout` after its most recent request started;
/// never expires an idle one.
struct Expiration {
    request_timeout: Duration,
    sleep: Pin<Box<Sleep>>,
}

impl WorkerExpiration for Expiration {
    fn poll(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        status: WorkerStatus,
        start: Instant,
    ) -> Poll<()> {
        let me = self.get_mut();
        let deadline = match status {
            WorkerStatus::Idle => return Poll::Pending,
            WorkerStatus::Requests | WorkerStatus::PostReturn => {
                match start.checked_add(me.request_timeout) {
                    Some(deadline) => tokio::time::Instant::from_std(deadline),
                    None => return Poll::Pending,
                }
            }
        };
        if deadline != me.sleep.deadline() {
            me.sleep.as_mut().reset(deadline);
        }
        me.sleep.as_mut().poll(cx)
    }
}

struct Worker {
    started: UnboundedSender<()>,
    dropped: UnboundedSender<Result<(), String>>,
    request_timeout: Duration,
}

impl WorkerState for Worker {
    type StoreData = Data;
    type RequestData = ();

    fn should_accept_request(&self, _: usize, _: usize) -> ShouldAccept {
        ShouldAccept::Yes
    }

    fn on_request_start(
        &self,
        _: StoreContextMut<'_, Data>,
        (): (),
        _: TaskGroupId,
    ) -> Pin<Box<dyn Future<Output = ()> + 'static + Send + Sync>> {
        _ = self.started.send(());
        Box::pin(tokio::time::sleep(self.request_timeout))
    }

    fn drop(&self, _: Store<Data>, result: Result<(), wasmtime::Error>) {
        _ = self.dropped.send(result.map_err(|e| format!("{e:?}")));
    }
}

struct Handler {
    engine: Engine,
    pre: ProxyPre<Data>,
    started: UnboundedSender<()>,
    dropped: UnboundedSender<Result<(), String>>,
    request_timeout: Duration,
    instances: AtomicUsize,
}

impl HandlerState for Handler {
    type StoreData = Data;
    type WorkerExpiration = Expiration;
    type WorkerState = Worker;

    async fn instantiate(&self) -> Result<Instance<Data, Expiration, Worker>> {
        let mut store = Store::new(
            &self.engine,
            Data {
                wasi: WasiCtx::builder().build(),
                http: WasiHttpCtx::new(),
                table: ResourceTable::new(),
            },
        );
        let proxy = self.pre.instantiate_async(&mut store).await?;
        self.instances.fetch_add(1, Relaxed);
        Ok(Instance {
            store,
            proxy,
            view: WasiHttpView::http,
            expiration: Expiration {
                request_timeout: self.request_timeout,
                sleep: Box::pin(tokio::time::sleep(Duration::MAX)),
            },
            state: Worker {
                started: self.started.clone(),
                dropped: self.dropped.clone(),
                request_timeout: self.request_timeout,
            },
        })
    }
}

fn handler(component: &str, request_timeout: Duration) -> Result<(ProxyHandler<Handler>, Events)> {
    let engine = test_programs_artifacts::engine(|config| {
        config.wasm_component_model_async(true);
    });
    let component = Component::from_file(&engine, component)?;
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p2::add_to_linker_async(&mut linker)?;
    wasmtime_wasi_http::p2::add_only_http_to_linker_async(&mut linker)?;
    wasmtime_wasi::p3::add_to_linker(&mut linker)?;
    wasmtime_wasi_http::p3::add_to_linker(&mut linker)?;
    let pre = linker.instantiate_pre(&component)?;
    let pre = match wasmtime_wasi_http::p3::bindings::ServicePre::new(pre.clone()) {
        Ok(pre) => ProxyPre::P3(pre),
        Err(_) => ProxyPre::P2(wasmtime_wasi_http::p2::bindings::ProxyPre::new(pre)?),
    };
    let (started, started_rx) = mpsc::unbounded_channel();
    let (dropped, dropped_rx) = mpsc::unbounded_channel();
    let handler = ProxyHandler::new(Handler {
        engine,
        pre,
        started,
        dropped,
        request_timeout,
        instances: AtomicUsize::new(0),
    });
    Ok((
        handler,
        Events {
            started: started_rx,
            dropped: dropped_rx,
        },
    ))
}

fn request(
    path: &str,
    body: impl Body<Data = Bytes, Error = wasmtime_wasi_http::Error> + Send + 'static,
) -> Request {
    http::Request::builder()
        .method(if path == "/echo" { "POST" } else { "GET" })
        .uri(format!("http://localhost{path}"))
        .body(WasiBody::new(body))
        .unwrap()
}

/// Starts a request, waits until a worker has accepted it, then drops the
/// `handle` future, as a hyper connection task does when the client
/// disconnects.
async fn start_and_abandon(handler: &ProxyHandler<Handler>, events: &mut Events) {
    let mut fut = pin!(handler.handle((), request("/", Empty::new().map_err(|e| match e {}))));
    tokio::select! {
        result = &mut fut => panic!("request completed unexpectedly: {result:?}"),
        started = events.started.recv() => assert_eq!(started, Some(())),
    }
}

const SETTLE: Duration = Duration::from_millis(500);

/// A p2 caller that drops after the guest has responded, but before it
/// observed the response, must not tear down the worker, which may by then
/// be serving another request.
#[tokio::test(flavor = "multi_thread")]
async fn p2_drop_after_response_keeps_worker() -> Result<()> {
    let (handler, _events) = handler(P2_API_PROXY_STREAMING_COMPONENT, Duration::MAX)?;
    let a = Full::new(Bytes::from_static(b"a")).map_err(|e| match e {});
    let mut a = pin!(handler.handle((), request("/echo", a)));
    assert!(futures::poll!(a.as_mut()).is_pending());
    tokio::time::sleep(SETTLE).await;

    let b = StreamBody::new(futures::stream::pending());
    let b = handler.handle((), request("/echo", b)).await?;
    assert_eq!(handler.state().instances.load(Relaxed), 1);

    drop(a);
    let mut b = b.into_body();
    let frame = timeout(SETTLE, b.frame()).await;
    assert!(frame.is_err(), "B ended early: {frame:?}");
    Ok(())
}

/// A p2 client disconnect is handled by discarding the instance, which is
/// not a guest trap or host panic, so `WorkerState::drop` should get `Ok(())`.
#[tokio::test(flavor = "multi_thread")]
async fn p2_caller_drop_is_not_a_worker_failure() -> Result<()> {
    let (handler, mut events) = handler(P2_CLI_SERVE_SLEEP_COMPONENT, Duration::from_secs(60))?;
    start_and_abandon(&handler, &mut events).await;
    let result = timeout(Duration::from_secs(5), events.dropped.recv())
        .await
        .expect("worker was not torn down after its only caller dropped");
    assert_eq!(result, Some(Ok(())));
    Ok(())
}

/// A p3 worker keeps serving requests after a caller drops; the abandoned
/// request alone must not retire the instance.
#[tokio::test(flavor = "multi_thread")]
async fn p3_caller_drop_does_not_retire_instance() -> Result<()> {
    let (handler, _events) = handler(P3_CLI_SERVE_SLEEP_COMPONENT, Duration::MAX)?;
    for _ in 0..4 {
        let request = request("/", Empty::new().map_err(|e| match e {}));
        let result = timeout(SETTLE, handler.handle((), request)).await;
        assert!(
            result.is_err(),
            "request completed unexpectedly: {result:?}"
        );
    }
    assert_eq!(handler.state().instances.load(Relaxed), 1);
    Ok(())
}

/// After a p3 caller drops, the guest task it started keeps running, so the
/// request deadline from `on_request_start` must still apply to it: the
/// worker hosting the orphaned task must be torn down once that deadline
/// passes, even while it keeps accepting new requests.
#[tokio::test(flavor = "multi_thread")]
async fn p3_caller_drop_keeps_request_expiration() -> Result<()> {
    let (handler, mut events) = handler(P3_CLI_SERVE_SLEEP_COMPONENT, SETTLE)?;
    let deadline = Instant::now() + 6 * SETTLE;
    while Instant::now() < deadline {
        start_and_abandon(&handler, &mut events).await;
        tokio::time::sleep(SETTLE / 5).await;
    }
    assert!(
        events.dropped.try_recv().is_ok(),
        "no worker was torn down within {} request timeouts; {} instance(s) hosting orphaned tasks",
        6,
        handler.state().instances.load(Relaxed),
    );
    Ok(())
}
