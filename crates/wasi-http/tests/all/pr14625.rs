use http_body_util::{BodyExt, Full, StreamBody};
use hyper::body::{Bytes, Frame};
use std::pin::Pin;
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::task::{Context, Poll};
use std::time::{Duration, Instant};
use wasmtime::component::{Component, Linker, ResourceTable, TaskGroupId};
use wasmtime::{Engine, Result, Store, StoreContextMut};
use wasmtime_wasi::{WasiCtx, WasiCtxView, WasiView};
use wasmtime_wasi_http::handler::{
    HandlerState, Instance, ProxyHandler, ProxyPre, Request, ShouldAccept, WorkerExpiration,
    WorkerState, WorkerStatus,
};
use wasmtime_wasi_http::{WasiBody, WasiHttpCtx, WasiHttpCtxView, WasiHttpView};

struct Host {
    table: ResourceTable,
    wasi: WasiCtx,
    http: WasiHttpCtx,
}

impl WasiView for Host {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

impl WasiHttpView for Host {
    fn http(&mut self) -> WasiHttpCtxView<'_> {
        WasiHttpCtxView {
            ctx: &mut self.http,
            table: &mut self.table,
            hooks: wasmtime_wasi_http::default_hooks(),
        }
    }
}

struct NeverExpire;

impl WorkerExpiration for NeverExpire {
    fn poll(self: Pin<&mut Self>, _: &mut Context<'_>, _: WorkerStatus, _: Instant) -> Poll<()> {
        Poll::Pending
    }
}

struct Worker;

impl WorkerState for Worker {
    type StoreData = Host;
    type RequestData = ();

    fn should_accept_request(&self, _: usize, _: usize) -> ShouldAccept {
        ShouldAccept::Yes
    }

    fn on_request_start(
        &self,
        _: StoreContextMut<'_, Host>,
        _: (),
        _: TaskGroupId,
    ) -> Pin<Box<dyn Future<Output = ()> + 'static + Send + Sync>> {
        Box::pin(std::future::pending())
    }

    fn drop(&self, _: Store<Host>, _: Result<()>) {}
}

struct Handler {
    engine: Engine,
    pre: ProxyPre<Host>,
    instances: AtomicUsize,
}

impl HandlerState for Handler {
    type StoreData = Host;
    type WorkerExpiration = NeverExpire;
    type WorkerState = Worker;

    async fn instantiate(&self) -> Result<Instance<Host, NeverExpire, Worker>> {
        let mut store = Store::new(
            &self.engine,
            Host {
                table: ResourceTable::new(),
                wasi: WasiCtx::builder().build(),
                http: WasiHttpCtx::new(),
            },
        );
        let proxy = self.pre.instantiate_async(&mut store).await?;
        self.instances.fetch_add(1, Relaxed);
        Ok(Instance {
            store,
            proxy,
            view: |h: &mut Host| h.http(),
            expiration: NeverExpire,
            state: Worker,
        })
    }
}

fn handler(path: &str) -> Result<ProxyHandler<Handler>> {
    let engine = test_programs_artifacts::engine(|c| {
        c.wasm_component_model_async(true);
    });
    let component = Component::from_file(&engine, path)?;
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
    Ok(ProxyHandler::new(Handler {
        engine,
        pre,
        instances: AtomicUsize::new(0),
    }))
}

fn req(path: &str, body: WasiBody) -> Request {
    http::Request::builder()
        .method("POST")
        .uri(format!("http://localhost{path}"))
        .body(body)
        .unwrap()
}

const SETTLE: Duration = Duration::from_millis(500);

// p2: A's guest responds, but A's caller drops before observing it. The
// worker is then killed, cutting off B's still-streaming response.
#[tokio::test(flavor = "multi_thread")]
async fn p2_completed_request_abandon_kills_reused_worker() -> Result<()> {
    let handler = handler(test_programs_artifacts::P2_API_PROXY_STREAMING_COMPONENT)?;

    let a_body = Full::new(Bytes::from_static(b"a")).map_err(|e| match e {});
    let mut a = Box::pin(handler.handle((), req("/echo", a_body.boxed_unsync())));
    assert!(futures::poll!(a.as_mut()).is_pending());
    tokio::time::sleep(SETTLE).await;

    let b_body = StreamBody::new(futures::stream::pending::<Result<Frame<Bytes>, _>>());
    let b = handler
        .handle((), req("/echo", b_body.boxed_unsync()))
        .await?;
    assert_eq!(handler.state().instances.load(Relaxed), 1);

    drop(a);
    let mut b_body = b.into_body();
    let frame = tokio::time::timeout(SETTLE, b_body.frame()).await;
    assert!(frame.is_err(), "B ended early: {frame:?}");
    Ok(())
}

// p3: every abandoned request retires its instance instead of reusing it.
#[tokio::test(flavor = "multi_thread")]
async fn p3_abandoned_request_retires_instance() -> Result<()> {
    let handler = handler(test_programs_artifacts::P3_CLI_SERVE_SLEEP_COMPONENT)?;

    for _ in 0..4 {
        let body = Full::new(Bytes::new()).map_err(|e| match e {});
        let r = tokio::time::timeout(SETTLE, handler.handle((), req("/", body.boxed_unsync())));
        assert!(r.await.is_err());
    }
    assert_eq!(handler.state().instances.load(Relaxed), 1);
    Ok(())
}
