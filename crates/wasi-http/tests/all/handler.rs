//! Reproducers for `ProxyHandler` behavior when the caller of `handle` drops
//! its future before the guest has produced a response.

use http_body_util::{BodyExt, Empty, Full, StreamBody};
use hyper::body::{Body, Bytes};
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
use std::task::{Context, Poll};
use std::time::{Duration, Instant};
use test_programs_artifacts::*;
use tokio::time::timeout;
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

struct NeverExpire;

impl WorkerExpiration for NeverExpire {
    fn poll(self: Pin<&mut Self>, _: &mut Context<'_>, _: WorkerStatus, _: Instant) -> Poll<()> {
        Poll::Pending
    }
}

struct Worker {
    request_timeout: Duration,
    retired: Arc<AtomicUsize>,
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
        Box::pin(tokio::time::sleep(self.request_timeout))
    }

    fn drop(&self, _: Store<Data>, _: Result<()>) {
        self.retired.fetch_add(1, Relaxed);
    }
}

struct Handler {
    engine: Engine,
    pre: ProxyPre<Data>,
    request_timeout: Duration,
    instances: AtomicUsize,
    retired: Arc<AtomicUsize>,
}

impl HandlerState for Handler {
    type StoreData = Data;
    type WorkerExpiration = NeverExpire;
    type WorkerState = Worker;

    async fn instantiate(&self) -> Result<Instance<Data, NeverExpire, Worker>> {
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
            expiration: NeverExpire,
            state: Worker {
                request_timeout: self.request_timeout,
                retired: self.retired.clone(),
            },
        })
    }
}

fn handler(component: &str, request_timeout: Duration) -> Result<ProxyHandler<Handler>> {
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
    Ok(ProxyHandler::new(Handler {
        engine,
        pre,
        request_timeout,
        instances: AtomicUsize::new(0),
        retired: Arc::default(),
    }))
}

fn request(
    path: &str,
    body: impl Body<Data = Bytes, Error = wasmtime_wasi_http::Error> + Send + 'static,
) -> Request {
    http::Request::builder()
        .method("POST")
        .uri(format!("http://localhost{path}"))
        .body(WasiBody::new(body))
        .unwrap()
}

/// Sends `n` requests to a guest that never responds, dropping each `handle`
/// future after `wait`, as a hyper connection task does when the client
/// disconnects. Returns the number of instances created.
async fn abandon(handler: &ProxyHandler<Handler>, n: usize, wait: Duration) -> usize {
    for _ in 0..n {
        let request = request("/", Empty::new().map_err(|e| match e {}));
        let result = timeout(wait, handler.handle((), request)).await;
        assert!(result.is_err(), "request completed: {result:?}");
    }
    handler.state().instances.load(Relaxed)
}

const SETTLE: Duration = Duration::from_millis(500);

/// A p2 caller that drops after the guest has responded, but before it
/// observed the response, must not tear down the worker, which may by then
/// be serving another request.
#[tokio::test(flavor = "multi_thread")]
async fn p2_drop_after_response_keeps_worker() -> Result<()> {
    let handler = handler(P2_API_PROXY_STREAMING_COMPONENT, Duration::MAX)?;
    let a = Full::new(Bytes::from_static(b"a")).map_err(|e| match e {});
    let mut a = Box::pin(handler.handle((), request("/echo", a)));
    _ = futures::poll!(a.as_mut());
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

/// A p2 caller drop must retire the instance running its request.
#[tokio::test(flavor = "multi_thread")]
async fn p2_caller_drop_retires_instance() -> Result<()> {
    let handler = handler(P2_CLI_SERVE_SLEEP_COMPONENT, Duration::MAX)?;
    assert_eq!(abandon(&handler, 1, SETTLE).await, 1);
    tokio::time::sleep(SETTLE).await;
    assert_eq!(handler.state().retired.load(Relaxed), 1);
    Ok(())
}

/// A p3 caller drop alone must not retire the instance.
#[tokio::test(flavor = "multi_thread")]
async fn p3_caller_drop_does_not_retire_instance() -> Result<()> {
    let handler = handler(P3_CLI_SERVE_SLEEP_COMPONENT, Duration::MAX)?;
    assert_eq!(abandon(&handler, 4, SETTLE).await, 1);
    Ok(())
}

/// The guest task of a dropped p3 request keeps running, so its request
/// deadline must still expire and stop the worker from accepting more.
#[tokio::test(flavor = "multi_thread")]
async fn p3_caller_drop_keeps_request_expiration() -> Result<()> {
    let handler = handler(P3_CLI_SERVE_SLEEP_COMPONENT, SETTLE)?;
    assert!(abandon(&handler, 10, SETTLE / 5).await > 1);
    Ok(())
}
