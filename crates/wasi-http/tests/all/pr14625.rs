use futures::channel::mpsc;
use http_body::Frame;
use http_body_util::{BodyExt, Full, StreamBody};
use hyper::body::Bytes;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};
use std::time::{Duration, Instant};
use wasmtime::component::{Component, Linker, ResourceTable, TaskGroupId};
use wasmtime::{Config, Engine, Result, Store, StoreContextMut};
use wasmtime_wasi::{WasiCtx, WasiCtxView, WasiView};
use wasmtime_wasi_http::handler::{
    HandlerState, Instance, ProxyHandler, ProxyPre, ShouldAccept, WorkerExpiration, WorkerState,
    WorkerStatus,
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

#[derive(Default)]
struct Log {
    instances: usize,
    starts: Vec<(u64, usize)>,
    drops: Vec<(usize, Result<(), String>)>,
}

struct NeverExpire;

impl WorkerExpiration for NeverExpire {
    fn poll(self: Pin<&mut Self>, _: &mut Context<'_>, _: WorkerStatus, _: Instant) -> Poll<()> {
        Poll::Pending
    }
}

struct WState {
    id: usize,
    max_total: usize,
    max_concurrent: usize,
    log: Arc<Mutex<Log>>,
}

impl WorkerState for WState {
    type StoreData = Host;
    type RequestData = u64;

    fn should_accept_request(&self, concurrent: usize, total: usize) -> ShouldAccept {
        if total >= self.max_total {
            ShouldAccept::Never
        } else if concurrent >= self.max_concurrent {
            ShouldAccept::No
        } else {
            ShouldAccept::Yes
        }
    }

    fn on_request_start(
        &self,
        _: StoreContextMut<'_, Host>,
        req: u64,
        _: TaskGroupId,
    ) -> Pin<Box<dyn Future<Output = ()> + 'static + Send + Sync>> {
        self.log.lock().unwrap().starts.push((req, self.id));
        Box::pin(std::future::pending())
    }

    fn drop(&self, store: Store<Host>, result: Result<()>) {
        self.log
            .lock()
            .unwrap()
            .drops
            .push((self.id, result.map_err(|e| format!("{e:?}"))));
        drop(store);
    }
}

struct HState {
    engine: Engine,
    pre: ProxyPre<Host>,
    max_concurrent: usize,
    log: Arc<Mutex<Log>>,
}

impl HandlerState for HState {
    type StoreData = Host;
    type WorkerExpiration = NeverExpire;
    type WorkerState = WState;

    async fn instantiate(&self) -> Result<Instance<Host, NeverExpire, WState>> {
        let mut store = Store::new(
            &self.engine,
            Host {
                table: ResourceTable::new(),
                wasi: WasiCtx::builder().inherit_stderr().build(),
                http: WasiHttpCtx::new(),
            },
        );
        let proxy = self.pre.instantiate_async(&mut store).await?;
        let id = {
            let mut log = self.log.lock().unwrap();
            log.instances += 1;
            log.instances - 1
        };
        Ok(Instance {
            store,
            proxy,
            view: |h: &mut Host| h.http(),
            expiration: NeverExpire,
            state: WState {
                id,
                max_total: 100,
                max_concurrent: self.max_concurrent,
                log: self.log.clone(),
            },
        })
    }
}

fn handler(path: &str, p3: bool) -> Result<(ProxyHandler<HState>, Arc<Mutex<Log>>)> {
    let mut config = Config::new();
    config.wasm_component_model_async(true);
    let engine = Engine::new(&config)?;
    let component = Component::from_file(&engine, path)?;
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p2::add_to_linker_async(&mut linker)?;
    wasmtime_wasi_http::p2::add_only_http_to_linker_async(&mut linker)?;
    wasmtime_wasi::p3::add_to_linker(&mut linker)?;
    wasmtime_wasi_http::p3::add_to_linker(&mut linker)?;
    let pre = linker.instantiate_pre(&component)?;
    let pre = if p3 {
        ProxyPre::P3(wasmtime_wasi_http::p3::bindings::ServicePre::new(pre)?)
    } else {
        ProxyPre::P2(wasmtime_wasi_http::p2::bindings::ProxyPre::new(pre)?)
    };
    let log = Arc::new(Mutex::new(Log::default()));
    Ok((
        ProxyHandler::new(HState {
            engine,
            pre,
            max_concurrent: if p3 { 16 } else { 1 },
            log: log.clone(),
        }),
        log,
    ))
}

fn req(method: &str, path: &str, body: WasiBody) -> wasmtime_wasi_http::handler::Request {
    http::Request::builder()
        .method(method)
        .uri(format!("http://localhost{path}"))
        .body(body)
        .unwrap()
}

fn full(b: &'static [u8]) -> WasiBody {
    Full::new(Bytes::from_static(b))
        .map_err(|e| match e {})
        .boxed_unsync()
}

const SETTLE: Duration = Duration::from_millis(500);

// p2: the caller of request A drops its `handle` future after the guest has
// already produced A's response (it is sitting in the oneshot), e.g. because
// the client disconnected at that moment. A's slot stays armed in `accepted`
// forever, so the reused worker is killed later, taking request B (an
// unrelated, still-streaming response) down with it.
#[tokio::test(flavor = "multi_thread")]
async fn p2_completed_request_abandon_kills_reused_worker() -> Result<()> {
    let (handler, log) = handler(
        test_programs_artifacts::P2_API_PROXY_STREAMING_COMPONENT,
        false,
    )?;

    // A: completes fully inside the guest, but the caller never polls the
    // result before dropping.
    let mut a = Box::pin(handler.handle(0, req("POST", "/echo", full(b"a"))));
    assert!(futures::poll!(a.as_mut()).is_pending());
    tokio::time::sleep(SETTLE).await;

    // B: streaming echo on the same (reused) instance.
    let (mut body_tx, body_rx) = mpsc::channel::<Result<Frame<Bytes>, wasmtime_wasi_http::Error>>(1);
    let b_body = StreamBody::new(body_rx).boxed_unsync();
    let resp = handler.handle(1, req("POST", "/echo", b_body)).await?;
    let mut resp_body = resp.into_body();
    futures::SinkExt::send(&mut body_tx, Ok(Frame::data(Bytes::from_static(b"hello"))))
        .await
        .unwrap();
    let first = resp_body.frame().await.unwrap().unwrap().into_data().unwrap();
    assert_eq!(&first[..], b"hello");

    // Now drop A, whose guest work finished long ago.
    drop(a);
    tokio::time::sleep(SETTLE).await;

    let _ = futures::SinkExt::send(&mut body_tx, Ok(Frame::data(Bytes::from_static(b"world")))).await;
    let second = tokio::time::timeout(SETTLE, resp_body.frame()).await;

    let log = log.lock().unwrap();
    eprintln!("starts = {:?}", log.starts);
    eprintln!("drops = {:?}", log.drops);
    eprintln!("B second frame = {second:?}");
    assert_eq!(log.starts, vec![(0, 0), (1, 0)], "both on instance 0");
    assert!(log.drops.is_empty(), "worker killed: {:?}", log.drops);
    let frame = second.expect("B timed out").expect("B body ended").expect("B body errored");
    assert_eq!(&frame.into_data().unwrap()[..], b"world");
    Ok(())
}

// p3: one caller dropping its future no longer leaves the instance reusable.
// Every abandoned request now pins a whole instance (the orphaned guest task
// keeps running) and the next request pays for a fresh instantiation.
#[tokio::test(flavor = "multi_thread")]
async fn p3_abandoned_request_retires_instance() -> Result<()> {
    let (handler, log) = handler(test_programs_artifacts::P3_CLI_SERVE_SLEEP_COMPONENT, true)?;

    for i in 0..4 {
        let r = tokio::time::timeout(
            Duration::from_millis(200),
            handler.handle(i, req("GET", "/", full(b""))),
        )
        .await;
        assert!(r.is_err());
        tokio::time::sleep(Duration::from_millis(100)).await;
    }

    let log = log.lock().unwrap();
    eprintln!("instances = {}", log.instances);
    eprintln!("starts = {:?}", log.starts);
    assert_eq!(log.instances, 1, "starts = {:?}", log.starts);
    Ok(())
}
