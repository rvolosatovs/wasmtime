use {
    super::util::config,
    component_async_tests::Ctx,
    wasmtime::{
        Engine, Result, Store,
        component::{Component, Linker, ResourceTable, StreamReader},
    },
    wasmtime_wasi::WasiCtxBuilder,
};

/// A component whose `run` export attempts to `stream.forward` from the
/// readable end it receives as an argument to the writable end of a stream it
/// creates itself.
const FORWARD_FROM_HOST_STREAM: &str = r#"
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run") (param $r.src i32)
      (drop (call $stream.forward
        (local.get $r.src)
        (i32.wrap_i64 (i64.shr_u (call $stream.new) (i64.const 32)))
        (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// `stream.forward` from a host-created stream is rejected with a graceful
/// error rather than a panic.
#[tokio::test]
pub async fn async_forward_host_stream_rejected() -> Result<()> {
    let engine = Engine::new(&config())?;

    let mut store = Store::new(
        &engine,
        Ctx {
            wasi: WasiCtxBuilder::new().inherit_stdio().build(),
            table: ResourceTable::default(),
            continue_: false,
        },
    );

    let component = Component::new(&engine, FORWARD_FROM_HOST_STREAM)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let rx = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;

    let error = match store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await
    {
        Ok(Ok(())) => panic!("expected `stream.forward` from a host-owned stream to fail"),
        Ok(Err(error)) | Err(error) => error,
    };

    let message = format!("{error:?}");
    assert!(
        message.contains("stream.forward involving host-owned streams is not yet supported"),
        "unexpected error: {message}"
    );

    Ok(())
}
