use {
    super::util::config,
    component_async_tests::{
        Ctx,
        util::{PipeConsumer, PipeProducer},
    },
    futures::{SinkExt, StreamExt, channel::mpsc},
    std::{
        pin::Pin,
        task::{Context, Poll},
        time::Duration,
    },
    wasmtime::{
        Engine, Result, Store, StoreContextMut,
        component::{
            Component, Destination, Linker, ResourceTable, StreamProducer, StreamReader,
            StreamResult,
        },
    },
    wasmtime_wasi::WasiCtxBuilder,
};

fn new_store(engine: &Engine) -> Store<Ctx> {
    Store::new(
        engine,
        Ctx {
            wasi: WasiCtxBuilder::new().inherit_stdio().build(),
            table: ResourceTable::default(),
            continue_: false,
        },
    )
}

/// A component whose `run` export forwards from the two host-created streams
/// it receives as arguments into a stream it creates itself, reading the
/// forwarded items back out and checking them.
///
/// Round 1 forwards with a budget matching the producer's four items exactly,
/// expecting `COMPLETED(4)`; round 2 forwards with a larger budget, expecting
/// `DROPPED(4)` once the producer is exhausted.
const FORWARD_FROM_HOST_PRODUCER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func $expect-forward-event (param $w.dst i32) (param $code i32)
      (local $ws i32)
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (local.get $code))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))
    )

    (func (export "run") (param $r.a i32) (param $r.b i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Round 1: nothing is parked on the destination yet, so the forward
      ;; blocks; a subsequent read rendezvouses with the host producer
      ;; through it. The budget matches the producer's four items exactly,
      ;; so the forward completes with COMPLETED(4).
      (if (i32.ne (call $stream.forward (local.get $r.a) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 6))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (i32.const 0x04030201))
        (then unreachable))

      (call $expect-forward-event (local.get $w.dst) (i32.const 0x40))

      ;; Round 2: the budget exceeds the producer's items, so the forward
      ;; ends with DROPPED(4) after delivering all four of them.
      (i32.store (i32.const 8) (i32.const 0))

      (if (i32.ne (call $stream.forward (local.get $r.b) (local.get $w.dst) (i32.const 6))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 6))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (i32.const 0x04030201))
        (then unreachable))

      (call $expect-forward-event (local.get $w.dst) (i32.const 0x41 (; (4<<4) | DROPPED ;)))

      (call $stream.drop-readable (local.get $r.a))
      (call $stream.drop-readable (local.get $r.b))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "a" (stream u8)) (param "b" (stream u8))
    (canon lift (core func $i "run")))
)
"#;

/// `stream.forward` from host-created streams, with the destination read by
/// the forwarding guest itself.
#[tokio::test]
pub async fn async_forward_from_host_producer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func =
        instance.get_typed_func::<(StreamReader<u8>, StreamReader<u8>), ()>(&mut store, "run")?;

    let a = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;
    let b = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (a, b)).await)
        .await??;

    Ok(())
}

/// A component whose `run` export parks an async read on its own stream, then
/// completes a synchronous `stream.forward` from a host-created stream
/// inline.
const SYNC_FORWARD_FROM_HOST_PRODUCER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park an async read on the destination first, then rendezvous with
      ;; the host producer via a sync forward, which completes inline.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 6))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      ;; The read's completion event is already queued.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (if (i32.ne (i32.load (i32.const 8)) (i32.const 0x04030201))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// Synchronous `stream.forward` from a host-created stream completes inline
/// when a read is already parked on the destination.
#[tokio::test]
pub async fn async_sync_forward_from_host_producer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, SYNC_FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let rx = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    Ok(())
}

/// A component whose `produce` export returns the readable end of a stream to
/// the host, and whose `drive` export forwards four items into that stream
/// from a stream it writes itself.
const FORWARD_TO_HOST_CONSUMER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $off i32)
      (local $code i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Start the forward; whether the host has already attached a consumer
      ;; or not, nothing is parked on the source yet, so this blocks.
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Feed the source with sync writes until all four bytes are through;
      ;; each write completes with however many items the host consumer
      ;; accepted.
      (i32.store (i32.const 0) (i32.const 0x04030201))
      (loop $l
        (local.set $code
          (call $stream.write
            (local.get $w1)
            (local.get $off)
            (i32.sub (i32.const 4) (local.get $off))))
        (if (i32.ne (i32.and (local.get $code) (i32.const 0xf))
                    (i32.const 0 (; COMPLETED ;)))
          (then unreachable))
        (local.set $off (i32.add (local.get $off) (i32.shr_u (local.get $code) (i32.const 4))))
        (br_if $l (i32.lt_u (local.get $off) (i32.const 4)))
      )

      ;; Await the forward's completion event.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (global.get $w2) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (global.get $w2))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (global.get $w2) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// `stream.forward` into a stream whose readable end is owned (and consumed)
/// by the host, with the consumer attached only after the forward and a
/// source write are already pending.
#[tokio::test]
pub async fn async_forward_to_host_consumer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_TO_HOST_CONSUMER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (tx, mut rx) = mpsc::channel(4);

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            let (drove, ()) = futures::join!(drive.call_concurrent(accessor, ()), async {
                // By this point `drive` has blocked with the forward pending
                // and a source write parked, so attaching the consumer
                // rendezvouses with them immediately.
                accessor
                    .with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))
                    .unwrap();
                for expected in 1..=4_u8 {
                    assert_eq!(Some(expected), rx.next().await);
                }
                assert!(rx.next().await.is_none());
            });
            drove
        })
        .await??;

    Ok(())
}

/// A component whose `produce` export hands the readable end of a stream to
/// the host and whose `drive` export attempts to `stream.forward` into it
/// from a host-created stream.
const FORWARD_BETWEEN_HOST_ENDS: &str = r#"
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive") (param $r.src i32)
      (drop (call $stream.forward
        (local.get $r.src)
        (global.get $w2)
        (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (param "x" (stream u8))
    (canon lift (core func $i "drive")))
)
"#;

/// `stream.forward` between two host-owned stream ends is rejected with a
/// graceful error rather than a panic.
#[tokio::test]
pub async fn async_forward_between_host_ends_rejected() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_BETWEEN_HOST_ENDS)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "drive")?;

    let rx = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;

    let error = match store
        .run_concurrent(async |accessor| {
            let _reader = produce.call_concurrent(accessor, ()).await?;
            drive.call_concurrent(accessor, (rx,)).await
        })
        .await
    {
        Ok(Ok(())) => panic!("expected `stream.forward` between host-owned ends to fail"),
        Ok(Err(error)) | Err(error) => error,
    };

    let message = format!("{error:?}");
    assert!(
        message.contains("cannot `stream.forward` between two host-owned stream ends"),
        "unexpected error: {message}"
    );

    Ok(())
}

/// A component whose `run` export cancels a `stream.forward` from a
/// host-created stream while the host producer is pending.
const CANCEL_FORWARD_FROM_HOST_PRODUCER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; This read starts a rendezvous with the host producer, which stays
      ;; pending because the producer has nothing to deliver.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Synchronously cancel the forward via the source handle; the
      ;; producer acknowledges promptly and the forward reports
      ;; CANCELLED(0), leaving the read parked.
      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; The read is now an ordinary parked read; cancel it too.
      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// Cancelling a `stream.forward` whose host-produce rendezvous is in flight
/// tears it down cleanly.
#[tokio::test]
pub async fn async_cancel_forward_from_host_producer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    drop(tx);

    Ok(())
}

/// A component whose `start` export forwards from a host-created stream
/// whose producer has nothing to deliver yet, leaving the rendezvous pending
/// in the background, and whose `finish` export collects the results after
/// the host has sent its items.
///
/// Single-item reads force one item per batch, so the forward re-registers
/// between batches: the first batch settles in the background and reaches the
/// reader as an event, while the later reads rendezvous inline.
const FORWARD_FROM_HOST_PRODUCER_BACKGROUND: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $r.src (mut i32) (i32.const 0))
    (global $r.dst (mut i32) (i32.const 0))
    (global $w.dst (mut i32) (i32.const 0))

    (func $expect-event (param $w i32) (param $event i32) (param $code i32)
      (local $ws i32)
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (local.get $event))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (local.get $code))
        (then unreachable))
      (call $waitable.join (local.get $w) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))
    )

    (func (export "start") (param $r i32)
      (local $t64 i64)
      (global.set $r.src (local.get $r))

      (local.set $t64 (call $stream.new))
      (global.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (global.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (global.get $r.src) (global.get $w.dst) (i32.const 3))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The producer has nothing to deliver yet, so the rendezvous stays
      ;; pending and will complete in the background once the host sends
      ;; items.
      (if (i32.ne (call $stream.read (global.get $r.dst) (i32.const 8) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
    )

    (func (export "finish")
      ;; The host has sent its items by now; the first batch settled in the
      ;; background and reaches the parked read as an event.
      (call $expect-event (global.get $r.dst)
        (i32.const 2 (; STREAM_READ ;)) (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 1))
        (then unreachable))

      ;; The remaining items are already buffered, so each re-grafted read
      ;; rendezvouses inline.
      (if (i32.ne (call $stream.read (global.get $r.dst) (i32.const 8) (i32.const 1))
                  (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 2))
        (then unreachable))

      (if (i32.ne (call $stream.read (global.get $r.dst) (i32.const 8) (i32.const 1))
                  (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 3))
        (then unreachable))

      ;; The third item exhausted the forward's budget.
      (call $expect-event (global.get $w.dst)
        (i32.const 7 (; STREAM_FORWARD ;)) (i32.const 0x30 (; (3<<4) | COMPLETED ;)))

      (call $stream.drop-readable (global.get $r.src))
      (call $stream.drop-readable (global.get $r.dst))
      (call $stream.drop-writable (global.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "start") async (param "x" (stream u8)) (canon lift (core func $i "start")))
  (func (export "finish") async (canon lift (core func $i "finish")))
)
"#;

/// A `stream.forward` from a host producer that is not immediately ready
/// completes its rendezvous in the background, re-registering the forward
/// between single-item batches.
#[tokio::test]
pub async fn async_forward_from_host_producer_background() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_FROM_HOST_PRODUCER_BACKGROUND)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let start = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "start")?;
    let finish = instance.get_typed_func::<(), ()>(&mut store, "finish")?;

    let (mut tx, rx) = mpsc::channel(3);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async move |accessor| {
            start.call_concurrent(accessor, (rx,)).await?;
            for i in 1..=3_u8 {
                tx.send(i).await.unwrap();
            }
            finish.call_concurrent(accessor, ()).await
        })
        .await??;

    Ok(())
}

/// A component whose `run` export exercises both cancellation flavors while a
/// host-produce rendezvous is in flight: first the destination's reader
/// cancels its grafted read (which the forward survives), then the forwarding
/// guest cancels the forward via the destination's write handle.
const CANCEL_GRAFTED_READ_THEN_FORWARD: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The reader cancels its own grafted read; the cancellation is routed
      ;; to the producer, and the forward itself survives.
      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; A fresh read re-grafts onto the still-pending forward.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Cancel the forward via the destination's write handle this time; the
      ;; producer acknowledges promptly and the read is left parked.
      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// Cancelling the grafted read of an in-flight host-produce rendezvous is
/// routed as a peer cancellation the forward survives; cancelling via the
/// destination's write handle then tears the forward down.
#[tokio::test]
pub async fn async_cancel_grafted_read_then_forward() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_GRAFTED_READ_THEN_FORWARD)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    drop(tx);

    Ok(())
}

/// A component whose `produce` export hands the readable end of a stream to
/// the host and whose `drive` export forwards into it while the attached host
/// consumer never accepts anything, exercising the zero-budget readiness
/// probe and both cancellation flavors on the consume side.
const CANCEL_FORWARD_TO_HOST_CONSUMER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write, then use a zero-budget forward as a readiness probe:
      ;; the parked write and the host consumer jointly satisfy it without
      ;; either making progress.
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 0))
                  (i32.const 0x0 (; COMPLETED(0) ;)))
        (then unreachable))

      ;; A real forward grafts the parked write onto the consumer, which
      ;; never accepts anything, so the rendezvous stays in flight.
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The writer cancels its own grafted write; the cancellation is routed
      ;; to the consumer, and the forward itself survives.
      (if (i32.ne (call $stream.cancel-write (local.get $w1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; A fresh write re-grafts onto the still-pending forward.
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Cancel the forward via its source handle; the consumer acknowledges
      ;; promptly and the write is left parked.
      (if (i32.ne (call $stream.cancel-read (local.get $r1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.cancel-write (local.get $w1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A host consumer that never accepts items keeps the consume rendezvous in
/// flight; the zero-budget probe completes immediately, a peer cancellation
/// of the grafted write leaves the forward pending, and cancelling via the
/// source handle then tears the forward down.
#[tokio::test]
pub async fn async_cancel_forward_to_host_consumer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_FORWARD_TO_HOST_CONSUMER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// `stream.forward` between a host producer and a destination whose readable
/// end the host has already attached a consumer to is rejected at
/// registration.
#[tokio::test]
pub async fn async_forward_between_host_ends_with_consumer_rejected() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_BETWEEN_HOST_ENDS)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "drive")?;

    let rx = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;
    let (tx, _sink_rx) = mpsc::channel::<u8>(4);

    let error = match store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, (rx,)).await
        })
        .await
    {
        Ok(Ok(())) => panic!("expected `stream.forward` between host-owned ends to fail"),
        Ok(Err(error)) | Err(error) => error,
    };

    let message = format!("{error:?}");
    assert!(
        message.contains("cannot `stream.forward` between two host-owned stream ends"),
        "unexpected error: {message}"
    );

    Ok(())
}

/// A component whose `drive` export registers a `stream.forward` from a
/// host-created stream and returns the destination's readable end to the
/// host, and whose `finish` export collects the forward's terminal event
/// afterwards.
const LIFT_DST_OF_FORWARD_FROM_HOST_PRODUCER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $r.src (mut i32) (i32.const 0))
    (global $w.dst (mut i32) (i32.const 0))

    (func (export "drive") (param $r.src i32) (result i32)
      (local $t64 i64)
      (global.set $r.src (local.get $r.src))
      (local.set $t64 (call $stream.new))
      (global.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (if (i32.ne (call $stream.forward
                    (global.get $r.src)
                    (global.get $w.dst)
                    (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "finish")
      (local $ws i32)
      ;; The destination's readable end is gone, so the forward settled with
      ;; DROPPED(0); collect the event from the destination's write handle.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (global.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (global.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x1 (; DROPPED(0) ;)))
        (then unreachable))
      (call $waitable.join (global.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-readable (global.get $r.src))
      (call $stream.drop-writable (global.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "drive") async (param "x" (stream u8)) (result (stream u8))
    (canon lift (core func $i "drive")))
  (func (export "finish") async (canon lift (core func $i "finish")))
)
"#;

/// Attaching a host consumer to the destination of a pending forward whose
/// source is owned by a host producer is rejected by `set_consumer` rather
/// than splicing two host ends together.
#[tokio::test]
pub async fn async_attach_consumer_to_forward_from_host_producer_rejected() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, LIFT_DST_OF_FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let drive =
        instance.get_typed_func::<(StreamReader<u8>,), (StreamReader<u8>,)>(&mut store, "drive")?;

    let (_tx, src_rx) = mpsc::channel::<u8>(1);
    let src = StreamReader::new(&mut store, PipeProducer::new(src_rx))?;
    let (sink_tx, _sink_rx) = mpsc::channel::<u8>(4);

    let error = match store
        .run_concurrent(async move |accessor| {
            let (reader,) = drive.call_concurrent(accessor, (src,)).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(sink_tx)))
        })
        .await
    {
        Ok(Ok(())) => panic!("expected attaching the consumer to fail"),
        Ok(Err(error)) | Err(error) => error,
    };

    let message = format!("{error:?}");
    assert!(
        message.contains("cannot `stream.forward` between two host-owned stream ends"),
        "unexpected error: {message}"
    );

    Ok(())
}

/// A component whose `run` export uses zero-budget forwards from a host
/// producer as readiness probes, both when a read is already parked on the
/// destination and when the read arrives only after the forward registers.
const FORWARD_FROM_HOST_PRODUCER_PROBE: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; A parked read and the host producer jointly satisfy a zero-budget
      ;; forward at registration, without either making progress.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 0))
                  (i32.const 0x0 (; COMPLETED(0) ;)))
        (then unreachable))
      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; With nothing parked, the zero-budget forward registers; a read then
      ;; satisfies the probe, settling the forward and parking normally.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 0))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x0 (; COMPLETED(0) ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// Zero-budget `stream.forward`s from a host producer act as readiness
/// probes, both completing inline at registration and settling via a later
/// read.
#[tokio::test]
pub async fn async_forward_from_host_producer_probe() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_FROM_HOST_PRODUCER_PROBE)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let rx = StreamReader::new(&mut store, vec![1_u8, 2, 3, 4])?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    Ok(())
}

/// A component whose `run` export parks a read on its own stream, satisfies
/// part of it with a direct write (leaving the accumulated completion event
/// undelivered), then forwards from a ready host producer, expecting the
/// read's completion event to accumulate both batches.
const FORWARD_FROM_HOST_PRODUCER_MERGE: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a read of 10 items on the destination.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 10))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Satisfy three of them directly; this queues STREAM_READ
      ;; COMPLETED(3) on the parked read and re-parks the remainder.
      (i32.store (i32.const 40) (i32.const 0x00030201))
      (if (i32.ne (call $stream.write (local.get $w.dst) (i32.const 40) (i32.const 3))
                  (i32.const 0x30 (; (3<<4) | COMPLETED ;)))
        (then unreachable))

      ;; Forward five more items from the ready host producer; the
      ;; rendezvous completes inline.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 5))
                  (i32.const 0x50 (; (5<<4) | COMPLETED ;)))
        (then unreachable))

      ;; The read's completion event must report both batches, not just the
      ;; forwarded one.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x80 (; (8<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (if (i32.ne (i32.load (i32.const 8)) (i32.const 0x04030201))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x08070605))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// A `stream.forward` rendezvous which completes a partially-satisfied read
/// accumulates the undelivered completion event rather than overwriting it.
#[tokio::test]
pub async fn async_forward_from_host_producer_merges_partial_read() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_FROM_HOST_PRODUCER_MERGE)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let rx = StreamReader::new(&mut store, vec![4_u8, 5, 6, 7, 8])?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    Ok(())
}

/// A component whose `produce` export hands the readable end of a stream to
/// the host and whose `drive` export parks a write, consumes part of it with
/// a direct read (leaving the accumulated completion event undelivered), then
/// forwards into the host consumer, expecting the write's completion event to
/// accumulate both batches.
const FORWARD_TO_HOST_CONSUMER_MERGE: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.read (canon stream.read $s (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write of 10 items on the source.
      (i32.store (i32.const 32) (i32.const 0x04030201))
      (i32.store (i32.const 36) (i32.const 0x08070605))
      (i32.store16 (i32.const 40) (i32.const 0x0A09))
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 32) (i32.const 10))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Consume three of them directly; this queues STREAM_WRITE
      ;; COMPLETED(3) on the parked write and re-parks the remainder.
      (if (i32.ne (call $stream.read (local.get $r1) (i32.const 48) (i32.const 3))
                  (i32.const 0x30 (; (3<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.and (i32.load (i32.const 48)) (i32.const 0xffffff))
                  (i32.const 0x030201))
        (then unreachable))

      ;; Forward into the host consumer, which accepts a single item per
      ;; batch, so the forward stays pending after the first batch settles.
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 5))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The write's completion event must report both batches, not just the
      ;; forwarded one.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w1) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w1))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w1) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      ;; Tear down the still-pending forward, which reports the one item it
      ;; forwarded.
      (if (i32.ne (call $stream.cancel-write (global.get $w2))
                  (i32.const 0x12 (; (1<<4) | CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A `stream.forward` rendezvous which completes a partially-consumed write
/// accumulates the undelivered completion event rather than overwriting it.
#[tokio::test]
pub async fn async_forward_to_host_consumer_merges_partial_write() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_TO_HOST_CONSUMER_MERGE)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (tx, mut rx) = mpsc::channel(8);

    store
        .run_concurrent(async move |accessor| -> Result<()> {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await?;
            assert_eq!(Some(4), rx.next().await);
            assert!(rx.next().await.is_none());
            Ok(())
        })
        .await??;

    Ok(())
}

/// A component whose `drive` export parks a write, consumes part of it with a
/// direct read, then grafts the remainder into an in-flight rendezvous with a
/// host consumer that never accepts anything, checking that the undelivered
/// completion event is not deliverable while the rendezvous holds the write
/// and resurfaces once the forward is cancelled.
const FORWARD_TO_HOST_CONSUMER_STALE_EVENT: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.read (canon stream.read $s (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.poll (canon waitable-set.poll (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.poll" (func $waitable-set.poll (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write of 10 items on the source.
      (i32.store (i32.const 32) (i32.const 0x04030201))
      (i32.store (i32.const 36) (i32.const 0x08070605))
      (i32.store16 (i32.const 40) (i32.const 0x0A09))
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 32) (i32.const 10))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Consume three of them directly; this queues STREAM_WRITE
      ;; COMPLETED(3) on the parked write and re-parks the remainder.
      (if (i32.ne (call $stream.read (local.get $r1) (i32.const 48) (i32.const 3))
                  (i32.const 0x30 (; (3<<4) | COMPLETED ;)))
        (then unreachable))

      ;; Graft the remainder into a rendezvous with the host consumer, which
      ;; never accepts anything, so the rendezvous stays in flight.
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 5))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; While the rendezvous holds the write, its earlier completion event
      ;; must not be deliverable: acting on it would release the write's
      ;; buffer while the consumer is still reading from it.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w1) (local.get $ws))
      (if (i32.ne (call $waitable-set.poll (local.get $ws) (i32.const 16))
                  (i32.const 0 (; NONE ;)))
        (then unreachable))

      ;; Cancel the forward via the source handle; the consumer acknowledges
      ;; promptly and the write is left parked.
      (if (i32.ne (call $stream.cancel-read (local.get $r1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; With the forward gone the deferred completion event is deliverable
      ;; again.
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w1))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x30 (; (3<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w1) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.poll" (func $waitable-set.poll))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A write grafted into an in-flight host-consume rendezvous defers its
/// undelivered completion event until the rendezvous resolves.
#[tokio::test]
pub async fn async_forward_to_host_consumer_defers_stale_write_event() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_TO_HOST_CONSUMER_STALE_EVENT)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// A component whose `run` export parks a read, satisfies part of it with a
/// direct write, then grafts the remainder into an in-flight rendezvous with
/// a host producer that never delivers anything, checking that the
/// undelivered completion event is not deliverable while the rendezvous holds
/// the read and resurfaces once the forward is cancelled.
const FORWARD_FROM_HOST_PRODUCER_STALE_EVENT: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.poll (canon waitable-set.poll (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.poll" (func $waitable-set.poll (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a read of 10 items on the destination.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 10))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Satisfy three of them directly; this queues STREAM_READ
      ;; COMPLETED(3) on the parked read and re-parks the remainder.
      (i32.store (i32.const 40) (i32.const 0x00030201))
      (if (i32.ne (call $stream.write (local.get $w.dst) (i32.const 40) (i32.const 3))
                  (i32.const 0x30 (; (3<<4) | COMPLETED ;)))
        (then unreachable))

      ;; Graft the remainder into a rendezvous with the host producer, which
      ;; never delivers anything, so the rendezvous stays in flight.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 5))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; While the rendezvous holds the read, its earlier completion event
      ;; must not be deliverable: acting on it would release the read's
      ;; buffer while the producer is still writing into it.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.poll (local.get $ws) (i32.const 16))
                  (i32.const 0 (; NONE ;)))
        (then unreachable))

      ;; Cancel the forward via the destination handle; the producer
      ;; acknowledges promptly and the read is left parked.
      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; With the forward gone the deferred completion event is deliverable
      ;; again.
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x30 (; (3<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (if (i32.ne (i32.and (i32.load (i32.const 8)) (i32.const 0xffffff))
                  (i32.const 0x030201))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.poll" (func $waitable-set.poll))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// A read grafted into an in-flight host-produce rendezvous defers its
/// undelivered completion event until the rendezvous resolves.
#[tokio::test]
pub async fn async_forward_from_host_producer_defers_stale_read_event() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, FORWARD_FROM_HOST_PRODUCER_STALE_EVENT)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    drop(tx);

    Ok(())
}

/// A component whose `drive` export registers a `stream.forward` into a
/// stream whose readable end is owned by the host (with a consumer attached
/// but no rendezvous in flight, since nothing is parked on the source) and
/// then cancels it via the destination handle.
const CANCEL_IDLE_FORWARD_TO_HOST_CONSUMER: &str = r#"
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Nothing is parked on the source, so the forward registers without a
      ;; rendezvous.
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; With no rendezvous in flight the cancellation tears the forward
      ;; down immediately, even though the destination's readable end is an
      ;; idle host consumer.
      (if (i32.ne (call $stream.cancel-write (global.get $w2))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// Cancelling a registered `stream.forward` whose destination's readable end
/// is an idle host consumer tears the forward down rather than blocking on a
/// rendezvous that was never started.
#[tokio::test]
pub async fn async_cancel_idle_forward_to_host_consumer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_IDLE_FORWARD_TO_HOST_CONSUMER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (tx, rx) = mpsc::channel::<u8>(4);

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// A component whose `run` export registers a `stream.forward` from a
/// host-created stream (whose producer is idle, since nothing is parked on
/// the destination) and then cancels it via the source handle.
const CANCEL_IDLE_FORWARD_FROM_HOST_PRODUCER: &str = r#"
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Nothing is parked on the destination, so the forward registers
      ;; without a rendezvous.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; With no rendezvous in flight the cancellation tears the forward
      ;; down immediately, even though the source's writable end is an idle
      ;; host producer.
      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// Cancelling a registered `stream.forward` whose source's writable end is an
/// idle host producer tears the forward down rather than blocking on a
/// rendezvous that was never started.
#[tokio::test]
pub async fn async_cancel_idle_forward_from_host_producer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_IDLE_FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    drop(tx);

    Ok(())
}

/// A component whose `drive` export cancels an in-flight host-consume
/// rendezvous asynchronously and then, once the cancellation has settled in
/// the background, issues a second cancellation request to collect the
/// queued result.
const DOUBLE_CANCEL_FORWARD_TO_HOST_CONSUMER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $thread.yield (canon thread.yield))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "thread.yield" (func $thread.yield (result i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $i i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write, then graft it into a rendezvous with the host
      ;; consumer, which never accepts anything.
      (i32.store (i32.const 32) (i32.const 0x04030201))
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 32) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Request cancellation asynchronously; the consumer acknowledges in
      ;; the background.
      (if (i32.ne (call $stream.cancel-write (global.get $w2))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Let the background rendezvous settle and queue its event.
      (loop $l
        (drop (call $thread.yield))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br_if $l (i32.lt_u (local.get $i) (i32.const 3)))
      )

      ;; A second cancellation request must report the settled result rather
      ;; than fail.
      (if (i32.ne (call $stream.cancel-write (global.get $w2))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; The writer's parked write survived the forward's cancellation.
      (if (i32.ne (call $stream.cancel-write (local.get $w1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "thread.yield" (func $thread.yield))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A second `stream.cancel-write` issued after an async forward cancellation
/// has settled in the background reports the queued `CANCELLED` result.
#[tokio::test]
pub async fn async_double_cancel_forward_to_host_consumer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, DOUBLE_CANCEL_FORWARD_TO_HOST_CONSUMER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// A component whose `drive` export awaits an in-flight forward's completion
/// via the destination handle in a waitable set, per the usual pattern, and
/// synchronously cancels the forward via the source handle at the same time.
const SYNC_CANCEL_FORWARD_SOURCE_WATCHED_DST: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.poll (canon waitable-set.poll (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.poll" (func $waitable-set.poll (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write, then graft it into a rendezvous with the host
      ;; consumer, which never accepts anything.
      (i32.store (i32.const 32) (i32.const 0x04030201))
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 32) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Await the forward's completion via the destination handle, per the
      ;; usual pattern.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (global.get $w2) (local.get $ws))

      ;; A synchronous source-side cancellation resolves via the source
      ;; handle, so it must not conflict with the destination handle's
      ;; waitable-set membership.
      (if (i32.ne (call $stream.cancel-read (local.get $r1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; The source-side cancellation consumed the forward's outcome, so no
      ;; event is delivered on the destination handle.
      (if (i32.ne (call $waitable-set.poll (local.get $ws) (i32.const 16))
                  (i32.const 0 (; NONE ;)))
        (then unreachable))
      (call $waitable.join (global.get $w2) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      ;; The writer's parked write survived the forward's cancellation.
      (if (i32.ne (call $stream.cancel-write (local.get $w1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.poll" (func $waitable-set.poll))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A synchronous source-side cancellation of an in-flight forward does not
/// trap when the destination handle is joined to a waitable set awaiting the
/// forward's completion.
#[tokio::test]
pub async fn async_sync_cancel_forward_source_with_watched_destination() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, SYNC_CANCEL_FORWARD_SOURCE_WATCHED_DST)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// A component whose `drive` export cancels an in-flight host-consume
/// rendezvous asynchronously via the source handle and expects the
/// completion event on that same handle.
const ASYNC_CANCEL_FORWARD_SOURCE_CONSUME: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write, then graft it into a rendezvous with the host
      ;; consumer, which never accepts anything.
      (i32.store (i32.const 32) (i32.const 0x04030201))
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 32) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Request cancellation via the source handle; the consumer
      ;; acknowledges in the background.
      (if (i32.ne (call $stream.cancel-read (local.get $r1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The completion event arrives on the cancelled handle itself, not on
      ;; the destination handle.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $r1) (local.get $ws))
      (call $waitable.join (global.get $w2) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $r1))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r1) (i32.const 0))
      (call $waitable.join (global.get $w2) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      ;; The writer's parked write survived the forward's cancellation.
      (if (i32.ne (call $stream.cancel-write (local.get $w1))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// An async source-side cancellation of an in-flight host-consume rendezvous
/// delivers its completion event on the cancelled source handle.
#[tokio::test]
pub async fn async_cancel_forward_source_event_on_source_consume() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, ASYNC_CANCEL_FORWARD_SOURCE_CONSUME)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// A component whose `run` export cancels an in-flight host-produce
/// rendezvous asynchronously via the source handle and expects the
/// completion event on that same handle.
const ASYNC_CANCEL_FORWARD_SOURCE_PRODUCE: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
  (core func $stream.cancel-read-sync (canon stream.cancel-read $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-read-sync" (func $stream.cancel-read-sync (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a read, then graft it into a rendezvous with the host
      ;; producer, which never delivers anything.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Request cancellation via the source handle; the producer
      ;; acknowledges in the background.
      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The completion event arrives on the cancelled handle itself, not on
      ;; the destination handle.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $r.src) (local.get $ws))
      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $r.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.src) (i32.const 0))
      (call $waitable.join (local.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      ;; The reader's parked read survived the forward's cancellation.
      (if (i32.ne (call $stream.cancel-read-sync (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-read-sync" (func $stream.cancel-read-sync))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// An async source-side cancellation of an in-flight host-produce rendezvous
/// delivers its completion event on the cancelled source handle.
#[tokio::test]
pub async fn async_cancel_forward_source_event_on_source_produce() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, ASYNC_CANCEL_FORWARD_SOURCE_PRODUCE)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    drop(tx);

    Ok(())
}

/// A component whose `run` export async-cancels an in-flight host-produce
/// rendezvous via both of the forward's handles, then collects a terminal
/// `STREAM_FORWARD` event on each of them.
const DOUBLE_CANCEL_FORWARD_FROM_HOST_PRODUCER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func $expect-forward-event (param $w i32) (param $code i32)
      (local $ws i32)
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (local.get $code))
        (then unreachable))
      (call $waitable.join (local.get $w) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))
    )

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; This read starts a rendezvous with the host producer, which stays
      ;; pending because the producer has nothing to deliver.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 8) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Async-cancel the forward via both handles; each cancel blocks until
      ;; the producer acknowledges.
      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Each cancelled handle receives its own terminal event.
      (call $expect-forward-event (local.get $w.dst) (i32.const 0x2 (; CANCELLED(0) ;)))
      (call $expect-forward-event (local.get $r.src) (i32.const 0x2 (; CANCELLED(0) ;)))

      ;; The read survived the cancellation and is parked again.
      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "x" (stream u8)) (canon lift (core func $i "run")))
)
"#;

/// Async-cancelling an in-flight host-produce rendezvous via both of the
/// forward's handles delivers a terminal event on each of them rather than
/// losing the first request.
#[tokio::test]
pub async fn async_double_cancel_forward_from_host_producer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, DOUBLE_CANCEL_FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (rx,)).await)
        .await??;

    drop(tx);

    Ok(())
}

/// A component whose callback-lifted `wait` export registers a
/// `stream.forward` and waits for its completion event on the destination's
/// write handle, and whose `cancel` export cancels that forward from a
/// sibling task via the source handle.
const CANCEL_FORWARD_FROM_SIBLING_TASK: &str = r#"
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))
  (core func $task.return (canon task.return))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))
    (import "" "task.return" (func $task.return))

    (global $r.src (mut i32) (i32.const 0))
    (global $w.src (mut i32) (i32.const 0))
    (global $r.dst (mut i32) (i32.const 0))
    (global $w.dst (mut i32) (i32.const 0))
    (global $ws (mut i32) (i32.const 0))

    (func (export "wait") (result i32)
      (local $t64 i64)

      (local.set $t64 (call $stream.new))
      (global.set $r.src (i32.wrap_i64 (local.get $t64)))
      (global.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (global.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (global.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (global.get $r.src) (global.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Wait for the forward's terminal event; the sibling task's
      ;; cancellation must deliver it here rather than stranding this task.
      (global.set $ws (call $waitable-set.new))
      (call $waitable.join (global.get $w.dst) (global.get $ws))
      (i32.or (i32.shl (global.get $ws) (i32.const 4)) (i32.const 2 (; WAIT ;)))
    )

    (func (export "cb") (param $event i32) (param $w i32) (param $code i32) (result i32)
      (if (i32.ne (local.get $event) (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (local.get $w) (global.get $w.dst))
        (then unreachable))
      (if (i32.ne (local.get $code) (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))

      (call $waitable.join (global.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (global.get $ws))

      (call $stream.drop-readable (global.get $r.src))
      (call $stream.drop-writable (global.get $w.src))
      (call $stream.drop-readable (global.get $r.dst))
      (call $stream.drop-writable (global.get $w.dst))

      (call $task.return)
      (i32.const 0 (; EXIT ;))
    )

    (func (export "cancel")
      (if (i32.ne (call $stream.cancel-read (global.get $r.src))
                  (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
      (export "task.return" (func $task.return))
    ))
  ))

  (func (export "wait") async
    (canon lift (core func $i "wait") async (callback (core func $i "cb"))))
  (func (export "cancel") async (canon lift (core func $i "cancel")))
)
"#;

/// Cancelling a pending forward from a sibling task while another task is
/// waiting on the destination's write handle for the forward's completion
/// delivers the `CANCELLED` event to the waiter instead of stranding it.
#[tokio::test]
pub async fn async_cancel_forward_from_sibling_task() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_FORWARD_FROM_SIBLING_TASK)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let wait = instance.get_typed_func::<(), ()>(&mut store, "wait")?;
    let cancel = instance.get_typed_func::<(), ()>(&mut store, "cancel")?;

    store
        .run_concurrent(async |accessor| {
            let (waited, cancelled) = futures::join!(wait.call_concurrent(accessor, ()), async {
                // Give the `wait` task time to register the forward and start
                // waiting before cancelling.
                component_async_tests::util::yield_times(64).await;
                cancel.call_concurrent(accessor, ()).await
            });
            waited.and(cancelled)
        })
        .await??;

    Ok(())
}

/// Attaching a host consumer to the destination of a pending forward whose
/// source is owned by a host producer fails, and the failure settles the
/// forward with `DROPPED` rather than leaving it registered forever.
#[tokio::test]
pub async fn async_attach_consumer_to_forward_from_host_producer_settles_forward() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, LIFT_DST_OF_FORWARD_FROM_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let drive =
        instance.get_typed_func::<(StreamReader<u8>,), (StreamReader<u8>,)>(&mut store, "drive")?;
    let finish = instance.get_typed_func::<(), ()>(&mut store, "finish")?;

    let (_tx, src_rx) = mpsc::channel::<u8>(1);
    let src = StreamReader::new(&mut store, PipeProducer::new(src_rx))?;
    let (sink_tx, _sink_rx) = mpsc::channel::<u8>(4);

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = drive.call_concurrent(accessor, (src,)).await?;
            let error = accessor
                .with(|mut store| reader.pipe(&mut store, PipeConsumer::new(sink_tx)))
                .expect_err("expected attaching the consumer to fail");
            let message = format!("{error:?}");
            assert!(
                message.contains("cannot `stream.forward` between two host-owned stream ends"),
                "unexpected error: {message}"
            );
            finish.call_concurrent(accessor, ()).await
        })
        .await??;

    Ok(())
}

/// A component whose `produce` export hands the readable end of a stream to
/// the host and whose `drive` export leaves a grafted write's async
/// cancellation to settle in the background, then observes its queued result
/// through another `stream.cancel-write`.
const CANCEL_GRAFTED_WRITE_IN_BACKGROUND: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s))
  (core func $stream.cancel-write (canon stream.cancel-write $s))
  (core func $stream.cancel-write-async (canon stream.cancel-write $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.cancel-write-async" (func $stream.cancel-write-async (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a write, then graft it onto the host consumer, which never
      ;; accepts anything, so the rendezvous stays in flight.
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Async-cancel the grafted write; the request is routed to the
      ;; consumer and blocks until it acknowledges.
      (if (i32.ne (call $stream.cancel-write-async (local.get $w1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Sync-cancel the forward via its source handle; while this waits,
      ;; the consumer acknowledges both cancellations, queueing the write's
      ;; CANCELLED(0) completion on its handle.
      (if (i32.ne (call $stream.cancel-read (local.get $r1))
                  (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))

      ;; A second cancel finds the queued completion and reports it.
      (if (i32.ne (call $stream.cancel-write (local.get $w1))
                  (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.cancel-write-async" (func $stream.cancel-write-async))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A `stream.cancel-write` that finds the queued `CANCELLED` completion of an
/// earlier async cancellation reports it instead of trapping.
#[tokio::test]
pub async fn async_cancel_grafted_write_in_background() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_GRAFTED_WRITE_IN_BACKGROUND)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            drive.call_concurrent(accessor, ()).await
        })
        .await??;

    drop(rx);

    Ok(())
}

/// A component whose `produce` export hands the readable end of a stream to
/// the host and whose `drive` export completes a synchronous `stream.forward`
/// into it, blocking until the host consumer becomes ready.
const SYNC_FORWARD_TO_HOST_CONSUMER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (global $w2 (mut i32) (i32.const 0))

    (func (export "produce") (result i32)
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (global.set $w2 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))
      (i32.wrap_i64 (local.get $t64))
    )

    (func (export "drive")
      (local $t64 i64)
      (local $r1 i32)
      (local $w1 i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r1 (i32.wrap_i64 (local.get $t64)))
      (local.set $w1 (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a single-item write, then forward it synchronously; the host
      ;; consumer is not ready yet, so the rendezvous completes in the
      ;; background and must wake this task when it settles.
      (i32.store8 (i32.const 0) (i32.const 42))
      (if (i32.ne (call $stream.write (local.get $w1) (i32.const 0) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.forward (local.get $r1) (global.get $w2) (i32.const 1))
                  (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
        (then unreachable))

      ;; The write's completion event is already queued.
      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w1) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w1))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w1) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-readable (local.get $r1))
      (call $stream.drop-writable (local.get $w1))
      (call $stream.drop-writable (global.get $w2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "produce") async (result (stream u8))
    (canon lift (core func $i "produce")))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// A synchronous `stream.forward` into a host consumer that only becomes
/// ready later blocks until the background rendezvous settles, then reports
/// `COMPLETED`.
#[tokio::test]
pub async fn async_sync_forward_to_host_consumer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, SYNC_FORWARD_TO_HOST_CONSUMER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let produce = instance.get_typed_func::<(), (StreamReader<u8>,)>(&mut store, "produce")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    let (mut tx, mut rx) = mpsc::channel(0);
    tx.try_send(9_u8).unwrap();

    store
        .run_concurrent(async move |accessor| {
            let (reader,) = produce.call_concurrent(accessor, ()).await?;
            accessor.with(|mut store| reader.pipe(&mut store, PipeConsumer::new(tx)))?;
            let (drove, ()) = futures::join!(drive.call_concurrent(accessor, ()), async {
                // By this point `drive` is sync-blocked in its forward with
                // the consumer pending; draining the channel lets the
                // consumer accept the item and settle the rendezvous.
                assert_eq!(Some(9), rx.next().await);
                assert_eq!(Some(42), rx.next().await);
                assert!(rx.next().await.is_none());
            });
            drove
        })
        .await??;

    Ok(())
}

/// A component whose callback-lifted `wait` export registers a
/// `stream.forward` from a host-owned source into a stream it creates itself
/// and then waits on the destination's write handle for the forward's
/// terminal event, and whose `drive` export parks a read on that destination
/// — grafting it into a host-produce rendezvous — and then cancels the
/// forward from a sibling task via the source handle.
const CANCEL_GRAFTED_FORWARD_FROM_SIBLING_TASK: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))
  (core func $task.return (canon task.return))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))
    (import "" "task.return" (func $task.return))

    (global $r.src (mut i32) (i32.const 0))
    (global $r.dst (mut i32) (i32.const 0))
    (global $w.dst (mut i32) (i32.const 0))
    (global $ws (mut i32) (i32.const 0))

    (func (export "wait") (param $r.src i32) (result i32)
      (local $t64 i64)

      (global.set $r.src (local.get $r.src))

      (local.set $t64 (call $stream.new))
      (global.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (global.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (global.get $r.src) (global.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Wait for the forward's terminal event.  The sibling task's
      ;; cancellation must deliver it here rather than stranding this task.
      (global.set $ws (call $waitable-set.new))
      (call $waitable.join (global.get $w.dst) (global.get $ws))
      (i32.or (i32.shl (global.get $ws) (i32.const 4)) (i32.const 2 (; WAIT ;)))
    )

    (func (export "cb") (param $event i32) (param $w i32) (param $code i32) (result i32)
      (if (i32.ne (local.get $event) (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (local.get $w) (global.get $w.dst))
        (then unreachable))
      (if (i32.ne (local.get $code) (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))

      (call $waitable.join (global.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (global.get $ws))

      (call $stream.drop-readable (global.get $r.src))
      (call $stream.drop-readable (global.get $r.dst))
      (call $stream.drop-writable (global.get $w.dst))

      (call $task.return)
      (i32.const 0 (; EXIT ;))
    )

    (func (export "drive")
      (local $ws2 i32)

      ;; Park a read on the destination: it is grafted into the host-owned
      ;; source, starting a host-produce rendezvous that stays in flight
      ;; while the producer is pending.
      (if (i32.ne (call $stream.read (global.get $r.dst) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Cancel the forward via the source handle while that rendezvous is in
      ;; flight.  The terminal event owed to the task waiting on the
      ;; destination must still be delivered.
      (if (i32.ne (call $stream.cancel-read (global.get $r.src))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (local.set $ws2 (call $waitable-set.new))
      (call $waitable.join (global.get $r.src) (local.get $ws2))
      (if (i32.ne (call $waitable-set.wait (local.get $ws2) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (global.get $r.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))
      (call $waitable.join (global.get $r.src) (i32.const 0))
      (call $waitable-set.drop (local.get $ws2))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
      (export "task.return" (func $task.return))
    ))
  ))

  (func (export "wait") async (param "src" (stream u8))
    (canon lift (core func $i "wait") async (callback (core func $i "cb"))))
  (func (export "drive") async (canon lift (core func $i "drive")))
)
"#;

/// Cancelling a forward via the source handle while a host-produce rendezvous
/// is in flight must still deliver the forward's terminal event to a task
/// waiting on the destination's write handle.  `cancel_read`'s graft branch
/// routes the event to the source unconditionally, so the waiter is stranded
/// and its destination handle is released out from under it.
#[tokio::test]
pub async fn async_cancel_grafted_forward_from_sibling_task() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_GRAFTED_FORWARD_FROM_SIBLING_TASK)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let wait = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "wait")?;
    let drive = instance.get_typed_func::<(), ()>(&mut store, "drive")?;

    // A producer that never yields an item, so the rendezvous the read grafts
    // into stays in flight until the cancellation finishes it.
    let (tx, rx) = mpsc::channel::<u8>(1);
    let rx = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    // A stranded waiter shows up as a future that never resolves, so bound
    // the run rather than hanging the suite.
    tokio::time::timeout(
        Duration::from_secs(30),
        store.run_concurrent(async |accessor| {
            let (waited, drove) = futures::join!(wait.call_concurrent(accessor, (rx,)), async {
                // Give `wait` time to register the forward before driving it.
                component_async_tests::util::yield_times(64).await;
                drive.call_concurrent(accessor, ()).await
            });
            waited.and(drove)
        }),
    )
    .await
    .expect("task waiting for the forward's terminal event was stranded")??;

    drop(tx);

    Ok(())
}

/// A producer that has an item in hand but withholds it until the rendezvous
/// it is part of is finishing, at which point it delivers the item and
/// reports completion.  This makes a batch that both satisfies a forward's
/// remaining budget and resolves a cancellation land in a single poll.
struct FlushOnFinishProducer(Option<u8>);

impl<D> StreamProducer<D> for FlushOnFinishProducer {
    type Item = u8;
    type Buffer = Option<u8>;

    fn poll_produce<'a>(
        mut self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        _: StoreContextMut<D>,
        mut destination: Destination<'a, Self::Item, Self::Buffer>,
        finish: bool,
    ) -> Poll<Result<StreamResult>> {
        if !finish {
            // The runtime records the cancellation waker for us.
            return Poll::Pending;
        }
        match self.0.take() {
            Some(item) => {
                destination.set_buffer(Some(item));
                Poll::Ready(Ok(StreamResult::Completed))
            }
            None => Poll::Ready(Ok(StreamResult::Dropped)),
        }
    }
}

/// A component whose `run` export forwards a single item from a host-owned
/// source, cancels the forward while the host-produce rendezvous is in
/// flight, and observes the result of the batch that both delivers the last
/// item of the budget and resolves the cancellation.
const CANCEL_RESOLVED_BY_COMPLETING_BATCH: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-write (canon stream.cancel-write $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))
  (core func $waitable-set.new (canon waitable-set.new))
  (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "m"))))
  (core func $waitable-set.drop (canon waitable-set.drop))
  (core func $waitable.join (canon waitable.join))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
    (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
    (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
    (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
    (import "" "waitable.join" (func $waitable.join (param i32 i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; A budget of exactly one item, which the producer's single flush
      ;; satisfies in full.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Park a read on the destination to start the rendezvous; the producer
      ;; withholds its item, so it stays in flight.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Cancelling makes the producer flush.  That batch fills the forward's
      ;; entire budget, so the forward completed: an exhausted budget outranks
      ;; a cancellation that had not yet taken effect.
      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      ;; The forwarded item lands in the parked read.
      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 16))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 20)) (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))
      (if (i32.ne (i32.load8_u (i32.const 0)) (i32.const 42))
        (then unreachable))

      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-write" (func $stream.cancel-write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))
  ))

  (func (export "run") async (param "src" (stream u8))
    (canon lift (core func $i "run")))
)
"#;

/// A rendezvous batch that both exhausts a forward's budget and resolves a
/// pending cancellation completes the forward.  `settle_forward_rendezvous`
/// tests the cancellation flag before the budget-exhausted arm, so it reports
/// `CANCELLED(1)` where the rule documented alongside `cancel_write` calls for
/// `COMPLETED(1)`.
#[tokio::test]
pub async fn async_cancel_resolved_by_completing_batch() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, CANCEL_RESOLVED_BY_COMPLETING_BATCH)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    let src = StreamReader::new(&mut store, FlushOnFinishProducer(Some(42)))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (src,)).await)
        .await??;

    Ok(())
}

/// A component whose `run` export probes a host-owned source with a
/// zero-length `stream.forward` while the producer has nothing to give.
const ZERO_LENGTH_FORWARD_PROBES_HOST_PRODUCER: &str = r#"
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (func (export "run") (param $r.src i32)
      (local $t64 i64)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Park a read on the destination so the destination side is ready and
      ;; only the host-owned source's readiness is in question.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; The producer is pending, so it is not ready to hand anything over.
      ;; A zero-length probe must say so rather than reporting readiness
      ;; merely because a producer is attached: the equivalent zero-length
      ;; `stream.read` on this same source polls the producer and blocks.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 0))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED(0) ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-readable (local.get $r.dst))
      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (param "src" (stream u8))
    (canon lift (core func $i "run")))
)
"#;

/// A zero-length `stream.forward` against a host-owned source reports
/// readiness purely because a producer is attached, without polling it, where
/// the equivalent zero-length `stream.read` polls and blocks.
#[tokio::test]
pub async fn async_zero_length_forward_probes_host_producer() -> Result<()> {
    let engine = Engine::new(&config())?;
    let mut store = new_store(&engine);

    let component = Component::new(&engine, ZERO_LENGTH_FORWARD_PROBES_HOST_PRODUCER)?;
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;
    let func = instance.get_typed_func::<(StreamReader<u8>,), ()>(&mut store, "run")?;

    // Never yields an item, and never reports itself done.
    let (tx, rx) = mpsc::channel::<u8>(1);
    let src = StreamReader::new(&mut store, PipeProducer::new(rx))?;

    store
        .run_concurrent(async |accessor| func.call_concurrent(accessor, (src,)).await)
        .await??;

    drop(tx);

    Ok(())
}
