use {
    super::util::config,
    component_async_tests::{
        Ctx,
        util::{PipeConsumer, PipeProducer},
    },
    futures::{StreamExt, channel::mpsc},
    wasmtime::{
        Engine, Result, Store,
        component::{Component, Linker, ResourceTable, StreamReader},
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
