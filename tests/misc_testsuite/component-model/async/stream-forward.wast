;;! component_model_async = true
;;! component_model_more_async_builtins = true

;; Golden-path tests for the `stream.forward` built-in sketched in
;; https://github.com/WebAssembly/component-model/issues/658.
;;
;; Each round below writes 4 bytes to stream A, forwards 4 items from A's
;; readable end to B's writable end, and reads 4 bytes from stream B,
;; issuing the three operations in a different order.  Whichever operation
;; arrives last completes the rendezvous inline with `COMPLETED(4)`;
;; operations that returned `BLOCKED` get `COMPLETED(4)` events on their
;; own end, with a blocked forward's event delivered as `STREAM_FORWARD`
;; on B's writable end.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
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

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.src i32)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      ;; Create the source stream A ($r.src/$w.src) and the destination
      ;; stream B ($r.dst/$w.dst).
      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))
      (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; Stage the 4-byte payload at address 0; each round copies it to
      ;; address 4.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))

      ;; Round 1: write, read, forward.  With the write and read already
      ;; pending, the forward completes immediately, copying directly from
      ;; the writer's buffer to the reader's buffer.
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      ;; The write and read completed during the forward, so their
      ;; `COMPLETED(4)` events are already queued.
      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      ;; Round 2: forward, write, read.  The forward blocks with no data
      ;; available, the write blocks because the forward has no destination
      ;; buffer yet, and the read completes the rendezvous inline.  The
      ;; write's event is delivered on $w.src and the forward's event is
      ;; delivered as `STREAM_FORWARD` on $w.dst.
      (i32.store (i32.const 4) (i32.const 0))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      ;; Round 3: forward, read, write.  The forward and the read block and
      ;; the write completes the rendezvous inline.  The read's event is
      ;; delivered on $r.dst and the forward's event is delivered as
      ;; `STREAM_FORWARD` on $w.dst.
      (i32.store (i32.const 4) (i32.const 0))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      ;; Round 4: read, forward, write.  Same events as round 3.
      (i32.store (i32.const 4) (i32.const 0))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $r.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      ;; Round 5: write, forward, read.  Same events as round 2.
      (i32.store (i32.const 4) (i32.const 0))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      ;; All events have been received, so every handle can be dropped.
      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-writable (local.get $w.src))
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

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_return (invoke "run"))

;; Forwarding a stream to itself traps.
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $t64 i64)
      (local.set $t64 (call $stream.new))
      (drop (call $stream.forward
        (i32.wrap_i64 (local.get $t64))
        (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32)))
        (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot forward a stream to itself")

;; Passing the same readable end as both arguments traps.
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $r.a i32)
      (local.set $r.a (i32.wrap_i64 (call $stream.new)))
      (drop (call $stream.forward (local.get $r.a) (local.get $r.a) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot have concurrent operations active on a future/stream")

;; Passing the ends swapped (a writable end as the source) traps.
(component
  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $w.a i32)
      (local $r.b i32)
      (local.set $w.a (i32.wrap_i64 (i64.shr_u (call $stream.new) (i64.const 32))))
      (local.set $r.b (i32.wrap_i64 (call $stream.new)))
      (drop (call $stream.forward (local.get $w.a) (local.get $r.b) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot have concurrent operations active on a future/stream")

;; A forward over a payload-free stream: items have no representation in
;; memory, so only the counts are transferred.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async))
  (core func $stream.write (canon stream.write $s async))
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

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.src i32)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))
      (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

      (call $waitable.join (local.get $r.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $r.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-writable (local.get $w.src))
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

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_return (invoke "run"))

;; A forward over `(stream string)` between distinct components: the copy
;; lifts each string from the writer's memory and lowers it into the
;; reader's memory via the reader's realloc.
(component
  (component $R
    (core module $libc
      (memory (export "mem") 1)
      (global $next (mut i32) (i32.const 1024))
      (func (export "realloc") (param i32 i32 i32 i32) (result i32)
        (local $ret i32)
        (local.set $ret (global.get $next))
        (global.set $next (i32.add (global.get $next) (local.get 3)))
        (local.get $ret)
      )
    )
    (core instance $libc (instantiate $libc))
    (core module $RM
      (import "" "mem" (memory 1))
      (import "" "task.return0" (func $task.return0))
      (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
      (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
      (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
      (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
      (import "" "waitable.join" (func $waitable.join (param i32 i32)))

      (func $strcheck (param $ptr i32) (param $len i32)
        (if (i32.ne (local.get $len) (i32.const 5))
          (then unreachable))
        (if (i32.ne (i32.load8_u (local.get $ptr)) (i32.const 104 (; 'h' ;)))
          (then unreachable))
        (if (i32.ne (i32.load8_u (i32.add (local.get $ptr) (i32.const 1))) (i32.const 101 (; 'e' ;)))
          (then unreachable))
        (if (i32.ne (i32.load8_u (i32.add (local.get $ptr) (i32.const 2))) (i32.const 108 (; 'l' ;)))
          (then unreachable))
        (if (i32.ne (i32.load8_u (i32.add (local.get $ptr) (i32.const 3))) (i32.const 108 (; 'l' ;)))
          (then unreachable))
        (if (i32.ne (i32.load8_u (i32.add (local.get $ptr) (i32.const 4))) (i32.const 111 (; 'o' ;)))
          (then unreachable))
      )

      (func (export "consume") (param $r.dst i32) (result i32)
        (local $ws i32)

        (call $task.return0)

        ;; Park a read for one string; the (ptr, len) pair lands at 16.
        (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 16) (i32.const 1))
                    (i32.const -1 (; BLOCKED ;)))
          (then unreachable))

        (local.set $ws (call $waitable-set.new))
        (call $waitable.join (local.get $r.dst) (local.get $ws))
        (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                    (i32.const 2 (; STREAM_READ ;)))
          (then unreachable))
        (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
          (then unreachable))
        (call $waitable.join (local.get $r.dst) (i32.const 0))
        (call $waitable-set.drop (local.get $ws))

        (call $strcheck (i32.load (i32.const 16)) (i32.load (i32.const 20)))

        (i32.const 0 (; EXIT ;))
      )
      (func (export "consume_cb") (param i32 i32 i32) (result i32)
        unreachable
      )
    )
    (type $ST (stream string))
    (canon task.return (memory (core memory $libc "mem")) (core func $task.return0))
    (canon stream.read $ST async (memory (core memory $libc "mem"))
      (realloc (core func $libc "realloc")) (core func $stream.read))
    (core func $waitable-set.new (canon waitable-set.new))
    (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "mem"))))
    (core func $waitable-set.drop (canon waitable-set.drop))
    (core func $waitable.join (canon waitable.join))
    (core instance $rm (instantiate $RM (with "" (instance
      (export "mem" (memory $libc "mem"))
      (export "task.return0" (func $task.return0))
      (export "stream.read" (func $stream.read))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
    ))))
    (func (export "consume") async (param "in" (stream string)) (canon lift
      (core func $rm "consume")
      async (memory (core memory $libc "mem")) (callback (core func $rm "consume_cb"))
    ))
  )
  (component $W
    (import "consume" (func $consume async (param "in" (stream string))))

    (core module $libc
      (memory (export "mem") 1)
      (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    )
    (core instance $libc (instantiate $libc))
    (core module $WM
      (import "" "mem" (memory 1))
      (import "" "stream.new" (func $stream.new (result i64)))
      (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
      (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
      (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
      (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
      (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
      (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
      (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
      (import "" "waitable.join" (func $waitable.join (param i32 i32)))
      (import "" "consume" (func $consume (param i32)))

      (func (export "run") (result i32)
        (local $t64 i64)
        (local $r.src i32)
        (local $w.src i32)
        (local $r.dst i32)
        (local $w.dst i32)
        (local $ws i32)

        (local.set $t64 (call $stream.new))
        (local.set $r.src (i32.wrap_i64 (local.get $t64)))
        (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

        (local.set $t64 (call $stream.new))
        (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
        (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

        (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 1))
                    (i32.const -1 (; BLOCKED ;)))
          (then unreachable))

        ;; $R parks a one-string read on the destination.
        (call $consume (local.get $r.dst))

        ;; "hello" at 64; the (ptr, len) pair at 16.
        (i32.store8 (i32.const 64) (i32.const 104 (; 'h' ;)))
        (i32.store8 (i32.const 65) (i32.const 101 (; 'e' ;)))
        (i32.store8 (i32.const 66) (i32.const 108 (; 'l' ;)))
        (i32.store8 (i32.const 67) (i32.const 108 (; 'l' ;)))
        (i32.store8 (i32.const 68) (i32.const 111 (; 'o' ;)))
        (i32.store (i32.const 16) (i32.const 64))
        (i32.store (i32.const 20) (i32.const 5))

        (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 16) (i32.const 1))
                    (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
          (then unreachable))

        (local.set $ws (call $waitable-set.new))
        (call $waitable.join (local.get $w.dst) (local.get $ws))
        (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 0))
                    (i32.const 7 (; STREAM_FORWARD ;)))
          (then unreachable))
        (if (i32.ne (i32.load (i32.const 4)) (i32.const 0x10 (; (1<<4) | COMPLETED ;)))
          (then unreachable))
        (call $waitable.join (local.get $w.dst) (i32.const 0))
        (call $waitable-set.drop (local.get $ws))

        (call $stream.drop-readable (local.get $r.src))
        (call $stream.drop-writable (local.get $w.src))
        (call $stream.drop-writable (local.get $w.dst))
        (i32.const 42)
      )
    )
    (type $ST (stream string))
    (canon stream.new $ST (core func $stream.new))
    (canon stream.write $ST async (memory (core memory $libc "mem"))
      (realloc (core func $libc "realloc")) (core func $stream.write))
    (canon stream.forward $ST async (core func $stream.forward))
    (canon stream.drop-readable $ST (core func $stream.drop-readable))
    (canon stream.drop-writable $ST (core func $stream.drop-writable))
    (core func $waitable-set.new (canon waitable-set.new))
    (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $libc "mem"))))
    (core func $waitable-set.drop (canon waitable-set.drop))
    (core func $waitable.join (canon waitable.join))
    (canon lower (func $consume) (core func $consume'))
    (core instance $wm (instantiate $WM (with "" (instance
      (export "mem" (memory $libc "mem"))
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
      (export "consume" (func $consume'))
    ))))
    (func (export "run") async (result u32) (canon lift (core func $wm "run")))
  )

  (instance $r (instantiate $R))
  (instance $w (instantiate $W
    (with "consume" (func $r "consume"))
  ))
  (func (export "run") (alias export $w "run"))
)
(assert_return (invoke "run") (u32.const 42))

;; A forward whose source producer and destination consumer are the same
;; runtime instance with a non-flat payload is rejected by the
;; intra-component copy restriction before any rendezvous state is
;; consumed.
(component
  (core module $libc
    (memory (export "mem") 1)
    (global $next (mut i32) (i32.const 1024))
    (func (export "realloc") (param i32 i32 i32 i32) (result i32)
      (local $ret i32)
      (local.set $ret (global.get $next))
      (global.set $next (i32.add (global.get $next) (local.get 3)))
      (local.get $ret)
    )
  )
  (core instance $libc (instantiate $libc))

  (type $s (stream string))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "mem"))
    (realloc (core func $libc "realloc"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "mem"))
    (realloc (core func $libc "realloc"))))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "mem" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.src i32)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))
      (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; "hello" at 64; the (ptr, len) pair at 16.
      (i32.store8 (i32.const 64) (i32.const 104 (; 'h' ;)))
      (i32.store8 (i32.const 65) (i32.const 101 (; 'e' ;)))
      (i32.store8 (i32.const 66) (i32.const 108 (; 'l' ;)))
      (i32.store8 (i32.const 67) (i32.const 108 (; 'l' ;)))
      (i32.store8 (i32.const 68) (i32.const 111 (; 'o' ;)))
      (i32.store (i32.const 16) (i32.const 64))
      (i32.store (i32.const 20) (i32.const 5))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 16) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 32) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (drop (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 1)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "mem" (memory $libc "mem"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot `stream.forward` between intra-component streams with non-numeric payload")

;; The same restriction applies when the rendezvous happens after the
;; forward is registered: the operation which completes the pairing traps
;; with the forward-specific message.
(component
  (core module $libc
    (memory (export "mem") 1)
    (global $next (mut i32) (i32.const 1024))
    (func (export "realloc") (param i32 i32 i32 i32) (result i32)
      (local $ret i32)
      (local.set $ret (global.get $next))
      (global.set $next (i32.add (global.get $next) (local.get 3)))
      (local.get $ret)
    )
  )
  (core instance $libc (instantiate $libc))

  (type $s (stream string))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "mem"))
    (realloc (core func $libc "realloc"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "mem"))
    (realloc (core func $libc "realloc"))))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "mem" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.src i32)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))
      (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      ;; "hello" at 64; the (ptr, len) pair at 16.
      (i32.store8 (i32.const 64) (i32.const 104 (; 'h' ;)))
      (i32.store8 (i32.const 65) (i32.const 101 (; 'e' ;)))
      (i32.store8 (i32.const 66) (i32.const 108 (; 'l' ;)))
      (i32.store8 (i32.const 67) (i32.const 108 (; 'l' ;)))
      (i32.store8 (i32.const 68) (i32.const 111 (; 'o' ;)))
      (i32.store (i32.const 16) (i32.const 64))
      (i32.store (i32.const 20) (i32.const 5))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 32) (i32.const 1))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (drop (call $stream.write (local.get $w.src) (i32.const 16) (i32.const 1)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "mem" (memory $libc "mem"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot `stream.forward` between intra-component streams with non-numeric payload")

;; Once a forward's entire budget completes, the source readable end is
;; released immediately, even before the `STREAM_FORWARD` completion event
;; is delivered: direct reads on the source work again while the event is
;; still queued.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
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

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.src i32)
      (local $r.dst i32)
      (local $w.dst i32)
      (local $ws i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))
      (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Complete the forward's entire budget; its event stays queued on
      ;; the destination writable end.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

      ;; The source readable end is usable again before the forward's
      ;; completion event is delivered: park a fresh read and complete it
      ;; with a direct write.
      (if (i32.ne (call $stream.read (local.get $r.src) (i32.const 16) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (i32.store (i32.const 0) (i32.const 0xfeedface))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (call $waitable.join (local.get $r.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 2 (; STREAM_READ ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $r.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 16)) (i32.const 0xfeedface))
        (then unreachable))
      (call $waitable.join (local.get $r.src) (i32.const 0))

      ;; The queued forward completion is delivered unchanged.
      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-writable (local.get $w.src))
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

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_return (invoke "run"))
