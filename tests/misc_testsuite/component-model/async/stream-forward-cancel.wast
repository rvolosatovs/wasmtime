;;! component_model_async = true
;;! component_model_more_async_builtins = true

;; Cancelling a pending `stream.forward` via `stream.cancel-write` on the
;; destination's writable end or `stream.cancel-read` on the source's
;; readable end: either cancel reports the number of items forwarded so far
;; (or, for `stream.cancel-write`, the completed result if the forward
;; already finished) and releases both ends.  Once the forward has
;; completed, the source handle is already released, so a late
;; `stream.cancel-read` on it traps.

;; Cancel an idle pending forward: `CANCELLED(0)`, after which both streams
;; are fully reusable, including for another forward.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
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
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
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

      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; Both streams are reusable; run a full forward round.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
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
      (export "stream.cancel-write" (func $stream.cancel-write))
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

;; Cancel a forward that has made partial progress: `CANCELLED(4)` after 4
;; of 8 items were forwarded.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
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
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
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

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 8))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
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

      (call $waitable-set.drop (local.get $ws))

      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const 0x42 (; (4<<4) | CANCELLED ;)))
        (then unreachable))

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
      (export "stream.cancel-write" (func $stream.cancel-write))
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

;; Cancel a forward whose completion event is already queued: the cancel
;; delivers the completed result as `COMPLETED(4)`, since the forward's
;; entire budget finished before the cancel request.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
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
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-write" (func $stream.cancel-write (param i32) (result i32)))
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

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      (if (i32.ne (call $stream.cancel-write (local.get $w.dst))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
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
      (export "stream.cancel-write" (func $stream.cancel-write))
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

;; Cancelling a pending forward via the source's readable end while a read
;; is parked on the destination: the forward reports `CANCELLED(0)`, the
;; parked read stays pending and is completed by a direct write, and the
;; source stream remains fully usable.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
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
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
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

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; The parked read survives the cancel and is completed by a direct
      ;; write on the destination.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.dst) (i32.const 0) (i32.const 4))
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

      ;; The source stream is fully usable as well.
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.src) (i32.const 16) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.src))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

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
      (export "stream.cancel-read" (func $stream.cancel-read))
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

;; Cancelling a read parked behind a pending forward: the read reports
;; `CANCELLED(0)`, the forward stays pending, and a later write/read pair
;; completes it.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
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
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
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

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.cancel-read (local.get $r.dst))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; The forward is still pending; a write parks and a fresh read
      ;; completes the rendezvous.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
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
      (export "stream.cancel-read" (func $stream.cancel-read))
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

;; Cancel an idle pending forward via the source's readable end:
;; `CANCELLED(0)`, after which both streams are fully reusable, including
;; for another forward.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
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
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
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

      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const 0x2 (; CANCELLED ;)))
        (then unreachable))

      ;; Both streams are reusable; run a full forward round.
      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
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
      (export "stream.cancel-read" (func $stream.cancel-read))
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

;; Cancel a forward via the source's readable end after a budget-partial
;; drive: `CANCELLED(4)` after 4 of 8 items were forwarded.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))
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
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))
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

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 8))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Drive 4 of 8 items through the forward.
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
      (call $waitable-set.drop (local.get $ws))

      (if (i32.ne (call $stream.cancel-read (local.get $r.src))
                  (i32.const 0x42 (; (4<<4) | CANCELLED ;)))
        (then unreachable))

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
      (export "stream.cancel-read" (func $stream.cancel-read))
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

;; Cancelling via the source's readable end after the forward has completed
;; (but before its event is delivered) traps: the source handle was already
;; released when the forward completed, so there is no pending read to
;; cancel.  `stream.cancel-write` on the destination handle is the reliable
;; way to cancel-or-collect a forward.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.cancel-read (canon stream.cancel-read $s async))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.cancel-read" (func $stream.cancel-read (param i32) (result i32)))

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

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (drop (call $stream.cancel-read (local.get $r.src)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.cancel-read" (func $stream.cancel-read))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "stream or future read cancelled when no read is pending")
