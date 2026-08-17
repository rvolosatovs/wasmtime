;;! component_model_async = true
;;! component_model_more_async_builtins = true

;; `stream.forward` interacting with dropped peer ends: a forward issued
;; after a peer end is already dropped returns `DROPPED(0)` immediately,
;; a drop discovered while the forward is pending is reported via a
;; `STREAM_FORWARD` event, and each end whose peer is gone is marked
;; done.

;; The source writer is dropped before the forward: the forward reports
;; `DROPPED(0)` and marks the source readable end done, while the
;; destination stream remains fully usable.
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

      (call $stream.drop-writable (local.get $w.src))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      ;; The destination stream is still usable.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))

      (if (i32.ne (call $stream.write (local.get $w.dst) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))
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

;; The destination reader is dropped before the forward: the forward
;; reports `DROPPED(0)` and marks the destination writable end done, while
;; the source stream remains fully usable.
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

      (call $stream.drop-readable (local.get $r.dst))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      ;; The source stream is still usable.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))

      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.read (local.get $r.src) (i32.const 4) (i32.const 4))
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

      (call $stream.drop-readable (local.get $r.src))
      (call $stream.drop-writable (local.get $w.src))
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

;; Both peers are dropped before the forward: the forward reports
;; `DROPPED(0)` and marks BOTH ends done, so a subsequent read on the
;; source traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

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

      (call $stream.drop-writable (local.get $w.src))
      (call $stream.drop-readable (local.get $r.dst))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      (drop (call $stream.read (local.get $r.src) (i32.const 4) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot read after being notified that the writable end dropped")

;; Both peers are dropped before the forward: a subsequent write on the
;; destination traps as well.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

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

      (call $stream.drop-writable (local.get $w.src))
      (call $stream.drop-readable (local.get $r.dst))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      (drop (call $stream.write (local.get $w.dst) (i32.const 4) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot write after being notified that the readable end dropped")

;; The source writer is dropped while a forward is pending: the next read
;; on the destination discovers it, the forward's `DROPPED(0)` event is
;; delivered as `STREAM_FORWARD` on B's writable end, and the read stays
;; pending until a direct write on B's writable end completes it.
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

      (call $stream.drop-writable (local.get $w.src))

      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      ;; The destination stream is still usable and the read is still
      ;; pending; complete it with a direct write.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.dst) (i32.const 0) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))

      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

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

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_return (invoke "run"))

;; The source writer is dropped while a forward is pending and a read is
;; parked, after 4 of 8 items have already been forwarded: the forward's
;; `DROPPED(4)` event is delivered as `STREAM_FORWARD` on B's writable end,
;; and the parked read is then completed by a direct write on B's writable
;; end.
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

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 8))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Forward the first 4 items.
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

      ;; Park another read, then drop the source writer.
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (call $stream.drop-writable (local.get $w.src))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x41 (; (4<<4) | DROPPED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      ;; The parked read is still pending; complete it with a direct write.
      (i32.store (i32.const 0) (i32.const 0xdecafbad))
      (if (i32.ne (call $stream.write (local.get $w.dst) (i32.const 0) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdecafbad))
        (then unreachable))

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

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_return (invoke "run"))

;; The destination reader is dropped while a forward is pending: the
;; forward's `DROPPED(0)` event is delivered as `STREAM_FORWARD` on B's
;; writable end and marks it done, while the source stream remains fully
;; usable.
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

      (call $stream.drop-readable (local.get $r.dst))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      ;; The source stream is still usable.
      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.src) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
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

;; The destination reader is dropped after the forward has completed but
;; before its event is delivered: the drop notice is merged into the queued
;; `STREAM_FORWARD` completion, which is delivered as `DROPPED(4)` and marks
;; B's writable end done, so a subsequent write on it traps.
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

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 4))
                  (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 4)) (i32.const 0xdeadbeef))
        (then unreachable))

      ;; Drop the reader while the forward's completion event is still
      ;; queued.
      (call $stream.drop-readable (local.get $r.dst))

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
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x41 (; (4<<4) | DROPPED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

      (call $waitable-set.drop (local.get $ws))

      ;; The merged event marked $w.dst done, so the next write traps.
      (drop (call $stream.write (local.get $w.dst) (i32.const 0) (i32.const 4)))
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

(assert_trap (invoke "run") "cannot write after being notified that the readable end dropped")

;; The source writer is dropped while the forward is pending (queueing a
;; `DROPPED(2)` teardown event), then the destination reader is also
;; dropped before that event is delivered: the reader-drop notice is merged
;; into the queued event, so the delivered `DROPPED(2)` marks B's writable
;; end done and a subsequent write on it traps.
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

      ;; Drive 2 of 4 items through the forward.
      (i32.store (i32.const 0) (i32.const 0xbeef))
      (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 2))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))
      (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 4) (i32.const 2))
                  (i32.const 0x20 (; (2<<4) | COMPLETED ;)))
        (then unreachable))

      (local.set $ws (call $waitable-set.new))
      (call $waitable.join (local.get $w.src) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 3 (; STREAM_WRITE ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.src) (i32.const 0))

      ;; Drop the source writer (tearing the forward down and queueing
      ;; `DROPPED(2)` on B's writable end), then the destination reader.
      (call $stream.drop-writable (local.get $w.src))
      (call $stream.drop-readable (local.get $r.dst))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x21 (; (2<<4) | DROPPED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))
      (call $waitable-set.drop (local.get $ws))

      (drop (call $stream.write (local.get $w.dst) (i32.const 0) (i32.const 1)))
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

(assert_trap (invoke "run") "cannot write after being notified that the readable end dropped")

;; The source writer is dropped after the forward has completed but before
;; its event is delivered: the source stream's state is already settled, so
;; the drop is an ordinary writer drop.  A direct read on the source
;; readable end observes `DROPPED(0)` immediately, and the queued
;; `STREAM_FORWARD` completion is delivered unchanged.
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

      ;; Complete the forward's entire budget; its event stays queued.
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

      ;; The forward has completed, so the source readable end is already
      ;; released: dropping the writer is an ordinary drop, observed by a
      ;; direct read as `DROPPED(0)`.
      (call $stream.drop-writable (local.get $w.src))
      (if (i32.ne (call $stream.read (local.get $r.src) (i32.const 8) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      ;; The queued completion is delivered unchanged.
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

;; The source writer is dropped while a forward is pending with NO
;; destination read parked: the forwarder waiting on B's writable end
;; receives `STREAM_FORWARD` `DROPPED(0)` immediately, without any
;; destination-side operation being needed to discover the drop.
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

      (call $stream.drop-writable (local.get $w.src))

      (local.set $ws (call $waitable-set.new))

      (call $waitable.join (local.get $w.dst) (local.get $ws))
      (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                  (i32.const 7 (; STREAM_FORWARD ;)))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 8)) (local.get $w.dst))
        (then unreachable))
      (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))
      (call $waitable.join (local.get $w.dst) (i32.const 0))

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

;; Dropping the source's readable end while it is busy with a pending
;; forward traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))

      (local.set $t64 (call $stream.new))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (call $stream.drop-readable (local.get $r.src))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot remove busy stream")

;; Dropping the destination's writable end while it is busy with a pending
;; forward traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))

      (local.set $t64 (call $stream.new))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (call $stream.drop-writable (local.get $w.dst))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot drop busy stream")

;; After a forward reports the source writer's drop, the source readable
;; end is done: forwarding from it again traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-writable (canon stream.drop-writable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $w.src i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))
      (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (call $stream.drop-writable (local.get $w.src))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      (drop (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot forward after being notified that the writable end dropped")

;; After a forward reports the destination reader's drop, the destination
;; writable end is done: forwarding into it again traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))
  (core func $stream.drop-readable (canon stream.drop-readable $s))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
    (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.src i32)
      (local $r.dst i32)
      (local $w.dst i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.src (i32.wrap_i64 (local.get $t64)))

      (local.set $t64 (call $stream.new))
      (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
      (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (call $stream.drop-readable (local.get $r.dst))

      (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                  (i32.const 0x1 (; DROPPED ;)))
        (then unreachable))

      (drop (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "cannot forward after being notified that the readable end dropped")
