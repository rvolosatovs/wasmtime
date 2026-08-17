;;! component_model_async = true
;;! component_model_more_async_builtins = true

;; Chained `stream.forward`s are not supported: a `stream.forward` whose
;; source stream is already the destination of a pending forward, or whose
;; destination stream is already the source of one, traps eagerly at the
;; `stream.forward` call itself, regardless of whether any reads or writes
;; are parked on the streams involved.

;; The destination stream's readable end is the source of a pending
;; forward (`dst.read == Forwarding`), with a write parked on the new
;; source: the second forward traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.write (canon stream.write $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.a i32)
      (local $w.a i32)
      (local $r.b i32)
      (local $w.b i32)
      (local $r.c i32)
      (local $w.c i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.a (i32.wrap_i64 (local.get $t64)))
      (local.set $w.a (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.b (i32.wrap_i64 (local.get $t64)))
      (local.set $w.b (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.c (i32.wrap_i64 (local.get $t64)))
      (local.set $w.c (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.b) (local.get $w.c) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Park a read on C and a write on A; neither changes the outcome.
      (if (i32.ne (call $stream.read (local.get $r.c) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (if (i32.ne (call $stream.write (local.get $w.a) (i32.const 0) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; B's readable end is the source of the pending B->C forward, so
      ;; forwarding A into B traps.
      (drop (call $stream.forward (local.get $r.a) (local.get $w.b) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "chained `stream.forward` is not supported")

;; The source stream's writable end is the destination of a pending
;; forward (`src.write == Forwarding`) with both forwards otherwise idle:
;; the second forward traps immediately rather than blocking.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.a i32)
      (local $w.a i32)
      (local $r.b i32)
      (local $w.b i32)
      (local $r.c i32)
      (local $w.c i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.a (i32.wrap_i64 (local.get $t64)))
      (local.set $w.a (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.b (i32.wrap_i64 (local.get $t64)))
      (local.set $w.b (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.c (i32.wrap_i64 (local.get $t64)))
      (local.set $w.c (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.a) (local.get $w.b) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; B's writable end is the destination of the pending A->B forward,
      ;; so forwarding B into C traps.
      (drop (call $stream.forward (local.get $r.b) (local.get $w.c) (i32.const 4)))
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

(assert_trap (invoke "run") "chained `stream.forward` is not supported")

;; The source stream's writable end is the destination of a pending
;; forward and a read is parked on the new destination
;; (`src.write == Forwarding` + `dst.read == GuestReady`): the second
;; forward still traps.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
  (core func $stream.forward (canon stream.forward $s async))

  (core module $m
    (import "" "m" (memory 1))
    (import "" "stream.new" (func $stream.new (result i64)))
    (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
    (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

    (func (export "run")
      (local $t64 i64)
      (local $r.a i32)
      (local $w.a i32)
      (local $r.b i32)
      (local $w.b i32)
      (local $r.c i32)
      (local $w.c i32)

      (local.set $t64 (call $stream.new))
      (local.set $r.a (i32.wrap_i64 (local.get $t64)))
      (local.set $w.a (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.b (i32.wrap_i64 (local.get $t64)))
      (local.set $w.b (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (local.set $t64 (call $stream.new))
      (local.set $r.c (i32.wrap_i64 (local.get $t64)))
      (local.set $w.c (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

      (if (i32.ne (call $stream.forward (local.get $r.a) (local.get $w.b) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      ;; Park a read on C so the second forward would have a ready
      ;; destination buffer; it traps anyway.
      (if (i32.ne (call $stream.read (local.get $r.c) (i32.const 4) (i32.const 4))
                  (i32.const -1 (; BLOCKED ;)))
        (then unreachable))

      (drop (call $stream.forward (local.get $r.b) (local.get $w.c) (i32.const 4)))
    )
  )

  (core instance $i (instantiate $m
    (with "" (instance
      (export "m" (memory $libc "m"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.forward" (func $stream.forward))
    ))
  ))

  (func (export "run") async (canon lift (core func $i "run")))
)

(assert_trap (invoke "run") "chained `stream.forward` is not supported")
