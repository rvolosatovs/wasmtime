;;! component_model_async = true
;;! component_model_more_async_builtins = true

;; Synchronous `stream.forward`: a sync forward that can complete
;; immediately does so inline, a sync forward that blocks suspends the
;; task until the forward completes, calling it from a task that may not
;; block traps, and a synchronous `stream.write` on the source of a
;; pending forward waits for the write to complete instead of returning
;; BLOCKED.

;; A sync forward with both peers already parked completes inline with
;; `COMPLETED(4)`.
(component
  (core module $libc (memory (export "m") 1))
  (core instance $libc (instantiate $libc))

  (type $s (stream u8))
  (core func $stream.new (canon stream.new $s))
  (core func $stream.read (canon stream.read $s async (memory (core memory $libc "m"))))
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

      (i32.store (i32.const 0) (i32.const 0xdeadbeef))

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

;; A sync forward that blocks: $C returns the destination's readable end
;; and then suspends in a sync `stream.forward`; $D drives it to completion
;; with a write on the source and a read on the destination.
(component
  (component $C
    (core module $Memory (memory (export "mem") 1))
    (core instance $memory (instantiate $Memory))
    (core module $CM
      (import "" "mem" (memory 1))
      (import "" "task.return1" (func $task.return1 (param i32)))
      (import "" "stream.new" (func $stream.new (result i64)))
      (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))
      (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
      (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))

      (global $result (mut i32) (i32.const 0x7fffffff))

      (func (export "forward") (param $r.src i32) (result i32)
        (local $t64 i64)
        (local $r.dst i32)
        (local $w.dst i32)

        (local.set $t64 (call $stream.new))
        (local.set $r.dst (i32.wrap_i64 (local.get $t64)))
        (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

        ;; Return the destination's readable end so the caller can read
        ;; from it, then suspend in the sync forward.
        (call $task.return1 (local.get $r.dst))

        (global.set $result
          (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4)))
        (if (i32.ne (global.get $result) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
          (then unreachable))

        (call $stream.drop-readable (local.get $r.src))
        (call $stream.drop-writable (local.get $w.dst))
        (i32.const 0 (; EXIT ;))
      )
      (func (export "forward_cb") (param i32 i32 i32) (result i32)
        unreachable
      )

      ;; Report the recorded forward result to the caller so it can verify
      ;; that the suspended forward actually completed.
      (func (export "check") (result i32)
        (global.get $result)
      )
    )
    (type $ST (stream u8))
    (canon task.return (result $ST) (memory (core memory $memory "mem")) (core func $task.return1))
    (canon stream.new $ST (core func $stream.new))
    (canon stream.forward $ST (core func $stream.forward))
    (canon stream.drop-readable $ST (core func $stream.drop-readable))
    (canon stream.drop-writable $ST (core func $stream.drop-writable))
    (core instance $cm (instantiate $CM (with "" (instance
      (export "mem" (memory $memory "mem"))
      (export "task.return1" (func $task.return1))
      (export "stream.new" (func $stream.new))
      (export "stream.forward" (func $stream.forward))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
    ))))
    (func (export "forward") async (param "in" (stream u8)) (result (stream u8)) (canon lift
      (core func $cm "forward")
      async (memory (core memory $memory "mem")) (callback (core func $cm "forward_cb"))
    ))
    (func (export "check") (result u32) (canon lift (core func $cm "check")))
  )
  (component $D
    (import "forward" (func $forward async (param "in" (stream u8)) (result (stream u8))))
    (import "check" (func $check (result u32)))

    (core module $Memory (memory (export "mem") 1))
    (core instance $memory (instantiate $Memory))
    (core module $DM
      (import "" "mem" (memory 1))
      (import "" "stream.new" (func $stream.new (result i64)))
      (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
      (import "" "stream.write" (func $stream.write (param i32 i32 i32) (result i32)))
      (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))
      (import "" "stream.drop-writable" (func $stream.drop-writable (param i32)))
      (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
      (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
      (import "" "waitable-set.drop" (func $waitable-set.drop (param i32)))
      (import "" "waitable.join" (func $waitable.join (param i32 i32)))
      (import "" "yield" (func $yield (result i32)))
      (import "" "forward" (func $forward (param i32) (result i32)))
      (import "" "check" (func $check (result i32)))

      (func (export "run") (result i32)
        (local $t64 i64)
        (local $r.src i32)
        (local $w.src i32)
        (local $r.dst i32)
        (local $ws i32)

        (local.set $t64 (call $stream.new))
        (local.set $r.src (i32.wrap_i64 (local.get $t64)))
        (local.set $w.src (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

        ;; $C registers the forward and suspends in it.
        (local.set $r.dst (call $forward (local.get $r.src)))

        ;; Drive the forward with a write on the source and a read on the
        ;; destination.
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
        (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
          (then unreachable))
        (call $waitable.join (local.get $w.src) (i32.const 0))
        (call $waitable-set.drop (local.get $ws))

        ;; Yield so $C's now-ready task can finish its suspended forward,
        ;; then verify it completed with `COMPLETED(4)`.
        (drop (call $yield))
        (if (i32.ne (call $check) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
          (then unreachable))

        (call $stream.drop-writable (local.get $w.src))
        (call $stream.drop-readable (local.get $r.dst))
        (i32.const 42)
      )
    )
    (type $ST (stream u8))
    (canon stream.new $ST (core func $stream.new))
    (canon stream.read $ST async (memory (core memory $memory "mem")) (core func $stream.read))
    (canon stream.write $ST async (memory (core memory $memory "mem")) (core func $stream.write))
    (canon stream.drop-readable $ST (core func $stream.drop-readable))
    (canon stream.drop-writable $ST (core func $stream.drop-writable))
    (core func $waitable-set.new (canon waitable-set.new))
    (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $memory "mem"))))
    (core func $waitable-set.drop (canon waitable-set.drop))
    (core func $waitable.join (canon waitable.join))
    (core func $yield (canon thread.yield cancellable))
    (canon lower (func $forward) (core func $forward'))
    (canon lower (func $check) (core func $check'))
    (core instance $dm (instantiate $DM (with "" (instance
      (export "mem" (memory $memory "mem"))
      (export "stream.new" (func $stream.new))
      (export "stream.read" (func $stream.read))
      (export "stream.write" (func $stream.write))
      (export "stream.drop-readable" (func $stream.drop-readable))
      (export "stream.drop-writable" (func $stream.drop-writable))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "waitable-set.drop" (func $waitable-set.drop))
      (export "waitable.join" (func $waitable.join))
      (export "yield" (func $yield))
      (export "forward" (func $forward'))
      (export "check" (func $check'))
    ))))
    (func (export "run") async (result u32) (canon lift (core func $dm "run")))
  )

  (instance $c (instantiate $C))
  (instance $d (instantiate $D
    (with "forward" (func $c "forward"))
    (with "check" (func $c "check"))
  ))
  (func (export "run") (alias export $d "run"))
)
(assert_return (invoke "run") (u32.const 42))

;; A sync forward from a task which may not block traps before touching
;; any stream state.
(component
  (component $child
    (core module $libc (memory (export "m") 1))
    (core instance $libc (instantiate $libc))

    (type $s (stream u8))
    (core func $stream.new (canon stream.new $s))
    (core func $stream.forward (canon stream.forward $s))

    (core module $m
      (import "" "stream.new" (func $stream.new (result i64)))
      (import "" "stream.forward" (func $stream.forward (param i32 i32 i32) (result i32)))

      (func (export "run")
        (local $t64 i64)
        (local $r.src i32)
        (local $w.dst i32)

        (local.set $t64 (call $stream.new))
        (local.set $r.src (i32.wrap_i64 (local.get $t64)))

        (local.set $t64 (call $stream.new))
        (local.set $w.dst (i32.wrap_i64 (i64.shr_u (local.get $t64) (i64.const 32))))

        (drop (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4)))
      )
    )
    (core instance $i (instantiate $m
      (with "" (instance
        (export "stream.new" (func $stream.new))
        (export "stream.forward" (func $stream.forward))
      ))
    ))
    (func (export "run") (canon lift (core func $i "run")))
  )
  (instance $child (instantiate $child))

  (component $parent
    (import "child" (instance $child
      (export "run" (func))
    ))
    (core func $child-run (canon lower (func $child "run")))
    (core module $m
      (import "" "child-run" (func $child-run))
      (func (export "run")
        (call $child-run)
      )
    )
    (core instance $i (instantiate $m
      (with "" (instance
        (export "child-run" (func $child-run))
      ))
    ))
    (func (export "run") async (canon lift (core func $i "run")))
  )
  (instance $parent (instantiate $parent (with "child" (instance $child))))

  (func (export "run") (alias export $parent "run"))
)

(assert_trap (invoke "run") "cannot block a synchronous task before returning")

;; A sync `stream.write` on the source of a pending forward with no
;; destination read parked waits until a later read drives the forward,
;; then returns `COMPLETED(4)` (rather than returning BLOCKED, which the
;; sync ABI forbids).
(component
  (component $Helper
    (core module $Memory (memory (export "mem") 1))
    (core instance $memory (instantiate $Memory))
    (core module $HM
      (import "" "mem" (memory 1))
      (import "" "task.return0" (func $task.return0))
      (import "" "yield" (func $yield (result i32)))
      (import "" "stream.read" (func $stream.read (param i32 i32 i32) (result i32)))
      (import "" "stream.drop-readable" (func $stream.drop-readable (param i32)))

      (func (export "consume") (param $r.dst i32) (result i32)
        ;; Return immediately so the caller can proceed, then yield so the
        ;; caller parks its sync write before the read below drives it.
        (call $task.return0)
        (drop (call $yield))

        (if (i32.ne (call $stream.read (local.get $r.dst) (i32.const 0) (i32.const 4))
                    (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
          (then unreachable))
        (if (i32.ne (i32.load (i32.const 0)) (i32.const 0xdeadbeef))
          (then unreachable))

        ;; Keep $r.dst alive: dropping it here would race with the caller
        ;; reaping the forward's queued completion event, merging a drop
        ;; notice into it.
        (i32.const 0 (; EXIT ;))
      )
      (func (export "consume_cb") (param i32 i32 i32) (result i32)
        unreachable
      )
    )
    (type $ST (stream u8))
    (canon task.return (memory (core memory $memory "mem")) (core func $task.return0))
    (core func $yield (canon thread.yield cancellable))
    (canon stream.read $ST (memory (core memory $memory "mem")) (core func $stream.read))
    (canon stream.drop-readable $ST (core func $stream.drop-readable))
    (core instance $hm (instantiate $HM (with "" (instance
      (export "mem" (memory $memory "mem"))
      (export "task.return0" (func $task.return0))
      (export "yield" (func $yield))
      (export "stream.read" (func $stream.read))
      (export "stream.drop-readable" (func $stream.drop-readable))
    ))))
    (func (export "consume") async (param "in" (stream u8)) (canon lift
      (core func $hm "consume")
      async (memory (core memory $memory "mem")) (callback (core func $hm "consume_cb"))
    ))
  )
  (component $Main
    (import "consume" (func $consume async (param "in" (stream u8))))

    (core module $Memory (memory (export "mem") 1))
    (core instance $memory (instantiate $Memory))
    (core module $MM
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

        ;; Register the forward with no read parked on the destination.
        (if (i32.ne (call $stream.forward (local.get $r.src) (local.get $w.dst) (i32.const 4))
                    (i32.const -1 (; BLOCKED ;)))
          (then unreachable))

        ;; Hand the destination's readable end to $Helper, which yields
        ;; once and then reads.
        (call $consume (local.get $r.dst))

        ;; The sync write parks (no read is present yet), suspends this
        ;; task, and completes once $Helper's read drives the forward.
        (i32.store (i32.const 0) (i32.const 0xdeadbeef))
        (if (i32.ne (call $stream.write (local.get $w.src) (i32.const 0) (i32.const 4))
                    (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
          (then unreachable))

        ;; The forward completed as part of the same drive.
        (local.set $ws (call $waitable-set.new))
        (call $waitable.join (local.get $w.dst) (local.get $ws))
        (if (i32.ne (call $waitable-set.wait (local.get $ws) (i32.const 8))
                    (i32.const 7 (; STREAM_FORWARD ;)))
          (then unreachable))
        (if (i32.ne (i32.load (i32.const 12)) (i32.const 0x40 (; (4<<4) | COMPLETED ;)))
          (then unreachable))
        (call $waitable.join (local.get $w.dst) (i32.const 0))
        (call $waitable-set.drop (local.get $ws))

        (call $stream.drop-readable (local.get $r.src))
        (call $stream.drop-writable (local.get $w.src))
        (call $stream.drop-writable (local.get $w.dst))
        (i32.const 42)
      )
    )
    (type $ST (stream u8))
    (canon stream.new $ST (core func $stream.new))
    (canon stream.write $ST (memory (core memory $memory "mem")) (core func $stream.write))
    (canon stream.forward $ST async (core func $stream.forward))
    (canon stream.drop-readable $ST (core func $stream.drop-readable))
    (canon stream.drop-writable $ST (core func $stream.drop-writable))
    (core func $waitable-set.new (canon waitable-set.new))
    (core func $waitable-set.wait (canon waitable-set.wait (memory (core memory $memory "mem"))))
    (core func $waitable-set.drop (canon waitable-set.drop))
    (core func $waitable.join (canon waitable.join))
    (canon lower (func $consume) (core func $consume'))
    (core instance $mm (instantiate $MM (with "" (instance
      (export "mem" (memory $memory "mem"))
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
    (func (export "run") async (result u32) (canon lift (core func $mm "run")))
  )

  (instance $helper (instantiate $Helper))
  (instance $main (instantiate $Main
    (with "consume" (func $helper "consume"))
  ))
  (func (export "run") (alias export $main "run"))
)
(assert_return (invoke "run") (u32.const 42))
