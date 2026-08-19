# `stream.forward` — outstanding work

Scratch notes from a review of the `f/sf` branch (commits `f9ad636e18..d7aa892c90`)
against `bc06450a02`. Delete this file before the branch is proposed upstream.

## Pre-submission blockers

- [ ] **Drop commit `5a454ec399` (`[TMP] use local dep overrides`).** It adds a
  `[patch.crates-io]` section (`Cargo.toml:795`) pointing 13 wasm-tools crates at
  `../wasm-tools/crates/*`, a path that only exists on the development machine, so
  the tree does not build anywhere else. The matching `Cargo.lock` hunk strips
  `source`/`checksum` from 14 registry packages, turning `cargo vet`-audited
  dependencies into unaudited path dependencies. Replace with a dependency bump to
  a published wasm-tools release carrying the `stream.forward` opcode (`0x2e`), as
  its own scoped commit.
- [ ] Revert the unrelated `tempfile` -> `getrandom 0.3.1` change that rode along
  in the same lockfile hunk (`Cargo.lock:3744`).

## Cancellation bugs

The first four shared a root cause: the code asked "is another task sync-blocked in
`stream.forward` on this handle?" by testing `waitable.common(..).set.is_some()`.
Waitable-set membership is not that fact — a guest can join a handle to a set purely
to poll it, and set-joining is an incidental detail of how `wait_for_event` parks
sync waiters.

Three of the four are now fixed. `forward_end_has_waiter` replaces the membership
test with the fact actually wanted: whether a guest thread is currently parked on the
set that end was joined to (`WaitableSet::waiting` is non-empty). That is checkable
today and distinguishes a blocked waiter from an incidental watcher, so it does not
need the `ForwardState` phase field originally sketched here. Whether to record the
owed delivery on the forward anyway remains open — see the `ForwardState` cleanup
below, which would make it fall out for free.

- [x] **`futures_and_streams.rs:2590` — host-graft cancel strands a sync forward.**
  The graft branches call `request_forward_cancel` unconditionally, without the
  `set.is_some()` check the pure-guest teardown branch 20 lines below (5769-5781)
  performs for exactly this hazard. A task sync-blocked in `stream.forward` against
  a host producer has its terminal event queued on `src.read_handle` only, while
  `restore_forward_end(.., ForwardEnd::Dst, false)` flips its still-in-use
  destination handle from `Busy` back to `Write { done: false }`. The waiting fiber
  never wakes; a sibling task can then legally `stream.write` the released handle,
  and any later event on that waitable makes `Waitable::on_delivery` `bail_bug!`.
  Same gap at 5756 and in the `ForwardCancel::Both` arm.

  Fixed: the four graft branches now go through `request_forward_cancel_for`, which
  records the opposite end as well when a thread is blocked there, so each end gets
  its own terminal event via the existing `ForwardCancel::Both` path. Covered by
  `async_cancel_grafted_forward_from_sibling_task`.

- [x] **`futures_and_streams.rs:5788` — `cancel_read` reports `Cancelled` while
  leaving the destination `Busy`.** Both sub-branches of the `ReadState::Forwarding`
  teardown fall through to `ReturnCode::Cancelled(fwd.forwarded)`, but the
  `set.is_some()` branch also queues the terminal event and restores only the source
  handle. `cancel_write` returns `Blocked` in the byte-identical situation (5601).
  A non-`BLOCKED` code means "no event will be delivered", so the guest reuses the
  destination: `stream.write` hits `TransmitLocalState::Busy` and traps with
  `ConcurrentFutureStreamOp`, `stream.drop-writable` fails with "cannot drop busy
  stream". The count is also reported twice, once as the return code and once in
  the queued event. Adding the `waitable.join` that is the normal way to observe an
  async forward is what flips the branch, so the same program works without it.

  Fixed as a consequence of the predicate change: a guest that joins the destination
  without parking on the set now takes the eager-restore path, so the outcome no
  longer turns on membership. A genuinely blocked waiter still gets its event.
  Covered by the last case in `stream-forward-cancel.wast`.

- [ ] **`futures_and_streams.rs:5576` — graft cancel waits on a waitable a sync
  forward already owns.** The graft branches reach `block_or_wait_for_*` before the
  `set.is_some()` check on the following `else if`. Exactly one terminal event is
  queued, so whichever party reaches it first consumes it and the other starves; if
  the canceller is sync, `wait_for_event` -> `trap_if_in_waitable_set` traps with
  `Trap::WaitableSyncAndAsync`. Symmetric at 5745.

  Still open, and not covered by a test: here the canceller and the sync-blocked
  forwarder are parked on the *same* waitable, so the "give each end its own event"
  fix above does not apply, and a guest cannot join a waitable another task is
  sync-waiting on, so the starvation has no guest-observable repro. It needs a
  decision on what an async cancel should promise when the event it would wait for
  is already owed to someone else — plausibly refuse rather than answer `BLOCKED`.

- [x] **`futures_and_streams.rs:2533` — cancel outranks a completed budget.**
  `settle_forward_rendezvous` tests `cancel_forward.requested()` before the
  `forwarded == fwd.count` arm, so a forward whose full budget lands in the same
  batch that resolves a cancellation reports `CANCELLED(count)` where the rule
  documented at 5555-5558 calls for `COMPLETED(count)`. The arm also hardcodes
  `done = false`, so a host-end drop reported in that same batch is discarded and
  `restore_forward_end` marks the source `Read { done: false }` even though
  `src.write` is `WriteState::Dropped`. `ForwardCancel::Both` likewise hardcodes
  `dropped: false` on both events.

  Fixed: the budget-exhausted arm is now tested first, and the `done` flag derived
  from the batch's code flows into all three cancellation arms. Covered by
  `async_cancel_resolved_by_completing_batch`.

## Other correctness work

- [ ] **`futures_and_streams.rs:5387` — zero-length forward answers without
  asking.** A zero-length `stream.forward` against a host-owned end returns
  `COMPLETED(0)` because a producer/consumer is attached, never polling it; the
  zero-length `stream.read`/`stream.write` probe on the same stream goes through
  `Instance::produce` and correctly reports `BLOCKED`. A guest using the cheap probe
  to decide when to issue the real forward is told both ends are ready and then
  blocks. Five sites define probe semantics independently of `forward_copy`'s
  `budget == 0` rule (4260): 5391, 5423, 4794, 5103, and `set_consumer` 3688.

  Premise confirmed by experiment: with a pending `StreamProducer` attached to the
  source, `stream.read(r_src, ptr, 0)` reports `BLOCKED` while
  `stream.forward(r_src, w_dst, 0)` reports `COMPLETED(0)`. Still open: answering
  honestly means polling the host end on a zero-budget forward and registering the
  forward so the guest is told when it becomes ready, which is new machinery rather
  than a local edit. `async_zero_length_forward_probes_host_producer` encodes the
  wanted behaviour and is `#[ignore]`d until then.

- [ ] **`futures_and_streams.rs:5562` — the `Cancelled` catch-all is too broad.**
  `(ReturnCode::Cancelled(_), _) => code` in `cancel_write` (5562) and `cancel_read`
  (5737) accepts every event discriminant, but these functions serve futures as well
  as streams. The `bail_bug!("unexpected code/event combo")` it replaced was the
  only detector for a mis-queued `Cancelled` event on futures and non-forward
  streams. Scope the arm to `Event::StreamForward { .. }` plus the specific
  graft-routed stream events that need it.

- [ ] **`futures_and_streams.rs:5483` — `wait_for_*` accept `StreamForward` for
  every waiter.** Which events are legal is a property of the caller, not of the
  wait primitive; the pre-existing `bail_bug!` was that assertion. Plain
  `guest_write` (4891), `guest_read` (5201) and the non-forward `HostReady` cancel
  branches (5620, 5801) all route through these. Pass the expected discriminants in.

- [ ] **`futures_and_streams.rs:4345` — one side's stride advances both sides.**
  `forward_copy` derives `item_size` from the writer's type arena (4287-4293) and
  uses it to re-park both the writer's and the reader's buffers. Safe today: the
  handle table forces both ends to one `TypeStreamTableIndex` and every stream-end
  transfer enforces payload equality rather than covariance. But the `flat_abi`
  guard is vacuous for pointer-bearing payloads (`None != None`), `Instance::copy`
  deliberately keeps `write_abi.size32` and `read_abi.size32` apart (4160, 4176),
  and `guest_write` (4718) / `guest_read` (5028) each use the restored side's own
  arena. Assert the two sizes agree, or derive each stride from its own side.

## Cleanups

- [ ] `ForwardState` lives as two hand-synced `Copy` snapshots, in the source's
  `ReadState::Forwarding` and the destination's `WriteState::Forwarding`, with no
  single source of truth. `request_forward_cancel` writes the cancel flag into the
  destination's copy during a produce rendezvous and the source's during a consume
  one; `finish_forward_produce`/`_consume` must each know which slot is
  authoritative and re-read it rather than trust their own `&ForwardState`
  parameter, which is stale; `forward_produce_graft`/`forward_consume_graft` exist
  only to rediscover the surviving copy by comparing handle reps. Storing it once
  in its own table slot, with both ends holding `Forwarding(TableId<ForwardState>)`
  and an explicit phase, removes both sniffers and the stale re-reads.

- [ ] `finish_forward_consume` (2613-2684) is a mirror clone of
  `finish_forward_produce` (2429-2503); the same mirroring repeats in
  `pipe_forward_to_guest`/`pipe_forward_from_guest` and in the two graft sniffers.
  Two of the bugs above are present in both twins, so a one-sided fix still passes
  the other direction's tests. `settle_forward_rendezvous` already shows the
  consolidation; parameterize the head the same way.

- [ ] Three copy-paste clusters worth extracting: the zero-budget probe settle block
  (3689-3699, 4795-4806, 5104-5115), the 12-line "detach an idle host end"
  destructure (4808-4819, 5117-5129, 5394-5406, 5426-5437 — note it `unreachable!()`s
  where the rest of the file `bail_bug!`s), and `guest_forward`'s sync tail
  (5457-5466), which re-implements the `block_or_wait_for_write` helper this same
  branch introduced.

- [ ] Per-batch waste on the data plane: `forward_copy` recomputes `item_size` and
  re-runs the `allow_intra_component_read_write` check that `Instance::copy`
  performs internally, though both answers are fixed for the forward's lifetime and
  `ForwardState` is `Copy`; the guest-to-guest steady state pays a full
  `clear_forward` + `register_forward` round trip to advance one counter; and the
  graft sniffers run a handle-table resolution on every plain, non-forward cancel.

- [ ] Test scaffolding: the ~45-line component preamble is copied into 51 components
  across the 7 new wast files and 20 more times as WAT strings in
  `component-async-tests/tests/scenario/forward.rs`, where 26 of 27 tests also
  repeat the same 6-line instantiate-and-run driver.

## Checked, no action needed

- Marking handles `Busy` before validation in `guest_forward` does not leave them
  wedged in a live store: a non-`Trap` `anyhow` error out of a component libcall
  converts uniformly through `HostResult`/`TrapSentinel::NegativeOne`, and
  `invoke_wasm_and_catch_traps` calls `store.0.set_trapped()`, so `may_enter`
  rejects the store permanently and the handles are unreachable. Same for
  `forward_copy`'s intra-component `bail!`. This matches `guest_write`'s existing
  `bail!`s.
- `cargo fmt` is clean on the touched crates, and no commit on the branch carries an
  AI `Co-Authored-By` trailer.
