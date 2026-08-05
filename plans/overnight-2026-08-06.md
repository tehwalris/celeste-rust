# Overnight ledger - 2026-08-06 (unsupervised session)

Philippe's directive at bedtime: finish the task list (deopt architecture,
task #78), then keep optimizing freely. Objectives: (a) get as far as
possible in frames within ~2 minutes of wall clock, (b) drive the
extrapolated time-to-final-frame (our frame 100 for room (1,0)) as low as
possible. Morning review decides what to keep.

Continues plans/overnight-2026-08-05.md (frontier-only search, widencheck,
the f59 premise-guard diagnosis, and the REJECTED death pruning - see the
correction there; the fix is the deopt architecture below).

## 1. Deopt architecture (task #78) - LANDED

Philippe's design: canonical state = the plain program's cross-frame
representation (the heap contract `verify` observes). Every rewritten
program is a specialization with a mechanical mapping to/from canonical,
derived from the recipe. Today exactly one rule changes the cross-frame
representation - `promote_capture` - so the mapping is its 7
`(closure fn, capture 0)` pairs (the obj.* methods): `from_canonical`
unboxes those captures, `to_canonical` reboxes them (fresh write-once cell
per capture; behaviorally identical because the box is write-once/read-only
by the rule's precondition).

Pieces:

* `src/rewrite/state_mapping.rs` - `StateMapping::from_recipe`,
  `to_canonical` / `from_canonical`, both loud on unexpected
  representations (double-conversion is an error, never a silent skip).
  Unit tests: roundtrip preserves `observe_state`, each direction refuses
  a state already in its target representation.
* `AbstractRun::start_with_deopt(program, plain, mapping, force)` - holds
  the plain program's own `PreparedCfg` + `FixedEnv` (its fun defs
  differ). `step()` runs each boundary state optimistically under the
  specialized program; on failure (premise `assert_true` Err, or a panic -
  same class `screen_trial` unwinds across), the frame-input snapshot
  (cheap: heap storage is Arc-shared) is mapped to canonical, the frame
  re-runs under plain, outputs map back. Deopt events are counted and the
  first trigger per frame is printed. An error under plain too is
  terminal - plain is ground truth.
* `rewrite deoptcheck --frames N` - certification in the widencheck
  spirit: force deopt on EVERY state of EVERY frame (specialized ->
  to_canonical -> plain -> from_canonical) and compare observations
  frame-by-frame against a plain run. GREEN at 34: 235 forced round
  trips, all identical.
* `rewrite bench --deopt` - opt-in for real runs. Deopt stays OFF in all
  verify/screen paths so it can never mask a rewrite divergence.

Why respawn dedup works: `vectorize_states` gc's every state at the
boundary (canonical renumbering), and the timer globals are pinned, so a
respawned player is bit-identical to the original spawn lineage and the
frontier-only visited set absorbs it.

Gates: state_mapping unit tests green; deoptcheck 34 green; verify 34 +
full test suite + deep run results recorded below as they complete.

## Results (filled in as runs complete)

* deoptcheck 34: GREEN (235 forced deopts, identical every frame).
* verify 34: GREEN (12.1s). Full cargo test suite: GREEN.
* **Deep run PAST THE f58 WALL** (frontier-only + deopt, --frames 62):
  completed in 179.9s / 16.36 GB peak - note this beats the old
  f58-and-stop run (476s / 34.5GB, non-frontier) while going 4 frames
  further. Deopt events: f59 4 states/719k lanes, f60 9/1.77M,
  f61 16/1.93M, f62 21/2.18M (total 50 states / 6.6M lanes re-ran under
  plain). The trigger is exactly the diagnosed premise
  (assert_true %3695, #objects==1, kill frames empty the objects array).
  Frontier new-lanes growth bends from x1.3-1.4 (f36-42) to x1.05-1.10
  (f50-58); f62 frontier = 2.46M new lanes, visited total 27.4M rows.
* Deopt cost observation: a whole boundary state deopts if ANY of its
  lanes dies mid-frame, so by f62 over half the pre-subtract lanes ran
  under plain. The dead-countdown lineages (0 objects for 15 frames)
  also deopt every frame by construction. Two future levers, both in
  the specialization-framework spirit (NO death special-casing):
  (a) finer deopt granularity (catch the premise failure at the split
  state mid-frame, not the boundary state), (b) a second specialized
  program for the dead-room class, once program families exist.

## 2. Win probe + frame-100 attempt

`count_room_x_lanes` (inspect.rs): lanes with global room.x == 2 at a
boundary have exited room (1,0) - next_room() writes the index. bench
now prints per-frame elapsed/lanes/RSS and the first win frame. For the
earliest-arrival frontier search, first win frame = optimal TAS length
under the stated abstractions. Expected: frame 100 exactly (2022-proven
optimum, witness in tas/room_1_0_exit_frame_100.txt) - anything else is
a bug, so the deep run is an end-to-end differential against 2022.

* Frame-100 run (--frames 102, frontier + deopt): launched, results
  below when complete.
