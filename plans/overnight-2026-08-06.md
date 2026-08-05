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
