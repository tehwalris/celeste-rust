# Status (2026-08)

Branch `rewrite`. Build is warning-free, 167 tests pass, working tree clean.

## Measured, frame 34 (the standard iteration benchmark, ~6s)

| program | time | memory |
|---|---|---|
| original | 6.64 s | 1.75 GB |
| + `promote_cell` (130 cells) | 5.47 s | 1.12 GB |
| + slot plumbing (identity map, no-op) | 5.69 s | 1.12 GB |

Frame 40, run once as a milestone check: 84.1 s / 25.8 GB -> 62.5 s / 14.45 GB.
That was stage 1's target (beat the old unverified mem2reg's 15.2 GB).

Frame 40 takes over a minute; keep routine runs at frame 34 or below.

## Done

* **Stage 0** - the rewrite pipeline. `src/rewrite/`: program, recipe, rules
  (each with an independently-written verifier), dominance-aware validation,
  differential execution. `rewrite build | print | diff | check | verify |
  bisect | bench | suggest | slots`.
  30 frames of the real abstract search costs 1.7 s and covers 15,250 lanes, so
  every rewrite is differentially verified against an enormous number of
  concrete behaviours.
* **Stage 1** - `promote_cell`. 130 cells, converged in one round.
* **Stage 2** - `inline`. Works and verifies (183 inlines), **parked** because
  it was a 12% regression. See `plans/inline-parked.md`.
* **Slot allocation step 1** - `LocalId` (logical, SSA) separated from slot
  (physical, dense) in `LocalEnv`, with an occupant array so a bad allocation
  fails loudly. Landed with the identity map, verified to be a no-op.

## Next: slot allocation step 2

The remaining work, in order:

1. **Thread `SlotMap` through the program.** Currently `LocalEnv::new()` always
   uses the identity map, so nothing can yet supply a real one. Needs:
   - `SlotMap` moved to `ir.rs` (it is part of the program representation, and
     `ir` should not depend on `interpreter`)
   - a `slots` field on `FunDef`
   - `PreparedCfg::with_slots`, populated by `FixedEnv::add_fun_def`
   - `core_interpreter.rs:411`, where the callee's environment is built, to use
     the callee's map
   - the two entry chunks (`__init`, `__frame`), which are driven through
     `interpret_cfg` rather than as functions
2. **The `allocate_slots` rewrite.** Rebuilds only the slot table, never the IR,
   so its diff is trivially reviewable. Linear scan over live ranges is ample -
   max simultaneous liveness is 12-18, so packing quality hardly matters.
   - verifier: independently recompute liveness (`src/rewrite/liveness.rs`) and
     check no two simultaneously-live values share a slot. That is a static
     proof; the occupant array is the backstop against the liveness analysis
     itself being wrong.
   - watch for the parallel-copy hazard: `flow_block_phi` assigns phis
     sequentially, so an allocation that makes two phis in a block swap slots is
     wrong. Simplest fix is to refuse to coalesce into a cycle.
3. **Re-land the 183 inlines** (already written and verified) and re-measure.
   The packed slot count barely moved under inlining, so this should now be a
   clear win rather than a regression.
4. **If-conversion** - `Select` in the IR, then `hoist` and `phi_to_select`.

Expected from (2): `player.update_21` from 848 slots to 12, `__frame` from 1508
to 15. Filtering is 33% of runtime and clones the environment every time.
