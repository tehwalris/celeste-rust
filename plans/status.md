# Status (2026-08)

Branch `rewrite`. Build is warning-free, 171 tests pass, working tree clean.

## Measured, frame 34 (the standard iteration benchmark)

| program | time | memory |
|---|---|---|
| as compiled | 6.64 s | 1.75 GB |
| + `promote_cell` (130 cells) | 5.47 s | 1.12 GB |
| + slot plumbing (identity map, no-op) | 5.69 s | 1.12 GB |
| + `allocate_slots` | 4.65 s | 1.05 GB |
| + 183 `inline`s | 4.47 s | 1.12 GB |

Lane count is identical throughout (92,713), which is the first thing to check
when a rewrite claims a win. Frame 37 confirms the same ratios at ~13 s. Keep
routine runs at frame 34 or below; frame 40 takes over a minute.

Full numbers and the time breakdown are in `BENCHMARK_DATA.md`.

## Done

* **Stage 0** - the rewrite pipeline. `src/rewrite/`: program, recipe, rules
  (each with an independently-written verifier), dominance-aware validation,
  differential execution. `rewrite build | print | diff | check | verify |
  bisect | bench | suggest | slots`.
  30 frames of the real abstract search costs 1.7 s and covers 15,250 lanes, so
  every rewrite is differentially verified against an enormous number of
  concrete behaviours.
* **Stage 1** - `promote_cell`. 130 cells, converged in one round.
* **Stage 2** - `inline`. 183 call sites. See `plans/inline.md` for why it was
  parked for two months and what unparked it. 305 calls remain.
* **Slot allocation** - `LocalId` (logical, SSA) is separated from slot
  (physical, dense). `Cfg::slots` is part of the program; `slots::allocate`
  colours the conflict graph; `validate` re-checks it after every rewrite; and
  `LocalEnv`'s occupant array is the run-time backstop. 5207 slots over the
  program became 357, against a liveness floor of 346.
* **Profiling** - `rewrite bench --profile` gives a self-time breakdown.

## The measurement that should drive what comes next

Merging states back together **at the frame boundary is 60% of runtime**. The
frame itself is 39%, and `filter_split_flr` - the only semantically necessary
filtering - is 0.5%.

The cause is still intra-frame branching (`filter_branch` fires 830 times per
frame, so a frame ends as ~630 states), so the lever is to produce fewer states.
But it means **if-conversion should be judged by the state count arriving at the
frame boundary**, not by the `filter_branch` share, which slot allocation
already cut from 33% to 13%.

## Next: stage D, if-conversion

Surveyed in `plans/rewrite-plan.md` section 9. The short version:

* The program has **145 triangles** (`cond_br H → A, J`; `A → J`, which is what
  Lua `and`/`or` compiles to) and only 12 diamonds. **84 triangles have fully
  speculatable arms** - no call, store, `create_if_missing` or `assert_closure`.
  33 of those are in `player.update_21`, the hottest function.
* Arms are short: median 2 instructions, 315 in total.
* So this is **one rule, `if_convert { join }`**, not the `hoist` +
  `phi_to_select` pair section 4 proposed. There is nothing for `hoist` to empty
  out.
* `Select` is partial. One `HeapId` per pointer and one type tag per value means
  it can only combine numbers, bools and intervals. Lua's `and`/`or` returns its
  operands, so some of the 84 will be selecting between a bool and a number and
  must fail loudly rather than widen.
* Expect to apply all 84 one at a time and keep the ones that verify.

Order of work is in section 9. After that, section 4's stage E (`peel`).

## Second lever, unscheduled

`gc` is 21% of runtime at ~45 us per state. It copies every heap value, and a
heap value holds a `MaybeVector::Vector(Vec<T>)` of per-lane data, so the cost
is (cells x lanes) even though gc only renumbers `HeapId`s and never touches
lane data. Making that `Arc<Vec<T>>` would make gc nearly free and would
probably help `merge_groups` and `dedup_state` too. This is close to what the
old `barrier` branch did. Worth roughly 20% on its own, and unlike if-conversion
it does not depend on the program's shape.
