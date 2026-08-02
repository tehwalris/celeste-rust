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

## Stage D landed, and it barely helped

`if_convert` works: 81 sites, `Select` in the IR and interpreter, an
independent verifier, `suggest if-convert` and `screen` to choose sites. Two of
84 candidates were dropped, for exactly the two predicted reasons - a select
that could not combine a bool with a number, and an arm that computed `-2 % 8`
on lanes that would have skipped it. Both were found by running, not by
analysis, which is the bargain the design makes.

    before   4.47 s   1.12 GB   576 mean fragments   28190 filter_branch
    after    4.59 s   1.06 GB   558 mean fragments   26886 filter_branch

81 conversions removed 4.6% of the splits.

## Why, and what to do instead

`rewrite bench --profile` now attributes each conditional branch to its source
block and says whether it split:

    branches: 21862 of 333379 executions split the state, across 33 distinct sites

**93% of branch executions are free.** All the fragmentation is in 33 places,
and `if_convert` was aimed at the wrong ones - `and`/`or` conditions are almost
always uniform across lanes. The full table is in BENCHMARK_DATA.md.

The top two sites are the same code twice: the `if` inside `btn()` in
`lua/builtin_level_4.lua`, inlined at two call sites. 2838 splits each, **zero**
uniform executions, 26% of all splits between them - and, because they duplicate
the state early in the frame, they multiply every split downstream.

That branch is the search's own branching factor. `__button_states[i]` starts as
an `UnknownBool`, and `flow.rs` sends a state branching on `UnknownBool` down
*both* edges unfiltered, so `btn` doubles the state count. No program rewrite
can remove it, because it is what enumerates the inputs.

But it does not have to be modelled as **state duplication**. The same
information fits in one state with twice as many lanes: `Bool(Vector([false; n]
++ [true; n]))` for the button, every other value repeated. Total lanes are
identical either way; the difference is the number of *states*, and every part
of the 60% merge cost is per state.

### Next: make input fan-out expand lanes instead of duplicating states

1. Replace the Lua `btn` with a native builtin that, given an `UnknownBool`
   button, returns **one** state of 2n lanes rather than two states of n. The
   builtin signature already allows it - `Vec<(State, Value)>` can be a
   singleton.
2. Measure the fragment count, not the `filter_branch` share.
3. If it works, look at whether `UnknownBool` branching in general should expand
   rather than duplicate. `__split_by_flr` partitions lanes rather than
   duplicating, which is why it costs 0.5% instead of 26%.

Unknown until measured: whether halving states at the button read compounds
through the downstream branches or is absorbed by them. The mechanism is cheap
to test, which is the main argument for testing it first.

After that, section 4's stage E (`peel`) for the loops.
