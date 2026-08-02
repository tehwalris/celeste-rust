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

### Tried: make input fan-out expand lanes. It is worse.

`btn` was rewritten branch-free, with a `__concretize` builtin doubling the lane
space instead of an `if` splitting the state. Lane content came out
byte-identical over 25 frames (`rewrite observe`), and it was a clear
regression: 4.47 s / 1.12 GB became 5.46 s / 1.72 GB, with **the fragment count
unchanged at 576**.

The split simply moved into the caller. `if btn(k_jump) and ...` now branches on
a lane-varying bool and produces the same two states one branch later - except
that the old version had something going for it that this analysis missed:
duplicating the state leaves the button a **scalar** in each copy, so every
downstream condition derived from it is uniform and therefore free. Expanding
lanes makes it a vector, those conditions start splitting for real, and every
derived value becomes per-lane storage.

So `UnknownBool` branching to both edges is load-bearing. A fragment is a
scalar-specialised copy of the state, which is why 93% of branches are free.
Lane expansion only pays once the frame is already branch-free and there is no
downstream branch left to absorb the split - a late-stage change, not an early
one. Reverted; details in BENCHMARK_DATA.md.

## Where that leaves things

Three ideas that looked good from the profile have been measured and rejected:
`Arc<Vec>` for gc (3%, not 20%), `if_convert` on the `and`/`or` triangles (4.6%
of splits), and lane expansion for input fan-out (a regression). The consistent
lesson is that **fragmentation is not obviously wasteful** - each fragment is a
state in which more values are scalar, and scalars are what make the interpreter
fast. Details for each are in BENCHMARK_DATA.md.

So the 60% spent merging is not a mistake to undo cheaply. It comes down either
by making the merge itself cheaper (`merge_groups` 26%, `gc` 20%,
`shape_grouping` 9%, `dedup_state` 9%, all per state on ~576 states per frame -
bounded, unexamined, and independent of the program's shape), or by reaching a
branch-free frame where there is one state and no merge. The second is the plan.

## Next: finish stage B, which is what unblocks stage D

Surveyed in `plans/rewrite-plan.md` section 10. 305 calls remain:

* **200 are native builtins** (`error` 54, `__print` 54, `mget` 17, `max` 16,
  `min` 16, `flr` 10, ...). A builtin is already a leaf operation, so these cost
  nothing at run time. They cost something at rewrite time only because
  `if_convert` rejects any arm containing a `Call`.
* **72 are method dispatch** (`.init` 32, `.collide` 13, `.check` 10,
  `.is_solid` 7) and are blocked by **captures, not dispatch**. `inline::apply`
  never cared how the closure was obtained; only `candidates` restricts itself
  to globals. All seven `obj.*` methods capture exactly one cell, holding the
  object itself, and that cell is `alloc`ed, stored once, and never rewritten.
* **33 have a callee defined in another block** and need cross-block candidate
  detection.

Two rules, in this order:

1. `promote_capture { fn, index }` - captured *cell* becomes captured *value*.
   Whole-program rule keyed by the callee, since one `FunDef` is shared by all
   32 closures made from it.
2. `AssertClosure` grows `captures: Vec<LocalId>`, and `inline` binds the
   callee's capture ids to them. After (1) the capture of `obj.collide_49` *is*
   the object, which `o:collide(...)` already passes as argument 0. Before (1)
   there is no local at the call site holding it, which is why the order
   matters.

Then stage D over the enlarged candidate set, then stage E for the loops.

And the thing to keep in view: the two biggest splitting sites are the `btn`
reads, 26% of splits, and no rewrite removes them. They stop mattering only when
the frame is otherwise branch-free and the inputs can fan out across lanes
instead of states - one state per frame, no merge. **None of the intermediate
stages collect that payoff**, which is worth remembering when the next one also
measures flat.
