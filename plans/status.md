# Status (2026-08)

Branch `rewrite`. Build is warning-free, 194 tests pass, working tree clean.

## Measured, frame 34 (the standard iteration benchmark)

| program | time | memory |
|---|---|---|
| as compiled | 6.64 s | 1.75 GB |
| + `promote_cell` (130 cells) | 5.47 s | 1.12 GB |
| + slot plumbing (identity map, no-op) | 5.69 s | 1.12 GB |
| + `allocate_slots` | 4.65 s | 1.05 GB |
| + 183 `inline`s | 4.47 s | 1.12 GB |
| + 81 `if_convert`s | 4.40 s | 1.06 GB |
| + stage B finished (see below) | 4.36 s | 1.06 GB |

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

## Stage B is finished

`promote_capture` turns a captured cell into a captured value;
`AssertClosure` gained a `captures` list so `inline` can bind a callee's
`capture_ids`; 84 method call sites inlined over three rounds, all screening
clean; then `promote_cell` took the 32 `obj` boxes nothing captured any more.
Details in `plans/rewrite-plan.md` section 11.

| step | frame 34 | fragments |
|---|---|---|
| + 81 `if_convert`s (previous) | 4.40 s / 1.06 GB | 558 |
| + `promote_capture` x7 | 4.46 s / 1.06 GB | 558 |
| + 32 `promote_cell`s | 4.42 s / 1.06 GB | 558 |
| + 84 method `inline`s | 4.34 s / 1.06 GB | 558 |
| + 16 more `if_convert`s | 4.36 s / 1.06 GB | 558 |

**The fragment count did not move once**, which was the predicted shape:
inlining relocates branches, it does not remove them. Instructions went
13109 -> 23362.

Two things this stage taught that were not in the plan:

* **The differential check had to give ground.** Removing the box changes the
  frame-boundary heap (272 cells -> 271), and the whole Tier 2 story assumed
  that was invariant. `observe_state` now normalises both sides so a closure
  capture is observed by the value it denotes rather than the box holding it.
  Narrow, stated, applied to both sides - and this premise will keep breaking,
  because stage C removes heap cells wholesale.
* **20-frame screening is not deep enough.** All 19 `if_convert` candidates
  passed at 20 frames; three failed at 34, at frames 29 and 32. Screen at the
  depth the result will be used at.

## Stage C, in progress - and it is the blocker, not a nicety

`suggest if-convert` reports what stands in the way of stage D:

    # 3 if-convertible join(s)
    # 155 more triangle(s) blocked by unspeculatable arms:
    #       89  store
    #       68  call
    #       41  assert_closure
    #       41  get_field

Inlining converted each `call` into a body full of stores and creating field
accesses. Those are unspeculatable for a better reason than the call was: they
mutate the heap. So removing the calls renamed the obstacle rather than
removing it, and **stage C is what makes stage D possible at all**.

Three pieces:

1. **`cse`** - one instruction per value, so each cell has a single accessor.
   Promotion's precondition is "no other instruction can produce a pointer to
   this cell", and inlining left 1675 redundant accessors (`get_field
   %2.hitbox` appeared 124 times in one function).
   * **Block-local: done.** 3833 instructions removed, 23362 -> 19529.
     Redundant accessors 1675 -> 597.
   * **Cross-block: next.** The remaining 597 need an earlier definition in a
     *dominating* block with no barrier on *any* path between - a real dataflow
     question rather than an interval scan. Promotion needs function-wide
     uniqueness, so this is not optional.
2. **`assume_eq`** - for pointers that reach one cell by different paths, which
   CSE cannot see. The `foreach` callback gets the object as argument `%2`
   while the loop also reaches it as `objects[i]`; nothing syntactic connects
   them. Inserts `assert %a == %b` and substitutes, converting an aliasing fact
   into syntactic identity. Sound by construction.
3. **Field promotion.** Unlike an `alloc` cell, a field cell *is* the game
   state, so it cannot simply be deleted: load once where the pointer is first
   available, work in SSA, store back before every `Return`. Inside the frame
   no heap traffic; at the boundary an identical heap.

   Measured, and it is less alarming than expected: of 146 field cells written
   in `player.update_21`, **142 are written exactly once** and only 4 need
   multi-store SSA construction with phi insertion. So most of this is the
   existing `promote_cell` shape plus the load-at-entry / store-at-return
   bracketing, not a general mem2reg.

## Keep replay fast

`rewrite build` prints where recipe replay went:

    replay: 2.0s total - clone 0.1s, apply 0.3s, validate 0.9s, verify 0.7s

It reached **67s** during this session before anyone measured it, and since
every command replays the recipe first, that was added to everything. 97% was
whole-program dominance validation after every entry, on a program that
inlining had grown to hundreds of blocks per function. Fixed by validating only
changed functions, bitset dominators, and a dominator-tree walk.

Watch this line. Replay is O(entries x program size) and both keep growing, so
it will drift again. If it ever needs another order of magnitude, the fallback
is a fast mode that validates once at the end and a conservative mode that
validates per entry - at the cost of losing which entry broke things, which is
what makes `bisect` necessary rather than optional.
