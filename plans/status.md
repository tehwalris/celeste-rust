# Status (2026-08)

Branch `rewrite`. Build is warning-free, 226 tests pass, working tree clean.

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
| + `cse`, stage C piece 1 | 4.32 s | 1.06 GB |
| + `demote_create` x46, `pin_builtin` x18 | 4.33 s | 1.06 GB |

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

## Stage D, first round: 81 conversions, 4.6% of splits

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

### Tried and reverted: input fan-out as lane expansion

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
| + block-local `cse` | 4.38 s / 1.06 GB | 558 |
| + cross-block `cse` (accessors and loads) | 4.32 s / 1.06 GB | 558 |

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

## How progress is measured

Use `measure_k`, not the fragment count.

`measure_k --sequences 120 --frames 45` runs the concrete interpreter over
sampled input sequences and reports K, the static size of a fully inlined,
fully unrolled frame body, broken down by instruction kind. It takes 3 seconds.

    ./target/release/measure_k --sequences 120 --frames 45              # the recipe program
    ./target/release/measure_k --sequences 120 --frames 45 --original   # the baseline

                              original   rewritten
    dynamic instrs/frame mean     2122        1715
    dynamic instrs/frame max      6422        5252
    distinct blocks reached        450         375
    K, fully unrolled             8321        7880
    K, loops kept as loops        2963        4170

The last row rises because inlining duplicates code statically. The row above
it is the one that matters for a branch-free kernel.

The breakdown by kind is the work list. `arith`, `const` and `select` are the
irreducible core a compiled kernel would emit; every other category has to
reach zero.

                original    rewritten
    heap          48.8%       43.9%     stage C
    terminator    22.3%        9.5%     if_convert
    call           3.9%        1.6%     stage B
    global         5.1%        5.5%
    phi            5.7%        3.7%
    guard             -        3.3%     cost of the runtime-checked rules
    arith          7.6%       25.8%     core
    const          6.5%        6.7%     core

Two things this replaces:

* **Fragments per frame cannot move until the end.** It has been 558 through
  every change in this file. The plan predicts that: fragmentation only drops
  when the frame is branch-free enough that input fan-out can move to lanes.
  Judging intermediate work by it produces false discouragement.
* **Instruction count of the rewritten program is not the metric either.**
  Cross-block `cse` removed 1454 instructions and made the frame 4% slower, by
  keeping values live across the blocks in between.

Both `measure_k` numbers were wrong until 2026-08. It compiled from source and
never replayed the recipe, so K described a program nobody runs. Pointing it at
the real program then reported K as 64685, because room-load frames were
excluded by matching the function name `load_room_60` and the recipe inlines
that function away. The room is now read from the state instead. Filters keyed
on names the rewrites are designed to erase will keep breaking this way.

## Stage C, in progress

Heap traffic is 43.9% of K and is the largest remaining category. It is also
what blocks stage D: an `if_convert` arm cannot be speculated if it might
mutate the heap.

`suggest if-convert` and `bench --profile` together price the work. The profile
joins each candidate triangle to whether its branch actually splits the state,
which matters because 93% of branch executions are uniform across lanes and
converting a uniform branch is worse than leaving it - the arm stops being
skipped and runs every time.

    triangles           count ever split     splits   % of all
    convertible            54          6       3072      15.7%
    blocked               104          4       3523      18.0%
    all branches            -          -      19573     100.0%

    state    function             join                        splits  in the way
    blocked  player.draw_22       if_join_222                   1499  store
    ready    player.update_21     in_i1_075_and_or_join_603     1356  -
    blocked  player.update_21     if_join_116                   1052  store
    ready    player.update_21     in_i1_076_and_or_join_603      768  -
    blocked  player.update_21     if_join_119                    684  store
    ready    player.update_21     in_i1_079_and_or_join_603      508  -
    blocked  player.update_21     if_join_98                     288  store
    ready    player.update_21     in_i1_080_and_or_join_603       72  -

Nearly all splits are concentrated in these few sites plus `btn`, which is the
search's own input fan-out (2838 x 2, irreducible by rewriting; it becomes lane
expansion once the frame is otherwise branch-free).

### Done

* **`cse`, both halves.** Block-local (3833 folds), then across blocks by
  available expressions. Cross-block reuse is restricted to heap accessors and
  loads; pure arithmetic stays block-local because keeping it live costs more
  than recomputing it. `cse` is barrier-bound rather than scope-bound:
  `player.update_21` has an accessor barrier every ~12 instructions and a load
  barrier every ~7, so better scope buys little. It unblocked no triangles,
  because every accessor blocking one is a `create`, and `cse` only folds reads.
* **`demote_create`.** Turns `get_field %r.f create` into a plain read plus an
  `assert_pointer`. `create_sites` (under `bench --profile`) measures 45
  creations in 107554 executions of `get_field ... create` and 0 in 30599 of
  `get_global ... create`; the creations are object construction. 46 of 48
  candidate sites hold; the 2 that do not are in `player_spawn.update_24`.
* **`pin_builtin`.** Turns `call %f(..)` into `call_builtin "max" via %f(..)`,
  which is the same call after asserting the callee is that builtin.
  `fixed_env::PURE_BUILTINS` (min, max, abs, flr) is one list used both by
  registration and by `is_speculatable`, so they cannot drift.
  `add_pure_builtin` registers one implementation under both signatures. 18
  sites. This moved blocked triangles from 31.8% of splits to 18.0%.
* **Asserts are speculatable.** `is_speculatable` now separates instructions
  that can go wrong silently (stores, allocs, calls, creating accessors - no
  rollback exists, so these stay refused) from ones that can only go wrong
  loudly (the asserts). All 35 triangles blocked by nothing but an
  `assert_closure` survive a 34-frame differential run.

### Not applied, and why

82 `if_convert` candidates passed screening and were not added to the recipe:
all of them split zero states. Applying them would run their arms
unconditionally and save nothing. This is the same misfire as the original 81
conversions, which were chosen on convertibility alone and made the frame
slower. Check `bench --profile` before applying an `if_convert`.

### Next: the store in the arm

Four sites, 3523 splits, 18.0%. Turn an arm's `%c = get_field %r.f create;
store %c <- %v` into a load before the branch, a `select` at the join, and one
store after it. `demote_create` already handles the `create` half.

### Then: the `and`/`or` ternary

The four `and_or_join_603` sites are `appr` inlined:

    function appr(val,target,amount)
      return val>target and max(val-amount,target) or min(val+amount,target)
    end

Converting one triangle at a time fails, because the `and` join combines a bool
with a number and `Select` carries one type tag per value, not per lane:

    at %1351 = select %1339 ? %1344 : %1339
    select cannot combine Number(...) and Bool(...) per lane

The mixed value never escapes, though: the `or` join takes it only when it is
truthy, and truthy means it came from the arm, so it is the number. Converting
the pair together gives `select %1339 ? %1344 : %1349`, both numbers. That
needs one static fact - that `%1344` is always truthy - which `pin_builtin`
supplies, since a numeric `CallBuiltin` returns a Number and every number is
truthy in Lua.

Lifting the truthiness test above the select is a valid identity
(`truthy(select(c,a,b)) == select(c,truthy(a),truthy(b))`) but does not apply
here: all six hot triangles store their phi value, so truthiness is not its only
use. `interpret_not` also rejects numbers, so `Truthy` would be a new operation.

### Later: `assume_eq`, whole-function field promotion

`assume_eq` handles pointers that reach one cell by different paths, which CSE
cannot see - the `foreach` callback gets the object as `%2` while the loop also
reaches it as `objects[i]`. Inserts `assert %a == %b` and substitutes.

Whole-function field promotion loads a field cell once, works in SSA, and
stores back before every `Return`. 142 of 146 field cells written in
`player.update_21` are written exactly once. It is blocked by the calls that
remain in that function, any of which could reach the cell.

## Screening

`rewrite screen --candidates FILE --frames 34` tries candidates by group
testing: the whole set at once, and on failure split in half and retry each
half on top of what has been accepted. Roughly `k*log(N/k)` runs for `k` bad out
of `N`, against `N` before. 38 candidates with 3 bad took 15 runs; 18 candidates
with 0 bad took 1.

Every acceptance runs the differential on the cumulative program, so the
accepted set is always one that has actually been run end to end.

Screen at the depth the result will be used at. Failures cluster at frames
26-32, so a 20-frame screen passes candidates that a 34-frame screen rejects.

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
