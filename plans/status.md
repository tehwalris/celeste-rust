# Status (2026-08)

Branch `rewrite`. Build is warning-free, 271 tests pass, working tree clean.

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
| + 4 store-blocked triangles converted | 3.36 s | 0.94 GB |
| + 4 `convert_ternary` (the `appr` pairs) | 2.62 s | 0.81 GB |
| + `decompose_truthy` x15, 3 more conversions | 2.36 s | 0.74 GB |
| + 2 diamonds absorbed (`absorb_stores`) | 1.99 s | 0.64 GB |

Lane count is identical throughout (92,713), which is the first thing to check
when a rewrite claims a win. Frame 37 confirms the same ratios (7.87 s, from
13.0 s before the store triangles). Keep routine runs at frame 34 or below;
frame 40 takes over a minute.

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
    dynamic instrs/frame mean     2122        1731
    dynamic instrs/frame max      6422        5283
    distinct blocks reached        450         359
    K, fully unrolled             8321        7874
    K, loops kept as loops        2963        4164

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

* **Fragments per frame only move when a *splitting* branch is removed.** It
  was 558 through every change up to and including `cse`, because none of
  those changes removed a branch that ever split - inlining relocates
  branches, `cse` folds reads, and the original 81 `if_convert`s were aimed at
  branches that ran uniformly. Converting the four store-blocked triangles,
  which really did split, took it to 335. So the number does respond, but only
  to that one kind of work; judging anything else by it produces false
  discouragement.
* **Instruction count of the rewritten program is not the metric either.**
  Cross-block `cse` removed 1454 instructions and made the frame 4% slower, by
  keeping values live across the blocks in between. In the other direction,
  the store conversions *raised* K by 11 (guards, arms running on every lane)
  and cut frame time 22%.

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
    convertible            42          0          0       0.0%
    blocked               101          0          0       0.0%
    all branches            -          -       5345     100.0%

**Every triangle, every `and`/`or` construct, and every convertible diamond
in the program is now silent.** The four store-blocked sites (3523 splits),
the four `appr` ternary pairs (2416), the ternary variants and everything
they exposed (546), and the two diamond-stage sites (1056 direct, 2457 with
the downstream effect) are all converted - see "Done" below. What still
splits:

    site                                          splits   what it is
    in_j2_042/046/050/052_if_join_10 (btn x4)       2540   input fan-out
    if_join_95, if_join_133/136, if_body_135        1706   blocked diamonds (below)
    anonymous_61/update_21 if_join_413, for_heads  ~1100   object loop bodies
    (in_k03x sites are the same loop body inlined)

`btn` is the search's own input fan-out (irreducible by rewriting, it becomes
lane expansion once the frame is otherwise branch-free). The three blocked
diamonds are priced in "Next" below; the object loops need unrolling or
specialization, which no select can express.

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
* **The store in the arm.** Two new rules turn a store-blocked triangle into
  straight-line code, applied per site as `speculate`, then `sink_store` per
  store (innermost-last first), then `if_convert`:
    - `speculate` hoists the arm's speculatable instructions into the head so
      the store's target pointer comes to dominate the branch. Its own
      obligation is the crossing: a hoisted instruction that moves above a
      store must commute with it. Non-heap instructions and non-create
      accessors always do (a store writes cell contents, never the
      name-to-cell map); a `load` only past a store to a **provably distinct
      cell**, and the one distinctness fact implemented is two `get_field`s on
      the same base with different names.
    - `sink_store` moves the arm's trailing store past the join: `load %p` in
      the head, `phi [arm: %v, head: %old]` at the join, one store after it.
      On the skipped path this stores back what it loaded, which is only the
      identity for a cell holding a plain value - `load` on a closure or
      table cell returns a pointer to the cell itself, and storing that back
      would corrupt it silently. A new `assert_value_cell` guard states that
      premise and fails loudly in the one case the roundtrip is not the
      identity.
  All four sites (13 recipe entries) screened clean in one 34-frame run.
  Frame 34: 4.33 s -> 3.36 s, fragments 558 -> 335, splits 19573 -> 11978.
  `player.draw_22` is now entirely branch-free.
* **The `and`/`or` ternary.** `convert_ternary` takes the double triangle that
  `a and b or c` compiles to in one step: both arms into the head, the
  `and`-join emptied, the `or`-join's phis become selects on the original
  condition. The intermediate phi - the value that mixes a bool with a number
  and made one-at-a-time conversion impossible - is deleted, replacing it with
  the `and`-arm's value on the edge where they provably agree. The one static
  fact required is that the `and`-arm's value can never be falsy; `pin_builtin`
  supplies it, since pure builtins return Numbers and no number is falsy. The
  rule refuses if the phi has any other use. Applied to the four `appr` copies
  in `player.update_21` that actually split (2416 splits); four more matching
  pairs split zero states and were not applied. Frame 34: 3.36 s -> 2.62 s,
  fragments 335 -> 232, splits 11978 -> 8472, K down 12.
* **`decompose_truthy`.** The design sketched below, built. Every value in a
  mixed `and`/`or` cascade splits into a (truthiness, value) pair of
  homogeneous selects - `c and y` becomes t = c, n = y (the select is
  *deleted*); `x or z` becomes two selects over the children's pairs; a phi
  splits into a t-phi and an n-phi, with a bool edge accepted only when the
  predecessor provably branches to the phi on that bool's false side (then a
  minted dead zero keeps the n-phi well-typed). The root must be `x or k`
  with `k` statically truthy: the cascade can then never be falsy, so the
  root keeps its id and consumers are untouched. All 15 cascades in the
  program (every inlined `sign()`, `flip.x and -1 or 1`, and friends)
  screened clean in one 34-frame run, defusing every mixed-select mine at
  once. That let plain `if_convert` take the two triangles the tail variant
  had failed on - and when removing the `sign` split at `in_i1_077` exposed
  the downstream `appr` pair (its 144 splits moved one block down, to a
  branch that used to run uniform), `convert_ternary` took that too. The
  exposure cascade then went quiet: zero triangle splits remain. Frame 34:
  2.62 s -> 2.36 s, fragments 232 -> 212, splits 8472 -> 7802, K 7879 ->
  7874. The unapplied tail shape of `convert_ternary` is subsumed and was
  removed (recoverable from git if a cascade without an always-truthy root
  ever appears).
* **The first diamonds (`absorb_stores`).** Two sites, both in
  `player.update_21`. `if_join_108` is `spd.x = abs(spd.x) > maxrun and
  appr(..) or appr(..)` - a true diamond, both arms doing work and storing to
  `spd.x`. `if_condition_92` is the grace-counter decrement, a one-store
  triangle that was invisible to `find_triangle` because its join has a third
  predecessor. One pipeline handles both, all but one rule pre-existing:
  `merge_blocks` collapses each arm's block chain, `demote_create` demotes
  the arms' creating accessors, `speculate` - extended to accept a *named*
  arm, whose shape requirement is exactly the hoisting soundness argument
  (only predecessor is the head whose branch names it) - empties each arm
  down to its store, and `cse` unifies the two arms' address chains so both
  stores name one local. The one new rule, `absorb_stores`, then replaces the
  branch with a select-store *in the head*: both-arms-same-local needs no
  guard (every path stored to that cell; same local means same cell, no
  aliasing argument), one-arm reuses `sink_store`'s guarded load-back trick.
  The join is never touched, which is what makes the shared-join shape and
  any future N-predecessor join reachable - the price is a phi-free join,
  checked. Crucially the *paired* store never went through `sink_store`:
  sinking two same-cell stores independently is silently wrong (the second
  load-back overwrites the first arm's value), so the pair had to be one
  rule. Screened clean in one 34-frame run. Frame 34: 2.36 s -> 1.99 s,
  mean fragments 140 (was 212), splits 7802 -> 5345 - 1056 direct and
  another ~1400 downstream, the multiplier at work. K 7874 -> 7770.
  Exposure check: `if_body_135` (the nested grace branch of the jump arm)
  woke up at 96 splits; it is blocked by the same `is_solid` loops as its
  parent.

### Not applied, and why

82 `if_convert` candidates passed screening and were not added to the recipe:
all of them split zero states. Applying them would run their arms
unconditionally and save nothing. This is the same misfire as the original 81
conversions, which were chosen on convertibility alone and made the frame
slower. Check `bench --profile` before applying an `if_convert`.

### Next: the three blocked diamonds, all waiting on something bigger

The five diamond/chain sites in `player.update_21` were read and tabulated
this session; two were convertible (see `absorb_stores` above) and three are
not, each blocked by a different later stage:

* **`if_join_95`** (840 splits, the largest non-`btn` site) branches on
  `dash_time > 0`. Its true arm is the dashing update - convertible on its
  own - but its false arm is the *entire walking/jumping/dash-start logic*,
  containing everything below. Convertible only after the whole else-arm is
  straight-line, i.e. last.
* **`if_join_133`** (444) branches on the jump input. The grounded-jump path
  is two stores, but the wall-jump path inlines `obj.is_solid`, which is an
  object *loop* (the `in_k03x` sites). Blocked until the object-loop stage.
* **`if_join_136`** (326) branches on the dash input. The dash-start arm
  contains the `btn(k_up)` / `btn(k_down)` reads - input fan-out lives
  *inside* the arm. Blocked until `btn` becomes lane expansion.
* `if_body_135` (96, exposed by this round) is `if_join_133`'s nested
  grace-check; same is_solid blocker.

So shape work is done until a bigger stage lands. The remaining splits are
`btn` x4 (2540 - lane expansion), the blocked diamonds above (~1700, gated on
loops and `btn`), and the object loop bodies (~1100 - unrolling or
specialization; `measure_k` prints the unroll price list, 6 iterations x ~62
instructions each for the top sites).

Expect the exposure cascade whenever a stage removes splits: lanes that used
to arrive pre-sorted arrive mixed, and quiet branches wake up. Re-run
`bench --profile` after each batch - the newcomer may be a shape an existing
rule already takes, as `in_i1_078` was for `convert_ternary` and
`if_body_135` was not.

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
