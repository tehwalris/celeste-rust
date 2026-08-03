# Status (2026-08)

Branch `rewrite`. Build is warning-free, 365 tests pass, working tree clean.

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
| + `is_solid` chains eager (`speculate_region`), consumers absorbed | 1.82 s | 0.62 GB |
| + pixel loops masked, wall-jump arm eager | 1.46 s | 0.53 GB |
| + `spikes_at` nest masked (`fuse_breaks` + `span`/`break_to` `mask_loop`) | 1.40 s | 0.46 GB |
| + 9 `pin_builtin`s, `cse` forward mode (block-local) | 1.35 s | 0.50 GB |

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
loops and `btn`), and the loops (~1100, next).

### Next stage: mask-unroll the movement loops

Light IR reconnaissance split "the object loops" into two families with
opposite properties:

* **The pixel-stepping loops in `obj.move_x`/`obj.move_y`** (`in_k1000`/
  `in_k1001`, inlined into `anonymous_61`). Celeste moves one pixel at a
  time: `for i=1,abs(amount) do if not is_solid(step) then pos += step else
  stop end`. The trip count `abs(amount)` is **per-lane data** (speed differs
  across lanes), so the loop-continue branch itself splits
  (`in_k1001_for_head_487`, 238), and the `is_solid` check in the body splits
  too (`if_join_413`, 275). These are the frame-time target.
* **The object-table loops in `check`/`collide`** (`in_k2000`-`2007`). The
  table length is heap data, and the heap is per-*state* - every lane agrees
  on how many objects exist. Uniform trip count, **zero splits**. They are
  most of `anonymous_61`'s 53% of K but cost no frame time; they wait for the
  kernel endgame (unroll or specialize by object type).

Also to pin: `player.update_21`'s `in_k03x_if_join_413` sites (~260, the
same `is_solid` body inlined at the wall-jump checks) and
`in_i1_068_for_head_638` (75).

The mechanism is masking, and the key realisation is that **the recipe
already does masked execution one instruction at a time**: `select` is a
masked move, and `sink_store`/`absorb_stores`' `store p <- select c ? new :
old` is a masked store spelled with unmasked primitives. What is new for
loops is masking *iteration*: unroll to a static bound and fold the
continue/break conditions into a per-lane active mask -

    active_k = active_{k-1} && (k <= abs(amount)) && !solid_at_next_pixel
    x        = select active_k ? x + step : x

Dead iterations compute garbage and keep old values - the same speculation
bargain, same guards. The unroll is not an alternative to masking but what
makes it expressible: the interpreter's model is uniform control flow and
uniform pointers per state, with only scalars varying per lane. A rolled
loop with per-lane trip counts would make "which iteration am I on"
per-lane control state, which has no representation; unrolled, every
iteration's addresses are the same for all lanes and per-lane activity
becomes data.

Block-level reconnaissance of the inlined `move_y` then reordered the
stage. Every one of its splitting branches is a short-circuit gate whose
*operand* is one of the table loops:

* `for_body_start_488` (50) branches on `step > 0` to skip the two
  platform checks (`in_k2004`/`2005`).
* `if_join_413` (275) branches on the `tile_flag_at` result to skip the
  fall-floor and fake-wall checks (`in_k2006`/`2007`) - it is the `or`
  short-circuit of `solid_at(..) or check(fall_floor) or check(fake_wall)`.
* `for_head_487` (238) is the pixel loop itself, whose body is all of the
  above.

So the `or`-chain cannot be made eager per-instruction - a loop is not an
instruction `speculate` can hoist - and the table loops gate everything,
not just K. Three facts make the fix cheap:

* **The collide loop body is already pure straight-line code.** Earlier
  stages (`decompose_truthy`, `cse`) flattened its `and`-chain to selects;
  what remains is loads, non-create accessors, arithmetic, selects, one
  early-exit branch, and a counter phi with `+1` increment and a
  loop-invariant `<= count` bound. No stores, no calls, no creates.
* **`tile_flag_at` is already a Rust builtin** backed by the per-room
  collision cache (`game_runner.rs`), and it never reads the abstract
  state - the room dependence is baked into the cache. Pure function of
  its args for a fixed room; errors are loud. The IR just still sees an
  anonymous `call`.
* The early-exit and head branches of the table loops **never split**
  today: the trip count (`#objects`) is per-state, and the type-equality
  select chain collapses the per-lane position bools to a uniform false
  when no object of the type exists.

The stage order that falls out, cheapest enabler first:

1. **Pin `tile_flag_at`** as a readonly builtin - a second category next
   to `PURE_BUILTINS` (speculatable, but conservatively does not commute
   with stores; our sites cross none). Also makes the `solid_at` chain
   hoistable in `player.update_21`'s `in_k03x` copies.
2. **`speculate_region`** - the triangle generalised to an arm that is a
   whole single-entry/single-exit *subgraph*. Head's branch becomes
   unconditional into the region; region internals are untouched; the
   join's phis become selects on the head condition. Obligations: every
   region instruction speculatable, single entry (only external pred is
   the head), single exit edge to the join, and termination - counter phi
   with positive constant increment and invariant bound, plus a loud
   runtime bound guard rather than an overflow argument.
3. Existing rules flatten the emptied `and`/`or` chain: `if_join_413`
   (275 + ~260 in the `in_k03x` copies), `body_start_488` (50), with the
   usual downstream multiplier - `is_solid` feeds `on_ground` which feeds
   the whole update.
4. **Unroll + mask the pixel loops last** (`for_head_487`, 238) - by then
   the body is straight-line except the uniform table loops, so the
   unroll is the clean self-loop case. A loud static bound on
   `abs(amount)` replaces a speed-cap argument. This is also what
   unblocks `if_join_133`/`if_body_135` (540 splits) later.

Parked: `in_i1_068` in `player.update_21` is inlined `spikes_at` - a
nested 2x2 tile loop with per-lane trip span (75 splits). Same
unroll+mask family, but its early exit feeds `kill_player`, which
mutates; take it after the move loops.

### Steps 1-3 landed (2026-08-03)

`tile_flag_at` went straight into `PURE_BUILTINS` - the builtin never
reads the abstract state, the room-(1, 0) collision cache is captured at
construction, so it is honestly a function of its arguments (and its
silent `as_i16` fallbacks became loud errors on the way). Seven call
sites pinned. `speculate_region` was built with one addition to the
sketch: termination is stated as a runtime guard (`bound < 32767` via the
new general-purpose `assert_true` instruction) rather than an overflow
argument, since `i <= bound < 32767` means a `+1` cannot wrap.

The application taught more than the rule:

* **Converting the chains alone removes nothing.** The `is_solid` result
  still feeds a branch; the splits relocated 1:1 (5345 -> 5345) until
  each *consumer* was converted, and then the downstream multiplier paid
  again. Chains and consumers are one batch, not two stages.
* **The `wall_dir` cascade was a live mixed-select mine.** `is_solid(-3,0)
  and -1 or is_solid(3,0) and 1 or 0` survived the decompose stage only
  because its operands arrived pre-sorted; making them per-lane blew up
  loudly at the first differential. `decompose_truthy` could not see it
  while an interior select fed the `or`-shortcircuit *terminator* - one
  more `speculate_region` on that shortcircuit deleted the branch, and
  the cascade became recognisable. Composition, not new machinery.
* **`absorb_stores` now takes arms with several stores.** Pairing is by
  target local (pair -> unguarded select-store, unpaired -> guarded
  load-back). No aliasing analysis: every triangle emission is
  load-adjacent, so it reproduces the cell's current value whatever
  aliases whatever. The `on_ground` diamond (grace in both arms, djump in
  one) was the driving site.
* **Two new distinctness facts** in `speculate`: a global's cell is never
  a field/index cell, and two differently-named globals differ. That
  unblocked hoisting `max_djump`/`assert_value_cell` past the grace
  store.

Converted this batch: the three splitting `is_solid` chains and the
`in_k034`/`wall_dir` shortcircuit (9 `speculate_region` entries), the
`on_ground` diamond, the gravity `appr` arm, the accel `elseif` chain
(bottom-up: inner triangle absorbed, residue re-speculated, outer diamond
paired), the wall-slide `maxfall` store, the `wall_dir` cascade. Frame 34:
1.99 s -> 1.82 s, fragments 140 -> 108, splits 5345 -> 4280, K 7770 ->
7323 (down - absorbing consumers deleted more than eager regions added).

Deferred, in order of value:

* **The jump/dash cluster** (`if_join_133` 366, `if_body_135` 156,
  `and_or_join_153` 102): blocked one level deep on `if_body_159`, whose
  `spd` re-load must cross the `spd.y` store. The fix is a runtime
  distinctness guard - `assert_true` on pointer inequality - which needs
  `~=` on pointers in the interpreter and an opt-in `speculate` flag so
  existing emissions stay stable. `if_join_136` (249) additionally wants
  the `btn(k_up/k_down)` reads inside its arm.
* **Step 4, the unroll** (`anonymous_61` 599 + `spikes_at` 114): as
  planned above; the `a61` chain conversions (`p017`-`p020` analogues)
  happen as part of it, since their splits only die with the loop.
* `if_join_95` (648) last, once its else-arm is straight-line.

### The pointer guard landed (2026-08-03)

Smaller than planned: `~=` on pointers already existed in `op.rs`
(`TwoEqual` compares `HeapId`s, `TildeEqual` negates it), so the entire
interpreter half was free. What was built is the `speculate` half: an
opt-in `guards` field on the recipe entry
(`"guards":[{"load":"%528","store":"%522"}]`) that permits one declared
load-across-store crossing in exchange for an emitted
`%c = <cell> ~= <target>; assert_true %c` immediately before the hoisted
read. Refusal messages now print the exact guard to declare. A guard that
matches no blocked crossing is refused (typos must not silently weaken
nothing), only `load`/`assert_value_cell` can cash one in (they read
exactly one cell), and entries without guards emit byte-identically.

Applied: `g024` (guarded speculate on `if_join_160`), `g025`
(`absorb_stores` on `and_or_join_153`, killing its branch), `g026`
(`merge_blocks`, -22 blocks; `fold` and `dce` were no-ops and were not
kept). Frame 34: 1.82 s -> 1.78 s, fragments 108 -> 102 mean, splits
4280 -> 4058 (102 direct at `and_or_join_153`, the rest downstream: the
hot `btn` pair 669 -> 618 each). K 7323 -> 7308. Differential identical
through 34 - the guard runs on every state through the jump path and
holds.

Two site renames from `merge_blocks`, for reading future profiles:
`if_join_95` -> `in_i1_074_cont` (648), `if_join_133` -> `and_or_join_126`
(366). The rest of the cluster is confirmed gated as predicted:
`if_body_135`'s false arm *is* the `is_solid` object-loop region (stores
inside a multi-block loop-bearing region - neither `absorb_stores` nor
pure-region speculation applies), and `if_join_136`'s arm holds the
`btn` reads. Both wait on their stages; next is the unroll.

### Step 4 landed as `mask_loop` - no unroll needed (2026-08-03)

The planned "unroll + mask" became **mask without unroll**, and that is
the finding worth keeping: the interpreter's model forbids *per-lane*
control state, not loops. A rolled loop whose trip count is per-state
uniform executes without splitting (the object-table loops always did),
so the pixel loops only needed their per-lane trip count turned into
data. `mask_loop` gives the head a fresh uniform counter (`k <= limit`,
a recipe constant), keeps the original `i <= bound` compare as a lane
mask, folds the break edge into an `active` mask, rewrites every
latch/break store to the load-adjacent masked select-store, deletes the
break block, and plants `assert_true(bound <= limit)` after the bound.
Termination becomes unconditional; inner cycles keep the
`speculate_region` counter-shape obligation (with recognition of guards
an earlier entry already planted, so overlapping regions do not get
double guards). Obligations beyond that: nothing defined in the loop is
used outside it, the exit has no phis, the latch sits on the branch's
true side.

Enablers built in the same batch:

* **`speculate_region` diamonds** (an explicit `join` field): the head's
  two targets each grow a region, both are checked as before, and they
  are *serialized* - named arm first, its exit rewired into the second
  entry, join phis to selects. Needed for the `or`-shortcircuit's
  "skip to true" arm (`join_421`), whose sibling is the whole
  `solid_at`-chain region. The second entry must be phi-free (its
  predecessor changes).
* The a61 chains went eager exactly like `update_21`'s (7
  `speculate_region` entries), and as predicted the splits relocated 1:1
  (4058 -> 4058) until `mask_loop` deleted the loop control they had
  moved into.

Measured at frame 34: 1.78 s -> 1.59 s, fragments 102 -> 67 mean
(614 -> 371 max), splits 4058 -> 2245, `anonymous_61` clean but for a
9-split `__entry` exposure. Per-lane dynamic work *doubled* (mean 1719 ->
3635: nine eager iterations always) and the frame still got 11% faster -
fragments are the cost, not instructions. K loops-kept 3884 -> 3943,
which is now the bound that matters: uniform rolled loops can stay rolled
in a compiled kernel too.

Limit choice: 8 (nine iterations, i = 0..8). `abs(amount)` beyond 8 would
need ~9 px/frame; the guard fails loudly if a later search depth reaches
it, and the fix is editing one number in two recipe entries.

Remaining splits (2245): `btn` x6 (1261, endgame lane expansion), the
dash diamond `in_i1_074_cont` (372, convertible last), the jump/dash
cluster (474, gated on the `update_21` wall-jump object-loop region and
on `btn`-in-arm), `spikes_at` (96 - mask family, but its break side calls
`kill_player`, which mutates; it needs the store-bearing-arm treatment or
`kill_player`'s effects made maskable), small exposures (~42).

Expect the exposure cascade whenever a stage removes splits: lanes that used
to arrive pre-sorted arrive mixed, and quiet branches wake up. Re-run
`bench --profile` after each batch - the newcomer may be a shape an existing
rule already takes, as `in_i1_078` was for `convert_ternary` and
`if_body_135` was not.

### The wall-jump arm went eager - masked regions landed (2026-08-03)

The last planned piece of conversion machinery: `speculate_region` with
`"mask": true` accepts regions that *write*. Every store becomes the
load-adjacent masked select-store (`assert_value_cell; load old;
select cond ? new : old; store`, original id kept) on the head's
condition - the named arm's stores land on its side, a diamond's second
region on the other, by swapping select operands rather than minting a
`not`. Serializing two store-carrying regions is sound for the same
reason the masking is: the masks are the two sides of one condition, so
on any lane exactly one region's stores land and the other region writes
back what was already there, which is exactly the state the live region
would have seen. No aliasing analysis anywhere; `mask` without a store
to justify it is refused, store-free entries emit byte-identically.

The stage that used it, in order:

* **`fold_reflexive`** (new small rule): the inliner leaves `is_solid(x, 0)`
  chains opening on `0 > 0` - a reflexive comparison of a
  `NumberConstant`, foldable to `false` with no arithmetic and no
  semantics dependence (16.16 fixed point has no NaN; the constant
  operand requirement keeps `t > t`-errors loud). 7 sites in 5
  functions; two fold/dce cascade rounds then deleted 14 never-taken
  collide loops - 1328 instructions, 105 blocks - including dead code
  *inside regions earlier entries had made eager*.
* One validator refinement fell out: between the `fold` that removes the
  last reachable edge into a loop and the `dce` that sweeps it, a phi in
  a live block can name an unreachable predecessor. Availability along
  such an edge is now not required (the edge can never be taken) -
  consistent with unreachable blocks' own instructions already being
  outside the dominance walk.
* Two `speculate_region` gates + 2 `demote_create` flattened what the
  cascade left of the wall-jump arm.
* **The grace diamond** (`if_body_135`): the first masked entry. True arm
  stores `grace = 0`, `spd.y = -2`; false arm is the whole `is_solid`
  scan ending in the pointer-guarded wall-jump stores. Both serialized,
  all four stores masked on `grace > 0`.
* **The jump branch** (`and_or_join_126`): the second masked entry, a
  triangle whose region is the entire linearized arm; its four stores
  (already masked once) re-mask on the jump condition. Masks compose as
  selects compose.

Measured at frame 34: 1.59 s -> 1.46 s, memory 0.63 -> 0.53 GB, fragments
67 -> 46 mean (371 -> 248 max), splits 2245 -> 1531 across 13 sites. The
jump/dash cluster's 474 splits are gone, and `btn` fell 1261 -> 943 with
the fragment count it multiplies. K barely moved (loops-kept 3943 ->
3934, mean dynamic 3635 -> 3728): this stage removed states, not work.
Differential identical through 34 after every batch.

**Nothing convertible splits any more.** The profile's triangle table is
0 splits across all 139 tracked shapes. What remains: `btn` x6 + the
dash gate + the dash diamond (1399 total, all `btn`-tainted - the
endgame lane-expansion family), `spikes_at` (96 - its break feeds
`kill_player`; either separate the hit flag from the kill or make the
kill's effects maskable, likely with this same rule once the call is
inlined), and ~36 of small exposures. The next structural work is
therefore the endgame itself: make `btn` lane expansion pay by doing it
after, not before, the remaining branches stop splitting - plus K
reduction (heap is 39.6% of the kernel) once the frame is one state.

### The `spikes_at` nest went eager - two-level breaks landed (2026-08-03)

The last mask-family split site. The inlined `spikes_at` is a 2-level
tile-loop nest whose four tile checks each `return true` - four break
edges jumping over *both* loops into a phi at the continuation, per-lane
bounds on both counters, and per-lane *inits* too
(`for i = max(0, flr(x/8)), min(15, (x+w-1)/8)`). `kill_player` turned
out to be a non-issue: it sits cleanly downstream behind the hit flag.

Four pieces, applied inside-out (entries g060-g077):

* **`mget` is now a pure builtin** (the cart never calls `mset`; same
  captured-immutable argument as `tile_flag_at`), so `pin_builtin` covers
  the nest's calls and the body becomes speculatable.
* **`fuse_breaks`** (new rule, ~200 lines): merges two consecutive
  early-exit branches - the second test block goes eager into the first,
  the conditions `or` together, the first break block goes unreachable
  for `dce`. Sound because both break blocks must feed the join's phis
  the same bool constant. Three applications collapse the cascade to one
  break branch.
* **`mask_loop` grew `span` and `break_to`** (both opt-in; the pixel-loop
  entries replay byte-identically). `span` handles per-lane init: run
  `span + 1` uniform iterations under `assert_true(init >= 0)` and
  `assert_true(bound - init < span + 1)` - strict, because Lua `for`
  bounds are fractional here (no `flr`), which the first guard draft
  learned loudly. `break_to` handles the multi-level break: the break
  edge is deleted and the exit carries the break out - the inner pass
  leaves `br active ? outer_latch : cont` (still splitting), which is
  *exactly the merged shape* the outer pass then consumes, leaving no
  branch at all: the join's phi entry becomes
  `select active ? false : true`, feeding the kill branch directly.
* **`Pico8Num::checked_rem` is now PICO-8's floored modulo** for positive
  integer divisors (`-2 % 8 == 6`, fractions participate). Eager
  execution feeds `y % 8` negative and fractional dividends for lanes the
  original loop skipped; the old non-negative-integer-only model was a
  strict subset, so nothing previously reachable changed.

Also in this stage: `loop_bound` accepts a bound defined *inside* the
loop when it is a small constant (the masked inner loop's `k <= 1` is
exactly that), checked statically instead of by the `< 32767` runtime
guard - constants defined outside now skip that guard too, which the
recipe replay confirmed changes no existing emission.

Measured at frame 34: 1.46 s -> 1.40 s, memory 0.53 -> 0.46 GB, fragments
46 -> 16 mean (248 -> 86 max), splits 1531 -> 505 across 11 sites. The 96
`spikes_at` splits are gone and every downstream site shrank with the
fragment count: `btn` 943 -> 331, the dash diamond 372 -> 108, the dash
gate 84 -> 30. The kill branch does not split at 34 frames. K: loops-kept
3934 -> 3980 (the eager 2x2 tile window runs every frame). Differential
identical through 34.

What remains is exactly the `btn`-tainted family (469 = `btn` x6 331 +
dash diamond 108 + dash gate 30) plus ~36 small exposures. The next
structural work is unchanged: the `btn` lane-expansion endgame, then K
reduction (heap is 39.1% of the kernel).

### The 36 small exposures are dash echoes, not rule targets (2026-08-03)

The three remaining non-`btn` sites were swept and none falls to any
existing or plausible rule. All three branch on per-lane game state and
guard regions full of calls, stores and allocs:

* `__frame in_i1_012_if_join_526` (9 splits): `if freeze>0 then
  freeze-=1 return end` - early exit over the *entire update*.
* `__frame in_i1_012_cont` (18): `if freeze>0 then return end` - early
  exit over the *entire draw*.
* `anonymous_61 __entry` (9): the cart's `spd.x~=0 or spd.y~=0` guard
  around the whole inlined `obj.move` (stores to `rem`/`x`/`y`,
  `__split_by_flr` calls, collision loops).

They are echoes of the dash decision across frame boundaries: dashing
lanes set `freeze=2`, the frame-boundary merge recombines lanes, and the
next frame's `freeze>0` check re-splits them. Same fundamental category
as `btn` - they end with lane expansion or lane filtering, not masking.
(One cart-level out for `anonymous_61`: the `spd~=0` guard is an
optimization added to the cart, and original Celeste calls `move`
unconditionally - removing it would trade the split for eager
`__split_by_flr` calls and collision loops on every object every frame.
Its correctness rests on the `rem in [-0.5, 0.5)` invariant, so it is a
cart change to weigh separately, not a recipe rule.)

Deep screen after the `spikes_at` stage: differential identical through
frame 37 (28.9 s) and frame 40 (104.5 s). Frame 40 bench: 23.63 s /
5.28 GB / 948,319 lanes, the same 11 split sites as frame 34, and the
kill branch still does not split - spikes stay unreachable through 40.

### The dash cluster is pure `btn` taint; the allocs live elsewhere (2026-08-03)

Investigated where the dash cluster's blockers actually are, in this cart's
IR rather than from memory:

* `in_k1039_cont` (30 splits) is the dash *initiation* gate
  (`djump>0 and dash`). Its arm is plain stores, converted selects and
  inlined `sign` - **no allocation anywhere** (the minimal cart has no
  smoke objects at all). What blocks masking it is `v_input`:
  `btn(k_up) and -1 or (btn(k_down) and 1 or 0)` *inside* the arm.
* `in_i1_074_cont` (108 splits) is the `dash_time>0` diamond. Its true
  arm (`appr` on `spd` toward `dash_target`) is maskable on its own; the
  else arm is the whole normal-physics block and *contains* the dash
  gate above, so it is `btn`-tainted transitively.
* The allocations in `player.update_21` that memory attributed to the
  dash arm are actually the platform `init_object`s inside the two
  inlined `load_room` copies (via `next_room`, taken at `y<-4`) plus the
  kill/`destroy_object` machinery - all behind uniformly-false branches
  through frame 40. They become relevant when the search first crosses a
  room boundary or dies, not before.

So the whole 469-split remainder outside the freeze/spd echoes reduces
to one question: how `btn` is modelled. The earlier "lane expansion is a
regression" measurement (4.47 -> 5.46 s) predates every masking stage -
fragments were 108 mean then and are 16 now, so the trade may have
flipped and wants re-measuring before the endgame is designed.

### K reduction landed as `cse` forward mode, not field promotion (2026-08-03)

The planned whole-function field promotion (load once, SSA, store back at
returns) needs phi insertion, spill/reload around opaque calls, and an
alias story for every base pointer. Before building that, the same goal -
removing heap traffic - turned out to have a much smaller-machinery form:
an opt-in `cse` mode (`"forward": true`, entry g087) with three changes,
each resting on a checked heap-model fact (see the rule's docs):

* **Store-to-load forwarding**: `store c <- v; ... load c` folds to `v`.
* **Field-name alias refinement**: a store through a `get_field _.x`
  pointer no longer kills loads of `.y` cells, globals, index cells or
  IR-alloc cells - only same-name and unknown-provenance loads. A field
  cell can only ever be reached under its own name, because field maps
  only ever receive freshly allocated cells.
* **`create` accessors kill no loads** (they never touch an existing
  cell), and only same-name accessors. This un-fenced the 185 `create`
  barriers.
* **Heap-oblivious calls**: `error`/`__print`/`__split_by_flr` invalidate
  nothing, guarded structurally - a whole-program scan proves those
  globals are only ever loaded, so they cannot have been shadowed. Nine
  new `pin_builtin` entries (g078-g086: `abs` x4, `flr` x2, `mget` x2,
  `tile_flag_at`) remove the last pure-builtin call fences on hot paths.

The live-range lesson bit immediately and is now measured precisely:
unrestricted, the mode finds 631 folds and K (loops kept) falls 3980 ->
3602, but `player.update_21` grows 33 -> 47 live slots and frame 37 gets
~2.5% slower - merge/dedup/filter pay per env slot per state, and at
269k lanes that eats the win. **Forward-mode folds are therefore
block-local**: 270 folds, K 3980 -> 3815, slots unchanged, frame 34
1.40 -> 1.35 s, frame 37 neutral. Differential identical through 34 and
37; lanes and fragments unchanged throughout (the stage removes
instructions, not states). The unrestricted variant is one deleted
restriction away when splitting is gone and env cost stops mattering.

### Lane expansion is built, measured, and waiting on consumer masking (2026-08-03)

The `btn` endgame's first machinery landed: `Instruction::Expand`
concretizes an unknown bool by doubling the state's lanes (first copy
`true`, second `false`) instead of duplicating the state - identity on a
concrete bool, so fixed-input runs are untouched. Two rules place it:

* `expand_bool` (pointed at the diamond's head) replaces a `btn`
  concretization diamond - branch on the loaded cell, arms storing
  `true`/`false` back through the same accessor - with
  `%phi = expand %c; <accessor>; store <- %phi`, no branch, no minted ids.
  `suggest expand-bool` finds all 12 btn diamonds and nothing else.
* `decompose_branch` (pointed at the select) is the branch-side counterpart
  of `decompose_truthy`, needed the moment the bool is a vector:
  `%p = select %c ? %k : %c; br %p` becomes `br %c` with the join phi
  taking `%k` on that edge (`%k` statically truthy demanded). Without it
  the `and`-select mixes a Number and a vector Bool per lane, which the
  representation refuses - loudly, which is how the screen found it.

Applied to the two dash-arm buttons (k_up, k_down; entries g088-g090 in git
history): **differentially identical through 34 and 37, and a ~11-13%
regression** - 1.37 -> 1.56 s at 34, 5.7 -> 6.4 s at 37, +0.13/+0.5 GB.
Same lanes, same 505 splits, same 527 fragments; the splits relocated to
the short-circuit branch and the `v_input` consumers. The profile is
unambiguous (see BENCHMARK_DATA.md): branching on an `UnknownBool`
duplicates the state *for free*, branching on the expanded vector pays a
per-lane mask filter per edge (`filter_branch` 0.18 s/328 calls -> 0.33
s/616), plus `expand_lanes` itself. **Expansion trades free state
duplication for paid lane filtering, so it only pays as a package with
masking every consumer of the expanded value** - the entries were
reverted, the machinery kept.

What the package needs, in order, for the dash arm alone:

1. An `__assert`-diamond-to-`assert_true` rule: the inlined range asserts
   are uniform branches full of `error`/`__print` calls, and they are what
   blocks region speculation across the k_down evaluation.
2. An opt-in for `expand` + the concretization store inside a masked
   region (`is_speculatable` refuses `expand` by design: on lanes that
   would have skipped the arm it doubles lanes nobody asked for - the cell
   is reset to `UnknownBool` at the frame boundary, so next-frame dedup
   reclaims the copies, but within the frame it is real cost that must be
   measured, ~4N vs 3N lanes for the up/down pair).
3. Then the existing rules: decompose the unconditional `v_input` chain,
   absorb the `if_body_187`/`if_condition_185` stores, mask the dash-gate
   arm (`in_k1039_cont`) and the `dash_time` diamond (`in_i1_074_cont`).

Only after that package is whole does the frame stop splitting on dash
inputs at all; every intermediate stage measures as a regression, so it
lands whole or not at all.

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
