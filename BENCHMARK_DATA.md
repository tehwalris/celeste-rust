# Benchmark Data

Always run these under `./safe-run.sh` (systemd scope with `MemoryMax=100G`).
It is easy to OOM the machine otherwise.

The standard iteration benchmark is **frame 34**, which takes a few seconds.
Frame 37 (~13 s) is the confirmation run; frame 40 takes over a minute and is
for milestones only.

```bash
./safe-run.sh -- ./target/release/rewrite bench --frames 34
./safe-run.sh -- ./target/release/rewrite bench --frames 34 --profile
```

`rewrite bench` runs the program the recipe produces. `celeste-rust -n N` runs
the *unrewritten* program through the full search harness, so its numbers are
not comparable.

## Current (2026-08, `rewrite` branch)

| program | frame 34 | frame 37 |
|---|---|---|
| as compiled | 6.64 s / 1.75 GB | - |
| + `promote_cell` (130 cells) | 5.47 s / 1.12 GB | - |
| + slot plumbing (identity, no-op) | 5.69 s / 1.12 GB | - |
| + `allocate_slots` | 4.65 s / 1.05 GB | 13.20 s / 3.63 GB |
| + 183 `inline`s | 4.47 s / 1.12 GB | 12.87 s / 3.75 GB |
| + 81 `if_convert`s | 4.40 s / 1.06 GB | - |
| + stage B finished (`promote_capture`, 84 method inlines) | 4.36 s / 1.06 GB | - |
| + `cse` (block-local, then cross-block for accessors) | 4.32 s / 1.06 GB | - |
| + `demote_create` x46, `pin_builtin` x18 | 4.33 s / 1.06 GB | - |
| + 4 store-blocked triangles converted (`speculate`, `sink_store`, `if_convert`) | 3.36 s / 0.94 GB | 9.22 s / 3.11 GB |
| + 4 `convert_ternary` (the `appr` pairs) | 2.62 s / 0.81 GB | 7.87 s / 2.83 GB |
| + `decompose_truthy` x15, `if_convert` x2, `convert_ternary` x1 | 2.36 s / 0.74 GB | - |
| + 2 diamonds absorbed (`absorb_stores`, `speculate` arms, `cse`) | 1.99 s / 0.64 GB | - |
| + `is_solid` chains eager (`speculate_region` x9), consumers absorbed | 1.82 s / 0.62 GB | - |
| + wall-jump stores absorbed (`speculate` with a pointer guard) | 1.78 s / 0.61 GB | - |
| + pixel loops masked (`mask_loop` x2, a61 chains eager) | 1.59 s / 0.63 GB | - |
| + wall-jump arm eager (`fold_reflexive` dead gates, masked `speculate_region`) | 1.46 s / 0.53 GB | - |
| + `spikes_at` nest masked (`fuse_breaks`, `mask_loop` `span`/`break_to`) | 1.40 s / 0.46 GB | - |

The store-triangle row is the first change that moved the fragment count: 558
-> 335 mean fragments per frame at frame 34, split executions 19573 -> 11978.
The ternary row continues it: 335 -> 232, splits -> 8472; the decompose row:
232 -> 212, splits -> 7802; the diamond row finishes every select-expressible
shape: 212 -> 140, splits -> 5345, and **no triangle, `and`/`or` construct,
or convertible diamond anywhere in the program splits any more**. Lane count
identical throughout (92,713). See "Which branches actually split" below for
why these sites paid when 81 earlier conversions did not.

The `speculate_region` row is the first *loop-stage* result: the collide
table loops behind `player.update_21`'s short-circuit gates run eagerly,
their `if_join_413` gates stop splitting, and the freed per-lane `is_solid`
values flowed to consumers that were absorbed in the same batch (the
`on_ground` diamond, the gravity `appr`, the accel `elseif` chain, the
wall-slide `maxfall` store, the `wall_dir` cascade via `decompose_truthy`).
Fragments 140 -> 108 mean (955 -> 671 max), splits 5345 -> 4280. The
instructive dynamic: converting only the chains moved splits (5345 -> 5345,
relocated); each consumer conversion then killed its share for real.

### K, and why it is the number to watch

`measure_k` reports the static size of a fully inlined, fully unrolled frame
body from sampled concrete runs. It takes 3 seconds and is the only measure
here that tracks distance to a compilable kernel.

|  | original | rewritten |
|---|---|---|
| dynamic instrs/frame, mean | 2122 | 3861 |
| dynamic instrs/frame, max | 6422 | 9146 |
| distinct blocks reached | 450 | 255 |
| K, fully unrolled | 8321 | 9807 |
| K, loops kept as loops | 2963 | 3980 |

By instruction kind, rewritten: heap 39.6%, arith 30.9%, const 7.8%,
terminator 6.7%, guard 5.4%, global 5.0%, phi 3.1%, call 1.1%. `arith`,
`const` and `select` are the core a compiled kernel emits; the rest has to
reach zero. The region stage *lowered* K (7770 -> 7323 unrolled) despite
running regions eagerly, because absorbing the consumers deleted whole arm
blocks and their duplicated address arithmetic.

The `mask_loop` stage then *raised* dynamic work on purpose - mean per-lane
instructions 1719 -> 3635, since the masked pixel loops now run a constant
9 iterations with eager bodies - and the frame still got 11% faster,
because fragments are what cost, not instructions. Note which K bound
matters now: with per-lane control flow gone from these loops, they can
stay *rolled* in a compiled kernel too, so the loops-kept bound (3943) is
the realistic kernel size, and "fully unrolled" (9755) mostly counts
uniform table-loop iterations that never needed flattening.

K and the frame time move independently, and the store-triangle conversions
are a clean example: K rose by 11 (guards added, arms now run on every lane)
while frame time fell 22%, because the change removes *states*, not per-lane
work. Fragments had been 558 through every earlier rewrite - none of those
removed a branch that ever split - and dropped to 335 the first time four
splitting branches went away. So: K measures distance to the kernel, fragment
count measures how much splitting remains, frame time follows fragments.
Judge a rewrite by the one it claims to move.

### What `cse` cost and bought, in the three variants that were run

Same recipe, same lanes (92,713), same memory (1.06 GB), same fragments (558).

| cse | frame 34 | instructions | live slots in `player.update_21` |
|---|---|---|---|
| block-local only | 4.38 s | 19529 | 23 |
| cross-block, every kind | 4.55 s | 18075 | 29 |
| cross-block, accessors and loads only | 4.32 s | 19073 | 24 |

The middle row is the one worth remembering: **1454 fewer instructions and a 4%
slower frame.** Only ~11% of frame time is running `player.update_21`; merge,
gc, dedup and shape grouping are ~65% and are charged per state for every live
value. Reusing a definition from an earlier block trades instructions for live
range, and for arithmetic that trade is a loss. Restricting cross-block reuse to
heap accessors and loads is smaller *and* faster than either extreme.

`cse` is also barrier-bound rather than scope-bound: `player.update_21` has 306
accessor barriers and 528 load barriers across 3801 instructions, so most of the
redundancy that survives is fenced by the 101 remaining calls and 185 `create`
accessors, not by block boundaries.

Frame 40, milestone checks only: 84.1 s / 25.8 GB as compiled, 62.5 s / 14.45 GB
after `promote_cell`. That beat the old unverified `mem2reg`'s 15.2 GB, which
was stage 1's target.

Lane counts are identical across all of these (92,713 at frame 34; 269,059 at
frame 37), which is the first thing to check when a rewrite claims a win -
identical lanes means the same work was done, differently.

## Where the time goes

`rewrite bench --frames 34 --profile`, on the current recipe. Self time, so the
rows partition wall clock rather than double counting nested spans. Profiling
costs about 8%.

| span | self | share |
|---|---|---|
| `vectorize:merge_groups` | 0.41 s | 20.9% |
| `vectorize:dedup_state` | 0.37 s | 18.7% |
| `gc:gc` | 0.29 s | 14.5% |
| `filter:filter_branch` | 0.27 s | 13.6% |
| everything under `cfg:` (actual interpretation) | ~0.40 s | ~20% |
| `vectorize:shape_grouping` | 0.12 s | 5.9% |
| `filter:filter_split_flr` | 0.02 s | 1.2% |

And the finding that matters most, from the `merge_site` bracketing spans:

| | total | calls |
|---|---|---|
| `merge_frame_boundary` | **1.02 s (51%)** | 34 |
| `merge_hint_normalize` | 0.25 s (13%) | 68 |
| inside the frame CFG (`cfg:__main`) | 0.94 s (47%) | 34 |

**Merging states back together at the frame boundary is half of runtime**
(down from 60% when this section was first written - the branch-removal
rounds attack exactly this). The semantically necessary interval refinement
(`filter_split_flr`) is ~1%.

This is a change from the earlier reading of this file, which had intra-frame
filtering at 33%. Slot allocation cut `filter_branch` from 33% to 13%, and what
was left standing was the merge.

The cause is intra-frame branching. `rewrite bench` reports it directly:

    fragments before merge: 4751 total, 140 mean, 955 max per frame
    (was: 19593 total, 576 mean, 5220 max before the branch-removal rounds)

A frame produces ~140 separate states which then have to be merged back into
one. Every part of the merge is per state, so **the fragment count is the
number a branch-removing rewrite should be judged by** - not the `filter_branch`
share, which slot allocation already cut from 33% to 13%.

Every frame still ends as **exactly one vectorized state**; all fragmentation is
intra-frame and fully re-merged.

### Which branches actually split (2026-08)

`rewrite bench --profile` attributes every conditional-branch execution to its
source block and records whether both edges got lanes. At frame 34:

    branches: 21862 of 333379 executions split the state, across 33 distinct sites

**93% of branch executions are free** - the condition is uniform across lanes,
every lane goes the same way and nothing is cloned. All the fragmentation comes
from 33 places, and the top 15 are 94% of it:

| function | block | splits | uniform |
|---|---|---|---|
| `player.update_21` | `in_j2_050_if_join_10` | 2838 | 0 |
| `player.update_21` | `in_j2_052_if_join_10` | 2838 | 0 |
| `player.update_21` | `if_join_133` | 1856 | 2500 |
| `obj.is_solid_47` | `if_join_413` | 1779 | 6247 |
| `player.draw_22` | `__entry` | 1499 | 7211 |
| `player.update_21` | `if_body_101` | 1356 | 504 |
| `obj.move_y_53` | `for_body_start_488` | 1064 | 996 |

This is why `if_convert` was nearly free but nearly useless: it was aimed at the
145 `and`/`or` triangles, whose conditions are almost always uniform.

The top two sites are **the same site twice** - the `if` inside `btn()` in
`lua/builtin_level_4.lua`, inlined at two call sites. They split 2838 times each
and *never* run uniformly, because that branch is the search's own branching
factor: `__button_states[i]` starts as an `UnknownBool`, and `flow.rs` sends a
state that branches on `UnknownBool` down **both** edges unfiltered. Together
they are 26% of all splits, and because they duplicate the state early in the
frame they multiply every split downstream.

Note the mechanism: this is not lane filtering, it is state *duplication*.
`filter_by_mask` is never called. It is also not something a program rewrite can
remove, because the branch is semantically necessary - it is how the search
enumerates inputs.

**Update, after the four store-blocked triangles were converted** (`speculate`
+ `sink_store` + `if_convert` on `player.draw_22 if_join_222` and
`player.update_21 if_join_98/116/119`):

    branches: 11978 of 181247 executions split the state, across 31 distinct sites

The four sites accounted for 3523 splits directly, but total splits fell by
7595 and branch *executions* fell by 152k, because a split early in the frame
multiplies every branch execution downstream - `draw_22`'s split alone cloned
the state for the whole rest of the frame. Even the irreducible `btn` sites
fell from 2838 to 1620 each, since fewer states now reach them. This
downstream multiplier is also why the 81 earlier conversions bought nothing:
they were aimed at branches that never split, so there was nothing to
multiply.

**Update, after `convert_ternary` on the four splitting `appr` pairs:**

    branches: 8472 of 130829 executions split the state, across 27 distinct sites

What still splits is no longer triangle-shaped: the four `btn` sites (3362,
input fan-out), diamonds and chains in `player.update_21` (`if_join_95` 1056,
`if_join_133` 838, `if_join_136` 581, `if_condition_92` 576, `if_join_108`
480), the object loop in `anonymous_61` (~660), and two ternary variants
`convert_ternary`'s strict shape check refused (346).

**Update, after `decompose_truthy` x15 defused every mixed-select cascade and
`if_convert`/`convert_ternary` took the two refused variants plus the `appr`
pair that removing them exposed:**

    branches: 7802 of 121074 executions split the state, across 26 distinct sites

Zero splits remain from triangles or `and`/`or` constructs - the profiler's
triangle table reads 0 across the board for the first time. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 (`in_j2_042/046/050/052`) | 3176 | input fan-out |
| `if_join_95/133/136`, `if_condition_92`, `if_join_108` | ~3400 | diamonds and chains |
| `if_join_413` x3, `for_head`s (object loop bodies) | ~920 | loop + method shapes |

One dynamic worth recording: removing the `sign()` split at `in_i1_077` made
the *downstream* `appr` pair at `in_i1_078` start splitting (144, previously
0 - its condition used to arrive pre-sorted into per-state-uniform lanes).
It was `ready` in the triangle table and one `convert_ternary` entry took it.
Removing splits un-hides downstream splits; re-profile after every batch.

**Update, after the diamond stage** (`absorb_stores` on `if_join_108` and
`if_condition_92`, the two convertible sites of the five read this session):

    branches: 5345 of 86751 executions split the state, across 24 distinct sites

Removing 1056 direct splits took ~2457 total - the two sites sat *upstream*
of most of `player.update_21`, so every splitter below them now sees fewer
fragments: `if_join_95` 1056 -> 840, `if_join_133` 766 -> 444, `if_join_136`
509 -> 326, the `btn` pair 1138 -> 820 each. Frame 34 fell 2.36 s -> 1.99 s
and 0.74 -> 0.64 GB. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 (`in_j2_042/046/050/052`) | 2540 | input fan-out |
| `if_join_95/133/136`, `if_body_135` | 1706 | diamonds blocked by loops / `btn` |
| `if_join_413` x4, `for_head`s (object loop bodies) | ~1100 | loop + method shapes |

The exposure this round: `if_body_135` (96 splits, previously quiet), the
nested grace-check inside the jump arm - same `is_solid`-loop blocker as its
parent `if_join_133`. Every remaining site is gated on a later stage (object
loops, `btn` lane expansion); shape work is done until one lands.

**Update, after the region stage** (`speculate_region` x9 making the
`player.update_21` `is_solid` chains eager, plus the consumer conversions
they exposed - the `on_ground` diamond via the multi-store `absorb_stores`,
the gravity `appr` arm, the accel `elseif` chain, the wall-slide `maxfall`
store, the `wall_dir` cascade via one more region + `decompose_truthy`):

    branches: 4280 of 64692 executions split the state, across 22 distinct sites

Frame 34: 1.99 s -> 1.82 s, 0.64 -> 0.62 GB, fragments 140 -> 108 mean
(955 -> 671 max). The chain conversions alone moved splits without removing
one (the `is_solid` result still fed a branch); each consumer conversion
then deleted its share, and the multiplier paid again downstream:
`if_join_95` 840 -> 648, `if_join_136` 326 -> 249, the `btn` pair 820 ->
669 each, `in_j2_042` 300 -> 195. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 | 1923 | input fan-out |
| `if_join_95` | 648 | the outer dash diamond, convertible last |
| `if_join_133/136`, `if_body_135`, `and_or_join_153` | 873 | the jump/dash cluster (below) |
| `anonymous_61` move loops (`413`, `for_head`s) | 599 | pixel loops, unroll stage |
| `in_i1_068` x2 (`spikes_at` tile loops) | 114 | unroll stage |

The jump cluster is one blocker deep: `if_body_159` (the wall-jump stores)
needs `speculate` to move a re-load of `this.spd` above the `spd.y` store,
and the cells are not provably distinct by the facts it has (the reload's
base is a loaded table, not a shared local). The clean fix is a runtime
distinctness guard (`assert_true` on pointer inequality), which needs `~=`
on pointers in `op.rs` - deferred, not attempted this round. Exposures that
did land: `if_condition_100` (the accel chain, 288 at its peak) was
converted in-batch; `if_body_135` rose 96 -> 156 and `in_k1000_for_head_472`
woke at 36, both gated on their stages.

**Update, after the wall-jump pointer guard** (`speculate` with the new
opt-in `guards` field on `if_join_160`, `absorb_stores` on
`and_or_join_153`, one `merge_blocks`):

    branches: 4058 of 62418 executions split the state, across 21 distinct sites

Frame 34: 1.82 s -> 1.78 s, fragments 108 -> 102 mean (671 -> 614 max).
`~=` on pointers turned out to already exist in `op.rs` (pointers compare
by `HeapId`), so the whole interpreter half of the planned work was free;
the stage was one `speculate` extension. Direct kill: `and_or_join_153`
(102). Downstream: the hot `btn` pair 669 -> 618 each, `if_join_136`
249 -> 231. K 7323 -> 7308 unrolled; dynamic max per lane rose 4986 ->
5190 because the wall-jump loads now run on the longest path too. The
remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 | 1821 | input fan-out |
| `in_i1_074_cont` (= old `if_join_95`) | 648 | the outer dash diamond, convertible last |
| `and_or_join_126` (= old `if_join_133`), `if_join_136`, `if_body_135` | 753 | jump/dash cluster, gated on object loops / `btn` |
| `anonymous_61` move loops (`413` x2, `for_head`s) | 626 | pixel loops, unroll stage |
| `in_i1_068` x2 (`spikes_at` tile loops) | 114 | unroll stage |
| 6 sites below the profiler's top-15 cutoff | 96 | tails of the above |

(`merge_blocks` renamed two sites: `if_join_95` and `if_join_133` merged
into their predecessors - same branches, same splits.) The cluster is now
exactly where the plan said it would be: `if_body_135`'s false arm *is*
the `is_solid` object-loop region and cannot flatten before the loop
stage, and `if_join_136`'s arm holds the `btn(k_up/k_down)` reads. The
guard machinery itself is the reusable part: any future load-across-store
whose distinctness is real but unprovable is now one recipe field, not a
new rule.

**Update, after the pixel loops were masked** (`mask_loop` x2 on
`anonymous_61`'s inlined `move_x`/`move_y`, their `is_solid` chains made
eager first with 7 `speculate_region` entries - one of them the first
*diamond* serialization - plus 6 `demote_create` and one `merge_blocks`):

    branches: 2245 of 36925 executions split the state, across 16 distinct sites

Frame 34: 1.78 s -> 1.59 s, fragments 102 -> 67 mean (614 -> 371 max),
splits 4058 -> 2245. The loops themselves accounted for 626; the
downstream multiplier paid the other ~1200: the hot `btn` pair 618 -> 393
each, the dash diamond 648 -> 372, `and_or_join_126` 366 -> 228,
`if_join_136` 231 -> 144, `if_body_135` 156 -> 102. `anonymous_61` no
longer splits at all except a 9-split `__entry` exposure. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x6 (two 17-split sites woke) | 1261 | input fan-out |
| `in_i1_074_cont` (= old `if_join_95`) | 372 | the outer dash diamond, convertible last |
| `and_or_join_126`, `if_join_136`, `if_body_135` | 474 | jump/dash cluster, gated on object loops / `btn` |
| `in_i1_068` x2 (`spikes_at` tile loops) | 96 | mask family, but its break calls `kill_player` |
| `__main` `in_i1_012` x2, `anonymous_61` `__entry`, 1 tail site | ~42 | small exposures |

`mask_loop` keeps the loop *rolled* and gives it a uniform constant trip
count (`k <= 8`, per-state, never splits); which lanes still iterate
becomes mask data (`take = active && (i <= bound)`), every latch/break
store is masked with the load-adjacent select-store, the break edge is
deleted, and `assert_true(bound <= 8)` covers the claim that 9 iterations
are enough. No unrolling: the model only forbids *per-lane* control
state, and a uniform-trip rolled loop has none - the same reason the
object-table loops were never a problem. The differential is identical
through frame 34, guard included.

**Update, after the wall-jump arm went eager** (`fold_reflexive` + 4
`fold`/`dce` rounds, 2 `speculate_region` gates, 2 `demote_create`, then
the grace diamond and the jump branch as the first two *masked*
`speculate_region` entries):

    branches: 1531 of 30868 executions split the state, across 13 distinct sites

Frame 34: 1.59 s -> 1.46 s, memory 0.63 -> 0.53 GB, fragments 67 -> 46
mean (371 -> 248 max), splits 2245 -> 1531. Two findings paid for the
stage. First, the `is_solid(x, 0)` chains all open with a gate on `0 > 0`:
`fold_reflexive` folds the reflexive comparison without evaluating
anything, and two fold/dce cascades deleted 14 never-taken collide loops
across 7 sites in 5 functions - 1328 instructions of dead code, including
inside regions earlier entries had made eager. Second, with the arm
flattened, `speculate_region` with `"mask": true` ran the whole wall-jump
arm - grace stores, `is_solid` scan, spd writes - unconditionally, every
store going through a load-adjacent masked select; the grace diamond's two
store-carrying arms serialize soundly because their masks are the two
sides of one condition. The jump/dash cluster (474 splits) is gone
entirely, and the `btn` family shrank with the fragment count it
multiplies (1261 -> 943). The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x6 | 943 | input fan-out |
| `in_i1_074_cont` (the outer dash diamond) | 372 | arms contain `btn` calls, waits for the endgame |
| `in_k1039_cont` (= old `if_join_136`, the dash gate) | 84 | condition reads `btn`, arm allocs |
| `in_i1_068` x2 (`spikes_at` tile loops) | 96 | mask family, but its break calls `kill_player` |
| `__main` `in_i1_012` x2, `anonymous_61` `__entry` | 36 | small exposures |

Every remaining split is now `btn`-tainted, `spikes_at`, or a small
exposure - there is no convertible triangle, diamond or maskable region
left that splits (the profile's triangle table shows 0 splits across all
139 tracked shapes).

**Update, after the `spikes_at` nest went eager** (`mget` promoted to a
pure builtin, 7 `pin_builtin`, 1 `speculate_region` triangle, 3
`fuse_breaks` collapsing the four-tile early-exit cascade to one break,
then `mask_loop` twice with the new `span` and `break_to` fields -
inside-out, the inner pass leaving exactly the merged shape the outer pass
consumes):

    branches: 505 of 12166 executions split the state, across 11 distinct sites

Frame 34: 1.46 s -> 1.40 s, memory 0.53 -> 0.46 GB, fragments 46 -> 16
mean (248 -> 86 max), splits 1531 -> 505. The 96 `spikes_at` splits are
gone, and everything downstream shrank with the fragment count it
multiplies - the same dynamic as every stage since the store triangles,
but larger, because the tile loops split *early* in the frame and every
later site paid per fragment. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x6 (the `in_j2_0xx_if_join_10` sites) | 331 | input fan-out |
| `in_i1_074_cont` (the outer dash diamond) | 108 | arms contain `btn` calls, waits for the endgame |
| `in_k1039_cont` (the dash gate) | 30 | condition reads `btn`, arm allocs |
| `__main` `in_i1_012` x2, `anonymous_61` `__entry` | 36 | small exposures |

The kill branch (`br found ? kill : continue`) that the conversion was
expected to expose does not appear as a split site at 34 frames: the
`in_j2_*` sites above are the `btn` inlines, not `kill_player`. K paid a
small price for the eager 2x2 tile window running every frame: loops-kept
3934 -> 3980, mean dynamic 3728 -> 3861.

Two guards carry the stage's premises: `assert_true(init >= 0)` and
`assert_true(bound - init < span + 1)` per loop (strict, because
`min(15,(x+w-1)/8)` has no `flr` and the first guard draft with `<= span`
failed loudly on fractional bounds two lanes wide). One interpreter
extension was needed: eager execution feeds `%` negative and fractional
dividends (`y % 8` for lanes above the screen), so `Pico8Num::checked_rem`
now implements PICO-8's floored modulo (`-2 % 8 == 6`) for positive
integer divisors - a strict generalisation of what the OCaml reference
computes on its non-negative domain, checked by the differential run.

Following the branch-site table, `btn` was rewritten to be branch-free: a
`__concretize` builtin that turns the `UnknownBool` button into a per-lane
`Bool` by doubling the lane space, instead of an `if` that sends the state down
both edges. Total lanes are identical either way, and `rewrite observe` confirmed
the lane content was byte-identical over 25 frames.

It is a clear regression:

| | time | memory | mean fragments |
|---|---|---|---|
| `btn` as an `if` (state duplication) | 4.47 s | 1.12 GB | 576 |
| `btn` via `__concretize` (lane expansion) | 5.46 s | 1.72 GB | 576 |

**The fragment count did not move at all.** The split moved from inside `btn` to
its caller: `if btn(k_jump) and ...` now branches on a lane-varying bool and
splits into the same two states, one branch later.

And it is *worse* than that, because the old formulation was doing something
useful. Duplicating the state gives two states in which the button is
`Bool(Scalar(true))` and `Bool(Scalar(false))` - **scalars**, which cost no
per-lane storage and make every downstream condition derived from the button
uniform, hence free. Lane expansion makes the button a vector, so those
conditions become lane-varying, they split with real `filter_by_mask` work, and
every value derived from the button is now per-lane storage. Hence the 54% more
memory.

So `UnknownBool` sending a state down both edges unfiltered is **load-bearing,
not a wart**. Fragments are not gratuitous: each one is a scalar-specialised
copy of the state, which is exactly why 93% of branches are free.

The corollary for the plan: input fan-out as lane expansion only pays once the
frame is *already* branch-free, because only then is there no downstream branch
left to absorb the split. It is a late-stage change, not an early one. Reverted;
`rewrite observe` was kept, since checking a harness change against lane content
is what made this cheap to evaluate.

### What `gc` is actually doing (2026-08)

`gc` is 21% of runtime at ~46 us per call, and the obvious guess is that it is
copying per-lane data: it rebuilds the heap to renumber `HeapId`s, and a
vectorized cell holds one value per lane. Making `MaybeVector::Vector` an
`Arc<Vec<T>>` would make that copy a refcount bump.

Instrumented, per gc call:

| | per call | total |
|---|---|---|
| cells visited | 280 | 6.0 M |
| lane elements copied | 3291 | 71 M (284 MB) |
| object tables rebuilt | 21 | 0.45 M |
| string keys cloned | 81 | 1.7 M (8 MB) |
| pointer slots rewritten | 105 | 2.3 M |

284 MB of lane data over the whole run is about 28 ms of memcpy against gc's
1.0 s, so **lane copying is ~3% of gc and the `Arc` change would buy almost
nothing.** The cost is per *cell*, not per lane: 165 ns each, which is roughly
two or three allocations - cloning the `HeapValue`, the `Box<HeapValue>` the
`FrozenVec` stores, the rebuilt `FxHashMap` for each object table - plus the
`old_to_new` hash map traffic.

That cost scales with the number of states, not with lanes per state. So it is
not a separate lever at all: it is the same fragmentation problem, and
if-conversion is what shrinks it.

### Recipe replay is part of the benchmark (2026-08)

`rewrite build` prints where replay time went:

    replay: 2.0s total - clone 0.1s, apply 0.3s, validate 0.9s, verify 0.7s

Every command in the tool replays the recipe before doing anything, so this is
a constant added to `verify`, `screen`, `suggest`, `bench` and `print` alike.
It had silently reached **67s** - 97% of it whole-program dominance validation
after each of 554 entries, on functions that inlining had grown to hundreds of
blocks. See the commit for the three fixes.

The lesson for benchmarking: quote replay separately from the thing being
measured. A `bench --frames 34` that reports 4.36s was, for most of this
session, a 70-second command.

## Scalar reference point

`concrete_run` executes 30 frames single-lane in 46 ms including parse and init,
i.e. ~0.5 ms per single-lane frame. The vectorized interpreter is already ~10x
more efficient per lane than the scalar path at frame 30, so vectorization is
working - the problem is what happens between frame boundaries.

## Historical note

The numbers once in this file (frame 30 = 18 s, frame 39 = 1030 s, OOM at frame
40) were collected 2024-12-29 and are obsolete by ~25x. They predate mimalloc,
LTO, COW `LocalEnv`, `FxHashMap`, GC-before-vectorize and the rest of the perf
work. Do not use them for extrapolation.

The `interpreter` branch's unverified `mem2reg` + `block_coalesce` were dropped
along with the rest of that optimizer, which cost 1.3x time and 1.9x memory at
frame 40. `promote_cell` won that back and then some.
