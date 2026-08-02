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

### K, and why it is the number to watch

`measure_k` reports the static size of a fully inlined, fully unrolled frame
body from sampled concrete runs. It takes 3 seconds and is the only measure
here that tracks distance to a compilable kernel. Frame time and fragment count
do not: fragments have been 558 through every rewrite so far, by design.

|  | original | rewritten |
|---|---|---|
| dynamic instrs/frame, mean | 2122 | 1715 |
| dynamic instrs/frame, max | 6422 | 5252 |
| distinct blocks reached | 450 | 375 |
| K, fully unrolled | 8321 | 7880 |
| K, loops kept as loops | 2963 | 4170 |

By instruction kind, rewritten: heap 43.9%, arith 25.8%, terminator 9.5%,
const 6.7%, global 5.5%, phi 3.7%, guard 3.3%, call 1.6%. `arith`, `const` and
`select` are the core a compiled kernel emits; the rest has to reach zero.

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
| `vectorize:merge_groups` | 1.27 s | 26.3% |
| `gc:gc` | 0.99 s | 20.5% |
| `filter:filter_branch` | 0.62 s | 12.9% |
| `vectorize:shape_grouping` | 0.43 s | 8.9% |
| `vectorize:dedup_state` | 0.41 s | 8.6% |
| everything under `cfg:` (actual interpretation) | ~1.0 s | ~20% |
| `filter:filter_split_flr` | 0.02 s | 0.5% |

And the finding that matters most, from the `merge_site` bracketing spans:

| | total | calls |
|---|---|---|
| `merge_frame_boundary` | **2.93 s (60%)** | 34 |
| `merge_hint_normalize` | 0.26 s (5%) | 68 |
| inside the frame CFG (`cfg:__main`) | 1.89 s (39%) | 34 |

**Merging states back together at the frame boundary is 60% of runtime.** The
frame itself is 39%, and the semantically necessary interval refinement
(`filter_split_flr`) is 0.5%.

This is a change from the earlier reading of this file, which had intra-frame
filtering at 33%. Slot allocation cut `filter_branch` from 33% to 13%, and what
was left standing was the merge.

The cause is intra-frame branching. `rewrite bench` reports it directly:

    fragments before merge: 19593 total, 576 mean, 5220 max per frame

A frame produces ~576 separate states which then have to be merged back into
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

### Input fan-out: lane expansion is worse than state duplication (2026-08)

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
