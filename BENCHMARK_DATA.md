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

The cause is still intra-frame branching: `filter_branch` fires 830 times per
frame, so the frame ends as ~630 separate states, and re-merging 630 states
costs more than producing them did. Roughly 45 us of `gc` per state, times 630
states, times 34 frames. So the lever is still to produce fewer states -
if-conversion - rather than to merge faster. Making `gc` cheaper is a real but
smaller second lever, worth maybe 20%.

Every frame still ends as **exactly one vectorized state**; all fragmentation is
intra-frame and fully re-merged.

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
