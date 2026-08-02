# Benchmark Data

Always run these under `./safe-run.sh` (systemd scope with `MemoryMax=100G`).
Frame 40 already peaks at ~29 GB, and it is easy to OOM the machine otherwise.

```bash
./safe-run.sh -- ./target/release/celeste-rust -n 40
```

## Current baseline (2026-08, `rewrite` branch)

Release build, single machine, from scratch (no checkpoint resume).

| frames | wall | lanes at end | peak RSS |
|--------|------|--------------|----------|
| 30 | 0.69 s | 15,250 | 0.21 GB |
| 34 | 6.1 s | 92,713 | 1.99 GB |
| 37 | 23.5 s | 269,059 | 7.04 GB |
| 40 | 87.1 s | 948,319 | 29.2 GB |

Growth is ~1.45x lanes/frame and ~1.5x time/frame.

### Derived quantities

- **Per-lane cost per frame is roughly flat**: ~45 us (f30) -> ~87 us (f37) ->
  ~92 us (f40). Fragmentation is not getting relatively worse with frame
  number; the lane count is just growing exponentially.
- **Per-lane memory is roughly flat at ~30 KB.** The end-of-frame state has
  `heap_len = 280` cells, of which maybe ~120 are per-lane numeric/bool. At 8 B
  that is ~1 KB/lane of actual information, so we carry a large constant-factor
  overhead from intra-frame state fragmentation.
- Every frame ends as **exactly one vectorized state**. All the fragmentation
  is intra-frame; it is fully re-merged at the frame boundary.
- At ~30 KB/lane the 100 GB budget is exhausted somewhere around **frame 43**.

## Effect of the two shipped CFG passes

The `interpreter` branch runs `mem2reg` + `block_coalesce` on every `FunDef`
(`FixedEnv::optimize_cfg`). Those passes are *not* on this branch - they were
dropped along with the rest of the unverified optimizer. Measured cost of
dropping them:

| frames | `interpreter` (with passes) | `rewrite` (without) | ratio |
|--------|------------------------------|---------------------|-------|
| 30 | 0.62 s / 0.15 GB | 0.69 s / 0.21 GB | 1.1x / 1.5x |
| 37 | 17.9 s / 3.86 GB | 23.5 s / 7.04 GB | 1.3x / 1.8x |
| 40 | 66.7 s / 15.2 GB | 87.1 s / 29.2 GB | 1.3x / 1.9x |

**This is a real, measured ~1.9x memory regression, and it is the first thing
the rewrite work has to win back.** It makes sense: promoting `Alloc` cells to
SSA removes heap cells, which shrinks both the per-lane footprint and
`StateShape`, which in turn lets more states merge.

So `promote_cell` (the verified, per-cell replacement for `mem2reg`) and
`merge_blocks` are not just enablers for later stages - they have an immediate
measurable target: **get frame 40 back under 15 GB.**

## Where the intra-frame time goes

From `trace_frame37.json` (one frame, 318k spans, generated with `--trace`):

| category | self time | share |
|---|---|---|
| `cfg` (nested CFG interpretation) | 6.05 s | 46.4% |
| `materialize` | 3.28 s | 25.2% |
| `vectorize` | 2.43 s | 18.6% |
| `gc` | 1.06 s | 8.1% |
| `filter_by_mask` | 0.21 s | 1.6% |

Top spans by self time:

```
 3.277s 25.2%  n=61130    materialize        [materialize]
 1.706s 13.1%  n=11255    tile_flag_at_72    [cfg]
 1.444s 11.1%  n=26883    tile_at_73         [cfg]
 1.149s  8.8%  n=38       dedup_state        [vectorize]
 1.047s  8.0%  n=20011    gc                 [gc]
 0.957s  7.3%  n=12       merge_groups       [vectorize]
 0.951s  7.3%  n=4        player.update_21   [cfg]
```

**236,052 nested CFG interpretations in a single frame** (`obj.collide` 35,636x,
`obj.check` 35,636x, `tile_at` 26,883x, `sign` 15,426x, `is_solid` 11,032x) -
for a frame that logically calls each a handful of times.

The lesson: `filter_by_mask` itself is only 1.6%. The *consequence* of having
filtered - a fragmented state set, each fragment re-entering every callee with
its own nested worklist, then a large normalization pass to un-fragment - is
most of the cost. See `plans/rewrite-plan.md`.

## Scalar reference point

`concrete_run` executes 30 frames single-lane in 46 ms including parse and init,
i.e. ~0.5 ms per single-lane frame. The vectorized interpreter is already ~10x
more efficient per lane than the scalar path at frame 30, so vectorization is
working - the problem is what happens between frame boundaries.

## Historical note

The numbers previously in this file (frame 30 = 18 s, frame 39 = 1030 s, OOM at
frame 40) were collected 2024-12-29 and are obsolete by ~25x. They predate
mimalloc, LTO, `Arc<Vec>` COW `LocalEnv`, `FxHashMap`, GC-before-vectorize and
the rest of the perf work. Do not use them for extrapolation.
