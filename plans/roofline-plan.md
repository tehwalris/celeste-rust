# Roofline plan: where the time goes and what is left

Status 2026-08-08, after the parallel campaign. Read `BENCHMARK_DATA.md`
first for the numbers; this file is the map and the ranked next steps.

## Where we are

Room (1,0), 60 frames, frontier-only + deopt: **89.2 s -> 17.2 s (5.2x)**
on 40% less memory, lane counts identical at every frame. Defaults are now
16 frame threads and an 8,000-lane chunk cap, which move together (see
`chunk_states`).

Phase split at the end:

| phase | time | parallel? |
|---|---|---|
| `fwd.interpret` (frame body + boundary prefix + row keys) | 12.4 s | yes, ~8x on 16 threads |
| `fwd.boundary_stream` (id assignment) | 1.9 s | no, and cannot be |
| `fwd.merge` (boundary merge) | 2.1 s | partly |
| `fwd.boundary_gather` (survivor gather) | 0.5 s | yes |

## What the structure forbids

Two facts bound any further work, and both are measured, not assumed:

1. **98.1% of computed lanes are duplicates.** A frame at f55 offers 82.1M
   lanes to the boundary and keeps 1.56M. This is *not* lost mid-frame
   merging - the figure is the same to 0.0006% at chunk caps from 8k to
   1M. It is the button fan-out landing on states already visited. The only
   way to not compute them is to know in advance which they are.
2. **Row ids must be assigned in one global order.** Everything downstream
   (checkpoints, bands, the backward sweep) is written in terms of them.
   So `insert_new` stays serial. Sharding it was built and measured: it
   works and is bit-identical, and it loses, because a frame does ~100M
   probes to ~1.6M inserts and the shard indirection taxes the 100M.

## Ranked next steps

**1. Integer-typed values end to end.** The one remaining idea that
reduces *work per lane* rather than redistributing it. `Pico8Num` is 16.16
fixed point in an i32 and `Pico8NumInterval` is two of them, but x, y, tile
coordinates, timers, sprite ids and flags only ever hold integers. Narrower
columns mean less traffic in the frame body, in the row-key hash, in the
gather and in the merge - every phase above at once. It is also the
largest and riskiest change on the list: a second numeric variant touches
every arithmetic path, and the lane-structural handlers must resize it
(the desync hazard that shaped `Value::MaybeBool` - see
`plans/tristate-plan.md`). Scope it before starting; do not do it on
impulse.

**2. The 24% serial tail.** `fwd.boundary_stream` (1.9 s) is now close to
irreducible - it is ~1.6M hash inserts and a counter. `fwd.merge` (2.1 s)
still has sequential parts inside `merge_groups`. The structural fix for
both is PIPELINING: the fold for batch *i* is independent of the frame
body for batch *i+1*, which touches no shared state. That needs the batch
loop restructured so the serial fold runs on the main thread while a
worker pool starts the next batch's interpretation. Worth ~15%, and it is
a real restructure of `step_parallel`.

**3. Chunk cap and thread count are one setting.** If either changes,
re-measure both. At 16 threads, f55 goes 27.1 s at cap 1000, 14.5 at 4000,
13.2 at 8000, 15.2 at 16000, 18.3 at 32000. The optimum is real and not
flat.

## Closed off (do not re-open without new information)

* Parallelising across the states *inside* a flow step. Measured twice,
  once at f42 and once at f60. After a boundary merge a frame is a handful
  of very wide states; there is nothing to spread there.
* mimalloc in the `rewrite` driver: +22% on the parallel path.
* Sharded row table (see above).
* Probing the local dedup set before the global table: 4% worse.
* Larger batches than the thread count: imbalance is not the loss.
* Bitmapping the collision tables: neutral, they were cache-resident. (The
  bitmap landed anyway - it is smaller and better tested.)
* Transparent huge pages: already `always` on this machine.

## A caveat on the older roofline numbers

The "31x off bound [B]" analysis further down `BENCHMARK_DATA.md` counted
212.5M lane-frames for a 100-frame run. That is the number of lanes that
*survived* to the visited set, not the work done - a frame at f60 turns
2.0M input lanes into 100.5M boundary lanes. So the derived "42 cycles per
lane-instruction" is inflated by the mid-frame lane multiplier, and bound
[B] wants recomputing from measured bytes (`CELESTE_CENSUS`) rather than
from an instruction count. Until that is done, quote the measured
speedups, not the ratio.
