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

## Correction: chunking is NOT lane-count-neutral where UnknownBool deopts

Earlier notes (and the first version of this file) say the chunk cap
"reorders ids, not rows". That was verified on room (1,0), and on room
(1,0) it is true. It is NOT true in general, and room (0,0) shows why.

A fruit state's widened collide check is `UnknownBool`, and an
`UnknownBool` branch sends the WHOLE state down both edges. So the
coarseness of the over-approximation depends on how lanes are grouped: a
chunk in which the condition happens to be definite avoids the doubling,
and a coarser chunk containing that lane alongside an ambiguous one does
not. Measured directly, same f075 checkpoint, same code, only the fruit
chunk cap different (800 vs 8,000 lanes):

    f76 new lanes    9,873,531   (cap 800)
    f76 new lanes    9,891,670   (cap 8,000)

The direction is guaranteed, which is what keeps this sound: finer chunks
can only ever remove doubling, never add it, so a coarser cap yields a
SUPERSET. The abstract search is an over-approximation and a bigger one is
still an over-approximation; the concrete optimum comes from the k=16
exact level plus a witness replay, neither of which this touches.

What it does mean:

* Two runs at different chunk caps are not comparable frame-by-frame on a
  room with fruit. Compare them only at the level of the certified answer.
* `parcheck.sh` is unaffected and its claim is unchanged - it holds the
  cap FIXED and varies only the thread count, and that is byte-identical.
* A cap change is a semantic choice on such rooms, not just a performance
  knob. Note it when changing one.

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


# BATCH INVARIANCE IS VIOLATED - measured 2026-08-09, unresolved

Philippe's guiding principle, which the whole vectorised design is
supposed to rest on:

> Running a set of lanes as one batch must give the same result as
> partitioning it into subsets, running each, and unioning the outputs.
> Threading, batch sizes and chunk caps are scheduling decisions and must
> not be visible in the answer.

`simdcheck.sh` mechanises it: run N frames under configurations that
differ ONLY in how lanes were grouped, and compare the SET of canonical
row keys reached. Ids are assignment-order and may differ; the set may
not. (`parcheck.sh` covers the stronger byte-identity claim for threads
alone at a FIXED grouping, and still passes.)

**Room (1,0), 42 frames - the room with no fruit, which I expected to
pass:**

    t16 cap 8000 fruit 8000    1208578 rows   digest 2194b24e5f749316
    t1  cap 8000 fruit 8000    1208578 rows   digest 2194b24e5f749316
    t16 cap 1000 fruit 1000    1208578 rows   digest 4a36e961159a1b3d
    t4  cap   97 fruit   97    1208578 rows   digest b354b05391aa9e85

Threads alone are invariant. CHUNK SIZE IS NOT. And the shape of the
difference is the informative part:

    |A| = |B| = 1,208,578    shared 343,514    only-A 865,064    only-B 865,064

**Identical cardinality, 28% overlap.** Two searches that genuinely
explored different state sets would almost certainly differ in size. Equal
counts with partial overlap is the signature of a KEYING difference - the
same logical rows receiving different keys - not of a different search.

## Why this matters more than the fruit story

I attributed the room (0,0) sweep divergence at f067 to the fruit chunk
cap changing mid-campaign, and pinned the knobs (c8dfb2f). That pin is
right regardless, but this measurement says the diagnosis was probably
INCOMPLETE: room (1,0) has no fruit and no `UnknownBool` collapse to
speak of, and its keys still move with chunk size. So there is a second,
more basic source of chunk dependence in the row key itself.

If the row key is not chunk-invariant then:

* the frontier dedup misses rows it should have matched. Sound - extra
  work, not wrong answers - but it inflates every search.
* the SWEEP cannot reproduce the forward pass's keys unless it chunks
  identically, which is exactly the f067 error, and explains why it
  appeared on a configuration mismatch rather than on fruit specifically.

## Where to look first

The key is `row_key_hashes(shape_hash, columns, ..)`. Its per-row part was
made order-independent and is representation-blind by construction. The
per-STATE part is the shape hash, and a shape difference moves every row
in that state at once - which fits "same count, different keys" exactly.
`normalize_value_for_shape` is blind to Scalar-vs-Vector (both map to
`VectorizableNumber` etc.), so that is not it; the candidates left are

* `Value::Number` vs `Value::NumberInterval` being DIFFERENT shapes, with
  the rem widening producing one or the other depending on the lanes
  present;
* the column ORDINALS shifting, since `row_key_hashes` mixes a column's
  position into its contribution and the column list comes from the heap;
* heap ids in `ValueShape::Pointer` after a gc whose reachable set depends
  on values.

The decisive experiment is one state, run whole versus split in two, with
the output row keys compared directly - a unit test, not a campaign. Do
that before changing anything.

## Consequence for the plan

This outranks everything in "Ranked next steps" above. It is also more
evidence for #105: partitioning on straddling lanes makes a lane's fate
depend only on its own comparison, which is one of the two things the
invariant needs. The other is a chunk-invariant key.
