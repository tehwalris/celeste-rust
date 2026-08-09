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


# Batch invariance: HOLDS on room (1,0). (And a retracted claim.)

Philippe's guiding principle, which the whole vectorised design rests on:

> Running a set of lanes as one batch must give the same result as
> partitioning it into subsets, running each, and unioning the outputs.
> Threading, batch sizes and chunk caps are scheduling decisions and must
> not be visible in the answer.

`simdcheck.sh` mechanises it: run N frames under configurations differing
only in how lanes were grouped, and compare the SET of canonical row keys
reached. Row IDS are assignment order and legitimately differ; the set may
not.

**Result, room (1,0) - the invariant HOLDS:**

    f29:  t=1 cap 8000 | t=1 cap 200 | t=16 cap 8000 | t=16 cap 200
          all 7284 rows, digest 4609008652eeb72b

    f42:  caps 8000 / 1000 / 97 across threads 1 / 4 / 16
          all 1208578 rows, digest 2ceb5fae06cf9f31

## RETRACTION, and the lesson

An earlier version of this file reported the opposite - "batch invariance
is violated by chunk size, even on room (1,0)" - with digests, overlap
counts, a frame-29 bisection and six ruled-out causes. **All of it was an
artifact of the checking tool.** `visited.bin` is COLUMNAR (a count, then
every key's low half, then every key's high half); my reader parsed it as
interleaved pairs, so it was pairing `lo[2i]` with `lo[2i+1]` and
digesting a function of the ROW-ID ORDER. Id order depends on discovery
order, which depends on chunking - so the tool reported a difference
exactly when chunking changed, which is indistinguishable from the real
thing.

The bisection was not wasted: it is what made the data small enough to
cross-check. The error was found by validating the reader against a
known-good source - an independent in-interpreter dump of the same keys -
which showed ZERO overlap where there should have been total overlap.

Two things to take from it:

* **A checker that can only say "differs" needs its own validation before
  its output is believed.** Compare it against a source of truth, or a
  deliberately perturbed run, BEFORE trusting an alarming result. I
  validated mine only after it had alarmed me, and after I had committed
  the conclusion.
* **The broken check passed exactly where it was blind.** Threads at a
  fixed cap produce identical id order, so even the broken reader agreed -
  which is what gave me confidence in it. A check that agrees on the easy
  case and disagrees on the interesting one looks precisely like a
  discovery.

`tools/rowset.py` now documents the layout. The validation against the
in-interpreter dump is the thing to repeat if it is ever touched.

## Still open: room (0,0)

Room (1,0) has NO mixed UnknownBool collapses (the counter reads zero) and
satisfies the invariant. Room (0,0) is 70.1% mixed. If the collapse is the
only cross-lane operation left, room (0,0) should violate the invariant
and room (1,0) should not - which would make Philippe's UnknownBool
diagnosis the SOLE known source of batch dependence, and #105 the fix that
restores it. Settle it with:

    ./simdcheck.sh 66 0,0 rewrites-room00.jsonl

Use 66, not less: the collapse counter reads ZERO on room (0,0) at f40,
f48, f52, f56, f60 and f64, and 14,603 constructions by f68. The fruit
only becomes reachable around f65, so anything cheaper does not exercise
the mechanism at all. That makes the test ~8 minutes and ~15 GB per
configuration - schedule it when no campaign is running.

The sweep divergence at room (0,0) f067 was a real error raised by the
program, not by this tooling, and the chunking pin in `ladder.sh` stands
regardless.
