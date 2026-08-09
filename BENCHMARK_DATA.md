# The time-expanded sweep: measured, and it is worth doing (2026-08-09)

Can the backward sweep drop its edge graph - 10,072,724,145 edges and
58 GB for room (1,0), and an outright OOM on room (0,0) - and instead
re-derive the successors each frame, filtered by PLAYER POSITION?
`rewrite sweep-census` answers that from the certified room (1,0)
universe (212,559,009 rows, concrete optimum 100) without running a
sweep. The unit throughout is ROW-EXPANSIONS: one row carried one frame
forward, which is what both designs actually pay for.

| over frames 1..H | H=100 | H=105 | H=110 |
|---|---|---|---|
| edge sweep today (each row once) | 212.6M | 212.6M | 212.6M |
| time-expanded, no filter | 4.433e9 (20.9x) | 4.433e9 | 4.433e9 |
| time-expanded, perfect predecessor oracle | 8.8M (0.04x) | 26.8M (0.13x) | 56.0M (0.26x) |
| time-expanded, flat 8px disc | 120.9M (0.57x) | 304.8M (1.43x) | 554.8M (2.61x) |
| time-expanded, per-cell learned radius | 1.652e9 (7.8x) | 1.787e9 (8.4x) | 1.945e9 (9.2x) |
| **time-expanded, exact per-cell source sets** | **98.3M (0.46x)** | **259.0M (1.22x)** | **486.8M (2.29x)** |

H=100 is the certified optimum, so its band is as thin as a band can
ever be - every row in it lies on an optimal path. H=105 and H=110 use
the same `g` with a slacker horizon to show the degradation; they are
what a horizon the ladder has not yet refuted would look like.

The naive time-expanded cost is 20.9x the edge sweep, not the 39x a
back-of-envelope from `mean g` suggests: B is monotone
(`B(i) ⊇ B(i+1) ∩ R(i)`), so a row that has already qualified is never
re-tested, and only `R(i) \ B(i+1)` is.

## The filter has to be a set, not a radius

The old solver (`celeste-rust-old`, `DistanceTracker`) learned one max
squared distance per destination cell and painted a disc. That is 7.8x
WORSE than the edge sweep here, and the reason is a single cell.

Of the 10.07e9 edges, 10,072,669,008 move at most 8 pixels. The other
55,137 all land in ONE cell - (136, 124) measured from the start room's
origin, which is the next room's spawn point, where a winning row
appears. Its true predecessors are the far edge of the PREVIOUS room, so
the disc that holds them has radius 134px and covers everything. And
that cell is the seed of `B(H)`, so it is in `B(i+1)` at every late
frame: the learned-radius candidate count is 119,070,337 at f090 where a
flat 8px reads 1,247,069.

Storing the exact SET of source cells per destination cell instead - a
grid bitmap, 8 KB per occupied cell, 8,143 cells and 66 MB on room (1,0),
47.1 source cells per destination on average - removes the problem
entirely (1,052,051 at f090) and is tighter than any radius everywhere
else too. 66 MB against 58 GB of edges.

The 8px disc is listed above for comparison only: it is NOT sound, by
exactly those 55,137 edges.

## What this does and does not buy

At the horizon the ladder actually lands on it is 0.46x the expansions,
and the 58 GB edge store, the 303 s CSR shard build, the 537 s
`fwd.merge` and the reverse BFS all disappear. Five frames of slack makes
it a wash and ten makes it 2.3x worse - so this is a MEMORY result, not
primarily a speed one. What it fixes is room (0,0), where the forward
pass completes (94 frames, 85 min, 60 GB) and only the sweep dies.

Two things the census does not measure, both of which have to be settled
before the sweep is rewritten:

* Where the source sets come from. Learning them from the edge chunks is
  fine for a measurement but circular for an implementation - they have
  to be recorded by the FORWARD pass, which sees every transition exactly
  once (frontier-only, and the successor relation is a static graph over
  rows) and can maintain the 66 MB table for free.
* How a candidate row becomes a `State`. Candidates at frame `i` were
  discovered at any earlier frame, but the saved batches are grouped BY
  discovery frame, so the obvious loop re-loads batches 1..i every frame
  - O(H²/2) decompressions, ~1900 s on room (1,0), which is more than the
  expansions cost.

Also note the gate has to change shape: a time-expanded sweep at horizon
H produces `g` only where `e + g <= H`, so it cannot be compared
element-wise against the full certified `g`. The equivalent full-array
gate is against the certified array THRESHOLDED at H.

# Room (1,0) re-certified on the parallel build (2026-08-08)

The end-to-end gate for everything below, and the one that matters: a
FULLY FRESH ladder at horizon 100 - level-0 forward, a from-scratch
backward sweep, then k=1..16 banded - reproducing the certified answer.

**CONCRETE OPTIMUM = 100 frames.** All 17 per-level answers identical to
the previously certified run (90, 94, 97, 98, 98, 99, then 100 from k=6
on), and the level-0 sweep matched to the edge: **10,072,724,145 edges, 2
win seeds** on both.

That is a strong check on the whole day's work at once - chunk-parallel
frames, the order-independent row key, the parallel visited probe, the
parallel sweep replay. A single mis-ordered row id or a hash collision
anywhere in 10 billion edges would move one of those seventeen numbers.

| stage | wall |
|---|---|
| level-0 forward, f1-f100 (16 GB peak) | ~22 min |
| backward sweep, 100 frames from scratch, 10.07e9 edges | ~45 min |
| k=1..16 banded levels + their sweeps | ~9 min |
| **total, nothing reused** | **~76 min** |

For comparison the previous certification took 1h45 - but that one reused
banked CSR shards for the sweep, which the cost analysis below estimates
would otherwise have added hours. The sweep here was built from nothing.

Caveat, so the table is not read as more than it is: the sweep's SPEEDUP
is unmeasured. It is parallel now and gated byte-identical
(`sweepcheck.sh`), but there is no controlled serial counterpart at this
depth - the f40 gate universe finishes in 5 s either way. See the pending
task before quoting a number for it.

# The parallel campaign, end state: 5.2x at 60 frames (2026-08-08)

Room (1,0), 60 frames, frontier-only + deopt. Lane counts identical to the
serial baseline at every step (1,997,387 at f60).

| | time | peak |
|---|---|---|
| serial, no chunking (the morning's baseline) | 89.2 s | 4.31 GB |
| chunk-parallel frames (16 threads, cap 8k) | 21.9 s | 2.55 GB |
| + parallel visited probe, local candidate dedup, parallel pre-merge gc | 18.6 s | 2.43 GB |
| + survivor gather moved off the serial path | **17.2 s** | **2.43 GB** |

**5.2x, on 40% less memory.** Phase split at the end: interpret 12.4 s
(parallel), boundary decide 1.9 s (serial), merge 2.1 s, gather 0.5 s
(parallel). The serial remainder is 24%.

## The shape of what is left

The parallel region is 12.4 s x 16 threads = ~200 CPU-seconds, and it is
spread thin - the flat profile's top entries are 12% (row keys), 7%
(select), 10% (tile_flag_at across four symbols), 5% (map2). Same finding
as the first profile: there is no single fix worth a multiple, so the
remaining levers are (a) less work per lane, which is what the queued
integer-representation idea is, and (b) the 24% serial tail.

Two structural facts that bound everything:

* **98.1% of the lanes a frame computes are duplicates.** f55 offers
  82,078,938 lanes to the boundary and keeps 1,563,989. That ratio is
  independent of chunk size (82,078,458 at cap 1M vs 82,078,938 at cap 8k,
  0.0006% apart), so it is not lost mid-frame merging - it is the button
  fan-out reaching states the search has already seen. Nothing short of
  knowing the answer in advance avoids computing them.
* **Parallel efficiency is ~8x on 16 threads** for the frame body
  (`fwd.interpret` at cap 8k, f55: 32.9 s at t=2, 19.0 at t=4, 11.1 at
  t=8, 7.5 at t=16 - still gaining 1.48x on the last doubling, so not
  saturated, but tailing).

## Measured and not landed

Recorded because each one closes a plausible-looking direction:

| idea | result |
|---|---|
| mimalloc in the `rewrite` driver (main.rs has it, bin/rewrite.rs never did) | **+22% on the parallel path.** glibc's per-thread arenas suit a short-lived lane vector per instruction on 16 threads better than mimalloc's segments |
| sharded row table, parallel batch insert (ids still serial and bit-identical) | cuts the serial fold 1.91 -> 1.27 s, **1.8% worse overall.** A frame does ~100M probes to ~1.6M inserts; the shard indirection taxes the 100M to parallelise the 1.6M |
| probe the cache-resident local dedup set before the global table | **4% worse.** It makes the local set hold every distinct row in the chunk instead of only the candidates (~2% of them) |
| batches larger than the thread count (barrier / load balance) | 24 is 1% better and 15% more memory; 32 and 48 are worse. Imbalance is not where the efficiency goes |
| bitmap the collision tables (112 KB -> 14 KB), hoist the (w,h) choice | exactly neutral (11.26 vs 11.27 s at f55) - they were already cache-resident. Kept anyway: smaller, and it replaced a placeholder test |
| hash-only `shape_hash_of_state`, parallel shape derivation | 17.27 -> 17.22 s. Kept for the same reason |
| dropping the global probe entirely (to price it) | saves 0.6 s of the 12.3 s parallel region, adds 5.7 s to the serial one |
| `CELESTE_DEOPT_COLLECT_FIRST` off (does the origin column block mid-frame dedup?) | 1.3%. The "origin forbids mid-frame merging" note was about the sweep's replays, not the forward pass |

Transparent huge pages are already `always` on this machine, so the
visited table's TLB behaviour is not on the table either.

# Chunk-parallel frames: 4.5x and less memory (2026-08-08)

Room (1,0), 60 frames, frontier-only + deopt, same lane counts throughout
(1,997,387 at f60 - the check that nothing semantic moved):

| | time | peak | note |
|---|---|---|---|
| serial, no chunking (the baseline all day) | 89.2 s | 4.31 GB | |
| 16 threads, no chunking | 57.0 s | 11.83 GB | 1.6x, and 2.7x the memory |
| 1 thread, chunk cap 8k | ~110 s | 1.2 GB | the tiling tax, ~23% |
| **16 threads, chunk cap 8k** | **19.8 s** | **2.48 GB** | **4.5x, and 42% less memory** |

The two knobs are one setting. Threads alone undo streaming - 16 chunks'
raw outputs in flight is exactly the accumulation the streaming boundary
was built to avoid. Chunking alone is a 23% loss, because each chunk
re-runs the work that is uniform across lanes. Together the chunks give the
threads work and the threads pay for the chunking, so the cap now derives
from the thread count instead of being a knob to remember.

Phase split at f60 (16 threads): interpret 12.1 s (parallel), boundary
4.5 s (serial), merge 2.8 s. Serial remainder is 37%, so Amdahl caps
further thread scaling at ~1.6x more; SMT is worthless (t=24 and t=32 are
both slightly slower than t=16 - this is memory-bound).

Chunk size is a real optimum, not a monotone knob: at 16 threads, f55 goes
27.1 s at cap 1000, 14.5 s at 4000, **13.2 s at 8000**, 15.2 s at 16000,
18.3 s at 32000. Too small and the per-chunk uniform work dominates; too
large and the batch barrier and the cache do.

## What made it scale

1. **The visited-set probe split in two.** Phase 1 (hash each lane to its
   128-bit row key and probe the table READ-ONLY) needs only `&RowTable`,
   so it runs in the worker. Phase 2 (insert, assigning ids) stays serial.
   At depth ~98% of offered lanes are rows already seen - f60 offers 100.5M
   and keeps 2.0M - so nearly all of the one-cache-miss-per-lane traffic
   parallelises. Boundary went 25.0 s -> 5.9 s on its own.
2. **The two parallelism levels had to stop multiplying.** A frame worker
   calling `hash_rows` would `thread::scope` another 16 OS threads per
   boundary state - thousands of spawns per frame. `set_nested_parallel`
   makes the inner level yield to the outer.
3. **Uniform columns leave the per-lane row hash.** Census: a boundary
   state has 50.8 columns that hold one value in every lane and 7.2 that
   vary, and the old sequential fold folded all 58 into every row. See the
   commit - the fix required making the combine order-independent, which is
   also a latent correctness point (uniformity is a property of a state,
   not of a column).

## Determinism, and what "identical" means here

Two separate axes, and only one of them moves anything:

* **Threads: byte-neutral.** At a fixed chunk cap, 16 threads produce
  byte-identical `states.bin` and `visited.bin` to 1 thread. Work goes out
  in batches of `threads` consecutive chunks and comes back in INPUT order,
  so the sequence of rows offered to the visited set is the serial one and
  row ids are unchanged. `parcheck.sh` is the gate.
* **Chunk cap: reorders ids, not rows.** A different cap changes which
  fragments merge mid-frame, so rows reach the boundary in a different
  order and get different ids. The row SET is unchanged - per-frame "new
  lanes" and "visited total" are equal at every frame, and only the
  pre-dedup `sub_before` count moves (23,873,562 vs 23,873,058 at one
  frame of a 45-frame run).

## Correction to the roofline accounting above

The 31x figure below rests on 1.95e11 lane-instructions for a 100-frame
room-(1,0) run, derived from 212.5M lane-frames. That count is the number
of lanes that SURVIVED to the visited set. It is not the work done: at f60
a frame turns 2.0M input lanes into 100.5M boundary lanes (the ~50x button
fan-out), and the frame body computes on those. So the real
lane-instruction count is far higher, the achieved rate far better, and the
"42 cycles per lane-instruction" figure is wrong by whatever the mid-frame
lane multiplier is. Bound [B] should be recomputed from measured bytes (the
`CELESTE_CENSUS` totals) rather than from an instruction count, and until
that is done the honest statement is the measured one: 4.5x today, with 37%
of the run now serial.

# Roofline: how far off are we? (measured 2026-08-08)

Philippe's yardstick: "the time to load all the compressed states for a
frame at peak theoretical memory bandwidth, plus the time to write the
compressed states at peak bandwidth - that's sort of the benchmark. Not
necessarily achievable, but you should be able to get close."

Machine: Ryzen 9 7950X3D, 16C/32T, 128 MiB L3. MEASURED bandwidth (not
spec sheet): 51.5 GB/s pure read, 36.0 GB/s STREAM triad, 29.0 GB/s
memcpy counting read+write.

Subject: room (1,0) level-0 forward pass, 100 frames, the FAST room -
212,559,007 lane-frames, 670 MB of compressed states (3.15 bytes/lane;
the compression is excellent), 1658 s wall.

| bound | time | we are |
|---|---|---|
| [A] compressed-state I/O only (Philippe's roofline) | 0.046 s | 36,000x off |
| [B] every intermediate materialised to RAM, bandwidth-bound | 53.8 s | 31x off |
| [C] compute: 1.95e11 lane-instructions, AVX2, 16 cores | 0.15 s | 11,000x off |
| actual | 1658 s | |

Per lane-instruction we spend 8.5 ns - about 42 cycles at 5 GHz - where
a vectorised op over a long lane vector should amortise to well under
one. Achieved rate is 1.18e8 lane-instructions/s.

Bound [B] is the one to internalise: even a *perfectly memory-bound*
implementation of the dataflow we already have would finish in 54 s
instead of 1658 s. The 31x is not "a few times slower than necessary".

Where the time goes (fwd metrics, same run):
  fwd.interpret        1190 s   72%
  fwd.boundary_stream   353 s   21%
  fwd.save_frames        45 s    3%
  fwd.merge              10 s    0.6%

Three gaps, in order of ratio-per-effort:

1. NO THREAD PARALLELISM IN INTERPRET. 72% of the time runs on one core
   of sixteen; only virtual_merge is threaded (std::thread::scope).
   Frames decompose into ~61 independent fragments, which is
   embarrassingly parallel. A state-parallel flow was tried and reverted
   (task #63, "only helped the plain path") - worth re-measuring now
   that the recipe path dominates and the fragment count is known.

2. ~42 CYCLES PER LANE-INSTRUCTION. Enum dispatch per instruction, the
   MaybeVector Scalar/Vector branch per operand, heap indirection, Arc
   refcount traffic, and an allocation per output vector.

3. NO TILING. Each instruction materialises its whole output vector
   (~111k lanes at f100 = ~444 KB, far past L2) before the next
   instruction reads it back: 1.56 TB of intermediate traffic for this
   run. Tiling a cache-sized block of lanes through the whole program,
   rather than the whole lane set through one instruction at a time, is
   exactly the difference between bound [B] and bound [A].

## Where the cycles go (perf, room (1,0) 60 frames, search-dominated)

Profile a 40-frame run and half of it is recipe build (liveness::analyze,
validate_function, slots::constraints - all startup). At 60 frames the
search dominates and the picture is:

| subsystem | % |
|---|---|
| tile_flag_at / collision lookup | 13.9 |
| subtract_visited (frontier) | 13.0 |
| MaybeVector map/map2 (arithmetic) | 11.4 |
| instruction dispatch | 9.9 |
| interpret_select::pick | 9.1 |
| abstraction (rem widening) | 5.5 |
| allocator | 3.5 |
| merge / hashing | 2.8 |

No single dominant hotspot - the top five are ~57% spread over five
subsystems. That is itself the finding: there is no one fix worth 10x,
so the primary lever has to be parallelism (which multiplies everything)
with tiling to make it work, and the per-subsystem items are secondary.

Landed from this profile: tile_flag_at converted x and y to i16 through
`as_i16_elements` before looking anything up - two intermediate Vec
allocations, two uniformity scans, two extra round-trips through memory
before map2 started. Fused into one pass: -2.8% (96.9 -> 94.2 s at 60
frames), byte-identical artifacts.

## State-parallel flow, retested at depth: still not it (2026-08-08)

The 2026-08-05 revert of state-parallel flow noted "+4-6% on the
rewritten path" and left the door open "if the tradeoff ever flips".
Hypothesis worth testing: that was measured at frame 42, where states
are small (~2900 lanes), so thread overhead would dominate; at frame 60+
states are ~115k lanes and it should win.

Retested by reverting the revert (it was already deterministic -
per-state outputs concatenate in input order) and measuring at 60
frames: 95.9 s off vs 94.7 s on. 1.3%. The tradeoff did not flip.

The useful conclusion is WHY. Parallelising across the states in a flow
step only helps when a step has many states. After each boundary merge a
frame starts as ~1 state and splits progressively, so most of the work
happens in WIDE SINGLE STATES that this parallelism never touches.

=> The right parallel axis is ACROSS LANES WITHIN A STATE, not across
states. That is also exactly the axis tiling wants: chunk the lane vector
into cache-sized blocks, run a block through the whole program, then the
next. Parallelism and tiling become the same refactor rather than two
that fight each other (which is what the 2026-08-05 "working sets evict
each other" note was really reporting).

Scratch branch discarded; nothing landed from this experiment except the
knowledge.

## Queued: spatial locality (Philippe, 2026-08-08)

"There's a ton of locality in the game state's spatial coordinates - if
you're around the bottom left corner and you run one frame, you're still
going to end up around the bottom left corner."

Two distinct uses, and the second is the one that matters:

1. tile_flag_at reads tiles near (x,y), so position-sorted lanes would
   make its lookups sequential rather than a random gather. But Philippe
   notes this one may not need locality at all: a room is 16x16 tiles, so
   solidity is 256 bits = 32 bytes, and a custom per-room lookup could be
   compressed small enough to sit in L1 permanently. Compression beats
   locality here.

2. DEDUP is where locality matters. subtract_visited is 13% of the
   profile and probes a hash table of 128-bit keys - random access into a
   large table, one cache miss per lane. Spatially adjacent states are
   also the ones most likely to be duplicates, so ordering or bucketing
   the visited set by position (rather than by hash) would turn those
   misses into local probes AND cluster the duplicates it is looking for.

## Queued: integer-typed values end to end (Philippe, 2026-08-08)

"Some numbers are pretty much exclusively ints so we could store them
that way end to end."

Pico8Num is 16.16 fixed point in an i32, but x, y, tile coordinates,
timers, sprite ids and flags only ever hold integers. A separate integer
representation would halve their memory traffic (the thing bound [B] is
made of), delete conversions like the one fused above, and make their
comparisons and arithmetic cheaper. It also composes with tiling: narrow
columns mean more lanes per cache line.

Not free: a second numeric variant touches every arithmetic path, and the
lane-structural handlers must resize it (exactly the desync hazard that
shaped the MaybeBool design - see plans/tristate-plan.md). Worth scoping
against the parallelism work rather than doing on impulse.

Caveat on the 36,000x: bound [A] assumes compute is free, so it is a
yardstick rather than a target. Bound [B] (31x) is the honest measure of
how much is being left on the table by the current execution strategy,
and gaps 1+3 are the levers on it.

# Benchmark Data

## Fresh-run cost: room (0,0) vs room (1,0) (2026-08-08)

A clean fresh 100 m campaign with today's code costs ~13 +/- 2 h vs the
measured 3h01m for 200 m (~4.5x): level-0 forward to first win ~1 h
(vs 14 min), first full backward sweep ~3 h (vs ~31 min), horizons
81-94 ~8-10 h (vs ~2 h), final ladder ~30 min. The 4.5x factors:
(a) ~3-4x slower program per lane (shape-agnostic prefix recipe, fruit
states on the plain path) applied to BOTH passes; (b) ~2.5x more state
(387M vs 151.6M visited rows; 21M vs 5.2M peak frontier); amplified by
the origin-tagged sweep replay being ~4x the forward cost on the same
frame (the origin column forbids mid-frame merging - f90: 2747 s replay
vs 740 s forward, 1.3B successors materialized). Banked offsets:
incremental CSR shards (would otherwise add ~4-6 h), streaming
boundary (without it the run OOMs), header-only chunk counts.
Recovery plan (deliberately deferred): S2/S3 variant recipes (both
passes ~3x), tri-state comparisons (kills the fruit plain-fallback and
coarse-level inflation, allows larger replay chunks), parallel replay
(single-threaded today). Target: fresh 100 m in ~2-4 h.


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
| + `spikes_at` nest masked (`fuse_breaks`, `mask_loop` `span`/`break_to`) | 1.40 s / 0.46 GB | 5.52 s / 1.51 GB |
| + 9 `pin_builtin`s, `cse` forward mode (block-local store forwarding) | 1.35 s / 0.50 GB | 5.56 s / 1.47 GB |
| + interpreter micro-opts round 1 + `Arc`'d lane payloads (see below) | 1.03-1.04 s / 0.47 GB | 4.11-4.16 s / 1.65 GB |
| + 16 `collapse_loop` (the `#objects` check/collide loops) | 1.02-1.03 s / 0.47 GB | 4.04 s / 1.66 GB |
| + 16 `assume_eq` + pointer folds: every object-table check returns nil | 0.92-0.94 s / 0.47 GB | 3.85 s / 1.71 GB |
| + 4 `collapse_break_loop` (the `foreach`/`del` sentinel loops) | 0.93 s / 0.46 GB | 3.77-3.91 s / 1.62 GB |
| + `split_call` + both update arms inlined (no dispatch left) | 0.94-0.96 s / 0.47 GB | 3.88-3.90 s / 1.65 GB |
| + 2 `unroll_loop` (pixel-move loops flat) + `merge_blocks` + `cse` forward | 0.88-0.89 s / 0.46 GB | 3.65-3.75 s / 1.65 GB |
| + `dedup_guards` (501 dominated asserts deleted) | time-neutral | time-neutral |
| + 2 `unroll_loop` on the `spikes_at` nest (inner then outer) | 0.88 s / 0.47 GB | 3.69-3.70 s / 1.65 GB |
| + `kill_dead` + cache-blocked dedup (2026-08-05 morning) | - | ~2.89 s |
| + uniform collapse + virtual merge + ranged filters (2026-08-05 night) | - | 2.38 s / **1.01 GB** |

The frontier with the 2026-08-05 night stack (sequential interpreter):

| | time | peak | lanes |
|---|---|---|---|
| bench `--frames 40` | 9.76 s | 3.70 GB | 948,319 |
| bench `--frames 41` | 16.07 s | 6.05 GB | 1,447,750 |
| bench `--frames 42` | 26.47 s | 9.36 GB | 2,181,716 |
| runner `-n 39` | 33.5 s | 14.6 GiB | 613,865 |
| runner `-n 40` | 50.4 s | 23.8 GiB | 948,319 |

Growth is ~1.65x/frame on the rewritten path, so a 60 s budget reaches
**frame 43** and 120 s reaches **frame 44** - two frames deeper than the
morning's estimate at the same budgets, still single-threaded.

Updated 2026-08-06 with the context-partitioned merge
(CELESTE_PARTITION_CELLS=dash_time,djump - see the partition section and
plans/overnight-2026-08-05.md):

| | time | peak |
|---|---|---|
| bench `--frames 42` | 13.1 s | 4.84 GB |
| bench `--frames 44` | 33.9 s | 10.8 GB |
| bench `--frames 45` | **54.6 s** | 15.3 GB |

A 60 s budget now reaches **frame 45**; 120 s reaches ~frame 46.

Updated again after the booleans joined the key (pm1 =
dash_time,djump,has_dashed,p_dash,p_jump - time-flat, peak -44%):

| | time | peak |
|---|---|---|
| bench `--frames 45` | 47.6 s | 8.5 GB |
| bench `--frames 46` | 70.0 s | 12.1 GB |
| bench `--frames 47` | **104.1 s** | 16.3 GB |

Frame 47 (12.0M lanes) is the deepest frame ever computed, inside a
120 s budget, and the per-frame growth softened to ~1.47-1.49x (memory
pressure feeds back into merge cost). For scale: 24 hours before this
measurement, the frontier was frame 41 at 107 s - six frames deeper at
the same cost, one working day later.

## 2026-08-06 overnight: the whole room

With frontier-only search (CELESTE_FRONTIER_ONLY=1), the deopt
architecture (`bench --deopt`, tasks #78/#80) and the conservative
widenings (timer pins + dash_effect_time clamp), the search now runs
the ENTIRE room (1,0), through 31 kill/respawn frames, to the abstract
win at frame 90 (see plans/overnight-2026-08-06.md for what frame 90
means - it is the rem-widened lower bound, not the concrete optimum
100):

| | time | peak | note |
|---|---|---|---|
| bench `--frames 62 --deopt` (v1 deopt) | 179.9 s | 16.4 GB | first run past f58 |
| bench `--frames 90 --deopt` (v2, clean) | 898 s | 32.5 GB | full room; WIN at f90; 151.6M visited rows |
| + CELESTE_DEOPT_COLLECT_FIRST | **737 s** | 32.7 GB | same win, same frontier, -18% |

Per-frame at depth (v2, clean run): f60 13.6 s, f76 (peak, 5.24M new
lanes) ~35-45 s, f90 29.6 s. Total plain re-runs across all 90 frames:
1.55M lanes (v1 re-ran ~5M lanes per frame by f74 - the lane-granular
deopt is what makes the deep half affordable). A 120 s budget reaches
**frame 63** (collect-first).

For scale: the 2022 hand-written solver completed this room in roughly
5-15 min. The general system is now at wall-clock parity on the
forward pass, while interpreting the original Lua.

Frontier-only + deopt are still opt-in (env var + flag); the standard
f34/f37 iteration benchmarks above are unchanged and remain the
regression gate.

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
| dynamic instrs/frame, mean | 2122 | 3776 |
| dynamic instrs/frame, max | 6422 | 8927 |
| distinct blocks reached | 450 | 255 |
| K, fully unrolled | 8321 | 9562 |
| K, loops kept as loops | 2963 | 3815 |

By instruction kind, rewritten: heap 38.5%, arith 32.2%, const ~8%,
terminator ~7%, guard ~6%, global ~4%, phi ~3%, call 0.9%. `arith`,
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

The forward-mode stage replayed the same lesson with sharper numbers. Refined
alias kills plus store-to-load forwarding finds 631 folds unrestricted; K
(loops kept) drops 3980 -> 3602, frame 34 improves 1.40 -> 1.33 s - and frame
37 *regresses* ~2.5%, because `player.update_21` goes from 33 to 47 live
slots and `anonymous_61` from 31 to 41, and merge/dedup/filter are charged
for the whole env per state. Restricting every new fold to one block keeps
270 of the folds, all of the frame-34 win that survives contact with depth
(1.35 s), K 3815, and the slot counts exactly at baseline. Fold reach is a
live-range decision, not a correctness one - the wider version only becomes
free once splitting is gone or folding is made slot-aware.

Frame 40, milestone checks only: 84.1 s / 25.8 GB as compiled, 62.5 s / 14.45 GB
after `promote_cell`. That beat the old unverified `mem2reg`'s 15.2 GB, which
was stage 1's target. After the `spikes_at` stage: **23.63 s / 5.28 GB**
(948,319 lanes), same 11 split sites as frame 34, differential identical
through 40 (104.5 s) - the `mask_loop` span guards and the floored modulo hold
in the deep regime, and the kill branch still does not split.

Lane counts are identical across all of these (92,713 at frame 34; 269,059 at
frame 37), which is the first thing to check when a rewrite claims a win -
identical lanes means the same work was done, differently.

### Lane expansion without consumer masking is a regression (2026-08)

The `expand` instruction concretizes an unknown bool by doubling the state's
lanes instead of duplicating the state (`rules::expand_bool` replaces `btn`'s
concretization diamond with it; `rules::decompose_branch` keeps the `and`
chain's short-circuit representable once the bool is a vector). Applied to the
two dash-arm buttons alone - k_up (`in_j2_050_if_join_10`) and k_down
(`in_j2_052_if_join_10`), entries `g088`-`g090` in git history - it is
differentially identical through frame 37 and a clean regression:

| recipe | frame 34 | frame 37 |
|---|---|---|
| without (committed) | 1.37-1.38 s / 0.50 GB | 5.65-5.75 s / 1.6-1.7 GB |
| expand k_up/k_down | 1.55-1.57 s / 0.63 GB | 6.30-6.53 s / 2.1-2.2 GB |

Same lanes, same 505 splits, same 527 fragments at frame 34 - the splits
*relocated* rather than disappearing (k_up's 72 to the short-circuit branch
`in_i1_081_cont`, k_down's 72 to the `v_input` consumers `if_body_187` /
`if_condition_185`). The profile attributes the entire +0.20 s: an
`UnknownBool` branch duplicates the state for free (both edges get it
unfiltered), while the same branch on the expanded vector pays a per-lane
mask filter per edge - `filter_branch` 0.18 s / 328 calls -> 0.33 s / 616
calls - plus 0.08 s of `expand_lanes` itself. Expansion converts free state
duplication into paid lane filtering; it can only pay off where the
downstream branches disappear entirely, i.e. as a package with masking every
consumer of the expanded value. The entries were reverted; the machinery
(instruction, both rules, differential result) is kept and tested.

### The `__assert` diamonds are gone - structural, time-neutral (2026-08-03)

`convert_assert` (entries `g091`-`g115` + a `dce` at `g116`) replaced all 25
inlined `__assert` failure diamonds - `%n = not %cond; br %n ? <print+error
subgraph> : join` - with a straight-line `assert_true %cond`. Every path
through such a subgraph calls `error`, which aborts the run exactly like a
failing `assert_true`, and `not` hard-errors on non-bools, so there is no
truthiness gap; the differential run is identical through 34 and 37.

Measured A/B on the same binary, 34 and 37 frames, 2-3 runs each: **neutral
within noise** (1.36-1.39 s / ~0.50 GB at 34 both ways; 5.71-5.79 s /
1.66-1.72 GB at 37 both ways; lanes and fragments identical). Expected: the
branches were uniform and never taken, so the interpreter never spent time
in them. What the change buys is structural: 1722 -> 1622 blocks and
17287 -> 16787 instructions, and - the point - the assert diamonds no
longer sit as branches between every `btn` call and its concretization, so
region speculation can cross them (`assert_true` is speculatable). This is
step 1 of the dash package in plans/status.md.

### The full dash package: assembled, correct, and a measured regression (2026-08-03)

The complete package from plans/status.md was built and measured (entries
preserved in plans/dash-package.jsonl; not in the recipe). It expands
`btn(k_up)`/`btn(k_down)` into lane fan-out and flattens every consumer:
the concretization diamonds become `expand` + store, the arg cells promote
to SSA (block-local multi-store promotion), the k_down short-circuit arm
runs eagerly under the new `speculate_region` expand opt-in, the 2x2
dash-direction nest (`input~=0` x `v_input~=0`) and the three tail
triangles (dash_target.y, dash_accel.x/y) all become select/masked-store
form. The dash arm `if_body_162` ends up one straight-line block.
Differentially **identical through 34 and 37**, and the 144 target splits
are gone (505 -> 361 splits; fragments 527 -> 383 at 34, 695 -> 515 at 37).

Measured A/B on the same binary, repeated:

| | 34 frames | 37 frames |
|---|---|---|
| baseline (committed recipe) | 1.36-1.37 s, 0.50 GB | 5.74 s, 1.69 GB |
| dash package | 1.61 s, 0.65 GB | 6.64 s, 2.20 GB |

**+17% time, +30% memory, at both depths.** The profile attributes the
entire +0.24 s at 34: `expand_lanes` 0.12 s/144 calls (an *eager physical
copy* of every vector in the state, where the `UnknownBool` split it
replaces shares the state lazily between fragments), `cfg:player.update_21`
self +0.06 s (the serialized short-circuit evaluates both buttons for
every dash-state where the branch evaluated one), gc +0.03 s (the doubled
vectors are real allocations). `filter_branch` is unchanged (0.17 -> 0.18
s) - the flattening did succeed in not paying vector-branch filters, which
was the failure mode of the bare-expand experiment above.

Masking the dash gate itself (`speculate_region` at `in_k1039_cont` with
mask+expand, so every state runs the arm) was measured too: **2.41 s at
34** - skipping states pay the 4x lane doubling for the rest of the frame
with nothing reclaimed until the boundary dedup.

The conclusion refutes the working hypothesis that expansion "pays as a
package with masking every consumer". It does not, at these depths,
because the baseline's split is not where the money is: an `UnknownBool`
branch duplicates the state by reference, the four sub-states run the
frame tail with mostly-scalar uniform values, and the frame-boundary merge
machinery folds them back into vectors anyway. The package converts that
lazy fork/merge into an eager mid-frame vector copy and buys back only
per-state fixed overhead that is not yet the bottleneck. Lane expansion
remains the right endgame shape - one state per frame with input fan-out
as lanes - but it becomes profitable only when enough of the frame is
branch-free that the merge machinery itself disappears, not
cluster-by-cluster against a healthy merge path. The remaining big
splitters (`in_i1_074_cont` 108 vector filter-splits on per-lane
`dash_time>0`, the jump/dash button diamonds at 102/51) are identical in
both programs and out of this package's scope: the `dash_time` diamond's
else-side is the whole normal-movement body, which contains splitting
branches and so cannot be a region today.

### Interpreter micro-optimization round 1 (2026-08-03)

perf (`-g --call-graph dwarf` on `rewrite bench`, replay samples excluded)
found what the span profile could not see inside `dedup_state` and
`filter_branch`. Five changes, each measured on 2-3 runs, differentially
identical through 34 and 37:

1. **Lazy span names.** Every builtin and closure call `format!`ed its
   trace span name eagerly, profiling on or off. `SpanGuard::new_lazy`
   builds the name only when profiling is enabled. ~1.5%.
2. **Column-major dedup hashing.** `dedup_state` hashed row-by-row - a
   strided walk over hundreds of separately-allocated vectors per lane.
   `hash_rows` folds each vector into per-row running hashes in one
   sequential pass per vector. ~5%. (`rows_equal`, the exactness confirm
   on true duplicates, remains dedup's floor - it must touch every vector
   at two indices and cannot be skipped without trusting the hash.)
3. **Gather-based filtering.** `filter_by_mask` re-scanned the full
   N-entry mask for every vector value in the state. The kept-lane index
   list is now computed once and every vector gathers O(kept). ~5%. The
   dead owned-path (`Value::filter_vectors`) went with it.

4. **One length check in `map2` instead of `zip_eq`'s check per
   element.** The arithmetic inner loop (`interpret_binary_op`) pays a
   branch per lane for an invariant the interpreter already guarantees;
   asserting it once re-enables auto-vectorization. ~5%.

5. **`Arc`'d lane payloads.** `MaybeVector::Vector` now holds
   `Arc<Vec<T>>`, so cloning a vector value - every `load` and `store` of
   a vectorized cell, every select on a uniform mask, every argument
   gather - is a refcount bump instead of a per-lane copy. Writers
   (`expand_lanes`) go through `Arc::make_mut` and pay the copy only when
   the payload is shared; `PartialEq` gets an `Arc::ptr_eq` fast path,
   which also short-circuits state-equality checks over shared vectors.
   ~8% at both depths, +0.15 GB at 37 from retained sharing. This was
   analysed and rejected in 2026-08 *as a gc optimization* ("lane copying
   is ~3% of gc") - correctly, but gc was never where vector clones
   lived: the interpreter's load/store/select paths were, and after
   rounds 1-4 they were the largest remaining cost.

| | 34 frames | 37 frames |
|---|---|---|
| before | 1.36-1.37 s, 0.50 GB | 5.74 s, 1.69 GB |
| after 1-3 | 1.17-1.21 s, 0.44 GB | 4.97-4.99 s, 1.63 GB |
| after 1-4 | 1.11-1.14 s, ~0.48 GB | 4.47 s, 1.50 GB |
| after 1-5 | 1.03-1.04 s, 0.47 GB | 4.11-4.16 s, 1.65 GB |

**-24% at 34, -28% at 37**, lanes identical throughout. Frame 40
milestone check: **18.82 s / 5.47 GB** (948,319 lanes) - the last
recorded milestone was 23.63 s / 5.28 GB on an older recipe, so the
memory cost of sharing stays within ~4% at depth.

**Tried and rejected: gc arena reuse.** gc clones every reachable heap
value into a fresh arena per call; a rewrite re-indexed pointer-free
values in the shared append-only arena (no clone) with a dense renumber
table and a compact-when-bloated backstop. Measured: 4.94 s vs 4.97-4.99 s
at 37 (noise) and +0.1 GB - the gc span was already only 0.07 s, and the
fresh arena per gc is what keeps garbage bounded. Reverted. The next
candidates by profile are the arithmetic inner loop (`interpret_binary_op`
+ `map2`, ~11% of the run) and `Value::clone` on loads (~14% cumulative,
would want `Arc`'d vectors - invasive, unexplored).

### `collapse_loop`: the singleton object-table loops (2026-08-03)

Room (1, 0) holds exactly one entity (read off the cart's map data), so
`objects` is a singleton for the whole search - confirmed by a probe over
the real abstract run: `#objects == 1` in every lane of every state
through frame 40 (948,319 lanes; no death reaches the horizon). The new
`collapse_loop` rule states that premise as a loud
`assert_true(bound == init)` in the loop's preheader and collapses the
loop: counter replaced by its initial value (`get_index objects[i]`
becomes `get_index objects[1]`), head deleted, preheader falls into the
body, latch falls out to the exit. The body is untouched, so early exits
keep working.

16 sites - every `for i=1,count(objects)` check/collide loop that
actually executes (6 in `anonymous_61`, 10 in `player.update_21`),
selected by intersecting `suggest collapse-loop` with the new
`measure_k --blocks` coverage listing. A guard on a never-executed site
would pass every screen without testing anything, so cold sites
(`load_room` scans, other object types) were left alone deliberately.

Two shapes refused themselves, exactly as designed: the inlined `foreach`
and `del` loops compile as `for i=1,32767` with an in-body
`#tbl < i` break - their guard compares `32767 == 1` and fired on frame
1 of the differential run. They need a different collapse (first
iteration always breaks), not this one.

    before   34f: 1.03-1.04 s / 0.47 GB    37f: 4.11-4.16 s / 1.65 GB
    after    34f: 1.02-1.03 s / 0.47 GB    37f: 4.04 s / 1.66 GB
    K unrolled 9546 -> 9226, dynamic instrs/frame mean 3765 -> 3651

Small on its own, as expected - the point is what it unlocks: each
collapsed body now reads `objects[1]` at a constant index, which is the
prerequisite for asserting `objects[1] == <the object being updated>`
(`assume_eq`) and folding the `o ~= obj` term that makes every such
check return nil in this room. Differentially identical through 34 and
37; lanes identical.

### `assume_eq` + pointer folds: the checks fold to nil (2026-08-03)

The cash-in. Three pieces, each independently verified:

* **`assume_eq`** (new pointed rule): `%g = b == a; assert_true %g`
  right after `b`'s definition, then every other use of `b` becomes `a`.
  16 sites - in each collapsed body, the loaded `objects[1]` is asserted
  equal to the object being updated (`%2`). The premise is the singleton
  table again, stated per site, checked per lane.
* **`fold_reflexive`, pointer families** (opt-in `"pointers": true`):
  `x ==/~= x` with a dominating pointer witness (a `get_field` receiver,
  an `assert_pointer`...), `nil` against `nil`, witnessed pointer against
  `nil`. The witness is load-bearing: `==` is *not* reflexive in the
  abstract semantics (`UnknownBool == UnknownBool` is `UnknownBool`,
  `NumberInterval == anything` is `false`, even itself). The opt-in is
  load-bearing too - extending the rule in place changed what the g043
  entry folded and broke replay of everything after it, which is exactly
  why the doc said to keep such rules apart.
* **`fold_select`** (new bulk rule): static truthiness by def-chain
  fixpoint. `a and b` compiles to `select a ? b : a`, so once
  `assume_eq` makes one link a constant `false`, the whole chain is
  falsy - but never constant. The rule classifies falsy/truthy through
  the `and`/`or` select shapes, resolves selects on classified
  conditions to the arm they must produce, and folds branches on
  classified conditions to the edge they must take.

The chain in every collapsed check body: `o ~= obj` folds false, the
select cascade goes falsy, the early-exit branch folds, the found-object
phi collapses to nil, `nil ~= nil` folds, and dce sweeps the lot - 829
instructions in the first round, 891 with the second (16755 -> 15896).

    before   34f: 1.02-1.03 s / 0.47 GB    37f: 4.04 s / 1.66 GB
    after    34f: 0.92-0.94 s / 0.47 GB    37f: 3.85 s / 1.71 GB
    K unrolled 9226 -> 5801, K static 3767 -> 2934

K unrolled is down 39% on the day (9546 this morning). Splits are
unchanged (505 across the same 11 sites) - this stage removed
computation, not branches; branch *executions* fell 10892 -> 6518.
Differentially identical through 34 and 37; lanes identical throughout.

### `collapse_break_loop` and the devirtualized dispatch (2026-08-03)

Two more stages of the singleton line, both neutral-to-positive on time
and structural on purpose:

* **`collapse_break_loop`** (4 sites): the `foreach`/`del` shape -
  `for i=1,32767` with an in-body `#tbl < i` break - collapsed with
  three runtime asserts and no static arithmetic: `init <= sentinel` in
  the preheader, `not break_check` where the break branch was, and a
  re-materialized break check after the payload that must pass
  (re-reading `# t`, so `player_spawn.update`'s mid-payload
  destroy-and-re-add counts). The three `foreach(objects, ...)` loops in
  `__frame` and the `del` inside the spawn's `destroy_object`; cold
  copies left alone. 37f 3.85 -> ~3.8 s, memory 1.71 -> 1.62 GB.
* **`split_call` + inline x2**: the object-update dispatch
  `obj.type.update(obj)` - per-state uniform but two-valued over the
  search - split under `type == load(get_global "player")` (a pure,
  uniform pointer compare; the rule is semantically neutral, both arms
  make the original call), then each arm pinned by its own
  `assert_closure` and inlined: `player.update_21` (2390 instructions)
  and `player_spawn.update_24` (318) now live inside `anonymous_61`.
  **No dynamic dispatch remains on the hot path**; remaining calls are
  1.5% of K, all statically-known foreach/draw closures.

A measurement lesson from the second stage: the machine drifted ~2%
between the morning's runs and these, and against a stale baseline the
inlines first read as a +3% regression. An interleaved A/B on the same
binary (3.88-3.91 vs 3.88-3.90 at 37) showed both neutral. Compare
variants interleaved, never against numbers from hours ago.

Frame 40 milestone: **17.53 s / 5.30 GB** (948,319 lanes; previous
milestone 18.82 / 5.47). K unrolled 5801 -> 5784; `anonymous_61` is now
84.5% of K and `player.update_21` is no longer a function the frame
calls. Differentially identical through 34 and 37 at every step.

### `unroll_loop`: the pixel-move loops laid flat (2026-08-03)

Census work first: of the 869 weighted loads per worst-case frame, the
heaviest cells (`self.check`/`.collide`/`.hitbox.*`, `objects`, `count`)
were re-loaded once per pixel-loop iteration - not because anything
invalidated them, but because `cse`'s forward mode is deliberately
block-local (the 33->47-slots lesson) and the classic cross-block mode
treats every store as a fence for every load. The fix was not a smarter
alias analysis; it was removing the back edges.

`mask_loop` had already made the trip counts uniform (`i <= 8` on
constants; the lane-varying `i <= amount` only feeds masked selects), so
the new `unroll_loop` rule lays each loop out straight: N renamed copies
of head + body chain, phi values threaded copy to copy. Nothing is
guarded because nothing is assumed - the trip count is *simulated* from
the constant init/step/bound in the interpreter's own fixed-point
arithmetic, and `verify` re-simulates it independently, then walks the
copies rebuilding the id bijection instruction by instruction. Applied
to the two hot loops only (the cold candidates - `title_screen`'s 30
iterations among them - stay rolled, per the cold-site discipline).

After `merge_blocks`, each unrolled region is one straight-line block,
and the *existing* block-local `cse` forward entry swept 1,682
instructions of re-derived stanzas - exactly the reach the live-slot
lesson said not to buy with cross-block liveness.

Numbers (interleaved A/B, same binary, 3 pairs each):

* frame 34: 0.93 -> 0.88-0.89 s (-4.5%), lanes identical (92,713)
* frame 37: 3.73-3.84 -> 3.65-3.75 s (-2%), lanes identical (269,059)
* K fully unrolled: 5784 -> **3808** (-34%); per-lane per-frame
  executed instructions mean 2416 -> 1695, max 5152 -> 3201
* reached (function, block) pairs: 240 -> 135
* K by kind: heap 37.7% -> 34.4% (2182 -> 1311 absolute), guard now
  18.1% (690 absolute, unchanged - duplicated asserts are the next
  target), arith 23.9%, const+global 14.3%

Program grew 18,620 -> 18,883 instructions (the 18 copies minus the
sweep) while the hot path shrank by a third. `fold` found nothing to do
afterwards because constant arithmetic folding is still deliberately
unimplemented (needs `op.rs`-differential testing); the unrolled counter
chains (`0+1`, `1+1`, ...) are what it would eat.

### `dedup_guards`: the assert tax (2026-08-03)

The unrolled straight lines made the guard duplication visible:
`inline` plants an `assert_closure` per spliced call site, `cse` unified
the values they check but has no key for asserts (they produce nothing),
and `dce` sees an effect it must keep. The new bulk rule deletes any
`assert_true`/`assert_pointer`/`assert_closure` dominated by an
identical assert on the same SSA operands - those asserts are
deterministic functions of immutable values, so the dominated copy can
never be the first to fire. `assert_value_cell` reads a heap cell and is
excluded. The verifier re-checks every deletion against a *surviving*
covering twin.

501 asserts deleted (program 18,883 -> 18,382). Time-neutral by
interleaved A/B at 34 and 37 - most of the deleted guards were per-state
scalar checks, which the interpreter barely feels. The point is K:
unrolled K 3808 -> **3368** (-12%), guard weight 690 -> 250 (-64%).
That is paid work the eventual compiled kernel no longer contains.
`dce` found nothing afterwards (the surviving first asserts keep their
operands alive), so no `dce` entry follows it in the recipe.

### The `spikes_at` nest unrolled (2026-08-03)

The instruction profiler picked the target: 7.2% of program-under-test
time in the inner spikes body (`mget` + four fixed-point `%` + selects,
re-derived per iteration). Stage L had already masked both nest loops to
constant-bound counters (`k <= 1.0`, 2 iterations, `span < 2` asserted
at runtime), so they were `unroll_loop` shapes except for one refusal:
head-defined values (the in-bounds flags) are read *directly* by the
exit blocks, not through phis. That is sound to unroll - a head-defined
value used outside the loop always observes the final head execution,
since every path out passes through it - so the rule now renames such
uses through the last copy instead of refusing (chain-defined outside
uses stay refused; they cannot dominate the exit). Replay of the earlier
unroll entries is byte-identical, so no opt-in flag was needed.

Inner loop first (its unroll linearizes the outer body), then the
outer, then the usual sweep. Interleaved A/B: 0.89-0.90 -> 0.88 s at
34, 3.75 -> 3.69-3.70 s at 37 (~-1.5%, matching the ceiling: the block
was 7.2% of the ~26% program share). K unrolled 3368 -> **3110**, and
the two K bounds nearly converged (3047 loops-kept vs 3110) - **the hot
path is now essentially loop-free**; what remains rolled is cold or in
`__main`'s per-frame input handling.

### The dash package re-measured; its expansion-free remainder landed (2026-08-03)

The dash package (`plans/dash-package.jsonl`) re-derived cleanly against
the fused frame - the suggester found all eight concretization diamonds,
and the stage-C nest survived the inline byte-for-byte modulo
renumbering. The bet was that the 2026-06 revert's stated blocker
(expanded lanes feeding rolled loops and real branches) was gone now
that downstream is straight-line selects. **The bet lost, at the same
relative magnitude as before**: interleaved A/B at 34, 5 pairs,

* time 0.88-0.90 -> 1.04-1.08 s (**+19%**), memory 0.47 -> 0.65 GB
  (**+38%**), lanes identical, fragments/frame 527 -> 383

`bench --profile` now says precisely why. `expand:expand_lanes` is 9.6%
of wall on its own (144 executions - **expanding duplicates every heap
cell of the state**, all ~279 of them, not just the button-dependent
few), `cfg:anonymous_61` self time rises 0.20 -> 0.26 s from
doubled-width vectors downstream, and the state machinery gives back
only ~0.02 s of `filter_branch` - because it still runs, fragments or
no. Partial lane expansion pays the full lane tax while the ~50% merge
machinery keeps its state count. This bounds the endgame: there is no
monotone path of individually-landable expansion stages; btn expansion
has to be judged as one jump (all sites plus all consumers, machinery
actually collapsing), and `expand` itself likely needs to get cheaper
(copy-on-write lanes, or expanding only reachable-from-button cells).

The expansion-free remainder of the package **did** land: the
dash-direction nest (two inner diamonds, the outer diamond, three tail
triangles, nine dash-gate `create` demotes) flattens with plain
speculate/absorb into select-stores, no `expand` involved. Time and
memory neutral at 34 and 37 (interleaved, lanes identical), and K drops
3110 -> **3060** with reached (function, block) pairs 129 -> 115 - the
`dedup_guards` pattern: a free K win, landed for the compiled kernel's
sake. Differentially identical through 37.

## Where the time goes

Refreshed 2026-08-06 after the overnight stack (see
plans/overnight-2026-08-05.md). `rewrite bench --frames 43 --profile`:

| span | self | share |
|---|---|---|
| `cfg:anonymous_61` (the frame body) | ~17 s | ~55% |
| `filter:filter_branch` | ~4-10 s (grows superlinearly with depth) | 15%+ |
| `vectorize:vm_hash` / `vm_verify` / `vm_probe` / `vm_pack` (the virtual merge, all parallel) | 2.2 / 2.2 / 1.9 / 0.9 s | ~23% |
| `vectorize:merge_groups`, `dedup_state`, `union_diff_states`, `gc` | ~0 | ~1% |

The old materialised-merge costs (`merge_groups` 21%, `dedup_state` 19%
in the previous version of this table, measured at frame 34) are gone:
the virtual merge replaced them and the union pass-through removed
`union_diff`. What remains at depth is the frame body itself - whose
instruction time the cardinality census showed to be 100% duplicate-lane
computation - and the two dash_time fork sites behind `filter_branch`.

The previous version of this section, kept for the fragment-count
narrative it documents (frame 34 numbers, pre-virtual-merge):

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

### `kill_dead`: deadness in the IR, -9% at depth (2026-08-05)

The filter-column census said 41.9% of the vector columns a filter has to
gather live in `local_env`, and the interpreter never drops any of them:
`src/liveness.rs` is a no-op stub and `glue.rs` passes `all_live()`, so
the `BlockBeforeJoin` pruning in `flow.rs` has never pruned anything.

New IR instruction `Kill { values }` and bulk rule `kill_dead` place one
kill at the end of each block naming everything dead there. Deadness is
computed once at rewrite time and stated in the program text, where a
verifier can refute it - the verifier walks *forward* from each kill and
fails if any path reads the value before redefining it, deliberately not
sharing the backwards liveness analysis that placed it.

Measured, interleaved, lanes identical (269,059 at 37; 613,865 at 39):

| | base | +kill_dead |
|---|---|---|
| `rewrite bench` 37 frames | 3.07 s | 2.77 s (**-10%**) |
| runner `--rewritten` frame 39 | 3.91 s | 3.56 s (**-9%**) |

Census at frame 39 shows exactly the predicted mechanism, and nothing else:

| | base | +kill_dead |
|---|---|---|
| state filter calls | 4,000 | 2,532 (-37%) |
| filter elements | 901 M | 432 M (-52%) |
| filter time | 1.12 s | 0.59 s (-47%) |
| in_i1_074_cont | 0.68 s | 0.34 s |
| and_or_join_126 | 0.35 s | 0.17 s |

Filter *calls* fall by a third and *elements* by half - the dead locals
were both numerous and wide. Split counts and lane counts are unchanged;
this buys nothing by removing branches, only by making each one carry
less. 197 kills in `anonymous_61`, naming 3,090 locals, 15.7 per kill.
Differentially identical through 37.

Two things worth keeping. The first version of the verifier treated a phi
operand as a use on *every* incoming edge, which rejected every loop head
in the program (`__init: kill of %746 ... is followed by a read of it`) -
a phi consumes its operand on one edge only. And the applier's kill set
needs a third term beyond the obvious two: locals live at the end of a
predecessor but dead on entry here, which die *on the edge*. Without it
one path drops a value while a sibling path carries it, and two states
that should merge differ by something neither can read.

### Uniform vectors collapse to `Scalar` at construction: -8.4% / -8.6% memory (2026-08-05)

The output-cardinality census (`CELESTE_INSTR_CARD=1` with
`rewrite bench --profile`) measured, for every instruction, how many
distinct values its output vector contains. Result at frame 37 of the
rewritten program: 0.54 s of non-call instruction time, and 100% of it -
to three decimals - is lanes duplicating a value that already exists
elsewhere in the same output vector. No hot instruction: the top one is
13 ms; mean output is ~50k lanes with 1-28 distinct values, max distinct
anywhere 51. Several selects produce *constant* vectors (dist/exec = 1.0),
and nothing mid-frame demoted a constant vector to `Scalar` - only the
merge's `unvectorize_if_possible` did, once per frame.

So `MaybeVector::vector` now early-exit-scans fresh lanes and returns
`Scalar` when they are uniform. Every producer funnels through it (map,
map2, select gather, filter gather, concat), so uniform vectors stop
existing at all, downstream ops go per-state instead of per-lane, and the
column drops out of the merge's dedup key. Safe against merge-group
fragmentation because shape normalization is representation-blind.
Non-uniform vectors pay a few comparisons (exit at the first differing
lane).

Interleaved A/B, 3 rounds each, identical lane counts:

|                    | base           | collapsed      | delta |
|--------------------|----------------|----------------|-------|
| runner `-n 39`     | 44.36 s / 16.2 GiB | 40.64 s / 14.9 GiB | -8.4% time, -8.6% mem |
| `bench --frames 37`| 2.89 s         | 2.71 s         | -6.4% |

Differentially identical through 37.

### The virtual-concat merge, fourth attempt: landed (2026-08-05)

Multi-state merge groups now dedup over a *virtual* concatenation
(`src/interpreter/virtual_merge.rs`): hash, dedup and gather run straight
off the per-fragment columns, and the 23.6M-row pre-dedup table (frame 37)
is never built. The two killers of the three parked attempts are handled -
uniform columns leave the dedup key explicitly, and every access is a
sorted piece walk. Output is identical to the materialised pipeline
(same hash seed, so the same survivor mask; a test holds the two states
equal, representation included), which stays as the fallback.

Interleaved A/B, 3 rounds each, identical lane counts:

|                    | materialised   | virtual        | delta |
|--------------------|----------------|----------------|-------|
| runner `-n 39`     | 40.42 s / 14.9 GiB | 39.03 s / 14.6 GiB | -3.4% time |
| `bench --frames 37`| 2.68 s / 1.41 GB peak | 2.57 s / **1.01 GB** peak | -4.1% time, **-28% mem** |

The memory figure is the point: on the rewritten path the transient
concat *was* a third of peak RSS. That gap should widen with depth, since
the pre-dedup table grows ~1.6x per frame.

### The hint_normalize points: measured load-bearing, kept (2026-08-06)

The two shipped mid-frame normalize points (plus the leftover in
_update_62) predate the branch-free rewrite, so removing them from the
rewritten program was tried via a new `remove_hint` recipe rule (the
rule is landed and stays; the entries are parked). Differential verify
is identical across the difference - merging is semantics-preserving -
and fragments grow 23 -> 505 mean per frame. Seven interleaved rounds
at frames 42: **18.39 s with hints vs 19.09 s without (+3.8%), peak
9.6 -> 10.0 GB**. So mid-frame fragment control still pays ~4% even
with the virtual merge making merges cheap. The context-partitioned
merge idea would *replace* these merge points with something stronger
(partition on the fork-condition cells) rather than delete them.

Separately and independently, the unsound per-column state comparison
that used to live behind these hint merges (union_diff's
NormalizedState) is deleted outright - union_diff is now a guarded
pass-through that panics with instructions if a real dedup is ever
needed again.

### Ranged filter gathers: -13.5% (2026-08-05)

A new census column asked how *chunky* filter gathers are: contiguous
runs in the kept-index list. Answer at frame 37: **30.6 lanes per run**
at `filter_branch`, 19.5 at `filter_split_flr` - nothing like the ~2 a
random mask would give. Adjacent lanes share history (concat and
first-occurrence dedup preserve arrival order), so they usually agree on
a branch condition. The same census also showed `filter_dedup` at zero
calls - the virtual merge has eliminated dedup-side filtering entirely.

So `filter_by_mask` now scans the mask once into `[start, end)` ranges
(`KeptLanes`) and each vector gathers by `extend_from_slice` per run -
memcpy, since the payloads are `Copy` - instead of one indexed copy per
lane.

Interleaved A/B, 3 rounds, identical lane counts:

|                    | before | after | delta |
|--------------------|--------|-------|-------|
| runner `-n 39`     | 38.77 s | 33.54 s | **-13.5%** |
| `bench --frames 37`| 2.54 s | 2.38 s | -6.4% |

The largest single win of 2026-08-05, and it stacks with the uniform
collapse (which raises run lengths by removing lane-varying columns).
If lanes were ever sorted by context at the merge, run lengths - and this
win - would grow further; that is the cheap consumer the sorted-lane idea
was missing.

### Hint-block union kept as states, not as a persistent normalized set (2026-08-05)

Tried: replacing the accumulated `Vec<State>` union at hint_normalize
blocks with a persistent `FxHashSet<NormalizedState>`, so fixed-point
rounds stop re-normalizing (clone + gc) the whole union. Parked on the
numbers: runner `-n 39` 38.77 s -> 39.13 s (+0.9%, slower in all three
interleaved pairs), bench 2.56 s -> 2.52 s with peak 1.01 -> 0.93 GB.

The reason there was nothing to win: `merge_hint_normalize` runs exactly
once per hint block per frame (74 spans = 37 frames x 2 blocks), so every
fixed point converges in a single round and the cross-round membership
check never rejects anything. The old code's `accumulated.is_empty()`
fast path was already optimal; the new version only added deep normalized
copies (the union states are Arc-shared, a `NormalizedState` is not) held
until the CFG ends. If fixed points ever start taking multiple rounds -
loops through hint blocks - revisit with a digest-indexed set that stores
Arc-shared states and re-normalizes only on digest hits.

### If-converting the two hot fork sites: not available, and the nearby one is a 2.2x regression (2026-08-05)

The per-site filter census put 99% of `filter_branch` on two branches -
`in_i1_074_cont` (0.68 s at frame 39, on per-lane `dash_time > 0`) and
`and_or_join_126` (0.35 s). Both were tried.

**Neither branch is convertible by any current rule.** `suggest` proposes
nothing for either: both arms contain `btn` branches on `UnknownBool`,
which no select can express. This re-confirms the stage-R note - the
`dash_time` diamond's else-side is the whole normal-movement body, and it
contains splitting branches, so it cannot be a region. Removing that
blocker needs `expand`, which has been measured as a regression twice
(+17%, +19%) with a diagnosed cause (`expand_lanes` physically doubles all
~279 heap cells when only the button-dependent few can differ).

What *is* available nearby: 34 candidates from `suggest` over
`anonymous_61` (if-convert, speculate-region, speculate, absorb-stores,
convert-ternary, decompose-truthy, sink-store), of which 25 verify at 32
frames. As a bundle they cost **3.02 s -> 6.6 s at 37 frames**, and
bisection puts the entire regression on one entry: `if_convert` at join
`in_h061_and_or_join_126`, the and/or triangle feeding the second hot
branch. Alone it is 3.02 -> 6.58 s (**+118%**), 1.39 -> 1.84 GB. The other
24 are neutral. Nothing landed.

The mechanism is worth keeping, because it generalises. Split counts are
**completely unchanged** - 503 splits across the same 11 sites with the
same per-site counts - and lanes through the two hot sites barely move
(9.8 -> 10.8 M and 4.1 -> 4.4 M at frame 37). What changes is the *width*
of every filter:

| frame 37 | baseline | +cif-014 |
|---|---|---|
| state filter calls | 3,972 | 32,912 (8.3x) |
| filter elements | 355 M | 2,029 M (5.7x) |
| filter time | 0.39 s | 1.76 s |
| in_i1_074_cont | 24 events, 0.23 s | 48 events, 0.89 s |
| and_or_join_126 | 12 events, 0.12 s | 24 events, 0.76 s |

A filter call is one vector column, so calls/events is the number of live
vector columns in the state. If-conversion roughly **quadrupled live
columns** at the split points, because eager evaluation keeps both sides'
intermediates live across the branch. So a conversion that does not
actually remove a split is doubly bad: it pays for both arms *and* makes
every downstream filter wider. The converted branch here was one of the
always-uniform ones (`and_or_continue_125`/`if_condition_100` never appear
in the split table), which is the same lesson as "if_convert removed 81
branches and only 4.6% of the splits" - now with a cost mechanism
attached.

Corollary for the ranked plan: **filter cost is driven by live vector
columns, not only by lane counts.** Narrowing what is live at a split is
an independent lever on the same 1.03 s, and unlike if-conversion it does
not require the btn blocker to fall first.

### The merge machinery measured against its task (2026-08-03)

Is the ~50% merge share an implementation problem or the task's real
cost? `bench --profile` now prints the merge data volume (new
`merge_stats` counters, always on, per-call granularity, no measurable
overhead), and a synthetic calibration replicated the dedup algorithm
on workload-shaped data. Answer: **the implementation is within ~20% of
optimal for this algorithm; the surprise is the task size.**

The volume, at 34 frames (0.88 s wall):

    merge data volume: 102 vectorize calls, 1012 states in -> 109 out (109 groups)
      concatenate: 929 states x ~279 heap cells = 0.3M cell clones (0.11s, 410 ns/cell)
      dedup: 109 calls, 5.68M rows x 17.1 row-weighted vector columns = 96.9M
             elements, 5.04M rows removed (89%)
      dedup throughput: 0.29s dedup_state self = 3.0 ns/element (50.4 ns/row)

At 37 frames (3.7 s wall): 23.61M rows, 90% removed, 58.5 ns/row, and
`merge_groups` cloning slows to 1268 ns/cell under 1.7 GB RSS.

Reading:

* **Throughput is near-optimal.** A stripped reimplementation of the
  same algorithm (dense i32 columns, same FxHash fold, same bucket
  structure, 170k rows x 17 columns, 89% dups) runs at 42.8 ns/row vs
  50.4 measured - and the elementwise hash pass is the *minor* term
  (0.4-3 ns/element; the ~40 ns/row hashmap probe/insert dominates
  both). A sort-based formulation ("sort the states, dedup
  consecutive") measures *slower*: 49.4 ns/row for the sort+scan alone,
  on top of the same hashing. Micro-optimization (open addressing, no
  per-bucket `Vec`) might buy 15-20% of the dedup span, i.e. ~0.05 s
  at 34. There is no large constant factor hiding here.
* **The task is ~60-90x the surviving lanes.** 5.68M rows are hashed
  to keep 0.64M distinct (92.7k lanes at the final boundary); at 37 it
  is 23.6M rows for 269k lanes. Every lane fans across the frame's
  unknown branches (6 btn splits send whole states down both edges,
  unfiltered) and gets re-crushed at ~3 merge points per frame
  (boundary + 2 `hint_normalize`). The 89-90% duplicate rate *is the
  search doing its job* - most button choices don't change the
  outcome, and dedup is where that convergence is detected.
* **Consequence**: the merge machinery is the search's pruning
  operator running at ~50 ns per candidate; the lever is not its speed
  but its input volume - fewer rows manufactured per frame (the lane
  expansion endgame) or a representation where duplicate detection is
  cheaper than per-row hashing (hierarchically ordered lanes, where
  most columns are runs). Caching row hashes across a frame's 3 merge
  points is the one modest implementation win available (~a third of
  row cost at the repeat sites).

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
