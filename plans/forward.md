# The forward frame: where it goes, and what is next (2026-10-09)

Measured on the harness in BENCHMARK_DATA.md ("The forward frame,
2026-10-09 night"): room (6,2) 100%, f56 -> f57 (5.86M states in, 6.74M
kept), 16 workers on a 7950X3D (16 cores, 32 threads), DDR5 at 3600 MT/s
(~45 GB/s measured).

## Where the frame goes now (4.6 s wave, 76 worker-s; f58 7.0 s)

| phase | worker-s | what |
|---|---|---|
| kernel | 6.6 | the AVX-512 kernels themselves (11% padding left) |
| emit | 41 | per (lane, body): cell, level -1, key (14.6 fields, 2 mixes each), transfer words and their id, the unit's dedup cache, the row push |
| flush.edges | 10.6 | 16-B raw edge records into per-worker files |
| flush.admit | 6.3 | the door |
| flush.gather | 4.9 | kept rows into the next frame's pieces |
| flush (sort, pre, post) | 1.8 | |
| beside the wave | ~27 CPU-s | the previous frame's edge compaction (raw records -> runs sorted by target); GONE since `edge-inversion` (below) |

**It is not memory-bound.** The format study's DRAM estimate for today's
format is 45-85 GB a frame (1-1.9 s at 45 GB/s); the wave is 4.6 s of 16
busy cores. The time is per-emission COMPUTE, and the emit profile is now
flat (no line above ~1%): 512M emissions (87 per input state) of which half
land on cells level -1 drops; 49.5M rows survive the unit's dedup; 6.7M are
new.

## What changed tonight, and why it is exact

- **Emissions that duplicate a one-fork neighbour are masked** (f7264fd).
  Button forks (and the two `move` floor forks on freeze lanes) are cancelled
  per lane by data the build cannot see: `freeze > 0`, `dash_time > 0`, a
  jump in the air with `p_jump` widened. On those lanes every such body
  computes the bit-identical row and transfer. A lane is skipped where an
  earlier body of its outcome, one fork away, took it and every differing
  output slot is equal: that body's emission IS this one, and the sink
  would have merged it. (Found by a census of (lane, key, transfer) groups,
  branch `fork-research`.)
- **Level -1 judges a row at emission** (eabc2a8): a dropped queue was
  dropped whole at flush, so the row is judged by (shape, cell) as soon as
  the kernel gives the cell; it records only its pos-graph edge and its
  source's drop note, as flush did. A runtime guard asserts the queue's
  shape is the one judged.
- **Slices by region across a whole unit** (4163a00, 197267a): a slice
  may span 64-lane id groups (each lane's predecessor records keyed by its
  own group), so a unit's lanes are bucketed by region, and units are ~16
  a worker (1024-4096 lanes). Padding 53% -> 11% here, 49% -> 11% in the
  kernel-bound room (3,0) at 8 px (its frame 3.77 -> 2.08 s).
- Transfer words per lane (2cbfcb4), edge record stores (691b80a, 853724c),
  the compaction's sort (a0cf74c): plain overhead.
- **The kernels and the level -1 table are built before the wave**
  (38ea337). Not a speedup of a real run (it paid the build once), but every
  one-frame number before it held a 3.9 s build (or a 34 s table rebuild).

Each was checked byte-identical on the graph (`ckhash --edges`, two
schedules and against the binary before), plus the gates and the full suite.

## Decisions to talk through

1. **Edges by source (design S of plans/format-study.md on `format-study`).**
   The edge pipeline is now the largest single cost: 13.6 worker-s of
   records in the wave plus ~35 CPU-s of compaction beside the next one,
   i.e. ~1/3 of the machine's work per frame, and 16 B per edge through the
   page cache (4 GB a frame here). Writing each source's edges where they
   are emitted (sources ARE the processing order) removes the raw files and
   the compaction, and the study sizes it at 1.8 B/edge against 3.5. The
   cost is on the backward: the BFS and the W fixpoint walk predecessors
   (runs by target). By source they become pull sweeps, or need a reverse
   index over the marked subgraph only. Not started: it changes the
   backward's algorithm, so it is a decision, not an optimization.
2. **The arc graph's memory.** Onlydiag 2500m's backward needed 176.7M
   marked nodes and 3.38G edges in RAM (~50 GB: 33 B a node + 12 B an
   edge), killed at a 50 GB cap and rerun at 90 GB. u32 ids and the
   by-source transfer tables would roughly halve it. Big rooms will hit this
   before they hit forward time.
3. **Exact packed keys instead of the 128-bit hash.** The study found the
   packed state fits a u64 (≤ 41 bits) with zero collisions: no hash, no
   collision caveat, and cheaper than the 2 x 14.6 mixes an emission pays.
   It moves every key, so the gates would need re-pinning on evidence.
4. **Forking on `freeze > 0` / `dash_time > 0` at trace time** (the research
   pass's further option) would let the build collapse ~95 bodies into one
   on those lanes, saving kernel work too. The mask already took the emit
   side; this would add forks the abstractions doc warns about. Not done.
5. **mimalloc's purge delay.** 0 (safe-run.sh) returns every freed page
   at once; 100 ms makes the frame ~10% faster (page faults 5.1M -> 1.2M a
   2-frame run, sys 32 -> 12 s) but keeps ~2 GB more resident between
   frames here, proportionally more in a big room. Left at 0; the better
   fix is fewer transient allocations (the edge buffers were the first, a
   fifth of the faults).
6. **Kernel-bound rooms.** Room (3,0) 100% at `r0sxhfn` (8 px regions):
   335k bodies, the largest kernel 3,328 bodies and 221k fused nodes; a
   frame of 8.4M states took ~90 s, 54% of it in the kernels themselves
   (perf on the live search at f67, the binary before the unit-wide
   slices). Half of that was padding (49% of slice lanes empty at 8 px),
   now 11% (f49: kernel 21.7 -> 7.7 worker-s, wave 3.77 -> 2.08 s). What
   remains: a slice evaluates every body's nodes, live or not. MEASURED
   (branch `skip-research`, `CELESTE_SKIP_CENSUS`, sampled slices of both
   harnesses): 78% (room (3,0)) and 89% (room (6,2)) of bodies have a live
   lane in a slice, so skipping dead ones saves little - ideally 25% / 31%
   of kernel instructions, with one guard per outcome 16% / 18%, i.e.
   ~4.5% / ~1.6% of the frame before guard and spill costs; guards by
   fork prefix save 0%. (A body's `error` cone must be skipped with it,
   else ~1%: the emit loop reads `error` only under `live`.) Not worth a
   codegen change. The real gap is 78-89% live against 44-51% taking a
   lane: bodies that duplicate a neighbour (the mask drops them after the
   kernel) - decision 4 would remove them before it.

## The edges inverted once (2026-10-09, branch `edge-inversion`)

The forward keeps every frame's raw records (no compaction beside the
waves) as 64k-record CHUNKS (`edges::write_chunk`, ~3.6 B a record against
16), and the backward inverts the whole tree into the unchanged runs when
it first opens the graph (`edges::invert`, 4 frames at once). Measured
(BENCHMARK_DATA.md, "The edges inverted once"): on the harness the
contention was ~25% of f58's wave (6.76 -> 5.11 s without the compaction,
4.91 s with the chunked writes; f57 4.4-4.5 s in all three); the raw tree
of the 2300m search is 46.3 GB (16-B records would have been 207 GB, more
than the disk had), its runs 49.2 GB; inverting its 13.0G records takes
~105 s (4 frames at once; 225 s one at a time). End to end, on a shared
machine (load 17-38), two pairs of the whole 2300m search: forward 349 /
431 s before, 319 / 292 s after; totals 439.5 / 523.4 s before, 484.9 /
464.5 s after (the inversion included). So the forward is faster but the
inversion gives it back: a wash end to end, at ~8% less CPU. The
inversion runs ~11 of 32 threads (90 ns of CPU an edge: the chunk
decodes, the bucket sorts, the run encoder); it would have to get ~3x
faster to make this a clear win. The 16-B records were only workable with
the per-frame compaction: a whole tree of them does not fit.

The compaction's own cost (`rewrite compact-bench` over a frame's raw
records kept by `CELESTE_KEEP_RAW=1`): f57's 258M records, 2.6 s wall and
29.8 CPU-s; with the bucketed scatter (no shared atomics, one buffer) 2.45
s and 27.2 CPU-s, the runs byte-identical. What remains is the encoder
(group sorts and varints into fresh buffers) and the per-file counts.
