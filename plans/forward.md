# The forward frame: where it goes, and what is next (2026-10-09)

Measured on the harness in BENCHMARK_DATA.md ("The forward frame,
2026-10-09 night"): room (6,2) 100%, f56 -> f57 (5.86M states in, 6.74M
kept), 16 workers on a 7950X3D (16 cores, 32 threads), DDR5 at 3600 MT/s
(~45 GB/s measured).

## Where the frame goes now (5.6 s wave, 95 worker-s)

| phase | worker-s | what |
|---|---|---|
| kernel | 8.9 | the AVX-512 kernels themselves |
| emit | 51.7 | per (lane, body): cell, level -1, key (14.6 fields, 2 mixes each), transfer words and their id, the unit's dedup cache, the row push |
| flush.edges | 13.6 | 16-B raw edge records into per-worker files |
| flush.admit | 7.4 | the door |
| flush.gather | 5.5 | kept rows into the next frame's pieces |
| flush (sort, pre, post) | 2.3 | |
| beside the wave | ~35 CPU-s | the previous frame's edge compaction (raw records -> runs sorted by target) |

**It is not memory-bound.** The format study's DRAM estimate for today's
format is 45-85 GB a frame (1-1.9 s at 45 GB/s); the wave is 5.6 s of 16
busy cores. The time is per-emission COMPUTE: 512M emissions (87 per input
state) of which half land on cells level -1 drops; 56.6M rows survive the
unit's dedup; 6.7M are new.

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
- Slices by region within an id group (d8361a3), transfer words per lane
  (2cbfcb4), edge record stores (691b80a): plain overhead.
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
