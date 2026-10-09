# The frame's dedup core: designs and envelopes (2026-10-09)

The problem, measured on the capture of room (6,2) 100% (2300m), f56 -> f57
(`CELESTE_EMIT_CAPTURE`, canonical-layer order):

| quantity | value |
|---|---|
| emissions after the kernels' dup mask | 511.9M |
| of them dropped by level -1 (note the source, nothing else) | 254.1M |
| lookups into the visited set (= the frame's distinct edges) | 257.7M |
| distinct targets of those lookups | 10.9M (4.2M old, 6.7M new) |
| hits on states from EARLIER frames / on states new THIS frame / inserts | 88.3% / 9.1% / 2.6% |
| visited set at the frame's start | 37.1M states (16-B key + 4-B id: 0.74 GB) |
| old states touched at all this frame | 11.3% |
| live window of a left-to-right sweep, by target cell | mean 4.9%, peak 14.2% of the set |
| hits by age of the state hit | mostly 13-24 frames old (4-8% each); see plots/ages-room62-f57.html |

Machine: 7950X3D, 16 cores / 32 threads, L2 1 MB a core, L3 96 MB + 32 MB
(two CCDs), DRAM ~45 GB/s measured, ~100 ns a miss.

## The floor for this core

The emissions come out of the kernel in registers; what must touch memory:

- the visited set's touched lines: 10.9M distinct targets x 64 B = 0.7 GB
  (each touched once, perfectly batched);
- the edges out: 257.7M x 12 B raw (src, target, transfer) = 3.1 GB, or ~1 GB
  compact (3.6 B an edge);
- the new rows: 6.7M x 64 B = 0.43 GB; the drop notes: a 23 MB array
  (cache-resident).

2.1-4.2 GB, so **0.05-0.1 s** at 45 GB/s. Compute: ~512M records at ~3 ns a
record over 32 threads = **~0.05 s**. Floor ~0.1 s; "within 3x" = **0.3 s**.

## Designs

Each: the idea, the envelope, and what is measured so far (replay of the
capture, bench/dedup; quiet machine).

### A. One global hash table (`v0`)
Every lookup a random probe into a ~1 GB table.
- Latency-bound: 257.7M misses x 100 ns / 32 threads = **0.8 s** at one miss
  in flight a thread; with batched prefetching (~8-10 in flight) it becomes
  bandwidth-bound: 257.7M x 64 B = 16.5 GB -> **0.37 s**.
- Measured: **2.1 s** (std HashMap behind 1024 mutexes, no prefetch).
- It throws away the stream's locality: neighbouring states hash apart.

### B. The visited set sharded by position (`v1`; what production's door does)
One small table per (shape, cell): ~17.9k shards, ~2k entries (~50 KB)
each. Lookups follow the frontier's position order, so a thread works in a
few shards at a time.
- The live window (5-15% of the set, 50-150 MB) is about L3-sized: most
  probes ~20-40 ns -> 257.7M x 30 ns / 32 = **~0.25 s**, plus locking.
- Measured: **1.0 s** (a mutex and a std HashMap per probe dominate).

### C. B with a strict sweep (`v2`)
The lookups in one global left-to-right sweep, one contiguous x-band per
thread (equal work): threads share fewer shards and cache lines.
- Envelope as B, less contention. Measured: **0.9 s**.

### D. Partition, then probe (radix-partitioned, lock-free)
Pass 1: each thread appends each lookup (key 16 B + src 4 + transfer 4 = 24 B)
to the bucket of the shard band that owns its target (sequential,
non-temporal stores). Pass 2: each bucket's thread probes ITS shards only
(no locks; a band's shards fit L2/L3), in-bucket duplicates adjacent if
sorted.
- Traffic: 6.2 GB written + 6.2 GB read sequentially = 12.4 GB -> **0.27 s**;
  probes ~5 ns x 257.7M / 32 = 0.04 s. **~0.3 s.** A 16-B record (8-B key
  fingerprint + index) halves the traffic: **~0.18 s**.
- The edges come out grouped by target band for free.

### E. Sort-merge join, no hash table at all
Sort the frame's lookups by (shard, key) with an LSD/MSD radix sort, then
merge-join them against the visited set kept SORTED per shard (production's
door base is already sorted per shard).
- Radix sort of 257.7M x 24 B: 2-3 passes of 6.2 GB read + write = 25-37 GB ->
  **0.55-0.8 s**; the merge scans only touched shards (<= 0.74 GB). **~0.6-0.9 s.**
- Fully sequential and deterministic; duplicates collapse in the sort; the
  edges come out SORTED BY TARGET, which is what the backward needs - it
  would replace the edge inversion (36-90 s a search) for free.

### F. Hot/cold tiers across frames
A hot tier (states touched in the last few frames, or young states) in a
cache-friendly table; the cold rest behind it, probed only on a hot miss.
- If the hot tier is ~15% of the set (~110 MB, about L3) and catches ~95% of
  the hits: 257.7M x 20 ns / 32 = 0.16 s + 5% cold x 100 ns = 0.04 s ->
  **~0.2 s**.
- Unknown: the re-touch statistics across frames (the ages show hits spread
  over ages 1-25, each age only 5-26% touched). Needs a multi-frame capture.

### G. Dedup in front of the lookups (production's unit cache)
A per-thread, L2-resident cache of recently seen keys in front of any of
A-F. Production's unit cache cuts 257.7M lookups to 31.7M rows reaching the
door (8x), because a unit's lookups repeat (same targets from neighbouring
sources).
- 257.7M cache probes x 3 ns / 32 = 0.025 s; 31.7M real lookups even at A's
  worst 100 ns / 32 = 0.1 s -> **~0.13 s**. The catch: an edge must still be
  recorded for every lookup, including ones whose target is only queued
  (not yet given an id): production's pred masks and extras.
- Combines with B-F.

## What production adds on top (not in this bench yet)

Production's wave is ~4.3-4.8 s; its emission (~41 worker-s) and flush
(~24 worker-s) also include the row KEY (14.6 fields x 2 mixes per
emission), the transfer's words and id, the pos-graph edges, the drop
notes, the queues' row columns, the new rows' gather and the edge records.
The bench takes the keys as given; a fair comparison needs those costs
measured separately (the key hash is per emission: 512M x ~15 ns = ~0.25 s
of 32 threads by itself).

## Next

Implement D, E, G (and B without a mutex: single-owner shards) in the bench;
measure each against its envelope with `perf stat` (LLC misses, bytes,
instructions); capture 3-4 consecutive frames for F.
