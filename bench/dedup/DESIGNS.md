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

## `vprod`: production's dedup path replayed (src/bin/vprod, 2026-10-09)

A faithful copy of production's path (fg-2300 d2eda1c: `run_slice`'s
emission loop, `ForwardSink`, `RowCache`, `DropNotes`, the direct edge
cache, the door verbatim, `end_frame`), driven by the capture; the
simplifications are listed at the top of src/bin/vprod/main.rs (a 64-B row
payload, key and transfer id given, level -1 table rebuilt from the drop
flags, `last_edge` reset per unit rather than per slice, no `any_win`, an
identity `Renumber`). It reproduces f57 exactly: raw 31,685,344, flushes
184,432, kept 6,735,699, edge words 257,724,013 (all distinct). The edge
record count (256.63M) moves by ~100 between runs because new ids depend on
which worker admits first, and those ids hash into the direct cache.
The emitted rows (raw) and kept counts are the same at 1 worker; flushes are
173,203 there.

Measured on a LOADED machine (load 27-41, other agents' searches running),
best of 3. 16 workers: wave **1.73-1.80 s** + end_frame 0.09 s. Of 28.8
worker-s: emit (net of flush) 18.7, flush 6.9 (admit 3.1, edges 2.75, sort
0.6, gather 0.27), end_call 0.1, finish 0.26. 1 worker (units in wave
order): **16.2-17.1 s** + end_frame 0.69 s. Of that: emit 12.9, flush 4.2
(admit 1.74, edges 1.70, sort 0.55). Reading and parsing the capture alone
(`vread`) takes 0.58 s at 16 threads and 1.45 s at 1 thread, and that is
inside emit.

## H. Bit-packed state + bitset (`census`, `splits`, `prepbits`, `bits`, `bitsr`, `packtime`; 2026-10-09)

The 2022 hard-coded version (`67e341d`): a dense `PosMap` by position, the
player's flags mixed-radix (`CompressedPlayerFlags`, 18480 values) with the
dash fields as ONE joint digit (`VALID_DASH_COMBOS`), a bitset over the flags.
Applied here to the capture's abstract states via `rewrite field-dump`
(every row of the tree `/var/tmp/canon-h62-f56`, r0sxhf, level -1 94,5,
f0-f57: its value cells' `av_code`s; every key recomputed from (shape,
fields): 0 mismatches). Only 15 cells vary (shape 2, 95.5% of the rows); rem,
p_jump/p_dash/jbuffer and every object are uniform at this level.

Shape 2 budget: shard = (shape, cell) (10531 occupied of a 126 x 113 box);
high = freeze 3 x djump 3 x grace 7 x flip 2 x spd.x 1351 x dash combo 89
(7 fields: dash_time, dash_effect_time, target x/y, accel x/y, has_dashed;
product 8910) = 15.15M (23.85 bits); low = spd.y 96 (6.58 bits). 30.44 bits
a shard, 43.8 with the cell. A dense bitset: 1.5e13 bits (1.9 TB) for 43.8M
states (density 3e-6) - infeasible by ~4 orders even per shard (182 MB a
cell). What fits: a per-shard open-addressing DIRECTORY on the exact high
digits (u32) holding a 128-bit bitset over spd.y (10.06M entries, 4.4 states
each, fill 4.5%): 0.69 GB at load 0.5 (v3c 2.54, v4 0.94).

Single thread, cpu 4, interleaved x3, load 1.5-2.1, counters over the timed
loop: v3c 2.98-3.00 s, v4 2.98-3.01 s, `bits` 2.24-2.25 s (7.6 ns a lookup;
id = slot << 7 | low), `bitsr` 2.94-2.95 s + 0.49 s frame-end pass (dense
rank ids: old = rank in the frame-start set, new ranked at the end).
DRAM fills 52.5M / 42.4M / 28.5M / 31.0M; L2 misses 102M / 79M / 46M / 57M.
All: 6,735,699 new, decision fingerprint 51e2f3ecb444e25d, and an exhaustive
per-lookup decision + id-bijection check against v3c's ids: 0 violations.
Packing from raw fields (scalar, binary search for spd): 34.6 ns a row
against 29.7 ns for the scalar 2 x 15 mix64 key.

### H2. Per-(shape, cell) dictionaries; speed-keyed masks (`cellcensus`, `bitcell`, `bitspd`, `bitspd2`)

Per cell (17,882 shards; "by states" = weighted): distinct spd.x p50 33
(by states 579, max 1273), spd.y 32 (57, max 75), dash combo 45 (72, max
89), flags combo 11 (19, max 54), flags+dash joint 151 (356, max 713).
Dense bitmask per cell over its LOCAL product, total and occupancy:
flags x dash x spd.x x spd.y 20.4 GB, 0.03%; (flags+dash) x spd.x x spd.y
4.7 GB, 0.12%; (flags+dash) x (spd.x, spd.y) joint 0.83 GB, 0.66%. None is a
few % dense. Only with a directory: key ((flags+dash), spd.x), mask over the
cell's spd.y dictionary: 8.9% of the product, 61 MB of masks + the directory.
Speed-keyed (mask over flags): key (cell, spd, dash), mask freeze/djump/grace/flip
(126): 1.36 states an entry, 1.07% fill; key (cell, spd), mask over the cell's
flags+dash joint (<= 768 bits): 2.37 states an entry, 0.61% fill (room-wide
dictionary 1419 values: 0.16%, 3.4 GB).

Implemented with ONE 64-bit word per directory entry (the word's index in the
key; 12-B entries), (key, bit) precomputed untimed like `bits`' packing.
Interleaved x3, cpu 4, load 1.9-2.2: v3c 2.98-3.00 s; bits 2.26-2.27;
**bitcell 2.09-2.13 s (7.0 ns, 0.35 GB; 10.07M words, 6.8% filled)**; bitspd
2.29 (0.70 GB, load 0.8 so ids fit 32 bits); bitspd2 2.22-2.23 (0.69 GB).
DRAM fills 52.6M / 28.4M / 25.4M / 33.4M / 30.7M. All: 6,735,699 new,
fingerprint 51e2f3ecb444e25d, exhaustive decision + id check 0 violations.
Production would need each cell's spd.y dictionary (or an append-only per-cell
one assigned on first sight, itself a lookup per emission), and the room's
flag/dash/spd.x value sets in advance or a fatal guard on a new value.

## I. The structure of the state set (`structure`, `sharing`, `bitintern`; 2026-10-09)

The set: the visited set at the END of f57 (door f0-56 + f57's 6,735,699 new),
shapes 2, 3, 7 (43,841,483 states, 17,854 (shape, cell) shards; the five
spawn/death shapes, ~120 rows, left out). Fields: freeze, has_dashed,
dash_effect_time, dash_time, djump, grace, dash_accel x/y, dash_target x/y,
flip.x, spd.x, spd.y (13; x, y = the cell); values as numeric ranks of the
room dictionaries (alphabets 3 2 11 5 3 7 3 3 3 3 2 1351 96).

### What a shard looks like (/var/tmp/emitcap/shards/2_<cell>.txt)

Samples: tiny (56,18) 8 states, median (103,107) 485, weighted-median (70,97)
12,079, heaviest (47,114) 69,428, near spawn (32,104) 15,723, mid-room (64,64)
1,262. Weighted-median, sorted flags, dash, spd.y, spd.x:

    freeze djump grace flip hasd d_time det tgt.x tgt.y accel.x accel.y  spd.y               spd.x
         0     2     0    0    0      0   0     0     0       0       0  -0.320068359375     0.7001190185546875
         0     2     0    0    0      0   0     0     0       0       0  -0.320068359375     0.7498779296875
         0     2     0    0    0      0   0     0     0       0       0  -0.320068359375     0.749908447265625
         0     2     0    0    0      0   0     0     0       0       0  -0.320068359375     0.7499237060546875
         ...   (0.74993896, 0.74995422, 0.74996948, 0.74998474, 0.75, 0.75001526, ...: consecutive ulps)
         0     2     0    1    0      0   0     0     0       0       0  -0.110076904296875  -0.6999359130859375
         0     2     0    1    0      0   0     0     0       0       0  -0.110076904296875  -0.699920654296875

- spd.x comes in tight CLUSTERS around multiples of 0.05 (0.7, 0.75, 0.2, 0.6,
  1): runs of consecutive raw fixed-point values (1/65536 apart) - friction and
  acceleration rounding, i.e. the remainder's widening does not reach spd.
  spd.y is a gravity ladder (-0.87, -0.66, -0.45, -0.24, -0.03, 0.075, 0.18,
  0.39, ...) shared by all flag groups of a cell.
- Functional dependences: dash_time is a function of dash_effect_time;
  dash_accel x/y and has_dashed are functions of dash_target (1.00 children a
  parent in the trie); the dash fields persist after a dash (direction of the
  LAST dash), so the 89 "combos" are ~(dash_time, target x, target y).
- The same sub-sets repeat ACROSS cells (`sharing`): the 10.06M nodes
  (shard, flags+dash, spd.x) carry only 79,390 distinct spd.y sets (0.8%);
  the 3.10M nodes (shard, flags+dash) only 93,211 distinct (spd.x, spd.y)
  sets (3.0%).

### Compression, bytes a state (raw: 14 B, u8 a field, spd.x u16)

| encoding | gzip -6 | zstd -3 | zstd -19 | xz -6 |
|---|---|---|---|---|
| all, row-major, layer order | 3.43 | 3.63 | 2.36 | 2.11 |
| all, column-major, layer order | 3.39 | 3.57 | 2.97 | 2.87 |
| all, sorted flags+dash > spd.x > spd.y (o0), row | 2.12 | 0.82 | 0.235 | 0.220 |
| o0, column | 0.358 | 0.375 | 0.145 | 0.153 |
| o0, delta to previous row | 0.455 | 0.444 | 0.131 | 0.166 |
| o0, first-changed field + its delta (raw 2.67 B) | 0.198 | 0.190 | 0.068 | 0.073 |
| greedy order (fewest trie nodes), first-changed (raw 3.31 B) | 0.240 | 0.201 | 0.040 | 0.053 |
| dash > flags > spd.y > spd.x (o3), first-changed (raw 3.39 B) | - | 0.204 | **0.040** | - |
| spd.x first / spd.y first, first-changed | - | 0.65 / 0.64 | 0.125 / 0.094 | - |
| 6 sample shards, raw row / o0 first-changed | 3.98 / 0.234 | 4.26 / 0.297 | 3.15 / 0.193 | 2.83 / **0.183** |

zstd's window decides it: greedy first-changed at -19 is 0.236 B with a 16 KB
window, 0.111 at 128 KB, 0.074 at 1 MB, 0.040 at 8 MB. The last factor ~6
is CROSS-SHARD repetition; within a shard (the samples, compressed apart) it
is ~1.5 bits a state.

### Tries and bounds (whole set; per level: `structure/report.txt`)

| order | subset bound sum log2 C(alphabet, children) | succinct trie (LOUDS or bitmap per level) |
|---|---|---|
| flags+dash > spd.x > spd.y | **6.45 bits** | **12.41 bits (1.55 B)** |
| dash > flags > spd.y > spd.x | 7.33 | 14.73 |
| greedy: has_dashed > grace > accel.x > tgt.y > accel.y > tgt.x > djump > flip > freeze > dash_time > det > spd.y > spd.x | 7.34 | 14.77 |
| spd.x > spd.y > flags+dash | 15.00 | 29.84 |

Flat bound log2 C(product, n) per shard: 21.0 bits (local dictionaries),
25.5 (room alphabets). Conditional entropies in o0 (bits, states uniform):
freeze 0.14, has_dashed 0.83, det 0.45, dash_time 0.00, djump 0.60, grace
0.41, accel 0.38 + 0.41, target 0.21 + 0.21, flip 0.97, spd.x 5.55, spd.y 2.87
(sum 13.05 = mean log2 of the shard size). The flags+dash prefix has 3.10M
nodes (175 a shard); spd.x then fans out 3.24, spd.y 4.36.

### Conclusions

- Flags+dash first, then the speeds, exposes the most per-shard structure;
  whatever is first, the speeds carry ~8.4 of the 13 bits of entropy.
- Achievable: ~6.5 bits a state per shard in theory (trie subset bound),
  ~12 bits as a plain succinct trie, ~1.5 bits with a good model of a shard
  (sample compression), ~0.3 bits exploiting cross-cell repetition - against
  12-20 B in the hash tables now, 8-12 B a state for bitcell's directory
  (0.35 GB; its masks alone 2.8 B a state).
- Structure 1, measured (`bitintern`): `bits` with the 128-bit spd.y mask
  hash-consed: entry = (high digits u32, mask id u32); the room's distinct
  masks in one table (68k at the frame start, 230k by its end with the
  garbage of inserts; 3.7 MB, L2/L3-resident); an insert interns mask | bit.
  0.23 GB (5.2 B a state). Interleaved x3, cpu 4, load 2.1-2.6: v3c 3.16-3.17 s,
  bits 2.27-2.28, **bitintern 2.45-2.47** (8.4 ns: +4.7G instructions for the
  intern, -16% DRAM fills against bits), bitcell 2.78-2.80. Validated as before
  (6,735,699 new, fingerprint 51e2f3ecb444e25d, 0 violations).
  bitcell was 2.09-2.13 s in H2's reps and is 2.77-2.80 s today, reproducibly
  (DRAM fills 25M -> 51M, dTLB misses 2.7M against bits' 0.19M; its process is
  21 GB RSS from the untimed precompute and only 4.7 GB of it on huge pages,
  MemFree 22 GB): page-size luck, not the design - unresolved.
- Structure 2 (sketch): a DAG trie - per shard a directory on the flags+dash
  combo (3.10M entries, 175 a shard: a sorted u16 array per shard, SIMD
  search, ~1 line) pointing to an INTERNED spd set (93k distinct sets holding
  4.27M (spd.x, spd.y-set id) entries, ~17 MB) and interned spd.y masks (79k x
  16 B = 1.3 MB). ~25 MB + 17 MB + 1.3 MB = ~45 MB, ~1 B a state. Lookup: shard
  array -> combo search -> spd.x binary search in a shared, cache-hot set (~6
  steps over ~46 entries) -> mask bit: ~3 dependent loads, mostly L2/L3
  (~15-25 ns cold, ~5-10 ns in a sweep). Insert (2.6% of lookups):
  copy-on-write of the path, intern 2 sets (~100-200 ns) -> +0.7-1.3 s of 6.7M
  inserts single-threaded, the catch; garbage collection of dead sets at the
  frame's end.
- Structure 3 (sketch): LSM - old layers frozen per shard as first-changed
  encoded blocks of 64 states with a block index (~3 B a state uncompressed,
  0.1-0.3 B with zstd blocks); the current frames in bitcell/bitintern. A
  lookup probes the small delta, then decodes one block (~64 x 3 B, ~50-100
  ns): only worth it where memory, not time, binds.

### I2. Position LAST: a mask over a region's cells (`posregion`, `posmask4`, `posmask8`)

Regions as the kernels tile them (`RegionGrid::of`: `x.div_euclid(px)`,
aligned at 0), keyed with the shape. Tree A: (shape, region) > flags+dash >
spd.x > spd.y > position mask. Same f57 set (43.8M states).

| R | regions | A leaves | states/leaf | mask fill | key u32 + mask, B/state (load 0.5) | distinct masks; interned B/state | succinct trie A / C | subset bound A | zstd -19 per region, A / C | explicit gamma, room / group dicts (A) |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 (cell) | 17,854 | 43.8M | 1 | - | 5.0 (10.0) | - | 13.4 / 12.5 b | 6.45 b | 0.352 / 0.341 B | 11.7 / 8.7 b |
| 4x4 | 1,262 | 5.47M | 8.0 | 50% | 0.75 (1.5) | 2,108; 1.0 | **3.5** / 12.1 b | 1.71 b | 0.066 / 0.116 B | 3.9 / 3.6 b |
| 8x8 | 385 | 2.19M | 20.0 | 31% | **0.60 (1.2)** | 19,095; 0.40 + 0.2 MB | 3.8 / 12.1 b | 1.76 b | 0.037 / 0.083 B | 3.5 / 3.2 b |
| 16x16 | 123 | 1.12M | 39.0 | 15% | 0.92 (1.8) | 47,236; 0.20 + 1.5 MB | 6.9 / 12.2 b | 2.38 b | **0.028** / 0.071 B | 3.45 / 3.2 b |

- The cross-position repetition IS now inside one region: zstd per 8x8
  region alone (0.037 B) matches the whole-set 8 MB window (0.040, section I);
  per cell it is 0.35. Position after flags+dash (C) loses it (12 bits).
- B (mask over (spd.y, position) under spd.x) is worse: 109 states a leaf at
  8x8 but 1.8% fill (room spd.y), 3.0% (per-region spd.y): 4.2 B/state.
- Explicit coding (no compressor; first changed level gamma(levels - level),
  its index delta gamma, following fields gamma(index + 1)): 3.2-3.9 bits a
  state with regions, 8.7-11.7 per cell. Section I's zstd inputs were already
  room-dictionary indices (numeric ranks), u8 a field, spd.x u16.
- Live window of the x-major sweep (A leaves; (region, high) entries): cell
  548k / 289k; 4x4 163k (1.0 MB) / 67k; 8x8 120k (1.4 MB at 12 B) / 43k;
  16x16 86k (3.1 MB at 36 B) / 27k. All L2/L3-sized.

`posmask4` / `posmask8` (run_words with a directory per (shape, region) on
(high digits, spd.y) = u32, one 64-bit word over the region's cells, id =
slot << 6 | cell; tables MADV_HUGEPAGE): posmask8 2.19M entries, 6.2M slots
x 12 B = **0.07 GB** (1.7 B a state); posmask4 5.47M entries, 0.19 GB (16 of
64 bits used). Validated: 6,735,699 new, fingerprint 51e2f3ecb444e25d, 0 id
bijection / decision violations. SMOKE timing only (load 4-6, cpu 4 shared:
the drops loop alone varied 0.30-0.63 s): posmask4 1.89 s, posmask8 2.19 s
(and 3.16 / 4.59 s under heavier contention), bits 4.64 s in the same
contended window (2.27 quiet). AnonHugePages at the timed loop 4-8.7 GB of
21 GB RSS (most RSS is the untimed precompute; the table is 70-190 MB).

## MLP: many misses in flight per thread (`mlp`, 2026-10-09)

`dedup-bench DIR mlp TABLE MODES` (src/mlp.rs) drives the existing tables
with latency-hiding loops, every one with EXACTLY the sequential order's
decisions and ids:

- `seq`: the plain loop (must time as the baseline);
- `gG` group prefetching (Chen et al. 2004): for G lookups compute the home
  line(s) and prefetch, then run the ordinary sequential probe on each;
- `pD` rolling prefetch at distance D: prefetch lookup i + D's home, probe i;
- `aK` AMAC (Kocberber et al. 2015): a ring of K lookups, each a state
  machine (prefetched -> scanned read-only -> resolved), COMMITTED IN QUERY
  ORDER.

Tables: `v4`, `bitcell`, `posmask4`, `posmask8`, `bitintern` (the baselines'
layouts), and `v4b`, `bitcellb`, `posmask4b`, `posmask8b`: the same keys in
64-B buckets of 5 entries (5 x 8-B fingerprint + 5 x 4-B id, or 5 x 4-B key +
5 x 8-B mask word), one AVX-512 masked compare a bucket. The keys are their
own tags, so there are no Swiss-table control bytes, which would cost a second line.

Exactness. A scan is read-only and may run before earlier lookups commit. Slots
are never deleted or moved, and a key, once written, never changes, so a
scan's Hit(s) stays valid. Its Empty(s) is re-checked at commit: s still empty
-> insert (the sequential probe stops there too); s now holds OUR key -> hit
(an in-flight duplicate: two lookups of one new state, the second sees the
first's insert); s holds another key -> continue the sequential probe after
s. Mutable payload (the id, the mask word, the interned mask) is touched only
at commit, in order. Chains longer than the prefetched lines: the scan stops
at the first slot outside them; AMAC re-prefetches and requeues it (or, for
the ring's head, which blocks every commit, finishes it with demand misses);
group/rolling prefetch take the demand miss in the sequential probe. Counted:
`re-prefetches` 13% of lookups for v4 (12-B slots at load 0.8), 4-7% for the
linear bitcell/posmask/bitintern tables, 0.4-0.6% for the buckets.

Validated, every table x mode (9 x 10: seq g8 g16 g32 p8 p16 p32 a8 a16 a32):
6,735,699 new, decision fingerprint 51e2f3ecb444e25d, 0 id-bijection /
decision violations against v3c's ids (bits::check); v4 and v4b ids
IDENTICAL to v3c's (0 of 257,724,013 differ).

The timed tables (and the edges/frontier buffers) are now
madvise(MADV_HUGEPAGE)d BEFORE first touch in every baseline too (THP is
`enabled=always, defrag=madvise`: without the advice a fault only takes a
free huge page, which explains H2 -> I's bitcell 2.1 -> 2.8 s); each TIMED line reports
the table's mappings' AnonHugePages and the process's.

### The envelope

    t_lookup ~ max(compute, sum over levels of misses/lookup x latency / in-flight)

Per lookup (quiet counters of H/H2; DRAM ~100 ns, L3 ~12 ns):

| table | DRAM fills/lk | L3 hits/lk | at 1 in flight | measured (quiet) | implied in flight | at 8 in flight | compute floor (insn/lk / IPC ~3, 4.5 GHz) |
|---|---|---|---|---|---|---|---|
| v4 | 0.165 | 0.14 | 18 ns | 10.4 ns | ~1.7 | 2.3 ns | ~80 / 3 -> ~6 ns |
| bitcell | 0.099 | 0.08 | 11 ns | 7.0 ns | ~1.6 | 1.4 ns | ~80 / 3 -> ~6 ns |
| posmask8 (70 MB table) | (stream only) | ~0.1-0.2 | 1-2 ns | ~4.3 ns (smoke) | - | <0.3 ns | ~70 / 3 -> ~5 ns |

The out-of-order core already keeps ~1.6-1.7 misses in flight (lookups are
independent), and the x-major sweep keeps 84-90% of lookups in cache. So
MLP can cut v4 by at most ~1.7x and bitcell by ~1.2x, down to the compute
floor. The prefetch pass costs instructions (home computed twice: v4b 63 ->
114-128 insn/lk with g32/p16; AMAC ~190), and on posmask8's L3-resident table
there is nothing left to hide: prefetching there can only lose.

### Smoke timings (NOISY: load 2-6, shared machine, cpu 6; not results)

Single runs (validation runs, checks excluded; `smoke-bench` with perf):
v4: seq 2.85, g8/16/32 3.06/2.87/2.65, p8/16/32 3.39/3.33/2.83, a8/16/32
5.03-6.59/4.82-5.55/3.77-4.62 s. v4b: seq 2.22, g32 1.86, p16 1.99, a16 2.78
s. bitcell: g32 2.24, p16 2.28, a16 3.22 (its seq outlier 5.14). bitcellb: seq
1.70, g32 1.67, p32 1.68, a16 2.44. posmask8: seq 1.34, g32 1.71, p16 1.69,
a16 2.71; posmask8b seq 1.37. posmask4 seq 1.71, posmask4b p32 1.67.
bitintern: seq 2.67, g32 2.41, p32 2.33, a16 3.35. Counters (one smoke run
each, per lookup): v4 2.3 branch misses and 176 insn (a 7.1 s outlier run),
v4b seq 0.2 and 63. In this noise, the bucket layout (c) is the visible win
(fewer mispredicted probe loops); group/rolling prefetch are worth 0-15%;
AMAC is slower everywhere (its per-step bookkeeping ~2-3x the instructions,
nothing out of order to exploit under in-order commit).

### Run it (quiet machine)

`/var/tmp/emitcap/mlp/bench.sh [CPU]` (copy: bench/dedup/mlp-bench.sh):
baselines v3c v4 bitcell bitintern posmask4 posmask8 and every table x mode,
interleaved x3, `taskset` to one cpu (default 4), `perf stat -D -1 --control
fifo` over the timed loop only (cycles, instructions, branch misses, L1D /
dTLB / L2 misses, L3 and DRAM fills), `uptime` per run, then a summary table
(min/median/max, ns and counters per lookup, IPC, the table's huge pages);
`bench.sh --summary LOG` reprints it. ~1 h for the full matrix (TABLES, MODES,
BASE, REPS override); the bitcell/posmask precompute is cached under
/var/tmp/emitcap/mlp/cache, keyed on its inputs' size and mtime.
