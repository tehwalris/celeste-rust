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

## J. Batched per-region processing (`regionbatch8`, `regionbatch16`; 2026-10-09)

The visited set at rest per (shape, R x R region); a core takes one region
and its batch of the frame's lookups: DECODE into an in-core structure, run
the batch (membership + inserts), RE-ENCODE. All regions of f57 run (397 at
8x8, 131 at 16x16), one core, in sequence. A state in a region: key =
flags+dash > spd.x > spd.y mixed radix (< 2^32), pos = its cell; an entry =
(key, mask over the region's cells). The batches (key, pos, query index; 12 B)
are built in the untimed setup, in sweep order: in production that grouping
is a partition pass over the emissions (design D's envelope, ~0.1-0.3 s at
32 threads), NOT included below.

At rest, bytes a frame-start state (37.1M), 8x8 / 16x16: (a) a posmask
table at load 0.5 1.69 / 2.53; (d) sorted (u32 key, mask) array 0.60 / 0.92,
+ zstd -1 0.16 / 0.11; (b) varint key delta + raw mask 0.46 / 0.85; (c) (b) +
zstd -1 0.06 / 0.05, zstd -3 0.05 / 0.05. (No lz4 crate offline.)

In-core structures: HASH (open addressing on the key, entries (key, old
mask, new mask), the batch in sweep order; at the end the entries sorted by
key for the re-encode) and MERGE (sort the batch by (key, pos), merge with
the decoded sorted entries: no hash table; std sort_unstable, no radix).

SMOKE timings, load 4-6, cpu 4 shared - NOISY, dedup only (no ids):

| R | format + structure | decode | batch | re-encode | total | ns a lookup |
|---|---|---|---|---|---|---|
| 8x8 | raw + hash | 0 | 0.96 s | 0 | 0.97 s | 3.7 |
| 8x8 | varint + hash | 0.01 | 0.96 | 0.00 | 0.97 | 3.8 |
| 8x8 | varint + zstd -1 + hash | 0.02 | 0.99 | 0.03 | 1.04 | 4.0 |
| 8x8 | varint + merge | 0.01 | 4.73 | 0.00 | 4.74 | 18.4 (the sort) |
| 16x16 | varint + hash | 0.01 | 1.83 | 0.01 | 1.84 | 7.2 |
| 16x16 | varint + zstd -1 + hash | 0.03 | 1.71 | 0.07 | 1.81 | 7.0 |
| 16x16 | varint + merge | 0.01 | 9.01 | 0.00 | 9.02 | 35 |

Per region (8x8, varint + hash): median 26,748 states / 104,501 lookups:
decode 2.2 us, run 381 us, encode 1.9 us (3.7 ns a lookup, 14 ns a stored
state); weighted-median 895,781 / 5.40M: 65 + 17,425 + 51 us (3.3 ns);
heaviest 1,556,132 / 15.07M: 350 + 56,826 + 227 us (3.8 ns). Decode and
re-encode are <1% even with zstd: the batch (24 lookups a target, 2.6% new)
dominates. bits ran 2.27 s quiet (4.64 s in the same contended window).
All variants: 6,735,699 new, every lookup's decision equal to v3c's,
fingerprint 51e2f3ecb444e25d (merge at 16x16 first overflowed a 24-bit
batch index: fixed).

IDs (kept optional, `regionbatch8 merge,hash ids`; validated once: ids dense
in [0, 43,841,605), bijection with v3c's, 0 violations): CANONICAL RANKS,
final when a region finishes - an old state = old_base[region] + rank in
the region's frame-start (key, pos) order (a prefix popcount over the
entries + popcount below pos), a new state = n_door + new_base[region] + rank
among the region's new states. Cost with ids: hash 1.77 s, merge 5.8 s (noisy).
Ideas for edges later, not implemented: (1) edges recorded per DESTINATION
region in the batch itself (src, xfer, pos, key) - the batch is the edge list
sorted by target, the backward's order; the destination id is the batch
entry's rank, resolved in the region pass; (2) a stable intermediate index:
(region, entry slot, pos) with slots append-only within a frame (inserts
never move an entry; re-encode compacts at end_frame and emits a slot ->
rank map); (3) source-side: the frontier's ids are ranks of the previous
frame's regions, so (src region, rank) is already canonical.

Quiet run left for later: /var/tmp/emitcap/regionbatch/bench.sh (cpu 4,
perf stat --control over the timed loops, interleaved with bits).

### J2. regionbatch on all cores (`regionpar`; 2026-10-09, quiet: load 1.6-3.7)

regionpar R fmt T:sched:split...: the J pipeline (decode, open-addressing
in-core table, re-encode), dedup only, specialized per mask width (8x8: a u64 mask, 16-B table
entries; J used 80-B entries for both, hence 0.57 s here against 0.97 s).
Untimed and reported apart: grouping + loading every region's at-rest bytes
and its batch (8-B (key, pos) entries) 11 s; building the units (heavy
regions split by a hash of the key into disjoint sub-batches, each with the
matching subset of the stored entries) 1-16 s; per-thread scratch prefaulted
to the largest unit (10-48 MB a thread, 0.05-0.16 s). Nothing in the timed
section allocates; the decision bits are pre-touched. Units heaviest first,
taken by an atomic index (steal) or assigned up front (static, LPT). Pinned:
1 thread cpu 4; 8 = cpus 0-7 (the V-cache CCD); 16 = 0-15; 32 = 0-31 (SMT).
Every run: 6,735,699 new, 0 decision mismatches, fingerprint 51e2f3ecb444e25d.

Wall, 3 interleaved reps (8x8 varint / 8x8 varint+zstd -1 / 16x16 varint):

| threads | no split | split 1M lookups | split 0.5M |
|---|---|---|---|
| 1 | 0.568-0.570 / 0.601-0.604 / 0.578-0.579 s | - | - |
| 8 | 0.085-0.086 / 0.089 / 0.136-0.137 | 0.078-0.079 / 0.085-0.086 / 0.074 | - |
| 16 | 0.078-0.081 / 0.082-0.094 / 0.152-0.154 | **0.057-0.058** / 0.059-0.060 / 0.064 | - |
| 32 | 0.094-0.097 / 0.096-0.107 / 0.152-0.155 | 0.064-0.067 / 0.067-0.068 / 0.074-0.076 | 0.059 / 0.061-0.063 / 0.065-0.066 |

Speedup (8x8 varint, best split): 7.2x at 8, 10x at 16, 9.6x at 32 threads.
- Critical path without a split = the heaviest region (15.1M lookups at 8x8:
  32 ms alone, 47 ms at 8 threads, 78-96 ms at 16-32 with the others
  running); 32 threads idle 27-35% (8x8), 67-70% (16x16). Splitting by key
  hash fixes it: idle 1-6% (steal). Static LPT: 22-31% idle at 32.
- Busy time grows with threads (sum 0.57 s at 1, 0.63 at 8, 0.90 at 16, 1.8-2.0
  at 32): bandwidth. System-wide DRAM (amd_umc CAS x 64 B) over the timed
  part: 1 thread 2.67 GB in 0.567 s; 8 threads 2.34 GB in 78 ms (30 GB/s);
  16 threads 2.53 GB in 58 ms (**44 GB/s**, the measured DRAM ceiling); 32
  threads 2.54 GB in 60 ms. The traffic is the batch stream itself (257.7M x
  8 B = 2.06 GB) + the at-rest sets: the region work is in-cache, the frame
  is bound by streaming its lookups once. zstd -1 at rest costs +3-5%.
- Partition cost (NOT measured, mental estimate): writing the 257.7M lookups
  into region batches = another 2.06 GB write + read at ~40 GB/s = ~0.1 s,
  unless the kernel's emissions go straight into per-region buffers. A
  pre-dedup of the emissions (G, the unit cache, 8x) would shrink the batch
  stream, the bound here, by the same factor.
Script: /var/tmp/emitcap/regionpar/bench.sh; log reps.log next to it.

## K. Interfaces and edges (design notes, not built)

(a) **Stable ids under posmask**: a state's id = (region, entry number,
cell). Entries are numbered per region in order of first appearance; within
a frame, canonically, by sorting the frame's new entries by key at the
region's end. A new bit in an existing entry never moves an id; a new entry
appends. Cost: ~4 B an entry (its number, or a key -> number map), spread
over ~20 states at 8x8.

(b) **Edge bundles**: a source-cell mask + one shift (dx, dy) + a transfer,
from one source entry to one target entry, split per target region (a
shifted 8x8 mask spans up to 4 target regions). Measured in K1 below. Open:
mapping the backward's per-remainder winning sets W onto mask operations (W
is per state and per remainder interval; a bundle shares the transfer, so
one W lookup per target entry could serve every cell of the mask, but the
W of different target cells differ).

(c) **Edges written per target region straight out of the region pass**
(J's batch IS the edge list sorted by target): the backward reads them in
target order and needs no inversion (today 36-90 s a search).

(d) **Fallback**: edges carry target KEYS, not ids; the backward resolves
them with its own per-region batch against the stored sets (the same
pipeline as J, run backward).

(e) **Kernel / storage interface options**:
- one generated STATE CODEC (field order, dictionaries, the joint
  flags+dash digit, region geometry), consumed by the ASM assembler (SIMD
  pack / unpack in the kernels) and by the storage;
- batch-granularity, monomorphized calls: arrays of packed emissions per
  L2-sized chunk, no per-element calls or callbacks;
- the storage owns the schedule: the sweep calls the kernels as producers
  and their emissions go straight into per-target-region buffers - this
  also removes the partition pass (J2's estimate ~0.1 s);
- the stored state IS the row (exact packing): the kernels decode the
  frontier from storage, no separate columns;
- migration: a thin adapter, a posmask door behind the existing admit call;
- a dictionary miss (a value the codec does not know): FATAL plus a rebuild
  at the frame boundary, or an exact, counted, LOUD slow path inside the
  storage - never silent (CLAUDE.md: no silent deopt, no unrefuted widening);
- before production: check the codec is EXACT (injective on the key) for the
  object levels and the platform levels, not only room (6,2) r0sxhf.

### K1. Edge bundle census (`edgecensus`, `edgepairs`; analysis only)

All 257,724,013 edges of f57 (the capture's non-dropped emissions; sources
from door.bin via src ids, targets by key, both through the field dump's
packing). TRANSFERS MUST BE CANONICAL BY CONTENT: `ForwardSink::xfer_id`
interns per worker, so the first census (62,870 "transfers", 151.7M bundles
of 1.70 edges, the transfer "splitting" 4.8x) was an artifact. The capture
now also writes each worker's table (`x{n}.bin`, `capture::dump_xfers`); it
was rerun for f56 -> f57 (CELESTE_HUNDRED=1 CELESTE_LOADING_JANK=2
CELESTE_LEVEL_MINUS_ONE=94,5, tree /var/tmp/cap2-tree, capture
/var/tmp/emitcap2) and reproduces the emissions exactly (raw 31,685,344,
kept 6,735,699, order-free multiset fingerprint d013be0be5231f9d in both).
92,406 distinct transfers by content. 853,523 edges shift > 8 px, 2,341,353
change shape (deaths / respawns).

Global bundles (no region pairing):
- source side (src shape + 8x8 region + key, transfer, tgt shape + key,
  shift): **35.3M bundles, 7.30 edges each**; median 4 cells, p90 18;
  edge-weighted median 16, p90 35; 21.6% single-cell (3.0% of edges). A
  shifted mask spans 1 / 2 / 3 / 4 target regions: 23.1M / 10.4M / 0.25M /
  1.55M. Target side: 36.0M bundles, 7.15 edges each.
- the transfer still splits 12.1% of the transfer-free bundles (31.5M ->
  35.3M). Of the 3.43M split groups: 1.52M have ONE source cell (one source
  state reaches the same target state through several guard pieces of its
  remainder: the move or a collision depends on where in [-0.5, 0.5) the
  remainder lies - e.g. x [0, 65534) const 32768 (stopped by a wall) against
  [65534, 65536) rot 2); 1.47M have several cells all with the SAME transfer
  set (the same guard pieces in every cell); only 0.45M differ ACROSS cells.
  By component: y guard + action kind 1.47M, y guard 0.92M, x guard + kind
  0.75M, x guard 0.28M: guards at remainder thresholds, and const actions
  (a wall or the floor stopping the move).
- bytes an edge: (b) bundle records (src entry u32, one tgt entry u32 per
  spanned region, shift 1 B, global transfer id varint, u64 mask) 2.92 raw,
  0.34 zstd -1; (a) flat stable-id edges per target region 4.76 raw, 1.37
  zstd -1.

Per (source 8x8 region, target 8x8 region) PAIR (`edgepairs`): 2,152 pairs;
edges a pair median 3,534, edge-weighted median 2.29M, max 12.5M (the
diagonal pairs dominate: the heaviest are a region to itself); a source
region reaches median 9 target regions (max 21). Within a pair, fields:
src entry, src cell, tgt entry, tgt cell, transfer. Bytes an edge:

| encoding (per pair) | B an edge |
|---|---|
| DFS first-changed varint, src entry > src cell > tgt entry > tgt cell > transfer (flat) | 4.78 |
| same, src entry > tgt entry > transfer > src cell > tgt cell | 3.42 |
| same, transfer > src entry > tgt entry > src cell > tgt cell (best) | 3.29 |
| LOUDS trie estimate, best order | 2.47 |
| flat bound log2 C(product of local alphabets, n) | 4.45 |
| (c) relation (src entry, tgt entry, transfer) -> cell pairs: 49.6M groups of 5.19 edges, 98.3% with ONE uniform shift (shift + u64 mask, or a short list) | **1.76** |
| (c) + zstd -1 / -3 per pair block | **0.18 / 0.16** |
| best DFS + zstd -1 / -3 per pair block | 0.28 / 0.25 |

Against production ~4.3 B an edge (vprod-edges raw_f057 1.1 GB) and flat
4.76: the per-pair relation is 2.4x smaller raw and ~25x with zstd. The flat
bound exceeds the trie and relation sizes because the relation's structure
(a uniform shift inside a group) is not in the product alphabet.
Dumps: /var/tmp/emitcap/edgepairs/ (small pair complete; the weighted-median
and heaviest pairs truncated to 3,000 lines). What one pair looks like: a
source entry fans out to a few target entries (different spd / flags after
the frame's inputs) at one or two shifts (the remainder's guard pieces land
in different cells), and the same (src entry, tgt entry, transfer) repeats
across a run of source cells with one shift - the mask.

## L. Roadmap (agreed with Philippe)

1. **Edges in the bench**: from K1, the per-(source region, target region)
   relation - (src entry, tgt entry, transfer) -> a uniform shift + a source
   cell mask (98.3%), else a list: 1.76 B an edge raw, 0.18 with zstd -1,
   against 4.3-4.7 now; write them from regionpar (a region pass emits its
   pairs' blocks); validate the edge set against vprod's key-based fingerprint
   (`3b4f8b60b8767421`, the sweep branch).
2. **A realistic bench**: no untimed grouping - a producer replays the
   emissions in kernel / source order into per-target-region chunk buffers
   drained in cache, no counting pass; rows for the new states (or "the
   stored state is the row"); the packing cost; drop notes and level -1;
   several consecutive frames (needs a multi-frame capture); an object or
   platform room to check the codec is exact.
3. **The real kernels end to end**, behind the gates (ckhash, posgraph, arc
   gate, arc-check, the known route), at the same throughput. The goal state
   is kernel compute (~0.44 s a frame) being the limit.

## M. Edges in the region pass (`regionedge`; roadmap stage 1, 2026-10-09)

Data: /var/tmp/emitcap2 (the capture with per-worker transfer tables, K1),
the field dump of its tree. Setup (untimed, ~35-40 s, plus 20 s to build
the units at split 100k): every non-dropped emission becomes a 16-B batch
element of its TARGET (shape, 8x8) region in kernel / capture order - (target
key, target cell, source region, source entry number, source cell, global
transfer id); transfers interned by CONTENT across the worker tables
(92,406). Units = target regions split by a key hash into parts of <= 100k
lookups (2,877 units); work stealing heaviest first, per-thread scratch and
edge arenas prefaulted, pinned as J2.

Stable ids: (region, entry number, cell). Frame-start entries numbered by
key within the region (the source ids are these); a frame's NEW entries
numbered at the unit's end `NEW | part << 20 | rank` (rank by key among the
part's new entries: canonical, schedule-free; dense renumbering is left to
the frame boundary). A new bit in an existing entry keeps the entry's number.

Edge encodings, written by each unit into its thread's arena right after
the dedup (no inline compression):
- `rel`: per (source region, target region) block, groups (src entry, tgt
  entry, transfer), first-changed varint; members: count + a uniform shift
  (zigzag varints) + a u64 source-cell mask (>= 8 members) or a list of
  source cells, else explicit (src cell, tgt cell) pairs. Built by sorting
  the unit's edges as u128 keys.
- `flat`: per target region, sorted (tgt entry, tgt cell, src id), varint
  deltas + transfer.
- `relh` (tried, slower): the groups by a hash table instead of a sort -
  0.93 s at 16 threads against 0.56: the group table is ~20% of the edges and
  does not stay in cache.

Validation (untimed, after every rel/flat run): every unit's block decoded
back to (src shape, cell, key, tgt shape, cell, key, transfer content) - 257,724,013
edges, order-free fingerprint fca41b0f1a28b51d EQUAL to the capture's; once
EXHAUSTIVELY (both sets sorted and compared: IDENTICAL, 257,724,013
distinct); the stable-id bijection over all 43,841,605 states: 0 violations;
6,735,699 new in every run.

Wall, 3 interleaved reps (load 2.0-3.6), split 100k:

| threads | none (dedup only, 16-B batch) | rel | flat |
|---|---|---|---|
| 1 | 0.630-0.642 s | 6.88-6.90 s | 6.81 s |
| 8 | 0.102-0.104 | 0.915-0.921 | 0.958-0.964 |
| 16 | 0.106-0.107 | **0.550-0.557** | 0.608-0.611 |
| 32 | 0.120-0.121 | 0.652-0.688 | 0.725-0.730 |

Edge bytes: rel **2.10 B an edge** (0.54 GB for the frame; zstd -1 per unit
block, untimed, 0.62 B); flat 6.17 B (zstd -1 3.74). The per-unit split costs
rel some grouping against K1's per-pair 1.76 B. Production ~4.3 B.
DRAM (system-wide UMC, timed part): none 1 thread 4.6 GB, 16 threads 4.8 GB
in 0.107 s (45 GB/s: the 16-B batch stream, 4.1 GB, at the DRAM ceiling;
J2's 8-B batch ran in 0.057 s); rel 1 thread 10.5 GB, 16 threads 16.9 GB in
0.562 s (30 GB/s); flat 16 threads 21.2 GB.

Where the time goes: the dedup is 0.64 s of one core; the edges add ~6.2 s
of one core (26.7 ns an edge against 2.5), almost all the u128 sort of each
unit's edges (100k x 16 B spills L2) and its traffic. Next: a cheaper
grouping - a radix / counting sort on a narrower per-unit key (source region
local index, dense target number), or keep the kernel's source order (an
edge's group repeats across the source cells of one entry, which a sweep
visits in x-major runs), and the 8-B batch with the edge payload carried
by the producer.
