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

## Sweep front (`sweep`, src/bin/sweep; 2026-10-09)

Approach #2, doing what `vprod` does, multithreaded, from the same capture.

**The work** (per frame, all of it timed): every emission (257.7M lookups +
254.1M level -1 drops) checks the level -1 table (a dense per-(shape, cell)
`from` array, not production's FxHashMap); a drop notes its source's min
`from`; every lookup writes an EDGE RECORD, 12 B (source row index, target
id, transfer), staged per thread in L1 and written with non-temporal stores
into 1M-edge segments of one arena (NOT production's format: vprod writes 16-B
words varint-coded per 64k chunk to per-(layer, worker) files, 1.19 GB);
every new state writes its 64-B row ONCE, at its insert, into its thread's
piece (one NT line; production pushes every non-cached emission's row into a
queue and gathers the new ones at the flush: here the decision is immediate,
so the push is the gather); the pos-graph edges per thread behind a
last-pair cache; and the frame's end.

**The sweep is over SOURCES.** The lookups arrive in source order (the
kernel emits per source block); a source emits targets within a few cells
(the per-cell window above), so a front over sources IS a front over targets
a few cells wide. Sweeping over targets would need design D's partition pass
(~6 GB more traffic) first. The streams (prep/q.bin, sweep/d2.bin: the drops
with their target shard, `sweep DIR prep`) are x-major by source cell, cut
into ~1024-lookup chunks at source boundaries (a source's drops and lookups
in one chunk, so its drop note has one writer). Threads, pinned one per
core, claim chunks from ONE atomic counter (`front=shared`): all threads
stay within ~T chunks of the line, so the live window (7-11 MB of states)
is one shared L3 working set. `front=ccd`: two counters over two x-bands
split at equal work, CCD0's threads (cpus 0-7, 16-23; 96 MB L3) on the left
band and CCD1's (8-15, 24-31; 32 MB) on the right, each L3 holding only its
band's window; a thread whose band is done steals from the other's front.

**Per chunk:** (1) the drops; (2) scan: the table check, the pos edge, and a
per-thread 4096-entry direct-mapped cache of recent keys (design G; 64.6% of
lookups hit it; an in-chunk repeat of a pending miss joins that miss, 9.2%),
(3) the misses (26%) go to the `Probe` (probe.rs, a trait with a batch
`resolve`, the hook for dedup-mlp's prefetching probe), (4) rows and edges.

**The table** (probe.rs): v4's (16-B slot: 64-bit fingerprint + id; load
0.8, 1.25 GB; an exploration as v4 is) or `probe=exact` (v3c's exact 16-B
keys, 24-B slot, load 0.5). Per-(shape, cell) open addressing, sized from
cnt.bin as v3c/v4 are. LOOKUPS ARE LOCK-FREE: slots only go empty -> full; a
slot is published by one Release store of its first word after the rest is
written, readers Acquire-load it. INSERTS take a per-shard spinlock (own
64-B line) and re-probe. Why a lock and not owner-only or CAS: inserts are
2.6% of lookups (6.7M), so the lock is off the hot path (10k races where the
locked re-probe found another thread's insert); owner-only would need the
lookups routed to the owner (D's pass) since chunks are claimed
dynamically; a CAS insert of an exact key needs a 16-B CAS or a
claim-then-fill protocol readers must wait on.

**Ids and determinism.** A new state gets a provisional id `n_door + thread
x 2^23 + k` (its row's place in the thread's piece). The frame's end
(`end_frame`, timed, ~0.09 s at 16 threads): per thread count rows per
shard, scatter (shard, key) into shard order, sort each shard by key, the
canonical id = `n_door +` position; the table's ids are rewritten (one owner
a shard) and a provisional -> canonical map kept for the edges (production's
`Renumber`). Canonical ids are therefore independent of the scheduling: the
fingerprint over (source, canonical target id, transfer) is `62aa7f1d56847ad4`
at 16 and 32 threads, shared and ccd fronts, v4 and exact, with and without
the cache.

**Validated** (`verify=1`, canon.rs; vprod `VPROD_VERIFY=1` prints the
same): new 6,735,699; 257,724,013 edges, all distinct; the edge SET over
(source id, target KEY, transfer) fp `3b4f8b60b8767421`, equal to vprod's;
drop notes 5,725,599, fp `5bbf9f41b591bd12`, equal to vprod's; level -1
mismatches 0; every new key's table id is its canonical id. Not compared:
the pos-graph edge set (191,284 distinct here; vprod only prints a per-worker
sum).

**Envelope.** Bytes production also moves: the table's touched lines 0.7 GB
(10.9M targets x 64 B, each once if the window stays in L3), edges 3.09 GB
(NT, no read-for-ownership), rows 0.43 GB, notes 23 MB: 4.2 GB / 45 GB/s =
**0.09 s**. The HARNESS adds its input streams, 8.25 GB (q.bin) + 2.03 GB
(d2.bin) = 10.3 GB, 0.23 s more (production's emissions are in registers):
**0.32 s floor for this bench**; `read=1` measures it. Latency: 67.7M table
probes (the cache's misses) at ~40 ns (L3, the window) / (16 threads x 1 in
flight) = 0.17 s (0.085 s at 32); the 10.9M first touches from DRAM at 100 ns
/ 16 = 0.07 s; prefetching (MLP 4-8) would divide both.

Smoke timings, NOISY (load 3-7, other agents running; best-of-1, verify
runs): vprod 16 workers 2.15 s wave + 0.10 s end_frame (in VPROD_VERIFY
mode); sweep 16 shared 0.62 + 0.09 s; 16 ccd/exact 0.47 + 0.09 s; 32 ccd
0.39 + 0.09 s; 32 shared without the cache (chunk 256) 0.56 + 0.10 s. Worker
time at 16 shared: drops 24%, scan 35%, probe 25%, emit 15%. The real
numbers: /var/tmp/emitcap/sweep/bench.sh (a copy: bench/dedup/sweep-bench.sh)
when the machine is quiet: interleaved reps of vprod 16 and sweep 16/32 x
shared/ccd, with phases, AnonHugePages, per-process perf (L2 misses, L3 /
other-CCD / DRAM fills) and system-wide UMC CAS reads+writes (DRAM bytes;
needs `sudo modprobe amd_uncore`, done 2026-10-09, not persistent).
