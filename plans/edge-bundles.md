# Edge bundles: edges stored the way states are (2026-10-11)

Philippe's idea. States are compact because the same state exists at many
nearby positions: per (shape, 8x8 region) an entry is a position-free key and
a u64 cell mask. Edges are not: each is stored alone, `(lid, cell, source
lane, transfer rank)` in its unit's block, ~3.1 B an edge, and edges are
85-95% of a format-14 tree (plans/rows-from-keys.md). An edge is (source
state, target state, transfer); a BUNDLE is one source ENTRY's edges to one
target at one transfer and one cell OFFSET (target cell minus source cell),
with a mask of the source cells it holds at. Prior research on a capture
(`bench/dedup/DESIGNS.md` K1/M/N on `emit-capture`) found 257.7M edges ->
35.3M bundles, 98% with a uniform shift, ~1.76 B an edge raw and ~0.18 with
zstd -1. This re-measures it on real trees in the current format (exact keys,
storage v2 edge files), across frames and rooms, and asks what the readers
would do with it.

## The tool

`rewrite bundle-census --level-dir D [--from F] --to T [--marked H
[--w-share]] [--time-reps N]` (`storage::bundles`), reading the tree's edge
files with `storage::edges::EdgeStore`. Bundles are formed PER RECORDED UNIT,
as a forward would form them at a unit's end. Two namings of the target:

- **A, target ENTRY** (the unit's lid = a `(region, entry)`): a source mask
  whose shifted cells fall into two target regions is two bundles;
- **B, target KEY** (shape and position-free key, any region): the A
  bundles of one (source entry, key, transfer, offset) merged.

Bundle size = its mask's popcount = its edges. The A bundles are encoded per
unit block, source-major (S: source entry, then lid) and target-major (T:
lid, then source entry - today's block order), as varints and as
bit-packed fixed width, each also with zstd -1; the built blocks are
re-encoded from the edges read and compared with the stored ones (byte for
byte equal on r10, r62h, r62h57; r42n and r60s differ only by transfer-rank
tie-breaks). `--marked H` keeps only the edges between the remainder-free
BFS's marks (what `arc_dp::load` keeps); `--w-share` then runs the
backward and counts how many distinct winning sets an entry's cells hold;
`--time-reps` times the unit's END both ways on each unit's real edges (and
checks the timed grouping makes exactly the census's bundles).

**Data**: the fixtures in `/var/tmp/ia-fixtures` (r10 room (1,0) r0sxh f1-56;
r42n room (4,2) r0sxhn level -1 f1-61; r60s room (6,0) r0sxhfp split steps
1-71; r62h room (6,2) 100% r0sxhf level -1 f1-51; r62h57 its frames 52-57,
the reference frame f57; r10arc the h56 synthetic-win search). They were
converted to FORMAT 14 in place at 12:23 today (another agent's
`convert-tree`), so this branch is based on `origin/convert-tree` (format
14), not develop (13). The edge files are version 5 in both.

The census runs in 0.3-18 s a fixture at 16 threads (r62h57 f52-57, 1.05G
edges: 18 s); everything below is from one run each (counts are exact; only
the timings move with the machine's load).

## 1. Bundle counts

| fixture | edges | A bundles | edges a bundle (median, p90; edge-weighted median, p90) | single-cell (bundles / edges) | B bundles | edges a bundle (median, p90; e-w median, p90) | single-cell |
|---|---|---|---|---|---|---|---|
| r10 | 148.0M | 39.39M | 3.76 (2, 8; 6, 30) | 38.8% / 10.3% | 29.78M | 4.97 (2, 9; 8, 48) | 33.1% / 6.7% |
| r42n | 67.9M | 15.73M | 4.31 (2, 9; 7, 24) | 26.5% / 6.1% | 11.81M | 5.75 (3, 13; 10, 32) | 22.3% / 3.9% |
| r60s | 10.1M | 1.62M | 6.25 (4, 16; 9, 32) | 19.9% / 3.2% | 1.37M | 7.41 (5, 16; 12, 32) | 17.2% / 2.3% |
| r62h | 430.9M | 76.40M | 5.64 (3, 12; 9, 30) | 20.8% / 3.7% | 51.86M | 8.31 (4, 21; 16, 44) | 17.8% / 2.1% |
| r62h57 f52-57 | 1048.2M | 195.03M | 5.37 (3, 12; 9, 28) | 22.3% / 4.1% | 134.11M | 7.82 (4, 20; 16, 40) | 19.0% / 2.4% |
| r62h57 f57 | 257.7M | 50.35M | 5.12 | 23.3% | 34.78M | 7.41 | |

Per frame (room (6,2)): edges a bundle (A / B) rise from ~1 at the spawn to
5.95 / 8.93 at f50, then fall slowly as the room fills: f52 5.82 / 8.57, f54
5.55 / 8.12, f57 5.12 / 7.41. K1's capture of the same f57 (hash keys,
grouped over the whole frame, not per unit): 35.3M B-type bundles, 7.30
edges each, 21.6% single-cell - the same as B here (34.78M, 7.41): exact
keys and per-unit grouping change nothing.

**Which naming the sharing depends on**: B merges 1.19-1.47 A bundles
(r62h57: 1.45; B bundles spanning 1 / 2 / 3 / 4 target regions: 86.3M /
40.7M / 1.0M / 6.1M). A source mask shifted by an offset falls into one
target region only ~64% of the time; a third of the sharing needs the target
named position-free. Masks are mostly small: half the bundles hold 2-4 cells,
but most EDGES are in bundles of 6-16 (edge-weighted medians).

## 2. Variants

| fixture | one offset per (src entry, target entry, transfer): groups (edges) | transfer out of the key: (src entry, target, offset) / A; one transfer (edges) | one transfer per (src entry, offset), edges; H(transfer \| src entry, offset) | "the whole entry moves together": entries (edges) | per-entry classes: items / A bundles |
|---|---|---|---|---|---|
| r10 | 94.7% (94.1%) | 0.80; 78.1% | 70.4%; 0.278 bits | 53.4% (27.8%) | 0.88 |
| r42n | 99.7% (99.6%) | 0.96; 96.0% | 93.0%; 0.062 | 77.2% (68.8%) | 0.77 |
| r60s | 88.5% (83.3%) | 0.96; 94.6% | 92.0%; 0.086 | 88.3% (80.5%) | 0.84 |
| r62h | 99.2% (99.4%) | 0.94; 95.9% | 94.3%; 0.056 | 79.2% (74.8%) | 0.77 |
| r62h57 f52-57 | 99.0% (99.1%) | 0.93; 94.6% | 92.2%; 0.074 | 74.7% (71.0%) | 0.81 |

(K1, f57: 98.3% one shift, 0.07 bits; here 99.0% and 0.085 at f57.)

- **Drop the transfer from the key, store it per (source entry, offset) when
  unique**: unique for 70-94% of the edges; the transfer-free bundles are
  0.80-0.96 of A. The transfer is ~1 B a bundle (0.17-0.38 B an edge raw, a
  quarter of a bundle's bytes; it is what zstd removes best). A side table
  per (source entry, offset) plus exceptions saves at most that, raw only.
  Not worth a second structure.
- **Key by (source entry, offset) only, listing targets**: 0.42M-8.47M
  headers (25-124 edges each); split into cell classes with equal (lid, transfer) lists, the
  classes LIST MORE items than there are A bundles (r62h57 206M against
  195M): a list per class repeats targets the masks share. Worse than A.
- **The whole entry moves together** (every cell of the entry that has edges
  has one (target key, transfer, offset) set): 53-88% of the multi-cell
  entries, 28-81% of their edges; 1.17-1.90 classes an entry. A layout of
  per-entry classes (a mask and a list of (key, transfer, offset)) lists
  0.77-0.88 of the A bundles' items but more than B's (r62h57 158.5M against
  134.1M; r60s a tie, 1.364M against 1.366M), and needs a mask per class
  plus the list. B dominates it.

So the best counts are B (target key) bundles, 7.4-8.3 edges each in the
big rooms, and A (target entry) 5.1-6.3; no variant beats B.

## 3. Bytes per edge

The A bundles per unit block (record: group field delta, other field,
transfer rank, offset byte, mask). Relative masks: a bundle's cells as bits
over its source entry's cells IN THE UNIT (`k` of them, from the unit's
source ids: `ceil(k/8)` bytes, none when `k` is 1) - the frontier entries
have 7-10 cells in a unit, so most masks are one byte.

| fixture | built on disk | built block | built block zstd -1 | S varint | S zstd | T varint | T zstd | S, u64 masks (zstd) | fixed width, u64 masks (zstd) |
|---|---|---|---|---|---|---|---|---|---|
| r10 | 2.96 | 2.56 | 0.47 | 1.62 | 0.21 | 1.62 | 0.19 | 3.31 (0.25) | 3.24 (0.87) |
| r42n | 3.06 | 2.61 | 0.57 | 1.34 | 0.22 | 1.34 | 0.20 | 2.88 (0.25) | 2.84 (0.77) |
| r60s | 3.71 | 2.78 | 0.75 | 0.89 | 0.25 | 0.89 | 0.24 | 1.94 (0.27) | 1.82 (0.66) |
| r62h | 3.05 | 2.78 | 0.43 | 1.14 | 0.15 | 1.14 | 0.12 | 2.24 (0.17) | 2.23 (0.59) |
| r62h57 f52-57 | 3.13 | 2.88 | 0.45 | 1.22 | 0.16 | 1.22 | 0.13 | 2.37 (0.18) | 2.36 (0.63) |

By field (r62h57, T): lid 0.19, source entry 0.19, transfer 0.32, offset
0.19, mask 0.34 B an edge - about 6.6 B a bundle. Fixed width (random
access) costs 2x the varints, mostly the u64 mask; the varint blocks are
decoded a lid at a time anyway.

**The tables and the reverse walk's index.** The built owner index is 16 B
a lid (0.18-0.70 B an edge); delta-coded by owner (as census.rs
`index_bytes`) it is 0.06-0.19. In T order a target's bundles are its lid's
run, as today: per-lid starts as varint lengths 0.012-0.044 B an edge. In S
order the reverse walk needs per lid its bundles' ranks: 0.24-0.35 B an edge,
which is why S loses.

| fixture | built total (block, starts, ranks, sources, owner index delta-coded) | T total | T total, zstd blocks | states (format 14 frames/, ids + headers + meta) | edges a state | edges B a state: on disk / T / T zstd |
|---|---|---|---|---|---|---|
| r10 | 2.75 | 1.76 | 0.33 | 9.7 B | 32.2 | 95 / 57 / 11 |
| r42n | 2.82 | 1.50 | 0.35 | 10.0 B | 22.0 | 67 / 33 / 7.7 |
| r60s | 3.19 | 1.18 | 0.52 | 11.1 B | 5.4 | 20 / 6.3 / 2.8 |
| r62h | 2.91 | 1.23 | 0.21 | 9.3 B | 28.8 | 88 / 36 / 6.2 |
| r62h57 f52-57 | 3.01 | 1.31 | 0.23 | 9.2 B | 36.6 | 115 / 48 / 8.2 |

So bundles take edges from ~3.1 to 1.2-1.8 B an edge raw (2.3x on the big
room) and 0.21-0.52 with zstd -1 per unit block (13x on the big room):
edges go from 10x the states' bytes to about the states' bytes. zstd -1 on
TODAY's blocks alone gets 0.43-0.75 for the block (plus 0.25-0.9 of
tables): the bundling and zstd compound, and bundles are what make the raw
form small enough to keep uncompressed where a reader needs random access.

## 4. Access patterns

| reader | today | with T-order A bundles |
|---|---|---|
| forward | writes per unit: counting sort by (lid, cell), `encode_block`, owner index | the same unit end with the sort keyed (lid; source entry, transfer rank, offset; cell): section 5 |
| BFS (`edges::bfs`, `preds_at`) | owner index -> (unit, lid) -> the lid's edges at the target's cell | unchanged in structure: the lid's bundles, each holding at most one source for the target cell (`c - offset` in the mask). Better: a whole target ENTRY's frontier cells at once - per bundle the sources are `mask AND shift(T, -offset)` (8x8 bitboard shifts), the marks are already masks per entry |
| `arc_dp::load` | streams units, builds a CSR of 12 B per kept edge (+16 B a node) | streams units the same way; keeps bundles instead (below) |
| arc-check | samples records | expands the sampled bundle's cells |
| `ckhash --edges`, census, `edge-census` | scan every edge | the scan expands bundles; the fingerprint stays over edges |

With zstd per unit block a probe decompresses a whole unit block (~0.1-0.3
MB raw at f57): the BFS would batch its targets per (frame, unit) and
decompress each needed block once per iteration (r62h57: 1.3 GB raw for
f52-57, ~0.1 s a full pass at 16 threads); the graph load and the scans
stream anyway.

**The in-RAM graph** (`--marked 56 --w-share` on r10arc, h56 synthetic
win): 337,145 nodes in 30,395 entries, 4,136,746 kept edges.

| | MB | against the CSR |
|---|---|---|
| `arc_dp` CSR: 12 B an edge + 16 B a node | 55.0 | 1x |
| per-unit A bundles (1.08M, 3.83 edges each): 24 B a bundle (target entry, transfer, offset, mask; its place in the target's list) + 24 B a marked entry | 26.7 | 2.06x smaller |
| graph-level A bundles (an entry's cells across ALL layers: 781k, 5.29 edges each) | ~19.5 | 2.8x smaller |

The graph is frame-independent, so the in-RAM bundles group a source entry's
cells from every layer (the files only see one layer's): more edges a
bundle. For room (2,3) gemskip h137 (373M edges, a 4.5 GB CSR) that would
be ~1.6-2.2 GB.

**W per (entry, cell) with masks** (the backward's sets, the same run): per
frame the cells of an entry hold 0.725 distinct winning sets per node with a
set (427,136 (node, t), 309,582 distinct (entry, set)), and along a bundle
the distinct target sets are 0.667 of the per-edge pulls. So a backward on
bundles would pull ~1.5x less where it pulls once per distinct target set,
and sets-with-masks would hold ~27% fewer set references: the win is the
CSR's RAM, not the DP's work.

## 5. Producing bundles in the forward

**The frontier is in (region, entry, cell) order**: a layer is stored and
gathered in id order (`region << 38 | entry << 8 | local`) and units are runs
of its rows, so a unit's lanes of one entry are contiguous. Measured: 0 of
16,920 units have sources out of id order; a unit boundary cuts about one
entry (r62h57: 5,393 of 2.34M source entries, 0.23%; r10 0.5%), which then
forms two half-bundles. The kernels emit per call (a slice of <= 16 lanes of
one shape and kernel region), per body, per lane: a lane's emissions are
NOT contiguous, but every one of the unit's emissions is in hand at
`UnitSink::end`, which already sorts them all.

**The grouping cost** (`--time-reps 3`, each unit's real edges, the minimum
of 3 reps per unit summed, 16 threads in parallel; inputs in two orders, by
source lane and shuffled, bracketing the kernels' slice-body-lane order;
load 7-10):

| | built end (counting sort by (lid, cell), segment sorts, `encode_block`) | bundles, S order | bundles, T order |
|---|---|---|---|
| r10 f50-56 (88M edges) | 10.7 / 14.9 ns an edge | 13.9 / 14.5 | 12.6 / 14.3 |
| r42n f55-61 (57M) | 10.8 / 15.3 | 13.5 / 14.1 | 12.4 / 14.1 |
| r62h57 f57 (258M) | 13.0 / 18.6 | 15.0 / 16.8 | 14.8 / 17.0 |

(an earlier run at lower load: f57 built 10.2 / 15.3, S 12.5 / 13.9.) The
bundled end is a counting sort by the group field (lid for T), then per
group a sort of u64 keys `source entry | transfer rank | local dx, dy |
relative cell` (the regions are fixed by entry and lid, so the local deltas
ARE the offset), OR-ing runs into masks, and the encoding. It costs the same
as the built end within +-2 ns an edge. The built end in the real forward
(`bench-frame --phases`, r62h57 f56 -> f57): "unit end (sort, encode)"
3.4-3.6 of 40-44 worker-s (8-9%), 13.2-14.0 ns an edge - so bundling moves a
frame by at most +-0.5 worker-s, ~1%. The old research's 7-18 ns an edge
(M2, N3) was ON TOP of the dedup: there the grouping was a pass of its own
over a stream (sorts of u128 keys, hash tables that did not stay in cache).
Here the unit end sorts every edge already; bundling only changes the key,
and the groups (at f57 ~88 edges a lid, ~450 a source entry) sort in
L1/L2.

## Recommended layout

**Edge file v6: T-order A bundles per unit block, zstd -1 per block.**

- Per unit, as today: its sources (a row range of the previous layer's
  frame file, or explicit ids), its lids (owners in the owner index), its
  transfer ranks.
- The block: lid by lid (today's order: the reverse walk is unchanged), per
  lid its bundles sorted (source entry, transfer rank, offset): a group
  delta (0 = the same lid), the source entry (the unit's rank of it, delta
  within the lid), the transfer rank, the offset byte (`(dx+8)<<4|(dy+8)`,
  0 then a varint beyond +-7 or for `NO_CELL`), the mask relative to the
  entry's cells in the unit (none for a one-cell entry).
- Per-lid starts as varint lengths; the owner index delta-coded by owner.
- zstd -1 per unit block where the tree is at rest; uncompressed (1.2-1.8 B
  an edge, 2.3x below today) if the BFS's random probes need it.

Expected: edges 3.0-3.7 -> 1.2-1.8 B raw, 0.21-0.52 with zstd; a tree (now
85-95% edges) ~2x smaller raw, ~5x with zstd (r62h57 f52-57: 3.28 GB of
edge files -> ~1.38 GB raw, ~0.24 GB zstd).

Keep the target an ENTRY (lid), not a key (B): B has 30% fewer bundles,
but each needs its target region resolved per cell at read (the offset and
the cell give the region, then a key lookup there), the owner index and the
reverse walk would change, and with zstd the difference is small. The
transfer stays in the key (section 2). The forward's cost is unchanged
(section 5).

**Follow-ups, in order**: (1) build it: `UnitSink::end` bundles, `EdgeStore`
decodes them (`preds_at`, `scan`, `UnitView::edges` expand), edge file
version 6, every fixture re-pinned on identical `ckhash --edges`; (2)
`arc_dp::load` keeps bundles (2-2.8x less RAM for the graph), the backward
expanding them per source entry; (3) a mask-parallel BFS over target
entries; (4) zstd per block with a per-iteration block cache in the BFS.
