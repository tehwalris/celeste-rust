# Edges target: ID-free edge bundles in their target regions (as built, 2026-10-11)

Branch `edges-target` (from `develop` e7e9f78). The running log is
`PROGRESS.md` at the worktree root. Research: plans/edge-bundles.md (branch
`edge-bundles`), bench/dedup/DESIGNS.md J/J2/O/O2/O3 (branch
`edge-bundle-bench`). This replaces plans/storage-v2.md's edge side - the
source-side unit blocks, lids, the owner index, claims, the translation -
and its forward; the visited set (per (shape, region) entries with cell
masks), the layers' frame files and the metadata stay as storage v2 built
them.

## The format

**A state** is `(shape, exact key, cell)`; an edge names it so, by what it
is: `storage::Sk` = (region, key, cell in region), region = shape index x
slots + slot. No edge names a state by a number.

**A bundle** (`storage::bundle`) is the edges of one source STATE KEY
(shape and key: every cell of it) into one TARGET KEY at one transfer and
one cell OFFSET, inside one target region: `(source shape and key,
transfer, offset, target key) -> mask` over the target region's 64 cells
(8 x 8 regions: `GEOMETRY.words == 1`). An edge into target cell `c` leaves
source cell `c - offset`. Offsets are `(dx + 1024) << 11 | (dy + 1024)`;
into `NO_CELL` `TO_NONE | source cell` (the target cannot give it back);
out of it `FROM_NONE`; `NONE_NONE` for neither.

**A block** holds a target region's bundles (one segment of one frame):
its dictionaries - the distinct target keys and source (shape, key)s,
sorted and delta-coded (varint128), so records hold RANKS, not ids - then
the records as a sorted trie in `ORDER` = mask, offset, transfer, source,
target: per record a level byte (the first field that changed), that
field's delta, the later fields raw (the mask a raw u64, the offset a byte
for |d| <= 7 else a varint). At rest zstd -1.

**The edge file** (`storage::edges`, VERSION 6, magic `CSE2`): per frame
`edges/f{frame}.bin` (a raise adds `f{frame}.r{seq}.bin`); a bincode header
- per segment its region, offset, lengths, bundle and edge counts, sorted by
region and within a region by slice; per target region the SOURCE regions
its edges leave (the out-edge lookup, `EdgeStore::targets_of`) - then the
zstd blocks. A region has one segment per slice of the wave that reached it
(below), plus one for the edges into keys made final at the frame's end.
Transfers are global ids into `edges/xfer.bin`, content-canonical: each
slice appends the pairs it met first, sorted; `done.txt` records a frame's
trusted table length. Nothing else: no lids, no owner index, no source id
lists, no translation tables.

**The order, re-validated on real trees** (the temporary `rewrite
bundle-orders`, since deleted: each tree's bundles re-encoded one block per
(target region, frame), B an edge with zstd -1):

| tree | `[M o x s t]` absolute masks (stored) | `[s x o t]` masks relative to the source key's cells |
|---|---|---|
| r10 (room (1,0), f0-56) | 0.099 | 0.105 |
| r42n (room (4,2) r0sxhn) | 0.129 | 0.127 |
| r60s (room (6,0) split) | 0.249 | 0.236 |
| r62h57 (room (6,2) 100%) | 0.081 | 0.075 |

Relative masks win by 1.5-7% on three trees and lose 6% on r10, and cost the
source key's cells at every read (up to four source regions). Absolute
`[M o x s t]` stays: the 48-order census of DESIGNS O3 measured it best
among absolute orders.

## The forward (`storage::wave`, `unit`, `route`)

1. **Units** are runs of frontier rows (1024-4096 lanes, a 256th of the
   frame: a function of the frame alone). A unit's sink
   (`unit::UnitSink`) keys each emission and ORs it into its bundle in a
   unit-local table (`route::UnitBundles`: one probe and an OR, in cache),
   keyed by the target's ROUTE (shape, region slot), target key, source
   (the unit's index of its source key), transfer (the worker's local
   id) and offset. Nothing is looked up in the visited set and no row is
   copied. The decode guard (a shape's first row each worker and wave is
   what its key and cell decode to) and level -1 / the ladder's verdicts
   happen here. At the unit's end each route's bucket is a CHUNK: the
   unit's distinct targets (key, cells), its sources, the bundles naming
   both by index (24 B each).
2. **Slices** of ~1M input lanes cut the units. When a slice's last unit
   ends, that worker GATHERS the complete slices in order: their transfers
   (each worker's pairs met first in the slice, `upto`, merged into the
   global table - the new pairs only, sorted; each worker gets its local
   ids' global ones), the new routes (each takes its region's table out of
   the visited set for the frame), and the chunks as JOBS, one per (route,
   slice), heaviest first.
3. **The pipeline**: one thread scope a frame. A worker takes a region job
   first, else the next unit - at most one slice past the last gathered -
   so the region phase of slice k runs alongside the kernels of slice
   k + 1, and the records in flight stay bounded (~1 GB a slice in room
   (6,2) 100%).
4. **The region phase** (`route::run_route`), a job at a time per route:
   per chunk target ONE probe of the region's table and an OR (the cells
   the entry lacked are the new states); per bundle its target named by
   the table's entry and its source by the job's source index; the job's
   bundles encoded (`bundle::Encoder`: dense target ranks, records grouped
   by (mask, offset, transfer) instead of sorted - byte-identical to the
   generic encoder) into one zstd segment. A key's bundles from two units
   stay two (their cells differ): a route-level bundle table merged ~17% of
   the bundles (0.101 against 0.113 B an edge at f57) but was DRAM-bound.
5. **The frame's end**: provisional keys made final (the frame's new codes
   sorted), new shapes numbered in hash order, per route its new entries
   renumbered in key order (`RegionTable::finish_frame`: only the new
   index slots move), its edges into provisional targets encoded with the
   final keys, the tables back into the visited set, the edge file
   written, and the new LAYER decoded from its states' keys and cells
   (`decode::Skeletons`: a shape's skeleton registered by its first
   emission; a mismatch is fatal).

Everything that names anything is canonical: entry numbers (key order),
transfer ids (a function of the slices, which are a function of the
frame: the unit size is too, not of the threads), segments (a function of
their (route, slice)'s chunks, in unit order; a route's segments filed in
slice order, whichever job ran first). Room (1,0) r0sxh f0-60 and r0sx
f0-56 (two slices a frame from f50): 3 against 32 threads and a resume
give byte-identical frame files, edge files and `xfer.bin`; `gates/raise.sh
1,0 55 56 58` passes.

## The backward and the search (`search::arc_dp`)

- **Nodes are states** (`Sk`), sorted region-major as the files are read;
  the winning sets' file (`Winning`) codes each node's `Sk` as a delta
  from the previous one (`put_sk`).
- **A step reads in-edges by target region**: the changed nodes grouped
  by region; per (region, frame) item - frames from the group's least
  layer to t + 1 - the region's segments decoded, each block's target
  dictionary ranked against the group's changed targets (both sorted), and
  every record's mask ANDed with its target's changed cells; a hit is an
  in-edge (source = the record's source key at target cell - offset).
- **Reach** (the next level's filter) and the concrete search's successors
  load out-edges as states are reached: a frame's sources by region, the
  file header's index names the target regions those regions' edges reach,
  their segments are decoded and the bundles of the sources' keys kept
  (`out_edges`).
- **The region-batched worklist** (plans/edge-bundles.md's proposal: drive
  the step by region so a block is decoded once for all of its changed
  targets) is what the reader does already: items are (region, frame), so
  each block a step needs is decoded once for every changed target in it.
  What remains is the decode AMPLIFICATION across steps (below).

## Numbers

(See BENCHMARK_DATA.md 2026-10-11 for the runs.)

## What still carries state ids (and the proposal)

`storage::StateId` = (region, entry, cell in region) remains in: the
layers' frame files (an id column, 8 B a state), the per-frame metadata
(the entries created, with keys: the Resolver), the level -1 drop notes
(sorted (id, horizon), 9.1 B a dropped source), a raise's per-state layers,
and the diagnostics that name states (`follow`, `spurious`, the UI export).
On r62h57 (942 MB): frames 403 MB (the id pieces 351, the metadata 52),
edges 136 MB, drop notes 398 MB. The proposal for Philippe (not built):
store a layer as per (shape, region) its new entries' keys and cell masks
(the visited set's own form, ~0.5 B a state raw, ~0.06 with zstd) and the
drop notes as per (region, key) masks with their horizons; an id becomes a
derived (region, entry, cell) of the visited set, never stored.
