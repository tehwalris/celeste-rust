# Storage v2: posmask regions, region ids, source-side edges (2026-10-10)

The plan for replacing the door, the queues, the unit row cache, the raw
edge records and the inversion with ONE storage system; the running log is
`PROGRESS.md` at the worktree root (branch `storage-v2`, from `fg-2300`
cc83334). Research: branch `emit-capture`, `bench/dedup/DESIGNS.md` A-N3.

## What it replaces, consumer by consumer

| today | where | becomes |
|---|---|---|
| `search::door::Door` (sorted shards per (shape, cell), 24 B + id a state) | `frame.rs` flush, `ForwardState`, resume, raise, `bench-frame`, `rerun` | `storage::Visited`: per (shape, region) a table of ENTRIES (non-position key -> entry number) with a cell mask each |
| `ForwardSink` queues, pool, `Slot` pred masks/extras, `RowCache`, `direct_edge` | `frame.rs`, `asm_kernel::run_slice`, `refengine` | `storage::UnitSink`: per emission one probe of a unit-local target table; requests for states the visited set lacks; rows copied only for requests |
| `pack_id(layer, seq, row)` ids | everywhere | `storage::StateId` = (region, entry, cell); a layer's files are sorted by it, rows carry it in an id column |
| `canon` (flush ids -> canonical, `Renumber`) | wave end, door, raw records | gone: entry numbers are canonical when assigned (translation sorts requests by key); `canon::gather` kept for building the layer's blocks |
| raw edge records (`edges/raw`), `edges::invert`, runs v5, `compact_raised`, `reopen_raised` | forward, `EdgeGraph::open`, raise | `storage::edges`: per frame ONE file of unit blocks (source-side), each sorted by target, with its translation table; raised frames add files |
| `EdgeGraph` (`preds_at`, `records_at`, `pair`) | `edges::bfs`, `arc_dp::load`, `arc-check`, `ckhash --edges`, `spurious`, `edge-census` | `storage::edges::EdgeStore` with the same queries (reverse walk through the translation tables) |
| `edges::Marks`, `MarkRanks` (bitmaps per (layer, seq)) | `bfs`, `arc_dp::load` | per region a mark mask per entry; ranks by prefix popcounts |
| `edges::resolve_ids` (ids -> (shape, key, cell) through frame files) | `arc_dp::solve` (gate fingerprints, marks files, node keys) | `storage::Resolver`: region -> (shape, region), entry -> key, from the per-frame storage metadata |
| per-worker transfer tables merged per frame, then globally in `arc_dp::load` | forward, inversion, `arc_dp` | ONE global table, content-canonical: frame f appends its new pairs sorted; ids stable for the tree |
| `DropNotes` keyed by 64-lane id groups | level -1 notes | per frontier row (block, lane), written as (StateId, horizon) sorted |
| `checkpoint` v11: rows in (region16, cell, key) order with a cell run index | everywhere | v12: rows in StateId order with an id column and a cell column (`cell_counts`, `rows_of_cell` derived) |
| `compact-bench` | rewrite | deleted (nothing to compact); `bench-storage` replaces it |

## The design

**Cells and regions.** A cell is today's `pos_graph` cell: the position
object's (player, else `player_spawn`) whole-pixel x/y plus the room offset,
on the 512 x 512 grid from -64; `NO_CELL` without one. A storage REGION is
an S x S square of cells, `S = CELESTE_STORAGE_REGION` (8 default, or 16),
aligned at 0 by `div_euclid` exactly as the kernels' `RegionGrid::of` (the
grid origin -64 is a multiple of both). Every storage region nests inside
one kernel region: the kernels' px (`CELESTE_REGION`, default 16) must be a
multiple of S, else the forward refuses to start. The mask over a region's
cells is S*S bits: one u64 at 8, four at 16. `NO_CELL` is one extra region
per shape with one cell. Region index = shape index * (grid regions + 1) +
slot (row-major over the grid, `NO_CELL` last), shape indices assigned per
frame to new shapes in hash order: id order is (shape, y, x) - spatial -
and a region's index needs no table but the shapes'. At 16 we would expect
(DESIGNS J2, N): ~2x the states per entry (39 against 20), half the
entries (1.12M against 2.19M at f57), 82% of edges inside their source
region against 67%, 0.48M cross-region translations against 1.25M, but
masks of 32 B instead of 8 B and twice the batch per region (the dedup was
DRAM-bound at both; 16x16 had worse critical-path tails without splits).
Measured, not assumed, before changing the default.

**Keys: position-free.** The row key stays a 128-bit sum of per-cell
mixes, but the position object's x and y contribute only their FRACTIONAL
part relative to the low end's whole pixel (`frac(n) = n - flr(n)`, an
interval `[a, b]` as `[a - flr(a), b - flr(a)]`): integers, the normal
case, all code alike, so the key is the state without its position. The
state is still `(shape, key, cell)`: `flr(lo x)` is the cell's x minus the
room offset, and the room is in the key, so the map from today's key is a
bijection (no merge, no split) - checked, not assumed (below). Position
fields that are not numeric while the object exists are FATAL in the key
(they would make the cell `NO_CELL` and the fraction meaningless). The
kernels' `KeyField` gets a position read; `KEY_CHECK` compares against
`Rt2::boundary`, which applies the same rule.

Collision exposure: one 128-bit hash compared within a table, as today.
Today a collision must happen between two distinct states of one (shape,
cell); now between two distinct non-position tuples of one (shape, region)
table. The per-pair probability is the same (2^-128, same width, same
mixing); the pairs compared are sum over regions of E_R^2 (E_R entries)
against sum over cells of N_c^2 (N_c states) - smaller wherever states
share their non-position fields across a region's cells (on room (6,2)
f57 entries are 2.19M for 43.8M states, ~20 cells an entry: ~8x fewer
pairs, to be confirmed on the built set and logged). The worst case (no
sharing) is S^2 times more pairs; both are ~1e-27 at 1e11 states. A
collision that IS detectable is fatal: two requests of one (entry, cell)
carrying different rows (checked under `CELESTE_KERNEL_KEY_CHECK=1`), an
entry whose stored key disagrees with a row's (resume), a row whose key
the boundary recomputes differently (`KEY_CHECK`).

**Ids.** `StateId(region, entry, cell)` in a u64: region 24 bits, entry 32,
cell-in-region 8. Entry numbers are per region, dense, assigned at the
TRANSLATION (below) in key order among the frame's new entries, so they
are canonical when assigned - independent of threads, units and splits. A
new state inside an existing entry keeps the entry's number. A layer is
stored in id order; a state's LAYER (first frame) is where its row is, and
the backward learns it for free: an edge recorded at frame f leaves a state
of layer f - 1.

**The forward wave.** The frontier (the previous layer, id order) is cut
into UNITS of contiguous rows (as today: 1024-4096 lanes, capped at 8192),
taken heaviest first by `threads()` workers. Per emission the unit:
1. drops level -1's rows at emission (as today) and notes their sources;
2. keys the row (position-free) and decodes its transfer (worker-local
   id, interned with the raw-words cache, as today);
3. probes its TARGET TABLE, keyed by (target shape, target region slot,
   key): a lid (local id) per distinct target entry. A new lid looks the
   entry up ONCE in the visited set (read-only during the wave) and caches
   the owner's entry number and its cell mask;
4. an emission into a cell the owner mask holds is an OLD state: edge only.
   Otherwise it is a REQUEST unless this unit requested that cell already:
   the row is copied (the only row copy) into the unit's row buffer of the
   shape;
5. records the edge (lid, cell, source lane in the unit, transfer) in the
   unit's buffer, and the pos-graph pair as today;
6. under the objects ladder, the coarser marks filter each (lid, cell) once
   per unit (the row materialized for the projection); a disallowed target
   gets no edge and no request, as today's flush filter.
At the unit's end its edges are sorted by (lid, cell, source, transfer),
deduplicated and encoded into the unit's BLOCK (bytes, final: lids are
names, only their owners are pending).

The TRANSLATION, after every unit: requests grouped by target region
(new shapes and regions numbered first, in hash and slot order), each
region on one worker: sorted by (key, cell, unit, row), each distinct key
found or appended as a new entry (key order: canonical), each distinct
(entry, cell) a new state (its first request's row) unless already set.
Every lid of every unit gets its owner (region, entry). The visited set is
written only here, one worker per region: no locks during the wave.

The frame's END: the new layer gathered from the row buffers in id order
(`canon::gather`), checkpointed with ids, cells, keys, wins; the frame's
edge file written (units' blocks, their lid tables sorted by owner, the
transfer remaps); the storage metadata (new shapes, new entries with their
keys); the drop notes; the pos graph; then `done.txt`.

Wrapped against halo: a lid is (owner region, key) - a neighbour's state
is named in the source unit with its cell in the OWNER's coordinates (the
"wrapped" layout: same local offset, the region delta in the lid). The
halo variant (one lid per key over a 16x16 window, overflow beyond) splits
one lid across up to four owners, so its translation is per (lid, quadrant).
Built: wrapped. Compared with N2's halo numbers on the same capture
(`bench-storage`), reported in PROGRESS.md with both numbers.

**The edge file** `edges/f{frame}.bin` (raised frames add
`f{frame}.r{n}.bin`): a header; per unit its worker, its sources (the
frontier rows it ran, in lane order: a row range of the previous layer's
frame file when the unit's block was whole, explicit ids otherwise), its
block (edges lid by lid with per-lid starts: a cell byte, varint source
deltas, the transfer's rank in the unit) and its ranks' global transfer
ids; per file the OWNER INDEX `(region, entry, unit, lid)` sorted - every
unit's translation table at once, the reverse walk; a unit's owners by
lid are derived from it when read (`EdgeStore::owners`, per frame). The
transfers: one table for the tree, `edges/xfer.bin` (each wave appends its
new pairs sorted; a pair's global id is its position).

**The backward.** `storage::edges::EdgeStore` answers:
- `preds_at(target, frame)`: the owner index's entries of the target's
  (region, entry) (a binary search), each a (unit, lid); in that unit's
  block the lid's edges at the target's cell: sources and global
  transfers. This IS the reverse walk through the translation
  tables; no inversion exists.
- `scan(frame)`: every edge of the frame, block by block (arc-check, ckhash
  `--edges`, the edge census, and the streaming graph load below).
The BFS (`edges::bfs`) runs as today on `preds_at`, its marks a mask per
entry per region, ranks by prefix popcounts; a mark carries its layer
(seeds from the frame files, predecessors from the frame of their edge).
`arc_dp::load` builds the same dense CSR; then (phase 2) by STREAMING each
frame's blocks once per pass, per source unit (pull order), instead of a
probe per marked node and frame. `backward`, `reach`, `optimum`, the
concrete search and `known` are unchanged but for the id type.
`resolve_ids` becomes `Resolver` (region -> shape and region; entry -> key)
over the storage metadata: no frame-file scan.

**Resume.** Trusted frames up to `done.txt`; later frames' files go. The
visited set is rebuilt from the storage metadata (shapes, entries, keys)
and the frame files' id columns (the masks); the frontier is the last
layer. A RAISE builds, beside it, each state's layer (u16 per state) to
refuse a hit from a later layer as today; new states join the frame as
extra pieces with new entry numbers; edges go to a raised edge file.

**What is kept as it is.** Level -1 (drops at emission, notes per source),
the pos graph, `CELESTE_SPLIT_FRAME` (steps), `CELESTE_TRIM_ROWS` (keeps
keys, cells, ids and wins), the marks files `(shape, cell, key, dist)`,
`MarkFilter`, `NodeKeys`, the concrete search, `export-ui` (reads
headers: cell counts now from the cell column), `follow`, `check-known`,
`ref-check`, `rerun-row` (its `--row L:S:R` is still a file position).

**Microbenchmarks kept.** `CELESTE_EMIT_CAPTURE=DIR` in the real forward
writes one frame's units and emissions (target shape, cell, key, source
lane, transfer content) - post level -1 - plus the run's env; `rewrite
bench-storage --capture DIR --tree TREE [--threads N] [--phase
units|translate|all] [--reps R]` replays them through the REAL `UnitSink`,
translation and frame end against the tree's visited set (resumed), with
per-phase wall times, validation counts (requests, new states, edges, a
content fingerprint of the edge set) and the phases as `perf`-friendly
named functions. `rewrite bench-frame` stays (one real frame, kernels
included).

## Phases (each committed and pushed; PROGRESS.md logs every milestone)

1. **1a, position-free keys** on today's pipeline: `Rt2::boundary`, the
   kernels' key fields, `KEY_CHECK`; the unit row cache keyed by (key,
   cell). Verify: per-frame kept counts identical to fg-2300 (room (1,0)
   to f62, room (6,2) 100% to f57), the pos graph identical, the arc gate's
   marks count, W node counts at every frame, optimum and witness
   identical; and that the SETS are the same, not just their sizes: a
   one-off check recomputing every stored row's OLD key reproduces
   `gates/ckhash_room10_f000-044.txt` exactly. Then re-pin the ckhash and
   arc gates (the keys moved, nothing else), saying so in the commit.
2. **1b, storage v2** (one step: ids couple the door, the rows, the edges
   and the backward, so no consumer can keep `pack_id` while the visited
   set hands out region ids): `storage::{Visited, UnitSink, translation,
   edges, Resolver}`, checkpoint v12, the backward on `EdgeStore`, every
   consumer of the table above; the door, queues, `RowCache`, raw records,
   inversion, runs and `canon::Renumber` deleted in the same step (no
   parallel implementation). Verify: kept counts and pos graph identical to
   1a (room (1,0) f62, (6,2) f57); `ckhash` identical to 1a's re-pinned
   gate; the arc gate identical (marks, W, optimum, witness) - the ids
   changed, the fingerprints are over (shape, key, cell); `KEY_CHECK`;
   `arc-check`; `--prefer` the known route; the full suite and the ignored
   tests; determinism at 3 and 32 threads and over a resume; `gates/raise.sh`.
3. **2, the backward on the layout and the speed**: the streaming CSR load;
   `bench-storage` against vprod/N2 on the (6,2) f57 capture; halo against
   wrapped; real searches end to end (room (1,0) `--ceiling 99`; an object
   room from plans/results.md) with identical optima and witnesses.
4. **3, cleanup and measurement**: anything left dead goes; the three
   oracles, ref-check and arc-check at an object level; before/after on the
   reference frame and full (1,0) and (6,2) searches (wave, phases, peak
   memory, tree on disk), interleaved on a quiet machine; architecture.md
   and BENCHMARK_DATA.md.

## Risks

- **The translation's critical path**: all inserts in one pass per region;
  the heaviest region at f57 has ~1.5M states. N3 measured 0.06 s for 6.1M
  requests at 16 threads; ours carries every request (own-region too).
- **Unit target tables** spill L2 for large units (~100k lids of ~56 B);
  units are capped, and lids touched in emission order are local.
- **Edge-file size**: the lid tables add ~12-20 B per lid (a few % of the
  edges); measured, compressed if it matters.
- **The BFS's probes**: a target's in-edges sit in the units of its region
  and its neighbours (1-9 probes a frame against 1 today); measured on the
  arc gate and a big room; the streaming load removes them from the CSR.
- **Raise**: a per-state layer index (2 B a state) only while raising.
- **Collisions** are undetectable in general (as today); the detectable
  ones are fatal (above).

## As built (2026-10-10, branch `storage-v2`)

Where the code differs from the design above, and why:

- **Units are runs of frontier rows**, not source regions: the frontier is
  in id order (shape, then position), so a unit of 1024-4096 rows is a few
  neighbouring regions, and a heavy region is simply several units. No unit
  writes the visited set (it is read-only during the wave), so there is no
  split by key hash and no lock-free table: every insert is the
  translation's, one worker per target region.
- **Lids are wrapped**: a lid is (target shape, target region slot, key),
  its cells in the OWNER's coordinates; an old state's owner is found when
  the lid is made, a new one's by the translation. **Claims**
  (`unit::Claims`): the first unit to request a new state copies its row;
  the others' lids are resolved after the translation (`resolve_lids`).
  Room (6,2) 100% f57: 12.7M requests -> exactly the 6.7M new states.
- **Halo, not built.** On the f57 capture the units name 3.80M lids
  (unit, shape, slot, key) against 1.68M distinct (unit, shape, key): a halo
  (one name per key over a window) would make 2.1M fewer lids. A lid costs
  one visited-set lookup when made (~0.1-0.2 us), so the halo's saving is
  bounded by ~0.3-0.4 CPU-s, ~20-30 ms of a 0.86 s units phase at 16
  threads (3%), against a lid that spans up to four owner regions (an owner
  per cell quadrant). Every emission probes one table either way. Kept
  wrapped (Philippe's preference).
- **Blocks are lid by lid** with per-lid starts (a reverse probe decodes
  its lid only) and per-unit transfer ranks; streamed to per-worker blocks
  files during the wave. 3.68 B an edge with every table (f57), fg-2300's
  runs ~3.4 B.
- **The backward**: the BFS walks back through the frame's owner index
  (the units' translation tables inverted at the frame's end, 16 B a lid:
  `EdgeStore::preds_at`); the graph load streams the blocks unit by unit -
  per SOURCE unit, the pull order - and keeps only edges between marked
  nodes.
- **Storage regions of 16** (`CELESTE_STORAGE_REGION=16`) are exact too
  (room (6,2) 100% f0-f57: kept counts, ckhash and pos graph identical to 8)
  and store 7% less (3.41 against 3.68 B an edge; 2.38M lids against 3.80M
  at f57), but cost more: `bench-storage` f57, 16 threads, three
  interleaved reps each, units 1.31-1.33 s against 1.17-1.21 s (the end's
  counting sort runs over 256 cells a lid), translation 0.21-0.22 against
  0.16 s (fewer, heavier regions). 8 stays the default.
- **`bench-storage --phase units|translate|all`** (not dedup|edges): units
  (sinks, edges, blocks), then the translation and the layer, then the edge
  file. Its `f`/`e` lines are `ckhash`'s, comparable with the real tree.

Verified (the commits say which): position-free keys (sets identical to
fg-2300 by rekeying its trees); room (1,0) f0-f62 and (6,2) 100% f0-f57 kept
counts, ckhash and pos graphs identical; the three pinned gates; KEY_CHECK;
arc-check at r0sx and r0sxhn; 3 vs 32 threads and a resume identical
(states and edge content); `gates/raise.sh`; the ignored tests; end to end
room (1,0) `--ceiling 99`, (6,2) 100% `r0sxhf,r0sxh --ceiling 94` and (4,2)
`r0sxhn,r0sxh --ceiling 71`: the same optima, witnesses and `[gate]` counts.

**Where the trees are bigger** (room (1,0) h99: 52-53 GB against fg-2300's
46; (6,2) 100% h94: 70-71 against 66): the lids' tables, 24 B a lid (owner
by lid 8 B, the owner index 16 B; 243M lids, 5.8 GB, in (1,0)), and the
units' explicit source ids (8 B a frontier row; 1.6 GB). The edges
themselves are ~3.5 B, as the runs were. Since: the owner by lid derived
from the owner index when read (`EdgeStore::owners`, 0.4 s a graph-load
pass in (1,0)), sources a row range of the previous layer's frame file
where the unit's block was whole: (1,0) 50 GB, (6,2) 67 GB.
