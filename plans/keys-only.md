# Keys only: a state is its key, in memory and on disk (as built, 2026-10-12)

Branch `keys-only` (from `develop` a4c3dfc). Philippe: "why do we need
anything except keys at all?" We do not. A state is exactly `(shape, region,
position-free exact key, cell)`; rows are a transient kernel-input format.
This replaces the last uses of state ids (plans/storage-v2.md's
`StateId`, the frame files' id columns, the metadata's entry numbers, the
drop notes by id) and the forward's two layers of decoded rows (the
frontier being expanded and the next layer being built: the mod room's
+95 run held 32 + 31 GB of them at f77, beside a 17 GB visited set).

The running log was `PROGRESS.md` at the worktree root (not committed).

## The design

**A LAYER** (`storage::layer::Layer`) is a set of states: per storage region
(ascending) its ENTRIES - a key (ascending in the region) and the mask of
its cells (8 x 8 regions: a u64) - and the skeletons its shapes' rows
decode with. 24 B an entry; on the mod room an entry holds 14 states at f40
(fm85), so ~1.7 B a state against ~92 B a decoded row.

- The forward's FRONTIER is a layer (the last frame's new states); the
  NEXT layer is built by the wave. Nothing else holds the frame's states.
- The VISITED SET (`storage::visited`) is unchanged in form - per (shape,
  region) a table of entries (key -> cell mask) - and becomes a pure dedup
  structure: nothing outside it names an entry, so its entries are never
  renumbered (storage v2 renumbered each frame's new entries in key order
  to make ids canonical).

**Philippe's design input** (a medium-weak opinion: a second copy of the
region tables for the frontier, rather than "new this frame" inside the
visited tables). Adopted for the frontier, for a reason beyond his: the
region jobs APPEND to the visited tables during the wave, so a frontier
that named table entries would be read concurrently with writes. The
frontier is its own sorted (key, mask) list - walked sequentially, cut into
units directly, freed when the frame ends - and its order, (region, key),
is canonical with no entry numbering at all. For the NEXT layer the
alternatives were measured (below, "New cells: three ways").

**The wave** (`storage::wave::run_wave`):

1. The frontier is cut into UNITS of whole entries of one shape (about a
   256th of the frame, 1024-4096 states plus at most 63: an entry is never
   split).
2. A unit is DECODED right before the kernels (`decode::decode_rows`, the
   frontier's skeleton of its shape) into the existing lane buffers - the
   kernel ABI is unchanged. Rows of one entry share everything but the
   position, so each field's code is looked up once per entry (a "run"),
   and a column stays uniform until a run differs.
3. `frame::not_expanded` (exits, a 100% run's lost berry, the orb's
   deadline) is decided on the decoded unit - THE one place, for the
   forward's own frontier, a resume's and a raise's alike - and only the
   expanded lanes go to the engine (the kernels' skip mask is gone: no
   skipped lane reaches a slice).
4. The emissions go to the routes' bundles as before (plans/edges-target.md);
   a unit names its sources by its entries (region, key) and each lane's
   cell (the 32 B a frontier row `Sources` array is gone, as are the
   frontier's per-row cell and order vectors and the 12 B a row drop
   notes); level -1's drops are noted per lane in the unit and leave it as
   notes per (entry, horizon).
5. The REGION PHASE dedups each chunk target against its region's table;
   the cells an entry lacked are new. At the frame's end each route lists
   its new states by key (`route::seal`) - the NEXT LAYER, assembled by
   region. Its states are checked to be exactly the region phase's
   insertions (count), then DECODED once more, in chunks of 4096 on every
   thread, for their WINS (`frame::wins_of`) and, under `--key-check`,
   for every row's key against the boundary's and its position against
   its cell.

**Disk: the frame files** (`frames/f{F}.bin`; a raise's additions
`frames/f{F}.r{seq}.bin`, `checkpoint::FORMAT_VERSION` 15): a header - the
shapes the frame numbered `(index, hash)`, what the key space gained
(codes and bit runs with their indices: order-free), the skeletons of the
shapes the file holds (`layer::SkelRecord`: structure with captures,
globals, strings, prints, the non-value cells' uniform columns, each value
cell's representation), per region its index entry (entries, states, wins,
offset, sizes) - then per region a zstd -1 block: the keys delta-coded
(varint128), the masks, the win masks (sparse). No ids, no entry numbers.
Identity is (shape, key, cell) everywhere: ckhash, the arc gate, resume,
raise, the marks files, the UI export, the known route, check-known,
follow and every diagnostic (`layer::Tree`: a frame's layer, a state's
layer by search, rows decoded on demand; `rerun-row --row LAYER:N`, the
N-th state of a frame in (region, key, cell) order).

**THE COMPLETE STORAGE CHECK** (`layer::check_frame_file`), replacing the
write-time decode guard's role for what is STORED: every frame file is
read back after it is written and compared, region by region, with the
forward's layer - every region, entry key, cell mask and win - and the
layer's state count is checked against the region phase's insertions. A
difference is FATAL. What a row DECODES to is guarded as before and not
weakened: a shape's first emission per worker and wave must decode from its
key and cell to exactly the emitted row (`Skeletons::check_decodes`),
`--key-check` checks every emission and every new state, the start's rows
are checked whole (`decode::check_piece`), and a second skeleton of one
shape that would decode otherwise is fatal (`Skeletons::register`,
comparing `SkelRecord`s).

**Drop notes** (`dropped/f{F}.bin`): `(region, key, horizon) -> cells`,
sorted, zstd. A raise reads them as the layer of sources to re-expand.

**Resume** rebuilds the visited set from the frame files (each frame's
regions in parallel; a state stored twice is an error); the frontier is
the last frame's layer, its skeletons from its file. **Raise**: the noted
sources the new horizon admits are a layer (their skeletons the frame's),
expanded beside the states the raise added one frame earlier; a state's
layer (the refusal of a hit from a later layer) is kept per table place
while raising (`wave::StateLayers`). The raised frame's new states go to
`f{F}.r{seq}.bin` (and its edges to `edges/f{F}.r{seq}.bin`).

**Deleted**: `StateId` and its helpers, the frame files' id columns and
piece files (`checkpoint::FrameFile`, `save_block`), `storage::meta`
(the per-frame entry metadata), `storage::marks::Resolver`,
`decode::TreeKeys`, `wave::decode_layer`, `DropNotes` by id, `Sources`,
`Block`'s ids, seq and skip mask, the kernels' skip mask, the visited
tables' renumbering and `push_numbered`.

## Verification

- Every fixture rebuilt at format 15 (tools/fixtures.sh: r10, r62h, r42n,
  r42x, r10arc, r60s, r62h57) reproduces its pinned `ckhash --edges`
  (and `--dropped` under level -1) at every frame: states, edges and drop
  notes identical. The single-frame checks' f/e lines are the pins,
  including the reference frame (f057 6735699 fdeb81d1ed0dc69e, e057
  257724013 66b87b7a69aedfe4).
- `check.sh t1`, `t2`, `big` all ok: KEY_CHECK on two fixtures, the
  backward frame, the storage replay, ref-check, arc-check and widen-check
  at three levels, the room (1,0) gates (posgraph, ckhash, the arc gate's
  W lines, optimum 33, the witness, the known route), the objects ladder,
  the platform room. The exact metrics moved only by the units' cut (runs
  of whole entries in (region, key) order instead of id-ordered pieces):
  units -0.2..-1.5%, kernel slices +0.2..+0.6%, bundles -0.1..-0.6%, edge
  bytes -0.7..-1.7% - re-pinned on that evidence.
- `gates/raise.sh 1,0 55 56 58`: the raised trees' `ckhash --edges` and
  pos graphs are the fresh ones'.
- Determinism: room (1,0) r0sx to f56 (two slices a frame from f50) at 3
  and 32 threads and resumed at f45: frame files, edge files, `xfer.bin`
  and the pos graph byte-identical.
- The decoder against the first version (kept as a test reference): every
  state of a room (1,0) r0sxh forward to f40, in chunks that cut entries
  anywhere, decodes to the same block.
- Determinism with level -1 and objects: room (4,2) r0sxhn, level -1
  71,5, to f61 at 3 and 32 threads and resumed at f50: frame files, edge
  files, drop notes and pos graph byte-identical.
- The unit tests (213), the ignored ones (3: resume, the room (7,1) level
  -1 table, the floor pin), the full arc-check of the gate tree (f1-f35,
  5.0M records: 0 without a transfer, 0 disagreements).
- The mod room (map-mod's cart, a merge branch `keys-only-mod`, local):
  the second half from the mid state (fm, r0sxhe, level -1 85) and the +95
  run (fmp, `--prune-rect`): every `[gate]` line (W at every frame, the
  support, the optimum) and every frame's in/raw/kept counts identical to
  develop's runs; the fm f68 -> f69 frame's f/e lines identical.
- `export-ui` of a search (the gate's, `--save-marks`): the frames' and the
  marks' binaries byte-identical to develop's; run.json the same but its
  timings and the renamed `in_entries`/`out_entries`.

## New cells: three ways (Philippe's input, measured)

The region phase must say which cells are new this frame. Three ways were
weighed on the mod room's fm85 frames (`[fwd]` lines: the regions the frame
reached held every entry of the visited set's touched tables):

| frame | entries of the reached regions | events (a chunk target with new cells) | new entries | per-entry array (8 B an entry, zeroed each frame) | events (16 B) | a second (key -> cells) set during the wave (~32-40 B a new entry) |
|---|---|---|---|---|---|---|
| f40 | 14.5M | 3.4M | 1.8M | 116 MB | 55 MB | 58-72 MB |
| f69 | 67.9M | 14.8M | 7.3M | 543 MB (0.98 GB with growth slack) | 237 MB | 234-292 MB |

Built: the EVENTS - an append per chunk target that found new cells, merged
per entry and sorted by key at the frame's end. The smallest with the set,
sequential during the wave (no second probe), and its merge is one sort of
a few million pairs per route. The set would cost a second hash probe per
target in the region phase for the same bytes. The per-entry array (storage
v2's) was the first version here and is gone.

## Measurements

Release builds, 16 threads; develop a4c3dfc and map-mod f32df10 the
baselines. The develop mod runs (fm85, fmp95) ran on 2026-10-11 under
other agents' load (30-65); ours on 2026-10-12 beside our own checks: walls
are indicative, memory and sizes are not load-dependent.

**The mod room, the +95 run** (fmp: r0sxhe from the mid state, level -1
95, `--prune-rect`; 7.98B states, 248G emissions):

| frame | rows in | develop rss start / wave / peak (GB) | keys only rss start / wave / peak (GB) | frame (s), develop / keys only |
|---|---|---|---|---|
| f050 | 65M | 8.7 / 19.6 / 20.2 | 2.6 / 4.5 / 5.5 | 27.0 / 27.8 |
| f060 | 178M | 22.3 / 45.0 / 47.2 | 5.6 / 8.6 / 10.5 | 63.5 / 58.9 |
| f070 | 242M | 33.0 / 56.4 / 56.6 | 9.9 / 12.8 / 14.6 | 75.4 / 62.1 |
| f075 | 329M | 43.4 / 73.6 / 75.1 | 12.5 / 16.5 / 19.1 | 114.0 / 84.1 |
| f077 | 348M | 46.4 / 77.3 / 78.1 | 13.9 / 18.1 / 20.6 | 118.9 / 87.3 |
| f080 | 315M | 44.8 / 68.5 / 79.1 | 15.3 / 18.2 / 20.6 | 76.5 / 60.5 |
| f090 | 118M | 29.2 / 38.6 / 79.1 | 18.0 / 19.1 / 20.9 | 24.6 / 19.2 |

Whole run: peak 79.05 -> 20.91 GB; forward 2830 -> 2454 s, backward 335 ->
341 s, search 3248 -> 2798 s; the arc phase's anonymous peak 21.1 -> 13.1
GB (the sweep; the fingerprints' 21.1 GB went with the Resolver); tree 101
-> 16 GB (frames 78 GB -> 669 MB, drop notes 8.6 GB -> 191 MB, edges 15 GB
both). At f077 the visited set is 17.3 GB allocated (2.73 B a state; rss
start 13.9 GB) and the rest of the frame's peak 6.7 GB ([wavemem] at f075:
the route tables 10.2 GB - the visited set's, out for the frame - new-cell
events 0.69, segments 0.60, chunks in flight 1.03, the sinks 1.05, the
region scratch 1.02, the frontier 0.45). Before the wave's transients were
found ([wavemem], 7bfc5fe..bbaa74c), the first keys-only version peaked at
15.15 GB on fm85 (develop 40.55) with ~7 GB of transients at its 171M-row
frames; the bundles into provisional targets alone were 5.8 GB at fmp's f53.

**The mod room, the second half** (fm: level -1 85; 3.68B states): peak
40.55 -> 15.15 GB (the first version, above), forward 1112 -> 923 s,
backward 55.0 -> 55.0 s, tree 43 -> 6.4 GB (frames 307 MB: 0.08 B a state).

**Its peak frame alone** (fm f68 -> f69, `bench-frame`, 171.9M rows in,
4.17G emissions, 156M new; two rounds): see the A/B table below.

**Decode**: 49 ns a row in a unit (release, the mod frame; the field codes
per run of one entry, ~14 states an entry at f40; `not_expanded`
included) - 2% of the units' worker time; the frame's end (the wins'
decode of 156M new states, the seal, the edge file) 1.2 s against develop's
6.1-6.3 s for its layer decode and the rest.

**What the mod room's exit can reach now** (plans/map-mod.md's projection,
r0sxhne counts: states S(H) of a search to the exit at H and its peak
frame P(H); its RAM column was develop's ~290 B a row in). Keys only, from
fmp: the visited set ~2.2 B a state resident (2.7 allocated; at the peak
frame ~80% of S(H) is visited), the frame's rest ~3.5 GB of
slice-bounded buffers plus ~9 B a row in:

| H | S(H) | P(H) | develop: wave memory | keys only: peak (est.) | disk (~2 B a state) | forward / backward |
|---|---|---|---|---|---|---|
| 205 | 7.3B | 319M | 117 GB | ~19 GB | ~15 GB | 1.4 / 1.1 h |
| 210 | 11.3B | 497M | 181 GB | ~28 GB | ~23 GB | 2.1 / 1.8 h |
| 220 | 27B | 1.2B | 437 GB | ~62 GB | ~54 GB | 5 / 4.2 h |
| ~225 | ~42B | ~1.9B | - | ~95 GB | ~85 GB | ~8 / ~7 h |

So RAM no longer stops the forward near H 203: it fits to H ~222-225 on
this machine (110 GB less headroom), and past H ~210 the WALL is the
limit (hours a run). What binds next is the visited set itself (2.2-2.7 B a
state: key 16 B, mask 8 B and index ~8 B an entry). The projection's big
uncertainty is S(H) past f152 (map-mod.md: extrapolated at +9%/frame).

## Follow-ups (not built)

- **The visited set** is now what RAM buys: a u64 key where a shape's key
  fits in 64 bits (8 B an entry less; the mod room's keys reach 66 bits
  at f66, so a per-shape choice), and less index slack.
- **Smaller slices** (`SLICE_LANES` 1M -> 256k): the chunks in flight and
  the region jobs' buffers scale with a slice (~2 GB of the mod room's
  frame peak), at the cost of more, smaller segments in the edge files.
- **The drop notes** (32 B a note, held to the frame's end): per unit
  sorted and delta-coded they would take a few bytes.
- **Decode in the kernel prologue** (the ABI unchanged for now).
