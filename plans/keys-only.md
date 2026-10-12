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
- The unit tests (212), the ignored ones and a full arc-check: see the
  commits.

## Measurements

(filled in below as measured)
