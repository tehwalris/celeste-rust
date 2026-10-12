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

**ONE STATE SET, used three times** (`storage::set`, 2026-10-12; Philippe:
"the same structure for the visited set, the frontier and the next layer:
separate copies of the same thing, nothing that pushes ids around"). A
`RegionSet` is per storage region (ascending) a `RegionTable` of ENTRIES -
an exact key and the mask of its cells (8 x 8 regions: a u64):

- the VISITED SET (`storage::visited`): INDEXED (a hash index on the keys),
  with the shapes it numbered and the key space; READ ONLY during a wave;
- the NEXT LAYER, while a wave builds it: per route an INDEXED table of the
  cells this frame reached that the visited set lacks (insert or OR);
- the FRONTIER, a frame's layer read from its file, the next layer once
  its wave ends: FROZEN (sorted by key, the index dropped) - iterated in
  (region, key) order, membership by binary search (`storage::layer::Layer`
  is a frozen set and its shapes' skeletons).

The operations: insert/OR (`insert_or`), membership (`find`, `mask_of`),
iteration in order, `freeze`, `merge` (a frozen set ORed into an indexed
one), `rekey` (a wave's provisional keys made final), `union` (a frame and
its raise), drop. Nothing names an entry outside its table, so entries are
never renumbered (storage v2 renumbered each frame's new entries in key
order to make ids canonical).

Lean, so every use gains alike:

- a KEY takes the bytes its table's largest key needs (1-16; the mod room's
  67-bit keys 9 B, room (6,2)'s 42-bit ones 6 B, a table holding a wave's
  provisional keys 16); a wider key repacks the table;
- the INDEX is `u32` slots - the entry and, in the bits the capacity
  leaves, a tag of the key's hash, so a probe reads a key only on a tag
  match - at a load of 0.5-0.85 over a non-power-of-two capacity (~5.6 B an
  entry; the u32 index at 0.25-0.5 was ~11);
- the keys and masks live in CHUNKS of 32768 entries: a table grows without
  a copy and holds part of one chunk unused at most (whole-array growth
  copied the visited set at every frame's end, both copies alive: +3.4 GB
  at the mod room's f77).

Per entry: key + 8 + ~5.6 B indexed, key + 8 frozen. The visited set at the
mod room's f76 (17.3 states an entry): 23.2 B an entry, 1.34 B a state
(2.74 before); its frontier 17 B an entry, 0.95 B a state (1.88).

**Philippe's earlier design input** (a second copy of the region tables for
the frontier, rather than "new this frame" inside the visited tables) is
this design's first use: the frontier is its own frozen set - walked
sequentially, cut into units directly, freed when the frame ends - and its
order, (region, key), is canonical with no entry numbering. Keeping the
visited set read only during the wave also makes the region jobs' reads of
it safe without taking its tables out.

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
5. The REGION PHASE looks each chunk target up in its region's visited
   table (read only) and ORs the cells it lacks into the route's NEXT-layer
   table (indexed; a second probe only where a target has new cells); the
   cells the next table lacked too are new states. A
   job's bundles into PROVISIONAL targets (a code new this frame: no final
   key yet) are encoded into a zstd segment with their provisional keys
   and encoded again with the final ones at the frame's end (held as
   56 B structs they were 5.8 GB at the mod room's fmp f53). At the
   frame's end each route's next table gets its final keys and is FROZEN
   (`route::seal`) - the NEXT LAYER, assembled by region. Its states are
   checked to be exactly the region phase's insertions (count), DECODED
   once more, in chunks of 4096 on every thread, for their WINS
   (`frame::wins_of`) and, under `--key-check`, for every row's key against
   the boundary's and its position against its cell, then MERGED into the
   visited set (region by region in parallel; none may be held already).
6. The wave's buffers are bounded and measured (`--phases`: `[wavemem]`
   per frame, the structures by bytes; `[mem]` per phase, the sampled
   peak): a unit keeps at most 512k bundles' room in its emptied buckets,
   a region worker 1M bundles' between jobs, a job names its targets in the
   job (the encoder's arrays the job's size: they were the region's, 2 GB
   at the mod room's f77), the segments are held at their compressed size,
   slices are 256k lanes (1M: the chunks in flight and region jobs 4x),
   the exited workers' freed pages are collected (`mi_collect`) after the
   pipeline and the frame's end.

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
layer merged in, its regions in parallel; a state stored twice is an
error); the frontier is the last frame's layer, its skeletons from its
file. **Raise**: the noted
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
tables' renumbering and `push_numbered`, the routes' per-entry new-cell
array, the new-cell events, the visited tables' take-out for a frame, the
raise's own map of its new cells. 2249 lines deleted, 2752 added (src, crates, ui): the frame file
and tree reader (`storage::layer`, 990 lines with its tests), the first
decoder kept as the test reference (~110), the `[wavemem]` accounting
(~90).

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
- The one set (c2325d2..fc6130b): t0 215/215, t1, t2 and big ok; every
  fixture's f/e lines the pins (the metrics re-pinned for the slices and
  edge bytes only: fc6130b); `gates/raise.sh` OK; room (1,0) r0sx to f56
  at 3 and 32 threads and resumed, and room (4,2) r0sxhn level -1 71,5 to
  f61 likewise: frame files, edge files, drop notes and pos graph
  byte-identical, ckhash the r42n pin; the mod room's fmp95: every
  `[gate]` line and every frame's counts identical; the A/B pairs below.
- `export-ui` of a search (the gate's, `--save-marks`): the frames' and the
  marks' binaries byte-identical to develop's; run.json the same but its
  timings and the renamed `in_entries`/`out_entries`.

## New cells: three ways (measured), then one structure

The region phase must say which cells are new this frame. On the mod
room's fm85 frames:

| frame | entries of the reached regions | events (a chunk target with new cells) | new entries | per-entry array (8 B an entry, zeroed each frame) | events (16 B) | a second (key -> cells) set |
|---|---|---|---|---|---|---|
| f40 | 14.5M | 3.4M | 1.8M | 116 MB | 55 MB | 58-72 MB |
| f69 | 67.9M | 14.8M | 7.3M | 543 MB (0.98 GB with growth slack) | 237 MB | 234-292 MB |

The events were built first (the smallest with the set, no second probe).
Philippe chose the set: one structure, three copies, nothing pushing
entry numbers around - and with the lean table the set is ~29 B a new
entry indexed during the wave (16 B keys while it can hold provisional
ones), 17 B frozen. Its second probe costs nothing measurable (the region
phase's worker time went DOWN, 4.2 -> 2.9 s on the reference frame, with
the job-local target names and smaller slices).

## Measurements

Release builds, 16 threads; develop a4c3dfc and map-mod f32df10 the
baselines. The develop mod runs (fm85, fmp95) ran on 2026-10-11 under
other agents' load (30-65); ours on 2026-10-12 beside our own checks: walls
are indicative, memory and sizes are not load-dependent.

**The one state set** (c2325d2..ef93c48 against 2ef9213, the first
keys-only build; interleaved, 2 rounds, at load 21-33 - another agent's
14-thread mod search beside it - so walls are good to ~5%). Bytes as
allocated (`bench-frame`'s load line, the `[fwd]` line's `visited`):

| | visited set: B a state (an entry) | frontier: B a state (an entry) | max RSS |
|---|---|---|---|
| the reference frame (6,2) 100% f56: 36.9M visited (1.84M entries), 5.86M rows in (576k) | 2.2 -> 1.05 (21) | 2.36 -> 1.37 (13.9) | 2.32-2.33 -> 1.88-1.91 GB (`bench-frame`: two visited copies) |
| the mod room's fmp f76: 5.98B visited (345M entries), 348M rows in (19.4M) | 2.74 -> 1.34 (23.2) | 1.88 -> 0.95 (17) | 37.4 -> 20.9-21.0 GB (the same) |
| room (1,0) `--ceiling 99`, f99 (213M visited) | 2.2 -> 0.94 | | 2.15 -> 1.65-1.68 GB |
| room (6,2) 100% `r0sxhf,r0sxh --ceiling 94`, f94 (315M visited) | 2.0 -> 0.92 | | 3.86-3.88 -> 2.94-3.04 GB |
| the mod room's +95 run (fmp95), f077 / f090 | 17.3 -> 8.34 GB / 22.4 -> 11.0 GB (1.32 / 1.24 B a state) | | 20.91 -> 13.66 GB |

At the same speed or faster: the reference frame's wave 3727-3977 ->
3439-3898 ms, its frame 3991-4217 -> 3694-4162 ms (the region phase's
worker time 6.1-7.8 -> 4.6-5.4 s); the mod room's fmp f76 -> f77
(`bench-frame`) wave 154.8 / 165.6 -> 134.8 / 142.1 s, frame 187.7 /
207.2 -> 143.3 / 150.1 s (the region phase 648 / 784 -> 244 / 316
worker-s: the encoder's arrays were its region's size); room (1,0)'s
forward 133.7 / 139.5 -> 137.0 / 136.5 s, its backward 2.9 / 3.2 -> 3.2 /
3.2 s; room (6,2)'s level-0 forward 211.8 / 207.5 -> 211.6 / 213.7 s,
backward 1.9 / 1.8 -> 1.9 / 1.7 s. Trees: room (1,0) 570 -> 593 MB, room
(6,2) 1.85 -> 1.84 GB, fmp 16 GB both (the 256k slices: more, smaller
segments; edges +4% on room (1,0), -4% on the mod room).

The 256k slices cost the BACKWARD at first: 2.3x the segments (fmp95:
150k -> 348k decoded), and its read walked each segment's targets
against the region's whole group of changed targets, one by one - 26% of
the mod room's backward CPU (perf). Galloping through the group
(ef93c48, `arc_dp::seek`), `bench-backward` W f86 of h95 on both
slicings' fmp trees (fc6130b against ef93c48, 2 interleaved rounds, load
22-25): the read of f87-f91 96.2 -> 58.4-59.4 s on the 256k-slice tree
and 73.5-74.5 -> 60.6-63.2 s on the 1M one; f86's read 23.6-24.2 ->
12.4-12.7 s (256k) and 15.8-16.4 -> 12.7-13.2 s (1M); the push down to
f87 145.7 -> 108.3 s (256k), 122-126 -> 111-115 s (1M); W f086
identical in all eight. So the slices cost the backward nothing now, and
its read is faster than before them.

Every pair: the same f/e lines, `[gate]` lines, optima and witnesses;
the known routes survive (`--prefer`); fmp95's per-frame counts
identical.

**The mod room, the +95 run** (fmp: r0sxhe from the mid state, level -1
95, `--prune-rect`; 7.98B states, 248G emissions):

| frame | rows in | develop rss start / wave / peak (GB) | keys only | the one set | frame (s), develop / keys only |
|---|---|---|---|---|---|
| f050 | 65M | 8.7 / 19.6 / 20.2 | 2.6 / 4.5 / 5.5 | 1.9 / 3.9 / 4.9 | 27.0 / 27.8 |
| f060 | 178M | 22.3 / 45.0 / 47.2 | 5.6 / 8.6 / 10.5 | 3.9 / 6.7 / 6.9 | 63.5 / 58.9 |
| f070 | 242M | 33.0 / 56.4 / 56.6 | 9.9 / 12.8 / 14.6 | 6.8 / 9.3 / 9.7 | 75.4 / 62.1 |
| f075 | 329M | 43.4 / 73.6 / 75.1 | 12.5 / 16.5 / 19.1 | 8.7 / 11.6 / 12.1 | 114.0 / 84.1 |
| f077 | 348M | 46.4 / 77.3 / 78.1 | 13.9 / 18.1 / 20.6 | 9.5 / 12.4 / 12.6 | 118.9 / 87.3 |
| f080 | 315M | 44.8 / 68.5 / 79.1 | 15.3 / 18.2 / 20.6 | 10.4 / 12.6 / 13.0 | 76.5 / 60.5 |
| f090 | 118M | 29.2 / 38.6 / 79.1 | 18.0 / 19.1 / 20.9 | 12.1 / 13.4 / 13.7 | 24.6 / 19.2 |

(The peak is the process's so far: the one set's f050 is the level -1
table's build. The one set's frames ran at load ~31 beside another
agent's search: their walls are not comparable, see below.)

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

**Interleaved A/B** (2026-10-12, load 5-17; `bench-frame` copies the
visited set, so its max RSS holds it twice):

| case | develop | keys only |
|---|---|---|
| the reference frame (6,2) 100% f56 -> f57, 2 x 3 reps | wave 2084-2157 ms, end 233-244 ms, frame 2590-2670 ms; max RSS 4.21-4.24 GB; f56 loaded in 1.3-1.5 s | wave 2147-2168 ms, end 55-59 ms, frame 2327-2353 ms; max RSS 2.26-2.30 GB; loaded in 0.20 s |
| the mod room's peak frame fm f68 -> f69 (171.9M rows in, 4.17G emissions, 156M new), 2 rounds | wave 31.4 / 33.1 s, end 6.2 s; f68 loaded in 61 s, frame with the bench's frontier copy 54.6-72.3 s; max RSS 59.6-60.5 GB | wave 31.5 / 31.5 s, end 1.17 s; loaded in 7.2 s, frame 33.5 s; max RSS 16.3 GB |

Whole searches (BENCHMARK_DATA.md 2026-10-12 has the table): room (1,0)
`--ceiling 99` forward 84.4 -> 78.1 s, max RSS 3.7 -> 2.1 GB, tree 2.50 ->
0.57 GB; room (6,2) 100% `r0sxhf,r0sxh --ceiling 94` level 0's forward 132
-> 122 s, max RSS 8.3 -> 4.0 GB, tree 8.08 -> 1.85 GB; the backward the
same; the same optima, witnesses and `[gate]` lines.

The reference frame's wave is 2-3% slower (the units decode their rows,
0.4 of 37 worker-s, and their (region, key) order changed the kernels'
slices by +0.2%), its frame 10% faster (the end: no layer decode, no row
copies). At first the bounds on the units' spare buckets and the region
scratch were tight (128k, 256k): the wave was 8% slower, the units
regrowing their buckets job after job; 512k and 1M cost the mod frame
~0.8 GB of peak against the tight ones and keep the wave within 3%.

**Decode**: 49 ns a row in a unit (release, the mod frame; the field codes
per run of one entry, ~14 states an entry at f40; `not_expanded`
included) - 2% of the units' worker time; the frame's end (the wins'
decode of 156M new states, the seal, the edge file) 1.2 s against develop's
6.1-6.3 s for its layer decode and the rest.

**What the mod room's exit can reach now** (plans/map-mod.md's projection,
r0sxhne counts: states S(H) of a search to the exit at H and its peak
frame P(H); its RAM column was develop's ~290 B a row in). From fmp95: the
first keys-only build held the visited set at ~2.2 B a state resident
(2.7 allocated) and the frame's rest at ~3.5 GB plus ~9 B a row in; the
one set holds it at 1.24-1.32 B a state (allocated = resident) and the
rest at ~2.6 GB plus ~4 B a row in. The peak is the larger of the peak
frame's (~80% of S(H) visited) and the last frames' (all of it):

| H | S(H) | P(H) | develop: wave memory | keys only: peak (est.) | the one set: peak (est.) | disk (~2 B a state) | forward / backward |
|---|---|---|---|---|---|---|---|
| 205 | 7.3B | 319M | 117 GB | ~19 GB | ~12 GB | ~15 GB | 1.4 / 1.1 h |
| 210 | 11.3B | 497M | 181 GB | ~28 GB | ~17 GB | ~23 GB | 2.1 / 1.8 h |
| 220 | 27B | 1.2B | 437 GB | ~62 GB | ~38 GB | ~54 GB | 5 / 4.2 h |
| ~225 | ~42B | ~1.9B | - | ~95 GB | ~58 GB | ~85 GB | ~8 / ~7 h |

So RAM no longer stops the forward near H 203: the first keys-only build
fit to H ~222-225 on this machine (110 GB less headroom), the one set to
H ~230 (S ~80B at +9% a frame), and past H ~210 the WALL is the limit
(hours a run). The backward's sweep (fmp95: 12.5 GB anonymous, the
forward's peak 13.7) is now as large as the forward; how it scales with H
is not measured. The projection's big uncertainty is S(H) past f152
(map-mod.md: extrapolated at +9%/frame).

## Follow-ups (not built)

- **The visited set** is still most of the forward's peak (1.24-1.32 B a
  state at the mod room: a 9 B key, the 8 B mask and ~5.6 B of index an
  entry of ~17 states).
- **The backward's sweep** (fmp95: 12.5 GB anonymous) now matches the
  forward's peak.
- **The drop notes** (32 B a note, held to the frame's end): per unit
  sorted and delta-coded they would take a few bytes.
- **Decode in the kernel prologue** (the ABI unchanged for now).
