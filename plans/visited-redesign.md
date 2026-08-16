# Visited-set redesign: fp-runs + mmap'd per-frame key files

STATUS 2026-08-16: LANDED and default-on (engine `mmap`; env
`CELESTE_VISITED_ENGINE=map` keeps the old one selectable). All gates
below passed; headline numbers in BENCHMARK_DATA.md ("The visited set
left RAM"). The probe tuning section landed too (sample index +
interpolation + local-first). Still open from this plan: nothing -
follow-ups live as tasks (#119 leftovers: ladder.sh doc touch-ups,
sweep/band memory parity improvements beyond v1).

2026-08-16. Replaces the in-RAM `FxHashMap<(u64,u64), u32>` row table
(measured ~25 GiB anonymous RSS at room (2,0) f073, 814,001,064 rows) and
the `visited.bin`-per-checkpoint-dir duplication (12.4 GB x 19 dirs).

## The observation this is built on

The frame files `frames/fNNN.bin` are a PARTITION of the row table by
discovery frame - the sweep's index build asserts exactly this (every
saved lane's key in the table, stamped with its batch's frame, in exactly
one batch, `placed == n_rows`). So the table's keys are redundant given
the frame files; only the MEMBERSHIP STRUCTURE has to live in RAM, and it
does not have to hold full keys.

Ids are per-frame: frame f's new rows are exactly ids
[watermark[f-1], watermark[f]). But they are NOT positional within the
saved batch: the boundary MERGE (`vectorize_states`) runs after the
subtract, so `fNNN.bin`'s lane order is not id-assignment order (the
sweep's asserts check bijection via `id_of`, never order). Ids must
therefore be carried explicitly: the live engine records them at
assignment time, and migration reads them out of the old `visited.bin`
table. Never derive an id from a file position.

## Numbers that shaped the design (room (2,0) f073 meta + logs)

- 814.0M rows over 73 frames; LAST frame alone adds 92.4M new rows. The
  frontier grows ~13%/frame, so "new rows per boundary" is tens of
  millions, not the ~2M of room (1,0).
- Full decompress+deserialize+hash pass over ALL of room (1,0)'s history
  (405.6M rows, 94 frame files): 156 s single-threaded (`bwdt.index`).
  Decompression is cheap but not per-boundary cheap: a scan-every-
  boundary design costs 25-40 min per campaign. Rejected as the primary
  mechanism; acceptable only as a migration/rebuild pass.
- A lossy in-RAM fingerprint (even fp64) that DECIDES dups has a false-
  drop expectation of ~0.4 per campaign at this scale (92M new/boundary x
  814M stored / 2^64 x 95 boundaries). A silently dropped genuinely-new
  state is unsound, and a detect-and-die audit would deadlock resume
  (deterministic replay hits the same collision). Therefore: fingerprints
  may only FILTER; every hit is confirmed against the full 128-bit key
  INLINE, before the lane is dropped.

## Design

Three layers, all consulted in phase 1 of the frontier subtract (the
parallel, read-only half):

1. **`frames/fNNN.rowkeys`** (new, uncompressed, mmap'd): header (magic
   `C8RK`, format version, count, xxh3 of the records) + records sorted
   by (key.0, key.1): `{key.0: u64, key.1: u64, id: u32, pad: u32}` -
   24 B/row, 18.3 GiB at f073, written once per frame at the boundary
   (new rows only), shared across checkpoint dirs like `frames/` already
   is. Uncompressed because 128-bit hashes measured 1.05x compressible -
   zstd bought nothing, and mmap + binary search needs random access.
   The id payload is what lets the sweep and the bands rebuild key->id
   without re-hashing states. The xxh3 is verified on the one occasion a
   consumer reads the file end-to-end anyway (run-index rebuild), not on
   every open.

2. **Fp-runs in RAM**: sorted runs of `(fp64 = key.0, frame: u16)`,
   10 B/row in two parallel arrays = 7.6 GiB at f073 (vs 25 GiB map);
   fp48 packing can take it to 6.1 GiB later if the RSS number wants it.
   One run per recent frame, geometrically merged to keep run count
   ~O(log frames). Probe = binary search per run (fps are uniform, so
   interpolation search is the planned optimization if the probe shows
   up in profiles). A MISS is exact: the row is definitely new (key.0
   present is necessary for the full key to be present). A HIT names the
   candidate home frame(s).

3. **Inline confirm**: for each hit, binary-search the home frame's
   mmap'd `.rowkeys` for the FULL key. Present -> dup, drop the lane
   (and the id payload is right there if a consumer wants it). Absent ->
   fp collision, the lane is NEW. Exact 128-bit semantics, same
   guarantee as today's map, no deferred anything. The mmap traffic is
   page-cache-warm in steady state; under cgroup pressure it degrades to
   disk I/O instead of an OOM kill - the RAM the map used to pin becomes
   reclaimable page cache. (cgroup v2 counts page cache in
   memory.current but reclaims it before killing; MemoryMax stays the
   safety net it was.)

Serial phase (order-preserving, unchanged in role): candidates that
survived phase 1 are deduped within the boundary by a per-boundary
`FxHashSet` of full keys - the same first-lane-wins decision
`insert_new` makes today, so ids and watermarks stay byte-identical.
The set is transient (~2-3 GiB at a 92M-new boundary, freed each frame)
and replaces dedup work the 25 GiB map used to do in place.

Boundary end: sort the frame's new keys, write `fNNN.rowkeys`, mmap it,
append its run, push the watermark, clear the batch set.

## What each consumer does

- **Forward pass** (`verify.rs` stream boundary): as above. The
  `RowTable` map dies; watermarks stay.
- **Checkpoints**: FORMAT_VERSION stays 4 - the serde shape of every
  `.bin` payload is UNCHANGED, and bumping it would refuse every
  existing certified artifact (states.bin, fNNN.bin, posgraph.bin,
  g.bin all carry the version in their headers). What changes is the
  directory layout, and `meta.json` says which era a dir is:
  `visited_bin_len` becomes `Option<u64>` - `Some` means visited.bin
  (old era), `None` means `.rowkeys` (new era). Old binaries refuse new
  metas (missing->Some field fails serde), new binaries read both.
  Resume rebuilds the fp-runs by streaming the `.rowkeys` key.0 columns
  (already sorted; ~1-2 min at 814M rows) and verifies each file's xxh3
  then. `checkpoint::load` keeps returning a full `RowTable` for the
  consumers that want a map (sweep, bands), built from visited.bin or
  from `.rowkeys` (key,id) records - interchangeable by construction.
- **Sweep**: unchanged in v1 - `checkpoint::load` hands it the same
  `RowTable` map it gets today, built from `.rowkeys` when visited.bin
  is absent. (Positional id assignment in the index build is NOT
  possible - see the merge-order note above. A later memory
  optimization is fp-runs + exact compare against the in-RAM RowIndex
  values.)
- **Bands**: prev-level key->(e,g) via prev level's `.rowkeys` (key->id
  by binary search / bulk load) + watermarks + g.bin.
- **Migration**: `rewrite migrate-visited` derives `.rowkeys` for a v4
  dir from `frames/*.bin` (the 156s-class pass, positional ids), rewrites
  meta to v5. Existing room1/room00/room20 artifacts stay usable without
  re-deriving the searches.

## Gates

- Engine env-gated during bring-up (`CELESTE_VISITED_ENGINE=map|mmap`);
  the gate is BYTE-IDENTICAL states.bin / frames/*.bin / meta counts +
  watermarks on a room (1,0) smoke run, then the full suite, then one
  deep frame on room (1,0) f094->f095 and room (2,0) f073->f074 under
  both engines for wall clock + peak RSS. The engine choice is
  deliberately NOT in the campaign fingerprint, for the same reason the
  variant list is not: certified-identical trajectories, interchangeable
  checkpoints.
- The map engine is deleted, not kept, once certified - two membership
  structures is exactly the kind of parallel infrastructure this project
  does not keep. (Until the consumers in task #119 migrate, the map
  loader survives for THEM, fed by `.rowkeys` instead of visited.bin.)

## Probe tuning (after the naive baseline is benchmarked)

The v1 probes are plain binary searches; two structural facts make the
tuned version much better, and Philippe's B-tree instinct is the right
frame for why:

- **Pinned page index**: the files are immutable and fixed-stride, so
  build the B-tree's top level explicitly - sample the first key of
  every 4 KB page into a per-file RAM array (~16 B/page; ~72 MB for all
  of room (2,0)'s 18 GB). Search that in RAM, then touch EXACTLY ONE
  mmap page per confirm. Under memory pressure the worst case becomes
  one NVMe read per confirm instead of a chain of dependent touches,
  and we control what stays pinned instead of hoping the kernel keeps
  the right pages.
- **Interpolation search**: the keys are uniform hashes - the textbook
  best case. Expected O(log log n) probes, final iterations within one
  page. Applies to the in-page scan and to the fp-runs (also sorted
  uniform arrays), where it turns the ~30-step binary search per lane
  probe into a few RAM touches.

## Pipelining note (Philippe, 2026-08-16 - for later, may not pan out)

Overlap frame N+1's compute with frame N's output work (checkpoint
write, states.bin zstd, frames/fNNN.bin write). Honest first pass:

- Both sides are CPU-bound, so overlap mostly steals cores rather than
  finding idle ones - the win is bounded by how much of the output work
  currently runs SERIALLY while workers sit idle. `fwd.save_frames` was
  152s of ~2600s at room20 f70 (~6%), checkpoint saves on top of that;
  a 1-2 thread background writer that displaces ~6-12% of worker
  capacity is roughly break-even on paper, better in the boundary's
  serial stretches. Measure, don't assume.
- The saved states are immutable once the frame closes (Arc-shared
  heaps, ~100 MB at the boundary), so holding them for a background
  writer is cheap; correctness is just write-before-checkpoint-rename
  ordering, which the tmp+rename protocol already gives.
- Visited-engine wrinkle: frame N's `.rowkeys` file is CONFIRMED
  against during frame N+1 (fp-hits into frame N), so its write is on
  the critical path today. To pipeline it, keep the most recent
  frame(s)' keys in a RAM buffer that the confirm path checks before
  the mmaps, and flush lazily - a small hybrid tier, not free
  complexity.
- The one clearly-free piece: states.bin zstd inside checkpoint::save
  is single-threaded today; zstd multithread (or compressing while the
  NEXT stage's serial phase runs) needs no pipeline redesign at all.

## Soundness notes

- No lossy structure ever decides a drop: fp-runs only filter, the mmap
  confirm is against full 128-bit keys. This keeps exactly today's
  128-bit guarantee. The exact ROW-VALUE compare (against
  `frames/*.bin` content, retiring even 128-bit collisions - the
  hardened version #76 owes) becomes a natural later layer at the same
  hook, but is NOT part of v1.
- Never widen without a narrowing rung: not applicable here - this is a
  representation change, certified bit-identical, not an abstraction.
