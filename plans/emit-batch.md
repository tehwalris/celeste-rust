# Batched emission, canonical transfer ids, the purge delay (2026-10-12)

Branch `emit-batch` (from `develop` 8e1ca54). Philippe: "get things
handled, your call on the details" - the wave is instruction-bound and most
of it was the Rust emission after the kernels (plans/fwd-perf.md on
`fwd-perf`: keys ~21%, transfers ~20%, the sink ~14%, unit ends ~10%,
level -1 and the target cell ~8%, measured before edges-target and keys
only). Re-measured first, then: (1) batched emission, (2) canonical
transfer ids, (3) the smaller items, (4) a verdict on in-kernel key
packing.

Release, 16 threads. The reference frame: room (6,2) 100% r0sxhf, level -1
94,5, f56 -> f57 (5.86M lanes in, 257.7M emissions, 6.74M new states). The
mod fixture: map-mod's fmp f52 -> f53 (`tools/mod_bench.sh`'s fmp52 on
`map-mod`, cut to `/var/tmp/emit-modfix/fmp52`; 89.3M lanes in, 3.26G
emissions, 99.5M new states), run by a LOCAL merge of this branch into
`map-mod` (`emit-batch-mod`, not pushed: the mod stays out of develop).

## 1. Where the wave went (develop 8e1ca54, the reference frame)

`perf record -F 1999` over 3 reps, the wave workers' samples only,
attributed through inline chains (`tools/perf_inline.py`, from `fwd-perf`;
`resolve` with the binary's text mapping, `within` / per-line):

| what | % of the wave | where |
|---|---|---|
| transfer | 21.4 | `ArcSlots::words` per (body, lane) 13.8, `xfer_id_raw` 7.6 |
| key packing | 18.3 | `keys16` per (body, field, lane) |
| the sink | 17.6 | `emit` inline 7.1, `UnitBundles::insert` 5.9, the pos-graph set insert 4.6 |
| level -1 | 7.3 | `minus_one_drop` 6.7 (a one-entry cache), notes 0.6 |
| target cell | 6.5 | `cell_out` per (body, lane) |
| region phase | ~7 | `run_route` (the encoder), sorts, `RegionTable::find`, zstd |
| kernel mode | 5.0 | page faults, purges (section 4) |
| dup masks | 3.9 | `DupRef::eq_mask` |
| kernels | 3.5 | `.so` 1.2, `co_tile_flag` 1.7, `pack_input` 0.6 |
| unit end | 1.2 | `UnitBundles::drain` |

Per slice (16 lanes) the kernel runs ~197 bodies, ~95 take a lane;
lane emissions after the dup masks 511.9M, after level -1 257.7M. The dup
masks remove 520.7M (body, lane)s (149.6M `eq_mask` calls, 344.7M slot-pair
compares). Per kernel the bodies' 4656 key-field reads are 725 distinct,
their transfer root sets 12 distinct (of ~312 bodies), position sources ~20.

## 2. Batched emission (as built)

`compiled/asm_kernel/emit.rs` (a child module of `asm_kernel`). Each kernel
gets an EMISSION PLAN at build time: its distinct READS - a key field of an
output shape `(shape, cell, root slot, read kind)`, a transfer root set
(`ArcSlots`, with `fin`), a position-source set, a COLUMN `(output shape,
position set)` - each body naming its own by index. A slice, after the
kernel and the dup masks:

1. CELLS per position set over the union of its bodies' lanes; the
   pos-graph edges per set (the graph is a set: once per cell column, not
   per (body, lane); a 1024-entry direct-mapped cache before the set).
2. LEVEL -1 per column (one `admitted_from` per (shape, cell) run), the
   dropped lanes' notes per column.
3. The sink's TARGETS per column (`unit::Targets`: route, cell bit, the
   source's index and region, the offset; a raise's skip mask).
4. KEY READS: each read once over the union of the lanes of the bodies
   naming it. Lanes of equal values one lookup (a big dictionary is probed
   once per distinct value: rounds of `same = eq(lo) & eq(hi)`); small
   dictionaries lane-parallel; results written by masked blends into
   `Placed`, 16 keys as two planes of u64 (low, high words).
5. TRANSFERS: per root set, lanes of one transfer one `xfer_id_raw`
   (`ArcSlots::same_as` compares the roots column-wise).
6. Per body: its key = the outcome's uniform part ORed with its reads'
   planes (vector code; the high plane only where a read reaches past bit
   63), misses to provisional keys as before, then `UnitSink::emit_body`
   (`BodyEmits`: take mask, keys, targets, transfers) - lanes into one
   bundle grouped before the table insert. `emit` (the reference engine,
   the capture replay) is `emit_body` of one lane: one implementation.

The dup masks' slot-pair equalities are memoized per slice (`DupPairs`:
807k pairs over 191k distinct, ~4x). Scratch is per thread (`Cols`, sized
to the largest plan; ~400 kernels on the mod room share it).

**Byte-identical**: the same cells, keys and transfers per (body, lane),
handed to the sink in the same body and lane order. Every t1 frame's f/e
lines and exact metrics (edge bytes included) were the pins before section
3 touched the ids; `--key-check` on r10 and r42n; a whole forward of room
(6,2) 100% to f51: frame files, edge files and `xfer.bin` byte-identical
to develop's. The worker-local transfer ids are interned in another order;
a slice's global ids did not depend on it.

Three bugs on the way, all caught by `--key-check`/the frame checks: a
lane that missed in a read kept the previous slice's placed bits in its
partial key (now 0), likewise a shape the key space lacks; the mod
build's `--prune-rect` must drop a lane before its pos-graph edge.

## 3. Canonical transfer ids

A transfer's id was its place in `edges/xfer.bin` in DISCOVERY order: per
slice, the pairs no earlier slice met, sorted. Now, at the frame's end,
the frame's new pairs are renumbered (`XferTable::canonical`) by the edges
the frame recorded with each, most first, then by content - a function of
the frame alone, and the frequent transfers get the short varint ids.

Renumbering every segment that holds a new id would re-encode ~90% of a
frame's bundles (room (6,2) f51: 163 of 376 segments, 13.2M of 15.1M
bundles; +40-60 ms on 150-450 ms frames). Only 7-13% of bundles carry a
new id, so those WAIT for the frame's end exactly as bundles into a
provisional target wait for their key (`route::RouteState::prov`, re-encoded
at `seal`); the jobs' segments hold old ids and are final as made.

Measured on room (6,2) 100% r0sxhf forward f0-f51: frame end +6-13 ms at
f45-f51; the edge store 46.73 MB (discovery) -> 44.97 MB (-3.8%; content
order alone was 47.20 MB); `xfer.bin` byte-identical at 64k and 256k
slices; frame files and `ckhash --edges --dropped` identical to develop's.
The fixtures (`tools/fixtures.sh`) record the table's order (`xfers
canonical`) instead of the slice size; all rebuilt, every pinned
fingerprint reproduced; re-pinned `frame.edge_bytes` only (-1.2% to -4.0%).

The edge files still follow the slice size (a segment is a (route, slice)
job); `SLICE_LANES` is a constant, so a tree is a function of the search
and the code.

Gap (not new): `bench-frame` loads the tree's WHOLE transfer table
(`ForwardState::at`), so a frame check meets no new transfer; the frame
end's numbering is exercised by full forwards (t2, fixture builds).

## 4. The smaller items

- **Checkpoint threads** (fwd-perf: 1,341 threads, 3-4 s a frame): gone with
  keys only. The frame file is written by `layer::write_frame` on
  `par_map` over the threads: 55-259 ms a frame on the mod room's fmp run
  (its `forward.tsv`, f52-f77).
- **Frame setup** (fwd-perf: ~2 s serial `cells`/`order`/drop notes): those
  structures are gone. What a frame spends outside the wave, the frame end,
  the checkpoint and the pos graph is 128-289 ms at the fmp run's f53-f77
  (0.3% of 52-87 s frames). Not parallelized.
- **Allocation churn**: the per-unit buffers fwd-perf listed (`encode_block`,
  `RowBuf`, `sort_at`, lids) went with edges-target and keys only. What
  faulted now (develop's safe-run, purge delay 0) is the units' chunk
  vectors (`UnitBundles::drain`: allocated by a unit, freed by the region
  job on another thread, 39% of the wave workers' faults), the bucket
  growth, rehashes, the units' decoded rows. With a 100 ms purge delay the
  wave workers' faults fell from 1.19M to 22k (2 reps of the reference
  frame), so no pool was written.
- **MIMALLOC_PURGE_DELAY** (safe-run.sh's default, now 100 ms; mimalloc
  v2's own default is 10): interleaved, 2 rounds, load 12-16; the
  reference frame 3 reps, the mod fixture 1 rep; whole process:

| delay | faults (ref / mod) | kernel cycles G | wave | anonymous RSS at the wave's end |
|---|---|---|---|---|
| 0 | 3.0M / 12.0M | 46 / 156 | 1107-1165 ms / 16.6-17.3 s | 1.70 / 5.70 GB |
| 10 | 1.4M / 4.6M | 26-27 / 72-73 | 1042-1103 ms / 16.1-16.2 s | 1.85 / 6.00 GB |
| 100 | 0.93M / 1.33M | 20 / 39 | 1007-1023 ms / 15.6 s | 1.94 / 6.25 GB |
| 1000 | 0.93M / 1.2M | - | 1033-1053 ms / 16.3-20.5 s (load 21-24) | 1.96 / 6.22-6.27 GB |

  A unit takes ~12 ms, so at 10 ms half its freed pages were gone before
  the next unit; 100 ms keeps them. The cost is ~0.25 GB (reference) and
  ~0.55 GB (mod fixture) of anonymous peak; it is per in-flight
  allocation, not per state, so it does not grow with the visited set.

## 5. In-kernel key packing: not needed now

After the batching, the key work is 17% of the reference frame's wave
(reads 8.5, assembly 7.3, provisional 1.2) on a wave 2.2x shorter: ~2.6
worker-s a rep against develop's 6.3 (`keys16`). Moving it into the kernel
would remove at most the reads' lookups (the assembly is the ORs any
scheme keeps, and the misses still need Rust) and add the gathers and
probe loops to every kernel (fwd-perf section 3: dictionaries as data per
frame, multi-run placement, a bounded vector probe, KEY_CHECK as the only
net). The reads are ~1.4 worker-s a rep now; the region phase (17%) and
the sink's bundle table (25%) are bigger targets. Left alone.

## 6. Results

Interleaved (3 rounds of 3 reps on the reference frame, 2 rounds on the mod
fixture; load 16-24: another agent's 16-thread run beside it), develop
(purge 0) against this branch (purge 100, and 0). Instructions per rep: the
`perf stat` count of the run less a `--reps 0` run's (kernel build and
load), over the reps (a rep: the frame and the bench's fingerprints).

| | develop 8e1ca54 | emit-batch, purge 100 (0) |
|---|---|---|
| reference frame: instructions a rep | 521.2 G | 203.8 G (-61%) |
| wave | 2179-2996 ms | 1025-1033 ms, outliers to 1602 (1109-1135, outliers to 1715) |
| frame (wave + end) | 2362-3192 ms | 1174-1188 ms (1287-1315) |
| kernel-mode cycles (3 reps, whole process) | 45-46 G | 20-21 G (45-49 G) |
| anonymous RSS at the wave's end | 1.64-1.69 GB | 1.96-1.98 GB (1.72-1.73) |
| mod fixture: instructions a rep | 7511 G | 3978 G (-47%) |
| wave | 35.7 / 35.8 s | 19.5 / 18.7 s (20.7 / 22.0) |
| frame | 38.0 / 38.0 s | 21.2 / 21.1 s (22.8 / 24.2) |
| anonymous RSS at the wave's end | 5.68-5.71 GB | 6.21 GB (5.69-5.71) |

Every run's f/e lines are the pins (f057 6735699 fdeb81d1ed0dc69e, e057
257724013 66b87b7a69aedfe4; f053 99462112 056dcb210d8aad3a, e053
3262602942 01167db33bbe9289).

**The wave now** (this branch, purge 100, the reference frame; 15.0
worker-s a rep against develop's 34.5):

| what | % | worker-s a rep |
|---|---|---|
| region phase (dedup, encoder, sorts, zstd) | 17.1 | 2.6 |
| sink: `emit_body` (bundle grouping, per lane) | 12.9 | 1.9 |
| sink: the unit's bundle table (`UnitBundles::insert`) | 12.2 | 1.8 |
| keys: reads | 8.5 | 1.3 |
| dup masks and slice setup | 7.6 | 1.1 |
| keys: assembly | 7.3 | 1.1 |
| kernels (`.so` 2.2, call-outs and packing 4.9) | 7.1 | 1.1 |
| transfers | 4.5 | 0.7 |
| kernel mode | 3.1 | 0.5 |
| unit end, level -1, cells and pos edges, targets, decode | 3.0, 2.9, 2.1, 1.2, 1.6 | |

Against develop: transfers 7.4 -> 0.7 worker-s, keys 6.3 -> 2.6, level -1
and cells 4.8 -> 0.8, the sink (with the pos graph) 6.1 -> 3.9.

## Verification

- t0 217/217 (+1: `a_waves_new_transfers_are_numbered_by_count_and_content`);
  t1, t2 and big ok on fixtures rebuilt by this branch (their pinned
  fingerprints reproduced); the ignored tests 3/3 (resume, the room (7,1)
  level -1 table, the floor pin).
- Determinism (`forward`, final binary): room (1,0) r0sx f0-f56 and room
  (4,2) r0sxhn level -1 71,5 f0-f61, each at 3 and 32 threads and resumed
  (at f45, f50): frame files, edge files, `xfer.bin`, drop notes and pos
  graph byte-identical (115 and 186 files).
- `gates/raise.sh 1,0 55 56 58`: the raised trees' `ckhash --edges` and pos
  graphs are the fresh ones'.
- The room (1,0) gates (t2), and the full arc-check of the gate tree
  (f1-f35, 5,038,385 records): 0 without a transfer, 0 disagreements.
- `--key-check` on the reference frame (beside t1's r10 and r42n): ok,
  its f/e lines the pins.
- The mod fixture's f/e lines the pin (`tools/mod_bench.sh`'s
  `frame.pin`) in every run.

## Reproducing

- Profiles: `perf record -F 1999 -m 8 -- rewrite bench-frame ... --reps 3`,
  `perf script -F tid,time,ip,sym,dso -G`, the text mapping from `perf
  script --show-mmap-events` and `readelf -lW` (DELTA = pgoff + the R E
  segment's vaddr - offset), the wave workers' tids (those with
  `run_slice`/`emit_with` samples), `tools/perf_inline.py resolve`, then
  `outer` / `within`.
- The mod fixture needs map-mod's binary: merge this branch into a local
  map-mod branch; `--prune-rect` drops a lane after level -1 and before its
  pos-graph edge (per column).
