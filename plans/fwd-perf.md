# Where the forward's time and memory go (2026-10-11, branch `fwd-perf`)

Philippe: "understand the full situation" of the forward on the mod room
(`--cart taslevel.p8`, room (0,0), coarse level `r0sxhne`, level -1, the
objects ladder `r0sxhne,r0sxhe,r0sxe`, h168, 16 threads). Read-only
investigation; the only code is two `--phases` diagnostics (`[mem]`,
`[keydiag]`, commit a2f7425) and `tools/perf_inline.py`.

## How it was measured

- **Live**: the mod search (`/var/tmp/mod-ck/lad168`, binary
  `/var/tmp/mod-target/release/rewrite`, map-mod 0d936cf, release + line
  tables). The first process (pid 1875052) was killed by its 60G cap during
  f158 at 14:58; the restart (pid 2434117, 95G) finished the forward at
  f168 at ~15:20. Profiles: Philippe's 20 s sample `/tmp/modperf.data`
  (end of f157's wave, its translation, the start of its layer) and one
  whole frame, f160, captured with `perf record -F 249` (cycles, every
  thread), `perf record -e page-faults -c 4 -d` and `perf stat -I 1000`
  (`/var/tmp/fwdperf/capture.sh`: from one `[fwd]` line to the next).
- **Fixtures**, `rewrite bench-frame --phases` (8 threads, `--edges`):
  the mod room itself - the r0sxhne tree `/var/tmp/mod-ck/mid155`
  (hard-linked, level -1 155,5) at f130 -> f131, which reproduces the live
  f131 exactly (753,671,263 emissions, 17,710,339 new states) - and the
  devloop fixtures r62h57 f56 (the reference frame; run at 95 GB
  available), r62h f50, r42n f60, r10 f55.
- **Attribution**: no frame pointers, so `-g` chains are junk and `perf
  report` puts everything inlined into `run_slice`. `tools/perf_inline.py`
  resolves every sampled address's INLINE chain (`addr2line -i`) and splits
  a function by its own source lines (the line numbers below are map-mod
  0d936cf's).
- **Caveat**: the machine was shared (load 30-65 from other agents; my
  8-thread fixture runs sat on the search's SMT siblings). Wall times of
  fixture runs moved up to 3.5x between reps; shares, counts, instructions
  and bytes did not. Quoted walls are the live search's or the best rep.

## 1. Time

### The frame (live f160, 105.8 s: 199.4M rows in, 9.02G emissions, 175.1M new states, 7.45G edges)

| phase | wall | threads busy | IPC | kernel-mode share | what |
|---|---|---|---|---|---|
| frame setup | ~2.5 s | 1 | 2.1 | 18% | `run_wave`'s `cells` (`Block::positions`), `order`, `DropNotes::new`: SERIAL |
| wave (units) | 77.4 s | 16 | 2.71 | 9.9% | kernels + emission + unit ends |
| translation | 8.8 s | 16, then 1 | 0.65 | 11.6% | memory-bound (below) |
| layer | 11.7 s | mixed | 0.70 | 25% | gather 2.2 s (16 thr), `XferTable::merge` ~5 s SERIAL, edge file ~2 s, frees/compaction ~2.5 s |
| checkpoint | 4.9 s | up to 1,341 threads | 0.85 | 86%* | one thread per piece, each decoding its piece (the rows-from-keys guard) |

(*) inflated by perf itself: inheriting counters into 1,341 fresh threads
shows as `perf_event_update_userpage`/`native_write_msr`. Without perf the
checkpoint is 3.3-6.2 s a frame (f153-f160 `[fwd]` lines).

Translation varies a lot between frames: 7.7-9.4 s at f157/f159/f160,
21-22 s at f155/f158 (cause not isolated; those frames' peaks were the
highest).

**The wave is instruction-bound**: 1,725 instructions and 638 cycles per
emission (after level -1), IPC 2.71, 1.8 LLC misses an emission. Cutting
instructions cuts time.

### The wave, split (live f157 sample: 81,193 samples on the 16 wave workers)

| what | % of the wave | where |
|---|---|---|
| **exact-key packing** | **21.2** | `AsmBody::keys16` (line 603): big dictionaries' per-lane hash probes (`find_num_placed`, `num_slot`) ~8.0; small dictionaries matched lane-parallel (lines 250-269) ~7.6; booleans ~1.7; reading roots, `field()`, the ORs, the rest |
| **transfer: `ArcSlots::words`** | **13.9** | line 615: per (body, lane) 10 roots matched by kind, 21 words built |
| transfer: `xfer_id_raw` | 5.6 | line 616: hash of 21 words + compare (misses 0.6-0.9% of lanes: decode 0.4) |
| sink `lid()` | 6.3 | the unit's lid table probe (`slot_hash`, `mix64`) + the visited lookup on a new lid |
| sink `emit` inline | 5.8 | `slot_local`, the lid mask test, `requests`/`edges` pushes |
| `Claims::claim` + `VisitedSet::find` | 1.5 | sharded mutex `FxHashSet` insert per request |
| row copy for requests (`push_row`) | 5.6 | 2.7 inline + 2.9 out of line; ~2% of emissions are requests, each copies every column |
| unit end | 9.7 | counting sort + `encode_block` 6.5, the per-target `sort_unstable` (u64 edges) and the transfer-rank sort 3.2 |
| level -1 drop check | 4.1 | `minus_one_drop` (line 579), a one-entry (shape, cell) cache |
| target cell | 3.8 | `cell_out` (line 577) per lane |
| dup mask | 2.9 | `DupRef::eq_mask` (line 555) |
| pos-graph pairs | 2.9 | `pos_edges.insert` per emission (an `FxHashSet<(u32,u32)>`, lines 584/622; only a one-pair cache before it) |
| kernels + call-outs + packing | 6.7 | `.so` bodies 2.8, `co_tile_flag` 2.4, `co_mget` 0.7, `pack_input` 0.4, `input_views` (a `Vec` per slice) 0.1 |
| kernel mode | 5.5 | faults, purges, TLB shootdowns (section 4) |
| unattributed | 0.9 | |

The fixture (mod f130 -> f131, release, 133,109 wave samples) agrees within
~2 points: transfer 18.9 (words 13.3, cache 5.4), keys 16.1 + the miss path
1.5, `push_row` 7.4, sink 14.4 (emit 5.0, lid 5.4, claims 2.5, rehash 0.9,
find 0.6), unit end 9.0, dup 2.9, level -1 2.9, `cell_out` 2.7, pos 1.8,
kernels + packing 8.1, kernel mode 5.3. The `[phases]` worker-seconds say
the same at every fixture: emit 55-73%, unit end 8-12%, kernel 2-7%, pack
1-2%.

So of `run_slice` inclusive (~86% of the wave with its non-inlined
callees), the key packing is ~25%, the transfer ~23%, the sink ~16%, the
row copy ~7%, the kernel calls and packing ~8%.

### The "19% inlined thread body" = the TRANSLATION

The 20 s sample covers three thread groups: the last 10.8 s of f157's wave
(16 workers), then the translation (16 threads, 12.2-18.4 s), then the
layer gather (19.2-20.0 s). The 19% `__rust_begin_short_backtrace` is the
translation workers' `translate::{{closure}}` inlined into the thread
entry (24,244 of the 41,410 translation samples). Within the translation
(lines of `storage/wave.rs`, map-mod):

| what | % of the translation's samples |
|---|---|
| `items.sort_unstable()` per region: (key, cell, unit, request), 28 B | ~25.5 (quicksort 22.8, small sorts 2.7) |
| `b = a + items[a..].partition_point(x.0 == key)` per key (480) | 13.6 |
| per region, a binary search in EVERY unit's request list (467-468): regions x units searches | 10.8 |
| gathering the region's items from every unit (470) | 7.4 |
| per cell `partition_point` (492) | 4.8 |
| `RegionTable::find` / `place` / `reindex` / `push_key` | ~7.7 |
| owners, `news` push, `set`, the rest | ~8 |
| kernel mode (fresh pages for `items`, tables) | 7.4 |

Both `partition_point`s are binary searches over the region's whole
remaining items where a linear scan of 1-3 entries would do; the per-unit
binary searches are regions x units (40k units at f158). IPC 0.65 overall.

### The sorts

| sort | where | cost |
|---|---|---|
| per target (lid, cell), the u64 edges; transfer ranks | `UnitSink::end` (wave) | 3.2% of the wave |
| the region's requests (28 B tuples, 130-185M a frame) | `translate` | ~25% of the translation |
| each unit's requests by region | `translate`'s `par_map` (helper threads) | ~10% of the helpers' samples |
| the owner index (r, e, u, l) per lid; new transfer pairs | `edges::write_file`, `XferTable::merge` | most of the layer's quicksort |

### Page faults: 40.2M a frame (f160), ~160 GB of zeroed 4 KB pages

| phase | share | from |
|---|---|---|
| wave | 47.8% | `encode_block`'s fresh `out` per unit, freed after the write (10.7% of ALL faults), `RowBuf` columns regrowing from empty every unit (`push_row`, 3.7%), libc `memcpy`/`memset` in growth and `sort_at.clone()` per unit (18.8%), mimalloc page extends (4.9%), hash-table rehashes (claims, pos sets: 4.1%), `lid_masks`, `edges` |
| checkpoint | 40.0% | `decode_rows` and `column` (the rows-from-keys guard decodes every new row into fresh columns, per piece thread) 30%, `whole_range_col` |
| layer gather | 9.2% | `gather_rows`' new columns |
| setup | 2.1% | `cells`, `order` |

`safe-run.sh` sets `MIMALLOC_PURGE_DELAY=0`, so every freed per-unit
buffer goes back to the OS and the next unit faults it again (and the
purges cost TLB shootdowns: `smp_call_function_many_cond`, the zone
spin lock 2.1% of the wave). The reason it was set ("the door rebuilds
every shard each frame") is gone with storage v2. A/B on the mod fixture
(whole process, interleaved): delay 0 -> 5.5M / 5.1M faults, 33 / 19 s sys;
delay 1000 ms -> 1.3M / 1.6M faults, 14 / 17 s sys. Walls were drowned by
load; peak memory under the longer delay not measured.

## 2. Memory

### Bytes per item (`[mem]`, the mod fixture f131; allocated capacity)

| per | item | B |
|---|---|---|
| input row | frontier row (`Rt2` columns + ids) | 86.4 |
| | `cells` 4, `order` 8 (a `usize` per live lane), drop notes 12.1 (an atomic 4 + a COPY of the ids 8), unit `sources` 7.9 | 32 |
| new state (= request = claim) | the copied row (`RowBuf`, its columns' doubling slack included) | 121.1 |
| | the claim (`FxHashSet<(u64, u32, Key, u32)>`, 32 B + ctrl at ~60% load) | 62.5 |
| | the request (16 B + slack) | 24.5 |
| | later: the translation's new state (24 B), the gathered layer row (85.1) | |
| lid | `Lid` 49.3 (32 B + slack), owner 8, starts + transfer ranks ~9 | ~66 |
| unit | the row buffers' skeleton copies | 27.5 KB |
| edge | nothing anonymous: blocks stream to the workers' `.blk` files | 0 |

The same rates on the other fixtures (r62h57: rows 118.6, claims 41.1,
lids 48.1, requests 23.5; r62h, r42n, r10 alike). The sum of the wave's
structures matched the measured growth (mod f131: 4.99 GB summed against
4.93 GB of phase peak growth).

### The live f155 frame (116.6M rows in, 129.9M new states, 78.0M lids, 28,235 units)

| | measured | accounted (allocated) |
|---|---|---|
| start | 14.23 GB | frontier 116.6M x 86.4 = 10.1 GB; visited set 6.2 GB (the `[fwd]` "in 7.93" is `Block::bytes`'s estimate, without ids and slack) |
| wave growth | +33.96 GB (48.19) | copied rows 15.7 (42%), claims 8.1 (22%), lids 5.2 (14%), requests 3.2 (9%), per-row vectors 3.7 (10%), skeletons 0.8, worker transfer tables ~0.3 = 37.0 GB |
| translation | | claims and the frontier freed (-18.2); new states 3.1, `unit_reqs` 1.0 transient, visited +0.8 |
| layer | peak 51.23 | the new layer 129.9M x 85 B = 11.1 GB while the units' rows still live; owner index 16 B a lid (1.25 GB) |

Allocated runs ~10-15% above resident (capacity slack never touched). At
f160 (199.4M rows, 175.1M new states, 109.8M lids) the same rates give
26.4 GB at the start and 51.3 GB of growth against the measured 22.1 GB
and +45.0 GB (peak 71.1 GB).

So two thirds of the wave's growth is per NEW STATE (208 B: row copy,
claim, request), 14% per lid, 10% per input row.

## 3. In-kernel exact-key packing

**What sharing there is** (`[keydiag]`, counts per frame):

| frame | key fields per emission | (body, field) evals -> distinct (root, cell) per slice | field-lanes: big dicts / bools | transfer lanes -> distinct (root set, lane) per slice |
|---|---|---|---|---|
| mod f131 | 16.5 | 983.6M -> 214.3M (4.6x) | 32.2% / 11.9% | 753.7M -> 69.5M (10.8x) |
| r62h57 f57 | 16.0 | 284.7M -> 50.9M (5.6x) | 31.2% / 12.5% | 257.7M -> 21.8M (11.8x) |
| r62h f51 | 16.0 | 5.6x | | 11.2x |
| r42n f61 | 15.9 | 4.5x | | 8.2x |
| r10 f56 | 16.0 | 4.4x | | 9.6x |

A slice (16 lanes) of the mod room runs 159 bodies, 59 of them take a lane
(12.5 lanes each), with 952 (body, field) key evaluations but only 207
distinct (root, cell): bodies share their output slots (one slot per
distinct root node, `build_one_shape`), and `keys16` redoes each shared
field for every body.

**The subtleties, concretely:**

1. **Dictionaries are data, not code.** They grow only at a frame's end
   (`finalize_keys`/`add_codes`) and are read-only during a wave, but the
   kernels are assembled once at startup (407 kernels, ~6-16 s): the
   tables must reach the kernel through `AsmCtx` per frame (a generation),
   never as immediates.
2. **A miss is "new", and only Rust can name it.** A code absent from its
   dictionary makes the state provisional (`MissKey`: the partial key, the
   missing (cell, code)s, interned in `Provisional`). The kernel can only
   return a miss mask; every missing lane takes today's slow path. A shape
   the key space lacks (`sk` None) misses every lane; an outcome whose
   uniform part misses (`PartKey::missing`) makes all its lanes provisional.
3. **Layouts move.** A field's bits are append-only RUNS
   (plans/exact-keys.md): a grown field gets a second run elsewhere, so
   `place(idx)` is a loop over 1-3 runs whose table changes per frame;
   `FieldDict` keeps every code's placed bits precomputed for that reason.
   In-kernel placement needs the run table as data (or the kernel returns
   INDICES and Rust places them).
4. **Big dictionaries need a vector hash probe.** 32% of field-lanes are
   numbers of fields with more than 4 codes (`NumSlot` open addressing):
   in a kernel that is gathers (`vpgatherqq` x2 per 16 lanes per probe
   step) and a probe LOOP with a data-dependent trip count - today's
   kernels are straight-line code (`mix` counts dynamic instructions as
   slices x static). Bounding the probe and flagging overflowing lanes as
   misses would be sound only if the Rust side then looks them up exactly.
5. **Intervals and unknowns.** A code is (lo, hi) compared jointly (`Num(n)`
   and `Ival(n, n)` are one code: a Num root compares lo = hi); booleans
   are tri-state (value, known) indexing three entries, unknown only where
   an output widening may write it (`may_unknown`).
6. **The position-free key and the cell.** A position field keys its
   FRACTION (`x & !POS_WHOLE`; an interval as both bounds less `whole(lo)`),
   the cell carries the pixels and comes from `cell_out` (4 sources incl.
   room x/y). Both are cheap vector ops and could move in together.
7. **Register pressure and output size.** 207 distinct key fields a slice,
   each ~20-40 instructions as SIMD (load table base, hash, 2 gathers,
   compare, blend, miss mask): ~5-8k instructions, about the kernel's whole
   body today (7,264 a slice on the reference frame) - kernel time 5% ->
   ~10% of the wave - and 207 x 64 B of extra outputs (u32 indices) a
   slice. Composing whole 128-bit keys per (body, lane) in the kernel would
   write 159 bodies x 256 B a slice, 63% of them for bodies that take no
   lane.
8. **The dup mask** compares bodies' roots (key fields included); unchanged,
   but with indices from the kernel it could compare (key, cell, transfer)
   instead.
9. **KEY_CHECK** checks results, not the path: `key_check` recomputes the
   boundary row's key in the frozen dictionaries for every emitted row and
   compares it (packed, or the provisional content). It covers an
   in-kernel path as it is; add a differential mode (kernel indices vs
   `keys16`, including miss masks, every emission) and a unit test over
   multi-run, grown, missing, interval, unknown-bool and position fields.
   A wrong index merges two states silently: this is a soundness surface,
   not only speed.

**Gain.** `keys16` is 21% of the wave (~25% of `run_slice` inclusive), of
which ~18 points are lookups and ~3 the per-lane ORs that any scheme keeps.
A **per-slice memo in Rust** - placed bits per distinct (root, cell),
computed once for the union of the takes, ORed per body - cuts the lookups
4.5x: **~14% of the wave**, no kernel change, no new soundness surface (the
same lookups, fewer times). In-kernel packing would save the remaining
~4 points of lookups minus 1-2 points of added kernel work: **~2-3% over
the memo**, for the largest effort and risk here. Do the memo; revisit
in-kernel packing only if keys stay on top after it.

The same memo for the TRANSFER is bigger and simpler: a body's transfer
roots (10 slots + fin) are shared 8-12x per (root set, lane); a per-kernel
root-set id at build time and a [sets x 16] memo of transfer ids per slice
would remove ~18 of the 19.5 points (**~17% of the wave**).

## 4. Buffer reuse against allocation churn

Reallocated per SLICE: `input_views` (a `Vec` per 16 lanes; 0.1%).

Reallocated per UNIT (1024-4096 lanes):
- `encode_block`'s `out` and `starts` (`out` freed after the write to the
  block file: 10.7% of all faults);
- `sort_at.clone()` (`cur`, lids x 64 cells x 4 B);
- the unit's `RowBuf`s: a skeleton `Rt2` clone per shape (27.5 KB) and
  every column grown from empty by doubling (`self.bufs = Vec::new()`);
- `lids`, `requests`, `sources`, `owners`, `used`, `pending`: replaced or
  taken (`mem::replace(.., Vec::with_capacity(n))`, `mem::take`), so the next
  unit regrows them (they move into `UnitOut`, so they cannot simply be
  kept);
- `run_chunk`'s `by_region` (and a sort of it).

Reallocated per FRAME: the sinks themselves (each worker's `xfer_ids` map
and 4096-slot cache rebuilt from nothing), `cells`/`order`/`DropNotes`
(32 B a row, built serially), claims shards growing by rehash from empty,
`unit_reqs`/`items`/`news`, the new layer's columns, and the checkpoint's
decode buffers (a thread per piece).

Effort and gain: the per-worker ones (`out`, `starts`, `cur`) are an hour
each; the unit-owned ones vanish with edges-target. Kernel mode is 9.9% of
the wave's cycles (faults, purges, TLB shootdowns, the zone lock); keeping
the per-unit buffers and a non-zero purge delay should take most of it:
**~5-8% of the wave**, plus **~3-4 s a frame** from the checkpoint (a
`par_map` over `threads()` workers reusing decode buffers instead of 1,341
threads) and **~2 s** from parallel frame setup. The purge delay's memory
cost must be measured on a big frame first.

## 5. What edges-target changes (lead's 8dcaa56 + uncommitted work at 15:31)

Built: units OR each emission into a unit-local bundle table
(`UnitBundles::insert`: route, source key, transfer, offset, target key ->
mask); per SLICE (1M lanes) the chunks go to a REGION PHASE per route
(dedup against the region's table, bundles merged, zstd segments); the
frame's end makes keys final, numbers entries and DECODES the new layer.
Deleted: lids, owners, owner index, claims, requests, translation,
`resolve_lids`, unit blocks, layer gather.

**Measured** (quick builds, r62h57 f56, 8 threads, interleaved, load
31-46):

| | units | translate / regions | layer / end | total |
|---|---|---|---|---|
| develop | 7.2-8.7 s | 0.24-0.36 s | 0.31-0.68 s | 8.4-9.7 s |
| edges-target | 5.9-7.8 s | 4.4-5.7 s | 0.3-1.0 s | 12.0-14.7 s |

`[phases]` of edges-target: emit 48.9 worker-s, unit end (chunks) 3.9,
**regions 25.5** (~600 ns a bundle, 42.5M bundles), kernel 1.1. In the
units, `UnitBundles::insert` + drain + its sort are ~12% where develop's
lid, emit, claims, row copy and unit end were ~30%.

| | today (share of the wave / frame) | after edges-target |
|---|---|---|
| sink `lid`, claims, `find`, lid masks, requests | ~13.6% | gone: one bundle probe + OR (~9% of the units) |
| row copy (`push_row`) | 5.6% | gone (only the decode guard, once per shape and worker) |
| unit end (sort, encode) | 9.7% | drain + sort by route (~3%); the encoding moves to the region phase |
| translation | 8.8-22 s a frame | gone; the region phase replaces it, today 4.4-5.7 s on r62h57 (half the units' worker time): **the new bottleneck** |
| layer gather 2.2 s, owner index + edge file ~2 s | | layer decoded from keys (150-240 ns a row single-threaded, parallel: ~2 s at f160), segments already encoded |
| per-state memory: rows 121 + claims 62.5 + requests 24.5 B; lids ~66 B | 32 GB of f155's 34 GB growth | gone; added: `Sources` 32 B a row (6.4 GB at f160), a slice's bundles (~0.5 GB) and route tables. Expected peak at f155 ~25-30 GB (visited 7 + frontier 10 + `Sources` and
the per-row vectors ~6.5 + slices; or the decoded new layer, 11 GB, at the
end), not 51 |
| keys 21%, transfer 20%, level -1 4%, cell 4%, dup 3%, pos 3%, kernels + packing 7% | ~62% of the wave | unchanged, so ~75-80% of the units |
| `XferTable::merge` | ~5 s serial at the frame's end (f160) | **now PER SLICE and still non-incremental** (below) |
| checkpoint (thread per piece + decode guard), frame setup | ~7 s a frame | unchanged |
| frontier rows | 86 B a row (10-17 GB) | unchanged (decode per unit would make it 28 B: rows-from-keys' follow-up) |

**Roadblock for the redesign on the mod room: the per-slice transfer
merge is quadratic.** `run_wave` calls `xfers.merge(edges_dir, &tabs)` per
slice with `tabs = sinks.iter().map(|k| k.xfer_tab.clone())` - each
worker's WHOLE table so far - and `merge` hashes every pair twice
(`contains_key`, then `index[p]`) against the tree's table (9.55M pairs,
248 MB, at h168) on one thread. On develop one merge per frame already
costs ~5 s at f160 (`get_inner` + `merge`, 91-96 s in the frame's
timeline). At f160 the frame has ~190 slices of 1M lanes, so the slices'
merges sum to ~S/2 times the full merge if the tables grow linearly
through the frame: ~95 x 5 s, minutes a frame - more than the 77 s wave. Fix: merge only each
worker's NEW pairs since the last slice (`tab[merged..]`, appending to its
remap), look them up in parallel, insert serially - linear in the frame.

## 6. Recommendation: the levers left after the redesign, by priority

| # | lever | expected gain | effort |
|---|---|---|---|
| 1 | Incremental, parallel `XferTable::merge` (per worker: only new pairs; one lookup a pair; parallel lookups, serial insert) | edges-target: removes a quadratic serial term (minutes a frame at f160); develop: ~5 s of 106 s | hours |
| 2 | The region phase's cost (edges-target): 25.5 worker-s on r62h57, ~600 ns a bundle - profile it (`run_route`'s probes into a table sized `records/2` up to 2^26, segment sorts 15%, `Map::fold` 23%) and the critical path (278 routes on 8 workers, 70% efficiency) | it is the frame's largest phase after the units; bring it under the translation's ~0.3 s here | days |
| 3 | Per-slice transfer memo: per-kernel root-set ids, [sets x 16] transfer ids a slice | ~17% of the wave (10.8x fewer `words` + cache lookups) | 1 day |
| 4 | Per-slice key-field memo: placed bits per distinct (root, cell) for the union of takes; misses keep the slow path | ~14% of the wave (4.5x fewer lookups) | 1-2 days |
| 5 | The same memo for `cell_out` and the level -1 check (per (pos sources, lane)); a small per-worker cache before `pos_edges.insert` | ~5-7% of the wave | hours |
| 6 | Checkpoint: `par_map` over `threads()` with reused decode buffers instead of a thread per piece | ~3-4 s a frame; 40% of the page faults | hours |
| 7 | Frame setup in parallel (`cells`, `order`, drop notes; drop the ids copy and the `usize` order) | ~2 s a frame; ~1.5 GB at f160 | hours |
| 8 | Allocation churn: per-worker reuse of the remaining per-unit buffers; `MIMALLOC_PURGE_DELAY` measured on a big frame (faults 3.5-4x fewer at 1000 ms, peak RSS unknown) | most of the wave's 9.9% kernel mode (~5-8%) | hours + a measurement |
| 9 | Frontier as (key, cell, id) decoded per unit for the kernels | 86 -> ~28 B a row (~11 GB at f160) for ~200 ns a frontier row (1/45 of the emissions) | days |
| 10 | In-kernel exact-key packing | ~2-3% of the wave on top of #4 | weeks, new soundness surface |

With #3-#5 the per-emission work that remains after edges-target (keys,
transfer, cell, level -1: ~49% of today's wave) drops by roughly 70%: the units should go from ~0.8x today's wave (edges-target alone)
to ~0.45-0.5x. Measure each on the mod fixture below (instructions per
emission is the load-independent number: 1,725 today).

## Reproducing

- The mod fixture: `cp -al /var/tmp/mod-ck/mid155/level00 D` and
  `/var/tmp/mod-target/release/rewrite bench-frame --level-dir D --frame
  130 --room 0,0 --level r0sxhne --level-minus-one 155,5 --win-at
  56..72,104..112 --cart /var/tmp/mod/taslevel.p8 --edges --threads 8
  --phases --l1-table T` (develop has no `--cart`: map-mod's binary; the
  diagnostics cherry-pick onto it cleanly). Its `[fwd]`-equivalent line
  equals the live f131.
- `[mem]` and `[keydiag]`: any `bench-frame`/`forward` with `--phases`
  (`[keydiag]` costs time: its counters run per slice).
- Raw data: `/var/tmp/fwdperf/` (live f160 cycles, page faults and
  interval counters; the fixture profiles; the resolved inline chains
  `*.json` for `tools/perf_inline.py`).
