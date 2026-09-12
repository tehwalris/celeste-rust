# Buckets: the packed frontier and emission-time provenance (2026-09-12)

Agreed with Philippe 2026-09-12. Replaces the per-(shape, pm1, cell) block
regrouping in `src/frame.rs` and the per-chunk accumulate/boundary/merge in
`src/compiled/`. Measured motivation (room (1,0) forward to f40/f44, release):
the assembled kernels are **5%** of wall; ~55% is bookkeeping around them -
115k kernel calls of 17 rows (39% of executed lanes are padding), a structure
clone per call and per partition group, the row key hashed per row per field
in scalar Rust and then hashed again by `boundary`, three rounds of
regrouping, and 17k checkpoint files per frame.

## Invariants

1. **The unit of storage is the unit of kernel invocation.** The frontier is
   a set of BUCKETS keyed by what the kernel is dispatched on: the output
   shape. A bucket is one packed `Rt2`; one kernel call per bucket.
   Position is a column, not part of the key. (Until 2026-09-12 the key
   also carried a CLASS - freeze global, moving key, pm1 cells - inherited
   from the generated kernels' uniformity premise; the fused ASM graph
   resolves those per lane, and dropping it reproduced every gate with
   44 calls for f0-f44 instead of 879.)
2. **Rows are routed on emission, never regrouped.** The kernel's append step
   knows each emitted row's outcome (per body), so it appends the row
   straight into the NEXT frame's bucket.
   `regroup_and_merge`, `partition_pm1`, `frame::regroup`,
   `collapse_uniform_cols`, `clone_block`-per-group: gone.
3. **Canonicalization is precomputed per outcome shape.** The accumulator
   template IS the canonical structure; `boundary` per output block is
   replaced by a debug assertion. The row key is computed once, in the
   append step, as the exact boundary key: `mix64(part + h)` with `part`
   per outcome (shape hash + uniform/konst cells) precomputed at build.
4. **Provenance is consumed at emission and never stored.** In the append
   loop the source lane of an emitted row is the slice bit being iterated.
   Forward: record the pos-graph edge `(cell_in[src], cell_out(row))` there,
   before dedup. Backward: pass the target set in; if `(key, cell_out)` hits,
   set bit `src` in a bitset over the call's INPUT rows; outputs are never
   materialized. The within-call `seen` slot carries one bit, "hit a
   target", so a re-emission of a hit row from another lane marks that lane
   too (the set-union the design needs, as one bit per key).
5. **Dedup happens once, at the door.** `visited.insert(key, cell)` decides
   whether an emitted row is appended at all. `seen` stays as the cheap
   pre-filter for same-source re-emissions.
6. **Checkpoint per (frame, shape)**, rows sorted by `(cell, key)` with a
   cell -> row-range index and raw fixed-width columns, so the backward
   loads "rows in these cells" as range copies out of an mmap. The
   partition is the search's own: shape (dispatch + row format), cell
   (locality), frame (write time).

## Gates (every stage must hold all of them)

- `rewrite forward --to 44 && rewrite ckhash --to 44` ==
  `gates/ckhash_room10_f000-044.txt` (frontier sets, all 45 frames).
- `rewrite forward --to 44` pos-graph fingerprint (pairs, hash) ==
  `gates/posgraph_room10_f044.txt`.
- `rewrite search --from 29 --to 35 --maxk 1 --win-at 9,101` marked-set
  fingerprints per (horizon, level) == `gates/marks_room10_win9-101_h29-33.txt`
  (the backward on a real fan-out, across horizons; must end in
  `OPTIMAL win frame: 33`, which the unfiltered Exact forward's first win
  confirms). Re-pinned 2026-09-12 when won rows stopped being expanded:
  h32/h33 levels 0-1 lost 18/36 marks with IDENTICAL re-run counts - the
  post-win rows that re-entered (9,101) and counted as seeds, nothing
  else - and the Exact lines and the optimum are unchanged.
- `cargo nextest run --cargo-profile quick` green; `kernel lanes: missed 0`.

## Stages

1. DONE `a2cb8fd`. Exact key in the append step (`part` per outcome),
   canonical template asserted, `boundary()` per acc removed.
2. DONE (with 3). Buckets: frontier = map (shape, class) -> Rt2; one call
   per bucket; rows routed into next-frame buckets after emission
   (`frame::route` / `Rt2::append_rows`); the regroup/merge/partition
   machinery deleted. Room (1,0) f0-f44: 53 s -> 26 s, 115k calls -> 879,
   0.4% padding. The class went 2026-09-12 (invariant 1): 44 calls.
3. DONE. Emission-time pos-graph edges (`ForwardSink::edges`, the tagged
   `RowSet` for re-emissions from another cell) and the door dedup inside
   the append step (`ForwardSink::visited`).
4. DONE. Wide backward: `ForwardSink::backward(targets, width)` runs every
   unmarked candidate row of a bucket in one call, materializes nothing,
   and reports a hit flag per input row (the `RowSet` tag is the hit bit
   for re-emissions). Marks and re-run counts identical to the per-lane
   draft, which is deleted.
5. DONE 2026-09-12. Checkpoint per (frame, shape) with a cell index
   (format v8); the backward loads by cell range and seeds from the win
   list in each file's header. Forced by the first real-horizon run: the
   H=89 level-0 backward spent 35 s per iteration decoding every layer
   <= i (2.5 GB of zstd bincode, ~100M rows) to keep a few hundred rows,
   ~27 min per horizon against ~1 s of actual re-runs.
6. DONE. The row key as two kernel roots per body (`Op::CellMix`/`AddW`/
   `Word`, lowered to 64-bit lane ops + `mix64` in AVX-512); the append
   step reads `(h1, h2)` off the output buffer and nothing hashes a row in
   Rust. Gated per row by `CELESTE_KERNEL_KEY_CHECK=1` (kernel key ==
   `Rt2::boundary` key over f0-f44) and by `asm_cell_mix_matches_the_
   boundary_cell_mix` against the scalar `cell_mix`. f44: 4.8 s -> 2.7 s.

## Transition points that remain on `State`

The initial state (`RefEngine::initial_state` -> one bucket) and the
`RefEngine` oracle (bridge at its edge). `MarkFilter` widens on the columns
(`Rt2::widen_to`) since 2026-09-12: exporting whole buckets through `State`
reached 62.8 GB in the level-1 forward at H=55.
