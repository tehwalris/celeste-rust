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
   a set of BUCKETS keyed by what the kernel is specialized/partitioned on:
   `(output shape, class)` with class = the values of the freeze global, the
   moving key and the pm1 cells. A bucket is one packed `Rt2`; one kernel
   call per bucket. Position is a column, not part of the key.
2. **Rows are routed on emission, never regrouped.** The kernel's append step
   knows each emitted row's outcome (per body) and class values (output
   fields), so it appends the row straight into the NEXT frame's bucket.
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
6. **Checkpoint per bucket**, rows sorted by cell with a cell -> row-range
   index, so the backward loads "rows in these cells" as range reads.

## Gates (every stage must hold all of them)

- `rewrite forward --to 44 && rewrite ckhash --to 44` ==
  `gates/ckhash_room10_f000-044.txt` (frontier sets, all 45 frames).
- `rewrite forward --to 44` pos-graph fingerprint (pairs, hash) ==
  `gates/posgraph_room10_f044.txt`.
- `rewrite search --from H --to H --maxk 1 --win-at X,Y` marked-set
  fingerprints per level == `gates/marks_room10_*.txt` (the backward, on a
  horizon with real fan-out).
- `cargo nextest run --cargo-profile quick` green; `kernel lanes: missed 0`.

## Stages

1. DONE `a2cb8fd`. Exact key in the append step (`part` per outcome),
   canonical template asserted, `boundary()` per acc removed.
2. DONE (with 3). Buckets: frontier = map (shape, class) -> Rt2; one call
   per bucket; rows routed into next-frame buckets after emission
   (`frame::route` / `Rt2::append_rows`); the regroup/merge/partition
   machinery deleted. Room (1,0) f0-f44: 53 s -> 26 s, 115k calls -> 879,
   0.4% padding.
3. DONE. Emission-time pos-graph edges (`ForwardSink::edges`, the tagged
   `RowSet` for re-emissions from another cell) and the door dedup inside
   the append step (`ForwardSink::visited`).
4. DONE. Wide backward: `ForwardSink::backward(targets, width)` runs every
   unmarked candidate row of a bucket in one call, materializes nothing,
   and reports a hit flag per input row (the `RowSet` tag is the hit bit
   for re-emissions). Marks and re-run counts identical to the per-lane
   draft, which is deleted.
5. Checkpoint per bucket + cell index; backward loads by cell range. Gate:
   ckhash + marks.
5. Deferred: the backward reads whole bucket files and filters rows by
   cell (`load_frame_cells`); a per-bucket cell index becomes worth it only
   if a profile of a deep backward shows the load dominating.
6. DONE. The row key as two kernel roots per body (`Op::CellMix`/`AddW`/
   `Word`, lowered to 64-bit lane ops + `mix64` in AVX-512); the append
   step reads `(h1, h2)` off the output buffer and nothing hashes a row in
   Rust. Gated per row by `CELESTE_KERNEL_KEY_CHECK=1` (kernel key ==
   `Rt2::boundary` key over f0-f44) and by `asm_cell_mix_matches_the_
   boundary_cell_mix` against the scalar `cell_mix`. f44: 4.8 s -> 2.7 s.

## Transition points that remain on `State`

The initial state (`RefEngine::initial_state` -> one bucket), the `RefEngine`
oracle (bridge at its edge), and `MarkFilter` at levels >= 1 (re-widening
through `State`; an `Rt2` widening later).
