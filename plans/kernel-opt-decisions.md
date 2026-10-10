# Kernel optimizations: decisions to review (branch `kernel-opt`, 2026-10-10)

The simplifications of plans/kernel-mix.md, implemented in order, each
verified byte-identical against `fg-2300` (`cc83334`). Each entry: what was
decided, and why. Measurements: plans/kernel-mix.md ("After") and
BENCHMARK_DATA.md.

## 1. `spikes_at` folded by range

- **A set reading of `mget`, not a `SpikesAt` intrinsic.** The intrinsic would
  also remove the unrolled loop's end checks, but it changes the TRACE (no
  loop, different error terms, different fork structure), so the kernels
  would no longer be the same program and byte-identity with fg-2300 would
  be luck, not construction. The set reading only decides nodes the existing
  interval fold (`transpile::ival`) already visits, exactly; the traced graph
  is untouched. The loop-end terms stay (they are cheap once the tile tests
  are constants: the loop's own `flr`/`min` arithmetic).
- **Only the lowering's fold reads the set** (`Graph::eval_fold_in`).
  `verify::Points` (the tracer's platform points) and level -1
  (`eval_narrow_top_in`) keep the old reading, where `mget` over an interval
  is TOP. Both would be exact with the set too, but they change the trace and
  the level -1 table respectively, so the outputs could move (an
  over-approximating kernel built from a differently forked trace keys
  different rows). Out of scope for a byte-identical step; a follow-up if
  wanted.
- **`Eq(k, mget)` is decided only against a literal k.** Against another
  interval the old hull comparison stands (no case in this program).
- **Non-integer coordinates.** `mget` of a fraction raises in the kernels
  (`zn_mget`: `as_i16().expect`) and in `CartData::mget`; the set covers
  `flr(lo) ..= flr(hi)`, which holds every integer of the hull, and the
  floor of any fraction in it, so it is a superset either way.
- **Out-of-map coordinates** read 0, as `mget_whole`: the rectangle is
  clipped to the map plus one "outside" row/column per side.
