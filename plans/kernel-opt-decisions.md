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

## 2. Decided planes folded in the codegen

- **Folded on `MaskVal`, at lowering** (`Lower::m_and`, `m_or`, `m_andn`,
  `m_xor`, `m_not`, `m_sel`, `n_sel`), not as a peephole over the emitted
  stream: a constant plane never becomes a register, so neither the
  broadcast nor its spill exists. Every rule is a bit identity (`x & -1 =
  x`, `x | -1 = -1`, `x & 0 = 0`, `x & x = x`, `x ^ x = 0`, `c ? t : t = t`,
  a select with a constant arm as and/or/andn), so the folded kernel's
  output is the same BITS in every lane - including the `val` plane on
  lanes whose `known` is false, which nothing reads but which the test
  checks anyway (`asm_bool_layer_with_decided_planes_is_bit_exact`).
- **The select's known plane** stays `c ? tk : fk` (ignoring `c`'s own
  known plane), exactly as before: an undecided condition is the select's
  own error, elsewhere.
- `codegen::foldable` (the mix diagnostic's estimate) is kept: it now
  measures what bit identities would STILL remove (near nothing), which is
  the check that the fold is complete.
