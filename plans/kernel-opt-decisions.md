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

## 3. Call-outs: live registers only; `/2^s` and `%2^m` inline

- **Liveness from the allocator's own intervals** (`codegen::call_saves`):
  a register is saved around a call iff it holds a vreg defined before and
  read after it. `allocate` never splits an interval, so a vreg's home is
  its home for its whole life and nothing else is in that register
  meanwhile: this is exact, not a heuristic. The scratches (zmm26-30) hold
  nothing across an instruction; `ZERO` (zmm31) is re-zeroed after a call
  when a `Neg` follows instead of saved.
- **Beyond the brief: values carried across a RUN of calls.** The hot kernels
  call `tile_flag_at` back to back (the player's `is_solid` probes); a value
  live across consecutive calls and not read between them stays in its
  save slot - not restored after the first, not saved again before the
  next. Measured: call-out marshalling 910M -> 430M dynamic instructions on
  the reference frame. Exact for the same reason (one vreg per register
  while live; one save slot per register, written only by that register's
  saves). Tested with a mutation: carrying unconditionally fails
  `values_held_across_a_run_of_calls_survive_it`.
- **`/ 2^s` inline only for `0 <= s <= 14`** (raw divisor `2^16 ..= 2^30`):
  `Pico8Num`'s `/` truncates toward zero and saturates on overflow; for a
  divisor >= 1 nothing overflows, and truncation is the biased arithmetic
  shift. Smaller divisors (`/0.5`, which can saturate) stay call-outs.
  `%` by any positive power of two `2^m` (raw, `m <= 30`) is the low-bit mask
  (`rem_euclid`), negatives included. Interval `/` by a power of two:
  per endpoint, as the call-out did. Checked bit-exact against `Pico8Num`
  over the edges of the i32 range (MIN, MAX, 0, +-1, every +-2^k and its
  neighbours) and random raws (`inline_div_rem_by_a_power_of_two_is_pico8s`).
- The stack frame keeps its 32-register save area (unchanged layout; only
  fewer moves).
