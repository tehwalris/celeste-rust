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

## 4. Constants rematerialized

- **A constant (`BcastD`) is rematerializable like an input load**: spilled,
  it takes no slot and no store, and each use recomputes it - `vpxord` for
  zero (the zeroing idiom), else `vpbroadcastd` from the pool. Not
  `vpternlogd $0xff` for all-ones: it takes a logic port (the kernels'
  busiest) and depends on its destination; a broadcast is a load, like the
  reload it replaces.
- **The reload pass also drops a repeated constant** (`constant_key` in
  `drop_redundant_reloads`): without it rematerialization cost MORE
  instructions than the reloads it replaced (repeated reloads of one slot
  into one scratch were already dropped).
- **Tried and dropped: making the allocator evict a rematerializable value
  first.** 2.66064G against 2.65981G dynamic instructions without it: no
  gain, more code.
- Result: a wash in instructions (-0.4%); kept because it is exact, smaller
  (no slot, no spill store for constants) and the brief asked for it.

## 5. Spills: no allocator work

- After (1)-(4) spill + reload are 21.6% of the kernels' remaining dynamic
  instructions, but the kernel phase is ~2% of the reference frame's wave
  worker-time (9% before). Even removing every spill would buy ~0.4% of the
  wave. The reloads are of long-lived values far from their spill (~90%
  more than 32 instructions after it), which only interval splitting plus a
  farthest-next-use policy would address: a real allocator, for nothing
  measurable. Written down instead (plans/kernel-mix.md "After").

## Overlap with `storage-v2`

None in code: the changes are in `transpile/graph.rs` (the evaluator),
`transpile/ival.rs` (one call), `transpile/lower.rs` (one call, a
diagnostic) and `transpile/asm/codegen.rs` + its tests; `compiled/mix.rs`
(the diagnostic from `kernel-mix`) gained one classification line. Nothing
touches the frame loop, the door, the edges, or `asm_kernel.rs`'s key
computation (`key_words16`). The kernels' inputs, outputs and buffer
layouts are unchanged, so the storage branch's key change composes.

## Region size (Philippe's addition): kernels stay at 16 px

Measured 8 against 16 (plans/kernel-mix.md "Kernel region 8 px against 16
px", BENCHMARK_DATA.md): identical outputs, -20% instructions per slice at
8, no measurable wave difference (the kernels are ~2% of the wave), and
3.2x the kernels with +1.4 s (room (1,0)) to +5.6 s (room (6,2)) at every
process start. Recommendation: storage at 16 to match; the default is NOT
changed. If storage must be 8, kernels at 8 are safe and cost only startup.

## Not done / follow-ups

- **`tile_flag_at` inline** (a precomputed per-(box, flag) bitmap and a
  gather instead of the call-out): after (1)-(3) the callee is about half of
  the remaining kernel time. Small in the wave (~1%).
- **The select's register move** (`vmovdqa64` before every `vpternlogd`
  select, 13% of the remaining instructions): freeing a dying operand's
  register for the result would let the select permute its LUT instead. On
  CPUs that eliminate zmm moves at rename it is free; not measured.
- **Spills**: see 5.
- The emission (`emit`, ~70% of the wave's worker-time, 87 lane emissions
  per input row) is where the reference frame's time is now.
- Steps 3 and 4 were not perf-profiled (perf's mmap failed while another
  agent profiled); their cycles are the `[phases]` kernel phase.
