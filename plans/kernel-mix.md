# What the kernels spend their instructions on (2026-10-10)

Room (6,2) 100% (2300m), level `r0sxhf`, `CELESTE_HUNDRED=1
CELESTE_LOADING_JANK=2 CELESTE_LEVEL_MINUS_ONE=94,5`, the reference frame
f56 -> f57 of `/var/tmp/canon-h62-f56` (branch `fg-2300`, `cc83334`). Analysis
only: nothing in the kernels changed.

## How it was measured

`CELESTE_KERNEL_MIX=DIR` (`src/compiled/mix.rs`, branch `kernel-mix`):
the codegen records, per SSA instruction, the fused-graph node it was lowered
from (`Compiled::prov`, a `#@k` comment line before its machine lines; no
effect on the machine code, empty when off), and per kernel the report
writes `<sym>.s/.so/.lines/.tsv/.guards`; `calls.tsv` at the end counts the
slices each kernel ran. `tools/kernel_mix.py DIR [--perf perf.data]`
aggregates. A kernel is straight-line code, so its dynamic instruction count
is EXACTLY slices x static (call-outs' callees excepted, measured by perf).
Cycles: `perf record -F 8000` on the same run, samples mapped by instruction
index (objdump order = `.lines` order, checked equal per kernel).

    MIXDIR=... PERF=1 /var/tmp/kmix-run.sh     # tools/bench_step.sh's copy + forward --to 57
    tools/kernel_mix.py /var/tmp/kmix-out3 --perf /var/tmp/kmix-perf.data
    tools/kernel_mix.py DIR --snippet SYM --at K --len N

Categories of a machine instruction: **a** numeric (add/sub/min/max/abs/neg,
the 16.16 multiply weave, floor masks); **b** boolean (vpcmpd, the
`vpmovm2d` mask->vector it needs, and/or/xor/andn/ternlog logic, the 0xca
ternlog selects); **c** movement/overhead (input loads, broadcasts, spills,
reloads, register moves, the call-out marshalling, the frame); **d** the
root stores. Role class of the node it came from: `value` (reaches only
fields/transfer roots), `guard-E`/`guard-L`/`guard-EL` (only the bodies'
error/live roots), `shared`.

The frame: 1432 calls, 5.86M rows, 366k slices, 86 of 306 kernels ran; 87.4
lane emissions per row, 58.4% of (body, slice) evaluations take a lane (as
before). Kernel phase 4.7-6.2 worker-s of ~58 (quick build, 16 workers).
ONE shape dominates: `0x71e1d8019968708a` (player + one object, 11 outcomes,
208 bodies, 16 distinct `live` and 16 distinct `error` roots), its region
variants are all of the top 12 kernels (34-58k instructions each, 1300-1500
spill slots).

## Task 1: the instruction mix

| | static (3.31M insts, 86 kernels) | dynamic (16.4G insts) | perf cycles (30.7k samples in kernel bodies) |
|---|---|---|---|
| a numeric | 1.0% | 1.0% | 0.7% |
| b boolean | 33.1% | 32.1% | 26.1% |
| c movement | 64.3% | 65.1% | 70.9% |
| d stores | 1.7% | 1.8% | 2.3% |

Dynamic detail: reload 32.9%, boolean logic 28.1%, call-out marshalling
17.2%, spill 11.6%, select 2.2% (+ its register copy 2.2%), field stores
1.2%, numeric 1.0%, compare 0.9% (+ mask->vector 0.9%), rematerialized input
loads 0.9%, error/live/transfer stores 0.6%, input loads 0.1%, constant
broadcasts 0.1%. Cycles follow: reload 28.5%, call-out marshalling 23.6%,
logic 23.2%, spill 16.4%.

By role: guard-L 35.6%, guard-E 28.7%, guard-EL 16.4% = **80.7% of the
dynamic instructions exist only for `live`/`error`**; value 10.4%, shared
7.0%, stores 1.9%. Cycles: guards 80.8%, value 9.8%, shared 7.3%.

**Call-outs** (42 tile_flag, 36 mget, 10.5 div, 10.6 rem per slice). Each
saves and restores all 32 zmm (~78 lines). In the profile's kernel window:
kernel bodies 30.7k samples, callees 9.6k (co_tile_flag 4.6k, co_mget +
mget_whole 3.2k, div/rem 1.8k). With the in-body marshalling (7.3k) the
call-outs are **~42% of kernel time**.

**Spills**: spill + reload = 44.5% of dynamic instructions (45% of cycles).
8% of the reloads (2.7% of all instructions) reload a spilled BROADCAST
CONSTANT (mostly the all-ones mask: only input loads rematerialize).
Constant materialization proper (vpbroadcastd) is 0.1%; embedded `{1to16}`
operands are on 1.2% of the instructions.

A typical tri-state `And` in a guard (hottest kernel `_243`; `FOLD` = removed
by bit identities, `SPIKE` = removed if `spikes_at` folds, below):

    6599 vpandnd %zmm29, %zmm21, %zmm23     # b.logic  guard-EL And/andn  FOLD SPIKE   (~pv & pk, pk all-ones)
    6600 vmovdqu64 11584(%rsp), %zmm30      # c.reload guard-EL And/andn  FOLD SPIKE
    6601 vpandnd %zmm13, %zmm30, %zmm26     # b.logic  guard-EL And/andn  FOLD SPIKE
    6602 vmovdqu64 %zmm26, 12288(%rsp)      # c.spill  guard-EL And/andn  FOLD SPIKE
    6603 vmovdqu64 12288(%rsp), %zmm29      # c.reload guard-EL And/or    FOLD SPIKE   (spilled, reloaded at once)
    6604 vpord %zmm29, %zmm23, %zmm18       # b.logic  guard-EL And/or    FOLD SPIKE
    6605 vmovdqu64 4096(%rsp), %zmm30       # c.reload guard-EL And/and   FOLD SPIKE   (4096(%rsp): the spilled all-ones)
    6606 vpandd %zmm13, %zmm30, %zmm26      # b.logic  guard-EL And/and   FOLD SPIKE

`Op::And`/`Op::Or` lower to SIX instructions (value + the five of the known
plane) even when both known planes are the constant all-ones: `mask_reg`
broadcasts the constant and the codegen does not fold. A call-out
(`spikes_at`'s `mget`) is ~78 lines of which 64 are the zmm save/restore:

    6218 vmovdqu64 10112(%rsp), %zmm30      # c.callout guard-EL Mget/call SPIKE  (args to the buffer)
    6222 vmovdqu64 %zmm0, 91200(%rsp)       # c.callout ... 32 of these, then the call, then 32 restores

## Task 2: the graph

The fused graphs of the 86 kernels: 341k nodes reached by roots (weighted by
slices 1.66G). By class: guard-L 31.3%, guard-E 27.2%, guard-EL 14.7%,
value 19.5%, shared 7.4% (weighted: 30.6 / 27.5 / 13.7 / 20.3 / 7.9). By
op (weighted): And 33.3% (guard), Or 21.3% (guard), Sel 19.2% (value 14.8,
shared 3.9), Not 6.0%, Eq 4.2%, arithmetic (Add/Sub/Min/Max/Mul) ~5%,
comparisons ~4%, Cell/Const ~2%, TileFlagAt 0.9%, Mget 0.8%, Restrict
0.1%, Lo/Hi 0.4%. Only ~5% of the graph is numeric.

Per body: 208 bodies a slice but only 16 distinct `live` and 16 distinct
`error` roots (bodies of one outcome share their guards; they differ in
fields). Each error root is an OR of ~50 terms; node-wise every guard node
is shared by several bodies (hash-consing works: no per-body duplication).

### Guard families

Error terms (`trace::error` own errors; classified by `mix::error_kind`):
per error root ~28 `pin` terms (the kernel's pinned inputs, `Not(Eq(c,
cell))`), ~16 `restrict` (`Lt(Lo(cell), lo)` / `Gt(Hi(cell), hi)`), 2-4
`contain` (a widened remainder against [-0.5, 0.5)), 2 `fork-cover`
(`SplitOk`), 1 unfinished unrolled loop, and in the 58k-instruction
variants ~16 `guarded-restrict` terms (a fused candidate's `live & error`,
`lower::specialize_frame` step 4, whose cost is its `live`). The terms are
many but CHEAP: pins and restrictions are cone-4 nodes, 0.35% of the
dynamic instructions together. The cost is in the big cones:

| guard-only instructions, dynamic | share of all |
|---|---|
| reached only from `live` | 35.6% |
| several families (`multi`) | 16.6% |
| `guarded-stated-other` (the unrolled loop's end, AND its site's guard) | 16.1% |
| `guarded-restrict` (fused candidates' `live & error`) | 9.0% |
| `contain` / `pin` / `restrict` / `fork-cover` | 0.5 / 0.25 / 0.1 / 0.1% |

By what the cone READS (leaf class: T tile_flag call, M mget call, F fork,
A arithmetic): guard nodes whose cone contains an `mget` are **68.3% of the
dynamic instructions** (guard-L 34.4, guard-E 22.8, guard-EL 11.2), and
`mget` appears in this program ONLY in `spikes_at` (`Eq(17|27|43|59,
Mget)`, 36 of each per hottest kernel; `tile_flag_at` is the `TileFlagAt`
intrinsic). `spikes_at` is the biggest guard family by far: a 2x2 unrolled
tile loop with four tile kinds, `%8` and `/8` call-outs and its loop-end
checks, evaluated at every post-move position of every path, feeding the
death outcome's `live`.

### Simplifications (estimates; nothing implemented)

1. **Fold `spikes_at` where the region cannot reach a spike** (or make it an
   intrinsic `SpikesAt` like `TileFlagAt`, with a range fold like
   `Graph::tile_flag_over`). Measured by constant-propagating every
   `Eq(k, Mget)` to false through And/Or/Not/Sel and taking what the roots
   still reach (`spikefold.*`): **64.6% of the dynamic instructions and
   65.6% of the kernel-body cycles go**, plus most of the `mget`/`rem`
   callees (~4.1k of 9.6k callee samples); 74 of the 86 kernels, 97.4% of
   the dynamic instructions, are in regions whose player square +-16 px
   contains no spike tile (room (6,2)'s spikes are in tile rows 0-2 only).
   SOUNDNESS: exact, not a widening. The map is constant; the player's x/y
   (and speed) enter only through `Restrict`, whose own error is checked on
   the raw input in every outcome, so on every lane that emits, every
   `mget` coordinate lies in the range the fold reads, and no tile there is
   17/27/43/59. It is the reasoning `tile_flag_over` already applies to
   `TileFlagAt`. Needs: `mget` over interval coordinates in `Graph::eval`
   as a SET of tiles (today it bails), so `Eq(k, Mget)` decides; or the
   intrinsic, which also removes the unrolled loop and its loop-end error
   terms (16% of the instructions are under those, partly overlapping).
   Gate: the oracles, arc-check, and a region WITH spikes ((6,2)'s top rows,
   room (1,0)) unchanged.
2. **Constant-fold the tri-state known plane in the codegen.** A decided
   operand's known plane is `MaskVal::Const(true)`, yet `And`/`Or`/bool
   `Sel` compute it at runtime (5 of 6 instructions of an And/Or). Bit
   identities alone (`x & -1 = x`, `x | -1 = -1`, `c ? t : t = t`, ...;
   `codegen::foldable`) remove **31.1% of the dynamic instructions** (logic
   13.4, reloads 12.8, spills 4.6), 28.2% of the cycles. SOUNDNESS: exact
   per lane (an identity holds for every input); no check is dropped. After
   (1) only 4.8% of the cycles are left to it (they overlap: the known
   planes live in the spike cones). Cheapest of all to do: fold in
   `Lower::dbin`/`vsel` when an operand is `MaskVal::Const`.
3. **Save only the live registers around a call-out.** 17.2% of the
   instructions / 23.6% of the body cycles are the 32-zmm save/restore of
   ~99 calls a slice; the homes live across a call are at most 26 and the 6
   scratches never are. SOUNDNESS: codegen-only, the callee clobbers what
   the ABI says. With (1), the remaining calls are the 42 tile_flag a
   slice; inlining `/8` and `%8` (PICO-8: floor division and a non-negative
   modulo by a power of two are a shift and a mask on the raw 16.16; to be
   checked bit-exact against `Pico8Num` in `asm::tests`) removes the
   remaining div calls (1.7%).
4. **Rematerialize constants instead of spilling them** (the all-ones mask
   is `vpternlogd $0xff`, any other a `vpbroadcastd` from the pool): 2.7% of
   the instructions are reloads of spilled broadcasts. Exact.
5. **Spill pressure** (44.5% of instructions) is the scheduler's: the
   kernels are 34-58k instructions with 1300-1500 spill slots, and values are
   spilled and reloaded on the next line (snippet above). (1) and (2) shrink
   the graph 2-3x first; measure the spill share again after them before
   touching the allocator.

Measured and NOT worth it: hoisting the input-only error terms (pins,
restrictions) into one shared premise. The error OR chains are already
shared by hash-consing (7.7k distinct OR nodes over 86 kernels); one premise
would make 10.8k. The `Restrict` checks themselves cost 0.1%.

What remains after (1), for the next round: guard-only 46% (live 21% of
it), value 29%, shared 20%; movement still 68%, boolean 26%.

Caveats: perf skid attributes a sample to a neighbouring instruction; the
dynamic count excludes callees (measured separately in the same window); the
spike-fold estimate assumes nothing else reads `mget` (true in this program)
and keeps the loop-end terms (so it is a lower bound for the intrinsic).
