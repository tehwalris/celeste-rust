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

### Simplifications (estimates, before; implemented on `kernel-opt`: "After" below)

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

## After: the simplifications implemented (2026-10-10, branch `kernel-opt`)

Same frame (f56 -> f57 of `/var/tmp/canon-h62-f56`, 366,133 slices, 86 of
306 kernels ran), release builds, each step on top of the previous one,
every step's outputs byte-identical to `fg-2300` (plans/kernel-opt-decisions.md
has the verification and the decisions). Dynamic instructions are exact
(slices x static); `kernel` is the `[phases]` kernel phase in worker-seconds
(bodies and call-outs, 16 workers), the cycle measure under load; wave times
are interleaved `tools/bench_step.sh` reps (see BENCHMARK_DATA.md).

| step | static insts | dynamic insts | per slice | reload / spill | call-out marshalling |
|---|---|---|---|---|---|
| fg-2300 | 3,313,993 | 16.36G | 44,692 | 5.39G / 1.90G | 2.81G |
| 1 `spikes_at` folded by range | 818,789 | 4.12G | 11,239 | 0.72G / 0.14G | 1.50G |
| 2 decided planes folded | 711,206 | 3.72G | 10,161 | 0.56G / 0.11G | 1.49G |
| 3 live-register saves, `/8` `%8` inline | 533,938 | 2.67G | 7,292 | 0.56G / 0.10G | 0.43G |
| 4 constants rematerialized | 531,516 | 2.66G | 7,264 | 0.47G / 0.10G (+0.08G constants) | 0.43G |

(1) The set reading of `mget` decides the spike tests in 74 of the 86
kernels; what the estimate called removable is gone (the `spikefold`
estimate on the new kernels: 0.4% of what is left, all in the 12 kernels
whose region reaches a spike tile). Bodies per hot kernel 208 -> 192: the
death outcome's bodies are live nowhere there and drop. (2) After it, bit
identities find 0.9% more (`codegen::foldable`). (3) The hot kernels call
`tile_flag_at` back to back (the player's `is_solid` probes, ~40 a slice):
saving only the registers live across a call took the marshalling from
~64 to ~17 moves a call, and keeping a value saved across a run of calls
that does not read it halved it again. (4) Is a wash in instructions: the
spilled constants' reloads became broadcasts one for one (the reload
pass dropped repeated reloads; it now drops repeated broadcasts too), and
preferring to evict a rematerializable value (tried, then dropped) gained
nothing more.

**Spills after (5).** Spill + reload are 21.6% of the remaining dynamic
instructions (0.57G), almost all reloads of long-lived values far from
their spill (>32 instructions: ~90% of the reloads; spilled and reloaded
within 2 instructions: 2%). The kernel phase is ~2% of the wave's
worker-time now (9% before), so a better allocator (interval splitting,
farthest-next-use eviction) could win at most ~0.4% of the wave. Not done:
the time is in emission (`emit` 70% of the wave's worker-time, 87 lane
emissions per input row, 6% kept by the call's dedup cache).

## Kernel region 8 px against 16 px (2026-10-10, for the storage regions)

The storage redesign (`storage-v2`) keys 8x8-cell storage regions; the
kernels key 16 px regions (`CELESTE_REGION`, default `16,6`). Measured with
the kernels after (1) (and the step-4 binary for times), reference frame
and room (1,0); numbers in BENCHMARK_DATA.md (2026-10-10).

- **Outputs: identical.** Room (1,0) r0sxh f0-f62 and (6,2) 100% r0sxhf
  f0-f50, `ckhash --edges` (and `--dropped`) equal at 8 and 16: the tighter
  constant lattice changes what the kernels compute internally, not the
  rows (here). The choice is cost only.
- **Kernels and startup:** 3.2x the kernels (room (6,2) 978 against 306,
  253 against 86 run on the frame; room (1,0) 326 against 102). Startup
  (walk + trace + assemble, 32 workers): (6,2) ~15 s against ~6 s, process
  +5.6 s; (1,0) +1.4 s. Paid at every process start (every resume, every
  level of a search, every diagnostic). Rooms with many shapes and the split
  frame (room (6,0): 914 traces at 16 px) would pay ~3x of a bigger number.
- **Instructions per slice: -20% at 8** (after (1): 8,962 against 11,239;
  before (1): 38,431 against 44,692). The spike fold removes about the same
  share at both (estimate 66.4% at 8, 64.6% at 16; measured 14.09G -> 3.29G
  at 8, 16.36G -> 4.12G at 16): the 16 px squares already separate the
  spike rows from the rest of room (6,2).
- **Slices: padding 0.0% -> 0.2-0.3%**, rows per call unchanged (a call is
  a dispatch group, not a kernel).
- **Kernel cycles:** the kernel phase 1.1 -> 0.8 worker-s on (6,2), 1.7-1.8
  -> 1.6-1.7 on (1,0): about the instruction ratio, on what is now ~2% of
  the wave.
- **Wave: no measurable difference** ((6,2) 3201-3283 against 3147-3197 ms;
  (1,0) waves summed 3611-3681 against 3620-3688 ms; the noise is larger
  than the kernel phase's 0.3 worker-s / 16 workers).

**Recommendation: keep the kernels at 16 px and make the storage regions
16 px**, if the storage side's own measurements allow (its region size is
what trades there: index size against scan width). Kernels at 8 buy nothing
measurable in the wave now that the kernels are ~2% of it, and cost ~3x the
kernels at every process start. If storage needs 8, kernels at 8 are safe
(identical outputs) and cost only the startup; the default stays 16 until
that is decided. Not changed.
