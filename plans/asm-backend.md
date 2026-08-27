# A native AVX-512 assembly backend for the traced kernels

Status: **prototype / vertical slice** (2026-08-27). A NEW, alongside-the-
production code generator that turns a `transpile::graph::Graph` directly
into native AVX-512 assembly, assembled with `as`/`gcc` and loaded with
`dlopen`. It attacks two costs of the current graph -> Rust -> rustc+LLVM
path:

1. **Compile time (primary).** `crates/celeste-kernels` is ~900k lines; any
   change forces a multi-minute rustc + fat-LTO relink. Assembling one small
   `.s` file is orders of magnitude faster (measured below).
2. **Runtime.** The profiled `frame` for room10/kernel1 spends ~42% in
   row-key hashing (`mix64`), ~14% in the register spills/moves LLVM emits
   allocating a 46k-instruction, thousands-of-live-value SIMD function. A
   purpose-built allocator/scheduler controls exactly those two.

Nothing here touches the production emit path (`src/transpile/lower.rs`) or
the checked-in kernels. It is a separate module, `src/transpile/asm/`.

## Scope of the slice

The highest-value, easiest-to-verify subgraph is the **row-key hashing
core** (this branch uses the sequential `Mix` fold):

- `Op::Word(u64)` - the fold seed (`zw_splat`, or a broadcast constant).
- `Op::Bits` - a value's representation bits as a machine word
  (`zw_bits_n` for a numeric column).
- `Op::Mix(c, 0)` = `zw_mix1(h, v, c)` = per half `mix64_v(h ^ mix64_v(v ^ c))`.
- `Op::Mix(c, 1)` = `zw_mix2(h, v, c)` = per half `h + mix64_v(v * ((c<<1)|1))`.

fed by numeric arithmetic (`Add/Sub/Min/Max/Neg/Abs/Flr`) over
`Op::Cell` / `Op::Const`. Call-free, arithmetic-dense (two nested `mix64`
per `Mix1`, a 64-bit multiply per `Mix2`), and 42% of the runtime. In
`kernel1` these ops dominate the census (623 `zw_mix1`, 623 `zw_mix2`, 271
`zw_bits_n`).

Deliberately OUT of the first slice, each a stretch, so the holes are
explicit rather than silent:

- **Booleans / masks** (`ZB`, `Op::Lt/And/Or/Sel/...`, `zw_bits_b`). These
  need the k-mask register class (k1-k7). The numeric+hashing core is closed
  without them.
- **Intervals** (`ZI`, `zw_bits_i`, `Op::Span/Frag`). Two planes per value.
- **Collision calls** (`Op::Mget/TileFlagAt`). These stay CALLS into the Rust
  cache in any real design; the slice omits them.
- **Div / Rem / Sin / Mul.** Div/Rem/Sin are holes in the Rust primitives
  too (they go scalar). `Mul`'s even/odd weave is off the critical hashing
  path and left out of the first slice.

A graph using an unsupported op fails at emit time with a named error, never
silently.

## The lane/type model, and how it maps to registers

| graph repr | Rust type | registers |
|---|---|---|
| Num, lane, !wide | `ZN` (`__m512i`, 16xi32) | 1 zmm |
| Word, lane | `ZW` (two `__m512i`, 16xu64) | **2 zmm** (half0 = lanes 0-7, half1 = 8-15) |

A `ZW` node is expanded into **two independent single-register SSA values**
(`half0`, `half1`) at lowering time, because `mix64_v`/`xor`/`mul`/`add`
act on each half identically. The register allocator then only ever sees
single-zmm values - a textbook problem.

### Register-file partition (of the 32 zmm)

- `zmm31` = constant ZERO (set once at entry; for `Neg`).
- `zmm30, zmm29` = `mix64_v` internal temporaries.
- `zmm28` = intermediate for the `Mix` two-step (`v^c`, `h^inner`, `v*k`).
- `zmm27, zmm26` = reload scratch for spilled operands.
- `zmm25` = scratch for a spilled *result* (computed then stored).
- `zmm0 .. zmm24` = **25 allocatable homes.**

Constants never occupy a register: `Op::Const` (i32) and `Op::Word` (u64)
and the `Mix` key `c` / `(c<<1)|1` and the two `mix64` constants become
**embedded-broadcast memory operands** from a deduplicated `.rodata` pool
(`vpaddd c(%rip){1to16}`, `vpxorq c(%rip){1to8}`, `vpmullq c(%rip){1to8}`).
Both faster (fewer live values) and realistic.

## Instruction selection (per op, matching kernel.rs bit-for-bit)

| Op | asm | mirrors |
|---|---|---|
| `Cell` (num,lane) | `vmovdqu64 off(%rdi), z` | input load |
| `Const(x,x)` | operand `c(%rip){1to16}` | `zn_splat` |
| `Add/Sub` | `vpaddd/vpsubd` | `zn_add/zn_sub` |
| `Min/Max` | `vpminsd/vpmaxsd` | `zn_min/zn_max` |
| `Neg` | `vpsubd z, ZERO, d` | `zn_neg` |
| `Abs` | `vpabsd` | `zn_abs` |
| `Flr` | `vpandd 0xffff0000(%rip){1to16}` | `zn_flr` |
| `Bits` (num) | half0 `vpmovzxdq %ymm(z)`; half1 `vextracti64x4 $1`+`vpmovzxdq` | `zw_bits_n` |
| `Word(w)` | operand `w(%rip){1to8}` on both halves | `zw_splat` |
| `Mix(c,0)` | `vpxorq c{1to8}` -> `mix64_v` -> `vpxorq h` -> `mix64_v` | `zw_mix1` |
| `Mix(c,1)` | `vpmullq ((c<<1)|1){1to8}` -> `mix64_v` -> `vpaddq h` | `zw_mix2` |
| root store | `vmovdqu64 z, off(%rsi)` | `ZW::to_array` layout |

`mix64_v(x)` (src reg X, result reg D; internal `t`=zmm30, `u`=zmm29):
```
vpsrlq $30,X,t ; vpxorq X,t,t ; vpmullq C1(%rip){1to8},t,t
vpsrlq $27,t,u ; vpxorq t,u,u ; vpmullq C2(%rip){1to8},u,u
vpsrlq $31,u,t ; vpxorq u,t,D
```
`C1=0xbf58476d1ce4e5b9`, `C2=0x94d049bb133111eb`. `vpmullq` is AVX-512DQ,
present on this machine (`/proc/cpuinfo`: avx512dq).

## Register allocation + scheduling

- **Schedule = node-id order.** The hash-consed arena appends nodes after
  their operands (`Graph::add`), so id order is already a valid topological
  order. The slice keeps it; a live-range-shortening list scheduler is future
  work.
- **Allocation = linear scan (Poletto-Sarkar).** Each vreg gets a live
  interval `[def, last_use]` over the schedule. Walk in def order with an
  active set sorted by end; expire, then either take a free home or spill the
  interval that ends latest to a stack slot. Spilled operands are reloaded
  into scratch at each use; a spilled result is computed into scratch and
  stored. Stack frame = 64 bytes/slot.

The allocator is the component most likely to have a silent bug, which is
exactly why the correctness gate is random and bit-exact.

## Emission / assembly / call mechanism

**Chosen: emit GAS `.s` text, assemble with `gcc -shared -fPIC`, `dlopen`.**
Justification vs a JIT crate (`dynasmrt`):

- `dynasm-rs`'s x64 assembler has **no EVEX / AVX-512 support**, so every
  instruction would need hand-encoded `.byte` directives - error-prone and
  unverifiable. `as` assembles AVX-512 natively and is installed.
- The compile-time comparison we want is *"assemble a `.s` vs rustc builds
  the crate"*; producing a real `.s` and timing `as` is the honest measure.
- `objdump -d` on the `.so` gives the instruction mix directly (no
  `llvm-mca` on this box).

ABI: `extern "C" fn(inputs: *const u8, outputs: *mut u8)`. `inputs` = input
cells packed as 64-byte `ZN` columns in ascending cell-id order; `outputs` =
roots packed as 64-byte half-columns. `dlopen`/`dlsym`/`dlclose` are declared
directly and linked from `dl`.

## Correctness

`asm::tests` builds random row-key subgraphs with the public `Graph` API
(the node shapes `lower.rs` builds), emits+loads them, and compares output
against an **independent Rust evaluator that calls the real
`celeste_engine::kernel` primitives** (`zw_mix1`, `zw_mix2`, `zw_bits_n`,
`zw_splat`, `zn_add`, ...) on the same random 16-lane inputs. Bit-exact over
many seeds and shapes, including deep `Mix` chains that force spilling. A
codegen bug is silent, so this gate is non-negotiable and runs as a normal
`#[test]`.

## Assumptions & judgment calls

- The graph is a valid DAG in id order (true by construction).
- Only the listed ops are supported; anything else is a hard error.
- `vpmullq`/`vpxorq`/`vpandd` (DQ) assumed present, same `cfg` the engine
  asserts.
- The allocator targets "fewer spills than LLVM on a huge function", not
  optimal allocation. Whether it beats LLVM at scale is an open measurement;
  the first slice is small enough to fit in registers, so the first runtime
  number isolates *hashing throughput*, not spill reduction.

## Measured results (2026-08-27, this machine; `quick` profile)

Benchmark `asm::tests::asm_backend_benchmark`: a `build_wide(24, 24)` key
graph (648 nodes, 24 shared `Bits`, 24 rows => ~1150 `Mix` ops, close to
kernel1's 623+623), emitted three ways and compared on byte-identical work.
Both the asm kernel and the rustc equivalent are `dlopen`ed, so this is
codegen-vs-codegen.

| metric | asm backend | rustc -O (same graph) |
|---|---|---|
| emit + assemble | **1.45 ms + 22.9 ms = 24.4 ms** | 125,524 ms |
| runtime | 2115 ns/call | 678 ns/call |

- **Compile time: ~5155x faster** (24 ms vs 126 s). This is the primary
  motivation and the result is emphatic - rustc+LLVM spent **two minutes** on
  ONE ~1150-node function (the same superlinear blow-up that makes the 900k-
  line kernels crate a multi-minute relink), where `as` assembled the 17k-line
  `.s` in 23 ms. The correctness gate (bit-exact vs the primitives) passed for
  both.
- **Runtime: 3.1x SLOWER than LLVM (a real, honest loss).** The cause is
  identified and is NOT fundamental: `mix64_v` is emitted through three FIXED
  scratch registers (`zmm28/29/30`) reused by *every* mix, which serializes
  all ~1150 otherwise-independent mix chains through the same physical
  registers - false dependencies that kill ILP. LLVM allocates fresh
  registers per mix and interleaves the 24 independent rows to hide the
  splitmix latency chain. The 70 spill slots (schedule = fixed node-id order,
  which keeps all 48 bit-halves live to the end) compound it.

### Conclusion / clearest next step

The compile-time thesis is proven overwhelmingly; the runtime thesis is not
yet - the naive scheduler loses the 14% spill budget and more. The next step
is a real **list scheduler + per-value scratch**: (1) give `mix64_v`/`Mix`
fresh allocator-managed temporaries instead of the three fixed scratch regs,
so independent chains stop aliasing; (2) schedule to interleave independent
rows and shorten live ranges (round-robin the row folds instead of emitting
row 0 fully, then row 1, ...), which also cuts the spill count. That is where
the promised spill/ILP win lives; the current slice deliberately isolates
correctness and compile time first.

## Hill-climb to LLVM parity (2026-08-27, Ryzen 9 7950X3D / Zen 4)

Goal: drive the `build_wide(24,24)` microbenchmark (≈1150 `Mix` ops, the
row-key hashing slice) to LLVM's number on the identical graph. All numbers
`--cargo-profile quick`, both kernels `dlopen`ed (codegen-vs-codegen on
byte-identical work); LLVM baseline **≈680 ns/call**. Bit-exactness vs the
`celeste_engine::kernel` primitives held after every step.

| step | ns/call | spills | note |
|---|---|---|---|
| start (fixed 3-scratch mix64, row-major) | 2115 | 70 | 3.1x off |
| 1. per-value vreg temporaries for mix64/Mix | 2006 | 69 | renaming already hid the false deps; barely moved |
| 2. depth-major interleave of the 24 rows | 2148 | 597 | ILP exposed but allocator drowned in spills |
| 3. rematerialize Bits/loads (never spill them) | — | — | cut bit spill traffic; enabled interleave |
| 4. GVN (share the inner `mix64(bit^c)` across rows) | 1239 | 120 | **the big one**: muls 4032->1392 |
| 5. ALAP order + batched interleave | ~1210 | 118 | shortened shared-inner live ranges |
| 6. instruction-level list scheduler, height-only | 3316 | 2745 | max ILP, ignored pressure -> spill firehose |
| 7. **pressure-aware list scheduler** (batch 4, limit 16) | **862** | 485 | expose ILP under a live-reg ceiling |

Result: **862 ns vs LLVM 680 ns = 1.27x** (from 3.1x). Compile time 23 ms
vs rustc 23,000 ms = **~1000x**. Not parity.

### Where it is stuck, with objdump evidence

The kernel's `objdump` mnemonic mix at the plateau:
`vpxorq 2641, vpsrlq 2016, vpmullq 1392, vpaddq 576` = **6625 vector-ALU
ops**, plus `vmovdqu64 1562` (spill/reload + I/O). The multiply count (1344,
after GVN dedups the shared inner `mix64`) equals LLVM's - GVN closed the
op-count gap, so this is NOT a redundant-work problem.

The bottleneck is **vector-execution-port throughput**. On Zen 4 AVX-512 is
double-pumped (a 512-bit op occupies a 256-bit pipe for two cycles), and
`vpmullq zmm` issues on the fewest pipes, so ~1344 muls plus the xor/shift/add
traffic saturate the vector ports at ≈680 ns - exactly where LLVM lands.
Spills are NOT the limiter: runtime was measured FLAT across 120..671 spill
slots, because spill loads/stores use the separate load/store ports (2 loads +
1 store per cycle) and overlap the vector work. The residual 1.27x is
dependency-stall bubbles on the mul port: the outer `mix64` of the mix1 rows
forms 24 serial 2-multiply chains, and the pressure-aware scheduler does not
interleave them as tightly as LLVM's, leaving the port idle ~25% of the time.
A latency-modelled (cycle-accurate) list scheduler is the next lever; a
plain "operands produced longest ago" tiebreak was tried and REGRESSED
(to ~930 ns), so the improvement needs a real hazard model, not a heuristic.

### Knobs (env, for A/B; defaults are the measured sweet spot)

- `CELESTE_ASM_BATCH` (default 4): rows per interleave batch in the node
  pre-order.
- `CELESTE_ASM_PRESSURE` (default 16): live-register ceiling above which the
  list scheduler switches to freeing registers instead of chasing ILP.
- `CELESTE_ASM_SCHED=0`: disable the instruction scheduler (node order only).

### Verdict for adoption

The compile-time thesis is proven ~1000x over. Runtime is 1.27x off LLVM and
port-bound; closing it needs a cycle-accurate scheduler, which is worth
building only if the hashing kernels are chosen for the asm backend on
compile-time grounds and the 27% is then bought back. For a kernel that is
NOT this hashing-dense, the port roofline is looser and parity is more likely
out of the box.

## Milestone 1: full value + hash DAG op coverage (2026-08-27)

The backend now lowers EVERY op a real kernel DAG contains, each gated
bit-exact against the corresponding `celeste_engine::kernel` primitive on
synthetic graphs (6 `#[test]`s, all green, warning-free). Verified against
the actual primitive census over all three generated rooms.

### Representation model (all in the zmm class - no new register class)

| graph value | asm | registers |
|---|---|---|
| `ZN` number | i32 lanes | 1 zmm |
| `ZI` interval | lo/hi planes | 2 zmm |
| `ZB` tri-state bool | **vector masks** (per-lane 0/0xFFFFFFFF) val/known | 2 zmm |
| `ZW` word | u64 halves | 2 zmm |

Keeping `ZB` as vector masks (not k-registers) was the key call: the whole
boolean layer - the bulk of a kernel (`zb_holds` 160k, `zsel_n` 23k,
`zb_and` 19k across the rooms) - reuses the existing allocator, scheduler,
spill and remat, with only a single fixed scratch k-reg (`k1`) touched
transiently inside a compare or blend.

### Op classes covered, and how

- **Arithmetic** `Add/Sub/Min/Max/Neg/Abs/Flr` (`vpaddd/…`), `Mul` (native
  even/odd `vpmuldq` weave = `zn_mul`).
- **Comparisons** `Lt/Le/Gt/Ge/Eq` -> `vpcmpd` to k1 -> `vpmovm2d` to a
  vector mask (`zn_*`); interval `Eq` folds to a numeric compare.
- **Tri-state bool** `And/Or/Not/Eq/Known` via `vpand/por/pxor/pandn`
  matching the Kleene formulas in `zb_*`.
- **Select** `Sel` (all widths `zsel_n/b/i`) via `vpternlogd $0xca` on the
  condition's val plane (join-on-unknown handled by the mask algebra).
- **Intervals** `zi_add/sub/min/max/neg/abs`, `Span`, `Flr` (`zi_flr`),
  interval compare (`zi_cmp`), `Frag/FragOk/SplitOk` (`zi_fork_flr` /
  `zi_span_ok`), `Known(Flr(interval))` -> `zi_flr_ok`. Wide `Const` -> `ZI`.
- **Hash bridge** `Bits` of number/interval/bool (`zw_bits_n/i/b`) so the
  value layer feeds the row-key fold; `Word`/`Mix` as before.
- **Call-outs** `Div/Rem/Sin/Mget/TileFlagAt` (below).

### The call-out ABI (the one genuinely hard interface)

`Div/Rem/Sin` (scalar per-lane even in the Rust primitives) and the
collision look-ups `Mget/TileFlagAt` (a cart/cache query mid-DAG) are
emitted as a `call` through an `AsmCtx` of function pointers - the kernel's
THIRD argument. Design:

- Loads/stores move to callee-saved `r13` (inputs) / `r14` (outputs); the
  ctx sits in `r15`. So a mid-DAG `call` may freely clobber `rdi/rsi/rdx`.
  The prologue pushes r13/r14/r15 (also making rsp 16-aligned for calls).
- Each SIMD operand is marshalled through a stack buffer; the wrapper
  (`callout.rs`) reads it, invokes the EXACT kernel primitive, and writes
  the result back - so the answer is bit-identical by construction. `Div/
  Rem/Sin/Mget` return a `ZN` (64-byte buffer); `TileFlag` returns a `u16`
  mask that is re-expanded to a vector mask via `kmovw`+`vpmovm2d`.
- A call clobbers every vector register, so the emitter saves all 32 zmm to
  a stack save-area before the call and restores them after (a spilled live
  value is in my frame, untouched by the callee). This is correctness-first
  and slow for collision-heavy kernels; a future pass can save only the live
  registers. `Mget`/`TileFlagAt` take the cart/cache via `AsmCtx::env`
  (a `CollisionEnv`); the collision test wires the REAL `CartData`/
  `CollisionCache` and matches `zn_mget`/`zn_tile_flag_at` bit-exact.

### What the DAG does NOT contain (so it is not built / not load-bearing)

Confirmed by census over all three rooms:

- `zi_mul_pos` / `zi_div_pos` (interval * / / positive scalar): **zero
  sites** - NOT implemented; interval `Mul`/`Div` raise a clean emit-time
  error if ever encountered.
- `zn_sin`: **zero sites** - implemented (call-out) and tested anyway.
- `zn_tile_flag_at_lanes` (per-lane hitbox): **zero sites** - the codegen
  path (`CallOp::TileFlagLanes`) and wrapper exist but are UNTESTED with a
  cart; the created-object frame that needs it is not among the generated
  kernels.
- `zi_min/max/neg/abs`, `zi_flr_ok`: zero sites - implemented + tested
  (cheap, and they fell out of the interval layer).
- `Free`/`Split`/`SplitValid`: eliminated by `specialize_into` before
  codegen (as in `lower.rs`), so codegen never sees them.

### Non-replicated error paths (documented, not hit by valid kernels)

- `zi_add`/`zi_sub` PANIC on overflow in the Rust primitive (scalar
  fallback); the asm wraps silently. Valid kernels never overflow here; the
  interval test uses bounded inputs.
