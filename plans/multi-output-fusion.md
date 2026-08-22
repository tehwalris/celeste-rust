# Multi-output-shape fusion: fuse on a pure graph, let members differ

2026-08-21, design agreed with Philippe. Successor to `plans/shape-tag-plan.md`
(tasks #159-#164, which built the fusion that exists today).

## The target

One INPUT shape, n members, **n OUTPUT shapes**, one shared value graph.
Each member computes its own validity and materializes its own rows; rows
that are invalid for a member are dropped before dedup.

Today's fuser does one input shape, n members, and **one** output shape:
member 0 (the primary) materializes rows, and every other member must
prove its boundary is block-uniform so the executor can collapse its lanes
to one representative per distinct output tuple. That special case covers
death (the corpse's per-lane cells drop out of the reachable set). It
cannot express any member where the shape changes and per-lane state
SURVIVES - fruit collection, room exit.

## Why the current design is stuck, in its own words

Two constraints in `src/transpile/fuse.rs` do all the blocking.

**One validity for all members** (the gate, ~line 437):

```rust
if pm.e.fork_depth != primary.e.fork_depth || pm.e.valid_expr != primary.e.valid_expr {
    bail!("member {} fork structure differs", pm.label);
}
```

`Emit` already carries a per-member `valid_expr` (`kernel.rs:155`). Validity
is already a value computed inside the graph. The fuser simply forbids the
members from disagreeing about it.

**Non-primaries may not have per-lane outputs** (~line 505): every reachable
per-lane boundary cell of a non-primary must be *textually identical* to the
primary's out column for that cell, or

```
member {}: per-lane boundary cell {} is not the primary's value
```

This check exists only to justify the representative-collapse trick. With
real per-member materialization it is not merely unnecessary - it is the
thing standing in the way.

## Why fusion under-fuses today

Fusion does not operate on a graph. It operates on **rendered Rust source**.
From `kernel.rs:83`:

```rust
/// `let name: ty = expr;` - a value node. `expr`'s arguments are
/// whole-identifier variable names, witness field accesses (`u.cN`,
/// `rin.cN`) or literals, so the graph edges recover by identifier scan.
Let { name: String, ty: &'static str, expr: String },
```

Edges are recovered by scanning identifiers out of a string. `fuse.rs`
canonicalizes that string (identifiers -> `<id>`) and interns it;
`NodeDef.render` is a `String`. **Two members share a node iff their emitted
text matches after operand renumbering.** There is no prover, no
normalization, no algebraic reasoning.

Consequence, measured on room (2,0) 2026-08-21 with the SAME member derived
two ways - same semantics, same membercheck certificate both times:

| derivation of the dying-fall member | shared with steady | union | cost of the member |
|---|---|---|---|
| pin every branch the death witness takes | ~270 / 1396 | 2348 | **+68%** |
| steady's recipe verbatim, flip only the kill gate | **1375 / 1396** | 1397 | **+1 node** |

The pinned member never computed different VALUES. It computed the same
values through constant-folded, textually different code. A ~950-node
difference produced by nothing but notation.

That is the case for P1 below. A member whose IR happened to get more
constant propagation than another's should still fuse with it; on a pure
graph you normalize both and structural-hash, and the question disappears.

## The plan

### P1 - REFUTED BY MEASUREMENT 2026-08-21, see "P1 was wrong" below

The original P1 is kept below for the record, then corrected. Do not
implement it as written.

### P1 (as originally written) - fuse on a normalized pure graph, not on text

Represent each member's lowered body as value nodes (opcode + operand ids),
not rendered strings. Normalize before hashing: constant-fold to fixpoint,
canonical operand order for commutative ops, drop dead nodes. Then
structural-hash for sharing.

This is the load-bearing change and it pays off **independently of P2/P3**:
it raises sharing on the members we already have and it retires the
hand-alignment discipline that today's result depends on.

Honest caveat: the graph is not fully pure. `zguard(...)`, deopt reporting
(`&mut dp`, tracked as `NodeDef.has_dp`) and bail statements (`*bd = true`)
are effects. They need to be explicit ordered effect nodes, and they
constrain reordering. They are a small minority of items, but "it's all
pure, so normalization is free" would be overselling it.

What P1 does NOT fix: where a pin genuinely DELETES a computation, the
graphs really do differ. That is fine - node membership is already a
bitmask (`NodeDef.members`), so a node present in only one member costs
only itself.

### P1 was wrong: the divergence CASCADES (measured 2026-08-21)

P1's rationale above - "a member whose IR got more constant propagation
should still fuse; normalize both and the question disappears" - was
tested before implementing it, on the exact adversarial pair, and is
**false**. `/tmp/normspike.py` (analysis only): parse both emitted
kernels, rebuild each member's DAG, intern into ONE shared table exactly
as `fuse.rs` `Ctx` does, then apply normalization and re-count. The
harness reproduces the fuser's own numbers with a constant -10 offset (10
node forms its regex misses), so it is trustworthy:

| pair | fuse says | harness | + normalization |
|---|---|---|---|
| steady + v2 blended | 1396 / 1376 / **1375** shared | 1386 / 1366 / 1365 | 1365 (**+0**) |
| steady + v1 pinned  | 1396 / 1169 / **271** shared | 1386 / 1159 / 261 | 261 (**+0**) |

Normalization buys **exactly zero**. Why: the difference is not local
notation, it CASCADES. Of steady's 1125 unshared nodes only **31 are on
the divergence frontier** (every operand shared); the other 1090 have at
least one operand that already diverged. Dying-only: 2 frontier, 892
cascade. **~33 real divergence roots poison ~1982 nodes - 60:1.** Once
one operand differs, structural hashing can never re-match anything
downstream of it, however well you normalise.

And the frontier is mostly not notational:

```
bool   true                                 <- a folded constant
ZB     zsel_b(<181>, <194>, <194>, &mut dp) <- identical arms, trivially foldable
ZI     zsel_i(<181>, <186>, <182>, &mut dp) <- steady BLENDS where the member PINNED
P8     u.c85 + <0>   vs the member's  u.c87 + <0>   <- a DIFFERENT uniform cell
bool   !u.c310, !u.c309, u.c46 >= <4>       <- uniform-cell comparisons
```

Unifying the `zsel_*(<181>, ..)` family needs PREMISE reasoning ("under
this member's guard `<181>` is true, so the select equals its arm"), not
constant folding - a much larger feature. `u.c85` vs `u.c87` is a
genuinely different input and must never be unified. Only the
identical-arms select is a cheap normalization win.

**Consequences.**

1. Derivation discipline is not a workaround for a weak fuser - it is
   the mechanism. A fuser cannot repair downstream what the derivation
   destroyed upstream. Build members as "the primary plus one flipped
   decision" because that is the only thing that works, not as a stopgap.
2. The recipe-alignment tooling dropped from the first draft comes BACK,
   in a better form the 60:1 ratio suggests: **a frontier report**. When
   fusion under-shares, print the ~33 frontier nodes - that is exactly
   where two derivations parted company, small enough to read, and it
   replaces a half-day of guessing at recipe entries with a diff. Cheap;
   `normspike.py` is a working prototype.
3. A pure-graph node IR may still be worth building for other reasons
   (readability, the identical-arms fold, P2's needs), but **not** on the
   promise of recovering sharing. That promise is measured and dead.

### P1' - the minimal graph IR (design agreed with Philippe, 2026-08-22)

This replaces the refuted P1. The point is not a better matching
algorithm; it is a smaller thing to match ON.

```
Node   = (op, [operand ids])       -- types inferred, not stored
Leaf   = input cell | literal
Member = { outputs: cell -> node, valid: node }
```

No lanes. No `dp`. No effects. No statement order. A member is a mapping
from input cells to output cells plus one boolean saying whether the
result counts.

**Types are one lattice, not three kinds of thing.** Two domains - bool
and number - each carrying an abstraction. A tri-state bool is a set of
booleans (`ZB { val, known }` is already exactly that, per lane); an
interval is a set of numbers; an exact number is a singleton interval.
Keep "exact number" distinct as a REPRESENTATION optimization, because
most ops preserve exactness and it is worth tracking - not as a third
base type.

**Lanes are lowering, not semantics.** `zn_splat` + `zb_splat` +
`zi_splat` = **1150 of the steady kernel's 3236 nodes, 35%**, and they
carry no meaning at all: they only move a uniform value into vector-land.
Model one lane and they do not exist. Uniformity recovers afterwards by
one forward pass from the leaves (`u.cN` uniform, `rin.cN` per-lane).
This is not only tidiness: **20 of steady's 31 divergence roots were
splats and constants**, so the lane distinction is actively MANUFACTURING
divergence between members. Pinning changes what is known, which changes
what is uniform, which moves the splats, which forks the node identities.

**Validity is a value, not a side channel.** Today `&mut dp` is threaded
through nearly every op, accumulating a per-lane bitmask; that is what
makes nodes effectful (`NodeDef.has_dp`) and what forces the
`dp_m{bitmask}` member-set registers. As ordinary nodes:

* `zguard(c)`            becomes  `valid := valid AND c`
* select on unknown `c`  becomes  `valid := valid AND known(c)`,
  with `known` an ordinary operator on the abstract bool

The graph is then PURELY FUNCTIONAL, which is the precondition for any
serious optimization over it (equality saturation included). The "it is
not really pure, guards and dp are effects" caveat in the original P1
dissolves - it was an artifact of the representation.

**Partial ops are an artifact, not a constraint** (Philippe, 2026-08-22).
`div_positive` / `scale_positive` `assert!(rhs.0 > 0)` only because the
negative side was never modelled - written without access to a real
PICO-8 to check against. We have one now, so the fix is to MODEL
DIVISION CORRECTLY over negatives and drop the assert. Then every op is
total, and "compute everything, filter at the end" is unconditionally
safe. Do this as part of stage 1 rather than designing around it.

**So the type story collapses further.** There are two domains, bool and
number. Everything else - exactness (is this number a singleton?) and
uniformity (is this the same for every lane?) - is a DERIVED PROPERTY
propagated over the finished graph, in the same way, by the same kind of
forward pass. Neither belongs in the node type.

#### Gates: the round trip through Rust is NOT the gate

Philippe's objection, accepted: lowering CFG -> graph -> Rust and
demanding byte-identity with the committed kernels would force the new IR
to reproduce every incidental ordering and naming decision of the current
emitter, including the splats and the dp threading it exists to delete.
That fights the design and proves the wrong thing.

The EVALUATOR is the gate; the code generator comes last.

1. **Node IR + lowering from the branch-free CFG + a direct evaluator.**
   ~30 pure ops whose bodies already exist in
   `crates/celeste-engine/src/kernel.rs`. Gate: evaluator output ==
   the existing kernel's output on real witness chunks. **No code
   generator is needed to validate the IR.**
2. **Optimization over the graph.** Gate: the same evaluator equality
   after optimizing. Semantics preservation is checked, not argued.
3. **graph -> Rust emitter.** Gate: the existing
   `CELESTE_COMPILED_FORWARD=check` at f42 (row-key sets against the
   interpreter, which is the reference) plus a perf measurement.
4. **Fusion on the new IR.** Re-run the pinned-vs-blended pair (271 vs
   1375 shared) and find out how much derivation-independence the new
   fuser actually buys. This is the experiment that settles the original
   disagreement, and it is now CHEAP: `tools/fuse_frontier.py` already
   reports the divergence frontier, so the answer is a diff.

Stage 1 is self-contained and is the right first commit.

#### What actually happened, 2026-08-22

Stages 1 and 3 landed; stage 2 was skipped as premature, and the standalone
evaluator gate was **subsumed rather than skipped**, which is worth being
precise about.

The graph now DRIVES code generation (`transpile::lower`), so the class
kernels the search runs are the graph's own output. That makes
`compiled_forward_reproduces_the_interpreter` and
`CELESTE_COMPILED_FORWARD=check` gates on the graph itself - strictly
stronger than "an evaluator agrees with the kernel", because it validates
the thing production executes rather than a second reading of it. What it
does NOT gate is `Graph::eval`, which no longer has a production consumer;
its unit tests are all that stands behind it. If a later stage makes the
evaluator load-bearing (equality saturation, a proof obligation), it needs
its own gate at that point.

Two invariants were added along the way that the plan did not anticipate,
both because the model would otherwise have been quietly incomplete:

* `Emit::ok` accumulates the BLOCK-UNIFORM bails (`*bd = true`) as well as
  the per-lane ones. Which of `dp` and `*bd` a condition lands in is
  derived from whether the condition is uniform, so one node replaces both
  side channels - and a uniform bail that was not in the chain would have
  made `ok` claim lanes the emitted code gives up on.
* `Op::ForkOk` records the ">2 floors" case `zi_fork_flr` deopts. It is
  always discharged by the fork call, so it never renders; it exists so the
  chain is complete. In today's kernels the same lanes are caught anyway by
  the `zi_flr` that follows every fork, but that is a coincidence of these
  programs, not a property of the lowering.

### P2 - per-member validity and per-member outputs

- Relax the `valid_expr` equality gate: each member carries its own
  validity, which is already a node in the shared graph.
- Give each member its own out-fields and materialization
  (`compute_out_fields` / `emit_interface` / `emit_key_cell` run per member
  instead of once for the primary).
- Drop rows that are invalid for their member before dedup. The boundary
  already filters; no new runtime primitive is needed.
- DELETE the block-uniformity proof (`reachable_cells` + the tuple/vary
  checks). Replace with: each member's output shape is registered against a
  witness, and the member masks plus the deopt count partition the chunk
  exactly (no lane lost, none double-counted).

An earlier draft of this plan proposed compacting each member's lanes into
a dense block before running its tail, so each row is written once.
Philippe's framing is better and is what is written above: every member
computes validity anyway, so materialize and filter. The only cost
difference is n x row-writing versus one gather, and the shared graph - the
expensive part - is computed once either way. **Compaction is dropped.**

### P3 - multi-output-shape plumbing

`FrameEngine::step` is already `(shape, rows) -> [(shape, rows)]`. The
interface already admits many output shapes; nothing has ever populated
more than one entry. Make the fused kernel return n `(shape, rows)` pairs
and check that dispatch, the bridge and the chunk accounting handle n > 1.

### Deliberately kept

- ONE input shape per specialization set.
- Identical fork skeleton across members - that is what makes a single
  shared prefix possible.
- All-or-nothing refusal. `//! Anything this pass cannot prove is REFUSED
  ... there is no partially-fused output.` Right stance for a proof
  pipeline; keep it.

### Dropped from the earlier draft

- **Lane compaction** - subsumed by P2 (see above).
- **Recipe-alignment tooling** ("keep members textually aligned so they
  fuse") - this was a workaround for a text-level fuser. P1 removes the
  need. Do not build it.

## Gates

- `membercheck` per member, unchanged - still the front line.
- `CELESTE_COMPILED_FORWARD=check` extended to compare multi-shape output
  sets per chunk.
- New: partition check - union of member masks + deopt count == lane count.
- Full suite under nextest; regen-generated.sh diff read before commit.

## Sequencing

0. **P1' stage 1 (node IR) - DONE.** Followed by stage 3 (the emitter),
   because implementing the two button fusions in the TEXT emitter would
   have meant retrofitting the binary prefix/suffix taint machinery that
   the fusions exist to delete. Order landed:
   A. graph completeness (validity covers the uniform bails, `live` split
      from `ok`, button bits are named leaves, the unexercised interval
      `__split_at` removed). Gate: all 8 kernels byte-identical.
   B. `transpile::lower` - representation, placement and guards all
      DERIVED from the graph; `render` emits from it.
   C. the two button fusions (exact 2^|cone| placement, output-signature
      dedup), which are consequences of B rather than features.
   D. `transpile::fuse` ported onto the graph; the `Line` stream and the
      taint machinery deleted.
1. **Census before P2/P3.** Split room (2,0)'s f42 miss tranche by cause.
   It is recorded today as one lump: "f39+ fruit-touch: 786,848 lanes are
   guard/bind refusals ON the covered a9e20c0a shape (class-leaving,
   bounce, dying) and 239,814 are the 409-cell spring-delay residual shape
   681d". Only the shape-change-with-survivors slice is what P2/P3 buys,
   and it is UNMEASURED. `CELESTE_KERNEL_MISS_DUMP` already produces the
   raw material.
2. P1' stages 2-4 (optimize over the graph; then the emitter; then
   fusion on the new IR). Stage 4 settles the pinned-vs-blended question.
3. P2 + P3 together (they are one coupled change; splitting them only
   yields a broken intermediate).
4. Gates, then measure per room - same discipline as engine adoption
   (BENCHMARK_DATA.md, "Engine adoption validation at depth").

## Stage C, designed but not built: the button tree

Today the button bits split the body in TWO - a prefix nothing button-
dependent may touch, and a suffix monomorphized over `2^|used|` values of
`const B: u8`. A node that depends on ONE button is therefore computed 64
times instead of 2. Measured per member (`node-evaluations per frame` in
the emitter's own census, 2026-08-22):

| member | nodes | binary split | exact 2^|cone| | ratio |
|---|---|---|---|---|
| steady     | 1526 | 12,740 | 4,504 | 2.8x |
| r20-steady | 2573 | 19,331 | 9,401 | 2.1x |
| frozen     |   46 |    424 |    52 | 8.2x |
| r20-frozen |   93 |    471 |    99 | 4.8x |
| dash       | 1279 |  1,657 | 1,285 | 1.3x |

Read that ratio carefully. It counts NODE EVALUATIONS assuming every
suffix node runs in every variant, and LLVM already dead-codes each
monomorphization, so it is not a wall-clock ratio. The honest claim is
narrower: a node whose value depends on exactly k buttons is computed 64
times where 2^k would do. For steady that is 24 nodes at 64-vs-2, 75 at
64-vs-4, 19 at 64-vs-8, 18 at 64-vs-16, 10 at 64-vs-32, 32 at 64-vs-64.
`transpile::fuse` already does this by hand for the FUSED artifact (M1
stage 2, "support segments"), and it measured **-2.6% wall**. Expect the
same order here, not 2.8x.

### The shape

Order the used bits and give each node a LEVEL: the position of the
highest-ordered bit in its cone (0 = no button reaches it). Then emit one
function per level, each computing only its own level's nodes:

```rust
fn frame(..)                      { <level 0>; s0::<false>(..); s0::<true>(..); }
fn s0<const K0: bool>(..)         { let kbA = K0; <level 1>; s1::<K0,false>(..); s1::<K0,true>(..); }
fn s1<const K0: bool, const K1: bool>(..) { .. }
```

Const generic params can be FORWARDED, which is what makes this work on
stable: `s1::<K0, true>` needs no const arithmetic, while the more obvious
`s1::<{B | (1 << 1)}>` would need `generic_const_exprs`. The source stays
one copy per level; LLVM makes 2, 4, 8, ... of them. Emitting the tree as
2^k - 1 concrete functions instead would put 127 functions in the file and
trade a compile-time explosion for the same result.

`out(B, ..)` needs the numeric mask back: `let b: u8 = ((K0 as u8) << bitA)
| ..;`, a `let` rather than a `const` item, because a const item may not
name its function's generic parameters. LLVM folds it.

### Variant dedup rides along

Group the 64 masks by the signature `(output cells, ok, live)` and emit
`out(..)` only for a representative of each group - `if matches!(b, 0 | 1 |
3 | ..)`, const-foldable, so a pruned leaf's whole cone dead-codes.

The signature MUST include `ok` and `live`, not just the outputs. The
consumer (`src/compiled/dispatch.rs:684`) ignores the variant mask and
dedups rows, so equal outputs really are interchangeable rows - but the
same callback also accumulates `deopt_rows` from `kout.deopt & kout.valid`
and aborts the chunk on `kout.bd`. Two variants that write the same cells
while deopting different lanes are NOT interchangeable: dropping one would
silently keep a lane on the kernel that belongs to the interpreter.

Measured 2026-08-22, both ways: steady 36/64 distinct either way, r20-
steady 36/64, dash and r20-dying-spikes 4/64, frozen and r20-dying-fall
1/64. So the wider signature costs nothing here - but it is the correct
one, and the outputs-only number was an upper bound that happened to be
tight rather than a result.

### What it collides with

`transpile::fuse` reads `e.pre` / `e.suf` as exactly two regions
(`parse_member`). An N-level split breaks that model, so stage C either
lands after stage D (fuse ported onto the graph) or keeps the two-way
bucketing as a PROJECTION for the fuser. The projection is honest - same
nodes, same representations, bucketed 2 ways instead of N - but it is a
second view of one derivation and should not outlive stage D.

## What this does NOT solve

- **The exit member still needs multi-room handoff on top of this** -
  successors live in a different room with a different collision cache and
  level data. P1-P3 are necessary but NOT sufficient for it. Do not let the
  two get conflated.
- **It does not make room (2,0) fit.** That is the spd rung (#110), whose
  14.5x row collapse is still a SIMULATION with no measurement anywhere in
  BENCHMARK_DATA.md. This plan makes the engine better, not the search
  smaller.

## Size

Comparable to #163/#164, each of which was multi-day. Not a side quest.
