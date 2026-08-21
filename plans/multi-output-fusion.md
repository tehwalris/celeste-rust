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

0. **Census before P2/P3.** Split room (2,0)'s f42 miss tranche by cause.
   It is recorded today as one lump: "f39+ fruit-touch: 786,848 lanes are
   guard/bind refusals ON the covered a9e20c0a shape (class-leaving,
   bounce, dying) and 239,814 are the 409-cell spring-delay residual shape
   681d". Only the shape-change-with-survivors slice is what P2/P3 buys,
   and it is UNMEASURED. `CELESTE_KERNEL_MISS_DUMP` already produces the
   raw material.
1. P1 alone, gated and measured on the members that exist.
2. P2 + P3 together (they are one coupled change; splitting them only
   yields a broken intermediate).
3. Gates, then measure per room - same discipline as engine adoption
   (BENCHMARK_DATA.md, "Engine adoption validation at depth").

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
