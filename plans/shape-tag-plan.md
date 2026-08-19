# Specialization sets + fusion

Status: DESIGN v2 (2026-08-19), agreed direction with Philippe. This file
previously held the "death as collideable:=false" design; that was a
game-semantic shortcut (correctness rested on a whole-program "who reads
this" argument, not a local premise) and is superseded by this, which is
generic. History of the three prior attempts and their refutations:
BENCHMARK_DATA.md "K2's pm1 death-partition fix", rewrites-death.jsonl's
header, and the K2 census in dedup-roofline-plan.md.

## The idea

Per input shape, the campaign carries not ONE recipe but a SET of
alternative recipes. Each member is a complete, independent,
individually-verified recipe that pins some structurally-divergent
branch to one polarity - using the machinery that already exists for
exactly this (`guard_branch`: assert_true the condition, delete the
other edge; it is how the steady/dash/frozen overlays are built today).
An assertion IS a specialization condition. Each member ends in its own
output heap shape.

Key realization: **rewrites.jsonl already IS one of the members** - its
collapse_loop/assume_eq assert family (`count(objects)==1`,
`objects[1]==this`) is the "kill branch false" premise. The death case
needs ONE new member: the same prefix + `guard_branch{taken:true}` on
the kill branch + dce + kill_dead - the exact recipe pattern of the
frozen overlay (which pins the early-return edge and collapses to 236
instructions). With the kill pinned, the heap ops resolve statically,
the collide loops fold, and the tail collapses to the restart-global
stores.

**Fusion** is then generic compiler logic with no game knowledge: the
members' bodies are pure, heap-free, branch-free expression graphs
(loads/stores are emit-time renames already - the 387 loads/129 stores
become register traffic); fuse them by value numbering. Shared nodes
(almost everything - the prefix and most reads) are computed once;
divergent nodes are predicated on the SELECTOR, which is not a new
artifact: the complementary `assert_true c` / `assert_true !c` pair
becomes it. Exhaustiveness of the guard set is the soundness side
condition (trivial for a complementary pair); classes no member covers
keep today's deopt as the safety net, so coverage is incremental and
never silently wrong.

The fused artifact gets its OWN representation - a node list over lane
columns - and its own simple executor. The CFG IR interpreter stays as
the reference and the fallback for uncovered classes. Fused programs
are GENERATED, not checked in (Philippe 2026-08-19; revisit if the
regen loop wants it).

**M1 (#152) is the same engine at n=64**: input-pinned members with
guards `input == k`, exhaustive by construction. The measured prize
there: 12.4 output rows per input lane, 8 of 9 bit-identical to a
sibling - work that fusion shares at compile time instead of hashing
away afterwards. The emitter's button-taint prefix/suffix cut is the
existing 1-cut approximation of this and gets subsumed.

## What exists vs what is new (survey 2026-08-19)

| needs | exists | new work |
|---|---|---|
| pin a branch under a premise | `guard_branch` (rules/guard_branch.rs) - proven on 3 class overlays | none for the death pair |
| branch-free bodies | done: 0 branches, 0 phis in the overlays; masked stores are already selects | none |
| node/expression list | NOTHING - transpile/kernel.rs streams text; `K` (kernel.rs:36) is the right value lattice | the arena; nodes dispatch into celeste-engine::kernel primitives |
| lane executor over a node list | deleted in K4 (Rt2's map1/map2/select_inner arena, `git show 23b96bc^`); kernel primitives (W=16, u16 deopt masks) survive | resurrect the arena shape as an interpreted node-list VM |
| cross-variant CSE | `cse` is per-function IR-level; `isocheck`'s canonical walk is the numbering precedent | the fusion pass proper (M1's "compiler project") |
| input pinning | emit-time only (`UBool{btn}` -> const-generic monomorphization) | graph-level variant attribute (or IR const-substitute; not needed for the death pilot) |
| guard -> selector | asserts emit `*bd=true`/`zguard` today (deopt) | reinterpret complementary guard pairs as select conditions during fusion |

Notes for the fusion pass: `cse`'s Pure keys are deliberately
BLOCK-LOCAL ("rematerialize, don't carry"), a cost decision tuned for
the interpreter's LocalEnv that INVERTS for a column executor - the
fusion CSE must be global. Fork loops (`zi_fork_flr`) and the
prefix/suffix cut become node attributes rather than emitted control
structure.

## Phasing - and why there is a Phase A before any fusion

**Phase A: specialization-set pilot with the EXISTING interpreter as
the executor of both members. No new representation.** Derive the dying
recipe (guard_branch{taken:true} on the kill branch + dce + kill_dead;
verify with existing tooling). Change the deopt TARGET: lanes that fail
the alive member's premises re-run under the DYING member instead of
the plain program (lane-granular collection exists, #80). The dying
member is tiny, so the retry is cheap - which also removes
collect-first's reason to exist (its tagged-mode tax, 307 s thread-CPU
at H=68, is a hedge against expensive retries). Expected to capture a
large slice of the measured ~94%-of-frame-body deopt cost with ~no new
machinery. Gates: dying-recipe differential verify; H=68 rowkey
set-identity vs ~/perf-scratch/k2ctl; deopt-to-plain lanes -> ~0;
collect-first-off pair.

Caveat to verify in Phase A: the dying member's premises must hold for
the LANES routed to it (they die this frame) - the guard pair must be
checked per-lane exhaustive, and lanes failing BOTH (should not exist)
still fall to plain, loudly counted.

**Phase B: the graph form + executor + fusion at n=2.** Node arena,
value-numbering fusion of {alive, dying}, guard pair -> selector,
boundary materialization by selector value (the split_by_liveness step
in split_precision_straddles' seam - unchanged from design v1). The
two-member case is the ideal test: tiny divergent tail, huge shared
prefix. Gate: per-class mechanical check (fused restricted to class i
== member i, for every i) + the same H=68 set gate.

**Phase C: n=64 input fusion (M1) + engine adoption decision.** This is
where the fused engine is expected to finally beat the interpreter
under CELESTE_FRONTIER_ONLY (the compiled engine currently loses 9-13%
there because per-row fan-out dominates - input fusion attacks exactly
that). Only after C does "fused engine as campaign default" get
decided, by measurement, against the then-current interpreter baseline.

## Verification, layered

1. Each member: existing per-recipe machinery (build, differential
   verify, screen; suite).
2. Guard-set exhaustiveness: complementary-literal check per pair;
   runtime counter for both-fail lanes (must be 0; plain fallback).
3. Fusion: per-class replay equivalence, every class.
4. End-to-end: H=68 per-frame rowkey set-identity vs k2ctl, deopt -> 0,
   then collect-first-off; suite; ladder configs unmixed
   (k2ctl config vs the d4run WIN_AT_XY config - do not compare across).

## Open questions (for Philippe)

- Phase A first, or straight to B? A is cheap and captures most of the
  deopt prize early, but it builds a two-pass structure fusion later
  deletes.
- Fingerprint story for recipe SETS: hash the member list + fusion pass
  version? (Variant list is deliberately un-fingerprinted today;
  members that change the executed program are different.)
- The dying member's derivation: share rewrites.jsonl's full prefix
  (S2 pattern) or a minimal prefix? Full prefix keeps the shared-prefix
  CSE trivial later.
- Composition of guard sets (death x input): fuse pairwise with product
  tags, or n-way in one pass?
