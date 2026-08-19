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
fusion CSE must be global. It is also a DIFFERENT KIND of CSE
(Philippe): it operates on the new graph IR (value numbering over pure
nodes), not on the CFG IR - do not extend rules/cse.rs; build it as
part of the fusion pass. Fork loops (`zi_fork_flr`) and the
prefix/suffix cut become node attributes rather than emitted control
structure.

## Phasing

(A deopt-to-specialization intermediate - re-route failing lanes to the
dying member instead of the plain program, no new representation - was
considered and DECLINED by Philippe 2026-08-19: "go for the full thing
instead, since that's the end state I want." If fusion stalls it
remains the documented fallback, together with #155.)

**Phase B (START HERE): the graph form + executor + fusion at n=2.**
First derive and verify the dying member (guard_branch{taken:true} on
the kill branch + dce + kill_dead - the frozen-overlay pattern;
existing per-recipe tooling). The guard pair must be per-lane
exhaustive; lanes failing both (should not exist) fall to plain,
loudly counted. Node arena,
value-numbering fusion of {alive, dying}, guard pair -> selector,
boundary materialization by selector value (the split_by_liveness step
in split_precision_straddles' seam - unchanged from design v1). The
two-member case is the ideal test: tiny divergent tail, huge shared
prefix. Gate: per-class mechanical check (fused restricted to class i
== member i, for every i) + the same H=68 set gate.

**Step 1 DONE (2026-08-19, commit 7f0adf0 + BENCHMARK_DATA.md "Dying
members GATED").** What it settled:

- The kill is TWO branches (spikes at lua:93, y>128 at lua:97), and both
  deaths are reachable in room (1,0) - so the member set for the player
  shape is n=3: {alive, dying-spikes, dying-fall}, an exhaustive guard
  TREE on two literals, not a single complementary pair. The fusion
  machinery should take n members from the start (n=64 needs it anyway).
- A dying member is NOT base + guard tail: the base's h010-h019 /
  h028-h037 premises (count==1 in player.update's collide loops) run
  post-kill and are falsified - the dying recipes OMIT them and leave
  those loops real (zero-trip dynamically, uniform branch). No new rule
  needed: zero-trip collapse for the fusion-facing branch-free form is
  guard_branch{taken:false} on the loop heads.
- `verify` cannot run a member (premise fails on most frames, no deopt
  in verify) - `rewrite membercheck` is the concrete member gate, and a
  resumed bench with `--variant 'player=<member>'` + rowkey set-identity
  is the abstract one. Variant dispatch is chunk-granular (mixed chunks
  fall back whole), so members do not cut deopt by themselves; the
  per-lane selector of the fused engine is what does.
- FUSION INPUTS must be branch-free, i.e. overlay-level: the campaign
  members verified here are the specification; the fusion pilot's
  members are (steady-class overlay) x {alive, dying-spikes, dying-fall}
  derived on the rewrites-trace10-steady base - same falsified-entry
  surgery + flipped kill pins + a branch census run over the DYING
  witnesses (tas/room_1_0_death_*.txt) to pin the post-kill tail.

**Phase C: n=64 input fusion (M1) + engine adoption decision.** This is
where the fused engine is expected to finally beat the interpreter
under CELESTE_FRONTIER_ONLY (the compiled engine currently loses 9-13%
there because per-row fan-out dominates - input fusion attacks exactly
that). Only after C does "fused engine as campaign default" get
decided, by measurement, against the then-current interpreter baseline.

## Step 2 working notes (2026-08-19)

The transpiler survey settled HOW the graph form gets built: the kernel
emitter (src/transpile/kernel.rs) already does everything except keep a
graph - it folds the heap at emit time, types every value (the K
lattice), tracks button taint, and streams `let v{n}: TY = op(args);`
lines through `Emit::bind` with NO memoization. The emitted kernel body
IS the member's pure branch-free expression graph, in SSA text. So the
graph IR is a REFACTOR, not a parallel system:

1. `bind()` appends `Node { name, ty, expr }` to a node list; rendering
   the node list reproduces today's text BYTE-IDENTICALLY (gate:
   `generated_is_current` + regen diff). Fork loops, the prefix/suffix
   cut and the pins become node/list attributes.
2. Exprs get structured (op head + arg names) - the text is
   machine-generated, so a tiny parser at bind time is safe.
3. Fusion (step 3) value-numbers across the n members' node lists;
   the executor (step 4) is an interpreted VM dispatching the same
   celeste-engine zn_*/zi_*/zb_*/zsel_* primitives per node.

Dying-overlay derivation (step 2a) correction: trace10's straightening
entries (xc/z families) were derived on the COLLAPSED collide loops, so
with h010-h019 removed some no longer apply. They are all optimizations
- a dropped if_convert just leaves a branch the terminal pin sweep pins
instead - so the derivation is a mechanical DROP LOOP (build, drop the
failing entry, repeat), keeping the full shared prefix (which is what
fusion CSE shares) and relying on the pin sweep + dce for straightness
of the dying tail. Pin directions come from `membercheck --trace-frame`
on the checked-in death witnesses (the branch census's successor).

### 2b findings (2026-08-19, evening)

Both dying overlays EMIT through the kernel emitter today (after teaching
it `__array_table_drop_last`, a purely emit-time structural intrinsic -
the array cell shrinks, no runtime code): 1042-line prefix, 58-line
suffix, 16 button variants each. The two dying kernels differ in EXACTLY
the kill-guard lines - `zguard(v817)` vs `zguard(zb_not(v817))` plus the
y>128 literal - the complementary selector pair, explicit in the
artifact. Against the steady kernel the emitted text diverges earlier
(steady blends standing/moving lanes with ti_spd_select's masked
selects; the dying overlays pinned that gate), so fusion sharing is NOT
text identity - but steady's masked-select ARMS compute the same
moved-arm nodes, so value numbering recovers the sharing. Known open
items for steps 3/4:

- OUT_CELLS of a dying kernel still lists the dead player's cells
  (boundary-unreachable writes); the boundary materializer must emit the
  DEAD shape (objects empty, globals only) for selector=dying lanes and
  ignore those columns.
- The dying members observe button bits [0,1,4,5] -> 16 variants; the
  fused artifact's suffix handling must reconcile per-member observed
  bit sets.

### Fused emission design (2026-08-19, for step 3 completion + step 4)

Inputs: the n KernelGraphs (c44a231) + the shared value numbering
(transpile::fuse). Emission:

1. **Selector extraction.** Number all members against ONE interned
   table. Walk each member's Raw guard lines (`zguard(v, ..)`); a
   guard whose operand's canonical id appears in another member NEGATED
   (zb_not of it) is a SELECTOR literal; the rest stay member-local
   guards. For {steady, dy-spikes, dy-fall}: sel_spikes (the spikes ZB)
   and sel_fall (the y>128 zn_gt). Exhaustiveness check: the members'
   selector-literal sign patterns must cover the full truth table of the
   literals mentioned (steady: !s∧!f; spikes: s; fall: !s∧f - covers all
   four assignments since spikes=s subsumes s∧f and s∧!f... verify per
   set and REFUSE emission on gaps: uncovered patterns keep deopt).
2. **One pass, member-tagged nodes.** Emit nodes in interleaved
   canonical order: shared nodes once; a node reachable from only some
   members carries their member set. Per-member deopt masks
   `dp_m: u16`; member-local zguards write their own dp_m. A lane's
   member id = the first member (fixed priority order) whose selector
   pattern matches AND whose dp_m bit is clear; lanes matching none
   deopt to the interpreter (loudly counted).
3. **Outputs.** Per-member out_fields; the callback gains the per-lane
   member-id column. Boundary materialization: alive members produce
   the steady shape as today; DYING members produce the dead shape
   (objects empty; globals deaths/will_restart/delay_restart/freeze from
   their out cells; the dead player's OUT_CELLS ignored). The dead-shape
   Rt2 template can be built once from any dead boundary state (the []
   shape the campaign already dedups).
4. **Suffix.** The fused suffix observes the UNION of member kb bit
   sets; per-member tainted outputs computed under the same B.
5. **Artifact + executor (step 4).** Fused programs are GENERATED, not
   checked in. Two execution options, decide by build: (a) render the
   fused graph as Rust into a scratch module compiled with the workspace
   (like kernels but git-ignored/OUT_DIR); (b) the interpreted node-list
   VM over the engine primitives (K4-deleted Rt2 arena shape,
   `git show 23b96bc^:native-probe/src/runtime2.rs`). (a) reuses the
   whole existing dispatch plumbing (FrameEngine registry entry with
   3-member coverage) and is likely the shorter path to the H=68 gate;
   (b) is the plan-of-record's own representation and avoids build-time
   codegen; the gate applies to either.
6. **Gates (step 4).** Per-class replay: fused restricted to member i's
   selector class == member i's kernel on saved real rows (k2ctl frames
   have the dying frames f58+); then the campaign H=68 rowkey
   set-identity vs ~/perf-scratch/k2ctl with the fused engine handling
   the player shape, deopt 399,516 -> ~0 for covered profiles, fallbacks
   loudly counted; then collect-first-off pair; suite.

## Verification, layered

1. Each member: existing per-recipe machinery (build, differential
   verify, screen; suite).
2. Guard-set exhaustiveness: complementary-literal check per pair;
   runtime counter for both-fail lanes (must be 0; plain fallback).
3. Fusion: per-class replay equivalence, every class.
4. End-to-end: H=68 per-frame rowkey set-identity vs k2ctl, deopt -> 0,
   then collect-first-off; suite; ladder configs unmixed
   (k2ctl config vs the d4run WIN_AT_XY config - do not compare across).

### Steps 3+4 AS BUILT (2026-08-19, commits 53a63fa + ab97067)

Implemented with two simplifications over the design above, both
strictly sound (BENCHMARK_DATA.md "Fused specialization-set kernel"):

1. **No explicit selector extraction.** Member coverage = the member's
   own deopt register set (`dp_m{member-bitmask}`, one register per
   distinct member set with effects; a member's dp is the OR of the
   registers naming it). A lane belongs to the FIRST member in priority
   order whose dp is clear; complementary kill-branch guards make the
   dying members disjoint wherever the selector is known; uncovered
   lanes deopt loudly. Static truth-table exhaustiveness is subsumed by
   the runtime uncovered count + the H=68 set gate.
2. **No dead-shape materialization.** The fuse pass PROVES each
   non-primary member's boundary rows block-uniform (`reachable_cells`
   of the member's final heap: every lane-varying out cell is a
   deleted-player field; survivors are 4 uniform globals) and the
   executor keeps ONE representative lane per distinct uniform tuple
   per chunk, routed through the EXISTING interpreter deopt path.
   Dropped lanes' rows are member-certified identical to the rep's, so
   the row SET is preserved by construction - no dead-template, no new
   boundary code, no new trust base.

Machinery: `kernel::emit_walk`/`render` split (one lowering, two
consumers; regen byte-identical), `compute_out_fields`/`emit_interface`
shared so the fused artifact IS a class kernel to the dispatcher,
`transpile --fuse` (artifact gitignored, feature `fused`),
`dispatch::fused` runner ahead of the steady kernel (CELESTE_FUSED=0
opt-out). The fused engine rides the COMPILED-FORWARD path
(CELESTE_COMPILED_FORWARD=1) - the default campaign never reaches
kernel dispatch.

Status: **steps 3+4 COMPLETE and gated.** H=68 f066-068 rowkey sets
IDENTICAL to k2ctl with the fused engine live. The kb5 residual is
CLOSED: the pinning entry was `dygb_anonymous_61_in_h061_in_k1039_cont`
(the dash-gate branch, displaced to the k1039 cont block by the dying
lineage's 0-trip loop pins), replaced in place with `dyzt_dash_blend`
(the zt001 pattern), certified by two new dash-press death witnesses.
The corpse dash arm's two live globals (freeze, has_dashed) are
per-lane, which forced the proof's planned generalization: a per-lane
reachable out cell is admitted iff it is the PRIMARY'S OWN out column
(node identity after CSE), and the executor extends the collapse key
with its per-lane value (`dy_vary_key`). Uncovered events 1.58M -> 0;
11.5M covered events -> 3,864 reps over a full 68-frame run.

Two expectations from the paragraph above were WRONG, by measurement:
- "deopt 66k/frame -> reps only" - that counter is the DEFAULT
  program's premise-failure population (the interpreter FORKS on the
  kill branch; it never deopted dying lanes), identical in the pure-
  interpreter control. Coverage shows up as interpreter WORK removed.
- The timing: full-68-frame pair, frontier-only, no collect-first:
  default 115.63s vs compiled-forward+fused 114.26s - PARITY (one run
  each). The former 9-13% compiled-forward penalty is gone; the engine-
  adoption call is now a wash at H=68 on room (1,0), to be revisited
  where kernels bind more of the population (deeper H, other rooms).

### Phase C scoping measurement (2026-08-19, from the v6 timing pair)

Thread-seconds inside fwd.interpret, full 68-frame production runs:

|                              | default | fused  |
|------------------------------|---------|--------|
| frame body                   | 638.7   | 714.4  |
| ...deopt specialized run     | 138.0   | 110.4  |
| ...deopt plain re-run        | 238.4   | 224.1  |
| frame body EXCL deopt        | 262.3   | 379.9  |
| boundary prepare             | 44.9    | 22.1   |

Two implications for Phase C ordering:
1. The fused kernel path costs MORE thread-seconds than the
   interpreter it replaced (380 vs 262 excl deopt) and wins wall time
   only via halved boundary prepare + parallel shape. That 118-ts gap
   is M1's direct target (per-variant suffix x64 + append_out; the
   suffix is 120 lines/variant vs a 792-line shared prefix, and most
   suffix nodes depend on a SUBSET of kb bits - CSE by kb-support
   evaluates each node once per distinct support value).
2. The DEOPT RE-RUNS are ~45% of all interpret thread-seconds in BOTH
   engines (334-376 ts) - the 66k lanes/frame failing the default
   program's premises (visible trigger: the wall-slide gate assert at
   in_h061_if_join_24). This is engine-independent and is exactly what
   ADDITIONAL MEMBERS attack - the machinery for which is now fully
   built and gated. A member (or premise-blend, as with the corpse
   gates) for the dominant premise-failure class may be a bigger,
   cheaper prize than M1. Census the deopt triggers first.

## Open questions (for Philippe)

- Fingerprint story for recipe SETS: hash the member list + fusion pass
  version? (Variant list is deliberately un-fingerprinted today;
  members that change the executed program are different.)
- The dying member's derivation: share rewrites.jsonl's full prefix
  (S2 pattern) or a minimal prefix? Full prefix keeps the shared-prefix
  CSE trivial later.
- Composition of guard sets (death x input): fuse pairwise with product
  tags, or n-way in one pass?
