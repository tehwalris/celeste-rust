# Getting the graph by TRACING, instead of by rewriting

Philippe's design, 2026-08-22. Written down because it retires most of the
codebase and that is worth being precise about before starting.

Notation: unmarked text is the design as Philippe framed it. Blocks marked
**[check]** are things I looked up or worked out to test it, including the
places I think it needs more than was said.

---

## 1. The claim

We currently get a pure graph like this:

```
Lua -> IR -> ~13,000 recipe-driven REWRITES -> a branch-free, call-free,
       straight-line `__frame` -> emit-time walk against a shape witness
       -> Graph
```

The rewrites exist to make the program straight-line so that the emit-time
walk can execute it symbolically in one pass. But the walk is *already* a
symbolic interpreter. So: make the interpreter do the whole job, and the
rewrites have nothing left to do.

```
Lua -> IR -> TRACE (symbolic interpretation, heap concrete, values
       symbolic, merge at joins) -> Graph
```

**[check]** The two biggest rules confirm this rather than merely permit
it. `mask_loop` (2,192 lines) is documented as "give a loop with a
per-lane trip count a uniform constant trip count, and turn per-lane
iteration into mask data" - which is what a tracer produces for free by
running the body `LIMIT` times with a running `active` mask.
`speculate_region` (2,958 lines) turns a conditional region into
unconditional code with phis becoming selects - which is what a tracer
does by construction when it merges two branch outcomes. In both cases
nearly all the lines are the cost of expressing it as *source-to-source
CFG surgery*: block splicing, phi fixing, dominance. A tracer never builds
a CFG, so none of that machinery exists.

## 2. The mechanism

**Keep the heap concrete.** Same shape model as today: a real GC heap,
real object identity, real field offsets. This is what makes pointer
topology resolvable at trace time, and it is why the graph has no
load/store ops at all.

**Make the VALUES symbolic.** Today an interpreter value is a concrete
number, a vector of numbers, an interval, a boolean. Add one more: a
handle to a graph node. Almost everything becomes that; concrete values
stay available and are almost never needed.

**Straight-line execution is then obvious.** For `a + b`: look up `a` and
`b` in the heap, get their node handles, intern an `Add` node over them,
store the new handle back. Identical in structure to what the interpreter
already does for scalars and vectors - only the payload type changed.

**Branches merge instead of exploding.** At a two-way branch:

1. take the state (heap and all), run the left arm, get a state;
2. take the same state, run the right arm, get a state;
3. GC both;
4. if the two states have the SAME SHAPE, insert a `Sel` on the branch
   condition for every heap cell, and you are back to one state with one
   heap and one set of node handles. No explosion.
5. if the shapes differ, keep both. This is bounded: we already specialize
   programs to heap shapes, and there are very few.

This is the same trick the current interpreter already uses - it keys
states by shape and merges at function boundaries specifically to stop the
state count exploding. Nothing new is being invented; the payload is
different.

**[check]** Step 4 needs one fold that does not exist yet:
`Sel(c, x, x) -> x`. `Graph::fold` collapses a select only when the
*condition* is decided. A per-cell merge proposes a select for every cell
in the heap and the overwhelming majority were untouched by either arm, so
without this the graph grows by ~(heap size) nodes per join. There is a
second, subtler half: `kernel::select` records `Known(cond)` as a validity
conjunct at every select site, so merging unchanged cells would also deopt
lanes whose condition is undecided, over selects that cannot depend on the
condition. Both are two-line fixes; neither is hard; both are required
before merging is affordable rather than after.

## 3. What this deletes

| | lines |
|---|---|
| `src/rewrite/rules` | 27,513 |
| `src/rewrite` (recipes, verify, drivers) | 10,613 |
| interpreter machinery that exists only for RUNTIME speed | ~4,100 |

**[check]** All ~37 rules are live - every one is invoked by a checked-in
recipe, `inline` 5,166 times down to `strip_kills` 5 times. So this is not
"delete dead code". The rules are dissolved by removing the need for the
data structure they operate on, which is a different and better argument.

The interpreter is vectorized and tuned because it is the real runtime
fallback: `vectorize.rs` (2,716) and `virtual_merge.rs` (1,428) are state
merging and dedup for the SEARCH, not for executing one frame. Tracing
needs none of it. A trace runs once at build time over one abstract state;
it can be as slow as it likes.

## 4. What does NOT dissolve

I claimed three things survived as tracer policy. Philippe pushed back on
all three and was right about all three; the corrected version is much
smaller, and the corrections are the interesting part.

**Loop bounds - performance only, not correctness.** ~~The trip counts
survive as policy.~~ They do not need to be per-site recipe content at
all. The loop handler runs a HEURISTIC that decides when to stop
unrolling, and emits a validity predicate asserting the loop had actually
finished by then. Get the heuristic wrong and the predicate fails - a
crash we notice, not a silently wrong graph. So the heuristic is tuning,
and correctness is carried by the guard.

(The part of my objection that stands: there is no fixpoint to be had. A
node handle has no lattice, so merging iteration n with n+1 gives
`Sel(Sel(..))` and grows without converging. Unrolling is not one option
among several - it is the only one. But bounded unrolling plus a guard is
all that needs saying.)

**Pins - not needed.** ~~The tracer needs the same pins or it refuses.~~
`assume_eq` and `pin_builtin` exist because the REWRITES DO NOT EXECUTE
ANYTHING. Without running the program they have no visibility into any
value, so anything they want to rely on has to be assumed and guarded. A
tracer is executing, in a specific state. Only numbers and booleans are
ever symbolic; pointers, functions and table identities are concrete
because the trace put them there. A loop counter is a trace-time value, so
`objects[i]` has a concrete `i` - which is exactly the case `pin_builtin`
was invented to recover. Being in a specific state IS the assumption, and
it needs no rewrite to express.

(If an index ever genuinely depends on symbolic data, the tracer refuses
loudly. That is a refusal, not a policy.)

**Merge vs speculate - do not optimize in the tracer.** ~~Default to
merge; speculate where it measures.~~ Wrong principle. The tracer should
do whatever most simply yields a COMPLETE and CORRECT heap-free graph, and
nothing else. Optimizing over the finished graph is easier, more reliable
and far better observable than arranging for the tracer to happen to emit
something already near-optimal. Speculation is a graph optimization; it
belongs after the trace, if at all.

**So almost nothing survives.** My "~3,000 of 13,000 recipe entries become
policy" was wrong in the same way three times over - I kept importing the
rewrite pipeline's decisions into the tracer because that is where they
live today. With the corrections above the number is approximately zero:
loop bounds are a heuristic, pins are unnecessary, speculation is a
later-stage graph pass.

**Shape divergence - naturally handled.** Different shapes on different
branches are simply emitted as they need to be. Fusion over shapes is
implicit rather than a feature to build. This is the multi-output-shape
problem (`plans/multi-output-fusion.md` P2/P3): the steady / dying-fall /
dying-spikes members exist as three hand-written recipes precisely because
a branch changes the heap shape and today's fuser emits one output shape.
A tracer produces all of them from ONE trace. **The tracer and P2/P3 are
the same project**, which is an argument for doing the tracer instead.

## 5. Decisions taken

- **Keep the IR.** Tracing the Lua AST directly was considered and
  dropped: the IR is cheap, it is what the interpreter already runs, and
  tracing an AST means re-implementing scoping and closures. The IR is not
  what costs 27.5k lines; the rewrites over it are.
- **Generalize the existing interpreter; do not write a second one.** The
  op semantics must stay single-sourced or the tracer drifts from the
  reference. `MaybeVector<T>` is already generic over its payload and
  already collapses a uniform vector back to `Scalar` - the same shape of
  idea as the select fold. Swap the domain, keep the ops.
- **Rewrites go to their own crate first**, which the crate we iterate on
  does not depend on at all. Correctness of the move is trivial; the point
  is build time.
- **The interpreter is not being deleted.** It stays as the oracle, and
  eventually gets much simpler rather than removed: plain scalars, no
  vectorization, no virtual merge - the machinery that exists to make it
  fast enough to be the RUNTIME fallback. (Integer ranges probably stay;
  open.) The end state is a small interpreter that exists only to be
  checked against, and not much else.
- **The tracer does not optimize.** Complete and correct graph first;
  every optimization is a pass over the finished graph, where it can be
  seen and measured.

## 6. Why now

Both paths already produce a `Graph`. That was not true a week ago. So the
migration has an exact, cheap, incremental gate: run the tracer on the
same shape witness and diff its graph against the one the recipe pipeline
produces - node for node, or by evaluation, one class kernel at a time. No
flag day, and a discrepancy localizes to a node instead of to "the search
is wrong at frame 40".

It also answers the one thing worth worrying about - whether merge-inserted
selects make the graph bigger than speculation does, and therefore the
kernels slower - on day one, before any code generation exists.

---

# Stages

## Stage 1 - make the loop we are working in fast

No behaviour change. Everything here is motivated by build time; see
`plans/build-time.md` for the measurements.

1. Split `celeste-rust` (40k lines, one crate, `lto = "fat"`,
   `codegen-units = 1` - so editing the emitter recompiles the
   interpreter and five binaries):

   ```
   celeste-ir         IR, Program, Lua compile        deps: core
   celeste-interp     the interpreter                 deps: core, ir
   celeste-rewrite    rules, recipes, verify          deps: core, ir, interp
   celeste-transpile  graph, lower, kernel, names,    deps: core, ir, names,
                      fuse                                  rewrite
   celeste-rust       search, compiled dispatch, bins deps: all + kernels
   ```

   `celeste-transpile` does NOT depend on `celeste-kernels`, which removes
   the bootstrap cycle (today a change to an engine primitive's signature
   can leave the workspace unable to build the tool that would fix it)
   rather than working around it.

   `celeste-rewrite` is a leaf as far as the iteration loop cares:
   `celeste-transpile` depends on it only to load recipes and build a
   program, and cargo rebuilds dependents, not dependencies - so editing
   the tracer never recompiles 38k lines of rules.

2. A profile for the edit loop: `lto = false`, `codegen-units = 16`.
   Separate target dir, so release artifacts and every benchmark in
   BENCHMARK_DATA.md stay valid. **Do not touch `[profile.release]`.**

3. Land the two select folds from section 2, since they are prerequisites
   and cost nothing.

Exit: editing the tracer crate rebuilds the tracer crate.

## Stage 2 - the tracing value domain, on straight-line input

Make the interpreter's op semantics generic over the value payload, and
add the symbolic payload (a graph node handle plus its type). Heap stays
exactly as it is.

Trace the ALREADY-REWRITTEN program - it is straight-line, so there is no
control flow to handle yet. This isolates the value-domain change
completely.

Gate: the traced graph equals the graph `emit_walk` produces today, for
all eight class kernels. Same input, same output, new machinery. If it
does not match, the difference is in the value domain and nowhere else.

Exit: `emit_walk` is deleted and the tracer feeds `transpile::lower`.

## Stage 3 - control flow

Add, in this order, each gated by graph comparison against the recipe
pipeline on the un-rewritten program:

1. **Joins, same shape.** Per-cell `Sel` merge.
2. **Joins, differing shape.** Keep both; n output states. This is where
   P2/P3 arrives - the kernel emitter needs per-member validity and
   per-member output shapes, which the tracer now supplies naturally.
3. **Loops.** Unroll under a stopping heuristic, with a predicate
   asserting the loop had finished. Heuristic wrong -> loud failure, never
   a wrong graph.

No speculation, no folding-for-size, no cleverness: complete and correct
first. Measure graph size against the recipe pipeline's anyway, because
that number is what says whether a later optimization pass is needed and
which one.

Exit: the tracer produces every class kernel's graph from the un-rewritten
program.

## Stage 4 - delete

- `celeste-rewrite` (38k lines) - the crate nothing depends on any more.
- The interpreter's vectorization and virtual-merge machinery (~4.1k), but
  ONLY once the compiled path covers every lane. Until then it is the
  deopt fallback and has to stay.

The interpreter itself STAYS - it is the oracle. What goes is what makes
it special: vectorization, virtual merge, the lane machinery. The end
state is a plain-scalar interpreter that exists to be checked against.
(Whether it keeps integer ranges is open.) It already runs the
un-rewritten program, so nothing has to be built for it to hold that role
- only removed.
