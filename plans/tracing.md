# Getting the graph by TRACING, instead of by rewriting

Philippe's design, 2026-08-22. Written down because it retires most of the
codebase and that is worth being precise about before starting.

Notation: unmarked text is the design as Philippe framed it. Blocks marked
**[check]** are things I looked up or worked out to test it, including the
places I think it needs more than was said.

---

## STATE, end of 2026-08-23

The campaign works end to end for one room. Read this section and the
three it points at; the T-numbered entries below are the working log and
are only worth reading when you need the reason for something.

### What runs

`the_room_runs_on_kernels_alone` (traced-kernel-check): room (1,0),
frames 1-30, on GENERATED KERNELS ONLY - no interpreter anywhere in the
execution path - and every frame's row key SET is identical to
`AbstractRun`'s, up to 27,024 rows at frame 30.

Gates, all green: the 609-test suite, `generated_is_current` both
variants, and the per-frame graph check (1,393,224 values).

### The numbers, all measured today

| | |
|---|---|
| speed vs the interpreter | **1.85x slower** (405 ms vs 219 ms, 30 frames, single-threaded) - was 2.75x before constant columns |
| row amplification | **46x**: 1.25M rows written to keep 27k |
| ...decomposed | 1.4x static, ~2.7x per-lane, ~2.7x cross-lane, 5.9x only-the-widenings |
| codegen | **16.3 instructions per graph node**, 48% stack traffic, 6.6% vector |
| live values | 512 peak - but **493 are OUTPUTS**; the computation's working set is ~19 |
| build loop | ~55 s (was 25 min until `black_box` on the fork trip count) |

### The design that follows, agreed with Philippe

ONE change, not four: the kernel computes `_update` + `_draw`, dedups AT
WRITE TIME against a rung-aware key, and stores survivors straight into
their output columns - emitting per output GROUP rather than per
variant, which is also what removes the fork's runtime loop.

Each piece has a measurement behind it; see "Where the row amplification
actually lives" for the derivation, including why the key is nearly free
(the emitter's shared/tainted split already partitions it) and why
specialization is a LAYER (shape, then rung, then pm1) rather than a
redefinition.

**The dangerous part**, kept visible on purpose: the rung-aware key is
where a mistake is SILENT. A key that erases something the boundary does
not erase drops real successors with no error. Derive it from the same
description the boundary widens from.

### Open

* Rust or direct assembly for the restructure. Today's spill numbers were
  measured on a shape this change dissolves, so re-argue it after.
  See "Philippe's codegen question".
* Whether the cross-lane half needs a frame-lifetime table on top of the
  per-slice dedup. A measurement, once the per-slice part exists.
* Deletion is still blocked on `gen.rs`, one room, one ladder rung, and
  the fact that the oracle IS the thing to be deleted. See "What is
  needed before deleting `celeste-rewrite`".

### Corrections made today, because the wrong versions were plausible

The build loop was never ten minutes (my polling, not a clock). The
mixed-domain select was not a mixed value. The static dedup is 1.4x, not
"most of it". The spilling is not inherent - 493 of the 512 live values
are outputs. Folding the widenings in is not dead-code elimination
(2.5%). Each is written up where it happened rather than quietly
replaced.

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

## Stage 2 - the tracer

### Which interpreter to build it on

The design says "we have an interpreter; make the heap concrete and the
values symbolic". There are two interpreters that could mean, and the
smaller one is the right answer.

**`celeste-interp` (16k lines)** has what the tracer needs structurally -
control flow, calls, closures, a GC heap, merge points (`hint_normalize`
blocks) - but its value domain is a 14-variant enum matched in ~700
places, and every one of those is LANE-VECTORIZED (`MaybeVector<T>`). The
tracer runs ONE state with no lanes, so all of that is dead weight, and
its merge (vectorize same-shape states into lane vectors) is not the merge
the tracer wants (insert a `Sel` per cell).

**`transpile::kernel::emit_walk`** is already a symbolic interpreter over
the same IR: concrete heap as a cell map, pointer topology resolved at
trace time, every value a graph node, stores as renames. It is what
produces the kernels today, so it is gated by the entire pipeline. It
bails on exactly three things:

```
terminator {:?} is not straight-line      (628)
call of non-builtin cell                  (880)
phi in the straightened program           (895)
```

Those three ARE the rewrites' whole contribution. So: extend `emit_walk`.

The counter-argument I made earlier - "op semantics must be single-sourced
or the tracer drifts from the reference" - does not survive contact with
the code. `emit_walk` is ALREADY a second implementation of the semantics,
and the thing that keeps it honest is a differential gate
(`compiled_forward_reproduces_the_interpreter`), not shared code.
Extending it adds no new semantics and no new drift surface. Building a
symbolic domain into `celeste-interp` would add a THIRD.

### T1 - `K` carries a `NodeId` (prerequisite, not preparation)

Today `K` carries emitted variable NAMES, and `Graph::operand` recovers the
graph's edges by parsing identifiers back out of the emitted text. A
tracer cannot work that way: merging two traced states means building
`Sel(cond, node_a, node_b)` from values that were never emitted as text
next to each other.

So `K`'s `String` payloads become `NodeId`, the walk's `Line` stream and
`Emit::scratch` are deleted, and `Graph::operand`'s text parsing goes with
them.

This is also a large simplification rather than a port. `K` has seven
representation variants (`SN`/`SI`/`SB`/`STri`/`ZN`/`ZI`/`ZB`) and the
walk's `unop`/`binop`/`cmp`/`select` branch on them to pick an emitted
form - but `transpile::lower` DERIVES representation now, so all of that
is redundant. `K` collapses to roughly `Num(NodeId)` / `Bool(NodeId)` plus
the structural variants (`Ptr`, `Nil`, `Str`, `UBool`), and several
hundred lines of representation dispatch in kernel.rs go with it.

**One question to settle first, cheaply.** `OutField::ty` comes from the
`K` variant and decides whether a boundary column is `Col::N` or `Col::I`
- which feeds the row key, so it is not free to change. `lower` already
checks `want.admits(have)`, where `have` is the graph's own derivation.
If `have == want` for every output cell of all eight kernels, the type can
simply be derived and `K` need not carry it. If any cell differs, `K` must
keep the boundary representation explicitly. Assert equality, regenerate,
and find out.

Gate: all eight kernels byte-identical.

### T2 - GC and merge

`reachable_cells` already walks globals -> cells; make it a real GC over
the trace-time cell map so two states are comparable. Then:

```
merge(cond, a, b) -> State
```

Same shape after GC: one state whose every cell is `Sel(cond, a_cell,
b_cell)`, which `Graph::fold` collapses to `a_cell` wherever the arms
agree - the overwhelming majority. Different shapes: keep both, and the
result is n output states rather than one.

Unit-testable without any CFG work.

### T3 - control flow

`ConditionalBranch` on a symbolic condition: trace both targets, merge at
the branch's IMMEDIATE POST-DOMINATOR, continue from there. Needs a
post-dominator pass (~80 lines, standard). The two arms' path conditions
are then exactly `c` and `!c`, which is what makes the merge a plain
two-arm `Sel` instead of a path-condition algebra.

`Phi { branches: [(Label, LocalId)] }` resolves at the merge: it names
which local to take from which predecessor, so it becomes the same `Sel`.
This is `rules::if_convert`, done by the tracer, at zero cost.

A conditional branch whose condition is CONCRETE just takes its arm - no
merge, no node. That is most of them.

### T4 - calls and closures

`emit_walk` bails on non-builtin calls because the rewrites inlined
everything. Tracing inlines by construction: save the local env, bind
arguments, walk the callee's CFG, restore. Closures need real capture
support - `CellT::Clo(String)` records only the function name today,
because `promote_capture` had already removed captures.

### T5 - loops

Unroll under a stopping heuristic, emitting a validity predicate that the
loop had actually finished. Wrong heuristic -> the predicate fails and we
notice; never a wrong graph. There is no fixpoint to reach for: a node
handle has no lattice, so merging iteration n with n+1 gives `Sel(Sel(..))`
and grows without converging.

### T5b - three boolean things (SETTLED, landed 032f05e)

The tracer has exactly three things of boolean type. Two earlier versions
of this section listed four and then reasoned about a `path` condition;
both were wrong and the corrections are the useful part, so they are kept
below rather than overwritten.

| thing | the question it answers | form |
|---|---|---|
| `Value::Bool` | what does the traced program compute? | a heap value |
| `State::guard` | WHEN does this state apply? | one `D::Bool` |
| `State::ok` | what must hold at RUN TIME for this to be right? | one `D::Bool` |

`guard` and `ok` are opposites, not degrees. A lane where `guard` is false
is not described by this state and a sibling describes it - nothing is
wrong. A lane where `ok` is false was computed WRONG and run time must
deopt. Downstream they are `Emit::live` and `Emit::ok`, and `ok`'s
negation is what emitted code spells `dp` (per-lane) or `*bd`
(block-uniform).

Nothing looks INSIDE either one. `guard` is built as `g and c` / `g and
not c` at a branch, OR-ed at a merge, and read only as a whole - as a
`Sel` condition, and eventually as the lane mask. `ok` merges by SELECT,
not conjunction: obligations are per-case, and conjoining would deopt
lanes for something incurred on a path they did not take.

INVARIANT the merge depends on: the guards of the outcomes in one frontier
are pairwise DISJOINT. Every fan-out is a split, so it holds by
construction - but `merge` selects with `t.guard` and would silently drop
`f`'s value on a lane both claimed, so a new source of outcomes has to
preserve it.

#### What was wrong twice, and why it is worth remembering

**First version: a fourth field, `State::path`,** a conjunction of assumed
literals, justified by "a merge condition must be recoverable
syntactically and an opaque node is not". The code disagreed: `path` had
exactly ONE semantic use, `find(|(l,_)| l == cond)` in `decide_on_path`
and `split_path`. Everything else was `differ_at_one`, the merge rule
guards replaced.

**Second version: derive the literals from the guard's conjuncts** by
walking the `And` spine. Also wrong, and wrong in a more interesting way -
it built a lookup to recover something thrown away one step earlier. The
scenario is `lua/celeste-minimal.lua:194`,
`v_input=(btn(k_up) and -1 or (btn(k_down) and 1 or 0))`:

1. `btn` splits on U and returns a CONCRETE true/false. The cart does this
   on purpose - `builtin_level_4.lua:31`, "this weird if statement
   concretizes the button state the first time it is read".
2. `collapse` merges the two states straight back and `Sel(U,true,false)`
   folds to `U`. The merge UNDOES the concretization.
3. `and` splits on U again and handed on `Bool(U)` - the node it had just
   decided.
4. The enclosing `or` needed the path to recover what step 1 established.

So `path` refereed a fight between `btn` splitting to gain concreteness
and `collapse` merging to cut states. Fixing step 3 - hand on the
CONSTANT, `!is_and`, since `and` keeps a falsy left operand and `or` a
truthy one - removed the need for step 4 entirely.

MEASURED, by counting path lookups reached and hit over a 25-frame trace:

| | frames 1-24 | frame 25 |
|---|---|---|
| before | 0 reached, 0 hit | 78 reached, 10 HIT |
| after | 0 reached, 0 hit | 68 reached, **0 hit** |

Then the field was deleted.

The general lesson, since it will come up again: **a mechanism that
recovers information the tracer already had is a sign the information was
discarded too early.** Look upstream before building the lookup.

### T5a - unmergeable returns (RESOLVED by guards, 032f05e)

`spikes_at` is `if .. return true elseif .. return true` inside a 3x3 tile
scan, and the frontier grew past 256 states. Bucketed by (flow, path
length) at the bail point: 276 `return` states over path lengths 4..43 and
53 `normal` ones - accumulation along a CHAIN, one rung per `elseif`, not
exponential branching. (An earlier note said 2^9; that was reasoned, not
measured.) All 276 returned the SAME value from a function that mutates
nothing.

Returns are not semantically special - a call gives back a list of
`(state, value)` and it does not matter which came from an early return.
`Flow::Return` only means "skip the rest of this body". It has one
property that matters: **a `return` escapes its join**, so the merge that
would have fired where the paths differ in exactly one literal never runs,
and by the function boundary they differ in twelve.

Nor was the merge POINT wrong: `exec_block` already carried non-`Normal`
outcomes forward and collapsed after every statement. The RULE was.
`collapse` now merges any two same-kind outcomes with a mergeable shape,
on `t`'s guard, and `differ_at_one` is gone.

Two numbers from the landing were explanations rather than measurements,
and both were flagged as such. One of them was wrong:

* frame 24 went 191 arena nodes to 425. Every split builds not+and+and and
  the arena never collects, so most of that should be guard construction
  that is dead after the merge - the live count was still not taken.
* frames 25-28 fanned out 12 -> 23 -> 38 -> 117 states, and the note said
  this was "consistent with real futures rather than a merge failure",
  since free buttons kill the player on some sequences and shapes then
  genuinely differ. **It was a merge failure.** See T7: `intern_body`
  minted a fresh id per evaluation, so two states that had built the same
  closure had different SHAPES. With that fixed the same frames go
  3 -> 5 -> 11.

  The tell was available and not looked at: "shapes genuinely differ" is
  checkable by printing the shapes, and the ten identical-looking outcomes
  differed in one field of one table. A story that explains a number is
  not the same as looking at what the number is made of.

### T6 - the gate

Trace the UN-REWRITTEN `__frame` and diff the resulting graph against the
one the recipe pipeline produces, per class kernel. Exact, incremental,
and it localizes a discrepancy to a node.

**What it actually needs, now that T7 exists.** `lower::emit_body` reads
far less of `Emit` than `Emit` contains: `graph`, `ok`, `live`, `uni`,
`vary_in`, and `of.fields` (cell id -> node). A traced `Frame` already has
four of those - `ok`, `live` is `guard`, the graph is the same
`transpile::graph::Graph`, and the outputs are nodes. The gap is the
NUMBERING: `of.fields` is keyed by canonical cell id and the tracer's
outputs are keyed by heap path.

So the missing piece is a `trace` counterpart to `compiled::bridge`, and
the shape of it is already fixed by the engine:

* `Rt2` is `structure: Vec<Cell2>` (`Val` / `Obj(field ids)` / `Arr` /
  `Clo` / `Bi`) plus `globals` indexed by `GLOBAL_NAMES`, numbered by a
  BFS that starts from the globals in `GLOBAL_NAMES` order. The tracer's
  heap maps onto that almost one-to-one; the only real mismatch is that a
  tracer `Table` has a hash part AND an array part where `Cell2` picks
  one, and no table in the cart uses both.
* Import gives every `Val` cell an `Op::Cell(canonical id)` leaf, which
  makes the tracer's cell ids the ENGINE's cell ids by construction
  rather than by a lookup table anyone has to maintain.
* Export gives `of.fields`, and the output shape falls out of the
  `structure` the traced state ends with - which is where Stage 3's
  multi-output-shape item stops being optional, since T7 measured FOUR
  output shapes for one frame of room (0,0).

Doing the numbering first is deliberate. Emitting a body against
tracer-local cell ids would produce Rust that compiles and plugs into
nothing, and the check for it would be `trace::eval`, which already
exists - so it would cost the work and buy no evidence.

### T7 - a frame as a FUNCTION, and the check (landed 608995b)

The tracer was a walk: run the cart, look at what came out. A kernel is a
function, so a frame needs a BOUNDARY - a fixed list of input cells and a
fixed list of output cells - and once it has one, the whole thing is
checkable end to end without any of the emitter existing yet.

`trace::iface` names a slot by its PATH from the globals table
(`objects[0].spd.x`) rather than by table id, because a frame can replace
an object and the id would then name nothing; `symbolize` swaps every
scalar under one subtree for an `Op::Cell` leaf. `trace::eval` evaluates a
traced graph at a point, delegating every operation to `domain::Concrete`
so that it is not a third definition of what the program means.
`trace::verify` runs the frame twice from one state - symbolically with
the player free, concretely with real numbers - and compares. Both sides
are the SAME interpreter over the SAME domain (the concrete side is the
symbolic one with every leaf already a constant, which folds), so a
disagreement can only be the compilation.

Today, 24 warm-up frames into room (0,0):

    24 input cells, 3 outcomes, 3,928 nodes for one frame
    6,720 points agree (448 declined), 384,000 field comparisons

Two design points that are load-bearing rather than incidental:

* **Perturb the inputs, and re-trace nothing.** Evaluating the graph only
  at the values it was traced at would pass for a graph that had folded
  every input away - the one bug this design is most exposed to. The sweep
  varies position and speed and crosses it with all 64 button assignments,
  and ONE graph answers for every point. That is the claim.
* **Compare every scalar reachable from the globals table, and compare the
  COUNTS.** Otherwise a missing output is a skipped comparison instead of
  a failure.

All 448 declines are one column, `spd.y = 8`: `move_y` steps further than
the unroll bound and the "loop finished" obligation is false. The refusal
working. Worth keeping one declining column in the sweep so that path
stays covered.

What it found immediately: `Interp::intern_body` minted a fresh `BodyId`
per evaluation of a `function ... end`, so two states that had built the
same closure were structurally different, and since a `Func` slot is part
of the shape they could not merge. Interning by AST pointer takes the
traced frame from 12 outcomes to 3. "Two outcomes with equal shapes" is
now an assertion.

Restrictions to remember, because the numbers above are only about the
program as restricted:

* only the player subtree is symbolized. Everything else is a
  specialization - sound for the check, since both sides specialize the
  same, but not yet a claim about the general frame.
* the frame is `_update()`; the pipeline's is
  `_update(); _draw(); __reset_button_states()`.
* the cell numbering is the tracer's own, not `celeste_names::FIELD_NAMES`.
  Lining those up is a separate job and doing it first would have meant
  debugging two things at once.

The multi-frame probe now stops at frame 28 (was 29) on `array index 3 is
past the end of a 0-element table`. Different merging, different path.

### T8 - a real oracle: differential corpora against PICO-8 (landed f7e9c93, 228a582)

Every oracle this project has had is another MODEL - the Rust interpreter,
and the OCaml one before it. That catches drift between them and nothing
at all that they both get wrong. PICO-8 is installed on this machine and
runs a cart headless (`pico8 -x`, `printh` to stdout), so there is no
reason for that to stay true.

`lua/probe/*.lua` is a corpus that runs UNCHANGED in both places - it
talks to the outside world only through `printh`, one value per call,
because the tracer implements neither `tostr` nor `..`. `*.expected` is
literally PICO-8's stdout, checked in; `./regen-pico8-golden.sh`
regenerates it. Two tests: one runs the corpus in the tracer and diffs
(works anywhere), one regenerates and compares BYTES (skips loudly where
PICO-8 is absent - not `git status`, since a golden file that is merely
staged is not stale).

It has already paid for itself twice, and in the same way both times: a
semantic I was confident about turned out to be wrong.

**`#` on a table with a hole.** Usually described as "undefined", which
invites picking an answer. It is not undefined - it is a deterministic
consequence of Lua's array-part/hash-part split, and it depends on how the
table was BUILT rather than what it holds:

```text
t={} t[3]="c"           #t == 0     -- dense-with-nils says 3
t[1]="a"                #t == 1
t[2]="b"                #t == 3
{"a",nil,"c"}           #t == 3     -- same contents as line 3, different #
{"a","b","c"} t[3]=nil  #t == 2
```

So `Table` has the three parts Lua has and `Table::len` is `luaH_getn`.

**Closure equality.** I argued for moving closures into the heap on the
grounds that `function() end == function() end` is false in Lua, so
structural equality answers it wrongly. PICO-8 says **true**: it is Lua
5.2, which caches closures on (prototype, upvalue cells). 5.4 removed
that; the folklore did not. The measured table is on `Value::Func`.

The tracer has the prototype exactly and approximates the cells by the
enclosing SCOPE, so `==` on two distinct closures is refused rather than
answered. Closures moved into the heap anyway - it matches the
interpreter, and it is the right shape for per-cell capture later - but
with the guard in place that move is observationally equivalent for this
cart, and it is worth saying so rather than dressing it up.

**The generalizable part.** Both of these were places where two models
agreed with each other and the runtime disagreed with both. The corpus is
where the next such question goes, and the cost of adding one is a few
lines of Lua plus a regen.

Known gaps the corpus surfaced rather than fixed: no multiple return
values (the cart uses none), no string concatenation, no `tostr`.

### T9 - the divergence audit (landed b510784, dec1684, and the ice commit)

The full report is kept verbatim at
`plans/audit-2026-08-22-tracer-vs-ir.md`, including the "categories
checked, nothing found" section. That section is the reason to keep it:
on a re-audit the useful record is not the bugs, which get fixed, but
what was LOOKED AT and found clean - which is exactly what a summary
throws away.

An Opus subagent read `src/trace/` against `crates/celeste-ir/src/frontend.rs`
plus the IR interpreter, construct by construct, settling every question it
could against real PICO-8. Philippe asked for it as "an extra sanity check";
it found more than the work it was checking.

**The rule that came out of it, which is now the standard for this
module:** every path either reproduces PICO-8 EXACTLY or RAISES.
Approximating is not a third option. Where the cart uses a feature, match
it; where it does not, make the path refuse. Both are valid; silently
differing is not.

Reachable, and fixed by MATCHING:

* **`break` was a no-op in `run_for_symbolic`.** `for_body` rewrote
  `Flow::Break` to `Normal`, so the state went back into the frontier and
  ran the body again. Reaches `move_x`/`move_y`. Masked in the values -
  it hits the same `break` next iteration - but it broke `ok`, since a
  lane that left early still had to satisfy "the loop finished within the
  bound".
* **A block was not a scope.** A `local` in an `if` arm landed in the
  enclosing function's scope. Never a wrong value (nothing in the cart
  shadows) but a merge failure, since a leaked name is in `Shape::scopes`.
* **Boolean `==`** compared `NodeId`s. **Assignment** evaluated its
  right-hand side first, where Lua does the left.

Reachable, and fixed by RAISING:

* **`tile_flag_at` answered `false` for every flag but 0.** Flag 4 is ice
  and `ice_at` gates acceleration and the wall-slide every frame.
  Measured over the map: **16 of 32 rooms contain ice**. It now answers
  only where the room provably has no such tile.

Unreachable, fixed by RAISING: `#` where the answer depends on rehash
history (see T8), and builtin arity (`min(5)` was a Rust index panic).

**The thing worth carrying forward.** The ice bug was wrong IDENTICALLY in
both implementations, so the differential machinery this project rests on
was structurally blind to it - as it was for `#` and for the closure
cache. Three findings, one shape: the two models agreed with each other
and the runtime disagreed with both. An oracle that is a second model can
only ever find drift.

Two of my own predictions were also wrong and were caught by measuring:

* I put the ice guard in the branch where every argument is known. With a
  symbolic player position `ice_at` has unknown x and y, so the call went
  straight past it into a graph node. Whether a room contains a flag does
  not depend on where in the room you look, so the flag has to be decided
  BEFORE the coordinates - which also folds it to a constant and takes
  the nodes out of the graph.
* I predicted the `break` fix would remove nodes and the block-scope fix
  would remove declines. Exactly backwards:

  ```text
  one frame of room (0,0)     nodes    declines
  before both                 4,063         448
  break fix only              4,060         192
  both                        3,447         192
  ```

Still open from the audit, both unreachable: the IR pipeline's
`local x = <expr mentioning x>` sees the new nil cell (the tracer is
correct here), and `run_for_symbolic`'s obligation could be more precise.

### T10 - the emitter join, and what a variant is worth (landed 2d0b65f..da06a33)

`transpile::lower` is the emitter the generated crates already use and it
consumes a `Graph`. The tracer produces a `Graph` with none of the
rewrites. They now meet: `trace::emit::lower_frame` builds an `Emit`
around a traced graph and `emit_body` lowers it.

    outcome            lines   variants   scalar cells
    0 player died         38          1             53
    1 no objects      10,801         40             34
    2 new room        18,576         40            104
    3 player alive    13,267         40             58

Three gaps, each found by running it rather than by reading:

* **`Op::And` had no general lowering.** Every `And` the old front end
  built was the `Known(x) AND x` idiom or a validity conjunct that
  `emit_body` FLATTENS, so an `And` was never rendered as a value, and the
  engine had no `zb_and`. A traced graph builds them freely - a guard is
  `g AND c`, `or` is De Morgan over two, and `ok` is a select tree whose
  arms are conjunctions. `zb_and` is Kleene and matches `Graph::fold`'s
  rule deliberately: the folder and the emitter must agree about what an
  AND means or a folded graph and an emitted one answer differently.
* **No uniform/per-lane classification for inputs.** `tile_flag_at` takes
  width and height as block-uniform `P8`. Stubbed by string-matching
  `.hitbox.`; see below.
* **Output kinds were derived from the node's op**, which is unknowable
  for `Op::Cell` and `Op::Sel`. `FrameOut` carries the kind now.

#### The two things between here and a kernel that runs

**Uniform vs per-lane.** The search runs a BLOCK of states, 16 to a SIMD
register, so every value is either per-lane (`ZN`, sixteen numbers) or
block-uniform (`P8`, one). `Ctx::derive` already propagates this as a
fixed point over the graph - `lane: args.any(|a| r(a).lane)` - so nothing
has to carry the annotation around. What is missing is only the SEED.

*(SUPERSEDED - see "the uniform/per-lane seed was the wrong question"
under T12. This paragraph claimed the seed is not derivable, on the
grounds that whether `hitbox.w` is uniform is a fact about how states
were grouped rather than about the program. Counting the witness says
otherwise: `hitbox.w` has a KNOWN VALUE, so it is a constant rather than
a uniform, and under specialize-per-key the classification is mechanical
- what the key and shape fix is a literal, everything else is per-lane.
The tracer does not declare anything; `symbolize` stops turning fixed
slots into cells.)*

**Boundary numbering.** Generated code says "read column 23". Four things
must agree on which field is which column: the loader, the kernel, the
unloader, and the row-key hash the search dedups on. Today that agreement
is defined by the IR pipeline, quirks included - a global holding a Lua
function is a `Val` cell pointing at a `Clo` cell, while a global holding
a builtin IS the builtin's cell, which is a fact about how the IR heap was
built and not about the program.

DECISION (Philippe): do not reproduce those quirks. The numbering will be
the tracer's own, clean. The cost to know going in: it feeds the shape
hash and the row keys, so switching invalidates checkpoints and makes the
existing kernels non-comparable. Not a change that can be done halfway.

#### What a variant is worth

The emitter dedups the 64 button assignments on (outputs, `ok`, `live`),
keyed by structural node identity after specialisation. The HONEST
question - Philippe's - is how many distinct successor ROWS can actually
come out; anything above that is code no input can tell apart. Measured
with `trace::eval` over 51 input points, counting only assignments whose
guard holds and whose `ok` is true:

    outcome            emitter keeps    rows possible
    0 player died                  1                1
    1 no objects                  40                2
    2 new room                    40                2
    3 player alive                40               24

**Where that costs.** Not code size, which is what I first said. Every
variant the kernel keeps is a row that gets HASHED AND LOOKED UP, and the
dedup is expensive relative to running the frame. 40 where 2 are possible
is a ~20x hashing overhead. Static dedup is on the critical path; there is
no such thing as over-specialisation, only under-deduplication.

**Why they do not collapse.** They merge exactly when their expressions
fold to the same tree. `freeze` keeps 33 distinct trees over 64
assignments: most fold back to the input cell, while the dash ones become
`freeze = 2 if a dash starts` and differ because their conditions mix
button constants with player-state cells.

Two levers, worth keeping apart:

1. **Normalisation** - sound and cheap, makes equal-but-differently-written
   expressions literally the same node. `Op::Or` is a candidate: the
   tracer's De Morgan makes every `or` three nodes. (Note this retires the
   old argument for having no `Op::Or` - "a vocabulary entry nothing
   constructs" - since something now constructs it.)
2. **Semantic equality beyond folding** - proving two trees always agree.
   Not free, and unsound by sampling alone, but affordable at build time.

Do (1) first and re-measure. The 2 and the 24 are LOWER bounds over
sampled points, so they justify normalisation, which can only merge
genuinely identical things - they do not justify trusting the count.

#### Open, and not to be guessed at

The SIZES. 18,576 lines against `r20_steady`'s 2,094. Not like-for-like -
these are general where the generated ones are specialised per shape class
- but removing the button cells moved it only 10,943 -> 10,801, so it is
not the variants. No explanation yet.

## T11 - normalization: half the emitted body was notation

The duplication census (T10) said 58% of outcome 2's emitted nodes were
boolean guard algebra that never varied over 97 sample points, dominated
by `Not` x 4,435 and `And` x 3,327. That count is a fingerprint of HOW the
tracer wrote things down, not of what the frame computes:

- `or` was De Morgan, because `Op::Or` was deliberately absent on the
  grounds that nothing constructed one. The tracer constructs them, so
  every `or` cost three nodes plus two operand `Not`s - and, worse, the
  detour destroyed the symmetry, so `a or b` and `b or a` interned apart.
- A merge of a boolean at a branch the tracer could not decide is
  `Sel(c, x, y)`. Where one arm is a constant - which is most of them,
  since Lua's `and`/`or` hand the enclosing expression a literal `true` or
  `false` for the short-circuited side - that select IS an `and` or an
  `or`, written the long way.
- Nothing put commutative operands in a canonical order, so structural
  interning, which is the only sharing mechanism there is, could not see
  that `a + b` and `b + a` are one node.

So `Graph::fold` now normalizes. Every rule is EXACT with respect to
`Graph::eval` - the folded node evaluates to the same abstract value as
the unfolded one at every assignment, not merely to a sound one. That
distinction is the whole discipline here: a rule that REFINED the answer
would be correct in isolation and would still change which lanes survive
`ok` and which rows dedup together, so "it can only help" is not a
defence. `folding_is_exact_not_merely_sound` enumerates 3,300 (folded,
unfolded) pairs over 243 tri-state assignments - 801,900 comparisons -
rather than arguing. It is also why `x and not x -> false` is ABSENT:
Kleene says unknown, `false` is a refinement, and refinements do not go
in the folder.

The rules: commutative operand ordering (`Add`, `Min`, `Max`, `Eq`,
`And`, `Or`; `Mul` only one-directionally, because `eval` is monotone
only with the exact side second); `Op::Or` as a first-class tri-state op
with `zb_or` to lower it; `Not(Not x) = x`; `Not(Lt) = Ge` and its three
siblings; `And(x,x) = x`, `Or(x,x) = x`; `Known(Not x) = Known(x)`; and
the six `Sel`-with-a-constant-arm rewrites.

Measured on the same traced frame, same 51-point sweep:

| | before | after |
|---|---|---|
| traced nodes, outcome 2 | 3,198 | 2,007 |
| specialised arena | 25,142 | 14,085 |
| emitted lines, outcomes 1/2/3 | 10,801 / 18,576 / 13,267 | 5,661 / 9,482 / 7,435 |
| constantly-valued nodes | 12,264 of 21,250 (58%) | 5,786 of 10,510 (55%) |
| distinct assignments, outcome 1 outputs | 36 | 35 |

Half the body, and it also merged variants - which is the cost that
matters, since a variant is a row the search has to hash and dedup.

What it did NOT do is explain the constant nodes: 55% of the body still
evaluates the same at every sampled point, now dominated by `And` x 2,980
and `Or` x 1,150. Two readings, and they call for different work:

- **Globally constant** - dead guard algebra the folder cannot see
  because the constancy is a fact about the arithmetic, not about the
  syntax. That would be a real fold, and finding it needs semantic
  equality rather than normalization.
- **Locally constant** - the sample is one game state jittered by +-8 on
  each numeric input, which is far too local to distinguish "always
  false" from "false everywhere near here". These would be the guards of
  branches this state does not take.

Do not guess which. The measurement that decides it is a WIDER sample -
points drawn from genuinely different game states rather than from one
neighbourhood - and that is the next thing, before any semantic-equality
work is priced.

(T12 ran that sample, concluded "not locality - it's real", and was
WRONG. A wider sample cannot distinguish "constant" from "constant on
the region the sampler reaches"; only a proof can, and the proof says
almost none of it is boolean-structural. See T12.)

## T12 - deciding the boolean layer

T11 left a number that needed explaining: 50% of the emitted body
evaluated to the same value at every one of 193 sampled game states.
Two readings were open - dead algebra, or a sample too local to tell
"always false" from "false around here" - and the plan said not to
guess between them. This is what chasing it found, including two things
that turned out to be wrong.

### The measurement that killed the plan I had

First, WHERE constancy is created. A false `And` is false because an
operand is, so counting constant nodes measures how far it spread, not
what started it. Call a node a SOURCE when it is constant and none of
its operands is. There are 514, of which 439 are `And` and 34 are `Or`.

Reading four of them suggested three rewrite rules: `a and not a`,
`a or not a`, and `(a and x) or (not a and x)` - the last being exactly
what a split-then-merge leaves behind, since `split` builds `g and c`
and `g and not c` and `merge` ORs them back together. A tidy story.
Counting how many sources each rule would actually fire on: **23 of
514**. The other 491 are `And`/`Or` nodes with no shape in common. Four
examples is an anecdote, and this anecdote was wrong.

So "is this node constant" has to be DECIDED, not pattern-matched.

### `transpile::bdd`

An ROBDD over the graph's boolean layer. `And`, `Or`, `Not`,
`ConstBool` and boolean `Sel` compose; everything else - comparisons,
`TileFlagAt`, `Known`, `Free`, boolean input cells - is an opaque ATOM.
Two nodes are equal exactly when their BDD references are equal, and
constant is the case where the reference is a terminal.

Atoms are INDEPENDENT free variables. That is where both the
incompleteness and the guarantee come from: `x < 3` and `x > 5` are two
atoms, so their conjunction looks satisfiable and the analysis will not
fold it - but anything constant when the atoms range over ALL
assignments is constant over the realizable ones, which are a subset.
Incomplete and sound is the right side to be on.

One exception to independence, and it exists because of T11: `fold`
rewrites `Not(Lt(x,y))` to `Ge(x,y)`, so normalization is what turns a
negated atom into a SECOND atom. Comparisons that are exact
complements over the same operands therefore share one variable, used
positively and negatively. Without that, T11 would have blinded the
analysis it was meant to feed - and the effect is not marginal: on
outcome 2 it took the constants found from 5 to 180 and the proved
equalities from 1,788 to 4,357.

### Where it goes, and what it is worth

AFTER specialization, not before. The guard algebra only collapses once
the buttons are constants; the same pass on the traced graph finds 4
constants where the specialized arena has 180. Outcome 2, specialized:

| | nodes |
|---|---|
| specialized arena | 10,510 |
| rebuilt with the BDD off (control) | 10,510 |
| rebuilt with it on | 4,714 |

The control matters. `simplify` rebuilds through `fold`, so some of the
shrinkage could have been normalization cascading on a second pass -
and the control says it is none of it. All 55% is 183 substitutions
(180 constants, 3 atom-equalities) cascading through `fold`: a `Sel`
whose condition is decided drops an arm, and everything only that arm
used goes with it.

It is behind `Emit::decide`, OFF for the walk-driven path. That path
produces the checked-in kernels, and a simplification that changes them
has to be regenerated and read, not slipped in.

End to end on the traced frame, which is what the whole exercise was
for:

| outcome | lines before T11 | after T11 | after T12 | variants |
|---|---|---|---|---|
| 1 | 10,801 | 5,661 | 2,833 | 40 -> 24 |
| 2 | 18,576 | 9,482 | 4,454 | 40 -> 24 |
| 3 | 13,267 | 7,435 | 3,577 | 40 -> 24 |

The VARIANT column is the one that matters. T11 halved the emitted text
and moved variants not at all; T12 moved them 40 -> 24, because two
button assignments whose guards differed only in provably-dead algebra
now specialize to identical nodes and dedup. A variant is a row the
search has to hash and look up, and that is the cost that dominates
running the frame - so this is the first change in the campaign that
touches it.

It is still 24 against a measured floor of 2 (the most distinct rows any
of 51 input points produces, on outcomes 1 and 2). The remaining gap is
numeric, not boolean: outcome 3's outputs genuinely differ 24 ways,
and outcomes 1 and 2 are held apart by `deaths`, `delay_restart` and
`will_restart`, which are numbers.

### What it did NOT find, which is the more useful half

Only 180 of ~10,500 nodes are provably constant. The census said 5,254
were constant at 193 sampled points. Those two numbers are both right,
and together they say the sampled constancy is NOT boolean-structural:
it comes from numeric relationships between atoms that this analysis
deliberately cannot see, or it is not real constancy at all and 193
points was still not enough. **T11's "not locality - it's real" was
overconfident and is hereby withdrawn.** The 193-point result never
distinguished "constant" from "constant on the region the sampler
reaches".

Deciding that would need atom implications - `x < 3` implies
`not (x > 5)` - which is a bounded extension (comparisons against
different constants on the same expression, and the `Eq`/`Lt`/`Gt`
triangle) and the obvious next thing to try if emitted size matters
again.

### Iterating, and the reason it earns its place

`simplify_until_stable` runs the pass until it stops finding anything. I
built it for a specific mechanism: atoms are opaque nodes and
comparisons are atoms, so `Lt(Sel(c, x, y), z)` and `Lt(x, z)` are two
INDEPENDENT variables. Prove `c` constant, the select collapses, and the
two comparisons become one node - so a later pass has relational
information an earlier one could not have had.

That mechanism is real (`a_second_pass_can_see_what_the_first_could_not`
constructs it) and **it does not fire on this cart**. Three of the four
outcomes find exactly 0 constants in pass 1. The atom merging DOES
happen - 545 atoms become 401 - it just yields nothing.

The 30 constants pass 1 finds on outcome 2 are a different effect
entirely, and only the per-PASS `capped` flag distinguishes them: pass 0
hit the BDD budget there, and pass 1 ran on a graph small enough to
finish. Capping correlates perfectly with a productive second pass and
nothing else does. Reported per pass rather than per run because
CLAUDE.md forbids silent caps - which is the only reason this was not
written up as the mechanism working.

So iteration's real value is that it makes the BUDGET not matter:

| | pass 0 | pass 1 | pass 2 | final nodes | lines |
|---|---|---|---|---|---|
| cap 4M | 180 (capped) | 30 | 0 | 4,641 | 4,454 |
| cap 32M | 255 (uncapped) | 0 | - | 4,641 | 4,454 |

Same fixed point, identical output, and the 8x cap costs ~18 GB of peak
memory for it. So: SMALL cap plus iteration, and the analysis is robust
to a budget that is too small rather than silently truncated by it.

### The variant floor, and what the 4,357 equalities are worth

The emitter dedups 64 button assignments on STRUCTURAL identity, which
is a sound under-approximation of "the same successor for every input".
How many are distinguishable AT ALL - fingerprint each assignment by its
whole output tuple over many points, and count:

| outcome | floor | emitted | |
|---|---|---|---|
| 0 | 1 | 1 | at its floor |
| 1 | 2 | 24 | 12x over |
| 2 | 2 | 24 | 12x over |
| 3 | 24 | 24 | at its floor |

Outcome 3 is the player-alive branch, where `spd.x`, `spd.y`,
`dash_time` and `djump` all move with the buttons; 24 successors is
correct there. Outcomes 1 and 2 are death/restart, where the buttons
should not reach the output at all - and they emit 24 for 2. The
button-dependence that survives is in the GUARD ALGEBRA that reached the
branch, which is boolean, which is what the BDD already proves equal and
`simplify` declines to act on. So the 4,357 deferred equalities and the
variant gap are one item, not two.

Three caveats, because this measurement is easy to over-read:

- The `__button_states` cells must be excluded. They end the frame
  holding NEXT frame's free choices, so they separate all 64 by
  construction. Leaving them in reported a floor of 64 against a
  structural count of 24 - impossible, since structural equality implies
  semantic equality and the floor can only be LOWER. Pre-registering
  that impossibility is what caught the bug.
- Every point is a perturbation of ONE game state. The emitted kernel is
  specialized on buttons but NOT on the input state, so it runs on every
  block in the search. "2 successors near this state" does not license
  deleting variants; it bounds RUNTIME waste at states like this one.
- Which makes the two remedies different in kind, and they must not be
  conflated. STATIC: prove two assignments equal at all inputs and emit
  one - only the BDD can establish that, and only for the boolean part.
  RUNTIME: notice duplicate rows before hashing them. The measurement
  argues for the second and only hints at the first.

### Atom implications - the concrete shape

The chains are real and they are the tile scan. `tile_flag_at` walks a
tile range whose bounds came out symbolic, so the tracer unrolls it and
each iteration tests the index against a literal:

```
Abs(Flr(Add#174)) compared against
  ["Le 0","Gt 0","Le 1","Gt 1","Le 2","Gt 2", ... ,"Le 8","Gt 8"]
```

Nine variables (the `Gt k`/`Le k` pairs already collapse via the
complement rule) where the real object is one integer with ten possible
positions, TOTALLY ORDERED: `Le 0 => Le 1 => ... => Le 8`. So
`Le 2 and Gt 5` is unsatisfiable and the independent-atom model cannot
see it. On outcome 2, 214 distinct values are compared against 2+
literals, 882 such comparisons, against ~545 atoms - so most atoms are
in a chain.

The fix is a care set: build `C = AND(Le_k -> Le_k+1)` per chain, and
"constant true" becomes `C and not node == 0`. Before writing it, two
things need measuring, because the mechanism being real and the
mechanism being what is happening here are different claims and they
have come apart three times in one day:

- Do the sample-constant nodes actually LIVE in those chains? The 5,254
  and the 882 are separate measurements and connecting them is an
  assumption. Fingerprint only the nodes whose support lies inside one
  chain and see whether the constants concentrate there.
- Does `C` stay small? Chain constraints are individually tiny, but the
  conjunction of 214 of them interleaved with 545 atoms under the
  current creation-order variable order could blow up worse than the
  plain analysis, which already capped on outcome 2.

### The uniform/per-lane seed was the wrong question

`lower.rs` derives representation as a fixed point over the graph, and
the fixed point needs SEEDS: every `Op::Cell` must be declared uniform or
varying. The walk gets that from the shape witness. The tracer has no
witness, so the probe stubs it by string-matching `.hitbox.` - hitbox
fields to `uni`, everything else to `vary_in` - because `tile_flag_at`
takes width and height as block-uniform `P8` and refuses a per-lane `ZN`.

That stub is solving a problem that does not exist. Counting the
r20-steady witness, restricted to GENUINE runtime scalars (its cells
store table and closure references as `k: "val"` too, which inflates
every naive count):

| | cells |
|---|---|
| genuine runtime scalars | 116 |
| per-lane | 11 |
| uniform, value KNOWN - already foldable | 65 |
| uniform, value not known | 40 |

and the hitbox is in the KNOWN group: `objects.4.hitbox.w = 393216`
(6.0), `.h = 327680` (5.0). It is a constant, not a uniform.

Of the 40 that remain, nearly all are key-like or shape-like: `freeze`
and `objects.4.dash_time` ARE the pm1 key (the kernel header says
`pm1 freeze=0 dash_time=0`; they are pinned by `pin_val` rather than by
the witness `val`), the six `__button_states` are the free choices,
`collideable` / `solids` / `flip` / `if_not_fruit` are fixed by the
shape, and `p_dash` / `p_jump` / `rem.x` / `rem.y` are the widenings
CLAUDE.md already flags. There is no principled third category - there
is a witness that does not record values it could.

So the tracer should not ask "is this cell uniform or per-lane". It
should ask "does this slot vary within the block", and under
specialize-per-key that is mechanical: what the key and shape fix stays
a CONSTANT folded into the graph, everything else is a per-lane cell.
`Emit::uni` ends up empty.

The defect is therefore in `iface::symbolize`, which turns EVERY
reachable slot into an `Op::Cell`. The hitbox should never have become
one. Fix: `symbolize` takes a predicate for which slots vary; the rest
keep their traced value as a literal. On this shape that would fold 65
of 116 scalars instead of feeding them in as inputs, and the
`.hitbox.` match disappears rather than being replaced.

(`Uni` is not WRONG in the current engine - packing a non-key field that
happens to be block-uniform as a scalar saves lane storage and enables
scalar ops. It is an optimization on top of a classification that is
mechanical, not a category the tracer has to reconstruct.)

### Proved and deliberately not acted on

4,357 further equalities on outcome 2: nodes that are provably the same
boolean function without being the same node. `simplify` COUNTS them
and leaves them alone, and only substitutes constants and atoms.

The reason is that the graph is also evaluated ABSTRACTLY, in Kleene,
per lane, and two forms of one function approximate it differently.
`And(x, y)` decides `false` as soon as either side is known false; a
select-shaped form of the same function answers `unknown` there.
Substituting the coarser form is SOUND but makes lanes undecided that
were decided - and an undecided `live` is not merely slower, it is a
lane that may fall out of every outcome. Constants and atoms are the
two cases where the representative is provably the most precise form
there is: a constant is exact, and an atom's abstract value is its
input's.

That is a refusal to guess, not a permanent one. The measurement that
would settle it is deopt volume in an actual search, which needs the
traced path wired in.

## Stage 3 - was "control flow", now folded into Stage 2

Written before Stage 2 had a concrete shape; T3 (control flow), T5 (loops)
and T6 (the gate) say the same things against the actual code, so this
stage is gone rather than left standing as a second plan.

The one item that was NOT absorbed, and still has to happen: **the kernel
emitter emits ONE output shape.** `OutFields` is a single cell set and
`emit_interface` writes one `KOutShared`/`KOut`; the fused artifact papers
over it with the block-uniform-collapse trick for dying members. A tracer
produces n output shapes the moment two branches disagree about the heap,
which is T2's "different shapes: keep both". So multi-output-shape
materialization (P2/P3 in plans/multi-output-fusion.md) is a hard
prerequisite for T6 on any program where a branch kills an object - which
is every room with a death, a fruit or an exit.

Sequence it after T3 (which is where differing shapes first appear) and
before T6.

## T13 - pm1 class specialization, and why it is worth much less here

The recipe pipeline's class kernels ARE pm1 specializations. `steady` is
`freeze = 0, dash_time = 0` folded in at emit time - the two cells do not
appear anywhere in `kernel_gen_steady.rs`, because the certified overlay
injected them as constants and everything downstream folded. The pm1 key
is six cells (`compiled/mod.rs`): globals `has_dashed`, `freeze`, and
player fields `dash_time`, `djump`, `p_dash`, `p_jump`.
`Rt2::partition_pm1` makes every block uniform in all six, which is what
makes pinning them exact rather than approximate: the dispatcher's class
guard rejects a block that disagrees, so no lane ever runs a body pinned
to a value it does not hold.

The tracer had no notion of this. `iface::symbolize` turned every scalar
under `roots` into a free `Op::Cell`, so all six were unconstrained -
and `has_dashed`, which the emit probe's roots happened not to reach,
stayed concrete WITHOUT being declared. That is the same folding with
nothing saying so and nothing guarding it, which is worse than either
choice made on purpose.

**The mechanism.** `symbolize` now takes a `pin: &[Path]` of prefixes to
leave concrete, and `Iface` carries `pinned: Vec<(Path, Conc)>` so a
specialization is recorded rather than implied. The emit probe pins the
six pm1 paths and prints them. That is the whole feature - a slot left
concrete is a specialization, which `iface.rs` already said; all that was
missing was a way to say which slots on purpose.

**What it bought, measured on room (0,0) f40, post-BDD (2026-08-23).**
Matching outcomes by output count, because pinning reorders them:

| outputs | unpinned | pinned | delta |
|---|---|---|---|
| 53 | 32 | 27 | -16% |
| 34 | 2,833 | 2,762 | -2.5% |
| 104 | 4,454 | 4,343 | -2.5% |
| 58 | 3,577 | 3,081 | -14% |
| **total** | **10,896** | **10,213** | **-6.3%** |

Reachable nodes 5,584 -> 5,418 summed, 2,159 -> 2,058 in the union.
Output shapes: 4 either way. Variants: 1/24/24/24 either way.

**Six percent, and no variants. That is the finding.** The expectation
going in - mine and Philippe's - was that pm1 specialization is a big
lever, because in the recipe pipeline it is the difference between having
a branch-free kernel and not having one. It is much smaller here, and the
reason is structural rather than incidental:

* The recipe path had no guards. A program that must be branch-free
  cannot express "freeze is 0 or it is not", so freeze had to be a
  CONSTANT, and a separate `kernel_gen_frozen.rs` had to exist for the
  other value. Specialization was load-bearing because it was the only
  mechanism.
* The tracer has guards (T5b). An unknown `freeze` produces guard algebra
  in one body rather than a second body. Pinning deletes that algebra -
  which is real, and is the 6% - but it deletes a term, not a program.

So pm1 specialization in the tracer is a **correctness requirement with a
small size dividend**, not an optimization. It has to be on, and it has
to be declared, because a body the dispatcher hands a steady block must
agree with steady. It is not where the emitted size lives.

**Where the size actually lives, again: the buttons.** 24 of the 64
assignments survive as variants on three of four outcomes, and pm1 is not
a button, so pinning cannot touch them. The variant floor is unchanged by
this work: outcome 0 at 1/1, the 34- and 104-output outcomes at 2/24, the
58-output one at 24/24. That gap - and the 4,357 proved-but-unapplied
equalities behind it - is still the item, and it is still blocked on
running the traced path in a real search.

**Interaction with the other two items.**

* With `symbolize` over-symbolizing (the 65 uniform-known scalars,
  `hitbox.w = 393216` and friends): SAME mechanism, different paths. Those
  are pins too - shape-fixed rather than class-fixed. The `pin` argument
  is what they need; what is missing is the list, and the honest way to
  get it is the shape witness rather than a hand-written census.
* With multi-output shapes (P2/P3): pinning shrinks the union by 5% and
  the shared-by-all prefix from 49 nodes to 46. It does not collapse any
  output shape, so it does not make P2/P3 smaller or optional. The four
  shapes on this frame are not a pm1 artifact.

## T14 - one body per pm1 key, and the key set is discovered not declared

T13 pinned the key the warm-up state happened to be in. Compiling for
the OTHERS needs three things, all of which now exist.

**Say the key, don't read it.** `symbolize` takes `pin: &[(Path, Conc)]`
- slot AND value - so one state compiles for any key. Pinned slots stay
input cells; nothing in the body reads them, because the constant was
folded in.

**Make the specialization self-checking.** `iface::pin_guard` builds
`AND over pins of (cell == value)` and `trace_frame` conjoins it into
`ok`, never into `guard`. `ok` is the deopt obligation, so a lane whose
key disagrees goes to the interpreter; `guard` is lane liveness, so
putting it there would drop the lane from every outcome, which is the
missing-successor failure. `a_body_pinned_to_a_pm1_key_refuses_any_other_key`
perturbs each of the six pins in turn and requires the body to decline.

**Discover the key set.** Not the ~1300-entry cross product of the six
cells' ranges, and not a hand-written list either: trace a frame for a
key, read the six pm1 slots off each outcome under each of the 64 button
assignments, and those are the successors. Iterate to a fixpoint.

**It closes at 24 keys.** Same answer at a cap of 64 as at 24 - so the
walk terminated on its own rather than hitting my limit. 24 bodies,
102,559 lines, 390 variants total.

Be careful with that sentence: 24 is the fixpoint OF THE SAMPLED
OPERATOR, and "not the cap" and "not more keys" are different claims. The
successor of a key depends on the whole input state, not just on the key
- whether `djump` resets depends on standing on ground, which depends on
position, which is an ordinary input cell. Step 3 of the walk evaluates
the six successor nodes at ONE value of those cells (the warm-up state's)
while varying only the 64 button assignments. So what it computes is the
keys reachable FROM THIS ONE STATE, and the true set can only be bigger.

A key it is known to miss: `max_djump` is 1 in room (0,0) and 2 later in
the mountain, and it is an input cell held at 1 throughout the walk - so
`djump = 2` never appears, and indeed all 24 keys have `djump` in {0, 1}.
Every block with `djump = 2` in a later room would find no body.

That is affordable only because of `pin_guard`: a key with no body fails
`ok` and deopts to the interpreter. Missing keys cost speed, not
correctness. **The key list is a performance decision**, which is the
only reason a sampled answer stands in for a proved one here.

**How to make it exact, now cheaply.** Evaluate the successor nodes
ABSTRACTLY instead of at a point: pinned cells at their key values, every
other cell at TOP, and read each pm1 output's interval - which is exactly
what `transpile::ival` (T15) does. Enumerating the integers in those
intervals gives an OVER-approximation: possibly some keys that cannot
occur (a dead body, harmless) but provably none missed, which is the
direction that makes the claim worth something. The risk to watch is that
TOP on position widens `freeze`/`dash_time` enough to enumerate uselessly
many keys; the answer to that is to bound a few more cells, not to fall
back to sampling.

### What the per-key sizes say

| key | outcomes | lines | variants |
|---|---|---|---|
| steady (`dash_time=0 p_dash=false p_jump=false`) | 4 | 10,261 | 73 |
| `p_dash=true p_jump=true` | 4 | 4,572 | 19 |
| `p_dash=true p_jump=false` | 4 | 4,877 | 25 |
| `p_dash=false p_jump=true` | 4 | 7,420 | 49 |
| any `dash_time > 0` | 4 | ~4,115 | **7** |
| any `freeze > 0` | 1 | ~35 | 1 |

The variant count - the thing that costs the search a row to hash and
dedup - collapses from 73 to 7 the moment the player is mid-dash, and to
1 while frozen. So pm1 specialization is worth far more on the keys that
are NOT steady, and T13's -6.3% was measured on the one key where it is
worth least. Both numbers are right; the -6.3% was the unrepresentative
one, and I should have said so when I reported it.

## T15 - `transpile::ival`: the interval evaluator was only ever a test

`Graph::eval` evaluates the graph over `Pico8NumInterval` with a
tri-state Kleene boolean, and it models `Abs` with its sign case split -
so it has always known that `0 > abs(x)` is false whatever `x` is.
Nothing folded with it. Every use was in a test.

That gap was visible in the census once it was pointed at the graph that
SURVIVES the BDD rather than the one that enters it: 1,141 of 2,916
surviving nodes still evaluated to one value at all 193 probe points, and
printing the biggest buckets' representatives showed them led by
`Gt(Const(0), Abs(Flr(..)))` and `Le(Const(0), Abs(Flr(..)))`. The BDD
cannot see this by construction - a comparison is an opaque atom, so both
are free variables that could go either way.

**`transpile::ival::fold`** runs the interval evaluator with every input
at TOP and replaces every node it pins down: booleans it decides, and
numbers whose interval is a single point. This is EXACT, not merely
sound - the interval domain over-approximates, so a node reported
`Bool(Some(b))` under TOP inputs is `b` under every assignment - and
unlike the equality substitutions in `bdd` it can only IMPROVE abstract
precision downstream, so the Kleene hazard does not apply.

Two supporting changes it needed:

* `Graph::eval_lenient`: a node the evaluator cannot model becomes TOP
  for its kind instead of an error. Exact-or-nothing is right for a
  check and useless for a transformation - one `TileFlagAt` made the
  whole graph unevaluable, and every traced graph has hundreds. The
  fallback is in ONE place rather than one per arm, so a new unmodelled
  op cannot forget to be sound.
* Checked interval arithmetic (`checked_add`/`checked_sub`/`checked_neg`/
  `checked_scale_positive`/`checked_div_positive`). At TOP the endpoints
  wrap, and `from_i64_endpoints` panicked on that by design. Its own
  comment invited the fix: "if a full-range interval ever becomes
  legitimate, its closure under wrapping arithmetic is the full interval
  - add that as an explicit, deliberate case then." The panicking
  entry points keep panicking; the evaluator uses the checked ones and
  turns a wrap into TOP.

**Worth, measured on room (0,0) f40 with pm1 pinned:** emitted lines
10,213 -> 9,304 (-8.9%); surviving nodes 2,916 -> 2,679; still-constant
1,141 -> 975. The pipeline is interval, then BDD, then interval again,
because each pass's constants are the other's input.

### What is left, and it is now specific

The residue stopped being a mystery the moment the representatives were
printed rather than counted.

* **658 nodes, always FALSE**, led by
  `And(.., TileFlagAt(Add(..), Add(..), Cell(12), Cell(11), Const(0)))`.
* **232 nodes, always TRUE**, led by
  `Not(Eq(Const(17), Mget(Add(..), Add(..))))`.

Both are the MAP, and I predicted that giving the evaluator the cart
would take 890 of the 975. **It took zero. The prediction was wrong and
wrong in a way I have been wrong before.**

The mechanism itself works. `Graph::eval_lenient_in` takes a `Room` and
decides `TileFlagAt` over intervals with two one-sided rectangle tests:
false everywhere if the UNION of the possible rectangles holds no solid
tile, true everywhere if their INTERSECTION holds one, unknown between.
`a_collision_test_is_decided_at_a_known_position_and_not_at_an_unknown_one`
checks all 256 tile-aligned positions in room (0,0) against the concrete
`solid_at` and they agree.

It changes NOTHING on the traced graph, because the player's position is
an unconstrained input. With x and y at TOP the player could be anywhere,
so "is there a wall here" genuinely can go either way - the union is the
whole room (solid) and the intersection is empty. Unknown is the correct
answer.

So those 658 nodes are not constant. They are **constant around one
game state**, which is what a census over 193 points near that state can
see and cannot distinguish from constancy. This is the third time in
this campaign that a sampled-constant population has turned out to be
locality; the rule that keeps being violated is that a sample near one
state cannot tell "always" from "always around here", and the fix is to
stop treating census buckets as an estimate of what a decision procedure
will find.

**What would actually decide them: bounds on the position inputs.** A
player is not anywhere - it is in the room, and its speed is bounded by
the physics. Narrowing an input is a SPECIALIZATION exactly like a pin,
so it carries the same obligation: `pin_guard` generalized from
`cell == value` to `cell` in an interval, conjoined into `ok`, and a lane
outside the assumed range deopts. That is a real design, it reuses the
machinery T14 already built, and it is not the free win I claimed the
cart would be.

The cart-aware evaluation stays regardless: it is exact, tested, and
costs two map queries. It just does not pay until the positions are
bounded.

* **462 + 63 nodes that are NOT constant** - genuine duplication, many
  copies of `Gt(Sel(..), Const(8388608))`. Same item as the deferred
  equalities: provably equal, structurally distinct, still blocked on
  the Kleene-precision question rather than on the ability to prove it.

## T16 - one body, every output shape (the first half of T6's blocker)

`emit_body` took ONE `OutFields` plus `Emit`'s single `ok`/`live`, so a
traced frame's four output shapes were four separate lowerings. It now
takes `&mut [Outcome]`, where an `Outcome` is one shape's fields plus its
own `ok` and `live`, and emits one body over the union of their roots.

Two things follow from the nodes being in one graph.

The BODY is shared. A node is bound once no matter how many outcomes
read it, so the whole frame up to the first branch that disagrees about
the heap is emitted once instead of four times.

The VARIANT COUNT is a max, not a sum. Two button assignments are
interchangeable only if they agree about every outcome, so the signature
spans all of them - and the result is the largest outcome's variant
count, not the total.

**Measured on room (0,0) f40, pm1-pinned, post-ival+BDD:**

| | lines | variants |
|---|---|---|
| four separate lowerings | 9,304 | 73 (1 + 24 + 24 + 24) |
| one fused body | **4,419** | **24** |

-52.5% of the body and -67% of the variants. That is better than the
"2.6x" the reachability census suggested, and my caveat about it was
wrong in an instructive way: I read "only 63 nodes are shared by ALL
four" as evidence the shared prefix was small. Sharing between PAIRS of
outcomes is what the fused body captures, and it is much larger than the
four-way intersection. A four-way intersection is the wrong statistic
for a fusion that is not four-way.

**The single-outcome path is untouched.** `lower_walk` wraps its one
`OutFields` in a one-element slice, and the emitted names keep their
original spelling when there is exactly one outcome, so the checked-in
kernels are BYTE-IDENTICAL - `generated_is_current{,_r20}` pass
unchanged. That was the point of doing the refactor this way round: the
regression gate is exact rather than a judgement call.

### What is still missing before this is T6

This is the EMITTER half; the numbering half is T18 below.

## T17 - the boundary numbering, done the other way round

T6 needs the tracer's slots and the engine's cells to be the same
things. The plan said to give the tracer the engine's numbering: a BFS
from the globals in `GLOBAL_NAMES` order, reproduced inside the tracer.

**Philippe's call, 2026-08-23: do not reproduce it. Make the consumer
accept the tracer's names instead.** That is strictly better and the
reason is not effort:

* Reproducing the BFS means two implementations of one numbering that
  must stay in step forever, and nothing checks that they do.
* Any change to the tracer's numbering changes the shape hash, hence the
  row key, hence what the search dedups on, hence every checkpoint.
  CLAUDE.md flags this as "the disruptive one".

`trace::bind` does neither. The kernel says which slots it wants BY
PATH, and the binder resolves them against whatever block it is handed,
once, at bind time. The engine's numbering is untouched, no checkpoint
is invalidated, and the shape hash is unchanged.

**The one thing to get right** is that a pointer is a cell of its own: a
global holding a table is a `Cell2::Val` whose column is `AV::Ptr(t)`,
and `t` is the `Obj`. Walking a path dereferences BETWEEN steps but not
at the end - the last step lands on the `Val` cell holding the scalar,
which is the cell a kernel reads and writes.

**Ground truth without running the search.** The shape witnesses under
`crates/celeste-kernels/witness/` are real boundary shapes a real search
produced, recorded with the canonical cell ids they had AND with the
path the boundary reached each cell by. Rebuilding a witness as a block
and resolving every recorded path back gives an exact check:

    steady-shape.json     282 paths resolved, 0 skipped
    r20-steady-shape.json 408 paths resolved, 0 skipped

"Resolved to" is "landed on", not "equalled", because the global slot
`objects` and the array it points at both carry the name `objects`. A
path names the SLOT, which is also what a kernel reads.

A path outside the shape is a REFUSAL - `a_path_outside_the_shape_is_refused`
covers an absent global, an index past the end, an absent field and a
field taken off a scalar. That refusal is what makes binding by path
safe: handed the wrong shape, a kernel declines to bind, which is the
all-or-nothing behaviour the kernels already have.

### Inputs, and then the shape an outcome ends in

`bind_inputs` resolves `Iface::slots`, giving "canonical cell for
`Op::Cell(i)`".

Outputs need more, because each outcome ends in its OWN heap shape and
one that allocates - a death making a new player, a fruit leaving -
names cells the input block does not have. So there has to be something
to resolve against, and `structure_of` builds it: the engine's structure
for a traced state, the mirror of `compiled::bridge::import_block` over
the tracer's heap instead of the interpreter's.

It follows the engine's numbering rule - breadth-first from the globals
in `GLOBAL_NAMES` order, object fields sorted by name, array items in
order - not because the tracer must agree with anything, but because the
block it describes gets handed to the engine, which renumbers by that
rule anyway. Producing it directly makes the ids the kernel writes and
the ids the engine reads the same ids by construction.

**OPEN, noted not resolved (Philippe, 2026-08-23): is the renumbering
pass needed at all?** `Rt2::boundary_canonicalize` re-derives every cell
id at every frame boundary so that two isomorphic heaps compact to
identical structures - which is what makes the row hash block-
independent and cross-block dedup exact. But if every producer emitted
canonical numbering in the first place, the pass would be the identity
everywhere and could go. `structure_of` is one producer that now does.
The others are `import_block` (which already walks in canonical order)
and the frame body itself, which allocates during execution and is the
one that would have to change. Unverified either way; the reason to
write it down is that the pass is on the per-frame path, so if it is
redundant it is redundant 40 times per search step.

Structure only: `cols` carries pointers, because the resolver follows
them, and nothing else. The values are what the kernel computes.

The check that matters is DISTINCTNESS, not resolvability. A structure
that merged two slots would resolve both paths happily and make the
kernel write one cell twice: last write wins, one field silently wrong,
the other's value gone. The block that comes out is still well-formed -
right cells, right kinds, hashes fine - so nothing downstream notices,
and the symptom is a wrong search result far from the cause.

It is a GUARD, not just a test. `resolve_all` refuses when two paths
land on one cell and names both, so the failure is a loud bind-time
refusal rather than a silent overwrite. It costs one hash set per bind,
not per lane. It cannot be a legitimate case: `iface::scalars` yields
one path per distinct slot, aliases collapsing to the first path that
reaches them, so a collision means the structure merged two slots that
are not the same slot.

`a_traced_state_becomes_a_structure_every_path_can_walk` then checks the
positive direction on real data: every scalar in a warmed-up traced
state resolves, 59 paths to 59 distinct `Val` cells, none dropped as
unnameable.

Two things it refuses rather than guesses: a table with both a hash and
an array part (no table in the cart has one, and the engine's `Cell2`
picks one), and integer keys outside the array part.

## T18 - the numbering, applied (and the bug it was hiding)

T17 built the resolver. This wires it in: `trace::emit::bind` takes a
traced frame and hands the emitter cell ids an `Rt2` actually has.

Three pieces.

`trace_frame` now resolves as it goes. It builds the engine structure
for the input state and for each outcome's state, and resolves every
path against the right one. `Frame` carries `in_rt2` / `in_cells`;
`FrameOut` carries `rt2` / `cells` / `ubool_cells`. Resolving at trace
time is not an optimization - it is the only moment the states exist.

`bind::renumber_cells` rewrites the graph. The tracer numbers input
cells `Op::Cell(0..n)` densely, in `Iface` order, because its own
evaluator and every perturbation test index that array directly. The
emitter needs the engine's sparse canonical ids. So the graph is rebuilt
once at the join, and the tracer keeps its dense names - the same choice
T17 made about paths, applied to nodes.

`emit::bind` puts them together and replaces the ad-hoc numbering three
probes were each doing by hand.

**Inputs and outputs are two numbering spaces, and that is not a
defect.** An outcome that allocates or frees an object shifts every
canonical id past the change. Measured on room (0,0) f40: an input
shape of 282 cells, and four outcomes of 274 / 282 / 229 / 400.

Note which way that falls. Exactly ONE of the four keeps the input
shape. Two are smaller (something freed) and one is much larger
(something allocated). So the generated `acc_init`, which today clones
the chunk's structure and overwrites the output columns, is only correct
for one outcome in four - see below. The
generated code reads inputs off the chunk and writes outputs onto an
accumulator built from the outcome's structure - different blocks, so
the two spaces never meet. An id is only meaningful against the
structure it came from.

The body did not change: 4,419 lines and 24 variants before and after.
Renumbering cells cannot change what the graph computes, and this says
it did not.

### `structure_of` was not producing canonical ids

T17 wrote that `structure_of` "follows the engine's numbering rule". It
did not. It allocated a table's cell inside the slot that pointed at it,
so a table-valued global put its pointee between itself and the next
global. Every id from global 12 onwards was off by a growing amount.

Nothing caught it, and it is worth being precise about why. The T17
tests check that every path RESOLVES and that no two paths land on one
cell. Both pass on a consistently wrong numbering: the structure is
internally coherent, just not the one the engine builds. The failure it
would have caused is the same one `resolve_all` guards against - a
kernel writing cells the engine reads back as different fields, with a
well-formed block coming out and nothing downstream noticing.

**The fix is to check against the rule, not to restate it.**
`Rt2::boundary_canonicalize` had the BFS, the compaction and the
canonical field sort inline. Those three are now
`Rt2::canonicalize_ids`, and `boundary_canonicalize` calls it after its
widenings. `the_structure_a_traced_state_becomes_is_already_canonical`
then requires `canonicalize_ids` to be the IDENTITY on what
`structure_of` builds.

That is the strongest form available here. There is one implementation
of the numbering rule, in the crate that owns it, and the tracer's
producer is checked against it rather than against a second copy of it.
A restatement of the rule inside a test would have been written from the
same misreading that produced the bug.

The field sort moved into `canonicalize_ids` for the same reason: field
order determines child discovery order, so it is part of the numbering
rule, and leaving it outside would have left that part unchecked.
Nothing between the old sort site and the BFS reads fields positionally
(`mark_walk` and the widenings all go through `obj_field_cell`), so the
move is behaviour-preserving.

### What is still missing

* `render` and `transpile::fuse` still read `Variant::per[0]`. Emitting
  a kernel with n > 1 means suffixing the whole output half of the
  generated interface - `KOutShared`, `KOut`, `OUT_CELLS`, `apply`,
  `acc_init`, `append_out`, `KEY_CELLS`, `row_keys` - once per outcome,
  with the suffix empty at n = 1 so the checked-in kernels stay
  byte-identical. The input half (`bind`, `rows`) is unaffected.

  **This has a design decision in it, and it is not cosmetic.**
  `acc_init` clones the chunk's structure. Three outcomes in four do not
  have the chunk's structure, so a suffixed-but-otherwise-unchanged
  `acc_init` would compile and produce a block with the input shape and
  the outcome's values - well-formed, silently wrong, exactly the
  failure class `resolve_all`'s distinctness guard exists to prevent.
  Doing this properly means the kernel carrying each outcome's structure
  as generated data, which is new emitted content rather than a
  suffixing pass.
* `FrameEngine::step` is already `(shape, rows) -> [(shape, rows)]` and
  nothing has ever populated more than one entry.
* The partition check P2 calls for: union of member masks plus the
  deopt count equals the lane count.

## T19 - a traced frame becomes a kernel that COMPILES

`trace::kernel::render` turns a traced frame into a whole Rust kernel.
Room (0,0) f40, pinned to the steady pm1 key: **6,681 lines, four output
shapes, 24 button assignments, and it compiles.**

That is the piece nobody had evidence for. `lower_frame` succeeding says
the emitter emitted lines. Whether the lines are Rust is a separate
question, and rustc is the only thing that answers it.
`./check-traced-kernel.sh` compiles the rendered file out-of-tree - not
checked in yet, since it changes on every emitter tweak and would
otherwise sit inside the same bootstrap the checked-in kernels have.

### Not a generalization of the walk's renderer, on purpose

`transpile::kernel::render` does this job for the recipe pipeline, and
the plan was to generalize it to n output shapes. Reading it changed my
mind. It carries `Op::Split`, `SplitValid`, `fork_depth` and
`valid_expr`, all of which exist because `zi_fork_flr` splits one
interval lane into two. A tracer's outcomes come from real branches, so
it never builds any of it. Generalizing would have meant inheriting
those assumptions and noticing them one at a time.

Two differences are the point rather than an accident.

**Inputs are named by PATH.** The walk binds by canonical cell id
against a shape witness. This emits `ROW_SLOTS: &[(&str, &str)]` - the
path each input came from - so the binder resolves against whatever
block it is handed. The cell ids in the struct field names are just
names.

**An output block is BUILT, not patched.** The walk's `acc_init` clones
the chunk's structure and overwrites what the frame wrote, because there
the output shape is the input shape. Here it is not: of the four
outcomes, one keeps the input's 282 cells and the others are 274, 229
and 400. So each outcome carries its own structure and the kernel fills
it. Nothing passes through from the input block - measured at ~105
pointer cells the structure carries, ~53 scalars the frame computes.

I had called this a design decision to bring to Philippe. It was not
one. It only looked like a fork because I was asking how to generalize
the existing emitter instead of what a traced kernel needs, and the
answer is *simpler* than what the walk does, not more complicated.

### `live`, and the assumption that was wrong

Several outcomes means the kernel has to say which lanes take which
successor, so `VarOut` gained a `live` mask alongside `ok` and `bd`.

I asserted the walk path would be unaffected because its `live` is
`ConstBool(true)`. It is not - `Emit::require_live` accumulates a
`SplitValid` per fork level - and `generated_is_current` caught it: 36
duplicate lines, one per variant, in every checked-in kernel. That is
twice this session I have reasoned about a value instead of reading it,
and both times the test was what noticed.

The rule is now explicit and CHECKED rather than assumed. With one
outcome, "which lanes take this successor" is "which lanes exist in this
fork configuration", which the walk already binds as `valid{d}` and
`render` already emits - so no mask is emitted, and every live conjunct
is required to be a `SplitValid`. A traced single-outcome frame passes
that the other way, with no conjuncts at all, because the frontier's
guards partition the lanes so a lone outcome claims all of them.
Anything else stops with an error instead of being silently dropped.
`generated_is_current{,_r20}` pass unchanged.

## T20 - and it RUNS, and it agrees with the graph

The kernel now has the rest of its interface, and something runs it.

**The shape is generated data.** `OUT_SHAPE_{i}` / `OUT_GLOBALS_{i}` /
`OUT_PTRS_{i}` are each outcome's heap layout as constants, and
`acc{i}` turns them into an empty block via
`celeste_engine::slots::build_block`. `append{i}` then pushes the lanes
that take that outcome and that the kernel is willing to keep. No
`acc_init` clone, because three outcomes in four do not have the input's
shape.

**Inputs bind by path.** `bind` resolves `UNI_SLOTS` and `ROW_SLOTS`
against whatever block it is handed and returns the cell ids;
`rows(b, &slots, lo)` gathers a 16-lane slice from them. The walk
implementation moved DOWN into `celeste_engine::slots` so generated code
can call it, and `trace::bind::resolve` now delegates there. One walk,
not two that have to agree forever - the same argument T17 made about
numbering.

**`traced-kernel-check/` runs it**, deliberately outside the workspace:
the kernel is generated, ~375 KB, and changes on every emitter tweak, so
in-tree it would sit inside the same bootstrap the checked-in kernels
have. `./check-traced-kernel.sh` renders, compiles and runs it.

The check is two evaluators of ONE graph. `trace::eval` walks the nodes;
the kernel is those nodes as compiled Rust. Same inputs, and every
output cell, liveness bit and deopt bit has to agree.

    rows per outcome [24, 23928, 48, 24]
    1,393,224 (row, cell) values agree with the graph

The inputs do not have to be reachable game states. Both sides evaluate
one graph, so any assignment of the input cells is a valid comparison -
and the unreachable ones are where a bug is most likely to survive.

### Coverage was the hard part, and it failed quietly twice

The first run reported 16,704 agreeing values and passed. Every one of
them was outcome 1. Three of the four output shapes had no row at all,
so `acc0/2/3` and `append0/2/3` were compiled and never executed.

The first cause was mundane: lanes perturbed `slots[k % len]` with
fifteen lanes and twenty-six slots, and the slots are in PATH order, so
it swept `collideable` through `flip.y` and never touched `x`, `y` or
`spd` - which are exactly what a shape-changing branch reads. Sweeping
every slot got three of four.

The fourth needed pairs. Outcome 0's liveness is `will_restart` AND
`delay_restart > 0` AND `delay_restart - 1 <= 0`, i.e. a flag set and a
counter at exactly 1. No single displacement satisfies a conjunction:
setting one loses the other. A pair sweep reaches it.

The lesson is about the assertion, not the sweep. "1.4 million values
agree" is the kind of number that reads as thorough, and the version of
it that checked a quarter of the kernel looked exactly the same. What
made the gap visible was counting rows PER OUTCOME and failing on any
that were never reached. A coverage claim needs a coverage assertion;
a volume number is not one.

### Still deferred

The pre-dedup `row_keys`/`KeyPlan` machinery. It is a performance
mechanism (8.3 emitted rows per surviving row at f35), not correctness.

And the comparison against the INTERPRETER on a real search block, which
is what `CELESTE_COMPILED_FORWARD=check` does for the walk's kernels.
What runs today closes the emitter-to-machine-code gap; the tracer's
agreement with the interpreter is already covered by the oracle test, so
these two together cover the chain - but not yet in one run, on a block
the search actually produced.

## T21 - the shape fixpoint: 3 shapes, once the walk stays in the room

Covering a room without ever deopting means a kernel per input shape,
which means knowing every shape the room reaches. Philippe: is that not
just a fixpoint from the spawn shape? It is.

**It is a fixpoint. It just does not converge to anything useful.**
The first run said four shapes, matching the emit probe's 274 / 229 /
400 / 282 at f40. That number was an artifact of traces aborting early;
see the end of this section.

**The concrete values do not matter, and that is what makes it a
fixpoint over SHAPES rather than over states.** Every non-frozen scalar
is symbolized at the start of each frame, so whatever a slot held is
erased before it can decide anything: two states with the same shape
trace identically. Stepping forward only needs a state with the right
shape, so the walk blanks the values rather than carrying ones that look
meaningful and are not.

### "Everything symbolic" is not every number in the heap

Full symbolization refused three times before it ran, and each refusal
was the same mistake in a different place: a PROGRAM CONSTANT treated as
state.

**`types` and the object prototypes.** `balloon.tile` is 22 in the
source and stays 22. Symbolized, `type.tile == tile` in `load_room`'s
scan becomes undecidable, so the tracer explores every object type for
every tile - including a balloon, whose `init` calls `rnd`, which the
minimal cart does not define. Room (0,0)'s tile map contains no balloon
at all. The trace had wandered into a room that does not exist. Frozen
structurally, from `types` itself, rather than by listing names.

**`room`.** `load_room` does `mget(room.x*16+tx, ...)`. Same failure,
one level up: a symbolic room is every room. Freezing it states an
existing specialization rather than adding one - a kernel is per room by
construction, since `G` carries that room's collision cache.

**The button indices.** `btn(k)` asserts its argument is one of the six,
so a symbolic `k_right` makes every `btn` call fail.

Note what is NOT frozen: `freeze`, `will_restart`, `delay_restart`,
`has_dashed` and `has_key` are also assigned at the cart's toplevel, and
they are state. "Set up at toplevel" is therefore not the rule. The rule
is "no frame writes it", and the frozen list is the part of that
discovered so far - by refusal, which is worth saying plainly.

**The risk direction is the safe one, and the doctrine is why.**
Freezing something that is really state would make the walk MISS a
shape, hence a kernel, hence a deopt at runtime - and a deopt now stops
the run and names itself. An over-freeze is therefore loud rather than
silent. That is the first thing the doctrine has bought.

### It also confirms skipping the pm1 tier

The fully symbolic frame - no pm1 pin, 41 slots - is **2,341 graph
nodes**. The pinned trace's reachable set was about 1,720 for its
largest outcome alone. Un-pinning costs very little, which is the same
answer T13 gave from the other direction (-6.3%).

So: one kernel per shape, covering every pm1 key. Four kernels for room
(0,0), not four times twenty-four.

### The refusal, chased - and "four" was wrong

One of the four shapes could not be stepped from:

    obj.type.update: in `this.delay>0`: comparison of nil and number

**The cause is an invariant the tracer cannot see.** `spring.init` sets
`hide_in` and `hide_for` and NOT `delay`, so a fresh spring's `delay` is
nil. `spring.update` then reads it only in a third branch, and the two
branches before it assign it first - `spr` starts at the tile value 18,
so the `spr==18` branch runs and sets `delay=10` before `spr` can ever
differ. In a real run `delay` is always a number by the time it is read.
Symbolize `spr` and that ordering is invisible, so the tracer walks into
the branch while `delay` is still nil.

PICO-8 raises on `nil > 0`. So this is not a modelling gap: it is a path
that IS NOT A LEGAL RUN.

**The fix is to poison the path, not to abort the trace.**
`Interp::poison` sets `ok` to false for that state and records why.

`ok`, not `guard`, and the choice is the whole point. Clearing `guard`
would say "no lane takes this path" - and if that were ever wrong the
successor would silently vanish, which is the one failure nothing
downstream sees. Clearing `ok` says "the kernel declines these lanes",
and under the never-deopt doctrine a lane that really got there stops
the run and names itself. The unsafe reading of the invariant is
reported rather than assumed.

Counted by reason, with the source expression, because turning an error
into a deopt hides modelling gaps at BUILD time. On the walk it is one
cause, exactly:

    768 x `this.delay - (1)`: arithmetic on nil and number
      6 x `this.delay>0`:     comparison of nil and number

`compiled_forward_reproduces_the_interpreter` and the oracle test still
pass, so no legal path the oracle takes is poisoned.

### Correction: every traced measurement labelled "room (0,0)" is room (1,0)

`_init` calls `load_room(1, 0)` and `apply_start_room` rewrites it from
`CELESTE_START_ROOM`, which defaults to **(1,0)**. So every number in
T13-T21 that says room (0,0) was measured on room (1,0). I wrote the
label myself and then repeated it.

It changes no measurement - the same runs, the same room, the wrong
name - but it matters for what comes next, because BENCHMARK_DATA's
6.44 s / 0.26 GB / 387,443 rows baseline IS room (0,0), reached with
`CELESTE_START_ROOM=0,0`. Comparing against it means tracing that room,
not this one.

The code labels are fixed. The section headings above are left as
written so the correction is visible rather than tidied away.

### "Four shapes" was an undercount, and the fixpoint does not converge

Fixing the abort changed the answer. The traces that used to die now
finish, and the walk finds **16 shapes and keeps going** - it stops on a
node limit, not on closure. Shapes reach 90 tables, against 19-40 for
the ones the search really produces.

That is over-approximation compounding. With every scalar symbolic, both
sides of every branch are explored, including the ones that SPAWN
objects; the outcomes feed back in, and the object count grows. One
frame on a 90-table state is over two million graph nodes - against
2,341 for the 41-slot spawn state. A kernel for such a shape would be
about a million lines.

Releasing the arena between frames (`rebase`: snapshot the heap's
constants, throw the graph away, re-intern) was worth doing and is not
the fix. Only 2 frames of 16 could not release, and the graph still hit
the limit inside a SINGLE trace.

### The divergence was ONE branch: the room exit

Philippe asked which branches spawn the objects, and what would have to
be concrete to stop it. The answer is neither of the obvious ones.

The census names what accumulated:

     19 tables: 1x?
     25 tables: 1xplayer_spawn
     26 tables: 1xplayer
     34 tables: 1x? 1xfruit 2xspring      (x4)
     40 tables: 1xfruit 1xplayer_spawn 2xspring  (x4)
     41 tables: 1xfruit 1xplayer 2xspring (x4)
     90 tables: 12xfall_floor 1xfly_fruit 1xplayer_spawn

Room (1,0) has exactly ONE object tile, `player_spawn`. Springs,
fruit, fall floors and fly fruit are not in it. Those are **other
rooms'** objects.

With the player's position symbolic the tracer takes the room-exit
branch. `next_room` calls `load_room(room.x+1, room.y)`, which loads a
different room's entire object set - and that state feeds the next
iteration, which exits again. The walk was not accumulating spawns; it
was walking through the game.

**Nothing needs to be made concrete.** The exit is a real successor - it
just belongs to another room's kernel set, which is the multi-room seam
BENCHMARK_DATA already parks as future work. Recording it as a terminal
and not stepping through it is the whole fix.

| | shapes | frames | time | graph |
|---|---|---|---|---|
| stepping through the exit | 16, not closed | 16 (14 refused) | 155 s | 2,000,008 nodes |
| exit as a terminal | **3, closed** | **3** | **0.2 s** | **4,423** |

So full symbolization IS usable for the shape set, on one condition: the
walk has to stay in the room the kernels are for. My "it is the wrong
question" was written one experiment too early.

Two things it is still worth being careful about. The three shapes are
an over-approximation - every branch is explored, so a shape the search
never produces costs a kernel nobody dispatches to, which is the safe
direction. And one of the three reports its object as `1x?`: the type
table is not any global's, which is either a naming gap in the census or
a hole in the `objects` array. Unchased.

What full symbolization has been good for either way is finding the
places where the tracer loses an invariant the game holds: three
frozen-constant classes, one poisoned field, and one branch that leaves
the room.

## T22 - a kernel per SHAPE, and the three gaps generating it found

Stage 1 of the end-to-end run Philippe asked for: a room (0,0)-style
forward run compared against BENCHMARK_DATA's 6.44 s / 0.26 GB /
387,443 rows. Three stages - a kernel per shape, a dispatcher, a frame
loop - and this is the first.

`trace::shapes` is the shape walk as production code rather than a test
helper. `trace::kernel::room_kernels_in` traces one frame per shape with
NO pm1 pin, so one kernel covers every key of its shape;
`write_room_kernels` emits a module per shape plus the table a
dispatcher indexes. It REFUSES rather than returning a partial set,
because a shape the walk could not trace is a kernel that will not
exist, and under the doctrine that is a run that stops.

Each kernel gained a uniform `step(b, lo, n, accs) -> Option<u16>`.
Every shape's `Uni`, `RowsIn` and `KOuts` are different types with the
same names, so anything that exposed them could not be
shape-independent; `step` and `acc` are the surface a dispatcher can
hold as function pointers. `None` is "not my shape". `Some(mask)` is the
lanes declined, which the doctrine turns into a stopped run.

### The gaps, which is what generating is for

**`renumber_cells` rebuilt the whole arena.** Shapes are traced into one
graph so they share subexpressions, but `Op::Cell` ids are each frame's
own dense slot indices - so the arena holds cells this frame's interface
does not name, and rebuilding all of it fails on someone else's. Now it
rebuilds only what the roots reach.

**`tile_flag_at` assumed a block-uniform box.** It is uniform in almost
every frame - a hitbox is fixed per object type - but not in the frame an
object is CREATED: `init_object` gives it a default 8x8 which
`type.init` may or may not replace, so the width arrives as a select on
a per-lane condition. `zn_tile_flag_at_lanes` added; the emitter picks
by the operands' representation, and the uniform form stays because
`CollisionCache::solid_map` is keyed by (w, h) and a per-lane box cannot
use it. `generated_is_current{,_r20}` pass, so the checked-in kernels
are byte-identical.

### OPEN: a select whose arms are a number and a boolean

The generator currently stops here:

    select arms disagree on domain: P8 vs ZB in
    Sel(And(And(Le(Cell(20), Const(0)), And(...)),
            Le(Sub(Cell(39), Const(1)), Const(0))),
        Const(0, 0), Cell(272))

`Cell(20)` is `freeze` and `Cell(39)` is `delay_restart`, so this is the
RESTART path - `will_restart and delay_restart>0` then
`delay_restart<=0`, which runs `load_room`. The true arm is a numeric
zero; the false arm is an input cell the emitter types as a boolean.

What is NOT the explanation: `joinable` already refuses to merge `Num`
with `Bool`, and `heap::Slot` distinguishes them, so two states that
disagree about a slot's type have different SHAPES and never merge.
Where the mixed select comes from is not established.

RESOLVED, T23. It was neither arm's type. See below.

## T23 - the mixed select was a STALE expression, and the room runs

### The bug

`shapes::blank` erased every non-frozen scalar between frames and left
`State::guard` and `State::ok` alone. Those two belong to the frame that
PRODUCED the state, not to the one about to be traced from it.

`guard` is what a merge selects on. A stale one therefore puts the
PREVIOUS frame's input cells inside THIS frame's output values - and cell
ids are each frame's own dense slot indices, so cell 39 is
`delay_restart` in one frame and something else in the other. Every
symptom follows:

- "select arms disagree on domain: P8 vs ZB" - a numeric arm and
  `objects[0].flip.x`, assembled under a condition from a finished trace.
- "Op::Cell(18) is not an interface slot (17 slots)" - the same stale
  expression seen from the shape whose interface is 17 globals because
  its object list is empty. This one only appeared after binding ALL
  shapes rather than stopping at the first, which is why it looked like a
  second bug.

Neither candidate from T22 was it. Lua's `and`/`or` do return mixed
types, but `eval_binop` FANS OUT on them, and two states that disagree
about a slot's type have different shapes, so they never reach `join`.
Philippe asked whether to just allow the mixed merge; the answer is no,
and not because it would be hard - it would have needed a tagged column
in the block model to compile an expression that should not have existed.

Dropping `ok` is not dropping an obligation: a lane only reaches this
state by satisfying the previous kernel's `ok`, which that kernel checks.
Re-checking it would deopt a lane twice for one obligation, and would
need cells this frame does not have.

Three diagnostics stayed, one per round trip the hunt cost: a lowering
failure names its cells by path (`trace::kernel::name_cells`), a foreign
cell prints the chain of ops that reached it
(`trace::bind::why_reached`), and that chain's root index gets a legend
saying which outcome and field it is (`trace::emit::root_legend`).

### What the probe also found, and what it means

The blanked states carry two closure-captured locals - `init_object`'s
`x` and `y`, held by the seven closures it stores on every object. They
are STATE the engine does not model at all: `structure_of` emits
`Cell2::Clo(0, [])`, dropping captures, exactly as the importer does.

That is sound only while nothing reads one, and that is checked rather
than assumed: a captured local's value is an expression over the previous
frame's cells, so a traced output that read one would reach a cell this
frame's interface does not name - which is the refusal above, now with a
chain that names the field. Nothing reads them today (`obj.collide` and
friends use `obj.x`, not `x`).

The check has a hole worth stating: it only fires when the captured value
is SYMBOLIC. In a state reached by concrete execution the value is a
constant, and a read would silently fold it in. The shape walk reaches
every shape from a blanked state, so every shape's frame is checked; the
hole is the reference frame, which is a probe.

### Stage 2 - the dispatcher

`trace::dispatch`. The key is the SHAPE HASH, not whether `bind`
succeeds: binding is by path, and the empty-object-list shape's slots are
a SUBSET of the shape with a player in it, so its `bind` succeeds against
a block it was not traced for and then computes a frame that assumed no
objects. `Rt2::shape_hash_of` hashes the whole canonical structure, so it
separates them, and dispatch is a map lookup with no candidate run to
find out it was wrong.

Each kernel gained `pub const SHAPE: u64`. `Kernel` itself moved OUT of
the generated `mod.rs` into `trace::dispatch`, next to the loop that
consumes it - a generated copy of a struct the loop must agree with is a
signature mismatch waiting for a regeneration.

### Stage 3 - the frame loop

`trace::run::Run`, a new loop rather than `compiled::FrameEngine`. That
engine is a pair of paths and a policy for choosing between them, and the
second path is the interpreter. What carried over is the mechanical part:
16-lane slices (the kernels' declined-lane mask is a `u16`, so that width
is the mask's and not a tuning knob), one accumulator per output shape,
merge-by-shape BEFORE the boundary because dedup is exact only within a
block, and the boundary itself, which is the engine's and is not
reimplemented.

A block with no kernel and a kernel that declines lanes both STOP the
run, with the shape hash or the lane count in the message. That is the
doctrine, not a limitation.

`trace::bind::concrete_block` is where a run starts: the state after
`_init` with every scalar at its real value, including the ones the
tracer freezes as program constants - a kernel never reads those, but
they are part of the row key and therefore part of what a run is compared
against.

## T24 - the widened `rem`, and why the tracer stops at frame 25

The frame loop runs 24 frames on kernels alone - all 24 ROW-KEY
IDENTICAL to the interpreter, including frame 24, where the player
appears and one row becomes 24 - and stops here:

    frame 25: shape 1 did not bind a block of its own shape:
    objects[0].rem.x: per-lane num slot holds Ival(-0.5, 0.5-) at lane 0

Philippe, asked whether to sidestep it by running at exact rem: no. The
kernels should run what the normal runner runs - level 0, widened `rem`,
and the ladder on top of it. An exact-rem detour was written and
reverted; what follows is the actual work.

(The detour did produce one number worth keeping: at f30 in room (1,0),
exact rem is 28,553 lanes against level 0's 27,024, and FASTER per frame
- 35.9 ms against 66.4 ms, no interval arithmetic. That says the level-0
`rem` widening is not buying much this early. It says nothing about
depth, which is where a widening earns its keep.)

### Why it does not already work

The surface gap is representation: the tracer's scalars are
`Value::Num` - a graph node standing for ONE number - and `Value::Bool`.
There is no interval, so an `AV::Ival` column binds to nothing.

The real gap is that `flr` of an interval is not a function. The cart:

    obj.rem.x = obj.rem.x + ox + 0.5
    obj.rem.x = __split_by_flr(obj.rem.x)
    amount    = flr(obj.rem.x)

With `rem.x` widened, that lane holds points whose floors differ.
`__split_by_flr` is the cart's own marker for "split the lane here", and
`Interp::eval_builtin` implements it as the IDENTITY, with a comment
admitting the punt: "On an EXACT value the floor is unique, so there is
one fragment and this is the identity."

Forking is not branching. A branch sends a lane to exactly one
successor, which is all the tracer does and what its outcomes, live
masks and appends assume. A fork puts one lane in two successors holding
different values.

### What makes it small - and why the fork is TWO-way

`__split_by_flr` is N-way in general: the fragment count is
`floor(hi) - floor(lo) + 1`, unbounded for a wide interval. It is at
most two exactly when the interval's width is below 1.

That holds here by construction, not by luck. `player.rem` is re-widened
to `[-0.5, 0.5 - 1ulp]` at every boundary - width `1 - 1ulp` - and it is
the ONLY interval in the expression: `rem.x + ox + 0.5` adds plain
numbers, and adding a number shifts an interval without widening it. So
what reaches the floor spans one or two integers.

It is also not an assumption the code relies on silently. `Op::SplitOk`
/ `zi_span_ok` IS the premise "this lane spans at most two floors", and
the engine is explicit about the failure: a lane spanning more stays
valid in outcome 0 only, is seen exactly once, and is filtered by that
premise. Under the never-deopt doctrine that stops the run and names it
rather than dropping successors quietly.

There is a live way for the premise to start failing:
`plans/spd-rung.md` widens `player.spd` into buckets. The moment `ox` is
itself an interval the width can exceed 1 - and then we hear about it.

`zi_fork_flr`, `zi_flr_ok`, `zi_span_ok` and the `ZI` lane type all
exist. This is tracer and emitter work, not engine work.

Only the PLAYER's `rem.x`/`rem.y` are widened - `mark_walk` collects
them from `player_objects` alone - so every other object's `rem` stays a
number and must NOT fork, or a room with n moving objects gets 2^2n fork
configurations for nothing.

### The shape of it

The graph already carries the whole vocabulary: `Choice::Split(d)`,
`Op::Split(d)`, `Op::SplitValid(d)`, `Op::SplitOk`. So the tracer does
not have to fork its own state at all - it names the fragment
symbolically and lets specialization enumerate it, exactly as it already
does for the six buttons:

1. `iface::symbolize` learns which paths are INTERVAL inputs (the
   player's `rem.x`/`rem.y`, found the same way `mark_walk` finds them),
   and the emitter declares those cells `ival`.
2. `__split_by_flr(x)` on an interval-tainted `x` returns `Split(d)(x)`,
   conjoins `SplitValid(d)(x)` into the state's GUARD (the fragments
   partition the lane, so this is liveness, not an obligation) and
   `SplitOk(x)` into `ok` (the <=2-floors premise, which IS an
   obligation). On an untainted `x` it stays the identity.
   "Interval-tainted" is a cone query - does this node reach an `ival`
   cell - memoized per node.
3. `trace::kernel::render` emits the fork configurations, which is the
   `fork_depth` machinery its module docs declined to inherit from the
   walk's renderer. The dimension is the same kind of thing as the 64
   button assignments it already iterates.
4. `player.rem` is written out as an interval (`Col::I`), which needs a
   `ZI` output field kind in the traced renderer.

### Open, and to be checked rather than assumed

`specialize_into` says splits are excluded from what it enumerates
(graph.rs:299) - so how fork configurations reach the emitted code has
to be read before step 3 is designed, not after.

### The build loop, measured rather than guessed

I claimed it was "ten minutes a cycle". It is not, and the number came
from my own polling latency rather than a clock. Measured 2026-08-23 in
`traced-kernel-check`:

| | |
|---|---|
| true no-op | 0.06 s |
| after a `celeste-rust` edit | 57 s |
| generated code only (touch a kernel) | 49 s |
| the room test itself | 8 s |

So ~50 s of that is compiling THIS package - three kernels, 22k lines,
each essentially one enormous function - and celeste-rust is 8 s of it.

Three levers tried, none of which helps:

* `opt-level = 1` for the package: **63 s**, worse than 49. Huge
  straight-line functions cost more in codegen and register allocation
  than optimisation saves, so lowering the level makes rustc do more
  work, not less.
* `incremental = true`: **58 s** on a real code change, plus an 88 s
  priming build. The change is inside the one giant function, so the
  whole codegen unit recompiles anyway and the bookkeeping is pure loss.
  (Beware measuring this with `touch` or a trailing comment - rustc
  hashes post-parse, so both come back in 0.4 s having done nothing.)
* splitting `kernel.rs` (380 KB) into its own crate: saves **4 s of 49**.
  The room kernels are the cost, not the reference kernel.

Left alone. The interesting consequence is Philippe's, below: if the
generated code did not go through LLVM at all, this cost is not reduced,
it disappears.

### T24 landed: 27 frames, and where it broke on the way

The tracer forks now, and the room runs three frames further than the
`rem` widening used to allow. Frames 1-27 are ROW-KEY IDENTICAL to the
interpreter; frame 28 has 7,264 kernel rows against 7,260 - 8 extra and
4 missing out of seven thousand.

What it took, in the order the failures came:

1. **`Domain::fork_flr` / `span_ok`**, and `__split_by_flr` using them
   instead of returning its argument. Validity goes in the GUARD (the
   fragments partition the lane), the span premise in `ok`.
2. **`Iface::ival`** and `Symbolic::ival_cells`, so a slot can be
   declared an interval; `shapes::ival_paths` finds the player's
   `rem.x`/`rem.y` the way `mark_walk` does, by type rather than index.
3. **`fork_depth` was never set**, so `lower.rs` emitted only fork level
   0 and silently dropped every node above it. Shape 1 came out at 5,395
   lines instead of 10,675 and looked like a win. `Frame::forks` carries
   it now.
4. **Nobody closed the fork loops.** The walk's renderer closes them at
   its end; the traced one did not, so the file had unbalanced braces.
   Everything after the body - shared outputs, per-variant outputs, the
   `out` calls - is emitted INSIDE the loops, which is the point.
5. **`SplitValid` rendered as a raw `u16`**, which is all the walk ever
   needs: its fork validity only reaches the liveness machinery. A
   traced frame conjoins it into the guard, and a guard is what merges
   select on, so it also has to be a `ZB`. Emitted on demand, so the
   checked-in kernels stay byte-identical.
6. **`is_interval` was a cone query**, which is the wrong question.
   `dash_effect_time` is a `Sel` whose CONDITION compares a position
   derived from `rem`; its arms are numbers and so is the result. Typing
   it as an interval made the kernel write an `AV::Ival` into a column
   the boundary refused - `player dash_effect_time is not a number`. It
   is a proper type rule now, deliberately the same one
   `lower.rs`'s `Repr::wide` applies, and the two check each other:
   disagree and lowering fails with "output cell wants a ZN but the
   graph computes a ZI".

### The cost, and the two-call fix

Adding two forks took the check crate's build from ~50 s to over
twenty-five minutes. The SOURCE barely grew - 22,170 lines to 22,206,
because a fork is a loop, not a duplication - so this was LLVM unrolling
`for c0 in 0..2` and `for c1 in 0..2` around a 10k-line body and
compiling four copies of it.

Hiding the trip count behind `std::hint::black_box` takes it back to
**55 s**. Same code, same two iterations; 27x the build time came from
LLVM deciding to duplicate. `Emit::opaque_forks` turns it on for the
TRACED path only - the walk's kernels are what the production search
runs and what every BENCHMARK_DATA number was measured on, and
`black_box` blocks optimisation across it, so enabling it there would
reprice recorded results to shorten my build loop.

### What a fork COSTS at runtime (Philippe, 2026-08-23)

Asked whether the fork is extra graph nodes or real runtime forking,
wanting to avoid the second. It is both, and the second is what happens.

In the graph it is nodes: `Op::Split(d)` and `Op::SplitValid(d)` over
one operand, no state fan-out, the fragment a CHOICE like a button. But
`lower.rs` emits that choice as a runtime loop, and everything after the
split point - including every per-variant output and the `out()` calls -
sits inside it. Per 16-lane slice the tail of the frame runs up to
2^forks = 4 times. That is the walk's design too, inherited rather than
introduced.

Two ways to make it data instead of control flow:

* **Lane doubling.** A fork doubles the LANES rather than repeating the
  body: a 16-lane slice becomes two, one per fragment, empties dropped
  by `f0_fv`. Same arithmetic, one pass, no duplicated code, and the
  fork becomes a property of the block rather than of the program.
* **Compile-time specialization.** `Choice::Split` exists alongside
  `Choice::Free` for exactly this; `specialize_into` currently excludes
  splits.

Philippe, on being shown the loop: the graph-duplication kind was what
he wanted, "because we can merge nodes back there and share
computations". He is right, and I did not weigh the two - `lower.rs`'s
fork machinery is the WALK's, and setting `fork_depth` is the switch
that turns it on. I made the existing emitter do its existing thing.

The loop is worse for exactly his reason. Most of a frame does not
depend on which fragment a lane is in, and the loop re-executes all of
it four times; under specialization those nodes are ONE node across all
four configurations, because `specialize_into` writes into a shared
hash-consed arena ("two button combinations that compute the same thing
land on the SAME node ids").

### Specializing a split needs NO new op

This was the thing I expected to block it. A `Free` becomes a CONSTANT
under specialization, which is why buttons collapse so well; a `Split`
is a narrowing of a runtime value and cannot. But it does not need to be
a constant - it needs to be ORDINARY ARITHMETIC, and it is.

`zi_fork_flr(a, c)` is two cases: fragment 0 is `a` with its top clamped
below the next floor boundary and is always valid; fragment 1 is `a`
with its bottom raised to that boundary, valid only where the interval
really spans two floors. Min/Max/Flr and a comparison. So under
specialization `Op::Split(d)` and `Op::SplitValid(d)` rewrite into plain
nodes that `fold` collapses and interning shares.

The >2-floors lane keeps its behaviour: fragment 0 is valid, fragment 1
is not, so the lane appears exactly once, and `SplitOk` / `zi_span_ok`
takes it off the kernel through `ok` - which under the doctrine stops
the run.

PLAN: enumerate splits in `specialize_into` beside frees, drop the fork
loop from the traced emitter, let `variants` dedup fork configurations
as it already dedups the 64 button assignments. Gated to the traced path
first, so the production kernels and every BENCHMARK_DATA number stay
untouched.

FIRST, though: the frame-28 discrepancy below is probably semantic -
where fork validity goes (guard) versus the span premise (`ok`) - so it
would survive the restructure and be chased through new code instead of
old.

### OPEN: frame 28, 8 extra and 4 missing - CHARACTERISED

Decoded by lane rather than guessed at. Row keys are hashes, but a key
belongs to a lane and a lane is a column, so the values are all there;
`tests/room.rs` now prints the unmatched rows and a position histogram.

Player positions at negative x, frame 28:

| | x=-3 spd=-2 | x=-2 spd=-2 | x=-1 spd=-2 | x=-1 spd=0 |
|---|---|---|---|---|
| kernels     | 4 | 4 | 28 | 0 |
| interpreter | 0 | 0 | 28 | **4** |

Twenty-eight rows agree exactly. Four lanes disagree, and the eight
extras are FOUR PAIRS - each pair identical except `x = -2` against
`x = -3`.

So on those four lanes the interpreter's `move_x` hits a solid and stops
at `x = -1` with `spd.x = 0`; the traced kernel keeps moving, and the
widened `rem` leaves `amount` ambiguous by one, so each lane emits two
rows where the interpreter emits none.

The pairing is the fork working correctly on a lane that should not have
got that far. So the fork is downstream of the bug, not the bug.

Ruled out:

* NOT a deopt. `Run::step` bails on any declined lane and did not, so
  these lanes were not routed off the kernel.
* NOT `break` mis-masking in the unrolled loop, which was the obvious
  suspect: `for i=start,abs(amount)` has a symbolic limit, so it goes
  through `run_for_symbolic` (unroll bound 8, from `unroll_bound`) - but
  a `break` there fans the state out (`Flow::Break => done.push`) rather
  than being masked, so a lane that stopped really does stop.

RESOLVED, and it was not the fork.

The player's screen clamp lives in `player.draw`, not `player.update`:

    draw=function(this)
      if this.x<-1 or this.x>121 then
        this.x=clamp(this.x,-1,121)
        this.spd.x=0
      end
    end

which IS the four missing rows - x = -1, spd.x = 0. The tracer was
tracing `_update()` alone, so a lane that walked off the left edge was
never stopped; it drifted to x = -2 and -3, and the widened `rem`
doubled each into two rows. The fork was working correctly on lanes that
should never have existed.

The interpreter's frame is `_update()` then `_draw()` then the button
reset (`rewrite::program::FRAME_CODE`). The tracer had ELEVEN scattered
`parse("_update()")` calls; it now has one `cart::FRAME_CODE` mirroring
the interpreter's, minus the reset, which `trace_frame` runs itself at
the same boundary.

Worth noting WHY the oracle test did not catch this: it runs the same
chunk on both sides, so both were wrong in the same way. Only comparing
against the interpreter's own notion of a frame exposed it - which is an
argument for the end-to-end room test existing at all, not just the
per-frame one.

## T25 - the room runs, end to end, 30 frames

`the_room_runs_on_kernels_alone` passes. Room (1,0), frames 1-30, on
generated kernels only - no interpreter anywhere in the execution path -
and every frame's ROW KEY SET is identical to `AbstractRun`'s:

    frame  24:       1 rows in ->      24 out; oracle 24
    frame  25:      24 rows in ->     204 out; oracle 204
    frame  26:     204 rows in ->     878 out; oracle 878
    frame  27:     878 rows in ->    2864 out; oracle 2864
    frame  28:    2864 rows in ->    7260 out; oracle 7260
    frame  29:    7260 rows in ->   15250 out; oracle 15250
    frame  30:   15250 rows in ->   27024 out; oracle 27024

Set equality per frame, not counts: two engines can agree on how many
rows survived and disagree about which, and the count is the weaker
claim.

This is what Philippe asked for at the start of the campaign's
end-to-end stage - "can we actually run rooms end to end now" - and the
answer is now yes for one room at level 0, with the widened `rem`, the
real boundary, and the real abstraction.

### What it took, in order

T22 a kernel per shape; T23 the stale `guard`/`ok` and the block
encoding; T24 the interval fork; and finally the frame being a THIRD of
a frame (`_update` without `_draw`).

Four of those five were found by the end-to-end comparison and could not
have been found by the per-frame oracle test, which runs the same chunk
on both sides.

### Speed, measured (2026-08-23)

Both engines timed separately in ONE process on the same data, release,
single-threaded. Room (1,0), 30 frames:

    kernels 619 ms, interpreter 220 ms  ->  2.75x SLOWER

The reason is not the frame computation. It is materialisation:

    frame  25:      24 in ->     2,040 raw ->    204 out  (10x dropped)
    frame  27:     878 in ->    74,928 raw ->  2,864 out  (26x dropped)
    frame  30:  15,250 in -> 1,246,632 raw -> 27,024 out  (46x dropped)

A kernel emits one row per (lane, button assignment, fork
configuration), writes every column of it into an accumulator, and the
boundary then discards 46 of every 47. The ratio GROWS with depth.

The interpreter avoids this by merging states before materialising them.
This is the pre-dedup / `KeyPlan` item deferred earlier as "perf, not
correctness" - it is now the entire gap, which is a good position: 46x
of provable waste is an easier target than a 2.75x algorithmic deficit.

### The 46x splits in two, and they want different fixes

`FrameStat::rows_distinct` (behind `Run::census`, since it hashes every
candidate row - the very work a pre-dedup avoids) counts DISTINCT raw
rows before the boundary widens anything. Frame 30:

    1,246,632 raw  ->  158,993 distinct  ->  27,024 out

* **87% are EXACT duplicates** (7.8x). Two button assignments that agree
  on a given lane produce byte-identical rows; the emitter's `variants`
  dedup only removes assignments that agree on EVERY lane.
* The remaining **5.9x** only the widenings can merge - rows that differ
  in `rem`, in a pinned timer, in a clamped `dash_effect_time`.

A plain hash set in `append` gets the first factor; the second needs a
key that knows what the boundary is about to erase, which is the
`KeyPlan`.

But the first is NOT free, and the arithmetic matters before building
it. Today: push 1.25M rows x ~100 columns, then the boundary hashes all
1.25M. With an exact-duplicate filter: hash 1.25M, push 159k, boundary
hashes 159k - roughly 1.8x, not 7.8x, because hashing a row costs about
what pushing it costs.

The way to get the rest is a CHEAPER KEY. Most output cells are folded
constants - 44 of outcome 0's 52 shared fields in kernel1 are
`zn_splat(...)` of a literal - so a key over only the non-constant cells
is ~8 mixes instead of ~100.

### How much is removable statically: 1.4x, not 7.8x (CORRECTED)

Per-variant field counts in kernel1, which is the real story:

    outcome 0:   0 per-variant fields,  53 shared
    outcome 1:   2 per-variant fields,  32 shared
    outcome 2:   4 per-variant fields, 100 shared
    outcome 3:  16 per-variant fields,  42 shared

Outcome 0 has ZERO. All 24 distinct button assignments write
byte-identical values there; they remain separate variants only because
they differ in WHICH LANES ARE LIVE. So a lane live in five of them
appends five identical rows - by construction, not by coincidence.

`variants` dedups on the whole `(outputs, live, ok)` tuple. Deduping
PER OUTCOME on the outputs alone, and unioning each variant's TAKE mask
(`live & !deopt` - exact whatever their `ok` says, since the values are
identical), removes those before they are written.

MEASURED, from the expressions:

    shape 0: 24 variants x 4 outcomes = 96 appends -> [19, 24, 24, 1] = 68
    shape 1: 24 variants x 4 outcomes = 96 appends -> [1, 19, 24, 24] = 68

**1.4x, not 7.8x.** The zero-tainted outcome does collapse 24 -> 1,
exactly as its field count predicted, but two of the four do not
collapse at all.

I claimed "most of it is removable statically" on the strength of that
one outcome. It is not. Per input lane at frame 30 the picture is 82
rows appended against 10.4 distinct, and static grouping accounts for
1.4 of the 7.8 - the rest is variants that write DIFFERENT values on
some lanes and the same value on this one, which is per-lane and
invisible to the emitter.

(The first attempt at this measurement said 1.0x, because it grouped on
`(outputs, ok, bd)` and `ok` is a per-variant variable NAME. Grouping on
strings that are distinct by construction measures nothing.)

So the order is: static grouping for 1.4x because it is free, then a
per-lane runtime key for the remaining ~5.6x, then a `KeyPlan` for the
5.9x of widening merges. The cheap-key finding is what makes the middle
one affordable - 44 of outcome 0's 52 shared fields are constant splats,
so the key is ~8 mixes, not ~100.

### The static grouping is NOT free the obvious way - ATTEMPTED, REVERTED

The cheap implementation looked ideal: keep emitting one `out()` per
variant, add a `take` mask to `KOut` that is `live & ok` for the first
variant of each output group and `0` for the rest, and append from
`take` instead of `live & !deopt`. No interface change, `live` and
`deopt` keep their meaning for deopt reporting, and 28 of the traced
kernels' variants came out taking nothing.

It made the build **over 20 minutes**, against 73 s. Killed and
reverted.

The reason is the shape of the union. Variant 0's `take` is
`(live_v0 & ok_v0) | (live_v1 & ok_v1) | ... ` over its whole group, so
48 mask variables that used to die immediately after their own `out()`
now stay live across the entire variant sequence. In a function this
size that is a register allocation problem, and LLVM pays for it in
compile time - and would presumably pay in spills at run time too.

The right form is GROUP-DRIVEN EMISSION: one `out()` per output group
with the union, and the duplicate variants' `KOut` never built at all.
That REMOVES code rather than extending live ranges. It is a bigger
change because `out()` passes all outcomes together and the grouping
differs per outcome, so the callback has to become per-outcome.

Worth recording as a general lesson for this emitter: adding a value
that references many earlier variants is not a local change. The body is
one enormous function and everything is a live range.

Caveats, stated so the number is not over-read: one room, one block, no
chunking, no threads, against an interpreter with a year of tuning.

## Where the row amplification actually lives (2026-08-23, with Philippe)

Measured on one block (frame 30's output, 27,024 lanes) so the factors
compose rather than being compared across frames:

    WHOLE block: 27,024 in -> 2,165,112 appended -> 296,765 distinct (7.3x)
    per-lane distinct extrapolates to 799,964     ->  2.7x is CROSS-LANE

So 7.3x = ~2.7x per-lane x ~2.7x cross-lane. I had said a slice-local
dedup would catch "nearly all of it". It catches about HALF. Different
input lanes converging on the same successor is the other half, and only
a table that outlives one 16-lane slice sees it.

The per-lane half is also not uniform. Sampling 201 lanes:

       1x duplication:   70 lanes    <- a third duplicate NOTHING
       6x duplication:   43 lanes
      12x duplication:   16 lanes
      24x duplication:   43 lanes
      96x duplication:   25 lanes    <- every combination identical

A 96x lane ignores the buttons AND the fork - a frozen or dead player.
The value of a dedup is concentrated there, not spread evenly.

### Philippe: fold the widenings INTO the kernel

"The boundary erasing thing should just not be a boundary at all... they
will just get immediately widened afterwards anyway, so we might as well
move it into the kernel now."

My first argument for it was dead-code elimination, and that was wrong.
Measured:

    shape 0: 2,138 nodes from all outputs, 2,085 without the erased
             cells -> 53 nodes (2.5%) exist only for discarded values

`rem` is not dead as a COMPUTATION - it feeds `amount`, which feeds the
position. Only the final store is dead. 2.5%, not a chunk.

The real mechanism is different and better: if the kernel applies the
widenings, its rows are CANONICAL, and that is what lets dedup-at-write
reach the full 46x instead of 7.3x. Two rows differing only in a `rem`
about to be erased look different to a kernel that has not widened yet;
after widening they compare equal - and so do rows from DIFFERENT lanes,
which is the cross-lane half. The two proposals compose exactly.

Cost to name: it makes a kernel specific to a LADDER RUNG. Today the
kernel is rung-agnostic and the boundary applies the abstraction, so
`CELESTE_REM_BITS` changes no generated code. Fold it in and every rung
needs its own kernels. They are already per-room and per-shape, so this
may be fine - but it changes what a kernel IS, and the ladder is the
whole optimality argument.

### Why write-time is the right place for dedup (Philippe, agreed)

"That's where we have the smallest working set and can statically know
the most stuff and do the most efficient comparisons." Plus a fourth
reason: at write time you know the DESTINATION, so a duplicate costs
nothing - you simply do not reserve a row. Detecting it later means the
columns are already paid for.

What write-time dedup cannot do alone is the cross-lane half, whose rows
are produced in different slices. That needs a table living for a FRAME,
keyed on the ~19 varying cells rather than the ~100 live ones - still
far cheaper than the boundary's.

### DONE: the widenings moved into the graph

Philippe, on the key-only variant I proposed: "we shouldn't be storing
something that might then get widened in a different way later. That's
not great." He is right, and the refinement is retracted. Hashing a
value you do not store is sound ONLY while every widening is a per-row
function - true today, unchecked anywhere, and silently wrong the day
someone adds a block-dependent one. The version with no invariant to
violate is: widen, then hash what you stored.

`trace::widen` applies all four inside the traced frame, so they are
graph nodes: player `rem` becomes the constant interval, the four timers
pin to zero, `dash_effect_time` clamps, a live fruit's `off`/`y` become
its bob band. `trace_frame` takes a `widen` flag - ON for the shape walk
and the room kernels, OFF for the differential check against the
CONCRETE oracle, whose job is frame semantics and which has no interval
to compare a widened `rem` against.

**The boundary's assertions came with it.** It does not only widen, it
checks that `rem` was inside the interval and the fruit's `y` inside the
band. Widening earlier would have retired those silently - the boundary
would then be handed the widened value and pass trivially - so each is
an `ok` conjunct now: a violating lane is refused and the run stops.

**One case refused rather than approximated.** The fruit band is
`start +- 2.5`, and the graph has no node for an interval built from two
values, only the literal `Op::Const(lo, hi)`. So it works when `start`
is concrete and bails loudly otherwise. Room (1,0) has no fruit, so this
is untested in anger - but a band computed from the wrong `start` is a
widening that does not contain the value it replaces, which is exactly
the failure that must not be silent.

A bug it exposed: `is_interval` had an early-out when no interval INPUT
existed, which was fine until a widening could CREATE one from a
literal. A widened `rem` was typed as a plain number and the lowering
refused it.

All 30 frames still match the interpreter, so the boundary's own
widening pass is now a no-op on those cells - which makes it a free
check that the two agree.

### DONE: constant columns are written ONCE - 2.75x -> 1.85x

The first increment of the restructure, and the biggest single win so
far. 44 of outcome 0's 52 output fields are compile-time constants; the
kernel was pushing each into a `Vec` once per row.

A constant that every variant agrees on holds the same value in EVERY
row of that outcome's accumulator, so its column is written once as
`Col::U` when the block is built, and skipped in `append` and in
`KShared` entirely. Detected from the specialized graph (`Op::Const` /
`Op::ConstBool`) rather than by matching emitted text, whose form varies
with the coercion the field's type asked for.

    KShared0 fields:  53 -> 4
    pushes per row:   52 -> 4
    30 frames:       604 ms -> 405 ms   (2.75x behind -> 1.85x)

All 30 frames still row-key identical; 609 tests pass. A third of the
total time, from one change, and it attacks both measured problems at
once - the per-row column writes AND the output values that were living
across the variant sequence.

### Specialization is a LAYER, not a redefinition (Philippe)

A kernel specific to a ladder rung is fine, "similar to what we did with
PM1": the most generic kernel is for one input SHAPE, and rung or pm1
specialization is an optimization on top. `trace_frame` already takes a
`pin` and `room_kernels_in` passes none, so the machinery exists.

That also shrinks the change. The dedup benefit comes from rows
COMPARING equal after widening, not from the kernel WRITING widened
values - so the minimal version is a rung-aware KEY over exact rows.
Sound because widening is a function of the row: whichever
representative survives, widening maps it to the same place. Folding the
output widening in as well saves writing ~6 cells of 493, which is
noise.

Three levels, then:

* **shape only** - today. Correct, rung-agnostic, dedups nothing.
* **shape + rung-aware key** - writes exact rows, dedups as if widened,
  reaches the full factor. This is where the performance is, and where a
  mistake is SILENT: a key that erases something the boundary does not
  erase drops real successors. Derive it from the same description the
  boundary widens from rather than writing it twice.
* **shape + rung + pm1** - the existing pin machinery.

### The key computation dedups ITSELF (Philippe)

"Most fields are identical in most of our outputs, and they're identical
because they were computed on the same path, so their hash would be
computed on the same path as well."

Right, and the emitter already has the partition to hang it on. Split
the per-cell mixes by what it knows:

* **constant cells** - 44 of outcome 0's 52 shared fields are
  `zn_splat` of a literal. A literal's mix is a literal: it folds at
  GENERATION time into one constant in the key. Zero runtime cost.
* **shared cells** - one expression across all 24 variants, so one mix
  per lane serves all of them.
* **tainted cells** - genuinely per-variant, one mix each.

Measured tainted counts: outcome 0 has **ZERO**, then 2, 4 and 16. So
outcome 0's whole key is constant-plus-shared, computed once per lane,
and the dedup finds all 24 variants identical without hashing anything
per row.

Implementation note: this belongs at the EXPRESSION level in the
emitter, not as new graph nodes. The graph IR is typed over Pico-8
fixed-point and booleans; a 64-bit accumulator is neither. The sharing
that matters is across variants for one cell, which the emitter already
tracks (`KShared` vs `KOut`).

### A note on the row hash, as asked

Philippe: "if they're invariant to which value is in which field,
that's not great... probably good enough for now, but worth a note."

The specific worry does not apply: `cell_mix(c, v, seed)` takes the CELL
INDEX, so swapping values between fields changes both terms.

The weaker property is the COMBINER. The per-lane key is a
`wrapping_add` sum, order-independent by design - that is what lets
uniform cells fold once into a block partial regardless of which cells
happen to be uniform in a given block. Sums are the collision-friendly
combiner: `a + b = c + d` is easy to arrange deliberately and hard to
hit by accident. At 128 bits, and with the search already staking
correctness on it, "good enough for now" is right. What is being relied
on is ACCIDENTAL collision resistance, not adversarial.

## What is needed before deleting `celeste-rewrite`

* **`gen.rs`.** `FIELD_NAMES`, `GLOBAL_NAMES` and now `FN_NAMES` (the
  tracer resolves closure names through it) are generated FROM the
  rewritten program, and `FIELD_NAMES` ORDER feeds the shape hash and
  the row key. Freeze as data or reproduce without the rewrite
  machinery. This is the known blocker and it got slightly bigger today.
* **One room.** Room exits are terminal in the shape walk and the frame
  loop stops on a shape with no kernel. Multi-room needs the seam.
* **One rung.** The traced path runs level 0. Finer rungs narrow `rem`
  to k bits - a different interval, possibly a different fork arity.
  `zi_span_ok` is the premise that would say so, loudly.
* **The oracle is the thing being deleted.** `the_room_runs_on_kernels_alone`
  checks against `AbstractRun`, which IS the vectorized interpreter.
  Removing the vectorization removes the oracle, so the scalar-
  interpreter step has to come first or the check goes with it.
* **The search is more than a forward loop.** Checkpoints, the backward
  sweep and position graphs all speak row keys - which is why matching
  the interpreter's block encoding (T23) was worth doing rather than
  inventing our own.

## Philippe's codegen question (2026-08-23)

Inspect the generated assembly, and consider emitting AVX-512 - or
bytes - directly instead of going through Rust.

It lines up with the build-loop measurement above from the other side:
LLVM is the entire build cost, and the reason is the same thing that
might be hurting the output - single enormous straight-line bodies.

The shape is already right. A kernel runs 16 lanes of `i32`
fixed-point, which is 64 bytes, which is exactly one AVX-512 register.
So "one instruction per graph node" is a coherent target rather than an
aspiration, and the first step is cheap: dump one kernel's assembly and
count instructions against graph nodes. Close to 1:1 means the Rust path
is fine and only the build is slow; 5:1 with spills everywhere is the
argument for emitting directly.

The catch is register allocation. A graph with thousands of live nodes
cannot stay in 32 registers, so a direct emitter needs its own spill
strategy - which is the part LLVM is actually earning its keep on.
Emitting into a small hand-written library with an explicit stack frame
is the version worth taking seriously.

MEASUREMENT FIRST, and not before frame 25 runs.

### The measurement (2026-08-23): 16.3 instructions per node

`objdump` of `kernels::k1::frame` in the room test binary, against the
generated source it came from:

    97,182 instructions
     5,959 graph nodes emitted as `let nN`
    ----------------------------------------
      16.3 instructions per node

Not one. And the composition says why:

| | share |
|---|---|
| stack traffic (`mov`/`vmov` touching `%rsp`) | **42.1%** |
| `mov` overall | 42.3% |
| vector instructions (zmm 1497 + ymm 934 + xmm 3998) | **6.6%** |
| scalar mask work (`cmove`/`test`/`or`/`shl`/`setg`/`setle`) | ~30% |

My first reading of this was that `ZB` - two thirds of the nodes, and
`{val: u16, known: u16}` - was the culprit, being a pair of scalars
rather than a vector. THAT IS WRONG and the numbers say so.

A Kleene AND on 16 lanes really is two `and` instructions on `u16`, one
per component, which is about optimal. The 3,412 boolean nodes account
for roughly 6,800 instructions. The booleans are fine.

The cost is SPILLING, and it is scalar:

    46,755 instructions touch (%rsp)   -- 48% of the function
       mov             34,362
       of which vector-register:  2,093  (4%)

96% of the stack traffic is plain scalar `mov`. The function has ~6,000
simultaneously live values and the machine has 16 general-purpose
registers, so masks and `P8` values shuttle to and from the stack
constantly. It is not the type. It is graph size against register count.

Which cuts both ways for a direct emitter. It cannot avoid the traffic
either - this is the register allocation problem LLVM genuinely earns
its keep on. What it CAN do is pick a better strategy than a general
allocator fighting 6,000 live ranges: a flat frame with values streamed
in graph order.

I then guessed the peak live set might be ~6,000 because of the emission
ORDER, making scheduling the cheapest win. Measured 511, and concluded
that spilling was therefore INHERENT and scheduling could only shave it.

Philippe: "I don't see where 500 temporaries would come from. if you
analyze the graph there's no way the min cut is 500." Correct, and my
conclusion did not follow from my measurement - I measured the live set
under the CURRENT ORDER and asserted it was near-minimal.

Measured properly, against a greedy min-pressure schedule of the actual
dependency graph:

    nodes 5959,  live-to-end (used by outputs) 493
    peak under EMISSION order: 512
    peak under GREEDY order:   508

**493 of the 512 are OUTPUTS, not temporaries.** The computation's
working set is about NINETEEN values. Rescheduling moves the peak by 4,
because no topological order helps: all 493 are needed at the same
moment, when the output structs are built at the end.

There is no 500-wide cut in the computation DAG. What there is: the
frame computes 493 output cells and holds every one live across the 24
button-assignment variants, because they are shared by all of them.

That reframes the spilling entirely:

* It is not register pressure from computing, so no allocator and no
  scheduler can fix it.
* Those 493 values ARE conceptually a small array - one slot per output
  cell - but they are expressed as 493 named locals, so LLVM sees 493
  live ranges and spills each individually with scalar `mov`s. That is
  the 34,362 movs.
* A direct emitter would hold them in an explicit frame slot indexed by
  cell: the same memory traffic in principle, but structured array
  access instead of 34,000 individually allocated spills.

And a cheaper version exists inside the current emitter: write each
shared output to the accumulator AS SOON AS IT IS COMPUTED, so it dies
immediately instead of waiting for the variant loop. That is the same
restructuring the row dedup wants (group-driven emission), which makes
the two one job.

## Doctrine: never deopt to the interpreter (Philippe, 2026-08-23)

**A deopt stops the run. It does not fall back.**

The reasoning is a cost argument, not a purity one. The interpreter is
the oracle and it is about to get slower on purpose - Stage 4 removes
its vectorization, virtual merge and lane machinery, leaving a plain
scalar interpreter. Running deopted lanes through that is not a
degraded mode, it is a different order of magnitude. Fixing the gap and
resuming is faster than absorbing it, and it is faster the FIRST time,
because a deopt is one lane costing thousands of lanes' worth of time.

So the shape of it is:

1. checkpoint every frame;
2. on any deopt, confirm the checkpoint is on disk, say exactly why, and
   exit with a distinct status;
3. someone reads the reason, adds the kernel or raises the bound;
4. resume from the checkpoint.

A deopt becomes a BUILD-TIME task instead of a runtime cost. That is the
whole idea.

### What does not change

`ok` stays, and it stays sound. It is still the lane-level obligation
the kernel cannot discharge; the only difference is what happens next.
Exiting is strictly safer than the alternative - nothing silently
continues on a lane the kernel could not compute.

The interpreter stays too. It is the differential oracle. It is just not
on the search path any more.

### Two refinements this needs

**Aggregate before exiting, do not stop at the first.** One exit should
yield every fix available, not one. The frame should finish classifying:
each distinct deopt reason, how many lanes hit it, and one example lane
per reason. Otherwise the loop is exit-fix-resume once per reason, which
is the same grind with extra steps.

**Checkpoint-before-exit is an invariant, not a hope.** The exit path
verifies the checkpoint is written and readable before it exits, and
uses a status code distinct from a crash, so tooling can tell "coverage
gap, resumable" from "something broke".

### Why this looks tractable, measured

The traced kernel's whole deopt surface is two things, and the generated
code says so:

* the pm1 pin guard - six `ok` conjuncts (`!has_dashed`, `freeze == 0`,
  `dash_time == 0`, `djump == 1`, `!p_dash`, `!p_jump`). Gone entirely
  if a kernel is specialized per shape and not per key.
* loop-bound exhaustion - `Interp::exec_for` unrolls a numeric `for`
  `bound` times and narrows `ok` on the lanes whose loop was not over.
  A knob: raise the bound, pay in body size.

There are ZERO interval-split deopts. `zi_flr_ok`, `zi_span_ok` and
`zi_fork_flr` do not appear in a traced kernel at all, because the
tracer never forks an interval - the walk's `Op::Split` machinery has no
tracer counterpart. That is a large part of the old path's deopt surface
that simply does not exist here.

### The one thing that would break it

A deopt reason that is genuinely irreducible - a lane that really needs
unbounded iteration, say - would turn "fix and resume" into an infinite
loop. Nothing observed so far looks like that, and the loop bound is the
only candidate. If one appears, the doctrine needs an escape hatch and
this section needs revisiting rather than working around.

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
