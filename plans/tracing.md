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
