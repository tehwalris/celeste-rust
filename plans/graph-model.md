# The graph model we are aiming for (Philippe, 2026-09-26/27)

The design the code should be aligned with. Not a description of what exists -
see "Where the code diverges" for that. Written after a day of trying to
shrink room (6,0)'s kernels by local rewrites and failing five times; the
value of this document is that it says WHY those were the wrong shape.

## 1. Two stages, and the interface between them

**Stage 1, tracing proper.** The program (one frame of the cart's Lua) becomes
a graph in which every operator, GIVEN CONCRETE INPUTS, yields a concrete
output. The whole graph is executable. This stage knows nothing about
abstraction or precision levels; its only parameters are things like unroll
bounds.

Its output is not one graph but a SET of outcome graphs with guards: a branch
that kills an object changes the heap shape, and no single graph can return
two shapes. Those guards are ordinary conditionals as far as stage 2 cares.

**Stage 2, making the graph executable under abstract inputs.** Feed in input
TYPES. A type denotes a SET of concrete values: a singleton (exact field), an
interval, `{true, false}`, all numbers. Propagate them. Most operators are
fine - interval arithmetic still yields one output. Where an abstract value
reaches a branch-like node the graph stops being executable, because no arm
can be chosen, and that is the ONLY case needing special handling. Stage 2
resolves it by forking until the graph is executable again, returning several
rows instead of one.

The interface is exactly "graph + input types". The precision ladder re-runs
stage 2 only.

## 2. Values are sets; types say which

A concrete value is atomic. An abstract value is a set of concrete values. The
input shape plus the level fix each input field's type; one static pass
(TYPE PROPAGATION) labels every node with whether its set is a singleton.

Type propagation is the whole of what decidedness means. It runs at compile
time, costs nothing at runtime, and decides two things: which branch-like
nodes need forking, and which assertions are discharged statically.

## 3. Forking is one operation

To fork: choose a node, partition its set into parts, duplicate the cone
downstream of it once per part with a narrower node substituted, and fold.
One input row becomes N rows. Each body carries a VALIDITY - "this lane's
value at that node lies in this part".

There is exactly one fork concept. Splitting `{true, false}` into `{true}` and
`{false}` substitutes interned constants; splitting an interval substitutes
fragments. Downstream, and for every simplification, they are the same thing;
the constant case is only the degenerate one.

The freedom that matters is WHICH node to split:

* Splitting the branch condition partitions a predicate's two values. It
  learns one bit, and two predicates of the same unknown split INDEPENDENTLY -
  so their product admits combinations no concrete value could produce, and
  each needs its own "is this answer achievable" algebra.
* Splitting the unknown the predicate reads partitions the unknown's own set.
  Every predicate derived from it is then consistent by construction, its
  validity is a membership test rather than a formula, and predicates that no
  longer matter fold away instead of becoming forks.

So: walk back from the offending branch to an unknown source, and partition
THAT, at the thresholds the branch compares against. Recurse if one split does
not make the branch decidable.

## 4. Error is a property of values

Each node carries an error condition: a boolean saying this computation was
invalid on this lane. Sources are genuine value facts - a fork's coverage
claim, the `flr`-span assertion, range bounds, a division by zero.

It propagates along the same edges as the computation:

    error(n) = own_error(n) or OR over args of error(arg)

and A ROW'S ERROR IS THE OR OF THE ERRORS OF THE VALUES IT STORES. This is a
second relation over the same nodes - a different edge set with its own roots,
not a separate graph.

Two properties fall out, and both are things we currently lack:

* **Demand-driven.** A premise about a value nothing stores is in no row's
  error. Obligations cannot outlive the values they are about.
* **One polarity.** Fusing bodies is OR of `live` and OR of `error`. No AND
  of implications anywhere, which is what makes fusion cheap (see 6).

`error`, not `ok`. The complement is not cosmetic: it is what matches the two
masks' polarity so they share structure in a hash-consed graph, where an `And`
tree and an `Or` tree share nothing.

## 5. The kernel

The compute graph with every fork resolved to constants, plus two masks per
body: `live` and `error`. A lane emits its row where `live and not error`.
Three-valued masks stay, with the polarity uniform: an unknown `live` reads as
live (over-approximate, and a finer level refutes), an unknown `error` reads as
error (strict, declines loudly).

## 6. Why the polarity and the value-attachment pay

Fusing the configurations of a fork that the ROW does not read currently needs

    live = OR over c of live_c
    ok   = AND over c of (live_c -> ok_c)

so each fragment needs a copy of BOTH formulas, and the two trees share
nothing. That is `lower::quantify`, and it inflated one room (6,0) body's
ok/live cone from 1,974 nodes to 96,540 (48.9x), which was 97-98% of the
kernel's arena.

Under this model the same fusion is

    live  = the lane's value lies in the UNION of the parts   (one test)
    error = OR over c of error_c                             (error cone only)

No AND-of-implications, no per-fragment copy of `live`, and the live side
collapses to a single membership test. `quantify` disappears.

## 7. `Known` is an assertion, never a type fact

`Known(c)` conflates two unrelated jobs today:

* **Decidedness** - "this lane decides `c`". That is type propagation's answer,
  known at compile time. As a runtime premise it is worse than useless: at a
  platforms-unknown level the condition is undecided for EVERY lane, so the
  check would refuse everything, and in practice `fork_known_premises` finds
  it later and converts it into a fork. It is a to-do marker from the tracer
  to the fork pass, encoded as a graph node that then sits in `ok` and
  inflates everything downstream of it.
* **A genuine value assertion** - `Known(Flr(x))` claims the lane's interval
  spans a single integer. Unknowable statically (and circular to discharge by
  propagation: `Flr` is exact BECAUSE of this premise). This is legitimate and
  belongs in `error`, under a name that says what it asserts.

Only the second survives. In the target model the first cannot arise, because
stage 2 forks a branch it cannot decide rather than merging it and leaving a
marker - there is no merge, so there is nothing to mark.

## Where the code diverges

* **The tracer is an interpreter, so the two stages are collapsed.** At an
  undecidable branch `state::split` copies the state, both arms run, and
  `state::merge` rejoins them into one state with a `Sel` per differing slot.
  The graph's shape therefore depends on which conditions were decidable,
  which depends on the level - so the lattice walk re-traces per (shape,
  region, level): room (6,0) does 914 traces in a 65 s walk before a single
  kernel is built. Under the separation that is one trace per shape.
* **`Sel` and `Known` are artifacts of merging, not of the program.**
* **Two fork mechanisms where there should be one.** `fork_flr` / `fork_int` /
  `fork_table` split a value; `both_values` + `verify::fork_known_premises`
  split a condition. The second is what room (6,0) uses, and it is the source
  of its 57-79 forks over 10 unknowns, of the validity algebra, of the
  impossible combinations, and of the 10-13 forks per frame that change no
  stored field at all (the platform wrap: whether it wrapped cannot matter,
  because the output widening overwrites `x` with the whole path either way).
* **`ok` is one boolean per state, accumulated by AND**, rather than per-value
  errors OR-ed into the rows that store them. Hence obligations outliving
  their values: in room (6,0), 0 of 10-14 merge premises protect a select that
  reaches any row.
* **`quantify` exists at all.**

## Order of work

Each step is checkable against the three pinned gates (ckhash, posgraph,
marks) plus room (5,0)'s kept counts, which is the only reason a refactor of
this size is safe.

1. Type propagation as one real pass. The pieces exist: `Symbolic::is_interval`
   (numbers) and `Symbolic::runtime_unknown` (booleans, added 2026-09-26).
2. Delete `Known`-as-decidedness, using that pass. Keep the assertion, renamed.
3. Unify the fork operations into one `fork(node, partition)`.
4. `ok` -> per-value `error`, OR-composed into rows. The invasive step; do it
   in two halves with the gates green between them.
5. Domain-first fork choice, partitioned at the thresholds the branch uses.
6. Delete `quantify`.

Steps 1-2 are small and land immediately. Steps 3-6 are load-bearing.

## The tension this model does NOT remove

A single lane stores a platform's `x` as the whole `[-16, 128]` path - that is
the point of the widening, and it is what lets states from different frames
merge. So any domain partition of it has arity equal to its part count, and
the overlap predicates only fold at roughly 4.5 px granularity: 32 parts, per
platform.

Simulating the platforms' deterministic motion over the room's 71 frames, if
the row stored a zone per platform instead of the whole path:

| zone size | distinct zone tuples over 71 frames |
|---|---|
| exact | 46 |
| 72 px | 4 |
| 36 px | 7 |
| 18 px | 15 |
| 9 px | 30 |
| 4.5 px | 46 |

Only reachable tuples occur - one underlying phase drives all ten platforms -
so storing zones costs about Z, not Z^10. But the granularity that makes
predicates fold (~4.5 px) is exactly the granularity at which no cross-frame
merging survives, and where merging is strong (36-72 px) nothing folds. The
possible middle is ~18 px, where predicates fold for lanes whose platform is
clearly far and forks remain only for the boundary band; that is unmeasured.

What this model changes is not that cost but WHERE it lives: in one explicit
choice of partition, instead of smeared across validity algebra, `Known`
markers, phantom forks and `quantify`. Room (6,0) still needs a decision about
how much the row remembers, and it should be taken with the machinery honest.
