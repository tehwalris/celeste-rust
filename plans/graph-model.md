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

Type propagation runs at compile time, costs nothing at runtime, and says which
values denote sets.

BUT IT IS NOT THE FORK TRIGGER, and conflating the two is a mistake I made and
measured on 2026-09-27. A LANE IS ITSELF A SET OF STATES, and the kernel
computes on interval representations, so a value can denote a set and still be
ONE computed quantity per lane that the kernel can branch on:

* `TileFlagAt` (and `Mget`) lower to a single instruction taking the
  coordinates as number registers, so a lane gets one answer however abstract
  its coordinates are. Lane-decidable, no fork.
* `Flr` of an interval is lane-decidable too - but only BECAUSE an assertion
  guarantees the span is one integer (`zi_flr_ok`). Different justification:
  not a type fact, an `own_error` (section 4).

What forces a fork is narrower: the condition's value differs ACROSS THE
CONCRETE STATES ONE LANE STANDS FOR. That is what `reads_interval_cmp`
approximates by asking whether a comparison has an interval operand, and why it
excludes the two families above - not sloppiness, as I first read it.

A `Sel` is where the two predicates must disagree, and the asymmetry is easy to
"tidy" away by mistake. For a NUMERIC operand the condition is irrelevant -
`Sel(c, 5, 7)` is one of two exact numbers whatever `c` is - so judging whether
a comparison straddles must skip it. For a BOOLEAN select it is the opposite: a
select straddles exactly when its condition does. Counting the condition in the
numeric case over-triggers on 31-33 conditions a frame in room (6,0).

So stage 2 needs two predicates, and they must stay apart:

* ABSTRACTNESS - does this value denote a set? (`Symbolic::abstractness`.)
  Used for typing, and for discharging assertions statically.
* LANE-UNDECIDABILITY - can one lane hold both answers? The fork trigger.

Measured on room (6,0): using abstractness as the trigger would mint 50-90
EXTRA forks a frame, all of them `TileFlagAt`/`Mget` (41-76) or `Flr` (8-16)
conditions the kernel decides per lane anyway.

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

## 4. Error is DERIVED from the operators, never carried

Error is a property of a value, and two rules leave it no freedom:

* if any input of an operator has error, its output has error;
* an operator may ADD error of its own, as a function of its inputs, even
  where none of them has any.

So

    error(n) = own_error(n, args) or OR over args of error(arg)

is a bottom-up function of the graph. Nothing in it depends on execution order
or on the path taken, which means IT NEED NOT BE EXPLICIT IN THE GRAPH AT ALL.
It is derivable whenever wanted, and there is exactly one moment worth deriving
it: very late, when the kernel is materialised. Then a row's error is the OR of
the errors of the values it stores.

Every obligation we actually have is an operator's own error:

| operator | `own_error` |
|---|---|
| `Div(a, b)` | `b = 0` |
| `Flr(x)` | `not (flr(lo x) = flr(hi x))` - the span claim |
| `Sel(c, t, f)` | `not Known(c)` - the arms differ, so an undecided `c` leaves the value undefined on that lane |
| a fork | the lane's set is not covered by the parts enumerated |
| an unrolled loop | its condition still holds after the bound |
| an input with an assumed range | the lane's value is outside it |

`Sel` is the one the first draft of this table omitted, and it is one of only
two error-side obligations the code actually has (`interp::premise_for_value`
conjoins `Known(cond)` where a merged VALUE is a select; the other is the
unrolled loop's bound, `interp.rs` "the obligation belongs only to the states
that never left"). It is an `own_error` and not a precondition: the arms are
values, and which one the lane takes is a question about this expression rather
than about the kernel's admissible inputs.

`Div` is vacuous in THIS program and the table keeps it only because the model
should be stated for the operator rather than for the cart. Every division in
the Lua divides by a numeric literal - `8` eleven times, `5` seven times, then
`30`, `1.5`, `64`, `60`, `40`, `4`, `32`, `3`, and no non-numeric divisor
anywhere (checked 2026-09-27 with comments stripped; an earlier scan "found"
`/big`, which is the text `platforms/big chest` in a comment). So there is no
`Div` obligation in the code, and that is correct rather than a gap: `b` is
never a value a lane could make zero.

`Flr` is the instructive one. As a premise (`Known(Flr(x))`) it looks circular -
`Flr` is exact BECAUSE of it, so propagation cannot discharge it without
assuming it - and encoding it that way is what made me fold it away unsoundly
on 2026-09-26. As an operator's own error there is no circularity at all:
`Flr` is simply PARTIAL, a singleton on pain of error.

Trace-time refusals (a symbolic table index, an unsupported construct) are not
error conditions. They are failures to compile, and stay so.

`Interp::poison` is one of these, and it must survive step 4 unchanged. It
fires where `binop_values` found arithmetic on a non-number - Lua itself would
have raised, so no legal run takes that path - and it substitutes a dummy `0`
and records the reason in `illegal`. Making it a value's error would be wrong
twice: the abort is not a property of the substituted `0` (nothing downstream
"uses a bad number"; the run does not continue at all), and a derived error
that no stored value depends on would VANISH, which is precisely the silent
loss the `ok`-not-`guard` choice exists to prevent. Its own doc makes the
argument: clearing `guard` would say "no lane takes this path" and a successor
could disappear unnoticed, while clearing `ok` says "the kernel declines these
lanes", which fails loudly. So `poison` keeps a precondition-shaped home.

One obligation is NOT any value's error and needs its own home: a fork's
COVERAGE - that the lane's set really is contained in the union of the parts
enumerated. It cannot be a value's error, because "the lane is outside part c"
is just `live = false` for that body, and "outside every part" is a property of
the body SET rather than of any value. Left implicit it is the one failure mode
we cannot tolerate: no body's `live` holds, the lane emits nothing, and a state
is lost SILENTLY instead of declining loudly. So coverage belongs with the
input preconditions - beside "this cell lies in the range this body was
specialised for" - which is what `SplitOk` already is today.

Consequences:

* **No `ok` in the tracer.** `State::ok` is not state. Carrying it treats a
  DATA property as a PATH property, which is the actual bug behind obligations
  outliving their values: in room (6,0), 0 of 10-14 merge premises protect a
  select that reaches any row.
* **Demand-driven by construction.** A value nothing stores contributes to no
  row's error, because error was never materialised for it.
* **Fusion never touches error.** Bodies fuse only when their row VALUES are
  equal, and equal values are literally the same nodes - so their error is the
  same expression, not two to be combined. Rows are fused before error exists;
  error is materialised once afterwards. Only `live` needs combining.
* **One polarity, because there is one materialisation.** No `ok`-versus-
  `error` sharing question in the hash-consed graph.

One subtlety, at the kernel boundary only: `own_error`'s condition can itself
be undecidable (`b = 0` where `b` is an interval containing zero), so error is
`{true, false}`. The kernel reads may-error AS error - strict, declines
loudly - which is the semantics `ok` has today.

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

Under this model there is nothing to fuse. Error is not carried, so fusion
concerns only the rows and `live`:

    live = the lane's value lies in the UNION of the parts   (one test)

and error is materialised ONCE, after fusion, from the fused body - bodies fuse
only on equal row values, so the members' errors are the same expression
anyway. No AND-of-implications, no per-fragment copy of either formula, no
incremental fold. `quantify` does not merely get cheaper - it has no mechanism
left by which to arise.

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
  spans a single integer.

NEITHER survives. The first cannot arise: stage 2 forks a branch it cannot
decide rather than merging it and leaving a marker, so there is no merge and
nothing to mark. The second is not an assertion the graph carries either - it
is `Flr`'s own error condition (section 4), which is what dissolves its
apparent circularity. `Known` disappears as a concept.

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
4. DELETE `ok`. Surveyed 2026-09-27: 16 sites write it, and they are three
   different things, so the step is CLASSIFY FIRST, then delete.

   * PRECONDITIONS, 11 sites - obligations on the kernel's admissible inputs,
     which the kernel declines loudly on (`live & !ok`): `widen.rs` 123 (the
     `require` helper - a free function there, NOT a `Domain` method), 201,
     401, 592, 667, 865, 884, 887, 973, `interp.rs:1962` (a `move` fork's
     span premise), `verify.rs:340` (the pin guard). These move to input
     preconditions, beside what `SplitOk` already is.
   * `own_error`, 2 sites - `interp.rs:702` (`Known(cond)` for a surviving
     select) and `interp.rs:890` (the unrolled loop's bound). These become
     derived error.
   * A TRACE-TIME REFUSAL, 1 site - `interp.rs:1240` (`poison`), which stays
     as it is (see section 4).

   The remaining writes are plumbing that dissolves with the field:
   `shapes.rs:159` and `verify.rs:1157` initialise it to true, `state.rs:360`
   merges it (and can REFUSE a merge when the merged `ok` is a select, which
   is the fusion cost this step removes), `verify.rs:601` remaps it.

   Still the invasive step, so do it in halves with the gates green between.
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
