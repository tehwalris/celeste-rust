# Making the recipe survive a change to the base program

## The problem, measured

Changing `foreach` in `lua/builtin_level_3.lua` from a plain index walk to
PICO-8's deletion-safe one - about fourteen extra instructions in ONE
function - invalidated **at least 371 of the recipe's 905 entries**, and the
drop-and-retry loop was still finding more when it was stopped.

That is not a property of the change. It is a property of the recipe.

### Why, exactly

Recipe entries address instructions as `%n`. Two different kinds of `%n`
hide behind that syntax, and only one of them is stable:

* **Source ids.** Assigned by `LocalIdGenerator` per FUNCTION at compile
  time. Editing `foreach`'s Lua cannot move `_update_62`'s ids, because
  they are numbered independently. These are already stable.
* **Rewrite-minted ids.** `rules::LocalIdAllocator::for_function` scans the
  function for the highest id in use and mints `max + 1`. So the id a rule
  mints depends on how many ids EVERY PRECEDING REWRITE happened to consume.

The entries that broke are the second kind. `_update_62`'s source
instructions stop at **%171**; the first cascade failure was `j2_014`
naming **%325** - an id no compiler ever produced. Inlining a two-instruction-
bigger `foreach` consumed more fresh ids, every later mint shifted, and
every entry naming one of them stopped resolving.

### What it has already cost

Two fidelity fixes in one week have been shaped around this rather than by
what is correct:

* `got_fruit[3]` on an empty table had to be fixed in the INTERPRETER
  rather than by adding four lines to `_init`, explicitly because those
  four lines would break every recipe entry in `_init`/`__init`.
* The `foreach` fix - which loses real spring bounces, leaves stale objects
  after a death, and shifts rooms (0,0)/(1,0)'s optima by a frame relative
  to PICO-8 - is reverted and unfixed.

More is coming: the numerics have never been differentially tested against
real PICO-8 either. **The recipe's addressing has made the model
effectively immutable, and that is the thing to fix.**

## Goal

A change to the base program should cost effort PROPORTIONAL TO THE CHANGE.
Editing `foreach` should break the entries that are about `foreach` - there
are three - and nothing else.

Secondary, from Philippe: the recipe should be simple to understand and
clearly correct. No dynamic behaviour, no clever matching that might bind
somewhere unintended. Named locals rather than piles of numbers.

## Phase 0 - the gate, BEFORE anything changes

The refactor's entire claim is that it changes how ids are ASSIGNED, not
what program comes out. So gate it on exactly that:

> the final rewritten program, before and after, must be identical up to a
> bijective renaming of `LocalId`s - AND slot-identical.

Philippe's framing, and it is the right one. Notes:

* This is NOT general graph isomorphism. The IR is ordered, so a canonical
  walk induces the bijection in linear time: at each instruction map A's
  defined id to B's, then require every operand to map consistently. A
  mismatch reports a location, not just "not isomorphic".
* `Cfg.named` is an `FxHashMap<Label, Block>` and prints in hash order, so
  the walk must order blocks explicitly - by reachability from entry, with
  label as a tiebreak. Trusting iteration order would make it flaky.
* **Slot identity is not optional, but it is not the end state either.**
  `allocate_slots` assigns each local a runtime slot, and row keys - hence
  every checkpoint and every certified `g` - depend on it. So the gate
  compares slots, and today that comparison is "did they move".

  Philippe's call (2026-08-15): slot NAMES should themselves be derived and
  stable, not merely preserved by accident, and it is worth breaking them
  ONCE to get that property. So the end state is:

      a local's slot is a deterministic function of its STABLE NAME,
      not of the order its id happened to be assigned in.

  which makes checkpoints survive future refactors instead of surviving
  this one by luck. The break is paid once, during Phase 2, and both rooms
  are re-derived after it - they already need re-deriving for the chunk-cap
  fingerprint anyway, so the marginal cost is one campaign, not three.

  Until that point the gate's slot half stays "identical"; after it, the
  gate becomes "slots are the function of the names that the allocator
  claims", which is a stronger and more useful statement.
* Capture today's final program as the golden baseline FIRST. Then every
  commit of the refactor is gated from the start, instead of the whole
  thing being validated retrospectively against a 905-entry recipe.

This gate outlives the refactor: it is the general "did my change alter the
compiled program at all" check, which is precisely what was missing when
the `foreach` edit turned out to move 371+ entries.

## Phase 1 - provenance instead of a running counter

Give a minted id an identity derived from WHO minted it, not from how many
ids existed when it was minted. Entry `i1_023`'s third temporary should be
the same id no matter what any other entry did.

### The stride sketch is DEAD - measured, 2026-08-15

The obvious version - reserve a disjoint id range per entry, `BASE + k *
STRIDE + i` - was implemented and then abandoned, because stability cannot
live in the id's VALUE:

* `SlotMap::identity()` is an empty vec meaning "slot == LocalId", which is
  what every CFG carries until `allocate_slots` runs last;
* `LocalEnv` sizes its storage BY SLOT;
* so a sparse id near 2^20 makes every intermediate state allocate a
  million slots - and `bisect`, `screen` and the rule verifiers all
  interpret the program before `allocate_slots` compacts anything.

Keeping ids dense and making the RANGE stable are in direct conflict. The
resolution is that they are answering different questions, and only one of
them needs the id:

    ids stay dense and are free to move;
    STABILITY LIVES IN A NAME, in a side table.

which is what Philippe proposed at the outset. A minted instruction gets a
name derived from the entry that minted it (`i1_023.t3`); the recipe
addresses names; the id remains whatever keeps the runtime compact. This
also subsumes Phase 2 rather than preceding it.

### The feasibility problem, and its answer

`LocalId` is a `usize` used as a DENSE ARRAY INDEX:
`slots.rs:217` does `of_local[usize::from(id)] = slot`. Sparse ids would
cost memory proportional to the maximum id, which a stride scheme makes
large (900 entries x stride).

Answer: **sparse while rewriting, compact once at the end.** Stability is
only needed while the recipe is being applied; density is only needed at
run time. `allocate_slots` already runs last, so a canonical compaction
immediately before it is a deterministic function of the final program and
is therefore itself stable. Phase 0's gate covers exactly this - if the
compaction is wrong, slots move and the gate fails.

Open questions to settle with measurement, not argument:

* does anything else index densely by `LocalId`? (grep found one site; the
  claim needs to be checked, not assumed)
* how large does the peak id actually get mid-rewrite, and does any
  structure allocate proportional to it before compaction?

## Phase 2 - names on top

Provenance makes ids STABLE; names make the recipe READABLE, and they are
what Philippe actually asked for. They layer on rather than replacing the
mechanism:

* names come from the Lua where one exists (a local's source name), and
  from the minting entry otherwise (`i1_023.tmp3`);
* inlining prefixes with the call site so nesting cannot collide;
* a mint asserts its name is unused in the function, and a whole-program
  well-formedness check asserts global uniqueness. Collision safety is
  asserted, not assumed.

The recipe then reads `{"cell": "%foreach_1.loop_i", ...}` rather than
`{"cell": "%21", ...}`, and the `%n` form stays valid for source ids so the
migration can be incremental.

## Phase 3 - migrate the 905 entries

Mechanical, and gated by Phase 0 at every step: for each entry, resolve its
current `%n` against the program as it stands at that point in the replay,
emit the stable name, rewrite the entry. The gate proves the resulting
program is unchanged.

## Order of work

0. isomorphism + slot gate, baseline captured  <- start here
1. provenance ids, gate green
2. end-of-pipeline compaction, gate green
3. names, gate green
4. migrate the recipe, gate green
5. THEN redo the `foreach` fix, and confirm it breaks only its own three
   entries - which is the whole point, and the test that this worked

## What NOT to do

* Do not re-derive the 905 entries by hand against the new numbering. That
  is the same work every time the model changes, which is the problem.
* Do not make the recipe match structurally/dynamically ("the third store
  in the loop body") to dodge numbering. That is the "random dynamic
  behaviour" Philippe ruled out: it silently rebinds to a different
  instruction when the program changes, which is worse than failing loudly.
* Do not accept the 705-entry recipe with the broken entries dropped
  without measuring what optimisation was lost.
