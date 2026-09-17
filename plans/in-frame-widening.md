# In-frame widening: absorbing fields and unmerged undecided outcomes (DESIGN, 2026-09-17, not started)

The fly fruit (plans/fly-fruit.md) and the fall floors (plans/fall-floors.md)
both want fields UNKNOWN at the coarse levels while the frame that reads them
still traces into kernels that refuse nothing and fork little. This is the
mechanism for both. Philippe's rules apply throughout: a widening only replaces
a value by something visibly containing it; no ranges justified from outside
the frame's computation; an uncollected fruit (or a floor) is one row.

## What exists today (the facts the design rests on)

- **Splits and merges** (`state::split`, `state::merge`, `Interp::collapse`).
  An undecided `if` splits the state; `collapse` runs after every statement and
  merges every pair of same-shape outcomes, joining differing numbers/booleans
  into `Sel(cond, t, f)` on the separating decision.
- **The premise.** A merge conjoins `Known(cond)` into `ok` only where a SELECT
  survives the join (`state::merge`, 2026-09-16): equal arms fold, booleans
  combine into Kleene algebra the kernel evaluates exactly on (value, known)
  masks. A surviving select on an undecided lane is a fatal decline.
- **Unmergeable outcomes are emitted.** Two outcomes of different shape (or a
  table vs nil `return`) stay separate with Kleene guards, and a kernel emits an
  outcome whose `live` is unknown (room (2,0)'s fruit band: the undecided `hit`
  emits both outcomes).
- **Forks** (`fork_flr`, `fork_int`, `fork_held_inputs`) are CONFIGURATIONS: the
  fused graph is specialized per configuration, so a fork duplicates the whole
  frame's bodies (hash-consing shares what folds equal). The kernels carry 2
  bits per fork (at most 4 ways). plans/regions.md measured the cost: lanes
  genuinely take ~4-5 of 16 configurations while every configuration's bodies
  run for every lane.
- **Held buttons** are the precedent for an unknown field at a level: forked at
  the frame's input, written unknown (`AV::UBool`) at its output, one level
  coordinate, a registry dimension, `Rt2::widen_to` for the mark filter.

## The tensions

1. **Soundness by containment**: the result must visibly contain the real
   value, per rule 1.
2. **Configurations are the expensive currency**: a fork multiplies the whole
   frame; an extra outcome only adds the bodies after its split.
3. **Premises are per merge, not per use**: a select that nothing reads later
   (a field the boundary is about to overwrite) still makes an undecided lane
   refuse.
4. **Identity**: two floors' unknown `collideable` are independent; one shared
   node (or one shared fork) would silently correlate them, which is unsound.
5. **Precision inside the frame**: absorbing to unknown early loses what the
   frame could still decide; absorbing late needs the premise machinery to
   tolerate undecided selects.
6. **The number type**: a full-range interval raises at the 16.16 extremes;
   an unknown must stay unknown under every operation.
7. **Generality**: a list of fields per level is acceptable (the fruit band, the
   held trails already are) but the mechanism should not know about floors.
8. **Two sides must agree**: the tracer's frame and the block model's boundary
   (row keys, checkpoints, the mark filter's projection).
9. **Tracing cost**: every undecided branch is traced; `collapse` keeps the
   frontier at the number of genuinely different futures (`max_states` 256 per
   block).

## Options

- **A. Boundary widening, fork every unknown input.** A floor's `state` 3-way,
  `delay` and `collideable` 2-way each: 12 configurations per nearby floor,
  x144 for two. Rejected: tension 2.
- **B. Absorbing fields.** A level names fields (per object type) that are
  UNKNOWN at that level. They enter the frame as unknown leaves, a merge joins a
  differing absorbing field to the unknown instead of a select (no select, so no
  premise), and the boundary writes them unknown. Everything an object computes
  about its own absorbing fields rejoins at the next statement, with no fork.
- **C. Premises on use.** Keep precise selects inside the frame and attach
  `Known(cond)` only to selects reachable from the frame's outputs, guards and
  live conditions. The floor's own update would stop refusing (its selects die
  at the boundary). More general than B, but it changes a core invariant of
  `ok`, and it does not help a select that IS read (the player's collision).
- **D. In-frame widening at object boundaries.** Widen an object's fields at the
  end of its update call. Too late: `collapse` merges the update's arms at the
  end of each `if`, inside the call.

And for the reads of an unknown by code that is NOT absorbing (the player's
collision against a floor whose `collideable` is unknown):

- **E1. Fork the unknown where it is read** (memoized per unknown leaf):
  configurations again, one per nearby floor, and the fork has to know it is at
  a use rather than inside the owner's own update.
- **E2. Do not merge on an unknown-derived undecided condition.** When a merge's
  separating condition depends on an unknown leaf and a non-absorbing select
  would survive, keep the two states as separate outcomes instead of adding a
  premise. Each carries its Kleene guard; the kernel emits both where the
  condition is undecided. This is the existing "an unknown live emits" path,
  taken on purpose.

## The design: B + E2

**Absorbing fields.** A level spec flag per widening (like `h`) switches on a
set
of `(object type, field)` pairs: at `f` the fly fruit's `step`, `spd.y`,
`rem.y`, `y`, `fly`; at the floors' flag the fall floors' `state`, `delay`,
`collideable`.

- **Input**: each absorbing field of each object is a fresh UNKNOWN leaf with
  its own identity (`Op::Unknown(id)`, numbers and booleans), never a pinned
  constant, and a missing field (the floors' `delay`) is materialized as its
  unknown - which replaces `widen::ABSENT_AS_ZERO` at these levels, provided the
  unknown covers nil.
- **Operations**: anything on an unknown number is unknown (never raises);
  `flr`/`__split_by_flr` of an unknown is unknown with NO fork (so the fruit's
  6-way `move` fork disappears); a comparison involving an unknown is an
  undecided boolean; And/Or/Not are Kleene.
- **Merge**: `join` of two differing values of an absorbing field yields that
  field's unknown leaf (containment: unknown contains both), not a select.
- **Output**: absorbing fields are written unknown at the boundary (a uniform
  column, `AV::UBool` or the new unknown number), so rows never differ in them.
- **Refusal**: an unknown (or a value derived from one) reaching a NON-absorbing
  output, a table index, a loop bound or a select's value bit is refused at
  trace time, loudly: it is a coverage gap in the field list, never silently
  a garbage value.

**Unmerged undecided outcomes (E2).** In `collapse`, a pair whose merge would
leave a non-absorbing select on a condition that depends on an unknown leaf is
not merged; both states continue as separate outcomes with their Kleene guards.
Where the condition is decided (exact levels, or folded away by the region
key's range analysis) nothing changes.

### Why this, against the tensions

- Containment (1): unknown contains every value; separate outcomes cover both
  arms.
- No configurations (2): nothing forks. The player's move against one undecided
  floor adds outcomes along its pixel steps (the stop arm ends the loop, so it
  grows linearly with the steps, not exponentially); the region key folds far
  floors, so this happens only next to one or two floors.
- Premises (3): absorbing merges build no select; E2 keeps outcomes apart where
  a select would need a premise, so no undecided lane refuses.
- Identity (4): one leaf per (object, field), independent in Kleene algebra; no
  shared fork correlates floors.
- Precision (5): inside the frame, a field only absorbs where two arms actually
  differ in it; a value the arms agree on stays exact. The coarse level is
  coarse by choice, and the finer levels keep the fields exact.
- The number type (6): the unknown leaf's operations are total.
- Generality (7): the mechanism knows fields and leaves, not floors or fruit.
- Two sides (8): the block model gets the unknown number type as a uniform
  column; `Rt2::widen_to` projects an exact row's absorbing fields to unknown for
  the mark filter; the boundary's key hashes the unknown as one value.
- Tracing (9): the owner's own branches are traced and rejoin immediately;
  unmerged outcomes stay bounded by `max_states`.

### The fly fruit under it

Every fruit field is absorbing at `f`. `move`: `rem.y + spd.y + 0.5` is unknown,
`__split_by_flr` returns unknown without forking, `y` stays unknown. The update's
`fly` branch splits and rejoins by absorption. The collection test is a Kleene
overlap; `collide` returns the player or nil (not joinable), so collected
(`djump` refilled, the fruit deleted: a different shape) and not collected are
separate outcomes, as today's room (2,0) fruit band. `y < -16` likewise. No fork,
one fruit row. This replaces plans/fly-fruit.md's per-field ranges (`spd.y`,
`rem.y` checked ranges are no longer needed: unknown contains them) and its
"local forks" item.

### The fall floors under it

`state`, `delay`, `collideable` absorbing at the floors' flag. The floor's
update branches split and rejoin by absorption. The player's `is_solid` ->
`collide` reads `collideable` in a Kleene And chain; the move loop's "stop or
step" on that undecided result would merge with a select on the player's
`x`/`spd.x`/`rem.x`, so E2 keeps stepped and stopped as separate outcomes. Far
floors fold away under the region key.

### Absent fields: a native nil-or-unknown number (replaces `ABSENT_AS_ZERO`)

Philippe, 2026-09-17: instead of writing a missing `delay` as 0, a NATIVE value
"nil, or an unknown number". Arithmetic (`delay - 1`) and ordering comparisons
(`delay <= 0`) treat it as an unknown number, with the argument that if it were
nil the cart would have halted right there, so no optimal run is on that path;
every other operator refuses (a trace-time error). A missing field of the listed
kind is materialized as this value at every level, so the field is always
present and the 2^12 shapes stay one, and `widen::ABSENT_AS_ZERO`,
`materialize_absent_fields` and `cart::check_absent_fields` go.

At the exact levels this costs nothing real: a never-broken floor holds the
value but never reads it (only states 1 and 2 read `delay`, and
`break_fall_floor` writes 15 before setting state 1), so nothing exact is lost
where the game can reach. It is the same leaf family as the absorbing unknown
(an unknown number that also covers nil), with a narrower operator table.

## Levels

The two-rungs-at-`r0` pattern (Philippe): a coarse `r0` with the fruit and the
floors absorbing, then an `r0` (or a later level, if level 0 is still too wide)
with them exact, filtered by the first one's marks, where fewer floors remain on
paths that can still finish in time.

## What has to be built

1. `Op::Unknown(id)` leaves (numbers, booleans) with total operations; the
   codegen for them (a known mask of 0 for booleans; an unknown number column
   that operations propagate and comparisons turn into undecided booleans).
2. Absorbing-field sets per level flag; input leaves; the absorbing `join`;
   unknown outputs at the boundary; the trace-time refusal of an unknown
   escaping into a non-absorbing sink.
3. E2 in `collapse`.
4. The block model: the unknown number column type, row keys, checkpoint
   format, `Rt2::widen_to` for the mark filter.
5. The level flags and registry dimensions.

## Open questions

- **E2's frontier** near several undecided floors inside one region: bounded by
  `max_states`, but how close does it get? Measure on room (3,0).
- **The escape refusal's reach**: which sinks count (a `Sel` value bit, a table
  index, a loop bound, a non-absorbing output); getting that list wrong is how a
  silent garbage value would get in.
- **Relation to C**: premises on use would also help other dead selects; worth
  keeping in mind, not needed here.
- **The fruit agent** (worktree, reduced scope) was told to find its own way to
  a no-split `move` for the fruit; this design would replace that part.
