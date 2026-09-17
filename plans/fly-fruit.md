# The fly fruit: unknown at level 0, exact above (PLAN, 2026-09-17; level 0 built)

Room (3,0)'s fly fruit is the largest single reason its level 0 does not
saturate (plans/room30.md "Level 0 does not saturate"). This is the plan for
widening it, agreed with Philippe.

## Built (2026-09-17, level 0 only, the minimum to measure)

Opt-in through the level flag `f` (`r0sxhf`; `FruitPrecision`, rem Bits(0) and
exact position only, a registry slot of its own). Not built: the block-model
side (`Rt2::widen_to`, the mark filter refuses a fruit-unknown coarser level
loudly), the finer-level dead `step`, gates and ladder runs.

- **The unknown number** is a graph leaf, `Op::UnknownNum`: arithmetic, `min`/
  `max`, `abs`/`flr` of it are it, `sin` of it is [-1, 1], a comparison with it
  is an undecided atom `Op::UnknownBool(k)` (one per site, so boolean
  simplification never cancels two independent unknowns). It never reaches a
  kernel: a field holding it is stored as the uniform `AV::UNum` (checkpoint
  tag 8), its root gets a literal placeholder, and `emit::bind` refuses any
  other root that reads it.
- **Literals stay literals.** At a fruit-unknown set, arithmetic and comparisons
  whose operands are all literal intervals are evaluated at trace time with
  `Graph::eval` (the one definition); an undecided literal comparison is an
  atom. So the fruit's whole numeric computation is lane-independent literals.
- **The merge joins** where NO LANE decides the condition (its cone has no cell,
  button or fork) and the arms are literals: the hull (or the unknown number;
  booleans that differ join to an atom). This is the "sound join" rejected for
  lane data (plans/bucket-dispatch.md); here no lane can take an arm, so the
  select + `Known` premise would decline every lane. Where the arms differ by
  lane data (collected or not once the fruit has flown away: the player's
  `djump`), the two states are NOT merged: they stay two successors, like two
  shapes (`Domain::independent_undecided`; the same for a `return` value in
  `collapse`/`collapse_values`).
- **The local fork (option chosen: a fork scoped to the call).** `__split_by_flr`
  of a literal interval runs each grid fragment as its own TRACE state (tagged
  `State::frag`, never merged by `collapse`), and they rejoin when the enclosing
  Lua call returns (`Interp::rejoin_fragments`): merged on an undecided atom, every
  difference must join to a literal, else the trace is refused. (A slot both
  fragments hold alike may already hold an older select, e.g. a fall floor's
  `delay`; the rejoin counts only the selects it makes.) For the fruit's
  `move` the 6 fragments of `rem.y + spd.y + 0.5` in [-3.5, 1.5) rejoin as `rem.y`
  = [-0.5, 0.5) exactly and `y` unknown, so the frame gains no configuration.
  Why not the others: a graph fork multiplies every body's configurations (6
  times, then collapsed again only after specialization), and a no-split `flr`
  would need to see `solids` at the split site, which the value does not carry.
- **The widening** (`widen::fork_fruit_inputs` at frame start, `widen_fly_fruit`
  at the outputs): `step`, `y` unknown; `spd.y` = [-3.5, 0.5] and `rem.y` =
  [-0.5, 0.5) as literals; `fly` an atom, stored `AV::UBool`. At the output the
  frame's computed `spd.y` / `rem.y` must be literals inside their ranges (checked
  at trace time, so for every row at once) or the trace is refused. The lattice
  never pins these fields. The post-`_init` start block (`Block::from_state`) is
  keyed with the fruit exact: storing the decided fruit is exact, the kernel
  replaces it at the frame's start, and no frame returns to that shape.

### Measured: room (3,0) level 0, `r0sxhf` against `r0sxh` (2026-09-17)

`CELESTE_REGION=32,6 rewrite forward --level r0sxhf --to 55 --room 3,0`,
release, 32 threads, fresh checkpoint dir. The walk reaches 10 shapes in 115
(shape, region) nodes (9 in 114 at `r0sxh`: the fruit collected after it
flew away is a shape of its own). No decline, no coverage gap, to f55.

| frame | `r0sxh` states | `r0sxhf` states | fewer | `r0sxhf` frame | growth |
|---|---|---|---|---|---|
| f40 | 176,925 | 133,228 | 1.33x | 0.88 s | x1.36 |
| f44 | 825,170 | 376,427 | 2.19x | 1.9 s | x1.31 |
| f48 | 3,886,471 | 1,334,635 | 2.91x | 3.9 s | x1.40 |
| f50 | 8,993,524 | 2,523,979 | 3.56x | 6.7 s | x1.37 |
| f52 | - | 5,289,522 | | 13.8 s | x1.47 |
| f55 | - | 17,556,119 | | 56.1 s | x1.51 |

(`r0sxh` frame times, from the stopped search: f40 0.4 s, f44 1.6 s, f48 7.9 s,
f50 20.8 s. Its RSS, 13-16 GB, held 17 prebuilt kernel sets; this run peaks at
4.9 GB at f50 and 14.7 GB at f55 for level 0 alone, so they don't compare.)
55,125,381 states visited through f55.

**Kernel build time regressed:** the walk takes 414 s (493 traces in 13
rounds) against 20.8 s at `r0sxh`; the 115 kernels then assemble in 2.2 s. Not
the atom walk (`reads_unknown_atom` memoized post-order: the same 414 s, and
identical kept/visited counts f1-f36). Unverified suspect: `collapse` re-trying,
at every statement, pairs of states it keeps as two successors, with a full
heap join before each refusal.

So the widening pays and the factor grows (1.3x at f40, 3.6x at f50), but level 0
still does not saturate: growth is x1.37 at f50 and rising to x1.51 at f55. That
matches room30.md: the fall floors and the player's exact speed remain.
Against the no-fruit EXPERIMENT (f050 797,366; f055 5,788,531) this is still
about 3x more. Not measured which part: the fruit gone (collected, or flown
away), the refilled `djump` rows, or the two-successor merges.

Found on the way, not fixed: `Interp::collapse_values` joins an expression's
two values with a select and no `Known` premise (the premise is only added for
heap selects), so `x = c and a or b` on an undecided per-lane `c` blends by the
condition's value bit.

## Why

- **Measured** (room (3,0), level 0 `r0sxh`, `rewrite coarse-census`, f47,
  2,591,387 states): erasing the fruit's fields merges 2.30x; `step`, `y`,
  `rem.y` and `spd.y` jointly 2.17x; any one alone ~1.00x (they vary together).
  With the fruit erased the state set is monotone (every earlier state recurs
  in a later frame), so the door would drop ~88% of f47's rows.
- **Two counters.** While it waits, `step` grows by 0.05 every frame and drives
  `spd.y = sin(step)*0.5`: the same in every row of a frame, but no state ever
  recurs across frames. Once the player dashes, it flies, and its `spd.y`/`y`
  follow a fixed curve from the frame of the dash: rows of one frame split by
  when you dashed, for the ~16-20 frames of flight.
- **It matters for the route.** It sits at (16, 32) in the open left column,
  about 40 px below the exit gap (x 40-63), next to the pillar
  (plans/room30.md, the map). It is collectible while flying (the collection
  test runs after either branch) and collection refills the dash
  (`djump = max_djump`): a false collection is a free mid-air dash near the
  exit.

## The fruit's fields

| field | waiting (`fly` = false) | flying (`fly` = true) |
|---|---|---|
| `step` | +0.05 per frame, read by `sin(step)` | never read (only the waiting branch reads it; the fly fruit has no `draw`) |
| `spd.y` | `sin(step)*0.5` | `appr(spd.y, -3.5, 0.25)`, exactly -3.5 within 16 frames |
| `rem.y`, `y` | `move`: `rem.y += spd.y`, `amount = flr(rem.y + 0.5)`, `y += amount` (while waiting `amount` is always 0 concretely) | the same; deleted when `y < -16` |
| `fly` | set once `has_dashed` | stays true |
| `x`, `start`, `hitbox`, `spr`, `flip`, `solids` (false), `spd.x`, `rem.x` | constant | constant |

Collection (overlap with the player) deletes it and sets a key in `got_fruit`:
existence is a SHAPE difference (two shapes), and that stays.

## The rules this plan keeps (Philippe)

1. **A widening only replaces a value by something visibly containing it**: a
   range that holds it, or unknown. Never by another value (writing `step :=
   0.5` because nobody reads it is out, however true the argument).
2. **No bands justified from outside the frame's computation.** "The fruit
   never leaves [-20, 128]" is true, but a frame computed from that interval
   moves it past its own edge; the containment would be an argument, not a
   computation. A widened range must be a fixed point of the frame itself, and
   checked per row where it is not trivially total.
3. **The uncollected fruit is ONE row per frame**, its fields intervals - not
   several rows because `move`'s `__split_by_flr` forks it.

## The ladder

```
CELESTE_LADDER="r0sxhf,r0sxh,r1sxh,...,r15sxh,rxsx"
```

- **`r0sxhf`** (new flag `f`: fruit unknown): the persistent, extended level 0,
  where the fruit costs nothing. Its backward marks every state on a path to a
  win, including false left-column refills.
- **`r0sxh`**: the same rem precision with the EXACT fruit, filtered by
  `r0sxhf`'s marks (its rows projected onto fruit-unknown keys). Its own marks
  have only real routes and feed `r1sxh` upward. A finer level: recomputed per
  horizon, small under the marks.
- **`r1sxh` ... `rxsx`**: the fruit exact, except the dead flying `step` (below).

## Per field, at `f` levels

| field | widened to | why it contains the real value, as a computation |
|---|---|---|
| `step` | UNKNOWN (the new number type below) | trivially; `sin(unknown)` = [-1, 1] |
| `spd.y` | [-3.5, 0.5], checked per row | both branches compute inside it: `sin*0.5` in [-0.5, 0.5], easing toward -3.5 from inside stays inside |
| `rem.y` | [-0.5, 0.5), checked per row | `move`'s own arithmetic keeps it there (as the player's rem at level 0) |
| `y` | UNKNOWN | trivially; unknown + amount stays unknown |
| `fly` | unknown boolean (like the held buttons) | contains both |

Consequences: "overlaps the player" is undecided wherever the player is in the
fruit's column, and `y < -16` is undecided: collected and deleted (the
fruit-gone shape, plus the refill) are possible on any frame. Coarse and false
refills are expected here and refuted by `r0sxh`.

## At the finer levels (`r0sxh` and up)

The fruit exact. While it waits nothing is unused. Once it flies `step` is dead,
and widening it to UNKNOWN is rule 1-safe; it merges almost nothing by itself
(erasing `step` alone: 1.00x), so this is a nicety, not a fix. The two counters
remain at these levels, inside the marked set: MEASURE there before adding
anything. If the dash cohorts cost too much, two upgrades, both deferred:

- **Flying fruit unknown at the middle levels** (`y := Sel(fly, unknown, y)`,
  flying `spd.y`/`rem.y` to their ranges): removes the cohorts, but a false
  collection becomes possible in its column during flight, on the likely route.
  Risky for the narrow bands of the finer levels.
- **A range merge of flying cohorts at storage** (the door's hull growth): after
  `spd.y` has converged to exactly -3.5 the cohorts differ only in `y` and
  `rem.y`; the `y` range then grows ~1 px per frame until deletion (~20 frames),
  so false collections stay along the stretch the fruit really passes.
  (Widening `spd.y`/`rem.y` while keeping `y` exact is NOT an option: `amount`
  would fork into up to 6 values per frame and the rows multiply.)

## What has to be built

1. **An UNKNOWN number type** that stays unknown under every operation and never
   raises. A full-range interval is not that: interval arithmetic at the 16.16
   extremes raises (Philippe). Unknown + 1, `sin(unknown)`, unknown compared
   with anything (undecided).
2. **Local forks for the fruit.** `move` calls `__split_by_flr` on every object
   before it looks at `solids`; with the fruit's ranges `amount` is one of -4..1
   (6 ways), past the kernels' 2 bits (4 ways) per fork, and a fork splits the
   whole frame's configurations. Wanted: the fruit forks where it is processed
   and merges again at once (every branch ends with `y` unknown and `rem.y` in
   its range, so the branches are one row). Options to understand first: a
   no-split `flr` for an object that does not collide (the tracer cannot see
   `solids` at the split site), a fork scoped to the fruit's `move` whose arms
   rejoin before anything else reads them, or wider fork encodings.
3. **The widening, in the places that must agree**: the tracer's in-graph
   widening (`trace::widen`, the level-0 kernels) and the block model's
   (`Rt2::widen_to` / the ladder boundary), which the Bits(1..15) sets and the
   mark filter's projection use. Same representation in both, or the second
   rung's marks do not match. (The reference engine refuses held-unknown
   levels, so it needs none.)
4. **The level flag `f`**: `abstraction::Level`, `parse`/`Display`,
   `coarser_or_equal`, `grid_consistent`; a fruit dimension in the kernel
   registry (`registry_for`, like `HELD_SLOTS`).
5. **Rule-1 widening of the dead flying `step`** at the finer levels.

## Order and measurements

1. The unknown number type.
2. The fruit widening, tracer and block model.
3. The `f` flag and the registry dimension.
4. Room (3,0) level 0, `r0sxhf` against `r0sxh`: states per frame, cross-frame
   saturation, frame times (the stopped run: 9.0 M states at f50, x1.5 per
   frame).
5. Then the fall floors: without the fruit level 0 still grew x1.49 per frame
   at f55 (the floors' timers and the player's speed), so `r0sxhf` only
   survives with the floors widened too.

## Risks

- **Earlier false wins, more horizons.** There is no reference time for room
  (3,0), so the search counts up from level 0's first win; a false refill near
  the exit will likely make `r0sxhf` win early, and every horizon in between
  runs the ladder until a level refutes it (`r0sxh` should, cheaply).
- **Projection mismatch** between the tracer's and the block model's widening:
  the second rung would drop everything. Check with the gates' method: a small
  horizon, `r0sxhf,r0sxh,rxsx`, the synthetic win must confirm.
