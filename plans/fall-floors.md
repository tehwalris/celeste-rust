# The fall floors: unknown at the coarse levels (DISCUSSION, 2026-09-17, not started)

Room (3,0) has 12 fall floors. Without the fly fruit, level 0 still grew x1.49
per frame at f55, and erasing the floors merged 3.13x there (plans/room30.md).
This records the field-by-field analysis and where the discussion landed; the
mechanism it needs is shared with plans/fly-fruit.md.

## Fields

A floor is an object (`init_object`) plus `fall_floor.init` (`state = 0`,
`solid = true`); `break_fall_floor` adds `delay`. Checked in the cart: the
player's collisions go `is_solid` -> `check(fall_floor)` -> `collide`, which
reads the other object's `type`, `collideable`, `x`, `y`, `hitbox`; the floor's
`solid` field is written and never read; the floor has no `draw`.

| field | values | read by | most it could be widened |
|---|---|---|---|
| `type`, `x`, `y`, `hitbox` | constant | every `collide` | nothing to gain (constant; unknown would make every collision undecided) |
| `spr`, `flip`, `solid` | constant | nobody | nothing to gain |
| `solids`, `spd`, `rem` | constant (true, 0) | the floor's own `move`, which never moves it | none needed |
| `collideable` | true; false while hidden (state 2) | the player's `is_solid`, every pixel step | unknown boolean, FORKED where read (below) |
| `state` | 0 idle, 1 shaking, 2 hidden | only its own `update` and `break_fall_floor` | unknown |
| `delay` | 15 -> 0 shaking, 60 -> 0 and below hidden, 0 by the "missing" hack | only its own `delay - 1`, `delay <= 0` | unknown (a checked range such as [1, 15] is a fixed point but no more precise, since the transition is undecided anyway) |

Measured (no-fruit experiment, f55, erasing across all 12 floors): `delay`
2.09x, `state` alone 1.00x, `collideable` alone 1.00x, all three 3.13x.

## Where it landed (Philippe)

At the coarse levels all three are unknown: every floor is just "maybe solid",
nothing unique per floor, no row differs by floor timing. With the region key
(`CELESTE_REGION`), a floor outside the region's box plus the player's reach
folds away in the range analysis (its overlap with the player, and its own
`check(player, ...)`), so only the one or two floors near a region stay
undecided and the forks stay bounded.

## The two mechanical problems

1. **The player's collisions.** An unknown `collideable` makes the move loop's
   "stop or step" undecided; the tracer would merge the arms under a
   `Known(cond)` premise and the kernel would refuse every such row. Fix, as the
   held buttons: fork each nearby floor's `collideable` input into both values at
   frame start (`SplitInt`), so each configuration's collisions are exact and the
   player's outcomes are separate rows (x2 per nearby floor). Unreachable forks
   (far floors) add no configurations.
2. **The floor's own update.** With `state`/`delay` unknown its branches are
   undecided and merge under the same premise. Everything it computes is
   overwritten by the widening at the frame's end, and in room (3,0) it has no
   other effect (it reads the player, writes only itself; no spring), so the
   refusal protects dead values. Options: fork its branches too (3 x 2 per floor,
   x36 for two floors: too many); a fork where the floor is processed that
   rejoins at once (the mechanism plans/fly-fruit.md needs); or attach a merge's
   `Known` premise only where the merged value is still read.

## Precision

"Maybe solid on any frame" lets the player jump up through an unbroken floor
from below and stand on a gone one: faster false climbs, an earlier first win at
level 0, more horizons. The same two-rungs-at-`r0` pattern as the fruit: a
coarse `r0` with the floors (and fruit) unknown, then an `r0` with exact floors
filtered by its marks.

## Order

1. The unknown number type (plans/fly-fruit.md, being built).
2. The local fork-and-rejoin (or the premise change), shared with the fruit.
3. The floors' widening on top; `collideable` forked where read.
4. Measure room (3,0) level 0 with floors and fruit unknown.

## Removing the `delay` hack (`widen::ABSENT_AS_ZERO`)

Today every frame writes a missing `delay` as the number 0, at every level
(de24a0e; `cart::check_absent_fields` refuses a cart where the field is read
other than by arithmetic or an ordering comparison, both runtime errors on nil).
It exists because a floor gains `delay` only when it first breaks, which made
"which floors have ever broken" 12 bits of the heap shape. Philippe: a hack,
TODO to remove.

- **Coarse levels**: once the floors' `delay` is unknown, a missing `delay` can
  be written as that unknown instead of 0 - within the rule "only replace a
  value by something containing it" only if the unknown value covers nil too
  ("any value", not just any number). The unknown number type
  (plans/fly-fruit.md) should be built with that property in mind.
- **Exact levels**: `delay` stays exact, so a never-broken floor still has no
  field; without the hack the exact levels are back to 2^12 shape variants, each
  with its kernels. Replacement: a real nil-or-number field (a per-row tag for
  "present"), which the kernels read with a premise where the cart does
  arithmetic on it.

Then `ABSENT_AS_ZERO`, `materialize_absent_fields` and `check_absent_fields` go.
