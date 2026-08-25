# Specialized kernels: per-field constants + player position/pm1 pinning

Autonomous session, 2026-08-25. Philippe asked me to build this end to
end while away, making and recording assumptions. This is the record.

## The goal, agreed over several turns

Room (2,0)'s kernels explode to ~900k nodes because a monolithic per-
shape kernel carries an undifferentiated set of lanes and cannot decide
collisions. The worst case: the player's death/exit flag depends on all
8 forks, because the springs bounce the player and the tracer can't
decide `collide(player,spring)` when the player varies per lane. Shape 3
alone emits 3,584 bodies for one death outcome (14 buttons x 2^8 forks).

The fix, as a single framework:

1. **Per-field constant lattice.** Seed from the concrete spawn state.
   As the reachable set grows, each scalar field is either "the same
   constant everywhere" or "abstract". Constant fields compile to
   literals; abstract fields to per-lane inputs in their domain. This
   turns static furniture (spring positions, hitboxes, tile flags) from
   per-lane cells into constants, which is a prerequisite for deciding
   collisions.

2. **Specialized kernels.** On top of the general (base) per-shape
   kernel, compile kernels that additionally pin the player's exact XY
   (and a pm1 key, and later a speed box). With the player pinned, every
   `collide(player,spring)` is decided, the "both springs" branch is
   provably empty, and the fork count drops toward room (1,0)'s 2.

3. **Base + guard, no refinement ladder.** The base kernel stays general
   and correct (it agrees with the interpreter). A specialized kernel
   carries a validity guard (`pin_guard`): a lane whose pinned fields
   disagree is declined and falls to the base kernel - NOT the
   interpreter, so the never-deopt doctrine holds. Sound by construction:
   the base catches whatever the guard rejects, so specialization can be
   as aggressive as we like.

## What already exists (found this session)

The pin machinery is BUILT. `trace::verify::trace_frame` takes
`pin: &[(Path, Conc)]`; `iface::symbolize` folds each pinned field to its
constant; `iface::pin_guard` produces the `ok` obligation that every
pinned cell equals its value. This is how the (now-deleted) pm1 class
kernels were specialized. `shapes::walk` calls `trace_frame` with an
EMPTY pin list - so the room kernels are the fully-general base. Adding
position/pm1 pins is passing that list through.

So this is less "build a specializer" and more "route pins into the room
walk, pick the pin values, and measure."

## Assumptions made (revisit later)

- **A1. Speed range deferred.** The ±8 speed box is not a constant pin
  and needs interval support with a specific range (the pin API pins to
  exact `Conc`, and `ival` only carries the rem [-0.5,0.5) range). I do
  position + pm1 + spring-constants FIRST (the dominant win), then add a
  bounded-interval pin for speed. Rationale: with position pinned,
  `collide(player,spring)` already decides for every spring the player is
  NOT on, killing the product; only the one spring the player IS on keeps
  a genuine `spd.y>=0` branch, which speed-range would not remove anyway.
- **A2. Pin the springs too.** To decide `collide(player,spring)` both
  sides must be concrete. Spring positions are room-constant (the spring
  update never writes its own x/y - verified in the Lua), so pinning them
  is sound for every lane in the shape. I pin them to their room-map
  values. Longer term this is the "static furniture -> constant" lattice
  doing it automatically; for the experiment I pin explicitly.
- **A3. Pin values from a real reachable state where possible.** I pull
  spring positions from room (2,0)'s map and a player position from a
  reachable state (checkpoint if available, else a concrete walk). If no
  real state is handy, I use plausible values and record it - the graph-
  SIZE collapse is nearly invariant to the exact values, only WHICH
  branch survives changes.
- **A4. Experiment behind a flag / probe, not in the checked-in path.**
  I add a `transpile` probe subcommand (prints text, builds under quick)
  rather than touching the generated kernels, until the shape is
  understood and Philippe has seen it.

## Plan of work

1. Probe: trace one room-(2,0) shape with position+pm1 pins, report node
   count, per-outcome bodies, fork count. (this file's first result)
2. Iterate: read every surviving node; confirm the spring branches are
   gone and the graph is ~room-(1,0) size. Understand anything weird.
3. Static-constant detection: the per-field lattice over the reachable
   set - which fields are constant everywhere. Feed those as automatic
   pins so the base kernel bakes them in.
4. Speed box (A1 follow-up).
5. Write up numbers and the design; leave the checked-in kernels alone
   unless a clear win is proven and gated.

## Results

(filled in as I go)

## Result 1: position does NOT simplify the graph (2026-08-25)

Built a probe (`transpile --spec-probe SHAPE`, env `CELESTE_SPEC_*`) that
re-traces one room-(2,0) shape with fields pinned and reports reachable
nodes + live forks. Room (2,0) shape 3 (the 190k-node monster, 14
objects, 1 player + 2 springs):

| pins | nodes | live forks |
|---|---|---|
| base (nothing) | 15,425 | 8 |
| player XY + springs XY + pm1 | 15,104 | 8 |
| + player `spd.x/spd.y` = 0 | 13,378 | 4 |
| ALL player scalars except `rem` | **2,597** | **0** |

(Node counts are the raw traced-frame cone, pre button/fork
specialization - much smaller than the 190k lowered kernel, but the
RATIOS are what matter.)

**Position specialization buys almost nothing** (15,425 -> 15,104). The
graph's size and its forks are driven by the player's DYNAMIC state -
velocity above all. This contradicts the position-first plan we agreed on
over the previous turns; recording it as the headline result so we can
re-decide. The lever that works is the full player scalar state (down to
2,597 nodes, no forks), which is not enumerable the way position is.

### Where the 8 forks come from

Every fork is `floor(0.5 + rem + spd_effective)` - the player's pixel
move. `spd_effective` is a `Sel` chain:

* 4 forks vanish when input `spd` is pinned: the player arriving with
  different velocities.
* The other 4 are the SPRING BOUNCE. `spd.y = Sel(Gt(objects[2].hide_for,0),
  Sel(Gt(objects[1].hide_for,0), 0, -3), -3)`: the springs set the
  player's `spd.y = -3` gated ONLY on each spring's `hide_for` (its
  hidden-timer), NOT on collision.

### The suspicious part: collide folds to "always colliding"

The bounce should be gated on `collide(player,spring)` too. It is not -
pinning the player and springs to positions 100+ px apart leaves the
`-3` bounce fully present, gated only on `hide_for`. So `collide` is
being decided TRUE (or dropped) regardless of position. Either the
tracer over-approximates `collide` to "the player is always on every
spring" - a real spurious-coupling BUG that would inflate room (2,0) on
its own - or the pins are not reaching the collide box test. This is the
next thing to pin down, and it matters independently of the
specialization strategy: if the player is being coupled to every spring
unconditionally, that is wrong, not just expensive.

### Strategy implication (for discussion)

If position does not help and velocity is the lever, the specialization
axis is a player MODE (velocity band + dash/jump state), not a position.
That is harder - velocity is not a small enumerable set the way reachable
positions are. But FIRST resolve the collide question: a large part of
the blow-up may be the spurious spring coupling, which is a correctness
bug to fix rather than an abstraction to specialize around.

## Result 2: the spring bounce is UNCONDITIONAL - a collide bug (2026-08-25)

Chased the position-blindness to a definite conclusion. Traced room
(2,0) shape 3 with the player position SYMBOLIC and everything else
concrete (spd=0, spring spr=18/hide_for=0/active, hitboxes at their
type values). Result: the player's `spd.y` becomes **-3 (the spring
bounce, line 307) as a CONSTANT** - not gated on the player position,
not gated on the collide box test, not gated on anything but the
spring being active.

Tested across player positions (10,20), (40,40), (60,40), (1000,1000):
byte-identical bounce. Tested with hitboxes pinned to their real values
(player {1,3,6,5}, spring {0,0,8,8}): still unconditional. So
`this.collide(player,0,0)` in `spring.update` returns the player
**regardless of position** - the box-overlap test is not being
evaluated against the player's coordinates at all.

This is a real bug/over-approximation in the tracer's `collide`, and it
is significant on two fronts:

* **Correctness.** A spring bounces the player every frame it is active,
  even when the player is nowhere near it. Room (2,0)'s traced kernels
  are not differentially tested at depth (only room (1,0) is), so this
  has gone uncaught. If the concrete oracle shares the bug it is a
  tracer bug; if only the symbolic path has it, it is a domain bug.
* **Size.** This spurious coupling is a big part of why room (2,0)
  explodes: the player's move is forked/duplicated across every active
  spring's (nonexistent) bounce, and the death/exit flags inherit the
  coupling. Fixing collide to actually read positions likely collapses
  much of the room-(2,0) blow-up on its own - BEFORE any specialization.

### This reframes the whole task

The room-(2,0) node explosion is not (mainly) an abstraction-granularity
problem to solve with position specialization. A large part of it is a
COLLIDE BUG. The order of work should be:
1. Root-cause and fix `collide` (find why it ignores position).
2. Re-measure room (2,0) - the explosion may largely vanish.
3. THEN decide whether specialization is still needed, and on what axis.

Position specialization was the wrong first lever (Result 1); the collide
bug is the real one.

## Result 2, CORRECTED: collide gates on collideable, but the box test does not fold

Refined the "unconditional bounce" claim - it was overstated. Pinning
the player's `collideable = false` removes the bounce entirely (shape 3
drops to 3,071 nodes, 0 forks). So `collide` DOES respect `collideable`
correctly; it is not returning the player unconditionally.

The narrower, real problem: the **box-overlap comparison does not fold to
false** when the player is pinned far from the spring. Pinning the player
at (1000,1000) with springs active at (60,40) still leaves the `spd.y=-3`
bounce present (as a branch/fork, gated on `collideable AND boxtest`,
whose `boxtest` never decides false). A concrete AABB test of
non-overlapping boxes should decide false and prune the branch; it does
not. There IS a small position dependence in the node count (9,982 at
(1000,1000) vs 10,319 at valid in-room positions), so the position is not
wholly ignored - but the specific box comparison that should prune the
bounce is not folding.

So: not a blatant "bounce everyone" bug, but a box-test-folding gap that
keeps every active spring's bounce branch live for every player,
inflating the fork/branch count. Root-causing the exact reason the AABB
comparison stays symbolic needs a read of the interp's `collide` loop /
comparison lowering that I have not finished; time-boxing it here and
recording the reproduction (`transpile --spec-probe 3` with
`CELESTE_SPEC_NOPOS`, `CELESTE_SPEC_SFIELDS`, `CELESTE_SPEC_PFIELDS`).

### Net so far

1. Position specialization does not shrink the graph (Result 1).
2. The player's dynamic state (spd + collideable + dash state) is the
   lever; pinning it all -> 2,597-3,071 nodes / 0 forks.
3. The spring bounce keeps a branch alive per active spring per player
   because the box test does not fold - a real inefficiency, possibly a
   correctness-relevant over-approximation, worth a proper interp fix and
   independent of specialization.

Next: (a) leave the collide box-fold for a focused interp session; (b)
build the per-field constant-across-reachable-states measurement (the
other half of the ask), since that is concrete and tells us which fields
(spring position, hitbox, tile flags) SHOULD be constants and are being
carried per-lane.
