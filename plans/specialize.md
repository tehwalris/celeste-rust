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
