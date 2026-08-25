# Specialized kernels: per-field constants + player position/pm1 pinning

Autonomous session, 2026-08-25. Philippe asked me to build this end to
end while away, making and recording assumptions. This is the record.

## EXECUTIVE SUMMARY (read this first)

I built a probe (`transpile --spec-probe SHAPE`, env `CELESTE_SPEC_*`)
and measured, on room (2,0) shape 3 (the 190k-node monster), what
collapses the traced graph. **The headline: none of the enumerable
specialization axes we agreed on help.** Position, all-object geometry,
the pm1 key, `collideable`, and resolving any button assignment ALL leave
the graph at ~15k traced nodes and its full 8-way fork. Only pinning the
player's continuous velocity/dynamic-state collapses it - and that is not
an enumerable axis. The emitted 2^8-fork x ~145-button explosion reflects
a REAL 8-way fork inherent to a 14-object, velocity-widened frame, not an
over-enumeration.

So: **position/pm1/geometry specialization is refuted as a room-(2,0)
size lever; do not build it for that purpose.** The session's value is
this negative result (it saves building the wrong thing) plus the probe
tool. I also chased and RETRACTED a "collide bug" and a "SUM-not-product
16x" idea - both wrong, both documented below with their refutations so
the trail is honest.

Two wrong turns I want to flag honestly: I first hypothesised a collide
closure-capture bug (Result 2/3) and then a fork over-enumeration
(SUM-not-product) - both refuted by further measurement in the same
session. The final table (in "TRULY FINAL conclusion") is the trustworthy
part.

Remaining honest options for room (2,0): accept it via an
interpret-the-graph base backend (the base need not be fast, only
correct), a velocity abstraction (hard), or interpreter-only until one is
worth it. Nothing in the checked-in kernels or production path was
changed; the probe is a flag-gated diagnostic.

---


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

## Result 3: the box test is position-INDEPENDENT-true - a closure-capture bug (leading hypothesis)

Dumped the symbolic comparisons in a fully-concrete trace: the collide
box test is NOT among them - it FOLDED. But the bounce is still present,
so it folded to TRUE. And it folds to true INDEPENDENT of both positions:

* player at (10,20), (64,80), (1000,1000): identical -3 bounce.
* springs at (60,40), (200,100): identical.

A correct AABB test `other.x + hb.w > obj.x` depends on both positions.
One that is true regardless of both is a **self-comparison**: `x + w > x`
(w>0, always true). That happens when `other` and `obj` are the SAME
object - i.e. `spring.collide(player)`'s captured `obj` is not the
spring.

### Leading hypothesis: closure `obj` capture corrupted by blank/GC/renumber

`obj.collide` is a closure created in `init_object`, capturing `obj`
(that object's table). The shape walk BLANKS state and GCs/renumbers
tables between frames (`shapes::walk` -> `blank`; the boundary
canonicalizes ids). If a closure's captured `obj` reference is not
remapped through a renumber - or blank rebinds it - then deep in a room
`spring.collide`'s `obj` points at the wrong table (plausibly the player,
or a shared one), the box test degenerates to `x+w > x` = always true,
and every active spring bounces every player.

This is consistent with room (1,0) being correct (differentially tested,
shallow, few objects) while room (2,0) - reached after a long walk with
many GC/renumber cycles and 14 objects - is wrong. It would also explain
the node explosion broadly: `is_solid`/`check` also go through `collide`,
so the player would spuriously collide with everything.

### What would confirm it

Trace `spring.collide` and check whether the captured `obj` table id
equals the spring's table id or is aliased. Or: trace the SPAWN frame
(no blank/GC yet) and check whether collide there is position-dependent
(if spawn is correct but a deep shape is not, the corruption is the
walk's blank/GC). This needs interp instrumentation, a focused session.

### If confirmed, this is the room-(2,0) fix

Not specialization - a closure-capture correctness fix. Room (2,0)'s
900k-node blowup would largely dissolve, because the spurious
spring/solid coupling is a big multiplier. Specialization (position/pm1)
was the wrong lever (Result 1); this is the right one, and it is a BUG,
not an abstraction choice.

## Handoff (autonomous session end)

Built: `transpile --spec-probe SHAPE` + `CELESTE_SPEC_*` envs, a probe
that re-traces one shape with fields pinned and reports nodes/forks (in
`trace::kernel::specialize_probe`). It is a diagnostic, not wired into
anything; kept for chasing this down. Nothing in the checked-in kernels
or production path changed.

Findings, in order of importance:
1. **collide box test is position-independent-true in room (2,0)** -
   almost certainly a closure-capture bug from the walk's blank/GC.
   This is the likely root cause of the room-(2,0) explosion AND a
   correctness issue (room 2 is not differentially tested at depth).
   FIX THIS FIRST, then re-measure - the explosion may vanish.
2. **Position specialization does not shrink the graph** (15,425 ->
   15,104). If specialization is needed after the bug fix, the lever is
   the player's dynamic state, not position.
3. The pin machinery (`trace_frame` `pin` + `pin_guard`) is the ready-
   made base+specialized mechanism if we still want it later.

Assumptions A1-A4 from the top still hold; A2 ("pin springs to decide
collide") turned out irrelevant because collide ignores the pinned
positions - which is how the bug was found.

## Result 3 RETRACTED, and the honest state (2026-08-25)

The self-comparison / closure-aliasing hypothesis is WRONG. With the
player position symbolic, the collide box test IS present and DOES
reference the player position:

    Gt( player.x + player.hb.x + player.hb.w ,  objects[0].x + objects[0].hb.x + ox )
    = Gt( Cell(87) + 1 + 6 , Cell(29) + Cell(19) + ... )

So collide compares real positions. What I got wrong: I pinned the TWO
springs (objects[1], objects[2], found by type) and the player, but the
box test that keeps the bounce alive references **objects[0]** - which I
never pinned. objects[0] is another object (its `.hitbox`/`.x` are slots
17-30); the frame has 14 objects and I only pinned 3 of them.

So the bounce branch stays live simply because the shape has MANY
objects and the player's collision cone touches objects I did not pin,
not because of any aliasing bug. Pinning 3 of 14 objects was never going
to fold the collision graph.

### Corrected conclusion

There is (probably) no collide bug. The room-(2,0) complexity is what it
looks like: a 14-object frame where the player can interact (bounce,
collide, stand on) with many objects, and each interaction is a branch.
The graph is large because the SITUATION is genuinely 14 objects, most
per-lane.

This puts us back on the ORIGINAL framing, and Result 1 stands as the
real finding: **pinning the player alone (position OR full state) does
not shrink the graph, because the OTHER 13 objects are the bulk.** The
2,597-node floor came from pinning ALL player scalars, which removes the
player's contribution - but the other objects remain per-lane and are
most of the frame.

### What this means for the strategy

To actually shrink room (2,0), the constant-lattice idea is the right
one AFTER ALL - but applied to ALL objects, not just the player: the
static furniture (spring positions, hitboxes, fall_floor/fake_wall
positions, tile flags) is constant across reachable states and should be
BAKED IN, which folds the many collision box tests. That is the
"per-field constant across reachable states" measurement I deferred, and
it is now clearly the highest-value next step - it attacks the 13 objects
the player-pin experiments left untouched.

The specialization-by-player-state lever is real but secondary; the
constant-baking of the whole object set is primary. Neither is position-
specialization, which remains refuted (Result 1).

## Corrected handoff

Solid findings:
1. **Position specialization does not shrink the room-(2,0) graph.**
   (Result 1, unretracted.) The player is a small part of a 14-object
   frame.
2. **The lever is baking in per-field constants across ALL objects**
   (spring/furniture positions, hitboxes, tile flags), which folds the
   collision box tests. This is the constant-lattice idea, applied to
   the whole object set, not just the player. NEXT STEP.
3. No collide bug (Result 3 retracted). The graph size reflects a
   genuinely 14-object situation carried per-lane.

Tooling left in place: `transpile --spec-probe` + `CELESTE_SPEC_*` (in
`trace::kernel::specialize_probe`), a diagnostic for pinning fields and
measuring the graph. Not wired into production. The three earlier
Result sections above are kept with this retraction so the reasoning
trail (and the two wrong turns) is visible.

## DEFINITIVE SUMMARY (autonomous session, 2026-08-25)

Exhaustively probed what specialization collapses room (2,0) shape 3's
traced graph. Full table (raw traced-frame cone / live forks):

| pins | nodes | forks |
|---|---|---|
| base | 15,425 | 8 |
| player XY + pm1 | 15,104 | 8 |
| ALL object positions + hitboxes | 15,115 | 8 |
| player collideable = true (its real value) | 15,080 | 8 |
| player spd = 0 | 13,378 | 4 |
| player collideable = false | 3,071 | 0 |
| ALL player scalars except rem | 2,597 | 0 |

### What is and isn't the lever

* **NOT position** (player or all objects): no change. Refutes the whole
  position-specialization plan.
* **NOT geometry** (all hitboxes + positions): no change. Refutes the
  "bake in furniture, fold collisions" idea too - the collisions are not
  what dominates.
* **NOT collideable-as-constant**: pinning it true (its real value) does
  nothing; only false helps, and only because false disables all
  collision (an unreal state).
* **The forks are the player's MOVE**, `floor(0.5 + rem + spd_eff)`,
  forked once per distinct conditional form of `spd_eff`. Pinning `spd`
  removes the input-velocity forms (8 -> 4 forks); the residual 4 are the
  conditional spring-bounce `spd.y = -3`.
* **Only pinning the player's whole dynamic state collapses it** (to
  2,597 nodes / 0 forks), which is not an enumerable specialization axis.

### The honest conclusion

Room (2,0)'s per-shape graph is INHERENTLY complex. Its ~15k traced
nodes are the player's velocity/dash/jump physics (Sel chains over spd,
buttons, and object interactions), and its 8 forks are the move forked
over the conditional forms of the player's velocity. None of the cheap,
enumerable specialization axes we hypothesized (position, pm1, geometry)
touch it. The emitted-kernel explosion (190k nodes = 2^8 forks x ~145
buttons over the cone) is driven by the FORK COUNT, and the only thing
that reduces the fork count is pinning the player's velocity - which is
continuous, not a small enumerable set.

### Where this leaves the room-(2,0) size problem

1. **Position/pm1/geometry specialization is refuted as a size lever.**
   Do not pursue it for room (2,0).
2. **The fork count is the emitted-size driver** (2^forks). Velocity is
   what sets it. If room (2,0) is to shrink, it is via reducing the fork
   multiplicity - either by not enumerating independent forks as a
   product (the earlier "SUM not product" idea, still valid and NOT yet
   built), or by a velocity abstraction, which is hard.
3. **The "SUM not product" emitter change is the most promising untried
   lever** - it attacks the 2^forks enumeration directly, independent of
   any state specialization, and Result 1-3's experiments do not touch
   it. That is where I would go next.

Net for Philippe: the specialization experiments came back negative for
the axes we picked (a real result - saves us building the wrong thing),
and they point back at the emitter's fork-product enumeration as the
actual size lever. The probe (`transpile --spec-probe`) stays as the
tool that established this.

## The fork structure, and the concrete "SUM not product" lever (2026-08-25)

The 8 forks are not 8 independent choices. They pair by axis (forks
0,2,4,6 are x-moves; 1,3,5,7 are y-moves), and the 4 pairs are the SAME
player move `floor(0.5 + rem + spd_eff)` under 4 different conditional
forms of `spd_eff` (dash / jump / spring-bounce / normal). fork_memo does
not share them because the operand includes `spd_eff`, which differs.

Crucially, a single lane takes ONE spd condition, so it is live in only
ONE pair (its x and y) - the other 6 forks belong to lanes in other
branches. The emitter, however, enumerates 2^8 = 256 fork configs as if
all 8 were simultaneously live. If the 4 spd-condition branches are
mutually exclusive (a frame either dashes, jumps, bounces, or does
neither), the honest count is ~4 branches x 2^2 = 16 configs - a **16x
over-enumeration**.

This is the concrete form of the "SUM not product" idea, and it is the
single most promising size lever found this session:

* It attacks the 2^forks emitted-size explosion directly (the thing that
  makes room 2 190k+ nodes).
* It needs no state specialization - it is an EMITTER change: recognise
  that forks born in mutually-exclusive branches do not combine into a
  product, and enumerate per branch (a sum) instead.
* NOT YET VERIFIED that the 4 branches are mutually exclusive - that is
  the thing to check first (trace the 4 fork-pairs' branch guards and
  test pairwise `guard_i AND guard_j == false`). If they are, the
  emitter's fork enumeration is provably over-counting and the fix is
  well-defined.

This supersedes position/pm1/geometry specialization (all refuted) as the
room-(2,0) direction. It is also consistent with room (1,0) being fine:
room 1's player has fewer conditional spd forms, so fewer fork pairs.

## The SUM-not-product claim REFUTED too (2026-08-25)

Tested it directly: for each of the 64 button assignments, resolve the
buttons and count live forks. Result: `{8 forks: all 64 assignments}`.
Every button leaves all 8 forks live. So the 8 forks are NOT a per-button
artifact and do NOT collapse to 2 per config - the "mutually exclusive
spd-condition branches / 16x over-enumeration" idea is wrong. Retracting
it.

The 8 forks are genuinely, simultaneously live. Only pinning the player's
velocity (`spd`) reduces them (8 -> 4), and velocity is continuous.

## TRULY FINAL conclusion for room (2,0) shape 3

Everything tried, and its effect on the graph:

| lever | forks | verdict |
|---|---|---|
| player position | 8 | no effect |
| all object geometry | 8 | no effect |
| pm1 key | 8 | no effect |
| collideable = true | 8 | no effect |
| resolve any button | 8 | no effect |
| player spd = 0 | 4 | halves - but velocity is continuous |
| all player dynamic state | 0 | not enumerable |

**No enumerable specialization axis collapses room (2,0) shape 3's
graph.** The 8-way fork and ~15k-node cone are inherent to a 14-object
frame with a velocity-widened player. The emitted 2^8 x ~145-button
explosion reflects a real 8-way fork, not an over-enumeration.

This is a comprehensive NEGATIVE result, and it is the useful kind: it
rules out position, pm1, geometry, and the SUM-not-product emitter change
as room-(2,0) size levers, so none of them should be built for that
purpose. The remaining honest options for room (2,0) are:
1. Accept it (interpret-the-graph backend, don't compile to Rust - the
   base kernel does not have to be fast, only correct), OR
2. A velocity abstraction (hard, continuous), OR
3. Live with room (2,0) at interpreter-only until 1 or 2 is worth it.

The session's concrete deliverable is this map of what does and does not
work, plus the `transpile --spec-probe` tool that produced it. The
strategy conversation that motivated position-specialization is answered:
it would not have helped.

## Confirmed across all three big shapes (2026-08-25)

The negative result is not shape-3-specific. Same probe, shapes 8 and 4:

| shape | base + pm1 | player fully pinned |
|---|---|---|
| 3 | 15,104 nodes, 8 forks | 2,597, 0 |
| 8 | 15,110 nodes, 8 forks | 2,601, 0 |
| 4 |  9,261 nodes, 8 forks | 2,027, 0 |

Identical pattern: the pm1/position pins do nothing; only pinning the
player's full dynamic state collapses the graph and kills all forks. The
8-way fork is a property of every big (14-object) shape in room (2,0),
driven by the player's velocity-dependent physics, and none of the
enumerable axes touch it. Conclusion stands and generalises.

## Why room 1 forks 2x and room 2 forks 8x (the remaining mechanism note)

Room (1,0) shape 1 (player alone) forks twice: `rem.x`, `rem.y`. Room
(2,0)'s big shapes fork eight times. Pinning the player's `spd` takes
room 2 from 8 to 4 forks, so 4 of the 6 extra forks are velocity-
dependent branches the room-1 player does not have (spring bounces set
`spd.y=-3` conditionally; velocity-sign branches in the physics). The
extra forks are the player interacting with a busier room, and they are
live for every button and every object geometry - i.e. genuinely per-
lane-velocity, not a specialization artifact. This is why the fork count,
and thus the emitted 2^fork size, is inherent to the room's complexity
rather than removable by pinning static facts.

## Probe validated against room (1,0) (2026-08-25)

Sanity-checked the probe on room (1,0), which we understand:

| shape | nodes | forks | is |
|---|---|---|---|
| 0 | 1,973 | 0 | spawn/exit state |
| 1 | 2,089 | **2** | the player (rem.x, rem.y) - exactly the known 2 forks |
| 2 | 78 | 0 | respawn animation |

The probe reproduces room (1,0)'s known 2-fork player shape and its tiny
sizes, so its room-(2,0) numbers (8 forks, ~15k nodes, no specialization
axis helps) are trustworthy. Room 2 shape 3 is genuinely 7x room 1's
node count and 4x its fork count - a real difference, the player in a
14-object room vs alone.

## SESSION END

Investigation complete and validated. Deliverables:
* `transpile --spec-probe SHAPE` + `CELESTE_SPEC_*` - a validated probe
  for pinning fields and measuring the traced graph (flag-gated, not in
  production).
* A comprehensive, honest map (this file) of what does and does not
  collapse room (2,0)'s graph: NOTHING enumerable does. Position, pm1,
  geometry, collideable, buttons - all no effect; only continuous
  velocity, which is not enumerable.
* Two hypotheses raised and retracted with their refutations (collide
  bug; SUM-not-product) - the trail is kept honest.

Recommendation for Philippe: the position/pm1/velocity specialization
direction we discussed will not shrink room (2,0). The realistic paths
are (a) an interpret-the-graph base backend so the big kernel need not
compile to Rust, or (b) room (2,0) interpreter-only for now. Both are
strategy calls for you; I did not build either, since I had just refuted
the stated direction and did not want to build on an unconfirmed pivot.
Nothing in production changed; 280 tests green; tree clean.

## The complete fork mechanism (the "understand the nodes" deliverable)

`obj.move(ox,oy)` calls `__split_by_flr(rem.x)` and `__split_by_flr(rem.y)`
- exactly 2 forks per move call, on `rem.x + ox + 0.5` and
`rem.y + oy + 0.5`. So room (1,0)'s player = 2 forks. Room (2,0)'s 8 =
the player's move traced in 4 distinct velocity contexts x 2 axes.

The 4 velocity contexts come from, in order of how I peeled them:
* **input velocity** (`spd`): pinning it 8 -> 4 forks. The player arrives
  with different velocities; velocity-sign branches in the physics.
* **spring STATE** (`spr`, `hide_for`): the remaining 4. Even with ALL
  object geometry concrete (positions + hitboxes), the forks stay at 8 -
  because the bounce is gated on whether each spring is ACTIVE
  (`spr==18`, `hide_for<=0`), which is per-lane (a spring gets hit and
  hides). Pinning spring `spr`/`hide_for` (+ spd) -> 2 forks.

So the 8 forks decompose as: `rem` (2, real) x input-velocity-branch (2)
x spring-active-state (2) = 8. Every factor is a genuine per-lane
distinction:
* input velocity varies per lane (continuous),
* each spring is independently active-or-hidden per lane.

None is a static constant or a decidable-by-position fact. The
"both springs bounce is impossible" mutual exclusivity does NOT help,
because the branching is on spring STATE (both springs CAN be active at
once - the player just is not on both), not on simultaneous collision.

This is why nothing enumerable collapses it: the forks encode real,
independent, per-lane game state (velocity + two springs' activity), and
the emitted 2^8 is the honest cross product of those. It is large because
room (2,0), mid-play, genuinely has that many live independent per-lane
degrees of freedom around the player. Room (1,0) has none of them.

### Final answer to "iterate until the graph is as simple as we expect"

It does not get simpler by specialization, and now we know WHY at the
node level: the fork count is `2 (rem) x 2 (velocity) x 2 (each active
spring)`, all genuine per-lane. The graph is as simple as it is going to
be for a 14-object mid-play frame. The size lever, if any, is not
specialization but a different BACKEND for the base kernel (interpret the
graph rather than compile 2^8 configs to Rust), or accepting room (2,0)
on the interpreter until then.

## RE-OPENED: Philippe was right - collide has a real position-blindness bug (2026-08-25)

Philippe disagreed with the "inherent" conclusion, correctly. His model:
at runtime x/y/spd are concrete per-lane, only rem is widened, so the
ONLY genuine forks are the 2 rem forks. A far player at a known velocity
cannot be bounced, so room (2,0) far from a spring should fork like room
(1,0). Two-step plan he wants: (1) compile with concrete position+speed
-> only rem forks, fix this first; (2) later, compile with a compile-time
INTERVAL assumption on speed (still concrete at runtime) -> should drop
the same spring forks.

Chased step 1's failure to root. CONFIRMED it is a bug, not inherent:

* `Graph::fold` does NOT constant-fold arithmetic (Add/Sub/Flr) or
  comparisons - they fall to `_ => {}`. `decide` only recognises literal
  `ConstBool`. BUT `Symbolic::arith` and `Symbolic::compare` DO fold when
  both operands are exact `Const` (`as_p8` requires lo==hi). So a box test
  of exact-constant positions SHOULD fold.
* Yet with the player pinned at (40,40) and the springs pinned FAR at
  (999,40)/(888,40), hitbox+spd+collideable all concrete, the spring
  bounce `spd.y=-3` still fires - 4 forks, gated only on the springs'
  `hide_for`, with NO position term. The collide box test folded to TRUE
  for a player 950px from the spring.
* `Gt(47, 999)` folds to false correctly (verified in the code path). So
  the box test's spring-position operand is NOT the pinned (999) value -
  the bounce's `collide` reads the spring position through a path the pin
  on `objects[1].x` does not reach. (A box test that DID use the pin,
  `player.x vs 999`, appears elsewhere with the player symbolic - so SOME
  collide uses the live cell, but the bounce's does not.)

So: the spring's `collide(player)` that drives the bounce is comparing
against a spring position that is not the live, pinnable `objects[k].x` -
a closure/object-identity or stale-read bug in the object-collide path.
Fixing it makes a concretely-far player prune the bounce, which is
exactly step 1. This RETRACTS the "no lever / inherent" conclusion: the
lever is fixing collide, and Philippe's prior ("it's not real") was
right.

### Next concrete step

Isolate in a minimal 2-object (player + spring) trace: call the spring's
`collide(player)` at known positions and confirm whether its `obj` (the
captured self) reads the live spring table or a stale/other one. The
`--spec-probe` evidence points at the bounce's collide obj not being the
pinned objects[k]; a unit-level trace will name the exact divergence.

## RESOLVED: the spring forks are spurious, from unknown spring SPEED (2026-08-25)

Root cause found and confirmed. The frame's per-object `foreach` does
`if obj.spd.x~=0 or obj.spd.y~=0 then obj.move(...)`. The SPRINGS' `spd`
is symbolic in the base kernel (unpinned), so the tracer cannot decide
`spring.spd~=0` and must trace the branch where the spring MOVES - which
makes the spring's own position symbolic. The spring's `collide(player)`
then compares the player against a SYMBOLIC spring position, so the box
test never folds, and the bounce cannot be pruned even for a concretely
far player. That is why pinning the spring POSITION did nothing: the
position it reads is `pinned_x + spurious_move`, not `pinned_x`.

Confirmed by instrumenting `compare`: the box test logged as
`Gt(47, symbolic)` - player side exact (40+7), spring side symbolic.

### The fix, and it validates Philippe's whole model

Pin the springs' `spd` to 0 (they are static - they never move) and a
concretely-far player's bounce prunes completely. With EVERYTHING
concrete except the player's rem, and the player moving (spd=2) far from
the springs:

  **2 live forks** - exactly `floor(0.5 + 2 + rem.x)` and
  `floor(0.5 + 2 + rem.y)`. Zero spring forks. 2,678 nodes vs 15,425.

So Philippe was right on every point: at runtime x/y/spd are concrete,
only rem is unknown, and the only genuine forks are the 2 rem forks.
There is NO inherent complexity and NO deep collide bug - the spring
forks were spurious, caused by the compiler not knowing the springs'
speed.

### Corrected strategy (this supersedes the whole negative-result arc)

1. **Constant-lattice, applied to STATIC FURNITURE'S SPEED, not just
   position.** Springs (and other static objects) have `spd = {0,0}`
   constant across all reachable states, and a constant position. Bake
   BOTH in. I dismissed the constant-lattice earlier ("geometry pinning
   did nothing") because I pinned spring POSITION but not spring SPEED -
   and the spurious move from symbolic speed defeated the position pin.
   Speed is the load-bearing constant.
2. **Then position-specialize the player** - with the springs baked to
   constants, a concrete player position folds the collide -> 2 rem
   forks (step 1).
3. **Interval speed (step 2)** drops the same forks with a compile-time
   speed range instead of a concrete value.

The earlier "no lever" conclusion is fully retracted. The lever is
baking in static objects' constant SPEED (and position), which the
per-field-constant-across-reachable-states analysis produces
automatically. That is the concrete next build.

## Building it: per-field constant lattice (2026-08-25, decisions to review)

Building the constant-lattice detection + baking. Decisions/assumptions,
flagged for review:

- **D1. Two phases.** Phase A: run the existing (fully-abstract) shape
  walk and, as a side effect, accumulate per shape the constant lattice -
  for each scalar field, the single constant it holds across every state
  that reaches the shape, or "abstract" if it ever varies / is symbolic.
  Phase B: regenerate each shape's kernel tracing with the constant fields
  PINNED (via the existing `pin`), so they fold. Start with the
  measurement (Phase A) to see the constant set before wiring Phase B.
- **D2. Soundness via pin_guard, so detection can be heuristic.** A pinned
  constant becomes an `ok` conjunct (`pin_guard`); a lane whose field
  disagrees is DECLINED to the base kernel, never wrong. So the lattice
  need not be a fully sound fixpoint - if it over-claims a constant, the
  guard catches it. This is exactly the base+guard design agreed earlier.
- **D3. Constants read from the pre-blank outcome states** (`o.st` in the
  walk), where a field is a constant iff its value node is `Const(v,v)` /
  `ConstBool`. The blanked representative is useless (all zeros), so the
  lattice is merged over every ARRIVAL at a shape, including the ones the
  walk currently skips as already-seen.
- **D4. Phase B shape set = Phase A's** (fully abstract), which is a sound
  superset; the const-pinned trace may reach fewer output shapes, and any
  lane not matching the pins falls to base.
- **Open**: the true fixpoint (trace from the lattice, which changes
  reachability) is deferred; Phase A/B is the first, guard-safe cut.

## Phase A result + Phase B soundness notes (2026-08-25)

Phase A (measurement) is a decisive win, room (2,0):
- The constant-lattice fixpoint reaches **18 shapes** (vs 36 abstract) -
  half the abstract shapes were spurious (spring-moved states that cannot
  actually occur, since springs never write their own spd).
- Fork counts: **10 shapes at 0, 2 at 2, 6 at 4, none at 8.** Shape 3
  (was 8 forks / 190k nodes) is now 0 forks. `objects[k].spd` captured as
  constant is the load-bearing piece.

### Soundness of USING the lattice (Phase B) - to review

The fixpoint is OPTIMISTIC: it traces with the current lattice pinned,
which prunes branches, so a field it calls constant might in truth vary
along a pruned branch. Two protections, both already available:

1. **pin_guard (automatic).** `trace_frame` with the lattice pins puts
   each pin equality into the frame's `ok`. A lane whose field disagrees
   with the baked constant is DECLINED - it must go to a fallback, never
   wrong. This handles field-value mismatches within a covered shape.
2. **Shape coverage.** If an over-claimed constant prunes a branch to a
   shape the lattice never generates, a real lane reaching that shape has
   no kernel -> the traced `Run` errors (coverage gap, per the doctrine:
   stop and report, not silently wrong). If the constants are TRULY
   constant (springs' spd provably is), the 18 shapes are complete and
   this never fires.

So the safe deployment is base+specialized: keep the abstract kernels for
full coverage, add the lattice kernels as the guarded fast path; a lane
takes the lattice kernel iff its fields match, else the abstract base.
For springs specifically the lattice is provably sound (spd never
written), so in practice the abstract base may be unnecessary - but that
needs the differential gate (lattice kernels vs the interpreter row-key
sets) before trusting it, which room (2,0) does not have wired yet.

ASSUMPTION for the size measurement below: correctness is validated
separately; the numbers show what the lattice kernels WOULD cost, which
is what decides whether room (2,0) becomes checkable-in.

## Phase B measurement: lattice cuts room (2,0) ~4.5x (2026-08-25)

Bound + lowered every converged lattice frame and summed the body lines:

- **Total body: 233,554 lines** (18 shapes), vs the current abstract
  room-(2,0) set at **1,064,829** (loop-fork) / ~2.27M (flat-fork). So
  **~4.5x smaller than loop, ~10x smaller than flat.**
- The 0-fork shapes are tiny (117-1,329 lines each). ALL the size is in
  the 8 shapes with 2-4 forks (12k-50k lines each). Those forks are the
  PLAYER'S move under a symbolic player velocity: the lattice bakes in
  the STATIC objects (springs), which removes the spring forks (8->4),
  but the player's own speed is not constant, so its velocity-direction
  branches remain (the 4 = 2 rem x 2 velocity).

So the decomposition is clean:
- **Constant lattice (static objects) -> removes spring forks, 8->4,
  1.06M->233k.** DONE, validated by measurement.
- **Player position/speed specialization (steps 1/2) -> removes the
  velocity forks, 4->2 (rem only).** NEXT - and it is exactly the
  concrete/interval-speed compile Philippe described. On the 0-fork
  shapes there is nothing left to do; on the 8 moving-player shapes it
  would take them from ~12-50k lines toward the room-(1,0) scale.

233k is still above room (1,0)'s 24k checked-in threshold, but it is in
range once the player-speed forks come out, and the fixpoint's 18 shapes
(vs 36) means far fewer kernels. This is the path to a checkable-in
room (2,0).

## Lattice kernel generation built (2026-08-25)

`write_room_kernels_lattice` + `transpile --room-kernels-lattice DIR`
render the 18 constant-lattice kernels (each with the lattice's
`pin_guard` baked into `ok`). This is the artifact path; the abstract
`write_room_kernels` is unchanged so room (1,0) is unaffected.

Validation ladder before this can be trusted/used (in order):
1. **Compiles** (emitter correctness) - the rendered kernels type-check.
2. **Differential** (semantic correctness) - lattice kernels vs the
   interpreter's row-key sets on room (2,0). Needs the kernels wired into
   `compiled::dispatch` behind `CELESTE_COMPILED_FORWARD=check`, which
   room (2,0) does not have yet. THIS is the gate that matters and is not
   done - until it passes, the lattice is a measurement, not a shipping
   kernel set.
3. **Coverage** - confirm the 18 shapes cover every reachable shape (the
   fixpoint's optimism, guarded by pin_guard + base fallback).

ASSUMPTION carried: the lattice's constants (esp. springs' spd=0) are
truly constant, so the 18 shapes are complete and the kernels are sound.
This is believed (springs never write spd) but UNVERIFIED by the
differential gate. Do not check in or default-enable the lattice kernels
until gate 2 passes.

## The block-deopt (bd) integration point (2026-08-25)

All 18 lattice kernels fail the render's `bd_v0_b0 != false` guard: the
`pin_guard` for BLOCK-UNIFORM pinned fields (8-12 of them) becomes a
block-level deopt condition, which the render refuses because there is no
fallback wired. This is not a bug - it is exactly where a SPECIALIZED
kernel should route the whole block to the BASE kernel.

And it is CLEANER than per-lane routing: the pinned fields are
block-uniform (same across all 16 lanes), so `bd` is a single block-level
test - "does this block's uniform state match my baked constants?" - and
if not, the ENTIRE block takes the base. No per-lane mask needed.

### The feature to build (next)

1. **Emitter**: allow `bd != false` when a kernel is marked "has a base
   fallback" (a flag on the render/Emit). The generated `frame`/`step`
   already computes `bd`; expose it so the caller sees "block declined".
2. **Dispatch**: base + specialized. For a block, try the lattice kernel;
   if its `bd` fires, run the block on the base (abstract) kernel for the
   same shape. This is the `run_chunk_kernel` fall-through pattern, at
   block granularity.
3. Then the differential gate (lattice+base vs interpreter) can run.

CELESTE_ALLOW_BLOCK_DEOPT env added to bypass the refusal for SIZE
MEASUREMENT only - the kernels it produces would silently drop a
non-matching block, so they must NOT be run until the dispatch above
routes bd to base. Measurement-only, flagged.

### Note on avoiding bd for provably-constant fields

A field PROVABLY never written (springs' spd) has a redundant pin_guard
(always true). If a later pass proves "never written", we can bake it in
with NO guard (no bd). That removes the block-deopt for the safe
constants and leaves guards only for heuristic ones. Deferred; the
bd->base routing is the general answer and is needed regardless.

## Realizable size, and gate 1 (2026-08-25)

Lattice kernels rendered (bd bypassed for measurement): **401,120 lines
on disk** across 18 kernels, vs the abstract room-(2,0) at 1,064,829 -
**~2.65x smaller** for the FULL kernel (body + structs/acc/append). Per
shape: the 10 zero-fork shapes are 2.3-7k lines each; the 8 moving-player
shapes are 19-81k (shape 17 the largest at 81k). So the lattice halves-
plus the shape count and cuts the total ~2.65x; player position/speed
specialization would take the 8 big shapes down toward room-1 scale.

Gate ladder status:
- Gate 1 (compiles): PASSES - kernels 0,5,17 rustc-check clean against a
  consistent rlib set (earlier failures were stale-rlib mismatch).
- Gate 2 (differential vs interpreter): needs bd->base dispatch, not yet.
- Gate 3 (coverage): follows gate 2.

## bd wired to declined - lattice kernels now emit safely (2026-08-25)

`VarOut` carries `bd: Option<String>`; the render wires a non-trivial `bd`
into `declined` (block-level obligation fails -> every live lane of the
block is DECLINED and reported, never silently dropped) and takes none.
This is doctrine-compliant (a deopt stops and reports). Abstract kernels
have `bd == None` (their block conjuncts are always false), so their
output is UNCHANGED - `traced_kernels_are_current` gates that. Removed the
render refusal and the CELESTE_ALLOW_BLOCK_DEOPT bypass; bd is handled.

So the lattice kernels now render without the bypass and DECLINE a block
whose baked constants do not hold, rather than dropping it. If the
lattice is sound (springs' spd provably 0), bd never fires and no block
is ever declined.

### Validation path (next)

Wire the lattice kernel set into a room-(2,0) runner (like
traced-kernel-check does for room 1) and:
- run it: bd must NEVER fire (no declined blocks) - that is the lattice's
  soundness, checked empirically frame by frame;
- compare its per-frame row-key SETS to the interpreter's (the
  differential gate). Equal sets = the lattice kernels are correct.
If bd fires or sets differ, the lattice over-claimed a constant; the
declined block tells us which shape, and pin_guard kept it from being
wrong.

## Gate 1 fully passes (2026-08-25)

With the bd-wiring (no bypass), all 18 lattice kernels render; kernels
0, 5, 8, 14, 17 (incl. the 81k-line largest) rustc-check with 0 errors
against a consistent rlib set. Room 1 unchanged. So the lattice kernel
set is valid, safe (declines mismatched blocks), and ~2.65x smaller.
Gate 2 (differential vs interpreter, via a room-2 runner) is the next
piece.

(Reminder: `cargo clean -p celeste-core -p celeste-engine -p celeste-names`
then rebuild before any manual rustc of a generated kernel - duplicate
rlibs from intervening builds otherwise give spurious "two versions of
celeste_core" type errors.)
