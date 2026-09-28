# Room (6,0) overnight, 2026-09-28 - decisions and findings

Philippe's brief (going to sleep): solve room (6,0) properly; make the
decisions myself, note them here, discuss in the morning; don't stop early,
don't move to other rooms.

## Where it started

Branch `step5-split`, on top of `e99ecb8`. Platform WORLDS enumerated as a
128-way fork at compile time built (2.3M bodies, 65M fused nodes, the largest
region 907k nodes, 35 GB resident for the kernels) and ran level 0 to f35
(550k states, 49 s a frame, x1.9 a frame) before a KERNEL COVERAGE GAP at f36.
Philippe: 900k-node kernels are no good; the worlds should narrow the path
condition, not fork.

## The design now: POINTS (verify::Points)

The platforms stay interval inputs (`x` the input cell, `last` = `x`, `rem.x`
the literal). The split pass (`split_undecided_selects`) carries, per path,
the set of (world, player pixel in the region) points its answers are
consistent with. Each comparison is evaluated at each point (the interval
evaluator over the comparison's cone, platforms' `x` pinned to the world,
the player's `x`/`y` to the pixel); an answer no point gives is no side, a
comparison with one answer at every point is decided without a split. At the
end each outcome's guard gains "the lane's pixel is one of mine"
(`position_guard`). Nothing about worlds reaches a lane.

## Decisions (each revertible)

1. **`rem.x` of the platforms stays a literal, not pinned per world.**
   Pinning it needs it to be an input cell, and an interval input cell's
   floor (the platform's own move) forks the frame, once per platform. A
   literal's floor runs as rejoined fragments (no fork). Cost: a pixel of
   slack in where a platform stands after its move, at the `p` rungs only;
   the exact-platform rungs above refine it.
2. **The error is finished, not split, once the row is resolved**
   (`settle_error`). Splitting the OUTCOME on the error's selects produced
   two sides with one row that merged back as `(g1 and e1) or (g2 and e2)`,
   doubling each round - a 41-node row under a 5M-node error, 50 GB.
   First version: the error read three-valued. That declined a spawn-frame
   lane (KERNEL COVERAGE GAP at f2): the hull of the platform wrap
   `x < -16 ? 128 : (x > 128 ? -16 : x)` is `[-16, 129]`, which fails its
   own containment. Now (`MayErr`): the error's may-be-true by case
   analysis on the error expression alone - `or` operand by operand (exact,
   and linear over the ten platforms), a node reading an undecided select
   split on its lowest atom under that answer's `may` guard and the points,
   `and`/`not` through may-true/may-false pairs (over-approximates: more
   declines, never fewer), three-valued past 4096 cases.
3. **Exact-only ops distributed over select arms in `three_valued`**
   (`exact_only`: TileFlagAt, Mget, Rem, Sin, Mul, Div; up to 64 arms, else
   unknown / the whole range): the kernel cannot run a tile lookup at a
   hulled position.
4. **Booleans typed bottom-up in `three_valued`**: a select of booleans read
   as another select's condition was taken for a number.
5. **Facts (the difference-bound narrowing) removed**: the points subsume
   them exactly.
6. **The lightweight split queue (`Lite`)**: the pass no longer clones a
   `FrameOut` (a whole `State` and block) per side.
7. **Walk parallelism**: `XWALK=N` (temporary knob) caps the lattice walk's
   workers; 32 concurrent heavy traces reached 45 GB. Runs use 16.

## Log

- f2 coverage gap with the three-valued error (above); the build then was
  1.13M bodies / 31M fused nodes over 305 shapes, 12.5 min. The largest
  kernel (673k nodes, 10 forks) is a no-player shape: its platforms'
  `spd.x` is not pinned (0 at the first frame, +-0.65 after), so the literal
  remainder plus the speed is no literal and each platform's move floor
  forks. Size, not correctness; left for now.
- The same f2 gap with `MayErr`. `CELESTE_EXPLAIN_DEPTH` showed the graph
  evaluator returned TOP for every `SplitOk`, hiding the kernel's values;
  **`Graph::eval` now models `SplitOk`** (true when the hull fits the
  fragments, else unknown - sound, and it also lets the interval fold
  decide premises that provably hold). The real error: each platform's
  containment `Hi(x) <= 128` on the not-wrapped arm of the wrap, `x = x0 +
  flr(Frag(..))` - the platform move is a real fork in the no-player traces
  whose `spd.x` is unpinned, and `widen::bounds` knew no `Frag`, so the `Add`
  failed before the branch fact `not (x > 128)` was consulted and `within`
  gave up. **`bounds` now passes `Frag`/`IntFrag` to their operand and falls
  back to the branch facts when the structure fails.**
- Still the gap after that: the move's bound needs each platform's `spd.x`,
  an unpinned input cell with no range. **`platform_inputs` now ranges each
  platform's `spd.x` cell by its speeds over the worlds, as an admissibility
  obligation** (checked per lane like the region bounds, never assumed).
  And **`within` now tries a node's own bounds (with the branch facts on
  it) before splitting a select into arms** - the wrap's fact is on the
  select "did it move", not on its arms. Unproven platform containments in
  the walk: 90 -> 0.
- f1-f23 (spawn phase) now run; f24 declined on `not Known(UnknownBool)`: a
  REAL BUG since the split pass exists - `rebuild_all` dropped where a node
  was evaluated (`Symbolic::evaluated`), so a rebuilt node's own error held
  on every lane (here the inner select of a three-valued guard's
  `Sel(Known(c), Sel(c, ..), ..)`). **`rebuild_all` now carries the
  registration to the rebuilt node, at the rebuilt condition.** This was
  sound before (it declined, never dropped) but it declined spuriously in
  every room the split pass touches.
- f24 still declined. A walk-only report (`XSTRAY`, removed) found ~750k
  selects on undecidable conditions with no evaluation registration - all
  in the CONDITIONS operators were evaluated under (`Symbolic::evaluated`,
  the tracer's path guards: e.g. `sign(x - last)` over a rejoined interval),
  which the split pass never sees but the error derivation reads. **After
  the split pass those conditions are read three-valued too
  (`three_valued_evaluated`)**: unknown where the lane cannot decide, so
  `own and at` reads as may-err - sound, no garbage bit.
- Still f24: the stray select was the guard's own, `three_valued`'s
  `picked` - hash-consed, so the original select - registered at `true` by
  the tracer; `evaluated_at` ORs registrations, so `Known(c) or true` =
  `true` and its `not Known(c)` held everywhere. **`three_valued` now SETS
  the registration to `Known(c)`** (after the split pass that wrapper is
  the only place a raw undecided select survives).
- Still f24: `CELESTE_EXPLAIN` + a walk report of unguarded `not Known(x)`
  error sources (`XSTRAY`, removed) found 546k of them, from decision 3's
  fallback (a numeric exact-only op past 64 arms became the whole range,
  then `Flr` of it) and from selects whose condition became undecidable only
  AFTER rewriting (a tile test past the cap is unknown) - tested on the
  original condition, left raw. Fixed: **`Mul`/`Div` are exact-only only
  without a positive-literal operand** (the kernel scales an interval by
  one); **undecidability is tested on the rewritten condition too**
  (`three_valued` and `arms`); **`three_valued` is idempotent** (it leaves
  its own wrapper's inner select alone - rewriting it again moved it off its
  registration); **guards and the error are read three-valued at the END of
  the split pass**, not the start, when the row's splits have substituted
  most of what they read. Fallbacks 86k -> 36k (mostly `TileFlagAt`),
  unguarded sources 546k -> 317 -> (arms fix) pending.
- Result: **f24 passes** (26 states); the kernel set shrank to 15.9M fused
  nodes (from 41M; the largest 343k). f25: a gap in the player shape
  `0xba8c6f00cf1d2931` - the same shape the 128-way-fork build died in at
  f36.
- f25 gap in the player shape: the lowering's own diagnostic
  (`CELESTE_BUILD_TRACE`, "error folds TRUE while live") showed pins on a
  platform's `spd.x` (-0.65) read against my world range for ANOTHER
  platform (`[0, 0.65]`). **The traced state's platforms are in its row's
  canonical order, not the load order the worlds were recorded in** - so
  the per-world pins (and before tonight, the 128-way world fork's) could
  hit the wrong platform. **Worlds now record `y` and `dir`, and
  `platform_inputs` matches each traced platform to a world platform by
  them**; platforms alike in both are alike in everything a row keeps, so
  any matching among them is the same.
- The platform matching was not it: the contradicting pins/ranges included
  a non-platform `x` pinned to 64 against a range [24, 27]. **The spawn
  chain's no-player ranges (`no_player_ranges`, by PATH) were applied to
  every no-player walk node, including a second no-player shape
  (`0xba8c...`) whose `objects[k]` is a different object** - every body of
  that shape erred. Now applied to the chain's own shape only. This is
  almost certainly also what killed the 128-way-fork build at f36 (same
  shape). (The `y`/`dir` matching stays: the order assumption was
  unchecked either way.)

## Level 0 runs (r0sxhp)

After the no-player fix the level-0 forward runs: f24 26, f30 15k, f35 463k,
f36 804k, f40 6.15M states (177 s a frame, 16 GB), growing x1.6-2 a frame -
it could not reach f72. `col-census` at f36: the player (x 57, spd.x 56, y 52,
spd.y 49 values) and **the fly fruit's bob phase** (`spd.y` 21, `rem.y` 20,
`step` 12, `y` 6, `fly` 2): like the platforms, a phase that stops states
from different frames merging. **Decision: level 0 is `r0sxhfp`** (fruit
unknown too; `plans/fly-fruit.md`, refined by the exact rungs above).
- `r0sxhfp`'s build blew up (50 GB, 30 min, still specializing): the
  no-player shape's trace, ten platform move-floor forks (the platforms'
  `spd.x` is unpinned there: 0 at the load, +-0.65 after) times ~230 fruit
  outcomes, 2048 configurations each. The ten forks are DEAD - the platforms'
  moved `x` is widened away and its containment proven statically, so every
  fragment gives the same row. **`specialize_frame` now takes a dead grid
  fork once** (all fragments give the same fields, keys, error, and `live`
  with the fork's own validity set true -> one configuration with validity
  TRUE, `graph::ANY_VALID`: the OR of the fragments' validities is the lane
  covered). Every room's lowering sees this; gates re-run.
- The dead-fork rule did not fire on the spawn shape: the fragments'
  errors differed only in OR-tree nesting, but their `live` genuinely read
  the platforms' moved `x` (the fly fruit's collision checks). Instead:
  **in a no-player trace, a platform's unpinned `spd.x` is read as the
  literal `dir * 0.65`** (asserted: the lane's is 0 or that one). With no
  player the move is unobservable - `x`, `last`, `rem.x` are widened at the
  frame's end and `update` sets `spd.x` anyway - so the row is the same, and
  the floor of literal plus literal rejoins as fragments (no fork). The
  region-less kernels: one 22M-node graph (OOM at 50 GB) -> 12.6k bodies,
  164k fused nodes. (The dead-fork rule stays: sound, and gates pass.)
- `r0sxhfp` then OOMed in ASSEMBLY (kernels of ~4M instructions, 32 at a
  time). **`CELESTE_BUILD_THREADS`** caps the lowering/assembly pool (runs
  use 8; the walk uses 16).
- ...and again at 61 GB with 8 build threads, 299 of 306 kernels
  assembled. **Decision: this build runs under `safe-run --memory 85G`**
  (107 GB available, /tmp 3.8 GB) with 6 build threads. Revisit: the
  resident kernel set is the cost (the largest kernels are 150-260 MB of
  assembly each).
- `r0sxhfp` BUILT (85G): 2.7M bodies, 26.7M fused nodes over 306 shapes
  (`r0sxhp`: 1.13M / 15.9M). Ran f1-f25 (f25: 270 states), then a gap at
  f26: every one of the 133 rows of the fresh player shape missed. Explain
  run under way.
- f26: the error held fresh unknown atoms - `three_valued`'s fallback for a
  tile test past 64 arms - which reached it through MERGED outcomes'
  `(g1 and e1) or (g2 and e2)`: a lane on path 2 declined for path 1's
  unknown guard. **The merged error is now settled at the end by `MayErr`**
  (exact case analysis with the points; three-valued only past 4096 cases)
  instead of read three-valued.

## Morning (2026-09-28) - node count

Philippe: 15-27M fused nodes will not finish the room; aim ~10k per kernel.
One-pixel probes (`CELESTE_REGION=1,6`, `CELESTE_WALK_REGIONS`):
- (72,88): 12 outcomes, 597-2388 bodies, 10.8-22k fused nodes per kernel.
- (96,80), above the y=88 platform row: 24 / 49 outcomes (49 = 7 x 7:
  the x and y move loops' stop pixels), 1194 / 2390 bodies, 12k / 25k nodes.
- Per outcome ~50 bodies = 24 button reps x 2-4 fork configurations; the
  outcomes differ by rebuilt dash/jump conditions reading the post-move
  state, not by buttons.
- 16 px region (4,5): 120 outcomes, 6024 bodies, 78k nodes (one shape).
- Separate: the spawn-landing trace of the second no-player shape has no
  bounds since its ranges were restricted to the chain's shape: 234
  outcomes. Needs its own chain's ranges.
Philippe: buttons should be forks (unknown booleans through the generic
fork mechanism), not a separate body dimension - delegated to a background
agent on a worktree.
- DONE (branch `buttons-as-forks`): `Domain::unknown_bool` is
  `Symbolic::both_values` (a 2-way `Ints` fork over the literal `[0, one
  grid step]`, no validity); `Op::Free`, the `frees` threading and the 64-way
  rep pass are gone. `specialize_frame` sorts an outcome's forks by what a
  configuration means: those every lane takes both ways (buttons, held
  trails, escaped atoms) are resolved one at a time with root dedup into
  CLASSES (the old 64 -> 24 collapse, now also over the held trails); those
  that partition lanes (validity read, tables) are enumerated per class as
  before, their dead-ness decided once per outcome. Same bodies everywhere,
  fused nodes equal or fewer, gates identical.
