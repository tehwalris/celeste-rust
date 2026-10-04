# The level flags: what each widens, why it is sound, what narrows it

A level is `abstraction::Level { pos, rem, spd, held, fruit, floors,
platforms }`, written `[x2][y2]r<k|x>s<w|x>[h][f][b|n|t][p]`
(`Level::parse`): `r0sxhn` = rem rung 0, exact speed, held buttons unknown,
fall floors (and every object's phase) widened except where the player
overlaps. A ladder (`CELESTE_LADDER`) lists levels coarsest first, each
coarser-or-equal to the next in every coordinate, the last exact in all.

## The rules every flag obeys

- **Never widen a field without a rung that narrows it back** (CLAUDE.md).
  Every flag below is off at the exact level. The exceptions are listed under
  "Widened at every level" and are either exact (lose nothing) or a stated
  best-case caveat.
- **A widening only replaces a value by something visibly containing it**: a
  range that holds it, or unknown. Never by another value (`step := 0.5`
  because nobody reads it is out, however true).
- **No bands justified from outside the frame.** A widened range must be a
  fixed point of the frame's own computation, checked per row (the widening's
  own error, `widen::SlotErrors`) where not trivially total.
- **An uncollected object is one row**, its fields intervals - not several
  rows because `move`'s `__split_by_flr` forks it.
- **Projection must agree**: the tracer's in-graph widening (`trace::widen`)
  and the block model's (`Rt2::widen_to`, which `MarkFilter` uses to key a
  fine row on the coarser level) must produce the same row, or the next rung
  drops everything. Check with a synthetic win and a small ladder.
- **The reference engine refuses every flag but rem/pos/spd** (`h`, `f`,
  `b`/`n`/`t`, `p`): witnesses run at exact levels.

## Decided: fork AFTER the frame, never eagerly (Philippe, 2026-09-21)

The first interval-comparison mechanism, the eager POINT SPLIT
(`split_compare`), forked each comparison of an interval with a number as it
was evaluated and put the answer's validity in the path guard. Every
outcome's `live` then read every comparison of the frame: room (6,0) at
`r0sxhp` minted 70-86 forks per trace, 64-82 live (2^70 bodies per outcome
past the 58-fork mask). Replaced by forking after tracing, only where an
undecided select SURVIVES folding - first `verify::fork_known_premises`, now
`verify::split_undecided_selects` (plans/architecture.md "The tracer and the
kernel model"). Room (5,0) level 0 kept counts were identical to the
point-split run at every frame.

## rem: the sub-pixel remainder (rungs `r0`..`r15`, `rx`) - BEING REMOVED (arcs)

- **Widens**: the player's `rem.x`/`rem.y` to a bucket of width 2^-k
  (`RemPrecision::Bits(k)`); rung 0 is the whole [-0.5, 0.5).
- **Mechanism**: the single-grid fork (`740e093`, `Graph::fork_bits`): at
  rung k the integers are bucket edges, so `move` cuts `rem + spd + 0.5` once
  at the bucket grid and each piece's floor is single; the boundary only
  snaps. Rung 1's bodies 2,451 -> 723, the level-1 frame 2.2 s -> 0.19 s.
- **Narrowed by**: the next rung; exact at `rx`.
- **Status**: every result in plans/results.md rests on it - and it DRIFTS.
  A move that straddles a bucket edge emits a sliver row the boundary widens
  to the whole bucket; no concrete state is in it, and the abstract player
  gains up to a bucket width per frame per axis (~1.9 px over 120 airborne
  frames at rem 6). In long rooms the marks grow 2-2.6x per rung instead of
  shrinking (room (3,3): six ladders out of memory). The rotation graph
  (plans/architecture.md "Arcs") tracks the remainder exactly and replaces
  the rungs.

## pos: 2 px position buckets (`x2`, `y2`) - unused, BEING REMOVED

- **Widens**: the player's whole-pixel `x`/`y` to a 2 px bucket, one rung
  below level 0. The frame never runs an interval position:
  `widen::fork_pos_inputs` forks the bucket into its pixels (`IntFrag`, an
  exact number per configuration), collisions are exact, and the output
  snaps back. Needs rem `Bits(0)` (`grid_consistent`); widths above 2 refused.
- **Measured**: post-hoc 2 px halves a frame's states (y alone 1.6x, x 1.3x).
  Realized on room (1,0): one ladder at h99 9:32 against 2:56 - the rung
  first wins at f74, so at h99 it marks 56% of what it visits and filters
  nothing. A coarse level only cuts near its OWN first win. Off by default.

## spd: speed buckets (`s16`, `s20x`, ...) - a loss everywhere, BEING REMOVED

- **Widens**: the player's `spd.x`/`spd.y` to buckets of a table (the cart's
  thresholds plus a grid, `celeste_core::spd_buckets`), dispatched per
  bucket key (`trace::kernel::SpeedKey`, the key fixpoint) so no comparison
  on the speed is undecided; `move`'s fork gets arity 3 where a spring
  rewrites the speed first. `CELESTE_SPD_LADDER` presets: `exact` (default),
  `bucket`, `level0`.
- **Measured**: room (1,0) f50: the realized merge 1.11x against a post-hoc
  ceiling of 1.8x, 1.5 rows per state from hull growth, 9x the raw rows, a
  frame 30-36x slower. The bucket ladder on room (1,0): 24 min to h88 against
  4:51 for the whole exact search. Room (2,0) (spring: 3,200 `spd.x` values,
  19x post-hoc) did not build; room (7,0) `r0s16h` 9,821 kernels, 36 GB, more
  states than exact; room (0,2) `r0s16hn` OOMed building. The level -1 filter
  is what handled the spring rooms instead.

## h: held buttons unknown (`HeldPrecision`) - CURRENT, every object room

- **Widens**: the player's `p_jump`/`p_dash` (the previous frame's buttons,
  used only to detect a press) to unknown at every non-exact level. Output: the
  uniform `AV::UBool` through the widen list (out of the per-lane key). Input:
  `widen::fork_held_inputs`, one 2-way `SplitInt` fork per trail over the
  literal [0, 1], no validity (every block writes them unknown).
- **Soundness**: a held button may retrigger (a ground jump then a wall jump
  on consecutive frames); over-approximation, refuted by the exact level. The
  same widening applied at EVERY level was rejected twice (2026-08-06,
  2026-08-16): nothing narrowed it.
- **Measured**: room (1,0) level 0 3.8x fewer states at f70, first win
  unchanged (f89), OPTIMAL 99 in 1:38. Room (2,0) f68: 50.7M -> 13.3M states,
  81 s -> 16 s a frame, 28.7 -> 8.7 GB. A constant factor, not a slower curve.
- **Later**: dominance at the exact rung (drop a held twin whose released twin
  is visited).

## f: the fly fruit unknown (`FruitPrecision`) - CURRENT for fruit rooms, low rungs only

- **Widens**: the fly fruit's `step`, `y` to the unknown number
  (`Op::UnknownNum`: stays unknown under every operation, never raises - a
  full-range interval raises at the 16.16 extremes); `spd.y` to [-3.5, 0.5]
  and `rem.y` to [-0.5, 0.5) as literals, checked per row; `fly` an unknown
  boolean (`widen::fork_fruit_inputs`, `widen_fly_fruit`).
- **Why**: while waiting, `step += 0.05` and the bob mean no fruit state ever
  recurs across frames (0.05 is not exact in 16.16: no lossless canon); once
  flying, rows split by the frame of the dash. Room (3,0) f47: erasing the
  fruit merges 2.30x, any one field ~1.00x (they vary together).
- **Mechanisms it brought** (shared by the floors and platforms): literals stay
  literals (lane-independent arithmetic evaluated at trace time); a merge
  where no lane decides the condition joins to a hull / unknown, where lane
  data decides it the two states stay two successors
  (`Interp::joins_independent`); `__split_by_flr` of a literal runs each
  fragment as its own trace state and rejoins at the call's return
  (`Interp::rejoin_fragments`), cut on the integers (`5a98e20`).
- **Measured**: room (3,0) level 0 3.6x fewer at f50, kernels 25k -> 90k
  bodies. Above level 0 it merges nothing (the undecided `collide(player)`
  forks collected / not) and fanned out in room (6,0) (331 trace states, cap
  256), so ladders keep it to rungs 0-4 (`r0sxhf .. r4sxhf, r4sxh ..`).
  Collected-or-not after it flew away stays two shapes by design (a collect
  refills the dash).

## b: fall floors fully unknown (`FloorsPrecision::Unknown`) - SUPERSEDED by `n`

- **Widens**: every fall floor's `state`/`delay` to the unknown number,
  `collideable` an unknown boolean, every frame, touched or not. An atom made
  inside a call that escapes it becomes one fork of both values at the
  return (`Domain::escaped_atom`), so the player (which updates after the
  floors) reads forks and its collisions are decided per configuration.
- **Why it lost**: "maybe absent" from frame 1 lets the player fall through any
  floor; room (7,0) step 80 2.0M states against 143k exact. Its fixes were
  worth keeping: a merge counts only the selects it made; `(g and c) or (g and
  not c)` folds to `g`; worker stacks 512 MB with a per-call frame-fits check.
- Room (3,0)'s 89 used `b` (`r1sxhb .. r15sxhb`); it is otherwise unused.

## n: everything abstract except where the player overlaps (`FloorsPrecision::Near`) - CURRENT, the object default

- **Widens**: each fall floor's `state` to the interval [0, 2]
  (`runtime2::FLOOR_STATE_RANGE`) with `collideable` DERIVED as `state ~= 2`
  (the cart keeps them in step), exact only where the player overlaps the
  floor at the frame's end (`widen::widen_near_floors`); the countdowns (floor
  `delay`, balloon `timer`) the whole 16.16 range; the objects' phases
  (`widen::phase_paths`: spring `spr`/`delay`/`hide_in`/`hide_for`, balloon
  `spr` and its bob `y` as `start +- 2`) their ranges. A widened object's
  update is then "maybe X" (maybe bounce, maybe refill) and nothing else. An
  overlapped floor stores the cart's invariant (hidden), owed per lane
  (`09505c7`); in the split frame a floor the player's `is_solid` can reach
  keeps its computed `collideable` mid-frame (`widen::PLAYER_PROBE`).
- **Soundness**: `diag-project` checked every row of a room (7,0) `t` tree
  (7.88M, steps 1-96) projects onto a row of the `n` tree. The interval-wrap
  caveat (plans/architecture.md "Where it still diverges") means the `n`
  results are believed sound, not proven.
- **Measured**: room (7,0) step 108: `b` 20.2M, `n` 3.4M, `t` 2.8M states
  before the join-bug fix; after `7f6b96e` (the boolean join of an undecided
  `collideable` threw away the player's overlap test, so every collision
  near a floor read "maybe") `n` level 0 reached step 168 at 15.4M states max,
  22.7 GB. Room (0,1) level 0 at f100: 14.8M states / 14 GB, against 40.0M /
  30 GB with the balloon's `spr` exact.
- **How long to keep it**: one rem rung past level 0 at least (room (0,1):
  the first exact-objects level peaked at 88k states a frame after `r1sxhn`,
  26.5M straight after level 0); through rem 4 is the usual recipe; balloons
  stop paying past ~rem 3 (room (4,1): marks 2.9M -> 121M from rem 3 to 8 on a
  spurious 91 the first exact-balloon level refuted at once).

## t: only the countdowns widened (`FloorsPrecision::Timers`) - UNSOUND as built, unused

- **Widens**: each floor's `delay` and the balloon's `timer` to the whole
  16.16 range; `state`/`collideable` exact. The cart only compares the
  countdowns with 0 and decrements them, so the whole range is as precise as
  any tighter one.
- **Why unused**: the ASM interval subtraction wrapped the low end of the full
  range (`delay - 1` -> [32767.99, 32766.99]), so `delay <= 0` came out "no":
  shaking floors never fell. Every `t` level of room (3,3) was unsound. And a
  `t` set is ~16x an `n` set (1.35M bodies, ~13 GB; `CELESTE_KERNEL_SETS=1`
  required). A sound `t` needs the countdown fork (`delay <= 0` forked, not
  computed on an interval) - which is also what merging `asm-interval-wrap`
  needs.

## p: moving platforms unknown (`PlatformsPrecision`) - CURRENT for platform rooms, low rungs only

- **Widens**: every platform's `x` and `last` to the whole path [-16, 128],
  `rem.x` the literal [-0.5, 0.5). `last == x` holds at every frame boundary
  and is bound as symbolic identity at the input (checked on every traced
  output: the output `last` is the output `x`'s node); the carry `(b + c) - b
  = c` cancels exactly in the tracer's subtraction; the wrap keeps x inside
  the path (proved on the traced select, else a per-lane error).
- **Why**: an exact platform is a frame counter in every state - nothing merges
  across frames (room (6,0) level 0 x1.4 a frame, 81M states at f50).
- **The points**: the platforms are a function of one number (frames since
  the room loaded), so `concrete::platform_worlds` records every arrangement
  and the split pass carries per path the (world, player pixel) points its
  answers are consistent with (`verify::Points`); nothing about worlds
  reaches a lane. (Enumerating ~128 worlds as a compile-time fork built
  2.3M-body, 35 GB kernel sets: plans/lessons.md.)
- **Measured**: room (6,0) `r0sxhfp` split frame: level 0 to step 150, 1.68G
  visited; optimum 70. Room (2,1): `r0sxhnp` then `r0sxhn` (platform exact at
  level 1). Room (1,2): the platform exact at level 1 marked 253M states
  (level 0: 22.6M) - keep `p` through a few rem rungs next time.

## Widened at every level (exact, or a stated caveat)

- **The balloon's phase** (`widen::canon_balloon_offset`): `offset` is read
  only through `sin`, and `sin` of a full period is [-1, 1], so it is stored
  as the canonical [0, 1) with a full-period premise. EXACT: room (5,0)
  reaches the same 1,374,280 balloon-free states through f50 in 296k rows
  against 1.42M (it had stopped all cross-frame dedup: `offset` advanced 0.01
  every frame).
- **`rnd`** is an interval at every level, the exact one included (decided
  with Philippe 2026-09-20). A refutation holds for every draw; a
  confirmation means "some draw wins". The witness is replayed on PICO-8 per
  seed (`pico8_diff/replay.py --balloon-seeds`, the TAS file's header fixes
  each balloon's offset); several witnesses exit only for some seeds
  (plans/results.md).
- **`widen::ABSENT_AS_ZERO`**: a fall floor gains `delay` only when it first
  breaks, which made "which floors ever broke" 12 bits of the heap shape. A
  missing `delay` is written as 0 at every level: the cart only does `delay -
  1`, `delay <= 0` with it, each a runtime error on nil, and
  `cart::check_absent_fields` refuses a cart that reads it any other way.
  Philippe: a hack. Replacement: a real nil-or-number field (a per-row
  presence tag) at the exact levels; at the widened levels the unknown can
  cover nil.
- **The region key** (`RegionGrid`, not a widening): kernels are specialized on
  the player being in one 16 px square with speed in [-6, 6], guarded per
  lane; a lane outside declines.
