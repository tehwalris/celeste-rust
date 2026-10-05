# The level flags: what each widens, why it is sound, what refutes it

A level is `abstraction::Level { held, fruit, floors, platforms }`, written
`r0sx[h][f][n][p]` (`Level::parse`): `r0sxhn` = held buttons unknown, fall
floors (and every object's phase) widened except where the player overlaps.
The `r0sx` prefix says what every level shares: the remainder widened at the
boundary and tracked exactly by the arcs, the speed exact. One search runs
ONE level (`rewrite search --level`, plans/architecture.md "The search");
the precision ladder that ran several (rem rungs, then exact objects,
`CELESTE_LADDER`) was deleted on 2026-10-05.

## The rules every flag obeys

- **Never widen a field without something exact that refutes it**
  (CLAUDE.md). The flags below are refuted by the concrete count-up: it runs
  the real game (the reference engine's concrete step, every input, every
  `rnd` leaf) and prunes by nothing but the arcs' exact winning sets, so a
  win the widening invented has no concrete path and the count-up moves past
  it. The exceptions are listed under "Widened at every level" and are either
  exact (lose nothing) or a stated best-case caveat.
- **A widening only replaces a value by something visibly containing it**: a
  range that holds it, or unknown. Never by another value (`step := 0.5`
  because nobody reads it is out, however true).
- **No bands justified from outside the frame.** A widened range must be a
  fixed point of the frame's own computation, checked per row (the widening's
  own error, `widen::SlotErrors`) where not trivially total.
- **An uncollected object is one row**, its fields intervals - not several
  rows because `move`'s `__split_by_flr` forks it.
- **Projection must agree**: the tracer's in-graph widening (`trace::widen`)
  and the block model's (`Rt2::widen_to`, which projects a CONCRETE state
  onto the level for the count-up's node lookup) must produce the same row,
  or the count-up misses the node and prunes a real path - a wrong "no win
  within f". `rewrite follow` (a known solution against a tree) checks it.
- **The reference engine as a frame step refuses `f` and `p`** (no reference
  form for their ranges and worlds; `h` and `n` it forks per path). The
  count-up's concrete steps are exact whatever the level.

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

## The remainder: arcs, at every level

- **Widens**: the player's `rem.x`/`rem.y` to the whole [-0.5, 0.5) at the
  boundary (`widen::widen_rem`), with its containment the widening's own
  error.
- **Refuted by**: nothing needs to: the transfer of every recorded edge
  (`search::arc_edges`, plans/architecture.md "Arcs") says EXACTLY what the
  frame did to it, and the backward over them is exact in the remainder.
- **History**: until 2026-10-05 the remainder was refined by rungs
  (`RemPrecision::Bits(1..=15)`, then exact), which DRIFTED: a move that
  straddles a bucket edge emits a sliver row the boundary widens to the whole
  bucket, the abstract player gaining up to a bucket a frame per axis, and in
  long rooms the marks grew 2-2.6x per rung (room (3,3): six ladders out of
  memory). Every result in plans/results.md before 2026-10-04 rests on the
  rungs; the arcs reproduce them (plans/results.md, "The arc pipeline").

## Deleted: position buckets (`x2`, `y2`), speed buckets (`s16`, ...), floors unknown (`b`), timers only (`t`)

All four are gone from the code (2026-10-04/05; git history). The position
rung cut a frame's states in half post hoc but marked 56% of what it
visited at h99 and filtered nothing. The speed buckets were a loss
everywhere (room (1,0) f50: a realized merge of 1.11x against a post-hoc
1.8x, frames 30-36x slower; the spring rooms were handled by level -1
instead). `b` let the player fall through any floor from frame 1 (room (7,0)
step 80: 2.0M states against 143k exact; superseded by `n`). `t` did not
build after the interval-wrap fix (`b47b118`: room (3,3) `r4sxht` out of
memory in the kernel walk). plans/lessons.md has the measurements.

## h: held buttons unknown (`HeldPrecision`) - CURRENT, every object room

- **Widens**: the player's `p_jump`/`p_dash` (the previous frame's buttons,
  used only to detect a press) to unknown. Output: the
  uniform `AV::UBool` through the widen list (out of the per-lane key). Input:
  `widen::fork_held_inputs`, one 2-way `SplitInt` fork per trail over the
  literal [0, 1], no validity (every block writes them unknown).
- **Soundness**: a held button may retrigger (a ground jump then a wall jump
  on consecutive frames); over-approximation, refuted by the concrete
  count-up. The same widening with nothing exact behind it was rejected twice
  (2026-08-06, 2026-08-16).
- **Measured**: room (1,0) level 0 3.8x fewer states at f70, first win
  unchanged (f89), OPTIMAL 99 in 1:38. Room (2,0) f68: 50.7M -> 13.3M states,
  81 s -> 16 s a frame, 28.7 -> 8.7 GB. A constant factor, not a slower curve.
- **Later**: dominance (drop a held twin whose released twin is visited).

## f: the fly fruit unknown (`FruitPrecision`) - CURRENT for fruit rooms

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
  bodies. On the old finer rem rungs it merged nothing (the undecided
  `collide(player)` forks collected / not) and fanned out in room (6,0) (331
  trace states, cap 256). Collected-or-not after it flew away stays two
  shapes by design (a collect refills the dash). Not yet run through the arc
  search.

## n: everything abstract except where the player overlaps (`FloorsPrecision::Near`) - CURRENT, the object default

- **Widens**: each fall floor's `state` to the interval [0, 2]
  (`runtime2::FLOOR_STATE_RANGE`) with `collideable` DERIVED as `state ~= 2`
  (the cart keeps them in step), exact only where the player overlaps the
  floor at the frame's end (`widen::widen_near_floors`); the countdowns (floor
  `delay`, balloon `timer`, spring `delay`/`hide_in`/`hide_for`) the unknown
  number (`AV::UNum`, since `b47b118`; before it the whole 16.16 interval,
  which the kernels' interval subtraction wrapped); the other phases
  (`widen::phase_paths`: spring `spr`, balloon `spr` and its bob `y` as
  `start +- 2`) their ranges. A widened object's
  update is then "maybe X" (maybe bounce, maybe refill) and nothing else. An
  overlapped floor stores the cart's invariant (hidden), owed per lane
  (`09505c7`); in the split frame a floor the player's `is_solid` can reach
  keeps its computed `collideable` mid-frame (`widen::PLAYER_PROBE`).
- **Soundness**: `diag-project` checked every row of a room (7,0) `t` tree
  (7.88M, steps 1-96) projects onto a row of the `n` tree. The results were computed before the
  interval-wrap fix (`549ecf5`, `b47b118`); two rooms re-run after it
  reproduce their counts exactly, the rest were not re-run
  (plans/results.md, "Caveats").
- **Measured**: room (7,0) step 108: `b` 20.2M, `n` 3.4M, `t` 2.8M states
  before the join-bug fix; after `7f6b96e` (the boolean join of an undecided
  `collideable` threw away the player's overlap test, so every collision
  near a floor read "maybe") `n` level 0 reached step 168 at 15.4M states max,
  22.7 GB. Room (0,1) level 0 at f100: 14.8M states / 14 GB, against 40.0M /
  30 GB with the balloon's `spr` exact.
- **On the ladder** (before 2026-10-05) it was kept through a few rem rungs
  (room (0,1): the first exact-objects level peaked at 88k states a frame
  after `r1sxhn`, 26.5M straight after level 0). With the arcs it is the
  level-0 option of every object room, refuted by the count-up: room (5,3)
  had its bound at 78 and the count-up refuted 78 in 7.2k concrete steps.

## p: moving platforms unknown (`PlatformsPrecision`) - kept, but its rooms cannot run the search yet

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
  (level 0: 22.6M). The arcs refuse a frame where the player moves twice (a
  platform carrying it), so the platform rooms ((6,0), (2,1), (1,2)) cannot
  run the search yet.

## Widened at every level (exact, or a stated caveat)

- **The balloon's phase** (`widen::canon_balloon_offset`): `offset` is read
  only through `sin`, and `sin` of a full period is [-1, 1], so it is stored
  as the canonical [0, 1) with a full-period premise. EXACT: room (5,0)
  reaches the same 1,374,280 balloon-free states through f50 in 296k rows
  against 1.42M (it had stopped all cross-frame dedup: `offset` advanced 0.01
  every frame).
- **`rnd`** is an interval at every level (decided with Philippe
  2026-09-20); the count-up takes every leaf of a frame that forks on it. A
  refutation holds for every draw; a confirmation means "some draw wins". The witness is replayed on PICO-8 per
  seed (`pico8_diff/replay.py --balloon-seeds`, the TAS file's header fixes
  each balloon's offset); several witnesses exit only for some seeds
  (plans/results.md).
- **`widen::ABSENT_AS_ZERO`**: a fall floor gains `delay` only when it first
  breaks, which made "which floors ever broke" 12 bits of the heap shape. A
  missing `delay` is written as 0 at every level: the cart only does `delay -
  1`, `delay <= 0` with it, each a runtime error on nil, and
  `cart::check_absent_fields` refuses a cart that reads it any other way.
  Philippe: a hack. Replacement: a real nil-or-number field (a per-row
  presence tag); at a widened level the unknown can cover nil.
- **The region key** (`RegionGrid`, not a widening): kernels are specialized on
  the player being in one 16 px square with speed in [-6, 6], guarded per
  lane; a lane outside declines.
