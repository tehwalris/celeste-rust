# The level flags: what each widens, why it is sound, what refutes it

A level is `celeste_engine::widening::Level { held, fruit, floors_near,
platforms }`, written `r0sx[h][f][n][p]` (`Level::parse`): `r0sxhn` = held
buttons unknown, fall floors (and every object's phase) widened except where
the player overlaps. The `r0sx` prefix says what every level shares: the
remainder widened at the boundary and tracked exactly by the arcs, the speed
exact. A search runs one level, or the OBJECTS LADDER (`--level
r0sxhn,r0sxh`: each finer level filtered by the coarser one's arc-marked
nodes; plans/architecture.md "The search"). The rem rungs were deleted on
2026-10-05.

**Every widening is one entry of ONE table** (`celeste_engine::widening::
TABLE`, plans/widen-table.md): per entry its flag (every level, or `h`, `f`,
`n`, `p`), its objects, per slot what it STORES, how the frame READS it,
whether it is EXACT or an OVER-APPROXIMATION, and why. A level is the set of
entries its flags turn on. The tracer interprets the table in the graph
(`trace::widen::widen`, `read_inputs`), the block side over columns
(`Rt2::widen_to`); neither has a widening the table does not name. The
table below is GENERATED from it (`widening::render_markdown`) and a test
keeps the two equal (`the_abstractions_doc_lists_the_table`;
`CELESTE_REGENERATE=1` rewrites this copy).

## The rules every flag obeys

- **Never widen a field without something exact that refutes it**
  (CLAUDE.md). The flags below are refuted by the concrete search (or by a
  finer level of the objects ladder, and then by the search): it runs
  the real game (the reference engine's concrete step, every input, every
  `rnd` leaf) and prunes by nothing but the arcs' exact winning sets, so a
  win the widening invented has no concrete path and the concrete search moves past
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
  onto the level for the concrete search's node lookup) must produce the same row,
  or the concrete search misses the node and prunes a real path - a wrong "no win
  within f". Both interpret the one table; `rewrite widen-check` (t1, every
  level's fixture) checks that the block side keeps every row the kernels
  stored and emit as it is, `ref-check` that it makes the kernels' row of a
  reference successor, `rewrite follow` a known solution against a tree.
- **The reference engine as a frame step refuses `f` and `p`** (no reference
  form for their ranges and worlds; `h` and `n` it forks per path). The
  concrete search's concrete steps are exact whatever the level.

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

## The table

<!-- THE WIDENING TABLE (generated: celeste_engine::widening::render_markdown; do not edit) -->
| entry | level | objects | slot | stored | read as | kind |
|---|---|---|---|---|---|---|
| held buttons | `h` | `player` | `p_jump` | the unknown boolean | both ways, every lane | over |
|  |  |  | `p_dash` | the unknown boolean | both ways, every lane |  |
| player remainder | every | `player` | `rem.x` | [-0.5, 0.49998]; owed: inside (per lane) | as stored | over |
|  |  |  | `rem.y` | [-0.5, 0.49998]; owed: inside (per lane) | as stored |  |
| dash effect | every | `player` | `dash_effect_time` | max(v, 0) | as stored | exact |
| strawberry bob | every | `fruit` | `y` | `start` +- 2.5; owed: inside (per lane) | as stored | over |
|  |  |  | `off` | one period [0, 39] | as stored |  |
| timers | every | globals | `frames` | 0 (dead) | as stored | exact |
|  |  |  | `seconds` | 0 (dead) | as stored |  |
|  |  |  | `minutes` | 0 (dead) | as stored |  |
|  |  |  | `deaths` | 0 (dead) | as stored |  |
| key sprite | every | `key` | `spr` | 8 (dead) | as stored | exact |
|  |  |  | `flip.x` | false (dead) | as stored |  |
| fly fruit | `f` | `fly_fruit` | `step` | the unknown number | the unknown number | over |
|  |  |  | `y` | the unknown number | the unknown number |  |
|  |  |  | `spd.y` | [-3.5, 0.5]; owed: inside (proved on literal arms, else per lane) | the literal; owed: inside |  |
|  |  |  | `rem.y` | [-0.5, 0.49998]; owed: inside (proved on literal arms, else per lane) | the literal; owed: inside |  |
|  |  |  | `fly` | the unknown boolean | an undecided atom |  |
| fall floor countdown | `n` | `fall_floor` | `delay` (may be absent) | the unknown number | the unknown number | over |
| balloon countdown | `n` | `balloon` | `timer` | the unknown number | the unknown number | over |
| spring phase | `n` | `spring` | `spr` | [0, 19]; owed: inside (proved statically, else per lane) | as stored | over |
|  |  |  | `delay` (may be absent) | the unknown number | the unknown number |  |
|  |  |  | `hide_in` | the unknown number | the unknown number |  |
|  |  |  | `hide_for` | the unknown number | the unknown number |  |
| balloon phase | `n` | `balloon` | `spr` | [0, 22]; owed: inside (proved statically, else per lane) | as stored | over |
|  |  |  | `y` | `start` +- 2; owed: inside (proved statically, else per lane) | as stored |  |
| near fall floor | `n` | `fall_floor` | `state` | [0, 2]; owed: inside (proved statically, else per lane) | by the hook | over, hook `NearFloor` |
|  |  |  | `collideable` | the unknown boolean | by the hook |  |
| platform path | `p` | `platform` | `x` | [-16, 128]; owed: inside (proved statically, else per lane) | by the hook | over, hook `PlatformInputs` |
|  |  |  | `last` | `x`'s; owed: equal to `x` | by the hook |  |
| platform remainder | `p` | `platform` | `rem.x` | [-0.5, 0.49998]; owed: inside (proved statically, else per lane) | by the hook | over, hook `PlatformInputs` |
| balloon offset | every | `balloon` | `offset` (may be absent) | a full period as [0, 0.99998]; owed: the width | as stored | exact |
| absent fall floor delay | every | `fall_floor` | `delay` (may be absent) | 0 where absent | as stored | exact |
| absent spring delay | every | `spring` | `delay` (may be absent) | 0 where absent | as stored | exact |

- **held buttons** (over-approximation). The previous frame's buttons (read only to detect a press) unknown, each read both ways: a held button may re-trigger (a ground jump then a wall jump on consecutive frames). Refuted by the CONCRETE SEARCH.
- **player remainder** (over-approximation). The sub-pixel remainder, to the whole [-0.5, 0.5) at the boundary (the region's `Restrict` bounds the input too). Refuted by the ARCS: every recorded edge carries the frame's exact transfer of the remainder, and the backward over them is exact in it.
- **dash effect** (EXACT). `dash_effect_time` decrements every frame and is read only as `dash_effect_time > 0`, so every value <= 0 behaves alike and `max(v, 0)` keeps every read.
- **strawberry bob** (over-approximation). A live strawberry's bob forgotten, `off` and `y` TOGETHER (one without the other is a row no level has). `off` is a nonnegative integer read only as `sin(off / 40)`, periodic in it with period 40 bit-exactly (`pico8_num::test_pico8_sin_period_40_bit_exact`), so one period [0, 39] covers every phase; `y` is the band `start +- 2.5` the bob stays in, owed. The fruit's collision reads `y`, so a collect becomes possible at any phase: refuted by the CONCRETE SEARCH.
- **timers** (EXACT). DEAD fields: the cart reads `frames`, `seconds`, `minutes` and `deaths` only to update each other and the key's sprite (below), never in anything a state's successors or the search's tests depend on, so every value of them behaves alike.
- **key sprite** (EXACT). DEAD fields: the key's `spr` (from `frames`) and `flip.x` (from `spr`) are read only by each other and the draw, so every value behaves alike.
- **fly fruit** (over-approximation). While waiting the fly fruit's `step += 0.05` and bob mean no fruit state recurs across frames; once flying, rows split by the frame of the dash. `step`/`y` the unknown number, `spd.y`/`rem.y` their ranges (read as the literals, owed per lane on the raw input), `fly` an unknown boolean. Refuted by the objects ladder and the CONCRETE SEARCH.
- **fall floor countdown** (over-approximation). The countdown, the unknown number (never the interval [MIN, MAX]: `delay - 1` of it would overflow, `Op::NoWrap`). The cart only decrements it and compares it with 0. Refuted by the CONCRETE SEARCH (or the ladder's exact level).
- **balloon countdown** (over-approximation). As the fall floor's countdown.
- **spring phase** (over-approximation). The spring's sprite (0 hidden, 18 ready, 19 compressed) its range, so `spr == 18` splits like a floor's state; its countdowns the unknown number. Its update becomes "maybe bounce". Refuted by the CONCRETE SEARCH.
- **balloon phase** (over-approximation). The balloon's sprite (0 popped, 22 present) its range and its `y` the bob band `start +- 2` (the bob runs only when `spr == 22`, and an exact `y` would give every state a twin). Its update becomes "maybe refill the dash". Refuted by the CONCRETE SEARCH.
- **near fall floor** (over-approximation). Each fall floor's `state` [0, 2] and `collideable` unknown, EXCEPT where a player certainly overlaps the floor at the frame's end (there the cart's invariant, hidden, owed; with `p` the computed value). The input derives `collideable` as `state ~= 2` (the cart keeps them in step). Refuted by the CONCRETE SEARCH or the ladder `r0sxhn,r0sxh`.
- **platform path** (over-approximation). Every moving platform's `x` the interval of its whole path [-16, 128] (the wrap keeps it inside, proved on the traced select or owed), `last` the same node (`last == x` at every frame boundary, checked). The worlds (`concrete::platform_worlds`) keep the platforms mutually consistent (`verify::Points`). Refuted by the CONCRETE SEARCH (the reference engine refuses `p`).
- **platform remainder** (over-approximation). A platform's `rem.x` the whole remainder, read as the literal (owed per lane): a pixel of slack at this level. Refuted by the CONCRETE SEARCH.
- **balloon offset** (EXACT). The balloon's `rnd` phase is read only through `sin(offset)`, and `sin` of a full period is [-1, 1] whatever the interval's ends, so a full-period interval is stored as the canonical [0, 1) (a point, a seeded phase, is left). Room (5,0) reaches the same balloon-free states with it.
- **absent fall floor delay** (EXACT). A fall floor gains `delay` only when it first breaks, which made "which floors ever broke" heap shape. A missing `delay` is written as 0: the cart can tell nil from 0 only by raising (`delay - 1`, `delay <= 0` halt PICO-8 on nil), and `cart::check_absent_fields` refuses a cart that reads it any other way. Exact on every execution that does not raise.
- **absent spring delay** (EXACT). As the fall floor's: a spring gains `delay` when it first breaks.
<!-- END OF THE WIDENING TABLE -->

"stored" is what the frame's end writes and the block side writes; "owed"
is checked per lane where the frame cannot prove it (the widening's own
error, `widen::SlotErrors`; on blocks an assertion); "read as" is the input
side. Two things a slot cannot say are HOOKS, named in their entries:

- **`NearFloor`** (`n`): the stored value depends per lane on ANOTHER
  object's fields (the player's position overlapping the floor at the
  frame's end), the overlapped case stores the cart's invariant (owed) or,
  with `p`, the computed value; mid split frame `collideable` stays computed
  in the player's probe window; the input DERIVES `collideable` from
  `state`. `trace::widen::widen_near_floors`/`read_near_floors`,
  `Rt2::widen_near_floors`.
- **`PlatformInputs`** (`p`): the platforms' input is per WORLD - which world
  platform an object is (its constant `y`, `dir`), `x` restricted to the
  path and recorded as that world's cell for `verify::Points`, `spd.x`
  bound by the worlds (with no player, the literal of its one speed). Their
  stored values are plain entries (`platform path`, `platform remainder`).
  `trace::widen::platform_inputs`.

Not in the table, and why: `rnd` (an interval at every level, decided with
Philippe 2026-09-20: not a field the boundary widens but the cart's own draw;
below); the region key (a kernel's dispatch, guarded per lane, not a stored
value); the bounds a kernel assumes (`Op::Restrict`, the pins: assumptions on
an input, checked per lane, never a stored value); the concrete search's
seeded-balloon view (`arc_dp::lookup_view`: a seeded phase mapped onto the
tree's `rnd` interval, the `rnd` caveat's).

## The remainder: arcs, at every level

- **Widens**: the player's `rem.x`/`rem.y` to the whole [-0.5, 0.5) at the
  boundary (the table's "player remainder"), with its containment the widening's own
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

## h: held buttons unknown - CURRENT, every object room

- **Widens**: the player's `p_jump`/`p_dash` (the previous frame's buttons,
  used only to detect a press) to unknown. Output: the
  uniform `AV::UBool` through the widen list (out of the per-lane key). Input:
  `Input::BothWays`, one 2-way `SplitInt` fork per trail over the
  literal [0, 1], no validity (every block writes them unknown).
- **Soundness**: a held button may retrigger (a ground jump then a wall jump
  on consecutive frames); over-approximation, refuted by the concrete
  concrete search. The same widening with nothing exact behind it was rejected twice
  (2026-08-06, 2026-08-16).
- **Measured**: room (1,0) level 0 3.8x fewer states at f70, first win
  unchanged (f89), OPTIMAL 99 in 1:38. Room (2,0) f68: 50.7M -> 13.3M states,
  81 s -> 16 s a frame, 28.7 -> 8.7 GB. A constant factor, not a slower curve.
- **Later**: dominance (drop a held twin whose released twin is visited).

## f: the fly fruit unknown - CURRENT for fruit rooms

- **Widens**: the fly fruit's `step`, `y` to the unknown number
  (`Op::UnknownNum`: stays unknown under every operation, never raises - a
  full-range interval raises at the 16.16 extremes); `spd.y` to [-3.5, 0.5]
  and `rem.y` to [-0.5, 0.5) as literals, checked per row; `fly` an unknown
  boolean (the table's "fly fruit").
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
  shapes by design (a collect refills the dash).
- **With `n` (`r0sxhfn`, 2026-10-08)**: the fruit's atoms refuse merges, and
  at `n` the countdowns are the unknown number too, so every floor's
  `delay <= 0` was a fruit-like atom and kept its two paths apart (room
  (3,0): 8192 trace states after the spawn frame's `foreach`; the level did
  not build). A comparison READING a countdown field (`<name>.delay`,
  `.timer`, `.hide_in`, `.hide_for`: `widen::countdown_fields`, from the table; the
  interpreter's hint) mints a COUNTDOWN atom instead, three-valued as at `n`
  without the fruit: merges select on it, the split pass splits what a row
  stores, an escape is no fork, and the independent joins refuse it (a hull
  would lose `state`/`collideable`'s correlation). And `may_answers` reads
  an equality's ends unless an OPERAND reads an unknown (it used to give up
  whenever the frame had any): a hidden floor's exact `state == 2` was
  "both ways", and its solid side owed `collideable` false with the player
  inside - a coverage gap at f42. Refuted like every flag, by the objects
  ladder (`r0sxhfn,r0sxhn,r0sxh`) and the concrete search. `rewrite
  arc-check` runs at an `f` level since 2026-10-09 (`RefEngine::step_at`:
  the reference steps the probe row projected onto the level with its
  unknowns forked as the kernels fork them - `fly` both ways, every
  comparison on the unknown `y`/`step` both ways - instead of reading
  their placeholders, which missed the fruit flying off or being collected:
  room (6,2) 100% `r0sxhf` f1-f40, 351 of 1214 inside probes bad before, 0
  after). Two pins keep it affordable, both exact for a successor projected
  onto the level and each checked against the unpinned reference by a test:
  the fly fruit's `spd.y`/`rem.y` pinned to 0 (they only move the unknown
  `y`, and the projection widens them again; ~140 leaves a step to ~9), and
  at `n` a floor ISOLATED from the player (20 px Chebyshev, no spring
  near; the player's per-frame move <= 8 checked on every leaf) pinned to
  state 0 (unpinned the floors' states are a product: room (3,0)'s 12
  floors passed 1M paths a step). Room (3,0) 100% `r0sxhfn` f1-f45: 0 bad
  (1460 inside, 622 outside; 65 min single-threaded); `--fault` makes both
  rooms fail. `rewrite follow` with the community route agrees with the
  `r0sxhfn` tree's keys through the spawn and the fruit taking off (f35)
  to the end of a tree built to f46. Room (3,0) nodiag: bound 76 against
  the optimum 93 (plans/nodiag.md).

## n: everything abstract except where the player overlaps - CURRENT, the object default

- **Widens**: each fall floor's `state` to the interval [0, 2]
  (`runtime2::FLOOR_STATE_RANGE`) with `collideable` DERIVED as `state ~= 2`
  (the cart keeps them in step), exact only where the player overlaps the
  floor at the frame's end (`widen::widen_near_floors`); the countdowns (floor
  `delay`, balloon `timer`, spring `delay`/`hide_in`/`hide_for`) the unknown
  number (`AV::UNum`, since `b47b118`; before it the whole 16.16 interval,
  which the kernels' interval subtraction wrapped); the other phases
  (the table's "spring phase", "balloon phase": spring `spr`, balloon `spr` and its bob `y` as
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
  level-0 option of every object room, refuted by the concrete search: room (5,3)
  had its bound at 78 and the concrete search refuted 78 in 7.2k concrete steps.
  Room (7,0) is where it is too loose for the search alone (bound 80, the
  optimum 84, the search's region 6.5x larger per frame of slack): there the
  ladder `r0sxhn,r0sxh` (bound 84, the witness in 292 steps).

## p: moving platforms unknown - CURRENT for the platform rooms

- **Widens**: every platform's `x` and `last` to the whole path [-16, 128],
  `rem.x` the literal [-0.5, 0.5). `last == x` holds at every frame boundary
  and is bound as symbolic identity at the input (checked on every traced
  output: the output `last` is the output `x`'s node); the carry `(b + c) - b
  = c` cancels exactly in the tracer's subtraction; the wrap keeps x inside
  the path (proved on the traced select, else a per-lane error). At the
  input `x` is `Restrict`ed to the path and `spd.x` to its worlds' range;
  the literal read for `rem.x` owes the lane's value inside it (per-lane
  errors, plans/architecture.md "A bound is a node").
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
  (level 0: 22.6M).
- **The arcs** (2026-10-08): the carry is `move_x` (whole pixels, no split);
  a carry blocked by a wall sets `rem.x = 0` before the player's own move,
  decoded as a constant (plans/architecture.md "Arcs"). There never was a
  second player split.
- **With `n` (the near level), broken since `b47b118`** (2026-10-03, the
  countdowns made the unknown number) and repaired 2026-10-08: `p` turns on
  the "unknowns" rules (`Symbolic::unknowns`: a merge on a condition reading
  an atom is two successors, an escaping atom is a fork), and they took the
  countdowns' `delay <= 0` atoms too. Room (2,1) `r0sxhnp` traced 354 states
  at the spawn (limit 256; `r0sxhn`: 5). Now (`Symbolic::countdown_atom`) a
  comparison of the unknown number is a COUNTDOWN atom wherever no fly
  fruit is unknown (every unknown number is then a countdown; with the fruit
  unknown, one reading a countdown field: `f` above): three-valued
  as at `n` alone - no merge refusal, no independent join on it alone, no
  escape fork (an escape fork for a platform atom enumerates the countdown
  atoms it reads too, so its restriction reads none). Then the near floors'
  hidden invariant failed: room (2,1) f26 declined every lane of the first
  player frame's bucket (the owed `apart or not collideable` of the floor
  under the spawn). With `p` an overlapped floor now stores its COMPUTED
  `state`/`collideable`, owing nothing (exact; `Rt2::widen_to` keeps an
  overlapped floor's values). Why `p` reaches an outcome with the
  overlapped floor solid is not proven. The likely reason: a merge `p`
  refuses leaves two outcomes whose live guards read a platform atom
  (three-valued, and an unknown `live` reads as live), so an outcome dead on
  a lane is evaluated there and its owe fails; at `n` alone the merge
  selects and the split pass decides it per lane. Storing the computed value
  turns that into spurious states, which the concrete search refutes.
  Since `p` met the equality's per-operand may-answers (`f`, "With `n`",
  merged 2026-10-08): room (2,1) `r0sxhnp` keeps the same states through
  the spawn (f24 21, f25 198) and fewer after (f26 938 against 1,266, f40
  322,059 against 672,322; with only that change reverted, every count and
  the ckhash equal the old ones); `--win-at 38,99 --to 45` still gives arc
  optimum 37 and the same 37-input witness. The `state == 2` "both ways"
  that change removed is the same kind of thing as the solid overlapped
  floor above; whether it was the cause is not tested.

## `rnd`, and the region key

- **`rnd`** is an interval at every level (decided with Philippe
  2026-09-20); the concrete search takes every leaf of a frame that forks on it. A
  refutation holds for every draw; a confirmation means "some draw wins". The witness is replayed on PICO-8 per
  seed (`pico8_diff/replay.py --balloon-seeds`, the TAS file's header fixes
  each balloon's offset); several witnesses exit only for some seeds
  (plans/results.md).
- **The region key** (`RegionGrid`, not a widening): kernels are specialized on
  the player being in one 16 px square with speed in [-6, 6], guarded per
  lane; a lane outside declines.
- The balloon's phase, the absent-as-zero fields, the strawberry's bob, the
  timers, the key's sprite and the dash effect are table entries (above).
  Before the table the bob and the timers were undocumented (SPEC annex D
  tr-6, tr-7); the timers and the key's sprite are EXACT as DEAD fields,
  checked by `dead_fields_are_unread` (plans/widen-table.md).
