# The level flags: what each widens, why it is sound, what refutes it

A level is `celeste_engine::widening::Level { fruit, floors_near }`,
written `r0sx[f][n]` (`Level::parse`). The `r0sx`
prefix is what every level shares: the remainder widened at the boundary
and tracked exactly by the arcs, the speed exact, and the buttons' memory
widened (the held buttons and the jump buffer: plans/correctness.md C7; the
former `h` flag, which a spec now refuses). A search runs one level, or the
OBJECTS LADDER (`--level r0sxn,r0sx`: each finer level filtered by the
coarser one's reached nodes; plans/architecture.md "The search").

| spec | use |
|---|---|
| `r0sx` | rooms without objects |
| `r0sxn` | the object default: springs, fall floors, balloons, key, chest |
| `r0sxf`, `r0sxfn` | a fly fruit |
| `r0sxn,r0sx` | the ladder, where level 0's bound is loose |

Moving platforms have no flag: they are EXACT at every level, keyed by the
first platform's fields where a room has several ("Moving platforms"
below). The `p` flag that widened them was removed on 2026-10-25
(plans/correctness.md C13); a spec holding `p` is refused.

**Every widening is one entry of ONE table** (`celeste_engine::widening::
TABLE`): per entry its flag (every level, or `f`, `n`), its
objects, per slot what it STORES, how the frame READS it, whether it is
EXACT or an OVER-APPROXIMATION, and why. The tracer interprets the table in
the graph (`trace::widen`), the block side over columns (`Rt2::widen_to`);
neither has a widening the table does not name, and `rewrite check widen`
checks that they agree. The table below is GENERATED
(`widening::render_markdown`) and a test keeps it equal
(`the_abstractions_doc_lists_the_table`; `CELESTE_REGENERATE=1` rewrites it).

## The rules every flag obeys

- **Never widen a field without something exact that refutes it**
  (CLAUDE.md). The flags are refuted by the concrete search (or a finer
  level of the ladder, then the search): it runs the real game and prunes
  by nothing but the arcs' exact winning sets, so a win the widening
  invented has no concrete path. The exceptions are the EXACT entries
  below (they lose nothing) and `rnd` (a stated best-case caveat).
- **A widening replaces a value only by something visibly containing it**:
  a range that holds it, or unknown. Never by another value.
- **No bands justified from outside the frame**: a widened range must be a
  fixed point of the frame's own computation, owed per lane where not
  trivially total (the widening's own error, `widen::SlotErrors`).
- **An uncollected object is one row**, its fields intervals, not several
  rows because `move`'s `__split_by_flr` forks it.
- **Projection must agree**: the concrete search projects a concrete state
  onto the level (`Rt2::widen_to`) to look it up; if that differs from the
  kernels' row, the search misses the node and prunes a real path.
- **Fork after the frame, never eagerly** (Philippe, 2026-09-21): the
  eager point split made every outcome's `live` read every comparison
  (room (6,0) `r0sxp`: 2^70 bodies an outcome). Forks happen only where an
  undecided select survives folding (`split::split_undecided_selects`).
- **The reference engine as a frame step** forks the buttons' memory, `f`
  and `n` per path. The concrete search's own steps are exact whatever the
  level: the buttons and the jump buffer as the cart keeps them.

## The table

<!-- THE WIDENING TABLE (generated: celeste_engine::widening::render_markdown; do not edit) -->
| entry | level | objects | slot | stored | read as | kind |
|---|---|---|---|---|---|---|
| held buttons | every | `player` | `p_jump` | the unknown boolean | both ways, every lane | over |
|  |  |  | `p_dash` | the unknown boolean | both ways, every lane |  |
|  |  |  | `jbuffer` | the unknown number, a countdown | the unknown number |  |
| player remainder | every | `player` | `rem.x` | [-0.5, 0.49998]; owed: inside | as stored | over |
|  |  |  | `rem.y` | [-0.5, 0.49998]; owed: inside | as stored |  |
| dash effect | every | `player` | `dash_effect_time` | max(v, 0) | as stored | exact |
| strawberry bob | every | `fruit` | `y` | `start` +- 2.5; owed: inside | as stored | over |
|  |  |  | `off` | one period [0, 39] | as stored |  |
| timers | every | globals | `frames` | 0 (dead) | as stored | exact |
|  |  |  | `seconds` | 0 (dead) | as stored |  |
|  |  |  | `minutes` | 0 (dead) | as stored |  |
|  |  |  | `deaths` | 0 (dead) | as stored |  |
| key sprite | every | `key` | `spr` | 8 (dead) | as stored | exact |
|  |  |  | `flip.x` | false (dead) | as stored |  |
| sound timers | every | globals | `sfx_timer` | 0 (dead) | as stored | exact |
|  |  |  | `music_timer` | 0 (dead) | as stored |  |
| fly fruit sound delay | every | `fly_fruit` | `sfx_delay` | 0 (dead) | as stored | exact |
| screen shake | every | globals | `shake` | 0 (dead) | as stored | exact |
| fly fruit | `f` | `fly_fruit` | `step` | the unknown number | the unknown number | over |
|  |  |  | `y` | the unknown number | the unknown number |  |
|  |  |  | `spd.y` | [-3.5, 0.5]; owed: inside | the literal; owed: inside |  |
|  |  |  | `rem.y` | [-0.5, 0.49998]; owed: inside | the literal; owed: inside |  |
|  |  |  | `fly` | the unknown boolean | an undecided atom |  |
| fall floor countdown | `n` | `fall_floor` | `delay` (may be absent) | the unknown number, a countdown | the unknown number | over |
| balloon countdown | `n` | `balloon` | `timer` | the unknown number, a countdown | the unknown number | over |
| spring phase | `n` | `spring` | `spr` | [0, 19]; owed: inside | as stored | over |
|  |  |  | `delay` (may be absent) | the unknown number, a countdown | the unknown number |  |
|  |  |  | `hide_in` | the unknown number, a countdown | the unknown number |  |
|  |  |  | `hide_for` | the unknown number, a countdown | the unknown number |  |
| balloon phase | `n` | `balloon` | `spr` | [0, 22]; owed: inside | as stored | over |
|  |  |  | `y` | `start` +- 2; owed: inside | as stored |  |
|  |  |  | `offset` | one period [0, 0.99998] | as stored |  |
| near fall floor | `n` | `fall_floor` | `state` | [0, 2]; owed: inside | by the hook | over, hook `NearFloor` |
|  |  |  | `collideable` | the unknown boolean | by the hook |  |
| balloon offset | every | `balloon` | `offset` (may be absent) | a full period as [0, 0.99998]; owed: the width | as stored | exact |
| absent fall floor delay | every | `fall_floor` | `delay` (may be absent) | 0 where absent | as stored | exact |
| absent spring delay | every | `spring` | `delay` (may be absent) | 0 where absent | as stored | exact |

- **held buttons** (over-approximation). THE BUTTONS' MEMORY, at every level (plans/correctness.md C7): the previous frame's buttons (read only to detect a press) unknown, each read both ways, and the jump buffer (`jbuffer`, a press kept for 4 updates) the unknown countdown: a jump may fire on any update its grace or a wall allows, which covers every buffered and re-triggered jump (a ground jump then a wall jump on consecutive updates). Refuted by the CONCRETE SEARCH, which runs the cart's buttons exactly.
- **player remainder** (over-approximation). The sub-pixel remainder, to the whole [-0.5, 0.5) at the boundary (the region's `Restrict` bounds the input too). Refuted by the ARCS: every recorded edge carries the frame's exact transfer of the remainder, and the backward over them is exact in it.
- **dash effect** (EXACT). `dash_effect_time` decrements every frame; the traced cart reads it only to decrement it (and the original cart only as `dash_effect_time > 0`), so every value <= 0 behaves alike and `max(v, 0)` keeps every read.
- **strawberry bob** (over-approximation). A live strawberry's bob forgotten, `off` and `y` TOGETHER (one without the other is a row no level has). `off` is a nonnegative integer read only as `sin(off / 40)`, periodic in it with period 40 bit-exactly (`pico8_num::test_pico8_sin_period_40_bit_exact`), so one period [0, 39] covers every phase (in doubles `sin` is not bit-periodic, but `off` feeds only `y`, and [0, 39] makes every `y` of the band); `y` is the band `start +- 2.5` the bob stays in, owed. The fruit's collision reads `y`, so a collect becomes possible at any phase: refuted by the CONCRETE SEARCH.
- **timers** (EXACT). DEAD fields: the cart reads `frames`, `seconds`, `minutes` and `deaths` only to update each other and the key's sprite (below), never in anything a state's successors or the search's tests depend on, so every value of them behaves alike. Checked: traced as free input cells along room (4,0)'s and (6,2)'s 100% routes and room (5,2)'s (the orb), no other stored field, guard, error or raise reads them, but for an error's overflow check of their own arithmetic (`minutes + 1`), which holds at the stored 0 (`trace::widen::tests::dead_fields_are_unread`).
- **key sprite** (EXACT). DEAD fields: the key's `spr` (from `frames`) and `flip.x` (from `spr`) are read only by each other and the draw, so every value behaves alike. Checked with the timers.
- **sound timers** (EXACT). DEAD fields: each read only to count itself down and to decide a call of the no-op `sfx`/`music`, so every value behaves alike. Checked with the timers.
- **fly fruit sound delay** (EXACT). DEAD field: read only to count itself down, then to call the no-op `sfx` and set the dead `sfx_timer`, so every value behaves alike. Checked with the timers.
- **screen shake** (EXACT). DEAD field: read only to count itself down and to call the no-op `camera` (with `rnd` offsets, unread), so every value behaves alike. Checked with the timers.
- **fly fruit** (over-approximation). While waiting the fly fruit's `step += 0.05` and bob mean no fruit state recurs across frames; once flying, rows split by the frame of the dash. `step`/`y` the unknown number, `spd.y`/`rem.y` their ranges (read as the literals, owed per lane on the raw input), `fly` an unknown boolean. Refuted by the objects ladder and the CONCRETE SEARCH.
- **fall floor countdown** (over-approximation). The countdown, the unknown number (never the interval [MIN, MAX]: `delay - 1` of it would overflow, `Op::NoWrap`). The cart only decrements it and compares it with 0. Refuted by the CONCRETE SEARCH (or the ladder's exact level).
- **balloon countdown** (over-approximation). As the fall floor's countdown.
- **spring phase** (over-approximation). The spring's sprite (0 hidden, 18 ready, 19 compressed) its range, so `spr == 18` splits like a floor's state; its countdowns the unknown number. Its update becomes "maybe bounce". Refuted by the CONCRETE SEARCH.
- **balloon phase** (over-approximation). The balloon's sprite (0 popped, 22 present) its range and its `y` the bob band `start +- 2` (the bob runs only when `spr == 22`, and an exact `y` would give every state a twin). Its update becomes "maybe refill the dash". The phase `offset` is read only as `sin(offset)` into `y`, so one period [0, 1) covers every `y` the bob makes (an unseeded `rnd` phase is that period already; a SEEDED one, which the float tools always set, would otherwise split on every undecided `spr == 22`: room (4,2)'s six balloons made f^6 states before the player). Refuted by the CONCRETE SEARCH.
- **near fall floor** (over-approximation). Each fall floor's `state` [0, 2] and `collideable` unknown, EXCEPT where a player certainly overlaps the floor at the frame's end (there the cart's invariant, hidden, owed). The input derives `collideable` as `state ~= 2` (the cart keeps them in step). Refuted by the CONCRETE SEARCH or the ladder `r0sxn,r0sx`.
- **balloon offset** (EXACT). The balloon's `rnd` phase is read only through `sin(offset)`, and `sin` of a full period is [-1, 1] whatever the interval's ends, so a full-period interval is stored as the canonical [0, 1) (a point, a seeded phase, is left). Room (5,0) reaches the same balloon-free states with it.
- **absent fall floor delay** (EXACT). A fall floor gains `delay` only when it first breaks, which made "which floors ever broke" heap shape. A missing `delay` is written as 0: the cart can tell nil from 0 only by raising (`delay - 1`, `delay <= 0` halt PICO-8 on nil), and `cart::check_absent_fields` refuses a cart that reads it any other way. Exact on every execution that does not raise.
- **absent spring delay** (EXACT). As the fall floor's: a spring gains `delay` when it first breaks.
<!-- END OF THE WIDENING TABLE -->

"stored" is what the frame's end writes; "owed" is checked per lane where
the frame cannot prove it (on blocks an assertion); "read as" is the input
side. What a slot cannot say is a HOOK:

- **`NearFloor`** (`n`): the stored value depends on ANOTHER object (the
  player overlapping the floor at the frame's end). Overlapped, it stores
  the cart's invariant (owed); mid split frame `collideable` stays computed
  in the player's probe window (`widen::PLAYER_PROBE`); the input derives
  `collideable` from `state`.

Not in the table: `rnd` (the cart's own draw, below); the region key (a
dispatch); the bounds a kernel assumes (`Restrict`, pins: assumptions on an
input, never a stored value); the concrete search's seeded-balloon view
(`concrete::lookup_view`, part of the `rnd` caveat).

## The remainder: arcs, at every level

- **Widens**: the player's `rem.x`/`rem.y` to the whole [-0.5, 0.5) at the
  boundary, its containment owed.
- **Refuted by**: nothing needs to. Every recorded edge carries the frame's
  exact transfer of the remainder, and the backward over them is exact.
- **The forward's hull** (2026-10-13): per state a box of two arcs holding
  every remainder it has been reached with, pushed from the start's exact
  point. It decides only which states the tree holds, how early, and which
  are expanded; the arcs and the concrete search refute what it
  over-admits. A test holds it between the exact reach and the
  whole-circle forward
  (`the_hull_forward_lies_between_the_exact_reach_and_the_widened_forward`).
- **History**: until 2026-10-05 the remainder was refined by rungs, which
  DRIFTED (a sliver row widened to a whole bucket, up to a bucket a frame
  per axis; plans/lessons.md). Every result before 2026-10-04 rests on the
  rungs; the arcs reproduce them.

## The buttons' memory (every level; the former `h`)

- **Widens**: `p_jump`/`p_dash` (the previous frame's buttons, read only to
  detect a press) to unknown, read both ways every lane, and the jump
  buffer `jbuffer` (a press kept for 4 updates, the original cart's) to the
  unknown number, a countdown (its `> 0` a countdown atom).
- **Soundness**: a held button may retrigger (a ground jump then a wall
  jump on consecutive frames), a buffered jump may fire on any update its
  grace or a wall allows. Refuted by the concrete search, which runs the
  cart's buttons and buffer exactly. The same widening with nothing exact
  behind it was rejected twice (2026-08-06, 08-16).
- **History**: the `h` flag until 2026-10-25, and the cart had no jump
  buffer (C7: the search missed real states, a buffered jump then a fresh
  one on consecutive updates). Since then every level widens the buttons;
  with the buffer back the sets are the former `h`'s exactly (every
  fixture's per-frame counts identical), and `h` in a spec is refused.
- **Measured**: room (1,0) level 0 3.8x fewer states at f70 than with the
  buttons exact, first win unchanged. Room (2,0) f68: 50.7M -> 13.3M
  states. A constant factor. With `jbuffer` exact instead, +224% to +344%
  states (the cart-fidelity prototype). The countdown splits a frame's
  bodies (+47%); its successors repeat across the jump button's classes,
  dropped at the kernel build (`asm_kernel::dup_refs`, alike outcomes
  from the same slots): the release budget +0.9..+1.5% against the cart
  without the buffer.

## f: the fly fruit unknown (fruit rooms)

- **Widens**: `step`, `y` to the unknown number (stays unknown under every
  operation, never raises); `spd.y` [-3.5, 0.5] and `rem.y` read as their
  literal ranges, owed; `fly` an unknown boolean.
- **Why**: while waiting, `step += 0.05` (inexact in 16.16) and the bob
  mean no fruit state recurs across frames; once flying, rows split by the
  frame of the dash. Room (3,0) f50: 3.6x fewer states.
- **Mechanisms it brought** (shared by the floors): literals
  stay literals; a merge no lane decides joins to a hull, one lane data
  decides stays two successors (`Interp::joins_independent`); a literal's
  `__split_by_flr` fragments rejoin at the call's return.
- **With `n`** (`r0sxfn`): a comparison reading a countdown field mints a
  COUNTDOWN atom, three-valued as at `n` alone (merges select on it, no
  escape fork); the fruit's atoms keep refusing merges. Equality reads its
  ends unless an operand reads an unknown. Refuted by the ladder
  (`r0sxfn,r0sxn,r0sx`) and the concrete search.
- **`check arcs` at `f`** steps the reference at the level with the
  kernels' forks (`ReferenceEngine::step_at`), under two exact pins, each checked
  against the unpinned reference: the fly fruit's `spd.y`/`rem.y` pinned
  to 0, and at `n` a floor isolated from the player (20 px, no spring near)
  pinned to state 0.

## n: everything abstract except where the player overlaps (the object default)

- **Widens**: each fall floor's `state` to [0, 2], `collideable` derived as
  `state ~= 2`, exact only where the player overlaps the floor at the
  frame's end (there the cart's invariant, owed). The countdowns (floor
  `delay`, balloon `timer`, spring `delay`/`hide_in`/`hide_for`) the
  unknown number, never [MIN, MAX] (`delay - 1` would overflow). The
  spring's `spr` and the balloon's `spr` and bob `y` their ranges. An
  object's update becomes "maybe bounce", "maybe refill".
- **The balloon, whole** (2026-10-16): `timer`, `spr`, `y` AND its phase
  `offset` (one period [0, 1)) are widened together. A seeded `offset`
  (`--balloon-seeds`, always under the float tools) used to stay exact and
  split on every undecided `spr == 22` (room (4,2)'s six balloons: 531,441
  states at f9 before the player). Sound: `offset` is read only as
  `sin(offset)` into `y`, which is stored as the bob band; every phase's
  `y` lies in it. Refuted by the concrete search, which runs the real
  seed, and by `r0sx`.
- **Soundness check**: `diag-project` (a diagnostic since deleted) found
  every row of a room (7,0) `t` tree projects onto a row of the `n` tree.
- **Measured**: room (7,0) step 108: `b` 20.2M, `n` 3.4M states. Room
  (0,1) f100: 14.8M against 40.0M with the balloon's `spr` exact. Where it
  is too loose for the concrete search alone (room (7,0): bound 80, the
  optimum 84), the ladder `r0sxn,r0sx` (bound 84).

## Moving platforms: exact, keyed by the first (no flag)

- **Exact at every level.** A platform is a CLOCK: `move` steps `rem.x` by
  `dir * 0.65` an update and nothing but its own update writes it, so every
  platform is a function of the updates since the room loaded, and no state
  merges across frames. Affordable in every platform room (below); `p`,
  which widened them, is gone (plans/lessons.md).
- **Keyed by the first platform** (`widening::CLOCKS`, `widening::key_deps`,
  `exact::Deps`): where a shape holds two or more platforms, the first
  one's `spd.x` LEADS the other `spd.x`s and its `rem.x` every other
  platform value cell (`x`, `last`, `rem`, `hitbox`, ...). Followers carry no
  key bits: their codes are recorded with each leader code and every row is
  CHECKED against that record wherever a key is formed (the boundary, a
  provisional key made final, the kernels' emission), a difference FATAL.
  Not a widening: two states differing in a follower can never share a key
  unnoticed. `rem.x` alone repeats once, across the room's first update
  (`move` reads `spd.x` before that update sets it, so the update changes
  only `spd.x`): the kernel check caught it at step 2. Without the
  dependencies room (6,0)'s ten platforms need more than 127 key bits by
  step 17; with them its keys take 42. A room with one platform ((2,1),
  (1,2)) declares nothing.
- **The arcs**: the carry is `move_x` (whole pixels, no split); a carry
  blocked by a wall sets `rem.x = 0` before the player's move, decoded as
  a constant. There is no second player split.
- **Measured** (2026-10-25, release, `--prefer` the stored witness, every
  result kept): room (6,0) split `r0sxf` any% 70 in 11 min, 18.2 GB (1.61G
  visited, 42 key bits); (2,1) `r0sxn` 82 in 6 min, 9.6 GB; (1,2) `r0sxn`
  103 in 8 min, 6.7 GB; nodiag alike (plans/results.md, "The platform rooms
  without `p`").

## `rnd`, and the region key

- **`rnd`** is an interval at every level (decided with Philippe
  2026-09-20); the concrete search takes every leaf. A refutation holds for
  every draw; a confirmation means "some draw wins". Each witness is
  replayed on PICO-8 per seed (`rewrite pico8 replay --balloon-seeds`);
  several exit only for some. A comparison that straddles an `rnd`
  interval forks without narrowing it, so two comparisons of one draw fork
  independently: combinations no single draw gives are admitted (the same
  best-case caveat). A chest is never seeded in the search: its berry's `x` stays the
  `rnd` interval and the runner picks the seed after.
- **The region key** (`RegionGrid`, not a widening): kernels are
  specialized on the player being in one 16 px square with speed in
  [-6, 6], checked per lane; a lane outside declines.

## Deleted levels

Position buckets (`x2`, `y2`), speed buckets (`s16`, ...), floors fully
unknown (`b`), timers only (`t`) and the remainder rungs (`r1sx`..`r15sx`,
`rxsx`) are gone (2026-10-04/05; `r1sx`/`rxsx` are refused). Moving
platforms unknown (`p`: `x`, `last` the whole path, `rem.x` the whole
remainder, player-vs-platform decided per "platform world" from a no-input
reference run) went on 2026-10-25 (plans/correctness.md C13; `p` is
refused).
plans/lessons.md has the measurement that killed each.
