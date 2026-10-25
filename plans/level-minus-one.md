# Level -1: a position-only cost-to-go table

A sound LOWER BOUND `d` on the frames to the exit from every (heap shape,
player cell), built from the same traced frames the kernels come from, used
as a FILTER on the forward under a horizon H: a push into a cell at frame
t is dropped when `t + d > H`. Code: `src/search/level_minus_one.rs` (its
one builder, `table`), `frame::TreeL1`, `UnitSink`'s level -1 drop.

```bash
# as a filter inside a search (H the horizon, S the speed bound in px/frame)
./safe-run.sh -- ./target/release/rewrite search --room 2,0 --frames 95 --ceiling --level-minus-one 95,5
# the table alone, with its fingerprint (cached in /var/tmp/celeste-l1-cache)
./safe-run.sh -- ./target/quick/rewrite l1 table --room 2,0 --spd 5
# as a probe: the table against a tree's recorded transitions (the tree's room and
# settings: its record; a settings flag is refused)
./safe-run.sh -- ./target/quick/rewrite l1 probe --spd S --tree DIR --steps H --from-step F --to-step T [--marks MARKS]
# a known solution against the table (fails if a state of it is too late)
./safe-run.sh -- ./target/quick/rewrite l1 check --room X,Y --inputs tas/FILE --frames EXIT
```

It replaced the time band (`CELESTE_BAND`, 8 px a frame), a measurement,
not a bound: a spring snaps the player up 8 px and `spd.y = -3` adds 3.

## What it computes

A node is `(heap shape, player cell)`. Every other input is one RANGE per
shape (numbers) or unknown (booleans): `rem` the whole [-0.5, 0.5), speed
[-S, S], the other fields found inductively. Lattice pins stay while they
hold. One frame from a node:

1. **Trace** the shape (`frame::trace_frame`, level-0 widenings) with the
   objects' unpinned `rem`/`spd` as bounded interval inputs (the `move`
   fork's arity is the hull's width, level -1 only).
2. **Copy the cone** of every outcome's `live`, error, position and fields:
   a button fork stands for its whole literal; `Known(..)` is true (this
   evaluator joins undecided selects, which is sound); `ForkOk` is checked.
3. **Specialize** per fork configuration: the move amount is one number, so
   pixel steps and collisions are exact at an exact position.
4. **Evaluate** over the ranges (`Graph::eval`, `Eval::NarrowTop`): an outcome
   whose `live` is not definitely false is a successor at the cells its
   position hull covers; one in another room is the EXIT; the error must be
   false on every live outcome.

A BFS over reachable nodes, then an INDUCTIVE check: every value flowing
into a shape must lie in its ranges and agree with its pins, else the range
widens (the hull twice, then that end to the extreme) or the pin drops, and
the pass is redone. `d` is a multi-source shortest path backward: an exit
edge 1, a DEATH 1 + the start's d (the room restarts). The spawn runs as a
CHAIN from the start state. A shape with a player and a fly fruit is traced
with the `f` level's fruit (only the player is measured, so a forgotten
fruit only weakens d).

## As a filter: why it is sound

- **The window is checked, not assumed**: no real player is outside x in
  [-1, 121] plus one move, below y = 128 or above y = -4; `too_late` panics
  on an in-room row outside it.
- **Only table nodes are refused**: a row without a player cell, outside
  the room, or of a shape or cell the table never reached is kept.
- **A tree remembers its filter** (its record's `level_minus_one` line: H,
  S and the table's fingerprint) **and holds its table**
  (`<level>/l1-table.bin`): a search refuses a horizon past H, a tree is
  extended and checked only under its own filter and table (whatever
  binary), and a larger horizon builds the tree again (a dropped push
  leaves hulls smaller and states later, so the frames depend on H
  throughout).
- 100% (`--category 100`, `gemskip100`): an exit that certainly leaves
  the berry behind has no successor in the table.

The table is horizon-independent: one per room and settings.

## Checks done

- Recorded transitions against the table, 0 violations: room (2,0)
  (463,567 pairs and 162 exit crossings at S = 5 and 6), (1,0) (383,247),
  (0,2) (288,366).
- Marked states too late: 0 (room (2,0) h95, 19.2M; room (1,0) f50-f99).
- `l1 check` of known solutions, 0 too late: (1,0) 99, (5,0) 77, (2,3) 106,
  (6,2) 75, (3,0) 89, (6,2) nodiag 80, (6,2) gemskip 120; in float, 94
  Celia goldens (8,054 states).

## Measurements

| room | table build | nodes | start's d | optimum | effect |
|---|---|---|---|---|---|
| (1,0) | 19.9 s | 17,904 | 44 | 99 | drops 34% of level 0's rows f0-f99, all from f73; memory, not time |
| (2,0) | 472 s | 154,408 | 45 | 95 | f80 24.0M states against 62.8M; OPTIMAL 95 in 27:28 |
| (5,1) | 39 s | | 45 | 104 | f90 7.0M kept (unfiltered f89 37.7M) -> 1.1M at f95 |
| (0,2) | 3:52 | 61,208 | 45 | 81 | f70 1.3M (unfiltered f75 65.8M, killed at 30 GB) |
| (6,1) | 6:43 | 78,544 | 49 | 89 | |

d is weak by design (start's d ~45 against optima of 80-130: the player
may move S px a frame in any direction). It bites in the last ~20 frames,
exactly where a room's level 0 explodes when its frames do not dedupe
(springs, ice). It does not rescue a room whose every frame is new (room
(6,0) with platforms exact: d 31 against the real 46; that needed `p`,
until the exact platforms keyed by the first one's fit, 2026-10-25).

## Build failures and their fixes

Each a premise the table checks that the evaluator could not decide; none
weakened:

- the balloon's phase: the walk records the start's own intervals, and a
  slot every state holds as one literal is read as that literal;
- a tile loop over a joined `x`: lifting every op through the select is
  the fallback per node in violation (`Traced::precise`; ~25x the graph);
- the fake wall's speed: the move arity is the hull's width;
- the fly fruit's flight grew `spd.y` to the 16.16 extreme: traced with the
  `f` level's fruit since 2026-10-07;
- a start slot only a later frame writes an interval to: only slots the
  start holds as an interval are seeded from its literal.

Still refused or not converging: the summit (it measures the room exit) and
the platform rooms (6,0), (2,1), (1,2), in fixed and float (measured at
`p`; not tried since the platforms are exact).

## Deferred: the off-screen lane

Room (2,0)'s left pocket stays in the frontier 16 frames too long: the
table's fastest path leaves the room on the left and flies up outside it.
`freeze` and `djump` are one full range per shape, so `_update`'s "moved"
and `_draw`'s clamp join every frame, with a dash every frame. Chosen fix,
not built: small counters exact per node (shape, cell, `freeze`, `djump`),
up to 6x the nodes, the filter taking the minimum d over a cell's
counters. Bigger: speed per node instead of [-S, S] everywhere.

## In doubles (`--arith float`)

The table is built in the run's arithmetic: the doubles' interval ops
round each corner with the operation's own (monotone) rounding; ranges are
kept in the total order (`-0` below `0`); a grown range jumps to
`+-f64::MAX`; the round fork counts the values of `flr(x + 1/2)`. Float
tables are within ~1-3% of the fixed ones' sizes with the same start d
(plans/float.md). Fixed is unchanged (the fixtures' tables rebuild to their
fingerprints).

## Limits

- `move`'s loops are unrolled 8 times (`--@unroll 8` in the cart), which caps S at 7.
- Deaths are modelled only through the respawn's d.
- No condition refinement: counters (`freeze`, `djump`, `dash_time`,
  `grace`, the springs' `delay`) go to the full range.
