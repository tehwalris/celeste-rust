# Level -1: a position-only cost-to-go table from the traced frames

A sound lower bound on the frames to the exit from every (shape, player cell),
built from the same traced frames the kernels come from, used as a FILTER on
level 0 under a known ceiling. It replaced the experimental time band
(`CELESTE_BAND="H,px"`, `frame::band`, 8 px/frame), which was a measurement,
not a bound: a spring snaps the player up to 8 px and `spd.y = -3` moves it
3 more in the same frame. Code: `src/trace/level_minus_one.rs`.

```bash
# as a filter inside a search (H = the ceiling, S = the speed bound in px/frame)
CELESTE_LEVEL_MINUS_ONE="95,5" ./safe-run.sh -- ./target/release/rewrite search --room 2,0 --ceiling 95
# as a probe: the table against a level-0 tree, with the too-late share per frame
CELESTE_START_ROOM=2,0 ./safe-run.sh -- ./target/quick/transpile \
    --level-minus-one S LEVEL_DIR CEILING FROM TO [MARKS]
```

## What it computes

A node is `(heap shape, cell of the player)`. Every other input is one RANGE
per shape (numbers) or unknown (booleans): the player's `rem` the whole
[-0.5, 0.5), its speed [-S, S], the other fields discovered inductively. The
heap shape stays concrete; the lattice pins stay pinned while they hold.

One frame from a node:

1. **Trace** the shape (`verify::trace_frame`, level-0 widenings) with every
   object's unpinned `rem`/`spd` as interval inputs, bounded, so
   `Domain::flr_ways` sizes the `move` forks from the static ranges (11-way at
   S=5; `Symbolic::uncapped_ways` gives the hull's width, level -1 only).
2. **Copy the cone** of every outcome's `live`, error, position and fields,
   with substitutions: a fork over a literal (a button) stands for its whole
   literal; `Known(..)` is true (this evaluator JOINS undecided selects, which
   is sound - `Symbolic::no_known_forks`); `SplitOk(..)` is checked directly
   (every fork operand spans at most its arity).
3. **Specialize** on every fork configuration into one shared graph: per
   configuration the move amount is one number, so the pixel steps and their
   collisions are exact at an exact position.
4. **Evaluate** over the ranges (`Graph::eval_narrow_top_in`): an outcome
   whose `live` is not definitely false is a successor at the cells its
   position hull covers; an outcome in another room is the EXIT; the error
   must be false on every live outcome.

Passes: a BFS over the reachable nodes, then an INDUCTIVE check - every value
flowing into a shape must lie in its ranges and agree with its pins, else the
range widens (the hull for its first two growths, then that end straight to
the 16.16 extreme) or the pin drops, and the pass is redone.

`d(node)` is `sound_d`: a multi-source shortest path backward, an exit edge 1,
a DEATH successor 1 + the start state's d (the room restarts and replays the
spawn; a second run is the fixpoint). The spawn runs as a CHAIN from the start
state (as a position-only node it never converges: `solids=false` and one
speed range per shape let it rise forever).

## As a filter (`frame::level_minus_one`, `CostToGo::too_late`)

A flush queue (one player cell) is dropped when `f + d(shape, cell) > H`. What
makes it sound where the probe only counted:

- **the window is CHECKED, not assumed**: no real player is outside x in
  [-1, 121] plus one move (the draw clamp, skipped on a dash's freeze frame),
  below y = 128 (the update kills) or above y = -4 (the room changes);
  `too_late` panics on any in-room row outside it. A clipped successor part
  is dropped (seeding it as a possible exit, d = 1, was sound and useless:
  room (2,0) f80 kept exactly the unfiltered 62.8M);
- **only table nodes are refused**: a row without a player cell, outside the
  room, or of a shape or cell the table never reached is kept;
- **H is the largest horizon the run tests**: level 0 persists across
  horizons, so this is for a `--ceiling` search with H = the ceiling.

The table is horizon-independent: one per room.

## Soundness checks done

- Room (2,0), against the recorded `posgraph.bin` of a level-0 tree: 463,567
  in-room pairs and 162 exit crossings, 0 violations at S=5 and S=6.
- The backward's marks at h95 (19,245,834 states): 0 marked states too late.
  With the filter on, h95's level-0 marks are EXACTLY the band run's: the
  filter dropped no state on a path to a win.
- Room (1,0) at S=5, ceiling 99: 0 violations against 383,247 recorded pairs,
  0 marked states too late at every frame f50-f99.
- Room (0,2): 288,366 recorded in-room transitions of an unfiltered `r0sxhn`
  tree, 0 not in the table.

- A known solution against the table: `CELESTE_START_ROOM=X,Y rewrite
  l1-check --inputs tas/FILE --horizon EXIT` steps it with the reference
  engine and fails if any state of the exiting lineage is too late. 0 too
  late (2026-10-07): (1,0) at 99, (5,0) at 77, (2,3) at 106, (6,2) at 75,
  (3,0) at 89, (6,2) nodiag at 80, (6,2) gemskip (TAS23's 120).

## Measurements that matter

| room | table build | nodes | start's d | known optimum | effect |
|---|---|---|---|---|---|
| (1,0) | 19.9 s | 17,904 | 44 | 99 | drops 34% of level 0's rows f0-f99, all from f73; pays in memory (4.25 vs 5.76 GB), not time |
| (2,0) | 472 s (S=5, 16 threads) | 154,408 | 45 | 95 | f80 24.0M states against 62.8M (49 s vs 131 s a frame), f90 1.0M, f95 1; OPTIMAL 95 in 27:28 at 28 GB with no band |
| (5,1) | 39 s | | 45 | 104 | resumed at f88: f90 7.0M kept (unfiltered f89 37.7M) -> 1.1M at f95 |
| (0,2) | 3:52 | 61,208 | 45 | 81 | f61 6.28M vs 6.96M, f70 1.3M (unfiltered f75 65.8M, killed at 30 GB) |
| (6,1) | 6:43 | 78,544 | 49 | 89 | |
| (7,1) | 32 s | 21,250 | 45 | 86 | |

d is weak by design: the start's d is ~45 against optima of 80-130 (the
player may move at S px/frame in any direction at every node). It bites only
in the last ~20 frames - which is exactly where a room's level 0 explodes
when its frames do not dedupe (springs, ice). It does NOT rescue a room
whose every frame is new: room (6,0) with platforms exact kept the
unfiltered counts at f50 (d = 31 against the real 46); that needed `p`.

## Build failures and their fixes (each a premise the table CHECKS that the evaluator could not decide; none weakened)

- **The balloon's phase** (rooms (6,1), (7,1)): the start representative
  holds a blanked 0 where every state holds the literal [0, 1); the walk now
  records the start's own intervals (`LatticeWalk::start_ivals`), a slot every
  state holds as one literal is read as that literal (`Shape::lits`), and
  `Lo`/`Hi`/`Sub` lift selects out (`Lift::Ends`).
- **A tile loop over a joined `x`** (rooms (7,1), (0,2), (6,1)): over
  `sel(spd ~= 0, x moved, x)` the loop's two ends are independent; lifting
  every op through the select decides it but grows the graph ~25x, so it is
  the FALLBACK per node in violation (`Traced::precise`).
- **The fake wall's speed** (room (0,2)): a 3-piece move operand; the arity is
  now the hull's width at level -1 (1,210 configurations).
- **The point split** (2026-09-20) and the move arity cap (`7dce5b4`) had
  silently broken level -1 everywhere, room (1,0) included, unnoticed because
  the room being run did not use it. Repaired in `4b31c1b`.
- **The fly fruit** (rooms (3,0), (6,2); 2026-10-07): exact, its flight
  (`spd.y = appr(spd.y, -3.5, 0.25)`, `fly` unknown) grows `spd.y` by 0.25 a
  pass until the widening jumps to the 16.16 extreme; its `move` fork then
  spans 65,536 floors at arity 2 and `rem.y` leaves [-0.5, 0.5). Now a shape
  with a player and a fly fruit is traced with the `f` level's fruit
  (`widen::fork_fruit_inputs`: `step`/`y` unknown, `spd.y`/`rem.y` their
  literal ranges, `fly` unknown; the evaluator reads the unknown number as
  TOP). Sound because only the player is measured: a forgotten fruit is
  "maybe collected, maybe gone" everywhere, which only weakens d. The spawn
  prefix stays exact (one concrete chain). Room (3,0): 64,650 nodes, start
  d 45, 5:38; room (6,2): 62,484 nodes, start d 45, 2:25 (the same table with
  `CELESTE_GEMSKIP=1`).
- **A start slot only a later frame writes an interval to** (rooms (5,0),
  (2,3): the balloon's bob `y`, "not a literal range"): the start state holds
  a constant there; only slots the start state itself holds as an interval
  (`LatticeWalk::start_ivals`, now with `None` for a non-literal) are seeded
  from the start's literal, the rest from the representative's value. Room
  (5,0): 17,342 nodes, start d 46; room (2,3): 19,222 nodes, start d 44.
- Rooms that built before keep their fingerprints ((1,0) `b909eb5d1645fad6`,
  (4,2) `af00ed208170568a`, (7,0) `9d3b5f393a563677`).
- **Still refused**: the summit (it measures the room exit); not re-checked:
  room (1,2) (a platform room).
- **Reverted** (`4b31c1b`, Philippe: the table was not asked for every room
  and did not rescue room (6,0)): platform phases from concrete snapshots, an
  "unmodelled node gets d = 1" rule, range-reading other objects' forks.

## Deferred: the off-screen lane (precision)

Room (2,0)'s left pocket stays in the frontier 16 frames too long: the table's
fastest path leaves the room on the left and flies up outside it at 5
px/frame. `freeze` and `djump` are one full range per shape (no condition
refinement), so `_update`'s "moved" and `_draw`'s "clamp skipped" join every
frame, with a dash every frame. Chosen fix (C, not built): small counters
exact per node - a node is (shape, cell, `freeze`, `djump`) - up to 6x the
nodes; the filter would take the minimum d over a cell's counters. Check with
`CELESTE_L1_PATH="16,48"`. Bigger: speed per node instead of [-S, S]
everywhere (would bend the top's straight cut along gravity and walls).

## Limits

- `unroll_bound("abs(amount)") = 8` caps S at 7.
- Deaths are modelled only through the respawn's d; the window is a claim
  from the code checked on every row the filter sees.
- No condition refinement in the evaluator: counters (`freeze`, `djump`,
  `dash_time`, `grace`, springs' `delay`) go to the full range.
