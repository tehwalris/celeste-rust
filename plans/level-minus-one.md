# Level -1: a position-only cost-to-go table from the traced frames (2026-09-17)

Built first as a probe, then wired into the search as a filter
(`CELESTE_LEVEL_MINUS_ONE="H,S"`, "As a filter" below) that replaced the
experimental time band (`CELESTE_BAND="H,px"`, `frame::band`), whose 8 px/frame
is a measurement, with a bound derived from the game code. Room (2,0) reports
OPTIMAL 95 with it and no band ("Room (2,0): OPTIMAL 95 without the band").

```bash
CELESTE_START_ROOM=2,0 CELESTE_THREADS=16 ./safe-run.sh -- ./target/quick/transpile \
    --level-minus-one S LEVEL_DIR CEILING FROM TO [MARKS]
# e.g. 5 /var/tmp/celeste-search20-held/level00 95 60 95 \
#      /var/tmp/celeste-search20-held/h095/level00.marks.bin
```

Code: `src/trace/level_minus_one.rs` (`probe`).

## What it computes

A node is `(heap shape, cell of the player)`. Every other input of the shape's
frame is one RANGE for the whole shape (numbers) or unknown (booleans): the
player's `rem` is its whole [-0.5, 0.5), its speed is [-S, S], and the other
fields are discovered (below). The heap shape stays concrete, and the lattice
pins stay pinned for as long as they hold.

One frame from a node:

1. **Trace** the shape (`verify::trace_frame`, level-0 widenings) with every
   object's unpinned `rem`/`spd` as interval inputs, and with `bounds` on them.
   The bounds let `Domain::flr_ways` size the `move` forks from the static
   ranges: 11-way at S=5, 13-way at S=6.
2. **Copy the cone** of every outcome's `live`, `ok`, position and fields out
   of the arena, with three substitutions:
   - `Free(b)` becomes an extra input cell, seeded unknown.
   - `Known(..)` becomes true: it is the kernels' premise that a select reads
     a decided condition, and this evaluator JOINS undecided selects instead,
     which is sound.
   - `SplitOk(..)` becomes true. It is checked directly instead: every fork
     operand spans at most the fork's arity.
3. **Specialize** on every fork configuration into one shared graph
   (`specialize_subset_into`). In a configuration the move amount is one
   number, so the pixel steps and their collisions are exact at an exact
   position.
4. **Evaluate** with `Graph::eval_narrow_top_in` over the ranges.
   - Outcomes whose `live` is not definitely false are successors, at the
     cells their position hull covers.
   - An outcome in another room is the EXIT.
   - `ok` must be TRUE on every live outcome. What is left in it: pin guards,
     the static range premises, the symbolic loops' "finished" obligation, rem
     containment.

Passes: a BFS over the reachable nodes, then an **inductive check**. Every
value flowing into a shape must lie in its ranges and agree with its pins. If
one doesn't, the range widens or the pin is dropped (and the shape
re-traced), and the pass is redone. The widening is the hull for a slot's
first two growths, then that end goes straight to the 16.16 extreme. The
hull comes first because a threshold overshot `widen_fruit`'s band premise.
`d(node)` is the fewest frames to an exit, backward from the exit edges.

### What it does not model (counted in the report)

- **The spawn** runs as a CHAIN from the start state (26 frames to the player
  at (8, 104)). As a position-only node it never converges: `solids=false`,
  and `state`, `delay` and the speed are one range per shape, so it can rise
  forever. Tried first, the spawn's `spd.y`/`delay` ranges doubled every pass
  to the fork arity limit (255).
- **Deaths** are dead ends. The comparison keeps `NO_CELL` rows.
- **Successor cells outside x, y in [-16, 143]** are clipped. Real player
  positions lie inside: the draw clamp to [-1, 121] is skipped on a dash frame
  (`freeze=2`), which leaves at most one move (<= 6 px) past it. The
  imprecision pushes hulls further.

## Final inductive ranges (room (2,0), both S converge after 6 passes)

The eight player shapes share one pattern (S=5; S=6 identical except the speed):

| field | range |
|---|---|
| `spd.x`, `spd.y` | [-S, S] (inductive for S=5 and S=6; the dash writes exactly 5) |
| `rem.x`, `rem.y` | [-0.5, 0.5) |
| `dash_target.x/y` | [-2, 2] |
| `dash_accel.x/y` | [0, 1.5] |
| `dash_effect_time` | [0, 10] (level-0 clamp) |
| `dash_time`, `djump`, `grace`, `freeze`, springs' `delay` | FULL 16.16 range |
| springs' `spr` | [0, 19] |
| fruit `off` / `y` | [0, 39] / [45.5, 50.5] |
| `delay_restart` | 0 |

No lattice pin was dropped. The full ranges are the evaluator having no
condition refinement: `if freeze>0 then freeze=freeze-1` computes the arm over
the whole range, so each pass steps one lower. That is sound, and it hardly
matters for positions, but it is why djump cannot limit dashes here.

## Soundness checks

- **Recorded transitions** (`level00/posgraph.bin` of the held-unknown room
  (2,0) tree): 464,896 pairs, 463,567 in-room pairs and 162 exit crossings
  checked, **0 violations** at S=5 and at S=6. 1,167 death/respawn pairs are
  not modelled.
- **The backward's marks at h95** (`h095/level00.marks.bin`, 19,245,834
  states, 10,673,512 of them in f060-f095): **0 marked states are too late**
  (`f + d > 95`) at S=5 and at S=6.
- No violation in the converged pass: every live `ok` is true, every fork
  operand fits its arity, and every position hull is whole pixels.
- The 11 rows of shape `0x38ca639158d2ee7b` at f073-f083 are a respawned spawn
  (after a death), a shape the table never reaches: kept.

## Comparison: share of states with `f + d > 95`

This is the tree the search ran WITH the band (94, 8), so the band's own rule
at ceiling 95 cuts 0 of it (measured). The band's effect as recorded in
plans/held-buttons.md: 60.3M against 62.8M states at f80, about 4%, with the
peak at f82-f83. prune-probe's goal is "y <= top + 8" rather than the exit,
so it is not strictly tighter than level -1. From f083 on, S=5 cuts more than
it.

| frame | states | level -1, S=5 | level -1, S=6 | prune-probe |
|---|---|---|---|---|
| f070 | 16,510,619 | 0.0% | 0.0% | 0.7% |
| f072 | 21,159,387 | 0.0% | 0.0% | 7.2% |
| f073 | 24,300,879 | 0.1% | 0.0% | 12.5% |
| f074 | 27,917,006 | 1.5% | 0.0% | 21.2% |
| f075 | 31,654,306 | 4.1% | 0.0% | 34.2% |
| f076 | 36,405,697 | 8.1% | 0.0% | 45.8% |
| f077 | 41,178,330 | 14.7% | 2.0% | 56.0% |
| f078 | 47,806,190 | 21.2% | 4.9% | 66.2% |
| f079 | 54,245,424 | 40.3% | 9.8% | 73.7% |
| f080 | 61,979,189 | 61.3% | 15.9% | 79.8% |
| f081 | 68,871,941 | 78.2% | 25.7% | 84.7% |
| f082 | 76,200,926 | 87.9% | 51.3% | 89.7% |
| f083 | 77,296,459 | 92.7% | 73.5% | 92.6% |
| f084 | 79,428,398 | 95.2% | 86.4% | 94.2% |
| f085 | 63,878,612 | 94.8% | 91.1% | 93.8% |
| f086 | 36,791,191 | 91.8% | 89.1% | 90.3% |
| f087 | 13,993,095 | 81.7% | 75.2% | 76.7% |
| f088 | 5,179,603 | 60.7% | 42.6% | 42.6% |
| f090 | 3,191,586 | 67.8% | 50.5% | 42.3% |
| f092 | 1,439,154 | 75.9% | 65.3% | 41.5% |
| f094 | 325,639 | 84.5% | 80.7% | 29.1% |

(f060-f069: 0.0% for both S. f095: 100% for both, since a state at the
horizon has d >= 1.)

d itself: the start state's d is 45 at S=5 (26 spawn frames + 19) and 42 at
S=6, against the known optimum of 95. The largest finite d is 25 / 22. 16,320
of 154,408 nodes (S=5) have no path to the exit.

## Size and cost (quick profile, 16 threads)

| | S=5 | S=6 |
|---|---|---|
| nodes / edges | 154,408 / 22,741,336 | 157,076 / 28,789,432 |
| exit nodes | 10,928 | 11,584 |
| fork configurations per shape | 726 (3x2x11x11) or 121 | 1690 (5x2x13x13) or 169 |
| specialized graph per shape | 171k-300k nodes | 243k-432k nodes |
| one pass | 84-89 s | 177-200 s |
| whole probe (6 passes) | 500.7 s | 1015.2 s alone |
| peak RSS | 1.10 GB (2.79 GB with the marks set) | 1.39 GB (3.05 GB) |

(The S=5 pass times ran concurrently with the S=6 run.) Lattice walk 8.7 s,
spawn chain 2.9 s, tracing and specializing 0.3-1.3 s per shape.

The first two forks (arity 3x2 / 5x2) are the player's move after a spring
has rewritten its speed (`hit.spd.x *= 0.2`, `spd.y = -3`) in the same frame.
They are a different operand, so a separate fork, and they multiply the
configurations 6-10x although the two arms exclude each other.

## Blockers and doubts

- `unroll_bound("abs(amount)") = 8` (src/trace/interp.rs:1853) caps S at 7.
  Beyond that the loop's "finished" premise fails. It would be reported, not
  absorbed.
- The spawn is not position-only (above). The chain works because the spawn
  reads no button.
- Deaths and respawns are dead ends, and the window is a claim from the code.
  Both were checked only against recorded transitions, never proven.
- Condition refinement is missing in the evaluator, so the counters go to the
  full range.
- The table relies on the lattice pins, which are checked inductively here.
  It also relies on the tracer's merge semantics: the evaluator joins where
  the kernels required `Known`.

## What it would take to use it

- **As a filter** (the band's place): drop a flush queue (one player cell)
  when `f + d(shape, cell) > H`. That is one table lookup per queue, and the
  table is horizon-independent, so one per room. It needs a death rule: model
  the no-player shapes and the respawn chain, or prove that a death is too
  late (a respawn costs 26 frames plus d(end) = 19 at S=5). It also needs the
  window premise handled (enlarge the window, or bound positions from the
  clamp).
- **Cost**: about 8 minutes for room (2,0) at S=5. Most of it is re-running
  the passes while counters walk down one step per pass. Seeding the ranges
  from the cart's own setters, or a faster widening, would cut that to 2-3
  passes.
- **Precision**: condition refinement in `eval_narrow_top_in` for comparisons
  on an input cell; splitting small integer ranges (`freeze` 0 vs 1-2); a node
  that carries dashes left (`djump`); per-node rather than per-shape speed
  ranges. Each costs nodes or evaluations.
- **As a ladder level** it is the same object: `f + d <= H` is its marked set,
  and it feeds level 0 exactly like `MarkFilter`. There is no forward at level
  -1, only the table.

## Room (1,0) (2026-09-17, against a current tree)

A fresh held-ladder search (`r0sxh..r15sxh,rxsx`, `--ceiling 99`, census
`394ae0b`): OPTIMAL 99 in 1:39.7 wall, 5.76 GB peak. (`/var/tmp/celeste-room10h`
predates the `rnd` global, so none of its shapes matched: every state was
"other shape". The fresh tree matches every shape.)

The probe at S=5, ceiling 99: 5 passes, 24 s, 1.0 GB, 17,904 nodes (15,662 reach
an exit), the start's d = 44, max finite d = 30; 0 violations against 383,247
in-room recorded pairs and 114 crossings; 0 marked states too late at every
frame f50-f99 (`h099/level00.marks.bin`).

| frame | states | too late (f + d > 99) | band (99, 8) |
|---|---|---|---|
| f050-f072 | | 0% | 0% |
| f075 | 1,373,019 | 1.2% | 0% |
| f080 | 1,102,103 | 7.7% | 0% |
| f083 | 961,138 | 33.9% | 0% |
| f085 | 1,021,640 | 50.0% | 0% |
| f090 | 1,290,067 | 78.2% | 19.9% |
| f095 | 1,643,893 | 93.3% | 79.1% |

As a filter it would drop 18,919,491 of the 55,577,462 level-0 rows f0-f99
(34.0%; the band 21.0%), all from f73 on.

## As a filter (2026-09-17): `CELESTE_LEVEL_MINUS_ONE="H,S"`

`trace::level_minus_one::cost_to_go` builds the table (the probe's passes,
`build`) and `frame::level_minus_one` drops a flush queue (one player cell)
when `CostToGo::too_late`. What makes it sound where the probe only counted:

- **`sound_d`**: a multi-source shortest path backward over the edges, seeded
  where the graph stops modelling. An exit edge is 1; a DEATH successor is 1 +
  the start state's d (the room restarts and replays the spawn chain, however
  long the countdown). The death seed reads the end's d, which it cannot lower,
  so a second run is the fixpoint.
- **a clipped successor part is dropped, and the window is CHECKED**: no real
  player is outside it at a frame boundary (the draw clamp keeps x in [-1,
  121], a freeze frame skips it by at most one move; below y = 128 the update
  kills; above y = -4 the room changes). `CostToGo::too_late` panics on any row
  in the room outside it, so the premise is checked on every row the filter
  sees rather than assumed.
- **only table nodes are refused**: a row without a player cell, one that has
  left the room (x >= 128), a shape or a cell the table never reached, is kept.
- **H is the largest horizon the run tests**: level 0 persists across
  horizons, so this is for a `--ceiling` search with H = the ceiling.

First tried: a clipped successor seeded as a possible exit (d = 1). Sound, and
useless in room (2,0): its six player shapes each clip 755,646 successors
(imprecise joins at the clamped edges), 25,224 of 154,408 nodes had one, every
node near them got a d of a frame or two, and level 0 at f80 kept 62,810,806
states - what it keeps with no filter at all. Stopped at f80.

Room (1,0), S=5, H=99 (release, held ladder, `--ceiling 99`), as committed:

- the sound d: the start state's d = 44, the plain probe's (12,199 nodes with a
  death successor lower nothing that reaches an exit);
- the probe with the sound d: too late 0.3% at f074, 7.4% at f079, 44.7% at
  f084, 69.9% at f089, 90.2% at f094; MARKED TOO LATE 0 at every frame;
- the search: **OPTIMAL 99** (h99 confirmed, h98 refuted at level 6), no row
  outside the window, 1:44.5 wall with the table's 19.9 s build, 4.25 GB peak;
  without the filter 1:39.7 and 5.76 GB. Room (1,0)'s late frames are small,
  so it pays in memory, not time. The room it is for is (2,0), whose level 0
  is ~60 M states at f80.

## Room (2,0): OPTIMAL 95 without the band (2026-09-17)

`CELESTE_LEVEL_MINUS_ONE="95,5"`, held ladder (`r0sxh..r15sxh,rxsx`), `--ceiling
95`, no `CELESTE_BAND`, release, census `c75f856`:

- the table: 472.1 s (16 threads), 154,408 nodes, the start state's d = 45;
- level 0 (the same counts as with no filter through f070): f080 23,997,253
  states (no filter: 62,810,806; x2.6), f085 3,315,620, f090 1,029,159, f095 1;
  f080 in 49 s against 131 s, 17.8 GB resident against 25.6 GB;
- h95 level 0 marked 19,245,834 states - exactly the band run's h95 level-0
  marks: the filter dropped no state on a path to a win by f95;
- h95 confirmed at every level through Exact (first win f95 from level 9 on),
  h94 refuted at level 9 (as with the band): **OPTIMAL 95**;
- 27:27.6 wall including the table, 28.0 GB peak, 114 GB of checkpoints; no
  row outside the window.

So room (2,0)'s 95 no longer rests on the 8 px band.
