# 100% (2026-10-08)

`CELESTE_HUNDRED=1` (`game_runner::hundred`): a room is won only by exiting
with its berry taken (`frame::wins_of` ANDs `frame::got_fruit`; an unknown
`got_fruit` at a coarse level counts as taken, the concrete search decides).
The 18 rooms of the tasdatabase's `classic/100` list. Runs:
`tools/category_runner.py` (category `100`), jobs and logs under
`/var/tmp/h100` (branch `hundred`).

Frames count as everywhere (plans/results.md): the frame DURING which the
room changes, spawn prologue included; the database lists `frames = exit
frame - prologue - 1` (UCT's clean save: inputs - 1).

## Results

Ours and the reference in our frames; "DB" is the database's count (ours in
its terms: UCT inputs - 1). Every "verified" row: the witness exits at the
optimum in the ORIGINAL cart on PICO-8 with the berry taken (a `lifeup`
before the exit), under the upload's seeds, and UniversalClassicTas
finishes the upload with no death (validate.sh since `0a67b9e`); all were
re-run through the merged runner on 2026-10-08 (`/var/tmp/h100/final`).
"Bounds" are the arc bounds per level of the ladder (a lower bound over
every seed, balloon phase and chest draw); the concrete search's first win
is the optimum.

| room | name | DB | ref | ours (DB terms) | levels, bounds | status | run |
|---|---|---|---|---|---|---|---|
| (0,0) | 100m | 90 | 118 | **115 (87f), -3** | `r0sxhn,r0sxh`: 115 | OPTIMAL, verified | overnight, 1051 s, 10.1 GB |
| (2,0) | 300m | 103 | 129 | - | `r0sxh` | OPEN: level 0 too big (below) | |
| (3,0) | 400m | 61 | 89 | 89 (61f), tie | real-play start (J 3): `r0sxhfn,r0sxhn,r0sxh`, h88 refuted at level 1 (level 0 bound 71) | OPTIMAL (real-play start), tie, verified | 2026-10-09, 1955 s, 27.2 GB (`/var/tmp/night2`); any% witness (takes the fly fruit) |
| (4,0) | 500m | 123 | 153 | **152 (122f), -1** | `r0sxhn,r0sxh`: 152 | OPTIMAL, verified (chest seed -1, the file's) | overnight, 2855 s, 11.7 GB |
| (6,0) | 700m | 75 | 99 | - | `r0sxhfp,r0sxh,r0sx` split | OPEN: arc phase out of memory (below) | |
| (0,1) | 900m | 94 | 122 | 122 (94f), tie | 114, 122 | tie, verified | overnight, 5365 s, 71 GB |
| (3,1) | 1200m | 52 | 78 | 78 (52f), tie | `r0sxhf,r0sxh`: 76, 78 | tie, verified | 1093 s, 16.4 GB |
| (4,1) | 1300m | 127 | 153 | 153 (127f), tie | `r0sxhn,r0sxh`: 153 | tie, verified | overnight, 1946 s |
| (5,1) | 1400m | 103 | 129 | - | `r0sxhn,r0sxh` | OPEN: level 0 too big (below) | |
| (6,1) | 1500m | 88 | 114 | 114 (88f), tie | `r0sxhn,r0sxh` split: f112, f113 | tie, verified | 4164 s, 30.5 GB |
| (0,2) | 1700m | 64 | 90 | 90 (64f), tie | 90 | tie, verified | overnight, 692 s |
| (2,2) | 1900m | 107 | 131 | 131 (107f), tie | 131 | tie, verified | overnight, 709 s, 44 GB |
| (6,2) | 2300m | 68 | 94 | **93 (67f), -1** | real-play start (J 2): `r0sxhf,r0sxh`: 89, 93 | OPTIMAL (real-play start), VALID in IL, UCT, Celia, the chain and the boot chain | 2026-10-08, 27:20, 31.8 GB (e50c575); the earlier 89 (63f) held only from an IL load and is withdrawn (tas/invalid) |
| (0,3) | 2500m | 108 | 134 | 134 (108f), tie | `r0sxhn,r0sxh`: 134 | tie, verified | 2452 s, 42.3 GB |
| (1,3) | 2600m | 137 | 163 | **161 (135f), -2** | `r0sxhn,r0sxh`: 161; real-play start (J 6, class {6, 7, 8}): 161, 161 | OPTIMAL under the real-play start, VALID in IL, UCT, Celia, the chain and the boot chain (chest seed 0.5) | overnight, 527 s, 34 GB; 2026-10-09 real play 485 s, 31.5 GB (`/var/tmp/night2`) |
| (3,3) | 2800m | 137 | 187 | 187 (137f), tie | 187 | tie, verified | overnight, 502 s |
| (4,3) | 2900m | 79 | 109 | 109 (79f), tie | `r0sxhn,r0sxh`: 106, 107 | tie, verified | 2768 s, 26.4 GB |
| (5,3) | 3000m | 82 | 114 | 114 (82f), tie | 102, 114 | tie, verified | overnight, 5581 s, 75 GB |

All runs: `--ceiling REF`, level -1 at (REF, 5) (steps under the split
frame), `CELESTE_CONCRETE_BALLOON_SEEDS` = the file's seeds. "overnight" is
the 2026-10-07 queue (`/var/tmp/overnight`, the search on `integrate`
before this branch; its results stand, only their verification was redone);
the others ran on `hundred` (berry-aware level -1, lost berries not
expanded), `CELESTE_TRIM_ROWS=1`, under 55 GB.

400m: the 100% wins are a subset of the any% wins, so the any% optimum 89
(the precision ladder, 2026-09-19, h88 refuted at level 6; no `n`/`t`
level, so the interval-wrap caveat does not apply) is a lower bound, and
the any% witness `tas/room_3_0_exit_frame_89.txt` takes the fly fruit: it
verifies as a 100% run (berry, f89 in the original cart, UCT 62 inputs).
2026-10-09: rerun under the REAL-PLAY start (`CELESTE_LOADING_JANK=3`, the
boot chain's J; the start classes {3, 4} are one): `--to 88` refuted at
level 1 (r0sxhn) after level 0 (r0sxhfn, 8 px regions) - so 89 is the
optimum of the room as real play enters it, not only a lower bound.
UPLOAD_400

Every upload starts at the room's prologue (the player's first frame), and
its UCT input count is the PICO-8 exit frame minus the prologue (UCT inputs
- 1 = ours in DB terms on every row): no file was lengthened to finish in
UCT, and no reference is "UCT only" any more (1300m and 3000m exit on
PICO-8 once the chests are seeded).

Upload files (UCT clean save): `/var/tmp/h100/final/upload/100/TAS*.tas`;
the four improvements are also in `tas/tasdatabase/100/upload/` with their
witnesses `tas/room_X_Y_hundred_frame_N.txt`.

## What was wrong with the overnight verification (fixed)

- **Chests were not seeded in the replay.** A tasdatabase `[seeds]` list
  runs over balloons AND chests in object creation order
  (UniversalClassicTas `set_seeds`); a chest's seed `s` is what its LAST
  shake's `rnd(3)` returns minus 1, so its berry appears at `x = start + s`.
  `pico8_diff/replay.py` gave a seed only to balloons: a chest swallowed
  none and every later balloon got the wrong one. That is why the community
  1300m and 3000m files "did not exit" in the replay (the overnight runner
  fell back to UCT's count), and why 500m's 152 did not get its berry
  (PICO-8's own `rnd` put it elsewhere). With the seeds in UCT's order:
  1300m's and 3000m's files exit at 153 and 114 in the original cart, and
  500m's witness takes the berry at f101 and exits at f152 under the file's
  own chest seed (-1).
- **The search takes every draw of the chest** (`rnd` is an interval at
  every level, and `CELESTE_CONCRETE_BALLOON_SEEDS` fixes only balloons), so
  its optimum is a lower bound over all chest seeds and its witness takes
  the berry for SOME draw. The runner now tries the file's chest seed, then
  each class of draws (s in -1, -0.5, 0, 0.5, 1, 1.5: only which side of a
  pixel the berry lies on matters), and the upload carries the seed that
  works: 2600m's 161 takes the berry with s = 0.5 (the file has 0). The
  chest is not seeded in the search itself: its fruit's `x` is the tree's
  literal `rnd` interval, so a seeded concrete fruit would need its own
  projection onto the tree's keys (`arc_dp::lookup_keys` does it for the
  balloons' phase); choosing the seed after the search proves the optimum
  over every seed instead of one.
- 1900m: our file finishes in UCT once its jumps are canonicalized (the
  overnight table recorded the uncanonicalized attempt).

## What made rooms cheaper

- **Lost berries are not expanded** (`frame::berry_lost`, exact): a row with
  `got_fruit` false and no fruit, fly fruit, key, chest or fake wall left
  can never win. In the fly-fruit rooms the fruit flies off at the first
  dash: room (3,1) `r0sxhf` f50 kept 3.78M -> 1.89M.
- **Level -1 knows the berry**: under `CELESTE_HUNDRED` an exit whose
  outcome certainly has not taken the berry (`got_fruit` entry nil or the
  constant false) has no successor in the table. Start d / largest finite
  d (S = 5): (2,0) 45/25 -> 45/40, (5,1) 45/25 -> 51/38, (0,3) 46/27 ->
  49/53, (3,1) 45 -> 47, (6,1) split 98 -> 122 steps. What it did in a
  run: room (0,3) at H = 134 peaked at 12.6M kept (f96) and fell to 4.0M
  by f109, where the overnight run (no berry rule) had 56.5M at f111 and
  still +7% a frame when it was stopped; the room finished in 2452 s.
  Room (6,1) (split, H = 228 steps) peaked at 18.3M a step (step 193).

## The open rooms

- **300m (2,0)**, `r0sxh` (the springs exact; `n` was 2.4x bigger in
  nodiag): identical to the overnight run through f60 (4.68M), which went on
  to 50.3M at f79 and 103.8M at f84 (+15% a frame, 98 GB of disk a frame,
  the disk guard). The berry is collected early (TAS3: f50, at (8, 48)) and
  the route to the exit is the long part; at H = 129 level -1 cuts nothing
  before f89 (its largest finite d is 40, with the berry rule), so the run
  is the overnight one until far past 55 GB. Needs a tighter level -1 (its
  5 px/frame in any direction is the weakness) or a coarser level 0.
- **1400m (5,1)**, `r0sxhn,r0sxh`: 63.8M kept at f94, +11% a frame, peak
  39.6 GB, stopped (nodiag's peak was 62.5M at f100 with H = 121 and needed
  60 GB; 100%'s H = 129 moves level -1's cut 8 frames later). The berry is
  next to the exit (top left; TAS14 takes it at f128, exits f129), so the
  berry rule barely moves d (start 45 -> 51).
- **700m (6,0)**, split, `r0sxhfp` (no level -1: the table refuses, 123,904
  fork configurations for one shape): the forward reaches step 198 (h99) in
  93 min, 30.3M kept at its widest (step 130), door 44.5 GB, peak 50.9 GB;
  the arc phase was OOM-killed after the remainder-free BFS (anon 16 GB,
  38 GB of mapped runs, the 55 GB cap). The fly fruit unknown (`f`) loses
  exactly what makes 100% narrow here: TAS7 never dashes before the fruit
  (a dash sends it flying), so every early-dash route is a dead end the `f`
  level cannot see. With the fruit exact (`r0sxhp`) berry_lost would prune
  them, but the waiting fruit's `step` is a frame counter: 19.5M kept at
  step 85 against 2.5M at step 80 under `f`.
