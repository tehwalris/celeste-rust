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

RESULTS_TABLE

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
  49/53, (3,1) 45 -> 47, (6,1) split 98 -> 122 steps.

ROOM_NOTES
