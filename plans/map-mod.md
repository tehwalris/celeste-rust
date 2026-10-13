# A map mod: inspect, search, validate, submit (2026-10-11)

A MAP MOD is a .p8 with another `__map__` / `__gfx__` (map rows 32-63) /
`__gff__`, and ideally the original Lua. The search reads the map and the
sprite flags from it (`--cart MOD.p8`); the traced Lua stays
lua/celeste-minimal.lua, so it holds only while the mod's Lua equals the
original's (`p8cart.py luadiff`).

## 1. Inspect

```bash
pico8_diff/p8cart.py check MOD.p8          # per room: same/MOD, spawn, objects, Celia's J; the Lua per function
pico8_diff/p8cart.py rooms MOD.p8 --show 0,0   # one room's tiles (## solid, ^^ spike, object tiles in hex)
pico8_diff/p8cart.py luadiff MOD.p8         # what each changed function changed (token diff)
pico8_diff/p8cart.py extract MOD.p8 DIR     # DIR/map-data.txt, flag-data.txt (cart/ format), game.lua
```

`luadiff` normalizes whitespace, comments, `f""` calls, number spellings and
the inlined `k_*` button constants: "0 changed" means the Lua is the
original's up to spelling. Anything else needs reading: a changed object
`init`/`update`, `load_room`, `next_room`, `level_index` comparisons or the
player is a change to the GAME, and the minimal cart would have to take the
same change (by hand, in lua/celeste-minimal.lua, then the frozen name table
`crates/celeste-names/src/gen.rs` APPENDED only if new globals appear). A
title or a `print` change is cosmetic.

Map-dependent constants that a mod can make wrong (from the inventory of
2026-10-11): `ORB_LEVEL = 21` (src/game_runner.rs; max_djump 2 past it;
replay.py's `> 21`; Celia's vanilla rule `idx >= 22`), `SUMMIT_LEVEL = 30`
(src/frame.rs: the summit win is the flag tile 118 of THAT level; the Lua's
`level_index() < 30` / `== 31`), the 8x4 room grid (`level_index`,
`win_room`). A room's own exit is the room change, whatever the map; an
object tile anywhere is made by the Lua's `load_room` as usual.

## 2. Search a room

```bash
./safe-run.sh -- ./target/release/rewrite search --cart MOD.p8 --room X,Y \
    --level r0sxhn --to H --balloon-seeds 0 --checkpoint-dir DIR [--loading-jank J]
```

- `--cart` is a recorded setting: the tree's settings.txt has `cart
  sha256:...` (the file's `sha256sum`), and a tree is refused under another
  cart or none. The level -1 cache key includes it.
- No reference: `--to H` with H generous (`--ceiling` needs a known
  solution). The objects ladder (`--level r0sxhn,r0sxh`) where the bound is
  loose, as for the original (plans/results.md "How to run a room").
- `--balloon-seeds 0`: Celia's balloons start at phase 0 plus the file's seed
  (`[]`: 0), so seeds 0 make the search exact for a `[]` file.
- `--loading-jank`: Celia's per-level validation loads level L with the
  objects from J = the previous room's object count on (`p8cart.py rooms`,
  column celia_J); the first level (room (0,0)) has none.
- Max dashes: the start room past `ORB_LEVEL` gets 2 (src/game_runner.rs);
  a mod whose orb is elsewhere needs that constant changed.

## 3. Check the witness

```bash
pico8_diff/p8cart.py extract MOD.p8 /var/tmp/mod-x
pico8_diff/replay.py --cart MOD.p8 --lua /var/tmp/mod-x/game.lua --begin-game --room X,Y \
    --balloon-seeds 0 DIR/witness_frame_F.txt          # the mod's own Lua on a real PICO-8
pico8_diff/replay.py --cart MOD.p8 --room X,Y DIR/witness_frame_F.txt   # the minimal cart on the mod's map
```

The witness counts frames from the load; the room changes on frame F.

## 4. The submission file and Celia on fixp

A Celia/tasdatabase file is `[seeds]i1,i2,...`: one input byte per frame
from the frame the PLAYER exists (the spawn's frames before it are not in
the file); Celia's count (the database's "frames") is the clean-saved file's
inputs minus 1 (the room changes on its last input's frame). Cut the
witness at the player's first frame (the first `replay.py` line with
` player `; try one earlier too, category_runner.py does) and write
`[]` + the rest:

```bash
CELIA_CART=MOD.p8 CELIA_FIXP=1 ./safe-run.sh --memory 8G -- tools/celia/validate.sh L TAS1.tas
#  -> "level L TAS1.tas: finished, Nf"   (N: Celia's count, the database's "frames")
CELIA_CART=MOD.p8 CELIA_FIXP=1 CELIA_FULLGAME=1 ./safe-run.sh --memory 8G -- tools/celia/validate.sh DIR_OF_TAS_FILES
#  -> "[celia] EXIT level n frames c time m:s.f" per level (Celia's Shift+N full-game playback)
```

Checked: the tasdatabase's arielle (a map mod) level 2 file in Celia fixp
via `CELIA_CART`: finished, 46f = the database's 46. A whole game is a set of
TAS<level>.tas files (the database zips them, `Full<Mod>Any.zip`).

## The mod of 2026-10-11 (taslevel.p8)

Lua identical to the original (`luadiff`: 77 of 77 functions); flags
identical; three rooms changed: (0,0) the challenge room (spawn (32,24), 3
strawberries, a spring, a balloon, spikes; exit at the top), (1,0) an orb
(big chest) room, (2,0) a flag room. Only room (0,0) matters.

The kernels needed the strawberries exact (level flag `e`, commit
df1795f: the bob's band around `start` cannot be built when taking one of
three strawberries moves the others in `objects`).

Measured at `r0sxhne`, no level -1, NOT `--balloon-seeds` in the forward
(a seeded balloon's offset is a point that advances only while `spr == 22`,
and `n` makes `spr` a range: the offset forked into 62 values a frame;
unseeded it is the canonical full period). `--concrete-balloon-seeds 0`
for the concrete phase instead. The player exists from frame 46 (the
upload cut: 45). New states a frame: f60 19k, f90 4.4M, f126 12.9M, f152
104.5M (+9% a frame); 270 GB of tree at f152 (quick build, ~3-25 s a
frame). First abstract win f152; `search --to 152` on that tree: the arcs
REFUTE 152 (15 s). In f140's largest shape (28.8M rows, the fruit all
taken) `spd.x` has 14845 distinct values.

### Searches with level -1 (2026-10-11)

Level -1 builds with `e` for this room (68800 entries, start d 89: it
widens `djump`, so it is weak early). Checked against the unfiltered f152
tree before deleting it: `transpile --probe` under the h152 remainder-free
marks, frames 120-152: 0 marked rows too late.

| H | flags | forward | peak new states/frame | disk (trimmed) | peak RSS | result |
|---|---|---|---|---|---|---|
| 152 | none (unfiltered) | to f152 | 104.5M (f152) | 270 GB | 25 GB | REFUTED (arcs) |
| 170 | `--level-minus-one 170,5 --trim-rows` | 347 s | 14.0M (f133) | 63 GB | 14 GB | REFUTED |
| 190 | `--level-minus-one 190,5 --trim-rows` | 1147 s | 108.8M (f154) | 317 GB | 64 GB | REFUTED (1251 s) |

So the optimum is at least 191 (the arcs' refutation at r0sxhne: sound). At
h190 the winning sets W reach back to f080 (1 node; f152 at h152), the
player exists from f046. A raise needs untrimmed rows (`--trim-rows` trees
are refused), so each H is a fresh run. The cost grows about 9% a frame per
frame of horizon in the middle frames: H=200 is ~2.4x H=190's disk.

The exit: the room's only opening is the top row's spikes (tile 27, cols
9-11). `spikes_at` kills on tile 27 when the hitbox top `y % 8 <= 2` and
`spd.y <= 0`, so the player must cross hitbox-top 2..0 in one frame (4+ px
up: a dash), the frame before leaving.

How loose level -1 is here: its start distance is 89 frames, the proven
lower bound 191 (h190 refuted), so it is at least 102 frames short. The room
is a LOOP (Philippe): the spawn pocket is walled by spikes, the route goes
down the left, across the bottom (spring, balloon) and back up the right to
the spiked top exit - and every spike crossing needs a frame that jumps the
spike's danger band (a dash's speed, a dash in hand). A position-only
distance knows neither the dashes nor the bands (it widens `djump`; the
probe's "VIOLATION"s are its premise checks on those widened outcomes), so
it measures the loop's length, not its cost. It still cuts 83-100% of the
late frames' states (h152 probe), which is what made h170/h190 fit.

A beam search over exact states (`rewrite beam`, branch `mod-beam`, WIP,
paused for the optimum): width 20000, no deaths, by d: it survives the
spawn pocket (alive through f218, 20k states) and reaches d 20 at (105, 115)
on f188, without an exit by f218.

### The synthetic mid win (2026-10-11)

`--win-at 56..72,104..112` (the open floor right of the spring), level -1
seeded with the rectangle's nodes (commit "Level -1 with a win rectangle":
the table's start d 62). Refuted at h95 (14 s), h105 (30 s), h115 (86 s),
h125 (299 s), h140 (607 s): the mid win needs more than 140 frames. The beam
(branch `mod-beam`, width 20000) reaches the rectangle on f168; that route
replays identically in the reference engine and on a real PICO-8 (the mod's
own Lua, `--balloon-seeds 0`). So the mid optimum is in 141..168.

h155 runs out of memory in the ARC phase (the forward finished: 1.047B
states, 39.4B edges, tree 191 GB at format 13, 127 GB at 14). Measured at
h140, `arc_dp::load`: 114.7M marked nodes (29% of the forward's 390M
states), 2.08B edges kept (16% of 12.8B); `graph built` 31.2 GB anon =
~49 B a node (marks 16, ids 8 - a copy of the marks' ids, out_at 8,
pred_at 8, layers 4, until 4, win 1) + 12 B an edge (`Out` dst+xfer 8,
`preds` 4); the backward +2.6 GB, fingerprints' phase peak 40.7 GB. h155's
BFS marked 371M nodes (the `[bfs]` lines); at h140's 18 edges a node that
is ~6.7B edges: ~18 GB of nodes + ~80 GB of edges before the backward.

### The mid synthetic win SOLVED by the objects ladder: 142 (2026-10-11)

`search --level r0sxhne,r0sxhe,r0sxe --to 168 --win-at 56..72,104..112
--level-minus-one 168,5 --cart MOD --concrete-balloon-seeds 0 --prefer
beam-mid-168.txt` (develop e7e9f78: the minimal push backward, format 14):

| level | forward | arc bound | known route (the beam's f168) |
|---|---|---|---|
| r0sxhne | 3.20B states, 2527 s (OOM at 60G at f158; resumed under 95G, peak 71 GB at f159's 199M), 426 GB | 141 (backward 1828 s, anon peak 24.6 GB) | survives every step |
| r0sxhe | 362k states, 9 s | 142 | survives |
| r0sxe | 641k states, 9 s | 142 | survives |

The concrete search's depth-first try at the bound f142: a win after 165
steps (0.1 s). OPTIMAL 142 (`lad168/witness_frame_142.txt`). The 141 of
`r0sxhne` was the `n` widening (objects' phases and countdowns): dropping
`n` gives 142; `h` changes nothing. Replay: the minimal cart on a real
PICO-8 reaches (61, 105) on f142; the mod's own Lua diverges at f114 - two
jump presses that fire nowhere in the minimal cart (f113, f139) fire from
the original's jump buffer. With them removed (`mid142-canon.txt`, as
category_runner's `canon_jumps`) both carts agree on every frame and reach
(61, 105) on f142.

### The coarse rung is not a bug: `h` merges, `n` does not (2026-10-11)

The ladder's finer rungs looked 10,000x smaller (r0sxhe 362k, r0sxe 641k
states against r0sxhne's 3.2B) only because they are FILTERED by the coarse
rung's arc-marked nodes. Run directly (no ladder), mid target, level -1:

| level | h | total states | per frame f80 / f100 / f120 | peak memory | wall |
|---|---|---|---|---|---|
| r0sxhne (mid140) | 140 | 0.39B | 2.33M / 5.55M / 9.35M | | |
| r0sxhe | 142 | 0.35B | 2.33M / 5.41M / 7.95M | 5.6 GB | 183 s |
| r0sxe | 142 | 1.35B | 8.37M / 20.5M / 30.5M | 20 GB | |

r0sxhne and r0sxhe are IDENTICAL up to f80, and r0sxhe is smaller after:
`n` (the balloon's and the spring's phases widened) merges nothing in this
room and loosens the bound (141 against 142). `h` (held buttons unknown)
is the 3.8x. RECOMMENDED: run this room at `r0sxhe` alone - the bound of
the exact objects at the coarse rung's cost; no ladder.

What grows is the player: f140's dominant shape (27.6M of 33M rows, every
fruit gone) has 14,846 distinct `spd.x` (110 x, 116 y, 102 `spd.y`); one
cell, (58,102), holds 9,432 rows with 1,911 `spd.x`. They are the exact
16.16 sums of the accelerations (0.6, 0.4, 0.15, 1.5, 1.0606) along
different input histories, and the spring's `spd.x * 0.2`: inherent to the
exact speed (the `s` of `r0sx`), not a widening.

### Benchmark and projection (2026-10-11)

- `tools/mod_bench.sh make|run|list`: single-frame fixtures cut (hard links)
  from finished trees under /var/tmp/mod-fixtures, the level -1 table pinned
  beside each, the first run's f/e lines pinned. he100 (r0sxhe f100: 154M
  emissions, 2.2 s), he120 (r0sxhe peak f120: 256M, 3.7 s), hne150 (r0sxhne
  mid155 f150: 1.46G emissions, 12.3 s). 16 threads.
- `tools/mod_project.py fit LOG...` / `project`: forward wall = a x
  emissions (a 11-21 ns at 16 threads, load-dependent; lad168 13.6), the
  wave's memory 2 GB + 292 B a row in + 68 B a new state, edges 3.0 B
  (format 14 + edges v5), the backward 92 ns an in-edge (14.6% of the
  edges read). States per frame N_H(f) = U(f) A(H - f): U the unfiltered
  forward (measured to f152, +9.3%/frame then), A the share level -1
  admits at slack s (from the exit runs h170/h190).
- Calibration: the mid target at h168 projected from mid140+mid155: 2.69B
  states, 0.5 h, 370 GB (actual lad168: 3.20B, 0.7 h, 426 GB); the exit at
  h190 from h170: 2.05B (actual 2.13B).
- The exit (r0sxhne counts; r0sxhe the same or smaller):

| H | states | peak frame | forward | disk now / at 0.15 B an edge | wave memory | backward |
|---|---|---|---|---|---|---|
| 195 | 3.1B | 131M | 0.6 h | 432 / 51 GB | 49 GB | 0.5 h |
| 200 | 4.8B | 205M | 0.9 h | 656 / 78 GB | 76 GB | 0.8 h |
| 205 | 7.3B | 319M | 1.4 h | 1005 / 119 GB | 117 GB | 1.1 h |
| 210 | 11.3B | 497M | 2.1 h | 1548 / 184 GB | 181 GB | 1.8 h |
| 220 | 27B | 1.2B | 5 h | 3.7 / 0.44 TB | 437 GB | 4.2 h |

  The wall is not the limit: the FRONTIER's memory (~290 B a row in) is,
  past H ~ 203 on this machine, and disk past ~200 on edges v5 (the
  target-region bundles, 0.08-0.25 B an edge, move that to ~215). The
  projection's big uncertainty is U past f152 (extrapolated at +9.3%/frame;
  a 3%/frame decay of that rate halves h220).

### Stitching the exit route from the mid point (2026-10-11)

`search --start-inputs PREFIX` (a search from the state a route prefix
reaches) and `search --all-wins FILE` (every concrete win inside W).

- Validated on this room: the beam's mid route split at f100 - from that
  state the optimum to the mid rect is 42, so 100 + 42 = 142, the proven
  optimum from the load; the beam's own suffix survives every pruning step.
- The f142 mid states (`--all-wins`, r0sxhe h142, 1.8 s of concrete BFS):
  8,013 (4,840 without the held buttons), all at y 105, x 61/62/64, every
  strawberry taken, spring and balloon identical; 22 classes by (speed,
  djump, freeze, dash_time, grace): 1,506 with the dash in hand (spd 1.85,2
  or -2,-2), 6,507 starting a dash that frame in one of 9 directions. f143-150
  (h150): W too loose for the BFS (x1.5 a layer, 454k states and 32 GB at
  layer 132): stopped.
- From the canonical f142 state (`mid142-canon.txt`, dash in hand), r0sxhe,
  `--balloon-seeds 0`, level -1:
  - to the real exit: REFUTED at h80 (first remainder-free win f68; forward
    19 min, 107M states at its peak frame, 148 GB, 41 GB): the stitched
    route is >= 142 + 81 = 223 (the room's bound: >= 191);
  - to the right shaft's foot (`--win-at 100..121,64..96`, after the
    balloon): REFUTED at h30, 40, 50, 60, 70 (h70: forward 16 min, 45M a
    frame at f49, first remainder-free win f26).
- The beam (`rewrite beam`, branch mod-beam merged here) from the same
  state toward the exit by level -1's d stalls at d 22-25 in the bottom
  pocket (y 110-128) for 30 frames.

### On the new storage: the second half and the waypoints (2026-10-11, develop a4c3dfc merged)

New storage numbers (fm85, fmp95): 11.1 ns an emission (16 threads), edges
0.06 B (were 3.0), frames ~9 B a state now dominate the disk; the forward's
anonymous memory is still ~215-295 B a frontier row (the decoded rows):
RAM, not disk or time, is the limit.

| run (r0sxhe, level -1, balloon seed 0) | target | H | states | forward | peak RSS | disk | verdict |
|---|---|---|---|---|---|---|---|
| fm | exit, from mid f142 | 85 | 3.68B | 1112 s | 40.6 GB | 43 GB | REFUTED |
| fmp, `--prune-rect=-8..63,-16..39` | exit, from mid f142 | 95 | ~8B | 2830 s (+335 s arc) | 78 GB (f77: 351M rows in) | 101 GB | REFUTED (heuristic) |
| wp | shaft x78-95 y8-23, from mid | 64 | | 121 s | | | REFUTED |
| wpa | right of the strip, from mid (13 mid states) | 40 | | 12-64 s | | | all REFUTED |
| far | x 88-95 under the strip, from mid | 30 | | 57 s | | | OPTIMAL +11 (route 153) |
| cross160 | strip's far side x96-103 y105-120, from load | 160 | | 335 s | 12 GB | | OPTIMAL 157 (252 states f157-160, all djump 0) |
| bal185 | the balloon x112-121 y96-112, from load | 185 | | 1905 s | 64 GB | 66 GB | REFUTED (remainder-free first win f107) |

- The prune (above the topmost berry, x <= 63, y <= 39) cut 17M of ~8B rows
  (0.2%): the states do not go back there. Not worth its caveat here.
- The strip: row 14 (y 112-119) cols 10-11 are up-spikes between the
  ceiling (row 13, y <= 111) and the floor (row 15): a player crosses at
  y 109-110 (or moving up) only - a dash along the ceiling. From the mid
  states it ends at (88,109) on f153 (`mod00-furthest`, replayed on PICO-8
  with the mod's Lua); the earliest crossing from the load is f157, with
  the dash spent, falling at ~1.5 px a frame: every such state dies in the
  pit within 10 frames (`--start-inputs` from it: refuted to +75, the
  forward empty by +10). The balloon (x >= 113 to touch, the only dash
  refill and the only way up into the right area) is not reached by f185.

### Keys-only (develop 2ef9213, merged b8edfbe) and the room's geometry (2026-10-12)

Keys-only memory: +105 from mid, f67 (365M rows in, 400M new) at 16 GB
(~44 B a row in, against ~250 before). The +105 run was stopped at f67:
the frames grew 9-10%/frame (f60 211M -> f67 400M) and project to ~2.5B a
frame at its peak (~f87, ~20 min a frame, ~110 GB): over the RAM and ~10 h.

Where the route has to go (tiles from the map; 17/27/43/59 are the
up/down/left/right-facing spikes, non-solid):
- The exit (row 0, cols 9-11, down-spikes) is reached only from the
  exit chamber (cols 10-11, rows 1-3), which opens only right, along the
  row-3 corridor (y 24-31, x 80-127; down-spikes above at row 2 cols 12-15,
  up-spikes below at row 4 cols 9-15).
- The corridor is reached only from below, through row 4's up-spikes
  (safe moving up) and row 5's one gap, col 12 (x 96-103, y 40-47), from
  the RIGHT SHAFT (rows 6-12, cols 11-15: x 88-127, y 48-103; its left side
  col 10 left-facing spikes over the solid col 9).
- The shaft is reached only from the PIT (rows 14-15, cols 12-15, no floor:
  falling out is death) through col 15 at row 13 (x 120-127, y 104-111).
  The pit holds the balloon (x 120, y 112; touched at player x >= 113,
  y 104-117) - the only dash refill on the right.
- The pit is reached from the mid area only through the STRIP (row 14,
  cols 10-11: up-spikes, ceiling above at y <= 111, floor below): player y
  109-110 or moving up - a dash along the ceiling.
So: strip (dash) -> pit -> balloon -> up into the shaft -> up 56+ px to the
gap -> corridor -> left -> up through the exit spikes. Measured: the
earliest strip crossing from the load is f157, its dash spent, ~1.4-1.55
px/frame right and falling: it reaches x ~107 at y ~126 (dead), short of
the balloon's x 113.

### bal200 stopped; is the balloon reachable at all? (2026-10-12)

bal200 (the balloon from the load, keys-only, r0sxhe, level -1 200):
stopped at f181 - 731M new states a frame (+6.8%/frame), 800 s a frame,
10.85B visited, the scope at 71 GB of its 80 GB cap: it would have hit the
cap around f186-190 (frames to f200 another ~8 h), and up to f185 is
already refuted. Wait for the unified RegionSet (visited 2.74 -> 1.34 B a
state, the frontier 1.88 -> 0.95).

The physics says the balloon may be unreachable at ANY horizon, which would
make the right side - and the exit - unreachable with max_djump 1:
- The strip (x 74-94 of player.x) is safe only at player.y 109-110 with
  yspd >= 0 (up-spikes: (y+7)%8 >= 6 kills; the ceiling stops y < 109) or
  while moving up. Air speed without a dash is <= 1 px/frame (`maxrun`);
  a dash gives 5, 3.5, 2, 2 px over its 4 frames (12.5 px, y held), then
  2 px decaying 0.15/frame whatever the input, and 3 frames of gravity
  before y reaches 111: ~18-21 px in all - just the strip's width. So
  every crossing spends the dash and leaves the strip at x ~95-98 with
  spd.x <= 1.7 (measured: 1.25-1.55) and y 109-111.
- No refill on the way (no ground, wall or refill object at the strip's
  far end; the spring and the strawberries are on the left).
- From there the balloon needs x >= 113 while y <= 117: 15+ px in ~5
  frames of falling - x reaches ~104-105 by then (the 252 crossings at
  f157-160 measured: all die in the pit within 10 frames).
So either the intended route uses a mechanic we do not model (it would
show as a replay mismatch - none so far), or the room needs something the
argument above misses. Checking it exactly is cheap: the crossing states
are few (252 by f160); `--start-inputs` from each, `--win-at` the balloon,
+20, settles their whole future.

Checked exactly: the 252 crossings of f157-160 (20 distinct states by
position, speed, remainder, djump) - `--start-inputs` each, `--win-at
113..127,100..120` (the balloon), +20: all 20 REFUTED (7-16 s each; what
survives to +20 is the respawn after the death).

The minimal cart's mechanics against the original (the mod's Lua is the
original's; `p8cart.luadiff` of lua/celeste-minimal.lua against it):
every refill is there - the spring (`hit.djump=max_djump`), the balloon,
the strawberry and the fly fruit, the orb (max_djump 2) - in the same
order within the frame; the player's update differs only by the dropped
jump buffer (a press is replayed at its firing frame: reachability is the
same), the smoke/sound/shake/hair/sprite code (no object reads it), and
the tracer's rewrites of `obj.move` (`flr(r + 0.5)` as `__split_by_flr`
of `r + 0.5`, the same numbers); `_update` skips `move(0, 0)` (a no-op).
The strawberries refill the dash but all three are on the left (x 16 and
40); the room's other refill sources (the spring at (48,96)) are left of
the strip too. Wall jumps: the only wall near the strip is the left pit's
left side (col 5, x 40-47): a kick of 2 px/frame right, 25+ px short of the
strip. So nothing the minimal cart lacks changes the strip argument.

### Exact reach over bal200 (2026-10-12, `rewrite reach`, branch exact-reach)

`rewrite reach --checkpoint-dir /var/tmp/mod-ck/bal200 --to 200` (the
search's settings; out: /var/tmp/mod-ck/bal200-reach, cells per frame in
bal200-reach-cells.txt, log logs/reach200h.*): from the load at its exact
remainder, every reached state's remainder set pushed along its recorded
edges through their transfers, frame by frame, W = everything. 75 min, 30
threads, RssAnon peak 79 GB (the scope's cap raised to 90G at f178; the
held out-edges, the loaded layers and the frames' sets). A set past 1024
segments (a cloud of the remainders many paths leave) is snapped outward
to the finest 2^k grid that fits: that state and everything it reaches
are INEXACT (supersets), 43% of the states at f143, 87% at f200 - every
count below is an upper bound on the exact one. UI: `mod00-reach-h200`,
level 2, "exact reach among states that level -1 kept for the balloon by
h200".

Level -1: the forward dropped successors only from f174 on (sources noted
per frame, `l1_noted_sources`: 4.3M at f174, 162M at f182, 0.1M by f198).
A dropped successor of a state reached at t comes at t+1 or later and was
dropped because level -1 (sound) bounds it out of the balloon by h200: for
the balloon the reach is complete; for "where can the player be" it lacks
only such too-late states.

- Right-most reached x per frame: 32 to f45 (the spawn), 65 by f100 and
  still at f140, 82 at f150, 95 at f156, 104 at f160, 114 at f165, 116 from
  f189.
- The strip: first crossed at f156 (x 95, y 111); at f157 x 96-97, y 111.
  Every decoded state past it (865, x >= 96 and y >= 100, at most 20 a
  frame) has djump 0; the exact ones at f157: spd.x 1.25-1.55, spd.y +0.42
  to +0.84 (falling), dash_effect_time 1-3 - the dash spent on the strip,
  as the physics argument says.
- The balloon (x >= 113, y 104-117): never reached. x >= 113 only at y
  124-127 (falling out of the pit, f165 on). Closest: (110,119) at f186,
  3.6 px short (x +3, y -2); 5 px at f163.
- Against the forward: reached at f100 76k states (its layer 5.4M), f143
  506k (30.8M), f180 5.1M (719M); ever reached 21.0M of the 13.3B visited
  (0.16%).

For an exact-remainder forward (the coordinator's question; `summary.txt`,
`rem_stats.tsv`, `reach.tsv` in bal200-reach):
- The sets are small clouds: per axis 5-64 segments for 27-60% of the
  states, 1 segment for 15-47%; a single point or a span <= 1/256 of the
  circle (1/256 px) for 98.7%+ of the EXACT states at f100, f143, f165, f185
  and f200. The wide ones (> 1/256: 33-60% of all states by f185-200) are
  the coarsened (<= 0.4% of the exact ones).
- First arrival in (state, remainder) - a state expanded again whenever
  its union of sets grows: entries per state 1: 9.74M, 2: 5.42M, 3-5:
  2.00M, 6+: 3.73M; 89M expansions (a lower bound: unions capped at 32
  segments) to 223M (a state entering at every frame it is reached),
  against the forward's 13.3B: 0.7-1.7%.
- Edges: the states ever reached have 295M distinct out-edges (the forward
  emitted 550B lanes); 191M (65%) ever pushed a non-empty set.

### One dash cannot reach the balloon: the geometric limit (2026-10-13)

The balloon is touched when the player's x >= 113 (x is clamped at
121) and its y is in (by - 9, by + 6), where by is in [110, 114]: the
region x 113-121, y 102-119 over all phases. bal200's rect (y 104-117)
was 2 rows too narrow at each end, but that changes nothing: no reached
state has x >= 113 above y 124. The closest exact approaches are
(110,119) at f186 and (112,121) at f187.

The limit, in the exact reach: no state is ever at x >= 97 with y <= 110
(the ceiling height), at any frame f158-f200. Level -1 dropped nothing
before f174. Why:
- The strip (row 14 cols 10-11, up-spikes, ceiling at y <= 111, floor
  block row 15 cols 10-11) is crossed only at y 109-110, or while moving
  up. Without the dash, the hover under the ceiling lasts 2-3 frames
  (rem.y reset to 0 by the bump, then gravity). That is ~6 px against
  the strip's 22, so the crossing spends the dash, and the dash must start
  at y 109/110.
- y 109/110 under the ceiling at x >= 66 is reached only by a grace jump
  that bumps the ceiling. The only ground on the left is the spring's
  base (row 13 col 6: the spring compressed, or not touched). The ceiling
  block's left face (col 9 row 13, x 72) stops x at 65 until y >= 109.
  Falling 96 -> 109 takes the whole grace window, even with a dash's
  downward speed. So the bump is at x <= 69 and the dash starts at x <= 72.
- A horizontal dash moves 6,4/5,3,3,3 px at constant y. It ends at x <= 91,
  and the player leaves the strip at about (96,111) with spd.x <= 1.55,
  falling, djump 0 (cross160's witness: f157).
- From there, x 113 comes at y >= 124. The down-spike at row 14 col 13
  (kill: hitbox overlapping it, (y+3)%8 <= 2, yspd <= 0: y 109-111 over
  x 98-110) kills any exit that is later or higher.

Checked independently on that crossing, frame by frame f145-f164 (the
jump, the bump, the dash, the death at (111,126)): real PICO-8 running
the mod's own Lua (`replay.py --lua --begin-game`) and Celia fixp with
the mod cart (CELIA_TRACE; Celia loads level 1 with max_djump 1) both
match our engine exactly.

The bal200 TREE (the forward, remainder widened) holds right-side
states with the balloon popped and djump 1: 2.25M at f160, popped as
early as f135 (`rewrite scan-rect`). That is the widened remainder (a
player hovering forever; remainder-free first win f107); the exact
reach removes them.

With max_djump 2 (real PICO-8, `replay.py --max-djump 2`), the f157
crossing plus R,R,R and a dash up-right reaches the shaft floor at
(103,96) on f179. Celia's dash override (its +/- keys,
max_djump_overload) survives level loads and a TAS file does not record
it. Whether the room is meant for 2 dashes is a question for Philippe.
`--max-djump N` (recorded in the tree) runs such a game.

The 1-dash assumptions, each checked against the mod's Lua (2026-10-13):
- Every exact-reach state right of the strip (x >= 95, y >= 96), f157-f190,
  decoded (`rewrite reach --decode-rect 95..127,96..128 --decode-max`):
  18,222 states, all djump 0 and the balloon present (spr 22). 17,905 of
  them are inexact supersets. No refill ever happens on the right.
- Ground: only flag-0 tiles are solid (`solid_at`). Spikes (flag 1) cannot
  be stood on. The only standable tiles are the spring's base (row 13
  col 6) and the shaft floor (row 13 cols 10-13). Row 15 cols 10-11 lie
  under the strip: standing there is y 112, which kills
  (`(y+h-1)%8 >= 6`, yspd >= 0).
- The spring never hides: `hide_in` is set only by a breaking fall floor,
  and the room has none. It re-arms 10 frames after a bounce.
- The ceiling block's corner: `move` runs x before y, so a diagonal
  cannot slip past x 65 at y < 109.
- Wall jumps: the faces near the strip push LEFT (the ceiling block's
  left face x 72, the floor block's left face x 80). The pit's left wall
  (x 48) pushes right but sits under the spring base (bump at y 109,
  x <= 54).
- Strawberries: (16,40), (16,48), (40,72), all left of x 48: a refill
  there does not change the ceiling geometry.
- The down-spike at row 14 col 13 kills at yspd = 0 (`yspd<=0`).
- The jump buffer the minimal cart drops (and Celia keeps) fires a press
  at the first frame with grace or a wall in range. That is the same as
  pressing then: reachability is unchanged.

### With two dashes: optimal 164 (NOT the default game, 2026-10-13)

`search --max-djump 2 --level r0sxhe --level-minus-one 190,5 --to 190
--concrete-balloon-seeds 0` (/var/tmp/mod-ck/two190): forward 190 frames
(peak ~80M new rows a frame around f150, the forward's RSS ~11 GB; the arc phase's peak 47 GB), remainder-free first win f148,
ARC BOUND 164, concrete win 164 (depth-first at the bound, 310 steps):
OPTIMAL 164 from the load under max_djump 2, 39 min in all. Validated
(`tools/mod_submit.py --max-djump 2`): real PICO-8 leaves the room on f164
with both the minimal cart and the mod's own Lua; Celia fixp with the mod
cart and the override (CELIA_DASHES=2, applied on level 1 by a reload as
Celia's +/- keys do) finishes it in 118f (Celia's count). Without the
override it dies at frame 27. File: /var/tmp/mod/submit/TAS1-2dash.tas
(+ README: NOT a submission). UI: `mod00-2dash`.
