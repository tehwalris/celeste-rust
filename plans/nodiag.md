# No Diagonal Dashes% (2026-10-04)

`CELESTE_NODIAG=1` (src/trace/cart.rs): the cart's diagonal dash arm (`if
input~=0 then if v_input~=0 then`, inside `if this.djump>0 and dash`) raises
(`nil > 0`); a raise has no successor, so a diagonal dash start is not a move,
exactly, at every level, in the kernels and the reference engine alike. A dash
with no direction held (horizontal, facing) stays legal. The 30 community
nodiag TASes press no diagonal dash. A reference frame path that raises ends
in no state (`refdriver::run_frame_all`), not an error.

References: the community nodiag TAS replayed in the ORIGINAL cart at the
room's any% prologue offset (the earliest exit), our frame counting.

| room | any% (ours) | nodiag ref | ours | how |
|---|---|---|---|---|
| (4,2) 2100m | 71 | 71 (TAS21) | 71 | ladder (any% recipe, L-1 71,5): h70 refuted at level 7 - validation |
| (5,3) 3000m | 79 | 96 (TAS30, offset 31) | 96 (tie) | `rewrite search --level r0sxhn,r0sxh` (L-1 96,5): r0sxhn bound 92 (no concrete win at 92); r0sxh (objects exact) bound 96 = a sound lower bound, witness at 96 (311 steps); 363 s, 9.6 GB. The first run's count-up refutations (92-95) used the unsound memo; this proof does not rely on them. `tas/room_5_3_nodiag_frame_96.txt` (PICO-8: exits f96) |
| (7,0) 800m | 84 | 99 (TAS8, offset 23) | 99 (tie) | `rewrite search --level r0sxhn,r0sxh` (L-1 99,5): bound 99 at both levels, witness 287 steps; 1302 s, 29.5 GB; PICO-8 exits f99. `tas/room_7_0_nodiag_frame_99.txt` |
| (4,3) 2900m | 85 | 111 (TAS29, offset 29) | **106 - 5 FASTER** | arc-only `rewrite search --level r0sxhn,r0sxh` (L-1 111,5): r0sxhn bound 104, no concrete win at 104; r0sxh bound 106, concrete witness at 106 (392 steps); exits during f106 on a real PICO-8 in celeste-minimal AND the original cart, seeds 0 / 0.5 / rnd. `tas/room_4_3_nodiag_frame_106.txt` |
| (0,2) 1700m | 81 | 89 (TAS17, offset 25) | **87 - 2 FASTER** | `rewrite search --level r0sxhn,r0sxh` (L-1 89,5): r0sxhn bound 87, witness 87; 316 s, 14.5 GB; exits f87 on a real PICO-8 in celeste-minimal and the original cart. `tas/room_0_2_nodiag_frame_87.txt` |
| (5,0) 600m | 77 | 94 (TAS6, offset 29, any seed) | **93 - 1 FASTER** | `rewrite search --level r0sxhn,r0sxh` (no L-1: balloon y): bounds 91, 93; witness 93; 132 s, 5.1 GB; exits f93 on a real PICO-8 in both carts for every seed tried. `tas/room_5_0_nodiag_frame_93.txt` |
| (0,1) 900m | 100 | 108 (TAS9, offset 27) | 108 (tie) | bounds 106, 108 (r0sxh); 619 s, 17.5 GB. `tas/room_0_1_nodiag_frame_108.txt` |
| (2,0) 300m | 95 | 108 (TAS3, offset 25) | - | level 0 explodes (122M states at f76, 620M visited, with level -1): the spring's speed spread; stopped |
| (1,1) 1000m | 94 | 94 (TAS10) | - | (validation, not run) |

## The concrete count-up (arc-search --witness)

The arc optimum over coarse objects is only a lower bound. The witness DFS is
EXHAUSTIVE (all 64 inputs, every `rnd` leaf, a fully explored concrete state
remembered per frame) and prunes only by W, which contains every concrete
winner - so "no witness within f" is a proof about the real game, and counting
f up from the arc optimum to the horizon, the first f with a witness is the
concrete optimum. Room (5,3): f92 1.3 s, f93 8 s, f94 48 s, f95 112 s, f96
276 s (100k concrete steps). The dead-state memo must key the EXACT concrete state (it keyed the
level-0 key until 2026-10-05: unsound, every 'none within' is rechecked).
The soundness also rests on the projection of a
concrete state onto a level-0 node matching the tree's keys (a mismatch would
prune a real winner); `rewrite follow` agrees along the spawn and first moves.

## The improvements against the community TASes (checked 2026-10-05)

The tasdatabase clone is at the remote's HEAD (bf184fa, 2026-07-25; 2900m
nodiag was last improved there 2026-06-22, 85 -> 81). Both TASes are replayed
in the ORIGINAL cart (`--lua celeste_ocaml/celeste.lua --begin-game`) in the
same harness: the community TAS behind the room's prologue (its earliest
exiting offset; every earlier offset never exits), ours as is. The player
first exists on the same frame in both, and both exit during the frame
counted - so the difference is the route, not counting. In the database's
terms (frames = inputs - 1): 600m 64 -> 63, 1700m 63 -> 61, 2900m 81 -> 76.

Frame-by-frame alignment (`tools/align_tas.py`, positions per frame):

- (5,0) 600m, -1: identical through f68. Ours dashes up at f70 from x=89
  instead of f71 from x=91, is 2 px higher from there, and the closing
  wall-jump chain runs one frame ahead.
- (0,2) 1700m, -2: TAS17 dashes left at once (f26) and climbs; ours jumps,
  dashes DOWN at f28 to land early, refills on the ground and jumps at f34 -
  both reach the up-dash at f40. TAS17 is then held against a wall at x=17
  for f50-52 while pressing right; ours starts its right run from x=14 on a
  line that is not blocked, up-dashes at f67 (TAS: f68) and jumps at f82
  (TAS: f83).
- (4,3) 2900m, -5: TAS29 uses three dashes (R f57, U f71, U f98); ours four
  (R f53, U f67, R f86, U f93): a lower early line puts the first right dash
  4 frames earlier, and an extra dash refill around (84,30) that TAS29 does
  not take allows a second right dash. Ours exits with balloon seeds 0, 0.5
  and PICO-8's rnd.

## Submission files (tasdatabase format)

`tas/tasdatabase/nodiag/{600m,1700m,2900m}_nodiag.tas`: `[seeds]` + inputs
from the room's first controllable frame (our witness minus the spawn
prologue), no trailing newline, the same seed list as the file they replace.
Replayed on a real PICO-8 in the original cart, same harness for both (the
community file behind the prologue; an earlier offset never exits; the DB's
frames = exit frame - prologue - 1, which reproduces every listed count):

| room | DB file, listed | its replay | ours | our replay |
|---|---|---|---|---|
| 600m | TAS6, 64f | exit f94 = 64f | 63f (64 inputs) | exit f93 = 63f |
| 1700m | TAS17, 63f | exit f89 = 63f | 61f (62 inputs) | exit f87 = 61f |
| 2900m | TAS29, 81f | exit f111 = 81f | 76f (77 inputs) | exit f106 = 76f |
