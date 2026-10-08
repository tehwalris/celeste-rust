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
| (2,0) 300m | 95 | 108 (TAS3, offset 25) | **100 - 8 FASTER** | `rewrite search --level r0sxh --to 100` (L-1 100,5): bound 100, witness in 393 steps; 47 min, 36 GB; the earlier failures ran `r0sxhn` (2.4x the states by f65) at 108 (below). Original cart f100, no diagonal dash; UCT 82f -> 74f. `tas/room_2_0_nodiag_frame_100.txt` |
| (3,0) 400m | 89 | 93 (TAS4, offset 27) | - | no level 0 fits: `r0sxhn` (fruit exact) 5.3M kept at f50, x1.46 a frame; `r0sxhf` (floors exact) 10.2M at f55, x1.5; `r0sxhfn` does not build (below); not solved (2026-10-08) |
| (1,3) 2600m | 127 | 135 (TAS26, offset 25) | **133 - 2 FASTER** | arc bound 133 at r0sxhn, witness; 153 s, 8.1 GB; original cart: TAS26 f135, ours f133; UCT: 109f -> 107f. `tas/room_1_3_nodiag_frame_133.txt` |
| (5,2) 2200m | 150 | 158 (TAS22, offset 25) | **157 - 1 FASTER** | arc bound 157 at r0sxhn, witness; 2027 s, 44.5 GB; original cart: TAS22 f158, ours f157; UCT: 132f -> 131f (TAS22's file has 3 inputs past the exit, which UCT's clean save trims). `tas/room_5_2_nodiag_frame_157.txt` |
| (3,3) 2800m | 172 | 184 (TAS28, offset 49) | **179 - 5 FASTER** | arc bound 179 at r0sxhn, witness; 262 s, 13.8 GB; original cart: TAS28 f184, ours f179; UCT: 134f -> 129f. `tas/room_3_3_nodiag_frame_179.txt` |
| (5,1) 1400m | 104 | 121 (TAS14, offset 25) | **119 - 2 FASTER** | `fit-big` (2026-10-07): `r0sxhn,r0sxh`, L-1 121,5, `CELESTE_TRIM_ROWS=1`, 60 GB; BENCHMARK_DATA.md |
| (3,2) 2000m | 128 | 152 (TAS20, offset 31, seeds 0) | **148 - 4 FASTER** | `rewrite search --level r0sxhn,r0sxh` with `arc_dp::reach` (L-1 152,5, `CELESTE_TRIM_ROWS=1`): bounds 133, 148; witness at 148 in 446 steps; level 0 13 min, level-1 forward 71 min, the arc phases ~25 min; peak 75 GB (the level-1 arc phase; below); exits f148 in the original cart under TAS20's seeds (0,0,0,0; not under 0.5 or PICO-8's rnd); UCT: 120f -> 116f. `tas/room_3_2_nodiag_frame_148.txt` |
| (1,1) 1000m | 94 | 94 (TAS10) | - | (validation, not run) |

## The three rooms that failed on size (2026-10-08, branch `nodiag-big`)

**(3,2) 2000m** (4 balloons, 1 fall floor). Level 0 `r0sxhn` with L-1 152,5:
the forward is cheap (127M visited, 13 min, 6.5 GB; tree 17 GB trimmed), the
arc bound is 133 against the reference's 152, and every way on from there
was too loose:
- the concrete search at level 0 alone (`--level r0sxhn`): 1.4x a layer from
  layer 33 (32k states at layer 43 of 152), stopped;
- the objects ladder with the old filter (every node whose W is non-empty):
  61.7M of the tree's nodes marked, the filtered `r0sxh` forward 15M kept at
  f106 and x1.12 a frame (exact balloons: `timer` counts 60 frames after a
  collection, a frame stamp until the balloon is back), stopped;
- the ladder at lower horizons, the level-0 tree reused (`--to H`): h138
  REFUTED in 96 s (level 1 dies at f43); h145 REFUTED in 17 min (level 1
  680 s, 4.4M kept at its widest). So the optimum is above 145.
`arc_dp::reach` (plans/architecture.md, "The search"): mark only the nodes
the rotation graph REACHES inside W from the start's remainder. h145: 38.0M
-> 486k nodes, level 1 680 s -> 16.7 s, the same answer. h152: 61.7M ->
1.65M nodes; the filtered `r0sxh` forward 4275 s, 425M visited, at most
21.3M kept a frame (f115), first remainder-free win f134; its arc phase
needs more than 50 GB (graph 51.3 GB built, OOM-killed in the backward).
With the trees kept and 72 GB (the machine had 105 GB available): the
level-1 arc phase marked 379M nodes over 2.66G edges (graph 51.1 GB,
built in 8 min), backward 283 s (640M spans over 200k distinct sets), anon
peak 74.5 GB, VmHWM 75.3 GB; level-1 arc bound **148**, and the depth-first
try at the bound found the witness in 446 steps: **OPTIMAL 148, four frames
under TAS20** (and above the 145 refuted directly). Verified like the
runner does (`/var/tmp/nd-big/verify.py`, its steps 3-4): exits during f148
in the ORIGINAL cart under TAS20's seeds 0,0,0,0, no diagonal dash, UCT
finishes it in 117 inputs (TAS20: 121); the upload file is
`tas/tasdatabase/nodiag/upload/TAS20.tas`. Under seeds 0.5 or PICO-8's rnd
it does not exit (the concrete search steps under the file's seeds).

**(3,0) 400m** (the fly fruit, 12 fall floors). No level 0 fits: with the
fruit exact (`r0sxhn`) every waiting frame's `step` is new (a frame
counter): 119.6k kept at f40, 5.3M at f50 (x1.46 a frame, 59 s a frame);
with the floors exact (`r0sxhf`) 10.2M at f55 (x1.5). The any% level
(`r0sxhfb`) had `b`, deleted. `r0sxhn` did not BUILD before `d15af76` (the
spawn shapes after the first frame were traced unbounded: every floor
"near", 2^12 outcomes); `r0sxhfn` still does not: with the fruit unknown
(`Symbolic::unknowns()`) a comparison with the unknown number mints an atom
that refuses merges (`state::merge_inner`, `refuse_selects`), and at `n` the
floors' countdowns ARE the unknown number, so each floor's `delay <= 0`
keeps its two paths apart: 8192 trace states (2 shapes) after the spawn
frame's `foreach`, then minutes of merge attempts. Tried on branch
`nodiag-big-fn-wip` (`76c5a82`, not merged): a comparison reading a
countdown field mints a COUNTDOWN atom that stays three-valued as at `n`
(joined, split by the split pass, not forked on escape) while the fruit's
keep refusing, and the no-player chain runs with the fruit exact. Then
`r0sxhfn` BUILDS (307 kernels, the walk 158 s) and runs to f42 (f40 92,350
kept, the same as `r0sxhf`: no gain yet that early), and stops on a KERNEL
COVERAGE GAP - lanes falling at `spd.y` 2 decline on an error disjunct
(`CELESTE_KERNEL_EXPLAIN`); not investigated further.

**(2,0) 300m** (2 springs): **OPTIMAL 100, eight frames under TAS3's 108.**
What had failed was the HORIZON and the level, not the room: `r0sxhn` (the
springs abstract: "maybe bounce" at every spring) is larger than `r0sxh`
here (f65, before any level -1 cut: 10.3M kept against 4.28M; f75, L-1 at
108 against 100: 98.2M against 16.6M), and the level -1 cut (`t + d > H`)
moves with H. `rewrite search --level
r0sxh --to 100` (L-1 100,5, `CELESTE_TRIM_ROWS=1`): the forward peaks at
40.1M kept (f83), 465M visited, 2830 s, 24 GB; the arc phase 9.9M marked
nodes, 162M edges, bound 100; the try at the bound wins in 393 steps. The
level's one widening is `h`, so 100 is a lower bound and the witness makes
it the optimum. Verified as the runner does: exits during f100 in the
ORIGINAL cart, dashes at f38 (none held), f75 and f90 (up), no diagonal;
UCT finishes it in 75 inputs (TAS3: 83; 82f -> 74f). Upload file
`tas/tasdatabase/nodiag/upload/TAS3.tas`, witness
`tas/room_2_0_nodiag_frame_100.txt`.

Before that, measured on the way: nodiag is SMALLER than any% frame for
frame (`r0sxh`, f62: 3.22M kept, 26.2M visited, against 5.56M / 45.7M);
what grows is the speed - at the hottest cell (48,88), f62, 548 `spd.x` and
49 `spd.y` values, 8,433 speed pairs against 465 states without the speed
(`col-census --cell`); `spring` multiplies `spd.x` by 0.2 and the `appr`
steps keep the odd fractions. A search at 108 itself (the community TAS's
frame, the obvious `--ceiling`) was not needed: the optimum is 100.

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

## Checked in the community tool, UniversalClassicTas (2026-10-06)

`tools/uct/validate.sh LEVEL FILE...` runs files through UCT
(CelesteClassic/UniversalClassicTas, 2022-01-09) headlessly: the official
LOVE 11.5 AppImage, extracted, with `SDL_VIDEODRIVER=offscreen` (no X, no
pacman), a `celeste.p8` built from the original cart's Lua + cart/ data, and a
small driver (`tools/uct/driver.lua`) that presses W (load), D (restart) and U
(clean save: play back, trim at the level end). All six files - the three
database files and ours - finish and clean-save unchanged: 600m 65 -> 64 inputs
(64f -> 63f), 1700m 64 -> 62 (63f -> 61f), 2900m 82 -> 77 (81f -> 76f), the
same counts as the real PICO-8. Upload files, as UCT's clean save writes them
(trailing commas; 600m's one balloon seed written as `[0,]`):
`tas/tasdatabase/nodiag/upload/TAS{6,17,29}.tas` (and, verified the same way, TAS{22,26,28}.tas on 2026-10-06 and TAS20.tas on 2026-10-08). To submit: one Discord
message `!uploadtas classic nodiag` with the three files attached.
