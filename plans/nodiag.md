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
| (3,0) 400m | 89 | 93 (TAS4, offset 27) | 93 (tie) | `rewrite search --level r0sxhfn,r0sxhn,r0sxh --to 93` (L-1 93,5, `CELESTE_REGION=8,6`, `CELESTE_TRIM_ROWS=1`): `r0sxhfn` bound 76 (no concrete win at 76), `r0sxhn` filtered by reach bound 93, witness at 93 in 93 steps; level 0 ~2.6 h, level 1 102 s; peak 35 GB. Original cart f93, no diagonal dash; UCT 66 inputs, both files. `tas/room_3_0_nodiag_frame_93.txt` (below) |
| (1,3) 2600m | 127 | 135 (TAS26, offset 25) | **133 - 2 FASTER** | arc bound 133 at r0sxhn, witness; 153 s, 8.1 GB; original cart: TAS26 f135, ours f133; UCT: 109f -> 107f. `tas/room_1_3_nodiag_frame_133.txt` |
| (5,2) 2200m | 150 | 158 (TAS22, offset 25) | **157 - 1 FASTER** | arc bound 157 at r0sxhn, witness; 2027 s, 44.5 GB; original cart: TAS22 f158, ours f157; UCT: 132f -> 131f (TAS22's file has 3 inputs past the exit, which UCT's clean save trims). `tas/room_5_2_nodiag_frame_157.txt` |
| (3,3) 2800m | 172 | 184 (TAS28, offset 49) | **179 - 5 FASTER** | arc bound 179 at r0sxhn, witness; 262 s, 13.8 GB; original cart: TAS28 f184, ours f179; UCT: 134f -> 129f. `tas/room_3_3_nodiag_frame_179.txt` |
| (5,1) 1400m | 104 | 121 (TAS14, offset 25) | **119 - 2 FASTER** | `fit-big` (2026-10-07): `r0sxhn,r0sxh`, L-1 121,5, `CELESTE_TRIM_ROWS=1`, 60 GB; BENCHMARK_DATA.md |
| (3,2) 2000m | 128 | 152 (TAS20, offset 31, seeds 0) | **148 - 4 FASTER** | `rewrite search --level r0sxhn,r0sxh` with `arc_dp::reach` (L-1 152,5, `CELESTE_TRIM_ROWS=1`): bounds 133, 148; witness at 148 in 446 steps; level 0 13 min, level-1 forward 71 min, the arc phases ~25 min; peak 75 GB (the level-1 arc phase; below); exits f148 in the original cart under TAS20's seeds (0,0,0,0; not under 0.5 or PICO-8's rnd); UCT: 120f -> 116f. `tas/room_3_2_nodiag_frame_148.txt` |
| (1,1) 1000m | 94 | 94 (TAS10) | - | (validation, not run) |
| (6,1) 1500m | 89 | 93 (TAS15, offset 25) | **92 - 1 FASTER** (UCT: 66f against 67f) | 2026-10-08, split frame, `r0sxhn`, level -1 (186,5) steps, one kept tree: arc bound f90 (no concrete win at 90); `--to 91`: no concrete win inside W (exhaustive, 153k steps); `--to 92`: the breadth-first search's first win at 92 (983 s with the tree reused). Jumps canonicalized; original cart (seeds 0,0) exits f92; UCT 67 inputs = 66f. `tas/room_6_1_nodiag_frame_92.txt`, `tas/tasdatabase/nodiag/upload/TAS15.tas` |
| (6,0) 700m | 70 | 74 (TAS7, offset 23) | **73 OPTIMAL - 1 FASTER** (UCT: 49f against 50f) | 2026-10-08, split frame, the reach-filtered ladder `r0sxhfp,r0sxh,r0sx` (no level -1) on one kept `r0sxhfp` tree: that forward to step 148 (h74) took 2526 s, 884M visited, at most 20M kept a step, the door 21.8 GB, peak 28.4 GB (under a 55 GB cap; it had stopped at the 30 GB cap near step 140 before); `--to 69` REFUTED at `r0sxh` (126 s); `--to 74`: bounds f68 (`r0sxhfp`, no concrete win at 68), f73 (`r0sxh`, 856k marked nodes), the try at 73 wins in 134 steps (248 s). Original cart on PICO-8 exits f73, no diagonal dash; UCT 50 inputs = 49f. `tas/room_6_0_nodiag_frame_73.txt`, `tas/tasdatabase/nodiag/upload/TAS7.tas` |
| (1,2) 1800m | 103 | 118 (TAS18, offset 27) | **113 OPTIMAL - 5 FASTER** (UCT: 86f against 90f) | 2026-10-08, the reach-filtered ladder `r0sxhnp,r0sxhn,r0sxh,r0sx` (level -1 refused with `p`) on one kept level-0 tree: `--to 112` REFUTED at `r0sxh` (192 s); `--to 118`: bounds 107, 109, 113, the try at 113 wins in 176 steps (359 s, peak 11.6 GB). Original cart on PICO-8 exits f113, no diagonal dash. UCT does not finish it at the PICO-8 alignment (a pixel off on the platforms, below); one frame later it does: 87 inputs = 86f. `tas/room_1_2_nodiag_frame_113.txt`, `tas/tasdatabase/nodiag/upload/TAS18.tas`. Before (`r0sxhnp` alone, count-up): no concrete win by 111; 112 grew 1.25x a layer |
| (2,1) 1100m | 82 | 87 (TAS11, offset 24) | **85 - 2 FASTER** (UCT: 61f against 62f) | 2026-10-08, `r0sxhnp,r0sxhn` (level -1 refused with `p`): r0sxhnp bound 82 (no concrete win at 82, 14k steps), r0sxhn (platform exact, filtered) bound 85, witness at 85 (2.2k steps); 697 s, 13.7 GB. Original cart (seeds 0, 0.8905): exits f85. Our first press is on frame 24, the frame the player is created (the community file's earliest exiting offset is 24 zeros): the upload starts there, 62 inputs = 61f in UCT, `tas/tasdatabase/nodiag/upload/TAS11.tas`, `tas/room_2_1_nodiag_frame_85.txt` |

## 700m and 1800m finished: the reach ladder down to exact objects (2026-10-08, branch `nodiag-finish`)

Both rooms were open because the objects' widening was refuted only by
the concrete count-up, which grows per frame of slack. What closed them is
the objects ladder with the reach filter (`3e324e5`) run down to EXACT
objects, `--level <level 0>,...,r0sxh,r0sx`, on one kept level-0 tree,
with the horizon counted from the bound rather than set at the reference:
each finer forward is filtered by the coarser level's REACHED nodes and
is tiny, and at `r0sxh` (only the held buttons widened) the arc bound is
the answer, so a horizon below it is REFUTED by the arcs themselves and a
horizon at or above it finds the witness at the bound in a few hundred
steps.

- **1800m (1,2): OPTIMAL 113, TAS18 118.** Level 0 `r0sxhnp` (forward to
  f118 169 s, 5.7 GB tree), bound 107. `--to 112`: level 1 `r0sxhn`
  (platforms exact) forward 136k visited in 8.5 s, bound 109, no win at
  109 by the try (41.7k steps); level 2 `r0sxh` (floors exact too) 22k
  visited, every state gone by f91: REFUTED (192 s). Level 1 alone would
  not have done it: its concrete breadth-first search at h112 had the SAME
  layer counts as level 0's (layers 61-63: 13,878, 14,320, 13,216 states
  at both) - the exact platform cut nothing the arcs had not; the exact
  floors did. `--to 118`: bounds 107, 109, 113 (level 1 4.6M visited,
  level 2 2.35M), the try at 113 wins in 176 steps; 359 s, peak 11.3 GB.
- **700m (6,0): OPTIMAL 73, TAS7 74.** The `r0sxhfp` split-frame forward
  to step 148 (h74) under a 55 GB cap: 2526 s, 884M visited, door 21.8 GB,
  peak 28.4 GB, in one run (the earlier forward was stopped at step 137
  and its resume ran out of memory rebuilding the door under 30 GB). Bound f68 (the try: no win
  at 68, 6.5k steps). `--to 69`: level 1 `r0sxh` (fruit and platforms
  exact) 4.7k visited, every state gone by step 108 (f54): REFUTED
  (126 s). `--to 74`: level 1 2.57M visited, 856k marked nodes, bound f73,
  the try at 73 wins in 134 steps (248 s; the level-0 arc phase's VmHWM
  36.8 GB with the mapped runs, 15.9 GB anonymous). Level -1 was not
  needed.
- **The concrete search runs one input per class** (`25a4879`): a frame
  that reads only some buttons has the same successors for every input
  agreeing on them (`RefEngine::frame_reads`; the freeze and spawn frames
  read none, up/down matter only where a dash starts). Room (1,2) h111 at
  level 0: 7.37M -> 1.45M concrete steps, 688 -> 236 s, every layer equal.
- **UCT is not PICO-8 on the platforms.** UniversalClassicTas computes in
  Lua doubles (not 16.16); in room (1,2) its player is a pixel off PICO-8's
  from frame 55 while riding a platform, for TAS18 as for ours (UCT
  capture against the original-cart replay, frame by frame). Our 113
  does not finish in UCT at the PICO-8 alignment (the file's first input
  on the player's first frame); one frame later (the witness's own leading
  zero kept) it does: 87 inputs = 86f against TAS18's 91 = 90f, where
  PICO-8 counts 85f. `category_runner` now tries that cut last. 700m's
  file finishes at the PICO-8 alignment (50 inputs = 49f).

## The platform rooms and the split frame (2026-10-08, branch `nodiag-platforms`)

700m (6,0), 1100m (2,1), 1800m (1,2) have moving platforms; 1500m (6,1) was
solved in any% only with the split frame. What blocked them, and what changed:

- **Not a second player split.** The capture's "two player moves in a
  frame" never happens: `move` is the only `__split_by_flr` of the player and
  `_update` calls it once per object; the platform carries the player with
  `move_x` (whole pixels, `rem` untouched). The one real case: a carry
  blocked by a wall sets `rem.x = 0` before the player's move, a split of one
  point, now `Const(fin)` from the whole circle (`arc_edges`, `9ed9124`).
- **The `p` level itself had not built with `n` since `b47b118`** (room (2,1)
  `r0sxhnp`: 354 trace states at the spawn; bisected). Countdown atoms stay
  three-valued under `p`, and with `p` an overlapped floor stores its
  computed state (`d5c6234`; plans/abstractions.md "p").
- **The split frame in the search** (`0231de8`): the tree in steps, the
  concrete search in whole frames. The objects ladder under it filtered
  mid-frame rows with a projection that does not key as the coarser tree's
  mid-frame rows: room (6,1) `r0sxhn,r0sxh` REFUTED 93, which TAS15 reaches in
  the minimal cart on PICO-8; filtering at frame boundaries only fixed it
  (`5ccf712`).
- 1500m `r0sxhn` alone (split, level -1 186 steps): the forward to step 186
  took ~70 min (25M states a step at most, peak 27.5 GB with the arc phase),
  arc bound f90 (ref 93), no concrete win at 90 (18.6k steps); the
  breadth-first search then grew to 27k states a layer by layer 61 at ~5 min
  a layer: stopped for the objects ladder. `r0sxhn,r0sxh` (after the filter
  fix): level 0 reproduced every kept and visited count of the first run
  (the `p` changes leave `n` alone untouched); level 1 (exact objects,
  filtered) kept 11-18M states a step near the horizon and its arc phase
  ran out of memory reading the edges (25 GB anonymous, 30.9 GB peak, the
  30 GB cap). Counting up at level 0 with a kept tree instead (`--to 91`,
  then 92: the breadth-first search's region is several times smaller per
  frame of slack) found 92 (the table above).
- 700m: the level -1 table did not finish in 17 min (`r0sxhfp`, split);
  without it the forward was stopped near step 140 at 30 GB (it reached
  step 148 in one run at 28.4 GB peak under a 55 GB cap: "700m and 1800m
  finished" above).
- `rewrite arc-check` runs out of memory on any tree with an unknown number
  (the near level's countdowns since `b47b118`, the fly fruit): it steps
  stored rows through the concrete engine, which reads `UNum` as the whole
  16.16 range (room (7,0) `r0sxhn`: OOM at f2 under 12 GB). Not run on these
  rooms' trees; the witnesses on PICO-8 are the check.
  Measured 2026-10-08 (room (7,0) `r0sxhn`, f2, under 12 GB): it is not
  one wide `flr` but the path count - `RefEngine::step` reads a widened
  field as its range and forks every straddling comparison on it
  independently, past 8,192 paths per stored row at ~20 two-way forks
  deep, each path a full state. A fix needs a different probe (e.g.
  concrete members of the row), not a bigger cap.

## The three rooms that failed on size (2026-10-08, branch `nodiag-big`; all three solved since)

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
(`CELESTE_KERNEL_EXPLAIN`, followed down: the error reads `UnknownBool(0)`,
the frame's first atom - the fruit's `fly` - next to `Cell(20) <= 0`, so it
is three-valued and reads as error); fixed below - that reading was wrong.

The (2,0) lesson (search at the answer, not at the reference) does not
rescue (3,0): nodiag's optimum is at least any%'s 89, and `r0sxhf` at
`--to 89` (L-1 89,5) cut nothing by f59: 53.9M kept, x1.5 a frame, 37 GB;
stopped.

**(3,0) solved (2026-10-08, branch `nodiag-30`): OPTIMAL 93, a tie with
TAS4.** Two fixes made `r0sxhfn` run (`plans/abstractions.md`, "f", "With
`n`"): the countdown atoms of `76c5a82`, and the coverage gap at f42, which
was `may_answers` giving `a == b` no ends whenever the frame held ANY
unknown - a hidden floor's exact `state == 2` split both ways, and its
solid side owed `collideable` false with the player inside it (`CELESTE_
KERNEL_EXPLAIN`: the declined lanes' error was the floor overlap with
`collideable` folded to true, not the fruit's `fly` the atoms in the guard
suggested). Equality reads its ends now unless an operand reads an
unknown. That made the kernels honest and big: 10.1M -> 25.8M fused nodes,
the three (6,5) kernels (the floors' square) 1.3M -> 7.8M nodes / 81 MB of
code each, and once the states gathered there (f60) they took 83% of the
time: f61 608 s, f62 1141 s for ~5M lanes (225 us a lane). `CELESTE_REGION=8,6`
(979 kernels, the largest 450k nodes; the same states, so the tree resumed
under it): f63 96 s; the widest frame f74-f76 (8.6M kept, 5-11 min a frame
on a machine shared with two other searches); 4 px is no faster than 8 at
f50 (12.6 s against 12.3 s, 8 threads). The forward to f93 with L-1 at 93:
kept f50 1.33M, f60 4.41M, f70 7.75M, f75 8.54M (the widest), f80 5.75M,
f85 3.40M, f90 351k; 206.9M visited. Its arc phase: 17.1M marked nodes,
391M edges, bound **76** (the fruit "maybe collected" is a free dash
refill; no concrete win at 76, 12.8k steps); reach marked 383,604 nodes for
level 1. `r0sxhn` filtered by them: 102.5 s, first remainder-free win f90,
arc bound **93**, and the try at the bound won in 93 steps - the community
route itself (the witness equals `--prefer`, TAS4's inputs behind the 27
spawn frames). Verified as the runner does (`/var/tmp/nd30/verify.py`):
exits during f93 in the ORIGINAL cart, dashes at f28 (right), f35 (none
held), f70 and f88 (up), no diagonal; UCT finishes both files in 66 inputs.
`tas/tasdatabase/nodiag/upload/TAS4.tas` is byte-identical to the
database's TAS4.tas: a proof that it is optimal, nothing to submit. Search
1:35 h wall after the region change (5720 s, level 0 resumed at f61; the
region-16 part to f61 1:04 h), VmHWM 35 GB (the kernel build).

The any% recipe's `b` (`r0sxhfb, r1sxhb..`: the floors fully unknown) has
no separate arc-era equivalent: `b` was deleted on 2026-10-04 because `n`
is finer and smaller (room (7,0): 5-6x fewer states from step 100), and
`r0sxhfn` IS the any% level 0 with `n` for `b`. Its looseness (bound 76
against 93) is the fruit's, which the ladder's `r0sxhn` removes.

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
`tas/room_2_0_nodiag_frame_100.txt`; the web UI's run `room20nodiag`
(rerun with `--save-marks`, 1713 s, the same witness; the UI's download
equals the upload file). (3,2) has no UI run: `--save-marks` keeps a row
per level-1 BFS mark (379M) on top of its 75 GB arc phase.

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
`tas/tasdatabase/nodiag/upload/TAS{6,17,29}.tas` (and, verified the same way, TAS{22,26,28}.tas on 2026-10-06, TAS{20,7,18}.tas on 2026-10-08, and TAS4.tas, the tie, identical to the database file). To submit: one Discord
message `!uploadtas classic nodiag` with the three files attached.
