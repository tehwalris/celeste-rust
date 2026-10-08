# Results: every room's optimum (2026-09-13 .. 2026-10-04)

Frames count from the room's load (frame 1), spawn prologue included: the
optimum is the frame DURING which the room changes (or, at the summit, the
player first touches the flag). The reference is the community TAS from
`CelesteClassic/tasdatabase` (`classic/any/TAS<level+1>.tas`), replayed in the
ORIGINAL cart on a real PICO-8 with its input 0 on the first frame the player
exists ("offset" = the prologue length), then fitted to celeste-minimal by
`rewrite trajectory` (`tas/room_X_Y_reference_frame_N.txt`).

**Every optimum TIES its community TAS.** One corrected this project's own
earlier claim by a frame: room (0,0)'s 94 "proven optimal" by the
pre-rebuild ladder (`51a4d99`, 2026-08-07, `tas/room_0_0_exit_frame_94.txt`).
Room (1,0)'s 99 is NOT an improvement on the 2022 searcher: its 0-based
control frame 75 is 76 player updates, the same as TAS2's 76 inputs and as
our 99 (prologue 23 + 76). `tas/room_1_0_exit_frame_100.txt` is that route
with an idle player update on frame 24 (it assumed the player first updates
on frame 25; under `all()` it updates on frame 24, the frame it is created).
Replayed on PICO-8, its frames 25-100 are the 99's frames 24-99 shifted by
one, the same integer position on every frame. A tie. Room (6,0) was first
reported as 2 frames under TAS7; that was an offset error in the replay
(`d1ae910`): it ties.

"Proven how": with `--ceiling C` the ladder confirms C through the exact
level and REFUTES C-1 at the named level (no win by C-1 at a level that
over-approximates the game). Where a single horizon ran, the lower bound is
the first level whose first win equals H. Level numbers are 0-based indices
into the ladder used. Witnesses are DFS walks through the exact level's marks
with the reference engine (`rewrite witness`), replayed on a real PICO-8
(`pico8_diff/replay.py`).

| room | name | optimum | reference | proven how | witness (`tas/`) | date | commit | key insight |
|---|---|---|---|---|---|---|---|---|
| (0,0) | 100 m | 93 | TAS1, offset 27 -> 93 | default ladder, count-up from level 0's first win f79; h92 refuted at level 7 | `room_0_0_reference_frame_93.txt` (TAS1 fitted; no search witness extracted) | 09-13 | `43a9f1f` | the old "94 proven" was wrong; 2:32 h on the kernel walk, 23:50 on the explicit graph |
| (1,0) | 200 m | 99 | TAS2, 23 + 76 = 99 | default ladder; h98 refuted at level 6 | `room_1_0_exit_frame_99.txt` | 09-13 | `d4eaca2` | ties TAS2 and the 2022 searcher (its control frame 75 = 76 updates = 23 + 76); its "100" counted the prologue as 24 |
| (2,0) | 300 m | 95 | TAS3, 25 + 70 = 95 | `r0sxh..r15sxh,rxsx`, level -1 (95,5); h94 refuted at level 9 | `room_2_0_exit_frame_95.txt` (TAS3 with two jbuffer presses moved) | 09-17 | `e906986` | held buttons unknown (3.8x); level -1 replaced the unsound 8 px band |
| (3,0) | 400 m | 89 | TAS4, 27 + 62 = 89 | `r0sxhfb, r1sxhb..r15sxhb, r15sxh, rxsx`; h88 refuted at level 6 | `room_3_0_exit_frame_89.txt` | 09-19 | `8533314` | the mark filter's deadline; the fly fruit is taken (looks necessary); TAS4's raw inputs do not transfer to the minimal cart, its route does |
| (4,0) | 500 m | 76 | TAS5, 29 + 47 = 76 | default ladder; h75 refuted at level 9 | `room_4_0_optimal_frame_76.txt` | 09-16 | `e0d5852` | the key's `spr`/`flip` pinned with the timers; 3:54 end to end |
| (5,0) | 600 m | 77 | TAS6, offset 29, its seed | rerun 10-04: `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx` (level -1 refused), `--ceiling 77`; h76 refuted at level 6 | `room_5_0_exit_frame_77.txt` (TAS6's seed 0.636; seeds 0.6-0.9) | 10-04 | `d3e58d5` | the balloon's `offset` canonical [0,1) restored cross-frame dedup (f60 10.6M -> 1.1M, `4e19168`); the second dash is the balloon's refill |
| (6,0) | 700 m | 70 | TAS7, 23 + 47 = 70 | split frame, `r0sxhfp, r0sxhf, r0sxh, r1sxh..r15sxh, rxsx`; frames 64-69 refuted (h72's level 7 first win at frame 70) | `room_6_0_exit_frame_70.txt` | 09-30 | `7811208` | platforms make every frame's states new: `p` + POINTS + the split frame |
| (7,0) | 800 m | 84 | TAS8, offset 23, seed 0 | rechecked 10-02: `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (84,5); h83 refuted at level 10 | `room_7_0_exit_frame_84.txt` (seeds 0, 0.25) | 10-01 | `c76bf10` | the floor-collision join bug `7f6b96e`; the `n` level |
| (0,1) | 900 m | 100 | TAS9, offset 27 | `r0sxhn, r1sxhn, r1sxh..r15sxh, rxsx` at h100; first win 100 from level 10 | `room_0_1_exit_frame_100.txt` | 10-01 | `b97fa9a` | objects abstract one rem rung longer: level 0 40M -> 14.8M states, the first exact level 26.5M -> 88k |
| (1,1) | 1000 m | 94 | TAS10, offset 25 | `r0sxhn..r8sxhn, r8sxh..r15sxh, rxsx` at h94; first win 94 from level 11 | `room_1_1_exit_frame_94.txt` | 10-01 | `267b136` | floors abstract through rem 8; stale edge runs of a killed level (fixed: a fresh forward clears its dirs) |
| (2,1) | 1100 m | 82 | TAS11, offset 24 | `r0sxhnp, r0sxhn, r1sxhn..r8sxhn, r8sxh..r15sxh, rxsx` at h82; first win 82 from level 8 | `room_2_1_exit_frame_82.txt` | 10-01 | `029b1c0` | an exact platform is a frame counter; the balloon's bob `y` doubled every state |
| (3,1) | 1200 m | 71 | TAS12, offset 25 | `r0sxhf..r5sxhf, r5sxh..r15sxh, rxsx` at h71; first win 71 from level 4 | `room_3_1_exit_frame_71.txt` | 10-01 | `4b2a0f4` | ice modelled (`4f1ed71`: only tile flag 0 was) |
| (4,1) | 1300 m | 95 | TAS13, offset 25, its seeds | `r0sxhn..r8sxhn, r8sxh..r15sxh, rxsx` at h95; first win 95 from level 9 | `room_4_1_exit_frame_95.txt` (TAS13's seeds, rnd) | 10-01 | `f8ea434` | abstract balloons held a spurious 91 through rem 8 (marks 2.9M -> 121M); the first exact level refuted it at once |
| (5,1) | 1400 m | 104 | TAS14, offset 25 | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx` at h104, level -1 (104,5) from f88; first win 104 from level 5 | `room_5_1_exit_frame_104.txt` | 10-01 | `39f35db` | the spring's speeds (2,903 `spd.x` values) handled by level -1, not buckets |
| (6,1) | 1500 m | 89 | TAS15, offset 25 | split frame, `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (178,5) steps; h176 steps refuted at level 5 | `room_6_1_exit_frame_89.txt` | 10-02 | `7c09449` | the split frame: kernels 288k -> 23k bodies, f2-f45 955 s -> 152 s, identical states |
| (7,1) | 1600 m | 86 | TAS16, offset 27 | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx` at h86; first win 86 from level 8 | `room_7_1_exit_frame_86.txt` | 10-01 | `2cd72d5` | the slack past the exact-object level was rem precision, refuted a frame per rung |
| (0,2) | 1700 m | 81 | TAS17, offset 25 | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (81,5); h80 refuted at level 11 | `room_0_2_exit_frame_81.txt` | 10-01 | `2a97a47` | the map-row nibble swap (`5746919`: rows 2-3 were garbage); `tile_flag_over` read only tile row 0 for an unknown `y` (`52a368a`, a spurious 80) |
| (1,2) | 1800 m | 103 | TAS18, offset 27, its seed | `r0sxhnp, r0sxhn, r1sxhn..r4sxhn, r4sxh..r15sxh, rxsx`; h102 refuted at level 8 | `room_1_2_exit_frame_103.txt` (seeds 0.9397, 0, rnd) | 10-02 | `f5661ef` | the platform exact at level 1 marked 253M states: keep `p` longer |
| (2,2) | 1900 m | 74 | TAS19, offset 23 | `r0sxh..r15sxh, rxsx`, level -1 (74,5); h73 refuted at level 5 | `room_2_2_exit_frame_74.txt` | 10-02 | `9a449c8` | no object to abstract |
| (3,2) | 2000 m | 128 | TAS20, offset 31, its seeds | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (128,5); h127 refuted at level 6 | `room_3_2_exit_frame_128.txt` (TAS20's seeds, rnd) | 10-02 | `b250941` | abstract-balloon marks grew with rem (31M -> 171M) until the first exact-balloon level (6.6M) |
| (4,2) | 2100 m | 71 | TAS21, offset 37 | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (71,5); h70 refuted at level 7 | `room_4_2_exit_frame_71.txt` (seeds 0, 0.5, rnd) | 10-02 | `540871f` | the default offset scan (20-32) found nothing: the player first exists on frame 38 |
| (5,2) | 2200 m (orb) | 150 | TAS22, offset 25 (takes the orb) | `--ceiling 150`; h149 refuted at level 4 | `room_5_2_exit_frame_150.txt` | 10-02 | `06f926c` | the win needs the orb (`2dc116d`; the bare exit is at 68); a closed chest past its deadline is not expanded |
| (6,2) | 2300 m | 75 | TAS23, offset 25 | `r0sxhf..r4sxhf, r4sxh..r15sxh, rxsx`; h74 refuted at level 11 | `room_6_2_exit_frame_75.txt` | 10-02 | `8f914bf` | rooms past the orb start with two dashes (`26a0e54`: TAS23 never exited otherwise); two rooms never merge (`3c2996a`) |
| (7,2) | 2400 m | 94 | TAS24, offset 23 | `r0sxh..r15sxh, rxsx`, level -1 (94,5); h93 refuted at level 9 | `room_7_2_exit_frame_94.txt` | 10-02 | `c28dcb4` | no objects |
| (0,3) | 2500 m | 98 | TAS25, offset 25 | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (98,5); h97 refuted at level 13 | `room_0_3_exit_frame_98.txt` | 10-02 | `3a6a81f` | a 97 survived the exact-object levels to rem 11 (marks to 23M): sub-pixel slack, not the objects |
| (1,3) | 2600 m | 127 | TAS26, offset 25 | `r0sxh..r15sxh, rxsx`, level -1 (127,5); h126 refuted at level 8 | `room_1_3_exit_frame_127.txt` | 10-02 | `c04678c` | the model REFUTED the known 127 at level 1: a number and its point interval keyed differently (`592c72f`) |
| (2,3) | 2700 m | 106 | TAS27, offset 23, its seeds | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx` (level -1 refused); h105 refuted at level 5 | `room_2_3_exit_frame_106.txt` (seeds [0,0.02485], 0, 0.5) | 10-02 | `4719f8f` | refuted at the first exact-balloon level |
| (3,3) | 2800 m | 172 | TAS28 -> 172 | the ROTATION GRAPH: `r0sxhn` level 0 with arc records, level -1 (171,5), `arc-search --marked-only`: no win by 171 (74 s); win at 172 | `room_3_3_arc_witness_frame_172.txt` (seeds 0, 0.1534) | 10-04 | `c0de454`, `4ef6321` | six rem ladders failed (marks 2-2.6x per rung, OOM): rem drift; exact remainders refute in 74 s |
| (4,3) | 2900 m | 85 | TAS29, offset 29 | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (85,5); h84 refuted at level 4 | `room_4_3_exit_frame_85.txt` (seeds 0, 0.5) | 10-02 | `cf4ca3e` | |
| (5,3) | 3000 m | 79 | TAS30, offset 31, its seeds | `r0sxhn..r4sxhn, r4sxh..r15sxh, rxsx`, level -1 (79,5); h78 refuted at level 5 | `room_5_3_exit_frame_79.txt` (TAS30's seeds, zeros; not 0.5) | 10-02 | `5d42e3e` | refuted at the first exact-objects level |
| (6,3) | summit | 55 | TAS31, 23 + 34 = 55 (touches the flag) | default ladder, `--ceiling 55`; h54 refuted at level 6 | `room_6_3_exit_frame_55.txt` | 10-02 | `0e67dfe` | no exit: the win is the flag's rect, x 55..67, y 41..52 (`c195f8c`, the original cart's `flag.draw`) |
| (7,3) | title | - | - | not a level (`level_index() == 31` is the title screen) | | | | |

## The arc pipeline (2026-10-05): the known optima again

`rewrite search` with no rem rungs (branch `arc-only`; plans/architecture.md
"The search"), on rooms whose optimum the ladder had proven. Release binary,
`--ceiling` the known optimum, the same level -1 filters; every witness
replayed on a real PICO-8 (`pico8_diff/replay.py`) exits on the optimum's
frame. Wall times are on a machine other searches shared (load 40-60 on 32
threads); peak is `VmHWM` (`/usr/bin/time`), the mapped edge runs and
checkpoints included.

| room | level(s), level -1 | known | arc bound | concrete search | found | PICO-8 | wall | peak | the ladder's |
|---|---|---|---|---|---|---|---|---|---|
| (1,0) | `r0sx`, - | 99 | 99 | DFS at the bound: 383 steps, 0.2 s | 99 | exits f99 (inputs = `room_1_0_exit_frame_99.txt`) | 5:33 (forward 4:32; 8.8M nodes, 214M edges, arc 62 s) | 32.7 GB (forward 10.1) | 2:56 one default ladder, 25 GB; arc-sets: 6:58 + 5:43, 18.2 GB |
| (4,2) | `r0sxhn`, (71,5) | 71 | 71 | DFS: 191 steps | 71 | exits f71, seeds 0 and 0.5 | 0:50 | 3.2 GB | 9 GB |
| (5,3) | `r0sxhn`, (79,5) | 79 | 78 | DFS at 78: none (7.2k steps, 4.7 s); BFS: 165k steps, 66 s | 79 | exits f79, seeds 0,0,0.1855,0 and zeros | 5:44 (the level -1 table 4:05) | 5.0 GB | 28.5 GB |
| (3,3) | `r0sxhn`, (172,5) | 172 | 172 | DFS: 608 steps, 0.9 s | 172 | exits f172, seeds 0,0.1534 (inputs = `room_3_3_arc_witness_frame_172.txt`) | 10:29 (forward 8:32; 16.1M nodes, 412M edges) | 40.1 GB* | six ladders out of memory |
| (7,0) | `r0sxhn`, (84,5) | 84 | 80 | DFS at 80: none, 172k steps, 4:18; at 81: 1.29M steps, 28 min; stopped | - | | | 13.4 GB | 5.2 GB |
| (7,0) | `r0sxhn,r0sxh`, (84,5) | 84 | 80, then 84 | level 0: DFS at 80 none (172k, 4:18); level 1 (filtered, 6.9 s): DFS at 84, 292 steps | 84 | exits f84, seeds 0 and 0.25 | 14:40 | 11.5 GB | |

\* (3,3) ran on the edge format before `9b967c3` (36 B per loaded edge,
against 8 B since): its peak is the old graph's.

What decides the cost is the concrete search's region: the concrete states
the level lets into W. Where the bound is the optimum it is a few hundred
steps; one frame of slack in (5,3) was 165k; four in (7,0) would have been
~300M (the region grew 6.5x a frame) - there the objects ladder takes over.
Room (7,0) with exact objects at level 0 alone (`r0sxh`, unfiltered) is no
alternative: 48.8M states kept at f55 (against 2.6M at `r0sxhn`), 300 s a
frame, stopped.

**Room (4,3) No Diagonal Dashes at h111** (`CELESTE_NODIAG=1`, level -1
(111,5); the nodiag TAS29 is 111): the `r0sxhn` forward visited 311M states
(10:14, peak 14 GB); its arc graph, 38.2M nodes and 769M edges, loaded in
3:18 (bfs 49 s, edges 127 s, graph 22 s; the old separate arc records were
OOM-killed at 60 GB on this tree), the backward 12 s, the bound 104; no
concrete win at 104 (18.7k steps). Alone, the breadth-first search then grew
~1.4x a layer (29k states at layer 46 of 111): OOM-killed at 40 GB twice
(the graph's edges alive, then interpreter states in the layers: `1b76615`,
`66d962a`) and stopped at 37.6 GB a third time (every successor's row kept
until the merge: `3135c9a`, not rerun here). The objects ladder
`r0sxhn,r0sxh` instead: the filtered `r0sxh`
forward 2:28, its bound 106, the witness at 106 in 392 steps - **106, five
frames under TAS29**, exiting on a real PICO-8 with seeds 0, 0.5 and
PICO-8's rnd, no diagonal dash (`tas/room_4_3_nodiag_frame_106.txt`,
plans/nodiag.md). 9:23 with the level-0 tree reused, peak 40.7 GB (VmHWM,
mapped files included).

**The ladder filtered by reach (2026-10-08).** The finer level is filtered
by the nodes the rotation graph REACHES inside W from the start's
remainder (`arc_dp::reach`), not every node with a winning set: in room
(3,2) nodiag 1.3-2.7% of them. That made **(3,2) nodiag 148, four frames
under TAS20's 152**: level 0 `r0sxhn` bound 133, level 1 `r0sxh` bound 148
and its witness in 446 steps, exiting f148 in the original cart under the
file's seeds (plans/nodiag.md). The level-1 arc phase peaked at 75 GB.
**(2,0) nodiag 100, eight under TAS3's 108**, by a single `r0sxh` level at
`--to 100` (47 min, 36 GB): the springs exact (`n`'s "maybe bounce" at
every spring is 2.4x the states by f65) and the horizon at the answer, not
at the reference - level -1's cut moves with it.
**(3,0) nodiag 93, a tie with TAS4**, by the three-level ladder
`r0sxhfn,r0sxhn,r0sxh` at `--to 93`: the fruit AND the floors abstract at
level 0 (it builds since `nodiag-30`: countdown atoms under the fruit, and
equality's may-answers per operand), bound 76; `r0sxhn` filtered by its
reach (383,604 nodes) bound 93 and the witness at 93 - the community route.
Its level 0 needs `CELESTE_REGION=8,6`: at 16 px the floors' square's three
kernels were 81 MB of code each and a frame of ~5M lanes took 19 min
(plans/nodiag.md).

**The platform rooms, by the reach ladder down to exact objects
(2026-10-08, branch `nodiag-finish`).** `--level <level 0>,...,r0sxh,r0sx`
on one kept level-0 tree: the filtered finer forwards are tiny, and at
`r0sxh` the arc bound is the answer, so a lower horizon is REFUTED by the
arcs and a higher one finds the witness at the bound. **(1,2) nodiag 113,
five under TAS18's 118**: `r0sxhnp` bound 107, `r0sxhn` 109, `r0sxh` 113;
`--to 112` REFUTED at `r0sxh`; the witness in 176 steps (359 s, 11.3 GB).
**(6,0) nodiag 73, one under TAS7's 74** (split frame): `r0sxhfp` to step
148 (42 min, 28.4 GB), bound f68; `r0sxh` bound f73, `--to 69` REFUTED;
the witness in 134 steps. Both exit at the optimum in the original cart on
PICO-8 and finish in UniversalClassicTas (1800m one frame later there:
plans/nodiag.md).

## 100% (2026-10-08, branch `hundred`; plans/hundred.md)

`CELESTE_HUNDRED=1`: the exit counts only with the room's berry taken. Of
the 18 rooms of the tasdatabase's 100% list, 15 are verified end to end
(original cart on PICO-8 with the berry, UniversalClassicTas with no
death): four IMPROVE on the community TAS - 100m 115 (-3), 500m 152 (-1),
2300m 89 (-5), 2600m 161 (-2) - and eleven tie (400m 89, 900m 122, 1200m
78, 1300m 153, 1500m 114, 1700m 90, 1900m 131, 2500m 134, 2800m 187, 2900m
109, 3000m 114). Open: 300m and 1400m (level 0 past 55 GB before level -1
cuts), 700m (the `f` level's arc phase past 55 GB). Two exact changes made
them fit: rows whose berry is lost are not expanded, and level -1 drops
exits that leave the berry behind. Witnesses: `tas/room_X_Y_hundred_frame_N.txt`.

## Real play: the loading jank (2026-10-08, branch `loading-jank`)

**Every result above was searched and verified from an IL LOAD** (the room
loaded alone, as UniversalClassicTas loads it). Real play does not enter a
room that way. The leaving player's `_update` calls `next_room()` from inside
`_update`'s `foreach(objects, ...)`. PICO-8's `all` resumes at the same index
in the NEW list, so the new room's objects from the leaving player's index J
on get one update (move, then update) on the loading frame. Then the frame
draws. Celia (gonengazit/Celia, the community's current tool) models this:
its IL load updates objects from the previous room's object count on.

Found on 100% 2300m (6,2). Our 63 (TAS23, submitted) exits in the IL load
and in UCT. It dies in Celia, in the chain from (5,2) and in the boot chain.
J = 2 there, so the fly fruit is one bob ahead (`step` 0.55, not 0.5). The
database's 68 exits in all of them. A community member doubted the 63 and
was right.

The tools:

- `pico8_diff/chain.py` plays rooms on a real PICO-8 through the real
  transitions, using the tasdatabase's files (the category's, else
  FALLBACK's).
  - `--boot` starts from 100m: this is the ground truth.
  - `--modes` diffs the room-entry state of the IL load and of the jank
    model against the chain's, field by field.
  - `--check-start` does the same for the search's own start state.
  - `--feasible` lists every J a play of the previous room can give.
  - `--vary` compares the entry state after every category's boot chain.
- `tools/celia/validate.sh` runs Celia headless.
- `tools/uct/check_uploads.py` runs IL, UCT, Celia, the chain and the boot
  chain, and gives a verdict.

Every database 100%/any% file from 100m to 2300m replays in the boot chain
at its database count. The jank model (`replay.py --jank J`, and the search's
`CELESTE_LOADING_JANK=J`) gives the chain's entry state exactly. Only these
fields differ: `seconds`, `music_timer`, `new_bg` and `frames` (timer, music,
background), and the key's sprite, which is drawn from `frames`.

**What the room entry depends on.** The jank index J depends on how the
previous room was played: J = 1 + the objects before the player when it
leaves. That is the previous room's objects less the spawn, less every
destroyed one (a berry taken, a fly fruit, a key, a chest, a fake wall),
plus the room title or the spawn's smoke if the exit came soon enough.
- 100% runs collect berries, which lowers J. Room (1,0) is entered with
  J 1 in 100% and J 2 in any%: the spawn itself is one frame ahead.
- Room (5,0)'s spawn and balloon move, in 100% and key.
- `--vary` over all levels, comparing 8 categories' boot chains: only J
  moves objects.
- Among the globals, only these vary: `max_djump` (the category's orb),
  `got_fruit` (the category's own record, not this room's) and the
  cosmetic ones.
- No held button carries over (a new player has `p_jump`/`p_dash` false).
- `freeze` at entry can be 2 after a dash on the exit frame. It freezes
  every object alike, so no count changes.
- `rnd` is seeded by the file's `[seeds]`, the stated caveat.

The search therefore takes the room's start as a set of J classes.
`category_runner` runs the main search at the boot chain's J (its witness is
valid after the database's previous rooms) and one search-only job per other
feasible start (`chain.start_classes`). The history-widened optimum is the
least of them.

Re-verification (2026-10-08). IL / UCT / Celia / boot chain, all at the
claimed count unless noted:
- 100%:
  - 100m 87, 500m 122 and 2600m 135 are VALID.
  - **2300m 63 is INVALID: it dies in Celia, the chain and the boot chain.**
  - All 11 ties are VALID.
- nodiag: **1800m 86 (TAS18, submitted) is INVALID**, dying in Celia, the
  chain and the boot chain. Room (1,2)'s platforms are janked. Every other
  nodiag upload is VALID.
- gemskipany and gemskipnodiag: every upload is VALID. The gemskipany boot
  chain breaks at the database's own 2600m, which dies on PICO-8 in every
  mode but finishes in UCT and Celia (they compute in doubles). Past it the
  boot restarts at 2700m.

## Caveats on what "optimal" means here

- **Balloon rooms**: `rnd` is an interval at every level, so a confirmation is
  "some draw wins" and a refutation holds for every draw. Each witness was
  replayed per seed; several exit only for some (the table's seed lists).
- **The silent interval wrap** (plans/architecture.md, "Interval
  overflow"): the ASM kernels wrapped an overflowing interval `+`/`-`/negation
  until `549ecf5` (2026-10-03 15:15) and `b47b118` (2026-10-03 18:07; now a
  lane error, countdowns the unknown number). EVERY result in the table except
  room (3,3)'s arc result and room (5,0)'s rerun (2026-10-04, after both) was
  computed with the wrapping binary. What the wrap could affect: only an interval whose
  endpoint reaches the 16.16 limits - in practice the countdowns widened to
  the whole range at the `t` (timers) and `n` (near) levels, which the cart
  decrements before `delay <= 0`. At `t` it was decisive: shaking floors never
  fell (found in room (3,3), where it refuted TAS28's own second half); no
  table result uses `t`. At `n` the kernels had leaned on it (dropping it
  declined room (1,1) at f35 until the near-floor owes were fixed in
  `549ecf5`); after both fixes, room (1,1) `r0sxhn` to f94 and room (6,1)
  split `r0sxhn` to step 90 reproduce the committed runs' kept and visited
  counts at every frame. The other `n` rooms ((7,0) recheck, (0,1), (2,1),
  (4,1), (5,1), (7,1), (0,2), (1,2), (3,2), (4,2), (0,3), (2,3), (4,3),
  (5,3)) were not re-run. Rooms whose ladders have no `n`/`t` level hold no
  full-range countdown interval. Interval `Mul`/`Div` by a constant is still
  unchecked (open).
- **The arcs result (3,3)** still over-approximates the objects and held
  buttons (level 0 `r0sxhn`); the remainder is exact. The 172 witness is
  concrete.
- The room (3,3) second-half experiment (from TAS28's state after 113 inputs)
  found 59 optimal from there, under `n`.

## How to run a room (the arc search, 2026-10-05)

```
CELESTE_LEVEL_MINUS_ONE="C,5" \
  ./safe-run.sh -- ./target/release/rewrite search --room X,Y --level r0sxhn \
  --ceiling C --checkpoint-dir DIR [--save-marks UIDIR]
```

- The level: `r0sx` for a room without objects, `r0sxhn` with springs, fall
  floors, balloons, key and chest (everything abstract but where the player
  overlaps a floor; held buttons unknown); `f` for the fly fruit. The
  concrete search refutes what the level invents. `p` for moving platforms
  (with `n`: `r0sxhnp`). `CELESTE_SPLIT_FRAME=1` where the unsplit kernels
  are too big (rooms (6,0), (6,1)): `--ceiling` stays in frames, level -1's
  horizon is in steps (`2 x C`).
- **When the level-0 bound is well below the reference** (room (7,0):
  `r0sxhn` 80 against 84) the concrete search's region grows several-fold a
  frame of slack: run the objects ladder, `--level r0sxhn,r0sxh` (the exact
  objects' forward filtered by `r0sxhn`'s arc-marked nodes; 6.9 s for (7,0)).
- The level -1 filter (plans/level-minus-one.md) with H = the ceiling (or
  `--to`), where its table builds; the table is cached on disk
  (`/var/tmp/celeste-l1-cache`).
- `--ceiling C` is a known solution (the replayed community TAS): a
  refutation, or no concrete witness by C, is an error, never a result.
  Without a reference, `--to H` with H generous: the forward is the cost, and
  the concrete search stops at the first concrete win.
- The witness lands in `DIR/witness_frame_F.txt`. **Balloon rooms**: replay it
  with the TAS's seeds (`pico8_diff/replay.py --balloon-seeds a,b`, the
  tasdatabase header) and with 0 / 0.5 / PICO-8's own rnd - the concrete search
  takes every `rnd` leaf, so a witness may exit for some draws only.
- **When a level grows, look at the states before reasoning about counts**:
  `rewrite cell-growth --by-age`, `col-census --cell x,y` (what varies at one
  position), `coarse-census --erase FIELD` (the merge ceiling of a widening),
  `spurious --real T --coarse C` (a coarse tree's states a finer tree never
  reaches, traced to the first spurious step; `--chain-out`), `rerun-row`
  (one stored row through a level's kernels), `ref-check` (kernels against
  the reference engine, row by row), `follow` (a known concrete solution
  stepped against a level's tree). That chain found the floor-collision join
  bug (99.8% of room (7,0)'s fully-unknown states at one cell were
  impossible) and room (1,3)'s keying bug.
- Finding the reference: scan prologue offsets (the first frame the player
  exists is the convention; it ranged 23-37), replay in the original cart
  (`replay.py --lua ~/src/github.com/tehwalris/celeste_ocaml/celeste.lua
  --begin-game`), then `rewrite trajectory` to fit it to celeste-minimal (the
  minimal cart has no jump buffer, so raw inputs often do not transfer).
- Before 2026-10-05 a room ran a precision LADDER (`CELESTE_LADDER="r0sxhn,
  r1sxhn,...,r4sxhn,r4sxh,...,r15sxh,rxsx"`): every result in the table
  above but (3,3)'s came from one; the recipes are in its "proven how" column.

## Runtime and memory worth keeping

| room | level 0 to the ceiling | whole search | peak |
|---|---|---|---|
| (0,0) | f79 first win, 22.8M states at f93 | 2:32 h (kernel walk) -> 23:50 (graph) -> 15:31 (regions, reverted) | 31-45 GB |
| (1,0) | 6.8M at f99 | 1:39.7 held ladder (`--ceiling 99`); 2:56 one default ladder at 99, 4:51 count-up | 5.8 GB held; 25 GB default |
| (2,0) | 24.0M at f80 with level -1 (62.8M without) | 27:28 incl. the 472 s table | 28.0 GB |
| (3,0) | frontier peak 22.7M at f74 | 34 min (level 0 resumed) | 12.8 GB, 210 GB of checkpoints |
| (4,0) | 7.98M at f76 | 3:54 | 8 GB |
| (5,0) | 1.94M/frame max, 35.2M visited (10-04 rerun) | 4:37 (`--ceiling 77`) | 10.9 GB |
| (6,0) | 1.68G visited to step 150 (split) | overnight | |
| (7,0) | 71M visited (recheck, unsplit, level -1) | | 5.2 GB |
| (0,1) | 14.8M states/frame max, 224M visited | minutes after level 0 | 14 GB |
| (1,1) | 3.37M/frame max, 110M visited | ~15 min (second ladder) | 19.4 GB |
| (2,1) | 3.86M/frame max, 121M visited | ~20 min | 25 GB |
| (3,1) | 54.8M at f70 (ice), 573M visited, ~45 min | | 37 GB |
| (4,1) | 7.68M/frame max, 264M visited | ~15 min | 37 GB |
| (5,1) | 441M visited | | 35 GB |
| (6,1) | 20.4M a step max, 743M visited | | 28 GB |
| (7,1) | 4.9M/frame max, 119M visited | | 16 GB |
| (0,2) | 87M visited with the filter | | 5.4 GB |
| (1,2) | 94M visited | | 43 GB (levels 1-3) |
| (2,2) | 21.9M visited | | 21 GB |
| (3,2) | 122M visited | | 33 GB |
| (4,2) | 6.6M visited | | 9 GB |
| (5,2) | 35M at f80 before the chest deadline; then minutes | | |
| (6,2) | 63M states at f75, 742M visited (no filter) | | 36 GB |
| (7,2) | 66M visited | | |
| (0,3) | 159M visited | | 27 GB |
| (3,3) | 13.8M marked nodes, 346M edges at h171 | arc search 74 s after level 0 | 29 GB |
| (4,3) | 91M visited | | |
| (5,3) | 13.0M visited | | 28.5 GB |
| (6,3) | 38M visited, 4.1M at f55 | ~8 min | 8.4 GB |

Memory is the state COUNT, not the bytes per state: ~100-150 B per frontier
row, 16-24 B per door entry. Machine: 7950X3D, 16 cores, 125 GB; long runs
used `safe-run.sh --memory 85G..95G` explicitly.
