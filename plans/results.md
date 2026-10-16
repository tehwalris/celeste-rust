# Results: every room's optimum

Frames count from the room's load (frame 1), spawn prologue included: the
optimum is the frame DURING which the room changes (at the summit: the
player first touches the flag). The reference is the community TAS
(`CelesteClassic/tasdatabase`), replayed in the ORIGINAL cart on a real
PICO-8 with its first input on the first frame the player exists ("offset"
= the prologue length), then fitted to celeste-minimal by `rewrite
trajectory` (`tas/room_X_Y_reference_frame_N.txt`). The database counts
`frames = exit frame - prologue - 1` (UCT's clean save: inputs - 1); "f"
marks a count in its terms.

Every witness here exits at its frame on a real PICO-8 (`pico8_diff/
replay.py`), balloon rooms under the seeds named. Every result before
2026-10-08 was searched from an IL LOAD (the room loaded alone); real play
enters through a transition ("Real play" below).

## any% (2026-09-13 .. 2026-10-04)

**Every optimum TIES its community TAS.** Room (0,0)'s earlier "proven" 94
(`51a4d99`) was a frame too long. Room (1,0)'s 99 equals the 2022
searcher's 76-update route (its "100" counted the prologue as 24:
`tas/room_1_0_exit_frame_100.txt` is the same route a frame later). Room
(6,0) was first reported 2 under TAS7: an offset error in the replay.

"Proven how" is the precision ladder of the time (deleted 2026-10-05): with
`--ceiling C` it confirms C at the exact level and REFUTES C-1 at the named
level (0-based index into the ladder used); a single horizon's lower bound
is the first level whose first win equals H. (3,3) is the first arc result.

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

### The arc pipeline reproduces them (2026-10-05)

`rewrite search` with no rem rungs, `--ceiling` the known optimum, the
same level -1 filters; release, on a shared machine.

| room | level(s), level -1 | arc bound | concrete search | found | wall | peak |
|---|---|---|---|---|---|---|
| (1,0) | `r0sx`, - | 99 | DFS at the bound, 383 steps | 99 | 5:33 | 32.7 GB |
| (4,2) | `r0sxhn`, (71,5) | 71 | DFS, 191 steps | 71 | 0:50 | 3.2 GB |
| (5,3) | `r0sxhn`, (79,5) | 78 | DFS at 78: none (7.2k steps); BFS 165k steps | 79 | 5:44 | 5.0 GB |
| (3,3) | `r0sxhn`, (172,5) | 172 | DFS, 608 steps | 172 | 10:29 | 40.1 GB |
| (7,0) | `r0sxhn`, (84,5) | 80 | DFS at 80: none (172k); at 81 1.29M steps; stopped | - | | 13.4 GB |
| (7,0) | `r0sxhn,r0sxh`, (84,5) | 80, 84 | level 1 (filtered, 6.9 s): DFS at 84, 292 steps | 84 | 14:40 | 11.5 GB |

The cost is the concrete search's region, the concrete states the level
lets into W: a few hundred steps where the bound is the optimum, ~6.5x a
frame of slack in (7,0), where the objects ladder takes over.

## No Diagonal Dashes (2026-10-04 .. 10-08; every improvement submitted)

`--nodiag`: the cart's diagonal dash arm raises, so a diagonal dash start
is no move, exactly, in the kernels and the reference alike. References:
the community nodiag TAS at the room's any% offset. Levels `r0sxhn,r0sxh`
with level -1 at the reference unless said; "bounds" are the arc bounds per
level; witnesses `tas/room_X_Y_nodiag_frame_N.txt`, uploads
`tas/tasdatabase/nodiag/upload/`.

| room | any% | ref | ours | how |
|---|---|---|---|---|
| (2,0) 300m | 95 | 108 (TAS3) | **100, -8** | `r0sxh --to 100` alone (`n` was 2.4x the states): bound 100; 47 min, 36 GB |
| (3,0) 400m | 89 | 93 (TAS4) | 93, tie | `r0sxhfn,r0sxhn,r0sxh --to 93`, `--region 8,6`: bounds 76, 93; the witness is TAS4's route |
| (5,0) 600m | 77 | 94 (TAS6) | **93, -1** | no level -1: bounds 91, 93 |
| (6,0) 700m | 70 | 74 (TAS7) | **73, -1** | split, `r0sxhfp,r0sxh,r0sx`, no level -1: bounds 68, 73; `--to 69` refuted |
| (7,0) 800m | 84 | 99 (TAS8) | 99, tie | bound 99 at both levels |
| (0,1) 900m | 100 | 108 (TAS9) | 108, tie | bounds 106, 108 |
| (2,1) 1100m | 82 | 87 (TAS11) | **85, -2** | `r0sxhnp,r0sxhn`, no level -1: bounds 82, 85 |
| (5,1) 1400m | 104 | 121 (TAS14) | **119, -2** | level -1 (121,5): bounds 100, 119; level 0 1.02G visited, 62.5M kept at its widest; 60 GB |
| (6,1) 1500m | 89 | 93 (TAS15) | **92, -1** | split, `r0sxhn`, level -1 (186,5) steps: bound 90; `--to 91` no concrete win (exhaustive); `--to 92` wins |
| (0,2) 1700m | 81 | 89 (TAS17) | **87, -2** | bound 87 at level 0 |
| (1,2) 1800m | 103 | 118 (TAS18) | **113, -5** | `r0sxhnp,r0sxhn,r0sxh,r0sx`, no level -1: bounds 107, 109, 113; `--to 112` refuted. Upload 85f (real play) |
| (3,2) 2000m | 128 | 152 (TAS20) | **148, -4** | bounds 133, 148 (the reach filter); exits under TAS20's seeds 0,0,0,0 only; peak 75 GB |
| (4,2) 2100m | 71 | 71 (TAS21) | 71, tie | the any% recipe (validation) |
| (5,2) 2200m | 150 | 158 (TAS22) | **157, -1** | `r0sxhn` alone: bound 157 |
| (1,3) 2600m | 127 | 135 (TAS26) | **133, -2** | `r0sxhn` alone: bound 133 |
| (3,3) 2800m | 172 | 184 (TAS28) | **179, -5** | `r0sxhn` alone: bound 179 |
| (4,3) 2900m | 85 | 111 (TAS29) | **106, -5** | bounds 104, 106; seeds 0, 0.5 and rnd |
| (5,3) 3000m | 79 | 96 (TAS30) | 96, tie | bounds 92, 96 |
| (1,1) 1000m | 94 | 94 (TAS10) | - | not run |

Our improvements differ from the community runs in the route, not the
counting: both replayed in the original cart in one harness, the player
first exists on the same frame. Every upload went through UCT's clean save
(`tools/uct/validate.sh`). UCT computes in doubles: in (1,2) its player is
a pixel off PICO-8's while riding a platform, so the 113 finishes in UCT
only a frame later (86f), and that 86 dies in real play; the upload is
the PICO-8 alignment, 85f, valid in IL, Celia, the chain and the boot
chain (re-searched from the real-play starts J 3 and J 4: 113 both).

## 100% (2026-10-08 .. 10-09)

`--hundred`: the exit counts only with the room's berry taken (an unknown
`got_fruit` at a coarse level counts as taken; the concrete search
decides). Two exact cuts made rooms fit: a row whose berry is lost is not
expanded (`frame::berry_lost`), and level -1 drops exits that leave the
berry behind. Runs `--ceiling REF`, level -1 at (REF, 5),
`--concrete-balloon-seeds` the file's seeds. The search takes every chest
draw; the runner tries the file's chest seed, then each class of draws,
and the upload carries the one that works. Witnesses
`tas/room_X_Y_hundred_frame_N.txt`; uploads `tas/tasdatabase/100/upload/`.

| room | DB | ref | ours (DB) | levels: bounds | status |
|---|---|---|---|---|---|
| (0,0) 100m | 90 | 118 | **115 (87f), -3** | `r0sxhn,r0sxh`: 115 | optimal; VALID in real play |
| (2,0) 300m | 103 | 129 | - | `r0sxh` | OPEN: level 0 past 55 GB before level -1 cuts |
| (3,0) 400m | 61 | 89 | 89 (61f), tie | real play J 3: `r0sxhfn,r0sxhn,r0sxh`, `--to 88` refuted | optimal (the any% witness takes the fruit) |
| (4,0) 500m | 123 | 153 | **152 (122f), -1** | `r0sxhn,r0sxh`: 152 | optimal; VALID (chest seed -1) |
| (6,0) 700m | 75 | 99 | - | split `r0sxhfp,r0sxh,r0sx` | OPEN: the arc phase past 55 GB (`f` cannot see that an early dash sends the fruit off) |
| (0,1) 900m | 94 | 122 | 122 (94f), tie | 114, 122 | |
| (3,1) 1200m | 52 | 78 | 78 (52f), tie | `r0sxhf,r0sxh`: 76, 78 | |
| (4,1) 1300m | 127 | 153 | 153 (127f), tie | 153 | |
| (5,1) 1400m | 103 | 129 | - | `r0sxhn,r0sxh` | OPEN: level 0 63.8M kept at f94, +11% a frame |
| (6,1) 1500m | 88 | 114 | 114 (88f), tie | split: f112, f113 | |
| (0,2) 1700m | 64 | 90 | 90 (64f), tie | 90 | |
| (2,2) 1900m | 107 | 131 | 131 (107f), tie | 131 | |
| (6,2) 2300m | 68 | 94 | **93 (67f), -1** | real play J 2: `r0sxhf,r0sxh`: 89, 93 | optimal from real play; VALID everywhere |
| (0,3) 2500m | 108 | 134 | 134 (108f), tie | 134 | |
| (1,3) 2600m | 137 | 163 | **161 (135f), -2** | 161; real play J 6: 161 | optimal; VALID (chest seed 0.5) |
| (3,3) 2800m | 137 | 187 | 187 (137f), tie | 187 | |
| (4,3) 2900m | 79 | 109 | 109 (79f), tie | 106, 107 | |
| (5,3) 3000m | 82 | 114 | 114 (82f), tie | 102, 114 | |

The submitted 2300m 63f held only from an IL load: it dies in Celia, the
chain and the boot chain (real play enters at J 2: the fly fruit one bob
ahead). Withdrawn (`tas/invalid`); 67f is the real-play optimum.

## Gemskip (2026-10-08 .. 10-16)

`--gemskip`: the orb room (5,2) is won by its bare exit and every later
room starts with ONE dash (the replay's `--one-dash`, UCT's `UCT_DASHES=1`).
gemskip nodiag adds `--nodiag`, gemskip 100% `--hundred`. Levels
`r0sxhn,r0sxh` unless said; "OPTIMAL" = the last level's arc bound equals
the witness's frame. Witnesses `tas/room_X_Y_gemskip*_frame_N.txt`,
uploads `tas/tasdatabase/gemskip*/upload/`.

| category | room | DB | ref | ours | how |
|---|---|---|---|---|---|
| any% | 2900m (4,3) | 60 | 90 | 90, tie | `r0sxhn` bound 90 |
| any% | 2600m (1,3) | 123 | - | 151 | the community run dies on PICO-8 at every offset (UCT only); `--to 149` refuted, bound 151. PARKED, no upload |
| any% | Summit (6,3) | - | 64 | **57, -7** | `r0sx --to 64` (no objects, no level -1): bound 57 |
| nodiag | 2400m (7,2) | 90 | 114 | **99, -15** | `r0sxhn` bound 99 |
| nodiag | 2600m (1,3) | 154 | 180 | **169, -11** | `r0sxhn` bound 169 |
| nodiag | 2700m (2,3) | 151 | 175 | **147, -28** | bounds 132, 147 |
| nodiag | 2800m (3,3) | 181 | 231 | **191, -40** | `--to 192` on the kept level-0 tree: bounds 186, 191 |
| nodiag | 2900m (4,3) | 93 | 123 | **109, -14** | bounds 106, 109 |
| nodiag | 3000m (5,3) | 88 | 120 | **106, -14** | `r0sxhn` bound 106; upload seeds nudged for UCT |
| 100% | 2500m (0,3) | 117 | 142 | **141 (116f), -1** | real play J 1, level -1 (150,5): bound 141. Fixed point only, like the database's own file (`gates/witnesses/gemskip100_TAS25.tas`) |
| 100% | 2800m (3,3) | 164 | - | **213 (163f), -1** | the database's file never exits on PICO-8; real play J 3, `--to` 195 and 210 refuted, bound 213 |
| 100% | 2900m (4,3) | 86 | - | **115 (85f), -1** | real play J 5 (after our 2800m), optimal there |

Gemskip any% 2600m: UCT kills our 151 at keypress 64 (its doubles let the
player fall a pixel at f79, the 1800m nodiag kind). Policy (Philippe,
2026-10-08): our result is the PICO-8 optimum; where UCT disagrees, look
for a same-length variant valid in both, never a longer run; a room whose
community run is valid only in UCT is parked. Gemskip 100% 2800m: the
first optimal witness died in UCT and Celia (a remainder of 0x0.7fff in
16.16 is exactly 0.5 in doubles); a second one is VALID everywhere, and
`pico8_diff/chain.py` plays it (`OVERRIDES`) so the boot chain runs
unbroken from 100m to 3000m.

## Real play: the loading jank (2026-10-08)

Real play enters a room through a transition: the new room's objects from
the leaving player's index J on get one update on the loading frame
(plans/architecture.md "The room's start"). UniversalClassicTas loads a
room alone and misses it; Celia models it.

- The jank model at the chain's J gives the chain's entry state in every
  any% room 200m-3000m (the only difference: `got_fruit` bookkeeping at
  500m; cosmetic fields aside), platforms, balloons, springs, floors, fly
  fruits, keys and chests included. The IL load differs from real play in
  gameplay fields in 14 rooms.
- J = 1 + the objects before the player when it leaves: the room's objects
  less the spawn and every destroyed one, plus the title or the spawn's
  smoke if the exit came soon enough. 100% runs collect berries, which
  lowers J. Only J moves objects across the categories' boot chains; no
  held button carries over; `freeze` at entry may be 2 (it freezes every
  object alike).
- The search takes the start as a set of J classes (`rewrite starts`); the
  runner runs the boot chain's J (its witness carries the upload) and one
  search per other class.
- **Celia's J is not always the boot chain's**: it counts the previous
  room's objects as if nothing were destroyed (any% differs at levels 5, 8,
  13, 18; 100% after most berries). A submittable witness must exit in
  both. Decision (Philippe, 2026-10-08): Celia should be fixed upstream;
  parked, both must pass until then.
- Re-verification (IL / UCT / Celia / boot chain): every tie VALID; 100%
  2300m 63f and nodiag 1800m 86f INVALID (both replaced, above); every
  other nodiag and gemskip upload VALID. The database's own gemskip any%
  2600m dies on PICO-8 and finishes in UCT and Celia (doubles); the boot
  chain restarts after it.

## Caveats on "optimal"

- **`rnd`** is an interval at every level: a confirmation is "some draw
  wins", a refutation holds for every draw; every balloon witness was
  replayed per seed (the tables' seed lists).
- **The silent interval wrap**: the kernels wrapped an overflowing interval
  `+`/`-`/negation until `549ecf5`/`b47b118` (2026-10-03). Every any% result
  but (3,3)'s and (5,0)'s rerun ran on the wrapping binary. Only intervals
  reaching the 16.16 limits were affected: the countdowns at `n` (and `t`,
  which no result uses). Rooms (1,1) and (6,1) re-run after the fix
  reproduce their counts at every frame; the other `n` rooms were not
  re-run.
- The arc result (3,3) over-approximates the objects and held buttons
  (`r0sxhn`); the remainder is exact and the witness concrete.

## How to run a room

```bash
./safe-run.sh -- ./target/release/rewrite search --room X,Y --level r0sxhn,r0sxh \
    --level-minus-one C,5 --ceiling C --checkpoint-dir DIR --prefer tas/FILE.txt \
    [--hundred | --nodiag | --gemskip] [--loading-jank J] [--concrete-balloon-seeds S]
```

- The level: `r0sx` without objects; `r0sxhn` with springs, fall floors,
  balloons, key and chest; `f` for a fly fruit, `p` for platforms
  (`r0sxhnp`; level -1 refuses `p`). `--split-frame` where the kernels are
  too big ((6,0), (6,1)): `--ceiling` stays in frames, level -1's H is in
  steps (2 x C).
- **The ladder** where the level-0 bound is well below the reference; down
  to `r0sxh` (and `r0sx`) the arc bound is the answer, so a lower horizon
  is refuted by the arcs and a higher one finds the witness at the bound.
  Count up from the bound on the kept level-0 tree.
- `--ceiling C` is a known solution: a refutation or no witness is an
  error. Without one, `--to H` with H generous.
- `tools/category_runner.py` runs a category end to end: the real-play
  start classes, the search with `--prefer` the community file, the
  original-cart replay, UCT and Celia, one upload file.
- Balloon rooms: replay the witness with the file's seeds and with 0 / 0.5
  / PICO-8's rnd (`pico8_diff/replay.py --balloon-seeds a,b`).
- When a level grows, look at the states before reasoning about counts
  (`census --cells`, `census --cell`, `census --erase`, `spurious`,
  `check ref --row`, `check known`): that chain found the floor-collision
  join bug and room (1,3)'s keying bug.
- Finding the reference: scan prologue offsets (23-37), replay in the
  original cart (`replay.py --lua ~/src/github.com/tehwalris/celeste_ocaml/
  celeste.lua --begin-game`), then `rewrite trajectory` (the minimal cart
  has no jump buffer, so raw inputs often do not transfer).
