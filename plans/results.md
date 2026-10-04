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
| (5,0) | 600 m | 77 | TAS6, 29 + 48 = 77 (exits only under some draws) | `r0sxhb..r15sxhb, r15sxh, rxsx`; h76 refuted at level 5 | none: the reference engine refused `rnd` then; NOT replayed | 09-21 | `4e19168` | the balloon's `offset` canonical [0,1) restored cross-frame dedup (f60 10.6M -> 1.1M); 77 is a best case over draws |
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

## Caveats on what "optimal" means here

- **Balloon rooms**: `rnd` is an interval at every level, so a confirmation is
  "some draw wins" and a refutation holds for every draw. Each witness was
  replayed per seed; several exit only for some (the table's seed lists).
  Room (5,0)'s 77 has no witness at all.
- **The silent interval wrap** (plans/architecture.md, "Interval
  overflow"): the ASM kernels wrapped an overflowing interval `+`/`-`/negation
  until `549ecf5` (2026-10-03 15:15) and `b47b118` (2026-10-03 18:07; now a
  lane error, countdowns the unknown number). EVERY result in the table except
  room (3,3)'s arc result (2026-10-04, after both) was computed with the
  wrapping binary. What the wrap could affect: only an interval whose
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

## How to run an object room

The ladder to start a room with objects (springs, fall floors, balloons, key
and chest), established on rooms (7,0) and (0,1):

```
CELESTE_LADDER="r0sxhn,r1sxhn,r2sxhn,r3sxhn,r4sxhn,r4sxh,r5sxh,...,r15sxh,rxsx" \
CELESTE_LEVEL_MINUS_ONE="C,5" \
  ./safe-run.sh -- ./target/release/rewrite search --room X,Y --ceiling C
```

- Every object abstract at level 0 (`n`), and kept abstract a few rem rungs
  (the switch to exact objects then happens on narrow marks); balloons stop
  paying past ~rem 3. Fly fruit rooms: `f` instead of `n` for rungs 0-4.
  Platform rooms: `p` at level 0 at least, longer if the first exact-platform
  level's marks explode. No object at all: `r0sxh..r15sxh,rxsx`.
- The level -1 filter (plans/level-minus-one.md) with H = the ceiling, where
  its table builds.
- Big kernel sets: `CELESTE_KERNEL_SETS=1` (or 2) caps the prebuilt sets;
  `CELESTE_SPLIT_FRAME=1` for floor-heavy or platform rooms whose kernels
  pass ~100k bodies (horizons and ceilings are then in steps, 2 per frame).
- **When a level grows, look at the states before reasoning about counts**:
  `rewrite cell-growth --by-age`, `col-census --cell x,y` (what varies at one
  position), `coarse-census --erase FIELD` (the merge ceiling of a widening),
  `cell-saturation`, `spurious --real T --coarse C` (a coarse level's states a
  finer tree never reaches, traced to the first spurious step), `ancestry`,
  `rerun-row` (one stored row through a level's kernels), `ref-check`
  (kernels against the reference engine, row by row), `follow` (a known
  concrete solution stepped against a level's tree). That chain found the
  floor-collision join bug (99.8% of room (7,0)'s fully-unknown states at one
  cell were impossible) and room (1,3)'s keying bug.
- A ceiling REFUTED at a coarse level ("a known concrete solution the model
  cannot reproduce") is a bug, never a result.
- **Balloon rooms**: replay the witness with the TAS's seeds
  (`pico8_diff/replay.py --balloon-seeds a,b`, the tasdatabase header) and
  with 0 / 0.5 / PICO-8's own rnd.
- Finding the reference: scan prologue offsets (the first frame the player
  exists is the convention; it ranged 23-37), replay in the original cart
  (`replay.py --lua ~/src/github.com/tehwalris/celeste_ocaml/celeste.lua
  --begin-game`), then `rewrite trajectory` to fit it to celeste-minimal (the
  minimal cart has no jump buffer, so raw inputs often do not transfer).

## Runtime and memory worth keeping

| room | level 0 to the ceiling | whole search | peak |
|---|---|---|---|
| (0,0) | f79 first win, 22.8M states at f93 | 2:32 h (kernel walk) -> 23:50 (graph) -> 15:31 (regions, reverted) | 31-45 GB |
| (1,0) | 6.8M at f99 | 1:39.7 held ladder (`--ceiling 99`); 2:56 one default ladder at 99, 4:51 count-up | 5.8 GB held; 25 GB default |
| (2,0) | 24.0M at f80 with level -1 (62.8M without) | 27:28 incl. the 472 s table | 28.0 GB |
| (3,0) | frontier peak 22.7M at f74 | 34 min (level 0 resumed) | 12.8 GB, 210 GB of checkpoints |
| (4,0) | 7.98M at f76 | 3:54 | 8 GB |
| (5,0) | 2.5M at f70 (balloon canonical; 49.5M before) | ~10 min | |
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
