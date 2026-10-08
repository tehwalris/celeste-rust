# The gemskip categories (2026-10-08, branch `gemskip-campaign`)

Gemskip: no orb. `CELESTE_GEMSKIP=1` (`game_runner::gemskip`): the orb room
(5,2) is won by its bare exit and every later room starts with ONE dash
(`max_djump = 1`); the replay takes `--one-dash`, UniversalClassicTas
`UCT_DASHES=1`. Three categories: gemskip any% (`gemskipany`), gemskip No
Diagonal Dashes (`gemskipnodiag`, with `CELESTE_NODIAG=1`) and gemskip 100%
(`gemskip100`, with `CELESTE_HUNDRED=1`: the room's berry taken). Frame
counts below are the database's (inputs - 1); "ref" and "ours" are our exit
frames (prologue included).

Every result is run and verified by `tools/category_runner.py` (jobs and
results in `/var/tmp/gs-campaign`): the community file replayed in the
ORIGINAL cart behind the room's prologue, `rewrite search`, our witness in
the original cart (jump presses canonicalized against the seeded minimal
cart where the original's jump buffer differs), no diagonal dash for
nodiag, the berry for 100%, both files through UniversalClassicTas, whose
clean save of ours is the upload file (`upload/<cat>/TAS<n>.tas`).

## Results

DB = the database's frames (inputs - 1); ref = the community file's exit
frame in the original cart (prologue included); ours = our exit frame; UCT
= inputs of the clean save (database file -> ours). Every upload was
re-checked with `tools/uct/check_uploads.py` after the validator fix
(`0a67b9e`: a death in UCT is a failure, not a "finished" from a later
attempt; `15e7834`: balloon seeds nudged where UCT's doubles and PICO-8's
16.16 phases differ).

| category | room | DB | ref | ours | UCT | how | status |
|---|---|---|---|---|---|---|---|
| any% | 2900m (4,3) | 60 | 90 | 90 | 61 -> 61 | `r0sxhn` bound 90, witness at the bound; 330 s, 12.4 GB | OPTIMAL, tie |
| any% | 2600m (1,3) | 123 | - | 151 | 124 -> dies | see "2600m any%" below | PICO-8 optimum; no file valid in both |
| nodiag | 3000m (5,3) | 88 | 120 | **106** | 89 -> 75 | `r0sxhn` bound 106, witness at the bound; 922 s, 9.6 GB; upload seeds nudged to `[0.9999,0,0.9999,0.9999]` | OPTIMAL, -14 |
| nodiag | 2900m (4,3) | 93 | 123 | **109** | 94 -> 80 | bounds 106 (`r0sxhn`), 109 (`r0sxh`); 4663 s, VmHWM 56.8 GB (anon 33.5) | OPTIMAL, -14 |
| nodiag | 2400m (7,2) | 90 | 114 | **99** | 91 -> 76 | `r0sxhn` bound 99, witness at the bound; 783 s | OPTIMAL, -15 (= gemskip any%'s 99) |
| nodiag | 2600m (1,3) | 154 | 180 | **169** | 155 -> 144 | `r0sxhn` bound 169, witness at the bound; 648 s | OPTIMAL, -11 |
| nodiag | 2700m (2,3) | 151 | 175 | **147** | 152 -> 124 | bounds 132, 147; 1500 s | OPTIMAL, -28 |
| nodiag | 2800m (3,3) | 181 | 231 | **191** | 182 -> 142 | count-up on the kept level-0 tree (below); `--to 192`: bounds 186, 191, witness in 309 steps, 450 s | OPTIMAL, -40 |

"OPTIMAL": the search's last level's arc bound equals the witness's frame
(a lower bound met by a concrete run). Every nodiag witness presses no
diagonal dash; every one exits at its frame in the original cart under the
upload's seeds.

### 2600m any%: the community file is a UniversalClassicTas route

TAS26 (gemskip any%, 124 inputs) finishes in UCT but not on PICO-8: in the
original cart, at every prologue offset 20-33, the player dies at f130
around (39,40) and respawns. So there is no PICO-8 reference. `--to 149`
(the UCT count behind the 25-frame prologue): REFUTED at level 0
(`r0sxhn`; 690 s). `--to 169` (the nodiag optimum, an upper bound): arc
bound 151, the try at the bound wins in 216 steps: **151 is the PICO-8
optimum** (1489 s, VmHWM 50.6 GB). But UCT kills our file at keypress 64:
the two agree to f78, and at f79 (x 37) PICO-8's player stays at y 61 while
UCT's falls a pixel (Lua doubles against 16.16, the same class as 1800m
nodiag's platform pixel, plans/nodiag.md). Neither file is valid in both.

## How the rooms were run

- The objects in the open rooms (`/var/tmp/gs-campaign/objs.py`, from
  `cart/map-data.txt`): (5,2) only the big chest; (6,2) only the fly fruit;
  (7,2) none; (0,3) 3 springs, 2 fall floors, key, chest; (1,3) key and
  chest; (2,3) 2 balloons; (3,3) key, chest, a balloon, 3 fall floors; (4,3)
  the berry, 3 balloons, a spring; (5,3) key, chest, 3 balloons, a fall floor.
  So in (5,2), (6,2) and (7,2) the states are the player's: what grows is
  its speed (one dash: long rooms, many `appr` speeds).
- `CELESTE_TRIM_ROWS=1` everywhere, `--memory 55G`, levels
  `r0sxhn,r0sxh` (the reach-filtered objects ladder) unless said.
- **Count up from the level-0 bound, on the kept level-0 tree.** 2800m
  nodiag: at `--ceiling 231` (the reference) level 0 took 489 s and gave the
  bound 186, but the filtered level 1 (`r0sxh`, exact objects) at h231 grew
  to 20.2M states a frame by f134 (212 s a frame) and was stopped; `--to
  192` on the same level-0 tree: level 1 marked 447k nodes, bound 191, the
  witness in 309 steps, 450 s. The reach marks and level -1 both shrink
  with the horizon. `category_runner`'s `reuse` now hands the level-0 tree
  back after a job for exactly this.
- A tree filtered by level -1 at H serves horizons up to H only; the search
  now refuses a reused or resumed tree past the H it was filtered at
  (`frame::check_level_minus_one`, `<dir>/level_minus_one.txt`).

## Room (6,2): the fly fruit at `f` doubles every state

`r0sxhf` forward to f50 (gemskip any, no level -1): two shapes of exactly
1,919,474 rows each at f50 - the same player states with the fruit and
"maybe gone" (with `y` unknown the fruit may have flown off the top on any
frame). The player's columns: 893 `spd.x`, 77 `spd.y` values, 14,344 speed
pairs; the hottest cells still growing (48,114: 5.8k new states at f50,
1.15k at f39). With the fruit exact (`r0sxh`) it is far worse: f47 9.37M
kept against 2.05M (no cross-frame dedup: the fruit is a frame counter).
