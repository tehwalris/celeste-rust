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
