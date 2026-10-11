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

BLOCKER (open): the kernels' lattice walk fails on room (0,0) at every
level: `objects[0].start: not a constant`. The "strawberry bob" widening
(`Flag::Always`, crates/celeste-engine/src/widening.rs) stores a fruit's `y`
as a band around its `start`, which must be a constant of the shape. With
THREE strawberries, taking one shifts the others' indices, so different
fruit subsets share a shape with different `start`s. The original map has
one strawberry a room, so nothing met this before.
