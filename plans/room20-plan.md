# Room (2,0) "300 m": springs, and a fruit that is alive from room load

Third room of the campaign, after (0,0) = 94 and (1,0) = 100. Two firsts:
the first SPRINGS the search has ever run, and the first fruit that exists
from `load_room` rather than appearing mid-room.

## Recon (decoded from cart/map-data.txt, verified against the lua)

`load_room` scans tx outer, ty inner, so the objects array at load is

    [fruit(8,48), player_spawn(8,104), spring(40,112), spring(104,112)]

and after the spawn animation (`del` then `add`)

    [fruit, spring, spring, player]

Collecting the fruit gives `[spring, spring, player]`; nothing else changes
the shape. Springs are never destroyed - they hide (`hide_in`/`hide_for`,
`spr` 18/19, `delay`) and come back.

* The spawn prologue is **25 frames**: the player object is created at the
  end of frame 25, so the first player update is frame 26. (Room (0,0) is
  28, room (1,0) is 24; the spawn falls from y=128 to the spawn tile, and
  this one is at y=104.)
* Spikes: tile 17 at tx 7..10 of ty 6 and ty 11. Deaths are possible.
* The room is left through the gap at the TOP, tx 8..11 (x 64..95); every
  other top tile is solid. The win is `room.x == 3`, as everywhere else.
* No `key`, no `fall_floor`, no `platform`, no `fake_wall`, no `balloon` -
  so no `sin(frames/30)` and no `rnd`.

## The witness: 95 frames (upper bound, verified concretely)

`tas/room_2_0_exit_frame_95.txt` = 25 spawn frames + the community TAS's
70 input bytes (`tasdatabase/classic/any/TAS3.tas`), with two jump presses
moved to their execution frames (68 -> 69, 91 -> 92) because celeste-minimal
drops the jump buffer. Derivation, and the instrument that made it
mechanical rather than guesswork:

* a scratch tree (`lua/` + `cart/` symlink) holding celeste-minimal WITH
  the original's `jbuffer` restored replays TAS3.tas unmodified, and
  `concrete_run` in that tree is the reference trajectory;
* the repaired sequence reproduces that trajectory frame for frame in
  position, speed and rem, under the unmodified minimal.

`next_room()` fires DURING FRAME 95 - established by a second scratch tree
in which `next_room` only sets `freeze`, so the frame it runs on is visible
in `concrete_run`'s output. Reading it off the raw replay is misleading:
`del` does not remove the player from `objects` while `_update`'s `foreach`
is iterating, so the OLD player keeps updating for two more frames and
re-enters `next_room` at 96 and 97 - and frame 97 loads room (5,0), whose
`balloon.init` calls the unimplemented `rnd`. That crash is two frames past
the exit and is harmless.

The route collects neither the fruit nor a spring.

## What springs and the load-time fruit actually needed

1. **`got_fruit[1 + level_index()] = true` with level_index() == 2.**
   `got_fruit` is `{}` (the original fills it with 30 falses in
   `title_screen()`, which minimal skips), so room (0,0) - level_index 0 -
   only ever wrote index 1, an append, and the interpreter's dense arrays
   were never asked for anything else. Room (2,0) writes index 3 of an
   empty table and died with "Index is not the next index in the array".
   Fixed in the INTERPRETER (`GetIndex ... create` materialises the skipped
   indices as nils), not in the lua: the recipes address instructions by
   `%n` per function, so adding four lines to `_init` breaks every recipe
   entry in `_init`/`__init` and with it two certified campaigns. Lua reads
   an absent key and a nil the same way, so this is exact for every read the
   cart performs; what it does not model is `#`/`add` on a hole-punched
   table, which no table in the cart is subject to.
2. **The fruit's `y` had to join the `off` widening.** See below - this one
   is a soundness bug in the ladder, not a missing feature.
3. **Springs: nothing.** Zero deopts through the whole forward pass with
   `rewrites-room00.jsonl` unchanged, and a hand-built concrete trajectory
   (run right off the spawn ledge, jump at +16, land on the spring at
   (40,112) on frame 40: `y` snaps to 108, `spd.y` to -3, djump refills)
   confirms the bounce is being executed, not skipped.

## The fruit-off widening was only half a widening

At every non-exact level `make_state_abstract_rem` widens each live fruit's
bob counter to `off := [0, 39]`. `off` is read in exactly one place -
`this.y = this.start + sin(this.off/40)*2.5` - and the coarse levels' own
interval arithmetic therefore puts the whole bob band in `y`, while the
EXACT level keeps one of the 40 concrete positions.

The band filter coarsens a level-k row by calling that same function. It
widened `off` and left `y` concrete, so a k16 row coarsened to k15 was a row
k15 never had, and every fruit-alive lane of the exact level was dropped as
an "unknown coarse row". On room (0,0) that was invisible: its fruit only
exists after the wall break, and the optimal path never breaks the wall, so
the dropped lanes were never needed. On room (2,0) a fruit is alive in every
lane from frame 1, and **k16 came out empty at frame 2 and refuted every
horizon**.

The fix widens `y` to `start +/- 2.5` (the range of `sin*2.5`) together with
`off`, with an assertion that the widening only ever grows the value it
replaces. Controls:

* before: horizons 35, 36, 37 all refuted, k16 empty from frame 2;
* after: horizon 34 refuted at k=2, horizon 35 converges through all 17
  levels, and `trace-witness` puts the witness in every level's band at
  every frame with `e + g = 35` throughout and `g = 0` at f035.

(Those numbers are from the `CELESTE_WIN_AT_XY=26,108` pipeline test - the
witness passes through whole-pixel (26,108) on frame 35 - which gives a
non-vacuous `g` in minutes instead of the full horizon.)

Room (0,0)'s and room (1,0)'s recorded results are unaffected: every
refutation there happened at a level k <= 7, where both k and k-1 widen the
bob identically, and the only k16 lanes the bug dropped were fruit-alive
ones that its winning path does not use. Their row hashes DO move (a fruit's
`y` is now the band from its creation frame rather than from the next one),
which matters only to checkpoints that the chunk-cap fingerprint already
invalidated.

## FIDELITY: `foreach` is not PICO-8's `all()` (needs a decision)

PICO-8's `all` is deletion-safe by construction:

    function all(c)
      if (c==nil or #c==0) return function() end
      local i,li=1
      return function()
        if (li==c[i]) i+=1     -- advance only if nothing shifted
        li=c[i]
        return c[i]
      end
    end

`lua/builtin_level_3.lua`'s is an index walk (`for i=1,#tbl do func(tbl[i])`),
so when an object destroys itself the array shifts under the index and the
NEXT object is skipped. Three consequences, all of them real in this cart:

1. **The player misses its creation-frame update** in a room where
   `player_spawn` is the LAST object - rooms (0,0) and (1,0). PICO-8 runs
   `player.update` on the frame the player is created; we run it first on
   the next frame. Room (2,0) is accidentally exempt: its spawn is at index
   2 of four, so the walk still reaches the player. Both room witnesses
   replay IDENTICALLY under a scratch build with PICO-8's `all` (room (0,0)
   still exits during f094, room (1,0) during f100) - the creation frame's
   input is 0 in both and the player is standing still, so the extra update
   is a no-op there. It is not a no-op in general: a search that is allowed
   to act on the creation frame has one more frame of input than ours does,
   so rooms (0,0) and (1,0) may each be one frame optimistic. The community
   TASes, which run on real PICO-8, use exactly the input-frame counts we
   proved optimal (66 and 76), which is evidence that the extra frame does
   not help - not a proof.
2. **The object after a self-destroying one skips an update.** In room
   (2,0) that is spring (40,112), skipped on the spawn frame (harmless) and
   on any frame where the fruit is collected (NOT harmless: a bounce that
   PICO-8 would deliver on that exact frame is lost).
3. **`load_room` does not destroy everything.** Its
   `foreach(objects, destroy_object)` deletes objects 1, 3, 5, ... so a
   death in room (2,0) reloads the room with a stale spring still in the
   array. This is not theoretical: the frontier's shape census at f055 and
   f060 shows lanes with the five-object shape
   `[spring, fruit, player_spawn, spring, spring]`, which no `load_room`
   can otherwise produce.

Rooms (0,0) and (1,0) never hit (2) or (3): their object arrays are down to
one element by the time `load_room` runs.

**Not fixed here, deliberately.** The fix is four lines in
`builtin_level_3.lua`, but it moves `foreach_1`'s instruction numbering and
13 recipe entries address it (3 `promote_cell`, 10 `inline`), it changes
every row hash in every room, and it can move rooms (0,0) and (1,0)'s
absolute optima by a frame. That is a campaign-level decision, and the
conservative move mid-campaign is to leave the model alone and report it.
When it is taken, the room (2,0) numbers here are the ones least affected:
its witness and its exit frame are identical under both semantics.

## The wall: this room does not fit, and the reason is SPEED, not geometry

Level-0 forward pass, campaign settings (8000/8000, 16 threads):

| frame | frontier lanes | visited rows | RSS | s/frame |
|---|---|---|---|---|
| 45 | 1,503,507 | 5.2M | 1.7 GB | 4.4 |
| 50 | 5,507,770 | 24.1M | 4.7 GB | 16.9 |
| 55 | 11,783,364 | 68.7M | 10.5 GB | 39.0 |
| 60 | 23,220,148 | 178M | 21.7 GB | 113.5 |
| 65 | 36,562,604 | - | 38.3 GB | 244.3 |
| 66 | 40,723,085 | - | 39.9 GB | 222.2 |

The per-frame ratio is still 1.09 at f065 and decaying slowly; the horizon
is 95. Straight-line extrapolation puts the 100 GB cap somewhere around
f075 and the f095 frontier near 10^8 lanes with 2-3 x 10^9 visited rows -
which is 4x this machine's RAM for the forward pass alone, before the sweep
(room (0,0)'s sweep needed 2x its forward pass).

The `CELESTE_XY_DUMP` census says exactly where it goes:

| frame | occupied (x, y) | lanes | lanes per position |
|---|---|---|---|
| 40 | 2,331 | 267,616 | 115 |
| 45 | 3,483 | 1,503,502 | 432 |
| 50 | 4,309 | 5,507,767 | 1,278 |
| 52 | 4,503 | 7,495,508 | 1,665 |

**The geometry has saturated** - the room only has about 4,500 reachable
whole-pixel positions and the frontier reached 96% of them by f050 - and
every remaining doubling is state AT a position. At level 0 `rem` is already
fully widened, so what is left is the velocity and the dash/jump machinery:
`spd.x`, `spd.y` (full 16.16 fixed point), `dash_time`, `dash_target`,
`dash_accel`, `dash_effect_time`, `djump`, `grace`, `p_jump`, `p_dash`.

That is a statement about the LADDER, not about this room's implementation:
the refinement ladder refines exactly one thing, `player.rem`, and its
coarsest rung is still exact in every other coordinate. Rooms (0,0) and
(1,0) are small and walled, so their reachable (position, velocity) product
stays inside 100 GB; a wide-open room does not. The natural next rung is a
level BELOW 0 that buckets `spd.x`/`spd.y` the way `rem` is bucketed, with
level 0 banded by it - the same refutable over-approximation the rem ladder
already is, and the same machinery (`--band-dir`, `--band-prev-bits`)
generalised from one abstracted field to two. Specialization (variants),
chunking and the recipe cannot touch this: they change the cost per lane,
and the problem is the number of lanes.

## Measurements that say what NOT to do

* **Pinning `has_dashed`** (gameplay-dead in any room without a fly_fruit)
  merges the dashed and never-dashed copy of every state. Measured on room
  (2,0): 5,507,770 -> 5,507,769 lanes at f050. One lane. Everything in the
  frontier has dashed by then.
* **Dropping the fruit-`off` widening** looked attractive here, because
  unlike room (0,0) - where fruits are born at 16 different frames - room
  (2,0)'s fruit is born at load, so `off` is the same in every lane and the
  widening buys no merging, while the interval `y` it forces makes every
  collision test near the fruit an `UnknownBool` split. Measured, with the
  widening off: **40,454,335 lanes at f052 against 7,495,512** - 5.4x worse.
  The widening stays.

## The position graph is recorded IN the forward pass here

`ladder.sh FUSE=1` passes `--record-pos-graph`, and the `l0-posgraph` stage
then finds a table that already covers the horizon and reuses it. Gate, on
this room:

* the visited row SETS at f035 and f040 are identical with and without it
  (`tools/rowdiff.py`, 0 differing rows both ways);
* the recorded table is a strict SUPERSET of the replay's - one extra pair,
  the spawn's own first move, which the replay cannot see because it starts
  from the frame-1 batch. Too large is safe (slower); too small is not;
* the sweep's `g` at `CELESTE_WIN_AT_XY=26,108`, horizon 40, is IDENTICAL as
  a function of the row (`tools/gjoin.py`, 886,234 rows, 0 differing).

`--record-pos-graph` under `--resume` used to write a table stamped with the
full horizon while holding only the resumed tail. It now seeds the observer
from the table on disk, and refuses to run if the table is missing or short
of the resume point.

`ladder.sh` also skips its `l0-bench` stage when the level-0 tree already
covers the horizon, so a campaign can build level 0 once, in one process,
past every horizon it will need - which is what makes the fused recording
usable at all (`bench --resume` refuses a checkpoint beyond `--frames`).
