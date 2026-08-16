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

* The spawn prologue is **25 frames**: the player object is created during
  frame 26 and, in this room, updates on that same frame, so frame 26 is the
  first input byte that matters. (Rooms (0,0) and (1,0) hand the player its
  first update one frame after creation - see the `foreach` section.)
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
`load_room`'s own `foreach(objects, destroy_object)` deletes every OTHER
element (see the `foreach` section), so the player survives its own room
change, keeps updating, and re-enters `next_room` on frames 96 and 97 -
and frame 97 loads room (5,0), whose `balloon.init` calls the unimplemented
`rnd`. That crash is two frames past the exit and is harmless.

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
   the next frame. Room (2,0) is exempt: its spawn is at index 2 of four, so
   the walk still reaches the player after the shift, and its player updates
   on its creation frame exactly as PICO-8's does.
   The effect on those two rooms is a LABEL, not a lost frame: the Nth
   player update still consumes the Nth input byte, so the reachable set
   after N updates is identical - it just happens on frame N+24 for us and
   N+23 in PICO-8. Their proven optima are therefore 94 and 100 room frames
   in this model against 93 and 99 in PICO-8, with the INPUT-frame counts
   (66 and 76, which is what the TAS database records) identical either way.
   Both witnesses replay identically under a scratch build with PICO-8's
   `all`, exiting on the same frames. Room (2,0)'s 95 needs no such
   adjustment.
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
| 68 | 50,674,575 | 450M | 47.7 GB | 305.3 |
| 70 | 62,890,020 | 570M | 67.6 GB | 347.9 |

f001..f070 is 2,773 s of forward pass and 76.08 GB peak, with the position
graph (326,410 pairs over 7,896 destination cells) recorded in it. 570M
visited rows at f070 is already 1.4x room (0,0)'s ENTIRE 94-frame campaign.

The per-frame ratio is still 1.09 at f065 and decaying slowly; the horizon
is 95. Straight-line extrapolation puts the 100 GB cap somewhere around
f075 and the f095 frontier near 10^8 lanes with 2-3 x 10^9 visited rows -
which is 4x this machine's RAM for the forward pass alone, before the sweep
(room (0,0)'s sweep needed 2x its forward pass).

Memory is the first wall but not the only one. At f068 a frame costs 305 s
and the per-frame time is growing about as fast as the lane count; carrying
that to f095 is ~11 h for the level-0 forward pass ALONE, before the first
sweep. A campaign is that pass plus a sweep per horizon plus 16 banded
levels per horizon, over the ~15 horizons between the abstract bound and the
answer. Even with the memory, this is a machine-weeks job as it stands.

The `CELESTE_XY_DUMP` census says exactly where it goes:

| frame | occupied (x, y) | lanes | lanes per position |
|---|---|---|---|
| 40 | 2,331 | 267,616 | 115 |
| 45 | 3,483 | 1,503,502 | 432 |
| 50 | 4,309 | 5,507,767 | 1,278 |
| 52 | 4,503 | 7,495,508 | 1,665 |
| 70 | 7,590 | 62,890,009 | 8,286 |

The f070 row is from `rewrite field-census` (below), which reproduces this
dump's f050 and f052 position counts exactly, and it **corrects the
"saturated at ~4,500 positions" reading**: the frontier had not saturated by
f052, it kept spreading to 7,590 positions (7,389 of them inside the room's
128x128, the rest a room-transition x of -4). What the four early rows made
look like saturation was the first derivative flattening, not the set
closing.

The conclusion survives the correction, because the two terms move at very
different rates: from f050 to f070 the positions grow 1.76x and the lanes
per position grow 6.5x. **The multiplicity is state AT a position**, and it
is what all the growth is. At level 0 `rem` is already fully widened, so
what is left is the velocity and the dash/jump machinery: `spd.x`, `spd.y`
(full 16.16 fixed point), `dash_time`, `dash_target`, `dash_accel`,
`dash_effect_time`, `djump`, `grace`, `p_jump`, `p_dash`.

That is a statement about the LADDER, not about this room's implementation:
the refinement ladder refines exactly one thing, `player.rem`, and its
coarsest rung is still exact in every other coordinate. Rooms (0,0) and
(1,0) are small and walled, so their reachable (position, velocity) product
stays inside 100 GB; a wide-open room does not. The next rung is a level
BELOW 0 that buckets some other field the way `rem` is bucketed, with level
0 banded by it - the same refutable over-approximation the rem ladder
already is, and the same machinery (`--band-dir`, `--band-prev-bits`)
generalised from one abstracted field to several. Specialization
(variants), chunking and the recipe cannot touch this: they change the cost
per lane, and the problem is the number of lanes.

WHICH field is measured in the next section, and the ten-field list above
turns out to be badly unbalanced.

## Which field carries the multiplicity: measured, and it IS `spd` - at depth

`rewrite field-census` (`src/interpreter/field_census.rs`) reads one frame's
saved boundary states and asks, per lane-varying field: how many rows survive
if that field is collapsed? Erasing a field is the crudest possible
abstraction of it, so `rows_after / rows_before` is an UPPER BOUND on what
any rung abstracting it could merge. `field:n` instead buckets it to `2^-n`,
the same `div_euclid(width) * width` the `rem` ladder applies (asserted in
the module's tests), which is what a rung would really do. Offline from
`~/celeste-checkpoints/room20/frames/`, 12 s and 1.4 GB at f052 - no search
time, so it cannot contaminate a benchmark. f070, at 62.9M rows, costs
~25 min and 11 GB.

The frontier has 15-19 lane-varying fields per object-array shape (20 across
all shapes at f050, 23 at f070, where more shapes exist). `rem` is not one
of them - already fully widened - and neither is anything on the fruit or
the object array except the springs' `delay`/`spr`.

**Measure at DEPTH.** The frames are not interchangeable and the difference
is not small: the same collapse of `spd` leaves 16.9% of the rows at f050
and 3.5% at f070, because `spd.x` alone goes from 187 distinct values to
3,460. A census taken only at f050 understates the `spd` rung by 4x and
leads to the opposite decision. f070 (62,890,020 rows, the deepest saved
frame before the wall) is the one to read.

Single fields erased, as a fraction of the rows before. Every frame's
`rows_before` reproduces the published frontier lane count exactly:

| field | values at f070 | f045 | f050 | f052 | **f070** |
|---|---|---|---|---|---|
| `player.spd.x` | 3,460 | 39.0% | 32.0% | 27.4% | **13.4%** |
| `player.x` | 129 | 18.6% | 17.2% | 17.3% | **13.6%** |
| `player.y` | 116 | 15.9% | 17.0% | 17.6% | **16.0%** |
| `player.spd.y` | 165 | 65.5% | 57.8% | 55.5% | **45.2%** |
| `player.p_jump` | 2 | 50.7% | 50.6% | 50.5% | **50.4%** |
| `player.p_dash` | 2 | 59.0% | 54.6% | 53.8% | **52.1%** |
| `player.grace` | 7 | 98.9% | 96.7% | 96.7% | **97.5%** |
| `player.dash_target.y` | 3 | 100.0% | 99.5% | 99.4% | **98.0%** |
| `player.flip.x` | 2 | 92.4% | 94.5% | 96.1% | **98.9%** |
| `player.djump` | 2 | 99.7% | 99.7% | 99.5% | **99.8%** |
| `player.dash_effect_time` | 11 | 99.1% | 99.3% | 99.4% | **99.9%** |
| `player.dash_target.x` | 3 | 100.0% | 99.4% | 99.1% | **99.8%** |
| `player.dash_time` | 5 | 100.0% | 100.0% | 100.0% | **100.0%** |
| `player.dash_accel.x/.y` | 3 | 100.0% | 100.0% | 100.0% | **100.0%** |
| springs' `delay`/`spr`, `freeze`, `has_dashed`, `delay_restart` | | ~100% | ~100% | ~100% | **~100%** |

Three things fall out of that table:

* **`spd.x` overtakes position.** At f070 erasing it leaves fewer rows than
  erasing the player's whole `x` coordinate, and its distinct-value count
  passes `x`'s by 27x (3,460 against 129). Position spreads over a bounded
  room; velocity does not.
* **The dash state machine is worth nothing.** `dash_time`,
  `dash_target.*`, `dash_accel.*` and `dash_effect_time` are 98-100%
  individually and 88.0% together at f070 - 1.14x for six fields. Neither
  are `djump`, `grace`, `flip.x` or anything on the springs. Of the ten
  non-`spd` fields this plan used to list, EIGHT are worth 1.19x between
  them.
* **Two booleans are worth 3.8x.** `p_jump`/`p_dash` are the previous
  frame's jump and dash button, kept only for edge detection
  (`jump = btn(k_jump) and not this.p_jump`). Read them as a pairing rate:
  erasing a boolean cannot leave less than 50% of the rows, and `p_jump`
  leaves 50.4% at f070 - so **99.2% of the frontier's rows have their
  `p_jump`-flipped twin in the frontier too**, and 95.8% have their `p_dash`
  twin. The frontier carries two (mostly four) copies of nearly every game
  state, differing only in which buttons were held on the frame that
  produced it. Unlike `spd`, this one is FLAT with depth (30.0% at f045,
  26.3% at f070), so it is a constant ~3.8x rather than a growing one.

That `p_jump`/`p_dash` are where the input history survives is consistent
with the rest of the model rather than a surprise: `__button_states` does
not appear in this census at all, because `__reset_button_states()` makes it
a fresh unknown at the end of every frame chunk - which is exactly what lets
the boundary merge dedup lanes that took different inputs to the same state.
`p_jump`/`p_dash` are the player's own copy of the same information, they
are NOT reset, and so they are the one channel through which the input that
was pressed keeps distinguishing rows forever.

Sets collapsed together, which is what a rung does:

| collapsed | f045 | f050 | f052 | **f070** |
|---|---|---|---|---|
| `spd.x`+`spd.y` erased | 25.3% | 16.9% | 11.3% | **3.5%** |
| `spd.x`+`spd.y` bucketed to 1 px/frame (`:0`) | 37.9% | 27.2% | 20.8% | **6.9%** |
| ... to 1/2 px/frame (`:1`) | 50.4% | 39.4% | 33.9% | **12.9%** |
| ... to 1/4 px/frame (`:2`) | 69.8% | 61.1% | 57.5% | - |
| `p_jump`+`p_dash` erased | 30.0% | 27.6% | 27.2% | **26.3%** |
| `dash_time`+`dash_target.*`+`dash_accel.*`+`dash_effect_time` | 89.9% | 86.2% | 85.9% | **88.0%** |
| all ten non-`spd` fields of the old list | 26.6% | 22.4% | 21.8% | - |
| `spd:0` + `p_jump` + `p_dash` | 12.3% | 8.0% | 5.9% | **1.9%** |
| `spd` erased + `p_jump` + `p_dash` | 8.6% | 5.1% | 3.3% | **1.0%** |
| all twelve erased | 1.8% | 0.8% | 0.6% | **0.2%** |

### What that buys in frames

The frontier grows by a factor of 1.1228 per frame over f066..f073
(watermark deltas of the f073 checkpoint), so a merge factor F is
`ln F / ln 1.1228` frames of headroom, and the wall (~f075) is 20 frames
short of the horizon (95). At f070:

| rung | factor at f070 | frames | (same rung at f050) |
|---|---|---|---|
| `spd` bucketed at 1 px/frame - the buildable rung | **14.5x** | **23.1** | 3.7x, 11.2 |
| `spd` bucketed at 1/2 px/frame | 7.8x | 17.7 | 2.5x, 8.0 |
| `spd` erased (unbuildable upper bound) | 29.0x | 29.1 | 5.9x, 15.4 |
| `p_jump`+`p_dash` erased | 3.8x | 11.5 | 3.6x, 11.1 |
| `spd:0` + `p_jump` + `p_dash` | 52.1x | 34.2 | 12.6x, 21.9 |
| all twelve erased | 606x | 55.3 | 119x, 41.3 |

**The `spd.x`/`spd.y` rung this plan proposed is the right rung, and the
coarsest useful version of it - one whole pixel per frame, zero fraction
bits, the same rung shape `CELESTE_REM_BITS=0` already is for `rem` - clears
the 20-frame gap with 3 frames to spare.** Half-pixel buckets do not (17.7).
So the rung must be built at its COARSEST setting to be worth building, and
the margin at that setting is thin enough that `p_jump`/`p_dash` should be
taken with it: together they are 52x, or 34 frames, which is margin rather
than a coin flip.

Two honest caveats, in order of size:

1. These are counts of surviving BOUNDARY ROWS. A bucketed `spd` becomes an
   interval that widens everything downstream of it, and a widened `p_jump`
   makes `btn(k_jump) and not this.p_jump` an `UnknownBool` that splits the
   state mid-frame. Neither cost is priced here. It is the same shape of
   unknown the `rem` ladder already carries, and the same control applies:
   build the rung, measure the lanes, keep it only if the boundary win
   survives the mid-frame loss. Room (2,0)'s own history says this is not a
   formality - dropping the fruit-`off` widening looked like a pure win on
   the same kind of reasoning and came out 5.4x WORSE, for exactly this
   reason.
2. The 20-frame gap is measured against a straight-line extrapolation of the
   f075 wall. The growth ratio is itself decaying (1.29/frame at f045-f050,
   1.12 at f070), so both the gap and the headroom are soft.

### The fields are massively correlated, so per-field counts do not multiply

Distinct values the field takes among the lanes sharing one position -
median and p90 OVER positions, and the value at the most crowded position.
f050 has 4,309 positions (median 1,062 lanes, worst 7,475 at (50, 86)); f070
has 7,590 (median 4,914, worst 77,691 at (48, 88)):

| field | f050 median | f050 p90 | f050 worst | **f070 median** | **f070 p90** | **f070 max** |
|---|---|---|---|---|---|---|
| `spd.x` | 43 | 87 | 139 | **117** | **845** | **2,595** |
| `spd.y` | 28 | 43 | 42 | **34** | **51** | **79** |
| `dash_effect_time` | 8 | 11 | 9 | 7 | 10 | 11 |
| `dash_time` | 5 | 5 | 5 | 3 | 5 | 5 |
| `grace` | 2 | 5 | 6 | 1 | 4 | 6 |
| `dash_target.x/.y`, `dash_accel.x/.y` | 3 | 3 | 3 | 3 | 3 | 3 |
| `djump`, `p_jump`, `p_dash`, `flip.x` | 2 | 2 | 2 | 2 | 2 | 2 |
| a spring's `delay` | 3 | 8 | 7 | 2 | 5 | 11 |

Only `spd.x` moves. Everything else is a handful of values at a position at
BOTH depths - which is the same story the erasure table tells, from the
other side.

The PRODUCT of the twelve candidate fields' counts is 1.8e7 at f070's median
position against 4,824 actual rows - a factor of 15,000, and 205,000 at the
worst position (1.6e10 against 77,691). The reachable state at a position is
a thin sliver of the product space, and `spd.x` x `spd.y` alone is 5.8x
correlated at both depths (f070 median product 4,454, median joint 854).
Three consequences:

* per-field distinct counts are useless for predicting a rung's value; only
  the joint measurements above mean anything. This is the trap the whole
  census exists to avoid;
* the multiplicity is not concentrated in one field to be knocked out. After
  fixing position AND the exact `(spd.x, spd.y)` pair there are still 4.9
  distinct states at f070's median position (5.3 at f050) - so `spd` and
  position TOGETHER explain all but a factor of 5;
* `spd.x`'s per-position spread is heavily skewed - median 117, p90 845, max
  2,595 - so a `spd` rung's value is concentrated in a minority of
  positions. That is an argument for a rung, not against one, but it means a
  per-position bucket count would be a poor summary of it.

Reproduce with:

    ./safe-run.sh --memory 60G -- ./target/release/rewrite \
        --recipe rewrites-room00.jsonl field-census \
        --checkpoint-dir ~/celeste-checkpoints/room20 --frame 70 \
        --collapse player.spd.x,player.spd.y \
        --collapse player.spd.x:0,player.spd.y:0,player.p_jump,player.p_dash \
        --out /tmp/fc070.csv

Two self-checks the tool passes and which are worth repeating after any
change to it: `rows_before` equals the frontier lane count exactly (a
frontier-only boundary has no duplicate rows), and collapsing EVERY
lane-varying field leaves exactly one row per object-array shape (5 at
f045).

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
