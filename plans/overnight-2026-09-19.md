# Overnight 2026-09-18/19: room (3,0) to its optimum

Philippe's brief (23:50): get room (3,0)'s full optimal solution, loaded into
the UI; decide without blocking, write the decisions down here.

## Where it stood

The search (`--ceiling 89`, ladder `r0sxhfb, r0sxh, r1sxh .. r15sxh, rxsx`)
finished level 0 to f89 (frontier peaked at 22.7M at f74, 5.4M at f89; its
backward marked 0.4-1.4M states per frame), then level 1 (`r0sxh`: fruit and
floors EXACT) blew up under the level-0 marks - 25.8M kept at f55, 39.0M at
f61, 62 GB peak - and was OOM-killed at the 60 GB cap. The mark filter barely
bounds it: a level-0 mark (floors unknown) admits every exact floor/fruit
state that widens onto it.

## Result

**Room (3,0): optimal 89 frames** (`OPTIMAL win frame: 89`, 03:06). h89
confirmed at all 18 levels; h88 refuted at level 6 (r6sxhb: no win by f88).
It equals the community TAS (TAS4: 62 inputs + 27 spawn frames). The search
itself, with level 0 resumed from last night's tree: 34 min wall (02:32 -
03:06), peak anonymous RSS 12.8 GB (level 1 of h89), 210 GB of checkpoints.
In the UI as `room30opt`, the default run: http://127.0.0.1:3011/celeste/

| h88 level | first win | marked |
|---|---|---|
| 0 | f64 | 21.3M |
| 1 | f74 | 2.34M |
| 2 | f83 | 1.10M |
| 3 | f87 | 356k |
| 4 | f88 | 327k |
| 5 | f88 | 243k |
| 6 | NO WIN | refuted |

What made it fit, in order of effect: the deadline-bounded mark filter
(decision 5), level 0 dropped from memory during the finer levels (6), the
kernel-set cap (4). Floors unknown up the rem ramp (1, 3) cost nothing and
changed no count where they were made exact.

The UI took level index 16 as "exact" and showed 17 levels; it now names
every level from its logged precision (L16 "15 bits/H", L17 "exact") and
takes the count from the run. Checked in the browser.

**Verified on a real PICO-8.** The concrete witness (`rewrite witness
--horizon 89 --level 17`, the reference engine's DFS through level 17's
marks) first died on the fruit: `to_interp_state` refused `got_fruit[3]`
(a sparse integer part), and with those successors skipped it found no
path (3.5M steps, 23,168 skipped) - every marked route takes the fruit.
The interpreter keeps such a table dense with explicit nils and `bind`
flattens it the same way, so `to_interp_state` now does too (`c9e9224`).
Then: **win at f89 in 279 concrete steps, 0 dead ends**, and
`pico8_diff/replay.py --room 3,0` shows the room change to (4,0) at f89
(f88: player at 48,-4). The inputs are `tas/room_3_0_exit_frame_89.txt`:
34 idle spawn frames, right with one jump at f42, a jump at f52, then
dash up-right (38) at f62 and f76 with jumps between.

**Against the community TAS (classic/any/TAS4.tas, 62 inputs).** With the
27 spawn frames first it exits at f89 on the ORIGINAL cart (PICO-8, f88
player at 47,-3), equal to ours. On celeste-minimal the same inputs do not
exit (f89: player at 23,75, falling) - the minimal cart diverges, as for
room (2,0). Both routes are controllable from f28; TAS4 runs right at once,
ours stands until f35 and is 14 px behind at f36, yet both are at (21,59)
at f60 and within 2 px from there to the exit - the first stretch has
slack. **Both take the fly fruit at f77**: the dash at f62 sets it flying,
it rises to (16,23), and at f77 it is gone - TAS4 leaves a `lifeup` at
(14,17), ours (the minimal cart has no lifeup) removes it with the player
at (22,24). (`pico8_diff/replay.py` now prints fly_fruit/fruit/lifeup.)
The fruit looks NECESSARY for 89: the witness run that skipped exactly the
fruit-taking successors searched level 17's marked states exhaustively and
found no win by f89. Taking it also refills the dash (`hit.djump =
max_djump` in `fly_fruit.update`), a plausible reason - not established.

**TAS4's positions against our search (2026-09-20).** At every frame
f28-f88 its position (original cart) is inside every h89 level's allowed
cells, the exact level included (a marked state at that cell with time
left to win by 89). The one miss - level 1, f78, (18,22) not first reached
at f78 - is the cross-frame dedup: that cell was reached earlier. And
`rewrite trajectory` follows TAS4's positions frame by frame in OUR game
and wins at f89: the route is physically ours; only its raw inputs do not
transfer to the minimal cart.

**Room (5,0) (600 m), started 2026-09-20.** TAS6 is 48 inputs; the player
is controllable from f30 on both carts, so ceiling 29 + 48 = 77 (the same
rule gives room (4,0)'s proven 76). But TAS6 only exits under favourable
random numbers: one original-cart replay left the room at f77, four others
(a one-frame-different cart) fell at (82,84). The room uses `rnd` (balloon
`offset=rnd(1)`, and a `rnd(3)`). Our model draws `rnd` as an interval
(`4e2d2e9`), so the search's optimum is the BEST CASE over random outcomes
- as a TAS that manipulates RNG. Worth deciding whether that is the
question we want answered.

Decided with Philippe (2026-09-20): `rnd` stays an interval at EVERY
level, so the ladder's "exact" level is exact except for the random draws.
Run the ladder to exact and see; then, at the exact level, try concrete
RNG seeds one after another until one reaches the win (or all are covered).
The first search died in the room walk: room (5,0) is loaded by `_init`,
so the balloon's `offset = rnd(1)` is an interval in the START state, which
the walk's write discovery never typed as an interval input ("objects[1].
offset was already symbolic"). The walk now types the start state's own
intervals as interval inputs too (`room_constant_lattice`, `c6d11e9`).

Plan, refined with Philippe: `rnd` is an interval at every level EXCEPT
exact; at exact, enumerate the concrete draws until one wins. Issues noted:
`rnd(1)` has 65,536 values (16 fraction bits) and the balloon draws once, in
`_init`, so exact is "the start state with offset = k/65536" per k -
confirming a horizon stops at the first k that wins, refuting one at exact
needs all 65,536 (rooms 3 and 4 refuted far below exact, at levels 6 and 9);
running them all at once is out (the coarser marks cannot tell k apart, so
the exact frontier would carry up to 65,536x), so in batches. Treating
each draw as free is the best case over VALUES, not over PICO-8 seeds.

**Superseded (2026-09-20, with Philippe):** the "refuse the merge" fix below
(the `rnd` taint, `reads_rnd`, the refusal diagnostic, `max_states` 1024)
was REVERTED uncommitted: with the cap raised the room (5,0) walk ran 21 min
(41 CPU-min) without finishing. Replaced by a SPLIT AT A POINT - the
balloon's interval `y` is cut at the player's per-lane position, exactly as
`__split_by_flr` cuts at a fixed grid, except the cut is a runtime value:
`Domain::split_compare`, called from `binop_values` when a comparison with
one interval side and one number side is undecided. A 2-way fork
(`both_values`) is the comparison's value; its validity (the half of the
interval that answer leaves is non-empty: `x < t` true iff `lo(x) < t`,
false iff `hi(x) >= t`; `x > t` true iff `hi(x) > t`, false iff
`lo(x) <= t`; mirrored when the interval is on the right) goes in the
path's guard. No new graph op, no cart edit, no marking.

The second blocker, at level 0 (f51, 4.6M states): the player-balloon hit
compares against the balloon's interval y (`74 < [71, 75]`), and the two
arms cannot merge (the pop refills the dash), so the merge added its
`Known(cond)` premise and the kernel declined the lanes - a coverage gap.
Fix (2026-09-20): a condition that reads a `rnd` draw refuses the merge like
one reading an unknown atom (`Domain::reads_rnd`, beside
`reads_unknown_atom` in `state::merge` and `joins_independent`), so the
two sides stay two successors and an undecided lane emits both. The draws
are a per-frame TAINT carried FORWARD by `Graph::fold` (seeded by each
`range_num` leaf and the cells of the walk's `ival_extra` slots; a point or
boolean constant is never tainted). The first version walked the
condition's cone backward and missed the balloon: `sin` of an interval is a
literal span [-1, 1], so `y = start + sin(offset) * 2` kept no link to the
draw (f50 unchanged at 4,581,199, same gap at f51). With no draw, `fold`
takes its old path and nothing changes.

With the taint, six room (5,0) walk nodes then hit the tracer's frontier
cap (288 and 312 states, limit 256). `CELESTE_TRACE_REFUSALS=1` (new: names
each refused merge's condition and first differing slot) showed the refused
merges are balloon-hit families being paired across: the hit splits every
later outcome of the frame in two (popped + dash refilled, or not), and
`merge_inner` already selects on the SEPARATING decision (`State::path`),
so within a family nothing is refused. 288 = 2 x 144 and 312 = 2 x 156:
the legitimate doubling of those regions' player frontiers. The cap
(`Interp::max_states`) is now 1024 - a limit no other room reaches, so their
kernels are unchanged; it still stops a real runaway before `collapse`.

**Room (5,0) assumptions (2026-09-20, decided while Philippe was busy):**

1. The split cuts only an UNDECIDED ordered comparison (`<`, `<=`, `>`,
   `>=`) with exactly ONE interval side; `==`, two intervals, and anything
   the static ranges already decide are left as before.
2. The interval is NOT narrowed in the heap: each comparison is its own
   2-way fork (shared only for the same operator, interval node and point
   node in one frame). The balloon's two hitbox tests fork independently, so
   a combination no single `y` allows can survive - sound, looser than a
   narrowed split.
3. "Interval" is `is_interval`'s answer: a literal interval constant, a span,
   or anything computed from an interval input cell. So the split also fires
   for the fly fruit at fruit-unknown levels and for speed buckets; neither is
   covered by a gate (room (2,0)'s / room (3,0)'s kernels at those levels
   change; the default ladder's do not - the room (1,0) gates are identical).
4. `rnd` stays an interval at EVERY level, the exact one included, for now:
   the exact-level enumeration of concrete draws is NOT built. So the ladder's
   "exact" level is exact except for the balloon's phase, and a confirmed
   horizon means "wins for some phase trajectory the interval allows" - the
   best case, possibly spurious. To be settled by the seed enumeration (or a
   witness + PICO-8 replay) before calling 77 the optimum.
5. The ceiling is 77 = 29 spawn frames + TAS6's 48 inputs; TAS6 left the room
   at f77 on the original cart in one replay and fell short in four others
   (the rnd draw), so the ceiling is a real solution only under a lucky draw.
6. The ladder is room (3,0)'s without fruit/floors: `r0sxh, r1sxh .. r15sxh,
   rxsx` (held buttons unknown up to the top): a level-0 forward with held
   exact reached 38.4M states at f60 (65 s/frame, 18 GB peak) and was still
   growing x2.6 per 5 frames. Kernel sets capped at 2; level 0 dropped while
   the finer levels run (count-down).
7. The ChoiceSet holds 58 forks a frame; each undecided interval comparison is
   one more. Room (5,0)'s walk fit; a room with many such comparisons would
   panic loudly ("fork N but a ChoiceSet holds only 58"), not go wrong.

**Room (5,0) projection (2026-09-20 23:17, search at level 0 f61):** held
unknown cut level 0 ~3x (1.42M at f50 against 4.58M held exact; 10.6M at
f60 against 38.4M). Growth per frame 1.27 (f51) -> 1.21 (f56) -> 1.19
(f61), ~1.3 us per state per frame, ~0.37 GB resident per million states
(4.7 GB at f61, 6.8 GB peak). If the ratio keeps easing to ~1.11, f77 is
~120M states: ~20 more minutes of level 0, but ~45 GB plus transients
against the 60 GB cap - a real OOM risk around f72-f77, unless level 0
peaks before the ceiling as room (3,0)'s did (22.7M at f74). Then the
backward (minutes), 16 filtered levels (~2 min each in room (3,0): 40-80
min), and h76 (a refutation mid-ladder: 20-30 min). About 1.5-2.5 h in all,
if level 0 fits. An alert fires above 45 GB.

Revised at 23:30 (level 0 f71): the growth STOPPED easing - 1.154 .. 1.160
per frame f65-f71 (24.0M -> 57.4M; visited 395M), frame time x1.27 per
frame (44 s -> 185 s), peak 11.4 -> 25.1 GB (x1.09-1.15 per frame). To f77:
~138M states, ~45-50 min more of level 0 (to ~00:20), peak ~50 GB against
the 60 GB cap - tight. Then the backward (~900M visited: minutes), the 16
filtered levels (40-90 min), h76 (20-40 min): done ~01:30-02:30.

At 23:47 (f74) the memory grows faster than that: anonymous memory mid-wave
23.8 / 27.4 / 31.8 / 36.7 GB at f71-f74 (x1.15 per frame; the door 9.8 ->
15.6 GB), file-backed ~0.05 GB. Projected f77: ~56 GB mid-wave, ~59 GB
peak against the 60 GB cap - about even odds. DECISION: let it run (a
failure costs ~35 min; restarting now would redo all of level 0, over an
hour, even where it fits). If it is OOM-killed at f77, restart with a
coarser level 0, `y2r0sxh` (2 px y buckets, ~half the states; the position
rung is sound - the finer levels narrow it back). The cap is not raised.

**Where room (5,0)'s memory goes (23:58, asked by Philippe).** Not per state:
~100 B per frontier row (rooms (0,0) 126, (1,0) 127, (3,0) 152) and the same
24.7 B per door entry. It is the COUNT: 105M kept at f75 against ~23M at the
peak in rooms (0,0) and (3,0) and 7.1M in room (1,0). `col-census` at f60:
the same varying fields as room (3,0) plus the balloon's `offset` (14
values), `timer` (11) and `spr` (2). `coarse-census --erase balloon`: f60
10.58M rows -> 8.09M states without the balloon (1.31x), f70 49.5M -> 22.0M
(2.25x). So two factors: the player's own reachable states (8.1M -> 22.0M
over f60-f70, room (3,0)'s level 0 peaked at 22.7M) and a balloon-history
multiplier that is growing (every frame more states pop it at a different
moment: their own respawn timer and phase shift, and a refilled dash).

**The real cause, and the fix (00:15, after Philippe asked to widen the
popped balloon's timer).** Widening the timer alone (to the unknown number,
under the floors-unknown flag `b`, `widen::fall_floor_paths`) cut level 0
only 4% at f60 (10.17M against 10.58M): the phase `offset` records the same
pop history. And it records more: `offset` advanced 0.01 EVERY frame the
balloon showed, in every state - so no room (5,0) state ever equalled one
from an earlier frame and the door's cross-frame dedup never fired. That,
not the balloon variants alone, is why room (5,0) outgrew every other room.
`offset` is only read through `sin`, and `sin` of any interval a full period
wide is [-1, 1], so it is now stored as the canonical [0, 1) at every level
(`widen::canon_balloon_offset`, with a full-period premise in `ok`; the block
model's projection does the same, `Rt2::widen_to` step 9). EXACT: through
f50 both trees reach exactly the same 1,374,280 distinct balloon-free states
(`coarse-census --erase balloon --from 1 --to 50`), the new one in 296k rows
at f50 against 1.42M (f40 23.8k against 76.4k, f60 1.08M against 10.58M).
The old search (f76: 122M states, 53 GB peak, projected ~62 GB at f77 against
the 60 GB cap) was stopped at 00:15 and room (5,0) restarted on this model,
ladder `r0sxhb .. r15sxhb, r15sxh, rxsx` (timer exact at the top two).

**First run on it (00:17-00:31):** level 0 to f77 in minutes (2.5M kept at
f70 against 49.5M; first win f73, 172k marked). h77 first wins by level:
f73, f74, f75, f76, f76, f77 (levels 5-16); level 15 marked 617, level 16
(timer exact) 354. Then level 17 (Exact) failed in its kernel build:
"objects[1].offset was already symbolic" - the exact set runs with
`opts.ival` off, and that switch gated the `rnd` interval inputs too (the
start-state seeding, the walk's discovery of interval writes, `key_frame`).
Fix: the boundary's own interval paths stay behind `opts.ival`
(`boundary_ival`), the `rnd`-derived `ival_extra` ones apply at EVERY level,
the exact one included (empty in a room without `rnd`, so its exact kernels
are unchanged). The search resumes from its checkpoint (level 0 reloads,
h77's finer levels recompute - seconds each).

**Room (5,0): OPTIMAL 77 (00:22).** h77 confirmed at all 18 levels (first
win f73, f74, f75, f76, f76, then f77 from level 5 through the exact level;
the exact level marked 921 states); h76 refuted at level 5 (r5sxhb: no win
by f76). Equal to the community TAS (29 spawn frames + TAS6's 48 inputs).
The REFUTATION of 76 is sound for every draw: level 5 holds `rnd` as the
whole interval and the respawn timer unknown, an over-approximation of every
phase and timing. The CONFIRMATION of 77 is for a favourable draw: even the
exact level keeps `rnd` an interval (assumption 4), so it says "some phase
allows 77" - consistent with TAS6 leaving the room at f77 on the original cart
in one replay and falling short in others. A concrete witness and a PICO-8
replay need the seed enumeration (the reference engine refuses `rnd`).
Wall time on the balloon model: ~10 min for the whole search. In the UI as
`room50`.

## The rest of the rooms (started 2026-09-21 ~00:30, Philippe asleep to ~10:00)

Every remaining room in order, level index 6 (room (6,0), 700 m) to 30 (the
summit), none skipped, by `/var/tmp/run-rooms.sh` (progress in
`/var/tmp/rooms-progress.txt`, logs `/var/tmp/celeste-ui/room<xy>.log`). Per
room: spawn = the first controllable frame of a zero-input replay (minimal
cart) - 1; ceiling = spawn + the inputs of `classic/any/TAS<level+1>.tas`;
ladder `r0sxhfb, r1sxhb .. r15sxhb, r15sxh, rxsx` (the flags are no-ops where
a room has no fly fruit / fall floors / balloon); kernel sets capped at 2; a
refuted ceiling (the model cannot reproduce the TAS) counts up from ceiling+1.
On success the room is exported to the UI (`runs.json`, newest first). The
driver stops at the first failure; each stop, its cause and its fix are
recorded below.

- **Stop 1, room (6,0) (01:09): the spawn pattern.** The driver looked for
  `freeze N player`; there the fly fruit prints before the player. Fixed (any
  ` player <n>` on the frame's line). Spawn 23, ceiling 70 (TAS7: 47 inputs).
- **Stop 2, room (6,0) (01:10): fruit unknown fans out.** The level-0 kernel
  build (`r0sxhfb`) fails: the start shape's `foreach` over objects grows to
  331 states (cap 256). `r0sxh` and `r1sxhb` build fine, and the pre-split
  binary fails worse (49 of 296 nodes, up to 448 states) - so not the point
  split: the fruit-unknown abstraction multiplies across this room's 10
  moving platforms. Fix: the fly fruit exact at EVERY level (ladder `r0sxhb,
  r1sxhb .. r15sxhb, r15sxh, rxsx`) - sound, more precise, and fruit-unknown
  was only an optimization. For all remaining rooms.
- **Stop 3, room (6,0) (01:35): level 0 explodes.** x1.3-1.5 per frame: 81.0M
  kept at f50 (47 GB mid-wave) with 20 frames to the ceiling; stopped before
  the OOM. `coarse-census` at f45 (17.9M rows): erasing the platforms changes
  nothing (their positions are the same in every state of a frame), erasing
  the fly fruit gives 7.55M (2.37x: its flight starts at the first dash). The
  real cost is the platforms making every frame's states unique - no
  cross-frame dedup, as with the balloon phase - but unlike the phase they
  cannot be canonicalized (their positions decide where the player stands) and
  their cycle is far longer than the room. Fix: the level -1 cost-to-go filter
  (`CELESTE_LEVEL_MINUS_ONE="H,5"`, plans/level-minus-one.md: sound, the basis
  of room (2,0)'s 95), which drops every state that cannot reach the exit by H
  - exactly the late frames the missing dedup inflates. For all remaining
  rooms (H = the ceiling; the count-up fallback uses its own top horizon).
- **Stop 4, room (6,0) (01:40): the level -1 table cannot be built.** "shape
  1: 8796093022208 fork configurations": the builder enumerated EVERY fork's
  fragments (cap 2^14), and each of the ten platforms' `move` forks twice (its
  `rem` is seeded with the whole [-0.5, 0.5)) - 2^43. Only the located
  object's forks need enumerating: they fix the player's pixel steps, so its
  successor cell is exact per configuration. Two kinds of fork are no longer
  enumerated (`level_minus_one.rs`):
  - another object's own fork (its operand reads that object's slots and none
    of the located object's): read over its operand's whole range (`Split` ->
    the operand, the hull of its fragments; `SplitValid` -> an unknown boolean
    of its own), which joins every fragment's outcome - sound, and costs
    nothing where the objects' positions are ranges anyway (the platforms' are
    [-16, 128]);
  - a POINT split (`split_compare`, 2026-09-20): a free choice of a
    comparison's answer, `Const(0, 1)` as its operand, so it reads no slot and
    the first rule enumerated it - 17 forks in room (1,0), 32 in room (6,0)
    (player against platforms), and room (1,0)'s own table (d = 44, the basis of
    its 99) no longer built either: the point split had silently broken level
    -1 everywhere, unnoticed because room (5,0) ran without it. Level -1 reads
    it as the comparison itself over the ranges (`Symbolic::point_splits`
    keeps each split's may-true / may-false conditions): true where only
    may-true holds, false where only may-false, unknown where both - exact
    where the ranges decide it, a join otherwise.
  Every other fork is enumerated as before. (Enumerating ONLY the forks that
  read the located object's slots lost exactness in room (1,0)'s spawn chain.)
  The arity check runs on every enumerated fork's operand; a range-read fork
  chooses no fragment, so its arity is moot.
  Two more things this uncovered, both also breaking room (1,0)'s table:
  - `7dce5b4` (2026-09-18) capped `flr_ways` at `move_ways` (2) for the region
    kernels, whose lanes' speeds are exact. Level -1's speed is [-S, S] at
    one node - `0.5 + rem + spd` spans 11 floors - so every node violated
    "a fork operand spans 11 floors, arity 2". A `Symbolic::uncapped_ways`
    flag, set only around level -1's trace, restores the full width.
  - A range-read object's `rem` leaves [-0.5, 0.5) under the hull (`rem -
    flr(rem + 0.5)` over an interval loses the correlation): room (6,0)'s
    platform `objects[0].rem.x` went to [-0.6, 0.75]. For such an object
    (`Traced::ranged_objects`) the rem is an ordinary range, widened as it
    grows; the player's rem is still held to [-0.5, 0.5), a finding if not.
  Committed as `78f78a4` (room (1,0): d = 44, OPTIMAL 99; suite and the
  three pinned oracles green). Room (6,0) still did not converge: from pass 2
  on, live outcomes' `ok` undecided - named, `1 > abs(x_after - last)`, the
  platform CARRY loop (`hit.move_x(this.x - this.last, 1)`) traced as zero
  iterations, with `x` and `last` independent ranges ([0, 0] and [-1, 125]).
  In the game `last == x` at every frame boundary (the update ends with
  `last = x`), so the carry is at most 1 px - a relation no interval keeps.
  Tracing the whole loop (up to 144 px) is sound but lets the table teleport
  the player along every platform row; relational folding fails at the wrap.
- **The fix: the platforms' phases** (`level_minus_one::Phases`). A platform's
  update reads only its own fields and `freeze`, and `load_room` re-creates
  it, so in any row at frame t the platforms hold P(u), the state after
  u <= t unfrozen updates. The table now reads each platform slot from
  snapshots P(0..=H) of a zero-input CONCRETE run (the reference
  interpreter), checked to start at the traced start state and to neither
  freeze nor lose its player. A node is evaluated over the snapshots' HULL
  first; only where that breaks a premise is it evaluated once per snapshot,
  exactly (the carry is then exact), joining the successors. The premise is
  CHECKED on every traced outcome: each platform output field's cone reads
  only platform slots and `freeze` (a point split counts as what its
  conditions read), else the build fails. Rooms without platforms are
  untouched (`phases: None`).
  The reference engine runs at the PROCESS-GLOBAL level (atomics set by
  `abstraction::set_level`), and the first attempt died on "the reference
  engine does not run held-unknown levels": the table was built lazily at
  the first flush, inside level 0's wave. The snapshot run now sets
  `Level::EXACT` and restores the saved level on every exit, and the table is
  built EAGERLY at the start of `find_optimum` / `find_optimum_from_ceiling`,
  before any wave, so no kernel dispatch can see the switch.
  With the phases (120 platform fields, 71 distinct snapshots for u <= 70;
  the concrete run's checks all held), pass 1 still had 13,986 violations,
  two kinds: `9 > abs(x_after - last)` definitely FALSE at cell (-2, 80) - a
  right-moving platform wrapping from 128 to -16 under a player at x = -2
  carries it -144 px, past `move_x`'s unrolled 8 (`interp::unroll_bound`,
  the same in every kernel) - and an `ok` of `freeze > 0 or ...`. x = -2 at a
  frame boundary is real: the x clamp is in the player's DRAW, skipped on the
  frame a dash sets `freeze`. So the glitch is a real configuration the
  traced frame does not model, at any level.
- **Unmodelled nodes get d = 1.** A node where a live outcome's `ok` (or an
  enumerated fork's arity, the kernels' `SplitOk`) is not provably true is no
  longer a build failure: it is UNMODELLED and seeded d = 1, the weakest
  bound. Sound: its own bound is trivially valid; a real transition that
  satisfies `ok` is still covered by the (over-approximating) outcomes, so the
  ranges stay inductive; and a real transition that breaks `ok` breaks the
  same premise in the kernels, which decline it as a FATAL coverage gap - the
  search never takes it silently. Position and box violations (the table's
  own premises) stay fatal. The price is precision near those nodes; the
  pass report prints how many and where.
  Room (6,0) with both (`706f287`): 0 violations in every pass; 114, then
  171 unmodelled nodes, ALL at x < -1 or x > 121 (the unclamped post-dash
  cells), none inside the room. The passes are slow - ~5.3 min each (the
  per-snapshot fallback, 71 evaluations per fallback node) and 12 of them
  (each shape's countdowns creep one step a pass before the jump, and the fly
  fruit's `step`/`y`/`spd.y` - its forks are range-read, so its rem is a
  range - joined late) - so this run IS room (6,0)'s search rather than a
  test: the quick binary at `706f287`, `CELESTE_LEVEL_MINUS_ONE="70,5"`, the
  driver's ladder, `--ceiling 70`, log `/var/tmp/l1test60.log`, tree
  `/var/tmp/l1test60` (its 2 h `timeout` wrapper killed so it can finish).
  Exported to the UI by hand; the driver resumes at room (7,0).
  The table: converged at pass 12 (76,800 nodes, 14.3M edges, 0 violations,
  171 unmodelled nodes all outside x in [-1, 121]), built in 3,569 s; the
  start state's d = 31 (the spawn ends at f24, so no exit before ~f55; the TAS
  exits at f70, 47 frames after the spawn - a lower bound, as it must be).
- **Stop 5, room (6,0) (03:56): the filter does not bite.** f45 kept 17.9M,
  f50 81.0M - IDENTICAL to the unfiltered run - at 49.8 GB; killed before the
  f51/f52 OOM. The table is sound but, like every level -1 table, weak: d = 31
  against the real 46 is the usual ratio (room (1,0): 44 against 75), because
  level -1 lets the player move at [-S, S] = 5 px/frame at every node. In the
  other rooms that is enough, since cross-frame dedup keeps each frame's
  frontier to the NEW states; here every frame's states are new (the
  platforms moved), the frontier is the whole set reachable at that exact
  frame (x1.4 a frame), and no state is provably too late until ~f55. f60 would
  be hours per wave whatever the memory. Level -1 cannot carry this room.
  The fundamental fix is to give the dedup back where the states are, on the
  ladder: the PLATFORMS PHASE-UNKNOWN at the coarse levels (like the fall
  floors), exact at the top, the coarse marks filtering the exact levels. What
  that needs in the kernels, and why each piece is sound:
  1. platforms' x / last / rem.x widened to their whole path at coarse levels
     (`Rt2::widen_to` step, a level flag, the paths as interval inputs);
  2. their own `move` fork range-read, not enumerated (platforms have
     `solids=false`: `x += amount`, no loop - an interval amount is fine; ten
     enumerated 2-way forks would be 1024 configurations per kernel);
  3. `last` ALIASED to `x` at trace time - the update ends with `last = x` and
     init sets it, so it holds at every boundary; checked structurally on
     every traced outcome (the output `last` is the output `x`'s node);
  4. a fold `Sub(Add(a, b), a) -> b` (exact in 16.16 wrapping arithmetic), so
     the carry `x_new - last` is the platform's move amount [0, 1] instead of a
     144 px hull; the wrap arm's platform sits at the constant -16 / 128, so
     its collide with the player's exact x is decided per lane.
  Several hours. Meanwhile the driver runs the LATER rooms from (7,0) on the
  release binary at `706f287` (room (6,0) postponed, not skipped: nothing of
  it is lost - the phases, the unmodelled rule and the table stay).
  Working it through against the kernel model, the phase-unknown design is
  blocked at the WRAP, not the carry:
  - the move fork is fine: a literal interval `rem` (every lane alike, as the
    fall floors' inputs) goes through `literal_fragments` - fragments as
    separate trace states, rejoined at the call's return, no per-lane fork;
  - but with x spanning the whole path, `if x < -16 then x = 128 ...` is
    undecided in EVERY lane: an ordinary select needs `Known(c)` (declines);
    two successors per platform compound to 2^10 trace states (the tracer's
    frontier cap is 256); a per-lane interval CELL for x instead makes every
    comparison a point split (a per-lane fork, `split_compare`) - 2^20;
  - wrap-free phase buckets avoid all that but reset the dedup at every wrap,
    and a late wrap brings back the whole frontier at that frame.
  So "platforms unknown" needs a kernel-level answer to an undecided,
  every-lane-alike condition over non-literal arms (a hull select without the
  `Known` premise) - a change to the kernel semantics, not a widening. Not
  forced tonight. The cheaper question first: does a COARSER level 0 (speed
  buckets `s16`, position buckets `x2y2`) bound the growth? With no
  cross-frame dedup the frontier is bounded by the abstract state space, and
  that may be small enough. Measured with `rewrite forward --level` to f45
  against `r0sxhb` (2.5M at f40, 17.9M at f45).
  Result: it cannot even be BUILT. `r0s16hb` (1 px speed buckets) was
  OOM-killed in the kernel build, past 60 GB, after a 2,579-node key fixpoint
  - before its first frame; `x2y2r0s16hb` was at 54 GB and still building
  after 14 min when I killed it (it was starving room (7,0)). So a coarser
  level 0 through speed buckets is out for this room.
  Where room (6,0) stands, for the morning: the table machinery is in and
  sound (`706f287`), but the room needs the platforms phase-unknown on the
  coarse rungs, and that needs a KERNEL SEMANTICS change: a select on an
  undecided, every-lane-alike condition that takes the hull of its arms
  instead of requiring `Known(c)` (the wrap), together with keeping each
  platform's `x` a single value per lane so `x - last` cancels. That is a
  design decision about the kernels, so it waits for you.
- **Stop 1, room (7,0) (04:37): the walk fans out.** Room (7,0) (balloon, six
  fall floors, spring; ceiling 84 = 23 + TAS8's 61) spent 33 min in the
  lattice walk without finishing: its log is 32 `[trace] frontier by flow`
  lines - each printed just before a trace BAILS with "frontier grew to N
  states (limit 256) - something is fanning out without merging back" (N =
  5,236 and 592). The walk records each refusal and, if a shape never traces,
  ends with a hard error listing them (`kernel.rs`, the post-fixpoint check),
  so it was headed for a failure, slowly (each refused trace first grows to
  thousands of states). Killed. Suspect: a multiplicative fan-out across the
  six floors' updates at the floors-unknown level (2^6 x balloon x spring =
  256 is exactly the cap; room (3,0)'s floors walked in 17.6 s). Diagnosis:
  walk-only runs (`CELESTE_KERNEL_ONLY=none`, `CELESTE_LATTICE_TRACE=1`) at
  `r0sxhb` (floors unknown) and `r0sxh` (floors exact).
- **Stop 2, room (7,0) (04:45): the exit wraps.** Both diagnostics panicked at
  once: `win_room_x: start room x=7 would wrap to the next map row`. The cart's
  `next_room` loads (0, y+1) from a row's last room, and three places assumed
  (x+1, y): the win test (`frame.rs`, both the block and the row-set variant,
  compared `room.x` alone), and the position labelling - `pos_graph::
  block_cells` and the kernels' `cell_out` put a row at `x + (room.x -
  start.x) * 128`, which for the wrapped exit is 7 rooms LEFT and one down, off
  the 512 px grid (and the level -1 filter's window assert). Fixed for rooms
  (7,0), (7,1), (7,2): `game_runner::win_room()` returns the exit room (both
  coordinates); both win tests compare both; `pos_graph::room_offset` labels
  the start room 0 and the exit room one ROOM_PX right whichever edge it was
  left by (the documented labelling), and errors on any other room; the
  kernels' `cell_out` uses it.
  With it the diagnostics run: floors EXACT (`r0sxh`) walks cleanly - 6
  shapes, 204 (shape, region) nodes, 621 traces in 4.9 s, 0 refusals - so the
  fan-out is the floors-unknown flag `b`. Floors exact is sound (more precise;
  the flag is only an optimization - the same call as the fly fruit for room
  (6,0)). Several rooms left have fall floors ((1,1), (2,1), (5,1), (6,1),
  (7,1)), so the driver now PROBES each room's floors-unknown walk (walk only,
  10 min; room (3,0)'s took 17.6 s) and falls back to floors exact at every
  level (`r0sxh, r1sxh .. r15sxh, rxsx`) if it does not finish cleanly,
  logging which ladder it chose. (It probes both ends of the ramp, `r0sxhb`
  and `r15sxhb`: the search prebuilds and later runs every floors-unknown
  rung, so a fan-out above rung 0 must not slip through.)
  Driver restarted 04:44 from room (7,0) on the release binary at `c91ed59`
  (`/var/tmp/rewrite-phases`), with the whole remaining list. Room (7,0)'s
  `r0sxhb` probe timed out at 10 min (8 refusals, no other error), so it runs
  floors exact at every level; its walk is clean (6 shapes, 204 nodes, 5.9 s).
- **Stop 3, room (7,0) (04:54): level -1 and the balloon's phase.** The table
  build failed at the spawn chain's first frame: a live outcome's `ok` was
  false on `Hi(offset + 0.01) - Lo(offset + 0.01) >= period` - the balloon's
  full-period premise (`widen::canon_balloon_offset`, room (5,0)): the kernels
  store the phase canonically as [0, BALLOON_PERIOD_RAW] at every level, but
  level -1 seeded the slot with the start representative's point. Room (5,0)
  never ran with level -1, so it did not show; it would have stopped every
  balloon room left ((0,1), (2,1), (4,1), (6,1), (7,1)). First fix (seed the
  canonical period as an interval input) moved the failure, not the cause:
  the premise went from false to UNDECIDED, because over a node's range a
  lane's `Lo`/`Hi` is anywhere in the hull. The fix: at level -1 the phase is a
  plain NUMBER per lane, never an interval input - so the trace makes no
  canonicalization and no premise - with its range the whole period
  [0, BALLOON_PERIOD_RAW] (`objects_of_type_in`): every real state holds some
  concrete phase in it, and the evaluator reads the bob's `sin` over all of
  them. Tested on rooms (7,0) and (5,0) before the restart.

## For the morning

- `concrete_run --object fly_fruit` prints "none" on every frame of a route
  where PICO-8 shows the fly fruit throughout: its `find_object` misses it.

- The fly fruit stays unknown only at level 0: unknown it multiplied states
  above level 0 and overflowed the 58-fork `ChoiceSet` from r8 up. Worth a
  look whether the fruit's undecided `collide` could merge instead.
- The deadline filter changes nothing in any marked set (the gate), but it
  is a new pruning rule in the ladder's soundness argument - worth reading
  `MarkFilter::allowed` and `edges::bfs`'s doc together.
- `refbridge::to_interp_state` now flattens a sparse integer part into a
  dense nil-padded array (as the interpreter and `bind` do). Worth a second
  look that no other table in the cart relies on the sparse form.
- `CELESTE_KERNEL_SETS` is opt-in; with level 0 dropped, room (3,0) peaked at
  12.8 GB, so a default cap is a question of rebuild time vs memory.

## Decisions

1. **Fruit and floors unknown up the rem ramp** (Philippe's suggestion), made
   exact only near the top. `Level::grid_consistent` now allows `f`/`b` at any
   rem rung with an exact position (like `h`); the rung kernel sets take
   `with_fruit`/`with_floors` (`build_registry_for_rung`). Nothing else keyed
   on level 0: the input forks and output widenings go by the domain's flags,
   the mark filter's projection by the coarser level's.
   The fly fruit's widening (`widen_fly_fruit`) used to bail above level 0
   (the fruit's rem.y is no literal once rem is exact): its containment in
   the widened range is now a runtime premise in `ok`, like the region
   bounds - a lane outside declines loudly.
3. **Fruit exact above level 0 after all; only the floors stay unknown.**
   Measured (unfiltered `forward --to 36`, room (3,0), kept at f36):

   | level | f36 kept | |
   |---|---|---|
   | r0sxhfb (level 0) | 37,506 | |
   | r1sxhfb | 79,666 | |
   | r1sxhb | 71,306 | fruit exact is SMALLER |
   | r4sxhfb | 526,195 | |
   | r15sxhb | 175,376 | |
   | r8sxhfb / r15sxhfb | - | panic: 103 forks > the 58 a `ChoiceSet` holds |

   An unknown fruit does not merge anything above level 0 - its undecided
   `collide(player)` forks the player's outcome (collected / not) and both
   are emitted - and at fine rem it overflows the split mask. So the fruit
   goes exact at rung 1 and the floors carry the merge.
4. **Resident kernel sets capped (`CELESTE_KERNEL_SETS`).** The old run's
   RSS was 31.0 GB at level 0's f001 (the first binary, which still kept
   every kernel's fused graph) and 32.5 GB at level 1's f001 with the latest
   binary - of which level 0's held door is ~9 GB (550M visited keys at
   16 B), leaving ~20 GB of the 18 prebuilt kernel sets. A third of the cap
   before level 1 had a single state; it died at 62 GB. (One set alone,
   r1sxhb: 5.5 GB RSS after its build, 8.0 GB peak - allocator retention
   included, so not a clean per-set number; the run below measures it.) The cache now keeps at
   most N built sets and evicts the least recently used (rebuilt when its
   level runs again, ~35 s each on 32 workers - ~10 min per horizon for 17
   rebuilt sets, against hours of forward). Default unset = old behaviour, so
   the gates are unaffected; the room (3,0) run uses 2. Chosen over shrinking
   the per-set footprint because it is certain; where the 1.7 GB goes is still
   unmeasured (the asm text is already dropped; the templates' `Rt2`s are
   the suspect).
5. **Level 1 still blew up; the mark filter gets a time bound.** The first
   run with this ladder (kernel sets capped at 2: level 1 started at 19.2 GB
   instead of 32.5) grew level 1 at ~2x the old one - 339k at f40, 14.6M at
   f51, 24.4M at f53 - and was stopped before its OOM. Level 0 marked only
   456k states at layer 51: the filter admitted fine states onto coarse
   states marked from EARLIER frames, with no time left to win. The BFS
   already knew better: iteration i marks exactly the states that win by H
   from frame i but not i+1, so i is the state's DEADLINE. It is now kept
   (`Marks` -> `Visited`'s per-key value) and `MarkFilter` admits a fine
   state at frame t only onto a coarse one with deadline >= t. Sound (the
   coarse level over-approximates: a fine state that wins from t widens to
   a coarse one that does) and it prunes only states with no winning
   descendant, so every level's marked set, win frame and outcome are
   unchanged - the marks gate is its check (identical, while its level 1
   keeps 99,960 states over all frames against 110,675: -10%; `4413c85`).
   Marks loaded from disk and the
   kernel re-run backward carry no deadline (`u16::MAX`): the old test.
   Unit test: `a_marks_deadline_is_the_last_frame_it_still_wins_from`.
6. **Level 0 dropped from memory while the finer levels run** (count-down
   only, `Ladder::drop_level0`): ~10 GB idle at f89, resumed from disk in
   42 s when the next horizon needs it.
   **Result (h89 level 1, r1sxhb):** level 1 started at 5.6 GB RSS (19.2
   with level 0 held, 32.5 with every kernel set), peaked at 15.7M states at
   f54 and then SHRANK - 3.2M at f60, 2.8M at f63 - where every earlier run
   was still growing (39M at f61 before the OOM). Peak anonymous RSS
   12.8 GB; the slowest frame 60 s.

   | frame | old r0sxh | membership only | + deadline |
   |---|---|---|---|
   | f45 | 1.09M | 2.11M | 1.81M |
   | f50 | 6.06M | 11.0M | 7.18M |
   | f54-55 | 25.8M | (24.4M at f53) | 15.7M (peak) |
   | f60 | 32.1M | - | 3.17M |

   The ladder then tightens fast (h89; each level ~2 min plus a ~30 s
   kernel-set rebuild under the cap):

   | level | first win | marked |
   |---|---|---|
   | 0 r0sxhfb | f64 | 26.7M |
   | 1 r1sxhb | f74 | 3.59M |
   | 2 r2sxhb | f83 | 1.68M |
   | 3 r3sxhb | f87 | 0.72M |
   | 4 r4sxhb | f88 | 748k |
   | 5 r5sxhb | f88 | 696k |
   | 6 r6sxhb | f89 | 813k |
   | 7-13 r7..r13sxhb | f89 | 492k-730k |
   | 14 r14sxhb | f89 | 352k |
   | 15 r15sxhb | f89 | 80,271 |
   | 16 r15sxh (floors exact) | f89 | 80,271 (same count, new keys) |
   | 17 rxsx (exact) | f89 | 59,268 |

   **h89 CONFIRMED at every level** (levels 1-17 in ~35 min, done 02:54).
   A trial `export-ui` of h89 alone (log cut at the confirmation, into a
   scratch dir) read the custom ladder's 18 levels fine: 6.8 MB in 63 s. Floors going exact at
   level 16 changed no count: on every winning route the fall floors behave
   as "unknown" does. The first win reached the ceiling at level 6.

   With this much headroom the kernel-set cap could go up (each rebuild is
   ~35 s, 17 per horizon); left at 2 - not the bottleneck.
7. **The crashed attempt's finer-level trees deleted** (`h089/`, `level01/`,
   63 GB): built under the old ladder. `level00/` (153 GB) is kept and reused -
   the new ladder's level 0 is the same `r0sxhfb`. Nothing on disk records a
   level's spec, so the resume would not notice a mismatch; the reuse rests on
   the level-0 kernels being unchanged by tonight's edits (the fruit premise
   only fires on a non-literal, which level 0 never has; the rung-set flags
   change only the non-level-0 sets).
