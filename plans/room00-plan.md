# Room (0,0) "100 m": multi-variant specialization + the fake wall

Goal: prove the optimal TAS for the FIRST room, including the
breakable hidden-strawberry wall. This is a NEW result - both the
2022 solver and the current campaign skipped 100 m precisely because
of the fake_wall. It is also the pilot for shape-dispatched
specialization ("JIT variants"), per Philippe 2026-08-06.

## Recon (verified against cart/map-data.txt and celeste-minimal.lua)

- Room (0,0) spawns exactly two objects: player_spawn at tile (1,12)
  and fake_wall at tile (1,4). Nothing else. load_room scans x then
  y, so the wall precedes the spawn in the objects array.
- fake_wall.update: only reacts to a dashing `player` (spawn-type
  cannot break it). On break: player gets spd.x=-sign*1.5,
  spd.y=-1.5 (an upward impulse - a genuine speed-tech candidate,
  which is WHY modeling it is required for optimality), dash_time=-1,
  wall destroyed, fruit spawned at wall+4.
- fruit.update: bobs via sin(this.off/40) - off is a PER-OBJECT
  counter, concrete per lane, so no widened-sin problem. (The room-2
  blocker is key's sin(frames/30) with the pinned frames global;
  room (0,0) has no key.) Collecting: djump refill, got_fruit set,
  fruit destroyed.
- Object-array shapes (complete): S1=[fake_wall, player_spawn],
  S2=[fake_wall, player], S3=[player, fruit], S4=[player].
  Transitions: S1->S2 (spawn done), S2->S3 (break), S3->S4
  (collect). S4 == the existing room-(1,0) singleton premise.
- Win predicate for (0,0): room becomes (1,0) (next_room on y<-4).

## Design: shape-dispatched variants

- Shape key per lane: the sequence of object type identities in the
  objects array (pointer-walk like room_x_lane_mask). Computed at
  frame boundaries on canonical states.
- Variant registry: shape key -> (recipe, built Program/PreparedCfg,
  StateMapping). Checked-in recipe file per shape. The existing
  recipe is the S4 (and near-S1-singleton) variant.
- Frame step: partition boundary lanes by key; each partition runs
  its variant; unregistered keys run the plain program (existing
  deopt plumbing). Mid-frame shape CHANGES (break/collect frames)
  fail the variant's premises and take the existing lane-granular
  deopt - one frame per transition per trajectory, so steady-state
  never deopts.
- Rows, bands, sweep, ladder, trace-witness, extract-tas, and
  count-optimal are shape-agnostic already; they only need the room/
  win-target parameterization.

## Plan

1. Room parameterization: start room (lua load_room + fingerprint),
   collision cache room, win predicate target, ladder.sh --room,
   per-room checkpoint dirs. Gate: plain-program concrete run of
   (0,0) exits to (1,0).
2. Correctness-first forward: shape-key partition with ONLY plain
   execution for S1-S3 (S4 uses the existing recipe). Full ladder on
   (0,0) end to end. This already yields the proof, just slower;
   measure how much S2 dominates.
3. Variant campaign for S2 [fake_wall, player]: 2-slot loop unroll
   (extend collapse_loop or unroll+assume_eq), per-slot
   devirtualization, field promotion for both objects. Reuse the
   mature suggest/screen tooling. Then S1/S3 only if measurement
   says they matter (S3 lanes exist only post-break; possibly rare).
4. Re-run the ladder with variants; compare wall clock; witness
   extraction; and answer THE question: does the optimal path break
   the wall?

## Notes

- got_fruit: verify _init initializes it when skipping the title
  screen (if_not_fruit reads it at spawn).
- An uncollected fruit adds off-counter phase to state (period-40
  bobbing) - more rows while a fruit is alive, handled by the band
  machinery, no code needed.
- Optimality = fastest exit; the berry itself is score, not time.

## Groundwork landed (2026-08-06, pre-integration)

- `CELESTE_START_ROOM=x,y` (default 1,0): single source of truth in
  game_runner::start_room(), driving (a) a STRICT exactly-once
  substitution of _init's load_room call, (b) the collision-cache
  room, (c) `sin` builtin registration, (d) the config fingerprint
  (hashed only when non-default, so all room-(1,0) checkpoints stay
  valid). sin is registered ONLY for non-(1,0) rooms because a new
  builtin global changes heap layout and therefore every row hash -
  registering it unconditionally would have silently poisoned the
  (1,0) universe under an unchanged fingerprint.
- `Pico8Num::pico8_sin`: PICO-8 semantics (turns, inverted), f32
  chain + C-style truncation, quarter-turn values exact (tested).
  OPEN: bit-exactness against a real console is unverified; the
  fruit bob only evaluates sin on the 40 residues of off/40, so a
  one-time table dump from real PICO-8 pins all of them. Needed
  before trusting a proof where fruit-collection timing matters.
- Gate so far: room (0,0) loads, spawns at (8,112), runs stably
  under concrete_run. The wall-break exercise is deferred to
  integration - the forward search will hit the break lanes
  exhaustively and any fruit/sin bug is a loud error, not a silent
  wrong answer.

## Fidelity deviations (proposed, need sign-off)

The original spawns extra display-only objects that minimal omits:
4 `smoke` per wall break (init calls rnd() - cannot be modeled
exactly AND deterministically) and 1 `lifeup` per fruit collect
(deterministic but display-only). Proposal: omit BOTH, with the
documented-deviation pattern already used for the dropped jump
buffer (celeste-minimal.lua ~line 103): neither object reads or
writes the player, acts as a solid, or influences any state that
interacting code reads, so the player trajectory and frame counts
are identical; and once smoke is omitted (forced by rnd), lifeup's
omission costs nothing further since array indices already differ.

## Room (0,0) layout (from cart map, for route planning)

Spawn (8,112) bottom-left; fake wall x 8-23, y 32-47, resting on a
ledge (tiles x 0-39 at y 48). Left wall column exists only for
y<=79; the spawn column is open to the screen edge (edge clamps, no
wall-jump). Small blocks at (24-39, 96) and (56-71, 88) stair up.
Natural break approach: reach the ledge right of the wall (stand
y=40, x>=24) and dash LEFT into it; the break bounce is spd.y=-1.5
with dash_time=-1.
