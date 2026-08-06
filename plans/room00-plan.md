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
