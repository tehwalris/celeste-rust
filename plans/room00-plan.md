# Room (0,0) "100 m": multi-variant specialization + the fake wall

## LATER FINDING (2026-08-15, from room (2,0)): the k16 band dropped
## every fruit-alive lane

The fruit-`off` widening widened `off` and not the bob POSITION derived
from it, so a level-16 row coarsened to k15 was a row k15 never had, and
the band filter dropped it as an "unknown coarse row". In THIS room that
was invisible and harmless - the fruit exists only after the wall break,
the optimal path never breaks the wall, and every refutation here happened
at k <= 7, where both levels widen the bob identically - but it means the
k16 stage of the 94-frame convergence was searching a strictly smaller set
than it should have been. The 94 stands (a concrete 94-frame witness exists
and the refutations are unaffected); what would NOT have stood is a k16
refutation of some horizon, had one occurred. Fixed in
`make_state_abstract_rem`; see plans/room20-plan.md. Room (0,0)'s row
hashes move slightly as a result (a fruit's `y` is the bob band from its
creation frame rather than from the next one).

## RE-DERIVED (2026-08-10) on the fixed interpreter: STILL 94

The whole ladder was rebuilt from nothing after the three interpreter
fixes (partitioned mixed comparisons, `select` splitting on an
`UnknownBool` condition, `select` with an `UnknownBool` arm), because
those make the abstraction strictly more precise and so invalidate
every pre-fix checkpoint. Result unchanged: **CONCRETE OPTIMUM = 94**,
all 17 levels winning at horizon 94, and every level's backward
`min(e+g)` equal to its own forward first-win.

The L-curve, which is the thing that could have moved and did not:

    L(0)=80  L(1)=88  L(2)=88  L(3)=91  L(4)=92  L(5)=93  L(6)=93
    L(7..16)=94

CORRECTION to the sentence below: it says "horizon 94 converged
through ALL levels k=1..16 with min(e+g)=94 at every level". The
second half is WRONG - the coarse levels bound at 88-93, as the L-curve
above shows and as the refutation list in the same paragraph always
implied (k=1 refuted at h87 means L(1)=88, k=3 at h90 means L(3)=91,
and so on). Every one of those seven values is reproduced exactly by
the 2026-08-10 run, which is the strongest evidence available that the
fixes did not move the abstraction in the unsafe direction: more
precision can only raise a level's bound, so a value BELOW the recorded
one would have meant the set had grown.

Cost: 2.62 h of useful work. The old estimate was 13 +/- 2 h and was
mostly a deopt that no longer exists. Full breakdown, and the
position-graph OOM that the doubled fragment count caused, are at the
top of BENCHMARK_DATA.md.

Two operational facts from that run, both binding:

* the level-0 position-graph replay peaks at **101.08 GB** on its last
  frame and needs `MEM=108G`; per-frame peaks are near-linear in lane
  count (74.19 / 83.00 / 91.31 / 101.08 GB at f090 / f091 / f092 /
  f093), so **horizon 95 does not fit on this machine** without work on
  that transient. It must also be built one process per few frames -
  glibc does not return the arenas between frames.
* `~/celeste-checkpoints/room00-prefix-stale` is the pre-fix tree, kept
  aside deliberately. It is not comparable to anything the current
  binary produces and must never be resumed from.

## RESULT (2026-08-07): CONCRETE OPTIMUM = 94 FRAMES - PROVEN

The optimal TAS for room (0,0), with the breakable strawberry wall
fully modeled, is 94 frames (28-frame spawn + 66 input frames) - exact
parity with the community TAS database's 66-input record, which is
hereby proven optimal. The optimal route runs RIGHT, up the stair
blocks, exiting at the top-right corner (x=103, y<-4 at f94); it never
touches the fake wall - the berry-bounce speed tech is proven
non-optimal, not just assumed.

Certificates, all three in agreement:
- Ladder: horizons 80-87 refuted at k=1, 88-90 at k=3, 91 at k=4,
  92 at k=5, 93 at k=7; horizon 94 converged through ALL levels
  k=1..16 with min(e+g)=94 at every level (level-0 abstract bound 80).
- extract-tas walked the k=16 exact band greedily to a concrete
  94-frame input sequence (tas/room_0_0_exit_frame_94.txt), final row
  g=0, player concretely in room (1,0). Ran under the RECIPE program.
- trace-witness replayed that TAS under the PLAIN program: in every
  probed level's band at every frame, e+g=94 throughout, g=0 at f94.
- concrete_run (standalone tool) reproduces the win at f94 after
  fixing its two bugs (below).

Known-deviation note: the community TAS's byte stream does NOT replay
under celeste-minimal - expected, since minimal drops the jump buffer
(the documented deviation) that console TASes lean on. The equal
frame counts cross-validate both results; the byte streams are not
interchangeable.

Tool bugs found by the closure gates (both fixed):
- concrete_run never applied apply_start_room: under
  CELESTE_START_ROOM it simulated a franken-room ((1,0) objects with
  the configured room's collision cache). This also explains the
  early hand-TAS confusion in the recon phase.
- player_xy_per_lane read objects[1] (the fake wall in this room)
  instead of finding the player by type.

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

   DONE 2026-08-16 as far as the existing rules reach, and the answer
   is **0.0%** - `rewrites-room00-s2.jsonl`, 738 entries, 68 of the 102
   shape candidates screened clean, every lane from f028 dispatched,
   zero fallbacks, 22.0 s either way at f048. Numbers and the control
   at the top of BENCHMARK_DATA.md.

   What this step got wrong, and it is worth keeping: "2-slot loop
   unroll, per-slot devirtualization" was written as if the two were
   independent. They are not, and the ORDER is the opposite of the one
   attempted. Devirtualising `type.update` inside `anonymous_61`
   (`split_call` on the `player` global, `fake_wall.update_37` in the
   other arm) works fine and is worth nothing, because `anonymous_61`
   is still ONE function called for both objects - so inside it neither
   the receiver nor any `objects[i]` has a static identity, and the
   `assume_eq` fold that made room (1,0) fast (`objects[1] == this`,
   hence every check nil) has nothing to stand on. The 2-slot unroll
   has to happen FIRST, in the caller, and `anonymous_61` has to be
   inlined per slot; only then is each slot's object identity static
   and the inner collapse + `assume_eq` meaningful. That needs `peel`
   (rewrite-plan.md section 4, unimplemented) plus a per-slot inline.

   Note also that even done properly this room cannot get room (1,0)'s
   fold in full: the fake wall is a real solid the player collides
   with, so slot 2's check against it is a genuine bbox test, not nil.
4. Re-run the ladder with variants; compare wall clock; witness
   extraction; and answer THE question: does the optimal path break
   the wall?

## Notes

- got_fruit: verify _init initializes it when skipping the title
  screen (if_not_fruit reads it at spawn).
- An uncollected fruit adds off-counter phase to state (period-40
  bobbing) - more rows while a fruit is alive, handled by the band
  machinery, no code needed. BUT: `off` increments forever, so
  fruit-alive rows never dedup across frames (an embedded frame
  counter). If post-break rows blow up, a conservative widening
  `off := off mod 40` is justifiable - `off`'s only read is
  `sin(off/40)`, which has period 40 in `off` (verify with a read
  census + widencheck before trusting). Not implemented.
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

## Fidelity deviations (SIGNED OFF by Philippe 2026-08-06)

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

Sign-off notes: this is the approach for now; eventually we want a
cleaner, mechanically-checked version of the non-interaction
argument - treat it as a known issue, not a settled proof. On sin:
Philippe will test bit-exactness against a real PICO-8 himself at
some point; the f32+truncation implementation is believed correct
and the stakes at this level are low, so it is not a blocker.

## Level-0 memory wall (measured 2026-08-07)

The shape-agnostic recipe fixed SPEED (44s/frame at f60 vs 130s on
the plain fallback, identical 5.69M lanes) but the level-0 forward
OOMed at the 100G cap during frame 80: f79 = 12.1M frontier lanes,
71.6 GB boundary rss (+3.3 GB/frame), 200.4M visited rows - already
1.3x ALL of room (1,0)'s campaign (151.6M) with the win still ~30-45
frames away. Per-lane cost is the same ~6 KB as (1,0); the frontier
is simply ~2.3x bigger at equivalent depth. Shape census at f79:
63.3% [fake_wall,player], 33.5% [player,fruit] (16 break cohorts,
off 1..17), 3.3% [player]. Projections at the win (~f105-125):
frontier ~40M lanes / ~240 GB, visited 500M-1B rows, sweep CSR far
past 100G. Program specialization CANNOT fix this - lanes are
abstraction-level. Options considered:

1. Coarser rung BELOW level 0: widen the p_jump/p_dash held-button
   trails to unknown at boundaries, as a refutable over-approximating
   ladder level (-1). Merges the held/released trail variants that
   the count-optimal census showed carry ~10^29-fold multiplicity.
   Level 0 then runs banded by level -1 like every other rung, and
   everything downstream (visited, sweep, bands) shrinks. NOTE: this
   widening was explicitly rejected by Philippe as the BASE
   abstraction (it admits input sequences the game forbids - see the
   NOTE in inspect.rs); as a ladder rung, over-approximation is the
   design and the exact levels refute it. NEEDS PHILIPPE'S SIGN-OFF
   (never-resurrect list).
2. External-memory frontier + sweep (disk-spilled boundary states,
   streamed expansion, sharded merge, external CSR). Sound, no
   abstraction change, removes the wall for any room; 1-2 days of
   infra and it must cover the sweep too, not just the forward pass.
3. off := off mod 40 (see the note above): sound and cheap but only
   bites once off >= 40; at f79 all cohorts are still below 17.
   Worth doing regardless; not sufficient alone.

## Queued after convergence: the fruit fast path (spec'd 2026-08-07)

Two-part fix so fruit states run the recipe instead of whole-state
plain fallback (currently ~10x-chunked as mitigation):

1. Per-lane tri-state interval comparisons: only genuinely straddling
   lanes lose definiteness. The v1 collapses the whole value to
   UnknownBool when ANY lane straddles, which in a million-lane state
   is always - it destroyed the sliver (h89: 5.9M lanes deopted for
   what per-lane analysis would have made a few thousand). The
   acceptance criterion (Philippe 2026-08-07): LANE INDEPENDENCE - no
   lane's imprecision may affect any other lane's execution. That
   invariant is what makes vectorization, chunking, merging and dedup
   correct; the v1 comparison violates it. Likely shape: a tri-state
   bool value (MaybeVector of Option<bool>), definite lanes routed
   per-lane at branches, only the unknown sliver double-executed.
   General form (Philippe): BATCHING INVARIANCE - for any partition
   of a state set, running the parts separately and running the
   concatenation must produce the same canonical outputs, modulo only
   the declared widenings; SIMD-ness is a pure optimization, never a
   behavior change. Certify mechanically with a `simdcheck` in the
   deoptcheck/widencheck family (singleton-split vs batched runs,
   observations compared per frame) - it becomes the tri-state work's
   acceptance gate. Note: the v1 violation only INFLATES coarse
   levels (a definite lane batched with a straddler gets dragged down
   the wrong arm - sound over-approx, visible as extra rows); the
   exact level k16 never sees interval comparisons (off concrete), so
   the campaign's proof is insulated.
2. select on an unknown condition resolves by EXPAND-STYLE LANE
   DUPLICATION (the btn fan-out machinery): each unresolved lane
   splits into its A- and B-resolution, exact and externally
   invisible. Explicitly REJECTED (Philippe 2026-08-07): the numeric
   interval-hull shortcut - select(unknown, 3, 7) -> [3, 7] admits
   values no execution produces and decouples the value from its
   condition; a semantic widening hidden in an instruction rather
   than a declared per-level abstraction. Duplication only.

## Room (0,0) layout (from cart map, for route planning)

Spawn (8,112) bottom-left; fake wall x 8-23, y 32-47, resting on a
ledge (tiles x 0-39 at y 48). Left wall column exists only for
y<=79; the spawn column is open to the screen edge (edge clamps, no
wall-jump). Small blocks at (24-39, 96) and (56-71, 88) stair up.
Natural break approach: reach the ledge right of the wall (stand
y=40, x>=24) and dash LEFT into it; the break bounce is spd.y=-1.5
with dash_time=-1.

CORRECTION (2026-08-06, full tile decode): the room has THREE spike
patches (tile 17, up-spikes), all on the low route right of spawn:
px x 40-55 at y 112 (floor level), x 72-87 at y 104, x 88-111 at
y 96. Deaths ARE possible in (0,0) - expect kill/respawn lane churn
like room (1,0), not the death-free search hoped for earlier. The
only object tiles remain player_spawn (1,12) and fake_wall (1,4);
everything else is terrain/decoration.
