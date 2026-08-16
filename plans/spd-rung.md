# The spd rung: velocity bucketing below level 0 (room (2,0) unblocker)

2026-08-16, approved by Philippe. Task #110.

## Why

Room (2,0) at level 0 (rem fully widened, everything else exact) grows
~13%/frame with no saturation - 814M rows by f73, ~8 min/frame, horizon
~95. The field census identified `player.spd.x/y` as the multiplicity
carrier (`appr()` accumulates subpixel velocity differences that never
reconverge); bucketing spd at 1 px/frame collapsed distinct rows 14.5x
in simulation (~23 frames of growth headroom vs ~20 needed - tight, so
the design admits coarser bottom rungs). Without the rung the sweep is
also infeasible: RowIndex holds every boundary state in RAM (~100 B/row;
room1's 405M rows = 42 GB), and unbucketed room (2,0) at h95 is billions
of rows.

## The abstraction

`SpdPrecision::WidthLog2(w)`: widen player.spd.x and player.spd.y at the
frame boundary to FLOOR-ALIGNED buckets of width 2^w in raw 16.16 units.
w=16 is 1 px/frame (the census's 14.5x); larger = coarser; `Exact` = no
widening (today's semantics, and the env default - unset changes
nothing).

Philippe's constraint, honored by construction: bucket boundaries are
STATIC and data-independent (floor-align on the raw i32,
`div_euclid`), so the scheme covers the entire speed range without any
assumption about what speeds occur; the game's bounded max speed only
means few buckets are ever occupied. A sanity assert (|spd| < 16 px/f)
guards against insanity without being load-bearing.

Power-of-two floor-aligned buckets NEST: every finer bucket lies inside
exactly one coarser bucket. Nesting gives two things:
- level w+1 over-approximates level w (the ladder's refutation
  direction), and
- band coarsening (map a finer state to the coarser level's row) can
  never straddle, so it needs no splitting - same argument as rem.

Straddle splitting at the boundary (`split_spd_straddles`, the analog of
`split_rem_straddles`): a mid-frame interval that spans bucket
boundaries splits into one lane per bucket, so row identity is canonical
per level. Unlike rem (asserted <= 2 buckets), spd's splitter is N-way
with a sanity cap - `appr` moves endpoints by a concrete accel so
boundary intervals stay narrow, but that is an expectation to assert,
not an invariant to assume.

Only PLAYER spd is widened (census: that is where the multiplicity is);
player_spawn frames have no "player"-typed object and stay exact, same
as rem.

## The ladder (the ground-rule part)

Never widen without a rung that narrows back. Extended ladder per
horizon H:

  S(w_max) -> ... -> S(w_min) -> level 0 (spd Exact, rem Bits(0))
      -> rem k=1..16 (unchanged)

* The PERSISTENT tree (extend + posgraph + sweep, today played by level
  0) moves to the COARSEST spd level - the only level cheap enough to
  extend unbanded on room (2,0).
* Every finer level, level 0 included, runs BANDED from the previous
  level's (e,g), rebuilt per horizon - exactly the k-rung mechanics.
* A coarse win is only ever refuted or confirmed by the finer levels;
  the optimum claim still rests solely on the exact top (k=16).

Precision becomes composite: `LadderPrecision { spd, rem }`. Ordering
in the ladder: refine spd fully to Exact FIRST, then rem - no
interleaving (simplest monotone chain; each level's band coarsening
applies the previous level's full composite widening).

## Plumbing

* Env: CELESTE_SPD_WIDTH_LOG2 (unset = Exact). Read-once like rem's.
* CampaignConfig gains the spd precision, hashed unconditionally per
  the fingerprint doctrine. This re-fingerprints everything, including
  the fresh bench-r1-fresh dir (~20 min to rederive; accepted).
* BandFilter's prev_precision becomes composite; bench grows
  --band-prev-spd alongside --band-prev-bits.
* ladder.sh: SPD_RUNGS env (whitespace list of w, coarse->fine; empty =
  exactly today's script, so room1/room00 campaigns are untouched).
* Shared machinery: rem's bucket/widen/split code generalizes to a
  field-parameterized helper used by both rem and spd, GATED by
  byte-identical room1 f30 artifacts before/after the refactor (the
  A/B harness from the visited-set work).

## Gates, in order

1. Unit tests: bucket nesting, N-way splitting, widen/split round
   trips, Exact = no-op.
2. Refactor byte-identity: room1 f30 artifacts identical before/after
   the shared-helper refactor (spd Exact).
3. Room (1,0) EXTENDED-ladder run (with spd rungs enabled): must
   reproduce the same optimum as its plain ladder on the current tree.
   The known-answer acceptance test for the whole rung.
4. Room (2,0) level-S(16) probe: forward growth curve vs the recorded
   level-0 metrics (which are on disk through f74). Decides the bottom
   rung width and the go/no-go for the full campaign. Numbers go to
   Philippe with the bucket-scheme decision BEFORE the campaign.

## Status 2026-08-16 evening

Machinery LANDED (2d9bae2): composite precision, widening, N-way
splitter (refactor gated byte-identical on room1 f001-f030), band
plumbing, fingerprint, interval min/max/abs (the first builtins widened
spd reaches that rem never did).

**The crossover is REAL and measured** (room1, w=16 vs exact):

| frame | exact rows | spd-16 rows | ratio |
|---|---|---|---|
| f30 | 27,047 | 75,237 | 2.78x WORSE |
| f40 | 902,303 | 1,974,666 | 2.19x worse |
| f50 | 8,033,952 | 4,940,489 | **1.63x BETTER** |

Position-forking from interval flr costs immediately; the dedup
collapse overtakes it between f40 and f50 and widens with depth -
consistent with the census's 14.5x at room (2,0) f070 depth.

**Blocker DIAGNOSED (f56, room1, w=16)** - full chain, one root site:
`anonymous_61` %2184-%2195 (Celeste's `if spd.x ~= 0 then flip.x =
spd.x < 0`): a mid-frame spd.x interval straddling 0 makes `flip.x` a
genuinely-unknown boolean (348 UnknownBool stores traced into heap 171
at f56); the dash's no-input arm `spd.x = flip.x and -1 or 1` then
converts it to an unknown NUMBER and stores it into spd.x (heap 206),
which the boundary widen refuses. Philippe's split-before-compare
design (approved) fixes it at the root: insert `__split_at(value, 0)`
before the flip comparison - a three-way interval split ({<c}, {==c},
{>c}, making EVERY comparison operator against c decidable) modeled on
`split_interval_by_floor`. NEXT: (1) `__split_at` state-splitting
builtin in game_runner.rs next to `__split_by_flr`; (2) a `split_at`
recipe rule (insert call_builtin + redirect later uses, the expand_bool
insertion shape); (3) one recipe entry at this site; (4) rerun room1
f60+, expect more sites to name themselves the same loud way (the
CELESTE_TRACE_UNKNOWN_STORE env + the widen panic now print cells).
Note bucket edges at every integer px coincide with the game's decision
thresholds, so only mid-frame accel drift ever straddles - the splits
should be rare.

**Original blocker note**: `player.spd` carries a literal
UnknownBool at a boundary - a select whose condition went unknown
(tri-state comparisons against the widened spd intervals) stored an
undecided value into spd on some lane; the widen panics (correctly -
that value cannot be soundly bucketed), the deopt catches the panic,
and the PLAIN program then also chokes (`UnknownBool < Number` in
player.update if_body_56 - the plain path has no select machinery at
all). NEXT STEP: find the producing select site(s) in the rewritten
program and apply the existing select-split/expand recipe treatment
(task #96's playbook - it eliminated exactly this class for the btn and
dash sites). Start from the f55 checkpoint in /tmp/spd-depth (fingerprint
needs CELESTE_SPD_WIDTH_LOG2=16 env) and instrument which select stores
into player_spd cells with an UnknownBool result. The select-splits
counter was firing (36k/frame at f55), so the machinery exists - the
new sites are just not in the recipe yet.

## Round 2 diagnosis (2026-08-16 late; after sp1_facing_x)

The facing split WORKS: f56 passes and f55's rows dropped 6.39M ->
5.24M (clipped intervals canonicalize better). The next failure
(f56-f60) is DEEPER and two-layered:

1. Some speculation guard in the rewritten path genuinely fails under
   widened spd -> those states legitimately deopt to the plain program
   (which guard: not yet identified - run with the deopt-collect
   reporting to name it; reducing deopt frequency is a later
   performance question, not a correctness one).
2. The PLAIN program then poisons spd itself, through Lua `and/or`
   VALUE semantics: in `appr`'s `val>target and max(..) or min(..)`,
   when the comparison partitions to unknown, the false arm of the
   `and` returns THE CONDITION VALUE - and the interpreter leaves it
   as UnknownBool instead of refining it to `false` on that edge. The
   UnknownBool then travels: `t or min(..)` branches on t, the truthy
   edge RETURNS t, and appr's result (stored to spd.x, plain-program
   site `player.update_21` %353 = store %340 <- %352) is a literal
   UnknownBool.

THE FIX (next session, fresh context): in the branch machinery
(flow.rs), when a state splits on a whole-value UnknownBool condition,
overwrite the condition LOCAL with Bool(true) / Bool(false) in the
respective successor states. Sound and exact: UnknownBool is a bool by
construction, and each edge knows its value. This is a global precision
improvement, fixes the plain fallback for interval spd generally
(every and/or value idiom, not just appr), and needs its own gates:
byte-identity on room1 f30 exact (no UnknownBool branches fire there
under exact spd? VERIFY, don't assume - btn branches DO fire, so
expect row changes... if rows change, this is fingerprint-relevant and
needs the full nextest + verify + room1-f30-compare treatment and a
careful review of whether hint/merge behavior depends on the
UnknownBool staying opaque).

Also consider the same refinement for MaybeBool spill states
(partition_maybe_bool sets the spill's local to UnknownBool; its
downstream branch could refine identically).

## S(16) SOUND on room (1,0) + the schedule decision (2026-08-16 night)

With sp1_facing_x and the branch edge refinement, spd-16 rem-0 runs
room (1,0) clean to f81+ and finds its FIRST ROOM-EXIT AT f80 <= 89 -
the rung over-approximates correctly on the known-answer room. Row
collapse vs exact at depth: f50 1.63x, f60 3.14x, f65 4.2x, widening.

**Schedule decision - LOCKSTEP/DIAGONAL, from measurement:**
marginal realized fake progress on room (1,0):
  rem full widening: 100 - 89 = 11 fake frames
  spd 1px buckets:    89 - 80 =  9 fake frames
Comparable rates at comparable coarseness -> errors ADD while cost
MULTIPLIES, so the ladder walks the diagonal (refine both dimensions
together, ~16 levels not 32). The leading-edge extraction
(`rewrite leading-edge`) confirms the mechanism: gap 0 px in
speed-capped stretches (caps bind), ~0.3 px/frame in free movement,
peak +24 px at f71.

Remaining for #110: ladder.sh diagonal schedule wiring, the room (1,0)
extended-ladder agreement run, then the room (2,0) S(16) probe.

## Open questions (carry to the probe, not decided by reasoning)

* Bottom rung width: 1 px predicted 14.5x; 2 px if growth still wins.
* Rung count/schedule: start sparse (e.g. 17, 16 -> Exact); each rung
  is a full per-horizon stage, and banded stages are cheap only if the
  tube is tight.
* spd.y granularity vs spd.x (gravity quantizes y harder; census can
  split it out if the probe says the collapse is lopsided).
* How much branch-doubling widened spd causes in move/collision physics
  (tri-state comparisons and straddle partitioning exist and are
  certified, but their cost here is an empirical question).

## Room (2,0) S(16) probe, attempt 2 (2026-08-16 evening): OOM at f47

`CELESTE_START_ROOM=2,0` + spd 1px + rem widened, `bench --frames 65
--deopt --save-frames`, 100G cap. Killed by the cap (exit 137) at f47/48:
lanes 2.6M (f42) -> 7.75M (f46), RSS peak 97.3 GB, frame times 180 -> 730 s.
The expansion hump never bent. Two independent causes, both room-(2,0)
artifacts, neither seen on room (1,0):

1. **The fruit breaks the devirt entries.** Room (2,0) is a 3-object room
   (player/spawn + fruit + ...); the `obj.type.update` devirtualization and
   collapsed object loops assert `player_spawn.update_24 | player.update_21`
   and fail on `fruit.update_34` - "retry failed for a non-premise reason";
   **5,396 whole-state deopt fallbacks** over 47 frames. Every fallback runs
   the plain program on the whole state: the frame cost multiplier plus
   merge-hostile plain-path fragmentation.
2. **Unsplit spd poison sites.** 3.86M UNKNOWN_STORE events:
   LocalId(367) 2.59M, LocalId(18) 1.26M, LocalId(200) 3.2k, LocalId(602) 6.
   These are the room-(2,0)-reachable analogues of the sp1_facing_x site -
   each needs a `split_at` recipe entry (rule exists) at the right
   threshold, or the stored unknowns fan out downstream.

Consequence: the S(16) rung cannot be probed on 300 m until the recipe is
made 3-object-clean - re-derive the devirt/collapse family for the fruit
shape (relates to #113's foreach re-derivations) and place split_at entries
for the four sites. The earlier row counts seen for f055-f065 (5.2M/8.0M/
12.3M) came from a run whose frames this rerun's fresh derivation deleted;
treat them as unverified until reproduced.

## v6 divergence isolation (2026-08-16 late, in progress)

The agent's scratch recipe /tmp/rewrites-v6.jsonl (base minus 39
1-object-specialization entries, plus split_call chain h060s + retargeted
h062 inline + retargeted sp1_facing_x %2168) verifies clean to f38 on
room (2,0), diverges at f39: candidate 67 states vs baseline 36, SAME
lane totals (619,516), "structure differs at heap slots [299, 322]" -
a merge-identity fragmentation, not a value bug. Linear per-entry bisect
measured impractical (24/852 entries in 40 min) - killed; targeted
trials instead, one 40-frame differential each:
- Trial A: v6 minus sp1_facing_x -> STILL DIVERGES (sp1 exonerated).
- Trial B: v6 minus h060s+h062 (split_call chain) -> RUNNING
  (/tmp/verify-noChain.log).
- If B clean: chain guilty, inspect split_call's state partitioning
  (likely: the split leaves a structurally distinct residue on the
  non-player_spawn side). If B diverges: the 39 removals are implicated
  (StateMapping/canonical-structure interaction) - re-add in halves.
Recipes: /tmp/rewrites-v6*.jsonl. Logs: /tmp/verify-*.log.

## v6 divergence RESOLVED: partition_merge pm1 (2026-08-16 ~01:00)

Trial ledger: A (no sp1) diverged; B' (no split_call chain, no sp1)
diverged; C/C' (+c043/c045: c045 re-crashes the spring nil-deref, c043
alone no change) diverged; CONTROL (base recipe) cannot even run room
(2,0) under verify (fruit devirt assert at f1 - the removals were
necessary, so the culprit had to be RETAINED); D (v8 minus pm1)
**VERIFIES IDENTICAL through f40**.

Diagnosis, enabled by the new slot-naming in the divergence reporter
(verify.rs): candidate states held per-state-uniform values where the
baseline had per-lane vectors (observation sentinel Num(i32::MIN)) - the
candidate keeps partition classes the plain baseline merges. pm1's
partitioning reconciled with the plain grouping on room (1,0) (its #69
verify passed) but does not on a fruit room.

Consequences:
- /tmp/rewrites-v9.jsonl (copy: ~/celeste-checkpoints/room20-s16-v9-
  recipe.jsonl) is the VERIFIED room (2,0) recipe: fruit-clean, verify-
  clean, WITHOUT pm1 (which cost: pm1 was -29% time / -49% mem on (1,0)).
- S16 f50 gate probe launched with v9 (room20-s16-v9 checkpoint dir).
- DECISION for Philippe: reinstate pm1 by making verify's observation
  partition-agnostic (merge same-shape states before comparing - aligned
  with the #97 batching-invariance certification, grouping must never
  change results), vs running fruit rooms without pm1. The verifier fix
  is the principled route; not built at 1am.
- Still open: split_at entry for the LocalId(200) UNKNOWN_STORE site
  (3.2k events in the v2 probe - small; f50 probe will size it).
