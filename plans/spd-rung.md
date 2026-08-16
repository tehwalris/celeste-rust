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
