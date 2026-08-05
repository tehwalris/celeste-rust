# Overnight log, 2026-08-05 -> 08-06

Free-run session on the state-structure findings (plans/state-structure.md).
Every change: full test suite + `rewrite verify --frames 37` + interleaved
3-round A/B on two binaries; regressions parked with numbers, one idea per
commit. All commits pushed to `census` (and `interpreter` follows it).

## Landed, in order

| change | runner `-n 39` | bench (rewritten) | commit |
|---|---|---|---|
| baseline (morning) | 44.36 s / 16.2 GiB | f37 2.89 s | 871178d |
| output-cardinality census (`CELESTE_INSTR_CARD`) | measurement | measurement | e981876 |
| uniform vectors collapse to `Scalar` at construction | 40.64 s (-8.4%), -8.6% mem | f37 2.71 s | 487e236 |
| virtual-concat merge (4th attempt, landed) | 39.03 s (-3.4%) | f37 2.57 s, peak -28% | ec5018f |
| ranged filter gathers (`KeptLanes`) | 33.54 s (**-13.5%**) | f37 2.38 s | bde95ef |
| parallel virtual merge (scoped threads, <=16) | 31.89 s (-4.8%) | f40 8.80 s (-10.6%) | (merge: parallelize) |
| union_diff guarded pass-through | **29.06 s (-8.8%)** | f40 **7.68 s (-12.1%)** | afd0653 |
| partitioned probe (hash high bits, >=512k rows) | 29.01 s (-0.4%) | f40 7.44 s (-2.4%) | (probe) |

| ~~state-parallel flow~~ REVERTED by morning review: only helped the plain program, which the search does not run | (-22.4% plain, gone) | flat | (reverted) |
| select pick loop specialized per arm repr | flat | f42 -2.6% | (select) |
| one-pass branch split (both edges at once) | 21.55 s (-3.4%) | f42 within noise | (branch) |
| parallel dense pack (tiles on scoped threads) | 20.97 s (-2.8% median) | f43 -1.2% | (pack) |
| chunk-buffered dedup verify (perf-guided) | flat | f43 -3.4% | (verify) |

**Milestone gate: `rewrite verify --frames 40` identical on the full
stack; `CELESTE_CHECK_UNION=1` at `-n 40`: zero mismatches.**

**Cumulative (before the morning revert of state-parallel flow, which
only affected the plain runner): runner -n 39 44.4 -> 21.0 s (-52.7%);
post-revert the plain runner is ~27 s (-39%) and the rewritten path -
the one that matters - keeps everything. bench f42 26.5 ->
~19.4 s, f43 32.2 -> ~30.8 s since the mid-night frontier.
Final headline re-run: runner -n 41 = 50.1 s / 40.2 GiB (was 107 s
yesterday, 87 s on the parallel branch) - 2.1x yesterday's baseline.**

Morning headlines (final binary):

| | time | peak |
|---|---|---|
| runner `-n 40` | 36.2 s | 24.8 GiB |
| runner `-n 41` | **54.0 s** | 41.3 GiB |
| runner `-n 42` | **81.9 s** | 62.5 GiB |
| bench `--frames 43` | **32.2 s** | 13.7 GB |
| bench `--frames 44` | **61.0 s** | 18.7 GB |

Yesterday's baseline reached frame 41 in 107 s; the overnight parallel
branch did 87 s. Tonight's stack does **54 s** - 2x yesterday. Frame 42
on the plain path - which yesterday's log said "needs bandwidth or task
volume" - now completes in 81.9 s / 62.5 GiB. On the rewritten path the
budget map is measured through f44: **frame 44 in 61 s / 18.7 GB**, so a
60 s budget reaches frame 44 and 120 s reaches ~frame 45 (~110 s
extrapolated, ~31 GB) - two frames deeper than the morning estimates.
Note the us/lane creep at depth (10.0 at f43 -> 13.3 at f44): the merge
grows superlinearly; the dedup input volume lever is not done.

Probe threshold lesson: at 16k rows partitioning was +0.3% runner (extra
O(n) passes beat mid-size savings); at 512k - where the map actually
outgrows the cache - both paths win. vm_probe 0.86 -> 0.43 s at f40.

Frontier measured mid-night (before the last two wins): bench f40 9.76 s /
3.70 GB, f41 16.07 s / 6.05 GB, f42 26.47 s / 9.36 GB (~1.65x/frame);
runner -n 40 50.4 s / 23.8 GiB. With the last two wins, subtract ~20%.

## Parked with numbers

* **Persistent seen-set at hint blocks**: +0.9% runner. Every hint fixed
  point converges in one round; nothing to save. (BENCHMARK_DATA.md)
* **Context-sorting merged lanes**: +3.3% runner / -3.4% bench split.
  Coding+permute on the plain path's 600k-lane boundary merges costs more
  than filters save. Natural lane order is already context-clustered.
* **(earlier that day) if-conversion of the hot forks, allocator tuning,
  block tiling** - see BENCHMARK_DATA.md.

## The important non-perf finding

`NormalizedState` (union_diff at hint blocks) compares vector columns by
**per-column sorted unique value sets** - an over-approximation of state
equality that could silently drop real states, invisible to differential
verify (both programs share the mechanism). `CELESTE_CHECK_UNION=1`
diagnostic added; measured **zero mismatches** (f37 rewritten, f38 plain) -
latent, not active, because arrivals are one-per-shape and fixed points are
single-round. The landmine is documented in the union_diff commit; if hint
blocks ever iterate (loops through hints), the coarse equality becomes
load-bearing and must be made exact first.

## Where the remaining time goes (bench f40 --profile, after all of it)

cfg:anonymous_61 self ~4.0 s (45%) - the frame body: instruction loop +
per-state overhead. virtual_merge ~2.1 -> ~1.3 s est. filter_branch ~1.3 s.
gc 0.07 s. Everything else small.

## The f44 profile (what to attack next, measured)

bench --frames 44 --profile: cfg:anonymous_61 self 34.7 s (56%),
filter_branch **9.8 s** (16%; it was 1.3 s at f40 - superlinear in
lanes, the two hot fork sites again), vm_pack_verify 5.6 s (pack still
sequential), vm_hash 3.7 s, vm_probe 3.2 s, everything else <1.5 s.
The two depth levers: (a) the frame body - but NOT via bandwidth:
perf stat at f40 measured IPC 1.95 and ~2.8 GB/s of DRAM traffic (539M
cache misses over 12.4 s), nowhere near saturation. The body is compute-
side with L3-resident per-state working sets, which also reframes the
parallel-flow regression on this path (likely mutual L3 eviction between
concurrent states, not bandwidth). The dictionary/per-context case
therefore rests on the census fact (100% of instruction time recomputes
values already present in the same vector, <=59 distinct) - the win is
computing each distinct result once per context, not narrower streams.
(b) filter_branch = the dash_time forks; if-conversion is measured
impossible (2.2x), so the fix is run-structured masks or splitting the
state permanently at those sites.

instr_time at f44 (same binary): **select alone is 13.4 s** of ~35 s
flat instruction time (call spans nest callees, 51.5 s by-site;
call_builtin 10.6 s). Fragments only grew 695 -> 1087 f37 -> f44, so the
depth growth is in per-lane op volume, and select - whose outputs the
census showed carry <=51 distinct values - is the single best target for
per-context/dictionary evaluation. Start there next session: select's
cost anatomy (gather vs allocation vs arms), then a prototype that
evaluates hot selects per distinct (mask, arm-value) context instead of
per lane.

## Op-level anatomy at frame 42 (CELESTE_CENSUS, frame 42 alone)

| op | time | ns/elem | GB/s |
|---|---|---|---|
| binop | 2.02 s | 0.51 | 14.7 |
| state filter | 1.57 s | 1.00 | 11.4 |
| dedup bucket (probe+pack+verify wall) | 1.55 s | 20.7/row | - |
| select | 1.42 s | 0.63 | 15.0 |
| hash_rows | 0.59 s | 0.46 | **42** |
| map | 0.17 s | 0.32 | - |

hash_rows runs at ~3x the GB/s of select/binop/filter: the latter are not
bandwidth-limited, they are allocation- and mixed-stream-bound. So the
per-lane ops have ~2-3x headroom even before any representation change,
and the representation change (per-context evaluation over <=59 distinct
values) is worth ~10x on top for the ops it can cover. The two fork
sites again: in_i1_074_cont 0.81 s + and_or_join_126 0.57 s of filter at
frame 42 alone.

## The cross-fragment memo: priced, not built

A pricing census (CELESTE_CENSUS=1, `op memo pricing` line) asked whether
ops recur with Arc-identical inputs across fragments: 17.8% of calls,
**12.7% of output elements** - under 2% of total time at depth, not worth
a Mutex/retention-bearing cache. What it rules out matters more: the
'100% duplicate-lane computation' is within-vector redundancy, so the
only lever that reaches it is per-context/dictionary evaluation. That is
now unambiguously the top item.

## Per-context evaluation priced and CLOSED (dict_pricing microbenchmark)

Two rounds - the first accidentally pessimized the direct baseline 4.5x
with a fn-pointer call per lane. With honest inlined baselines, at real
cardinalities over 2M lanes:

  on-the-fly dict:  1.9-20.8x slower (hash-coding per lane >> arithmetic)
  pre-coded inputs: add 1.03-1.35x (slower or equal!), div 0.61-0.83x

**Per-context evaluation loses on cheap arithmetic even when the codes
are free.** The dictionary/context branch of state-structure.md is
measured out. What remains of the representation idea: u8 columns for
the bandwidth-bound phases only (row hashing runs at 42 GB/s - actual
DRAM saturation) and the ~4x memory footprint at depth. Neither
justifies whole-pipeline code plumbing on its own; revisit only if
memory becomes the binding constraint again.

Next session's honest top items are therefore: (1) the two dash_time
fork sites (filter_branch superlinear at depth), (2) whatever a fresh
profile of the frame body's ~55% shows beyond instruction time, (3)
memory-side work if the search pushes past frame 45.

Post-fix perf top symbols (bench f42, CPU shares; wall shares are lower
for the parallel merge pieces): virtual_unique_mask machinery 10.3%
(was 17.7% before the chunk-buffered verify), select::pick 4.5%,
interpret_binary_op 4.0%, the tile_flag_at builtin family ~3.2% (worth
a look: expensive per lane over positions with cardinality <=59 - the
one op family where the dict_pricing verdict does not apply), recipe
replay ~2.4% (a constant bench includes), allocator ~2%.

## Morning session 2 (2026-08-06, with Philippe): the partition line

* **Context-partitioned merge landed (env-gated pilot)**: merges group by
  (shape, values of designated cells); lane-varying states split per
  class first. Key dash_time,djump - the two hot forks' condition cells,
  read from the IR. Both forks now route instead of splitting.
  f42: 18.37 -> 13.08 s (**-29%**), 9.5 -> 4.84 GB (**-49%**).
  f44: 61 -> 40.6 s (dash_time) -> **33.9 s / 10.8 GB** (dash_time,djump;
  -44%/-42% vs this morning). **f45: 54.6 s / 15.3 GB - the first frame
  45 ever, inside a 60 s budget**; f46 extrapolates to ~88 s.
  Boundary-only variant (hints removed) measures far worse - the hint
  merges and the partition are complementary. Graduation to a recipe
  annotation (program-carried, per-site keys) is next.
* The union_diff pass-through guard aborted - correctly - when djump
  varied at a hint site: with partitioning, arrivals are one per
  (shape, class), and the guard now uses that key.
* **would-dedup census built** (CELESTE_WOULD_DEDUP=1): ~59% of lanes are
  already duplicates at nearly every block from frame start, uniformly.
  But the obvious exploitation - add_hint at the earliest once-per-frame
  59% site - is **+30% time / -17% memory**: removing them costs more
  than carrying them, at least at that site. Parked; the census remains
  the map, and the memory drop makes it a dial if depth becomes
  memory-bound. The buttons-widened variant showed zero delta everywhere,
  which needs a resolution-bug check before trusting it.
* Hint removal itself: parked earlier at +3.8% (hints are load-bearing);
  the unsound NormalizedState comparison is deleted outright.
* **The 59% explained** (and the census caveats with it): would-dedup
  counts *intra-fragment* duplicates - each fragment is a single worklist
  item, and buttons are scalar within one, so the widened variant is
  trivially identical (no resolution bug). The duplicates themselves are
  the **rem/flr cycle**: lanes differing only in stale `rem` become
  widened-equal at frame start - after the boundary merge already ran on
  fresh rem - and the mid-frame flr splits then re-concretize rem,
  recreating exactly the lanes an early dedup removes. Hence the early
  hint's +30%: kill 59%, pay a merge, flr resurrects them. The
  redundancy is structural to the interval abstraction. A variant that
  deferred the flr re-split (keeping rem widened longer) was considered
  and **ruled out by Philippe (2026-08-06): it changes the normalized
  result set, and the abstraction's precision is not up for trade. Do
  not revisit.**

## Afternoon: key sweep + class-dead speculation sizing (2026-08-06)

* Key sweep at f42 found the knee: +freeze adds zero classes (constant
  mid-game); +grace,dash_effect_time is +45% time (758 mean fragments);
  +spd-signs is +500% (5,398). **The 5-cell key is the optimum.**
* Frontier with it: f45 47.6 s / 8.5 GB, **f46 70.0 s**, **f47 104.1 s /
  16.3 GB** (12M lanes) - six frames past yesterday at equal cost.
* Philippe's constant-folding question, sized: 73% of select executions
  route on uniform masks; they discard **770M vector-arm lanes** (vs
  ~5.8B total select+binop lanes at f42) - >=13% of vector work is
  class-dead speculation, more counting the discarded arms' upstream
  chains. Estimated prize for per-class constant folding + DCE of
  speculated regions: ~3-10% of frame time. Real but second-order at
  current costs; the assume_eq/fold machinery is most of a static
  two-variant (dashing vs not) implementation when it climbs the
  priority list.

## The specialization thread's conclusion -> region skipping (2026-08-06)

Sizing chain: dynamic census says 73% of selects route / 770M vector-arm
lanes discarded; static classdead v2 (cell-forwarding + buttons) says
only ~1-3% is *statically* class- or button-determined, because the
partition cells are legitimately overwritten mid-frame under
position-derived conditions. Verdict: the prize is dynamic per-fragment
uniformity, and the mechanism is **region skipping**:

* New terminator (ConditionalSkip): if the mask is uniformly false in
  this state, jump past the region; otherwise fall through. Never
  splits, never predicts - the uniformity is observed per state, the
  fallback is today's exact behavior.
* guard_region recipe rule wraps the masked speculated regions (dash
  package first); its verifier must prove the region is effect-free
  under uniformly-false mask (every store mask-guarded, no other
  effects, region-defined values consumed only through mask-selects).
* Expected: converts the discarded-arm compute into skipped compute for
  uniform states; mixed states unchanged. Sized >=13% of vector work
  plus chains and dispatch.
* Philippe's per-button-combo program variants: statically small on its
  own (buttons fold little beyond routing), still interesting later for
  deleting the fan-out machinery itself.

Status: ConditionalSkip terminator LANDED (routing semantics + 3 glue
tests + all plumbing; commit "ir: ConditionalSkip terminator"). The
guard_region rule remains. Its concrete design:

* Entry: {rule: guard_region, fn, block, from, to, mask, prefix} -
  split the block's instructions at [from..=to] into head / region /
  join blocks; head ends with ConditionalSkip mask ? join : region;
  region ends br join; join holds the tail + original terminator.
* For each region-defined local used in the tail: it must be
  `select mask ? new : old` with `old` defined before the region; the
  join gets phi [head: old, region: select] and tail uses are rewritten
  to the phi. That is exactly the value the select yields when the mask
  is uniformly false, so skipping is semantics-preserving.
* Verifier: mask defined in the head; every region store's value is a
  mask-form select whose false arm is the pre-store cell value (load of
  the target inside the region before any store to it); no calls except
  pinned-readonly; the assert family inserted by the speculation rules
  is allowed (they cannot fire on lanes whose values are the old ones);
  every region-def escaping the region is mask-form as above.
* suggest side: scan fused blocks for maximal ranges whose escaping defs
  share a common mask. Apply to the dash-package regions first.
* Scoping scan (select count per mask in anonymous_61): no mask guards
  more than 4 selects - per-mask regions are small. But the 3-select
  family %5972..%6852 is the unrolled pixel-move iterations, and masks
  chain from root predicates (iteration mask = root && cond), so the
  high-value form is a guard on the ROOT mask over a super-region: root
  uniformly false implies every derived mask false, skipping the whole
  family. The verifier then needs implied-falseness along the mask
  def-chain (and/select-of-root forms, the decompose_truthy shapes),
  not just literal mask equality. Start with one pixel-move super-region
  as the pilot entry.

## Region skipping: first measurement (2026-08-06 afternoon)

The whole pipeline works end to end: 7 per-iteration guards on the
pixel-move continue-flags applied, rule-verified, and differentially
identical through 37 - the machinery is proven. But the measurement
parks this granularity: **+3.4%** at f42 (14.55 -> 15.06 s, all pairs).
Each guard adds per-fragment block-crossing overhead (phi flows,
dispatch, queueing x 254 fragments x 7 guards) that outweighs skipping
~15-instruction regions.

The fix is granularity, per the original super-region design: ONE guard
per axis on the axis root mask over all 8 iterations (~180
instructions + 8 tile_flag_at calls) - skips fire for every fragment
not moving on that axis (very common: grounded fragments for y, idle
for x). Needs one more implied_false form: `select c ? _ : d` with both
c and d implied-false of the guard (the continue-flag chain's shape:
flag_i = select stop_i ? false : flag_{i-1}). Entries then target the
whole [first-bound-check .. last-store] range per axis.

## Region skipping: closed at the pixel-move sites (2026-08-06)

The axis-level super-region (one guard, 8 iterations, calls included)
applies, rule-verifies, and is differentially identical through 37 -
the machinery is complete and proven. But the skip census settles it:
22% of skip evaluations fire carrying ~0.0M lanes, against 1.8M lanes
entering. Per-select routing uniformity is a property of the many tiny
fragments; the lane-heavy states are never uniformly idle, so region
skipping cannot reach the cost at these sites at any granularity
(per-iteration +3.4%, axis-level flat). No recipe entries remain; the
terminator + rule + verifier + census stay for sites with genuinely
state-uniform masks.

The day's deep lesson, three measurements in a row (specialization,
per-iteration guards, axis guards): the remaining frame-body cost is
carried by lane-heavy states that are *mixed* in every discrete
dimension the machinery can see - the redundancy that remains is
per-lane inside heavy states, already minimised at the op level, and
the levers that remain are the ones already banked (partitioning,
merge machinery) plus whatever reduces lane count itself.

## The 2022 reference (develop branch) benchmarked (2026-08-06)

Philippe's 2022 hand-written searcher (celeste-rust-old, branch develop:
hardcoded physics, exact states, guided brute force + fast forward pass
+ backward pass; historically finished 100m matching the public TAS and
finished 200m) re-run today on room (1,0):

* **24.0 s wall / 405 MB for 29 forward frames**, frontier ~964k TRUE
  states, 1.28 us per actual state-run, 79 ns per potential-run (their
  pruning skips 94% of potential runs).
* Its growth curve is the saturation story measured exactly: 7.0x ->
  1.105x by f29, monotone decline - the same shape our lane counts show
  (1.53x -> 1.35x at our f41-f47). The room genuinely saturates.
* Rough per-state comparison (frame alignment imperfect - our counts
  include ~11 pre-spawn frames): theirs 1.28 us/state-run vs ours
  ~4.7 us/lane at f43 - **~4x slower per lane for a general compiled-
  Lua interpreter vs hand-written physics**, which is closer than
  expected. The bigger gap is state count: they dedupe rem-EQUIVALENT
  states ('Don't run_player_update multiple times for equivalent rems'),
  not just rem-equal ones - and our distinct-modulo census measured
  exactly that slack at 1.8-1.9x on heavy states.
* **The lever this uncovers**: rem-equivalence dedup is result-
  preserving (their run matched the public TAS) - it merges states with
  provably identical futures, which is NOT the precision change that
  was ruled out. Porting their equivalence into our merge is likely a
  ~2x state cut. Their game.rs holds the definition.
* Open: the room's exact optimal frame count - the baseline TAS
  (tas/baseline/TAS2.tas) needs the 2022 input encoding (their tas.rs)
  to demarcate the first room; a naive decode through concrete_run gave
  a suspicious trajectory, so the bit mapping differs.

## The rem question, resolved to a decision point (2026-08-06)

Read the 2022 definition: ALL_DIFFERENT_REMS_FOR_MOVE is the four rem
corners (+-0.5 per axis). The 2022 searcher does not track rem: every
frame it runs the move with both extreme rems per axis and merges by
post-move outcome. Mathematically grounded (flr(rem+spd+0.5) over the
unit rem interval takes at most two values, realized at the extremes) -
but methodologically it is an **over-approximation**: rem is
existentially quantified per frame, independent of history. Optimality
still holds by the two-sided argument (over-approx bounds the horizon,
the concrete TAS witnesses achievability - and 2022's matched the
public TAS), but the normalized result set is a superset of exact
tracking.

Consequences:
* An exact in-model rem equivalence beyond bit-equality is essentially
  empty (distinct rems differ under some future spd sequence), so the
  1.8-1.9x modulo-rem census slack is only reachable via the 2022-style
  over-approx + concrete-witness methodology.
* If adopted, the natural implementation is a rewrite: replace the rem
  accumulate at frame start with a four-corner expand (the expand
  machinery exists), plus concrete replay validation of any final TAS -
  and verify would compare against a rem-projected observation.
* **Blocked on Philippe**: this is a normalized-result-set change, which
  he ruled out earlier in general; but it is also exactly the
  methodology his own 2022 solve used. His call, explicitly.

## Ranked next steps

1. ~~Fragment-parallel frame execution~~ DONE as state-parallel flow
   steps, opt-in for the plain program (-22.4% runner). The rewritten
   path regresses under it (+4-6%: its fused block is memory-bound per
   state), so its win must come from somewhere else - likely making
   per-state work narrower (dictionary/RLE), not more concurrent.
   Two lessons the hard way: std::thread::available_parallelism reads
   procfs per call (7.7M syscalls before caching); per-call scoped
   spawns at flow frequency cost minutes of system time (rayon pool).
2. ~~Virtual merge probe pass~~ DONE (partitioned, 512k threshold).
   Remaining inside virtual_merge at f40: vm_pack_verify 0.61 s (pack is
   still sequential), vm_probe 0.43 s, vm_hash 0.40 s.
3. **filter_branch 1.3 s**: the remaining cost is mask production and
   per-vector dispatch, not the gather. The structural fix is fewer forks
   (rewrite side) or RLE masks.
4. **Dictionary/RLE representation** (state-structure.md #1/#2): the
   biggest structural lever, unbuilt. All instruction time is redundant
   (census), but it is spread thin; only a representation change collects it.
5. Deep-frame headline run for the morning: bench f43/f44 with the final
   binary; runner -n 41.

## Operational notes

* A/B binaries in /tmp: ab_{base,new,vm,ud,rf,sort,par,un}_{runner,rewrite}.
  "un" = current HEAD.
* `merge_hint_normalize` span count == frames x 2 -> single-round fixed
  points; if that ever changes, revisit both the seen-set idea and the
  union_diff guard.
* The equivalence test pins virtual merge == materialized pipeline exactly;
  the materialized path is the fallback and must stay in sync.
