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
* Rough per-state comparison (frame alignment now pinned - our frames
  1-24 are the spawn prologue, first player update at frame 25, so
  their control frame k = our frame 24+k): theirs 1.28 us/state-run vs ours
  ~4.7 us/lane at f43 - **~4x slower per lane for a general compiled-
  Lua interpreter vs hand-written physics**, which is closer than
  expected. The bigger gap is state count - but the first attribution
  of that gap to rem was WRONG; see "The rem question, corrected" below.
  (RETRACTED: "our distinct-modulo census measured that slack at
  1.8-1.9x" - the excluded columns in that census cannot have been the
  player's rem, which is widened-uniform at the frontier. The 4 excluded
  non-uniform columns were cell158/cell163 x/y pairs whose identity was
  never verified - plausibly spd and/or position, which would make the
  1.8-1.9x number meaningless. Census must be redone with verified cell
  identities before any state-count conclusion.)
* ~~Open: the room's exact optimal frame count~~ RESOLVED (2026-08-06),
  see "Room (1,0) optimal pinned" below.

## The rem question, CORRECTED (2026-08-06 morning)

Philippe asked the right question ("what does our current code widen and
when?") and the answer overturns the previous section's framing.

**What the current code actually does** (verified in source, not
reconstructed):
* `make_state_abstract` (inspect.rs:487) widens exactly two heap cells -
  `player.rem.x` and `player.rem.y`, found by walking `objects[]` for
  `type == "player"` - to the full interval [-0.5, 0.5-eps], with a
  containment assert. Nothing else is ever widened. `player_spawn`'s
  rem is NOT widened (tracked exactly during spawn frames).
* It runs at EVERY frame boundary, after the frame's flow and BEFORE
  the boundary merge - main.rs:363 and verify.rs:306 (bench/verify use
  the same order). So the merge sees uniform rem columns, and lanes
  differing only in rem already dedup at frame end.
* Mid-frame, rem lives as an interval the whole time: obj.move shifts
  it by spd+0.5, `__split_by_flr` fans lanes into floor classes (<=2
  per axis), flr extracts a concrete amount, rem becomes the refined
  subinterval; collision writes concrete 0. The fan-out persists to
  frame end, where widening + merge collapse reconverged lanes.
* No widening exists in the recipe: zero `widen_rem`/`widen_buttons`
  entries; the `__widen_rem` builtin is unused machinery from the
  parked task #51 experiments.

**Consequences (correcting the previous section):**
* Our system ALREADY existentially quantifies player rem per frame -
  the same abstraction as 2022's four-corner treatment, in a different
  mechanization (full interval + floor-split vs corner enumeration).
  Both discard all rem information at every frame boundary. There is
  NO normalized-result-set decision on the table; nothing is blocked
  on Philippe. The previous section's "over-approximation vs exact
  tracking" contrast was a false dichotomy - we never tracked rem
  across frames.
* The real remaining differences vs 2022 are (a) collapse TIMING: they
  group by post-move outcome immediately after move, we carry the flr
  fan-out through the rest of the frame to the boundary merge; and
  (b) per-lane interpreter speed (~4x).
* Lever (a) is result-preserving under current semantics: player.rem is
  dead from the end of the player's move() until the boundary widening,
  so widening it early (the existing `widen_rem` rule) plus an early
  hint merge collapses the fan-out where 2022 does, without changing
  frame-boundary results. This is exactly parked task #51 (+110% ->
  +33% when merges were expensive) - worth re-testing now that merges
  are partitioned and much cheaper.

## Room (1,0) optimal pinned: exit during frame 100 (2026-08-06)

TAS2.tas decoded and replayed. Facts, all verified on our engine:

* The input bit encoding is IDENTICAL between the repos (bit0..5 =
  left,right,up,down,jump,dash) - the earlier "suspicious trajectory"
  was pure frame misalignment, not bit mapping.
* TAS2.tas is 77 control-frame inputs for the first room only. The 2022
  model has NO spawn phase: its control frame k = our frame 24+k (our
  frames 1-24 are the spawn prologue; the player object is created at
  the end of frame 24; the first player update is frame 25 - the
  abstract search uses the same numbering, main.rs test comment
  confirms "player spawn at frame 25").
* **Semantic difference found: celeste-minimal has NO jump buffer.**
  Classic (and the 2022 hand model, game.rs jbuffer) buffers a jump
  press for 4 frames; celeste-minimal consumes `btn(k_jump) press` the
  same frame or loses it (presses during dash_time>0 frames, or
  airborne before a wall comes in range, do nothing). This does not
  change reachability: a buffered jump executes identically to a press
  at the execution frame, so the two games have the same optimal frame
  counts; only input sequences need re-timing.
* Adapting TAS2 by moving four J presses to their execution frames
  (control indices 5->7, 47->49, 53->54, 62->63) reproduces the 2022
  trajectory FRAME-FOR-FRAME in integer position, and the room
  transition to (2,0) fires during our frame 100 - the 76th player
  update, exactly the 2022 win frame (their 0-based control frame 75).
  Witness checked in: tas/room_1_0_exit_frame_100.txt.
* **Consequence for the search**: the optimal exit is during frame 100
  in our numbering (2022's search proved 76 control frames optimal in
  a model whose trajectories are a superset of ours; the witness shows
  ours achieves it). The abstract forward search certifies this by
  reaching frame 100 and finding the exit reachable, with frame <= 99
  showing none. Current frontier f47 = control frame 22 of 76.
* Incidental: concrete_run cannot run past the room transition - room
  (2,0)'s fruit.update calls `sin`, which the fixed env lacks.

**Retested (2026-08-06, task #74): parked at +40%.** The exact old
stack (widen_buttons + widen_rem at in_h061_if_join_103, add_hint at
in_h061_and_or_join_219) on top of the current recipe: differentially
identical through 34; interleaved A/B at f42 gives 14.75 -> 20.57 s
(**+40%**), 2.74 -> 2.36 GB (**-15%**), identical frontier (2181716
lanes, confirming result-preservation). Fragments crush 10674 -> 3934
total (max 892 -> 332), but even partition-routed virtual merges cost
more mid-frame than the duplicate lanes they remove. Third consistent
verdict for this shape (+30%, +33%, +40%): a full-population mid-frame
merge does not pay on this program at these depths. The -15% memory is
a real dial if depth becomes memory-bound - the recipe stack is three
lines, kept in this note, not in the recipe.
* The 1.8-1.9x distinct-modulo number is retracted pending a redone
  census with verified cell identities (see above).

**Census redone with named cells (2026-08-06, f42):** cell_names now
follows pointers and array elements, so player fields resolve as
objects.1.rem.x etc. Findings:
* objects.1.rem.{x,y} NEVER appear among non-uniform frontier columns -
  player rem is widened-uniform at every frontier state. Zero rem slack;
  the boundary widening + merge already collapse it. Confirmed
  empirically, not just from source.
* Yesterday's excluded cell158/cell163 were spd plus another x/y table
  (dash_target or dash_accel). Excluding only spd.{x,y} today gives
  1.1-1.6x on most states but 10-13x on dash-cluster states and 109x on
  one 122k-lane state - which is CARDINALITY STRUCTURE, not slack:
  distinct spd values have distinct futures and cannot be deduped.
  The distinct-modulo census only finds exploitable slack for cells that
  are provably future-irrelevant (as rem is, via widening); no such cell
  remains at the frontier.
* Every frontier state shows distinct-full == lanes: no exact duplicate
  rows survive the merge. The merge is airtight.
* Consequence: the frontier state-count gap vs 2022 (if it survives a
  fair frame-aligned comparison at all) is NOT recoverable by smarter
  boundary dedup on the current abstraction. Remaining levers: per-lane
  speed, and intra-frame collapse timing (task #74).

## Evening 2026-08-06: frontier-only search, the speed set, and the f59 gate

* **Frontier-only expansion landed** (Philippe's insight; env-gated,
  CELESTE_FRONTIER_ONLY): expand only never-before-seen rows. Enabled by
  pinning the gameplay-dead timer globals (frames/seconds/minutes/
  deaths) - the frames counter had made all cross-frame rows distinct.
  f42: 14.65s/2.74GB/2.18M lanes -> 9.99s/1.41GB/944k new (with the
  freeze partition key). Hash-only visited set; proof-grade needs exact
  rows (or the dense encoding below).
* **dash_effect_time clamp**: decrements forever, only read is `> 0` -
  clamped at 0 (no-op in every room). Dedup fraction at f38 jumped from
  ~15% to ~45% of pre-subtract lanes.
* **widencheck landed** (Philippe's certification): search with rem-only
  widening + post-hoc conservative widenings == widen-every-boundary
  run, frame by frame. PASSES through f38; the pins/clamp are certified
  pure quotients. p_jump/p_dash widening REJECTED (asymmetric
  over-approximation; see the make_state_abstract note).
* **Speed set (Opus investigation)**: reachable spd.x = 65 values,
  spd.y = 106, joint 3684 pairs (12 bits); flags 3696 (12 bits);
  full state key ~39 bits in a u64 with position as the sparse outer
  index. Ice (0.05 accel) and springs (x0.2) destroy the lattice -
  absent in room (1,0). Dense-encoding design is the open lever on the
  ~9x per-state cost gap vs 2022.
* **State-count gap vs 2022 explained** (control-frame-1 comparison, 24
  vs 7 rows): freeze-intermediate copies (~x3 on dash states; 2022
  schedules dash successors 2 frames ahead), dominated input variants
  (~x2; 2022 prunes jump+dash-type combos), death lineages (2022 prunes
  deaths), compounding until reconvergence. Same fields, same physics -
  the gap is counting semantics, priced in by our genericness choices.
* **f59 landmine**: first kill frame. kill_player empties objects
  mid-update; the collapsed loops' #objects == 1 premise assert fires
  (loudly, as designed) - recipe-only unsoundness, plain program fine.
  Boundary death pruning (built, env-gated) cannot catch it (mid-frame).
  Fix: __prune_state builtin at kill_player's head via recipe entry =
  death pruning at the kill site. BLOCKED on Philippe's approval of
  death pruning as default search semantics; until then every run
  > f58 stops at the guard. Probe confirms zero boundary death states
  through f58 (frontier there: 18.95M lanes, 476s/34.5GB non-frontier).
* 2022 comparison, properly aligned: at their control 29 (our f53):
  24s/405MB theirs vs ~200s/18GB ours (frontier-only) - ~8-9x time,
  ~45x memory. Full-room estimate: theirs ~5-15 min, ours ~3-5 h
  projected IF the new-lane curve bends like theirs (their accumulated
  set reaches ~9.45M by the win frame; ours ~75x that ratio).

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
