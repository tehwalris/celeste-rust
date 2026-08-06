# Overnight ledger - 2026-08-06 (unsupervised session)

Philippe's directive at bedtime: finish the task list (deopt architecture,
task #78), then keep optimizing freely. Objectives: (a) get as far as
possible in frames within ~2 minutes of wall clock, (b) drive the
extrapolated time-to-final-frame (our frame 100 for room (1,0)) as low as
possible. Morning review decides what to keep.

Continues plans/overnight-2026-08-05.md (frontier-only search, widencheck,
the f59 premise-guard diagnosis, and the REJECTED death pruning - see the
correction there; the fix is the deopt architecture below).

## MORNING SUMMARY (read this first)

The night in one line: **the search now runs the entire room (1,0) -
through every death and respawn - to its abstract optimum in ~12
minutes / 33 GB, and that optimum is frame 90**, the rem-widened lower
bound on the concrete 100.

Landed (each its own commit, every gate green, all pushed):
1. **Deopt architecture (#78)** - canonical-state mapping from the
   recipe (promote_capture pairs), optimistic specialized execution,
   re-run under plain on premise failure. Certified by the new
   `rewrite deoptcheck` (forced round-trip of every state, green at
   34 and 37).
2. **Lane-granular deopt (#80)** - origin column + collect-mode
   asserts; only the lanes that actually violate a premise pay the
   plain price (f60: 1.77M plain lanes -> 525). Full-depth A/B:
   bit-identical reachable sets.
3. **Collect-first mode** - failing states pay one specialized run,
   not two: full room 898s -> 737s.
4. **128-bit visited keys** - the full-room frontier series is
   IDENTICAL to the 64-bit run (no collision ever fired); bound now
   rests on ~1e-23 instead of ~6e-4.
5. **Win probe** + the frame-90 result and its interpretation
   (NOT a bug - see the HEADLINE section; my earlier "anything != 100
   is a bug" claim was wrong and is corrected there).
6. **Per-coordinate saturation (#77)** - CELESTE_XY_DUMP + analysis:
   per-pixel arrival/taper is regular enough to forecast room cost
   from early frames.
7. **Exact-rem mode** (CELESTE_EXACT_REM) + feasibility measurement:
   13x lanes at f42 and still compounding - full-width exact forward
   is out; the tube refinement (#81) is the path to closing 90 -> 100.
   A run-until-the-cap exact run is charting the multiplier curve
   (/tmp/bench60_exact.log).

Objective scoreboard: 2-minute frontier f60 -> **f63**;
time-to-room-solved: unreachable yesterday (f58 wall) -> **737 s**.
2022-solver comparison: rough wall-clock parity on the forward pass,
computing the abstract bound while interpreting the original Lua.

Decisions needed from Philippe (task #81 and ledger sections):
* Direction for closing the 10-frame gap (backward/tube refinement vs
  propagated-rem middle ground; full exact forward is measured out).
* Whether 128-bit hashing suffices for the eventual proof or exact
  keys (dictionary-packed rows) are required.
* Whether collect-first + frontier-only should stop being opt-in.
* Room-2 policy eventually: room-parametric tile_flag_at, and what to
  do about `sin(frames/30)` in fruit rooms (the pin guard works; a
  policy is needed).

## 1. Deopt architecture (task #78) - LANDED

Philippe's design: canonical state = the plain program's cross-frame
representation (the heap contract `verify` observes). Every rewritten
program is a specialization with a mechanical mapping to/from canonical,
derived from the recipe. Today exactly one rule changes the cross-frame
representation - `promote_capture` - so the mapping is its 7
`(closure fn, capture 0)` pairs (the obj.* methods): `from_canonical`
unboxes those captures, `to_canonical` reboxes them (fresh write-once cell
per capture; behaviorally identical because the box is write-once/read-only
by the rule's precondition).

Pieces:

* `src/rewrite/state_mapping.rs` - `StateMapping::from_recipe`,
  `to_canonical` / `from_canonical`, both loud on unexpected
  representations (double-conversion is an error, never a silent skip).
  Unit tests: roundtrip preserves `observe_state`, each direction refuses
  a state already in its target representation.
* `AbstractRun::start_with_deopt(program, plain, mapping, force)` - holds
  the plain program's own `PreparedCfg` + `FixedEnv` (its fun defs
  differ). `step()` runs each boundary state optimistically under the
  specialized program; on failure (premise `assert_true` Err, or a panic -
  same class `screen_trial` unwinds across), the frame-input snapshot
  (cheap: heap storage is Arc-shared) is mapped to canonical, the frame
  re-runs under plain, outputs map back. Deopt events are counted and the
  first trigger per frame is printed. An error under plain too is
  terminal - plain is ground truth.
* `rewrite deoptcheck --frames N` - certification in the widencheck
  spirit: force deopt on EVERY state of EVERY frame (specialized ->
  to_canonical -> plain -> from_canonical) and compare observations
  frame-by-frame against a plain run. GREEN at 34: 235 forced round
  trips, all identical.
* `rewrite bench --deopt` - opt-in for real runs. Deopt stays OFF in all
  verify/screen paths so it can never mask a rewrite divergence.

Why respawn dedup works: `vectorize_states` gc's every state at the
boundary (canonical renumbering), and the timer globals are pinned, so a
respawned player is bit-identical to the original spawn lineage and the
frontier-only visited set absorbs it.

Gates: state_mapping unit tests green; deoptcheck 34 green; verify 34 +
full test suite + deep run results recorded below as they complete.

## Results (filled in as runs complete)

* deoptcheck 34: GREEN (235 forced deopts, identical every frame).
* verify 34: GREEN (12.1s). Full cargo test suite: GREEN.
* **Deep run PAST THE f58 WALL** (frontier-only + deopt, --frames 62):
  completed in 179.9s / 16.36 GB peak - note this beats the old
  f58-and-stop run (476s / 34.5GB, non-frontier) while going 4 frames
  further. Deopt events: f59 4 states/719k lanes, f60 9/1.77M,
  f61 16/1.93M, f62 21/2.18M (total 50 states / 6.6M lanes re-ran under
  plain). The trigger is exactly the diagnosed premise
  (assert_true %3695, #objects==1, kill frames empty the objects array).
  Frontier new-lanes growth bends from x1.3-1.4 (f36-42) to x1.05-1.10
  (f50-58); f62 frontier = 2.46M new lanes, visited total 27.4M rows.
* Deopt cost observation: a whole boundary state deopts if ANY of its
  lanes dies mid-frame, so by f62 over half the pre-subtract lanes ran
  under plain. The dead-countdown lineages (0 objects for 15 frames)
  also deopt every frame by construction. Two future levers, both in
  the specialization-framework spirit (NO death special-casing):
  (a) finer deopt granularity (catch the premise failure at the split
  state mid-frame, not the boundary state), (b) a second specialized
  program for the dead-room class, once program families exist.

## 2. Win probe + frame-100 attempt

`count_room_x_lanes` (inspect.rs): lanes with global room.x == 2 at a
boundary have exited room (1,0) - next_room() writes the index. bench
now prints per-frame elapsed/lanes/RSS and the first win frame. For the
earliest-arrival frontier search, first win frame = optimal TAS length
under the stated abstractions. Expected: frame 100 exactly (2022-proven
optimum, witness in tas/room_1_0_exit_frame_100.txt) - anything else is
a bug, so the deep run is an end-to-end differential against 2022.

* Frame-100 run (--frames 102, frontier + deopt): launched, results
  below when complete.
* Soundness note on the win probe: `tile_flag_at` is baked to room
  (1,0) (game_runner.rs:439-448), so frames AFTER a room exit simulate
  room (2,0) with stale collision data - those frames are untrusted
  and only exist because the run continues past the win. The win frame
  itself is sound: everything up to the exit uses room-1 data
  correctly, and the room-2 boundary state is produced by load_room's
  generic mget/fget reads, not the baked cache. Crossing rooms for
  real needs a room-parametric tile_flag_at.

## HEADLINE: the deep run reached the end of room (1,0)

The v1-deopt frontier run (--frames 102) ran 90 frames and found the
first room-exit lanes at **frame 90** - then crashed at f91, correctly
and by design (see below). Numbers: cumulative ~28 min wall clock
(heavily contended by concurrent builds/tests), 47.5 GB peak RSS,
visited set ~111M rows at f80, new-lanes peaking ~5.2M/frame around
f72-74 then declining (saturation + respawn dedup).

**Interpretation - read this before anything else.** The win probe
commit (661b0ec) claimed "anything other than frame 100 is a bug".
That framing was WRONG, and the run showed why: frame 90 is the
correct *abstract* answer, not a falsification of the 2022 proof.

* Our abstraction re-widens player.rem.x/y to [-0.5,0.5) at EVERY
  frame boundary - the abstract adversary gets free subpixels every
  frame, strictly more freedom than the concrete rem dynamics (which
  evolve deterministically from spawn). Upward progress can gain up to
  ~1px/frame from that freedom; ~10 frames over a ~66-control-frame
  climb is plausible slack.
* The 2022 solver's forward pass used 4 rem corner VALUES plus a
  domination/monotonicity argument (strategy.md section 1) - much
  tighter in the rem dimension - and proved the concrete optimum, 76
  control frames = our frame 100.
* So: abstract lower bound 90 <= concrete optimum 100. Consistent.
  The 10-frame gap is exactly the slack the strategy's backward
  refinement pass (plans/strategy.md) exists to close - e.g. re-run
  forward with rem tracked exactly (or 2022-style corners) only along
  the surviving tube of the widened search.
* Confidence in "no bug": timer pins + d_e_t clamp are certified
  quotients (widencheck); the jbuffer strip only REMOVES options;
  frontier hash collisions can only LOSE states (delay wins, not
  create them); and from f74 on, ~99% of each frame's lanes ran under
  the PLAIN ground-truth interpreter via v1 whole-state deopt, so the
  winning trajectory largely executed unrewritten. The remaining
  unverified surface is rewrite behavior beyond the f40 differential
  horizon on the ~1% of lanes that stayed specialized.

The f91 crash is the designed guard firing, not a failure: room (2,0)
has a fruit, fruit.update reads sin(frames/30), and `sin` was
deliberately left out of the fixed env when the timer globals were
pinned - a room where gameplay reads the timer crashes loudly instead
of silently using a pinned value. Room-2 frames are untrusted anyway
(tile_flag_at is baked to room (1,0)).

Saturation curve (full room, from the run's frontier lines): new
rows/frame peak at ~5.24M around f76, then decline to ~3.8M (f85) with
a small second wave to 4.7M at f90. Accumulated distinct rows by f90:
151.6M - about 16x the 2022 solver's ~9.45M by its win frame, which is
the priced-in genericness gap (freeze copies, p-bit trails, death
lineages, interval-rem multiplicity) now measured end-to-end.

New facts for the room-1 certificate (all modulo the experimental
64-bit-hash visited set):
* No input sequence exits room (1,0) before frame 90 even with free
  subpixels every frame (abstract bound).
* Deaths/respawns are fully handled: the search ran through 31 kill
  frames and respawn cohorts (first deaths f59, first respawns ~f74;
  new-lanes declining after f74 shows respawn dedup absorbing them).

## 3. Lane-granular deopt (deopt v2)

Measured need: with v1, a whole boundary state re-runs under plain if
ANY of its lanes dies mid-frame - f58 6.0s -> f59 13.9s -> f60 29.1s,
and by f72 the deopt drag put frames at ~100s. Millions of alive lanes
were paying the plain price for a few dying ones.

The generic fix (no death special-casing - works for ANY premise):

* On a failing boundary state, retry the specialized frame with a
  synthetic `__lane_origin` global: a per-lane Number column (raw-bits
  lane index) the program never reads. Every existing filter / expand /
  merge carries it automatically; its only semantic effect is
  preventing cross-origin dedup during the retry.
* Collect mode (`interpreter/deopt_collect.rs`, process-global sink):
  `assert_true` captures the origins of falsifying lanes instead of
  aborting - scalar-false and UnknownBool capture the whole fragment
  and drop it, a mixed vector captures the false lanes and continues
  the true ones (FILTER_DEOPT). Non-bool stays a hard error.
* The driver filters the retry's outputs to drop rows whose origin was
  captured, strips the origin column, and re-runs ONLY the captured
  lanes under plain via the canonical mapping. Lane-coverage
  accounting (every origin in outputs-or-captured, in range) guards
  against silently lost lanes; any surprise - non-premise retry error,
  panic, accounting mismatch - falls back to the sound v1 whole-state
  path with a printed reason.
* Also fixed in passing: the census FILTER_REASON arrays were [3] while
  REASON_NAMES had 4 entries - filter_visited would have indexed out of
  bounds under CELESTE_CENSUS=1. Now sized by REASON_COUNT (5, with
  filter_deopt).

### Collect-first refinement (env: CELESTE_DEOPT_COLLECT_FIRST)

Attempt-then-retry pays twice for failing states. Collect-first runs
EVERY frame in collect mode with the origin column injected up front:
a failing state pays one specialized run, a clean state pays only the
origin-column overhead. Measured (uncontended): +2.9% on clean frames
(f45: 10.37 -> 10.67s), -19 to -21% on kill frames (f60: 11.7 -> 9.2s,
f61: 11.35 -> 9.08s), identical outputs everywhere. Env-gated, default
off. The feared cross-origin mid-frame dedup loss did not materialize
(+2.9% total includes it).

Verification:
* New integration test `granular_deopt_reproduces_the_baseline`:
  corrupts the fused frame body with 3 synthetic premises (assert_true
  on comparison outputs - lane-mixed once the input fan-out starts) and
  runs 28 frames with deopt; observations must equal the unmodified
  rewritten program's trace every frame. PASSES (deopts fire from the
  prologue on, partial and capture-all paths both taken).
* A/B at the real kill frames: v2 reproduces v1's per-frame
  post-subtract lane counts and visited totals EXACTLY (f59
  1838536/20791318, f60 1997387/22788705 - bit-identical reachable
  sets), while the plain re-runs collapse from 719k lanes -> 40 (f59)
  and 1.77M -> 525 (f60). Frame times 13.9s -> 9.1s and 29.1s -> 13.6s.
  The dying-lane count really is 4 orders of magnitude below the
  whole-state count; the remaining overhead is the retry itself (the
  failing states run specialized twice). A later refinement could
  predict-and-skip the first attempt for states in classes that
  deopted last frame - optimization only, correctness is done.

## Ranked next steps (for morning review)

1. **Close the 10-frame gap (task #81)** - the search now computes the
   abstract bound (90); the prize is the concrete optimum (100) proved
   by OUR system. Options in the task; the strategy-doc answer is
   backward refinement over the widened tube with rem tracked tightly.
   Needs Philippe's direction.
2. **Proof-grade visited set** - the whole room-1 result is modulo
   64-bit row hashes. Exact keys (dictionary-encoded packed rows or
   128-bit hashes as a stopgap) turn the frontier search into
   something one can argue about. The Opus speed-set analysis (spd
   12 bits, flags 12 bits, pos sparse) is the design input.
3. **Deep-frame profile** - after the collect-first full-room run,
   profile f70+ to see what is left: specialized interpret vs merge vs
   visited-subtract. Decides whether the next lever is representation
   (dictionary/RLE) or merge mechanics.
4. **#47 dash package / btn expand** - fragments are 326 mean per
   frame at f45; expanding the button fan-out into lanes is the
   structural fix and plans/dash-package.jsonl already exists. Was
   parked pre-partitioning; the landscape changed.
5. **Room 2 readiness** (later): room-parametric tile_flag_at, a `sin`
   builtin decision for fruit rooms (the timer pins are unsound where
   gameplay reads frames - the guard works, now it needs a policy),
   program family for the multi-object room.

## Operational notes

* Runs tonight: /tmp/bench62_deopt.log (v1 first crossing),
  /tmp/bench102_deopt.log (v1 full, win at f90, crash f91 by design),
  /tmp/bench90_v2.log (v2 clean headline, 898s/32.5GB),
  /tmp/bench90_cf.log (collect-first full room).
* The win-probe crash ordering caveat: if a run past the win crashes
  (room-2 sin guard), the per-frame WIN lines are already printed -
  the result survives; only the end-of-run summary is lost.
* deoptcheck/verify/test-suite all green at every commit tonight;
  every change is one commit on `census`, pushed.

## 4. Visited-set hardening: 128-bit row keys

Two independently-seeded 64-bit row hashes (seed mixed into every
element hash) keyed as a pair. At 151.6M rows the single-64-bit
birthday risk was ~6e-4 per run; now negligible (~1e-23). f45 frontier
counts bit-identical to the 64-bit version (so no shallow collision
had occurred); cost ~2% of a deep run. The full-room run was repeated
under 128-bit keys to re-certify the frame-90 bound - result recorded
below. Exact-key encoding (dictionary-packed rows) remains the
morning-review item if "negligible hash risk" is not acceptable for
the eventual proof.

## 5. Dash package (#47): re-parked

Tried appending plans/dash-package.jsonl to the recipe under the new
stack (partitioned merges + frontier + collect-first): the entries no
longer apply - the recipe evolved past the 2026-08-03 derivation
('in_h061_in_i1_081_if_body_23' is not exactly accessor + bool +
store). Re-deriving is task-#45-scale work for what measured +17%
time / +30% memory before; the expand-copies-every-vector fundamental
is unchanged. Re-parked; revisit only if fragment-count overhead rises
in future profiles (f72 profile: 55% straight-line execution, 19%
merge, 6.5% deopt filtering, 12% tile_flag_at builtins, 3% gc).

### 128-bit re-certification result

Full-room run repeated under 128-bit keys: win at frame 90 again, and
the ENTIRE 90-frame frontier series (every per-frame new-lane count
and visited total) is identical to the 64-bit run - no 64-bit
collision ever fired in the room-1 search. 749.9s / 34.1 GB
(vs 737.5s / 32.7 GB at 64-bit: +1.7% time, +4% memory). The frame-90
bound now rests on a ~1e-23 hash risk rather than ~6e-4.

## 6. Per-coordinate saturation (task #77)

Data: CELESTE_XY_DUMP=path on bench dumps one CSV row per occupied
player pixel per frame, counting frontier lanes there (under
frontier-only these are NEW rows per (frame,x,y) - the arrival/taper
series). Regenerate with:
  CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1 \
  CELESTE_XY_DUMP=/tmp/xy90.csv ./safe-run.sh -- \
  ./target/release/rewrite bench --frames 90 --deopt
(170k rows; not committed. Fit script pattern in the ledger history.)

Findings (full room, 90 frames):
* 7,843 pixels ever occupied (~48% of the 128x128 room).
* Per-coordinate shape is regular: rise to peak in ~10-20 frames
  (early arrivals) shrinking to ~4 (late), taper HALF-LIFE only 2-10
  frames after peak - the bulk of a coordinate's new rows lands in a
  ~15-25 frame window after arrival - but with a long thin tail:
  coordinates arriving before ~f50 are still trickling new rows when
  the run ends (median active span = run end for all arrivals >= f50).
* correlation(arrival frame, active span) = -0.92, dominated by
  end-of-run truncation - i.e. almost nothing fully saturates before
  the room is solved. "Saturation" at the room level comes from the
  per-coordinate RATES tapering, not from coordinates dying.
* Late second wave: arrivals at f71-79 (~120-320 new pixels/frame,
  rise ~15) - the hard-to-reach top-of-room pixels, not respawns
  (respawns dedup instantly).
* Forecasting use (Philippe's original ask): a new coordinate's future
  contribution is predictable from arrival frame + first few counts
  via a shared taper kernel; room-total work ~ sum of per-coordinate
  kernels. For deeper rooms this gives an early-run projection of
  total cost long before saturation is visible in aggregate.

The XY-dump run also reproduced the frame-90 win and the exact final
frontier (4668057) with dump overhead of ~0% (738.6s).

## 7. Refinement pipeline build (daytime, with Philippe)

Direction set in the morning discussion: the strategy.md iterative
precision refinement, not brute force. Everything below is committed:

* Row table (dense ids + watermarks): R~(f) is a prefix of id space.
* Checkpoints: JSON meta + columnar binary + zstd + strict fingerprint
  (bench --checkpoint-dir/--resume; resume series bit-identical).
* Backward sweep (rewrite sweep): static row graph via origin-tagged
  replay of saved frame batches, g(row) by reverse BFS; B(f)/band from
  the (e, g) scalars; edge chunks + win seeds persist for incremental
  horizon extension. f45 smoke: zero replay divergence.
* Rem precision ladder (CELESTE_REM_BITS): Bits(0) = historic, k in
  1..15 = nested floor-aligned 2^-k buckets, 16 = exact. k=1 at f40:
  455,637 lanes, between level 0 (174,938) and exact (1,937,074).
* Band-restricted forward (bench --band-dir/--band-horizon/
  --band-prev-bits): coarsen each lane to the previous level,
  drop lanes with e > f or g > horizon - f (FILTER_BAND).

In flight: the full level-0 sweep over the f90 run (expect
min(e+g) = 90, cross-checking the forward win frame). Then: k=1 with
band at horizon 90 - expected to REFUTE horizon 90 immediately (no
win row by f90 at k=1), driving the first horizon bump toward the
concrete optimum 100. Witness extraction at the final level: walk
rows with decreasing g, recovering the input per step by trying the
32 inputs concretely - the reference TAS should fall out.

### Refinement: first results (afternoon)

* Level-0 sweep completed after the union_diff fix: the room-1
  transition graph at level 0 is 151.6M nodes / 7.196B edges (~47.5
  successors/row - vs <= 32 for a deterministic game; the excess is
  rem-interval flr splitting, ~1.5 abstract successors per (state,
  input), a per-level health metric that should shrink as k rises).
  Zero replay divergences across all edges. BFS from the single win
  seed: 316s. **min(e+g) = 90, exactly matching the forward first-win
  frame** - the backward pass independently cross-checks the forward
  result.
* 143.3M of 151.6M rows can eventually reach the exit, but the
  horizon-90 deadline collapses the band to HUNDREDS of rows per
  frame (f25: 4, f40: 280, f60: 16, f89: 160, f90: 1). The optimal
  tube is razor thin; the deadline does all the pruning.
* **k=1 refutes horizon 90**: the banded 1-bit pass runs the tube
  (1-40 lanes/frame, every coarse row found) and empties at frame 50 -
  at half-pixel rem precision no state can stay on a 90-frame winning
  path past f49. First refutation; total cost of the k=1 pass: 0.7s.
* Win-lane absorption added (bench + sweep): room-exit lanes are
  terminal; expanding them would simulate room (2,0) into the sin
  guard.
* refine.sh now climbs horizons (extend level-0 one frame via resume,
  chunk-incremental sweep, k=1 probe) until k=1 first wins at some
  H1 (90 < H1 <= 100); then k=2..16 repeat the pattern toward the
  concrete optimum, expected to land at exactly 100 with the
  reference TAS as a surviving path.
