# Overnight ledger - 2026-08-06 (unsupervised session)

Philippe's directive at bedtime: finish the task list (deopt architecture,
task #78), then keep optimizing freely. Objectives: (a) get as far as
possible in frames within ~2 minutes of wall clock, (b) drive the
extrapolated time-to-final-frame (our frame 100 for room (1,0)) as low as
possible. Morning review decides what to keep.

Continues plans/overnight-2026-08-05.md (frontier-only search, widencheck,
the f59 premise-guard diagnosis, and the REJECTED death pruning - see the
correction there; the fix is the deopt architecture below).

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
