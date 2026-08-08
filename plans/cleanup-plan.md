# Codebase cleanup & simplification campaign (planned 2026-08-08)

Philippe's direction after the room-(0,0) campaign: near-term focus is
cleaning up and simplifying, not performance (the 100 m fresh-run cost
recovery - variants, tri-state, parallel replay - is deferred; see
BENCHMARK_DATA.md "Fresh-run cost" and plans/room00-plan.md "Queued
after convergence").

Standing gate for ALL cleanup work: the differential verifies on BOTH
rooms plus both witness TASes (tas/room_1_0_exit_frame_100.txt,
tas/room_0_0_exit_frame_94.txt via trace-witness), not just the (1,0)
suite. Rationale: the fake_wall incident - a verified-looking recipe
entry was unsound in code that room (1,0) never executes.

## Baseline (established 2026-08-08, logs in
## ~/celeste-checkpoints/cleanup-baseline-2026-08-08/)

- nextest: 469 passed, 1 skipped.
- `verify --frames 40`: ok both rooms (room1 63.2s, room00 42.1s).
- trace-witness room00 (h94, k0..16): PASSES every level every frame.
- trace-witness room1 (h100, k0..16): in-band at every level through
  f099; at f100 (the post-exit frame, player at (8,124) in room (2,0))
  only k16 matches, k0-k15 MISS. This is NOT a cleanup regression and
  NOT a soundness hole: the historical certificate (full pass incl.
  f100) was issued before the fruit-`off` interval widening changed how
  fruit-bearing states abstract at Bits(k) levels, and room (2,0) has a
  fruit on screen. The room1 tables are internally consistent with the
  code that built and certified them; current-code probes of those
  tables mismatch only on the one fruit-bearing (post-exit) frame, and
  only at widened levels. Phase B's re-derivation restores code/table
  consistency. Until then, gate = outputs identical to these logs.

## Phase A: hash-neutral passes (each independently gated)

1. Single source of truth for program assembly. concrete_run
   hand-assembled the game (own lua load WITHOUT apply_start_room -
   the franken-room bug - own frame chunk, own set_concrete_buttons;
   three copies of input plumbing exist). Every binary goes through
   Sources::load_from_disk / Program::compile and shared helpers.
2. Restructure verify.rs step() (god-function: variants + three deopt
   modes + chunking + streaming + phased boundary + band + subtract +
   gc, accreted under fire). Once streaming is parity-verified on
   (1,0), make it the only frontier path and express the boundary as
   explicit pipeline stages. Also factor bin/rewrite.rs's ~200-line
   command arms (shared level-loading/probe boilerplate).
3. Split inspect.rs: proof-critical abstraction code (the widenings,
   rem buckets, straddle splits, object_shape) into its own module
   with proof-machinery documentation standards; debug dumps and
   summaries separately.
4. Dead-weight audit: compare_frames (references a long-gone "old
   hardcoded Rust reference"), view_frames, slow_call_benchmark,
   vectorize_benchmark, and lib modules instr_time / would_dedup /
   input_capture / block_coverage / branch_sites / create_sites /
   liveness - verify usage, delete what is dead (house rule: no dead
   code). Also kill silent-skip tests (/tmp/test_merge_states.jsonl).
5. Env-var graduation where fingerprint-neutral: FRONTIER_ONLY,
   DEOPT_COLLECT_FIRST, STREAM_BOUNDARY are de-facto always-on in
   campaigns; make defaults match reality.

## Phase B: the one-time hash-breaking batch (sanctioned by Philippe:
## "let hashes change from under us once")

All in ONE change, then re-derive both rooms' checkpoint universes
(doubles as the regression run):
1. Native builtins from the start: min/max/abs/flr/sin (and audit what
   else builtin_level lua defines that pin_builtin entries re-pin).
   Deletes those recipe entries, shrinks builtin lua, and speeds the
   PLAIN program - narrowing exactly the plain/recipe gap that made
   (0,0) expensive (fruit states, deopt reruns, sweep replays).
2. Remove back-compat conditionality: sin registered unconditionally;
   start_room always in the fingerprint.
3. CampaignConfig struct as the single source for all search flags
   (currently ~8 CELESTE_* env vars that must agree across forward,
   sweep, and every analysis tool - fingerprint mismatches from
   missing env bit twice on 2026-08-07); the fingerprint is computed
   FROM the config.

## Execution ledger (overnight 2026-08-08)

Phase A - DONE, each pass gated green (tests + both verifies + both
witness traces byte-identical to the frozen baseline logs):

1. Dead weight (2 commits): compare_frames, view_frames,
   slow_call_benchmark, vectorize_benchmark, the input_capture module
   and its interpreter hooks, the silent-skip vectorize test, stale
   main.rs allows, dead tas.rs/input.rs, six unused deps (bv, regex,
   work-queue, crossbeam, image, petgraph). Audited and KEPT as live:
   liveness, branch_sites, create_sites, would_dedup, instr_time,
   merge_stats, block_coverage. ~2,100 lines gone.
2. A1: `concrete` lib module (buttons, initial state, frame stepping) +
   every binary through Sources/Program. concrete_run output
   byte-identical on both witness TASes; extract-tas still reproduces
   the room-(1,0) reference TAS byte-identically.
3. A3: proof-critical abstraction layer split into
   interpreter::abstraction with the over-approximation contract in its
   module doc; inspect.rs is now read-only debug tooling.
4. A2: step() decomposed into chunk_states / interpret_state /
   report_frame_events / finish_{streaming,phased}_boundary (+
   apply_band_filter, subtract_frontier, boundary_gc_if_enabled);
   bench artifacts byte-identical in both boundary modes. Level-loading
   and row-probe boilerplate factored (load_level / widened_row_key).
5. A5: CELESTE_STREAM_BOUNDARY deleted - every frontier run streams
   (set-equivalence re-verified: byte-identical artifacts);
   CELESTE_MAX_STATE_LANES defaults to the campaign's 1M (0 disables).

Phase B - code LANDED (commit "hash-breaking batch"), FORMAT_VERSION 3:
sin registered for every room (fixes the cross-room replay crash);
plain programs pinned at execution (Program::pin_native_builtins -
NOTE: the recipe must keep building on the unpinned base and keep its
own pin entries, because whole-program passes like cse-forward drift
every later id reference on a pinned base; this was tried and the
replay failed at m010); CampaignConfig + fingerprint-from-config with
every field hashed by value. Old universes archived at
~/celeste-checkpoints/pre-phaseB/. Re-derivation: single-horizon
ladders (the optima are known - 100 and 94), room (1,0) then room
(0,0), logs at ~/celeste-checkpoints/rederive-room*.log.

## Phase C: on the new baseline

1. Recipe minimization tool: drop entries whose removal changes
   nothing (re-verify after each); make the shape-agnostic vs
   shape-specific split explicit instead of a line number
   (rewrites-room00.jsonl is a manual prefix cut today).

## Explicitly deferred (performance, not cleanup)

- Tri-state per-lane comparisons + expand-only select (+ simdcheck
  gate) - specced in room00-plan.md with the lane-independence /
  batching-invariance contracts.
- S2/S3 variant recipes via the shape dispatch mechanism (#86).
- Parallel sweep replay.
- Worktree migration off /tmp/census to the main checkout (housekeeping,
  do at the next natural boundary).
