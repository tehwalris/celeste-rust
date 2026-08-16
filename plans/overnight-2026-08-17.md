# Overnight 2026-08-17: the end-to-end perf push

Philippe's brief, ratified in conversation before he slept. This session
(post-compaction) works autonomously against these goals, in whatever
order measurement suggests, with judgment-based rather than per-change
gating. Read plans/native-probe.md for the compiled-engine state and
stage-2 checklist first.

## North star

A compiled per-shape engine running REAL end-to-end searches - Philippe
cares most about room (2,0)/300m, not re-running the two proven rooms -
at roofline, with every phase (forward, merge/dedup, backward sweep)
either at its roofline or carrying a precise measured gap-picture
(auto-generated vs hand-tuned vs theoretical).

## Goals

1. FINISH ZERO DIVERGENCE (the two cornered sites; ledger in
   native-probe.md round 3): opt-in cell-precise forward-cse (a store
   through a non-escaping alloc cell cannot alias; NEW FLAG so old
   entries replay byte-identically) -> the four iterator cells drop ->
   the already-proven speculate_region{mask:true} entry converts the
   freeze gate (7 shadows die with it) -> dash-trigger stage-D
   (speculate+absorb over the flattened dash body). Census: 0 real
   divergent.

2. ROW-STRUCT EMISSION: transpiler emits the per-shape row layout -
   fixed struct of the ~184 guarded slots + buttons, heap gone from the
   generated path. Oracle: still hex-exact vs concrete_run.

3. CRATE RESTRUCTURE FOR REAL USE: the generated code must be importable
   by the real runner, not just the probe. Likely shape: transpiler
   mechanism as a lib module; generated code as a build-script artifact
   or generated sub-crate the main crate links. Philippe explicitly
   wants the concrete-transpile mechanism USED by real end-to-end runs.

4. STAGE-2 ABSTRACT ENGINE + SIMD: abstract semantics natively compiled
   (intervals, the four choice-point kinds as tape forks, boundary
   widenings, row extraction - checklist in native-probe.md), then SIMD
   over lane blocks. Roofline-profile on the reserved core (15/31;
   background jobs tasksetted OFF it). Deliverable even where hand-tuned
   wins: the gap picture - roofline model per phase, auto number,
   hand-tuned number, which transformation closes which gap. Within
   ~1.2x of hand-tuned = done, move on.

5. THE 300M PROJECTION (do EARLY, it steers the night): measured
   bytes/row compiled vs interpreter, us/lane, projected onto room
   (2,0)'s hump (2.4M lanes, 87GB, ~1.7x/frame at S16-1px). Two attack
   channels are first-class co-goals with speed: ROW DENSITY (dense
   fixed rows may swallow the hump outright) and SPEED-ENABLES-STREAMING
   (recompute instead of store; frontier-chunked hump traversal).
   Written as a go/no-go vs rem-banding for the S-rung.

6. SHAPE REGISTRY + DISPATCH + DEOPT FALLBACK: per-shape programs with
   boundary dispatch and interpreter fallback on premise failure - what
   "integrate end-to-end on a real multi-shape room" requires. The
   ratified architecture (shape_key -> ordered transform lists; row
   layout mechanical at codegen; no runtime renumbering).

7. MERGE/DEDUP/BACKWARD AT ROOFLINE: once the forward engine is fast the
   bottleneck moves here. Dense fixed-width rows change the sport:
   radix/hash dedup, cache-sized tiling, algorithm choice before
   micro-opt. Transpile-style codegen allowed here too but likely
   unnecessary. Profile first, then pick algorithms, then tune.

## Ground rules for the night

- Careful but NOT mechanical: gates are MILESTONE-level, not per-change.
  The two non-negotiable oracles (the product's meaning lives here):
  (a) abstract-engine milestones get a row-set-equality check vs the
  interpreter on sampled frames; (b) SIMD gets a batching-invariance
  check. Everything else is judgment.
- Base-recipe and compile-recipe changes are IN SCOPE (batch them; the
  owed ladder re-derives happen once, later, when recipes settle - the
  room10/room00 ladder re-runs are explicitly deprioritized).
- The cart lua (celeste-minimal.lua) stays sacred. Harness shims are
  ours.
- Cores 15/31 stay reserved for benchmarking; background jobs run
  tasksetted 0-14,16-30. BENCHMARK_DATA.md stays current with every
  claim - numbers, not reasoning.
- Commit and push continuously. Big surprises (oracle mismatch, a
  premise pattern that breaks a whole approach) get written up in plans/
  and that line stops, rather than pushing through.
- Freeze-gate/dash and everything else here is fair game for this
  session - NOTHING is reserved for Philippe, including design
  decisions he might not agree with. His explicit instruction: make the
  call, keep moving; he may reverse a design in the morning and that is
  fine and expected. Blocking on his judgment is the wrong move;
  writing down WHY a call was made (so reversing it is cheap) is the
  right one.

## Useful state from tonight (2026-08-17 early)

- rewrites-compile.jsonl: heap 0 multi (184 single-receiver guarded),
  9 divergent = 2 real + 7 freeze-gate shadows. Verified to 40 frames,
  hex-exact 400 frames. All committed through 0cec8d6.
- New rules: drop_dead_cell (unit-tested), collapse_all_loop
  (peel-2-assert-3; discharges #113's missing rule; holds through the
  spawn transition).
- Fresh room (1,0) artifacts under the new fingerprint:
  ~/celeste-checkpoints/room10-newlua (runner ckpts) and
  room10-newlua-bench (bench f005..f037 states.bin layout; the probe's
  load_states_any reads both).
- Probe binary: native-probe/target/release/native-probe (NOT
  target/release). transpile --recipe <file> regenerates gen.rs.
- Room (2,0) S-rung campaign is PAUSED (1px and 2px both OOM the hump);
  goal 5's projection is the input to un-pausing it.
