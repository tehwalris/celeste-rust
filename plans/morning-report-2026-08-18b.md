# Morning report — overnight 2026-08-17→18 (the kernel night)

Directive: transform the system into your architecture (recovered from the
compaction-lost messages), heavily optimize, microbench first, integrate,
retire old executors. Everything below is committed and pushed on `census`;
every claim has a gate.

## The short version

Your architecture is BUILT and CERTIFIED end to end:

- `(shape, rows) -> [(shape, rows)]` with exactly the two implementations
  you asked for on the steady class: the fully-compiled lane kernel in
  front, reference behind, deopt = per-lane bits routing input rows back.
- End-to-end engine (abstract-bench f35, gates 1+2 vs the interpreter,
  row-key set EQUAL): **380 ms vs 518 ms** for the previous best engine —
  1.36x faster overall while the kernel covers only the steady class
  (61% of lanes). f20/f25/f30 also certified (spawn shapes refuse + fall
  back cleanly).
- Kernel microbench (single-threaded): **14.8–30.2 ns per row-input-frame**
  (lower bound = LLVM DCE'd unused outputs, upper = all outputs forced).
  Your ~50 ns bar is beaten either way, before any asm-level tuning.

## What was built (in order, each certified before the next)

1. **(shape, pm1) dispatch key** (verify.rs Variant + `--variant-pm1` /
   `@PM1`), steady overlay `rewrites-trace10-steady.jsonl`: __frame with
   ZERO branches, hosted verify f40 identical, 43 GB peak.
   - Lesson: "literally one block" OOM'd the interpreter at 97 GB — kills
     sit at block ends, so the seams are the interpreter's liveness
     boundaries. 5 straight-line blocks is the certified interpreter form;
     the kernel doesn't care.
2. **K0 census** (`--row-census`): a steady row is 11 columns (~44 B);
   16 rows of full varying state = 11 zmm registers. Steady = 60.9% of
   f35 lanes; dash=1..4 = 23%, freeze = 16%.
3. **K1 emitter** (`transpile --kernel WITNESS OUT` + `--emit-shape`):
   emit-time abstract evaluation over a shape witness. The heap folds
   away (387 loads/129 stores -> registers), k_* button globals pin with
   bind-time guards, buttons bake into 64 monomorphized suffixes.
4. **K2 gate + hill-climb** (`--kernel-bench`): row-key set EXACT.
   - Straddle-deopt was wrong at scale (97% deopt: widened rem straddles
     on every fractional-spd lane) -> **<=2-way forks** (nested runtime
     loops, per-lane valid masks): ZERO deopt, still exact.
   - mget through the raw grid (-33%), then **button-taint hoisting**:
     only the button-dependent cone (166 of 808 lines) runs x64 (-88%).
5. **K3 integration** (`CELESTE_TILE=3` in frame_step): kernel-first,
   deopt rows requeued for the reference, generated acc_init/append_out
   builds ONE wide output block per chunk (the 12 s -> 380 ms lesson:
   the OUTPUT path dominated, not compute).
6. **K4 (partial)**: TILE=1 variant-tile executor DELETED (superseded);
   dead Rt3 variant machinery deleted. Suite 549/549 green throughout.

## Numbers to remember

| engine (f35, 187,859 lanes, one frame, all gates exact) | wall |
|---|---|
| Rt2 columnar | 2.0 s |
| Rt3 dynexp (yesterday's best) | 518 ms |
| **kernel mode (TILE=3)** | **369–380 ms** |

Kernel alone: 108–221 ms single-core for 61% of lanes x 64 inputs.

## What I did NOT do (and why)

- Per-class overlay kernels (dash/freeze classes, the other 39% of
  lanes): the mechanism is clear (same guard_branch machinery, opposite
  edges; multi-witness emitter runs; kernel registry in dispatch) — it
  needs the emitter parameterized by module name. Pilot possibly started
  after this report; see git log.
- Rt3 de-genericization + Rt2 retirement: gated on per-class kernels
  lifting coverage; Rt2 is still the in-probe fallback. The INTERPRETER
  is the system-level reference (your model) — Rt2 survives only as the
  probe's internal fallback until the class kernels land.
- Room (2,0): untouched tonight; the kernel-first engine is the right
  base for it and the shape witness/overlay flow is now proven.
- The two OOMs early in the night were my own doing (concurrent 100G
  scopes; stale perf data in tmpfs) — both process rules now written
  into kernel-plan.md and followed since.

## Decisions you may want to revisit

- Straddle forks enumerate <=2 fragments per split site (4 configs);
  >2 floors deopts (impossible at width<1, but guarded).
- Untainted scalar outputs are written Col::U with last-write-wins per
  chunk (gate-2-certified premise; a config-dependent uniform output
  would be caught by the oracle).
- The pm1 dispatch key matches by cell-name suffix with scalar equality;
  typo'd keys fail loudly via the never-dispatched check.
- steady-shape.json + kernel_gen.rs are gitignored generated artifacts;
  regen chain is documented in kernel-plan.md.
