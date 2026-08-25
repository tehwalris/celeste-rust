# Autonomous overnight plan (Philippe asleep, 2026-08-25)

Philippe's instructions: delete the OLD kernels entirely. At RUNTIME the
search must use PURELY the new lattice kernels - nothing else. The old
interpreter stays ONLY as a reference/oracle for validating the kernels,
never in the runtime search path. Finish all in-flight work independently;
report in the morning. Benchmark ONLY on an idle machine (all-cores
workload; a contended run is meaningless).

## The sequence (sound ordering: origin's emitter change is inherited by the
## lattice generator, so origin FIRST, then latticeify)

1. **Origin metadata (subagent running).** Engine-carried per-lane origin
   through the kernel + dedup, so the backward sweep + fused pos-graph run
   on kernels (interpreter out of the backward path). On completion: MERGE
   into census, confirm gates.

2. **Latticeify everything + DELETE old kernels.** Launch after origin
   merges (both touch `trace/kernel.rs`; sequence for a clean merge).
   Scope:
   - Lattice-specialize all three widening variants (base Bits(0),
     rung-agnostic, exact k16) for rooms (0,0), (1,0), (2,0).
   - One combined `KERNELS` dispatch table across rooms+variants (build the
     multi-room merge in the generator; today it overwrites single-room).
   - DELETE the plain `crates/celeste-kernels/src/traced`, and the
     non-lattice `ladder`/`exact` sets - replace with lattice equivalents.
     Repoint `dispatch::TracedMode` at the lattice sets.
   - Runtime = purely lattice kernels; `CELESTE_KERNEL_STRICT` default-on
     intent (miss = fatal, interpreter never in the runtime path).
   - Per-room+per-variant staleness + differential gates green.
   On completion: MERGE into census.

3. **Clean-machine benchmark.** ONLY after all subagents are done and the
   machine is idle. Release build. Room (1,0) forward at the production
   horizon on the LATTICE kernels vs the written baseline (57.3 us/lane,
   340.91 s, 8.18 GB @ f94) - THIS is the number Philippe wants (the old
   room-1 benchmark was the plain traced set = the baseline itself, so it
   proved nothing about the lattice kernels' speed). Also room (2,0) once
   wired. Report timing + peak memory, clearly labelled release/idle.

## Notes
- Drive via subagent completion notifications; merge each as it lands.
- Do NOT benchmark while a subagent is running (contention).
- If a merge has a semantic conflict (e.g. origin rewiring references a set
  the lattice task deleted), reconcile then REGENERATE the lattice kernels
  with the merged (origin-aware) emitter so they inherit origin support.

## 4. Whole real runs for rooms (0,0), (1,0), (2,0) - the final deliverable

After steps 1-3, on an IDLE machine, run the FULL ladder campaign for each
of rooms 0, 1, 2 end to end. Requirements (Philippe, explicit):
- **Fully lattice FORWARD and BACKWARD** - the new kernels drive both
  directions; interpreter is reference-only, never in the runtime path.
- **Side-effect-derived position graph** - the FUSED `record_pos_graph`
  path (pos graph recorded as a byproduct of the forward pass), NOT the
  dedicated replay pass. This is the mode Philippe wants to be the only
  mode; the origin work (step 1) is what unblocks it on kernels.
- Collect ALL counts and timings: per-stage wall + peak RSS, us/lane,
  frontier funnel (raw -> self-dedup -> frontier-dedup -> new lanes),
  kernel coverage (missed/plain-routed must be 0 under strict), the win
  frame / optimum per room, and the row-key/g.bin identity vs the
  interpreter reference.
- Room (0,0) needs MEM=108G and few-frames-per-process (its level-0
  position-graph replay peaks ~101 GB) - see BENCHMARK_DATA.md; use
  ladder.sh's room-0 handling. Rooms 1,2 fit the 60G default.
- Write the numbers into BENCHMARK_DATA.md (keep it current) and report.
