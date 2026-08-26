# Morning report - overnight autonomous session (2026-08-26)

Philippe asked (before sleep): delete the old kernels; runtime = purely the new
lattice kernels; interpreter reference-only; finish all in-flight work; then
whole real runs for rooms 0/1/2 with lattice fwd+bwd + FUSED (side-effect)
pos-graph, all counts+timings; benchmark only on an idle machine.

## DONE (all merged into `census`, the live branch)

1. **Origin metadata on kernels** (665a2a4). Engine-carried per-lane origin
   (`Rt2::origin`) through kernel+split+merge+dedup. The backward sweep, the
   pos-graph replay, AND the fused recording now run on kernels with
   BYTE-IDENTICAL `g.bin`/`posgraph.bin` vs the interpreter. The subagent
   correctly REJECTED the "any survivor origin is fine" idea we floated - it is
   unsound (drops candidates); origin participates in every dedup key instead.

2. **Latticeify everything + delete old** (a876469/38c25d5/6732033). The
   constant-lattice fixpoint is the SOLE kernel generator. All 3 widening
   variants (base Bits0 / rung-agnostic / exact-k16) x rooms (0,0),(1,0),(2,0),
   one combined `SETS` dispatch table per rung, no shape-hash collision. Old
   plain/ladder/exact sets DELETED. `CELESTE_KERNEL_STRICT` default-ON (a miss
   is fatal; interpreter reachable only as reference). 289 tests green,
   warning-free, origin gates intact.

3. **Benchmark - lattice is ~1.9x FASTER** (idle, fat-LTO release, room (1,0)
   forward f94): **175.73 s / 29.5 us/lane** vs baseline plain-traced
   **340.91 s / 57.3 us/lane**, identical search (172,626,763 lanes, missed 0,
   win at f94). Lower bound, since the run also carries the new origin work.

4. **Whole room (1,0) campaign** (KERNELS=1 ladder.sh 94 94 2): fwd + bwd sweep
   + FUSED pos-graph, all lattice, missed 0. Finds the optimum correctly - level
   0 wins f89, k=1 wins f93, k=2 refutes H=94 - identical to the interpreter.
   Stage timings in BENCHMARK_DATA.

## GAPS / DECISIONS FOR YOU

- **Rooms (0,0) & (2,0) whole campaigns are BLOCKED.** No frozen rewritten
  artifacts (`rewrites-room00/room20.program.zst` don't exist) and the
  rule-application tooling (`bin/freeze`, the fold/dce/inline passes) was
  DELETED with the rewrite campaign. `bench` needs the frozen program; the raw
  compiled program's closure-boxing shape-misses the kernels. Their KERNELS are
  validated for 30 frames (in-suite gates `room00/room20_lattice_kernels_match_
  the_interpreter`), but the full in-search campaign needs a restored
  frozen-program path - your call how (I did NOT restore deleted program-
  transform tooling autonomously; wrong = unsound search).
- **Kernel crate size**: 1.34M lines / 62 MB (room 2 = 77%). Fat-LTO release
  LINK is pathological (>30 min). Proposal (subagent): split into one crate per
  room so a regen recompiles only its room. 17 rooms would be ~300 MB source.
- **Fused pos-graph is CORRECT but currently EXPENSIVE** (489 s fwd+fused vs
  175 s pure fwd; dedicated pass was ~137 s separate). Right architecture, perf
  TODO, not soundness.
- **Peak memory up** (~11-19 GB vs 8 GB baseline) - partly the origin vector;
  attribute + measure before pricing it to the lattice.
- Spd precision rungs still refused (unchanged).
