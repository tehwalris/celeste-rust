# Kernel plan (overnight 2026-08-18, from Philippe's architecture messages)

Source: Philippe's messages 2026-08-17 22:13/22:21/22:27/22:35 CEST (recovered
from the transcript after a compaction dropped them) plus tonight's directive:
transform the system into this shape, heavily optimize, run autonomously
overnight.

## Target architecture (Philippe's, verbatim intent)

ONE interface: `(shape, rows) -> [(shape, rows)]` — a big set of states in,
the fully expanded set of states out. Exactly TWO implementations:

1. **Reference** = the normal vectorized interpreter running the rewritten
   program. Not a transpiled twin — the interpreter IS the reference.
2. **Kernel** = per SIMD-width slice of the rows, a tiny fully-compiled
   program: no traits, no tags, everything inlined, straight-line.
   Native lane types: Num (i32 16.16), Interval (2×i32 planes),
   Bool/UBool (masks). Deopt = per-lane booleans; deopted lanes' INPUT
   rows go to a deopt list handled by the interpreter. Output rows are
   dedup-ready (on-the-fly or big-list — not important initially).

Consequences Philippe drew explicitly:
- The scalar transpiled implementation and the transpiled reference (Rt2)
  are not needed in the end state. Retire the extra executors.
- Button fan-out: most of the code is SHARED across the 64 button inputs —
  shared prefix once per slice, per-variant suffix with baked button
  constants (the prefix/suffix split at the first button read).
- Sequencing: **inner SIMD kernel FIRST, standalone, on real rows from a
  real frame, hill-climbed in microbenchmarks until convincingly at the
  roofline** (his bar: ~50ns/frame-ish territory from instruction counts);
  only then integrate, keeping the speed; then delete the old executors.

## What tonight's rewrite work already settled

The kernel consumes the CERTIFIED steady overlay
(`rewrites-trace10-steady.jsonl`, dispatch key player@freeze:0,dash_time:0):
__frame = 0 branches, 0 phis, ~2,150 instrs, 5 straight-line blocks.
All deopt insertion (23 trace guards + 2 gate guards) lives in the rewrite
system — single source of truth, per Philippe's 22:35 message. The emitter
does NOT record traces or place guards; it compiles what the recipe built.
Remaining dynamic constructs and their kernel treatment:
- 6 __new_unknown_boolean + 8 expand sites = the button fan-out →
  disappear into the ×64 suffix variants with baked constants.
- 2 __split_by_flr + 1 __split_at → per-lane straddle check: straddling
  lanes set their deopt bit (22/frame at (1,0) = noise), uniform lanes
  continue with the resolved floor.
- assert_true guards → per-lane deopt bits under the running mask.
- heap ops (387 loads / 129 stores / 129 get_fields, all statically
  addressed in the steady shape) → row-struct fields via the shape facts.
- 14 allocs are proven region-private scratch → kernel locals.

Hardware: 7950X3D = Zen4, full AVX-512. Width 16 × i32 = one zmm per
Num column; Interval = two zmms (lo/hi planes); masks in k-regs/i32.

## Phases

- **K0 recon**: shape-fact dump (cell → row slot, types, array lengths)
  against the steady overlay; extract real steady-class rows from a
  room (1,0) checkpoint frame (f35-ish); baseline numbers from Rt3.
- **K1 emitter v1**: transpile gains a kernel emission mode: slot-typed,
  straight-line Rust over [i32; 16] lane arrays (LLVM + target-cpu=native
  autovectorizes elementwise code to zmm; verify with disasm, drop to
  intrinsics only where LLVM fails). Prefix/suffix split at first button
  read, 64 suffixes, per-lane deopt mask accumulated.
- **K2 microbench + hill-climb**: correctness gate first — per (row,btn)
  cell-value equality vs the interpreter (and Rt2 while it lives).
  Then perf: ns/(row·btn), instr counts, IPC, port pressure vs roofline;
  iterate (dead columns, shared subexpressions across suffixes, load/store
  minimization, interleaving). Convince before integrating.
- **K3 integrate**: implement `(shape, rows) -> [(shape, rows)]` with the
  kernel front and interpreter deopt path; gate = observation-identical
  boundary states on room (1,0) through f40; measure retained speed
  end-to-end.
- **K4 retire**: delete Rt2/Rt3/scalar-oracle paths that the new engine
  obsoletes (keep whatever the certification gates still need, delete the
  rest — no dead code).
- **K5 if time**: start the room (2,0) campaign on the new engine
  (spd rung, #110/#139); shape-specific overlays where needed.

Ground rules for the night: safe-run for anything heavy, ONE heavy job at
a time, --memory 60G tripwire on certification runs, perf data in
~/perf-scratch/ (never /tmp), measure before/after, commit+push at every
stable point, keep this file and BENCHMARK_DATA.md current.

## K0 results (2026-08-18 night)

Baseline re-measured (f35 bench, 187,859 lanes in, 40 blocks):
- Rt3 TILE=2 (current best): **518 ms, 2757 ns/input-lane**, exact
  (gate 2 row-key set equal), 0 splits. The 1.55 s in
  columnar-engine.md is stale; yesterday's stage-2 work improved it 3x.
  Per (row x button-variant): ~43 ns. This is the number to beat.

Row census at f35 (new `native-probe --row-census DIR FRAME`):
- pm1 classes: steady (freeze=0, dash=0) = **60.9%** of lanes;
  dash=1..4 = 23.4%; freeze=1..2 (all dash=4) = 15.7%. The kernel's
  steady overlay covers 61% today; dash/freeze classes are future
  per-class overlays (same guard_branch machinery, different edges).
- The ENTIRE per-lane state of a steady row is **11 columns**:
  x, y, spd.x, spd.y, grace, dash_effect_time, dash_accel.x/.y,
  dash_target.x/.y (all Num) + flip.x (Bool). ~44 B/row.
  16 rows of full varying state = 11 zmm registers.
- Uniform-but-abstract cells: __button_states.1-6 = UBool (the fan-out
  source), objects.1.rem.x/.y = uniform Interval (the widened rem).
  Everything else is block-uniform plain values.

Kernel consequences: register-resident frame is feasible; value
classes at emit time are S (block-uniform scalar, computed once) vs
Z (per-lane: ZNum=[i32;W], ZIval=2x[i32;W], masks); buttons become
per-suffix compile-time constants (64 monomorphized suffixes, LLVM
folds each variant's dead selects); per-lane Ival appears mid-frame
(num + uniform rem interval), flr straddle -> per-lane deopt bit;
tri-state compares carry (value, known) masks; select on unknown
cond -> deopt bit (v1).
