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

## K2 status (2026-08-18 ~02:30)

GATE PASSES: `--kernel-bench` on f35 steady class (114,458 lanes,
10 blocks): kernel + deopt-to-reference row-key set EQUAL to the
certified frame_step pipeline (197,316 keys, 0 missing, 0 extra).
Speed of the raw v1 emission: 360.5 ms for 114,458 lanes x 64 inputs
single-threaded = 49.2 ns per row-input-frame - AT the ~50ns bar
before any hill-climbing.

BUT the deopt rate is 97% (7.13M of 7.33M lane-variants): the
"straddle -> deopt" v1 decision is wrong at scale. rem is
boundary-widened to a width-1 interval, so flr(rem+spd+0.5) straddles
for EVERY fractional spd lane (all falling lanes). The 49.2ns number
is therefore mostly garbage-computation + the reference doing the
real work; not yet meaningful.

NEXT (the fork restructure, matching Philippe's "output = a handful
of SIMD lanes depending on forks"):
- A width-<1 interval spans AT MOST 2 floors, so each __split_by_flr
  is a <=2-way fork; __split_at on per-lane Nums is identity (spd.x
  is Num), so exactly 2 fork sites exist, both in the PREFIX.
- Emitter restructure: cut segments at fork sites like at the button
  expand: P0 | fork1 | P1 | fork2 | P2 + 64 suffixes. Forks are
  RUNTIME loops (for c in 0..2), not monomorphized: fragment c of a
  lane = intersect with its c-th floor bucket; lanes whose fragment c
  is empty drop out via a per-lane VALID mask (not deopt) threaded to
  the output. Segments become fns; crossing-value structs per cut
  (the existing Pre machinery generalized to a chain).
- Then re-measure: expected well under 49.2ns/row-btn of real work
  once 97% of lanes stop being garbage; then disasm + hill-climb.
- Deopt then remains only for: genuine >2-floor straddles (cannot
  happen at width<1), split_at on true intervals, unknown-cond
  selects, guard failures - the rare tail.

Bench harness detail worth keeping: deopted input rows are re-run
through frame_step and their keys unioned - that IS the K3
architecture (kernel front, reference behind), and the gate proves
the composition exact.

## K2 FORKS LAND (2026-08-18 ~02:50): kernel EXACT with ZERO deopt

zi_fork_flr (<=2-way fork per split site, runtime nested loops in the
emitted frame(), per-lane VALID masks instead of deopt) lands. Gate on
f35 steady class: row-key set EQUAL (197,316 keys), 0 missing, 0
extra, **0 deopt lane-variants**, 0 bd slices - the kernel computes
100% of the steady class itself, exactly, with the reference touching
nothing.

Timing (single-threaded, unoptimized): 1347 ms for 114,458 lanes x 64
inputs = 183.9 ns per row-input-frame, including the 4 fork configs.
For scale: Rt3's 518 ms was on ~30 cores (~7.7 s single-core-equiv),
so the kernel is roughly ~6x faster per core already, before any
hill-climbing (no disasm pass yet, no dead-config pruning, no
parallelism in the bench).

Hill-climb list (next): disasm the suffix (verify zmm vectorization),
skip suffixes for all-invalid configs (integer-spd lanes have empty
fragment 1), parallelize slices across cores in the bench, cross-suffix
sharing of button-independent Z ops, then K3 integration.

## Continuation state (context boundary ~03:00)

Where things stand exactly:
- Certified + committed: steady overlay (b4d1060), K0 census (ad2847e),
  K1 emitter (977e209), K2 gate (c12ab4c), K2 forks (9b6ffa4). All
  pushed to census. Working tree may carry only this note.
- Regen chain (after any emitter/recipe change):
  1. `./native-probe/target/release/native-probe --emit-shape
     ~/celeste-checkpoints/room10-newlua-bench 35 native-probe/steady-shape.json`
  2. `./target/release/transpile --recipe rewrites-trace10-steady.jsonl
     --kernel native-probe/steady-shape.json native-probe/src/kernel_gen.rs`
  3. `cd native-probe && cargo build --release` (~4 min: 64 suffixes)
  4. `./target/release/native-probe --kernel-bench
     ~/celeste-checkpoints/room10-newlua-bench 35 --reps 5`
- Current numbers: gate EXACT/zero-deopt; 1347 ms single-thread
  = 183.9 ns/row-input-frame (4 fork configs x 64 suffixes).

NEXT ACTIONS in order:
1. Hill-climb (K2): profile ONLY the timing loop (add a
   CELESTE_KERNEL_GATE=0 skip or perf --delay; perf data in
   ~/perf-scratch/). Expected wins: suffix work for all-invalid
   configs already skipped - check config-validity distribution;
   hoist cache.solid_map lookups (24 tile_flag_at sites, uniform w/h);
   cross-suffix sharing of button-independent Z ops; check ymm vs zmm
   (znver4 prefer-width) via -C prefer-vector-width=512 experiment.
2. K3: (shape, rows) -> [(shape, rows)] engine: kernel front on steady
   class, everything else + deopt rows -> frame_step; wire into the
   abstract-bench as a third engine mode; gate 2 + timing.
3. K4: retire Rt2-as-reference ambitions per Philippe (interpreter is
   THE reference at system level; native-side frame_step stays only as
   long as the native bench needs it), delete Rt3 TILE=1 variant path
   if TILE=2 + kernel supersede it. No dead code.
4. K5 if time: room (2,0) campaign on the new engine.
Suite + verify gates before any push that touches the main crate.

## K2 hill-climb (2026-08-18 ~03:20)

| step | best of 5 | ns/row-input |
|---|---|---|
| forks land (exact, 0 deopt)     | 1347 ms | 183.9 |
| fast mget (raw grid indexing)   |  898 ms | 122.6 |
| BUTTON-TAINT HOIST              |  108 ms |  14.8 |

The taint hoist is the Philippe-named lever ("most of the code is
shared across button inputs - merge across those"): emit-time taint
analysis routes every instruction by button-dependence; the x64
suffix shrank 642 -> 166 lines (74% of per-variant work now runs once
per fork config). Gate stays row-key EXACT with ZERO deopt.

14.8 ns/row-input single-threaded is 3.4x under the ~50ns bar; the
single-core kernel outruns Rt3 on 30 cores (~2.9x wall, ~85x
per-core). Remaining: re-profile (prefix likely dominates now),
then K3 integration.

Measurement honesty note (~03:40): the 108 ms sink read only 2 fields,
so LLVM DCE'd most output computation - partially fictional. With
black_box forcing ALL outputs materialized: 221 ms = 30.2 ns/row-input
(still 1.7x under the bar, single-core). KOut is now split
KOutShared (per config) / KOut (per variant, tainted cells only), so
the per-variant copy is minimal. True engine cost lands between 14.8
and 30.2 depending on K3's output-buffer design - measure there, not
here. K2 microbench: DONE enough to integrate; further squeezing
(prefix share, zmm width experiment) is follow-up, not blocker.

## K3 LANDS (2026-08-18 ~04:30): kernel integrated, certified, FASTER

CELESTE_TILE=3 in frame_step: kernel-first on bindable chunks; deopted
rows re-queue (kernel_ok=false) for the reference paths; bd/off-shape
chunks fall through whole. Output path: generated acc_init/append_out -
ONE wide accumulator block per chunk with typed per-lane appends
(untainted scalar outputs stay uniform - the timer-pin panic taught
that), boundary once per chunk.

abstract-bench f35, all 40 blocks, gate 1 + gate 2 (f37 chase) EXACT:
| engine | wall (5-rep min) |
|---|---|
| Rt2 columnar          | 2.0 s   |
| Rt3 TILE=2 (prev best)| 518 ms  |
| TILE=3 KERNEL         | 380 ms  |

1.36x over the previous best END TO END while the kernel covers only
the steady class (61% of lanes; the dash/freeze classes still ride
the old paths inside those 380 ms). Journey: naive per-slice blocks
12.1 s -> accumulated 8.3 s -> typed appends 380 ms - the lesson is
the OUTPUT path, not the compute, dominated integration.

Remaining after this: per-class overlay kernels (dash=1..4, freeze)
to lift coverage past 61%, parallel scaling check, K4 retirement of
obsoleted executors, then the (2,0) campaign.

## Per-class overlays COMPLETE (2026-08-18 ~05:40): 100% class coverage

Every pm1 class of the player shape now has a CERTIFIED branch-free
overlay (hosted verify vs rewrites-compile, identical through f40,
~115s each):

| overlay | classes | __frame | certified at |
|---|---|---|---|
| rewrites-trace10-steady.jsonl | freeze:0,dash:0 (61%) | 5 blocks, ~2150 instrs | dash_time:0 |
| rewrites-trace10-dash.jsonl   | dash_time 1..4 (23%)  | 5 blocks, 1804 instrs  | 1, 2, 3, 4 |
| rewrites-trace10-frozen.jsonl | freeze 1..2 (16%)     | 3 blocks, **236 instrs** | freeze:1, freeze:2 |

Each overlay was FOUR recipe lines on top of the shared trace10 prefix
(guard_branch entries with the class's pinned edges + dce + kill_dead)
- the trace-straightening machinery amortized exactly as intended.

NEXT SESSION: the kernel registry - parameterize the emitter by module
name (kernel_gen_steady / _dash / _frozen), emit witnesses per class
(--emit-shape needs a class filter argument), try each kernel in
run_chunk_kernel by bind order, then re-bench TILE=3 (expect well
under 300 ms with 100% kernel coverage; the frozen classes' 236-instr
frame is nearly free). Then Rt3/Rt2 retirement becomes real (K4), and
the room (2,0) campaign (K5) runs on a fully-kernelized room (1,0)
precedent.

## K3b: kernel registry LANDS (2026-08-18 ~06:45)

Three class kernels (kernel_gen_steady/_dash/_frozen) emitted from the
certified overlays + class-filtered witnesses (--emit-shape DIR FRAME
OUT CLASS); run_chunk_kernel tries them in coverage order (a wrong
class fails its own gb guard -> bd on slice 0 -> next kernel).

The taint analysis DISCOVERED game semantics: the dash and frozen
frames have EMPTY button suffixes (mid-dash Celeste ignores input;
frozen skips the update entirely) - their 64 variants are identical,
emitted as one call.

Coverage counters (f35 bench, steady rep): steady 114,458 + dash
43,824 + frozen 29,577 lanes, missed 0 - **100% of player lanes run
kernel code**, gates 1+2 exact.

Wall: 393 ms (vs 369 steady-only, 518 TILE=2). The flat wall despite
full coverage is the finding: FRAME COMPUTE IS NOW ESSENTIALLY FREE;
the residual is the generic row machinery (append_out, boundary
hashing, cross-block dedup, k-way merge). That is the next
optimization frontier - and it is exactly the "(shape, rows) ->
[(shape, rows)]" outer scope, so the work stays inside the
architecture. Candidates: dedup-on-the-fly during append (Philippe's
message allowed either), boundary hash directly from kernel outputs
(skip the Col materialization), parallel per-chunk pipelines.

## CORRECTION (2026-08-18, 11:40): the dash kernel was losing rows

The morning report's "gates exact" claim did NOT hold at f35 once the
dash class kernel was in the registry - I did not re-run the f35 gate
after K3b landed, and reported the pre-registry result. The first thing
the next session's gate printed was:

    MISMATCH at f037: engine 363,671 vs interpreter 365,029
    (row keys: 1,358 missing, 0 extra)     [f35: 236,995 vs 269,059 lanes]

Bisected with a new `CELESTE_KERNEL_CLASSES=steady|dash|frozen` knob:
steady and frozen were exact, dash lost the rows. Cause: the emitter
decided "no instruction depends on the buttons -> emit suffix::<0>
only" from the suffix INSTRUCTION buffer, but the dash class writes the
jump/dash buttons straight into p_jump/p_dash - tainted OUT FIELDS with
no instruction behind them. 63 of 64 variants were never produced. An
under-approximation: exactly the class of error the ground rules call
out, and it survived a night because the claim was not re-gated.

Fix (af294d4) generalizes rather than removes the optimization: `B`
reaches the suffix only through the six `kbK` bindings, so an
identifier scan of the suffix body + its tainted KOut exprs is an EXACT
test of the observable bits. Variants agreeing on those bits produce an
identical KOut and dedup collapses them, so one call per distinct
observed assignment is set-equal to all 64:

| class  | observed button bits | suffix calls |
|--------|----------------------|--------------|
| steady | 0,1,2,3,4,5          | 64           |
| dash   | 4,5 (jump, dash)     | 4            |
| frozen | none                 | 1            |

Two emit-time guards added: an untainted output that mentions a button
bit is an error, and a button bit reaching the once-per-config PREFIX
is an error (that would be the same bug one level up).

PROCESS RULE (the real lesson): regenerating any kernel invalidates
every gate. Run the f35 gate after `cargo build` in the regen chain,
before claiming anything. It costs ~6 minutes and it caught this.
