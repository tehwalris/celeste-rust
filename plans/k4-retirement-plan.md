# K4: retire Rt2/Rt3 engines — keep kernel + vectorized interpreter only

Philippe's directive (2026-08-18 morning): remove the older compiled
backends entirely; keep the kernel and the vectorized interpreter;
simplify whatever the removal makes possible.

Vocabulary that keeps this safe:
- "Rt2 the ENGINE" = executing the transpiled program (gen::call_fn)
  over the columnar Engine impl. DIES.
- "Rt2 the STRUCT" = the columnar block (structure/cols/width),
  boundary(), dedup, merge, retain_lanes, import_block. This is the
  (shape, rows) DATA MODEL. STAYS (rename later if we care).
- Rt3 = tile engine. DIES entirely (runtime3.rs).
- Scalar Rt = concrete hex oracle for the Engine-trait emission; when
  the Engine emission dies its subject dies. DIES with stage 4.
- gen.rs = transpiled program + NAME TABLES. The name tables
  (GLOBAL_NAMES / FIELD_NAMES / field_id / global_id) are load-bearing
  for import/boundary/witness; the program body dies in stage 4.

## Stages (each ends: bench gates f20/25/30/35 exact + commit)

1. **Uniform-output guard** (agreed with Philippe): in append_out's
   uniform-scalar writes, compare-on-rewrite; mismatch sets bd (chunk
   deopts). Turns the config-independence premise into a guard.
   [DONE if committed - check git log]
2. **Block -> State exporter + interpreter fallback**: native-probe
   gets `export_block(&Rt2) -> State` (inverse of import_block: heap
   from structure+cols, globals by name, MaybeVector columns; lane
   count = width). frame_step's fallback path (non-kernel chunks,
   deopt rows, SplitReq re-runs) becomes: export -> run ONE frame via
   celeste_rust interpreter glue (the program built once from
   rewrites-compile.jsonl, same as the system reference) -> take the
   boundary output states -> import_block back. The SplitReq / plain
   gen::call_fn worklist code is DELETED in the same commit (the
   interpreter handles splits internally).
   Gate: f20 (spawn - all lanes take this path!), f25, f30, f35.
   Also compare wall times before/after: the fallback is ~0.1-39% of
   lanes depending on frame; slower fallback is acceptable, note the
   numbers.
3. **Delete Rt3**: runtime3.rs, run_chunk_dynexp, TILE env parsing
   (kernel mode becomes the only compiled mode; keep a
   CELESTE_KERNEL=0 escape hatch routing everything to the
   interpreter fallback for A/B). Delete bail/gate-reject counters.
4. **Delete the Engine emission + scalar oracle**: transpile emits
   (a) the name tables gen.rs currently provides, (b) the kernels.
   Delete: Engine trait, runtime.rs scalar execution, Rt2 execution
   impl (arena/call_fn/ColId ops - keep struct + boundary/merge),
   concrete bench modes in native-probe main, gen.rs program body.
   This is large but mechanical; compiler errors are the worklist.
   KEEP: import.rs, boundary/dedup/merge/retain in runtime2.rs,
   row-key hashing, kernel.rs, kernel_gen_*.
5. **Simplify**: whatever became single-use (Engine generics on
   import? BoundaryIds construction? BUILTIN_NAMES ABI comments),
   collapse it. Suite + all bench gates + commit.

Order matters: 2 before 3 (need the fallback before deleting the old
one). The dedup-on-the-fly plan (plans/dedup-on-the-fly-plan.md) is
INDEPENDENT of stages 3-5 and can interleave; it touches
run_chunk_kernel + the emitter only.

Interpreter-fallback design notes for stage 2:
- Program: build once from rewrites-compile.jsonl at probe startup
  (lazy static). Its FixedEnv needs the game builtins installed the
  same way game_runner does for the system verify - reuse
  celeste_rust::game_runner helpers (create_initial_state... no:
  look at how bin/rewrite's differential runs build FixedEnv for a
  Program; call the same function).
- Export fidelity: import drops fields the program never names; the
  exporter reconstructs only what the block carries - that is exactly
  the boundary state content, which is what gate 2 already compares.
  UnknownBool cols -> Value::UnknownBool; Ival -> NumberInterval;
  pointers rebuild the heap topology from structure.
- The interpreter output states then go through the NORMAL boundary
  (make_state_abstract etc.) interpreter-side before re-import? No:
  import_block + .boundary(ids) on the imported block reproduces the
  canonical form - mirror what load_states_any + bench do today.

## Progress (2026-08-18)

- Stage 1 DONE (uniform-output guard, commit edeb248).
- **Stage 3 DONE and moved AHEAD of stage 2**: Rt3 (runtime3.rs, 1,676
  lines), run_chunk_dynexp, the TILE mode switch and the tile bail /
  shape-gate-reject counters are deleted. This was safe to do first
  because the class kernels now cover 100% of player lanes on the
  engine's OWN blocks (see the partitioner fixes), so dynexp had no
  remaining job: 34-frame run 175 -> 173 ms, gates exact at
  f20/25/30/35. The kernel is now the DEFAULT path; CELESTE_KERNEL=0
  routes everything to the reference for A/B.
- **Stage 2 DONE.** `import::export_block` (the inverse of
  `import_block`) plus `run_chunk_interpreted`: a chunk the kernels
  decline is exported to a `State`, run through
  `interpret_prepared_cfg`, re-imported and boundaried by the SAME
  `Rt2::boundary` the compiled path uses. The `gen::call_fn` worklist
  and its SplitReq partition-and-rerun dance are deleted with it - the
  interpreter splits internally and just returns more output states.
  Gates f20/25/30/35 all "row-key SET EQUAL (gate 2) OK".
  Cost, measured: f20 (the only frame where the fallback actually
  runs - every later frame is 100% kernel) 0.58 -> 0.95 ms; f30 15.42
  -> 14.93 ms and f35 103.0 -> 105.4 ms, i.e. noise. As predicted, the
  fallback's speed is irrelevant.
  - **The afternoon this cost, and the lesson.** The first version
    built the fallback from `Program::compile_executable_from_disk()`,
    because that is what `gen.rs`'s generated header says its source
    is. It is not: the canonical regen is `transpile --recipe
    rewrites-compile.jsonl`, so gen.rs is the REWRITTEN program, and
    the header line was emitted unconditionally. The plain program
    boxes a captured `self` where the recipe's `demote_create` does
    not, so the output heap gained one cell, every later cell id
    shifted by one, and the gate reported "204 missing, 204 extra" -
    a total mismatch caused by an aliasing difference in ONE closure
    capture. Two things came out of it: the emitter now names the
    actual source program (regenerating gen.rs changed those two
    comment lines and NOTHING else, which is independent confirmation
    of the diagnosis), and the gate's structural dump now prints
    every cell's kind WITH its field/capture targets and only the
    differing cells - the old Obj-names-only dump showed two
    identical-looking cell lists next to two different shape hashes,
    which is worse than no dump.
- Stages 4-5 (delete the Engine trait / scalar oracle / gen.rs program
  body, then simplify) follow, and stage 2 has removed their last
  blocker.

## Stage 4: the constraint the plan missed (found 2026-08-18)

"transpile emits the name tables plus the kernels" is NOT a deletion,
because **the name tables are a side effect of the emission walk.**
`Gen` interns STRINGS / GLOBAL_NAMES / FIELD_NAMES / FN_NAMES /
SITE_INFO / BRANCH_INFO as it walks the program emitting function
bodies; delete the walk and the tables lose their source, change the
walk and they can silently REORDER.

Reordering them is not cosmetic. `FIELD_NAMES` is the canonical field
ordering the boundary hashes (`Cell2::Obj` holds interned field ids),
so a different order is a different shape hash, a different row key,
and a different search. This is the same class of thing as the
BUILTIN_NAMES ABI already flagged below.

So stage 4 splits into two pieces with very different risk:

1. **Stop WRITING the program body** (`call_value`, `call_fn`, the
   per-function bodies, `use crate::runtime::*`) - gen.rs 29,672 ->
   ~300 lines - and delete everything in native-probe that consumed
   it: the `Engine` trait, runtime.rs's scalar execution, `Rt2`'s
   Engine impl (keeping the STRUCT + boundary/dedup/merge/retain),
   `import_lane` + `assert_lane_matches_block`, the concrete `-i/-f`
   and `--bench` modes, `SplitReq` and its panic hook. Mechanical;
   compiler errors are the worklist. KEEP `BUILTIN_NAMES` - import.rs
   needs it for both importers.
2. **Turn `Gen`'s emitter into a pure interning walk** (~540 lines of
   `format!` deleted, the recursion kept). Only worth doing after (1),
   and it needs its own gate.

The gate for BOTH is the same and it is cheap and total: snapshot the
name-table section, regenerate gen.rs, and require the section to be
**byte-identical**. That is exactly the property a reordering would
break. (It has already paid once: regenerating after the header fix
changed two comment lines and nothing else, which is what confirmed
gen.rs was the rewritten program.) Then f20/25/30/35 + the suite.

Note gen.rs is GITIGNORED, so the snapshot is a local before/after, not
a diff against the tree - extract it by content, not by line number:

    sed -n '/^pub static STRINGS/,/^pub fn field_id/p' native-probe/src/gen.rs \
        | head -n -1 > ~/perf-scratch/gen-nametables-before.txt

Do NOT do (2) as part of (1). If the tables move, you want to know
which change moved them.

## Stage 4 piece (1): DONE 2026-08-18

`transpile` still walks every function - the walk is what interns the
tables - and throws the emitted text away. gen.rs went **29,672 -> 3,962
lines**, and the name-table gate passed byte-identical, so nothing
reordered.

Deleted with it: `runtime.rs` entirely (1,004 lines - the `Engine`
trait, `Callee`, the scalar `Rt` and its 18 builtin impls, `V`, `Cell`),
`Rt2`'s Engine impl and its arena (the ColId ops, `put`/`get`/`map1`/
`map2`/`select_inner`/`split_multi_with`, the `av_*` per-lane op ports),
`SplitReq` and its panic hook, `import_lane` + `assert_lane_matches_
block`, `Rt2::from_scalar`, and the probe's concrete modes (`-i/-f`,
`--bench`, `--from-checkpoint` gap census, `--branch-census`). What was
`runtime.rs` is now `builtins.rs`: 37 lines, `BUILTIN_NAMES` only, which
is an ABI in exactly the way FIELD_NAMES is (the index is what
`Cell2::Bi` stores and what import/export translate through).

Two things this forced that are improvements in their own right:

- **The starting position now comes from the interpreter.** `--abstract`
  used to begin from `build_rt()`, which ran the transpiled `__init` on
  the scalar runtime and hand-placed the builtin cells - a second
  implementation of a starting position. It now runs the program's
  `init_cfg` through `interpret_cfg` and imports the result, i.e. the
  same construction as `verify.rs`'s `AbstractRun::start`. Gate: lane
  counts match the campaign checkpoints exactly - frame 25 = 204, frame
  26 = 878, frame 30 = 27,024.
- **The pm1 merge patterns are now set in the probe.** They are a process
  global that `AbstractRun::start` sets and the probe never did; any
  frame this process interprets has to run under the same setting or it
  merges differently from the reference it claims to be.

And the crate-wide `#![allow(unused_variables, unused_assignments,
unused_mut, unreachable_code, dead_code)]` - which existed for the
transpiled body - is **gone**, replaced by `#[allow(dead_code)]` on the
four GENERATED modules only. That surfaced ~60 warnings of real dead
code, all deleted here. (CLAUDE.md's warning about a crate-wide allow
hiding ~150 lines was about a different crate; this one was hiding its
own.)

Gates: f20/25/30/35 all "row-key SET EQUAL (gate 2) OK"; suite 551/551.
Times f20 1.44 / f25 1.37 / f30 15.52 / f35 107.2 ms (min of 3), i.e.
unchanged at depth - f35 was 105.4 before, which is run-to-run noise.

## Stage 4 piece (2) + stage 5: DONE 2026-08-18

`Gen`'s emitter is now `walk_instruction` / `walk_function`: an arm per
instruction kind that interns what that instruction's emitted form used
to intern, and nothing else. `src/bin/transpile/main.rs` went 1,125 ->
449 lines and gen.rs 3,962 -> **236**.

Deleted with it: `SlotMap` and the whole `--site-slots` / `--emit-slots`
slot-binding subsystem (load, whole-program escape analysis, dense
numbering - it fed `HAS_SLOTS`, which died with the Engine trait),
`phi_copies`, `collect_locals`, `button_of_expand`, `l()`, and the
emission of the tables that had no consumer left: SITE_INFO,
BRANCH_INFO, SLOT_CELLS, SLOT_SHAPE, N_SLOTS, FN_INIT, FN_FRAME.

Three checks were deliberately KEPT even though nothing is emitted from
them, because each one catches a program this toolchain cannot
represent, and here is the cheapest place to say so: a closure targeting
an unknown function, a builtin outside the ABI or at an arity with no
lowering, and a program missing `__init` or `__frame`.

The gate held exactly as designed: STRINGS / GLOBAL_NAMES / FIELD_NAMES /
FN_NAMES came out **byte-identical** (cmp, 224 lines) across both the
walk rewrite and the table deletion, so the interning order did not move.
Gates f20/25/30/35 exact; suite 551/551. Times 1.39 / 1.32 / 14.94 /
110.98 ms - f35 spans 105.4-111.0 ms across the three runs today, which
is the noise band, not a trend.

Net for stage 4: **~30,300 generated lines and ~2,500 hand-written ones
deleted**, with the compiled path (kernels) and the reference path
(interpreter) both untouched and byte-exact throughout.

## Where this sits in the overall queue

See plans/campaign-cost-plan.md "ORDER OF WORK": stages 2 and 4-5 of
this plan are items 3 and 4 of the merged queue, and they come BEFORE
the crate split (P1) deliberately - so the split moves the ~5k lines of
generated code that survive rather than the ~34k that includes gen.rs's
program body, which stage 4 deletes.
