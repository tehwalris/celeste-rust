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
