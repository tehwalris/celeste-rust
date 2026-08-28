# Execution scope: ASM kernel cutover + pos-graph partition recorder

Both scoped 2026-08-29 with Philippe. Do both, either order. Companion maps
were produced by two Explore agents this session (findings distilled here).

## A. Pos-graph partition recorder (P5, "the easy one") — DONE 2026-08-29 (9165bd5)

Implemented as scoped: `partition_position_cells` (resolved by object type,
not name pattern), `set_partition_player_position` augments
`resolve_partition_cells`, `PosObserver::input_cell` asserts a uniform chunk
and `record(c_in, outputs)` pairs it, tag path (`POS_ORIGIN`, `tag()`,
`ORIGIN_TAGS` entry, the strip in `interpret_state_base`) deleted. Gated by
three tests (row-set preserved off/on + provably splits; a real streaming
recording pass never trips `input_cell`; the sweep passthrough test trimmed
to SWEEP_ORIGIN). 301/301 quick. STILL TO DO: measure the room (0,0) memory
payoff on a campaign run (was 101 GB with the tag).

### Original scope below (kept for reference)

## A. Pos-graph partition recorder (P5, "the easy one")

GOAL: replace the per-lane `POS_ORIGIN` tag (which defeats mid-frame dedup and
balloons room (0,0) to 101 GB) with a PARTITION by input position, so every
lane in a processing group shares one input cell and no tag is needed.

Philippe's framing: it's the SAME layer as the pm1 merge partition. Position is
CONTENT, so partitioning by it does NOT change the frontier row SET (position-
distinct states are already distinct rows) - it only makes the merge grouping
finer, which is far cheaper than the tag (which forbids ALL merges).

Machinery (already exists, `crates/celeste-interp/src/interpreter/vectorize.rs`):
- `set_merge_partition_patterns(patterns)` / `CELESTE_PARTITION_CELLS` - the
  configured partition cells (by name pattern).
- `resolve_partition_cells(state)` - patterns -> cell indices.
- `partition_class` / `split_states_by_partition` - split states so each has
  uniform partition cells.
- `PosObserver` (src/search/pos_graph.rs): today `tag()` injects POS_ORIGIN per
  lane, `record()` reads it back and pairs (src cell -> dst cell).

PLAN:
1. When recording (`--record-pos-graph`), AUGMENT the merge partition with the
   player position cells (player.x, player.y) so the frontier/input states are
   uniform-position. Philippe: partition on the exact position (named x/y
   cells), NOT a quantized cell_of - it only has to be uniform per group, which
   sidesteps needing a new derived/quantized partition key. (x/y are concrete
   per lane; the widening is on rem/spd, not position.)
2. Replace the tag: for each uniform-position INPUT state, its input cell
   c_in = cell_of(its x,y) is known; run the frame; read each output state's
   cells; record (c_in -> each output cell). No POS_ORIGIN injection, so
   mid-frame dedup within a group fires normally.
3. Delete the tag path (PosObserver::tag, the inject/strip of POS_ORIGIN in
   interpret_state_base, the origin plumbing on Rt2 for pos-graph) once the
   partition path is gated equal.
GATE (cheap, room (1,0)/(2,0)): the reachable row SET and the pos-graph pairs
must equal a tag-based run. Then measure room (0,0) memory (the payoff).

Risk: associating each output with its input group's cell without the tag needs
the frame body to process a uniform-position input and record per-input-state.
Check where interpret_state_base calls tag/record and thread c_in through.

## B. ASM kernel cutover (P6) — PROGRESS 2026-08-29

Committed increments (branch census):
- `e986a9e` — **bool inputs** in the ASM codegen (`CellRepr::Bool`, a
  `LoadMask` inst: movzwl/kmovw/vpmovm2d). Closes gap #2 (bool half). Gated
  by `asm_bool_input_matches_primitives`.
- `b029e74` — **ival inputs** (`CellRepr::Ival`, two ZN planes; repr-aware
  input layout: `Compiled.input_offsets/input_reprs/input_bytes`). Gated by
  `asm_ival_input_matches_primitives`. Plus `trace::emit::asm_roots_and_reprs`
  (extract the flat root list + input reprs from a `Bound`) and the real-graph
  gate `every_start_room_kernel_graph_asm_extracts_and_compiles`.

Findings from the real-graph gate (drove the above):
- The row key is recomputed generically in Rust (`Rt2::row_keys_canonical`/
  `boundary_finish`, seeds match byte-for-byte, `key_check` already asserts
  equality) — so h1/h2 are NOT ASM roots and `CellMix`/`AddW` never need ASM
  codegen. ASM roots = per outcome: output field nodes, then `live`, then `ok`.
- Real kernel inputs are num + bool + **ival** (`player.rem`); all three now
  load. No other input kinds appear.
- The codegen ALREADY implements the specialized fork ops `Frag`/`FragOk`/
  `Span`/`SplitOk`. The ONLY ops it cannot lower on a real graph are the
  CHOICE nodes: `Free` (button assignment) and `Split`/`SplitValid` (interval
  fork). These are UN-SPECIALIZED — `bind`'s graph still holds them.

NEXT INCREMENT (the crux): **per-variant graph specialization**. The real
kernel compute is per-variant (the `Lowered.variants`): each variant fixes
the choices — `Free(d)` -> a constant button state, `Split(d)` -> `Frag((s>>d)
&1)` (graph.rs `fold`, ~:491). So the ASM path must, per shape, enumerate the
choice assignments (the `ChoiceSet`/variant enumeration), specialize
`bind.graph` per choice into a straight-line graph (only Frag/FragOk left,
which the codegen handles), remap the roots, and ASM-compile ONE kernel per
variant. `Lowered.variants` gives the variants but as rendered Rust exprs, not
node graphs — need the node-level specialization (graph.rs specialize/fold +
the choice enumeration) exposed. THEN: generic Rust append/dedup/boundary from
the outcome metadata; dispatch shim at `dispatch.rs:241`; startup retrace;
ASM fingerprint + gates; delete the generated kernel crates.

### Original scope below (kept for reference)

## B. ASM kernel cutover (P6, "wire in ASM, delete the old kernels")

GOAL: replace the ~900k lines of checked-in Rust kernels (the rustc+LLVM
multi-minute relink) with the AVX-512 ASM backend loaded at runtime. Tolerate
the current ~1.3-3x runtime slowdown; integrate first, benchmark after.

### The seam (from the dispatch map)
`src/compiled/dispatch.rs:241 run_traced_kernel(chunk, ids, done)` - the ONE
place a kernel runs. It: `find_by_shape(shape_hash)` -> per 16-lane slice
`(k.step)(chunk, lo, n, accs, seen, skip)` -> `acc.boundary[_exact]` ->
`done.push(acc)`. Called from `compiled/mod.rs:543` (run_frame_chunk) and :995
(step). Both the dispatch AND the ASM backend (`src/transpile/asm`) are in the
TOP crate (celeste-rust), so NO layering move is needed - a JIT'd kernel can be
called right here.

### The minimal cut (from the map)
Keep the ENTIRE Rust wrapper - `bind`, `acc{i}`, `append{i}`, `RowSet` dedup,
`skip`, `boundary[_exact]`. Replace ONLY the `frame` COMPUTE core (the ~1000-op
value-DAG that is the compile-time killer) with a JIT'd `asm::KernelFn`. The
`Kernel.step` (celeste-engine/src/traced.rs) is `fn(&Rt2, lo, n, &mut[Rt2],
&mut[RowSet], &skip) -> Option<u16>`; its guts are `frame`(compute) + `append`.

### The ASM backend (from the API map)
`transpile::asm::compile_and_load(&Graph, roots, tag) -> (Compiled, Loaded)`:
`compile` -> `.s` -> `gcc -shared -fPIC` -> `dlopen`. `KernelFn = unsafe extern
"C" fn(*const u8 packed-in, *mut u8 packed-roots, *const AsmCtx)`. Input = cell-
id-ascending ZN columns (64B each); output = per-root 128B typed slots
(Num/Bool/Ival/Word). Call-outs (div/rem/sin/mget/tile_flag) go through
`AsmCtx`/`CollisionEnv{cart,cache}` into the real engine primitives (bit-exact).

### The gaps to close (bounded)
1. ROW KEY. The h1/h2 fold is built INTO the graph as `Op::CellMix`/`Op::AddW`
   nodes by `lower.rs:1082-1085`. The ASM codegen does NOT implement CellMix/
   AddW (hard-errors). Two options:
   (A) implement CellMix(cell,half)=cell_mix and AddW=wrapping-add in the ASM
       codegen (close to the `Mix` it already does), OR
   (B) EXCLUDE CellMix/AddW from the ASM roots; compute the row key in a GENERIC
       Rust shim from the ASM-computed output cells (one reusable cell_mix fold
       over the output cells, matching runtime2 boundary_finish). (B) avoids the
       codegen work and reuses the boundary key exactly. LEAN (B) first.
2. NON-NUMERIC INPUTS. `Op::Cell` inputs can be bool/interval (e.g. has_dashed),
   but the ASM codegen treats every `Op::Cell` as ZN and packs numeric-only. Add
   ZB/ZI input support (Repr from the cell's type) + input packing for bool
   (u16 masks) / interval columns. This is the real unknown; confirm the extent
   on a real kernel graph (a room (1,0) shape via `trace::kernel::
   reference_frame_in(".")` after CELESTE_START_ROOM=1,0).
3. MARSHALLING SHIM at the seam: pack the chunk's input_cells columns -> input
   buffer, build `AsmCtx::new(&CollisionEnv{cart,cache})`, call, read roots per
   root_kinds -> feed the existing Rust `append{i}`/dedup/boundary.
4. ROOT SELECTION: map each outcome's outputs (cells + live/ok masks, and h1/h2
   under option A) to the ASM `roots` slice. Per-kernel metadata (already known
   at generation: OUT_SLOTS, the outcome masks).
5. GENERATION MODE: JIT at engine startup (re-trace each shape via the tracer
   already in celeste-rust -> compile_and_load) OR pre-assemble `.so`s in
   regen-generated.sh + dlopen by shape hash. Startup-JIT is simpler for the
   slice; needs `gcc` at runtime. The compile-time win holds either way (no rustc
   on 900k lines).
6. FINGERPRINT + GATES: `set_fingerprint_for` (dispatch.rs:146) reads
   `celeste_kernels::*::FINGERPRINT` (a hash of Rust sources) into the checkpoint
   fingerprint (checkpoint.rs:184). An ASM path needs its own fingerprint (hash
   the graph or the `.s`). `*_kernels_are_current` compare committed Rust source
   byte-for-byte - they break/retire when generation stops emitting Rust.
   `*_kernels_reproduce_the_interpreter` (differential.rs:448/518/580) is the
   SEMANTIC gate the ASM kernel must pass (lane counts, missed==0, traced>0).

### Staging (de-risk)
1. VERTICAL SLICE: one room (1,0) shape end-to-end through ASM in the engine,
   gated bit-exact against the current Rust kernel (or the interpreter via
   CELESTE_COMPILED_FORWARD=check). Discovers the input-type extent concretely.
2. Generalize to all shapes/sets (traced/ladder/exact) x rooms.
3. Wire the ASM fingerprint; retire/replace `*_kernels_are_current`.
4. DELETE the generated kernel crates (celeste-kernels-room{00,10,20}) and the
   Rust emitter path (transpile::lower) once the ASM path passes all gates.
   ~900k lines out; they're in git.
