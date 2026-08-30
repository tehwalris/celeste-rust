# Kernel boundary, wrapper deletion, and the search objective

Status: strategy, 2026-08-30. Philippe's direction after profiling the forward
(the kernel is ~37% of a frame body; the wrapper machinery around it is ~63%).

## The approach

Aggressively delete code. Clearly define interface boundaries. Make the code
adhere to those boundaries. Then delete more. Iterate. We treat the kernels as
a GIVEN - their speed is fixed - and drive everything *around* them toward the
kernel's own floor.

Reference floor (room (1,0), f79, 16 threads, QUICK build), from
`CELESTE_KERNEL_DRYRUN=1` (pack + kernel call, discard - no dedup, no
materialize, no boundary, no store):

| what | wall | note |
|---|---|---|
| pure kernel (dry) | **9.1 s** | peak 6 GB |
| full frame body (`fwd.interpret`) | **24.8 s** | kernel is ~37%, machinery ~63% |
| + boundary merge, save/checkpoint | ~+17 s | outside the body |

The 63% is what we are attacking: the dedup layers we materialize-then-filter,
the frontier subtract's cache misses, the boundary heap-walk, the re-keying,
the gc. Most of it is stale wrapper that a clean kernel boundary makes
unnecessary.

## The kernel interface (the boundary we define and hold)

A kernel call takes a FIXED-SIZE batch of input lanes (16) in a clearly-defined
format, and returns 16-lanes-worth of output. Specifically:

- **Input:** 16 input lanes, fixed format.
- **Output:** a set of output lanes - "16 lanes worth of outputs." NOT
  separated by input lane. The kernel just emits some outputs.
- **Within-16 dedup is the kernel's option, not a guarantee.** The kernel has
  static visibility into its 16 lanes, so it MAY dedup them; callers must not
  rely on either "deduped" or "not deduped."
- **No input-lane -> output-row tracking.** We delete this feature entirely
  (the origin / passthrough column). The kernel does not tell you which input
  lane produced which output row.
- **Keys come from the kernel, and only from the kernel.** Each output row
  carries (a) its fully GC'd shape, (b) the hash of that fully-GC'd shape (the
  SHAPE key), and (c) the hash of the row's values (the CONTENT key). The
  kernels already produce GC'd shapes and these hashes; we add/keep tests that
  gate exactly this. **No hash is computed anywhere outside the kernel.**
  Everyone downstream REUSES (shape_hash, content_hash). This deletes the
  boundary canonical heap-walk, the gc-before-hashing, and the
  `engine_row_keys` recompute.

### Consequence for the position graph

Removing input->output tracking removes the fine "which output row came from
which input position" mapping. So the online pos-graph builder either:

- passes a **position-uniform input batch** (same input XY across all 16
  lanes), so every output of that call is known to come from that one input
  position - no per-lane tracking needed; or
- accepts a **coarser** input->output mapping and adds phantom edges to its
  graph.

Either way the origin column is gone.

## The interpreter is the reference, behind the same interface

We do not care about interpreter performance. We pack the interpreter behind
the EXACT SAME interface as the kernels - same input format, same output
format, same (shape_hash, content_hash) keys, GC'd shapes - and it becomes the
reference implementation you can swap in instead of the kernels. It must be
byte-identical in what it returns, not in how fast.

This is what lets us delete the vectorized-interpreter engine machinery: once
the interpreter is a per-call reference, the search does not need it to be a
fast parallel engine.

## The objective to minimize

End to end: run the search binary and get the exact provably-fastest TAS for
room (1,0), starting FRESH (no cached checkpoints). Minimize wall-clock.

Requirements:

- **Checkpoint every frame** - a crash may lose at most the current in-flight
  frame, nothing earlier.
- Same pattern of forward runs as today. Forward runs until it finds a solution
  at rem = 0 bits, then backward to mark, then forward again at rem = 1, and so
  on, until either some rem finds nothing (the horizon is refuted) or the final
  exact kernel run finds the exact solution.

No other requirements. Interpreter speed does not count.

## Strategy: delete, define, adhere, delete

Round 1 target: **delete ~1/4 of the code** (~15k of ~62k lines). The rounds:

1. Delete what the new boundary makes dead: input->output tracking, hashing
   outside the kernel, the boundary re-key/gc, chunking machinery (the
   `chunk_states` input cap, the two inner-slice knobs, the slice loop - keep
   only the load-bearing freeze/moving semantic partition), and the
   vectorized-interpreter engine once the interpreter is a per-call reference.
2. Define the kernel interface as a real trait/boundary; make the ASM kernel
   and the interpreter both implement it.
3. Delete again whatever the boundary orphaned.

Dedup itself STAYS multi-tiered on purpose (different lookup/update costs per
tier - a cache-tiling design), but moves to decide-at-the-door: a lane is
kept-or-dropped as it leaves the kernel, through cheap-local -> within-frame ->
historic tiers, and only survivors are written, already in final form. No
materialize-then-filter, no output buffer, no flush - the memory bound falls
out of never materializing the pre-dedup fan-out.

## Where the ~15k deletion actually lives: gut the interpreter

The interpreter is now ONLY a correctness reference (the kernels are the engine).
But it was originally built to be the fast engine, which is where all its
complexity comes from: the vectorized abstract execution, and running off the
IR/CFG (the IR was built for the deleted rewrite campaign). Reference does not
need any of that. Simplify the interpreter to:

1. **Not vectorized** - one scalar abstract lane at a time. Deletes the
   interpreter's vectorization (`vectorize.rs`, `virtual_merge`, the per-lane
   column/split machinery, the state-set/work-list management).
2. **Run directly off the AST, not the IR** - exactly like the tracer already
   does. The tracer's `Interp<'a, D: Domain>` walks `full_moon::ast` with a
   pluggable domain; the reference interpreter is that walker plus a
   concrete/abstract-value domain. This deletes the IR/CFG entirely
   (`celeste-ir`'s `frontend` CFG builder, `ir`, the CFG printer, and the
   interpreter's CFG glue `interpret_cfg`/`interpret_prepared_cfg`), because
   the IR is consumed ONLY by the interpreter path (concrete.rs, the compiled
   fallback in compiled/mod.rs, program setup). Lua PARSING (full_moon) and the
   builtin table stay.
3. **Forks by decision-counting DFS, not a work list.** Run one scalar path.
   At a fork that must go N ways, consult a decision cursor: if this fork is
   within the cursor, take the recorded choice; if it is a NEW fork past the
   cursor, take choice 0 and append `(0, N)`. At a leaf (frame end) emit the
   state (with its shape + value hashes). Then ADVANCE the cursor and RE-RUN
   FROM THE START instead of backtracking: increment the last decision; if it
   hits N, pop it and increment the new last; empty list => done. This is a
   depth-first search over the decision tree by re-execution - no continuation
   capture, no state snapshots, no work list. Correctness needs: deterministic
   execution (input + cursor -> the same path), a stable fork order with a
   consistent N per fork, truncate-on-advance (diverging at fork i discards the
   old choices after i, re-derived fresh), and a finite fork tree (a game frame
   terminates, so it is). The enumerated leaf SET must equal what the vectorized
   split produces today - that is the gate. Re-execution cost is fine: this is a
   reference, and per-lane fan-out is small.

Sequencing (this is not a blind delete - it is delete the OLD oracle only after
the NEW one is gated against it):

1. Build the AST scalar DFS interpreter behind the kernel interface (reuse the
   tracer's `Interp<D>` walker + a reference domain).
2. Gate it: identical output keys to the current interpreter (and to the
   kernels, which are gated against the current interpreter) across many states.
3. THEN delete the old vectorized IR interpreter + the whole IR/CFG layer +
   `vectorize`/`virtual_merge`. This is the bulk of the ~15k.

This pairs with packing the interpreter behind the kernel interface - same work,
one boundary.

## Concrete design (from the 2026-08-30 architecture map)

The tracer's `Interp<D>` (`src/trace/interp.rs`) is ALREADY an abstract
interpreter over the Lua AST, generic over a `Domain` (`src/trace/domain.rs`),
with two impls: `Concrete` (Num=P8, Bool=bool, the oracle) and `Symbolic`
(the tracer). It owns control flow, the state/heap model, the split builtins,
fork/merge/collapse - and does NOT touch the IR. So the new reference
interpreter is a THIRD domain plus a driver, not a new engine.

Key decision - keep it SCALAR + DFS, do NOT add a lane-vector domain. The
map's first instinct (a vectorized lane domain) re-creates the very
vectorization we are deleting. Instead:

- **`DecisionDomain`**: Num = an abstract number that is either a point or a
  closed interval (the only widened numeric form); Bool = definite or
  undecided. Its `decide` NEVER returns `None`: on an undecided condition it
  consults a **decision cursor** (choice i of N), records the decision, and
  returns that choice - narrowing the interval/bool to the chosen side. So
  `Interp`'s own fork-merge (the `sel_*` path) is never triggered; `Interp`
  runs ONE scalar path. The fork points - straddling compare, `unknown_bool`,
  `fork_flr`/`__split_by_flr`, `__split_at` - all route through the cursor.
- **DFS driver**: run the frame via `Interp<DecisionDomain>` to a leaf, emit
  the output state's `(shape_hash, content_hash)`, then advance the cursor
  (increment last decision; pop-and-increment on overflow) and RE-RUN from the
  start. Depth-first over the decision tree by re-execution; no merge, no
  snapshots, no work list. The enumerated leaf SET must equal the vectorized
  split's output set - the gate.
- **Abstract Num ops** replicate the OLD interpreter's semantics (`op.rs`,
  `game_runner.rs`): interval endpoint arithmetic, `sin(interval)=[-1,1]`,
  interval compares -> definite / cursor-fork, `flr` on an interval -> fork.
- **Widenings**: reuse `src/trace/widen.rs` (already replicates rem/spd/
  dash-clamp/fruit-off/timer pins and imports `RemPrecision`/`SpdPrecision`).
- **Bridge**: import a checkpoint `State` (one lane) into `trace::State<D>` for
  the initial heap; extract the output `(u64,u64)` row key the SAME way
  `engine_row_keys` does (`runtime2::boundary_finish`: shape hash + per-cell
  `cell_mix` sum, two seeds). Gate: identical row-key SET to the engine and
  the old interpreter, replayed over the on-disk checkpoints
  (`/var/tmp/celeste-checkpoints/{kfwd,ladder-kern3}`, 94 frames each).

## Honest deletion scope (this corrects the ~20-30k estimate)

A large part of `celeste-interp` is NOT the interpreter - it is the SEARCH's
dedup / frontier-subtract / boundary-merge, which the LIVE compiled search
calls around every frame (the kernel only replaces the frame BODY):
`vectorize.rs` (2744) + `virtual_merge.rs` (1263) ~= 4k lines. These are
SHARED and must be PORTED/KEPT, not deleted by the interpreter work. (Their
simplification is the separate decide-at-the-door boundary redesign above.)

Deleting the old interpreter is GATED: re-point its live consumers first -
`src/search/run.rs`, `src/compiled/mod.rs`, `src/concrete.rs`, `native-probe`,
`src/program/mod.rs` - and make the NEW interpreter the oracle for the
kernel-correctness gates in `src/search/differential.rs`
(`asm_kernels_reproduce_the_interpreter` et al., which run every commit).
Keep `game_runner` room setup and `abstraction::{Rem,Spd}Precision`.

What the interpreter rewrite cleanly deletes once the new one is gated and the
consumers re-pointed:

- the IR/CFG: `celeste-ir` `frontend.rs` (1421) + `ir.rs` (999) + `print.rs`
  (238) ~= 2.7k, IF no CFG consumer remains;
- the CFG interpreter core: `core_interpreter.rs` (1379) + `op.rs` (1363) +
  `flow.rs` (523) + `glue.rs` (316) ~= 3.6k;
- one copy of the widenings (`abstraction.rs` 1476) if consolidated with
  `trace/widen.rs`;
- relocating (not deleting) the 52 e2e tests now stranded in `src/main.rs`.

So the INTERPRETER work is ~6-8k of clean deletion, not 20-30k. Reaching
"about half" needs the OTHER two redesigns too - decide-at-the-door (which
shrinks the 4k dedup layer) and the chunking removal. Same direction, three
fronts; this doc is the interpreter front.

## Progress log (interpreter front)

Committed:
- `refdomain.rs` - `RefDomain` (Num=interval, Bool=bool) + the DFS `Cursor`,
  the value ops (interval arithmetic, compares fork-or-definite, unknown_bool,
  fork_flr). Unit-tested (cursor DFS incl. path-dependent trees, interval fork).
- `refdriver.rs` - `run_frame_all` enumerates a frame's fork tree by
  re-execution over one shared `Interp<RefDomain>`. An ignored smoke test warms
  the REAL cart to the player and runs a real frame end to end through
  RefDomain (arithmetic, control flow, collision, buttons, builtins) - it
  PASSES (~11s). So the domain handles the real game on the concrete path.
- `verify::find_player` generalized to `<D: Domain>`.

In flight:
- `refbridge.rs` - the state bridge (checkpoint `interp::State` lane <->
  `trace::State<RefDomain>`) + `row_key_of` (via the trusted `engine_row_keys`)
  + the first real GATE: old interpreter vs RefDomain on an early checkpoint
  frame, comparing output row-key SETS.

Next after the gate is green on early (concrete) frames:
- Generalize `trace/widen.rs` from `State<Symbolic>` to `<D: Domain>` so
  RefDomain states widen (needed to gate mid/late frames, and it consolidates
  the widening with `abstraction.rs` - a deletion).
- A RefDomain forward loop (frontier + widen + dedup by row key), gated
  per-frame against the kernel checkpoint rowkeys
  (`/var/tmp/celeste-checkpoints/kfwd/room1/frames/*.rowkeys`).
- Make RefDomain the oracle for the `differential.rs` kernel gates
  (`asm_kernels_reproduce_the_interpreter` et al.), re-point the live
  consumers (`run.rs`, `compiled/mod.rs`, `concrete.rs`, `native-probe`,
  `program/mod.rs`), then delete the old CFG interpreter + IR.

## GATE RESULT (2026-08-30): the new interpreter reproduces the oracle

Verified: `RefDomain` == old interpreter, EXACT row-key sets, all lanes, at
f5/f10/f20/f30/f40/f60/f94 (concrete AND widened rem-interval frames), 0
mismatched, 0 errored; bridge round-trips exact. Committed `f16df8c`. The
oracle contract holds - the new AST scalar interpreter is a correct
replacement.

## The deletion phase and its ONE real blocker: RefDomain per-lane cost

RefDomain is ~6 s PER LANE at f94 (the DFS re-runs the whole frame once per
button/fork path, ~64+ paths, no vectorization - by design). Correct, but
~1e5-1e6x slower per lane than the old vectorized interpreter. This does NOT
matter for its reference role on a few states, but it DOES matter for the
existing consumers/gates that run the reference on THOUSANDS of lanes:

- `differential.rs` kernel gates check the kernel against the interpreter over
  a frame's worth of states. With RefDomain that is hours-to-days unless they
  SAMPLE (check N lanes/frame). Sampling is the intended answer - strong,
  bounded-time coverage - but it is a real change to those gates.
- `run.rs`'s per-frame non-compiled search step (`None => interpret_prepared_cfg`,
  ~2582/2633) is high-volume. RefDomain cannot replace it at scale. To delete
  the old interpreter this fallback must be REMOVED (the search always uses the
  compiled engine - which strict mode, `CELESTE_KERNEL_STRICT` default on,
  already assumes: a missed chunk is fatal). That is an architectural commitment
  aligned with the never-deopt doctrine, but it touches the live search and
  needs care (some interpreter-only tests exercise the fallback).

So the cut order is:
1. Build the bridge WRAPPER `interp::State -> Vec<interp::State>` via
   RefDomain (drop-in shape for `interpret_prepared_cfg`), gated against it.
2. Re-point the LOW-VOLUME consumers (init, deopt-single-state, `concrete.rs`
   oracle, `native-probe`) to the wrapper; make the multi-lane gates SAMPLE.
3. Remove the interpreter search fallback (search := compiled-only); re-point
   `run.rs` init/reference.
4. Now `interpret_cfg`/`interpret_prepared_cfg`/`frontend::compile` have no
   consumers -> delete `glue.rs` + CFG interp (`core_interpreter`/`op`/`flow`)
   + IR (`frontend`/`ir`/`print`). ~6k lines.
5. Consolidate the widenings (`abstraction.rs` <-> `trace/widen.rs`).

Steps 2-3 touch the live search, so they are done with the full suite as the
guardrail (revert any step that reddens it), NOT rushed unattended. Step 1 is
safe and additive; it is the next thing.

## Rebuild progress (inside-out minimal copy, 2026-08-30)

Building the target architecture (plans/architecture.md) inside-out by writing
the interfaces fresh and wiring the existing engines behind them. Add the clean
core first, delete the old machinery last.

Done (committed, all compiles warning-free):
- `src/frame.rs` (198 lines) - the whole innermost contract:
  - `Block` = opaque multi-lane `State` + exposed key/position columns
    (`keys()`=engine_row_keys, `positions()`=state_cells) + `keep(mask)` split
    (via `State::split_by_condition`) + an optional engine-supplied key cache.
  - `trait FrameStep { run(&mut self, &Block) -> Vec<Block> }` - interface #1.
  - `forward_frame()` - the minimal forward frame: run each frontier block,
    dedup at the door by a per-lane is-key-new mask, keep survivors. ~20 lines.
  - `Visited` - the frontier key set behind insert/contains.
- `RefEngine` impls `FrameStep` (trace/refengine.rs) - the trusted reference.
- `FrameEngine` impls `FrameStep` (compiled/mod.rs) - the fast kernels, handing
  back the precomputed key column.
- e2e test `forward_frame_drives_the_reference_engine` (#[ignore], 0.46s):
  4 frames from the initial block, visited grows 2->3->4->5. Passes.
- refgate already proves RefEngine == FrameEngine key sets (the trait impls are
  thin wrappers over the exact methods it compares).

Next, still inside-out on top of `forward_frame`:
1. The forward DRIVER: frames-until-win loop + win detection (is_win over the
   position column) + block checkpoint (compact batched serialize) + the
   position-graph recording. Copy the minimum from run.rs's step machinery.
2. Backward + ladder against the same FrameStep.
3. Migrate the real search (bin/rewrite Ladder/Sweep/Bench) onto frame.rs; then
   DELETE run.rs's step_inner/step_parallel/chunking/phased machinery (~2.6k)
   and whatever else the migration orphans.

The line count rises first (frame.rs added: 50,362 -> 50,497); the big drop is
step 3, when run.rs's machinery is deleted after the migration.

### Update 2026-08-30 (cont): forward driver done; pos-graph is coupled to backward

Added on top of the frame-step core, committed, tested:
- `forward_run(engine, initial, dir, max_frames) -> Option<win_frame>` - the
  whole outer forward loop (seed visited + checkpoint f0; loop forward_frame +
  checkpoint survivors until win/empty/horizon). No chunking, no variants.
- `block_wins()` wraps the existing per-lane `win_lane_mask`.
- Checkpoint = REUSE of `save_frame_states` (bincode+zstd over the whole
  frontier as one batch). Added `save_frame_state_refs` so the frontier is saved
  by BORROW, not cloned; byte-identical file.
- e2e `forward_run_drives_the_reference_engine`: 5 frames over the reference,
  every frontier checkpointed and reloaded. 0.47s, green.

Finding: `PosObserver::input_cell` REQUIRES each recorded block to be uniform in
player position (the position partition, `vectorize::set_partition_player_
position`, installed while recording). So pos-graph recording is structural, not
a bolt-on - and it is precisely backward's input. Next push builds pos-graph
recording + backward + ladder together against the same FrameStep, then migrates
bin/rewrite (Ladder/Sweep) onto frame.rs and deletes run.rs's step machinery.

### Update 2026-08-30 (cont): the (shape,position) partition kills origin tracking

Philippe's design: make (shape, position) the BLOCK IDENTITY - store/group
states by shape AND position, so every block is uniform in both. Then the frame
step's lanes all share one input position, so:
- the kernel needs NO per-lane origin tag (its whole job - "which input did this
  lane come from" - is answered by the block's position), and
- the pos-graph is a direct read: input block at cell c_in -> edge to every
  output position. No tracking.

Three unrelated things are named "origin"; only #3 is deleted:
1. virtual_merge::Origin (Heap/Local/Outer) - merged-column provenance, feeds
   ShapeCensus. KEEP.
2. pos_graph grid ORIGIN - cell-numbering coordinate origin. KEEP.
3. Rt2::origin + origin_tag_of + deopt_collect::read_origins_named/inject_named
   + asm_kernel track_origin + SWEEP_ORIGIN - the per-lane input->output tag.
   DELETE.

Why the delete is safe (the coupling): the tag's main consumer is the backward
SWEEP (sweep_time.rs reads SWEEP_ORIGIN for exact src-key->dst-key edges). The
tag is a STORED-EDGE OPTIMIZATION to avoid re-running the frame step in backward.
The clean backward (architecture.md sentence 8) RE-RUNS the frame step on
candidates and keeps those that reach a kept key, using the pos-graph only to
NARROW candidates by position. Re-run + narrow replaces stored edges -> the tag
has no consumers -> delete.

All the primitives already exist (reuse, not rewrite):
- regroup survivors -> canonical (shape,position) blocks:
  vectorize::split_states_by_partition (position cells via
  set_partition_player_position/abstraction::partition_position_cells) +
  virtual_merge::merge_dedup_group (canonicalize a group).
- pos-graph: pos_graph::PosObserver::record(c_in, outputs), position partition on.
- backward: FrameStep::run re-run + PosGraph narrowing + the checkpoints.

Build order (each on the last, build-then-delete):
1. Clean regroup/boundary in the forward loop: after forward_frame's dedup,
   group survivors by (shape,position) and merge_dedup_group each -> next
   frontier of canonical position-uniform blocks. (Handle the no-player
   "no position" class explicitly.)
2. Pos-graph recording in forward_run (record c_in->output positions per input
   block; build+save the PosGraph at the end).
3. New backward on PosGraph + FrameStep re-run + checkpoints; replaces the
   CellStore/sweep_time backward.
4. DELETE origin #3 everywhere once (3) lands: Rt2::origin field + append/dedup
   propagation, origin_tag_of/export_block_tagged, deopt_collect read/inject,
   asm_kernel track_origin, SWEEP_ORIGIN, sweep_time's origin path.

Occupancy caveat to instrument at step 1: mean lanes/block under shape vs
shape*position at a real horizon (finer partition must not starve AVX-512).

### Update 2026-08-30 (cont): steps 1-2 DONE (partition + pos-graph, no tags)

- Step 1 (regroup boundary): forward_frame regroups survivors via
  vectorize_states -> canonical (shape, partition-class) blocks. Committed.
- Step 2 (pos-graph): forward_run `record` mode installs
  set_partition_player_position, so blocks are (shape,position)-uniform, and
  records the graph as a direct read (input cell -> output positions) via
  PosObserver::record_dsts. ForwardResult { win_frame, frames, pos_graph }.
  No per-lane origin tag anywhere. e2e green (4 live cells over the intro).

Remaining:
- Step 3 (NEXT, the bigger build): new backward on (PosGraph + FrameStep re-run
  + checkpoints). Walk H-1..1; a checkpointed block is kept if re-running the
  frame step on it produces a kept-key output; narrow candidates by the pos-graph
  (only cells that reach a kept cell). Replaces sweep_time.rs's CellStore/origin
  path. This is what removes the tag's last consumer.
- Step 4 (delete, after 3): Rt2::origin + append/dedup propagation,
  origin_tag_of/export_block_tagged, deopt_collect read/inject, asm_kernel
  track_origin, SWEEP_ORIGIN, sweep_time origin path.

### Update 2026-08-30 (cont): sharding + backward design pinned

Design decisions agreed with Philippe (the horizon-anchored minimal ladder;
DELETE the e/g/band numbering):
- Backward is SINGLE-PASS (H-1..1), not a distance DP. g is redundant: g =
  H - mark-frame; band = the marked set. Delete g.bin/save_g/load_g, e+g,
  FILTER_BAND, banded-rung plumbing.
- Ladder: forward at rem0 -> mark backward -> forward at rem1 discarding any
  state whose widened-to-rem0 form is not marked -> mark -> ... until full
  precision or empty (impossible). The marked set is the cross-rem filter.
- Backward marking needs input->output lane PROVENANCE (dedup makes it a SET:
  one output can come from several of the 16 inputs; on a hit, mark ALL). Carry
  it as a per-output lane BITMASK column (u16/u64), OR-ed on merge - a NARROW,
  per-call reuse of the origin-union logic, NOT the deleted cross-frame plumbing.
  So origin deletion is PARTIAL: delete forward/global origin; keep a per-call
  backward lane mask.
- Everything shards by (shape, cell): storage (done), visited (done), and the
  marked bitmask (one bit per row-offset within each shard file).

Existing code map (Philippe asked what does-this vs does-other):
- sweep_time.rs ALREADY does the frame-walk + re-derive-successors + pos-graph
  candidate cells (mark_dst_cell) + bitmap + position-keyed CellStore = the
  design. KEEP the shape.
- sweep_time.rs + sweep.rs ALSO compute/store g + e + band = the numbering.
  DELETE. Blast radius: checkpoint.rs band loader, dispatch, asm_kernel
  fingerprint references.
- SWEEP_ORIGIN is the backward lane-provenance -> KEEP (re-typed to per-call
  lane bitmask), contradicting the earlier "delete all origin".

Open (proceeding on defaults, flag inline): narrowing precision (position-only,
load all shapes at candidate cells) - Philippe to confirm.

### Update 2026-08-30 (cont): frame.rs architecture logic complete (mostly draft)

The rebuilt core now has every logical piece, each committed:
- Forward: forward_run (dedup at door, sharded checkpoint, pos-graph). TESTED.
- Storage + visited: sharded by (shape, cell). TESTED.
- Backward: backward_run/backward_walk - single-pass, position-narrowed, cost-
  bounded. DRAFT (per-lane, no provenance). Machinery TESTED on the intro chain.
- MarkFilter: the ladder forward discard-filter (widen-to-coarser + membership),
  replacing e/g/band. TESTED (keeps marked, discards unmarked).
- ladder(): SKELETON tying it together; termination semantics OPEN.

None of this is wired into the real driver (bin/rewrite still uses run.rs/sweep).
Remaining, roughly in order:
1. Ladder termination semantics (soundness, WITH Philippe): per-level horizon vs
   refutation vs re-run-at-larger-horizon; what the reported optimum is.
2. Real-room validation of backward + ladder (the "together" step; needs a full
   room, the compiled engine, a real win).
3. Wide + per-lane-bitmask provenance for backward (kernel-side work; validate
   equal marks vs the per-lane draft).
4. Migrate the real driver (bin/rewrite Ladder/Sweep) onto frame.rs.
5. DELETE: e/g/band numbering (sweep.rs g/save_g/load_g, FILTER_BAND, banded
   rungs) + forward/global origin (Rt2::origin, origin_tag_of, read/inject,
   asm_kernel track_origin) - KEEPING SWEEP_ORIGIN re-typed as the narrow
   per-call backward lane mask.
