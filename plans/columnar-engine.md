# The columnar abstract engine (stage 2 of plans/native-probe.md)

Design written 2026-08-18 (overnight session; design call delegated -
Philippe reviews in the morning). This is the architecture for goals 2+4
of plans/overnight-2026-08-17.md, fused: the row struct IS the column
set, and SIMD is the loop over lanes inside each column op.

## The insight that zero divergence buys

The zero-divergence census (0 divergent of 26 executed branch sites,
commit 760c8a6) means the compiled shape program's control flow does not
depend on WHICH lane is running: every lane of the shape takes the same
path through the 26 uniform branches (loop trip counts, shape gates).
All input- and abstraction-dependence is DATA: masked select-stores,
`expand` (UBool -> both bools), `__split_by_flr` / `__split_at`
(interval -> per-floor / 3-way alternatives).

Consequence: the abstract engine needs NO DFS, NO choice tape, NO
continuations, NO per-fork snapshots. Execute the frame ONCE over a
BLOCK of lanes; lane-multiplying ops append lanes to the block
mid-flight (duplicate the lane's live values, set the split value per
alternative). Control flow stays native Rust straight-line/loops -
exactly what the scalar probe compiles - with a uniformity assert at
each of the 26 branch sites.

## Representation

- `AV` (per-lane abstract value, Copy): `Num(P8) | Ival(P8, P8) |
  Bool(bool) | UBool | Str(u32) | Nil | Ptr(u32) | NilPtr`.
  Semantics ported from op.rs/flow.rs (see "Ported semantics" below).
- Column `Col`: `Uniform(AV)` | `Vec<AV>` (one per lane). Uniform is the
  common case (constants, pointers, shape-uniform fields) and is what
  keeps the block cheap; ops preserve uniformity when both inputs are
  uniform.
- Heap: the STRUCTURE (Obj/Arr/Clo/Bi cells, pointer topology) is
  UNIFORM across lanes by the shape premise - kept once, exactly the
  scalar probe's `Vec<Cell>`. Only `Cell::Val` cells become columns:
  `Vec<Col>` indexed by the same cell id. get_field/get_index/
  get_global logic is IDENTICAL to the scalar probe (walks shared
  structure); loads/stores hit the column table. Structural mutation
  mid-frame (add/del on objects) is uniform too (same op for all lanes)
  - the one per-lane structural difference (death removing the player)
  exits via deopt, below.
- Locals in generated code: `Col` values in Rust variables (the existing
  generator's shape, with `V` -> `Col`).

## Lane splits

`expand`, `__split_by_flr`, `__split_at` return per-lane VALUES after
widening the block: for each lane whose input admits k > 1 alternatives,
append k-1 duplicate lanes (copy every live column's entry; the split
result differs per copy). Lane order: original lanes first, appended
in (lane, alternative) order - batching invariance requires the
RESULT SET be order-independent, and dedup at the boundary makes it so.
Duplicating "every live column" = the value-cell columns (heap) plus
the live locals; locals are visible to the generated code, so the
generator emits the duplication call with the list of live local ids at
the split site (the kill instructions already carry liveness).
NOTE v0 simplification: rather than tracking live locals per site,
v0 keeps ALL numbered locals of the current function in a registry the
runtime can reach (a `Vec<*mut Col>` built per function entry is unsafe
and ugly; instead v0 generates: split returns a lane-map plan, and the
generated code applies `apply_split(&mut colX)` to each local in scope
- the generator knows the set statically). Cost is bounded by K (live
locals), which allocate_slots already minimized (~30-50).

## Uniform branches and deopt

At each conditional branch: evaluate the condition column; if uniform,
take the edge natively. If NOT uniform (expected only on the death
frames - kill_player is the one genuinely lane-divergent event left in
the shape), partition: lanes on the majority... NO - v0 keeps it
simpler and fully sound: on a non-uniform branch condition, the WHOLE
BLOCK deopts - every lane of the block is re-run by the interpreter for
this frame (the block's frame-start rows are still in hand). Death is
rare per (state, frame); losing the block's compiled speed on those
frames is noise, and there is no partial-filter machinery to get wrong.
v1 refinement if profiling demands: filter the minority lanes out
(generator emits per-site live-local filter calls) and continue.

Same policy for per-lane premise-assert failures (assert_true on a
non-uniform bool column, guard mismatches): whole-block deopt for the
frame. The deopt lane-set and trigger site are logged - the frequency
IS the measurement that decides whether v1 filtering is worth building.

## Frame boundary

Port of the interpreter's boundary, room-(1,0)-relevant subset first
(abstraction.rs reads done, 2026-08-18):

1. rem widening per precision (Bits(0): rem.x/y -> [-0.5, 0.5-eps]
   closed; Bits(k): floor-aligned bucket; Exact: no-op). Cells found by
   the mark walk (player -> rem -> x/y) over the shared structure heap.
2. spd widening per precision (Exact default; WidthLog2(w) buckets).
3. dash_effect_time clamp at 0 from below.
4. Timer globals frames/seconds/minutes/deaths pinned to Num(0).
5. Fruit off/bob widenings: no fruit in room (1,0); port when the
   engine goes to (2,0)/(0,0) shapes (the code reads are in
   abstraction.rs:590-704, 748-818).
6. Straddle canonicalization (split_rem_straddles): boundary lanes
   whose rem interval spans 2 buckets split into one lane per bucket
   (max 2 by invariant, asserted).

Then row extraction + dedup: a lane's row = the tuple of its
value-column entries in cell-id order (uniform structure means cell-id
order is canonical per shape) + nothing else (locals dead at boundary;
prints must be empty/uniform - asserted). Hash 128-bit, dedup in a
FxHashMap per frame; survivors are the next frame's block.

IMPORTANT identity caveat: the interpreter's row identity canonicalizes
heap ids via its own layout; ours uses the fixed cell-id order of the
shared structure. Both collapse exactly value-tuples over the same
cell population IF the structure heap is per-shape-constant and cell
population matches. Gate 2 (row-set equality) is what proves this,
via reconstructing States and hashing with the interpreter's own path.

## Oracles (the meaning lives here)

- Gate 1: per-frame lane counts vs `rewrite bench` on room (1,0) level
  0 (Bits(0)), f1..f30, exact match.
- Gate 2: row-SET equality vs the interpreter on sampled frames
  (reconstruct native rows as interpreter States, canonical-hash with
  the interpreter's own code).
- Batching invariance: block-partition invariance of the result row
  set (the simdcheck doctrine).
- The concrete probe's oracle stays green (scalar path untouched).

## Code layout

- native-probe/src/runtime2.rs: AV, Col, block, columnar ops, splits,
  boundary, dedup. Ports carry file:line pointers to op.rs/flow.rs/
  abstraction.rs like runtime.rs does.
- transpile.rs grows a `--columns` mode emitting gen2.rs against
  runtime2 (same walker, different value type + the split/branch
  emission differences). The scalar gen.rs stays as-is (it is the
  hex-exact oracle instrument).
- Init: run __init + set_buttons ONCE via the scalar probe, convert the
  concrete heap to (structure, columns at width 1) at frame 0 - no
  columnar __init needed.

## Why not DFS-with-tape (the older stage-2 sketch)

The pre-zero-divergence sketch (native-probe.md "Stage 2 design") had
choice points as tape forks with replay from a frame-start snapshot.
Replay costs (#leaves x full frame) and the snapshot machinery vanish
entirely under the columnar model, which the zero-divergence program
makes legal. The tape sketch stays the fallback for shapes/programs
with real divergence; for compiled shapes the divergence is deopt by
definition.

## Roofline expectations (to be measured, not assumed)

Scalar concrete probe: 2922 ns/frame at width 1. The columnar engine
amortizes all uniform work across the block; per-lane incremental cost
is the vector-lane loops over ~30-50 live columns. The gap picture
(goal 4) compares: auto columnar vs hand-tuned SIMD kernel of the
hottest column ops vs the memory-bandwidth roofline of touching
(lanes x live columns x 8-16 B) per frame.

## Implementation plan v0 (decided 2026-08-18, in progress)

ONE generated code path for both engines: transpile emits
`fn f_N<E: Engine>(rt: &mut E, caps: &[E::V], args: &[E::V]) -> E::V`.
`Engine` trait (defined in runtime.rs) with assoc type `V: Copy`; the
scalar `Rt` implements it with `V = V` (methods unchanged, moved into
the impl). The columnar `Rt2` (runtime2.rs) implements it with
`V = ColId` - a Copy handle into a column arena (`Vec<Option<ColData>>`
+ free list). Generated code additionally emits `rt.kill(&[ids])` at
IR `kill` instructions: scalar no-op, columnar frees the slots - this
is what bounds the live-column set that lane-splits must widen.

Lane split cost = O(live varying columns) per appended lane; Uniform
columns append for free. Heap value cells: `Vec<Col>` beside the shared
structure heap (282 cells at the room-(1,0) boundary - measured).

Gate 1 reference (rewrite bench --frames 30, level 0, this build):
f1..f23 = 1 lane (spawn anim), f24=24, f25=204, f26=878, f27=2864,
f28=7260, f29=15250, f30=27024. Interpreter: 0.22s total, 8.2 us/lane,
282 heap cells, /tmp/bench-ref30.log.
No deaths before ~f58 ("states start dying" - bench --deopt doc), so
v0 needs no deopt path for the gate; non-uniform branch = loud panic.

Driver (--abstract N in probe main.rs): scalar __init once -> convert
concrete heap to (structure, width-1 columns) -> per frame: run
f___frame columnar; boundary = rem widening Bits(0) [-0.5,0.5-eps]
closed + dash_effect_time clamp + timer pins (frames/seconds/minutes/
deaths -> 0) + straddle split (no-op at Bits(0)); extract rows (value
columns in cell-id order), dedup 128-bit, next block = survivors.
Buttons: frame entry stores UBool via __reset_button_states - the
expands fork them per lane (no set_buttons call - matches the search).

## v0 RESULTS (2026-08-18, overnight): gate 1 EXACT, the gap picture

Gate 1 (per-frame lane counts vs `rewrite bench --frames 30`, room (1,0)
level 0): **EXACT for all 30 frames** (1x23, 24, 204, 878, 2864, 7260,
15250, 27024), spawn transition, dash/freeze mixing and the btn fan-out
included. The scalar concrete oracle stays hex-identical (f340) at
1933 ns/frame after the same runtime changes.

What it took beyond the design doc:
- TWO genuinely lane-divergent gates exist that the f35 concrete branch
  census could not see, both deciding on FRAME-START data: the
  update-side freeze gate (`freeze > 0`; pm1's cell) and the
  `spd.x ~= 0 or spd.y ~= 0` moving gate at anonymous_61 __entry
  (obj.move runs before update touches spd). Handled by frame-start
  pre-partitioning (freeze value, per-object moving bit) plus a GENERIC
  reactive mechanism: a non-uniform branch throws `SplitReq` with the
  per-origin condition truth, the driver partitions the frame-start
  block and reruns both sides. Divergence WITHIN one origin lane would
  panic (=> deopt work); none observed through f30.
- The zero-divergence claim is therefore STATE-WINDOW-DEPENDENT: the
  census certifies (state, input)-uniformity at its window, not
  cross-state uniformity inside an abstract block. The morning docs
  should say so.

Performance ladder measured at f30 (27,024 boundary lanes, ~1.7M
offered after the 64x btn fan-out), single frame:
- naive columnar (per-op Vec allocs, one block): 6.6 s
- + alloc-free splits, buffer pool, memcpy widening, mimalloc: 6.4 s
- + CHUNKED execution, 64 input lanes/chunk (cache blocking - the
  interpreter's fragments average ~300 lanes for the same reason): 2.4 s
- + parallel chunks (30 threads): 1.26 s wall; 30 frames in 2.16 s.
Reference: the interpreter runs the same frame in 0.08 s single-core
(8.2 us/boundary-lane; 30 frames in 0.22 s).

Honest read (single-core): columnar v0 is ~89 us/boundary-lane =
~1.4 us/offered-lane - already faster per offered lane than the scalar
concrete probe (2.0 us) while computing ABSTRACT semantics, but ~11x
slower than the tuned interpreter per boundary lane. Profile: the
remaining cost is per-lane AV enum dispatch in map1/map2 (~3-7 ns per
lane-op x ~200-400 varying ops) - branchy tag matching that the
compiler cannot vectorize.

The ladder to interpreter-parity and past it (next unit, in order):
1. TYPED columns: a column knows "all Num" / "all Ival" / "all Bool"
   (the shape premises make most columns monomorphic); ops become
   branchless i32-array loops, 4 B/lane instead of 16, auto-SIMD.
   Expected 4-10x on the varying-op cost.
2. FUSED loops: the transpiler emits one lane-loop per straight-line
   run of arithmetic (registers, no intermediate materialization) -
   kills the per-op read/write traffic that the interpreter pays too.
   This is where "beat the interpreter" lives.
3. Chunk-level uniform hoisting: the ~19k uniform ops re-execute per
   chunk (422 chunks x 5 us = 2 ms - currently noise, matters later).

Bytes/row today (goal-5 input): 282 canonical cells/row; varying
columns only in Col::V - the dense-row projection uses the varying
count (~30-80 at f30), i.e. ~0.5-1.3 KB/row at AV width, ~120-320 B
typed. Boundary + dedup + compaction cost: included in the numbers
above (the canonical BFS renumbering runs per chunk per frame).
