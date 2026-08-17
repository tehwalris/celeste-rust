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

## The 300m projection (goal 5 of the overnight brief; written 2026-08-18)

Room (2,0) level-0 measured wall (BENCHMARK_DATA.md): frontier 62.9M
lanes at f070, 347.9 s/frame, 67.6 GB RSS, growth 1.09-1.11x/frame,
horizon 95; f001..f070 cost 2,773 s, peak 76 GB. The S(1px/2px) rungs
both OOM'd their humps under the interpreter (2px: 57.6 GB at f36 with
only 1.66M lanes - the cost is INTRA-frame interpreter machinery, not
boundary storage).

What the columnar engine changes, from tonight's measured numbers
(~1.4 us/offered-lane v0, ~5x more expected from typed columns; row
storage = varying columns only, ~40-80 cols x 4-16 B):

- MEMORY: intra-frame peak becomes chunk-local (MBs per thread) +
  frontier rows. The 2px S-rung hump (1.66M lanes) is ~0.5-1 GB under
  the engine vs the 57.6 GB OOM. Boundary storage at the (2,0) f070
  frontier: ~10-20 GB typed vs 67.6 GB interpreter RSS. The OOM wall
  disappears for every campaign that fits ~100M lanes.
- TIME: v0 already ~10x the interpreter per offered lane at width
  (and parallel chunks scale); typed columns target another ~5x.
- BUT the level-0 EXACT run stays impossible: 1.10x/frame growth means
  ~680M lanes by f95 - the lane count itself is the wall ("this is the
  number of lanes", BENCHMARK_DATA). No engine fixes an exponential.

GO/NO-GO verdict: GO, engine-first, S-rung second. The decision is NOT
"engine vs rem-banding" - it is: the engine reprices every rung of the
existing S-rung ladder (the algorithmic answer that already exists and
OOM'd only on interpreter overhead). Path: port the fruit widenings
(abstraction.rs:590-704, 748-818) + gate 2 + the death-frame deopt,
then re-run the S-rung campaign natively. rem-banding stays parked
unless the repriced S-rung still cannot reach h=95.

## Typed-column plan (the next unit; the 5-10x)

The profile says the remaining single-core cost is per-lane AV tag
dispatch. Plan, in order, gate 1 re-run after each step:
1. `Col::N(Vec<P8>)` - an all-Num column stored raw (4 B/lane).
   Constructors detect it (buf fill already single-site); `at()` maps.
   Fast paths in map2/map1 for N x N / N x U(Num): arith and compares
   become branchless i32 loops (P8 add/sub are i32 wrapping ops; mul is
   (i64 product) >> 16 - all auto-vectorizable).
2. `Col::Bm(BitVec)` for all-Bool columns if select/compare masks still
   show in the profile.
3. Fused loops need the transpiler (emit one lane-loop per straight-line
   arithmetic run); only reach for it if 1+2 leave a >2x gap to the
   interpreter per boundary lane.

## Typed columns, measured (2026-08-18, late)

Col::N (raw P8) + Col::I (raw interval pairs) with numeric/interval
fast paths in add/sub/compares, compression at boundary + merges.
Gate 1 stays exact. Serial f30: 2.4 -> 2.12 -> 2.22 s (Num helped 12%,
Ival neutral). Verdict: blind type-specialization has hit its ceiling -
the remaining ~2s is spread across compares-to-AV-bool columns, select,
per-lane builtin closures (tile_flag_at), boundary hashing and the
long tail of small ops at width. Next lever needs DATA, not guesses:

1. A per-op time census inside the engine (op kind x column kinds x
   width buckets - the op_census doctrine, ported).
2. Then either fused loops (transpiler emits one lane-loop per
   straight-line arithmetic run) or targeted fixes at whatever the
   census names.

Wall clock at f30: 1.24 s on 30 cores (poor scaling ~1.7x - round-robin
chunk queues without stealing + serial merge; also unmeasured
bandwidth ceiling). 30 frames end-to-end: 2.13 s wall.

## Per-op census result (2026-08-18, ~06:30)

`CELESTE_OP_CENSUS=1` on the serial 30-frame run (3.6 s total):

    widen          1740.6 ms   1572 calls   <- 48% of everything
    map2_generic    526.6 ms  50759 calls
    select          425.1 ms  73320 calls
    load             33.7 ms  81578 calls
    store            32.9 ms  28406 calls
    map1_generic     16.9 ms  16429 calls

The dominator is WIDEN - the lane-append at expand/split sites copies
every LIVE VARYING column (hundreds mid-frame) at the current width.
Eager frame-start button expansion was tried to make widens hit an
empty arena and is MEASURED OUT: the whole frame then runs 64x wide
from instruction 0 (f30 serial 2.2 -> 7.0 s; reverted, fn kept dead
with a note). The interpreter's lazy expansion wins for the same
reason.

Named next steps for the widen cost, in expected-win order:
1. COW / lane-indirection columns: a split appends a lane MAP, not
   data; columns materialize lazily on first write at the new width.
   Turns the 6 doublings into O(1) each; ops read through the map.
2. Shrink the live-varying set at expand sites (kill_dead is block-
   grained; expand-site liveness could be much tighter - measure how
   many varying columns are live at each of the 20 expand sites).
3. Fused regions (transpiler) remain the map2/select answer (~950 ms
   combined) once widen is gone.

## Gate 1 extended to f40 (2026-08-18, ~07:00)

Interpreter vs columnar, f31..f40: 44566, 66206, 92713, 132153,
187859, 269059, 365029, 498541, 673479, 902280 - IDENTICAL every
frame. 902k boundary lanes at f40 with the dash straddle splits
active. The engine's semantics hold at depth.

## COW/lane-indirection column design (the widen fix, ready to build)

Physical duplication at splits is the 48%. Replace with lazy width:

- Rt2 keeps `history: Vec<(width_before, map)` per frame, where `map`
  maps CURRENT lanes to the lane space of that width. On widen(srcs):
  extend every existing history map by `map[src]` per appended lane,
  then push `(old_width, identity ++ srcs)`. Cost O(epochs x appended)
  - no column is touched.
- A varying column keeps its creation-time length. `len == width` =>
  direct. `len < width` => stale: lane i reads `data[map_i]` where the
  map is found by matching `len` against history widths (widths
  strictly increase per frame, so len -> epoch is unique; <=32 entries,
  binary search).
- Whole-column ops resolve a stale column once into the op loop (no
  write-back needed: SSA values are mostly read once; measure).
- boundary/retain/slice/merge/hash materialize stale columns first
  (bounded by the live set).
- begin_frame clears history (boundary compaction materializes
  everything live).

Expected: kills the 1.74s widen almost entirely; appended-lane cost
becomes O(epochs) bookkeeping + lazy copies only for columns actually
read across a split.

## COW columns LANDED (2026-08-18, ~07:30)

The design above, built and gated. Gate 1 EXACT through f40 (902,280
lanes). Census after: widen 1740 ms -> 9.8 ms; the profile is now the
ops themselves (select 650 ms, map2 466 ms serial-with-census).

| f30 (27k lanes) | before COW | after COW |
|---|---|---|
| serial | 2.1 s | 1.21 s |
| 30 cores | 1.22 s | 0.144 s |
| 30 frames wall | 2.10 s | 0.29 s |

Parallel scaling jumped 1.7x -> 8.5x - the physical widen was the
scaling bottleneck (memory-bound duplication in every worker).
Interpreter reference: f30 0.08-0.10 s single-core. The engine's WALL
CLOCK at f30 now matches the interpreter's single core.

At depth the boundary becomes the frontier: f40 = 14.2 s parallel vs
interpreter 0.99 s - 902k lanes x 282 cells x 2 hashers + per-chunk
uniform re-execution + k-way merge. That is goal-7 territory
(dedup/merge at roofline: hash only varying columns, radix passes,
parallel merge) plus chunk-size scaling with width. Both named, not
started.

Remaining ladder: (1) boundary/dedup roofline pass, (2) select/map2
fusion or typed-bool columns, (3) fused loops via the transpiler.

## Final measurements of the night (~08:00)

- Order-independent row keys (per-cell mixes summed per lane; uniform
  cells fold once per block - split-independent identity, the
  interpreter's own order-independent-key doctrine). Counts unchanged
  through f40 at 902k rows - strong collision evidence. Depth timing
  unchanged: hashing was NOT the depth bottleneck.
- CHUNK re-sweep at f40 under COW: 64 -> 14.2s, 256 -> 16.5s,
  1024 -> 20.0s, 4096 -> 25.5s. Cache blocking still rules; per-chunk
  overhead is not dominant. The remaining depth cost sits in the
  serial merge/dedup section (k-way merge of ~10k sub-blocks + a
  serial 902k-entry dedup map) and in per-offered-lane op work -
  both named for the goal-7 pass (parallel merge, radix dedup).

## Select fast paths (2026-08-18, ~08:30, last unit of the night)

Typed select (varying-bool cond over Num/interval sides -> raw column):
f40 14.3 -> 10.1 s, 40 frames 43.7 -> 31.5 s, serial 30f 2.0 -> 0.96 s
(census on). The win CASCADES: selects now emit typed columns, so
downstream ops take the bin_num fast paths (map2_generic calls halved).
Counts exact through f40 re-verified twice. Census now: select 372 ms
(non-numeric remainder), map2 177 ms, everything else < 40 ms.

Sharded parallel cross-block dedup also landed (neutral today, removes
a serial ceiling). Remaining depth cost = worker-phase op work; the
next levers stay as prioritized in the morning report.

## The one-frame dev-loop bench (2026-08-18, landed)

    native-probe --abstract-bench ~/celeste-checkpoints/room10-newlua-bench 35 --reps 5

Loads the REAL f35 boundary states (187,859 lanes, 40 pm1/shape
blocks) via a new vectorized State -> Rt2 importer (intervals -> Col::I,
vector numbers -> Col::N; reachability-based, drops program-unnamed
fields like the scalar importer), runs ONE abstract frame repeatedly,
and self-checks by chasing to the next existing checkpoint: 2 frames
land EXACTLY on the interpreter's f37 = 365,029 lanes - the engine is
exact on real mid-campaign states, not just from-scratch runs.

Numbers (30 cores): one frame = 2.0 s, 10.6 us/input-lane - 3x slower
per lane than the from-scratch gate-1 states, because real states carry
live dash/freeze/interval variety: more varying columns, so the select
fast path misses more (census: select 920k executions/frame dominates,
then load - which clones a full column per call). Dev loop total: ~12 s
per iteration after a runtime-only rebuild.

This is the baseline the tile kernel is measured against. Design note
(delegated call): the bench oracle compares LANE COUNTS at the chased
checkpoint; row-set equality remains gate 2 work.

## Tile-kernel pre-work verified (2026-08-18)

1. STRAIGHT-LINE: the branch census now classifies executed sites by
   sequence hash - all 26 executed sites run EXACTLY ONCE per frame on
   the real f35 states (64 inputs x 256 lanes). No loops, no
   multi-visits in the steady-state frame; the room-load path (which
   does loop) stays off the kernel. The straight-line SIMD emission
   premise is verified, not assumed.
2. STRADDLE RATE: 5 splits per block max at f35 real states - the
   counter-replay path is rare, as designed.

Both green-light the emission plan: straight-line tree with per-variant
button constants, SIMD across row tiles, counter-replay for straddles,
sticky fail-mask -> reference-path rerun.

## Rt3: the tile runtime (design, 2026-08-18 mid-morning; building now)

The third Engine impl over the SAME generated program. One tile = a
fixed small set of input rows (TILE = 16 to start). Key differences
from Rt2:

- Values live in a bump arena of FIXED-SIZE entries ([AV; TILE] or
  uniform), reset per frame - no heap Vecs, no allocation in the frame,
  L1-resident working set.
- BUTTONS ARE CONCRETE: the driver loops over the 64 input variants
  OUTSIDE the kernel and writes concrete bools into the button cells
  before f_frame. In-frame `expand` sees Bool -> identity. Tile width
  therefore NEVER changes mid-frame (no widen/COW/history machinery at
  all). This is the "fan-out as outer structure" model; v1 pays physics
  64x (no trunk sharing yet) - the specialization tree recovers that
  later, and v1's measurement prices exactly how much the trunk is
  worth.
- Straddle splits (__split_by_flr / __split_at) use the COUNTER-REPLAY:
  the runtime carries a choice tape; pass P gives site j alternative
  P_j; a lane is VALID on pass P iff P_j < k_j(lane) for every site
  (cartesian enumeration, lanes mask out on passes beyond their own
  alternative count). Passes beyond the first only happen for tiles
  containing straddling lanes (measured: rare).
- Guards: sticky per-lane fail mask (no panics, no ctx strings on this
  path); failed lanes rerun on Rt2 (the reference path) for the real
  message / deopt semantics.
- Boundary: per-lane widenings inlined at tile exit; rows emitted into
  the shared dedup (the same row-key scheme as Rt2 so cross-checking is
  trivial).

Oracle: the one-frame bench - lanes-out must equal Rt2's / the
interpreter's at the chased checkpoint, per run. Baseline to beat:
2.0 s / 10.6 us per input lane on real f35 states.

v1 deliberately defers: trunk-sharing specialization tree, typed tile
columns / SIMD, const-generic variant folding. Each is a measured step
on the same bench afterwards.

## Rt3 v1 measured (2026-08-18, midday)

Built: runtime3.rs (the tile Engine impl: 16-lane tiles, concrete
buttons per variant, counter-replay tape, TileBail -> Rt2 fallback with
bail-site aggregation), tile-mode chunk executor (CELESTE_TILE=1),
chunk accumulator (structure-fingerprint checked, no per-variant Rt2
materialization), TCol slimmed to 16 B (payloads in a tile pool - the
inline [AV; TILE] enum was 260 B and cloning dominated: 37 -> 12 s).

Result on the one-frame bench (real f35 states, EXACT 269,059 out,
fallbacks included via 214 non-uniform-branch bails):

    Rt2 (columnar, shares uniform work across fan-out):  2.0 s
    Rt3 v1 (tiles, NO sharing, NO folding):             10.9 s
    per OFFERED lane: Rt3 0.9 us vs Rt2 1.4 us

Profile is pure compute now (select 29%, add 9%, av arithmetic, zero
alloc/clone overhead). Reading: the tile model's per-lane cost already
beats columnar, but v1 pays the 64x fan-out at full price while Rt2
amortizes uniform work via Col::U. The two designed remedies are
exactly the deferred steps, now priced:
  1. const-specialized variants (buttons fold, tails shrink) - needs
     slot compilation so LLVM sees through cells: the row-struct
     emission step, NEXT.
  2. trunk sharing (physics once per tile, 64 tails) - the
     specialization-tree emission, after 1.
Plus typed tiles (select's 29% is per-lane AV tag dispatch).

## Where the 58 us/input-lane sits (roofline decomposition, for the record)

58 us = 64 (unshared fan-out) x 0.9 us/offered-lane, and 0.9 us itself
is ~5-10x typed-SIMD headroom (AV tag dispatch) on top of a frame that
specialization shrinks. Remedies map 1:1: trunk sharing + const-folded
variants attack the 64x; typed tiles attack the tag dispatch; folding
shrinks the frame. Target: a few hundred ns per input lane. The
per-OFFERED-lane number (0.9 vs Rt2's 1.4 vs scalar probe's 1.9) is
the evidence the tile model is the right substrate.

## Slot compilation (next unit, in progress)

Goal: heap accesses on the kernel path become struct-field/array ops
LLVM can see through (SROA + const-fold), which is what makes
per-variant button constants actually fold. Approach: per-shape SLOT
BINDING - the ~178 single-receiver sites' access paths are static; at
block-bind time walk each path once to map site -> cell; the kernel
then reads/writes a dense slot array instead of pointer-chasing
structure. Guards stay (site's receiver must match the bound cell -
the census doctrine). Emission: a transpiler pass that rewrites
get_field/get_index+load/store pairs on bound sites into slot ops.

## Variant const-folding: scaffold landed, win blocked on slots (midday)

Built: transpiler resolves all 20 expand sites' buttons statically
(def-chain walk to the k_* global) and emits `rt.expand_btn::<K>`;
`Rt3<const BTN>` folds the specialized bits; monomorphic dispatch.
Results:
- Full 64-way monomorphization EXPLODES compile time (>10 min, killed).
  4-way (jump+dash bits only) compiles in ~2 min.
- Arena removal (Engine::V = TCol by value): 10.9 -> 8.0 s. Real win.
- Jump/dash const variants: NEUTRAL (8.2 s). Diagnosis: the constants
  die at the first store/load pair - the program's dataflow runs
  through heap cell columns (self.cols[p]), opaque to LLVM. SSA-only
  folding cannot reach past a single instruction.

CONCLUSION (the arc of the day converges): SLOT COMPILATION is
strictly the critical path for both remaining factors - the ~178
single-receiver cells must become compiler-visible locals/struct
fields in the generated kernel (guarded, census doctrine) before
per-variant folding or trunk sharing can pay. Everything else is
scaffolding already in place: bench, tile runtime, variant dispatch,
counter-replay, fallback, oracles.

## Slot compilation: full design (ready to execute)

The unit that unlocks both remaining factors. A cell is COMPILER-VISIBLE
when every access to it goes through a constant-indexed slot, so LLVM
tracks values across the store/load pairs that currently kill folding.

1. SITE -> CELL map (per shape): extend the gap census to record each
   get_field/get_index site's RESULT cell (it already records the
   receiver); dump `site-slots-<shape>.json`: groups of sites sharing a
   result cell -> slot id, plus the access path (global -> fields) for
   bind-time verification.
2. TRANSPILER (`--site-slots FILE`): eligibility analysis - a cell is
   slot-compiled iff ALL its loads/stores flow through def-chain-visible
   bound sites (no escaped pointers, no phis); partial compilation of a
   cell is INCOHERENT (slots and cols would diverge) and must not
   happen. Emit for eligible sites:
       l = get_field(...); v = load(l)   =>  v = rt.slot_get::<K>()
       store(l, x)                       =>  rt.slot_set::<K>(x)
   (K = constant slot index). Report the eligible/ineligible counts -
   the ineligible remainder stays on the generic path and the two
   worlds stay coherent because ineligible cells never appear in slots.
3. RT3: `slots: [TCol; N_SLOTS]` (a plain struct array; constant
   indices => SROA). Bind at from_rt2: walk each slot's access path,
   copy cell -> slot; at tile exit write slots back to cells before
   append_into. Guards: v1 puts the per-site receiver check behind a
   debug/verify build flag (delegated design call - the doctrine's
   loud-check lives in the bench oracle + Rt2 fallback in release).
4. Then re-measure the jump/dash const variants (they should now fold
   through), then the 64-way question (compile time vs win - consider a
   separate codegen crate so monomorphization parallelizes), then trunk
   sharing, then typed slots (the [TCol] array becomes typed fields).

State of scaffolding (all landed and exact on the bench): one-frame
bench with chased-checkpoint oracle; Rt3 tiles with by-value TCol;
counter-replay; TileBail->Rt2 fallback with site aggregation; variant
dispatch + static button resolution; chunk accumulator.

## Slot binding subtlety (found while building step 1; groundwork landed)

The census records cells in the IMPORT numbering; Rt3 tiles are built
from post-boundary blocks in the CANONICAL numbering - the two differ
(global_env name-order DFS vs global-index BFS). So the slot map cannot
carry raw cell ids. Two options for the next session:
  (a) bind by ACCESS PATH: census emits (receiver cell, field/index,
      result cell) triples per site; reconstruct root->cell paths
      (get_index sites must also record the concrete index); Rt3 walks
      the path once per block at bind time. Robust, slightly more code.
  (b) prove/assert numbering stability: make import_block use the SAME
      order as the boundary BFS (global-index BFS), so import ids ==
      canonical ids for boundary states, and the census can emit raw
      canonical ids. Less code, one invariant to hold (assert both
      numberings agree on a reference state).
Recommendation: (b) first - aligning import_block's traversal with the
boundary BFS is a small change and makes ids stable everywhere; keep
(a) as fallback if any state disagrees.

Landed groundwork: scalar runtime now logs per-site RESULT cells
(result_log, same lattice as the receiver log) - the input for the
site->slot grouping either way.

## Canonical-id stability: option (b) LANDED and verified

import_block now traverses in the boundary BFS order (globals by gen
index, breadth-first; the old name-order DFS importer deleted). The
bench asserts `is_canonical_order()` on every imported block - PASSES
on all 40 real f35 blocks, both engine modes still exact. Slot maps
can therefore carry raw canonical cell ids; no path walking needed.

Slot-compilation prerequisites now ALL in place:
  1. per-site result-cell logging (runtime.rs result_log)   [landed]
  2. canonical-id stability                                  [landed+verified]
  3. design + eligibility analysis + emission plan           [written]
Next session: census dump (--emit-slots), transpiler --site-slots
eligibility + slot_get/slot_set emission, Rt3 slots array, re-measure
the const variants.

One more numbering note for the census dump: run_census uses the
SCALAR import_lane, which still numbers cells in the old name-order
DFS - align it to the BFS order too (same restructure as import_block)
OR run the result-cell census through the vectorized path, so the
emitted slot ids are canonical. Small either way; do it first.

## Slot compilation LANDED end-to-end (2026-08-17)

The full chain is built and gated:
1. `--emit-slots FILE` on the census (scalar importer restructured to
   the canonical BFS, cross-checked against import_block on real data:
   282 cells agree). Room (1,0) f35: 47 slots over 175 of 178
   single-receiver sites; 3 multi-result; a TAINT set excludes any
   cell a multi-result site ever produced (0 aliased here).
2. Transpiler `--site-slots FILE`: whole-program escape analysis - a
   bound site's result local may be used ONLY as Load source / Store
   target / Kill hint; anything else escapes the pointer and reverts
   the CELL (all its sites). 28 of 47 slots eligible, 102 sites
   compiled. The 73 reverted sites are dominated by the masked-store
   rewrites (`store(select(mask, a, b), v)` - the pointer feeds a
   Select). v2 extension: masked slot_set for select-of-two-slots.
   Emission: `if E::HAS_SLOTS { rt.slot_get(K) } else { generic }` -
   the branch is a per-monomorphization constant, so Rt/Rt2 keep the
   generic path at zero cost and site identity is (fn, iid), stable
   across regenerations.
3. Rt3: `slots: [TCol; gen::N_SLOTS]`, bind after set_buttons, write
   back before structure_fp/append. CELESTE_SLOT_GUARD=1 = the runtime
   guard for what the escape analysis cannot prove (unreached-site
   aliasing): slot_set mirrors into cols, slot_get compares - an
   aliased write desyncs them and asserts. PASSES on the whole bench
   (~3% cost). Exactness: 269,059 out + f37 chase 365,029, both exact.

Result: NEUTRAL on time (8.16 s vs 8.0 s baseline). The profile says
why, and it REVISES yesterday's attribution for the neutral const
variants: the hot Engine ops are NOT INLINED into the generated frame
(`select` 19%, `av_addsub` 17%, `op_add` 8.5%, `av_cmp` 7% - all
standalone symbols; get_field is <2%). Out-of-line ops mean NO
constant - from slots or from the const-button variants - ever reaches
the op bodies; store/load opacity was the second-order effect. The
pointer chase the slots removed was never the cost.

Consequence: inline-the-ops is the gate to ALL folding wins. First
attempt (#[inline(always)] on 19 Rt3 op methods) blew compile time
past 10 min for the 4-variant build - the huge fused frame fn x 4
variants x fully inlined per-lane loops is too much for one codegen
unit. Options, in order: (a) fewer inlined ops (select + add/cmp
only), (b) one variant (drop Rt3<BTN> monomorphization, keep dynamic
buttons) with full inlining - measures the inlining win in isolation,
(c) separate codegen crate with per-variant compilation units in
parallel. Doing (a)/(b) to size the prize before paying (c).

## Dynamic-expand tiles (CELESTE_TILE=2): trunk sharing built (2026-08-17)

The fan-out moves INSIDE the tile - Philippe's "batch of rows in, all
inputs, bigger batch out" model:
- Tile = ONE boundary row; the lane axis carries the input variants.
  Every column starts uniform (width 1), so the whole pre-input trunk
  (obj.move physics, spikes, collision - the heavy pixel loops run
  BEFORE update reads buttons) executes as O(1) uniform ops, shared by
  all 64 variants.
- `expand` on a uniform UBool doubles the lane axis in-tile: new lane
  j descends from parent j>>1 with button value j&1. Old tiles are
  NEVER rewritten - each tile records log2(width) at creation and
  `tat` projects `lane >> (log_w - tile_log[ix])`. No COW, no remap.
- TILE 16 -> 64 (the fan-out needs 64; concrete-button mode just runs
  wider row tiles). Straddle splits keep the counter tape; valid bits
  duplicate on expand. Re-expanded buttons (per-lane Bool tiles) pass
  through; a second fan-out of the same button would exceed 64 lanes
  and bail to Rt2 - sound, not silent.
- Mode sentinel Rt3<0xFF>: expand_btn const-folds back to the dynamic
  expand; no set_buttons, no per-variant monomorphization on this path.
- Exactness argument: fan-out lane ORDER differs from Rt2's appends,
  but the boundary compares canonical row SETS (order-independent
  keys + dedup), and the bench oracle gates counts.

## Mode 2 RESULTS: trunk sharing is the win (2026-08-17)

| engine (bench: 187,859 lanes in, one frame, 30 cores) | time | ns/in-lane |
|---|---|---|
| Rt2 columnar (reference)                | 2.0 s   | 10.6k |
| Rt3 concrete-button tiles (TILE=1 mode) | 8.16 s  | 43.4k |
| Rt3 dynamic-expand tiles (TILE=2 mode)  | 1.55 s  | 8.3k  |

5.3x over the variant tiles, faster than Rt2, EXACT (269,059 +
f37 chase), zero bails (one row per tile = no cross-row divergence, so
the fallback never fires), guard passes (+2%).

Bug found by gate 1 from scratch and fixed: SLOT BINDING IS PER-SHAPE.
Frame 1's spawn-shape block bound steady-state canonical cell ids -
coherently wrong (the guard cannot see it; slots and cols agreed).
Fix: the census dump carries the canonical shape hash; gen.rs emits
SLOT_SHAPE; import_block stamps shape_hash (BFS = canonical); both
tile drivers deopt to Rt2 off-shape. This also scopes the
zero-divergence premise to its certified window - the tile kernel is a
steady-shape fast path BY CONSTRUCTION now. Second bug in the same
sweep: imported bench blocks had shape_hash unset, silently deopting
every chunk (mode 2 == Rt2 timing was the tell); import_block now
computes it.

Next levers, in order of expected value:
1. The mode-2 profile (redo it): with the trunk uniform, the residual
   is post-fan-out vectorized AV ops - typed tiles (Num-only [P8;64]
   panes, real SIMD) attack exactly that.
2. Slots are now load-bearing scaffolding (neutral on time but the
   shape-scoped binding + guard machinery is what typed slots build
   on). Typed slots = the [TCol] array becomes typed fields.
3. Batch rows per tile at LOW fan-out depth: 1 row/tile wastes lanes
   1..63 during the trunk (op dispatch amortizes over 1 row). A
   16-row tile that expands 16->... exceeds 64 - needs valid-masked
   partial fan-out or TILE=1024 panes; measure before building.
4. Drop the 4 Rt3<BTN> const variants (mode 1 + variants lost to mode
   2; keep mode 1 as a control until mode 2 has survived a campaign).

## Evening ladder result + the pivot (2026-08-17, end of day)

One-frame bench, all steps EXACT + guard + gate-1-f40 gated:
8.16 s -> 1.54 (trunk sharing) -> 1.49 (select pass-through) -> 1.32
(typed panes) -> 1.26 (undo-log reset) -> 1.04 s (typed append).
Rt2 reference 2.0 s; the morning's engine is 7.8x behind.

Where the next factor is NOT: at depth the kernel no longer matters -
`--abstract 40` runs the SAME 31 s wall under Rt2, tile mode 1 and
mode 2, because the serial boundary/merge/dedup dominates from ~f35
on (902k lanes). The kernel's residual profile (f_15 glue 15%,
boundary 14%, select 12%, append 10%) says roughly one more 2x is
available in-frame, but the CAMPAIGN-relevant lever is now goal 7:
merge/dedup/boundary at roofline. That is the next unit.

## Shape-gate diagnostic: two steady shapes (late evening)

`--abstract 40` under tile mode rejects 48,767 chunks at the shape
gate: the NATIVE from-scratch run's steady shape (0x893c9b0ef0d84006)
differs from the interpreter-campaign checkpoint shape the census was
taken on (0xc51b1bf0e3dba1ac) - boundary timer pins and the importer's
unknown-field dropping make both legitimate but distinct. Consequences,
stated honestly:
- The tile kernel accelerates the CAMPAIGN path (checkpoint-imported
  states, which is what the search runs on); the from-scratch
  `--abstract` harness runs Rt2 and its "both modes exact" claim is
  really an Rt2 claim post-gate.
- Tile-mode exactness at scale is certified by the BENCH: real f35
  states, 187,859 lanes in, exact through f37 (365,029), guard green.
- If tile acceleration for native from-scratch runs is ever wanted,
  dump a second census from a native steady state (the pipeline is
  shape-generic); the gate now PRINTS rejected shapes, so this is
  observable, not silent.

## Goal 7 opened and re-aimed: the depth wall was the gate, not the boundary (2026-08-17)

Measured first (`CELESTE_PHASE_TIME=1`, per-frame phase split of
`frame_step`: part / run / dedup / retain / merge). From-scratch
`--abstract 40`, f40 under the with-slots build: run 9.66 s (94%),
part 232 ms, merge 218 ms, retain 109 ms, dedup 91 ms. The "serial
boundary/merge/dedup dominates at depth" hypothesis from the evening
pivot is REFUTED - the serial epilogue is ~7% of a deep frame. The
sharded dedup + k-way merge already at roofline enough; the 31.5 s
wall was Rt2 kernel execution behind the slot shape gate (previous
section: from-scratch steady shape != census shape, all 48,767
steady chunks rejected).

The fix fell out of the slot-neutrality measurement: slots buy no
time, so gen.rs is regenerated WITHOUT --site-slots. N_SLOTS=0 and
the gate (already conditional on N_SLOTS > 0) opens; the tile path
is shape-generic without slots (generic get_field/get_index resolve
by name; anything odd bails to Rt2). Result: mode-2 tiles carry the
ENTIRE from-scratch depth run, spawn shape included - 0 bails, 0
gate rejects, every frame's lane count exactly equal to the Rt2
reference, 31.57 s -> 17.55 s (f40 frame 10.3 -> 5.2 s). Scalar
oracle hex-exact, suite 527/527, bench unchanged (mode 2 1040.7 ms).

Canonical regen is now `transpile --recipe rewrites-compile.jsonl`
(no slot map). plans/site-slots-room10-f035.json stays checked in as
the typed-slots seed; regenerating WITH it re-arms the one-shape gate
and is only sound to ship together with censuses for every shape a
run visits.

Standing profile after this: a deep frame is still 93% run phase
(kernel + per-chunk boundary, parallel). The remaining in-frame ~2x
is the kernel residual ladder (f_15 glue, select residual, append
residual, low-depth row batching). Campaign-wise the engine is now
fast AND shape-open, which unblocks the real integration question:
crate restructure (#133), wiring gen.rs into the actual runner.

## Afternoon ladder on the reopened kernel (2026-08-17)

With tiles carrying depth, two profile-led steps on the 1.04 s bench:
1. bi_tile_flag_at 19% -> gone: perf annotate showed ~85% of the
   symbol on the Arc refcount `lock incq/decq` of the per-call
   `cache/cart.clone()` pair - 29 threads bouncing two cache lines.
   Inline all-Num pane->mask path with only immutable borrows of
   self; (wi,hi) map dispatch hoisted out of the lane loop.
   1031 -> 639 ms; --abstract 40 17.55 -> 10.85 s. The general
   lesson: per-call Arc clones in hot helpers are contention under
   parallelism, and the profile attributes them to the CALLEE body.
2. select mask-cond + bool-arm bitwise combine (mask_src normalizes
   U(Bool)/B(m,e) to a width-projected lane mask): 639 -> 607 ms,
   and mode 1 (select-heavy) 3776 -> 2064 ms.

State at close: bench 607 ms = 3230 ns/input-lane (Rt2 reference
2030 ms), --abstract 40 = 10.42 s from scratch (was 31.57 s this
morning), every step exact + scalar hex-exact. The profile tail is
flat (f_15 diffuse codegen traffic, boundary/append typed streaming);
the next engine units are structural: #132 row-struct emission,
Col::B through Rt2, SIMD panes. The next CAMPAIGN unit is #133 -
linking the generated engine into the real runner, where checkpoint
chains, visited sets and the position graph live.

## Gate 2 LANDED: row-SET equality, plus the two-shapes mystery solved (2026-08-17)

Gate 2 needed no export machinery: both sides funnel through the SAME
canonicalizer. The bench oracle now imports the interpreter's
next-checkpoint states, runs them through `boundary` (its widenings
are idempotent on boundary states), and compares canonical 128-bit
row-key SETS against the engine's chased output. Passing means the
engine's surviving row set IS the interpreter's - not just the same
count.

First sweep found exactly one mismatch: f020 -> f025, counts equal
(204 = 204) but ALL keys differing. Diagnosis: row keys are seeded
with the shape hash, and the engine-spawned player object carries its
fields in STORE order while the importer sorts them by name - same 25
fields, different order, different shape, and (since the canonical
BFS discovers children in field order) different cell numbering
everywhere downstream. Fix: `boundary` sorts every Obj's fields by
name before the BFS - one canonical order regardless of an object's
lineage.

Consequences:
- Gate 2 GREEN at every (1,0) checkpoint: f005, f010, f015, f020,
  f025, f030, f035 -> f037, spawn transition included.
- THE TWO STEADY SHAPES WERE THIS ARTIFACT. From-scratch
  `--abstract 40` now converges to the census shape
  0xc51b1bf0e3dba1ac. One steady shape everywhere: the checked-in
  slot map would be valid for from-scratch runs too, if slots return.
- All prior gates hold: lane counts exact f1..f40, scalar hex-exact,
  bench exact, suite green.

Rooms recon (the 300m question): the engine REJECTS rooms (2,0) and
(0,0) loudly and correctly - `AssertClosure(fn 52) failed at
anonymous_61` on fruit (fn 42) / (0,0)-object (fn 39) update
closures. The compiled program's devirtualized update dispatch is
specialized to the (1,0) steady window; the interpreter survives
this via lane-granular deopt to the PLAIN program, which the engine
does not have. Engine-on-(2,0) therefore needs either the deopt
fallback or a broader devirt in the compile recipe - a named unit,
not a surprise.

## 300m RE-projection with same-day measurements (2026-08-17)

All numbers measured today on this machine, cores 0-14/16-30.

| metric | engine (mode-2 tiles) | interpreter (`rewrite bench`) |
|---|---|---|
| (1,0) f35 -> f36, 187,859 lanes | 607 ms (3.23 us/lane wall) | ~300 ms (1.6 us/lane) |
| (1,0) f40 frame, 673k lanes in | 3.08 s (4.6 us/lane) | 0.94 s (1.4 us/lane) |
| (1,0) f1..f40 total | 10.40 s | 4.02 s, peak 1.6 GB |
| row storage at f35 | ~9 varying cols, ~50 typed B/row per pm1 block | 354 fragments/frame mean |

Honest revision of the 2026-08-18 projection:

1. TIME: the old "v0 ~10x per offered lane, 5x more coming" does NOT
   hold against the PRODUCTION interpreter. Today the interpreter is
   2-3x faster per frame at (1,0) depth. Its edge is representation:
   pm1/fragment partitioning keeps ~350 mostly-uniform fragments where
   the engine merges to ONE wide block per shape and then streams
   every varying column per lane. Same rows (gate 2 proves it), ~4x
   less core-time per lane.
2. (2,0) TIME projection: campaign wall was 347.9 s/frame at 62.9M
   lanes (5.5 us/lane, 16 threads, pos-graph included). Engine at
   4.6 us/lane x est. 1.3-2x for 4-object rows = 6-9 us/lane -> rough
   PARITY, not a win - and it cannot run (2,0) yet (devirt asserts;
   needs the fruit-clean v9 recipe compiled + fruit widenings + death
   deopt, or an engine deopt-to-plain).
3. MEMORY - the real prize, unchanged: engine frontier at the (2,0)
   f70 scale is 62.9M x ~50-100 B = 3-6 GB vs the interpreter's
   67.6 GB RSS; the S-rung humps that OOM'd (57.6-97 GB for 1.7-2.4M
   lanes of interpreter INTRA-frame machinery) become ~100-250 MB of
   boundary rows + chunk-local MBs. The OOM wall that paused the
   S-rung ladder is gone under the engine.

Refined GO path (unchanged in direction, repriced in reason): the
engine un-parks the S-RUNG LADDER on (2,0) by removing its memory
wall; raw per-lane time is at parity until the engine closes the
representation gap. The named engine unit for that gap: keep pm1-key
partitions through frame_step instead of merging to one block per
shape (partition_by_key exists; most columns become Col::U again,
shrinking both streaming and storage) - measure, then re-run this
table. Prerequisites for (2,0) at all: compile the fruit-clean v9
recipe (verified on (2,0)) + port fruit widenings + death-frame
deopt.
