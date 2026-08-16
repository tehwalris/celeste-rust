# Native-compile probe (task #124)

Question (Philippe, 2026-08-16): take a compiled version of the room and real
lane data, write the frame as plain Rust "capturing exactly what the IR
captures without deopts", run it in a tight loop on one isolated core - how
fast CAN a frame-lane go? Bet was 5-10x over current.

## Result (2026-08-16)

**6.2-6.4 µs per frame-lane, hex-exact.** Measured on core 15 (frequency CCD,
SMT sibling idle, the S16 campaign pinned to cpus 0-14,16-30).

| path | per frame-lane | vs native |
|---|---|---|
| native probe (this) | 6.2-6.4 µs | 1x |
| abstract interpreter, per lane (BENCHMARK_DATA) | ~17 µs | 2.7x |
| concrete_run interpreter, quiet machine | ~640 µs | ~100x |
| concrete_run interpreter, under campaign load | ~1,140 µs | ~180x |

Counters (perf stat, 400k frames): **IPC 3.87**, cache-miss rate **0.91%**
of refs, 7.4M branch misses / 48.6B instructions. Fully compute-bound and
effectively L1/L2-resident, as predicted - and this is v0, with linear-scan
field lookups on `Vec<(u32,u32)>` object tables, an 8-byte tagged value
enum, no allocation reuse and no GC (heap grows monotonically over the
trajectory).

The provocative comparison is the middle row: a *scalar, single-lane* native
frame already beats the vectorized abstract interpreter's *per-lane* cost by
2.7x. The interpreter's vectorization amortizes dispatch over thousands of
lanes and still loses to compiled scalar code - dispatch + boxed values +
COW heap machinery cost more than vectorization saves.

## What was built

- `src/bin/transpile.rs` - IR -> Rust generator. Input: the PLAIN executable
  program (`Program::compile_executable_from_disk()`, the exact program
  `concrete_run` interprets - so the oracle needs no deopt story). One Rust
  `fn` per `FunDef` (77), blocks lowered to a `loop { match block }` state
  machine, phis destructed into parallel copies on edges (entry-edge phi
  branches resolved by elimination against named predecessors), calls
  dispatched through a generated `match` on dense fn ids, closure cells
  carrying `(fn_id, captures)`. Field/global/string names interned to u32 at
  codegen time.
- `native-probe/` - the generated crate (gitignored `src/gen.rs`, ~12k
  lines; hand-written `runtime.rs` + `main.rs`). `runtime.rs` is a
  line-for-line port of the interpreter's scalar semantics with file:line
  pointers at every op; anything the concrete path cannot reach panics.
  Path-dependency on the main crate for `Pico8Num` (16.16 arithmetic, the
  console-dumped sin table) and `CartData`/`CollisionCache` - none of the
  dangerous semantics were re-implemented.
- Validation: `diff` of `^Frame` lines against `concrete_run` - the full
  printed state (x, y, spd, rem hex, freeze) every frame. Gate runs:
  the canonical 10-input sequence (30 frames) and a seeded random 400-frame
  input tape visiting 270 distinct player positions (deaths, respawns,
  dashes, walls). Both IDENTICAL.
- Bench: `--bench N` replays the trajectory N times from the post-init
  snapshot (restore cost amortized over 400 frames/rep).

Repro:

```bash
cargo build --release --bin transpile && ./target/release/transpile
cd native-probe && cargo build --release && cd ..
seq=$(python3 -c "import random; random.seed(7); print(','.join(str(random.choice([0,1,2,2,2,18,18,34,50,16,32,48,6,10,22,38,2,2,54,17,33])) for _ in range(400)))")
diff <(./target/release/concrete_run -i "$seq" -f 400 | grep '^Frame') \
     <(./native-probe/target/release/native-probe -i "$seq" -f 400 | grep '^Frame')
taskset -c 15 ./native-probe/target/release/native-probe -i "$seq" -f 400 --bench 1000
```

## What this means / next steps (not started)

1. **v1 - transpile the REWRITTEN program.** The generator already lowers
   every recipe-planted instruction (Select, Expand, Kill, the assert
   guards); pointing it at `recipe::build` output instead is a flag plus a
   premise-failure story (panic = the honest "no deopt" probe).
2. **v2 - abstract semantics, natively compiled.** Intervals are pairs,
   spd/rem buckets are masks; a compiled abstract frame over small lane
   blocks (tiled to stay L1-resident, as this probe proves the frame logic
   itself is) attacks the 17 µs/lane row directly. If even a fraction of
   the 2.7x scalar gap survives vectorized-abstract codegen, every rung of
   every remaining room reprices.
3. Known v0 headroom if a faster ORACLE is ever wanted: struct-layout
   specialization of object tables (linear scans are the top cost),
   allocation reuse/arena reset per frame. Not worth doing until a consumer
   exists - 6 µs already makes 400-frame differential tapes effectively free
   (2.6 ms/tape), e.g. for property-fuzzing rewrites against the oracle.

## Caveats

- The probe transpiles the plain program; the rewrite campaign's soundness
  story (differential verification per entry) is untouched. The probe is a
  measurement instrument, not a proof artifact.
- `tile_flag_at` non-zero flags return false, matching
  `tile_flag_at_computed`'s per-element core (game_runner.rs:718) - the
  interpreter has the same behavior, so the oracle agrees; neither models
  real ice/spike flag reads through this path (the cart's spikes go through
  `spikes_at`, not `tile_flag_at`).
- `BUILTIN_NAMES` order is a shared ABI between `src/bin/transpile.rs` and
  `native-probe/src/runtime.rs`; both sides carry a comment.

## Stage log (task #125, hill-climb)

- **Rewritten program transpiled and gated** (`transpile --rewritten` =
  plain compile + full rewrites.jsonl). Hex-IDENTICAL vs concrete_run on a
  no-death 400-frame tape (seed 11, 282 distinct positions). On the death
  tape it panics at `anonymous_61 %3772: AssertTrue(#objects == 1)` at
  exactly the frame the player dies - the collapsed-loop premise the search
  handles via deopt; the probe's no-deopt contract makes it a loud panic.
  Assert sites now carry fn+%id context strings.
- **Rewritten runs 2.5x FASTER than plain natively: 2.54 µs/frame** (vs
  6.4 µs). The recipe's 890 entries (269 inlines, promote_cell,
  collapse_loop) pay off for native code too. That is **6.7x under the
  abstract interpreter's ~17 µs/lane**, still scalar.
- **get_field inline cache: tried, measured WORSE (2.63 vs 2.54 µs),
  reverted.** A global epoch invalidation dies to the per-frame
  `__button_states = {}` StoreEmptyTable. Real fix if ever needed:
  codegen-time shape specialization (fixed slot offsets per allocation
  site), not runtime caching. Profile: generated code 60%, get_field 15%,
  store 4%, get_index 4%.
- debug=2 AND debug=1 SIGSEGV rustc's LLVM (DWARF pass) on the fused
  functions; probe builds with debug=false.

## Stage 2 design (abstract single-lane, next)

- `V::Ival(P8, P8)` + tri-state compares; interval op arms ported
  EXACTLY from op.rs / game_runner.rs (eq-on-interval => false, per the
  flagged pre-campaign review item; add/sub lift; min/max/abs/sin-full;
  flr panics unless both endpoints share a floor - the split rewrites are
  what keep that from firing).
- Buttons stay UnknownBool at frame entry (no set_buttons): the btn
  choice points ARE the 64-input fan-out.
- Choice points, each a tape entry: branch-on-UBool (fork 2, condition
  refined per flow.rs edge refinement), Expand-on-UBool (fork 2),
  __split_by_flr on Ival (fork per floor, clipped), __split_at (3-way).
  Select-on-UBool panics (rule #96 eliminated those).
- Fork = DFS replay from a frame-start heap snapshot with a choice tape;
  successors = completed replays. Boundary: apply rem widening
  (make_state_abstract equivalent) + optional spd bucket clip, extract row.
- Oracle ladder: (1) per-frame state/lane counts vs `rewrite bench` on
  room (1,0) level 0; (2) exact row-set equality via reconstructing native
  successors as interpreter `State`s through the lib and hashing with the
  interpreter's own canonicalization.

### Stage 2 implementation checklist (precise porting targets)

- Boundary abstraction = `make_state_abstract` (abstraction.rs:286):
  `apply_conservative_widenings(spd(rem(state)))`. Port each:
  rem: mark "player_rem_xy", range [-0.5, 0.5) (0x8000 frac), bucket
  width `0x1_0000 >> CELESTE_REM_BITS` (full widening when unset+not
  EXACT); spd: mark "player_spd_xy", `CELESTE_SPD_WIDTH_LOG2`, sanity
  +/-16px (from_parts(-16..16)); conservative widenings: READ
  abstraction.rs apply_conservative_widenings before porting (timer
  globals? fruit bob?). Straddle canonicalization: split_rem_straddles /
  split_precision_straddles (single-bucket invariant, max 2 buckets).
- mark_heap: which cells carry which marks - read abstraction.rs.
- Canonical row identity for native dedup: BFS from globals (sorted
  global name order, fields sorted) assigning canonical ids, hash values
  in visit order; also serves as per-frame compaction/GC. Must collapse
  exactly what the interpreter's boundary dedup collapses (gate 1 =
  per-frame lane counts vs `rewrite bench` room (1,0) level 0, f1..f30).
- Choice-tape DFS driver in native-probe main: buttons stay UBool;
  choice points = branch-on-UBool (refine condition per flow.rs),
  Expand-on-UBool, __split_by_flr/__split_at on Ival.
- Interval op arms in runtime.rs: port from op.rs 397-640 interval arms +
  game_runner min/max/abs/sin/flr interval impls. eq-on-Ival => false
  (op.rs, flagged review item - mirror the interpreter, do not "fix").
- Keep concrete tapes as regression gates: Ival arms must be unreachable
  on a concrete run; the 400-frame no-death diff must stay IDENTICAL.

### Stage 2 porting details captured so far (abstraction.rs reads)

- rem widening (make_state_abstract_rem, :521): Exact = untouched;
  Bits(0) = full interval [-0.5, 0.5-eps] (eps = next_smallest of 0x8000
  frac; CLOSED interval endpoints); Bits(k) = floor-aligned bucket
  [low, low+width-1] raw, width = 0x1_0000 >> k, low = raw.div_euclid(width)
  * width. Values asserted inside [-0.5, 0.5). Mid-frame interval residue
  widens to the span of its endpoint buckets.
- apply_conservative_widenings (:710): (1) clamp player.dash_effect_time
  at 0 from below (only read is `> 0`); MORE FOLLOWS after :740 (p_jump/
  p_dash note; likely the #75 timer pinning) - read the rest before
  porting. mark_heap provides named cell sets ("player_rem_xy",
  "player_spd_xy", "player_dash_effect_time", ...) - port the mark
  traversal or reimplement mark lookup natively via the same paths.

### Per-shape instantiation: gap census v0 design (2026-08-16 night)

Dynamic, not static: import real snapshot lanes (State -> Rt converter;
interval cells get placeholder scalars - pointer topology is what matters)
and run frames in the native engine with per-site RECEIVER LOGGING: each
get_field/get_index/get_global site records its receiver cell id across
lanes/frames of one shape. Sites with a single receiver per shape =
columnizable (compiled row program keeps a cheap runtime guard, same
doctrine as AssertClosure: statically assumed, loudly checked). Sites with
varying receivers = the gap report = the shape's overlay to-do list.
Probe grows `--from-checkpoint DIR --frame N --lane L` mode for this and
for the abstract-oracle runs later. Priority order by inventory lane mass:
room1 player shape, then room20 fruit+spring+spring+player pair, then
room00 fake_wall+player.

### Gap census implementation state (checkpoint for continuation)

- native-probe/src/import.rs WRITTEN: State->Rt importer (per-lane, memoized
  cycle-safe, drops program-unnamed fields/globals, interval cells get
  low-endpoint placeholders under a flag; MaybeBool panics).
- TODO next, in order:
  1. Rt: cart/cache fields -> Arc (share across per-lane Rt rebuilds);
     add `site_log: Vec<u64>` (0=unseen, cell+1, MAX=multi lattice).
  2. transpile.rs: number get_field/get_index sites, pass SITE arg;
     emit SITE_INFO: &[(kind, fn_name, name_id)] + N_SITES in gen.rs.
  3. main.rs: `--from-checkpoint DIR --frame N [--census-frames K]
     [--max-lanes M]` mode: per matching lane fresh heap/globals,
     import_lane, run K frames over a few input bytes, join site_log into
     a global accumulator; report gaps (MULTI sites) grouped by fn with
     resolved names + columnizable percentage.
  4. Cargo.toml probe profile: panic=abort -> unwind (census must survive
     per-lane premise panics via catch_unwind); RE-BASELINE the 2.5us
     bench after the switch.
- Then: emit the row program for the room1 player shape (columnizable
  sites -> fixed row slots with runtime guards), gap list -> overlay work.

## Zero-heap / zero-branch status (2026-08-17, room (1,0) player shape)

The compile-only recipe `rewrites-compile.jsonl` (base + overlay before
kill_dead; consumed by `transpile --recipe`, NEVER by the runner - the
expansion measured +19%/+38% on the interpreter and stays out of the base)
now measures, against fresh post-Lua-edit f35 states (256 lanes, 64-input
fan-out, 0 panics, hex-identical to concrete_run over 400 frames):

- HEAP: **0 multi-receiver sites** (was 22). 182 single-receiver live
  sites = guarded fixed row slots; 2892 unreached. Zero-heap for this
  shape is measured, not projected. The enabler was the `__button_states`
  toplevel-init Lua edit (commit ff4a853).
- BRANCHES: 611 static (was 693), 29 executed, **11 divergent** (was 22):
  - 1 `anonymous_61 @in_h061_and_or_join_65` - the dash-trigger branch;
    its arm holds stores/calls, needs a stage-D speculate+absorb
    derivation (m-package pattern). The last genuine blend site.
  - 1 `__frame @in_i1_012_cont` - freeze early-out; pm1 already
    partitions on freeze, so it is uniform per dispatch class.
  - 9 smoke-birth lifecycle (j2_008/009 draw-loop iterators + the
    anonymous_63/64 type lambdas): jump/dash spawns a smoke object, draw
    loops iterate 1 vs 2. Per-shape unroll domain - the draw shims are
    noops, so the masked smoke arm is nearly empty.

Overlay contents: 20x expand_bool (every remaining btn concretization
diamond), 4x decompose_branch (mixed and-selects), 6x speculate_region
expand:true (the short-circuit btn arms run eagerly - the k_left/k_jump/
k_dash reads concretize for all lanes), 53x if_convert, merges + dce.
Every entry rule-verified on apply; whole recipe differentially verified
identical through 40 frames (81.5s); all 5 checked-in recipes replay
(the test now GLOBS rewrites*.jsonl).

## Zero-divergence grind, round 3 status (2026-08-17, late)

Target: the 2 real divergent branches (freeze gate + dash trigger; the
other 9 census sites are measured pure shadows of the freeze gate).

Landed groundwork:
- `drop_dead_cell` rule (pointed; unit-tested): deletes an alloc'd cell
  nothing reads - the alloc, its stores, and its provably-true guards
  (`assert_closure` matching a unique dominating `store_closure`;
  `assert_value_cell` dominated by a plain store). Needed because
  inlining a call-through-cell leaves an undeletable cluster behind.
  Deliberately NOT a `dce` widening - that would silently change what
  the certified recipes produce.

Derivation attempt (reverted to keep the recipe green): the freeze-gate
conversion pipeline is proven end-to-end EXCEPT one link. What worked,
in order, on top of the committed overlay: inline the two _draw lambdas
+ player.draw at their call sites; drop the dead closure cells; convert
the all()-iterator first-slot triangles (speculate + absorb_stores +
merge + forward-cse + drop_dead_cell); then
`speculate_region {mask:true}` over the whole draw body APPLIED (18
changes - the gate became an unconditional masked region). The broken
link: the draw foreach loops must be COLLAPSED first, and both existing
collapse rules mismatch - `collapse_loop` plants `assert(32767==1)`
(the differential screen caught it at frame 1, exactly as designed),
and `collapse_break_loop` predates #112's all() shape (nil-keyed
sentinel, no length read). This is precisely open task #113.

Next unit (fresh session): implement the all()-shape collapse
(`collapse_all_loop`: 2-iteration unroll under runtime asserts -
iter 1 payload runs, iter 2 hits the nil sentinel), re-apply the
proven pipeline, then the dash-trigger stage-D (speculate +
absorb_stores over the flattened dash body). Both are the last things
between the compiled room (1,0) shape and 0 divergent branches.

## Zero-divergence round 3 RESULT (2026-08-17): 22 -> 9, both real sites cornered

Measured on the regenerated room (1,0) f35 states (hex-identical 400
frames; differential verify identical through 40, spawn frames included):

- branch census: 9 divergent = 2 REAL + 7 shadows of the freeze gate.
  The type-lambda and loop-head shadows are GONE - the three per-frame
  all() walks are collapsed by the new `collapse_all_loop` rule
  (peel-2-assert-3; task #113 discharged), the draw lambdas and
  player.draw are inlined at all four peeled sites, the six advance
  diamonds are select-stores, and eight scratch cells are dropped.
- gap census: 0 multi-receiver, 184 single-receiver, 0 panics.

The two real sites, each one link from done:
1. freeze gate (__frame @in_i1_012_cont): the masked-region conversion
   is PROVEN to apply once the four idx/last iterator cells drop; their
   loads sit behind stores to sibling scratch cells, which forward-cse
   treats as fences. Missing link: an opt-in cell-precise forward mode
   (a store through a non-escaping alloc cell cannot alias any other
   cell) - opt-in so certified recipes stay byte-identical. Then:
   forward -> drop_dead_cell x4 -> speculate_region {mask:true} (the
   entry that applied cleanly in the reverted round) -> gate gone,
   7 shadows with it.
2. dash trigger (anonymous_61 @in_h061_if_condition_43): stage-D
   speculate + absorb_stores over the already-flattened dash body
   (m-package pattern), a mechanical per-store derivation.
