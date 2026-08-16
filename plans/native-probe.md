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
