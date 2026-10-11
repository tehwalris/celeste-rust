# Jank in the tracer and the reference engine (2026-10-11)

Scope: `src/trace/` without `widen.rs` and `level_minus_one.rs` (12,260 lines
with tests: interp 2147, verify 2151, domain 1618, kernel 880, bind 639, ...),
plus `celeste-engine`'s `runtime2.rs`/`slots.rs` where they meet the tracer.
Line numbers are against `origin/develop` 432a844. Every claim below was
checked by reading the code and grepping for its users; nothing was deleted
and rebuilt to check it.

This goes past SPEC annex D. Where annex D already says something (tr-2..5,
tr-13, rm-1, rm-3), it is only extended here, not repeated. Since annex D was
written, develop has fixed gl-1 (the level is a value, `ed70c5b`) and the
settings global (`8a240a8`) in this area.

**Bottom line.** Roughly 1,500 lines (about 12% of the area) can go without
changing one kernel byte. About 1,050 of them are high confidence (T1-T9). The
tracer's core is mostly essential complexity: the interpreter, merge, the
derived error and the split pass. The fat is in four places:
- what crosses the boundaries: tracer to kernel builder (T10), heap to block
  (T7), the interpreter's hold on the AST (T5);
- duplicate vocabularies: paths, kinds, "find the player", shape identities;
- dead diagnostics;
- boilerplate the `D: Domain` generic forces.

## Ranking

Score = lines x confidence / effort (effort in rough half-days).

| # | finding | lines | conf | effort | score |
|---|---|---|---|---|---|
| T1 | dead code and pass-through wrappers | ~190 | .95 | 1 | 180 |
| T2 | hand-written Clone/PartialEq/Default over `D: Domain`; `Rt2` "not Clone" | ~100 | .95 | .75 | 127 |
| T3 | test-side duplication: helpers in `verify.rs`, five value-to-`Conc` copies, printing tests | ~110 | .9 | 1 | 99 |
| T4 | one worker pool instead of 13 hand-rolled ones | ~70 | .85 | 1 | 60 |
| T5 | **boundary**: the interpreter and the program (raw-pointer AST keys, 4 `unsafe impl Send`, 8 boot copies) | ~130 | .85 | 2 | 55 |
| T6 | **boundary**: one path vocabulary, one object finder | ~60 | .9 | 1 | 54 |
| T7 | **boundary**: one heap-to-block converter (`structure_of` and `to_block` are the same function twice) **(done)** | ~180 | .8 | 3 | 48 |
| T8 | duplication inside `interp.rs`/`state.rs` | ~100 | .9 | 2 | 45 |
| T9 | **boundary**: `Domain` is Symbolic's interface; `Concrete` is vestigial; RefDomain point arithmetic diverges **(divergence done)** | ~80 | .85 | 1.5 | 45 |
| T10 | **boundary**: the trace-to-kernel hand-off (Frame / Bound / Lowered / Reference, two root orders, string kinds) | ~250 | .65 | 4 | 41 |
| T11 | RefEngine's eight step entry points | ~45 | .9 | 1 | 40 |
| T12 | domain ops: four mini-enums mirroring `Op`, a 5-step preamble written 4 times | ~100 | .7 | 3 | 23 |
| T13 | duplication inside the split pass (`verify.rs`) | ~40 | .8 | 1.5 | 21 |
| T14 | six memoized per-node graph predicates, six memo maps | ~80 | .6 | 3 | 16 |
| T15 | fork origins as strings | ~35 | .7 | 1.5 | 16 |
| T16 | three builtin lists (beyond rm-3) | ~15 | .8 | 1 | 12 |
| T17 | `Rt2::prints`: dead, yet persisted | ~15 | .95 | 1.5 | 10 |
| T18 | shape identity: the walk keys on `format!("{:?}", Shape)` | ~30 | .6 | 2 | 9 |
| T19 | interpreter special cases keyed by spelling | ~40 | .5 | 3 | 7 |
| T20 | two "run the room with no input" mechanisms | ~40 | .45 | 3 | 6 |
| T21 | **layering**: the reference engine lives in `trace/` but sits above `frame`/`storage`; the engine crate knows the game | ~10 | .8 | 2 | - |

Overlaps (T2 and T10 on `FrameOut`, T3 and T5 on the boot, T12 and T14 on
`Symbolic`) take ~200 off the sum of ~1,700, hence ~1,500.

---

## The boundaries (where they are cut, where they should be)

### T10. Tracer to kernel builder: three parallel views and flat root vectors

**Now.** One traced outcome crosses four types:
- `verify::FrameOut` (verify.rs:24-54);
- `emit::FrameOutcome`, inside `Bound` (emit.rs:45-69);
- `transpile::lower::Outcome`, holding `OutFields`;
- `lower::SpecializedBody`, then `emit::AsmBody` (emit.rs:246-252).

`trace::kernel::Reference` (kernel.rs:11-17) bundles `Frame`, `Bound` and
`Lowered`, plus a cart and a cache. `asm_kernel` then indexes three of these
views in parallel for the same outcome: `r.frame.outs[oi]`,
`r.bound.outcomes[oi]` and `r.lowered.outs[oi]` (asm_kernel.rs:1520-1564,
1777-1822).

Roots travel as flat `Vec<NodeId>`, laid out by index arithmetic, in **two
orders**:
- `emit::bind` builds `[fields, live, error, arc]` (emit.rs:99-105, 212-213);
- the lowering and the codegen use `[fields, error, live, arc]`
  (asm_kernel.rs:1530, lower.rs:319);
- `root_legend` (emit.rs:74-87) and verify's `Lite.fields`, which carries the
  transfer roots appended (verify.rs:892), are two more layouts.

Kinds travel as strings:
- `"ZN"/"ZI"/"ZB"` are made in verify.rs:143-149 and matched back in
  asm_kernel.rs:1797-1801 and lower.rs:485;
- `"bool"/"ubool"/"num"/"ival"` are made in emit.rs:165-173 and parsed back in
  `asm_input_reprs` (emit.rs:226-242).

The same notion "what a cell holds" has six encodings: `graph::CellKind`,
`asm::CellRepr`, `codegen::RootKind`, `asm_kernel::Kind`, the two string
vocabularies, and `Iface.init` + `Iface.ival`.

Other leaks across this boundary:
- A uniform input is found by substring: `show(path).contains(".hitbox.")`
  (emit.rs:178).
- `asm_fused_from` -> `asm_fused_of` (emit.rs:254-285) only reshapes a tuple,
  and clones the fused graph and every body to do it.
- `kernel::name_cells` (kernel.rs:20-42) recovers names by parsing `Cell(` out
  of an error string.

**Should be.** `trace_frame` ends in engine numbering and returns one
`TracedFrame`:
- the cone graph (canonical cell ids);
- `inputs: Vec<(cell, ColKind, display name)>`, with a uniform flag set by the
  tracer from the object type, not from a substring;
- `forks` and `fork_ways`;
- per outcome: its canonical output skeleton (`Rt2`), `fields: Vec<(cell,
  ColKind, NodeId)>`, its dead cells, and named roots `{ live, error,
  transfer: Option<[NodeId; 10]>, fin }`.

`ColKind { Num, Ival, Bool, UBool }` lives once, beside `AV` in
`celeste-engine`. The lowering takes `&TracedFrame`, and `asm_kernel` reads one
struct per outcome.

**Disappears:**
- `emit::bind`, `Bound`, `FrameOutcome`, `root_legend`, `asm_input_reprs`,
  `asm_fused_*`, `Lowered.bodies`: emit.rs goes from 328 to ~110 lines;
- `Reference` and `name_cells`;
- the string kinds and their parsers;
- the index arithmetic in asm_kernel.

Estimate: ~250 lines, confidence .65 (level -1 also reads `Frame`;
level_minus_one.rs:420-442 would have to move with it).

**Verification.** The kernels must come out byte-identical. `./check.sh t1`:
the frame steps compare exact metrics including dynamic instruction counts,
then keycheck and refcheck. Then `t2`: the gates and forward60, the platform
kernels. Then the oracle test (t0).

### T7. Heap to block: the same converter written twice

**Done (2026-10-11, branch `ref-fixes`).** `trace::block`: `to_rt2<D>`
builds in any order, then `canonicalize_ids`; the domain supplies only what
a number and a boolean become (`skeleton`: the tracer's, all `Nil`;
`refbridge::to_block`: the reference's values). `from_block<D>` is beside
it. A builtin is a `Bi` cell at EVERY slot (the reference's first-slot rule
is gone); a closure is one cell, like a table (the tracer made one per
slot); captures are tables on both sides. The canonical-claim test and the
`asm_kernel` probe are gone. `RefEngine::check_agreement` reads every lane
of a block into both domains and requires one block and one exact key back:
a t0 test over rooms (1,0) and (4,2)'s exits and the kernels' successors,
one with a builtin in three slots, and `ref-check` (t1) on every sampled
fixture row, its kernel successors and reference leaves. No output moved
(t0-t2 and big unchanged, no re-pin).

**Now.** Two functions turn a heap into an `Rt2`, and their doc comments ask
each other to "agree":
- `bind::structure_of` (bind.rs:60-228, 169 lines): `State<Symbolic>` to a
  structure, numbered by BFS as it builds;
- `refbridge::to_block` + `ToBlock` (refbridge.rs:204-365, 162 lines):
  `State<RefDomain>` to a block with values, DFS then `canonicalize_ids`.

Both flatten integer keys into a dense array (bind.rs:164-196 and
refbridge.rs:259-275) and both drop non-ABI fields. They already disagree on
builtins:
- `structure_of` writes a `Bi` cell **in place at every slot** holding a
  builtin (bind.rs:148-156);
- `to_block` makes only the first slot the `Bi` cell and every later one a
  `Val(Ptr)` (refbridge.rs:241-247).

A builtin reachable from two slots would key differently in a kernel row and
in a reference row. Today that is latent: the cart holds each builtin in one
global. It is still exactly the silent divergence the ground rules forbid.

Because `structure_of` claims canonical numbering by construction, two more
pieces of code exist only to check that claim: the test at bind.rs:495-547 and
the probe in asm_kernel.rs:1778-1786.

**Should be.** One `trace::block` module with:
- `to_rt2<D>(st, value: impl Fn(&Value<D>) -> AV)`, build in any order then
  `canonicalize_ids()`. The tracer passes `|_| AV::Nil`; the reference passes
  the value mapping.
- `from_block` (refbridge.rs:52-189) beside it as its inverse.

**Disappears.** One converter, the agreement comments, the canonical-claim
test and probe. Estimate: ~180 lines, confidence .8.

**Verification.**
- t0: the bind tests, `inputs_agreeing_on_the_buttons_read...`, and
  `nodiag_and_onlydiag...`;
- t1: `refcheck-r10/-r42x` (kernel rows against reference rows, by key) and
  `keycheck`;
- t2: `gate10-arc`, whose concrete witness comes through `to_block`.

### T5. The interpreter and the program

**Now.**
- `Interp<'a, D>` keys bodies by `*const ast::FunctionBody` (interp.rs:60).
  That is the **only** `!Send` field: full_moon 0.19's AST is `Sync` (SmolStr,
  no `Rc`/`RefCell`), and `Graph` and the domains are `Send`.
- So there are four `unsafe impl Send`, each with a SAFETY essay:
  kernel.rs:843-845, refengine.rs:45-48, verify.rs:1740-1742 and 2045-2047.
- The ASTs are `Box::leak`ed per walk (kernel.rs:362) and per `RefEngine::new`
  (refengine.rs:54: four ASTs including the whole cart). `arc_dp` builds one
  engine per thread, and the frame.rs tests build about 15.
- The boot sequence is written out 8 times: sources, parse,
  `check_absent_fields`, cart + cache, `fresh_state`, toplevel,
  `inject_tile_flag_at`, `_init`. The copies are at kernel.rs:360-386,
  refengine.rs:53-72, bind.rs:502-522 and 555-575, verify.rs:1629-1645,
  1701-1724 and 1842-1859, and widen.rs:718-731.
- There are two "run a chunk, expect one state" functions: `cart::run_chunk`
  (cart.rs:340-354) and `verify::run_one` (verify.rs:101-115).
- The room context is `Option<Arc<CartData>>` + `Option<Arc<CollisionCache>>`
  in `Interp` (interp.rs:70-71), with five "no cart" bails. The same pair is
  carried again by `Reference`, `LatticeWalk`, `eval::Env`, `graph::Room` and
  every `Rt2`.
- Clients configure the domain by mutating it around calls and restoring it:
  `it.d.level.fruit` at kernel.rs:229-271 and level_minus_one.rs:418-432, and
  `uncapped_ways`.

**Should be.**
- `Program { top, init, reset, frame, frame_concrete }`, parsed once per
  sources and held in an `Arc`;
- `Program::boot::<D>(&self, d, room: Room) -> (Interp<D>, State<D>)`;
- `body_ids` keyed by `usize`;
- one non-optional `Room` (cart + cache + coordinates);
- per-trace options as a `TraceSpec` argument, not fields toggled on `it.d`.

**Disappears.** Four unsafe impls and the `Worker` wrapper structs, two leak
sites, seven boot copies, `run_chunk`, `refdriver::fresh_interp`, the Option
bails. Estimate: ~130 lines, confidence .85.

**Verification.** t0 (oracle, bind tests, refengine tests), t1
`refcheck`/`arccheck`, t2. This changes no trace.

### T6. One path vocabulary, one object finder

**Now.**
- `iface::Step`/`Path` (iface.rs:20-30) and `slots::PathStep` (slots.rs:17-26)
  are the same enum. slots.rs says it "mirrors" iface's, and `bind::steps`
  (bind.rs:24-32) converts one into the other on every resolve.
- `kind(&Cell2)` is duplicated verbatim (bind.rs:39-48, slots.rs:109-118).
- `widen::field` is re-implemented as a closure in `RegionGrid::bounds`
  (kernel.rs:139-145).

"Find the player" has five implementations:
- `widen::objects_of_type(st, "player")`, by type;
- `shapes::player_path` (shapes.rs:260-271): the same loop again;
- `verify::find_player` (verify.rs:1463-1476), test only, by a `djump` field;
- `refengine::players`;
- on blocks, `Rt2::player_objects` and `concrete::objects_of_type`.

Two `position` helpers exist: `widen::position<D>` and `refengine::position`
(refengine.rs:270-276).

**Should be.**
- The tracer uses `slots::PathStep` as its `Step`.
- One generic `objects_of_type<D>` and one `position<D>` (via `as_const`).
- `player_path` becomes `objects_of_type(st, "player").first()`.

Estimate: ~60 lines, confidence .9.

**Verification.** t0, and t1 frames byte-identical.

### T9. `Domain` is Symbolic's interface; `Concrete` is vestigial

**The divergence is done (2026-10-11, branch `ref-fixes`); the `Concrete`
refactor is not.** Philippe's decision: an overflow RAISES in the
reference, point or interval, rather than wrapping as PICO-8 does (the game
never overflows; an interval's overflow is the kernels' lane error). `+ - *
/`, negation and `abs` go through `Pico8NumInterval::checked_*`
(`checked_mul`/`checked_div` of any two intervals by their corners,
`checked_abs`, new); an overflow is an error naming the operation and
operands, with the Lua source, the fork path, the input and the players.
Interval x positive scalar (and x 0) no longer panics. Found on the way:
`floor_span` in `i16` counted the unknown number's 65536 floors as ONE in
release builds, so `flr` of it was a point and `__split_by_flr` gave one
fragment; `Pico8Num`'s `MIN % -0x0.0001` panicked (`rem_euclid`; now 0).
Still wrapping: the kernels' scalar (`ZN`) columns and the tracer's constant
fold (`Concrete`); only interval columns have `NoWrap`.

**Now.**
- The trait has ~25 methods (domain.rs:103-259). About 14 are defaults that
  only `Symbolic` overrides: the joins, atoms, escape, evaluation sites,
  `flr_ways`, `describe_num`, ...
- `Interp<Concrete>` is instantiated by exactly one test (interp.rs:1935).
  `Concrete` is otherwise a bag of `P8` ops that `Symbolic` uses for constant
  folding (domain.rs:972, 1005, 1039, 1064) and `eval.rs` uses at line 66.
- The docs saying it "builds the initial heap / is the oracle" are stale:
  mod.rs:13 and domain.rs:2-4. cart.rs:4 says the symbolic domain builds the
  heap, and the oracle test runs `Interp<Symbolic>` on constants.

**Divergence found.** `RefDomain::arith` sends **points** through
`checked_add`/`checked_sub`/`checked_neg` (refdomain.rs:135-136, 157). A
point overflow is an error in the reference engine, while PICO-8, `Concrete`
and the kernels wrap; points under `Mul`/`Div`/`Rem` do wrap
(refdomain.rs:138). Interval x positive scalar calls `scale_positive`, which
**panics** on a wrap (refdomain.rs:139-140, pico8_num.rs:358-361); the kernels
decline such a lane via `NoWrap`. The concrete search runs on this engine. It
is latent and loud, not silent.

**Should be.**
- `Concrete` becomes four free functions (`p8_arith/fun1/fun2/compare`, ~35
  lines), used by Symbolic's folding, `eval.rs` and RefDomain's point case.
- The Symbolic-only machinery moves behind a `Tracing` extension trait, so the
  base `Domain` is value semantics only.

Estimate: ~80 lines and a latent bug fix, confidence .85.

**Verification.** t0 (oracle, refdomain tests), t1 `refcheck`/`arccheck`,
t2 `gate10-arc` (the concrete search).

### T21. Layering

- **`refengine`/`refbridge`/`refdriver`/`refdomain` live in `trace/` but sit
  above `frame`, `storage`, `compiled` and `search`.** They import
  `frame::{Block, FrameStep}`, `storage::unit::UnitSink`,
  `compiled::{room_context, ids}` and `search::pos_graph` (refengine.rs,
  refbridge.rs:330). Meanwhile `trace::kernel` reaches up to
  `search::pos_graph::cell_xy` (kernel.rs:133) and to `concrete`, which uses
  `RefEngine`. The reference engine is a `FrameStep`, the peer of
  `compiled::FrameEngine`. Put both in one `engines/` module and the tracer
  stays below them.
- **Both FrameSteps do the storage's bookkeeping.** RefEngine inserts
  pos-graph pairs into `sink.pos_edges` under `sink.edges_on` itself
  (refengine.rs:341-345), as asm_kernel.rs:582-584 and 620-622 do. The sink's
  emit should take `(row, cell_in, cell_out)` and record the pair.
- **The engine crate knows the game.** `BoundaryIds` (runtime2.rs:94-127)
  carries fruit, fly fruit, key, chest, fake wall, big chest, platform and
  balloon ids for `berry_lost`, the orb deadline and `lookup_view`. `Rt2` has
  `player_objects`/`position_object`/`player_xy_cells`. The widening table
  needs some of these ids (widen area: coordinate). The category and search
  ones belong in a `game` module in `celeste-rust`, next to the heap-side
  queries of T6.

Few lines, but this is the layering the rest needs. Verification: t0 and t1.

---

## Other findings

### T1. Dead code and pass-through wrappers (~190 lines)

- `Symbolic::abstractness` (domain.rs:520-569), its memo (411-412, 444) and
  its test (1567-1580). Only its own test calls it; error.rs:201 names it in a
  comment.
- `Symbolic::reads_interval_cmp` (domain.rs:649-666): no caller.
- `Symbolic::range_folds` (domain.rs:427-428, 438, 1120): written, never read.
- `Interp::for_iterations` and `merge_fallbacks` (interp.rs:79-84, 122-124,
  524, 560, 738): written, never read.
- **The walk's fork census** (kernel.rs:774-795). Per job it computes
  `bdd::reachable` over the worker's whole arena and scans every node, to
  fill `LatticeWalk.forks/forkops`. Nobody reads either (also kernel.rs:
  392-394, 551-554, 728-731). `LatticeWalk.graph` (a full arena copy,
  kernel.rs:674) and `.opts` are never read either. Its cost is unmeasured.
- `Lowered.bodies` (emit.rs:21-23, 326): never read.
- `eval::run(.., strict)` (eval.rs:63, 183): always `true`.
- The `"__split_at"` arm (interp.rs:1673): no Lua file calls it.
- One-line wrappers with one caller each:
  - `shapes::boundary_widened_paths`, `shapes::ival_paths`,
    `kernel::boundary_ival`;
  - `bind::bind_inputs`, `cart::sources`, `refdriver::fresh_interp`;
  - `Interp::decide`, `Rt2::boundary_widen`;
  - `state::merge_on`/`merge_canon` (state.rs:188-209: three entry points
    with the `same_heap` dispatch twice).

Verification: a warning-free build, then t0. The census goes without changing
anything t1 pins.

### T2. Boilerplate the `D: Domain` generic forces (~100 lines)

These are hand-written because `#[derive]` would bound `D: Clone`, and
`RefDomain`/`Concrete` are not `Clone`:
- heap.rs:44-72 (`Value` Clone/PartialEq);
- heap.rs:183-216 (`Table`, `Scope`);
- heap.rs:253-273 (`Heap`);
- state.rs:86-96 (`ArcAxis`) and 139-153 (`State`);
- interp.rs:22-30 (`Flow`);
- verify.rs:59-76 (`FrameOut`, through `clone_block`).

Fix: derive `Clone` on the three domains and on `Cursor`, then derive the rest.
Keep `Value`'s tagged `Debug`. In the same spirit, `Rt2` is "deliberately not
Clone" (slots.rs:123) yet has `clone_block` (runtime2.rs:630), and a test
builds a structure twice to avoid cloning it (bind.rs:525). Make it `Clone`.

Verification: t0. This is mechanical.

### T3. Test-side duplication (~110 lines)

- Seven `#[cfg(test)] pub fn` helpers live in the production module
  (verify.rs:1461-1616).
  - `find_player` duplicates `shapes::player_path`.
  - `set_buttons` duplicates `refbridge::set_buttons` for the other domain.
  - `pm1_paths`/`pm1_key` keep annex D's dead "pm1" word alive for one test.
  - Move them into the test module and reuse the production helpers.
- "A heap scalar as `Conc` if it is a constant" is written five times:
  iface.rs:205-215, iface.rs:258-268, verify.rs:1496-1506, shapes.rs:241-252
  and shapes.rs:112-123. One `conc_of(d, &Value)` replaces them.
- `a_traced_frame_agrees_with_the_oracle` prints outcome tables and "where the
  first same-size pair parts company" (verify.rs:1967-2024, 2117-2140). Only
  `same == 0` is asserted ("diagnostics are not tests").
- The test at kernel.rs:325-327 passes vacuously when the Lua file is missing.
- `probe.rs` is a `pub mod` whose every item is test-only. Interp's
  `printh`/`prints`/`fmt_p8` (interp.rs:77, 1631-1647, 2126-2134) serve only
  the probe; refdriver.rs:67 clears `prints` for nothing.

### T4. One worker pool (~70 lines here, more crate-wide)

The pattern "AtomicUsize queue, `spawn_scoped` with a big stack, join, sort by
index" appears 13 times. The copies in this area or next to it:
kernel.rs:63-97 and 461-522, asm_kernel.rs:1301-1331, verify.rs:1743-1781 and
2048-2096, refengine.rs:433-460.

Fix: `util::par_map(items, stack, per_worker_state, f)`, returning results in
item order. Verification: t0, plus t1 (the walk's determinism; frames
identical).

### T8. Duplication in `interp.rs`/`state.rs` (~100 lines)

- `collapse` and `collapse_values` (interp.rs:503-572) are one loop that
  differs in two closures. Make it generic over a `Joinable` (Flow / Value).
- "Split, run each side, concatenate, or collapse if both are live" is written
  three times: `exec_arms` (473-492), `and`/`or` (961-988) and the symbolic
  `for` (700-707).
- `run_for` (752-792) inlines `for_body` (731-750).
- The test helper `fresh` (interp.rs:1899-1906) duplicates
  `cart::fresh_state`, and the state.rs tests spell a `State` literal six
  times (513, 536, 591, 621, 636, 658).

Verification: t0 (interp/state tests and oracle) and t1 frames. The order of
merges must not move.

### T11. RefEngine's eight entry points (~45 lines)

`step`, `step_reads`, `frame`, `frame_reads`, `step_at`, `step_at_pinned`,
`step_one` and `run_lane` (refengine.rs:94-208). Three of them repeat
`from_block -> set_buttons -> button_reads -> run_frame_all -> to_block`.

Fix: one `run(row, lane, Inputs::{Byte(b), Forked}, level, pins)
-> (Vec<Block>, u8)`, plus the two thin wrappers callers actually use
(`step_one`, `frame_reads`).

`run_frame_all` takes the `level` only to refuse `p` and to concretize near
floors (refdriver.rs:51-75). That belongs to the engine's setup.
Verification: t0 refengine tests, t1 `arccheck`, t2 `gate10-arc`.

### T12. Domain ops mirror `Op` (~100 lines)

- `Arith`/`Fun1`/`Fun2`/`Cmp` (domain.rs:16-47) are four slices of
  `graph::Op`, and the mapping is spelled out six times: domain.rs:991-997,
  1026-1031, 1053-1056, 1081-1087 and **again** 1125-1131 in the same
  function; eval.rs:99-128; verify.rs:1439-1445.
- `Symbolic::arith/fun1/fun2/compare` (domain.rs:969-1133) each repeat the same
  five-step preamble: constant fold via `Concrete`, unknown number, select of
  the unknown on either side, literal evaluation.

Fix: `Domain::apply(op: Op, args)` with one preamble. Confidence .7 because the
fold order must stay exact. Verification: t0 oracle, t1 frames and metrics
byte-identical.

### T13. Duplication inside the split pass (~40 lines)

- `MayErr::may` and `split_undecided_selects` both find the lowest
  lane-undecidable atom, compute the sides over `Points`, then `may_answers`,
  then rebuild (verify.rs:553-566 and 925-944).
- `three_valued` and `arms` each expand the arm combinations under `ARMS`
  (verify.rs:1170-1194 and 1315-1332).

Verification: t1 frames and metrics, and t2 forward60 (the platforms decide
over `Points`).

### T14. Six memoized graph predicates (~80 lines)

`lane_undecidable`, `abstract_beneath_lane_ops`, `is_interval`,
`reads_unknown_atom`, `lane_independent` and `may_answers`, plus the dead ones
of T1. Each has its own recursion and its own memo (domain.rs:398-418, five of
them in `RefCell`s), reset together by `forget_intervals` (441-448).

The arena is append-only and an argument always precedes its node. So one
`Vec<Facts>` (a bitset per node), filled forward from the last computed id and
cleared when `ival_cells` changes, gives every predicate as a bit read. The
per-op tables stay, deliberately different.

Confidence .6: the tables must be transcribed exactly. Verification: the
domain tests (the fork-trigger contract), the oracle, t1.

### T15. Fork origins as strings (~35 lines)

Every fork records an origin `String` (domain.rs:385-386, 805). Escaped atoms
build theirs from closures that call `describe_num` (interp.rs:1412-1445,
domain.rs:1357). Its only consumers are `--build-trace` and the test helper
`at_buttons`, which finds the button forks by comparing against the string
`UNKNOWN_BOOL_ORIGIN` (verify.rs:1543).

Fix: `enum ForkOrigin { Button(u8), Floor, Escaped }`, and record
`Frame.buttons: [u8; 6]`.

### T16. Builtins (beyond rm-3)

There are three lists: `cart::NATIVE` (cart.rs:16-36), core's
`BUILTIN_NAMES`, and the arms of `call_builtin`.
- NATIVE installs the dead `__widen_rem`, `__new_vector`, `__split_at` and
  `error` as globals. They are in `GLOBAL_NAMES`, so **every block carries
  their `Bi` cells**. Dropping them changes every shape hash, a re-pin.
- `to_block` silently skips a fourth set, the non-ABI hints `_hint_normalize`
  and `printh` (refbridge.rs:334-337).

Fix: one `Builtin` enum in core with the ABI index; NATIVE derived from it.
Drop the dead interpreter arm now and the dead globals at the next format
change.

### T17. `Rt2::prints`

The field is never pushed to (runtime2.rs:138), and the boundary asserts it is
empty (582). It is still copied into checkpoints and the storage meta
(search/checkpoint.rs:44, 86, 240; storage/wave.rs:724; decode.rs:48, 197;
convert.rs:39, 105). Removing it needs a format bump (storage area).

### T18. Shape identity

The walk keys everything (lattice, reps, regions, frames, `ival_extra`) on
`format!("{:?}", st.shape()?)`: a Debug string of the whole heap shape, built
per outcome per trace (kernel.rs:388, 807). It then needs `by_hash: u64 ->
String` (kernel.rs:675-678) to meet the kernels, which key on
`Rt2::shape_hash_of`. `FrameOut.shape` (a third form) is used for dedupe
(verify.rs:880).

asm_kernel already refuses two walk frames with one hash (asm_kernel.rs:1336).
So the walk can key on the `u64` and keep `heap::Shape` only inside merge,
with a collision assert. Level -1 consumes the String keys: coordinate. There
is a memory and time upside here, unmeasured.

### T19. Interpreter special cases keyed by spelling

- `unroll_bound` keys loop bounds on **source text** (interp.rs:1803-1811).
  Where restrictions exist (the region grid), `range_of(limit)` gives the same
  bound; keep the text table only for `--region off`.
- The countdown hint comes from the operand's spelled field name
  (interp.rs:994-1002).
- The arc capture parses the argument's spelling `<obj>.rem.<x|y>` on every
  call and reads the locals `ox`/`oy` by name (interp.rs:1502-1538). The two
  call sites in `move` could be identified once at boot instead.
- `room` is read by name in four places: state.rs:216-224,
  interp.rs:1294-1325, shapes.rs:32-52 and 205-217.
- `"__button_states"` appears in four files.
- The type names `player`, `player_spawn`, `fall_floor` and `fly_fruit` appear
  across six files.

Fix: a `cart::names` module, and interpreter hooks instead of fields:
`button_reads` (interp.rs:1159-1166) and `arc_capture`/`split_site` are
concerns of the concrete search and the arcs, not of a Lua interpreter.
Confidence .5.

### T20. Two "run the room with no input" mechanisms

`no_player_ranges` (kernel.rs:213-318) runs the tracer fully pinned, with
`rebase`, for up to 240 frames, saving and restoring `level.fruit`.
`concrete::platform_worlds` (concrete.rs:71-92) runs `RefEngine::step_one` for
127 frames. The first could read its per-shape hulls from reference states
(`from_block` -> `shape()` + `scalars`).

Confidence .45: the reference forks on an `rnd` draw where the pinned chain
stops, and the hulls must stay identical (t1 frames, t2 forward60).

---

## Considered and rejected

- **The symbolic trace (or the kernels) as the reference.** The concrete search
  is the proof's exact half. It must not depend on the tracer's merge, split,
  fold or codegen, which are what it refutes. `ref-check` and the oracle test
  only sample those. Keep RefEngine independent.
- **RefEngine fan-out instead of re-execution.** `Interp` can already fan
  states out (`Multi`, `literal_fragments`). With `decide -> None` and merging
  off, one run would share prefixes instead of re-running per cursor path,
  dropping `Cursor` and the driver loop (~100 lines). But it holds every live
  path at once, where DFS re-execution holds one. At the 1M path cap and
  arc-check's object levels, that is the memory the pins exist to avoid.
  Deferred until path counts are measured.
- **Sharing interval semantics between `RefDomain` and `Graph::eval_full`.**
  They differ: `Abs` is checked in one and unchecked in the other; `Sin` of an
  interval is `[-1, 1]` in one and a bail in the other; a straddling compare
  forks in one and is `None` in the other. Merging them would make the
  reference and the fold share bugs. Add an agreement property test instead.
- **`trace::eval` replaced by `Graph::eval` on points.** No: `Graph::eval`
  errors where PICO-8 wraps exact arithmetic (`checked_add`), and `eval.rs`
  delegates to the P8 ops on purpose.
