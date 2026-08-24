# Deleting the interpreter, the IR, and the walk path (scoped 2026-08-24)

Philippe's ask: keep the traced kernels and the engine, delete everything
else - the vectorized interpreter, the rewrite remnants, the walk kernels.
This is the scope. No code has changed for it yet.

## The shape of the problem in one paragraph

The TRACER is already independent. `src/trace` (12.0k lines) parses the
Lua with `full_moon`, evaluates it on its own AST interpreter in two
domains (`Concrete` = its own oracle, `Symbolic` = the graph), and its
only call into `celeste-interp` is `game_runner::start_room()` - a pair
of integers read from `CELESTE_START_ROOM`. The ENGINE (`celeste-engine`,
3.1k) depends on `core` and `names` only. The traced kernel set for room
(1,0) is checked in and has run the production horizon: f94, 172,626,763
lanes, **missed 0, plain-routed 0, every class kernel 0 lanes**, 1.50x
the interpreter's speed at 55% of its peak (BENCHMARK_DATA.md
2026-08-24). What is glued to the interpreter is not the kernels but
**the campaign around them**: the forward driver, the checkpoints, the
visited set, the backward sweep, the precision ladder, and the oracle.

## Inventory

Hand-written lines, by what they are attached to.

| group | lines | what |
|---|---|---|
| **keeps** | | |
| `celeste-core` | 1,184 | pico8 numbers, cart, collision cache |
| `celeste-names` | 262 | GENERATED name tables; the shape hash and row key order |
| `celeste-engine` | 3,137 | `Rt2`, boundary, merge, dedup, lane primitives, slots, `Kernel` |
| `src/trace` | 11,997 | the tracer, its frame loop, dispatch, verify, PICO-8 probe |
| `src/transpile` (traced half) | ~5,000 | graph, bdd, ival, lower, and the traced half of `kernel.rs` |
| `celeste-kernels/src/traced` | 32,538 gen | room (1,0)'s three shape kernels |
| **goes with the walk path (Phase 1)** | | |
| `kernel_gen_*.rs` x8 + `fused_gen_player.rs` | 14,429 + 4,219 gen | class kernels; took 0 lanes at f94 |
| `transpile::fuse` | 993 | fused specialization set |
| `transpile::names` generator | ~400 | walks the IR to produce `gen.rs`; `gen.rs` itself stays, frozen |
| `transpile::kernel` walk mode + loop-fork emitter | ~1,000 est. | `emit_kernel`, `Emit{flat_forks:false}`, the `for cN` nest |
| `compiled::dispatch` class registry | ~600 est. | `class_kernel_runner!`, fused dispatch, miss dumps |
| witnesses, recipes, frozen programs | 8 + 12 + 8 files | `witness/*.json`, `rewrites-trace{10,20}-*.jsonl` and their `.program.zst` |
| `regen-generated.sh`, `fused` feature, ~10 env knobs | | |
| **goes with the interpreter (Phase 4)** | | |
| `celeste-interp` | 14,387 | the interpreter, vectorize, virtual merge, abstraction, visited, checkpoint format |
| `celeste-ir` | 2,725 | IR, Lua->IR frontend, builtins |
| `src/program` | 1,077 | `Program`, recipes, frozen artifacts |
| `src/search` | 5,984 | REPLACED, not deleted: the driver, checkpoints, sweep, pos graph are all over `State` |
| `src/compiled` | 2,637 | `FrameEngine` + `bridge` (State<->Rt2). `boundary_ids` and the traced adapter survive |
| `src/main.rs` | 2,837 | the un-rewritten `celeste-rust -n` runner and 52 interpreter unit tests |
| `src/bin/rewrite.rs` | 2,464 | campaign CLI; every subcommand drives `AbstractRun` |
| `native-probe` | 2,280 | engine gate harness, State-based |
| `concrete.rs`, `concrete_run`, `lua_run` | 427 | interpreter tools |
| remaining recipes + frozen programs | 8 + 2 files | `rewrites.jsonl`, `rewrites-compile.jsonl`, room00/20, death... |
| `plans/*.md` that describe deleted things | ~20 files | |

Roughly **55k hand-written lines go, 22k stay**, plus 18.6k of generated
class kernels out and 32.5k of generated traced kernels in.

## What the interpreter still does that nothing else does

Each of these is a thing to BUILD before the crate can go. None is
optional; the first is on the critical path of the search itself.

### 1. The precision ladder (the big one)

`ladder.sh` runs level 0 and then k=1..16 banded levels. Level 0 is what
the compiled path does: `Rt2::boundary` ports `make_state_abstract` at
`RemPrecision::Bits(0)` and `SpdPrecision::Exact` ONLY (runtime2.rs:551,
"other precisions are follow-up work"), and `compiled_forward`
(run.rs:1236) bails on any other `CELESTE_REM_BITS` /
`CELESTE_SPD_WIDTH_LOG2`. **Every finer rung runs on the interpreter
today.** The rungs are `abstraction.rs`: `make_state_abstract_rem(Bits(k))`,
`make_state_abstract_spd(WidthLog2(w))`, `coarsen_to`,
`split_precision_straddles`, `win_lane_mask`.

Philippe, 2026-08-24: the whole ladder is required in the compiled path,
same forward/backward flow, same refinement; compiling one kernel set
per rung for a long time is acceptable.

Since the level-0 widenings were moved INTO the graph (tracing.md "DONE:
the widenings moved into the graph"), the natural shape is: a rung is a
parameter of the trace, and `write_room_kernels(room, rung)` emits a set
per rung. The boundary's key must agree with the kernel's widening at
every rung - the "dangerous part" note in tracing.md STATE applies
per rung, not once. CLAUDE.md's rule is the whole point: a widening with
no rung that narrows it is unsound for the optimum claim.

### 2. A block-native campaign driver

`AbstractRun` (run.rs, 2,645 lines, 86 interpreter references) is the
production driver and it is written over `interpreter::State`.
`FrameEngine::run_frame_chunk` is called per 8,000-lane chunk through
`bridge::import_block` / `export_block`, so today every frame crosses
State -> Rt2 -> kernels -> Rt2 -> State. `trace::run::Run` (270 lines) is
the block-native loop and is deliberately not that engine; it has no
chunking, no threads, no visited set, no checkpoint, no band filter, no
frontier subtract, no pos-graph recording, no win detection.

What the new driver needs, each of which exists today only over `State`:

| | today | lines |
|---|---|---|
| chunking + 16 threads + streaming boundary | `run.rs` | ~900 |
| visited set (mmap engine, fp-run index) | `interpreter::visited`, `row_table` | 1,010 - **State-free already**, keyed on `(u64,u64)`; MOVES to the engine |
| frontier subtract | `vectorize::subtract_*` | ~300 |
| band filter (e, g) | `run.rs` + `sweep.rs` | ~250 |
| checkpoint | `checkpoint.rs` + `inspect.rs` | 762 + 628; saves `Vec<State>`, so the FORMAT changes and existing campaign checkpoints do not carry over (fingerprint bump) |
| deopt -> exit with status | doctrine, tracing.md | `Run` already errors; needs the aggregate-and-checkpoint half |
| win detection | `abstraction::win_lane_mask`, `CELESTE_WIN_AT_XY` | small |
| pos-graph recording | `pos_graph.rs` | 669; simpler on blocks (player cell is a column) |

### 3. The backward sweep

`sweep_time.rs` (821) re-derives successors per frame by replaying the
forward frame through `AbstractRun`. It needs the new driver's frame
function and nothing else new; `sweep.rs` (203) is the vocabulary and is
representation-independent.

### 4. Room coverage

Only room (1,0) has a traced set. Room (2,0) traces (36 shapes, 1.06 M
lines, 296 s compile) and the artifact model is an open decision. Room
(0,0) has no kernels of any kind and runs interpreter-only today; under
the doctrine that is a coverage gap to close by tracing it, and whether
the tracer handles (0,0) has not been tried. Deleting the interpreter
makes "no kernel" a hard stop for that room until it is traced.

### 5. The oracle

The interpreter is checked against in three places:
`traced_kernels_reproduce_the_interpreter` (12 frames, lane counts under
`CELESTE_COMPILED_FORWARD=check`, which compares row-key SETS per chunk),
`a_traced_frame_agrees_with_the_oracle` (actually the tracer's OWN
`Concrete` domain, not celeste-interp), and the f94 lane-count identity
in BENCHMARK_DATA.md. What replaces it:

* **Frozen goldens** - the interpreter's per-frame row-key-set digests
  for room (1,0) f1..f94 (94 x 16 bytes, checked in), and for every rung
  the ladder has run. Generated while the interpreter exists; a test
  asserts the block-native search reproduces them. This preserves the
  evidence after the witness is gone and is the cheapest thing in this
  plan.
* the tracer's `Concrete` vs `Symbolic` (same code, catches emitter and
  lowering bugs, not tracer bugs);
* the PICO-8 corpora in `lua/probe` (Lua semantics, against the real
  thing);
* a concrete TAS replay through the tracer's `Concrete` domain, replacing
  `concrete_run`;
* batch invariance (`simdcheck.sh`, `parcheck.sh`), which needs no
  oracle at all;
* the OCaml implementation stays the semantic reference, as CLAUDE.md
  already says.

### 6. `celeste-names`

`gen.rs` is produced by `transpile::names` WALKING THE IR of the frozen
`rewrites-compile` program; `FIELD_NAMES`' order is the shape hash and
the row key. The tracer reads `FN_NAMES` (by base name), `GLOBAL_NAMES`
and `FIELD_NAMES` through `engine::slots`. Deleting the IR freezes
`gen.rs` for good: a new field name (only possible if the Lua changes)
would be APPENDED by hand, which is order-preserving and therefore safe.
`generated_is_current{,_r20}` go; a header comment says what the file is
frozen from.

## Order

**Phase 1 - the walk path.** No new code, no decisions beyond one.
Class kernels, fused set, `transpile::fuse`, the walk mode of
`transpile::kernel` including the loop-style fork emitter, the class
registry in `compiled::dispatch`, the trace10/trace20 recipes and frozen
programs, the witnesses, `regen-generated.sh`, the `fused` feature, the
`CELESTE_KERNEL_CLASSES` / `CELESTE_FUSED*` / `CELESTE_KERNEL_MISS*`
knobs, `generated_is_current{,_r20}`. Gate: the suite,
`traced_kernels_reproduce_the_interpreter`, and a 30-frame
`CELESTE_COMPILED_FORWARD=check` run. Evidence it is safe: the f94 log
(`/tmp/tk-traced.log`) shows every class kernel at 0 lanes with the
traced set first.

The one decision: deleting the loop emitter commits the traced kernels
to `flat_forks`, which is currently UNCOMMITTED and measured at +67%
lines on room (1,0) kernel1 and 2.13x on room (2,0). Either accept that
and fix the growth inside the flat design, or settle it first.

**Phase 2 - pin the evidence.** Generate the frozen goldens (item 5)
from the interpreter for every room and rung it has run. Cheap, and it
is what makes Phase 4 checkable. Do it BEFORE any driver work so the
goldens are from an untouched interpreter.

**Phase 3 - build.** The rung-parameterized trace and kernel sets (1),
the block-native driver with the visited set moved into the engine (2),
the sweep over it (3), room (2,0) and (0,0) traced sets (4). Gate every
step against the goldens AND against `CELESTE_COMPILED_FORWARD=check`
while the interpreter still exists - check mode is the strongest gate
this repo has and it dies with the interpreter, so spend it now. This is
also where the terminology simplification belongs: the driver is new
code.

**Phase 4 - delete.** `celeste-interp`, `celeste-ir`, `src/program`,
`compiled::bridge` and `FrameEngine`, `src/main.rs`'s runner,
`rewrite.rs`'s State half, `native-probe`, the concrete tools, the names
generator, the remaining recipes and frozen programs, the stale plans.

## Decisions needed from Philippe

1. Phase 1's fork-emitter question above.
2. Rung representation: one traced kernel set per rung (his stated
   preference) - confirm, since it multiplies the artifact problem in 4
   by up to 17.
3. Room (2,0)'s artifact model (generated per campaign, gitignored,
   fingerprinted - the `fused` pattern), still pending from tracing.md.
4. Accepting frozen goldens + tracer-concrete + PICO-8 corpora as the
   oracle after the interpreter is gone.
5. Existing campaign checkpoints under `~/celeste-checkpoints` become
   unreadable by the new driver. Fine, or keep a one-shot converter?
