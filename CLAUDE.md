# Claude Code Guidelines

## What this project is

An abstract interpreter for PICO-8 Celeste, used to search for a provably
optimal TAS. It compiles the original Lua to a CFG IR and runs a vectorized
abstract interpreter over sets of game states.

Read these before doing anything substantial:

- `plans/strategy.md` - the overall search strategy (forward/backward
  refinement). Mostly still future work.
- `plans/rewrite-plan.md` - the current work: rewriting the compiled program
  into a branch-free, call-free, compilable form via a checked-in list of
  individually-verifiable rewrite instructions.
- `BENCHMARK_DATA.md` - current performance baseline. Keep it current; the
  previous version of that file was stale by 25x and nearly caused bad
  decisions.

The original OCaml implementation is at `~/src/github.com/tehwalris/celeste_ocaml`
and is still the reference for interpreter semantics. Read it when implementing
Rust equivalents.

## Ground rules

- This is a complex project. Take your time. Do not take shortcuts due to time
  pressure.
- Don't create parallel/simpler implementations that bypass existing
  infrastructure.
- **Correctness beats cleverness.** A transformation we cannot check is worse
  than no transformation. If you cannot verify something, insert a runtime
  guard instead of assuming.
- **Never widen a field without a rung that narrows it back.** Every
  widening is an over-approximation. That is sound for REFUTING a horizon -
  if the coarse pass finds no win by H, the concrete game has none - but the
  ladder reports "concrete optimum = H" when all 17 levels win at H, and
  that conclusion rests on the top rung being EXACT in every coordinate.
  `player.rem` is fully widened at level 0 and gets away with it only
  because k=16 narrows it back, so a spurious coarse win is refuted by a
  finer level. A widening applied at EVERY level is refuted by nothing: it
  survives to k=16 and the ladder reports a spurious win as the optimum.
  This has come up twice (`p_jump`/`p_dash`, 2026-08-06 and again from the
  field census 2026-08-16), so: the cost of a new abstraction is the
  abstraction PLUS its refinement ladder, and anything advertised as a free
  merge is mispriced.
- **Never deopt to the interpreter.** A deopt is a coverage gap, not a
  degraded mode. The search checkpoints every frame; a deopt confirms the
  checkpoint, reports every distinct reason with lane counts, and exits
  with a distinct status. Fix the gap and resume. Once Stage 4 removes
  the interpreter's vectorization, absorbing a deopt costs orders of
  magnitude more than fixing it. See `plans/tracing.md` "Doctrine".
- **Measure before and after.** Any change that claims a performance effect
  needs numbers from an actual run, not reasoning.
- Do not leave dead code behind. The build is warning-free; keep it that way.
  A previous crate-wide `#![allow(dead_code)]` hid ~150 lines of dead code and
  an entire abandoned dataflow framework.

### A note on history

There was a previous attempt (Jan 2026, now the
`interpreter-abandoned-2026-01-11` branch) at a generic CFG optimizer - mem2reg, inlining, heap elimination, call resolution, DCE, etc.
Roughly 11k lines. Most of it was never wired into the game runner, some of it
produced CFGs the interpreter cannot execute, and its validation did not check
dominance. That branch is kept as reading material only. Do not reintroduce
untested passes; see `plans/rewrite-plan.md` for the approach that replaces it.

## Branches

`interpreter` is the live line (the rewrite campaign plus the current
interpreter work). `parallel-experiments` holds the overnight parallelism,
which is deliberately not merged - see BENCHMARK_DATA.md.
`interpreter-abandoned-2026-01-11` is the January CFG optimizer above.

## Running safely

Use `./safe-run.sh` for EVERYTHING that builds or runs - `cargo build`,
`cargo check`, nextest, `transpile`, benches, the forward search - and
that includes commands issued by subagents. It runs the command in a
systemd scope with `MemoryMax=60G` (Philippe's default, 2026-08-25) so an
accidental blowup kills the process rather than the machine.

Why "everything" and not "the expensive things": on 2026-08-24 a process
that ran OUTSIDE the wrapper reached 120 GB twice (system-wide OOM,
`CONSTRAINT_NONE` in the kernel log), and the second time it took the
whole Claude session and a background deletion agent with it. The
agent's hour of work was uncommitted and is gone. The wrapper costs
nothing; the judgment call about which command is "cheap enough" is
exactly the thing that fails.

```bash
cargo nextest run <filter>                                      # DEV LOOP, 1.3 s
./safe-run.sh -- cargo nextest run --cargo-profile quick         # PRE-COMMIT, 47 s
./safe-run.sh -- cargo nextest run --cargo-profile quick --run-ignored all  # rare, ~6 min
./safe-run.sh -- ./target/release/celeste-rust -n 40
```

### Iterating quickly - read this before running anything

**Pick the cheapest thing that answers your question.** Three profiles,
and the mistake is always reaching for the most expensive one out of
habit. Measured 2026-08-22, after the crate split:

| what you want | command | cost |
|---|---|---|
| "did my edit compile and does its unit test pass" | `cargo nextest run <filter>` (DEBUG) | **1.3 s** after an edit, 0.2 s no-op |
| "does the whole suite still pass" | `cargo nextest run --cargo-profile quick` | **47 s** build + run |
| the pre-commit run | `cargo nextest run --cargo-profile quick` | **47 s**, same as above |
| anything you will quote a NUMBER from | `--release` | ~110 s build + run |
| deliberately running the `#[ignore]`d tests (pm1 key walk, diagnostics) | `--cargo-profile quick --run-ignored all` | ~6 min |

**Never use `--release` for unit tests.** This is the actual trap. The
release profile is `lto = "fat"` + `codegen-units = 1`, so a one-line edit
relinks the whole workspace - ~100 s to run a test that EXECUTES in 6 ms.
I did this repeatedly on 2026-08-22 before noticing. `[profile.quick]`
exists for when a test genuinely needs optimization (the compute-bound
ones: `traced_kernels_reproduce_the_interpreter`,
`asm_kernels_reproduce_the_interpreter` and its ladder/exact variants,
`shape_variant_dispatch_reproduces_the_baseline`).

Do NOT run the full suite in plain debug: those same tests are
compute-bound (`compiled_forward_reproduces_the_interpreter`, since
deleted, went 20 s -> 153 s). Debug wins
when a filter keeps them out; `--cargo-profile quick` wins when it cannot.

**Tools get `--profile quick`, not `--release`.** `transpile` (now just
the analysis probes: `--room-consts`, `--spec-probe`) prints text; no
number anyone quotes comes out of it, so fat LTO buys nothing. 15 s to
build under quick against ~78 s under release. (I wrote the rule below
and then immediately reached for `--release` to build `transpile`,
because it is genuinely too slow in debug. "Too slow in debug" argues
for OPTIMIZATION, not for the gate's profile.)

**`--release` is for the gate and for benchmarks only.** Every number in
BENCHMARK_DATA.md was measured under it. Never make `[profile.release]`
cheaper to speed the loop up - that silently reprices every recorded
result. Add a profile instead.

**Other things that cost more than they look:**

- **One cargo at a time - use `./one-cargo.sh`.** A `cargo build`
  started while a background `nextest` is still building will contend on
  the lock and can swap the binary under a running A/B measurement. On
  2026-08-22 this invalidated a benchmark side and it had to be re-run.

  The failure mode is not an error. Cargo prints `Blocking waiting for
  file lock on build directory` ONCE, into a log nobody is tailing, and
  the second build then appears to take as long as the first one has
  left. On 2026-08-23 that cost hours: builds "taking 25 minutes" were a
  70-second build queued behind another of mine, and it also produced a
  false alarm about a hung test and a bogus theory about the emitter.

  `./one-cargo.sh cargo ...` takes an flock on the build directory, so a
  second invocation WAITS and says so instead of silently queueing.
- **Background anything over ~30 s** (`run_in_background: true`) and use
  a Monitor with an until-loop to wait. Do not poll in a loop.
- **`touch` the file you care about** to measure what an edit really
  costs: `touch src/transpile/lower.rs && time cargo nextest run transpile`.
  Guessing at build cost is how the stale table this replaced survived.

**The pre-commit run is `--cargo-profile quick`, and it does NOT include
the ignored tests.** `--release` is for numbers only; gating correctness
under fat LTO costs ~110 s of relink to run tests that execute in
milliseconds, and it contradicts "never use `--release` for unit tests"
three lines above.

The slow test that is `#[ignore]`d is
`every_reachable_pm1_key_gets_its_own_body` (~240 s).
(`every_checked_in_recipe_replays` went with the rewrite rules,
`generated_is_current{,_r20}` with the walk kernels, and the
`*_kernels_are_current` staleness gates with the generated kernel crates
themselves, 2026-08-29 - there is no checked-in artifact left to go
stale.) `#[ignore]` over an
env check on purpose: nextest prints them as skipped, so the skip is
visible rather than silent.

**They are not run on every commit** (Philippe, 2026-08-23): they cost
~6 min, and the expected cost of occasionally breaking one and bisecting
back to it is lower than paying that on every commit. Run them when you
have a REASON to think they will fire:

- touched the tracer, the lowering, or the ASM codegen -> nothing extra:
  the `asm_kernels_reproduce_the_interpreter` gates (+ ladder/exact) and
  `every_start_room_kernel_graph_asm_compiles_the_fused_graph` already
  run on every commit, and there is no staleness gate to run by hand
  because the kernels are assembled at startup, not checked in;
- touched the tracer's pinning or key walk ->
  `every_reachable_pm1_key_gets_its_own_body`.

Do NOT read past the "N skipped" line and call the suite green when one
of those reasons applies. That is the exact mistake behind `a8f4635`.

### Diagnostics are not tests (Philippe, 2026-08-24)

A `#[test] #[ignore]` that asserts NOTHING and only `eprintln!`s a table
is a diagnostic wearing a test's clothes. It is the wrong shape twice:
nothing fails when its answer changes, and the next person running
`--run-ignored all` pays its full cost for output nobody reads. Five of
these exist today, all zero-assertion:

| | |
|---|---|
| `trace::kernel::what_specializing_the_fork_would_cost` | ~120 s |
| `trace::kernel::how_many_variants_write_the_same_row` | |
| `trace::kernel::how_much_of_a_frame_is_erased_immediately` | |
| `trace::verify::tracing_a_frame_with_everything_symbolic` | |
| `trace::verify::specialising_to_one_player_position` | |

They belong behind ONE binary - `src/bin/probe.rs` with a subcommand
each - not in the test harness. `#[ignore]` is for tests that assert
something and are slow (`every_reachable_pm1_key_gets_its_own_body`),
not for a hiding place.

Until that binary exists: do not add a new zero-assertion `#[test]`.
Write it as a subcommand, or give it an assertion that states the
finding it exists to defend.

Run the suite with NEXTEST, never bare `cargo test --release`: the tests
are fine (21 s wall for all 515 under nextest, 2026-08-16) but several
of them mutate process-global state (instr_time, partition toggles),
and under cargo test's shared-process harness the
suite has twice been observed degrading to ~70-85 MINUTES at one core.
nextest runs each test in its own process, which contains every such
leak by construction.

Exit code 137 means OOM. One job needs MORE than the 60 GB default:
room (0,0)'s level-0 position-graph replay peaks at 101.08 GB on its last
frame, so that campaign runs `ladder.sh` with `MEM=108G` and builds the
graph a few frames per process. See BENCHMARK_DATA.md; do not raise the cap
past what `free` leaves after /tmp, which is a tmpfs.

Peak memory is much lower than it used to be (the
chunk-parallel work took frame 60 of the rewritten path from 4.3 GB to
2.4 GB), but the unrewritten `celeste-rust -n 40` runner is still the old
~29 GB - do not run higher frame counts unsandboxed.

The search runs on 16 threads with an 8,000-lane chunk cap by default.
Those two are ONE setting: threads without chunking undoes the streaming
boundary's memory bound, and chunking without threads is a ~23% loss. See
`plans/roofline-plan.md` before changing either, and `./parcheck.sh` to
re-check that the parallel path stays byte-identical to the serial one.

## Crate layout

A cargo workspace since 2026-08-18 (task #150). The dependency order is
load-bearing, not cosmetic:

```
crates/celeste-core      pico8_num, cart_data, collision_cache   deps: -
crates/celeste-names     FROZEN name tables                      deps: -
crates/celeste-ir        ir, frontend (Lua -> IR), builtins,     deps: core
                         print
crates/celeste-interp    the INTERPRETER (the oracle),           deps: core, ir
                         game_runner, its instrumentation
crates/celeste-engine    Rt2 block model, boundary/dedup/merge,  deps: core, names
                         row keys, kernel.rs lane primitives
.  (celeste-rust)        search driver, program assembly,        deps: all
                         AST tracer, graph lowering, the ASM
                         assembler + runtime kernel registry,
                         compiled dispatch, campaign bins
native-probe             bench/gate binary for the engine        deps: all
```

There used to be four more crates here: `celeste-kernels` plus one
GENERATED kernel crate per room (~923k lines between them). They were
deleted 2026-08-29 (`d027f7e`, plans/asm-and-posgraph-execution.md B)
when the AVX-512 ASM backend (`compiled::asm_kernel`) became the
engine's kernel implementation - the kernels are now assembled at
startup, so there is nothing generated to check in, split, aggregate,
or keep current.

The 38k lines of rewrite rules that used to be the next thing to
extract are DELETED (`plans/deletion.md`); what is left in
`celeste-rust` is laid out as:

```
src/program/   Program assembly, recipes, and the frozen artifacts
src/search/    the abstract forward search: run, differential, checkpoint,
               sweep, sweep_time, pos_graph, state_mapping
src/trace/     the AST tracer (Lua -> transpile::graph::Graph)
src/transpile/ the graph IR, the lowering, and the ASM assembler
               (transpile::asm)
src/compiled/  FrameEngine dispatch, the ASM kernel registry
               (compiled::asm_kernel), and the State <-> block bridge
```

`src/search` was called `src/rewrite` until 2026-08-23, which was a lie by
then - only the `program` half was ever about rewriting.

One frame of the abstract search is `celeste_rust::compiled::FrameEngine`
`::step` - `(shape, rows) -> [(shape, rows)]`, the runtime-assembled ASM
kernels where they bind and the interpreter where they do not. It lives in
celeste-rust so both the forward search and the backward sweep can call it;
`compiled::bridge` is the `State` <-> block translation and is the only
module that names both.

`FrameEngine::run_frame_chunk` is the same engine as ONE campaign chunk's
frame body, and `CELESTE_COMPILED_FORWARD=1` puts it there. **Default
OFF, but the right setting is PER ROOM, by measurement** (2026-08-20,
BENCHMARK_DATA.md "Engine adoption validation at depth"): on room (1,0)
at the production horizon (f094) the compiled+fused engine is -23% wall
/ -40% peak with all 94 per-frame rowkey sets identical. (Both numbers
predate the ASM cutover - they were measured against the generated Rust
kernels, and the old room (0,0) "4x SLOWER" number was measured when no
kernel bound there at all. Remeasure under `compiled::asm_kernel`
before quoting either.) The engine's identity
(`asm_kernel::engine_fingerprint`, a content hash over the assembled
set) is hashed into the campaign fingerprint when it is on, so engines
never share checkpoints. The kernel backend is `compiled::asm_kernel`:
one binary, no cargo features, no checked-in kernel artifact - it
retraces the start room's shapes at startup, specializes each on the
CONSTANT LATTICE for the active precision mode (Level0 / ladder /
exact, matching `dispatch::traced_mode`), and assembles the fused
fork-free graph with gcc + dlopen. `CELESTE_NO_ASM_KERNELS` opts back
to the pure reference engine (debugging only). The per-class "walk"
kernels and the `fused` artifact were deleted 2026-08-25
(plans/delete-the-interpreter.md Phase 1) after the traced set took
every lane at f94 (BENCHMARK_DATA.md 2026-08-24: missed 0, plain-routed
0); the non-lattice sets were replaced by the lattice-specialized ones
2026-08-26 (plans/specialize.md); and the generated Rust kernel crates
themselves went 2026-08-29 (plans/asm-and-posgraph-execution.md B).
When the engine is on, a missed chunk is FATAL by default
(`CELESTE_KERNEL_STRICT=0` opts back into the counted fall-through).
`CELESTE_COMPILED_FORWARD=check` runs both engines and compares row-key
sets per chunk; that is the gate, and also a test.

`celeste-rust` re-exports `pico8_num` / `cart_data` / `collision_cache` at
its own root, so `celeste_rust::pico8_num::...` still resolves everywhere.

### There is NO checked-in kernel artifact

The kernels are assembled at startup: `compiled::asm_kernel::registry`
retraces the configured start room (`trace::kernel::room_kernels_in`),
fuses each shape's graph (`trace::emit::asm_fused` ->
`lower::specialize_frame` - every fork resolved at compile time, one
hash-consed arena), and `transpile::asm` assembles it with gcc + dlopen,
one .so per shape, in milliseconds where rustc+LLVM took minutes over
~900k generated lines. There is no regen step, no staleness gate, and
no diff to read: what runs is always what the tracer produces from the
Lua in this checkout. The set's content hash
(`asm_kernel::engine_fingerprint`) goes into the campaign fingerprint,
so two builds that assemble different kernels never share checkpoints.
`CELESTE_NO_ASM_KERNELS` opts out to the pure reference engine, for
debugging.

The gates on what the kernels COMPUTE:
`asm_kernels_reproduce_the_interpreter` (plus its ladder/exact
variants), `every_start_room_kernel_graph_asm_compiles_the_fused_graph`
(every start-room shape assembles and loads), the per-op bit-exact unit
tests in `transpile::asm::tests`, and a
`CELESTE_COMPILED_FORWARD=check` run.

`crates/celeste-names/src/gen.rs` is FROZEN, not generated. Its generator
(`transpile::names`) walked the rewritten IR and was deleted with the walk
kernels; `FIELD_NAMES`' ORDER is the canonical field ordering the boundary
hashes, so it feeds the shape hash, the row key, and what the search
dedups on. A reordering is a different search. New names (only possible
if the Lua changes) may be APPENDED by hand, never inserted.

## Useful entry points

```bash
# Abstract forward search for N frames (the main thing)
./safe-run.sh -- ./target/release/celeste-rust -n 30

# Single-lane concrete execution with a fixed input sequence - fast, and the
# basis for differential testing of rewrites
./target/release/concrete_run -i 42,0,0,0,0,16,2,2,2,2 -f 10

```

## Installing packages

Feel free to install pacman packages when needed (e.g., for profiling tools
like `perf`).

## A note on deleted tooling (2026-08-23)

`--profile`, `--trace`, the op census, the Chrome-trace `server/`
workflow, `measure_k` and the rewrite-finding half of `bin/rewrite` were
all deleted with the rewrite campaign (`plans/deletion.md`). They were
instrumentation for the interpreter and for finding rewrites, and both
of those jobs are done. They are in git history if a number is ever
needed again.

Never commit generated JSON - a 28 MB `cfg_analysis.json` blob used to
live in git and dominated the whole diff.
