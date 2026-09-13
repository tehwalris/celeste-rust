# Claude Code Guidelines

## What this project is

An abstract interpreter for PICO-8 Celeste, used to search for a provably
optimal TAS. It traces the original Lua into a graph IR, assembles that into
branch-free ASM kernels at startup, and runs a forward / backward /
precision-ladder search over columnar blocks of abstract game states.

Read these before doing anything substantial:

- `plans/architecture.md` - the agreed, load-bearing design: the three
  interfaces (frame step / block / kernel set), the outer loop, and a
  post-rebuild "Deferred follow-ups" list. This is the current work, and the
  ONLY surviving `plans/` doc (the other ~45 were deleted in the tear-out).
- `src/frame.rs` - the rebuilt search itself (~950 lines): `Block`, the
  `FrameStep` trait (implemented by the compiled `FrameEngine` and the
  reference `RefEngine`), `ForwardSink` (what a frame step emits into),
  `ForwardState` (a forward that is EXTENDED frame by frame) /
  `backward_run` / `Ladder` / `find_optimum`, the precision ladder,
  per-bucket checkpoints, and `MarkFilter` (the cross-precision link).
  `plans/buckets.md` is the design of the loop's data flow (2026-09-12).
  Every level runs TO the horizon and the backward seeds from the wins at
  every frame <= it; level 0 persists across horizons and is extended by
  one frame per step (2026-09-12).
- `BENCHMARK_DATA.md` - performance baseline, but STALE: every number in it was
  measured against the pre-rebuild search path (the now-deleted
  `run.rs` / `sweep*.rs`) and needs re-benchmarking for `rewrite search`. Keep
  it current once remeasured; the previous version was stale by 25x and nearly
  caused bad decisions.

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
  (When the ladder reported 99 for room (1,0) against a "proven" 100 on
  2026-09-13, the answer was `rewrite witness` + a real-PICO-8 replay,
  not trust in either number: the 99 was real.)
  `player.rem` is fully widened at level 0 and gets away with it only
  because k=16 narrows it back, so a spurious coarse win is refuted by a
  finer level. A widening applied at EVERY level is refuted by nothing: it
  survives to k=16 and the ladder reports a spurious win as the optimum.
  This has come up twice (`p_jump`/`p_dash`, 2026-08-06 and again from the
  field census 2026-08-16), so: the cost of a new abstraction is the
  abstraction PLUS its refinement ladder, and anything advertised as a free
  merge is mispriced.
- **Never deopt to the interpreter silently.** A deopt is a coverage gap, not
  a degraded mode. When the compiled engine has no kernel for a chunk it is
  FATAL (`KERNEL COVERAGE GAP`, with a miss report); there is no
  fall-through. Fix the gap rather than absorbing it.
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
untested passes; see `plans/architecture.md` for the approach that replaces it.

## Branches

`develop` is the main line; the search rebuild happens on feature branches off
it (this work is on `census`). `parallel-experiments` holds the overnight
parallelism, which is deliberately not merged - see BENCHMARK_DATA.md.
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
./safe-run.sh -- ./target/release/rewrite search --room 1,0        # THE SEARCH (checkpoints: /var/tmp/celeste-checkpoints)
./safe-run.sh -- ./target/release/rewrite forward --to 44 --room 1,0   # one timed forward pass
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
of them mutate process-global state (the rem precision, the kernel registry),
and under cargo test's shared-process harness the
suite has twice been observed degrading to ~70-85 MINUTES at one core.
nextest runs each test in its own process, which contains every such
leak by construction.

Exit code 137 means OOM. Peak memory for the rebuilt `rewrite search` has not
been re-benchmarked to the horizon (2026-09-07: the level-0 forward on room
(1,0) was at 16 GB and a 4.2M-lane frontier at f65, still growing), so run
it sandboxed under `./safe-run.sh` with the 60 GB default and watch. Do not
raise the cap past what `free` leaves after /tmp, which is a tmpfs.

Checkpoints go on DISK (`/var/tmp/celeste-checkpoints`, the default), not
under the tmpfs at /tmp. (The inode blow-up that forced this - one file per
17k cell-uniform blocks per frame - is gone: a frame is one file per SHAPE,
rows sorted by (cell, key) with a cell index (`search::checkpoint`, format
v8), so the backward reads a cell's rows as a range instead of decoding the
layer. Uncompressed: ~3x the zstd size on disk, and the decode that cost
the H=89 backward 35 s per iteration is gone.)

The loop is parallel since 2026-09-13 (`plans/parallel.md`): every frame
is two phases - EMIT, units of lanes over `threads()` workers, each
sorting its rows by OWNER; OWN, one worker per owner doing the filter,
the door (its private visited shard) and the append into its own
next-frame pieces - with nothing shared and one barrier. `CELESTE_THREADS`
overrides the default of one worker per physical core. The result is a
function of the frame and the thread count, not of scheduling: the gates
are identical at 1, 16 and 32 threads.

## Crate layout

A cargo workspace since 2026-08-18 (task #150). The dependency order is
load-bearing, not cosmetic:

```
crates/celeste-core      pico8_num, cart_data, collision_cache,  deps: -
                         ids, builtins
crates/celeste-names     FROZEN name tables                      deps: -
crates/celeste-interp    the old interpreter's `State` model     deps: core
                         (heap/value/local_env/state), the
                         abstraction layer (rem/spd widenings,
                         precision ladder), game_runner. The
                         reference engine is `trace::refengine`
                         in celeste-rust; this crate is what it
                         and the bridge speak.
crates/celeste-engine    Rt2 block model, boundary/dedup/merge,  deps: core, names
                         row keys, kernel.rs lane primitives
                         (the ASM call-outs + the codegen oracle)
.  (celeste-rust)        the search (frame.rs), the AST tracer,  deps: all
                         graph lowering, the ASM assembler +
                         runtime kernel registry, the bins
```

`celeste-ir` (2026-09) and `native-probe`, `src/main.rs` (the legacy
runner) and the `src/program/` recipe machinery (all 2026-09-07) are gone;
with them went the interpreter's merge/dedup/visited machinery
(`vectorize`, `virtual_merge`, `visited`, `row_table`, `merge_dump`), which
only the legacy runner reached.

There used to be four more crates here: `celeste-kernels` plus one
GENERATED kernel crate per room (~923k lines between them). They were
deleted 2026-08-29 (`d027f7e`)
when the AVX-512 ASM backend (`compiled::asm_kernel`) became the
engine's kernel implementation - the kernels are now assembled at
startup, so there is nothing generated to check in, split, aggregate,
or keep current.

The 38k lines of rewrite rules that used to be the next thing to
extract are DELETED; what is left in `celeste-rust` is laid out as:

```
src/frame.rs   the search: `Block` (an `Rt2` with its key column), the
               `FrameStep` trait + `ForwardSink`, ForwardState (extend) /
               backward_run / Ladder / find_optimum, the precision ladder,
               bucket routing, per-bucket checkpoints, MarkFilter
src/search/    checkpoint (block serialization), pos_graph (the position
               graph + the block's position column)
src/trace/     the AST tracer (Lua -> transpile::graph::Graph) and the reference
               engine (trace::refengine, the RefEngine oracle)
src/transpile/ the graph IR, the lowering, and the ASM assembler
               (transpile::asm)
src/compiled/  FrameEngine (`run_bucket`), the ASM kernel registry
               (compiled::asm_kernel, whose append step is where rows are
               keyed, deduped at the door and given their pos-graph edge),
               dispatch counters, and the State <-> block bridge
```

One frame of the abstract search is `celeste_rust::compiled::FrameEngine`
`::run_bucket` - one BUCKET in (one shape's `Rt2`; the old per-class
split on freeze / moving key / pm1 cells was the generated kernels'
premise and went 2026-09-12 with every gate identical), rows out through a
`ForwardSink`. The frontier IS a set of buckets (one block per shape per
owner), run as units of lanes (<1% lane padding), and the kernel's append
step emits each surviving row already at the boundary - canonical
structure, exact row key, its `(input cell, output cell)` pos-graph edge
recorded from the slice lane it came from - straight into the slot of
the row's OWNER; the owner then consults its visited shard at the door
and appends the survivors into its next-frame pieces (`Rt2::append_rows`).
There is no regroup, no merge, no per-block `boundary`, and no
provenance stored anywhere: it is consumed at emission (plans/buckets.md,
plans/parallel.md).
`FrameEngine` is one impl of the `FrameStep` trait (`src/frame.rs`); the
reference `RefEngine` is the other, and crosses `compiled::bridge` (the
only module that names both `State` and `Rt2`) at its edge. The loop
itself never touches a `State`. Measured 2026-09-12, room (1,0) f0-f44:
53 s -> 26 s, f44 9.4 s -> 4.8 s, 115k kernel calls -> 879, identical
frontier sets and pos-graph.

The kernel backend is `compiled::asm_kernel`: one binary, no cargo
features, no checked-in kernel artifact - it retraces the start room's
shapes at startup, specializes each on the CONSTANT LATTICE for the active
precision mode (Level0 / ladder / exact, `dispatch::traced_mode`), and
assembles the fused fork-free graph with gcc + dlopen. There is no other
engine to switch to: a chunk the kernels cannot take is FATAL.

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
Lua in this checkout.

The gates on what the kernels COMPUTE:
`asm_kernels_reproduce_the_interpreter` (plus its ladder/exact
variants), `every_start_room_kernel_graph_asm_compiles_the_fused_graph`
(every start-room shape assembles and loads), the per-op bit-exact unit
tests in `transpile::asm::tests`, and `rewrite ckhash` - the per-frame
(key, cell)-set fingerprint of a checkpoint tree. The pinned reference is
`gates/ckhash_room10_f000-044.txt` (room (1,0), level 0, f0-f44, taken
2026-09-07 before the block rewrite and reproduced after it and after the
cleanup): `rewrite forward --to 44 --room 1,0 && rewrite ckhash --to 44
--room 1,0 | diff - gates/ckhash_room10_f000-044.txt` must be empty. The
same run's `posgraph` line must equal `gates/posgraph_room10_f044.txt`, and
`rewrite search --from 29 --to 35 --maxk 1 --win-at 9,101` must reproduce
`gates/marks_room10_win9-101_h29-33.txt` (the backward's marked sets per
horizon and level, ending in `OPTIMAL win frame: 33`).

`crates/celeste-names/src/gen.rs` is FROZEN, not generated. Its generator
(`transpile::names`) walked the rewritten IR and was deleted with the walk
kernels; `FIELD_NAMES`' ORDER is the canonical field ordering the boundary
hashes, so it feeds the shape hash, the row key, and what the search
dedups on. A reordering is a different search. New names (only possible
if the Lua changes) may be APPENDED by hand, never inserted.

## Useful entry points

```bash
# THE SEARCH: find the minimal winning frame via the full precision ladder
# (Bits 0..=15 then Exact), driven by find_optimum over per-bucket checkpoints.
# --checkpoint-dir defaults to /var/tmp/celeste-checkpoints (on disk, not the
# tmpfs). --win-at x,y forces a cheap synthetic win.
./safe-run.sh -- ./target/release/rewrite search \
    --room 1,0 [--from H0] [--to H] [--maxk 15] [--checkpoint-dir DIR]

# One forward pass at one precision with the per-frame timing line
# (emit / own / checkpoint ms, lanes in/raw/kept, RSS; CELESTE_THREADS=N)
# and the pos-graph fingerprint; then the checkpoint-tree fingerprint.
# The three pinned oracles are under gates/ (frontier sets, pos-graph edges,
# backward marks); every change to the loop or the kernels must reproduce them.
./safe-run.sh -- ./target/release/rewrite forward --to 44 --room 1,0
./target/release/rewrite ckhash --to 44 --room 1,0

# The concrete witness behind a ladder result: DFS through one level's
# marks with the reference engine's single-input step. Prints the input
# bytes, or "NO WITNESS" - which is what a spurious win looks like.
./target/release/rewrite witness --horizon 99 --level 16 --room 1,0

# Single-lane concrete execution with a fixed input sequence - fast, and the
# basis for differential testing.
./target/release/concrete_run -i 42,0,0,0,0,16,2,2,2,2 -f 10

# The same sequence on a REAL PICO-8 (~/pico-8/pico8), headless: the
# fidelity check that made the 99-frame room (1,0) result believable.
pico8_diff/replay.py tas/room_1_0_exit_frame_99.txt

# Tracer analysis probes (text only): the reachable constant lattice, and the
# specialization collapse for one shape.
./target/release/transpile --room-consts
```

## Installing packages

Feel free to install pacman packages when needed (e.g., for profiling tools
like `perf`).

## A note on deleted tooling

`--profile`, `--trace`, the op census, the Chrome-trace `server/`
workflow, `measure_k` and the rewrite-finding half of `bin/rewrite` were
deleted with the rewrite campaign (2026-08-23). Then the 2026-08-31 tear-out
replaced the old forward/backward-sweep search: `src/search/run.rs`,
`sweep.rs`, `sweep_time.rs`, the deopt-collection path, shape-variant dispatch,
banding, and the g/e/band numbering are GONE, and `bin/rewrite` lost every
subcommand except `search` (Bench, Sweep, Ladder, ExtractTas, TraceWitness,
CountOptimal, Widencheck, Simdcheck, Deoptcheck, ShapeCensus, LeadingEdge,
ShapeInventory, MigrateVisited - all gone). They are in git history if a
number is ever needed again.

Never commit generated JSON - a 28 MB `cfg_analysis.json` blob used to
live in git and dominated the whole diff.
