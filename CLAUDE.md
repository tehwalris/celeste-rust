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

Use `./safe-run.sh` for anything that runs the forward search or the test
suite. It runs the command in a systemd scope with `MemoryMax=100G` so an
accidental blowup kills the process rather than the machine.

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
| deliberately re-checking the generated files / recipes | `--cargo-profile quick --run-ignored all` | ~6 min |

**Never use `--release` for unit tests.** This is the actual trap. The
release profile is `lto = "fat"` + `codegen-units = 1`, so a one-line edit
relinks the whole workspace - ~100 s to run a test that EXECUTES in 6 ms.
I did this repeatedly on 2026-08-22 before noticing. `[profile.quick]`
exists for when a test genuinely needs optimization (the compute-bound
ones: `compiled_forward_reproduces_the_interpreter`,
`shape_variant_dispatch_reproduces_the_baseline`, `generated_is_current`).

Do NOT run the full suite in plain debug: those same tests are
compute-bound and `compiled_forward` alone goes 20 s -> 153 s. Debug wins
when a filter keeps them out; `--cargo-profile quick` wins when it cannot.

**Tools get `--profile quick`, not `--release`.** `transpile` prints text;
no number anyone quotes comes out of it, so fat LTO buys nothing. 15 s to
build under quick against ~78 s under release, for a generator that runs
24.6 s instead of 22.3 s. `regen-generated.sh` uses quick throughout and
no longer refreshes `target/release` - build that yourself before
benchmarking. (I wrote the rule below and then immediately reached for
`--release` to build `transpile`, because it is genuinely too slow in
debug. "Too slow in debug" argues for OPTIMIZATION, not for the gate's
profile.)

**`--release` is for the gate and for benchmarks only.** Every number in
BENCHMARK_DATA.md was measured under it. Never make `[profile.release]`
cheaper to speed the loop up - that silently reprices every recorded
result. Add a profile instead.

**Other things that cost more than they look:**

- **One cargo at a time.** A `cargo build` started while a background
  `nextest` is still building will contend on the lock and can swap the
  binary under a running A/B measurement. On 2026-08-22 this invalidated
  a benchmark side and it had to be re-run. Background long jobs, then
  leave the build directory alone until they finish.
- **Background anything over ~30 s** (`run_in_background: true`) and use
  a Monitor with an until-loop to wait. Do not poll in a loop.
- **`touch` the file you care about** to measure what an edit really
  costs: `touch src/transpile/lower.rs && time cargo nextest run transpile`.
  Guessing at build cost is how the stale table this replaced survived.
- **`./regen-generated.sh` is ~2 min**, of which ~1m45 is two release
  builds and only ~29 s is the nine generation jobs (which now run in
  parallel). If you only need ONE kernel, call `transpile` directly
  instead of running the whole script.

**The pre-commit run is `--cargo-profile quick`, and it does NOT include
the ignored tests.** `--release` is for numbers only; gating correctness
under fat LTO costs ~110 s of relink to run tests that execute in
milliseconds, and it contradicts "never use `--release` for unit tests"
three lines above.

Three slow tests are `#[ignore]`d: `every_checked_in_recipe_replays`
(~200 s), and `generated_is_current{,_r20}` (~44 s / ~70 s), plus
`every_reachable_pm1_key_gets_its_own_body` (~240 s). `#[ignore]` over an
env check on purpose: nextest prints them as skipped, so the skip is
visible rather than silent.

**They are not run on every commit** (Philippe, 2026-08-23): they cost
~6 min, and the expected cost of occasionally breaking one and bisecting
back to it is lower than paying that on every commit. Run them when you
have a REASON to think they will fire:

- touched an emitter, `transpile::names`, or anything feeding the
  generated files -> `generated_is_current` (this one has already caught
  a real stale-kernel commit, `a8f4635`, so treat any emitter change as
  a reason);
- touched the rewrite rules or a checked-in recipe ->
  `every_checked_in_recipe_replays`, the only thing that replays them;
- touched the tracer's pinning or key walk ->
  `every_reachable_pm1_key_gets_its_own_body`.

Do NOT read past the "N skipped" line and call the suite green when one
of those reasons applies. That is the exact mistake behind `a8f4635`.

Run the suite with NEXTEST, never bare `cargo test --release`: the tests
are fine (21 s wall for all 515 under nextest, 2026-08-16) but several
of them mutate process-global state (tracing, instr_time, op_census,
partition toggles), and under cargo test's shared-process harness the
suite has twice been observed degrading to ~70-85 MINUTES at one core.
nextest runs each test in its own process, which contains every such
leak by construction.

Exit code 137 means OOM. One job needs MORE than the 100 GB default:
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
crates/celeste-names     GENERATED name tables                   deps: -
crates/celeste-ir        ir, frontend (Lua -> IR), builtins,     deps: core
                         print
crates/celeste-interp    the INTERPRETER (the oracle),           deps: core, ir
                         game_runner, its instrumentation
crates/celeste-engine    Rt2 block model, boundary/dedup/merge,  deps: core, names
                         row keys, kernel.rs lane primitives
crates/celeste-kernels   GENERATED per-class lane kernels        deps: core, engine
.  (celeste-rust)        rewrite machinery, transpile emitters,  deps: all
                         compiled dispatch, campaign bins
native-probe             bench/gate binary for the engine        deps: all
```

The split is HALF DONE (plans/tracing.md stage 1). Still to come out:
`celeste-rewrite` (~38k lines, the rules) and `celeste-transpile` (~6k,
the emitters), which is the pair that actually makes the edit loop small.
Two cycles block them: `rewrite -> compiled` (4 sites in verify.rs and
checkpoint.rs, all of which test the COMPILED path and belong upstairs)
and `compiled -> rewrite` (`Program`, which wants to move down;
`StateMapping`, which needs a home).

One frame of the abstract search is `celeste_rust::compiled::FrameEngine`
`::step` - `(shape, rows) -> [(shape, rows)]`, the generated class kernels
where they bind and the interpreter where they do not. It lives in
celeste-rust so both the forward search and the backward sweep can call it;
`compiled::bridge` is the `State` <-> block translation and is the only
module that names both.

`FrameEngine::run_frame_chunk` is the same engine as ONE campaign chunk's
frame body, and `CELESTE_COMPILED_FORWARD=1` puts it there. **Default
OFF, but the right setting is PER ROOM, by measurement** (2026-08-20,
BENCHMARK_DATA.md "Engine adoption validation at depth"): on room (1,0)
at the production horizon (f094) the compiled+fused engine is -23% wall
/ -40% peak with all 94 per-frame rowkey sets identical, while on room
(0,0) - where no kernel binds, the witnesses being room (1,0) shapes -
it is 4x SLOWER and 12x the peak, still set-identical. The engine's
identity is hashed into the campaign fingerprint when it is on, so
engines never share checkpoints. The fused build needs
`--features fused` plus a per-campaign generated artifact
(gitignored); watch out for stale non-fused binaries after
regen-generated.sh (see the script's NOTE).
`CELESTE_COMPILED_FORWARD=check` runs both engines and compares row-key
sets per chunk; that is the gate, and also a test.

The generated code is TWO crates because the engine reads
`celeste_names::FIELD_NAMES` while the generated kernels read
`celeste_engine::{Rt2, Col, AV}`; in one crate that is a cycle. Name
tables below the engine, kernels above it.

`celeste-rust` re-exports `pico8_num` / `cart_data` / `collision_cache` at
its own root, so `celeste_rust::pico8_num::...` still resolves everywhere.

### The generated files are CHECKED IN

`crates/celeste-names/src/gen.rs` and
`crates/celeste-kernels/src/kernel_gen_*.rs`, plus the three shape
witnesses under `crates/celeste-kernels/witness/` that generation reads.
Regenerate with:

```bash
./regen-generated.sh     # then READ THE DIFF, then commit
```

`transpile::names::tests::generated_is_current` fails until you do.
Byte-for-byte, on purpose: `FIELD_NAMES`' ORDER is the canonical field
ordering the boundary hashes, so it feeds the shape hash, the row key, and
what the search dedups on. A reordering is a different search.

There is a bootstrap to know about: the emitters live in `celeste-rust`,
which depends on `celeste-kernels`, whose contents they produce. Change
what the kernel emitter emits and the committed kernels stop compiling,
which stops `cargo build --bin transpile` from building the tool that
would fix them. `regen-generated.sh` avoids it by generating into a
scratch dir and only installing what builds; if you get stuck anyway,
`git checkout crates/celeste-kernels/src`, build, regenerate.

## Useful entry points

```bash
# Abstract forward search for N frames (the main thing)
./safe-run.sh -- ./target/release/celeste-rust -n 30

# Single-lane concrete execution with a fixed input sequence - fast, and the
# basis for differential testing of rewrites
./target/release/concrete_run -i 42,0,0,0,0,16,2,2,2,2 -f 10

# Chrome trace of where intra-frame time goes
./safe-run.sh -- ./target/release/celeste-rust -n 37 --trace /tmp/trace.json
```

## Installing packages

Feel free to install pacman packages when needed (e.g., for profiling tools
like `perf`).

## Serving trace files

Trace files (for Chrome's `chrome://tracing` viewer) are served from the
`server/` directory:

```bash
./safe-run.sh -- ./target/release/celeste-rust -n 37 \
    --checkpoint-dir checkpoints --resume --trace /tmp/trace.json.zst

cp /tmp/trace.json.zst server/
cd server && python3 -m http.server 8000 &

# Verify the file is served correctly
curl -s http://localhost:8000/trace.json.zst | sha256sum
sha256sum server/trace.json.zst  # Should match
```

`server/` and `serve/` are gitignored. Never commit generated JSON - a 28 MB
`cfg_analysis.json` blob used to live in git and dominated the whole diff.
