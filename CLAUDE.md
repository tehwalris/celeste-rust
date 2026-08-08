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
./safe-run.sh -- cargo test --release
./safe-run.sh -- ./target/release/celeste-rust -n 40
```

Exit code 137 means OOM. Peak memory is much lower than it used to be (the
chunk-parallel work took frame 60 of the rewritten path from 4.3 GB to
2.4 GB), but the unrewritten `celeste-rust -n 40` runner is still the old
~29 GB - do not run higher frame counts unsandboxed.

The search runs on 16 threads with an 8,000-lane chunk cap by default.
Those two are ONE setting: threads without chunking undoes the streaming
boundary's memory bound, and chunking without threads is a ~23% loss. See
`plans/roofline-plan.md` before changing either, and `./parcheck.sh` to
re-check that the parallel path stays byte-identical to the serial one.

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
