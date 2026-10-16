# Claude Code Guidelines

## What this project is

An abstract interpreter for PICO-8 Celeste, used to search for provably
optimal per-room TASes. It traces the cart's Lua into a graph IR, assembles
that into branch-free AVX-512 kernels at startup, and searches in three
steps (`rewrite search`; plans/architecture.md "The search"):

1. a FORWARD over abstract states (the room's objects optionally widened),
   each with a HULL of the sub-pixel remainders it is reached with,
   recording every edge WITH what the frame did to the remainder (its
   transfer);
2. a BACKWARD over those edges: per node the exact set of remainders that
   still win by the horizon. Its optimum is exact in the remainder and a
   lower bound on the game's (no win refutes the horizon);
3. the CONCRETE SEARCH: exhaustive over concrete states (reference engine,
   every input, every `rnd` leaf), pruned only by those sets. Its first win
   is the optimum and the witness.

Where a level's bound is loose, `--level r0sxhn,r0sxh` runs the OBJECTS
LADDER: the finer level's forward filtered by the coarser one's reached
nodes. The arithmetic is a setting: PICO-8's 16.16 (`--arith fixed`, the
real game) or the TAS tools' doubles (`--arith float`; `rewrite dual` for
runs valid in both).

Every any% room has a confirmed optimum, and the categories nodiag, 100%
and gemskip are mostly done (plans/results.md).

State of the code (2026-10-16): 65.4k lines of Rust (42.1k code, 9.5k
comments, 10.7k tests, 3.1k blank), up from 36.5k on 2026-10-05. A
refactor to shorten and simplify it is starting: plans/refactor.md
(temporary).

## Read these before doing anything substantial

- `plans/architecture.md` - the system as it is: interfaces, the tracer and
  kernel model, keys, the hull forward and its storage, the recorded edges,
  the search, the checks, open issues.
- `plans/abstractions.md` - every level flag (`h f n p`; the remainder):
  what it widens, why it is sound, what refutes it (the widening table).
- `plans/float.md` - the float arithmetic and the dual search.
- `plans/level-minus-one.md` - the cost-to-go filter.
- `plans/devloop.md` - the check ladder (`./check.sh`), the fixtures, the
  pins, the benchmarks.
- `plans/results.md` - every room's optimum, how it was proven, its
  witness; how to run a room.
- `plans/lessons.md` - abandoned approaches and the measurement that killed
  each. Check it before proposing something that sounds familiar.
- `plans/refactor.md` - sizes, features, knobs and simplification
  candidates for the refactor (deleted when it is done).
- `BENCHMARK_DATA.md` - dated measurements, newest first; partly stale.

The original OCaml implementation (`~/src/github.com/tehwalris/celeste_ocaml`)
is the reference for interpreter semantics. Its `celeste.lua` is the
ORIGINAL cart; the traced program is `lua/builtin_level_3.lua` +
`lua/builtin_level_4.lua` + `lua/celeste-minimal.lua` - count things in the
latter.

## Ground rules

- This is a complex project. Take your time; no shortcuts under time
  pressure. Aim for the best end state, not the nearest one.
- Don't create parallel or simpler implementations that bypass existing
  infrastructure.
- **Correctness beats cleverness.** A transformation we cannot check is
  worse than none. If you cannot verify something, add a runtime guard.
- **Never widen a field without something EXACT that refutes it.** Every
  widening is an over-approximation: sound for REFUTING a horizon, never
  for confirming one. The remainder's widening is undone by the arcs
  (exact transfers); every other one by the concrete search, which is a
  proof only because it is EXHAUSTIVE and prunes by nothing but the arcs'
  exact winning sets. A widening the concrete search also sees (a memo
  keyed on a widened key, `522de36`) is refuted by nothing. The cost of an
  abstraction is the abstraction PLUS what refutes it. (Exceptions:
  plans/abstractions.md - exact canonicalizations, and `rnd`, a stated
  best-case caveat.)
- **Match PICO-8 exactly, or raise.** For overflow, division by zero and
  the like, the interpreter, the kernels and the reference engine compute
  exactly what PICO-8 computes or raise (a kernel declines the lane).
  Computing something else is never acceptable.
- **Believe a result after a witness, not before.** Replay the witness on a
  real PICO-8, in real play (below). The project's own earlier "proven" 94
  for room (0,0) was a frame too long.
- **Never deopt to the interpreter silently.** A lane the kernels cannot
  take is FATAL (`KERNEL COVERAGE GAP`); fix the gap. Likewise a ceiling
  REFUTED (or without a witness) and a known route PRUNED are bugs, never
  results.
- **Measure before and after.** A performance claim needs numbers from a
  run: instruction counts first, wall times only on a quiet machine.
- **No dead code.** The build is warning-free; keep it that way.
- **Diagnostics are not tests.** A `#[test]` that only prints asserts
  nothing. Write a `rewrite` subcommand, or assert the finding.
- **Make misuse loud, keep knobs few.** Settings are command-line flags,
  never environment variables: a binary refuses any `CELESTE_*` variable.
  They are parsed once into `settings::Settings` and PASSED (no globals). A
  tree records the settings that change it (`<level>/settings.txt`) and
  refuses a run under others (`--override-settings` passes only
  `--region`); flags never default to the record. A new tree setting is a
  field, a record line and a flag, and bumps `settings::VERSION`. Add a
  knob only when necessary; prefer Rust to Python (Python only drives
  external tools: PICO-8, UCT, Celia, the tasdatabase).
- **Never commit generated JSON** (a 28 MB `cfg_analysis.json` once
  dominated the history), nor `ui/dist`, `ui/node_modules` or exported UI
  data. **Never commit an unsubmitted or teased route** (witness, golden
  or upload): those live outside the repo.
- `crates/celeste-names/src/gen.rs` is FROZEN, not generated: `FIELD_NAMES`'
  order is the canonical field order keys and shape hashes depend on. New
  names may be APPENDED by hand, never inserted.
- `plans/abstractions.md`'s widening table is generated and test-checked
  (`CELESTE_REGENERATE=1` on `the_abstractions_doc_lists_the_table`).

## Branches

`develop` is the main line. Work happens on short-lived feature branches
off it, merged back after the check ladder. `interpreter-abandoned-2026-01-11`
is reading material only.

## Running safely

Use `./safe-run.sh` for EVERYTHING that builds or runs - cargo, nextest,
the binaries, benches, the search - including commands issued by
subagents and by scripts (the `scripts_wrapped` test fails on a script
line that is not wrapped). It runs the command in a systemd scope with a
memory cap (default 60G, `--memory 85G` to raise it for one run), so a
blowup kills the process, not the machine (2026-08-24: 120 GB outside the
wrapper took the whole session down).

- Exit code 137 means OOM. `safe-run.sh` refuses a `--memory` above
  MemAvailable less 10 GB (MemAvailable counts /tmp, a tmpfs) and lowers
  its default to fit. Caps that together exceed the machine took it down on
  2026-10-11: serialize big runs.
- **One cargo at a time: `./one-cargo.sh cargo ...`** (an flock on the
  build directory: a second build waits and says so).
- Background anything over ~30 s and wait with a Monitor until-loop; do
  not poll.
- Checkpoints go on DISK (`/var/tmp/celeste-checkpoints`, the default
  `--checkpoint-dir`), never under /tmp. A big room's trees are tens of
  GB: check `df` first.
- Reading memory: the `[fwd]` line's `rss` is the ANONYMOUS resident set;
  `file` is mapped files (reclaimable); `peak` is `VmHWM`, both together.
- `safe-run.sh` sets `MIMALLOC_PURGE_DELAY=100` (why, in the script).
- A search RESUMES from its checkpoint directory: rerun the same command;
  delete the directory for a fresh run.

## Checking a change

`./check.sh t0|t1|t2|big`; what each tier runs and protects, the fixtures,
the pins and re-pinning: plans/devloop.md. No check runs longer than ~2
minutes.

- **Run tests with NEXTEST, never bare `cargo test`** (tests mutate
  process-global state; under cargo test the suite once took 70-85 minutes
  on one core). One test after an edit: `cargo nextest run <filter>`.
- **Never use `--release` for tests or tools** (`lto = "fat"`: ~100 s
  relink after any edit). Tests and diagnostics use the `quick` profile;
  `--release` is for the gate and benchmarks. Never make
  `[profile.release]` cheaper: it silently reprices every recorded result.
- **Do not read past the "N skipped" line** and call the suite green when
  you touched what the `#[ignore]`d tests cover (plans/devloop.md, t3).
- **Re-pin only on evidence that the SETS did not move**, and say so in the
  commit.
- `kernel lanes: missed 0` in every log.

## Verifying a result: REAL PLAY, not the IL load

A per-room TAS is valid only if it works in a full-game chain. Real play
enters a room through a transition: its objects from the leaving player's
index J get one extra update on the loading frame (plans/architecture.md
"The room's start"). UniversalClassicTas loads a room alone and misses
this; Celia models it. Searches run with `--loading-jank J` (the category
runner sets it and searches the other feasible J's too).

```bash
tools/uct/check_uploads.py [--cat CAT] FILE.tas...   # IL, UCT, Celia, chain, BOOT chain -> VALID/INVALID
tools/celia/validate.sh LEVEL FILE.tas               # Celia headless (CELIA_DASHES=1 gemskip, CELIA_FIXP=1 fixed point)
pico8_diff/chain.py CAT LEVEL FILE --boot            # real PICO-8 from 100m with the database's files
pico8_diff/chain.py CAT LEVEL FILE --modes           # IL / jank / chain entry states diffed field by field
pico8_diff/chain.py CAT LEVEL x --check-start target/quick/concrete_run   # the search's start vs the boot chain
target/quick/rewrite starts --room X,Y [category] --balloon-seeds 0   # every J and its start class
```

The boot chain is the verdict; a passing UCT run is not. Our result is the
PICO-8 optimum: never lengthen a run to satisfy UCT; look for a
same-length variant valid in both.

## Code layout

A cargo workspace; the dependency order is load-bearing: `celeste-core`
(numbers, the arithmetic trait, cart, collision) and `celeste-names`
(frozen tables) <- `celeste-engine` (`Rt2` blocks, exact keys, lane
primitives, the widening table) <- `celeste-rust` (`src/frame.rs` the
forward, `src/storage/`, `src/search/` the backward and concrete search,
`src/trace/` the tracer, the reference engine and level -1,
`src/transpile/` graph IR and assembler, `src/compiled/` the kernel
registry, `src/settings.rs`, the bins). Details: plans/architecture.md;
sizes: plans/refactor.md.

There is NO checked-in kernel artifact: the registry retraces the start
room's shapes at startup, specializes them for the level and assembles
them with gcc + dlopen. What runs is what the tracer makes of this
checkout's Lua.

## The commands

Every binary documents itself: `rewrite --help` (the search and every
diagnostic), `rewrite <command> --help`, `transpile --help` (the level -1
table), `concrete_run --help` (single-lane concrete execution). The
settings flags (`--room`, the category, `--level-minus-one`, `--threads`,
...) are shared by all three. The most common:

```bash
# THE SEARCH: --ceiling H for a known solution (a refutation or no witness
# is an error), --to H otherwise. Witness: DIR/witness_frame_F.txt; outcome:
# DIR/result.json.
./safe-run.sh -- ./target/release/rewrite search --room 1,0 --ceiling 99 --checkpoint-dir DIR
# An object room with the objects ladder, level -1 and the known route checked.
./safe-run.sh -- ./target/release/rewrite search --room 7,0 --level r0sxhn,r0sxh \
    --level-minus-one 84,5 --ceiling 84 --checkpoint-dir DIR --prefer tas/FILE.txt
# In the float tools' doubles (Celia's start and seeds by default).
./safe-run.sh -- ./target/release/rewrite search --arith float --room 7,0 ...
# THE DUAL SEARCH: the shortest file valid in both arithmetics (plans/float.md).
./safe-run.sh --memory 40G -- ./target/release/rewrite dual --room 0,3 --gemskip --hundred \
    --loading-jank 1 --level r0sxhn,r0sxh --level-minus-one 150,5 --to 150 --checkpoint-dir DIR
# A known solution against every pruning step of finished trees.
./safe-run.sh -- ./target/quick/rewrite check-known --checkpoint-dir DIR --level SPEC \
    --horizon H --room X,Y --inputs tas/FILE.txt
# One forward at one level; then its fingerprint.
./safe-run.sh -- ./target/release/rewrite forward --to 44 --room 1,0 --checkpoint-dir DIR
./safe-run.sh -- ./target/quick/rewrite ckhash --to 44 --checkpoint-dir DIR
# A float witness against a nudged libm `sin`.
./safe-run.sh -- ./target/quick/rewrite robust --arith float --room X,Y [category] --inputs FILE
# On a real PICO-8, headless (--lua .../celeste_ocaml/celeste.lua --begin-game: the original cart).
pico8_diff/replay.py --room 1,0 tas/room_1_0_exit_frame_99.txt
```

### The UI (`ui/`)

A phone-first web view of one finished search (`search --save-marks DIR`,
then `rewrite export-ui --help`); building and serving: `ui/README.md` and
UI-HOSTING.md. Served under `/celeste/` on port 3011 only, never uncapped
(`systemd-run --user --scope -p MemoryMax=2G`); nothing generated is
committed.

## Installing packages

Feel free to install pacman packages when needed (e.g. `perf`).
