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

State of the code (2026-10-16, after the refactor that shortened and
simplified it): 60.6k lines of Rust (38.1k code, 9.8k tests, 9.1k
comments, 3.5k blank), down from 64.8k (42.1k code) at `fa251711`; what it
left open is in plans/architecture.md "Open issues".

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
- `BENCHMARK_DATA.md` - the reference numbers for today's code (the pinned
  frames, whole searches), each dated with its commit.

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
  tree records the settings that change it (`<level>/tree.txt`) and
  refuses a run under others (`--override-settings` passes only
  `--region`); a run's flags never default to the record. A command that
  only READS a finished tree takes its settings from the record and refuses
  them as flags. A new tree setting is a field of `Settings` (its flag) and
  a row of `Settings::rows` (its record line), and bumps
  `settings::VERSION`. Add a knob only when necessary.
- **No Python.** Every tool is a `rewrite` subcommand, the external ones
  included (PICO-8, UCT, Celia, the tasdatabase: `src/tools/`). The only
  other code is the Lua that runs inside PICO-8 or LOVE, and the thin
  shell wrappers (safe-run.sh, one-cargo.sh, check.sh, tools/fixtures.sh).
- **Never commit generated JSON** (a 28 MB `cfg_analysis.json` once
  dominated the history), nor `ui/dist`, `ui/node_modules` or exported UI
  data. **Never commit an unsubmitted or teased route** (witness, golden
  or upload): those live outside the repo.
- `crates/celeste-names/src/gen.rs` is FROZEN, not generated, and
  APPEND-ONLY. A shape's identity is the names it holds
  (`Rt2::shape_words`: fields, functions and builtins by name, the globals
  present), so appending a name moves no hash; but `GLOBAL_NAMES`' order is
  the canonical cell order (the walk from the globals), so a reordering or
  an insertion is a different search. Its `GLOBAL_NAMES` include
  `cart::NATIVE`'s unimplemented builtins (`__widen_rem`, `__new_vector`,
  `__split_at`): every block carries them, so removing one changes every
  shape hash.
- `plans/abstractions.md`'s widening table is generated and test-checked
  (`CELESTE_REGENERATE=1` on `the_abstractions_doc_lists_the_table`).

## Branches

`develop` is the main line. Work happens on short-lived feature branches
off it, merged back after the check ladder. `interpreter-abandoned-2026-01-11`
is reading material only.

## Running safely

Use `./safe-run.sh` for EVERYTHING that builds or runs - cargo, nextest,
the binaries, benches, the search - including commands issued by
subagents and by scripts (the `rewrite` binary and one-cargo.sh refuse to
run without a memory cap: `settings::refuse_uncapped`). It runs the
command in a systemd scope with a
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
- Trees go on DISK (`/var/tmp/celeste-checkpoints`, the default
  `--search-dir` and `--tree`), never under /tmp. A big room's trees are tens of
  GB: check `df` first.
- Reading memory: the `[fwd]` line's `rss` is the ANONYMOUS resident set;
  `file` is mapped files (reclaimable); `peak` is `VmHWM`, both together.
- `rewrite` sets mimalloc's purge delay to 100 ms at its start, whatever
  `MIMALLOC_PURGE_DELAY` says (why: `compiled::set_purge_delay`).
- A search RESUMES from its checkpoint directory: rerun the same command;
  delete the directory for a fresh run.

## Checking a change

`./check.sh TIER...` (e.g. `./check.sh t0 t1 t2 big`). What each tier runs and pins, the fixtures, the
instruction budget and re-pinning: plans/devloop.md. The contract is the
black-box pins (the fixture frames, the room (1,0) gates, the objects
ladder, float and dual, the reference-engine checks, the instruction
budget): a change keeps them byte-identical, or re-pins them on evidence.

| tier | when | wall |
|---|---|---|
| inner loop | while editing: `./safe-run.sh -- ./one-cargo.sh cargo check --all-targets`, `... cargo nextest run <filter>` | 1 s, ~7 s |
| `t0` | before every commit: the unit tests | ~3 s (+ the test build) |
| `t1` | after any change to the forward, kernels, tracer or storage: single frames, fixed and float | ~35 s |
| `t2` | before handing a change over: end to end, and the slow tests | ~55 s |
| `big` | the reference and platform frames | ~40 s |
| `perf` | milestones and pre-merge: the instruction budget of the RELEASE build (2% over a pin fails) | ~4 min after an edit (the release build ~3.5 min, the frames 33 s) |
| `all` | before a merge: every tier, the local goldens, every fixture rebuilt, the pre-merge tests | ~11 min |

- **Run tests with NEXTEST, never bare `cargo test`** (tests mutate
  process-global state; under cargo test the suite once took 70-85 minutes
  on one core).
- **Never use `--release` for tests or tools** (`lto = "fat"`: ~100 s
  relink after any edit). Tests and diagnostics use the `quick` profile;
  `--release` is for the gate and benchmarks. Never make
  `[profile.release]` cheaper: it silently reprices every recorded result.
- **Every `#[ignore]` names the tier that runs it** (`slow, ` t2;
  `pre-merge, ` `all`; `local, ` `./check.sh local`): t0's "N skipped" are
  those, not a pass.
- **Re-pin only on evidence that the SETS did not move**, and say so in the
  commit.
- `kernel lanes: missed 0` in every log.
- **Concurrent checks are safe**: the fixtures are shared and READ-ONLY (a
  write into one fails), each run's scratch its own (`CHECK_DIR` per checkout).

## Verifying a result: REAL PLAY, not the IL load

A per-room TAS is valid only if it works in a full-game chain. Real play
enters a room through a transition: its objects from the leaving player's
index J get one extra update on the loading frame (plans/architecture.md
"The room's start"). UniversalClassicTas loads a room alone and misses
this; Celia models it. Searches run with `--loading-jank J` (the category
runner sets it and searches the other feasible J's too).

```bash
# Under ./safe-run.sh --memory 8G -- (PICO-8 and LOVE run inside the cap):
rewrite validate uploads [--cat CAT] FILE.tas...    # IL, UCT, Celia, chain, BOOT chain -> VALID/INVALID
rewrite validate celia [--dashes 1] [--fixp] LEVEL FILE.tas   # Celia headless (gemskip: one dash; 16.16); `uct` likewise
rewrite pico8 chain FILE --room X,Y --category C --boot     # real PICO-8 from 100m with the database's files
rewrite pico8 chain FILE --room X,Y --category C --modes    # IL / jank / chain entry states diffed field by field
rewrite pico8 check-start --room X,Y --category C           # the search's start vs the boot chain
rewrite room --room X,Y [--category C] --balloon-seeds 0    # every J and its start class
rewrite category JOBS.json OUTDIR                           # the category runner (`tools::category`)
rewrite golden uct|celia CAT all                            # the float oracle's goldens
```

The boot chain is the verdict; a passing UCT run is not. Our result is the
PICO-8 optimum: never lengthen a run to satisfy UCT; look for a
same-length variant valid in both.

## Code layout

A cargo workspace; the dependency order is load-bearing: `celeste-core`
(numbers, the arithmetic trait, cart, collision) and `celeste-names`
(frozen tables) <- `celeste-engine` (`Rt2` blocks, exact keys, the
widening table) <- `celeste-rust` (`src/frame.rs` the
forward, `src/storage/`, `src/search/` the backward, the concrete search
and level -1, `src/trace/` the tracer, `src/engines/` the reference
engine, `src/transpile/` graph IR and assembler, `src/compiled/` the kernel
registry, `src/settings.rs`, the `rewrite` binary in `src/bin/rewrite/`). Details: plans/architecture.md.

There is NO checked-in kernel artifact: the registry retraces the start
room's shapes at startup, specializes them for the level and assembles
them with gcc + dlopen. What runs is what the tracer makes of this
checkout's Lua. A set built before from the same sources (the whole tree),
profile, toolchain, cart and settings loads from `/var/tmp/celeste-kernels`
instead (`compiled::cache`; files hash-checked); the `[asm build]` line says
HIT or MISS.

## The commands

One binary documents itself: `rewrite --help` (the search, every
diagnostic, `run`: one concrete run), `rewrite <command> --help` (`rewrite
check --help`: the checks of a tree; `rewrite bench --help`; `rewrite l1
--help`: the level -1 table, its probe and check). The settings flags
(`--room`, the category, `--level-minus-one`, `--threads`, ...) are shared
by every command; a command that
reads a finished tree (`--tree DIR`, or a search's `--search-dir`)
takes them from the tree's records instead and refuses them. UNITS are in
the flag names: `--frames` counts game frames (the search's horizon,
`--level-minus-one`'s H), `--steps`/`--step` a tree's search steps (two a
frame under `--split-frame`: `settings::Clock`). The most common:

```bash
# THE SEARCH to --frames H; --ceiling: H is a known solution's (a
# refutation or no witness is an error). Witness: DIR/witness_frame_F.txt;
# outcome: DIR/result.json.
./safe-run.sh -- ./target/release/rewrite search --room 1,0 --frames 99 --ceiling --search-dir DIR
# An object room with the objects ladder, level -1 and the known route checked.
./safe-run.sh -- ./target/release/rewrite search --room 7,0 --level r0sxhn,r0sxh \
    --level-minus-one 84,5 --frames 84 --ceiling --search-dir DIR --prefer tas/FILE.txt
# In the float tools' doubles (Celia's start and seeds by default).
./safe-run.sh -- ./target/release/rewrite search --arith float --room 7,0 ...
# THE DUAL SEARCH: the shortest file valid in both arithmetics (plans/float.md).
./safe-run.sh --memory 40G -- ./target/release/rewrite dual --room 0,3 --category gemskip100 \
    --loading-jank 1 --level r0sxhn,r0sxh --level-minus-one 150,5 --frames 150 --search-dir DIR
# A known solution against every pruning step of a finished search (its
# settings, levels and horizon from its records).
./safe-run.sh -- ./target/quick/rewrite check known --search-dir DIR --inputs tas/FILE.txt
# One forward at one level; then its fingerprint (a command that reads a
# tree takes its settings from the tree's records: no --room, --level, ...).
./safe-run.sh -- ./target/release/rewrite forward --steps 44 --room 1,0 --tree DIR
./safe-run.sh -- ./target/quick/rewrite ckhash --tree DIR --steps 44
# A float witness against a nudged libm `sin`.
./safe-run.sh -- ./target/quick/rewrite robust --arith float --room X,Y [category] --inputs FILE
# On a real PICO-8, headless (--original: the game as released, celeste_ocaml's celeste.lua).
./safe-run.sh -- ./target/quick/rewrite pico8 replay --room 1,0 --inputs tas/room_1_0_exit_frame_99.txt
```

### The UI (`ui/`)

A phone-first web view of one finished search (`search --save-marks DIR`,
in either arithmetic, or `dual --save-marks DIR`: the float half's arc pass
and the dual witness; then `rewrite export-ui --help`, which reads each tree
in its recorded arithmetic); building and serving: `ui/README.md` and
UI-HOSTING.md. Served under `/celeste/` on port 3011 only, never uncapped
(`systemd-run --user --scope -p MemoryMax=2G`); nothing generated is
committed.

## Installing packages

Feel free to install pacman packages when needed (e.g. `perf`).
