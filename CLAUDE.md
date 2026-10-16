# Claude Code Guidelines

## What this project is

An abstract interpreter for PICO-8 Celeste, used to search for provably
optimal per-room TASes. It traces the cart's Lua into a graph IR, assembles
that into branch-free AVX-512 kernels at startup, and searches in three
steps (`rewrite search`, plans/architecture.md "The search"):

1. a level-0 FORWARD over columnar blocks of abstract states (optionally the
   room's objects widened), each state with a HULL of the sub-pixel
   remainders it is reached with (a box of arcs, pushed along the edges:
   plans/architecture.md "The hull forward"), recording every edge WITH
   what the frame did to the remainder (its transfer);
2. a BACKWARD over the rotation graph ("arcs"): per node the exact set of
   remainders that still win by the horizon. Its optimum is exact in the
   remainder and a lower bound on the game's (no win refutes the horizon);
3. the CONCRETE SEARCH: exhaustive over concrete states (reference engine,
   every input, every `rnd` leaf), pruned only by those sets - a depth-first
   try at the bound, then a parallel breadth-first search. Its first win is
   the concrete optimum and the witness.

Where an abstract level's bound is loose, `--level r0sxhn,r0sxh` runs the
OBJECTS LADDER: the finer level's forward is filtered by the coarser one's
arc-marked nodes (room (7,0)).

Every room has a confirmed optimum (`plans/results.md`); all tie the
community TAS. The rem rungs of the precision ladder that found them were
deleted on 2026-10-05 (branch `arc-only`).

State of the code (2026-10-05, after the `tier3` cleanup): ~36.5k lines of
Rust (25.6k code, 4.4k comments, 4.7k tests, 1.8k blank; 44.9k before it); a
cleanup toward ~10k is in progress.

## Read these before doing anything substantial

- `plans/architecture.md` - the current design: interfaces, the tracer and
  kernel model, the wave and the storage, the recorded edges and their
  transfers, the search (arcs, the concrete search), gates, open issues.
- `plans/abstractions.md` - every level flag (`h f n p`; the remainder):
  what it widens, why it is sound, what refutes it, its status. Generated
  from the widening table (`celeste_engine::widening::TABLE`, interpreted
  by the tracer and by `Rt2::widen_to`; plans/widen-table.md).
- `plans/keys-only.md` (a state is its key: the frontier and the frame
  files are per-region (key, cell mask) entries; rows are decoded per unit
  right before the kernels), `plans/storage-v2.md` (the
  visited set), `plans/edges-target.md` (the edge files: ID-free bundles in
  their target regions; the routed forward) and `plans/exact-keys.md` (a
  state's key is its packed field codes, not a hash).
- `plans/devloop.md` - the check ladder (`./check.sh`) and the fixtures.
- `plans/footguns.md` - the settings (flags, recorded in each tree) and
  the other footguns fixed, with the decisions behind them.
- `plans/results.md` - every room's optimum, how it was proven, its witness;
  how to run an object room.
- `plans/lessons.md` - abandoned approaches and the measurement that killed
  each. Check it before proposing something that sounds familiar.
- `plans/level-minus-one.md` - the cost-to-go filter.
- `src/frame.rs` (~2.3k lines: the block, the frame step, the forward),
  `src/search/hull.rs` (a state's remainder hull and its push),
  `src/storage/` (the wave, its units and re-pushes, the visited set, the
  edge, out-edge and hull files) and
  `src/search/` (checkpoint, edges, arcs, arc_dp, known: the search).
- `BENCHMARK_DATA.md` - dated measurements, newest first; partly stale (see
  its first line).

The original OCaml implementation (`~/src/github.com/tehwalris/celeste_ocaml`)
is still the reference for interpreter semantics. Its `celeste.lua` is the
ORIGINAL cart; the traced program is `lua/builtin_level_3.lua` +
`lua/builtin_level_4.lua` + `lua/celeste-minimal.lua` - count things in the
latter.

## Ground rules

- This is a complex project. Take your time. Do not take shortcuts due to time
  pressure.
- Don't create parallel/simpler implementations that bypass existing
  infrastructure.
- **Correctness beats cleverness.** A transformation we cannot check is worse
  than no transformation. If you cannot verify something, insert a runtime
  guard instead of assuming.
- **Never widen a field without something EXACT that refutes it.** Every
  widening is an over-approximation: sound for REFUTING a horizon, never for
  confirming one. The remainder's widening is undone by the arcs (exact
  transfers); every other one (the level's flags) by the concrete search,
  which runs the real game - and it is a proof only because it is
  EXHAUSTIVE and prunes by nothing but the arcs' exact winning sets. A
  widening the concrete search also sees (a memo keyed on a widened key was one,
  `522de36`) is refuted by nothing. This has come up three times
  (`p_jump`/`p_dash` twice). The cost of a new abstraction is the
  abstraction PLUS what refutes it; anything advertised as a free merge is
  mispriced. (The exceptions in the code are listed in plans/abstractions.md:
  exact canonicalizations, and `rnd`, which is a stated best-case caveat.)
- **Believe a result after a witness, not before.** `rewrite search` writes
  the concrete witness; replay it on a real PICO-8. The project's own
  earlier "proven" 94 for room (0,0) was a frame too long.
- **Never deopt to the interpreter silently.** When the kernels cannot take a
  lane it is FATAL (`KERNEL COVERAGE GAP`, with a miss report); there is no
  fall-through. Fix the gap rather than absorbing it. Likewise a ceiling
  REFUTED (or without a concrete witness) is a bug, never a result.
- **Measure before and after.** Any change that claims a performance effect
  needs numbers from an actual run, not reasoning.
- **No dead code.** The build is warning-free; keep it that way.
- **Diagnostics are not tests.** A `#[test]` that asserts nothing and only
  prints a table is the wrong shape: nothing fails when its answer changes,
  and `--run-ignored all` pays for output nobody reads. Write a `rewrite`
  subcommand, or give the test an assertion that states the finding it
  defends.
- **Never commit generated JSON** (a 28 MB `cfg_analysis.json` once dominated
  the history), nor `ui/dist`, `ui/node_modules` or exported UI data.
- `crates/celeste-names/src/gen.rs` is FROZEN, not generated: `FIELD_NAMES`'
  order is the canonical field order the boundary hashes (shape hash, row
  key, dedup). A reordering is a different search. New names may be APPENDED
  by hand, never inserted.

## Branches

`develop` is the main line; work happens on feature branches off it and is
merged back.
`parallel-experiments` (overnight parallelism, deliberately not merged),
`asm-interval-wrap` (a rejected first version of the interval-overflow fix;
the accepted one is on `arc-sets`), `interpreter-abandoned-2026-01-11` (the
January CFG optimizer, reading material only).

## Running safely

Use `./safe-run.sh` for EVERYTHING that builds or runs - `cargo build`,
`cargo check`, nextest, the binaries, benches, the search - including
commands issued by subagents. It runs the command in a systemd scope with
`MemoryMax=60G` (`--memory 85G` etc. to raise it for one run) so a blowup
kills the process, not the machine. Why everything: on 2026-08-24 a process
outside the wrapper reached 120 GB twice (system-wide OOM) and took the
whole Claude session and a background agent's uncommitted hour of work with
it. The judgment call about which command is "cheap enough" is exactly the
thing that fails.

- Exit code 137 means OOM. `safe-run.sh` REFUSES a `--memory` above
  MemAvailable less 10 GB (MemAvailable counts /tmp, a tmpfs) and lowers
  its default to fit, printing the limit: caps that together exceed the
  machine took it down on 2026-10-11 (a 100G search beside an UNWRAPPED
  70 GB `ckhash` in tools/fixtures.sh).
- Scripts too: every binary and cargo a script runs goes through
  `safe-run.sh` (`scripts_wrapped`, a t0 test, fails on a line that does not).
- **One cargo at a time: `./one-cargo.sh cargo ...`** (an flock on the build
  directory: a second build WAITS and says so, instead of blocking silently
  or swapping the binary under a running measurement).
- Checkpoints go on DISK (`/var/tmp/celeste-checkpoints`, the default
  `--checkpoint-dir`), never under /tmp. A long run's trees are 100-200 GB:
  check `df` first.
- Reading memory: the `[fwd]` line's `rss start/wave/end` is the ANONYMOUS
  resident set (`RssAnon`); `file` is file-backed pages (mapped edge files
  and checkpoints, reclaimable); `peak` is `VmHWM`, both together. Reading
  `VmRSS` once mistook the mmaps for heap.
- `safe-run.sh` sets `MIMALLOC_PURGE_DELAY=100` (freed pages return after
  100 ms: a wave's units reuse each other's; why, and the numbers, in the
  script). Override it in the environment to experiment.
- **Settings are flags**, never environment variables: a binary refuses any
  `CELESTE_*` variable (naming the flag that replaced it). A tree records
  the settings that change it (`<level dir>/settings.txt`) and refuses a run
  under others; `--override-settings` proceeds only past a different
  `--region`, loudly. A tree without the record is refused (delete it).
- A search RESUMES from its checkpoint directory: rerun the same command
  after a crash or kill; delete the directory for a fresh run.

## Checking a change

The check ladder is `./check.sh t0|t1|t2|big` (plans/devloop.md: what each
tier runs and protects, the fixtures, the pins). No check runs longer than
~2 minutes; background anything over ~30 s and wait with a Monitor
until-loop, do not poll.

| tier | what |
|---|---|
| `t0` | the unit tests (nextest, quick profile): the pre-commit run |
| `t1` | single frames from the fixtures, exact: the forward, `--key-check`, the widening table, a backward frame, the storage, the kernels and transfers against the reference engine |
| `t2` | end to end: the three pinned room (1,0) oracles and the known route, their float twins (`--arith float`: the doubles' kernels; `gates/float_*`) and the dual search (`gates/dual_*`), an objects-ladder search, a platform room. Every change to the loop, the kernels or the tracer must pass it |
| `big` | the reference frame (6,2) 100% f56 -> f57 (and its storage replay), the one kernel and storage measurements use |

- **Run tests with NEXTEST, never bare `cargo test`.** Several tests mutate
  process-global state (the kernel registry, the counters); under cargo
  test's shared process the suite twice degraded to 70-85 MINUTES on one
  core. For one test after an edit: `cargo nextest run <filter>` (debug).
- **Never use `--release` for unit tests or tools.** The release profile
  is `lto = "fat"` + `codegen-units = 1` (~100 s relink after any edit).
  Tests and diagnostics get the `quick` profile; `--release` is for the
  gate and for benchmarks. Never make `[profile.release]` cheaper to speed
  up the loop - it silently reprices every recorded result.
- **The `#[ignore]`d tests** (resume, the room (7,1) level -1 table, the
  floor pin) and the full-tree `arc-check` are t3 (plans/devloop.md): run
  them deliberately when you touched the tracer, the lowering, the ASM
  codegen, the edge recording, resume, level -1 or the reference engine. Do
  NOT read past the "N skipped" line and call the suite green when one of
  those reasons applies (`a8f4635`).
- **Re-pin only on evidence that the SETS did not move** - every per-frame
  count, marked count, first win and optimum identical - and say so in the
  commit (`./check.sh t1 --pin` etc.; plans/devloop.md "Re-pinning").
- **The known-route check** (`search::known`): `search --prefer FILE` and
  `rewrite check-known` step a known solution through the reference engine
  and FAIL at the first pruning step that drops it. A pruned known route is
  a soundness bug, never a result (room (6,1)'s wrong refutation of 93,
  `5ccf712`). Run real rooms with `--prefer` their community TAS or our
  witness.
- `kernel lanes: missed 0` in every log.

## Verifying a result: REAL PLAY, not the IL load

A per-room TAS is valid only if it works in a full-game chain. In real play
a room is entered through a transition: its objects from the leaving
player's index J get one extra update on the loading frame (the "loading
jank"; plans/architecture.md "The room's start").
- UniversalClassicTas loads a room alone and misses this. A 100% 2300m 63
  that UCT finished died in real play (2026-10-08).
- Celia (gonengazit/Celia) models it.
- Searches run with `--loading-jank J`. The category runner sets it, and
  searches the other feasible J's too.

```bash
tools/uct/check_uploads.py [--cat CAT] FILE.tas...   # IL, UCT, Celia, chain, BOOT chain -> VALID/INVALID
tools/celia/validate.sh LEVEL FILE.tas               # Celia headless (CELIA_DASHES=1 for gemskip)
pico8_diff/chain.py CAT LEVEL FILE --boot            # real PICO-8 from 100m with the database's files
pico8_diff/chain.py CAT LEVEL FILE --modes           # IL / jank / chain entry states diffed field by field
pico8_diff/chain.py CAT LEVEL x --check-start target/quick/concrete_run   # the search's start vs the boot chain
target/quick/rewrite starts --room X,Y [category] --balloon-seeds 0   # every J a previous-room play gives, its classes
```

The boot chain is the verdict; a passing UCT run is not. Some database files
die on PICO-8 but finish in UCT and Celia (both compute in doubles), e.g.
gemskipany 2600m. A boot chain broken by one restarts after it and says so.

## Code layout

A cargo workspace; the dependency order is load-bearing:
`celeste-core` (numbers, cart, collision) and `celeste-names` (frozen tables)
<- `celeste-engine` (`Rt2` blocks, keys, lane primitives) <- `celeste-rust`
(the search `src/frame.rs` + `src/search/`, the tracer `src/trace/` with the
reference engine `refengine`, the graph IR / lowering / assembler
`src/transpile/`, the kernel registry `src/compiled/`, the level
`src/abstraction.rs`, the bins). Details: plans/architecture.md.

There is NO checked-in kernel artifact: `compiled::asm_kernel::registry`
retraces the start room's shapes at startup, specializes each on the constant
lattice for the level, and assembles it with gcc + dlopen. What runs is
always what the tracer produces from the Lua in this checkout.

## The commands

Every binary documents itself: `rewrite --help` (the search and every
diagnostic), `rewrite <command> --help`, `transpile --help` (the level -1
table), `concrete_run --help` (single-lane concrete execution). The
settings flags (`--room`, the category, `--level-minus-one`, `--threads`,
the diagnostics, ...) are shared by all three. The most common:

```bash
# THE SEARCH: --ceiling H for a known solution (a refutation or no witness is
# an error), --to H otherwise; the witness goes to DIR/witness_frame_F.txt,
# the outcome (levels, bounds, verdict, witness) to DIR/result.json.
./safe-run.sh -- ./target/release/rewrite search --room 1,0 --ceiling 99 --checkpoint-dir DIR
# An object room, and the objects ladder (plans/results.md, "How to run a room").
./safe-run.sh -- ./target/release/rewrite search --room 7,0 --level r0sxhn,r0sxh \
    --level-minus-one 84,5 --ceiling 84 --checkpoint-dir DIR --prefer tas/FILE.txt
# The same in the float tools' doubles (Celia's start and seeds by default;
# --float-tool uct for UniversalClassicTas): plans/float-solver.md.
./safe-run.sh -- ./target/release/rewrite search --arith float --room 7,0 ...
# THE DUAL SEARCH: the shortest file valid in BOTH arithmetics under rule (b)
# (the float tool's exit is the count; real play, 16.16 from --loading-jank J,
# the category's boot-chain J, must have left by then). Each half's levels to
# the same horizon under DIR/fixed and DIR/float, then the search over pairs;
# the witness replayed in both, to DIR/witness_frame_F.txt and DIR/result.json.
./safe-run.sh --memory 40G -- ./target/release/rewrite dual --room 0,3 --gemskip --hundred \
    --loading-jank 1 --level r0sxhn,r0sxh --level-minus-one 150,5 --to 150 --checkpoint-dir DIR \
    [--prefer FILE] [--float-half celia|uct]
# A float witness against a nudged libm `sin` (one ulp up, down, random per call).
./safe-run.sh -- ./target/quick/rewrite robust --arith float --room X,Y [category] --inputs FILE
```

Every witness in every tool (PICO-8 IL, chain and boot chain; UCT; Celia):
`tools/uct/check_uploads.py --cat CAT TAS<level>.tas`, plus Celia's
fixed-point mode with `CELIA_FIXP=1 tools/celia/validate.sh LEVEL FILE`.

On a real PICO-8, headless: `pico8_diff/replay.py --room X,Y FILE` (and
`--help`; `--lua ~/src/github.com/tehwalris/celeste_ocaml/celeste.lua
--begin-game` for the ORIGINAL cart).

### The UI (`ui/`)

A phone-first web view of one finished search (`search --save-marks DIR`,
then `rewrite export-ui --help`); building and serving it: `ui/README.md`
and UI-HOSTING.md. It is served under `/celeste/` on port 3011 only, never
uncapped (`systemd-run --user --scope -p MemoryMax=2G`), and nothing
generated (`ui/dist`, `ui/node_modules`, the exported data) is committed.

## Installing packages

Feel free to install pacman packages when needed (e.g. `perf`).
