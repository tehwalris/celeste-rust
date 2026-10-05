# Claude Code Guidelines

## What this project is

An abstract interpreter for PICO-8 Celeste, used to search for provably
optimal per-room TASes. It traces the cart's Lua into a graph IR, assembles
that into branch-free AVX-512 kernels at startup, and searches in three
steps (`rewrite search`, plans/architecture.md "The search"):

1. a level-0 FORWARD over columnar blocks of abstract states (the player's
   sub-pixel remainder widened to the whole pixel, optionally the room's
   objects too), recording every edge WITH what the frame did to the
   remainder (its transfer);
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

State of the code (2026-10-05): ~44.9k lines of Rust (26.2k code, 9.1k
comments, 7.4k tests); a cleanup toward ~10k is in progress.

## Read these before doing anything substantial

- `plans/architecture.md` - the current design: interfaces, the tracer and
  kernel model, the waves frame and the door, the recorded edges and their
  transfers, the search (arcs, the concrete search), gates, open issues.
- `plans/abstractions.md` - every level flag (`h f n p`; the remainder):
  what it widens, why it is sound, what refutes it, its status.
- `plans/results.md` - every room's optimum, how it was proven, its witness;
  how to run an object room.
- `plans/lessons.md` - abandoned approaches and the measurement that killed
  each. Check it before proposing something that sounds familiar.
- `plans/level-minus-one.md` - the cost-to-go filter.
- `src/frame.rs` (~2.7k lines: the block, the frame step, the forward) and
  `src/search/` (door, edges, checkpoint, arcs, arc_dp: the search).
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

`develop` is the main line; work happens on feature branches off it
(current: `integrate`, and `arc-only` for the arc-only search).
`parallel-experiments` (overnight parallelism, deliberately not merged),
`asm-interval-wrap` (a rejected first version of the interval-overflow fix;
the accepted one is on `arc-sets`), `interpreter-abandoned-2026-01-11` (the
January CFG optimizer, reading material only).

## Running safely

Use `./safe-run.sh` for EVERYTHING that builds or runs - `cargo build`,
`cargo check`, nextest, `transpile`, benches, the search - including
commands issued by subagents. It runs the command in a systemd scope with
`MemoryMax=60G` (`--memory 85G` etc. to raise it for one run) so a blowup
kills the process, not the machine. Why everything: on 2026-08-24 a process
outside the wrapper reached 120 GB twice (system-wide OOM) and took the
whole Claude session and a background agent's uncommitted hour of work with
it. The judgment call about which command is "cheap enough" is exactly the
thing that fails.

- Exit code 137 means OOM. Never raise the cap past what `free` leaves after
  /tmp, which is a tmpfs.
- Checkpoints go on DISK (`/var/tmp/celeste-checkpoints`, the default
  `--checkpoint-dir`), never under /tmp. A long run's trees are 100-200 GB:
  check `df` first.
- Reading memory: the `[fwd]` line's `rss start/wave/end` is the ANONYMOUS
  resident set (`RssAnon`: door, frontier, queues); `file` is file-backed
  pages (mapped edge records and checkpoints, reclaimable); `peak` is `VmHWM`,
  both together. Reading `VmRSS` once mistook the mmaps for heap.
- `safe-run.sh` sets `MIMALLOC_PURGE_DELAY=0` (freed pages return at once: the
  door rebuilds every shard each frame and the delayed purge kept 2 GB of
  copies resident). Override it in the environment to experiment.
- A search RESUMES from its checkpoint directory: rerun the same command
  after a crash or kill; delete the directory for a fresh run. Nothing on disk
  records a level's spec, so do not resume a tree under another `--level`.

## Iterating quickly - read this before running anything

**Pick the cheapest thing that answers your question.** The mistake is
always reaching for the most expensive profile out of habit.

| what you want | command | cost |
|---|---|---|
| "did my edit compile and does its unit test pass" | `cargo nextest run <filter>` (DEBUG) | ~1.3 s after an edit |
| "does the whole suite still pass" / the pre-commit run | `./safe-run.sh -- cargo nextest run --cargo-profile quick` | ~47 s build + run |
| anything you will quote a NUMBER from | `--release` | ~110 s build + run |
| the `#[ignore]`d tests, deliberately | `--cargo-profile quick --run-ignored all` | minutes |

- **Never use `--release` for unit tests.** The release profile is
  `lto = "fat"` + `codegen-units = 1`: a one-line edit relinks the workspace,
  ~100 s to run a test that executes in 6 ms.
- **Do not run the full suite in plain debug**: the compute-bound tests
  (`every_start_room_kernel_graph_asm_compiles_the_fused_graph`,
  `a_traced_frame_agrees_with_the_oracle`) are slow without optimization.
  Debug wins when a filter keeps them out; `--cargo-profile quick` otherwise.
- **Tools get `--profile quick`, not `--release`** (`transpile` probes,
  `rewrite` diagnostics, export-ui): 15 s to build against ~78 s. "Too slow
  in debug" argues for optimization, not for the gate's profile.
- **`--release` is for the gate and for benchmarks only.** Never make
  `[profile.release]` cheaper to speed up the loop - it silently reprices
  every recorded result. Add a profile instead.
- **Run tests with NEXTEST, never bare `cargo test`.** Several tests mutate
  process-global state (the precision, the kernel registry); under cargo
  test's shared process the suite twice degraded to 70-85 MINUTES on one core.
  nextest isolates each test in its own process.
- **One cargo at a time: `./one-cargo.sh cargo ...`** takes an flock on the
  build directory, so a second build WAITS and says so. Without it, a build
  started while another runs prints `Blocking waiting for file lock` once,
  into a log nobody tails, and then appears to take forever (2026-08-23: a
  70 s build "taking 25 minutes", a false hung-test alarm, a bogus theory);
  it can also swap the binary under a running A/B measurement.
- **Background anything over ~30 s** (`run_in_background: true`) and wait
  with a Monitor until-loop. Do not poll in a loop.
- **`touch` the file you care about** to measure what an edit really costs:
  `touch src/transpile/lower.rs && time cargo nextest run transpile`.

**The ignored tests** (7, `#[ignore]` rather than an env check so nextest
prints them as skipped): `every_reachable_pm1_key_gets_its_own_body`
(~240 s), `forward_extended_frame_by_frame_matches_fresh`,
`the_rooms_shape_set_is_a_fixpoint`, `room_71_table_builds_with_its_balloon`
(level -1), and the reference-engine end-to-end tests. They are NOT part of
the pre-commit run (Philippe, 2026-08-23: ~6 min each time costs more than
the occasional bisect). Run them when you have a reason:

- touched the tracer, the lowering, the ASM codegen or the edge recording ->
  the ignored tests AND the three pinned oracles (below); the kernels are
  assembled at startup, so the oracles are what check what they COMPUTE;
- touched the tracer's pinning or key walk ->
  `every_reachable_pm1_key_gets_its_own_body`.

Do NOT read past the "N skipped" line and call the suite green when one of
those reasons applies. That is the exact mistake behind `a8f4635`.

## Gates

Every change to the loop, the kernels or the tracer must reproduce the three
pinned room (1,0) oracles:

```bash
./safe-run.sh -- ./target/release/rewrite forward --to 44 --room 1,0 --checkpoint-dir D   # prints the posgraph line
./target/release/rewrite ckhash --to 44 --room 1,0 --checkpoint-dir D | diff - gates/ckhash_room10_f000-044.txt   # empty
# the posgraph line must equal gates/posgraph_room10_f044.txt
./safe-run.sh -- ./target/release/rewrite search --to 35 --win-at 9,101 --checkpoint-dir D2 > log
grep '^\[gate\]' log | diff - gates/arc_room10_win9-101_h35.txt          # empty
```

- The arc gate (2026-10-05, replacing the ladder's marks gate) pins the
  remainder-free marks at h35 (count and a fingerprint over (shape, key,
  cell)), the winning sets W at every frame 0-35 (node count and a
  fingerprint over each node's (shape, key, cell) and its region), the arc
  optimum (33) and the concrete witness (its 33 inputs). Ids depend on the
  scheduling; none of these do (identical at 3 and 32 threads, and over a
  reused tree).
- **Re-pin only on evidence that the SETS did not move** - posgraph
  identical, every per-frame kept count, marked count, first win and the
  optimum identical - and say so in the commit (done for `4e2d2e9` and
  `18208c6`, both new globals that move every key).
- After any change to the edge or transfer recording: `rewrite arc-check
  --level-dir D/level00 --to N` (every record has a transfer; sampled
  transfers probed with the reference engine inside and outside their
  guards; exits non-zero on any disagreement).
- Kernel keys against the boundary, per row: `CELESTE_KERNEL_KEY_CHECK=1` on a
  forward. Kernels against the reference engine, row by row: `rewrite
  ref-check`. `kernel lanes: missed 0` in every log.

## Code layout

A cargo workspace; the dependency order is load-bearing:
`celeste-core` (numbers, cart, collision) and `celeste-names` (frozen tables)
<- `celeste-interp` (the interpreter's `State`, `abstraction::Level`) and
`celeste-engine` (`Rt2` blocks, keys, lane primitives) <- `celeste-rust`
(the search `src/frame.rs` + `src/search/`, the tracer `src/trace/`, the
graph IR / lowering / assembler `src/transpile/`, the kernel registry
`src/compiled/`, the bins). Details: plans/architecture.md.

There is NO checked-in kernel artifact: `compiled::asm_kernel::registry`
retraces the start room's shapes at startup, specializes each on the constant
lattice for the level, and assembles it with gcc + dlopen. What runs is
always what the tracer produces from the Lua in this checkout.

## Useful entry points

```bash
# THE SEARCH (the horizon: --to H, or --ceiling H for a known solution, where
# a refutation or no witness is an error). The tree goes to DIR/level00 and
# is reused when it already reaches the horizon; the witness to
# DIR/witness_frame_F.txt. --win-at x,y forces a cheap synthetic win.
./safe-run.sh -- ./target/release/rewrite search --room 1,0 --ceiling 99 --checkpoint-dir DIR
# Object rooms: the objects abstract at level 0 and the level -1 filter
# (plans/results.md, "How to run a room").
CELESTE_LEVEL_MINUS_ONE="C,5" ./safe-run.sh -- ./target/release/rewrite search \
    --room X,Y --level r0sxhn --ceiling C --checkpoint-dir DIR [--save-marks UIDIR]
# The objects ladder, where r0sxhn's bound is loose (room (7,0): 80 against 84):
# the r0sxh forward filtered by r0sxhn's arc-marked nodes.
CELESTE_LEVEL_MINUS_ONE="84,5" ./safe-run.sh -- ./target/release/rewrite search \
    --room 7,0 --level r0sxhn,r0sxh --ceiling 84 --checkpoint-dir DIR
# Other knobs: CELESTE_THREADS, CELESTE_KERNEL_SETS=N (resident kernel sets),
# CELESTE_REGION="px,S" | off. (CELESTE_SPLIT_FRAME=1 still runs a forward,
# two steps a frame, but the search refuses it.)

# One forward at one level, with the per-frame [fwd] line; then its fingerprint.
./safe-run.sh -- ./target/release/rewrite forward --to 44 --room 1,0 [--level r0sxhn]
./target/release/rewrite ckhash --to 44 --room 1,0

# The transfers of a tree, checked against the reference engine.
./safe-run.sh -- ./target/quick/rewrite arc-check --level-dir DIR/level00 --room 3,3 --to 80 --level r0sxhn

# Level -1 as a probe (the table against a tree, too-late share per frame).
CELESTE_START_ROOM=2,0 ./safe-run.sh -- ./target/quick/transpile --level-minus-one S LEVEL_DIR CEILING FROM TO [MARKS]

# A known solution stepped against a level's tree (which state loses it).
./target/release/rewrite follow --level-dir D --level SPEC --inputs tas/FILE.txt

# Single-lane concrete execution; the same inputs on a REAL PICO-8, headless.
./target/release/concrete_run -i 42,0,0,0,0,16,2,2,2,2 -f 10
pico8_diff/replay.py --room 1,0 tas/room_1_0_exit_frame_99.txt
pico8_diff/replay.py --room 7,0 --balloon-seeds 0 tas/room_7_0_exit_frame_84.txt
# --lua ~/src/github.com/tehwalris/celeste_ocaml/celeste.lua --begin-game: the ORIGINAL cart

# A reference for OUR game from a community TAS: replay it in the original
# cart, take its per-frame positions, find inputs that follow them (~1 min).
./target/quick/rewrite trajectory --trajectory positions.txt --room 0,0

# Diagnostics (rewrite subcommands, quick profile): cell-growth --by-age,
# col-census [--cell x,y --erase F --every N], coarse-census --erase F,
# spurious [--chain-out], rerun-row, ref-check, diag-project [--fine-marks
# --coarse-marks], bench-frame, trajectory.
# The level -1 table alone, with its fingerprint (cached in /var/tmp/celeste-l1-cache):
CELESTE_START_ROOM=X,Y ./target/quick/transpile --level-minus-one-table S
```

### The UI (`ui/`)

A phone-first web view of one finished search: the room as a heatmap per
(horizon, level, pass, frame), set sizes, the timing waterfall, and an arc
pass (`search --save-marks`; old ladder runs still export with their
bands). Static: `rewrite export-ui` turns a finished
checkpoint tree + its run log into `run.json` + per-level binaries; a Vite
build plus `ui/serve.mjs` serve it under `/celeste/` on port 3011
(UI-HOSTING.md; control model and data layout in `ui/README.md` and at the
top of `src/search/ui_export.rs`).

```bash
cp /tmp/room10f.log /var/tmp/celeste-ui/room10f.log      # the run's log is the timing source
./one-cargo.sh ./safe-run.sh -- cargo build --profile quick --bin rewrite
./safe-run.sh -- ./target/quick/rewrite export-ui --log /var/tmp/celeste-ui/room10f.log \
    --out /var/tmp/celeste-ui/data --room 1,0             # from the repo root (loads cart/)
cd ui && npm install && npm run typecheck && npm run build
systemd-run --user --scope -p MemoryMax=2G --quiet node serve.mjs &   # http://localhost:3011/celeste/
```

Never serve uncapped, never on another port, and nothing generated
(`ui/dist`, `ui/node_modules`, the exported data) is committed.

## Installing packages

Feel free to install pacman packages when needed (e.g. `perf`).
