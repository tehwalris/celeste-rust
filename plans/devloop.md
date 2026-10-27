# The check ladder

One entry point: `./check.sh TIER...` (several tiers run in order: `./check.sh t0 t1 t2 big`). Each step runs under `safe-run.sh` with
a 10G cap (the nextest runs 20G) and prints `ok`/`FAIL` with its wall time.
The run exits non-zero on any failure. `--pin` writes the outputs as the new
pins (see Re-pinning). The times below are from the quick profile at 16
threads, on the shared 32-thread machine at load 15-40, prebuilt. Walls move
up to 2x with the load; instruction counts do not.

## The refactor's contract: the black-box pins

These checks do not care how the code is written. A change keeps them
byte-identical, or re-pins them on evidence.

- **The fixture frames' fingerprints**: the `f`, `h` and `e` lines (new
  states, frontier hulls, edges, by content) of single frames, 16.16 and
  float (t1, big), and their `b` lines (the bytes of every file the frame
  wrote: frame and edge files). The fixtures themselves are rebuilt from scratch against
  their pins by `./check.sh fixtures`.
- **The room (1,0) gates** (t2): the f0-f44 `ckhash` (states, edges and
  hulls), and the arc gate. The arc gate pins the marks, W at every frame
  f0-f35, the optimum, the concrete witness (its `[gate]` lines: `rewrite
  gate --search-dir`, the arc phase rerun over the finished search) and the
  known route (`--prefer`, `search::known`).
- **The objects-ladder search** (t2): room (4,2) `r0sxn,r0sx`, every
  `[gate]` line, and its UI export.
- **The float and dual gates** (t2), and the float twins' frames (t1).
- **The differential checks against the reference engine**:
  - t1: `check ref`, `check arcs` (and `check arcs --fault action` must fail),
    `check keys` and `check widen`; `keycheck-r60s`, the one fixture whose
    keys have FOLLOWERS (room (6,0)'s ten platforms, `exact::Deps`), every
    row's key recomputed at the boundary with its followers checked;
  - t2: `reference10[f]`, the reference engine's forward tree equal to the
    kernels', f0-f27 with edges and hulls, fixed and float; `reference60f`,
    the same at the fruit level (the r60s fixture to step 48);
  - t2: `hull10[f]`, the hull replay over the recorded edges.
- **A platform room** (t2): its forward to step 71. Big has its frame.
- **The instruction budget** (perf).

## The ladder

| tier | runs | wall |
|---|---|---|
| inner loop | `./safe-run.sh -- ./one-cargo.sh cargo check --all-targets`; `... cargo nextest run <filter>` (debug) | 1 s; 7 s after an edit |
| `t0` | the unit tests (nextest, quick; the `#[ignore]`d aside): the pre-commit run | 2-3 s (+15-40 s test build after an edit) |
| `t1` | single frames from the fixtures, fixed and float, in 5 lanes: THE REFACTOR GATE | 35 s |
| `t2` | end to end and the slow tests, in 4 lanes | 55 s |
| `big` | the reference frame (6,2) f56 -> f57, the platform frame | 38 s |
| `perf` | the instruction budget: the RELEASE build, measured; milestones and `all`, not every change | ~4 min after an edit (the release build ~3.5 min, the frames 33 s) |
| `local` | the local goldens, the PICO-8 probe goldens | ~2 min (the probe build) |
| `fixtures` | every fixture rebuilt from scratch against its pin | 3-4 min |
| `all` | the pre-merge run: every tier above and the pre-merge tests (53 s) | ~11 min (the release build in it) |

Develop's ladder before this one (2026-10-16, measured against this one
back to back, twice): t0 123 s, t1 61 s, t2 81 s, big 7 s.

Run the inner loop while editing, t0 before every commit, and t1 after any
change to the forward, the kernels, the tracer or the storage. Run t2
before handing a change over, perf at milestones (a change meant to be
faster, or one that may cost), and `all` before a merge. Background anything
over ~30 s and wait for it with a Monitor until-loop; do not poll.

### t0: the unit tests

Unit tests are kept only for the basics:

- PICO-8 and doubles arithmetic (`pico8_num`, `lua_f64`, and the
  kernels against both, `transpile::asm::tests`), with overflow raising;
- the data structures: key packing (`exact`), the visited set and its
  packed tables, region sets, bundles, edge and frame files;
- the arcs' set operations and the backward and reach DP against their
  definitions (`arcs`, `backward`, `reach`, `hull`), the decode against
  the cart (`rem`);
- what no pin reaches. These cover the tracer's and the emitter's edge
  cases (overflow declines, error ownership, folding exactness), the
  console's corpora (`probe`), the CLI and the tools (`src/tools/`).

A test that builds a room's kernels or runs a forward does not belong in
t0. 2026-10-16: tests the pins cover were deleted. Each deletion's evidence
is a bug injected by hand that the pins caught; see the commit. The slow
ones moved out. Every `#[ignore]` reason starts with the tier that runs it
(`conventions` checks this):

- `slow, `: t2 runs it (nextest `--run-ignored only`, 8 at a time);
- `pre-merge, `: `all` runs it, one at a time (a level -1 table build,
  ~250 CPU-s);
- `local, `: `./check.sh local` runs it.

So t0's "N skipped" is these, not a pass.

### t1: single frames (the refactor gate)

The steps run in five lanes, one per fixture family: r10, r62h, r42 (r42n,
r42x), f10 and f42 (f42n, f42x). A step names the fixture's tree (`--tree`)
and the frame, never its settings: the commands that read a tree take
them from its records (and refuse them as flags).

| steps | what |
|---|---|
| frame-r10, -r62h, -r42n | `bench frame --edges --checkpoint --expect` of r10 f55, r62h f50, r42n f60: the f/h/e/b lines against their pins |
| frame-f10, -f42n | the same in float |
| keycheck-r10, -r42n, -f42n | `check keys`: every emitted row's key recomputed from its fields, and decoded back; every stored row's key the boundary's; the f/h/e lines against the frame pins |
| refcheck-r10, -r42x, -f10, -f42x | `check ref`: 32 rows through the kernels and the reference engine; the converters agree |
| arccheck-, arcfault- (r10, f10; arccheck also r42x, f42x) | `check arcs`: 32 recorded transfers against the reference; with `--fault action` it must fail |
| widen-r10, -r62h, -r42n, -f10, -f42n | `check widen`: the widening table's two sides agree on every stored row and 64 rows' emissions |
| backward-r10arc, -f10arc | one arc backward step, W f45 of h56 |

### t2: end to end

The steps run in four lanes:

- **gates**: `gate10-forward` and `gate10-arc` (CLAUDE.md's three room (1,0)
  oracles and the known route), their float twins `gate10f-*`,
  `gate10d-dual` (the dual search, its bound and witness), `search42-objects`
  (the objects ladder at a synthetic win, every `[gate]` line), and
  `export-ui` (the UI export of four of these searches, by digest);
- **properties**: `reference10` and `reference10f` (`check reference`: the
  reference engine's forward against the kernels', `ckhash --edges --hulls`
  f0-f27), and
  `hull10[f]` (`check replay` over the kernels' tree to f36: its recorded
  edges reproduce its states at every frame);
- **platforms**: `forward60-platforms` (room (6,0) `r0sxf` split frame to
  step 71, its ten platforms exact and keyed by the first one's; its
  `ckhash --edges --hulls` the r60s pin), `widen-r60s`, and `reference60f`
  (`check reference` of the r60s fixture to step 48: the fruit level);
- **slow**: the `slow, ` tests. These are the hull forward between the exact
  reach and the widened forward (f34), the bounds the region and platform
  kernels check, the split frame at a near level, a run killed before its
  done marker, the respawn and near-floor walks, the float goldens (UCT,
  Celia), the fly fruit's pin, the reference at the fruit level and a
  fault it catches, the exact reach of the known route, a wrong cut
  `check known` names, the
  maps' tile scans, and the concrete and dual searches' breadth-first
  layers (room (1,0)'s arc gate with the depth-first try skipped: the
  witnesses pinned, the same at 3 and 32 threads).

### big, perf, local, fixtures

- **big**: frame-r62h57 (the reference frame (6,2) 100% f56 -> f57: 800,821
  new states, 27M edges re-pushed) and frame-r60s (the platform frame step
  70 -> 71), with their b lines.
- **perf, the instruction budget** (`gates/perf.txt`): one rep's user-space
  instructions on three bench frames, of the RELEASE binary (fat LTO, one
  codegen unit; the tier builds it): r62h f50 (18.86 G), r62h57 f56
  (45.42 G) and float f42n f60 (6.00 G), pinned from develop
  `fa251711`.
  - The count is `perf stat -e instructions:u` of a `bench frame --reps 3`
    run, less a `--reps 1` run, over 2, after a `--reps 0` run that fills
    the kernel cache: both counted runs load the kernels, so the kernels,
    the tree's load and the start cancel out. Always 16 threads.
  - Not the quick binary: its 16 codegen units re-partition on unrelated
    edits (the wave's generics are instantiated in the binary crate), and
    a diagnostic's edit in `rewrite.rs` moved all three frames by +2.2-3.1%
    (2026-10-16). Release, and quick at one codegen unit, are stable
    across such edits.
  - A frame more than 2% over its pin FAILS. One more than 2% under says to
    re-pin. One more than 20% off either way FAILS as implausible (a run
    that built its kernels against one that loaded them), never a re-pin.
  - Run to run the counts stay within 0.2% at load 30-190. A loop injected
    into the wave, adding 0.73 G a rep, failed r62h (+3.7%) and f42n
    (+11.9%); r62h57 passed at +1.5%.
  - Re-pin (`./check.sh perf --pin`) only for an intended speedup, or a
    change that has to cost, and say which in the commit.
- **local**: what needs the machine, not the checkout, so t0 does not run
  it. `trace::oracle::tests::the_local_goldens` replays the local store
  `/var/tmp/float-goldens`: our unsubmitted witnesses' goldens.
  `trace::probe::tests::the_golden_files_are_current` runs every probe
  corpus on a real PICO-8 (`rewrite pico8 probe-goldens`) and fails where
  its golden moved (rewritten in `lua/probe/`: read the diff). Where the
  store or PICO-8 is absent, its test passes and prints SKIPPED, the run's
  last line says so, and the run FAILS: a check that compared nothing is
  no pass (plans/correctness.md C14).
- **fixtures**: `tools/fixtures.sh verify all big` rebuilds every fixture
  beside the installed one and checks its fingerprint. This is the only
  check of a fixture's early frames under the current code. The other tiers
  run one frame from it.

### The forward frame benchmark (`rewrite bench frame`)

perf is the regression check. This is the profiling workflow, for speed
work on the forward outside the kernels: emission, unit ends, re-pushes, the
region phase, the frame end, the checkpoint.

`rewrite bench frame --tree DIR --step F --edges --checkpoint --reps N`
runs step F -> F+1 of a tree (`ForwardState::at`). It prints the pins: the
`f`, `h` and `e` lines and `b` (the bytes of every file the frame wrote).
Every rep must agree. Its stderr renders the frame's statistics
(`storage::stats`, one registry: the `[fwd]` line's and `forward.tsv`'s
too): per rep a `[bench] rep` line, and the `[cost]` report: per time
(walls, worker times, and with `--phases` the wave's worker-seconds by
phase) its min and max over the reps, its share of the wave's budget, its
hardware counts and its cost per element; the counts, the bytes written
and the resident sets.

`--phases`' `[mem]` lines sample the anonymous resident set and the live
heap at the phases' boundaries.

The tree is a fixture (`tools/fixtures.sh ensure NAME`; a new one is a
`spec` case there) or any forward's (`rewrite forward --steps F --tree
DIR ...`): a tree serves any step up to its last. The run checks itself
against a pin of its own: `--expect FILE` (the lines), written by a first
run with `--pin`; a later run that differs FAILS and shows every
difference.

```bash
./safe-run.sh -- target/release/rewrite forward --room X,Y --level LEVEL [--level-minus-one H,S] --steps F --tree DIR
./safe-run.sh -- target/release/rewrite bench frame --tree DIR --step F --edges --checkpoint --reps 3 --threads 16 \
    --expect DIR.lines [--pin]
```

Wall times need a quiet machine:

- give the load, repeat, and take the minimum;
- prefer worker-seconds and per-element costs, or perf's instruction counts;
- never quote a speedup from one noisy pair.

For a profile, `perf record -g -m 8 -- rewrite bench frame ...`:

- `-m 8`: the mlock budget is per user.
- The kernels are `.so`s in `$TMPDIR/asm-scratch/PID/`, gone when the next
  process starts: run `perf report` first.

## The fixtures

`tools/fixtures.sh ensure|build|pin|verify|list NAME...` (`all`, `big`).
The trees live under `/var/tmp/celeste-fixtures/FORMATS` (FORMATS as the
binary states them, `rewrite --version`; one directory per
file-format set, `tools/fixtures.sh root`; `CELESTE_FIXTURES` overrides), so
checkouts on different formats never rebuild each other's fixtures; they are
never committed.

- Each is built by a pinned command and checked against its pinned
  fingerprint (`gates/fixtures/NAME.ckhash`: `ckhash --edges --hulls`
  through its last frame).
- `ensure` rebuilds a fixture whose spec or file formats changed, and says
  so: `STALE ... REBUILDING`.
- One built from other sources is only reported. A fixture is an INPUT,
  pinned by its fingerprint.
- Transfer ids are final at each slice's gather (`XferTable::merge`: a
  function of the frame), so a frame check's edge bytes are a function of
  the search.
- Level -1 fixtures hold their table, as every filtered tree does
  (`NAME/l1-table.bin`, its fingerprint in the record): a frame check uses
  it, whatever binary.
- Fixtures are shared by every checkout and READ-ONLY (`ensure` makes them
  so): a step that writes into one fails. Scratch is the run's own:
  `bench frame`'s `/var/tmp/celeste-bench-frame-PID` (removed after), and
  check.sh's `CHECK_DIR`, by default per checkout
  (`/var/tmp/celeste-check-NAME-HASH`), so concurrent runs never share it.
- A fixture's frames are the tree's: SEARCH STEPS (two a game frame under
  `--split-frame`, as r60s's "to step 71"; `settings::Clock`, plans/
  architecture.md "The split frame"), as the flags named so count them
  (`forward --steps`, the tree commands' `--steps`/`--step`); `search
  --frames` and `--level-minus-one`'s H count frames.

| fixture | what | build |
|---|---|---|
| r10, f10 | room (1,0) 200m `r0sx` to f56, 16.16 and float; frame 55 -> 56 | 4 s each |
| r62h | room (6,2) 100% `r0sxf`, level -1 (94,5), to f51; frame 50 -> 51 | 50-110 s (the table) |
| r42n, f42n | room (4,2) 2100m `r0sxn`, level -1 (71,5), to f61; frame 60 -> 61 | 15-25 s |
| r42x, f42x | room (4,2) `r0sx` (objects exact) to f56: the reference checks' object room | 14-18 s |
| r10arc, f10arc | `search --level r0sx --frames 56 --win-at 40,64`: the backward's tree | 4-5 s |
| r60s | room (6,0) 700m `r0sxf` (ten exact platforms keyed by the first), split frame, to step 71 | 18-40 s |
| r62h57 (big) | room (6,2) 100% to f57, the reference frame | 57 s |

## Re-pinning

Re-pin only on evidence that the sets did not move: every per-frame count,
marked count, first win and optimum identical. Or give the reason they did.
Say which in the commit.

- `tools/fixtures.sh pin NAME...`: a fixture's own frames;
- `./check.sh t1 --pin`, `t2 --pin`, `big --pin`: frame lines, gate
  lines (the room (1,0) oracles and their float and dual twins too), the UI
  digests: every comparison of a tier goes through check.sh's `same`, so
  `--pin` re-pins exactly what the tier compares. The one exception is t2's
  `forward60`, which compares with the r60s fixture's pin (`fixtures.sh
  pin r60s` writes it);
- `./check.sh perf --pin`: the instruction budget.

`git diff gates/` shows what moved. A file-format change rebuilds every
fixture (`ensure` does it) and moves every hash pin: `pin all`, `pin big`,
then the tiers' `--pin`.

## Gaps

- **The n level against the reference.** `check ref` and `check arcs` at
  `r0sxn` fork the widened objects into thousands of paths a row. t1
  checks the object room at `r0sx`, and the n kernels end to end
  (search42) and by `check keys`.
- **Every build of the sources builds its kernels once** (the kernel
  cache, `compiled::cache`: a set is keyed on the sources, so a commit's
  first check builds every set; a hit loads in 0.1-2 s against room
  (1,0)'s ~5 s, (6,2)'s ~20-30 s and (6,0) split's ~70 s).
- **A frame check meets no new transfer** (`ForwardState::at` loads the
  whole table). The numbering of a frame's new transfers is checked by
  the full forwards (t2, `fixtures`).
