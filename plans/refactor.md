# Refactor: what there is, and what could go (2026-10-16; TEMPORARY)

A working doc for the shorten-and-simplify refactor: delete it when the
refactor is done. It replaces branch `spec`'s SPEC.md (2026-10-10, before
the storage rewrite, the hull forward and float); the contracts it held are
in plans/architecture.md now. Philippe marks the cuts in the FEATURE and
KNOB tables (the "mark" column is a proposal); the refactor then runs in
the slices at the end, each gated by the black-box pins.

Measured on `docs-refresh` after merging develop `0765e7c5`. Lines are
`wc -l`; "code / test / comment" split by a line classifier (test = inside
`#[cfg(test)] mod` blocks and test-only files; blank lines not shown).

## SIZE

**Rust: 65,641 lines** = 42,246 code + 10,767 test + 9,518 comment + 3,110
blank (36.5k on 2026-10-05; the storage rewrite, the hull forward and
float added most of it). Plus Lua 1,979 (the traced cart and its split
twin), Python and shell ~3,700, the UI ~7,650 (TS and CSS).

| module | files | lines | code | test | comment |
|---|---|---|---|---|---|
| src/trace/ (tracer, reference engine, level -1) | 23 | 17,352 | 11,102 | 3,096 | 2,396 |
| src/storage/ | 14 | 10,496 | 7,331 | 904 | 1,644 |
| src/transpile/ (graph IR, assembler) | 14 | 10,201 | 5,620 | 3,005 | 1,200 |
| src/search/ | 18 | 9,512 | 6,070 | 1,484 | 1,520 |
| crates/celeste-engine | 6 | 3,962 | 2,571 | 577 | 564 |
| src/compiled/ (kernel registry, emission) | 6 | 3,585 | 2,905 | 0 | 488 |
| src/bin/ | 3 | 3,522 | 2,784 | 39 | 622 |
| crates/celeste-core | 7 | 2,530 | 1,274 | 720 | 349 |
| src/frame.rs | 1 | 2,240 | 1,169 | 627 | 345 |
| src/settings.rs | 1 | 755 | 465 | 90 | 169 |
| src/census.rs | 1 | 419 | 312 | 19 | 69 |
| the rest (names, metrics, starts, concrete, par, game_runner, ...) | 9 | 1,067 | 642 | 206 | 152 |

Files of 300 lines or more (total, code / test / comment):

| module | files |
|---|---|
| trace/ | verify 2,346 (1,356/710/227); level_minus_one 2,328 (1,878/116/234); interp 2,261 (1,693/287/198); domain 1,907 (1,211/229/348); kernel 1,142 (806/109/182); widen 873; refengine 763; state 705; heap 538; error 531; oracle 480; cart 472; refdomain 442; block 431; bind 408; probe 363; emit 332; 6 more 1,030 |
| storage/ | wave 1,845 (1,407/85/285); unit 1,214 (939/0/216); set 1,011; route 884; edges 851; bundle 768; layer 736; bench 731; out 731; decode 561; visited 418; hulls 398; 2 more 348 |
| transpile/ | asm/codegen 2,807 (2,367/0/322); graph 1,832 (931/561/271); asm/tests 1,264 (test); bdd 955; garith 740; asm/codegen/float 647; lower 498; asm/tests_float 497 (test); ival 437; 5 more 524 |
| search/ | arc_dp 2,467 (1,648/373/363); ui_export 1,149; arcs 958; rem_f64 878; hull 560; reach 544; dual 484; hull_replay 352; pos_graph 329; arc_check 323; arc_edges 322; rem 318; 6 more 828 |
| compiled/ | asm_kernel 1,620; mix 778; asm_kernel/emit 704; 3 more 483 |
| bin/ | rewrite 3,197 (2,552/13/585); transpile 90; concrete_run 235 |
| celeste-engine | exact 1,157; widening 1,009; kernel 984; runtime2 643 |
| celeste-core | pico8_num 878 (289/432/103); lua_f64 632; arith 552; collision_cache 314 |

The biggest single files: `bin/rewrite.rs` (3,197: 30 subcommands in one
`main`), `transpile/asm/codegen.rs` (2,807), `search/arc_dp.rs` (2,467:
the backward, reach, the concrete search), `trace/verify.rs`,
`trace/level_minus_one.rs`, `trace/interp.rs`.

## FEATURE

Evidence: who calls it - `check.sh` tiers (the pins), scripts, the
category runner - and roughly how often it was run in the session
transcripts since 2026-10-12 ("runs"; approximate). Marks: **core**,
**diag** (diagnostic still useful), **dead?** (likely dead), **dup X**
(duplicate of X).

### `rewrite` subcommands (30)

| command | purpose | evidence | mark |
|---|---|---|---|
| `search` | the pipeline: levels, backward, concrete search, `result.json` | t2 gates, runner; ~200 runs | core |
| `dual` | the run valid in both arithmetics | t2 `gate10d-dual`; M6 rooms | core |
| `forward` | one level's forward, the profiling entry | t2, fixtures.sh, fwd_bench.sh; ~440 runs | core (gates) |
| `ckhash` | a tree's fingerprints (f/e/h lines) | t2 gates, fixtures.sh; ~66 runs | core (gates) |
| `bench-frame` | one forward frame, repeatable; exact metrics | t1/big pins, fixtures.sh, fwd_bench.sh; ~100 runs | core (checks) |
| `bench-backward` | one backward frame | t1 pin | core (checks) |
| `ref-check` | kernels against the reference, row by row | t1 | core (checks) |
| `arc-check` | transfers against the reference | t1 | core (checks) |
| `widen-check` | the widening table's two sides agree | t1, t2 | core (checks) |
| `check-known` | a known route against finished trees' pruning | `search --prefer` runs the same check; ~6 runs | diag (dup of `search --prefer` on a kept tree) |
| `export-ui` | the UI's data | t2 `export-ui`, runner | core (UI) |
| `room` | a room's objects as JSON for the tools | tools/celeste/rooms.py, check_uploads `--nudge` | core (tooling) |
| `starts` | the feasible loading-jank J's and classes | runner, celia/golden.py, chain.py `--feasible` | core (tooling) |
| `robust` | a float witness against a nudged `sin` | M6 validation; ~20 runs | diag |
| `trajectory` | inputs that follow a TAS's positions (references) | finding references; ~18 runs | diag |
| `l1-check` | a known solution against the level -1 table | float validation; ~14 runs | diag (fold into an `l1` group) |
| `bench-storage` | a captured frame replayed through the storage without kernels | t1 `storage-r10`, big `storage-r62h57` | dup `bench-frame --checkpoint`? (kernels are ~2-3% of a frame) |
| `follow` | a concrete route through a tree's rows, kernel diff at the first miss | ~16 runs | dup `check-known` (+ its miss explanation) |
| `rerun-row` | one stored row through the kernels, printed | ~38 runs | dup `ref-check` on one row |
| `col-census` | per-column cardinalities of a frame | ~86 runs | diag (one `census` command) |
| `cell-growth` | per-cell saturation | ~24 runs | diag (one `census` command) |
| `coarse-census` | distinct states with fields erased | ~10 runs | diag (one `census` command) |
| `spurious` | walk a tree's states back through the edges | ~6 runs | diag, rare |
| `reach` | exact-remainder reach over a finished tree (the hull study) | ~48 runs, before the hull forward | dead? (study done; `search::reach` stays for a soundness test) |
| `hull-stats` | hull frontier statistics | ~0 runs | dead? |
| `hull-replay` | the hull forward replayed; the float sliver study | float M3 (the sliver bit was rejected) | dead? |
| `hull-route` | a known route against a hull tree | float M3 | dup `check-known` |
| `census` | float against fixed state counts (reference engine) | float M2 | dead? (study done) |
| `diag-project` | a coarse tree closed over a fine one's rows | 0 recent runs | dead? |
| `edge-census` | transfers per (source, target) pair | ~4 runs; stale since the edge redesign | dead? |

### Other binaries and tools

| tool | purpose | evidence | mark |
|---|---|---|---|
| `transpile` | the level -1 table alone (`--level-minus-one-table`), the probe (`--probe`) | fixtures' tables; ~28 table runs | dup: fold into `rewrite l1` (a third table builder) |
| `concrete_run` | one concrete run, `--dump-start` | chain.py `--check-start`; ~120 runs | core: fold into `rewrite run` (it steps half frames under `--split-frame`) |
| check.sh, tools/fixtures.sh, tools/metrics_diff.py | the check ladder, its fixtures and metric pins | every change | core |
| safe-run.sh, one-cargo.sh | memory cap, one build at a time | every command | core |
| tools/category_runner.py, tools/celeste/ | a category end to end; the shared game model | every category campaign | core |
| tools/uct/{validate.sh, check_uploads.py}, tools/celia/validate.sh | witnesses in UCT, Celia, PICO-8 chains | every upload | core |
| tools/{uct,celia}/{golden.py, exact.lua}, tools/uct/canonical_jumps.py | the float oracle's goldens | t0 oracle tests | core (float) |
| pico8_diff/replay.py, chain.py | PICO-8 replay, chains, the boot chain | every result | core (move to tools/: jank-tools phase 1b) |
| tools/fwd_bench.sh | the forward frame benchmark | perf work on `fwd-perf2` | diag |
| tools/perf_inline.py | per-line cycles through inlined frames | perf work | diag |
| tools/kernel_mix.py | aggregate a `--kernel-mix` dir | the 2026-10-10 kernel study | dead? |
| tools/compare_video.py, tools/uct/add_gfx.py, capture.lua | side-by-side videos of two runs | occasional | diag |
| tools/align_tas.py | the UI's witness/reference paths | runner | core (UI) |
| regen-pico8-golden.sh | the `lua/probe` goldens on a real PICO-8 | probe corpora | core |

### Capabilities

| capability | where | mark |
|---|---|---|
| fixed and float arithmetic, the dual search | everywhere (generic `A`/`N`) | core |
| the hull forward, level -1, the objects ladder | frame, storage, search | core |
| categories: `--hundred`, `--nodiag`, `--gemskip` | cart patches, wins | core |
| `--onlydiag` (Only Diagonal Dashes) | `cart::forbid_straight_dashes` | no run yet; kept by Philippe (2026-10-10) |
| the split frame | a 939-line Lua twin, steps-vs-frames plumbing | core for rooms (6,0), (6,1) |
| real-play starts (`--loading-jank`, `starts`) | trace::cart, starts.rs | core |
| the UI export and `ui/` | search::ui_export (1,149), ui/ (~7.6k) | core; its ladder-era model parked by Philippe |
| the kernel mix census (`compiled::mix`) | 778 lines | the `--metrics` pins use its counts; the per-instruction report is dead? |

## KNOB

Settings flags (shared by the three binaries; plans/architecture.md "Per-
tree records"):

| flag | recorded | mark |
|---|---|---|
| `--arith fixed\|float`, `--float-tool celia\|uct` | yes | core |
| `--room X,Y` | yes | core |
| `--win-at x,y` | yes | core (gates, tests) |
| `--split-frame` | yes | core (2 rooms) |
| `--region px,S \| off` | yes (overridable) | core default; other values used once ((3,0) nodiag `8,6`) |
| `--hundred`, `--nodiag`, `--gemskip` | yes | core |
| `--onlydiag` | yes | kept by Philippe, unused |
| `--loading-jank J\|none` | yes | core |
| `--balloon-seeds S` | yes | core (float trees, starts) |
| `--concrete-balloon-seeds S` | no | core (runner) |
| `--level-minus-one H,S` | own record | core |
| `--l1-table FILE` | fingerprint | core (fixtures) |
| `--threads N` | no | core |
| `--override-settings` | no | diag; passes `--region` only |
| `--key-check` | no | core (t1) |
| `--kernel-explain` | no | diag (coverage gaps) |
| `--build-trace`, `--phases` | no | diag (`bench-frame` sets `--phases`) |
| `--kernel-mix DIR` | no | dead? (the kernel study) |
| `--emit-capture DIR` | no | goes with `bench-storage` |

Command flags worth a look: `search --no-witness` (unused by scripts),
`trajectory --spec/--tree` (keyed lines of the rem-ladder days), `ckhash
--cells/--dump-edges`, `reach --decode-rect/--cells`, `export-ui --arc`
(the marks dir; jank-tools J5 wanted it under the checkpoint dir). Six
spellings name a tree (`--checkpoint-dir`, `--level-dir`, `--tree`,
`--coarse/--real`, `--fine-dir/--coarse-dir`, `--capture`), three a
horizon (`--to`, `--horizon`, `--step`).

Environment (outside the binaries, which refuse `CELESTE_*`):
`CELESTE_FIXTURES` (scripts), `CELESTE_REGENERATE` (one test),
`MIMALLOC_PURGE_DELAY` (safe-run.sh, 100), `PICO8`, `UCT`, `CELIA`, `LOVE`,
the tool harnesses' `UCT_*`/`CELIA_*` (`DASHES`, `FIXP`, `EXACT`,
`CAPTURE`, `TRACE`, `LOG`, `LEVEL`, `JANK_OFFSET`), fwd_bench.sh's
`FWD_BENCH`/`BIN`/`THREADS`/`MEMORY`; `safe-run.sh --memory`, `check.sh
--pin`.

Constants that act as knobs: the concrete search's DFS budget (200k steps
in `arc_dp`, 400k in `dual`: two values), `REACH_SEGS` 32,
`SLICE_LANES` 256k, `storage::GEOMETRY` 8, `MIN_FREE_GB` 20,
`NO_PLAYER_FRAMES` 240, `PLATFORM_WORLD_FRAMES` 127, the region default
16,6, the walk's trace-state cap.

## JANK

Concrete simplification candidates, ordered by lines saved x safety
(estimates; "risk" is how likely a byte-pinned output moves).

### Cuts (Philippe's marks decide)

| # | candidate | where | saves | risk |
|---|---|---|---|---|
| C1 | the finished studies: `reach` (subcommand), `hull-stats`, `hull-replay` + `hull-route` (`search::hull_replay`), `census` (`src/census.rs`), `diag-project`, `edge-census` | rewrite.rs arms and helpers (`reach_command`, `rem_shapes`, `hull_stats`), hull_replay.rs, census.rs | ~1,400 | none (no pin reads them) |
| C2 | one census command for `col-census`, `cell-growth`, `coarse-census` (`ckhash --cells`), or cut them | rewrite.rs (+ `erased_states`) | ~250 | none |
| C3 | `follow` and `rerun-row` folded into `check-known`'s miss report and `ref-check` | rewrite.rs | ~170 | none |
| C4 | `bench-storage` + `--emit-capture` + the capture writers, if `bench-frame --checkpoint` covers the storage | storage/bench.rs (~450 of 731), unit.rs's capture, fixtures `cap-*` | ~550 | none (t1/big steps go) |
| C5 | the kernel mix report (`--kernel-mix`, the per-instruction provenance, kernel_mix.py), keeping the counts the `--metrics` pins need (or none, if the pins move to `perf stat` instruction counts) | compiled/mix.rs, codegen's `prov`, tools/kernel_mix.py | ~500-900 | low |
| C6 | `transpile` and `concrete_run` folded into `rewrite` (`l1 table/probe/check`, `run`): one level -1 builder (today three: `frame::minus_one_table`, transpile, `l1-check`; only the first has the cache key, the pin and the win-rect check); whole frames under `--split-frame` | bin/transpile.rs, bin/concrete_run.rs, rewrite.rs | ~250 | low (table fingerprints) |
| C7 | the UI's ladder-era data model (J10 of jank-tools) | ui_export.rs, ui/src (space.ts 1,741) | ~1,300 | parked by Philippe |

### Structure

| # | candidate | where | saves | risk |
|---|---|---|---|---|
| S1 | **The search's driver lives in the binary**: `run_levels`, `search`, `dual`, `check_known`, `search_record` (~350 lines) belong in the library as `search::run(&SearchSpec) -> SearchResult`; the binary prints and maps verdicts to exit codes. `filtered_for.txt` is written by the binary. | bin/rewrite.rs 1054-1435 | ~150 | low (t2) |
| S2 | **A `Tree` handle** for read-only commands: settings, level, filter and last frame from the tree's own records; one row selector; `--room`/`--level` refused on diagnostics (today restated in every call, and several commands do not check them at all) | rewrite.rs (`tree_of`, ~18 loads, 4 last-frame loops), check.sh, fixtures.sh | ~200 | low |
| S3 | **One concrete search** for one engine and for a pair: `dual::search` repeats `arc_dp::concrete_search`'s DFS-then-BFS, memo and input classes (and its own budget) | search/dual.rs (484), arc_dp.rs (~260) | ~250 | medium (arc gate witness, dual gate) |
| S4 | **arc_dp.rs split** into the backward, reach and the concrete search; its module doc is stale ("an edge enters a layer <= f" is false for hull trees; `search/mod.rs` still describes checkpoint files as row pieces) | search/arc_dp.rs | ~0 (clarity) | none |
| S5 | **The remainder model twice**: `rem::Rem` is ~25 forwarding methods; `rem_f64` re-implements the hull's two-chart arcs and the transfer decode for a 2^64 circle. One hull and transfer generic over the circle's point type | search/rem.rs (318), rem_f64.rs (878), hull.rs (560), arc_edges.rs (322) | ~350 | medium (fixed bytes, float gates) |
| S6 | **Four file formats, four block codecs**: frame, hull, edge and out files each write their own header, zstd blocks, varints and read-back check | storage/layer.rs, hulls.rs, edges.rs, bundle.rs, out.rs | ~250 | medium (`b` lines pin the bytes) |
| S7 | **One worker pool**: `par::Pool` exists, yet 48 `thread::scope`/spawn sites in 14 files hand-roll the atomic cursor, big stacks and ordered results | storage/wave.rs, decode.rs, edges.rs, spool.rs, search/arc_dp.rs, arc_check.rs, dual.rs, census.rs, trace/* | ~150 | low (determinism pins) |
| S8 | **Settings written four times** (struct, clap args, `recorded()`, `startup`'s log): table-driven | settings.rs | ~80 | low |
| S9 | **Steps vs frames**: the split frame's steps leak into the tree, the marks, level -1's H and `Found.frame`; the split cart is a 939-line near-copy of the minimal cart, not a patch | frame.rs, search/*, lua/celeste-minimal-split.lua | ~100 Rust + the Lua twin generated | medium (the split cart's text feeds the traces) |
| S10 | **Small bugs and stale bits**: `rewrite --help` shows `check-known`'s text under `reach` (a doc comment in the wrong place) and none for `check-known`; `refdriver.rs`'s error names a deleted doc; DFS budgets 200k vs 400k; stale section names in comments | rewrite.rs, trace/refdriver.rs, search/* | ~20 | none |
| S11 | **Cart patches written twice** (Rust and replay.py, with different semantics; jank-tools J14) and the move of pico8_diff/ into tools/ | trace/cart.rs, pico8_diff/replay.py | ~60 Python | medium (replays) |
| S12 | **The engine crate knows the game** (`BoundaryIds`, `player_objects`) and the reference engine sits in `trace/` above `frame`/`storage` (jank-trace T21): an `engines/` module beside `compiled` | celeste-engine runtime2.rs, trace/ref* | ~0 (layering) | low |

### The tracer (`src/trace`, `src/transpile` where they meet)

**Tracer: see audit (pending)** - a separate read-only audit
(`/var/tmp/tracer-audit.md`) will give the responsibility map, the
findings with file and line, and a target shape; its top findings and
their line estimates go here when it lands.

Still open from jank-trace (2026-10-11, ~1,500 lines in all; T7 and T9's
divergence are done, T4 partly): dead code and pass-through wrappers (T1
~190), hand-written Clone/PartialEq over `D: Domain` (T2 ~100), test-side
duplication (T3 ~110), the interpreter's raw-pointer AST keys and eight
boot copies (T5 ~130), two path vocabularies and five "find the player"s
(T6 ~60), duplication in interp/state (T8 ~100), the trace-to-kernel
hand-off in four types with two root orders and string kinds (T10 ~250),
the reference engine's eight entry points (T11 ~45), domain ops mirroring
`Op` (T12 ~100), six memoized graph predicates (T14 ~80), fork origins as
strings (T15), three builtin lists (T16), the walk keyed on `format!("{:?}",
Shape)` (T18), interpreter special cases keyed by spelling (T19), two
"run the room with no input" mechanisms (T20).

## Headline candidates

| rank | candidate | lines saved |
|---|---|---|
| 1 | C1 finished studies | ~1,400 |
| 2 | tracer (jank-trace, pending the audit) | ~1,500 |
| 3 | C7 the UI's ladder-era model (parked) | ~1,300 |
| 4 | C5 the kernel mix report | ~500-900 |
| 5 | C4 `bench-storage` and the capture | ~550 |
| 6 | S5 one remainder model | ~350 |
| 7 | C2 one census command | ~250 |
| 8 | C6 fold `transpile` and `concrete_run` | ~250 |
| 9 | S3 one concrete search | ~250 |
| 10 | S6 one block codec for the four file formats | ~250 |
| 11 | S2 a `Tree` handle | ~200 |
| 12 | C3 `follow`, `rerun-row` folded in | ~170 |
| 13 | S1 the search driver in the library | ~150 |
| 14 | S7 one worker pool | ~150 |
| 15 | S9 steps vs frames, the split cart as a patch | ~100 + the Lua twin |

## The order of slices

Each slice is small enough for one gate run: the fixture fingerprints
(`./check.sh t1`: f/e/h/b lines byte-identical), the room (1,0) gates and
their float and dual twins (t2), the differential checks (ref-check,
arc-check, widen-check, `--key-check`), and the kernels' instruction counts
within ~2% (the metrics pins). The pins must be in place first (the tests
agent's devloop.md work).

1. **Cuts with no pinned output** (C1, C2, C3, S10, after Philippe's
   marks): delete subcommands and their modules. Gate: t0-t2 unchanged.
2. **Fold the binaries** (C6): one level -1 builder; `rewrite run`. Gate:
   the level -1 fingerprints, `chain.py --check-start` for three rooms.
3. **The binary thins** (S1, S2): `search::run`, the `Tree` handle, the
   subcommands grouped (`tree`, `check`, `bench`, `l1`). Gate: t2,
   `result.json` identical but for times.
4. **Diagnostics' plumbing** (C4, C5, if marked): gate t1 without the
   steps that go; instruction counts unchanged.
5. **Shared primitives** (S7, S8): gate the determinism pins (3 against 32
   threads) and t1.
6. **Search structure** (S4, S3): gate the arc gate's witness, the dual
   gate, the known routes.
7. **The remainder model** (S5): gate fixed and float t1 frames and the
   float gates; instruction counts.
8. **The storage codecs** (S6): gate the `b` lines (file bytes).
9. **The tracer**, in the audit's slices: gate t1's kernel metrics exactly
   (same kernels), the oracle, ref-check.
10. **Steps vs frames, the split cart** (S9), **tooling** (S11, S12):
    gate r60s and the platform room in t2; every witness replays to the
    same exit.

The UI (C7) waits for Philippe.
