# Jank in the CLI, the UI export and the tooling (2026-10-11)

Scope:
- `src/bin/rewrite.rs` (2,684 lines, 25 subcommands), `src/bin/transpile.rs`
  (89), `src/bin/concrete_run.rs` (235);
- `src/search/ui_export.rs` (1,101) and `ui/` where it meets the export
  (~6k TS + 1.8k CSS);
- the Python and shell tooling: `tools/` (category_runner, align_tas,
  compare_video, kernel_mix, metrics_diff, uct/, celia/, fixtures.sh),
  `pico8_diff/` (replay, chain, run.sh, wrap.sh, cases/), `check.sh`,
  `gates/raise.sh`, `safe-run.sh`, `one-cargo.sh`.

Not covered: the backward (`arc_dp`) and the edge storage (being redesigned),
map-mod specifics (`p8cart.py` lives on `origin/map-mod` only), and
`src/trace` (plans/jank-trace.md on `origin/jank-dig`). Line numbers are
against `origin/develop` e7e9f78.

Every claim was checked by reading the code and grepping for its users. The
divergences in "Found while digging" were checked against the recorded
`results.jsonl` files under `/var/tmp`. Nothing was built or run.

This goes past SPEC annex D. Where annex D already lists an item (kn-2, sf-2,
rm-1, rm-4, bw-2, fw-13), it is extended here, not repeated.

**Bottom line.**
- About 4,000 lines can go, plus ~27 MB of committed generated files:
  - Rust ~2,500. The area's 5,100 Rust lines (rewrite.rs, the two small
    binaries, ui_export, `storage::convert`, `search::inspect`) would end
    near 2,600.
  - TS ~1,200 of the UI's 6k.
  - Python and shell ~800, ~400 of them a dead harness.
- About 1,000 of those lines are high-confidence deletions that need no
  design: J1, J2, J7.
- The jank sits in four places:
  - **Logs and text are the interfaces.** The runner and the exporter
    regex the search's stderr. Two writers emit one upload format, and they
    already disagree.
  - **The tree does not speak for itself.** Every diagnostic reopens a tree
    by hand and makes you retype the settings the tree already records.
  - **Finished studies and migrations are kept as permanent subcommands.**
  - **Python reimplements the game model in five places.** These are the
    category table, the tasdatabase format, room/level names, the map's
    object tiles and the cart patches, with no shared module.

## Status (2026-10-11, branch `tools-cleanup`)

Phase 1 is done: the items independent of the edge-storage refactor
(`edges-target`) and of the map mod (`map-mod`, which edits
pico8_diff/replay.py and src/trace/cart.rs). Every commit passed
`./check.sh t0` and `t1`; the last also `t2`.

| item | done | commit |
|---|---|---|
| J1 | the 16 numerics/sequence cases are lua/probe corpora (all agree with PICO-8 0.2.7, 234 lines; `__hex` native in the probe runner); run.sh, wrap.sh, known_fail.txt, cases/, README, trace_frame37.json (26 MB), baseline/isocheck (1.1 MB), PROGRESS.md deleted; the sin table's generator moved to cart/pico8_sin_table.p8 | `The PICO-8 numerics cases as probe corpora` |
| J2 | `bounds-audit` (+ `check_any_level`, `ids.f_last`) and `adopt-settings` retired (the sweep found one tree without a record, /var/tmp/fg-r62h-new, a scratch tree) | `Retire bounds-audit and adopt-settings` |
| J7 | the runner's `env` shim refused; `ancestry`/`--forward-only` names; docs | same |
| J8 | `par::Pool` for ref-check, ui_export and the kernel build | `One scoped worker pool` |
| J5 | `result.json` (verdict, levels, bounds, witness, wall, peak); the level -1 refusal a verdict; the forward's per-frame stats in each tree (`forward.tsv`); export-ui reads them (no `--log`, no `arc.txt`, no `--paths-only`); the runner reads the record | `The search writes result.json` |
| J4 | `export-ui --download FILE`: the UI's download IS the upload; `TasFile` and the path files' tasdatabase lines gone | same |
| J6 | tools/celeste/ (categories, tasdb, rooms, pico8, love); the runner, check_uploads, align_tas and chain.py its clients | `One Python package for the game model` |
| J15 | `rewrite starts`: the feasible J's and their classes in Rust (equal to chain.py's for levels 2-31) | same |
| seeds | `rewrite room` (`trace::cart::room_objects`, the one object scan): the runner's seed slots | same |

Not done in phase 1, and why:
- **J14 (the cart patches) and moving replay.py/chain.py to tools/**: they
  rewrite pico8_diff/replay.py's `build_cart` and trace/cart.rs, which the
  map-mod branch is editing (`--cart`, `--max-djump`, `cart_data`). Phase
  1b, below.
- **J8's fingerprint module, arc-check's pool, J2's `convert-tree`,
  `edge-census`, `spurious --chain-out` (+ `trajectory --spec/--tree`),
  J7's `fixtures.sh formats()`** (it greps `storage/edges.rs`): they touch
  the edge storage. Phase 2.

Merging `edges-target` over this branch: expect small conflicts in
src/frame.rs (`log_frame` appends `forward.tsv`; `minus_one_table` returns a
`Result`), src/search/arc_dp.rs (no `arc.txt`) and src/bin/rewrite.rs (the
`Search` arm writes `result.json`; `bounds-audit`, `adopt-settings` gone).
Merging `map-mod`: rewrite.rs (the retired arms' `cfg.room` lines: take the
deletion), ui_export.rs (`export` takes the room and `Paths`; map-mod
passes `&cfg` for the mod's tiles), trace/cart.rs (`room_objects` beside
`OBJECT_TILES`; map-mod's `s.cart_data_in(root)` goes into the callers).

### Phase 1b: after map-mod merges

1. `rewrite cart --lua minimal|original --room X,Y [--gemskip]
   [--loading-jank J] [--seeds hook|inline] [--cart FILE.p8] [--max-djump
   N]` prints the patched Lua; `trace::cart` owns the start-room and orb
   patch for both carts (`load_room(1, 0)` and begin_game's
   `load_room(0,0)`), the loading frame (one text: Rust skips `move` at a
   zero speed, the replay does not; `move(0, 0)` can only zero a `rem`
   inside a solid, and every `rem` is 0 at a load - check it) and the
   seeds. Two seed modes, both Rust's: INLINE by position (the search: no
   new global) and a HOOK (`this.offset=__balloon_seed()` and the chest's
   last shake, the replay's driver supplying the queue, per room in a
   chain). The concrete search's chests stay `rnd`, as a stated option.
2. replay.py keeps only its driver (the dash notes, the dump, the
   segments, the frame lines) and asks `rewrite cart`; `minimal_jank` and
   `OBJECT_TILES` go (the J translation is `room_objects`).
3. replay.py and chain.py move to tools/ (`celeste.pico8` owns the cart
   build; the CLIs thin); pico8_diff/ is gone.
4. Verify: `chain.py --check-start` for rooms (3,1), (6,2), (2,1)
   unchanged; every tas/*.txt witness and every upload replays to the same
   exit frame (PICO-8, a few minutes).

### Phase 2: after edges-target merges

In this order (each step its own commit, t0/t1 each, t2 at the end):

1. **J11 `storage::Tree::open(dir)`**: the recorded `Settings` and level,
   the `TreeFilter`, the last frame, the `Resolver`, the `EdgeStore` on
   demand, `rows(frame, sel)` with ONE row selector (`--frame F [--cell
   x,y] [--samples N] | --id ID`), `engines()` built under the tree's own
   settings. Every read-only command takes `--tree DIR` and REFUSES the
   settings flags (check.sh, fixtures.sh and raise.sh drop `--room
   --level ...` from those lines; `ckhash --room` goes).
2. **One binary, grouped** (the structure below): `rewrite tree
   hash|census|rows|project`, `rewrite check known|kernels|transfers`,
   `rewrite bench frame|backward|storage`, `rewrite l1 table|probe|check`,
   `rewrite run`, `rewrite cart`, `rewrite room`, `rewrite starts`;
   src/bin/rewrite.rs split into src/bin/rewrite/{main, search, tree,
   check, bench, l1, run}.rs.
3. **J3**: `transpile` and `concrete_run` folded in (`rewrite l1 ...` through
   one `frame::minus_one_table`, its cache key, pin and win-rect check;
   `rewrite run --inputs F|LIST [--frames N] [--object T] [--dump-start]`
   over `RefEngine::frame`, whole frames; `trajectory`, `follow`, `l1
   check` on `frame` too, so `step`/`step_one` have no tool callers);
   `chain.py --check-start` asks `rewrite run --dump-start`; the dump's
   TYPES once (replay.py's list goes with phase 1b).
4. **J13**: `check kernels` (ref-check + rerun-row + widen-check: one
   sampler, three comparisons); `follow`'s kernel diff becomes
   `check known`'s explanation of a "not in the tree" failure.
5. **J9**: `tree census --frames A..B [--cell] [--erase] --by
   frame|cell|column|age` (cell-growth, col-census, coarse-census, ckhash
   --cells), on the Tree's row iterator.
6. **J12**: `search::run(&SearchSpec) -> SearchResult` in the library
   (the record `result.json` already is); `check known` is `run` with
   `forward: ReuseOnly, concrete: false`; `filtered_for.txt` written by the
   storage, not the binary.
7. **J5's rest**: the arc pass's artifacts under the checkpoint dir in
   their own format (`search --ui-data`: W's support, the reached nodes;
   not `Visited`'s bincode read by byte offset); `export-ui --dir D --out O
   [--witness --reference --download]` (no `--arc`, no `--room`);
   gates/raise.sh compares two `result.json`s with the walls dropped.
8. **J8's rest**: `storage::fingerprint` (state, edge, pair, note and the
   line formatters) for ckhash, bench-frame, bench-storage and
   bench-backward; arc-check on `par::Pool`; `rewrite formats` for
   fixtures.sh.
9. **J2's rest**: sweep the disk for format-13 trees, then `convert-tree`
   and storage/convert.rs go; `edge-census` as the edge owner decides;
   `spurious --chain-out` with `trajectory --spec/--tree`.
10. **J10** (the UI): PARKED. Philippe (2026-10-11): "I don't really want to change much for now." Keep the UI as is; only keep it working (exports from the current search records).

### J10: questions for Philippe

1. Which views stay? Today: the room heatmap with the ladder's horizon x
   level grid and passes (space.ts, 1.7k lines), sizes, the timing
   waterfall, rem. A search is now a LIST of levels (each a forward and an
   arc phase) and one concrete result: is a level list what you want to
   browse?
2. Per level, what matters: states per frame and cell, the arc bound,
   W's support and the reached nodes by frame, the forward's timings and
   memory (forward.tsv)? Anything about the concrete search (its DFS/BFS
   steps)?
3. Several runs of one room (the categories, a room's other start
   classes, a retried horizon): side by side in one view, or separate runs
   in the switch as now?
4. The witness against the community path, the input strip and the dash
   markers: keep as they are?
5. The arc artifacts cost disk (the mod room's W support: 230M nodes,
   ~7 GB): written only on request (`--ui-data`), or always for the
   runner's improvements?
6. Old exports (format 1, the ladder's horizons and `bwd`): keep them
   rendering, or re-export what is worth keeping and drop the code?
7. The video export (export.ts): keep?
8. Should run.json's schema be generated from the Rust types (`ts-rs`, a
   build dependency) or checked by a test against data.ts?

## Ranking

Score = lines x confidence / effort (effort in rough half-days).

| # | finding | lines | conf | effort | score |
|---|---|---|---|---|---|
| J1 | dead PICO-8 diff harness and committed generated files | ~400 (+26 MB json, 43k-line baseline) | .9 | 1 | 360 |
| J2 | retire the one-shot and finished-study subcommands | ~630 | .8 | 1.5 | 330 |
| J3 | fold `transpile` and `concrete_run` into `rewrite`; tools step whole frames | ~175 | .85 | 1 | 150 |
| J4 | **boundary**: one writer of the tasdatabase file (the UI's download = the upload) | ~80 | .9 | .5 | 145 |
| J5 | **boundary**: the search writes a result record; nothing parses its log | ~220 | .8 | 2 | 88 |
| J6 | **boundary**: one Python module for the game model the tooling needs | ~200 | .8 | 2 | 80 |
| J7 | stale knobs and docs (refused env vars in CLAUDE.md, a dead flag shim) | ~40 | .95 | .5 | 76 |
| J8 | shared primitives: one worker pool, one fingerprint module | ~125 | .85 | 1.5 | 71 |
| J9 | the census diagnostics as one `tree census` | ~190 | .55 | 1.5 | 70 |
| J10 | **boundary**: the UI's data model is the deleted ladder's | ~1,280 (1.2k TS) | .5 | 10 | 64 |
| J11 | **boundary**: a `Tree` handle; read-only commands take the tree's own settings | ~190 (+~40 script) | .75 | 2.5 | 57 |
| J12 | the search's level loop lives in the binary, written twice | ~70 | .8 | 1 | 56 |
| J13 | the verification diagnostics overlap (ref-check / rerun-row / widen-check; follow / check-known) | ~140 | .7 | 2 | 49 |
| J14 | **boundary**: the cart patches written twice, with different semantics (rm-1) | ~60 | .6 | 4 | 9 |
| J15 | **boundary**: the loading-jank model in Python, its effect in Rust | ~0 net | .5 | 2 | - |

Order of work is not score order:
- J11 (the tree handle) makes J9 and J13 cheap and removes most of what
  J3 and J12 would otherwise copy.
- J5 must precede J10.
- J12 goes with J5.
- Do J1, J2, J4 and J7 first: they are independent and nearly free.

---

## Found while digging (bugs and divergences, not just size)

1. **The UI's "Download .tas" is not the uploaded file** in two recorded
   runs.
   - The runner writes the upload as `f"[{sd}]" + inputs[cut:]`
     (category_runner.py:405).
   - Rust re-derives "the same" file for the UI as `TasFile`
     (ui_export.rs:165-187), from align_tas.py's `db/prologue/seeds` lines.
   - The runner then compares the two and only records the answer
     (category_runner.py:444-446). Two recorded runs disagree:
     - `night2` 100/500m: upload `[-1]...`, UI `[-1,]...` (the seeds'
       trailing comma);
     - `jank/runs2` nodiag/1800m: upload cut at 26, UI prologue 27. The UI
       offers a file one frame different from the one submitted.
   - J4 removes the second writer.
2. **`concrete_run --split-frame` consumes one input byte per HALF frame**:
   it calls `RefEngine::step_one` per input (concrete_run.rs:213), and
   `step_one` is one search step. `trajectory` (rewrite.rs:2364) does the
   same; annex D sf-2 already lists `follow` and `l1-check`. A tool means a
   frame and should only be able to call `RefEngine::frame` (J3, and T11 in
   plans/jank-trace.md).
   - `concrete::platform_worlds(127)` likewise steps 127 steps, while the
     guard in `storage/wave.rs:173-179` admits 127 game frames.
   - The impact looks nil (a platform's `spd.x` is constant), but the
     parameter's unit is wrong.
3. **Three level -1 table builders.** Only `frame::minus_one_table`
   (frame.rs:474-500) has all of these: the cache key, the `--l1-table` pin
   and the win-rect assertion. The other two lack them:
   - `transpile --level-minus-one-table` (transpile.rs:61-70), documented as
     "the A/B check for a change to the build";
   - `rewrite l1-check` (rewrite.rs:2536-2542).

   Each also picks its threads differently. Separately, the runner decides
   its level -1 retry by grepping the search's PANIC text, `building the
   level -1 table` (category_runner.py:315, frame.rs:494).
4. **`category_runner.job_flags` translates `CELESTE_TRIM_ROWS` to
   `--trim-rows`** (category_runner.py:64-85), a flag that no longer exists.
   It would build a command line that `rewrite` refuses. The whole
   `ENV_FLAGS`/`ENV_FIXED` shim is for job files from before 2026-10-10.
5. **CLAUDE.md's "Useful entry points" no longer run.** They set
   `CELESTE_LEVEL_MINUS_ONE`, `CELESTE_START_ROOM`, `CELESTE_SPLIT_FRAME`,
   `CELESTE_TRIM_ROWS` and `CELESTE_THREADS`, which every binary now refuses
   (`settings::refuse_env`). They also call
   `transpile --level-minus-one S LEVEL_DIR ...`, which is now `--probe`.
   The Gates block's search command works.
6. **The PICO-8 numerics cases have not run since 2026-08-30.**
   - `pico8_diff/run.sh` builds `--bin lua_run`, which was deleted then;
     `frontend::compile` is gone too.
   - The 16 cases cover `/`, `%`, `flr`, `sin`, `abs`, literals, `foreach`
     and `add`/`del`. They guarded exactly the 16.16 semantics the concrete
     search relies on.
   - `lua/probe` + `trace::probe` + `regen-pico8-golden.sh` is the live
     version of the same idea, with checked-in goldens, but it holds only
     the 9 table-length corpora (J1).
7. **`trace_frame37.json` (26 MB) is committed**, from 2025-12-31, against
   CLAUDE.md's "never commit generated JSON". So is `baseline/isocheck/`
   (1.1 MB, 43.5k lines, the 2026-08-15 "recipe-stability" gate). Neither
   has a reader.

---

## The boundaries

### J5. The search's outcome travels as log text

**Now.** Three consumers parse `rewrite search`'s output by regex.
- `ui_export::parse_log` (ui_export.rs:211-309, plus tests 1036-1100,
  ~165 lines) reads three kinds of line:
  - the `[fwd] fNNN in ...` lines that `frame.rs:1447` formats from a
    `FrameStats` struct, with compat for older spellings (`emit`/`own`/
    `door`, `rss X GB`, a binary range of 2026-10-09);
  - the `[search] level i (SPEC): forward to fH ...; first win Some(W)`
    line (rewrite.rs:863, whose comment says "`ui_export::parse_log`
    reads this line");
  - `OPTIMAL win frame: F (S s)`.

  It needs `export-ui --log` and makes a level of the UI from the log.
- `category_runner.run` (category_runner.py:315-338) regexes:
  - `peak ([\d.]+) GB`, `ARC BOUND: no win before f(\d+)`,
    `OPTIMAL win frame`, `^(REFUTED...|no concrete win by...)`;
  - the WITNESS from the arc gate's fingerprint line,
    `[gate] h.. concrete optimum N inputs (\S+)` (arc_dp.rs:1897). The
    search has already written that witness to
    `<dir>/witness_frame_F.txt` (rewrite.rs:883-893).
- `gates/raise.sh:45` diffs the search's stdout after a `sed` strips the
  timings.

The arc pass's side outputs are a fourth format. `--save-marks` writes
`arc.txt` (a key-value file `attach_arc` reads, ui_export.rs:789-820) and a
`witness.txt` in yet another format (bw-2).

**Should be.** Records written by whoever owns the data:
- the forward appends its `FrameStats` per frame to the tree
  (`<level>/forward.tsv`, or a field of `frames/fNNN/meta`);
- the search writes `<dir>/result.json`: per level its spec, forward wall,
  first win and arc bound; the verdict (`optimal F | refuted H | no
  witness`); the witness inputs; wall; peak.

Then:
- export-ui reads the trees and `result.json`;
- the runner reads `result.json`;
- raise.sh compares two `result.json`s with the walls dropped;
- the `[gate]` lines go back to being a gate;
- the level -1 refusal becomes a field or an exit code, not a panic message
  to grep.

**Removes.**
- In ui_export: `parse_log`, `is_frame_line`, `parse_fwd`, `num`, `after`,
  `level_run`'s log half, `read_keyed` + `arc.txt` and its tests;
  `export-ui --log` and `--paths-only` (it exists because the log and
  trees go away).
- The runner's regexes.
- About 220 lines in all.

**Risk.** Low.
- The data already exist as structs (`FrameStats`, `Solved`).
- Logs of old runs can no longer be exported. The exported `ui/data` stays,
  and old runs would need the old binary.

**Verify.**
- Export a stored run both ways. `run.json` must be equal but for the
  fields J10 drops.
- Run the runner on a gate job: same `results.jsonl` fields.
- `gates/raise.sh --search 1,0 ... 55 56` passes.

### J4. Two writers of the tasdatabase file

**Now.**
- The upload is written in Python (category_runner.py:405).
- The UI's download is re-derived in Rust from a path file:
  - `TasFile::new` (ui_export.rs:164-187);
  - the `db`/`prologue`/`seeds` handling in `read_witness` (725-784),
    including a "database count to the EXIT" correction;
  - its test (1068-1074).
- In between, align_tas.py writes `db/prologue/seeds` lines
  (align_tas.py:55-56) from the reference's offset, not from the upload's
  cut.
- Then the runner rewrites the seeds line to match the upload
  (category_runner.py:434-438) and records whether the two came out equal
  (444-446). They did not, twice (see "Found while digging", item 1).

**Should be.**
- The witness/path file carries `upload PATH`, or export-ui takes
  `--download FILE`. `run.json` embeds that file's bytes verbatim.
- Rust stops knowing the tasdatabase format.
- The level name to `TAS<n>` mapping, the prologue stripping and the seed
  printing exist once, in Python (J6).

**Removes.** ~60 Rust lines (`TasFile`, three header keys, the test
cases), the runner's seed rewrite and equality check, and align_tas's three
header lines. ~80 in all.

**Risk.** None for the search. The UI's download button changes source.

**Verify.**
- Re-export `night2` 100/500m and `jank/runs2` nodiag/1800m.
- The `run.json` download must be byte-equal to the upload file.

### J11. A `Tree` handle; read-only commands take the tree's settings

**Now.** Every diagnostic reassembles a tree from parts.

The parts:
- `tree_of` (rewrite.rs:704-710);
- 18 `Resolver::load`/`tree_keys` calls;
- four hand-written "last frame" loops (rewrite.rs:706, 2057, 2336, 2430);
  ui_export.rs:547-555 is a fifth;
- 4 `EdgeStore::open(dir.join("edges"), ..)` (rewrite.rs:1073, 1371, 1919,
  1995).

Then it asks the user to restate what the tree records:
- `--level` beside `--level-dir`, and every setting (`--room`, the
  category, `--split-frame`, `--loading-jank`, balloons) so that
  `cfg.check(dir, level)` passes (7 call sites);
- `cfg.room` is passed to 20 row decodes (`load_rows(.., cfg.room, ..)`).

The 9 subcommands that skip `cfg.check` would decode a (6,2) tree in room
(1,0) without a word (ckhash, col-census, coarse-census, cell-growth,
spurious, diag-project, edge-census, bench-backward, export-ui). This is
harmless today only because their outputs do not read the collision cache.

Scripts pay the cost:
- check.sh passes `--room R --level L $L1_62` to every diagnostic line
  (check.sh:67-142);
- fixtures.sh, raise.sh and check.sh pass `--room` to `ckhash`, which
  ignores it;
- `bench-storage` asks you to retype the capture's settings from its
  `env.txt` (rewrite.rs:300-301).

Six names exist for "the tree": `--checkpoint-dir` (also used for a single
level's tree by ckhash), `--level-dir`, `--tree`, `--coarse/--real`,
`--fine-dir/--coarse-dir` and `--dir`. Three exist for its level:
`--level`, `--spec` (trajectory) and none (bounds-audit). Three exist for
its horizon: `--to`, `--horizon` and `--step`.

`settings.txt`, `level_minus_one.txt`, `filtered_for.txt` (written by the
BINARY, rewrite.rs:839-850) and `raising.txt` are four per-tree records,
each read by its own code.

**Should be.** A `Tree::open(dir)`, next to `storage` (it is the read side
of what the forward writes), that holds:
- the recorded `Settings` and `Level`, the `TreeFilter` and the last
  frame;
- the `Resolver`, and the `EdgeStore` on demand;
- `rows(frame, sel)`, `row(id)`, `files(frame)`;
- `engines()`: the kernels and the reference built under the tree's own
  settings.

Every read-only subcommand takes `--tree DIR` and REFUSES the settings
flags. Only run options remain (`--threads`, `--l1-table`, `--path-cap`,
diagnostics).

This does not contradict plans/footguns.md item 2 ("flags do not default to
the record on resume"). That rule protects a command that BUILDS a tree.
A diagnostic only reads one, and restating its settings can only be wrong.
A shared row selector (`--frame F [--cell x,y] [--samples N] | --id ID`)
replaces the four ad-hoc samplers (rewrite.rs:1684-1688, 1704-1716,
1564-1565, `--row L:S:R`).

**Removes.** ~190 lines in rewrite.rs (setup, checks, room plumbing,
samplers, `check_any_level`), and ~40 in check.sh, fixtures.sh and
raise.sh. Six flag spellings collapse to one.

**Risk.** Low.
- A tree without `settings.txt` cannot be read. That is already true for
  most commands, and `adopt-settings` covers it until J2.

**Verify.**
- `./check.sh t1` and `t2` give identical outputs after the settings flags
  are dropped from the diagnostic lines.
- A diagnostic given `--room` refuses.

### J6. The tooling reimplements the game model, five times over

None of this is shared:

| concept | copies |
|---|---|
| tasdatabase `[seeds]inputs` parsing | chain.py:61-66 (floats), category_runner.py:92-94 (raw string), align_tas.py:17 (inline), uct/validate.sh:48 (sed count) |
| `database.json` lookup | chain.py:81-89 (by file, with FALLBACK), category_runner.py:228 (by name), check_uploads.py:75 (by file), align_tas.py:15 (by name) |
| category -> behaviour | `mode_flags` (category_runner.py:50-61), `replay_args` (:88), `CAT_LABEL` (:47), `FALLBACK` (chain.py:41-51), `cat.startswith("gemskip")` (chain.py:137, 142; check_uploads.py:71-72), `UCT_DASHES`/`CELIA_DASHES` env (category_runner.py:182, check_uploads.py:37, 45, 50) |
| level / room / name | `room_of` (chain.py:57), `rx + 8*ry + 1` (category_runner.py:242, 472), `int(name.rstrip("m"))//100` (:260, 393), `TAS(\d+)` (check_uploads.py:69), Rust `level_index` (game_runner.rs:22), `TasFile`'s name parse (ui_export.rs:178), `levelIndex`/`roomTitle` (ui/src/data.ts) |
| the map's object tiles | `minimal_jank` + `OBJECT_TILES` (replay.py:48-61), `room_objects` (chain.py:229-232), `feasible_janks` scanning again inline (chain.py:254-256), `seed_tiles` with its OWN hex-map reader (category_runner.py:191-197); in Rust `fix_balloon_seeds`, `minimal_index` and `room_tiles` |
| hard-coded paths | `DB` and `ORIG` in chain.py:36-37, category_runner.py:44-45, align_tas.py:9, 20 (`/home/philippe/...`) |
| the replay's `@P8@` lines | parsed by regex in category_runner.py:111-116 (`replay_exit`) and 166-170 (`canon_jumps`: a jump "fired" iff `spd.y == 0xfffe.0000`), align_tas.py:25-33, chain.py:94-97 |
| "the berry was taken" | a `lifeup` line before the exit (category_runner.py:360-361) vs `got_fruit` in the exit line (replay.py:262) |
| "a diagonal dash" | input-byte heuristics `diag_dashes`/`straight_dashes` (category_runner.py:134-151). Meanwhile replay.py records each dash START as the cart decides it (`dash DIR`, replay.py:107-109), and align_tas already reads that. |

Layering is also wrong: check_uploads.py imports `category_runner` for
`nudged_seeds` (check_uploads.py:30, 110), and category_runner imports
check_uploads (:396).

**Should be.** One package, `tools/celeste/`, with no CLI:
- `categories.py`: flags, fallbacks, label, one dash, hundred;
- `tasdb.py`: the file parsed and written once, and the database lookup;
- `rooms.py`: level, room and name; object tiles;
- `pico8.py`: cart build, run, and the `@P8@` lines parsed into typed
  frames (room, player, spd, rem, dash, lifeup);
- `love.py`: the UCT and Celia runs, nudges included.

replay.py, chain.py, category_runner.py, check_uploads.py and align_tas.py
become clients. The dash and berry checks read the cart's own lines. The
object tiles could come from Rust (`rewrite room-info --json`), so the tile
lists exist once (J14).

**Removes.** ~200 of the ~1,700 lines in these five files. One module
replaces the import cycle.

**Risk.** Medium-low.
- The runner is long-running and hard to test end to end.

**Verify.**
- Rerun `tools/uct/check_uploads.py` over `tas/tasdatabase/*/upload/*.tas`:
  identical rows.
- Rerun one improved and one tie job of a stored jobs file: identical
  `results.jsonl` (minus times).

### J10. The UI's data model is the deleted precision ladder's

**Now.** The export fakes a ladder run so that the UI can draw a search.
- One `HorizonRun` (ui_export.rs:98-105).
- `bwd: [u32; 0]` "kept for the UI" (76), `reruns` always `None` (79),
  `marks_have_dist` always true (88, 263), `prebuild_s` always `None` (132,
  984).
- The arc pass is a synthetic extra level with `forward_of`
  (ui_export.rs:811-815) and "precision" strings like `"r0sxh, rem free"`.
- Marks files are read by byte offset:
  - `MARKS_ROWS_AT = 24`, the checkpoint header (16) plus bincode's `Vec`
    length (8), then 32-byte rows (ui_export.rs:353-388);
  - this is a second reader of `frame::save_marks`'s bincode layout and
    field order.

The UI mirrors all of it by hand:
- `BwdLine`, the `bwd` chapters and `Phase = "fwd" | "bwd"` (data.ts:27-38,
  229-286);
- `precisionName` for `Bits(k)` and `ladderPrecisions` (305-322);
- verdicts per horizon, `defaultHorizon` and `horizonOrder`, for
  count-down ceilings (380-427);
- `LEVELS = 17` and the height bands (color.ts:46, 110-150);
- "backward iterations" and "kernel re-runs" (sizes.ts:15-50, 133-201);
- "kernel prebuild (17 rungs)" (timeline.ts:31-34);
- the horizon picker, the ladder panel and passes in space.ts (1,741
  lines, ~330 references to horizon, pass, ladder, band or bwd);
- a comment on `--forward-only`, which no longer exists (data.ts:69).

architecture.md "Deferred follow-ups" item 4 notes the problem.

**Should be.** An export model of TODAY's search, written from J5's
records:
- the room and category, the horizon, the tiles and box;
- per level: the spec, per-frame forward stats, the frames file, the first
  win, the arc bound, W's support and the reached-node files (written by
  the search in a format of their own, not `Visited`'s bincode);
- the concrete result: the optimum, and the witness and reference paths.

The UI then shows a level list, not a horizon x ladder grid. The JSON
schema should be generated from the Rust types (`schemars`/`ts-rs`) or
checked by a test, not mirrored by hand.

**Removes.** By eye: ~1,200 TS (horizon and verdict logic, bwd chapters
and metrics, ladder naming, bands, the passes grain), ~80 Rust, and most of
ui/README's ladder text.

**Risk.** It is a redesign. What the UI should show is Philippe's call.
Old exports would stop loading (`format: 2`).

**Verify.** Type-check, build, and look at three exported runs: a
one-level room, the (7,0) ladder and a nodiag room with a reference path.

### J14. The cart patches, written twice with different semantics (extends rm-1)

**Now.** The start room, the loading frame and the balloon seeds are
patched into the Lua twice, and the two disagree in detail:

| patch | Rust (`trace/cart.rs`, `game_runner.rs`) | Python (`replay.py` `build_cart`) |
|---|---|---|
| start room + orb | `load_room(1, 0)` -> `max_djump=2 load_room(x, y)` past level 21 unless `--gemskip` | the same text, also for the original cart's `load_room(0,0)`; `--one-dash` for `--gemskip` |
| seeds | balloons only, each inlined by POSITION (`fix_balloon_seeds`); chests left to `rnd` | balloons AND chests from a queue, UCT's order (`__balloon_seed`), the chest's last shake fixed |
| loading frame | inlined after `load_room` inside `_init`: `if o.spd.x ~= 0 or o.spd.y ~= 0 then o.move(..) end` | appended after `_init()`: `if o then o.move(..)` unconditionally |
| J translation | `OBJECT_TILES` + `minimal_index` | `OBJECT_TILES` + `minimal_jank` (a verbatim twin) |

**Should be.** One implementation with explicit options. Rust is the
natural owner: the search must run exactly these patches.
- `rewrite cart --lua minimal|original --room --loading-jank
  --balloon-seeds [--chest-seeds]` prints the patched Lua.
- replay.py only appends its driver.

The concrete search's chests stay `rnd`, a stated option rather than an
accident.

Independence is not lost by sharing:
- the patches are already validated against what does not use them, the
  boot chain on a real PICO-8 (`chain.py --check-start`);
- the replay's job is to run the witness on real hardware, not to
  re-derive the patches.

**Removes.** ~60 Python lines. Rust gains ~30.

**Risk.** Medium: a replay could change subtly.

**Verify.**
- `chain.py --check-start` for rooms (3,1), (6,2) and (2,1) is unchanged.
- Replaying every `tas/*.txt` witness and every upload gives identical
  exit frames.

### J15. The loading-jank model lives in Python, its effect in Rust

`chain.feasible_janks` (chain.py:244-267) models the set of feasible J's:
- the destroyable tiles;
- `MAX_SPEED = 6` (which must equal the search's region S);
- the title's 36 frames and the smoke's 15;
- a PICO-8 run of the previous room to find its spawn frame.

`start_classes` (chain.py:270-280) then shells out to `concrete_run
--dump-start` once per J and groups the classes by the dump's TEXT. The
effect of J is Rust's `apply_loading_frame`.

The model belongs next to the patch, as `rewrite starts --room X,Y`:
- the J's, and their classes by exact state;
- no subprocess per J;
- S read from the settings, not a copy;
- the spawn frame from the reference engine.

The boot chain (ground truth) stays in Python.

Net lines ~0. This is about where the model lives. Low priority.

---

## The CLI

### J2. Retire the one-shot and finished-study subcommands

| subcommand | lines (arm + clap + helpers) | why it can go | conf |
|---|---|---|---|
| `bounds-audit` | 113 + 14 + `Settings::check_any_level` ~15 (rewrite.rs:2043-2155, 537-550; settings.rs:202-206) | It audited, once, that no stored row lay outside the bounds kernels did not check "until `Op::Restrict`". Every kernel now checks them per lane as its own error, a FATAL coverage gap (architecture.md, "A bound is a node"). It is its only user of `check_any_level`. | .9 |
| `convert-tree` | 15 + 12 + `storage/convert.rs` 316 | A format-13 -> 14 migration (2026-10-11). Delete after one sweep of the disk for format-13 trees. | .8 |
| `adopt-settings` | 8 + 13 | Records settings for trees built before 2026-10-10. Same sweep. | .7 |
| `spurious --chain-out` + `trajectory --spec/--tree` (keyed lines) | ~60 (rewrite.rs:513-518, 1931-1991 in part, 621-636, 2309-2343, 2374-2381, 2405-2409) | A realizer for abstract chains from the rem-ladder days. The `trajectory` doc still names it `ancestry --chain-out`, a name that is gone. | .7 |
| `edge-census` | 49 + 17 | A study of transfers per (source, target) pair, marked stale in SPEC annex B. The edge storage is being redesigned: let its owner keep or drop it. | .6 |
| `storage-census` | 4 + 15 + `storage/census.rs` 552 | NOT proposed now: plans/storage-unify.md section 4 says keep it to re-price layouts. Revisit with the edge redesign. | - |

**Removes** ~630 lines, or ~1,200 with storage-census.

**Risk.**
- A kept tree of format 13, or one without `settings.txt`, becomes
  unreadable. It can be rebuilt; run the sweep first.

**Verify.**
- `./check.sh t0`, which includes `the_command_line_is_well_formed` and
  `scripts_wrapped`.
- Grep plans/, CLAUDE.md and scripts for the names.

### J3. Fold `transpile` and `concrete_run` into `rewrite`

`transpile` (89 lines) is two level -1 commands (kn-2). It has:
- its own `Cli`, its own copy of `the_command_line_is_well_formed`, and
  two 256 MB-stack thread spawns (transpile.rs:51-57, 63-69);
- a third table builder that bypasses `frame::minus_one_table` (item 3
  above).

`rewrite l1-check` (rewrite.rs:2534-2597, + clap 683-699) is the third
level -1 command and the third spawn.

**Should be** `rewrite l1 table S | l1 probe ... | l1 check ...`, each going
through `frame::minus_one_table` (made public). That gives one builder, the
pin, the cache key and the win-rect assertion everywhere.

`concrete_run` (235 lines) duplicates several things:
- replay.py's `TYPES` (concrete_run.rs:46, replay.py:38);
- an input parser that panics on a bad byte (190), beside
  `concrete::read_inputs`, which reads `tas/` files too;
- a third way of naming a row's fields: `dump_start`/`dump_value` (beside
  `search::inspect::cell_names` and `concrete::num_at`).

It also steps half frames under `--split-frame` (item 2). **Should be**
`rewrite run --inputs FILE|LIST [--frames N] [--object T] [--dump-start]`
over `RefEngine::frame`.

`trajectory`, `follow` and `l1-check` switch to `frame` too.
`RefEngine::step` and `step_one` then have no tool callers.

**Removes.** ~175 lines net, two `[[bin]]`s, two copies of the clap test.
Callers to update: chain.py (`check_start`, `start_classes`), the runner's
`CONCRETE_RUN`, CLAUDE.md, pico8_diff/README.

**Verify.**
- The old and new `--dump-start` outputs are identical for 3 rooms.
- `run -i` gives the same frame lines as `concrete_run` without the split.
- With `--split-frame`, run equals the run without it, as
  `concrete::split_frame_lands_on_the_unsplit_states_through_a_dash_freeze`
  already asserts for the engine.
- `rewrite l1 table 5 --room 2,0`'s fingerprint is unchanged.

### J12. The search's level loop lives in the binary, twice

`Search` (rewrite.rs:811-907) and `CheckKnown` (908-934) each contain:
- the levels parsed from a comma string (821, 912);
- the `prev` marks filter;
- the `level{li:02}` naming;
- frames turned into steps;
- the `solve` call with 9 positional arguments, three of them `Option`s and
  two of them bools.

`Search` additionally holds the objects ladder's tree validity record,
`filtered_for.txt` (839-850). That is a storage concern written by the
CLI. The initial-state closure is repeated in `Forward` (858, 958).

**Should be** `search::run(&SearchSpec) -> SearchResult` in the library
(the same value J5 writes as `result.json`), with `check-known` as
`run(spec { forward: ReuseOnly, concrete: false, known })`.
- A `LevelList` value parser.
- The binary prints and maps verdicts to exit codes.

**Removes** ~70 lines. **Verify:** the arc gate and `search42` are
unchanged (check.sh t2).

### J13. The verification diagnostics overlap

- **`rerun-row`** (rewrite.rs:1794-1823, + clap 460-480) is `ref-check`'s
  kernel half for one row, printed instead of compared.
  - **`widen-check --emitted`** (1681-1694) reruns sampled rows through the
    kernels with its own sampler.
  - **Should be** one `check kernels --tree T <row selector> [--reference]
    [--widen] [--print]`: one sampler and one rerun, with three comparisons
    (the reference engine's successors, the block-side widening's fixed
    point, none).
- **`follow`** (2421-2510, + clap 637-657) steps a concrete route through a
  tree's rows. `check-known` (`search::known`) does the same against every
  pruning step and reports the node and whether the tree holds it. What
  `follow` adds is the kernel diff at the first miss: the parent through
  the kernels, and the closest kernel successors by field distance.
  - **Should be** `check-known`'s explanation on a "not in the tree"
    failure.
  - `follow` also loads every row of every frame into a hash map up front
    (2428-2441). On a big tree that is the whole tree in memory.
- **`l1-check`** moves under `l1` (J3).

**Removes** ~140 lines. **Verify:**
- inject the two faults in architecture.md's known-route section; the
  merged report still names them;
- check.sh t1's refcheck and widen steps give identical lines.

### J9. The census diagnostics are one command

These four all iterate the rows of a frame range (optionally one cell),
name the cells (`inspect::cell_names`) and count:
- `cell-growth` (1286-1363 + 376-402): new states per cell per frame;
- `col-census` (2156-2275 + 551-581): per shape and column, distinct values
  and a cell's saturation;
- `coarse-census` (1631-1640 + `erased_states` 715-750 + 357-375): distinct
  states with prefixes erased;
- `ckhash --cells` (1142-1151).

**Should be** one `tree census --frames A..B [--cell x,y] [--erase P,..]
--by frame|cell|column|age`, built on J11's row iterator. Today's mid-frame
1M-row chunking is written twice (727, 2181).

**Removes** ~190 lines. Confidence .55: these are exploration tools, and
the right set of views is a judgment call.

### J8. Shared primitives

- **One worker pool.** The same scoped pool is written three times in
  this area (an atomic cursor, `thread::scope`, results sorted by index,
  join-panic messages):
  - arc-check (rewrite.rs:1566-1596);
  - ref-check (1756-1781);
  - `ui_export::par_map` (321-351).

  There are 25 `thread::scope` sites in `src/`; T4 in jank-trace makes the
  same point for the tracer. Put one `par::map_indexed(n, threads, f)` in
  the library.
- **One fingerprint module.** The `e`-line edge hash is written twice:
  - `ckhash --edges` (rewrite.rs:1061-1126);
  - `storage::bench::edge_fingerprint` (bench.rs:340-370).

  In both, the pair hash fold and `mix(t ^ s.rotl(21) ^ p.rotl(42))` are
  copied. The `f`-line state sum exists as `layer_fingerprint`
  (bench.rs:328) and inline in ckhash (1039-1060). The `d` line and the
  arc gate's `gate_w_hash` are cousins. **Should be** `storage::fingerprint`
  with `state`, `edge`, `pair`, `note` and the line formatters, used by
  ckhash, bench-frame, bench-storage and bench-backward.

**Removes** ~125 lines. **Verify:** the ckhash gate,
`gates/fixtures/*.frame` and `r10arc.backward` are byte-identical
(check.sh t1, t2).

---

## The tooling

### J1. The dead PICO-8 harness and committed generated files

- `pico8_diff/run.sh` (174), `wrap.sh` (40), `known_fail.txt`, `gen/`
  and most of `pico8_diff/README.md` (~160): a harness for `lua_run`, which
  has not existed since 2026-08-30.
  - `src/scripts_wrapped.rs` still special-cases it (lines 44, 74-81).
  - Its 16 `cases/` (620 lines of numerics, `foreach` and `add`/`del`
    probes) are worth keeping. **Port them to `lua/probe`**, which runs in
    `trace::probe` against goldens checked in by `regen-pico8-golden.sh`.
    This needs an exact-hex print in the probe runner, because the cases
    print `tostr(x, true)` and the tracer has no `tostr`.
- `trace_frame37.json` (26 MB), `baseline/isocheck/` (1.1 MB) and
  `PROGRESS.md` (a merged branch's working log): no readers.
- After the port, `pico8_diff/` holds only replay.py and chain.py; they
  belong in `tools/` (J6).

**Removes** ~400 hand-written lines and ~27 MB.

**Risk.** None for deletion. The port can show a numeric divergence that
has gone unseen since August. That is the point of doing it.

**Verify.** `cargo nextest run probe` gains the ported corpora; on a
machine with PICO-8, `regen-pico8-golden.sh` writes their goldens.

### J7. Stale knobs and docs

- CLAUDE.md "Useful entry points": rewrite with flags (item 5).
- `category_runner.ENV_FLAGS`/`ENV_FIXED`/`job_flags` (category_runner.py:64-85):
  delete. A job with `env` should fail loudly, as the binaries do.
- `rewrite.rs:623` cites `ancestry --chain-out` (the name is `spurious`);
  `data.ts:69` cites `export-ui --forward-only` (gone).
- `fixtures.sh formats()` greps `FORMAT_VERSION` out of Rust SOURCE
  (fixtures.sh:69-71). `rewrite formats` (or the binary's own refusal of a
  stale tree) is the honest source.

**Removes** ~40 lines.

### Keep as they are

- **`safe-run.sh`** (the headroom refusal) and **`one-cargo.sh`**.
- **`settings.rs`**: one value from the flags, recorded in the tree, refused
  in the environment. Its one smell: every setting is written four times
  (struct, args, `recorded`, `startup`'s log). A table-driven
  `recorded()`/`startup` would save ~40 lines. Not ranked.
- **`check.sh`'s tiers** and **fixtures.sh's** pinned fingerprints.
- **The `scripts_wrapped` test.**
- **`metrics_diff.py`** and **`compare_video.py`**.
- **`uct/` and `celia/`**. Their `validate.sh`s share most of their shell
  (~40 lines could merge behind `--tool`), but the Lua drivers differ for
  real reasons (Celia deep-copies the state each step).
- **The real-play adjustments in the runner**: the offset scan,
  `chest_seed_variants`, `nudged_seeds`, the cut candidates and
  `chain.OVERRIDES`. They are domain workarounds with documented causes
  (UCT's doubles, the database's own files).
- **`canon_jumps`**, which exists because the minimal cart drops
  `jbuffer` (lua/celeste-minimal.lua:102-107). That is a fidelity choice
  outside this area.

`kernel_mix.py` re-derives the dynamic mix by category that
`bench-frame --metrics` (`frame_metrics`, rewrite.rs:2603-2670) also sums.
Each serves its reader, so it is not ranked.

---

## The proposed structure

### One binary, grouped by what a command touches

```
rewrite search      --room --level SPEC[,SPEC] (--to H | --ceiling H) [--prefer F] [--no-witness] [--ui-data] --dir D
                    -> D/levelNN/, D/result.json, D/witness_frame_F.txt, D/ui/ (W support, reached nodes)
rewrite forward     one level, profiling and gates          -> D/ (+ per-frame stats in the tree)
rewrite run         --inputs F|LIST [--frames N] [--object T] [--dump-start]   (was concrete_run; whole frames)
rewrite trajectory  --positions F                                              (fit a TAS's positions)
rewrite starts      --room X,Y                       (feasible loading jank J's and their start classes; J15)
rewrite cart        --lua minimal|original --room ... (the patched Lua, for replay.py; J14)
rewrite l1          table S | probe ... | check --inputs F --horizon H         (was transpile + l1-check)

rewrite tree  <cmd> --tree T [--frame F | --frames A..B] [--cell x,y] [--erase P,..]
                    READ-ONLY; settings, room, level and filter FROM THE TREE; settings flags refused
      hash          ckhash: f/e/d lines
      census        cell-growth + col-census + coarse-census + ckhash --cells
      rows          one row or a sample, by name; --ancestry N (spurious's walk back)
      project       --onto COARSE_TREE [--marks ..]   (diag-project; spurious --real)

rewrite check <cmd>   (exit non-zero on a disagreement)
      known         --dir D --inputs F --horizon H   (check-known; follow's kernel diff on a miss)
      kernels       --tree T <rows> [--reference] [--widen] [--print]   (ref-check, widen-check, rerun-row)
      transfers     --tree T --from --to [--samples N] [--fault K]      (arc-check)

rewrite bench <cmd>   frame | backward | storage   (+ storage-census, edge-census until the edge redesign decides)
rewrite export-ui     --dir D --out O [--witness F --reference F --download F]   (no --log, --room, --arc)
```

Gone:
- `bounds-audit`, `convert-tree`, `adopt-settings` (after the disk sweep);
- `follow`, `rerun-row`, `spurious --chain-out` and the keyed trajectory;
- the `transpile` and `concrete_run` binaries.

In the library:
- `Tree::open` + a row selector (J11);
- `search::run` (J12);
- `storage::fingerprint` and one worker pool (J8).

`src/bin/rewrite.rs` becomes `src/bin/rewrite/{main, search, tree, check,
bench, l1, run}.rs` at roughly 1,500 lines, down from 3,008 in the three
binaries.

### The tooling

```
tools/celeste/       a package, no CLI: categories, tasdb (parse/write once, database.json),
                     rooms (level/room/name; object tiles via `rewrite cart`/`room-info`),
                     pico8 (cart driver, run, typed @P8@ frames, chains), love (UCT, Celia, nudges)
tools/replay.py, tools/chain.py      thin CLIs (moved from pico8_diff/)
tools/category_runner.py             jobs -> boot-chain J -> reference -> `rewrite search` -> result.json
                                     -> original-cart replay -> ONE upload file -> export-ui --download
tools/check_uploads.py, align_tas.py (paths only), compare_video.py, kernel_mix.py, metrics_diff.py
tools/uct/, tools/celia/             LOVE harnesses
check.sh, tools/fixtures.sh, gates/raise.sh, safe-run.sh, one-cargo.sh    (minus restated settings)
lua/probe/                           + the 16 numerics cases from pico8_diff/cases
```

**Where the Python/Rust line goes.**
- Python drives external programs: PICO-8, UCT, Celia, LOVE and the
  tasdatabase. It owns the job queue, the uploads and the videos.
- Rust owns the game model and the search's facts:
  - the cart patches (J14) and the loading-jank model (J15);
  - the search's outcome as a record (J5).
- Python never parses Rust's stderr, and Rust never writes a tasdatabase
  file (J4).

The runner's shape is right: a resumable queue of jobs, each checked end to
end on real hardware. Its interface to the search is not (J5).

---

## Considered and rejected

- **Dumping rows as TSV and analysing them offline** instead of J9's
  census. Room trees hold 10^7-10^8 rows a frame. The censuses stream and
  count, and a dump would be the size of the tree. Keep one streaming
  `census`.
- **Moving the boot chain, `check_uploads` or the offset scan into Rust.**
  They exist to run the witness on OTHER implementations (PICO-8, UCT,
  Celia). A Rust reimplementation would be checked against itself.
- **Making the replay re-derive the cart patches independently**, to keep
  two implementations as a cross-check. The cross-check that matters is
  the boot chain, which uses no patch (`--check-start`). Two diverging
  patch texts (J14's table) are a liability, not a check.
- **Deleting `bench-storage`, `bench-backward` or `bench-frame`.** check.sh
  t1 pins their outputs, and they are the only single-frame A/B tools for
  the storage, the backward and the kernels.
- **Writing the arc artifacts (W support, reached nodes) always** instead
  of `--ui-data`. The mod room's support has 230M nodes (~7 GB at 32 B a
  row). Keep them opt-in, but under the checkpoint dir, in their own
  format.
