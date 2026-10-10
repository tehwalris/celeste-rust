# Footguns: what was fixed, how it is tested, what to review (2026-10-10)

Branch `footguns` (off `develop` 5f3b2d6, the widening refactor). Philippe:
"let's get all of that fixed. Also, generally, avoiding foot guns and
avoiding too many knobs is good", then "in general I'd prefer command-line
flags instead of env vars". Items as SPEC annex D names them (fw-*, bw-*,
gl-*). Outputs byte-identical except item 2 (below); every check: t0, t1,
t2 after each commit, `big` at the end.

## The items

| # | footgun | fix | test |
|---|---|---|---|
| 1 | fw-2/fw-3: the pos graph saved AFTER `edges/done.txt` (a crash between left a tree `resume` refused); three trust markers (`done.txt`, `xfer.len`, `posgraph.frame`) | every frame writes drop notes, frame files, meta, pos graph, THEN `done.txt` (then trims the previous frame's rows, which only a done frame may lose). ONE marker: `done.txt` = `F xfers N` (the frame and the transfer table's trusted length). A raise re-marks at its end. | `a_frame_killed_before_its_done_marker_is_rerun`: kernels, room (1,0) to f30, the marker rewound to f29, resume -> f29, rerun -> every frame's keys and `done.txt`, `xfer.bin`, `posgraph.bin`, `settings.txt` byte-identical to the uninterrupted tree |
| 2 | fw-5: `extend` applied `not_expanded` only on a frame with a win (or in the orb room), `resume` always | `frame::expandable`, the ONE function: start (frame 0), extend, raise and `ForwardState::at` | `a_resume_expands_what_the_uninterrupted_forward_expands` (100% in room (1,0), which has no berry: every row lost from f0; the frontier and first win of `at` equal the forward's at every frame) |
| 3 | fw-4: the forward's first win was `reaches_win` (berry aside), the trees' (`restore_visited`, `tree_first_win_through`) the win rows (`wins_of`) | `checkpoint_frontier` returns whether a stored row wins: the forward reads its first win off the rows a resume reads; `Wave.won` gone, `reaches_win` private | the same test (first win per frame); item 2 removed the reason for the old notion (below) |
| 4 | bw-4: `known::check` took level -1 from the environment | `TreeFilter::of_tree`: the tree's `level_minus_one.txt`, its table checked against the recorded fingerprint; no record or a legacy one refused | `a_trees_level_minus_one_record_round_trips` (off, legacy, none); t2's two `--prefer` searches |
| 5 | gl-2: nothing recorded what a tree was built under | `src/settings.rs`: ONE record `<level>/settings.txt` (room, win rect, split frame, kernel region, storage region, hundred, nodiag, onlydiag, gemskip, loading jank J, balloon seeds, the level), written with frame 0, checked on resume, reuse, bench-frame, the arc phase and the `--level-dir` diagnostics, every difference named | `settings::tests::a_tree_is_refused_under_other_settings`, `flags_make_the_settings` |
| 6 | fw-6/fw-7: `CELESTE_KERNEL_KEY_CHECK=0` turned the check ON; flag parsing per knob | superseded by the flags (below): no environment knob is read at all; a binary refuses every `CELESTE_*` variable, naming the flag that replaced it | `flags_make_the_settings` (malformed values refused) |
| 7 | fw-10: edge-file frame numbers parsed as 3 digits | `edges::frame_of_name`: every digit after the `f`. The other readers (`frames/fNNN` by path, `ui_export`, bounds-audit) parse the whole name already; `dropped/` and metas are only ever built from a frame number | `edge_file_names_give_their_frame_at_any_width` (f999, f1000, f1234, raised and block names; `discard_after` past 999) |
| 8 | dead tools, dead and undocumented knobs | below | - |

## Item 2 changes 100% trees (raised, isolated in its own commit)

Room (6,2) 100% (L1 94): f27-f43 lose 4-13% of their states (266 -> 248
... 930013 -> 894307): rows whose berry is LOST (no fruit, fly fruit, key,
chest or fake wall left, the berry not taken), which c59b373 meant never to
expand and an uninterrupted forward expanded until its first exit. From f44
on the states are IDENTICAL (level -1, which knows the berry, cut those
lineages there anyway); a few edges fewer per frame. Re-pinned `r62h` and
`r62h57` (the only 100% fixtures); the t1/big frame pins did not move
(`bench-frame` already started from `at()`'s frontier). Exact (no state of
a lost row can win), so no optimum can move. No other fixture or gate is
100%. Under `--win-at` the old guard also let a row that LEFT the room into
the next frame on a frame without a win (a coverage gap, fatal, so never a
silent result).

Item 3's history: `601a29d` made the first win `reaches_win` because the
same `won` switched the mask on, and a 100% exit without the berry must not
be expanded (a coverage gap at (6,2) f66). With the mask always on, the
first win is only reported; it is now the true win, so `win_frame` cannot
differ after a resume or a reuse. (In a 100% log the "first win" line now
names the first exit WITH the berry.)

## Settings are flags

Every setting is a flag of the binaries (`settings::SettingsArgs`,
flattened `global` into `rewrite`, and into `transpile` and
`concrete_run`), parsed and validated into ONE `settings::Settings`, which
is also what a tree records (its search-changing part). Made once per
process by `settings::startup` and PASSED to what reads it (no global
since branch `settings-explicit`, below).

| flag | was | recorded in a tree |
|---|---|---|
| `--room X,Y` | CELESTE_START_ROOM (and per-command `--room`) | yes |
| `--win-at x,y \| x0..x1,y0..y1` | CELESTE_WIN_AT_XY (and `--win-at`) | yes |
| `--split-frame` | CELESTE_SPLIT_FRAME | yes |
| `--region px,S \| off` | CELESTE_REGION | yes |
| ~~`--storage-region 8\|16`~~ (cut: fixed at 8) | CELESTE_STORAGE_REGION (was also checked by the metas) | old records' `storage_region 8` accepted |
| `--hundred`, `--nodiag`, `--onlydiag`, `--gemskip` | CELESTE_* | yes |
| `--loading-jank J\|none` | CELESTE_LOADING_JANK | yes |
| `--balloon-seeds S` | CELESTE_BALLOON_SEEDS | yes |
| `--concrete-balloon-seeds S` | CELESTE_CONCRETE_BALLOON_SEEDS | no (the concrete phase) |
| `--level-minus-one H,S` | CELESTE_LEVEL_MINUS_ONE | its own record (`TreeFilter`: a raise changes it) |
| `--trim-rows` | CELESTE_TRIM_ROWS | no (the files say which rows are trimmed) |
| `--threads N` | CELESTE_THREADS (and per-command `--threads`) | no |
| ~~`--min-free-gb N`~~ (cut: 20 GB, `frame::MIN_FREE_GB`) | CELESTE_MIN_FREE_GB | no |
| `--l1-table FILE` | CELESTE_L1_TABLE | no (the tree records the table's fingerprint) |
| `--key-check` | CELESTE_KERNEL_KEY_CHECK | no |
| `--kernel-explain`, `--kernel-mix DIR`, `--build-trace`, `--phases` | CELESTE_* | no |
| `--emit-capture DIR` (every wave; ~~`--emit-capture-frame F`~~ cut) | CELESTE_EMIT_CAPTURE(_FRAME) | no |
| ~~`--walk-regions`~~, ~~`--l1-path x,y`~~ (cut) | CELESTE_WALK_REGIONS, CELESTE_L1_PATH | no |
| `--override-settings` (new) | - | no |
| (internal) | CELESTE_KERNEL_METRICS, CELESTE_PHASES set by `bench-frame` itself | - |

**Knob count: 36 `CELESTE_*` variables read in code before; 0 after; 26
flags.** Gone entirely: CELESTE_ROOT (always "."), WALK_THREADS,
BUILD_THREADS, UNIT_LANES, MAX_TRACE_NODES (tuning without users: their
defaults stay), ASM_STATS (the kernel mix reports spills and reloads per
instruction with provenance), LATTICE_TRACE (now `--build-trace`), EDGE_DUMP
(now `ckhash --dump-edges F`), L1_CACHE (the cache dir is fixed;
`--l1-table` pins a table). Left: `CELESTE_FIXTURES` (read by the shell
tools only; the binaries accept it) and `CELESTE_REGENERATE` (develop's
widening-table TEST, which rewrites plans/abstractions.md; not a binary's).

Callers updated: check.sh, tools/fixtures.sh (a fixture's spec is its
flags: the six fixtures with settings were rebuilt and matched their pinned
fingerprints, the evidence that the flags give the trees the environment
gave), gates/raise.sh (`--extra` passes flags), tools/category_runner.py (a
job's `"flags"`; an old job's `"env"` is translated for the six knobs a job
used, anything else refused), pico8_diff/chain.py, plans/devloop.md, the
instructional parts of architecture.md, level-minus-one.md, results.md,
ui/README.md. Historical mentions (BENCHMARK_DATA.md, lessons, dated
sections, `tas/` headers) are left as they were run.

## Decisions to review

1. **A process-global settings struct, not explicit parameters
   everywhere.** The deep readers (the cart patches, the kernel build and
   walk, the storage geometry, `steps_per_frame`, the level -1 table)
   read the installed `Settings` through their existing accessors
   (`game_runner::start_room`, `frame::steps_per_frame`, `trim_rows`,
   `threads`, `region_grid`, `storage::geometry`,
   `abstraction::synthetic_win_rect`, `asm_kernel::key_check_on`, ...);
   ~120 call sites keep their signatures. Explicit `&Settings` only where
   one process needs two configurations: `cart::sources`,
   `RefEngine::with` (the concrete phase's seeded engines; the tests).
   It is set once from the flags, typed and validated - not the environment
   - but it is global state like the old level. Making it a parameter
   everywhere is possible and large (the kernel registry, the tracer, the
   storage); not done. The diagnostics (`--key-check` deep in
   `storage::unit`, `--build-trace` in the tracer and codegen, `--kernel-mix`
   in the assembler) are the awkward ones to plumb.
2. **Flags do NOT default to a tree's recorded settings on resume.** The
   same command line would then mean another search depending on what is
   on disk (rerun after deleting the tree: silently another category).
   A difference is an error naming every setting.
3. **A tree without `settings.txt` is refused** (resume, reuse,
   bench-frame, check-known, the diagnostics). Every tree built before this
   branch, including kept category trees. `rewrite adopt-settings
   --level-dir D --level L <flags>` writes the record for such a tree: it
   cannot be checked, so it is a deliberate act. Alternative: accept a tree
   without a record silently (what develop's `level.txt` did).
4. **develop's `level.txt` folded in**: the level is a line of
   `settings.txt`; `frame::check_tree_level` is gone and its callers check
   the whole record (`bounds-audit`, which has no `--level`, checks all but
   the level: `Settings::check_any_level`).
5. **The level -1 table cache is keyed on the settings record**
   (`Settings::cart_key`), not on every `CELESTE_*` variable but a list:
   every cached table is rebuilt once per room (12-60 s), then shared by
   runs that differ only in threads, diagnostics or H.
6. **KEY_CHECK keeps turning claims off.** It is essential to what it
   checks: with claims off every unit's copy of a new state reaches the
   translation, which compares the copies of one state across units - a
   key-collision check the per-row boundary check cannot make (that one
   proves each key is the key space's key of its row, not that the key
   space is injective). The claimed path itself is checked by the frame
   pins: t1's keycheck steps compare their f/e lines with the unchecked
   run's.
7. **The empty `--concrete-balloon-seeds=`** (a tasdatabase `[]`) is a
   value, as the empty env var was (every balloon 0). Note: the
   intermediate commit `Knobs: one declared list` refused an empty value;
   the flags commit after it accepts it again - do not deploy that commit
   alone.
8. `transpile --level-minus-one` keeps its default of 8 threads (the
   table build all cores), as before.
9. Deleted tools: tools/server.py + tree_viewer.html, framesetdiff.py,
   gdiff.py, gjoin.py, rowset.py, rowdiff.py, posgraph_pairs.py,
   key_soundness.py, key_unsound.py, shape_share.py, fuse_frontier.py,
   lua_tests/ (unreferenced), and tools/bench_step.sh (it copied storage-v1
   dirs no tree has; bench-frame replaced it; the auto-memory note
   `forward-bench.md` still describes it).

Candidates to cut further (not done, for Philippe): `--storage-region`
(only 8 has been used since storage v2?), `--walk-regions` (one test),
`--l1-path` (a probe printout), `--emit-capture-frame` (could be
`--emit-capture DIR,F`), `--min-free-gb` (a constant would do),
`--onlydiag` (no category run uses it yet?).

## CLAUDE.md: changes it needs (left for review)

- Every command example with `CELESTE_*`: `CELESTE_LEVEL_MINUS_ONE="C,5"`
  -> `--level-minus-one C,5`; `CELESTE_START_ROOM=X,Y transpile ...` ->
  `transpile --room X,Y ...`; `CELESTE_KERNEL_KEY_CHECK=1` -> `--key-check`;
  `CELESTE_THREADS`, `CELESTE_REGION`, `CELESTE_SPLIT_FRAME=1`,
  `CELESTE_TRIM_ROWS=1` -> `--threads`, `--region`, `--split-frame`,
  `--trim-rows`; "Other knobs" paragraph -> the flags table above.
- "Nothing on disk records a level's spec, so do not resume a tree under
  another `--level`": now recorded and refused (`settings.txt`).
- "A search RESUMES from its checkpoint directory: rerun the same
  command": still true, and a command with other settings is refused.
- The "Running safely" note on `MIMALLOC_PURGE_DELAY` is unchanged (not a
  `CELESTE_*` variable).

## Interface for the widening work

`Settings::write(dir, level)` / `Settings::check(dir, level)` take the
engine's `Level` value; the record's `level` line is `Level`'s `Display`.
A new tree setting is a field of `Settings`, a line of `recorded()`, and
a flag of `SettingsArgs`; bump `settings::VERSION` when a line changes
meaning (tools/fixtures.sh then rebuilds the fixtures).
