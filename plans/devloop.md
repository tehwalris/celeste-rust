# The check ladder: what to run, from cheapest (2026-10-10)

Philippe's requirements: nothing on the scale of hours, nothing over ~2
minutes (and strongly avoid even that); the unit tests roughly as they
are; full-room searches only for very short rooms or synthetic wins; and
the missing kind, SINGLE-FRAME checks from stored fixtures, for
performance work and profiling, checked exactly by fingerprints.

One entry point: `./check.sh t0|t1|t2|big|all` (builds the quick binary,
runs each step under `safe-run.sh`, prints `ok`/`FAIL` and the wall time
per step, exits non-zero on any failure). `t1 --pin`, `t2 --pin` and
`big --pin` write the outputs as the new pins instead (only on evidence;
say why in the commit). Timings are reported, never pinned.

CLAUDE.md's "Checking a change" points here (it replaced CLAUDE.md's
"Gates" and "Iterating quickly" sections, branch `settings-explicit`).

**Settings are flags** (2026-10-10, branch `footguns`): every setting of a
run is a command-line flag of the binaries (`--room`, `--hundred`,
`--level-minus-one H,S`, `--split-frame`, `--threads`, `--key-check`, ...;
`rewrite --help`), and a binary REFUSES any `CELESTE_*` variable in its
environment (naming the flag that replaced it). Only `CELESTE_FIXTURES`,
read by the shell tools, remains. A tree records the settings that change
it (`<level>/settings.txt`) and refuses a run under other ones
(`--override-settings` proceeds past a different `--region` only; a tree
without a record is refused). Nothing reads a global: the
binaries pass the settings (plans/footguns.md, "Follow-ups").

## The ladder

Times: quick profile, 16 threads, on the shared 32-thread machine with other
agents' searches running (load: the 1-minute average at the start; it moved
between 6 and 47 during these runs, and the times with it). Measured on
develop 79b2479 (exact keys, tree format 13) plus this branch, after the
rebase; earlier format-12 numbers where noted.

| tier | command | runs | protects | measured |
|---|---|---|---|---|
| t0 | `./check.sh t0` | nextest, quick profile (182 tests) | the unit level: interpreter, tracer, lowering, codegen, storage, arcs | 20 s run (load 6-14) + 12 s test build after an edit |
| t1 | `./check.sh t1` | one frame from each of three fixtures; KEY_CHECK on two; one backward frame; the storage replay; ref-check and arc-check samples | the forward's exact output and cost, the kernels' keys, the backward, the storage, the kernels and transfers against the reference | 32 s (load 26); 28-41 s over the day (load 10-47) |
| t2 | `./check.sh t2` | the room (1,0) gates; an objects-ladder search at a synthetic win; a platform room's forward and one frame of it | end to end: forward, arcs, concrete search, known route, platforms | 60 s (load 21), 50 s of it the two platform steps |
| big | `./check.sh big` | the reference frame (6,2) 100% f56 -> f57 and its 12 GB capture | the frame every kernel/storage measurement used | 17 s (fixtures: 2:38 once) |
| t3 | by hand, deliberately | see "t3" below | the rest of the old gates | minutes |

### t0: the unit tests

`./safe-run.sh -- ./one-cargo.sh cargo nextest run --cargo-profile quick`.
The wall is the slowest test's, and those are now KERNEL BUILDS: each builds
a whole room's registry (walk + trace + assemble) to run a few rows, and
they build concurrently under nextest, so each takes ~1.5-2.5x its solo time
(times in the suite at load 6-14):

| test | solo | in the suite | what costs |
|---|---|---|---|
| `a_lane_outside_its_platform_bounds_declines` | 13.0 s | 19.8 s | room (2,1) `r0sxhnp` kernels: the walk 7.8 s, trace 11 s |
| `near_floors_side_by_side_split_only_what_collisions_read` | | 17.9 s | room (6,1) `r0sxhn` lattice walk |
| `room_13_exit_frame_keys_the_concrete_exit` | | 16.8 s | room (1,3) `r0sxh` registry for ONE row |
| `split_frame_reaches_the_unsplit_frontier_at_a_near_level` | | 15.6 s | room (2,1), two kernel sets |
| `room_02_spawn_frame_kernels_make_the_reference_successors` | | 14.5 s | room (0,2) registry |
| `pinning_the_fly_fruits_motion_keeps_the_projected_successors` | 7.4 s | 14.1 s | the reference's unpinned steps along the route |

Made faster here (solo, quick): `a_traced_frame_agrees_with_the_oracle` 24
-> 3.2 s, `ice_at_answers_the_tile_scan_in_every_room` 20 -> 3.3 s,
`pinning_the_fly_fruits_motion_keeps_the_projected_successors` 26.5 ->
7.4 s (their sweeps in parallel, one interpreter or engine per worker,
the assertions unchanged); `one_backward_gives_the_optimum` 8.1 -> 0.15 s
(a backward step spawned 16 workers for a 15-node graph, ~90k thread
spawns). Suite wall: 26.6 s for 173 tests at the start (load 3-8) -> 20 s
for 182 tests after the rebase (load 6-14); the kernel-building tests set it
now. What would cut them: building only the
(shape, region) kernels a test touches (a lazy registry), or a cached
walk. Not done (the registry is built whole by design).

The two `#[ignore]`d tests (resume, level -1 balloon) are t3.

### t1: single frames from fixtures (< 1 min)

| step | command (abridged) | protects | time |
|---|---|---|---|
| frame-r10 | `bench-frame --level-dir F/r10 --frame 55 --edges --metrics` | room (1,0) r0sxh f55 -> f56: 451,316 new states, 14,537,665 edges; f/e lines and exact metrics | 1.6 s |
| frame-r62h | same, room (6,2) 100% r0sxhf f50 -> f51, level -1 | 2,597,457 states, 88,076,797 edges | 6.7 s (kernels 4 s, the 88M-edge fingerprint, the wave 0.7 s) |
| frame-r42n | same, room (4,2) r0sxhn f60 -> f61, level -1 | the object level's kernels: 553,666 states | 2.5 s |
| keycheck-r10 | `bench-frame --key-check` r10 f55 | every emitted row's key recomputed from its fields | 2.7 s |
| keycheck-r42n | the same, r42n f60 | the object level's rows | 4.4 s |
| backward-r10arc | `bench-backward --horizon 56 --frame 45` | one arc backward step (W f45 of a synthetic-win h56 search; the gate's W line) | 0.4 s |
| storage-r10 | `bench-storage` of r10's f56 capture | the storage alone; its f/e lines must equal frame-r10's | 0.4 s |
| widen-r10, -r62h, -r42n | `widen-check --emitted 64` of r10 f55, r62h f50, r42n f60 | the widening table's two sides agree at `h`, `hf`, `hn`: every stored row and the rows the kernels emit from 64 of them are fixed points of the block side's widening (`Rt2::widen_to`), exactly | 1.6 s, 7.1 s, 3.3 s |
| refcheck-r10 | `ref-check` 32 rows of r10 f55 | kernels against the reference engine, row by row | 2.5 s |
| refcheck-r42x | same, room (4,2) objects exact f55 | the same in an object room | 4.9 s |
| arccheck-r10 | `arc-check` 32 records of r10 f56, `--path-cap 4096` | the recorded transfers against the reference | 3.0 s |
| arccheck-r42x | same, (4,2) objects exact f56 | the same in an object room | 2.6 s |

(Times of the run at load 26; at load 10-47 over the day the steps moved by
up to 2x.)

Every frame step checks the frame twice over: `bench-frame` runs `--reps`
times and refuses a rep whose lines differ (determinism), and the lines
are `ckhash --edges --hulls`'s for the frame (`f`, `h`, `e`), so they equal
the real tree's (checked: room (1,0) f44 and f56, and the reference frame
f57). Under the hull forward (2026-10-13) the fixture frames are smaller
than they were: r10 f55 -> f56 has 85k new and 125k grown states in, 2.2M
emissions and 2.5M edges re-pushed (14.5M edges before); the steps' times
above are from before.

### t2: short end to end (< 2 min)

| step | protects | time |
|---|---|---|
| gate10-forward | `forward --to 44`, posgraph and `ckhash --hulls` against gates/ (CLAUDE.md's oracles 1-2) | 2.8 s |
| gate10-arc | `search --to 35 --win-at 9,101 --prefer` (oracle 3 and the known route) | 1.8 s |
| gate10f-forward, gate10f-arc | the same two in float (`--arith float`, the doubles' kernels; `gates/float_*`, plans/float-solver.md milestone 5) | 4.0 s, 3.2 s |
| search42-objects | room (4,2) `r0sxhn,r0sxh` (the objects ladder) to f50 at a synthetic win (32,56): arc bounds 47/47, concrete optimum 47; its witness as the known route; every `[gate]` line pinned | 5.0 s |
| export-ui | `rewrite export-ui` of gate10-arc's and search42's searches with their marks (`--save-marks`): every binary file and run.json without its timings, pinned by digest (`gates/fixtures/export-ui.sha256`; pinned 2026-10-12 from the in-memory export, which the streamed one reproduces byte for byte, as it does the mod room's balloon run) | 0.2 s |
| forward60-platforms | room (6,0) `r0sxhfp` split frame to step 71: `ckhash --edges --hulls` = the r60s fixture's pin | 25.5 s (the platform kernels' walk ~20 s) |
| frame-r60s | one platform frame (step 70 -> 71) and its exact metrics: 49,469 kernel instructions a slice | 25.0 s (the same walk again) |
| widen-r60s | `widen-check` of r60s steps 70 and 71 (stored rows): the widening table's two sides at `hfp`, at a boundary and mid-frame | ~2 s |

### big: the reference frame

Under the hull forward (2026-10-13): 704,524 new and 1,206,211 grown states
in, 18,601,288 emissions and 27,056,166 edges re-pushed, 800,821 new
states; 258,065,198 dynamic kernel instructions over 44,239 slices;
frame-r62h57 5.5 s (the wave 0.28 s), storage-r62h57 1.9 s. Before
(the remainder-free forward), r0sxhf f56 -> f57: 5,857,035 lanes in,
6,735,699 new states, 257,724,013 edges, 2,660,121,881 dynamic kernel
instructions over 366,186 slices (7,264 a slice: plans/kernel-opt-decisions.md
measured 2.660G and 7,264). frame-r62h57 8.2-11.1 s (kernels 4.1 s, the f56
tree 0.6 s, the wave 2.2 s, 37M-state visited set copied per rep);
storage-r62h57 3.0-5.8 s (`bench-storage` of the 12 GB capture: units and
region jobs 2.1 s, frame end 0.25 s at 16 threads, edge file v6;
2026-10-11).

### t3: deliberate, by hand

Not part of any tier: run them when their reason applies.

- the two `#[ignore]`d tests (`--run-ignored all`) and the full arc-check of
  the gate tree, after touching the tracer, lowering, codegen, edge
  recording, resume or level -1;
- 3 against 32 threads and a resume, identical (every frame, hull, edge
  and out file, byte for byte), after touching the wave's scheduling, the
  hulls, the out store or resume;
- a full search with `--ceiling` and `--prefer` (the category runner) before
  a result is claimed.

## The fixtures

`tools/fixtures.sh ensure|build|pin|list NAME...` (`all`, `big`). Trees under
`/var/tmp/celeste-fixtures` (`CELESTE_FIXTURES`), never committed; each
built by a pinned command and checked against its pinned fingerprint
(`gates/fixtures/NAME.ckhash`: `ckhash --edges --hulls` through its last
frame). A manifest records the spec, the file formats,
the transfer table's order and the sources. `ensure` rebuilds a fixture
whose spec, file formats or table order changed (loudly: `STALE fixture ...
REBUILDING`); one built from other sources is reported, not rebuilt: a
fixture is an INPUT pinned by its fingerprint, and a change to the
forward shows in the frame checks and in t2's full-room gates. A frame
check encodes the fixture's transfers by their ids, so its edge bytes
follow the fixture's `xfer.bin`; since branch `emit-batch` (2026-10-12)
the ids are CANONICAL (a frame's new transfers numbered by their edges
in the frame, then content: `edges::XferTable::canonical`), so the table
is a function of the search, not of the slices or units that met the
transfers first (before, a fixture cut by another slice size gave a frame
check other edge bytes, -0.04 to -1.3%). When a deliberate change moves a
fixture's own frames, `tools/fixtures.sh pin NAME` rebuilds it and re-pins.

`bench-frame` (`ForwardState::at`) loads the tree's WHOLE transfer table,
so a frame check meets no new transfer: the frame's end's canonical
numbering is checked by full forwards (t2's, the fixtures' builds), not
by the single-frame checks.

Level -1 fixtures keep their table (`NAME/l1-table.bin`, `--l1-table`):
the table cache is keyed on the binary's bytes, so every rebuild of the
binary rebuilt the table (room (4,2) 12 s, (6,2) 37-61 s) before the first
frame. `bench-frame` refuses a table whose fingerprint is not the one the
tree recorded.

| fixture | what | frames | build (load 5-20) | size (format 15; 14) |
|---|---|---|---|---|
| r10 | room (1,0) 200m `r0sxh` | f0-f56; frame 55 -> 56 | 3-14 s | 22 MB (61) |
| r62h | room (6,2) 100% `r0sxhf`, level -1 (94,5) | f0-f51; frame 50 -> 51 | 48-76 s (the table ~40-60 s of it) | 56 MB (280) |
| r42n | room (4,2) 2100m `r0sxhn`, level -1 (71,5) | f0-f61; frame 60 -> 61 | 16-28 s | 15 MB (43) |
| r42x | room (4,2) `r0sxh` (objects exact), level -1 | f0-f56 | 9-16 s | 7 MB (17) |
| r10arc | room (1,0) `search --level r0sxh --to 56 --win-at 40,64` | the level-0 tree to h56 | 4-15 s | 22 MB (61) |
| cap-r10 | r10's f56 emissions (`--emit-capture`) | f56 | 1-3 s | 515 MB |
| r60s | room (6,0) 700m `r0sxhfp`, split frame | steps 0-71; 70 -> 71 | 26-33 s | 8 MB (25) |
| r62h57 (big) | room (6,2) 100%, as r62h | f0-f57; the reference frame 56 -> 57 | 60-151 s | 181 MB (0.9 GB) |
| cap-r62h57 (big) | its f57 capture | f57 | 12-14 s | 12 GB |

All of `all`: 2:37 at load 5-15 (most of it r62h's table); `big` 2:38. Using a fixture
is fast: frame F loads in 0.02-0.06 s (r10, r42n), 0.25 s (r62h f50, 12.6M
visited states), 0.20 s (r62h57 f56, 37M; 0.62 s at format 14).

## The single-frame commands

- `rewrite bench-frame --level-dir D --frame F [--edges] [--reps N]
  [--threads T] [--metrics FILE]`: the forward as it stood after frame F
  (`ForwardState::at`: visited set, transfers, level -1 filter, pos graph;
  later frames ignored), the wave of F+1 run `--reps` times from copies.
  stdout: `f{F+1}`, `h{F+1}` and `e{F+1}` lines (`ckhash --edges
  --hulls`'s); stderr per rep:
  the wave (with the region jobs' worker time inside it) and the frame
  end's walls, the workers' phases (`[phases]`: pack, kernel, emit, unit
  end, regions, frame end), `kernel lanes: traced N missed 0` (a miss is
  an error).
- `rewrite bench-backward --level-dir D --horizon H --frame F [--reps N]
  [--win-at X,Y] [--threads T]`: the edge files opened (timed), W pushed
  down to F+1, then the step to W_F `--reps` times from a copy: read, pull
  and merge walls, and the gate's `[gate] hH W fF count hash` line. (Each
  step of the push prints its `[arc] fNNN` line.)
- `rewrite bench-storage --capture DIR/fF --tree D`: the units, region
  jobs, frame end and edge file of a captured frame, without kernels.
  Small capture: cap-r10 (518 MB); big: cap-r62h57 (12 GB).
- `rewrite ref-check` and `rewrite arc-check`: now `--threads` (a reference
  engine per worker), `--path-cap N` (a sample whose reference step forks
  more than N paths is SKIPPED and counted), `ref-check --room` and a
  non-zero exit on a soundness gap.

Profiling: `perf stat -e instructions,cycles -- rewrite bench-frame ...
--reps 5` and `perf record -g -m 8 -- ...` both run cleanly (`-m 8`: the
mlock budget for perf's buffers is per user, and another agent's perf can
hold it: "Permission error mapping pages"). The kernels are their own
`.so`s in `$TMPDIR/asm-scratch/PID/`, removed by the next process that
starts: run `perf report` before the next run, or keep them with
`--kernel-mix DIR`. The kernel build is in the profile too; use
`--reps` or `perf record -D MS` to weight the wave.

## The metrics check

`bench-frame --metrics FILE` writes one `key value` line per EXACT metric of
the first rep; `tools/metrics_diff.py PINNED NEW` prints every moved key with
its percent change, the kernels that appeared, disappeared or changed, and
exits non-zero; `./check.sh t1 --pin` re-pins. Keys:

- `all.*`: every built kernel's census summed: `traced.nodes` and
  `traced.kind.*` (the traced frame's graph), `fused_nodes`, `nodes` and
  `kind.*` (the fused graph, reachable nodes) with kinds `leaf num cmp bool
  sel restrict call`; `bodies`, `bodies.distinct_error`/`_live` (guard
  roots), `error_terms`, `live_conjuncts`, `roots`, `rootrole.*`; `insts`,
  `ssa`, `spill_slots`, and `cat.*` the static instructions by category
  (`compiled::mix`: `a.*` numeric, `b.*` boolean/mask, `c.*` movement and
  call-outs, `d.*` stores);
- `dyn.*`: the frame's dynamic kernel instructions (slices x static count,
  exact: a kernel is straight-line code; call-outs' callees aside), in all
  and by category, and `dyn.slices`;
- `k.<sym>.*`: per kernel that ran: traced and fused nodes, instructions,
  spill slots, slices, dynamic instructions;
- `calls.*`: kernel calls, rows, slice-lanes, (body, slice) evaluations and
  those that took a lane, lane emissions and those after the dedup cache,
  lanes missed;
- `frame.*`: lanes in, emissions, states kept, units, slices, routes,
  edges, bundles, the edge file's bytes and millibytes an edge (exact: a
  segment is a function of its (route, slice)'s units, the transfer ids
  global and canonical).

The same at every thread count since the edges target (2026-10-11: a
unit is a 256th of the frame, not of the workers): `check.sh t1` at 8 and
at 16 threads gives the same metrics, f/e lines and edge files. The census costs the build ~0.6 s for room (6,0)'s
918 kernels (2.1 against 1.5 s of assembly).

## Costs measured

Incremental quick builds after `touch`ing one file (the binaries, then the
test binaries on top), at load 6-10 (at load 12-38 the same took 14-38 s):

| touched | `cargo build --profile quick --bins` | + `nextest --no-run` |
|---|---|---|
| src/bin/rewrite.rs | 4.2 s | 0.8 s |
| src/frame.rs | 14.2 s | 11.5 s |
| src/storage/wave.rs | 14.2 s | 11.6 s |
| src/trace/interp.rs | 14.4 s | 11.6 s |
| src/transpile/asm/codegen.rs | 14.4 s | 11.6 s |
| crates/celeste-engine/src/runtime2.rs | 14.7 s | 11.7 s |
| crates/celeste-core/src/pico8_num.rs | 14.7 s | 11.7 s |

Any library edit costs ~14 s of binary and ~12 s of test build: the
`celeste-rust` crate is one codegen unit set, whichever file moved.

Kernel registry at startup (every process; walk + trace + assemble):
room (1,0) r0sx/r0sxh 0.9-1.4 s (102 kernels), (4,2) r0sxhn 1.8-1.9 s
(103), (6,2) r0sxhf 3.7-4.7 s (306), (6,0) r0sxhfp split 24-32 s (918; the
walk 20-26 s). The level -1 table without a cached file: (4,2) 12 s, (6,2)
37-61 s.

## What changed against the old checks, and why

| old | now | why |
|---|---|---|
| the three room (1,0) oracles (CLAUDE.md "Gates") | t2, unchanged | cheap: 2.7 + 2.1 s |
| `--key-check` (was `CELESTE_KERNEL_KEY_CHECK=1`) on a forward f0-f44 | one frame, r10 f55 (451k states) | the same check per emitted row, on a bigger frame, in 2-3 s |
| `ref-check` 8 samples x 30 rows, two rooms, single-threaded | 32 rows each of r10 f55 and r42x f55, in parallel, exit code on a soundness gap | was ~0.5 s a row on one thread; now 3-6 s a room |
| `arc-check` of the gate tree, every frame x 20 samples | 32 records of one frame each of r10 and r42x, in parallel | sampled, one frame: t3 keeps the whole-tree check |
| the object-level arc-check (r0sxhn, hours, never finished) | NOT covered at r0sxhn: t1 checks the object room at r0sxh (objects exact) | the reference engine forks the n level's widened objects into thousands of paths a step (below) |
| `tools/bench_step.sh` (a hard-linked copy, `forward --to F+1`) | `bench-frame` from `ForwardState::at` | no copy, no checkpoint write, repeatable, exact lines |
| the reference frame `/var/tmp/canon-h62-f56` (hand-made) | the r62h57 fixture, `./check.sh big` | reproducible, pinned |
| a synthetic-win search in an object room | t2 `search42-objects` | new |
| the platform rooms (verified by hand: (6,0) split to step 75) | t2 forward + frame + metrics | new, pinned |
| `ckhash --edges`: frames one at a time | in parallel (room (1,0) to f56: >15 s -> 4 s); since 2026-10-11 streamed (frames in order, each frame's units on every thread, each entry's key hashed once) | every fixture build and gate uses it. The parallel frames each materialized their edges: r62h57 peaked at 38 GB (70 GB unwrapped in the 11:38 OOM); streamed 4.9 GB, 6 s |

## What remains over budget, and gaps

- **The n level against the reference.** `ref-check` and `arc-check` at
  `r0sxhn` are unbounded in practice: the reference engine enumerates every
  fork path by re-running the frame (`refdriver::run_frame_all`), and the n
  level's widened objects (fall-floor states, the countdowns, the balloons'
  phases) make thousands of paths a row: room (4,2) f50, 15 of 16 rows over
  2000 paths; 300k paths ran >10 min for one sample; 16 workers at 20k paths
  reached the 40 GB cap (a path's state is ~100 KB). `--path-cap` makes it
  bounded but skips everything. The n kernels' successors are therefore
  checked only end to end (t2's objects-ladder search, whose exact level and
  concrete search refute what n over-approximates) and by KEY_CHECK on
  their rows (keycheck-r42n). Closing it needs a reference that keeps far objects widened
  (as the kernels do) instead of enumerating them.
- **t0's wall** is the kernel-building tests' (above): 29-32 s at load
  15-30.
- **The platform kernels' walk** (~20-30 s a process for room (6,0)): t2's
  two platform steps are most of t2's time.
- **The level -1 table cache is keyed on the binary**: a search in an
  object or 100% room after any rebuild pays 12-61 s for the table. The
  fixtures pin their tables; the searches still pay. Keying it on the
  sources instead (a build-time hash) would be sound only if the build is
  deterministic in them; not changed.
- **A frame check meets no new transfer** (`ForwardState::at` loads the
  whole table, above): recording each frame's table length would let it
  start from the frame's own.
- **The fixtures follow the format**: a format change (exact keys: 12 ->
  13) rebuilds every fixture (`ensure` does it) and moves every hash pin
  (`pin all`, `pin big`, then `t1/t2/big --pin`, ~8 min); the counts are the
  evidence (done for 79b2479: every state, edge and drop count identical).

## Re-pinning

After a change that is meant to move outputs, on evidence that the sets did
not move (or with the reason they did): `tools/fixtures.sh pin NAME...`
(a fixture's own frames), `./check.sh t1 --pin`, `./check.sh t2 --pin`,
`./check.sh big --pin`; `git diff gates/fixtures` shows what moved; the
commit says why.
