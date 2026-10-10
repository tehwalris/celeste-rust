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

This file replaces CLAUDE.md's "Gates" and "Iterating quickly" sections
once reviewed; until then both hold.

## The ladder

Times: quick profile, 16 threads, on the shared 32-thread machine with other
agents' searches running (load in brackets: 1-minute average at the start;
it moved between 10 and 47 during these runs, and the times with it).

| tier | command | runs | protects | measured |
|---|---|---|---|---|
| t0 | `./check.sh t0` | nextest, quick profile (173 tests) | the unit level: interpreter, tracer, lowering, codegen, storage, arcs | 29-33 s run + 5-15 s build (load 14-28) |
| t1 | `./check.sh t1` | one frame from each of three fixtures; KEY_CHECK on two; one backward frame; the storage replay; ref-check and arc-check samples | the forward's exact output and cost, the kernels' keys, the backward, the storage, the kernels and transfers against the reference | 28 s (load 10); 29-41 s (load 25-47) |
| t2 | `./check.sh t2` | the room (1,0) gates; an objects-ladder search at a synthetic win; a platform room's forward and one frame of it | end to end: forward, arcs, concrete search, known route, platforms | 36-55 s without the platform frame, 84 s with it (load 35) |
| big | `./check.sh big` | the reference frame (6,2) 100% f56 -> f57 and its 12 GB capture | the frame every kernel/storage measurement used | 11-14 s (fixtures: 2:43 once) |
| t3 | by hand, deliberately | see "t3" below | the rest of the old gates | minutes |

### t0: the unit tests

`./safe-run.sh -- ./one-cargo.sh cargo nextest run --cargo-profile quick`.
The wall is the slowest test's, and those are now all KERNEL BUILDS: each
builds a whole room's registry (walk + trace + assemble) to run a few rows,
and they build concurrently under nextest, so each takes 2-3x its solo time:

| test | solo | in the suite | what costs |
|---|---|---|---|
| `a_lane_outside_its_platform_bounds_declines` | 13.0 s | 29-32 s | room (2,1) `r0sxhnp` kernels: the walk 7.8 s, trace 11 s |
| `near_floors_side_by_side_split_only_what_collisions_read` | | 26-29 s | room (6,1) `r0sxhn` lattice walk |
| `room_13_exit_frame_keys_the_concrete_exit` | | 22-26 s | room (1,3) `r0sxh` registry for ONE row |
| `split_frame_reaches_the_unsplit_frontier_at_a_near_level` | | 20-23 s | room (2,1), two kernel sets |
| `room_02_spawn_frame_kernels_make_the_reference_successors` | | 19-21 s | room (0,2) registry |

Made faster here (solo, quick): `a_traced_frame_agrees_with_the_oracle` 24
-> 3.2 s, `ice_at_answers_the_tile_scan_in_every_room` 20 -> 3.3 s,
`pinning_the_fly_fruits_motion_keeps_the_projected_successors` 26.5 ->
7.4 s (their sweeps in parallel, one interpreter or engine per worker,
the assertions unchanged); `one_backward_gives_the_optimum` 8.1 -> 0.15 s
(a backward step spawned 16 workers for a 15-node graph, ~90k thread
spawns). Suite wall 26.6 s -> 29-32 s at a much higher load: the
kernel-building tests set it now. What would cut them: building only the
(shape, region) kernels a test touches (a lazy registry), or a cached
walk. Not done (the registry is built whole by design).

The two `#[ignore]`d tests (resume, level -1 balloon) are t3.

### t1: single frames from fixtures (< 1 min)

| step | command (abridged) | protects | time |
|---|---|---|---|
| frame-r10 | `bench-frame --level-dir F/r10 --frame 55 --edges --metrics` | room (1,0) r0sxh f55 -> f56: 451,316 new states, 14,537,665 edges; f/e lines and exact metrics | 1.5-3.3 s |
| frame-r62h | same, room (6,2) 100% r0sxhf f50 -> f51, level -1 | 2,597,457 states, 88,076,797 edges | 5.6-14.7 s (11.8 at load 47) |
| frame-r42n | same, room (4,2) r0sxhn f60 -> f61, level -1 | the object level's kernels: 553,666 states | 2.3-4.9 s |
| keycheck-r10 | `CELESTE_KERNEL_KEY_CHECK=1 bench-frame` r10 f55 | every emitted row's key recomputed from its fields | 1.6-3.1 s |
| keycheck-r42n | the same, r42n f60 | the object level's rows | 2.5 s |
| backward-r10arc | `bench-backward --horizon 56 --frame 45` | one arc backward step (W f45 of a synthetic-win h56 search; the gate's W line) | 0.4-0.7 s |
| storage-r10 | `bench-storage` of r10's f56 capture | the storage alone; its f/e lines must equal frame-r10's | 0.2-1.7 s |
| refcheck-r10 | `ref-check` 32 rows of r10 f55 | kernels against the reference engine, row by row | 2.7-3.5 s |
| refcheck-r42x | same, room (4,2) objects exact f55 | the same in an object room | 5.0-6.5 s |
| arccheck-r10 | `arc-check` 32 records of r10 f56, `--path-cap 4096` | the recorded transfers against the reference | 3.0-3.8 s |
| arccheck-r42x | same, (4,2) objects exact f56 | the same in an object room | 2.7-2.9 s |

Every frame step checks the frame twice over: `bench-frame` runs `--reps`
times and refuses a rep whose lines differ (determinism), and the lines
are `ckhash --edges`'s for the frame, so they equal the real tree's
(checked: room (1,0) f44 and f56, and the reference frame f57).

### t2: short end to end (< 2 min)

| step | protects | time |
|---|---|---|
| gate10-forward | `forward --to 44`, posgraph and ckhash against gates/ (CLAUDE.md's oracles 1-2) | 2.7-8.9 s |
| gate10-arc | `search --to 35 --win-at 9,101 --prefer` (oracle 3 and the known route) | 1.7-7.1 s |
| search42-objects | room (4,2) `r0sxhn,r0sxh` (the objects ladder) to f50 at a synthetic win (32,56): arc bounds 47/47, concrete optimum 47; its witness as the known route; every `[gate]` line pinned | 4.9-6.8 s |
| forward60-platforms | room (6,0) `r0sxhfp` split frame to step 71: `ckhash --edges` = the r60s fixture's pin | 26-44 s (the platform kernels' walk ~20 s) |
| frame-r60s | one platform frame (step 70 -> 71) and its exact metrics | 32 s (the same walk again) |

### big: the reference frame

Room (6,2) 100% (`CELESTE_HUNDRED=1 CELESTE_LOADING_JANK=2
CELESTE_LEVEL_MINUS_ONE=94,5`) r0sxhf f56 -> f57: 5,857,035 lanes in,
6,735,699 new states, 257,724,013 edges, 2,660,121,881 dynamic kernel
instructions over 366,186 slices (7,264 a slice: plans/kernel-opt-decisions.md
measured 2.660G and 7,264). frame-r62h57 8.2-10.5 s (kernels 4.1 s, the f56
tree 0.6 s, the wave 2.2 s, 37M-state visited set copied per rep);
storage-r62h57 3.0 s (`bench-storage` of the 12 GB capture: units 1.23 s,
translation 0.16 s, layer 0.11 s, edge file 0.11 s at 16 threads).

### t3: deliberate, by hand

Not part of any tier: run them when their reason applies.

- the two `#[ignore]`d tests (`--run-ignored all`) and the full arc-check of
  the gate tree, after touching the tracer, lowering, codegen, edge
  recording, resume or level -1;
- `gates/raise.sh` after touching the forward's filters, the notes or the
  edge files;
- 3 against 32 threads and a resume, identical (`ckhash --edges`), after
  touching the wave's scheduling;
- a full search with `--ceiling` and `--prefer` (the category runner) before
  a result is claimed.

## The fixtures

`tools/fixtures.sh ensure|build|pin|list NAME...` (`all`, `big`). Trees under
`/var/tmp/celeste-fixtures` (`CELESTE_FIXTURES`), never committed; each
built by a pinned command and checked against its pinned fingerprint
(`gates/fixtures/NAME.ckhash`: `ckhash --edges`, and `--dropped` under level
-1, through its last frame). A manifest records the spec, the file formats
and the sources. `ensure` rebuilds a fixture whose spec or file formats
changed (loudly: `STALE fixture ... REBUILDING`); one built from other
sources is reported, not rebuilt: a fixture is an INPUT pinned by its
fingerprint, and a change to the forward shows in the frame checks and in
t2's full-room gates. When a deliberate change moves a fixture's own
frames, `tools/fixtures.sh pin NAME` rebuilds it and re-pins.

Level -1 fixtures keep their table (`NAME/l1-table.bin`, `CELESTE_L1_TABLE`):
the table cache is keyed on the binary's bytes, so every rebuild of the
binary rebuilt the table (room (4,2) 12 s, (6,2) 37-61 s) before the first
frame. `bench-frame` refuses a table whose fingerprint is not the one the
tree recorded.

| fixture | what | frames | build (load ~10-40) | size |
|---|---|---|---|---|
| r10 | room (1,0) 200m `r0sxh` | f0-f56; frame 55 -> 56 | 14 s | 800 MB |
| r62h | room (6,2) 100% `r0sxhf`, level -1 (94,5) | f0-f51; frame 50 -> 51 | 62-74 s (the table ~60 s of it) | 2.8 GB |
| r42n | room (4,2) 2100m `r0sxhn`, level -1 (71,5) | f0-f61; frame 60 -> 61 | 18-28 s | 464 MB |
| r42x | room (4,2) `r0sxh` (objects exact), level -1 | f0-f56 | 9-16 s | 143 MB |
| r10arc | room (1,0) `search --level r0sxh --to 56 --win-at 40,64` | the level-0 tree to h56 | 15 s | 800 MB |
| cap-r10 | r10's f56 emissions (`CELESTE_EMIT_CAPTURE`) | f56 | 3 s | 518 MB |
| r60s | room (6,0) 700m `r0sxhfp`, split frame | steps 0-71; 70 -> 71 | 33 s | 187 MB |
| r62h57 (big) | room (6,2) 100%, as r62h | f0-f57; the reference frame 56 -> 57 | 151 s | 8.4 GB |
| cap-r62h57 (big) | its f57 capture | f57 | 12 s | 12 GB |

All of `all`: 2:20 at load 15-20 (most of it r62h's table). Using a fixture
is fast: frame F loads in 0.02-0.06 s (r10, r42n), 0.25 s (r62h f50, 12.6M
visited states), 0.62 s (r62h57 f56, 37M).

## The single-frame commands

- `rewrite bench-frame --level-dir D --frame F [--edges] [--reps N]
  [--threads T] [--metrics FILE]`: the forward as it stood after frame F
  (`ForwardState::at`: visited set, transfers, level -1 filter, pos graph;
  later frames ignored), the wave of F+1 run `--reps` times from copies.
  stdout: `f{F+1}` and `e{F+1}` lines (`ckhash --edges`'s); stderr per rep:
  the wave, translation and layer + edge file walls, the workers' phases
  (`[phases]`: pack, kernel, emit, unit end, translate, layer), `kernel
  lanes: traced N missed 0` (a miss is an error).
- `rewrite bench-backward --level-dir D --horizon H --frame F [--reps N]
  [--win-at X,Y] [--threads T]`: the rotation graph loaded (timed), W down
  to F+1, then the step to W_F `--reps` times from a copy: candidates, pull
  and merge walls, and the gate's `[gate] hH W fF count hash` line. (The
  remainder-free BFS is part of the load; its per-frame times are its
  `[bfs]` lines.)
- `rewrite bench-storage --capture DIR/fF --tree D` (unchanged): the units,
  translation, layer and edge file of a captured frame, without kernels.
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
`CELESTE_KERNEL_MIX=DIR`. The kernel build is in the profile too; use
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
- `frame.*`: lanes in, emissions, states kept, units, requests, lids, edges;
- `approx.frame.edge_bytes`, `approx.frame.edge_millibytes_per_edge`: NOT
  exact (a unit's block is encoded during the wave with its worker's local
  transfer ids, whose varint lengths depend on which units it ran first:
  28 bytes in 41.8 MB between two runs); compared within 0.5%.

Exact at a fixed thread count (the units, so the slices and calls, follow
the workers); `check.sh` uses 16. Checked: identical run to run, and at 8
against 16 threads for room (1,0) f44 (whose units are the 1024-lane
minimum either way). The census costs the build ~0.6 s for room (6,0)'s
918 kernels (2.1 against 1.5 s of assembly).

## Costs measured

Incremental quick builds after `touch`ing one file (binaries, then the test
binaries on top; load 12-38, so the spread is load as much as the file):

| touched | `cargo build --profile quick --bins` | + `nextest --no-run` |
|---|---|---|
| src/bin/rewrite.rs | 38 s (load 38) | 18 s |
| src/frame.rs | 24 s | 12 s |
| src/storage/wave.rs | 20 s | 16 s |
| src/trace/interp.rs | 19 s | 13 s |
| src/transpile/asm/codegen.rs | 14 s | 11 s |
| crates/celeste-engine/src/runtime2.rs | 15 s | 12 s |
| crates/celeste-core/src/pico8_num.rs | 15 s (load 12) | 13 s |

Kernel registry at startup (every process; walk + trace + assemble):
room (1,0) r0sx/r0sxh 0.9-1.4 s (102 kernels), (4,2) r0sxhn 1.8-1.9 s
(103), (6,2) r0sxhf 3.7-4.7 s (306), (6,0) r0sxhfp split 24-32 s (918; the
walk 20-26 s). The level -1 table without a cached file: (4,2) 12 s, (6,2)
37-61 s.

## What changed against the old checks, and why

| old | now | why |
|---|---|---|
| the three room (1,0) oracles (CLAUDE.md "Gates") | t2, unchanged | cheap: 2.7 + 2.1 s |
| `CELESTE_KERNEL_KEY_CHECK=1` on a forward f0-f44 | one frame, r10 f55 (451k states) | the same check per emitted row, on a bigger frame, in 2-3 s |
| `ref-check` 8 samples x 30 rows, two rooms, single-threaded | 32 rows each of r10 f55 and r42x f55, in parallel, exit code on a soundness gap | was ~0.5 s a row on one thread; now 3-6 s a room |
| `arc-check` of the gate tree, every frame x 20 samples | 32 records of one frame each of r10 and r42x, in parallel | sampled, one frame: t3 keeps the whole-tree check |
| the object-level arc-check (r0sxhn, hours, never finished) | NOT covered at r0sxhn: t1 checks the object room at r0sxh (objects exact) | the reference engine forks the n level's widened objects into thousands of paths a step (below) |
| `tools/bench_step.sh` (a hard-linked copy, `forward --to F+1`) | `bench-frame` from `ForwardState::at` | no copy, no checkpoint write, repeatable, exact lines |
| the reference frame `/var/tmp/canon-h62-f56` (hand-made) | the r62h57 fixture, `./check.sh big` | reproducible, pinned |
| a synthetic-win search in an object room | t2 `search42-objects` | new |
| the platform rooms (verified by hand: (6,0) split to step 75) | t2 forward + frame + metrics | new, pinned |
| `ckhash --edges`: frames one at a time | in parallel (room (1,0) to f56: >15 s -> 4 s) | every fixture build and gate uses it |

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
- **Edge bytes are not exact** (the per-worker transfer ids, above).

## Re-pinning

After a change that is meant to move outputs, on evidence that the sets did
not move (or with the reason they did): `tools/fixtures.sh pin NAME...`
(a fixture's own frames), `./check.sh t1 --pin`, `./check.sh t2 --pin`,
`./check.sh big --pin`; `git diff gates/fixtures` shows what moved; the
commit says why.
