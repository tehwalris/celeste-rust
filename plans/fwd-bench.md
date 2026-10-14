# The forward frame benchmark (2026-10-14, branch `fwd-perf2`)

Philippe: "make a good benchmark for the other 98% and optimize". The
kernels are ~2-3% of a hot forward frame; this benchmark times the rest -
emission, unit ends, re-pushes, the region phase, the frame end, the
checkpoint - on real data through the real code, cheaply enough to rerun
after every change, and checks that the outputs stay byte-identical.

## What it runs

`rewrite bench-frame --edges --checkpoint` (the frame F -> F+1 of a
fixture tree, `ForwardState::at`, as `extend` runs it):

1. the WAVE (`storage::wave::run_wave`): units and region jobs, the frame's
   edge and out files into a scratch dir under the tree (`bench-edges/`);
2. the FRAME END inside it (keys, seal, filter, absorb, layer, wins, edge
   file, out file: `frame::END_STEPS`);
3. with `--checkpoint`, the CHECKPOINT as `extend` makes it
   (`ForwardState::bench_checkpoint`): the frame file and the hull file,
   each read back and checked, then the pos graph (`bench-edges/ckpt/`).

`--reps N` runs it N times from copies of the visited set and the transfer
table; the LAST rep runs on the state's own (no copy beside it: a big
frame's peak less one visited set). Every rep's lines must agree.

### The output lines (stdout; the pins)

- `f{F+1} n hash`: the new states (`ckhash`'s `f` line);
- `h{F+1} n hash`: the next frontier with its layers and hulls (`ckhash --hulls`);
- `e{F+1} n hash`: the frame's edges by content (`ckhash --edges`);
- `b{F+1} files bytes hash` (`--checkpoint`): the BYTES of every file the
  frame wrote (edge file, out file, frame file, hull file, pos graph) -
  so a change to how files are encoded or compressed proves them identical.

The `f`/`h`/`e` lines are the real tree's: the big fixture's f231 lines
equal those another binary (an older format-16 build) printed for
the same frame of a different tree of the same search.

### The cost report (stderr, `[cost]` lines; `storage::bench::print_cost`)

Min and max over the reps, with the load average at each rep's start:

- the frame's WALL split into the wave, the frame end and the checkpoint;
- the wave's WORKER-SECONDS by phase (`frame::phases`, `--phases`, which
  `bench-frame` always sets) with its share of the budget (workers x wave
  wall): unit decode, slice setup, pack, kernel, emit, the rest of
  `engine.run`, unit end, the units' out blocks (zstd), re-push units,
  region jobs, the slice gather, busy untimed, IDLE;
- the frame end's and the checkpoint's steps (walls);
- each with its cost PER ELEMENT: ns a lane in (decode, pack, kernel), an
  emission (emit, unit end), an edge (out blocks, files), a re-pushed edge,
  a push (region jobs), a touched / new / frontier state (the frame end,
  the checkpoint);
- the bytes written (edge, out, frame, hull files) and the anonymous RSS
  at the wave's start, after it, at the end, and the peak; `--phases`'
  `[mem]` and `[wavemem]` lines in the log break the memory down by phase.

## How to run it

```bash
# Build a fixture (a forward to F into $FWD_BENCH/NAME; FLAGS are the tree's settings and --level):
tools/fwd_bench.sh make NAME F -- --room X,Y --level LEVEL [--level-minus-one H,S --l1-table T] ...
# Run F -> F+1 REPS times; the first run pins NAME/frame.pin, a later one that differs FAILS:
BIN=target/release/rewrite tools/fwd_bench.sh run NAME [REPS]
tools/fwd_bench.sh list
```

`FWD_BENCH` defaults to /var/tmp/fwd-bench, `BIN` to the release binary
(benchmark numbers are release numbers; the checks themselves run with
any profile), `THREADS` 16, `MEMORY` 15G (safe-run's cap). A fixture dir
holds the tree, `fixture.txt` (frame, flags, how it was built), the pin
and every run's full log (`runs/`). A tree serves any frame up to its last
(`ForwardState::at` ignores later frames): a hard-linked copy with another
`frame` line in `fixture.txt` is a second fixture for free.

**Wall times need a quiet machine.** The report gives the load; on a
shared machine repeat and take the min, and prefer worker-seconds and the
costs per element, which move less. Never quote a speedup from one noisy
pair.

## The fixtures (2026-10-14)

The workload that matters is a private object room (its cart and every route or result stay out of
the repo; the fixtures are built locally) in its hot
frames: unseeded, one dash, `r0sxhe`, the hull forward. Its forward grows
~4-7% a frame in states.

| fixture | frame | in (new + old) | emissions | re-pushed edges | new + grown out | edges | 1 rep | peak |
|---|---|---|---|---|---|---|---|---|
| `mu220` (inner loop) | f220 -> f221 | 3.89M + 2.02M | 119.5M | 71.0M | 4.16M + 2.19M | 118.5M | ~5-7 s | 9.6 GB (3 reps) |
| `mu230` (big) | f230 -> f231 | 7.48M + 4.42M | 255.1M | 160.6M | 7.97M + 4.76M | 253.0M | ~10-11 s | 10.5 GB (1 rep) |

Both cut from one tree: `forward --to 230 --room R --cart CART --level
r0sxhe --threads 16` (no level -1), 142 s, 3.9 GB on disk. A run is
~30 s of overhead (kernel build ~10 s, the tree loaded ~6-11 s) plus the
reps. Under the coordinator's 15 GB cap `mu230` runs one rep a process
(two reps, with the first one's copy of the visited set, would peak
~14 GB): repeat the run for a minimum.

The develop fixtures (`tools/fixtures.sh`; `r62h57` is the reference frame
(6,2) 100% f56 -> f57) run the same way: `tools/fwd_bench.sh make` with
their flags, or `bench-frame --checkpoint` on them directly.

## Baseline (develop f652026, release, 16 threads)

`mu230`, one rep a run, two runs at load 17-19 (the machine shared with a
16-thread search; at load 42-65 the same frame took 10.5-16.7 s - walls
mean little there, the instructions did not move):

| | run 1 | run 2 | per element (run 1) |
|---|---|---|---|
| frame | 6.10 s | 6.46 s | 766 ns a new state |
| wave | 5.32 s | 5.65 s | 21 ns an emission |
| frame end | 0.70 s | 0.73 s | 88 ns a new state |
| checkpoint | 0.09 s | | 7 ns a frontier state |
| user instructions | 589.1 G | 589.1 G | 2309 an emission |
| user cycles (IPC) | 354.5 G (1.66) | 375.6 G (1.57) | |
| kernel-mode cycles | 10.3 G (3%) | 10.1 G | |

| wave worker-seconds (run 1; budget 85.1) | s | share | per element |
|---|---|---|---|
| unit decode | 1.25 | 1.5% | 167 ns a lane in |
| slice setup + pack + kernel | 3.68 | 4.3% | 493 ns a lane in |
| emit | 35.39 | 41.6% | 139 ns an emission |
| unit end | 13.63 | 16.0% | 53 ns an emission |
| unit out blocks (zstd) | 2.22 | 2.6% | 8.8 ns an edge |
| re-push units | 11.80 | 13.9% | 74 ns a re-pushed edge |
| region jobs | 13.79 | 16.2% | 528 ns a push |
| slice gather | 0.85 | 1.0% | |
| idle | 2.36 | 2.8% | |

| frame end, wall | s | | checkpoint, wall | s |
|---|---|---|---|---|
| seal | 0.242 | | frame file + check | 0.026 |
| absorb | 0.189 | | hull file + check | 0.052 |
| layer | 0.077 | | pos graph | 0.007 |
| wins | 0.056 | | | |
| edge + out files | 0.097 | | | |
| keys, rest | 0.039 | | | |

Files: edge 80 MB, out 161 MB, frame 5 MB, hull 36 MB. Anonymous RSS
5.0 GB at the start, 7.9 GB after the wave, 6.1 GB at the end, peak
11.0 GB.

`mu220`, three reps, load 15-20: frame 4.37-6.56 s (wave 3.78-5.09, end
0.52-1.39, checkpoint 0.07-0.17).
