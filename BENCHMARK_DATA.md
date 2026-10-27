# Benchmark data: the reference numbers for today's code

Each entry is dated with the commit it was measured on. Re-measure before
relying on a number: the code moves. Older dated measurements described
code that is gone (the door, the precision ladder, the remainder-free
forward, ...); they are in git (`git show cde97096:BENCHMARK_DATA.md`), and
the design decisions they drove are in plans/lessons.md.

The machine: a 7950X3D (16 cores, 32 threads), 125 GB, DDR5 at 3600 MT/s.
It is usually shared with other agents' runs, so wall times move by up to
2x between runs; instruction counts, sizes and state counts do not. Quote
those, and a wall time only with the load it ran at. The check ladder's
own costs (each tier's steps, incremental builds, the kernel registry and
level -1 table at startup) are in plans/devloop.md.

## The pinned frames (exact)

The fixtures' frames (`check.sh` t1 and big pin their f/h/e/b lines,
`gates/fixtures/NAME.frame`). The counts were read from the kernel census
(`bench frame --metrics`), deleted 2026-10-27 with the hand-written
backend: the kernel instructions are that backend's (the next section has
the LLVM kernels'). "in" is the frame's expanded states.

| fixture | room, level | frame | in | emissions | new states | edges | edge bytes | kernel instructions | slices | pinned |
|---|---|---|---|---|---|---|---|---|---|---|
| r10 | (1,0) `r0sxh` | f55 -> f56 | 85,482 | 2,200,600 | 92,821 | 2,200,600 | 860 kB | 52.6M | 5,404 | `7ae5e5bf` 2026-10-13 |
| r62h | (6,2) 100% `r0sxhf`, level -1 (94,5) | f50 -> f51 | 305,439 | 6,625,220 | 349,275 | 6,625,220 | 2.11 MB | 115.8M | 19,277 | `b42ce58c` 2026-10-15 |
| r42n | (4,2) `r0sxhn`, level -1 (71,5) | f60 -> f61 | 47,523 | 891,489 | 49,300 | 891,475 | 342 kB | 45.8M | 3,010 | `cb2d72f9` 2026-10-16 |
| r60s | (6,0) `r0sxhfp`, split frame | step 70 -> 71 | 51,762 | 875,126 | 63,942 | 214,457 | 319 kB | 148.9M | 3,278 | `b42ce58c` 2026-10-15 |
| r62h57 (big) | (6,2) 100% `r0sxhf`, level -1 (94,5) | f56 -> f57 | 704,524 | 18,601,288 | 800,821 | 18,601,288 | 4.54 MB | 258.1M | 44,239 | `140b7028` 2026-10-16 |

The kernels are ~2-3% of a hot frame's work; the rest is emission, unit
ends, re-pushes and the region phase (`bench frame`'s `[cost]` lines).
The float twins (`f10`, `f42n`) are pinned by their f/h/e lines;
plans/float.md has their costs.

## The kernels' backend (2026-10-27, LLVM IR + `llc`)

The hand-written backend (2a639ea7) against the LLVM one (the branch
`llvm-backend`'s head), release binaries, the perf tier's method (`--reps
3` less `--reps 1`, over 2), 16 threads, back to back at load 10-20; every
frame's f/h/e lines the pinned ones. The kernels' share is a sampled `perf
record -e instructions:u` of the 3-rep run by DSO (the `k_*.so`, +-2%),
the kernel time `[cost] kernel_s`'s minimum (ns per input lane).

| frame | instructions a rep | kernels' instructions a rep | kernel ns / lane in |
|---|---|---|---|
| r62h f50 | 16.884 -> 16.876 G (-0.05%) | 0.178 -> 0.136 G (-24%) | 105.0 -> 87.0 |
| r62h57 f56 | 44.122 -> 44.101 G (-0.05%) | 0.394 -> 0.332 G (-16%) | 99.8 -> 81.6 |
| f42n f60 | 5.040 -> 4.964 G (-1.51%) | 0.260 -> 0.236 G (-9%) | 650.4 -> 441.0 |

The kernels are 0.6-3% of a frame's instructions; the rest of the binary
measured +0.2% (r62h, r62h57) against the base, the tier's run-to-run band.
`gates/perf.txt` re-pinned at the replacement: r62h 16.878 G, r62h57
44.091 G, f42n 4.964 G. Static instructions of a kernel set: r10 873k ->
470k, r60s 3.21M -> 1.71M.

A COLD kernel set (its cache entry removed), 16 workers, CPU seconds per
tool (the children's `wait4` usage, the emitting thread's clock):

| set | kernels, fused nodes | trace | hand-written: codegen + gcc | LLVM: emit + llc + as + ld | assemble wall |
|---|---|---|---|---|---|
| r10 | 102, 210k | 1.1 s | 0.35 + 1.71 = 2.1 CPU-s | 0.11 + 9.65 + 0.37 + 0.18 = 10.3 CPU-s | 0.21 -> 0.76 s |
| r62h | 306, 522k | 4.6 s | 0.88 + 4.84 = 5.7 CPU-s | 0.30 + 26.65 + 1.01 + 0.55 = 28.5 CPU-s | 0.56 -> 2.07 s |
| r60s | 918, 1.17M | ~14 s | 1.76 + 13.08 = 14.8 CPU-s | 0.65 + 49.0 + 2.20 + 1.63 = 53.5 CPU-s | 1.27 -> 3.78 s |

The largest kernels (2.7-3.8k fused nodes): 23-31 ms -> 126-238 ms.
`llc` is linear, ~45-50 us a fused node. plans/lessons.md has the flags
measured on the way.

## Whole searches (2026-10-13, the hull forward)

Release, 16 threads, `rewrite search` with `--prefer` the known route,
warm level -1 caches; the hull forward's build (`edd643c`), at load 20-45.
`perf stat -e instructions`. The same searches on the remainder-free
forward (develop `26de6cd`) in parentheses.

| search | wall | CPU | instructions | peak RSS | tree | forward | backward | states expanded (re-pushed) | optimum |
|---|---|---|---|---|---|---|---|---|---|
| (4,2) 2100m `--level r0sxhn --level-minus-one 71,5 --ceiling 71` | 3.5 s | 28 s | 0.23 T (0.40) | 0.87 GB | 20 MB | 2.9 s | 0.07 s | 0.6M (0.9M) (6.6M) | 71 |
| (6,2) 100% `--level r0sxhf,r0sxh --level-minus-one 94,5 --ceiling 94` | 62.9 s | 422 s | 2.91 T (9.75) | 3.36 GB | 0.84 GB | 48.8 + 10.9 s | 0.57 + 0.46 s | 30.6M (44.9M) (315.1M) | 93 |
| (1,0) 200m `--ceiling 99` | 80.3 s | 782 s | 5.86 T (6.25) | 4.30 GB | 1.39 GB | 79.7 s | 0.16 s | 65.8M (94.5M) (205.8M) | 99 |

Room (1,0) is the hull forward's worst case: its hulls end wide (72% of
the states full in x, 60% in y), so its frontier holds as many states as
the old forward's and the re-pushes add their edges. Its walls over four
pairs: 53.5, 48.5, 54.2, 80.3 s against 58.5, 44.4, 82.7, 45.3 s. The
visited set's hulls at f99: 0.58 GB (31M of 68M states stored, 10 B each).
Later commits (develop to `0765e7c5`) packed the visited set (~3 B a
state with its layer and hull), spooled the frame's out blocks and
segments to disk, and moved W's sets to scratch files: the peaks above
likely overstate today's.

## Category searches

The walls and peaks in plans/results.md's tables were measured on the code
of their dates, all before the hull forward (which expands far fewer
states). Typical then: an object room's search 2 min to ~1.5 h (room (3,0)
nodiag's level 0 alone 2.6 h), peaks 5-75 GB (rooms (3,2) nodiag and
(5,3) 100% the largest). On today's code: the dual search of nodiag 1800m (room (1,2),
`r0sxhnp,r0sxhn,r0sxh,r0sx`, h118, both halves and the pair search) took
133 s (2026-10-16, `53d6fab3`, `--memory 40G`).
