# Tiling the rewritten frame: what the measurement says to tile

Follows from plans/roofline.md. The plan below the first section is what
was actually measured, and it retired most of the roadmap this file
originally carried - block-level tiling is worth almost nothing, and the
cache cliff is somewhere else entirely.

## Prerequisite, prototyped and measured first

The search runner executed the UNREWRITTEN program; the rewritten one
(differentially verified identical through frame 40) ran only in `rewrite
bench`. A `--rewritten` flag now runs the recipe program through the same
search loop (`AbstractRun`), pre-parallel, single-threaded:

| frame | unrewritten | rewritten | speedup |
|---|---|---|---|
| 34 | 2.12 s | 0.40 s | 5.3x |
| 37 | 6.77 s | 1.67 s | 4.1x |
| 39 | 15.4 s | 4.54 s | 3.4x |

Identical lane counts (613,865 at 39). Cumulative to 39: ~46 s -> ~12.4 s.

## The scaling measurement that decides what to tile

`op_census::reset()` plus a per-frame report in the `--rewritten` runner
gives each op's ns/elem *at a known lane count*, which cumulative totals
hide. Lane counts grow ~1.6x/frame, so frames 29-39 sweep the working set
across an 85x range - from wholly L1-resident to far past L3.

ns/elem, rewritten program, single-threaded:

| frame | lanes | KB/col | select | binop | concat | filter | hash_rows | dedup bucket |
|---|---|---|---|---|---|---|---|---|
| 29 | 7,260 | 28 | 0.54 | 0.27 | 0.31 | 0.78 | 0.19 | - |
| 32 | 44,566 | 174 | 0.59 | 0.25 | 0.26 | 0.97 | 0.17 | 43.7 |
| 35 | 132,153 | 516 | 0.56 | 0.26 | 0.27 | 0.94 | 0.32 | 42.6 |
| 37 | 269,059 | 1,051 | 0.58 | 0.29 | 0.30 | 1.16 | 0.48 | 56.8 |
| 39 | 613,865 | 2,398 | 0.65 | 0.35 | 0.41 | 1.23 | 0.51 | 65.7 |

**Block-level streaming ops are not cache-bound.** `select` costs the same
0.54-0.65 ns/elem whether a column is 28 KB (inside L1) or 2.4 MB (past
L3) - 15 GB/s throughout. It is bound by per-element interpreter work,
about 2 cycles/elem, not by memory. `binop` moves 0.27 -> 0.35. Running
these over cache-sized lane tiles cannot recover time they are not losing:
the whole select+binop+map budget at frame 39 is 0.76 s, and the part of
it attributable to working-set size is **about 0.06 s of a 4.5 s frame.**

So the original roadmap's step 2 - a tile executor for the straight-line
regions between split sites, full-width heap columns, per-tile scratch,
inverted instruction loop - is **retired before being built.** It was
premised on the filter benchmark's 3-10x cache-residency result applying
to block execution; measured on the actual op mix, it does not. (It would
also have multiplied per-instruction dispatch by the tile count.)

**The cache cliff is entirely in the merge pipeline**, which is also where
the time is: at frame 39 the frame-boundary machinery (dedup 1.30 +
filter 1.11 + concat 0.47 + hash 0.17 + normalize 0.20 = 3.25 s) is 81% of
censused time, and it is what degrades with depth - hash_rows 2.7x,
dedup 1.5x, filter 1.6x from frame 29 to 39. Tiling belongs there.

## What landed: cache-blocked dedup verification

Splitting the dedup bucket phase with a measurement-only build (skip
`rows_equal`) gave 0.43 s hash probe + 0.89 s row verification, and a
dedup-detail counter explained the second half: 303 M column-cell reads
scattered over ~17 separately-allocated columns, a ~340 MB footprint at
2.9 ns/cell. 97% of rows are duplicates, and every comparison reads all
17 columns and reports equal - no early exit to win, only cheaper reads.

Restructured (commit "dedup: cache-blocked verification"): probe pass
assigns each row the first row of its hash class comparing nothing; the
candidate-unique rows alone are packed row-major into a dense word array,
built in L2-sized tiles of rows; verification streams the columns and
compares each duplicate against one packed representative row. 17 random
accesses per duplicate become 1. Exact - a disagreement means a 64-bit
hash collision between distinct rows, and then those hash classes are
redone by the original algorithm (classes are independent). Two tests
drive that branch with hand-made colliding hashes.

| frame | before | after |
|---|---|---|
| 38 | 2.663 s | 2.541 s (-4.6%) |
| 39 | 4.62 s | 4.10 s (-11.5%) |
| cumulative to 39 | 15.84 s | 14.70 s (-7.2%) |

Dedup bucket at 39: 1.32 s -> 0.80 s. The win grows with depth, as the
penalty it removes does.

## Remaining budget at frame 39 (4.10 s wall, 3.51 s censused)

| item | sec | reading |
|---|---|---|
| state filter | 1.11 | 1.23 ns/kept vs 0.50 in the standalone gather benchmark; 1.5x of it is working-set growth. **Biggest single item, cause not yet isolated.** |
| dedup bucket | 0.80 | now mostly the hash probe. Radix-partitioning by hash bits would make probe *and* verify L2-local, but needs the row data physically partitioned too. |
| merge concat | 0.46 | 16 GB/s, degrades 1.6x with depth |
| select | 0.40 | 12.9 GB/s, flat in lane count - a codegen/SIMD target, not a cache one |
| binop | 0.34 | 21 GB/s, nearly flat |
| normalize | 0.20 | 143 ns/elem, sorts |
| hash_rows | 0.16 | degrades 2.7x with depth |
| uncensused | 0.47 | dispatch, phi moves, env/heap bookkeeping |

Ranked next steps, by measured prize:

1. **Isolate the state filter's 2.5x gap** against the standalone gather
   benchmark. It is the largest item and the gap is not explained yet.
2. **Radix-partitioned dedup** - partition rows by hash bits so each
   partition's probe table and representative rows fit L2. Est. 0.80 ->
   ~0.3 s, but it requires physically partitioning the columns.
3. **SIMD/codegen for select and binop** - flat in lane count means the
   loop body is the cost. This is the "compile blocks to real vector
   instructions" direction, and it is the *only* thing that helps block
   execution; tiling does not.
4. Re-apply the overnight parallel stack on what is left.

Measured and dismissed: **allocator tuning**. 922 k page faults and 1.05 s
of sys time in a cumulative-37 run looked like the first-touch cliff from
the calibration, but `MALLOC_MMAP_THRESHOLD_`/`MALLOC_TRIM_THRESHOLD_`/
`MALLOC_TOP_PAD_` and `glibc.malloc.hugetlb=1` each move it under 1%. The
faults are genuine heap growth, not churn; glibc already recycles.
