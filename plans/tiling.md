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
| state filter | 1.09 | **1.03 s of it is `filter_branch`** - 42 state splits per frame, 18.5 M lanes kept. Not an interpreter problem at all: this is the branch-removal campaign's remaining exposure. See below. |
| dedup bucket | 0.80 | now mostly the hash probe. Radix-partitioning by hash bits would make probe *and* verify L2-local, but needs the row data physically partitioned too. |
| merge concat | 0.46 | 16 GB/s, degrades 1.6x with depth |
| select | 0.40 | 12.9 GB/s, flat in lane count - a codegen/SIMD target, not a cache one |
| binop | 0.34 | 21 GB/s, nearly flat |
| normalize | 0.20 | 143 ns/elem, sorts |
| hash_rows | 0.16 | degrades 2.7x with depth |
| uncensused | 0.47 | dispatch, phi moves, env/heap bookkeeping |

### The filter is branch splits, and they have names

Splitting filter time by reason (new census counters) at frame 39:

| reason | state filters | lanes kept | sec |
|---|---|---|---|
| filter_branch | 42 | 18.5 M | **1.03** |
| filter_split_flr | 80 | 1.8 M | 0.07 |
| filter_dedup | 2 | 1.6 M | 0.06 |

So a quarter of the frame is still the interpreter cloning a state
because a branch condition varies across lanes - the exact cost the
rewrite recipe exists to remove, not an interpreter inefficiency.

Charging each filter to the conditional that caused it (census names the
branch site in `glue.rs`, `filter_by_mask` charges it) splits the 11
split sites into two families that cost *completely different things*:

| site | filters | lanes in | kept | sec | unknown dups |
|---|---|---|---|---|---|
| in_h061_in_i1_074_cont | 24 | 24.3 M | 50.0% | **0.68** | - |
| in_h061_and_or_join_126 | 12 | 11.1 M | 50.0% | **0.35** | - |
| __main in_i1_012_if_join_526 | 2 | 0.8 M | 50.0% | 0.01 | - |
| anonymous_61 __entry | 2 | 0.7 M | 50.0% | 0.01 | - |
| __main in_i1_012_cont | 2 | 0.1 M | 50.0% | 0.00 | - |
| in_h061_in_i1_072_cont (k_dash) | 0 | - | - | 0.00 | 24 / 12.2 M |
| in_h061_in_k030_cont (k_jump) | 0 | - | - | 0.00 | 12 / 6.1 M |
| in_h061_if_body_162 (k_up) | 0 | - | - | 0.00 | 12 / 6.1 M |
| in_h061_and_or_continue_182 (k_down) | 0 | - | - | 0.00 | 12 / 6.1 M |
| in_h061_and_or_continue_75 | 0 | - | - | 0.00 | 4 / 2.0 M |
| in_h061_if_join_60 | 0 | - | - | 0.00 | 4 / 2.0 M |

**Two sites are 99% of the branch-filter cost**, and both are genuine
vector conditions - `in_i1_074_cont` branches on `dash_time > 0`,
`and_or_join_126` on a computed select. Those are if-conversion targets:
selects, `speculate_region`, `absorb_stores`.

**The four btn sites pay no filter time at all.** Their condition is an
`UnknownBool`, and `flow.rs` sends the whole state down both edges
without filtering - so they cost 68 state duplications over 34.5 M
lanes, which is what inflates the dedup input to 19.8 M rows and the
fragment count to ~695/frame. Their bill arrives as merge volume, not
filter time, and the fix is `expand` (task #47), not selects.

Both filter sites keep exactly 50.0% of lanes, every time. That is the
signature of a condition that tracks an expanded button's lane bit -
`expand` lays lanes out as [all-true half, all-false half], and
`dash_time > 0` follows the dash button exactly. Worth confirming: if
the split is contiguous in lane order, it is a slice rather than a
gather, which changes both what if-conversion is worth and what the
filter itself could cost.

Ranked next steps, by measured prize:

1. **If-convert `in_i1_074_cont` and `and_or_join_126`** - 1.03 s of a
   4.10 s frame, and it is two sites, not six. Both are vector
   conditions, so this is the existing select/speculate_region
   machinery. Recipe-side work, where `rewrite bench` is the right A/B.
1b. **`expand` the four btn sites** (task #47) - no filter time, but 68
   state duplications over 34.5 M lanes feeding the merge. Its cost is
   the merge volume, so measure it there, not in filter time. The trade
   is more favourable than when k_up/k_down were done, since dedup just
   got ~40% cheaper.
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
