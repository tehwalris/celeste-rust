# Roofline census of the interpreter (2026-08-04)

Measured on the PRE-parallel baseline (branch `census` off 06b6248) with a
per-op census (`src/op_census.rs`, `CELESTE_CENSUS=1`): every vector-scale
operation records calls, elements, bytes, nanoseconds. Census overhead is
negligible (f39: 15.43 s instrumented vs 15.48 uninstrumented). The workload
is the search runner (`celeste-rust -n 39`), i.e. the *unrewritten* program -
what the search actually executes today.

## Machine calibration (/tmp/roofline_cal.rs, single thread)

7950X3D: L1d 32 KiB/core, L2 1 MiB/core, L3 128 MiB (96 MiB V-cache CCD).

| pattern | L2-sized | 32 MiB | 80 MiB | 1 GiB |
|---|---|---|---|---|
| copy into FRESH Vec | 98-149 | 5.9 | 5.2 | 5.1 GB/s |
| copy into REUSED buffer | 97 | 36.9 | 34.1 | 29.8 GB/s |
| add into fresh Vec | 115 | 8.0 | 8.0 | 7.5 GB/s |
| add into reused buffer | 93 | 37.5 | 32.4 | 31.1 GB/s |
| hash fold (in place) | 41 | 38 | 37 | 36 GB/s |
| 10% gather (fresh dest) | 83 | 35 | 26 | 13 GB/s |

**The 5-6x cliff between fresh-allocation writes and reused buffers is the
headline**: beyond L2 size, first-touch page faults on freshly allocated
Vecs cap streaming writes at 5-8 GB/s where the memory system sustains
30-37. The interpreter allocates a fresh Vec for *every* op result.

## The census at 39 frames (cumulative, ~46 s wall)

| category | calls | Melems | GB | sec | GB/s | ns/elem |
|---|---|---|---|---|---|---|
| **state filter** | 5,602,299 | 18,878 | 205.9 | **23.50** | **8.8** | 1.24 |
| dedup bucket | 339 | 76.3 | - | 4.28 | - | 56.1 |
| normalize | 373 | 22.9 | - | 2.92 | - | 127.8 |
| gc | 58,326 | - | - | 2.38 | - | - |
| merge concat | 26,723 | 5,553 | 35.2 | 1.31 | 26.9 | 0.24 |
| binop (map2) | 330,844 | 1,220 | 8.7 | 0.60 | 14.4 | 0.49 |
| hash_rows | 339 | 1,300 | 25.3 | 0.40 | 63.5 | 0.31 |
| map/map_to | 27,196 | 147 | 0.9 | 0.07 | 12.4 | 0.47 |
| censused total | | | | **35.5** (~77% of wall) | | |

(`select` never fires in the unrewritten program - selects are a rewrite
product; the uncensused ~23% is interpreter dispatch, phi moves, env/heap
bookkeeping, shape grouping, input expansion.)

## Reading

1. **State filtering is half the runtime (23.5 of ~46 s) and runs at 8.8
   GB/s - the fresh-allocation roofline, not the memory roofline.** 5.6M
   per-cell gathers allocate 5.6M fresh Vecs and touch 206 GB. At the
   reused-buffer rate (~30 GB/s) the same work costs ~7 s: **~16 s (~35% of
   wall) is recoverable by buffer reuse alone, single-threaded.** This is
   what the overnight parallelism was actually treating - page faults
   parallelize - and reuse is the simpler, bigger fix.
2. **The pointer-bound trio** - dedup bucket probes (56 ns/row), normalize
   sorts (128 ns/elem), gc walks (58k calls x 41 us) - costs ~9.6 s (~21%)
   and is latency-bound, not bandwidth-bound. Roofline here is algorithmic
   (fewer probes, cached normal forms, incremental gc), not memory.
3. **True vector compute is ~5% of wall** (binop+map+concat+hash ~= 2.4 s).
   Even at perfect L2-blocked throughput the program's arithmetic is
   nearly free; the interpreter's cost is *moving and reorganizing lanes*,
   not computing on them.
4. **Locality**: binops average 3.7k elems/call (L2-resident sources) yet
   reach only 14 GB/s - allocator traffic again. Concat at 26.9 GB/s is
   already near the reuse roofline. Lane-blocking (running blocks over
   cache-sized lane groups) matters most for the filter+concat pipeline
   where the same lanes are touched repeatedly per frame; with a 96 MiB
   V-cache a whole deep-frame working set nearly fits.

## Implied priority order (all pre-parallel, single-threaded)

1. Buffer/arena reuse for filter gathers (and op results generally):
   ~16 s at f39-scale. Design: per-thread scratch arenas, or recycle the
   source Vec when refcount is 1 (`Arc::try_unwrap` / make_mut in-place
   compaction - filters already own many of their sources uniquely).
2. Algorithmic cuts to the pointer-bound trio (dedup probe layout,
   normalize caching - partially landed on `rewrite` - and gc batching).
3. Lane-blocking / compiled kernels for the remaining streaming work.
4. THEN re-apply parallelism to whatever is left; it composes with all of
   the above, and its biggest current contribution (parallelizing page
   faults) becomes unnecessary.
