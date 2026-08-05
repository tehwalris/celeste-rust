# Overnight log, 2026-08-05 -> 08-06

Free-run session on the state-structure findings (plans/state-structure.md).
Every change: full test suite + `rewrite verify --frames 37` + interleaved
3-round A/B on two binaries; regressions parked with numbers, one idea per
commit. All commits pushed to `census` (and `interpreter` follows it).

## Landed, in order

| change | runner `-n 39` | bench (rewritten) | commit |
|---|---|---|---|
| baseline (morning) | 44.36 s / 16.2 GiB | f37 2.89 s | 871178d |
| output-cardinality census (`CELESTE_INSTR_CARD`) | measurement | measurement | e981876 |
| uniform vectors collapse to `Scalar` at construction | 40.64 s (-8.4%), -8.6% mem | f37 2.71 s | 487e236 |
| virtual-concat merge (4th attempt, landed) | 39.03 s (-3.4%) | f37 2.57 s, peak -28% | ec5018f |
| ranged filter gathers (`KeptLanes`) | 33.54 s (**-13.5%**) | f37 2.38 s | bde95ef |
| parallel virtual merge (scoped threads, <=16) | 31.89 s (-4.8%) | f40 8.80 s (-10.6%) | (merge: parallelize) |
| union_diff guarded pass-through | **29.06 s (-8.8%)** | f40 **7.68 s (-12.1%)** | afd0653 |
| partitioned probe (hash high bits, >=512k rows) | 29.01 s (-0.4%) | f40 7.44 s (-2.4%) | (probe) |

**Cumulative: runner -n 39 44.4 -> 29.0 s (-34.6%). bench f40 9.76 -> 7.44 s
since the mid-night frontier measurement; f37 2.89 -> ~2.0 s est (-30%).**

Probe threshold lesson: at 16k rows partitioning was +0.3% runner (extra
O(n) passes beat mid-size savings); at 512k - where the map actually
outgrows the cache - both paths win. vm_probe 0.86 -> 0.43 s at f40.

Frontier measured mid-night (before the last two wins): bench f40 9.76 s /
3.70 GB, f41 16.07 s / 6.05 GB, f42 26.47 s / 9.36 GB (~1.65x/frame);
runner -n 40 50.4 s / 23.8 GiB. With the last two wins, subtract ~20%.

## Parked with numbers

* **Persistent seen-set at hint blocks**: +0.9% runner. Every hint fixed
  point converges in one round; nothing to save. (BENCHMARK_DATA.md)
* **Context-sorting merged lanes**: +3.3% runner / -3.4% bench split.
  Coding+permute on the plain path's 600k-lane boundary merges costs more
  than filters save. Natural lane order is already context-clustered.
* **(earlier that day) if-conversion of the hot forks, allocator tuning,
  block tiling** - see BENCHMARK_DATA.md.

## The important non-perf finding

`NormalizedState` (union_diff at hint blocks) compares vector columns by
**per-column sorted unique value sets** - an over-approximation of state
equality that could silently drop real states, invisible to differential
verify (both programs share the mechanism). `CELESTE_CHECK_UNION=1`
diagnostic added; measured **zero mismatches** (f37 rewritten, f38 plain) -
latent, not active, because arrivals are one-per-shape and fixed points are
single-round. The landmine is documented in the union_diff commit; if hint
blocks ever iterate (loops through hints), the coarse equality becomes
load-bearing and must be made exact first.

## Where the remaining time goes (bench f40 --profile, after all of it)

cfg:anonymous_61 self ~4.0 s (45%) - the frame body: instruction loop +
per-state overhead. virtual_merge ~2.1 -> ~1.3 s est. filter_branch ~1.3 s.
gc 0.07 s. Everything else small.

## Ranked next steps

1. **Fragment-parallel frame execution.** cfg:anonymous_61 is 45% and
   embarrassingly parallel across fragments (states in the worklist are
   independent). Blockers: profiler/DAG/census thread-safety, worklist
   order. The parallel-experiments branch never did this - it parallelized
   within ops instead (and its big-vector-op piece regressed).
2. ~~Virtual merge probe pass~~ DONE (partitioned, 512k threshold).
   Remaining inside virtual_merge at f40: vm_pack_verify 0.61 s (pack is
   still sequential), vm_probe 0.43 s, vm_hash 0.40 s.
3. **filter_branch 1.3 s**: the remaining cost is mask production and
   per-vector dispatch, not the gather. The structural fix is fewer forks
   (rewrite side) or RLE masks.
4. **Dictionary/RLE representation** (state-structure.md #1/#2): the
   biggest structural lever, unbuilt. All instruction time is redundant
   (census), but it is spread thin; only a representation change collects it.
5. Deep-frame headline run for the morning: bench f43/f44 with the final
   binary; runner -n 41.

## Operational notes

* A/B binaries in /tmp: ab_{base,new,vm,ud,rf,sort,par,un}_{runner,rewrite}.
  "un" = current HEAD.
* `merge_hint_normalize` span count == frames x 2 -> single-round fixed
  points; if that ever changes, revisit both the seen-set idea and the
  union_diff guard.
* The equivalence test pins virtual merge == materialized pipeline exactly;
  the materialized path is the fallback and must stay in sync.
