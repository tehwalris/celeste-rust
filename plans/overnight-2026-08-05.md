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

| state-parallel flow (opt-in: plain program only) | **22.33 s (-22.4%)**, +14% mem | flat (gate off) | (parallel flow) |
| select pick loop specialized per arm repr | flat | f42 -2.6% | (select) |
| one-pass branch split (both edges at once) | 21.55 s (-3.4%) | f42 within noise | (branch) |
| parallel dense pack (tiles on scoped threads) | 20.97 s (-2.8% median) | f43 -1.2% | (pack) |

**Milestone gate: `rewrite verify --frames 40` identical on the full
stack; `CELESTE_CHECK_UNION=1` at `-n 40`: zero mismatches.**

**Cumulative: runner -n 39 44.4 -> 21.0 s (-52.7%). bench f42 26.5 ->
~19.4 s, f43 32.2 -> ~30.8 s since the mid-night frontier.
Final headline re-run: runner -n 41 = 50.1 s / 40.2 GiB (was 107 s
yesterday, 87 s on the parallel branch) - 2.1x yesterday's baseline.**

Morning headlines (final binary):

| | time | peak |
|---|---|---|
| runner `-n 40` | 36.2 s | 24.8 GiB |
| runner `-n 41` | **54.0 s** | 41.3 GiB |
| runner `-n 42` | **81.9 s** | 62.5 GiB |
| bench `--frames 43` | **32.2 s** | 13.7 GB |
| bench `--frames 44` | **61.0 s** | 18.7 GB |

Yesterday's baseline reached frame 41 in 107 s; the overnight parallel
branch did 87 s. Tonight's stack does **54 s** - 2x yesterday. Frame 42
on the plain path - which yesterday's log said "needs bandwidth or task
volume" - now completes in 81.9 s / 62.5 GiB. On the rewritten path the
budget map is measured through f44: **frame 44 in 61 s / 18.7 GB**, so a
60 s budget reaches frame 44 and 120 s reaches ~frame 45 (~110 s
extrapolated, ~31 GB) - two frames deeper than the morning estimates.
Note the us/lane creep at depth (10.0 at f43 -> 13.3 at f44): the merge
grows superlinearly; the dedup input volume lever is not done.

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

## The f44 profile (what to attack next, measured)

bench --frames 44 --profile: cfg:anonymous_61 self 34.7 s (56%),
filter_branch **9.8 s** (16%; it was 1.3 s at f40 - superlinear in
lanes, the two hot fork sites again), vm_pack_verify 5.6 s (pack still
sequential), vm_hash 3.7 s, vm_probe 3.2 s, everything else <1.5 s.
The two depth levers: (a) the frame body - but NOT via bandwidth:
perf stat at f40 measured IPC 1.95 and ~2.8 GB/s of DRAM traffic (539M
cache misses over 12.4 s), nowhere near saturation. The body is compute-
side with L3-resident per-state working sets, which also reframes the
parallel-flow regression on this path (likely mutual L3 eviction between
concurrent states, not bandwidth). The dictionary/per-context case
therefore rests on the census fact (100% of instruction time recomputes
values already present in the same vector, <=59 distinct) - the win is
computing each distinct result once per context, not narrower streams.
(b) filter_branch = the dash_time forks; if-conversion is measured
impossible (2.2x), so the fix is run-structured masks or splitting the
state permanently at those sites.

instr_time at f44 (same binary): **select alone is 13.4 s** of ~35 s
flat instruction time (call spans nest callees, 51.5 s by-site;
call_builtin 10.6 s). Fragments only grew 695 -> 1087 f37 -> f44, so the
depth growth is in per-lane op volume, and select - whose outputs the
census showed carry <=51 distinct values - is the single best target for
per-context/dictionary evaluation. Start there next session: select's
cost anatomy (gather vs allocation vs arms), then a prototype that
evaluates hot selects per distinct (mask, arm-value) context instead of
per lane.

## Ranked next steps

1. ~~Fragment-parallel frame execution~~ DONE as state-parallel flow
   steps, opt-in for the plain program (-22.4% runner). The rewritten
   path regresses under it (+4-6%: its fused block is memory-bound per
   state), so its win must come from somewhere else - likely making
   per-state work narrower (dictionary/RLE), not more concurrent.
   Two lessons the hard way: std::thread::available_parallelism reads
   procfs per call (7.7M syscalls before caching); per-call scoped
   spawns at flow frequency cost minutes of system time (rayon pool).
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
