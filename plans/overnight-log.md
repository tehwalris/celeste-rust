# Overnight log (2026-08-04)

Goal and rules: `plans/overnight-north-star.md`. One entry per attempt;
numbers for both directions; kept/parked verdicts. Baseline binary and
recipe as of commit 06b6248.

## Baseline

`./safe-run.sh -- ./target/release/celeste-rust -n 40`, commit 06b6248.
Per-frame times: f30 0.33s, f32 0.84, f34 2.09, f35 3.12, f36 4.55,
f37 6.78, f38 9.92, f39 15.06, f40 23.85. Growth ~1.55x/frame (lanes
1.54x/frame: f40 = 948,319 lanes). Cumulative through f40 ~= 69 s ->
**baseline reaches frame 41 (~106 s) within the 120 s budget**; frame
42 (~+58 s) does not fit. us/lane degrades 9.5 (f34) -> 25 (f40):
deep frames are memory-bound. Every frame ends 31 -> 1 states.

## Attempt 1: virtual-concat merge (candidate #1) - PARKED, three variants

Hypothesis: merges materialize the full concatenation (90% duplicate
rows), hash it, then filter survivors - dedup over a *virtual*
concatenation should kill the double copy. All variants byte-identical
through verify 34; all interleaved at 37 vs base 3.70-3.79 s / 1.65 GB.

1. **Segmented-virtual** (per-state scalar-or-slice columns, dedup mask
   before single masked materialization): 3.97-4.03 s (**+7%**), 1.35
   GB (**-18% memory**). merge_groups self 0.45 -> 0.16 s (masked
   materialization is a real -0.29 s) but dedup 1.34 -> 1.87 s: 19.0
   key columns vs 17.1 (all-equal vectors no longer collapse before
   keying) and 4.2 vs 3.0 ns/element (segmented access + enum dispatch
   in the 21M duplicate verifications).
2. **Contiguous-key** (materialize only key columns as scratch, fast
   hash, masked state build): 4.26-4.31 s (**+15%**), memory win gone
   (1.73 GB). The key concat moved into the dedup span and the masked
   gather (branch per element, 90% skipped) is slower than
   filter_by_mask's O(kept) index gather.
3. **Borrow-only concat** (old pipeline, no pre-clone of heap values):
   neutral (3.77-3.78 vs 3.70-3.79 overlapping). The pre-clone was an
   Arc refcount bump, not a copy - the earlier attribution of the
   merge span to "cell clones" was wrong; the span is the concat
   itself. Patch preserved at /tmp/patch-borrow-concat.diff.

Verdict: parked (time regressions; the metric is time-bound - memory
does not bind until ~frame 43). The -18% memory of variant 1 is worth
remembering if the 100 GB cap ever becomes the constraint. Corrected
understanding: the merge span is concat, not clones; masked
materialization only pays if the dedup mask can be computed at
contiguous-hash speed, which the virtual layouts cannot.

## Attempt 2: parallelism (candidate #3) - LANDED, three commits

The interpreter was entirely single-threaded on a 32-core machine.
All parallel results are bit-identical to sequential (arguments in the
commit messages); every step verified through 34, the stack through 37.

* **99c806e parallel dedup**: hash pass over row chunks; bucket phase
  partitioned by hash high bits (equal rows share a hash, so every
  duplicate-candidate set lands intact in one partition; first
  occurrence preserved). At 39: **10.45-10.60 -> 7.70-7.73 s (-26%)**.
  dedup_state span 4.04 -> 0.99 s.
* **(follow-up commit) parallel concat**: per-cell merges split across
  threads. ~-3% median, never slower; merge_groups 1.73 -> 0.98 s.
* **(follow-up commit) parallel heap filter**: the gathering
  allocations of filter_vectors_if_needed computed across threads.
  7.36-7.71 -> 6.98-7.15 s.
* **1eac28e lane pool + parallel vector ops**: two measured dead ends
  on the way - (a) per-op scoped-thread spawns: **+75%** (12.5-12.7 s
  at 39) from spawn cost x hundreds of thousands of ops; (b) routing
  the sequential path through the generic chunk builder: ~2x slower at
  34 (inlining of the op closure lost). Final form: persistent
  condvar-dispatch pool, direct loops verbatim below 128k lanes,
  pooled chunks above. Neutral-to-slightly-positive at 39; expected to
  pay at 40-41 (1-1.5M lanes). 32k threshold measured worse.

Net at 39 frames: **10.5 -> ~6.9-7.1 s (-33%)**. Threads capped at 16
(merges) / 8 (ops) - the work is memory-bound.

* **9433cec parallel local_env filter**: the env holds up to ~30
  vector locals, comparable to the heap's columns; same pattern.
  6.75-6.98 -> 6.48-6.54 s at 39. Also serialized the profiling tests
  (global registry, raced under the parallel runner, flaked once).
* **Parked: parallel normalize in union_diff_states** - consistent
  small LOSS (~+0.15 s at 39) in two shapes (per-state threads and
  chunked); allocation contention in clone+gc+sort beats the
  parallelism. union_diff (0.52 s at 39) stays sequential; the real
  fix is caching normalized forms across fixpoint rounds (the
  accumulated set is re-normalized every call) - task #52 territory.

## Working notes (for post-compaction continuation)

* Metric runs: `./safe-run.sh -- ./target/release/celeste-rust -n 41
  --detail-from 99 --detail-to 99`, read the `... in Xs` per-frame
  lines. Full-run times vary +-15% run to run - the FINAL headline
  number must be interleaved full runs, base commit 06b6248 binary vs
  final binary, 3+ pairs. Bench comparisons stay interleaved
  two-binary at 39 (`/tmp/rewrite-<name> bench --frames 39`).
* Latest full run (9433cec): f39 13.4-13.5, f40 19.0, f41 27.2
  (earlier run at 1eac28e: 12.0/18.1/28.8 - within cross-run noise).
  Baseline (06b6248): f39 15.06, f40 23.85, f41 not run (~37 est).
* Profile at 39 after all parallel commits (6.7s wall): cfg:a61 2.09,
  filter_branch 1.62, merge_groups 1.01, dedup_state 1.01, union_diff
  0.52, everything else <0.15.
* Remaining ideas, rough order: (a) filter_branch residual - the
  O(n) mask->kept scan is sequential per call, and each branch filters
  BOTH sides = two gathers over the same state, could share; (b)
  cfg:a61 - what remains after pooled ops: sub-threshold ops on
  fragment states, per-instruction overhead, local_env clones -
  instr_time profile (bench --profile) resolves it; (c) merge concat
  ns/cell still ~2000 at 39 - the columns concat could go through the
  lane pool instead of per-cell chunks; (d) task #52 caching for
  union_diff; (e) GC arena/pooling if memory becomes the binder at
  f42+ (~2x per frame growth; f42 ~8GB?, cap far).
* Binaries preserved in /tmp: rewrite-base2 (06b6248), rewrite-par1/2/3
  (dedup/concat/heap-filter), rewrite-par6 (pool ops), rewrite-parA
  (env filter), rewrite-parB (single-pass kept). Baseline full-run log
  /tmp/baseline-40.log; metric /tmp/metric-41.log.
* Further threshold probes: op-pool at 64k measured worse than 128k
  (6.75-6.97 vs 6.45-6.71), same as 32k earlier - 128k stands.
* Single-pass kept (committed): neutral at 39, strictly less work.
* Full stack differentially verified through 37 (31.9 s).
* First headline A/B (3 pairs, /tmp/metric-ab.log): deep frames won
  big (f41 37.5-37.7 -> 27.3-27.7 s, **-27%**; f40 -20%, f39 -11%) but
  frames 34-37 REGRESSED (+72% at f34), which bench had not shown.
* **Bisect on the runner at -n 37**: base 6.66, 1eac28e (pool) 5.66,
  9433cec (env filter) 7.66, pool-routed filters 7.18. The parallel
  local_env filter was the regression - bench at 39 said -4%, the
  runner said +35%. **bench and the search runner disagree on
  filter-level parallelism; the runner is the metric.** Reverted
  heap.rs/local_env.rs to their 1eac28e form (heap keeps scoped
  gathers, env sequential), kept single-pass kept + test lock.
  Runner at 37 after revert: 5.76-5.80 vs base 6.71-6.74.
* Memory at f41: ~44 GB both sides (cap 100 GB - not binding at 41;
  frame 42 at ~1.55x growth -> ~68 GB, still fits; frame 43 would not).
## Headline result (interleaved full runs, base 06b6248 vs HEAD 0405fba)

Two pairs, /tmp/metric-ab2.log, per-frame seconds (base -> final):

| frame | base | final | delta |
|---|---|---|---|
| 34 | 2.12 | 2.13 | 0% |
| 36 | 4.61 | 4.09 | -11% |
| 37 | 6.77 | 5.78 | -15% |
| 38 | 10.05 | 8.23 | -18% |
| 39 | 15.35 | 12.20 | -20% |
| 40 | 24.00 | 18.66 | -22% |
| 41 | 37.57 | 29.35 | **-22%** |

Peak RSS at 41: 44.3 -> 42.8 GB. No shallow-frame regression (the
first A/B's +72% at f34 was the env-filter commit, since reverted).

**Metric**: cumulative to frame 41 ~= 107 s (base) -> **87 s (final)**.
Both reach frame 41 inside the 120 s budget; final has 33 s of
headroom vs base's 13 s. Frame 42 (~45 s at the 1.55x growth rate)
does not yet fit - it needs roughly another 25-30% off the deep
frames. Memory would fit (f42 ~ 65 GB < 100 GB cap; f43 would not).

The speedup is entirely the parallelization chain (dedup partitioning,
per-cell merge concat, scoped heap-filter gathers, pooled vector ops);
the win grows with depth because that is where the machinery is
memory-bound and wide.

Next-best known items toward frame 42, in order: (a) cache normalized
forms across fixpoint rounds in union_diff (the accumulated set is
re-normalized every call - task #52); (b) whatever a fresh deep
profile of the *runner* (not bench) says; (c) the parked virtual-merge
memory variant if the cap ever binds.

## Post-headline: union_diff cache (517c191)

Item (a) built: StateSetAccumulator carries the normalized set across
fixpoint rounds (states immutable once inside -> cache never stale;
membership semantics identical). **Neutral on the runner at 39 over
four pairs** - the span is dominated by normalizing the *pending*
arrivals, not the accumulated set (which holds only 1-3 states per
round). Landed as strictly-less-work + removes a latent quadratic in
fixpoint rounds. Verify 34 clean.

Conclusion for item (b): union_diff was not the frame-42 lever. What
remains at depth per the last profiles: cfg vector ops (pooled above
128k lanes only), merge concat bandwidth, dedup bandwidth - all now
parallel and memory-bound. Frame 42 in 120 s likely needs either more
memory bandwidth (NUMA placement? huge pages?) or a task-volume
change (the endgame/hierarchical directions from plans/), not more
thread fan-out.
