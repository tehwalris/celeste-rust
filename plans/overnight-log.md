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
