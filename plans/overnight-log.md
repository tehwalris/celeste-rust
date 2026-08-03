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
