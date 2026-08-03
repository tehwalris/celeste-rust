# Overnight north star: maximum frames per fixed wall-clock budget

Written 2026-08-04 on Philippe's direction, before an unattended
optimization session. Morning review decides what survives; nothing in
this doc pre-commits us to keeping anything.

## The goal

**Make the abstract forward search reach as many frames as possible
within 120 seconds of wall clock, under the 100 GB `safe-run.sh`
memory cap.**

* 120 s rather than 60: frame cost grows super-linearly, so the longer
  budget gives resolution *inside* the expensive frames (38-41), which
  is where an optimization matters for the real search. 60 s would
  mostly measure the cheap prefix.
* The metric is interpretation time only - recipe replay/build (~4.5 s)
  is excluded, or equivalently held identical between A and B.
* Canonical measurement: `./safe-run.sh -- ./target/release/celeste-rust -n <N>`
  (or `rewrite bench --frames N` for recipe-side changes), timing the
  frame loop; report the deepest frame completed within budget plus
  per-frame times for the last few frames. Establish the baseline
  first and record it below before changing anything.
* Exit 137 = OOM = the attempt failed the constraint, full stop.

## Standing rules (unchanged overnight)

* Interleaved A/B on the same binary; identical lane counts as the
  first check; **never land a measured regression** - park it with its
  numbers instead.
* Semantics-touching changes need `rewrite verify --frames 34` minimum,
  37 for anything that lands; frame-40 confirmation for the final
  configuration if time allows.
* Warning-free build, no dead code, tests green before every commit.
* One idea = one commit, so morning review can keep/drop per idea.
* Keep a running log in `plans/overnight-log.md`: for every attempt -
  the hypothesis, the numbers (both directions), kept/parked, and why.
  Failed ideas with clean measurements are deliverables too.

## Candidate directions, roughly ranked

Ranked by (expected wall-clock effect at deep frames) x (confidence),
informed by this week's measurements. Deviate freely if the profile
says otherwise - **profile at 37-40, not 34**: deep frames are
memory-bound (state clones 3x slower at 37, 29 GB peak at 40), so the
bottleneck ranking differs from the shallow frames we usually tune on.

1. **Kill the concat-then-filter double copy.** Today a merge
   materializes the full concatenated state (every cell of every
   fragment cloned), hashes it, then `filter_by_mask` copies the
   survivors again. Deep frames pay ~GB of memcpy per merge for rows
   that are 90% garbage. Restructure: hash rows *across fragments
   in place* (virtual concatenation), then build only the surviving
   rows once. Attacks both the 0.46 s `merge_groups` clone cost at 37
   and peak memory - the metric's two constraints at once.
2. **`kill_slot` IR instruction + liveness-pruned merges** (task #52,
   Philippe's design: deadness embedded in the IR, rewrite-placeable,
   verifier-checkable). Shrinks mid-frame merge keys 24.5 -> ~17
   columns; then re-test the early-merge stack (widen_buttons +
   widen_rem + add_hint, currently +33% with fragments 527 -> 61) -
   plausibly flips to a win, and a win here compounds at depth.
3. **Parallelism.** The interpreter is effectively single-threaded
   today; fragments between merge points are independent, and the
   column sweeps in hashing/merging are embarrassingly parallel.
   Check determinism carefully (dedup keeps first occurrence; state
   order must stay canonical for differential verify). Potentially the
   largest single multiplier on the metric; also the most invasive -
   attempt only with clean verify gates.
4. **Dedup micro-optimizations**: flat/open-addressing map keyed by
   digest (measured headroom ~15-20% of the dedup span), u32 indices,
   reuse of scratch allocations across merges. Small but safe.
5. **Per-merge-site removal counters, then skip no-op dedups**
   (corrected estimate: ~10-15% of dedup volume, 0.03-0.05 s at 34 -
   measure per-site first, build only if the counters agree).
6. **GC and allocation behavior at depth**: gc runs ~1100x per run;
   check its share at frame 39-40, and whether arena/pooling of the
   dominant Vec sizes moves the memory ceiling (the cap is a
   constraint, not just a nicety - headroom lets frame 41+ fit).

## What "done" looks like in the morning

* `plans/overnight-log.md` with every attempt, numbers, and verdicts.
* A chain of individually-committed, individually-verified wins on the
  branch; parked ideas preserved with their measurements.
* The headline number: frames reached in 120 s / peak RSS, before vs
  after, same binary discipline.
* An honest shortlist of "worth the complexity?" questions for review.
