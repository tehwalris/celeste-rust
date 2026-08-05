# What the state set actually looks like (2026-08-05)

Measured with `CELESTE_DUMP_MERGE=<min lanes>` (see
`src/interpreter/merge_dump.rs`), which dumps every state at a merge point,
post-gc, as raw binary and measures it four ways. All numbers below are
frame 38's deepest merge unless stated: **56 fragments / 11,901,141 lanes ->
1 state / 399,553 lanes**.

## 1. The state set is ~2200x larger than its information content

| | uncompressed | zstd-3 column-major | zstd-3 row-major |
|---|---|---|---|
| unmerged | 2213.2 MiB | 20.1 MiB (110x) | 32.4 MiB (68x) |
| merged | 74.3 MiB | 1.0 MiB (71x) | 1.7 MiB (43x) |
| merge gain | 29.8x | 19.4x | 19.0x |

* Dedup and compression are **complementary**: merging still wins 19.4x
  *after* compression, so zstd does not already capture what dedup does.
* The merged state has no duplicate rows by construction and is still
  **71x compressible**.
* Column-major beats row-major ~1.6x everywhere. The existing SoA layout is
  the right one.
* Compressibility falls with depth: 142x -> 132x -> 120x -> 110x (unmerged)
  across frames 35-38. Do not extrapolate to frame 43.

## 2. Every field has a tiny value domain

Distinct values per field, **identical before and after merging** - merging
removes duplicate rows, not distinct values:

| field | distinct | of lanes |
|---|---|---|
| cell197.x | 59 | 11,901,141 |
| cell197.y | 57 | |
| player x (cell151.x) | 51 | |
| player y | 20 | |
| dash_effect_time | 13 | |
| grace | 7 | |
| dash_time | 5 | |
| freeze, cell158.x/y, cell163.x/y | 3 | |
| has_dashed, djump, p_dash, p_jump, cell169.x | 2 | |
| the other 41 fields | 1 | |

**Maximum cardinality anywhere is 59.** Fields are stored at 4 bytes per
lane; six bits would do.

## 3. The rows are hierarchical, and the hierarchy is shallow

Columns visited cheapest-first, counting distinct prefixes after each:

| field added | card | distinct prefixes | branch |
|---|---|---|---|
| has_dashed | 2 | 2 | x2.00 |
| djump | 2 | 3 | x1.50 |
| cell169.x | 2 | 6 | x2.00 |
| p_dash | 2 | 10 | x1.67 |
| p_jump | 2 | 20 | x2.00 |
| freeze | 3 | 28 | x1.40 |
| cell158.x/y, cell163.x/y | 3 | 180 | x1.45-1.86 |
| dash_time | 5 | 524 | x2.91 |
| grace | 7 | 786 | x1.50 |
| dash_effect_time | 13 | **1,484** | x1.89 |
| player y | 20 | 10,666 | x7.19 |
| player x | 51 | 117,178 | x10.99 |
| cell197.y | 57 | 151,417 | x1.29 |
| cell197.x | 59 | **399,553** | x2.64 |

Only 17 of 58 fields ever branch. Every branching factor is far below the
field's own cardinality, so the fields are strongly conditioned on each
other rather than independent. The product of cardinalities is ~1.2e13 and
the rows occupy 4e5 of it - **3e-8 of the product space**.

The shape this suggests: **~1,484 discrete contexts** (all the booleans and
timers together), each carrying a set of positions. That split is exactly
where compression divides too - the discrete tail compresses ~10,000x, the
position fields only 7-11x. The positions are the information; the rest is
context that repeats.

## Where this points

Ordered by how much the measurements support them, not by ease:

1. **Dictionary-encode the columns.** Max cardinality 59 means one byte per
   field per lane instead of four, and per-lane arithmetic becomes a lookup
   over a domain of tens of values. The interpreter currently computes
   ~59 distinct results 11.9 M times.
2. **Factor the state set into (context, positions).** 1,484 contexts vs
   399,553 rows. Ops on the 41 constant fields and the 13 discrete ones
   would run once per context.
3. **Virtual-concat merge** (task #57, scaffolding in
   `src/interpreter/virtual_merge.rs`). The concat materialises 2.2 GB
   holding ~20 MiB of information. Parked three times on *time*; the memory
   case is much stronger than it was, and memory binds around frame 43.
4. **Attribute op time to fields.** Not done. The cheap version: record each
   vector instruction's output cardinality keyed by `LocalId` alongside the
   existing `instr_time`, then rank by `time x distinct/lanes`. Measures the
   waste directly without needing field attribution to be exact.

## Outcomes (2026-08-05, the night after)

What the list above turned into, in the order it was executed. All numbers
in BENCHMARK_DATA.md; every change differentially verified through 37.

* **(4) ran first** as the `CELESTE_INSTR_CARD` census. Verdict: 0.54 s of
  non-call instruction time at frame 37, **100% of it** duplicate-lane
  computation, no hot instruction (top 13 ms). The redundancy is uniform,
  so per-op fixes are pointless; representation-level changes are not.
* The census surfaced a cheaper fact than (1): instruction outputs with
  dist/exec = 1.0 - *constant* vectors nothing demotes. **Uniform collapse
  in `MaybeVector::vector`** landed: -8.4% runner, -8.6% memory.
* **(3) landed** on the fourth attempt (uniform columns + sorted piece
  walks): -3.4% runner, -28% peak on the rewritten path.
* A new filter census asked how chunky filter gathers are: **30.6
  lanes/run** already. **Ranged gathers (`KeptLanes`)** landed: -13.5%
  runner, the night's largest single win.
* Cumulative: runner `-n 39` 44.4 -> 33.5 s (-24%), `bench --frames 37`
  2.89 -> 2.38 s (-18%).

* **Context-sorting merged lanes was tried and parked.** Dictionary-code
  the <=32-cardinality columns, sort lanes by the packed key at every
  merge. Split verdict: rewritten bench 2.38 -> 2.30 s (-3.4%), but the
  runner 33.6 -> 34.7 s (+3.3%) - the coding + permute over the plain
  path's 600k-lane frame-boundary merges costs more than its filters
  save. Also instructive: mean lanes/run barely moved (30.6 -> 27.5,
  run-count-weighted), yet census filter time fell 24%, so the natural
  lane order is already context-clustered and the sort mostly upgrades
  the lane-weighted tail. Revisit if the frame-boundary merge shrinks,
  with a mid-frame-only gate, or when a per-context execution scheme
  needs contiguous contexts anyway.

Still open from the list: (1) dictionary codes as the *stored*
representation, and (2) context/position factoring.
