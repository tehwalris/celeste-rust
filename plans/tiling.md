# Tiled (SIMT-style) execution of the rewritten frame

Follows from plans/roofline.md: cache residency is worth 3-10x and the
census says the cost is moving lanes, not computing on them. Analysis of
"fixed-width tiles until a branch point, masks after" against the code the
search actually needs to run.

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
Rewritten census at 39: filter calls 5.6M -> 46k (the branch-removal
campaign's real payoff); remaining time: dedup bucket 3.2 s, filters
2.75 s (still 8.8 GB/s), concat 1.1, select 0.9, binop 0.8, hash 0.4.

## Why tiling fits the rewritten program specifically

* The hot path is ~a dozen giant straight-line blocks (loop-free since
  stage Q; K = 3060 instructions), lane-wise by construction. Tiling a
  straight-line block = run its instruction sequence over an 8k-lane tile
  with locals in reused tile scratch (~8k x live-values x 4-8 B ~= under
  1 MiB = L2-resident; sources stream through L1). The 91-99 GB/s
  cache-resident row of the filter benchmark is this regime.
* Scalar ops re-execute per tile: measured negligible (ns each,
  ~128 tiles at 1M lanes).
* Branch inventory on the hot path is exactly the 11 split sites:
  - scalar-condition branches: tile-uniform, zero divergence cost;
  - UnknownBool btn splits: state-level duplication BETWEEN tiled
    regions - orthogonal to tiling, unchanged semantics;
  - vector-condition gates (dash_time>0, freeze, move gate): SIMT
    divergence - carry a per-tile mask, run both sides masked, reconverge
    at the static join. Uniform tiles (lanes are history-ordered, so
    clustering is likely) take one side at zero cost; the cost of a mixed
    tile is bounded by tile width. Masked stores already exist
    semantically (select-stores/masked speculate_region); the executor
    needs a tile-mask context, not new IR.
* Not tileable and fine: frame-boundary merge/hash/dedup and hint
  merges are cross-lane by nature - already near roofline (hash) or
  algorithmic (bucket probes; the overnight parallel dedup addresses
  exactly this and composes).
* The unrewritten program is NOT viably tileable (450 blocks,
  lane-varying loops, 5.6M filter events) - the rewrite recipe is what
  makes tiling possible, and the --rewritten switch is what makes it
  matter.

## Roadmap

1. Land the runner-on-rewritten switch properly (3.4-5.3x now).
2. Tile executor for straight-line regions between split sites: full-width
   heap columns pre-allocated once, per-tile scratch locals reused,
   instruction loop inverted (tile-outer, instruction-inner). Targets the
   ~3.3 s of streaming ops plus filter locality.
3. SIMT masks for the vector gates - removes most remaining branch
   filters and the g162-style whole-state masking penalty (uniform tiles
   skip).
4. Re-apply the overnight parallel stack on top: tiles are natural
   parallel units, each core's tile resident in its own L2 - better
   scaling than full-width column splitting, no bandwidth fight.
