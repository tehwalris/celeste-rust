# Guarded regions in the fused kernels (2026-09-14)

## The problem

A shape's fused kernel is ONE straight-line AVX-512 program: every fork
configuration and every button representative is a body, the bodies
share nodes by hash-consing, and every slice evaluates all of it. Two
measurements on 2026-09-14 (`CELESTE_BODYSETS=1`, `CELESTE_CONE_CENSUS=1`,
`rewrite bench-frame`):

- room (2,0) level 0: 2,738 bodies over 18 shapes (~150 per shape);
  only **14.6% of (body, slice) evaluations took a lane**; 77 rows
  emitted per input row, 6.7% surviving the call's dedup. A frame cost
  ~1 us per raw row against 0.17 us in room (0,0): f50 23 s, f60 88 s,
  f69 221 s (50M lanes) - hours per horizon, and the 60 GB cap by f70.
- room (0,0) level 1: 336 bodies for the main shape (level 0: 36),
  28.5% taken - why level 1's forward was the finer levels' cost.
- Per lane only **4-12 distinct body sets** occur per shape (each 32-54
  bodies: one fork configuration times the button representatives), so
  a lane's configuration is nearly determined by its state and the
  waste is the OTHER configurations' nodes.
- Per node: the `ok`/`live` cones are 63-73% of a big shape's graph (a
  separate guard-only kernel would cost as much as it saves), but only
  3-9% of nodes are reached by every configuration and 33-84% by one.

## The structure

`transpile::asm::regions`. A REGION is a partial fork assignment (a set
of `(fork, resolution)` pairs). Each body's roots (its fields, `ok`,
`live`, its key chains) carry the body's own assignment over the forks
it depends on (`AsmBody::premises`, from `lower::specialize_frame`); a
node's region is the INTERSECTION of its consumers' assignments,
computed in one descending pass (ids refer downward). Regions nest by
set inclusion, and every node's operands lie in less specific regions
than its own, so emitting regions in order of specificity keeps every
value defined before any block that reads it - whenever that block runs.

The GUARD of a region: `Or` over the configurations consistent with it
of `Or` over their bodies of `And` over the body's premises of
`premise or not Known(premise)`. A lane whose premise is UNDECIDED keeps
the region alive: the body runs, declines the lane, and the coverage
gap surfaces exactly as before (a guard that only tested `holds` would
silently drop such lanes). Guards are ordinary graph nodes, interned
and folded; their cones are pulled into the root region (evaluated
unconditionally); they are stored as extra roots after every body's.

Census (root = unskippable): room (2,0) main shapes 5-7% of nodes in
the root region, ~90% in eight size-2 regions; room (0,0) level 1: 3%
root, 87% in size-2 regions.

## The emitter

`codegen::compile_regions`: nodes are lowered region by region
(`schedule_in` over a region's members with a shared `scheduled` set,
the region's exits as its batch roots), each non-root region between
`Inst::Guard { mask, label }` and `Inst::Label`. `reschedule` treats
them as barriers (segments scheduled on their own); the linear-scan
allocator is unchanged - a spilled value is stored at its definition,
so a skipped block leaves nothing half-written that a later block
reads, and a register freed by a use inside a skipped block merely
holds a stale value nobody reads. The value-numbering memo (loads,
broadcast constants) is scoped to the region: a constant materialized
inside a skippable block is not reused outside it. Root stores stay at
the end, unguarded: a skipped body's stores write stale registers or
never-written spill slots, which cannot fault, and the run loop masks
each body's `live` by its region's stored mask
(`AsmBody::region_root`, `present`), so no stale `ok`/`live`/key is
ever read as a lane's.

`CELESTE_ASM_REGIONS=0` emits the old straight-line kernel for an A/B.

## First A/B, and why it was small (2026-09-14, quick, 16 threads)

room (2,0) f45: wave 8.9-9.3 s -> 5.6-7.1 s; room (0,0) level 1 f80:
1.13 -> 0.87 s; room (1,0) f70 unchanged (4 regions, all present). The
census of what a slice needs (`CELESTE_BODYSETS=1`): room (2,0) 19.3
non-root regions per slice, **12.3 present (64%)** - lanes are sliced in
cell order, a slice of 16 mixes most configurations, and a region runs
if ANY lane needs it. A single lane needs ONE configuration (its 32-54
body set is one configuration times the button representatives), so
grouping lanes by configuration before slicing is where the 5-8x is.

## Lane grouping by configuration (the plan)

- A GUARD KERNEL per shape: the fused graph compiled with the region
  guards as its only roots - the root region, 3-7% of the nodes. Run
  over a piece in natural order it yields per lane the set of regions
  the lane can take (a small bitset: 4-33 regions).
- At the start of a frame each piece's rows are physically sorted by
  that bitset (a counting sort; the skip mask and cells follow) and the
  PERMUTATION (real row per virtual position) is written beside the
  frame's edges (`edges/perm/f{frame}/s{seq}.u32`). The step then sees
  consecutive VIRTUAL ids, so the recording (64-lane groups, masks, the
  dedup cache, the door) is untouched; only the edge runs' bases are
  virtual, and the BFS translates `base + lane` through the permutation
  of the run's frame and piece when it reads a record.
- Units are still cut in cell order across pieces; within a piece the
  rows are now ordered (configuration, cell), so a slice is one
  configuration except at boundaries.

## Lane grouping: what landed and what it measured (2026-09-14, quick, 16 threads)

Implemented as planned: `GuardKernel` per shape (`AsmKernel::classify`),
`FrameStep::classify`, `frame::group_by_configuration` (a piece's rows
sorted by (region set, cell), virtual ids, `edges/perm/f{frame}/s{seq}.u32`),
`EdgeGraph::real_pred` in the BFS and the diagnostics. Gates: ckhash,
posgraph, marks fingerprints identical, `bench-backward --diff` 0/0.

| | straight-line | regions | regions + grouping |
|---|---|---|---|
| room (2,0) f45 wave | 9.0 s | 5.6-7.1 s | 5.6 s |
| room (0,0) level 1 f80 wave | 1.15 s | 0.87 s | 0.75 s |
| room (1,0) f70 wave | 3.35 s | 3.35 s | 2.25 s |
| room (2,0) regions present per slice (of 19.3) | - | 12.3 | 10.3 |

About 1.5-1.6x everywhere, not the 5-8x the census suggested, and the
reason is in the per-lane keys: after grouping a slice is one key, but
the key itself holds ~10 regions. Two things the fork regions cannot
express:

1. **Spanning forks.** At level 0 `rem` is widened to a whole unit, so
   `rem + spd` spans two floors on each axis and BOTH resolutions of a
   rem fork are non-empty fragments for the same lane - the lane
   genuinely takes ~4-5 of the 16 configurations (its 54-body set is
   ~1.25 configurations' worth of bodies because only one OUTCOME is
   live, not because one configuration is). Finer rungs narrow rem and
   the span with it, which is why level 1 gains more.
2. **Dead outcomes.** A body's `live` (its outcome's branch was taken)
   is not in the guard - it is computed deep in the graph (the ok/live
   cones are 63-73% of it) - so an outcome that is dead for the whole
   slice still has its tail (fields and key chains, roughly half the
   nodes) evaluated. Keying regions by outcome as well needs `live`
   early, which it is not; a separable "outcome decided" predicate
   would have to come from the tracer.

Room (2,0) itself remains out of reach on this machine for a different
reason: the level-0 tree passes 50M lanes and ~600M visited states at
f69 (53 GB RSS) with the win around f95 (~10^8 lanes, 2-3 x 10^9 visited:
the door alone ~40 GB, before the frontier and the compaction's
transient). That is a state-count problem (the abstraction, the
motion-free specialization census in BENCHMARK_DATA.md), not a kernel
speed problem.
