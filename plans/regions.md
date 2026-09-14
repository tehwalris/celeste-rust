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

## What is left on the table

Lanes are sliced in cell order, so a slice mixes configurations and a
region runs if ANY of its 16 lanes needs it. Grouping a call's lanes by
configuration before slicing (a cheap pre-pass on the premises, or a
sort of the frontier by a configuration key at the checkpoint) would
make most slices single-configuration and the skip near-complete.
