# Overnight 2026-09-18/19: room (3,0) to its optimum

Philippe's brief (23:50): get room (3,0)'s full optimal solution, loaded into
the UI; decide without blocking, write the decisions down here.

## Where it stood

The search (`--ceiling 89`, ladder `r0sxhfb, r0sxh, r1sxh .. r15sxh, rxsx`)
finished level 0 to f89 (frontier peaked at 22.7M at f74, 5.4M at f89; its
backward marked 0.4-1.4M states per frame), then level 1 (`r0sxh`: fruit and
floors EXACT) blew up under the level-0 marks - 25.8M kept at f55, 39.0M at
f61, 62 GB peak - and was OOM-killed at the 60 GB cap. The mark filter barely
bounds it: a level-0 mark (floors unknown) admits every exact floor/fruit
state that widens onto it.

## Decisions

1. **Fruit and floors unknown up the rem ramp** (Philippe's suggestion), made
   exact only near the top. `Level::grid_consistent` now allows `f`/`b` at any
   rem rung with an exact position (like `h`); the rung kernel sets take
   `with_fruit`/`with_floors` (`build_registry_for_rung`). Nothing else keyed
   on level 0: the input forks and output widenings go by the domain's flags,
   the mark filter's projection by the coarser level's.
   The fly fruit's widening (`widen_fly_fruit`) used to bail above level 0
   (the fruit's rem.y is no literal once rem is exact): its containment in
   the widened range is now a runtime premise in `ok`, like the region
   bounds - a lane outside declines loudly.
3. **Fruit exact above level 0 after all; only the floors stay unknown.**
   Measured (unfiltered `forward --to 36`, room (3,0), kept at f36):

   | level | f36 kept | |
   |---|---|---|
   | r0sxhfb (level 0) | 37,506 | |
   | r1sxhfb | 79,666 | |
   | r1sxhb | 71,306 | fruit exact is SMALLER |
   | r4sxhfb | 526,195 | |
   | r15sxhb | 175,376 | |
   | r8sxhfb / r15sxhfb | - | panic: 103 forks > the 58 a `ChoiceSet` holds |

   An unknown fruit does not merge anything above level 0 - its undecided
   `collide(player)` forks the player's outcome (collected / not) and both
   are emitted - and at fine rem it overflows the split mask. So the fruit
   goes exact at rung 1 and the floors carry the merge.
4. **Resident kernel sets capped (`CELESTE_KERNEL_SETS`).** The old run's
   RSS was 31.0 GB at level 0's f001 (the first binary, which still kept
   every kernel's fused graph) and 32.5 GB at level 1's f001 with the latest
   binary - of which level 0's held door is ~9 GB (550M visited keys at
   16 B), leaving ~20 GB of the 18 prebuilt kernel sets. A third of the cap
   before level 1 had a single state; it died at 62 GB. (One set alone,
   r1sxhb: 5.5 GB RSS after its build, 8.0 GB peak - allocator retention
   included, so not a clean per-set number; the run below measures it.) The cache now keeps at
   most N built sets and evicts the least recently used (rebuilt when its
   level runs again, ~35 s each on 32 workers - ~10 min per horizon for 17
   rebuilt sets, against hours of forward). Default unset = old behaviour, so
   the gates are unaffected; the room (3,0) run uses 2. Chosen over shrinking
   the per-set footprint because it is certain; where the 1.7 GB goes is still
   unmeasured (the asm text is already dropped; the templates' `Rt2`s are
   the suspect).
5. **The crashed attempt's finer-level trees deleted** (`h089/`, `level01/`,
   63 GB): built under the old ladder. `level00/` (153 GB) is kept and reused -
   the new ladder's level 0 is the same `r0sxhfb`. Nothing on disk records a
   level's spec, so the resume would not notice a mismatch; the reuse rests on
   the level-0 kernels being unchanged by tonight's edits (the fruit premise
   only fires on a non-literal, which level 0 never has; the rung-set flags
   change only the non-level-0 sets).
