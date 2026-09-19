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
5. **Level 1 still blew up; the mark filter gets a time bound.** The first
   run with this ladder (kernel sets capped at 2: level 1 started at 19.2 GB
   instead of 32.5) grew level 1 at ~2x the old one - 339k at f40, 14.6M at
   f51, 24.4M at f53 - and was stopped before its OOM. Level 0 marked only
   456k states at layer 51: the filter admitted fine states onto coarse
   states marked from EARLIER frames, with no time left to win. The BFS
   already knew better: iteration i marks exactly the states that win by H
   from frame i but not i+1, so i is the state's DEADLINE. It is now kept
   (`Marks` -> `Visited`'s per-key value) and `MarkFilter` admits a fine
   state at frame t only onto a coarse one with deadline >= t. Sound (the
   coarse level over-approximates: a fine state that wins from t widens to
   a coarse one that does) and it prunes only states with no winning
   descendant, so every level's marked set, win frame and outcome are
   unchanged - the marks gate is its check (identical, while its level 1
   keeps 99,960 states over all frames against 110,675: -10%; `4413c85`).
   Marks loaded from disk and the
   kernel re-run backward carry no deadline (`u16::MAX`): the old test.
   Unit test: `a_marks_deadline_is_the_last_frame_it_still_wins_from`.
6. **Level 0 dropped from memory while the finer levels run** (count-down
   only, `Ladder::drop_level0`): ~10 GB idle at f89, resumed from disk in
   42 s when the next horizon needs it.
   **Result (h89 level 1, r1sxhb):** level 1 started at 5.6 GB RSS (19.2
   with level 0 held, 32.5 with every kernel set), peaked at 15.7M states at
   f54 and then SHRANK - 3.2M at f60, 2.8M at f63 - where every earlier run
   was still growing (39M at f61 before the OOM). Peak anonymous RSS
   12.8 GB; the slowest frame 60 s.

   | frame | old r0sxh | membership only | + deadline |
   |---|---|---|---|
   | f45 | 1.09M | 2.11M | 1.81M |
   | f50 | 6.06M | 11.0M | 7.18M |
   | f54-55 | 25.8M | (24.4M at f53) | 15.7M (peak) |
   | f60 | 32.1M | - | 3.17M |

   With this much headroom the kernel-set cap could go up (each rebuild is
   ~35 s, 17 per horizon); left at 2 - not the bottleneck.
7. **The crashed attempt's finer-level trees deleted** (`h089/`, `level01/`,
   63 GB): built under the old ladder. `level00/` (153 GB) is kept and reused -
   the new ladder's level 0 is the same `r0sxhfb`. Nothing on disk records a
   level's spec, so the resume would not notice a mismatch; the reuse rests on
   the level-0 kernels being unchanged by tonight's edits (the fruit premise
   only fires on a non-literal, which level 0 never has; the rung-set flags
   change only the non-level-0 sets).
