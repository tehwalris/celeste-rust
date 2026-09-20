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

## Result

**Room (3,0): optimal 89 frames** (`OPTIMAL win frame: 89`, 03:06). h89
confirmed at all 18 levels; h88 refuted at level 6 (r6sxhb: no win by f88).
It equals the community TAS (TAS4: 62 inputs + 27 spawn frames). The search
itself, with level 0 resumed from last night's tree: 34 min wall (02:32 -
03:06), peak anonymous RSS 12.8 GB (level 1 of h89), 210 GB of checkpoints.
In the UI as `room30opt`, the default run: http://127.0.0.1:3011/celeste/

| h88 level | first win | marked |
|---|---|---|
| 0 | f64 | 21.3M |
| 1 | f74 | 2.34M |
| 2 | f83 | 1.10M |
| 3 | f87 | 356k |
| 4 | f88 | 327k |
| 5 | f88 | 243k |
| 6 | NO WIN | refuted |

What made it fit, in order of effect: the deadline-bounded mark filter
(decision 5), level 0 dropped from memory during the finer levels (6), the
kernel-set cap (4). Floors unknown up the rem ramp (1, 3) cost nothing and
changed no count where they were made exact.

The UI took level index 16 as "exact" and showed 17 levels; it now names
every level from its logged precision (L16 "15 bits/H", L17 "exact") and
takes the count from the run. Checked in the browser.

**Verified on a real PICO-8.** The concrete witness (`rewrite witness
--horizon 89 --level 17`, the reference engine's DFS through level 17's
marks) first died on the fruit: `to_interp_state` refused `got_fruit[3]`
(a sparse integer part), and with those successors skipped it found no
path (3.5M steps, 23,168 skipped) - every marked route takes the fruit.
The interpreter keeps such a table dense with explicit nils and `bind`
flattens it the same way, so `to_interp_state` now does too (`c9e9224`).
Then: **win at f89 in 279 concrete steps, 0 dead ends**, and
`pico8_diff/replay.py --room 3,0` shows the room change to (4,0) at f89
(f88: player at 48,-4). The inputs are `tas/room_3_0_exit_frame_89.txt`:
34 idle spawn frames, right with one jump at f42, a jump at f52, then
dash up-right (38) at f62 and f76 with jumps between.

**Against the community TAS (classic/any/TAS4.tas, 62 inputs).** With the
27 spawn frames first it exits at f89 on the ORIGINAL cart (PICO-8, f88
player at 47,-3), equal to ours. On celeste-minimal the same inputs do not
exit (f89: player at 23,75, falling) - the minimal cart diverges, as for
room (2,0). Both routes are controllable from f28; TAS4 runs right at once,
ours stands until f35 and is 14 px behind at f36, yet both are at (21,59)
at f60 and within 2 px from there to the exit - the first stretch has
slack. **Both take the fly fruit at f77**: the dash at f62 sets it flying,
it rises to (16,23), and at f77 it is gone - TAS4 leaves a `lifeup` at
(14,17), ours (the minimal cart has no lifeup) removes it with the player
at (22,24). (`pico8_diff/replay.py` now prints fly_fruit/fruit/lifeup.)
The fruit looks NECESSARY for 89: the witness run that skipped exactly the
fruit-taking successors searched level 17's marked states exhaustively and
found no win by f89. Taking it also refills the dash (`hit.djump =
max_djump` in `fly_fruit.update`), a plausible reason - not established.

**TAS4's positions against our search (2026-09-20).** At every frame
f28-f88 its position (original cart) is inside every h89 level's allowed
cells, the exact level included (a marked state at that cell with time
left to win by 89). The one miss - level 1, f78, (18,22) not first reached
at f78 - is the cross-frame dedup: that cell was reached earlier. And
`rewrite trajectory` follows TAS4's positions frame by frame in OUR game
and wins at f89: the route is physically ours; only its raw inputs do not
transfer to the minimal cart.

**Room (5,0) (600 m), started 2026-09-20.** TAS6 is 48 inputs; the player
is controllable from f30 on both carts, so ceiling 29 + 48 = 77 (the same
rule gives room (4,0)'s proven 76). But TAS6 only exits under favourable
random numbers: one original-cart replay left the room at f77, four others
(a one-frame-different cart) fell at (82,84). The room uses `rnd` (balloon
`offset=rnd(1)`, and a `rnd(3)`). Our model draws `rnd` as an interval
(`4e2d2e9`), so the search's optimum is the BEST CASE over random outcomes
- as a TAS that manipulates RNG. Worth deciding whether that is the
question we want answered.

Decided with Philippe (2026-09-20): `rnd` stays an interval at EVERY
level, so the ladder's "exact" level is exact except for the random draws.
Run the ladder to exact and see; then, at the exact level, try concrete
RNG seeds one after another until one reaches the win (or all are covered).
The first search died in the room walk: room (5,0) is loaded by `_init`,
so the balloon's `offset = rnd(1)` is an interval in the START state, which
the walk's write discovery never typed as an interval input ("objects[1].
offset was already symbolic"). The walk now types the start state's own
intervals as interval inputs too (`room_constant_lattice`, `c6d11e9`).

Plan, refined with Philippe: `rnd` is an interval at every level EXCEPT
exact; at exact, enumerate the concrete draws until one wins. Issues noted:
`rnd(1)` has 65,536 values (16 fraction bits) and the balloon draws once, in
`_init`, so exact is "the start state with offset = k/65536" per k -
confirming a horizon stops at the first k that wins, refuting one at exact
needs all 65,536 (rooms 3 and 4 refuted far below exact, at levels 6 and 9);
running them all at once is out (the coarser marks cannot tell k apart, so
the exact frontier would carry up to 65,536x), so in batches. Treating
each draw as free is the best case over VALUES, not over PICO-8 seeds.

**Superseded (2026-09-20, with Philippe):** the "refuse the merge" fix below
(the `rnd` taint, `reads_rnd`, the refusal diagnostic, `max_states` 1024)
was REVERTED uncommitted: with the cap raised the room (5,0) walk ran 21 min
(41 CPU-min) without finishing. Replaced by a SPLIT AT A POINT - the
balloon's interval `y` is cut at the player's per-lane position, exactly as
`__split_by_flr` cuts at a fixed grid, except the cut is a runtime value:
`Domain::split_compare`, called from `binop_values` when a comparison with
one interval side and one number side is undecided. A 2-way fork
(`both_values`) is the comparison's value; its validity (the half of the
interval that answer leaves is non-empty: `x < t` true iff `lo(x) < t`,
false iff `hi(x) >= t`; `x > t` true iff `hi(x) > t`, false iff
`lo(x) <= t`; mirrored when the interval is on the right) goes in the
path's guard. No new graph op, no cart edit, no marking.

The second blocker, at level 0 (f51, 4.6M states): the player-balloon hit
compares against the balloon's interval y (`74 < [71, 75]`), and the two
arms cannot merge (the pop refills the dash), so the merge added its
`Known(cond)` premise and the kernel declined the lanes - a coverage gap.
Fix (2026-09-20): a condition that reads a `rnd` draw refuses the merge like
one reading an unknown atom (`Domain::reads_rnd`, beside
`reads_unknown_atom` in `state::merge` and `joins_independent`), so the
two sides stay two successors and an undecided lane emits both. The draws
are a per-frame TAINT carried FORWARD by `Graph::fold` (seeded by each
`range_num` leaf and the cells of the walk's `ival_extra` slots; a point or
boolean constant is never tainted). The first version walked the
condition's cone backward and missed the balloon: `sin` of an interval is a
literal span [-1, 1], so `y = start + sin(offset) * 2` kept no link to the
draw (f50 unchanged at 4,581,199, same gap at f51). With no draw, `fold`
takes its old path and nothing changes.

With the taint, six room (5,0) walk nodes then hit the tracer's frontier
cap (288 and 312 states, limit 256). `CELESTE_TRACE_REFUSALS=1` (new: names
each refused merge's condition and first differing slot) showed the refused
merges are balloon-hit families being paired across: the hit splits every
later outcome of the frame in two (popped + dash refilled, or not), and
`merge_inner` already selects on the SEPARATING decision (`State::path`),
so within a family nothing is refused. 288 = 2 x 144 and 312 = 2 x 156:
the legitimate doubling of those regions' player frontiers. The cap
(`Interp::max_states`) is now 1024 - a limit no other room reaches, so their
kernels are unchanged; it still stops a real runaway before `collapse`.

## For the morning

- `concrete_run --object fly_fruit` prints "none" on every frame of a route
  where PICO-8 shows the fly fruit throughout: its `find_object` misses it.

- The fly fruit stays unknown only at level 0: unknown it multiplied states
  above level 0 and overflowed the 58-fork `ChoiceSet` from r8 up. Worth a
  look whether the fruit's undecided `collide` could merge instead.
- The deadline filter changes nothing in any marked set (the gate), but it
  is a new pruning rule in the ladder's soundness argument - worth reading
  `MarkFilter::allowed` and `edges::bfs`'s doc together.
- `refbridge::to_interp_state` now flattens a sparse integer part into a
  dense nil-padded array (as the interpreter and `bind` do). Worth a second
  look that no other table in the cart relies on the sparse form.
- `CELESTE_KERNEL_SETS` is opt-in; with level 0 dropped, room (3,0) peaked at
  12.8 GB, so a default cap is a question of rebuild time vs memory.

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

   The ladder then tightens fast (h89; each level ~2 min plus a ~30 s
   kernel-set rebuild under the cap):

   | level | first win | marked |
   |---|---|---|
   | 0 r0sxhfb | f64 | 26.7M |
   | 1 r1sxhb | f74 | 3.59M |
   | 2 r2sxhb | f83 | 1.68M |
   | 3 r3sxhb | f87 | 0.72M |
   | 4 r4sxhb | f88 | 748k |
   | 5 r5sxhb | f88 | 696k |
   | 6 r6sxhb | f89 | 813k |
   | 7-13 r7..r13sxhb | f89 | 492k-730k |
   | 14 r14sxhb | f89 | 352k |
   | 15 r15sxhb | f89 | 80,271 |
   | 16 r15sxh (floors exact) | f89 | 80,271 (same count, new keys) |
   | 17 rxsx (exact) | f89 | 59,268 |

   **h89 CONFIRMED at every level** (levels 1-17 in ~35 min, done 02:54).
   A trial `export-ui` of h89 alone (log cut at the confirmation, into a
   scratch dir) read the custom ladder's 18 levels fine: 6.8 MB in 63 s. Floors going exact at
   level 16 changed no count: on every winning route the fall floors behave
   as "unknown" does. The first win reached the ceiling at level 6.

   With this much headroom the kernel-set cap could go up (each rebuild is
   ~35 s, 17 per horizon); left at 2 - not the bottleneck.
7. **The crashed attempt's finer-level trees deleted** (`h089/`, `level01/`,
   63 GB): built under the old ladder. `level00/` (153 GB) is kept and reused -
   the new ladder's level 0 is the same `r0sxhfb`. Nothing on disk records a
   level's spec, so the resume would not notice a mismatch; the reuse rests on
   the level-0 kernels being unchanged by tonight's edits (the fruit premise
   only fires on a non-literal, which level 0 never has; the rung-set flags
   change only the non-level-0 sets).
