# Level -1 as a native coarse level: a design study (2026-10-10)

Philippe: "Level -1 is janky. We do need a filter like that, but it should be
a more native abstraction and not so special-cased." The idea studied: make
level -1 an ordinary COARSE LEVEL - widen everything but the heap shape and
the player cell, run it with the same tracer, kernels, storage and edges to a
fixpoint, take each coarse node's distance to the exit from the BFS, and
filter the fine levels as the objects ladder does (`MarkFilter`). Branch
`l1-native` (prototype code: the `v` level flag, `coarse-census --floor`,
`CELESTE_BUILD_PROGRESS`).

## Verdict

**Not feasible on the kernel path.** Whatever the coarse level does with the
player's SPEED, it is either too expensive to compile or too big to close:

| speed at the coarse level | kernel per (shape, region), room (1,0) | coarse state space (lower estimate) |
|---|---|---|
| exact (today's `r0sxh`) | 96-112 bodies, ~2.2k fused nodes, 0.02 s to lower | timers widened: 3.16M (cell, speed) states by f75, +130k/frame, not closing |
| literal `[-5, 5]` (level -1's S) | 6,800-10,674 bodies, **~830k fused nodes**, 3-5 s to lower; the 102-kernel build OOMs at 30 GB | (shape, cell): closes (level -1: 17,904 nodes) |
| 1 px buckets `[k, k+1)` per lane | **220 outcomes, ~77k bodies, ~707k fused nodes**, 10 s to lower | 131k (cell, bucket) by f75 with timers widened; 1.62M with timers exact |

Today's level -1 for comparison: room (1,0) 17,904 nodes, start d 44, **11.5 s**
(quick, 8 threads, cache off, fingerprint `b909eb5d1645fad6` as pinned); room
(7,1) 21,250 nodes, start d 45, **21.0 s**.

The level -1 evaluator gets away with the 11-way move forks because it
evaluates one specialized graph per (shape, cell) node over ranges, in
Rust, once; the kernels pay for every fork configuration in assembled AVX-512
code per (shape, region), whatever the lane count. A coarse level has tens of
thousands of lanes in total: the compile is the whole cost, and it is
100-400x a level-0 compile. This is the same wall `plans/lessons.md` records
twice ("Speed buckets", "`flr_ways` capped": 13-way move forks took room
(3,0) square (5,13) from 1,236 to 35,502 bodies); the prototype re-measured it
for this design rather than assume it.

**Recommended instead**: keep level -1's producer (it is the cheap way to
evaluate a speed-widened frame) and make everything AROUND it native - one
filter interface shared with the objects ladder, win predicates, horizon from
the search, the record in the tree, the known-route check against the same
lookup. Details under "Recommended plan".

## 1. The abstraction: what the coarse level would have to widen

To match level -1's node `(shape, cell)` the coarse level must widen, per
row:

- **the remainder**: already `[-0.5, 0.5)` at every level (the arcs);
- **the speed**: level -1 uses `[-S, S]`, S = 5 (the cart's top speed is the
  dash's 5 px/frame; S <= 7 because `move`'s loop unrolls for `abs(amount) <= 8`);
- **the player's counters and flags**: `grace` (7 values at f75 of a room
  (1,0) tree), `dash_time` (5), `dash_effect_time` (11), `djump` (2),
  `dash_target`/`dash_accel` (3 each), `flip.x`, `has_dashed`, the global
  `freeze` (3) - level -1 reads each as one range per shape, discovered
  inductively; natively they would be the unknown number (`AV::UNum`, the
  countdown atoms `n` already has) and unknown booleans;
- **the objects**: beyond `n`. In room (7,1), (shape, cell) with the objects at
  `n` precision is already 560k states by f70 (level 0 there: 38.4M), against
  level -1's 21,250 nodes: level -1 lifts every object field to one range per
  shape (`Lift::Ends`/`All`), and the fly fruit to the `f` level's fields.

What the tracer, lowering and kernels support, as tried:

- **Speed as a lane-independent literal** (the first `v`: input
  `Const(-5, 5)` with containment owed per lane, as `fork_fruit_inputs`;
  output the same literal, containment owed; `flr_ways` uncapped as level -1
  sets it). The tracer accepted it and every frame traced; `move` forks 11
  ways per axis (fork ways `[2 x 8, 11, 11]`), so 121 move configurations per
  body. Lowering: 830k fused nodes per kernel (380x); the build OOMed at 30 GB
  with 32 workers and did not finish lowering in 120 s with 4.
- **Speed as a per-lane bucket** (the committed `v`: output forked on its
  floor, 2 ways, each part stored as the span `[flr, flr + 1)`; input an
  interval cell; `move` stays 2-way). Traces; but every comparison the frame
  makes on the speed (`appr`'s `>`, `abs(spd.x) > maxrun`, `sign`, the
  spring's `*0.2`) is now lane-undecidable and the split pass splits outcomes
  on it: 6 -> 220 outcomes per kernel, 6.8k -> 77k bodies.
- **Forks on lane-undecidable comparisons** work as designed (the split pass);
  they are exactly what makes both variants big.
- **Widened object phases**: `n` exists; going further (level -1's per-shape
  ranges for every object field) has no kernel form today and would need the
  same inductive range discovery level -1 does, moved into the lattice walk.
- **The transfers** (the arcs) assume the player's move is a rotation by one
  `ox` per row; with speed an interval the image is a smear. A coarse level
  would record no usable transfer (only the remainder-free BFS is needed, so
  this is a cost, not a blocker: the transfer roots would have to be skipped
  at that level). Not reached in the prototype (no kernel set finished).

## 2. Size and cost (measured, room (1,0) and room (7,1))

Census of what a coarse key would merge, over level-0 trees (`rewrite
coarse-census --erase P --floor P`, distinct states through the frame; a
coarse forward over-approximates, so its closure is LARGER than these):

| room, tree | kept | (shape, cell) | (cell, exact speed), counters widened | (cell, 1 px speed) | (cell, 1 px speed), counters exact | (cell), counters exact |
|---|---|---|---|---|---|---|
| (1,0) `r0sxh` f0-f75 | 24.3M | 4,773 | 3.16M | 131k | 1.62M | 707k |
| (7,1) `r0sxhn` f0-f70 | 38.4M | 560k (objects at `n`) | 37.7M (objects at `n`) | | | |

- Level -1 reaches more nodes than the tree's 4,773 (17,904: off-screen
  pockets, unreachable cells its widening admits) and still builds in 11.5 s.
- A coarse forward with exact speed would be ~1/8 of level 0 and grows
  without a visible fixpoint by f75: not a pre-pass.
- The kernel build is the cost for any widened speed (table above); no
  forward ran, so no fixpoint frame count could be measured. With (shape,
  cell) keys the visited set would close within ~start d + the longest
  shortest path (~50-90 frames for (1,0)); that part of the idea is sound.

## 3. Precision

Not measurable natively (no coarse level built). What is known:

- Level -1's start d: 44 for (1,0) (optimum 99, remainder-free first win of
  level 0 f89), 45 for (7,1) (optimum 86). It bites only in the last ~20
  frames (plans/level-minus-one.md): (1,0) drops 34% of level 0's rows f0-f99,
  all from f73.
- A coarse level with speed per node (1 px buckets) would be TIGHTER - it is
  plans/level-minus-one.md's own "bigger" fix (speed per node instead of
  `[-S, S]` everywhere bends the top's straight cut along gravity and walls) -
  and so would counters kept exact (its "fix C", `freeze`/`djump` per node).
  Both multiply the node count (131k and 1.62M above against 17.9k), and both
  need kernels the prototype measured at 77k bodies each.
- With the remainder-free BFS a coarse level's distance equals level -1's
  modulo the widenings: the BFS deadline over a closed graph is `H - dist`
  for every node reached by its deadline, and a fine row at step t projects
  onto a node reached by t, so `deadline >= t` is exactly `t + dist <= H`.
  The filter semantics carry over unchanged.

## 4. Every consumer and special case

How each would look natively, and what the recommended plan does with it.

- **Deaths and respawns.** Natively a death is an edge to the respawn chain,
  and the BFS needs no `1 + d(start)` rule. But level -1 had to run the spawn
  as a CHAIN because a position-only spawn node never converges (`solids =
  false` and one speed range per shape let it rise forever): a (shape, cell)
  coarse level would meet the same divergence in its forward. Keep the chain;
  it is the right model of a fully deterministic prefix.
- **Win rects and synthetic wins.** Today refused (`minus_one_table` asserts
  `win_rect().is_none()`). Natively free: the BFS seeds `wins_of`. In the
  recommended plan: the table's Dijkstra seeds the nodes inside the rect (a
  position predicate on (x, y) cells) besides the exit edges; then the gate
  and the dev loop's synthetic wins can run with level -1.
- **Split frame and steps.** Natively the coarse level counts steps like every
  level. Today `CELESTE_LEVEL_MINUS_ONE`'s H is in steps while `--to` /
  `--ceiling` are frames; take H from the search's horizon instead
  (`--level-minus-one S`), converted once by `steps_per_frame`.
- **Raise and drop notes.** They need `admitted_from(row, t)`, the smallest
  horizon admitting a row. For a distance filter that is `t + d` - for level
  -1 AND for remainder-free coarse marks (above): with distances instead of
  deadlines the objects ladder's filtered tree could be raised too, instead
  of rebuilt per horizon. Arc-REACHED marks (start- and horizon-dependent) stay
  fixed-H (`admitted_from` = never beyond H): a raise refuses them, as today.
- **Known-route checks.** Today `known.rs` reads `MinusOne::from_env`; it
  should read the tree's record (`TreeFilter::read`), like the forward. With
  one filter interface the check is one call per step for every filter
  (`admitted_from(projection, s) <= H`), and `rewrite l1-check` becomes
  `check-known` (its per-lineage "too late" report is the same verdict).
- **The objects ladder.** Level -1 would be rung 0, `--level` gaining a
  distance-only rung. With the producer unchanged it is still "rung 0 with
  its own engine": the plan names it so (a `Filter` the ladder stacks)
  without pretending it is a kernel level.
- **Caching.** Natively the coarse tree is a checkpoint dir, reused when its
  inputs match (as a tree is now, by its record). Today's cache key hashes the
  binary and every `CELESTE_*` variable not known irrelevant: a superset by
  design (an unknown knob costs a rebuild, never a wrong table). Keep the
  binary hash (code changes move tables); replace "every env var" with the
  table's declared inputs (start room, loading jank, split frame, S, level
  flags read) plus a refusal of unknown `CELESTE_*` variables that the tracer
  reads - i.e. the same list, inverted, so it fails loudly instead of silently
  missing.
- **Determinism.** Natively free (the storage's ids are canonical). The table
  is deterministic today (fingerprints reproduce across thread counts:
  `b909eb5d1645fad6` again here at 8 threads).
- **The emission-time filter is a performance feature.** `UnitSink::
  minus_one_drop` drops a row by a (shape, cell) hash lookup BEFORE it is
  keyed ("half the emissions of room (6,2) 100% f57"). A MarkFilter-style
  filter projects (`Rt2::widen_to`) and keys every row first. A native coarse
  node must stay keyed by (shape, cell) to keep that, which is one more
  reason the coarse node cannot carry speed or counters.

## 5. Deletion

If the native coarse level had worked (all of it replaced by a level flag and
the BFS):

| what | lines |
|---|---|
| `src/trace/level_minus_one.rs` (tracing, cone copy, specialization, range evaluator, inductive fixpoint, Dijkstra, `CostToGo`, cache, `probe`) | 2,210 |
| `frame.rs`: `MinusOne`, `minus_one_table`, `TreeFilter`, the minus-one half of `Filters` | ~210 |
| `src/bin/transpile.rs` | 54 |
| `rewrite l1-check` | ~75 |
| `UnitSink::minus_one_drop`, the level -1 parts of `known.rs`, `domain.rs`'s `uncapped_ways` / `no_known_forks` | ~70 |
| total | ~2,600 |

(The raise, the drop notes and `gates/raise.sh` would stay, generalized.)

The recommended plan deletes less, ~500 lines: `transpile` (54; a `rewrite`
subcommand covers it), `probe` (~280, superseded by `check-known` and the
census), `l1-check` (~75), the env plumbing of `MinusOne`/`TreeFilter::Legacy`
(~60), and the duplicate filter code in `known.rs`/`unit.rs` behind one
interface. The producer (~1,650) stays and is honestly named.

## 6. Recommended plan, migration and risks

1. **One filter interface** (`frame::Filter`: `admitted_from(row, t) -> u32`,
   with `notes_drops()`): level -1's table (`t + d`), remainder-free coarse
   marks (`t + dist`), arc-reached marks (fixed H). `UnitSink`, `known.rs`
   and the raise call only it. Validate: the gates, `gates/raise.sh`, and
   `check-known` with every pinned TAS (the l1-check list in
   plans/level-minus-one.md: (1,0) 99, (5,0) 77, (2,3) 106, (6,2) 75, (3,0) 89)
   - zero "too late" on an exiting lineage, now through the same lookup the
   forward uses.
2. **The horizon from the search**: `--level-minus-one S` (default off),
   H = the search's horizon in frames, converted to steps once; drop
   `CELESTE_LEVEL_MINUS_ONE`. The tree record (`level_minus_one.txt`) stays
   the authority for a resumed tree; `known.rs` reads it too.
3. **Win predicates**: seed the table's backward with the cells of
   `win_rect` / the synthetic win. Validate: the arc gate with level -1 on
   (`--win-at 9,101 --to 35`) must produce identical `[gate]` lines (the
   filter may drop rows but not marked ones) - this also gives level -1 a
   cheap gate it lacks today.
4. **The window**: keep the panic (it checks a premise), but report the row
   and the frame as a `KERNEL COVERAGE GAP`-style fatal with a hint, not a
   bare assert.
5. **Cache key**: declared inputs + the binary, refusing unknown tracer
   knobs. Risk: a forgotten input reuses a wrong table - mitigated by keeping
   the binary hash and the table fingerprint in the tree record (a mismatch
   refuses the tree, as today).
6. **Not now**: speed per node or counters per node (precision). Both are
   real gains in the last ~20 frames and both need a coarse node bigger
   than (shape, cell); they belong in the EVALUATOR (one evaluation per node,
   no compile), not in the kernels. The measurement to make first: level -1
   with node = (shape, cell, 1 px speed) on room (2,0) - node count, build
   time, start d against 45.

What would refute the conclusion here: a kernel form of the move that does
not enumerate amounts (a per-pixel loop over a lane's amount range, bodies
independent of the hull width). That is a different lowering of `move`, not
a level flag; if it existed, the speed-literal coarse level could be revisited
with the numbers in section 1.

## Reproduce

```bash
# worktree /var/tmp/l1n-wt, CARGO_TARGET_DIR=/var/tmp/l1n-target, quick profile
# today's level -1
CELESTE_L1_CACHE=off CELESTE_THREADS=8 CELESTE_START_ROOM=1,0 ./safe-run.sh -- target/quick/transpile --level-minus-one-table 5
# the bucket-speed level's kernel sizes (stop it after the [lower] lines; the build does not finish)
CELESTE_BUILD_PROGRESS=1 taskset -c 0-7 ./safe-run.sh --memory 30G -- target/quick/rewrite forward --to 200 --room 1,0 --level r0sxhv --checkpoint-dir D
# the census
target/quick/rewrite forward --to 75 --room 1,0 --level r0sxh --checkpoint-dir T
target/quick/rewrite coarse-census --level-dir T --from 0 --to 75 \
    --erase 'rem,dash_effect_time,player[0],freeze,has_dashed' --floor spd
```

The literal-speed variant is not in the code: it was the bucket variant's
`widen_speed` writing `Const(-5, 5)` with containment owed on both sides
(`fork_fruit_inputs`' pattern) and `uncapped_ways` set at the level.
