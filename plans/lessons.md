# Lessons: what was tried, what killed it

One paragraph per abandoned approach, with the commit and the measurement.
Read before proposing something that sounds like one of these.

## Program transformation and kernels

**The January CFG optimizer** (branch `interpreter-abandoned-2026-01-11`).
A generic CFG optimizer - mem2reg, inlining, heap elimination, call
resolution, DCE - ~11k lines. Most of it was never wired into the game
runner, some of it produced CFGs the interpreter cannot execute, and its
validation did not check dominance. Reading material only: do not reintroduce
untested passes. (A crate-wide `#![allow(dead_code)]` of that era hid ~150
lines of dead code and a whole abandoned dataflow framework - hence the
warning-free rule.)

**The rewrite rules and the generated kernel crates.** 38k lines of rewrite
rules (deleted with the rewrite campaign, 2026-08-23, along with `--profile`,
`--trace`, the op census, the Chrome-trace `server/`, `measure_k`), then one
GENERATED kernel crate per room, ~923k lines between them, deleted
2026-08-29 (`d027f7e`) when the AVX-512 ASM backend assembled kernels at
startup with gcc + dlopen in milliseconds (rustc + LLVM took minutes). No
regen step, no staleness gate, nothing to keep current.

**The old forward/backward sweep search** (`src/search/run.rs`, `sweep.rs`,
`sweep_time.rs`, the deopt-collection path, shape-variant dispatch, banding,
the g/e/band numbering, and every `bin/rewrite` subcommand but `search`),
torn out 2026-08-31 for `src/frame.rs`. Its "proven" room (0,0) "94"
(`51a4d99`) was a frame too long. The lesson: a ladder result is believed
after `rewrite witness` + a real-PICO-8 replay, not before. (Room (1,0)'s
"100" was NOT a frame too long: it is the 2022 searcher's 76-update route
with its first input on frame 25 instead of 24, a prologue-count
difference; our 99 is the same route, plans/results.md.)

**Local rewrites of room (6,0)'s kernels** (2026-09-26, five failures in a
day), which produced the graph model (plans/architecture.md, "The tracer and
the kernel model"). The waste was not fusion: an outcome whose row reads 0-1
forks had a guard reading ~138, and `ok` - carried as state, AND-ed per merge -
made obligations outlive their values. `lower::quantify` (on the abandoned
`ac307ce` line) took one body's ok/live cone from 1,974 nodes to 96,540
(97-98% of the arena). Deriving error from the operators instead
(`trace::error`) removed the mechanism: room (3,0) 6.24M -> 3.25M fused
nodes, (5,0) 1.06M -> 406k. Two mistakes measured on the way: using
ABSTRACTNESS as the fork trigger (50-90 extra forks a frame), and counting
divisors in the reference cart instead of the traced one.

**The eager point split** (`split_compare`, 2026-09-20). Each comparison of
an interval with a number forked where evaluated, its validity in the path
guard, so every outcome's `live` read every comparison: room (6,0) at `r0sxhp`
70-86 forks per trace, 64-82 live - 2^70 bodies per outcome. Replaced by
forking after the frame on what survives folding (plans/abstractions.md).

**Platform worlds as a compile-time fork** (2026-09-27/28). The platforms
are a function of one number, so ~128 recorded arrangements were enumerated
as a fork: 2.3M bodies, 65M fused nodes, the largest region 907k nodes,
35 GB resident for the kernels, a coverage gap at f36. Grouping worlds whose
decided roots agree only took 128 to 74. Replaced by POINTS in the split pass
(`verify::Points`): the worlds narrow the path condition, they do not fork.

**Guarded regions in the fused kernels** (2026-09-14, reverted the same
morning; code at `07b3a15`). Room (2,0) level 0 ran at 14.6% body
utilization, so non-root regions of a kernel were skipped per slice, plus
lane grouping by configuration. ~1.5x (room (2,0) f45 9.0 -> 5.6 s, room
(0,0) end to end 23:50 -> 15:31), not the 5-8x the census promised: at level
0 a lane genuinely takes ~4-5 of 16 configurations (rem spans two floors per
axis), and dead outcomes' tails still run (`live` is computed deep). Philippe:
kernels stay compiled up front and branch-free; the idea survives as the
STATIC split of the frame (`CELESTE_SPLIT_FRAME`), which bought 10-20x in
kernel size.

**Keys hashed inside the kernels** (`Op::Word`/`CellMix`/`AddW`, 2026-09-12
to 09-18). Every body hashed every lane though a lane takes ~22 of 13,812
bodies; the room (3,0) (6,5) player kernel ran at 0.12 IPC, 57% stack
traffic. Folding the key per EMITTED row in the append step, then packing the
output buffer and dropping redundant reloads: the bench frame ~80 s -> ~33 s
(cycles 5.42T -> 2.56T).

**Diagnostics as `#[test] #[ignore]`** (five zero-assertion tests that only
printed tables, deleted in `2017312`). Nothing failed when their answer
changed, and every `--run-ignored all` paid ~2 min for output nobody read.
Diagnostics are `rewrite` subcommands now.

## The frame and memory

**The two-phase frame** (emit by source into per-owner slots / barrier / own
by destination with a hash-set visited shard per owner, 2026-09-13, then
emit/own BATCHES under `CELESTE_EMIT_BUDGET_GB`). It worked, but its
transient was a batch (4-8 GB), the visited set 24.8 B/entry with a random
DRAM probe per row, and it carried three concepts that existed only for the
split (owners, the owner hash, the budget). Superseded the same evening by
the waves frame: room (1,0) f0-f70 same speed, **9.45 GB -> 4.08 GB** peak;
room (0,0) f90 24.8 -> 14.2 GB. Before batching, room (0,0) died at f88 with
10.5 GB of raw rows in the slots; glibc retained ~23 GB more than mimalloc
(44 vs 24.8 GB at f90) - mimalloc is the global allocator since.

**The door, the queues and the inverted runs** (2026-09-13 .. 2026-10-10;
replaced by storage v2, plans/storage-v2.md). The waves frame's storage:
per worker a pool of (shape, cell) queues behind a per-unit row cache,
flushed into a door of sorted per-(shape, cell) shards (24 B a state and
its id), ids `(layer, piece, row)` renumbered at the wave's end (`canon`),
raw edge records per worker inverted into per-(layer, frame) runs once by
the backward. It worked; what it cost on room (6,2) 100% h94 (fg-2300,
shared machine): the inversion 54 s, the BFS and the graph load another
40 s, peak 27.2 GB (the door 1.1 GB at f57 and the runs mapped), a search
312 s; storage v2 (posmask entries, region ids, source-side blocks with
their translation tables, no inversion): 212 s, peak 8.9 GB, the same
answer. The research behind it: branch `emit-capture`, bench/dedup/DESIGNS.md.

**Other frame shapes rejected on paper** (plans of 2026-09-13): lockstep per
input cell (150-lane kernel calls per worker, the dedup window collapses, raw
rows 2x), streaming owners (hides <= 10% of a frame), two-pass emit-keys-then-
materialize (the kernel runs twice). And a cross-piece k-way merge into
"one cell in one chunk": built, cost ~250 ms a frame at f70 to save ~50 ms
of pushes (the door's delta already catches cross-piece duplicates).

**The incremental backward** (marks with distances, only untested pairs
re-run): built, exact, removed the same day - it saved ~15 s of a 524 s run
and did not fit the per-cell structure. Then the kernel re-run backward
itself gave way to the recorded graph: room (0,0) 2:32 h -> 23:50.

**Checkpoint layouts.** One file per cell-uniform block: ~17k files a frame,
an inode blow-up on tmpfs. zstd bincode layers: the H=89 backward spent 35 s
per iteration decoding ~100M rows to keep a few hundred. Now one
uncompressed file per (frame, shape piece) with a cell index (~3x the zstd
size on disk; checkpoints live on disk, not tmpfs).

**Prebuilt kernel sets for every level.** With 18 levels prebuilt, room
(3,0) held ~30 GB before its frontier (every kernel kept its fused graph).
Now graphs are dropped after assembly (7.3 -> 1.6 GB a set) and one set
is resident (the LRU cap `CELESTE_KERNEL_SETS` went with the rem ladder,
tier 3: levels now only change between forwards).

## Abstractions

**`p_jump`/`p_dash` widened at every level** (2026-08-06, again from the
field census 2026-08-16). A held trail made unknown admits a ground jump at n
and a wall jump at n+1; applied at every level, nothing narrowed it, and the
ladder could report a spurious optimum. Rejected twice; the rule "never widen
without something exact that refutes it" came from it. The working version
(`h`) is refuted by the concrete count-up.

**Speed buckets and the bucket dispatch** (2026-09-14..16; deleted 2026-10-04).
Post-hoc census promised 4.5x (room (1,0)) and 19x (room (2,0), the spring's
`spd.x *= 0.2`). Realized: a lane holding a speed hull enumerates the
successors of every speed in it - room (1,0) f50 1.11x fewer states, 1.5 rows
per state from hull re-emission, 9x the raw rows, frames 30-36x slower; the
bucket ladder 24 min to h88 against 4:51 for the whole exact search. Room
(2,0)'s key fixpoint did not build; room (7,0) `r0s16h` 9,821 kernels, 36 GB,
more states than exact at step 90; room (0,2) `r0s16hn` OOMed building. On
the way: the absolute table fork (10,444 bodies per kernel), a silent drop of
lanes on an unknown `live` (every speed-bucket run before 2026-09-14), lazy
per-bucket kernels behind a mutex, a sound join on undecided selects and
`fork_bools` (both quietly widen; replaced by the loud premise). Bucketing at
level 0 only cost about what rem-only cost; every speed refinement above it
was pure cost.

**The position rung** (`x2`/`y2`, 2026-09-14; deleted 2026-10-04). Halves a
frame's states for its own forward, then filters nothing: room (1,0) one
ladder at h99 9:32 against 2:56. The rule it taught: a coarse level only cuts
when the horizon is close to its own first win; a level whose backward marks
more than a small fraction of its forward is not earning its place.

**The time band** (`CELESTE_BAND="H,8"`, 2026-09-16). Drop a cell when 8 px
per frame up cannot reach the exit: carried room (2,0) to OPTIMAL 95, but 8 px
was a measured maximum, not a bound (spring snap + `spd.y = -3` is 11 px).
Replaced by level -1 (plans/level-minus-one.md); h95's level-0 marks were
identical, so the band had dropped nothing that mattered - by luck.

**In-frame widening** (design 2026-09-17, never built): "absorbing fields"
that a merge joins to unknown instead of a select, plus forks of an unknown
where read. Superseded piecemeal by the fruit's and floors' mechanisms
(literals stay literals, fragments rejoined at the call's return, escaped
atoms forked once, independent successors) and then by the split pass.

**The fly fruit unknown above level 0** (2026-09-18/19). Room (3,0) f36:
`r1sxhfb` 79,666 states against `r1sxhb` 71,306 (exact fruit SMALLER), and
at fine rem it overflowed the 58-fork mask; in room (6,0) the fruit-unknown
level-0 trace fanned out to 331 states (cap 256). Kept to the low rungs.

**Fall floors fully unknown (`b`) vs widened-but-near (`n`)** (2026-09-30).
`b` makes every floor "maybe absent" from frame 1: room (7,0) step 80 2.0M
states against 143k exact; `n` beat `b` 5-6x from step 100. `n`'s own
blowups then turned out to be a TRACER BUG (`7f6b96e`: the boolean join of an
undecided `collideable` threw the player's overlap test away). Lesson: when a
level grows, look at the states (`spurious`, `ancestry`, `rerun-row`) before
reasoning about the abstraction.

**The timers level `t`** (2026-09-30). Fewest states in room (7,0)'s early
steps, but 472k bodies (largest 2^13 configurations), ~16x an `n` set, and
UNSOUND: the ASM interval subtraction wrapped the full range's low end, so
`delay <= 0` was "no" and shaking floors never fell. Every `t` level of room
(3,3) was wrong. Fixed on `arc-sets` (`549ecf5`, then `b47b118`: overflow is
a lane's error, countdowns the unknown number; the first attempt `72fdea7`,
overflow -> the whole range silently, was rejected). The `t` level itself
now runs out of memory building room (3,3)'s kernels.

**Exact speed at level 0 in spring rooms** (room (2,0), 2026-09-15). The
overnight run OOMed at the 90 GB cap in f79: 175M kept at f78, +18%/frame,
door 36.6 GB. The "7,500 states per position at f69" estimate was a point on
a rising curve. Fixed by `h` + level -1, not by buckets.

**Level -1 for every room** (overnight 2026-09-21, reverted `4b31c1b`): it
did not rescue room (6,0) (d = 31 against the real 46; with platforms exact
every frame's states are new, so nothing is "too late" until ~f55), and its
night-time additions (platform phases, an "unmodelled node" rule) went. Kept:
the two repairs room (1,0)'s own table needed.

**A second round at the same rung** (`CELESTE_LADDER_RUNGS=0,0,0,1,16`): the
marks are closed under predecessors, so a repeated level reproduces them
exactly. Only precision narrows.

**Coarser level 0 for platforms by speed/position buckets** (room (6,0),
2026-09-21): `r0s16hb` OOMed in the kernel build past 60 GB; `x2y2r0s16hb`
was at 54 GB after 14 min. Platforms needed `p`.

**The rem rungs and their drift** (2026-10-02/03; deleted 2026-10-05). Room (3,3):
the coarse levels won at f154-f165 against the real 172 and the marks GREW
2-2.6x per rung; six ladders (objects abstract longer, timers only, held
exact at rem 6) ran out of memory. The mechanism (`ancestry --chain-out`,
`trajectory --spec`): a move that straddles a bucket edge by one raw unit
emits a SLIVER row that the boundary widens to the whole bucket, so the
abstract player gains up to a bucket width per frame per axis, and every
straddle doubles a row. Halving the bucket halves the drift per frame: a long
room needs many rungs. The rotation graph tracks the remainder exactly and
refuted 171 in 74 s (`c0de454`).

## Misc measurements that drove decisions

- **`dash_effect_time` pinned where no fake wall reads it**: merges 0.09%
  (room (2,0) f60) and 0.36% (room (4,0) f76). A field implied by the rest of
  the state merges nothing when erased: the `has_dashed` lesson. Not built.
- **What multiplies in an object room** (`coarse-census --erase`): room (3,0)
  f47 the fly fruit 2.30x, the player's speed 2.29x, the floors 1.05x (3.13x
  by f55); room (7,0) step 108 floors 4.6x, speed 3.2x, both 47.5x; room (2,0)
  f68 `p_jump` 1.98x, `p_dash` 1.92x, speed 26.6x.
- **Position census** (post-hoc, 2 px): ~2x in every room; y merges more than
  x (1.56x against 1.3x).
- **Rooms (1,2) and (3,2) had "no ceiling"** (2026-09-16: several player spawns,
  no replay left the room): the map-row nibble-swap bug (`5746919`), not the
  rooms.
- **`mget` outside the map** returns 0 in PICO-8; the branch-free kernel
  evaluates `spikes_at`'s second column for every lane, so a player at x >= 119
  read column 16. Fixed in `CartData::mget` and `zn_mget`.
- **The fruit is taken on room (3,0)'s 89**: the witness DFS with
  fruit-taking successors skipped found no win by 89.
- **Parallelism** (2026-09-13): room (1,0) level-0 f0-f89 791 s on one
  thread -> 65.6 s at 32; the full ladder 1 h 10 min -> 8:51. The gates are
  identical at 1, 16 and 32 threads.
- **Re-pins of the gates** happened only on evidence that the SETS did not
  move (posgraph, per-frame counts, marked counts, first wins, OPTIMAL line
  identical): `4e2d2e9` (`rnd` became a global), 2026-10-02 (`18208c6`'s
  `__phase`/`__frozen` globals). The edge-count "re-pins" `ffe939e`,
  `9921c0d` were scheduling noise in "edges read".

**The precision ladder** (2026-08-30 .. 2026-10-05; its rem rungs and driver
deleted on `arc-only`). The ladder confirmed a horizon by running every level
- rem rungs, then exact objects - each a fresh forward filtered by the
coarser level's marks (`MarkFilter`, with deadlines), and it found every
room's optimum. The remainder axis went: the arcs track it exactly (room
(3,3), where six ladders drifted out of memory, refuted 171 in 74 s). The
objects axis was deleted too, on the strength of rooms (1,0), (4,2), (5,3),
where the concrete search alone was cheap - and came back the same day:
room (7,0) at `r0sxhn` has its bound 4 frames under the optimum and the
search's region grew 6.5x a frame of slack (1.29M steps for f81, ~300M for
84), and an unfiltered exact-objects level 0 was 19x the states by f55. The
objects ladder on the arcs (`--level r0sxhn,r0sxh`, the filter now on the
coarse level's ARC marks) does the room in 14:40, its filtered second level
in 6.9 s. Lesson: an abstraction's cost is not the abstraction's alone but
its refutation's, and that can explode per frame of slack - measure on the
room where the bound is loosest. With the rem rungs went the kernel re-run
backward (the BFS's oracle, which found every graph bug of 2026-09) and its
pinned marks gate.

**The count-up's memo keyed on the level's row key** (`arc-sets`, fixed
`522de36`). The DFS remembered fully explored concrete states by
`Block::keys()`, the row key AT THE SEARCH'S LEVEL - remainder widened, held
buttons unknown at `h` - so two different concrete states shared one entry,
and a state that could win was skipped as "explored": "no concrete win
within f" was not a proof. Every count-up refutation before the fix (room
(5,3) any% 78, nodiag 92-95) had to be rechecked. A cache in front of an
exact check must be keyed EXACTLY; the widening it inherits is refuted by
nothing.


## Measurements moved out of code comments (tier 3, 2026-10-05)

The code's comments used to carry the story behind each design; the
design-deciding numbers are kept here, one line each.

- **Lane type as a register** (`ZN`, kernel.rs, 2026-08-23): as
  `[Pico8Num; 16]` the frame kernel was 61% scalar instructions (the loop
  vectorizer got 39% of the way); converting one primitive to AVX-512 while
  the rest stayed arrays was 27% SLOWER; changing only the lane type was 20x
  on instructions, runtime and build time.
- **`RowCache` is bounded** (2026-09-14): the unbounded open-addressing set
  it replaced grew to MBs per unit, missed the cache on every probe and was
  a third of the append loop; the fused graph re-emits each row ~8x from
  neighbouring lanes, which an L2-resident table catches.
- **Debuginfo off for celeste-engine in release**: line tables cost 6.1% on
  the f35 one-frame bench (103.6 vs 109.9 ms).
- **The global BDD analysis** (one table for the whole graph): its 2^22-node
  cap filled on the first formulas and 5.5-8.3k boolean nodes per kernel
  went unanalysed at nearly all the build's CPU. `bdd::simplify_local` (a
  small BDD per node over its bounded cone, 2026-09-15) finds more (room
  (1,0) player shape 11,844 -> 7,569 fused nodes) at ~1/150 the time;
  expansion 4-32 finds the same, 64+ overflows the local table.
- **Merging a node into any same-BDD node of its cone** (plans/graph-audit.md):
  the proof was wrong; the preservation gate caught it losing a decided
  lane. Only the common-factor shape survives (counterexample in bdd.rs).
- **`State::ended` per path**: frame-global (OR-ed into every outcome) took
  room (3,0)'s largest kernel from 78k to 3.3M configurations (2026-09-27).
- **Own errors scoped to the path's decisions, not the guard**: the guard is
  exact but makes every fork site's guard a kernel root (room (3,0)'s
  largest: 456k -> 1.41M nodes).
- **A merge selects on the separating decision, not the guard**: on the
  guard one two-valued `freeze` became 37 nodes and 37 bodies.
- **`merge_inner` reads both sides only once the merge is certain**: copying
  and GC'ing both heaps per attempt took room (3,0)'s fruit-unknown walk
  21 s -> 415 s (2026-09-17).
- **Error derivation in one bottom-up pass**: per-outcome cone walks were
  outcomes x graph (room (3,0) trace 25 s -> 448 s). Exact (Kleene-lazy)
  derivation cost +23% frame time on room (1,0) f0-f44; dropping "where it
  was evaluated" (strict) declined room (1,0) at f25. Plain `abstractness`
  for an own error added 1M fused error nodes in room (3,0)'s largest kernel.
- **`flr_ways` capped**: the region's full speed range made move forks
  13-way (room (3,0) square (5,13): 1,236 -> 35,502 bodies).
- **Near-level countdown atoms not forked on escape**: forking them took
  room (1,1) `r0sxhn` from 10 to 14 forks a kernel, 12k -> 29k bodies, 1.7x
  forward time, same states.
- **Split pass order and targets**: splitting the guard's selects gave room
  (6,0) 50,000 splits for 112 outcomes; splitting the error gave a 41-node
  row under a 5M-node error; anything but largest-cone-first churned 20k
  splits over ~20 outcomes. `absorbed`: room (6,1) bodies 287,705 ->
  140,323 but fused nodes only 11.6M -> 10.7M (~80% are `live`/`error`
  cones). An unmemoised `may_answers` was 14.7 s per trace.
- **Near floors store the cart's invariant where overlapped**
  (`widen_near_floors`): storing the computed `state`/`collideable` made the
  split pass resolve each floor's whole update (room (6,1): 3,638 outcomes,
  344k bodies, four regions past the 4,096 cap); storing "hidden" and owing
  it gave 108 outcomes (5,454 bodies). Room (7,0) level 0 f54: erasing the
  floor delays merged 3.1x, all floor fields 4.6x.
- **Split frame, middle of the frame**: widening a floor's `collideable`
  inside the player's probe window let step 2 read a floor step 1 had
  decided as both (room (2,1) `r0sxhn` f34: 496k states against 387k).
