# Architecture (the current design, 2026-10-05)

The load-bearing design: what the search is, the invariants every change must
keep, and where it is going. Results are in `plans/results.md`, the level
flags in `plans/abstractions.md`, abandoned approaches in `plans/lessons.md`,
the cost-to-go filter in `plans/level-minus-one.md`.

## Status, honestly

The code is ~36.5k lines of Rust (25.6k code, 4.4k comments, 4.7k tests;
47.5k before `arc-only` deleted the rem ladder and the kernel re-run
backward, 44.9k before the `tier3` cleanup). The 2026-08-30 target was
~10-12k; a cleanup toward ~10k is in progress. Every room of the
game has a confirmed optimum (`plans/results.md`), found by the precision
ladder this branch deleted.

**The search is the arc pipeline** (Philippe's direction, 2026-10-04; built
2026-10-05): the rotation graph ("arcs") is the only treatment of the
sub-pixel remainder; what a level widens beyond it (the objects, the held
buttons) is refuted by an exhaustive concrete search inside the winning
sets, and where that is too loose by a finer level filtered by the coarser
one (the objects ladder, room (7,0)). No rem rungs. See "The search" and
"Arcs" below.

## The whole system in one line

The **search** drives a **frame step** over **blocks**; the frame step is the
compiled **kernels** (or the reference **interpreter**, the oracle); the
kernels come from the **tracer** via one hand-off.

## The three interfaces - the only things that cross between parts

1. **The frame step** (`frame::FrameStep`). A range of input lanes of one
   block in; every output row out through a `ForwardSink`, each row already
   carrying its KEY and its CELL (player position), already widened for the
   level. Branching, widening and keying happen inside. Two impls: the
   compiled `compiled::FrameEngine` and the reference `trace::refengine`
   (`RefEngine`: the tracer's interpreter over `RefDomain`, one lane and one
   fork path at a time; `trace::refbridge` turns a block's lane into its
   state and each leaf back into a one-row block, boxes and all, so its rows
   key like the kernels'). The loop never touches an interpreter state.
2. **The block** (`Rt2` in `celeste-engine`, `frame::Block` = an `Rt2` with its
   key column and its rows' ids). Columnar; key and cell are exposed, the
   fields are opaque columns. Serialized whole (`search::checkpoint`).
3. **The kernel set** (`compiled::asm_kernel::Registry`). The tracer's whole
   output for one level: one assembled AVX-512 kernel per (shape, region),
   built at startup with gcc + dlopen. There is no checked-in kernel artifact.

To the outer loop a state is OPAQUE: it touches a row's key (dedup, door,
checkpoint, marks) and its cell (wave order, sharding, the pos graph, level
-1). The frame step is the only thing that opens a state.

## Code layout

```
crates/celeste-core      pico8_num, cart_data, collision_cache, ids, builtins   deps: -
crates/celeste-names     FROZEN name tables (FIELD_NAMES order = canonical      deps: -
                         field order; append only)
crates/celeste-engine    Rt2 block model, boundary/keys, lane primitives        deps: core, names
.  (celeste-rust)        everything else                                         deps: all
  src/frame.rs           Block, FrameStep, ForwardSink (queues, door, edge
                         records with transfers), forward_frame,
                         ForwardState (extend, resume), MarkFilter  ~2.7k lines
  src/search/            checkpoint, door, edges (recorded graph + BFS +
                         transfer tables), arc_edges (the transfer decode),
                         arcs (remainder sets), arc_dp (THE SEARCH: load,
                         backward, optimum, concrete search), pos_graph,
                         ui_export
  src/trace/             the AST tracer (Lua -> transpile::graph::Graph), the
                         constant-lattice walk (kernel.rs), widen.rs, verify.rs
                         (split pass), error.rs, level_minus_one.rs, the
                         reference engine (refengine, refdriver, refbridge)
  src/transpile/         graph IR, lowering (lower::specialize_frame), ASM
                         assembler (transpile::asm)
  src/compiled/          FrameEngine, the kernel registry and append step
  src/abstraction.rs     abstraction::Level (a level's object flags)
  src/game_runner.rs     the start room, the win room
  src/bin/               rewrite (search + tools), transpile (probes, level -1),
                         concrete_run
```

The traced program is `lua/builtin_level_3.lua` + `lua/builtin_level_4.lua` +
`lua/celeste-minimal.lua` (`cart::sources_in`). Measure THAT, not the OCaml
project's reference cart (`celeste_ocaml/celeste.lua`), which is right for
semantics and wrong for counting anything. `CELESTE_SPLIT_FRAME=1` loads
`lua/celeste-minimal-split.lua` instead: `_update` as two steps around the
player's move (part a: timers, freeze, objects up to and including the
player's `move`; part b: the player's update - the buttons - and the rest), so
a "frame" is two search steps (step 2k is frame k). Kernels shrink ~10-20x
(room (6,0): 1.13M bodies / 15.9M fused nodes -> 46k / 3.1M) with identical
states at frame boundaries (room (6,1) f0-f45, ckhash-equal). Two fidelity
fixes made that true: a global assigned nil is REMOVED, as Lua does
(`__frozen = nil` had left a slot that shifted every heap pointer), and the
cut keeps a floor's computed `collideable` wherever the player's `is_solid`
can reach it (`widen::PLAYER_PROBE`). With platforms abstract the split still
reaches slightly FEWER states than unsplit in room (2,1) (unresolved). The
search runs it (2026-10-08): the tree, the arcs, the marks and level -1 count
STEPS (`frame::steps_per_frame`; `--to`/`--ceiling` stay frames), each step's
transfer decoded as usual (part a holds the player's one split, part b
none); the arc optimum in steps s bounds the game at ceil(s / 2) frames; the
concrete search steps WHOLE frames (`RefEngine::frame`, both parts under the
frame's input) and looks a state up in W at step 2k, a frame boundary.

## The tracer and the kernel model (from the 2026-09-27 graph model)

Written after a day of failing to shrink room (6,0)'s kernels by local
rewrites (plans/lessons.md); the value is in WHY.

**Two stages.** (1) Tracing: one frame of the Lua becomes a set of OUTCOME
graphs with guards (a branch that kills an object changes the heap shape, so
one graph cannot return both); with concrete inputs every operator yields a
concrete output. (2) Making it executable under abstract inputs: an input
TYPE denotes a set (a singleton, an interval, `{true, false}`, unknown).
Interval arithmetic still yields one value; only where an abstract value
reaches a branch-like node is the graph no longer executable, and stage 2
forks until it is. In the code the two stages are collapsed: the tracer is an
interpreter (`state::split` at an undecidable branch, `state::merge` rejoining
with a `Sel` per differing slot), so the graph's shape depends on the level and
the lattice walk re-traces per (shape, region, level) - room (6,0) does 914
traces before a kernel is built. This is the main structural divergence left.

**Abstractness is not the fork trigger.** A lane is itself a set of states,
and the kernel computes on interval representations. What forces a fork is
LANE-UNDECIDABILITY - the condition differs across the concrete states one
lane stands for. `TileFlagAt`/`Mget` take coordinates as registers (one answer
per lane); `Flr` of an interval is lane-decidable because its own error says
the span is one integer. Using abstractness as the trigger minted 50-90 extra
forks a frame in room (6,0). For a `Sel`, a NUMERIC select straddles only if
its arms do; a BOOLEAN select straddles exactly when its condition does.

**One fork concept.** Choose a node, partition its set, duplicate the
downstream cone per part, fold. Splitting a boolean substitutes constants,
an interval substitutes fragments (`Op::Split*`, `IntFrag`). Prefer splitting
the UNKNOWN the predicate reads over the predicate: two predicates of one
unknown split independently admit combinations no concrete value gives (the
"lost correlation" behind room (7,0)'s phantom raise, below). Buttons, held
trails and escaped atoms are 2-way forks every lane takes both ways
(`Symbolic::both_values`, since 2026-09-28; there is no separate button
dimension). `verify::split_undecided_selects` runs after tracing: find a
select driven by something lane-undecidable, split the outcome at the most
upstream place, refold, dedupe outcomes with equal rows (errors combined per
lane), repeat. For the platforms it carries per path the set of (world, player
pixel) POINTS its answers are consistent with (`verify::Points`), so a
comparison one answer at every point is decided without a split.
`lower::specialize_frame` then enumerates, per outcome, only the forks in its
own cone, resolves every-lane-both-ways forks into classes with root dedup, and
takes a dead grid fork once (`graph::ANY_VALID`).

**Error is derived, never carried** (`trace::error`, step 4 of the model,
2026-09-27; there is no `State::ok`). `error(n) = own_error(n, args) or
OR(error(args))`, materialized once, after fusion:

| operator | own error |
|---|---|
| `Flr(x)` | `flr(lo x) != flr(hi x)` (the span claim) |
| `Sel(c, t, f)` | `not Known(c)` |
| a fork fragment | `not SplitOk(ways)` / `not SplitOkTab`: the lane spans more parts than enumerated |
| an unrolled loop | its condition still holds after the bound (`State::ended`) |
| interval `Add`/`Sub`/`Neg` | `not NoWrap(op)`: an endpoint overflowed 16.16 on this lane (`b47b118`; skipped where the static ranges bound the result) |
| a widening | containment of the slot it writes (`widen::SlotErrors`), only where stored |
| the frame | inputs outside the kernel's admissible set (pins, region bounds) |

An own error holds where its operator was EVALUATED (`own and at`, `at` the
path's decisions at its site, `Symbolic::evaluated`): the kernel evaluates
every node on every lane, and off-path operands are garbage (strict
propagation declined room (1,0) at f25). `Div` has no obligation in this
program: all ten divisors are literals. `Known` is no longer a tracer concept;
`Op::Known` survives only inside derived errors. Result: fused nodes room
(1,0) 483k -> 305k, (3,0) 6.24M -> 3.25M, gates unchanged.

**A Lua raise is its own row.** `Interp::poison` (arithmetic or an ordered
comparison on a non-number) records the guard in `Interp::raised`;
`verify::trace_frame` folds them into `Frame::raise`. A raising lane is
ROUTED, not dropped - every lane still ends in exactly one outcome. It fires
only in rooms with a spring standing ON a fall floor ((7,0), (6,1), (7,1)):
`break_fall_floor` -> `break_spring`, and a merge of the fresh `(spr=18,
hide_for=0)` and hidden `(spr=0, hide_for=60)` springs loses their correlation,
admitting `(spr=0, hide_for=0)` where the third arm reads a nil `delay`. Not
reachable concretely. Trace-time refusals (indexing a non-table, a symbolic
table index) are `bail!`s: failures to compile, keyed per (shape, region).

**The kernel.** The compute graph with every fork resolved, two masks per
body, `live` and `error`; a lane emits where `live and not error`. Unknown
`live` reads as live (over-approximate; a finer level refutes); unknown
`error` reads as error (`asm_kernel::read_zb_may`): a declined lane is a
FATAL `KERNEL COVERAGE GAP` for the whole call. Error inside a `live` cone is
global (a wrongly built kernel). `Graph::fold` treats `And`/`Or` as
commutative, so no operand order can protect another.

**The constant lattice walk** (`trace::kernel::room_constant_lattice`):
from the start state, walk (shape, region) nodes in parallel rounds, each
traced against the round's lattice snapshot in a fresh copy of the walk's
arena (reproducible node counts); narrowings re-trace a shape's regions only
once no new region is left. A region (`RegionGrid`, `CELESTE_REGION`, default
16 px) bounds the player's whole-pixel `x`/`y` (and `rem` to [-0.5, 0.5),
speed to [-S, S], S <= 7 because `move`'s loop is unrolled for `abs(amount)
<= 8`), all guarded per lane; the range analysis folds the far objects'
collision tests. A row outside every region has no kernel and stops the run.

**Keys.** The row key is folded per EMITTED row in the append step
(`AsmBody::key_words`: `Σ cell_mix` over the key fields read off the packed
output buffer), checked per row against `Rt2::boundary` by
`CELESTE_KERNEL_KEY_CHECK=1`. A number and its point interval key alike
(`592c72f`). FIELD_NAMES' order feeds the shape hash and every key: a
reordering is a different search.

**Where it still diverges / is open.** The re-trace per level (above). The
platform tension: storing a platform's `x` as its whole path is what lets
frames merge, and the overlap predicates only fold at ~4.5 px granularity -
exactly where no cross-frame merge survives; storing zones would cost ~Z
tuples (one phase drives all ten platforms), the ~18 px middle unmeasured.
**Interval overflow.** Until 2026-10-03 the assembled interval `+`, `-` and
negation WRAPPED an overflowing endpoint silently (plain `vpaddd`/`vpsubd`;
the Rust primitives panicked, and the per-op tests bounded their inputs).
Fixed on `arc-sets` by `549ecf5` (overflow -> the whole range; with the
near-floor changes it exposed) and then `b47b118` (Philippe's design, which
replaced the whole-range rule): an overflowing interval Add/Sub/Neg is a
lane's OWN ERROR (`Op::NoWrap`), so the lane declines loudly; and a countdown
that spans everything (floor `delay`, balloon `timer`, at `n` the spring's
`delay`/`hide_in`/`hide_for`) is stored and read as the unknown number
(`AV::UNum`, `widen::forget_countdown_inputs`), never as the interval [MIN,
MAX]. (`72fdea7` on branch `asm-interval-wrap` was a rejected first version.)
STILL UNCHECKED: interval `Mul`/`Div` by a positive constant (its known
source, the rem rungs' scale, is gone) can still wrap - an open item. Which
results ran before the fix: plans/results.md.

## The frame: waves (2026-09-13)

One pass per frame, no owners, no budget, one barrier.

1. **Units in cell order.** The frontier is the workers' pieces of the
   previous frame, each sorted by (cell, key) and checkpointed in FLUSH order
   (format v10, a run index of `(cell, start, len)` per file; v10 stores a
   mixed column in 9 B a row and a boolean-like one in 1 B, against 16 B).
   Units of
   `unit_lanes()` lanes (1024 since 2026-09-18; `CELESTE_UNIT_LANES`, a
   multiple of 64) are pulled by `threads()` workers (default one per physical
   core, `CELESTE_THREADS`) in cell order across pieces: the wave. A unit is
   also the kernel call's within-call dedup window (`ForwardSink::seen`): 16k
   lanes deduped slightly more, but a few heavy units outlasted the rest
   (room (3,0) f44: 2.23 s / 45% idle at 16k, 1.47 s / 9% at 1024, the same
   kept set).
2. **Queues.** Each worker's `ForwardSink` holds a fixed pool of small queues
   (`POOL_QUEUES` 256 x `QUEUE_ROWS` 256) keyed by (shape, cell); every queue
   of a shape has the shape's skeleton (the union of its outcome templates'
   varying cells). Small queues won: 128-256 rows beat 4096 by 1.9x (the pool
   stays cache-resident); the frame's whole transient is ~60-80 MB.
3. **Flush** (when a queue fills or is evicted, by the worker holding it):
   the level -1 filter, sort, `Door::admit` under the shard's lock
   only, survivors gathered into the worker's piece, the win check, the
   edge records. **A flush is idempotent** - the door is a set - so nothing
   needs to know when a cell is "done"; eviction policy changes only the
   flush count.
4. **The door** (`search::door::Door`): per (shape, cell) a sorted `base`
   (every earlier frame, read-only during the frame) plus a small sorted
   per-frame `delta`, a bucket index on the key's top bits with software
   prefetch, 16 B/entry plus the state's id. `end_frame` merges deltas once,
   in parallel (~1.4 ns/entry). A flush must read `base` (~88% of emitted rows
   are old) and `delta` (the same state twice in one frame would be two
   frontier rows).
5. **End of frame**: the door merges, the pieces are the next frontier, the
   checkpoint is written, the edge compaction runs behind the next wave.

The result is a function of the frame, not of scheduling: the gates are
identical at 1, 16 and 32 threads. Room (1,0) f0-f70: same speed as the
two-phase frame it replaced, peak RSS 4.08 GB against 9.45 GB; room (0,0) to
f90: 14.15 GB against 24.8 GB (44 GB under glibc - mimalloc is the global
allocator, and `safe-run.sh` sets `MIMALLOC_PURGE_DELAY=0`). The wave is
kernel-bound.

### Invariants of the data flow (from plans/buckets.md, 2026-09-12)

1. **The unit of storage is the unit of kernel invocation**: a bucket is
   one shape's packed `Rt2`. Position is a column, not part of the key. (The
   per-class split on freeze / moving key / pm1 cells was the generated
   kernels' premise; dropping it took room (1,0) f0-f44 from 879 kernel calls
   to 44 with every gate identical.)
2. **Rows are routed on emission, never regrouped**: the append step puts a
   row straight into its (shape, cell) queue. No regroup, no merge, no
   per-block `boundary`.
3. **Canonicalization is precomputed per outcome shape**: the accumulator
   template IS the canonical structure.
4. **Provenance is consumed at emission and never stored**: the source lane
   is the slice bit being iterated; the pos-graph edge and the backward edge
   record are written there.
5. **Dedup happens once, at the door.**
6. **Checkpoint per (frame, shape)** (pieces), rows by (cell, key) with a
   cell index, raw fixed-width columns, mmapped: the backward reads a cell's
   rows as a range. (One file per cell-uniform block was ~17k files a frame;
   zstd-bincode layers cost the H=89 backward 35 s per iteration to decode.)

The initial state (`RefEngine::initial` -> one bucket) and the
reference oracle are the only places the interpreter meets the loop. A row's
projection onto a level is on the columns (`Rt2::widen_to`); exporting
buckets through `State` once reached 62.8 GB at H=55.

## The recorded graph (2026-09-13; transfers 2026-10-05)

The forward records every edge once, with what the frame did to the
remainder; every backward reads the records and re-runs no kernel
(`search::edges`).

- **Ids.** `frame::pack_id(layer, seq, row)`: the checkpoint file and the
  row in it. The door stores the id with the key, so a re-emission of an old
  state resolves to its id; a resume rebuilds that from the files.
- **Recording.** A queued row carries `(pred_base, pred_xfer, pred_mask)`: a
  64-lane predecessor group, the TRANSFER of those lanes (the worker's
  interned id of the (x, y) pair the kernel decoded, `ForwardSink::xfer_id`)
  and the lanes; a re-emission ORs its bit in only under an equal transfer,
  else it is an extra entry; after the flush the cache holds the door's id
  so later re-emissions go through a direct-mapped `(target, base, transfer)
  -> mask` merge. Records go to `edges/raw/f{frame}/`, one per lane (16 B,
  layer-local: target, source, transfer), each worker's transfer table
  beside them.
- **Runs.** At each frame's end the workers' tables merge into the frame's
  (`edges/xfer/f{frame}.bin`, sorted by value: a function of the frame, not
  the scheduling), and each layer's records are range-partitioned by target,
  sorted by it (a parallel counting sort on the target's dense rank above
  512k records), and encoded in 256-edge blocks with an index:
  `<level>/edges/l{layer}/f{frame}.bin` (run v5, 2026-10-07). Per edge a
  varint head (a new target's delta, or the source's delta under the same
  target), the source as a DENSE number (the source pieces laid end to end,
  a table in the header) when the target is new, and the transfer's RANK in
  the run (its transfers by descending use, a table in the header): ~4 B an
  edge, against 8.8 B a v4 record (lanes per record: 1.006) - room (2,3)
  gemskip f0-f137 8.27 -> 4.00 GB, room (1,0) f0-f44 433 -> 292 MB. A run is
  read in place (mmap), index and tables included. The compaction runs
  BEHIND the next frame's wave; `edges/done.txt`
  names the last complete frame, and a resume trusts frames up to it and
  discards the rest (at most one). Room (1,0) f0-f44: 408 MB of runs against
  216 MB without transfers and 2.8 GB for the separate arc-record stream it
  replaced (`d7c373a`).
- **The BFS** (`edges::bfs`). Seeds: the win rows of EVERY layer <= H (a win
  reached at H is filed under the layer that first reached the state). For
  i = H-1 down to 1, each newly marked state's runs f = layer..=i+1 are looked
  up; lookups parallel, inserts sequential in frontier order (the marks are a
  function of the graph). Iteration i marks exactly the states that win by H
  from frame i but not i+1 - with SOME remainder: i is the state's DEADLINE.
- **Checks.** `rewrite arc-check` (every record has a transfer; sampled
  transfers probed with the reference engine inside and outside their
  guards). The kernel re-run backward that was the BFS's oracle
  (`bench-backward --diff`, which found every graph bug of 2026-09) is gone
  with the ladder (2026-10-05); the arc gate pins the BFS's marks.

## The search (2026-10-05, branch `arc-only`)

`rewrite search --to H | --ceiling H [--level SPEC[,SPEC...]]`, per level of
the list (coarsest first; one level is the usual case):

1. **[Level -1]** (`CELESTE_LEVEL_MINUS_ONE="H,S"`, plans/level-minus-one.md):
   cells that provably cannot exit by H are dropped at the flush.
2. **The forward** at the level (default `r0sx`; `r0sxhn` for an object
   room), to H, into `<dir>/level{i:02}` (`ForwardState`: resumed from its
   checkpoints, or used as it is when it already reaches H). Won rows are
   checkpointed (backward seeds) and not expanded. **A finer level is
   filtered** (`frame::MarkFilter`): a row at frame t is kept only if its
   projection onto the previous level is ARC-MARKED there (its winning set
   non-empty at some frame) with a deadline >= t. Sound because the remainder
   is exact at both levels: a fine state that wins from t with remainder r
   projects to a coarse node that wins from t with r. This is the OBJECTS
   LADDER (below), the only ladder left.
3. **The arc phase** (`arc_dp::solve`): the remainder-free BFS marks the
   nodes that can win by H at all, with their deadlines; only the edges into
   them, from them, are loaded (`preds_at`, the BFS's lookup), each with an
   index into the merged transfer table; `arc_dp::backward` computes the
   winning sets `W_t`; `arc_dp::optimum` reads the optimum off them. That
   optimum is exact in the remainder and over-approximates the level's other
   widenings (and `rnd`): a LOWER BOUND, and no win REFUTES H.
   MEMORY (2026-10-07, `[mem]` lines at every phase boundary): a node is the
   rank of its mark in the BFS's own bitmaps (`edges::MarkRanks`, no hash
   map); the edges are read twice, counting then filling the two adjacencies
   in place (12 B an edge, nothing held beside them); W is kept as SPANS
   (node, frames, set: 12 B per unchanged run) into an arena of the distinct
   sets instead of a table per frame of `Arc`s; one pass over the frame
   files yields the gate's fingerprints, the marks files and the concrete
   search's sorted node keys (no hash maps). Room (2,3) gemskip h137, level
   0 (18.6M nodes, 373M edges, 18.5M spans over 2.9M distinct sets): arc
   phase peak 14.2 -> 8.1 GB anonymous, 20.2 -> 9.3 GB with the mapped runs;
   W 4.4 -> 0.9 GB; the graph's build 12.1 -> 5.4 GB. Level 1 (22.9M nodes,
   58.5M spans over 0.65M sets): peak 17.6 -> 7.2 GB, 2.6 GB left for the
   concrete search (17.0 before).
4. **The concrete search** (`arc_dp::concrete_search`) over concrete states
   (the reference engine's concrete step, every input, every `rnd` leaf) from
   the room's start, admitting a successor only if its projection onto the
   level is a node whose W holds its exact remainder, and holding each EXACT
   state once. W holds every concrete winner, so the search is exhaustive
   inside it and prunes by nothing else. First a depth-first try for a win AT
   the bound (`W_{H-bound+k}`, one engine, 200k steps at most: where the bound
   is the optimum it takes a few hundred); then a breadth-first search inside
   `W_k` (aligned to H), layers expanded in parallel: the first layer with a
   win is the CONCRETE optimum and its path the witness
   (`<dir>/witness_frame_F.txt`). With `--ceiling` a refutation or no witness
   is an error (a known solution the model cannot reproduce). Its cost is the
   number of concrete states inside W, which the level's widenings decide:
   see "Validation". At a level that is not the last only the try at the
   bound runs (a win there is the optimum); otherwise the next level.

Resume: rerun the same command; the forward resumes or is reused, the arc
phase reruns (minutes). A fresh forward clears `frames/` and `edges/`.
`CELESTE_TRIM_ROWS=1` (2026-10-07) trims every frame's checkpoint files to
keys, cells and wins once a later frame's runs are complete
(`checkpoint::trim`): the search, a resume and `export-ui` read nothing
else of an old frame, and the rows' values are most of a tree's frames
(room (2,3) gemskip h137, level 0: 3.03 -> 0.78 GB; the tree 7.0 -> 4.8
GB). The diagnostics that load old rows refuse a trimmed tree, so it is off
by default.
Nothing on disk records the level: reusing a tree under another `--level` is
on you. One kernel set is resident, the process-global level's; a call at
another level drops it and builds that level's. Fused graphs are dropped
after assembly unless `CELESTE_KERNEL_EXPLAIN`.

**The objects ladder, and when it is needed.** Validation (plans/results.md,
"The arc pipeline"): rooms (1,0) 99, (4,2) 71, (5,3) 79, (3,3) 172 and
(7,0) 84 reproduce their known optima, each witness replayed on a real
PICO-8. Where the bound is the optimum ((1,0), (4,2), (3,3)) the concrete
search takes a few hundred steps; (5,3) (bound 78) refuted 78 in 7.2k steps
and found 79 in 165k. Room (7,0) at `r0sxhn` alone is where it BLOWS UP:
bound 80 against 84, and the exhaustive region grew 6.5x a frame of slack
(172k concrete steps at f80, 1.29M at f81; ~300M to reach 84) - the
abstract objects let too many concrete states into W. An exact-objects level
0 (`r0sxh`) does not run either (48.8M states kept at f55 against `r0sxhn`'s
2.6M). The ladder `r0sxhn,r0sxh` does: the filtered `r0sxh` forward took
6.9 s, its bound is 84 and the try at the bound found the witness in 292
steps (14:40 for the room). Room (4,3) nodiag the same way: `r0sxhn` bound
104 (no concrete win there, 18.7k steps), then its breadth-first search grew
~1.4x a layer - 2.0M steps and 20k states by layer 44, 4.2M steps and 29k
states by layer 46 of 111, at the 40 GB cap; `r0sxhn,r0sxh` bound 106 and
the witness at 106 in 392 steps - five frames under the community TAS,
replayed on PICO-8. So: one level where the bound is tight, the objects
ladder where it is not; a level-0 bound well below the reference is the
sign.

**What deleted (2026-10-05).** The rem rungs (`RemPrecision`, the bucket
widening and fork grid, the rem-keyed kernel sets), the precision ladder's
driver (`Ladder`, `find_optimum*`, `CELESTE_LADDER`, `--maxk`; the mark
filter came back as the objects ladder's link, now on arc marks), the kernel
re-run backward (`backward_run`, `CELESTE_BACKWARD=kernel`, `bench-backward`),
`rewrite witness`, `arc-search`. Why: plans/lessons.md "The precision
ladder".

Win conditions: the room exit (`game_runner::win_room()`, including the wrap
from a row's last room to (0, y+1)); the summit's flag rect
(`frame::win_rect`, `c195f8c`); the orb room's exit WITH the orb
(`frame::orb_required`, `2dc116d`); a synthetic `CELESTE_WIN_AT_XY` /
`CELESTE_WIN_RECT` / `--win-at` for experiments. Rooms past (5,2) start with
`max_djump = 2` (`26a0e54`).

### A level is a set of object flags

`abstraction::Level { held, fruit, floors, platforms }`, spec
`r0sx[h][f][n][p]` (the remainder's rung 0 and the exact speed are in every
spec; `r1sx`/`rxsx` are refused). The flags, what each widens and its
status: `plans/abstractions.md`.

## Arcs: the rotation graph (2026-10-04; the only remainder treatment since 2026-10-05)

Stop refining the sub-pixel remainder rung by rung; track it exactly.

**The model (checked).** Per axis one frame does `rem := rem + ox + 1/2;
amount := flr(rem); rem := rem - 1/2 - amount` (`move`), then steps
`|amount| + 1` pixels with a collision check each (a blocked step sets
`rem := 0`, `spd := 0`). Mod 1 that is a ROTATION of the circle by `ox`, and
`amount` steps by one where the rotated value crosses the wrap. So for a fixed
remainder-free state (a NODE) and input, the circle is cut at one point per
axis into two PIECES; inside a piece the frame does one discrete thing (one
target node) and maps the remainder by a rotation, or to the constant 0 (a
collision). The applied `ox` is the speed at the player's move, which is not
always the stored `spd` (a freeze moves nothing; a spring updated earlier in
the frame may set it). `rewrite arc-proto` held this on room (1,0)'s gate
through layer 28 (1.2M concrete steps), spawn and freezes included. A path is
real iff the start remainder lies in the intersection of its guards pulled
back through its rotations, and "intersect, then rotate" distributes over
unions, so the union over all paths is pushed exactly (`search::arcs`).

**The design.**

1. **Forward, remainder-free** (level 0's forward as today). Each emitted row
   carries, per axis, the remainder after `rem -= 1/2 + amount` (the image)
   and at the frame's end before the widening (final), recorded with the edge,
   not in the row (keys unchanged): guard = the image's width at the end it
   touches; action = a rotation by `image.lo - guard.lo`, or the constant
   `final` on a collision; identity where the frame has no player split on the
   axis. Captured as per-body transfer roots where the tracer forks
   `__split_by_flr` on the player's own `rem`. A second player split on an
   axis in one path is refused, loudly - and cannot happen: `move` is the
   only splitter and `_update` calls it once per object. A moving platform
   carries the player with `move_x` (whole pixels, no split, `rem`
   untouched) - unless a wall blocks the step, which sets `rem.x = 0` before
   the player's own move (platforms precede the player in `objects`); that
   split's argument is then one point, decoded as `Const(fin)` from the whole
   circle (`arc_edges::set_before_the_move`). (Until 2026-10-08 this was
   written up as "two player moves in a frame", and the platform rooms were
   believed blocked by it; they were blocked by the `p` level itself, see
   plans/abstractions.md.)
2. **Backward, arc sets.** `W_t(n)`: the remainder rectangles from which node
   n at frame t wins by the horizon. `W_t(n) = U_e guard_e ∩
   action_e^-1(W_{t+1}(dst_e))`, a win edge contributing its guard.
3. **Forward, exact, inside W** from the start's remainder (a point): the
   first win is the optimum at the node graph's precision (the bound); the
   concrete search (above) makes it the game's.

**Built.**

- The transfers are always recorded at every level (they used to be a
  separate 50 B-per-edge stream behind `CELESTE_ARC_EDGES=1`; since
  `d7c373a` they are part of the edge record). A body without transfer roots
  is refused at the kernel build.
- `search::arcs::Region`: a set of the torus in CANONICAL form (y slabs with
  x segments): equal sets are equal values. `pull` is the preimage under an
  edge.
- `search::arc_dp::backward`: dense CSR, a parallel pull per node,
  incremental (a frame recomputes only predecessors of changed nodes and nodes
  whose deadline starts). `arc_dp::optimum`: ONE backward gives the optimum -
  the graph is the same at every frame but for the layers, which never bind
  on a walk from the start - so `point ∈ W_t(start)` means "a win within
  H - t". Property-tested against per-horizon backwards.

Measured on `arc-sets` (the separate record stream, before `d7c373a`):

| case (room (3,3), quick, `r0sxhn`) | edges | load | backward | total | peak |
|---|---|---|---|---|---|
| synthetic win (19,23), h88 | 66M | 4.3 s | 1.6 s | 6.4 s | 5.6 GB |
| same, h110 | 324M | 33 s | 17.5 s | 55 s | 27 GB |
| real exit, h171 | 346M | 66 s (38 s reading 139 GB of records) | 5.7 s | 74 s | 29 GB |

The synthetic case answers f69, as the full object ladder does. Room (3,3):
171 refuted, 172 wins; W stays small (median 1-6 rectangles per node, max
~300). Six ladder attempts had failed there because every rem rung drifted
(marks 2-2.6x per rung; plans/lessons.md).

**Room (1,0) sanity (2026-10-04)**: against the known truth (the full rem
ladder: 99 optimal, 98 refuted at Bits(6)). Level-0 forward `r0sx` to f99
with `CELESTE_ARC_EDGES=1` (quick): 6:58 wall, peak RSS 10.4 GB, first
remainder-free win f89; 289 GB on disk, 240 GB of it `edges/arc/`.
`arc-search --marked-only --witness` (the old separate stream; `search`
does this now):

| horizon | answer | edges / nodes loaded | load (records) | backward | wall | peak |
|---|---|---|---|---|---|---|
| 99 | OPTIMAL f99 | 214M / 8.8M | 217 s (174 s) | 8.4 s | 5:43 (witness keying 114 s) | 18.2 GB |
| 98 | REFUTED | 154M / 6.7M | 148 s (121 s) | 1.2 s | 2:30 | 13.1 GB |

At h98 the remainder-free marks still reach the start (they would win from
f9), so the refutation is the arc sets' (W empty at the start). W at h99:
median 1-3 rectangles per node, max 37. The concrete witness is
byte-identical to `tas/room_1_0_exit_frame_99.txt`; `concrete_run` and a
real PICO-8 (`pico8_diff/replay.py`) both change room during frame 99.
`arc-check --samples 30` f1-f66 (1.07G records, 7.5k probes inside and
outside the guards): 0 disagreements, every edge with its record and back;
23.5 min, then OOM-killed at f67 under a 30 GB cap (its memory follows the
frame's record count: 13 GB at f58, 30 GB at f67; later frames have up to
2x more).

**Open.** W's fragmentation (the number that decides the backward's cost;
small so far: median 1-6 rectangles per node), time-indexing W only where it
changes. (Moving platforms and the split frame run since 2026-10-08.)

## Gates

Every change to the loop, the kernels or the tracer must reproduce the
three pinned room (1,0) oracles (commands and comparison in CLAUDE.md):
`gates/ckhash_room10_f000-044.txt` (frontier sets f0-f44),
`gates/posgraph_room10_f044.txt` (pos-graph edges), and
`gates/arc_room10_win9-101_h35.txt` (the search at a synthetic win at h35:
the remainder-free marks, W's fingerprint at every frame, the arc optimum 33
and the concrete witness; it replaced the ladder's marks gate on
2026-10-05). Plus the quick suite, `kernel lanes: missed 0`, and for the
recording `arc-check`. A kernels-against-interpreter check: `rewrite
ref-check` (row by row; the kernels over-approximate where the reference
splits an interval, so ckhash equality with the reference engine is no
test).

## Deferred follow-ups

1. **One file per (shape, cell), dispatch key inside** (Philippe,
   2026-09-15): blocks per dispatch key with a key -> range index; count files
   and keys per frame on a real tree first (the per-shape layout exists because
   of the inode blow-up).
2. **The transfer capture costs the forward** ~25-45% CPU (room (1,0)
   f0-f44, `d7c373a`); the kernels compute five roots per axis per body
   where the transfer depends only on the fork configuration and `ox`.
3. **Overflow in interval `Mul`/`Div` by a constant** is unchecked (it can
   wrap silently, as Add/Sub did before `b47b118`): give it a `NoWrap`-style
   own error - or refuse it: its one known source was the rem rungs' bucket
   snap, gone.
4. **A batch-invariance test** (`a_lanes_key_does_not_depend_on_its_
   neighbours` went with the old sweep) and a widening-soundness check
   (`diag-project` checks one projection; `ref-check` the kernels).
5. **The UI** (`ui/`, TypeScript) still carries the ladder's concepts
   (several horizons, kernel re-run backward iterations, the band colours);
   the exporter writes one horizon and leaves those fields empty.

## Where the old plans went (2026-10-04)

Code comments still cite plans/ docs by their old names. The 44 docs were
merged on 2026-10-04 (branch `docs-cleanup`); the originals are in git
history (`git show arc-sets:plans/NAME.md`).

| old doc | now |
|---|---|
| `waves.md`, `buckets.md`, `arcs.md`, `graph-model.md`, `memory.md`, `parallel.md` | this file (the superseded parts: plans/lessons.md) |
| `held-buttons.md`, `fly-fruit.md`, `fall-floors.md`, `platforms-unknown.md`, `bucket-dispatch.md`, `speed-abstraction.md`, `room30.md` (ABSENT_AS_ZERO, the region key) | plans/abstractions.md |
| `regions.md`, `in-frame-widening.md`, `overnight-2026-09-15.md` | plans/lessons.md |
| `overnight-2026-09-19.md`, `room60-overnight-2026-09-28.md`, `room70-2026-09-30.md`, `roomXY-2026-10-0N.md` | plans/results.md (findings: lessons, abstractions) |
| older names (`specialize.md`, `spd-rung.md`, `kernel-ladder.md`, `tracing.md`, `asm-backend.md`, ...) | deleted in the 2026-08-31 tear-out; git history only |
