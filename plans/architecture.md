# Architecture (the current design, 2026-10-04)

The load-bearing design: what the search is, the invariants every change must
keep, and where it is going. Results are in `plans/results.md`, the level
flags in `plans/abstractions.md`, abandoned approaches in `plans/lessons.md`,
the cost-to-go filter in `plans/level-minus-one.md`.

## Status, honestly

The code is ~56k lines of Rust (35k code, 12.6k comments, 8.8k tests). The
2026-08-30 target was ~10-12k; it was never met, and a cleanup toward ~10k is
in progress on other branches (deleting the diagnostics, the speed buckets, the
rem rungs). Every room of the game has a confirmed optimum
(`plans/results.md`).

The direction (Philippe, 2026-10-04): **the rotation graph ("arcs") becomes the
only treatment of the sub-pixel remainder**, replacing the rem rungs of the
precision ladder. The remaining ladder is over the objects and the held
buttons only. See "Arcs" below. Code for the speed buckets, the position rung
and the rem rungs is described here only as far as it still runs.

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
   (`RefEngine`, which crosses `compiled::bridge` - the only module naming both
   `State` and `Rt2` - at its edge). The loop never touches a `State`.
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
crates/celeste-interp    the interpreter's State model, the abstraction layer   deps: core
                         (abstraction::Level and its widenings), game_runner
crates/celeste-engine    Rt2 block model, boundary/keys, lane primitives        deps: core, names
.  (celeste-rust)        everything else                                         deps: all
  src/frame.rs           Block, FrameStep, ForwardSink, ForwardState (extend),
                         Ladder / find_optimum, MarkFilter, the kernel re-run
                         backward (backward_run, the BFS's oracle)  ~3.9k lines
  src/search/            checkpoint, door, edges (recorded graph + BFS),
                         pos_graph, arc_edges / arcs / arc_dp (the rotation
                         graph), ui_export
  src/trace/             the AST tracer (Lua -> transpile::graph::Graph), the
                         constant-lattice walk (kernel.rs), widen.rs, verify.rs
                         (split pass), error.rs, level_minus_one.rs, refengine
  src/transpile/         graph IR, lowering (lower::specialize_frame), ASM
                         assembler (transpile::asm)
  src/compiled/          FrameEngine, the kernel registry and append step,
                         the State <-> block bridge
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
can reach it (`widen::PLAYER_PROBE`); mid-frame rows pass the mark filter
(`frame::mid_frame_rt2`). With platforms abstract the split still reaches
slightly FEWER states than unsplit in room (2,1) (unresolved).

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
STILL UNCHECKED: interval `Mul`/`Div` by a positive constant (the rem rung's
scale) can still wrap - an open item. Which results ran before the fix:
plans/results.md.

## The frame: waves (2026-09-13)

One pass per frame, no owners, no budget, one barrier.

1. **Units in cell order.** The frontier is the workers' pieces of the
   previous frame, each sorted by (cell, key) and checkpointed in FLUSH order
   (format v9, a run index of `(cell, start, len)` per file). Units of
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
   ladder filter (`MarkFilter`), sort, `Door::admit` under the shard's lock
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

The initial state (`RefEngine::initial_state` -> one bucket) and the
reference oracle are the only places a `State` meets the loop.
`MarkFilter` widens on the columns (`Rt2::widen_to`); exporting buckets
through `State` reached 62.8 GB at H=55.

## The backward: an explicit graph (2026-09-13)

The forward records every edge once; the backward is a BFS that re-runs no
kernel (`search::edges`).

- **Ids.** `frame::pack_id(layer, seq, row)`: the checkpoint file and the
  row in it. The door stores the id with the key, so a re-emission of an old
  state resolves to its id; a resume rebuilds that from the files.
- **Recording.** A queued row carries `(pred_base, pred_mask)`: a 64-lane
  predecessor group and the lanes that produced it; re-emissions OR their bit
  in through the dedup cache, and after the flush the cache holds the door's id
  so later re-emissions go through a direct-mapped `(target, base) -> mask`
  merge. Records (20 B, layer-local) go to `edges/raw/f{frame}/`.
- **Runs.** At each frame's end each layer's records are range-partitioned by
  target, sorted (a parallel counting sort on the target's dense rank above
  512k records), merged, and delta-varint encoded in 256-pair blocks with an
  index: `<level>/edges/l{layer}/f{frame}.bin` (~5 B/pair). The compaction runs
  BEHIND the next frame's wave; `edges/done.txt` names the last complete
  frame, and a resume trusts frames up to it and discards the rest (at most
  one).
- **The BFS** (`edges::bfs`). Seeds: the win rows of EVERY layer <= H (a win
  reached at H is filed under the layer that first reached the state). For
  i = H-1 down to 1, each newly marked state's runs f = layer..=i+1 are looked
  up; lookups parallel, inserts sequential in frontier order (the marks are a
  function of the graph). Iteration i marks exactly the states that win by H
  from frame i but not i+1: i is the state's DEADLINE, kept with the mark.
- **The oracle.** The kernel re-run walk (`frame::backward_run`,
  `CELESTE_BACKWARD=kernel`) stays. `rewrite bench-backward --level-dir D
  --horizon H --diff` prints the two walks' symmetric difference (must be
  empty) and, per disputed state, its recorded edges against its real
  successors (`CELESTE_DIFF_RERUN=1` re-runs the frame with the tree's door).
  It found every graph bug so far (a lookup across an index block, a queue
  index overflowing the row ref's 8 bits, empty pieces renumbered). Run it on
  a real room's tree after any change to the recording.

Room (0,0) end to end: 2 h 32 min on the kernel walk -> 23:50 on the graph,
every ladder fingerprint identical.

## The outer loop

1. **Level 0 persists across horizons** and is EXTENDED one frame per step
   (`ForwardState`): frontier, door, pos graph and first win carry over. It is
   dropped from memory while the finer levels run (`Ladder::drop_level0`,
   ~10 GB idle in room (3,0)) and resumed from disk.
2. **Every level runs TO the horizon** and the backward seeds from the wins at
   every frame <= it (the forward used to stop at the first win, so no horizon
   past it could confirm). Won rows are not expanded.
3. **Finer levels are filtered** by the coarser level's marks (`MarkFilter`:
   project the fine row onto the coarser level with `Rt2::widen_to`, look up
   its key) **with a deadline**: a fine state at frame t is admitted only onto
   a coarse state whose deadline is >= t. Sound (a fine state that wins from t
   widens to a coarse state that does), prunes only states with no winning
   descendant, and turned room (3,0)'s level 1 from an OOM into a 15.7M peak.
   Marks loaded without deadlines (`u16::MAX`) fall back to membership.
4. **The ladder at horizon H**: levels coarsest first; a level with no win by H
   REFUTES H (sound: every level over-approximates). If the exact last level
   wins at H, H is confirmed. A level's first win is a lower bound on the
   optimum. A second forward/backward round at the SAME level reproduces its
   marks exactly (they are closed under predecessors): only precision narrows.
5. **Horizons.** Without a reference, count up from level 0's first win (each
   horizon a ladder). With one (`--ceiling C`, the replayed community TAS),
   count DOWN: C must confirm (a refutation there is an error: the model
   cannot reproduce a known solution), then C-1 is tested until refuted - two
   ladders when the ceiling is optimal. Every room since room (2,0) ran this
   way.
6. **Resume.** A search resumes from its checkpoint directory: level 0's
   frames reload as the forward (frontier minus won rows, `Door::from_shards`
   over every layer, the pos graph, saved per frame since `78fbb60` with a
   `posgraph.frame` marker), horizons with `hNNN/outcome.txt` are skipped, the
   finer levels of the horizon in progress recompute. A fresh forward clears
   its level's `frames/` and `edges/` (a killed run's stale edge runs were read
   against new rows once). Nothing on disk records a level's spec: reusing a
   tree under a changed ladder is on you.
7. **Kernel sets** are keyed by the full level; `CELESTE_KERNEL_SETS=N` keeps
   at most N built sets (LRU, rebuilt on demand, ~35-75 s): needed where sets
   are big (`t` sets ~13 GB each). Fused graphs are dropped after assembly
   unless `CELESTE_ASM_EVAL_CHECK` / `CELESTE_KERNEL_EXPLAIN` (7.3 -> 1.6 GB a
   set).

Win conditions: the room exit (`game_runner::win_room()`, including the wrap
from a row's last room to (0, y+1)); the summit's flag rect
(`frame::win_rect`, `c195f8c`); the orb room's exit WITH the orb
(`frame::orb_required`, `2dc116d`); a synthetic `CELESTE_WIN_AT_XY` /
`CELESTE_WIN_RECT` / `--win-at` for experiments. Rooms past (5,2) start with
`max_djump = 2` (`26a0e54`).

### The ladder is a list of levels

A level is `abstraction::Level { pos, rem, spd, held, fruit, floors,
platforms }`. `CELESTE_LADDER="r0sxhn,r1sxhn,...,rxsx"` gives an explicit
list, each level coarser-or-equal to the next in every coordinate and the
last exact in all. Without it the ladder is rem `Bits(0..=15)` then exact,
everything else exact. The flags, what each widens and its status:
`plans/abstractions.md`. The recipe for object rooms: `plans/results.md`.

## Arcs: the rotation graph (2026-10-04, branch `arc-sets`)

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
   `__split_by_flr` on the player's own `rem`. More than one player split per
   axis in a frame (a platform carrying the player) is refused, loudly.
2. **Backward, arc sets.** `W_t(n)`: the remainder rectangles from which node
   n at frame t wins by the horizon. `W_t(n) = U_e guard_e ∩
   action_e^-1(W_{t+1}(dst_e))`, a win edge contributing its guard.
3. **Forward, exact, inside W** from the start's remainder (a point): the
   first win is the optimum at the node graph's precision; the path is the
   witness.
4. **The remaining ladder is over everything but the remainder**: objects
   (`n` -> exact) and held buttons (`h` -> exact), each rung a remainder-free
   forward filtered by the previous rung's nodes. No rem rungs, no drift.

**Built.**

- `CELESTE_ARC_EDGES=1` on any forward records `search::arc_edges` (50 B per
  record beside the edge runs). `rewrite arc-check` checks it: every edge has
  its record and back, and sampled transfers are probed with the reference
  engine inside and outside the guards (room (1,0) gate; room (3,3) with exact
  objects f1-f80: 207M records, 34k probes, 0 disagreements).
- `rewrite arc-search --level-dir D --horizon H --marked-only [--witness]`:
  the level's remainder-free BFS gives the nodes that can win at all and their
  deadlines; only edges between them are loaded (parallel, streamed, dense).
- `search::arcs::Region`: a set of the torus in CANONICAL form (y slabs with
  x segments): equal sets are equal values. `pull` is the preimage under an
  edge.
- `search::arc_dp::backward`: dense CSR, a parallel pull per node,
  incremental (a frame recomputes only predecessors of changed nodes and nodes
  whose deadline starts). `arc_dp::optimum`: ONE backward gives the optimum -
  the graph is the same at every frame but for the layers, which never bind
  on a walk from the start - so `point ∈ W_t(start)` means "a win within
  H - t". The witness is a greedy walk inside W. Property-tested against
  per-horizon backwards.

| case (room (3,3), quick, `r0sxhn`) | edges | load | backward | total | peak |
|---|---|---|---|---|---|
| synthetic win (19,23), h88 | 66M | 4.3 s | 1.6 s | 6.4 s | 5.6 GB |
| same, h110 | 324M | 33 s | 17.5 s | 55 s | 27 GB |
| real exit, h171 | 346M | 66 s (38 s reading 139 GB of records) | 5.7 s | 74 s | 29 GB |

The synthetic case answers f69, as the full object ladder does. Room (3,3):
171 refuted, 172 wins; W stays small (median 1-6 rectangles per node, max
~300). Six ladder attempts had failed there because every rem rung drifted
(marks 2-2.6x per rung; plans/lessons.md).

**Direction and open.** Make arcs the only remainder treatment. Costs to
attack first: the arc records (50 B per edge, written before the marks know
which edges matter: 139 GB and a 37 s read for room (3,3)), the marks BFS and
graph build, and the object/held ladder on top of exact remainders. Open: W's
fragmentation (the number that decides the cost), time-indexing W only where
it changes, moving platforms (two player moves in a frame).

## Gates

Every change to the loop, the kernels or the tracer must reproduce the
three pinned room (1,0) oracles (commands and comparison in CLAUDE.md):
`gates/ckhash_room10_f000-044.txt` (frontier sets f0-f44),
`gates/posgraph_room10_f044.txt` (pos-graph edges), and
`gates/marks_room10_win9-101_h29-33.txt` (the backward's marked sets per
horizon and level, ending `OPTIMAL win frame: 33`). Plus the quick suite,
`kernel lanes: missed 0`, and for the recording `bench-backward --diff`, for
arcs `arc-check`. A kernels-against-interpreter check: `rewrite ref-check`
(row by row; note ckhash equality with the reference engine is a test only at
an exact level - at Bits(0) the kernels over-approximate where the reference
splits the interval).

## Deferred follow-ups

1. **One file per (shape, cell), dispatch key inside** (Philippe,
   2026-09-15): blocks per dispatch key with a key -> range index; count files
   and keys per frame on a real tree first (the per-shape layout exists because
   of the inode blow-up). Mostly moot if the speed buckets go.
2. **The in-kernel coarser key**: the rung-k kernel emits the row's key widened
   to the coarser level, so `MarkFilter` is one lookup per row instead of a
   clone + widen + rekey (was ~30% of a level-1 frame).
3. **Overflow in interval `Mul`/`Div` by a constant** is unchecked (it can
   wrap silently, as Add/Sub did before `b47b118`): give it a `NoWrap`-style
   own error.
4. **A batch-invariance test** (`a_lanes_key_does_not_depend_on_its_
   neighbours` went with the old sweep) and a new-path widening-soundness
   check (`diag-project` checks one projection; `ref-check` the kernels).

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
