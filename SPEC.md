# SPEC: what the system promises (contracts), 2026-10-10

Scope: the merged state = `storage-v2` (f3ccf68) + `kernel-opt` (codegen folds,
`CELESTE_KERNEL_MIX`). Written from the code; where plans/ disagree, the code
wins and Annex E lists the disagreement. Main text = contracts. Annexes =
what to cut from: [SIZE] A, [KNOB] B, [FEATURE] C, [JANK] D, plans-vs-code E.

Decomposition (changed from the brief): level -1 and the objects ladder are
one section, "Filters", because they have the same contract (a forward
filter whose soundness comes from an exact or lower-bound argument and which
the known-route check audits). Persistence keeps its own section. The rest
follows the brief.

Dependency order (load-bearing): `celeste-core` (numbers, map, collision) and
`celeste-names` (FROZEN field order) <- `celeste-engine` (`Rt2` blocks, keys,
lane primitives) <- `celeste-rust` (tracer, kernels, storage, search, bins).

---------------------------------------------------------------------------

## 1. Goal and proof obligation

**Problem.** Per room (x,y) and category, find the least N such that some
input sequence makes the room change DURING frame N, counted from the room's
load (frame 1 = first `_update` after `load_room`; the spawn prologue of
23-37 frames is included). Database count = N - prologue - 1.

**Win predicate** (`frame::reaches_win`, `frame::wins_of`):
- the room global becomes `win_room()`: (x+1,y), or (0,y+1) from x=7;
- orb room (5,2): only with `max_djump == 2` (`orb_required`);
- summit (6,3): the player touches the flag rect (`win_rect`);
- 100% (`CELESTE_HUNDRED`): additionally `got_fruit` for this room
  (`wins_of` only, see D.forward-7);
- nodiag (`CELESTE_NODIAG`): the cart is patched so a diagonal dash raises,
  i.e. has no successor;
- synthetic: `--win-at x,y | x0..x1,y0..y1` (`CELESTE_WIN_AT_XY`), for gates
  and experiments.

**Start state.** `_init()` of the traced cart with `load_room(1,0)` rewritten
to the start room; past `ORB_LEVEL=21` `max_djump=2` unless gemskip. Real
play enters a room with the loading-frame jank J (sec. 12); `CELESTE_LOADING_JANK=J`
appends that frame to the start. No held buttons carry over. Balloon/chest
`rnd` are intervals (sec. 2).

**What counts as a proof of optimum N.**
1. Every abstraction the search uses is either exact or over-approximates the
   game (sec. 4), so "no abstract win by N-1" REFUTES N-1.
2. The concrete search (sec. 8) runs the reference engine exhaustively
   (every input, every `rnd` leaf) from the start and prunes ONLY by the arc
   winning sets W, which contain every concrete winner. Its first win layer
   is the concrete optimum; its path is the witness.
3. With `--ceiling C` (a known solution's length), a refutation of C or no
   witness is an ERROR (a bug), never a result.
4. A result is believed only after its witness replays on a real PICO-8
   with the ORIGINAL cart (sec. 12). The project's own "proven 94" for room
   (0,0) was one frame too long.

**Stated caveats.** (a) `rnd`: a refutation holds for every draw, a
confirmation means "some draw wins" (seeds are then fixed by the witness file).
(b) Interval `Mul`/`Div` by a constant may wrap silently in the kernels
(open; Add/Sub/Neg are checked). (c) Most ladder-era results ran on a binary
that wrapped interval Add/Sub (before `b47b118`); the `n` rooms were not all
re-run (plans/results.md "Caveats").

**Witness format.** `tas/room_X_Y_<kind>_frame_N.txt`: `#` header, then one
input byte per frame from frame 1; bits 0..5 = left,right,up,down,jump,dash.
Uploads use the tasdatabase format `[seeds,]i1,i2,...` from the player's
creation frame.

## 2. Game semantics

**Traced program** (`trace::cart::sources_in`): `lua/builtin_level_3.lua`
(`add`, `foreach` with PICO-8 `all` semantics) + `lua/builtin_level_4.lua`
(buttons, `count`, `del`, draw no-ops) + `lua/celeste-minimal.lua` (894
lines). Differences from the original cart (`celeste_ocaml/celeste.lua`, 1428
lines): no smoke/hair/particles/`message`/`flag`/title; NO `jbuffer` (an early
press is lost; claimed not to change optima, so TAS inputs need re-timing,
`rewrite trajectory`); `move` rewritten around `__split_by_flr` (exact in
16.16); compound ops expanded. `celeste-minimal-split.lua` (934 lines) is the
same cart with `_update` cut into two steps (sec. 6).

**Load-time source patches** (string `replacen`): start room, loading jank,
nodiag, balloon seeds. Every consumer (kernels, reference engine, level -1,
platform worlds) reads the patched source.

**Numbers** (`celeste-core::pico8_num`): i32 16.16. Add/sub/neg wrap; mul =
(i64 product) >> 16; div truncates and saturates (x/0 saturates); `%` =
`rem_euclid`, `a%0 = 0`; `sin` from a table dumped from a real console.
Map/flags from `cart/`; `mget` outside the map reads 0.

**rnd.** `rnd(x)` (x constant) is the interval [0, x). Symbolically an
interval node; in the reference engine an interval whose straddling
comparisons fork 2 ways. Uses: balloon `offset=rnd(1)`, chest
`x=start-1+rnd(3)`. `CELESTE_BALLOON_SEEDS` replaces the balloon draw by
constants.

**Reference engine** (`trace::refengine`, `refdomain`, `refbridge`,
`refdriver`): the SAME `Interp<D>` as the tracer over `RefDomain` (scalar
intervals, real bools, no merges), enumerating the fork tree depth-first by
re-execution (cap 1M paths). It is ground truth for: the concrete search's
steps (always `Level::EXACT`), `arc-check`, `ref-check`, `follow`,
`known`, platform worlds. It is NOT ground truth for PICO-8: that is
`trace::probe` (goldens from a real console, `lua/probe/`), `pico8_diff/run.sh`
(builtin differential tests) and witness replay. `refbridge` turns a block lane
into an interpreter state and each leaf back into a keyed one-row block.
Refuses level `p`.

## 3. The tracer: Lua -> graph IR

**Input.** Cart AST (full_moon), a start heap, a level, per-shape pins
(constants from the lattice walk) and bounds (`Restrict`). **Output**
(`verify::trace_frame` -> `Frame`): an interface (input slots by path), a
set of outcomes, a RAISE row, fork metadata, transfer roots. A frame is
`__reset_button_states(); _update(); _draw()` (`_draw` holds the screen
clamp).

**Model.** An interpreter over the AST with a CONCRETE heap (tables,
closures, lengths) and SYMBOLIC scalars hash-consed into one graph
(`transpile::graph::Graph`). Symbolic table indexing is a refusal (`bail!`).
At a lane-undecidable branch the state splits (`state::split`) and rejoins
with a `Sel` per differing slot (`state::merge`). Limits: 256 trace states,
2M nodes.

**Terms** (as the code uses them):
- **shape**: the heap structure; states of equal shape trace identically
  because every non-frozen scalar is symbolic. Three identities exist (D.tr-4).
- **outcome** (`verify::FrameOut`; name disliked, better "successor" or
  "arm"): one guarded successor of a frame = (guard, derived error, end
  fields, end shape, transfer roots). Guards are disjoint and cover every lane:
  each lane ends in exactly one outcome or in the RAISE row.
- **fork**: one concept: choose a node, partition its set, duplicate the
  downstream cone per part, fold. Kinds: `Split/Frag` (from `__split_by_flr`),
  `SplitInt/IntFrag` (held trails, near floors), and 2-way forks every lane
  takes both ways (buttons, escaped atoms). The trigger is
  LANE-UNDECIDABILITY (the condition differs within one lane's set), not
  abstractness.
- **body** (`lower::SpecializedBody`): one fork configuration of an outcome's
  cone, resolved and folded; masks `live`, `error`. A lane emits where
  `live & !error`.
- **row / lane**: a row is one state in a block; a lane is also a zmm slot
  (16 wide). Used interchangeably.
- **block** (`Rt2`): one shared heap structure + per-cell value columns
  (`AV::{Num, Ival, Bool, UBool, UNum, Nil, Ptr, ...}`); `frame::Block` adds
  the key column and ids.
- **key** (row key, `runtime2::boundary_finish`): 128 bits; shape hash plus
  a SUM of `cell_mix(canonical cell id, value code)` over value cells, then
  mixed. A number and its point interval key alike. POSITION-FREE since
  `29d8364`: the position object's x/y contribute only their part past the
  low end's whole pixel. A state is `(shape, key, cell)`.
- **cell**: (a) a canonical `Rt2` structure index; (b) the position object's
  (player, else `player_spawn`) whole-pixel (x,y), `NO_CELL` without one.
  Meaning (b) is what storage, pos graph and level -1 use.
- **region**: (a) kernel region: 16 px square of the player's whole pixel x,y,
  speed in [-S,S] (S<=7, default 6), rem in [-0.5,0.5); (b) storage region:
  8 (or 16) cells square, nested in (a).

**Error is derived, not carried** (`trace::error`): `error(n) = own(n) or
OR(error(args))`, own errors only where evaluated on the lane's path:
`Flr` spanning two integers; `Sel` with undecided condition; fork coverage
(`SplitOk`); interval Add/Sub/Neg overflow (`NoWrap`); widening containment
(`SlotErrors`); `Restrict` violation on the raw cell (charged to every
outcome); unfinished unrolled loop; pins violated.

**Post-trace** (`verify::split_undecided_selects`): split every STORED select
whose condition is lane-undecidable, at its most upstream unknown, refold,
merge equal outcomes; for platforms decide comparisons over `verify::Points`
(world x player pixel) instead of splitting.

**Invariants.** Every lane of every input row ends in exactly one outcome or
RAISE (RAISE fires only from merge-lost correlations: spring-on-fall-floor
rooms; unreachable concretely). Trace refusals are failures to compile per
(shape, region), never silent. Tested at concrete points against the
`Concrete` oracle (`a_traced_frame_agrees_with_the_oracle`: one outcome claims
each point, error false, fields equal).

## 4. Abstraction levels

A level is `abstraction::Level { held, fruit, floors_near, platforms }`,
spec `r0sx[h][f][n][p]` (prefix fixed; flags in order). Process-global.
Widening happens twice and MUST agree: in-graph at the frame end
(`trace::widen::widen`, containment owed per lane) and on blocks
(`Rt2::widen_to`, for the concrete search's lookup and the ladder's
projection). Disagreement silently prunes real paths; `follow` and `known`
detect it.

**Rules.** Never widen without something EXACT that refutes it. A widening
replaces a value only by a visible superset (range or unknown). A widened
range must be a fixed point checked per row. Refutation = the arcs (for the
remainder) or the concrete search (everything else), optionally a finer
ladder level in between.

| widening | what | sound because | refuted by |
|---|---|---|---|
| remainder (every level) | player `rem.x/y` := [-0.5,0.5) | containment owed; transfer recorded per edge | arcs (exact) |
| `h` | `p_jump`/`p_dash` unknown; input forked 2 ways | superset (held may retrigger) | concrete search |
| `f` | fly fruit `step`,`y` unknown number; `fly` unknown; `spd.y`,`rem.y` literal ranges owed | superset | ladder + concrete |
| `n` | fall floor `state` [0,2], `collideable` derived, exact where player certainly overlaps; countdowns unknown number; spring/balloon phases ranges | superset; `diag-project` once checked t->n | concrete (or ladder `r0sxhn,r0sxh`) |
| `p` | platform `x`,`last` := path [-16,128], `rem.x` literal; `last==x` identity | platforms are a function of frames-since-load: `Points` | concrete; ref engine refuses `p` |
| strawberry bob (every level, undocumented) | `off` [0,39], `y` start +- 2.5 | superset, y owed | concrete only |
| timers (every level, undocumented) | `frames/seconds/minutes/deaths` := 0, key `spr`:=8, `flip.x`:=false | display-only fields: VALUE REPLACEMENT, breaks the rule | nothing (claimed unread) |

**Exact canonicalizations (no refutation needed).** `widen_dash`:
`dash_effect_time := max(det,0)` (read only as `>0`). `canon_balloon_offset`:
full-period offset -> canonical [0,1) (read only through `sin`).
`ABSENT_AS_ZERO`: a missing floor/spring `delay` is written 0
(`cart::check_absent_fields` refuses carts reading it otherwise; a hack).
Position-free keys (sec. 3).

**Restrict.** A bound (kernel region, speed, rem, platform x/spd) enters ONLY
as `Op::Restrict(lo,hi)(cell)`: value unchanged, range attached; every range
analysis reads it; its own error checks the RAW cell in every outcome, so no
fold can remove the check. Codegen: identity + two compares. Two literals
are owed per lane instead (platform `rem.x`, fruit `spd.y/rem.y`).

**rnd caveat.** `rnd` is an interval at every level, including the "concrete"
search, where each straddling comparison forks independently: combinations
no single draw gives are admitted. Sound for refutation, stated best-case for
confirmation; witnesses are replayed per seed.

## 5. Kernels

**Pipeline** (once per process per level, lazily; nothing cached on disk):
1. Lattice walk (`trace::kernel::room_constant_lattice`): fixpoint over
   (shape, kernel region) from the start; per shape the intersection of
   constants (pins); parallel rounds; monotone.
2. Lower + specialize (`trace::emit::lower_frame`, `lower::specialize_frame`):
   enumerate forks in each outcome's cone, decide by interval fold
   (`ival::fold`) + local BDD (`bdd::simplify_local`) + fold, fuse candidates
   with equal roots, drop bodies with `live == false`.
3. Assemble (`asm::codegen::compile`): SSA + GVN, schedule, linear-scan
   allocation (zmm0-25 homes), AVX-512 text -> `gcc -shared` -> `dlopen`.
4. Run (`Registry::run_chunk`): bucket lanes by (shape hash, region), slices
   of 16, per body take `live & !error & valid & !skip`, dedup, level -1 drop,
   key (`key_words16`), transfer id, pos-graph pair, `sink.emit`.

**Contract.** Per lane, every emitted row equals a reference-engine successor
widened to the level (the kernels may over-approximate where the reference
splits an interval; they never miss). Any lane the kernels cannot take is
FATAL: `KERNEL COVERAGE GAP` (`FrameEngine::run_bucket` panics) on (a) no
kernel for (shape, region) (`[asm] MISS`), (b) a declined lane (`live &
error`, unknown error reads as error; `[kernel] ... declined`). No fallback
to the interpreter. `kernel lanes: missed 0` must hold in every log.

**Folds relied on** (each exact, not merely sound):
- interval fold over `Restrict` ranges (`Graph::eval_fold_in`); refining
  rules excluded (they would change which lanes error);
- map fold, two forms: `tile_flag_over` (false if the union rectangle holds
  no flagged tile, true if the intersection does) and, kernel-opt, `mget`
  over an interval rectangle as the SET of tiles (decides `spikes_at`;
  only the lowering's fold uses it, level -1 and `Points` keep TOP);
- local BDD: constant over all atom assignments => constant;
- dead forks -> `ANY_VALID`; take-both classes deduped by root identity;
- kernel-opt codegen: decided boolean planes by bit identities (`x&-1=x`,
  `c?t:t=t`, ...), `/2^s` (s<=14) as biased shift and `%2^m` as mask,
  live-register-only saves around call-outs (exact because the allocator
  never splits intervals), constant rematerialization.
Checked by per-op bit-exactness tests against `celeste-engine::kernel`
primitives (`asm::tests`), the oracle test, `ref-check`, `KEY_CHECK`, and
byte-identity of kernel-opt against `fg-2300`.

**Call-outs** (`asm::callout`): `div` (general), `rem` (non-pow2), `sin`,
`mget`, `tile_flag_at` (uniform and per-lane w/h). Each calls the engine
primitive itself: exact by construction.

**Known hole.** Interval `Mul`/`Div` by a positive constant has no `NoWrap`.

## 6. The forward frame step and storage v2

**Entry.** `frame::grow_tree(dir, to, level -1, marks, engine)`: reuse a
tree complete through `to` (`edges/done.txt`), else resume or start, raise
if needed, then `extend` frame by frame. Horizons count STEPS
(`steps_per_frame` = 2 under split frame).

**Frame contract.** Input: frontier = layer f-1 (rows in id order, skip
masks). Output: layer f = every successor state not visited at any earlier
frame, each once, with an edge for EVERY (source, target) emission
including targets already visited; the frame's drop notes; the pos graph.
State identity is `(shape, key, cell)`.

**Visited set** (`storage::visited`): per (shape, storage region) a table
of ENTRIES: (position-free key, mask over the region's cells). Read-only
during a wave; written only by the translation.

**Ids** (`StateId` = region<<38 | entry<<8 | local; region = shape index x
slots + slot). Entries are numbered in KEY order among a frame's new entries,
shapes in hash order: ids do not depend on threads or units (they do depend
on raise history).

**The wave** (`storage::wave::run_wave`):
1. Units: contiguous runs of 1024-4096 live frontier rows of one block;
   workers pull units.
2. Per emission (`UnitSink::emit`): level -1 drop (noting the source);
   ladder verdict (batched per unit); a LID per distinct target entry
   (shape, slot, key) per unit, owner looked up once in the visited set. Cell
   in owner's mask -> old state, edge only. Else a REQUEST; the first unit to
   claim a new state copies its row (`Claims`).
3. Unit end: edges sorted by target, transfers ranked per unit, encoded into
   a block, streamed to the worker's `.blk` file.
4. Translation (`wave::translate`): requests grouped by target region, one
   worker per region, sorted (key, cell); new keys -> entries, new cells ->
   states; every lid gets its owner (`resolve_lids`).
5. Layer (`gather_layer`): new states in id order into pieces of 2^17 rows.

**Edges and transfers.** An edge recorded at frame f leaves a state of
layer f-1. Edge file `edges/fNNN.bin` (v5, "CSE1"): per unit its sources
(ids, or a row range of f-1's file), its block (per lid: cell byte, varint
source deltas, transfer rank), per-rank global transfer ids; per file the
OWNER INDEX `(region, entry, unit, lid)` sorted, the reverse walk. Transfers:
one content-canonical table per tree, `edges/xfer.bin`. ~3.7 B/edge plus
~20 B/lid. A TRANSFER per axis = guard (arc of input remainders) + action
(`Rotate(r)` or `Const(c)` for a blocked step or spawn), decoded from five
captured roots per axis (`took, pre, frag, ox, fin`; `arc_edges::decode_axis`).
Inconsistent captures are refused. A body without transfer roots is refused
at build. The transfer is never in the row or key.

**Model behind transfers.** Per axis `move` does `rem += ox + 1/2; amount =
flr(rem); rem -= 1/2 + amount`, then steps `|amount|+1` pixels: mod 1 a
rotation by `ox`, cut once per axis; a blocked step sets rem 0. At most one
player split per axis per path (refused otherwise); a platform carry is
whole pixels, a wall-blocked carry is `Const`.

**Wins.** Exiting rows are checkpointed and not expanded (`not_expanded`:
exits, left-room under a win rect, 100% berry lost, orb deadline). First win
uses `reaches_win`; checkpoint win rows (backward seeds) use `wins_of`.

**Pos graph** (`search::pos_graph`): per frame the set of (src cell -> dst
cell) pairs over all emissions incl. level -1 drops; used by level -1 checks,
the UI, and a gate.

**Split frame** (`CELESTE_SPLIT_FRAME`): part a = timers, freeze, objects up
to and including the player's `move`; part b = the player's update and the
rest. Kernels ~10-20x smaller; states at frame boundaries identical to
unsplit (test). Tree, arcs, marks, level -1 count steps; the concrete search
steps whole frames and looks up W at even steps; arc optimum in steps s
bounds the game at ceil(s/2).

**Trim rows** (`CELESTE_TRIM_ROWS=1`): once frame f is trusted, f-1's files
keep keys, ids, cells, wins only. Raise, arc-check, rerun-row, follow refuse
a trimmed tree.

**Determinism.** State sets, ids, frame files, meta, xfer table and edge
CONTENT are identical across thread counts and resume. Edge FILE bytes
(units, lids, owner index) are not. Gates therefore fingerprint (shape, key,
cell), never ids.

## 7. The backward

**Remainder-free BFS** (`search::edges::bfs`): seed every layer's win rows;
for i = H-1..1 mark predecessors via `EdgeStore::preds_at`. A mark's
DEADLINE is the last step from which the state can win with SOME remainder.
Marks = masks per entry (`storage::marks`), ranked densely (`MarkRanks`).

**Graph load** (`arc_dp::load`): edges between marked nodes only, unit by
unit, two passes (degrees, CSR fill; 12 B/edge), each with its global
transfer id. The start is added with deadline 0.

**Winning sets** (`arc_dp::backward`): `W_t(n) = U_e pull_e(W_{t+1}(dst_e))`
while n is live (layer <= t <= deadline); a win node holds the whole torus.
`pull` = preimage inside the guard. Sets are `arcs::Region` (canonical: y
slabs of x segments; equal sets equal values). EXACT in the remainder because
"intersect, then rotate/const" distributes over unions and arithmetic is
integer. Incremental: recompute predecessors of changed nodes. Storage:
spans (node, t-range, set id) into an interned arena.

**Optimum** (`arc_dp::optimum`): `steps = H - max{t : p0 in W_t(start)}`,
p0 = (0,0) (the start has no player). One backward serves every horizon
<= H (layers never bind from the start; tested). It is a LOWER BOUND on the
game: the node graph over-approximates every widening except the
remainder, and every filter is a superset. Empty -> H REFUTED.

**Reach marks** (`arc_dp::reach`): forward from p0 inside W, `R_{t+1}(m) =
U push_e(R_t(p)) n W_{t+1}(m)`; a set over 32 segments is replaced by W_t
(superset). Output per node: last t with R non-empty. Feeds the next ladder
level and `arc.marks.bin`. A concrete winner's remainder is in R by
induction.

## 8. The concrete search

`arc_dp::concrete_search`, at the LAST level only, unless `--no-witness`.

- **State**: exact reference-engine `Rt2` states; one step = one game frame
  (`RefEngine::frame`, both halves under split frame).
- **Successors**: all 64 inputs (an input equal on every button the frame
  read to one already run is skipped), every `rnd` leaf.
- **Admission**: a winning successor is accepted; otherwise its projection
  onto the level (`lookup_keys`, via `Rt2::widen_to`) must be a node whose
  `W_t` contains its EXACT remainder. Nothing else prunes (level -1, the
  BFS marks and the ladder filter are already inside W).
- **Dedup**: by the UNWIDENED key + cell (128-bit hash), per layer. Never
  by a widened key (`522de36` lesson).
- **Phases**: (1) DFS for a win exactly at the bound, one engine, 200k steps;
  a win before the bound is fatal ("the backward lost a path"); exhaustion
  proves nothing. (2) Parallel BFS by layers inside `W`: the first layer with
  a win is the concrete optimum; deterministic order (parent, input, leaf).
- **Output**: `<dir>/witness_frame_F.txt`; `[gate] hH concrete optimum`.
  `--prefer FILE` puts the known route's inputs first and runs the
  known-route check.
- **Cost** = number of concrete states inside W, decided by the level's
  widenings: a few hundred steps when the bound is tight; it blew up in room
  (7,0) at `r0sxhn` (bound 80 vs 84) -> the ladder.

## 9. Filters on the forward: level -1 and the objects ladder

Both drop rows at emission (`UnitSink`); both are monotone in the frame, so
a recorded expansion reused at a later frame only over-approximates; the
known-route check audits both.

**Level -1** (`trace::level_minus_one`, `CELESTE_LEVEL_MINUS_ONE="H,S"`).
- Table `d(shape hash, cell)`: a lower bound on frames to the room EXIT.
  Nodes are (heap shape, player cell); every other input is one range per
  shape (rem whole, speed [-S,S], the rest found inductively). Built by
  tracing each shape, specializing per fork configuration, evaluating over
  ranges, a BFS plus an inductive fixpoint (ranges widen until closed).
  `sound_d`: backward shortest path; exit edge 1; death = 1 + respawn
  chain + d(start), to a fixpoint.
- Filter: drop a row when `f + d > H` (H in STEPS). Sound because d is a
  lower bound; the screen window is CHECKED (panic outside); rows without a
  table node are kept. Refuses `win_rect` rooms (summit, synthetic wins).
- Horizon-independent, one per room: cached in `/var/tmp/celeste-l1-cache`,
  keyed on the binary, the Lua, room, S and every `CELESTE_*` but 5.
- The tree records the filter it was built under (`level_minus_one.txt`);
  extension uses the tree's filter, not the environment's. Drop sources are
  noted (`dropped/fNNN.bin`, min admitting horizon) for the raise.

**Objects ladder** (`--level A,B`, e.g. `r0sxhn,r0sxh`; `frame::MarkFilter`).
- Level i>0's forward keeps a row at step t only if its projection onto
  level i-1 (`Rt2::widen_to`) is REACH-marked there with deadline >= t;
  mid-frame (odd) steps pass (the `5ccf712` fix).
- Sound: the remainder is exact at both levels and the coarse level
  over-approximates the objects, so a fine winner projects into R_t.
- The filtered tree is reused only if `filtered_for.txt` (steps, level -1,
  marks count, fingerprint) matches.
- Use it where the coarse bound is well below the reference ((7,0), (4,3)
  nodiag, gemskip rooms).

## 10. Persistence

**Layout** `<dir>/level{i:02}/`: `frames/fNNN/{s<shape>_<seq>.bin, meta.bin,
meta.r<seq>.bin}` (checkpoint v12: raw columns + key, id, cell; header with
win rows and a trimmed flag), `edges/{fNNN.bin, fNNN.wWWW.blk, fNNN.rSEQ.bin,
xfer.bin, xfer.len, done.txt}`, `dropped/fNNN.bin`, `level_minus_one.txt`,
`posgraph.bin`, `posgraph.frame`, `filtered_for.txt`, `raising.txt`,
`metrics.jsonl`. Witness at `<dir>/witness_frame_F.txt`.

**Trust.** Frame f is trusted when `edges/done.txt >= f`. Resume
(`ForwardState::resume`): rerun the same command; frames past done are
deleted, the visited set rebuilt from metas + id columns (key equality
checked), frontier = last layer with skip masks. The arc phase reruns.

**Raise** (`ForwardState::raise`): a tree built under level -1 at H extended
to H' > H re-expands, frame by frame, exactly the noted sources with
`f+d <= H'` plus the states the raise added, against the whole visited set;
new states join their layer (`Layer::Raised`), new edges go to `fNNN.rSEQ.bin`.
Contract: equal to a fresh tree at H' (same (key, cell) sets, edges, pos
graph, notes); a hit on a state of a LATER layer is refused. Refused: trimmed
trees, the orb room, other S or table, unrecorded filters, an interrupted raise.

**Not on disk** (resuming under a different value is on the user): the level
spec, `CELESTE_STORAGE_REGION`, `CELESTE_SPLIT_FRAME`, `CELESTE_REGION`, the
room, the category flags.

## 11. Verification surface

| check | what it asserts | protects | cost today |
|---|---|---|---|
| quick suite (169 tests, 3 `#[ignore]`) | unit/property tests incl. oracle, per-op asm bit-exactness, backward = definition, reach | everything locally | ~47 s build+run |
| ignored tests | resume = fresh; room (7,1) table (start d 45); isolated-floor pin | resume, level -1, ref pins | minutes |
| ckhash gate | (key, cell) set per frame f0-44, room (1,0) | forward, kernels, tracer, storage | one forward to 44 (seconds) |
| posgraph gate | cell-pair set at f44 | same | same run |
| arc gate (`--win-at 9,101 --to 35 --prefer`) | BFS marks count+fp, W fp per frame, arc optimum 33, concrete witness | backward, transfers, concrete search | small |
| known route (`--prefer`, `check-known`) | a known solution survives every pruning step: level -1, ladder filter, node exists, W_s holds exact rem, reached deadline; prints first failing step | soundness of all pruning | 0.05-1 s per level in-search; `check-known` reruns arc phases (~25 s) |
| `arc-check` | every edge has a transfer; sampled transfers probed with the reference engine inside/outside guards; `--fault` self-test | transfer recording | (1,0) f1-66 x30: 23 min; object level (4,2) r0sxhn did NOT finish in 1.5 h on v2 |
| `ref-check` | kernel successors vs reference successors by named field (ref-only = soundness gap) | kernels | per frame; object level hit 60 GB once |
| `CELESTE_KERNEL_KEY_CHECK` | per emitted row, kernel key = `Rt2::boundary_canonicalize` | key computation | slows forward; changes code path (claims off) |
| `kernel lanes: missed 0` | no coverage gap | kernels | free |
| `gates/raise.sh` | raised tree = fresh tree (ckhash --edges, posgraph, gate lines) | raise, notes, edge files | minutes |
| `follow` | a known route's widened keys are in a tree; else nearest kernel successor | widen/widen_to agreement, kernels | one route |
| `l1-check` | no state of a known route is too late | level -1 table | seconds |
| `bounds-audit` | no stored row outside Restrict/owed ranges | Restrict | per tree |
| `diag-project` | a fine tree projects into a coarse one | a widening (one-off) | per tree pair |
| `bench-storage` | replayed capture reproduces ckhash f/e lines | storage determinism | seconds |
| witness replay (sec. 12) | the witness exits on real PICO-8 | everything | seconds |

Re-pin a gate only with evidence the SETS did not move (posgraph, per-frame
kept counts, marks, first win and optimum identical).

## 12. Real-play validation

**Replay** (`pico8_diff/replay.py`): builds a `.p8` from the ORIGINAL cart
(`--lua .../celeste.lua --begin-game`) or the minimal one, runs `pico8 -x`
headless, prints one line per frame; the caller reads the frame the room
changes. Options: `--room`, `--balloon-seeds` (also chest seeds, UCT
semantics), `--jank J`, `--dump` (entry state), `--one-dash`. It asserts
nothing itself.

**Loading jank J.** The leaving player calls `next_room()` inside
`foreach(objects)`; `all` resumes at the same index in the new list, so the
new room's objects from index J get one update on the loading frame. J = 1 +
objects before the player at exit = previous room's objects - spawn -
destroyed (berry, fly fruit, key, chest, fake wall) + title/smoke if early.
J is HISTORY: the sound start is the set of feasible J's grouped by the start
state they give (`chain.py feasible_janks/start_classes`, compared via
`concrete_run --dump-start`); the category runner searches each class; the
history-widened optimum is the least. Only J moves objects; cosmetic globals
differ.

**Chains** (`pico8_diff/chain.py`): rooms played through real transitions
with database files; `--boot` from 100m is GROUND TRUTH for start state and
verdict; `--check-start` compares the search's start field by field.

**Other emulators.** UCT (UniversalClassicTas, the database's checker; Lua in
doubles, no loading frame) and Celia (the community tool; models the jank
but with the ORIGINAL object count, ignoring destroyed objects).
`tools/uct/check_uploads.py`: VALID = boot chain exits by the file's count
(with berry for 100%) AND Celia finishes by it. UCT is information only.

**Policy.** Submit the PICO-8 16.16 optimum; find a variant (another optimal
witness, seed nudges, the cut) that also passes UCT/Celia at the SAME count;
never lengthen. Celia should be fixed upstream (parked); until then both must
pass. Float-only database files are parked.

**Pipeline** (`tools/category_runner.py`): boot-chain J -> reference
replay (offset scan) -> `rewrite search --ceiling/--to --prefer` per start
class -> original-cart replay of the witness (+ jump canonicalization, chest
seed variants, nodiag/berry checks) -> upload cut by real play ->
`align_tas.py` + `export-ui` when improved.

---------------------------------------------------------------------------

## Annex A [SIZE] (wc -l, tests included; storage-v2, merged where noted)

| subsystem | lines | main files |
|---|---|---|
| numbers, map, names (crates core+names) | 1,354 | pico8_num 645, collision_cache 308, gen.rs 243 |
| block model (crate engine) | 2,232 | runtime2 1264, kernel 803 (lane primitives) |
| game semantics: Lua | 1,969 | minimal 894, split 934, builtins 141 |
| tracer (src/trace minus ref engine, level -1) | 11,602 | interp 2151, verify 2083, domain 1611, widen 941, kernel 924 |
| reference engine | 1,275 | refengine 436, refbridge 364, refdomain 359 |
| levels/start/win (abstraction, concrete, game_runner) | 401 | |
| kernels (transpile + asm + compiled) | 8,653 -> 10,351 merged | codegen 2000->2488, asm_kernel 1816->1881, graph 1611->1697, bdd 951, mix.rs 716 new |
| forward + storage + persistence | ~6,870 | frame 2082, storage 3625, checkpoint 579, pos_graph 324 |
| backward (BFS, load, W, reach) | ~2,500 | arc_dp 1749 (of which concrete ~420), arcs 546, arc_edges 322, edges 138 |
| concrete search + known route | ~690 | arc_dp ~420, known 264 |
| level -1 | ~2,400 | level_minus_one 2210, frame ~90, transpile bin 54 |
| CLI (rewrite.rs) | 2,105 | 20 subcommands; search arm ~100 |
| diagnostics outside rewrite.rs | ~540 | storage/bench 366, search/inspect 169 |
| UI export + UI | 1,092 Rust + ~5,960 TS | ui_export.rs |
| Python/shell tooling (live) | ~2,500 | category_runner 466, replay 405, chain 372, uct 452, celia 140, compare_video 283 |
| Python tooling (dead) | ~660 | Annex C |
| total Rust | 41,338 -> ~43,000 merged | |

## Annex B [KNOB]

**Env vars read in code (33 CELESTE_*, incl. KERNEL_MIX from kernel-opt).** L = live, T = tuning/resource, D =
diagnostic, X = effectively dead.

| var | effect | status |
|---|---|---|
| START_ROOM | start room x,y (set by `--room`) | L |
| WIN_AT_XY | synthetic win (set by `--win-at`) | L (gates) |
| LOADING_JANK | J or `none` | L (runner always) |
| NODIAG / GEMSKIP / HUNDRED | category semantics | L |
| BALLOON_SEEDS | fix balloon `rnd` in the cart | L (internal + chain.py) |
| CONCRETE_BALLOON_SEEDS | seeds for the concrete phase only (copied into BALLOON_SEEDS) | L |
| SPLIT_FRAME | two steps per frame, split cart | L |
| LEVEL_MINUS_ONE | "H,S" filter (H in steps) | L |
| L1_CACHE | table cache dir / off | L default, knob unused |
| TRIM_ROWS | trim old frames | L opt-in |
| REGION | kernel region "px,S" / off | T |
| STORAGE_REGION | 8 / 16 | T (not recorded in tree) |
| THREADS | workers | T |
| WALK_THREADS / BUILD_THREADS | kernel build parallelism | T, undocumented |
| UNIT_LANES | unit size | T, no users |
| MIN_FREE_GB | disk guard (20) | T, unused knob |
| MAX_TRACE_NODES | tracer cap (2M) | T, no users |
| ROOT | repo root | X (always ".") |
| KERNEL_KEY_CHECK | key check; disables claims; any value incl. "0" enables | D (gate) |
| KERNEL_EXPLAIN | keep fused graphs to explain declines | D |
| KERNEL_MIX (kernel-opt) | instruction-mix dumps | D |
| BUILD_TRACE | build logging | D |
| ASM_STATS | instruction counts | D, no users |
| LATTICE_TRACE | walk logging | D, no users |
| WALK_REGIONS | trace only listed regions | D, test only |
| PHASES | TSC phase line | D |
| EMIT_CAPTURE / EMIT_CAPTURE_FRAME | capture a frame for bench-storage | D |
| EDGE_DUMP | ckhash prints a frame's edges | D, no users |
| L1_PATH | level -1 fastest path | D |
| (non-CELESTE) PICO8, MIMALLOC_PURGE_DELAY | console path; asserted allocator setting | L |

Counts: semantics/category 9, pruning/storage shape 5 (LEVEL_MINUS_ONE,
L1_CACHE, TRIM_ROWS, REGION, STORAGE_REGION), resources 7, diagnostics 12.
Dead names still in docs only: WIN_RECT, ARC_EDGES, BAND, LADDER,
LADDER_RUNGS, BACKWARD, KEEP_RAW, EMIT_BUDGET_GB, KERNEL_SETS, SKIP_CENSUS.

**Binaries.** `rewrite` (20 subcommands), `transpile` (level -1 only:
`--level-minus-one`, `--level-minus-one-table`), `concrete_run`
(`-i -f --object --dump-start`).

| subcommand | role | status |
|---|---|---|
| search | the pipeline; flags `--to --ceiling --level --checkpoint-dir --room --win-at --no-witness --save-marks --prefer` | L |
| check-known | known route vs finished trees | L |
| forward, ckhash | gates | L |
| export-ui | UI data | L |
| trajectory | fit community TAS to minimal cart | L |
| arc-check, ref-check, follow, l1-check, bounds-audit, diag-project | verification | D |
| bench-frame, bench-storage | benchmarks | D |
| cell-growth, col-census, coarse-census, spurious, rerun-row, edge-census | censuses | D (edge-census likely stale) |

## Annex C [FEATURE]

| feature | size | depends on it / notes | live |
|---|---|---|---|
| objects ladder | ~60 Rust + `filtered_for.txt` + reach (~140) | (7,0), nodiag/gemskip big rooms | yes |
| split frame | 934 Lua (70-line diff), ~25 Rust, steps/frames plumbing everywhere | rooms (6,0), (6,1) | yes, opt-in |
| trim rows | ~20 | big trees; blocks raise and diagnostics | opt-in |
| raise | ~350 in frame.rs + notes + `Layer::Raised` + raised edge/meta files | count-up campaigns under level -1 | yes |
| level -1 | ~2,400 | most rooms' forwards; raise | yes |
| pos graph | 324 + hooks | a gate, UI, level -1 audits | yes, always on |
| nodiag | ~25 Rust + runner | category | yes |
| 100% | ~110 Rust (wins_of, berry_lost) | category | yes |
| gemskip | ~15 Rust | category | yes |
| loading jank | ~60 Rust + chain.py/replay.py | every real-play result | yes |
| UI export | 1,092 Rust + ~6k TS | `--save-marks`; still carries ladder concepts | yes |
| bench-storage / emit capture | 366 + hooks | storage perf | diag |
| kernel mix | 716 Rust + 276 Py (kernel-opt) | kernel perf | diag |
| robust path, playable | not on this branch (branches `robust-path`, `playable`) | | no |
| dead tooling | tools/server.py, tree_viewer.html, framesetdiff, gdiff, gjoin, rowset, rowdiff, posgraph_pairs, key_soundness, key_unsound, shape_share, fuse_frontier, lua_tests/ (~660 lines) | formats no longer written | dead |

## Annex D [JANK] (file/function: why)

**Concepts and names**
- tr-1 `trace::widen` vs `Rt2::widen_to`: every widening implemented twice,
  synced by convention; mismatch silently prunes; only `follow`/`known` catch it.
- tr-2 "outcome" (`verify::FrameOut`) = guarded successor; lower.rs has another `Outcome`.
- tr-3 "cell" = canonical structure index AND player pixel; "region" = kernel
  grid AND storage grid; "lane" = row AND zmm slot; "key" = row key, shape
  hash, `verify::RowKey`, `pm1_key`.
- tr-4 three shape identities: `heap::Shape`, u64 `shape_hash` (FxHash, no
  collision check), the walk's `String`.
- tr-5 "concrete": `domain::Concrete` (oracle), `concrete.rs` (utilities), the
  "concrete search" (runs `RefDomain` intervals for rnd).
- kn-1 five "kernel" modules (`transpile/kernel.rs`, `trace/kernel.rs` = the
  lattice walk, engine `kernel.rs` = lane primitives, `compiled/asm_kernel.rs`,
  `AsmKernel`); two `AsmBody` types.
- kn-2 `src/bin/transpile.rs` only runs level -1.
- fw-1 two "visited" (`frame::Visited` = marks set, `storage::visited`), two
  "marks", two "edges" modules (`search/edges.rs` is the BFS).
- bw-1 two "deadline" meanings in one u16 (BFS vs reach); marks file stores
  `horizon - deadline`.
- bw-2 two remainder-set types (`arcs::Set/Rects` for tests and arc-check,
  `arcs::Region` for the pipeline); two witness files.

**Unstated or rule-breaking widenings**
- tr-6 `widen::widen_fruit`: strawberry bob widened at EVERY level, a real
  over-approximation, absent from abstractions.md.
- tr-7 `widen::widen_timers`: value replacement with no owe (frames, deaths,
  key `spr`/`flip`), breaks "never by another value".
- tr-8 `ABSENT_AS_ZERO` + `cart::check_absent_fields` (a syntactic count)
  instead of a nil-or-number type (self-declared hack).
- tr-9 countdown atoms recognized by FIELD NAME (`domain::COUNTDOWN_FIELDS`).
- tr-10 with `p`, an overlapped floor stores its computed value because an
  owe failed for unproven reasons (workaround).
- tr-11 RAISE rows exist only because merges lose spring/floor correlation.
- tr-12 two owed input literals (platform `rem.x`, fruit `spd.y/rem.y`): a
  third bound mechanism beside pins and `Restrict`.

**rnd and seeds**
- tr-13 `RefDomain::compare` forks each straddling comparison independently:
  the "exhaustive concrete" search admits draw combinations that do not exist.
- bw-3 `arc_dp::concrete_engines` copies `CELESTE_CONCRETE_BALLOON_SEEDS`
  into `CELESTE_BALLOON_SEEDS` in the process env, then removes it;
  `lookup_view` rewrites balloon offset/y back to the tree's interval.

**Global state and what disk does not record**
- gl-1 level is a process-global Mutex but some paths take it as a parameter;
  `Block::keyed` reads `current_level().held` only; one resident kernel set.
- gl-2 level spec, STORAGE_REGION, SPLIT_FRAME, REGION, room, category flags
  not recorded in a tree: resume under another env misdecodes silently.
- gl-3 many knobs are `OnceLock`-cached; tests mutate env (needs nextest isolation).

**Steps vs frames**
- sf-1 `--to/--ceiling` in frames, `CELESTE_LEVEL_MINUS_ONE` H and tree labels
  in steps, `Found.frame` steps, `Solved.arc` frames.
- sf-2 `follow`, `l1-check` call `RefEngine::step` once per input byte:
  wrong under split frame.
- sf-3 split cart is a 934-line near-copy, not a patch; `__phase`/`__frozen`
  globals as tables to force distinct shapes.

**Forward / storage / persistence**
- fw-2 `frame::extend` saves the pos graph AFTER `done.txt`: a crash between
  them leaves a tree `resume` refuses ("pos graph covers ...").
- fw-3 three trust markers: `done.txt`, `xfer.len`, `posgraph.frame`.
- fw-4 two win notions: forward's first win uses `reaches_win`, checkpoint
  win rows and resume use `wins_of` (100% berry): `win_frame` can differ
  after resume.
- fw-5 `extend` applies `not_expanded` only when the frame `won` (or orb);
  `resume` always: in 100% a berry-lost row is expanded fresh but skipped
  after a resume.
- fw-6 `CELESTE_KERNEL_KEY_CHECK` turns claims off (different code path) and
  `var_os` makes `=0` enable it.
- fw-7 flag parsing inconsistent: SPLIT_FRAME any value, TRIM_ROWS/PHASES `=="1"`.
- fw-8 checkpoint `cell` column duplicates what the id encodes.
- fw-9 edge files not self-contained (sources as row ranges of f-1's frame file).
- fw-10 frame numbers parsed from 3 digits (`EdgeStore::open`,
  `discard_after`): breaks at step 1000 (split frame, long rooms).
- fw-11 `save_value_to` stamps checkpoint `FORMAT_VERSION` on every side file.
- fw-12 `ForwardState::start/resume(record)` only controls the pos graph.
- fw-13 leftovers: `Block::keep/with_ids`, `one_frame`, `bench-frame` beside
  `bench-storage`; three sentinels `NONE/NO_OWNER/PENDING`; stale docs
  ("queue", "flush", "door").

**Backward / search**
- bw-4 `known::check` takes level -1 from the environment, not the tree's
  `TreeFilter`: can report false PRUNED or miss one.
- bw-5 `solve` hard-codes p0 = (0,0); known.rs special-cases the start by id.
- bw-6 `NodeKeys::new` keeps the lowest node on duplicate (shape, key, cell), silently.
- bw-7 concrete dedup is a 128-bit hash, per layer only; DFS memo includes depth.
- bw-8 magic constants: `REACH_SEGS=32`, `DFS_BUDGET=200k`, `CHUNK=256`,
  256 trace states, `PLATFORM_WORLD_FRAMES=127`, 20 px floor isolation.
- bw-9 `CostToGo::admitted_from` panics outside the window (forward crashes).
- bw-10 `frame::cell_too_late` ("the time band") is a leftover of `CELESTE_BAND`.

**Kernels**
- kn-3 three evaluator entry points with different `mget` semantics
  (`eval_lenient_in` bails, `eval_fold_in` sets, `eval_narrow_top_in` TOP);
  two map-over-rectangle folds with different clipping.
- kn-4 `drop_redundant_reloads`/`constant_key` re-parse emitted AT&T text.
- kn-5 miss report is a lane count; reasons only on stderr; a decline cannot
  explain itself without a rerun under `CELESTE_KERNEL_EXPLAIN`.
- kn-6 key-exclusion rules spread (`widen_uniform`, `konst_av`); `unify`
  makes a shape's skeleton depend on which region kernels exist.
- kn-7 a skipped lane's error is ignored (special case from room (6,2) f84).
- kn-8 `BuildPurgeDelay` mimalloc hack (raw option id 15); MB-sized kernel
  stack frames need `set_thread_stack` on every caller thread.
- kn-9 `Emit.decide=false` path dead; `TileFlag` vs `TileFlagLanes`.
- kn-10 stale comments after kernel-opt ("saves all 32 zmm").

**Cart and room special cases**
- rm-1 cart patching by exact-whitespace `replacen` in Rust
  (`forbid_diagonal_dashes`, `fix_balloon_seeds`, `apply_loading_frame`,
  `apply_start_room`) AND again in replay.py (`assert count==1`).
- rm-2 `ORB_LEVEL=21`, `x==7` wrap, summit flag rect, `orb_required` in
  `reaches_win`; `OBJECT_TILES`/`minimal_jank` remap J between carts.
- rm-3 builtin lists disagree (`cart::NATIVE` declares unused `__widen_rem`,
  `__new_vector`, `__split_at`; frozen `GLOBAL_NAMES` preserves dead ABI).
- rm-4 tooling: chain.py `OVERRIDES` (gemskip100 28), hardcoded jank
  constants (title 36 frames, smoke 15), runner `canon_jumps`, chest-seed
  variants, seed nudges, offset scan +-2, hardcoded `/home/philippe/...`
  paths in align_tas.py.

## Annex E: plans vs code

- architecture.md/level-minus-one.md: level -1 drops "at the flush"/"queues";
  code drops per emission (`UnitSink::minus_one_drop`). H is in steps.
- architecture.md "The search" step 4: "at a non-last level only the try at
  the bound runs"; since `86b4a6d` no concrete search runs there.
- architecture.md frame-end order (edges, meta, notes, pos graph, done); code:
  notes, checkpoint, meta, done, trim, pos graph (fw-2).
- architecture.md "result is a function of the frame": true for states, ids
  (absent raises) and edge content, not edge-file bytes.
- architecture.md mentions `CELESTE_WIN_RECT`, `rewrite arc-proto`: gone.
- abstractions.md: `Level { ..., floors }` is `floors_near`;
  `HeldPrecision`/`FruitPrecision`/... types do not exist; "ref engine refuses
  `f` and `p`": only `p`; the every-level list omits `widen_fruit`,
  `widen_timers`, `widen_dash`.
- storage-v2.md id layout 24/32 bits; code 26/30. Owner-by-lid figures pre-`fa2b2dc`.
- known.rs/architecture.md: the check uses level -1 "as the search ran"; code
  uses the environment (bw-4).
- CLAUDE.md: door, queues, runs, RowCache are gone; `Level` fields; status line counts.
- storage-unify study (origin/storage-unify): do NOT build unified per-region
  sets (region-scoped blocks 4-9% bigger, no record saved); instead edge file
  v6 with delta-varint owner index and varint starts (~20 -> ~7 B/lid, -5 to
  -9% of the edge store).
