# SPEC: what the system promises (contracts), 2026-10-10

Scope: the merged state, `storage-v2` (f3ccf68) + `kernel-opt` (codegen folds,
`CELESTE_KERNEL_MIX`). Written from the code; where plans/ disagree the code wins (Annex E).
Main text = contracts. Annexes = what to cut from: [SIZE] A, [KNOB] B, [FEATURE] C,
[JANK] D (tags like `tr-6` are referenced from the main text), plans-vs-code E.

Decomposition (changed from the brief): level -1 and the objects ladder are one section
(sec. 9, "filters"): both drop rows in the forward, both are monotone in the frame, and the
known-route check audits both. Crate order is load-bearing: `celeste-core` (numbers, map,
collision) and `celeste-names` (FROZEN field order) <- `celeste-engine` (`Rt2` blocks, keys,
lane primitives) <- `celeste-rust` (tracer, kernels, storage, search, bins).

## 1. Goal and proof obligation

**Problem.** Per room (x,y) and category: the least N such that some input sequence makes
the room change DURING frame N. Frame 1 = the first `_update` after `load_room`; the spawn
prologue (23-37 frames) is included. Database count = N - prologue - 1.

**Win predicate** (`frame::reaches_win`, `frame::wins_of`): the room becomes `win_room()`
((x+1,y), or (0,y+1) from x=7); orb room (5,2) only with `max_djump==2`; summit (6,3):
touching the flag rect; 100% (`CELESTE_HUNDRED`) also `got_fruit` (in `wins_of` only, fw-4);
nodiag (`CELESTE_NODIAG`): the cart is patched so a diagonal dash raises (no successor);
synthetic `--win-at` for gates.

**Start state.** `_init()` with `load_room(1,0)` rewritten to the start room; past
`ORB_LEVEL=21` `max_djump=2` unless gemskip. Real play adds the loading frame from object J
(`CELESTE_LOADING_JANK`, sec. 12). No held buttons carry over.

**A proof of optimum N.**
1. Every abstraction is exact or over-approximates the game (sec. 4), so "no abstract win by
   N-1" REFUTES N-1.
2. The concrete search (sec. 8) runs the reference engine exhaustively (every input, every
   `rnd` leaf) and prunes ONLY by the arc winning sets W, which contain every concrete
   winner. Its first win layer is the optimum; its path the witness.
3. Under `--ceiling C` (a known solution's length) refuting C, or no witness, is an ERROR.
4. A result is believed only after the witness replays on a real PICO-8 with the ORIGINAL
   cart (sec. 12). The project's own earlier "proven 94" for room (0,0) was a frame long.

**Stated caveats.** `rnd`: a refutation holds for every draw, a confirmation means "some draw
wins" (tr-13). Interval `Mul`/`Div` by a constant may wrap unchecked in the kernels. Most
ladder-era results predate the Add/Sub overflow fix (`b47b118`); `n` rooms not all re-run.

**Witness.** `tas/room_X_Y_<kind>_frame_N.txt`: `#` header, one input byte per frame from
frame 1 (bits 0..5 = left, right, up, down, jump, dash). Uploads: tasdatabase
`[seeds,]i1,...` from the player's creation frame.

## 2. Game semantics

**Traced program** (`cart::sources_in`): `lua/builtin_level_3.lua` (`add`, `foreach` with
PICO-8 `all` semantics) + `builtin_level_4.lua` (buttons, `count`, `del`, draw no-ops) +
`celeste-minimal.lua` (894 lines; count things here). Against the original
(`celeste_ocaml/celeste.lua`): no smoke/hair/particles/`message`/`flag`/title; NO `jbuffer`
(an early press is lost; claimed not to change optima; TAS inputs need re-timing, `rewrite
trajectory`); `move` rewritten around `__split_by_flr` (exact). Load-time string patches:
start room, loading jank, nodiag, balloon seeds; every consumer reads the patched source.

**Numbers** (`pico8_num`): i32 16.16. Add/sub/neg wrap; mul = (i64 product)>>16; div
truncates and saturates (incl. /0); `%` = `rem_euclid`, `a%0=0`; `sin` from a table dumped
from a console; `mget` outside the map reads 0.

**rnd.** `rnd(x)` = the interval [0,x): a symbolic interval in the tracer; in the reference
engine an interval whose straddling comparisons fork 2 ways. Uses: balloon `offset`, chest
`x`. `CELESTE_BALLOON_SEEDS` replaces the balloon draw by constants.

**Reference engine** (`trace::refengine/refdomain/refbridge/refdriver`): the SAME
`Interp<D>` as the tracer over `RefDomain` (scalar intervals, real bools, no merges),
enumerating the fork tree by re-execution (cap 1M paths). Ground truth for the concrete
search (always `Level::EXACT`), `arc-check`, `ref-check`, `follow`, `known`, platform worlds.
Not ground truth for PICO-8: that is `trace::probe` (console goldens), `pico8_diff/run.sh`
(builtin differential tests) and witness replay. `refbridge` maps a block lane to an
interpreter state and each leaf back to a keyed one-row block. Refuses level `p`.

## 3. The tracer: Lua -> graph IR

**In:** cart AST (full_moon), start heap, level, per-shape pins (lattice constants), bounds
(`Restrict`). **Out** (`verify::trace_frame` -> `Frame`): input interface (slots by path),
outcomes, a RAISE row, fork metadata, transfer roots. One frame = `__reset_button_states();
_update(); _draw()` (`_draw` holds the screen clamp).

**Model.** An AST interpreter with a CONCRETE heap (tables, closures, lengths) and SYMBOLIC
scalars hash-consed into one `transpile::graph::Graph`. Symbolic indexing is a refusal. At a
lane-undecidable branch the state splits and rejoins with a `Sel` per differing slot. Caps:
256 trace states, 2M nodes.

**Terms.**
- **shape**: heap structure; equal shapes trace identically (all non-frozen scalars
  symbolic). Three identities exist (tr-4).
- **outcome** (`verify::FrameOut`; bad name, really "successor"/"arm"): (guard, derived
  error, end fields, end shape, transfer roots). Guards are disjoint and cover every lane.
- **fork**: choose a node, partition its set, duplicate the downstream cone per part, fold.
  `Split/Frag` (from `__split_by_flr`), `SplitInt/IntFrag` (held trails, near floors), and
  2-way forks every lane takes both ways (buttons, escaped atoms). Trigger:
  LANE-UNDECIDABILITY (the condition differs within one lane's set), not abstractness.
- **body** (`lower::SpecializedBody`): one fork configuration of an outcome's cone, folded;
  masks `live`, `error`; a lane emits where `live & !error`.
- **row / lane / block**: a row is a state in a block (`Rt2`: one heap structure + value
  columns of `AV::{Num, Ival, Bool, UBool, UNum, ...}`); "lane" also means a zmm slot.
- **key** (`runtime2::boundary_finish`): 128 bits = shape hash + SUM of `cell_mix(canonical
  id, value)`, mixed; a number and its point interval key alike. POSITION-FREE (`29d8364`):
  the position object's x/y contribute only their part past the whole pixel. A state is
  `(shape, key, cell)`.
- **cell**: (a) canonical `Rt2` structure index; (b) the position object's (player, else
  `player_spawn`) whole-pixel (x,y), or `NO_CELL`. Storage, pos graph, level -1 use (b).
- **region**: (a) kernel region: 16 px square of player x,y + speed [-S,S] (S<=7, default
  6) + rem [-0.5,0.5); (b) storage region: 8 (or 16) cells square, nested in (a).

**Error is derived, never carried** (`trace::error`): `error(n) = own(n) or OR(error(args))`,
own errors only where evaluated on the lane's path: `Flr` spanning two integers; `Sel` with
undecided condition; fork coverage (`SplitOk`); interval Add/Sub/Neg overflow (`NoWrap`);
widening containment (`SlotErrors`); `Restrict` on the raw cell (charged to every outcome);
unfinished unrolled loop; pins.

**Post-trace** (`verify::split_undecided_selects`): split every STORED select whose condition
is lane-undecidable at its most upstream unknown, refold, merge equal outcomes; platforms
decide comparisons over `verify::Points` (world x player pixel) instead of splitting.

**Invariants.** Every lane ends in exactly one outcome or RAISE (RAISE fires only from
merge-lost correlations, tr-11). Refusals fail the (shape, region) build, never silently.
`a_traced_frame_agrees_with_the_oracle`: at concrete points exactly one outcome claims each
point, its error is false, fields equal the `Concrete` oracle.

## 4. Abstraction levels

`abstraction::Level { held, fruit, floors_near, platforms }`, spec `r0sx[h][f][n][p]`
(prefix fixed, flags in order), process-global. Widening is implemented TWICE and must agree
(tr-1): in-graph at frame end (`trace::widen::widen`, containment owed per lane) and on
blocks (`Rt2::widen_to`, for the concrete lookup and the ladder projection). Disagreement
silently prunes real paths; `follow` and `known` detect it.

**Rules.** Never widen without something EXACT that refutes it; replace a value only by a
visible superset (range or unknown); a widened range is a fixed point checked per row.
Refuters: the arcs (remainder), the concrete search (everything else), optionally a finer
ladder level first.

| widening | what | refuted by |
|---|---|---|
| remainder (all levels) | player `rem.x/y` := [-0.5,0.5), containment owed; transfer recorded per edge | arcs (exact) |
| `h` | `p_jump`/`p_dash` unknown; input forked 2 ways (a held button may retrigger) | concrete |
| `f` | fly fruit `step`,`y` unknown number, `fly` unknown, `spd.y`/`rem.y` owed literal ranges | ladder + concrete |
| `n` | fall floor `state` [0,2] with `collideable` derived, exact where the player certainly overlaps; countdowns unknown number; spring/balloon phases ranges | concrete or ladder `r0sxhn,r0sxh` |
| `p` | platform `x`,`last` := path [-16,128] (`last==x` identity), `rem.x` owed literal; worlds via `Points` | concrete (ref engine refuses `p`) |
| strawberry bob (all levels, undocumented, tr-6) | `off` [0,39], `y` start +-2.5 owed | concrete only |
| timers (all levels, undocumented, tr-7) | `frames/seconds/minutes/deaths`:=0, key `spr`:=8, `flip.x`:=false | nothing: value replacement, claimed unread |

**Exact canonicalizations.** `widen_dash` (`dash_effect_time := max(det,0)`, read only as
`>0`); `canon_balloon_offset` (full period -> [0,1), read only by `sin`); `ABSENT_AS_ZERO`
(missing floor/spring `delay` written 0; tr-8); position-free keys.

**Restrict.** A bound (kernel region, speed, rem, platform x/spd) enters ONLY as
`Op::Restrict(lo,hi)(cell)`: value unchanged, range attached, read by every range analysis;
its own error checks the RAW cell in every outcome, so no fold can remove the check; codegen
= identity + two compares. Two literals are owed instead (tr-12).

**rnd.** An interval at every level including the concrete search, which forks each
straddling comparison independently (tr-13): sound for refutation, best case for
confirmation; witnesses are replayed per seed.

## 5. Kernels

**Pipeline** (once per process per level, lazily; nothing cached on disk):
1. Lattice walk (`trace::kernel::room_constant_lattice`): monotone fixpoint over (shape,
   kernel region) from the start; per-shape pins = intersection of constants.
2. Lower + specialize (`emit::lower_frame`, `lower::specialize_frame`): enumerate forks per
   outcome cone; decide by interval fold + local BDD + fold; fuse candidates with equal
   roots; drop `live == false` bodies.
3. Assemble (`asm::codegen::compile`): SSA + GVN, schedule, linear-scan allocation, AVX-512
   text, `gcc -shared`, `dlopen`.
4. Run (`Registry::run_chunk`): bucket lanes by (shape hash, region), slices of 16; per
   body take `live & !error & valid & !skip`; dedup; level -1 drop; key; transfer id;
   pos-graph pair; `sink.emit`.

**Contract.** Every emitted row is a reference-engine successor widened to the level (the
kernels may over-approximate where the reference splits an interval; never miss). A lane the
kernels cannot take is FATAL `KERNEL COVERAGE GAP` (`FrameEngine::run_bucket`): no kernel
for (shape, region) (`[asm] MISS`), or a declined lane (`live & error`; unknown error reads
as error). No interpreter fallback. Every log must show `kernel lanes: missed 0`.

**Folds relied on** (exact, not merely sound): the interval fold over `Restrict` ranges
(refining rules excluded: they would change which lanes error); the map fold in two forms,
`tile_flag_over` (union rectangle has no flagged tile -> false, intersection has one ->
true) and kernel-opt's `mget` over an interval rectangle as a SET of tiles (decides
`spikes_at`; lowering only, level -1 and `Points` keep TOP); local BDD (constant over all
atom assignments); dead forks -> `ANY_VALID`. Kernel-opt codegen: decided boolean planes by
bit identities (`x&-1=x`, `c?t:t=t`, ...); `/2^s` (s<=14) as a biased shift, `%2^m` as a
mask; saves only of live registers around call-outs (exact: the allocator never splits
intervals); constant rematerialization. Checked by per-op bit-exactness tests against the
engine primitives (`asm::tests`), the oracle test, `ref-check`, `KEY_CHECK`, and byte
identity of kernel-opt against `fg-2300`.

**Call-outs** (`asm::callout`): `div` (general), `rem` (non-pow2), `sin`, `mget`,
`tile_flag_at` (uniform / per-lane w,h): each calls the engine primitive, exact by
construction. **Known hole:** interval `Mul`/`Div` by a constant has no `NoWrap`.

## 6. The forward frame step and storage v2

**Entry.** `frame::grow_tree(dir, to, level -1, marks, engine)`: reuse a tree complete
through `to` (`edges/done.txt`), else resume or start, raise if needed, `extend` frame by
frame. Horizons count STEPS (2 per frame under split frame).

**Frame contract.** In: layer f-1 (id order, skip masks). Out: layer f = every successor
state not visited at any earlier frame, once; an edge for EVERY (source, target) emission
incl. old targets; the frame's drop notes; the pos graph.

**Visited set** (`storage::visited`): per (shape, storage region) ENTRIES = (position-free
key, mask over the region's cells); read-only during a wave, written only by translation.
**Ids** (`StateId` = region<<38 | entry<<8 | local; region = shape index x slots + slot):
entries numbered in KEY order among a frame's new entries, shapes in hash order, so ids do
not depend on threads (they do on raise history).

**The wave** (`storage::wave::run_wave`):
1. Units = runs of 1024-4096 live frontier rows of one block; workers pull units.
2. `UnitSink::emit`: level -1 drop (note the source); ladder verdict (batched); a LID per
   distinct target entry (shape, slot, key) per unit, owner looked up once. Cell in owner's
   mask -> old state, edge only. Else a REQUEST; the first claimer copies the row (`Claims`).
3. Unit end: edges sorted by target, transfers ranked per unit, encoded, streamed to `.blk`.
4. Translation (`wave::translate`): requests by target region, one worker a region, sorted
   (key, cell): new keys -> entries, new cells -> states; every lid gets its owner.
5. Layer (`gather_layer`): new states in id order, pieces of 2^17 rows.

**Edges.** An edge recorded at frame f leaves layer f-1. `edges/fNNN.bin` (v5): per unit
its sources (ids, or a row range of f-1's file), its block (per lid: cell byte, varint source
deltas, transfer rank), rank -> global transfer id; per file the OWNER INDEX `(region, entry,
unit, lid)` sorted (the reverse walk). One content-canonical transfer table per tree
(`xfer.bin`). ~3.7 B/edge + ~20 B/lid.

**Transfers.** Per axis `move` does `rem += ox + 1/2; amount = flr(rem); rem -= 1/2 +
amount`, then `|amount|+1` pixel steps: mod 1 a rotation by `ox`, cut once per axis; a
blocked step sets rem 0. A transfer per axis = GUARD (arc of input remainders) + ACTION
(`Rotate(r)`, or `Const(c)` for a blocked step or spawn), decoded from five captured roots
(`took, pre, frag, ox, fin`; `arc_edges::decode_axis`). Inconsistent captures and a second
player split on an axis are refused; a body without transfer roots fails the build. A
platform carry is whole pixels; a wall-blocked carry is `Const`. Never in the row or key.

**Wins.** Exiting rows are checkpointed, not expanded (`not_expanded`: exits, left-room
under a win rect, 100% berry lost, orb deadline). First win: `reaches_win`; checkpoint win
rows (the backward's seeds): `wins_of`.

**Pos graph** (`search::pos_graph`): per frame the (src cell -> dst cell) pairs of all
emissions incl. level -1 drops. Used by a gate, level -1 audits, the UI.

**Split frame** (`CELESTE_SPLIT_FRAME`, `celeste-minimal-split.lua`): part a = timers,
freeze, objects up to and including the player's `move`; part b = the player's update and
the rest. Kernels ~10-20x smaller; states at frame boundaries equal unsplit (test). Tree,
arcs, marks, level -1 count steps; the concrete search steps whole frames and looks up W at
even steps; an arc optimum of s steps bounds the game at ceil(s/2) frames.

**Trim rows** (`CELESTE_TRIM_ROWS=1`): once f is trusted, f-1 keeps keys, ids, cells, wins.
Raise, arc-check, rerun-row, follow refuse a trimmed tree.

**Determinism.** State sets, ids, frame files, meta, xfer table and edge CONTENT are equal
across thread counts and resume; edge FILE bytes (units, lids, owner index) are not. Gates
fingerprint (shape, key, cell), never ids.

## 7. The backward

**Remainder-free BFS** (`search::edges::bfs`): seed every layer's win rows; for i = H-1..1
mark predecessors (`EdgeStore::preds_at`). A mark's DEADLINE is the last step from which the
state wins with SOME remainder. Marks = masks per entry, ranked densely (`MarkRanks`).

**Graph load** (`arc_dp::load`): only edges between marked nodes, unit by unit, two passes
(degrees, CSR fill; 12 B/edge), each with its transfer id; the start added with deadline 0.

**Winning sets** (`arc_dp::backward`): `W_t(n) = U_e pull_e(W_{t+1}(dst_e))` while n is
live (layer <= t <= deadline); a win node holds the whole torus; `pull` = preimage inside
the guard. Sets are `arcs::Region`, canonical (y slabs of x segments), so equal sets are
equal values. EXACT in the remainder: "intersect, then rotate/const" distributes over
unions; arithmetic is integer. Incremental (only predecessors of changed nodes); stored as
spans (node, t-range, set id) into an interned arena.

**Optimum** (`arc_dp::optimum`): `steps = H - max{t : p0 in W_t(start)}`, p0 = (0,0) (no
player at the start). One backward serves every horizon <= H (layers never bind from the
start; tested). A LOWER BOUND on the game: the graph over-approximates every widening but
the remainder, and every filter keeps a superset. Empty -> H is REFUTED.

**Reach marks** (`arc_dp::reach`): `R_{t+1}(m) = U push_e(R_t(p)) n W_{t+1}(m)` from p0; a
set over 32 segments becomes W_t (superset). Per node the last t with R non-empty; feeds the
next ladder level and `arc.marks.bin`. A concrete winner's remainder is in R by induction.

## 8. The concrete search

`arc_dp::concrete_search`, at the LAST level only, skipped by `--no-witness`.
- **State**: exact reference-engine `Rt2` rows; a step = one game frame (both halves under
  split frame).
- **Successors**: all 64 inputs (skip one equal on every button the frame read to one
  already run), every `rnd` leaf.
- **Admission**: a winning successor is accepted; otherwise its projection onto the level
  (`lookup_keys` via `Rt2::widen_to`) must be a node whose `W_t` holds its EXACT remainder.
  Nothing else prunes (level -1, BFS marks and the ladder filter are already inside W).
- **Dedup**: UNWIDENED key + cell (128-bit hash), per layer; never a widened key (`522de36`).
- **Phases**: DFS for a win exactly at the bound (one engine, 200k steps; a win before the
  bound is fatal, "the backward lost a path"; exhaustion proves nothing), then a parallel
  BFS by layers inside W: the first layer with a win is the optimum, in deterministic
  (parent, input, leaf) order.
- **Out**: `<dir>/witness_frame_F.txt`, `[gate] ... concrete optimum`. `--prefer FILE`
  orders the known route's inputs first and runs the known-route check (sec. 11).
- **Cost** = concrete states inside W, set by the widenings: hundreds of steps when the bound
  is tight; blew up in room (7,0) at `r0sxhn` (bound 80 vs 84), hence the ladder.

## 9. Filters on the forward: level -1 and the objects ladder

Both drop rows at emission (`UnitSink`), are monotone in the frame (a recorded expansion
reused at a later frame only over-approximates), and are audited by the known-route check.

**Level -1** (`trace::level_minus_one`, `CELESTE_LEVEL_MINUS_ONE="H,S"`, H in STEPS).
- Table `d(shape hash, cell)` = a lower bound on frames to the room EXIT. Nodes are (shape,
  player cell); every other input is one range per shape (rem whole, speed [-S,S], the rest
  found inductively). Built by tracing each shape, specializing per fork configuration,
  evaluating over the ranges; BFS + inductive fixpoint (ranges widen until closed).
  `sound_d`: backward shortest path; exit edge 1; death = 1 + respawn chain + d(start).
- Filter: drop when `f + d > H`. The screen window is CHECKED (panic outside); rows without
  a table node are kept. Refuses `win_rect` rooms (summit, synthetic wins).
- One table per room, horizon-independent; cached in `/var/tmp/celeste-l1-cache` keyed on
  the binary, Lua, room, S and all `CELESTE_*` but five.
- A tree records its filter (`level_minus_one.txt`) and is extended under it, not the
  environment's; drop sources are noted (`dropped/fNNN.bin`) for the raise (sec. 10).

**Objects ladder** (`--level A,B`, e.g. `r0sxhn,r0sxh`; `frame::MarkFilter`).
- Level i>0 keeps a row at step t only if its projection onto level i-1 is REACH-marked
  there with deadline >= t; mid-frame (odd) steps pass (the `5ccf712` fix).
- Sound: the remainder is exact at both levels and the coarse level over-approximates the
  objects, so a fine winner projects into R_t.
- The filtered tree is reused only if `filtered_for.txt` matches. Use the ladder where the
  coarse bound is well below the reference ((7,0), (4,3) nodiag, gemskip rooms).

## 10. Persistence

**Layout** `<dir>/level{i:02}/`: `frames/fNNN/{s<shape>_<seq>.bin, meta.bin,
meta.r<seq>.bin}` (checkpoint v12: raw columns + key, id, cell; win rows, trimmed flag);
`edges/{fNNN.bin, fNNN.wWWW.blk, fNNN.rSEQ.bin, xfer.bin, xfer.len, done.txt}`;
`dropped/fNNN.bin`; `level_minus_one.txt`; `posgraph.{bin,frame}`; `filtered_for.txt`;
`raising.txt`; `metrics.jsonl`. Witness: `<dir>/witness_frame_F.txt`.

**Trust and resume.** Frame f is trusted when `done.txt >= f`. Resume = rerun the same
command: later frames deleted; visited set rebuilt from metas + id columns (key equality
checked); frontier = last layer with skip masks; the arc phase reruns.

**Raise** (`ForwardState::raise`): a tree under level -1 at H, extended to H' > H,
re-expands frame by frame exactly the noted sources with `f+d <= H'` plus what the raise
added, against the whole visited set; new states join their layer (`Layer::Raised`), new
edges go to `fNNN.rSEQ.bin`. Contract: equal to a fresh tree at H' ((key, cell) sets, edges,
pos graph, notes); a hit on a state of a LATER layer is refused. Refused: trimmed trees,
the orb room, another S or table, unrecorded filters, an interrupted raise.

**Not on disk** (resuming under another value is on the user, gl-2): the level spec,
`STORAGE_REGION`, `SPLIT_FRAME`, `REGION`, the room, category flags.

## 11. Verification surface

| check | asserts | protects | cost today |
|---|---|---|---|
| quick suite (169 tests) | oracle, per-op asm bit-exactness, backward = definition, reach | all | ~47 s |
| 3 `#[ignore]` tests | resume = fresh; (7,1) table start d 45; isolated-floor pin | resume, level -1 | minutes |
| ckhash + posgraph gates | (key, cell) sets f0-44 and cell pairs at f44, room (1,0) | forward, kernels, tracer, storage | seconds |
| arc gate (`--win-at 9,101 --to 35`) | marks count+fp, W fp per frame, optimum 33, witness | backward, transfers, concrete | small |
| known route (`--prefer`, `check-known`) | a known solution survives every pruning step (level -1, ladder filter, node exists, W_s holds exact rem, reached deadline); prints the first failing step | soundness of all pruning | <1 s/level in search; check-known ~25 s |
| `arc-check` | every edge has a transfer; sampled transfers vs the reference inside/outside guards; `--fault` self-test | transfer recording | (1,0) f1-66: 23 min; object level (4,2) did NOT finish in 1.5 h on v2 |
| `ref-check` | kernel vs reference successors by field (ref-only = soundness gap) | kernels | per frame; object level hit 60 GB once |
| `KERNEL_KEY_CHECK` | kernel key = `Rt2::boundary_canonicalize` per row | keys | slower; other code path (fw-6) |
| `missed 0` | no coverage gap | kernels | free |
| `gates/raise.sh` | raised tree = fresh trees | raise, notes, edge files | minutes |
| `follow`, `l1-check` | a route's keys are in a tree / no route state too late | widen agreement, level -1 | seconds |
| `bounds-audit`, `diag-project` | no row outside Restrict ranges / fine projects into coarse | Restrict, one widening | per tree |
| `bench-storage` | a replayed capture reproduces ckhash lines | storage determinism | seconds |
| witness replay (sec. 12) | exits on real PICO-8 | everything | seconds |

Re-pin a gate only on evidence the SETS did not move (posgraph, per-frame kept counts, marks,
first win, optimum identical).

## 12. Real-play validation

**Replay** (`pico8_diff/replay.py`): builds a `.p8` from the ORIGINAL cart (`--lua
.../celeste.lua --begin-game`) or the minimal one, runs `pico8 -x` headless, prints a line
per frame; the caller reads the frame the room changes. `--room`, `--balloon-seeds` (chest
seeds too, UCT semantics), `--jank J`, `--dump`, `--one-dash`. Asserts nothing itself.

**Loading jank J.** The leaving player calls `next_room()` inside `foreach(objects)`; `all`
resumes at the same index in the new list, so the new room's objects from index J update
once on the loading frame. J = 1 + objects before the player at exit (previous room's
objects - spawn - destroyed + title/smoke if early). J is HISTORY: the sound start is the
set of feasible J's grouped by the start state they give (`chain.py feasible_janks /
start_classes`, via `concrete_run --dump-start`); the runner searches each class; the
history-widened optimum is the least. Only J moves objects.

**Chains** (`pico8_diff/chain.py`): rooms played through real transitions with database
files; `--boot` from 100m is GROUND TRUTH for start and verdict; `--check-start` compares
the search's start field by field.

**Other emulators.** UCT (the database's checker; doubles, no loading frame) and Celia (the
community tool; jank with the ORIGINAL object count). `tools/uct/check_uploads.py`: VALID =
the boot chain exits by the file's count (with berry for 100%) AND Celia finishes by it; UCT
is information only. **Policy:** submit the PICO-8 16.16 optimum; find a variant (another
optimal witness, seed nudges, the cut) that also passes at the SAME count; never lengthen.
Celia should be fixed upstream (parked); until then both must pass.

**Pipeline** (`tools/category_runner.py`): boot-chain J -> reference replay (offset scan) ->
`search --ceiling/--to --prefer` per start class -> original-cart replay (jump
canonicalization, chest-seed variants, nodiag/berry checks) -> upload cut by real play ->
`align_tas.py` + `export-ui` when improved.

---

## Annex A [SIZE] (wc -l incl. tests; storage-v2 -> merged)

| subsystem | lines | main files |
|---|---|---|
| numbers, map, names (core + names crates) | 1,354 | pico8_num 645, collision_cache 308, gen.rs 243 |
| block model (engine crate) | 2,232 | runtime2 1264, kernel 803 |
| Lua | 1,969 | minimal 894, split 934 |
| tracer (src/trace w/o ref engine, level -1) | 11,602 | interp 2151, verify 2083, domain 1611, widen 941, kernel 924 |
| reference engine | 1,275 | refengine 436, refbridge 364, refdomain 359 |
| level/start/win (abstraction, concrete, game_runner) | 401 | |
| kernels (transpile + asm + compiled) | 8,653 -> 10,351 | codegen 2000->2488, asm_kernel 1816->1881, graph 1611->1697, bdd 951, mix 716 new |
| forward + storage + persistence | ~6,870 | frame 2082, storage 3625, checkpoint 579, pos_graph 324 |
| backward | ~2,500 | arc_dp 1749 (concrete ~420 of it), arcs 546, arc_edges 322 |
| concrete search + known route | ~690 | arc_dp ~420, known 264 |
| level -1 | ~2,400 | level_minus_one 2210 |
| CLI `rewrite.rs` | 2,105 | 20 subcommands; search arm ~100 |
| other diagnostics | ~540 | storage/bench 366, search/inspect 169 |
| UI | 1,092 Rust + ~6k TS | ui_export.rs, ui/ |
| Python/shell tooling | ~2,500 live, ~660 dead | category_runner 466, replay 405, chain 372, uct 452 |
| total Rust | 41,338 -> ~43,000 | |

## Annex B [KNOB]

**33 `CELESTE_*` env vars read in code** (incl. KERNEL_MIX from kernel-opt). L live, T
tuning/resource, D diagnostic, X dead. Counts: semantics/category 9, pruning/storage shape 5,
resources 7, diagnostics 12.

| var | effect | status |
|---|---|---|
| START_ROOM, WIN_AT_XY | start room; synthetic win (set by `--room`, `--win-at`) | L |
| LOADING_JANK | J or `none` | L (runner always) |
| NODIAG, GEMSKIP, HUNDRED | category semantics | L |
| BALLOON_SEEDS, CONCRETE_BALLOON_SEEDS | fixed balloon draws; the second only for the concrete phase, copied into the first (bw-3) | L |
| SPLIT_FRAME | two steps per frame | L |
| LEVEL_MINUS_ONE, L1_CACHE | "H,S" filter; table cache dir/off | L; L1_CACHE knob unused |
| TRIM_ROWS | trim old frames | L opt-in |
| REGION, STORAGE_REGION | kernel region "px,S"/off; storage side 8/16 | T (neither recorded in a tree) |
| THREADS, WALK_THREADS, BUILD_THREADS, UNIT_LANES, MIN_FREE_GB, MAX_TRACE_NODES | resources | T; all but THREADS undocumented/no users |
| ROOT | repo root | X (always ".") |
| KERNEL_KEY_CHECK | key check; disables claims; `=0` also enables | D (gate) |
| KERNEL_EXPLAIN, KERNEL_MIX, BUILD_TRACE, ASM_STATS, LATTICE_TRACE, WALK_REGIONS, PHASES, EMIT_CAPTURE(_FRAME), EDGE_DUMP, L1_PATH | logging, dumps, captures | D; ASM_STATS, LATTICE_TRACE, EDGE_DUMP have no users |

Non-CELESTE: `PICO8`, `MIMALLOC_PURGE_DELAY` (asserted). Dead names in docs only: WIN_RECT,
ARC_EDGES, BAND, LADDER, LADDER_RUNGS, BACKWARD, KEEP_RAW, EMIT_BUDGET_GB, KERNEL_SETS,
SKIP_CENSUS.

**Binaries.** `rewrite` (20 subcommands), `transpile` (level -1 only), `concrete_run` (`-i
-f --object --dump-start`).

| subcommands | role | status |
|---|---|---|
| search (`--to --ceiling --level --checkpoint-dir --room --win-at --no-witness --save-marks --prefer`), check-known | the pipeline | L |
| forward, ckhash | gates | L |
| export-ui, trajectory | UI data; fit a community TAS to the minimal cart | L |
| arc-check, ref-check, follow, l1-check, bounds-audit, diag-project | verification | D |
| bench-frame, bench-storage | benchmarks | D |
| cell-growth, col-census, coarse-census, spurious, rerun-row, edge-census | censuses | D (edge-census stale) |

## Annex C [FEATURE]

| feature | size | dependents / notes | live |
|---|---|---|---|
| objects ladder | ~60 + reach ~140 + `filtered_for.txt` | (7,0), big nodiag/gemskip rooms | yes |
| split frame | 934 Lua (70-line diff), steps/frames plumbing everywhere | rooms (6,0), (6,1) | opt-in |
| raise | ~350 in frame.rs + notes + raised edge/meta files | count-up campaigns under level -1 | yes |
| level -1 | ~2,400 | most forwards; raise | yes |
| trim rows | ~20 | big trees; blocks raise and diagnostics | opt-in |
| pos graph | 324 + hooks | a gate, UI, level -1 audits | always on |
| nodiag / 100% / gemskip | ~25 / ~110 / ~15 Rust + runner | categories | yes |
| loading jank | ~60 Rust + chain.py/replay.py | every real-play result | yes |
| UI export | 1,092 Rust + ~6k TS | `--save-marks`; still carries ladder-era concepts | yes |
| bench-storage / kernel mix | 366 / 716 Rust + 276 Py | perf work | diag |
| robust path, playable | branches `robust-path`, `playable`, not merged | | no |
| dead tooling (~660) | tools/server.py, tree_viewer.html, framesetdiff, gdiff, gjoin, rowset, rowdiff, posgraph_pairs, key_soundness, key_unsound, shape_share, fuse_frontier; lua_tests/ | formats no longer written | dead |

## Annex D [JANK] (file/function: why)

Names and concepts
- tr-1 `trace::widen` vs `Rt2::widen_to`: every widening twice, synced by convention.
- tr-2 "outcome" = guarded successor; `lower.rs` has a second `Outcome`.
- tr-3 "cell" (structure index / player pixel), "region" (kernel / storage), "lane" (row /
  zmm slot), "key" (row key / shape hash / `verify::RowKey` / `pm1_key`) overloaded.
- tr-4 three shape identities: `heap::Shape`, u64 FxHash `shape_hash` (no collision check),
  the walk's `String`.
- tr-5 "concrete" = `domain::Concrete` oracle, `concrete.rs` utilities, and the "concrete
  search", which runs `RefDomain` intervals.
- kn-1 five "kernel" modules (`transpile/kernel.rs`, `trace/kernel.rs` = the lattice walk,
  engine `kernel.rs` = lane primitives, `compiled/asm_kernel.rs`, `AsmKernel`); two `AsmBody`.
- kn-2 `bin/transpile.rs` only does level -1.
- fw-1 two "visited" (`frame::Visited` is a marks set), two "marks", two "edges" modules
  (`search/edges.rs` is the BFS).
- bw-1 one u16 "deadline" with two meanings (BFS vs reach); marks file stores H - deadline.
- bw-2 two remainder-set types (`arcs::Set/Rects` vs `arcs::Region`); two witness files.

Widenings outside the rules
- tr-6 `widen::widen_fruit`: strawberry bob widened at EVERY level, not in abstractions.md.
- tr-7 `widen::widen_timers`: value replacement, no owe (frames, deaths, key `spr`/`flip`).
- tr-8 `ABSENT_AS_ZERO` + `cart::check_absent_fields` (syntactic count) for a nil-or-number type.
- tr-9 countdown atoms recognized by FIELD NAME (`domain::COUNTDOWN_FIELDS`).
- tr-10 with `p` an overlapped floor stores its computed value because an owe failed for
  unproven reasons.
- tr-11 RAISE rows exist only because merges lose spring/floor correlation.
- tr-12 owed input literals (platform `rem.x`, fruit `spd.y/rem.y`): a third bound
  mechanism beside pins and `Restrict`.
- tr-13 `RefDomain::compare` forks each straddling `rnd` comparison independently: the
  "exhaustive concrete" search admits draw combinations no draw gives.
- bw-3 `arc_dp::concrete_engines` copies CONCRETE_BALLOON_SEEDS into BALLOON_SEEDS in the
  process env and removes it; `lookup_view` resets balloon offset/y to the tree's interval.

Global state, steps vs frames
- gl-1 level is a global Mutex yet passed as a parameter in places; `Block::keyed` reads
  `current_level().held` only; one resident kernel set.
- gl-2 level, STORAGE_REGION, SPLIT_FRAME, REGION, room, category flags not recorded in a tree.
- gl-3 knobs cached in `OnceLock`s; tests mutate the env (needs nextest isolation).
- sf-1 `--to/--ceiling` in frames; LEVEL_MINUS_ONE H, tree labels, `Found.frame` in steps.
- sf-2 `follow`, `l1-check` call `RefEngine::step` per input byte: wrong under split frame.
- sf-3 split cart is a 934-line near-copy, not a patch; `__phase`/`__frozen` tables force shapes.

Forward, storage, persistence
- fw-2 `frame::extend` saves the pos graph AFTER `done.txt`: a crash in between leaves a tree
  `resume` refuses ("pos graph covers ...").
- fw-3 three trust markers: `done.txt`, `xfer.len`, `posgraph.frame`.
- fw-4 forward's first win uses `reaches_win`, checkpoint win rows and resume use `wins_of`:
  `win_frame` can differ after a resume (100%).
- fw-5 `extend` applies `not_expanded` only on a frame that `won` (or orb room), `resume`
  always: a 100% berry-lost row is expanded fresh but skipped after a resume.
- fw-6 `CELESTE_KERNEL_KEY_CHECK` turns claims off (another code path); `var_os`: `=0` enables.
- fw-7 flag parsing inconsistent: SPLIT_FRAME any value, TRIM_ROWS/PHASES `=="1"`.
- fw-8 the checkpoint `cell` column duplicates what the id encodes.
- fw-9 edge files not self-contained (sources as row ranges of f-1's frame file).
- fw-10 frame numbers parsed as 3 digits (`EdgeStore::open`, `discard_after`): breaks at
  step 1000 (split frame, long rooms).
- fw-11 `save_value_to` stamps the checkpoint `FORMAT_VERSION` on every side file.
- fw-12 `ForwardState::start/resume(record)` only controls the pos graph.
- fw-13 leftovers: `Block::keep/with_ids`, `one_frame`, `bench-frame`; sentinels
  `NONE/NO_OWNER/PENDING`; doc words "queue", "flush", "door".

Backward, search
- bw-4 `known::check` takes level -1 from the env, not the tree's `TreeFilter`: false
  PRUNED or a missed check.
- bw-5 `solve` hard-codes p0 = (0,0); known.rs special-cases the start by id.
- bw-6 `NodeKeys::new` keeps the lowest node on a duplicate (shape, key, cell), silently.
- bw-7 concrete dedup = 128-bit hash, per layer only; DFS memo keyed with depth.
- bw-8 magic constants: `REACH_SEGS=32`, `DFS_BUDGET=200k`, `CHUNK=256`, 256 trace states,
  `PLATFORM_WORLD_FRAMES=127`, 20 px floor isolation.
- bw-9 `CostToGo::admitted_from` panics outside the window: the forward crashes.
- bw-10 `frame::cell_too_late` ("the time band") is a leftover of `CELESTE_BAND`.

Kernels
- kn-3 three evaluators with different `mget` semantics (`eval_lenient_in` bails,
  `eval_fold_in` sets, `eval_narrow_top_in` TOP); two map folds with different clipping.
- kn-4 `drop_redundant_reloads`/`constant_key` re-parse emitted AT&T text.
- kn-5 the miss report is a lane count, reasons only on stderr; a decline explains itself
  only on a rerun under `CELESTE_KERNEL_EXPLAIN`.
- kn-6 key-exclusion rules spread (`widen_uniform`, `konst_av`); `unify` makes a shape's
  skeleton depend on which region kernels exist.
- kn-7 a skipped lane's error is ignored (special case from room (6,2) f84).
- kn-8 `BuildPurgeDelay` mimalloc hack (raw option id 15); MB-sized kernel frames need
  `set_thread_stack` on every caller thread.
- kn-9 dead `Emit.decide=false` path; `TileFlag` vs `TileFlagLanes`; stale "saves all 32 zmm".

Cart and room special cases
- rm-1 cart patches by exact-whitespace `replacen` in Rust (`forbid_diagonal_dashes`,
  `fix_balloon_seeds`, `apply_loading_frame`, `apply_start_room`) AND again in replay.py.
- rm-2 `ORB_LEVEL=21`, the `x==7` wrap, the summit rect, `orb_required` in `reaches_win`;
  `OBJECT_TILES`/`minimal_jank` remap J between carts.
- rm-3 builtin lists disagree (`cart::NATIVE` declares unused `__widen_rem`, `__new_vector`,
  `__split_at`); frozen `GLOBAL_NAMES` preserves dead ABI.
- rm-4 tooling: chain.py `OVERRIDES` (gemskip100 28), jank constants (title 36, smoke 15),
  runner `canon_jumps`, chest-seed variants, seed nudges, offset scan +-2, hardcoded
  `/home/philippe/...` paths in align_tas.py.

## Annex E: plans vs code

- architecture.md, level-minus-one.md: level -1 drops "at the flush" / per "queue"; code drops
  per emission (`UnitSink::minus_one_drop`), H in steps.
- architecture.md "The search" step 4: "at a non-last level only the try at the bound runs";
  since `86b4a6d` nothing concrete runs there.
- architecture.md frame-end order; code: notes, checkpoint, meta, done, trim, pos graph (fw-2).
- architecture.md "a function of the frame": true for states, ids (absent raises) and edge
  content, not edge-file bytes. `CELESTE_WIN_RECT`, `rewrite arc-proto`: gone.
- abstractions.md: field `floors` is `floors_near`; `HeldPrecision` etc. do not exist; "the
  reference engine refuses `f` and `p`": only `p`; omits `widen_fruit/timers/dash`.
- storage-v2.md id layout 24/32 bits; code 26/30. Owner-by-lid figures pre-`fa2b2dc`.
- known.rs, architecture.md: the check uses level -1 "as the search ran"; code uses the env.
- CLAUDE.md: door, queues, runs, RowCache are gone; line counts stale.
- storage-unify study (`origin/storage-unify`): do NOT build unified per-region sets (blocks
  4-9% bigger, no records saved); instead edge file v6, delta-varint owner index + varint
  starts (~20 -> ~7 B/lid, -5 to -9% of the edge store).
