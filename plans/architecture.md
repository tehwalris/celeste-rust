# Architecture (2026-10-16)

The system as it is: what the search does, the invariants every change must
keep, and where it is open. Elsewhere: the level flags in
plans/abstractions.md, the cost-to-go filter in plans/level-minus-one.md,
the float arithmetic in plans/float.md, results in plans/results.md,
abandoned approaches in plans/lessons.md, the checks in plans/devloop.md.
What the 2026-10-16 refactor left open: "Open issues" below.

## The search in one paragraph

`rewrite search` runs, per level of `--level` (coarsest first):

1. **The forward**: abstract states in columnar blocks, stepped by
   AVX-512 kernels assembled at startup from the traced Lua. Each state
   carries a HULL of the sub-pixel remainders it has been reached with.
   Every edge is recorded with what the frame did to the remainder (its
   TRANSFER). Optionally filtered by level -1 (frames to the exit) and by
   the previous level's reached nodes (the objects ladder).
2. **The backward**: per node and frame the exact set of remainders that
   still win by the horizon (W), one push over the recorded in-edges. Its
   optimum is exact in the remainder and a LOWER BOUND on the game's: no
   win refutes the horizon.
3. **The concrete search** (last level only): the reference engine, every
   input, every `rnd` leaf, pruned only by W. Its first win is the
   optimum and the witness.

Every widening a level makes is refuted by something exact: the remainder
by the arcs, everything else by the concrete search (CLAUDE.md, "Never
widen a field without something EXACT that refutes it").

## Code layout

A cargo workspace. The dependency order is load-bearing.

```
crates/celeste-core     numbers (Pico8Num 16.16, LuaF64, the Arith trait),     deps: -
                        cart data, collision cache, builtins
crates/celeste-names    FROZEN name tables (FIELD_NAMES order = canonical       deps: -
                        field order; append only)
crates/celeste-engine   Rt2 blocks, exact keys, lane primitives, the            deps: core, names
                        widening table
celeste-rust (src/)
  frame.rs              the forward (ForwardState: start, resume, extend),
                        trees (grow_tree, TreeFilter), wins, MarkFilter
  storage/              the visited set, the wave, the region phase, the
                        frame/hull/edge/out files (one container: file;
                        their varints and keys: codec), decode, bench
  search/               driver (the search's levels, each level's arc
                        phase), backward (W, the optimum), reach (inside W;
                        exact), concrete (the concrete search; dual: over
                        pairs), known, arcs, hull, rem (+ rem_f64),
                        arc_edges, arc_check, hull_replay, tree (a finished
                        tree opened), pos_graph, result, ui_export, inspect,
                        level_minus_one (the cost-to-go table, its one
                        builder), robust (a float witness against the libm)
  trace/                the tracer: interp, state, heap, domain (Symbolic) and
                        refdomain (RefDomain), frame (one frame traced), split
                        (the split pass), widen, error, walk (the lattice
                        walk), lower, bind, block (states <-> blocks), cart;
                        oracle and probe (test-only)
  engines/              the reference engine (`reference`: the interpreter
                        over RefDomain as a frame step)
  transpile/            graph IR, garith, ival, bdd, lowering, asm (codegen,
                        float codegen, call-outs, jit)
  compiled/             FrameEngine, the kernel registry, emission, mix
  settings.rs           the run's settings (flags), the tree's record
  starts.rs             the feasible loading-jank J's and their classes
  par.rs, metrics.rs, concrete.rs, game_runner.rs
  bin/rewrite/          the search and its commands (main), the checks of a
                        tree (check), benches (bench), diagnostics (diag),
                        one concrete run (run), level -1 (l1: table, probe,
                        check)
```

The traced program is `lua/builtin_level_3.lua` + `lua/builtin_level_4.lua`
+ `lua/celeste-minimal.lua` (`--split-frame`: its `_update` replaced by
`lua/split-frame.lua`'s, `trace::cart::split_frame`).
Count things there, not in the OCaml project's `celeste.lua`, which is the
reference for semantics only.

## The three interfaces

1. **The frame step** (`frame::FrameStep`): a range of input lanes of one
   block in; every output row out through the storage's `UnitSink`, keyed,
   with its cell (player position), widened for the level. Two impls: the
   compiled `compiled::FrameEngine` and the reference `engines::reference`
   (`RefEngine`: the tracer's interpreter over `RefDomain`, one lane and
   one fork path at a time). Both convert states to blocks through one
   converter (`trace::block`), so their rows key alike.
2. **The block** (`Rt2`): columnar; key and cell exposed, fields opaque.
   A transient: the forward holds states as keys, a block exists only
   inside a wave's unit (decoded right before the kernels) and in readers.
3. **The kernel set** (`compiled::asm_kernel::Registry`): one assembled
   kernel per (shape, region), built at startup with gcc + dlopen. Nothing
   is checked in; what runs is what the tracer makes of this checkout's Lua.

To the outer loop a state is opaque: `(shape, key, cell)`.

## The arithmetic: fixed or float

`--arith fixed` (PICO-8's 16.16, the default and the real game) or `--arith
float` (the TAS tools' Lua doubles: Celia's; `float-uct` for UCT's). It is a
compile-time trait (`celeste_core::arith::Arith`; `Rem` for the remainder
model): the binary dispatches once, and a tree, kernel set and W of
different arithmetics cannot mix. Fixed monomorphizes to the same code and
bytes it had before. `rewrite dual` searches for a run valid in both.
Details: plans/float.md.

**The rule: match PICO-8 exactly, or raise** (Philippe, 2026-10-11). The
interpreter, the kernels and the reference engine compute what the console
computes or raise (in a kernel: decline the lane, a fatal coverage gap).
The game never leaves 16.16, so every overflow, wrap, saturation and `x/0`
is an error, for points and intervals alike. `Pico8Num` has no `+ - * /`
operators: `checked_*`, and the console's `wrapping_*`/`saturating_*`
spelled out for tests. The console's own values stay pinned by the
`lua/probe` overflow corpora.

## The tracer and the kernel model

**Two stages, collapsed into one interpreter.** Tracing turns one frame of
the Lua into OUTCOME graphs with guards (a branch that kills an object
changes the heap shape). The tracer is an interpreter that splits at an
undecidable branch (`state::split`) and merges with a `Sel` per differing
slot (`state::merge`). So the graph depends on the level, and the lattice
walk re-traces per (shape, region, level).

**Lane-undecidability, not abstractness, forces a fork.** A lane is a set
of states computed on intervals; it forks where a condition differs across
the states one lane stands for. A numeric select straddles only if its
arms do; a boolean select exactly when its condition does. Prefer splitting
the UNKNOWN a predicate reads over the predicate (two predicates of one
unknown split apart admit combinations no value gives).

**One fork concept**: choose a node, partition its set, duplicate the
downstream cone per part, fold. Buttons, held trails and escaped atoms are
2-way forks every lane takes both ways. `split::split_undecided_selects`
runs after tracing: split a select driven by something lane-undecidable at
its most upstream place, refold, dedupe outcomes. For the platforms it
carries per path the (world, player pixel) POINTS its answers hold at
(`split::Points`). `lower::specialize_frame` enumerates per outcome only
the forks in its own cone.

**Error is derived, never carried** (`trace::error`): `error(n) =
own_error(n) or OR(error(args))`, where it was evaluated:

| operator | own error |
|---|---|
| `Flr(x)` | the span covers more than one integer |
| `Sel(c, t, f)` | `c` lane-undecidable and not known |
| a fork fragment (`Split`, `SplitInt`; a double's round fork `RSplit`) | the lane spans more parts than enumerated |
| an unrolled loop | its condition still holds after the bound |
| an operation that can leave the arithmetic's range (`GraphArith::range_error`: 16.16's `+ - * /`, neg, `abs`; the doubles' `+ - * / %`) | `NoWrap`: the exact result left the range (16.16's would wrap or saturate, a double's would not be finite); skipped where static ranges bound it, at most two compares when one operand is a literal |
| a widening | the slot's containment (`widen::SlotErrors`) |
| `Restrict(lo, hi)(x)` | the raw input outside `[lo, hi]`, charged to every outcome |
| `Pin(c)(x)` | the raw input not `c`, an error of the whole frame |

A Lua raise (arithmetic or an ordered comparison on a non-number) is its
own outcome (`Frame::raise`): every lane ends in exactly one outcome. It
fires only where a merge loses a correlation (a spring on a fall floor) and
is not reachable concretely.

**The kernel**: the compute graph with every fork resolved, per body two
masks, `live` and `error`; a lane emits where `live and not error`.
Unknown `live` reads as live (over-approximate); unknown `error` reads as
error: a declined lane is a FATAL `KERNEL COVERAGE GAP`. There is no
fallback to the interpreter.

**The lattice walk** (`trace::walk::room_constant_lattice`) walks (shape,
region) nodes from the start in parallel rounds, traced against the
round's lattice snapshot. A region (`--region px,S`, default 16 px, S <= 7
because `move`'s loop is unrolled for `|amount| <= 8`) bounds the player's
whole-pixel x/y, remainder and speed, all checked per lane. A row outside
every region has no kernel and stops the run.

**Input assumptions are nodes on the raw input**, checked per lane
(`Symbolic::assumptions`): `Restrict(lo, hi)` passes the value through with
its range attached (the region's bounds, the no-player phase's hulls, the
platforms' path); `Pin(c)` disconnects the input, every use reads the
constant (lattice constants; inputs read as a literal range so lanes merge:
a platform's `rem.x`, the fly fruit's `spd.y`/`rem.y`). No input cell
carries a range; only a node does. The one assumption outside the family is
open (below).

**The map folds by range**: the lowering's fold reads `mget` over interval
coordinates as the set of tiles in the rectangle, so a region with no
reachable spike tile folds the death test away. Exact: the map is
constant, the hull covers every lane's coordinate, and a lane outside a
restriction is in error by the restriction's own check.

**The codegen** (`transpile::asm`): hand-written GAS, gcc only assembles.
Decided mask planes fold at lowering (bit identities); a call-out saves
exactly the registers live across it (a run of calls keeps values in their
save slots); `/2^s` and `%2^m` are inline; constants rematerialize; the
register allocator is deliberately simple (spills are ~0.4% of a frame).
The kernels are ~2-3% of a hot frame; the rest is emission and storage.

## Keys

A state's key is EXACT (`celeste_engine::exact`): per (shape, value cell)
an append-only dictionary of codes (a number and its point interval are one
code), each field's index in append-only bit runs of a 127-bit key that
never moves as dictionaries grow. A shape needing more bits is fatal. The
append step looks keys up read-only during a wave; a code they lack makes
the key PROVISIONAL until the frame's end appends the new codes sorted.
`--key-check` compares every row's key with `Rt2::boundary_canonicalize`.

The key holds NO POSITION: the position object's x/y contribute only their
part past the cell's whole pixel (`Code::pos`). States sharing all but the
position share a key, which is what a storage entry is. FIELD_NAMES' order
feeds the shape hash and the cell numbering: a reordering is a different
search. A shape's hash is its name; its signature is stored and compared.
The concrete search compares whole states, never keys.

## The room's start

The search starts from `_init()` and `load_room(x, y)`. Real play enters a
room differently: the leaving player calls `next_room()` inside `_update`'s
`foreach`, and PICO-8's `all` resumes at the same index in the new list, so
the new room's objects from index J on get one update on the loading frame
(the "loading jank"). `--loading-jank J` patches that into the Lua
(`trace::cart::apply_loading_frame`; J counts the original cart's objects).
J depends on how the previous room was played: `rewrite starts` computes
the feasible J's grouped by the exact start state they give, and the
category runner searches each class. The boot chain on a real PICO-8 is the
ground truth (`pico8_diff/chain.py --boot`, `--check-start`).

**Frame 0 is the start as the engine made it**, keyed by its own codes
(not its boundary view), and nothing looks it up by projection.

**The no-player phase is explored** (`trace::walk`: `explore_no_player`).
Before the player exists no input matters, so the walk traces the spawn
phase under the hulls its concrete states take. A death reloads the room,
so the walk explores every respawn entry it reaches, and the start, at the
level's own widenings, into the same hulls. No merge selects on the state's
own unknown inputs (`merge_refused_on`). Fatal: a spawn field held unknown,
a phase without a player after 240 frames, an explored state or a frame of
the start's concrete chain that does not trace. The chain alone may end
early, bounding nothing (a `[walk]` line says so): on a start holding an
`rnd` draw (an unseeded balloon's `offset`) or a frame forking into several
outcomes (the level's unpinned widened fields).

## The forward: hulls and storage

**The hull forward.** Each state keeps a REMAINDER HULL (`search::hull`,
per axis one arc of the circle, 8 B): the remainders it has been reached
with, over-approximated. The start's hull is its exact point. A push along
an edge maps the hull's points inside the transfer's guard by its action;
pushes into one target accumulate order-free (`HullAcc`: two charts per
axis, cut at 0 and 1/2, merged by min/max). The frontier of frame t is
every state whose hull GREW at t: NEW states run the kernels (the only
expansion a state ever gets; its edges are recorded at its layer + 1);
GROWN ones re-push their grown hull along their stored out-edges
(`wave::run_repush`, only where the image grows). Level -1 acts at every
push. A full hull is not stored and grows no more.

Sound by induction: a concrete path at state p with remainder r at frame t
finds p in the tree with layer <= t and r in its hull, and its successor
along the recorded edge holds r's image. The hull only decides which states
the tree holds, how early, and which are expanded; the arcs (exact over the
recorded edges) and the concrete search refute what it over-admits. With
every hull widened to the whole circle (`hull::WIDEN`, test-only) it is the
old remainder-free forward. A tree is NEVER raised: a push level -1 dropped
leaves a hull smaller and a state later or unreached, so a tree's frames
depend on its horizon throughout.

**A state is `(shape, key, cell)`** (`storage::Sk`: region, key, cell in
region). No state has a number, in memory or on disk.

**One state set** (`storage::set`): per storage region (8 x 8 cells,
`storage::GEOMETRY`, nested in a kernel region; `NO_CELL` one more region
per shape) a table of ENTRIES, an exact key and a u64 cell mask, sorted by
key. Two forms: PLAIN (arrays: the frontier, a frame read back, the next
layer) and PACKED (zstd blocks of ~1024 entries with each state's layer and
hull: the visited set, ~3 B a state). The visited set is read-only during a
wave.

**The wave** (`storage::wave::run_wave`), one pipeline a frame:
- the frontier is cut into UNITS of whole entries of one shape (~1/256 of
  the frame) and SLICES (256k lanes). A unit is DECODED right before the
  kernels (`decode::decode_rows`); `frame::not_expanded` (exits, a lost
  berry, the orb's deadline) is decided there;
- per emission the unit's sink drops level -1's rows, keys the row and ORs
  it into a BUNDLE (source key, transfer, offset, target key -> target
  cells) keyed by the target's ROUTE (shape, region slot);
- a unit ends as per-route CHUNKS with their pushes, and an OUT block,
  spooled (`storage::spool`). Re-push units make pushes only;
- a finished slice's chunks become REGION JOBS (`route::run_route`): the
  pushes merged into the route's touched states, the bundles encoded into
  a zstd segment of the frame's edge file. The region phase overlaps the
  kernels.

**The frame's end**: provisional keys made final, new shapes numbered,
transfers numbered canonically (by the frame's edge count, then content),
each route SEALED against the visited set (new, grown, or nothing), the
ladder's filter applied, everything merged into the visited set
(`absorb`). Then the FRAME FILE (`frames/f{F}.bin`: per region the new
entries' keys, masks and win masks; the shapes' skeletons), the HULL FILE
(`hulls/f{F}.bin`), the edge and out files, the pos graph, and last
`done.txt`. Frame and hull files are read back and compared with the
forward's state (fatal on a difference). The DECODE GUARD checks a shape's
first emitted row per worker and wave against its decode.

**Resume** rebuilds the visited set from the frame files, replays the hull
files, and takes the last hull file as the frontier. A search resumes by
rerunning the same command. The result is a function of the frame, not of
scheduling: every file is byte-identical at any thread count and over a
resume.

**Per-tree records**: `settings.txt` (the settings that change the tree,
the level included; a run under others is refused, `--override-settings`
passes only `--region`), `level_minus_one.txt` (the filter: `off` or H, S
and the table's fingerprint; a tree serves horizons up to H and is never
raised past it), `filtered_for.txt` (a ladder level's marks, horizon and
filter), `forward.tsv` (per-frame stats), `done.txt` (the one trust
marker: the frame and the transfer table's trusted length).

## The recorded graph

An edge recorded at frame f leaves a state of layer f - 1. It may enter a
state first reached later, or never (its push was empty or dropped).

- **Edge files** (`storage::edges`, `edges/f{F}.bin`): segments by TARGET
  region; a block holds bundles `(source shape and key, transfer, offset,
  target key) -> mask` over the target region's 64 cells, with sorted
  dictionaries and a trie of records. ~0.11 B an edge (room (6,2) 100%,
  f57).
- **Transfers**: one table per tree, `edges/xfer.bin`.
- **Out files** (`storage::out`, `edges/out/f{F}.bin`): the same edges by
  SOURCE, for the re-pushes; a header indexes blocks by entry range.
- **`EdgeStore`**: segments by (frame, region), the target regions a
  source region reaches, a state's in-edges (diagnostics).

## Arcs: the rotation graph

Per axis one frame does `rem += ox + 1/2; amount = flr(rem); rem -= 1/2 +
amount`, then steps `|amount| + 1` pixels (a blocked step sets `rem = 0`).
Mod 1 that is a ROTATION by `ox`, cut at one point. So for a remainder-free
node and an input, the circle splits into two pieces; in each the frame
does one discrete thing and maps the remainder by a rotation, or to a
constant on a collision. The transfer is captured where the tracer forks
`__split_by_flr` on the player's own `rem` (`arc_edges`): guard = the
piece, action = rotation or `Const(fin)`; identity on an axis without a
split. A second player split on an axis in one path is refused (and cannot
happen: `move` is the only splitter). A platform carry blocked by a wall
sets `rem.x = 0` before the move: a split of one point, decoded as a
constant. A body without transfer roots is refused at the kernel build.

`search::arcs::Region` is a set of the torus in canonical form (y slabs
of x segments); `pull` is the preimage under an edge. "Intersect, then
act" distributes over unions, so the union over all paths is exact.

## The search

`rewrite search --frames H [--ceiling] [--level SPEC[,SPEC...]]`; per level:

1. **The forward** to H into `<dir>/level{i:02}` (resumed, or reused when
   it reaches H). Won rows are stored and not expanded.
   - **Level -1** (`--level-minus-one H,S`, plans/level-minus-one.md): a
     push into a cell that cannot exit by H is dropped.
   - **A finer level is filtered** (`frame::MarkFilter`): a state at frame
     t is kept only if its projection onto the previous level is REACHED
     there with a deadline >= t. REACHED (`reach::inside_w`): the start's
     remainder pushed forward inside W, `R_{t+1}(m) = U push_e(R_t(p)) ∩
     W_{t+1}(m)`. A concrete winner is in R at every frame by induction. A
     set past 32 segments is replaced by W_t (a superset). This is the
     OBJECTS LADDER, the only ladder.
2. **The backward** (`backward::backward`): `W_t(n)` only grows as t falls
   (more time never hurts), so `W_t(p) = W_{t+1}(p) ∪ U pull_e(W_{t+1}(m))`
   over the edges whose target CHANGED at t + 1. Frame by frame, the
   changed nodes' in-edges are read from their target regions' blocks, from
   the region's first frame with edges (a hull tree's edge may enter a
   state first reached later), in bounded rounds, and pulled into their
   sources in parallel. No graph is loaded and no node is marked first: a
   node no winning set reaches is never touched. W is kept as varints in a
   scratch file beside the tree over an arena of distinct sets mapped from
   disk. `backward::optimum` reads the optimum off the start's sets: a lower
   bound, and no win refutes H.
3. **The concrete search** (`concrete::concrete_search`, last level only):
   from the room's start, every input (one per class of the buttons the
   frame reads), every `rnd` leaf; a successor is admitted only if its
   projection is a node whose W holds its exact remainder; each exact
   state held once. First a depth-first try for a win AT the bound (200k
   steps at most), then breadth-first inside W: the first layer with a win
   is the optimum, its path the witness (`<dir>/witness_frame_F.txt`).
   With `--ceiling` a refutation or no witness is an error. Its cost is
   the number of concrete states inside W.

The outcome goes to `<dir>/result.json` (levels, bounds, verdict, witness).

**When to use the ladder.** One level where the bound is tight; the
objects ladder (`--level r0sxhn,r0sxh`) where the level-0 bound is well
below the reference. Room (7,0) at `r0sxhn` alone: bound 80 against 84,
the concrete region 6.5x larger per frame of slack (~300M steps to 84);
with the ladder, the filtered `r0sxh` forward 6.9 s, bound 84, the
witness in 292 steps. Count up from the level-0 bound on a kept tree
rather than setting the horizon at the reference: reach marks and level -1
both shrink with H.

**Win conditions**: the room exit (including the wrap from a row's last
room); the summit's flag rect; the orb room's exit WITH the orb (or bare
in the gemskip categories); in 100% and gemskip 100%, the exit with the
berry; `--win-at`, a synthetic win for experiments. Rooms past (5,2) start
with two dashes (one in the gemskip categories). The category is
`--category` in the tasdatabase's names (`settings::Category`): nodiag,
gemskipnodiag and onlydiag make the forbidden dash arm raise, so it is no
move, exactly.

**The split frame** (`--split-frame`, rooms (6,0), (6,1)): `_update` as two
steps around the player's move; kernels shrink ~10-20x with identical
states at frame boundaries. UNITS (`settings::Clock`, where every
conversion is): STEPS are the internal unit, since a mid-frame state is a
node of its own (the backward pulls W through it). The tree and its
files, the edges, W, the marks, level -1's recorded H (`2C` for a horizon
of C frames) and the flags named so (`forward --steps`, the tree
commands' `--steps`/`--step`) count steps. The search's `--frames`,
`--level-minus-one`'s H, its bounds, witnesses and `result.json` count FRAMES;
the concrete search steps whole frames and looks states up at frame
boundaries. The ladder filters
at frame boundaries only (mid-frame rows do not key as the coarser tree's:
room (6,1) wrongly refuted 93 until `5ccf712`).

**The dual search** (`rewrite dual`): each arithmetic's levels to one
horizon, then a concrete search over pairs of states, each half inside its
own W, for the shortest run valid in both (plans/float.md).

## Checks

The check ladder is `./check.sh t0|t1|t2|big` (plans/devloop.md). The
verification surface behind it:

- **The pinned room (1,0) gates** (`gates/`): each frame's states and
  hulls f0-f44 (`ckhash --hulls`), the pos graph, the arc search at a
  synthetic win (W's support and fingerprint per frame, optimum 33, the
  witness), their float twins and the dual gate.
- **The known-route check** (`search::known`): `search --prefer FILE` and
  `rewrite check known` step a known solution through the reference engine
  and FAIL at the first pruning step that drops it: level -1, the ladder's
  filter, the node's W at the exact remainder, the reach marks. Pruning
  errors produce a wrong optimum or a false tie, never a fake improvement,
  so nothing else notices them.
- **Kernels against the reference**: `check ref` (row by row),
  `--key-check` (keys against the boundary), `check widen` (the widening
  table's two sides agree), `check arcs` (every edge has a transfer;
  sampled transfers probed with the reference inside and outside their
  guards). `kernel lanes: missed 0` in every log.
- **The float oracle** (`trace::oracle`): the float reference engine
  against UniversalClassicTas's and Celia's state after every update, bit
  for bit (`gates/uct`, `gates/celia`: database files and our submitted
  routes only).

## Open issues

- **The no-player platform's `spd.x`** (`trace/widen.rs`, the platform
  input hook's no-player branch): read as its one moving speed v and owed
  "0 or v" per lane (0 on the creation frame). The only input assumption
  outside the Pin/Restrict family. Open: why the assertion is needed (the
  move is unobservable without a player, `x` being the path at `p`), and
  whether it should be a table entry plus a pin with a set check. Do not
  change without Philippe.
- **Absent-as-zero** fields (a fall floor's or spring's missing `delay`):
  exact modulo raises, guarded syntactically (`cart::check_absent_fields`);
  the clean form, a nil-or-number column, would re-pin everything.
- **Countdowns are recognized by field name** in the interpreter (the
  list comes from the widening table).
- **With `p`, an overlapped floor stores its computed value** because an
  owe failed for unproven reasons; spurious states the concrete search
  refutes.
- **The split frame with `p`** reaches slightly fewer states than unsplit
  in room (2,1) (unexplained).
- **`check arcs` at object levels** forks every straddling comparison on a
  widened phase (balloons, springs): room (7,0) `r0sxhn` exceeds its path
  cap. A different probe (concrete members of the row) is needed.
- **The transfer capture costs the forward** (five roots per axis per body
  where the transfer depends only on the fork configuration and `ox`).
- **W's fragmentation** decides the backward's cost (small so far: median
  1-6 segments a node).
- **The UI** (`ui/`) still models the deleted precision ladder (horizons,
  kernel re-run iterations); parked by Philippe.
- **Level -1**: the platform rooms' tables do not converge; see
  plans/level-minus-one.md for its precision limits.

**Left by the 2026-10-16 refactor** (its working doc was
plans/refactor.md: `git show c4dd3241:plans/refactor.md`).

- Open candidates:
  - The cart patches are written twice, in Rust (`trace::cart`) and in
    `pico8_diff/replay.py` with different semantics; `pico8_diff/` belongs
    under `tools/`.
  - The engine crate knows the game (`runtime2::BoundaryIds`,
    `Rt2::player_objects`).
  - The tracer beyond its audit: one trace for every level (today each
    level re-traces), the split pass replaced. These change behaviour.
  - `--category onlydiag` has no run yet (kept by Philippe); `--override-settings`
    passes `--region` only.
- Dropped, with the reason:
  - Folding `widen::within` into `GraphArith::pieces_of` (the audit's
    T12): `within`'s facts from a select's `x op k` condition would change
    `range_of` wherever it is read (comparison folding, `bounded`, fork
    arity), so the kernels and owed checks would move by construction,
    not byte-identically; ~100 lines.
  - `RefDomain` sharing `Graph::eval`'s interval rules (T10): they agree
    case by case, and that agreement is a check - with one shared rule a
    wrong `Graph::compare` decision would pass both the lowering and the
    oracle `check ref` runs the kernels against; ~30 lines.
  - T8's classifier helper (deeper stack frames for ~15 lines); T21's
    typed lowering errors (`name_cells` reparses `Cell(n)` out of an error
    string; messages only).
  - Thread pools left hand-rolled (not `par::Pool`): the wave's pipeline (a
    scheduler: per-worker sinks, region jobs), the out and edge file
    writers, the backward's budgeted read round, the big-stack builder
    threads (a panic becomes a recorded refusal), the memory sampler,
    level -1's `observe` pool and condvar queue.

## Where the old plans went

The plans were merged on 2026-10-04 and again on 2026-10-16. Code comments
still cite the old names; the originals are in git history (`git show
5eff48f5:plans/NAME.md`).

| old doc | its live content now |
|---|---|
| storage-v2, storage-unify, rows-from-keys, keys-only, exact-keys, edges-target, emit-batch, arc-memory | this file ("Keys", "The forward", "The recorded graph", "The search"); measurements: plans/lessons.md |
| format-study, forward, fwd-bench, kernel-mix, kernel-opt-decisions | this file ("The codegen"); the benchmark: plans/devloop.md |
| footguns | CLAUDE.md (the rules); this file ("Per-tree records") |
| widen-table | plans/abstractions.md; its open to-do: "Open issues" above |
| jank-trace, jank-tools | "Open issues" above (`git show c4dd3241:plans/refactor.md`) |
| nodiag, hundred, gemskip | plans/results.md |
| float-solver | plans/float.md |
| waves, buckets, arcs, graph-model, memory, parallel, regions, held-buttons, fly-fruit, fall-floors, platforms-unknown, room30, ... (merged 2026-10-04) | this file, plans/abstractions.md, plans/lessons.md (`git show arc-sets:plans/NAME.md`) |

Also cited in code but never on develop (their branches' history only):
`fwd-perf`, `visited-packed`, `speed-bundles`, `edge-bundles`.
