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
   block in; every output row out through the storage's `UnitSink`
   (`storage::unit`), each row already carrying its KEY and its CELL (player
   position), already widened for the level. Branching, widening and keying happen inside. Two impls: the
   compiled `compiled::FrameEngine` and the reference `trace::refengine`
   (`RefEngine`: the tracer's interpreter over `RefDomain`, one lane and one
   fork path at a time; `trace::block` turns a block's lane into its
   state and each leaf back into a one-row block, boxes and all, by the one
   converter the tracer's output shapes come from, so its rows key like the
   kernels'). The loop never touches an interpreter state.
2. **The block** (`Rt2` in `celeste-engine`, `frame::Block` = an `Rt2`, with
   its key column where it was decoded from keys). Columnar; key and cell
   are exposed, the fields are opaque columns. A TRANSIENT: the forward
   holds states as keys (`storage::layer::Layer`), and a block exists only
   inside a wave's unit (decoded right before the kernels) and in readers.
3. **The kernel set** (`compiled::asm_kernel::Registry`). The tracer's whole
   output for one level: one assembled AVX-512 kernel per (shape, region),
   built at startup with gcc + dlopen. There is no checked-in kernel artifact.

To the outer loop a state is OPAQUE: it is `(shape, key, cell)` (the
visited set, the layers and frame files, the edges, the marks) - its cell
for regions, the pos graph and level -1. The frame step is the only thing
that opens a state; a unit decodes rows for it from keys.

## Code layout

```
crates/celeste-core      pico8_num, cart_data, collision_cache, ids, builtins   deps: -
crates/celeste-names     FROZEN name tables (FIELD_NAMES order = canonical      deps: -
                         field order; append only)
crates/celeste-engine    Rt2 block model, boundary/keys, lane primitives        deps: core, names
.  (celeste-rust)        everything else                                         deps: all
  src/frame.rs           Block, FrameStep, ForwardState (start, resume,
                         extend), MarkFilter, the frame files
  src/storage/           THE STORAGE (plans/storage-v2.md, plans/edges-target.md):
                         visited (region tables, every state's layer and
                         hull), unit (UnitSink: the kernels' emissions into
                         bundles and pushes), route (the region phase), wave
                         (the frame's pipeline, re-pushes and end), bundle
                         and edges (the edge files, transfers, EdgeStore),
                         out (the out-edges, by source), hulls (the hull
                         files), layer (the frontier, frame files and the
                         tree reader: states as keys), decode, bench (the
                         capture and `bench-storage`)
  src/search/hull.rs     a state's remainder HULL, its push, the order-free
                         merge (`HullAcc`)
  src/search/            checkpoint, arc_edges (the transfer decode), arcs
                         (remainder sets, their arena), arc_dp (THE SEARCH:
                         the backward push, optimum, reach, concrete search),
                         known, pos_graph, ui_export
  src/trace/             the AST tracer (Lua -> transpile::graph::Graph), the
                         constant-lattice walk (kernel.rs), widen.rs, verify.rs
                         (split pass), error.rs, level_minus_one.rs, the
                         reference engine (refengine, refdriver, refbridge),
                         block.rs (state <-> block, every domain)
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
semantics and wrong for counting anything. `--split-frame` loads
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
| `Add`/`Sub`/`Neg`/`Abs`/`Mul`/`Div`, of POINTS and of intervals | `not NoWrap(op)`: the exact result (an endpoint) left 16.16 on this lane, where the console wraps `+ - *` and negation and saturates `/` and `abs` (intervals: `b47b118`, `mul-nowrap`; points and `abs`: 2026-10-11, branch `overflow-errors`). Skipped where the static ranges bound the exact result (`Symbolic::bounded`, unwrapped and unsaturated). With one operand not a literal point (`graph::one_operand`: `x + k`, `k - x`, `x * k`, `x / k`, `-x`, `abs(x)`) the check is at most two compares of that operand (of an interval's two ends), none where every value fits (`x / 2`, `x * 0.5`: `Graph::fold` decides it) and constant false where none does (`x / 0`); two non-literals' `+`/`-` is the sign test (`vpternlogd` + compare); two non-literals' `*`, and `/` by one, have no check (the codegen refuses them; the cart has none) |
| a widening | containment of the slot it writes (`widen::SlotErrors`), only where stored |
| `Restrict(lo, hi)(x)` (a bounded input) | `Lo(x) < lo or Hi(x) > hi` on the raw input, charged to EVERY outcome |
| `Pin(c)(x)` (a pinned input) | `x` outside `c` on the raw input, an error of the whole frame (`error::pins_hold`) |
| the frame | what the platforms' input hook owes (a no-player speed `0 or v`) |

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
once no new region is left. A region (`RegionGrid`, `--region`, default
16 px) bounds the player's whole-pixel `x`/`y` (and `rem` to [-0.5, 0.5),
speed to [-S, S], S <= 7 because `move`'s loop is unrolled for `abs(amount)
<= 8`), all guarded per lane; the range analysis folds the far objects'
collision tests. A row outside every region has no kernel and stops the run.

**A bound is a node, not an environment** (`Op::Restrict`, 2026-10-09). A
bounded input (the region's square, speed and remainder; the no-player
phase's hulls) enters the body only as `Restrict(lo, hi)(cell)`: the value
unchanged, the range ATTACHED. Every range analysis - the tracer's
`range_of`/`pieces_of`, `Points`, `widen::bounds`, the lowering's interval
fold (`Graph::eval` narrows to the range) - reads the range off that node;
nothing seeds a cell. The node's own error is the check, on the RAW cell,
which nothing narrows, so no fold removes it. It is charged to every outcome
of the frame (`error::for_outcomes`), not by reachability: a comparison the
range decided is a constant that no longer reads the node. Until then the
bounds were both seeded (`d.ranges`, and the lowering's `fold_with(..,
ranges)`) and guarded, and the lowering's fold, seeded with the very range,
folded the guard to true: a lane with speed or remainder outside was
computed silently wrong (x/y were held by the dispatch alone). The trees
audited (`rewrite bounds-audit`, retired 2026-10-11 now that every kernel
checks per lane) never held such a row. The codegen emits
`Restrict` as the identity; the error is two compares a bound.
The platforms (`p`, 2026-10-09) were the last seeded ranges: a platform's
`x` (the path) and, with a player, its `spd.x` (its range over the worlds)
were `Symbolic::ranges` entries - `x`'s checked nowhere, `spd.x`'s by a
separate obligation. Both are restrictions now, and `Symbolic::ranges`, the
seeds of `pieces_of` and of `ival::seed_cells` are gone: no input cell has a
range, only a node does. `Points` pins a world's `x` on the raw cell beneath
the restriction.

**Input assumptions: one family, two kinds** (2026-10-10, branch
`input-assumptions`). Everything a kernel assumes about an input is a node
on the raw input, recorded in `Symbolic::assumptions` and checked per lane:
- `Restrict(lo, hi)(x)`, the RANGE: the value PASSES THROUGH, its range
  attached; downstream reads the node. Its own error is charged to every
  outcome (`error::for_outcomes`).
- `Pin(c)(x)`, the PIN: the input is DISCONNECTED; every use reads the
  constant `c` (`Pinned`: a point number, a literal interval, a boolean),
  so the node has no data output and `x` feeds only its check, `x` inside
  `c`, an error of the whole frame (`error::pins_hold`). `trace_frame`
  asserts that no outcome's values, guards, transfers or derived errors
  reach a pinned input. Pins are (1) a shape's lattice constants
  (`iface::symbolize` makes the slot read the constant; the cell stays an
  input for the check), (2) inputs READ AS their whole literal range so that
  lanes merge across them: a platform's `rem.x`, the fly fruit's `spd.y`/
  `rem.y` (the table's `Input::Literal`; the cells are typed interval
  inputs). A stored row holds the literal itself, so a literal pin never
  fires on the search's own rows; it makes a row from elsewhere (a start
  state, a test) decline instead of being computed as something it is not.
Before, the lattice pins were `iface::pin_guard` and the literals owed
obligations: the same checks, built the same way (kernels byte-identical,
metrics included). Charging a pin per outcome as a restriction cost up to
+0.3% dynamic instructions, so the two kinds keep their charging. The one
assumption left outside the family: the no-player phase's `spd.x`, read as
its one moving speed `v` and owed `0 or v` per lane (a two-point set; a pin
to the hull would admit speeds in between). A lane outside is the
kernel's own error (a fatal coverage gap).

**The map folds by range** (`spikes_at`, 2026-10-10, branch `kernel-opt`).
`tile_flag_at` is an intrinsic whose interval reading (`Graph::tile_flag_over`)
the lowering's fold has always used; `spikes_at` stays Lua, a 2x2 unrolled
tile scan whose four tests are `Eq(k, Mget(x, y))` for the spike tiles k =
17, 27, 43, 59. The fold's evaluator (`Graph::eval_fold_in`, used by
`transpile::ival` only) reads `Mget` over interval coordinates as the SET of
tiles the rectangle `flr(lo) ..= flr(hi)` holds (`TileSet`; outside the map,
0), its value the set's hull, and `Eq(k, Mget)` against a literal as
membership: false where no tile there is k, true where every one is. In a
region whose reachable rectangles hold no spike tile the death test folds
to false and its whole cone (the `%8`/`/8` call-outs, the four guards, the
`mget`s) goes. Why it is EXACT, not a widening: (1) the map is constant, so
the set is a fact about the cart; (2) the coordinates' hull covers every
lane's coordinate: the evaluator's arithmetic is checked interval arithmetic
over the same graph the kernel runs (a wrap is TOP, an undecided `Sel` the
join of its arms), and every node is evaluated on every lane whatever the
path, so the hull holds off-path lanes too; (3) the player's position, speed
and remainder enter only through `Restrict`, which narrows the hull, and a
lane outside a restriction is in error by the restriction's OWN error,
checked on the raw cell and charged to every outcome, so it emits nothing
whatever the folded node says; (4) a lane's `mget` coordinate is an
integer (the kernels' `zn_mget` raises on a fraction), and every integer in
the hull is in the rectangle. So on every lane that emits, the folded
constant is what the per-lane evaluation computes. Where the evaluator
cannot bound a coordinate (TOP), the rectangle is the whole map, which holds
spikes, and the test stays per lane. The tracer (`verify::Points`) and level
-1 keep the old reading (no set), so neither the trace nor the table moves.
`ival::a_tile_test_over_a_restricted_rectangle_folds_exactly_by_the_set_of_tiles`
checks fold against per-lane evaluation in every room.

**Keys.** A state's key is EXACT (2026-10-10, plans/exact-keys.md): per
(shape, value cell) a dictionary of the codes the tree has stored (a
number and its point interval are one code, `592c72f`), each field's index
in append-only bit runs of a 127-bit key that never moves as the
dictionaries grow. The append step looks each emitted row's key fields up
in the dictionaries (read-only during a wave); a code they lack makes the
state new and its key PROVISIONAL (its content interned), made final at
the frame's end, which appends the frame's new codes sorted - so keys stay
canonical. Checked per row against `Rt2::boundary_canonicalize` keyed in
the same dictionaries by `--key-check`. FIELD_NAMES' order
feeds the shape hash (a shape's name; its signature is stored and compared)
and the cell numbering: a reordering is a different search. The key holds
NO POSITION (`29d8364`): the position object's (player, else
`player_spawn`) `x`/`y` contribute only their part past the low end's whole
pixel (`exact::Code::pos`), which the cell holds; the room is in the key, so
a state is still `(shape, key, cell)`, and states sharing everything but the
position share a key - the storage's entries (below). The concrete search
compares whole states (`exact::exact_rows`), never keys.

**Where it still diverges / is open.** The re-trace per level (above). The
platform tension: storing a platform's `x` as its whole path is what lets
frames merge, and the overlap predicates only fold at ~4.5 px granularity -
exactly where no cross-frame merge survives; storing zones would cost ~Z
tuples (one phase drives all ten platforms), the ~18 px middle unmeasured.
**Overflow.** Until 2026-10-03 the assembled interval `+`, `-` and
negation WRAPPED an overflowing endpoint silently (plain `vpaddd`/`vpsubd`;
the Rust primitives panicked, and the per-op tests bounded their inputs).
Fixed on `arc-sets` by `549ecf5` (overflow -> the whole range; with the
near-floor changes it exposed) and then `b47b118` (Philippe's design, which
replaced the whole-range rule): an overflowing interval Add/Sub/Neg is a
lane's OWN ERROR (`Op::NoWrap`), so the lane declines loudly; and a countdown
that spans everything (floor `delay`, balloon `timer`, at `n` the spring's
`delay`/`hide_in`/`hide_for`) is stored and read as the unknown number
(`AV::UNum`, the table's `Input::Unknown`), never as the interval [MIN,
MAX]. (`72fdea7` on branch `asm-interval-wrap` was a rejected first version.)
Interval `Mul`/`Div` by a positive literal got the same own error on
2026-10-10 (branch `mul-nowrap`): the operation is monotone, so the check is
the operand's endpoints against the range whose image fits (bisected on
`checked_mul` / `checked_div`: `Pico8Num::mul_fits`). The cart's factors
above 1 multiply bounded values (`sin(..)*2`) and its divisors are at least
1, so no kernel of rooms (1,0), (6,2) 100% or (4,2) emits one (their trees
and dynamic instruction counts are unchanged). The static range of
a product is now computed unwrapped (`pieces_of`: past 16.16 it is unknown,
not PICO-8's wrapped endpoints). Which results ran before the fix:
plans/results.md.
**THE RULE: match PICO-8 exactly, or raise** (Philippe, 2026-10-11). For
overflow, division by zero and every case like them, the interpreter, the
kernels and the reference engine either compute exactly what PICO-8
computes or RAISE (the kernels: decline the lane, a fatal coverage gap).
Raising is always acceptable as long as real runs never hit it; computing
something else is never acceptable. So a wrap or a saturation is an error,
and so is `x / 0` (PICO-8 saturates it): no division in the traced cart
(`lua/celeste-minimal.lua`, the builtins) or the original
(`celeste_ocaml/celeste.lua`) has a divisor that can be 0 - every one is a
nonzero literal (8, 30, 40; the original's draw code 1.5, 3, 4, 5, 32, 60,
64), and `%` by 0 is PICO-8's exact 0. The console's own values where the
interpreter raises stay pinned by the `lua/probe` `*_overflow` corpora
(`trace::probe::OVERFLOW`: every case raises; `Pico8Num`'s explicit
`wrapping_*`/`saturating_*` reproduce their golden PICO-8 output).

**Every overflow is an error** (Philippe's decision, 2026-10-11, branch
`overflow-errors`): the game never leaves the 16.16 range, so nothing
wraps or saturates silently, for points as for intervals. `Pico8Num` has
no `+ - * /` operators: `checked_*` (the exact result or `None`) and the
console's `wrapping_*`/`saturating_*`, spelled out, for the kernels' lanes
and the tests. (1) THE KERNELS: a scalar `+ - * /`, negation or `abs` owes
`NoWrap` as an interval one does (the table above; `kernel::zn_*_wraps`
the reference masks), so an overflowing lane declines - a fatal coverage
gap. (2) THE TRACER'S CONSTANT FOLDING: the cart's arithmetic on constants
goes through `Concrete` (checked: an error naming the operation, its
operands and the source text, at trace time); `Graph::fold` of literal
points panics with the operands (it has no error to return; what the cart
folds never reaches it); the evaluator (`Graph::eval`, the lowering's
fold, level -1, `Points`) is checked throughout (an overflow is an error,
TOP where lenient), and a literal past the range (`32768`), a `for`
counter past it, are errors. (3) ELSEWHERE: the fall floors' collide
window and the fruit's band (checked), the reference's distance tests
(`i64`), and the fork grid's arithmetic in `zi_span_ok`/`zi_fork_flr`,
which wraps only for a lane in its operand's top `ways - 1` floors, where
`SplitOk(ways)` is false - the fork's own error, charged wherever a fork's
operand is an interval. The transfer roots are error roots too (the
remainder before the widening stores no field). The dead timers'
`minutes + 1` is checked at the stored constant
(`widen::tests::dead_fields_are_unread`). Cost, measured (t1 metrics,
2026-10-11): kernel dynamic instructions +1.9% (room (1,0) f55), +3.0%
((6,2) 100% f50), +2.6% ((4,2) `n` f60); outputs identical, no lane
declines. The checks the ranges do not fold: `spd +- dash_accel` in
`appr` (the dash's acceleration is no restricted input: 1.6-2.7%), and
`dash_effect_time - 1`, `grace - 1`, `dash_time - 1` (0.2-1.2%; the first
is stored `max(v, 0)`, the others decrement under `> 0`, which no range
analysis reads); interval `+ k` as compares saved 0.4% in (4,2).

## The hull forward (2026-10-13, branch `hull-forward`)

The forward used to widen every state's sub-pixel remainder to the whole
circle (the table's "player remainder"): a state reached at all was
expanded with every remainder, and every successor any remainder allows
was kept. It now keeps per state a REMAINDER HULL - the remainders it has
been reached with so far, over-approximated per axis by one ARC of the
circle (`search::hull::Hull`, 8 B: the box of two arcs) - and pushes it
along the recorded edges through their exact transfers.

- **The start** is the start's exact remainder (a point).
- **A push** along an edge (`Hull::push`): per axis the arc's points inside
  the transfer's guard, moved by its action (a rotation, or the collision's
  constant); nothing when either axis misses (a box's image is empty with
  either side empty). A target ACCUMULATES its pushes in a frame
  order-free (`HullAcc`): per axis two CHARTS - the circle cut at 0 and at
  1/2 - each a linear `[lo, hi]` merged by min and max (commutative,
  associative: region jobs run in any order); the arc is the shorter
  chart's. That is the exact hull of the union whenever the union fits in
  half the circle, and a superset of it otherwise (always one of the
  union).
- **The frontier** of frame t is every state whose hull GREW at t: the NEW
  ones (first reached at t: frame t's layer, its frame file) and the GROWN
  ones (reached before; `wave::Frontier::old`, per layer). The new ones run
  the kernels once - the only time a state is ever expanded - and their
  edges are recorded at t + 1 as before; the pushes go with the emissions
  (`UnitSink::pushes`). A grown one runs no kernel: its RE-PUSH
  (`wave::run_repush`) reads its out-edges (the OUT STORE, below) and pushes
  its grown hull along each edge whose image grows past the image of the
  hull it last pushed (`Hull::push_grown`: an axis it rotates meets more of
  the guard, or the box was empty and is not) - an image dropped then (by
  level -1 or the ladder's filter) is dropped at every later frame too, so
  the skip loses nothing.
- **Level -1 acts at every push**, emitted or re-pushed: a push into a
  target at frame t + 1 is dropped where `t + 1 + d(target) > H`.
- **The frame's end** (`route::seal`): per touched state, its new hull from
  its pushes and its hull so far; a state the visited set lacks is NEW
  (layer t + 1), one whose hull grew is GROWN (the next frontier), the rest
  nothing. A full hull is not stored (`visited::RegionInfos`: 10 B a state
  otherwise) and grows no more. The objects ladder's filter is applied to
  the new and grown states.

**Sound** (every concrete winner stays): by induction over frames, a
concrete path from the start that is at state p with remainder r at
frame t finds p in the tree with layer <= t and r in its hull. p's
successor q under the path's input is a recorded edge of p (p was
expanded at its layer; the successors of a state do not depend on the
frame), and r is in the edge's guard with the action taking it to q's
remainder (the transfers are exact: `arc-check`). r entered p's hull at
some frame t' <= t; at t' + 1 that hull was pushed along the edge (or its
image had been pushed earlier, unchanged), so q holds the image, with
layer <= t' + 1 <= t + 1. Level -1 drops only targets that cannot exit by
H from that frame or any later one (d is a lower bound and the frame only
grows), and the mark filter only nodes the coarser level proves losing;
the merge only adds. An edge level -1 dropped at its source's EMISSION
(frame L + 1, the earliest that source can move) is useless at every frame
the source can be at. The forward with every hull widened to the whole
circle is exactly the old one (`hull::WIDEN`, test-only: it reproduces the
old gate, `gates/ckhash_room10_f000-044_widened.txt`), and the hull
forward holds a SUBSET of its states, each in a layer no earlier, and
every state the exact-remainder reach reaches with its remainders
(`the_hull_forward_lies_between_the_exact_reach_and_the_widened_forward`).

**What refutes it**: the hull is a widening (a box of arcs holds
remainders no path brings) - but it only decides which states the tree
holds, how early, and which are expanded. The arcs are EXACT in the
remainder over the recorded edges (the backward), and the concrete search
prunes by their winning sets alone, so a state or an edge the hull admits
and no path reaches is never a win. Nothing outside the forward reads a
hull.

**What changed for the readers.** A frame's states are those first
reached there (as before: a layer is the frame a state was first reached
at), but an edge recorded at frame f may enter a state first reached
LATER (its push was empty or dropped at f) or never. The backward
therefore reads a changed target's in-edges from its region's first frame
with edges, with no per-target layer bound (`arc_dp::Backward::first`):
the bound it had ("an edge recorded at f enters a layer <= f") is false
here - room (4,2)'s known route was pruned by it at step 39 (`NOT IN W's
SUPPORT`). `arc-check` keys an edge's target by its key, not by its row
(it may have no frame). A tree is NEVER RAISED (`grow_tree` refuses one
filtered by level -1 below the horizon): a push level -1 dropped leaves a
hull smaller and a state later or unreached, so the frames depend on the
horizon throughout; the raise, its frame and edge files and the drop notes
are gone.

**Measured** (BENCHMARK_DATA.md, 2026-10-13; release, 16 threads, the
full `search` back to back with develop `26de6cd` on a machine another
search loaded - wall clocks moved up to 2x between runs, instructions did
not): the same optimum and witness in each, and

| room | instructions | forward states expanded (re-pushed) | backward | tree | peak RSS |
|---|---|---|---|---|---|
| (4,2) 2100m `r0sxhn`, L1 71 | 0.23 T against 0.40 | 0.6M (0.9M) against 6.6M | 0.07 s against 0.73 | 20 MB against 29 | 0.87 GB against 0.91 |
| (6,2) 100% `r0sxhf,r0sxh`, L1 94 | 2.91 T against 9.75 | 30.6M (44.9M) against 315.1M | 1.0 s against 2.8 | 0.84 GB against 1.85 | 3.4 GB against 4.3 |
| (1,0) 200m `r0sx`, ceiling 99 | 5.86 T against 6.25 | 65.8M (94.5M) against 205.8M | 0.16 s against 1.88 | 1.39 GB against 0.60 | 4.3 GB against 2.3 |

Room (1,0) is the worst case: its hulls grow to the whole circle (72% of
its states full in x, 60% in y), so its frontier holds as many states as
the old forward's and the re-pushes add their edges; its wall clock is
develop's within the noise (54 s median against 52 over four pairs). The
tree is larger by its out store (by source, for the re-pushes) and the
hull files; the peak by the visited set's hulls (10 B a stored state)
and the out store's mapped pages.

## The forward frame and the storage (keys only, 2026-10-12)

One storage system (plans/storage-v2.md built the visited set;
plans/edges-target.md the ID-free edges and the routed forward;
plans/keys-only.md the rest: no state ids, no rows held):

- **A state is `(shape, key, cell)`** and is named so everywhere
  (`storage::Sk`: region, key, cell in region; region = shape index x
  slots + slot). No state has a number: not in memory, not on disk.
- **The visited set** (`storage::visited`): per (shape, storage REGION) a
  table of ENTRIES - a position-free key and a mask over the region's
  cells. A storage region is 8 cells square (`storage::GEOMETRY`; 16 was
  the other measured choice), aligned as the kernels' regions
  (`div_euclid`) and nested in one (the kernels' px a multiple of it, else
  refused); `NO_CELL` is one more region per shape. A pure dedup
  structure: nothing outside names an entry, so entries keep their places.
- **A LAYER** (`storage::layer::Layer`): per region (ascending) its
  entries - key (ascending) and cell mask, 24 B an entry - and the
  skeletons its shapes' rows decode with. The forward's FRONTIER is a layer
  (the last frame's new states, with their hulls) and its grown states
  (by layer, `wave::OldLayer`: per entry its region, key and cells, per
  state its hull and the hull it last pushed - 16 B a state and 32 an
  entry), and
  the wave builds the next: no row layer exists in memory.
- **The wave** (`storage::wave::run_wave`), one pipeline a frame: the
  frontier cut into UNITS of whole entries of one shape (a 256th of the
  frame, 1024-4096 states plus at most 63) and the units into SLICES (~256k
  states). A unit is DECODED right before the kernels (`decode::decode_rows`,
  a field's code once per entry); `frame::not_expanded` is decided on it -
  for the forward's own frontier and a resume's alike - and only its
  expanded lanes run. Per emission the unit's sink
  (`storage::unit::UnitSink`) drops level -1's rows, keys the row and ORs it
  into its BUNDLE (source key, transfer, offset, target key -> target
  cells) in a unit-local table, keyed by the target's ROUTE (shape, region
  slot). Nothing is looked up in the visited set and no row is copied. At
  its end the unit's bundles are per route a CHUNK, with per target cell
  the merged PUSH of the bundles' sources' hulls (a bundle's target cells
  map to its source cells by one shift of the region grid,
  `bundle::shift_into`), and its out-edges one OUT BLOCK, appended to the
  frame's SPOOL (`storage::spool`: an unlinked scratch file the out file's
  writer copies from at the end). A transfer the tree's table holds is
  named by its global id (the table is read only during the wave, shared
  by the workers); only a pair new this frame gets a worker-local id. The
  RE-PUSH units (the grown states, by layer, ~16k states a unit) make
  chunks of pushes only. When a slice's units are done its new transfers
  get their global ids (the gather numbers them in a table of their own,
  appended at the frame's end) and its chunks become REGION JOBS (one per
  (route, slice)), which the workers take before the next slice's units:
  the region phase overlaps the kernels.
- **The region phase** (`storage::route::run_route`): per chunk target its
  pushes merged into the route's TOUCHED states (`route::Touched`: per key
  one probe, its cells' hulls in a run), and the job's bundles encoded into
  one zstd SEGMENT of the frame's edge file (spooled too). A route's
  touched states are written only by its jobs, one at a time. The wave
  takes the frontier and drops it once its units are done.
- **The frame's end**: provisional keys made final, new shapes numbered,
  each route SEALED (`route::seal`: its touched keys sorted, each against
  the visited set in parts - new, grown, or nothing; "The hull forward";
  a route put together by the worker that ends its last part, so it is
  freed then): the NEXT LAYER and the grown states; the ladder's filter on
  both; both merged into the visited set with their layers and hulls
  (`VisitedSet::absorb`: each run of a region's packed blocks makes its
  own changes); the
  new states DECODED in chunks on every thread for their wins (and, under
  `--key-check`, each row's key and cell); the edge file and the OUT FILE
  written. The forward then writes the FRAME FILE (`frames/f{F}.bin`: per
  region a zstd block of the new entries' keys, masks and win masks; a
  header with the shapes numbered, the key space's additions, the shapes'
  skeletons and those first emitted there) and the HULL FILE
  (`hulls/f{F}.bin`, `storage::hulls`: the new states' hulls in the frame
  file's order, the grown states with their layers and hulls), READS BOTH
  BACK and compares them with the forward's (`layer::check_frame_file`,
  `hulls::check`, FATAL on a difference); the pos graph; then `done.txt`.
  The DECODE GUARD: a shape's first emitted row (each worker, each wave)
  must be what its key and cell decode to, and every emission's skeleton
  the shape's (FATAL otherwise); `--key-check` compares every row.
- **Resume** rebuilds the visited set from the frame files (each frame's
  layer merged in, regions in parallel; a state stored twice is an error),
  replays the hull files in order (a state's layer and hull are its last
  file's), registers every skeleton the files hold (a re-push may reach a
  state of a shape the frame emits nothing of), and takes the last frame's
  hull file as the frontier, each grown state with the hull it last pushed.
  Readers (`layer::Tree`) give a frame's layer, a state's layer, rows
  decoded on demand.

The result is a function of the frame, not of scheduling or the thread
count: room (4,2) `r0sxhn` under level -1 (71,5) f0-f60 at 16 and 5
threads and over a resume at f40 (at 7 threads), every frame, hull, edge
and out file, `xfer.bin` and `done.txt` are byte-identical (244 files,
2026-10-13); the t1 frames' lines and metrics are the same at 3, 16 and 32
threads. `rewrite bench-storage` replays one captured frame
(`--emit-capture`) through this code without the kernels (its re-pushes run
for real, under the tree's level -1), and reproduces the real tree's
`ckhash` lines of that frame.

## The recorded graph (edges target, 2026-10-11)

The forward records every edge once, with what the frame did to the
remainder (its transfer); the backward reads the edge files and re-runs no
kernel. An edge recorded at frame f leaves a state of layer f - 1.

- **The edge file** (`storage::edges`, v6; plans/edges-target.md): per
  frame `edges/f{frame}.bin`: a header
  - the segments by TARGET region (one a slice that reached it), and per
  target region the source regions its edges leave - and the zstd blocks.
  A block holds BUNDLES: `(source shape and key, transfer, offset, target
  key) -> mask` over the target region's 64 cells, its target keys and
  sources as sorted dictionaries, the records a trie in the order mask,
  offset, transfer, source, target. Nothing names a state by a number.
  The transfers: ONE table for the tree, `edges/xfer.bin`,
  content-canonical (each slice appends its new pairs in value order).
  Room (6,2) 100% f57: 257.7M edges in 50.9M bundles, 29 MB (0.113 B an
  edge; storage v2's unit blocks 3.15 B).
- **Every expanded state's edges, once** (the hull forward): a state is
  expanded at its layer L and all its kernel successors are recorded at
  L + 1, whether the frame's push into them was empty, dropped or not - so
  a target may be first reached later, or never (it has no frame).
- **The out store** (`storage::out`, `edges/out/f{frame}.bin`): the same
  edges by SOURCE, for the re-pushes - per kernel unit sub-blocks of whole
  entries (~2048 bundles), each its routes, transfer pairs and target keys
  as dictionaries and per source entry its bundles, zstd; a header indexes
  the blocks by their first and last entry, so a re-push unit (the grown
  states of one layer, sorted) decompresses only the blocks holding its
  states. A unit whose targets had provisional keys has its block fixed up
  at the frame's end. ~0.25 GB a frame on room (1,0) at f80 (14.6x
  compressed).
- **`EdgeStore`**: per (frame, region) its segments (`segments`), every
  block (`all_segments`, `par_blocks`), the target regions a source region
  reaches (`targets_of`), and for the diagnostics a state's in-edges
  (`preds_at`) and a frame's edges sorted (`edges_at`).
- **The backward reads the edge files, it loads no graph** (2026-10-11,
  plans/arc-memory.md): per step the in-edges of the nodes whose winning set
  changed, read per (target region, frame): the region's blocks decoded and
  their records matched against the changed targets' keys and cells; `reach`
  loads out-edges as states are reached (`arc_dp::ReachEdges`, through the
  header's source-region index).
- **Checks.** `rewrite arc-check` (every edge has a transfer; sampled
  transfers probed with the reference engine at the tree's level inside and
  outside their guards); `ckhash --edges` (each frame's edges by content).

## The search (2026-10-05, branch `arc-only`)

`rewrite search --to H | --ceiling H [--level SPEC[,SPEC...]]`, per level of
the list (coarsest first; one level is the usual case):

1. **[Level -1]** (`--level-minus-one H,S`, plans/level-minus-one.md):
   cells that provably cannot exit by H are dropped at the flush.
2. **The forward** at the level (default `r0sx`; `r0sxhn` for an object
   room), to H, into `<dir>/level{i:02}` (`ForwardState`: resumed from its
   checkpoints, or used as it is when it already reaches H). Won rows are
   checkpointed (backward seeds) and not expanded. **A finer level is
   filtered** (`frame::MarkFilter`): a row at frame t is kept only if its
   projection onto the previous level is ARC-MARKED there with a deadline
   >= t. Sound because the remainder is exact at both levels: a fine state
   that wins from t with remainder r projects to a coarse node that wins
   from t with r. This is the OBJECTS LADDER (below), the only ladder left.
   ARC-MARKED (2026-10-08, `arc_dp::reach`) is REACHED inside W: the
   rotation graph run forward from the start's remainder, `R_{t+1}(m) =
   U push_e(R_t(p)) ∩ W_{t+1}(m)` (`Region::push`: the image inside the
   guards), the deadline the last frame R is non-empty. A concrete winner's
   remainder is in W_t of its node and is the image of its previous one
   along the recorded edge, so it is in R_t by induction. Before, a node
   was marked when its W was non-empty at some frame: room (3,2) nodiag at
   h145 marked 38.0M nodes that way and 486k by reach, and the filtered
   `r0sxh` forward took 16.7 s instead of 681 s, with the same answer. A set
   past 32 segments is replaced by W_t (a superset: the filter only
   loosens). The UI's `arc.marks.bin` are the same reached nodes.
3. **The arc phase** (`arc_dp::solve`; plans/arc-memory.md): ONE BACKWARD
   PUSH over the recorded in-edges (`arc_dp::backward`). `W_t(n)` only grows
   as `t` falls (more time never hurts, and a state's edges do not depend on
   the frame), so `W_t(p) = W_{t+1}(p) ∪ U pull_e(W_{t+1}(m))` over the
   edges `p -> m` whose target CHANGED at t + 1: frame by frame from the
   wins, the changed nodes' in-edges are read - from their region's first
   frame with edges on, whatever the target's layer (a hull tree's edge may
   enter a state first reached after it: "The hull forward") - from their target regions'
   blocks (each block's sorted targets met by galloping through the
   region's sorted changed ones: a block costs its own size, not the
   group's), in bounded ROUNDS (~20M in-edges, 0.8 GB, at most), and pulled into their
   sources in parallel state ranges. No graph is held and nothing marks nodes
   first: a node no winning set reaches is never touched, and W's SUPPORT
   (the nodes with a set at some frame) replaces the remainder-free marks
   in the gate. W is kept as varints - per node its state's delta, layer and
   spans, per span its length and set, ~4 B a node and ~3 a span - in a
   scratch file beside the tree, mapped back (`<level>/arc-scratch`,
   unlinked once mapped), over an arena of the distinct sets
   (`RegionArena`: records of u16 words in 64 MB chunks, a full chunk
   written to the scratch dir and mapped back, so W's sets are page
   cache, not anonymous memory; a step's own sets an in-memory arena,
   compacted between rounds past 1 GB). `arc_dp::optimum` reads
   the optimum off the start's sets. That optimum is exact in the remainder
   and over-approximates the level's other widenings (and `rnd`): a LOWER
   BOUND, and no win REFUTES H. A node IS its state (`Sk`: region, key,
   cell, as the edge files name it), so the gate's fingerprints, the marks
   files and the concrete search's node keys need no metadata. A big
   room at h155 (230M nodes, 430M spans, 6.0G in-edges read): the arc phase
   in ~15 GB anonymous under a 30 GB cap, where the remainder-free BFS and
   its 12 B-an-edge graph were OOM-killed at ~104 GB.
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

**The horizon and level -1** (`frame::grow_tree`). A tree records the
filter its frames were built under, `<level>/level_minus_one.txt`
(`TreeFilter`: `off`, or `h H speed S table FP`); a forward extends a tree
under ITS filter, never the environment's, so one tree is filtered alike at
every frame, and `--to H` under a tree filtered at H2 >= H uses it as it
is. A tree filtered BELOW the run's horizon is refused: the hull forward's
frames depend on the horizon through every push level -1 dropped, so it
cannot be raised exactly (the raise of 2026-10-08 - re-expanding the noted
sources of the drops frame by frame - did that for the remainder-free
forward; it is gone with its drop notes and raised files). Refused, loudly:
another S or a changed table, a tree without a record. A FINER level's tree
(the objects ladder) is filtered by the coarser marks for one horizon; it is
kept only for the same marks (with deadlines), horizon and level -1
(`filtered_for.txt`), else built again - it is the cheap one.
Frame files hold KEYS only (2026-10-12, plans/keys-only.md; format 15,
after format 14's ids and skeletons, plans/rows-from-keys.md): a row is a
function of its shape's skeleton, its exact key and its cell, so a frame
file is per region its new entries' keys and cell masks (and win masks)
with the shapes' skeletons, and a row is DECODED when needed
(`storage::decode`). The forward copies no row and holds no row layer: a
unit decodes its rows from the frontier's keys, the frame's end decodes
the new states once for their wins. The DECODE GUARD checks a shape's
first emitted row each worker and wave against its decode
(`decode::Skeletons`, FATAL otherwise); `--key-check` checks every row; every
frame file is read back and compared with the forward's states. Frames
take ~0.08 B a state on a big room (fm85: 307 MB for 3.68B states;
format 14 ~9 B).
The level is a value (2026-10-10): the engines carry it (`FrameEngine`,
`RefEngine::at_level`, `FrameStep::level`), and a tree records it in its
settings record (`<level>/settings.txt`, `settings::Settings`, written when a
forward starts it, with the room, category and region settings;
`Settings::check` refuses another `--level` or any other setting on resume,
reuse, the arc phase and the diagnostics; a tree without the record
is refused). One kernel set is resident; a call at another level drops it and builds
that level's. Fused graphs are dropped
after assembly unless `--kernel-explain`.

**The concrete search runs at the LAST level only** (2026-10-08). A coarser
level's try at its bound almost never wins (its bound is loose wherever its
objects are) and the finer level decides the optimum anyway: gemskip-nodiag
2800m at h192 spent 379 s of a 450 s search in level 0's fruitless try at
f186 (optimum 191, found by level 1 in 0.4 s).

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
(`frame::orb_required`, `2dc116d`); a synthetic
`--win-at` for experiments. Rooms past (5,2) start with
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

1. **Forward** (remainder-free until 2026-10-13; now over remainder
   hulls, "The hull forward"). Each emitted row
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

## The room's start: as real play enters it (2026-10-08)

The search starts from `_init()` on the minimal cart: `load_room(x, y)` for
the start room (`game_runner::apply_start_room`). Real play enters a room
differently. The leaving player's update calls `next_room()` INSIDE
`_update`'s `foreach`, and PICO-8's `all` resumes at the same index in the
new `objects`. So the new room's objects from the leaving player's index J
on get one update (move, then update) on the loading frame, and then the
frame draws.

`--loading-jank J` (`trace::cart::apply_loading_frame`) appends that
loop and a `_draw()` to the start room's load. J counts the ORIGINAL cart's
objects; it is translated past the `message` and `flag` that the minimal cart
lacks (`minimal_index`). Unset, the start is the IL load, and the binary says
so on stderr (`[start]`). The patch is in the Lua sources, so everything that
reads `cart::sources` sees the same start: the kernels' retrace, the
reference engine, level -1 (whose cache key includes the env and the Lua) and
the platform worlds.

Room (1,0) with J >= 2 is the IL start: it has only the spawn, and the gates
are unchanged. J = 1 (a 100% run) moves the spawn one frame earlier, and the
arc gate's optimum becomes 32.

J is HISTORY: it depends on how the previous room was played (plans/results.md
"Real play"). The sound treatment is the set of feasible J's, grouped by the
start state each gives:
- `rewrite starts` (`src/starts.rs`) computes the feasible J's and the
  classes, comparing the reference engine's start states exactly (until
  2026-10-11 `pico8_diff/chain.py`, one `concrete_run --dump-start` text
  per J; identical classes for levels 2-31);
- `tools/category_runner.py` `expand` runs one search per class;
- the boot chain's class carries the witness and the upload.

The ground truth for both the start and the verdict is the boot chain on a
real PICO-8 (`chain.py --boot`). `chain.py --check-start CONCRETE_RUN`
compares the search's start against it, field by field.

## Gates

Every change to the loop, the kernels or the tracer must reproduce the
three pinned room (1,0) oracles (commands and comparison in CLAUDE.md):
`gates/ckhash_room10_f000-044.txt` (each frame's new states and its
frontier's hulls f0-f44, `ckhash --hulls`; the forward before hulls is
`..._widened.txt`, which a test holds the WIDEN forward to),
`gates/posgraph_room10_f044.txt` (pos-graph edges), and
`gates/arc_room10_win9-101_h35.txt` (the search at a synthetic win at h35:
W's support, W's fingerprint at every frame, the arc optimum 33 and the
concrete witness; it replaced the ladder's marks gate on 2026-10-05; its
first line was the remainder-free marks until 2026-10-11, plans/arc-memory.md). Plus the quick suite, `kernel lanes: missed 0`, and for the
recording `arc-check`. A kernels-against-interpreter check: `rewrite
ref-check` (row by row; the kernels over-approximate where the reference
splits an interval, so ckhash equality with the reference engine is no
test). After a change to the forward, the hull files or the out store: a
resume against an uninterrupted forward, byte for byte ("The forward
frame and the storage").

**A known solution against the pruning** (`search::known`, 2026-10-08).
Every pruning step of the search - level -1, the objects ladder's filter,
the winning sets the concrete search looks states up in, and under them the kernels' successors, the visited set and the split frame's
mid-frame steps - removes real winners silently if it is wrong: the result
is a wrong optimum or a false tie, never a fake improvement (witnesses are
replayed), so nothing else notices. Room (6,1) nodiag `r0sxhn,r0sxh` REFUTED
93, which TAS15 reaches (plans/nodiag.md, `5ccf712`). The check steps a known
route (a TAS, or an earlier witness) through the reference engine, under
the concrete search's seeds, and per level asserts at every search step s
that level -1 does not call its cell too late and the ladder's filter
admits it, and at every frame boundary that its projection
(`arc_dp::lookup_keys`, the concrete search's own) is a node of the arc
graph whose `W_s` holds its exact remainder, and, where the level filters
the next one, that the node is REACHED with a deadline >= s. Mid-frame
states are not looked up (they carry the split frame's cut widening, which
`Rt2::widen_to` does not apply); `W` at the next boundary is pulled through
them. One winning `rnd` lineage passing everywhere suffices. On a failure
it prints each winning lineage's first failing step - the check, the node
`(shape, key, cell)` and whether the tree holds it at all, `W_s` against
the remainder, the player - and the two frames after, then fails.
`rewrite search --prefer FILE` runs it at every level before the concrete
search whenever the file exits by the horizon (a file that does not is
reported and skipped); `rewrite check-known --checkpoint-dir D --level SPEC
--horizon H --inputs FILE` reruns the arc phases over finished trees and
checks alone. The gate's arc search runs it (`--prefer
gates/known_room10_win9-101.txt`). Cost (quick profile, loaded machine):
the route's concrete steps and one lookup per frame - room (1,0) gate 0.05 s;
(6,1) nodiag split `r0sxhn,r0sxh` synthetic h40: 0.2-0.4 s a level; (5,0)
nodiag `r0sxhn,r0sxh` to 93 (the TAS, bounds 91 and 93): 0.63 + 0.17 s of a
471 s search; `check-known` over those finished trees 25 s (the two arc
phases rerun). Caught, injected in a scratch build: the split-frame filter
fix reverted (room (6,1) nodiag, `--win-at 70,104 --to 40`: level 1 found
no win at all) - `LEAVES THE TREE at step 57 (mid-frame 29): THE LADDER'S
FILTER drops it`, its projection onto `r0sxhn` unmarked; and `reach`
skipping the `Const` transfers (room (1,0), `--level r0sx,r0sx`) - `NOT
REACHED` at frame 24, the frame the player spawns with `rem` set to 0.

## Deferred follow-ups

1. **One file per (shape, cell), dispatch key inside** (Philippe,
   2026-09-15): blocks per dispatch key with a key -> range index; count files
   and keys per frame on a real tree first (the per-shape layout exists because
   of the inode blow-up).
2. **The transfer capture costs the forward** ~25-45% CPU (room (1,0)
   f0-f44, `d7c373a`); the kernels compute five roots per axis per body
   where the transfer depends only on the fork configuration and `ox`.
3. **A batch-invariance test** (`a_lanes_key_does_not_depend_on_its_
   neighbours` went with the old sweep) and a widening-soundness check
   (`diag-project` checks one projection; `ref-check` the kernels).
4. **The UI** (`ui/`, TypeScript) still carries the ladder's concepts
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
