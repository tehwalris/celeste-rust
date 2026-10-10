# The widening table (branch `widen-table`, 2026-10-10)

Philippe: widening was written twice by hand (`trace::widen`, 941 lines,
and `Rt2::widen_to`, ~330), "assume something about an input" had about
five mechanisms, several always-on widenings were hidden (the strawberry's
bob, the timers, absent-as-zero, the dash effect, the balloon's phase), and
the level was a process-global Mutex passed as four loose bools. Target:
one declarative table interpreted twice, one input mechanism where
possible, every widening visible and justified, the level a value.

## Design

`celeste_engine::widening` (the engine crate, so both sides see it):

- `Level { held, fruit, floors_near, platforms }` (moved from
  `abstraction`, which re-exports it); `Level::has(Flag)`, `Level::entries()`.
- `TABLE: &[Entry]`, applied in order by both sides. An `Entry` has a
  `name`, a `Flag` (`Always`, `Held`, `Fruit`, `Near`, `Platforms`: a level
  is the set of entries its flags turn on), a `Target` (the instances of an
  object type, or globals), `Slot`s, a `Kind` (`Exact` / `Over`), an optional
  `Hook`, and `why` (the proof sketch for an exact entry, what refutes an
  over-approximation).
- A `Slot`: a field path below the target, what it STORES (`Stored`), how
  the frame READS it (`Input`), and whether it may be absent.
  - `Stored`: `Range{lo, hi, proof}`, `Band{around, radius, proof}` (around
    another field), `Phase{lo, hi}` (one period of a periodic read),
    `AtLeast(k)`, `UnknownNum`, `UnknownBool`, `Num(k)`/`Bool(b)` (dead
    fields), `FullPeriod(p)`, `SameAs(field)`, `AbsentAsZero`.
  - A containment is proved from the graph where it can be
    (`widen::within`), else checked per lane on the interval's ends; on
    blocks every containment is a per-lane assertion. (Branch `widen-table`
    kept the three constructions the old code used - per lane, static,
    literal arms - to stay byte-identical; this branch unifies them.)
  - `Input`: `Stored`, `BothWays` (a 2-way fork every lane takes),
    `Atom` (an undecided atom), `Unknown` (the unknown number), `Literal`
    (read as the stored range, owed per lane on the raw input), `Hook`.
- The block interpreter: `Rt2::widen_to(ids, level)` (names resolved once
  to ids, `celeste_names`), with `Rt2::widen_near_floors` for the hook.
- `render_markdown()`: the table as plans/abstractions.md lists it; the
  test `the_abstractions_doc_lists_the_table` keeps them equal
  (`CELESTE_REGENERATE=1` rewrites the doc's copy).

`trace::widen` is the graph interpreter: `widen` (the frame's end: each slot
written as stored, its containment owed as the widening's own error),
`read_inputs` (the frame's start, per `Input`), `materialize_absent_fields`,
and the two hooks. Derived from the table instead of hand lists:
`shapes::ival_paths`/`boundary_widened_paths` and `kernel::boundary_ival`
(`widen::interval_input`, `widen::stores_widened`), the near floors' interval
column (`widen::near_floor_states`), the countdown field names
(`widen::countdown_fields`, was `domain::COUNTDOWN_FIELDS`), the
absent-as-zero fields (`widening::absent_as_zero`, was
`widen::ABSENT_AS_ZERO`, read by `cart::check_absent_fields` and
`Interp::index_key`), the reference's fly-fruit pins (`refengine::
pin_fly_fruit_motion`). A third copy of two widenings in `trace::emit`
(the remainder's and the timers' cells as uniform key fields) is gone:
both are constant nodes in every body, which `konst_av` finds.

## The table as built

17 entries (the generated list with every slot: plans/abstractions.md, "The
table"):

| entry | level | kind | stores |
|---|---|---|---|
| held buttons | `h` | over | `p_jump`, `p_dash` unknown; read both ways |
| player remainder | every | over (the arcs) | `rem.x/y` [-0.5, 0.5), owed per lane |
| dash effect | every | EXACT | `max(dash_effect_time, 0)` |
| strawberry bob | every | over | `y` = `start` +- 2.5 (owed), `off` one period [0, 39] |
| timers | every | EXACT (dead) | `frames`, `seconds`, `minutes`, `deaths` := 0 |
| key sprite | every | EXACT (dead) | `spr` := 8, `flip.x` := false |
| fly fruit | `f` | over | `step`, `y` unknown; `spd.y`, `rem.y` ranges read as literals; `fly` an atom |
| fall floor countdown | `n` | over | `delay` unknown |
| balloon countdown | `n` | over | `timer` unknown |
| spring phase | `n` | over | `spr` [0, 19]; `delay`, `hide_in`, `hide_for` unknown |
| balloon phase | `n` | over | `spr` [0, 22]; `y` = `start` +- 2 |
| near fall floor | `n` | over, hook | `state` [0, 2], `collideable` unknown, except where the player overlaps |
| platform path | `p` | over, hook (input) | `x` the path [-16, 128], `last` the same |
| platform remainder | `p` | over, hook (input) | `rem.x` [-0.5, 0.5) |
| balloon offset | every | EXACT | a full-period phase as [0, 1) |
| absent fall floor delay | every | EXACT (modulo raises) | a missing `delay` as 0 |
| absent spring delay | every | EXACT (modulo raises) | as the fall floor's |

## What stayed hooks, and why

- **`Hook::NearFloor`** (`n`). Its value per lane depends on ANOTHER
  object's fields (the player overlapping the floor at the frame's end);
  the overlapped case stores the cart's invariant (owed) or, with `p`, the
  computed value; mid split frame `collideable` stays computed in the
  player's probe window; the input derives `collideable` from `state`. The
  table still says what it stores where the player is apart (its slots).
  Both sides implement it (`widen::widen_near_floors`/`read_near_floors`,
  `Rt2::widen_near_floors`), and `rewrite widen-check` checks they agree.
- **`Hook::PlatformInputs`** (`p`, input only). Which world platform an
  object is (its constant `y`, `dir`), `x` restricted to the path and
  recorded as that world's cell for `verify::Points`, `spd.x` bound by the
  worlds (with no player, the literal of its one speed): all per WORLD,
  which a slot cannot say. The stored values are plain entries (`Range` and
  `SameAs`).
- **`read_inputs` materializes absent fields first at a near level**, not at
  the others: at `n` an absent `delay` must be read as the unknown number
  (0 is inside it); at an exact level the absent field stays absent until
  the frame's end. Kept as it was (a change would move kernels).

Not in the table: `rnd` (an interval at every level, the cart's own draw),
the region key (dispatch), the input bounds (`Op::Restrict`, pins: input
assumptions, never a stored value), the seeded balloons' lookup view
(`arc_dp::lookup_view`, part of the `rnd` caveat).

## The level is a value

`abstraction::{set_level, current_level}` and their Mutex are gone. The
engines carry the level (`FrameEngine::new(settings, level)`,
`RefEngine::at_level`, `FrameStep::level`); the kernel registry, the wave,
the sink's reference-row key view, frame 0's keys and the capture's env
read it from the engine; `Symbolic` carries a `Level` (the four bools are
gone). A tree records its level (`<level dir>/level.txt`, written when a
forward starts it); `frame::check_tree_level` refuses another level in
`grow_tree` (reuse, resume, raise), `ForwardState::at` (resume,
bench-frame), `arc_dp::solve` and the diagnostics that take `--level-dir`
and `--level`. Checked by hand: a room (1,0) tree started at `r0sxh` is
refused at `r0sx` and extended at `r0sxh`.

Coordination: branch `footguns` is adding ONE tree settings record (split
frame, regions, room, category flags, win rect, the level). `level.txt` is
deliberately minimal; it should become that record's level field. (Done
on `footguns`: `level.txt` is gone, the level is a line of
`<level dir>/settings.txt`, and a tree without the record is refused.)

## Verification

- Byte-identical, no re-pin: `./check.sh t0` and `t1` (the three fixture
  frames' f/e lines and every exact metric, KEY_CHECK, backward, storage,
  ref-check, arc-check) pass at every commit (`f172fed` checked with the
  next); `t2` (the room (1,0) gates, the objects-ladder search, the platform
  room's forward and frame metrics) at the table, widen-check,
  level-as-value and last commits; `big` (the reference frame and its
  capture) at the table and level-as-value commits.
- The agreement test that replaces `rewrite follow` as the cross-check:
  `rewrite widen-check` (t1: r10 `r0sxh` f55, r62h `r0sxhf` f50, r42n
  `r0sxhn` f60, each with the rows the kernels emit from 64 stored rows;
  t2: r60s `r0sxhfp` steps 70 and 71). Every stored and emitted row is a
  fixed point of the block side's widening, compared as canonical bytes:
  431,881 + 1,689, 2,226,618 + 4,981, 495,272 + 1,521, 325,608 and 458,050
  rows, 0 moved. A deliberate fault (the block side's unknown number as the
  full interval) moves 495,271 of r42n's rows.
- `dead_fields_are_unread` (below), `the_table_resolves`,
  `the_abstractions_doc_lists_the_table`.
- Level -1 (its tracer runs the table too; the fixtures load cached
  tables, so t1/t2 do not rebuild one): `transpile --level-minus-one-table
  5` rebuilt with this branch gives the fixtures' recorded fingerprints,
  room (4,2) `af00ed208170568a`, room (6,2) 100% `067cab08a33c645f`.
- The `#[ignore]`d tests (t3: resume, the level -1 balloon table, the
  isolated floors' pin): 3/3 pass.

## The timers: an exact canonicalization, checked

They were a value replacement with no owe (SPEC annex D tr-7). They stay a
replacement - by a CONSTANT, so a dead field - classified EXACT and checked
mechanically: `trace::widen::tests::dead_fields_are_unread` traces every
frame of room (4,0)'s 100% route (the player runs and dashes, takes the
key, opens the chest; 97 frames until the chest's `rnd` forks the concrete
step) with exactly the table's dead slots (`Stored::Num`/`Bool`: the timers,
the key's `spr` and `flip.x`) as free input cells; after the BDD's boolean
simplification (a guard re-merged after a branch on `frames` is `c or not
c`) no other stored field, guard, error or raise reads them. With the
player's remainder declared dead instead it fails. No output moved.

## Decisions for Philippe

1. **DONE on branch `input-assumptions`** (Philippe: a pin disconnects its
   input, uses read the constant, the input feeds only `input == c`): pins
   were already that underneath; they are `Op::Pin` nodes now, beside
   `Restrict`, in one family (`Symbolic::assumptions`), byte-identical.
   The measurement that decided it: **Pins as `Restrict(c, c)` are not free**. Semantically equal
   (every frame's states and edges identical), but the kernels move:
   - numeric pins as `Restrict(v, v)` with the generic own error (`Lo < v or
     Hi > v`): dynamic kernel instructions +0.15% (r10), +0.51% (r62h),
     +1.25% (r42n); error terms +27-51%;
   - with a point restriction's own error written `not (x == v)`: +0.01%,
     -0.04%, +0.32% (the error is charged per outcome instead of once in the
     frame's `admissible`);
   - BOOLEAN pins have no `Restrict` form (it is numeric), so pins stay a
     mechanism unless `Restrict` gains a boolean twin.
   Options: keep pins (today); adopt the second variant with a re-pin (+0-0.3%)
   and keep boolean pins; or add a boolean restriction too.
2. **DONE on branch `input-assumptions`**, as PINS to the literal interval
   (`Pinned::Num(lo, hi)`), not as `Restrict`: the input is read as the
   literal (disconnected), so it is the pin kind; byte-identical, metrics
   included. The earlier measurement: **Owed input literals as `Restrict` assumptions**: the fly
   fruit's `spd.y`/`rem.y` and a platform's `rem.x` read as their literal
   range, their containment owed through `admissible`. Done as a
   `Restrict(lo, hi)` on the raw cell beside the literal: r62h states
   identical, dynamic instructions +0.06%. Same choice as 1.
3. **Absent-as-zero stays** (a table entry now, EXACT modulo executions
   that raise; `cart::check_absent_fields` the syntactic guard). The clean
   replacement, a nil-or-number field (a per-row presence tag), changes
   shapes and keys (a re-pin of everything) and needs kernel support for a
   nil-or-number column. Not attempted.
4. **DONE (merged into `input-assumptions`)**: **Three `Proof`s** (per lane, static, literal arms) mirror the three
   constructions the code had. Unified on `Static` (`widen::within`, else
   the interval's exact containment per lane), every frame's states and
   edges stay identical and the kernels SHRINK: dynamic instructions
   -1.83% (r10), -5.57% (r62h), -3.21% (r42n); static -2.2%, -3.4%, -4.7%.
   Platforms (r60s step 70) -2.23%, the reference frame (r62h57) -5.42%.
   Not applied here (it moves every metrics pin): branch
   `widen-table-static-proof` is the one commit with the metrics re-pinned
   (only the five `.metrics` files moved), t0/t1/t2/big green, both level
   -1 tables' fingerprints unchanged. Merge it if wanted.
5. **Trees without `level.txt`** (all fixtures, every tree built before
   this branch) are accepted silently. Alternative: refuse, and rebuild or
   stamp the fixtures.
6. **Reclassified**: the timers and the key's sprite EXACT (dead,
   checked); the dash effect EXACT and in fact never read in the traced cart
   but to decrement it (its old comment said "read only as > 0", the
   original cart's); the strawberry bob an over-approximation at every
   level (it was undocumented; collecting the berry becomes possible at any
   phase, refuted by the concrete search).
7. **The countdowns are still recognized by field NAME** in the
   interpreter (SPEC annex D tr-9); the list now comes from the table.
8. `widen-check` cannot check a mid-split-frame step at a near level (the
   graph side keeps a floor's computed `collideable` in the probe window
   there, the block side does not; the same limitation as `MarkFilter`).

## To-do (Philippe, 2026-10-10)

- **No-player platform `spd.x` (`src/trace/widen.rs`, the platform input hook,
  "NO PLAYER" branch).** In a no-player shape the hook reads a platform's
  `spd.x` as its one moving speed v and owes per lane "the input is 0 or v"
  (0 on the platform's creation frame: `init` never sets it, `update` sets
  `dir*0.65` after the frame's `move`). The only input assumption outside the
  Pin/Restrict family. Open questions: why the `0 or v` assertion is needed at
  all (the move is argued unobservable without a player, `x` being widened to
  the path at `p`), and whether it should become a table entry (exact,
  depending on the `p` widening of `x`) plus a pin with a set check. Not
  decided; do not change without Philippe.

## Lines

Rust under `src/` and `crates/`: 46,262 before, 46,550 after (+288).
`trace::widen` 941 -> 818 (85 of them the dead-field test), `runtime2`
1,092 -> 646, `widening` 0 -> 984 (the table with its proofs ~330, the
block interpreter ~260, the rendering ~100, tests ~90), `rewrite
widen-check` +80, the level plumbing the rest.
