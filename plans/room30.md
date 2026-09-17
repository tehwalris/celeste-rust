# Room (3,0): fall floors, absent fields and the region key (2026-09-17)

Room (3,0) has 12 fall floors in six adjacent pairs, one fly fruit and the
player spawn; no spikes, no springs. Two things stopped the kernel build:

1. the SHAPE count: a fall floor gains `delay` the first time it breaks;
2. the per-frame TRACE: with the player's position unknown, every collision
   loop of every move step checks all 12 floors.

## 1. Absent-as-zero fields (`widen::ABSENT_AS_ZERO`)

`fall_floor.init` sets `state` and `solid`; `break_fall_floor` adds
`delay = 15` and nothing removes it. The heap shape lists an object's field
names, so "which floors have ever broken" was 12 bits of the shape: up to
4,096 shapes (times fruit gone / player dead). The walk found 128 new shapes
in 150 s and had not converged; the diffs were the tail of `objects` (the fly
fruit or the player deleted: `del` leaves a trailing nil slot) and `delay`
added.

The fix writes a missing `delay` as the number 0 at every frame's outcomes
(`trace_frame` and the reference engine's `run_frame_all`), at every level:
it is part of the shape, not of the precision. Why nil and 0 cannot be told
apart by this cart:

- the cart only does `delay - 1` and `delay <= 0` with it (and `> 0`, `< 0`
  on the spring's and the spawn's own `delay`, which `init` sets);
- on nil each of those is a runtime error that halts PICO-8, so a path that
  would read the missing value is a path the real game never continues -
  treating it as a number is the crash-safe answer (Philippe, 2026-09-17);
- `cart::check_absent_fields` refuses the program if any `.delay` is not an
  assignment target or a direct operand of arithmetic or an ordering
  comparison (`==`, `and`/`or`, a call argument, a local: refused), and
  `Interp::index_key` refuses a bracketed or computed `t["delay"]`.

A consequence: the post-`_init` start state (no `delay` yet) is a shape no
frame ever returns to, so no outcome narrows its constant lattice, and it kept
`__button_states[0..5] = false` pinned. The block holds the buttons unknown
(their canonical form: dead at the boundary, reset before every read), so the
start kernel's pin guard read an undecided boolean at f1. In rooms (1,0) and
(2,0) the spawn's own successor narrowed the pin away. `field_constants` now
never makes a button a constant.

Rooms (1,0) and (2,0) have no fall floors: the three pinned gates reproduce
(ckhash f0-f44 empty diff, posgraph f044 identical, marks identical).

Measured on the upper-floors box (the earlier bounds experiment): the walk
converges at 6 shapes, 18 traces, where it had 128+ shapes unconverged. The
fruit stays two shapes (gone / not), as does death.

## 2. The region key (`trace::kernel::RegionGrid`, `CELESTE_REGION="px,S"`)

A kernel is specialized on the player's whole-pixel `x`/`y` lying in one
square of a `px` grid and `spd.x`/`spd.y` in `[-S, S]`, all guarded in `ok`
(a lane outside declines loudly). The range analysis then folds the far
floors' collision tests.

- The walk (`room_constant_lattice`) is over (shape, region) nodes. The
  constant lattice stays per shape; a narrowed lattice re-traces every
  region its shape has reached. Successor regions are the hull of the
  outcome's player `x`/`y` (`Symbolic::range_of`), clamped to the screen
  plus one region; an axis with no static range (the player the spawn
  creates at its own position) takes that whole window. Both only ADD
  kernels: a row outside has no kernel and stops the run (`[asm] MISS`).
- The registry is keyed by (shape hash, `KernelKey { speed, region }`); a
  chunk's lanes arrive in cell order, so `run_chunk` runs each run of one
  region on its kernel. A chunk takes a region only if its shape has a
  `player` object (the walk's `player_path` rule): a row's CELL also locates
  a `player_spawn`, and the first forward stopped at f1 on the spawn at
  y = 128 asking for region (0, 4) of a shape whose kernel has none.
- `S <= 7`: the move loop is unrolled for `abs(amount) <= 8`. The game's
  largest speed is the dash's 5.
- Only exact-speed, exact-position levels (the held ladder `r*sxh`, `rxsx`);
  the speed-key fixpoint refuses with a region grid.

## Measurements

- **The level-0 walk with `CELESTE_REGION=32,6`** (`transpile --room-consts`,
  quick profile) converges at 9 shapes in 114 (shape, region) nodes, from
  471 traces in about 5.5 min, 713 MB resident. Two thirds of the traces are
  re-traces: a shape's lattice narrows a few pins at a time (shape 7: 245 ->
  215 pinned over 12 narrowings), and each narrowing re-traces all ~30
  regions the shape has reached. Parallel rounds, with narrowings applied
  between rounds, would cut both the re-traces and the wall time; not built.
  With the report's bind + lower of every frame: 6:17 wall, 968 MB peak,
  25,034 bodies over the 114 kernels.
- **Kernels against the reference engine** (`rewrite forward --reference`,
  `ckhash` of both trees), room (3,0) to f32:
  - exact level `rxsx`: IDENTICAL through f32 (4,818 states at f32, posgraph
    f032 identical). The region kernels and the absent-as-zero `delay` compute
    what the interpreter does.
  - level 0 `r0sx` (rem Bits(0)): identical through f28, then the kernels keep
    MORE states (f32: 7,364 against the reference's 2,778 and the exact level's
    4,818). Room (1,0) at the same level does the same (f33: the kernels'
    26,507, the pinned gate's own count, against the reference's 7,202; they
    part at f25): at Bits(0) the kernels over-approximate where the reference
    engine splits the interval, so ckhash equality is a test only at an exact
    level. Not a room (3,0) problem.
- **Level-0 forward** (`r0sxh`, release): 114 kernels in 315 s; states from f29
  (the player's first input), 22,656 at f35, 176,925 at f40 (x1.5 per frame),
  f40 in 426 ms, 0.95 GB.

## 3. Level 0 does not saturate: the fly fruit (2026-09-17, search stopped at f52)

The held-ladder search (`CELESTE_REGION=32,6`, `r0sxh..r15sxh,rxsx`, `--from 1
--to 250`) prebuilt its 17 kernel sets in 547 s (13 GB resident) and extended
level 0 at x1.5 states per frame with no sign of saturating:

| frame | states | frame time | RSS |
|---|---|---|---|
| f40 | 176,925 | 0.4 s | 13.1 GB |
| f44 | 825,170 | 1.6 s | 13.3 GB |
| f48 | 3,886,471 | 7.9 s | 14.5 GB |
| f50 | 8,993,524 | 20.8 s | 16.5 GB |

Every visited cell still had 20%+ new states at f47 (`rewrite cell-growth`).
It was stopped at f52; the partial run is in the UI as `room30`.

What multiplies (`rewrite coarse-census`, f47, 2,591,387 states):

| erased | states | fewer |
|---|---|---|
| all 12 fall floors' `delay` | 2,511,159 | 1.03x |
| everything on the fall floors | 2,465,319 | 1.05x |
| the player's `spd.` / `rem.` | 1,131,202 | 2.29x |
| the fly fruit | 1,127,623 | 2.30x |
| the fly fruit and the player's speed | 278,578 | 9.3x |

The fruit's fields vary TOGETHER: erasing only `step` (1.00x), only `y`/`rem`/
`spd` (1.01x) or only `fly` (1.00x) merges almost nothing, while `step`, `y`,
`rem` and `spd` jointly give 2.17x. So a lossless `step := 0` while flying (it
is never read again) would not merge the dash cohorts on its own; the position
has to be abstracted with it.

So the floor timers are not the problem; the FLY FRUIT is, twice over:

- while it waits, `step += 0.05` every frame and its `y`/`rem.y`/`spd.y` bob
  with `sin(step)`: every state of frame f carries a fruit no earlier frame
  had, so the door's cross-frame dedupe never fires. With the fruit erased the
  set is MONOTONE (the cumulative count through f47 equals f47's own count:
  idle inputs keep every earlier state), and the new states per frame at f47
  would be ~294k instead of 2.59 M (x8.8), growing x1.37 per frame.
- once the player dashes it flies (`spd.y` toward -3.5, frozen `step`), so
  rows split by the frame of the dash.

Room (2,0)'s `fruit` has a band widening (`widen_fruit`, `Rt2` 3b, the
interpreter's `make_state_abstract_rem`) and a lossless `off mod 40`. Neither
transfers directly: that fruit ASSIGNS `y = start + sin(off/40)*2.5`, so its
band is inductive; the fly fruit INTEGRATES `spd.y` through `move`, so an
interval band moved by up to a pixel is not contained in itself (the
containment premise would fail at the edge), and its waiting bob is not
exactly periodic (0.05 is not exact in 16.16), so `step mod 1` alone does not
make rows repeat. A sound fly-fruit abstraction needs a design of its own.

`CELESTE_EXPERIMENT_NO_FLY_FRUIT=1` (`game_runner::apply_start_room`) removes
the fly fruit type from the cart to measure the rest of the room without it.
A different game; nothing it reports is an answer for the real cart.
