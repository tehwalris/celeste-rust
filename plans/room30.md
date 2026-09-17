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
  region on its kernel.
- `S <= 7`: the move loop is unrolled for `abs(amount) <= 8`. The game's
  largest speed is the dash's 5.
- Only exact-speed, exact-position levels (the held ladder `r*sxh`, `rxsx`);
  the speed-key fixpoint refuses with a region grid.

## Measurements

(filled in below as they land)
