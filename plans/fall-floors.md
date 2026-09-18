# The fall floors: unknown at the coarse levels (DISCUSSION 2026-09-17; level 0 built 2026-09-18)

## Built (2026-09-18, level 0 only)

Level flag `b` (`abstraction::FloorsPrecision`, spec `r0sxhfb` / `r0sxhb`: rem
Bits(0), exact position, its own registry slot), built on the fly fruit's
machinery (plans/fly-fruit.md), no new mechanism:

- **Input** (`widen::fork_floor_inputs`): every fall floor's `state` and `delay`
  the unknown number, `collideable` a FORK of both values (`Symbolic::both_values`,
  the held buttons' fork: no validity, every configuration for every lane). A
  never-broken floor's missing `delay` is materialized first (`ABSENT_AS_ZERO`),
  then replaced.
- **In the frame**: a comparison with the unknown is an atom; the floor's own
  branches split and rejoin by the independent join (unknown / hull / a fresh
  atom), so a floor's update leaves one state. Its `collideable` writes join
  into an atom, and an atom made inside a call that ESCAPES it (in the heap or
  returned) becomes one fresh fork of both values at the call's return
  (`Domain::escaped_atom`, `Interp::fork_escaped_atoms`). So the player, which
  updates after the floors, reads forks: per configuration its collisions are
  decided, and a floor its region's ranges fold away adds no configuration.
- **Output** (`widen::widen_fall_floors`): `state`/`delay` `AV::UNum`,
  `collideable` `AV::UBool` (`emit::bind`); the lattice never pins them.
- **Projection** (`Rt2::widen_to(.., floors)`, step 8): the same, per lane
  checked; a block from the post-`_init` state has no `delay` cell and is keyed
  exact (`Block::from_state`).
- The reference engine refuses a floors-unknown level.

**The first trace blew up** (both `r0sxhb` and `r0sxhfb`: the spawn shapes
past 2M nodes, 60 GB at a 30M budget). Every floor merge refused over the SPAWN's
`delay`, `Sel(Lt, 3, 0)` on both sides: `state::merge` counted a select a slot
already held (from an earlier merge, identical in both states) as a select on
this merge's condition, and on an atom condition a select refuses. So each
floor's update left 6 states, multiplied through the object loop (1.2M `collide`
iterations in one trace). Fixed: a merge counts only the selects it made
(`a != b`, and `ok` likewise). An unchanged select reads its own condition and
carries its own premise, so dropping the redundant `Known(cond)` is sound.

**Then three more, in order** (the floors-only trace, room (3,0)):

1. `collideable` as an atom: the player's `on_ground` and every "stop or step"
   stayed undecided and its successors multiplied (46M refused merges in a
   15-minute trace, all on the player's data: `x`, `grace`, `accel`, `freeze`).
   Input as a fork: no change - the floor's OWN update joins its `collideable`
   writes under undecided `state` branches into a fresh atom, which the player
   read next.
2. Minting a fork at every such join instead: `fork 58 but a ChoiceSet holds
   only 58` (about four joins per floor). Hence the escaped-atom rule above:
   one fork per floor, at its update's return.
3. The kernel build then went OOM: outcomes over 16 forks (917,504
   configurations). `and` splits on an undecided `collideable` and merges back,
   and the merged guard `(g and c) or (g and not c)` never folded to `g`, so
   every outcome's `live` read every floor's fork. `Graph::fold` now folds it
   (exact for every concrete `c`; the negation may be the opposite comparison),
   test `a_split_guard_merged_back_is_the_guard`.

Result, floors only (`r0sxhb`, quick profile): walk 17.6 s (9 shapes, 114
nodes), 114 kernels with 272,634 bodies (25,034 at `r0sxh`), level-0 counts
identical to `r0sxh` through f40 (176,925), but frames ~2.8x slower (f40 1.13 s
against 0.40 s). The largest outcomes still enumerate 98,304 configurations =
24 button reps x 2^12: regions where the player has no static range
(`successor_regions`' window fallback) fold no floor away. Cost, not soundness.

Then with the fruit too (`r0sxhfb`), two more fork-id wastes (`fork 63 but a
ChoiceSet holds only 58`): the call-return swap minted forks for atoms in the
callee's dead frame (it now walks only what the caller reaches, `Heap::reachable`),
and the input `collideable` forks were dead (the floor's update replaces them;
input is an atom now).

### `r0sxhfb` level 0, room (3,0) (2026-09-18, release)

115 kernels, 330,549 bodies, 5.0M fused nodes; the largest kernel 14,656
bodies, 29 forks, 173,301 fused nodes. Level 0 nearly saturates:

| frame | `r0sxhf` (fruit only) | `r0sxhfb` | frame time `r0sxhfb` | growth |
|---|---|---|---|---|
| f44 | 376,427 | 383,340 | 5.1 s | |
| f50 | 2,523,979 (6.4 s) | 2,520,816 | 98.7 s | x1.24 |
| f55 | 17,556,119 (52.6 s) | 6,563,684 | 454.9 s | x1.17 |
| f58 | 60,781,081 (291.7 s) | 8,984,868 | 701.4 s | x1.12 |

States grow x1.1 per frame by f58 (the fruit alone: x1.5), but a frame costs
~15x more per state than `r0sxhf`: every lane runs every configuration of its
kernel, and the big kernels enumerate 2^12 and more.

**A segfault at f59**: the largest kernel's spill frame is 83 MB (`sub
$87047808, %rsp`), the forward workers' stack was 64 MB. Workers now get 512 MB
(virtual), record it (`asm_kernel::set_thread_stack`), and every kernel call
asserts its frame fits (`Compiled::frame_bytes` + 1 MB): a loud refusal instead
of a touch past the stack.

### One floor too many, and the kernel dev loop (2026-09-18, afternoon)

Philippe's hand count of the floors an 8 px square can reach disagreed with the
kernels: square (5,13) forked on the floor at (64,112), 17 px to the right. The
fork origins now name the floor (`collideable at x 64 y 112`, from the table's
`x`/`y` at the call return), and `CELESTE_KERNEL_DIFF=<outcome>@<fork>` with
`CELESTE_KERNEL_CONE=N` prints the two bodies' difference as a DAG with each
node's static range. The reader was the wall-jump test `is_solid(3,0)` after
the move, with the moved `x` in [32, 55] for a square [40, 47]: 8 px each
way. `rem` had no seeded range, so the move amount `flr(rem + spd + 0.5)` had
none, the unrolled `for i=0,abs(amount)` kept every one of its 8 iterations,
and every later collision box grew by a pixel. Fixes:

- the region key also bounds `rem.x`/`rem.y` to [-0.5, 0.5) (`RegionGrid::
  bounds`, guarded in `ok` like the rest: a lane outside declines);
- `Symbolic::flr_ways` is capped at `move_ways`: with any range seeded it
  took the arity from the static range's width, which for a region's speed
  range [-6, 6] is 13 floors (35,502 bodies in (5,13)); one lane's interval
  spans at most `move_ways` floors, and too small an arity declines (`SplitOk`).

Square (5,13): 1,236 -> 852 bodies, forks only on its two floors. The 16 px
spawn square (2,6), the main player shape: 1,254 -> 870 bodies, its big
outcomes 384 = 24 button reps x 2 floors x 2 move forks, the static count
the abstraction predicts. Level-0 kept counts f1-f40 at 32 px are identical
before and after. 16 px is the default grid now (`region_grid_for`;
`CELESTE_REGION=off` for none): 307 kernels, 158,848 bodies (330,549 at 32 px
this morning).

The dev loop (`CELESTE_KERNEL_ONLY="(5,13)"`: lower and assemble only those
regions, then exit; `none` stops after the walk) went from 100+ s to 18 s:

| | before | after |
|---|---|---|
| walk, 8 px grid (979 nodes, 1,266 traces) | 38.9 s | 16.5 s |
| lower + assemble (5,13)'s 3 kernels | 80 s + a codegen over 60 s | 1.8 s |

- **Merges of an unchanged heap** (`state::same_heap`, `Sides::Same`): the arms
  of `a and b` where `b` only reads hold the same heap by id, so the merge pairs
  every slot with itself and needs no `Canon` - computing both was a third of
  the walk. Equal values join to themselves and a select of equal arms folds, so
  the result is the one the canon path made. 38.9 s -> 26.0 s.
- **The build's purge delay** (`asm_kernel::BuildPurgeDelay`): `safe-run.sh`
  purges freed pages at once, and under the tracer threads that was a third of
  the walk in TLB shootdowns. While a kernel set builds, purges wait a second.
  26.0 s -> 16.5 s.
- **The spill-slot expiry** was a list scanned per interval, quadratic in the
  largest kernels; a heap now.

What is left of the walk is the tracer itself: ~0.7 CPU-s per trace, heap
clones at splits and merges 31%, dropping states 17%, canons of `if` merges
16%. Copy-on-write tables would be the next step.

Room (3,0) has 12 fall floors. Without the fly fruit, level 0 still grew x1.49
per frame at f55, and erasing the floors merged 3.13x there (plans/room30.md).
This records the field-by-field analysis and where the discussion landed; the
mechanism it needs is shared with plans/fly-fruit.md.

## Fields

A floor is an object (`init_object`) plus `fall_floor.init` (`state = 0`,
`solid = true`); `break_fall_floor` adds `delay`. Checked in the cart: the
player's collisions go `is_solid` -> `check(fall_floor)` -> `collide`, which
reads the other object's `type`, `collideable`, `x`, `y`, `hitbox`; the floor's
`solid` field is written and never read; the floor has no `draw`.

| field | values | read by | most it could be widened |
|---|---|---|---|
| `type`, `x`, `y`, `hitbox` | constant | every `collide` | nothing to gain (constant; unknown would make every collision undecided) |
| `spr`, `flip`, `solid` | constant | nobody | nothing to gain |
| `solids`, `spd`, `rem` | constant (true, 0) | the floor's own `move`, which never moves it | none needed |
| `collideable` | true; false while hidden (state 2) | the player's `is_solid`, every pixel step | unknown boolean, FORKED where read (below) |
| `state` | 0 idle, 1 shaking, 2 hidden | only its own `update` and `break_fall_floor` | unknown |
| `delay` | 15 -> 0 shaking, 60 -> 0 and below hidden, 0 by the "missing" hack | only its own `delay - 1`, `delay <= 0` | unknown (a checked range such as [1, 15] is a fixed point but no more precise, since the transition is undecided anyway) |

Measured (no-fruit experiment, f55, erasing across all 12 floors): `delay`
2.09x, `state` alone 1.00x, `collideable` alone 1.00x, all three 3.13x.

## Where it landed (Philippe)

At the coarse levels all three are unknown: every floor is just "maybe solid",
nothing unique per floor, no row differs by floor timing. With the region key
(`CELESTE_REGION`), a floor outside the region's box plus the player's reach
folds away in the range analysis (its overlap with the player, and its own
`check(player, ...)`), so only the one or two floors near a region stay
undecided and the forks stay bounded.

## The two mechanical problems

1. **The player's collisions.** An unknown `collideable` makes the move loop's
   "stop or step" undecided; the tracer would merge the arms under a
   `Known(cond)` premise and the kernel would refuse every such row. Fix, as the
   held buttons: fork each nearby floor's `collideable` input into both values at
   frame start (`SplitInt`), so each configuration's collisions are exact and the
   player's outcomes are separate rows (x2 per nearby floor). Unreachable forks
   (far floors) add no configurations.
2. **The floor's own update.** With `state`/`delay` unknown its branches are
   undecided and merge under the same premise. Everything it computes is
   overwritten by the widening at the frame's end, and in room (3,0) it has no
   other effect (it reads the player, writes only itself; no spring), so the
   refusal protects dead values. Options: fork its branches too (3 x 2 per floor,
   x36 for two floors: too many); a fork where the floor is processed that
   rejoins at once (the mechanism plans/fly-fruit.md needs); or attach a merge's
   `Known` premise only where the merged value is still read.

## Precision

"Maybe solid on any frame" lets the player jump up through an unbroken floor
from below and stand on a gone one: faster false climbs, an earlier first win at
level 0, more horizons. The same two-rungs-at-`r0` pattern as the fruit: a
coarse `r0` with the floors (and fruit) unknown, then an `r0` with exact floors
filtered by its marks.

## Order

1. The unknown number type (plans/fly-fruit.md, being built).
2. The local fork-and-rejoin (or the premise change), shared with the fruit.
3. The floors' widening on top; `collideable` forked where read.
4. Measure room (3,0) level 0 with floors and fruit unknown.

## Removing the `delay` hack (`widen::ABSENT_AS_ZERO`)

Today every frame writes a missing `delay` as the number 0, at every level
(de24a0e; `cart::check_absent_fields` refuses a cart where the field is read
other than by arithmetic or an ordering comparison, both runtime errors on nil).
It exists because a floor gains `delay` only when it first breaks, which made
"which floors have ever broken" 12 bits of the heap shape. Philippe: a hack,
TODO to remove.

- **Coarse levels**: once the floors' `delay` is unknown, a missing `delay` can
  be written as that unknown instead of 0 - within the rule "only replace a
  value by something containing it" only if the unknown value covers nil too
  ("any value", not just any number). The unknown number type
  (plans/fly-fruit.md) should be built with that property in mind.
- **Exact levels**: `delay` stays exact, so a never-broken floor still has no
  field; without the hack the exact levels are back to 2^12 shape variants, each
  with its kernels. Replacement: a real nil-or-number field (a per-row tag for
  "present"), which the kernels read with a premise where the cart does
  arithmetic on it.

Then `ABSENT_AS_ZERO`, `materialize_absent_fields` and `check_absent_fields` go.
