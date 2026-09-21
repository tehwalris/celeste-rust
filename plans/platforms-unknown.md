# The platforms unknown at the coarse levels (plan, 2026-09-21)

Room (6,0) has ten moving platforms. Their positions change every frame, so a
state reached at frame t never equals one reached at t' != t: no cross-frame
dedup, and level 0 grows x1.4 a frame (81M states, 50 GB at f50 of 70;
plans/overnight-2026-09-19.md, stops 3-5). The fix is the one the fly fruit
and the fall floors already use: on the coarse rungs the platforms' phase is
forgotten (every state holds the same widened platform fields, so states from
different frames merge again), and the exact rungs at the top of the ladder
keep the real positions, filtered by the coarse marks as usual.

## The platform code, one frame (lua/celeste-minimal.lua)

Platforms are created when the room loads, so they update before the player.

```
move:    rem.x += spd.x + 0.5; rem.x = __split_by_flr(rem.x)
         step = flr(rem.x)             -- 0 or 1 px in dir (spd.x = 0.65 dir)
         rem.x -= 0.5 + step
         x1 = x0 + step                -- solids = false: no loop
update:  spd.x = dir * 0.65
         x2 = x1 < -16 ? 128 : (x1 > 128 ? -16 : x1)          -- the wrap
         if not check(player, 0, 0) and hit = collide(player, 0, -1):
             hit.move_x(x2 - last, 1)                         -- the carry
         last = x2
```

`last == x` at every frame boundary: `init` sets it, `update` ends with it, a
frozen frame changes neither, a reload re-runs `init`.

The player lands on a platform through `is_solid`'s one-way check
(`oy > 0 and not check(platform, ox, 0) and check(platform, ox, oy)`).

## The design

A new level flag `p` (`abstraction::PlatformsPrecision`, spec suffix after
`b`), valid where `f`/`b` are (a rem rung, exact position), wired like the
floors flag: `Level`, the parser and `Display`, `grid_consistent`,
`coarser_or_equal`, the process global and `set_level`, `WalkOpts::platforms`,
`Symbolic::platforms_unknown` (and `unknowns()`), the kernel-set slot, and
`Rt2::widen_to(.., platforms)` for the mark filter's projection.

At a platforms-unknown level:

1. **Stored rows.** Every platform's `x` and `last` hold the uniform interval
   `[-16, 128]` (the whole path: a platform is only ever in it), `rem.x` the
   literal `[-0.5, 0.5)`. `spd.x`, `dir`, `y` are unchanged (constant after the
   first frame). The boundary widening (`Rt2::widen_to` step) checks each lane
   before it writes: `last == x` and both inside `[-16, 128]`.
2. **Inputs of a traced frame.** `x` is an interval INPUT cell (per-lane data,
   like the balloon's phase), and `last` is bound to the SAME value - the
   relation `last == x` as symbolic identity. `rem.x` is the literal.
3. **The move.** `__split_by_flr` of the literal `rem.x` runs as literal
   fragments (existing: `Domain::literal_fragments`), one trace state with
   step 0 and one with step 1, rejoined when `move` returns. NEW: that join of
   `x0` and `x0 + 1` is `x0 + [0, 1]` (a structural join: both arms are the
   same value plus a literal; the hull of the literals keeps the value), not a
   split into two successors.
4. **The carry.** NEW in the tracer's subtraction (`Symbolic::arith`, not
   `Graph::fold`, which must stay exact against the range evaluator):
   `(b + c) - b = c` when both `b` are the same value (exact in 16.16 wrapping
   arithmetic). With `last` bound to `x0`, the carry is `x0 + [0,1] - x0 =
   [0, 1]`: the existing loop premise `abs(amount) <= 8` holds.
5. **The wrap.** `x1 < -16` / `x1 > 128` compare an interval with a constant:
   the point split (`split_compare`) makes each a fork of both answers. In the
   non-wrap configuration `x2` IS `x0 + step` and the carry cancels; in a wrap
   configuration the platform sits at exactly 128 / -16, where the collide
   with the player's exact x is decided per lane (a clamped player never
   touches it; a lane that did would decline loudly, never be dropped).
6. **Landing.** `check(platform, ..)` against an interval x is undecided: the
   point split again, a fork per platform the player's region can reach (2-3
   in a platform row, none elsewhere: the region's ranges decide the rest).
7. **Outputs.** `x` and `last` are written back as the `[-16, 128]` uniform
   interval, `rem.x` as its literal. The containment is NOT visible from the
   value (`x0 + [0, 1]` reaches 129): it follows from the wrap, which is
   `x2 = x1 < -16 ? 128 : (x1 > 128 ? -16 : x1)`, in [-16, 128] for any x1.
   Proved statically on the traced select (both conditions are point splits of
   comparisons of the same `x1` with the two constants), else a runtime premise
   in `ok` that declines loudly. And the output `last` must be the output `x`'s
   node (`last = x2`): checked on every traced outcome, it is what makes the
   input alias of step 2 sound by induction (with the start state's concrete
   `last == x`).

## Soundness in one paragraph

Every widening only over-approximates (the platform is somewhere in its path,
its sub-pixel remainder anywhere); the alias `last == x` is a checked
invariant (the start state, and every traced output); the cancellation is an
exact identity; the forks enumerate both answers; the output containment is
proved or checked. A lane the model cannot handle (a carry that touches the
player in a wrap configuration) declines as a coverage gap, never silently.
The exact rungs keep the real platforms and are filtered by the coarse marks.

## Status (2026-09-21, afternoon)

Built (steps 1-7, flag `p`): suite 126/126, ckhash / posgraph / marks OK (the
cancellation and the structural join change nothing in room (1,0)).

Room (6,0) at `r0sxhp`: the lattice walk traces cleanly - 8 shapes, 305
(shape, region) nodes, 914 traces in 9.4 s, no fan-out, `last` stays `x`,
the carry cancels. The KERNEL BUILD then stops: "fork 58 but a ChoiceSet holds
only 58". The worst traces mint 82-86 forks, 76-80 of them point splits
(`Lt`/`Gt split at a point`: every comparison of a platform's interval `x`
with a number, `collide`'s x-overlap test before its y test, for every
platform in every collide) and 2 held-button forks.

The limit is on fork NUMBERS, not on what a kernel enumerates: `specialize_frame`
enumerates per outcome only the forks in its own cone (`bits_of`), but
`Graph::choice_cones` builds a 64-bit mask per node of the whole arena and
`Choice::bit` asserts every fork number fits. So: if each outcome sees few
live forks, widening `ChoiceSet` (128 bits) is the fix; if they see most of
them, forking on every platform comparison costs 2^k configurations per
outcome, and that is a design question.

Measured (`CELESTE_BUILD_TRACE`, all 914 traces): the forks reachable from
the outcomes (fields, guard, `ok`) are nearly all of them - 64-82 live per
trace against 70-86 minted, 66-78 of the live ones point splits. The cause
is the point split's VALIDITY: `split_compare` conjoins `(c and may_true) or
(not c and may_false)` into the path guard at the comparison, so every
outcome's `live` reads every comparison of the frame, whether or not the
answer changes any of its values. Widening the mask would trade the panic for
2^70 bodies per outcome. Needs a decision (below, "Options").

## Decided: fork AFTER the frame (Philippe, 2026-09-21)

The eager point split (`split_compare`, forking each interval comparison as
it is evaluated and putting its validity in the path's guard) is gone. A
comparison is traced as an ordinary condition: branches merge into selects
with `Known(cond)`, everything folds, and only then does
`verify::fork_known_premises` turn each SURVIVING `Known(cond)` whose `cond`
reads an interval comparison into a fork - `cond` -> the fork in every
outcome, `Known(cond)` -> true, the validity (`Symbolic::may_answers`) only
in the guards of outcomes that read it. Level -1 opts out
(`Symbolic::no_known_forks`): its evaluator joins undecided selects.

Room (5,0), the balloon the point split was built for, level 0 `r0sxhb` to
f60: kept counts IDENTICAL to the point-split run (f40 23,772, f45 89,797,
f50 296,257, f51 363,568, f55 646,053, f60 1,078,261) - no coverage gap at
f51. Room (6,0) at `r0sxhp`: still too many forks (worst traces 131-144 live,
nearly all from the pass) - under investigation.

## Options considered before (for the record)

1. A fork whose answers agree is no fork: where two configurations give an
   outcome the same values and `ok`, merge them and OR their `live` - the two
   validity terms OR to `may_true or may_false`, true for every lane. Sound
   (exact), general, but it is a change to `specialize_frame`'s enumeration,
   which today enumerates every fork in an outcome's cone before merging.
2. Keep the answer's validity OUT of the guard where the comparison's result
   does not reach any value: the fall floors' forks have no validity at all
   (`both_values`: every configuration applies to every lane), which is why
   their dead forks fold away. For a point split that is only sound if both
   answers are taken for every lane that could take either - i.e. the
   over-approximation of dropping the validity.
3. Fewer comparisons: a coarser model of `collide` against a platform at
   these levels (the platform "anywhere in its row"), one fork per platform
   per frame instead of one per comparison.

## Order of work, and what decides it pays

1. The flag plumbing, the boundary step, the trace inputs/outputs, the join and
   the cancellation (steps 1-7).
2. Room (6,0)'s kernel build at `r0sxhp` (fruit exact, floors irrelevant): does
   the start shape trace under the 256-state cap, and do the wrap and landing
   forks fold away where the region decides them? (The fruit-unknown build
   fanned out in this same room: 331 states.)
3. Level-0 growth to f45 at `r0sxhp` against today's `r0sxh` (17.9M at f45).
4. A ladder with `p` on the low rungs, exact at the top, and the gates.
