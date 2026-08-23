# One frame, without writing rows we throw away

Standalone statement of the problem we are actually stuck on. No history;
everything needed to work on it is here.

## The system, in four sentences

`celeste-rust` searches for a provably optimal TAS of PICO-8 Celeste by
abstract interpretation: it holds a SET of game states and advances all
of them one frame at a time, deduplicating after each frame. Today that
set is advanced by a vectorized interpreter. We are replacing it with
GENERATED KERNELS - Rust compiled from a graph obtained by symbolically
tracing the cart's Lua - so the interpreter can be deleted. The kernels
work and agree with the interpreter exactly; they are 1.55x too slow.

## The task

> Given a block of N game states, produce the SET of distinct successor
> states, at least as fast as the interpreter produces it.

## What a kernel does now

A block is column-major (`Rt2`): one array per heap cell, N lanes. The
kernel processes it in slices of 16 lanes, and for each slice it runs the
frame body once, then enumerates every way the frame can branch on
something the search does not fix:

* **64 button assignments.** Six buttons, free per frame. The emitter
  folds these to **24 distinct** ones at emit time (many assignments
  compute the same thing).
* **Up to 4 fork configurations.** `player.rem` is widened to the
  interval `[-0.5, 0.5)` by the level-0 abstraction, so `flr(rem + spd +
  0.5)` can take two values; the cart marks the split with
  `__split_by_flr`, and there are two such calls per frame (x and y).

So **96 configurations per slice**, each producing up to 16 rows, in up
to 4 different output shapes ("outcomes" - a frame that kills an object
ends in a different heap shape than one that does not).

Each row is then written into an accumulator, same-shape accumulators
are merged, and the engine's boundary canonicalizes and dedups them.

## The numbers (room (1,0), frame 30, single thread)

    15,250 input lanes
     x 96 configurations (24 distinct assignments x 4 fork configs)
    = 1,246,632 candidate rows
    ->  197,612 written   (after the kernel's own dedup)
    ->   27,024 distinct  (after the boundary)

    compute           90 ms
    append + dedup   329 ms      <- the problem
    merge              2 ms
    boundary          23 ms
    total            356 ms   vs the interpreter's 230 ms

Writing all 1.25M rows cost ~175 ms and made the boundary cost 138 ms.
Checking all 1.25M candidates so as to write only 197k costs ~240 ms and
makes the boundary cost 23 ms. The two are nearly a wash.

## What is static and what is not

The emitter knows, per outcome, which output cells are compile-time
constants (one value for the whole accumulator) and which can differ
between button assignments. Measured on the room's kernels:

                per-variant   shared, non-constant   constant   total
    outcome 0        0                4                 50        54
    outcome 1        2                3                 30        35
    outcome 2        3                2                100       105
    outcome 3       16               13                 30        59

Outcome 0's 24 assignments produce a BYTE-IDENTICAL row for every lane,
knowably at emit time - it has no per-variant cells at all. No runtime
comparison can ever say otherwise.

For the others, two assignments may agree on one lane and differ on
another, so per-lane equality is genuinely dynamic - but it depends on
at most 16 cells, and on 2 or 3 for two of the four outcomes.

Note the ratio: of 253 output cells across the four outcomes, 210 are
constants that are already written once, and 43 vary per row.

Fork configurations are different: they change values through the frame
body, so rows from different fork configurations are generally distinct.

## Constraints

1. **The successor SET must be exactly the interpreter's.** Checked per
   frame by row-key set equality (`the_room_runs_on_kernels_alone`), not
   by count. A missing successor is a wrong answer, not a slow one.
2. **The row key is fixed.** A 128-bit hash over canonical cell values,
   summed per cell so that block-uniform cells fold once. It is what
   every checkpoint, the backward sweep and the position graph speak, so
   it cannot be redefined.
3. **Never deopt.** A lane the kernel cannot handle stops the run with a
   reason; there is no interpreter fallback. (CLAUDE.md.)
4. **The generated code must compile in reasonable time.** One frame is
   ~6,000 graph nodes emitted as one function. Anything that lengthens
   live ranges across the variant sequence is dangerous: unioning masks
   across 24 variants took the build from 70 s to 20+ minutes, and
   letting LLVM see the fork loop's trip count did the same.
5. **16 lanes, `u16` masks.** Liveness, validity and deopt are all
   16-bit masks; the lane width is not a tuning parameter.
6. **Widening happens inside the kernel**, so a row is hashed on the
   value it stores. Every boundary widening is a per-row function.

## What has been tried

* **Constant columns written once** instead of per row. 44 of 52 output
  fields are compile-time constants. 604 -> 405 ms. KEPT.
* **Write-time dedup** with a 128-bit key over non-constant cells, in an
  open-addressed generation-stamped table. Rows written 1.25M -> 197k,
  boundary 138 -> 23 ms, but the kernel phase rose 252 -> 329 ms. KEPT,
  net 405 -> 356 ms.
* **A `std::HashSet` for that dedup.** Hashes the key a second time;
  append 175 -> 279 ms. REPLACED.
* **Static grouping as a bolt-on** - give one variant the union of its
  group's take masks. Correct, but it keeps 48 mask variables live
  across the whole function: build 70 s -> 20+ minutes. REVERTED.

## The question

How do you enumerate the DISTINCT successors of a state without
enumerating all 96 and then comparing them?

The specific shape of the opportunity: most of the duplication is
answerable at emit time (outcome 0's 24 assignments are provably
identical), and the rest depends on at most 16 cells. The current design
asks a general-purpose runtime question - hash 493-cell rows, probe a
table - 1.25 million times per frame, to answer something largely known
in advance.

Candidate directions, none chosen:

1. **Group-driven emission.** Emit one output group per outcome instead
   of 24 variants whose rows are then discarded. Kills the static
   duplication outright and removes the fork's runtime loop. Bigger
   change; must avoid constraint 4.
2. **A cheaper dynamic check** over only the distinguishing cells,
   vectorized across 16 lanes rather than scalar per lane.
3. **Neither** - accept 1.55x and spend the time on coverage instead,
   since deleting the interpreter is blocked on the kernels covering
   more than one room and one ladder rung, not on speed.
