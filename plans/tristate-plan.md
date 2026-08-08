# Tri-state comparisons and the fruit fast path (2026-08-08)

Replaces the whole-value `UnknownBool` that interval comparisons emit
when ANY lane straddles. Motivation and acceptance criteria are
Philippe's, recorded in room00-plan.md "Queued after convergence":
LANE INDEPENDENCE (no lane's imprecision may affect another lane) and
its general form BATCHING INVARIANCE (running a partition of a state
set separately must equal running the concatenation).

## Why now

Measured on the archived room-(0,0) sweep logs: the level-0 backward
sweep replays 25.15 B edges at 0.46 M edges/s, against room (1,0)'s
2.71 M edges/s - a 5.9x penalty, consistent across five frames
(f089-f093). The cause chain, from the log:

    at %157 = select %125 ? %154 : %125: select on a condition with
    no per-lane value: UnknownBool ... whole-state fallback
    (in fruit.update_34 -> obj.collide_49)

The strawberry's `off` is widened to [0,39] so `sin` spans [-1,1], so
its `y` is an interval, so the collide comparison straddles for some
lanes - and one straddling lane collapses the whole value, forcing the
entire state onto the plain program (3-4x slower per lane), which
branches (copying all lanes down both arms), with origin tags blocking
dedup in the sweep, mitigated by 100x tighter chunking that destroys
vectorization. A fresh room-(0,0) re-derivation at this rate costs
14-20h; that is why it is worth fixing before re-deriving.

## Design: resolve eagerly, never persist

`Value::MaybeBool(MaybeVector<Option<bool>>)` is a TRANSIENT. The
invariant is that it never reaches local_env, the heap, a checkpoint or
a row hash - it exists only between an instruction computing it and
that instruction's result being assigned.

Rationale: the dangerous failure mode for a new per-lane variant is
silent lane desync - a variant that lands in a `_ =>` arm of
`expand_lanes` or `filter_by_mask_in_place` is not resized with its
neighbours and every downstream lane index is wrong, with no error.
Making it transient means those handlers can reject it LOUDLY
(house rule: a guard beats an assumption) instead of being extended
to handle a case that should never arrive.

Pieces:

1. Interval comparisons (op.rs) return `MaybeBool` when lanes disagree
   on definiteness. All-definite still returns `Bool`; all-unknown
   still returns `UnknownBool`. So behaviour is bit-identical except in
   the mixed case, which is exactly the case that was spuriously
   collapsing.

2. `State::duplicate_lanes(mask)` appends copies of the masked lanes
   (the selective sibling of `expand_lanes`, which doubles everything).

3. `resolve_maybe_bool`: given a `MaybeBool` and `&mut State`,
   duplicate the ambiguous lanes and return a definite `Bool` - the
   original ambiguous lanes resolved true, their appended copies false.
   Exact: the interval genuinely admits both, so both are reachable.
   No invented values (the interval-hull shortcut is REJECTED - it
   would admit values no execution produces).

4. `interpret_non_call_instruction` resolves before assigning. One
   choke point, so select AND branch both get definite conditions and
   need no new cases at all.

## Staging, each independently gated

* S1: add the variant, make everything compile, never construct it.
  Gate: byte-identical bench artifacts + both verifies. Proves the
  plumbing is inert.
* S2: `duplicate_lanes` + `resolve_maybe_bool` with unit tests.
* S3: flip the comparison to emit `MaybeBool`. Gate: room (1,0)
  unchanged (its level-0 interval comparisons, if any, must not
  straddle); room (0,0) verify passes and the deopt count drops.
* S4: `simdcheck` - batched vs singleton-split runs must agree as a
  lane set. The acceptance gate for the whole change.
* S5: measure the sweep rate, then re-derive room (0,0) once.

## Note on soundness of the current code

The v1 whole-value collapse only ever made things COARSER (more
deopt, more duplication), so it never dropped a reachable state: the
94-frame proof stands, and k16 (exact rem) never sees interval
comparisons at all. This is a latent hazard and a large performance
tax, not a wrong answer.


# Postscript: the collapse is a cross-lane JOIN, and the fix is a PARTITION

(2026-08-09, from Philippe.)

`Value::UnknownBool` is a whole-VALUE variant with no lane structure, and
it gets constructed in two situations that are not the same operation:

* every lane straddles - then it is a lossless COMPRESSION of "a vector of
  unknowns", and lanes have still not interacted;
* some lanes straddle - then it is a JOIN ACROSS LANES, and lane 3's
  ambiguity destroys lane 5's perfectly good answer.

Only the first preserves the per-lane independence the whole vectorised
abstraction rests on. Stating the invariant that way - *UnknownBool may
only be built when every lane is unknown* - names the bug precisely, and
it says what the fix has to be.

MEASURED, room (0,0) to f68, counting at every construction:

    14603 constructions, 10233 mixed (70.1%)
    2,396,690 of 3,470,283 lanes had a definite answer (69.06%)

So the lossy case is the common case, and roughly two thirds of the
precision at each site is thrown away.

## Why this is not tri-state again

Tri-state (this file, above) attacked the same problem and lost on memory:
102 GB at frame 66. The reason is now clear, and it is not "the estimate
was wrong" - it is that `resolve_maybe_bool` DUPLICATED every ambiguous
lane into two (a true copy and a false copy), so the growth compounds
multiplicatively across the several comparisons inside `obj.collide`.

Partitioning does not duplicate anything. The state splits into

* the lanes with definite answers, carrying a real `Bool` vector, and
* the lanes that straddle, carrying `UnknownBool` - where the all-unknown
  invariant now genuinely holds.

TOTAL LANE COUNT IS PRESERVED EXACTLY. It restores the invariant instead
of working around its absence, and it is strictly better than both current
options: no contamination (unlike the collapse) and no growth (unlike
tri-state).

It also lands on room (0,0)'s actual bottleneck. The whole-state deopt to
the plain program fires because `select` meets an `UnknownBool` condition;
under partitioning only the straddling side deopts and ~69% of the lanes
stay on the specialized program.

## The obstacle, which is architectural rather than semantic

`interpret_binary_op` returns a `Value`, not states - which is exactly why
`MaybeBool` was designed as a transient resolved at the assignment choke
point. Partitioning needs an INSTRUCTION to yield two states into the
flow. Branches can already do that; instructions cannot. That is the work,
and it should be scoped before anything else here is touched.

Note the pleasant consequence: `MaybeBool` and its guards already exist
and are already tested. The transient is the right carrier; only its
RESOLUTION changes, from duplicate-the-lane to split-the-state.
