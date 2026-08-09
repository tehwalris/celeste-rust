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

## The obstacle is SMALLER than it looked (checked 2026-08-09)

I recorded below that partitioning "needs an INSTRUCTION to yield two
states into the flow. Branches can already do that; instructions cannot."
That is wrong - the machinery is already there.

`flow.rs`, the site that runs a block's instructions, accumulates into
`dst: Vec<State>`:

    Instruction::Call { .. }        => dst.extend(...)     // MANY states
    Instruction::AssertTrue { .. }  => ... dst.push(state) // ZERO or one
    _                               => ... dst.push(state) // exactly one

The `Call` arm already extends with many, and the lane-granular deopt's
`collect_assert_true` already yields zero or one. Subsequent instructions
then run over every state in `dst`. So a non-call instruction returning a
partition needs no new plumbing - only a wider return type on the arm that
currently pushes exactly one.

### Suggested shape, avoiding a per-instruction allocation

Do NOT return `Vec<State>` from `interpret_non_call_instruction`: it runs
for every instruction of every state and would allocate on the hot path
for a case that is rare. Return the spill instead:

    fn interpret_non_call_instruction(..) -> Result<Option<State>>

`None` in the overwhelmingly common case; `Some(ambiguous)` when a
`MaybeBool` was partitioned off. The flow arm becomes

    let extra = interpreter.interpret_non_call_instruction(*local_id, instruction)?;
    dst.push(interpreter.into_state());
    dst.extend(extra);

and `resolve_maybe_bool` is replaced by a `partition_maybe_bool` that
filters the state twice - definite lanes carrying a real `Bool` vector,
straddling lanes carrying an `UnknownBool` whose all-unknown invariant now
genuinely holds. Both filters already exist (`filter_by_kept_clone`,
`split_by_condition`); neither duplicates a lane, so the total lane count
is preserved exactly, which is the whole point.

### What still needs care

* The ambiguous state must inherit the local's `UnknownBool`, not the
  `MaybeBool` - the transient must still never be stored.
* An all-definite or all-ambiguous partition must produce NO spill, or
  every comparison doubles the state count for nothing.
* Lane order: the definite side keeps its order and the spill keeps its
  own, so the boundary merge sees two states rather than one reordered
  one. That is the same situation any branch split already creates.

## The obstacle, which is architectural rather than semantic

`interpret_binary_op` returns a `Value`, not states - which is exactly why
`MaybeBool` was designed as a transient resolved at the assignment choke
point. Partitioning needs an INSTRUCTION to yield two states into the
flow. Branches can already do that; instructions cannot. That is the work,
and it should be scoped before anything else here is touched.

Note the pleasant consequence: `MaybeBool` and its guards already exist
and are already tested. The transient is the right carrier; only its
RESOLUTION changes, from duplicate-the-lane to split-the-state.

## Result (2026-08-09): built, correct, and very nearly pointless

`partition_maybe_bool` is implemented and wired through the spill return
described above. It behaves exactly as designed. It does not pay.

Room (1,0), 45 frames: **byte-identical** with the partition on and off.
That is the predicted control - room (1,0) produces zero mixed
comparisons, so the partition must be a no-op there, and it is.

Room (0,0), 70 frames, interleaved A/B, 8 threads:

| | partition OFF | partition ON |
|---|---|---|
| time | 277.07 s | 272.77 s |
| lanes | 8,164,454 | 8,121,405 |
| fragments | 100,272 | 100,220 |
| deopt lanes | 66,872 | 65,960 |
| peak RSS | 19.42 GB | 20.32 GB |

The **cost** side is the good news, and it settles the question this plan
was written to answer: fragments moved by 0.05%. The feared multiplicative
re-splitting - each partition being cut again by the next comparison -
does not happen. Lanes even fell 0.5%, which is what a strictly more
precise abstraction should do.

The **benefit** side did not materialise. The whole point was to stop
dropping states onto the plain program, and the deopt fell only 1.4%.

### Why, and it is not a bug

The per-frame collapse census at f070 (partition on):

    78,184 constructions where some lane is unknown
    13,109 MIXED (16.8%)  <- all the partition can touch
    65,075 ALL-unknown (83.2%)

An all-unknown comparison collapses to `UnknownBool` no matter what,
because the whole-value tag is *honest* there - no lane has an answer to
preserve. Partitioning is defined to leave that case alone.

And the surviving deopt is exactly that case. Every deopt at f070 is the
same site, the strawberry's collide test:

    %122 = %110 > %121                  <- comparison collapses
    %125 = select %93 ? %122 : %93      <- and: Bool ? UnknownBool -> UnknownBool
    %157 = select %125 ? %154 : %125    <- select on UnknownBool: whole-state fallback

So the partition is aimed at 17% of collapses while the deopt is driven by
the other 83%. The earlier "70.1% of constructions are mixed" figure that
motivated this plan was measured over a different frame range and does not
hold at the depth where the deopt actually costs anything.

### What would actually remove the deopt

Not this. The lever is the *consumer*: make `select` on a whole-value
`UnknownBool` SPLIT THE STATE (one copy with the condition true, one
false) instead of erroring. That is sound for an all-unknown condition -
it is precisely what `branch` on `UnknownBool` already does - and it keeps
the rest of the frame on the fast path instead of re-running the whole
frame under the plain program. The spill plumbing this plan added is
exactly the mechanism it needs, so the work here is not wasted; it is the
first half.

That is task #96, and it should not be started until the pending
measurement says what the deopt actually costs in TIME. 131,920 deopted
lanes out of 8.1M is 1.6% of lanes, but a deopted lane costs far more than
a normal one, so the time share could be anywhere from 2% to 40%. A
`record_deopt_nanos` counter now reports it per frame. Build nothing here
until that number exists.

### Disposition

Kept, env-gated `CELESTE_PARTITION_STRADDLES`, default OFF - it is neutral
on time and costs 4.6% peak RSS, and memory is the binding constraint. It
stays because it is the prerequisite for #96 and because it is the only
known fix for room (0,0)'s batch-dependence (whether a collapse fires
depends on whether ANY lane in the state straddles, so it changes with the
chunk size; room (1,0) never collapses, which is why it certifies as
chunk-invariant and room (0,0) does not).

### One cheaper alternative, already disproved

Before building the split, the obvious cheaper move is to propagate the
tri-state THROUGH `select` rather than collapsing: in

    %125 = select %93 ? %122 : %93

the lanes where `%93` is false give `%125 = false` definitely, whatever
`%122` is, so this could yield a `MaybeBool` that the partition then
handles - no state split at all.

It does not apply here. If `%93` had any false lane, `select` would have
failed at `%125` with "cannot combine UnknownBool and Bool per lane", and
the observed deopt is at `%157`. So `%93` is uniformly TRUE in exactly
these states, `select` returns the `if_true` arm verbatim, and `%125`
inherits `%122`'s `UnknownBool` unchanged. There are no definite lanes to
salvage; the information is genuinely absent.

That leaves the state split as the only lever, and it is worth being
clear-eyed about its cost: `obj.collide` is a four-sided box test, so a
chain of all-unknown conditions can split 2^4. The reason it is still
expected to win is that the PLAIN PROGRAM ALREADY PAYS EXACTLY THAT - a
branch on `UnknownBool` sends the whole state down both edges - and pays
`to_canonical`/`from_canonical` and an unoptimized run of the WHOLE frame
on top. The split's win is confined to keeping the rest of the frame on
the fast path, which is why its size depends entirely on what share of
frame time the deopt is, and why that measurement gates the work.

## Decision (2026-08-09, Philippe): lane independence is not negotiable

> "I want the unknown bool lanes being independent thing either way,
> whether or not performance. I don't want lanes to be dependent on each
> other. I don't want the results to change depending on what is in which
> other lane."

So the partition is now ON by default (opt out with
CELESTE_NO_PARTITION_STRADDLES, for A/B measurement only), despite costing
4.6% peak RSS for a 1.4% deopt reduction. The property being bought is
that batching is a pure implementation detail: a lane's result must not
depend on which other lanes share its state, or the chunk size becomes
part of the abstraction and every result is contingent on it.
`rewrite simdcheck` is the acceptance gate for that claim and should be
run on room (0,0), which is the room that can violate it.

## The select split

Also landed, same session, on the same decision: `select` on a whole-value
`UnknownBool` now SPLITS the state - a true copy and a false copy - rather
than erroring into the whole-state plain-program fallback.

Soundness: the condition is unknown for EVERY lane (an honest
`UnknownBool` post-partition), so the two copies cover every possibility
between them. It is precisely what `branch` on `UnknownBool` already does,
and what the plain fallback was doing by a much more expensive route -
which yields a testable prediction: the split should produce results
IDENTICAL to the deopt, not merely sound. Diverging lane counts in the A/B
would mean one of the two paths is wrong and must be understood before the
split is trusted.

The condition local is overwritten in each copy, not just the arms picked,
so that a later instruction reading the same condition sees the branch its
copy represents. Two copies that disagreed about the same fact would be
unsound in a way no lane count would reveal.

Cost: unlike the partition, the split COPIES lanes rather than moving
them, so lanes double at each firing, and `obj.collide` is a four-sided
test. The guard against runaway is that the plain program already paid
exactly this doubling via branches; `record_select_split` counts firings
per frame so it can be watched against the fragment count.

Why it was worth building, measured on room (0,0) before the change:

    frame   deopt CPU   % of frame wall   deopt lanes
    f066      0.75 s          4.6%              652
    f067      2.57 s         13.9%            2,278
    f068      6.68 s         28.3%            5,729
    f069     14.97 s         54.6%           17,343
    f070     27.15 s         98.0%           40,870

Deopted lanes were 0.5% of all lanes but >=12% of frame CPU, i.e. a
deopted lane cost >=25x a normal one, and they grew ~2.4x per frame while
total lanes grew ~2%. Frames 66-70 alone were 52 s of a 234 s run. The
lane count alone said 1.6% and would have closed this as not worth doing;
the timer is what showed otherwise, which is the general lesson.

NOTE on the "% of frame wall" column: it is CPU summed over 8 worker
threads against WALL time, so it overstates the recoverable share. The
true wall saving is bounded below by ~12% (deopt perfectly spread across
workers) and above by the printed figure (deopt entirely on the critical
path). The A/B of the split against the deopt is the number that settles
it, and is what should be quoted.

### Correction to the deopt-lane figures above

An earlier revision of this file, and the commit message for the partition,
quoted the deopt as 133,744 -> 131,920 lanes. Those are exactly DOUBLE the
real numbers, which the run summaries give as **66,872 -> 65,960**. The
error was mine in transcription, not in the runs. The ratio is unaffected
(-1.4% either way) and no conclusion changes, but the absolute figures
were wrong and are corrected here.

## Result: the deopt is gone at f70

The condition split and the UnknownBool-arm rule together, room (0,0) to
f70, 8 threads, campaign chunk settings (8000/8000), 486->488 tests:

                 baseline    +split     +arm rule
  time           243.86 s   239.44 s     218.68 s
  peak RSS        21.28 GB   16.99 GB     13.63 GB
  deopt      12st/65,960  7st/47,000            0
  fragments       100,220    100,768      102,694
  lanes         8,121,405  8,115,505    8,062,451

Time -10%, peak memory -36%, and no whole-state fallback survives.
Fragments rise 2.5%, so the splits do not multiply - the failure mode
that took the original duplicate-the-lane design to 102 GB at f66.

Split firings per frame: 96 / 240 / 432 / 828 / 1848.

THE TWO RULES ONLY PAY OFF TOGETHER. Frame 68 recorded zero splits in the
split-only run, because `select cannot combine UnknownBool and Bool`
fired first and dropped the whole state before any split could happen.
Fixing only the condition moves the failure one instruction down the
chain; fixing only the arm leaves the condition failing above it.

### Not yet certified

Lanes fell 0.7%. Fewer lanes is EITHER more precision (the split keeps
the rewritten program's precision, where the deopt round-trips through
`to_canonical`, which widens) OR dropped reachable states, which would be
unsound. A lane count cannot tell those apart, and the pleasant reading
must not be assumed.

`trace-witness` is the gate: it replays the concrete reference TAS
(tas/room_0_0_exit_frame_94.txt) under the PLAIN program and probes each
frame's abstract row table, so a MISS is the exact address of a leak.
`g.bin` is optional there - without it the probe checks table membership
and `e`, which is exactly the question - so no sweep is needed and the
gate is cheap.

Until it passes, none of the numbers above should be quoted.

### Next, if it passes

Re-measure at f94, where the deopt was 81.7M lanes of a 4502 s run. That
is where the payoff should be largest, and it is the figure the room (0,0)
end-to-end estimate (13 +/- 2 h) is built on.

### CERTIFIED: the lane reduction is precision, not loss

`trace-witness` on room (0,0), horizon 70, level 0 (115,656,896 rows):

    f010 (  8, 93):  k0:ok(e=10)
    f020 (  8, 93):  k0:ok(e=20)
    f030 ( 10, 96):  k0:ok(e=30)
    f040 ( 33, 88):  k0:ok(e=40)
    f050 ( 60, 80):  k0:ok(e=49)
    f060 ( 88, 71):  k0:ok(e=59)
    f068 ( 81, 52):  k0:ok(e=66)
    f069 ( 79, 50):  k0:ok(e=67)
    f070 ( 77, 48):  k0:ok(e=68)
    witness trace PASSES every probed level's band at every frame

The concrete winning path is present in the abstract row table at every
frame, INCLUDING the straddling frames f066-f070 where the splits fire.
So the 0.7% lane reduction is added precision - the split keeps the
rewritten program's precision where the deopt round-tripped through
`to_canonical`, which widens - and not dropped states. The f70 numbers
above can be quoted.

Scope of the claim, stated honestly: this is ONE trajectory, so it is the
strongest certificate this project has rather than a proof. A soundness
bug that spares the reference TAS would survive it. The complementary
gate is `simdcheck`, which is about lane independence rather than
containment, and the checkpoint run reproduced 8,062,451 lanes exactly,
so the pipeline is at least deterministic.
