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

## Two tasks, and this doc is about the first

**A - THE KERNEL (inner).** Given one slice of exactly 16 lanes, append
its distinct successors to the output arrays. This is generated code,
per heap shape, and it is where the interesting problem is.

**B - THE LOOP (outer).** Drive A over a block, merge the per-shape
outputs, canonicalize, dedup across slices, feed the result to the next
frame. The baseline implementation just calls A in a loop over all
lanes, 16 at a time, PADDING the tail - and padding is free: replicate
any real lane, and the rows it produces are duplicates of that lane's,
which A removes by its own postcondition.

### Task A, precisely

    step(block, lo, out) -> declined_mask

Always exactly 16 lanes - B pads the tail, so A never sees a partial
slice.

* **Input.** `block` is column-major (`Rt2`): one array per heap cell.
  `lo` selects EXACTLY 16 lanes. Every lane is a game state of one known
  heap shape - the kernel is generated for that shape and refuses any
  other.
* **Output.** Append rows to `out[i]`, one array per OUTCOME - per heap
  shape a frame can end in (a frame that kills an object ends in a
  different shape than one that does not). Appending means pushing one
  value per cell onto that outcome's columns.
* **What must be appended.** For each of the 16 input states, every
  distinct successor it has. A successor exists per free choice the
  search does not fix: the 6 button bits, and the <=2-way splits of the
  widened `player.rem` where `flr` is ambiguous.
* **Postcondition.** Ideally no two appended rows are equal, where
  "equal" means what the boundary means: same outcome, equal values in
  every cell. This is a TARGET, not a hard requirement - the boundary
  dedups anyway, so letting a duplicate through is slow, not wrong. See
  "when is removing a duplicate worth it" below.
* **Self-contained.** One call sees one slice and nothing else. No
  state carried between calls, deliberately: cross-slice duplicates are
  B's problem, and statelessness keeps A simple while it is still slow.
* **Scratch is fine.** A may use private scratch memory; the caller does
  not need to know. It is handed arrays and must append to them.
* **Row order does not matter.**
* **May assume.** All 16 lanes have the same heap shape. Values are
  Pico-8 fixed point, booleans (tri-state), or intervals.
* **Must not.** Drop a successor. A lane the kernel cannot handle is
  reported in `declined_mask`, which stops the run - there is no
  interpreter to fall back to.

Duplicates BETWEEN slices are task B's problem, not A's.

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
   frame by row-key set equality, not by count. A missing successor is a
   wrong answer, not a slow one.
2. **"Same row" must mean what the boundary means.** The kernel may
   decide two rows are duplicates only if the boundary would too -
   equal values, cell by cell, after the widenings (which the kernel
   applies itself). Deciding it on less risks dropping a real
   successor; deciding it on more just leaves work for the boundary.
3. **Never deopt.** A lane the kernel cannot handle stops the run with a
   reason. There is no interpreter fallback (CLAUDE.md).
4. **16 lanes, `u16` masks.** Liveness, validity and deopt are all
   16-bit masks.
5. **It has to compile.** One frame is ~6,000 graph nodes in one
   function, and the generated code is already ~10k lines per shape.
   Solutions that multiply the emitted code have a real cost - see the
   two build blow-ups under "what has been tried".

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
  group's take masks. Correct, but it made the build 70 s -> 20+
  minutes: the union references every variant's masks, so 48 values that
  used to die immediately stayed live across the whole function and the
  register allocator drowned. REVERTED. (The same thing happened when
  LLVM could see the fork loop's trip count and unrolled a 10k-line body
  four times; `black_box` on the bound fixed that one.) Both are the
  same lesson: in a function this size, anything that reaches backwards
  across the variant sequence is expensive.

## When is removing a duplicate worth it

Philippe's rule: if dropping a duplicate inside the kernel saves more
than it costs the outer system to drop it, do it in the kernel.

Measured, from the runs with and without the kernel's dedup:

    downstream cost of one written row   ~120 ns
        (merge + boundary, 149 ms / 1.25M rows before dedup,
         25 ms / 197k rows after - consistent)

    kernel cost of one dedup CHECK        ~62 ns
        (kernel phase 252 -> 329 ms over 1.25M candidates)

    duplicate rate                         84%
        (1.25M candidates -> 197k written)

So the budget for a check is `120 ns x 0.84 = ~100 ns`, and the current
check costs 62 ns. That predicts a saving of
`1.25M x (100 - 62) ns = ~48 ms`; the observed total went 401 -> 356 ms,
a saving of 45 ms. The model holds.

Which says the current dedup is worth keeping and NOT worth tuning: even
a free check would only buy the remaining 62 ns x 1.25M = 78 ms. The
prize is not a cheaper check - it is not generating the duplicate
candidates in the first place.

## The bar

As fast as possible. Beating the interpreter (230 ms for these 30
frames) is the floor, not the goal - "we're very far away from as fast
as possible, so measure and iterate". Simplicity counts too: the current
state is complicated AND slow, which is the worst quadrant to linger in.

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

### 1. Group-driven emission

What the kernel emits today, inside the fork loops, per configuration:

    let sh0 = KShared0 { /* cells shared by all assignments */ };
    // then 24 times, once per distinct button assignment:
    let o0 = KOut0 { live: live_v0, deopt: !ok_v0, /* per-variant cells */ };
    let o1 = KOut1 { ... }; let o2 = ...; let o3 = ...;
    out(0b000000, &KOuts { sh0: &sh0, v0: &o0, ... });

24 `out()` calls, each carrying all four outcomes. Outcome 0 has ZERO
per-variant cells, so all 24 `KOut0`s are identical but for `live` and
`deopt` - the driver appends its row 24 times per lane and the runtime
dedup discards 23.

Group-driven: group variants PER OUTCOME by their per-variant cell
expressions, and emit one append per group with the union of that
group's lanes.

    let mut take0: u16 = 0;
    // ... variant 0's masks computed here
    take0 |= live_v0 & ok_v0;
    // ... variant 1's masks
    take0 |= live_v1 & ok_v1;
    //  ... 24 times
    append0(acc, &sh0, take0);        // ONE append for outcome 0

The duplicate rows are never produced, so never checked either.

**Why this is not the thing that blew up the build.** The failed attempt
built the union as ONE expression - `(live_v0 & ok_v0) | (live_v1 &
ok_v1) | ...` - which needs all 48 masks alive at once. Accumulating
incrementally keeps one `u16` alive and lets each variant's masks die
immediately. Same result, opposite register pressure.

**Expected size.** Groups per outcome measure `[1, 19, 24, 24]`, so 96
appends per fork configuration become 68: ~29% fewer candidates, hence
~29% off both the check and the append. Kernel ~329 -> ~270 ms, total
~356 -> ~300 ms. Real and simpler, but ~15%, not a breakthrough.
### 2. A cheaper dynamic check

Over only the distinguishing cells, vectorized across 16 lanes rather
than scalar per lane. Bounded by the economics above: even a FREE check
only buys 78 ms of 356.
### 3. Neither

Accept 1.55x and spend the time on coverage, since deleting the
interpreter is blocked on the kernels covering more than one room and
one ladder rung - not on speed.
