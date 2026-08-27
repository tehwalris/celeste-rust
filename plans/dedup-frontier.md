# Dedup frontier redesign (2026-08-27)

The forward search's runtime is dominated by MATERIALIZING rows, ~80% of
which are duplicates it discards. Two design directions, agreed
interactively with Philippe, attack that. This file is the spec; the
load-bearing property to VERIFY before implementing is called out.

## What the profile actually showed (room10, compiled forward, frontier-only)

Per-frame `CELESTE_DEDUP_CENSUS` at ~f50:
- offered 9.46M rows -> ~1.97M distinct-in-frame -> 1.19M genuinely new.
- **within-frame (cross-chunk) 4.8:1**, **cross-frame 1.7:1**, total ~7.9:1.
- Of every duplicate killed, **~90% is a hit against a row emitted earlier
  THIS frame** (another chunk), only ~10% against prior-frame history.
- Global frontier probe is ~0 ns/row in-memory (8M keys); the cost is
  `hash 8.5 ns/row` + MATERIALIZATION, not the probe. Probe only bites at
  billions of keys on mmap.
- Chunk phases: run (kernel+append) 72.8%, dedup+merge 23.6%.
- (Already landed: chunk-wide RowSet made WITHIN-CHUNK dedup on-the-fly,
  2.7:1, kernel phase ~2.24x. The 4.8:1 above is what's LEFT, cross-chunk.)

The dedup is three-level: L1 within-chunk (done), L2 within-frame
(cross-chunk, currently materialize-then-subtract), L3 cross-frame global
frontier. A within-frame dup IS a frontier hit because the frontier is
updated incrementally, so the sets overlap; the 90/10 split is just "was
the first occurrence this frame or earlier."

## Design 1: kill the canonical key; key by (shape, kernel-key)

Today the boundary CANONICALIZES each output (GC + renumber) and computes
a shape-independent CANONICAL key that the frontier dedups on. Instead:

- The kernel EMITS CANONICAL shapes directly (normalization happens in the
  kernel, not a post-pass).
- The frontier is ONE hash set PER SHAPE, keyed by the kernel's own key.
- The full row identity is `(shape_key, kernel_key)` concatenated, but the
  shape half is IMPLICIT - it's which per-shape set you look in.

Drop the separate canonical key entirely.

**Soundness** (safe by construction): keying by (shape, kernel-key) can
never LOSE a state. Its only failure mode is the opposite of dangerous -
two representations of the same semantic state fail to merge, so both are
kept: frontier BLOAT and wasted work, never a missed state. Canonical
emission is exactly what removes that bloat.

**Benefits**: per-shape sets are smaller + homogeneous (better cache
locality, per-shape layout), the boundary's canonicalize+key pass is
deleted (part of the 23.6% dedup+merge), lookups compare fixed-width
kernel keys within a shape with no structure walk.

**LOAD-BEARING PROPERTY TO VERIFY FIRST**: emission must be
PATH-INDEPENDENT. Two input chunks that converge on the same semantic
successor - even from different input shapes - must emit a byte-identical
output shape + kernel key. Identity must be a function of WHERE YOU ARE,
not HOW YOU GOT THERE. If any representation detail depends on the input
rather than the resulting state, those occurrences won't share a key and
the frontier re-inflates (sound, but defeats the point). MEASURE THIS
before committing.

**Not dropped**: the WIDENING / precision part of the boundary (rem
intervals) is separate from the canonical key and stays (level-0 kernels
already emit pre-widened). Do not conflate.

**Validation consequence**: the gate today compares canonical row-key
SETS against the interpreter. It would need to compare on (shape,
kernel-key), i.e. teach the oracle side the same key. Testing rework, not
a fundamental blocker.

## Design 2: check the frontier BEFORE materializing (write-buffer / LSM)

Today dedup runs AFTER the kernel materializes the row and the boundary
canonicalizes it. Move the check before the append:

- **Freeze the frontier for the whole frame** - read-only, no inserts
  mid-frame.
- Per row: look up the frontier (read). Hit -> discard (cross-frame dup).
  Miss -> look up the small WITHIN-FRAME buffer. Hit -> discard
  (within-frame dup). Miss -> materialize + insert into the buffer.
- **At frame end, bulk-flush the buffer into the frontier** in one merge.

**Why the freeze is the win, not just cheaper inserts**:
1. A read-only frontier is LOCK-FREE - all 16 threads probe it with zero
   coordination. Today the insert side is "the one piece of shared
   mutable state," serialized for deterministic ids; that serialization
   is the bottleneck. This is what makes check-before-materialize
   affordable at scale.
2. A stable structure is READ-OPTIMIZABLE - prefetch, immutable sorted
   runs, a bloom filter in front. Can't do that to a set mutating under
   you. (The frontier is big but slow-changing - ideal.)
3. Bulk flush beats millions of incremental inserts, especially for the
   sorted/mmap structure where incremental inserts spawn many small runs
   that slow every later lookup.

**The within-frame buffer is load-bearing here** (unlike a "check global
first, always update" scheme where it's redundant): because the frontier
is frozen, a within-frame cross-chunk dup MISSES the frontier (first
occurrence not flushed yet), so the buffer is exactly what catches it.
Both levels earn their place.

**Depends on Design 1**: to check before materializing you need the row's
key up front, which is the kernel key - so canonical emission is what
makes this possible without the boundary.

**Determinism constraint**: row ids are assigned in INPUT ORDER for
byte-identical checkpoints. The bulk flush must preserve that - sort the
buffer by input order before flushing. Mechanical, but required.

## Why this is worth it

The primary cost is materialization (73% kernel phase), ~80% of it
duplicates. Checking before the append kills that ~80% regardless of the
cache structure. The multi-level cache (buffer + frontier) is how you
keep the early check cheap once the frontier is DRAM/mmap-bound; per the
profile we are NOT there yet (probe ~free), so a single frozen frontier
checked early is most of the win today.

## Open items / order of work

1. VERIFY path-independence of emission (Design 1's load-bearing property)
   - does same-successor-from-different-inputs emit identical shape+key
   today? This gates everything.
2. Measure the frontier INSERT cost per frame today (buried in
   dedup+merge / the serial subtract) to size the write-buffer win. The
   probe is ~free; the insert is the unmeasured half.
3. Concrete implementation plan; prototype only reversible/verifiable
   pieces. Anything that changes the SEARCH IDENTITY (shape hash, row
   key, FIELD_NAMES order) is Philippe's call - surface it, do not commit
   it silently. CLAUDE.md: "A reordering is a different search."

---

# Findings (agent, 2026-08-27)

## 1. Path-independence verdict: SPLIT — shape YES, kernel key NO (UNSOUND)

Two separable claims live under "emission is path-independent":

**(a) The output SHAPE is path-independent — YES, by construction and
empirically.** Each kernel's `OUT_SHAPE_i` is `bind::structure_of(output_state)`
(`trace::verify.rs:229`), which is (i) proven CANONICAL by the existing test
`the_structure_a_traced_state_becomes_is_already_canonical` — canonicalizing it
is the identity — and (ii) a PURE FUNCTION OF THE STATE, not the input. So two
input-shape kernels that converge on the same semantic successor bake a
byte-identical `OUT_SHAPE`+`OUT_GLOBALS`. Confirmed empirically:
`tools/shape_share.py` shows output-shape sharing is the NORM, not the
exception — room20 has one output structure produced by 7 different
input-shape kernels; every room shares nearly all output structures across
kernels. And the D1 key-gate (`native-probe --key-gate`, BENCHMARK_DATA "D1
GATED") already proves the engine's BOUNDARY key (all cells, canonical) ==
the interpreter's canonical key, bijection on 55.9M rows.

**(b) The KERNEL KEY that Design 1 proposes to dedup on is NOT
path-independent, and keying the frontier by it is UNSOUND.** The kernel's
own row key (`KOut{h1,h2}`, the `RowSet` key) hashes only the NON-CONSTANT
output cells — the emitter DROPS every cell that is "agreed across button
variants AND compile-time Const" (`transpile/lower.rs:1020-1046`). Which cells
are const is a property of the INPUT shape's frame dynamics, NOT of the output
shape. So for a shared output shape, different kernels drop DIFFERENT cells,
and worse, bake DIFFERENT CONSTANT VALUES for a dropped cell.

Concrete proof (`tools/key_unsound.py`, 7 such (cell, shared-shape) instances
in the checked-in kernels). Cleanest: output structure `c1684e2288f8` (room00)
is produced by kernel0#0, kernel1#1, kernel2#3. Cell 159:
- kernel0 `acc0`: `cols[159] = Col::U(AV::Num(0))`  (const, dropped from key)
- kernel1 `acc1`: `cols[159] = Col::U(AV::Num(65536))` (const, dropped from key)
- kernel2#3: cell 159 is PER-LANE (in the key)

Two reachable states in shape `c1684e2288f8` that differ ONLY in cell 159
(one =0 from kernel0, one =65536 from kernel1) compute the SAME kernel key
(159 excluded in both) and would COLLIDE in a shared per-shape frontier —
one is dropped — a LOST REACHABLE STATE. The current boundary key (all cells)
distinguishes them (0 vs 65536 hash differently), which is why the search is
sound today.

**Consequence for Design 1 as written ("key by the kernel's own key"): not
viable — it is UNSOUND, not merely re-inflating.** The mandate's rule applies:
adapt or scope down, never ship a silent mis-merge.

### The sound adaptation (what "canonical emission" must actually mean)

The kernel key must be a pure function of `(shape, full output values)` — i.e.
it must hash ALL output cells in canonical cell order, matching the boundary
key's partition. The dropped-const-cells optimization is what breaks it. The
fix that keeps it CHEAP: the const cells are compile-time constants per
kernel-outcome, so the emitter can FOLD them into the mix chain at generation
time (constant-folded by rustc) and mix the per-lane cells at runtime exactly
as now. Result: a key equal in PARTITION to the boundary/canonical key
(hence path-independent AND sound, equal visited set), at ~the same runtime
cost (same number of per-lane mixes; the const folds are free). This is the
only sound way to key a per-shape frontier by a kernel-computed key. It is
what Design 1 must become. It changes the numeric row keys (checkpoint
compatibility) but NOT the search result / visited set — see Assumptions.

## Assumptions & decisions (running log for evening review)

- **A1 (verified).** Output shape emission is path-independent. Gate:
  `the_structure_a_traced_state_becomes_is_already_canonical` (existing) +
  `native-probe --key-gate` D1 bijection + `tools/shape_share.py` (shows the
  property is heavily EXERCISED: shapes shared by up to 7 kernels).
- **A2 (verified, NEGATIVE).** The kernel's own row key (drops agreed+const
  cells) is NOT a sound frontier key across kernels. Gate:
  `tools/key_unsound.py` finds 7 concrete (cell, shared-shape) collisions;
  hand-confirmed cell 159 of room00 out `c1684e2288f8` (kernel0 bakes 0,
  kernel1 bakes 65536, both drop it). DECISION: do NOT key the frontier by
  the current kernel key. Design 1 must become "kernel emits the FULL
  canonical key (all cells, const-folded at compile time)". This makes the
  key a pure function of (shape, values) = the boundary key's partition.
- **A3 (decision).** The flushing prototype (Design 2 reversible subset)
  keeps the CURRENT canonical/boundary key and does NOT touch the kernel
  key. It only defers frontier inserts to an end-of-frame bulk flush and
  freezes the frontier mid-frame. This is search-identity-preserving and
  gated byte-identical. [status below]
