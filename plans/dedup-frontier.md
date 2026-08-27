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

## 2. Insert-cost measurement (frontier-only, compiled forward, room10)

Command: `CELESTE_COMPILED_FORWARD=1 CELESTE_FRONTIER_ONLY=1 CELESTE_DEDUP_CENSUS=1
CELESTE_CHUNK_PHASE_TIME=1 ./target/release/rewrite bench --frames 50`
(in-memory map frontier, 16 threads, 8000-lane cap; release/fat-LTO).

Final: 50 frames, 5.01s wall, 8,033,952 rows visited (1,191,326 new at f50;
offered 9.46M -> 1.97M distinct -> 1.19M new — matches the spec profile).

Phase totals (WALL seconds over the whole 50-frame run):

| phase | wall | what |
|---|---|---|
| `fwd.interpret`        | 3.42s | kernel + materialize + boundary prepare + probe (PARALLEL, ~8x: 28.0 thread-s body) |
| `fwd.partition_filter` | 0.54s | within-frame dedup tier (parallel) |
| `fwd.merge`            | 0.49s | vectorize survivors |
| **`fwd.boundary_stream`** | **0.44s** | **the SERIAL insert side: `subtract_decide` -> `insert_new`** |
| `fwd.boundary_gather`  | 0.08s | gather survivors (parallel) |

**The INSERT half (the previously-unmeasured `fwd.boundary_stream`) is 0.44s
= 8.8% of wall, ~55 ns per newly-inserted row (0.44s / 8.03M rows).** It is
SERIAL (the RowTable is the one piece of shared mutable state; ids are
assigned in input order). The probe half is confirmed ~free (`filter+probe
0.0 ns/row`; hash is 8-45 ns/row and lives in the parallel prepare).

Sizing the write-buffer win: the ENTIRE serial insert is 0.44s / 8.8%. A
frozen-frontier + bulk-flush cannot beat that by more than 8.8% of wall on
THIS in-memory profile, and only the part that is genuinely
insert-bound (hashmap insert of new rows) is reclaimable — the candidate
iteration and KeptLanes build stay. The real prize of freezing is
architectural (lock-free probes + read-optimizable structure + enabling
check-before-materialize to kill the 73% materialization), NOT the ~9%
insert phase itself, which is small at DRAM scale and only bites on the
mmap engine at billions of keys. Numbers here are the in-memory floor.

## 3. Safe flushing prototype: buffered (frozen-frontier) MAP engine

Landed opt-in as `CELESTE_FRONTIER_BUFFERED=1` (default OFF). The map engine
now mirrors the mmap engine, which ALREADY ships this exact design: buffer a
frame's new rows in a within-frame set (`RowTable::pending`, the analogue of
`MmapVisited::batch_set`) and BULK-FLUSH them into `rows` at `end_frame`
(`end_frame_buffered`), so every mid-frame `id_of`/`contains_historic` probe
hits a FROZEN table (completed frames only). The within-frame cross-chunk
dedup is carried by the existing `partition_seen` tier (partitioned filter)
plus `pending` - exactly the "buffer is load-bearing under a frozen frontier"
point in Design 2.

Determinism: ids are assigned at `insert_new` time in discovery (input)
order, `pending` is flushed in that same order, so ids/watermarks/rowkeys are
byte-identical to the classic path. This is the SAME argument (and the same
artifacts) that already makes the map and mmap engines interchangeable, so
byte-identity is precedented, not new.

Gates: `buffered_matches_classic_ids_and_freezes_mid_frame` (unit),
frontier-only sequence identity vs golden, and `./parcheck.sh`. [RESULTS below]

## 4. Full design implementation plan (canonical emission + check-before-materialize)

The Step-1 verdict rewrites Design 1. The naive kernel key is UNSOUND across
kernels; the FIX is that the kernel must emit a key that is a pure function of
`(shape, full values)`. The boundary key ALREADY IS exactly that, and it is
even better than "canonical order" requires: it is a COMMUTATIVE SUM,
`part = shape_seed + Σ_cells cell_mix(cell_id, value)` (runtime2.rs:976-1015),
so order does not matter and const cells fold into `shape_seed` at compile
time. So "canonical emission" should mean **the kernel emits the BOUNDARY key
inline**, NOT a new key. This has a decisive safety property: the key stays
NUMERICALLY EQUAL to today's, so it is checkpoint-COMPATIBLE and is NOT a
search-identity change - it only moves where the key is computed.

Steps, each with its gate; identity-affecting ones flagged.

- **Step A - kernel emits the boundary key inline (SEARCH-IDENTITY-NEUTRAL).**
  Change the emitter (`transpile/lower.rs:1003-1046`) so the row-key nodes are
  `shape_seed + Σ over ALL output cells of cell_mix(cell_id, value)` instead of
  the current sequential `zw_mix` chain over only the non-const cells. Const
  cells fold into `shape_seed` at generation time (free at runtime); per-lane
  cells use the additive `cell_mix` primitive the boundary uses (add the
  `ZW`/vector form of `cell_mix` to `celeste-engine::kernel` if absent). Then
  regenerate all kernels.
  - GATE: the D1 key-gate upgrades from BIJECTION to EQUALITY
    (`native-probe --key-gate` / `--key-gate-outputs`: the r2i map is the
    identity). `traced_kernels_reproduce_the_interpreter` + room00/room20
    lattice differentials + `CELESTE_COMPILED_FORWARD=check` all green.
    `traced_kernels_are_current` + the `#[ignore]`d room00/room20 currency
    tests (touching the emitter) after `./regen-generated.sh`, READ THE DIFF.
  - NOT an identity change (keys are byte-identical numbers), so checkpoints
    stay valid. This is the safe foundation and should land first.

- **Step B - check the frozen frontier BEFORE materializing (perf; neutral).**
  With the boundary key available in-register in the kernel's `append`
  (Step A), probe the FROZEN frontier + within-frame buffer there and skip
  the column pushes on a hit. Requires plumbing a `&Visited` (frozen) and the
  within-frame buffer into `run_traced_kernel`/`append`. The frontier stays
  frozen for the whole frame (Step 3 already does this for the map engine);
  the buffer catches within-frame cross-chunk dups. Kills the ~73%
  materialization of duplicates.
  - GATE: byte-identical frontier sequence + `./parcheck.sh` + the
    differential. Row SET and ids unchanged (same key, same order); only
    WHEN materialization is skipped changes. Determinism preserved because
    survivorship/id order is still decided serially in input order.
  - NOT a search-identity change.

- **Step C - per-shape frontier sets (perf; OPTIONAL).** Split the one
  frontier into one set per shape_hash, keyed by the (shape-independent) value
  half. Pure locality optimization; the key already carries the shape, so this
  cannot change the partition. Defer until A+B are measured.

### PHILIPPE'S CALL - identity-changing variants (do NOT commit)

The plan above is deliberately identity-NEUTRAL (it emits the SAME boundary
key). The following would change the search identity and are OUT OF SCOPE for
autonomous work - flagged per CLAUDE.md "A reordering is a different search":

- Replacing the boundary key with any DIFFERENT key function (e.g. the current
  cheap sequential `zw_mix` over non-const cells, or a per-shape key that omits
  the shape mix) - changes the stored row keys, invalidates every checkpoint,
  band and sweep artifact, and (for the non-const-dropping variants) is the
  UNSOUND merge from Step 1. Do not.
- Changing `FIELD_NAMES` order, the shape hash, or which fields the key covers.
- Dropping the shape mix from the frontier key on the theory that per-shape
  sets make it redundant - it also guards against cross-shape hash collisions;
  removing it is a soundness argument Philippe must make.

### Step 3 RESULTS (measured 2026-08-27, release/fat-LTO, room10)

BYTE-IDENTICAL, four gates green:
- unit: `buffered_matches_classic_ids_and_freezes_mid_frame` PASS.
- frontier-only sequence (`--frames 50`) IDENTICAL to the classic golden
  (per-frame new-lanes + visited-total, all 50 frames).
- `./parcheck.sh 45 8000` under `CELESTE_FRONTIER_BUFFERED=1`: serial vs
  16-thread `states.bin` byte-identical, frontier identical every frame.
- buffered vs classic artifacts (frames 45): all 45 `.rowkeys` files +
  `states.bin` + `meta.json` BYTE-IDENTICAL.
- `cargo nextest run --cargo-profile quick`: 292 passed, 12 skipped.

PERF: a NET LOSS in-memory. Clean A/B, 2 runs each, quiet machine:
- classic (OFF): 5.00s, 5.01s; peak 1.22-1.23 GB.
- buffered (ON): 5.28s, 5.29s; peak 1.30 GB.  => **~+5.6% wall, +6% peak.**

Why: the in-memory insert was already cheap (~55 ns/row, 8.8% of wall); the
buffer adds a SECOND hashmap op (the `pending` dedup set) plus a bulk flush,
which the frozen frontier does not pay back on its own. `fwd.boundary_stream`
drops 0.44->0.36s only because the inserts MOVE to the (untimed) end-of-frame
flush - the work does not disappear.

CONCLUSION: the write-buffer / frozen frontier is NOT a standalone win at
DRAM scale - it is the verified, determinism-safe PREREQUISITE for Step B
(check-before-materialize), where the real prize (killing the ~73%
materialization of duplicates) lives. Kept OFF by default; the escape hatch
and byte-identity are what make Step B safe to build on. This matches the
spec's own prediction ("a single frozen frontier checked early is most of
the win today" - the frozen frontier only pays once the EARLY CHECK exists).

## Assumptions & decisions (continued)

- **A4 (measured).** The buffered map engine is byte-identical to classic
  (four gates above) and to the already-shipping mmap engine's design, so
  determinism holds under it. Gate: parcheck + artifact diff + unit test.
- **A5 (measured, decision).** Buffered-alone is ~+5.6% wall in-memory, so
  it stays OFF by default (`CELESTE_FRONTIER_BUFFERED`, opt-in). It is a
  stepping stone, not a shippable win on its own. DECISION: do not flip the
  default; land it as the frozen-frontier substrate for Step B.
- **A6 (judgment, scope).** The full design's Steps A/B/C (kernel emits the
  boundary key inline, check-before-materialize, per-shape sets) are a large,
  high-risk change to the emitter + all generated kernels + `run_frame_chunk`
  + the frontier structure. Completing AND gating them responsibly exceeds
  this session; per CLAUDE.md "correctness beats cleverness" and "don't leave
  half-done", they are LEFT AS A PLAN (Section 4) rather than shipped
  half-gated. The plan is written so the identity-NEUTRAL foundation (Step A:
  emit the SAME boundary key, upgrading the D1 bijection to equality) lands
  first and safely. Not started in code, deliberately.
