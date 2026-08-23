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

## Where the time actually goes (measured 2026-08-23)

`perf record` on the 30-frame room test, release build, group-driven
emission in place. Phase timing: kernel 327 ms, merge 2 ms, boundary
24 ms; interpreter oracle 230 ms on the same data.

Symbol breakdown *within* the kernel phase:

| symbol | % of whole profile |
|---|---|
| `k1::append3` | 2.49 |
| `k1::frame` (the entire arithmetic graph) | 0.82 |
| `k1::append0/1/2` | 0.02 each |

So **75% of kernel time is one append, and the arithmetic graph is the
other 25%.** Every earlier plan aimed at the graph. The graph is not the
problem.

### It is the hash, not the stores

`perf annotate` on `append3` shows the heat spread uniformly across a
straight-line block of `imul`/`shr`/`xor` - the row hash. Instruction mix
of the function body: 192 `imul`, 213 `shr`, 271 `xor`, 674 `mov`, 2008
instructions total for ONE row.

The hash is 29 cells x 2 accumulators, written as

```rust
h1 = mix64(h1 ^ mix64(v ^ c));            // NESTED - order-dependent
h2 = h2.wrapping_add(mix64(v.wrapping_mul((c << 1) | 1)));
```

`h1`'s form is a serial dependency chain: ~190 `imul`s at 3-cycle latency
that cannot overlap. ~500 cycles per candidate row.

### The cost model, confirmed two ways

- 82 candidate rows appended per input lane (lane census, frame 30),
  26,504 input lanes over 30 frames -> ~2.17M candidate rows hashed.
- 2.17M x ~500 cycles = 1.09e9 cycles = ~270 ms at 4 GHz.
- `perf` attributes 2.49% of 43.9e9 cycles = 1.09e9 cycles to `append3`.

Same number from the static instruction count and from the sampler. Also
note 87% of those 2.17M rows are duplicates thrown away *after* paying the
full hash.

### Three fixes, in size order

1. **Commutative fold.** Of the 29 hashed cells in `append3`, 13 come
   from `KShared` - identical across all 24 button assignments - and 16
   from `KOut`. The nested `h1` form forces all 29 to be recomputed for
   each of the ~82 candidates. If both accumulators combine
   commutatively (xor/add of independently mixed terms), the 13 shared
   cells are hashed ONCE per lane and reused. ~45% off the hash. Safe:
   each term already mixes the cell index, so position stays encoded.
2. **Vectorise across lanes.** The hash is scalar, one lane at a time,
   and the 16 lanes are independent. Because this is a latency chain and
   not a throughput limit, SIMD across lanes is close to free.
3. **Cheaper primitive.** `_mm_crc32_u64` (3-cycle latency, 1/cycle
   throughput) instead of two 64-bit multiplies per cell.

### What did NOT help

Group-driven emission (96 sink calls -> 68) is correct - 30 frames
row-key identical - and worth **nothing**: 353 ms vs 356 ms, inside the
noise. The sink call count was never the cost. Keep it for the smaller
generated code, not for speed.

## The fix: fold the row key IN THE GRAPH (2026-08-23)

Philippe's call, and it subsumes both hand-rolled fixes above.

The insight I had missed: the fold being sequential over CELLS is not the
problem. The 16 LANES are the axis the parallelism lives on, and a graph
node is already 16 lanes wide. So the chain stays exactly as long while
every link does sixteen rows at once - and hash-consing shares the
button-independent PREFIX across all 24 assignments for free, which is
fix 1 without having to weaken the hash to a commutative combine. The
hash function itself is unchanged.

Verified before building it: `mix64` written as plain Rust over a
`[u64; 16]`, compiled with the `target-cpu=native` the repo already
passes, emits 8 `vpmullq` and ZERO scalar `imul`. AVX-512 with `avx512dq`
is present on this machine. No intrinsics needed.

### What it added

- `celeste-engine`: `ZW = [u64; W]` plus `zw_splat`, `zw_bits_{n,i,b}`,
  `zw_mix{1,2}`. The only lane type here that is not an abstract game
  value.
- `transpile::graph`: `Op::Word(u64)` (the seed), `Op::Bits` (the
  representation bits of one value), `Op::Mix(cell, half)` (one fold
  step). A separated layer: it reads the value layer and nothing feeds
  back, so the value layer is still exactly what the interpreter checks.
- `transpile::lower`: `Dom::Word`, its `Repr`/coercion/render arms, and
  the fold construction in `emit_body` - built in the SPECIALIZED arena,
  after `ival::fold`, with the agreed cells ordered FIRST so the shared
  part is a prefix.
- `Emit::row_key`, ON for the tracer and OFF for the walk. The walk's
  kernels have no write-time dedup, so a key there would be dead lets in
  a CHECKED-IN generated file. The checked-in kernels are byte-identical.
- `trace::kernel`: `KOut{i}` carries `h1`/`h2`; `append{i}` does one
  table probe and no mixing at all.

### Sharing, measured on the emitted code

| | mix1 steps |
|---|---|
| no sharing (29 cells x 24 variants, 4 outcomes) | 1032 |
| perfect prefix sharing (predicted) | 526 |
| actually emitted | **506** |

Generated code grew ~14% in lines (3 shapes: 21,743 -> 24,559).

### What is NOT in the graph, on purpose

The `RowSet` probe. A hash table is sequential and scalar; only the
mixing moved. So the ceiling on this change is removing the ~270 ms of
mixing, not removing `append`.

## Build time: measured, and it is 90 s (2026-08-23)

I claimed the room-test loop was ~25 minutes and used that to argue
against emitting more code. Measured in isolation, with nothing else
holding the build lock: **1:29.68 wall**, touching `src/trace/kernel.rs`.
Per unit, from `cargo build --timings`:

| unit | s |
|---|---|
| `traced-kernel-check` lib (the ~24k generated lines) | 84.0 |
| `traced-kernel-check` lib (test) | 30.1 |
| `celeste-rust` | 22.4 |
| everything else | < 2 |

(Those overlap; 90 s is the wall clock.)

The 25-minute figure was my own cargo contention - the exact failure
CLAUDE.md documents, where a 70-second build queued behind another one
looks like a hung build. `./one-cargo.sh` exists to prevent it and I was
not using it. **Route every cargo invocation through it.**

Two consequences:

- The build-time objection to emitting the row-key fold was mispriced.
  There is a lot of headroom at 90 s.
- Dropping the interpreter oracle out of `traced-kernel-check` would
  save 22 s of 90, not the bulk. Not worth doing yet. The generated
  kernel code itself is the cost, and that is the code we want.

### Result (2026-08-23)

| | before | after |
|---|---|---|
| kernel | 327 ms | **154 ms** |
| merge | 2.0 ms | 2.0 ms |
| boundary | 23.9 ms | 23.9 ms |
| **total** | **356 ms** | **182 ms** |
| interpreter, same data | 230 ms | 226 ms |
| ratio | 0.65x (slower) | **1.24x (faster)** |

Both checks green: the per-frame graph check (every node against the
emitted Rust on the same inputs) and the 30-frame room run (row-key
identical to the interpreter on every frame). Merge and boundary did not
move, which is the control - only the fold was touched.

**The kernels beat the interpreter for the first time.**

Honest about the size of it: I predicted ~16x on the fold (2x sharing x
8x SIMD) and got ~3.5x. The scalar version only paid for lanes it
actually wrote; the vector version computes the key for all 16 lanes and
all 24 assignments whether or not those rows are taken. Density won
anyway, but that is where the remaining headroom is.
