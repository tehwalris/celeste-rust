# Dedup roofline plan (2026-08-19)

The forward stage costs **3,175 ns per input lane** and **44% of that is
hashing rows and probing the visited set**. This plan is about that 44%,
and about the two things measured alongside it that make the compiled
engine worth less than it should be.

Written against the architecture Philippe sketched: a compiled SIMD
kernel that runs one W-wide batch of input rows against every input
combination with the identical code paths already merged at compile time
and the shared physics CSE'd across variants; a tiered dedup (core-local
L1-sized, spilling to L2/L3-sized, then coordinating across cores and to
memory); deopts pushed to a side array and processed in one later pass by
just calling the interpreter, never inline; and fewer plain-program
fallbacks because the shapes that fall back have compiled programs.

The gap between that and what exists is large but it is not uniform, and
the pieces are not equally ready. This file takes stock piece by piece,
then orders the work.

## The numbers this plan exists to move

Room (1,0), H=68, ladder level-0 environment, 16 threads, 50,976,062
input lanes, 68 frames. Full table in BENCHMARK_DATA.md.

| | interpreter | compiled |
|---|---|---|
| forward stage, per input lane | 3,181 ns | 3,095 ns |
| **row keys + visited probe** | **1,413 ns (44%)** | **1,163 ns (38%)** |
| rows OFFERED to the probe | 2,911,742,036 | 1,484,253,970 |
| rows kept (new) | 55,958,742 | 55,958,766 |
| kill ratio | **52.0 : 1** | 26.5 : 1 |
| offered rows per input lane | 57.1 | 29.1 |
| **cost per offered row** | **24.7 ns** | **39.9 ns** |
| frame body | 963 ns | 1,218 ns |
| ...kernel `run` | - | 346 ns |
| ...deopt re-run, PLAIN program | 412 ns | 430 ns |

Three things to read off it before designing anything:

- **The unit of the problem is 2.9 BILLION rows, not 51 million.** Dedup
  sees 57 offered rows per input lane and keeps one in 52. Any design
  that is not built for a 50:1 kill ratio is solving a different problem.
- **~28 of the 52 is WITHIN a frame; only ~1.7 is against history**
  (`CELESTE_DEDUP_CENSUS=1`, measured f50..f64, stable). This decides the
  architecture, see below.
- **24.7 ns per offered row is already respectable.** A probe into a
  56M-row visited set is a guaranteed cache miss (~80-100 ns of latency),
  so 25 ns means the existing path already gets real memory-level
  parallelism out of it. The roofline study has to beat a decent number,
  not a naive one. Do not assume 10x is sitting there.
- **The kernel's pre-dedup already halves the stream** (2.91G -> 1.48G,
  the 8.3:1 within-chunk collapse) - and yet the compiled path's cost per
  offered row is 1.6x WORSE (39.9 vs 24.7 ns), so it banks only 18% of
  the 50% row reduction. That discrepancy is unexplained and is the first
  thing the study should attack, because it is free money if it is a
  layout or batching artifact.

## Piece by piece: what exists, what the gap is

### 1. Merged program over input variants (CSE across the 64 inputs)

WHAT EXISTS. The compile recipe's `expand_bool` overlay turns the btn
concretization diamonds into lane EXPANSION, so the kernel runs the same
straight-line body once per input configuration, independently. There is
no sharing of any kind between configurations - not of the physics, not
of the control flow, not of the output rows until the pre-dedup hashes
them afterwards.

THE GAP. All of it. There is no representation in which "the program for
input config A" and "the program for input config B" are the same object
with a shared prefix. This is a compiler project, not a tuning pass.

THE PRIZE, measured. After pre-dedup the kernel still materializes
456,590,573 rows for 36,707,603 kernel-dispatched input lanes = **12.4
output rows per input lane**. Before pre-dedup it is ~8.3x that. The
pre-dedup ratio IS the evidence that the configurations converge: 8 of
every 9 emitted rows are bit-identical to another row from the same
chunk, which means the work that produced them was largely the same work.
Sharing it at compile time instead of discovering it by hashing
afterwards is the same win taken earlier and cheaper.

PRIORITY: **last**. Not because the prize is small - it may be the
biggest single one here - but because the two items below are cheaper,
measured, and independent, and because this one wants the dedup roofline
number first: if dedup gets 3x cheaper, the argument for eliminating rows
before they exist changes shape.

### 2. Tiered dedup

WHAT EXISTS, and it is three unrelated mechanisms rather than one:

- **Kernel pre-dedup** (`dispatch.rs`): an `FxHashSet<(u64,u64)>`
  allocated fresh per chunk, keyed from the kernel's output registers
  before materializing. Kills 8.3:1 within a chunk. Rebuilt per chunk, so
  nothing is remembered across chunks, and a chunk is 2,048 rows.
- **`Rt2::boundary`** re-hashes the surviving rows into canonical row
  keys, and within-block dedups again (measured 1.0:1 - the pre-dedup
  already caught everything).
- **`visited_row_keys` + `Visited`**: hashes every output lane of every
  fragment into a 128-bit key and probes the process's visited set, which
  since the visited-redesign is NOT a RAM hash map but mmap'd per-frame
  key files with an fp-run index and a sample/interpolation probe
  (plans/visited-redesign.md). Then a SERIAL pass assigns ids.

THE GAP. There is no tiering and no locality strategy: the per-chunk set
is thrown away, the visited probe goes straight to the global structure
for every one of 2.9 billion rows, and nothing batches probes to overlap
their misses deliberately. There is no core-local victim tier, no
sized-to-cache spill hierarchy, and no cross-core coordination protocol -
the parallelism is "each worker probes the shared read-only structure",
with the mutating insert serialized afterwards.

PRIORITY: **first**, as an isolated study before any integration. See D2.

THE SHAPE IS NOW MEASURED, and it is the favourable one. At f64:

```
  190,597,048  offered
   58,154,366  global probes today   (the per-fragment `seen` set: 3.3x)
    6,827,968  distinct in-frame     (a frame-wide tier would leave this: 8.5x fewer)
    3,850,608  new                   (history only kills 1.7:1)
```

So the first tier's working set is ONE FRAME'S DISTINCT ROWS - 6.8M keys,
109 MB raw, or hash-partitioned across 16 workers 425k keys / 6.8 MB
each (L3 resident; under 1 MB with 2-byte fingerprints, i.e. L2). Not the
56M-row global set. And **8.5x of today's global probes are redundant** -
the same key already probed by another fragment of the same frame -
because the existing local set is per-FRAGMENT and a frame has ~720 of
them. Widening that set from fragment to worker-batch or frame is the
first experiment, and it may be most of the win on its own.

The second tier is then a 6.8M-row stream against a 56M-row mmap'd set
at a 1.7:1 kill. That is a genuinely memory-bound problem and the place
for prefetch/MLP batching - but it is 3.6% of the rows, so it is no
longer the headline.

### 3. Deferred deopt

WHAT EXISTS: the opposite of the sketch. A chunk whose frame fails a
specialization premise is handled INLINE, on the worker, immediately,
and the whole chunk is redone. On the compiled path a failing chunk runs
the frame three times: the compiled attempt, then the specialized program
with a per-lane origin column over the whole chunk to find the failing
lanes, then the plain program on those lanes.

THE COST, measured: 412-430 ns/input-lane for the plain re-run alone,
comparable to the entire compiled engine (415 ns). Plus 204 ns for the
compiled path's retry. And it STARVES the kernel: only 36.7M of 51M
input lanes (72%) ever reach a kernel, because failing chunks are re-run
wholesale by the interpreter and f58..f68 hold 66% of the run's lanes.

It is also concentrated and growing: nothing deopts before f58, then f58
is 0.20 s of plain-program CPU, f62 11.71 s, f68 53.68 s - **255% of that
frame's own wall**. A campaign to H=90+ is dominated by it.

PRIORITY: **second**. It is the cheapest large win here, it is
independent of the dedup work, and it is what unblocks the kernel's
coverage.

### 4. Fewer plain-program fallbacks

WHAT EXISTS: 399,516 of 51M lanes (0.78%) need the plain program, all
from f58 on. f58 is where states start dying, and the death/restart path
is outside what the recipe was verified over. There is NO census of which
shapes or which premises - the one datum we have is incidental, from an
unrelated experiment: `assert_true` in `anonymous_61`'s
`in_h061_in_k030_and_or_continue_2`, on states with `will_restart=true`,
`has_dashed=true` and an empty object array.

THE GAP: the census, and then either compiled coverage for those shapes
or recipe entries that make the premises hold.

PRIORITY: **with 3**. It is a diagnostic pass, and it tells us whether
the deopt volume can be cut at the source rather than merely deferred.

## Constraints the sketch does not mention

These are not objections. They are the things that will sink the work at
integration time if they are not decided at design time.

### DETERMINISM is the hard one

Row ids are assigned in INSERTION ORDER, and checkpoints, bands, `g.bin`
and the whole backward sweep are written in terms of those ids. The
chunk-parallel frame path is batched rather than queued for exactly this
reason - see `step_parallel`'s doc comment, whose entire design
constraint is that the sequence of rows offered to the visited set is the
serial path's.

A tiered dedup with core-local tiers that spill when full does not
preserve that order. Decide before building:

- **(a) Split membership from identity.** Dedup in parallel for
  MEMBERSHIP only, then assign ids in one deterministic pass over the
  survivors in a canonical order. The ordered pass is over ~56M
  survivors, not 2.9G probes, so it should be cheap. This is the
  recommended option and it needs its own gate (byte-identical
  checkpoints against today). Philippe's suggestion of core-local ids
  composes with this: cores can assign local ids freely as long as the
  local->global renumbering is a deterministic function of a canonical
  order, which a hash partition gives for free.
- **(b) Accept isomorphism** and re-gate everything downstream. More
  freedom, much more risk, and it invalidates every byte-comparison gate
  we have.

Pick (a) unless the study shows it costs more than it saves.

### The visited set is not in RAM

Since the visited-redesign landed, the global tier is mmap'd per-frame
key files with an fp-run index, not a hash map. So "L3 then memory" is
really "L3, then page cache, then disk", and the existing local-first
probe tuning is prior art rather than something to reinvent. The tiering
has to be designed against that structure, and it may well be that the
right shape is a core-local Bloom/fingerprint tier in front of it rather
than a cache-sized copy of it.

### There are two key implementations and they are not known to agree

The kernel keys rows from its output registers (deliberately NOT a
bit-exact copy of `Rt2::boundary`'s key - see
plans/dedup-on-the-fly-plan.md), `Rt2::boundary` keys them canonically,
and `visited_row_keys` keys the exported `State` a third time. Gate 2
only ever proves `Rt2` keys equal `Rt2` keys. Nothing has ever shown that
`Rt2`'s keys equal `visited_row_keys`'. One tiered dedup needs ONE key
function, so this has to be established (or made true) first - and if it
IS true, a compiled run can stop computing keys two extra times, which is
worth most of 1,163 ns/input-lane on its own.

### The 24-row divergence: FOUND and FIXED (2026-08-19, D0 done)

It was never 24 rows. The per-frame `.rowkeys` sidecar diff showed the
two runs' key sets were **entirely disjoint from f25 onward** - the
searches were isomorphic (equal counts every frame) but globally
re-keyed, and the visited totals happened to land 24 apart because f25
produced 204 new rows under one keying and 180 under the other.

Root cause, pinned by `native-probe --frame-diff` (runs one frame under
both engines from the same checkpoint and dumps every output lane as a
readable row): at f25 the game first mints a
`Nil(Some("nil pointer to field tile"))`. The bridge cannot round-trip
the hint (`AV::Nil` has no payload), so compiled exports carried
`Nil(None)` - a different SHAPE HASH, hence different `visited_row_keys`
for every descendant forever. With the hint normalized away, the two
engines' f25 output row sets were already **identical, 204 = 204**. The
engines agreed all along; a diagnostic string was distinguishing search
states.

Fix: `erase_provenance_hints` in `make_state_abstract` - `Nil(Some(_))`
-> `Nil(None)` and `NilPointer(name)` -> `NilPointer("")` at the frame
boundary. Both strings are provenance-only (born on loads through a
`NilPointer`, consumed only by error messages; grep-verified), so this
is a canonicalization, not a widening - no rung needed. It also
pre-empts the same bug one step later: the bridge erases `NilPointer`
names too, and those sit in the shape hash the same way.

Gate: H=68 A/B re-run under FORMAT_VERSION 5 - visited totals equal
(55,958,742 both) and **all 68 frames' rowkeys sidecars are
set-identical** between the compiled and interpreted runs. On room
(1,0) the interpreter's own totals did not move (the hint was uniform
across states there), so the change is pure re-keying on this room -
but hint-distinct twins are possible in general, hence the format bump.

Residue: pre-fix checkpoints refuse to load (FORMAT 4 vs 5) and
certified campaign artifacts need re-derivation before further deep
gates use them. Also hardened alongside: a `check`-mode row-set
mismatch is now a typed `CheckMismatch` error that the optimistic deopt
arm PROPAGATES instead of catching as a premise failure (it was blind
at exactly f58+, the frames it was needed for), and the mismatch
message now prints the differing keys.

## Order of work

| # | item | why here |
|---|---|---|
| **D0** | ~~Explain the 24-row divergence~~ **DONE**: bridge erased nil provenance hints; fixed by erasing them at the boundary (FORMAT 5), gated set-identical at H=68 | blocks trusting any deep A/B, including this plan's own gates |
| **D1** | One key function: gate `Rt2` keys == `visited_row_keys`, or make them equal | prerequisite for a shared dedup; if it holds, a compiled run stops keying 3x |
| **D2** | ~~Dedup microbenchmark, isolated~~ **DONE** - see BENCHMARK_DATA.md "Dedup roofline, isolated". today 218 ns/row -> partitioned-16 **6.1 ns/row**; hash is 7 ns/row (nearly free); one mmap probe is ~700 ns; worker-persistent seen REFUTED (1.46x, needs the partition) | establish the ns/row roofline the way K2 established the kernel's |
| **D3** | Determinism design decision (a) vs (b), with a gate. **Decided by D2's data**: (a), hash-partition for membership; phase 2 assigns ids over candidates restored to (fragment, lane) order, which reproduces today's ids BYTE-IDENTICALLY - that is the gate | must precede any integration |
| **D4** | ~~Integrate the winning design~~ **DONE**: `partition_filter` in step_parallel, default ON. H=68: 168.6 -> **115.9 s wall (-31%)**, sidecars byte-identical (keys AND ids), suite green. Residual gap to chase: `fwd.partition_filter` is an 18.6 s BARRIER phase (could overlap the next batch's bodies), and the probe itself is still ~700 ns | the kernel precedent: microbench said 198 ns, campaign delivers 346 |
| **K1** | Deferred deopt: batch failures, one later interpreter pass | 412-430 ns/lane, and unblocks 28% of lanes for the kernel |
| **K2** | Fallback census **DONE** (2026-08-19): at H=68 room (1,0), ALL 2,935 deopting chunks (399,516 lanes) fail ONE premise - `assert_true %3771` in `anonymous_61` `in_h061_in_k030_and_or_continue_2`, the death/restart path (will_restart=true, empty object array; k030 = the `obj.is_solid_47` inline into `player.update_21`). Two candidate fixes, unmeasured: (1) add the death predicate (e.g. `will_restart`) to the pm1 partition cells so dead and alive lanes never share a chunk - the alive majority then never deopts and kernel coverage stops being starved (today failing MIXED chunks are re-run wholesale; f58-f68 = 66% of lanes). Row set provably unchanged (partitioning only regroups lanes); recipe change, so fingerprints move - the #126 partition-agnostic verify exists for exactly this. (2) a per-run shape-keyed deopt memo: once a shape has failed the attempt, skip the attempt for that shape and go straight to the granular pass - turns the failing chunks' triple-run into a double-run, no recipe change. (1) is the real fix; (2) is a stopgap | cut deopt volume at the source, not just defer it |
| **M1** | Merged program / CSE across input variants | biggest and hardest; wants D2's number first |

### D2 in detail, because it is the one that starts

The point is a number, isolated from the campaign, that we can then hold
the integration to - exactly what `--kernel-bench` did for the kernel
(198 ns/input-lane, and the campaign now delivers 346, and that 1.75x gap
is a known, chaseable thing rather than a mystery).

**Input data must be real.** The `frames/*.rowkeys` sidecars hold the
SURVIVING keys, which is the wrong distribution - the interesting stream
is the 52:1 offered one, and it is not persisted. So:

1. Add a dump mode that writes one frame's OFFERED key stream in probe
   order. Size check: f60 offers 120M rows = 1.9 GB at 16 B/key; f68
   offers 287M = 4.6 GB. Dump f55..f60, not f68.
2. Also dump the visited set as it stood at the start of that frame, so
   the benchmark probes against a realistic 25-50M-row set rather than a
   cold one.

**Baseline to beat:** today's path on that stream, which is 24.7 ns/row
(interpreter) and 39.9 ns/row (compiled). Reproduce both in the harness
before touching anything, and explain the 1.6x between them - that is the
first result, and it may be most of the win.

**Variants to measure**, each on its own and then composed:

- batched probing with explicit prefetch (issue N probes, then consume) -
  this is the classic MLP lever on a pointer-chasing probe and the
  existing 24.7 ns suggests some of it is already happening by accident
- a core-local fingerprint tier (8-16 bit fingerprints, L1-sized) in
  front of the global structure, sized to the measured duplicate recency
  (task #116 already measured dup-recency and pending-set size - read it
  before choosing a size)
- an L2/L3-sized second tier, and the spill policy between them
- cross-core: partition by key hash so tiers are disjoint and need no
  coordination at all, versus shared tiers with coordination. Partitioned
  is almost certainly right and also helps determinism option (a).
- SIMD-batched key hashing, separately from probing - the "keys" half and
  the "probe" half of the 24.7 ns have never been split, and they should
  be before anything is designed around either.

**Report**: ns/row for each variant at realistic set sizes (5M, 25M, 50M,
and one extrapolation to room (0,0) scale), plus the split between
hashing and probing, plus what each variant does to determinism.

## What is NOT in this plan

K5 (kernels for rooms (0,0)/(2,0)) and K6 as previously written. Both are
downstream of the numbers above: K5 multiplies a factor measured at ~1.0
under the ladder's own configuration, and K6's "make one emitted row
cheaper" is really M1 plus D2 once you look at where the row cost is.
plans/campaign-cost-plan.md's queue should be re-ordered behind this
file, not alongside it.
