# The ladder on kernels: forward + backward + refinement without the interpreter

Autonomous session, 2026-08-25 (worktree). Task: run the SAME full
precision ladder (`ladder.sh`: level-0 forward + backward sweep + banded
k=1..16 forward passes + horizon climbing) with the TRACED KERNELS as the
frame engine, interpreter as reference only.

## What the ladder actually is (mapped from the code)

One horizon H of `ladder.sh` runs, per precision level:

| stage | binary | primitive |
|---|---|---|
| level-0 forward extend | `rewrite bench --frames H` | `AbstractRun::step`, one frame = chunks through the frame body |
| position graph | `rewrite pos-graph` (or fused `--record-pos-graph`) | the SAME forward replay, with `PosObserver` tagging each lane (`inject_named(POS_ORIGIN)`) |
| backward sweep | `rewrite sweep --horizon H` | `sweep_time::backward_sweep_time`: candidate rows re-expanded through **`AbstractRun::step`** with a `__sweep_origin` column injected per lane |
| banded level k | `CELESTE_REM_BITS=k rewrite bench --band-dir <k-1>` | `AbstractRun::step` again, plus `BandFilter` (coarsen each output row to level k-1, look it up in k-1's row table + g) |

So there is exactly ONE frame primitive in the whole ladder -
`AbstractRun::step` - and it already dispatches per chunk to
`compiled::FrameEngine::run_frame_chunk` when `CELESTE_COMPILED_FORWARD`
is set. The backward pass, the band, the refinement mapping
(`coarsen_to` + row-table lookup), checkpoints, the visited set and the
horizon loop are all SEARCH machinery layered on that primitive; none of
them names the interpreter. Driving the ladder with kernels therefore
means making `run_chunk_kernel` serve every rung, not rebuilding any
pass.

## Why it does not already work: precision

Three places hardcode the level-0 abstraction:

1. **The kernels themselves.** `trace::widen` bakes the boundary
   widenings into the traced graph (rem := `[-0.5, 0.5)`, timer pins,
   `dash_effect_time` clamp, fruit band). Sound ONLY at Bits(0); at
   Bits(k) the full-interval rem has already destroyed the bucket, and
   no downstream pass can narrow it back. This is the "never widen
   without a rung that narrows it back" rule showing up as compiled
   code.
2. **`Rt2::boundary_canonicalize`** re-applies the same Bits(0)
   widenings (with containment asserts) to every kernel accumulator.
3. **`compiled_forward()`** therefore refuses `CELESTE_REM_BITS != 0`
   and any spd rung, up front.

## The design: rung-agnostic kernels, rung applied by the campaign

The key observation (already latent in `run_frame_chunk`'s contract):
the campaign re-applies its OWN abstraction to every frame output -
`stream_boundary_prepare` runs `split_precision_straddles` +
`make_state_abstract` on every state either engine produces, and THOSE
are already parameterized over `LadderPrecision`. So the kernels do not
need to know the rung at all; they need to stop pre-empting it:

* **Generate a second kernel set with `widen = false`** - the flag
  `trace_frame` already has (the concrete-oracle differential uses it).
  The kernel computes the frame's EXACT outputs; rem comes out as the
  computed interval (input width preserved: rem_in + const shifts, the
  flr fork splits), not the constant.
* **The engine boundary gets an exact variant** (`Rt2::boundary_exact`):
  materialize + canonicalize + hash + dedup, NO widening. Dedup of
  exact-identical rows is sound at every rung. The campaign boundary
  then applies the rung: bucket-straddle splitting and bucket widening
  for Bits(k), exactly as the interpreter path does, in the same code.
* **Dispatch selects the set by rung**: Bits(0) keeps the checked-in
  widened set (`celeste_kernels::traced` - faster, its in-kernel dedup
  key is rung-0-canonical); Bits(1..=15) uses the rung-agnostic set
  (`celeste_kernels::ladder`); `CELESTE_TRACED_SET=ladder` forces the
  agnostic set at Bits(0) for gating. Exact (k=16) and spd rungs stay
  refused for now (below).

Correctness argument, per rung k in 0..=15: the traced set is gated
row-key-identical against the interpreter per chunk
(`CELESTE_COMPILED_FORWARD=check`); with no widening on either side of
the bridge, both engines hand the campaign the same exact row set, and
the campaign applies the identical rung abstraction to both. The check
comparator must compare at the RUNG's abstraction, not at Bits(0) -
`FrameEngine::row_key_set` funnels through `Rt2::boundary`, which
widens at Bits(0), and two rung-k-distinct sets can coarsen equal. So
check mode at rem != Bits(0) abstracts both sides with the campaign's
`make_state_abstract` and compares `sweep::row_keys` sets (a strictly
stronger comparator at the rung).

### Why not kernels-per-precision-level?

Considered and REJECTED as the first step, kept as the optimization:

* Bits(k) baked in means the widening becomes `flr(rem * 2^k)/2^k`
  bucket arithmetic in the graph plus a SECOND fork (the output bucket
  straddle - a width-2^-k interval crosses at most one bucket boundary,
  the same <=2-fragment structure as the flr fork). Buildable with the
  existing `Op::Split` vocabulary, but it is 16 kernel sets, a
  per-rung staleness gate, and a per-rung differential - and what it
  buys is only the in-kernel dedup catching rows that differ in
  soon-to-be-widened bits (the 5.9x factor of the 46x analysis at
  level 0; MUCH smaller at high rungs, where rows genuinely differ).
  tracing.md already frames this: "shape only - correct, rung-agnostic,
  dedups nothing" -> "shape + rung-aware key" is a LAYER on top. Build
  the correct rung-agnostic base first, specialize per rung when a
  measurement says the dedup loss matters.
* The banded rungs are TUBE-CONFINED (band filter) and empirically
  cheap next to level 0 (`ladder.sh`: "Level>=1 runs are tube-confined
  and cheap"), so the performance case for per-rung baking is weakest
  exactly where it would be used.

### The rungs still refused, and what each needs

* **Exact rem (k=16, the top rung) - DONE later the same session.** An
  exact-rem block carries rem as `Col::N` (plain numbers); the agnostic
  set's rem slot is `ival`, so bind refuses. The THIRD set
  (`celeste_kernels::exact`, `WalkOpts::EXACT`: `widen = false`,
  `ival_paths = []`) traces rem as a plain symbolic num, so
  `__split_by_flr` is the identity and the set has NO rem forks -
  which is why it is 16,043 lines against the ladder set's 28,407
  (kernel1: 7,702 vs 20,066; the fork dimension gone). Auto-selected at
  `RemPrecision::Exact`, `boundary_exact`, gated by
  `exact_kernels_reproduce_the_interpreter_at_k16`. NOTE that
  `make_state_abstract` skips the fruit off/y widening at Exact, which
  the exact set also does not bake (moot for room (1,0), no fruit).
* **Spd rungs (`CELESTE_SPD_WIDTH_LOG2`).** Bucketed spd makes
  `player.spd.x/y` interval INPUTS, which the kernels type as num.
  Needs `ival_paths += spd` plus emitter support for whatever spd
  feeds that cannot take an interval yet (cf. the interval-sin fix for
  room 2's fruit). Room (1,0)'s ladder does not use spd rungs; still
  refused.

## The interpreter out of the search path: strict mode

`run_frame_chunk` today falls through to the campaign program on any
missed chunk. That is a silent interpreter dependency, so:

* `CELESTE_KERNEL_STRICT=1`: a chunk the kernel set cannot take
  (shape miss, bind refusal, declined lane) is FATAL. The failure
  aggregates every distinct miss reason with lane counts
  (`KERNEL_MISS_WHY` already collects them) and panics with a
  distinct message after the frame's chunks have all been dispatched
  (the dispatch-all-before-interpreting order already guarantees the
  census is complete). Checkpoints are per completed frame, so the run
  resumes at frame f-1 after the gap is fixed - the doctrine's shape.
* Default (unset) keeps today's fall-through, because `check` mode and
  the differential tests NEED the reference path.

## The backward sweep and pos-graph: the passthrough column

Both the sweep and the position graph attribute outputs to inputs by
injecting a per-lane u32 column as a global (`__sweep_origin`,
`POS_ORIGIN`) and reading it back after the frame. Two designs were on
the table: (1) origin-shape kernels - append `__origin` to
`GLOBAL_NAMES` and trace tagged shapes, clean but it re-keys every
shape hash, row key, checkpoint and visited table; (2) an
engine-carried origin column - metadata beside the block, no shape
change. Philippe chose (2), and it is DONE (2026-08-25, worktree):

* **`Rt2::origin`** (celeste-engine) is the carrier: optional per-lane
  u32 metadata, empty = untracked, else `len == width`. Not part of
  the heap, the shape hash, or any column - a tagged block binds the
  same kernel as an untagged one. Every lane permutation carries it
  (`retain_lanes`, `slice_lanes`, `partition_*` via clone+retain,
  `merge_many` concatenates it and asserts all-or-nothing presence,
  `widen` copies the source lane's).
* **The bridge is where the representations meet**
  (`compiled::run_frame_chunk`): outside the engine the tag stays what
  it always was - a per-lane heap global the interpreter path executes
  natively and the observers read. On entry the chunk's tag global
  (`ORIGIN_TAGS`: `SWEEP_ORIGIN` / `POS_ORIGIN`) is read into
  `block.origin` (import drops the global itself, so the shape is the
  untagged one); on every exit back to `State` - the merged kernel
  output, and each miss path's export before interpreting - the
  metadata is re-injected as the global (`export_block_tagged`). The
  campaign-program fallback runs the ORIGINAL state, tag intact.
* **The generated `append{i}` writes provenance**: `step` slices
  `&b.origin[lo..lo+n]` into the sink, and each written row pushes its
  input lane's origin into `acc.origin`. The kernel mechanically knows
  which lane produced each row - that knowledge just goes into
  metadata now instead of being lost.
* **Origin participates in EVERY dedup key when present.** The
  in-kernel `seen` key mixes it (`mix64`, a bijection, so same row +
  different origins can never collide), and `Rt2::boundary_finish`
  mixes it into the per-lane row key, which the in-block dedup, the
  cross-block dedup and `merge_many` all key on.

**The soundness wrinkle, resolved AGAINST the "any survivor is fine"
relaxation.** The tempting design was to keep origin out of the dedup
keys and let a deduped row keep any one origin. That is wrong for both
consumers: the sweep marks candidate `src` rows whose successors land
in B, so if lanes from candidates A and B produce the SAME successor
row and only A's survives dedup, B never qualifies at that frame - its
`g` comes out too large, silently, in the direction that loses winning
paths. The pos graph loses the pair the same way, and a too-small
table under-generates sweep candidates with nothing to notice. The
injected GLOBAL got this right for free - a per-lane column is part of
the state, so dedup kept distinct origins apart - and the metadata
design must reproduce exactly that, which is what mixing the origin
into the keys does. What IS legal (and happens): two lanes with the
same origin and the same row dedup to one, and the kernel path can
drop (origin, row) DUPLICATES the interpreter's unmerged fragments
keep - the pair SET is identical, and both consumers are idempotent
per pair (the sweep's `newly` bitset, the recorder's `sort+dedup`).

Wired through: `backward_sweep_time` and `pos_graph::build_from_replay`
no longer disable the engine (`interpret_origin_replays` is deleted),
`interpret_state_base` no longer refuses pos-graph recording under the
compiled engine (the FUSED recording runs on kernels), `ladder.sh
KERNELS=1` no longer forces `FUSE=0`, and check mode's
`row_key_set` mixes origins too, so a check-mode replay compares
(origin, row) PAIR sets rather than the row projection. Gated by
`kernel_replays_carry_origins_like_the_interpreter` (pair-set identity
against the interpreter for both taggings, strict, coverage asserted)
and by the g.bin / posgraph.bin A/B (BENCHMARK_DATA.md "The origin
passthrough", 2026-08-26: both byte-identical across engines on room
(1,0) at H=68, fused row table identical, fused table = replay + the
1 spawn pair).

## Refinement mapping: nothing to do

`BandFilter` coarsens level-k output rows with `coarsen_to` (State
domain, campaign side of the bridge) and looks them up in level k-1's
row table. The kernels never see it. The band-miss soundness TODO in
`run.rs` (a miss against an unbanded previous level must be fatal) is
orthogonal to the engine and untouched here.

## Work done in this session

1. `Rt2::boundary_exact` in celeste-engine (`boundary_prepare` +
   `boundary_finish` shared with `boundary_canonicalize`; the widenings
   are `boundary_widen`, which the exact path skips).
2. `widen` threaded through `shapes::walk` -> `room_shapes_in` ->
   `room_kernels_widened_in` -> `write_room_kernels_ladder`; `transpile
   --room-kernels-ladder DIR` generates the rung-agnostic set;
   `regen-generated.sh` regenerates both sets.
3. The room (1,0) rung-agnostic set checked in at
   `crates/celeste-kernels/src/ladder/` (3 shapes, 28,407 lines), with
   `FINGERPRINT`, and a staleness gate `ladder_kernels_are_current`
   (byte-for-byte, like `traced_kernels_are_current`; ~2 s under quick).
   Checking it in re-triggered the known rustc DWARF stack-overflow
   SEGV on huge generated functions, this time in the DEV profile -
   `[profile.dev.package.celeste-kernels] debug = false` beside the
   existing release override.
4. `compiled::dispatch::TracedMode`: registry + boundary selected once
   per process from the rem rung (`CELESTE_TRACED_SET=traced|ladder`
   overrides); `compiled_forward()` now admits Bits(1..=15) via the
   ladder set and refuses Level0-set overrides off Bits(0), Exact rem,
   and spd rungs, each with the reason; `traced_set_fingerprint()`
   returns the ACTIVE set's fingerprint, mode-tagged, so engines never
   share checkpoints.
5. Check mode at rem != Bits(0) compares rung-abstracted
   `sweep::row_keys` sets (`rung_row_key_set`: both sides through the
   campaign's own `split_precision_straddles` + `make_state_abstract`).
6. `CELESTE_KERNEL_STRICT=1` as above (`run_frame_chunk` panics with
   `dispatch::miss_report()` before the fallback would run).
7. `AbstractRun::interpret_origin_replays()`, called by
   `backward_sweep_time` and `pos_graph::build_from_replay`: drops the
   compiled engine for origin-tagged replays with a printed notice
   instead of failing mid-sweep on the length check.
8. `ladder.sh KERNELS=1`: exports `CELESTE_COMPILED_FORWARD=1` +
   `CELESTE_KERNEL_STRICT=1` and forces the pos-graph replay path
   (fused recording needs the passthrough too).
9. **The engine fingerprint is per precision level** - found by the
   first end-to-end `KERNELS=1` smoke, not by review. The kernel set
   now varies with the rung, so `traced_set_fingerprint()` does too -
   and the k=1 band loader recomputed LEVEL 0's checkpoint fingerprint
   with k=1's own set and refused the checkpoints level 0 had just
   written. `compiled_engine_fingerprint(rem)` /
   `dispatch::traced_mode_for(rem)` compute each level's fingerprint
   with THAT level's engine; a level-0 process's fingerprint is
   byte-identical to what it was before this campaign (the traced
   set's), so recorded campaigns stay valid.
10. End-to-end smoke through the real driver (`KERNELS=1 L0=... KROOT=...
   ./ladder.sh 40 40 1`, room (1,0), release): level-0 forward 40 frames
   entirely on kernels (`kernel lanes: traced 673503 missed 0
   plain-routed 0`, strict), pos-graph and sweep replays print the
   disable notice and run, k=1 banded loads level 0's band and refutes
   (h40 has no win, so an empty band is the correct answer).
11. **A full ladder horizon at production depth, on kernels**
   (`KERNELS=1 ./ladder.sh 94 94 3`, room (1,0), release, scratch
   dirs). Every forward stage strict and kernel-served, `missed 0
   plain-routed 0` throughout:

   | stage | wall | peak | result |
   |---|---|---|---|
   | l0 bench f94 | 350 s | 8.30 GB | win at f89; traced 172,626,763 lanes (the Stage-5 lane count exactly) |
   | l0 pos-graph (interpreter replay, notice printed) | 775 s | 6.48 GB | - |
   | l0 sweep (interpreter replay, notice printed) | 184 s | 22.0 GB | 178.6 M rows, 2 win seeds, e+g optimum 89 |
   | k1 bench (LADDER set, banded) | 16 s | 9.10 GB | win at f93; traced 1,163,134 lanes |
   | k1 sweep | 26 s | 1.5 GB | 1 win seed, e+g optimum 93 |
   | k2 bench (LADDER set, banded) | 1 s | 0.18 GB | no win -> horizon 94 REFUTED at k=2; traced 69,008 lanes |

   These wall times are a smoke run sharing the machine with a test
   build, not benchmark numbers. The banded rungs cost SECONDS against
   level 0's minutes, which is the tube-confinement working and the
   reason per-rung kernel specialization was deferred. The k=1/k=2
   stages were then re-run under `CELESTE_COMPILED_FORWARD=check`
   (per-chunk row-set comparison at the rung's abstraction) - a
   spurious refutation is the failure that matters most, so the
   refuting rung is the one that got the differential treatment.
   RESULT: both rc 0 with no mismatch over all 94 frames; k=1 wins at
   93 on both engines, k=2 wins on neither. The h94 refutation stands
   on the interpreter's own authority, chunk by chunk.
12. Gates, all green: `ladder_kernels_reproduce_the_interpreter_at_bits1`
   (28 frames, room (1,0), check mode at `CELESTE_REM_BITS=1` with
   strict on, kernel engagement AND zero missed lanes asserted - frames
   25-28 cover the fork/straddle region), the same at Bits(0) with
   `CELESTE_TRACED_SET=ladder`, and `ladder_kernels_are_current`.

## Not done, in honesty order

* Spd rungs.
* Per-rung baked kernels (the optimization layer: a rung-aware in-kernel
  dedup key, worth pricing only if the agnostic set's weaker dedup shows
  up in a campaign measurement).
* A full ladder campaign run on kernels end to end, and any performance
  number - needs release builds and hours; the per-rung differential
  gates are the evidence offered instead, and no perf claim is made.

## Latticeified, all rooms, one table (2026-08-26, worktree)

Philippe's spec (plans/specialize.md "Spec: latticeify everything, all
rooms, one table") is implemented; the non-lattice generators are gone.

* **Every checked-in set is now CONSTANT-LATTICE specialized.** The three
  entry points (`write_room_kernels{,_ladder,_exact}`) all run
  `room_constant_lattice(root, WalkOpts)` - the fixpoint and every traced
  frame under the variant's own widening options - then emit. The walk
  generator (`shapes::walk`-based `room_kernels_with`/`room_shapes_in`)
  is deleted; `shapes::walk` itself survives only inside
  `specialize_probe`. A shape the fixpoint cannot trace, bind, lower or
  render is FATAL at generation (a missing kernel is a runtime coverage
  gap, not a smaller set).
* **Layout: one `room<x><y>/` subdirectory per room per variant.** Rooms
  are generated one PROCESS at a time (`CELESTE_START_ROOM` feeds
  OnceLocks in game_runner), so the multi-room merge is file-level:
  `transpile --merge-kernels DIR` writes the top-level `mod.rs` with
  `SETS: &[&[Kernel]]` (every room's `KERNELS` table) and a
  `FINGERPRINT` re-hashed over every room's kernel sources.
  `Dispatch::new_multi` flattens `SETS` into the one shape-hash
  registry and REFUSES any collision - rooms have distinct object
  composition, so a collision is a generator/hash bug to surface. (The
  base and ladder variants of one room share shape hashes - pinning
  changes values, not structure - which is why variants stay separate
  registries selected by `TracedMode`, never one table.)
* **Rooms (0,0), (1,0), (2,0) generated for all three variants.**
  Sizes: traced 532,098 lines / 26 MB, ladder 558,725 / 27 MB, exact
  244,819 / 9.7 MB - 1.34 M generated lines total. Generation cost
  (quick build): room (1,0) ~2 s per variant, (0,0) ~3-9 s, (2,0)
  ~13-34 s; the whole 3x3 matrix ~1.8 min.
* **The block-uniform interval row-key hole is filled.** The fruit
  rooms' rung-agnostic kernels compute the fruit's `y` from the
  boundary-widened (block-uniform) `off`, and that interval feeds the
  in-kernel dedup key; `lower.rs` `Op::Bits` now packs the two u32
  endpoints as `(lo << 32) | hi`, injective, the same packing
  `zw_bits_i` uses per lane. The tri-state-uniform-bool hole stays open
  and named.
* **Strict is the DEFAULT.** `CELESTE_KERNEL_STRICT` unset means a
  missed chunk is fatal with the full reason census;
  `CELESTE_KERNEL_STRICT=0` restores the counted fall-through for
  diagnosing. `check` mode is unaffected in the covered configs (the
  comparison runs the interpreter as the reference, not as a fallback).
* **Gates.** Per-variant staleness gates regenerate room (1,0) and
  re-check every room's fingerprint via `merged_mod_rs` (so hand-edits
  to ANY room fail fast); `room00_kernels_are_current` /
  `room20_kernels_are_current` (#[ignore]) regenerate the other rooms
  byte-for-byte. The room-1 differentials (`traced_kernels_reproduce_
  the_interpreter` - now also asserting `missed_lanes() == 0` -,
  bits1, k16, level0-agnostic, the origin pair-set gate) run against
  the frozen DCE'd program as before. Rooms (0,0)/(2,0) get
  `room00_lattice_kernels_match_the_interpreter` /
  `room20_lattice_kernels_match_the_interpreter` in the main suite:
  kernel-only `trace::run::Run` vs `AbstractRun` on the room-aware
  `compile_from_disk` program, comparing per-frame REACHABLE-STATE sets
  (row keys are confounded by the raw program's closure-upvalue boxing;
  the frozen artifacts for those rooms do not exist and `bin/freeze` is
  deleted, so this is the strongest per-room oracle available).
* **traced-kernel-check reconciliation.** The feature-gated, gitignored
  room-(2,0) `lattice` module and its test are DELETED; the checked-in
  `traced::room20` set plus the main-suite gate replace them.
  `traced_kernel_check::kernels` now re-exports
  `celeste_kernels::traced::room10`.

### Still not done, in honesty order

* Spd rungs (unchanged).
* In-search campaigns for rooms (0,0)/(2,0) on kernels: blocked on the
  missing frozen rewritten artifacts (`rewrites-room00.jsonl` /
  `rewrites-room20.jsonl` have no `.program.zst` and the freeze tooling
  was deleted). The kernels are ready; the campaign reference program is
  not. Restoring a freeze path (or bridging the raw program's boxing
  cells) is its own task.
* A ladder/exact differential for the fruit rooms at their rungs (the
  variant semantics are gated on room (1,0), the lattice constants on
  the base variant per room; the cross term - e.g. room (2,0) at
  Bits(1) - has no oracle until the campaign artifact exists).
