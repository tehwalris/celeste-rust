# Keying / widening / subtraction flow — actual vs intended

Written 2026-08-29 after the Bits(2) "divergence" turned out to be a check
artifact (frontier subtraction applied to the ASM side but not the interpreter
reference). Philippe asked to compare the REAL flow against the model he has in
mind, and to write down the properties to hold so we can re-check later. This
doc is the record; nothing here is a fix yet.

## The model Philippe has in mind (intended flow)

- **Interpreter**: run the frame → WIDEN → key → subtract (within-frame, then
  cross-frame/frontier). The order after "run the frame" is widen-then-key, and
  subtraction is on the widened keys.
- **Kernels**: run the frame → directly emit ALREADY-WIDENED states → the keys
  it computes already correspond to those widened states → within-frame AND
  cross-frame (frontier) subtraction during emission, ALL on the widened keys
  and widened states.

The essential property: **both engines widen, then key, then subtract, on the
SAME widened key space.** No engine subtracts on a pre-widening (exact) key.

## The actual flow (as of this commit)

Forward worker: `run.rs::step_parallel` → per chunk: `interpret_state_base`
(the frame body, kernel or interpreter) → `stream_boundary_prepare` → 
`stream_boundary_subtract`.

`stream_boundary_prepare` (run.rs:487), applied to EVERY engine's frame output:
1. `split_precision_straddles` — split a lane whose rem straddles a Bits(k)
   bucket boundary into one lane per bucket.
2. `make_state_abstract` — the WIDENING: `make_state_abstract_rem` (rem →
   Bits(k) buckets) + `make_state_abstract_spd` + `apply_conservative_widenings`
   + `erase_provenance_hints`.
3. band filter (refinement ladder) — see below.
4. `state.gc()` — canonicalize (gc IS the canonicalizer; row hashing is
   heap-layout-sensitive without it).
5. `engine_row_keys(state)` — key the WIDENED + gc'd state. THIS is the one
   search key; the checkpoint stores this state so the backward sweep recomputes
   the identical key.
6. `candidates_from_keys(engine_keys, …, visited)` then `subtract_decide` —
   frontier subtract on the WIDENED key, and `visited` is POPULATED with the
   widened key.

So downstream of the frame body, the flow IS widen → key → subtract on the
widened key, identically for both engines. `make_state_abstract`'s doc even says
it "is also what makes the compiled engine's exports and the interpreter's
states ONE key space." Interpreter side: matches the model exactly.

### Where the kernel departs from the model

Inside `interpret_state_base`, the compiled path runs `FrameEngine::run_frame_chunk`
→ `asm_kernel::run`. That append (asm_kernel.rs:243) does its OWN keying and
frontier subtraction BEFORE `stream_boundary` ever widens:

- **Level0 set** (`exact == false`, Bits(0)): the graph is PRE-WIDENED
  (`trace::widen` baked the Bits(0) rem/timer widenings into the emitted
  values), and `acc.boundary(ids)` re-applies the boundary widening as an
  agreement check and computes keys. So Level0 kernels emit WIDENED states and
  key them widened — this matches the model.
- **Ladder set** (`exact == true`, Bits(1..15)) and **exact set** (k=16): the
  kernel is RUNG-AGNOSTIC — one assembled kernel is shared across every rung,
  so it CANNOT widen to a specific Bits(k). It calls `acc.boundary_exact()` (no
  widening) and keys the EXACT rows. Then `chunk_skip(acc.row_keys[i])` does a
  frontier subtract on those EXACT keys. The rung widening happens LATER, in
  `stream_boundary`'s `make_state_abstract`.

So for the ladder/exact rungs the kernel does NOT match the model: it emits
exact, keys exact, and frontier-subtracts on the exact key — the widening is
deferred to `stream_boundary`.

### Is the ladder kernel's exact-key `chunk_skip` a bug?

Not a soundness bug in the FORWARD SEARCH, but a half-measure:
- `chunk_skip = frontier_hit || within_frame_dup`. The frontier (`Visited`)
  stores WIDENED keys (populated in `stream_boundary_subtract`, step 6). The
  kernel checks `contains_historic(EXACT key)`. That hits only when
  `exact_key == widened_key` — i.e. clean-bucket, no-straddle, no
  widened-timer/spd rows. For those the widened key IS in the frontier, so the
  row WOULD be dropped by `stream_boundary` anyway → the early drop is SOUND
  (drops a subset of what the real subtraction drops, on keys that match). For
  straddling / widened-field rows `contains_historic(exact)` misses → the row
  is kept and `stream_boundary` subtracts it on the widened key. So the forward
  search is correct end to end.
- It is a PARTIAL optimization: it only saves the export/merge of the
  clean-bucket rows, and it acts on a key space (exact) that is NOT the search's
  identity (widened). That mismatch is exactly what makes it invisible-until-it-
  bites (the check, below), and it is NOT the model.

### Why the check saw a "divergence"

`CompiledForward::run_chunk` in check mode compares:
- `reference` = `interpret_prepared_cfg(state)` — full frame output, NO
  subtraction (raw interpret, no `stream_boundary`).
- `got` = `run_frame_chunk(state)` — kernel output WITH the exact-key
  `chunk_skip` frontier subtract applied.
Both are then rung-abstracted (`rung_row_key_set`) and compared.

Under `CELESTE_FRONTIER_ONLY=1` the frozen frontier is non-empty, so `got` is
missing the already-visited clean-bucket rows and `reference` is not → the
"720 missing rows". WITHOUT frontier-only the frontier is empty, `chunk_skip` is
a no-op, and the check passes all 30 frames. So the check compares the ASM's
post-frontier-subtract, pre-rung-widen output against the interpreter's
pre-subtract, pre-widen output — asymmetric in TWO ways at once (subtraction,
and the key stage it happens at).

## Properties to hold (the checklist to re-verify)

1. **One widened key space.** Every stored/compared row key is
   `engine_row_keys(make_state_abstract(split(state)))` — the widened key. No
   forward decision (store, subtract, band) is made on a pre-widening (exact)
   key. STATUS: true in `stream_boundary`; VIOLATED by the ladder kernel's
   `chunk_skip`, which subtracts on the exact key (sound-but-partial today, and
   the source of the check artifact).

2. **Widen-then-key-then-subtract, both engines.** STATUS: true downstream of
   the frame body for both engines (`stream_boundary`). The ladder kernel keys +
   subtracts BEFORE the widen; the Level0 kernel widens in the graph first.

3. **Canonical widening (band lookup).** `make_state_abstract` must be
   CANONICAL: `widen(s)` is EXACTLY the representation the coarser level would
   itself have produced, because the band lookup that follows
   (`stream_boundary_prepare` step 3) is EXACT EQUALITY on the coarsened key,
   not intersection. Idempotence does NOT imply this (the fruit `off`/`y` bug
   was idempotent and still landed between rungs). STATUS: KNOWN GAP, already
   written up as the TODO at run.rs ~517-588. See below.

4. **Intersection semantics is what we actually mean.** A finer (e.g. Bits(0))
   forward state should be kept iff it INTERSECTS any coarser (e.g. Bits(2))
   marked/banded state — because an interval state represents a SET of concrete
   states, and we want to keep it if any concrete member is reachable-at-the-
   coarse-level. The exact-equality band lookup is an OPTIMIZATION of the
   intersection test that is only valid when widening is canonical (property 3).
   The robust version replaces the lookup with the intersection test directly;
   then widening need not be canonical at all. STATUS: not implemented; the
   exact-equality lookup is what runs.

5. **Symmetric subtraction in the check.** The check must apply the SAME
   subtraction (and at the SAME key stage) to both sides, or none. STATUS:
   VIOLATED under frontier-only (asymmetric); `CELESTE_ASM_NO_SKIP=1` restores
   symmetry by turning the ASM's early drop off. The clean fix is to compare the
   frame's full widened output on both sides (turn the exact-key `chunk_skip`
   off in check mode) — NOT to add an exact-key subtraction to the reference,
   which would bless the wrong key stage. See "check fix" below.

## The refinement-ladder intersection property (property 3 + 4, expanded)

The forward pass at level k stores widened Bits(k) rows. The band filter keeps a
level-k row only if, coarsened to level k-1, its key is in level k-1's visited
table AND within the backward-marked band (`g_prev`, `earliest_frame`). This is
an EXACT-EQUALITY lookup on the coarsened key.

Philippe's framing (to preserve): a state with intervals represents a SET of
concrete states. What we want is: keep the finer state if it INTERSECTS any
marked coarser state. One way to get that with an exact lookup is a widening
SPECIFICALLY DESIGNED so a finer (bit-free) state widens to be bit-two-
equivalent — it lands EXACTLY on the coarse state's representation precisely
when it would have intersected it at all. That is "canonical widening" (property
3). The alternative is to test intersection directly (property 4), which needs
no canonicity.

The existing TODO (run.rs, in the `None =>` arm of the band mask) records:
- A miss against an UNBANDED previous level (k=1, whose prev is level 0) MUST be
  zero and SHOULD be fatal — under a correct canonical widening it is
  impossible (simulation: level k's row coarsens to a level k-1 row that k-1
  itself reached). It is silently a dropped-lane counter today.
- The fruit `off`/`y` bug: `make_state_abstract_rem` widened `off` but left `y`
  concrete while the coarse level derives `y` FROM `off` by interval arithmetic,
  so the coarsened row landed BETWEEN rungs and every fruit-alive lane was
  dropped. The violated property was canonicity, NOT soundness of the widening.
- Two proposed fixes, neither done: (1) assert `band_missing == 0` when the
  previous level is unbanded (exact, free, turns wrong-answer into stopped
  campaign, but only usable at k=1); (2) replace the exact lookup with the
  intersection test (removes the failure class at every k, costs more, band path
  only).

## The check fix (separate from the architecture)

Make check mode compare the frame's full WIDENED output on both sides:
- Turn the ladder kernel's exact-key `chunk_skip` OFF in check mode (it already
  does exactly this for `CELESTE_ASM_NO_SKIP`; wire `CELESTE_COMPILED_FORWARD=check`
  to the same off-switch). Then `got` = full kernel output, `reference` = full
  interpret output, both rung-widened and compared — symmetric, "off for both".
- Do NOT instead add an exact-key frontier subtract to the reference: that would
  make the check bless the pre-widening key stage, which is the very thing this
  doc flags as not-the-model.
- Add a NO-frontier `*_at_bits2` differential gate (it already passes) so the
  real property — the ladder kernel reproduces the interpreter's widened row set
  at Bits(2) — is locked in.

Longer term, if the kernel's early frontier drop is worth keeping as an
optimization, it should act on the WIDENED key (property 1). For the rung-
agnostic ladder kernel that means either (a) the kernel widens to the active
rung before its own key/subtract (giving up rung-agnosticism, one kernel per
rung), or (b) dropping the kernel-internal frontier drop entirely and letting
`stream_boundary` be the only subtractor (simplest; the export/merge saving is
the only thing lost).

## Open questions to check later

- RESOLVED: `within_frame_dup` (the Option-4 shared within-frame set,
  dispatch.rs:326) acts on `acc.row_keys` = the EXACT key for the ladder, same
  as the frontier drop. But it is SOUND: exact-equal implies widened-equal
  (widening is a function), so early-dropping an exact-duplicate sibling can
  never drop a distinct widened row — the two would dedup to the same widened
  row in `stream_boundary` anyway. It shares the frontier drop's "acts on the
  exact key, not the model's widened key" property, but unlike the frontier drop
  it cannot even be partial-wrong. It is still off the model (property 1/2).
- Confirm the Level0 kernel's `boundary(ids)` widening is byte-identical to
  `make_state_abstract`'s Bits(0) widening (it is described as an "agreement
  check" — verify it actually agrees, i.e. the graph pre-widening + boundary ==
  make_state_abstract at Bits(0)).
- Decide property 4 vs 3: intersection test vs canonical-widening guarantee. The
  TODO leans toward the k=1 assertion (cheap) now and intersection later.

## The plan: move widening INTO the graph (agreed 2026-08-29)

Goal state: the kernel emits ALREADY-WIDENED rows, so the one key (still folded
in Rust today, ASM later) is computed on the widened = stored state. No widening
in the wrapper on the kernel path. The current bug - key the exact state, store
the widened one - goes away because there is one state and one key.

Corrected gate (replaces the weaker interp-based "property 3" above): the
canonical-match gate is a KERNEL-vs-KERNEL differential that isolates the
widening from both the frame compute and the interpreter:
- Kernel A (exact, today's): emit exact -> widen EXTERNALLY
  (`split_precision_straddles` + `make_state_abstract`, already the
  `stream_boundary` path) -> dedup -> set X.
- Kernel B (new): emit widened in the graph (bucket-fork + snap) -> dedup -> Y.
- Assert X == Y.
A and B share the exact same frame compute, so any diff is purely the widening.
Composes with the existing no-frontier check (`A == interp` exact) to give
`B == widen(interp)` = what should be stored. This gate covers the FORWARD
widening at the current rung ONLY. The band coarsening (`coarsen_to` to the
previous level, exact-equality lookup - the fruit off/y class) is a SEPARATE
operation, already exists, and consumes B's widened output as its input, so it
is out of scope here.

### Execution order

Phase 1 - rem widening in the graph + de-risk + gates.
  1a. Prototype the rem bucket-fork+widen in the fused graph tail (rem.x/y):
      bucket-edge fork = `scale by 2^k -> zi_fork_flr -> scale back`, then snap
      each fragment to its full bucket. Behind a flag. MEASURE node count per
      shape at a rung, before/after. Expectation: hash-consing shares most of it
      with the existing `__split_by_flr` forks; confirm it is cheap. If it is
      not, stop and reconsider before going wider.
  1b. Build the A-vs-B differential gate and the assert-noop gate
      (`make_state_abstract`+`split` on B's output is a fixed point - catches
      UNDER-widening; A-vs-B catches WIDENED-DIFFERENTLY). Iterate rem until
      both green across rungs and shapes. No behavior change yet: B is behind a
      flag, `stream_boundary` still runs (idempotent double-widen).

Phase 2 - the rest of `make_state_abstract` in the graph.
  Add the spd rung (same bucket-fork shape), fruit `off` -> [0,39], and
  `apply_conservative_widenings`. Confirm `erase_provenance_hints` (bridge
  already exports hint-less) and the gc/canonical numbering (boundary's
  `canonicalize_ids`) are already covered. Extend the two gates to these; green
  across rungs, shapes, AND a room with a live fruit (room 2,0) - the assert-noop
  fruit case is exactly where a non-canonical widening hides.

Phase 3 - make B default + turn the wrapper widening OFF on the kernel path.
  Flip B on. Disable `split_precision_straddles`+`make_state_abstract` in
  `stream_boundary` for KERNEL-provenance output (branch by provenance; the
  interpreter-fallback output keeps them). The assert-noop gate is the guard.
  `stream_boundary` still keys, band-filters, subtracts, and INSERTS into
  `visited` for both paths. Re-benchmark (fork-count growth is the perf watch).

Phase 4 - make the check frontier-symmetric (the real fix, not off-for-both).
  With B keying the widened state, both engines share one widened key space.
  Fix `CELESTE_COMPILED_FORWARD=check` to widen the reference (idempotent for B)
  and apply the SAME read-only frontier subtraction to BOTH sides before
  comparing - "on for both". Add a no-frontier `*_at_bits2` differential gate
  (already passes) and a frontier-symmetric check-mode test. The compiled ladder
  can then run rungs under frontier-only.

Later / out of scope now: fold the key HASH into the ASM (currently Rust
`boundary_finish`); the band-coarsening canonical property (separate, exists).

## Progress log

### Phase 1 DONE (2026-08-29)

The rem rung widening is in the graph, behind `CELESTE_WIDEN_IN_GRAPH=1`,
with both gates green and the node-count de-risk measured.

- `trace::widen::rem_bucket_node` - the rem bucket fork + snap:
  `scaled = old / 2^-k` (a DIVISION by a representable constant, never a
  multiply by the unrepresentable `2^k`), fork at its integer floors (the
  `__split_by_flr` primitive), then `flr(frag) * 2^-k` snaps to the full
  bucket. `widen_rem_rung` wraps it with the [-0.5, 0.5) containment
  premise; `WidenMode::RemRung` selects it. `WidenMode::Level0` is the
  unchanged full Bits(0) widening.
- `WalkOpts::LADDER_WIDEN` (= LADDER + `widen_rem_rung`), threaded through
  `trace_frame` (now `widen: Option<WidenMode>`) and selected in
  `asm_kernel::registry()` / `engine_fingerprint()` when
  `dispatch::widen_in_graph()`.
- The ASM codegen gained interval `Mul`/`Div` by a positive-constant
  scalar (endpoint scaling, matching `Pico8NumInterval::scale_positive` /
  `div_positive` and `graph.eval`) - the rem scale is the only source. A
  frame never scaled an interval before, so existing kernels are
  unaffected. `graph.eval`'s `SplitOk` becomes TOP under narrow-top eval
  (`eval_narrow_top`), so a bare fork sub-DAG is evaluable.
- Gates (all green, full quick suite 306/306):
  - `trace::widen::tests::rem_bucket_node_matches_rem_bucket_exact` /
    `_is_idempotent` / `_forks_a_straddle_into_two_buckets` - the bucket
    MATH, exhaustive over the rem range and every rung (the "widened the
    same" + assert-noop properties at the value level).
  - `asm_kernel_a_vs_b_isolate_the_rem_widening_at_bits2` - the A-vs-B
    differential: A (exact rem + external `make_state_abstract_rem`) ==
    B (`LADDER_WIDEN`, in-graph) after dedup, per frame to f26 (the fork
    fires at f25), Bits(2). Both registries built directly;
    `CELESTE_ASM_NO_SKIP` makes both emit full sets. Catches "widened
    DIFFERENTLY" and the WIRING into the real start-room graphs.
  - `widen_in_graph_is_cheap_in_nodes_at_bits2` - node-count de-risk:
    +3.1% fused nodes (4166 -> 4295), well under the 50% guard. Fusion +
    hash-consing share the rem fork with the frame's arithmetic, as
    predicted.

Not yet done: B is opt-in (default OFF), `stream_boundary` still
double-widens on the kernel path (idempotent). Phase 2 adds spd / fruit /
conservative widenings; Phase 3 flips B on and drops the wrapper widening
on the kernel path; Phase 4 makes the check frontier-symmetric.

### Phase 2 + 4 (2026-08-29, second pass)

Phase 2 - the rest of `make_state_abstract` in the graph:
- `widen()` restructured to COMPOSE rung-independent steps: `widen_rem`
  (Bits(0) constant / Bits(k) fork / Exact no-op), `widen_spd` (the spd
  rung, same bucket-fork shape via `spd_bucket_node`, no-op at Exact),
  `widen_dash` (max(0,.)), `widen_fruit` (`off`->[0,39], `y`->band), and
  `widen_timers`. Level 0 = rem Bits(0) + spd Exact, so it runs exactly the
  old steps in the old order (spd is a no-op) - byte-identical. RemRung now
  emits a FULL fixed point of `make_state_abstract` (rem + spd + fruit +
  dash + timers), so `apply_conservative_widenings` (dash + fruit-off-mod)
  and the fruit `off`/`y` are covered; `erase_provenance_hints` is a no-op
  on hint-less bridge exports and gc is the boundary's `canonicalize_ids`,
  as the plan noted.
- Gates: `spd_bucket_node_matches_make_state_abstract_spd` (value-level,
  every w in 8..=20); `asm_kernel_a_vs_b_at_a_fruit_room_2_0` (the A-vs-B
  on room (2,0), where a fruit is alive from load - frame 1 only, since
  the room explodes without the spd rung and the point is the fruit
  widening, not depth); the room (1,0) A-vs-B now keys A with the FULL
  `make_state_abstract`, so it covers dash + timers too.
- The assert-noop guard (point 1): `dispatch::widen_noop_check()`
  (`CELESTE_KERNEL_WIDEN_NOOP=1`) re-abstracts every widen-in-graph kernel
  output and asserts the row-key set is unchanged (`assert_widen_is_noop`).
  Gate: `kernel_widen_is_a_noop_at_bits2`.

Phase 4 - the check is symmetric under `CELESTE_FRONTIER_ONLY`. The
kernel's `chunk_skip` is a per-chunk-asymmetric optimization on TWO axes -
the frozen frontier AND the within-frame cross-chunk dedup (a sibling
chunk covers a dup) - so "on for both" cannot fix the within-frame half
without cross-chunk deduping the per-chunk reference too. The clean fix
(and what the plan's own "check fix" section recommended) is "off for
both": `chunk_skip` is now auto-disabled whenever
`CELESTE_COMPILED_FORWARD=check`, so the check compares FULL per-chunk
outputs. The skip only saves the export/merge - the campaign boundary
re-does the subtraction - so dropping it in check changes no stored
result. Combined with the widening in the graph (so the SEARCH's own
`chunk_skip` acts on the widened key, the real fix), the Bits(2) "720 rows
missing" artifact is gone. Gate:
`frontier_only_check_agrees_with_widening_in_graph_at_bits2` - the exact
repro config, now green through frame 27. (An earlier attempt filtered the
reference by the frozen frontier; it could not address the within-frame
axis and is not what shipped.)

Phase 3 (make B the default + drop the wrapper widening on the kernel
path) is DEFERRED as a measured follow-up: with B, `make_state_abstract`
is idempotent on the kernel output (the assert-noop property, now
guarded), so the wrapper double-widen is correctness-NEUTRAL - the skip is
a pure performance optimization. It needs (a) provenance threaded into
`stream_boundary_prepare` (which takes a bare State today), (b) the
in-process ladder to rebuild the rung-SPECIFIC registry per rung (the
OnceLock caches one rung's kernels), and (c) a before/after benchmark.
None of it is required for correctness; B fixes the keying bug on its own.

### Phase 3 (2026-08-29): widen-in-graph is the DEFAULT

`dispatch::widen_in_graph()` now defaults ON (`CELESTE_WIDEN_IN_GRAPH=0`
is the kill-switch). The exact-ladder-then-wrapper-widen path is no longer
the default; the ladder set (Bits 1..15) always bakes the widening into
the graph. Safe for the production ladder because `ladder.sh` runs one
rung PER PROCESS, so the rung-specific registry OnceLock is correct.

A/B measured (quick profile, room (1,0), Bits(2), n=33 - RELATIVE only, a
release re-measure is owed before these land in BENCHMARK_DATA.md):

| frame | off (exact+wrapper) | default (widen-in-graph) | delta |
|---|---|---|---|
| f30 | 114 ms | 90 ms | -21% |
| f31 | 211 ms | 131 ms | -38% |
| f32 | 329 ms | 207 ms | -37% |
| peak RSS | 724 MB | 530 MB | -27% |

The win is ROW REDUCTION: the kernel's within-frame dedup collapses rows
on the widened key before they reach the boundary, so fewer rows are
exported, widened, keyed and subtracted. Total wall is ~0.4 s higher only
because `LADDER_WIDEN` has +3.1% nodes -> marginally slower one-time gcc;
over a real f94+ run the per-frame win dominates. This refutes the earlier
"could be slower" caution, which was reasoning (the double-widen does more
work) rather than measurement (the row reduction more than pays for it).

Still redundant: `stream_boundary` re-runs `make_state_abstract` on the
already-widened kernel output (idempotent, guarded by the assert-noop).
Dropping it is a further micro-opt - the big win is captured by the
default flip - and it touches the proof-critical boundary across three
parallel worker paths, so it is a separate, carefully-gated step.
