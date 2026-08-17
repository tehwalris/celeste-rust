# Morning report, overnight 2026-08-17 -> 08-18

Everything below is committed and pushed on `census` (eb0ec1f ..
HEAD). Every claim has a measurement behind it; the two meaning-bearing
oracles (gate-1 lane counts, hex-prefix vs concrete_run) gate every
commit. Read plans/columnar-engine.md for the full engine story.

## Landed

1. **ZERO DIVERGENCE** (760c8a6): 9 -> 0 divergent branches on the
   compiled room (1,0) shape. The missing cse mode was not aliasing -
   forward mode was deliberately block-local; the new opt-in
   `cse {forward, cells}` lets alloc-cell load pairs cross block edges
   (old entries replay byte-identically; 5 new unit tests). Then:
   4 iterator cells dropped, freeze gate -> masked region (34 changes,
   7 shadows died), dash trigger -> one speculate_region {mask, expand}
   triangle (57 changes; needed one demote_create + drop_dead_cell, no
   per-store stage-D). Census: 0 divergent of 26 executed, 0 multi-
   receiver heap sites, 178 single-receiver slots, 0 panics. Suite
   527/527, verify-40 identical, all recipes replay.

2. **Oracle correction** (same commit): the old "hex-identical 400
   frames" claim for the compile recipe was overbroad - the seed-7
   tape's death frame (341) exits the shape domain with a loud
   `count(objects)==1` assert AT HEAD TOO (same frame, same assert).
   Corrected doctrine: hex-exact through the premise-holding prefix
   (f340 re-verified) + loud same-frame domain exit. Full-tape oracle
   returns when the probe grows the deopt fallback.

3. **Engine trait** (one generated program, two runtimes): transpile
   emits `f_N<E: Engine>`; the scalar concrete probe implements it
   unchanged - and got FASTER: 2922 -> 1933 ns/frame (inline(always)
   delegation). Hex oracle green throughout.

4. **THE COLUMNAR ABSTRACT ENGINE** (the night's centerpiece,
   plans/columnar-engine.md): zero divergence makes the abstract
   engine columnar - no DFS, no choice tapes, no continuations. Lanes
   are columns over a shared uniform heap structure; expand/splits
   append lanes mid-flight; boundary = ported rem widening + clamps +
   timer pins + canonical BFS compaction/renumbering + 128-bit row
   dedup. **Gate 1 is EXACT: per-frame lane counts equal the
   interpreter's for all 30 frames** (1x23, 24, 204, 878, 2864, 7260,
   15250, 27024), spawn transition and post-dash freeze mixing
   included.
   - Two real divergent gates surfaced that the f35 concrete census
     could not see (both frame-start data): the update-side freeze
     gate and the `spd ~= 0` moving gate. Handled by frame-start
     pre-partitioning + a generic SplitReq partition-and-rerun
     mechanism. Lesson recorded: the zero-divergence census certifies
     per-(state, input) uniformity at its window, not cross-state
     uniformity inside an abstract block.
   - Perf ladder (f30, 27k boundary lanes, ~1.7M offered): naive 6.6s
     -> chunked (64-lane cache blocking) 2.4s -> +typed Num columns
     2.12s serial; 1.24s on 30 cores; 30 frames in 2.13s wall.
     Interpreter reference: 0.08s/frame single-core (8.2 us/lane).
     HONEST: still ~11x off the interpreter per boundary lane serial;
     already faster per OFFERED lane (1.4 us) than the scalar concrete
     probe (2.0 us) while computing abstract semantics. Next lever
     written down: per-op time census, then fused loops.

5. **300m projection** (plans/columnar-engine.md): GO, engine-first.
   The engine removes the OOM wall (intra-frame peak becomes
   chunk-local; the 2px S-rung hump that OOM'd at 57.6 GB is ~1 GB
   under it) but does NOT fix the 1.10x/frame lane growth of exact
   level 0 - the S-rung ladder stays the algorithmic answer, repriced
   ~10x+ cheaper. rem-banding stays parked unless the repriced S-rung
   still fails h=95.

## Not done (deliberately, with reasons)

- **Crate restructure (goal 3)**: not started - the engine needed to
  exist first, and starting a workspace split at 5am with the engine
  mid-flight would have risked everything else. The shape is written:
  celeste-core (lib) / celeste-engine (generated + runtimes) /
  celeste-cli (bins). It is the next mechanical unit.
- **Gate 2 (row-SET equality)**: gate 1 (counts) passed everywhere;
  the State-reconstruction path for set equality is designed
  (import.rs reversed) but not built. Counts + deterministic per-lane
  dynamics make silent set divergence unlikely but NOT proven - do not
  treat gate 2 as done.
- **Fruit widenings, death deopt**: needed before the engine touches
  rooms (2,0)/(0,0); reads are catalogued (abstraction.rs:590-704,
  748-818).
- **simdcheck-style batching-invariance certificate for the columnar
  engine**: chunking passed gate 1 at several chunk sizes (64/128/256/
  512), which is evidence, not the certificate.

## Reversible design calls made tonight (review these)

1. Columnar-over-DFS architecture (plans/columnar-engine.md) - the
   central bet; gate 1 says the semantics are right.
2. `cse {cells}` semantics (alloc-cell pairs cross edges) - argued
   sound because an alloc names exactly one cell; unit-tested.
3. Whole-block SplitReq rerun on divergence instead of minority
   filtering (v0 simplicity; frequency logged).
4. Probe switched to mimalloc; rustc-hash + serde_json deps added to
   native-probe.
5. The morning docs treat the zero-divergence census as state-window-
   scoped evidence, not a global property.

## Late addendum (post-report work)

- Per-op census landed (`CELESTE_OP_CENSUS=1`): WIDEN is 48% of the
  serial abstract run - the lane-append at expand/split sites copying
  every live varying column. Eager btn expansion measured out (frame
  runs 64x wide; reverted with note). The named fix is COW/
  lane-indirection columns (plans/columnar-engine.md, census section).
- Final suite run of the night: 527/527.

## Where the next session starts

plans/columnar-engine.md "Typed columns, measured" bottom: per-op time
census inside the engine, then fused loops OR the crate restructure
(goal 3), whichever the morning review prioritizes. The gap picture
target remains: within ~1.2x of hand-tuned per phase, or a named
transformation for each remaining gap.
