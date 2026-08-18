# Campaign cost plan (2026-08-18)

Written after the first top-down cost breakdown of the whole pipeline
(BENCHMARK_DATA.md, "Top-down campaign cost breakdown"). Everything here
is priced against measured numbers, not guesses. Setup for all figures:
room (1,0), synthetic win at (64,44), H=72, 30 cores.

Level 0 at H=72: forward 248 s, pos-graph 242 s, sweep 276 s (~12.8 min).
Banded levels ~100-135 s each; a 16-level ladder at ONE horizon ~40 min.

Where the time actually is (phases are inclusive; `fwd.*` inside the
sweep are sub-phases of `bwdt.replay`):

| | forward 248 s | sweep 276 s |
|---|---|---|
| `bwdt.replay` | - | 190 s (69%) |
| ...`fwd.interpret` | 195 s (79%) | 88 s (32%) |
| ...`fwd.merge` | 8 s (3%) | 91 s (33%) |
| `bwdt.keys` / `bwdt.index` | - | 34 s / 25 s |
| boundary stream + save | 31 s | - |

Two conclusions drive everything below:
1. The forward pass is interpreter-bound. The sweep is NOT - it is
   one-third interpreter and one-third merge/regroup, because it
   regroups candidates from many discovery frames into lane groups the
   forward pass never used.
2. The pos-graph stage is pure waste in its default configuration.

---

## ORDER OF WORK (merged with the K4 retirement stages)

The K4 stages (plans/k4-retirement-plan.md) and the P items here are one
queue, because P1's crate split should move only code that survives K4.

| # | item | status | effort | why here |
|---|---|---|---|---|
| 1 | **P0** fused pos-graph by default | DONE | - | -46% of level 0 (task #109) |
| 2 | **P0b** one pos-graph for all k levels (#149) | DONE | - | removed 16 stages/horizon; gated at k=1..3 with two controls |
| 3 | **K4 stage 2** block->State exporter + interpreter fallback | DONE | - | one reference, not two; gates f20/25/30/35 exact, cost is noise |
| 4a | **K4 stage 4 piece 1** stop writing the program body; delete the Engine trait, the scalar runtime, Rt2's execution impl, SplitReq and the concrete probe modes | DONE | - | gen.rs 29,672 -> 3,962 lines; runtime.rs (1,004) gone; name tables byte-identical; gates f20/25/30/35 exact, suite 551/551. Bonus: the crate-wide dead_code allow is gone and `--abstract` now starts from the interpreter's own init |
| 4b | **K4 stage 4 piece 2 + stage 5** emitter is a pure interning walk; slot subsystem and the consumerless tables deleted | DONE | - | transpile 1,125 -> 449 lines, gen.rs 3,962 -> 236; four name tables byte-identical; gates f20/25/30/35 exact, suite 551/551 |
| 5 | **P1** crate split + one frame interface (#150) | | days | the campaign can finally call the kernel; forward AND sweep at once |
| 6 | **K5** kernels for rooms (0,0)/(2,0) | | days | P1's win is room-shaped until this |
| 7 | **P2** the sweep's merge/regroup third (#151) | | days | 91 s of the sweep's 190 s replay; lands on P1's interface |
| 8 | **#114** variant dispatch for pos-graph/sweep | | ? | only matters for rooms that use variants - not (1,0) |

Ordering rationale, in one line each:
- P0b before everything: hours, no new machinery, and it is measured.
- K4 stage 2 before 4 because the fallback must exist before the engine
  it replaces is deleted.
- K4 before P1 so the crate split moves ~5k lines of generated code
  instead of ~34k, most of which is scheduled for deletion.
- K5 after P1 because the interface is what a new room's kernel plugs
  into; doing it earlier means integrating twice.
- P2 after P1 for the same reason.

## P0 - pos-graph fusion by default (hours, -46% of level 0)

`bench --record-pos-graph` records the table DURING the forward pass:
265 s vs 248 + 242 = 490 s at H=72, and the recorded table is a strict
SUPERSET (166,456 pairs / 4,189 cells vs 154,937 / 3,928 - it sees the
spawn's first move, which a replay starting from the frame-1 batch
cannot). A larger table is sound by construction: it only shrinks the
sweep's candidate set, and the EXPANSION is what establishes an edge.

Steps:
1. Run `posgraphcheck.sh` with `ROOM=1,0`. **DONE 2026-08-18, all four
   checks pass**: rows identical (900,028, onlyA=0 onlyB=0), `g`
   IDENTICAL as a function of the row, table a strict superset (+1
   pair), resume correct.
   - The gate was itself broken and said so misleadingly: its row and
     `g` comparisons read `f<H>/visited.bin`, which the DEFAULT mmap
     visited engine no longer writes, so both died on a missing file and
     printed "*** DIVERGED ***" - loud, but for the wrong reason. Fixed
     by pinning `CELESTE_VISITED_ENGINE=map` in the script; the real fix
     is porting tools/rowdiff.py + gjoin.py to the rowkeys sidecars,
     which task #122 needs anyway.
2. Flip ladder.sh's `FUSE` default to on. **DONE**: on for rooms (1,0)
   and (2,0) - the gated ones - and the replay elsewhere with a printed
   reason. FUSE=0/1 still force either path.
3. Gate room (0,0) the same way before its next campaign.
4. Only then consider deleting the standalone replay builder. It is the
   conservative path for an ungated room and for extending a table
   written by an older run, so "the only impl" should wait until every
   campaign room is gated. Cheap insurance; do not rush it.

## P0b - stop rebuilding the pos-graph per k level (hours, -16 stages/horizon)

The pos-graph is HORIZON-independent by construction (pos_graph.rs says
so: it reads no horizon, band or `g`). It should also be PRECISION
independent in the direction that matters: level 0 is the coarsest, so
its reachable-position set is a superset of every k level's, and a
superset is sound. Today ladder.sh rebuilds it per (k,H) - measured
19-22 s x 16 levels x every horizon.

Steps:
1. Verify the superset claim on real data. **DONE 2026-08-18, it
   holds exactly**: level 0 has 166,455 pairs, k=1 has 34,137 and k=2
   has 21,579, and both are strict SUBSETS - zero pairs outside level
   0's table. So the level-0 table can serve every k level.
2. Let `prepare_pos_graph` accept a table whose fingerprint differs only
   in rem/spd precision, with the level-0 table passed explicitly
   (`--pos-graph-from DIR`), rather than loosening the fingerprint check
   globally - the check has caught real mistakes. **DONE.**
3. Gate: `g.bin` identical at k=1..3 with the reused table vs rebuilt.
   **DONE - `posgraphsharecheck.sh`.**

### How the exemption is stated (and why it is not a loosening)

A fingerprint is a hash, so "differs only in precision" cannot be read
off one. The level space is small (17 rem x 14 spd = 238), so the check
ENUMERATES it: `checkpoint::coarser_precision_fingerprints` recomputes
this campaign's fingerprint at every level coarser than or equal to the
current one and requires the table's to be among them. Everything else -
recipe, lua sources, room, chunk caps, frontier-only, synthetic win -
must still match exactly.

"Coarser" is `LadderPrecision::coarser_or_equal`, the PRODUCT order on
(spd, rem), not the ladder's visiting order: `coarsen_to` applies both
components independently, so what is needed is that the source widens
each coordinate at least as much. A unit test ties that predicate to the
widening arithmetic itself (both ladders are floor-aligned power-of-two
buckets, rem at raw width 2^(16-k), spd at 2^w) rather than to the
convention that smaller k means coarser.

### Gate results (room (1,0), H=40, synthetic win at (33,104))

The win is the witness trajectory's position at frame 35, so a concrete
winning path exists inside the horizon at EVERY level - the banded levels
have to win too or the comparison is vacuous. The script checks that
rather than trusting it (level 0 must report an optimum; the first
attempt at (26,108) correctly aborted as vacuous).

| level | own pairs | level 0's | own-only | g rebuilt vs borrowed |
|---|---|---|---|---|
| k=1 | 1,404 | 21,324 | 0 | byte-identical |
| k=2 | 965 | 21,324 | 0 | byte-identical |
| k=3 | 640 | 21,324 | 0 | byte-identical |

Byte comparison is right HERE and only here: both sweeps read the same
checkpoint dir, so row ids are the same assignment (unlike
posgraphcheck.sh, which compares two forward passes and must join through
row keys).

Two controls, because a check that only ever passes is not a check:
* level 0 borrowing k=1's table (the FINER direction, the unsound one) is
  refused;
* a table from a different synthetic win is refused at the same
  precision.

## P1 - one engine behind one interface (days, the big one)

### Stage 1: the crate split. DONE 2026-08-18.

The topology below is built, and it came out as designed. What is worth
recording is what the plan got wrong or did not know.

- **`celeste-names` is 236 lines, not the table zoo below.** `SITE_INFO`,
  `BRANCH_INFO` and `SLOT_*` no longer exist - K4 stage 4 piece 2 deleted
  them with their last consumer. The crate is STRINGS / GLOBAL_NAMES /
  FIELD_NAMES / FN_NAMES plus `global_id` / `field_id`.
- **The profile conflict is resolved twice over.** Per-package overrides
  (`[profile.release.package.celeste-engine] debug = false`) work at the
  workspace root, which was the plan's fix. But they are also no longer
  needed for the reason task #133 recorded: the LLVM DWARF crash was on
  the 29k-line transpiled body, which no longer exists, and the probe
  builds clean under the root's `debug = 1`. They are kept for a
  DIFFERENT, measured reason - see the cost note below.
- **Generated code is checked in, with the witnesses.** `gen.rs` and the
  three `kernel_gen_*.rs` are committed and gated by
  `transpile::names::tests::generated_is_current`, which regenerates all
  four in-process and compares byte for byte. That forced the three
  ~31 KB shape witnesses into git too: they are INPUTS to generation, and
  they are dumps of a checkpoint frame, so without them the kernels have
  no reproducible provenance. `./regen-generated.sh` is the canonical
  regen.
- **The bootstrap is real and needs the script.** The generators live in
  celeste-rust, which will depend on celeste-kernels, whose contents they
  produce - so a change to the emitted PREAMBLE breaks the build of the
  tool that would fix it. It bit once during this work. `regen-generated
  .sh` generates into a scratch dir and only installs what builds.
- **The emitter moved into the library** (`src/transpile/`), with
  `src/bin/transpile.rs` a thin CLI, because the staleness gate has to be
  a `#[test]` and tests can only call library code.

Gates: full suite 539/539; `--abstract 30` lane counts, block counts and
final shape hashes IDENTICAL to the pre-split binary; `--abstract-bench
room10-newlua-bench 35` row-key SET EQUAL (gate 2) with 100% kernel
coverage (steady 114,458 + dash 43,824 + frozen 29,577, missed 0); all
four generated files regenerate byte-identically.

COST, measured, one-frame f35 bench (30 reps, min of run):

| build | min | mean |
|---|---|---|
| pre-split (6ea6937), same machine, same hour | 103.0 ms | 110.5 ms |
| post-split, `debug = false` on the compiled path | 106.8 ms | 121 ms |
| post-split, root `debug = 1` everywhere | 109.9 ms | 119.2 ms |

Two separate things. The +6% from debuginfo is why the per-package
`debug = false` overrides stayed. The residual +3.7% min is the split
itself, and it is NOT at the crate boundary you would guess: folding
`kernel_gen_*` back into `celeste-engine` as `#[path]` modules gives
106.3 ms, i.e. no change, so fat LTO is inlining across the
engine/kernels edge fine. The cost is spread, which is what code-layout
drift looks like. Recorded, not chased: the interface this unblocks is
worth 195/248 s of forward and 88/276 s of sweep.

(Note the recorded 89 ms in BENCHMARK_DATA is not reproducible today -
the same pre-split binary measures 103 ms this afternoon. That is why
this table is an A/B against a rebuilt baseline and not against the file.)

### Stage 2: the frame interface. NOT STARTED.

Still to do: move `frame_step` / `run_chunk_kernel` / the `Fallback` and
the import-export bridge out of native-probe into celeste-rust behind one
`(shape, rows) -> [(shape, rows)]` call, then have both the forward loop
and the sweep's `bwdt.replay` use it.

### Why the split had to happen first

"Make the backward pass use the fast forward" is gated on a structural
fact: **native-probe depends on celeste-rust, not the other way round.**
The kernel engine, the (shape, rows) block model, import,
boundary/dedup/merge and the generated kernels all live in the probe.
The campaign literally cannot call them.

### Topology (Philippe's proposal: a shared crate, generated code apart)

```
celeste-core      pico8_num, cart_data, collision_cache        deps: -
celeste-names     GENERATED name tables: STRINGS, GLOBAL_NAMES, deps: -
                  FIELD_NAMES, FN_NAMES, SITE_INFO, BRANCH_INFO,
                  global_id/field_id, SLOT_*
celeste-engine    Rt2 block model, boundary/dedup/merge/retain, deps: core, names
                  row keys, kernel.rs lane runtime
celeste-kernels   GENERATED kernel_gen_* + their bind/rows/     deps: core, engine
                  append glue
celeste-rust      interpreter, rewrite machinery, transpile,    deps: all
                  campaign bins, and the import/export BRIDGE
native-probe      thin bench/gate binary                        deps: all
```

Two things this gets right, both verified rather than assumed:

- **The engine is interpreter-free.** runtime2, kernel.rs, runtime.rs
  and all three kernel_gen_* reference ZERO interpreter types - their
  entire main-crate surface is pico8_num + cart_data + collision_cache,
  and those three are themselves leaves. `import.rs` is the single
  module naming State/Value/HeapId, so it is the bridge and it belongs
  in celeste-rust, above both. celeste-engine can be State-agnostic.
- **The generated code must be TWO crates, and the graph says so.**
  Today runtime2 (engine) reads `gen::FIELD_NAMES` (canonical field
  ordering in boundary) and `gen::BRANCH_INFO`, while the generated
  kernel_gen_* read `runtime2::{Rt2, Col, AV}`. That is a cycle. Split
  by direction: name tables BELOW the engine, kernels ABOVE it. This is
  forced, not aesthetic - and it is an argument for keeping generated
  code in its own crates rather than folding it into the engine.

### The profile conflict is solved by this shape (VERIFIED)

Task #133 deferred the split on it: a workspace ignores member
`[profile]` tables, the root wants `debug = 1` (perf line attribution
for the interpreter), and the probe MUST have `debug = false` because
rustc's LLVM DWARF variable-DIE pass SIGSEGVs on the fused generated
functions. `lto`/`codegen-units`/`panic` already agree on both sides;
`debug` was the only conflict.

A per-package override at the workspace root fixes exactly that, and it
was listed as untested. Tested 2026-08-18 on a throwaway two-crate
workspace: with root `debug = 1` and

    [profile.release.package.celeste-generated]
    debug = false

the generated crate compiles with NO debuginfo flag while the app keeps
`-C debuginfo=1`. So the generated crates get `debug = false` and
everything else keeps line tables.

### Sequencing: retire before you move

Do K4 stages 2 and 4 FIRST (interpreter fallback, then delete the Rt2
ENGINE and gen.rs's program body). Reasons:
1. `gen.rs` is 29,672 lines of generated program body whose ONLY
   consumer is native-probe's `gen::call_fn`. Moving it into the shared
   graph and then deleting it is pure churn.
2. It shrinks what has to be checked in: the campaign's generated
   dependency becomes the name tables plus ~4.7k lines of kernels,
   instead of 34k lines including a program body we are deleting.
3. runtime2's `BRANCH_INFO`/`SLOT_SHAPE` uses are Engine-execution
   leftovers and may die with it, leaving only `FIELD_NAMES` - which
   makes celeste-names smaller still.

### Checked-in vs build.rs

Check the generated sources in, and gate staleness with a test that
regenerates and compares (the recipe-replay test is the precedent). A
build.rs would have to run `transpile`, which needs the rewrite
machinery, in the middle of building a crate the main crate depends on -
and it would put a ~4 min kernel build in everyone's dev loop. Checked-in
also means a fresh clone builds. Keep the 29k-line `gen.rs` OUT of this
by doing the sequencing above; what gets committed is small.

`transpile` itself has no dependency on generated code (it only emits
text), so there is no bootstrap cycle - it stays in celeste-rust.

### Then the actual point

Define ONE interface, `(shape, rows) -> [(shape, rows)]` for one frame,
and have both the forward loop and the sweep's `bwdt.replay` call it.
The kernel then lands in both at once - that is why this is worth doing
as an interface rather than as "wire the kernel into the sweep".

Coverage stays room-shaped: kernels exist for room (1,0)'s player
classes only; spawn shapes and rooms (0,0)/(2,0) fall back to the
interpreter (which is the reference, so that is safe, just not fast).

Gates: byte-identical checkpoints, `g.bin` identical (tsweepcheck.sh),
row-key set equality per frame, full suite. Nothing here may change a
row.

Known loose end to keep in view: `BUILTIN_NAMES` order is a
hand-maintained ABI shared between src/bin/transpile and the engine's
runtime.rs. The split does not break it but moves the two halves further
apart - it wants an assert or a generated single source.

Expected value: the forward's `fwd.interpret` 195/248 s and the sweep's
88/276 s. It does NOT touch the sweep's other third (P2).

## P2 - the sweep's merge third (days)

`fwd.merge` is 91 s of the sweep's 190 s replay - MORE than the
interpreter there. The sweep regroups candidates from many discovery
frames, so it pays merge costs the forward pass does not. Options, in
increasing order of ambition:
1. Apply the pre-dedup lesson from the kernel work (dedup from keys
   BEFORE materializing rows) to the interpreter's merge path.
2. Preserve the forward pass's grouping for candidates instead of
   regrouping - cheaper merges, and it also shrinks the batch-invariance
   surface the sweep's soundness argument has to cover.
3. `bwdt.keys` 34 s + `bwdt.index` 25 s: the same generated-key idea
   that took the probe's row hashing off the profile.

## P3 - ladder-level waste (unmeasured, list only)

- `--variant` is not accepted by `pos-graph`/`sweep` (task #114), so
  those stages run the base recipe. On room (0,0) pos-graph was the
  largest stage at 3567 s; P0 removes most of that, but the sweep's
  replay still cannot use a variant.
- Each refuted horizon re-runs the sweep from scratch (276 s at H=72,
  and it grows faster in H than the forward does: 20 s at H=60, 160 s at
  H=70). Whether `g` can be extended across horizons instead of rebuilt
  is a research question, not a cleanup - park it until P1/P2 land.

## Suggested order

P0 and P0b first: hours of work, no new machinery, and they remove
roughly half of level 0 plus 16 redundant stages per horizon. Then P1,
scoped honestly - the crate move is the gating cost, not the kernel. P2
after P1, because P1's interface is where the merge work will land
anyway. P3 is a list, not a plan.
