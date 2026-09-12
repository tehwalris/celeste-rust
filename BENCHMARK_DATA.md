# ASM kernels vs the deleted Rust lattice kernels, room (1,0) forward @ f94 (2026-08-29, release)

First perf number for the ASM backend after the append dedup fix. Room (1,0)
forward to f94, `celeste-rust --rewritten -n 94`, `CELESTE_COMPILED_FORWARD=1`
`CELESTE_FRONTIER_ONLY=1`, release. Ends at **5,949,326 expanded** - identical
to the LATTICE-kernel baseline below, i.e. the SAME search.

| backend | forward wall | note |
|---|---|---|
| LATTICE Rust kernels (2026-08-26, `bench --frames 94 --deopt`) | 175.73 s | the deleted set |
| **ASM (this run)** | **155.0 s** (frame-sum; +~4 s startup retrace) | at parity - slightly FASTER |

So the ASM backend is AT PARITY with (a touch faster than) the Rust kernels it
replaced - comfortably inside the ~1.3x Philippe budgeted, and the compile-time
win (gcc-assemble a graph in ms vs a >30 min fat-LTO relink of 1.3M lines) is
free on top.

The append dedup (2a41ba9) is what got it there: before it, the generic append
over-materialized ~76:1 (materialize-then-boundary-dedup) and the forward ran
~2.3x the Rust kernels; porting the Rust `seen` (h1,h2) fold to skip
duplicate rows before materializing cut a heavy frame from 2.77 s to 0.95 s.
Residual over-materialize is still ~10:1 - CROSS-CHUNK within-frame dups the
per-chunk `seen` does not catch (the post-boundary frozen-frontier skip drops
them, but after materializing). Closing that (a shared within-frame set keyed
by the real boundary key) is the next perf lever if wanted; we are already at
parity without it.

CAVEATS: not a perfectly clean A/B - this run has no `--deopt`/collect-first
(the baseline did; ASM needs neither, it covers every shape), the machine was
not certified idle, and 155 s is the frame-sum not total wall. But the search
is byte-identical (same final expanded count) and the direction is
unambiguous: parity or better.

> **ASM cutover note (2026-08-29).** The generated Rust kernel crates,
> `regen-generated.sh` and the whole checked-in-kernel workflow are GONE
> (plans/asm-and-posgraph-execution.md B); the kernel backend is now the
> runtime-assembled AVX-512 set (`compiled::asm_kernel`, retrace at
> startup, `CELESTE_NO_ASM_KERNELS` opts out). Entries below that measure
> "traced"/"lattice" KERNELS measured the deleted generated-Rust set;
> they stand as history, but no kernel-side number below has been
> remeasured under the ASM backend yet - do that before quoting one as
> current.

# WHOLE room (1,0) campaign via the in-process `ladder` (2026-08-28, QUICK profile, interpreter forward)

Tonight's run of the NEW single-process ladder: `rewrite ladder --from 94
--to 94 --maxk 2 --room 1,0` (commit fe1822f). Three things make this NOT a
clean A/B against the block below - read the caveats before quoting a
speedup:

  1. **QUICK profile, not release.** Philippe's call: skip the fat-LTO
     relink until the kernels are asm-only. Absolute numbers run a bit hot
     vs release, but the win is large enough to record anyway (labelled).
  2. **INTERPRETER forward, not the compiled/lattice engine.** The compiled
     forward currently writes a checkpoint the sweep cannot read (engine key
     != sweep's row_keys; see plans/pos-graph-and-memory.md 5), so the
     ladder runs the interpreter until that is fixed.
  3. **This session's work-stealing + parallel-merge** speedups (commits
     e6636d1 / fbebb72 / 7a14292) are in, and they apply to the interpreter
     path too.

| stage | wall | peak RSS | note |
|---|---|---|---|
| l0 forward (+save-frames +fused pos-graph) | 382 s | 24.4 GB | win seed at f89 |
| l0 backward sweep | 176 s | 27.9 GB | 178,576,090 rows, 2 win seeds in B(94), e+g optimum 89 |
| k=1 banded forward | 7 s | 26.1 GB | k=1 WINS |
| k=1 backward sweep | 22 s | 18.5 GB | 1,163,134 rows, e+g optimum 93 |
| k=2 banded forward | 1 s | 15.9 GB | H=94 REFUTED at k=2 |
| **TOTAL** | **589 s** | | |

Verdict BYTE-for-the-answer identical to the block below and to ladder.sh:
level 0 wins at f89, k=1 wins at f93, k=2 refutes H=94. Every row count and
win-seed count matches to the digit - the in-process ladder reproduces the
whole forward+sweep+banded pipeline exactly.

Wall: 589 s total vs the release/lattice 676 s below - faster DESPITE quick,
because work-stealing + parallel merge more than pay for the quick penalty
(the f50 forward frame alone went 5.03 s -> 2.89 s, -43%, this session). But
the axes are confounded (profile AND engine AND work-stealing all differ), so
this is a "whole campaign is ~10 min and correct", not a clean per-lever
number.

PEAK RSS is HIGHER here (24-28 GB vs 9-19 GB below), for two reasons, both
noted for room (0,0) where it would matter:
  - SINGLE PROCESS: the ladder runs every stage in one process, so glibc
    keeps each stage's arenas (the sweep peak includes the forward's
    retained memory). ladder.sh runs separate processes precisely for this
    isolation; room (0,0) needs that, so the in-process ladder is a
    room-(1,0)/(2,0) tool until it can spawn per-stage subprocesses.
  - the work-stealing forward's per-frame (vs per-batch) filtering raised
    the forward transient (9.28 -> 24.4 GB); a super-batch fix is planned
    (plans/pos-graph-and-memory.md 4).

# WHOLE room (1,0) campaign on LATTICE kernels, fwd+bwd+FUSED pos-graph (2026-08-26, idle, KERNELS=1 ladder.sh 94 94 2)

Step 4 of the overnight plan, room (1,0), the full precision ladder on the
lattice kernels: forward + backward sweep + the FUSED (side-effect) position
graph - `pos-graph: recorded IN the forward pass`, NOT the dedicated pass, as
Philippe asked. Every stage on lattice kernels, strict, MISSED 0. Scratch
checkpoints, MEM=60G.

| stage | wall | peak | note |
|---|---|---|---|
| level-0 forward (+save-frames +fused pos-graph) | 489 s | 9.28 GB | win seed at f89 |
| level-0 backward sweep | 161 s | 19.17 GB | 178,576,090 rows, 2 win seeds in B(94), e+g optimum 89 |
| k=1 banded forward | 16 s | 9.10 GB | k=1 WINS |
| k=1 backward sweep | 9 s | 1.04 GB | 1,163,134 rows, e+g optimum 93 |
| k=2 banded forward | 1 s | 0.16 GB | H=94 REFUTED at k=2 |

Result: level 0 wins at f89, k=1 wins at f93, k=2 refutes H=94 - BYTE-for-the-
answer identical to the ladder subagent's earlier non-lattice run and to the
interpreter (the per-chunk check gate proved that chunk by chunk). So the
whole forward+backward+fused campaign runs correctly on the lattice kernels
with the interpreter reference-only.

FINDING - the fused pos-graph is currently EXPENSIVE, not cheap. The level-0
forward is 489 s here vs 175 s for pure `bench --frames 94` (no pos-graph, no
save-frames). Part is `--save-frames` I/O, but the origin-attributed
pos-observation on kernels carries real overhead (the origin subagent measured
the kernel pos-graph replay ~1.2x the interpreter's). The dedicated pass was
~137 s as a SEPARATE stage, so fused-forward (489) vs forward(175)+dedicated
(137)=312 currently favors the DEDICATED split on wall clock. Fused is the
right architecture (one pass, Philippe's call) and it is CORRECT here; it just
needs perf work before it is also cheaper. Not a soundness issue - a perf TODO.
The level-0 sweep peak (19 GB) is also the campaign's high-water mark.

# LATTICE kernels vs plain traced, room (1,0) forward @ f94 (2026-08-26, idle machine, fat-LTO release)

The constant-lattice kernels are now the SOLE runtime set (all rooms/variants,
strict-by-default). First fair benchmark of them: room (1,0) forward, the
production horizon, ladder env (frontier-only, collect-first, 8000-lane caps),
`bench --frames 94 --deopt`, `CELESTE_COMPILED_FORWARD=1`, idle machine.

| set | wall | us/lane | peak | missed | lanes |
|---|---|---|---|---|---|
| plain traced (baseline 2026-08-24) | 340.91 s | 57.3 | 8.18 GB | 0 | 5,949,326 |
| **LATTICE (this run)** | **175.73 s** | **29.5** | 11.45 GB | 0 | 5,949,326 |

**~1.9x FASTER** (29.5 vs 57.3 us/lane), IDENTICAL search (172,626,763 kernel
lanes both, win at f94, missed 0, plain-routed 0 - pure lattice, no
interpreter). And this is a LOWER BOUND on the lattice speedup: the run also
carries the new per-lane origin metadata (mixed into dedup + row keys), which
ADDS work vs the baseline. So the constant-lattice specialization roughly
halves the per-lane forward cost on room (1,0) - a room that had no spurious
forks to begin with (the win is from baking constants + smaller kernels, not
fork elimination).

Tradeoff: peak RSS is HIGHER, 11.45 vs 8.18 GB (+40%). Partly the origin vector
(a per-lane u32 + wider dedup/row keys, added since the baseline); a plain-set
run at current HEAD also showed ~11.5 GB, so it is NOT lattice-specific.
Attribute + measure separately before calling it a lattice cost.

Caveat: the fat-LTO release LINK of the 1.34M-line kernels crate is the cost of
this set (>30 min); see plans/kernel-ladder.md - crate-per-room split proposed
if it starts hurting the loop.

# The origin passthrough: sweep + pos-graph on kernels (2026-08-26)

The backward sweep and the pos-graph recording were the ladder's last
interpreter dependency: their per-lane origin column (a heap global) kept
every kernel from binding, so `KERNELS=1` dropped the engine for those
stages. The origin is engine METADATA now (`Rt2::origin`,
plans/kernel-ladder.md "the passthrough column"): the bridge moves the tag
global into block metadata on import and back on export, the generated
`append{i}` records each row's input-lane origin, and the origin is mixed
into every dedup key so distinct origins never collapse - the exact
semantics the injected global had. `interpret_origin_replays` is deleted;
`ladder.sh KERNELS=1` no longer forces `FUSE=0`.

GATE (room (1,0), synthetic win (64,44), H=68, 16 threads; ONE
kernel-strict forward pass as the fixture, then each replay stage run
twice on copies - interpreter (`CELESTE_KERNEL=0`) vs kernels (strict,
missed 0)):

| stage | interpreter | kernels | artifact |
|---|---|---|---|
| `pos-graph --frames 68` | 141,236 pairs, 136 s wall | same pairs, 167 s | `posgraph.bin` **byte-identical** |
| `sweep --frames 68 --horizon 68` | 511,124 of 55,958,742 reach the exit, optimal 64, 75 s | identical, 64 s | `g.bin` **byte-identical** |
| `bench --record-pos-graph` (fused, kernels) | - | 174 s | row table **identical** (onlyA=0 onlyB=0), table = replay's + the 1 spawn pair |

Walls are single runs, not claims - the point of this campaign is the
interpreter OUT of the ladder, not speed. Two honest observations:

* The kernel pos-graph replay is ~1.2x SLOWER than the interpreter path
  here. Origin-tagged chunks dedup per (origin, row), so the in-kernel
  dedup - the engine's main lever - finds almost nothing, while the
  bridge (import + export + tag re-injection) is paid in full. The sweep
  is ~1.2x faster on kernels. Nobody should quote either without a
  proper A/B.
* `out_of_table`: 7,972,684 on the interpreter replay of this
  KERNEL-BUILT forward pass, 0 on the kernel replay. The interpreter's
  UnknownBool handling is lane-grouping-sensitive, and the sweep's
  candidate gather groups lanes differently from the engine's forward
  partition, so the interpreter replay reaches (widened) rows the
  engine's table never contained - the documented benign direction, and
  `g.bin` identity shows no qualifier was affected. The kernels are
  lane-wise, so their replay lands exactly in the table.

Unit gates: `kernel_replays_carry_origins_like_the_interpreter`
((origin, row key) pair-set identity against the interpreter, both the
sweep's distinct-id tagging and the recorder's repeating-cell tagging,
strict + coverage asserted) and
`check_mode_compares_origin_pairs_without_false_alarms` (check mode now
compares pair sets when a tag is present).

# (2,0) dying members straightened + kernels wired; fused set blocked on corpse-gate blending (2026-08-21, afternoon)

Both (2,0) dying overlays are now BRANCH-FREE (0 conditional branches,
0 rolled loops in `__frame`) and membercheck-certified: fall applies on
[47], spikes on [64], observations identical to the plain program
(`CELESTE_START_ROOM=2,0` is REQUIRED for these memberchecks - without
it the (1,0) trajectory replays and the member is vacuous, which cost
half a morning of false alarm). Yesterday's "label ambiguity" diagnosis
was wrong: the transplanted unroll bound the RIGHT loop; the trip was 4
where the post-kill h061 check loops run at 3 (the kill fires at the top
of player.update and the update continues on the dead object).

Machinery this took, all landed with suite 555/555:
- `unroll_loop` `invert` flag: exit-on-true heads (`br i>#t ? exit :
  body`, the inlined-`del` scan), continue-condition = negated compare,
  and suffix trip-guard placement when the head itself recomputes the
  bound. Historical prefix layout untouched (committed kernels stay
  byte-stable).
- del-scan straightening sequence (both overlays): pin sentinel head
  true + not-found gate true + shift arm false + found-at-end true,
  merge, `speculate` the arm's `bool true`, `absorb_stores` the found
  diamond into select-stores, `dce` the dead shift arm, then
  `unroll_loop` trip 4 invert. `membercheck --trace-frame` is the
  derivation tool: it prints the implied pin per site; NOT PINNABLE
  sites are exactly the per-iteration ones the unroll resolves.
- Kernels emit from both overlays (126/134 KB, 408 witness facts, the
  a9e20c0a engine-aggregate witness) and are wired into dispatch as
  class `dying` (mask bit 8, `CELESTE_KERNEL_CLASSES=dying`);
  regen-generated.sh covers them (`R20_CLASSES`).

Measured outcome at f42 (`=check`, honest recipe): gate PASSES, 19.34 s
(vs 19.69 s yesterday), sets identical - but the standalone dying
kernels bind **0 lanes**, and that is structural, not a bug: chunks are
partitioned by (shape, pm1), and a chunk mixes lanes that survive the
frame with lanes that die in it, so no chunk-granular kernel can accept
it (steady's no-kill guard fails on the dying lanes, dying's kill guard
fails on the rest). Per-lane death selection is the FUSED kernel's job
(guard-as-selector, #163/#164). The (2,0) fuse attempt:
- census: steady 1396 nodes, union {steady, dy-spikes, dy-fall} 2348 =
  **+68% over steady alone** - the v3-stage number ((1,0) was +5.5% at
  v3, +0.5% after the v5 corpse-gate blending);
- `--fuse` refuses outright: "member dying-spikes fork skeleton differs
  from the primary's" - the spikes suffix observes kb4/kb5 (the corpse
  jump/dash gates are PINNED, steady BLENDS them), the same kb5 gap the
  (1,0) campaign closed with the corpse dash-start blend.

So the next tranche is exactly the (1,0) v5 playbook on (2,0): blend
the corpse gates (dash-start, wall-slide analog, and whatever else the
fork-skeleton diff names) in both dying overlays, re-certify by
membercheck, then `--fuse` and gate. Until then the ~787K dying-adjacent
lanes stay on the all-or-nothing interpreter fallback and the engine's
f42 numbers are unchanged from the morning's adoption.

# Room (2,0) engine adoption: kernels from engine witnesses + state-level fallback (2026-08-21)

The r20 class kernels now bind for real, and the engine is at parity-plus
on (2,0) at f42. All runs: `--recipe rewrites-room20.jsonl bench
--frames 42 --deopt`, `CELESTE_START_ROOM=2,0`, fresh campaigns, this
machine. THE RECIPE MATTERS: earlier "gates" ran the default (1,0)
rewrites.jsonl, whose devirt premise fails on fruit.update in every
unfrozen (2,0) frame - the reference premise-deopted the whole
steady/dash population and only the frozen slice ever reached dispatch.

| run | wall | peak RSS | us/lane |
|---|---|---|---|
| interpreter-only | 11.44 s | 3.02 GB | 6.3 |
| engine `=1` | **10.94 s** | **2.87 GB** | 6.0 |
| `=check` (both + per-state set compare) | 19.69 s | 3.26 GB | 10.8 |

The `=check` gate PASSES: row-key sets identical on every checked state,
42 frames. Kernel lanes over the run: r20-steady 1,786,320 + r20-dash
682,272 + r20-frozen 515,409 = 2.98 M bound; 1.03 M missed (74%
coverage of dispatched lanes). The missed tranche is f39+ fruit-touch:
786,848 lanes are guard/bind refusals ON the covered a9e20c0a shape
(class-leaving, bounce, dying) and 239,814 are the 409-cell spring-delay
residual shape 681d - both are the next coverage tranche, recorded by
CELESTE_KERNEL_MISS_DUMP.

Witness provenance changed: the r20 witnesses are now DUMPED FROM ENGINE
CHUNKS (dispatch's CELESTE_KERNEL_MISS_DUMP aggregate), not emitted from
interpreter-saved reference frames - the engine's real shape is 408
cells (no spring `delay` cell) with ival fruit off/y + player rem, and
reference-frame witnesses bound zero engine lanes.

Fallback doctrine, learned expensively (three frame-f40 multi-hour
grinds): a missed chunk must NOT be interpreted in engine-partitioned or
bridge-roundtripped form - per-chunk compile-program, per-chunk
campaign-program and re-vectorized-batch fallbacks all collapsed in
split_by_condition on lane groupings the campaign's own flow never
forms. The fallback is ALL-OR-NOTHING per state: any miss discards the
engine's partial work and the ORIGINAL campaign state runs under the
CAMPAIGN program (run_frame_chunk's `campaign` parameter). Uncovered
states thus cost interpreter + wasted kernel work; covered states are
pure engine. Check mode also carries the REFERENCE states forward, so an
engine partition pathology cannot compound across frames.

Room (1,0) regression: `=check` f42 13.00 s, sets identical.

# Stage-2 hill-climb session 1 (2026-08-17 evening)

Real-frame bench (f35, 187,859 rows, 29 cores): 607 -> **520 ms**
(2.77 us/row); from-scratch `--abstract 40`: 9.80 -> **8.28 s**. Steps
and gates in plans/columnar-engine.md (inline/outline op split,
val_dirty undo log, append memcpy, transpiler block-scoped locals;
CHUNK=64 confirmed optimal by sweep). Measured ground truth: the
scalar concrete frame retires 25k instructions at IPC 3.48 (1.37
us/frame) - branch-free executes both arms of every former branch, so
the "count the instructions" floor is 25k/frame amortized over lanes,
not ~250. Remaining profile: f_15 20%, boundary 17%, append 16%,
select 8% - next rungs are typed 8-byte values, Col::B through Rt2,
row batching.

# Engine vs production interpreter, same day, same machine (2026-08-17)

CORRECTION of a wrong claim made mid-day ("the interpreter campaign
is far behind both"): it is not. `rewrite bench --frames 40` on
cores 0-14/16-30: **4.02 s total, f40 frame 0.94 s (1.4 us/lane
wall), peak 1.6 GB** - 2-3x FASTER than the engine's 10.40 s / 3.08 s
at the same exact lane counts. The interpreter's edge is its
fragment/pm1 partitioned representation (354 mostly-uniform fragments
per frame vs the engine's one wide block per shape). The engine's
measured edge is memory locality (intra-frame peak is chunk-local)
and its Rt2-relative kernel speed; see the 300m re-projection in
plans/columnar-engine.md for what this means for room (2,0) - short
version: the engine un-parks the S-rung ladder by removing the
57-97 GB intra-frame OOM wall (frontier rows are ~50-100 typed
B/lane), while raw per-lane time is currently at parity, pending the
pm1-partition representation fix in frame_step.

# Goal-7 measurement + slotless tiles at depth (2026-08-17; plans/columnar-engine.md)

Per-phase timing of `frame_step` (`CELESTE_PHASE_TIME=1`), from-scratch
`--abstract 40`, 30 cores. The goal-7 premise ("serial
boundary/merge/dedup dominates at depth") is REFUTED: at f40 (673k
input lanes -> 902,280 out) the serial epilogue (part + dedup + retain
+ merge) is ~650 ms of the 10.3 s frame; the parallel run phase is
9.66 s = 94%. The 31.5 s wall was Rt2 KERNEL time: the tile shape gate
(slot binding scoped to the census shape 0xc51b...) rejected all
48,767 steady-shape chunks because from-scratch runs reach a DIFFERENT
steady shape (0x893c...), so every engine config fell back to Rt2.

Fix: slots are measured time-neutral, so `gen.rs` is now generated
WITHOUT `--site-slots` (N_SLOTS=0) and the gate - already conditional
on `N_SLOTS > 0` - opens. Tiles then carry the whole depth run, spawn
shape included (0 bails, 0 gate rejects, mode 2):

| from-scratch `--abstract 40`, 30 cores | 40 frames | f40 frame | f40 run phase | f40 serial epilogue |
|---|---|---|---|---|
| with-slots gen.rs (all engines = Rt2 past the gate) | 31.57 s | 10.31 s | 9.66 s | 0.65 s |
| slotless gen.rs, CELESTE_TILE=2 | **17.55 s** | **5.20 s** | 4.84 s | 0.36 s |

EXACT: every frame's lane count equals the Rt2 reference (f40 =
902,280). Scalar probe stays hex-exact (30f vs concrete_run); suite
527/527. Slotless bench (below) unchanged: mode 2 min 1040.7 ms vs
1041 with slots; Rt2 2030 ms; mode 1 4373 ms with 222 truthy_b bails
(divergent branch truthiness across a concrete-button tile; sound -
bailed chunks rerun on Rt2, oracle still exact).

Even under tiles the run phase is 93% of a deep frame, so the next
lever stays the kernel/per-chunk-boundary, not the serial epilogue.

Follow-up the fresh profile found immediately: `bi_tile_flag_at` was
19% of the kernel, and `perf annotate` put nearly all of it on the
`lock incq/decq` pair of `self.cache.clone()` + `self.cart.clone()` -
29 threads bouncing two Arc refcount cache lines once per builtin
call. Rewritten as an inline all-Num pane->mask loop with only
immutable borrows (no Arc clones; (wi,hi) map dispatch hoisted out of
the lane loop): bench **1031 -> 639 ms** (3402 ns/input-lane, exact),
mode 1 4373 -> 3776 ms, from-scratch `--abstract 40` 17.55 ->
**10.85 s** (f40 frame 3.17 s), all lane counts exact, scalar
hex-exact. Post-fix profile: f_15 glue 17%, select 14%, per-chunk
Rt2::boundary 12.6%, append_into 12.3% - flat memory-bound work, no
contention pattern left in the top symbols. Lesson recorded: in
per-call helpers on the hot path, Arc clones are NOT free under
parallelism - profile attribution showed the cost inside the callee,
not at the clone site.

Second step: select with a B-mask condition and Bool/B arms is now a
pure bitwise combine (`cm&tm | !cm&fm`) instead of the per-lane AV
pick: bench 639 -> **607 ms** (3230 ns/input-lane), mode 1 3776 ->
2064 ms, `--abstract 40` 10.42 s. All exact + scalar hex-exact.

Ladder tail (2026-08-17 profile, 607 ms bench): f_15 17% (diffuse
16-byte TCol stack traffic across the huge straight-line generated
fn - flat in annotate, max 5% on one instruction; the fix is
codegen-structural, #132 row-struct emission), Rt2::boundary 13.6% +
append_into 13.5% (typed streaming loops, real data movement; a
Col::B bool column through Rt2 would shave some), select residual
10% (num blends - real work). No cheap wins left in the top table;
next units are structural.
Re-arming slots (a future typed-slots campaign) means regenerating
with `--site-slots plans/site-slots-room10-f035.json` - that closes
the gate to one shape again, so it must come with a census for EVERY
shape the run visits, or stay off.

# Where the 52:1 kill ratio comes from (2026-08-19)

`CELESTE_DEDUP_CENSUS=1` splits the frontier's kill ratio into the part
that dies against rows THIS frame produced and the part that dies against
history. The two want completely different machinery, and the answer is
lopsided. Room (1,0), H=64, ladder level-0 env; distinct counts are a
1/64 sample scaled up (exact would need 4.6 GB of keys at f68).

| frame | offered | ~distinct in-frame | new | within-frame | cross-frame | global probes |
|---|---|---|---|---|---|---|
| f60 | 107,694,485 | 3,729,472 | 2,199,846 | 28.9:1 | 1.7:1 | 32,242,294 |
| f62 | 134,329,805 | 4,958,208 | 2,802,179 | 27.1:1 | 1.8:1 | 41,312,314 |
| f64 | 190,597,048 | 6,827,968 | 3,850,608 | 27.9:1 | 1.8:1 | 58,154,366 |

**~28:1 of the 52:1 is within-frame; only ~1.7:1 is against history.** The
ratio is stable from f50 on (31.8:1 at f50 drifting to 27.9:1 at f64), so
it is a property of the search and not of the depth.

That decides the architecture. The first stage's working set is ONE
FRAME'S DISTINCT ROWS - 6.8M keys at f64, 109 MB raw, and hash-partitioned
across 16 workers 425k keys or 6.8 MB each, which is an L3 resident, or
under 1 MB with 2-byte fingerprints, which is L2. It is not the 56M-row
global set. Only what survives that stage - 6.8M rows, 3.6% of the
offered stream - has to touch the mmap'd structure at all, and it dies
there at a mere 1.7:1.

**The gap is measured, not hypothetical.** A local `seen` set already
exists in `visited_row_keys`, but it is per-FRAGMENT, and a frame has
~720 fragments. So it takes 190.6M offered rows down to only 58.2M global
probes, and **8.5x of those probes are redundant** - the same key, already
probed by another fragment of the same frame. A frame-wide or worker-wide
tier removes that 8.5x directly:

```
  190,597,048  offered
   58,154,366  global probes today   (per-fragment seen set: 3.3x)
    6,827,968  distinct in-frame     (<- what a frame-wide tier would leave: 8.5x fewer)
    3,850,608  new
```

# Dying members GATED: spikes + fall specialization recipes (2026-08-19)

Phase B step 1 of plans/shape-tag-plan.md (#159/#160), commit 7f0adf0.
Two specialization-set members for the [player] shape, each derived as
rewrites.jsonl minus the death-falsified premises (h010-h019
collapse_loop + h028-h037 assume_eq, all post-kill in player.update_21)
plus a guard_branch tail pinning the kill:

| member | guard tail | pinned premise |
|---|---|---|
| rewrites-dying-spikes | and_or_join_18 taken | dies on spikes this frame |
| rewrites-dying-fall | and_or_join_18 not taken + if_join_21 taken | falls out (y>128) this frame |

Both causes are real in room (1,0): up-spikes at x in [64,88) y=104 and
an open bottom for x>=80 (map row 15 non-solid there). Checked-in TAS
witnesses die during f92 (spikes) / f101 (fall).

Gates, all passed:
- `rewrite membercheck` (NEW; `verify` cannot run a member - its guard
  asserts a premise most frames falsify, and verify has no deopt):
  concrete TAS replay, member attempted per frame from the plain
  pre-state through StateMapping. Each member applies on ITS dying frame
  with an identical observation, and SKIPS the other member's dying frame
  at the discriminating guard - the complementary-pair exhaustiveness
  argument observed concretely. 4 runs (2 members x 2 death TASes).
- Abstract set gate: bench resumed f065->f068 from the k2ctl checkpoints
  with `--variant 'player=<member>'`, per-frame rowkey sets of f066-f068
  IDENTICAL to k2ctl for both members (counts 4,591,412 / 4,756,768 /
  4,980,465; full sorted-set sha256 equal). 43 s per gate run.
- Suite 553/553 (checked_in_recipe_tests replays both members).

What the gate run also showed: variant dispatch is CHUNK-granular. A
mixed alive+dying chunk fails the member's guard and falls back to the
base program whole, where the dying lanes then deopt exactly as before
(399,516 - unchanged, as expected). The lanes that DID run under the
members (47,106 over f66-f68) are the premise-trivial frozen chunks.
Lane-granular member execution - the actual deopt kill - is what the
fused engine's per-lane selector delivers; these members are its
verified inputs, not themselves a campaign optimization.

# Fusion sharing census: 3 members fuse to +5.5% of steady alone (2026-08-19)

Value-numbering census over the emitted kernels (leaves = u.cN / rin.cN /
kbK / constants / fork configs; dp side-args stripped; sequential
canonical numbering, so one divergent leaf poisons its whole cone -
i.e., these are LOWER bounds on shareable work):

| members | nodes | distinct |
|---|---|---|
| steady (checked in) | 1286 | 871 |
| dying-spikes (aligned, derivation 3) | 1160 | 784 |
| dying-fall (aligned) | 1160 | 784 |

| intersection | shared |
|---|---|
| dying-spikes n dying-fall | 783 of 784 (all but the flipped kill guard) |
| steady n dying | 736 |
| union of all three | **919** (vs 2439 run separately) |

So the fused {alive, dying-spikes, dying-fall} program computes ~919
nodes - **+5.5% over steady alone** - to cover both death causes with a
per-lane selector instead of chunk deopt. The alignment that unlocked it:
derivation 2 pinned the spd move gate to the witness direction and shared
only ~110 nodes (every post-blend node diverges); keeping the gate as
steady's ti_spd_select masked select (derivation 3) recovered 626 nodes
and also covers standing deaths.

# Fused specialization-set kernel: H=68 rowkey sets IDENTICAL, dying lanes collapse to reps (2026-08-19)

Steps 3+4 of plans/shape-tag-plan.md (#163/#164), commits 53a63fa +
ab97067. `transpile --fuse steady,dy-spikes,dy-fall WITNESS OUT.rs`
emits ONE kernel (gitignored, feature `fused`) with per-member-set deopt
registers; the executor (dispatch::fused, ahead of the steady class
kernel, CELESTE_FUSED=0 opt-out) runs the steady rows exactly as the
class kernel does and collapses DYING-covered lanes to one interpreter
representative per distinct uniform-output tuple - sound because the
fuse pass PROVES the dying boundary rows block-uniform (final-heap
reachability: every lane-varying out cell is a deleted-player field;
what survives is will_restart/delay_restart/frames/deaths, all uniform
scalars), so dropped lanes' rows are member-certified identical to the
representative's and the row SET is preserved by construction.

Design deltas vs the plan's spec, both simplifications:
- guard-as-selector is IMPLICIT: member coverage = the member's own dp
  register set; lane -> first covered member in priority order;
  uncovered lanes deopt loudly. The static truth-table check is
  subsumed by the runtime count + the set gate below.
- no dead-shape materialization: representatives ride the EXISTING
  deopt path; the interpreter produces the exact dead rows.

Prerequisite fix (v5 dying overlays): the v3 overlays pinned the corpse
wall-slide gate to the witness's input==0 - in the fused kernel that is
a kb-dependent scalar *bd firing in every input!=0 variant, refusing
every chunk. v5 blends it (0-trip pins on the corpse check loops, which
scan an EMPTY objects table + dce/merge + speculate_region), certified
by a NEW input=R witness pair (member applies; v3 skipped). Node census
after v5: union 846 vs steady-alone 842 - the 3-member set now costs
**+0.5%** over steady alone (was +5.5% python / +1.8% rust at v3).

Gate (compiled-forward campaign CELESTE_COMPILED_FORWARD=1, k2ctl
config, resume f065->f068):
- f066/067/068 per-frame rowkey sets IDENTICAL to k2ctl (counts
  4,591,412 / 4,756,768 / 4,980,465; sorted-set sha256 equal). The
  fused kernel handled all 5,449,742 former steady-kernel lanes
  (steady: 0). Control run of the refactored emitter on the default
  engine: byte-identical rowkey files. Suite 553/553.
- 5,200,160 dying-covered lane-events collapsed to 1,385 reps.
- Residual: 449,728 uncovered lane-events at f066, and the per-variant
  histogram is decisive - **100% are kb5=1 (dash-press) variants;
  kb5=0 coverage is complete**. Because a lane uncovered in ANY
  (config, variant) must be interpreted whole, per-frame deopt lane
  counts are UNCHANGED so far (66,136 / 64,026 / 72,620). The dying
  members still pin the corpse dash-start gate (`btn(5) and djump>0`,
  assert %4010 at the merged in_i1_012_if_join_12; steady BLENDS that
  site) - blending it the same way as the wall-slide gate is the
  remaining step before the deopt drop and the timing measurement.

## Corpse dash-start blend closes the kb5 gap: UNCOVERED 0, and the engine reaches TIME PARITY (2026-08-19, follow-up)

The pinning entry was `dygb_anonymous_61_in_h061_in_k1039_cont` (the
dash-gate branch lands at the k1039 cont block in the dying lineage,
because it 0-trip-pins the k-loops that steady's collapse_loop folds -
same gate steady blends as zt001). Replaced IN PLACE in both dying
overlays with `dyzt_dash_blend` (speculate_region, arm
in_h061_if_body_107, mask+expand). The corpse arm writes only
deleted-player fields plus TWO live globals - `freeze=2`,
`has_dashed=true` (the minimal cart strips smoke/shake/psfx, so no
object creation, no shape change). Certified by two NEW dash-press
death witnesses (R+X on the death frame; member applies, observation
identical): tas/room_1_0_death_{spikes_frame_92,fall_frame_101}_dash_press.txt.

Those two globals are per-lane outs (the gate reads the post-grace-
refill djump, which is position-dependent), so the block-uniformity
proof got its planned generalization: a per-lane reachable out cell is
admitted iff its value is THE PRIMARY'S OWN OUT COLUMN for the same
cell (same fused node after CSE - checked by node identity at emission)
- then the executor reads it from KOut per lane (`dy_vary_key`,
N_DY_VARY=2: c20 freeze ZN, c41 has_dashed ZB) and the collapse key
becomes (uniform tuple, vary-at-lane). Uniform-scalar tuple cells may
now also be button-tainted (Dy is built inside the per-variant
callback, so a tainted scalar is still one value per callback).

Node census after the blend: 845 union, **829 shared by all three**
(was 846/755) - blending the gate ALIGNED the member graphs.

Gate (same k2ctl resume f065->f068, compiled-forward + frontier-only +
collect-first): f066/067/068 rowkey sets IDENTICAL again (same counts
and set hashes as k2ctl). **UNCOVERED-events: 1,576,352 -> 0.**
6,776,512 dying-covered lane-events -> 2,099 representatives.

Deopt-counter clarification (kills a wrong expectation the previous
section stated): the per-frame "deopt lanes" counts (66,136 / 64,026 /
72,620) are IDENTICAL in the pure-interpreter control too - that
counter is the DEFAULT program's premise-failure population (the
interpreter FORKS on the kill branch rather than deopting), so kernel
coverage never feeds it. The fused collapse shows up as interpreter
WORK removed, i.e. in time.

TIMING, full 68-frame runs, frontier-only, no collect-first, 16
threads, --save-frames (the engine-adoption number), two runs each:
- default engine:                115.63s / 116.04s   7.66 / 7.85 GB peak
- compiled-forward + fused:      114.26s / 114.72s   7.44 / 7.44 GB peak
Fused is **-1.2% time and ~-0.3 GB peak, reproduced** - call it parity
with a slight edge. The former 9-13% penalty of compiled-forward under
frontier-only is GONE (the uncovered dying lanes were the fan-out
cost). Fused runner totals for the full run: 36,054,933 lanes through
the fused kernel, 11,480,704 dying-covered events -> 3,864 reps,
uncovered 0. All 68 per-frame rowkey sets identical between the pair
(sorted-set sha256).

## The plain path for kernel deopt sub-chunks: -22% time, -45% peak (2026-08-19, same day)

The thread-second profile of the pair above exposed the real sink: the
per-frame "deopt lanes" counter (66k/frame) was ENGINE-INVARIANT
because the kernels' deopt sub-chunks (dying representatives, class-
leaving rows) ran the SPECIALIZED interpreter inside `run_chunk`, which
has no deopt context - one rep failing the #objects-unchanged premise
(the census was unanimous: 1,427/1,427 triggers are that single
assert) errored the attempt, the outer optimistic arm re-ran the
ENTIRE 8000-lane state through granular deopt, and the kernel's rows
were computed and thrown away for ~450 of ~570 states per frame.

Fix (`FrameEngine::plain_block` + `PlainPath`): route kernel_ok=false
sub-chunks straight through to_canonical -> PLAIN program ->
from_canonical (sound for every lane - it is the reference
semantics), with the COMPILE recipe's StateMapping. The compiled
attempt stops failing, so the whole granular block disappears:

- default engine:                115.63s / 116.04s   7.66 / 7.85 GB peak
- fused + plain path:             89.80s /  89.55s   4.21 / 4.20 GB peak
**-22% time, -45% peak memory vs the default engine**, two runs each.
Thread-seconds: frame body 714 -> 525, deopt blocks 334 -> 0.00,
boundary prepare 22.1 -> 9.4. Plain-routed lanes: 14,284 TOTAL over
68 frames (vs 66k/frame granular before). Fused totals rose to
50.0M lanes / 47.5M dying-covered events -> 9,503 reps because
attempts no longer fail out of the compiled path.

Gates: all 68 per-frame rowkey sets IDENTICAL to the default engine
(sorted-set sha256); suite 553/553.

**Engine adoption is now a live decision** (was "parity"): the
compiled-forward+fused engine wins big at H=68 on room (1,0). Still
default OFF pending (a) the fingerprint story for the fused artifact
(fingerprint-invisible today - fine for gates, not for ladder
provenance) and (b) Philippe's call. CLAUDE.md's "default OFF is a
measurement" note is now STALE in this configuration.

(Blocker (a) closed the same day: commit 7932d54 hashes
`compiled_engine = hash(compile recipe text, fused artifact
self-fingerprint)` into the campaign fingerprint when the engine is
on, `None` reproducing the legacy stream so interpreter checkpoints
stay valid. Consequence used below: engines cannot share checkpoints,
so an A/B pair means two full runs from frame 1.)

## The TRACED kernels at the production horizon (2026-08-24)

(The checked-in traced set this measures was deleted 2026-08-29 - the ASM
backend replaced it. The zero-deopt COVERAGE result carries over, since the
ASM kernels are assembled from the same fused graphs; the WALL/PEAK numbers
need remeasuring under `compiled::asm_kernel`.)

Stage 5 of plans/tracing.md: the per-shape kernels the AST tracer emits,
checked in (then) at `crates/celeste-kernels/src/traced/`, wired into
`run_chunk_kernel`. Room (1,0), `rewrites.jsonl`, `bench --frames 94
--deopt`, ladder env (frontier-only, collect-first, 8000-lane caps, 16
threads, `--save-frames`), ONE release binary, no `--features fused`,
the three sides an env var apart:

| side | wall | peak | us/lane | deopt lanes | coverage |
|---|---|---|---|---|---|
| plain (interpreter) | 512.21 s | 14.76 GB | 86.1 | 4,400,724 | - |
| classes (8 committed `kernel_gen_*`) | 608.44 s | 14.60 GB | 102.3 | 4,138,311 | 56 missed, 1,809,074 plain-routed |
| **traced (3 per-shape)** | **340.91 s** | **8.18 GB** | **57.3** | **0** | **0 missed, 0 plain-routed** |

All three report **5,949,326 lanes**, identical - the cross-check that
these are the same search. **-33.4% wall and -44.6% peak against the
interpreter**, 1.50x.

Three things worth more than the headline:

* **Zero deopt, at 172,626,763 lanes.** Nothing routed to the
  interpreter. The recorded fused engine below still had 456,960
  uncovered lane-events; three checked-in files have none. The shape
  walk closes to a FIXPOINT, which is why this is a property rather
  than a sample - a shape the room reaches is a shape the set has.
* **The class kernels are a net LOSS without `fused`.** 608.44 s
  against the interpreter's 512.21 s. The -22.9% recorded below was
  compiled **+fused**; compiled-without-fused was never measured, and
  it is negative. The fused artifact is generated per campaign and
  never checked in, so what the repository actually ships as its
  compiled engine was, until tonight, slower than the interpreter.
* **Fragments before merge fell 160,656 -> 48,144**, which is where
  the 6.6 GB of peak went.

Not directly comparable to the 424.39 s below - that was fused, on
another night. Tonight's interpreter side ran 512.21 s against that
night's 550.65 s, so conditions differ by ~7%.

Two caveats on the numbers, stated because they bound what they claim:
the traced set is room **(1,0)** only (`transpile --room-kernels` walks
`CELESTE_START_ROOM`), and the five `kernel_gen_r20_*` kernels are room
(2,0)'s, still the only kernels that room has.

## Engine adoption validation at depth (2026-08-20)

The H=68 numbers above are mid-room; the ladder's first real horizon
on room (1,0) is 94. Full forward passes from frame 1 under the ladder
env (frontier-only, collect-first, 8000-lane caps, 16 threads,
`--save-frames`), one binary (`--features fused`, engine toggled by
`CELESTE_COMPILED_FORWARD=1`), two runs each, back-to-back same night:

- room (1,0), `rewrites.jsonl`, `bench --frames 94 --deopt`:
  - default engine:      550.65 / 550.87 s   15.42 / 15.33 GB peak
  - compiled + fused:    424.39 / 424.40 s    9.24 /  9.20 GB peak
  **-22.9% wall, -40% peak at the production horizon** - the H=68
  result holds at depth. ALL 94 per-frame rowkey sets identical
  (178,576,090 rows total on each engine); both engines report first
  room-exit lanes at frame 89. The fused counters are bit-identical
  across the two fused runs (lanes 170,292,074, dying-covered
  248,759,424, reps 56,267) - the engine is deterministic end to end.
  Known residue: 456,960 UNCOVERED lane-events (0.27% of covered
  events; 0 at H=68), uniform across every kb bit. DIAGNOSED
  2026-08-20 with the CELESTE_FUSED_UNCOV_DUMP microscope: it is NOT
  a death mode - it is the WIN-ADJACENT population. Every dumped lane
  sits at y=-4 or y=-3 with upward spd at x=95..113 (the exit notch);
  the steady overlay pins the `this.y<-4` exit branch FALSE, and that
  guard's TRUE side is reachable either deterministically (y=-4,
  moving up) or through the level-0 rem widening (y=-3: the +-0.5
  interval reaches past the threshold) - input-independent both ways,
  which is the uniform kb histogram. Covering it means an EXIT MEMBER
  whose output is the room transition (successors in a DIFFERENT room
  and shape) - multi-room seam work, not a dying-member derivation.
  Cost today ~0 (the lanes re-run whole in the interpreter; the
  campaign's deopt re-run line reads 0.00s), so it waits for the
  multi-room ladder rather than for a perf need.

- room (0,0), `rewrites-room00.jsonl`, `bench --frames 40 --deopt`
  (first compiled-forward run ever on a foreign-shape room):
  - default engine:        6.44 s   0.26 GB peak
  - compiled + fused:     25.72 s   3.13 GB peak
  All 40 per-frame rowkey sets identical, 387,443 rows - exactly the
  historical room (0,0) f40 row count, a free cross-check. Every one
  of the 78,220 lanes MISSED the kernels (steady/dash/frozen 0,
  fused 0: the witnesses are room (1,0) shapes), so this measures the
  engine's per-chunk fan-out overhead with zero kernel payoff: 4.0x
  time, 12x peak.

**The adoption shape that falls out: per-room, not global.** The
engine is CORRECT everywhere the gates have reached (94 + 40 frames,
two rooms, ~179M rows, set-identical) but profitable only where its
kernels bind. Flipping room (1,0) ladder campaigns to
`CELESTE_COMPILED_FORWARD=1` on a `--features fused` build buys -23%
wall / -40% peak per level-0 extend; flipping room (0,0) would cost
4x. A room's campaign should opt in when its shapes have kernel +
fused coverage (today: room (1,0) only). Decision is Philippe's;
ladder.sh does not set the env either way.

### The "step-gate mystery" was a gate bug, not an engine bug (2026-08-19)

`native-probe --abstract-bench` on a frontier-only campaign dir
(k2ctl, f065->f066) reported 0 missing / 4,123,936 EXTRA row keys from
`FrameEngine::step`, and a first check against the `frames/*.rowkeys`
sidecars found 0 of the extras among visited rows - which read as the
engine fabricating novel states. Both halves of that were wrong, and
the failure mode is worth recording:

* The extras were REVISITED rows, exactly what a frontier-only
  reference misses: the saved frames are each frame's NEW rows, while
  `step` returns the raw successor set. Recomputed in the engine's own
  key space (import + `boundary` over every saved frame f000..f065:
  41.6M visited rows), **4,123,936 of 4,123,936 extras are visited
  rows, 0 unexplained**. An extra-lane autopsy agreed before the count
  did: each sampled extra differed from its nearest reference row in
  ONE cell - `objects.1.y` by a pixel, `objects.1.spd.y` by exactly
  one gravity tick (0.21) - i.e. real game states from earlier in the
  search, not corruption. A `CELESTE_FUSED=0` bisect produced the
  byte-identical mismatch, exonerating the fused executor before the
  frontier result landed.
* The refuting check intersected keys ACROSS KEY SPACES. The sidecars
  hold the interpreter's keys (`vectorize::visited_row_keys`,
  `row_key_hashes` over State columns); the gate compares Rt2
  `boundary` keys. D1 (#156) established these as a BIJECTION, not an
  identity - raw-value intersection across the two spaces is empty by
  construction and refutes nothing. Rule: a key is meaningful only in
  the space that minted it; any cross-engine set comparison must
  canonicalize BOTH sides through one keyer first (which is exactly
  what gate 2 itself already did - the frontier check just didn't).

The bench's gate 2 is frontier-aware now: verdict "row-key set equal
MODULO VISITED" requires missing == 0 AND every extra to be a visited
row. Four intermediate wrong conclusions from 2026-08-19 (fabricated
rows, p_dash twin-block suspicion, chimera-lane theory, "do not trust
step()-based gates") are all withdrawn; step() and the campaign path
agree, and the campaign gates were never affected.

### M1 stages 1+2: suffix hoist + support-segment emission (2026-08-19)

Task #152. The fused artifact used to evaluate its whole 120-line
suffix once per input variant (64x). Stage 1 hoisted the 14
kb-independent lets into the prefix; stage 2 groups the remaining
lines by transitive kb-support and emits per-support segment fns
computed once per ASSIGNMENT of their support (frame() caches
2/4/4/8/16/32 copies of the 6 segments; only a 15-line full-support
residual and the epilogue still run per variant). Suffix node evals:
6,720 -> ~1,638 per 16-lane row (4.1x fewer).

Measured on `--abstract-bench ~/perf-scratch/k2ctl 65` (fused,
16 threads), same-day A/B at 6 reps, stage-1 emitter (HEAD) vs
stage-2, identical gate output both ways (engine 8,715,348 =
4,591,412 ref + 4,123,936 revisited, MODULO VISITED OK):

| | min | mean |
|---|---|---|
| stage 1 | 13,405.81 ms | 13,578.92 ms |
| stage 2 | 13,055.78 ms | 13,160.15 ms |
| delta | **-2.6%** | **-3.1%** |

Day-to-day drift is LARGER than this effect (the same stage-1 binary
measured 14.24 s the previous evening), so cross-day bench numbers on
this workload are not comparable - A/B on the same day or not at all.
perf (whole process): artifact compute 10.9% -> 4.9% (suffix::<B>
9.6% -> 3.0%, seg fns 0.7%, frame 1.2%). The `run_fused` closure -
prededup row hashing, dy rep keying, append_out - is unchanged at
8.5% and is now the single biggest fused-engine cost; that is M1
stage 3 (executor-side reuse of per-segment column values in the
row-key path). Memberchecks ok, suite 553/553.

### Free-choice specialization: -2.7% wall, and it is real (2026-08-22)

The buttons stop being a code structure: the graph is rebuilt once per
free assignment into one interned arena, the body is emitted once, and the
64 variants collapse to the DISTINCT ones (36 on steady). Same-day A/B,
`--abstract-bench ~/perf-scratch/k2ctl 65`, non-fused release build, two
invocations of 5 reps per side, baseline pinned to an explicit SHA:

| | best min | median min | median mean |
|---|---|---|---|
| 69fb2a5 (pre-specialization) | 16,937.44 ms | 17,048.59 ms | 17,114.33 ms |
| 3d7b536 (specialized) | 16,484.64 ms | 16,587.53 ms | 16,659.81 ms |
| delta | **-2.7%** | **-2.7%** | **-2.7%** |

Clean separation - the worst specialized run (16,690.41) beats the best
baseline run (16,937.44) - and the three statistics agree to within 0.04
points, which they did not in any of the contaminated attempts below. Row
keys identical on every run.

This is the first change in this campaign whose effect is larger than the
drift. It also lands exactly where predicted: M1 stage 2 measured -2.6%
for the same idea done by hand over text (support segments), and the
prediction written down before running this was "expect that order, not
the 2.8x node-evaluation ratio". The ratio was never a wall-clock claim.

**Three attempts were thrown away before this one, all my own fault**, and
the failure modes are worth recording because they all LOOKED like results:

1. Concurrent `cargo build` while the bench ran. The min looked fine
   (16,785) but the means were visibly polluted (17,783 vs a 16,920
   baseline). Same "one cargo at a time" rule I had written into CLAUDE.md
   that morning after it cost a measurement earlier the same day.
2. `HEAD~1` hardcoded as the baseline. HEAD advanced, the fuse-port commit
   did not touch generated kernels, so both sides ran the IDENTICAL
   binary. The tell was `Finished in 0.02s` on the second build - cargo
   had nothing to rebuild. Baselines get explicit SHAs now.
3. `pkill -f "bash /tmp/ab.sh"` matched its own wrapper's command line and
   killed the shell before the heredoc that created the next script, so
   the run never started - and `pgrep -f "ab2.sh"` matched the MONITOR's
   command line, so it reported "still running" for half an hour of
   nothing. Sentinel files (`/tmp/ab2.done`) instead of pgrep now.

**Emitted size went UP where the collapse happens**: steady 1925 -> 3472
lines, r20-steady 2259 -> 3797 (36 variants' distinct nodes materialized
in source rather than left for LLVM to make 64 copies of); the other six
kernels shrank slightly. The fused artifact went 122 KB -> 605 KB and
takes 48 s to compile under `quick`. So this trades source size and build
time for run time, and the trade is worth it at these numbers - but it is
a trade, not a free win.

### Graph-driven codegen: -1% wall, and a size drop that is not a speedup (2026-08-22)

Task #176 stage B. The class-kernel BODY is now emitted from the graph IR
(`transpile::lower`) instead of from the walk's text stream:
representation, scope placement and guard emission are derived rather than
chosen per emit site. Same-day A/B, `--abstract-bench ~/perf-scratch/k2ctl
65` (NON-fused build, 16 threads - the class kernels are what this
exercises; the fused artifact is a separate path), 3 invocations of 10 reps
per side:

| | best min | median min | median mean |
|---|---|---|---|
| old (text emitter) | 17,584.16 ms | 17,615.73 ms | 17,862.78 ms |
| new (graph emitter) | 17,358.18 ms | 17,456.29 ms | 17,617.78 ms |
| delta | **-1.3%** | **-0.9%** | **-1.4%** |

The three new runs all sit below all three old runs (max new min 17,487.95
< min old min 17,584.16), so the direction is clean - but the effect is
~1%, which is INSIDE the day-to-day drift this workload is known to have
(M1 stage 2's note: the same binary moved 13.4 s -> 14.24 s overnight).
Treat it as "not a regression, plausibly a small win", not as a result.
Gate output identical on every run: engine 8,715,348 = 4,591,412 ref +
4,123,936 revisited, MODULO VISITED OK.

**The emitted code got ~50% smaller and that is NOT where the time went.**
r20_steady 4562 -> 2094 lines, r20_dash 3837 -> 1754, steady 2248 -> 1747.
Dead lets and duplicate splats were already LLVM's problem, not the CPU's;
the size drop buys compile time and readability, not throughput. Anyone
reading the diff should not expect the two numbers to be related.

**What the graph's own census says is still on the table**, and what it
does not say: a node whose value depends on k of the 6 button bits is
computed 64 times where 2^k would do, because the prefix/suffix split is
binary. Per member, node-evaluations per frame under the binary split vs
exact 2^|cone|: steady 12,740 vs 4,504 (2.8x), r20-steady 19,331 vs 9,401
(2.1x), frozen 424 vs 52 (8.2x), dash 1,657 vs 1,285 (1.3x). Those ratios
count evaluations ASSUMING every suffix node runs in every variant, and
LLVM already dead-codes each monomorphization, so they are not wall-clock
predictions. The comparable thing that was actually measured is M1 stage 2
- the same sharing done by hand for the fused artifact - at **-2.6%**.
Expect that order from the button tree (plans/multi-output-fusion.md,
"Stage C"), not 2.8x.

Also measured, because it was quoted as a fusion opportunity: the 64
button variants collapse to **36 distinct** on steady and r20-steady, 4 on
dash and r20-dying-spikes, 1 on frozen and r20-dying-fall. The signature
must be (output cells, ok, live), not outputs alone - `dispatch.rs`
ignores the variant mask but accumulates `deopt_rows` from `kout.deopt &
kout.valid` and aborts on `kout.bd`, so two variants writing the same
cells while deopting different lanes are NOT interchangeable. Widening the
signature turned out to cost nothing here (36/64 both ways), which makes
the earlier outputs-only number a bound that happened to be tight rather
than a result.

### M1 stage 3: support-factored row keys (2026-08-20)

The pre-dedup key is a commutative per-cell sum, so the artifact now
emits it factored by what each cell reads (row_keys_base / _class /
_var + KEY_CLASS_SUPS; the monolithic row_keys stays for one-shot
callers) and run_fused caches base + per-class partials, hashing only
the 5 full-support cells per variant. Cell-mixes per (16-lane group x
fork combo): 1,280 -> 378 (3.4x). Byte-identical keys by
construction; `CELESTE_KEYCHECK=1` verifies factored == monolithic at
runtime and localizes divergence to the stale class.

Same-night A/B, 6 reps, same gate output both ways:

| | min | mean |
|---|---|---|
| stage 2 | 13,170.73 ms | 13,235.99 ms |
| stage 3 | 12,847.15 ms | 13,090.04 ms |
| delta | **-2.5%** | **-1.1%** |

(dy_reps BTreeMap -> FxHashMap: NEUTRAL, 12,938 ms min; reverted.)
perf: the run_fused closure 8.5% -> 7.8% of process; key hashing
visible at 0.25% (row_keys_class; base/var/fin inline into the
closure). What remains in the closure is dy rep keying, the seen-set,
append_out, and the partial-add loops - diminishing returns from here.

Two traps found en route, both worth remembering:

1. **frame()'s variant sweep runs once per PREFIX FORK COMBINATION.**
   Executor caches keyed per 16-lane group leaked across combos; the
   row gate caught it (1.18M missing / 2.59M extra). The fused
   callback now leads with a fork-combo counter `cfg` and the
   executor resets its caches when it changes.
2. **regen-generated.sh clobbers the fused native-probe.** (Historical -
   the script and the fused feature are both gone now.) Its final
   workspace build rewrites target/release/native-probe WITHOUT the
   fused feature, and a subsequent `cargo build -p native-probe
   --features celeste-rust/fused` may see a fresh fingerprint and
   skip the relink - the stale non-fused binary stays on disk, ~33%
   slower with the row gate still green (fallback preserves rows).
   `touch native-probe/src/main.rs` before rebuilding, and check the
   `fused: lanes ...` stderr line actually appears.

# K2's pm1 death-partition fix: REFUTED - inert and slightly slower (2026-08-19)

The census's preferred fix (add `will_restart` to the pm1 partition
cells so dead and alive lanes never share a chunk) was built, verified
(differential verify 40 identical, suite 553/553) and measured at H=68,
room (1,0), frontier-only + collect-first, 16 threads, four runs in a
2x2: {old, new} pm1 x {interpreter, compiled} - and it does NOTHING:

| | old pm1 | new pm1 |
|---|---|---|
| interp wall | 110.88 s | 113.31 s (+2.2%) |
| compiled wall | 112.22 s | 115.85 s (+3.2%) |
| deopt lanes (both paths) | 399,516 | 399,516 |
| per-frame rowkey SETS, all 68 frames | = | **= (all four runs identical)** |

The premise was wrong: dead states (empty objects array) already have
their OWN SHAPE, and fragments are grouped by shape - dead lanes were
never sharing fragments or chunks with alive ones. `will_restart` never
varies within a state (alive states are uniformly false, dead uniformly
true), so the partition is semantically inert - which the four-way
key-set identity now certifies - and its only effect is partition-key
overhead. Not landed; the recipes stay at six cells.

Two real findings from the same runs:

* **Deopt machinery is ~94% of frame-body CPU at depth** (old pm1,
  interp): frame body 581.2 s thread-CPU, of which origin-tagged
  specialized runs 307.4 s + plain-program re-runs 240.7 s - all to
  re-run 399,516 dead lanes (0.8% of the run's 50M input lanes). The
  tagged mode taxes every state; the plain re-runs pay full plain-program
  cost on tiny dead fragments. That is K1's (deferred deopt) prize, and
  a death-shape VARIANT program (the countdown dynamics are trivial:
  delay_restart decrements, everything else frozen; 14 of 15 dead frames
  are pure countdown) would remove the failures at the source.
* **Config discipline**: these four runs are mutually comparable but NOT
  comparable to the D4 gate table above - the D4/D0 reference runs used
  `CELESTE_WIN_AT_XY=64,44` (synthetic win) WITHOUT collect-first. A
  first comparison against d4run mis-read the env delta (+1,716 rows by
  f68) as a pm1 effect. Always re-grep the reference run's exact env
  from its log before claiming a diff.

(Bonus certificate: the 2x2's key-set identity re-confirms compiled ==
interpreted row sets at HEAD, both pm1 variants, in this config.)

# D1 GATED: engine row keys == interpreter row keys, as an equivalence (2026-08-19)

The two key constructions (`Rt2::boundary`'s cell_mix sums, the
interpreter's `row_key_hashes`) are different hashes and never
numerically equal; what a shared dedup needs is that they merge exactly
the SAME lanes. That is now gated on real data, per lane, both
directions (`native-probe --key-gate` / `--key-gate-outputs`; each lane
of a canonical state is keyed by BOTH functions - `boundary_canonicalize`
is the boundary split before its dedup, so dropped lanes still have keys -
and the pairs feed two run-global maps whose any conflict is a broken
bijection):

| gate | data | lanes | distinct keys | result |
|---|---|---|---|---|
| frontier f001-f068 | every distinct row of the H=68 room (1,0) run, sidecar-grounded per frame | 55,958,742 | 55,958,742 = 55,958,742 | **BIJECTION HOLDS** |
| outputs f040 | one frame's raw pre-subtract outputs, both engines' lineages, 26:1 dup | 9,853,518 | 370,077 = 370,077 | **BIJECTION HOLDS** |
| outputs f058 | death-shape frame, 20:1 dup (dead inputs fed directly; both frame bodies skip the 4 dead-input fragments, ~719k lanes, reported - the campaign deopts exactly those) | 70,946,080 | 3,530,217 = 3,530,217 | **BIJECTION HOLDS** |

The frontier direction kills the live soundness worry: `run_frame_chunk`
dedups internally with engine keys BEFORE the campaign re-keys (`dedup_
keeps_serial`, kernel pre-dedup), so an engine-key collision on rows the
interpreter distinguishes is silent row loss (the #148 class). Zero
collisions across all 55.9M distinct rows the search ever visited. The
outputs direction covers the reverse (engine over-splitting what the
interpreter merges) on dup-heavy real streams.

Repricing the plan's "a compiled run stops keying 3x": D2 measured
hashing at 7 ns/row - the redundant keying is worth ~nothing in wall
time, so D1's deliverable is this certificate, not a perf change.
Numeric unification (one shared key function) would only matter if
sidecars were to be written straight from blocks, i.e. if the compiled
engine became the campaign default, which it is not (see "The compiled
engine inside the campaign"). Not pursued.

# D4 LANDED: hash-partitioned visited filter, -31% forward wall (2026-08-19)

The D2 design, integrated. Workers now only HASH their fragments
(`visited_lane_keys`); the seen+probe filter runs after each batch's
join in `partition_filter` - one seen set per thread, disjoint by key
hash, PERSISTING for the whole frame - and the serial id assignment is
unchanged. Default ON; `CELESTE_PARTITIONED_FILTER=0` restores the
classic in-worker filter.

Gate, room (1,0) H=68 ladder level-0 environment, same binary A/B:

| | classic | partitioned |
|---|---|---|
| bench wall | 168.56 s | **115.87 s (-31.3%)** |
| row keys + visited probe, worker CPU | 862.65 s | **20.90 s** (hash only) |
| `fwd.partition_filter` (the moved filter) | - | 18.61 s |
| peak RSS | 7.58 GB | 7.93 GB (+4.6%, the seen sets) |
| visited total | 55,958,742 | 55,958,742 |
| rowkeys sidecars, all 68 frames | - | **byte-identical, keys AND ids** |

Byte-identity is by construction, not luck: a candidate whose key
`insert_new` would reject can be added or removed without changing
survivors or ids, and the classic and partitioned candidate lists
differ only in such entries (first-in-fragment vs first-in-frame
occurrences of the same first-in-serial-order keys). Suite 553/553.

The serial phase also shrinks (3.65 s `fwd.boundary_stream`): it now
receives ~2.2M candidates per deep frame instead of ~17.8M.

# Dedup roofline, isolated (D2): the probe is everything (2026-08-19)

`native-probe --dedup-bench CKPT 60` replays f60's real OFFERED key
stream (104,953,085 keys in 6,474 fragments, dumped by
`CELESTE_DUMP_OFFERED`) against the real mmap'd visited structure as of
f59 (22.8M rows), timing the FILTER half of `visited_row_keys` under
different designs. Decisions are identical across variants by
construction; the distinct-candidate count (2,199,846) is asserted.

First, the campaign's own split (census now prints it): **hashing the
rows is 7 ns/row; the filter+probe is 233-333 ns/row of worker CPU** -
the hash half of "row keys + visited probe" is nearly free, and one
mmap `contains_historic` probe costs ~700 ns.

| variant (f60 stream) | ns/offered row | notes |
|---|---|---|
| today: per-fragment seen, 1 thread | **218** | validates the campaign figure |
| frame-wide seen, 1 thread | **37.4** | 8.5x fewer probes, same answers |
| hash-partitioned, 16 threads | **6.1** | whole frame's filter in 0.64 s |
| worker-persistent seen, 16 threads | 14.5 | **REFUTED as a shortcut, see below** |
| seen-set machinery alone (no probe) | 6.0-9.7 | the floor |

Three structural findings:

1. **Today's pipeline hands the serial phase 17,784,200 candidate
   events for 2,199,846 distinct new rows** - every new key is
   re-offered ~8x because the seen set is per-fragment. The serial
   `insert_new` collapses them, but they are hashed, buffered and
   crossed over a thread boundary first.
2. **The drop-in fix does not work.** Making each worker's seen set
   persist across its fragments (no resharding, decisions provably
   unchanged) only cuts probes 31.8M -> 21.8M (1.46x), because a hot
   key appears in ~8 fragments that land on ~8 DIFFERENT workers.
   14.5 ns/row at 16 threads - no better than today's 218/16. The 8.5x
   only exists if the tier is shared across workers, i.e. keys are
   HASH-PARTITIONED to owner threads.
3. **Partition memory is cache-sized, as the census predicted**: 3.74M
   frame-distinct keys / 16 partitions = ~234k keys = 3.7 MB per
   thread, L3-resident. That is why partitioned-16 (6.1 ns) beats even
   the modelled frame-seen-at-16 (37.4/16 = 2.3 would ignore that the
   single shared set is DRAM-sized; the real win needs the partition).

Roofline projection, H=68 interpreter run: the keys phase is 862 s of
worker CPU (~55 s of wall). At 7 ns hash + ~6 ns filter per offered
row, the same work is ~38 s of worker CPU (~2.5 s wall) plus 8x less
serial-phase input - the 44% headline item of the forward stage drops
to low single digits. The integration design this implies is in
plans/dedup-roofline-plan.md D3/D4: partition for membership,
serial-order id assignment for byte-identical determinism.

# The "24-row divergence" was a hint-erasure re-keying; FIXED (2026-08-19)

The compiled and interpreted H=68 runs' visited sets differed by 24 rows
(55,958,766 vs 55,958,742). The per-frame rowkeys sidecar diff showed the
truth was much larger and much cleaner: **identical through f24, entirely
disjoint key sets from f25 on**, with equal per-frame counts everywhere
except f25 itself (204 vs 180 new rows - the entire +24).

Cause: at f25 the game first mints `Nil(Some("nil pointer to field
tile"))`. `compiled::bridge` cannot carry the hint (`AV::Nil` has no
payload), so compiled exports said `Nil(None)` - a different shape hash,
hence different `visited_row_keys` for every descendant state forever.
`native-probe --frame-diff` (new: one frame under both engines, every
output lane dumped as a readable row) showed the engines' f25 outputs
were **value-identical, 204 = 204**, once the hint was normalized. The
semantics never disagreed; an error-message string was search state.

Fix: `erase_provenance_hints` at the frame boundary
(`make_state_abstract`): `Nil(Some(_))` -> `Nil(None)`,
`NilPointer(name)` -> `NilPointer("")`. Provenance-only (grep-verified:
born on loads through a NilPointer, consumed only in messages), so it is
a canonicalization, not a widening. FORMAT_VERSION 4 -> 5 because keys
change from the first hinted nil onward; pre-fix checkpoints refuse.

Gate: H=68 A/B under FORMAT 5 - totals equal (55,958,742 both, frontier
4,979,427 both) and **all 68 frames' rowkeys sidecars set-identical**
between the two engines. The compiled forward is now the SAME search.
(On room (1,0) the interpreter's own totals did not move - the hint was
uniform there - so the historical (1,0) numbers in this file remain
comparable.)

# Collect-first deopt loses at H=68, but do NOT flip the default (2026-08-19)

`CELESTE_DEOPT_COLLECT_FIRST=1`, which ladder.sh sets, runs every frame
under the origin-tagged specialized program so a failing state pays one
run instead of an attempt plus a retry. At H=68 on room (1,0) that trade
is backwards: optimistic is **154.70 s against collect-first's
162.17 s (-4.6%)**, with an identical visited set (55,958,742), identical
final frontier (4,976,277 lanes) and identical first-win frame (64).
Failures are ~7% of chunks here, so paying the origin column on 100% of
them costs more than retrying 7%.

That is a statement about H=68, not about depth. At H=90 on the same
room, collect-first was an **-18% WIN** (737 s vs 898 s, further down
this file) - the failure share climbs steeply through the kill/respawn
frames (v1 re-ran ~5M lanes per frame by f74), and past the crossover
the origin column is cheaper than the retries. (Those H=90 numbers
predate task #96's deopt elimination, so the exact crossover has moved,
but the direction is structural.) So ladder.sh keeps collect-first: the
ladder exists for deep campaigns, and the fix for the whole trade is
deferred deopt (K1 in plans/dedup-roofline-plan.md), not a flipped
default tuned at mid-depth.

# End-to-end forward, per INPUT LANE, and where it goes (2026-08-19)

The frame-body figures in the next section are the compute. This is the
whole forward stage on the same scale: `rewrite bench --frames 68 --deopt
--checkpoint-dir D --save-frames` under the ladder's level-0 environment
(frontier-only, collect-first deopt, chunk caps 8,000), room (1,0),
synthetic win, 16 threads, 68 frames, **50,976,038 input lanes**.

Per INPUT LANE - one row fed into the system for one frame, summed over
the run - because that is the unit optimizations trade against (sharing
work across the 64 inputs changes the per-input figure but not this one).

| | interpreter | compiled |
|---|---|---|
| **total** | **3,181 ns** (162.17 s) | **3,095 ns** (157.77 s) |
| frame body | 963 | 1,218 |
| ...compiled engine | - | 415 |
| ......kernel `run` | - | 346 |
| ......block dedup+merge | - | 62 |
| ......import + partition + export | - | 10 |
| ...specialized frame (interpreter) | 496 | - |
| ...deopt retry (specialized, tagged) | - | 204 |
| ...**deopt re-run, PLAIN program** | **412** | **430** |
| ...rest (drops, moves) | 55 | 169 |
| boundary prepare | 1,465 | 1,193 |
| ...**row keys + visited probe** | **1,413** | **1,163** |
| ...abstraction + gc | 51 | 30 |
| `fwd.save_frames` | 224 | 218 |
| `fwd.boundary_stream` (serial subtract) | 210 | 182 |
| `fwd.merge` | 116 | 89 |
| `fwd.boundary_gather` | 26 | 19 |
| checkpoint writes, init, bookkeeping | 178 | 175 |

## Three things this says, none of which was visible before

**1. `fwd.interpret` is not the interpreter.** It is a wall slice wrapping
the whole worker scope, and each worker runs the frame body AND
`stream_boundary_prepare`. By worker thread-seconds the split is 39.7% /
60.3% (interpreter run), and `visited_row_keys` alone is 58.2%. The
"forward pass is 79% interpreter" line in the campaign cost breakdown
below was about 40% interpreter and about 58% row hashing.

**2. Hashing rows and probing the visited set is the single biggest item
in the forward pass** - 1,413 ns/input-lane, 44%, and 1.5x the entire
frame body. On a compiled run it is done three times over: the kernel
keys rows out of its output registers for pre-dedup, `Rt2::boundary` keys
them again, and then the campaign exports to a `State` and
`visited_row_keys` keys them a third time. Handing the engine's keys to
the campaign is the largest single lever in the stage - but it needs a
new gate, because gate 2 only proves `Rt2` keys equal `Rt2` keys, never
that they equal `visited_row_keys`.

**3. The deopt machinery costs as much as the engine, and it also starves
it.** 399,516 of 51M lanes (0.78%) fail a specialization premise and
re-run under the PLAIN (unrewritten) program. That re-run is 412-430
ns/input-lane amortized over every lane - comparable to the entire
compiled engine (415) and close to the interpreter's own frame (496).

It is concentrated and growing, not spread: NOTHING deopts before f58,
and then f58 costs 0.20 s of plain-program CPU, f62 11.71 s, f68
**53.68 s - 255% of that frame's own wall**. f58 is where states start
dying, and the death/restart path is outside what the recipe was verified
over. A campaign to H=90+ is dominated by this, and no plan has priced
it.

The starvation is the part nobody would guess. Kernel coverage is
essentially perfect where the kernel runs - 36,707,603 lanes dispatched
(steady 35.8M, dash 0.54M, frozen 0.32M) against **43 missed** - but that
is only **72% of the 51M input lanes**. The other 28% never reach a
kernel at all: a chunk whose compiled attempt fails is re-run WHOLESALE
by the interpreter, and f58..f68 hold 66% of the run's input lanes. So
the compiled engine's 415 ns is amortized over every lane while covering
under three quarters of them.

For a failing chunk the compiled path runs the frame THREE times:

1. the compiled attempt (kernel) - `run`
2. the specialized program with a per-lane origin column, over the WHOLE
   chunk, to find which lanes failed - "deopt retry", 204 ns
3. the plain program on just the failed lanes - 430 ns

Collect-first does 2 and 3 only, skipping the optimistic attempt by
paying the origin column on 100% of chunks (496 ns) instead. That is the
trade, and it is why the interpreter column has no "retry" row. A
compiled run cannot use collect-first as it stands, because the origin
column changes the shape hash so no kernel binds (P1 stage 3) - so the
compiled path is on the wrong side of that trade at exactly the depths
where deopt matters.

## The kernel number, chased from microbench to campaign

| | ns/input-lane |
|---|---|
| kernel microbench, f35 steady blocks, compute only | 198 |
| same kernel inside the campaign, all frames and classes | 346 |
| + block dedup/merge, import, export = the compiled engine | 415 |
| + deopt machinery and drops = the "frame body" | 1,218 |
| + row keying and the abstraction = `fwd.interpret` | 2,411 |
| + merge, subtract, save, checkpoints = the forward stage | 3,095 |

The 1.7x from 198 to 346 is one favourable frame against the whole run.
Everything after that is not the kernel.

"Rest (drops, moves)" is a RESIDUAL, not a measurement: frame-body
thread-time minus the compiled engine, the deopt retry, the plain re-run
and the snapshot. By construction it cannot overlap them, but it does
absorb any under-measurement in them, so read it as an upper bound. What
belongs in it: dropping the input `State` and its snapshot at end of
scope, moving the output `Vec<State>`, the `catch_unwind` guard, and on
the compiled path the exported states and imported blocks discarded when
an attempt unwinds. The interpreter's is 55 ns; the compiled path's 169
ns is consistent with its much larger allocation churn.

Caveats: sub-phase wall figures apply worker thread-time ratios to
`fwd.interpret`'s wall (the parts run back to back in the same workers,
so the split is proportional, but derived rather than measured directly);
and the keying cost scales with pre-subtract OUTPUT lanes, so its
ns/input-lane is a rate for the run, not a cost per key. The
instrumentation is always on and free - 162.17 s here against 162.32 s
for the same run before it existed.

# Kernel vs interpreter, frame body only, same frame (2026-08-18)

The headline "100-180x" (task #124) is a CONCRETE per-frame-lane figure:
compiled code against the interpreter executing ONE lane at a time. The
abstract search never runs in that regime - it is a vectorized interpreter
that runs one instruction across thousands of lanes - so that ratio does
not transfer, and the number below is what does.

Both sides: real f35 checkpoint states from `room10-newlua-bench`, frame
BODY only (no abstraction, dedup or merge), best of N, 7950X3D (16 cores /
32 threads). `--kernel-bench` is bind + gather + `frame` with the output
callback black-boxed; `--interp-bench` is `interpret_prepared_cfg` on the
CAMPAIGN recipe (`rewrites.jsonl`, not the compile overlay, so the
interpreter is at its best). Normalized per ROW-INPUT-FRAME: one abstract
state advanced one frame under one of the 64 button inputs.

| threads | kernel (114,458 steady lanes) | interpreter (187,859 lanes, best lane cap) |
|---|---|---|
| 1 | **31.5 ns** | **39.8 ns** (cap 200k; 43.3 at cap 8k) |
| 8 | 4.31 ns | 8.29 ns (cap 8k) |
| 16 | 3.09 ns | **6.35 ns** (cap 8k) |
| 30 | **2.21 ns** | 7.15 ns |
| 32 | 2.56 ns | - |

So: **1.26x single-core, 2.06x at equal thread count, 2.87x best against
best.** Same order of magnitude, not two.

Two caveats, both of which make the kernel look BETTER than it is:

- The kernel number is compute only - the output callback is
  `black_box`ed, so materializing rows, keying them and pre-deduping them
  are all excluded. Those are 85% of the compiled frame body in the real
  engine (see the next section). The interpreter number INCLUDES producing
  its output states.
- The kernel figure covers the steady class only (114,458 of 187,859
  lanes at f35); the interpreter's covers all of them.

Scaling: the kernel gets 10.2x from 16 threads and 14.2x from 30, and
regresses at 32. The interpreter gets 6.3x at 16 and gets WORSE past that
- SMT does not help it, and its lane cap matters a lot (cap 32k costs it
2.4x at 16 threads, because 40 states in 6 chunks cannot fill 16
workers).

This is the number to quote when asking what a kernel is worth. It also
explains the campaign A/B below without any appeal to integration
overhead: a 2x compute engine, whose compute is 3% of its own frame body,
against an interpreter that amortizes across lanes.

# P2 step 1: the sweep stops merging states it throws away (2026-08-18)

The backward loop replays a frame, reads `(origin, row key)` pairs off
each output state, and discards the states. The k-way same-shape merge in
the boundary is therefore work thrown away, and its dedup buys nothing
there anyway: the sweep's per-lane `SWEEP_ORIGIN` column makes every row
distinct. `AbstractRun::skip_boundary_merge` stops the boundary after the
abstraction and the GC; the sweep also now TAKES the output states instead
of borrowing and cloning each one to strip the origin column.

Room (1,0), synthetic win at (64,44), H=68, 9,383,878 expansions, 16
threads. `CELESTE_SWEEP_MERGE=1` restores the merge for A/B.

| | merge | no merge |
|---|---|---|
| sweep wall (2 runs) | 91.37 / 91.20 s | **85.76 / 85.04 s** |
| peak RSS | 8.84 GB | 8.49 GB |
| `bwdt.replay` | 48.28 s | **27.01 s** |
| ...of which `fwd.merge` | 22.63 s | 1.07 s |
| `bwdt.keys` | 6.19 s | **21.42 s** |
| `bwdt.index` | 18.23 s | 18.12 s |

`g.bin` **byte-identical**, optimal win frame 64, `out_of_table` 0 both
ways.

So the honest reading is not "-44% on the replay" but **-6.5% on the
sweep**: the merge was doing materialization that `bwdt.keys` then rode on
cheaply, and most of what the merge stops paying, the per-fragment key
read starts paying. Taking rather than cloning the states is worth ~1%
(85.4 vs 86.3 s wall, two runs each) - real but small, and it moves the
drop INTO `bwdt.keys`, which is why that phase's number looks worse than
the wall clock does.

The remaining P2 prize is now `bwdt.keys` at 21.4 s (25% of the sweep) and
`bwdt.index` at 18.1 s (21%) - i.e. option 3 of the plan, the generated-key
idea that took row hashing off the probe's profile. The merge itself is
done.

# `--variant` on every replay stage (2026-08-18, task #114)

`pos-graph` and `sweep` now take the same `--variant` set as `bench`, and
ladder.sh passes `${VARIANT_ARGS[@]}` to all three. Not for correctness -
dispatch is semantically invisible, so a variant-free replay of a
variant-recorded forward pass is legal - but so that a WRONG variant shows
up as a disagreement between stages instead of as a campaign whose stages
quietly disagree.

GATE, room (1,0), synthetic win at (64,44), H=68, an IDENTITY variant
(`--variant 'player|player_spawn=rewrites.jsonl'`, i.e. the base recipe
registered as a variant of itself, so any difference is the dispatch
machinery and nothing else):

| stage | without | with | result |
|---|---|---|---|
| `pos-graph --frames 68` | 141,236 pairs / 3,677 cells, 137.0 s | same, 150.0 s | `posgraph.bin` **byte-identical** |
| `sweep --frames 68 --horizon 68` | 511,124 of 55,958,742 rows reach the exit, 9,383,878 expansions, optimal 64 | identical | `g.bin` **byte-identical** |

Both sides did real work - this is not a vacuous gate: the sweep's backward
loop ran 9.4M expansions and the pos-graph was rebuilt from replay in both
runs. The identity variant also exercised the FALLBACK path: it is the
recipe's own program, whose `assert_true` premises do not hold on the
late `will_restart` states, so those frames printed "falling back to the
base program - a registered variant's premises must hold for its shape,
fix the registry" and ran the base. Loud, and the artifacts still came out
identical, which is exactly the behaviour a variant registry is supposed
to have.

The variant registry is built by a `VariantBuilder` closure rather than
handed over as a `Vec<Variant>`, because a sweep creates two replay
engines in sequence (pos-graph recorder, backward loop) and `Variant` owns
a `FixedEnv`, which is deliberately not `Clone`.

# The compiled engine inside the campaign (2026-08-18, P1 stage 3)

`CELESTE_COMPILED_FORWARD=1` replaces the campaign's frame body with
`compiled::FrameEngine::run_frame_chunk` - kernels where they bind, the
interpreter where they do not, with the campaign's own boundary, frontier
subtract, band and row table left in place. `=check` runs both engines on
every chunk and compares canonical row-key SETS. Room (1,0), 16 threads,
identical lane counts on every row of every table here.

| f45, NO frontier subtract | wall | peak RSS |
|---|---|---|
| campaign, interpreter | 15.64 s | 5.41 GB |
| campaign, compiled | **8.85 s** | **1.03 GB** |
| `native-probe --abstract 45`, the engine standalone | 8.74 s | - |

| f55, `CELESTE_FRONTIER_ONLY=1` | interpreter | compiled |
|---|---|---|
| chunk cap 4,000 | 13.17 s / 2.09 GB | 12.99 s / 1.77 GB |
| chunk cap 8,000 (the campaign default) | **11.93 s** / 2.27 GB | 13.44 s / 1.87 GB |

**1.77x on the un-subtracted search, 9-13% SLOWER under the frontier
subtract - and the ladder runs with the subtract.** The compiled campaign
at f45 is within 1% of the engine running standalone, so the insertion
point is not what is costing: the engine is genuinely not faster than the
interpreter on a frontier-subtracted workload. This refutes the
`fwd.interpret` 195/248 s projection in the cost breakdown below; do not
re-plan off that row without re-measuring.

Where the compiled frame body's time goes (f55 frontier-only,
`CELESTE_CHUNK_PHASE_TIME=1`, summed over worker threads):

```
  import        0.63s   0.6%      <- the State <-> block bridge is 1.4% total
  partition     0.31s   0.3%
  run          85.11s  84.9%
  dedup+merge  13.31s  13.3%
  export        0.85s   0.8%
```

So it is the kernel, and inside the kernel it is the OUTPUT handling, not
the compute: the compile recipe's `expand_bool` overlay turns the btn
diamonds into lane expansion, so one row is emitted per (lane, fork
config, button variant) and pre-deduped. Pre-dedup is load-bearing -
`CELESTE_PREDEDUP=0` takes that f55 run from 13.4 s to **94.4 s**. Profile
at f50: `run_class_kernel_steady::{{closure}}` 19.2% of cycles,
`Rt2::boundary` 6.8%, hashbrown insert 6.1%, `kernel_gen_steady::frame`
3.1%.

Two real gains, f60 frontier-only: `fwd.merge` 2.47 s -> 1.46 s and
`fwd.boundary_gather` 0.57 s -> 0.26 s (8,854 fragments over the run vs
22,193), which is part of P2's prize as a side effect; and peak RSS down
13-80% depending on mode.

Knobs, both measured: the campaign chunk cap is best at 4,000-8,000 under
either engine (1M is 2.6x worse - one chunk per frame is no parallelism),
and the compiled path's INNER chunk defaults to 2,048 rows rather than
`step`'s 256, because its input is already a campaign chunk (f50: 8.71 s
at 256, 7.50 at 1024, 7.43 at 2048, 7.89 at 8000).

Refused loudly rather than served wrong: `CELESTE_REM_BITS != 0`, any spd
rung, `--variant`, and position-graph recording. The first two because
`Rt2::boundary` implements the level-0 rem widening and nothing else.

# Top-down campaign cost breakdown (2026-08-18, synthetic win)

CAUTION: the `fwd.interpret` share below is what motivated P1, and P1
stage 3 measured that replacing it with the compiled engine does not help
under the ladder's configuration. See the section above.

The engine numbers below this section are ONE STAGE of the campaign. This
section costs the whole pipeline, so the engine work can be priced against
the thing it is supposed to speed up.

Setup: room (1,0), `CELESTE_WIN_AT_XY=64,44` - the witness trajectory's
position at frame 70, so real wins exist at a horizon reachable in
minutes instead of the real exit's 90-100 frames. The synthetic win is in
the campaign fingerprint, so these artifacts can never be confused with a
real campaign's. 30 cores, `--memory 60G` tripwire, chunk caps 8000.
Level-0 forward to H=72 leaves 5,136,510 lanes; first win at frame 66.

## Level 0 at H=72

| stage | wall | peak RSS | what dominates it |
|---|---|---|---|
| forward bench 0->72 | 248 s | 8.1 GB | `fwd.interpret` 195 s = **79%** |
| pos-graph (full build, UNFUSED - see below) | ~242 s | 5.0 GB | frame replay |
| sweep | 276 s | 15.1 GB | `bwdt.replay` 190 s = **69%**, keys 34 s, index 25 s |
| **level 0 total** | **~12.8 min** | 15.1 GB | |

pos-graph is incremental: measured 52 s to reach H=60, +53 s to 65, +92 s
to 70, +45 s to 72. The ~242 s is the sum, i.e. what a from-scratch build
at H=72 costs; a ladder that extends by 2 frames pays only the 45 s.

## Banded levels at H=72 (the k ladder)

| k | bench | pos-graph | sweep | total | min(e+g) |
|---|---|---|---|---|---|
| 0 | 248 s | ~242 s | 276 s | ~766 s | 64 |
| 1 | 27 s | 22 s | 86 s | 135 s | 66 |
| 2 | 18 s | 19 s | 64 s | 101 s | 67 |
| 3 | 19 s | 19 s | 62 s | 100 s | 68 |

The tube works: a banded level is ~100-135 s against level 0's ~766 s,
and peaks drop to 1-4.5 GB. Extrapolating k=4..16 at ~100 s, a FULL
16-level ladder at one horizon is **~40 min**, of which level 0 is a
third.

The `pos-graph` column above is now GONE (P0b, 2026-08-18): every banded
level borrows level 0's table (`sweep --pos-graph-from`, ladder.sh
`SHARE_POSGRAPH=1`), which removes 19-22 s x 16 levels ~ **5 min per
horizon** and leaves a banded level at ~80-115 s. It is not a heuristic:
level 0 over-approximates every level above it, so its table CONTAINS
theirs, and a superset only shrinks the sweep's candidate set. Measured
per level rather than assumed - at H=40 k=1/2/3 have 1,404 / 965 / 640
pairs against level 0's 21,324, with ZERO pairs outside it, and `g.bin`
is byte-identical rebuilt vs borrowed (`posgraphsharecheck.sh`, which
also checks that the finer direction and a different search are refused). The ladder also converges as designed - 64, 66, 67, 68 climbing
toward the true first win (the witness reaches the target at frame 70).

## Scaling with frames

Forward, per frame (level 0): 0.05 s at f30, 0.17 s at f36, 0.57 s at
f42, 1.65 s at f48, 2.85 s at f54, 5.76 s at f60, 17-24 s at f66-72.
Lanes 11.7k -> 5.1M. Superlinear until the frontier saturates.

Sweep, whole stage: 20 s at H=60, 39 s at H=65, 160 s at H=70, 276 s at
H=72 - it grows FASTER than the forward pass and overtakes it (at H=60
the forward's cumulative cost is ~60 s against the sweep's 20 s; by H=72
it is 248 s against 276 s). Peak RSS 2.2 -> 15.1 GB over the same range.

## What the stages ARE, and what is actually inside them

Three separate processes, sequenced by ladder.sh through files - not one
fused pass:

- **forward** (`bench`) - the abstract search. Writes checkpoints, the
  per-frame boundary batches the other two stages read, and the visited
  rowkeys.
- **pos-graph** - a coarse over-approximation of the predecessor
  relation PROJECTED ONTO PLAYER POSITION and nothing else: ~385k
  `(dst cell, src cell)` pairs, ~1.5 MB for room (1,0). One node per
  position cell, never per row. Horizon-INDEPENDENT - a property of the
  game's geometry, built once per room.
- **sweep** - the backward pass in time-expanded space. Uses the
  pos-graph only to SHRINK the candidate set, then re-runs the frame
  function on those candidates; that expansion is what establishes an
  edge. Emits `g.bin`.

So "step back along the coarse graph, then step forward exactly to find
the true back edges" is the sweep, and it is `bwdt.replay`.

Phase attribution, measured (metrics phases are INCLUSIVE, so nested
ones double-count - `fwd.*` inside the sweep are sub-phases of replay):

| | forward 248 s | sweep 276 s |
|---|---|---|
| `bwdt.replay` | - | 190 s (69%) |
| ...of which `fwd.interpret` | 195 s (79%) | 88 s (32%) |
| ...of which `fwd.merge` | 8 s (3%) | 91 s (33%) |
| `bwdt.keys` / `index` | - | 34 s / 25 s |
| boundary stream + save | 31 s | - |

The correction that matters: the FORWARD pass is interpreter-bound
(79%), but the SWEEP is only a third interpreter - an equal third is the
merge/regroup row machinery, because the sweep regroups candidates from
many discovery frames into different lane groups than the forward used.
A compiled kernel in the replay therefore addresses ~1/3 of the sweep,
not ~2/3; the row-machinery work (pre-dedup, keys) addresses another
third. Earlier text here claimed "~75-80% of the campaign is frame
execution" - that is right for the forward stage and wrong for the
backward one.

## pos-graph does not need its own stage

`bench --record-pos-graph` (ladder.sh `FUSE=1`) records the table DURING
the forward pass. Measured at H=72, same synthetic setup:

| | wall | peak |
|---|---|---|
| forward, then pos-graph as a stage | 248 + 242 = 490 s | 8.1 / 5.0 GB |
| forward with `--record-pos-graph` | **265 s** | 8.2 GB |

Fusing costs 17 s (7%) on top of the forward and removes a 242 s stage:
**-46% on level 0**. The recorded table is also a strict superset
(166,456 pairs / 4,189 cells vs the replay's 154,937 / 3,928 - it sees
the spawn's first move, which a replay starting from the frame-1 batch
cannot). It is OFF by default because it is gated only on room (2,0)
(`posgraphcheck.sh`); running that gate on room (1,0) is the cheapest
campaign-level win available.

Caveat: these are shape-of-curve numbers at a mid-room synthetic target.
A real room (1,0) campaign runs to H~90-100 where both stages are much
bigger; the RATIOS are what transfers, not the absolute minutes.

# The kernel engine on the one-frame dev-loop bench (2026-08-18; plans/kernel-plan.md)

Same bench as the tile table below (`--abstract-bench room10-newlua-bench
35`, 187,859 lanes in, 269,059 out, f37 chase 365,029, 30 cores), so the
numbers are directly comparable. Every row here is gate-2 EXACT (row-key
SET EQUAL) at f20/f25/f30/f35.

| engine | one frame | vs Rt2 |
|---|---|---|
| Rt2 columnar (reference) | 2.0 s | 1x |
| Rt3 dynexp, tuned (CELESTE_TILE=2) | 518 ms | 3.9x |
| kernel, steady class only (2026-08-18 night) | 380 ms | 5.3x |
| kernel, all three classes (steady/dash/frozen) | 348 ms | 5.7x |
| + pre-dedup from kernel registers | 134 ms | 14.9x |
| + chunk 64 -> 256 (the trade-off inverted) | **89 ms** | **22.5x** |

CAUTION on that 89 ms: it does not reproduce. Re-measuring the SAME
binary (6ea6937) on 2026-08-18 afternoon gives 103.0 ms min / 110.5 mean
over 30 reps. Machine state, not a code change - but it means the 89 is a
best-case number and any comparison against it has to rebuild the
baseline rather than read this row. The P1 crate split was A/B'd that way
(plans/campaign-cost-plan.md): post-split is 106.8 ms min against a
rebuilt 103.0 ms baseline, +3.7%, plus a separate +6% if the compiled
path is built with debuginfo (which is why it is not).

100% of player lanes at f35 run compiled kernel code (steady 114,458 +
dash 43,824 + frozen 29,577, missed 0). The dash and frozen classes
ignore the buttons almost entirely - the emitter's taint analysis found
that on its own - so they enumerate 4 and 1 button variants where steady
enumerates 64.

Pre-dedup is the lever that mattered, and it is an OUTPUT-side lever,
not a compute one: a chunk emits one row per (lane, fork config, button
variant) - 25.1M per f35 frame - and 8.3 of every 9 are duplicates that
boundary discarded AFTER they were materialized and hashed. Keying rows
straight out of the kernel's output registers and consulting a per-chunk
seen-set before `append_out` cut materialized rows to 3.03M and the
frame to 134 ms. The register-side key mirrors boundary's value
canonicalizations exactly, which is measurable rather than asserted: the
residual within-chunk dedup ratio after boundary went 8.3:1 -> 1.0:1.

Chunk size then inverted. It used to trade mid-frame traffic against
dedup ratio with small chunks winning; with duplicates dying as a hash
probe, more lanes per chunk simply means more duplicates caught:

| chunk lanes | 64 | 128 | 256 | 512 | 1024 | 4096 |
|---|---|---|---|---|---|---|
| before pre-dedup | 348 ms | 489 | 584 | 673 | - | - |
| after pre-dedup | 133 ms | 98 | 91 | 86 | 82 | 85 |
| peak RSS | 0.99 GB | - | 1.12 | 1.44 | 2.02 | 4.82 |

256 is the default: 1.46x for +13% memory, mean as tight as the min.

Profile at 89 ms (25 timed reps, chase excluded as far as sampling
allows): kernel arithmetic 16.6%, append+key callback 17.1%, boundary
15.1%, the non-kernel fallbacks (Rt3 + Rt2) 21% - those last are the f36
shapes the gate chases, and they are what K4/K5 address next.

## The multi-frame number, and why the one-frame bench was flattering

The one-frame bench feeds the engine INTERPRETER checkpoints, which are
already pm1-partitioned, so they bind and 100% of lanes run kernel code.
The engine's own output did not bind: at f35 the very next frame ran
269,059 lanes with ZERO kernel coverage. Two partitioner bugs (see the
commits): pm1 resolved player fields off the player TYPE table instead
of the instance, and partitioning left the split cell a constant Col::N
where bind requires Col::U - including on the early-return path taken
when a block is ALREADY pure, which is the common one.

`native-probe --abstract 34` (34 frames from the room start, lane counts
identical throughout - gate 1):

| | 34 frames | frame 34 (132,153 lanes) |
|---|---|---|
| before today | 824 ms | 300 ms |
| pm1 instance + collapse on split | 471 ms | 158 ms |
| + collapse on the pure path (100% coverage) | **175 ms** | **52 ms** |

The lesson worth keeping: measure the engine on ITS OWN output. A bench
that replays interpreter states measures a path the campaign never
takes, and it hid a 100%-fallback frame for a full day.

CAUTION, learned the hard way (2026-08-18): regenerating a kernel
invalidates every gate. The dash kernel shipped overnight emitting ONE
button variant instead of 64 because a tainted output cell had no
instruction behind it, and the f35 gate caught it the next morning
(1,358 rows missing at f37). Re-run the gate after every regen.

# Tile engines on the one-frame dev-loop bench (2026-08-17; plans/columnar-engine.md)

`--abstract-bench room10-newlua-bench 35`: real f35 campaign states,
187,859 boundary lanes in, one frame, exactness oracle 269,059 lanes
out + f37 chase 365,029 (all configs below EXACT). 30 cores.

| engine | one frame | ns/input-lane |
|---|---|---|
| Rt2 columnar (reference) | 2.0 s | 10.6k |
| Rt3 tiles, concrete buttons x64 variants (CELESTE_TILE=1) | 8.16 s | 43.4k |
| Rt3 dynamic in-tile expand (CELESTE_TILE=2), first light | 1.55 s | 8.3k |
| + uniform-cond select pass-through | 1.49 s | 7.9k |
| + typed panes (N=[P8;64] pool, B=u64 lane mask) | 1.32 s | 7.0k |
| + per-chunk template, undo-log reset (no clones/row) | 1.26 s | 6.7k |
| + typed append (Col::N stays typed through the boundary) | **1.04 s** | **5.5k** |

Evening ladder, all steps EXACT + guard + gate-1-f40 gated. Post-panes
profile: per-op arithmetic is out of the top table; the frontier is
append_into 12% + boundary 14.5% + generated-code self 15%. TILE=64 +
panes also took mode 1 to 5.39 s (kept as a control).

CELESTE_TILE=2 = trunk sharing: one boundary row per tile, the input
fan-out grows the lane axis in-tile (expand doubles width 1 -> 64), so
the pre-input physics runs once per row for all 64 variants. 5.3x over
the concrete-button tiles, and no Rt2 fallback fires (0 bails).
Slot compilation (102 sites -> 28 slot cells) is IN but measured
neutral; the profile says out-of-line ops (select 19%, av_addsub 17%)
block all folding, and force-inlining them is a measured LOSS (9.25 s,
icache). Slot binding is SHAPE-SCOPED (gen::SLOT_SHAPE): off-shape
blocks (spawn/death/other rooms) deopt to Rt2 - frame 1's spawn shape
binding steady-state cell ids was a real caught bug. Gate 1 from
scratch f1..f40 EXACT (902,280 lanes) in both tile modes;
CELESTE_SLOT_GUARD=1 passes everywhere (~2%).

# Columnar abstract engine v0 (2026-08-18 overnight; plans/columnar-engine.md)

Gate 1 (per-frame lane counts vs `rewrite bench`, room (1,0) level 0,
f1..f30): EXACT every frame. Timings, f30 = 27,024 boundary lanes
(~1.7M offered after the btn fan-out):

| config | f30 | 30 frames |
|---|---|---|
| interpreter (`rewrite bench`, 1 core) | 0.08 s | 0.22 s |
| columnar serial (CHUNK=64, typed cols) | 2.1 s | 3.5 s |
| columnar parallel (30 cores) | 1.22 s | 2.10 s |
| + COW lane-indirection columns, serial | 1.21 s | 2.0 s |
| + COW, 30 cores | **0.144 s** | **0.29 s** |

Gate 1 also EXACT through f40 (902,280 lanes; f31..f40 counts equal
the interpreter's). f40 wall 14.2 s parallel vs interpreter 0.99 s -
at depth the boundary/dedup path is the new frontier (see
plans/columnar-engine.md).

Per-op census (serial): widen (lane-append at expand/split sites)
1.74 s = 48%, map2 0.53 s, select 0.43 s. Named fixes in the plan
(COW/lane-indirection columns first). Eager frame-start button
expansion measured OUT (7.0 s serial - the whole frame runs 64x wide).

# Native probe, zero-divergence program (2026-08-18 overnight)

Compile-only recipe (`rewrites-compile.jsonl` with the zr/zs/zt groups:
cells-mode forward-cse, 5 dead cells dropped, freeze gate + dash trigger
converted to masked regions). Censuses at room (1,0) f35 bench states:
branch census 0 divergent of 26 executed (0 panics), gap census 0
multi-receiver / 178 single-receiver (0 panics). Single-lane concrete
replay, core 15, seed-7 trajectory, 1000 reps x 340 frames:

| program | ns/frame |
|---|---|
| zero-divergence probe (this) | 2922 |

Hex-identical to `concrete_run` through f340; the trajectory's death
frame (341) exits the compiled shape's domain with a loud premise assert
(`count(objects)==1`), identical at HEAD - see plans/native-probe.md
"The hex-exactness oracle, corrected".

# The visited set left RAM: fp-runs + mmap'd rowkeys, certified identical
# (2026-08-16, tasks #115-#120; design in plans/visited-redesign.md)

The cross-frame visited set was an `FxHashMap<(u64,u64), u32>` - measured
~33 B/row, 25 GiB pinned at room (2,0) f073's 814M rows - plus a 12.4 GB
`visited.bin` duplicated into EVERY checkpoint dir (x19 = 62 GB). It is
now: sorted fingerprint runs in RAM (10 B/row, only filters), per-frame
`frames/*.rowkeys` files (sorted 128-bit keys + ids, uncompressed,
mmap'd), and an INLINE exact confirm behind every fp hit - a pinned
per-file sample index brackets each confirm to one ~3 KB block, so under
memory pressure the worst case is one page fault per confirm, and the
residency that used to be an OOM-kill cliff is now evictable page cache.
No lossy structure ever decides a drop; the 128-bit guarantee is exactly
today's.

## Deep frame, room (1,0) f100->f101, fresh post-fix derivation
## (386M offered lanes -> 6.28M new, 225.3M rows total; single run each,
## one idle-ish core of background noise, identical for all rows)

| engine | total wall (incl. resume) | fwd.interpret | peak RSS |
|---|---|---|---|
| map (old) | 110.4 s | 62.6 s | 17.5 GB |
| mmap, naive binary search | 133.2 s | 108.6 s | 16.3 GB |
| + sample index & interpolation search | 108.1 s | 85.2 s | 16.8 GB |
| + local-first chunk dedup | **95.9 s** | 72.9 s | 16.5 GB |

* The resume half: map loads visited.bin and rebuilds the 225M-entry map
  (~44 s); mmap scans the already-sorted rowkeys into runs (~22 s). The
  ladder resumes per horizon, so this recurs.
* Peak RSS understates the change: the mmap engine's residency INCLUDES
  ~5 GB of file-backed, evictable pages. The pinned share fell ~7.4 GB
  -> ~2.3 GB; at room (2,0) f073 scale that is 25 GiB -> 7.6 GiB
  (+ 72 MB of sample indexes), linear in rows.
* Local-first dedup: the old measurement ("local set first is 4% worse")
  was taken when a global probe was one map lookup; with the costlier
  mmap probe the trade flips, so the filter order is now an engine
  property (`Visited::local_dedup_first`). Candidates are identical
  either way.

## Gates (all passed)

* f30 room (1,0), map vs mmap: all 60 frame+rowkeys files, states.bin,
  meta counts byte-identical; re-proved after each tuning step and after
  the default flip.
* Cross-engine resume, both directions, f30->f32: byte-identical.
* Sweep + pos-graph on a visited.bin-less dir: full bijection asserts
  pass on the table rebuilt from rowkeys.
* `migrate-visited` on the old room (1,0) campaign: 212.6M rows, keys
  from states + ids from visited.bin, rebuilt table equal id-for-id
  (1:54 wall / 20 GB peak; load 37.3 s, hash 15.9 s, write 17.1 s).
* Full `cargo test --release`: green (engine landed; the tuned probes
  additionally carry a differential unit test against reference
  searches, and the scoped checkpoint/sweep/state-mapping tests were
  re-run under the new default).

Default engine is now mmap for runs with a checkpoint dir
(`CELESTE_VISITED_ENGINE=map` selects the old one; dir-less runs keep the
in-RAM map). Deliberately NOT in the campaign fingerprint - trajectories
are certified identical and checkpoints interchangeable, like variants.

Note for interpreting older sections: the fresh room (1,0) derivation
under the post-console-fix tree reaches 219.0M rows and first room-exit
at LEVEL-0 frame 89 (was 90); the certified optimum of 100 predates the
fixes and re-certification is the owed B5 ladder campaign.

# The room (0,0) S2 variant is speed-neutral, and the control says why
# (2026-08-16)

Task #86 / plan item B2: derive a recipe specialised for room (0,0)'s
dominant object-array shape S2 = `[fake_wall, player]` and register it as a
shape variant. The recipe exists, is verified, and **buys 0.0%.** The
control experiment below says that is not a derivation failure - it is
where the time in room (1,0)'s extra 232 entries actually sits.

## The headline, room (0,0) f048, idle machine, 3 runs each

`rewrite bench --frames 48`, host `rewrites-room00.jsonl`, variant
`--variant 'fake_wall,player=rewrites-room00-s2.jsonl'`:

| | wall | lanes | peak RSS | us/lane |
|---|---|---|---|---|
| without the variant | 22.09 / 22.00 / 21.92 s | 4,614,581 | 8.18 / 8.17 / 8.16 GB | 4.8 |
| with the variant | 22.11 / 21.93 / 22.02 s | 4,614,581 | 8.20 / 8.20 / 8.21 GB | 4.8 |

Not "within noise" in the hand-wavy sense - the phases agree too:
`fwd.interpret` 7.56 s vs 7.59 s, `fwd.merge` 11.49 s vs 11.56 s. And the
variant is not idle: 2,057 states / 12,741,383 lanes ran under it, which is
EVERY lane from frame 28 (the spawn) to frame 48. Zero fallbacks.

Worth reading off the same table: at this depth the frame body is 35% of
wall time and the boundary merge is 53%. Program specialisation is bidding
for a third of the clock before it starts.

## The control: what the 232 missing entries are actually worth

Room (1,0) can run both recipes, so the gap is directly measurable there.
f037, 3 runs each, lanes identical (365,029) in all four:

| recipe | entries | wall | peak RSS |
|---|---|---|---|
| base `rewrites.jsonl` | 889 | 1.82 / 1.81 / 1.81 s | 0.71 GB |
| base minus `h068`,`h069` | 887 | 1.74 / 1.76 / 1.79 s | 0.65 GB |
| base minus the 34 shape entries | 855 | 2.00 / 1.98 / 2.00 s | 0.65 GB |
| `rewrites-room00.jsonl` | 657 | 2.04 / 2.04 / 2.04 s | 0.64 GB |

So the whole 232-entry gap is **0.23 s, 11.3%** - not the 26% the entry
count suggested - and **0.19 s of it (83%) is 34 entries**: the 16
`collapse_loop`s, the 16 `assume_eq`s that follow them, and two
`unroll_loop`s. The other 198 are worth 0.04 s, at the edge of the noise.
The two `unroll_loop`s are worth nothing at all on their own (removing them
is, if anything, faster).

Those 34 are exactly the 34 the S2 screen dropped, and the reason is not
mechanical. `collapse_loop` collapses a counted loop by asserting
`bound == init`, a trip count of ONE; room (0,0) has TWO objects, so all 16
guards fire at frame 29 - loudly, which is the rule working. But the prize
in room (1,0) was never the collapse (its own commit measured it "small on
its own"); it is the `assume_eq` that follows, folding `objects[1] == this`
so that every `check`/`collide` in the room returns nil statically.

**That fold does not exist in room (0,0), at any trip count.** The second
object is a real `fake_wall` the player collides with - `obj.is_solid`
checks `fake_wall` on every pixel step and the answer is a genuine bbox
test, not `nil`. A two-trip collapse rule would remove the loop heads and
constant-fold the `get_index`; it would not produce the fold, because
`anonymous_61` is ONE function called for both objects and neither
iteration's identity is static inside it.

Getting the fold back needs the whole of room00-plan.md step 3: unroll the
OUTER `foreach(objects, ...)` into two copies, inline `anonymous_61` per
slot, and only then collapse and `assume_eq` the inner loops per slot. That
is new rule work (rewrite-plan.md's unimplemented `peel`, plus a per-slot
inline), not a recipe edit, and it is the only thing on this room's
critical path that the measurement supports.

## Registering a variant used to cost 3.6x

Found while benchmarking, fixed before measuring anything else. `step_inner`
refused the chunk-parallel path whenever variants were registered, because
`dispatch_variant_frame` took `&mut self`. Room (0,0) f040, the host recipe
registered as a NO-OP variant of itself:

| | wall | us/lane |
|---|---|---|
| no variant | 2.13 s | 5.5 |
| no-op variant, before | 7.59 s | 19.6 |
| no-op variant, after | 2.13 s | 5.5 |
| no variant, `CELESTE_FRAME_THREADS=1` | 7.22 s | 18.6 |

The last row is the control: the 3.6x was threading, not dispatch. Dispatch
itself - the shape probe plus the base -> canonical -> variant -> canonical
-> base round trip - is 7.59/7.22 = 5%. Dispatch is a pure function of the
state, so it now lives in `interpret_state_base` behind a shared borrow and
rides the workers like everything else. `parcheck.sh 45 8000` still passes
byte-identically.

## "Dispatch is invisible" holds for the row sets, not the row ids

Room (0,0) f040 under campaign settings (frontier-only, deopt, 8000-lane
chunks), with and against the S2 variant:

* identical: fingerprint, `row_count` 387,443, all 40 per-frame watermarks,
  `state_count`, `lane_count`, and every per-frame `frontier-only: -> N new
  lanes, visited total M` line;
* different: `states.bin` (319,986 vs 316,684 bytes) and `visited.bin`.

A variant frame emits its raw lanes in a different order and multiplicity -
fewer duplicates, which is what `if_convert`/`speculate` do - and row ids
are assigned in insertion order.

This is NOT the chunk cap's kind of semantic: the cap changes the reachable
SET on rooms with fruit, and this changes nothing but the numbering. So
`ladder.sh`'s VARIANTS note is right that checkpoints stay interchangeable
and a variant run can resume from a variant-free tree - everything
downstream reads ids out of the tree it was handed. Two narrower things do
follow, and neither was written down before:

* artifacts are not BYTE-comparable across the setting, so a
  `parcheck.sh`-style byte gate has to hold the variant set fixed;
* a `g.bin` is only meaningful against the row table it was computed from,
  which is one more reason to apply the same set to every stage - exactly
  what ladder.sh already does, and it does it for the "stages quietly
  disagree" reason rather than this one.

# Room (2,0) does not fit: the ladder needs a rung below 0 (2026-08-15)

Room (2,0) is the first room the campaign cannot afford. Everything else
works - the pipeline was taken end to end on it, all 17 levels, with a
non-vacuous `g` - but the level-0 forward pass runs out of RAM around
frame 75 and the horizon it has to reach is 95. Full write-up in
`plans/room20-plan.md`; the numbers:

| frame | frontier lanes | visited rows | RSS | s/frame |
|---|---|---|---|---|
| 45 | 1,503,507 | 5.2M | 1.7 GB | 4.4 |
| 50 | 5,507,770 | 24.1M | 4.7 GB | 16.9 |
| 55 | 11,783,364 | 68.7M | 10.5 GB | 39.0 |
| 60 | 23,220,148 | 178M | 21.7 GB | 113.5 |
| 65 | 36,562,604 | | 38.3 GB | 244.3 |
| 68 | 50,674,575 | 450M | 47.7 GB | 305.3 |
| 70 | 62,890,020 | 570M | 67.6 GB | 347.9 |

f001..f070 cost 2,773 s and peaked at 76.08 GB, with the position graph
recorded inside it. Per-frame growth is still 1.09-1.11 at f070 and 570M
visited rows is already 1.4x room (0,0)'s ENTIRE campaign - which peaked at
12.1M frontier lanes at f079 and finished in 405M rows.

**Where it goes is measured, not guessed** (`CELESTE_XY_DUMP`, extended to
f070 by `rewrite field-census`, which reproduces its f050/f052 counts
exactly): the frontier occupies 4,309 whole-pixel positions at f050 and
7,590 at f070, at 1,278 and 8,286 lanes per position. So the position set is
NOT saturated - the earlier "~4,500, 96% by f050" reading was an
extrapolation from f040..f052 and is wrong - but the two terms grow at very
different rates (positions 1.76x from f050 to f070, lanes per position
6.5x), so the growth is still overwhelmingly state AT a position. With `rem`
already fully widened at level 0, that is the velocity and dash machinery.
No amount of specialization, chunking or recipe work touches it: those
change the cost per lane, and this is the number of lanes. The ladder
refines exactly one field (`player.rem`) and its coarsest rung is exact in
every other coordinate; a wide-open room wants a rung BELOW 0 that buckets
some other field the same way, with level 0 banded by it.

**Which field, priced** (`rewrite field-census`, 2026-08-16 - offline from
the saved boundary states, 12 s / 1.4 GB at f052 and ~25 min / 11 GB at
f070, so it costs no search time and cannot contaminate a benchmark).
Rows surviving if a field is collapsed, out of 5,507,770 at f050 and
62,890,020 at f070 - both reproduced exactly by the tool:

| collapsed | f050 | **f070** |
|---|---|---|
| `spd.x`+`spd.y` bucketed to 1 px/frame - the buildable rung | 27.2% | **6.9%** |
| `spd.x`+`spd.y` bucketed to 1/2 px/frame | 39.4% | 12.9% |
| `spd.x`+`spd.y` ERASED - the unbuildable upper bound | 16.9% | 3.5% |
| `p_jump`+`p_dash` erased (two booleans) | 27.6% | 26.3% |
| whole dash state machine (`dash_time`, `dash_target.*`, `dash_accel.*`, `dash_effect_time`) | 86.2% | 88.0% |
| `spd:0` + `p_jump` + `p_dash` | 8.0% | **1.9%** |

**Read this at f070, not at f050.** The `spd` rung's value grows steeply
with depth - `spd.x` goes from 187 distinct values to 3,460 - and a census
taken at f050 understates it by 4x and flips the decision. The frontier
grows 1.1228x per frame over f066..f073, so a factor F is
`ln F / ln 1.1228` frames and the wall is 20 frames short of the horizon:
the buildable `spd:0` rung is **14.5x = 23.1 frames**, which clears it;
`spd:1` is 7.8x = 17.7 frames, which does not. So the rung is the right one
but only at its coarsest setting, and the margin is thin enough to want
`p_jump`/`p_dash` (the previous frame's button state, kept for edge
detection) with it - 52x = 34 frames together, for two booleans that need no
bucketing machinery at all. The dash state machine proper is 1.14x and is
not where to look; nor is `djump`, `grace` or `flip.x`. Per-field counts
conditioned on a position do NOT multiply out - at f070 their product is
15,000x the actual row count at the median position - so nothing but the
joint measurement predicts a rung. Full tables and caveats in
plans/room20-plan.md.

Standing result for the room: the optimum is **at most 95 frames**
(`tas/room_2_0_exit_frame_95.txt`, derived from the community TAS and
verified concretely), and at least whatever frame the level-0 pass reaches
without a win.

## What DID land, and what it cost

* A soundness fix to the ladder: the fruit-`off` widening did not widen the
  bob POSITION with it, so every fruit-alive lane of the EXACT level was
  dropped from the band as an "unknown coarse row". Invisible on room (0,0)
  (its winning path has no live fruit); fatal on room (2,0), where k16 came
  out empty at frame 2 and refuted every horizon. Control: with the fix,
  horizon 34 is refuted at k=2 and horizon 35 converges through all 17
  levels at `CELESTE_WIN_AT_XY=26,108`, where `trace-witness` also passes
  every level at every frame.
* Sparse index creation in the interpreter: `got_fruit[1 + level_index()]`
  is an append only in room (0,0).
* The fused position-graph recording is now correct under `--resume` and
  gated by `posgraphcheck.sh` (row sets equal, table a strict superset, `g`
  identical as a function of the row). `ladder.sh FUSE=1` uses it, and the
  ladder now skips its level-0 extend when the tree already covers the
  horizon.
* A fidelity finding that needs a decision, not a fix by me: `foreach` is
  an index walk, PICO-8's `all()` is not, and the difference is visible in
  this room's frontier (five-object shapes after a death). See
  plans/room20-plan.md.

# Room (0,0) end to end on the fixed interpreter: 2.6 h (2026-08-10)

The full ladder re-derived from nothing on the post-fix interpreter.
**CONCRETE OPTIMUM = 94 frames**, unchanged, and all 17 levels agree: every
level's backward `min(e + g)` equals its own forward first-win frame.

    L(0)=80  L(1)=88  L(2)=88  L(3)=91  L(4)=92  L(5)=93  L(6)=93
    L(7..16)=94

That L-curve reproduces the 2026-08-07 campaign's refutation record exactly
(horizons 80-87 refuted at k=1, 88-90 at k=3, 91 at k=4, 92 at k=5, 93 at
k=7). Note that room00-plan.md's summary sentence "min(e+g)=94 at every
level" was wrong and is corrected there; the refutation list in the same
paragraph was right.

## Where the 2.6 h goes

Clean-path wall clock, 16 threads, campaign chunk settings (8000/8000):

| stage | wall | peak RSS | share |
|---|---|---|---|
| level-0 forward, f1..f94 | 1,810 s | 37.88 GB | 19% |
| level-0 position graph | 3,567 s | 101.08 GB | 38% |
| level-0 re-index | 156 s | (in the sweep) | 2% |
| level-0 backward loop | 2,088 s | 73.70 GB | 22% |
| levels k=1..16, all 48 stages | 1,766 s | 21.58 GB (k1) | 19% |
| **total** | **9,436 s = 2.62 h** | **101.08 GB** | |

k=1 is 65% of the whole k-ladder (1,147 s of 1,766 s) and k=7..16 are 14 s
each - the band collapses from 27.5M rows at k=1 to 962 at k=16.

Within the level-0 forward pass, frames 80-94 are 1,396 s of 1,804 s
(**77%**); the first 79 frames are 408 s. Phase totals: `fwd.interpret`
1,256.5 s, `fwd.merge` 219.1 s, `fwd.save_frames` 88.8 s,
`fwd.boundary_gather` 75.9 s, `fwd.boundary_stream` 53.8 s.

Within the sweep: `bwdt.replay` 1,397.7 s (of which `fwd.merge` 961.7 s and
`fwd.interpret` 340.1 s), `bwdt.keys` 569.8 s, `bwdt.index` 156.0 s.

**The old estimate was 13 +/- 2 h and is now 2.6 h.** That estimate was
built on the f94 forward pass costing 5,079 s, 81.8M deopted lanes, and a
6,171 s position graph whose f090 alone was 401.8 s of whole-state plain
fallbacks. The deopt is gone, so the forward pass is 1,810 s and the graph
is 3,567 s. Nothing else about the ladder changed.

## The sweep, against the pre-fix universe

Same room, same horizon, the only difference being the interpreter fixes:

| level 0, H=94 | pre-fix | rebuilt |
|---|---|---|
| rows | 460,658,200 | **405,616,308** (-11.9%) |
| rows reaching the exit | 24,514,668 | 24,512,603 (-2,065) |
| row-expansions | 451,884,260 | 449,013,771 (-0.6%) |
| successors outside the row table | 0 | **0** |
| `min(e+g)` | 80 | 80 |
| position graph | 405,332 pairs / 7,923 cells | 405,325 / 7,922 |
| position graph wall | 6,171.5 s | 3,567 s |
| re-index | 181.2 s | 156.0 s |
| backward loop | ~2,600 s | 2,088 s |

Zero out-of-table successors again, over 449M expansions. The row table is
12% smaller and the exit-reaching set moved by 2,065 rows out of 24.5M -
the safe direction, and the abstract bound is unchanged at 80.

The forward pass reproduced every recorded figure from the f94 A/B exactly:
21,663,480 lanes, 1,857,040 fragments, first room-exit at frame 80, and all
15 winning lane counts including the f94 **18,780** (one below the pre-fix
18,781). It ran in 1,804.41 s against that A/B's contended 1,992.93 s, and
peaked at **37.88 GB against its 31.76 GB** - the A/B got ~9.4 of the 16
cores it asked for, and fewer concurrent chunk transients is a lower peak.
Quote 37.88 GB as the uncontended figure.

## The fragment doubling BIT, and it cost 2.6 h

The f94 note below flagged fragments more than doubling (871k -> 1.86M) as
"the number to watch if per-fragment cost ever becomes significant". The
position-graph replay is exactly that place, and it OOMed:

    one process, whole range, 250k group: 96.60 GB, killed at 9,040 s

Two things that were tried and did NOT fix it, both worth recording because
they are the obvious levers:

* `CELESTE_POSGRAPH_GROUP_LANES` 250,000 -> 40,000 -> 10,000. The last frame
  OOMed at 96.58 and 96.61 GB respectively. Below f090 the knob helps a lot
  (RSS fell from 40-76 GB to 6-10 GB over f1..f80); at f091+ it does nothing.
* `CELESTE_FRAME_THREADS` 16 -> 4. Climbed monotonically to 99.8 GB and died
  anyway, 2.3x slower on the way.

What worked: one process per few frames (glibc does not hand the arenas back
between frames, and a fresh process does), plus a **108 GB** cap for the last
frame, which peaked at **101.08 GB**. Per-frame peaks are almost linear in
the frame's lane count - 74.19 GB through f090, 83.00 at f091, 91.31 at f092,
101.08 at f093 - so f094 would want ~108 GB and f095 ~115 GB. **Room (0,0)
at horizon 95 does not fit on this machine without work on the replay's
transient.** That is the concrete next constraint, not a projection.

The staged rebuild is `ladder.sh`'s new `MEM=` knob plus a pos-graph stage in
its own process; the failed attempts cost 9,439 s on top of the 9,436 s of
useful work, so the wall clock of the session was 5.2 h for a 2.6 h campaign.

### The group size does not change the answer, measured

`chunk_states` only ever SPLITS a state and never merges across states, so
the group knobs cannot change how lanes are grouped - only how many
already-chunked states are in flight. The runs confirm it on room (0,0)
itself: at group 250,000 and at group 100,000 the recorded table had the
same pair count at every printed frame - 98,742 at f050, 196,278 at f060,
324,347 at f070, 399,844 at f080. Same table, 10x less memory.

## Gates

* `trace-witness` on `tas/room_0_0_exit_frame_94.txt`, **all 17 levels**,
  every frame: PASSES, with `e+g = 94` and `g = 0` at f094.
* `tsweepcheck.sh` on room (1,0) k8: `g` still matches the certified edge
  sweep element-wise (the control - the script grew ROOM/RECIPE and this
  proves the change is inert).
* `tsweepcheck.sh` on room (0,0) k8 (169,601 rows) and k2 (6,201,650 rows):
  `g.bin` byte-identical at 1 and 16 threads, and a full re-run reproduces
  the ladder's own `g` element-wise, 0 differing entries. Room (0,0) has no
  certified `g` to compare against, so this is reproducibility and
  thread-invariance, not certification - stated plainly because the script
  still prints the word "certified".
* `cargo nextest run --release`: 488 passed. Build warning-free.
* `~/celeste-checkpoints/room1/g.bin` sha256 unchanged (`a97ca05c...`).

# The deopt is gone on room (0,0): 2.55x, -47% memory (2026-08-09)

Three interpreter fixes, all default-on, remove every plain-program
fallback from room (0,0). Full horizon, level 0, campaign chunk settings
(8000/8000), against the ladder's own `l0-h94.log` baseline:

| | before | after | |
|---|---|---|---|
| time | 5,079.41 s | **1,992.93 s** | 2.55x |
| lanes | 26,696,437 | 21,663,480 | -18.9% |
| peak RSS | 59.83 GB | **31.76 GB** | -47% |
| deopt | 10,235 states / 81,766,256 lanes | **0** | gone |
| fragments | 871,168 | 1,857,040 | **+113%** |

Per-frame the gain grows with depth, because the deopt did: f86 2.44x,
f88 2.58x, f90 2.85x, f92 2.96x, f93 2.95x. The baseline spent 42% of its
whole run in the last five frames.

## What the fixes are

Every one removes a join ACROSS LANES, where one lane's ambiguity
destroyed its neighbours' answers:

1. a MIXED interval comparison PARTITIONS the state (definite lanes keep
   a real `Bool`, straddling lanes spill into a state whose `UnknownBool`
   is then honest) instead of collapsing the whole value;
2. `select` on an `UnknownBool` CONDITION splits into a true copy and a
   false copy instead of erroring into the whole-state fallback;
3. `select` with one `UnknownBool` boolean ARM yields a per-lane
   tri-state, which (1) then resolves.

They only pay off together. Frame 68 recorded ZERO splits with only (2):
`select cannot combine UnknownBool and Bool` fired first and dropped the
state before any split could happen. Fixing one link moves the failure
one instruction down the same `and` chain in `obj.collide`.

## The cost, stated plainly

Fragments MORE THAN DOUBLE (871k -> 1.86M, peak 72,617 -> 181,198 per
frame). At f70 the increase was only 2.5%, so this is a depth effect: the
splits do multiply where the strawberry's chain is hot. The boundary
merge absorbs it - lanes and memory both fell - but anything that makes
per-fragment cost matter more should expect this to bite.

## Why it is precision and not a leak

* `trace-witness` passes at every frame through f070, including every
  frame where splits fire, against 115,656,896 rows.
* The lane reduction is exactly 0.00% at f65 and turns on at f66, where
  the first straddling comparison appears; it is monotone and never
  negative-signed the wrong way.
* The first-win frame is UNCHANGED at 80, and the winning lane counts are
  identical at f80-f93 (1040, 4268, 3472, 5030, 8966, 8770, 10096, 12794,
  14754, 14570, 9440, 8188, 4474, 8400).
* Room (1,0) is byte-identical with all three fixes on or off, so its
  certified campaign stands.

ONE EXCEPTION, which should not be buried: at f94 the winning lane count
is 18,780 against the baseline's 18,781. One fewer. That is the safe
direction - a refinement can only shrink the abstract set, so a spurious
winning state was dropped - and the reported optimum is set by the FIRST
win at f80, which is unchanged. But it is a change to the winning set and
it is the only frame of 15 that moves.

## Lane independence, certified with a control

`simdcheck` on room (0,0):

    simdcheck PASSES: 23063 lanes over 70 frames produce identical
    canonical rows batched and alone

That claim is only worth something because the POSITIVE CONTROL fails.
With the fixes disabled:

    f070: 23079 lanes checked so far, 3 violation(s)
    VIOLATION frame 67 state 9: batched 376 rows vs singletons 360 rows
    VIOLATION frame 69 state 9: batched 492 rows vs singletons 488 rows
    VIOLATION frame 70 state 9: batched 648 rows vs singletons 624 rows
    Error: simdcheck FAILED: 3 state(s) whose batched result differs from
           running their lanes separately

All three sit in the straddling region (f66+), and every one has the
batched run producing MORE rows than the singletons - the whole-value
collapse adding spurious rows to lanes that had answers, which is the
mechanism predicted before the run. The two runs sample almost identically
(23,079 lanes against 23,063), so the only difference between PASS and
FAIL is the fixes. The gate discriminates and the pass is not vacuous.

Getting the gate to run at all required a fix: it had refused any run with
the frontier subtract on, but without the subtract room (0,0) exhausts
100 GB before frame 66, so it could only ever certify frames incapable of
violating the property. The subtract is now disabled per PROBE, which is
what must be comparable, rather than per command.

## Measurement caveat

The f94 run shared the machine with a background sweep and with simdcheck,
getting ~9.4 cores of the 16 it asked for; the baseline's conditions are
not recorded. The time ratios are therefore soft and most likely
understate the gain. Lane counts, peak RSS, deopt counts and win counts
are contention-independent.

# Room (0,0) SWEEPS. The thing that could not run, runs (2026-08-09)

**SUPERSEDED by the end-to-end section at the top of this file (2026-08-10),
which re-derived all of it on the fixed interpreter.** Kept for the
allocator finding and for the before/after comparison.

**Read the universe caveat first.** Every room (0,0) number below is
against the forward pass as it stood at `census` 1d24aba, i.e. BEFORE the
interpreter fixes that partition mixed interval comparisons and split
`select` on an `UnknownBool` (861c4c7, b8187b6 and follow-ups). Those make
the abstraction strictly more precise, so room (0,0) now produces fewer
lanes - the gap is zero through f65, -1.25% at f70, -13% at f79 - and
`~/celeste-checkpoints/room00` no longer matches what the current binary
produces. What is demonstrated here is that the sweep RUNS at this scale
and what it costs; the `g` it produced is not a certification of anything
and should not be reused. The abstract first-win frame is 80 either way.

The sweep OOMed on room (0,0) with the edge graph - one frame there
produces 644,653,017 successor lanes, each an edge - while the forward
pass completed fine. That was the whole point of the rewrite, and it is
done:

| room (0,0), H=94, 460,658,200 rows | |
|---|---|
| rows that reach the exit within the horizon | 24,514,668 |
| row-expansions | 451,884,260 |
| successors outside the row table | **0** |
| `min(e + g)` | **80**, = the forward pass's first room-exit frame |
| position graph | 405,332 pairs / 7,923 cells / 2.7 MB, 6,171.5 s |
| re-index | 181.2 s (310.5 s on a second, contended run) |
| backward loop | ~2,600 s |

`min(e+g) = 80` against the forward pass's "first room-exit lanes appeared
at frame 80" is the ladder's own consistency check, and it is the only
check available here: room (0,0) has no certified `g`, because the sweep
that would have produced one is the one that OOMs.

**Zero out-of-table successors is the surprise.** Room (0,0) is 89.0%
mixed UnknownBool collapses (64,272,681 of 72,192,126 constructions), so
regrouping its candidate lanes was expected to reach rows the forward pass
never did. Over 451.9M expansions it reached none. That does not make the
room batch-invariant - the collapse can still turn a definite lane into
two - but whatever it does produce stayed inside the row table.

Two costs are genuinely worse here than on room (1,0), and both come from
the fruit: the position graph took 6,171.5 s to record (against 1,121.9 s)
because f090 alone is 401.8 s of `UnknownBool` whole-state fallbacks, and
the expansions are 0.98x the row count rather than 0.46x.

## The allocator, which cost an OOM

The first attempt died anyway, in the backward loop, at the 100 GB cap -
and not because the sweep needs 100 GB:

| after the re-index, room (0,0) | RSS |
|---|---|
| position graph built in the same process | **94 GB** (OOM-killed later) |
| identical graph loaded from `posgraph.bin` | **41.6 GB** (finished) |

Same index, same data, 52 GB apart. The replay's transient peaks around
76 GB, and freeing it does not return it: glibc keeps it in its arenas,
invisible to us and fully counted by the cgroup.

The remedy that is measured is the second row: build the table with
`rewrite pos-graph`, in its own process, and let the sweep load it - which
banks the 1.7 h on disk anyway. `sweep_time` also calls `malloc_trim`
between its two phases, but how much that recovers is NOT measured, and
should not be quoted: room (1,0)'s transient is too small to show it (its
post-index RSS is 22.9 GB without the call and 24.7 GB with it, the
difference being contention, not the call) and reproducing room (0,0)'s
costs 1.7 h.

# The time-expanded sweep is BUILT, and it gates to the edge (2026-08-09)

`rewrite sweep --time-expanded` reproduces the certified `g` for room
(1,0) at H=100 element-wise over all 212,559,009 rows, with no edge graph
at all. The census's projection held to 0.001%: it predicted 98,279,103
row-expansions, the sweep performed **98,280,066** (0.46x the edge
sweep's 212,559,009).

| room (1,0), H=100, from nothing | edge sweep | time-expanded |
|---|---|---|
| row-expansions | 212,559,009 | 98,280,066 (0.46x) |
| stored predecessor data | 10.07e9 edges, 58 GB on disk | 383,943 cell pairs, 1.0 MB |
| wall, total | ~45 min | **30:53** |
| peak RSS | (plus the 58 GB of shards) | 33.7 GB |

Where the 30:53 goes:

| stage | wall | how often |
|---|---|---|
| position graph replay | 1121.9 s | once per PRECISION LEVEL, extended over new frames |
| re-index | 71.9 s | once per sweep |
| backward loop (expansions + scan + keys) | ~660 s | once per sweep |

So a second horizon at the same level costs ~730 s, not 30 min. Against
the edge sweep that is a straight win on memory (58 GB of shards gone)
and roughly a wash on time at H=100; five frames of horizon slack makes
it a wash on expansions too and ten makes it 2.3x worse (see the census
table below). It is a MEMORY result first.

## The re-index was the missing piece

Candidates at frame `i` were discovered at any earlier frame, but the
saved batches are grouped BY discovery frame, so the obvious loop re-reads
batches 1..i every frame - O(H²/2) decompressions, ~1900 s on room (1,0),
more than the expansions cost. One pass over the 2,592 saved states
instead, recording `(state, lane, cell, win)` per row id: **71.9 s**, and
every later fetch is an array index. The states stay resident, which is
most of the 33.7 GB peak - 8.5 GB of decompressed bincode for room (1,0),
20.6 GB for room (0,0).

## What the gate had to be, and what it must not be

A horizon-H sweep never looks past H, so it produces `g` only where
`e + g <= H` - 8,823,156 of 212,559,009 rows at H=100, against the
certified array's 203,369,203. The comparison is therefore against the
certified array THRESHOLDED at H (`tools/gdiff.py --threshold H --meta
meta.json`), element-wise, every row.

Not against the reported optimum. A wrong sweep earlier the same day
printed `abstract optimal win frame from e+g: 90`, matching the certified
run exactly, while `g` was wrong for 95% of rows - the win chain happens
to be stamp-monotone. That is the number the ladder greps.

Three cheaper gates come first and are worth keeping: the certified
banded levels have real wins and real expansions at 1/100th the size.
room1-k16 (327 rows), room1-k8 (92,869) and room1-k1 (8,888,141 rows,
29.5M expansions, 285 s) each reproduce their level's certified `g`
exactly.

## The regrouping, and why room (1,0) is the easy case

Candidates come from every earlier frame, so they are expanded in
different lane groups than the forward pass used - and grouping is
semantic: a comparison against a widened interval that straddles in ANY
lane of a chunk collapses to a whole-value `UnknownBool` and sends the
entire chunk down both edges. Room (1,0) has ZERO mixed collapses and
satisfies batch invariance, which is why its `g` here is identical rather
than merely sound; the sweep reported **0** successor lanes outside the
row table, exactly as that predicts. Room (0,0) is 70.1% mixed.

What holds regardless is the only property the band needs: every grouping
over-approximates the CONCRETE transition relation (a finer group merely
lets a lane take the branch it would concretely have taken), so no row on
a concrete winning path is ever pruned.

# The position table has to be per LANE, not per chunk (2026-08-09)

The table below says a positional predecessor filter is worth building.
The first attempt at RECORDING one - which is the part an implementation
must have, since deriving it from the edges is circular and impossible on
room (0,0) anyway - is correct and useless, and the gap between those two
words is the whole result.

`AbstractRun::record_pos_graph` is a read-only probe: it reads the
positions going into the frame body and coming out, and injects nothing,
so a recorded run takes exactly the path an unrecorded one does. What it
cannot do without tagging lanes is say WHICH input produced which output,
so it records every source cell of a chunk against every destination cell
of that chunk. On room (1,0), 100 frames, that is:

| table | pairs | src cells per dst | candidates at H=100 |
|---|---|---|---|
| exact, from the 10.1e9 edges | 383,528 | 47.1 | 98.3M (0.46x the edge sweep) |
| **recorded per 8,000-lane chunk** | **58,333,241** | **7,162** | **4.150e9 (19.53x)** |
| no filter at all | - | - | 4.433e9 (20.86x) |

The recorded table retains **93.6%** of the unfiltered candidate set. Per
frame it is not a filter at all: 106,587,071 candidates at f080 against a
flat 8px disc's 2,301,506, out of |R(080)| = 111,171,031.

It is genuinely conservative - `pos-graph --check-against-edges` confirms
all 383,360 (dst, src) pairs derivable from the edges are present - so
this is a tightness failure, not a correctness one. An 8,000-lane chunk
is just spatially wide: its lanes span thousands of cells, and the cross
product is the square of that.

Build cost, for the record: 2,056.8 s for 100 frames, ~6 GB peak after
the replay was made to expand in 250k-lane groups (it was 28 GB at f065
in one go, the same transient that OOMs the edge sweep).

## What has to change

Per-lane attribution, which means an origin column after all - but one
carrying the source CELL (~8,000 distinct values on room (1,0)) rather
than the source ROW (213M). That is the sweep's existing replay cost,
paid ONCE per room to produce a table of ~1.5 MB, instead of 58 GB of
edges rebuilt per horizon.

It must be done in the replay, not in the forward pass: a per-lane column
is per-lane distinct, so it forbids the boundary dedup that decides how
coarse the over-approximation is, and a tagged forward pass would be a
different search from the certified one. The replay already runs with
`disable_frontier`, so it has no boundary dedup to lose - which is
exactly why the cost lands there and not on the forward pass.

`--check-against-edges` is the gate for it on room (1,0), and the census's
`cand recorded` column is the number that says whether it is tight enough.
The target to beat is the exact table's 98.3M; anything near 4.15e9 is
the same failure again.

# The time-expanded sweep: measured, and it is worth doing (2026-08-09)

Can the backward sweep drop its edge graph - 10,072,724,145 edges and
58 GB for room (1,0), and an outright OOM on room (0,0) - and instead
re-derive the successors each frame, filtered by PLAYER POSITION?
`rewrite sweep-census` answers that from the certified room (1,0)
universe (212,559,009 rows, concrete optimum 100) without running a
sweep. The unit throughout is ROW-EXPANSIONS: one row carried one frame
forward, which is what both designs actually pay for.

| over frames 1..H | H=100 | H=105 | H=110 |
|---|---|---|---|
| edge sweep today (each row once) | 212.6M | 212.6M | 212.6M |
| time-expanded, no filter | 4.433e9 (20.9x) | 4.433e9 | 4.433e9 |
| time-expanded, perfect predecessor oracle | 8.8M (0.04x) | 26.8M (0.13x) | 56.0M (0.26x) |
| time-expanded, flat 8px disc | 120.9M (0.57x) | 304.8M (1.43x) | 554.8M (2.61x) |
| time-expanded, per-cell learned radius | 1.652e9 (7.8x) | 1.787e9 (8.4x) | 1.945e9 (9.2x) |
| **time-expanded, exact per-cell source sets** | **98.3M (0.46x)** | **259.0M (1.22x)** | **486.8M (2.29x)** |

H=100 is the certified optimum, so its band is as thin as a band can
ever be - every row in it lies on an optimal path. H=105 and H=110 use
the same `g` with a slacker horizon to show the degradation; they are
what a horizon the ladder has not yet refuted would look like.

The naive time-expanded cost is 20.9x the edge sweep, not the 39x a
back-of-envelope from `mean g` suggests: B is monotone
(`B(i) ⊇ B(i+1) ∩ R(i)`), so a row that has already qualified is never
re-tested, and only `R(i) \ B(i+1)` is.

## The filter has to be a set, not a radius

The old solver (`celeste-rust-old`, `DistanceTracker`) learned one max
squared distance per destination cell and painted a disc. That is 7.8x
WORSE than the edge sweep here, and the reason is a single cell.

Of the 10.07e9 edges, 10,072,669,008 move at most 8 pixels. The other
55,137 all land in ONE cell - (136, 124) measured from the start room's
origin, which is the next room's spawn point, where a winning row
appears. Its true predecessors are the far edge of the PREVIOUS room, so
the disc that holds them has radius 134px and covers everything. And
that cell is the seed of `B(H)`, so it is in `B(i+1)` at every late
frame: the learned-radius candidate count is 119,070,337 at f090 where a
flat 8px reads 1,247,069.

Storing the exact SET of source cells per destination cell instead - a
grid bitmap, 8 KB per occupied cell, 8,143 cells and 66 MB on room (1,0),
47.1 source cells per destination on average - removes the problem
entirely (1,052,051 at f090) and is tighter than any radius everywhere
else too. 66 MB against 58 GB of edges.

The 8px disc is listed above for comparison only: it is NOT sound, by
exactly those 55,137 edges.

## What this does and does not buy

At the horizon the ladder actually lands on it is 0.46x the expansions,
and the 58 GB edge store, the 303 s CSR shard build, the 537 s
`fwd.merge` and the reverse BFS all disappear. Five frames of slack makes
it a wash and ten makes it 2.3x worse - so this is a MEMORY result, not
primarily a speed one. What it fixes is room (0,0), where the forward
pass completes (94 frames, 85 min, 60 GB) and only the sweep dies.

Two things the census does not measure, both of which have to be settled
before the sweep is rewritten:

* Where the source sets come from. Learning them from the edge chunks is
  fine for a measurement but circular for an implementation - they have
  to be recorded by the FORWARD pass, which sees every transition exactly
  once (frontier-only, and the successor relation is a static graph over
  rows) and can maintain the 66 MB table for free.
* How a candidate row becomes a `State`. Candidates at frame `i` were
  discovered at any earlier frame, but the saved batches are grouped BY
  discovery frame, so the obvious loop re-loads batches 1..i every frame
  - O(H²/2) decompressions, ~1900 s on room (1,0), which is more than the
  expansions cost.

Also note the gate has to change shape: a time-expanded sweep at horizon
H produces `g` only where `e + g <= H`, so it cannot be compared
element-wise against the full certified `g`. The equivalent full-array
gate is against the certified array THRESHOLDED at H.

# Room (1,0) re-certified on the parallel build (2026-08-08)

The end-to-end gate for everything below, and the one that matters: a
FULLY FRESH ladder at horizon 100 - level-0 forward, a from-scratch
backward sweep, then k=1..16 banded - reproducing the certified answer.

**CONCRETE OPTIMUM = 100 frames.** All 17 per-level answers identical to
the previously certified run (90, 94, 97, 98, 98, 99, then 100 from k=6
on), and the level-0 sweep matched to the edge: **10,072,724,145 edges, 2
win seeds** on both.

That is a strong check on the whole day's work at once - chunk-parallel
frames, the order-independent row key, the parallel visited probe, the
parallel sweep replay. A single mis-ordered row id or a hash collision
anywhere in 10 billion edges would move one of those seventeen numbers.

| stage | wall |
|---|---|
| level-0 forward, f1-f100 (16 GB peak) | ~22 min |
| backward sweep, 100 frames from scratch, 10.07e9 edges | ~45 min |
| k=1..16 banded levels + their sweeps | ~9 min |
| **total, nothing reused** | **~76 min** |

For comparison the previous certification took 1h45 - but that one reused
banked CSR shards for the sweep, which the cost analysis below estimates
would otherwise have added hours. The sweep here was built from nothing.

Caveat, so the table is not read as more than it is: the sweep's SPEEDUP
is unmeasured. It is parallel now and gated byte-identical
(the since-deleted `sweepcheck.sh`), but there is no controlled serial counterpart at this
depth - the f40 gate universe finishes in 5 s either way. See the pending
task before quoting a number for it.

# The parallel campaign, end state: 5.2x at 60 frames (2026-08-08)

Room (1,0), 60 frames, frontier-only + deopt. Lane counts identical to the
serial baseline at every step (1,997,387 at f60).

| | time | peak |
|---|---|---|
| serial, no chunking (the morning's baseline) | 89.2 s | 4.31 GB |
| chunk-parallel frames (16 threads, cap 8k) | 21.9 s | 2.55 GB |
| + parallel visited probe, local candidate dedup, parallel pre-merge gc | 18.6 s | 2.43 GB |
| + survivor gather moved off the serial path | **17.2 s** | **2.43 GB** |

**5.2x, on 40% less memory.** Phase split at the end: interpret 12.4 s
(parallel), boundary decide 1.9 s (serial), merge 2.1 s, gather 0.5 s
(parallel). The serial remainder is 24%.

## The shape of what is left

The parallel region is 12.4 s x 16 threads = ~200 CPU-seconds, and it is
spread thin - the flat profile's top entries are 12% (row keys), 7%
(select), 10% (tile_flag_at across four symbols), 5% (map2). Same finding
as the first profile: there is no single fix worth a multiple, so the
remaining levers are (a) less work per lane, which is what the queued
integer-representation idea is, and (b) the 24% serial tail.

Two structural facts that bound everything:

* **98.1% of the lanes a frame computes are duplicates.** f55 offers
  82,078,938 lanes to the boundary and keeps 1,563,989. That ratio is
  independent of chunk size (82,078,458 at cap 1M vs 82,078,938 at cap 8k,
  0.0006% apart), so it is not lost mid-frame merging - it is the button
  fan-out reaching states the search has already seen. Nothing short of
  knowing the answer in advance avoids computing them.
* **Parallel efficiency is ~8x on 16 threads** for the frame body
  (`fwd.interpret` at cap 8k, f55: 32.9 s at t=2, 19.0 at t=4, 11.1 at
  t=8, 7.5 at t=16 - still gaining 1.48x on the last doubling, so not
  saturated, but tailing).

## Measured and not landed

Recorded because each one closes a plausible-looking direction:

| idea | result |
|---|---|
| mimalloc in the `rewrite` driver (main.rs has it, bin/rewrite.rs never did) | **+22% on the parallel path.** glibc's per-thread arenas suit a short-lived lane vector per instruction on 16 threads better than mimalloc's segments |
| sharded row table, parallel batch insert (ids still serial and bit-identical) | cuts the serial fold 1.91 -> 1.27 s, **1.8% worse overall.** A frame does ~100M probes to ~1.6M inserts; the shard indirection taxes the 100M to parallelise the 1.6M |
| probe the cache-resident local dedup set before the global table | **4% worse.** It makes the local set hold every distinct row in the chunk instead of only the candidates (~2% of them) |
| batches larger than the thread count (barrier / load balance) | 24 is 1% better and 15% more memory; 32 and 48 are worse. Imbalance is not where the efficiency goes |
| bitmap the collision tables (112 KB -> 14 KB), hoist the (w,h) choice | exactly neutral (11.26 vs 11.27 s at f55) - they were already cache-resident. Kept anyway: smaller, and it replaced a placeholder test |
| hash-only `shape_hash_of_state`, parallel shape derivation | 17.27 -> 17.22 s. Kept for the same reason |
| dropping the global probe entirely (to price it) | saves 0.6 s of the 12.3 s parallel region, adds 5.7 s to the serial one |
| `CELESTE_DEOPT_COLLECT_FIRST` off (does the origin column block mid-frame dedup?) | 1.3%. The "origin forbids mid-frame merging" note was about the sweep's replays, not the forward pass |

Transparent huge pages are already `always` on this machine, so the
visited table's TLB behaviour is not on the table either.

# Chunk-parallel frames: 4.5x and less memory (2026-08-08)

Room (1,0), 60 frames, frontier-only + deopt, same lane counts throughout
(1,997,387 at f60 - the check that nothing semantic moved):

| | time | peak | note |
|---|---|---|---|
| serial, no chunking (the baseline all day) | 89.2 s | 4.31 GB | |
| 16 threads, no chunking | 57.0 s | 11.83 GB | 1.6x, and 2.7x the memory |
| 1 thread, chunk cap 8k | ~110 s | 1.2 GB | the tiling tax, ~23% |
| **16 threads, chunk cap 8k** | **19.8 s** | **2.48 GB** | **4.5x, and 42% less memory** |

The two knobs are one setting. Threads alone undo streaming - 16 chunks'
raw outputs in flight is exactly the accumulation the streaming boundary
was built to avoid. Chunking alone is a 23% loss, because each chunk
re-runs the work that is uniform across lanes. Together the chunks give the
threads work and the threads pay for the chunking, so the cap now derives
from the thread count instead of being a knob to remember.

Phase split at f60 (16 threads): interpret 12.1 s (parallel), boundary
4.5 s (serial), merge 2.8 s. Serial remainder is 37%, so Amdahl caps
further thread scaling at ~1.6x more; SMT is worthless (t=24 and t=32 are
both slightly slower than t=16 - this is memory-bound).

Chunk size is a real optimum, not a monotone knob: at 16 threads, f55 goes
27.1 s at cap 1000, 14.5 s at 4000, **13.2 s at 8000**, 15.2 s at 16000,
18.3 s at 32000. Too small and the per-chunk uniform work dominates; too
large and the batch barrier and the cache do.

## What made it scale

1. **The visited-set probe split in two.** Phase 1 (hash each lane to its
   128-bit row key and probe the table READ-ONLY) needs only `&RowTable`,
   so it runs in the worker. Phase 2 (insert, assigning ids) stays serial.
   At depth ~98% of offered lanes are rows already seen - f60 offers 100.5M
   and keeps 2.0M - so nearly all of the one-cache-miss-per-lane traffic
   parallelises. Boundary went 25.0 s -> 5.9 s on its own.
2. **The two parallelism levels had to stop multiplying.** A frame worker
   calling `hash_rows` would `thread::scope` another 16 OS threads per
   boundary state - thousands of spawns per frame. `set_nested_parallel`
   makes the inner level yield to the outer.
3. **Uniform columns leave the per-lane row hash.** Census: a boundary
   state has 50.8 columns that hold one value in every lane and 7.2 that
   vary, and the old sequential fold folded all 58 into every row. See the
   commit - the fix required making the combine order-independent, which is
   also a latent correctness point (uniformity is a property of a state,
   not of a column).

## Determinism, and what "identical" means here

Two separate axes, and only one of them moves anything:

* **Threads: byte-neutral.** At a fixed chunk cap, 16 threads produce
  byte-identical `states.bin` and `visited.bin` to 1 thread. Work goes out
  in batches of `threads` consecutive chunks and comes back in INPUT order,
  so the sequence of rows offered to the visited set is the serial one and
  row ids are unchanged. `parcheck.sh` is the gate.
* **Chunk cap: reorders ids, not rows.** A different cap changes which
  fragments merge mid-frame, so rows reach the boundary in a different
  order and get different ids. The row SET is unchanged - per-frame "new
  lanes" and "visited total" are equal at every frame, and only the
  pre-dedup `sub_before` count moves (23,873,562 vs 23,873,058 at one
  frame of a 45-frame run).

## Correction to the roofline accounting above

The 31x figure below rests on 1.95e11 lane-instructions for a 100-frame
room-(1,0) run, derived from 212.5M lane-frames. That count is the number
of lanes that SURVIVED to the visited set. It is not the work done: at f60
a frame turns 2.0M input lanes into 100.5M boundary lanes (the ~50x button
fan-out), and the frame body computes on those. So the real
lane-instruction count is far higher, the achieved rate far better, and the
"42 cycles per lane-instruction" figure is wrong by whatever the mid-frame
lane multiplier is. Bound [B] should be recomputed from measured bytes (the
`CELESTE_CENSUS` totals) rather than from an instruction count, and until
that is done the honest statement is the measured one: 4.5x today, with 37%
of the run now serial.

# Roofline: how far off are we? (measured 2026-08-08)

Philippe's yardstick: "the time to load all the compressed states for a
frame at peak theoretical memory bandwidth, plus the time to write the
compressed states at peak bandwidth - that's sort of the benchmark. Not
necessarily achievable, but you should be able to get close."

Machine: Ryzen 9 7950X3D, 16C/32T, 128 MiB L3. MEASURED bandwidth (not
spec sheet): 51.5 GB/s pure read, 36.0 GB/s STREAM triad, 29.0 GB/s
memcpy counting read+write.

Subject: room (1,0) level-0 forward pass, 100 frames, the FAST room -
212,559,007 lane-frames, 670 MB of compressed states (3.15 bytes/lane;
the compression is excellent), 1658 s wall.

| bound | time | we are |
|---|---|---|
| [A] compressed-state I/O only (Philippe's roofline) | 0.046 s | 36,000x off |
| [B] every intermediate materialised to RAM, bandwidth-bound | 53.8 s | 31x off |
| [C] compute: 1.95e11 lane-instructions, AVX2, 16 cores | 0.15 s | 11,000x off |
| actual | 1658 s | |

Per lane-instruction we spend 8.5 ns - about 42 cycles at 5 GHz - where
a vectorised op over a long lane vector should amortise to well under
one. Achieved rate is 1.18e8 lane-instructions/s.

Bound [B] is the one to internalise: even a *perfectly memory-bound*
implementation of the dataflow we already have would finish in 54 s
instead of 1658 s. The 31x is not "a few times slower than necessary".

Where the time goes (fwd metrics, same run):
  fwd.interpret        1190 s   72%
  fwd.boundary_stream   353 s   21%
  fwd.save_frames        45 s    3%
  fwd.merge              10 s    0.6%

Three gaps, in order of ratio-per-effort:

1. NO THREAD PARALLELISM IN INTERPRET. 72% of the time runs on one core
   of sixteen; only virtual_merge is threaded (std::thread::scope).
   Frames decompose into ~61 independent fragments, which is
   embarrassingly parallel. A state-parallel flow was tried and reverted
   (task #63, "only helped the plain path") - worth re-measuring now
   that the recipe path dominates and the fragment count is known.

2. ~42 CYCLES PER LANE-INSTRUCTION. Enum dispatch per instruction, the
   MaybeVector Scalar/Vector branch per operand, heap indirection, Arc
   refcount traffic, and an allocation per output vector.

3. NO TILING. Each instruction materialises its whole output vector
   (~111k lanes at f100 = ~444 KB, far past L2) before the next
   instruction reads it back: 1.56 TB of intermediate traffic for this
   run. Tiling a cache-sized block of lanes through the whole program,
   rather than the whole lane set through one instruction at a time, is
   exactly the difference between bound [B] and bound [A].

## Where the cycles go (perf, room (1,0) 60 frames, search-dominated)

Profile a 40-frame run and half of it is recipe build (liveness::analyze,
validate_function, slots::constraints - all startup). At 60 frames the
search dominates and the picture is:

| subsystem | % |
|---|---|
| tile_flag_at / collision lookup | 13.9 |
| subtract_visited (frontier) | 13.0 |
| MaybeVector map/map2 (arithmetic) | 11.4 |
| instruction dispatch | 9.9 |
| interpret_select::pick | 9.1 |
| abstraction (rem widening) | 5.5 |
| allocator | 3.5 |
| merge / hashing | 2.8 |

No single dominant hotspot - the top five are ~57% spread over five
subsystems. That is itself the finding: there is no one fix worth 10x,
so the primary lever has to be parallelism (which multiplies everything)
with tiling to make it work, and the per-subsystem items are secondary.

Landed from this profile: tile_flag_at converted x and y to i16 through
`as_i16_elements` before looking anything up - two intermediate Vec
allocations, two uniformity scans, two extra round-trips through memory
before map2 started. Fused into one pass: -2.8% (96.9 -> 94.2 s at 60
frames), byte-identical artifacts.

## State-parallel flow, retested at depth: still not it (2026-08-08)

The 2026-08-05 revert of state-parallel flow noted "+4-6% on the
rewritten path" and left the door open "if the tradeoff ever flips".
Hypothesis worth testing: that was measured at frame 42, where states
are small (~2900 lanes), so thread overhead would dominate; at frame 60+
states are ~115k lanes and it should win.

Retested by reverting the revert (it was already deterministic -
per-state outputs concatenate in input order) and measuring at 60
frames: 95.9 s off vs 94.7 s on. 1.3%. The tradeoff did not flip.

The useful conclusion is WHY. Parallelising across the states in a flow
step only helps when a step has many states. After each boundary merge a
frame starts as ~1 state and splits progressively, so most of the work
happens in WIDE SINGLE STATES that this parallelism never touches.

=> The right parallel axis is ACROSS LANES WITHIN A STATE, not across
states. That is also exactly the axis tiling wants: chunk the lane vector
into cache-sized blocks, run a block through the whole program, then the
next. Parallelism and tiling become the same refactor rather than two
that fight each other (which is what the 2026-08-05 "working sets evict
each other" note was really reporting).

Scratch branch discarded; nothing landed from this experiment except the
knowledge.

## Queued: spatial locality (Philippe, 2026-08-08)

"There's a ton of locality in the game state's spatial coordinates - if
you're around the bottom left corner and you run one frame, you're still
going to end up around the bottom left corner."

Two distinct uses, and the second is the one that matters:

1. tile_flag_at reads tiles near (x,y), so position-sorted lanes would
   make its lookups sequential rather than a random gather. But Philippe
   notes this one may not need locality at all: a room is 16x16 tiles, so
   solidity is 256 bits = 32 bytes, and a custom per-room lookup could be
   compressed small enough to sit in L1 permanently. Compression beats
   locality here.

2. DEDUP is where locality matters. subtract_visited is 13% of the
   profile and probes a hash table of 128-bit keys - random access into a
   large table, one cache miss per lane. Spatially adjacent states are
   also the ones most likely to be duplicates, so ordering or bucketing
   the visited set by position (rather than by hash) would turn those
   misses into local probes AND cluster the duplicates it is looking for.

## Queued: integer-typed values end to end (Philippe, 2026-08-08)

"Some numbers are pretty much exclusively ints so we could store them
that way end to end."

Pico8Num is 16.16 fixed point in an i32, but x, y, tile coordinates,
timers, sprite ids and flags only ever hold integers. A separate integer
representation would halve their memory traffic (the thing bound [B] is
made of), delete conversions like the one fused above, and make their
comparisons and arithmetic cheaper. It also composes with tiling: narrow
columns mean more lanes per cache line.

Not free: a second numeric variant touches every arithmetic path, and the
lane-structural handlers must resize it (exactly the desync hazard that
shaped the MaybeBool design - see plans/tristate-plan.md). Worth scoping
against the parallelism work rather than doing on impulse.

Caveat on the 36,000x: bound [A] assumes compute is free, so it is a
yardstick rather than a target. Bound [B] (31x) is the honest measure of
how much is being left on the table by the current execution strategy,
and gaps 1+3 are the levers on it.

# Benchmark Data

## Fresh-run cost: room (0,0) vs room (1,0) (2026-08-08)

A clean fresh 100 m campaign with today's code costs ~13 +/- 2 h vs the
measured 3h01m for 200 m (~4.5x): level-0 forward to first win ~1 h
(vs 14 min), first full backward sweep ~3 h (vs ~31 min), horizons
81-94 ~8-10 h (vs ~2 h), final ladder ~30 min. The 4.5x factors:
(a) ~3-4x slower program per lane (shape-agnostic prefix recipe, fruit
states on the plain path) applied to BOTH passes; (b) ~2.5x more state
(387M vs 151.6M visited rows; 21M vs 5.2M peak frontier); amplified by
the origin-tagged sweep replay being ~4x the forward cost on the same
frame (the origin column forbids mid-frame merging - f90: 2747 s replay
vs 740 s forward, 1.3B successors materialized). Banked offsets:
incremental CSR shards (would otherwise add ~4-6 h), streaming
boundary (without it the run OOMs), header-only chunk counts.
Recovery plan (deliberately deferred): S2/S3 variant recipes (both
passes ~3x), tri-state comparisons (kills the fruit plain-fallback and
coarse-level inflation, allows larger replay chunks), parallel replay
(single-threaded today). Target: fresh 100 m in ~2-4 h.


Always run these under `./safe-run.sh` (systemd scope with `MemoryMax=100G`).
It is easy to OOM the machine otherwise.

The standard iteration benchmark is **frame 34**, which takes a few seconds.
Frame 37 (~13 s) is the confirmation run; frame 40 takes over a minute and is
for milestones only.

```bash
./safe-run.sh -- ./target/release/rewrite bench --frames 34
./safe-run.sh -- ./target/release/rewrite bench --frames 34 --profile
```

`rewrite bench` runs the program the recipe produces. `celeste-rust -n N` runs
the *unrewritten* program through the full search harness, so its numbers are
not comparable.

## Current (2026-08, `rewrite` branch)

| program | frame 34 | frame 37 |
|---|---|---|
| as compiled | 6.64 s / 1.75 GB | - |
| + `promote_cell` (130 cells) | 5.47 s / 1.12 GB | - |
| + slot plumbing (identity, no-op) | 5.69 s / 1.12 GB | - |
| + `allocate_slots` | 4.65 s / 1.05 GB | 13.20 s / 3.63 GB |
| + 183 `inline`s | 4.47 s / 1.12 GB | 12.87 s / 3.75 GB |
| + 81 `if_convert`s | 4.40 s / 1.06 GB | - |
| + stage B finished (`promote_capture`, 84 method inlines) | 4.36 s / 1.06 GB | - |
| + `cse` (block-local, then cross-block for accessors) | 4.32 s / 1.06 GB | - |
| + `demote_create` x46, `pin_builtin` x18 | 4.33 s / 1.06 GB | - |
| + 4 store-blocked triangles converted (`speculate`, `sink_store`, `if_convert`) | 3.36 s / 0.94 GB | 9.22 s / 3.11 GB |
| + 4 `convert_ternary` (the `appr` pairs) | 2.62 s / 0.81 GB | 7.87 s / 2.83 GB |
| + `decompose_truthy` x15, `if_convert` x2, `convert_ternary` x1 | 2.36 s / 0.74 GB | - |
| + 2 diamonds absorbed (`absorb_stores`, `speculate` arms, `cse`) | 1.99 s / 0.64 GB | - |
| + `is_solid` chains eager (`speculate_region` x9), consumers absorbed | 1.82 s / 0.62 GB | - |
| + wall-jump stores absorbed (`speculate` with a pointer guard) | 1.78 s / 0.61 GB | - |
| + pixel loops masked (`mask_loop` x2, a61 chains eager) | 1.59 s / 0.63 GB | - |
| + wall-jump arm eager (`fold_reflexive` dead gates, masked `speculate_region`) | 1.46 s / 0.53 GB | - |
| + `spikes_at` nest masked (`fuse_breaks`, `mask_loop` `span`/`break_to`) | 1.40 s / 0.46 GB | 5.52 s / 1.51 GB |
| + 9 `pin_builtin`s, `cse` forward mode (block-local store forwarding) | 1.35 s / 0.50 GB | 5.56 s / 1.47 GB |
| + interpreter micro-opts round 1 + `Arc`'d lane payloads (see below) | 1.03-1.04 s / 0.47 GB | 4.11-4.16 s / 1.65 GB |
| + 16 `collapse_loop` (the `#objects` check/collide loops) | 1.02-1.03 s / 0.47 GB | 4.04 s / 1.66 GB |
| + 16 `assume_eq` + pointer folds: every object-table check returns nil | 0.92-0.94 s / 0.47 GB | 3.85 s / 1.71 GB |
| + 4 `collapse_break_loop` (the `foreach`/`del` sentinel loops) | 0.93 s / 0.46 GB | 3.77-3.91 s / 1.62 GB |
| + `split_call` + both update arms inlined (no dispatch left) | 0.94-0.96 s / 0.47 GB | 3.88-3.90 s / 1.65 GB |
| + 2 `unroll_loop` (pixel-move loops flat) + `merge_blocks` + `cse` forward | 0.88-0.89 s / 0.46 GB | 3.65-3.75 s / 1.65 GB |
| + `dedup_guards` (501 dominated asserts deleted) | time-neutral | time-neutral |
| + 2 `unroll_loop` on the `spikes_at` nest (inner then outer) | 0.88 s / 0.47 GB | 3.69-3.70 s / 1.65 GB |
| + `kill_dead` + cache-blocked dedup (2026-08-05 morning) | - | ~2.89 s |
| + uniform collapse + virtual merge + ranged filters (2026-08-05 night) | - | 2.38 s / **1.01 GB** |

The frontier with the 2026-08-05 night stack (sequential interpreter):

| | time | peak | lanes |
|---|---|---|---|
| bench `--frames 40` | 9.76 s | 3.70 GB | 948,319 |
| bench `--frames 41` | 16.07 s | 6.05 GB | 1,447,750 |
| bench `--frames 42` | 26.47 s | 9.36 GB | 2,181,716 |
| runner `-n 39` | 33.5 s | 14.6 GiB | 613,865 |
| runner `-n 40` | 50.4 s | 23.8 GiB | 948,319 |

Growth is ~1.65x/frame on the rewritten path, so a 60 s budget reaches
**frame 43** and 120 s reaches **frame 44** - two frames deeper than the
morning's estimate at the same budgets, still single-threaded.

Updated 2026-08-06 with the context-partitioned merge
(CELESTE_PARTITION_CELLS=dash_time,djump - see the partition section and
plans/overnight-2026-08-05.md):

| | time | peak |
|---|---|---|
| bench `--frames 42` | 13.1 s | 4.84 GB |
| bench `--frames 44` | 33.9 s | 10.8 GB |
| bench `--frames 45` | **54.6 s** | 15.3 GB |

A 60 s budget now reaches **frame 45**; 120 s reaches ~frame 46.

Updated again after the booleans joined the key (pm1 =
dash_time,djump,has_dashed,p_dash,p_jump - time-flat, peak -44%):

| | time | peak |
|---|---|---|
| bench `--frames 45` | 47.6 s | 8.5 GB |
| bench `--frames 46` | 70.0 s | 12.1 GB |
| bench `--frames 47` | **104.1 s** | 16.3 GB |

Frame 47 (12.0M lanes) is the deepest frame ever computed, inside a
120 s budget, and the per-frame growth softened to ~1.47-1.49x (memory
pressure feeds back into merge cost). For scale: 24 hours before this
measurement, the frontier was frame 41 at 107 s - six frames deeper at
the same cost, one working day later.

## 2026-08-06 overnight: the whole room

With frontier-only search (CELESTE_FRONTIER_ONLY=1), the deopt
architecture (`bench --deopt`, tasks #78/#80) and the conservative
widenings (timer pins + dash_effect_time clamp), the search now runs
the ENTIRE room (1,0), through 31 kill/respawn frames, to the abstract
win at frame 90 (see plans/overnight-2026-08-06.md for what frame 90
means - it is the rem-widened lower bound, not the concrete optimum
100):

| | time | peak | note |
|---|---|---|---|
| bench `--frames 62 --deopt` (v1 deopt) | 179.9 s | 16.4 GB | first run past f58 |
| bench `--frames 90 --deopt` (v2, clean) | 898 s | 32.5 GB | full room; WIN at f90; 151.6M visited rows |
| + CELESTE_DEOPT_COLLECT_FIRST | **737 s** | 32.7 GB | same win, same frontier, -18% |

Per-frame at depth (v2, clean run): f60 13.6 s, f76 (peak, 5.24M new
lanes) ~35-45 s, f90 29.6 s. Total plain re-runs across all 90 frames:
1.55M lanes (v1 re-ran ~5M lanes per frame by f74 - the lane-granular
deopt is what makes the deep half affordable). A 120 s budget reaches
**frame 63** (collect-first).

For scale: the 2022 hand-written solver completed this room in roughly
5-15 min. The general system is now at wall-clock parity on the
forward pass, while interpreting the original Lua.

Frontier-only + deopt are still opt-in (env var + flag); the standard
f34/f37 iteration benchmarks above are unchanged and remain the
regression gate.

The store-triangle row is the first change that moved the fragment count: 558
-> 335 mean fragments per frame at frame 34, split executions 19573 -> 11978.
The ternary row continues it: 335 -> 232, splits -> 8472; the decompose row:
232 -> 212, splits -> 7802; the diamond row finishes every select-expressible
shape: 212 -> 140, splits -> 5345, and **no triangle, `and`/`or` construct,
or convertible diamond anywhere in the program splits any more**. Lane count
identical throughout (92,713). See "Which branches actually split" below for
why these sites paid when 81 earlier conversions did not.

The `speculate_region` row is the first *loop-stage* result: the collide
table loops behind `player.update_21`'s short-circuit gates run eagerly,
their `if_join_413` gates stop splitting, and the freed per-lane `is_solid`
values flowed to consumers that were absorbed in the same batch (the
`on_ground` diamond, the gravity `appr`, the accel `elseif` chain, the
wall-slide `maxfall` store, the `wall_dir` cascade via `decompose_truthy`).
Fragments 140 -> 108 mean (955 -> 671 max), splits 5345 -> 4280. The
instructive dynamic: converting only the chains moved splits (5345 -> 5345,
relocated); each consumer conversion then killed its share for real.

### K, and why it is the number to watch

`measure_k` reports the static size of a fully inlined, fully unrolled frame
body from sampled concrete runs. It takes 3 seconds and is the only measure
here that tracks distance to a compilable kernel.

|  | original | rewritten |
|---|---|---|
| dynamic instrs/frame, mean | 2122 | 3776 |
| dynamic instrs/frame, max | 6422 | 8927 |
| distinct blocks reached | 450 | 255 |
| K, fully unrolled | 8321 | 9562 |
| K, loops kept as loops | 2963 | 3815 |

By instruction kind, rewritten: heap 38.5%, arith 32.2%, const ~8%,
terminator ~7%, guard ~6%, global ~4%, phi ~3%, call 0.9%. `arith`,
`const` and `select` are the core a compiled kernel emits; the rest has to
reach zero. The region stage *lowered* K (7770 -> 7323 unrolled) despite
running regions eagerly, because absorbing the consumers deleted whole arm
blocks and their duplicated address arithmetic.

The `mask_loop` stage then *raised* dynamic work on purpose - mean per-lane
instructions 1719 -> 3635, since the masked pixel loops now run a constant
9 iterations with eager bodies - and the frame still got 11% faster,
because fragments are what cost, not instructions. Note which K bound
matters now: with per-lane control flow gone from these loops, they can
stay *rolled* in a compiled kernel too, so the loops-kept bound (3943) is
the realistic kernel size, and "fully unrolled" (9755) mostly counts
uniform table-loop iterations that never needed flattening.

K and the frame time move independently, and the store-triangle conversions
are a clean example: K rose by 11 (guards added, arms now run on every lane)
while frame time fell 22%, because the change removes *states*, not per-lane
work. Fragments had been 558 through every earlier rewrite - none of those
removed a branch that ever split - and dropped to 335 the first time four
splitting branches went away. So: K measures distance to the kernel, fragment
count measures how much splitting remains, frame time follows fragments.
Judge a rewrite by the one it claims to move.

### What `cse` cost and bought, in the three variants that were run

Same recipe, same lanes (92,713), same memory (1.06 GB), same fragments (558).

| cse | frame 34 | instructions | live slots in `player.update_21` |
|---|---|---|---|
| block-local only | 4.38 s | 19529 | 23 |
| cross-block, every kind | 4.55 s | 18075 | 29 |
| cross-block, accessors and loads only | 4.32 s | 19073 | 24 |

The middle row is the one worth remembering: **1454 fewer instructions and a 4%
slower frame.** Only ~11% of frame time is running `player.update_21`; merge,
gc, dedup and shape grouping are ~65% and are charged per state for every live
value. Reusing a definition from an earlier block trades instructions for live
range, and for arithmetic that trade is a loss. Restricting cross-block reuse to
heap accessors and loads is smaller *and* faster than either extreme.

`cse` is also barrier-bound rather than scope-bound: `player.update_21` has 306
accessor barriers and 528 load barriers across 3801 instructions, so most of the
redundancy that survives is fenced by the 101 remaining calls and 185 `create`
accessors, not by block boundaries.

The forward-mode stage replayed the same lesson with sharper numbers. Refined
alias kills plus store-to-load forwarding finds 631 folds unrestricted; K
(loops kept) drops 3980 -> 3602, frame 34 improves 1.40 -> 1.33 s - and frame
37 *regresses* ~2.5%, because `player.update_21` goes from 33 to 47 live
slots and `anonymous_61` from 31 to 41, and merge/dedup/filter are charged
for the whole env per state. Restricting every new fold to one block keeps
270 of the folds, all of the frame-34 win that survives contact with depth
(1.35 s), K 3815, and the slot counts exactly at baseline. Fold reach is a
live-range decision, not a correctness one - the wider version only becomes
free once splitting is gone or folding is made slot-aware.

Frame 40, milestone checks only: 84.1 s / 25.8 GB as compiled, 62.5 s / 14.45 GB
after `promote_cell`. That beat the old unverified `mem2reg`'s 15.2 GB, which
was stage 1's target. After the `spikes_at` stage: **23.63 s / 5.28 GB**
(948,319 lanes), same 11 split sites as frame 34, differential identical
through 40 (104.5 s) - the `mask_loop` span guards and the floored modulo hold
in the deep regime, and the kill branch still does not split.

Lane counts are identical across all of these (92,713 at frame 34; 269,059 at
frame 37), which is the first thing to check when a rewrite claims a win -
identical lanes means the same work was done, differently.

### Lane expansion without consumer masking is a regression (2026-08)

The `expand` instruction concretizes an unknown bool by doubling the state's
lanes instead of duplicating the state (`rules::expand_bool` replaces `btn`'s
concretization diamond with it; `rules::decompose_branch` keeps the `and`
chain's short-circuit representable once the bool is a vector). Applied to the
two dash-arm buttons alone - k_up (`in_j2_050_if_join_10`) and k_down
(`in_j2_052_if_join_10`), entries `g088`-`g090` in git history - it is
differentially identical through frame 37 and a clean regression:

| recipe | frame 34 | frame 37 |
|---|---|---|
| without (committed) | 1.37-1.38 s / 0.50 GB | 5.65-5.75 s / 1.6-1.7 GB |
| expand k_up/k_down | 1.55-1.57 s / 0.63 GB | 6.30-6.53 s / 2.1-2.2 GB |

Same lanes, same 505 splits, same 527 fragments at frame 34 - the splits
*relocated* rather than disappearing (k_up's 72 to the short-circuit branch
`in_i1_081_cont`, k_down's 72 to the `v_input` consumers `if_body_187` /
`if_condition_185`). The profile attributes the entire +0.20 s: an
`UnknownBool` branch duplicates the state for free (both edges get it
unfiltered), while the same branch on the expanded vector pays a per-lane
mask filter per edge - `filter_branch` 0.18 s / 328 calls -> 0.33 s / 616
calls - plus 0.08 s of `expand_lanes` itself. Expansion converts free state
duplication into paid lane filtering; it can only pay off where the
downstream branches disappear entirely, i.e. as a package with masking every
consumer of the expanded value. The entries were reverted; the machinery
(instruction, both rules, differential result) is kept and tested.

### The `__assert` diamonds are gone - structural, time-neutral (2026-08-03)

`convert_assert` (entries `g091`-`g115` + a `dce` at `g116`) replaced all 25
inlined `__assert` failure diamonds - `%n = not %cond; br %n ? <print+error
subgraph> : join` - with a straight-line `assert_true %cond`. Every path
through such a subgraph calls `error`, which aborts the run exactly like a
failing `assert_true`, and `not` hard-errors on non-bools, so there is no
truthiness gap; the differential run is identical through 34 and 37.

Measured A/B on the same binary, 34 and 37 frames, 2-3 runs each: **neutral
within noise** (1.36-1.39 s / ~0.50 GB at 34 both ways; 5.71-5.79 s /
1.66-1.72 GB at 37 both ways; lanes and fragments identical). Expected: the
branches were uniform and never taken, so the interpreter never spent time
in them. What the change buys is structural: 1722 -> 1622 blocks and
17287 -> 16787 instructions, and - the point - the assert diamonds no
longer sit as branches between every `btn` call and its concretization, so
region speculation can cross them (`assert_true` is speculatable). This is
step 1 of the dash package in plans/status.md.

### The full dash package: assembled, correct, and a measured regression (2026-08-03)

The complete package from plans/status.md was built and measured (entries
preserved in plans/dash-package.jsonl; not in the recipe). It expands
`btn(k_up)`/`btn(k_down)` into lane fan-out and flattens every consumer:
the concretization diamonds become `expand` + store, the arg cells promote
to SSA (block-local multi-store promotion), the k_down short-circuit arm
runs eagerly under the new `speculate_region` expand opt-in, the 2x2
dash-direction nest (`input~=0` x `v_input~=0`) and the three tail
triangles (dash_target.y, dash_accel.x/y) all become select/masked-store
form. The dash arm `if_body_162` ends up one straight-line block.
Differentially **identical through 34 and 37**, and the 144 target splits
are gone (505 -> 361 splits; fragments 527 -> 383 at 34, 695 -> 515 at 37).

Measured A/B on the same binary, repeated:

| | 34 frames | 37 frames |
|---|---|---|
| baseline (committed recipe) | 1.36-1.37 s, 0.50 GB | 5.74 s, 1.69 GB |
| dash package | 1.61 s, 0.65 GB | 6.64 s, 2.20 GB |

**+17% time, +30% memory, at both depths.** The profile attributes the
entire +0.24 s at 34: `expand_lanes` 0.12 s/144 calls (an *eager physical
copy* of every vector in the state, where the `UnknownBool` split it
replaces shares the state lazily between fragments), `cfg:player.update_21`
self +0.06 s (the serialized short-circuit evaluates both buttons for
every dash-state where the branch evaluated one), gc +0.03 s (the doubled
vectors are real allocations). `filter_branch` is unchanged (0.17 -> 0.18
s) - the flattening did succeed in not paying vector-branch filters, which
was the failure mode of the bare-expand experiment above.

Masking the dash gate itself (`speculate_region` at `in_k1039_cont` with
mask+expand, so every state runs the arm) was measured too: **2.41 s at
34** - skipping states pay the 4x lane doubling for the rest of the frame
with nothing reclaimed until the boundary dedup.

The conclusion refutes the working hypothesis that expansion "pays as a
package with masking every consumer". It does not, at these depths,
because the baseline's split is not where the money is: an `UnknownBool`
branch duplicates the state by reference, the four sub-states run the
frame tail with mostly-scalar uniform values, and the frame-boundary merge
machinery folds them back into vectors anyway. The package converts that
lazy fork/merge into an eager mid-frame vector copy and buys back only
per-state fixed overhead that is not yet the bottleneck. Lane expansion
remains the right endgame shape - one state per frame with input fan-out
as lanes - but it becomes profitable only when enough of the frame is
branch-free that the merge machinery itself disappears, not
cluster-by-cluster against a healthy merge path. The remaining big
splitters (`in_i1_074_cont` 108 vector filter-splits on per-lane
`dash_time>0`, the jump/dash button diamonds at 102/51) are identical in
both programs and out of this package's scope: the `dash_time` diamond's
else-side is the whole normal-movement body, which contains splitting
branches and so cannot be a region today.

### Interpreter micro-optimization round 1 (2026-08-03)

perf (`-g --call-graph dwarf` on `rewrite bench`, replay samples excluded)
found what the span profile could not see inside `dedup_state` and
`filter_branch`. Five changes, each measured on 2-3 runs, differentially
identical through 34 and 37:

1. **Lazy span names.** Every builtin and closure call `format!`ed its
   trace span name eagerly, profiling on or off. `SpanGuard::new_lazy`
   builds the name only when profiling is enabled. ~1.5%.
2. **Column-major dedup hashing.** `dedup_state` hashed row-by-row - a
   strided walk over hundreds of separately-allocated vectors per lane.
   `hash_rows` folds each vector into per-row running hashes in one
   sequential pass per vector. ~5%. (`rows_equal`, the exactness confirm
   on true duplicates, remains dedup's floor - it must touch every vector
   at two indices and cannot be skipped without trusting the hash.)
3. **Gather-based filtering.** `filter_by_mask` re-scanned the full
   N-entry mask for every vector value in the state. The kept-lane index
   list is now computed once and every vector gathers O(kept). ~5%. The
   dead owned-path (`Value::filter_vectors`) went with it.

4. **One length check in `map2` instead of `zip_eq`'s check per
   element.** The arithmetic inner loop (`interpret_binary_op`) pays a
   branch per lane for an invariant the interpreter already guarantees;
   asserting it once re-enables auto-vectorization. ~5%.

5. **`Arc`'d lane payloads.** `MaybeVector::Vector` now holds
   `Arc<Vec<T>>`, so cloning a vector value - every `load` and `store` of
   a vectorized cell, every select on a uniform mask, every argument
   gather - is a refcount bump instead of a per-lane copy. Writers
   (`expand_lanes`) go through `Arc::make_mut` and pay the copy only when
   the payload is shared; `PartialEq` gets an `Arc::ptr_eq` fast path,
   which also short-circuits state-equality checks over shared vectors.
   ~8% at both depths, +0.15 GB at 37 from retained sharing. This was
   analysed and rejected in 2026-08 *as a gc optimization* ("lane copying
   is ~3% of gc") - correctly, but gc was never where vector clones
   lived: the interpreter's load/store/select paths were, and after
   rounds 1-4 they were the largest remaining cost.

| | 34 frames | 37 frames |
|---|---|---|
| before | 1.36-1.37 s, 0.50 GB | 5.74 s, 1.69 GB |
| after 1-3 | 1.17-1.21 s, 0.44 GB | 4.97-4.99 s, 1.63 GB |
| after 1-4 | 1.11-1.14 s, ~0.48 GB | 4.47 s, 1.50 GB |
| after 1-5 | 1.03-1.04 s, 0.47 GB | 4.11-4.16 s, 1.65 GB |

**-24% at 34, -28% at 37**, lanes identical throughout. Frame 40
milestone check: **18.82 s / 5.47 GB** (948,319 lanes) - the last
recorded milestone was 23.63 s / 5.28 GB on an older recipe, so the
memory cost of sharing stays within ~4% at depth.

**Tried and rejected: gc arena reuse.** gc clones every reachable heap
value into a fresh arena per call; a rewrite re-indexed pointer-free
values in the shared append-only arena (no clone) with a dense renumber
table and a compact-when-bloated backstop. Measured: 4.94 s vs 4.97-4.99 s
at 37 (noise) and +0.1 GB - the gc span was already only 0.07 s, and the
fresh arena per gc is what keeps garbage bounded. Reverted. The next
candidates by profile are the arithmetic inner loop (`interpret_binary_op`
+ `map2`, ~11% of the run) and `Value::clone` on loads (~14% cumulative,
would want `Arc`'d vectors - invasive, unexplored).

### `collapse_loop`: the singleton object-table loops (2026-08-03)

Room (1, 0) holds exactly one entity (read off the cart's map data), so
`objects` is a singleton for the whole search - confirmed by a probe over
the real abstract run: `#objects == 1` in every lane of every state
through frame 40 (948,319 lanes; no death reaches the horizon). The new
`collapse_loop` rule states that premise as a loud
`assert_true(bound == init)` in the loop's preheader and collapses the
loop: counter replaced by its initial value (`get_index objects[i]`
becomes `get_index objects[1]`), head deleted, preheader falls into the
body, latch falls out to the exit. The body is untouched, so early exits
keep working.

16 sites - every `for i=1,count(objects)` check/collide loop that
actually executes (6 in `anonymous_61`, 10 in `player.update_21`),
selected by intersecting `suggest collapse-loop` with the new
`measure_k --blocks` coverage listing. A guard on a never-executed site
would pass every screen without testing anything, so cold sites
(`load_room` scans, other object types) were left alone deliberately.

Two shapes refused themselves, exactly as designed: the inlined `foreach`
and `del` loops compile as `for i=1,32767` with an in-body
`#tbl < i` break - their guard compares `32767 == 1` and fired on frame
1 of the differential run. They need a different collapse (first
iteration always breaks), not this one.

    before   34f: 1.03-1.04 s / 0.47 GB    37f: 4.11-4.16 s / 1.65 GB
    after    34f: 1.02-1.03 s / 0.47 GB    37f: 4.04 s / 1.66 GB
    K unrolled 9546 -> 9226, dynamic instrs/frame mean 3765 -> 3651

Small on its own, as expected - the point is what it unlocks: each
collapsed body now reads `objects[1]` at a constant index, which is the
prerequisite for asserting `objects[1] == <the object being updated>`
(`assume_eq`) and folding the `o ~= obj` term that makes every such
check return nil in this room. Differentially identical through 34 and
37; lanes identical.

### `assume_eq` + pointer folds: the checks fold to nil (2026-08-03)

The cash-in. Three pieces, each independently verified:

* **`assume_eq`** (new pointed rule): `%g = b == a; assert_true %g`
  right after `b`'s definition, then every other use of `b` becomes `a`.
  16 sites - in each collapsed body, the loaded `objects[1]` is asserted
  equal to the object being updated (`%2`). The premise is the singleton
  table again, stated per site, checked per lane.
* **`fold_reflexive`, pointer families** (opt-in `"pointers": true`):
  `x ==/~= x` with a dominating pointer witness (a `get_field` receiver,
  an `assert_pointer`...), `nil` against `nil`, witnessed pointer against
  `nil`. The witness is load-bearing: `==` is *not* reflexive in the
  abstract semantics (`UnknownBool == UnknownBool` is `UnknownBool`,
  `NumberInterval == anything` is `false`, even itself). The opt-in is
  load-bearing too - extending the rule in place changed what the g043
  entry folded and broke replay of everything after it, which is exactly
  why the doc said to keep such rules apart.
* **`fold_select`** (new bulk rule): static truthiness by def-chain
  fixpoint. `a and b` compiles to `select a ? b : a`, so once
  `assume_eq` makes one link a constant `false`, the whole chain is
  falsy - but never constant. The rule classifies falsy/truthy through
  the `and`/`or` select shapes, resolves selects on classified
  conditions to the arm they must produce, and folds branches on
  classified conditions to the edge they must take.

The chain in every collapsed check body: `o ~= obj` folds false, the
select cascade goes falsy, the early-exit branch folds, the found-object
phi collapses to nil, `nil ~= nil` folds, and dce sweeps the lot - 829
instructions in the first round, 891 with the second (16755 -> 15896).

    before   34f: 1.02-1.03 s / 0.47 GB    37f: 4.04 s / 1.66 GB
    after    34f: 0.92-0.94 s / 0.47 GB    37f: 3.85 s / 1.71 GB
    K unrolled 9226 -> 5801, K static 3767 -> 2934

K unrolled is down 39% on the day (9546 this morning). Splits are
unchanged (505 across the same 11 sites) - this stage removed
computation, not branches; branch *executions* fell 10892 -> 6518.
Differentially identical through 34 and 37; lanes identical throughout.

### `collapse_break_loop` and the devirtualized dispatch (2026-08-03)

Two more stages of the singleton line, both neutral-to-positive on time
and structural on purpose:

* **`collapse_break_loop`** (4 sites): the `foreach`/`del` shape -
  `for i=1,32767` with an in-body `#tbl < i` break - collapsed with
  three runtime asserts and no static arithmetic: `init <= sentinel` in
  the preheader, `not break_check` where the break branch was, and a
  re-materialized break check after the payload that must pass
  (re-reading `# t`, so `player_spawn.update`'s mid-payload
  destroy-and-re-add counts). The three `foreach(objects, ...)` loops in
  `__frame` and the `del` inside the spawn's `destroy_object`; cold
  copies left alone. 37f 3.85 -> ~3.8 s, memory 1.71 -> 1.62 GB.
* **`split_call` + inline x2**: the object-update dispatch
  `obj.type.update(obj)` - per-state uniform but two-valued over the
  search - split under `type == load(get_global "player")` (a pure,
  uniform pointer compare; the rule is semantically neutral, both arms
  make the original call), then each arm pinned by its own
  `assert_closure` and inlined: `player.update_21` (2390 instructions)
  and `player_spawn.update_24` (318) now live inside `anonymous_61`.
  **No dynamic dispatch remains on the hot path**; remaining calls are
  1.5% of K, all statically-known foreach/draw closures.

A measurement lesson from the second stage: the machine drifted ~2%
between the morning's runs and these, and against a stale baseline the
inlines first read as a +3% regression. An interleaved A/B on the same
binary (3.88-3.91 vs 3.88-3.90 at 37) showed both neutral. Compare
variants interleaved, never against numbers from hours ago.

Frame 40 milestone: **17.53 s / 5.30 GB** (948,319 lanes; previous
milestone 18.82 / 5.47). K unrolled 5801 -> 5784; `anonymous_61` is now
84.5% of K and `player.update_21` is no longer a function the frame
calls. Differentially identical through 34 and 37 at every step.

### `unroll_loop`: the pixel-move loops laid flat (2026-08-03)

Census work first: of the 869 weighted loads per worst-case frame, the
heaviest cells (`self.check`/`.collide`/`.hitbox.*`, `objects`, `count`)
were re-loaded once per pixel-loop iteration - not because anything
invalidated them, but because `cse`'s forward mode is deliberately
block-local (the 33->47-slots lesson) and the classic cross-block mode
treats every store as a fence for every load. The fix was not a smarter
alias analysis; it was removing the back edges.

`mask_loop` had already made the trip counts uniform (`i <= 8` on
constants; the lane-varying `i <= amount` only feeds masked selects), so
the new `unroll_loop` rule lays each loop out straight: N renamed copies
of head + body chain, phi values threaded copy to copy. Nothing is
guarded because nothing is assumed - the trip count is *simulated* from
the constant init/step/bound in the interpreter's own fixed-point
arithmetic, and `verify` re-simulates it independently, then walks the
copies rebuilding the id bijection instruction by instruction. Applied
to the two hot loops only (the cold candidates - `title_screen`'s 30
iterations among them - stay rolled, per the cold-site discipline).

After `merge_blocks`, each unrolled region is one straight-line block,
and the *existing* block-local `cse` forward entry swept 1,682
instructions of re-derived stanzas - exactly the reach the live-slot
lesson said not to buy with cross-block liveness.

Numbers (interleaved A/B, same binary, 3 pairs each):

* frame 34: 0.93 -> 0.88-0.89 s (-4.5%), lanes identical (92,713)
* frame 37: 3.73-3.84 -> 3.65-3.75 s (-2%), lanes identical (269,059)
* K fully unrolled: 5784 -> **3808** (-34%); per-lane per-frame
  executed instructions mean 2416 -> 1695, max 5152 -> 3201
* reached (function, block) pairs: 240 -> 135
* K by kind: heap 37.7% -> 34.4% (2182 -> 1311 absolute), guard now
  18.1% (690 absolute, unchanged - duplicated asserts are the next
  target), arith 23.9%, const+global 14.3%

Program grew 18,620 -> 18,883 instructions (the 18 copies minus the
sweep) while the hot path shrank by a third. `fold` found nothing to do
afterwards because constant arithmetic folding is still deliberately
unimplemented (needs `op.rs`-differential testing); the unrolled counter
chains (`0+1`, `1+1`, ...) are what it would eat.

### `dedup_guards`: the assert tax (2026-08-03)

The unrolled straight lines made the guard duplication visible:
`inline` plants an `assert_closure` per spliced call site, `cse` unified
the values they check but has no key for asserts (they produce nothing),
and `dce` sees an effect it must keep. The new bulk rule deletes any
`assert_true`/`assert_pointer`/`assert_closure` dominated by an
identical assert on the same SSA operands - those asserts are
deterministic functions of immutable values, so the dominated copy can
never be the first to fire. `assert_value_cell` reads a heap cell and is
excluded. The verifier re-checks every deletion against a *surviving*
covering twin.

501 asserts deleted (program 18,883 -> 18,382). Time-neutral by
interleaved A/B at 34 and 37 - most of the deleted guards were per-state
scalar checks, which the interpreter barely feels. The point is K:
unrolled K 3808 -> **3368** (-12%), guard weight 690 -> 250 (-64%).
That is paid work the eventual compiled kernel no longer contains.
`dce` found nothing afterwards (the surviving first asserts keep their
operands alive), so no `dce` entry follows it in the recipe.

### The `spikes_at` nest unrolled (2026-08-03)

The instruction profiler picked the target: 7.2% of program-under-test
time in the inner spikes body (`mget` + four fixed-point `%` + selects,
re-derived per iteration). Stage L had already masked both nest loops to
constant-bound counters (`k <= 1.0`, 2 iterations, `span < 2` asserted
at runtime), so they were `unroll_loop` shapes except for one refusal:
head-defined values (the in-bounds flags) are read *directly* by the
exit blocks, not through phis. That is sound to unroll - a head-defined
value used outside the loop always observes the final head execution,
since every path out passes through it - so the rule now renames such
uses through the last copy instead of refusing (chain-defined outside
uses stay refused; they cannot dominate the exit). Replay of the earlier
unroll entries is byte-identical, so no opt-in flag was needed.

Inner loop first (its unroll linearizes the outer body), then the
outer, then the usual sweep. Interleaved A/B: 0.89-0.90 -> 0.88 s at
34, 3.75 -> 3.69-3.70 s at 37 (~-1.5%, matching the ceiling: the block
was 7.2% of the ~26% program share). K unrolled 3368 -> **3110**, and
the two K bounds nearly converged (3047 loops-kept vs 3110) - **the hot
path is now essentially loop-free**; what remains rolled is cold or in
`__main`'s per-frame input handling.

### The dash package re-measured; its expansion-free remainder landed (2026-08-03)

The dash package (`plans/dash-package.jsonl`) re-derived cleanly against
the fused frame - the suggester found all eight concretization diamonds,
and the stage-C nest survived the inline byte-for-byte modulo
renumbering. The bet was that the 2026-06 revert's stated blocker
(expanded lanes feeding rolled loops and real branches) was gone now
that downstream is straight-line selects. **The bet lost, at the same
relative magnitude as before**: interleaved A/B at 34, 5 pairs,

* time 0.88-0.90 -> 1.04-1.08 s (**+19%**), memory 0.47 -> 0.65 GB
  (**+38%**), lanes identical, fragments/frame 527 -> 383

`bench --profile` now says precisely why. `expand:expand_lanes` is 9.6%
of wall on its own (144 executions - **expanding duplicates every heap
cell of the state**, all ~279 of them, not just the button-dependent
few), `cfg:anonymous_61` self time rises 0.20 -> 0.26 s from
doubled-width vectors downstream, and the state machinery gives back
only ~0.02 s of `filter_branch` - because it still runs, fragments or
no. Partial lane expansion pays the full lane tax while the ~50% merge
machinery keeps its state count. This bounds the endgame: there is no
monotone path of individually-landable expansion stages; btn expansion
has to be judged as one jump (all sites plus all consumers, machinery
actually collapsing), and `expand` itself likely needs to get cheaper
(copy-on-write lanes, or expanding only reachable-from-button cells).

The expansion-free remainder of the package **did** land: the
dash-direction nest (two inner diamonds, the outer diamond, three tail
triangles, nine dash-gate `create` demotes) flattens with plain
speculate/absorb into select-stores, no `expand` involved. Time and
memory neutral at 34 and 37 (interleaved, lanes identical), and K drops
3110 -> **3060** with reached (function, block) pairs 129 -> 115 - the
`dedup_guards` pattern: a free K win, landed for the compiled kernel's
sake. Differentially identical through 37.

## Where the time goes

Refreshed 2026-08-06 after the overnight stack (see
plans/overnight-2026-08-05.md). `rewrite bench --frames 43 --profile`:

| span | self | share |
|---|---|---|
| `cfg:anonymous_61` (the frame body) | ~17 s | ~55% |
| `filter:filter_branch` | ~4-10 s (grows superlinearly with depth) | 15%+ |
| `vectorize:vm_hash` / `vm_verify` / `vm_probe` / `vm_pack` (the virtual merge, all parallel) | 2.2 / 2.2 / 1.9 / 0.9 s | ~23% |
| `vectorize:merge_groups`, `dedup_state`, `union_diff_states`, `gc` | ~0 | ~1% |

The old materialised-merge costs (`merge_groups` 21%, `dedup_state` 19%
in the previous version of this table, measured at frame 34) are gone:
the virtual merge replaced them and the union pass-through removed
`union_diff`. What remains at depth is the frame body itself - whose
instruction time the cardinality census showed to be 100% duplicate-lane
computation - and the two dash_time fork sites behind `filter_branch`.

The previous version of this section, kept for the fragment-count
narrative it documents (frame 34 numbers, pre-virtual-merge):

| span | self | share |
|---|---|---|
| `vectorize:merge_groups` | 0.41 s | 20.9% |
| `vectorize:dedup_state` | 0.37 s | 18.7% |
| `gc:gc` | 0.29 s | 14.5% |
| `filter:filter_branch` | 0.27 s | 13.6% |
| everything under `cfg:` (actual interpretation) | ~0.40 s | ~20% |
| `vectorize:shape_grouping` | 0.12 s | 5.9% |
| `filter:filter_split_flr` | 0.02 s | 1.2% |

And the finding that matters most, from the `merge_site` bracketing spans:

| | total | calls |
|---|---|---|
| `merge_frame_boundary` | **1.02 s (51%)** | 34 |
| `merge_hint_normalize` | 0.25 s (13%) | 68 |
| inside the frame CFG (`cfg:__main`) | 0.94 s (47%) | 34 |

**Merging states back together at the frame boundary is half of runtime**
(down from 60% when this section was first written - the branch-removal
rounds attack exactly this). The semantically necessary interval refinement
(`filter_split_flr`) is ~1%.

This is a change from the earlier reading of this file, which had intra-frame
filtering at 33%. Slot allocation cut `filter_branch` from 33% to 13%, and what
was left standing was the merge.

The cause is intra-frame branching. `rewrite bench` reports it directly:

    fragments before merge: 4751 total, 140 mean, 955 max per frame
    (was: 19593 total, 576 mean, 5220 max before the branch-removal rounds)

A frame produces ~140 separate states which then have to be merged back into
one. Every part of the merge is per state, so **the fragment count is the
number a branch-removing rewrite should be judged by** - not the `filter_branch`
share, which slot allocation already cut from 33% to 13%.

Every frame still ends as **exactly one vectorized state**; all fragmentation is
intra-frame and fully re-merged.

### `kill_dead`: deadness in the IR, -9% at depth (2026-08-05)

The filter-column census said 41.9% of the vector columns a filter has to
gather live in `local_env`, and the interpreter never drops any of them:
`src/liveness.rs` is a no-op stub and `glue.rs` passes `all_live()`, so
the `BlockBeforeJoin` pruning in `flow.rs` has never pruned anything.

New IR instruction `Kill { values }` and bulk rule `kill_dead` place one
kill at the end of each block naming everything dead there. Deadness is
computed once at rewrite time and stated in the program text, where a
verifier can refute it - the verifier walks *forward* from each kill and
fails if any path reads the value before redefining it, deliberately not
sharing the backwards liveness analysis that placed it.

Measured, interleaved, lanes identical (269,059 at 37; 613,865 at 39):

| | base | +kill_dead |
|---|---|---|
| `rewrite bench` 37 frames | 3.07 s | 2.77 s (**-10%**) |
| runner `--rewritten` frame 39 | 3.91 s | 3.56 s (**-9%**) |

Census at frame 39 shows exactly the predicted mechanism, and nothing else:

| | base | +kill_dead |
|---|---|---|
| state filter calls | 4,000 | 2,532 (-37%) |
| filter elements | 901 M | 432 M (-52%) |
| filter time | 1.12 s | 0.59 s (-47%) |
| in_i1_074_cont | 0.68 s | 0.34 s |
| and_or_join_126 | 0.35 s | 0.17 s |

Filter *calls* fall by a third and *elements* by half - the dead locals
were both numerous and wide. Split counts and lane counts are unchanged;
this buys nothing by removing branches, only by making each one carry
less. 197 kills in `anonymous_61`, naming 3,090 locals, 15.7 per kill.
Differentially identical through 37.

Two things worth keeping. The first version of the verifier treated a phi
operand as a use on *every* incoming edge, which rejected every loop head
in the program (`__init: kill of %746 ... is followed by a read of it`) -
a phi consumes its operand on one edge only. And the applier's kill set
needs a third term beyond the obvious two: locals live at the end of a
predecessor but dead on entry here, which die *on the edge*. Without it
one path drops a value while a sibling path carries it, and two states
that should merge differ by something neither can read.

### Uniform vectors collapse to `Scalar` at construction: -8.4% / -8.6% memory (2026-08-05)

The output-cardinality census (`CELESTE_INSTR_CARD=1` with
`rewrite bench --profile`) measured, for every instruction, how many
distinct values its output vector contains. Result at frame 37 of the
rewritten program: 0.54 s of non-call instruction time, and 100% of it -
to three decimals - is lanes duplicating a value that already exists
elsewhere in the same output vector. No hot instruction: the top one is
13 ms; mean output is ~50k lanes with 1-28 distinct values, max distinct
anywhere 51. Several selects produce *constant* vectors (dist/exec = 1.0),
and nothing mid-frame demoted a constant vector to `Scalar` - only the
merge's `unvectorize_if_possible` did, once per frame.

So `MaybeVector::vector` now early-exit-scans fresh lanes and returns
`Scalar` when they are uniform. Every producer funnels through it (map,
map2, select gather, filter gather, concat), so uniform vectors stop
existing at all, downstream ops go per-state instead of per-lane, and the
column drops out of the merge's dedup key. Safe against merge-group
fragmentation because shape normalization is representation-blind.
Non-uniform vectors pay a few comparisons (exit at the first differing
lane).

Interleaved A/B, 3 rounds each, identical lane counts:

|                    | base           | collapsed      | delta |
|--------------------|----------------|----------------|-------|
| runner `-n 39`     | 44.36 s / 16.2 GiB | 40.64 s / 14.9 GiB | -8.4% time, -8.6% mem |
| `bench --frames 37`| 2.89 s         | 2.71 s         | -6.4% |

Differentially identical through 37.

### The virtual-concat merge, fourth attempt: landed (2026-08-05)

Multi-state merge groups now dedup over a *virtual* concatenation
(`src/interpreter/virtual_merge.rs`): hash, dedup and gather run straight
off the per-fragment columns, and the 23.6M-row pre-dedup table (frame 37)
is never built. The two killers of the three parked attempts are handled -
uniform columns leave the dedup key explicitly, and every access is a
sorted piece walk. Output is identical to the materialised pipeline
(same hash seed, so the same survivor mask; a test holds the two states
equal, representation included), which stays as the fallback.

Interleaved A/B, 3 rounds each, identical lane counts:

|                    | materialised   | virtual        | delta |
|--------------------|----------------|----------------|-------|
| runner `-n 39`     | 40.42 s / 14.9 GiB | 39.03 s / 14.6 GiB | -3.4% time |
| `bench --frames 37`| 2.68 s / 1.41 GB peak | 2.57 s / **1.01 GB** peak | -4.1% time, **-28% mem** |

The memory figure is the point: on the rewritten path the transient
concat *was* a third of peak RSS. That gap should widen with depth, since
the pre-dedup table grows ~1.6x per frame.

### The hint_normalize points: measured load-bearing, kept (2026-08-06)

The two shipped mid-frame normalize points (plus the leftover in
_update_62) predate the branch-free rewrite, so removing them from the
rewritten program was tried via a new `remove_hint` recipe rule (the
rule is landed and stays; the entries are parked). Differential verify
is identical across the difference - merging is semantics-preserving -
and fragments grow 23 -> 505 mean per frame. Seven interleaved rounds
at frames 42: **18.39 s with hints vs 19.09 s without (+3.8%), peak
9.6 -> 10.0 GB**. So mid-frame fragment control still pays ~4% even
with the virtual merge making merges cheap. The context-partitioned
merge idea would *replace* these merge points with something stronger
(partition on the fork-condition cells) rather than delete them.

Separately and independently, the unsound per-column state comparison
that used to live behind these hint merges (union_diff's
NormalizedState) is deleted outright - union_diff is now a guarded
pass-through that panics with instructions if a real dedup is ever
needed again.

### Ranged filter gathers: -13.5% (2026-08-05)

A new census column asked how *chunky* filter gathers are: contiguous
runs in the kept-index list. Answer at frame 37: **30.6 lanes per run**
at `filter_branch`, 19.5 at `filter_split_flr` - nothing like the ~2 a
random mask would give. Adjacent lanes share history (concat and
first-occurrence dedup preserve arrival order), so they usually agree on
a branch condition. The same census also showed `filter_dedup` at zero
calls - the virtual merge has eliminated dedup-side filtering entirely.

So `filter_by_mask` now scans the mask once into `[start, end)` ranges
(`KeptLanes`) and each vector gathers by `extend_from_slice` per run -
memcpy, since the payloads are `Copy` - instead of one indexed copy per
lane.

Interleaved A/B, 3 rounds, identical lane counts:

|                    | before | after | delta |
|--------------------|--------|-------|-------|
| runner `-n 39`     | 38.77 s | 33.54 s | **-13.5%** |
| `bench --frames 37`| 2.54 s | 2.38 s | -6.4% |

The largest single win of 2026-08-05, and it stacks with the uniform
collapse (which raises run lengths by removing lane-varying columns).
If lanes were ever sorted by context at the merge, run lengths - and this
win - would grow further; that is the cheap consumer the sorted-lane idea
was missing.

### Hint-block union kept as states, not as a persistent normalized set (2026-08-05)

Tried: replacing the accumulated `Vec<State>` union at hint_normalize
blocks with a persistent `FxHashSet<NormalizedState>`, so fixed-point
rounds stop re-normalizing (clone + gc) the whole union. Parked on the
numbers: runner `-n 39` 38.77 s -> 39.13 s (+0.9%, slower in all three
interleaved pairs), bench 2.56 s -> 2.52 s with peak 1.01 -> 0.93 GB.

The reason there was nothing to win: `merge_hint_normalize` runs exactly
once per hint block per frame (74 spans = 37 frames x 2 blocks), so every
fixed point converges in a single round and the cross-round membership
check never rejects anything. The old code's `accumulated.is_empty()`
fast path was already optimal; the new version only added deep normalized
copies (the union states are Arc-shared, a `NormalizedState` is not) held
until the CFG ends. If fixed points ever start taking multiple rounds -
loops through hint blocks - revisit with a digest-indexed set that stores
Arc-shared states and re-normalizes only on digest hits.

### If-converting the two hot fork sites: not available, and the nearby one is a 2.2x regression (2026-08-05)

The per-site filter census put 99% of `filter_branch` on two branches -
`in_i1_074_cont` (0.68 s at frame 39, on per-lane `dash_time > 0`) and
`and_or_join_126` (0.35 s). Both were tried.

**Neither branch is convertible by any current rule.** `suggest` proposes
nothing for either: both arms contain `btn` branches on `UnknownBool`,
which no select can express. This re-confirms the stage-R note - the
`dash_time` diamond's else-side is the whole normal-movement body, and it
contains splitting branches, so it cannot be a region. Removing that
blocker needs `expand`, which has been measured as a regression twice
(+17%, +19%) with a diagnosed cause (`expand_lanes` physically doubles all
~279 heap cells when only the button-dependent few can differ).

What *is* available nearby: 34 candidates from `suggest` over
`anonymous_61` (if-convert, speculate-region, speculate, absorb-stores,
convert-ternary, decompose-truthy, sink-store), of which 25 verify at 32
frames. As a bundle they cost **3.02 s -> 6.6 s at 37 frames**, and
bisection puts the entire regression on one entry: `if_convert` at join
`in_h061_and_or_join_126`, the and/or triangle feeding the second hot
branch. Alone it is 3.02 -> 6.58 s (**+118%**), 1.39 -> 1.84 GB. The other
24 are neutral. Nothing landed.

The mechanism is worth keeping, because it generalises. Split counts are
**completely unchanged** - 503 splits across the same 11 sites with the
same per-site counts - and lanes through the two hot sites barely move
(9.8 -> 10.8 M and 4.1 -> 4.4 M at frame 37). What changes is the *width*
of every filter:

| frame 37 | baseline | +cif-014 |
|---|---|---|
| state filter calls | 3,972 | 32,912 (8.3x) |
| filter elements | 355 M | 2,029 M (5.7x) |
| filter time | 0.39 s | 1.76 s |
| in_i1_074_cont | 24 events, 0.23 s | 48 events, 0.89 s |
| and_or_join_126 | 12 events, 0.12 s | 24 events, 0.76 s |

A filter call is one vector column, so calls/events is the number of live
vector columns in the state. If-conversion roughly **quadrupled live
columns** at the split points, because eager evaluation keeps both sides'
intermediates live across the branch. So a conversion that does not
actually remove a split is doubly bad: it pays for both arms *and* makes
every downstream filter wider. The converted branch here was one of the
always-uniform ones (`and_or_continue_125`/`if_condition_100` never appear
in the split table), which is the same lesson as "if_convert removed 81
branches and only 4.6% of the splits" - now with a cost mechanism
attached.

Corollary for the ranked plan: **filter cost is driven by live vector
columns, not only by lane counts.** Narrowing what is live at a split is
an independent lever on the same 1.03 s, and unlike if-conversion it does
not require the btn blocker to fall first.

### The merge machinery measured against its task (2026-08-03)

Is the ~50% merge share an implementation problem or the task's real
cost? `bench --profile` now prints the merge data volume (new
`merge_stats` counters, always on, per-call granularity, no measurable
overhead), and a synthetic calibration replicated the dedup algorithm
on workload-shaped data. Answer: **the implementation is within ~20% of
optimal for this algorithm; the surprise is the task size.**

The volume, at 34 frames (0.88 s wall):

    merge data volume: 102 vectorize calls, 1012 states in -> 109 out (109 groups)
      concatenate: 929 states x ~279 heap cells = 0.3M cell clones (0.11s, 410 ns/cell)
      dedup: 109 calls, 5.68M rows x 17.1 row-weighted vector columns = 96.9M
             elements, 5.04M rows removed (89%)
      dedup throughput: 0.29s dedup_state self = 3.0 ns/element (50.4 ns/row)

At 37 frames (3.7 s wall): 23.61M rows, 90% removed, 58.5 ns/row, and
`merge_groups` cloning slows to 1268 ns/cell under 1.7 GB RSS.

Reading:

* **Throughput is near-optimal.** A stripped reimplementation of the
  same algorithm (dense i32 columns, same FxHash fold, same bucket
  structure, 170k rows x 17 columns, 89% dups) runs at 42.8 ns/row vs
  50.4 measured - and the elementwise hash pass is the *minor* term
  (0.4-3 ns/element; the ~40 ns/row hashmap probe/insert dominates
  both). A sort-based formulation ("sort the states, dedup
  consecutive") measures *slower*: 49.4 ns/row for the sort+scan alone,
  on top of the same hashing. Micro-optimization (open addressing, no
  per-bucket `Vec`) might buy 15-20% of the dedup span, i.e. ~0.05 s
  at 34. There is no large constant factor hiding here.
* **The task is ~60-90x the surviving lanes.** 5.68M rows are hashed
  to keep 0.64M distinct (92.7k lanes at the final boundary); at 37 it
  is 23.6M rows for 269k lanes. Every lane fans across the frame's
  unknown branches (6 btn splits send whole states down both edges,
  unfiltered) and gets re-crushed at ~3 merge points per frame
  (boundary + 2 `hint_normalize`). The 89-90% duplicate rate *is the
  search doing its job* - most button choices don't change the
  outcome, and dedup is where that convergence is detected.
* **Consequence**: the merge machinery is the search's pruning
  operator running at ~50 ns per candidate; the lever is not its speed
  but its input volume - fewer rows manufactured per frame (the lane
  expansion endgame) or a representation where duplicate detection is
  cheaper than per-row hashing (hierarchically ordered lanes, where
  most columns are runs). Caching row hashes across a frame's 3 merge
  points is the one modest implementation win available (~a third of
  row cost at the repeat sites).

### Which branches actually split (2026-08)

`rewrite bench --profile` attributes every conditional-branch execution to its
source block and records whether both edges got lanes. At frame 34:

    branches: 21862 of 333379 executions split the state, across 33 distinct sites

**93% of branch executions are free** - the condition is uniform across lanes,
every lane goes the same way and nothing is cloned. All the fragmentation comes
from 33 places, and the top 15 are 94% of it:

| function | block | splits | uniform |
|---|---|---|---|
| `player.update_21` | `in_j2_050_if_join_10` | 2838 | 0 |
| `player.update_21` | `in_j2_052_if_join_10` | 2838 | 0 |
| `player.update_21` | `if_join_133` | 1856 | 2500 |
| `obj.is_solid_47` | `if_join_413` | 1779 | 6247 |
| `player.draw_22` | `__entry` | 1499 | 7211 |
| `player.update_21` | `if_body_101` | 1356 | 504 |
| `obj.move_y_53` | `for_body_start_488` | 1064 | 996 |

This is why `if_convert` was nearly free but nearly useless: it was aimed at the
145 `and`/`or` triangles, whose conditions are almost always uniform.

The top two sites are **the same site twice** - the `if` inside `btn()` in
`lua/builtin_level_4.lua`, inlined at two call sites. They split 2838 times each
and *never* run uniformly, because that branch is the search's own branching
factor: `__button_states[i]` starts as an `UnknownBool`, and `flow.rs` sends a
state that branches on `UnknownBool` down **both** edges unfiltered. Together
they are 26% of all splits, and because they duplicate the state early in the
frame they multiply every split downstream.

Note the mechanism: this is not lane filtering, it is state *duplication*.
`filter_by_mask` is never called. It is also not something a program rewrite can
remove, because the branch is semantically necessary - it is how the search
enumerates inputs.

**Update, after the four store-blocked triangles were converted** (`speculate`
+ `sink_store` + `if_convert` on `player.draw_22 if_join_222` and
`player.update_21 if_join_98/116/119`):

    branches: 11978 of 181247 executions split the state, across 31 distinct sites

The four sites accounted for 3523 splits directly, but total splits fell by
7595 and branch *executions* fell by 152k, because a split early in the frame
multiplies every branch execution downstream - `draw_22`'s split alone cloned
the state for the whole rest of the frame. Even the irreducible `btn` sites
fell from 2838 to 1620 each, since fewer states now reach them. This
downstream multiplier is also why the 81 earlier conversions bought nothing:
they were aimed at branches that never split, so there was nothing to
multiply.

**Update, after `convert_ternary` on the four splitting `appr` pairs:**

    branches: 8472 of 130829 executions split the state, across 27 distinct sites

What still splits is no longer triangle-shaped: the four `btn` sites (3362,
input fan-out), diamonds and chains in `player.update_21` (`if_join_95` 1056,
`if_join_133` 838, `if_join_136` 581, `if_condition_92` 576, `if_join_108`
480), the object loop in `anonymous_61` (~660), and two ternary variants
`convert_ternary`'s strict shape check refused (346).

**Update, after `decompose_truthy` x15 defused every mixed-select cascade and
`if_convert`/`convert_ternary` took the two refused variants plus the `appr`
pair that removing them exposed:**

    branches: 7802 of 121074 executions split the state, across 26 distinct sites

Zero splits remain from triangles or `and`/`or` constructs - the profiler's
triangle table reads 0 across the board for the first time. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 (`in_j2_042/046/050/052`) | 3176 | input fan-out |
| `if_join_95/133/136`, `if_condition_92`, `if_join_108` | ~3400 | diamonds and chains |
| `if_join_413` x3, `for_head`s (object loop bodies) | ~920 | loop + method shapes |

One dynamic worth recording: removing the `sign()` split at `in_i1_077` made
the *downstream* `appr` pair at `in_i1_078` start splitting (144, previously
0 - its condition used to arrive pre-sorted into per-state-uniform lanes).
It was `ready` in the triangle table and one `convert_ternary` entry took it.
Removing splits un-hides downstream splits; re-profile after every batch.

**Update, after the diamond stage** (`absorb_stores` on `if_join_108` and
`if_condition_92`, the two convertible sites of the five read this session):

    branches: 5345 of 86751 executions split the state, across 24 distinct sites

Removing 1056 direct splits took ~2457 total - the two sites sat *upstream*
of most of `player.update_21`, so every splitter below them now sees fewer
fragments: `if_join_95` 1056 -> 840, `if_join_133` 766 -> 444, `if_join_136`
509 -> 326, the `btn` pair 1138 -> 820 each. Frame 34 fell 2.36 s -> 1.99 s
and 0.74 -> 0.64 GB. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 (`in_j2_042/046/050/052`) | 2540 | input fan-out |
| `if_join_95/133/136`, `if_body_135` | 1706 | diamonds blocked by loops / `btn` |
| `if_join_413` x4, `for_head`s (object loop bodies) | ~1100 | loop + method shapes |

The exposure this round: `if_body_135` (96 splits, previously quiet), the
nested grace-check inside the jump arm - same `is_solid`-loop blocker as its
parent `if_join_133`. Every remaining site is gated on a later stage (object
loops, `btn` lane expansion); shape work is done until one lands.

**Update, after the region stage** (`speculate_region` x9 making the
`player.update_21` `is_solid` chains eager, plus the consumer conversions
they exposed - the `on_ground` diamond via the multi-store `absorb_stores`,
the gravity `appr` arm, the accel `elseif` chain, the wall-slide `maxfall`
store, the `wall_dir` cascade via one more region + `decompose_truthy`):

    branches: 4280 of 64692 executions split the state, across 22 distinct sites

Frame 34: 1.99 s -> 1.82 s, 0.64 -> 0.62 GB, fragments 140 -> 108 mean
(955 -> 671 max). The chain conversions alone moved splits without removing
one (the `is_solid` result still fed a branch); each consumer conversion
then deleted its share, and the multiplier paid again downstream:
`if_join_95` 840 -> 648, `if_join_136` 326 -> 249, the `btn` pair 820 ->
669 each, `in_j2_042` 300 -> 195. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 | 1923 | input fan-out |
| `if_join_95` | 648 | the outer dash diamond, convertible last |
| `if_join_133/136`, `if_body_135`, `and_or_join_153` | 873 | the jump/dash cluster (below) |
| `anonymous_61` move loops (`413`, `for_head`s) | 599 | pixel loops, unroll stage |
| `in_i1_068` x2 (`spikes_at` tile loops) | 114 | unroll stage |

The jump cluster is one blocker deep: `if_body_159` (the wall-jump stores)
needs `speculate` to move a re-load of `this.spd` above the `spd.y` store,
and the cells are not provably distinct by the facts it has (the reload's
base is a loaded table, not a shared local). The clean fix is a runtime
distinctness guard (`assert_true` on pointer inequality), which needs `~=`
on pointers in `op.rs` - deferred, not attempted this round. Exposures that
did land: `if_condition_100` (the accel chain, 288 at its peak) was
converted in-batch; `if_body_135` rose 96 -> 156 and `in_k1000_for_head_472`
woke at 36, both gated on their stages.

**Update, after the wall-jump pointer guard** (`speculate` with the new
opt-in `guards` field on `if_join_160`, `absorb_stores` on
`and_or_join_153`, one `merge_blocks`):

    branches: 4058 of 62418 executions split the state, across 21 distinct sites

Frame 34: 1.82 s -> 1.78 s, fragments 108 -> 102 mean (671 -> 614 max).
`~=` on pointers turned out to already exist in `op.rs` (pointers compare
by `HeapId`), so the whole interpreter half of the planned work was free;
the stage was one `speculate` extension. Direct kill: `and_or_join_153`
(102). Downstream: the hot `btn` pair 669 -> 618 each, `if_join_136`
249 -> 231. K 7323 -> 7308 unrolled; dynamic max per lane rose 4986 ->
5190 because the wall-jump loads now run on the longest path too. The
remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x4 | 1821 | input fan-out |
| `in_i1_074_cont` (= old `if_join_95`) | 648 | the outer dash diamond, convertible last |
| `and_or_join_126` (= old `if_join_133`), `if_join_136`, `if_body_135` | 753 | jump/dash cluster, gated on object loops / `btn` |
| `anonymous_61` move loops (`413` x2, `for_head`s) | 626 | pixel loops, unroll stage |
| `in_i1_068` x2 (`spikes_at` tile loops) | 114 | unroll stage |
| 6 sites below the profiler's top-15 cutoff | 96 | tails of the above |

(`merge_blocks` renamed two sites: `if_join_95` and `if_join_133` merged
into their predecessors - same branches, same splits.) The cluster is now
exactly where the plan said it would be: `if_body_135`'s false arm *is*
the `is_solid` object-loop region and cannot flatten before the loop
stage, and `if_join_136`'s arm holds the `btn(k_up/k_down)` reads. The
guard machinery itself is the reusable part: any future load-across-store
whose distinctness is real but unprovable is now one recipe field, not a
new rule.

**Update, after the pixel loops were masked** (`mask_loop` x2 on
`anonymous_61`'s inlined `move_x`/`move_y`, their `is_solid` chains made
eager first with 7 `speculate_region` entries - one of them the first
*diamond* serialization - plus 6 `demote_create` and one `merge_blocks`):

    branches: 2245 of 36925 executions split the state, across 16 distinct sites

Frame 34: 1.78 s -> 1.59 s, fragments 102 -> 67 mean (614 -> 371 max),
splits 4058 -> 2245. The loops themselves accounted for 626; the
downstream multiplier paid the other ~1200: the hot `btn` pair 618 -> 393
each, the dash diamond 648 -> 372, `and_or_join_126` 366 -> 228,
`if_join_136` 231 -> 144, `if_body_135` 156 -> 102. `anonymous_61` no
longer splits at all except a 9-split `__entry` exposure. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x6 (two 17-split sites woke) | 1261 | input fan-out |
| `in_i1_074_cont` (= old `if_join_95`) | 372 | the outer dash diamond, convertible last |
| `and_or_join_126`, `if_join_136`, `if_body_135` | 474 | jump/dash cluster, gated on object loops / `btn` |
| `in_i1_068` x2 (`spikes_at` tile loops) | 96 | mask family, but its break calls `kill_player` |
| `__main` `in_i1_012` x2, `anonymous_61` `__entry`, 1 tail site | ~42 | small exposures |

`mask_loop` keeps the loop *rolled* and gives it a uniform constant trip
count (`k <= 8`, per-state, never splits); which lanes still iterate
becomes mask data (`take = active && (i <= bound)`), every latch/break
store is masked with the load-adjacent select-store, the break edge is
deleted, and `assert_true(bound <= 8)` covers the claim that 9 iterations
are enough. No unrolling: the model only forbids *per-lane* control
state, and a uniform-trip rolled loop has none - the same reason the
object-table loops were never a problem. The differential is identical
through frame 34, guard included.

**Update, after the wall-jump arm went eager** (`fold_reflexive` + 4
`fold`/`dce` rounds, 2 `speculate_region` gates, 2 `demote_create`, then
the grace diamond and the jump branch as the first two *masked*
`speculate_region` entries):

    branches: 1531 of 30868 executions split the state, across 13 distinct sites

Frame 34: 1.59 s -> 1.46 s, memory 0.63 -> 0.53 GB, fragments 67 -> 46
mean (371 -> 248 max), splits 2245 -> 1531. Two findings paid for the
stage. First, the `is_solid(x, 0)` chains all open with a gate on `0 > 0`:
`fold_reflexive` folds the reflexive comparison without evaluating
anything, and two fold/dce cascades deleted 14 never-taken collide loops
across 7 sites in 5 functions - 1328 instructions of dead code, including
inside regions earlier entries had made eager. Second, with the arm
flattened, `speculate_region` with `"mask": true` ran the whole wall-jump
arm - grace stores, `is_solid` scan, spd writes - unconditionally, every
store going through a load-adjacent masked select; the grace diamond's two
store-carrying arms serialize soundly because their masks are the two
sides of one condition. The jump/dash cluster (474 splits) is gone
entirely, and the `btn` family shrank with the fragment count it
multiplies (1261 -> 943). The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x6 | 943 | input fan-out |
| `in_i1_074_cont` (the outer dash diamond) | 372 | arms contain `btn` calls, waits for the endgame |
| `in_k1039_cont` (= old `if_join_136`, the dash gate) | 84 | condition reads `btn`, arm allocs |
| `in_i1_068` x2 (`spikes_at` tile loops) | 96 | mask family, but its break calls `kill_player` |
| `__main` `in_i1_012` x2, `anonymous_61` `__entry` | 36 | small exposures |

Every remaining split is now `btn`-tainted, `spikes_at`, or a small
exposure - there is no convertible triangle, diamond or maskable region
left that splits (the profile's triangle table shows 0 splits across all
139 tracked shapes).

**Update, after the `spikes_at` nest went eager** (`mget` promoted to a
pure builtin, 7 `pin_builtin`, 1 `speculate_region` triangle, 3
`fuse_breaks` collapsing the four-tile early-exit cascade to one break,
then `mask_loop` twice with the new `span` and `break_to` fields -
inside-out, the inner pass leaving exactly the merged shape the outer pass
consumes):

    branches: 505 of 12166 executions split the state, across 11 distinct sites

Frame 34: 1.46 s -> 1.40 s, memory 0.53 -> 0.46 GB, fragments 46 -> 16
mean (248 -> 86 max), splits 1531 -> 505. The 96 `spikes_at` splits are
gone, and everything downstream shrank with the fragment count it
multiplies - the same dynamic as every stage since the store triangles,
but larger, because the tile loops split *early* in the frame and every
later site paid per fragment. The remainder:

| family | splits | what it is |
|---|---|---|
| `btn` x6 (the `in_j2_0xx_if_join_10` sites) | 331 | input fan-out |
| `in_i1_074_cont` (the outer dash diamond) | 108 | arms contain `btn` calls, waits for the endgame |
| `in_k1039_cont` (the dash gate) | 30 | condition reads `btn`, arm allocs |
| `__main` `in_i1_012` x2, `anonymous_61` `__entry` | 36 | small exposures |

The kill branch (`br found ? kill : continue`) that the conversion was
expected to expose does not appear as a split site at 34 frames: the
`in_j2_*` sites above are the `btn` inlines, not `kill_player`. K paid a
small price for the eager 2x2 tile window running every frame: loops-kept
3934 -> 3980, mean dynamic 3728 -> 3861.

Two guards carry the stage's premises: `assert_true(init >= 0)` and
`assert_true(bound - init < span + 1)` per loop (strict, because
`min(15,(x+w-1)/8)` has no `flr` and the first guard draft with `<= span`
failed loudly on fractional bounds two lanes wide). One interpreter
extension was needed: eager execution feeds `%` negative and fractional
dividends (`y % 8` for lanes above the screen), so `Pico8Num::checked_rem`
now implements PICO-8's floored modulo (`-2 % 8 == 6`) for positive
integer divisors - a strict generalisation of what the OCaml reference
computes on its non-negative domain, checked by the differential run.

Following the branch-site table, `btn` was rewritten to be branch-free: a
`__concretize` builtin that turns the `UnknownBool` button into a per-lane
`Bool` by doubling the lane space, instead of an `if` that sends the state down
both edges. Total lanes are identical either way, and `rewrite observe` confirmed
the lane content was byte-identical over 25 frames.

It is a clear regression:

| | time | memory | mean fragments |
|---|---|---|---|
| `btn` as an `if` (state duplication) | 4.47 s | 1.12 GB | 576 |
| `btn` via `__concretize` (lane expansion) | 5.46 s | 1.72 GB | 576 |

**The fragment count did not move at all.** The split moved from inside `btn` to
its caller: `if btn(k_jump) and ...` now branches on a lane-varying bool and
splits into the same two states, one branch later.

And it is *worse* than that, because the old formulation was doing something
useful. Duplicating the state gives two states in which the button is
`Bool(Scalar(true))` and `Bool(Scalar(false))` - **scalars**, which cost no
per-lane storage and make every downstream condition derived from the button
uniform, hence free. Lane expansion makes the button a vector, so those
conditions become lane-varying, they split with real `filter_by_mask` work, and
every value derived from the button is now per-lane storage. Hence the 54% more
memory.

So `UnknownBool` sending a state down both edges unfiltered is **load-bearing,
not a wart**. Fragments are not gratuitous: each one is a scalar-specialised
copy of the state, which is exactly why 93% of branches are free.

The corollary for the plan: input fan-out as lane expansion only pays once the
frame is *already* branch-free, because only then is there no downstream branch
left to absorb the split. It is a late-stage change, not an early one. Reverted;
`rewrite observe` was kept, since checking a harness change against lane content
is what made this cheap to evaluate.

### What `gc` is actually doing (2026-08)

`gc` is 21% of runtime at ~46 us per call, and the obvious guess is that it is
copying per-lane data: it rebuilds the heap to renumber `HeapId`s, and a
vectorized cell holds one value per lane. Making `MaybeVector::Vector` an
`Arc<Vec<T>>` would make that copy a refcount bump.

Instrumented, per gc call:

| | per call | total |
|---|---|---|
| cells visited | 280 | 6.0 M |
| lane elements copied | 3291 | 71 M (284 MB) |
| object tables rebuilt | 21 | 0.45 M |
| string keys cloned | 81 | 1.7 M (8 MB) |
| pointer slots rewritten | 105 | 2.3 M |

284 MB of lane data over the whole run is about 28 ms of memcpy against gc's
1.0 s, so **lane copying is ~3% of gc and the `Arc` change would buy almost
nothing.** The cost is per *cell*, not per lane: 165 ns each, which is roughly
two or three allocations - cloning the `HeapValue`, the `Box<HeapValue>` the
`FrozenVec` stores, the rebuilt `FxHashMap` for each object table - plus the
`old_to_new` hash map traffic.

That cost scales with the number of states, not with lanes per state. So it is
not a separate lever at all: it is the same fragmentation problem, and
if-conversion is what shrinks it.

### Recipe replay is part of the benchmark (2026-08)

`rewrite build` prints where replay time went:

    replay: 2.0s total - clone 0.1s, apply 0.3s, validate 0.9s, verify 0.7s

Every command in the tool replays the recipe before doing anything, so this is
a constant added to `verify`, `screen`, `suggest`, `bench` and `print` alike.
It had silently reached **67s** - 97% of it whole-program dominance validation
after each of 554 entries, on functions that inlining had grown to hundreds of
blocks. See the commit for the three fixes.

The lesson for benchmarking: quote replay separately from the thing being
measured. A `bench --frames 34` that reports 4.36s was, for most of this
session, a 70-second command.

## Scalar reference point

`concrete_run` executes 30 frames single-lane in 46 ms including parse and init,
i.e. ~0.5 ms per single-lane frame. The vectorized interpreter is already ~10x
more efficient per lane than the scalar path at frame 30, so vectorization is
working - the problem is what happens between frame boundaries.

## Native speed of light (2026-08-16, plans/native-probe.md)

The IR transpiled to plain Rust (`transpile` + `native-probe/`), single lane,
hex-exact against `concrete_run` over a 400-frame randomized input tape, on
an otherwise-idle core 15 with the campaign pinned off it:

- **6.2-6.4 µs per frame-lane** (1000 reps x 400 frames). IPC 3.87,
  cache-miss rate 0.91% - compute-bound, L1-resident.
- Interpreted `concrete_run` same day, same core: ~1.14 ms/frame under
  campaign load (~0.64 ms on a quiet machine) -> **~100-180x**.
- The abstract interpreter's ~17 µs *per lane* means compiled scalar code
  beats vectorized interpretation per lane by 2.7x - before any
  specialization (v0 uses linear-scan field lookups and no allocation
  reuse). This prices the compile-the-abstract-semantics direction.

## Historical note

The numbers once in this file (frame 30 = 18 s, frame 39 = 1030 s, OOM at frame
40) were collected 2024-12-29 and are obsolete by ~25x. They predate mimalloc,
LTO, COW `LocalEnv`, `FxHashMap`, GC-before-vectorize and the rest of the perf
work. Do not use them for extrapolation.

The `interpreter` branch's unverified `mem2reg` + `block_coalesce` were dropped
along with the rest of that optimizer, which cost 1.3x time and 1.9x memory at
frame 40. `promote_cell` won that back and then some.

# The bucket loop (2026-09-12, plans/buckets.md)

Room (1,0), level-0 forward f0-f44, release, single thread, `rewrite
forward --to 44` (record mode = the ladder's forward). Every row below
reproduces `gates/ckhash_room10_f000-044.txt`, `gates/posgraph_room10_f044.txt`
and `gates/marks_room10_win9-101_h35.txt` bit for bit.

| | State-bridged loop (`93313b6`) | Rt2 blocks (`31002e3`) | buckets (`78d0276`) | key in the kernel (stage 6) |
|---|---|---|---|---|
| f0-f44 wall (incl. ~4 s kernel assembly) | 92.6 s | 53.2 s | 26.2 s | **16.7 s** |
| f44 frame | 15.1 s | 9.4 s | 4.8 s | **2.7 s** |
| kernel calls / rows per call | 115,039 / 17.2 | 115,039 / 17.2 | 879 / 2,252 | 879 / 2,252 |
| executed AVX-512 lanes that are padding | 39.2% | 39.2% | 0.4% | 0.4% |
| blocks per frame at f44 | 17,578 | 17,578 | 42 | 42 |
| checkpoint at f44 | 2.3 s | 1.28 s | 0.12 s | 0.12 s |
| peak RSS | 3.66 GB | 1.60 GB | 0.62 GB | **0.59 GB** |

Where the buckets' f0-f44 time goes: `fwd.engine` 24.0 s of 25.8 s (the
kernel call including its append step: key fold, door dedup, column
pushes), `fwd.checkpoint` 0.62 s, `fwd.route` 0.24 s. Before buckets the
profile was 5% assembled-kernel arithmetic and ~55% bookkeeping (per-call
setup on 17-row calls, structure clones, the row key hashed twice in
scalar Rust, three rounds of regrouping).

Backward (synthetic win (9,101), horizon 35, levels Bits(0)/Bits(1)): the
wide re-run marks the same 48 / 40 states with the same 525 / 34 row
re-runs as the per-lane draft it replaced.
