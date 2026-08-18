# Dedup-on-the-fly: DONE 2026-08-18 (348 -> 89 ms at f35)

Landed, gates exact at f20/25/30/35. What shipped differs from the plan
below in one deliberate way, so read this first:

- The generated key is NOT a bit-exact copy of `Rt2::boundary`'s key.
  It mixes ORIGINAL cell ids rather than the canonical BFS numbering,
  which is all a per-chunk seen-set needs, and it removed the whole
  class of "mirror the canonical numbering" bugs the plan warned about.
  It does use boundary's own `mix64`/`cell_mix` (hoisted to module
  scope in runtime2) and mirrors the two VALUE canonicalizations that
  decide what merges: rem cells (replaced by one wide interval, so they
  contribute nothing per-lane and are skipped) and dash_effect_time
  (clamped at 0). Which cells those are comes from `Rt2::mark_walk` per
  chunk, as a `KeyPlan` mask, re-checked against the OUTPUT block
  before the boundary - a stale rem bit would merge rows that differ.
- Step 1's CELESTE_KEYCHECK gate was therefore replaced by a stronger
  and cheaper check: the within-chunk dedup ratio AFTER boundary. It
  went 8.3:1 -> 1.0:1, i.e. the register-side key provably collapsed
  everything boundary would have.
- Step 4 (boundary_with_keys) is NOT needed and not done: with 8.3x
  fewer rows reaching it, boundary's re-hash is 15% of a much smaller
  frame.
- Chunk size inverted as a consequence and is retuned to 256 (see
  BENCHMARK_DATA.md).

Knobs: CELESTE_PREDEDUP=0 (old path, for A/B), CELESTE_CHUNK_ROWS,
CELESTE_KERNEL_CLASSES=steady,dash,frozen (bisect a class kernel).

Remaining ideas from the original plan are still open at the bottom
("Other ideas"); the cross-chunk 11:1 that pre-dedup does NOT catch
would need a shared seen-set and is unmeasured.

---

# Dedup-on-the-fly plan (next lever after the kernel night)

Context: plans/kernel-plan.md. The kernel engine (CELESTE_TILE=3) runs
f35 in ~390 ms wall (multi-core), gates exact. Phase split: 79% is the
"run" phase, and inside it the kernel arithmetic is ~free — the cost is
MATERIALIZING AND HASHING ~12M output rows that dedup down to ~269k
(a 45:1 ratio). Killing the materialization of doomed rows is the lever.

## The change (precise, in order — each step has a gate)

Everything stays inside the existing architecture; no new concepts.
The certification gate for every step is the SAME one used all night:

    CELESTE_TILE=3 ./safe-run.sh --memory 60G -- \
      ./native-probe/target/release/native-probe --abstract-bench \
      ~/celeste-checkpoints/room10-newlua-bench 35 --reps 5
    # must print: row-key SET EQUAL (gate 2) OK
    # also run frames 20, 25, 30 once each

### Step 1: generated per-lane row keys (the risky step — do it alone)

Add to the kernel emitter (src/bin/transpile/kernel.rs, render()):
a generated `pub fn row_keys(chunk: &Rt2, lo: usize, n: usize, live: u16,
sh: &KOutShared, kv: &KOut, keys: &mut [(u64, u64); W])`.

It must reproduce `Rt2::boundary`'s key EXACTLY, bit for bit
(runtime2.rs ~line 2140-2215: part1/part2 seeds + per-varying-cell
cell_mix / mix64 chain). Facts that make this small:
- Uniform cells contribute to part1/part2 ONCE per block. Generate a
  `pub fn key_seed(chunk: &Rt2) -> (u64, u64)` that folds every cell
  that is uniform in the OUTPUT block (input uniforms + untainted
  scalar outs + OUT_UBOOL cells + the boundary's canonicalized rem*)
  and cache it per chunk.
- Per-lane work is then only the varying outputs (~9-20 cells) + the
  identity vary columns: cell_mix(cell_id, value, seed) per cell,
  matching the Col::N fast path in boundary (the hoisted-constant
  form) and the Col::I / Col::V(Bool) forms for interval/bool cells.
- *rem canonicalization: boundary REPLACES rem cells with the wide
  interval [-0.5, 0.5-eps] before hashing (runtime2 "rem widening,
  Bits(0)"). So rem cells hash as a CONSTANT — put them in key_seed,
  NOT the per-lane part, even though the kernel outputs per-lane rem.
  Same for pinned timer globals (mark_walk's det_cells — read what
  boundary does to them and mirror it exactly).
- Make cell_mix/mix64 pub(crate) in runtime2 and call them — do NOT
  copy the constants.

Gate for step 1 (before using the keys for anything): in
run_chunk_kernel, compute the generated keys AND keep the old path;
after acc.boundary(ids), assert the generated key set equals
acc.row_keys as a set per chunk (debug_assert or env-gated check,
CELESTE_KEYCHECK=1). Run the bench with it on. Any mismatch = stop,
the generated hash is wrong; the usual suspects are rem, timers, the
button UBool cells, and Obj field-order canonicalization.

### Step 2: dedup before materializing

In run_chunk_kernel: per chunk keep `seen: FxHashSet<(u64,u64)>`
(capacity ~64*width). In the frame() callback: compute keys for live
lanes; for each live lane, `if seen.insert(key)` KEEP else drop.
Build a new live mask of kept lanes; call append_out only with that
mask. acc.boundary(ids) stays (it still canonicalizes + re-hashes the
~45x smaller block; that redundancy is fine for v1).

Gate: the standard bench gate (row-key SET EQUAL). Cross-block dedup
downstream makes per-chunk dedup semantically invisible.

### Step 3: measure + scaling

- reps 5 at f35 + phase split (CELESTE_PHASE_TIME=1). Expect the run
  phase to collapse toward the kernel's compute cost.
- Thread-scaling check: CELESTE_CHUNK_ROWS (default 64) — if worker
  count > chunk count at shallow frames, smaller chunks help; measure
  32 and 128 too.
- Record everything in plans/kernel-plan.md with a table; update
  BENCHMARK_DATA.md if the engine-level numbers change.

### Step 4 (only if step 2 wins big): skip boundary re-hash

Give Rt2 a `boundary_with_keys(ids, keys)` that trusts precomputed
keys (skip the hash loop, keep canonicalization + within-block dedup
+ retain). Gate unchanged. Do not do this before steps 1-3 are green.

## Other ideas (worth writing down, none blocking)

- **prefer-vector-width=512**: one-line RUSTFLAGS experiment
  (-C llvm-args or -Ctarget-feature tuning) — Zen4 double-pumps, so
  expect neutral-ish; measure once, keep the note.
- **Config density**: fork configs where most lanes are invalid still
  run full suffixes. Sorting rows within a chunk by frac(spd)!=0 at
  partition time would make fork configs dense. Only worth it if the
  post-dedup profile says forks matter.
- **KOut elision**: with generated row_keys, a variant whose 16 keys
  are ALL duplicates never needs its KOut consumed — LLVM may already
  elide; check the profile before doing anything.
- **K4 continuation**: Rt2 stays the in-probe fallback (spawn shapes,
  deopt rows). Rt3-dynexp becomes deletable only when per-shape
  kernels cover spawn/death shapes — do NOT delete yet.
- **Room (2,0) (K5)**: the recipe pattern is proven — per class:
  4-line overlay + certify + witness + emit. New shapes (fruit) need
  their own base trace campaign first (branch census → gb entries →
  select/fork conversions), same playbook as trace10.
- **Compile time**: 64 suffixes ≈ 4 min per kernel build; if the
  kernel count grows, split kernel_gen_* into their own codegen units
  or a subcrate.

## Ground rules (unchanged)

safe-run for anything heavy, ONE heavy job at a time, --memory 60G on
certification runs, perf data in ~/perf-scratch/, measure before and
after, suite (cargo nextest, never bare cargo test) before pushing
main-crate changes, commit+push at every stable point.

- **Fork-invariant hoisting (Philippe, 2026-08-18 morning)**: sharing
  across BUTTON variants is done (button-taint). The same should hold
  across SPLIT configs: an instruction whose operands don't depend on
  the forked fragment can hoist ABOVE that fork loop. Statically
  analyzable with the taint machinery generalized to one taint bit per
  fork site: route each instruction to the SHALLOWEST nesting level
  its operand taints allow (classic loop-invariant scheduling; the
  emitter's pre/suf buffer pair becomes a stack of buffers, one per
  fork depth + the button suffix). Not high priority - the fork loops
  are cheap after dedup-on-the-fly - but it composes with everything
  and the emitter already owns the mechanism.
