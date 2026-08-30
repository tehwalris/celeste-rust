# Old-search tear-out plan

Staged removal of the OLD abstract-search stack now that the minimal rebuild in
`src/frame.rs` is validated (forward byte-identical to `AbstractRun` per-frame
row-key sets through fan-out at 620k keys/frame; backward+`MarkFilter` preserve
the win across rem levels on the compiled engine).

This file maps every consumer of the old machinery, classifies the
`bin/rewrite.rs` subcommands, and gives a staged deletion order. **Stage 1
(provably-dead-now) is the only stage executed by the agent that wrote this
file; everything past it is for Philippe to drive** because it is either a
feature-removal decision or blocked on the driver migration.

The old stack being retired:

- `src/search/run.rs` (2611 lines) — `AbstractRun` + the forward step machinery.
- `src/search/sweep.rs` (191) + `src/search/sweep_time.rs` (1210) — the g/e/band
  "numbering" backward pass.
- the forward per-lane **origin** plumbing — `Rt2::origin`, `origin_tag_of`,
  `deopt_collect::{read_origins_named,inject_named}`, `asm_kernel` `track_origin`,
  `sweep::SWEEP_ORIGIN`.

`src/search/{checkpoint.rs, pos_graph.rs, state_mapping.rs}` are NOT part of the
retirement per se — but the OLD checkpoint format is what couples the drivers to
`AbstractRun` (see Stage 3).

---

## 1. Dependency surface (who consumes the old machinery)

### `AbstractRun` (`src/search/run.rs`)
Consumers, grouped:

- **`src/main.rs`** — the `--rewritten` raw forward runner (`AbstractRun::start`,
  `.step`, `.absorb_won_lanes`, `.states`, `.lane_count`). Lines ~63–90.
- **`src/bin/rewrite.rs`** — the big consumer:
  - `run_bench_forward` helper (~535): `start` / `start_with_deopt`, `set_variants`,
    `set_band`, `configure_visited_dir`, `record_pos_graph{,_from}`, `restore`,
    `absorb_won_lanes`, `visited_table`, `deopt_events`, `variant_events`,
    `pos_graph_pairs`, `take_pos_graph`, `states_before_merge`.
  - `Widencheck` (~1423): `start` + `start_rem_only` + `observe_frame`.
  - `Simdcheck` (~1467): `start_with_deopt`, `without_frontier_subtract`,
    `restore`, `observe_frame`.
  - `Deoptcheck` (~1605): `start` + `start_with_deopt`, `observe_frame`,
    `deopt_events`.
  - `Variant` / `BandFilter` builders (~442, ~945).
- **`src/search/sweep_time.rs`** — `backward_sweep_time` drives a forward
  `AbstractRun` internally (`start_with_deopt` at ~863, `set_variants`,
  `disable_frontier`, `skip_boundary_merge`, `restore`, `take_states`,
  `deopt_events`). **This is the tightest coupling**: the backward pass is built
  on the forward engine.
- **`src/search/checkpoint.rs`** — stores `deopt_events`, `lane_count`,
  `effective_chunk_cap()`; the OLD checkpoint schema. The driver coupling.
- **`src/trace/probe.rs`** (~290) — a `#[test]` uses `AbstractRun::start` only to
  obtain the single `_init` state for a differential against `FrameEngine`.
- **`src/frame.rs`** (~856) — the NEW code's forward-differential test builds an
  old `AbstractRun` to compare against. Expected; goes away when the old engine
  goes, not before.

### `sweep_time` / `backward_sweep_time` (`src/search/sweep_time.rs`)
- **`src/bin/rewrite.rs`** only: `run_ladder` (~1208, ~1272) and the `Sweep`
  subcommand (~2299). Plus `sweep::{save_g,load_g,band_sizes,row_keys,
  G_UNREACHABLE}` used by `run_ladder`, `Sweep`, `CountOptimal`, `TraceWitness`,
  `MigrateVisited`.
- `row_keys` and `G_UNREACHABLE` also leak into `src/search/run.rs` (the
  band-filter inside the forward step) — so `run.rs` and `sweep.rs` are mutually
  coupled through the band.

### Forward origin plumbing
The per-lane origin column exists **solely to serve the backward sweep's row
provenance** — `ORIGIN_TAGS` is exactly `[SWEEP_ORIGIN]`:

- `src/search/sweep.rs:37` — `pub const SWEEP_ORIGIN`.
- `src/compiled/mod.rs` — `ORIGIN_TAGS = [SWEEP_ORIGIN]`, `origin_tag_of`,
  `inject`/`read` around `Rt2::origin` on import/export (~191, ~199, ~217, ~484,
  ~608, ~644).
- `src/compiled/asm_kernel.rs` — `track_origin` gate (~128, ~237, ~248): carries
  the origin column through the fused kernel.
- `crates/celeste-interp/.../deopt_collect.rs` — `read_origins_named` /
  `inject_named`; only callers are `sweep_time.rs` and `compiled/mod.rs`.
- `crates/celeste-engine/.../runtime2.rs` — the `origin` column itself
  (grow/boundary-mix/keep/split/append, ~324, ~1071, ~1302, ~1493, ~1521, ~1569).
- `src/search/sweep_time.rs` — the only place that injects a real tag
  (`SWEEP_ORIGIN`) and reads it back (~959, ~1021, ~1081).

Conclusion: **when the sweep goes, the entire origin column can go with it** —
nothing else uses a non-empty origin. This is a Stage-2 feature removal blocked
on the sweep deletion, not a Stage-1 item.

---

## 2. `bin/rewrite.rs` subcommand classification

| Subcommand | Line | Category | Drives |
|---|---|---|---|
| `Bench`        | 1738 | **LIVE search** | forward `AbstractRun` (via `bench`/`run_bench_forward`) |
| `Sweep`        | 2280 | **LIVE search** | `backward_sweep_time` |
| `Ladder`       | 2353 | **LIVE search** | `run_ladder` = forward bench + `backward_sweep_time` |
| `Widencheck`   | 1423 | diagnostic (old path) | `AbstractRun::start`+`start_rem_only`, `observe_frame` |
| `Simdcheck`    | 1467 | diagnostic (old path) | `start_with_deopt`, `observe_frame` |
| `Deoptcheck`   | 1605 | diagnostic (old path) | `start`/`start_with_deopt`, `deopt_events` |
| `ShapeCensus`  | 1653 | diagnostic (old path) | forward states |
| `LeadingEdge`  | 2453 | diagnostic (checkpoints) | reads saved frame batches |
| `ShapeInventory` | 2482 | diagnostic (checkpoints) | reads checkpoint dir |
| `TraceWitness` | 1870 | TAS-output | `sweep::{load_g,G_UNREACHABLE}` band probe |
| `ExtractTas`   | 1964 | TAS-output | band/g replay |
| `CountOptimal` | 2101 | TAS-output | `backward_sweep_time` |
| `MigrateVisited` | 2370 | one-time migration | old visited.bin → rowkeys |

The three LIVE subcommands are what the rebuild must replace before `run.rs` /
`sweep*.rs` can be deleted. The diagnostics and TAS tools are the long tail that
must each be either ported to `frame.rs` or dropped.

---

## 3. Staged deletion order

### Stage 1 — provably dead NOW (DONE by this agent)

Removing an item with zero callers anywhere in the workspace cannot change
behavior. The workspace is the 5 crates + root (`native-probe` /
`traced-kernel-check` are not present in this checkout); `find src crates`
covers all compiled code.

- **DONE, committed:** `StateObservation::digest` + its only helper `hash_into`
  in `src/search/run.rs` (14 lines + the now-unused `use std::hash::{Hash,
  Hasher}`). Zero callers; `observe_frame`'s `BTreeSet` is the only comparison
  path. Commit `f11804f`.

### Stage 1b — verified zero-caller orphans OUTSIDE the search stack (ready, NOT done)

The whole-workspace scan surfaced 20 more `pub` fns/methods with **zero callers**
(definition is the only reference in `src crates`), none of them trait-method
impls (so not reachable via dynamic dispatch), the build already warning-free.
These are orphans of the **earlier interpreter/CFG deletion**, not of the search
tear-out, so they are split out here for a separate green-lit sweep rather than
folded into the search commits. Each is individually safe to delete; expect small
cascades (a deleted method may orphan a private helper — chase the warning).

Left undeleted deliberately: I did not want to unilaterally prune the foundational
`celeste-ir` / `celeste-core` / `celeste-engine` public surface during Philippe's
in-flight tear-out. Confirm intent (some may be deliberately-kept debug/inspection
API) and delete in one commit per crate.

| Item | File | Enclosing |
|---|---|---|
| `eval_all` | `src/trace/eval.rs:79` | free fn |
| `Run::start_block` | `src/trace/run.rs:243` | inherent |
| `Graph::eval_strict_in` | `src/transpile/graph.rs:780` | inherent |
| `Program::block_count` | `src/program/mod.rs:258` | inherent |
| `format_program` | `src/program/mod.rs:29` | free fn |
| `Program::instruction_count` | `src/program/mod.rs:251` | inherent |
| `Rule::cell_fields_mut` | `src/program/recipe.rs:550` | inherent |
| `Recipe::to_text` | `src/program/recipe.rs:645` | inherent |
| `Instruction::has_side_effects` | `crates/celeste-ir/src/ir.rs:522` | inherent |
| `Cfg::map_blocks` | `crates/celeste-ir/src/ir.rs:793` | inherent |
| `Block::new_label_map` | `crates/celeste-ir/src/ir.rs:684` | inherent |
| `Block::split_block_phi_instructions` | `crates/celeste-ir/src/ir.rs:652` | inherent |
| `Terminator::successor_labels` | `crates/celeste-ir/src/ir.rs:601` | inherent |
| `SlotMap::try_slot_of` | `crates/celeste-ir/src/ir.rs:732` | inherent |
| `format_block` | `crates/celeste-ir/src/print.rs:162` | free fn |
| `parse_local_name` | `crates/celeste-ir/src/print.rs:23` | free fn |
| `Pico8Vec2::as_i16s_or_err` | `crates/celeste-core/src/pico8_num.rs:225` | inherent |
| `Rt2::is_canonical_order` | `crates/celeste-engine/src/runtime2.rs:522` | inherent |
| `build_block` | `crates/celeste-engine/src/slots.rs:224` | free fn |
| `resolve_path` | `crates/celeste-engine/src/slots.rs:173` | free fn |

(`src/frame.rs::find_optimum` also scans as zero-caller — it is NEW rebuild API
awaiting its driver; do NOT delete.)

Re-verify a candidate before deleting with:
`grep -rn "\bNAME\b" src crates --include=*.rs` → only the def line.

### Stage 2 — feature removals (decisions, for Philippe)

Each removes a capability, not dead code. Blocked on a decision that the rebuild
covers (or intentionally drops) the feature.

- **Origin column / backward-sweep provenance.** Delete `SWEEP_ORIGIN`,
  `ORIGIN_TAGS`, `origin_tag_of`, the `deopt_collect` module, `asm_kernel`
  `track_origin`, and the `Rt2::origin` column end-to-end. Blocked on: the sweep
  being gone (its only consumer). Cheap and mechanical once Stage 4 lands sweep.
- **Deopt / `start_with_deopt` / `StateMapping`.** The forward deopt path
  (`Deoptcheck`, `--deopt` on `Bench`, `state_mapping.rs`,
  `checkpoint.deopt_collect_first`). Blocked on: whether the rebuild keeps a
  deopt story at all (CLAUDE.md doctrine: "never deopt to the interpreter" — this
  may be a pure deletion, but confirm the rebuild's coverage-gap handling first).
- **Shape variants (`Variant` / `set_variants` / `build_variants`).** Used by
  `Bench`/`Sweep`/`Ladder` `--variants`. Blocked on: does the rebuild need
  per-shape variant dispatch, and is it already covered by `frame.rs`'s engine?
- **Banding (`BandFilter` / `set_band` / `sweep::band_sizes`).** Blocked on:
  whether the rebuild's `MarkFilter` subsumes the band. It appears to (the
  validated backward+filter is the replacement), so this is likely a delete —
  confirm the ladder driver no longer needs `band_dir`/`band_horizon`.
- **Numbering (g/e vectors, `save_g`/`load_g`).** The whole `sweep.rs` g-array
  vocabulary. Blocked on: whether TAS tools (`TraceWitness`, `ExtractTas`,
  `CountOptimal`) move to the new backward or are dropped.

### Stage 3 — driver migration (for Philippe)

The blocker for the big file deletions. `main.rs --rewritten`, the three LIVE
`rewrite.rs` subcommands (`Bench`/`Sweep`/`Ladder`), and `trace/probe.rs`'s test
must move off `AbstractRun` / `backward_sweep_time` / the OLD checkpoint format
(`search/checkpoint.rs`) onto `frame.rs`'s `forward_run`/`forward_resume`,
sharded checkpoints, and `backward_run`/`backward_walk`/`ladder_at_horizon`/
`find_optimum`. `probe.rs` needs only the single `_init` state — trivially
repointed at `FrameEngine` / a one-lane block.

### Stage 4 — the big file deletions (for Philippe, after Stage 3)

Once no driver references them:

- `src/search/run.rs` (2611) — after Bench/main/probe/sweep_time are migrated.
- `src/search/sweep_time.rs` (1210) + `src/search/sweep.rs` (191) — after
  Sweep/Ladder/CountOptimal/TraceWitness/ExtractTas are migrated or dropped, and
  after the `run.rs` band coupling (`row_keys`/`G_UNREACHABLE`) is severed.
- The diagnostics/TAS subcommands in `rewrite.rs` — port or drop each per §2.
- `MigrateVisited` — delete once confirmed the old visited.bin format is gone
  from every live checkpoint dir (one-time tool; kept only until then).
- The OLD `src/search/checkpoint.rs` — after the new sharded format is the only
  one written/read.

Ordering constraint: `run.rs` and `sweep*.rs` cannot be deleted independently —
they are mutually coupled through the band (`run.rs` imports `sweep::row_keys`
and `sweep::G_UNREACHABLE`; `sweep_time.rs` drives an `AbstractRun`). Remove the
band feature (Stage 2) first, then delete both together.

---

## 4. Notes for the executor

- Gate after every change: `./one-cargo.sh cargo check --workspace --tests`
  (must stay warning-free — a new dead-code warning means a caller/callee pair
  got split; chase it).
- Pre-commit: `./safe-run.sh -- ./one-cargo.sh cargo nextest run --cargo-profile quick`.
- Several `sweep_time.rs` `pub` items (`build_index`, `build_cell_store`,
  `prepare_pos_graph`, `RowIndex`, `CellStore`) have zero EXTERNAL callers but
  are used inside the file — they are over-public, not dead. Reducing their
  visibility is a valid cleanup but not a deletion; left for whoever deletes the
  file wholesale.
