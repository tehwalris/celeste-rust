# Code-slimming survey (2026-08-28)

Read-only survey for dead code, duplication, and reorg opportunities. No
edits made. Line counts from `wc -l`; non-use claims from `rg`/`grep` with
the command shown. Generated kernel bodies
(`crates/celeste-kernels-room{00,10,20}/src/**`, ~900k lines) are NOT
counted as duplication - they are generated per room and per variant.

Confidence key: **certain** (grep proves it) / **likely** (strong evidence,
one assumption) / **judgment** (needs Philippe's call on direction).

Hand-written totals for orientation:

| unit | lines |
|---|---|
| celeste-core | 1,184 |
| celeste-names (frozen `gen.rs`) | 263 |
| celeste-ir | 2,725 |
| celeste-interp | 14,785 |
| celeste-engine | 3,426 |
| src/ (incl. bins) | 37,311 |
| native-probe | 2,074 |

The owner's "delete about half" is real and already has a written plan:
`plans/delete-the-interpreter.md` scopes ~55k lines out / ~22k kept. This
survey confirms that scope with file-level evidence and adds a few items
that plan does not name (the asm prototype, the splitmix duplication, the
spec-probe diagnostic).

---

## Theme A - the interpreter/IR/program stack (the "half")

The single largest slimming, already planned as Phase 4 of
`delete-the-interpreter.md`. It is REPLACE-then-delete, not free deletion:
the block-native driver, precision ladder, visited set, sweep, and frozen
goldens must be built on the engine first. Flagged here with today's
dependency evidence so the ordering is visible.

| unit | lines | what still needs it TODAY | confidence |
|---|---|---|---|
| `crates/celeste-interp` | 14,785 | the oracle + the production driver `AbstractRun` (run.rs) run over `interpreter::State`; ladder rungs k=1..16 run interpreter-only (`abstraction.rs`) | judgment (gated on build) |
| `crates/celeste-ir` | 2,725 | frontend feeds the interpreter and the frozen `Program`; the tracer keeps its OWN Lua frontend (`src/trace`), so IR dies with the interpreter | judgment |
| `src/program` (frozen/recipe) | 966 | `frozen::rewritten("rewrites.jsonl")` loaded by main.rs, compiled/mod.rs, differential.rs, trace/probe.rs tests | judgment |
| `src/main.rs` runner + 52 tests | 2,837 | the un-rewritten `celeste-rust -n` runner + 52 interpreter unit tests | judgment |
| `src/bin/rewrite.rs` | 2,934 | campaign CLI; every subcommand drives `AbstractRun`/`State` | judgment |
| `native-probe` | 2,074 | engine gate harness, State-based (`state_mapping`) | judgment |
| `src/concrete.rs` + `src/bin/concrete_run.rs` | 317 | interpreter TAS-replay tool | judgment |
| `src/search/differential.rs` | 938 | reachable only from `bin/rewrite.rs` + its own tests (grep: no other `differential::` referent) | likely |

`src/trace` (12.0k) is already independent of the interpreter except for
`celeste_interp::game_runner::start_room()` - a pair of ints from
`CELESTE_START_ROOM`. Verified: `rg 'celeste_interp|interpreter::' src/trace`
returns only `game_runner::start_room` / `apply_start_room` call sites. So
the tracer is NOT entangled and does not block the deletion.

---

## Theme B - duplication (confirmed)

### B1. Two full row-key implementations - interpreter vs engine (certain)

The canonical key is the ENGINE's:
`runtime2::{mix64, cell_mix, av_code, boundary_finish}`
(`key = mix64(part + Σ_cells cell_mix(cell, av, seed))`,
runtime2.rs:35-58), plus its vectorized twin
`engine::kernel::{mix64_v, zw_mix64, zn_cell_mix, ...}` (kept beside the
scalar one ON PURPOSE - documented kernel.rs:267, gated byte-identical by
`the_vector_hash_agrees_with_the_scalar_mix64` and
`vector_cell_mix_agrees_with_the_scalar_definition`).

The interpreter has a SECOND, independent implementation:
`virtual_merge::row_key_hashes` (virtual_merge.rs:641) builds the per-row
`(h1,h2)` from columns, and `RowTable::key(shape_hash,h1,h2)`
(row_table.rs:63) folds the shape hash in. Callers today:
`sweep.rs:54`, `vectorize.rs:{1545,1589}`, `checkpoint.rs`. These exist so
the interpreter path dedups the same way the engine does; a gate
(`CELESTE_KERNEL_KEY_CHECK`, runtime2.rs:57) proves they agree.
**Deletable once the search keys on the engine key and the interpreter
retires** - i.e. Theme A. Confidence certain that it is a second
implementation; deletion is Phase-4-gated (judgment).

`RowTable` itself (row_table.rs, ~210 lines) is the interpreter's visited
table, used by `visited.rs`, `sweep*.rs`, `checkpoint.rs`. The plan moves
its role ("visited set, State-free already, keyed on (u64,u64)") into the
engine; the machinery is duplicated with whatever the engine driver grows.

### B2. splitmix64 finalizer copied (certain, tiny)

`runtime2::mix64` (runtime2.rs:35) and `row_table.rs`'s local `mix`
(row_table.rs:53, comment literally says "Splitmix64 finalizer") are the
same function - same constants `0xbf58476d1ce4e5b9`, `0x94d049bb133111eb`.
The `RowTable::key` half-fold also hard-codes the golden ratio constant
`0x9e37_79b9_7f4a_7c15` (row_table.rs:64) that appears again in kernel.rs.
Small, but a real copy; the row_table copy goes with Theme A. (The
vectorized `mix64_v` in kernel.rs is a deliberate, gated duplicate, not
this.)

### B3. Two shape-hash producers (certain)

- interpreter: `vectorize::shape_hash_of_state` / `shape_of_state(..).cached_hash()` (vectorize.rs:160,211), used by `sweep.rs:47`, `bin/rewrite.rs`.
- engine: `Rt2::shape_hash_of` (runtime2.rs:594), used by the tracer/driver (`trace/*`, `compiled/bridge.rs`).

Same idea, two hashers. The interpreter one dies with Theme A;
`Rt2::shape_hash_of` is the keeper. Gated equal by
`shape_hash_matches_the_built_shape` (vectorize.rs:2072).

### B4. Two AST/IR interpreters coexist (NOT a merge target - clarification)

`src/trace/interp.rs` (1,965) is the tracer's own AST interpreter (two
domains, `Concrete`/`Symbolic`); `crates/celeste-interp` is the vectorized
IR interpreter/oracle. This LOOKS like duplication but is the intended end
state: the tracer's is the keeper, the interp crate is the oracle slated
for deletion. No action beyond Theme A.

### B5. streaming vs phased frame paths in run.rs (judgment)

`src/search/run.rs` (2,927) carries both `finish_streaming_boundary` /
`step_parallel_phased` and the phased path (run.rs:451-2118). The phased
path is retained deliberately for the frontier-subtract corner and as the
equivalence reference (`parcheck.sh`), per the comments at run.rs:1777.
Not safe to delete in isolation; whole file is Phase-4 replaced anyway.

---

## Theme C - checked-in experiment/prototype left in the tree

### C1. The AVX-512 asm backend - `src/transpile/asm/` (judgment, BIG)

**3,155 lines** (`codegen.rs` 1,912, `tests.rs` 958, plus `jit`, `callout`,
`mod`). `mod.rs` states it outright: "A PROTOTYPE ... It is not wired into
any kernel; `super::tests` is its correctness gate." Proof of non-use:
`rg 'transpile::asm|asm::compile|asm::jit'` outside `src/transpile/asm/`
returns NOTHING. It compiles into the normal build (not `#[cfg(test)]`
gated at the `pub mod asm` site), so it is dead weight on every build of
`celeste-rust`.

Its purpose - measuring emit/runtime cost of hand-emitted AVX-512 vs the
graph->Rust->LLVM path - appears DONE: `tests.rs` prints the numbers
(~1000x emit cost, ~1.27x off the LLVM runtime, port-bound) and
`plans/asm-backend.md` (346 lines) records them. If the measurement is
concluded, deleting the module + plan removes ~3,155 lines with zero
production impact. The only reason not to is if Philippe still wants the
backend as a future direction. **Single biggest standalone deletion that
does not depend on retiring the interpreter.**

### C2. `--spec-probe` / `CELESTE_SPEC_*` diagnostic (likely, small)

`trace::kernel::specialize_probe` + the `transpile --spec-probe` subcommand
+ `CELESTE_SPEC_*` envs were built in the `plans/specialize.md` session as
a flag-gated diagnostic, explicitly "not wired into anything ... kept for
chasing this down." That investigation reached a conclusion (negative for
enumerable specialization; positive for the constant-lattice, which then
grew its own `write_room_kernels_lattice` path). The probe is a diagnostic
in the sense CLAUDE.md warns about - it belongs in a `src/bin/probe.rs`
subcommand or should be dropped now that the question is answered. Needs a
line count and owner call.

### C3. Zero-assertion `#[test]` diagnostics (judgment, per CLAUDE.md)

CLAUDE.md already names five zero-assertion `#[test]`s in `trace::kernel`
/ `trace::verify` that only `eprintln!` a table (e.g.
`what_specializing_the_fork_would_cost` ~120 s). They are the wrong shape
(nothing fails, `--run-ignored all` pays their cost). The standing
instruction is to move them behind one `src/bin/probe.rs`. Consolidating
C2 + C3 into that binary is the clean reorg.

---

## Theme D - over-large modules mixing concerns

Rough sizes and contents; most overlap Theme A, so "shrink" here largely
means "will be replaced by the block-native driver," not independent
refactors.

| file | lines | mixes | note |
|---|---|---|---|
| `src/search/run.rs` | 2,927 | AbstractRun driver + streaming boundary + phased path + chunking/threads + band filter + row-key set | 86 interpreter refs; Phase-4 replaced |
| `src/bin/rewrite.rs` | 2,934 | campaign CLI: many subcommands over State | Phase-4 |
| `src/main.rs` | 2,837 | `-n` runner + 52 unit tests in one file | tests are the interpreter's; Phase-4 |
| `interpreter/vectorize.rs` | 2,858 | shape, columns, visited keys, subtract | runtime-speed machinery; Phase-4 |
| `src/trace/verify.rs` | 2,986 | frame verify + sweeps + PICO-8 checks | KEEPER; could split verify vs probe harness |
| `src/trace/kernel.rs` | 2,960 | key emit + room walk + spec-probe | KEEPER; carries C2 diagnostic |
| `interpreter/{abstraction,virtual_merge}.rs` | 1,404 + 1,395 | ladder rungs / merge+dedup | Phase-4 (ladder must be reimplemented on engine first) |

No independent "split this keeper" win is large; the size is concentrated
in the Phase-4 set.

---

## Theme E - stale artifacts / provenance

Mostly already handled by `plans/deletion.md` (28 MB `cfg_analysis.json`,
the whole rewrite campaign) and Phase 1 of delete-the-interpreter.md
(class/fused kernels, 8 witnesses, trace10/20 recipes). Residual, called
out in that plan as deliberately-left: four `tas/*.txt` headers and
comments in `rewrites-dying-*.jsonl` naming deleted recipes; ~20
`plans/*.md` describing deleted things. Low value, provenance only - leave
until Phase 4.

---

## Rough deletion plan (biggest safe wins first)

**Tier 1 - safe now, not gated on the interpreter:**

1. **Delete the asm prototype** `src/transpile/asm/` + `plans/asm-backend.md`
   - **~3,155 lines**, zero production references, measurement concluded.
   ONE decision from Philippe: is the backend a dead end? (Theme C1)
2. **Consolidate diagnostics into `src/bin/probe.rs`** and delete the
   `--spec-probe`/`CELESTE_SPEC_*` path + the five zero-assertion
   `#[test]`s - net negative lines, and CLAUDE.md already asks for it.
   (Themes C2, C3)
3. **De-dup the splitmix64 finalizer** - trivial, but only worth doing as
   part of Tier 3 since the copy lives in `row_table.rs`. (B2)

**Tier 2 - build the replacements (the gated prerequisite work):**

4. Frozen goldens, block-native driver, precision ladder on the engine,
   sweep over it, room (2,0)/(0,0) traced sets - per Phase 2/3 of
   delete-the-interpreter.md. No deletion yet; this is what unlocks Tier 3.

**Tier 3 - delete once the search keys on the engine key (the "half"):**

5. `crates/celeste-interp` (14,785) - includes the duplicate row-key
   (`virtual_merge::row_key_hashes`, `RowTable`), duplicate shape hash
   (`shape_hash_of_state`), and the splitmix copy. (A, B1, B2, B3)
6. `crates/celeste-ir` (2,725), `src/program` (966). (A)
7. `src/main.rs` runner (2,837), `src/bin/rewrite.rs` (2,934),
   `native-probe` (2,074), `concrete*` (317), `differential.rs` (938). (A)
8. Collapse `run.rs` to the single block-native path (streaming only),
   dropping the phased/State pair. (B5, D)

Tier 3 is ~24k+ hand-written lines and is the bulk of the owner's
"half"; it is entirely downstream of Tier 2 and must not be started
before the goldens + driver exist (CLAUDE.md: a deopt/coverage gap is not
a degraded mode).

## Method notes / limits

- Non-use was proven with `rg` across `crates src native-probe` excluding
  generated room dirs. The build is warning-free, so a `pub` item unused
  only *externally* would not warn; I confirmed the flagged items by
  callsite grep, not by the compiler. A truly exhaustive dead-`pub` census
  needs `cargo`/`warnalyzer` and was out of scope.
- No files were edited.
