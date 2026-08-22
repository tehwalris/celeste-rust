# Build time: why it is bad, and what would fix it

Philippe, 2026-08-22: "our builds take way too long ... most of the time
we don't need most rebuilds." Measured that day, so the next person does
not have to re-derive it.

## What it costs today

| action | wall |
|---|---|
| edit `src/transpile/*`, `cargo build --release --bin transpile` | ~78 s |
| full `cargo build --release` (workspace) | ~110 s |
| `./regen-generated.sh` end to end (before parallelising) | ~5-6 min |
| `cargo nextest run --release --run-ignored all` | ~250 s |

## Why

**`celeste-rust` is a 40k-line monolith.**

```
src/interpreter/   16,196 lines
src/rewrite/       10,613
src/transpile/      6,243
src/bin/            5,024
src/compiled/       2,494
```

One crate, so editing the EMITTER recompiles the interpreter, the rewrite
machinery, the compiled dispatch and five binaries. There is no such thing
as a small change.

**`lto = "fat"` + `codegen-units = 1`.** Every build LTOs the whole
workspace with no codegen parallelism, on a 32-core box. This exists for
the SEARCH binary's runtime and is worth it there. It is worth nothing for
`transpile`, which just prints text, or for most of the test suite.

**The bootstrap makes it worse.** `celeste-rust` depends on
`celeste-kernels`, whose contents `transpile` (inside `celeste-rust`)
writes. So a change to the emitted code, or to an engine primitive's
signature, can leave the workspace unable to build the tool that would fix
it. `regen-generated.sh` works around the common case by generating into a
scratch dir; it cannot help when the breakage is upstream of generation
(2026-08-22: dropping `&mut dp` from the engine primitives needed the
committed kernels mechanically rewritten before `transpile` would build).

## What would fix it, cheapest first

1. **A profile for the edit loop.** `[profile.quick]` inheriting release
   with `lto = false, codegen-units = 16`, used by `regen-generated.sh`
   and the transpile edit loop. Different target subdir, so it does not
   invalidate release artifacts and the GATE still runs release.
   Unmeasured; expected to be the single biggest win for the smallest
   change.

2. **Split `celeste-rust`.** The dependency order that falls out of the
   existing module structure:

   ```
   celeste-ir         IR, Program, compile from Lua      deps: core
   celeste-interp     the reference interpreter          deps: core, ir
   celeste-rewrite    recipes, verify, differential      deps: core, ir, interp
   celeste-transpile  the emitters (graph/lower/kernel/  deps: core, ir,
                      names/fuse)                              rewrite, names
   celeste-run        compiled dispatch, bridge, bins    deps: everything
                                                               + kernels
   ```

   Note what this does to the bootstrap: `celeste-transpile` does NOT
   depend on `celeste-kernels`, so `cargo build -p celeste-transpile`
   works no matter how broken the generated code is. The cycle that
   forces the scratch-dir dance disappears rather than being managed.

3. **Per-package debuginfo.** The root already opts `celeste-engine` /
   `celeste-kernels` / `native-probe` out of `debug` for a measured 6%
   runtime reason. `debug = 1` on the 40k-line monolith is paid on every
   build and every link; worth measuring whether the profile in (1) can
   drop it.

## What NOT to do

Do not lower `lto`/`codegen-units` on the RELEASE profile to make the loop
faster. That profile is what the search runs and what every benchmark in
BENCHMARK_DATA.md was measured under; changing it silently reprices every
recorded number. Add a profile, do not edit this one.

# Code size: 81.5k hand-written lines, and where the guesses are wrong

Philippe, 2026-08-22: "we also just have way, way, way too much code ...
I'm not even really sure what parts our code has and what we actually use
and which features are effectively entirely dead."

Measured the same day. The instinct is right about the SIZE and wrong
about the CAUSE, at least where the bulk is.

## The size

**81,583 hand-written lines**, plus 14,748 checked-in generated.

| | lines |
|---|---|
| `src/rewrite/rules` | 27,513 |
| `src/interpreter` | 16,196 |
| `src/rewrite` (top level) | 10,613 |
| `src/` top level | 8,021 |
| `src/transpile` | 6,258 |
| `src/bin` | 5,024 |
| `crates/*` + `native-probe` | 5,438 |
| `src/compiled` | 2,494 |

## The "dead features" hypothesis does NOT hold in `rules/`

A third of the codebase is the rewrite rules, so that is where deletion
would pay. Every one of the ~37 rules is INVOKED by a checked-in recipe -
counted straight out of `rewrites*.jsonl`:

```
5166 inline      2285 if_convert    775 pin_builtin   494 speculate
3011 promote_cell 1493 demote_create 721 guard_branch 475 convert_assert
... down to ... 19 allocate_slots, 13 split_at, 5 strip_kills
```

Nothing there is dead. It is 27.5k lines for 37 rules - ~740 lines each -
of genuine CFG surgery. Any reduction has to come from SHARED MACHINERY
(37 hand-written transforms almost certainly re-implement the same block
splicing, phi fixing and dominance checking), which is a refactor, not a
delete. Do not go in expecting to find corpses.

## Where deletion probably IS available

Unverified - this is where to look, not what was found:

1. ~~**Instrumentation at `src/` top level.**~~ CHECKED, and the guess
   was wrong. `op_census` (671), `block_coverage` (248), `instr_time`
   (197), `merge_stats` (126), `metrics` (92), `branch_sites` (82),
   `create_sites` (72), `liveness` (39), `block_flow` (28),
   `instruction_flow` (4) - I expected several to be corpses. Every one
   has external references, and all but `metrics` are referenced from the
   SEARCH path (`main`/`game_runner`/`interpreter`/`compiled`), not just
   from tools:

   ```
   op_census   search:9  tools:2      liveness      search:2  tools:3
   merge_stats search:2  tools:1      instr_time    search:2  tools:1
   block_flow  search:2  tools:0      block_coverage search:1 tools:1
   metrics     search:0  tools:3      instruction_flow search:1 tools:0
   ```

   Being referenced is not the same as being NEEDED - a probe read only
   by another probe is still dead weight - but "delete the obviously
   unused instrumentation" is not the available win. It is ~1.5k lines
   anyway, 1.8% of the total. Several of these mutate process-global
   state, which is why the suite must run under nextest at all.
2. **The two monster CLIs.** `src/bin/rewrite.rs` (4,324) and
   `src/main.rs` (2,944). Multi-subcommand tools collect one-off
   subcommands used once during one campaign and never again.
3. **`src/interpreter` at 16k.** Unexamined. `vectorize.rs` (2,716),
   `virtual_merge.rs` (1,428), `core_interpreter.rs` (1,421), `op.rs`
   (1,393).

## How to answer it with data instead of reading

- Remove any remaining crate-wide `#![allow(dead_code)]` and read the
  warnings. (One of these previously hid ~150 lines of dead code and an
  entire abandoned dataflow framework - see CLAUDE.md.)
- `cargo +nightly udeps` for unused dependencies.
- Reachability from the REAL entry points: the search binary, `transpile`,
  and the test suite. Anything reachable only from a test that exists to
  test it is a candidate.
- The crate split above pairs with this: splitting forces every
  cross-module dependency to become explicit, and the ones nothing needs
  fall out on their own.
