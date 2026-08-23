# What can actually be deleted (2026-08-23)

`plans/tracing.md` "What is needed before deleting `celeste-rewrite`"
lists five blockers and concludes not much can go yet. Re-checked from
scratch today. **Two of the five are wrong, and about 35k lines are
reachable** - most of it behind ONE move.

Everything below is from a dependency sweep with file:line evidence; the
numbers are `wc -l`.

## The sizes

| | lines |
|---|---|
| `src/rewrite/rules/` + `rules.rs` + `slots.rs` | **28,216** |
| `src/bin/rewrite.rs` (the campaign CLI) | 4,324 |
| celeste-interp instrumentation/census group | ~5,070 |
| `src/rewrite/` service modules outside the core closure | 2,049 |
| celeste-interp core interpreter | 5,970 |
| celeste-interp set machinery (vectorize/abstraction/visited) | 6,600 |
| celeste-interp `game_runner.rs` (setup, builtins, `start_room`) | 922 |

## Blocker 1, `gen.rs`: WRONG, it is not a blocker

The claim was that `FIELD_NAMES`/`GLOBAL_NAMES`/`FN_NAMES` are generated
from the rewritten program and so pin the rewrite machinery.

What is actually true:

- `crates/celeste-names/src/gen.rs` is 236 lines of `pub static &[&str]`
  plus two `position()` helpers. It is CHECKED IN and has **no
  build.rs** - deliberately, `celeste-names/src/lib.rs:3-9` says a build
  script there would be a bootstrap cycle.
- **Nothing at build time or run time invokes the generator.** The only
  things tying `gen.rs` to `src/rewrite/` are `emit_names` in
  `src/transpile/names.rs:307` and two `#[ignore]`d gate tests
  (`names.rs:404`, `:456`).
- **No rewrite rule invents a name.** Every non-builtin entry in both
  tables is a literal identifier in `lua/*.lua`; the only string-literal
  `field:` values anywhere in `src/rewrite/rules/` are `x`, `y`, `rem`,
  `type`, `update`, all already in the source. The rewrites perturb
  which sites survive and in what ORDER the walk meets them - an
  incidental dependency, not an essential one.
- `FN_NAMES` has no rewrite dependency at all: it is `program.functions`
  insertion order, and every `functions.insert(` in `rules/` is inside a
  `#[cfg(test)]` block.

Deleting the generator leaves `gen.rs` compiling and every reader
working. The cost is losing the byte-identity gate - if `lua/*.lua` or
`rewrites-compile.jsonl` ever change, nothing notices the tables went
stale. Given the campaign is over, that is a cost worth paying, but say
so out loud in the file rather than letting it be discovered.

Two things die with the generator that are NOT about names and should be
kept or re-homed: `names.rs:101-116` also CHECKS the program (unknown
closure target, builtin outside the ABI, `CallBuiltin` arity with no
lowering), and `:343` asserts `__init`/`__frame` exist.

**Order really is load-bearing** - `runtime2.rs:567` `shape_hash_of`
hashes field IDs, `bind.rs:130` builds globals by iterating
`GLOBAL_NAMES` in order, and `kernel_gen_*.rs:11` bakes `SHAPE_HASH`.
So: freeze, never reorder. That is an argument FOR freezing, not against.

## Blocker 4, "the oracle is the vectorized interpreter": RIGHT, and worse

Accurate and understated.

- There is **no separate scalar interpreter to fall back on.**
  `src/bin/concrete_run.rs` -> `src/concrete.rs:15` -> the same
  `glue::interpret_prepared_cfg` that `AbstractRun` uses, same
  `core_interpreter.rs`, same `MaybeVector` value domain, and it still
  reaches `vectorize_states` at `glue.rs:202` on every `hint_normalize`
  block. `src/concrete.rs:4-8` says this is deliberate.
- The room test compares row-key SETS per frame. A scalar interpreter
  produces traces, not sets, so replacing the oracle needs a
  set-of-states semantics - `abstraction.rs` + `row_table.rs` +
  `visited.rs`, i.e. most of the "vectorized" group again.
- The interpreter is not only an oracle: `src/compiled/mod.rs:329,695`
  puts it on `FrameEngine`'s deopt/fallback path, which ships.

So the ~12.5k of interpreter core + set machinery stays for now. Note
the tension with CLAUDE.md's "never deopt to the interpreter": the
fallback exists to cover shapes with no kernel, and closing that
coverage gap is what eventually frees it.

## The one move that unlocks the 28k

The rules are reachable exactly two ways:

1. `recipe::apply_entry` dispatch, i.e. REPLAYING `rewrites.jsonl` to
   rebuild the rewritten `Program` (`recipe.rs:859-1104`);
2. the 18 `candidates()` / 1 `blockers()` entry points, called only from
   `src/bin/rewrite.rs:1541-2580` - the `suggest`/`screen` tooling that
   FINDS rewrites.

(2) dies when the campaign is over. (1) dies if we stop rebuilding the
program from the recipe - **freeze the rewritten `Program` as a
checked-in artifact, exactly the way `gen.rs` is frozen.**
`rewrites-compile.jsonl` is 1084 entries over 35 rules, so there is no
subsetting: it is all of them or none.

Prerequisite: the IR is not serializable today. `crates/celeste-ir/src/
ir.rs` derives `Serialize`/`Deserialize` on the small id types (`:12`,
`:23`, `:29`, `:80`) but NOT on `Instruction` (`:139`), `Block`
(`:633`), `Cfg` (`:760`) or `FunDef` (`:932`). Adding them is
mechanical.

Determinism is already established: function order is an insertion-
ordered `IndexMap`, block order is `blocks_in_order` (sorted by label,
explicitly to dodge hash iteration), the interner is insertion-ordered,
no cart, no clock, no RNG - and `generated_is_current` has been passing
byte-for-byte as a gate.

## Why the tracer does not care

The tracer parses Lua directly with `full_moon` and interprets the AST
(`src/trace/interp.rs:20`, `src/trace/cart.rs:160`). It never touches
`Program`, the IR, or a rule. Its only use of celeste-interp is
`game_runner::start_room` / `apply_start_room`. So the rewrite machinery
is on a wholly separate path from the thing replacing it - which is why
this is a deletion problem and not a migration problem.

## Order of work

1. **Free today, no prerequisites.** Four rules appear in NO checked-in
   recipe and have no `candidates()`: `add_hint` (214), `remove_hint`
   (103), `widen_buttons` (265), `widen_rem` (271) = **853 lines**.
   Reachable only if someone hand-writes such a recipe entry.
2. **Freeze the program.** serde on the IR, serialize once, check it in,
   point `recipe::build`'s callers at the artifact. Then delete
   `rules/` + `rules.rs` + `slots.rs`: **28,216 lines**.
3. **Retire the campaign CLI.** `src/bin/rewrite.rs` is 4,324 lines and
   mixes rewrite-FINDING subcommands (`suggest`, `screen`, `bisect`,
   `derive-overlay`, `migrate-names`, `check`, `print`, `diff`,
   `classdead`, `slots`, `isocheck`, `labels`) with search tooling
   (`verify`, `sweep`, `bench`, `shape-census`, `extract-tas`,
   `count-optimal`, `pos-graph`). Split it; the first half goes.
4. **Then the instrumentation follows.** ~5,070 lines in celeste-interp
   whose only consumers are those CLI subcommands, plus `class_dead.rs`
   (435), `isocheck.rs` (587), `liveness.rs` (206), `sweep_time.rs`
   (821) in `src/rewrite/`.
5. **Blocked until coverage.** Interpreter core (5,970) and set
   machinery (6,600): oracle + `FrameEngine` fallback.

Reachable total: **~35,000 lines**, versus ~2,900 without step 2.

## What to be careful about

- **Do not reorder anything in `gen.rs`.** A reorder moves the shape
  hash and every row key, and invalidates
  `crates/celeste-kernels/witness/*` and the checkpoints.
- **Freezing means the Lua is frozen too.** If `lua/*.lua` changes we
  would need the rules back to rebuild. They stay in git history; say so
  in the frozen artifact's header.
- **Keep the program CHECKS that live in the names generator** rather
  than losing them with it.

## Progress (2026-08-23)

**Step 1 done: the four unused rules are gone.** `add_hint`,
`remove_hint`, `widen_buttons`, `widen_rem` - 933 lines with their
`Rule` variants and dispatch. Note `__widen_rem` the BUILTIN stays; it
is in `gen.rs` and `builtins.rs` and has nothing to do with the rule.

**Step 2 in progress: the freeze works.** `src/rewrite/frozen.rs` +
`src/bin/freeze.rs`. Both live recipes build, freeze, and read back
equal to what the rules produced (the tool refuses to report success
otherwise):

    rewrites.jsonl          -> rewrites.program.zst          77 fns, 293 KiB
    rewrites-compile.jsonl  -> rewrites-compile.program.zst  77 fns, 284 KiB

Prerequisite done: serde derives on `Label`, `UnaryOp`, `BinaryOp`,
`Instruction`, `Terminator`, `Block`, `SlotMap`, `Names`, `Cfg`,
`FunDef`. The artifact stores functions as an ordered `Vec` of pairs,
not a map - `Program::functions` insertion order is what `FN_NAMES` and
every deterministic print depend on, and a `HashMap` round-trip does not
promise to give it back.

**Only the two live recipes get frozen** (Philippe, 2026-08-23). The
other 18 `rewrites*.jsonl` exist only to regenerate kernels that are
themselves already checked in, so they are covered by the same argument
that covers `gen.rs`. Consequences to be explicit about:

- `regen-generated.sh` stops working once the rules are gone.
  `crates/celeste-kernels/src/kernel_gen_*.rs` and `gen.rs` become
  frozen artifacts in the same sense the program now is. The tracer is
  replacing the walk kernels anyway.
- Those 18 recipes become inert data. Left in place for now; deleting
  checked-in data is a separate decision.

**A gate that survives, unexpectedly.** I expected freezing to cost us
`generated_is_current`. It does not: that test regenerates `gen.rs` from
the program and compares byte-for-byte, so pointing it at the frozen
program keeps it meaningful - it becomes "`gen.rs` is consistent with
the frozen program" instead of "with the rules' output". What we DO lose
is `every_checked_in_recipe_replays`, which is the rules' own test and
goes with them.

## Final tally (2026-08-23)

| step | lines |
|---|---|
| 4 unused rules | 933 |
| `rules/` + validate + isocheck + slots + class_dead + liveness + recipe's replay half + 14 CLI subcommands | 31,676 |
| campaign instrumentation (field_census, block_coverage, would_dedup, merge_stats, branch_sites, create_sites, branch_trace, measure_k) | 1,871 |
| the state-flow profiler + `--profile` | 1,523 |
| Chrome tracing spans + the op census + `--trace` | 1,692 |
| **total** | **~37,700** |

Hand-written Rust after: **58,180** lines (plus 18,959 generated).
`src/rewrite` went 37,894 -> 6,989, and only ~1,000 of what remains is
about rewrites at all: the rest is `verify.rs` (the abstract search
driver, 3,178), checkpoints, sweeps and position graphs. **That module
should be renamed** - the search does not belong under `rewrite/`.

The tracer (11,096) is now the largest thing in the repo, which is the
right shape: it is what replaces all of the above.

### What the checks were

- Every deletion: the full suite, warning-free build.
- The rules: `every_checked_in_recipe_replays` over all 20 recipes
  before deleting them; `generated_is_current{,_r20}` after, which
  regenerates `gen.rs` and all 8 kernels from the frozen program and
  compares byte-for-byte.
- The interpreter edits (213 op-census sites, 37 span sites, the
  profiler's DAG-id threading): the ROOM TEST. The kernels are
  untouched, so an interpreter that still yields identical row-key sets
  for 30 frames is an interpreter whose behaviour did not change. That
  is a stronger check than the unit tests for this kind of edit.

### Still standing, and why

- **Interpreter core + set machinery, ~12.5k.** Oracle for the room
  test, and `FrameEngine`'s fallback for shapes with no kernel. Cutting
  it is not a cleanup - room (0,0) would stop working rather than run
  slowly, and the kernels would lose their independent check. Needs
  kernel coverage first.
- **`merge_dump::cell_names`** - `compiled::dispatch` uses it.
- **`interpreter::inspect`** - houses the search's checkpoint save/load.
- **The 18 unreferenced recipes** and the 8 checked-in kernels they
  built: inert data now. `regen-generated.sh` no longer works, which is
  fine while the tracer replaces the walk kernels, but say it out loud
  rather than letting someone discover it.
