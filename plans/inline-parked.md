# Why `inline` is implemented but not in the recipe

Status: the rule works and verifies; it is a measured performance *loss* with
the current `LocalEnv` representation. Parked, not abandoned.

## What was built

`inline` splices a capture-free callee into one call site. It is sound by
construction rather than by analysis: it inserts `assert_closure %c is G`, so a
call that would ever have gone elsewhere fails loudly instead of silently
running the wrong code. The suggester derives global -> function structurally
from the init chunk rather than by stripping digits off mangled names.

Applied to the whole program it converges at 183 inlines over three rounds, and
`rewrite verify --frames 30` passes.

## The guard immediately earned its keep

The first full run failed with:

    AssertClosure(tile_flag_at_72) failed: target is BuiltinFun("tile_flag_at")

`game_runner::inject_tile_flag_at_builtin` overwrites that global right after
init, swapping the interpreted tile lookup for the collision-cache one. So the
assumption "a global holding a function is never reassigned" - which the
previous attempt's call resolution made without checking - is false in this
program. `REPLACED_AT_RUNTIME` in `rules/inline.rs` now excludes it.

## Why it is parked

Measured at frame 34, same harness:

    promote_cell only          5.47 s   1.12 GB
    + 183 inlines              6.45 s   1.21 GB

`LocalEnv` is `Arc<Vec<Option<Value>>>` **indexed by LocalId**, so its length is
max-LocalId + 1, and `filter_vectors_in_place` does `Arc::make_mut` - a full
clone of that vector - on every branch filter. Inlining grows the id space:

    function              max LocalId before   after
    __frame                                9    1507
    _update_62                           171    1444
    player.update_21                     847    3205

So every state filter inside `player.update` went from copying ~850 slots (plus
~170 for the caller in `outer_local_envs`) to ~3200. At roughly 40 bytes per
`Option<Value>` that is ~128 KB per filter, and filtering is 33% of runtime.

Inlining is doing exactly what it should; the container it lands in is the
problem.

## What would unblock it

1. **A `renumber` rule.** Ids are not dense after inlining, so renumbering
   recovers maybe 1.6x of the ~3x regression. Necessary but not sufficient, and
   it has to be the last entry in a recipe segment because it invalidates every
   `%N` authored before it.
2. **Liveness pruning.** `src/liveness.rs` is still a no-op stub returning
   `None` ("keep everything"), and the hook is already wired through
   `flow_block_before_join`. The OCaml implementation does this. It would cut
   *occupancy* dramatically - most of an inlined function's locals are dead most
   of the time - but not the vector's length, since that is max-index.
3. **A `LocalEnv` whose clone cost is proportional to occupancy, not capacity.**
   This is the real fix, and it is close to what the `barrier` branch already
   did (Vec-based COW structures replacing the `im` ones).

(2) and (3) together are what make inlining pay. Until then the recipe stops
after `promote_cell`.

## Consequence for the plan

The staging in `plans/rewrite-plan.md` had inline (stage B) before if-conversion
(stage D) on the grounds that removing calls is a prerequisite. That is true for
the *endgame* but wrong as an ordering for *incremental wins*: if-conversion
reduces fragmentation directly, works inside a single function, and does not
grow the id space. It should come first.
