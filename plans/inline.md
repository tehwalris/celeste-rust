# `inline`

Status: in the recipe, 183 call sites, converged over three rounds.

## What it does

`inline` splices a capture-free callee into one call site. It is sound by
construction rather than by analysis: it inserts `assert_closure %c is G`, so a
call that would ever have gone elsewhere fails loudly instead of silently
running the wrong code. The suggester (`rewrite suggest inline`) derives
global -> function structurally from the init chunk rather than by stripping
digits off mangled names.

## The guard immediately earned its keep

The first full run failed with:

    AssertClosure(tile_flag_at_72) failed: target is BuiltinFun("tile_flag_at")

`game_runner::inject_tile_flag_at_builtin` overwrites that global right after
init, swapping the interpreted tile lookup for the collision-cache one. So the
assumption "a global holding a function is never reassigned" - which the
previous attempt's call resolution made without checking - is false in this
program. `REPLACED_AT_RUNTIME` in `rules/inline.rs` now excludes it.

## It was parked for two months, and why it is not any more

Measured at frame 34 when it was first written:

    promote_cell only          5.47 s   1.12 GB
    + 183 inlines              6.45 s   1.21 GB

`LocalEnv` was a flat array **indexed by LocalId**, so its length was
max-LocalId + 1, and every branch filter cloned it. Inlining grows the id space
enormously - `player.update_21` from 848 to 3206 - so each filter inside
`player.update` went from copying ~850 slots to ~3200, and filtering is 33% of
runtime. Inlining was doing exactly what it should; the container it landed in
was the problem.

Slot allocation fixed the container. `LocalEnv` is now indexed by *slot*, and
the allocator packs the 3206 ids of an inlined `player.update_21` into 18 slots
- the same 12-18 it needed before inlining, because inlining does not change how
many values are live at once. Re-measured on the same harness:

    frame 34   without inlines   4.65 s   1.05 GB
               with inlines      4.47 s   1.12 GB
    frame 37   without inlines  13.20 s   3.63 GB
               with inlines     12.87 s   3.75 GB

## Why land it, given that is a wash

On its own, inlining buys ~3% of time and costs ~3% of memory. That is not the
reason to have it. The reason is that **if-conversion cannot cross a call**: a
branch containing a call cannot be flattened into a `select`, because the callee
has its own control flow and its own side effects. Every call left inside a
lane-varying branch is a fragmentation point that survives if-conversion.

So this is a prerequisite landing, not a win. 305 calls remain; whether they
also need to go depends on which of them sit inside lane-varying branches, which
is the next thing to measure.
