# Overnight 2026-08-16

Philippe's brief, in his words: finish the cleanup, get the recipe stable,
get rooms (0,0) and (1,0) to have **nice complete recipes** - including, for
room (0,0), actually JITting per shape instead of "the less optimised same
program for both shapes kind of thing that we were doing because
optimisation was hard". Then do room 3 = (2,0), again tailoring the recipe
to the specific shapes, with multiple shapes in the JIT, and sorting out the
widening issues. Then benchmark towards the end of the room, or with fake
endpoints, get a PROJECTION for how long the full run takes, and launch it.
If room 3 works out and he is still away, start room 4 = (3,0).

Token budget is explicitly not a constraint. Expect several compactions;
this file is the thing that survives them. **Keep it updated as work lands.**

## Order, and why

Fidelity first, because every fidelity fix invalidates every campaign, and
re-deriving a room costs hours. Do them ALL, then re-derive ONCE.

### A. Fidelity (blocks everything downstream)

- [x] A1 `foreach` -> PICO-8 `all()` semantics. Confirmed against a real
      console: five scenarios, index walk fails two, snapshot fails a
      DIFFERENT two, `all` matches all five. STAGED, not committed - waiting
      on the suite.
- [x] A2 PICO-8 installed at `~/bin/pico8` (0.2.7a6), `pico8 -x` headless.
- [x] A3 `pico8_diff/` harness (subagent): `lua_run` binary + 15 cases,
      exact hex on both sides. 8 pass / 8 xfail, the xfails being real bugs.
- [x] A4 **`sin` exactness.** DONE. Table at `cart/pico8_sin_table.bin`
      (16384 x i32), generator at `pico8_diff/gen/`. Reconstruction verified
      against all 65536 console values, zero mismatches. All three witnesses
      still replay frame-exactly, so the exact `sin` did not move the known
      TASes - but it can move a proof, which is the point.
- [x] A5 The other harness findings. All fixed, each pinned by a test
      carrying the console's own values:
      * literal parsing is now exact decimal -> 16.16, truncating, with the
        integer part wrapping. Checked that all 55 of the cart's distinct
        literals parse IDENTICALLY either way, so this moves nothing today.
      * division truncates toward zero and saturates to +/-0x7fff.ffff on
        overflow, including by zero (0/0 -> +max). It used to PANIC on a
        zero divisor, which is a soundness hole rather than a safe failure -
        the console keeps running, so the search would lose states.
      * `abs` saturates, negation WRAPS. Measured asymmetry, not a guess.
      * `%` is exactly `rem_euclid` on raw bits with `a % 0 == 0`; the old
        doc comment claimed Lua's sign rule, which is wrong.
      * `add` returns the appended value.
      Original list, for the record:
      * `from_f32` does `n as i16`, and Rust float->int casts SATURATE, so
        the literal `-32768` evaluates to -32767. Also text -> f32 -> 16.16
        loses precision (24 mantissa bits for a 31-bit format).
      * division by zero PANICS for us; the console saturates
        (`1/0 = 0x7fff.ffff`, `-1/0 = 0x8000.0001`). Ordinary division
        overflow saturates too - `32767/0.5` is `0x7fff.ffff`, we give
        `0xfffe.0000`.
      * `abs(-32768)` wraps negative for us, saturates on the console.
      * `add` returns its value on the console, ours returns nothing.
      * `checked_rem`'s doc comment is WRONG (says the result takes the
        divisor's sign; `7 % -3` is 1, and the result is never negative).
        The rule is exactly `i32::rem_euclid` on raw bits, `a % 0 == 0`,
        confirmed on 20/20 console data points.
- [ ] A6 Re-run the three witness TASes and the suite. Commit the lot.

### B. Recipe completeness

- [ ] B1 Re-derive the 4 entries the new `foreach` loop shape dropped
      (h050/h051/h052 `collapse_break_loop` + p1_127). Optimisation only -
      per-lane time went DOWN without them - but do not leave them lost.
- [ ] B2 Room (0,0) **per-shape recipes** - task #86, and the "actually JIT"
      item. Sized, 2026-08-16:

          base recipe (room (1,0)):        889 entries
          rewrites-room00.jsonl:           658 entries  ("shape-agnostic subset")
          dropped for room (0,0):          232          = 26% of the optimisation

      Dropped by family: h 65, unprefixed 42, m 38, p1_ 34, k 28, i1_ 18.
      Those are exactly the entries that need to know how many objects there
      are - loop collapses, object-loop unrolls, devirtualisation.

      Room (0,0)'s shapes are already enumerated in plans/room00-plan.md:
      S1=[fake_wall, player_spawn], S2=[fake_wall, player],
      S3=[player, fruit], S4=[player], with S2 at 63.3% of lanes and S3 at
      33.5% - so two variants cover ~97%.

      The machinery EXISTS and is unused by the ladder: `Variant` in
      verify.rs, `--variant 'shape|shape=RECIPE'` in bin/rewrite.rs. Its
      correctness gate is already the right one - boundary states must be
      identical with and without variants, so a variant cannot change the
      answer, only the speed. ladder.sh has no `--variant` wiring yet; that
      is part of this task.
- [ ] B3 Room (1,0) recipe complete on the same footing.
- [ ] B4 NOTE: `rewrites-room00.jsonl` is a SECOND recipe file and needs the
      same `foreach` re-derivation as the base one. Check it replays.

### C. Room 3 = (2,0)

- [ ] C1 Shapes census for (2,0): fruit + 2 springs. What object-array
      shapes actually occur, and for how many lanes each.
- [ ] C2 Per-shape recipes.
- [ ] C3 Widening: the band-filter soundness issue (verify.rs `None =>` arm,
      k=1 misses must be fatal), and whatever the fruit's `off` widening
      needs now that `sin` is exact.
- [ ] C4 Benchmark with a FAKE endpoint (a synthetic win short of the real
      exit, `CELESTE_WIN_AT_XY`) to get a per-frame cost curve.
- [ ] C5 Project the full run time from that curve, write it down, launch.

### D. Room 4 = (3,0), only if C lands cleanly

`fall_floor` introduces per-object state machines - see the earlier
discussion. Expect the state to need an abstraction, not just more lanes.

## Standing rules for the night

* Commit early and often; every commit must have the gate green
  (`rewrite isocheck`, `cargo test --release`, the three witness TASes).
* `./safe-run.sh` for anything long. Exit 137 is OOM.
* **Do not benchmark while a test suite is running.** I did that once
  tonight and had to throw the numbers away.
* Every fidelity claim gets checked against the console, not against lore.
  Lore is what produced the wrong `foreach`.

## Log

* 02:00 label densification + name-keyed slots committed (e57b837).
* 02:15 PICO-8 installed; `foreach` = `all()` confirmed on five cases.
* 02:40 `sin` table dumped, folded 65536 -> 16384, reconstruction exact.
