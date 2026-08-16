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
- [x] A6 DONE. Committed as 5d4a5d3, plus the Lua-level `add` fix.
      `pico8_diff`: **16 passed, 0 failed, 0 xfail** - the interpreter now
      agrees with a real console on every case in the harness.

      The `add` fix is the clearest evidence the stability work paid for
      itself: `add` is INLINED at many sites, and changing its body cost
      **zero** recipe entries. Before tonight that class of edit cost 371.

### B. Recipe completeness

- [x] B1 PARKED, with a measurement rather than a guess. The 3 dropped
      entries are `collapse_break_loop`; the new loop breaks on
      `tbl[i] == nil` instead of `#tbl < i`, so the rule's shape check
      refuses. Rather than extend the rule speculatively I priced it: the
      ONE `collapse_break_loop` entry that still applies was removed and
      re-benchmarked on an idle machine, 3 runs each.

          with it:     0.90s / 0.90s / 0.90s   132153 lanes   6.8 us/lane
          without it:  0.90s / 0.90s / 0.89s   132153 lanes   6.8 us/lane

      Identical, to three digits and to the lane. It does change the program
      (1754 -> 1764 blocks), so it applies - it just buys nothing at 34
      frames on room (1,0). Extending the rule for the other three is
      therefore not justified by anything measured yet. REVISIT for room
      (2,0), which has more objects and where object-loop collapsing has
      more to bite on; that is the case this measurement does NOT cover.
      p1_127 is separate and not merely owed: `i`/`prev` are genuinely
      multi-store loop-carried cells, which mem2reg-lite cannot promote.
- [x] B2 Room (0,0) **per-shape recipes** - task #86, and the "actually JIT"
      item. DONE and **speed-neutral**; the control experiment is the
      result. Full numbers at the top of BENCHMARK_DATA.md. Sized,
      2026-08-16:

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

      **S2 landed. `rewrites-room00-s2.jsonl`, 738 entries** = the 657 of
      `rewrites-room00.jsonl`, plus 13 `fake_wall.update_37` entries that
      room (1,0) never executes and so never screened, plus 68 of the 102
      shape candidates. Registered with

          --variant 'fake_wall,player=rewrites-room00-s2.jsonl'

      Three things had to happen, in this order:

      1. **Registering a variant used to switch the chunk-parallel path
         off** (`step_inner` refused it because dispatch owned `&mut self`).
         Measured on room (0,0) f40: 2.13 s without a variant, 7.59 s with a
         no-op one - a 3.6x handicap no variant could ever win back. The
         dispatch is a pure function of the state, so it moved into
         `interpret_state_base` behind a shared borrow and now runs on the
         workers like everything else. Same frame at 2.13 s again.
      2. **A variant could not be screened at all**, because `screen` and
         `verify` run the recipe as the WHOLE program from frame 1, and
         room (0,0) spends its first 27 frames in S1 - where an S2 recipe's
         devirtualisation premise is false. `--variant-of HOST
         --variant-shapes SHAPES` hosts the trial instead, and compares
         against the HOST's own trace, which is exactly the claim a variant
         makes. Two extra rejections that no observation check would catch:
         a nonzero fallback count, and a variant that never dispatched.
      3. Only then the derivation. 102 shape candidates screened, 68 kept.

      **The result: 0.0%.** Room (0,0) f048, 3 runs each on an idle
      machine, 4,614,581 lanes both ways, zero fallbacks, every lane from
      f028 on dispatched:

          without:  22.09 / 22.00 / 21.92 s   8.18 GB   4.8 us/lane
          with:     22.11 / 21.93 / 22.02 s   8.20 GB   4.8 us/lane

      `fwd.interpret` 7.56 s vs 7.59 s - the specialised program is
      exactly as fast as the shape-agnostic one.

      What did NOT survive, and it is the whole story: **all 32
      `collapse_loop`/`assume_eq` entries**, plus two `unroll_loop`s.
      `collapse_loop` collapses by asserting `bound == init`, a trip count
      of ONE; room (0,0) has TWO objects and every one of those guards
      fires at frame 29.

      The control makes the size of that exact: on room (1,0), which can
      run both recipes, the whole 232-entry gap is **0.23 s / 11.3% at
      f037, not 26%** - and **0.19 s of it (83%) is those 34 entries.** The
      other 198, which include everything the S2 variant did land, are
      worth 0.04 s between them.

      And a two-trip collapse rule would NOT recover it. Room (1,0)'s prize
      was never the collapse (its own commit measured it "small on its
      own") but the `assume_eq` that folds every object-table check to nil,
      and that fold is FALSE in room (0,0) - the second object is a real
      `fake_wall` that `obj.is_solid` collides with on every pixel step.
      Recovering it needs room00-plan.md step 3 in full: unroll the OUTER
      `foreach(objects, ...)` to two copies, inline `anonymous_61` per
      slot, and collapse + `assume_eq` per slot, where each iteration's
      object identity IS static. That is `peel` plus a per-slot inline -
      new rule work, days, and the measurement above is what should decide
      whether 11% of a third of the clock is worth it.

      NOT DONE, and worth knowing before wiring the ladder: **the backward
      sweep cannot use variants.** `sweep_time` builds its own
      `AbstractRun::start_with_deopt` and never calls `set_variants`, so the
      replay - which is ~4x the forward cost on the same frame - still runs
      the shape-agnostic program.

      A refinement to the VARIANTS note now in ladder.sh: "dispatch is
      invisible" holds for the row SETS and not for the row IDS. f040 under
      campaign settings gives an identical fingerprint, row_count,
      watermarks and lane count either way, and a `states.bin` (319,986 vs
      316,684 bytes) and `visited.bin` that both differ, because a variant
      frame emits its raw lanes in a different order and ids are assigned
      in insertion order. Checkpoints stay interchangeable - everything
      downstream reads ids out of the tree it was handed, so resuming a
      variant run from a variant-free tree is fine, as the note says. What
      does follow is narrower: artifacts are not BYTE-comparable across the
      setting (a `parcheck.sh`-style gate has to hold it fixed), and a
      `g.bin` only means anything against the row table it came from. Both
      now in the flag's doc comment.

      Given all of the above, **there is no reason to turn VARIANTS on for
      room (0,0) yet**: 0.0% at f048, and `pos-graph`/`sweep` - the largest
      stages - cannot dispatch at all.
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
* 03:30 numerics batch committed (5d4a5d3). Also found and fixed: room00's
  recipe had been silently broken since e57b837 because only the base
  recipe was migrated to stable names, and NO test replayed the second
  recipe. `every_checked_in_recipe_replays` now does.
* 04:00 Lua-level `add` returns its value; 0 recipe entries broken;
  harness fully green at 16/16.
