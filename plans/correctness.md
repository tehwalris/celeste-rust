# Correctness to-dos (2026-10-20; from the second audit, /var/tmp/audit2/)

Open soundness and verification questions. Philippe wants to handle these
hands-on: refactors must NOT change the code paths below until each item is
settled here. Evidence is file:line at develop 19869c5d.

## C1. Pruning is not first-class; the known-route check misses cuts
- `search::known` (known.rs:320-392) lists the pruning steps by hand:
  level -1 `too_late`, the ladder's `allowed`, the hull claim, W's support,
  W at the exact remainder, the reach marks.
- `frame::not_expanded` (frame.rs:327-347, applied at wave.rs:333) also cuts:
  exits, `berry_lost`, `orb_deadline_skip`, out-of-room under `--win-at`. None
  is a named step: a route a wrong cut drops shows only as "NOT IN THE TREE"
  (known.rs:366-374), unattributed.
- Proposal (audit architecture.md M1): one `Prune` interface (filters and
  cuts, each with its projection, its tree-record line and its horizon);
  `check known` iterates it, so it is complete by construction.

- Unblocks native level -1 (assessment, 2026-10-20): feasible but a design
  decision: per-shape inductive ranges carried by the kernels, a loud
  widen-and-retrace path for declined lanes, and a position-only level whose
  backward computes distances; about -1,000 lines net; every level -1 result
  re-verified. The known-route check must cover it first (this item).

## C2. The orb deadline: a hand-derived bound nothing checks
- `ORB_MIN_FRAMES_AFTER_CHEST = 70` (frame.rs:284-286): "61 paused + 8 until
  the orb is collectable + 10 frozen = 79, taken as 70". No test derives or
  checks it against the cart.
- Its ceiling is level -1's H (`TreeL1::orb_ceiling`, frame.rs): without
  level -1 the orb room never gets the cut (sound, but arbitrary coupling).
- To do: check the bound with the reference engine (or derive it), and give
  the cut its own horizon (the search's).
- It also mixes units: the cut compares steps with frames (frame.rs:160-163);
  sound today only because the orb room is not split-frame.

## C3. Room (2,1): the split frame reaches FEWER states than unsplit at `p`
- Noted in architecture.md ("unexplained"). Fewer states is the direction a
  soundness bug shows. Find a state reached unsplit but not split and explain
  it (a legitimate projection difference, or a bug). Blocks any further split-
  frame refactor (audit M6).

## C4. The minimal cart's move guard: UNREACHABLE (proven); pick a form
- Original: `obj.move(obj.spd.x,obj.spd.y)` unconditionally (celeste.lua:1195);
  minimal cart, split-frame.lua and trace/cart.rs:264 guard it on nonzero
  speed. `move_x(amount,0)` AND `move_y` start their loops at 0 (orig 1042),
  so `move(0,0)` inside a solid zeroes spd and rem on both axes.
- Origin: celeste_ocaml 07ba38a (2022-02-04), no stated reason; likely the
  native `tile_flag_at` refusing non-integer coordinates (66 routes error
  unguarded). Copied here in b176c4fe.
- Proven unreachable on the game's maps: only the player can have speed with
  `solids`, and after any executed move a player inside a solid has rem and
  spd 0; the only position changes between moves (spring, platform carry,
  draw clamp, a fall floor reappearing) cannot embed it with speed (0,0) and
  rem != 0. Empirically 0 divergence: 159 known routes (17,309 per-frame state
  hashes identical), the original cart instrumented on PICO-8, and 1.56M
  exploration steps in 16 rooms. The loading frame is identical too (rem 0).
- To decide: (1) document the proof plus a loud check outside the cart, or
  (2) unguard the player only (identical hashes on all routes; gates must hold).
- Evidence: /var/tmp/c4-quick/cart-differences.md; probe worktree
  /var/tmp/c4-wt (branch c4-probe, uncommitted), /var/tmp/c4-probe-scripts/.
- Still to do: a standing DIFFERENTIAL CHECK running the original cart's Lua
  and the minimal cart side by side (every room, many inputs, state per frame
  modulo the handled differences), so the minimal cart and the native
  builtins are checked, not trusted.

## C5. Verification gaps at the object and platform levels
- At `p` the reference engine refuses to run (engines/reference.rs:539): the
  platform kernels are checked only against regression pins.
- At `n`, `check ref` and `check arcs` skip most rows over the reference's
  path cap; room (7,0) `check arcs` is an open issue.
- Proposals: a reachable-sample soundness check at every level (audit
  architecture.md M3: concrete states from playouts, the concrete search and
  known routes, stepped exactly, projected, held against the tree's edges and
  hulls); concrete members in place of `step_at`'s pins (audit
  kernel-pipeline.md K10).

## C6. Closure captures are not part of a state's exact identity
- `shape_sig`/`shape_hash_of` hash a closure's function id and capture COUNT
  (exact.rs:435, runtime2.rs:391-395); `exact_rows` reads only `Val` cells
  (exact.rs:964-1000): capture targets are hashed nowhere.
- Fixed by structure today (the converter refuses non-table captures; "every
  capture in this cart is the owning object", trace/block.rs:198-205), and the
  forward's `Skeletons::register` would catch a mismatch, but the concrete and
  dual searches dedup on `ExactRow` without that check.
- To do: assert in `trace::block` that a capture IS the closure's owner.

## C7. HIGH: the jump buffer is removed, so the search MISSES real states
- The original buffers a jump press for 4 updates (orig 136-142, 211-212);
  the minimal cart jumps only on a fresh press (min 109-110, 169-170). Its
  argument "a buffered jump executes identically to an exactly-timed press"
  (min 102-108) is incomplete: the original can jump on two CONSECUTIVE
  updates (a buffered jump, then a fresh press), the minimal cart cannot.
- PROVEN on PICO-8: room (1,0), inputs /var/tmp/c4-probe-scripts/jbuf-inputs.txt:
  the original is at (49,109) spd (-2,-2) with djump 1 at f42; the minimal
  cart's exhaustive level-0 tree has that position and speed by f42 only with
  djump 0. min is a subset of the original, so this only MISSES routes.
- Open: does any room's optimum need a buffered-then-fresh pair (e.g. a
  landing jump then an immediate wall kick)? Every result ties the community
  TAS (made WITH the buffer): weak evidence, not proof.
- Fix: restore `jbuffer` in the minimal cart (one 0..4 field) and re-run
  every room (results can only stay or improve).

- Scope, refined (2026-10-21): the `h` level widens `p_jump`/`p_dash` (read
  both ways), so at `h` a jump at u needs only the input at u: the tree, W
  and the ARC BOUND include every buffered-jump behaviour, and the lower
  bound is sound for the real game. The CONCRETE search runs the cart exactly
  (no buffer) and so rejects exactly those behaviours. Hence:
  - final level has `h` and its arc bound == the found optimum: proven for
    the real game (bound covers the buffer; the witness is real);
  - final level has `h`, bound < found: NOT proven (the gap may be a
    buffered-pair route the concrete search rejected), e.g. (6,1) 90 vs 92;
  - proved without `h` (`r0sx`): NOT proven, e.g. any% (1,0) 99, the
    Summit 57, (6,0) (ladder ends at `r0sx`; `r0sxh` bound 68 vs 73), (1,2).
- Removing button state from the cart is SAFE for the abstract levels (an
  over-approximation; `h` already does it) but UNSAFE for the concrete
  search, which must be the real game exactly: missing behaviour gives false
  optima, extra behaviour (stateless buttons: holding jump retriggers, three
  jumps on consecutive updates) gives fake wins.
- Proposed fix: the cart gets the original's button semantics exactly
  (`p_jump`, `p_dash`, the 4-update `jbuffer`); every abstract level widens
  button memory (`h` always, extended to `jbuffer`); the concrete search runs
  it exactly. Then re-prove the at-risk rooms (results only stay or improve);
  `canonical_jumps` re-timing becomes unnecessary (TAS files play directly).
- PROTOTYPED (2026-10-22, branch `cart-fidelity` ecebea1d, NOT merged;
  report /var/tmp/cart-fidelity/report.md): the original's `jbuffer` exactly,
  "held buttons" at every level plus `jbuffer` unknown. Abstract sets at every
  level = today's `h` (all fixtures' counts identical); the buffered example
  reproduced on PICO-8 and kept by `--prefer`; 29/30 community any% TAS
  replay raw (develop's engine never exits on 14). COST: release +9-11% per
  frame (the jump button's two classes emit the same successors twice; the
  unchecked fix of reading jump as released at abstract levels LOSES states,
  so any fix must be checked per kernel); concrete BFS x3.7 steps; `check
  ref` 5 -> 19 GB. Re-proven under it: any% (0,0) 93, (1,0) 99, (4,0) 76,
  summit 55, gemskip summit 57, (5,3) 79. Still at risk: nodiag (6,0) 73
  (also C13), nodiag (6,1) 92, nodiag (1,2) 113, 100% (6,1) 114, 100% (4,3)
  109. Decision pending (Philippe).

## C8. Death and exit in the same update counts as a win
- `kill_player` (orig 121-122) then `next_room()` (orig 305) in one update:
  the search's win test is the room alone (frame.rs:189-212), so a dying exit
  is a win (FAKE by the category rules; `rewrite pico8 chain` reports a death), and the
  next room's pending-reload start is never searched (MISSES).
- Rooms with side spikes in row 0 next to the exit: (2,2), (6,2), (0,3),
  (2,3), (3,3). Not reached by any actual route yet.

## C9. The category runner falls back to the IL start
- `tools::chain::entry_jank` (used by `rewrite category`): when the previous
  room's database file breaks the boot chain, only the IL start is searched,
  which may not be a real start (MISSES). `tools::uploads::boot` restarts the
  chain its own way; the two stay apart until this is settled.

## C10. Smaller items
- `rnd` is a free interval per call (stated best-case caveat); the original
  also consumes the stream in removed code (clouds, particles, smoke's
  `maybe()`, wall-slide `rnd(10)`, camera shake).
- `rewrite starts` bounds the climb at 6 px a frame (safe today by margin).
- Latent builtin issues: native `tile_flag_at` reads the boot room's
  collision cache, not the live room; number literals parsed by decimal
  truncation (all 15 fractional cart literals match bit for bit today).
- A frame is always `_update();_draw()` (PICO-8 skips draws over the CPU
  budget): suspected safe.

## C11. Balloon seeds: PICO-8 verification hands them out in another order
- The PICO-8 replay (`rewrite pico8 replay`, `tools::pico8::FENCED`:
  `SeedOrder::Pop`; `--seed-order creation` is the alternative, implemented)
  assigns a file's `[seeds]` in POP order (balloons at the load, chests when
  opened); Rust's `--balloon-seeds`
  (`RoomObject::seeded`) and UCT use CREATION order. Where a chest precedes a
  balloon in creation order (rooms (3,3), (5,3)), the search's concrete
  balloons and the PICO-8 verification's get different seeds: 3 of 45
  chest-variant replays differ (found by agent F, 2026-10-20).
- To do: settle which order the tasdatabase's tools (Celia, UCT) define, make
  the replay match, and re-verify the (3,3) and (5,3) uploads.

## C12. Tooling degraded silently (FIXED, landed 4633008e)
- Since f3123072 (2026-10-15) replay.py (now `rewrite pico8`, whose t0 test
  checks every tracer hook has a stub) lacked the minimal cart's
  `__split_by_round` hook: every minimal-cart PICO-8 replay crashed, and the
  category runner's `canon_jumps` silently dropped every jump press
  ("original cart exit None -> None"). Re-check any runner result since then.
- The runner also passed `--room` to export-ui, which refuses it, so its UI
  step failed on every improvement.

## C13. HIGH, CONFIRMED: the platform-world table misses real positions
- `platform_worlds(frames, s)` (concrete.rs:99) records the world over
  `0..=frames` `step_one`s of a NO-INPUT reference run; its one call site
  (walk.rs:490) passes 127. Under `--split-frame` a step is half a frame, so
  the table covers 64 GAME frames, while the guard (wave.rs:743-745) admits
  game frames up to 127. A `p` answer whose (world, pixel) set is empty is
  dropped (split.rs:126 `sides()`), so real successors can be lost.
- MEASURED (probe /var/tmp/c13-probe.diff, logs /var/tmp/c13-probe*.log):
  room (6,0) split: table 65 worlds, uncovered from game frame 65 on (63
  frames; 41 x values per platform missing). (1,2) split: same, but it was
  searched unsplit (complete to 127).
- SECOND GAP, not split-related: in room (2,1) the idle player dies (f49) and
  the room restarts (f64), so the no-input run only ever holds platform
  counts 0-63 (x 76-116); a living player meets x < 76 from ~f64. The
  docstring's claim ("a no-input run visits every arrangement") is false
  whenever the idle player dies; the guard compares a constant, not coverage.
- Since 8ff30b84 (09-29) for the split part.
- No witness is affected (each stays 25+ px from every platform in uncovered
  frames), but these REFUTATIONS are unproven: room (6,0) any% 70 (horizons
  65-69), (6,0) nodiag 73 (`--to 69`, bounds 68/73); (6,0) 100% 75 (open)
  must not run until fixed. (1,2) and (6,1) are unaffected.
- (2,1) RE-PROVEN without `p` (2026-10-21, 35b93f4e, /var/tmp/clock-measure):
  `search --level r0sxhn` alone, the platform exact, no table, no level -1.
  any% 82: forward 22 min, 8.9 GB, 35.5M states at f82 (641M visited, 63 key
  bits; new states per frame +16% at f56 falling to +2% at f82); arc bound
  82 in 2 s; concrete win at 82 in 0.2 s. nodiag 85: forward 10.5 min,
  6.3 GB; arc bound 85; concrete win at 85. Both witnesses equal the stored
  routes; `--prefer` finds no pruning step that drops either. An exact
  platform is a clock (no merge across frames), affordable here.
- (6,0) exact: FATAL at step 9, 128 key bits (10 platforms, each field its
  own dictionary). Every platform is a function of the frame count, so
  coding the platform fields JOINTLY (one dictionary for the tuple, ~7 bits)
  would fit; its cost (split, no merge across frames) is unmeasured. If it
  is affordable, `p`, the table, its guard and `sides()` can go, with (1,2)
  and (6,1) re-run without `p`.
- C3 is NOT explained by this (proven: its comparison stayed within f34, and
  (2,1)'s split and unsplit tables are identical).
- Fix (not applied): count game frames (`s.clock().steps(frames)`); make the
  generator immune to the idle player (step only the platforms, or refuse
  loudly on a restart or freeze during generation); have `platform_worlds`
  return the frames it covers and guard on THAT. Then re-run (6,0)'s two
  results.

## C14. Checks that can pass with nothing compared (FIXED but (f), branch correctness-batch)
- FIXED 99fd23ad: `differential::verdict` FAILS a check that compared
  nothing (every sample SKIPPED over the path cap); the fence constant is
  gone. `check arcs`/`check ref --path-cap 1` on the r10 fixture exited 0
  before, 1 now. No tier step relied on it (t1's harness steps compare 32
  of 32). check.sh's local tier, whose tests pass and print SKIPPED without
  the store or PICO-8, now fails the run.
- FIXED 9d68fb1b: the no-player exploration's identities (walk.rs) are
  `state_id`: the shape and each constant's path and bits, not `Debug`
  text; room (0,3)'s walk and kernel text identical. driver.rs's was
  `MinusOne::text` since d47ae948.
- SETTLED 24cf016e: `l1 check` by the UNWIDENED shape agrees with the
  forward: the replay's states are canonical and the block side's widening
  writes number and boolean columns only, never what `shape_words` hashes
  or the renumbering follows. The known-route check (which looks level -1
  up by the widened shape) asserts the two are equal at every step.
- FIXED 24cf016e: `l1 check` under `--split-frame` judges the steps against
  `clock.steps(frames)`; room (1,0) split's any% 99 route was UNSOUND from
  step 57 before, passes now (a slow test).
- FIXED ef53c2bb: `l1 probe` takes the tree's recorded settings and refuses
  settings flags (a wrong `--room` is refused); `--marks` on a float tree is
  refused (marks load as 16.16 only).
- SETTLED ff553440: the region grid does not change a tree (ckhash with
  edges and hulls identical under 16,6 and `--region off` in five forwards,
  fixed, float, objects, fruit, split; level -1 tables identical in three
  rooms; a grid the kernels cannot be built on fails at the build). It is
  no longer recorded and `--override-settings` is deleted; settings
  VERSION 4.
- OPEN (f): the `= P8` type defaults let fixed-only code run on float trees
  by omission (`Visited::load` exists only for 16.16); remove the defaults
  so the arithmetic is explicit. Deferred: a wide mechanical change across
  the engine and storage, which the `p` deletion (platforms-exact) is
  rewriting now.
