# Correctness to-dos (2026-10-20; from the second audit, /var/tmp/audit2/)

Open soundness and verification questions. Philippe wants to handle these
hands-on: refactors must NOT change the code paths below until each item is
settled here. Evidence is file:line at develop 19869c5d.

## C1. Pruning is not first-class; the known-route check misses cuts (FIXED, branch correctness-2)
- Was: `search::known` listed the pruning steps by hand (level -1
  `too_late`, the ladder's `allowed`, the hull claim, W's support, W at the
  exact remainder, the reach marks); `frame::not_expanded`'s cuts (exits,
  `berry_lost`, `orb_deadline_skip`, out-of-room under `--win-at`) were no
  named step: a route a wrong cut drops showed only as "NOT IN THE TREE",
  unattributed, a step later.
- FIXED: ONE interface, `prune::Prune` (`About`: name, kind - FILTER, dropped
  where made, or CUT, kept unexpanded - and projection; its tree-record
  line; its horizon; per lane `prunes`, and `why`). `prune::Prunes` holds a
  forward's: the tree's (`Recorded`: `TreeL1`, `OrbCut`), the ladder's
  `MarkFilter`, the cuts its settings imply (`Exit`, `OutOfRoom`,
  `BerryLost`). It replaces `Filters` and the `orb` parameter in `WaveCtx`.
  The hot forms are concrete: level -1 per lane (`TreeL1::drops`) and the
  ladder per batch (`MarkFilter::allowed`) as fields of the sink, the cuts
  once a unit through `Prunes::not_expanded`, which applies exactly the cuts
  `Prunes::each` lists (one `dyn` call per cut per unit, none per lane).
  `each` destructures every field. Prunes reach neither the backward, the
  reach nor the concrete search.
- `check known` / `search --prefer` iterate `Prunes::each` on the level's row
  (`lookup_view` widened as the tree holds it): every filter at every step
  past the start AND on the winning leaf (new: an exit row a filter drops
  was blamed on W), every cut at every step before the exit (a cut's
  report beats "not in W's support", which it causes). A failure names the
  prune, its projection, its record line and the step (the error message
  too). Two guards: each prune's record line is the tree's (the ladder's
  marks recomputed by the check must be the ones the tree was filtered
  for, but for a lower `--frames`, a stricter filter, noted), and no
  prune's horizon is below the check's. `check known` also reads a tree
  that ENDED on an empty frontier (`frame::tree_ends_by`: a cut that takes
  every row), which it refused before.
- Proof (2026-10-26): every pin of `./check.sh t0 t1 t2 big` identical; the
  31 `[asm build]` kernel-text digests of those runs identical to the base
  959d669b's; `--prefer` passes on the t2 gates (arc10, arc10f, dual10) and
  search42 (its ladder level: "the ladder's filter (`filtered_for steps 50
  level-1 off marks 241 2f8fd886e0cf30be`)", the tree's record line).
  `check known` on finished searches with each kind of cut: arc10 (exit,
  out-of-room), s42 (the ladder), room (5,2) any% `r0sx`, (150,5), h150
  (level -1, exit, the orb cut `orb_cut h 150`: optimum 150, the stored
  witness, 66.6 s, 7.1 GB), room (0,0) 100% `r0sx`, (115,5), h115 (level -1,
  exit, the lost-berry cut: optimum 115, the stored witness, 50.3 s, 9.2
  GB). NEW TEST `known::tests::check_known_names_a_wrong_cut_and_its_step`
  (t2): a test-only fault on the exit cut (`prune::FAULT`) at the known
  route's cell at step 30 drops it, and `check known` fails "PRUNED ... by
  step 30: the exit cut does NOT EXPAND it at step 30"; at step 20 the
  fault ends the tree and the check names it alike. The record guard,
  shown by hand: a copy of s42 with its `filtered_for` fingerprint changed
  is refused. Perf (instructions a rep, release, base -> C1): r62h
  16.815G -> 16.812G (-0.02%), r62h57 43.919G -> 43.931G (+0.03%), f42n
  5.0123G -> 5.0125G (+0.00%).
- Unblocks native level -1 (assessment, 2026-10-20): feasible but a design
  decision: per-shape inductive ranges carried by the kernels, a loud
  widen-and-retrace path for declined lanes, and a position-only level whose
  backward computes distances; about -1,000 lines net; every level -1 result
  re-verified. The known-route check covers it now: it would be one more
  `Prune` (set aside by Philippe).

## C2. The orb deadline: a hand-derived bound nothing checks (FIXED, branch correctness-batch)
- CHECKED: `ORB_MIN_FRAMES_AFTER_CHEST = 70` against the cart
  (`frame::tests::the_orb_deadline_is_below_the_carts_chest_to_orb_time`,
  reference engine, unsplit and split): room (5,2)'s any% witness to its
  chest's opening, then no input (the shortest: nothing freezes the draw
  that ticks the chest and the orb) to the orb at rest, collectable, 68
  frames; the witness's collection to the first update that runs, 11 (10
  frozen). A win is >= 79 frames after the opening, identical split and
  unsplit; 70 keeps its margin, and the test fails if the constant passes
  the cart's figure.
- OWN HORIZON: the cut is `frame::OrbCut`, the horizon the tree was started
  for (the search's, `forward --steps`), recorded as the tree's `orb_cut`
  line (settings VERSION 5); `grow_tree` refuses to raise a tree past it,
  as for level -1. `TreeL1::orb_ceiling` is gone: the orb room is cut with
  or without level -1.
- UNITS: the cut counts frames. A chest closed after step k opens at the
  earliest in frame `frame_of(k + 1)`; it is skipped when that plus 70
  exceeds the horizon's frame (`the_orb_cut_counts_frames_under_the_split_frame`).
  Unsplit this is the old test exactly (`frame + 1 + 70 > H`), so a tree
  built under level -1 at the search's horizon is unchanged.
- No result changes: a cut row cannot win by the horizon (the bound above).
  The witnesses through the orb (any% 150, nodiag 157) replay to their
  exits with the orb under the new code; no winning route meets the cut,
  which starts at frame H-70 while a route exiting by H opened by H-79.
  Re-searched under it (fc516a4d): room (5,2) any% `search --level r0sx
  --level-minus-one 150,5 --frames 150 --ceiling --prefer
  tas/room_5_2_exit_frame_150.txt`: arc bound 150, concrete optimum 150,
  the known route survives every pruning step (5.6 min, peak 7 GB).

## C3. Room (2,1): the split frame reaches FEWER states than unsplit (EXPLAINED: the hulls' precision, no bug; branch correctness-2)
- Was: at `p` the split frame reached fewer states, "unexplained"; fewer
  is the direction a soundness bug shows.
- Re-checked with `p` gone, at `r0sxn` (2026-10-26, develop 959d669b + C1,
  release): `forward --room 2,1 --level r0sxn --steps 60` (43 s, 4.5 GB)
  against `--split-frame --steps 120` (25 s, 4.1 GB). Every boundary the
  same states (count and content) through f29; from f30 the split frame
  FEWER, cumulatively -26 at f30, -207 f35, -715 f41, -38,957 f57,
  -29,131 f60 (87,212,592 against 87,241,723, -0.03%). At f30 the split
  states a strict subset (26 missing, none extra); 223 missing by f33.
- THE EXACT REACH (`reach::exact`: exact in the remainder over each tree's
  recorded graph) is IDENTICAL in the two trees at every boundary f0-f33,
  states and remainder sets (17,433 states at f30, 75,514 at f33), every
  split one inside its hull; NONE of the missing states is exactly reached
  in the unsplit graph (past f33 the oracle passes its 1024-segment cap).
- So they are the unsplit hulls' over-approximation, not states of the
  game: a hull is one arc per axis, which holds no union, and its arc
  depends on what was merged (the split frame merges at the mid-frame
  node). One of them: f30, `player.x = -2, y = 112`, a right dash just
  begun (`move_x`'s loop from 0 takes 2 px at rem.x -0.5; the dash's
  freeze skips the draw's clamp), from x = 0, y = 112, spd.x -0.2, grace 3
  at f29, whose unsplit hull's x arc is [45876, 19661] across the end
  (index 0..19661 in it) and split one [19661, 65535]: the edge's guard x
  [0, 13108) meets only the unsplit arc. Both hulls hold the state's exact
  set; neither holds the other.
- A LEGITIMATE PRECISION DIFFERENCE (the hull representation), not a
  soundness bug: the split frame is not a superset in hull trees, and the
  same in the exact reach. The slow test
  `frame::tests::split_frame_reaches_the_unsplit_frontier_at_a_near_level`
  now states it to f30 (both forwards; the exact reaches equal at every
  boundary, the hull trees through f29, at f30 the split one a subset by
  26, no missing state exactly reached). Its doc's old claim that the
  boundaries' hulls are the unsplit frame's was wrong. As far as this item
  goes, a further split-frame refactor (audit M6) is unblocked.
- Not shown past f33 (the exact oracle's cap); the probe was a throwaway
  test (/var/tmp/correctness-2-scripts/frame-with-c3-probe.rs, not
  committed).

## C4. The minimal cart's move guard: UNREACHABLE (proven); the player unguarded (DECIDED, branch jump-buffer)
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
- DECIDED (2026-10-25): (2), `24188cfc`. The player moves unconditionally
  (the minimal cart, split-frame.lua, `trace::cart::apply_loading_frame`);
  every other object keeps the guard (fully unguarded, the balloon rooms
  fail kernel assembly on fractional object coordinates). Measured: every
  16.16 tree byte-identical (r10 r42x r42n r10arc r60s, the room (1,0)
  gates, (6,2) 100% without level -1 to f45); level -1 builds another
  (6,2) table (the r62h tree -9.6% states; the 100% witness's `l1 check`
  clean); in doubles +0.01 to +2.8% states (the original turns a `-0`
  remainder `+0`).
- Evidence: /var/tmp/c4-quick/cart-differences.md; probe worktree
  /var/tmp/c4-wt (branch c4-probe, uncommitted), /var/tmp/c4-probe-scripts/.
- Still to do: a standing DIFFERENTIAL CHECK running the original cart's Lua
  and the minimal cart side by side (every room, many inputs, state per frame
  modulo the handled differences), so the minimal cart and the native
  builtins are checked, not trusted.

## C5. Verification gaps at the object levels
- (`p` is gone, C13: platforms are exact and the reference runs them.)
- `check reference` cannot compare at `f`: in room (6,0) at `r0sxhf` the
  kernels make 2 states a step, the reference 1 (the reference widens only
  the held buttons, the kernels the fruit too; plans/architecture.md "Open
  issues"). The fruit level is checked only by pins and the concrete search.
- At `n`, `check ref` and `check arcs` skip most rows over the reference's
  path cap; room (7,0) `check arcs` is an open issue.
- Proposals: a reachable-sample soundness check at every level (audit
  architecture.md M3: concrete states from playouts, the concrete search and
  known routes, stepped exactly, projected, held against the tree's edges and
  hulls); concrete members in place of `step_at`'s pins (audit
  kernel-pipeline.md K10).

## C6. Closure captures are not part of a state's exact identity (FIXED, branch correctness-batch)
- `shape_sig`/`shape_hash_of` hash a closure's function id and capture COUNT
  (exact.rs:435, runtime2.rs:391-395); `exact_rows` reads only `Val` cells
  (exact.rs:964-1000): capture targets are hashed nowhere.
- FIXED: the one converter (`trace::block::ToRt2::closure`) asserts every
  capture IS the closure's OWNER, the one table holding it (`obj.move`
  captures `obj`), which the block's structure already says; a capture of
  another table, a closure with captures held by two tables or by a global,
  is refused with an error naming the closure. Every block passes through it
  (the tracer's skeletons and outcomes, the reference engine's rows), so the
  concrete and dual searches' `ExactRow` dedup is exact by construction.
  Test: `trace::block::tests::a_closure_capturing_another_table_than_its_owner_is_refused`
  (room (1,0)'s start converts; with every captured name rebound to the
  globals table it is refused); the whole ladder converts every block of the
  real cart through the assert.

## C7. HIGH: the jump buffer is removed, so the search MISSES real states (FIXED, branch jump-buffer; re-proofs below)
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
- FIXED (2026-10-25, branch jump-buffer): the cart has the original's
  buttons exactly (`p_jump`, `p_dash`, `jbuffer`, `02076e4f`); every level
  widens the buttons' memory (the former `h`, plus `jbuffer` the unknown
  countdown; a spec with `h` is refused at parse); the concrete search runs
  them exactly. The sets are the former `h`'s (every fixture's per-frame
  counts identical); the buffered example reproduced on PICO-8 in both
  carts and pinned (`concrete::tests::a_buffered_jump_then_a_fresh_one_as_pico8_runs_them`);
  the re-timing is deleted (`02fe9d5c`: the goldens are the raw files, the
  float oracle replays all 336 bit for bit). The buffer's duplicate
  emissions are dropped by a kernel-build proof (`47c17fae`): the release
  budget +0.9..+1.5% against develop.

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
  (C13 since fixed), nodiag (6,1) 92, nodiag (1,2) 113, 100% (6,1) 114, 100% (4,3)
  109. Decided (Philippe, 2026-10-25): the fix above.
- RE-PROVEN under the real buttons (2026-10-25, branch jump-buffer on
  develop d63f0056; plans/results.md "Under the real buttons"): every result
  at risk, NONE moved - any% (0,0) 93, (1,0) 99, (4,0) 76, (5,3) 79, (1,2)
  103 (develop's platform re-proof had bound 99 < 103: at risk too), summit
  55, gemskip summit 57; nodiag (6,0) 73, (6,1) 92, (1,2) 113; 100% (6,1)
  114, (4,3) 109. Every known route kept, every witness the stored one and
  valid on PICO-8 in the original cart. The breadth-first proofs grow with
  the buffer ((4,3) 100%: 31.2M steps, 26 GB).

## C8. Death and exit in the same update counts as a win (FIXED, branch correctness-batch)
- The original cart: `kill_player` (orig 121-122 spikes, 125-126 the bottom)
  destroys the player and `restart_room()` sets `will_restart=true`,
  `delay_restart=15`; the REST of the same update runs on the dead player's
  table, so line 305's `if this.y<-4 ... next_room()` loads the next room in
  that update; `load_room` leaves `will_restart` alone, so 15 updates later
  `_update`'s restart block (1186-1190) reloads the NEW room and its player
  respawns there. The minimal cart is the same (min 93/97, 231, 709-720,
  778-783). The search's win test was the room alone, so a dying exit won.
- POLICY (Philippe, 2026-10-25): a dying exit is NOT a win: the per-room
  optima must chain in real play, where the next room restarts 15 updates
  in (and the category rules count a death). The cart's semantics, no knob:
  `frame::wins_of` requires `will_restart` false (`died`). Only
  `kill_player` sets it, and it holds until the reload, during which no
  player exists to leave the room: a row that left with it set left in its
  player's dying update. Such a row is not expanded either (it is in the
  next room). The level -1 table, a bound on the frames to ANY exit, stays a
  lower bound.
- Test: `frame::tests::a_dying_exit_is_no_win`, room (2,2): a constructed
  rise at x 64 under the top row's right-facing spike (tile 59 at tile 8)
  dies and loads (3,2) in one update: no win (the test fails without the
  fix); the same rise at x 40 is a clean exit and wins.
- The MISSES half (the next room's pending-reload start is never searched)
  needs no handling: that start restarts its room 15 updates after entry,
  so a dying exit could pay off only if it beat the room's best clean exit
  by more than about 15 frames (everything the next room's player does
  before the restart is lost), and it fails the real-play validity check (a
  death) besides.
- No result changes: fewer wins only raise bounds, and every witness still
  wins. The 15 witnesses of the five rooms with spikes beside the exit
  ((2,2), (6,2), (0,3), (2,3), (3,3): any%, 100%, nodiag, gemskipnodiag
  and the community references) replay to their named exit frames under the
  new test with a living winner; no winning lineage holds a pending restart.

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

## C11. Balloon seeds: PICO-8 verification hands them out in another order (FIXED, branch correctness-batch)
- SETTLED: both TAS tools hand a file's `[seeds]` out in CREATION order,
  balloons and chests together: UniversalClassicTas `set_seeds`
  (CelesteTAS/TAS.lua:537-559) walks `pico8.cart.objects`, Celia
  `load_rng_seeds` (cctas.lua:646-672) the initial state's
  `cart.objects`, each giving the next seed to the next balloon or chest;
  the room's object list is `load_room`'s creation order (x outer, y
  inner), `trace::cart::room_objects`'s, which the search already uses.
- FIXED: the PICO-8 replay (`tools::pico8`) seeds by creation order
  (`seed_table`); the POP variant and `--seed-order` are deleted. The two
  agree on every first load (each `init` pops in creation order); they
  differ only after a room reload in a single-room replay, where POP's list
  was used up (measured: room (3,3), seeds 0.5,0.25, a run that dies at
  f100: from the reload at f115 the balloon's offset is 0 under POP, its
  seed under creation). That is the likely source of the 3 of 45 differing
  chest-variant replays: a death no valid route has.
- Re-verified, no verdict changed: `rewrite validate uploads` of the (3,3)
  and (5,3) uploads (in-repo gemskip100, gemskipnodiag and nodiag TAS28,
  gemskipnodiag TAS30, and /var/tmp/to-submit/gemskipnodiag TAS28 and
  TAS30: all VALID) and of the database's own 100, gemskip100 and any%
  TAS28 and TAS30 (VALID but the database's gemskip100 TAS28, INVALID as
  before: it never exits on PICO-8): every row byte-identical under both
  orders. `rewrite pico8 replay` of the eight (3,3) and (5,3) witnesses
  (any%, the references, nodiag, gemskipnodiag): every frame line identical
  under both orders, each exiting at its named frame.

## C12. Tooling degraded silently (FIXED, landed 4633008e)
- Since f3123072 (2026-10-15) replay.py (now `rewrite pico8`, whose t0 test
  checks every tracer hook has a stub) lacked the minimal cart's
  `__split_by_round` hook: every minimal-cart PICO-8 replay crashed, and the
  category runner's `canon_jumps` silently dropped every jump press
  ("original cart exit None -> None"). Re-check any runner result since then.
- The runner also passed `--room` to export-ui, which refuses it, so its UI
  step failed on every improvement.

## C13. FIXED (branch `platforms-exact`): the platform-world table missed real positions
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
- FIXED by deleting the table (2026-10-25, branch `platforms-exact`,
  Philippe's decision): platforms are EXACT at every level. 3e245528 keys
  room (6,0)'s ten by the first one's `spd.x` and `rem.x` (`exact::Deps`,
  followers recorded per leader code and checked on every row, a
  difference FATAL; plans/architecture.md "Keys"); a83674e6 deletes `p`,
  `concrete::platform_worlds`, its guard, `split::Points` and `sides()`,
  and the reference engine's refusal (a spec holding `p` is refused).
  Every platform result re-proven on the final code, release, each with
  `--prefer` its stored witness (all kept; every witness the stored route):
  (2,1) any% 82 `r0sxhn` (arc bound 82, 6 min, 9.6 GB); (2,1) nodiag 85
  (bound 85, 3 min, 6.7 GB); (6,0) any% 70 split `r0sxhf` (bound 70, 11 min,
  18.2 GB, 1.61G visited, 42 key bits); (6,0) nodiag 73 (bound 73, 11 min,
  12.4 GB); (1,2) any% 103 `r0sxhn` (bound 99, the concrete search's first
  win 103, 8 min, 6.7 GB); (1,2) nodiag 113 `r0sxhn,r0sxh,r0sx` (bounds
  109, 113, 113, 2 min, 5.6 GB). So (6,0)'s refutations hold again: no
  any% win before 70 and no nodiag win before 73, by the arcs over exact
  platforms (plans/results.md, "The platform rooms without `p`").

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
- SETTLED (f), round 4 F3b/F3c: the `= P8` type defaults let fixed-only
  code run on float trees by omission (`Visited::load` exists only for
  16.16). No type parameter defaults to an arithmetic or a circle any
  more, nor do the aliases `RefEngine`/`FloatRefEngine`, `Graph::new` or
  `runtime2::P8` stand for one: fixed-only code says `<Fixed>`, and
  `conventions::no_type_parameter_defaults_to_an_arithmetic` refuses a
  default.
