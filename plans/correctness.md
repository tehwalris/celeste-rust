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

## C2. The orb deadline: a hand-derived bound nothing checks
- `ORB_MIN_FRAMES_AFTER_CHEST = 70` (frame.rs:284-286): "61 paused + 8 until
  the orb is collectable + 10 frozen = 79, taken as 70". No test derives or
  checks it against the cart.
- Its ceiling is level -1's H (`ForwardState::ceiling` = `TreeFilter::ceiling`,
  frame.rs:668-673, 863-868): without level -1 the orb room never gets the cut
  (sound, but arbitrary coupling).
- To do: check the bound with the reference engine (or derive it), and give
  the cut its own horizon (the search's).

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

## C8. Death and exit in the same update counts as a win
- `kill_player` (orig 121-122) then `next_room()` (orig 305) in one update:
  the search's win test is the room alone (frame.rs:189-212), so a dying exit
  is a win (FAKE by the category rules; chain.py reports a death), and the
  next room's pending-reload start is never searched (MISSES).
- Rooms with side spikes in row 0 next to the exit: (2,2), (6,2), (0,3),
  (2,3), (3,3). Not reached by any actual route yet.

## C9. The category runner falls back to the IL start
- tools/category_runner.py:378-380: when the previous room's database file
  breaks the boot chain, only the IL start is searched, which may not be a
  real start (MISSES).

## C11. Balloon seeds: PICO-8 verification hands them out in another order
- pico8_diff/replay.py assigns a file's `[seeds]` in POP order (balloons at
  the load, chests when opened); Rust's `--balloon-seeds`
  (`RoomObject::seeded`) and UCT use CREATION order. Where a chest precedes a
  balloon in creation order (rooms (3,3), (5,3)), the search's concrete
  balloons and the PICO-8 verification's get different seeds: 3 of 45
  chest-variant replays differ (found by agent F, 2026-10-20).
- To do: settle which order the tasdatabase's tools (Celia, UCT) define, make
  the replay match, and re-verify the (3,3) and (5,3) uploads.

## C12. Tooling degraded silently (fixed on branch simp-f, landing)
- Since f3123072 (2026-10-15) replay.py lacked the minimal cart's
  `__split_by_round` hook: every minimal-cart PICO-8 replay crashed, and the
  category runner's `canon_jumps` silently dropped every jump press
  ("original cart exit None -> None"). Re-check any runner result since then.
- The runner also passed `--room` to export-ui, which refuses it, so its UI
  step failed on every improvement.

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
- Tooling: pico8_diff/replay.py cannot run the minimal cart on PICO-8 since
  f3123072 (no `__split_by_round` stub); fails loudly.

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
