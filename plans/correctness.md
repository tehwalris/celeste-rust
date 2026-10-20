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

## C4. Rust and Python loading-frame patches differ
- Rust, trace/cart.rs:264: `if o.spd.x ~= 0 or o.spd.y ~= 0 then o.move(...)`
  (the minimal cart's own `foreach`, celeste-minimal.lua:788).
- Python, pico8_diff/replay.py:229: `o.move(o.spd.x, o.spd.y)`, unguarded,
  also on the minimal cart.
- `move(0,0)` is not a no-op: `move_x`/`move_y` (celeste-minimal.lua:664-696)
  run their loop once at amount 0 and zero `rem` inside a solid. So the
  search and the PICO-8 validation can disagree on a jank start.
- To do: settle which matches the ORIGINAL cart's loading frame, then make
  one source (audit foundations #1: patches written once in Rust, the PICO-8
  carts generated) and re-check every witness, upload and boot chain.

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
