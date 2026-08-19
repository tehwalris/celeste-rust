# Shape-as-data: per-lane liveness tags + boundary materialization

Status: DESIGN (2026-08-19). Owner context: the dying-frame deopt (task
#159; census in dedup-roofline-plan.md K2; two refuted fixes in
BENCHMARK_DATA.md "K2's pm1 death-partition fix" and
rewrites-death.jsonl's header).

## The problem, one paragraph

All 399,516 deopting lanes at H=68 are alive at frame start and die
mid-frame: `kill_player` (celeste-minimal.lua:93/:97) runs
`destroy_object`, which structurally shrinks the objects array, and the
very next statement (`:99`, `is_solid(0,1)`) walks into collide loops
whose rewrites premise `count(objects) == 1` (collapse_loop h010-h019)
and `objects[1] == this` (assume_eq h022-h037). Structure is per-state
(ArrayTable is `Vec<HeapId>`, no lane dimension — value.rs:519), so a
per-lane death cannot be represented mid-frame; today it asserts and
deopts, costing ~94% of frame-body CPU at depth. Input-keyed dispatch
cannot fix it (measured twice). The fix must let the dying lane finish
its frame INSIDE the specialized, branch-free program.

## The design

**Mid-frame: death is a VALUE, not a structure change.** The kill site's
`destroy_object(obj)` is rewritten to per-lane value stores only:
`this.collideable := false` (the dead mark) — plus the stores kill_player
already does (`deaths += 1` is a pinned timer global, `will_restart`,
`delay_restart` are plain globals). The array keeps its length; the
premises `bound == init` and `objects[1] == this` stay TRUE; nothing
asserts; dying and surviving lanes share one state, one straight-line
trace, SIMD-friendly.

**Boundary: materialize the tag.** A `split_by_liveness` step in the
canonicalization chain — same seam as `split_precision_straddles`
(main.rs:284, verify.rs:2288, BEFORE vectorize_states) — splits each
state on the dead mark (`split_by_condition`), then performs the
deferred structural del + gc on the dead sub-state. Downstream
(shape grouping, band filter, frontier subtract, dedup keys, sweep)
sees exactly today's canonical dead states. THE GATE IS SET-IDENTITY:
H=68 per-frame rowkey sets equal to ~/perf-scratch/k2ctl, deopt
399,516 -> 0.

**Following frames:** materialized dead states have the empty shape and
dedup to ~2 rows/frame; they route to the (already landed, currently
19-lane) `[]=rewrites-death.jsonl` variant, which becomes load-bearing.

## Why the death case is CHEAP: the invisibility argument

The enumeration (this plan's underlying study) shows the dead player is
almost invisible to the rest of its frame ALREADY:

- Every same-frame read after the kill is a `collide`-family loop
  (`:99`, `:161`, `:177` — 8-12 loops), and collide's condition
  (`:625`) requires `other.type == type` (the player is never the
  queried type: platform/fall_floor/fake_wall) AND `other ~= obj`
  (self-excluded) AND `other.collideable`. With `collideable := false`
  the dead object matches nothing REGARDLESS of type — this is the
  robust, room-independent gate, and `collideable` is already a
  per-object value field read inside already-if-converted conditions,
  so per-lane falseness flows through existing selects.
- The `_update` foreach continues after player.update returns; in
  (1,0) there are no other objects. In (0,0) the wall precedes the
  player in array order, so its update already ran. (Verify per room,
  don't assume — see obligations.)
- `_draw` visits the dead object; the draw builtins are noops. The
  post-kill stores player.update makes to `this` (`:99` onward
  computes on the orphan today too) are garbage-but-harmless: the
  boundary materialization drops the object, so those columns are
  never read by assembly. (This is the general shape-as-data
  simplification: assembly reads only the columns the tag's shape
  includes.)

So the rewrite surface for death is SMALL: the kill_player
destroy_object call site (one of the 9 destroy_object_55 inlines — the
h-family and the other 8 sites are untouched), plus the boundary step.
No new mid-frame branch. No pm1 change (mid-frame merging of dead and
alive lanes into one state is now CORRECT and desired; the dead mark is
a Bool value cell, so it enters the row key via collect_columns_labeled
for free — virtual_merge.rs:390-436).

## Verification obligations (guard, don't assume)

1. **Invisibility**: for the remainder of the dying frame, a
   dead-marked object with `collideable=false` is observation-
   equivalent to a destroyed one. Mechanized as: the rewritten kill
   site + boundary materialization, differentially checked against the
   plain program. Deep differential verify OOMs past ~f60, so the gate
   is bench set-identity at H=68 (all 68 frames), plus the suite.
   Room-specific residuals (e.g. a later-indexed object's update
   running in the same frame and querying `check(player, ...)`) are
   covered by the collideable gate; a read that does NOT go through
   collideable (grep says none today) would be a loud screening
   failure, not a silent wrong answer, because the entry is derived by
   `screen`, not hand-asserted.
2. **foreach semantics**: keeping the array intact must reproduce
   all()'s deletion behavior for the frames where it matters. In the
   original, `del` mid-iteration shifts successors; with no del, the
   walk is the plain walk. For (1,0) death (singleton array, player is
   the only element) the walks are identical. For multi-object rooms
   this DIFFERS in general (the known model deviations in
   room20-plan.md:151-172 are exactly this class) — phase 2 territory,
   per-room screening.
3. **Boundary equivalence**: materialized dead rows byte-equal today's
   dead rows (will_restart/delay_restart values, timer pins, gc of the
   orphan and its closures). Set-identity gate covers it.
4. **Ladder**: the dead mark is not a widening (it is exact per lane),
   so no refinement rung is owed. The materialization must run at
   every level identically (it sits before the widening-dependent
   grouping, same as the straddle split).

## What this does NOT cover yet (phase 2, explicitly deferred)

- **Mid-frame CREATION** (fake_wall break spawns a fruit, chest, orb,
  spawn->player's destroy+create): needs the union-array version —
  pre-allocate possibly-live objects marked inactive, activate
  per-lane, mask their updates while inactive. Real design, real
  per-frame masking cost; only worth it when (0,0)/(2,0) campaigns
  need branch-free break/collect frames. The S1-S4 lattice and the
  0.0% S2 result (room00-plan.md) say: do not build this before the
  death case has paid.
- **Fruit collect / fly_fruit / key / chest destroys**: same
  collideable-style treatment probably applies (destroy-only sites),
  but each needs its own invisibility check (fruit is READ by
  player-collide? no — fruit checks player, player never checks
  fruit) and the off-counter/bob coupling constraint
  (abstraction.rs:735-748) applies to anything tag-gated.
- **Kernels**: adopt the dead-mark column after the interpreter path
  is certified; the steady overlay gains two select sites (the kill
  stores), zero branches.

## Order of work

| step | what | gate |
|---|---|---|
| 1 | `rewrite print` the kill path; identify the exact destroy_object inline entries for the two kill sites and the assert ids | reading, no gate |
| 2 | New rule (or speculate/masked-store composition) rewriting the kill-site destroy into the value stores; screen it | rule verifier + suite |
| 3 | `split_by_liveness` boundary step (straddle-split pattern, abstraction.rs:477 as template), behind an env flag initially | suite; unit test on a hand-built dying state |
| 4 | H=68 bench A/B vs k2ctl: deopt -> 0, rowkey sets identical, wall | THE gate |
| 5 | Drop CELESTE_DEOPT_COLLECT_FIRST; measure the pair; update ladder.sh if it wins | bench pair |
| 6 | Kernel adoption + (2,0) fruit/death port (columnar-engine.md:288 prerequisites) | gate 2, kernel gates |

Prize accounting (measured 2026-08-19): tagged runs 307.4s + plain
re-runs 240.7s of 581.2s frame-body thread-CPU at H=68; kernel
starvation 72% coverage. Expected: steps 2-5 recover most of both.
