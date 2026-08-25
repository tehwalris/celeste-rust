# Open decisions for Philippe (2026-08-25 session)

Scattered questions collected from the room-2 kernel + kernel-ladder work.
Answer in batches; I'll act on each as it lands.

## Batch 1 (most blocking)

1. **Merge the ladder worktree?** The forward-backward subagent's work
   (forward ladder on kernels across all precision rungs + exact k16,
   strict mode, per-rung fingerprint, gates green) lives on branch
   `worktree-agent-a4a016a80c62ca8e1`, NOT merged. Merge into the live
   line, or review first?

2. **Origin-passthrough design for backward-on-kernels.** The backward
   sweep + pos-graph still use the interpreter because they tag lanes
   with an origin column the kernels can't carry. Two designs:
   (a) append `__origin` to `GLOBAL_NAMES` - clean, but changes every
       shape hash => invalidates ALL checkpoints + visited tables
       (CLAUDE.md: "not a change done halfway"); your call to take it.
   (b) engine-carried optional origin column through the generated
       interface (bind/rows/append/keys) - no invalidation, more
       emitter surface + a regeneration.
   Which one?

3. **Wire room (2,0) into the REAL engine - which kernel set?** Room 2
   is only in the test crate today. The real engine uses the PLAIN
   traced set, but room 2's plain set is ~1.06M lines (too big to check
   in - the whole reason the lattice exists). The lattice set is small
   (401k) but specialized (pin-guards, reduced 18-shape space) and needs
   a mod.rs merge step to coexist with room 1. Options: (a) check in the
   lattice set + build the multi-room merge; (b) something smaller;
   (c) defer room-2-in-real-engine until the ladder lands. Which?

## Batch 2

4. **Spd precision rungs.** Refused today (bucketed spd makes spd an
   interval input the kernels type as num; needs `ival_paths += spd` +
   emitter support). Priority, or defer? Room (1,0)'s ladder doesn't use
   them.

5. **Lattice decline -> hard crash.** Earlier doctrine call: a lattice
   kernel's baked-constant violation should be a hard stop (never-deopt),
   not a counted deopt. Wire that in now, or leave the gate-only behavior?

6. **Fairer timing - which measurement?** Today's numbers were quick-
   profile / raw-interpreter / standalone-runner. A fair number needs
   release + the rewritten program + the production engine. Which:
   release room-1 at a longer horizon, room-2 once wired, or the full
   ladder end-to-end?

## Batch 3 (lower urgency / already noted)

7. **Single-binary ladder** (recorded in strategy.md) - eventual, not now.
8. **Plans cleanup** - subagent running; will surface UNCERTAIN files
   needing your keep/delete call.
9. **Per-rung baked kernels** - optimization layer, price only if a
   campaign measurement shows the rung-agnostic set's dedup loss matters.
