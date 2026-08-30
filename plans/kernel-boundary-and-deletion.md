# Kernel boundary, wrapper deletion, and the search objective

Status: strategy, 2026-08-30. Philippe's direction after profiling the forward
(the kernel is ~37% of a frame body; the wrapper machinery around it is ~63%).

## The approach

Aggressively delete code. Clearly define interface boundaries. Make the code
adhere to those boundaries. Then delete more. Iterate. We treat the kernels as
a GIVEN - their speed is fixed - and drive everything *around* them toward the
kernel's own floor.

Reference floor (room (1,0), f79, 16 threads, QUICK build), from
`CELESTE_KERNEL_DRYRUN=1` (pack + kernel call, discard - no dedup, no
materialize, no boundary, no store):

| what | wall | note |
|---|---|---|
| pure kernel (dry) | **9.1 s** | peak 6 GB |
| full frame body (`fwd.interpret`) | **24.8 s** | kernel is ~37%, machinery ~63% |
| + boundary merge, save/checkpoint | ~+17 s | outside the body |

The 63% is what we are attacking: the dedup layers we materialize-then-filter,
the frontier subtract's cache misses, the boundary heap-walk, the re-keying,
the gc. Most of it is stale wrapper that a clean kernel boundary makes
unnecessary.

## The kernel interface (the boundary we define and hold)

A kernel call takes a FIXED-SIZE batch of input lanes (16) in a clearly-defined
format, and returns 16-lanes-worth of output. Specifically:

- **Input:** 16 input lanes, fixed format.
- **Output:** a set of output lanes - "16 lanes worth of outputs." NOT
  separated by input lane. The kernel just emits some outputs.
- **Within-16 dedup is the kernel's option, not a guarantee.** The kernel has
  static visibility into its 16 lanes, so it MAY dedup them; callers must not
  rely on either "deduped" or "not deduped."
- **No input-lane -> output-row tracking.** We delete this feature entirely
  (the origin / passthrough column). The kernel does not tell you which input
  lane produced which output row.
- **Keys come from the kernel, and only from the kernel.** Each output row
  carries (a) its fully GC'd shape, (b) the hash of that fully-GC'd shape (the
  SHAPE key), and (c) the hash of the row's values (the CONTENT key). The
  kernels already produce GC'd shapes and these hashes; we add/keep tests that
  gate exactly this. **No hash is computed anywhere outside the kernel.**
  Everyone downstream REUSES (shape_hash, content_hash). This deletes the
  boundary canonical heap-walk, the gc-before-hashing, and the
  `engine_row_keys` recompute.

### Consequence for the position graph

Removing input->output tracking removes the fine "which output row came from
which input position" mapping. So the online pos-graph builder either:

- passes a **position-uniform input batch** (same input XY across all 16
  lanes), so every output of that call is known to come from that one input
  position - no per-lane tracking needed; or
- accepts a **coarser** input->output mapping and adds phantom edges to its
  graph.

Either way the origin column is gone.

## The interpreter is the reference, behind the same interface

We do not care about interpreter performance. We pack the interpreter behind
the EXACT SAME interface as the kernels - same input format, same output
format, same (shape_hash, content_hash) keys, GC'd shapes - and it becomes the
reference implementation you can swap in instead of the kernels. It must be
byte-identical in what it returns, not in how fast.

This is what lets us delete the vectorized-interpreter engine machinery: once
the interpreter is a per-call reference, the search does not need it to be a
fast parallel engine.

## The objective to minimize

End to end: run the search binary and get the exact provably-fastest TAS for
room (1,0), starting FRESH (no cached checkpoints). Minimize wall-clock.

Requirements:

- **Checkpoint every frame** - a crash may lose at most the current in-flight
  frame, nothing earlier.
- Same pattern of forward runs as today. Forward runs until it finds a solution
  at rem = 0 bits, then backward to mark, then forward again at rem = 1, and so
  on, until either some rem finds nothing (the horizon is refuted) or the final
  exact kernel run finds the exact solution.

No other requirements. Interpreter speed does not count.

## Strategy: delete, define, adhere, delete

Round 1 target: **delete ~1/4 of the code** (~15k of ~62k lines). The rounds:

1. Delete what the new boundary makes dead: input->output tracking, hashing
   outside the kernel, the boundary re-key/gc, chunking machinery (the
   `chunk_states` input cap, the two inner-slice knobs, the slice loop - keep
   only the load-bearing freeze/moving semantic partition), and the
   vectorized-interpreter engine once the interpreter is a per-call reference.
2. Define the kernel interface as a real trait/boundary; make the ASM kernel
   and the interpreter both implement it.
3. Delete again whatever the boundary orphaned.

Dedup itself STAYS multi-tiered on purpose (different lookup/update costs per
tier - a cache-tiling design), but moves to decide-at-the-door: a lane is
kept-or-dropped as it leaves the kernel, through cheap-local -> within-frame ->
historic tiers, and only survivors are written, already in final form. No
materialize-then-filter, no output buffer, no flush - the memory bound falls
out of never materializing the pre-dedup fan-out.
