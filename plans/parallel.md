# Parallelism: emit by source, own by destination (2026-09-13)

Overnight work, decided alone where a decision was needed (listed at the
end). Target: the level-0 forward's wall time, with the code staying one
layer simpler than before, not one layer more.

## The shape of a frame

Two phases with one barrier, the shape of a partitioned hash join:

1. **EMIT, partitioned by INPUT.** The frontier's blocks are cut into
   units of `UNIT_LANES` (2048) lanes; `threads()` workers pull units off
   an atomic counter and run the frame step on each. A worker's
   `ForwardSink` sorts every emitted row into a SLOT keyed by the row's
   OWNER (`owner_of(shape, cell)`, a hash) and output shape; the kernel's
   append step pushes fields straight into the slot's packed block. The
   only dedup here is the step's within-call `seen`. Nothing shared is
   touched: no visited set, no atomics, no locks.
2. **OWN, partitioned by OUTPUT.** Worker `o` walks every sink's slots
   addressed to it: ladder filter, then the door against ITS OWN visited
   shard (`Vec<Visited>`, one per owner - the two-tier `(shape, cell) ->
   {content}` structure kept intact, now private), then `append_rows` of
   the survivors into its own next-frame PIECES (one block per shape).
   Each `(shape, cell)` is seen by exactly one thread, so the visited
   set needs neither locks nor a concurrent table.

The next frontier is the union of the owners' pieces (up to `threads()`
blocks per shape); nothing is merged. The checkpoint writes one file per
piece, in parallel, each sorted by `(cell, key)` in place - so the file
set for a frame is a canonical function of the frame and the thread
count, not of scheduling. The backward re-runs go through the same unit
scheduler (a per-unit hit vector, disjoint by construction), and each
iteration's layers are loaded in parallel.

Why not one shared visited set with CAS at the door: it works (one cache
miss per insert either way) but every row costs an atomic, the two-tier
structure needs a concurrent inner table with a resize protocol, and the
outcome depends on which thread wins each race. The owner phase costs a
compact copy of ~2.3 emitted rows per kept row and buys zero shared
state and a deterministic result.

Why a hash and not a cell range for the owner: the frontier lives in a
corridor; a range would hand one owner the whole corridor. Inside an
owner the visited set is still per-cell.

## What must scale with it

- `FrameStep::run(&self, block, cell_in, lanes, sink)` - engines are
  `Sync` and take a lane range. `FrameEngine` is stateless; the reference
  engine sits behind a `Mutex` (`impl FrameStep for Mutex<RefEngine>`).
- Checkpoint: per-piece files, parallel. Dropping won rows from the
  frontier: per block, parallel. Pos-graph edges: per-worker sets merged
  at the frame's end (thousands of pairs).
- Serial per frame: unit list construction, the frame-stats fold, the
  pos-graph flush, the log line.

## Decisions taken alone

- `threads()` = `CELESTE_THREADS`, else half the logical CPUs (one per
  physical core: the kernels are AVX-512 bound and SMT siblings share the
  units). Owners == workers.
- `UNIT_LANES = 2048`: ~2,500 units at a 5M-row frontier; per-unit setup
  is the `seen` sets and the buffers, a few hundred microseconds.
- The kernel's `CELESTE_KERNEL_KEY_CHECK` gate moved from the call to the
  owner phase (`asm_kernel::key_check` per slot); same check, same
  environment variable.
- The `class` bucketing (already gone) is not coming back for this: the
  owner's pieces are per shape.

## Measurements

(filled in as they land; see BENCHMARK_DATA.md for the tables)
