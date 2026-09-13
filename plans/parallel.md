# Parallelism: emit by source, own by destination (2026-09-13)

Overnight work, decided alone where a decision was needed (listed at the
end). Target: the level-0 forward's wall time, with the code staying one
layer simpler than before, not one layer more.

## The shape of a frame

Two phases with one barrier, the shape of a partitioned hash join:

1. **EMIT, partitioned by INPUT.** The frontier's blocks are cut into
   units of lanes (`unit_lanes`); `threads()` workers pull units off
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
- Units are adaptive: `lanes / (workers * 16)` clamped to [2048, 16384].
  At a fixed 2048 the within-unit `seen` dedup let 45.6M raw rows through
  at f70 instead of 30.9M (the old whole-bucket call's figure); adaptive
  units bring it back to 29.4M with ~16 units per worker for balance.
- The kernel's `CELESTE_KERNEL_KEY_CHECK` gate moved from the call to the
  owner phase (`asm_kernel::key_check` per slot); same check, same
  environment variable.
- The `class` bucketing (already gone) is not coming back for this: the
  owner's pieces are per shape.

## The backward: one unit per cell, one barrier per iteration (2026-09-13)

The first parallel backward kept the phased shape of the serial one -
load every layer, filter, re-run as lane units, mark - with two barriers
and three serial passes per iteration. Philippe's version replaced it:
the unit is a (cell, all layers <= i); a worker gathers the cell's rows
from every mapped layer file (cell index + range copy), drops the rows
the read-only marked set already holds, runs them through the frame
step in backward mode against the read-only target set, and hands back
the hits. The one barrier per iteration applies the hits to the marked
set, in cell order; they are the next iteration's targets. All of a
cell's rows go through the kernel together (same collision tiles, same
few target cells), and nothing is shared for writing.

The incremental variant (marks with distances, only untested pairs
re-run) was built, measured exact against from-scratch, and REMOVED the
same day: it saved ~15 s of a 524 s run and did not fit the per-cell
structure. Non-incremental and simple wins.

Seeds are the win rows of every layer <= H, not layer H alone: a win
state reached at frame H is filed under the layer where the forward
FIRST reached it, which may be earlier.

## Measurements

Room (1,0), release, 7950X3D (16 cores / 32 threads). Level-0 forward
f0-f89: 791 s single-threaded -> 65.6 s at 32 threads (per frame at f70:
34.0 s -> 2.2 s); peak RSS 27 GB -> 12 GB with the visited set sharded
by shape. The full ladder to the optimum: 1 h 10 min -> 8 min 51 s at 32
threads before the incremental backward. Tables in BENCHMARK_DATA.md.
