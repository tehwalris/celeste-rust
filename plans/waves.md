# Waves: the frame as one pass with flush-on-full queues (design, 2026-09-13)

The successor to the batched two-phase frame (plans/parallel.md,
plans/memory.md): the design with its boundaries, the reasons each piece
is shaped the way it is, what it costs, and the order it was built in
(every step gate-checked). Steps 1-2 are IN (2026-09-13, "What landed"
at the end); step 3 follows.

## What it replaces and why

Today a frame is EMIT (all workers, into per-owner slots) / barrier /
OWN (one thread per owner: filter, hash-set door, append), repeated per
batch when the fan-out exceeds a byte budget. It works, and the
measured problems are small but structural:

- the transient is "a batch" (4-8 GB) rather than "what is open";
- the visited set is a hash set per (shape, cell) at 24.8 B/entry with
  random DRAM probes at the door (~115 ns per raw row; 1 s of a 9.5 s
  frame at room (0,0) f90, and it is the term that grows fastest: 8.8 GB
  at f90, ~24 GB at f110);
- two barriers per batch and a static owner partition (own idle 12-15%);
- three concepts that exist only to make the split work: owners, the
  owner hash, the byte budget.

The new frame has one pass, no owners, no budget. Inputs are consumed in
SPATIAL order (a wave through the room); outputs go into small fixed
queues keyed by (shape, cell); a queue that fills, or that must be
evicted, is flushed BY THE WORKER THAT HOLDS IT: sort, dedup against the
cell's shard of the door, append the survivors into the worker's piece.
The door's shard is a sorted array with a per-frame delta, merged once
at the end of the frame.

The one fact that makes it simple: **a flush is idempotent.** The door
is a set. A cell flushed early and touched again later is just flushed
again - the second flush dedups against the first's entries and appends
again. So nothing needs to know when a cell is "done", no predecessor
sets, no pos-graph knowledge, no double buffering with a fallback lock.
The eviction policy only affects HOW MANY flushes happen, never what the
frame computes.

## The three levels of dedup, and what each one writes

Conceptually the set we dedup against is "every state reached up to and
including this frame". Structurally it is `Door`: (shape, cell) at the
top, a sorted list of 128-bit keys below (the state itself is not stored
here; it lives in the checkpoint layer of the frame that first reached
it, and 128 bits is enough not to double-check). It is NOT split per
frame. What is split per frame is the `delta` - and the point of the
design is that during a frame the big set is only READ:

| level | where | dedups against | writes |
|---|---|---|---|
| 1. the kernel call's `RowCache` | inside one 16k-lane call | the call's own emissions (lossy cache) | nothing shared; drops ~90% of re-emissions before they reach a queue |
| 2. a queue flush | the worker holding the queue, shard lock held for `admit` only | itself (sort, adjacent duplicates), then `base` (every earlier frame, read-only) and `delta` (this frame's earlier flushes of this cell) | appends the new keys to the small `delta`; the new ROWS go to the worker's piece |
| 3. end of frame | one parallel pass over the shards, plus the pieces' merge into the canonical frontier | - | `base = merge(base, delta)`: the one insertion into the big set, sequential, once per frame |

Why a flush must read `base` rather than only dedup within itself: ~88%
of emitted rows are old, and a flush that cannot tell has to KEEP them
until the end of the frame - that is the whole-fan-out transient the
batching exists to avoid. Reading `base` is what makes discarding them
at flush time possible, and it is cheap precisely because `base` is
immutable during the frame: a sequential sweep of an L2-resident array,
no insertion, no rehash. Why a flush must also read `delta`: the same
state emitted twice in one frame from different inputs at different
times would otherwise become two frontier rows (the same layer holding
a state twice, run twice next frame, re-run twice by the backward) -
not wrong as a set, but waste that compounds.

Today's frame does levels 2 and 3 as ONE thing: the owner phase inserts
straight into the hash set per batch, i.e. it writes the big set
continuously, with a random DRAM probe per row. Level 1 is the same
`RowCache` as today. So the structure is already "temporary lists, then
the door"; what changes is that the door's cross-frame part becomes
read-only within a frame and its this-frame part becomes a small sorted
delta, and that the temporary lists are per output cell instead of per
owner.

## The invariants

1. **The frontier is cell-sorted, one run of chunks per shape.** After a
   frame, the workers' pieces of a shape are sorted by (cell, key) and
   k-way merged into ONE cell-sorted sequence, cut into chunks of at
   most `CHUNK_LANES` (~1M) lanes. This is canonical (a function of the
   set, not of scheduling), the checkpoint writes it as-is (one file per
   chunk, already sorted - the per-piece sort the checkpoint does today
   moves here), and a cell's rows are ONE contiguous range in ONE file
   (the backward's `rows_of_cell` best case, instead of 16 owner files).
2. **Units are contiguous lane ranges of a chunk, pulled in order.** The
   unit list is all chunks' ranges sorted by first cell across shapes.
   Workers pulling consecutive units ARE the wave; a grab of consecutive
   units is a contiguous range of one chunk (the kernel's within-call
   dedup window stays 16k lanes - with owner-hashed blocks it would have
   collapsed to 2048, +50% raw rows). No adaptive grab sizing: a fixed
   `GRAB_LANES` (16384) since nothing downstream needs bounding.
3. **A queue holds rows of one (shape, cell) in the shape's skeleton.**
   The skeleton is the union of the shape's outcome templates' varying
   cells (see "the kernel side"), so every queue of a shape has the same
   columns as the shape's piece and the flush appends column-for-column.
4. **The door is the only shared mutable state, and only under a
   per-shard lock at flush time.** Pushes touch nothing shared.

## The pieces and their boundaries

### `search::door::Door` (new module, ~200 lines, unit-tested alone)

```
pub struct Door { shards: RwLock<FxHashMap<(u64 shape, u32 cell), Arc<Mutex<Shard>>>> }
struct Shard { base: Vec<(u64,u64)>, delta: Vec<(u64,u64)> }   // both sorted

impl Door {
    /// keys: sorted, deduped. Returns the indices of the keys NOT in the
    /// shard (base or delta) and adds them to delta. One lock, one
    /// merge-join sweep over base and delta.
    pub fn admit(&self, shape: u64, cell: u32, keys: &[(u64,u64)], new: &mut Vec<u32>);
    /// End of frame: every shard's delta merged into its base, in
    /// parallel over shards. O(total entries) sequential bytes, once.
    pub fn end_frame(&self, workers: usize);
    pub fn len(&self) -> usize;  pub fn alloc_bytes(&self) -> usize;
}
```

- Outer map: read-locked for the lookup; write-locked only to create a
  shard (misses stop after the early frames). `Arc` so the read lock is
  dropped before the shard lock is taken.
- `admit` is a merge-join: the queue's keys are sorted by the flusher
  (which also collapses in-queue duplicates), so the sweep over `base`
  is sequential and `base` is L2-resident after its first touch
  (~17k entries, ~270 KB at room (0,0) f90). No random probes.
- `delta` is small (a cell's new keys this frame) and is searched the
  same way. It is NOT merged per flush: a cell is flushed ~5 times a
  frame at f90 and re-merging 270 KB each time is ~28 GB of memmove per
  frame; merging once at `end_frame` is O(V) per frame (~11 GB at f90,
  ~0.3 s wall over 16 threads) whatever the flush count.
- 16 B/entry, no load factor, no control bytes: visited at f110 ~15.5 GB
  instead of ~24. `end_frame`'s transient is one shard's `base + delta`
  per thread.
- `Visited` (hash set, `save/load/fingerprint/contains/insert`) STAYS
  for the backward's marks and `MarkFilter`: those need single-key
  insert/contains and serialization, not batch admission, and the
  backward is untouched by this design.
- Test: `Door` vs a `Visited` on random (shape, cell, key) streams with
  repeated flushes and frames - identical admitted sets, identical
  counts.

### `frame::Queue`, `frame::QueuePool` (the `Slot` reshaped, ~150 lines)

```
pub struct Queue { shape: u64, cell: u32, skeleton: Rt2 /* shared per shape */,
                   cols: Vec<(usize, TCol)>, keys: Vec<(u64,u64)>, cap: usize, touched: u32 }
struct QueuePool { queues: Vec<Queue>, index: FxHashMap<(u64,u32), u32>, free: Vec<u32>,
                   last: ((u64,u32), u32) /* the run cache */, clock: u32 }
```

- Fixed pool per worker (`POOL_QUEUES`, ~256) of fixed capacity
  (`QUEUE_ROWS`, ~4096): a queue never reallocates, the transient is
  `workers x POOL_QUEUES x QUEUE_ROWS x row bytes` (~1.7 GB at ~100 B
  rows) by construction, and it is allocated once per level and reused
  every frame (no page faults, no doubling copies - the arena discussion).
- `pool.get(shape, cell) -> &mut Queue`: the run cache first (rows arrive
  in runs of one cell), then the index; on a miss, a free queue or the
  least recently touched one (second-chance clock, no linked list) after
  flushing it. Eviction is `touched`-based because the wave makes the
  oldest cell the one least likely to be touched again.
- The queue keeps NO owner, NO template id: the skeleton is the shape's.

### `frame::ForwardSink` (the worker's context, ~150 lines changed)

```
pub struct ForwardSink<'a> {
    // forward mode
    pool: QueuePool, door: &'a Door, filter: Option<&'a MarkFilter<'a>>,
    pieces: FxHashMap<u64, Rt2>, won: bool, edges: FxHashSet<(u32,u32)>, emitted: u64,
    flushes: u64, flushed_rows: u64,
    // backward mode, unchanged: targets: Option<&'a TargetSet>, hits, hit_base
}
impl ForwardSink {
    pub fn queue(&mut self, shape, cell, init: impl FnOnce() -> Rt2) -> &mut Queue;
    /// Called by the emitter after a push; flushes when full.
    pub fn pushed(&mut self, q: u32);
    fn flush(&mut self, q: u32);      // sort keys -> filter -> door.admit -> gather into piece -> any_win
    pub fn finish(mut self) -> (Vec<Rt2> pieces, bool won, stats);   // flushes every queue
}
```

- `flush` is the old owner loop for one queue: `filter.allowed(&q.to_rt2())`
  at levels >= 1 (unchanged interface), `door.admit`, `gather_into` the
  worker's piece of the shape (same skeleton -> direct column extends,
  no `col_push` adaptation), the win check. It runs on the worker that
  holds the queue, with the shard lock held for `admit` only.
- The emitter contract (both engines): every row of a shape is pushed
  with the SAME skeleton (the shape's union), asserted in debug builds.
- Lock cost, room (0,0) f90: 144M rows / 4096 per flush ~= 35k flushes
  per frame x ~200 us (sort 4k keys, sweep, gather) = 7 s of thread time
  over 21k locks and a ~7.6 s frame - contention is negligible even with
  all 16 workers in one band of the room, so the wave can be one wave.

### `frame::forward_frame` (~100 lines, replacing ~230)

```
units = sorted lane ranges of the frontier's chunks (by first cell)
workers pull grabs -> engine.run(chunk, cells, range, &mut sink)   // no batches, no budget
sinks.finish() -> pieces per worker
door.end_frame()
frontier' = per shape: sort each piece by (cell, key), k-way merge, cut into chunks
edges -> pos observer; stats
```

One barrier per frame (the end). `FrameStats` drops emit/own/batches for
`flushes`, `rows per flush`, `merge ms`; the `[fwd]` line follows.

### The kernel side (`compiled::asm_kernel`, ~80 lines)

- The registry computes, per output SHAPE, the union of the varying
  cells over every template of that shape across all kernels
  (`Registry::skeleton(shape) -> &Rt2`). `AccTemplate` gets, per union
  column the template holds uniform, the value to write per row
  (`uniform: Vec<(col, raw)>`); `BodyCols::of(body, skeleton)` maps roots
  to union columns; `push_row` writes roots + uniforms.
- `run`: `owner_of` and the `slot_ids[outcome][owner]` table go;
  per emitted row: `let q = sink.queue(shape, cout, || skeleton.clone())`
  (the run cache makes this one compare in the common case), `push_row`,
  `sink.pushed(q)`.
- Backward mode: identical to today (targets/hits only).
- The reference engine's `emit_row`: its skeleton rule (every
  Num/Ival/Bool cell typed) is already a per-shape constant; unchanged.

**The one number to measure before building this part:** the union
skeleton's row width vs the per-template width today (~95 B). If a
shape's templates make many different cells uniform, the union widens
every row (transient and push cost). Step 3 below builds queues keyed by
(template, cell) first, which needs no union, so this is measured on the
real thing rather than estimated.

### `ForwardState`

`visited: Vec<Visited>` -> `door: Door`, `pools: Vec<QueuePool>` (kept
across frames), frontier as chunks. `checkpoint_frontier` loses its
sort. `visited_len()` -> `door.len()`. Everything above it (`Ladder`,
`find_optimum`, the backward, the UI export) is untouched.

## What it does to the numbers

- Transient: ~1.7 GB fixed (was 4-8 GB by budget), all of it allocated
  once and reused.
- Visited: 16 B/entry (was 24.8): 15.5 GB at room (0,0) f110 (was ~24).
- Frontier: unchanged (136 B/lane); in + next both alive at the frame's
  end. Chunks make "free behind the wave" possible later (a chunk whose
  units are done is dropped; next-frame pieces grow as it goes), which
  would make it max(in, next): -2.3 GB at f90, -6 GB at f110. Optional.
- f110 estimate: 15.5 + 6.4 + 1.7 + ~2 ~= 26 GB (batched + mimalloc: ~43).
- Time: the own phase (10-13% of a frame, half of it barrier idle)
  becomes ~6% of inline flush work with no barrier; the door's random
  probes become sequential sweeps; the end-of-frame merge and the k-way
  merge of pieces add ~0.4 s at f90. The kernel + push loop (64%) is
  untouched, so expect "a bit faster", not a step; the design is for
  the memory and for removing three concepts, not for speed.
- Determinism: the frontier is canonical (invariant 1), so the unit
  partition, and therefore even the raw counts, no longer depend on the
  thread count. Gates (ckhash/posgraph/marks) are sets and must be
  identical throughout.

## Build order (each step gate-checked, committed alone)

1. `Door` + its equivalence test. No behaviour change.
2. Invariant 1: merge the pieces into cell-sorted chunks per shape at
   the end of the frame; checkpoint per chunk without re-sorting; units
   sorted by first cell. Still the batched two-phase frame. Gates
   identical; raw counts change (window now always 16k) and become
   thread-count-independent.
3. Queues + flush + `Door` replace the own phase, owners, batches, the
   budget; queues keyed by (template, cell) using today's `BodyCols`.
   Gates identical. Measure: flushes/frame, rows/flush, RSS, ms.
4. Union skeleton per shape; queues keyed by (shape, cell). Measure the
   row width; gates identical.
5. Optional: free chunks behind the wave.

## Open questions (decide by measurement in steps 3-4)

- `POOL_QUEUES` / `QUEUE_ROWS`: the wave's live output-cell set per
  worker vs the pool; the log's flushes/frame and rows/flush say whether
  evictions are flushing near-empty queues (pool too small) or the
  transient is larger than it needs to be (too big).
- Whether `delta` is worth having over merge-per-flush: the memmove math
  says yes at f90 and more so later; step 1's test covers both.

## Prototype results (2026-09-13, `rewrite dump-emissions` / `rewrite bench-door`)

The tiers on real data without the kernels: room (1,0) f80's emission
stream (23.3M post-`RowCache` rows from a canonical cell-sorted
frontier - 5.5 per input lane, against 30.9M from owner-hashed blocks:
invariant 1 alone buys the full 16k dedup window), the door preloaded
with layers 0..=80 (115M entries), 16 threads, `--profile quick`. The
admitted count equals the real f81's 3,912,529 rows in every
configuration (the check caught one pool bug: an evicted queue handed
out while still on the free list).

| door / queues | wave wall | thread-ms push / sort / admit / gather | end_frame | transient | door B/entry |
|---|---|---|---|---|---|
| hash sets, 256 x 4096 rows, one wave | 378 ms | 1095 / 703 / 1954 / 711 | 0 | 1.95 GB | 24.9 |
| sorted + gallop, 256 x 4096, one wave | 485 ms | 1266 / 727 / 3505 / 499 | 134 ms | 1.95 GB | 16.0 |
| sorted + bucket index, 256 x 4096, regions | 413 ms | 1003 / 710 / 1912 / 541 | 147 ms | 1.95 GB | 16.7 |
| sorted + bucket index, 256 x 128, regions | 258 ms | 353 / 389 / 2356 / 147 | 166 ms | **60 MB** | 16.7 |
| + prefetch, 256 x 128, regions | **217 ms** | 555 / 397 / 1642 / 184 | 160 ms | 60 MB | 16.7 |
| same, payload 0 (keys only) | 145 ms | 260 / 389 / 1195 / 15 | 173 ms | | |
| same, 1 thread | 1563 ms | 226 / 361 / 904 / 72 | 442 ms | 3.8 MB | |

What the prototype decided:

- **Small queues, not big ones.** 128-256 rows per queue beat 4096 by
  1.9x on wall time: the per-worker pool stays cache-resident (3.8 MB
  instead of 120 MB), so pushes stop paying a read-for-ownership miss
  per line, and sort/gather shrink with the batch. The transient of the
  whole frame is 60 MB. The "arena" question is moot at this size.
- **The sorted shard needs an index.** A merge-join or gallop over a
  ~19k-entry base costs more than a hash probe when the batch is ~100
  keys: it touches the whole shard per flush. A bucket index by the top
  bits of `key.0` (~8 entries per bucket, 0.5 B/entry) plus software
  prefetch of the next 8 keys' buckets makes a lookup one index word and
  one line, overlapped: admit 3505 -> 1642 thread-ms, below the hash
  door's 1954 at 16.7 B/entry instead of 24.9.
- **Regions, not one wave.** Worker w taking the w-th contiguous run of
  units (its own band of the room) instead of all workers pulling the
  next unit: 20% less admit time (shard locks no longer collide) and
  fuller flushes (990 vs 844 rows at 4096-row queues). Balance suffers
  (busy 58-80%); a hybrid (regions first, then stealing) is the obvious
  fix if it matters.
- **The wave's shape is set by the cell order.** A cell receives its
  rows from ~9 predecessor cells that a (cell-id)-sorted frontier visits
  in ~3 separate bursts (the neighbours in the other axis are a column
  away), so a cell is flushed ~3-5 times a frame whatever the pool
  size; a Morton/Hilbert cell order would make it ~2. Not needed now.
- **Cost per row, all-in** (queue push + 96 B payload, sort, door,
  gather): ~120 ns at 16 threads (2.8 thread-s / 23.3M), against
  today's own phase alone at ~130 ns/row (256 ms wall for 30.9M rows)
  PLUS the slot pushes inside `AsmKernel::run`. So the tiers are at
  worst a wash on time with ~30x less transient and -33% door memory,
  and the frame's cost stays where it was: the kernel.
- `end_frame` is 160 ms for 115M entries (a sequential merge + index
  rebuild per shard, parallel): ~1.4 ns/entry, so ~1.3 s at room (0,0)
  f110's ~1G entries, against a ~27 s frame.

## What landed (2026-09-13)

Steps 1 and 2 are in (`f8834cb`, `44559b0`), with one deviation from
the design above: **there is no cross-piece merge.** The canonical
frontier is the workers' pieces, each sorted by (cell, key) by the
checkpoint (which it did already); a cell's rows may sit in several
pieces. The merge into "one cell in exactly one chunk" was built first
and cost ~250 ms per frame at f70 through the generic `append_rows` /
`gather_lanes` (12% of the frame) to save ~50 ms of queue pushes: the
door's delta already catches every cross-piece duplicate within the
frame, so the merge only ever saved push work. The units are still
pulled in cell order across pieces (the wave), and the kernel's window
is still 16k lanes.

Room (1,0) f0-f70, 16 threads, release, gates identical:

| | f0-f70 | peak RSS | f70 frame | f70 raw rows | f70 transient | f70 door |
|---|---|---|---|---|---|---|
| two-phase + batches (before) | 25.5 s | 9.45 GB | 1984 ms (emit 1452, own 283, ckpt 218) | 41.3M | 4.94 GB slots | 1.62 GB hash sets |
| waves (steps 1-2) | 25.4 s | **4.08 GB** | 1907 ms (wave 1601, door 72, ckpt 207) | 28.9M | 0.08 GB queues | 1.11 GB |

Same speed, 43% of the memory. The wave is kernel-bound as before; the
flushes run at ~180 rows each (162k per frame at f70).

Room (0,0) f0-f90 (the run that was OOM-killed at f88 under glibc and
needed 24.8 GB batched under mimalloc): **14.15 GB peak**, 277 s, the
same pos-graph fingerprint; at f90 the frame is 10.1 s (wave 8.8 s,
door 0.35 s, ckpt 0.8 s) against 9.5 s two-phase, door 5.96 GB against
8.8 GB of hash sets, queues 0.25 GB against ~8 GB of slots. The three
memory cuts compose: 44 GB (glibc) -> 24.8 (mimalloc) -> 14.2 (waves).
