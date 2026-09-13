# Memory: where a frame's bytes are, what the peak will be (2026-09-13)

Room (0,0)'s level-0 forward was OOM-killed at f88 (RSS 58 GB against the
60 GB cap) before reaching its horizon. This is the accounting behind
that, the options for the frame's structure compared, and the estimate
of the peak at the worst frame. Everything measured is release, 16
threads (one per physical core), the `[fwd]` line's own instrumentation
(`in` = the input frontier's row bytes, `sinks` = the workers' slot
capacity at its peak, `visited` = the shards' capacity, `rss` = the
process at the frame's start / after emit / after own).

## What lives where

One level-0 frame has three kinds of memory:

- **Persistent across frames** (grows with the search):
  - the VISITED set: every distinct `(shape, cell, key)` ever reached,
    the door's dedup. `FxHashSet<(u64,u64)>` per `(shape, cell)` shard,
    measured 24.8 B/entry. Grows by the kept rows of every frame.
  - the FRONTIER: the current frame's rows, 136 B/lane at room (0,0)
    (the varying columns of a level-0 row). Twice at the end of a frame:
    the input and the next pieces are both alive until `forward_frame`
    returns.
  - the pos graph, the kernels: negligible.
- **Transient within a frame**:
  - the EMITTED ROWS in the workers' slots between emit and own: the raw
    fan-out, ~8.4 rows per input lane at room (0,0) f90 (85 per lane at
    room (1,0) f70 - the number is the shape mix), ~95 B each. Before
    batching, the whole frame's fan-out was held at once.
  - the checkpoint's sorted copy of each piece (frontier-sized, per
    block in parallel), the per-thread kernel scratch (MBs).
- **Allocator retention**: what the allocator keeps resident beyond what
  is live. Under glibc this was the LARGEST term.

## Measured (room (0,0), level 0)

| frame | frontier lanes | visited entries | raw rows | in GB | sinks GB | visited GB | RSS glibc, unbatched | RSS glibc, batched | RSS mimalloc, batched | ms |
|---|---|---|---|---|---|---|---|---|---|---|
| f60 | 5.69M | 48.5M | 45.6M | 0.77 | 5.3 | 1.2 | | 9.2 | 8.8 | 4.9 s |
| f70 | 8.11M | 124M | 64.8M | 1.10 | 6.1 | 3.1 | | 23.8 | 14.0 | 7.0 s |
| f80 | 10.4M | 217M | 78.4M | 1.41 | 6.0 | 5.4 | 39.4 (32 thr, 12.6 s) | 35.8 | 16.1 | 7.4 s |
| f84 | 11.8M | 263M | 89.9M | 1.61 | 6.6 | 6.5 | 49.2 | 38.7 | | 7.7 s |
| f87 | 14.1M | 305M | 111M | 1.91 | 7.4 | 7.6 | 58.3, killed at f88 | 41.0 | | 8.2 s |
| f90 | 17.2M | 356M | 144M | 2.34 | 8.0 | 8.8 | - | 44.1 | 24.8 | 9.5 s |

Three findings:

1. **The transient fan-out was the killer, and batching bounds it.**
   Unbatched, f84 held 10.5 GB of raw rows against a 1.6 GB frontier and
   a 6.5 GB visited set. Batched (`CELESTE_EMIT_BUDGET_GB`, default 4,
   plans/parallel.md "Batches"), the slots peak at ~6-8 GB of CAPACITY
   whatever the frame (the budget counts data; Vec doubling makes the
   capacity up to 2x - counting capacity instead would make the peak the
   budget exactly, a knob if it matters).
2. **glibc was retaining ~23 GB.** At f90 the live terms sum to ~21 GB
   (2.3 in + 2.5 next + 8.0 sinks + 8.8 visited) and glibc's RSS was
   44 GB; mimalloc's is 24.8 GB at the same speed (9.51 vs 9.45 s). The
   pattern is the classic one: short-lived slot chunks interleaved with
   long-lived visited nodes across 16 arenas, so freed chunks never
   return. `mimalloc` had been a dependency all along and was not the
   global allocator; it is now (`src/bin/rewrite.rs`).
3. **The batched frame is faster**: f80 12.6 s at 32 threads before,
   7.3 s at 16 threads after. Slot capacity is reused across batches
   instead of ~13 GB/frame of fresh pages being faulted in and doubled
   into. Not yet an A/B at equal thread counts; treat as provisional
   until BENCHMARK_DATA.md has one.

The accounting under mimalloc is tight: RSS at a frame's start = frontier
+ visited + 2-5 GB (mimalloc's own retention plus the checkpoint's
buffers), and the frame adds the slots' capacity on top.

## The frame's structure: options compared

The work of a frame is a partitioned hash join: emit (kernel, ~80% of
the frame at f90, dynamic units, 2% idle), then own (filter + door +
append, ~10%, one thread per owner, 12-15% idle), a checkpoint (~8%).
Memory bandwidth is not the constraint: at f90 the frame moves ~45 GB
(raw rows written and read once, 144M random 16 B visited probes, the
frontier and pieces and the checkpoint's sorted copy) in 9.5 s, ~5 GB/s
against the machine's ~60+. The kernel is. So an option is judged on
the transient it holds, the barriers/imbalance it adds, what it does to
the WITHIN-CALL DEDUP WINDOW (a kernel call dedups its own emissions;
2048-lane calls emitted 46M raw rows where 16k-lane calls emit 29M at
room (1,0) f70 - smaller calls mean more rows for the owners), and its
complexity.

| | transient | barriers / frame | imbalance | dedup window | complexity | verdict |
|---|---|---|---|---|---|---|
| A. one emit, one own (before) | the whole fan-out: 10-14 GB at f84-f90 and growing with the frontier | 1 | emit 2%, own ~10% | 16k lanes | exists | killed at f88 |
| B. batched emit/own (now) | budget (4 GB data, ~8 GB capacity) | 2 per batch, 2-3 batches at f90 | same as A | 16k lanes (the grab, sized to the budget) | +60 lines, gate-identical | DONE |
| C. lockstep per input cell: all workers split one (shape, cell)'s lanes, barrier, next cell; an owner finalizes an output cell once its predecessors are done | ~one cell's fan-out, negligible | one per input cell: ~7k at room (0,0) (~0.3 s of barrier time, tolerable) | bad: 2.4k lanes per cell on average split 16 ways is 150 lanes, ~10 kernel slices, per worker per barrier | 150 lanes: raw rows up 2x or more, so the owners' work doubles | high: the "finalize an output cell" step needs the cell's predecessor set, which is the pos graph of THIS frame (only the previous frame's is known; a runtime guard would be needed), or a shared visited set with locks | more time for a memory saving B already gives; the locality it buys (the cell's collision tiles) is already there, rows are cell-sorted within a block |
| D. streaming: owners consume from bounded per-owner queues while emit runs, no mid-frame barrier | budget-sized queues | 1 | best: own overlaps emit | 16k | medium: every thread emits AND owns, back-pressure without deadlock (drain before push) | hides the own phase, <= 10% of a frame; not now |
| E. two-pass: emit only (key, cell) (20 B/row), own decides the kept set, re-run the kernel materializing the kept 12% | fan-out / 4.5 | 2 | as A | 16k | medium: the kernel needs a "materialize these lanes' rows" mode | kernel runs twice; roughly a wash on time, and B bounds the transient for free |

B is the choice: the smallest change, the same frame when it fits, and it
composes with D later if the own phase ever matters.

## The peak: what the worst frame will cost

The worst frame is the LEVEL-0 FORWARD AT THE HORIZON. Levels >= 1 run
under the coarser level's marks (`MarkFilter`) and are far smaller (room
(1,0): 7857 marked states at level 0 against a 4-7M frontier); the
backward maps the layers read-only and holds marks and targets, tens of
MB. Under B + mimalloc:

    RSS(f) ~= 136 B x F(f)       frontier (x2 briefly at the frame's end)
            + 24.8 B x V(f)      visited
            + ~8 GB              slots (budget 4 GB data, capacity ~2x)
            + 2-5 GB             allocator + checkpoint buffers

with F the frontier lanes and V = sum of kept rows so far. Room (0,0)'s
F grows 5.2%/frame over f80-f90 and is accelerating (x1.28 over f70-f80,
x1.66 over f80-f90); room (1,0)'s was flat at 4-7M. Extrapolating the
f80-f90 rate:

| frame | F | V | frontier | visited | RSS estimate |
|---|---|---|---|---|---|
| f90 (measured) | 17.2M | 356M | 2.3 GB | 8.8 GB | 24.8 GB |
| f100 | ~29M | ~590M | 3.9 | 14.6 | ~31 GB |
| f110 | ~47M | ~970M | 6.4 | 24 | ~43 GB |
| f120 | ~78M | ~1.6G | 10.6 | 40 | ~64 GB |

Where the horizon is: the level-0 win came at f79; room (1,0)'s
coarse-to-exact gap was 10 frames (f89 -> 99); the community 100m TAS
has 91 inputs after the spawn (the spawn is 24 frames here, but the TAS
did not replay to an exit under our alignment, so it is a hint, not a
horizon). Plan for H in 90-115: **f110 fits the 60 GB cap with ~15 GB
to spare, f120 does not.** The machine has 125 GB; the cap is a safety
choice (/tmp is a tmpfs), so there is headroom in the cap itself before
any new technique.

The frame's TIME is the other side: ~9.5 s at f90, ~16 s at f100, ~27 s
at f110 - the level-0 forward to f110 is ~10 minutes. The ladder then
re-runs the level-0 backward per horizon step (25 s at room (1,0) H=99;
room (0,0) has 3x the states and 2.8x the per-row kernel cost), so
20-35 horizon steps is where the hours would go, not the memory.

If the estimate is wrong on the high side, the levers in order of cost:

1. Count slot CAPACITY in the budget (peak slots 8 -> 4 GB): a one-line
   change.
2. The visited entry: 24.8 B -> 16-17 B with a denser table (the key is
   16 B; hashbrown's average load is what costs the rest): -30% of the
   biggest term, ~100 lines.
3. Raise the cap toward what `free` leaves after /tmp.
4. Fewer states: the per-position / concrete-move kernels (the census
   work) shrink everything by shrinking the state count - the real
   project, not a memory fix.
