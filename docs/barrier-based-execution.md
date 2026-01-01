# Barrier-Based Parallel Execution

## Overview

A new execution strategy that uses fast scalar interpretation with abstract path enumeration, synchronized at explicit barriers.

## Core Concepts

### 1. Scalar Interpretation with Path Enumeration

For each vector lane:
1. Extract the scalar state from the vector
2. Run through the scalar interpreter (fast, no symbolic tracking)
3. On abstract branches (UnknownBool), use PathCounter to pick true/false
4. On concrete branches, evaluate normally
5. Collect the output state
6. Increment PathCounter, repeat until all paths exhausted
7. Result: set of all reachable scalar states for that lane

This is essentially what we have now, but **without** symbolic expression tracking or caching attempts.

### 2. Parallelization

- Process vector lanes in parallel (each lane explores all its abstract paths)
- Limit parallelism to avoid memory explosion
- Can offload completed states to disk if needed

### 3. Barriers - The Key Innovation

Instead of running start-of-frame → end-of-frame in one go, we introduce **barriers** where all states synchronize.

At a barrier:
- All states that reached that barrier are collected
- States can be vectorized/merged (like current vectorize_states)
- Memory can be reclaimed
- Then execution continues to the next barrier

## Barrier Design (Clarified)

### Granularity
- Barriers are **inside CFGs**, similar to `hintNormalize`
- Can potentially reuse/replace the `hintNormalize` infrastructure
- Barriers mark specific blocks in the IR

### Barrier Identification
- **Explicit tuple** in the code: `__barrier({1, 2, 3})`
- The tuple provides a total ordering across all barriers

### Hit Count
- **Per-state**: each state tracks how many times IT has hit each barrier
- When State A hits barrier `(1,)`:
  - First time → key = `((1,), hit=0)`
  - Second time → key = `((1,), hit=1)`
  - Third time → key = `((1,), hit=2)`

### Implicit End Barrier
- There's an **implicit barrier at the very end** of execution
- States that skip earlier barriers (e.g., early return) wait at this end barrier

### Processing Order
- **Drain all hits of barrier N before moving to barrier N+1**
- Order: sort by (barrier_tuple, then hit_count)

```
Processing order:
1. ((0,), hit=0)  - all states at barrier 0, first hit
2. ((0,), hit=1)  - all states at barrier 0, second hit
3. ((0,), hit=2)  - all states at barrier 0, third hit
... (until no more states at barrier 0)
4. ((1,), hit=0)  - all states at barrier 1, first hit
5. ((1,), hit=1)  - all states at barrier 1, second hit
...
N. (END, hit=0)   - all states at implicit end barrier
```

## Concrete Example

```lua
function _update()
    for i = 1, 3 do
        __barrier({1})  -- barrier 1
        do_something()
    end
    __barrier({2})  -- barrier 2
end
```

State execution with ONE state S:

```
Initial: State S at start

S runs, hits barrier({1}) first time  → S waiting at ((1,), hit=0)
[Process ((1,), hit=0)]
S continues, hits barrier({1}) second time → S waiting at ((1,), hit=1)
[Process ((1,), hit=1)]
S continues, hits barrier({1}) third time → S waiting at ((1,), hit=2)
[Process ((1,), hit=2)]
S continues (loop done), hits barrier({2}) first time → S waiting at ((2,), hit=0)
[No more states at barrier 1, move to barrier 2]
[Process ((2,), hit=0)]
S continues to end → S waiting at (END, hit=0)
[Process (END, hit=0)]
Done!
```

## Design Decisions (Finalized)

### What happens at a barrier?

- **Normal barriers**: GC + Vectorize
- **End-of-frame barrier**: GC + Vectorize + Abstraction (special case, like current end-of-frame logic)

### Parallelization

- **Multiple lanes run simultaneously** (with parallelism limit)
- **Only ONE source barrier is processed globally at a time**
- Pick the lowest (barrier_id, hit_count), process ALL states waiting there in parallel
- Each lane explores all its paths (PathCounter enumeration) to completion

### PathCounter behavior

- **Resets at each barrier** (local to barrier segment)
- From one source barrier, a single vector lane may have multiple abstract paths to explore
- All paths' output states accumulate at their destination barriers
- When starting from the next barrier, PathCounter starts fresh

### hintNormalize relationship

- **Remove hintNormalize entirely**, replace with barriers
- Barriers are the new mechanism for both synchronization and vectorization

### State representation at barriers

```rust
struct WaitingState {
    state: State,           // The actual state
    barrier_id: Vec<i32>,   // Which barrier (tuple for ordering)
    hit_count: usize,       // How many times this state hit this barrier
    // NO PathCounter - it resets at each barrier
}
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Barrier Executor                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌───────────────────────────────────────────────────────┐ │
│  │  Pending States Map                                    │ │
│  │  Map<(barrier_id, hit_count), Vec<WaitingState>>      │ │
│  └───────────────────────────────────────────────────────┘ │
│                           │                                 │
│                           ▼                                 │
│  ┌───────────────────────────────────────────────────────┐ │
│  │  Select lowest (barrier_id, hit_count)                │ │
│  └───────────────────────────────────────────────────────┘ │
│                           │                                 │
│                           ▼                                 │
│  ┌───────────────────────────────────────────────────────┐ │
│  │  Vectorize states at this barrier point               │ │
│  └───────────────────────────────────────────────────────┘ │
│                           │                                 │
│                           ▼                                 │
│  ┌───────────────────────────────────────────────────────┐ │
│  │  For each vectorized state:                           │ │
│  │    For each scalar lane:                              │ │
│  │      Run scalar interpreter with PathCounter          │ │
│  │      Until next barrier OR path exhausted             │ │
│  │      Collect output states + their barrier destinations│ │
│  └───────────────────────────────────────────────────────┘ │
│                           │                                 │
│                           ▼                                 │
│  ┌───────────────────────────────────────────────────────┐ │
│  │  Insert output states into pending map                │ │
│  │  (keyed by their destination barrier + hit_count)     │ │
│  └───────────────────────────────────────────────────────┘ │
│                           │                                 │
│                           ▼                                 │
│              Repeat until pending map empty                 │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Execution Flow (Detailed)

```
1. Start with input states at implicit "START" barrier

2. Loop:
   a. Find lowest (barrier_id, hit_count) with waiting states
   b. Collect all states waiting at that barrier point
   c. Apply GC + Vectorize (+ Abstraction if end-of-frame)
   d. Split vectorized states into scalar lanes
   e. In parallel (with limit):
      For each scalar lane:
        - PathCounter = new()
        - Loop:
          - Run interpreter until next barrier OR path exhausted
          - Record output state + destination barrier
          - If path had forced choices:
            - Increment PathCounter
            - If more paths, continue loop
          - Else: done with this lane
   f. Insert all output states into pending map
      (keyed by destination barrier + updated hit_count)
   g. Repeat until pending map empty

3. Return states that reached END barrier
```

## Implementation Plan

### Phase 1: IR Changes
- Add `Barrier(Vec<i32>)` instruction to IR
- Remove `HintNormalize` from IR
- Update frontend to compile `__barrier({...})` calls

### Phase 2: Interpreter Changes
- Modify interpreter to yield at barriers (like current HintNormalize behavior)
- Return barrier destination along with output state
- Track per-state hit counts for each barrier

### Phase 3: Executor
- Implement `BarrierExecutor` that orchestrates the barrier-based execution
- Priority queue for (barrier_id, hit_count) ordering
- Parallelism limit for lane processing
- Integration with existing GC/vectorize/abstract infrastructure

### Phase 4: Lua Integration
- Add `__barrier` builtin function
- Place barriers in celeste-minimal.lua at strategic points
  - Inside object iteration loops
  - Between major game phases

## Current Implementation Status (COMPLETE)

The barrier-based executor is implemented and verified correct in `src/interpreter/barrier_executor.rs`.

### Key Implementation Details

1. **PathCounter Threading**: PathCounter passes through ALL function call levels via `interpret_call_with_path_counter()` and `run_cfg_to_barrier_or_completion()`.

2. **PHI Node Handling**: Must track `incoming_label` (source block) separately from `current_block_label` (current block). Uses fake `"__entry"` label for blocks entered from CFG entry point.

3. **Barriers Inside Function Calls**: When a barrier is hit inside a nested function call, execution yields back with a `CallStack` that records where to resume. The `ResumeContext` structure tracks:
   - `top_level_resume_block` and `top_level_instruction_index`: Where to resume in the top-level CFG
   - `call_stack`: Stack of `CallFrame`s representing suspended function calls

4. **Resume Context Keying**: States at the same barrier but with different resume contexts (e.g., called from different call sites) are kept separate and not merged. This is critical for correctness - states with different call stacks cannot be merged.

5. **Verified Correctness**: Produces identical state counts to flow-based executor:
   - Frame 25: 24, Frame 26: 204, Frame 27: 878, Frame 28: 2864, Frame 29: 7260, Frame 30: 15250

### Performance Status

Barrier-based is currently **slower** than flow-based at larger frames due to the overhead of path enumeration:

| Frame | Barrier | Flow | Ratio |
|-------|---------|------|-------|
| 27 | 1.1s | 0.8s | 1.4x slower |
| 28 | 5.9s | 2.7s | 2.2x slower |
| 29 | 25s | 4.6s | 5.4x slower |
| 30 | 107s | 10.4s | 10x slower |

The barrier-based approach is slower because:
1. **Exponential path enumeration**: PathCounter enumerates all 2^n paths for n UnknownBool branches
2. It processes barriers in strict order, which serializes some work
3. There's overhead from tracking and cloning ResumeContext/CallStack structures
4. States with different resume contexts can't be merged, reducing vectorization benefits

The gap widens at higher frames because the number of paths grows faster than the flow-based approach can handle:
- Frame 28: 40,942 paths (67% dedup rate)
- Frame 29: 173,076 paths (68% dedup rate)
- Frame 30: 463,994 paths (68% dedup rate)

### Optimizations Applied

1. **Batched Intermediate Vectorization**: Vectorize accumulated states every 64 paths during enumeration. This merges duplicate states early, reducing total path count by ~70% (41000→12600).
2. **Parallel Lane Processing**: All lanes processed in parallel via rayon.
3. **Fast State Normalization**: `normalize_gc_state_for_comparison()` avoids redundant clone+GC.
4. **FxHashSet for Deduplication**: Uses faster hash function than SIP.
5. **Early Deduplication**: States are deduplicated as generated, not batched.
6. **FxHasher for All im::HashMap Instances**: LocalEnv, Heap's new_values overlay, and global_env all use FxHasher instead of SipHash. This reduced Frame 28 time from 3.1s to 2.1s (~32% faster).
7. **Optimized Lane Extraction**: `extract_at_index` directly extracts single lane values instead of filter_by_mask.
8. **Heap::from_values**: Build heaps directly from Vec<Option<HeapValue>> to avoid im::HashMap insert overhead.
9. **Heap Freezing**: Freeze heaps after vectorization to move values from im::HashMap overlay to Arc<Vec> for O(1) access.
10. **Static Labels**: Use LazyLock for frequently-used labels and barrier IDs to avoid repeated allocations.
11. **jemalloc Allocator**: Use jemalloc instead of glibc malloc to reduce memory allocation overhead (~10% improvement).
12. **FxHashMap for GC and ObjectTable**: Use FxHashMap in state GC and ObjectTable operations for faster hashing.

## Expected Benefits

1. **Memory control**: Vectorization at barriers reduces state explosion
2. **Parallelization**: Process lanes in parallel within memory limits
3. **Simplicity**: Single mechanism replaces flow algorithm + hintNormalize
4. **Flexibility**: Barrier placement can be tuned for performance
