# Agent Prompt: Celeste Rust Interpreter - Speed Optimization

You are working on a Rust abstract interpreter for PICO-8 Celeste. The goal is running the **100m room** (first room of Celeste Classic) correctly and fast.

## Current State

**Read first**: `docs/barrier-based-execution.md` (especially "Current Implementation Status" section)

The barrier-based executor is **implemented and verified correct**. It produces identical state counts to the flow-based executor:
- Frame 25: 24, Frame 26: 204, Frame 27: 878, Frame 28: 2864

**The problem**: It's much slower than flow-based due to exponential path enumeration:
- Flow-based Frame 28: 1090 states before merge (4.85s)
- Barrier-based Frame 28: 41000 states before merge (257s)

## Your Task: Optimize Barrier-Based Execution

Key optimization opportunities:

1. **Early Path Merging**: Currently states are only merged at barriers. Merge during PathCounter enumeration when states become identical (same heap, local_env structure after gc).

2. **Parallel Lane Processing**: Currently vector lanes are processed sequentially. Use rayon to process lanes in parallel.

3. **Path Pruning**: Skip paths that would produce duplicate states. Hash states after each path segment.

4. **Caching**: Cache function call results when inputs are identical.

The flow-based approach achieves efficiency by working with vectorized states throughout. The barrier approach enumerates scalar paths which explodes exponentially. Find ways to reduce this explosion while keeping the barrier architecture.

## Reference State Counts

| Frame | Expanded | Flow Time | Target |
|-------|----------|-----------|--------|
| 1-24  | 1        | ~1ms      | <1ms   |
| 25    | 24       | 15ms      | <20ms  |
| 26    | 204      | 157ms     | <200ms |
| 27    | 878      | 1.1s      | <1.5s  |
| 28    | 2864     | 4.6s      | <6s    |

## Key Files

- `src/interpreter/barrier_executor.rs` - The barrier executor (optimize this)
- `src/interpreter/flow.rs` - Flow-based executor (for comparison/inspiration)
- `src/interpreter/vectorize.rs` - State vectorization/merging
- `src/interpreter/state.rs` - State with gc() method

## How to Test

```bash
# Run barrier-based (what you're optimizing)
./safe-run.sh -- cargo run --release --bin celeste-rust -- --barrier -n 28

# Run flow-based (baseline to beat)
./safe-run.sh -- cargo run --release --bin celeste-rust -- -n 28
```

Verify correctness: expanded state counts must match between both modes.

## Success Metric

Get barrier-based execution to within 2x of flow-based time while maintaining correct state counts.

## How to Work

1. Profile to understand where time is spent
2. Implement optimizations incrementally
3. Verify correctness after each change (state counts must match)
4. Commit working improvements often

This branch is yours - commit frequently.
