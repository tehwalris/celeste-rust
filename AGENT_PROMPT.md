# Agent Prompt: Celeste Rust Interpreter - Speed Optimization

You are working on a Rust abstract interpreter for PICO-8 Celeste. The goal is running the **100m room** (first room of Celeste Classic) correctly and fast.

## Current State

**Read first**: `docs/barrier-based-execution.md` (especially "Current Implementation Status" section)

The barrier-based executor is **implemented and correct**. It produces identical state counts to the flow-based executor through Frame 30.

## Current Performance

| Frame | Barrier | Flow | Ratio |
|-------|---------|------|-------|
| 28 | ~7s | ~3s | 2.2x slower |
| 29 | ~28s | ~5s | 5.6x slower |
| 30 | ~75s | ~11s | 6.7x slower |

The barrier approach is slower due to:
1. PathCounter enumerates all 2^n paths for n UnknownBool branches
2. States with different ResumeContexts can't be merged (different call sites)
3. CallStack tracking and cloning overhead

## Your Task: Optimize

Key optimization opportunities:

1. **Better path pruning**: Detect infeasible paths earlier, skip redundant paths
2. **Resume context merging**: Find ways to merge states with compatible resume contexts
3. **Reduce CallStack overhead**: More efficient representation or fewer clones
4. **Caching**: Cache function call results when inputs are identical
5. **Parallel barrier processing**: Process independent barriers concurrently

## Key Files

- `src/interpreter/barrier_executor.rs` - The barrier executor (optimize this)
- `src/interpreter/flow.rs` - Flow-based executor (for comparison/inspiration)
- `src/interpreter/vectorize.rs` - State vectorization/merging
- `src/interpreter/state.rs` - State with gc() method

## How to Test

```bash
# Run barrier-based (what you're optimizing)
./safe-run.sh -- cargo run --release --bin celeste-rust -- --barrier -n 28

# Run flow-based (baseline to compare against)
./safe-run.sh -- cargo run --release --bin celeste-rust -- -n 28
```

Verify correctness: expanded state counts must match between both modes.
- Frame 25: 24, Frame 26: 204, Frame 27: 878, Frame 28: 2864

## Success Metric

Get barrier-based execution closer to flow-based time while maintaining correct state counts.

## How to Work

1. Profile to understand where time is spent
2. Implement optimizations incrementally
3. Verify correctness after each change (state counts must match)
4. Commit working improvements often

This branch is yours - commit frequently.
