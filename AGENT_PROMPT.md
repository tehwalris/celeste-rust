# Agent Prompt: Barrier Executor Optimization

You are optimizing the barrier-based executor for a Rust abstract interpreter of PICO-8 Celeste. The goal is to make barrier-based execution competitive with flow-based execution.

## Benchmark

```bash
# Barrier-based (what you're optimizing)
./safe-run.sh -- cargo run --release --bin celeste-rust -- --barrier -n 30

# Flow-based (baseline to compare against)
./safe-run.sh -- cargo run --release --bin celeste-rust -- -n 30
```

**Current performance** (barrier vs flow):
- Frame 28: ~7s vs ~3s (2.2x slower)
- Frame 29: ~28s vs ~5s (5.6x slower)
- Frame 30: ~75s vs ~11s (6.7x slower)

**Goal**: Reduce barrier execution time to be closer to flow-based.

## Correctness Constraint

**The expanded state count per frame MUST remain identical.** For example:
- Frame 25: 24 expanded
- Frame 26: 204 expanded
- Frame 27: 878 expanded
- Frame 28: 2864 expanded
- Frame 29: 7260 expanded
- Frame 30: 15250 expanded

If state counts change, the optimization broke correctness and must be reverted.

## Key Files

- `src/interpreter/barrier_executor.rs` - The barrier executor (optimize this)
- `src/interpreter/flow.rs` - Flow-based executor (for comparison)
- `docs/barrier-based-execution.md` - Design documentation

## Approach

Profile first to identify bottlenecks, then optimize based on data. Key areas:
- PathCounter enumeration (2^n paths for n UnknownBool branches)
- CallStack/ResumeContext tracking overhead
- State deduplication and vectorization

## Workflow

Each iteration:

1. **Profile** to identify hotspots

2. **Implement** one optimization

3. **Measure** and verify:
   - State counts identical (correctness)
   - Timing improved

4. **Commit**:
   - If BETTER and CORRECT:
     ```bash
     git add -A && git commit -m "perf: <description>"
     git push
     ```
   - If WORSE or INCORRECT:
     ```bash
     git checkout -- .
     ```

Begin by profiling the current state and making your first optimization attempt.
