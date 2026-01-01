# Agent Prompt: Barrier Executor Optimization

You are optimizing the barrier-based executor for a Rust abstract interpreter of PICO-8 Celeste. Your goal is to make barrier-based execution competitive with flow-based execution.

See `docs/barrier-based-execution.md` for design documentation.

## Benchmark

```bash
# Barrier-based (what you're optimizing)
./safe-run.sh -- cargo run --release --bin celeste-rust -- --barrier -n 30

# Flow-based (baseline to compare against)
./safe-run.sh -- cargo run --release --bin celeste-rust -- -n 30
```

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

## Approach

Any optimization technique is fair game as long as correctness is preserved. Profile first to identify bottlenecks, then optimize based on data. The barrier executor is in `src/interpreter/barrier_executor.rs`.

Use `./safe-run.sh` to run commands with memory limits. Use `perf` for profiling.

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
     git add -A && git commit -m "experiment: <description> (reverting)"
     git revert HEAD --no-edit
     git push
     ```

Begin by profiling the current state and making your first optimization attempt.
