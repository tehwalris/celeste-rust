# Agent Prompt: Barrier Executor Optimization

You are optimizing the barrier-based executor for a Rust abstract interpreter of PICO-8 Celeste. Your goal is to maximize throughput: complete as many frames as possible within **60 seconds** and **80GB memory**.

See `docs/barrier-based-execution.md` for design documentation.

## Benchmark

```bash
./safe-run.sh -- timeout 60s cargo run --release --bin celeste-rust -- --barrier --frames 100
```

**Goal**: Maximize the highest frame number completed within 60 seconds.

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
   - Frame count same or higher
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
