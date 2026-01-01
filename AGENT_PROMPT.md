# Agent Prompt: Celeste Rust Interpreter

You are working on a Rust abstract interpreter for PICO-8 Celeste. The goal is running the **100m room** (first room of Celeste Classic) correctly and fast.

## Approach: Barrier-Based Parallel Execution

**Read first**: `docs/barrier-based-execution.md`

The core idea:
1. Fast scalar interpretation with PathCounter for abstract path enumeration
2. Explicit barriers in code where states synchronize
3. At barriers: GC + Vectorize (+ Abstraction at end-of-frame)
4. Process one barrier at a time, all states at that barrier in parallel

This replaces hintNormalize entirely. Barriers are the new mechanism for both synchronization and vectorization.

## Reference State Counts

Expanded state counts (sum of vector_size) from the vectorized Rust implementation:

| Frame | Expanded | Time    |
|-------|----------|---------|
| 1-24  | 1        | ~1ms    |
| 25    | 24       | 15ms    |
| 26    | 204      | 157ms   |
| 27    | 878      | 1.1s    |
| 28    | 2864     | 4.6s    |
| 29    | 7260     | 8.0s    |
| 30    | 15250    | 17.1s   |
| 31    | 27024    | 28.9s   |
| 32    | 44558    | 46.4s   |
| 33    | 66194    | 69.1s   |
| 34    | 92717    | 99.4s   |
| 35    | 132117   | 156.6s  |

States grow ~1.5-3x per frame after movement starts.

## Success Metric

Maximize frames executed correctly in 60s / 80GB.

"Correctly" = state counts per frame match the reference. Use the first 28 frames for correctness testing.

## Resources

- OCaml reference implementation: `~/src/github.com/tehwalris/celeste_ocaml`
- OCaml has `hint_normalize` in IR blocks - we're replacing this with barriers

## How to Work

Explore the codebase, understand the current state, and make progress. This could mean fixing bugs, implementing missing features, improving code quality, improving performance, or adding tests.

Commit often - this branch is yours.
