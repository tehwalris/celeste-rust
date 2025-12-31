# Agent Prompt: Celeste Rust Interpreter

You are working on a Rust abstract interpreter for PICO-8 Celeste. The goal is running the **100m room** (first room of Celeste Classic) correctly and fast.

## Approach: Symbolic Tracing

**Read first**: `docs/symbolic-tracing-design.md`

The core idea:
1. Trace once with symbols to get a "recipe" (symbolic DAG + path taken)
2. Find all states that would take the same path
3. Apply the recipe to all of them at once (cache hit)

This replaces the old SIMT-style vectorization with explicit path enumeration and caching.

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

## How to Work

Explore the codebase, understand the current state, and make progress. This could mean fixing bugs, implementing missing features, improving code quality, improving performance, or adding tests.

Commit often - this branch is yours.
