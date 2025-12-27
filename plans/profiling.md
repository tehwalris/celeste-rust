# Profiling Infrastructure Plan

## Problem

We start and end each frame with a single vectorized state, but have no visibility into what happens in between. Could be splitting to scalars immediately and running most execution unvectorized.

## Three Profiling Angles

### 1. State Flow DAG

Track how states split and merge during execution.

- **Nodes**: State sets at a point in execution (could be single vectorized state, or multiple states after split)
- **Edges**: Splits (one node → multiple nodes) and vectorization/joins (multiple nodes → one node)
- **Goal**: Detect "too late" joins - cases where we run lots of execution with states split that could have been merged earlier

Per-node stats:
- Number of states, total vector_size
- Time spent processing this node
- Number of function calls made
- Maybe: which operation caused split/join

### 2. Fixed-Point Recursion Tree

The interpreter runs a fixed-point loop, but function calls recurse into nested fixed-point solvers.

- **Tree structure**: Each fixed-point invocation is a node, children are nested calls
- **Per-node stats**: Time spent, iterations, states processed
- **Goal**: Understand cost of recursion, find hot spots in call tree

Questions to answer:
- How deep does recursion go?
- Are small functions being called many times with high overhead?
- Where is time actually spent vs overhead?

### 3. Linear Span Trace (Chrome Tracing)

Standard wall-clock span-based profiling, output as Chrome tracing JSON.

- Shows where time is spent in execution order
- Reference DAG nodes and tree nodes by ID for cross-correlation
- Useful for "what's slow right now" questions

## Output Format

TBD - likely:
- DAG/Tree: JSON or custom format, viewable with simple script
- Spans: Chrome tracing JSON (`chrome://tracing`)

## Implementation Notes

- Keep profiling code separate from core interpreter
- Use feature flag or runtime flag to enable (avoid overhead when not profiling)
- Start simple: basic stats per fixed-point, then add DAG tracking
