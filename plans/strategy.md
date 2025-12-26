# Celeste TAS Optimizer: Strategy and Architecture

## Goal

Find the optimal TAS speedrun for Celeste Classic (least frames from start to end screen) and prove it is optimal. "Optimal" means proving no shorter valid path exists.

## High-Level Approach

### Core Idea: DP with Abstract Interpretation and Iterative Refinement

The game is deterministic with finite state per frame. In principle, you could do DP on the concrete state graph to find the shortest path. In practice, the state space is too large (fixed-point positions, velocities, subpixels, etc.).

**Solution: Iterative refinement of precision**

The search operates on two nested loops:

**Outer loop: Path length**
- Start with path length 0 (or 1)
- For each path length, run the inner refinement loop
- If refinement proves the path length is impossible (empty state set), move to the next longer path length
- Continue until we find a concrete solution

**Inner loop: Precision refinement (for a fixed path length)**
1. Start with coarse abstraction (0 fractional bits for numeric values)
2. Find all states on paths of exactly length N to goal at this abstraction level
3. Increase precision (add 1 fractional bit), but only explore states that "map up" to marked states from the coarser level
4. Repeat until full precision (16 fractional bits for PICO-8 fixed point)

At any point in the inner loop, if the reachable state set becomes empty, we've proven: "not possible to finish the game with this path length at this level of concretization or finer." Since finer includes fully concrete, we've excluded the entire class of solutions with that path length.

At each level, the abstraction over-approximates: it may include unreachable states (false positives) but never excludes reachable ones (no false negatives). The coarser levels prune the search space for finer levels.

**Key invariant**: Marked states at precision N always contain all states that could be on optimal paths at precision N+1.

### Abstraction Sources

- **Numeric ranges**: The primary abstraction. Concrete numbers become intervals that widen to integer boundaries, then to larger ranges.
- **Widened fields**: A marking mechanism identifies "interesting" fields (e.g., player position) vs irrelevant ones (e.g., smoke particle state). Irrelevant fields get widened to abstract values.
- **Unknown booleans**: When control flow branches on unknown values, execution splits and explores both paths.
- **RNG**: TAS runs can have a seed that affects many minor things. Current plan: `rnd()` returns an abstract numeric interval representing the whole possible range, ignoring whether a seed actually exists that makes all calls return those specific values "together." Finding concrete seeds is deferred to later—we'll need to solve it eventually, but it's a separate problem from finding the optimal abstract path.

Widening is always safe: worst case, you over-approximate so much that solving becomes intractable (devolves to concrete DP), but never incorrect.

## Implementations

### 1. Old Hardcoded Rust (main branch)

- Rust port of a game subset with exactly one hardcoded branching point
- Actually completed first room and proved existing TAS is optimal
- Has iterative refinement implemented
- **Not viable for other levels**: Branching gets nested/complex, hardcoding doesn't scale, and correctness is hard to argue when you're not interpreting original Lua

### 2. OCaml Abstract Interpreter

- General-purpose abstract interpreter that runs the original Lua
- Can run ~30 frames of first room before memory/performance issues
- Has enough features for most of first room, but full game would need more
- More principled: interpreting original code makes correctness arguments easier
- Iterative refinement not fully implemented

### 3. Rust Abstract Interpreter (current work, `interpreter` branch)

- Port of OCaml implementation
- Goal: Match OCaml semantics but with better performance
- Currently diverges from OCaml around frame 26 (likely missing features like liveness-based variable removal)
- No iterative refinement yet

## Correctness Layers

### Layer A: Single-Step Abstraction Soundness

**Property**: If concrete state `a` transitions to concrete state `b` under some inputs, then abstract state `a'` (containing `a`) must transition to abstract state `b'` (containing `b`).

This is the foundation. Currently likely has bugs and is hard to verify without better infrastructure.

### Layer B: Iterative Refinement Soundness

**Property**: When mapping states from precision N+1 to precision N, if a state at N+1 could be on an optimal path, the corresponding state at N must be marked.

Not yet implemented in the abstract interpreter codebases.

### Layer C: Implementation Correctness (Optimizations)

**Property**: Optimizations (parallelization, vectorization, caching) don't change the semantics.

Will get harder to maintain as we add more optimizations. Ideally, define a "correctness core" that optimizations provably don't affect.

## Current Blockers

### Observability

The biggest blocker is lack of debugging/inspection infrastructure:
- Currently: run for minutes → get some debug prints → ???
- Can't reason about correctness when we can't see what's happening
- Can't debug divergences between implementations
- Will only get worse at scale (TBs of state data, days of runtime)

**Needed**:
- State serialization/dumps at checkpoints
- Viewer for exploring abstract states (heap structure, shapes, values)
- "Is this concrete state contained in this abstract state?" queries
- Integration with correctness testing

### Concrete-Abstract Testing Infrastructure

For Layer A correctness, we need:
1. A concrete interpreter (simple, clearly correct)
2. The abstract interpreter
3. Abstraction application (concrete value → abstract value)
4. Containment checker (is concrete result ⊆ abstract result?)

**Two levels of testing**:

- **End-to-end**: Given TAS inputs, run through concrete interpreter, check abstract interpreter's reachable set contains the concrete result
- **Unit-level**: Given arbitrary Lua program with concrete inputs → apply abstraction → run abstract → check containment

The current abstract interpreter can execute concrete programs. Key insight: share code between concrete and abstract paths, but ensure complex code (vectorization, etc.) stays out of the concrete code path. Use same input/output types but assert they're in concrete subset.

## Near-Term Plan

### Phase 1: Inspection/Debugging Infrastructure
- State serialization (dump states at checkpoints)
- Basic state viewer (heap structure, shapes, field values)
- Concrete-abstract containment queries

### Phase 2: Correctness Testing
- Minimal concrete interpreter (or mode of abstract interpreter)
- Property-based fuzzing: Lua programs × abstractions → check containment
- End-to-end tests with TAS inputs

### Phase 3: Fix Layer A Bugs
- Use testing infrastructure to find and fix abstraction soundness bugs
- Match OCaml behavior or understand why divergence is acceptable

### Phase 4: Implement Refinement (Layer B)
- Port refinement logic from old hardcoded Rust
- Test with room 1

### Phase 5: Scale
- Performance optimization
- Parallelization (with correctness testing)
- Handle multi-room game

## Architecture Notes

### State Comparison

States are compared via:
1. Heap normalization (deterministic heap IDs via GC)
2. Shape extraction (structure with vectorizable values as placeholders)
3. Field-by-field comparison for states of same shape

Two states intersect if they have the same shape and all corresponding fields intersect.

**Wrinkle: GC depends on liveness analysis**

The GC result depends on liveness analysis for closures—you need to know if a closure will capture something to know if it's garbage or not. This creates a tricky dependency: state shape depends on GC, GC depends on liveness, liveness depends on code analysis.

Open question: Is there a formulation based on subgraphs or something that's "safe" and tolerant to imprecise liveness? The answer may depend on what we're comparing states for—sometimes it's fine to have not-fully-deduplicated states, sometimes it really isn't.

### Vectorization

States with the same "shape" can be merged:
- Numbers and NumberIntervals are vectorizable (merged into vectors)
- Bools are NOT vectorizable (different bool values = different shapes)
- This is a performance optimization that should not affect correctness

### Keeping Correctness Scope Narrow

Goal: Keep tooling separate from core interpreter logic so that:
- Correctness arguments focus on a small, simple core
- Debugging/inspection code doesn't complicate the core
- Optimizations are clearly separate from semantics

## Open Questions

- What's the right format for state dumps? (Needs to handle TBs of data eventually)
- How to structure the state viewer? (CLI? TUI? Web?)
- How to handle the "abstract states contain unreachable concrete states" problem when debugging? (False positives are expected but confusing)
- Should we invest in making OCaml faster, or focus entirely on Rust?
