# Symbolic Tracing Design

## Motivation

The current SIMT-style vectorized interpreter has a bottleneck: filtering during every tiny branch. Even when most vector lanes "go the same way", the filtering overhead is significant because it happens at every branch point, function call boundary, etc.

The new approach:
1. **Trace once** with symbols to get a "recipe" (symbolic DAG + path taken)
2. **Find all vector elements** that would take the same path
3. **Apply the recipe** to all of them at once (one "filter" up front, not thousands during execution)

## Core Concepts

### Symbolic Values

During tracing, every `Value` carries both:
- **Concrete value**: The actual runtime value (used for branch decisions)
- **Symbol**: A symbolic expression showing how this value was computed from inputs

At frame start, all values get fresh input symbols. At frame end, values contain derived symbols like `add(a, sub(b, c))` where `a`, `b`, `c` are input symbols.

### Counting DFS for Path Enumeration

When tracing hits a branch with an abstract value (like `UnknownBool`), we use counting DFS to enumerate all paths:

- Path counter: `[(0, 2), (1, 3), (0, 2)]` means:
  - First abstract branch had 2 options, we took option 0
  - Second abstract branch had 3 options, we took option 1
  - Third abstract branch had 2 options, we took option 0

- To explore all paths, we "increment" the counter like an odometer
- New branches can extend the counter as we explore

This replaces the current `flow.rs` approach for handling abstract branching.

### Path Conditions

Each traced path produces boolean symbolic expressions (path conditions) that must be satisfied for that path to be valid. When replaying onto a concrete state, we evaluate these conditions to verify compatibility.

## Execution Flow

```
Frame N states (vectorized, compact storage)
    │
    ▼
┌─────────────────────────────────────────────────────┐
│ All elements start with path_counter = []           │
│                                                     │
│ While any elements are not "done":                  │
│   1. Pick an unfinished element                     │
│   2. Trace it with counting DFS until frame ends    │
│      - Record symbolic transformations              │
│      - Record path conditions                       │
│      - Record final path_counter                    │
│   3. Find all other elements with:                  │
│      - Same path_counter structure                  │
│      - Path conditions satisfied by their values    │
│   4. Apply symbolic transformation to all matches   │
│   5. Mark matched elements as "done"                │
│   6. GC the traced result                           │
└─────────────────────────────────────────────────────┘
    │
    ▼
Frame N+1 states (re-vectorized for storage)
```

### Scope of Tracing

Initially: trace an entire frame at once.

Later optimization: could trace finer regions (e.g., one `foreach` loop iteration inside `_update` or `_draw`).

## Data Structures

### Symbolic Expressions

```rust
// A symbolic identifier for an input value
#[derive(Clone, Hash, Eq, PartialEq)]
struct SymbolId(u32);

// Symbolic expression DAG node
enum SymExpr {
    // Input symbol (created at frame start)
    Input(SymbolId),
    // Primitive operations
    Add(Box<SymExpr>, Box<SymExpr>),
    Sub(Box<SymExpr>, Box<SymExpr>),
    Mul(Box<SymExpr>, Box<SymExpr>),
    Div(Box<SymExpr>, Box<SymExpr>),
    Lt(Box<SymExpr>, Box<SymExpr>),
    Eq(Box<SymExpr>, Box<SymExpr>),
    And(Box<SymExpr>, Box<SymExpr>),
    Or(Box<SymExpr>, Box<SymExpr>),
    Not(Box<SymExpr>),
    // ... other ops matching interpreter primitives
    // Constant (for literals, nil, etc.)
    Const(ConcreteValue),
}
```

### Traced Values

```rust
// A traced value: concrete result + how we got there
struct TracedValue {
    concrete: ConcreteValue,  // The actual value (for path decisions)
    symbol: SymExpr,          // How to compute it from inputs
}
```

### Path Counter

```rust
// Path counter: sequence of (branch_taken, num_branches)
type PathCounter = Vec<(usize, usize)>;
```

### Traced Path Result

```rust
// Result of tracing one path through a frame
struct TracedPath {
    path_counter: PathCounter,
    path_conditions: Vec<SymExpr>,  // Boolean exprs that must be true
    output_state: TracedState,      // State with TracedValues
}
```

## Heap Handling

Vectorized states already have the same heap structure (same HeapIds point to same logical objects). This is preserved:

1. **During tracing**: Heap operations work normally. Values written to heap carry their symbols.
2. **After tracing**: GC the traced state to get a clean heap.
3. **During replay**: Map over the GC'd heap, replacing values by applying symbolic transforms to the replayed-onto state's concrete values.

Multiple writes to the same HeapId: the last write's symbol wins (same as current behavior).

Creating new heap entries: these get HeapIds during tracing, and the recipe includes "create entry at HeapId X with symbol Y".

## Side Effects

- **Prints**: Collected during tracing for debugging. Not replayed (prints are just for debugging).
- **Other side effects**: Handle similarly - execute during trace, skip during replay.

## Applying Transformations

When we have a `TracedPath` and want to apply it to element `i`:

```rust
fn apply_traced_path(
    input_state: &ConcreteState,  // Element i's state at frame start
    traced: &TracedPath,
) -> ConcreteState {
    // Build substitution: SymbolId -> ConcreteValue from input_state
    let subst: HashMap<SymbolId, ConcreteValue> = /* map input symbols to input_state values */;

    // Evaluate each output symbol with this substitution
    let output_state = traced.output_state.map_values(|traced_val| {
        evaluate_symbol(&traced_val.symbol, &subst)
    });

    output_state
}
```

## Why This Is Faster

Current approach: Execute N vector lanes in lockstep, filtering at every branch creates subsets that execute separately. Many tiny filters accumulate overhead.

New approach:
1. **Many lanes share the same path** (likely true - most game states follow similar control flow)
2. **Applying transformation is cheaper** (no filtering during each step of execution, just once up front when we match elements to a traced path)
3. **Path matching is cheap** (compare path counters, evaluate path conditions)

Key insight: **Split up front to match a traced state is much cheaper than continuously splitting within many tiny branches and function calls.**

## Implementation Plan

1. Create branch, write this design doc
2. Implement core data structures (`SymExpr`, `TracedValue`, `PathCounter`)
3. Port existing tests to the new system where applicable
4. Get all tests passing
5. Run the full game for a few frames
6. Compare resulting state counts per frame to old implementation
7. Get counts to match

**Success metric**: Get as many correctly count-matched frames executed in 60s / 80GB limits as possible. First optimize for correctness (matching frame counts), then for speed (more frames in the time limit).

## State Representation

- **Within a frame**: Interpreter works with scalar states + symbols (no `MaybeVector`)
- **Between frames**: Vectorized storage for compactness (same as current)

The path counter is purely a "within frame" concept. We finish executing the frame once we've covered all counters and vectorized the results into one set of vectorized states like what we started with.

## Implementation Status (2026-01-01)

### Completed
- Core data structures: `SymExpr`, `TracedValue`, `PathCounter`, `TracingInterpreter`
- Non-recursive tracing interpreter with explicit call stack
- Path enumeration via counting DFS
- Symbolic expression tracking for all operations
- Path condition collection for branches
- Heap allocation tracking

### Key Findings

#### Caching Challenge: Path-Dependent Branches
The original design assumed caching based on `(shape, abstract_path)` would work. However, **branch conditions are path-dependent**:

- Different traces taking different concrete branches encounter different subsequent branches
- A trace with `concrete_path = [true, false, true]` sees different conditions than one with `[false, true, true]`
- Templates (condition sequences) created from the first trace don't apply to subsequent traces
- Result: 95%+ of traces have mismatched branch counts vs their template

This means we can't predict `concrete_path` without actually tracing. The potential cache hit rate is 99.8%, but we'd need to trace to know the path.

#### Performance Bottlenecks and Optimizations

**Initial state**: ~8-32x slower than reference interpreter

| Frame | States | Ref (ms) | Sym (ms) | Ratio |
|-------|--------|----------|----------|-------|
| 28    | 2,864  | 4,596    | 40,327   | 8.8x  |
| 29    | 7,260  | 8,127    | 179,907  | 22.1x |
| 30    | 15,250 | 17,081   | 550,994  | 32.3x |

**Optimizations applied**:

1. **Arc<Cfg> for function calls** (commit 26a908f)
   - Problem: 4.5M CFG clones (236.8 function calls × 19,092 traces)
   - Solution: Store CFG in Arc, cloning is now a refcount bump
   - Result: Frame 28: 40.3s → 24.9s (38% faster)

2. **Avoid Block cloning** (commit fe010ad)
   - Problem: Cloned Block struct on every iteration of main loop
   - Solution: Clone Arc<Cfg> (cheap) and borrow Block reference
   - Result: Frame 28: 24.9s → 22.5s (10% faster)

3. **Vec for local_symbols** (commit 5579612)
   - Problem: HashMap overhead for local symbol lookups/inserts
   - Solution: Use Vec<Option<SymExpr>> indexed by LocalId
   - Result: Frame 28: 22.8s → 21.7s (5% faster)

4. **Vec for heap_symbols + heap freeze** (commit 6b379b1)
   - Problem: heap_symbols HashMap overhead, input_heap_ids HashSet unused
   - Solution: Use Vec<Option<SymExpr>> indexed by HeapId, pre-allocate Vec,
     freeze heap before tracing for fast get_opt (Vec lookup instead of HashMap)
   - Result: Frame 28: 21.7s → 18.4s (15% faster)

5. **Parallel tracing with rayon** (commit 0369517)
   - Problem: Single-threaded tracing doesn't utilize multi-core systems
   - Solution: Process trace batches in parallel using rayon's par_iter
   - Result: Frame 28: 18.5s → 7.9s (2.3x speedup with multi-core)

6. **Fast mode: Skip symbolic tracking** (commit 830336c)
   - Problem: Symbolic expressions (heap_symbols, local_symbols, path_conditions)
     are unused when caching is disabled
   - Solution: Add `track_symbols` flag to TracingInterpreter, use `new_fast()`
     constructor when caching is disabled
   - Result: Frame 28: 18.6s → 15.7s (16% faster), no more OOM at frame 30

**Current state** (after optimizations, with fast mode and parallel):
| Frame | Ref (ms) | Sym Seq (ms) | Sym Parallel (ms) | Parallel Ratio |
|-------|----------|--------------|-------------------|----------------|
| 28    | 4,722    | 15,671       | 8,003             | 1.7x           |
| 29    | 7,633    | 71,000+      | 36,633            | 4.8x           |
| 30    | 16,715   | 230,000+     | 111,343           | 6.7x           |

Note: Frame 30 now completes (was OOM at 100GB before fast mode).

Profile breakdown (sequential, with fast mode):
- **Bitmap iteration** (im-rs internal): ~10%
- **TracingInterpreter::interpret** (main loop): ~9%
- **Hashing** (im-rs HAMT): ~8%
- **hash_key** (HAMT lookup): ~5%
- **Heap::get_opt**: ~5%
- **HAMT insert**: ~3%
- **Value cloning**: ~2%

Root cause: The tracer uses the same `State` structure as the reference interpreter, which uses persistent data structures (im-rs HAMT) for the Heap and LocalEnv. This adds overhead for scalar tracing where we don't benefit from structural sharing.

#### Caching Potential Analysis

Stats from frame 28 (2,864 expanded states, 19,092 traces):
- Unique (shape, abstract_path) pairs: 305
- Unique (shape, abstract_path, concrete_path) tuples: 410
- Potential cache hits: 18,787 (98.4%)

This suggests massive opportunity for caching, but:
- Caching by (shape, abstract_path) fails because concrete_path varies
- Caching by (shape, abstract_path, concrete_path) requires symbolic evaluation
  to reconstruct output for different input values
- Without symbolic tracking (fast mode), we can't reconstruct outputs

### Next Steps

1. **Further data structure optimizations**:
   - Use simpler (non-persistent) heap for scalar tracing
   - Consider custom LocalEnv for tracer with Vec storage

2. **Hybrid symbolic tracking**:
   - Enable symbol tracking only when potential cache hit rate is high
   - Fast mode for exploration, symbolic mode for caching

3. **Alternative approaches**:
   - Block-level caching (smaller units, less path dependence)
   - Trace by abstract path first, then split on concrete branches
