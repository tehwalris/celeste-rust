# Symbolic Tracing Design

## Overview

Replace the current vectorized abstract interpretation with a symbolic tracing approach:
1. Execute concrete states one at a time
2. Build symbolic constraints during execution
3. Cache traces for reuse with matching states
4. Systematically enumerate all paths via counter-based exploration

## Key Data Structures

### PathCounter
Tracks which path we're exploring through choice points:
```rust
struct PathCounter {
    // Each element is (num_options, chosen_option)
    // e.g., [(2, 0), (3, 1)] means:
    //   - First choice had 2 options, we took option 0
    //   - Second choice had 3 options, we took option 1
    choices: Vec<(usize, usize)>,
}
```

Increment like a counter:
- `[(2,0), (3,2)]` -> `[(2,1)]` (last overflows, increment previous, truncate)
- `[(2,1), (3,2)]` -> overflow, all paths explored

### SymbolicValue
Values that track their symbolic origin:
```rust
enum SymbolicValue {
    // Concrete number with optional symbolic expression
    Number {
        concrete: Pico8Num,
        symbolic: Option<SymExpr>,
    },
    // Concrete bool with optional symbolic expression
    Bool {
        concrete: bool,
        symbolic: Option<SymBoolExpr>,
    },
    // ... other types stay concrete (strings, pointers, nil)
}
```

### SymExpr / SymBoolExpr
Symbolic expressions for numbers and booleans:
```rust
enum SymExpr {
    Const(Pico8Num),
    InputVar(InputVarId),  // Reference to input state value
    Add(Box<SymExpr>, Box<SymExpr>),
    Sub(Box<SymExpr>, Box<SymExpr>),
    Mul(Box<SymExpr>, Box<SymExpr>),
    Div(Box<SymExpr>, Box<SymExpr>),
    Neg(Box<SymExpr>),
    Ite(Box<SymBoolExpr>, Box<SymExpr>, Box<SymExpr>),
}

enum SymBoolExpr {
    Const(bool),
    InputVar(InputVarId),
    Lt(Box<SymExpr>, Box<SymExpr>),
    Le(Box<SymExpr>, Box<SymExpr>),
    Eq(Box<SymExpr>, Box<SymExpr>),
    And(Box<SymBoolExpr>, Box<SymBoolExpr>),
    Or(Box<SymBoolExpr>, Box<SymBoolExpr>),
    Not(Box<SymBoolExpr>),
}
```

### CachedTrace
A cached execution trace:
```rust
struct CachedTrace {
    // Shape of input state (structure without values)
    input_shape: StateShape,
    // Path taken through the code (for matching)
    path_signature: Vec<(usize, usize)>,  // Same as PathCounter choices
    // Constraints on input values for this path
    path_constraints: Vec<SymBoolExpr>,
    // Symbolic template for output state
    output_template: SymbolicState,
}
```

### TraceCache
Collection of cached traces:
```rust
struct TraceCache {
    // Indexed by input shape for fast lookup
    by_shape: HashMap<StateShape, Vec<CachedTrace>>,
}
```

## Algorithm

### Main Loop (per frame)
```
input_states: Set of concrete states
pending_explorations: Queue of (State, PathCounter)
output_states: Set of concrete states

1. For each state in input_states:
   - Add (state, PathCounter::new()) to pending_explorations

2. While pending_explorations not empty:
   - Pop (state, counter)
   - Try to find matching cached trace
   - If found:
     - Apply substitution to get output state
     - Add to output_states
   - If not found:
     - Execute with symbolic tracing, using counter for choices
     - Record new trace in cache
     - Add output to output_states
     - If any choices were made, increment counter and re-add to pending
       (unless counter overflows = all paths explored)

3. Return output_states
```

### Executing with Symbolic Tracing
```
1. Create symbolic state from concrete state
   - Each value gets an InputVarId
2. Execute instructions, building symbolic expressions
3. At conditional branches:
   - Check counter for which path to take
   - Record constraint (e.g., "x < 10" or "NOT(x < 10)")
   - Continue on chosen path
4. At end, extract:
   - Path constraints
   - Output symbolic state
   - Cache the trace
```

### Cache Lookup
```
1. Compute shape of input state
2. Look up traces with matching shape
3. For each matching trace:
   - Check if input values satisfy path_constraints
   - If yes, return this trace
4. If no match, return None (need to execute)
```

### Substitution
```
Given: CachedTrace, concrete input State
1. Build substitution map: InputVarId -> concrete value
2. Walk output_template, replacing InputVarIds with concrete values
3. Evaluate symbolic expressions to get concrete output state
```

## Simplifications for V1

1. **No symbolic expressions initially** - just cache (shape, path) -> output_state
   - Much simpler, still useful if many states take same path
   - Can add symbolic expressions later for more cache hits

2. **Path-based caching only** - key is (shape, path_choices)
   - Don't need constraint checking
   - Less flexible but simpler

3. **Concrete execution** - use existing interpreter
   - Just add path tracking
   - Output is concrete, not symbolic

## Metrics to Track

1. **Cache hit rate** - how often we avoid re-execution
2. **Paths per shape** - how many unique paths exist per shape
3. **Exploration overhead** - how many re-executions for path enumeration
4. **Memory usage** - cache size
5. **Time breakdown** - execution vs lookup vs enumeration

## Phases

### Phase 1: Path Tracking (No Caching)
- Modify interpreter to track path choices
- Implement PathCounter
- Verify we can enumerate all paths correctly
- Get correctness on first 30 frames

### Phase 2: Basic Caching
- Cache (shape, path) -> output_state
- Implement cache lookup
- Measure cache hit rate

### Phase 3: Symbolic Expressions
- Add SymExpr tracking during execution
- Cache symbolic templates
- Implement substitution
- Should increase cache hit rate significantly

### Phase 4: Optimization
- Profile and optimize hot paths
- Reduce memory usage
- Parallelize where beneficial
