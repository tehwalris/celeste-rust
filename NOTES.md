# celeste-rust notes

## Goal
Find optimal TAS for Celeste Classic via abstract interpretation. State-space is too large for exact DP, so:
1. Compute lower bound (safe over-approximation - guaranteed not to miss paths)
2. Refine by splitting intervals to add precision

## OCaml architecture

```
Lua source → Frontend (parse, compile) → IR (CFG with SSA-like form)
                                              ↓
                                         Interpreter (abstract execution)
                                              ↓
                                         StateSet (all possible outcomes)
```

### Key abstractions

**Values** (what variables can hold):
- `SNumber` / `SNumberInterval` - concrete or interval
- `SBool` / `SUnknownBool` - concrete or abstract (causes branch split)
- `SPointer` / `SNilPointer` - heap references
- Scalars or `Vector` (SIMD-like: pack N states into one)

**State**: heap + local_env + outer_local_envs (call stack) + global_env + prints + vector_size

**LazyStateSet**: lazy normalized/deduped set of states. Critical for avoiding explosion.

### Builtins (by level)
- L1: `__new_unknown_boolean`, `__new_vector`, `__print` (test infra)
- L2: `min`, `max`, `abs`, `flr`, `__split_by_flr` (math)
- L3: `add`, `foreach` (Lua helpers, not native)
- L5: `mget`, `fget` (game map/flag data)

### Refinement mechanism
`__split_by_flr` splits interval [1.5, 3.7] into [1.5,2), [2,3), [3,3.7] - three states with narrower intervals. This is the precision refinement step.

## Rust status (interpreter branch, Nov 2024)

### Done
- Frontend (Lua → IR): complete
- IR types, CFG: complete
- Fixpoint algorithm: complete
- Flow through blocks (phi, branch, return): complete
- Most instruction handling: complete
- Vectorization structure (`MaybeVector<T>`): exists

### WIP/TODO
- `interpret_non_call_instruction`: logic exists but ends in `todo!()`
- `interpret_call_instruction`: `todo!()`
- State deduplication in `join_mut` (TODO comment)

### Missing
- Number intervals (`SNumberInterval`)
- Builtins
- `LazyStateSet` (state deduplication/normalization)
- Game glue (loading celeste.lua, running frames)
- State inspection

## OCaml bottleneck
GC - even heavily optimized. Hence Rust rewrite.

## Next steps to run OCaml tests in Rust
1. Wire up `interpret_non_call_instruction` (logic exists, just needs return)
2. Implement `interpret_call_instruction`:
   - Builtin: lookup + call
   - Closure: push local_env, setup inner state, recurse into CFG, pop
3. Add level 1+2 builtins
4. Port test harness (compare prints)

## 100m room
Target room for initial feature parity. Has one berry, basic jumps.
