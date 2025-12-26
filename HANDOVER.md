# Handover Context for Claude Code

Read this file to continue the implementation work.

## Project Overview

This is a Rust port of `celeste_ocaml` - an abstract interpreter for Lua/PICO-8 code (specifically the Celeste game). The goal is to support the first room (100m) with abstract interpretation, but faster than OCaml (which was bottlenecked by GC).

**OCaml reference**: `~/src/github.com/tehwalris/celeste_ocaml`

## Current State

Branch: `interpreter` (the active development branch)

### Completed
1. ✅ State fields added: `prints`, `outer_local_envs`, `vector_size`
2. ✅ `interpret_non_call_instruction` wired up in `flow.rs`
3. ✅ `FixedEnv` struct created in `src/interpreter/fixed_env.rs`

### In Progress
4. 🔄 Need to implement `interpret_call_instruction` using `Rc<RefCell<Interpreter>>` pattern

### Pending
5. ⏳ Add builtins (`__print`, `__new_unknown_boolean`, `error`, `min`, `max`, `abs`, `flr`)
6. ⏳ Build test harness to run OCaml lua tests
7. ⏳ Get 22 tests passing (function calls + 2 builtins unlock 22 of 30 tests)

## Key Architecture Decision

**Problem**: `interpret_call_instruction` needs to recursively call `interpret_cfg` for closure calls, but this creates a circular reference through the fixpoint machinery.

**Chosen Solution**: Use `Rc<RefCell<Interpreter>>` pattern. The user confirmed preference for this approach (Option A - keep using fixpoint machinery).

Proposed pattern:
```rust
struct Interpreter {
    fixed_env: FixedEnv,  // owned
}

struct InterpreterAnalysis {
    interpreter: Rc<RefCell<Interpreter>>,
    cfg: Cfg,
    labels: IndexSet<Label>,
}

impl Interpreter {
    fn interpret_cfg(self: &Rc<RefCell<Self>>, states: Vec<State>, cfg: &Cfg) -> Result<...> {
        let analysis = InterpreterAnalysis {
            interpreter: Rc::clone(self),
            cfg: cfg.clone(),
        };
        // run fixpoint
    }
}
```

## Key Files

- `src/interpreter/fixed_env.rs` - NEW: FixedEnv with fun_defs and builtin_funs
- `src/interpreter/core_interpreter.rs` - CoreInterpreter, has `interpret_call_instruction` with `todo!()`
- `src/interpreter/flow.rs` - Flow functions, calls CoreInterpreter
- `src/interpreter/glue.rs` - InterpreterAnalysis, implements fixpoint Analysis trait
- `src/interpreter/state.rs` - State with heap, local_env, outer_local_envs, prints, vector_size

## OCaml Reference Points

- `~/src/github.com/tehwalris/celeste_ocaml/interpreter.ml` lines 892-898: `fixed_env` type
- `~/src/github.com/tehwalris/celeste_ocaml/interpreter.ml` lines 1209-1344: call instruction handling
- `~/src/github.com/tehwalris/celeste_ocaml/builtin.ml`: builtin implementations

## Guidelines (from CLAUDE.md)

- Follow OCaml architecture closely
- Don't create parallel/simpler implementations that bypass existing infrastructure
- Use existing: `fixed_point.rs`, `glue.rs`, `flow.rs`, `block_flow.rs`

## Next Steps

1. Refactor `glue.rs` to hold `Rc<RefCell<FixedEnv>>` or similar
2. Implement `interpret_call_instruction` in `core_interpreter.rs`:
   - Look up closure/builtin from heap
   - For builtins: call the builtin function
   - For closures: recursively call `interpret_cfg` on the closure's CFG
3. Add basic builtins in `fixed_env.rs`
4. Build test harness (see `~/src/github.com/tehwalris/celeste_ocaml/lua_tests/` for test files)
5. Run tests and fix issues until 22 pass
