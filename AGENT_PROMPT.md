# Agent Prompt: Celeste Rust Interpreter

You are working on porting an OCaml abstract interpreter to Rust. The goal is running the **100m room** (first room of Celeste Classic) correctly and fast.

## Phases

### Phase 1: All Tests Passing

Port the OCaml Lua tests (`~/src/github.com/tehwalris/celeste_ocaml/lua_tests/`) to Rust and get them all passing. See `lua_tests.ml` for test structure.

No `todo!()` should remain in the interpreter when done.

### Phase 2: 100m Room Running

Get the actual game running. See `frontend_example.ml` in the OCaml project for how this works.

**Success metric**: State counts per frame matching OCaml output, running in reasonable time.

## Approach

1. Follow OCaml architecture - match the overall structure
2. Use existing Rust infrastructure - don't bypass it with simpler implementations
3. When unsure, read the OCaml code and/or create comparison tests
4. Commit often - this branch is yours

## How to Work

1. Explore the codebase to understand current state
2. Run `cargo test` to see what's passing/failing
3. Pick the next blocker and fix it
4. Verify with tests
5. Commit and repeat

## OCaml Reference

The OCaml project is at `~/src/github.com/tehwalris/celeste_ocaml`. Key files:
- `interpreter.ml` - main interpreter
- `builtin.ml` - builtins
- `lua_tests.ml` - test harness
- `frontend_example.ml` - 100m room runner
