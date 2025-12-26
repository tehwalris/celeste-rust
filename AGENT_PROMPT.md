# Agent Prompt: Celeste Rust Interpreter

You are working on porting an OCaml abstract interpreter to Rust. The goal is running the **100m room** (first room of Celeste Classic) correctly and fast.

The OCaml project is at `~/src/github.com/tehwalris/celeste_ocaml`.

## Phases

### Phase 1: All Tests Passing

Port all tests from the OCaml project to Rust and get them passing. The OCaml tests are in `lua_tests.ml`.

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

Explore the codebase, understand the current state, and make progress. This could mean fixing bugs, implementing missing features, improving code quality, or adding tests.
