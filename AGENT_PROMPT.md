# Agent Prompt: Codebase Cleanup

You are improving the quality of a Rust abstract interpreter for PICO-8 Celeste. The interpreter compiles Lua to a CFG IR and runs optimization passes (mem2reg, DCE, inlining, heap elimination, call resolution, etc.).

## Goal

Make the codebase cleaner, more robust, and easier to maintain. This includes:
- Reducing code duplication
- Improving test coverage (especially integration tests on real Celeste CFGs)
- Fixing validation warnings and edge cases
- Clarifying confusing code
- Removing dead code

## Codebase State

There are two main entry points:

1. **Game runner** (primary): Runs the abstract interpreter on Celeste for N frames
   ```bash
   ./safe-run.sh -- cargo run --release -- -n 30
   ```
   This is the main use case we care about. State counts per frame must remain correct.

2. **CFG viewer**: Generates JSON for a web-based CFG visualization tool
   ```bash
   ./safe-run.sh -- cargo run --release --bin cfg_viewer
   # Output: serve/cfg_analysis.json
   ```
   The viewer shows each optimization pass step-by-step on real Celeste functions.

**Important context**: Some CFG passes are implemented and visible in the viewer but not yet fully integrated with the game runner. The goal is to:
- Preserve these passes by making them well-tested
- Make it easy for future agents to iterate on passes
- Keep passes visible in the viewer (for humans) AND convenient to test (for agents)

## Constraints

- Don't break existing tests
- Don't change correctness semantics (optimization passes should produce equivalent results)
- Run `./safe-run.sh -- cargo test --release` to verify changes

**Note**: CFG transformations may have unhandled edge cases, subtle bugs, or missing assertions. Don't assume the code is always correct. If something is clearly broken, fix it.

## Your Task

You are running **one iteration** of a cleanup loop. Do the following:

1. **Explore**: Look at the codebase to find one thing worth improving. Check for:
   - Test warnings or validation errors
   - Duplicated code patterns
   - Missing test coverage for edge cases
   - Confusing or poorly documented code

2. **Improve**: Make an improvement. This can be small or large (at your discretion), as long as you expect to reasonably finish within one context window.

3. **Verify**: Run tests, check for new warnings

4. **Commit** (if tests pass):
   ```bash
   git add -A && git commit -m "cleanup: <description>"
   ```

After committing, stop. The next iteration will be handled by a fresh invocation.

## Key Areas

- `src/interpreter/` - CFG passes (~19k lines)
- `src/interpreter/cfg_analysis.rs` - Pipeline orchestration
- `src/interpreter/celeste_integration_test.rs` - Integration tests on real Celeste code
- `src/bin/cfg_viewer.rs` - Generates viewer data
