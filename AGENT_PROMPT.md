# Agent Prompt: Fix Barrier Executor - Barriers Inside Functions Are Ignored!

## The Critical Issue

**Read first**: `docs/barrier-based-execution.md` (see "KNOWN LIMITATION" section)

The barrier-based executor has a **critical architectural flaw**: barriers inside function calls are completely ignored!

When `run_to_next_barrier` encounters a Call instruction, it calls `interpret_call_with_path_counter`,
which uses `run_cfg_to_completion`. This function runs the **entire function to completion ignoring all barriers**!

### Evidence

Run with verbose output to see:
```bash
./safe-run.sh -- cargo run --release --bin celeste-rust -- --barrier -n 26 2>&1 | grep Barrier
```

Output shows ONLY the END barrier ([-2147483648]) - no intermediate barriers:
```
Barrier [-2147483648] hit=0: 1 vectorized states (1 expanded)
Barrier [-2147483648] hit=0: 1 vectorized states (24 expanded)
...
```

But `_update()` contains `_hint_normalize()` calls which compile to `Barrier([0])`. These are never hit!

### The Impact

| Frame | Barrier | Flow | Barrier's Problem |
|-------|---------|------|-------------------|
| 28 | 3.1s | 4.9s | Fast (but lucky) |
| 29 | 12.2s | 8.0s | Path explosion starts |
| 30 | 35.5s | 18.7s | Unbounded explosion |

Paths grow: 12572 → 53434 → 180008 with no intermediate merging!

## Your Task: Fix This

**Option A - Propagate Barrier Yields Through Call Stack** (recommended):
1. Change `run_cfg_to_completion` to yield at barriers like `run_to_next_barrier` does
2. When a barrier is hit inside a function call, propagate it up through `interpret_call_with_path_counter`
3. The caller must then suspend and resume execution when the barrier is processed

This is complex because it requires:
- Saving call stack state when yielding at a barrier
- Restoring call stack state when resuming

**Option B - Inline Function Calls**:
- Before barrier execution, inline all function bodies into a single CFG
- Simpler to implement but makes CFG much larger

**Option C - Add Barriers at Frame CFG Level** (workaround):
- Modify `main.rs` to add `__barrier()` calls between `_update()` and `_draw()`
- Doesn't fix the root issue but may help somewhat

## Key Files

- `src/interpreter/barrier_executor.rs`:
  - `run_to_next_barrier()` - yields at barriers (lines 728-860)
  - `run_cfg_to_completion()` - does NOT yield at barriers (lines 249-346)
  - `interpret_call_with_path_counter()` - calls run_cfg_to_completion (lines 125-247)

- `src/ir.rs`:
  - `Block.barrier: Option<BarrierId>` - barrier on blocks (line 322)

## How to Test

```bash
# Run barrier-based (should now show intermediate barriers)
./safe-run.sh -- cargo run --release --bin celeste-rust -- --barrier -n 28

# Verify correctness - state counts must match
./safe-run.sh -- cargo run --release --bin celeste-rust -- -n 28
```

Frame 25-28 expanded counts: 24, 204, 878, 2864

## Success Metric

1. Barrier output should show intermediate barriers (not just END barrier)
2. State counts must still match flow-based
3. Performance at Frame 30 should improve (currently 35.5s barrier vs 18.7s flow)

## Notes

- This is a significant architectural change - take your time
- The existing batched vectorization (every 64 paths) helps but isn't enough
- The PathCounter must thread through properly when resuming
- Consider looking at how `flow.rs` handles the same problem
