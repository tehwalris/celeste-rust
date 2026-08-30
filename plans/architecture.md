# Architecture (agreed 2026-08-30)

The target we untangle toward. First-attempt module split; refine while doing it.
Goal size ~10-12k lines total; the outer loop is the priority (it is where the
perf problems and the least-understood code are, and cutting it down IS
understanding and fixing it).

## The whole system in one line

The **search** drives a **frame step** over **blocks**; the frame step is either
the compiled **kernels** or the **interpreter**; the kernels come from the
**compiler** via one hand-off.

## The three interfaces - the ONLY things that cross between parts

1. **The frame step.** `run(block, precision) -> [block]`. A small batch of
   input lanes in; all their output rows out, each row already carrying its KEY
   and its POSITION, already widened for `precision`. Branching, widening, and
   keying happen INSIDE. The kernels and the interpreter are two implementations
   of this one trait - fast and trusted.
2. **The block.** A columnar batch of lanes. Key column + position column are
   EXPOSED; the field data is opaque columns. A whole block serializes/compresses
   compactly (batches share structure - never serialize lanes one at a time).
   This is the only data type that crosses everywhere.
3. **The kernel set.** The compiler's entire output to the engine: the assembled
   ASM kernels for the start room's shapes at a precision. The engine loads it
   and knows nothing about tracing/graphs/lowering/assembly.

To the outer loop a state is OPAQUE: it only ever touches a row's **key**
(dedup, visited set, checkpoint) and its **position** (grouping, the position
graph). The frame step is the only thing that opens a state.

## The parts (first-attempt module/crate split)

1. **core** - the game: pico-8 numbers, cart, collision, frozen names. Pure
   data/math. (~1.2k, irreducible)
2. **block** - the columnar batch + key/position columns + compact batch
   serialization. The data currency. (today: runtime2 `Rt2` + checkpoint serde)
3. **kernel-compiler** - Lua + precision -> assembled ASM kernels. Inside: the
   tracer, graph, lowering, assembler - none visible outside. Hands the engine a
   kernel set. (today: `src/trace` (minus the interpreter) + `src/transpile`;
   ~18k, the biggest shrink target)
4. **engine** - the frame step trait + two impls: the kernel runner (dispatches
   the compiled kernels) and the scalar interpreter (the reference). Both attach
   key+position and hide branching+widening. (today: `src/compiled` +
   `celeste-engine` run + `src/trace/ref*`)
5. **search** - the outer loop: forward, backward, ladder. Speaks only
   key+position; passes opaque blocks to the engine; checkpoints blocks. (today:
   `src/search`; ~6k -> target ~2k)
6. **the binary** - the ladder/bench/sweep CLI.

`celeste-ir` (the CFG) is not a part; its CFG only carries program metadata now
and folds into `search`'s program assembly (or dies) as that reworks.

## The outer loop in ~10 sentences (the rewrite target)

Anything in today's `src/search` that is not serving one of these is cut.

1. Forward starts from the initial block (frame 0).
2. Each frame, run the frame step on every block in the frontier -> output rows.
3. Dedup the output rows by key.
4. Subtract rows whose key is already in the visited set (frontier-only).
5. What remains is the next frontier; add its keys to the visited set.
6. Serialize the frontier's blocks to a checkpoint (compact, batched).
7. Repeat until a row exits the room (a win) at horizon H.
8. Backward: seed the kept set with the win rows at H; walking frames H-1..1, a
   row is KEPT if the frame step sends it to a kept row - candidates narrowed to
   the positions the position graph says can reach a kept position.
9. A row first kept at frame i has frame-distance g = H - i; that is the output.
10. Ladder: run forward+backward at rising rem precision (0,1,..,16) until some
    precision finds no win (the horizon is refuted) or the exact top rung
    confirms it - the minimal win frame is the answer.

The perf redesigns fall out of writing this cleanly: dedup decided at the door
(no materialize-then-filter -> kills the visited-set cost and the sweep memory),
no chunking (the legacy knob disappears), one frame-step trait the loop drives
without knowing kernels-from-interpreter.

## Docs

This file is the AGREED, load-bearing architecture - keep it current. Session
scratch (progress logs, deletion inventories) lives in
`plans/kernel-boundary-and-deletion.md` and is freely deletable.

## Deferred follow-ups (post-rebuild, 2026-08-31)

The rebuild replaced the old search (`run.rs`/`sweep*.rs`, deleted) with
`src/frame.rs` + the `rewrite search` command. Still open, in rough priority:

1. **Trace extraction** - the winning input sequence at the concrete level. The
   old `ExtractTas`/`TraceWitness` were deleted with the old backward; the new
   `find_optimum` reports the optimal FRAME but not yet the witness trace.
   Rebuild minimally on the sharded checkpoints + concrete-level backward.
2. **Incremental rem-0 horizon extension** - `find_optimum` re-runs the whole
   ladder fresh per horizon; `forward_resume` exists to extend rem-0 by one frame
   instead (Philippe: "one more frame at rem zero"). Wire it in.
3. **Wide + lane-bitmask provenance backward** - `backward_run` re-runs one input
   lane at a time (correct, slow). Make the kernel emit a per-output source-lane
   bitmask so backward runs 16-wide (design agreed; the narrow per-call reuse of
   the old origin-union-on-merge).
4. **Re-home the batch-invariance test** - `a_lanes_key_does_not_depend_on_its_
   neighbours` went with `sweep.rs`; re-point onto `engine_row_keys` if wanted
   (parcheck's byte-identity was the stronger property, now also gone with the
   parallel path).
5. **A new-path widening-soundness check** - `Widencheck` validated the old
   forward's widening; the new path widens in `MarkFilter`. Add an equivalent.
