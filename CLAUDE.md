# Claude Code Guidelines

## Architecture
- This is a Rust port of `celeste_ocaml`. Follow the OCaml architecture closely.
- Don't create parallel/simpler implementations that bypass existing infrastructure.
- When unsure about architectural decisions, ask explicitly before diverging.

## Reference
- OCaml source is at `~/src/github.com/tehwalris/celeste_ocaml`
- Read OCaml code as reference when implementing Rust equivalents.

## Existing Infrastructure (use it)
- `fixed_point.rs` - fixpoint algorithm
- `glue.rs` - interpreter analysis setup
- `flow.rs` - flow functions for blocks
- `block_flow.rs` - CFG block flow graph
