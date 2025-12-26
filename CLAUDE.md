# Claude Code Guidelines

- This is a Rust port of `celeste_ocaml`. Follow the OCaml architecture closely.
- Don't create parallel/simpler implementations that bypass existing infrastructure.
- This is a complex project. Take your time. Do not take shortcuts due to time pressure.
- OCaml source is at `~/src/github.com/tehwalris/celeste_ocaml`
- Read OCaml code as reference when implementing Rust equivalents.

## Running tests safely

Use `./safe-run.sh` for cargo commands to prevent OOM from killing your session (100G limit by default):

```bash
./safe-run.sh -- cargo test
./safe-run.sh -- cargo test test_run_celeste_game_frame
```

Exit code 137 means OOM.
