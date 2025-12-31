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

## Installing packages

Feel free to install pacman packages when needed (e.g., for profiling tools like `perf`).

## Serving trace files

Trace files (for Chrome's `chrome://tracing` viewer) are served from the `server/` directory:

```bash
# Generate a trace for frame 37 (from cached checkpoint)
./safe-run.sh -- cargo run --release --bin celeste-rust -- -n 37 --checkpoint-dir checkpoints --resume --trace /tmp/trace.json.zst

# Copy to server directory and start HTTP server
cp /tmp/trace.json.zst server/
cd server && python3 -m http.server 8000 &

# Verify the file is served correctly
curl -s http://localhost:8000/trace.json.zst | sha256sum
sha256sum server/trace.json.zst  # Should match
```

The `server/` directory is gitignored.
