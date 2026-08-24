//! The kernel emitter and the graph IR it lowers.
//!
//! `graph` is the value graph the tracer (`src/trace`) builds, `bdd` and
//! `ival` decide its boolean and interval layers, `lower` turns it into
//! straight-line Rust, and `kernel` holds the emitter state (`Emit`) and
//! the `Line` stream `trace::kernel::render` assembles into the checked-in
//! traced kernels (`crates/celeste-kernels/src/traced/`). The CLI is
//! `src/bin/transpile.rs`.
//!
//! These live in the LIBRARY, not in the binary, for one reason: the
//! generated files are committed, so something has to fail loudly when they
//! drift from the emitter, and a `#[test]` can only call library code. See
//! `traced_kernels_are_current` in `trace::kernel`.
//!
//! Note the bootstrap this creates, because it will bite whoever changes the
//! emitted preamble. `celeste-rust` (this crate) is where the emitter
//! lives, and it depends - through the frame interface - on `celeste-kernels`,
//! whose contents it produces. So if you change what the emitter emits, the
//! checked-in kernels stop compiling and `cargo build --bin transpile`
//! cannot build the tool that would fix them. The recovery is `git checkout
//! crates/celeste-kernels/src`, build, regenerate; or use
//! `./regen-generated.sh`, which regenerates into a scratch dir and only
//! installs files that build.
//!
//! `names` (the IR walk that produced `celeste-names/src/gen.rs`) and
//! `fuse` (the fused specialization set) were deleted with the walk path
//! (plans/delete-the-interpreter.md Phase 1); `gen.rs` is frozen.

pub mod bdd;
pub mod graph;
pub mod ival;
pub mod kernel;
pub(crate) mod lower;
