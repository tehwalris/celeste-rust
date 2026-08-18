//! The code generators for the two CHECKED-IN generated crates.
//!
//! `names` writes `crates/celeste-names/src/gen.rs` (the interned name
//! tables), `kernel` writes `crates/celeste-kernels/src/kernel_gen_*.rs`
//! (the per-class lane kernels). The CLI over both is `src/bin/transpile.rs`.
//!
//! These live in the LIBRARY, not in the binary, for one reason: the
//! generated files are committed, so something has to fail loudly when they
//! drift from the emitter, and a `#[test]` can only call library code. See
//! `generated_is_current` in `names`.
//!
//! Note the bootstrap this creates, because it will bite whoever changes the
//! emitted preamble. `celeste-rust` (this crate) is where the generators
//! live, and it depends - through the frame interface - on `celeste-kernels`,
//! whose contents these generators produce. So if you change what the kernel
//! emitter emits, the checked-in kernels stop compiling and `cargo build
//! --bin transpile` cannot build the tool that would fix them. The recovery
//! is `git checkout crates/celeste-kernels/src`, build, regenerate; or use
//! `./regen-generated.sh`, which regenerates into a scratch dir and only
//! installs files that build.

pub mod kernel;
pub mod names;
