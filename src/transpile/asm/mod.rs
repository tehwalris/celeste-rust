//! A PROTOTYPE native AVX-512 assembly backend for the row-key hashing
//! slice of a traced kernel graph (`plans/asm-backend.md`).
//!
//! This is ALONGSIDE the production Rust emitter (`super::lower`), not a
//! replacement: it exists to measure the compile-time and runtime cost of
//! emitting AVX-512 directly (assembled with `as`, loaded with `dlopen`)
//! against the graph -> Rust -> rustc+LLVM path. It is not wired into any
//! kernel; `super::tests` is its correctness gate.

mod codegen;
mod jit;

pub use codegen::{compile, Compiled};
pub use jit::{assemble, KernelFn, Loaded};

use anyhow::Result;
use std::path::PathBuf;

/// Compile `roots` of `g` to assembly, assemble it, and load it. Returns
/// the layout metadata and the loaded function.
pub fn compile_and_load(
    g: &crate::transpile::graph::Graph,
    roots: &[crate::transpile::graph::NodeId],
    tag: &str,
) -> Result<(Compiled, Loaded)> {
    let sym = format!("kernel_{tag}");
    let compiled = compile(g, roots, &sym)?;
    let so: PathBuf = assemble(&compiled.asm, tag)?;
    let loaded = Loaded::open(&so, &sym)?;
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
