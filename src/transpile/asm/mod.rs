//! A PROTOTYPE native AVX-512 assembly backend for the row-key hashing
//! slice of a traced kernel graph (`plans/asm-backend.md`).
//!
//! This is ALONGSIDE the production Rust emitter (`super::lower`), not a
//! replacement: it exists to measure the compile-time and runtime cost of
//! emitting AVX-512 directly (assembled with `as`, loaded with `dlopen`)
//! against the graph -> Rust -> rustc+LLVM path. It is not wired into any
//! kernel; `super::tests` is its correctness gate.

mod callout;
mod codegen;
mod jit;

pub use callout::{AsmCtx, CollisionEnv};
pub use codegen::{compile, CellRepr, Compiled, RootKind};
pub use jit::{assemble, KernelFn, Loaded};

use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;

/// Compile `roots` of `g` to assembly, assemble it, and load it. Returns
/// the layout metadata and the loaded function. All input cells numeric;
/// use `compile_and_load_reprs` when a cell is bool.
pub fn compile_and_load(
    g: &crate::transpile::graph::Graph,
    roots: &[crate::transpile::graph::NodeId],
    tag: &str,
) -> Result<(Compiled, Loaded)> {
    compile_and_load_reprs(g, roots, tag, &HashMap::new())
}

/// `compile_and_load` with an explicit per-cell input repr (a cell absent
/// from `cell_reprs` is `Num`).
pub fn compile_and_load_reprs(
    g: &crate::transpile::graph::Graph,
    roots: &[crate::transpile::graph::NodeId],
    tag: &str,
    cell_reprs: &HashMap<u32, CellRepr>,
) -> Result<(Compiled, Loaded)> {
    let sym = format!("kernel_{tag}");
    let compiled = compile(g, roots, &sym, cell_reprs)?;
    let so: PathBuf = assemble(&compiled.asm, tag)?;
    let loaded = Loaded::open(&so, &sym)?;
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
