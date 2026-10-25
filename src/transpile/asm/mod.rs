//! The AVX-512 assembly backend: a traced kernel graph is compiled to GAS
//! text (`codegen`), assembled with gcc and loaded with `dlopen` (`jit`).
//! Ops it does not vectorise inline go through call-outs (`callout`).
//! `tests` checks every op, in both arithmetics, bit for bit against the
//! arithmetic's own scalar operations.

mod callout;
mod codegen;
mod jit;

pub use callout::{AsmCtx, CallLanes, CollisionEnv};
pub use codegen::{compile, Compiled};
pub(crate) use codegen::{compile_fixed, compile_float};
pub use jit::{assemble, check_mxcsr, KernelFn, Loaded};

use anyhow::Result;
use std::path::PathBuf;

/// Compile `roots` of `g` to assembly as the function `sym`, assemble it,
/// and load it. Returns the layout metadata (each input cell's by its type
/// in `g`) and the loaded function; the text is dropped once loaded (it is
/// GBs across a room's kernel sets; the census compiles its own,
/// `check::census`).
pub fn compile_and_load<A: crate::transpile::garith::GraphArith>(
    g: &crate::transpile::graph::Graph<A>,
    roots: &[crate::transpile::graph::NodeId],
    sym: &str,
) -> Result<(Compiled, Loaded)> {
    let mut compiled = compile(g, roots, sym, false)?;
    let so: PathBuf = assemble(&compiled.asm, sym)?;
    let loaded = Loaded::open(&so, sym)?;
    compiled.asm = String::new();
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
