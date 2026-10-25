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

/// What an assembly is built with: `mix` (`bench-frame --metrics`:
/// instruction provenance kept, the assembly text kept for the kernel
/// census).
#[derive(Clone, Copy, Debug, Default)]
pub struct AsmOptions {
    pub mix: bool,
}

/// Compile `roots` of `g` to assembly, assemble it, and load it. Returns
/// the layout metadata (each input cell's by its type in `g`) and the
/// loaded function.
pub fn compile_and_load<A: crate::transpile::garith::GraphArith>(
    g: &crate::transpile::graph::Graph<A>,
    roots: &[crate::transpile::graph::NodeId],
    tag: &str,
    opts: AsmOptions,
) -> Result<(Compiled, Loaded)> {
    let sym = format!("kernel_{tag}");
    let mut compiled = compile(g, roots, &sym, opts)?;
    let so: PathBuf = assemble(&compiled.asm, tag)?;
    let loaded = Loaded::open(&so, &sym)?;
    // The assembly text is only the assembler's input: drop it once loaded
    // (it is GBs across a room's kernel sets). The mix diagnostic reads it.
    if !opts.mix {
        compiled.asm = String::new();
    }
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
