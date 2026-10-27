//! The AVX-512 backend: a traced kernel graph is emitted as LLVM IR
//! (`llvm`), built into a shared object by `llc`, GNU `as` and `ld` and
//! loaded with `dlopen` (`jit`). Ops it does not vectorise inline go through
//! call-outs (`callout`). `tests` checks every op, in both arithmetics, bit
//! for bit against the arithmetic's own scalar operations.

mod callout;
mod jit;
mod llvm;

pub use callout::{AsmCtx, CallLanes, CollisionEnv};
pub(crate) use jit::toolchain;
pub use jit::{check_mxcsr, KernelFn, Loaded};
pub(crate) use llvm::{compile_fixed, compile_float};
pub use llvm::{compile, Compiled};

use anyhow::Result;

/// Compile `roots` of `g` to the function `sym`, build it, and load it.
/// Returns the layout metadata (each input cell's by its type in `g`), the
/// digest of `llc`'s text and the stack frame, and the loaded function.
pub fn compile_and_load<A: crate::transpile::garith::GraphArith>(
    g: &crate::transpile::graph::Graph<A>,
    roots: &[crate::transpile::graph::NodeId],
    sym: &str,
) -> Result<(Compiled, Loaded)> {
    let (mut compiled, ll) = compile(g, roots, sym)?;
    let built = jit::build(&ll, sym)?;
    compiled.text_digest = built.text_digest;
    compiled.frame_bytes = built.frame_bytes;
    Ok((compiled, Loaded::open(&built.so, sym)?))
}

#[cfg(test)]
mod tests;
