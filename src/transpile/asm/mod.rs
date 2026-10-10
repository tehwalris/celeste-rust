//! The AVX-512 assembly backend: a traced kernel graph is compiled to GAS
//! text (`codegen`), assembled with gcc and loaded with `dlopen` (`jit`).
//! Ops it does not vectorise inline go through call-outs (`callout`).
//! `tests` checks every op bit-exact against the Rust lane primitives.

mod callout;
mod codegen;
mod jit;

pub use callout::{AsmCtx, CollisionEnv};
pub use codegen::{compile, CellRepr, Compiled, RootKind};
pub use jit::{assemble, KernelFn, Loaded};

use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;

/// What an assembly is built with: `trace` (`--build-trace`: the build's
/// progress lines) and `mix` (`--kernel-mix` or `bench-frame --metrics`:
/// instruction provenance kept, the assembly text kept for the report).
#[derive(Clone, Copy, Debug, Default)]
pub struct AsmOptions {
    pub trace: bool,
    pub mix: bool,
}

/// Compile `roots` of `g` to assembly, assemble it, and load it. Returns
/// the layout metadata and the loaded function. All input cells numeric;
/// use `compile_and_load_reprs` when a cell is bool.
pub fn compile_and_load(
    g: &crate::transpile::graph::Graph,
    roots: &[crate::transpile::graph::NodeId],
    tag: &str,
) -> Result<(Compiled, Loaded)> {
    compile_and_load_reprs(g, roots, tag, &HashMap::new(), AsmOptions::default())
}

/// `compile_and_load` with an explicit per-cell input repr (a cell absent
/// from `cell_reprs` is `Num`).
pub fn compile_and_load_reprs(
    g: &crate::transpile::graph::Graph,
    roots: &[crate::transpile::graph::NodeId],
    tag: &str,
    cell_reprs: &HashMap<u32, CellRepr>,
    opts: AsmOptions,
) -> Result<(Compiled, Loaded)> {
    let sym = format!("kernel_{tag}");
    let t0 = std::time::Instant::now();
    let mut compiled = compile(g, roots, &sym, cell_reprs, opts)?;
    let t_compile = t0.elapsed();
    let t1 = std::time::Instant::now();
    let so: PathBuf = assemble(&compiled.asm, tag)?;
    let t_asm = t1.elapsed();
    let loaded = Loaded::open(&so, &sym)?;
    if opts.trace {
        eprintln!(
            "[build] {tag}: {} roots, asm text {:.1} MB: compile {:.1}s, gcc+load {:.1}s",
            roots.len(),
            compiled.asm.len() as f64 / 1e6,
            t_compile.as_secs_f64(),
            t_asm.as_secs_f64()
        );
    }
    // The assembly text is only the assembler's input: drop it once loaded
    // (it is GBs across a room's kernel sets). The mix diagnostic reads it.
    if !opts.mix {
        compiled.asm = String::new();
    }
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
