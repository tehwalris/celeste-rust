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
    let t0 = std::time::Instant::now();
    let mut compiled = compile(g, roots, &sym, cell_reprs)?;
    let t_compile = t0.elapsed();
    let t1 = std::time::Instant::now();
    let so: PathBuf = assemble(&compiled.asm, tag)?;
    let t_asm = t1.elapsed();
    let loaded = Loaded::open(&so, &sym)?;
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!(
            "[build] {tag}: {} roots, asm text {:.1} MB: compile {:.1}s, gcc+load {:.1}s",
            roots.len(),
            compiled.asm.len() as f64 / 1e6,
            t_compile.as_secs_f64(),
            t_asm.as_secs_f64()
        );
    }
    // `CELESTE_ASM_STATS`: per kernel, the instruction count and the share of
    // stack traffic (a big kernel's reloads miss the caches).
    if std::env::var_os("CELESTE_ASM_STATS").is_some() {
        let (mut insts, mut reloads, mut spills) = (0usize, 0usize, 0usize);
        for line in compiled.asm.lines() {
            let Some(body) = line.strip_prefix("    ") else { continue };
            if body.starts_with('.') {
                continue;
            }
            insts += 1;
            if let Some(ops) = body.strip_prefix("vmovdqu64 ") {
                if ops.contains("(%rsp), %zmm") {
                    reloads += 1;
                } else if ops.starts_with("%zmm") && ops.ends_with("(%rsp)") {
                    spills += 1;
                }
            }
        }
        eprintln!(
            "[asm stats] {sym}: {insts} instructions, {reloads} reloads ({:.0}%), {spills} spills ({:.0}%), {} spill slots, frame {:.1} MB, {} output roots ({:.1} MB per slice)",
            100.0 * reloads as f64 / insts.max(1) as f64,
            100.0 * spills as f64 / insts.max(1) as f64,
            compiled.spill_slots,
            compiled.frame_bytes as f64 / 1048576.0,
            compiled.n_roots,
            compiled.out_bytes as f64 / 1048576.0
        );
        let count = |k: RootKind| compiled.root_kinds.iter().filter(|r| **r == k).count();
        eprintln!(
            "[asm stats] {sym}: output slots by kind: {} num, {} interval, {} bool",
            count(RootKind::Num),
            count(RootKind::Ival),
            count(RootKind::Bool)
        );
    }
    // The assembly text is only the assembler's input: drop it once loaded
    // (it is GBs across a room's kernel sets).
    compiled.asm = String::new();
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
