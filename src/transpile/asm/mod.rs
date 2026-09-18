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
    // `CELESTE_ASM_STATS`: per kernel, the instructions and how many of them
    // are stack traffic - the measure codegen changes are judged by (a big
    // kernel's reloads miss the caches: room (3,0), 0.12 instructions per
    // cycle, 2026-09-18).
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
            "[asm stats] {sym}: {insts} instructions, {reloads} reloads ({:.0}%), {spills} spills ({:.0}%), {} spill slots, frame {:.1} MB, {} output slots ({:.1} MB per slice)",
            100.0 * reloads as f64 / insts.max(1) as f64,
            100.0 * spills as f64 / insts.max(1) as f64,
            compiled.spill_slots,
            compiled.frame_bytes as f64 / 1048576.0,
            compiled.n_roots,
            compiled.n_roots as f64 * 128.0 / 1048576.0
        );
    }
    // The assembly TEXT is only the assembler's input: dropped once the
    // .so is loaded (room (2,0)'s 17 kernel sets held ~8 GB after their
    // prebuild, 2026-09-14). `CELESTE_KEEP_ASM` keeps it for a dump.
    if std::env::var_os("CELESTE_KEEP_ASM").is_none() {
        compiled.asm = String::new();
    }
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
