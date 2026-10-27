//! The AVX-512 assembly backend: a traced kernel graph is compiled to GAS
//! text (`codegen`), assembled with gcc and loaded with `dlopen` (`jit`).
//! Ops it does not vectorise inline go through call-outs (`callout`).
//! `tests` checks every op, in both arithmetics, bit for bit against the
//! arithmetic's own scalar operations.
//!
//! PROTOTYPE: with the TEMPORARY cargo feature `llvm`, the kernels are LLVM
//! IR instead (`llvm`), built by `opt` + `llc` + gcc; the A/B of branch
//! `llvm-backend`, never merged as a switch.

mod callout;
#[cfg_attr(feature = "llvm", allow(dead_code))]
mod codegen;
mod jit;
#[cfg(feature = "llvm")]
mod llvm;

pub use callout::{AsmCtx, CallLanes, CollisionEnv};
pub use codegen::{compile, Compiled};
#[cfg(not(feature = "llvm"))]
pub(crate) use codegen::{compile_fixed, compile_float};
#[cfg(feature = "llvm")]
pub(crate) use llvm::{compile_fixed, compile_float, describe as llvm_pipeline};
pub use jit::{assemble, check_mxcsr, KernelFn, Loaded, ToolTimes};

use anyhow::Result;

/// This thread's CPU seconds.
fn thread_cpu() -> f64 {
    // SAFETY: `clock_gettime` writes the given timespec.
    let mut ts: libc::timespec = unsafe { std::mem::zeroed() };
    unsafe { libc::clock_gettime(libc::CLOCK_THREAD_CPUTIME_ID, &mut ts) };
    ts.tv_sec as f64 + ts.tv_nsec as f64 * 1e-9
}

/// Compile `roots` of `g` to assembly as the function `sym`, assemble it,
/// and load it. Returns the layout metadata (each input cell's by its type
/// in `g`) and the loaded function; the text is dropped once loaded (it is
/// GBs across a room's kernel sets; the census compiles its own,
/// `check::census`). TEMPORARY (the prototype's measurement): a `[kbuild]`
/// line per kernel, the emission's and each tool's wall and CPU seconds.
pub fn compile_and_load<A: crate::transpile::garith::GraphArith>(
    g: &crate::transpile::graph::Graph<A>,
    roots: &[crate::transpile::graph::NodeId],
    sym: &str,
) -> Result<(Compiled, Loaded)> {
    let (t0, c0) = (std::time::Instant::now(), thread_cpu());
    let mut compiled = compile(g, roots, sym, false)?;
    let (emit_wall, emit_cpu) = (t0.elapsed().as_secs_f64(), thread_cpu() - c0);
    let mut times = ToolTimes::default();
    #[cfg(not(feature = "llvm"))]
    let so = assemble(&compiled.asm, sym, &mut times)?;
    #[cfg(feature = "llvm")]
    let so = {
        let (so, frame, spills) = llvm::build(&compiled.asm, sym, &mut times)?;
        compiled.frame_bytes = frame;
        compiled.spill_slots = spills;
        so
    };
    let loaded = Loaded::open(&so, sym)?;
    let live = g.reachable(roots).iter().filter(|b| **b).count();
    let tools: String = times.steps.iter().map(|(n, w, c)| format!(" {n} {w:.4} {c:.4}")).collect();
    eprintln!(
        "[kbuild] {sym} nodes {live} roots {} text {} so {} emit {emit_wall:.4} {emit_cpu:.4}{tools}",
        roots.len(),
        compiled.asm.len(),
        std::fs::metadata(&so).map(|m| m.len()).unwrap_or(0)
    );
    compiled.asm = String::new();
    Ok((compiled, loaded))
}

#[cfg(test)]
mod tests;
