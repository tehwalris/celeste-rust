//! The graph IR the tracer builds and its lowering to kernels.
//!
//! `graph` is the value graph the tracer (`src/trace`) builds, `eval` the
//! one abstract evaluation every decision and fold reads, `lower` specializes it
//! into the fused fork-free frame, `kernel` holds the lowering's records,
//! `asm` builds the result into AVX-512 kernels (LLVM IR, `llc`), and `why`
//! explains a graph for diagnostics.

pub mod eval;
pub mod garith;
pub mod graph;
pub mod kernel;
pub(crate) mod lower;
pub(crate) mod why;

/// The AVX-512 backend: graph -> LLVM IR -> llc + as + ld -> dlopen.
pub mod asm;
