//! The graph IR the tracer builds and its lowering to kernels.
//!
//! `graph` is the value graph the tracer (`src/trace`) builds, `bdd` and
//! `ival` decide its boolean and interval layers, `lower` specializes it
//! into the fused fork-free frame, `kernel` holds the lowering's records,
//! `asm` assembles the result into AVX-512 kernels, and `why` explains a
//! graph for diagnostics.

pub mod bdd;
pub mod garith;
pub mod graph;
pub mod ival;
pub mod kernel;
pub(crate) mod lower;
pub(crate) mod why;

/// The AVX-512 assembly backend: graph -> GAS text -> gcc -> dlopen.
pub mod asm;
