//! The graph IR the tracer builds and its lowering to kernels.
//!
//! `graph` is the value graph the tracer (`src/trace`) builds, `bdd` and
//! `ival` decide its boolean and interval layers, `lower` specializes it
//! into the fused fork-free frame, `kernel` holds the lowering's inputs and
//! outputs, and `asm` assembles the result into AVX-512 kernels. The
//! analysis CLI is `src/bin/transpile.rs`.

pub mod bdd;
pub mod graph;
pub mod ival;
pub mod kernel;
pub(crate) mod lower;

/// The AVX-512 assembly backend: graph -> GAS text -> gcc -> dlopen.
pub mod asm;
