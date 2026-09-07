//! `cengine`: a FORK of the reference engine, to be stripped into a concrete
//! executor with a symbolic spd/rem prior (branch `fiber-spike`, 2026-09-03).
//!
//! This is a verbatim copy of the `RefEngine` closure out of `src/trace/` -
//! `interp`, `domain` (minus the `Symbolic` tracer domain), `heap`, `state`,
//! `cart`, `refdomain`, `refdriver` (+ `verify::run_one`), `refbridge`,
//! `refengine` - with `crate::trace::` repointed to `crate::cengine::`. It
//! shares NOTHING with the tracer/lowering/ASM path except the interpreter
//! `State` at its boundary and the row-key hashing, so it can be cut down
//! without touching the oracle the kernels are gated against.
//!
//! The point of the fork: the reference domain already IS "everything
//! concrete except the interval slots, one straight-line path per leaf,
//! forks enumerated by re-execution" - which is the engine the concrete
//! search wants. What it is not is cheap: it pays the interval/fork tax on
//! every op, tree-walks the AST, and clones a heap per path. Cutting that
//! down is the work; `src/trace/` stays as the untouched oracle so every cut
//! can be checked against it (`fiber_fwd`).
//!
//! `src/trace/` is the ORIGINAL and stays the reference. When this engine
//! works, decide what to do with the two copies.

pub mod cart;
pub mod domain;
pub mod fast;
pub mod heap;
pub mod interp;
pub mod refbridge;
pub mod refdomain;
pub mod refdriver;
pub mod refengine;
pub mod state;
