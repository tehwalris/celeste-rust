//! The tracer: an interpreter over the cart's Lua AST that leaves a graph IR
//! behind, with no rewrites in between.
//!
//! The AST, not a CFG: a join point is syntactic (the end of an `if`), so
//! "trace both arms and merge" is ordinary recursion, and calls are
//! recursion too - nothing is inlined. The cart uses no `while`, `repeat`,
//! generic `for`, `goto`, varargs or metatables.
//!
//! The heap stays concrete: tables, fields, lengths, closure identity and
//! scopes are facts at trace time, so `count(objects)`, `#t` and loop bounds
//! resolve concretely. Only game DATA is symbolic.
//!
//! One interpreter, several domains: `domain::Concrete` (the oracle, builds
//! the initial heap), `domain::Symbolic` (the tracer) and
//! `refdomain::RefDomain` (the reference engine) share the code, so they
//! cannot drift.
//!
//! Values own nothing: `Symbolic` owns the `Graph` and a `Num`/`Bool` is a
//! `NodeId`, so cloning a state to run the other arm clones only the heap,
//! and both arms emit into one hash-consed arena. Equal computations are the
//! same node, so `Sel(c, x, x)` folds to `x` and a merge costs a select only
//! where the arms disagree; the arena stays topologically ordered for free.

pub mod refdomain;
pub mod refdriver;
pub mod refengine;
pub mod refbridge;
pub mod bind;
pub mod cart;
pub mod domain;
pub mod emit;
pub mod error;
#[cfg(test)]
pub mod eval;
pub mod heap;
pub mod iface;
pub mod kernel;
pub mod interp;
pub mod level_minus_one;
pub mod shapes;
pub mod probe;
pub mod state;
pub mod verify;
pub mod widen;
