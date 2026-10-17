//! The tracer: an interpreter over the cart's Lua AST that leaves a graph IR
//! behind, with no rewrites in between. The AST, not a CFG: a join point is
//! syntactic (the end of an `if`), so "trace both arms and merge" is
//! ordinary recursion, and calls are recursion too. The cart uses no
//! `while`, `repeat`, generic `for`, `goto`, varargs or metatables. The heap
//! stays concrete (`heap`); only game DATA is symbolic.
//!
//! One interpreter, two domains: `domain::Symbolic` (the tracer) and
//! `refdomain::RefDomain` (the reference engine, the independent oracle)
//! share the code, so they cannot drift. A `Symbolic` value is a `NodeId`,
//! so cloning a state to run the other arm clones only the heap, and both
//! arms emit into one hash-consed arena: a merge costs a select only where
//! the arms disagree.

pub mod refdomain;
#[cfg(test)]
mod oracle;
pub mod bind;
pub mod block;
pub mod cart;
pub mod domain;
pub mod error;
pub mod heap;
pub mod iface;
pub mod lower;
pub mod split;
pub mod walk;
pub mod interp;
pub mod shapes;
#[cfg(test)]
mod probe;
pub mod state;
pub mod frame;
pub mod widen;
