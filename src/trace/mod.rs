//! Getting the graph by TRACING the Lua, with no rewrites in between.
//!
//! See `plans/tracing.md`. The short version: the ~13,000 recipe-driven
//! rewrites exist to flatten the program into something a straight-line
//! symbolic evaluator can walk in one pass. An interpreter that handles
//! control flow itself does not need them.
//!
//! ## Why the AST and not the IR
//!
//! A join point is SYNTACTIC on the AST - the end of an `if` statement is
//! right there - so "trace both arms and merge" is ordinary recursion, and
//! the post-dominator analysis a CFG would need does not exist. Function
//! calls are recursion too: hand the callee the state, get the state back,
//! carry on. Nothing is inlined because nothing has to be.
//!
//! The language this has to cover is small. `frontend.rs` handles seven
//! statement kinds and about nine expression kinds, and the cart uses no
//! `while`, no `repeat`, no generic `for`, no `goto`, no varargs and no
//! metatables.
//!
//! ## Why the heap stays concrete
//!
//! Because it can. Tables, fields, lengths, closure identity and scope
//! structure are all facts at trace time, so `count(objects)`, `#t` and
//! `objects[i]` resolve concretely. Only game DATA is symbolic. That is
//! also why most loops need no unrolling heuristic at all - `for i=1,
//! count(objects)` has a concrete bound. The genuinely symbolic loops are
//! the two pixel-steppers in `move_x`/`move_y` and the tile scans, all
//! with small natural bounds.
//!
//! ## One interpreter, two domains
//!
//! `domain::Concrete` is the ORACLE and builds the initial heap;
//! `domain::Symbolic` is the tracer and leaves a graph behind. They are
//! the same code, which is the only way they cannot drift.
//!
//! ## How both arms of a branch write into ONE graph
//!
//! The values own nothing. `Symbolic` owns the `Graph`; `Num` and `Bool`
//! are both just a `NodeId`, and the interpreter threads `&mut D` through
//! every operation. So cloning a state to run the other arm clones the
//! HEAP - a few hundred u32s - and never the graph, and both arms emit
//! into one arena by construction.
//!
//! Because that arena is hash-consed, anything the two arms compute the
//! same way becomes literally the same node. That is what makes merging
//! affordable: `Sel(c, x, x)` folds to `x`, so a per-cell merge costs a
//! select only where the arms genuinely disagree, not once per heap cell.
//! It also keeps the graph topologically ordered across branches for
//! free, since a node is only ever created after its operands exist.

pub mod domain;
pub mod heap;
pub mod state;
