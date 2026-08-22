//! The program representation, and the two things that only need it.
//!
//! One step above `celeste-core` and below everything else: the IR the Lua
//! compiles to (`ir`), the compiler that produces it (`frontend`), the
//! builtin table it names (`builtins`), and the printer (`print`).
//!
//! It is its own crate for two reasons, both about what must NOT depend on
//! what. The interpreter needs to print an instruction, and the printer
//! used to live under `rewrite` - which made the interpreter depend on the
//! rewrite machinery for one `format_instruction` call, and that edge is
//! the wrong way round. And the emitters need the IR without needing the
//! 38k lines of rewrite rules, which is the whole point of the split
//! (plans/build-time.md).
//!
//! Nothing here knows about the interpreter, the rewrites, the emitters or
//! the search.

#[macro_use(anyhow)]
extern crate anyhow;

pub mod builtins;
pub mod frontend;
pub mod ir;
pub mod print;
