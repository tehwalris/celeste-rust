//! The RESOLVED program: what the fast interpreter executes.
//!
//! Every name is a small integer (`Sym`), every local is a slot in its
//! function's frame, every upvalue is an index into the closure's captured
//! values, every constant is already a `Value`, and every builtin is an
//! enum. Nothing here is a string at run time; the strings live in the
//! `Interner` and come back out only at the boundary (`bridge`) and in
//! error messages.
//!
//! Semantics are the reference interpreter's (`cengine::interp`), not
//! Lua's, wherever the two differ - it is the oracle this must reproduce
//! leaf-for-leaf. `lower.rs` documents each such choice where it makes it.

use rustc_hash::FxHashMap;

use crate::cengine::domain::{Arith, Cmp};
use crate::cengine::fast::heap::Value;

/// An interned name: a global, a field, a string literal, a capture name.
pub type Sym = u32;
pub type FuncId = u32;

#[derive(Default)]
pub struct Interner {
    names: Vec<String>,
    map: FxHashMap<String, Sym>,
}

impl Interner {
    pub fn intern(&mut self, s: &str) -> Sym {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = self.names.len() as Sym;
        self.names.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }

    pub fn get(&self, s: &str) -> Option<Sym> {
        self.map.get(s).copied()
    }

    pub fn name(&self, s: Sym) -> &str {
        &self.names[s as usize]
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }
}

/// The native builtins the reference interpreter implements
/// (`Interp::call_builtin`). `Unsupported` covers the NATIVE names the
/// reference registers but refuses to call (`error`, `__widen_rem`,
/// `__new_vector`); they exist so a boundary state naming them still
/// converts, and calling one bails exactly as the reference does.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Builtin {
    Abs,
    Flr,
    Sin,
    Mget,
    Fget,
    TileFlagAt,
    Print,
    UPrint,
    Printh,
    HintNormalize,
    ArrayTableDropLast,
    SplitByFlr,
    SplitAt,
    NewUnknownBoolean,
    Min,
    Max,
    Unsupported(u8),
}

impl Builtin {
    /// The reference's `&'static str` for this builtin - the same strings
    /// `cengine::cart::NATIVE` holds, so the boundary can hand them back.
    pub fn name(self) -> &'static str {
        match self {
            Builtin::Abs => "abs",
            Builtin::Flr => "flr",
            Builtin::Sin => "sin",
            Builtin::Mget => "mget",
            Builtin::Fget => "fget",
            Builtin::TileFlagAt => "tile_flag_at",
            Builtin::Print => "print",
            Builtin::UPrint => "__print",
            Builtin::Printh => "printh",
            Builtin::HintNormalize => "_hint_normalize",
            Builtin::ArrayTableDropLast => "__array_table_drop_last",
            Builtin::SplitByFlr => "__split_by_flr",
            Builtin::SplitAt => "__split_at",
            Builtin::NewUnknownBoolean => "__new_unknown_boolean",
            Builtin::Min => "min",
            Builtin::Max => "max",
            Builtin::Unsupported(i) => UNSUPPORTED[i as usize],
        }
    }

    pub fn from_name(name: &str) -> Option<Builtin> {
        Some(match name {
            "abs" => Builtin::Abs,
            "flr" => Builtin::Flr,
            "sin" => Builtin::Sin,
            "mget" => Builtin::Mget,
            "fget" => Builtin::Fget,
            "tile_flag_at" => Builtin::TileFlagAt,
            "print" => Builtin::Print,
            "__print" => Builtin::UPrint,
            "printh" => Builtin::Printh,
            "_hint_normalize" => Builtin::HintNormalize,
            "__array_table_drop_last" => Builtin::ArrayTableDropLast,
            "__split_by_flr" => Builtin::SplitByFlr,
            "__split_at" => Builtin::SplitAt,
            "__new_unknown_boolean" => Builtin::NewUnknownBoolean,
            "min" => Builtin::Min,
            "max" => Builtin::Max,
            other => Builtin::Unsupported(UNSUPPORTED.iter().position(|n| *n == other)? as u8),
        })
    }
}

const UNSUPPORTED: &[&str] = &["error", "__widen_rem", "__new_vector"];

pub struct Program {
    pub interner: Interner,
    pub funcs: Vec<Func>,
    /// The cart chunk itself, as a parameterless function.
    pub top: FuncId,
    /// `celeste_names::gen::FN_NAMES` index -> the function it names. Every
    /// closure that crosses the boundary is identified by its fn_id, so this
    /// is how a boundary closure finds its body.
    pub by_fn_id: FxHashMap<u32, FuncId>,
}

pub struct Func {
    /// The name the IR frontend would give it (`player.update`), for
    /// diagnostics.
    pub name: String,
    pub fn_id: Option<u32>,
    pub nparams: u16,
    /// Frame size: parameters first, then every local and loop variable
    /// the body declares, each with its own slot (no reuse).
    pub nslots: u16,
    /// Where each upvalue comes from IN THE ENCLOSING FUNCTION at the
    /// moment the closure is created. Order is the reference's capture
    /// order (`Interp::captures_of`), which is the order the boundary
    /// stores captured values in.
    pub captures: Vec<CaptureSrc>,
    pub capture_names: Vec<Sym>,
    pub body: Block,
}

#[derive(Clone, Copy, Debug)]
pub enum CaptureSrc {
    Local(u16),
    Upval(u16),
}

pub struct Block {
    pub stmts: Vec<Stmt>,
    pub last: Option<Last>,
}

pub enum Last {
    Break,
    Return(Option<Expr>),
}

pub enum Stmt {
    /// `local x = e` (or `local x`, value `None`).
    Local { slot: u16, value: Option<Expr> },
    SetLocal { slot: u16, value: Expr },
    SetGlobal { name: Sym, value: Expr },
    /// `t.k = v`: `table` evaluated first, then `value`.
    SetField { table: Expr, key: Sym, value: Expr },
    /// `t[i] = v`: `table`, then `index`, then `value`.
    SetIndex { table: Expr, index: Expr, value: Expr },
    Call(Expr),
    If { arms: Vec<(Expr, Block)>, els: Option<Block> },
    /// Numeric `for` without a step. `unroll` is the reference's
    /// `unroll_bound` for this loop's limit text, consulted only when a
    /// bound comes out symbolic.
    For { slot: u16, start: Expr, limit: Expr, body: Block, unroll: Option<u32> },
}

pub enum Expr {
    Const(Value),
    Local(u16),
    Upval(u16),
    Global(Sym),
    Field(Box<Expr>, Sym),
    Index(Box<Expr>, Box<Expr>),
    /// The callee, the arguments, and the callee's source text for the
    /// error message (`calling player.move: ...`).
    Call(Box<Expr>, Vec<Expr>, Box<str>),
    Neg(Box<Expr>),
    Len(Box<Expr>),
    Not(Box<Expr>),
    /// The operands and the expression's source text, for `poison`'s
    /// message (which names the expression).
    Arith(Arith, Box<Expr>, Box<Expr>, Box<str>),
    /// `negate` is `~=`.
    Cmp { op: Cmp, negate: bool, lhs: Box<Expr>, rhs: Box<Expr>, src: Box<str> },
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Function(FuncId),
    /// Constructor fields in source order: `key = value` or positional.
    Table(Vec<(Option<Sym>, Expr)>),
}
