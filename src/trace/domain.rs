//! The value domain the tracer is generic over.
//!
//! One interpreter, two instantiations:
//!
//! * `Concrete` - `Num = Pico8Num`, `Bool = bool`. This is the ORACLE.
//!   It also builds the initial heap by running the cart's top level.
//! * `Symbolic` - `Num = Bool = NodeId` into a `transpile::graph::Graph`.
//!   This is the TRACER; what it leaves behind is the graph.
//!
//! They are the same code because that is the only way they cannot drift.
//! An oracle that is a separate implementation is a second definition of
//! what the program means, and then two things have to be kept true at
//! once (`plans/tracing.md`).
//!
//! ## The one method that matters
//!
//! `decide` asks "can you tell me this condition's value RIGHT NOW".
//! `Concrete` always can. `Symbolic` can only when the node folded to a
//! constant. `None` is exactly the case where the interpreter stops being
//! an interpreter and starts being a compiler: it traces both arms and
//! merges them with `sel_*`.
//!
//! So the concrete instantiation never calls `sel_*` at all, and the
//! symbolic one calls it only where the program genuinely branches on
//! something it cannot know - which, because the HEAP stays concrete, is
//! far less often than it sounds. `count(objects)` is a number, `#t` is a
//! number, a table field's presence is a fact; only game data is symbolic.

use std::fmt::Debug;

use anyhow::{bail, Result};

use crate::pico8_num::Pico8Num as P8;
use crate::transpile::graph::{Graph, NodeId, Op};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
}

/// A numeric builtin that is the same shape in both domains.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Fun1 {
    Neg,
    Abs,
    Flr,
    Sin,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Fun2 {
    Min,
    Max,
}

pub trait Domain {
    type Num: Clone + Debug + PartialEq;
    type Bool: Clone + Debug + PartialEq;

    fn num(&mut self, v: P8) -> Self::Num;
    fn boolean(&mut self, b: bool) -> Self::Bool;

    fn arith(&mut self, op: Arith, a: &Self::Num, b: &Self::Num) -> Result<Self::Num>;
    fn fun1(&mut self, f: Fun1, a: &Self::Num) -> Result<Self::Num>;
    fn fun2(&mut self, f: Fun2, a: &Self::Num, b: &Self::Num) -> Result<Self::Num>;
    fn compare(&mut self, op: Cmp, a: &Self::Num, b: &Self::Num) -> Result<Self::Bool>;
    fn not(&mut self, a: &Self::Bool) -> Self::Bool;

    /// The value of this condition, if the domain knows it. `None` sends
    /// the interpreter down both arms.
    fn decide(&self, c: &Self::Bool) -> Option<bool>;

    /// Merge two values under an undecided condition. `Concrete` never
    /// reaches these, because `decide` never returns `None` for it.
    fn sel_num(&mut self, c: &Self::Bool, t: &Self::Num, f: &Self::Num) -> Self::Num;
    fn sel_bool(&mut self, c: &Self::Bool, t: &Self::Bool, f: &Self::Bool) -> Self::Bool;

    /// A fresh UNKNOWN boolean - one of the search's free choices.
    ///
    /// The oracle has no such thing: to run a frame concretely you supply
    /// the actual button values, so `Concrete` refuses rather than
    /// inventing one. That asymmetry is real and worth the loud failure -
    /// it is the difference between running the game and compiling it.
    fn unknown_bool(&mut self) -> Result<Self::Bool>;

    /// A number this value definitely is, if the domain knows it. The
    /// interpreter needs this for things the HEAP depends on - an array
    /// index, a table key, a loop bound - where an unknown is a refusal
    /// rather than a branch.
    fn as_const(&self, v: &Self::Num) -> Option<P8>;
}

// ---------------------------------------------------------------- concrete

#[derive(Default)]
pub struct Concrete;

impl Domain for Concrete {
    type Num = P8;
    type Bool = bool;

    fn num(&mut self, v: P8) -> P8 {
        v
    }
    fn boolean(&mut self, b: bool) -> bool {
        b
    }
    fn arith(&mut self, op: Arith, a: &P8, b: &P8) -> Result<P8> {
        Ok(match op {
            Arith::Add => *a + *b,
            Arith::Sub => *a - *b,
            Arith::Mul => *a * *b,
            Arith::Div => *a / *b,
            Arith::Rem => *a % *b,
        })
    }
    fn fun1(&mut self, f: Fun1, a: &P8) -> Result<P8> {
        Ok(match f {
            Fun1::Neg => -*a,
            Fun1::Abs => a.abs(),
            Fun1::Flr => a.flr(),
            Fun1::Sin => a.pico8_sin(),
        })
    }
    fn fun2(&mut self, f: Fun2, a: &P8, b: &P8) -> Result<P8> {
        Ok(match f {
            Fun2::Min => (*a).min(*b),
            Fun2::Max => (*a).max(*b),
        })
    }
    fn compare(&mut self, op: Cmp, a: &P8, b: &P8) -> Result<bool> {
        Ok(match op {
            Cmp::Lt => a < b,
            Cmp::Le => a <= b,
            Cmp::Gt => a > b,
            Cmp::Ge => a >= b,
            Cmp::Eq => a == b,
        })
    }
    fn not(&mut self, a: &bool) -> bool {
        !*a
    }
    fn decide(&self, c: &bool) -> Option<bool> {
        Some(*c)
    }
    fn sel_num(&mut self, c: &bool, t: &P8, f: &P8) -> P8 {
        if *c {
            *t
        } else {
            *f
        }
    }
    fn sel_bool(&mut self, c: &bool, t: &bool, f: &bool) -> bool {
        if *c {
            *t
        } else {
            *f
        }
    }
    fn unknown_bool(&mut self) -> Result<bool> {
        bail!("the concrete domain has no unknown booleans - supply real inputs")
    }
    fn as_const(&self, v: &P8) -> Option<P8> {
        Some(*v)
    }
}

// ---------------------------------------------------------------- symbolic

/// The tracing domain. Owns the graph it is building.
#[derive(Default)]
pub struct Symbolic {
    pub graph: Graph,
    /// How many free choices have been handed out. `__reset_button_states`
    /// asks for six, in slot order, which is what makes these line up with
    /// the kb0..kb5 the kernels already speak.
    pub frees: u8,
}

impl Symbolic {
    fn konst(&mut self, v: P8) -> NodeId {
        let raw = v.as_raw_u32() as i32;
        self.graph.leaf(Op::Const(raw, raw))
    }
    /// The constant a node is, if it is one.
    fn as_p8(&self, n: NodeId) -> Option<P8> {
        match self.graph.get(n).op {
            Op::Const(lo, hi) if lo == hi => Some(P8::from_raw(lo)),
            _ => None,
        }
    }
}

impl Domain for Symbolic {
    type Num = NodeId;
    type Bool = NodeId;

    fn num(&mut self, v: P8) -> NodeId {
        self.konst(v)
    }
    fn boolean(&mut self, b: bool) -> NodeId {
        self.graph.leaf(Op::ConstBool(b))
    }
    fn arith(&mut self, op: Arith, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        // Fold where both sides are known, so that everything the heap
        // depends on - indices, counts, loop bounds - stays concrete
        // without the interpreter having to ask.
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            return Ok(self.konst(c.arith(op, &x, &y)?));
        }
        let g = match op {
            Arith::Add => Op::Add,
            Arith::Sub => Op::Sub,
            Arith::Mul => Op::Mul,
            Arith::Div => Op::Div,
            Arith::Rem => Op::Rem,
        };
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn fun1(&mut self, f: Fun1, a: &NodeId) -> Result<NodeId> {
        if let Some(x) = self.as_p8(*a) {
            let mut c = Concrete;
            return Ok(self.konst(c.fun1(f, &x)?));
        }
        let g = match f {
            Fun1::Neg => Op::Neg,
            Fun1::Abs => Op::Abs,
            Fun1::Flr => Op::Flr,
            Fun1::Sin => Op::Sin,
        };
        Ok(self.graph.fold(g, vec![*a]))
    }
    fn fun2(&mut self, f: Fun2, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            return Ok(self.konst(c.fun2(f, &x, &y)?));
        }
        let g = match f {
            Fun2::Min => Op::Min,
            Fun2::Max => Op::Max,
        };
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn compare(&mut self, op: Cmp, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            let r = c.compare(op, &x, &y)?;
            return Ok(self.graph.leaf(Op::ConstBool(r)));
        }
        let g = match op {
            Cmp::Lt => Op::Lt,
            Cmp::Le => Op::Le,
            Cmp::Gt => Op::Gt,
            Cmp::Ge => Op::Ge,
            Cmp::Eq => Op::Eq,
        };
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn not(&mut self, a: &NodeId) -> NodeId {
        self.graph.fold(Op::Not, vec![*a])
    }
    fn decide(&self, c: &NodeId) -> Option<bool> {
        match self.graph.get(*c).op {
            Op::ConstBool(b) => Some(b),
            _ => None,
        }
    }
    fn sel_num(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> NodeId {
        self.graph.fold(Op::Sel, vec![*c, *t, *f])
    }
    fn sel_bool(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> NodeId {
        self.graph.fold(Op::Sel, vec![*c, *t, *f])
    }
    fn unknown_bool(&mut self) -> Result<NodeId> {
        if self.frees >= 6 {
            bail!("more than six free choices - the kernels only model six buttons");
        }
        let b = self.frees;
        self.frees += 1;
        Ok(self.graph.leaf(Op::Free(b)))
    }
    fn as_const(&self, v: &NodeId) -> Option<P8> {
        self.as_p8(*v)
    }
}

/// A refusal the tracer makes rather than guessing. Kept separate from
/// ordinary errors because these are the interesting ones: each is a place
/// the heap would have had to become symbolic.
pub fn refuse_unknown(what: &str) -> anyhow::Error {
    anyhow::anyhow!(
        "{} is not known at trace time - the heap cannot depend on a symbolic value",
        what
    )
}

#[allow(dead_code)]
fn _assert_object_safe(_: &dyn Fn(&mut Symbolic)) {}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(v: i16) -> P8 {
        P8::from_i16(v)
    }

    #[test]
    fn the_concrete_domain_always_decides() {
        let mut d = Concrete;
        let a = d.num(p(3));
        let b = d.num(p(4));
        let lt = d.compare(Cmp::Lt, &a, &b).unwrap();
        assert_eq!(d.decide(&lt), Some(true));
        // Which is why it never merges: `decide` returning Some is exactly
        // the condition under which the interpreter takes one arm.
        let sum = d.arith(Arith::Add, &a, &b).unwrap();
        assert_eq!(d.as_const(&sum), Some(p(7)));
    }

    #[test]
    fn the_symbolic_domain_folds_what_it_can() {
        let mut d = Symbolic::default();
        let a = d.num(p(3));
        let b = d.num(p(4));
        let sum = d.arith(Arith::Add, &a, &b).unwrap();
        // Constant-folded, so the HEAP can still index with it. This is
        // what keeps `count(objects)`, `#t` and loop bounds concrete
        // without the interpreter special-casing them.
        assert_eq!(d.as_const(&sum), Some(p(7)));
        let lt = d.compare(Cmp::Lt, &a, &b).unwrap();
        assert_eq!(d.decide(&lt), Some(true));
    }

    #[test]
    fn an_unknown_condition_is_what_makes_the_interpreter_branch() {
        let mut d = Symbolic::default();
        let x = d.graph.leaf(Op::Cell(1)); // stands in for game data
        let k = d.num(p(0));
        let gt = d.compare(Cmp::Gt, &x, &k).unwrap();
        assert_eq!(d.decide(&gt), None, "this is the case that merges");
        // And the merge itself collapses when the arms agree.
        let t = d.num(p(1));
        assert_eq!(d.sel_num(&gt, &t, &t), t);
        assert_ne!(d.sel_num(&gt, &t, &k), t);
    }
}
