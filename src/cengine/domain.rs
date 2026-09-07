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
    fn and(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool;
    /// De Morgan by default, which is all `Concrete` needs. `Symbolic`
    /// overrides it to build `Op::Or` directly: in a GRAPH the difference
    /// is three nodes against one, and the detour also destroys the
    /// symmetry, so `a or b` and `b or a` fail to intern together.
    fn or(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool {
        let (na, nb) = (self.not(a), self.not(b));
        let both = self.and(&na, &nb);
        self.not(&both)
    }

    /// The value of this condition, if the domain knows it. `None` sends
    /// the interpreter down both arms.
    fn decide(&self, c: &Self::Bool) -> Option<bool>;

    /// Merge two values under an undecided condition. `Concrete` never
    /// reaches these, because `decide` never returns `None` for it.
    fn sel_num(&mut self, c: &Self::Bool, t: &Self::Num, f: &Self::Num) -> Self::Num;
    fn sel_bool(&mut self, c: &Self::Bool, t: &Self::Bool, f: &Self::Bool) -> Self::Bool;

    /// `mget(x, y)` with coordinates not known at trace time. The map is
    /// data and it is concrete, so this only arises inside a tile scan
    /// whose bounds came out symbolic - and there the emitted kernel does
    /// it in one instruction (`zn_mget`) rather than a lookup table.
    fn mget(&mut self, x: &Self::Num, y: &Self::Num) -> Result<Self::Num>;

    /// `tile_flag_at(x, y, w, h, flag)` where the coordinates are NOT
    /// known at trace time.
    ///
    /// This has to be a primitive rather than traced into, for the same
    /// reason the IR pipeline makes it one (`zn_tile_flag_at`): the Lua
    /// version scans a tile range with a loop whose bounds are derived
    /// from x and y, so tracing it with a symbolic x would need the
    /// unroll machinery for something the emitted kernel does in one
    /// call. Four of the cart's six symbolic loops are this function.
    ///
    /// The caller folds it when everything IS known, so the concrete
    /// domain never reaches here.
    fn tile_flag_at(
        &mut self,
        x: &Self::Num,
        y: &Self::Num,
        w: &Self::Num,
        h: &Self::Num,
        flag: &Self::Num,
    ) -> Result<Self::Bool>;

    /// A fresh UNKNOWN boolean - one of the search's free choices.
    ///
    /// The oracle has no such thing: to run a frame concretely you supply
    /// the actual button values, so `Concrete` refuses rather than
    /// inventing one. That asymmetry is real and worth the loud failure -
    /// it is the difference between running the game and compiling it.
    fn unknown_bool(&mut self) -> Result<Self::Bool>;

    /// How much graph there is, for the tracer's budget check. Zero for a
    /// domain that does not build one.
    fn node_count(&self) -> usize {
        0
    }

    /// A number this value definitely is, if the domain knows it. The
    /// interpreter needs this for things the HEAP depends on - an array
    /// index, a table key, a loop bound - where an unknown is a refusal
    /// rather than a branch.
    fn as_const(&self, v: &Self::Num) -> Option<P8>;

    /// What this value IS, for an error message.
    ///
    /// A refusal that says "`start` is symbolic" leaves the reader
    /// guessing between an input cell, a fold that did not fire, and a
    /// genuine expression - three completely different fixes. Costs
    /// nothing until something refuses.
    fn describe(&self, _v: &Self::Num) -> String {
        "<opaque>".to_string()
    }

    /// Is this value an INTERVAL - a set of numbers rather than one?
    ///
    /// Asked before forking, because forking a value that is already a
    /// single number costs an outcome and buys nothing. Every object
    /// calls `move`, so a room with n moving objects would get 2^2n fork
    /// configurations for the sake of one player.
    fn is_interval(&self, _v: &Self::Num) -> bool {
        false
    }

    /// Fork at `flr`: the value restricted to a fresh fork choice, and
    /// which lanes fall in the chosen fragment.
    ///
    /// `flr` of an interval is not a function - the lane holds points
    /// whose floors differ - so the cart marks the place with
    /// `__split_by_flr` and the program enumerates the cases. This
    /// returns ONE node, not two states: the fragment is a choice, like
    /// a button, and specialization enumerates it (`Choice::Split`).
    ///
    /// The default is the identity, which is what an exact value needs:
    /// one fragment, always valid.
    fn fork_flr(&mut self, v: &Self::Num) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// The premise a fork is taken under: this lane's interval spans at
    /// most as many floors as there are fragments. An obligation, not a
    /// guard - a lane that fails it is REAL and this body cannot run it.
    fn span_ok(&mut self, _v: &Self::Num) -> Self::Bool {
        self.boolean(true)
    }
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
    fn and(&mut self, a: &bool, b: &bool) -> bool {
        *a && *b
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
    fn mget(&mut self, _: &P8, _: &P8) -> Result<P8> {
        bail!("mget with unknown coordinates cannot happen concretely")
    }
    fn tile_flag_at(&mut self, _: &P8, _: &P8, _: &P8, _: &P8, _: &P8) -> Result<bool> {
        bail!("tile_flag_at with unknown coordinates cannot happen concretely")
    }
    fn unknown_bool(&mut self) -> Result<bool> {
        bail!("the concrete domain has no unknown booleans - supply real inputs")
    }
    fn as_const(&self, v: &P8) -> Option<P8> {
        Some(*v)
    }

    fn describe(&self, v: &P8) -> String {
        format!("{:?}", v)
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
}
