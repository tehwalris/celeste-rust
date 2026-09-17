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

    /// A number known only to lie in `[lo, hi]` - `rnd`'s value. Like
    /// `unknown_bool` this is a thing only an abstract domain has: the
    /// concrete oracle refuses rather than inventing a draw.
    fn range_num(&mut self, lo: P8, hi: P8) -> Result<Self::Num> {
        bail!("a number in [{lo:?}, {hi:?}] has no concrete value - this domain runs real inputs only")
    }

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

    /// The premise that `b` is DECIDED on this lane (`Op::Known`). A
    /// concrete boolean always is.
    fn known(&mut self, _b: &Self::Bool) -> Self::Bool {
        self.boolean(true)
    }

    /// Is a merged value a SELECT the kernel reads by its condition's value
    /// bit (`Op::Sel`), rather than a value the merge folded away (equal
    /// arms, a decided condition) or into Kleene boolean algebra, which the
    /// kernel evaluates exactly on (value, known) masks? Only the former
    /// needs the merge's condition decided (`state::merge`). A concrete
    /// merge never selects.
    fn is_select_num(&self, _v: &Self::Num) -> bool {
        false
    }

    /// `is_select_num` for a boolean.
    fn is_select_bool(&self, _b: &Self::Bool) -> bool {
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
    /// `ways` is the fork's arity - how many floors the fragments cover.
    ///
    /// The default is the identity, which is what an exact value needs:
    /// one fragment, always valid.
    fn fork_flr(&mut self, v: &Self::Num, _ways: u8) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// A fork over an interval of WHOLE numbers whose fragments are the
    /// numbers themselves, each EXACT (`Op::SplitInt`): the player's
    /// position under a bucket. Same validity and premise as `fork_flr`.
    fn fork_int(&mut self, v: &Self::Num, _ways: u8) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// A RELATIVE fork at a TABLE of ranges (`Op::SplitTab`) with `arity`
    /// fragments: fragment `c` is the value clipped to the `c`-th entry
    /// from the one its low end lies in, valid where the value reaches it.
    /// The default is the identity (an exact value is in one range).
    fn fork_table(&mut self, v: &Self::Num, _ranges: &[(i32, i32)], _arity: u8) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// The arity of the fork `__split_by_flr` takes on `v`: `move_ways`,
    /// or where the trace knows `v`'s static range (a kernel specialized
    /// on its speed key) the most grid cells one piece of it crosses -
    /// a lane's interval lies within one piece, and `SplitOk` checks it.
    fn flr_ways(&mut self, _v: &Self::Num) -> u8 {
        self.move_ways()
    }

    /// The premise a fork is taken under: this lane's interval spans at
    /// most `ways` floors, as many as there are fragments. An
    /// obligation, not a guard - a lane that fails it is REAL and this
    /// body cannot run it.
    fn span_ok(&mut self, _v: &Self::Num, _ways: u8) -> Self::Bool {
        self.boolean(true)
    }

    /// The arity of the `move` fork (`__split_by_flr`): 2 unless the
    /// traced set buckets the player's speed, where an object updating
    /// before the player (the spring: `hit.spd.x *= 0.2`) can hand
    /// `move` a bucket no longer aligned to the grid, and `rem + spd +
    /// 0.5` then spans THREE floors (room (2,0) f36, 2026-09-14).
    fn move_ways(&self) -> u8 {
        2
    }

    /// The join of a merge on a condition NO LANE can decide, where the arms
    /// are values every lane holds alike (plans/fly-fruit.md): the hull of
    /// two literal intervals, or the unknown number. `None` keeps the
    /// ordinary select (and its `Known` premise). Only a fruit-unknown set
    /// has such conditions; the concrete domain never merges.
    fn join_num_independent(&mut self, _c: &Self::Bool, _t: &Self::Num, _f: &Self::Num) -> Option<Self::Num> {
        None
    }

    /// `join_num_independent` for booleans: two different arms join to an
    /// undecided atom.
    fn join_bool_independent(&mut self, _c: &Self::Bool, _t: &Self::Bool, _f: &Self::Bool) -> Option<Self::Bool> {
        None
    }

    /// Does the undecided condition `c` read an unknown atom (`Op::UnknownBool`)?
    /// No lane ever decides an atom, so wherever `c` depends on one it stays
    /// undecided: a merge on `c` that would leave a select is not a merge but two
    /// successors, since those lanes would decline the select's `Known(c)`
    /// premise (`state::merge`).
    fn reads_unknown_atom(&mut self, _c: &Self::Bool) -> bool {
        false
    }

    /// `__split_by_flr` of a value every lane holds alike (a literal interval):
    /// its fragments on the fork grid, as literals, for the interpreter to run
    /// as separate trace states and rejoin when the enclosing call returns
    /// (`Interp::rejoin_fragments`). `None`: an ordinary per-lane fork.
    fn literal_fragments(&mut self, _v: &Self::Num) -> Option<Vec<Self::Num>> {
        None
    }

    /// A fresh undecided boolean every lane holds alike: what a literal split's
    /// fragments are merged on when they rejoin.
    fn undecided_atom(&mut self) -> Result<Self::Bool> {
        bail!("this domain has no undecided booleans")
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

// ---------------------------------------------------------------- symbolic

/// The tracing domain. Owns the graph it is building.
#[derive(Default, Clone)]
pub struct Symbolic {
    pub graph: Graph,
    /// How many free choices have been handed out. `__reset_button_states`
    /// asks for six, in slot order, which is what makes these line up with
    /// the kb0..kb5 the kernels already speak.
    pub frees: u8,
    /// How many FORK choices have been handed out this frame. The other
    /// half of `ChoiceSet`, above the six buttons.
    pub forks: u8,
    /// The arity of the `move` fork for the set being traced (see
    /// `Domain::move_ways`); `trace_frame` sets it from the widen mode.
    /// 0 (the default) reads as 2.
    pub move_ways: u8,
    /// Held buttons unknown for the set being traced
    /// (`abstraction::HeldPrecision`): `trace_frame` forks the player's
    /// `p_jump` / `p_dash` (`widen::fork_held_inputs`). Set by the walk.
    pub held_unknown: bool,
    /// The fly fruit unknown for the set being traced
    /// (`abstraction::FruitPrecision`, plans/fly-fruit.md): `trace_frame`
    /// replaces the fruit's inputs (`widen::fork_fruit_inputs`), arithmetic on
    /// literal intervals folds to literals, and a merge on a condition no lane
    /// decides joins its literal arms (`join_num_independent`). Set by the walk.
    pub fruit_unknown: bool,
    /// How many `Op::UnknownBool` atoms this frame handed out.
    pub unknown_atoms: u32,
    /// `lane_independent`'s memo. Structural (a node's op and operands never
    /// change), so it outlives a frame.
    lane_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// `reads_unknown_atom`'s memo: structural, like `lane_memo`.
    atom_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// Fork choices already handed out THIS FRAME, by the value forked.
    ///
    /// Two call sites that floor the same value do not need two choice
    /// dimensions: forking one value twice yields the same fragments,
    /// so the second choice is determined by the first and every
    /// configuration where they disagree is empty. The runtime masks
    /// prune those, so it was never wrong - it was two extra levels of
    /// loop nest and a block of emitted nodes each.
    ///
    /// Measured on room (2,0) before this existed: `kernel1` and
    /// `kernel4` forked 16 times over 14 distinct values, which is
    /// ~10,600 of their 83,710 nodes (12.7%) and 65,536 runtime
    /// configurations instead of 16,384.
    ///
    /// Per frame, like `forks` itself - cleared beside it.
    pub fork_memo: std::collections::HashMap<NodeId, (u8, (NodeId, NodeId))>,
    /// `fork_memo` for `fork_int` (a different op over the same value).
    pub fork_memo_int: std::collections::HashMap<NodeId, (u8, (NodeId, NodeId))>,
    /// Input cells that hold an INTERVAL rather than a number - the
    /// player's `rem.x`/`rem.y`, which the boundary widens.
    ///
    /// By CELL, in the tracer's own dense numbering, because that is what
    /// a graph node names. `is_interval` is a cone query against this
    /// set: a value is an interval exactly when it was computed from one.
    pub ival_cells: std::collections::BTreeSet<u32>,
    /// STATIC RANGES (2026-09-15, the bucket dispatch): input cells whose
    /// value is known to lie in a range - a body specialized on the
    /// player's speed BUCKET - and the memo of the range analysis over
    /// them (`range_of`). A comparison both of whose operands have ranges
    /// that decide it folds to a constant (`compare`), so the branch is
    /// never traced and the merge never built. Per frame, like `forks`.
    pub ranges: std::collections::HashMap<NodeId, (i64, i64)>,
    range_memo: std::collections::HashMap<NodeId, Option<Pieces>>,
    /// Comparisons `compare` decided from ranges this frame (a probe stat).
    pub range_folds: u64,
}

pub use crate::transpile::graph::Pieces;

impl Symbolic {
    /// Forget the static ranges (a new frame, new cells).
    pub fn clear_ranges(&mut self) {
        self.ranges.clear();
        self.range_memo.clear();
        self.range_folds = 0;
    }

    /// The static range of `n` (`graph::pieces_of` over the seeded cells).
    pub fn range_of(&mut self, n: NodeId) -> Option<Pieces> {
        crate::transpile::graph::pieces_of(&self.graph, &self.ranges, &mut self.range_memo, n)
    }

    /// The unknown number (`Op::UnknownNum`).
    pub fn unknown_num(&mut self) -> NodeId {
        self.graph.leaf(Op::UnknownNum)
    }

    fn is_unknown_num(&self, n: NodeId) -> bool {
        matches!(self.graph.get(n).op, Op::UnknownNum)
    }

    /// A fresh undecided atom (`Op::UnknownBool`), distinct from every other
    /// this frame.
    pub fn unknown_bool_atom(&mut self) -> NodeId {
        let k = self.unknown_atoms;
        self.unknown_atoms += 1;
        self.graph.leaf(Op::UnknownBool(k))
    }

    /// Does no lane's data reach `n` - no input cell, button or fork?
    pub fn lane_independent(&mut self, n: NodeId) -> bool {
        let mut stack: Vec<(NodeId, bool)> = vec![(n, false)];
        while let Some((x, expanded)) = stack.pop() {
            if self.lane_memo.contains_key(&x) {
                continue;
            }
            let node = self.graph.get(x);
            let dependent = matches!(
                node.op,
                Op::Cell(_) | Op::Free(_) | Op::Split(_) | Op::SplitValid(_) | Op::SplitInt(_) | Op::SplitTab(_) | Op::SplitValidTab(_) | Op::SplitKeyTab(_) | Op::SplitOkTab(_)
            );
            if dependent || node.args.is_empty() {
                self.lane_memo.insert(x, !dependent);
                continue;
            }
            let args = node.args.clone();
            if expanded {
                let r = args.iter().all(|a| self.lane_memo[a]);
                self.lane_memo.insert(x, r);
            } else {
                stack.push((x, true));
                stack.extend(args.iter().filter(|a| !self.lane_memo.contains_key(a)).map(|a| (*a, false)));
            }
        }
        self.lane_memo[&n]
    }

    /// `op` over LITERAL operands, at least one an interval, in the graph's
    /// own interval semantics (`Graph::eval` on the literals: one definition,
    /// not a second): the value, or `None` where eval does not model it (a
    /// wrap at the 16.16 extremes, a non-positive scale). A fruit-unknown set
    /// only.
    fn eval_literal(&self, op: &Op, args: &[NodeId]) -> Option<crate::transpile::graph::Val> {
        if !self.fruit_unknown {
            return None;
        }
        let lits: Vec<(i32, i32)> = args
            .iter()
            .map(|a| match self.graph.get(*a).op {
                Op::Const(lo, hi) => Some((lo, hi)),
                _ => None,
            })
            .collect::<Option<_>>()?;
        if lits.iter().all(|(lo, hi)| lo == hi) {
            return None;
        }
        let mut g = Graph::new();
        let leaves: Vec<NodeId> = lits.iter().map(|(lo, hi)| g.leaf(Op::Const(*lo, *hi))).collect();
        let n = g.add(op.clone(), leaves);
        g.eval(&std::collections::HashMap::new()).ok().map(|v| v[n as usize])
    }

    /// `eval_literal`'s number as a literal node.
    fn literal_num(&mut self, op: Op, args: &[NodeId]) -> Option<NodeId> {
        match self.eval_literal(&op, args)? {
            crate::transpile::graph::Val::Num(iv) => {
                Some(self.graph.leaf(Op::Const(iv.low.as_raw_u32() as i32, iv.high.as_raw_u32() as i32)))
            }
            crate::transpile::graph::Val::Bool(_) => None,
        }
    }

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
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(self.unknown_num());
        }
        let g = match op {
            Arith::Add => Op::Add,
            Arith::Sub => Op::Sub,
            Arith::Mul => Op::Mul,
            Arith::Div => Op::Div,
            Arith::Rem => Op::Rem,
        };
        if let Some(n) = self.literal_num(g.clone(), &[*a, *b]) {
            return Ok(n);
        }
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn fun1(&mut self, f: Fun1, a: &NodeId) -> Result<NodeId> {
        if let Some(x) = self.as_p8(*a) {
            let mut c = Concrete;
            return Ok(self.konst(c.fun1(f, &x)?));
        }
        // `sin` of anything is in [-1, 1]; every other builtin of an unknown
        // number is unknown.
        if self.is_unknown_num(*a) {
            return Ok(match f {
                Fun1::Sin => self.graph.leaf(Op::Const(-0x1_0000, 0x1_0000)),
                _ => self.unknown_num(),
            });
        }
        // `sin` of an interval is its full range [-1, 1], matching the
        // interpreter's `builtin_sin` (game_runner.rs) exactly. Emit the
        // constant range so the emitter never has to lower a `Sin` over a
        // ZI (it cannot). This is the fruit bob's `sin((1+off)/40)` once
        // the boundary has widened `off` - see plans/specialize.md.
        if matches!(f, Fun1::Sin) && self.is_interval(a) {
            let lo = self.konst(P8::from_i16(-1));
            let hi = self.konst(P8::from_i16(1));
            return Ok(self.graph.fold(Op::Span, vec![lo, hi]));
        }
        let g = match f {
            Fun1::Neg => Op::Neg,
            Fun1::Abs => Op::Abs,
            Fun1::Flr => Op::Flr,
            Fun1::Sin => Op::Sin,
        };
        if let Some(n) = self.literal_num(g.clone(), &[*a]) {
            return Ok(n);
        }
        Ok(self.graph.fold(g, vec![*a]))
    }
    fn fun2(&mut self, f: Fun2, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            return Ok(self.konst(c.fun2(f, &x, &y)?));
        }
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(self.unknown_num());
        }
        let g = match f {
            Fun2::Min => Op::Min,
            Fun2::Max => Op::Max,
        };
        if let Some(n) = self.literal_num(g.clone(), &[*a, *b]) {
            return Ok(n);
        }
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn compare(&mut self, op: Cmp, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            let r = c.compare(op, &x, &y)?;
            return Ok(self.graph.leaf(Op::ConstBool(r)));
        }
        // With an unknown number nothing is decided.
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(self.unknown_bool_atom());
        }
        // Two literals: decided by their intervals, or an undecided atom of
        // its own (the same in every lane, and never one atom with another).
        let lit = match op {
            Cmp::Lt => Op::Lt,
            Cmp::Le => Op::Le,
            Cmp::Gt => Op::Gt,
            Cmp::Ge => Op::Ge,
            Cmp::Eq => Op::Eq,
        };
        match self.eval_literal(&lit, &[*a, *b]) {
            Some(crate::transpile::graph::Val::Bool(Some(r))) => return Ok(self.graph.leaf(Op::ConstBool(r))),
            Some(crate::transpile::graph::Val::Bool(None)) => return Ok(self.unknown_bool_atom()),
            _ => {}
        }
        // Decided by the static ranges (the bucket dispatch): a branch
        // the specialized body never takes is never traced. Decided iff
        // every pair of pieces decides it the same way.
        if !self.ranges.is_empty() {
            if let (Some(xs), Some(ys)) = (self.range_of(*a), self.range_of(*b)) {
                let one = |x: (i64, i64), y: (i64, i64)| -> Option<bool> {
                    match op {
                        Cmp::Lt => if x.1 < y.0 { Some(true) } else if x.0 >= y.1 { Some(false) } else { None },
                        Cmp::Le => if x.1 <= y.0 { Some(true) } else if x.0 > y.1 { Some(false) } else { None },
                        Cmp::Gt => if x.0 > y.1 { Some(true) } else if x.1 <= y.0 { Some(false) } else { None },
                        Cmp::Ge => if x.0 >= y.1 { Some(true) } else if x.1 < y.0 { Some(false) } else { None },
                        Cmp::Eq => if x.0 == x.1 && y.0 == y.1 && x.0 == y.0 { Some(true) } else if x.1 < y.0 || y.1 < x.0 { Some(false) } else { None },
                    }
                };
                let mut decided: Option<Option<bool>> = None;
                'pairs: for x in &xs {
                    for y in &ys {
                        let r = one(*x, *y);
                        match (decided, r) {
                            (None, r) => decided = Some(r),
                            (Some(p), r) if p == r => {}
                            _ => {
                                decided = Some(None);
                                break 'pairs;
                            }
                        }
                    }
                }
                if let Some(Some(r)) = decided {
                    self.range_folds += 1;
                    return Ok(self.graph.leaf(Op::ConstBool(r)));
                }
            }
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
    fn and(&mut self, a: &NodeId, b: &NodeId) -> NodeId {
        self.graph.fold(Op::And, vec![*a, *b])
    }
    fn or(&mut self, a: &NodeId, b: &NodeId) -> NodeId {
        self.graph.fold(Op::Or, vec![*a, *b])
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
    fn mget(&mut self, x: &NodeId, y: &NodeId) -> Result<NodeId> {
        Ok(self.graph.fold(Op::Mget, vec![*x, *y]))
    }
    fn tile_flag_at(
        &mut self,
        x: &NodeId,
        y: &NodeId,
        w: &NodeId,
        h: &NodeId,
        flag: &NodeId,
    ) -> Result<NodeId> {
        Ok(self.graph.fold(Op::TileFlagAt, vec![*x, *y, *w, *h, *flag]))
    }
    fn range_num(&mut self, lo: P8, hi: P8) -> Result<NodeId> {
        Ok(self.graph.leaf(Op::Const(lo.as_raw_u32() as i32, hi.as_raw_u32() as i32)))
    }
    fn unknown_bool(&mut self) -> Result<NodeId> {
        if self.frees >= 6 {
            bail!("more than six free choices - the kernels only model six buttons");
        }
        let b = self.frees;
        self.frees += 1;
        Ok(self.graph.leaf(Op::Free(b)))
    }
    fn node_count(&self) -> usize {
        self.graph.len()
    }
    /// Is this value an interval?
    ///
    /// NOT a cone query. "An interval input is reachable" is the wrong
    /// question and it was the first thing I wrote: `dash_effect_time`
    /// is a `Sel` whose CONDITION compares a position derived from
    /// `rem`, so an interval reaches it - but its arms are numbers and so
    /// is the result. Typing it as an interval made the kernel write an
    /// `AV::Ival` into a column the boundary then refused, at
    /// `player dash_effect_time is not a number`.
    ///
    /// So this is a proper type rule, and deliberately the same one
    /// `transpile::lower`'s `Repr::wide` applies - a select is an
    /// interval when an ARM is, `flr` never is (it is exact by guard),
    /// and a cart lookup never is. The two are checked against each
    /// other by construction: disagree, and lowering fails with "output
    /// cell wants a ZN but the graph computes a ZI".
    fn is_interval(&self, v: &NodeId) -> bool {
        fn go(g: &Graph, ival: &std::collections::BTreeSet<u32>, memo: &mut Vec<Option<bool>>, n: NodeId) -> bool {
            if let Some(b) = memo[n as usize] {
                return b;
            }
            let node = g.get(n);
            let a = &node.args;
            let any = |memo: &mut Vec<Option<bool>>, xs: &[NodeId]| {
                xs.iter().any(|x| go(g, ival, memo, *x))
            };
            let r = match node.op {
                Op::Cell(c) => ival.contains(&c),
                Op::Const(lo, hi) => lo != hi,
                Op::Split(_) | Op::SplitTab(_) | Op::SplitKeyTab(_) => true,
                // An exact whole number per configuration.
                Op::SplitInt(_) | Op::Lo | Op::Hi => false,
                // Unconditionally, like a non-degenerate `Const`: a
                // span exists precisely because its two bounds are
                // different nodes. (`fold` collapses a span of two
                // literals to a `Const`, so the degenerate case is
                // already gone by the time anything asks.) The literal
                // interval and the computed one are the same kind of
                // value, and this is the sibling of `Op::Const(lo, hi)
                // if lo != hi` above.
                Op::Span => true,
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs
                | Op::Min | Op::Max => any(memo, a),
                // The CONDITION does not make the result an interval.
                Op::Sel => any(memo, &a[1..]),
                // Exact by guard - the lane survives only where the floor
                // is unique, which is a conjunct of `ok`.
                Op::Flr => false,
                // The walk replaces `sin` of an inexact input with its
                // RANGE as a constant, so this follows its operand.
                Op::Sin => any(memo, a),
                _ => false,
            };
            memo[n as usize] = Some(r);
            r
        }
        // NO early-out on an empty `ival_cells`. A value can be an
        // interval without any interval INPUT reaching it: the widening
        // writes `Op::Const(lo, hi)` with `lo != hi`, a literal
        // interval. Returning false for those typed a widened `rem` as
        // a `ZN` and the lowering refused it - "output cell 278 wants a
        // ZN but the graph computes a (P8, P8)".
        let mut memo = vec![None; self.graph.len()];
        go(&self.graph, &self.ival_cells, &mut memo, *v)
    }

    fn fork_flr(&mut self, v: &NodeId, ways: u8) -> (NodeId, NodeId) {
        // Already forked this exact value this frame? Reuse the choice.
        // See `fork_memo`. Hash-consing is what makes this sound and
        // also what makes it FIRE: two `move` calls on values that are
        // structurally the same expression are the same node id.
        if let Some(hit) = self.fork_memo.get(v) {
            let (d, out) = *hit;
            self.graph.set_fork_ways(d, ways);
            return out;
        }
        let d = self.forks;
        self.forks += 1;
        self.graph.set_fork_ways(d, ways);
        let out = (
            self.graph.fold(Op::Split(d), vec![*v]),
            self.graph.fold(Op::SplitValid(d), vec![*v]),
        );
        self.fork_memo.insert(*v, (d, out));
        out
    }

    fn fork_int(&mut self, v: &NodeId, ways: u8) -> (NodeId, NodeId) {
        if let Some(hit) = self.fork_memo_int.get(v) {
            let (d, out) = *hit;
            self.graph.set_fork_ways(d, ways);
            return out;
        }
        let d = self.forks;
        self.forks += 1;
        self.graph.set_fork_ways(d, ways);
        let out = (
            self.graph.fold(Op::SplitInt(d), vec![*v]),
            self.graph.fold(Op::SplitValid(d), vec![*v]),
        );
        self.fork_memo_int.insert(*v, (d, out));
        out
    }

    fn span_ok(&mut self, v: &NodeId, ways: u8) -> NodeId {
        self.graph.fold(Op::SplitOk(ways), vec![*v])
    }

    fn flr_ways(&mut self, v: &NodeId) -> u8 {
        // Only a trace with seeded ranges knows a static range: an
        // unspecialized trace keeps the fixed arity (and its gates).
        if self.ranges.is_empty() {
            return self.move_ways();
        }
        match self.range_of(*v) {
            Some(ps) => {
                let sh = 16 - self.graph.fork_bits() as u32;
                ps.iter().map(|p| (p.1 >> sh) - (p.0 >> sh) + 1).max().unwrap_or(1).clamp(1, crate::transpile::graph::MAX_WAYS as i64) as u8
            }
            None => self.move_ways(),
        }
    }

    fn fork_table(&mut self, v: &NodeId, ranges: &[(i32, i32)], arity: u8) -> (NodeId, NodeId) {
        let d = self.forks;
        self.forks += 1;
        self.graph.set_fork_table(d, ranges.to_vec(), arity);
        (
            self.graph.fold(Op::SplitTab(d), vec![*v]),
            self.graph.fold(Op::SplitValidTab(d), vec![*v]),
        )
    }

    fn known(&mut self, b: &NodeId) -> NodeId {
        self.graph.fold(Op::Known, vec![*b])
    }

    fn is_select_num(&self, v: &NodeId) -> bool {
        matches!(self.graph.get(*v).op, Op::Sel)
    }

    fn is_select_bool(&self, b: &NodeId) -> bool {
        matches!(self.graph.get(*b).op, Op::Sel)
    }

    fn move_ways(&self) -> u8 {
        self.move_ways.max(2)
    }

    fn join_num_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        if !self.fruit_unknown || self.decide(c).is_some() || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        if self.is_unknown_num(*t) || self.is_unknown_num(*f) {
            return Some(self.unknown_num());
        }
        match (self.graph.get(*t).op.clone(), self.graph.get(*f).op.clone()) {
            (Op::Const(a0, a1), Op::Const(b0, b1)) => Some(self.graph.leaf(Op::Const(a0.min(b0), a1.max(b1)))),
            _ => None,
        }
    }

    fn reads_unknown_atom(&mut self, c: &NodeId) -> bool {
        if !self.fruit_unknown || self.decide(c).is_some() {
            return false;
        }
        // Post-order, every node walked memoized (`lane_independent`'s walk):
        // the conditions a frame merges on share most of their cones.
        let mut stack: Vec<(NodeId, bool)> = vec![(*c, false)];
        while let Some((x, expanded)) = stack.pop() {
            if self.atom_memo.contains_key(&x) {
                continue;
            }
            let node = self.graph.get(x);
            if matches!(node.op, Op::UnknownBool(_)) || node.args.is_empty() {
                let atom = matches!(node.op, Op::UnknownBool(_));
                self.atom_memo.insert(x, atom);
                continue;
            }
            let args = node.args.clone();
            if expanded {
                let r = args.iter().any(|a| self.atom_memo[a]);
                self.atom_memo.insert(x, r);
            } else {
                stack.push((x, true));
                stack.extend(args.iter().filter(|a| !self.atom_memo.contains_key(a)).map(|a| (*a, false)));
            }
        }
        self.atom_memo[c]
    }

    fn join_bool_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        if !self.fruit_unknown || self.decide(c).is_some() || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        Some(self.unknown_bool_atom())
    }

    fn literal_fragments(&mut self, v: &NodeId) -> Option<Vec<NodeId>> {
        if !self.fruit_unknown {
            return None;
        }
        let Op::Const(lo, hi) = self.graph.get(*v).op else { return None };
        if lo == hi {
            return None;
        }
        let step = 1i64 << (16 - self.graph.fork_bits() as i64);
        let (lo, hi) = (lo as i64, hi as i64);
        let (fl, fh) = (lo.div_euclid(step) * step, hi.div_euclid(step) * step);
        if (fh - fl) / step + 1 > crate::transpile::graph::MAX_WAYS as i64 {
            return None;
        }
        let mut out = Vec::new();
        let mut base = fl;
        while base <= fh {
            let (a, b) = (lo.max(base), hi.min(base + step - 1));
            out.push(self.graph.leaf(Op::Const(a as i32, b as i32)));
            base += step;
        }
        Some(out)
    }

    fn undecided_atom(&mut self) -> Result<NodeId> {
        Ok(self.unknown_bool_atom())
    }

    fn as_const(&self, v: &NodeId) -> Option<P8> {
        self.as_p8(*v)
    }

    fn describe(&self, v: &NodeId) -> String {
        // One level of operands as well as the op. A bare `Sel/3` says
        // "a select" and leaves open the thing that decides whether a
        // refusal is easy to lift - whether its ARMS are constants.
        let n = self.graph.get(*v);
        let args: Vec<String> = n
            .args
            .iter()
            .map(|a| format!("{:?}", self.graph.get(*a).op))
            .collect();
        format!("node {} = {:?}({})", v, n.op, args.join(", "))
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
