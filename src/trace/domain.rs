//! The value domain the interpreter is generic over: `Symbolic` (the
//! TRACER: `NodeId`s into a `transpile::graph::Graph`) here, `RefDomain`
//! (the reference engine) in `refdomain`. `decide` answers a condition NOW;
//! on `None` the interpreter traces both arms and merges with `sel_*`.

use std::fmt::Debug;

use anyhow::{bail, Result};

use celeste_core::arith::{Arithmetic, Fixed};
use crate::transpile::garith::GraphArith;
use crate::transpile::graph::{Cut, Eval, Graph, NodeId, Op, Ty, Unknown};

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

impl<A: GraphArith> From<Arith> for Op<A> {
    fn from(a: Arith) -> Op<A> {
        match a {
            Arith::Add => Op::Add,
            Arith::Sub => Op::Sub,
            Arith::Mul => Op::Mul,
            Arith::Div => Op::Div,
            Arith::Rem => Op::Rem,
        }
    }
}

impl<A: GraphArith> From<Cmp> for Op<A> {
    fn from(c: Cmp) -> Op<A> {
        match c {
            Cmp::Lt => Op::Lt,
            Cmp::Le => Op::Le,
            Cmp::Gt => Op::Gt,
            Cmp::Ge => Op::Ge,
            Cmp::Eq => Op::Eq,
        }
    }
}

impl<A: GraphArith> From<Fun1> for Op<A> {
    fn from(f: Fun1) -> Op<A> {
        match f {
            Fun1::Neg => Op::Neg,
            Fun1::Abs => Op::Abs,
            Fun1::Flr => Op::Flr,
            Fun1::Sin => Op::Sin,
        }
    }
}

impl<A: GraphArith> From<Fun2> for Op<A> {
    fn from(f: Fun2) -> Op<A> {
        match f {
            Fun2::Min => Op::Min,
            Fun2::Max => Op::Max,
        }
    }
}

impl Cmp {
    pub fn of<A: GraphArith>(op: &Op<A>) -> Option<Cmp> {
        Some(match op {
            Op::Lt => Cmp::Lt,
            Op::Le => Cmp::Le,
            Op::Gt => Cmp::Gt,
            Op::Ge => Cmp::Ge,
            Op::Eq => Cmp::Eq,
            _ => return None,
        })
    }
}

/// The origin of the forks `Domain::unknown_bool` mints: the six buttons, in
/// `__reset_button_states`' order.
pub const UNKNOWN_BOOL_ORIGIN: &str = "__new_unknown_boolean";

/// HOW a fork partitions its value's set into configurations: cut as `cut`
/// says (`Op::Fork` -> `Op::Frag`), into `ways` fragments. `memo` false keeps
/// two forks of one node independent (`p_jump`/`p_dash` both fork `[0,
/// 1<<16]` into its whole numbers).
pub struct Partition {
    pub cut: Cut,
    pub ways: u8,
    pub memo: bool,
}

/// What a fork hands back: this configuration's fragment, which lanes fall in
/// it, and the choice dimension minted.
pub struct Fork {
    pub value: NodeId,
    pub valid: NodeId,
    pub id: u8,
}

pub trait Domain: Clone + Default {
    /// The game's arithmetic (`celeste_core::arith`).
    type A: Arithmetic;
    type Num: Clone + Debug + PartialEq;
    type Bool: Clone + Debug + PartialEq;

    fn num(&mut self, v: Self::A) -> Self::Num;
    fn int(&mut self, k: i16) -> Self::Num {
        self.num(Self::A::from_i16(k))
    }
    fn boolean(&mut self, b: bool) -> Self::Bool;

    fn arith(&mut self, op: Arith, a: &Self::Num, b: &Self::Num) -> Result<Self::Num>;
    fn fun1(&mut self, f: Fun1, a: &Self::Num) -> Result<Self::Num>;
    fn fun2(&mut self, f: Fun2, a: &Self::Num, b: &Self::Num) -> Result<Self::Num>;
    fn compare(&mut self, op: Cmp, a: &Self::Num, b: &Self::Num) -> Result<Self::Bool>;
    fn not(&mut self, a: &Self::Bool) -> Self::Bool;
    fn and(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool;
    /// De Morgan by default; `Symbolic` builds `Op::Or` (one node, interns
    /// symmetrically).
    fn or(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool {
        let (na, nb) = (self.not(a), self.not(b));
        let both = self.and(&na, &nb);
        self.not(&both)
    }

    /// The value of this condition, if known; `None` runs both arms.
    fn decide(&self, c: &Self::Bool) -> Option<bool>;

    /// Merge two values under an undecided condition (never reached by
    /// `RefDomain`, which decides every condition).
    fn sel_num(&mut self, c: &Self::Bool, t: &Self::Num, f: &Self::Num) -> Self::Num;
    fn sel_bool(&mut self, c: &Self::Bool, t: &Self::Bool, f: &Self::Bool) -> Self::Bool;

    /// `mget(x, y)` with coordinates unknown at trace time (a call-out).
    fn mget(&mut self, x: &Self::Num, y: &Self::Num) -> Result<Self::Num>;

    /// `tile_flag_at(x, y, w, h, flag)` with coordinates unknown at trace
    /// time: a primitive (a call-out), since the Lua's scan bounds
    /// derive from x and y. The caller folds the known case.
    fn tile_flag_at(
        &mut self,
        x: &Self::Num,
        y: &Self::Num,
        w: &Self::Num,
        h: &Self::Num,
        flag: &Self::Num,
    ) -> Result<Self::Bool>;

    /// A fresh UNKNOWN boolean, a free choice of the search.
    fn unknown_bool(&mut self) -> Result<Self::Bool>;

    /// A number known only to lie in `[lo, hi]` (`rnd`).
    fn range_num(&mut self, lo: Self::A, hi: Self::A) -> Result<Self::Num>;

    fn node_count(&self) -> usize {
        0
    }

    /// A number this value definitely is. Needed where the HEAP depends on it
    /// (index, key, loop bound); unknown is a refusal.
    fn as_const(&self, v: &Self::Num) -> Option<Self::A>;

    /// What this number IS, for a message: its value where known.
    fn describe(&self, v: &Self::Num) -> String;

    /// Is this value an INTERVAL (a set of numbers)?
    fn is_interval(&self, _v: &Self::Num) -> bool {
        false
    }

    /// Is a merged value a SELECT the kernel reads by its condition (`Op::Sel`)?
    /// Only a select reads the merge's condition, so only it can make the
    /// merge refuse (`state::merge`).
    fn is_select(&self, _v: &super::heap::Value<Self>) -> bool {
        false
    }

    /// Fork an interval at `cut`'s grid: `__split_by_flr` (`Cut::Floors`), or
    /// `__split_by_round` (`Cut::Rounds`) where its fragments are NOT the floor
    /// fork's of `x + 1/2` shifted back (doubles: `x + 1/2 - 1/2` rounds;
    /// 16.16 shifts instead, in `Interp`). `flr` of an interval is not a
    /// function, so the fragment is a choice of arity `ways`. Returns the
    /// restricted value and which lanes fall in it.
    fn split(&mut self, cut: Cut, v: &Self::Num, ways: u8) -> Result<(Self::Num, Self::Bool)>;

    /// The arity of `split` on `v`: `MOVE_WAYS`, or with a known static range
    /// the most grid cells one piece crosses (`ForkOk` checks).
    fn split_ways(&mut self, _cut: Cut, _v: &Self::Num) -> u8 {
        MOVE_WAYS
    }

    /// `n`, an operator with an own error (`trace::error`), was computed
    /// where `at` holds; its error holds only there.
    fn evaluated_at(&mut self, _n: &Self::Num, _at: &Self::Bool) {}

    /// The join of a merge on a condition NO LANE can decide, with arms every
    /// lane holds alike: the hull of two literals, or the unknown number.
    /// `None` keeps the ordinary select.
    fn join_num_independent(&mut self, _c: &Self::Bool, _t: &Self::Num, _f: &Self::Num) -> Option<Self::Num> {
        None
    }

    /// `join_num_independent` for booleans: different arms join to an atom.
    fn join_bool_independent(&mut self, _c: &Self::Bool, _t: &Self::Bool, _f: &Self::Bool) -> Option<Self::Bool> {
        None
    }

    /// Does undecided `c` read an unknown atom (a countdown's does not count:
    /// `Symbolic::countdown_atom`)? No lane decides one, so a merge that
    /// would select on `c` is two successors instead.
    fn reads_unknown_atom(&mut self, _c: &Self::Bool) -> bool {
        false
    }

    /// Does undecided `c` read an input this trace REFUSES MERGES on
    /// (`frame::trace_frame`'s `merge_refused_on`)? A merge that would
    /// select on `c` is two successors instead, as on an unknown atom.
    fn reads_merge_refused(&mut self, _c: &Self::Bool) -> bool {
        false
    }

    /// `__split_by_flr` of a literal interval: its integer fragments, run as
    /// separate trace states and rejoined when the call returns; with
    /// `round`, `__split_by_round`'s (where `flr(v + 1/2)` is one integer:
    /// `Arithmetic::round_fragment`). `None`: an ordinary per-lane fork.
    fn literal_fragments(&mut self, _v: &Self::Num, _round: bool) -> Result<Option<Vec<Self::Num>>> {
        Ok(None)
    }

    /// A fresh undecided boolean every lane holds alike (rejoined fragments
    /// merge on it).
    fn undecided_atom(&mut self) -> Result<Self::Bool> {
        bail!("this domain has no undecided booleans")
    }

    /// How many undecided atoms this frame handed out (marked per call).
    fn atoms_minted(&self) -> u32 {
        0
    }

    /// A boolean reading an atom minted at or after `since` that ESCAPES its
    /// call becomes a fork, restricted per lane to its possible values, so
    /// later reads agree per configuration. `None` if it reads no such atom.
    fn escaped_atom(&mut self, _b: &Self::Bool, _since: u32, _origin: &dyn Fn(&Self) -> String) -> Option<Self::Bool> {
        None
    }
}

/// How many atoms an escaping value may read before its fork is left
/// unrestricted (the restriction enumerates their assignments).
const ESCAPE_ATOMS: usize = 6;

/// THE overflow error of the tracer's constant folding (`Symbolic::arith`):
/// the game never overflows, so one is a modelling surprise, never a
/// wrapped value.
fn overflow<A: Arithmetic>(what: String) -> anyhow::Error {
    anyhow::anyhow!("arithmetic overflow: {what} leaves {}", A::RANGE_NOTE)
}

/// `op` of two constants: the exact result, or `overflow`.
fn fold_arith<A: Arithmetic>(op: Arith, a: A, b: A) -> Result<A> {
    let (r, sym) = match op {
        Arith::Add => (A::add(a, b), "+"),
        Arith::Sub => (A::sub(a, b), "-"),
        Arith::Mul => (A::mul(a, b), "*"),
        Arith::Div => (A::div(a, b), "/"),
        Arith::Rem => (A::rem(a, b), "%"),
    };
    r.ok_or_else(|| overflow::<A>(format!("{} {sym} {}", A::show(a), A::show(b))))
}

/// `f` of a constant, as `fold_arith`.
fn fold_fun1<A: Arithmetic>(f: Fun1, a: A) -> Result<A> {
    Ok(match f {
        Fun1::Neg => A::neg(a).ok_or_else(|| overflow::<A>(format!("-{}", A::show(a))))?,
        Fun1::Abs => A::abs(a).ok_or_else(|| overflow::<A>(format!("abs({})", A::show(a))))?,
        Fun1::Flr => A::flr(a),
        Fun1::Sin => A::sin(a).ok_or_else(|| overflow::<A>(format!("sin({})", A::show(a))))?,
    })
}

fn fold_fun2<A: Arithmetic>(f: Fun2, a: A, b: A) -> A {
    match f {
        Fun2::Min => A::min(a, b),
        Fun2::Max => A::max(a, b),
    }
}

fn fold_compare<A: Arithmetic>(op: Cmp, a: A, b: A) -> bool {
    match op {
        Cmp::Lt => A::lt(a, b),
        Cmp::Le => A::le(a, b),
        Cmp::Gt => A::lt(b, a),
        Cmp::Ge => A::le(b, a),
        Cmp::Eq => A::num_eq(a, b),
    }
}

/// The tracing domain. Owns the graph it is building.
#[derive(Default, Clone)]
pub struct Symbolic<A: GraphArith = Fixed> {
    pub graph: Graph<A>,
    /// `--build-trace`: the tracer's progress lines.
    pub build_trace: bool,
    /// `split_ways` takes a static range's full width (level -1, whose speed
    /// is a range at every node).
    pub uncapped_ways: bool,
    /// The level traced (`widening::TABLE`'s entries it applies, in and out).
    pub level: celeste_engine::widening::Level,
    /// Keep undecided selects as selects (level -1); off, they become forks.
    pub no_known_forks: bool,
    /// What THIS trace accumulates (`frame::trace` starts each from a fresh
    /// one).
    pub frame: FrameScratch<A>,
    /// `lane_independent`'s memo (whether a lane's data reaches a node);
    /// structural, so it outlives a frame.
    lane_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// `reads_unknown_atom`'s memo: structural, like `lane_memo`.
    atom_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// These three depend on the input cells' types (the graph's
    /// `cell_kind`; dropped by `forget_intervals`).
    undecidable_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    beneath_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `may_answers`'s memo: `(may_true, may_false)`.
    may_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, (NodeId, NodeId)>>,
}

/// What ONE trace accumulates. `frame::trace` replaces it wholesale at its
/// start, so a trace that failed part-way leaves nothing behind.
#[derive(Default, Clone)]
pub struct FrameScratch<A: GraphArith = Fixed> {
    /// FORK choices handed out this frame (the fork ids), buttons included.
    pub forks: u8,
    /// What each `both_values` fork was made for (`--build-trace`).
    pub fork_origins: Vec<(u8, String)>,
    /// Fork choices by `(value, kind)`: a second fork of one value reuses the
    /// first's choice instead of an empty dimension.
    fork_memo: std::collections::HashMap<(NodeId, Cut), (u8, (NodeId, NodeId))>,
    /// How many `Op::UnknownBool` atoms this frame handed out.
    pub unknown_atoms: u32,
    /// `escaped_atom`'s memo (the atoms' ids restart with the frame).
    pub escaped: rustc_hash::FxHashMap<NodeId, NodeId>,
    /// WHERE each node with an own error was evaluated (the OR of its path
    /// guards).
    pub evaluated: rustc_hash::FxHashMap<NodeId, NodeId>,
    /// The frame's INPUT ASSUMPTIONS, in the order made: `Op::Restrict` (a
    /// bounded input, the only source of a static range) and `Op::Pin` (an input
    /// the body reads as a constant), each charged to every outcome.
    pub assumptions: Vec<NodeId>,
    range_memo: std::collections::HashMap<NodeId, Option<Pieces<A>>>,
    /// The input cells this trace refuses merges on, sorted
    /// (`frame::trace_frame`'s `merge_refused_on`, `set_merge_refused`).
    merge_refused: Vec<u32>,
    merge_refused_memo: rustc_hash::FxHashMap<NodeId, bool>,
}

use crate::transpile::garith::Pieces;

impl<A: GraphArith> Symbolic<A> {
    pub fn set_merge_refused(&mut self, mut cells: Vec<u32>) {
        cells.sort_unstable();
        cells.dedup();
        self.frame.merge_refused = cells;
        self.frame.merge_refused_memo.clear();
    }


    /// The input cells' types changed: the memos reading them no longer hold.
    pub fn forget_intervals(&mut self) {
        self.undecidable_memo.get_mut().clear();
        self.beneath_memo.get_mut().clear();
        self.may_memo.get_mut().clear();
    }

    /// THE FORK TRIGGER: can ONE LANE hold this condition both ways? (Not
    /// whether it denotes a set: a set may still be one value per lane.)
    /// `TileFlagAt`/`Mget` are lane-decidable by instruction; `Flr` by assertion
    /// (`zi_flr_ok`, its own error, so `Known(Flr(..))` must not fold away).
    pub fn lane_undecidable(&self, b: NodeId) -> bool {
        fn go<A: GraphArith>(d: &Symbolic<A>, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(x) = memo.get(&n) {
                return *x;
            }
            let node = d.graph.get(n);
            let args = node.args.clone();
            let any = |memo: &mut rustc_hash::FxHashMap<NodeId, bool>, xs: &[NodeId]| xs.iter().any(|x| go(d, memo, *x));
            let r = match node.op {
                // THE source: an operand spans values, judged BENEATH the
                // lane-decidable operators, or the exclusions never bite.
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => args.iter().any(|a| d.abstract_beneath_lane_ops(*a)),
                Op::UnknownBool(_) => true,
                Op::Not | Op::And | Op::Or | Op::Sel => any(memo, &args),
                // Lane-decidable by instruction.
                Op::TileFlagAt | Op::Mget => false,
                // Lane-decidable by assertion (`own_error`, not a type fact).
                Op::Flr => false,
                Op::Known => false,
                // Validity/coverage masks: per-lane comparisons.
                Op::ForkValid(..) | Op::ForkOk(..) | Op::FragOk(..) | Op::NoWrap => false,
                // A pin's check on the raw input.
                Op::Same => false,
                Op::ConstBool(_) => false,
                _ => false,
            };
            memo.insert(n, r);
            r
        }
        go(self, &mut self.undecidable_memo.borrow_mut(), b)
    }

    /// Does `n` denote a SET of values, judged STOPPING at operators a lane
    /// decides itself (`Flr`, `TileFlagAt`, `Mget`)? How comparison operands are
    /// judged, without spurious forks.
    pub fn abstract_beneath_lane_ops(&self, n: NodeId) -> bool {
        fn go<A: GraphArith>(d: &Symbolic<A>, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(x) = memo.get(&n) {
                return *x;
            }
            let node = d.graph.get(n);
            let args = node.args.clone();
            let r = match node.op {
                // A lane decides these whatever their operands.
                Op::Flr | Op::TileFlagAt | Op::Mget => false,
                Op::Cell(_) => d.graph.ty(n) == Ty::Ival,
                Op::Const(lo, hi) => lo != hi,
                Op::Span => true,
                Op::UnknownNum { .. } | Op::UnknownBool(_) => true,
                // An `Ints` fragment is one exact number.
                Op::Fork(Cut::Ints, _) | Op::Frag(Cut::Ints, _) | Op::Lo | Op::Hi => false,
                Op::Fork(..) | Op::Frag(..) => true,
                Op::ConstBool(_) | Op::Known | Op::NoWrap => false,
                // THE CONDITION DOES NOT MAKE THE RESULT A SET: `Sel(c, 5, 7)`
                // is one of two numbers, chosen by forking `c` (the opposite of
                // `lane_undecidable` for booleans, deliberately).
                Op::Sel => args[1..].iter().any(|a| go(d, memo, *a)),
                _ => args.iter().any(|a| go(d, memo, *a)),
            };
            memo.insert(n, r);
            r
        }
        go(self, &mut self.beneath_memo.borrow_mut(), n)
    }

    /// The static range of `n`, from the restrictions.
    pub fn range_of(&mut self, n: NodeId) -> Option<Pieces<A>> {
        A::pieces_of(&self.graph, &mut self.frame.range_memo, n)
    }

    /// Do the static ranges put every value of `n`'s EXACT result inside the
    /// arithmetic's range (computed unwrapped: a result past it is unknown)?
    /// Where true, `n` cannot overflow.
    pub fn bounded(&mut self, n: NodeId) -> bool {
        self.range_of(n).is_some_and(|pieces| A::bounded(&pieces))
    }

    /// Does anything give this frame a static range (a restriction)?
    fn has_ranges(&self) -> bool {
        self.frame.assumptions.iter().any(|a| matches!(self.graph.get(*a).op, Op::Restrict(..)))
    }

    /// THE RANGE ASSUMPTION: `x` restricted to `[lo, hi]` (`Op::Restrict`),
    /// its own error charged to every outcome. Read it in `x`'s place.
    pub fn restrict(&mut self, x: NodeId, lo: A::Lit, hi: A::Lit) -> NodeId {
        let r = self.graph.add(Op::Restrict(lo, hi), vec![x]);
        if !self.frame.assumptions.contains(&r) {
            self.frame.assumptions.push(r);
        }
        r
    }

    /// THE PIN: the input `x` lies in the constant `c` (`Op::Pin`), its check
    /// charged to the frame. No data output: the caller makes every use of the
    /// input read the constant.
    pub fn pin(&mut self, x: NodeId, c: crate::transpile::graph::Pinned<A>) {
        let p = self.graph.add(Op::Pin(c), vec![x]);
        if !self.frame.assumptions.contains(&p) {
            self.frame.assumptions.push(p);
        }
    }

    /// THE unknown number (`countdown`: a countdown's, `Op::UnknownNum`).
    pub fn unknown_num(&mut self, countdown: bool) -> NodeId {
        self.graph.leaf(Op::UnknownNum { countdown })
    }

    fn is_unknown_num(&self, n: NodeId) -> bool {
        matches!(self.graph.get(n).op, Op::UnknownNum { .. })
    }

    /// Is any of `ns` a countdown's unknown number?
    fn countdown_unknown(&self, ns: &[NodeId]) -> bool {
        ns.iter().any(|n| matches!(self.graph.get(*n).op, Op::UnknownNum { countdown: true }))
    }

    /// `n` as `Sel(c, t, f)` with an arm the unknown number (a countdown reset
    /// on some lanes), so operations distribute over the arms and the unknown is
    /// never an arithmetic operand. EXACT: `op(Sel(c, t, f)) = Sel(c, op(t),
    /// op(f))`, same error.
    fn unknown_select(&self, n: NodeId) -> Option<(NodeId, NodeId, NodeId)> {
        fn holds<A: GraphArith>(g: &Graph<A>, n: NodeId) -> bool {
            let node = g.get(n);
            match node.op {
                Op::UnknownNum { .. } => true,
                Op::Sel => holds(g, node.args[1]) || holds(g, node.args[2]),
                _ => false,
            }
        }
        let node = self.graph.get(n);
        (node.op == Op::Sel && holds(&self.graph, n)).then(|| (node.args[0], node.args[1], node.args[2]))
    }

    /// Is anything unknown in the traced set (the fly fruit)? Enables
    /// literal folding, independent joins and literal splits.
    pub fn unknowns(&self) -> bool {
        self.level.fruit
    }

    /// `n` as `base + literal` (either side), else `n + 0`.
    fn base_plus_literal(&self, n: NodeId) -> (NodeId, (A::Lit, A::Lit)) {
        let node = self.graph.get(n);
        if let Op::Add = node.op {
            for (b, k) in [(node.args[0], node.args[1]), (node.args[1], node.args[0])] {
                if let Op::Const(lo, hi) = self.graph.get(k).op {
                    return (b, (lo, hi));
                }
            }
        }
        let zero = A::lit(A::from_i16(0));
        (n, (zero, zero))
    }

    /// For boolean `n`: where a lane CAN answer true, and where false (a
    /// cover). Judged by `lane_undecidable`, THE SAME PREDICATE THAT DECIDES WHAT
    /// TO FORK: this is a fork's validity, so the two must agree.
    pub fn may_answers(&mut self, n: NodeId) -> (NodeId, NodeId) {
        if let Some(hit) = self.may_memo.borrow().get(&n) {
            return *hit;
        }
        let out = self.may_answers_uncached(n);
        self.may_memo.borrow_mut().insert(n, out);
        out
    }

    fn may_answers_uncached(&mut self, n: NodeId) -> (NodeId, NodeId) {
        if !self.lane_undecidable(n) {
            let nn = self.graph.fold(Op::Not, vec![n]);
            return (n, nn);
        }
        let (op, args) = {
            let node = self.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        let yes = self.graph.leaf(Op::ConstBool(true));
        match op {
            Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                let (alo, ahi) = (self.graph.fold(Op::Lo, vec![args[0]]), self.graph.fold(Op::Hi, vec![args[0]]));
                let (blo, bhi) = (self.graph.fold(Op::Lo, vec![args[1]]), self.graph.fold(Op::Hi, vec![args[1]]));
                let ((t, ta, tb), (f, fa, fb)) = match op {
                    Op::Lt => ((Op::Lt, alo, bhi), (Op::Ge, ahi, blo)),
                    Op::Le => ((Op::Le, alo, bhi), (Op::Gt, ahi, blo)),
                    Op::Gt => ((Op::Gt, ahi, blo), (Op::Le, alo, bhi)),
                    _ => ((Op::Ge, ahi, blo), (Op::Lt, alo, bhi)),
                };
                (self.graph.fold(t, vec![ta, tb]), self.graph.fold(f, vec![fa, fb]))
            }
            // `a == b` may hold iff the ranges meet, and fail iff they are not one
            // number. Only where neither operand reads an unknown, so `Lo`/`Hi` are an
            // interval's ends: judged per node, not per frame (else, with the fly
            // fruit unknown, an exact hidden floor's `state == 2` goes both ways).
            Op::Eq if !self.reads_unknown(&args) => {
                let (alo, ahi) = (self.graph.fold(Op::Lo, vec![args[0]]), self.graph.fold(Op::Hi, vec![args[0]]));
                let (blo, bhi) = (self.graph.fold(Op::Lo, vec![args[1]]), self.graph.fold(Op::Hi, vec![args[1]]));
                let (meet_a, meet_b) = (self.graph.fold(Op::Le, vec![alo, bhi]), self.graph.fold(Op::Le, vec![blo, ahi]));
                let (below, above) = (self.graph.fold(Op::Lt, vec![alo, bhi]), self.graph.fold(Op::Gt, vec![ahi, blo]));
                (self.graph.fold(Op::And, vec![meet_a, meet_b]), self.graph.fold(Op::Or, vec![below, above]))
            }
            Op::Not => {
                let (t, f) = self.may_answers(args[0]);
                (f, t)
            }
            Op::And | Op::Or => {
                let parts: Vec<(NodeId, NodeId)> = args.iter().map(|a| self.may_answers(*a)).collect();
                let (join_t, join_f) = if op == Op::And { (Op::And, Op::Or) } else { (Op::Or, Op::And) };
                let t = self.graph.fold(join_t, parts.iter().map(|p| p.0).collect());
                let f = self.graph.fold(join_f, parts.iter().map(|p| p.1).collect());
                (t, f)
            }
            _ => (yes, yes),
        }
    }

    /// Does any of `ns` read an unknown number or boolean atom?
    fn reads_unknown(&self, ns: &[NodeId]) -> bool {
        self.graph.cone(ns).into_iter().any(|n| matches!(self.graph.get(n).op, Op::UnknownNum { .. } | Op::UnknownBool(_)))
    }

    /// `a - r` where `a` is `r + c`: `c` EXACTLY, also under a select's arms
    /// (a platform's `x - last`). Not in `Graph::fold`: over ranges `(r + c) -
    /// r` evaluates wider, and `fold` must agree with the evaluator. Only where
    /// the static ranges bound the sum (an overflowing `r + c` is an error the
    /// cancelled form would no longer read).
    fn cancel_sub(&mut self, a: NodeId, r: NodeId) -> Result<Option<NodeId>> {
        // `x - x` is 0 exactly in either arithmetic (a double's is `+0`).
        if a == r {
            return Ok(Some(self.konst(A::from_i16(0))));
        }
        let op = self.graph.get(a).op.clone();
        let args = self.graph.get(a).args.clone();
        match op {
            // `(r + c) - r` is `c` only where the sum is exact.
            Op::Add if args[0] == r && self.exact_sum(a, args[0], args[1]) => Ok(Some(args[1])),
            Op::Add if args[1] == r && self.exact_sum(a, args[1], args[0]) => Ok(Some(args[0])),
            Op::Sel => {
                let (c, t, f) = (args[0], args[1], args[2]);
                let (tc, fc) = (self.cancel_sub(t, r)?, self.cancel_sub(f, r)?);
                if tc.is_none() && fc.is_none() {
                    return Ok(None);
                }
                let t2 = match tc {
                    Some(n) => n,
                    None => self.arith(Arith::Sub, &t, &r)?,
                };
                let f2 = match fc {
                    Some(n) => n,
                    None => self.arith(Arith::Sub, &f, &r)?,
                };
                Ok(Some(self.graph.fold(Op::Sel, vec![c, t2, f2])))
            }
            _ => Ok(None),
        }
    }

    /// Does `sum - r` lie in `c` on every lane, `sum` being `r + c`? Where the
    /// ranges bound the sum and addition is exact (16.16); or `r` is an integer,
    /// the sum is below 2^53 in magnitude, and `c` is an integer (the sum is
    /// exact) or a literal with integer ends (a move's rejoined fragments: `fl`
    /// is monotone and exact at the ends, so the difference lies in the
    /// literal).
    fn exact_sum(&mut self, sum: NodeId, r: NodeId, c: NodeId) -> bool {
        if !self.bounded(sum) {
            return false;
        }
        if A::EXACT_ADDITION {
            return true;
        }
        const EXACT: i64 = 1 << 53;
        let small = self.range_of(sum).and_then(|p| A::floor_hull(&p)).is_some_and(|(lo, hi)| lo > -EXACT && hi < EXACT);
        let whole = |l: A::Lit| {
            let v = A::from_lit(l);
            A::num_eq(A::flr(v), v)
        };
        let whole_literal = matches!(self.graph.get(c).op, Op::Const(lo, hi) if whole(lo) && whole(hi));
        small && self.integral(r, 0) && (whole_literal || self.integral(c, 0))
    }

    /// Is `n` an integer on every lane? A whole literal, a floor, an integer
    /// fork's fragment, and sums, differences, negations, `abs`, `min`, `max`
    /// and selects of those (a rounded sum of integers is an integer).
    /// Structural; `false` if unsure.
    fn integral(&self, n: NodeId, depth: u32) -> bool {
        if depth > 32 {
            return false;
        }
        let node = self.graph.get(n);
        let all = |args: &[NodeId]| args.iter().all(|&a| self.integral(a, depth + 1));
        match node.op {
            Op::Const(lo, hi) if lo == hi => {
                let v = A::from_lit(lo);
                A::num_eq(A::flr(v), v)
            }
            Op::Flr | Op::Frag(Cut::Ints, _) => true,
            Op::Add | Op::Sub | Op::Neg | Op::Abs | Op::Min | Op::Max => all(&node.args),
            Op::Sel => all(&node.args[1..]),
            _ => false,
        }
    }

    /// `a + r` where `a` is `x - r`: `x` EXACTLY, where addition is exact
    /// (16.16) and the ranges bound `x - r`, so the cancelled sum overflowed
    /// nowhere. `move` reads `flr(rem + 0.5)` of `__split_by_round`'s fragment,
    /// itself `x - 0.5` in 16.16.
    fn cancel_add(&mut self, a: NodeId, r: NodeId) -> Option<NodeId> {
        if !A::EXACT_ADDITION {
            return None;
        }
        let (op, x, k) = {
            let node = self.graph.get(a);
            (node.op.clone(), node.args.first().copied(), node.args.get(1).copied())
        };
        match (op, x, k) {
            (Op::Sub, Some(x), Some(k)) if k == r && self.bounded(a) => Some(x),
            _ => None,
        }
    }

    /// A boolean every lane holds in BOTH values (buttons, held trails,
    /// escaped atoms): an unmemoized `Ints` fork of `[0, 1]` with no validity,
    /// so `ForkOk(2)` folds true and the fragments to `0` and `1`.
    pub fn both_values(&mut self, origin: &str) -> NodeId {
        let (zero, one) = (A::lit(A::from_i16(0)), A::lit(A::from_i16(1)));
        let choices = self.graph.leaf(Op::Const(zero, one));
        let f = self.fork(&choices, Partition { cut: Cut::Ints, ways: 2, memo: false }, origin);
        let zero = self.graph.leaf(Op::Const(zero, zero));
        self.graph.fold(Op::Gt, vec![f.value, zero])
    }


    /// THE fork: partition `v`'s set, mint a choice dimension, and hand back
    /// this configuration's piece and its lanes. Every fork comes through here.
    fn fork(&mut self, v: &NodeId, p: Partition, origin: &str) -> Fork {
        // Already forked this value this frame (hash-consed)? Reuse the choice.
        if p.memo {
            if let Some(hit) = self.frame.fork_memo.get(&(*v, p.cut)) {
                let (d, out) = *hit;
                // RAISED, not set: each site's `ForkOk` bounds its own lanes.
                self.graph.set_fork_ways(d, p.ways);
                return Fork { value: out.0, valid: out.1, id: d };
            }
        }
        let d = self.frame.forks;
        self.frame.forks += 1;
        self.frame.fork_origins.push((d, origin.to_string()));
        self.graph.set_fork_ways(d, p.ways);
        let value = self.graph.fold(Op::Fork(p.cut, d), vec![*v]);
        let valid = self.graph.fold(Op::ForkValid(p.cut.grid(), d), vec![*v]);
        if p.memo {
            self.frame.fork_memo.insert((*v, p.cut), (d, (value, valid)));
        }
        Fork { value, valid, id: d }
    }

    /// THE unknown boolean an output widening writes: one hash-consed node,
    /// stored as `AV::UBool`. Nothing in the frame reads an output, so sharing
    /// correlates nothing; inputs and comparisons take fresh atoms.
    pub fn unknown_bool_output(&mut self) -> NodeId {
        self.graph.leaf(Op::UnknownBool(Unknown::Output))
    }

    /// Is `b` the shared output unknown (`unknown_bool_output`)?
    pub fn is_unknown_output(&self, b: NodeId) -> bool {
        matches!(self.graph.get(b).op, Op::UnknownBool(Unknown::Output))
    }

    /// `root` with `subst`'s nodes replaced and everything above re-folded.
    pub fn substitute(&mut self, root: NodeId, subst: &rustc_hash::FxHashMap<NodeId, NodeId>) -> NodeId {
        let mut done: rustc_hash::FxHashMap<NodeId, NodeId> = subst.clone();
        let mut stack: Vec<(NodeId, bool)> = vec![(root, false)];
        while let Some((n, expanded)) = stack.pop() {
            if done.contains_key(&n) {
                continue;
            }
            let (op, args) = {
                let node = self.graph.get(n);
                (node.op.clone(), node.args.clone())
            };
            if !expanded {
                stack.push((n, true));
                stack.extend(args.iter().filter(|a| !done.contains_key(a)).map(|a| (*a, false)));
                continue;
            }
            let new_args: Vec<NodeId> = args.iter().map(|a| done[a]).collect();
            let out = if new_args == args { n } else { self.graph.fold(op, new_args) };
            done.insert(n, out);
        }
        done[&root]
    }

    pub fn unknown_bool_atom(&mut self) -> NodeId {
        let k = self.frame.unknown_atoms;
        self.frame.unknown_atoms += 1;
        self.graph.leaf(Op::UnknownBool(Unknown::Atom(k)))
    }

    /// A COUNTDOWN's atom: a comparison of the unknown number where it is a
    /// countdown (a near level's floor `delay`, balloon `timer`, spring timers).
    /// It stays THREE-VALUED wherever the set has other unknowns too: decided
    /// per lane where read and split by the split pass - never a merge refusal
    /// or an escape fork, under which every fall floor's `delay <= 0` made two
    /// successors that never merged (12 floors: 2^13 trace states).
    fn countdown_atom(&mut self) -> NodeId {
        let k = self.frame.unknown_atoms;
        self.frame.unknown_atoms += 1;
        self.graph.leaf(Op::UnknownBool(Unknown::Countdown(k)))
    }

    /// Does no lane's data reach `n` - no input cell or fork?
    pub fn lane_independent(&mut self, n: NodeId) -> bool {
        let dependent = |op: &Op<A>| matches!(op, Op::Cell(_) | Op::Fork(..) | Op::ForkValid(..)).then_some(true);
        !cone_any(&self.graph, &mut self.lane_memo, n, dependent)
    }

    /// `op` over LITERAL operands (one an interval) in `Graph::eval`'s
    /// semantics (one definition); `None` where eval does not model it.
    fn eval_literal(&self, op: &Op<A>, args: &[NodeId]) -> Option<crate::transpile::graph::Val<A>> {
        if !self.unknowns() {
            return None;
        }
        let lits: Vec<(A::Lit, A::Lit)> = args
            .iter()
            .map(|a| match self.graph.get(*a).op {
                Op::Const(lo, hi) => Some((lo, hi)),
                _ => None,
            })
            .collect::<Option<_>>()?;
        if lits.iter().all(|(lo, hi)| lo == hi) {
            return None;
        }
        let mut g = Graph::<A>::default();
        let leaves: Vec<NodeId> = lits.iter().map(|(lo, hi)| g.leaf(Op::Const(*lo, *hi))).collect();
        let n = g.add(op.clone(), leaves);
        g.eval(&std::collections::HashMap::new(), Eval::Exact, None).ok().map(|v| v[n as usize])
    }

    /// `eval_literal`'s number as a literal node.
    fn literal_num(&mut self, op: Op<A>, args: &[NodeId]) -> Option<NodeId> {
        match self.eval_literal(&op, args)? {
            crate::transpile::graph::Val::Num(iv) => {
                let (lo, hi) = A::bounds(iv);
                Some(self.graph.leaf(Op::Const(A::lit(lo), A::lit(hi))))
            }
            crate::transpile::graph::Val::Bool(_) => None,
        }
    }

    /// The steps every operation on `args` takes first, in order: constants
    /// fold, so what the heap depends on stays concrete; the unknown number
    /// makes `unknown` (told whether an operand is a countdown's); an operand
    /// selecting the unknown number on some lanes distributes the operation
    /// over its arms (`unknown_select`). `None` where none applies.
    fn prologue(
        &mut self,
        args: &[NodeId],
        point: impl FnOnce(&mut Self, &[A]) -> Result<NodeId>,
        unknown: impl FnOnce(&mut Self, bool) -> NodeId,
        again: impl Fn(&mut Self, &[NodeId]) -> Result<NodeId>,
    ) -> Result<Option<NodeId>> {
        if let Some(xs) = args.iter().map(|a| self.graph.point(*a)).collect::<Option<Vec<_>>>() {
            return point(self, &xs).map(Some);
        }
        if args.iter().any(|a| self.is_unknown_num(*a)) {
            let countdown = self.countdown_unknown(args);
            return Ok(Some(unknown(self, countdown)));
        }
        for (k, a) in args.iter().enumerate() {
            if let Some((c, t, f)) = self.unknown_select(*a) {
                let mut at = args.to_vec();
                at[k] = t;
                let x = again(self, &at)?;
                at[k] = f;
                let y = again(self, &at)?;
                return Ok(Some(self.graph.fold(Op::Sel, vec![c, x, y])));
            }
        }
        Ok(None)
    }

    fn konst(&mut self, v: A) -> NodeId {
        let l = A::lit(v);
        self.graph.leaf(Op::Const(l, l))
    }
}

impl<A: GraphArith> Domain for Symbolic<A> {
    type A = A;
    type Num = NodeId;
    type Bool = NodeId;

    fn num(&mut self, v: A) -> NodeId {
        self.konst(v)
    }
    fn boolean(&mut self, b: bool) -> NodeId {
        self.graph.leaf(Op::ConstBool(b))
    }
    fn arith(&mut self, op: Arith, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let Some(n) = self.prologue(&[*a, *b], |d, x| Ok(d.konst(fold_arith::<A>(op, x[0], x[1])?)), Self::unknown_num, |d, x| d.arith(op, &x[0], &x[1]))? {
            return Ok(n);
        }
        if let Arith::Sub = op {
            if let Some(n) = self.cancel_sub(*a, *b)? {
                return Ok(n);
            }
        }
        if let Arith::Add = op {
            if let Some(n) = self.cancel_add(*a, *b) {
                return Ok(n);
            }
        }
        if let Some(n) = self.literal_num(op.into(), &[*a, *b]) {
            return Ok(n);
        }
        Ok(self.graph.fold(op.into(), vec![*a, *b]))
    }
    fn fun1(&mut self, f: Fun1, a: &NodeId) -> Result<NodeId> {
        // `sin` of anything is in [-1, 1]; other builtins of unknown: unknown.
        let unknown = |d: &mut Self, countdown: bool| match f {
            Fun1::Sin => d.graph.leaf(Op::Const(A::lit(A::from_i16(-1)), A::lit(A::from_i16(1)))),
            _ => d.unknown_num(countdown),
        };
        if let Some(n) = self.prologue(&[*a], |d, x| Ok(d.konst(fold_fun1::<A>(f, x[0])?)), unknown, |d, x| d.fun1(f, &x[0]))? {
            return Ok(n);
        }
        // `sin` of an interval is [-1, 1]: the emitter cannot lower `Sin` over a ZI.
        if matches!(f, Fun1::Sin) && self.is_interval(a) {
            let lo = self.konst(A::from_i16(-1));
            let hi = self.konst(A::from_i16(1));
            return Ok(self.graph.fold(Op::Span, vec![lo, hi]));
        }
        if let Some(n) = self.literal_num(f.into(), &[*a]) {
            return Ok(n);
        }
        Ok(self.graph.fold(f.into(), vec![*a]))
    }
    fn fun2(&mut self, f: Fun2, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let Some(n) = self.prologue(&[*a, *b], |d, x| Ok(d.konst(fold_fun2::<A>(f, x[0], x[1]))), Self::unknown_num, |d, x| d.fun2(f, &x[0], &x[1]))? {
            return Ok(n);
        }
        if let Some(n) = self.literal_num(f.into(), &[*a, *b]) {
            return Ok(n);
        }
        Ok(self.graph.fold(f.into(), vec![*a, *b]))
    }
    fn compare(&mut self, op: Cmp, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        // With an unknown number nothing is decided: a countdown's atom where
        // the number is a countdown's, or nothing else is unknown.
        let unknown = |d: &mut Self, countdown: bool| if countdown || !d.level.fruit { d.countdown_atom() } else { d.unknown_bool_atom() };
        let point = |d: &mut Self, x: &[A]| Ok(d.graph.leaf(Op::ConstBool(fold_compare::<A>(op, x[0], x[1]))));
        if let Some(n) = self.prologue(&[*a, *b], point, unknown, |d, x| d.compare(op, &x[0], &x[1]))? {
            return Ok(n);
        }
        // Two literals: decided by their intervals, or a fresh atom of their own.
        let g: Op<A> = op.into();
        match self.eval_literal(&g, &[*a, *b]) {
            Some(crate::transpile::graph::Val::Bool(Some(r))) => return Ok(self.graph.leaf(Op::ConstBool(r))),
            Some(crate::transpile::graph::Val::Bool(None)) => return Ok(self.unknown_bool_atom()),
            _ => {}
        }
        // Decided by static ranges iff every pair of pieces agrees.
        if self.has_ranges() {
            if let (Some(xs), Some(ys)) = (self.range_of(*a), self.range_of(*b)) {
                if let Some(r) = A::decide_cmp(&g, &xs, &ys) {
                    return Ok(self.graph.leaf(Op::ConstBool(r)));
                }
            }
        }
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
    fn range_num(&mut self, lo: A, hi: A) -> Result<NodeId> {
        Ok(self.graph.leaf(Op::Const(A::lit(lo), A::lit(hi))))
    }
    /// A button: a fork both of whose configurations every lane takes.
    fn unknown_bool(&mut self) -> Result<NodeId> {
        Ok(self.both_values(UNKNOWN_BOOL_ORIGIN))
    }
    fn node_count(&self) -> usize {
        self.graph.len()
    }
    /// Is this value an interval? Its lane type (`Graph::ty`).
    fn is_interval(&self, v: &NodeId) -> bool {
        self.graph.ty(*v) == Ty::Ival
    }

    /// A literal interval is refused at the rounds: every lane would take
    /// each fragment (the interpreter splits a literal first:
    /// `literal_fragments`).
    fn split(&mut self, cut: Cut, v: &NodeId, ways: u8) -> Result<(NodeId, NodeId)> {
        let origin = match cut {
            Cut::Floors => "a floor fork (`move`)",
            Cut::Rounds => {
                if let Op::Const(lo, hi) = self.graph.get(*v).op {
                    bail!("__split_by_round of the literal [{}, {}] as a per-lane fork", A::show(A::from_lit(lo)), A::show(A::from_lit(hi)));
                }
                "a round fork (`move`)"
            }
            Cut::Ints => bail!("a split cuts at the floors or the rounds, not the whole numbers"),
        };
        let f = self.fork(v, Partition { cut, ways, memo: true }, origin);
        Ok((f.value, f.valid))
    }

    fn split_ways(&mut self, cut: Cut, v: &NodeId) -> u8 {
        // Only restrictions know a static range.
        if !self.has_ranges() {
            return MOVE_WAYS;
        }
        // At most `MOVE_WAYS`: the static range covers ALL lanes, but one lane
        // spans at most that many cells (too small declines via `ForkOk`). Level
        // -1 takes the HULL: its lanes are ranges.
        match self.range_of(*v) {
            Some(ps) => {
                let w = A::spanned(cut.grid(), &ps, self.uncapped_ways);
                (if self.uncapped_ways { w } else { w.min(MOVE_WAYS as i64) }) as u8
            }
            None => MOVE_WAYS,
        }
    }

    fn is_select(&self, v: &super::heap::Value<Self>) -> bool {
        matches!(v, super::heap::Value::Num(n) | super::heap::Value::Bool(n) if self.graph.get(*n).op == Op::Sel)
    }

    fn evaluated_at(&mut self, n: &NodeId, at: &NodeId) {
        let at = match self.frame.evaluated.get(n) {
            Some(before) => self.graph.fold(Op::Or, vec![*before, *at]),
            None => *at,
        };
        self.frame.evaluated.insert(*n, at);
    }

    fn join_num_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        // A condition that reads no atom but a countdown's (`countdown_atom`)
        // is a select, as at a near level alone.
        if !self.reads_unknown_atom(c) || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        if self.is_unknown_num(*t) || self.is_unknown_num(*f) {
            let countdown = self.countdown_unknown(&[*t, *f]);
            return Some(self.unknown_num(countdown));
        }
        match (self.graph.get(*t).op.clone(), self.graph.get(*f).op.clone()) {
            (Op::Const(a0, a1), Op::Const(b0, b1)) => Some(self.graph.leaf(Op::Const(lit_min::<A>(a0, b0), lit_max::<A>(a1, b1)))),
            // One value plus a literal on each side: that value plus the
            // literals' hull, so `x - last` still cancels.
            _ => {
                let (tb, tk) = self.base_plus_literal(*t);
                let (fb, fk) = self.base_plus_literal(*f);
                if tb != fb || matches!(self.graph.get(tb).op, Op::Const(..)) {
                    return None;
                }
                let k = self.graph.leaf(Op::Const(lit_min::<A>(tk.0, fk.0), lit_max::<A>(tk.1, fk.1)));
                Some(self.graph.fold(Op::Add, vec![tb, k]))
            }
        }
    }

    fn reads_unknown_atom(&mut self, c: &NodeId) -> bool {
        if !self.unknowns() || self.decide(c).is_some() {
            return false;
        }
        cone_any(&self.graph, &mut self.atom_memo, *c, |op| match op {
            Op::UnknownBool(k) => Some(!matches!(k, Unknown::Countdown(_))),
            _ => None,
        })
    }

    fn reads_merge_refused(&mut self, c: &NodeId) -> bool {
        if self.frame.merge_refused.is_empty() || self.decide(c).is_some() {
            return false;
        }
        let refused = &self.frame.merge_refused;
        cone_any(&self.graph, &mut self.frame.merge_refused_memo, *c, |op| match op {
            Op::Cell(i) => Some(refused.binary_search(i).is_ok()),
            _ => None,
        })
    }

    fn join_bool_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        // As `join_num_independent`.
        if !self.reads_unknown_atom(c) || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        // Arms every lane holds alike: a fresh atom (escapes as a fork).
        if self.lane_independent(*t) && self.lane_independent(*f) {
            return Some(self.unknown_bool_atom());
        }
        // An arm reading lane data joins EXACTLY in three-valued logic; a
        // fresh atom would throw the lane's data away.
        let ct = self.and(c, t);
        let nc = self.not(c);
        let cf = self.and(&nc, f);
        Some(self.or(&ct, &cf))
    }

    fn atoms_minted(&self) -> u32 {
        self.frame.unknown_atoms
    }

    fn escaped_atom(&mut self, b: &NodeId, since: u32, origin: &dyn Fn(&Self) -> String) -> Option<NodeId> {
        // Only with unknowns, and never a countdown's atom (`countdown_atom`):
        // those stay three-valued (decided where read); forking adds a
        // dimension even if unread.
        if !self.unknowns() {
            return None;
        }
        if let Some(f) = self.frame.escaped.get(b) {
            return Some(*f);
        }
        // The atoms of this call that `b` reads. Only those make a fork; every
        // countdown's `b` reads is enumerated with them all the same, so the
        // restriction below reads no atom (one left in it is unknown on every
        // lane: the fork's coverage would decline them all).
        let atoms: Vec<NodeId> = self.graph.cone(&[*b])
            .into_iter()
            .filter(|n| matches!(self.graph.get(*n).op, Op::UnknownBool(Unknown::Countdown(_))) || matches!(self.graph.get(*n).op, Op::UnknownBool(Unknown::Atom(k)) if k >= since))
            .collect();
        if !atoms.iter().any(|n| matches!(self.graph.get(*n).op, Op::UnknownBool(Unknown::Atom(_)))) {
            return None;
        }
        let name = origin(self);
        let g = self.both_values(&name);
        // A bare atom is both values everywhere. An atom joined with lane data
        // is the fork RESTRICTED per lane to `b`'s possible values, so every
        // later read sees one value per configuration.
        let out = if atoms.len() == 1 && atoms[0] == *b {
            g
        } else if atoms.len() > ESCAPE_ATOMS {
            g
        } else {
            let (mut may_t, mut may_f) = (self.graph.leaf(Op::ConstBool(false)), self.graph.leaf(Op::ConstBool(false)));
            for bits in 0u32..(1 << atoms.len()) {
                let subst: rustc_hash::FxHashMap<NodeId, NodeId> =
                    atoms.iter().enumerate().map(|(i, a)| (*a, self.graph.leaf(Op::ConstBool(bits >> i & 1 == 1)))).collect();
                let v = self.substitute(*b, &subst);
                let nv = self.graph.fold(Op::Not, vec![v]);
                may_t = self.graph.fold(Op::Or, vec![may_t, v]);
                may_f = self.graph.fold(Op::Or, vec![may_f, nv]);
            }
            let not_f = self.graph.fold(Op::Not, vec![may_f]);
            let only_t = self.graph.fold(Op::And, vec![may_t, not_f]);
            let both = self.graph.fold(Op::And, vec![may_t, may_f]);
            let either = self.graph.fold(Op::And, vec![both, g]);
            self.graph.fold(Op::Or, vec![only_t, either])
        };
        self.frame.escaped.insert(*b, out);
        Some(out)
    }

    fn literal_fragments(&mut self, v: &NodeId, round: bool) -> Result<Option<Vec<NodeId>>> {
        if !self.unknowns() {
            return Ok(None);
        }
        let Op::Const(lo, hi) = self.graph.get(*v).op else { return Ok(None) };
        if lo == hi {
            return Ok(None);
        }
        // On the INTEGERS: a fragment need only make `flr` (`flr(v + 1/2)`)
        // exact, and they rejoin into one hull (`move` reads it only via its floor
        // and a linear `rem - 0.5 - amount`).
        let iv = A::ival(A::from_lit(lo), A::from_lit(hi));
        let what = if round { "__split_by_round" } else { "__split_by_flr" };
        let show = || format!("[{}, {}]", A::show(A::from_lit(lo)), A::show(A::from_lit(hi)));
        let n = if round { A::rounds(iv)? } else { A::floors(iv) };
        let n = n.ok_or_else(|| anyhow::anyhow!("{what} of the literal {}: past 65536 integers", show()))?;
        // Too many is refused at trace time (else a body erroring on every lane).
        anyhow::ensure!(
            (n as usize) <= crate::transpile::graph::MAX_WAYS,
            "{what} of the literal {} spans {n} integers, more than {} fragments",
            show(),
            crate::transpile::graph::MAX_WAYS
        );
        let mut out = Vec::new();
        for k in 0..n {
            let frag = if round { A::round_fragment(iv, k)? } else { A::floor_fragment(iv, k) };
            let (a, b) = A::bounds(frag);
            out.push(self.graph.leaf(Op::Const(A::lit(a), A::lit(b))));
        }
        Ok(Some(out))
    }

    fn undecided_atom(&mut self) -> Result<NodeId> {
        Ok(self.unknown_bool_atom())
    }

    fn as_const(&self, v: &NodeId) -> Option<A> {
        self.graph.point(*v)
    }

    fn describe(&self, v: &NodeId) -> String {
        let n = self.graph.get(*v);
        match n.op {
            Op::Const(lo, hi) if lo == hi => A::show(A::from_lit(lo)),
            Op::Const(lo, hi) => format!("{}..{}", A::show(A::from_lit(lo)), A::show(A::from_lit(hi))),
            // One level of operands too: whether a select's arms are constants.
            _ => format!("node {} = {:?}({})", v, n.op, n.args.iter().map(|a| format!("{:?}", self.graph.get(*a).op)).collect::<Vec<_>>().join(", ")),
        }
    }
}

/// Does a node of `n`'s cone satisfy `leaf`? `leaf` decides a node it
/// answers for; every other node is its operands' OR (an operand-free one
/// `false`). Memoized in `memo`: a frame's conditions share their cones.
fn cone_any<A: GraphArith>(g: &Graph<A>, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId, leaf: impl Fn(&Op<A>) -> Option<bool>) -> bool {
    let mut stack: Vec<(NodeId, bool)> = vec![(n, false)];
    while let Some((x, expanded)) = stack.pop() {
        if memo.contains_key(&x) {
            continue;
        }
        let node = g.get(x);
        if let Some(v) = leaf(&node.op) {
            memo.insert(x, v);
        } else if expanded || node.args.is_empty() {
            let v = node.args.iter().any(|a| memo[a]);
            memo.insert(x, v);
        } else {
            stack.push((x, true));
            stack.extend(node.args.iter().filter(|a| !memo.contains_key(a)).map(|a| (*a, false)));
        }
    }
    memo[&n]
}

/// The `move` fork's arity: exact speed plus a rem within one grid cell
/// spans at most two floors.
pub const MOVE_WAYS: u8 = 2;

/// The lesser and the greater of two literals, by value.
fn lit_min<A: Arithmetic>(a: A::Lit, b: A::Lit) -> A::Lit {
    if A::lt(A::from_lit(b), A::from_lit(a)) { b } else { a }
}
fn lit_max<A: Arithmetic>(a: A::Lit, b: A::Lit) -> A::Lit {
    if A::lt(A::from_lit(a), A::from_lit(b)) { b } else { a }
}

/// A refusal instead of a guess: where the heap would have become symbolic.
pub fn refuse_unknown(what: &str) -> anyhow::Error {
    anyhow::anyhow!(
        "{} is not known at trace time - the heap cannot depend on a symbolic value",
        what
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num as P8;

    fn p(v: i16) -> P8 {
        P8::from_i16(v)
    }

    /// THE ROUND FORK (doubles): `rem + ox` of an interval remainder, forked
    /// at `flr(x + 1/2)`, resolves per configuration to the reference engine's
    /// fragment bit for bit, valid exactly where the reference has that many,
    /// and `ForkOk(Rounds)` says whether a lane takes at most `ways` values. Around
    /// the cut below 1, where `0.5 - 2^-54 + 0.5` rounds up.
    #[test]
    fn a_round_fork_resolves_to_the_reference_fragments() {
        use crate::transpile::graph::{Val, OPEN};
        use celeste_core::arith::Float;
        use celeste_core::lua_f64::{LuaF64, LuaF64Interval};
        let f = |v: f64| LuaF64::new(v).expect("finite");
        let ways = 3u8;
        for ox in [0.0, 0.3, 1.0, -1.7, 2.5, 0.5f64.next_down()] {
            let mut d = Symbolic::<Float>::default();
            d.graph.set_cell_kind(0, Ty::Ival);
            d.forget_intervals();
            let x = d.graph.leaf(Op::Cell(0));
            let k = d.num(f(ox));
            let y = d.arith(Arith::Add, &x, &k).expect("add");
            let (v, valid) = d.split(Cut::Rounds, &y, ways).expect("a round fork");
            let fork = d.frame.forks - 1;
            let covered = d.graph.fold(Op::ForkOk(Cut::Rounds, ways), vec![y]);
            for (lo, hi) in [(-0.5, 0.5f64.next_down()), (-0.5, -0.5), (0.2, 0.4), (0.5f64.next_down(), 0.5), (-2.25, 1.75), (0.0, 0.0)] {
                let iv = LuaF64Interval::new(f(lo), f(hi));
                let cells = std::collections::HashMap::from([(0u32, Val::<Float>::Num(iv))]);
                let yv = iv.checked_add(LuaF64Interval::point(f(ox))).expect("finite");
                let n = yv.rounds().expect("rounds").expect("few");
                for c in 0..ways {
                    let mut splits = vec![OPEN; d.frame.forks as usize];
                    splits[fork as usize] = c;
                    let mut g = d.graph.like();
                    let map = d.graph.specialize_subset_into(&splits, None, &mut g);
                    // Strict, a lane spanning more values than the fork's
                    // arity is an error of the evaluation (as `ForkOk`).
                    let vals = match g.eval(&cells, Eval::Exact, None) {
                        Ok(vals) => vals,
                        Err(e) => {
                            assert!(n > ways as u32 && format!("{e}").contains("ForkOk"), "ox {ox} [{lo}, {hi}] c {c}: {e}");
                            continue;
                        }
                    };
                    assert_eq!(vals[map[covered as usize] as usize], Val::Bool(Some(true)), "ox {ox} [{lo}, {hi}]: coverage");
                    assert!(n <= ways as u32, "ox {ox} [{lo}, {hi}]: {n} values covered by {ways}");
                    let ok = (c as u32) < n;
                    assert_eq!(vals[map[valid as usize] as usize], Val::Bool(Some(ok)), "ox {ox} [{lo}, {hi}] c {c}: validity");
                    if ok {
                        let want = yv.round_fragment(c as u32).expect("a fragment");
                        let Val::Num(got) = vals[map[v as usize] as usize] else { panic!("a number") };
                        assert_eq!((got.lo.to_bits(), got.hi.to_bits()), (want.lo.to_bits(), want.hi.to_bits()), "ox {ox} [{lo}, {hi}] c {c}: the fragment");
                    }
                }
            }
        }
    }

    /// `__split_by_round` of a LITERAL (a literal remainder plus a speed, in
    /// float) splits into the arithmetic's round fragments, as a literal
    /// `__split_by_flr` into its floors; a point is no split.
    #[test]
    fn a_literal_round_split_takes_the_arithmetics_round_fragments() {
        use celeste_core::arith::{Arithmetic, Float};
        use celeste_core::lua_f64::{LuaF64, LuaF64Interval};
        let f = |v: f64| LuaF64::new(v).expect("finite");
        let mut d = Symbolic::<Float>::default();
        d.level.fruit = true;
        // The literal rem [-1/2, pred(1/2)] plus a speed -0.65.
        let iv = LuaF64Interval::new(f(-0.5), f(0.5f64.next_down())).checked_add(LuaF64Interval::point(f(-0.65))).expect("finite");
        let lit = d.graph.leaf(Op::Const(Float::lit(iv.lo), Float::lit(iv.hi)));
        let frags = d.literal_fragments(&lit, true).expect("in range").expect("a split");
        let n = iv.rounds().expect("rounds").expect("few");
        assert_eq!((frags.len(), n), (2, 2), "flr(x + 1/2) is -1 or 0 over [{:?}, {:?}]", iv.lo, iv.hi);
        for (k, v) in frags.iter().enumerate() {
            let want = iv.round_fragment(k as u32).expect("a fragment");
            assert_eq!(d.graph.get(*v).op, Op::Const(Float::lit(want.lo), Float::lit(want.hi)), "fragment {k}");
        }
        let point = d.graph.leaf(Op::Const(Float::lit(f(0.25)), Float::lit(f(0.25))));
        assert!(d.literal_fragments(&point, true).expect("in range").is_none(), "a point is no split");
    }

    /// The fake wall's `spd.x = -sign(spd.x)*1.5` before `move`: three pieces
    /// two floors wide each; a kernel lane holds one (2-way), level -1 the
    /// HULL (five floors).
    #[test]
    fn level_minus_one_sizes_a_move_fork_by_the_hull_of_its_pieces() {
        let mut d = Symbolic::<celeste_core::arith::Fixed>::default();
        let (rem, spd) = (d.graph.leaf(Op::Cell(0)), d.graph.leaf(Op::Cell(1)));
        let (rem, spd) = (d.restrict(rem, -0x8000, 0x7fff), d.restrict(spd, -5 << 16, 5 << 16));
        let zero = d.graph.leaf(Op::Const(0, 0));
        let k = |d: &mut Symbolic, v: i32| d.graph.leaf(Op::Const(v, v));
        let (neg, pos, half) = (k(&mut d, -0x18000), k(&mut d, 0x18000), k(&mut d, 0x8000));
        let gt = d.graph.fold(Op::Gt, vec![spd, zero]);
        let lt = d.graph.fold(Op::Lt, vec![spd, zero]);
        let inner = d.graph.fold(Op::Sel, vec![lt, pos, zero]);
        let wall = d.graph.fold(Op::Sel, vec![gt, neg, inner]);
        let moved = d.graph.fold(Op::Add, vec![rem, wall]);
        let operand = d.graph.fold(Op::Add, vec![half, moved]);
        assert_eq!(d.split_ways(Cut::Floors, &operand), 2, "a kernel lane takes one piece");
        d.uncapped_ways = true;
        d.frame.range_memo.clear();
        assert_eq!(d.split_ways(Cut::Floors, &operand), 5, "level -1 joins the pieces: floors -2..=2");
    }

    /// The FORK TRIGGER's contract on hand-built graphs.
    #[test]
    fn the_fork_trigger_excludes_what_a_lane_decides_for_itself() {
        let mut d = Symbolic::<celeste_core::arith::Fixed>::default();
        let ival = d.graph.leaf(Op::Cell(0));
        let exact = d.graph.leaf(Op::Cell(1));
        d.graph.set_cell_kind(0, Ty::Ival);
        d.forget_intervals();
        let zero = d.graph.leaf(Op::Const(0, 0));

        // A comparison on an interval operand IS the source of forking.
        let cmp = d.graph.fold(Op::Gt, vec![ival, zero]);
        assert!(d.lane_undecidable(cmp), "a comparison on an interval must fork");
        // On an exact operand it is not.
        let exact_cmp = d.graph.fold(Op::Gt, vec![exact, zero]);
        assert!(!d.lane_undecidable(exact_cmp));

        // `Flr` is lane-decidable BY ASSERTION, not a fork trigger.
        let flr = d.graph.fold(Op::Flr, vec![ival]);
        let flr_cmp = d.graph.fold(Op::Gt, vec![flr, zero]);
        assert!(!d.lane_undecidable(flr_cmp), "Flr is decided per lane by its assertion");

        // `TileFlagAt` is lane-decidable BY INSTRUCTION.
        let w = d.graph.leaf(Op::Const(8 << 16, 8 << 16));
        let tile = d.graph.fold(Op::TileFlagAt, vec![ival, ival, w, w, zero]);
        assert!(!d.lane_undecidable(tile), "TileFlagAt is decided per lane");
        // And a boolean tree over it stays decidable.
        let not_tile = d.graph.fold(Op::Not, vec![tile]);
        assert!(!d.lane_undecidable(not_tile));

        // THE `Sel` ASYMMETRY: a NUMERIC select ignores the condition.
        let five = d.graph.leaf(Op::Const(5 << 16, 5 << 16));
        let seven = d.graph.leaf(Op::Const(7 << 16, 7 << 16));
        let num_sel = d.graph.fold(Op::Sel, vec![cmp, five, seven]);
        let sel_cmp = d.graph.fold(Op::Gt, vec![num_sel, zero]);
        assert!(
            !d.lane_undecidable(sel_cmp),
            "a select's condition does not make its numeric result span values"
        );
        // A BOOLEAN select straddles exactly when its condition does.
        let t = d.graph.leaf(Op::ConstBool(true));
        let f = d.graph.leaf(Op::ConstBool(false));
        let bool_sel = d.graph.fold(Op::Sel, vec![cmp, t, f]);
        assert!(d.lane_undecidable(bool_sel), "a boolean select straddles with its condition");
    }

    /// A countdown set on some lanes and unknown on others: `delay - 1` and
    /// `delay - 1 <= 0` distribute over the select, so set lanes stay exact.
    #[test]
    fn an_operation_on_a_select_of_the_unknown_number_is_distributed() {
        let mut d = Symbolic::<celeste_core::arith::Fixed>::default();
        let c = d.graph.leaf(Op::Cell(0));
        let (sixty, one, zero) = (d.num(p(60)), d.num(p(1)), d.num(p(0)));
        let u = d.unknown_num(false);
        let delay = d.sel_num(&c, &sixty, &u);
        let less = d.arith(Arith::Sub, &delay, &one).unwrap();
        let fifty_nine = d.num(p(59));
        assert_eq!(less, d.sel_num(&c, &fifty_nine, &u));
        let done = d.compare(Cmp::Le, &less, &zero).unwrap();
        let node = d.graph.get(done).clone();
        assert!(d.graph.cone(&[done]).iter().all(|n| !matches!(d.graph.get(*n).op, Op::UnknownNum { .. })), "{node:?}");
        assert!(d.graph.cone(&[done]).iter().any(|n| matches!(d.graph.get(*n).op, Op::UnknownBool(_))), "the unknown lanes: an atom");
    }

    /// `x == k` on an interval `x` may be true where `k` is in `x` and false
    /// where `x` is not exactly `k` (`[0, 2]`, `[1, 1]`, `[0, 0]` vs `k = 1`),
    /// also with the fly fruit unknown elsewhere in the frame (room (3,0)
    /// `r0sxhfn`'s hidden near floor).
    #[test]
    fn an_equality_on_an_interval_may_answer_as_its_ends_allow() {
        use crate::transpile::graph::Val;
        let one = 1 << 16;
        for fruit_unknown in [false, true] {
            let answers = |lo: i32, hi: i32| {
                let mut d = Symbolic::<celeste_core::arith::Fixed> { level: celeste_engine::widening::Level { fruit: fruit_unknown, ..Default::default() }, ..Default::default() };
                let x = d.graph.leaf(Op::Const(lo, hi));
                let k = d.graph.leaf(Op::Const(one, one));
                let eq = d.graph.fold(Op::Eq, vec![x, k]);
                let (t, f) = d.may_answers(eq);
                let v = d.graph.eval(&Default::default(), Eval::Exact, None).unwrap();
                let b = |n: NodeId| match v[n as usize] {
                    Val::Bool(b) => b,
                    Val::Num(_) => panic!("a number"),
                };
                (b(t), b(f))
            };
            assert_eq!(answers(0, 2 * one), (Some(true), Some(true)), "[0, 2] both ways");
            assert_eq!(answers(one, one), (Some(true), Some(false)), "[1, 1] only equal");
            assert_eq!(answers(0, 0), (Some(false), Some(true)), "[0, 0] only unequal");
        }
        // An operand reading an unknown: no ends to read, both ways.
        let mut d = Symbolic::<celeste_core::arith::Fixed> { level: celeste_engine::widening::Level { fruit: true, ..Default::default() }, ..Default::default() };
        let u = d.unknown_bool_atom();
        let (a, b) = (d.graph.leaf(Op::Const(0, 2 * one)), d.graph.leaf(Op::Const(one, one)));
        let x = d.graph.fold(Op::Sel, vec![u, a, b]);
        let k = d.graph.leaf(Op::Const(one, one));
        let eq = d.graph.fold(Op::Eq, vec![x, k]);
        let yes = d.graph.leaf(Op::ConstBool(true));
        assert_eq!(d.may_answers(eq), (yes, yes));
    }
}
