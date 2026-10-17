//! The graph IR: one traced frame as a pure, hash-consed DAG of
//! `(op, [operand ids])` over input cells and literals. It describes ONE
//! lane (uniformity is derived later), has no effects (a body's `live` and
//! `error` are boolean nodes), and no types beyond bool/number: exactness is
//! a property of the value, so one op covers every width.

use std::collections::HashMap;

use std::sync::Arc;

use anyhow::{anyhow, bail, Result};

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

use celeste_core::arith::{Arithmetic, Fixed};
use celeste_core::pico8_num::Pico8Num;

pub use super::garith::GraphArith;

/// An abstract value for ONE lane: an exact number is a singleton interval,
/// a known boolean is `Some`. In the arithmetic `A` (16.16 by default).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Val<A: Arithmetic = Fixed> {
    Num(A::Ival),
    /// `None` = unknown, the abstract interpreter's third boolean.
    Bool(Option<bool>),
}

impl<A: Arithmetic> Val<A> {
    pub fn exact_num(n: A::Num) -> Self {
        Val::Num(A::point(n))
    }
    /// The number this value is, if it is a number and it is exact.
    fn as_exact(self) -> Option<A::Num> {
        match self {
            Val::Num(i) => A::as_point(i),
            Val::Bool(_) => None,
        }
    }
    fn as_num(self, op: &str) -> Result<A::Ival> {
        match self {
            Val::Num(i) => Ok(i),
            Val::Bool(_) => bail!("{}: expected a number, got a boolean", op),
        }
    }
    fn as_bool(self, op: &str) -> Result<Option<bool>> {
        match self {
            Val::Bool(b) => Ok(b),
            Val::Num(_) => bail!("{}: expected a boolean, got a number", op),
        }
    }
}

pub type NodeId = u32;

/// In a `splits` vector: leave this fork standing.
pub const OPEN: u8 = u8::MAX;

/// OR-ed into a `splits` fragment index: that fragment with validity TRUE, for
/// a DEAD fork whose fragments all give the same row (the OR of their
/// validities is the lane covered).
pub const ANY_VALID: u8 = 0x80;

/// The map, for the evaluator (`CollisionCache` knows its room).
#[derive(Clone)]
pub struct Room {
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
}

/// What a `Pin` reads its input as: a number (a point, or a literal
/// interval `[lo, hi]`, as literals: `Arithmetic::Lit`) or a boolean.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Pinned<A: Arithmetic = Fixed> {
    Num(A::Lit, A::Lit),
    Bool(bool),
}

/// The op vocabulary: one variant per MEANING, not per emitted form. In the
/// arithmetic `A` (16.16 by default): its literals are `A::Lit`.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Op<A: Arithmetic = Fixed> {
    // Leaves.
    /// A literal: low and high bounds as `A::Lit` (raw 16.16, a double's
    /// bits: raw so `Op` is `Hash`); an exact literal is `Const(x, x)`.
    Const(A::Lit, A::Lit),
    ConstBool(bool),
    /// An input cell.
    Cell(u32),
    /// `Split(d)`: the operand RESTRICTED to fork d's outcome, eliminated by
    /// specialization.
    Split(u8),

    // number -> number
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    Abs,
    Flr,
    Sin,
    Min,
    Max,
    /// `Span(lo, hi)`: from `lo`'s low to `hi`'s high bound, for a widening
    /// with per-lane bounds (a fruit's bob band). Exactly the hull; containment
    /// is the widening's own error.
    Span,
    /// `Restrict(lo, hi)(x)`: `x`, ASSUMED to lie in `[lo, hi]` (raw 16.16,
    /// inclusive). A bounded kernel input (the region's square, speed and
    /// remainder) enters the body only through one, so the range analyses
    /// (`eval`'s hull, `pieces_of`) take the range from this node and never
    /// from a seeded environment. The kernel computes `x` unchanged; the
    /// assumption is the node's OWN ERROR, `Lo(x) < lo or Hi(x) > hi` on the
    /// raw operand (`trace::error`), charged to every outcome of the frame,
    /// because a comparison the range decided no longer reads the node.
    Restrict(A::Lit, A::Lit),
    /// `Pin(c)(x)`: the input `x` ASSUMED to lie in the constant `c` (a
    /// number, a literal interval or a boolean; `Pinned`), the twin of
    /// `Restrict` that DISCONNECTS its input: every USE of the input reads
    /// the constant `c`, never this node or `x`, so the node has no data
    /// output and exists for its check, `x` inside `c` on the raw input
    /// (`trace::error::pins_hold`), an error of the whole frame. A shape's
    /// lattice constants enter a kernel this way (points), and so do inputs
    /// read as a literal range so that lanes merge across it (a fly fruit's
    /// `spd.y`/`rem.y`, a platform's `rem.x`).
    Pin(Pinned<A>),
    /// THE UNKNOWN NUMBER. Not the full-range interval (interval arithmetic
    /// at the extremes raises): it stays unknown under every operation and a
    /// comparison with it is an `UnknownBool`. It never reaches a kernel: it
    /// is stored only as `AV::UNum`, and `bind` refuses a root reading it.
    UnknownNum,

    // number -> bool
    Lt,
    Le,
    Gt,
    Ge,
    Eq,

    // bool -> bool
    Not,
    /// Tri-state AND: a decided `false` wins over an unknown.
    And,
    /// Tri-state OR: a decided `true` wins (its own node so `a or b` and
    /// `b or a` intern alike).
    Or,
    /// An UNDECIDED boolean, the same in every lane. The index separates
    /// sites, so simplification never treats two independent unknowns as one.
    UnknownBool(u32),

    /// `Sel(cond, then, else)`, in every width.
    Sel,

    /// `Known(v)`: is the value DETERMINED? Survives only in derived errors.
    Known,
    /// `SplitValid(d)`: which lanes fall in the chosen fragment (fragments
    /// PARTITION the lanes). Dropped for forks every lane takes both ways.
    SplitValid(u8),
    /// `Split(d)` specialized: fragment `c` is the `c`-th grid cell from the
    /// one the low end lies in, clipped to the interval. `Frag` narrows to it,
    /// `FragOk` says whether the interval reaches it (together `zi_fork_flr`).
    /// Ordinary unary ops, so configurations SHARE every node they agree on.
    Frag(u8),
    FragOk(u8),
    /// A fork over WHOLE numbers whose fragments are the numbers, EXACT:
    /// resolves to `IntFrag(c)`, fragment `c`'s low end. Validity and premise
    /// are shared with `Split`.
    SplitInt(u8),
    IntFrag(u8),
    /// `SplitOk(n)`: does this lane's interval span at most `n` floors (the
    /// arity)? Otherwise the lane declines. Shared by every configuration.
    SplitOk(u8),
    /// `NoWrap(x)`, `x` an interval `+`, `-`, negation, or `*` / `/` by a
    /// positive constant: did no endpoint leave the 16.16 range on this lane
    /// (`Pico8NumInterval`'s checked operations)? The kernel wraps (`/`
    /// saturates) in i32, so that is the operation's OWN ERROR and the lane
    /// declines. True of anything else (`scaled`: which `*` and `/`).
    NoWrap,
    /// An interval's endpoints as plain numbers (of a number, the number), so
    /// a fork at a TABLE of cuts is ordinary arithmetic once specialized.
    Lo,
    Hi,
    // Cart lookups.
    Mget,
    TileFlagAt,

    // The round fork, where `x + 1/2` rounds (doubles).
    /// `RSplit(d)`: the operand RESTRICTED to fork d's outcome in the ROUND
    /// partition: fragment `c` is where `flr(x + 1/2)` takes the `c`-th
    /// value from the low end's, cut at the arithmetic's exact points
    /// (`Arithmetic::round_fragment`). Eliminated by specialization. 16.16
    /// never makes one: its round fork is the floor fork of `x + 1/2`,
    /// shifted back exactly.
    RSplit(u8),
    /// `SplitValid`'s twin: which lanes fall in the chosen fragment.
    RSplitValid(u8),
    /// `RSplit(d)` specialized: `Frag`/`FragOk`'s twins.
    RFrag(u8),
    RFragOk(u8),
    /// `SplitOk`'s twin: does this lane's interval take at most `n` values
    /// of `flr(x + 1/2)` (the arity)? Otherwise the lane declines.
    RSplitOk(u8),
    /// `Same(x, y)`: are two numbers the same BITS? A pin's check where the
    /// cart's `==` is not identity (a double's `-0 == 0`).
    Same,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Node<A: Arithmetic = Fixed> {
    pub op: Op<A>,
    pub args: Vec<NodeId>,
}

/// A hash-consed arena: same op over same operand ids is the same node.
#[derive(Default, Clone)]
pub struct Graph<A: GraphArith = Fixed> {
    nodes: Vec<Node<A>>,
    /// rustc-hash 2 (1.1's Fx made probing dominate). Never iterated.
    intern: rustc_hash2::FxHashMap<Node<A>, NodeId>,
    /// Per fork `d`, its fragment count (absent = 2).
    fork_ways: Vec<u8>,
    /// Each input cell's KIND, recorded where made and carried through every
    /// rebuild: the evaluator's weakest value for it. Not guessed from uses.
    cell_kinds: rustc_hash::FxHashMap<u32, CellKind>,
}

/// The kind of an input cell (`Graph::cell_kind`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CellKind {
    Num,
    Ival,
    Bool,
}

/// The most fragments one fork can have (a byte names a fragment).
pub const MAX_WAYS: usize = 255;

/// `Graph::eval_full`'s `defined`.
const DEFINED: bool = true;

impl Graph<Fixed> {
    /// An empty 16.16 graph (`Graph::<A>::default()` in another arithmetic).
    pub fn new() -> Self {
        Self::default()
    }
}

impl<A: GraphArith> Graph<A> {

    /// An empty graph on the same fork grid and cell kinds.
    pub fn like(&self) -> Self {
        Graph {
            fork_ways: self.fork_ways.clone(),
            cell_kinds: self.cell_kinds.clone(),
            ..Default::default()
        }
    }

    pub fn set_cell_kind(&mut self, cell: u32, kind: CellKind) {
        self.cell_kinds.insert(cell, kind);
    }

    /// The recorded kind of `cell`; an undeclared cell is a number.
    pub fn cell_kind(&self, cell: u32) -> CellKind {
        self.cell_kinds.get(&cell).copied().unwrap_or(CellKind::Num)
    }

    /// The kinds, for a rebuild that renumbers the cells.
    pub fn cell_kinds(&self) -> impl Iterator<Item = (u32, CellKind)> + '_ {
        self.cell_kinds.iter().map(|(c, k)| (*c, *k))
    }

    /// Forget every fork's arity: a new frame's forks restart at 0.
    pub fn reset_forks(&mut self) {
        self.fork_ways.clear();
    }

    /// Fork `d`'s arity: how many fragments `Split(d)` resolves to.
    pub fn fork_ways(&self, d: u8) -> u8 {
        self.fork_ways.get(d as usize).copied().unwrap_or(2)
    }

    /// Give fork `d` at least `ways` fragments. Monotone: each site's own
    /// `SplitOk` bounds the lanes it admits.
    pub fn set_fork_ways(&mut self, d: u8, ways: u8) {
        debug_assert!(ways >= 1, "fork arity {ways}");
        if self.fork_ways.len() <= d as usize {
            self.fork_ways.resize(d as usize + 1, 2);
        }
        self.fork_ways[d as usize] = self.fork_ways[d as usize].max(ways);
    }

    pub fn add(&mut self, op: Op<A>, args: Vec<NodeId>) -> NodeId {
        let node = Node { op, args };
        if let Some(id) = self.intern.get(&node) {
            return *id;
        }
        let id = self.nodes.len() as NodeId;
        self.nodes.push(node.clone());
        self.intern.insert(node, id);
        id
    }

    pub fn leaf(&mut self, op: Op<A>) -> NodeId {
        self.add(op, Vec::new())
    }

    pub fn get(&self, id: NodeId) -> &Node<A> {
        &self.nodes[id as usize]
    }

    /// Per node of the arena: does one of `roots` read it (through
    /// operands)?
    pub fn reachable(&self, roots: &[NodeId]) -> Vec<bool> {
        let mut need = vec![false; self.len()];
        let mut stack: Vec<NodeId> = roots.to_vec();
        while let Some(n) = stack.pop() {
            if !std::mem::replace(&mut need[n as usize], true) {
                stack.extend(self.get(n).args.iter().copied());
            }
        }
        need
    }

    /// The nodes `roots` read, ascending (operands precede their node).
    /// Its cost is the cone's, not the arena's.
    pub fn cone(&self, roots: &[NodeId]) -> Vec<NodeId> {
        let mut seen: rustc_hash::FxHashSet<NodeId> = Default::default();
        let mut stack: Vec<NodeId> = roots.to_vec();
        while let Some(n) = stack.pop() {
            if seen.insert(n) {
                stack.extend(self.get(n).args.iter().copied());
            }
        }
        let mut out: Vec<NodeId> = seen.into_iter().collect();
        out.sort_unstable();
        out
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Is `n` a fork over a LITERAL, one every lane takes both ways?
    pub fn is_literal_fork(&self, n: NodeId) -> bool {
        let node = self.get(n);
        matches!(node.op, Op::Split(_) | Op::SplitInt(_)) && matches!(self.get(node.args[0]).op, Op::Const(lo, hi) if lo != hi)
    }

    /// Rebuild into `out` with forks RESOLVED per `splits` (`OPEN` leaves one
    /// standing), over a SUBSET (`need[i]` false maps `i` to `UNBUILT`),
    /// folding as it goes; returns old -> new. `out` is shared and
    /// hash-consed, so configurations computing the same thing share node ids.
    /// Ids only refer downward, so one forward pass suffices.
    pub fn specialize_subset_into(&self, splits: &[u8], need: Option<&[bool]>, out: &mut Graph<A>) -> Vec<NodeId> {
        out.fork_ways = self.fork_ways.clone();
        const UNBUILT: NodeId = NodeId::MAX;
        let mut map: Vec<NodeId> = Vec::with_capacity(self.nodes.len());
        for (i, node) in self.nodes.iter().enumerate() {
            if let Some(need) = need {
                if !need[i] {
                    map.push(UNBUILT);
                    continue;
                }
            }
            let arg = |map: &Vec<NodeId>, k: usize| map[node.args[k] as usize];
            let s = splits;
            let id = match node.op.clone() {
                Op::Split(d) if s[d as usize] != OPEN => out.fold(Op::Frag(s[d as usize] & !ANY_VALID), vec![arg(&map, 0)]),
                Op::SplitValid(d) if s[d as usize] != OPEN && s[d as usize] & ANY_VALID != 0 => out.leaf(Op::ConstBool(true)),
                Op::SplitValid(d) if s[d as usize] != OPEN => out.fold(Op::FragOk(s[d as usize]), vec![arg(&map, 0)]),
                Op::SplitInt(d) if s[d as usize] != OPEN => out.fold(Op::IntFrag(s[d as usize] & !ANY_VALID), vec![arg(&map, 0)]),
                Op::RSplit(d) if s[d as usize] != OPEN => out.fold(Op::RFrag(s[d as usize] & !ANY_VALID), vec![arg(&map, 0)]),
                Op::RSplitValid(d) if s[d as usize] != OPEN && s[d as usize] & ANY_VALID != 0 => out.leaf(Op::ConstBool(true)),
                Op::RSplitValid(d) if s[d as usize] != OPEN => out.fold(Op::RFragOk(s[d as usize]), vec![arg(&map, 0)]),
                _ => {
                    let args: Vec<NodeId> =
                        node.args.iter().map(|a| map[*a as usize]).collect();
                    out.fold(node.op.clone(), args)
                }
            };
            map.push(id);
        }
        map
    }

    /// Is operand order meaningless? Commuting ops get a canonical order since
    /// interning is the only sharing. Not `Mul`: `arith` is monotone only with
    /// the EXACT side second. Not the doubles' `min`/`max`
    /// (`GraphArith::MINMAX_COMMUTE`).
    fn commutes(op: &Op<A>) -> bool {
        matches!(op, Op::Add | Op::Eq | Op::And | Op::Or) || (A::MINMAX_COMMUTE && matches!(op, Op::Min | Op::Max))
    }

    /// Add `op(args)`, normalizing and folding. Every rule is EXACT w.r.t.
    /// `eval`, not merely sound: a refining rule (`x and not x` -> false)
    /// would change which lanes are in error
    /// (`folding_is_exact_not_merely_sound`).
    pub fn fold(&mut self, op: Op<A>, mut args: Vec<NodeId>) -> NodeId {
        if args.len() == 2 {
            if Self::commutes(&op) {
                if args[0] > args[1] {
                    args.swap(0, 1);
                }
            } else if op == Op::Mul {
                let konst = |g: &Graph<A>, a: NodeId| matches!(g.nodes[a as usize].op, Op::Const(..));
                if konst(self, args[0]) && !konst(self, args[1]) {
                    args.swap(0, 1);
                }
            }
        }
        let cbool = |g: &Graph<A>, i: usize| -> Option<bool> {
            args.get(i).and_then(|a| match g.nodes[*a as usize].op {
                Op::ConstBool(b) => Some(b),
                _ => None,
            })
        };
        match op {
            // Fragment 0 is never empty, so `FragOk(0)` is true (as in `eval`),
            // making configuration 0 as cheap as no fork.
            Op::FragOk(0) | Op::RFragOk(0) => return self.leaf(Op::ConstBool(true)),
            // The span premise over a LITERAL is decided once (`zi_span_ok`).
            Op::SplitOk(n) => {
                if let Op::Const(lo, hi) = self.nodes[args[0] as usize].op {
                    return self.leaf(Op::ConstBool(A::lit_split_ok(lo, hi, n)));
                }
            }
            Op::RSplitOk(n) => {
                if let Op::Const(lo, hi) = self.nodes[args[0] as usize].op {
                    if let Some(k) = round_count::<A>(A::ival(A::from_lit(lo), A::from_lit(hi))) {
                        return self.leaf(Op::ConstBool(k <= n as u32));
                    }
                }
            }
            // Two literal points are the same bits or not.
            Op::Same => {
                let point = |g: &Graph<A>, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) if lo == hi => Some(A::from_lit(lo)),
                    _ => None,
                };
                if let (Some(x), Some(y)) = (point(self, 0), point(self, 1)) {
                    return self.leaf(Op::ConstBool(A::bits(x) == A::bits(y)));
                }
            }
            // `IntFrag(c)` of a literal is a constant (as in `eval`): buttons
            // become constants.
            Op::IntFrag(c) if matches!(self.nodes[args[0] as usize].op, Op::Const(..)) => {
                let Op::Const(lo, _) = self.nodes[args[0] as usize].op else { unreachable!("guarded above") };
                let v = A::lit_int_frag(lo, c);
                return self.leaf(Op::Const(v, v));
            }
            // No-wrap decided where the arithmetic can (`GraphArith::fold_no_wrap`).
            Op::NoWrap => {
                if let Some(b) = A::fold_no_wrap(self, args[0]) {
                    return self.leaf(Op::ConstBool(b));
                }
            }
            // Arithmetic on literal POINTS folds EXACTLY (the arithmetic's own
            // operation, bit for bit), so a select on it folds instead of being
            // split both ways. A result past the arithmetic panics with the
            // operands (`checked`): `fold` has no error to return, the traced
            // Lua's constant arithmetic errs in the domain (`Symbolic::arith`),
            // and what a specialization or a substitution folds overflows
            // nowhere.
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Min | Op::Max => {
                let point = |g: &Graph<A>, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) if lo == hi => Some(A::from_lit(lo)),
                    _ => None,
                };
                if let (Some(x), Some(y)) = (point(self, 0), point(self, 1)) {
                    let r = match op {
                        Op::Add => checked::<A>("+", A::add(x, y), x, y),
                        Op::Sub => checked::<A>("-", A::sub(x, y), x, y),
                        Op::Mul => checked::<A>("*", A::mul(x, y), x, y),
                        Op::Div => checked::<A>("/", A::div(x, y), x, y),
                        Op::Min => A::min(x, y),
                        _ => A::max(x, y),
                    };
                    let l = A::lit(r);
                    return self.leaf(Op::Const(l, l));
                }
            }
            Op::Neg | Op::Abs | Op::Flr => {
                if let Op::Const(lo, hi) = self.nodes[args[0] as usize].op {
                    if lo == hi {
                        let x = A::from_lit(lo);
                        let r = match op {
                            Op::Neg => checked::<A>("unary -", A::neg(x), x, x),
                            Op::Abs => checked::<A>("abs", A::abs(x), x, x),
                            _ => A::flr(x),
                        };
                        let l = A::lit(r);
                        return self.leaf(Op::Const(l, l));
                    }
                }
            }
            // A comparison of two literal POINTS is a constant.
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => {
                let point = |g: &Graph<A>, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) if lo == hi => Some(A::from_lit(lo)),
                    _ => None,
                };
                if let (Some(x), Some(y)) = (point(self, 0), point(self, 1)) {
                    let r = match op {
                        Op::Lt => A::lt(x, y),
                        Op::Le => A::le(x, y),
                        Op::Gt => A::lt(y, x),
                        Op::Ge => A::le(y, x),
                        _ => A::num_eq(x, y),
                    };
                    return self.leaf(Op::ConstBool(r));
                }
            }
            // A span of two literals IS a literal interval (as in `eval`).
            Op::Span => {
                let lit = |g: &Graph<A>, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) => Some((lo, hi)),
                    _ => None,
                };
                if let (Some((lo, _)), Some((_, hi))) = (lit(self, 0), lit(self, 1)) {
                    // Tolerant of `lo > hi` (an unreached table-fork fragment).
                    let (lo, hi) = if A::lt(A::from_lit(hi), A::from_lit(lo)) { (hi, lo) } else { (lo, hi) };
                    return self.leaf(Op::Const(lo, hi));
                }
            }
            // A decided condition picks its arm.
            Op::Sel => {
                if let Some(c) = cbool(self, 0) {
                    return if c { args[1] } else { args[2] };
                }
                // Same arms: the condition cannot matter (keeps joins cheap).
                if args[1] == args[2] {
                    return args[1];
                }
                // A boolean select with a constant arm is boolean algebra,
                // exact in Kleene (`sel(c, true, x)` = `c or x`).
                let (c, t, f) = (args[0], args[1], args[2]);
                match (cbool(self, 1), cbool(self, 2)) {
                    (Some(true), Some(false)) => return c,
                    (Some(false), Some(true)) => return self.fold(Op::Not, vec![c]),
                    (Some(true), None) => return self.fold(Op::Or, vec![c, f]),
                    (Some(false), None) => {
                        let n = self.fold(Op::Not, vec![c]);
                        return self.fold(Op::And, vec![n, f]);
                    }
                    (None, Some(true)) => {
                        let n = self.fold(Op::Not, vec![c]);
                        return self.fold(Op::Or, vec![n, t]);
                    }
                    (None, Some(false)) => return self.fold(Op::And, vec![c, t]),
                    _ => {}
                }
            }
            Op::Not => {
                if let Some(b) = cbool(self, 0) {
                    return self.leaf(Op::ConstBool(!b));
                }
                let inner = self.nodes[args[0] as usize].clone();
                // Involution.
                if inner.op == Op::Not {
                    return inner.args[0];
                }
                // A negated comparison is the opposite comparison (exact on
                // intervals): `x < y` and `not (x >= y)` are one node.
                let flip = match inner.op {
                    Op::Lt => Some(Op::Ge),
                    Op::Le => Some(Op::Gt),
                    Op::Gt => Some(Op::Le),
                    Op::Ge => Some(Op::Lt),
                    _ => None,
                };
                if let Some(f) = flip {
                    return self.fold(f, inner.args);
                }
            }
            Op::Known => {
                if cbool(self, 0).is_some() {
                    return self.leaf(Op::ConstBool(true));
                }
                // Negation preserves decidedness.
                let inner = self.nodes[args[0] as usize].clone();
                if inner.op == Op::Not {
                    return self.fold(Op::Known, inner.args);
                }
            }
            Op::And => {
                if args[0] == args[1] {
                    return args[0];
                }
                let (x, y) = (cbool(self, 0), cbool(self, 1));
                // `false AND anything` is false.
                if x == Some(false) || y == Some(false) {
                    return self.leaf(Op::ConstBool(false));
                }
                match (x, y) {
                    (Some(_), Some(_)) => return self.leaf(Op::ConstBool(true)),
                    (Some(_), None) => return args[1],
                    (None, Some(_)) => return args[0],
                    _ => {}
                }
            }
            Op::Or => {
                if args[0] == args[1] {
                    return args[0];
                }
                let (x, y) = (cbool(self, 0), cbool(self, 1));
                // `true OR anything` is true.
                if x == Some(true) || y == Some(true) {
                    return self.leaf(Op::ConstBool(true));
                }
                match (x, y) {
                    (Some(_), Some(_)) => return self.leaf(Op::ConstBool(false)),
                    (Some(_), None) => return args[1],
                    (None, Some(_)) => return args[0],
                    _ => {}
                }
                // `(g and c) or (g and not c)` is `g`, exactly; otherwise `live`
                // would pull every split fork into the configuration product.
                let and_args = |g: &Graph<A>, n: NodeId| -> Option<[NodeId; 2]> {
                    let node = &g.nodes[n as usize];
                    (node.op == Op::And).then(|| [node.args[0], node.args[1]])
                };
                if let (Some(a), Some(b)) = (and_args(self, args[0]), and_args(self, args[1])) {
                    for i in 0..2 {
                        for j in 0..2 {
                            if a[i] == b[j] && self.complements(a[1 - i], b[1 - j]) {
                                return a[i];
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        self.add(op, args)
    }

    /// Is `y` the negation of `x` (`Not(x)` or the opposite comparison)?
    fn complements(&self, x: NodeId, y: NodeId) -> bool {
        let (nx, ny) = (&self.nodes[x as usize], &self.nodes[y as usize]);
        if (nx.op == Op::Not && nx.args[0] == y) || (ny.op == Op::Not && ny.args[0] == x) {
            return true;
        }
        let opposite = matches!(
            (&nx.op, &ny.op),
            (Op::Lt, Op::Ge) | (Op::Ge, Op::Lt) | (Op::Le, Op::Gt) | (Op::Gt, Op::Le)
        );
        opposite && nx.args == ny.args
    }

    /// Evaluate every node from the input cells in one forward pass. Exact or
    /// an error.
    pub fn eval(&self, cells: &HashMap<u32, Val<A>>) -> Result<Vec<Val<A>>> {
        self.eval_inner(cells, false, true, None)
    }

    /// `eval` with the map: `mget` and `tile_flag_at` of exact coordinates.
    pub fn eval_in(&self, cells: &HashMap<u32, Val<A>>, room: &Room) -> Result<Vec<Val<A>>> {
        self.eval_inner(cells, false, true, Some(room))
    }

    /// As `eval`, but an unmodelled node is TOP for its kind (sound) instead
    /// of an error. The same match, so the two cannot diverge.
    pub fn eval_lenient(&self, cells: &HashMap<u32, Val<A>>) -> Result<Vec<Val<A>>> {
        self.eval_inner(cells, true, false, None)
    }

    /// `eval_lenient` with the map, deciding `TileFlagAt`.
    pub fn eval_lenient_in(&self, cells: &HashMap<u32, Val<A>>, room: &Room) -> Result<Vec<Val<A>>> {
        self.eval_inner(cells, true, false, Some(room))
    }

    /// `eval_lenient_in` that also reads `mget` over INTERVAL coordinates as
    /// the SET of tiles the rectangle holds (`tiles_over`), so `Eq(k, mget)`
    /// decides where no tile there is `k`, and evaluates `DEFINED` (below).
    /// The interval fold's evaluator (`transpile::ival`); the tracer's
    /// (`split::Points`) is `eval_lenient_in`.
    pub fn eval_fold_in(&self, cells: &HashMap<u32, Val<A>>, room: &Room) -> Result<Vec<Val<A>>> {
        self.eval_full(cells, true, false, Some(room), true, DEFINED)
    }

    /// `eval_fold_in` without a room (no `mget`, `TileFlagAt` TOP).
    pub fn eval_fold(&self, cells: &HashMap<u32, Val<A>>) -> Result<Vec<Val<A>>> {
        self.eval_full(cells, true, false, None, false, DEFINED)
    }

    /// Forks resolved via `Frag`; an unmodelled node (`Mget`, roomless
    /// `TileFlagAt`) is TOP. `DEFINED`: level -1 takes the kernels'
    /// premises as holding.
    pub fn eval_narrow_top_in(&self, cells: &HashMap<u32, Val<A>>, room: &Room) -> Result<Vec<Val<A>>> {
        self.eval_full(cells, false, false, Some(room), false, DEFINED)
    }

    /// `eval_narrow_top_in` without a room, for bare arithmetic sub-DAGs.
    pub fn eval_narrow_top(&self, cells: &HashMap<u32, Val<A>>) -> Result<Vec<Val<A>>> {
        self.eval_full(cells, false, false, None, false, DEFINED)
    }

    fn eval_inner(
        &self,
        cells: &HashMap<u32, Val<A>>,
        frag_lenient: bool,
        strict_err: bool,
        room: Option<&Room>,
    ) -> Result<Vec<Val<A>>> {
        self.eval_full(cells, frag_lenient, strict_err, room, false, !DEFINED)
    }

    /// `defined` (`DEFINED`): an operation's value need cover only the lanes
    /// where it is DEFINED - `abs` of a hull holding MIN is `[0, MAX]`, the
    /// MIN lane being in error (`abs`'s own `NoWrap`). Sound where that
    /// error is charged to whatever reads the value: once the outcomes'
    /// errors are derived and are roots of the graph evaluated (the
    /// lowering's fold: every node it decides is in an outcome's cone, and
    /// so is the source's error), or where the premises are taken as holding
    /// (level -1). NOT in the tracer, which decides comparisons BEFORE the
    /// errors are derived: a decided one would no longer reach the `abs`, so
    /// nothing would charge its error.
    fn eval_full(
        &self,
        cells: &HashMap<u32, Val<A>>,
        frag_lenient: bool,
        strict_err: bool,
        room: Option<&Room>,
        mget_sets: bool,
        defined: bool,
    ) -> Result<Vec<Val<A>>> {
        let lenient = frag_lenient;
        // Per `Mget` node read as a set (`mget_sets`): the tiles its
        // coordinates' rectangle holds.
        let mut tiles: HashMap<NodeId, TileSet> = HashMap::new();
        let full = Val::Num(A::unknown());
        let mut out: Vec<Val<A>> = Vec::with_capacity(self.nodes.len());
        for (i, node) in self.nodes.iter().enumerate() {
            macro_rules! a {
                ($k:expr) => {
                    out[node.args[$k] as usize]
                };
            }
            let computed = (|| -> Result<Val<A>> {
                Ok(match &node.op {
                Op::Const(lo, hi) => Val::Num(A::ival(A::from_lit(*lo), A::from_lit(*hi))),
                Op::ConstBool(b) => Val::Bool(Some(*b)),
                // Nothing is known, in every lane.
                Op::UnknownBool(_) => Val::Bool(None),
                // No interval holds it: unmodelled (TOP when lenient).
                Op::UnknownNum => bail!("node {}: an unknown number has no interval", i),
                // No data output: its uses read the constant.
                Op::Pin(_) => bail!("node {}: a pin has no value (its uses read the constant)", i),
                // The hull (`fold`'s two-constant rule agrees). Tolerant of `lo > hi`.
                Op::Span => {
                    let (lo, hi) = (A::bounds(a!(0).as_num("Span")?).0, A::bounds(a!(1).as_num("Span")?).1);
                    Val::Num(if A::lt(hi, lo) { A::ival(hi, lo) } else { A::ival(lo, hi) })
                }
                // The operand inside the assumed range: a lane outside is in
                // error (the node's own), so the value need not cover it.
                // Disjoint, every lane is in error: the operand, as the kernel.
                Op::Restrict(lo, hi) => {
                    let x = a!(0).as_num("Restrict")?;
                    let (xl, xh) = A::bounds(x);
                    let (rl, rh) = (A::from_lit(*lo), A::from_lit(*hi));
                    let (lo, hi) = (if A::lt(xl, rl) { rl } else { xl }, if A::lt(rh, xh) { rh } else { xh });
                    Val::Num(if A::le(lo, hi) { A::ival(lo, hi) } else { x })
                }
                // A split RESTRICTS its operand: lenient, the operand covers it.
                Op::Split(d) if lenient => {
                    let _ = d;
                    a!(0)
                }
                Op::SplitInt(_) | Op::RSplit(_) if lenient => a!(0),
                Op::Split(d) | Op::SplitValid(d) | Op::SplitInt(d) | Op::RSplit(d) | Op::RSplitValid(d) => {
                    bail!("node {}: split {} has no value outside an outcome", i, d)
                }
                // `SplitOk`'s twin over the values of `flr(x + 1/2)`.
                Op::RSplitOk(ways) => {
                    let iv = a!(0).as_num("RSplitOk")?;
                    if round_count::<A>(iv).is_some_and(|k| k <= *ways as u32) {
                        Val::Bool(Some(true))
                    } else if !strict_err {
                        Val::Bool(None)
                    } else {
                        bail!("node {}: RSplitOk of an interval that may not fit", i)
                    }
                }
                // The RESOLVED round fork, as `Frag`/`FragOk` (`ev` below).
                Op::RFrag(c) | Op::RFragOk(c) => {
                    let iv = a!(0).as_num("RFrag")?;
                    // Values taken minus one (past 65536: every fragment).
                    let n = round_count::<A>(iv).map_or(i64::MAX, |k| k as i64 - 1);
                    let c = *c as i64;
                    if lenient {
                        return Ok(match (&node.op, c) {
                            (Op::RFrag(_), _) => a!(0),
                            (_, 0) => Val::Bool(Some(true)),
                            _ if n < c => Val::Bool(Some(false)),
                            _ => Val::Bool(None),
                        });
                    }
                    let valid = n >= c;
                    match &node.op {
                        Op::RFrag(_) if valid => Val::Num(A::round_fragment(iv, c as u32)?),
                        Op::RFrag(_) => a!(0),
                        _ => Val::Bool(Some(valid)),
                    }
                }
                // Bits: decided only on two points, or two values apart.
                Op::Same => {
                    let (x, y) = (a!(0).as_num("Same")?, a!(1).as_num("Same")?);
                    match (A::as_point(x), A::as_point(y)) {
                        (Some(p), Some(q)) => Val::Bool(Some(A::bits(p) == A::bits(q))),
                        _ => {
                            let ((xl, xh), (yl, yh)) = (A::bounds(x), A::bounds(y));
                            if A::lt(xh, yl) || A::lt(yh, xl) {
                                Val::Bool(Some(false))
                            } else {
                                Val::Bool(None)
                            }
                        }
                    }
                }
                // ONE lane's end, over the lanes: anywhere in the hull (except a
                // span's or a literal's). Never the hull's end as a point, or
                // the fold decides premises on it.
                Op::Lo | Op::Hi => {
                    let low = matches!(node.op, Op::Lo);
                    let arg = &self.nodes[node.args[0] as usize];
                    match arg.op {
                        Op::Span => Val::Num(out[arg.args[if low { 0 } else { 1 }] as usize].as_num("Lo/Hi of Span")?),
                        Op::Const(l, h) => Val::Num(A::point(A::from_lit(if low { l } else { h }))),
                        _ => Val::Num(a!(0).as_num("Lo/Hi")?),
                    }
                }
                // Fragment `c`'s low end, exact (lenient: its hull over lanes).
                Op::IntFrag(c) => Val::Num(A::ev_int_frag(a!(0).as_num("IntFrag")?, *c, lenient)),
                // `zi_span_ok`: high end's floor at most `ways - 1` cells above
                // the low end's. On a hull: fits => every lane fits, else unknown.
                Op::SplitOk(ways) => {
                    let iv = a!(0).as_num("SplitOk")?;
                    if A::ev_split_ok(iv, *ways) {
                        Val::Bool(Some(true))
                    } else if !strict_err {
                        Val::Bool(None)
                    } else {
                        bail!("node {}: SplitOk of an interval that may not fit", i)
                    }
                }
                // The operation's checked form (points and intervals alike).
                // On a hull: no wrap is no wrap on any lane (the operations
                // are monotone in each operand, or bilinear); a wrap is
                // unknown per lane.
                Op::NoWrap => {
                    let x = &self.nodes[node.args[0] as usize];
                    let arg = |k: usize| out[x.args[k] as usize].as_num("NoWrap");
                    let fits = match x.op {
                        Op::Add => A::ival_add(arg(0)?, arg(1)?).is_some(),
                        Op::Sub => A::ival_sub(arg(0)?, arg(1)?).is_some(),
                        Op::Mul => A::ival_mul(arg(0)?, arg(1)?).is_some(),
                        Op::Div => A::ival_div(arg(0)?, arg(1)?).is_some(),
                        Op::Neg => A::ival_neg(arg(0)?).is_some(),
                        Op::Abs => A::ival_abs(arg(0)?).is_some(),
                        // `%` (a double's own error, `GraphArith::range_error`):
                        // on points only.
                        Op::Rem => match (A::as_point(arg(0)?), A::as_point(arg(1)?)) {
                            (Some(p), Some(q)) => A::rem(p, q).is_some(),
                            _ => bail!("node {}: NoWrap of `%` over an interval is not modelled", i),
                        },
                        _ => true,
                    };
                    Val::Bool(if fits || !lenient { Some(fits) } else { None })
                }
                // The RESOLVED fork: `zi_fork_flr` on one interval.
                Op::Frag(c) | Op::FragOk(c) => {
                    let iv = a!(0).as_num("Frag")?;
                    // Cells spanned minus one.
                    let n = A::ev_frag_cells(iv);
                    let c = *c as i64;
                    // Lenient, the operand is a HULL over lanes: a lane in a wide
                    // hull may still span two floors, so `FragOk(c)` is decided
                    // only where the hull rules it out. The value is the hull.
                    if lenient {
                        return Ok(match (&node.op, c) {
                            (Op::Frag(_), _) => a!(0),
                            (_, 0) => Val::Bool(Some(true)),
                            // Too few cells: no lane reaches fragment `c`.
                            _ if n < c => Val::Bool(Some(false)),
                            _ => Val::Bool(None),
                        });
                    }
                    // Valid iff the interval reaches cell `c`; an invalid fragment
                    // keeps the whole value (never taken).
                    let valid = n >= c;
                    match &node.op {
                        Op::Frag(_) if valid => Val::Num(A::ev_frag(iv, c as u8)),
                        Op::Frag(_) => a!(0),
                        _ => Val::Bool(Some(valid)),
                    }
                }
                Op::Cell(c) => match cells.get(c) {
                    Some(v) => *v,
                    None => bail!("node {}: input cell {} was not supplied", i, c),
                },
                // CHECKED: inputs run at TOP on purpose (`transpile::ival`), so
                // a wrap is expected.
                Op::Add => Val::Num(A::ival_add(a!(0).as_num("Add")?, a!(1).as_num("Add")?).ok_or_else(|| anyhow!("node {}: Add wrapped", i))?),
                Op::Sub => Val::Num(A::ival_sub(a!(0).as_num("Sub")?, a!(1).as_num("Sub")?).ok_or_else(|| anyhow!("node {}: Sub wrapped", i))?),
                Op::Neg => Val::Num(A::ival_neg(a!(0).as_num("Neg")?).ok_or_else(|| anyhow!("node {}: Neg wrapped", i))?),
                Op::Mul | Op::Div | Op::Rem => Self::arith(&node.op, a!(0), a!(1))?,
                // `defined`: a lane where `abs` fails is in error (its own),
                // so the value need not cover it (`GraphArith::ev_abs`).
                Op::Abs => Val::Num(A::ev_abs(a!(0).as_num("Abs")?, defined).ok_or_else(|| anyhow!("node {}: Abs saturated", i))?),
                // flr is monotone, so endpoints suffice.
                Op::Flr => {
                    let (lo, hi) = A::bounds(a!(0).as_num("Flr")?);
                    Val::Num(A::ival(A::flr(lo), A::flr(hi)))
                }
                // sin is NOT monotone; only exact inputs are sound here.
                Op::Sin => match a!(0).as_exact() {
                    Some(n) => Val::exact_num(A::sin(n).ok_or_else(|| anyhow!("node {}: sin failed", i))?),
                    None => bail!("Sin over a non-exact interval is not modelled"),
                },
                // Monotone in each operand: the ends' (the arithmetic's own
                // `min`/`max`, its tie rule included).
                Op::Min | Op::Max => {
                    let ((xl, xh), (yl, yh)) = (A::bounds(a!(0).as_num("MinMax")?), A::bounds(a!(1).as_num("MinMax")?));
                    let pick = |p: A::Num, q: A::Num| if matches!(node.op, Op::Min) { A::min(p, q) } else { A::max(p, q) };
                    Val::Num(A::ival(pick(xl, yl), pick(xh, yh)))
                }
                // A tile against a literal, decided by the SET of tiles.
                Op::Eq if node.args.iter().any(|m| tiles.contains_key(m)) => {
                    let m = if tiles.contains_key(&node.args[0]) { 0 } else { 1 };
                    match a!(1 - m).as_exact() {
                        Some(k) => Val::Bool(tiles[&node.args[m]].eq::<A>(k)),
                        None => Self::compare(&node.op, a!(0), a!(1))?,
                    }
                }
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => {
                    Self::compare(&node.op, a!(0), a!(1))?
                }
                Op::Not => Val::Bool(a!(0).as_bool("Not")?.map(|b| !b)),
                Op::And => {
                    let (x, y) = (a!(0).as_bool("And")?, a!(1).as_bool("And")?);
                    // Short-circuit: `false AND unknown` is false.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(p && q),
                        (Some(false), None) | (None, Some(false)) => Some(false),
                        (Some(_), None) | (None, Some(_)) | (None, None) => None,
                    })
                }
                Op::Or => {
                    let (x, y) = (a!(0).as_bool("Or")?, a!(1).as_bool("Or")?);
                    // Short-circuit on a decided true, mirroring And.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(p || q),
                        (Some(true), None) | (None, Some(true)) => Some(true),
                        (Some(_), None) | (None, Some(_)) | (None, None) => None,
                    })
                }
                Op::Sel => match a!(0).as_bool("Sel")? {
                    Some(true) => a!(1),
                    Some(false) => a!(2),
                    // Undecided: the lane is in error (the select's own), so the
                    // join serves.
                    None => Self::join(a!(1), a!(2))?,
                },
                // Per-LANE decidedness. On a hull: decided means every lane is;
                // undecided says nothing (unknown, NOT false).
                Op::Known => {
                    let decided = match a!(0) {
                        Val::Bool(b) => b.is_some(),
                        Val::Num(i) => A::as_point(i).is_some(),
                    };
                    if decided {
                        Val::Bool(Some(true))
                    } else if lenient {
                        Val::Bool(None)
                    } else {
                        Val::Bool(Some(false))
                    }
                }
                Op::TileFlagAt if room.is_some() => {
                    Self::tile_flag_over(room.unwrap(), a!(0), a!(1), a!(2), a!(3), a!(4))?
                }
                // `mget` on EXACT coordinates, as in the kernels; over an
                // interval, the tiles the rectangle holds (`mget_sets`).
                Op::Mget if room.is_some() => {
                    let (x, y) = (a!(0).as_num("Mget")?, a!(1).as_num("Mget")?);
                    let (Some(xp), Some(yp)) = (A::as_point(x), A::as_point(y)) else {
                        if !mget_sets {
                            bail!("node {}: mget over an interval coordinate", i);
                        }
                        let set = TileSet::over::<A>(room.unwrap(), x, y);
                        tiles.insert(i as NodeId, set);
                        return Ok(set.hull());
                    };
                    let whole = |v: A::Num, what: &str| A::as_i16(v).ok_or_else(|| anyhow!("mget: {what} is not an integer"));
                    let t = room.unwrap().cart.mget_whole(whole(xp, "x")?, whole(yp, "y")?);
                    Val::exact_num(A::from_i16(t as i16))
                }
                Op::Mget | Op::TileFlagAt => {
                    bail!("{:?} needs the cart; not supported by the pure evaluator yet", node.op)
                }
                })
            })();
                // The ONE place `lenient` acts: TOP for its kind is sound
                // whatever the reason.
            let v = match computed {
                Ok(v) => v,
                Err(e) => {
                    if strict_err {
                        return Err(e);
                    }
                    Self::top_of(&node.op, &node.args, &out, full)
                }
            };
            out.push(v);
        }
        Ok(out)
    }

    /// The weakest value of this op: `Bool(None)` for booleans, the full
    /// range for numbers; `Sel` follows its arms.
    fn top_of(op: &Op<A>, args: &[NodeId], out: &[Val<A>], full: Val<A>) -> Val<A> {
        match op {
            Op::ConstBool(_)
            | Op::SplitValid(_)
            | Op::SplitOk(_)
            | Op::NoWrap
            | Op::FragOk(_)
            | Op::RSplitValid(_)
            | Op::RSplitOk(_)
            | Op::RFragOk(_)
            | Op::Same
            | Op::Lt
            | Op::Le
            | Op::Gt
            | Op::Ge
            | Op::Eq
            | Op::Not
            | Op::And
            | Op::Or
            | Op::Known
            | Op::UnknownBool(_)
            | Op::TileFlagAt => Val::Bool(None),
            Op::Sel => match args.get(1).map(|x| out[*x as usize]) {
                Some(Val::Bool(_)) => Val::Bool(None),
                _ => full,
            },
            _ => full,
        }
    }

    /// `tile_flag_at` over INTERVALS: FALSE if the UNION of the rectangles
    /// holds no flagged tile, TRUE if their INTERSECTION holds one, unknown
    /// between. Two `solid_at` calls on derived rectangles.
    fn tile_flag_over(room: &Room, x: Val<A>, y: Val<A>, w: Val<A>, h: Val<A>, flag: Val<A>) -> Result<Val<A>> {
        // The size and flag must be exact.
        let ex = |v: Val<A>, what: &str| -> Result<i32> {
            v.as_exact()
                .and_then(A::as_i16)
                .map(|n| n as i32)
                .ok_or_else(|| anyhow!("tile_flag_at: {} is not an exact integer", what))
        };
        let (w, h) = (ex(w, "w")?, ex(h, "h")?);
        let flag = ex(flag, "flag")? as i16;
        // Every integer from floor(low) to floor(high); non-integers fail
        // concretely, so covering them is conservative.
        let span = |v: Val<A>, what: &str| -> Result<(i32, i32)> {
            let (lo, hi) = A::bounds(v.as_num(what)?);
            let lo = A::as_i16(A::flr(lo)).ok_or_else(|| anyhow!("{}: unbounded", what))? as i32;
            let hi = A::as_i16(A::flr(hi)).ok_or_else(|| anyhow!("{}: unbounded", what))? as i32;
            Ok((lo, hi))
        };
        let (xlo, xhi) = span(x, "x")?;
        let (ylo, yhi) = span(y, "y")?;
        // Clamp the rectangle's EDGES to the 16-tile room, never corner and
        // size apart: at TOP that leaves only row 0, reading "nothing solid".
        let edges = |lo: i32, size: i32| -> (i16, i16) {
            let hi = lo + size - 1;
            let (lo, hi) = (lo.clamp(-4096, 4096), hi.clamp(-4096, 4096));
            (lo as i16, (hi - lo + 1) as i16)
        };
        let (dx, dy) = (xhi - xlo, yhi - ylo);
        let solid = |px: i32, py: i32, pw: i32, ph: i32| -> Result<bool> {
            let ((px, pw), (py, ph)) = (edges(px, pw), edges(py, ph));
            room.cache.flag_at(&room.cart, px, py, pw, ph, flag)
        };
        if !solid(xlo, ylo, w + dx, h + dy)? {
            return Ok(Val::Bool(Some(false)));
        }
        if dx < w && dy < h && solid(xhi, yhi, w - dx, h - dy)? {
            return Ok(Val::Bool(Some(true)));
        }
        Ok(Val::Bool(None))
    }

    fn arith(op: &Op<A>, x: Val<A>, y: Val<A>) -> Result<Val<A>> {
        // Exact op exact is exact, or an error where it leaves the range.
        if let (Some(p), Some(q)) = (x.as_exact(), y.as_exact()) {
            let r = match op {
                Op::Mul => A::mul(p, q),
                Op::Div => A::div(p, q),
                Op::Rem => A::rem(p, q),
                _ => unreachable!(),
            };
            return Ok(Val::exact_num(r.ok_or_else(|| anyhow!("{:?} of {:?} and {:?} wrapped", op, p, q))?));
        }
        let xi = x.as_num("arith")?;
        let scalar = match y.as_exact() {
            Some(q) => q,
            None => bail!("{:?} of two non-exact intervals is not modelled", op),
        };
        // The interval helpers are monotone only for a positive scalar.
        if A::le(scalar, A::from_i16(0)) {
            bail!("{:?} by a non-positive scalar is not modelled", op);
        }
        // Checked, as for `Add`: at TOP a scale wraps.
        let k = A::point(scalar);
        Ok(Val::Num(match op {
            Op::Mul => A::ival_mul(xi, k).ok_or_else(|| anyhow!("Mul over an interval wrapped"))?,
            Op::Div => A::ival_div(xi, k).ok_or_else(|| anyhow!("Div over an interval wrapped"))?,
            Op::Rem => bail!("Rem over an interval is not modelled"),
            _ => unreachable!(),
        }))
    }

    fn compare(op: &Op<A>, x: Val<A>, y: Val<A>) -> Result<Val<A>> {
        // Booleans only compare with Eq.
        if let (Val::Bool(p), Val::Bool(q)) = (x, y) {
            if !matches!(op, Op::Eq) {
                bail!("{:?} is not defined on booleans", op);
            }
            return Ok(Val::Bool(match (p, q) {
                (Some(a), Some(b)) => Some(a == b),
                _ => None,
            }));
        }
        let ((al, ah), (bl, bh)) = (A::bounds(x.as_num("compare")?), A::bounds(y.as_num("compare")?));
        // Decided iff the answer is the same for every pair in the boxes.
        let all = |f: &dyn Fn(A::Num, A::Num) -> bool| -> Option<bool> {
            let corners = [f(al, bl), f(al, bh), f(ah, bl), f(ah, bh)];
            // Order comparisons: corners bound the box; equality needs disjointness.
            if corners.iter().all(|c| *c) {
                Some(true)
            } else if corners.iter().all(|c| !*c) {
                Some(false)
            } else {
                None
            }
        };
        Ok(Val::Bool(match op {
            Op::Lt => all(&|p, q| A::lt(p, q)),
            Op::Le => all(&|p, q| A::le(p, q)),
            Op::Gt => all(&|p, q| A::lt(q, p)),
            Op::Ge => all(&|p, q| A::le(q, p)),
            Op::Eq => {
                if A::lt(bh, al) || A::lt(ah, bl) {
                    Some(false) // disjoint
                } else if let (Some(p), Some(q)) = (A::as_point(x.as_num("compare")?), A::as_point(y.as_num("compare")?)) {
                    Some(A::num_eq(p, q))
                } else {
                    None // overlapping, at least one inexact
                }
            }
            _ => unreachable!(),
        }))
    }

    fn join(x: Val<A>, y: Val<A>) -> Result<Val<A>> {
        Ok(match (x, y) {
            (Val::Num(a), Val::Num(b)) => {
                let ((al, ah), (bl, bh)) = (A::bounds(a), A::bounds(b));
                Val::Num(A::ival(A::min(al, bl), A::max(ah, bh)))
            }
            (Val::Bool(a), Val::Bool(b)) => Val::Bool(if a == b { a } else { None }),
            _ => bail!("cannot join a number with a boolean"),
        })
    }
}

/// The tiles `mget` can return over a rectangle of coordinates: one bit per
/// tile number (`mget` returns a byte).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct TileSet([u64; 4]);

impl TileSet {
    /// Every tile `mget(x, y)` returns for `x` in `xs`, `y` in `ys`. A lane's
    /// coordinate is an integer inside its hull (`mget` of a fraction raises,
    /// in the kernels too), so it is one of `flr(lo) ..= flr(hi)`; outside the
    /// map `mget` is 0, so the rectangle is clipped to the map plus one row of
    /// "outside" on each side.
    fn over<A: GraphArith>(room: &Room, xs: A::Ival, ys: A::Ival) -> TileSet {
        let ((x0, x1), (y0, y1)) = (A::tile_span(xs, 128), A::tile_span(ys, 64));
        let mut set = TileSet([0; 4]);
        for x in x0..=x1 {
            for y in y0..=y1 {
                let t = room.cart.mget_whole(x as i16, y as i16) as usize;
                set.0[t / 64] |= 1 << (t % 64);
            }
        }
        set
    }

    fn contains(&self, t: usize) -> bool {
        t < 256 && self.0[t / 64] & (1 << (t % 64)) != 0
    }

    /// The hull of the tiles, as a number interval.
    fn hull<A: Arithmetic>(&self) -> Val<A> {
        let mut it = (0..256).filter(|t| self.contains(*t));
        let lo = it.next().expect("a rectangle holds a tile") as i16;
        let hi = it.last().map_or(lo, |t| t as i16);
        Val::Num(A::ival(A::from_i16(lo), A::from_i16(hi)))
    }

    /// `mget == k` over the set: false if no tile is `k`, true if every one is.
    fn eq<A: Arithmetic>(&self, k: A::Num) -> Option<bool> {
        let t = A::as_i16(k).filter(|t| (0..256).contains(t)).map(|t| t as usize);
        match t {
            Some(t) if self.contains(t) => (self.0.iter().map(|w| w.count_ones()).sum::<u32>() == 1).then_some(true),
            _ => Some(false),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::Pico8NumInterval;

    fn pieces_of(g: &Graph, memo: &mut HashMap<NodeId, Option<crate::transpile::garith::Pieces>>, n: NodeId) -> Option<crate::transpile::garith::Pieces> {
        <Fixed as GraphArith>::pieces_of(g, memo, n)
    }

    fn n(v: i16) -> Pico8Num {
        Pico8Num::from_i16(v)
    }

    #[test]
    fn interning_shares_identical_nodes() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let s1 = g.add(Op::Add, vec![a, b]);
        let s2 = g.add(Op::Add, vec![a, b]);
        assert_eq!(s1, s2, "identical op over identical operands is one node");
        assert_eq!(g.len(), 3);
        // `add` does not normalize (`fold` does).
        let s3 = g.add(Op::Add, vec![b, a]);
        assert_ne!(s1, s3);
    }

    #[test]
    fn evaluates_exact_arithmetic() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let k = g.leaf(Op::Const(n(3).as_raw_u32() as i32, n(3).as_raw_u32() as i32));
        let sum = g.add(Op::Add, vec![a, k]);
        let cells = HashMap::from([(1u32, Val::exact_num(n(4)))]);
        let out = g.eval(&cells).unwrap();
        assert_eq!(out[sum as usize], Val::exact_num(n(7)));
    }

    #[test]
    fn comparison_of_overlapping_intervals_is_unknown() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let lt = g.add(Op::Lt, vec![a, b]);
        // [0,10] < [5,15] is neither always true nor always false.
        let cells = HashMap::from([
            (1u32, Val::Num(Pico8NumInterval::new(n(0), n(10)))),
            (2u32, Val::Num(Pico8NumInterval::new(n(5), n(15)))),
        ]);
        assert_eq!(g.eval(&cells).unwrap()[lt as usize], Val::Bool(None));
        // Disjoint boxes decide it.
        let cells = HashMap::from([
            (1u32, Val::Num(Pico8NumInterval::new(n(0), n(3)))),
            (2u32, Val::Num(Pico8NumInterval::new(n(5), n(15)))),
        ]);
        assert_eq!(g.eval(&cells).unwrap()[lt as usize], Val::Bool(Some(true)));
    }

    #[test]
    fn validity_is_an_ordinary_node() {
        // Validity is an ordinary node: a select is valid only where `known(c)`.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let t = g.leaf(Op::Const(n(1).as_raw_u32() as i32, n(1).as_raw_u32() as i32));
        let f = g.leaf(Op::Const(n(2).as_raw_u32() as i32, n(2).as_raw_u32() as i32));
        let sel = g.add(Op::Sel, vec![c, t, f]);
        let known = g.add(Op::Known, vec![c]);
        let all = g.leaf(Op::ConstBool(true));
        let valid = g.add(Op::And, vec![all, known]);

        // Decided condition: the lane is valid and picks an arm.
        let cells = HashMap::from([(1u32, Val::Bool(Some(true)))]);
        let out = g.eval(&cells).unwrap();
        assert_eq!(out[valid as usize], Val::Bool(Some(true)));
        assert_eq!(out[sel as usize], Val::exact_num(n(1)));

        // Undecided: the lane is INVALID, and the value is the sound join.
        let cells = HashMap::from([(1u32, Val::Bool(None))]);
        let out = g.eval(&cells).unwrap();
        assert_eq!(out[valid as usize], Val::Bool(Some(false)));
        assert_eq!(
            out[sel as usize],
            Val::Num(Pico8NumInterval::new(n(1), n(2)))
        );
    }

    #[test]
    fn and_short_circuits_through_unknown() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let and = g.add(Op::And, vec![a, b]);
        // false AND unknown is FALSE, not unknown.
        let cells = HashMap::from([(1u32, Val::Bool(Some(false))), (2u32, Val::Bool(None))]);
        assert_eq!(g.eval(&cells).unwrap()[and as usize], Val::Bool(Some(false)));
    }

    /// `SplitOk(n)` over a literal folds to what `zi_span_ok` computes.
    #[test]
    fn split_ok_of_a_literal_is_what_zi_span_ok_says() {
        use celeste_engine::kernel::{zi_span_ok, zn_splat, ALL, ZI};
        let raws = [i32::MIN, -0x2_8000, -0x1_0000, -1, 0, 1, 0x8000, 0xffff, 0x1_0000, 0x1_0001, 0x2_0000, 0x3_7fff, i32::MAX - 0x1_0000, i32::MAX];
        let mut g = Graph::new();
        for &lo in &raws {
            for &hi in raws.iter().filter(|h| **h >= lo) {
                let c = g.leaf(Op::Const(lo, hi));
                for ways in 1..=4u8 {
                    let got = g.fold(Op::SplitOk(ways), vec![c]);
                    let iv = ZI { lo: zn_splat(Pico8Num::from_raw(lo)), hi: zn_splat(Pico8Num::from_raw(hi)) };
                    let want = zi_span_ok(iv, ways).val == ALL;
                    assert_eq!(g.get(got).op, Op::ConstBool(want), "ways {ways} [{lo:#x}, {hi:#x}]");
                }
            }
        }
    }

    #[test]
    fn folding_is_exact_not_merely_sound() {
        // Every `fold` rule must be EXACT: `fold` and `add` evaluate alike at
        // every leaf assignment (hence no `x and not x -> false`).
        let mut g = Graph::new();
        let b: Vec<NodeId> = (0..3).map(|i| g.leaf(Op::Cell(i))).collect();
        let nums: Vec<NodeId> = (10..12).map(|i| g.leaf(Op::Cell(i))).collect();
        let tt = g.leaf(Op::ConstBool(true));
        let ff = g.leaf(Op::ConstBool(false));

        // Boolean subexpressions built unfolded, for the rules to bite on.
        let not0 = g.add(Op::Not, vec![b[0]]);
        let nn1 = g.add(Op::Not, vec![b[1]]);
        let notnot1 = g.add(Op::Not, vec![nn1]);
        let lt = g.add(Op::Lt, vec![nums[0], nums[1]]);
        let ge = g.add(Op::Ge, vec![nums[0], nums[1]]);
        let and01 = g.add(Op::And, vec![b[0], b[1]]);
        let kn = g.add(Op::Known, vec![b[2]]);
        let bools = vec![b[0], b[1], b[2], tt, ff, not0, notnot1, lt, ge, and01, kn];
        let numexprs = vec![
            nums[0],
            nums[1],
            g.add(Op::Add, vec![nums[0], nums[1]]),
            g.leaf(Op::Const(n(3).as_raw_u32() as i32, n(3).as_raw_u32() as i32)),
        ];

        // (folded, unfolded) pairs to compare.
        let mut pairs: Vec<(NodeId, NodeId)> = Vec::new();
        let both = |g: &mut Graph, op: Op, args: Vec<NodeId>| {
            let f = g.fold(op.clone(), args.clone());
            let p = g.add(op, args);
            (f, p)
        };
        for x in bools.clone() {
            for op in [Op::Not, Op::Known] {
                pairs.push(both(&mut g, op, vec![x]));
            }
            for y in bools.clone() {
                for op in [Op::And, Op::Or, Op::Eq] {
                    pairs.push(both(&mut g, op, vec![x, y]));
                }
                for z in bools.clone() {
                    pairs.push(both(&mut g, Op::Sel, vec![x, y, z]));
                }
            }
        }
        for c in bools.clone() {
            for x in numexprs.clone() {
                for y in numexprs.clone() {
                    pairs.push(both(&mut g, Op::Sel, vec![c, x, y]));
                    for op in [Op::Add, Op::Min, Op::Max, Op::Lt, Op::Le, Op::Gt, Op::Ge, Op::Eq] {
                        pairs.push(both(&mut g, op, vec![x, y]));
                    }
                }
            }
        }

        let tri = [Some(true), Some(false), None];
        let ivals = [
            Pico8NumInterval::from_number(n(0)),
            Pico8NumInterval::from_number(n(1)),
            Pico8NumInterval::new(n(0), n(2)),
        ];
        let mut checked = 0usize;
        for p0 in tri {
            for p1 in tri {
                for p2 in tri {
                    for i0 in ivals {
                        for i1 in ivals {
                            let cells = HashMap::from([
                                (0u32, Val::Bool(p0)),
                                (1u32, Val::Bool(p1)),
                                (2u32, Val::Bool(p2)),
                                (10u32, Val::Num(i0)),
                                (11u32, Val::Num(i1)),
                            ]);
                            let out = g.eval(&cells).expect("every op in the pool evaluates");
                            for (f, p) in &pairs {
                                assert_eq!(
                                    out[*f as usize],
                                    out[*p as usize],
                                    "fold changed the value of {:?} at {:?}",
                                    g.get(*p),
                                    cells
                                );
                                checked += 1;
                            }
                        }
                    }
                }
            }
        }
        assert!(pairs.len() > 1_000, "pool collapsed to {} pairs", pairs.len());
        let fired = pairs.iter().filter(|(f, p)| f != p).count();
        assert!(fired > 500, "only {} of {} pairs were folded", fired, pairs.len());
        eprintln!(
            "[fold] {} pairs, {} folded, {} value comparisons",
            pairs.len(),
            fired,
            checked
        );
    }

    #[test]
    fn commutative_operands_are_put_in_a_canonical_order() {
        // `add` stays the raw constructor (the control above).
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        for op in [Op::Add, Op::Min, Op::Max, Op::Eq, Op::And, Op::Or] {
            assert_eq!(
                g.fold(op.clone(), vec![a, b]),
                g.fold(op.clone(), vec![b, a]),
                "{:?} did not normalize its operand order",
                op
            );
        }
        // Subtraction does not commute, and must not be normalized.
        assert_ne!(g.fold(Op::Sub, vec![a, b]), g.fold(Op::Sub, vec![b, a]));
        // `Mul` commutes but `eval` is monotone only with the exact side
        // second: a constant moves right.
        let k = g.leaf(Op::Const(0, 0));
        assert_eq!(g.fold(Op::Mul, vec![k, a]), g.fold(Op::Mul, vec![a, k]));
        assert_ne!(g.fold(Op::Mul, vec![a, b]), g.fold(Op::Mul, vec![b, a]));
    }

    #[test]
    fn a_select_between_boolean_constants_is_boolean_algebra() {
        // `if c then true else x` is `c or x`.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let x = g.leaf(Op::Cell(2));
        let tt = g.leaf(Op::ConstBool(true));
        let ff = g.leaf(Op::ConstBool(false));
        assert_eq!(g.fold(Op::Sel, vec![c, tt, ff]), c, "select of true/false is the condition");
        assert_eq!(
            g.fold(Op::Sel, vec![c, ff, tt]),
            g.fold(Op::Not, vec![c]),
            "select of false/true is its negation"
        );
        assert_eq!(g.fold(Op::Sel, vec![c, tt, x]), g.fold(Op::Or, vec![c, x]));
        assert_eq!(g.fold(Op::Sel, vec![c, x, ff]), g.fold(Op::And, vec![c, x]));
        let nc = g.fold(Op::Not, vec![c]);
        assert_eq!(g.fold(Op::Sel, vec![c, ff, x]), g.fold(Op::And, vec![nc, x]));
        assert_eq!(g.fold(Op::Sel, vec![c, x, tt]), g.fold(Op::Or, vec![nc, x]));
    }

    #[test]
    fn a_split_guard_merged_back_is_the_guard() {
        // `(g and c) or (g and not c)` is `g` and must not read `c`.
        let mut g = Graph::new();
        let guard = g.leaf(Op::Cell(1));
        let c = g.leaf(Op::Cell(2));
        let nc = g.fold(Op::Not, vec![c]);
        let t = g.fold(Op::And, vec![guard, c]);
        let f = g.fold(Op::And, vec![nc, guard]);
        assert_eq!(g.fold(Op::Or, vec![t, f]), guard);
        // The negation of a comparison folds into the opposite comparison.
        let (a, b) = (g.leaf(Op::Cell(3)), g.leaf(Op::Cell(4)));
        let gt = g.fold(Op::Gt, vec![a, b]);
        let le = g.fold(Op::Not, vec![gt]);
        let t = g.fold(Op::And, vec![guard, gt]);
        let f = g.fold(Op::And, vec![guard, le]);
        assert_eq!(g.fold(Op::Or, vec![t, f]), guard);
        // Two different conditions are not complements.
        let d = g.leaf(Op::Cell(5));
        let f = g.fold(Op::And, vec![guard, d]);
        assert_ne!(g.fold(Op::Or, vec![t, f]), guard);
    }

    #[test]
    fn a_negated_comparison_is_the_opposite_comparison() {
        // Makes `x < y` and `not (x >= y)` the SAME node.
        let mut g = Graph::new();
        let (x, y) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)));
        for (op, opp) in [
            (Op::Lt, Op::Ge),
            (Op::Le, Op::Gt),
            (Op::Gt, Op::Le),
            (Op::Ge, Op::Lt),
        ] {
            let a = g.fold(op.clone(), vec![x, y]);
            assert_eq!(g.fold(Op::Not, vec![a]), g.fold(opp.clone(), vec![x, y]));
        }
        // Equality has no negation in the vocabulary, so it keeps its Not.
        let e = g.fold(Op::Eq, vec![x, y]);
        let ne = g.fold(Op::Not, vec![e]);
        assert_eq!(g.get(ne).op, Op::Not);
        // ... and double negation is the identity.
        assert_eq!(g.fold(Op::Not, vec![ne]), e);
    }

    #[test]
    fn a_select_between_equal_arms_is_not_a_select() {
        // A join proposes a select per cell, almost all untouched: free.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let x = g.leaf(Op::Cell(2));
        assert_eq!(g.fold(Op::Sel, vec![c, x, x]), x);
        // A genuine difference still costs a node.
        let y = g.leaf(Op::Cell(3));
        let sel = g.fold(Op::Sel, vec![c, x, y]);
        assert_ne!(sel, x);
        assert_ne!(sel, y);
        // And a decided condition still picks its arm.
        let t = g.leaf(Op::ConstBool(true));
        assert_eq!(g.fold(Op::Sel, vec![t, x, y]), x);
    }

    #[test]
    fn specializing_the_button_forks_collapses_the_configurations_that_agree() {
        // `input` is Sel(right, 1, Sel(left, -1, 0)) over two buttons: with
        // right = true, {left,right} and {right} share a node.
        let mut d = crate::trace::domain::Symbolic::default();
        let left = d.both_values("left");
        let right = d.both_values("right");
        // Its coverage premise (`trace::error`) holds on every lane.
        let fork = d.graph.get(left).args[0];
        assert!(d.graph.is_literal_fork(fork), "a button is a fork over a literal");
        let literal = d.graph.get(fork).args[0];
        let covered = d.graph.fold(Op::SplitOk(2), vec![literal]);
        assert_eq!(d.graph.get(covered).op, Op::ConstBool(true), "the literal fits two ways");
        let g = &mut d.graph;
        let one = g.leaf(Op::Const(n(1).as_raw_u32() as i32, n(1).as_raw_u32() as i32));
        let neg = g.leaf(Op::Const(n(-1).as_raw_u32() as i32, n(-1).as_raw_u32() as i32));
        let zero = g.leaf(Op::Const(0, 0));
        let inner = g.add(Op::Sel, vec![left, neg, zero]);
        let input = g.add(Op::Sel, vec![right, one, inner]);

        let mut shared = Graph::new();
        let sig = |l: u8, r: u8, shared: &mut Graph| {
            let map = d.graph.specialize_subset_into(&[l, r], None, shared);
            (map[input as usize], map[left as usize], map[right as usize])
        };
        let r_only = sig(0, 1, &mut shared);
        let both = sig(1, 1, &mut shared);
        assert_eq!(r_only.0, both.0, "right dominates left; these are one configuration");
        let l_only = sig(1, 0, &mut shared);
        let none = sig(0, 0, &mut shared);
        assert_ne!(l_only.0, none.0);
        assert_ne!(l_only.0, both.0);
        // Each configuration resolves a button to a constant.
        for (cfg, want) in [(none, (false, false)), (l_only, (true, false)), (both, (true, true))] {
            assert_eq!(shared.get(cfg.1).op, Op::ConstBool(want.0), "left");
            assert_eq!(shared.get(cfg.2).op, Op::ConstBool(want.1), "right");
        }
    }

    #[test]
    fn unmodelled_operations_are_loud() {
        // An op we cannot evaluate EXACTLY must error, never approximate.
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let m = g.add(Op::Mul, vec![a, b]);
        let _ = m;
        let cells = HashMap::from([
            (1u32, Val::Num(Pico8NumInterval::new(n(0), n(10)))),
            (2u32, Val::Num(Pico8NumInterval::new(n(2), n(3)))),
        ]);
        let err = g.eval(&cells).unwrap_err().to_string();
        assert!(err.contains("not modelled"), "unexpected error: {}", err);
    }

    /// `NoWrap` of an operation with one operand not a literal point
    /// (`one_operand`), every form, literals of both signs: `fits` is
    /// exactly where the checked operation has an answer (its ends fit, one
    /// past them does not), `fold` decides it where every operand fits or
    /// none does, and `eval` answers it on an interval with the checked
    /// operation (a fit decided, one past it unknown per lane).
    #[test]
    fn one_operand_no_wrap_fits_exactly_where_the_checked_operation_does() {
        let p = Pico8Num::from_raw;
        let ks = [0, 1, -1, 0x8000, -0x8000, 0xffff, 0x1_0000, -0x1_0000, 0x1_0001, -0x1_0001, 0x2_0000, -0x2_0000, 0x7fff_0000, i32::MAX, i32::MIN];
        // (op, the literal first)
        let forms = [(Op::Add, false), (Op::Add, true), (Op::Sub, false), (Op::Sub, true), (Op::Mul, false), (Op::Mul, true), (Op::Div, false)];
        let mut checked_forms = 0;
        for (op, first) in forms {
            for k in ks {
                let mut g = Graph::new();
                let x = g.leaf(Op::Cell(0));
                let c = g.leaf(Op::Const(k, k));
                let m = g.add(op.clone(), if first { vec![c, x] } else { vec![x, c] });
                let o = one_operand(&g, m).expect("one operand");
                assert_eq!(o.operand, first as usize, "{op:?} {k:#x}");
                let exact = |v: i32| {
                    let (a, b) = if first { (p(k), p(v)) } else { (p(v), p(k)) };
                    match op {
                        Op::Add => a.checked_add(b),
                        Op::Sub => a.checked_sub(b),
                        Op::Mul => a.checked_mul(b),
                        _ => a.checked_div(b),
                    }
                    .is_some()
                };
                let nw = g.fold(Op::NoWrap, vec![m]);
                let Some((lo, hi)) = o.fits else {
                    assert!(op == Op::Div && k == 0, "{op:?} {k:#x}: only `x / 0` fits nowhere");
                    assert!([i32::MIN, -1, 0, 1, i32::MAX].iter().all(|v| !exact(*v)));
                    assert_eq!(g.get(nw).op, Op::ConstBool(false));
                    continue;
                };
                assert!(exact(lo) && exact(hi), "{op:?} {k:#x}: the ends fit");
                assert!(lo == i32::MIN || !exact(lo - 1), "{op:?} {k:#x}: below {lo:#x}");
                assert!(hi == i32::MAX || !exact(hi + 1), "{op:?} {k:#x}: above {hi:#x}");
                let everywhere = (lo, hi) == (i32::MIN, i32::MAX);
                assert_eq!(g.get(nw).op == Op::ConstBool(true), everywhere, "{op:?} {k:#x}: decided iff every value fits");
                let at = |a: i32, b: i32| {
                    let cells = HashMap::from([(0u32, Val::Num(Pico8NumInterval::new(p(a), p(b))))]);
                    g.eval_lenient(&cells).expect("lenient")[nw as usize]
                };
                assert_eq!(at(lo, hi), Val::Bool(Some(true)), "{op:?} {k:#x}: the whole fitting range");
                if lo > i32::MIN {
                    assert_eq!(at(lo - 1, hi), Val::Bool(None), "{op:?} {k:#x}: one past the low end");
                }
                if hi < i32::MAX {
                    assert_eq!(at(lo, hi + 1), Val::Bool(None), "{op:?} {k:#x}: one past the high end");
                    // A point past it: the operation itself is the error.
                    let cells = HashMap::from([(0u32, Val::exact_num(p(hi + 1)))]);
                    assert!(g.eval(&cells).unwrap_err().to_string().contains("wrapped"), "{op:?} {k:#x}");
                }
                checked_forms += !everywhere as usize;
            }
        }
        assert!(checked_forms > 40, "{checked_forms} forms that can overflow");
        // Negation and `abs`: everything but MIN.
        for op in [Op::Neg, Op::Abs] {
            let mut g = Graph::new();
            let x = g.leaf(Op::Cell(0));
            let m = g.add(op.clone(), vec![x]);
            assert_eq!(one_operand(&g, m), Some(OneOperand { operand: 0, fits: Some((i32::MIN + 1, i32::MAX)) }));
        }
        // `k / x` is no `OneOperand`, nor is an operation of two cells.
        let mut g = Graph::new();
        let (x, y, c) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)), g.leaf(Op::Const(0x1_0000, 0x1_0000)));
        for (op, args) in [(Op::Div, vec![c, x]), (Op::Add, vec![x, y]), (Op::Mul, vec![x, y])] {
            let m = g.add(op, args);
            assert_eq!(one_operand(&g, m), None);
            let nw = g.fold(Op::NoWrap, vec![m]);
            assert_eq!(g.get(nw).op, Op::NoWrap, "stays for the lanes");
        }
    }

    /// Literal POINT arithmetic folds exactly; a result past the 16.16
    /// range (which PICO-8 wraps or saturates) is an error, never a wrapped
    /// constant: `fold` panics with the operation and its operands.
    #[test]
    fn a_literal_fold_folds_exactly() {
        let mut g = Graph::new();
        let k = |g: &mut Graph, v: i32| g.leaf(Op::Const(v, v));
        let (a, b) = (k(&mut g, 3 << 16), k(&mut g, -(5 << 16)));
        for (op, want) in [(Op::Add, -2 << 16), (Op::Sub, 8 << 16), (Op::Mul, -15 << 16), (Op::Div, -0x9999)] {
            let r = g.fold(op.clone(), vec![a, b]);
            assert_eq!(g.get(r).op, Op::Const(want, want), "{op:?}");
        }
        // One step inside the edges is exact.
        let (max, one) = (k(&mut g, i32::MAX - 1), k(&mut g, 1));
        let r = g.fold(Op::Add, vec![max, one]);
        assert_eq!(g.get(r).op, Op::Const(i32::MAX, i32::MAX));
        let min = k(&mut g, i32::MIN + 1);
        let r = g.fold(Op::Neg, vec![min]);
        assert_eq!(g.get(r).op, Op::Const(i32::MAX, i32::MAX));
    }

    #[test]
    #[should_panic(expected = "constant fold overflow: + of the literals 32767.99998")]
    fn a_literal_fold_that_overflows_is_an_error() {
        let mut g = Graph::new();
        let (max, eps) = (g.leaf(Op::Const(i32::MAX, i32::MAX)), g.leaf(Op::Const(1, 1)));
        g.fold(Op::Add, vec![max, eps]);
    }

    #[test]
    #[should_panic(expected = "constant fold overflow: / of the literals 1 and 0 leaves the 16.16 range")]
    fn a_literal_division_by_zero_is_an_error() {
        let mut g = Graph::new();
        let (one, zero) = (g.leaf(Op::Const(1 << 16, 1 << 16)), g.leaf(Op::Const(0, 0)));
        g.fold(Op::Div, vec![one, zero]);
    }

    #[test]
    #[should_panic(expected = "constant fold overflow: abs of the literals -32768 and -32768")]
    fn a_literal_abs_of_min_is_an_error() {
        let mut g = Graph::new();
        let min = g.leaf(Op::Const(i32::MIN, i32::MIN));
        g.fold(Op::Abs, vec![min]);
    }

    /// A product's range is computed unwrapped: one past the 16.16 range is
    /// unknown, not PICO-8's wrapped endpoints (which bound nothing).
    #[test]
    fn a_product_range_that_overflows_is_unknown() {
        let mut g = Graph::new();
        let raw = g.leaf(Op::Cell(0));
        let s = g.add(Op::Restrict(0, i32::MAX), vec![raw]);
        let two = g.leaf(Op::Const(0x2_0000, 0x2_0000));
        let m = g.add(Op::Mul, vec![s, two]);
        assert_eq!(pieces_of(&g, &mut HashMap::new(), m), None);
        let t = g.add(Op::Restrict(0, 0x3fff_ffff), vec![raw]);
        let m = g.add(Op::Mul, vec![t, two]);
        assert_eq!(pieces_of(&g, &mut HashMap::new(), m), Some(vec![(0, 0x7fff_fffe)]));
    }

    /// A select on a non-constant condition is the UNION of its arms.
    #[test]
    fn pieces_keep_a_select_as_a_union() {
        let mut g = Graph::new();
        let raw = g.leaf(Op::Cell(0));
        let s = g.add(Op::Restrict(-(1 << 16), 1 << 16), vec![raw]);
        let five = g.leaf(Op::Const(5 << 16, 5 << 16));
        let cond = g.leaf(Op::Cell(1));
        let sel = g.add(Op::Sel, vec![cond, five, s]);
        let neg = g.add(Op::Neg, vec![sel]);
        let mut memo = HashMap::new();
        assert_eq!(pieces_of(&g, &mut memo, sel), Some(vec![(-(1 << 16), 1 << 16), (5 << 16, 5 << 16)]));
        assert_eq!(pieces_of(&g, &mut memo, neg), Some(vec![(-(5 << 16), -(5 << 16)), (-(1 << 16), 1 << 16)]));
        // A cell is unknown, and so is anything over it.
        let other = g.leaf(Op::Cell(2));
        let sum = g.add(Op::Add, vec![s, other]);
        assert_eq!(pieces_of(&g, &mut memo, sum), None);
        assert_eq!(pieces_of(&g, &mut memo, raw), None);
    }

    /// `Lo`/`Hi` of a hull are not points, so nothing may decide a comparison
    /// one lane could fail.
    #[test]
    fn the_ends_of_a_hull_are_not_points() {
        let mut g = Graph::new();
        let v = g.leaf(Op::Cell(0));
        let hi = g.add(Op::Hi, vec![v]);
        let one = g.leaf(Op::Const(1 << 16, 1 << 16));
        let le = g.add(Op::Le, vec![hi, one]);
        let hull = Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(0), Pico8Num::from_raw(3 << 16)));
        let vals = g.eval_lenient(&HashMap::from([(0u32, hull)])).expect("eval");
        assert_eq!(vals[hi as usize], hull, "Hi of a hull is the hull");
        assert_eq!(vals[le as usize], Val::Bool(None), "a lane [0, 0.5] has Hi <= 1, a lane [2, 3] does not");
        let r = g.add(Op::Restrict(0, 3 << 16), vec![v]);
        let hi_r = g.add(Op::Hi, vec![r]);
        assert_eq!(pieces_of(&g, &mut HashMap::new(), hi_r), Some(vec![(0, 3 << 16)]));
        let le_r = g.add(Op::Le, vec![hi_r, one]);
        let (out, map, _) = crate::transpile::ival::fold(&g, &[le_r], None).expect("fold");
        assert_eq!(out.get(map[le_r as usize]).op, Op::Le, "the fold leaves it to the lanes");
    }
}

/// How many values `flr(x + 1/2)` takes over `iv` (`None`: past 65536).
fn round_count<A: Arithmetic>(iv: A::Ival) -> Option<u32> {
    A::rounds(iv).ok().flatten()
}

/// `Graph::fold`'s literal arithmetic: the exact result, or a panic naming
/// the operation (`fold` cannot return an error; see there).
fn checked<A: Arithmetic>(op: &str, r: Option<A::Num>, x: A::Num, y: A::Num) -> A::Num {
    r.unwrap_or_else(|| {
        panic!(
            "constant fold overflow: {op} of the literals {} and {} leaves {} (overflow is an error)",
            A::show(x),
            A::show(y),
            A::RANGE_NOTE
        )
    })
}

/// A numeric operation whose operands are literal POINTS but one, `operand`
/// (an argument index): `x + k`, `k + x`, `x - k`, `k - x`, `-x`, `abs(x)`,
/// `x * k`, `k * x`, `x / k`. Its exact result fits the 16.16 range iff the
/// operand lies in `fits` (raw, inclusive; `None`: nowhere, `x / 0`): each
/// map is monotone in `x`, or (`abs`) grows with `|x|`, so the set that fits
/// is an interval, and an interval operand fits iff both its ends do. So
/// `Op::NoWrap` of one is at most two compares (the codegen's), none where
/// `fits` is the whole range (`Graph::fold` decides those). Not `k / x`:
/// the divisors that fit are no interval (none is near 0).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OneOperand {
    pub operand: usize,
    pub fits: Option<(i32, i32)>,
}

/// `x` as a `OneOperand`, if it is one (the first operand where both are
/// literal points).
pub fn one_operand(g: &Graph<Fixed>, x: NodeId) -> Option<OneOperand> {
    let node = g.get(x);
    let point = |k: usize| match g.get(node.args[k]).op {
        Op::Const(lo, hi) if lo == hi => Some(lo as i64),
        _ => None,
    };
    let (min, max) = (i32::MIN as i64, i32::MAX as i64);
    // The operand's values `[lo, hi]` inside the range: never empty below.
    let clip = |lo: i64, hi: i64| Some((lo.max(min) as i32, hi.min(max) as i32));
    let p8 = |k: i64| Pico8Num::from_raw(k as i32);
    let raw = |(lo, hi): (Pico8Num, Pico8Num)| (lo.as_raw_u32() as i32, hi.as_raw_u32() as i32);
    let (operand, fits) = match node.op {
        Op::Neg | Op::Abs => (0, Some((i32::MIN + 1, i32::MAX))),
        Op::Add => match (point(0), point(1)) {
            (_, Some(k)) => (0, clip(min - k, max - k)),
            (Some(k), None) => (1, clip(min - k, max - k)),
            _ => return None,
        },
        Op::Sub => match (point(0), point(1)) {
            (_, Some(k)) => (0, clip(min + k, max + k)),
            (Some(k), None) => (1, clip(k - max, k - min)),
            _ => return None,
        },
        Op::Mul => match (point(0), point(1)) {
            (_, Some(k)) => (0, Some(raw(Pico8Num::mul_fits(p8(k))))),
            (Some(k), None) => (1, Some(raw(Pico8Num::mul_fits(p8(k))))),
            _ => return None,
        },
        Op::Div => (0, Pico8Num::div_fits(p8(point(1)?)).map(raw)),
        _ => return None,
    };
    Some(OneOperand { operand, fits })
}

