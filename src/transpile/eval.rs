//! THE ONE ABSTRACT EVALUATION of the graph IR: per node what is KNOWN of
//! its value (`Known`: an interval for a number, or nothing; three values for
//! a boolean; the tiles an `mget` may read) from what is known of its inputs,
//! one rule per operator (`rule`). Every decision and fold made on a graph
//! reads it:
//!
//! - the TRACER's static ranges (`Symbolic::range_of`, `Facts::default`:
//!   over the lanes, nothing known of an input but its restriction, no
//!   map): the comparisons they decide, the overflow they rule out
//!   (`trace::error`), a fork's arity, the walk's regions, a widening's
//!   containment (`within`, under a select's branch facts);
//! - `Graph::fold` and the tracer on LITERAL operands (`literal`: every
//!   lane holds a literal; an operand not one is any number);
//! - the LOWERING's fold (`fold`: over the lanes, `mget` over intervals read
//!   as the set of tiles the rectangle holds);
//! - ONE LANE (`eval` with `Lanes::One`: a row, as level -1 and `explain`
//!   read it).
//!
//! **What a value covers**: what the CONSOLE computes, on every lane the
//! inputs stand for (16.16 wraps a sum that leaves the range, and
//! `abs(-32768)` saturates to the greatest number, so `abs` of a hull
//! reaching it is the hull's above it), so a decision holds on every lane
//! whatever its errors - the tracer decides before they are derived. What
//! no interval is known to hold (a double that may not be finite, an
//! operation the rules do not model) is UNKNOWN. A RESTRICTION is the
//! one assumption: its operand inside its range, its own error charged to
//! every outcome (`trace::error`).
//!
//! **Over the lanes** (`Lanes::Over`) a value is a hull of every lane's: a
//! fork's fragment is its operand, a lane's check the hull does not decide
//! unknown (a lane inside the hull may pass), `Known` of a set unknown.
//! **One lane** (`Lanes::One`) a value is that lane's, as its kernel
//! computes it: a fragment exact, `Known` of a set false, a check the
//! lane's interval fails false, an operation the kernel computes on exact
//! operands only (`Graph::exact_operands_only`) unknown on a set.
//!
//! **Exact, not merely sound**, where it folds (`fold`, `literal`): a node
//! known to be one value is that value under every assignment its graph is
//! defined on, so replacing it changes nothing the graph computes.

use std::collections::HashMap;

use celeste_core::arith::Arithmetic;
use celeste_core::cart_data::CART;
use celeste_core::room::Room;

use super::garith::GraphArith;
use super::graph::{Cut, Graph, NodeId, Op, Ty};

/// An abstract value for ONE lane (an input of `eval`, and what it gives):
/// an exact number is a singleton interval, a known boolean is `Some`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Val<A: Arithmetic> {
    Num(A::Ival),
    /// `None` = unknown, the abstract interpreter's third boolean.
    Bool(Option<bool>),
}

impl<A: Arithmetic> Val<A> {
    pub fn exact_num(n: A) -> Self {
        Val::Num(A::point(n))
    }
}

/// What is known of one node: a number's interval (`None`: no interval is
/// known to hold the console's value), the tiles an `mget` over intervals
/// can read (the rectangle's), a boolean's three values.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Known<A: Arithmetic> {
    Num(Option<A::Ival>),
    Tiles(TileSet),
    Bool(Option<bool>),
}

impl<A: Arithmetic> Known<A> {
    pub fn num(self) -> Option<A::Ival> {
        match self {
            Known::Num(v) => v,
            Known::Tiles(t) => Some(t.hull::<A>()),
            Known::Bool(_) => None,
        }
    }
    pub fn boolean(self) -> Option<bool> {
        match self {
            Known::Bool(b) => b,
            _ => None,
        }
    }
    pub fn point(self) -> Option<A> {
        self.num().and_then(A::as_point)
    }
    /// What any value of type `ty` is known to be: a boolean nothing, a
    /// number one of the arithmetic's (a stored number is).
    fn weakest(ty: Ty) -> Self {
        match ty {
            Ty::Bool | Ty::UBool => Known::Bool(None),
            _ => Known::Num(Some(A::unknown())),
        }
    }
    /// As a lane's value: a number nothing is known of is the whole range.
    fn val(self) -> Val<A> {
        match self {
            Known::Bool(b) => Val::Bool(b),
            k => Val::Num(k.num().unwrap_or_else(A::unknown)),
        }
    }
}

impl<A: Arithmetic> From<Val<A>> for Known<A> {
    fn from(v: Val<A>) -> Self {
        match v {
            Val::Num(i) => Known::Num(Some(i)),
            Val::Bool(b) => Known::Bool(b),
        }
    }
}

/// Whose values the inputs and nodes stand for (the module doc).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Lanes {
    Over,
    One,
}

/// What the inputs are known to be.
#[derive(Clone, Copy)]
pub enum Inputs<'c, A: Arithmetic> {
    /// Nothing (the tracer: a number has a range only through its
    /// restriction).
    Unknown,
    /// Any of their type's values (the lowering's fold).
    Weakest,
    /// These values; a cell not given is any of its type's.
    Cells(&'c HashMap<u32, Val<A>>),
}

/// THE EVALUATION of one graph: what is known of each node it was asked
/// about (and of their cones), computed once. The graph may grow between
/// questions (the tracer's arena): a node's knowledge depends on its cone
/// only.
#[derive(Clone)]
pub struct Facts<'a, A: GraphArith> {
    lanes: Lanes,
    inputs: Inputs<'a, A>,
    room: Option<&'a Room>,
    known: Vec<Option<Known<A>>>,
    /// `within`'s branch facts: a node narrowed to an interval; the nodes no
    /// lane reaches under them (contradicting facts, or an operand that is).
    narrowed: Vec<(NodeId, A::Ival)>,
    unreached: rustc_hash::FxHashSet<NodeId>,
}

/// The tracer's: over the lanes, nothing known of the inputs, no map.
impl<A: GraphArith> Default for Facts<'_, A> {
    fn default() -> Self {
        Facts::new(Lanes::Over, Inputs::Unknown, None)
    }
}

impl<'a, A: GraphArith> Facts<'a, A> {
    pub fn new(lanes: Lanes, inputs: Inputs<'a, A>, room: Option<&'a Room>) -> Self {
        Facts { lanes, inputs, room, known: Vec::new(), narrowed: Vec::new(), unreached: Default::default() }
    }

    /// What is known of node `n` of `g`.
    pub fn of(&mut self, g: &Graph<A>, n: NodeId) -> Known<A> {
        if let Some(Some(k)) = self.known.get(n as usize) {
            return *k;
        }
        if self.known.len() < g.len() {
            self.known.resize(g.len(), None);
        }
        // The cone's nodes not yet known, in id order (an operand before its
        // reader).
        let mut todo: Vec<NodeId> = Vec::new();
        let mut stack = vec![n];
        let mut seen: rustc_hash::FxHashSet<NodeId> = Default::default();
        while let Some(m) = stack.pop() {
            if self.known[m as usize].is_none() && seen.insert(m) {
                todo.push(m);
                stack.extend(g.get(m).args.iter().copied());
            }
        }
        todo.sort_unstable();
        for m in todo {
            let k = self.step(g, m);
            self.known[m as usize] = Some(k);
        }
        self.known[n as usize].expect("evaluated")
    }

    /// What `op` over `args` (nodes of `g`) is known to be, the node not
    /// built.
    fn of_op(&mut self, g: &Graph<A>, op: &Op<A>, args: &[NodeId]) -> Known<A> {
        for a in args {
            self.of(g, *a);
        }
        let known = &self.known;
        let arg = |i: usize| known[args[i] as usize].expect("an operand is known");
        let deep = |m: NodeId| known[m as usize].expect("an operand's operand is known");
        rule(self.lanes, self.room, g, op, args, arg, deep)
    }

    /// The interval `n` is known to lie in.
    pub fn range(&mut self, g: &Graph<A>, n: NodeId) -> Option<A::Ival> {
        self.of(g, n).num()
    }

    /// The comparison `op` of `a` and `b` (no node made), where decided.
    pub fn compare(&mut self, g: &Graph<A>, op: &Op<A>, a: NodeId, b: NodeId) -> Option<bool> {
        let (x, y) = (self.of(g, a), self.of(g, b));
        compare(op, x, y).boolean()
    }

    /// Does `n`'s exact result (an operation that can leave the arithmetic's
    /// range, `GraphArith::range_error`) lie inside the range on every lane:
    /// its `NoWrap`, decided true?
    pub fn fits(&mut self, g: &Graph<A>, n: NodeId) -> bool {
        let node = g.get(n);
        let k: Vec<Known<A>> = node.args.iter().map(|a| self.of(g, *a)).collect();
        no_wrap(self.lanes, &node.op, |i| k[i]) == Some(true)
    }

    /// Node `n`, its operands known: `rule`, a cell its input's, and
    /// `within`'s facts.
    fn step(&mut self, g: &Graph<A>, n: NodeId) -> Known<A> {
        let node = g.get(n);
        let facts = !self.narrowed.is_empty();
        if facts && node.op != Op::Sel && node.args.iter().any(|a| self.unreached.contains(a)) {
            self.unreached.insert(n);
            return Known::Num(None);
        }
        let known = &self.known;
        let arg = |i: usize| known[node.args[i] as usize].expect("an operand is known before its reader");
        let deep = |m: NodeId| known[m as usize].expect("an operand's operand is known");
        let k = match node.op {
            Op::Cell(c) => match self.inputs {
                Inputs::Cells(cells) if cells.contains_key(&c) => cells[&c].into(),
                Inputs::Unknown if !g.ty(n).is_bool() => Known::Num(None),
                _ => Known::weakest(g.ty(n)),
            },
            // An arm no lane reaches is no arm.
            Op::Sel if facts => {
                let unreached = |i: usize| self.unreached.contains(&node.args[i]);
                match (arg(0).boolean(), unreached(1), unreached(2)) {
                    (Some(true), true, _) | (Some(false), _, true) | (None, true, true) => {
                        self.unreached.insert(n);
                        return Known::Num(None);
                    }
                    (None, true, false) => arg(2),
                    (None, false, true) => arg(1),
                    _ => rule(self.lanes, self.room, g, &node.op, &node.args, arg, deep),
                }
            }
            _ => rule(self.lanes, self.room, g, &node.op, &node.args, arg, deep),
        };
        // One lane: a number nothing is known of is the whole range (as the
        // lane holds one of the arithmetic's).
        let k = match k {
            Known::Num(None) if self.lanes == Lanes::One => Known::Num(Some(A::unknown())),
            k => k,
        };
        if facts {
            return self.narrow(n, k);
        }
        k
    }

    /// `k` under `within`'s facts on `n`: narrowed (nothing known: the
    /// fact's interval); disjoint, no lane reaches `n`.
    fn narrow(&mut self, n: NodeId, mut k: Known<A>) -> Known<A> {
        for &(_, f) in self.narrowed.iter().filter(|(m, _)| *m == n) {
            k = match k.num() {
                None => Known::Num(Some(f)),
                Some(v) => match intersect::<A>(v, f) {
                    Some(i) => Known::Num(Some(i)),
                    None => {
                        self.unreached.insert(n);
                        return Known::Num(None);
                    }
                },
            };
        }
        k
    }
}

/// Two intervals' common part.
fn intersect<A: Arithmetic>(x: A::Ival, y: A::Ival) -> Option<A::Ival> {
    let ((xl, xh), (yl, yh)) = (A::bounds(x), A::bounds(y));
    let (l, h) = (A::max(xl, yl), A::min(xh, yh));
    A::le(l, h).then(|| A::ival(l, h))
}

/// `NoWrap(x)`, `x` being `op` of operands `k`: the operation's checked
/// form on their intervals - fits: true; does not: false on one lane,
/// unknown over the lanes (a lane inside may fit). True of an operation
/// that cannot leave the range.
fn no_wrap<A: GraphArith>(lanes: Lanes, op: &Op<A>, k: impl Fn(usize) -> Known<A>) -> Option<bool> {
    let arg = |i: usize| k(i).num();
    let fits = match op {
        // A divisor 0 on every lane fits on none.
        Op::Div if k(1).point().is_some_and(|q| A::num_eq(q, A::from_i16(0))) => return Some(false),
        Op::Add => A::ival_add(arg(0)?, arg(1)?).is_some(),
        Op::Sub => A::ival_sub(arg(0)?, arg(1)?).is_some(),
        Op::Mul => A::ival_mul(arg(0)?, arg(1)?).is_some(),
        Op::Div => A::ival_div(arg(0)?, arg(1)?).is_some(),
        Op::Neg => A::ival_neg(arg(0)?).is_some(),
        Op::Abs => A::ival_abs(arg(0)?).is_some(),
        // `%` (a double's own error): on points only.
        Op::Rem => A::rem(k(0).point()?, k(1).point()?).is_some(),
        _ => true,
    };
    (fits || lanes == Lanes::One).then_some(fits)
}

/// THE RULES: what is known of `op` over `args`, from what is known of them
/// (`arg(i)`) and of a span's operands (`deep`).
fn rule<A: GraphArith>(
    lanes: Lanes,
    room: Option<&Room>,
    g: &Graph<A>,
    op: &Op<A>,
    args: &[NodeId],
    arg: impl Fn(usize) -> Known<A>,
    deep: impl Fn(NodeId) -> Known<A>,
) -> Known<A> {
    use Known::{Bool as B, Num as N};
    let over = lanes == Lanes::Over;
    let num = |i: usize| arg(i).num();
    let point = |i: usize| arg(i).point();
    // A result that may leave the range: what the console makes of it, one
    // of 16.16's numbers (`GraphArith::CLOSED`); a double's may not be finite.
    let wrapped = || A::CLOSED.then(A::unknown);
    match op {
        Op::Const(lo, hi) => N(Some(A::ival(A::from_lit(*lo), A::from_lit(*hi)))),
        Op::ConstBool(b) => B(Some(*b)),
        Op::UnknownBool(_) => B(None),
        // No interval holds it; a pin has no value (its uses read the
        // constant); a cell is its input's (`Facts::step`).
        Op::UnknownNum { .. } | Op::Pin(_) | Op::Cell(_) => N(None),
        // The hull, tolerant of `lo > hi` (an unreached table-fork fragment).
        Op::Span => N(num(0).zip(num(1)).map(|(a, b)| {
            let (lo, hi) = (A::bounds(a).0, A::bounds(b).1);
            if A::lt(hi, lo) {
                A::ival(hi, lo)
            } else {
                A::ival(lo, hi)
            }
        })),
        // The operand inside the assumed range (a lane outside is in error,
        // the node's own); disjoint, every lane is in error: the operand, as
        // the kernel.
        Op::Restrict(lo, hi) => {
            let r = A::ival(A::from_lit(*lo), A::from_lit(*hi));
            N(Some(match num(0) {
                None => r,
                Some(x) => intersect::<A>(x, r).unwrap_or(x),
            }))
        }
        // A fork RESTRICTS its operand: over the lanes, the operand covers
        // it; one lane holds no fork (outside an outcome).
        Op::Fork(..) if over => arg(0),
        Op::Fork(..) => N(None),
        Op::ForkValid(..) => B(None),
        // At most `ways` cells of the grid: a literal's as its kernel
        // computes it (`GraphArith::lit_split_ok`, wraps included), every
        // lane holding it; a hull that fits fits on every lane.
        Op::ForkOk(grid, ways) => match g.get(args[0]).op {
            Op::Const(lo, hi) if *grid == Cut::Floors => B(Some(A::lit_split_ok(lo, hi, *ways))),
            Op::Const(..) => B(num(0).map(|iv| A::cells(*grid, iv)).filter(|c| *c != i64::MAX).map(|c| c < *ways as i64)),
            _ => B(num(0).filter(|iv| A::cells(*grid, *iv) < *ways as i64).map(|_| true)),
        },
        // An `Ints` fragment: its cell's low end, exact (over the lanes: its
        // hull).
        Op::Frag(Cut::Ints, c) => N(num(0).and_then(|iv| A::fragment(Cut::Ints, iv, *c, over).ok())),
        // Fragment 0 is never empty.
        Op::FragOk(_, 0) => B(Some(true)),
        // The RESOLVED fork (the floors': the kernel's `zi_fork_flr`).
        Op::Frag(cut, c) | Op::FragOk(cut, c) => {
            let frag = matches!(op, Op::Frag(..));
            let Some(iv) = num(0) else { return if frag { N(None) } else { B(None) } };
            // Cells spanned past the low end's.
            let reaches = A::cells(*cut, iv) >= *c as i64;
            match (frag, over) {
                // Over the lanes a lane in a wide hull may still span `c` cells:
                // decided only where the hull rules it out.
                (true, true) => N(Some(iv)),
                (false, true) => B((!reaches).then_some(false)),
                // An invalid fragment keeps the whole value (never taken).
                (true, false) if reaches => N(A::fragment(*cut, iv, *c, false).ok()),
                (true, false) => N(Some(iv)),
                (false, false) => B(Some(reaches)),
            }
        }
        // Bits: decided on two points, or two values apart.
        Op::Same => B(num(0).zip(num(1)).and_then(|(x, y)| match (A::as_point(x), A::as_point(y)) {
            (Some(p), Some(q)) => Some(A::bits(p) == A::bits(q)),
            _ => {
                let ((xl, xh), (yl, yh)) = (A::bounds(x), A::bounds(y));
                (A::lt(xh, yl) || A::lt(yh, xl)).then_some(false)
            }
        })),
        // ONE lane's end, over the lanes: anywhere in the hull (except a
        // span's or a literal's), never the hull's end as a point, or a fold
        // would decide premises on it.
        Op::Lo | Op::Hi => {
            let low = *op == Op::Lo;
            let a = g.get(args[0]);
            match a.op {
                Op::Span => N(deep(a.args[if low { 0 } else { 1 }]).num()),
                Op::Const(l, h) => N(Some(A::point(A::from_lit(if low { l } else { h })))),
                _ => N(num(0)),
            }
        }
        Op::NoWrap => {
            let x = g.get(args[0]);
            B(no_wrap(lanes, &x.op, |i| deep(x.args[i])))
        }
        Op::Add => N(num(0).zip(num(1)).and_then(|(x, y)| A::ival_add(x, y).or_else(wrapped))),
        Op::Sub => N(num(0).zip(num(1)).and_then(|(x, y)| A::ival_sub(x, y).or_else(wrapped))),
        Op::Neg => N(num(0).and_then(|x| A::ival_neg(x).or_else(wrapped))),
        // The console's: 16.16's `abs(-32768)` saturates.
        Op::Abs => N(num(0).and_then(A::console_abs)),
        // One lane, an operation its kernel computes on exact operands only
        // (an interval times or over a positive literal aside) is unknown on
        // a set.
        Op::Mul | Op::Div | Op::Rem | Op::Sin if !over && g.exact_operands_only(op, args) && (0..args.len()).any(|i| point(i).is_none()) => N(None),
        Op::Mul => N(num(0).zip(num(1)).and_then(|(x, y)| A::ival_mul(x, y).or_else(wrapped))),
        Op::Div => N(num(0).zip(num(1)).and_then(|(x, y)| A::ival_div(x, y).or_else(wrapped))),
        Op::Rem => N(point(0).zip(point(1)).and_then(|(p, q)| A::rem(p, q)).map(A::point)),
        // Over the lanes, of a set: the console's `sin` lies in [-1, 1].
        Op::Sin => N(match point(0) {
            Some(p) => A::sin(p).map(A::point),
            None => Some(A::ival(A::from_i16(-1), A::from_i16(1))),
        }),
        // flr is monotone, so endpoints suffice.
        Op::Flr => N(num(0).map(|x| {
            let (lo, hi) = A::bounds(x);
            A::ival(A::flr(lo), A::flr(hi))
        })),
        // Monotone in each operand: the ends' (the arithmetic's own
        // `min`/`max`, its tie rule included).
        Op::Min | Op::Max => N(num(0).zip(num(1)).map(|(x, y)| {
            let ((xl, xh), (yl, yh)) = (A::bounds(x), A::bounds(y));
            let pick = |p: A, q: A| if *op == Op::Min { A::min(p, q) } else { A::max(p, q) };
            A::ival(pick(xl, yl), pick(xh, yh))
        })),
        // A tile against a literal, decided by the SET of tiles.
        Op::Eq => match (arg(0), arg(1)) {
            (Known::Tiles(t), k) | (k, Known::Tiles(t)) if k.point().is_some() => B(t.eq::<A>(k.point().expect("guarded"))),
            (x, y) => compare(op, x, y),
        },
        Op::Lt | Op::Le | Op::Gt | Op::Ge => compare(op, arg(0), arg(1)),
        Op::Not => B(arg(0).boolean().map(|b| !b)),
        // Kleene: a decided `false` wins an `and`, a `true` an `or`.
        Op::And => B(match (arg(0).boolean(), arg(1).boolean()) {
            (Some(false), _) | (_, Some(false)) => Some(false),
            (Some(true), Some(true)) => Some(true),
            _ => None,
        }),
        Op::Or => B(match (arg(0).boolean(), arg(1).boolean()) {
            (Some(true), _) | (_, Some(true)) => Some(true),
            (Some(false), Some(false)) => Some(false),
            _ => None,
        }),
        // Undecided: the lane is in error (the select's own), so the join
        // serves.
        Op::Sel => match arg(0).boolean() {
            Some(true) => arg(1),
            Some(false) => arg(2),
            None => join(arg(1), arg(2)),
        },
        // Per-lane decidedness. Over the lanes: decided means every lane is;
        // undecided says nothing.
        Op::Known => {
            let decided = match arg(0) {
                Known::Bool(b) => b.is_some(),
                k => k.point().is_some(),
            };
            B(if decided { Some(true) } else { (!over).then_some(false) })
        }
        Op::TileFlagAt => B(room.and_then(|room| tile_flag_over::<A>(room, [num(0), num(1), num(2), num(3), num(4)]))),
        // `mget` on EXACT coordinates, as in the kernels; over the lanes, of
        // intervals, the tiles the rectangle holds.
        Op::Mget => match (room, num(0), num(1)) {
            (Some(_), Some(x), Some(y)) => match (A::as_point(x), A::as_point(y)) {
                (Some(xp), Some(yp)) => N(A::as_i16(xp).zip(A::as_i16(yp)).map(|(xi, yi)| A::point(A::from_i16(CART.mget_whole(xi, yi) as i16)))),
                _ if over => Known::Tiles(TileSet::over::<A>(x, y)),
                _ => N(None),
            },
            _ => N(None),
        },
    }
}

/// The comparison `op` of two values: decided iff the answer is the same
/// for every pair in the boxes; booleans compare with `==` only.
fn compare<A: Arithmetic>(op: &Op<A>, x: Known<A>, y: Known<A>) -> Known<A> {
    if let (Known::Bool(p), Known::Bool(q)) = (x, y) {
        return Known::Bool(match (op, p, q) {
            (Op::Eq, Some(a), Some(b)) => Some(a == b),
            _ => None,
        });
    }
    let (Some(a), Some(b)) = (x.num(), y.num()) else { return Known::Bool(None) };
    let ((al, ah), (bl, bh)) = (A::bounds(a), A::bounds(b));
    // Order comparisons: the corners bound the box (`ah < bl` decides `a < b`).
    let order = |yes: bool, no: bool| if yes { Some(true) } else if no { Some(false) } else { None };
    Known::Bool(match op {
        Op::Lt => order(A::lt(ah, bl), A::le(bh, al)),
        Op::Le => order(A::le(ah, bl), A::lt(bh, al)),
        Op::Gt => order(A::lt(bh, al), A::le(ah, bl)),
        Op::Ge => order(A::le(bh, al), A::lt(ah, bl)),
        // Equality needs disjointness or two points.
        _ => {
            if A::lt(bh, al) || A::lt(ah, bl) {
                Some(false)
            } else {
                A::as_point(a).zip(A::as_point(b)).map(|(p, q)| A::num_eq(p, q))
            }
        }
    })
}

/// Two values a lane may hold either of: the hull; booleans agreeing.
fn join<A: Arithmetic>(x: Known<A>, y: Known<A>) -> Known<A> {
    match (x, y) {
        (Known::Bool(a), Known::Bool(b)) => Known::Bool(if a == b { a } else { None }),
        (Known::Bool(_), _) | (_, Known::Bool(_)) => Known::Bool(None),
        _ => Known::Num(x.num().zip(y.num()).map(|(a, b)| {
            let ((al, ah), (bl, bh)) = (A::bounds(a), A::bounds(b));
            A::ival(A::min(al, bl), A::max(ah, bh))
        })),
    }
}

/// Every node of `g` evaluated from `cells` (a cell not given: any value of
/// its type), as `lanes` says (`room`: the map, for `mget` and
/// `tile_flag_at`); a number nothing is known of is the arithmetic's whole
/// range, and `mget`'s tiles their hull (a lane's values, not the fold's
/// sets).
pub fn eval<A: GraphArith>(g: &Graph<A>, cells: &HashMap<u32, Val<A>>, lanes: Lanes, room: Option<&Room>) -> Vec<Val<A>> {
    let mut out: Vec<Val<A>> = Vec::with_capacity(g.len());
    for n in 0..g.len() as NodeId {
        let node = g.get(n);
        let k = match node.op {
            Op::Cell(c) => cells.get(&c).map_or_else(|| Known::weakest(g.ty(n)), |v| (*v).into()),
            _ => rule(lanes, room, g, &node.op, &node.args, |i| out[node.args[i] as usize].into(), |m| out[m as usize].into()),
        };
        out.push(k.val());
    }
    out
}

/// THE LOWERING'S FOLD: `g` rebuilt over the nodes `roots` reach, every
/// node the evaluation pins down (inputs at their weakest, the map's tiles
/// as sets) a constant; evaluated on the REBUILT graph as it is built, so
/// what a replacement exposes is seen in the same pass. Returns the new
/// graph and a FULL-length node map (`graph::UNBUILT` where not reached).
pub fn fold<A: GraphArith>(g: &Graph<A>, roots: &[NodeId], room: Option<&Room>) -> (Graph<A>, Vec<NodeId>) {
    let need = g.reachable(roots);
    let mut out = g.like();
    let mut facts = Facts::new(Lanes::Over, Inputs::Weakest, room);
    // A constant for what is known to be one, a node built for nothing.
    let konst = |k: Known<A>, out: &mut Graph<A>| match (k.boolean(), k.point()) {
        (Some(b), _) => Some(out.leaf(Op::ConstBool(b))),
        // A point whose bits its value fixes (not a double's zero: a lane may
        // hold the other one, `GraphArith::alone_in_its_value`).
        (_, Some(p)) if A::alone_in_its_value(p) => Some(out.leaf(Op::Const(A::lit(p), A::lit(p)))),
        _ => None,
    };
    let map = g.rebuild_into(Some(&need), &mut out, |_, node, args, out| {
        if let Some(c) = konst(facts.of_op(out, &node.op, args), out) {
            return Some(c);
        }
        // `Graph::fold`'s own rules may find more (a branch re-merge is its
        // guard).
        let n = out.fold(node.op.clone(), args.to_vec());
        Some(konst(facts.of(out, n), out).unwrap_or(n))
    });
    (out, map)
}

/// `op` over `args` in `g`, known from the literals among them (every lane
/// holds each: one lane's rules) and nothing of the rest (over the lanes,
/// each at its weakest), where the operation cannot err: `Graph::fold`'s and
/// the tracer's literal folding.
pub fn literal<A: GraphArith>(g: &Graph<A>, op: &Op<A>, args: &[NodeId]) -> Known<A> {
    let known = |a: NodeId| match g.get(a).op {
        Op::Const(lo, hi) => Known::Num(Some(A::ival(A::from_lit(lo), A::from_lit(hi)))),
        Op::ConstBool(b) => Known::Bool(Some(b)),
        _ => Known::weakest(g.ty(a)),
    };
    // Literals alone (a `NoWrap`'s: its operation's operands) are every
    // lane's: one lane's rules.
    let lit = |a: &NodeId| matches!(g.get(*a).op, Op::Const(..) | Op::ConstBool(_));
    let lanes = match op {
        Op::NoWrap if g.get(args[0]).args.iter().all(lit) => Lanes::One,
        _ if args.iter().all(lit) => Lanes::One,
        _ => Lanes::Over,
    };
    let arg = |i: usize| known(args[i]);
    // A fold must not drop an owed error: an operation that may leave the
    // range on these operands is not known.
    if A::range_error(op) && no_wrap(lanes, op, arg) != Some(true) {
        return Known::Num(None);
    }
    rule(lanes, None, g, op, args, arg, known)
}

/// Does `v` provably lie in `[lo, hi]`? Its range, and through select arms
/// on `x op k` (`k` a literal) with what each answer says about `x` (so a
/// wrap `x < -16 ? 128 : ...` is bounded). `false` if unsure.
pub fn within<A: GraphArith>(g: &Graph<A>, v: NodeId, lo: A, hi: A) -> bool {
    within_under(g, v, A::ival(lo, hi), &mut Vec::new())
}

fn within_under<A: GraphArith>(g: &Graph<A>, v: NodeId, bound: A::Ival, narrowed: &mut Vec<(NodeId, A::Ival)>) -> bool {
    let mut f = Facts::default();
    f.narrowed = narrowed.clone();
    let k = f.of(g, v);
    let (lo, hi) = A::bounds(bound);
    if f.unreached.contains(&v) || k.num().is_some_and(|iv| A::le(lo, A::bounds(iv).0) && A::le(A::bounds(iv).1, hi)) {
        return true;
    }
    let node = g.get(v);
    if node.op != Op::Sel {
        return false;
    }
    let (c, t, e) = (node.args[0], node.args[1], node.args[2]);
    let Some((x, yes, no)) = comparison_facts(g, c) else {
        return within_under(g, t, bound, narrowed) && within_under(g, e, bound, narrowed);
    };
    narrowed.push((x, yes));
    let a = within_under(g, t, bound, narrowed);
    narrowed.pop();
    narrowed.push((x, no));
    let b = within_under(g, e, bound, narrowed);
    narrowed.pop();
    a && b
}

/// `c` as `x op k` (`k` a literal point, either side): `x` and where it lies
/// `(when true, when false)`; `k`'s neighbours are the arithmetic's
/// (`Arithmetic::below`, and above as `-below(-k)`).
fn comparison_facts<A: GraphArith>(g: &Graph<A>, c: NodeId) -> Option<(NodeId, A::Ival, A::Ival)> {
    let node = g.get(c);
    if !matches!(node.op, Op::Lt | Op::Le | Op::Gt | Op::Ge) {
        return None;
    }
    // `k op x` is `x op' k` with the operator mirrored.
    let (x, k, op) = match (g.point(node.args[0]), g.point(node.args[1])) {
        (None, Some(k)) => (node.args[0], k, node.op.clone()),
        (Some(k), None) => (
            node.args[1],
            k,
            match node.op {
                Op::Lt => Op::Gt,
                Op::Le => Op::Ge,
                Op::Gt => Op::Lt,
                _ => Op::Le,
            },
        ),
        _ => return None,
    };
    let pred = A::below(k)?;
    let succ = A::below(A::neg(k)?).and_then(A::neg)?;
    let (min, max) = A::bounds(A::unknown());
    Some(match op {
        Op::Lt => (x, A::ival(min, pred), A::ival(k, max)),
        Op::Le => (x, A::ival(min, k), A::ival(succ, max)),
        Op::Gt => (x, A::ival(succ, max), A::ival(min, k)),
        _ => (x, A::ival(k, max), A::ival(min, pred)),
    })
}

/// `tile_flag_at` over INTERVALS: FALSE if the UNION of the rectangles holds
/// no flagged tile, TRUE if their INTERSECTION holds one, unknown between.
fn tile_flag_over<A: GraphArith>(room: &Room, [x, y, w, h, flag]: [Option<A::Ival>; 5]) -> Option<bool> {
    // The size and flag must be exact.
    let ex = |v: Option<A::Ival>| v.and_then(A::as_point).and_then(A::as_i16).map(|n| n as i32);
    let (w, h, flag) = (ex(w)?, ex(h)?, ex(flag)? as i16);
    // Every integer from floor(low) to floor(high); non-integers fail
    // concretely, so covering them is conservative.
    let span = |v: Option<A::Ival>| -> Option<(i32, i32)> {
        let (lo, hi) = A::bounds(v?);
        Some((A::as_i16(A::flr(lo))? as i32, A::as_i16(A::flr(hi))? as i32))
    };
    let ((xlo, xhi), (ylo, yhi)) = (span(x)?, span(y)?);
    // Clamp the rectangle's EDGES to the 16-tile room, never corner and
    // size apart: at TOP that leaves only row 0, reading "nothing solid".
    let edges = |lo: i32, size: i32| -> (i16, i16) {
        let hi = lo + size - 1;
        let (lo, hi) = (lo.clamp(-4096, 4096), hi.clamp(-4096, 4096));
        (lo as i16, (hi - lo + 1) as i16)
    };
    let (dx, dy) = (xhi - xlo, yhi - ylo);
    let solid = |px: i32, py: i32, pw: i32, ph: i32| -> Option<bool> {
        let ((px, pw), (py, ph)) = (edges(px, pw), edges(py, ph));
        room.flag_at(px, py, pw, ph, flag).ok()
    };
    if !solid(xlo, ylo, w + dx, h + dy)? {
        return Some(false);
    }
    if dx < w && dy < h && solid(xhi, yhi, w - dx, h - dy)? {
        return Some(true);
    }
    None
}

/// The tiles `mget` can return over a rectangle of coordinates: one bit per
/// tile number (`mget` returns a byte).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TileSet([u64; 4]);

impl TileSet {
    /// Every tile `mget(x, y)` returns for `x` in `xs`, `y` in `ys`: a lane's
    /// coordinate is an integer inside its hull (`mget` of a fraction raises),
    /// and outside the map `mget` is 0, so the rectangle is clipped to the map
    /// plus one row of "outside" on each side.
    fn over<A: GraphArith>(xs: A::Ival, ys: A::Ival) -> TileSet {
        let ((x0, x1), (y0, y1)) = (A::tile_span(xs, 128), A::tile_span(ys, 64));
        let mut set = TileSet([0; 4]);
        for x in x0..=x1 {
            for y in y0..=y1 {
                let t = CART.mget_whole(x as i16, y as i16) as usize;
                set.0[t / 64] |= 1 << (t % 64);
            }
        }
        set
    }

    fn contains(&self, t: usize) -> bool {
        t < 256 && self.0[t / 64] & (1 << (t % 64)) != 0
    }

    fn hull<A: Arithmetic>(&self) -> A::Ival {
        let mut it = (0..256).filter(|t| self.contains(*t));
        let lo = it.next().expect("a rectangle holds a tile") as i16;
        let hi = it.last().map_or(lo, |t| t as i16);
        A::ival(A::from_i16(lo), A::from_i16(hi))
    }

    /// `mget == k` over the set: false if no tile is `k`, true if every one is.
    fn eq<A: Arithmetic>(&self, k: A) -> Option<bool> {
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
    use celeste_core::arith::Fixed;
    use celeste_core::pico8_num::Pico8NumInterval;
    use crate::pico8_num::Pico8Num;

    /// A box with both ends zero holds lanes of either zero: `min(0, x)`
    /// for `x` restricted to `[0, 5]` is `0` by value, but a lane of `x =
    /// -0` gives `-0` (the tie goes to the second argument). 16.16 folds it
    /// to the constant; the doubles keep the node.
    #[test]
    fn a_zero_box_folds_to_a_constant_only_where_zero_has_one_sign() {
        use celeste_core::arith::Float;
        fn min_of_zero_and<A: GraphArith>(lo: A, hi: A) -> bool {
            let mut g = Graph::<A>::default();
            let x = g.leaf(Op::Cell(0));
            let x = g.add(Op::Restrict(A::lit(lo), A::lit(hi)), vec![x]);
            let z = A::lit(A::from_i16(0));
            let zero = g.leaf(Op::Const(z, z));
            let m = g.fold(Op::Min, vec![zero, x]);
            let (out, map) = fold(&g, &[m], None);
            matches!(out.get(map[m as usize]).op, Op::Const(..))
        }
        assert!(min_of_zero_and::<celeste_core::arith::Fixed>(Pico8Num::from_i16(0), Pico8Num::from_i16(5)), "16.16 folds it");
        assert!(!min_of_zero_and::<Float>(Float::from_i16(0), Float::from_i16(5)), "the doubles keep it");
    }

    /// `abs` is non-negative, so a comparison of it against zero is not a free
    /// variable however opaque its operand is.
    #[test]
    fn a_comparison_of_abs_against_zero_is_not_a_free_variable() {
        let mut g = Graph::<Fixed>::default();
        let x = g.leaf(Op::Cell(0));
        let y = g.leaf(Op::Cell(1));
        let sum = g.fold(Op::Add, vec![x, y]);
        let a = g.fold(Op::Abs, vec![sum]);
        let zero = g.leaf(Op::Const(0, 0));
        let gt = g.fold(Op::Gt, vec![zero, a]);
        let le = g.fold(Op::Le, vec![zero, a]);
        let (out, map) = fold(&g, &[gt, le], None);
        assert_eq!(out.get(map[gt as usize]).op, Op::ConstBool(false), "0 > abs(x)");
        assert_eq!(out.get(map[le as usize]).op, Op::ConstBool(true), "0 <= abs(x)");
    }

    /// The map is a constant, so a collision test at a known position is
    /// decidable; at an unknown position it genuinely is not.
    #[test]
    fn a_collision_test_is_decided_at_a_known_position_and_not_at_an_unknown_one() {
        let (rx, ry) = crate::settings::Settings::default().room;
        let room = Room::of((rx, ry));

        let at = |x: i16, y: i16| -> Option<bool> {
            let mut g = Graph::<Fixed>::default();
            let n = |g: &mut Graph<Fixed>, v: i16| {
                let r = Pico8Num::from_i16(v).as_raw_u32() as i32;
                g.leaf(Op::Const(r, r))
            };
            let (px, py) = (n(&mut g, x), n(&mut g, y));
            let (w, h) = (n(&mut g, 8), n(&mut g, 8));
            let f = n(&mut g, 0);
            let t = g.fold(Op::TileFlagAt, vec![px, py, w, h, f]);
            let (out, map) = fold(&g, &[t], Some(room));
            match out.get(map[t as usize]).op {
                Op::ConstBool(b) => Some(b),
                _ => None,
            }
        };
        // Every exact position must be decided and agree with the concrete
        // collision routine.
        let mut solid_seen = false;
        let mut open_seen = false;
        for y in (0..128).step_by(8) {
            for x in (0..128).step_by(8) {
                let want = room.flag_at(x, y, 8, 8, 0).expect("tile_flag_at");
                assert_eq!(at(x, y), Some(want), "at ({}, {})", x, y);
                solid_seen |= want;
                open_seen |= !want;
            }
        }
        assert!(solid_seen && open_seen, "the room has both walls and space");

        // An UNKNOWN position is not decidable.
        let mut g = Graph::<Fixed>::default();
        let cell = g.leaf(Op::Cell(0));
        let n = |g: &mut Graph<Fixed>, v: i16| {
            let r = Pico8Num::from_i16(v).as_raw_u32() as i32;
            g.leaf(Op::Const(r, r))
        };
        let (w, h) = (n(&mut g, 8), n(&mut g, 8));
        let f = n(&mut g, 0);
        let t = g.fold(Op::TileFlagAt, vec![cell, cell, w, h, f]);
        let (out, map) = fold(&g, &[t], Some(room));
        assert_eq!(out.get(map[t as usize]).op, Op::TileFlagAt, "unknown stays unknown");
    }

    /// A collision test with ONE coordinate at TOP is decided false only where
    /// no position along that axis is solid, in every level room.
    #[test]
    fn a_collision_test_at_a_top_coordinate_folds_only_to_what_every_position_gives() {
        let raw = |v: i16| Pico8Num::from_i16(v).as_raw_u32() as i32;
        let (mut checked, mut decided) = (0usize, 0usize);
        for ry in 0..4i16 {
            for rx in 0..8i16 {
                if (rx, ry) == (7, 3) {
                    continue; // the summit: no level room
                }
                let room = Room::of((rx, ry));
                // The player's hitbox (6x5) and a whole tile.
                for (w, h) in [(6i16, 5i16), (8, 8)] {
                    for at in -8..136i16 {
                        for top_is_y in [true, false] {
                            let mut g = Graph::<Fixed>::default();
                            let exact = g.leaf(Op::Const(raw(at), raw(at)));
                            let top = g.leaf(Op::Cell(0));
                            let (px, py) = if top_is_y { (exact, top) } else { (top, exact) };
                            let (gw, gh, f) = (g.leaf(Op::Const(raw(w), raw(w))), g.leaf(Op::Const(raw(h), raw(h))), g.leaf(Op::Const(0, 0)));
                            let t = g.fold(Op::TileFlagAt, vec![px, py, gw, gh, f]);
                            let (out, map) = fold(&g, &[t], Some(room));
                            let got = match out.get(map[t as usize]).op {
                                Op::ConstBool(b) => Some(b),
                                _ => None,
                            };
                            // What the concrete test gives along the TOP axis
                            // (beyond [-16, 144] every rectangle misses the room).
                            let any_solid = (-16..=144i16).any(|v| {
                                let (x, y) = if top_is_y { (at, v) } else { (v, at) };
                                room.flag_at(x, y, w, h, 0).expect("tile_flag_at")
                            });
                            checked += 1;
                            decided += got.is_some() as usize;
                            assert!(got != Some(true), "room ({rx},{ry}) {w}x{h} at {at} ({}) folded to true", if top_is_y { "x; y TOP" } else { "y; x TOP" });
                            assert!(
                                got != Some(false) || !any_solid,
                                "room ({rx},{ry}) {w}x{h} at {} = {at}, the other axis TOP: folded to false, but a position along it is solid",
                                if top_is_y { "x" } else { "y" }
                            );
                        }
                    }
                }
            }
        }
        // Open columns ARE decided false: the test is not vacuous.
        assert!(decided > 0 && decided < checked, "{decided} of {checked} decided");
    }

    /// `mget == k` over a RESTRICTED rectangle of tiles (as `spikes_at` reads
    /// the map) folds to false exactly where no tile is `k`, to true where every
    /// one is, and is otherwise left standing; lane by lane inside the rectangle
    /// the folded graph computes what the unfolded one does. Every level room,
    /// rectangles up to 3x3 tiles.
    #[test]
    fn a_tile_test_over_a_restricted_rectangle_folds_exactly_by_the_set_of_tiles() {
        let cart = &celeste_core::cart_data::CART;
        let raw = |v: i16| Pico8Num::from_i16(v).as_raw_u32() as i32;
        let (mut decided_false, mut kept) = (0usize, 0usize);
        for ry in 0..4i16 {
            for rx in 0..8i16 {
                let room = Room::of((rx, ry));
                let tile = |i: i16, j: i16| cart.mget_whole(rx * 16 + i, ry * 16 + j) as i16;
                for (i0, j0) in [(0i16, 0i16), (3, 1), (7, 12), (13, 13), (-1, 14), (14, -1)] {
                    for (w, h) in [(1i16, 1i16), (2, 2), (3, 2), (3, 3)] {
                        let mut g = Graph::<Fixed>::default();
                        let (ci, cj) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
                        let ri = g.fold(Op::Restrict(raw(i0), raw(i0 + w - 1)), vec![ci]);
                        let rj = g.fold(Op::Restrict(raw(j0), raw(j0 + h - 1)), vec![cj]);
                        let (ox, oy) = (g.leaf(Op::Const(raw(rx * 16), raw(rx * 16))), g.leaf(Op::Const(raw(ry * 16), raw(ry * 16))));
                        let (x, y) = (g.fold(Op::Add, vec![ox, ri]), g.fold(Op::Add, vec![oy, rj]));
                        let m = g.fold(Op::Mget, vec![x, y]);
                        let mut roots = Vec::new();
                        for k in [17i16, 27, 43, 59, tile(i0.max(0), j0.max(0))] {
                            let kn = g.leaf(Op::Const(raw(k), raw(k)));
                            roots.push((k, g.fold(Op::Eq, vec![m, kn])));
                        }
                        let rs: Vec<NodeId> = roots.iter().map(|r| r.1).collect();
                        let (out, map) = fold(&g, &rs, Some(room));
                        for &(k, r) in &roots {
                            let lanes: Vec<bool> = (i0..i0 + w).flat_map(|i| (j0..j0 + h).map(move |j| (i, j))).map(|(i, j)| tile(i, j) == k).collect();
                            match out.get(map[r as usize]).op {
                                Op::ConstBool(false) => {
                                    decided_false += 1;
                                    assert!(lanes.iter().all(|t| !t), "room ({rx},{ry}) tile {k} at {i0},{j0} {w}x{h}: folded false, but a tile is {k}");
                                }
                                Op::ConstBool(true) => assert!(lanes.iter().all(|t| *t), "folded true, but a tile is not {k}"),
                                _ => {
                                    kept += 1;
                                    assert!(lanes.iter().any(|t| *t) && !lanes.iter().all(|t| *t), "room ({rx},{ry}) tile {k} at {i0},{j0} {w}x{h}: decidable, left standing");
                                }
                            }
                            // Lane by lane, folded and unfolded agree.
                            for i in i0..i0 + w {
                                for j in j0..j0 + h {
                                    let cells = HashMap::from([(0u32, Val::exact_num(Pico8Num::from_i16(i))), (1u32, Val::exact_num(Pico8Num::from_i16(j)))]);
                                    let before = eval(&g, &cells, Lanes::Over, Some(room))[r as usize];
                                    let after = eval(&out, &cells, Lanes::Over, Some(room))[map[r as usize] as usize];
                                    assert_eq!(before, after, "room ({rx},{ry}) tile {k} lane ({i},{j})");
                                    assert_eq!(before, Val::Bool(Some(tile(i, j) == k)));
                                }
                            }
                        }
                    }
                }
            }
        }
        assert!(decided_false > 0 && kept > 0, "{decided_false} decided false, {kept} kept: the test is not vacuous");
    }

    /// A node it cannot model must not be decided, and must not poison the
    /// nodes that do not depend on it.
    #[test]
    fn an_unmodelled_op_becomes_top_rather_than_an_error() {
        let mut g = Graph::<Fixed>::default();
        let x = g.leaf(Op::Cell(0));
        let w = g.leaf(Op::Const(65536, 65536));
        let tile = g.fold(Op::TileFlagAt, vec![x, x, w, w, w]);
        let a = g.fold(Op::Abs, vec![x]);
        let zero = g.leaf(Op::Const(0, 0));
        let le = g.fold(Op::Le, vec![zero, a]);
        let both = g.fold(Op::And, vec![tile, le]);
        let (out, map) = fold(&g, &[both], None);
        // The tile lookup stays; the arithmetic fact next to it does not.
        assert_eq!(out.get(map[le as usize]).op, Op::ConstBool(true));
        assert_eq!(out.get(map[both as usize]).op, Op::TileFlagAt);
    }

    /// Fork validity is a per-LANE fact, and the interval pass sees a hull over
    /// every lane: a hull spanning many floors contains lanes that span exactly
    /// two, so `FragOk(1)` must stay undecided there. A hull inside ONE floor
    /// decides it.
    #[test]
    fn fragment_validity_is_not_decided_from_a_wide_hull() {
        let mut g = Graph::<Fixed>::default();
        let x = g.leaf(Op::Cell(0));
        let ok1 = g.fold(Op::FragOk(Cut::Floors, 1), vec![x]);
        let frag1 = g.fold(Op::Frag(Cut::Floors, 1), vec![x]);
        let (out, map) = fold(&g, &[ok1, frag1], None);
        assert_eq!(out.get(map[ok1 as usize]).op, Op::FragOk(Cut::Floors, 1), "wide hull: undecided");
        assert_eq!(out.get(map[frag1 as usize]).op, Op::Frag(Cut::Floors, 1), "the fragment stays a fragment");

        // Inside one floor nothing straddles, so fragment 1 is never valid.
        let mut g = Graph::<Fixed>::default();
        let lo = g.leaf(Op::Const(65536 * 3 + 1000, 65536 * 3 + 1000));
        let hi = g.leaf(Op::Const(65536 * 3 + 50000, 65536 * 3 + 50000));
        let x = g.fold(Op::Span, vec![lo, hi]);
        let ok1 = g.fold(Op::FragOk(Cut::Floors, 1), vec![x]);
        let (out, map) = fold(&g, &[ok1], None);
        assert_eq!(out.get(map[ok1 as usize]).op, Op::ConstBool(false));
    }

    /// `Known` is a per-lane fact too: a hull spanning several floors contains
    /// lanes whose floor IS decided, so `Known(Flr(hull))` must stay undecided,
    /// not fold to false (which refuses every lane).
    #[test]
    fn known_of_a_wide_hull_is_undecided() {
        let mut g = Graph::<Fixed>::default();
        let x = g.leaf(Op::Cell(0));
        let fl = g.fold(Op::Flr, vec![x]);
        let known = g.fold(Op::Known, vec![fl]);
        let (out, map) = fold(&g, &[known], None);
        assert_eq!(out.get(map[known as usize]).op, Op::Known, "wide hull: undecided");

        // A point hull is decided everywhere.
        let mut g = Graph::<Fixed>::default();
        let p = g.leaf(Op::Const(65536 * 3 + 1000, 65536 * 3 + 1000));
        let fl = g.fold(Op::Flr, vec![p]);
        let known = g.fold(Op::Known, vec![fl]);
        let (out, map) = fold(&g, &[known], None);
        assert_eq!(out.get(map[known as usize]).op, Op::ConstBool(true));
    }

    /// Folding must not change what the graph computes (sampled).
    #[test]
    fn folding_does_not_change_what_the_graph_computes() {
        let mut g = Graph::<Fixed>::default();
        let x = g.leaf(Op::Cell(0));
        let y = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let mut roots = Vec::new();
        let zero = g.leaf(Op::Const(0, 0));
        let four = g.leaf(Op::Const(4 * 65536, 4 * 65536));
        let sum = g.fold(Op::Add, vec![x, y]);
        let ab = g.fold(Op::Abs, vec![sum]);
        let fl = g.fold(Op::Flr, vec![ab]);
        roots.push(g.fold(Op::Ge, vec![fl, zero]));
        roots.push(g.fold(Op::Lt, vec![ab, four]));
        roots.push(g.fold(Op::Sel, vec![b, ab, fl]));
        let ff = g.leaf(Op::ConstBool(false));
        roots.push(g.fold(Op::Or, vec![b, ff]));
        let (out, map) = fold(&g, &roots, None);
        for i in -3i16..4 {
            for j in -3i16..4 {
                for bv in [false, true] {
                    let mut cells: HashMap<u32, Val<Fixed>> = HashMap::new();
                    cells.insert(0, Val::exact_num(Pico8Num::from_i16(i)));
                    cells.insert(1, Val::exact_num(Pico8Num::from_i16(j)));
                    cells.insert(2, Val::Bool(Some(bv)));
                    let before = eval(&g, &cells, Lanes::Over, None);
                    let after = eval(&out, &cells, Lanes::Over, None);
                    for r in &roots {
                        assert_eq!(
                            before[*r as usize],
                            after[map[*r as usize] as usize],
                            "root {} at ({}, {}, {})",
                            r,
                            i,
                            j,
                            bv
                        );
                    }
                }
            }
        }
    }

    /// A range computed past the 16.16 range is the whole range (where the
    /// console's wrapped result lies), not PICO-8's wrapped endpoints
    /// (which bound nothing), and the operation does not fit.
    #[test]
    fn a_product_range_that_overflows_is_the_whole_range() {
        let mut g = Graph::<Fixed>::default();
        let raw = g.leaf(Op::Cell(0));
        let s = g.add(Op::Restrict(0, i32::MAX), vec![raw]);
        let two = g.leaf(Op::Const(0x2_0000, 0x2_0000));
        let m = g.add(Op::Mul, vec![s, two]);
        let mut f = Facts::default();
        assert_eq!(f.range(&g, m), Some(<Fixed as Arithmetic>::unknown()));
        assert!(!f.fits(&g, m));
        let t = g.add(Op::Restrict(0, 0x3fff_ffff), vec![raw]);
        let m = g.add(Op::Mul, vec![t, two]);
        assert_eq!(f.range(&g, m), Some(Pico8NumInterval::new(Pico8Num::from_raw(0), Pico8Num::from_raw(0x7fff_fffe))));
        assert!(f.fits(&g, m));
    }

    /// A select on an undecided condition is the hull of its arms; the
    /// tracer knows nothing of a cell but through its restriction.
    #[test]
    fn a_select_on_an_undecided_condition_is_the_hull_of_its_arms() {
        let mut g = Graph::<Fixed>::default();
        let raw = g.leaf(Op::Cell(0));
        let s = g.add(Op::Restrict(-(1 << 16), 1 << 16), vec![raw]);
        let five = g.leaf(Op::Const(5 << 16, 5 << 16));
        let cond = g.leaf(Op::Cell(1));
        g.set_cell_kind(1, crate::transpile::graph::Ty::Bool);
        let sel = g.add(Op::Sel, vec![cond, five, s]);
        let neg = g.add(Op::Neg, vec![sel]);
        let p = |v: i32| Pico8Num::from_raw(v);
        let mut f = Facts::default();
        assert_eq!(f.range(&g, sel), Some(Pico8NumInterval::new(p(-(1 << 16)), p(5 << 16))));
        assert_eq!(f.range(&g, neg), Some(Pico8NumInterval::new(p(-(5 << 16)), p(1 << 16))));
        let other = g.leaf(Op::Cell(2));
        let sum = g.add(Op::Add, vec![s, other]);
        assert_eq!(f.range(&g, sum), None);
        assert_eq!(f.range(&g, raw), None);
    }

    /// `Lo`/`Hi` of a hull are not points, so nothing may decide a comparison
    /// one lane could fail.
    #[test]
    fn the_ends_of_a_hull_are_not_points() {
        let mut g = Graph::<Fixed>::default();
        let v = g.leaf(Op::Cell(0));
        let hi = g.add(Op::Hi, vec![v]);
        let one = g.leaf(Op::Const(1 << 16, 1 << 16));
        let le = g.add(Op::Le, vec![hi, one]);
        let hull = Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(0), Pico8Num::from_raw(3 << 16)));
        let vals = eval(&g, &HashMap::from([(0u32, hull)]), Lanes::Over, None);
        assert_eq!(vals[hi as usize], hull, "Hi of a hull is the hull");
        assert_eq!(vals[le as usize], Val::Bool(None), "a lane [0, 0.5] has Hi <= 1, a lane [2, 3] does not");
        let r = g.add(Op::Restrict(0, 3 << 16), vec![v]);
        let hi_r = g.add(Op::Hi, vec![r]);
        assert_eq!(Facts::default().range(&g, hi_r), Some(Pico8NumInterval::new(Pico8Num::from_raw(0), Pico8Num::from_raw(3 << 16))));
        let le_r = g.add(Op::Le, vec![hi_r, one]);
        let (out, map) = fold(&g, &[le_r], None);
        assert_eq!(out.get(map[le_r as usize]).op, Op::Le, "the fold leaves it to the lanes");
    }
}
