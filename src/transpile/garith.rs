//! What the graph IR needs of an arithmetic beyond `Arithmetic`
//! (plans/float.md, "The kernels' safety"): the rules that are the
//! arithmetic's own. 16.16's are the kernels' raw `i32` grid, wraps
//! included; the doubles' follow `LuaF64` and the reference engine
//! (`refdomain`) exactly.
//!
//! - the FORK GRID: the integer floors a `move` fork's fragments are cut
//!   at (`Op::Fork` and its family, `Cut`);
//! - `NoWrap`, an operation's own error: 16.16's result left the range
//!   (the console wraps or saturates); a double's is not finite;
//! - `min`/`max`'s symmetry: LuaJIT returns the second argument on a tie,
//!   which tells `-0` from `+0`, so the doubles' do not commute;
//! - the STATIC RANGES (`pieces_of`): from the restrictions through the
//!   arithmetic, as pieces; and what they decide.

use std::collections::HashMap;
use std::fmt::Debug;

use celeste_core::arith::{Arithmetic, Fixed, Float};
use celeste_core::lua_f64::{LuaF64, LuaF64Interval};
use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};

use super::graph::{one_operand, Cut, Graph, NodeId, Op};

pub trait GraphArith: Arithmetic {
    const MINMAX_COMMUTE: bool;
    /// Is addition exact, so `(x - r) + r` is `x` and `(r + c) - r` is `c`
    /// wherever the ranges bound them (16.16: yes; doubles round)?
    const EXACT_ADDITION: bool;
    /// Is the cart's `==` on numbers bit identity (16.16: yes; a double's
    /// `-0 == 0`)? Where not, a pin checks `Op::Same`.
    const EQ_IS_IDENTITY: bool;
    /// A static range's piece end (`End`).
    type End: End;

    /// `ForkOk(Floors, n)` of the literal interval `[lo, hi]`, in the
    /// kernel's own arithmetic.
    fn lit_split_ok(lo: Self::Lit, hi: Self::Lit, n: u8) -> bool;
    fn lit_int_frag(lo: Self::Lit, c: u8) -> Self::Lit;
    /// An `Ints` fork's fragment `c` over `iv`: the low end of its part in
    /// the `c`-th floor, exact (`lenient`: its hull over the lanes `iv`
    /// covers).
    fn int_fragment(iv: Self::Ival, c: u8, lenient: bool) -> Self::Ival;

    /// The cells of `cut`'s grid `iv` spans past its low end's (`i64::MAX`
    /// past counting): fragment `c` is reached iff `c <= cells`.
    fn cells(cut: Cut, iv: Self::Ival) -> i64 {
        let n = match cut.grid() {
            Cut::Rounds => Self::rounds(iv).ok().flatten(),
            _ => Self::floors(iv),
        };
        n.map_or(i64::MAX, |k| k as i64 - 1)
    }

    /// Fragment `c` of `iv` (reached), cut as `cut` says: its part in the
    /// `c`-th cell (`Ints`: `int_fragment`).
    fn fragment(cut: Cut, iv: Self::Ival, c: u8, lenient: bool) -> anyhow::Result<Self::Ival> {
        Ok(match cut {
            Cut::Floors => Self::floor_fragment(iv, c as u32),
            Cut::Ints => Self::int_fragment(iv, c, lenient),
            Cut::Rounds => Self::round_fragment(iv, c as u32)?,
        })
    }
    /// `NoWrap(x)` decided at a fold, where it can be (`None`: it stays).
    fn fold_no_wrap(g: &Graph<Self>, x: NodeId) -> Option<bool>;
    /// `abs` of `x` (`defined`: a lane where it fails is in error, so the
    /// value need not cover it); `None` where it fails everywhere.
    fn ev_abs(x: Self::Ival, defined: bool) -> Option<Self::Ival>;
    /// The whole numbers a coordinate interval covers, clipped to `-1 ..=
    /// size` (`mget`'s rectangle).
    fn tile_span(iv: Self::Ival, size: i32) -> (i32, i32);

    /// A literal as a piece end.
    fn end(l: Self::Lit) -> Self::End;
    /// A piece end as a literal (`None`: outside the arithmetic).
    fn end_lit(e: Self::End) -> Option<Self::Lit>;
    /// `x` times or (`div`) over a constant `k` (not zero), as the
    /// arithmetic rounds it (16.16: in `i64`, unwrapped and unsaturated, so
    /// a result outside the range makes the range unknown at `normalize`).
    fn scale(x: Self::End, k: Self::End, div: bool) -> Self::End;

    /// The static range of `n` from the restrictions (`None`: unknown, never
    /// wrong). `memo` is valid for one graph.
    fn pieces_of(g: &Graph<Self>, memo: &mut HashMap<NodeId, Option<Pieces<Self>>>, n: NodeId) -> Option<Pieces<Self>> {
        pieces_under(g, memo, n, &[])
    }
    /// Does every pair of pieces of `x` and `y` give comparison `op` (an
    /// `Op::Lt`..`Op::Eq`) one answer?
    fn decide_cmp(op: &Op<Self>, xs: &Pieces<Self>, ys: &Pieces<Self>) -> Option<bool> {
        let one = |x: (Self::End, Self::End), y: (Self::End, Self::End)| -> Option<bool> {
            match op {
                Op::Lt => if x.1 < y.0 { Some(true) } else if x.0 >= y.1 { Some(false) } else { None },
                Op::Le => if x.1 <= y.0 { Some(true) } else if x.0 > y.1 { Some(false) } else { None },
                Op::Gt => if x.0 > y.1 { Some(true) } else if x.1 <= y.0 { Some(false) } else { None },
                Op::Ge => if x.0 >= y.1 { Some(true) } else if x.1 < y.0 { Some(false) } else { None },
                _ => if x.0 == x.1 && y.0 == y.1 && x.0 == y.0 { Some(true) } else if x.1 < y.0 || y.1 < x.0 { Some(false) } else { None },
            }
        };
        decide_pairs(xs, ys, one)
    }
    /// Does the range keep every value inside the arithmetic (no overflow:
    /// `trace::error`)?
    fn bounded(p: &Pieces<Self>) -> bool {
        p.iter().all(|&(lo, hi)| lo.valid() && hi.valid())
    }
    /// The most cells of `grid` (the floors, the values of `flr(x + 1/2)`)
    /// a piece spans (`uncapped`: the hull's), at least 1: a fork's arity.
    fn spanned(grid: Cut, p: &Pieces<Self>, uncapped: bool) -> i64;
    /// The whole numbers the pieces' hull spans.
    fn floor_hull(p: &Pieces<Self>) -> Option<(i64, i64)> {
        let lo = p.iter().map(|q| q.0).reduce(End::least)?.pixel();
        let hi = p.iter().map(|q| q.1).reduce(End::most)?.pixel();
        Some((lo, hi))
    }
    /// The pieces' hull as literals (`None`: outside the arithmetic).
    fn lit_hull(p: &Pieces<Self>) -> Option<(Self::Lit, Self::Lit)> {
        let (lo, hi) = (p.iter().map(|q| q.0).reduce(End::least)?, p.iter().map(|q| q.1).reduce(End::most)?);
        Some((Self::end_lit(lo)?, Self::end_lit(hi)?))
    }
    /// A literal interval as raw 16.16, where the arithmetic is 16.16's.
    fn lit_raw_fixed(lo: Self::Lit, hi: Self::Lit) -> Option<(i32, i32)>;
    /// Is `v` the only number of its VALUE, so a box whose ends are `v`
    /// holds `v`'s bits on every lane (`ival`'s point fold)? A double's
    /// zero is not: `-0 == 0`, and a lane of `-0` lies in `[0, 0]`.
    fn alone_in_its_value(v: Self) -> bool;
    /// The kernel computing `roots` of `g`, as assembly (`transpile::asm`:
    /// the arithmetic's lowering of each node and its lane layout).
    fn compile_kernel(g: &Graph<Self>, roots: &[NodeId], sym: &str, opts: super::asm::AsmOptions) -> anyhow::Result<super::asm::Compiled>;
    /// Can `op` leave the arithmetic's range (`NoWrap`, its own error,
    /// `trace::error`)? 16.16: `+ - * /`, negation and `abs` wrap or
    /// saturate. Doubles: `+ - * /` and `%` can leave the finite doubles
    /// (`%` by zero is NaN); negation and `abs` of a finite double cannot.
    fn range_error(op: &Op<Self>) -> bool;
}

/// Every number of `v`'s VALUE, as an interval: `v` alone wherever it is
/// alone in its value, a double's zero both zeros. An input a compile-time
/// decision pins to a value (a pixel, a platform's `x`) is pinned to this: a
/// comparison cannot tell `-0` from `0`, a quotient can.
pub fn value_box<A: GraphArith>(v: A) -> A::Ival {
    if A::alone_in_its_value(v) {
        return A::point(v);
    }
    let w = A::neg(v).expect("negating a value alone in neither sign stays in range");
    let (lo, hi) = if v.to_f64().total_cmp(&w.to_f64()).is_le() { (v, w) } else { (w, v) };
    A::ival(lo, hi)
}

#[cfg(test)]
mod value_box_tests {
    /// A value's box: the point where the value has one number (16.16
    /// always), both zeros for a double's zero.
    #[test]
    fn a_doubles_zero_boxes_both_zeros() {
        use celeste_core::arith::{Arithmetic, Fixed, Float};
        use celeste_core::lua_f64::LuaF64;
        let f = |v: f64| LuaF64::new(v).expect("finite");
        assert_eq!(super::value_box::<Fixed>(Fixed::from_i16(0)), Fixed::point(Fixed::from_i16(0)));
        assert_eq!(super::value_box::<Float>(f(3.0)), Float::point(f(3.0)));
        for z in [0.0, -0.0] {
            let (lo, hi) = Float::bounds(super::value_box::<Float>(f(z)));
            assert_eq!((lo.value().to_bits(), hi.value().to_bits()), ((-0.0f64).to_bits(), 0.0f64.to_bits()), "{z:?}");
        }
    }
}

/// A static range as sorted, disjoint pieces (inclusive ends).
pub type Pieces<A = Fixed> = Vec<(<A as GraphArith>::End, <A as GraphArith>::End)>;

/// The most pieces a range keeps; past that it is one hull.
pub const MAX_PIECES: usize = 64;

/// A static range's piece end: 16.16's raw value in `i64` (exact and
/// unwrapped: a result past the range is seen, and refused, at
/// `normalize`), a double's value. Each operation is the arithmetic's own
/// on the ends: monotone (`abs`: in the magnitude), and a double's rounded
/// to nearest at both ends, monotone too, so the ends' results bound every
/// lane's.
pub trait End: Copy + PartialOrd + Debug + Send + std::ops::Add<Output = Self> + std::ops::Sub<Output = Self> + std::ops::Neg<Output = Self> {
    const ZERO: Self;
    /// The arithmetic's least and greatest values.
    const MIN: Self;
    const MAX: Self;
    /// Inside the arithmetic (16.16's `i32` raws; the finite doubles).
    fn valid(self) -> bool;
    /// Does a piece starting at `lo` join one ending at `hi` (16.16: also
    /// the next raw; doubles: overlapping only, the doubles between two
    /// pieces are not in the range)?
    fn joins(lo: Self, hi: Self) -> bool;
    fn order(a: Self, b: Self) -> std::cmp::Ordering;
    fn least(self, o: Self) -> Self;
    fn most(self, o: Self) -> Self;
    fn magnitude(self) -> Self;
    /// `flr`.
    fn flr(self) -> Self;
    /// The whole number `flr` of the end is.
    fn pixel(self) -> i64;
}

/// Sort, merge overlapping (`End::joins`) pieces, refuse an end outside the
/// arithmetic ("unknown"), and cap the count.
fn normalize<E: End>(mut v: Vec<(E, E)>) -> Option<Vec<(E, E)>> {
    v.retain(|(lo, hi)| lo <= hi);
    if v.is_empty() || v.iter().any(|(lo, hi)| !lo.valid() || !hi.valid()) {
        return None;
    }
    v.sort_unstable_by(|a, b| E::order(a.0, b.0).then(E::order(a.1, b.1)));
    let mut out: Vec<(E, E)> = Vec::with_capacity(v.len());
    for (lo, hi) in v {
        match out.last_mut() {
            Some(last) if E::joins(lo, last.1) => last.1 = last.1.most(hi),
            _ => out.push((lo, hi)),
        }
    }
    if out.len() > MAX_PIECES {
        out = vec![(out[0].0, out[out.len() - 1].1)];
    }
    Some(out)
}

/// What a branch says about a value: node `.0` lies in `[.1, .2]`.
pub type Fact<A> = (NodeId, <A as GraphArith>::End, <A as GraphArith>::End);

/// `GraphArith::pieces_of` (no `facts`), once for both arithmetics; under
/// `facts`, each node a fact names is narrowed to it (a node with no range
/// of its own takes the fact's), and a node no lane reaches (contradicting
/// facts, or an operand that is) is the empty range. `memo` is valid for
/// one graph and one set of facts.
fn pieces_under<A: GraphArith>(g: &Graph<A>, memo: &mut HashMap<NodeId, Option<Pieces<A>>>, n: NodeId, facts: &[Fact<A>]) -> Option<Pieces<A>> {
    if let Some(r) = memo.get(&n) {
        return r.clone();
    }
    let node = g.get(n);
    let (op, args) = (node.op.clone(), node.args.clone());
    let konst = |n: NodeId| match g.get(n).op {
        Op::Const(lo, hi) if lo == hi => Some(A::end(lo)),
        _ => None,
    };
    let rec = |memo: &mut HashMap<NodeId, Option<Pieces<A>>>, k: usize| pieces_under(g, memo, args[k], facts);
    let unreached = !facts.is_empty() && !matches!(op, Op::Sel) && (0..args.len()).any(|k| rec(memo, k).is_some_and(|p| p.is_empty()));
    let r: Option<Pieces<A>> = if unreached { Some(Vec::new()) } else { match op {
        Op::Const(lo, hi) => Some(vec![(A::end(lo), A::end(hi))]),
        // The assumed range (as `eval`): its own error makes it a fact.
        Op::Restrict(lo, hi) => {
            let (lo, hi) = (A::end(lo), A::end(hi));
            match rec(memo, 0) {
                None => Some(vec![(lo, hi)]),
                Some(a) => {
                    let inside: Pieces<A> = a.iter().map(|p| (p.0.most(lo), p.1.least(hi))).filter(|p| p.0 <= p.1).collect();
                    if inside.is_empty() {
                        Some(a)
                    } else {
                        normalize(inside)
                    }
                }
            }
        }
        Op::Add | Op::Sub | Op::Min | Op::Max => {
            let (a, b) = (rec(memo, 0)?, rec(memo, 1)?);
            let mut out = Vec::new();
            for x in &a {
                for y in &b {
                    out.push(match op {
                        Op::Add => (x.0 + y.0, x.1 + y.1),
                        Op::Sub => (x.0 - y.1, x.1 - y.0),
                        Op::Min => (x.0.least(y.0), x.1.least(y.1)),
                        _ => (x.0.most(y.0), x.1.most(y.1)),
                    });
                }
            }
            normalize(out)
        }
        Op::Neg => normalize(rec(memo, 0)?.into_iter().map(|a| (-a.1, -a.0)).collect()),
        Op::Abs => normalize(
            rec(memo, 0)?
                .into_iter()
                .map(|a| {
                    let lo = if a.0 <= A::End::ZERO && a.1 >= A::End::ZERO { A::End::ZERO } else { a.0.magnitude().least(a.1.magnitude()) };
                    (lo, a.0.magnitude().most(a.1.magnitude()))
                })
                .collect(),
        ),
        // Product/quotient by a CONSTANT is monotone in the other operand.
        Op::Mul | Op::Div => {
            let (k_of, k, is_div) = match (konst(args[0]), konst(args[1])) {
                (_, Some(k)) => (0usize, k, matches!(op, Op::Div)),
                (Some(k), None) if matches!(op, Op::Mul) => (1, k, false),
                _ => {
                    memo.insert(n, None);
                    return None;
                }
            };
            if is_div && k == A::End::ZERO {
                memo.insert(n, None);
                return None;
            }
            let a = rec(memo, k_of)?;
            normalize(
                a.into_iter()
                    .map(|a| {
                        let (x, y) = (A::scale(a.0, k, is_div), A::scale(a.1, k, is_div));
                        (x.least(y), x.most(y))
                    })
                    .collect(),
            )
        }
        Op::Flr => normalize(rec(memo, 0)?.into_iter().map(|a| (a.0.flr(), a.1.flr())).collect()),
        Op::Sin => {
            let one = A::end(A::lit(A::from_i16(1)));
            Some(vec![(-one, one)])
        }
        Op::Sel => match g.get(args[0]).op {
            Op::ConstBool(true) => rec(memo, 1),
            Op::ConstBool(false) => rec(memo, 2),
            _ => {
                let (mut a, b) = (rec(memo, 1)?, rec(memo, 2)?);
                a.extend(b);
                if a.is_empty() {
                    Some(a)
                } else {
                    normalize(a)
                }
            }
        },
        Op::Span => {
            let (a, b) = (rec(memo, 0)?, rec(memo, 1)?);
            Some(vec![(a[0].0, b[b.len() - 1].1)])
        }
        // A fragment lies within its operand.
        Op::Fork(..) | Op::Frag(..) => rec(memo, 0),
        // A lane's end, over the lanes (see `eval`'s `Lo`).
        Op::Lo | Op::Hi => {
            let low = matches!(op, Op::Lo);
            let arg = g.get(args[0]).clone();
            match arg.op {
                Op::Span => pieces_under(g, memo, arg.args[if low { 0 } else { 1 }], facts),
                Op::Const(l, h) => {
                    let v = A::end(if low { l } else { h });
                    Some(vec![(v, v)])
                }
                _ => rec(memo, 0),
            }
        }
        _ => None,
    } };
    let mut r = r;
    for &(_, lo, hi) in facts.iter().filter(|f| f.0 == n) {
        let p = r.unwrap_or_else(|| vec![(A::End::MIN, A::End::MAX)]);
        r = Some(p.into_iter().map(|(a, b)| (a.most(lo), b.least(hi))).filter(|(a, b)| a <= b).collect());
    }
    memo.insert(n, r.clone());
    r
}

/// Does `v` provably lie in `[lo, hi]`? Its static range (`pieces_of`),
/// and through select arms on `x op k` (`k` a literal) with what each
/// answer says about `x` (so a wrap `x < -16 ? 128 : ...` is bounded).
/// `false` if unsure.
pub fn within<A: GraphArith>(g: &Graph<A>, v: NodeId, lo: A, hi: A) -> bool {
    let (lo, hi) = (A::end(A::lit(lo)), A::end(A::lit(hi)));
    within_under(g, v, (lo, hi), &mut Vec::new())
}

fn within_under<A: GraphArith>(g: &Graph<A>, v: NodeId, (lo, hi): (A::End, A::End), facts: &mut Vec<Fact<A>>) -> bool {
    if pieces_under(g, &mut HashMap::new(), v, facts).is_some_and(|p| p.iter().all(|&(a, b)| lo <= a && b <= hi)) {
        return true;
    }
    let node = g.get(v);
    if node.op != Op::Sel {
        return false;
    }
    let (c, t, f) = (node.args[0], node.args[1], node.args[2]);
    let Some((x, yes, no)) = comparison_facts(g, c) else {
        return within_under(g, t, (lo, hi), facts) && within_under(g, f, (lo, hi), facts);
    };
    facts.push((x, yes.0, yes.1));
    let a = within_under(g, t, (lo, hi), facts);
    facts.pop();
    facts.push((x, no.0, no.1));
    let b = within_under(g, f, (lo, hi), facts);
    facts.pop();
    a && b
}

/// `c` as `x op k` (`k` a literal point, either side): `x` and its range
/// `(when true, when false)`; `k`'s neighbours are the arithmetic's
/// (`Arithmetic::below`, and above as `-below(-k)`).
fn comparison_facts<A: GraphArith>(g: &Graph<A>, c: NodeId) -> Option<(NodeId, (A::End, A::End), (A::End, A::End))> {
    let node = g.get(c);
    let point = |n: NodeId| match g.get(n).op {
        Op::Const(a, b) if a == b => Some(A::from_lit(a)),
        _ => None,
    };
    if !matches!(node.op, Op::Lt | Op::Le | Op::Gt | Op::Ge) {
        return None;
    }
    // `k op x` is `x op' k` with the operator mirrored.
    let (x, k, op) = match (point(node.args[0]), point(node.args[1])) {
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
    let end = |v: A| A::end(A::lit(v));
    let pred = end(A::below(k)?);
    let succ = end(A::below(A::neg(k)?).and_then(A::neg)?);
    let (k, (min, max)) = (end(k), (A::End::MIN, A::End::MAX));
    Some(match op {
        Op::Lt => (x, (min, pred), (k, max)),
        Op::Le => (x, (min, k), (succ, max)),
        Op::Gt => (x, (succ, max), (min, k)),
        _ => (x, (k, max), (min, pred)),
    })
}

// 16.16.

/// 16.16's raw value, exact and unwrapped.
impl End for i64 {
    const ZERO: i64 = 0;
    const MIN: i64 = i32::MIN as i64;
    const MAX: i64 = i32::MAX as i64;
    fn valid(self) -> bool {
        (<i64 as End>::MIN..=<i64 as End>::MAX).contains(&self)
    }
    fn joins(lo: i64, hi: i64) -> bool {
        lo <= hi + 1
    }
    fn order(a: i64, b: i64) -> std::cmp::Ordering {
        a.cmp(&b)
    }
    fn least(self, o: i64) -> i64 {
        Ord::min(self, o)
    }
    fn most(self, o: i64) -> i64 {
        Ord::max(self, o)
    }
    fn magnitude(self) -> i64 {
        i64::abs(self)
    }
    fn flr(self) -> i64 {
        Pico8Num::from_raw(self as i32).flr().as_raw_u32() as i32 as i64
    }
    fn pixel(self) -> i64 {
        self.div_euclid(1 << 16)
    }
}

/// One fork-grid step (the integers) in raw 16.16 units.
const GRID_STEP: i32 = 1 << 16;

/// The fork grid: one step in raw 16.16 and the mask flooring to it.
fn grid() -> (i32, i32) {
    (GRID_STEP, !(GRID_STEP - 1))
}

impl GraphArith for Fixed {
    const MINMAX_COMMUTE: bool = true;
    const EXACT_ADDITION: bool = true;
    const EQ_IS_IDENTITY: bool = true;
    type End = i64;

    fn end(l: i32) -> i64 {
        l as i64
    }
    fn end_lit(e: i64) -> Option<i32> {
        i32::try_from(e).ok()
    }
    fn scale(x: i64, k: i64, div: bool) -> i64 {
        if div {
            (x << 16) / k
        } else {
            (x * k) >> 16
        }
    }

    fn lit_split_ok(lo: i32, hi: i32, n: u8) -> bool {
        // In the kernel's own i32 arithmetic, wrap included.
        let step = GRID_STEP;
        let (fl, fh) = (lo & !(step - 1), hi & !(step - 1));
        let top = fl.wrapping_add(step.wrapping_mul(n as i32 - 1));
        fh <= top
    }

    fn lit_int_frag(lo: i32, c: u8) -> i32 {
        let (step, mask) = grid();
        let base = (lo & mask) as i64 + c as i64 * step as i64;
        (lo as i64).max(base).clamp(i32::MIN as i64, i32::MAX as i64) as i32
    }

    fn int_fragment(iv: Pico8NumInterval, c: u8, lenient: bool) -> Pico8NumInterval {
        let (step, mask) = grid();
        let gflr = |p: Pico8Num| p.as_raw_u32() as i32 as i64 & mask as i64;
        let (fl, fh) = (gflr(iv.low), gflr(iv.high));
        // Clamped: the full-range hull's high end plus a step would wrap.
        let clamp = |v: i64| v.clamp(i32::MIN as i64, i32::MAX as i64) as i32;
        let base = fl + c as i64 * step as i64;
        let lo = clamp((iv.low.as_raw_u32() as i32 as i64).max(base));
        if lenient {
            let hi = clamp((iv.high.as_raw_u32() as i32 as i64).max(fh + c as i64 * step as i64));
            Pico8NumInterval::new(Pico8Num::from_raw(lo), Pico8Num::from_raw(hi.max(lo)))
        } else {
            Pico8NumInterval::new(Pico8Num::from_raw(lo), Pico8Num::from_raw(lo))
        }
    }

    // No-wrap of anything but `+ - * /`, negation and `abs` holds (as in
    // `eval`). Of one with a single operand that is no literal point it is
    // decided where every value fits (`x + 0`, `x * 0.5`), none does (`x / 0`),
    // or the operand is a literal; one with no literal operand YET stays.
    fn fold_no_wrap(g: &Graph<Fixed>, x: NodeId) -> Option<bool> {
        match g.get(x).op {
            Op::Add | Op::Sub | Op::Neg | Op::Abs | Op::Mul | Op::Div => {
                let o = one_operand(g, x)?;
                let operand = g.get(x).args[o.operand];
                match (o.fits, &g.get(operand).op) {
                    (None, _) => Some(false),
                    (Some((i32::MIN, i32::MAX)), _) => Some(true),
                    (Some((lo, hi)), Op::Const(a, b)) => Some(lo <= *a && *b <= hi),
                    _ => None,
                }
            }
            _ => Some(true),
        }
    }

    // `defined`: a lane holding MIN is in error (`abs`'s own), so the value
    // is the hull over the others, in [0, MAX].
    fn ev_abs(mut x: Pico8NumInterval, defined: bool) -> Option<Pico8NumInterval> {
        let min = Pico8Num::from_raw(i32::MIN);
        if defined && x.low == min && x.high > min {
            x.low = Pico8Num::from_raw(i32::MIN + 1);
        }
        x.checked_abs()
    }

    fn tile_span(iv: Pico8NumInterval, size: i32) -> (i32, i32) {
        let (lo, hi) = (iv.low.flr().whole_part_as_i16() as i32, iv.high.flr().whole_part_as_i16() as i32);
        (lo.clamp(-1, size), hi.clamp(-1, size))
    }

    /// The floors: `raw >> 16`; the values of `flr(x + 1/2)`: `(raw + 1/2)
    /// >> 16`, exactly.
    fn spanned(grid: Cut, ps: &Pieces, uncapped: bool) -> i64 {
        let half = if grid == Cut::Rounds { 0x8000 } else { 0 };
        let r = |v: i64| (v + half) >> 16;
        let w = if uncapped {
            let lo = ps.iter().map(|p| r(p.0)).min().unwrap_or(0);
            let hi = ps.iter().map(|p| r(p.1)).max().unwrap_or(0);
            (hi - lo + 1).min(i64::from(u8::MAX))
        } else {
            ps.iter().map(|p| r(p.1) - r(p.0) + 1).max().unwrap_or(1)
        };
        w.max(1)
    }

    fn lit_raw_fixed(lo: i32, hi: i32) -> Option<(i32, i32)> {
        Some((lo, hi))
    }
    fn alone_in_its_value(_v: Pico8Num) -> bool {
        true
    }
    fn compile_kernel(g: &Graph<Fixed>, roots: &[NodeId], sym: &str, opts: super::asm::AsmOptions) -> anyhow::Result<super::asm::Compiled> {
        super::asm::compile_fixed(g, roots, sym, opts)
    }
    fn range_error(op: &Op<Fixed>) -> bool {
        matches!(op, Op::Add | Op::Sub | Op::Neg | Op::Abs | Op::Mul | Op::Div)
    }
}

fn decide_pairs<P: Copy>(xs: &[P], ys: &[P], one: impl Fn(P, P) -> Option<bool>) -> Option<bool> {
    let mut decided: Option<Option<bool>> = None;
    for x in xs {
        for y in ys {
            let r = one(*x, *y);
            match decided {
                None => decided = Some(r),
                Some(p) if p == r => {}
                _ => return None,
            }
        }
    }
    decided.flatten()
}

// Doubles.

/// A double's value.
impl End for f64 {
    const ZERO: f64 = 0.0;
    const MIN: f64 = f64::MIN;
    const MAX: f64 = f64::MAX;
    fn valid(self) -> bool {
        self.is_finite()
    }
    fn joins(lo: f64, hi: f64) -> bool {
        lo <= hi
    }
    fn order(a: f64, b: f64) -> std::cmp::Ordering {
        a.total_cmp(&b)
    }
    fn least(self, o: f64) -> f64 {
        f64::min(self, o)
    }
    fn most(self, o: f64) -> f64 {
        f64::max(self, o)
    }
    fn magnitude(self) -> f64 {
        f64::abs(self)
    }
    fn flr(self) -> f64 {
        f64::floor(self)
    }
    fn pixel(self) -> i64 {
        self.floor() as i64
    }
}

/// An `Ints` fragment `c` of a value: its low end, the floor `c` integers
/// above `v`'s, at least `v` (the cart's `max`, as the reference takes it).
fn int_frag_low(v: LuaF64, c: u8) -> LuaF64 {
    let base = LuaF64::new(v.value().floor() + c as f64).expect("a floor near a finite double is finite");
    v.lua_max(base)
}

impl GraphArith for Float {
    const MINMAX_COMMUTE: bool = false;
    const EXACT_ADDITION: bool = false;
    const EQ_IS_IDENTITY: bool = false;
    type End = f64;

    fn end(l: u64) -> f64 {
        Float::from_lit(l).value()
    }
    fn end_lit(e: f64) -> Option<u64> {
        LuaF64::new(e).map(Float::lit)
    }
    fn scale(x: f64, k: f64, div: bool) -> f64 {
        if div {
            x / k
        } else {
            x * k
        }
    }

    fn lit_split_ok(lo: u64, hi: u64, n: u8) -> bool {
        Self::cells(Cut::Floors, LuaF64Interval::new(Float::from_lit(lo), Float::from_lit(hi))) < n as i64
    }

    fn lit_int_frag(lo: u64, c: u8) -> u64 {
        Float::lit(int_frag_low(Float::from_lit(lo), c))
    }

    fn int_fragment(iv: LuaF64Interval, c: u8, lenient: bool) -> LuaF64Interval {
        let l = int_frag_low(iv.lo, c);
        if lenient {
            let h = int_frag_low(iv.hi, c).lua_max(l);
            LuaF64Interval::new(l, h)
        } else {
            LuaF64Interval::point(l)
        }
    }

    /// A double's `NoWrap` is "finite": undecided exactly where the
    /// operation can leave the finite doubles (`range_error`: `%` by zero
    /// included).
    fn fold_no_wrap(g: &Graph<Float>, x: NodeId) -> Option<bool> {
        if Self::range_error(&g.get(x).op) {
            None
        } else {
            Some(true)
        }
    }

    fn ev_abs(x: LuaF64Interval, _defined: bool) -> Option<LuaF64Interval> {
        Some(x.abs())
    }

    fn tile_span(iv: LuaF64Interval, size: i32) -> (i32, i32) {
        let w = |v: f64| v.floor().clamp(-1.0, size as f64) as i32;
        (w(iv.lo.value()), w(iv.hi.value()))
    }

    /// The floors; the values of `flr(fl(x + 1/2))`, monotone in `x`
    /// (`LuaF64Interval::rounds`).
    fn spanned(grid: Cut, ps: &Pieces<Float>, uncapped: bool) -> i64 {
        let half = if grid == Cut::Rounds { 0.5 } else { 0.0 };
        let r = |v: f64| (v + half).floor();
        let w = if uncapped {
            let lo = ps.iter().map(|p| r(p.0)).fold(f64::INFINITY, f64::min);
            let hi = ps.iter().map(|p| r(p.1)).fold(f64::NEG_INFINITY, f64::max);
            (hi - lo + 1.0).min(u8::MAX as f64)
        } else {
            ps.iter().map(|p| r(p.1) - r(p.0) + 1.0).fold(1.0, f64::max)
        };
        (w as i64).max(1)
    }

    fn lit_raw_fixed(_lo: u64, _hi: u64) -> Option<(i32, i32)> {
        None
    }
    fn alone_in_its_value(v: LuaF64) -> bool {
        v.value() != 0.0
    }
    fn compile_kernel(g: &Graph<Float>, roots: &[NodeId], sym: &str, opts: super::asm::AsmOptions) -> anyhow::Result<super::asm::Compiled> {
        super::asm::compile_float(g, roots, sym, opts)
    }
    fn range_error(op: &Op<Float>) -> bool {
        matches!(op, Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem)
    }
}
