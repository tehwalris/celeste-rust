//! What the graph IR needs of an arithmetic beyond `Arithmetic`
//! (plans/float-solver.md, milestone 5): the rules that are the
//! arithmetic's own. 16.16's are the graph's code as it always was, moved
//! here verbatim (the kernels' raw `i32` grid, wraps included); the
//! doubles' follow `LuaF64` and the reference engine (`refdomain`) exactly.
//!
//! - the FORK GRID: the integer floors a `move` fork's fragments are cut
//!   at (`Split`, `Frag`, `FragOk`, `SplitOk`, `IntFrag`);
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

use super::graph::{one_operand, Graph, NodeId, Op};

pub trait GraphArith: Arithmetic {
    /// Do `min` and `max` commute (bit for bit)?
    const MINMAX_COMMUTE: bool;
    /// Is addition exact, so `(x - r) + r` is `x` and `(r + c) - r` is `c`
    /// wherever the ranges bound them (16.16: yes; doubles round)?
    const EXACT_ADDITION: bool;
    /// A static range, as pieces (`pieces_of`).
    type Pieces: Clone + Debug + PartialEq;

    /// `SplitOk(n)` of the literal interval `[lo, hi]`, in the kernel's own
    /// arithmetic.
    fn lit_split_ok(lo: Self::Lit, hi: Self::Lit, n: u8) -> bool;
    /// `IntFrag(c)` of a literal whose low end is `lo`.
    fn lit_int_frag(lo: Self::Lit, c: u8) -> Self::Lit;
    /// `IntFrag(c)` over `iv`: fragment `c`'s low end, exact (`lenient`: its
    /// hull over the lanes `iv` covers).
    fn ev_int_frag(iv: Self::Ival, c: u8, lenient: bool) -> Self::Ival;
    /// `SplitOk(ways)` over `iv`: does it span at most `ways` floors?
    fn ev_split_ok(iv: Self::Ival, ways: u8) -> bool;
    /// The floors `iv` spans, minus one.
    fn ev_frag_cells(iv: Self::Ival) -> i64;
    /// Fragment `c` of `iv`: its part in the `c`-th floor from its low end's
    /// (`c` at most `ev_frag_cells`).
    fn ev_frag(iv: Self::Ival, c: u8) -> Self::Ival;
    /// `NoWrap(x)` decided at a fold, where it can be (`None`: it stays).
    fn fold_no_wrap(g: &Graph<Self>, x: NodeId) -> Option<bool>;
    /// `abs` of `x` (`defined`: a lane where it fails is in error, so the
    /// value need not cover it); `None` where it fails everywhere.
    fn ev_abs(x: Self::Ival, defined: bool) -> Option<Self::Ival>;
    /// The whole numbers a coordinate interval covers, clipped to `-1 ..=
    /// size` (`mget`'s rectangle).
    fn tile_span(iv: Self::Ival, size: i32) -> (i32, i32);

    /// The static range of `n` from the restrictions (`None`: unknown, never
    /// wrong). `memo` is valid for one graph.
    fn pieces_of(g: &Graph<Self>, memo: &mut HashMap<NodeId, Option<Self::Pieces>>, n: NodeId) -> Option<Self::Pieces>;
    /// Does every pair of pieces of `x` and `y` give comparison `op` (an
    /// `Op::Lt`..`Op::Eq`) one answer?
    fn decide_cmp(op: &Op<Self>, xs: &Self::Pieces, ys: &Self::Pieces) -> Option<bool>;
    /// Does the range keep every value inside the arithmetic (no overflow:
    /// `trace::error`)?
    fn bounded(p: &Self::Pieces) -> bool;
    /// The most floors a piece spans (`uncapped`: the hull's), at least 1.
    fn floors_spanned(p: &Self::Pieces, uncapped: bool) -> i64;
    /// A literal interval as raw 16.16, where the arithmetic is 16.16's
    /// (the literal splits of the levels with unknowns are so far).
    fn lit_raw_fixed(lo: Self::Lit, hi: Self::Lit) -> Option<(i32, i32)>;
}

// ---- 16.16 ----

/// One fork-grid step (the integers) in raw 16.16 units.
const GRID_STEP: i32 = 1 << 16;

/// The fork grid: one step in raw 16.16 and the mask flooring to it.
fn grid() -> (i32, i32) {
    (GRID_STEP, !(GRID_STEP - 1))
}

/// A static range as sorted, disjoint pieces (raw 16.16, inclusive).
pub type Pieces = Vec<(i64, i64)>;

/// The most pieces a range keeps; past that it is one hull.
pub const MAX_PIECES: usize = 64;

/// Sort, merge overlapping/adjacent, refuse a 16.16 overflow ("unknown"),
/// and cap the count.
pub fn normalize_pieces(mut v: Pieces) -> Option<Pieces> {
    v.retain(|(lo, hi)| lo <= hi);
    if v.is_empty() || v.iter().any(|(lo, hi)| *lo < i32::MIN as i64 || *hi > i32::MAX as i64) {
        return None;
    }
    v.sort_unstable();
    let mut out: Pieces = Vec::with_capacity(v.len());
    for (lo, hi) in v {
        match out.last_mut() {
            Some(last) if lo <= last.1 + 1 => last.1 = last.1.max(hi),
            _ => out.push((lo, hi)),
        }
    }
    if out.len() > MAX_PIECES {
        out = vec![(out[0].0, out[out.len() - 1].1)];
    }
    Some(out)
}

impl GraphArith for Fixed {
    const MINMAX_COMMUTE: bool = true;
    const EXACT_ADDITION: bool = true;
    type Pieces = Pieces;

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

    fn ev_int_frag(iv: Pico8NumInterval, c: u8, lenient: bool) -> Pico8NumInterval {
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

    fn ev_split_ok(iv: Pico8NumInterval, ways: u8) -> bool {
        let step = GRID_STEP as i64;
        let floor = |v: Pico8Num| (v.as_raw_u32() as i32 as i64).div_euclid(step) * step;
        floor(iv.high) <= floor(iv.low) + (ways as i64 - 1) * step
    }

    fn ev_frag_cells(iv: Pico8NumInterval) -> i64 {
        let (step, mask) = grid();
        let gflr = |p: Pico8Num| Pico8Num::from_raw(p.as_raw_u32() as i32 & mask);
        let (fl, fh) = (gflr(iv.low), gflr(iv.high));
        // Cells spanned minus one, in i64 (a full-range hull overflows).
        (fh.as_raw_u32() as i32 as i64 - fl.as_raw_u32() as i32 as i64) / step as i64
    }

    fn ev_frag(iv: Pico8NumInterval, c: u8) -> Pico8NumInterval {
        let (step, mask) = grid();
        let fl = (iv.low.as_raw_u32() as i32 & mask) as i64;
        let base = fl + c as i64 * step as i64;
        let lo = (iv.low.as_raw_u32() as i32 as i64).max(base);
        let hi = (iv.high.as_raw_u32() as i32 as i64).min(base + step as i64 - 1);
        Pico8NumInterval::new(Pico8Num::from_raw(lo as i32), Pico8Num::from_raw(hi as i32))
    }

    // No-wrap of anything but `+ - * /`, negation and `abs` holds (as in
    // `eval`). Of one with a single operand that is no literal point
    // (`one_operand`) it is decided where that operand's range is: every
    // value fits (`x + 0`, `x * 0.5`, `x / 2`), none does (`x / 0`), or the
    // operand is a literal. One with no literal operand YET stays: a
    // specialization may make it one.
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

    fn pieces_of(g: &Graph<Fixed>, memo: &mut HashMap<NodeId, Option<Pieces>>, n: NodeId) -> Option<Pieces> {
        if let Some(r) = memo.get(&n) {
            return r.clone();
        }
        let node = g.get(n);
        let (op, args) = (node.op.clone(), node.args.clone());
        let konst = |g: &Graph<Fixed>, n: NodeId| -> Option<Pico8Num> {
            match g.get(n).op {
                Op::Const(lo, hi) if lo == hi => Some(Pico8Num::from_raw(lo)),
                _ => None,
            }
        };
        let raw = |p: Pico8Num| p.as_raw_u32() as i32 as i64;
        let p8 = |v: i64| Pico8Num::from_raw(v as i32);
        let rec = |memo: &mut HashMap<NodeId, Option<Pieces>>, k: usize| Self::pieces_of(g, memo, args[k]);
        let r: Option<Pieces> = match op {
            Op::Const(lo, hi) => Some(vec![(lo as i64, hi as i64)]),
            // The assumed range (as `eval`): its own error makes it a fact.
            Op::Restrict(lo, hi) => {
                let (lo, hi) = (lo as i64, hi as i64);
                match rec(memo, 0) {
                    None => Some(vec![(lo, hi)]),
                    Some(a) => {
                        let inside: Pieces = a.iter().map(|p| (p.0.max(lo), p.1.min(hi))).filter(|p| p.0 <= p.1).collect();
                        if inside.is_empty() {
                            Some(a)
                        } else {
                            normalize_pieces(inside)
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
                            Op::Min => (x.0.min(y.0), x.1.min(y.1)),
                            _ => (x.0.max(y.0), x.1.max(y.1)),
                        });
                    }
                }
                normalize_pieces(out)
            }
            Op::Neg => normalize_pieces(rec(memo, 0)?.into_iter().map(|a| (-a.1, -a.0)).collect()),
            Op::Abs => normalize_pieces(
                rec(memo, 0)?
                    .into_iter()
                    .map(|a| {
                        let lo = if a.0 <= 0 && a.1 >= 0 { 0 } else { a.0.abs().min(a.1.abs()) };
                        (lo, a.0.abs().max(a.1.abs()))
                    })
                    .collect(),
            ),
            // Product/quotient by a CONSTANT is monotone in the other operand.
            Op::Mul | Op::Div => {
                let (ka, kb) = (konst(g, args[0]), konst(g, args[1]));
                let (k_of, k, is_div) = match (ka, kb) {
                    (_, Some(k)) => (0usize, k, matches!(op, Op::Div)),
                    (Some(k), None) if matches!(op, Op::Mul) => (1, k, false),
                    _ => {
                        memo.insert(n, None);
                        return None;
                    }
                };
                if is_div && k.as_raw_u32() == 0 {
                    memo.insert(n, None);
                    return None;
                }
                let a = rec(memo, k_of)?;
                // In i64, unwrapped and unsaturated: a product or quotient
                // that leaves the 16.16 range makes the range unknown
                // (`normalize_pieces`), where PICO-8's wrapped or saturated
                // endpoints would bound nothing (and a lane there is in error).
                let f = |x: i64| -> i64 {
                    if is_div {
                        (x << 16) / raw(k)
                    } else {
                        (x * raw(k)) >> 16
                    }
                };
                let mut out = Vec::new();
                for a in a {
                    let (x, y) = (f(a.0), f(a.1));
                    out.push((x.min(y), x.max(y)));
                }
                normalize_pieces(out)
            }
            Op::Flr => {
                let a = rec(memo, 0)?;
                normalize_pieces(a.into_iter().map(|a| (raw(p8(a.0).flr()), raw(p8(a.1).flr()))).collect())
            }
            Op::Sin => Some(vec![(-(1i64 << 16), 1i64 << 16)]),
            Op::Sel => match g.get(args[0]).op {
                Op::ConstBool(true) => rec(memo, 1),
                Op::ConstBool(false) => rec(memo, 2),
                _ => {
                    let (mut a, b) = (rec(memo, 1)?, rec(memo, 2)?);
                    a.extend(b);
                    normalize_pieces(a)
                }
            },
            Op::Span => {
                let (a, b) = (rec(memo, 0)?, rec(memo, 1)?);
                Some(vec![(a[0].0, b[b.len() - 1].1)])
            }
            // A fragment lies within its operand.
            Op::Split(_) | Op::SplitInt(_) | Op::Frag(_) | Op::IntFrag(_) => rec(memo, 0),
            // A lane's end, over the lanes (see `eval`'s `Lo`).
            Op::Lo | Op::Hi => {
                let low = matches!(op, Op::Lo);
                let arg = g.get(args[0]).clone();
                match arg.op {
                    Op::Span => Self::pieces_of(g, memo, arg.args[if low { 0 } else { 1 }]),
                    Op::Const(l, h) => {
                        let v = if low { l } else { h } as i64;
                        Some(vec![(v, v)])
                    }
                    _ => rec(memo, 0),
                }
            }
            _ => None,
        };
        memo.insert(n, r.clone());
        r
    }

    fn decide_cmp(op: &Op<Fixed>, xs: &Pieces, ys: &Pieces) -> Option<bool> {
        let one = |x: (i64, i64), y: (i64, i64)| -> Option<bool> {
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

    fn bounded(p: &Pieces) -> bool {
        p.iter().all(|&(lo, hi)| lo >= i32::MIN as i64 && hi <= i32::MAX as i64)
    }

    fn floors_spanned(ps: &Pieces, uncapped: bool) -> i64 {
        let sh = 16;
        let w = if uncapped {
            let lo = ps.iter().map(|p| p.0 >> sh).min().unwrap_or(0);
            let hi = ps.iter().map(|p| p.1 >> sh).max().unwrap_or(0);
            (hi - lo + 1).min(i64::from(u8::MAX))
        } else {
            ps.iter().map(|p| (p.1 >> sh) - (p.0 >> sh) + 1).max().unwrap_or(1)
        };
        w.max(1)
    }

    fn lit_raw_fixed(lo: i32, hi: i32) -> Option<(i32, i32)> {
        Some((lo, hi))
    }
}

/// One answer for every pair of pieces, or `None`.
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

// ---- doubles ----

/// A static range of doubles as sorted, disjoint closed pieces.
pub type FloatPieces = Vec<(f64, f64)>;

/// Sort, merge overlapping pieces, refuse a non-finite end ("unknown"), and
/// cap the count. No adjacency merge: the doubles between two pieces are
/// not in the range.
fn normalize_float(mut v: FloatPieces) -> Option<FloatPieces> {
    v.retain(|(lo, hi)| lo <= hi);
    if v.is_empty() || v.iter().any(|(lo, hi)| !lo.is_finite() || !hi.is_finite()) {
        return None;
    }
    v.sort_unstable_by(|a, b| a.0.total_cmp(&b.0).then(a.1.total_cmp(&b.1)));
    let mut out: FloatPieces = Vec::with_capacity(v.len());
    for (lo, hi) in v {
        match out.last_mut() {
            Some(last) if lo <= last.1 => last.1 = last.1.max(hi),
            _ => out.push((lo, hi)),
        }
    }
    if out.len() > MAX_PIECES {
        out = vec![(out[0].0, out[out.len() - 1].1)];
    }
    Some(out)
}

/// The next double below `x` (`x` finite).
fn below(x: f64) -> f64 {
    x.next_down()
}

impl GraphArith for Float {
    const MINMAX_COMMUTE: bool = false;
    const EXACT_ADDITION: bool = false;
    type Pieces = FloatPieces;

    fn lit_split_ok(lo: u64, hi: u64, n: u8) -> bool {
        Self::ev_split_ok(LuaF64Interval::new(Float::from_lit(lo), Float::from_lit(hi)), n)
    }

    fn lit_int_frag(lo: u64, c: u8) -> u64 {
        let v = Float::from_lit(lo).value();
        // Fragment `c`'s low end: the floor `c` integers above `lo`'s, at
        // least `lo` (exact: integers are doubles here).
        Float::lit(LuaF64::new((v.floor() + c as f64).max(v)).expect("finite"))
    }

    fn ev_int_frag(iv: LuaF64Interval, c: u8, lenient: bool) -> LuaF64Interval {
        let (lo, hi) = (iv.lo.value(), iv.hi.value());
        let base = lo.floor() + c as f64;
        let l = lo.max(base);
        let n = |v: f64| LuaF64::new(v).expect("finite");
        if lenient {
            let h = hi.max(hi.floor() + c as f64);
            LuaF64Interval::new(n(l), n(h.max(l)))
        } else {
            LuaF64Interval::point(n(l))
        }
    }

    fn ev_split_ok(iv: LuaF64Interval, ways: u8) -> bool {
        iv.hi.value().floor() - iv.lo.value().floor() <= (ways as f64 - 1.0)
    }

    fn ev_frag_cells(iv: LuaF64Interval) -> i64 {
        let d = iv.hi.value().floor() - iv.lo.value().floor();
        if d >= i64::MAX as f64 {
            i64::MAX
        } else {
            d as i64
        }
    }

    fn ev_frag(iv: LuaF64Interval, c: u8) -> LuaF64Interval {
        let (lo, hi) = (iv.lo.value(), iv.hi.value());
        let base = lo.floor() + c as f64;
        let n = |v: f64| LuaF64::new(v).expect("finite");
        LuaF64Interval::new(n(lo.max(base)), n(hi.min(below(base + 1.0))))
    }

    /// A double's `NoWrap` is "finite": decided only where the operation
    /// has no arithmetic to fail (the static ranges decide the rest, in
    /// `trace::error`).
    fn fold_no_wrap(g: &Graph<Float>, x: NodeId) -> Option<bool> {
        match g.get(x).op {
            Op::Add | Op::Sub | Op::Neg | Op::Abs | Op::Mul | Op::Div => None,
            _ => Some(true),
        }
    }

    fn ev_abs(x: LuaF64Interval, _defined: bool) -> Option<LuaF64Interval> {
        Some(x.abs())
    }

    fn tile_span(iv: LuaF64Interval, size: i32) -> (i32, i32) {
        let w = |v: f64| v.floor().clamp(-1.0, size as f64) as i32;
        (w(iv.lo.value()), w(iv.hi.value()))
    }

    fn pieces_of(g: &Graph<Float>, memo: &mut HashMap<NodeId, Option<FloatPieces>>, n: NodeId) -> Option<FloatPieces> {
        if let Some(r) = memo.get(&n) {
            return r.clone();
        }
        let node = g.get(n);
        let (op, args) = (node.op.clone(), node.args.clone());
        let v = |l: u64| Float::from_lit(l).value();
        let konst = |g: &Graph<Float>, n: NodeId| -> Option<f64> {
            match g.get(n).op {
                Op::Const(lo, hi) if lo == hi => Some(v(lo)),
                _ => None,
            }
        };
        let rec = |memo: &mut HashMap<NodeId, Option<FloatPieces>>, k: usize| Self::pieces_of(g, memo, args[k]);
        // Every operation below is monotone in each operand (or `abs`,
        // in its magnitude) and rounded to nearest at both ends, which is
        // monotone too: the ends' results bound every lane's (section 4).
        let r: Option<FloatPieces> = match op {
            Op::Const(lo, hi) => Some(vec![(v(lo), v(hi))]),
            Op::Restrict(lo, hi) => {
                let (lo, hi) = (v(lo), v(hi));
                match rec(memo, 0) {
                    None => Some(vec![(lo, hi)]),
                    Some(a) => {
                        let inside: FloatPieces = a.iter().map(|p| (p.0.max(lo), p.1.min(hi))).filter(|p| p.0 <= p.1).collect();
                        if inside.is_empty() {
                            Some(a)
                        } else {
                            normalize_float(inside)
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
                            Op::Min => (x.0.min(y.0), x.1.min(y.1)),
                            _ => (x.0.max(y.0), x.1.max(y.1)),
                        });
                    }
                }
                normalize_float(out)
            }
            Op::Neg => normalize_float(rec(memo, 0)?.into_iter().map(|a| (-a.1, -a.0)).collect()),
            Op::Abs => normalize_float(
                rec(memo, 0)?
                    .into_iter()
                    .map(|a| {
                        let lo = if a.0 <= 0.0 && a.1 >= 0.0 { 0.0 } else { a.0.abs().min(a.1.abs()) };
                        (lo, a.0.abs().max(a.1.abs()))
                    })
                    .collect(),
            ),
            Op::Mul | Op::Div => {
                let (ka, kb) = (konst(g, args[0]), konst(g, args[1]));
                let (k_of, k, is_div) = match (ka, kb) {
                    (_, Some(k)) => (0usize, k, matches!(op, Op::Div)),
                    (Some(k), None) if matches!(op, Op::Mul) => (1, k, false),
                    _ => {
                        memo.insert(n, None);
                        return None;
                    }
                };
                if is_div && k == 0.0 {
                    memo.insert(n, None);
                    return None;
                }
                let a = rec(memo, k_of)?;
                let f = |x: f64| if is_div { x / k } else { x * k };
                normalize_float(a.into_iter().map(|a| (f(a.0).min(f(a.1)), f(a.0).max(f(a.1)))).collect())
            }
            Op::Flr => normalize_float(rec(memo, 0)?.into_iter().map(|a| (a.0.floor(), a.1.floor())).collect()),
            Op::Sin => Some(vec![(-1.0, 1.0)]),
            Op::Sel => match g.get(args[0]).op {
                Op::ConstBool(true) => rec(memo, 1),
                Op::ConstBool(false) => rec(memo, 2),
                _ => {
                    let (mut a, b) = (rec(memo, 1)?, rec(memo, 2)?);
                    a.extend(b);
                    normalize_float(a)
                }
            },
            Op::Span => {
                let (a, b) = (rec(memo, 0)?, rec(memo, 1)?);
                Some(vec![(a[0].0, b[b.len() - 1].1)])
            }
            Op::Split(_) | Op::SplitInt(_) | Op::Frag(_) | Op::IntFrag(_) => rec(memo, 0),
            Op::Lo | Op::Hi => {
                let low = matches!(op, Op::Lo);
                let arg = g.get(args[0]).clone();
                match arg.op {
                    Op::Span => Self::pieces_of(g, memo, arg.args[if low { 0 } else { 1 }]),
                    Op::Const(l, h) => {
                        let x = v(if low { l } else { h });
                        Some(vec![(x, x)])
                    }
                    _ => rec(memo, 0),
                }
            }
            _ => None,
        };
        memo.insert(n, r.clone());
        r
    }

    fn decide_cmp(op: &Op<Float>, xs: &FloatPieces, ys: &FloatPieces) -> Option<bool> {
        let one = |x: (f64, f64), y: (f64, f64)| -> Option<bool> {
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

    /// Finite (`normalize_float` refuses the rest).
    fn bounded(p: &FloatPieces) -> bool {
        p.iter().all(|&(lo, hi)| lo.is_finite() && hi.is_finite())
    }

    fn floors_spanned(ps: &FloatPieces, uncapped: bool) -> i64 {
        let w = if uncapped {
            let lo = ps.iter().map(|p| p.0.floor()).fold(f64::INFINITY, f64::min);
            let hi = ps.iter().map(|p| p.1.floor()).fold(f64::NEG_INFINITY, f64::max);
            (hi - lo + 1.0).min(u8::MAX as f64)
        } else {
            ps.iter().map(|p| p.1.floor() - p.0.floor() + 1.0).fold(1.0, f64::max)
        };
        (w as i64).max(1)
    }

    fn lit_raw_fixed(_lo: u64, _hi: u64) -> Option<(i32, i32)> {
        None
    }
}
