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
//! - where an overflow lands (`CLOSED`), `abs`'s saturation
//!   (`console_abs`): what the one evaluation (`transpile::eval`) reads.

use celeste_core::arith::{Arithmetic, Fixed, Float};
use celeste_core::lua_f64::{LuaF64, LuaF64Interval};
use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};

use super::graph::{Cut, Graph, NodeId, Op};

pub trait GraphArith: Arithmetic {
    const MINMAX_COMMUTE: bool;
    /// Is addition exact, so `(x - r) + r` is `x` and `(r + c) - r` is `c`
    /// wherever the ranges bound them (16.16: yes; doubles round)?
    const EXACT_ADDITION: bool;
    /// Is the cart's `==` on numbers bit identity (16.16: yes; a double's
    /// `-0 == 0`)? Where not, a pin checks `Op::Same`.
    const EQ_IS_IDENTITY: bool;
    /// Does the console's result of an operation that leaves the range stay
    /// one of the arithmetic's numbers (16.16 wraps or saturates; a double
    /// may become non-finite)?
    const CLOSED: bool;

    /// `ForkOk(Floors, n)` of the literal interval `[lo, hi]`, in the
    /// kernel's own arithmetic.
    fn lit_split_ok(lo: Self::Lit, hi: Self::Lit, n: u8) -> bool;
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
    /// `abs` of `x` as the console computes it (16.16's `abs(-32768)`
    /// saturates to the greatest number, `abs(-32767.99998)`'s); `None` of
    /// that one point alone, the error a literal fold must not hide.
    fn console_abs(x: Self::Ival) -> Option<Self::Ival>;
    /// The whole numbers a coordinate interval covers, clipped to `-1 ..=
    /// size` (`mget`'s rectangle).
    fn tile_span(iv: Self::Ival, size: i32) -> (i32, i32);

    /// The most cells of `grid` (the floors, the values of `flr(x + 1/2)`)
    /// a value of `iv` spans, at least 1, at most `u8::MAX`: a fork's arity.
    fn spanned(grid: Cut, iv: Self::Ival) -> i64 {
        Self::cells(grid, iv).saturating_add(1).clamp(1, u8::MAX as i64)
    }
    /// The whole numbers `iv` spans.
    fn floor_hull(iv: Self::Ival) -> (i64, i64) {
        let (lo, hi) = Self::bounds(iv);
        (Self::flr(lo).to_f64() as i64, Self::flr(hi).to_f64() as i64)
    }
    /// Is `v` the only number of its VALUE, so a box whose ends are `v`
    /// holds `v`'s bits on every lane (`ival`'s point fold)? A double's
    /// zero is not: `-0 == 0`, and a lane of `-0` lies in `[0, 0]`.
    fn alone_in_its_value(v: Self) -> bool;
    /// The kernel computing `roots` of `g`, as assembly (`transpile::asm`:
    /// the arithmetic's lowering of each node and its lane layout).
    fn compile_kernel(g: &Graph<Self>, roots: &[NodeId], sym: &str, provenance: bool) -> anyhow::Result<super::asm::Compiled>;
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

// 16.16.

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
    const CLOSED: bool = true;

    fn lit_split_ok(lo: i32, hi: i32, n: u8) -> bool {
        // In the kernel's own i32 arithmetic, wrap included.
        let step = GRID_STEP;
        let (fl, fh) = (lo & !(step - 1), hi & !(step - 1));
        let top = fl.wrapping_add(step.wrapping_mul(n as i32 - 1));
        fh <= top
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

    fn console_abs(mut x: Pico8NumInterval) -> Option<Pico8NumInterval> {
        let min = Pico8Num::from_raw(i32::MIN);
        if x.low == min && x.high > min {
            x.low = Pico8Num::from_raw(i32::MIN + 1);
        }
        x.checked_abs()
    }

    fn tile_span(iv: Pico8NumInterval, size: i32) -> (i32, i32) {
        let (lo, hi) = (iv.low.flr().whole_part_as_i16() as i32, iv.high.flr().whole_part_as_i16() as i32);
        (lo.clamp(-1, size), hi.clamp(-1, size))
    }

    fn alone_in_its_value(_v: Pico8Num) -> bool {
        true
    }
    fn compile_kernel(g: &Graph<Fixed>, roots: &[NodeId], sym: &str, provenance: bool) -> anyhow::Result<super::asm::Compiled> {
        super::asm::compile_fixed(g, roots, sym, provenance)
    }
    fn range_error(op: &Op<Fixed>) -> bool {
        matches!(op, Op::Add | Op::Sub | Op::Neg | Op::Abs | Op::Mul | Op::Div)
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
    const CLOSED: bool = false;

    fn lit_split_ok(lo: u64, hi: u64, n: u8) -> bool {
        Self::cells(Cut::Floors, LuaF64Interval::new(Float::from_lit(lo), Float::from_lit(hi))) < n as i64
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

    fn console_abs(x: LuaF64Interval) -> Option<LuaF64Interval> {
        Some(x.abs())
    }

    fn tile_span(iv: LuaF64Interval, size: i32) -> (i32, i32) {
        let w = |v: f64| v.floor().clamp(-1.0, size as f64) as i32;
        (w(iv.lo.value()), w(iv.hi.value()))
    }

    fn alone_in_its_value(v: LuaF64) -> bool {
        v.value() != 0.0
    }
    fn compile_kernel(g: &Graph<Float>, roots: &[NodeId], sym: &str, provenance: bool) -> anyhow::Result<super::asm::Compiled> {
        super::asm::compile_float(g, roots, sym, provenance)
    }
    fn range_error(op: &Op<Float>) -> bool {
        matches!(op, Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem)
    }
}
