//! The REFERENCE domain: a scalar abstract interpreter over the Lua AST, the
//! oracle the kernels are checked against.
//!
//! It plugs into `Interp<D>` like `Concrete` and `Symbolic`, but runs one
//! ABSTRACT scalar path and enumerates the fork tree by RE-EXECUTION
//! (`refdriver`). `Num` is an interval (a point is `[n,n]`); `Bool` is a
//! plain `bool`: every fork (a straddling comparison, an unknown button, a
//! `__split_by_flr` fragment) is resolved by a DECISION CURSOR, so `decide`
//! never returns `None` and `Interp` never merges.
//!
//! OVERFLOW IS AN ERROR, for a point as for an interval: an operation whose
//! exact result can leave the 16.16 range (where PICO-8 wraps `+ - *` and
//! saturates `/` and `abs`) fails the frame, loudly, with the operands. The
//! game never overflows, so one is a modelling surprise to investigate; an
//! interval's overflow is the kernels' lane error too (`Op::NoWrap`).

use anyhow::{bail, Result};

use crate::pico8_num::{Pico8Num as P8, Pico8NumInterval as Iv};
use crate::trace::domain::{Arith, Cmp, Domain, Fun1, Fun2};

/// A depth-first cursor over a decision tree, enumerated by re-execution.
///
/// During a run, `choose(n)` is called at each fork (in a stable order): it
/// returns the recorded choice if this fork is already on the path, else it
/// appends `(0, n)` and returns 0. After a run reaches a leaf, `advance()`
/// bumps the deepest choice (popping exhausted ones) and returns whether any
/// path remains. `reset()` rewinds the position for the next run.
#[derive(Debug, Default)]
pub struct Cursor {
    /// The path: `(choice, count)` per fork, in encounter order.
    decisions: Vec<(u32, u32)>,
    /// Position within `decisions` during the current run.
    pos: usize,
}

impl Cursor {
    pub fn new() -> Self {
        Cursor::default()
    }

    /// Rewind for the next run (`advance` drops the stale tail).
    pub fn reset(&mut self) {
        self.pos = 0;
    }

    /// A fork with `n` options. `n <= 1` is not a choice (identity fork), so it
    /// is not recorded. Returns the choice index in `0..n`.
    pub fn choose(&mut self, n: u32) -> u32 {
        if n <= 1 {
            return 0;
        }
        let c = if self.pos < self.decisions.len() {
            let (c, cnt) = self.decisions[self.pos];
            debug_assert_eq!(cnt, n, "fork count changed across reruns (nondeterminism)");
            c
        } else {
            self.decisions.push((0, n));
            0
        };
        self.pos += 1;
        c
    }

    /// Advance to the next path DFS-style. Returns false when the tree is
    /// exhausted. Call after a run, before `reset()`.
    pub fn advance(&mut self) -> bool {
        // Drop forks the just-finished run did not reach (shorter path).
        self.decisions.truncate(self.pos);
        loop {
            match self.decisions.last_mut() {
                None => return false,
                Some((c, cnt)) => {
                    *c += 1;
                    if *c < *cnt {
                        return true;
                    }
                    self.decisions.pop();
                }
            }
        }
    }
}

/// The reference domain: the cursor is its only state.
#[derive(Debug, Default)]
pub struct RefDomain {
    pub cursor: Cursor,
}

impl RefDomain {
    pub fn new() -> Self {
        RefDomain { cursor: Cursor::new() }
    }
}

/// The unknown number: the whole 16.16 range.
fn unknown() -> Iv {
    Iv::new(P8::from_raw(i32::MIN), P8::from_raw(i32::MAX))
}

fn is_unknown(v: &Iv) -> bool {
    v.low == P8::from_raw(i32::MIN) && v.high == P8::from_raw(i32::MAX)
}

/// How many distinct integer floors `v` spans: 1 to 65536 (in `i32`: the
/// unknown number's 65536 floors overflow an `i16` difference).
fn floor_span(v: &Iv) -> u32 {
    (v.high.whole_part_as_i16() as i32 - v.low.whole_part_as_i16() as i32 + 1) as u32
}

/// The part of `v` in its `k`-th floor (`k < floor_span(v)`).
fn floor_fragment(v: &Iv, k: u32) -> Iv {
    let base = v.low.whole_part_as_i16() as i32 + k as i32;
    // `[base, base + 1 - eps]`: a fragment spans exactly one floor.
    let (floor, top) = (P8::from_raw(base << 16), P8::from_raw((base << 16) | 0xffff));
    Iv::new(v.low.max(floor), v.high.min(top))
}

/// A number for an error message: a point as its decimal value, an
/// interval as `[lo, hi]`.
pub fn show(v: &Iv) -> String {
    let dec = |n: P8| (n.to_bits() as i32) as f64 / 65536.0;
    match v.to_number() {
        Some(n) => format!("{}", dec(n)),
        None => format!("[{}, {}]", dec(v.low), dec(v.high)),
    }
}

/// `r`, or the overflow error naming the operation and its operands.
fn exact(r: Option<Iv>, op: impl FnOnce() -> String) -> Result<Iv> {
    r.ok_or_else(|| {
        anyhow::anyhow!(
            "reference arithmetic overflow: {} leaves the 16.16 range (PICO-8 would wrap or \
             saturate; the reference raises)",
            op()
        )
    })
}

impl Domain for RefDomain {
    type Num = Iv;
    type Bool = bool;

    fn num(&mut self, v: P8) -> Iv {
        Iv::from_number(v)
    }
    fn boolean(&mut self, b: bool) -> bool {
        b
    }

    fn arith(&mut self, op: Arith, a: &Iv, b: &Iv) -> Result<Iv> {
        // An unknown number (the whole 16.16 range) stays unknown, as the
        // kernels' `UnknownNum` folds: it stands for any value, not for a
        // range every value of which must fit.
        if is_unknown(a) || is_unknown(b) {
            return Ok(unknown());
        }
        // Points and intervals alike: the exact result, or the overflow error.
        let said = |sym: &str| format!("{} {sym} {}", show(a), show(b));
        match op {
            Arith::Add => exact(a.checked_add(*b), || said("+")),
            Arith::Sub => exact(a.checked_sub(*b), || said("-")),
            Arith::Mul => exact(a.checked_mul(*b), || said("*")),
            Arith::Div => exact(a.checked_div(*b), || said("/") + " (or divides by zero)"),
            // Never overflows: in `[0, |y|)`, or 0 for `y == 0`.
            Arith::Rem => match (a.to_number(), b.to_number()) {
                (Some(x), Some(y)) => Ok(Iv::from_number(x % y)),
                _ => bail!("interval rem {}: points only", said("%")),
            },
        }
    }

    fn fun1(&mut self, f: Fun1, a: &Iv) -> Result<Iv> {
        Ok(match f {
            Fun1::Neg => exact(a.checked_neg(), || format!("-{}", show(a)))?,
            Fun1::Abs => exact(a.checked_abs(), || format!("abs({})", show(a)))?,
            Fun1::Flr => {
                if floor_span(a) > 1 {
                    bail!("flr of an interval spanning >1 floor - call __split_by_flr first");
                }
                Iv::from_number(a.low.flr())
            }
            Fun1::Sin => match a.to_number() {
                Some(x) => Iv::from_number(x.pico8_sin()),
                None => Iv::new(P8::from_i16(-1), P8::from_i16(1)),
            },
        })
    }

    fn fun2(&mut self, f: Fun2, a: &Iv, b: &Iv) -> Result<Iv> {
        Ok(match f {
            Fun2::Min => Iv::new(a.low.min(b.low), a.high.min(b.high)),
            Fun2::Max => Iv::new(a.low.max(b.low), a.high.max(b.high)),
        })
    }

    fn compare(&mut self, op: Cmp, a: &Iv, b: &Iv) -> Result<bool> {
        // Eq: two points compare directly; disjoint intervals are unequal;
        // overlapping ones may be either (fork, as the orderings below).
        if op == Cmp::Eq {
            return Ok(match (a.to_number(), b.to_number()) {
                (Some(x), Some(y)) => x == y,
                _ if a.high < b.low || b.high < a.low => false,
                _ => self.cursor.choose(2) == 0,
            });
        }
        // Definite when the intervals are separated, else fork.
        let definite_true = match op {
            Cmp::Lt => a.high < b.low,
            Cmp::Le => a.high <= b.low,
            Cmp::Gt => a.low > b.high,
            Cmp::Ge => a.low >= b.high,
            Cmp::Eq => unreachable!(),
        };
        let definite_false = match op {
            Cmp::Lt => a.low >= b.high,
            Cmp::Le => a.low > b.high,
            Cmp::Gt => a.high <= b.low,
            Cmp::Ge => a.high < b.low,
            Cmp::Eq => unreachable!(),
        };
        Ok(if definite_true {
            true
        } else if definite_false {
            false
        } else {
            self.cursor.choose(2) == 0
        })
    }

    fn not(&mut self, a: &bool) -> bool {
        !a
    }
    fn and(&mut self, a: &bool, b: &bool) -> bool {
        *a && *b
    }
    fn or(&mut self, a: &bool, b: &bool) -> bool {
        *a || *b
    }

    fn decide(&self, c: &bool) -> Option<bool> {
        Some(*c)
    }

    fn sel_num(&mut self, _c: &bool, _t: &Iv, _f: &Iv) -> Iv {
        unreachable!("RefDomain never merges: decide is always Some");
    }
    fn sel_bool(&mut self, _c: &bool, _t: &bool, _f: &bool) -> bool {
        unreachable!("RefDomain never merges: decide is always Some");
    }

    fn mget(&mut self, _x: &Iv, _y: &Iv) -> Result<Iv> {
        // Concrete coords are folded by the caller; the cart splits interval
        // coords before collision.
        bail!("RefDomain::mget with non-const coords - TODO (should be split first)");
    }

    fn tile_flag_at(&mut self, _x: &Iv, _y: &Iv, _w: &Iv, _h: &Iv, _flag: &Iv) -> Result<bool> {
        bail!("RefDomain::tile_flag_at with non-const coords - TODO (collision wiring)");
    }

    fn unknown_bool(&mut self) -> Result<bool> {
        Ok(self.cursor.choose(2) == 1)
    }

    fn range_num(&mut self, lo: P8, hi: P8) -> Result<Iv> {
        Ok(Iv::new(lo, hi))
    }

    fn as_const(&self, v: &Iv) -> Option<P8> {
        v.to_number()
    }

    fn is_interval(&self, v: &Iv) -> bool {
        v.to_number().is_none()
    }

    fn describe(&self, v: &Iv) -> String {
        match v.to_number() {
            Some(n) => format!("{:?}", n),
            None => format!("[{:?}, {:?}]", v.low, v.high),
        }
    }

    fn fork_flr(&mut self, v: &Iv, _ways: u8) -> (Iv, bool) {
        let n = floor_span(v);
        let k = self.cursor.choose(n);
        (floor_fragment(v, k), true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cursor_enumerates_a_two_by_three_tree_depth_first() {
        let mut cur = Cursor::new();
        let mut leaves = Vec::new();
        loop {
            cur.reset();
            let a = cur.choose(2);
            let b = cur.choose(3);
            leaves.push((a, b));
            if !cur.advance() {
                break;
            }
        }
        assert_eq!(
            leaves,
            vec![(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]
        );
    }

    #[test]
    fn cursor_handles_a_path_dependent_tree() {
        // The second fork exists only when the first chose 0: three leaves.
        let mut cur = Cursor::new();
        let mut leaves: Vec<Vec<u32>> = Vec::new();
        loop {
            cur.reset();
            let mut path = vec![cur.choose(2)];
            if path[0] == 0 {
                path.push(cur.choose(2));
            }
            leaves.push(path);
            if !cur.advance() {
                break;
            }
        }
        assert_eq!(leaves, vec![vec![0, 0], vec![0, 1], vec![1]]);
    }

    #[test]
    fn one_way_fork_is_not_a_decision() {
        let mut cur = Cursor::new();
        cur.reset();
        assert_eq!(cur.choose(1), 0);
        assert!(!cur.advance());
    }

    #[test]
    fn straddling_compare_forks_both_ways() {
        let mut results = Vec::new();
        let mut d = RefDomain::new();
        loop {
            d.cursor.reset();
            let a = Iv::new(P8::from_i16(3), P8::from_i16(7));
            let b = Iv::from_number(P8::from_i16(5));
            results.push(d.compare(Cmp::Lt, &a, &b).unwrap());
            if !d.cursor.advance() {
                break;
            }
        }
        assert_eq!(results, vec![true, false]);
    }

    #[test]
    fn separated_compare_is_definite_no_fork() {
        let mut d = RefDomain::new();
        d.cursor.reset();
        let a = Iv::new(P8::from_i16(1), P8::from_i16(2));
        let b = Iv::from_number(P8::from_i16(5));
        assert!(d.compare(Cmp::Lt, &a, &b).unwrap());
        assert!(!d.cursor.advance(), "no fork should have been recorded");
    }

    fn raw(v: i32) -> Iv {
        Iv::from_number(P8::from_raw(v))
    }

    fn overflows(r: Result<Iv>, what: &str) {
        let e = format!("{:#}", r.expect_err(what));
        assert!(e.contains("reference arithmetic overflow"), "{what}: {e}");
    }

    /// Every operation that can leave the 16.16 range RAISES, for a point
    /// and for an interval: `+ - * /`, negation and `abs`, an interval times
    /// a positive scalar (which panicked) and a division by zero; nothing
    /// wraps or saturates. Off the edge a point is `Pico8Num`'s result.
    #[test]
    fn every_overflow_raises_for_points_and_intervals() {
        let mut d = RefDomain::new();
        let (max, min) = (raw(i32::MAX), raw(i32::MIN));
        let one = Iv::from_number(P8::from_i16(1));
        let two = Iv::from_number(P8::from_i16(2));
        let near_top = Iv::new(P8::from_i16(30000), P8::from_i16(32767));
        let near_bottom = Iv::new(P8::from_i16(-32768), P8::from_i16(-30000));
        for (op, a, b, what) in [
            (Arith::Add, max, raw(1), "MAX + eps"),
            (Arith::Add, near_top, Iv::from_number(P8::from_i16(1000)), "[30000, 32767] + 1000"),
            (Arith::Sub, min, raw(1), "MIN - eps"),
            (Arith::Sub, Iv::from_number(P8::from_i16(0)), near_bottom, "0 - [-32768, -30000]"),
            (Arith::Mul, Iv::from_number(P8::from_i16(200)), Iv::from_number(P8::from_i16(200)), "200 * 200"),
            (Arith::Mul, near_top, two, "[30000, 32767] * 2"),
            (Arith::Mul, two, near_bottom, "2 * [-32768, -30000]"),
            (Arith::Mul, near_top, Iv::from_number(P8::from_i16(-2)), "[30000, 32767] * -2"),
            (Arith::Div, Iv::from_number(P8::from_i16(20000)), raw(0x8000), "20000 / 0.5"),
            (Arith::Div, near_top, raw(0x8000), "[30000, 32767] / 0.5"),
            (Arith::Div, one, Iv::from_number(P8::from_i16(0)), "1 / 0"),
            (Arith::Div, one, Iv::new(P8::from_i16(-1), P8::from_i16(1)), "1 / [-1, 1]"),
        ] {
            overflows(d.arith(op, &a, &b), what);
        }
        overflows(d.fun1(Fun1::Neg, &min), "-MIN");
        overflows(d.fun1(Fun1::Neg, &near_bottom), "-[-32768, -30000]");
        overflows(d.fun1(Fun1::Abs, &min), "abs(MIN)");
        overflows(d.fun1(Fun1::Abs, &Iv::new(P8::from_raw(i32::MIN), P8::from_i16(5))), "abs([MIN, 5])");
        assert!(!d.cursor.advance(), "an overflow forks nothing");

        // Just inside the edges: the console's value, a point a point.
        let x = P8::from_raw(i32::MAX - 1);
        assert_eq!(d.arith(Arith::Add, &raw(i32::MAX - 1), &raw(1)).unwrap(), Iv::from_number(x + P8::from_raw(1)));
        assert_eq!(d.arith(Arith::Sub, &raw(i32::MIN + 1), &raw(1)).unwrap(), min);
        assert_eq!(d.fun1(Fun1::Neg, &max).unwrap(), raw(-i32::MAX));
        assert_eq!(d.fun1(Fun1::Abs, &raw(i32::MIN + 1)).unwrap(), max);
        let (a, b) = (P8::from_i16(181), P8::from_i16(181));
        assert_eq!(d.arith(Arith::Mul, &Iv::from_number(a), &Iv::from_number(b)).unwrap(), Iv::from_number(a * b));
        assert_eq!(d.arith(Arith::Div, &min, &one).unwrap(), min, "MIN / 1 fits");
        // An interval by a positive scalar, and by zero (which panicked).
        assert_eq!(d.arith(Arith::Mul, &two, &Iv::new(P8::from_i16(-3), P8::from_i16(4))).unwrap(), Iv::new(P8::from_i16(-6), P8::from_i16(8)));
        let zero = Iv::from_number(P8::from_i16(0));
        assert_eq!(d.arith(Arith::Mul, &near_top, &zero).unwrap(), zero);
        assert_eq!(d.arith(Arith::Div, &near_top, &two).unwrap(), Iv::new(P8::from_i16(15000), P8::from_raw(0x3fff_8000)));
    }

    /// A range of more than 32767 floors: `flr` refuses it (it is no
    /// point), and `__split_by_flr` forks every floor - the unknown number
    /// spans 65536, which an `i16` difference wrapped to one.
    #[test]
    fn a_range_of_many_floors_is_not_one_floor() {
        let mut d = RefDomain::new();
        assert_eq!(floor_span(&unknown()), 65536);
        assert_eq!(floor_span(&Iv::new(P8::from_i16(-20000), P8::from_i16(20000))), 40001);
        assert!(d.fun1(Fun1::Flr, &unknown()).is_err(), "flr of the unknown number is no point");
        assert_eq!(floor_fragment(&unknown(), 0), Iv::new(P8::from_raw(i32::MIN), P8::from_raw(i32::MIN | 0xffff)));
        assert_eq!(floor_fragment(&unknown(), 65535), Iv::new(P8::from_i16(32767), P8::from_raw(i32::MAX)));
        assert_eq!(floor_fragment(&Iv::new(P8::from_raw(-0x1_8000), P8::from_raw(0x8000)), 1), Iv::new(P8::from_i16(-1), P8::from_raw(-1)));
        d.cursor.reset();
        let (frag, _) = d.fork_flr(&Iv::new(P8::from_i16(-20000), P8::from_i16(20000)), 0);
        assert_eq!(frag, Iv::new(P8::from_i16(-20000), P8::from_raw((-20000 << 16) | 0xffff)));
        assert_eq!(d.cursor.decisions, vec![(0, 40001)]);
    }
}
