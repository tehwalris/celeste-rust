//! The REFERENCE domain: a scalar abstract interpreter over the Lua AST, the
//! oracle the kernels are checked against.
//!
//! It plugs into `Interp<D>` like `Concrete` and `Symbolic`, but runs one
//! ABSTRACT scalar path and enumerates the fork tree by RE-EXECUTION
//! (`refdriver`). `Num` is an interval (a point is `[n,n]`); `Bool` is a
//! plain `bool`: every fork (a straddling comparison, an unknown button, a
//! `__split_by_flr` fragment) is resolved by a DECISION CURSOR, so `decide`
//! never returns `None` and `Interp` never merges.

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

/// How many distinct integer floors `v` spans.
fn floor_span(v: &Iv) -> u32 {
    let lo = v.low.flr().as_i16_or_err().unwrap_or(0);
    let hi = v.high.flr().as_i16_or_err().unwrap_or(0);
    (hi - lo + 1).max(1) as u32
}

fn floor_fragment(v: &Iv, k: u32) -> Iv {
    let base = v.low.flr().as_i16_or_err().unwrap_or(0) + k as i16;
    let frag_lo = P8::from_i16(base);
    // The top is `base+1 - eps`, not `base+1`: a fragment spans exactly one floor.
    let frag_hi_top = P8::from_i16(base + 1).next_smallest();
    let lo = if v.low > frag_lo { v.low } else { frag_lo };
    let hi = if v.high < frag_hi_top { v.high } else { frag_hi_top };
    Iv::new(lo, hi)
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
        // An unknown number (the whole 16.16 range) stays unknown: in wrapping
        // 16.16 every result is possible, as the kernels' `UnknownNum` folds.
        if is_unknown(a) || is_unknown(b) {
            return Ok(unknown());
        }
        Ok(match op {
            Arith::Add => a.checked_add(*b).ok_or_else(|| anyhow::anyhow!("interval add overflow"))?,
            Arith::Sub => a.checked_sub(*b).ok_or_else(|| anyhow::anyhow!("interval sub overflow"))?,
            Arith::Mul => match (a.to_number(), b.to_number()) {
                (Some(x), Some(y)) => Iv::from_number(x * y),
                (Some(x), None) if x >= P8::from_i16(0) => b.scale_positive(x),
                (None, Some(y)) if y >= P8::from_i16(0) => a.scale_positive(y),
                _ => bail!("interval mul: only point*point or interval*positive-scalar"),
            },
            Arith::Div => match (a.to_number(), b.to_number()) {
                (Some(x), Some(y)) => Iv::from_number(x / y),
                (None, Some(y)) if y > P8::from_i16(0) => a.div_positive(y),
                _ => bail!("interval div: only point/point or interval/positive-scalar"),
            },
            Arith::Rem => match (a.to_number(), b.to_number()) {
                (Some(x), Some(y)) => Iv::from_number(x % y),
                _ => bail!("interval rem: points only"),
            },
        })
    }

    fn fun1(&mut self, f: Fun1, a: &Iv) -> Result<Iv> {
        Ok(match f {
            Fun1::Neg => a.checked_neg().ok_or_else(|| anyhow::anyhow!("interval neg overflow"))?,
            Fun1::Abs => {
                let z = P8::from_i16(0);
                if a.low >= z {
                    *a
                } else if a.high <= z {
                    a.checked_neg().ok_or_else(|| anyhow::anyhow!("interval abs overflow"))?
                } else {
                    let nl = (z - a.low).max(a.high);
                    Iv::new(z, nl)
                }
            }
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
        // Eq: two points compare directly; a real interval is never equal to
        // anything.
        if op == Cmp::Eq {
            return Ok(match (a.to_number(), b.to_number()) {
                (Some(x), Some(y)) => x == y,
                _ => false,
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
}
