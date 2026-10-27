//! The doubles' ops as LLVM IR (`codegen::float`'s, bit for bit): IEEE
//! `fadd`..`fdiv` without fast-math flags (one rounding each, never fused:
//! `LLC_FLAGS`' `-fp-contract=off`), `flr` as `llvm.floor`, LuaJIT's
//! `min`/`max` as a compare and a select (the SECOND argument on a tie),
//! negation and `abs` on the sign bit, `sin` a call-out; an interval's ends
//! as the reference takes them.

use super::*;

const SIGN: u64 = 0x8000_0000_0000_0000;
const MAGNITUDE: u64 = 0x7fff_ffff_ffff_ffff;

fn k(v: f64) -> Num {
    Num::Q(v.to_bits())
}

impl Emit<'_, Float> {
    fn fbin(&mut self, op: &str, a: Num, b: Num) -> Num {
        Num::Reg(ins!(self, "{op} <8 x double> {}, {}", n(a), n(b)))
    }

    fn floor(&mut self, a: Num) -> Num {
        Num::Reg(ins!(self, "call <8 x double> @llvm.floor.v8f64(<8 x double> {})", n(a)))
    }

    /// An ordered compare.
    fn fcmp(&mut self, pred: &str, a: Num, b: Num) -> Mask {
        Mask::Reg(ins!(self, "fcmp {pred} <8 x double> {}, {}", n(a), n(b)))
    }

    /// A plane's bits as `<8 x i64>`.
    fn bits(&mut self, a: Num) -> String {
        match a {
            Num::Q(b) => format!("splat (i64 {})", b as i64),
            Num::Reg(r) => format!("%v{}", ins!(self, "bitcast <8 x double> %v{r} to <8 x i64>")),
            Num::I32(_) => unreachable!("a 16.16 literal in the doubles' kernel"),
        }
    }

    /// A 64-bit integer op on the bits.
    fn qbin(&mut self, op: &str, a: Num, b: Num) -> Num {
        let (x, y) = (self.bits(a), self.bits(b));
        let r = ins!(self, "{op} <8 x i64> {x}, {y}");
        Num::Reg(ins!(self, "bitcast <8 x i64> %v{r} to <8 x double>"))
    }

    fn same_bits(&mut self, a: Num, b: Num) -> Mask {
        let (x, y) = (self.bits(a), self.bits(b));
        Mask::Reg(ins!(self, "icmp eq <8 x i64> {x}, {y}"))
    }

    /// LuaJIT's `min(a, b)`: `a` if `a < b`, else `b`.
    fn lua_min(&mut self, a: Num, b: Num) -> Num {
        if a == b {
            return a;
        }
        let lt = self.fcmp("olt", a, b);
        self.n_sel(lt, a, b)
    }

    /// LuaJIT's `max(a, b)`: `a` if `a > b`, else `b`.
    fn lua_max(&mut self, a: Num, b: Num) -> Num {
        if a == b {
            return a;
        }
        let gt = self.fcmp("ogt", a, b);
        self.n_sel(gt, a, b)
    }

    /// The lesser in the TOTAL order (`-0` below `0`): a tie's bits OR to
    /// the negative zero (or are one value).
    fn least(&mut self, a: Num, b: Num) -> Num {
        if a == b {
            return a;
        }
        let lt = self.fcmp("olt", a, b);
        let gt = self.fcmp("olt", b, a);
        let tie = self.qbin("or", a, b);
        let inner = self.n_sel(gt, b, tie);
        self.n_sel(lt, a, inner)
    }

    /// The greater in the total order: a tie's bits AND to the positive zero.
    fn most(&mut self, a: Num, b: Num) -> Num {
        if a == b {
            return a;
        }
        let gt = self.fcmp("ogt", a, b);
        let lt = self.fcmp("ogt", b, a);
        let tie = self.qbin("and", a, b);
        let inner = self.n_sel(lt, b, tie);
        self.n_sel(gt, a, inner)
    }

    fn fneg(&mut self, a: Num) -> Num {
        self.qbin("xor", a, Num::Q(SIGN))
    }

    fn fabs(&mut self, a: Num) -> Num {
        self.qbin("and", a, Num::Q(MAGNITUDE))
    }

    /// `|a| <= MAX`: false for an infinity and a NaN.
    fn finite(&mut self, a: Num) -> Mask {
        let m = self.fabs(a);
        self.fcmp("ole", m, k(f64::MAX))
    }

    /// The next double below a finite `x`.
    fn next_down(&mut self, x: Num) -> Num {
        let pos = self.fcmp("ogt", x, k(0.0));
        let neg = self.fcmp("olt", x, k(0.0));
        let dec = self.qbin("sub", x, Num::Q(1));
        let inc = self.qbin("add", x, Num::Q(1));
        let off_zero = self.n_sel(neg, inc, Num::Q(0x8000_0000_0000_0001));
        self.n_sel(pos, dec, off_zero)
    }

    /// The next double above `x`.
    fn next_up(&mut self, x: Num) -> Num {
        let pos = self.fcmp("ogt", x, k(0.0));
        let neg = self.fcmp("olt", x, k(0.0));
        let inc = self.qbin("add", x, Num::Q(1));
        let dec = self.qbin("sub", x, Num::Q(1));
        let off_zero = self.n_sel(neg, dec, Num::Q(1));
        self.n_sel(pos, inc, off_zero)
    }

    /// `flr(fl(x + 1/2))`.
    fn round_of(&mut self, x: Num) -> Num {
        let h = self.fbin("fadd", x, k(0.5));
        self.floor(h)
    }

    /// `LuaF64Interval::round_cut(t)`: the first double `c` with
    /// `round_of(c) >= t`, from `t - 1/2`, at most four moves; the lanes it
    /// settles on.
    fn round_cut(&mut self, t: Num) -> (Num, Mask) {
        let mut c = self.fbin("fsub", t, k(0.5));
        let mut done = Mask::Const(false);
        for _ in 0..4 {
            let r = self.round_of(c);
            let up = self.fcmp("olt", r, t);
            let pc = self.next_down(c);
            let rp = self.round_of(pc);
            let down = self.fcmp("oge", rp, t);
            let nu = self.next_up(c);
            let moved_down = self.n_sel(down, pc, c);
            let next = self.n_sel(up, nu, moved_down);
            let moves = self.m_or(up, down);
            let settles = self.m_not(moves);
            c = self.n_sel(done, c, next);
            done = self.m_or(done, settles);
        }
        (c, done)
    }

    /// `LuaF64Interval::floor_fragment(c)`, and whether `a` reaches it.
    fn floor_fragment(&mut self, a: [Num; 2], c: u8) -> ([Num; 2], Mask) {
        let fl = self.floor(a[0]);
        let base = self.fbin("fadd", fl, k(c as f64));
        let lo = self.lua_max(a[0], base);
        let next = self.fbin("fadd", base, k(1.0));
        let top = self.next_down(next);
        let hi = self.lua_min(a[1], top);
        let cells = self.floor_cells(a);
        let valid = self.fcmp("oge", cells, k(c as f64));
        ([lo, hi], valid)
    }

    /// `floor(hi) - floor(lo)`.
    fn floor_cells(&mut self, a: [Num; 2]) -> Num {
        let fl = self.floor(a[0]);
        let fh = self.floor(a[1]);
        self.fbin("fsub", fh, fl)
    }

    /// `round_of(hi) - round_of(lo)`.
    fn round_cells(&mut self, a: [Num; 2]) -> Num {
        let rl = self.round_of(a[0]);
        let rh = self.round_of(a[1]);
        self.fbin("fsub", rh, rl)
    }

    /// `LuaF64Interval::round_fragment(c)`, whether `a` reaches it, and
    /// whether both cuts settled.
    fn round_fragment(&mut self, a: [Num; 2], c: u8) -> ([Num; 2], Mask, Mask) {
        let rl = self.round_of(a[0]);
        let t = self.fbin("fadd", rl, k(c as f64));
        let t1 = self.fbin("fadd", t, k(1.0));
        let (cut_lo, s_lo) = self.round_cut(t);
        let (cut_hi, s_hi) = self.round_cut(t1);
        let lo = self.lua_max(a[0], cut_lo);
        let top = self.next_down(cut_hi);
        let hi = self.lua_min(a[1], top);
        let cells = self.round_cells(a);
        let valid = self.fcmp("oge", cells, k(c as f64));
        let settled = self.m_and(s_lo, s_hi);
        ([lo, hi], valid, settled)
    }

    /// An interval product or quotient: the four corners, the least and the
    /// most in the total order.
    fn corners(&mut self, op: &str, a: [Num; 2], b: [Num; 2]) -> [Num; 2] {
        let c = [self.fbin(op, a[0], b[0]), self.fbin(op, a[0], b[1]), self.fbin(op, a[1], b[0]), self.fbin(op, a[1], b[1])];
        let (l0, l1) = (self.least(c[0], c[1]), self.least(c[2], c[3]));
        let (h0, h1) = (self.most(c[0], c[1]), self.most(c[2], c[3]));
        let lo = self.least(l0, l1);
        [lo, self.most(h0, h1)]
    }

    /// Both ends finite (one for a number).
    fn finite_value(&mut self, x: NodeId) -> Result<Mask> {
        Ok(match self.vals[x as usize] {
            Some(Value::Num(v)) => self.finite(v),
            Some(Value::Ival([lo, hi])) => {
                let (fl, fh) = (self.finite(lo), self.finite(hi));
                self.m_and(fl, fh)
            }
            _ => bail!("node {}: not a number where one was needed", x),
        })
    }

    /// A compare of two numbers, or tri-state over intervals.
    fn compare(&mut self, op: &Op<Float>, a: [NodeId; 2]) -> Result<Value> {
        if self.wide(a[0]) || self.wide(a[1]) {
            let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
            let (t, f) = match op {
                Op::Lt => (self.fcmp("olt", x[1], y[0]), self.fcmp("oge", x[0], y[1])),
                Op::Le => (self.fcmp("ole", x[1], y[0]), self.fcmp("ogt", x[0], y[1])),
                Op::Gt => (self.fcmp("ogt", x[0], y[1]), self.fcmp("ole", x[1], y[0])),
                _ => (self.fcmp("oge", x[0], y[1]), self.fcmp("olt", x[1], y[0])),
            };
            let known = self.m_or(t, f);
            return Ok(Value::Bool([t, known]));
        }
        let pred = match op {
            Op::Lt => "olt",
            Op::Le => "ole",
            Op::Gt => "ogt",
            _ => "oge",
        };
        let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
        Ok(Value::Bool([self.fcmp(pred, x, y), Mask::Const(true)]))
    }

    pub(super) fn lower_node_f64(&mut self, id: NodeId) -> Result<()> {
        if let Some(v) = self.lower_common(id)? {
            self.vals[id as usize] = Some(v);
            return Ok(());
        }
        let node = self.g.get(id);
        let a = node.args.clone();
        let wide = |l: &Self, i: usize| l.wide(a[i]);
        let val = match &node.op {
            op @ (Op::Add | Op::Sub) => {
                let fop = if matches!(op, Op::Add) { "fadd" } else { "fsub" };
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (yl, yh) = if matches!(op, Op::Add) { (y[0], y[1]) } else { (y[1], y[0]) };
                    let lo = self.fbin(fop, x[0], yl);
                    Value::Ival([lo, self.fbin(fop, x[1], yh)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(self.fbin(fop, x, y))
                }
            }
            op @ (Op::Min | Op::Max) => {
                let max = matches!(op, Op::Max);
                let pick = |l: &mut Self, p: Num, q: Num| if max { l.lua_max(p, q) } else { l.lua_min(p, q) };
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let lo = pick(self, x[0], y[0]);
                    Value::Ival([lo, pick(self, x[1], y[1])])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(pick(self, x, y))
                }
            }
            op @ (Op::Mul | Op::Div) => {
                let fop = if matches!(op, Op::Mul) { "fmul" } else { "fdiv" };
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    Value::Ival(self.corners(fop, x, y))
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(self.fbin(fop, x, y))
                }
            }
            // LuaJIT's `a - floor(a / b) * b`.
            Op::Rem => {
                if wide(self, 0) || wide(self, 1) {
                    bail!("node {}: `%` over an interval is not modelled (the reference refuses it)", id);
                }
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let q = self.fbin("fdiv", x, y);
                let f = self.floor(q);
                let p = self.fbin("fmul", f, y);
                Value::Num(self.fbin("fsub", x, p))
            }
            Op::Neg => {
                if wide(self, 0) {
                    let x = self.as_ival(a[0])?;
                    let lo = self.fneg(x[1]);
                    Value::Ival([lo, self.fneg(x[0])])
                } else {
                    let x = self.as_num(a[0])?;
                    Value::Num(self.fneg(x))
                }
            }
            // Each end's where the interval keeps one sign, else `[0, max(|lo|, hi)]`.
            Op::Abs => {
                if wide(self, 0) {
                    let x = self.as_ival(a[0])?;
                    let nonneg = self.fcmp("oge", x[0], k(0.0));
                    let nonpos = self.fcmp("ole", x[1], k(0.0));
                    let (al, ah) = (self.fabs(x[0]), self.fabs(x[1]));
                    let straddle_hi = self.lua_max(al, x[1]);
                    let lo_np = self.n_sel(nonpos, ah, Num::Q(0));
                    let lo = self.n_sel(nonneg, al, lo_np);
                    let hi_np = self.n_sel(nonpos, al, straddle_hi);
                    Value::Ival([lo, self.n_sel(nonneg, ah, hi_np)])
                } else {
                    let x = self.as_num(a[0])?;
                    Value::Num(self.fabs(x))
                }
            }
            Op::Flr => {
                let x = self.as_ival(a[0])?;
                Value::Num(self.floor(x[0]))
            }
            Op::Sin => {
                if wide(self, 0) {
                    bail!("node {}: `sin` of an interval reached the kernel (the tracer writes its range)", id);
                }
                let x = self.as_num(a[0])?;
                self.call(Call::Arith1(CTX_SIN), &[x])
            }
            op @ (Op::Lt | Op::Le | Op::Gt | Op::Ge) => self.compare(op, [a[0], a[1]])?,
            Op::Eq => {
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (xp, yp) = (self.same_bits(x[0], x[1]), self.same_bits(y[0], y[1]));
                    let both = self.m_and(xp, yp);
                    let lo_eq = self.fcmp("oeq", x[0], y[0]);
                    let val = self.m_and(both, lo_eq);
                    let (d1, d2) = (self.fcmp("ogt", x[0], y[1]), self.fcmp("ogt", y[0], x[1]));
                    let apart = self.m_or(d1, d2);
                    Value::Bool([val, self.m_or(both, apart)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Bool([self.fcmp("oeq", x, y), Mask::Const(true)])
                }
            }
            Op::Same => {
                if wide(self, 0) || wide(self, 1) {
                    bail!("node {}: `Same` of an interval (a pin checks each end)", id);
                }
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                Value::Bool([self.same_bits(x, y), Mask::Const(true)])
            }
            Op::Known => {
                let inner = a[0];
                if let Op::Flr = self.g.get(inner).op {
                    let src = self.g.get(inner).args[0];
                    if self.wide(src) {
                        let x = self.as_ival(src)?;
                        let (fl, fh) = (self.floor(x[0]), self.floor(x[1]));
                        self.vals[id as usize] = Some(Value::Bool([self.fcmp("oeq", fl, fh), Mask::Const(true)]));
                        return Ok(());
                    }
                }
                match self.g.ty(inner) {
                    Ty::Bool | Ty::UBool => unreachable!("a boolean's `Known` is `lower_common`'s"),
                    Ty::Ival => {
                        let x = self.as_ival(inner)?;
                        Value::Bool([self.same_bits(x[0], x[1]), Mask::Const(true)])
                    }
                    Ty::Num => Value::Bool([Mask::Const(true), Mask::Const(true)]),
                }
            }
            Op::Frag(cut, c) | Op::FragOk(cut, c) => {
                let x = self.as_ival(a[0])?;
                let ([lo, hi], valid) = match cut.grid() {
                    Cut::Rounds => {
                        let (f, valid, _) = self.round_fragment(x, *c);
                        (f, valid)
                    }
                    _ => self.floor_fragment(x, *c),
                };
                match node.op {
                    Op::FragOk(..) => Value::Bool([valid, Mask::Const(true)]),
                    Op::Frag(Cut::Ints, _) => Value::Num(lo),
                    _ => Value::Ival([lo, hi]),
                }
            }
            Op::ForkOk(Cut::Floors, ways) => {
                let x = self.as_ival(a[0])?;
                let cells = self.floor_cells(x);
                Value::Bool([self.fcmp("ole", cells, k(*ways as f64 - 1.0)), Mask::Const(true)])
            }
            Op::ForkOk(Cut::Rounds, ways) => {
                let x = self.as_ival(a[0])?;
                let cells = self.round_cells(x);
                let count = self.fbin("fadd", cells, k(1.0));
                let mut ok = self.fcmp("ole", count, k(*ways as f64));
                for c in 0..*ways {
                    let (_, _, settled) = self.round_fragment(x, c);
                    ok = self.m_and(ok, settled);
                }
                Value::Bool([ok, Mask::Const(true)])
            }
            // The result is finite; a quotient's divisor excludes zero.
            Op::NoWrap => {
                let x = a[0];
                let xn = self.g.get(x).clone();
                let ok = match xn.op {
                    Op::Add | Op::Sub | Op::Mul | Op::Rem => self.finite_value(x)?,
                    Op::Div => {
                        let fin = self.finite_value(x)?;
                        let apart = match self.as_ival(xn.args[1])? {
                            [Num::Q(l), Num::Q(h)] => Mask::Const(f64::from_bits(l) > 0.0 || f64::from_bits(h) < 0.0),
                            [l, h] => {
                                let (above, below) = (self.fcmp("ogt", l, k(0.0)), self.fcmp("olt", h, k(0.0)));
                                self.m_or(above, below)
                            }
                        };
                        self.m_and(fin, apart)
                    }
                    _ => Mask::Const(true),
                };
                Value::Bool([ok, Mask::Const(true)])
            }
            other => bail!("node {}: op {:?} is not supported by the doubles' LLVM kernels", id, other),
        };
        self.vals[id as usize] = Some(val);
        Ok(())
    }
}
