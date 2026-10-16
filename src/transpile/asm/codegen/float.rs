//! THE DOUBLES' LOWERING (plans/float-solver.md, milestone 5): a graph in
//! `Float` to 8 `f64` lanes a zmm, each operation the reference engine's
//! (`refdomain` over `LuaF64`/`LuaF64Interval`), bit for bit:
//!
//! - IEEE `vaddpd`/`vsubpd`/`vmulpd`/`vdivpd`, one rounding each, in the
//!   cart's order. No fused multiply-add: none is emitted, and
//!   `jit::assemble` refuses a text holding one;
//! - `flr` as `vrndscalepd` round-down (exact);
//! - `%` as LuaJIT's `a - flr(a / b) * b`, three rounded operations;
//! - `min`/`max` as LuaJIT's: a compare and a blend, the SECOND argument on
//!   a tie (`-0` against `0` included), never `vminpd`;
//! - negation and `abs` on the sign bit;
//! - `sin` a per-lane call-out to `LuaF64::lua_sin` (glibc's `sin`);
//! - an interval's ends as the reference takes them: sums and differences
//!   end by end, products and quotients the least and most corner in the
//!   total order (`-0` below `0`), the forks' fragments
//!   `LuaF64Interval::floor_fragment`'s and `round_fragment`'s formulas
//!   operation by operation.
//!
//! An operation's own error (`NoWrap`, `trace::error`) is "the result is
//! finite", and a quotient's divisor must exclude zero, as
//! `LuaF64Interval::checked_div` requires. `asm::tests` checks every
//! operation against `LuaF64`, `-0`, ties and subnormals included.

use super::*;

/// `vcmppd` predicates, ordered and quiet: no NaN reaches a compare whose
/// answer is read (a non-finite value is its operation's own error).
const EQ: u8 = 0x00;
const LT: u8 = 0x11;
const LE: u8 = 0x12;
const GE: u8 = 0x1d;
const GT: u8 = 0x1e;
/// `vrndscalepd`: round toward minus infinity, the precision exception
/// suppressed: `floor`, exact.
const FLOOR: u8 = 0x09;
const SIGN: u64 = 0x8000_0000_0000_0000;
const MAGNITUDE: u64 = 0x7fff_ffff_ffff_ffff;

/// A double as a broadcast operand.
fn k(v: f64) -> Src {
    Src::BQ(v.to_bits())
}

impl Lower<'_, Float> {
    /// `a op b`, one IEEE rounding. `+` and `*` commute bit for bit (the
    /// sign of a zero sum or product included): one key for both orders.
    fn fbin(&mut self, op: FOp, a: Vreg, b: Src) -> Vreg {
        let (a, b) = match (op, b) {
            (FOp::Add | FOp::Mul, Src::Reg(r)) if r < a => (r, Src::Reg(a)),
            _ => (a, b),
        };
        self.pure(Key::FBin(op, a, b.key()), move |d| Inst::FBin { dst: d, op, a, b })
    }

    /// `fbin` of two values (a constant first operand materialized).
    fn fval(&mut self, op: FOp, a: NumVal, b: NumVal) -> Vreg {
        let ar = self.num_reg(a);
        let bs = self.num_src(b);
        self.fbin(op, ar, bs)
    }

    fn floor(&mut self, a: Vreg) -> Vreg {
        self.pure(Key::FRnd(a, FLOOR), move |d| Inst::FRnd { dst: d, a, imm: FLOOR })
    }

    /// `a pred b` as a vector mask.
    fn fcmp(&mut self, imm: u8, a: Vreg, b: Src) -> Vreg {
        self.pure(Key::FCmp(imm, a, b.key()), move |d| Inst::FCmp { dst: d, imm, a, b })
    }

    /// A 64-bit integer op on the bits (`And`/`Or`/`Xor` commute).
    fn qbin(&mut self, op: QOp, a: Vreg, b: Src) -> Vreg {
        let (a, b) = match (op, b) {
            (QOp::And | QOp::Or | QOp::Xor, Src::Reg(r)) if r < a => (r, Src::Reg(a)),
            _ => (a, b),
        };
        self.pure(Key::QBin(op, a, b.key()), move |d| Inst::QBin { dst: d, op, a, b })
    }

    /// The same bits (a vector mask).
    fn same_bits(&mut self, a: Vreg, b: Vreg) -> Vreg {
        let (a, b) = if b < a { (b, a) } else { (a, b) };
        self.pure(Key::QCmp(0, a, SrcKey::Reg(b)), move |d| Inst::QCmp { dst: d, imm: 0, a, b: Src::Reg(b) })
    }

    /// LuaJIT's `min(a, b)`: `a` if `a < b`, else `b` (`LuaF64::lua_min`).
    fn lua_min(&mut self, a: Vreg, b: Vreg) -> Vreg {
        if a == b {
            return a;
        }
        let m = self.fcmp(LT, a, Src::Reg(b));
        self.vsel(m, a, b)
    }

    /// LuaJIT's `max(a, b)`: `a` if `a > b`, else `b` (`LuaF64::lua_max`).
    fn lua_max(&mut self, a: Vreg, b: Vreg) -> Vreg {
        if a == b {
            return a;
        }
        let m = self.fcmp(GT, a, Src::Reg(b));
        self.vsel(m, a, b)
    }

    /// The lesser in the TOTAL order (`-0` below `0`): `a < b`, `b < a`, or
    /// equal values, whose bits OR to the negative zero (or are one value).
    fn least(&mut self, a: Vreg, b: Vreg) -> Vreg {
        if a == b {
            return a;
        }
        let lt = self.fcmp(LT, a, Src::Reg(b));
        let gt = self.fcmp(LT, b, Src::Reg(a));
        let tie = self.qbin(QOp::Or, a, Src::Reg(b));
        let inner = self.vsel(gt, b, tie);
        self.vsel(lt, a, inner)
    }

    /// The greater in the total order: a tie's bits AND to the positive zero.
    fn most(&mut self, a: Vreg, b: Vreg) -> Vreg {
        if a == b {
            return a;
        }
        let gt = self.fcmp(GT, a, Src::Reg(b));
        let lt = self.fcmp(GT, b, Src::Reg(a));
        let tie = self.qbin(QOp::And, a, Src::Reg(b));
        let inner = self.vsel(lt, b, tie);
        self.vsel(gt, a, inner)
    }

    /// Unary minus: the sign bit flipped (`-0` for `0`).
    fn neg(&mut self, a: Vreg) -> Vreg {
        self.qbin(QOp::Xor, a, Src::BQ(SIGN))
    }

    /// `math.abs`: the sign bit cleared.
    fn abs(&mut self, a: Vreg) -> Vreg {
        self.qbin(QOp::And, a, Src::BQ(MAGNITUDE))
    }

    /// Is `a` finite (`|a| <= MAX`; false for an infinity and a NaN)?
    fn finite(&mut self, a: Vreg) -> Vreg {
        let m = self.abs(a);
        self.fcmp(LE, m, k(f64::MAX))
    }

    /// The next double below `x` (`f64::next_down` of a finite `x`): one
    /// unit off the bits, away from zero below it; a zero's is the least
    /// negative subnormal.
    fn next_down(&mut self, x: Vreg) -> Vreg {
        let pos = self.fcmp(GT, x, k(0.0));
        let neg = self.fcmp(LT, x, k(0.0));
        let dec = self.qbin(QOp::Sub, x, Src::BQ(1));
        let inc = self.qbin(QOp::Add, x, Src::BQ(1));
        let tiny = self.num_reg(NumVal::ConstQ(0x8000_0000_0000_0001));
        let off_zero = self.vsel(neg, inc, tiny);
        self.vsel(pos, dec, off_zero)
    }

    /// The next double above `x` (`f64::next_up`).
    fn next_up(&mut self, x: Vreg) -> Vreg {
        let pos = self.fcmp(GT, x, k(0.0));
        let neg = self.fcmp(LT, x, k(0.0));
        let inc = self.qbin(QOp::Add, x, Src::BQ(1));
        let dec = self.qbin(QOp::Sub, x, Src::BQ(1));
        let tiny = self.num_reg(NumVal::ConstQ(1));
        let off_zero = self.vsel(neg, dec, tiny);
        self.vsel(pos, inc, off_zero)
    }

    /// `flr(fl(x + 1/2))`, as `move` computes the pixel amount.
    fn round_of(&mut self, x: Vreg) -> Vreg {
        let h = self.fbin(FOp::Add, x, k(0.5));
        self.floor(h)
    }

    /// `LuaF64Interval::round_cut(t)`, step by step: the first double `c`
    /// with `round_of(c) >= t`, from `t - 1/2`, at most four moves; the
    /// lanes it settles on (else the reference raises).
    fn round_cut(&mut self, t: Vreg) -> (Vreg, MaskVal) {
        let mut c = self.fbin(FOp::Sub, t, k(0.5));
        let mut done = MaskVal::Const(false);
        for _ in 0..4 {
            let r = self.round_of(c);
            let up = self.fcmp(LT, r, Src::Reg(t));
            let pc = self.next_down(c);
            let rp = self.round_of(pc);
            let down = self.fcmp(GE, rp, Src::Reg(t));
            let nu = self.next_up(c);
            let moved_down = self.vsel(down, pc, c);
            let next = self.vsel(up, nu, moved_down);
            let moves = self.m_or(MaskVal::Reg(up), MaskVal::Reg(down));
            let settles = self.m_not(moves);
            c = match self.n_sel(done, NumVal::Reg(c), NumVal::Reg(next)) {
                NumVal::Reg(v) => v,
                other => self.num_reg(other),
            };
            done = self.m_or(done, settles);
        }
        (c, done)
    }

    /// `LuaF64Interval::floor_fragment(c)` of `a`: `[max(lo, b), min(hi,
    /// pred(b + 1))]` with `b = floor(lo) + c` (the cart's `max`/`min`), and
    /// whether `a` reaches it: `floor(hi) - floor(lo) >= c` (`ev_frag_cells`).
    fn floor_fragment(&mut self, a: [Vreg; 2], c: u8) -> ([Vreg; 2], Vreg) {
        let fl = self.floor(a[0]);
        let base = self.fbin(FOp::Add, fl, k(c as f64));
        let lo = self.lua_max(a[0], base);
        let next = self.fbin(FOp::Add, base, k(1.0));
        let top = self.next_down(next);
        let hi = self.lua_min(a[1], top);
        let cells = self.floor_cells(a);
        let valid = self.fcmp(GE, cells, k(c as f64));
        ([lo, hi], valid)
    }

    /// `floor(hi) - floor(lo)`: the floors an interval spans, minus one.
    fn floor_cells(&mut self, a: [Vreg; 2]) -> Vreg {
        let fl = self.floor(a[0]);
        let fh = self.floor(a[1]);
        self.fbin(FOp::Sub, fh, Src::Reg(fl))
    }

    /// `round_of(hi) - round_of(lo)`: the values of `flr(x + 1/2)` an
    /// interval takes, minus one.
    fn round_cells(&mut self, a: [Vreg; 2]) -> Vreg {
        let rl = self.round_of(a[0]);
        let rh = self.round_of(a[1]);
        self.fbin(FOp::Sub, rh, Src::Reg(rl))
    }

    /// `LuaF64Interval::round_fragment(c)` of `a`: `t = round_of(lo) + c`,
    /// `[max(lo, cut(t)), min(hi, pred(cut(t + 1)))]`; whether `a` reaches
    /// it; and whether both cuts settled.
    fn round_fragment(&mut self, a: [Vreg; 2], c: u8) -> ([Vreg; 2], Vreg, MaskVal) {
        let rl = self.round_of(a[0]);
        let t = self.fbin(FOp::Add, rl, k(c as f64));
        let t1 = self.fbin(FOp::Add, t, k(1.0));
        let (cut_lo, s_lo) = self.round_cut(t);
        let (cut_hi, s_hi) = self.round_cut(t1);
        let lo = self.lua_max(a[0], cut_lo);
        let top = self.next_down(cut_hi);
        let hi = self.lua_min(a[1], top);
        let cells = self.round_cells(a);
        let valid = self.fcmp(GE, cells, k(c as f64));
        let settled = self.m_and(s_lo, s_hi);
        ([lo, hi], valid, settled)
    }

    /// An interval product or quotient (`LuaF64Interval::checked_mul`/
    /// `checked_div`): the four corners, each rounded, and the least and
    /// the most in the total order.
    fn corners(&mut self, op: FOp, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        let c = [
            self.fbin(op, a[0], Src::Reg(b[0])),
            self.fbin(op, a[0], Src::Reg(b[1])),
            self.fbin(op, a[1], Src::Reg(b[0])),
            self.fbin(op, a[1], Src::Reg(b[1])),
        ];
        let (l0, l1) = (self.least(c[0], c[1]), self.least(c[2], c[3]));
        let (h0, h1) = (self.most(c[0], c[1]), self.most(c[2], c[3]));
        [self.least(l0, l1), self.most(h0, h1)]
    }

    /// Both ends finite (one for a number).
    fn finite_value(&mut self, x: NodeId) -> Result<MaskVal> {
        Ok(match self.vals[x as usize] {
            Some(Value::Num(n)) => {
                let r = self.num_reg(n);
                MaskVal::Reg(self.finite(r))
            }
            Some(Value::Ival(iv)) => {
                let [lo, hi] = self.ival_regs(iv);
                let (fl, fh) = (self.finite(lo), self.finite(hi));
                self.m_and(MaskVal::Reg(fl), MaskVal::Reg(fh))
            }
            _ => bail!("node {}: not a number where one was needed", x),
        })
    }

    /// A compare of two numbers, or tri-state over intervals (`zi_cmp`'s
    /// structure: decided true, decided false, else unknown), as
    /// `refdomain`'s `compare` decides it.
    fn compare(&mut self, op: &Op<Float>, a: [NodeId; 2]) -> Result<Value> {
        if self.dom(a[0]) == 2 || self.dom(a[1]) == 2 {
            let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
            let (x, y) = (self.ival_regs(x), self.ival_regs(y));
            let (t, f) = match op {
                Op::Lt => (self.fcmp(LT, x[1], Src::Reg(y[0])), self.fcmp(GE, x[0], Src::Reg(y[1]))),
                Op::Le => (self.fcmp(LE, x[1], Src::Reg(y[0])), self.fcmp(GT, x[0], Src::Reg(y[1]))),
                Op::Gt => (self.fcmp(GT, x[0], Src::Reg(y[1])), self.fcmp(LE, x[1], Src::Reg(y[0]))),
                _ => (self.fcmp(GE, x[0], Src::Reg(y[1])), self.fcmp(LT, x[1], Src::Reg(y[0]))),
            };
            let known = self.m_or(MaskVal::Reg(t), MaskVal::Reg(f));
            return Ok(Value::Bool([MaskVal::Reg(t), known]));
        }
        let imm = match op {
            Op::Lt => LT,
            Op::Le => LE,
            Op::Gt => GT,
            _ => GE,
        };
        let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
        let xr = self.num_reg(x);
        let ys = self.num_src(y);
        Ok(Value::Bool([MaskVal::Reg(self.fcmp(imm, xr, ys)), MaskVal::Const(true)]))
    }

    /// A literal operand as an `i16` (the collision box's sizes and flag).
    fn literal_i16(&self, id: NodeId, n: NumVal, what: &str) -> Result<i32> {
        match n {
            NumVal::ConstQ(bits) => {
                let v = f64::from_bits(bits);
                if v == v.floor() && (i16::MIN as f64..=i16::MAX as f64).contains(&v) {
                    Ok(v as i16 as i32)
                } else {
                    bail!("node {}: tile_flag's {what} {v} is not an integer", id)
                }
            }
            _ => bail!("node {}: tile_flag's {what} must be an exact constant", id),
        }
    }

    pub(super) fn lower_node_f64(&mut self, id: NodeId) -> Result<()> {
        if let Some(v) = self.lower_common(id)? {
            self.vals[id as usize] = Some(v);
            return Ok(());
        }
        let node = self.g.get(id);
        let a = node.args.clone();
        let wide = |l: &Self, k: usize| l.dom(a[k]) == 2;
        let val = match &node.op {
            Op::Const(lo, hi) => {
                if lo == hi {
                    Value::Num(NumVal::ConstQ(*lo))
                } else {
                    Value::Ival([NumVal::ConstQ(*lo), NumVal::ConstQ(*hi)])
                }
            }
            Op::Cell(c) => {
                let off = *self.cell_off.get(c).expect("input cell offset assigned before lowering");
                match self.cell_reprs.get(c).copied().unwrap_or(CellRepr::Num) {
                    CellRepr::Num => Value::Num(NumVal::Reg(self.pure(Key::Load(off), move |d| Inst::Load { dst: d, off }))),
                    CellRepr::Bool => {
                        let v = self.pure(Key::LoadMask8(off), move |d| Inst::LoadMask8 { dst: d, off });
                        Value::Bool([MaskVal::Reg(v), MaskVal::Const(true)])
                    }
                    CellRepr::UBool => {
                        let known_off = off + 2;
                        let v = self.pure(Key::LoadMask8(off), move |d| Inst::LoadMask8 { dst: d, off });
                        let kn = self.pure(Key::LoadMask8(known_off), move |d| Inst::LoadMask8 { dst: d, off: known_off });
                        Value::Bool([MaskVal::Reg(v), MaskVal::Reg(kn)])
                    }
                    CellRepr::Ival => {
                        let hi_off = off + 64;
                        let lo = self.pure(Key::Load(off), move |d| Inst::Load { dst: d, off });
                        let hi = self.pure(Key::Load(hi_off), move |d| Inst::Load { dst: d, off: hi_off });
                        Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                    }
                }
            }
            op @ (Op::Add | Op::Sub) => {
                let fop = if matches!(op, Op::Add) { FOp::Add } else { FOp::Sub };
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (x, y) = (self.ival_regs(x), self.ival_regs(y));
                    // `[x.lo + y.lo, x.hi + y.hi]`, `[x.lo - y.hi, x.hi - y.lo]`.
                    let (yl, yh) = if fop == FOp::Add { (y[0], y[1]) } else { (y[1], y[0]) };
                    let lo = self.fbin(fop, x[0], Src::Reg(yl));
                    let hi = self.fbin(fop, x[1], Src::Reg(yh));
                    Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(NumVal::Reg(self.fval(fop, x, y)))
                }
            }
            // End by end, the cart's tie rule at each (`refdomain`'s `fun2`).
            op @ (Op::Min | Op::Max) => {
                let max = matches!(op, Op::Max);
                let pick = |l: &mut Self, p: Vreg, q: Vreg| if max { l.lua_max(p, q) } else { l.lua_min(p, q) };
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (x, y) = (self.ival_regs(x), self.ival_regs(y));
                    let lo = pick(self, x[0], y[0]);
                    let hi = pick(self, x[1], y[1]);
                    Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    let (x, y) = (self.num_reg(x), self.num_reg(y));
                    Value::Num(NumVal::Reg(pick(self, x, y)))
                }
            }
            op @ (Op::Mul | Op::Div) => {
                let fop = if matches!(op, Op::Mul) { FOp::Mul } else { FOp::Div };
                if wide(self, 0) || wide(self, 1) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (x, y) = (self.ival_regs(x), self.ival_regs(y));
                    let [lo, hi] = self.corners(fop, x, y);
                    Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(NumVal::Reg(self.fval(fop, x, y)))
                }
            }
            // LuaJIT's `a - floor(a / b) * b` (`LuaF64::checked_rem`).
            Op::Rem => {
                if wide(self, 0) || wide(self, 1) {
                    bail!("node {}: `%` over an interval is not modelled (the reference refuses it)", id);
                }
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let xr = self.num_reg(x);
                let ys = self.num_src(y);
                let q = self.fbin(FOp::Div, xr, ys);
                let f = self.floor(q);
                let p = self.fbin(FOp::Mul, f, ys);
                Value::Num(NumVal::Reg(self.fbin(FOp::Sub, xr, Src::Reg(p))))
            }
            Op::Neg => {
                if wide(self, 0) {
                    let x = self.as_ival(a[0])?;
                    let x = self.ival_regs(x);
                    let (lo, hi) = (self.neg(x[1]), self.neg(x[0]));
                    Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                } else {
                    let x = self.as_num(a[0])?;
                    let xr = self.num_reg(x);
                    Value::Num(NumVal::Reg(self.neg(xr)))
                }
            }
            // `LuaF64Interval::abs`: each end's where the interval keeps one
            // sign (`lo >= 0`, `hi <= 0` by value), else `[0, max(|lo|, hi)]`.
            Op::Abs => {
                if wide(self, 0) {
                    let x = self.as_ival(a[0])?;
                    let x = self.ival_regs(x);
                    let nonneg = self.fcmp(GE, x[0], k(0.0));
                    let nonpos = self.fcmp(LE, x[1], k(0.0));
                    let (al, ah) = (self.abs(x[0]), self.abs(x[1]));
                    let zero = self.num_reg(NumVal::ConstQ(0));
                    let straddle_hi = self.lua_max(al, x[1]);
                    let lo_np = self.vsel(nonpos, ah, zero);
                    let lo = self.vsel(nonneg, al, lo_np);
                    let hi_np = self.vsel(nonpos, al, straddle_hi);
                    let hi = self.vsel(nonneg, ah, hi_np);
                    Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                } else {
                    let x = self.as_num(a[0])?;
                    let xr = self.num_reg(x);
                    Value::Num(NumVal::Reg(self.abs(xr)))
                }
            }
            // The low end's floor (`refdomain`: a lane spanning one floor).
            Op::Flr => {
                let x = self.as_ival(a[0])?;
                let lo = self.num_reg(x[0]);
                Value::Num(NumVal::Reg(self.floor(lo)))
            }
            Op::Sin => {
                if wide(self, 0) {
                    bail!("node {}: `sin` of an interval reached the kernel (the tracer writes its range)", id);
                }
                let x = self.as_num(a[0])?;
                let xr = self.num_reg(x);
                let dst = self.fresh();
                self.insts.push(Inst::Call { op: CallOp::FSin, dst, args: vec![xr], scalars: Vec::new() });
                Value::Num(NumVal::Reg(dst))
            }
            op @ (Op::Lt | Op::Le | Op::Gt | Op::Ge) => self.compare(op, [a[0], a[1]])?,
            Op::Eq => {
                if self.dom(a[0]) == 1 {
                    let (p, q) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                    // val = ~(pv ^ qv), known = pk & qk
                    let x = self.m_xor(p[0], q[0]);
                    let val = self.m_not(x);
                    let known = self.m_and(p[1], q[1]);
                    Value::Bool([val, known])
                } else if wide(self, 0) || wide(self, 1) {
                    // As `refdomain`: two points (each end's bits one) by
                    // value, apart intervals unequal, else unknown.
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (x, y) = (self.ival_regs(x), self.ival_regs(y));
                    let (xp, yp) = (self.same_bits(x[0], x[1]), self.same_bits(y[0], y[1]));
                    let both = self.m_and(MaskVal::Reg(xp), MaskVal::Reg(yp));
                    let lo_eq = self.fcmp(EQ, x[0], Src::Reg(y[0]));
                    let val = self.m_and(both, MaskVal::Reg(lo_eq));
                    let (d1, d2) = (self.fcmp(GT, x[0], Src::Reg(y[1])), self.fcmp(GT, y[0], Src::Reg(x[1])));
                    let apart = self.m_or(MaskVal::Reg(d1), MaskVal::Reg(d2));
                    let known = self.m_or(both, apart);
                    Value::Bool([val, known])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    let xr = self.num_reg(x);
                    let ys = self.num_src(y);
                    Value::Bool([MaskVal::Reg(self.fcmp(EQ, xr, ys)), MaskVal::Const(true)])
                }
            }
            // A pin's check: the same bits.
            Op::Same => {
                if wide(self, 0) || wide(self, 1) {
                    bail!("node {}: `Same` of an interval (a pin checks each end)", id);
                }
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let (x, y) = (self.num_reg(x), self.num_reg(y));
                Value::Bool([MaskVal::Reg(self.same_bits(x, y)), MaskVal::Const(true)])
            }
            Op::Known => {
                let inner = a[0];
                // `Known(Flr(x))` reads the interval `x`: one floor by value
                // (`refdomain`'s `floor_span`).
                if let Op::Flr = self.g.get(inner).op {
                    let src = self.g.get(inner).args[0];
                    if self.dom(src) == 2 {
                        let x = self.as_ival(src)?;
                        let x = self.ival_regs(x);
                        let (fl, fh) = (self.floor(x[0]), self.floor(x[1]));
                        let one = self.fcmp(EQ, fl, Src::Reg(fh));
                        self.vals[id as usize] = Some(Value::Bool([MaskVal::Reg(one), MaskVal::Const(true)]));
                        return Ok(());
                    }
                }
                match self.dom(inner) {
                    1 => {
                        let b = self.as_bool(inner)?;
                        Value::Bool([b[1], MaskVal::Const(true)])
                    }
                    // A number is decided where its ends are one value's bits
                    // (`Graph::eval`'s `as_point`).
                    2 => {
                        let x = self.as_ival(inner)?;
                        let x = self.ival_regs(x);
                        Value::Bool([MaskVal::Reg(self.same_bits(x[0], x[1])), MaskVal::Const(true)])
                    }
                    _ => Value::Bool([MaskVal::Const(true), MaskVal::Const(true)]),
                }
            }
            // The floor fork, resolved.
            Op::Frag(c) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let ([lo, hi], _) = self.floor_fragment(x, *c);
                Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
            }
            Op::FragOk(c) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let (_, valid) = self.floor_fragment(x, *c);
                Value::Bool([MaskVal::Reg(valid), MaskVal::Const(true)])
            }
            // The low end of fragment `c` as an exact number.
            Op::IntFrag(c) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let ([lo, _], _) = self.floor_fragment(x, *c);
                Value::Num(NumVal::Reg(lo))
            }
            // At most `ways` floors (`ev_split_ok`).
            Op::SplitOk(ways) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let cells = self.floor_cells(x);
                let ok = self.fcmp(LE, cells, k(*ways as f64 - 1.0));
                Value::Bool([MaskVal::Reg(ok), MaskVal::Const(true)])
            }
            // The round fork, resolved.
            Op::RFrag(c) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let ([lo, hi], _, _) = self.round_fragment(x, *c);
                Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
            }
            Op::RFragOk(c) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let (_, valid, _) = self.round_fragment(x, *c);
                Value::Bool([MaskVal::Reg(valid), MaskVal::Const(true)])
            }
            // At most `ways` values of `flr(x + 1/2)` (`LuaF64Interval::
            // rounds`), and every cut a fragment reads settled (else the
            // reference raises): the fork's own error.
            Op::RSplitOk(ways) => {
                let x = self.as_ival(a[0])?;
                let x = self.ival_regs(x);
                let cells = self.round_cells(x);
                let count = self.fbin(FOp::Add, cells, k(1.0));
                let mut ok = MaskVal::Reg(self.fcmp(LE, count, k(*ways as f64)));
                for c in 0..*ways {
                    let (_, _, settled) = self.round_fragment(x, c);
                    ok = self.m_and(ok, settled);
                }
                Value::Bool([ok, MaskVal::Const(true)])
            }
            // The operation's own error (`trace::error`,
            // `GraphArith::range_error`): its result is finite; a quotient's
            // divisor excludes zero (`checked_div`).
            Op::NoWrap => {
                let x = a[0];
                let xn = self.g.get(x).clone();
                let ok = match xn.op {
                    Op::Add | Op::Sub | Op::Mul | Op::Rem => self.finite_value(x)?,
                    Op::Div => {
                        let fin = self.finite_value(x)?;
                        let d = self.as_ival(xn.args[1])?;
                        let apart = match d {
                            [NumVal::ConstQ(l), NumVal::ConstQ(h)] => MaskVal::Const(f64::from_bits(l) > 0.0 || f64::from_bits(h) < 0.0),
                            _ => {
                                let d = self.ival_regs(d);
                                let (above, below) = (self.fcmp(GT, d[0], k(0.0)), self.fcmp(LT, d[1], k(0.0)));
                                self.m_or(MaskVal::Reg(above), MaskVal::Reg(below))
                            }
                        };
                        self.m_and(fin, apart)
                    }
                    _ => MaskVal::Const(true),
                };
                Value::Bool([ok, MaskVal::Const(true)])
            }
            Op::Mget => {
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let args = vec![self.num_reg(x), self.num_reg(y)];
                let dst = self.fresh();
                self.insts.push(Inst::Call { op: CallOp::FMget, dst, args, scalars: Vec::new() });
                Value::Num(NumVal::Reg(dst))
            }
            Op::TileFlagAt => {
                let (nx, ny) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let (x, y) = (self.num_reg(nx), self.num_reg(ny));
                let (wv, hv, fv) = (self.as_num(a[2])?, self.as_num(a[3])?, self.as_num(a[4])?);
                let flag = self.literal_i16(id, fv, "flag")?;
                let dst = self.fresh();
                match (wv, hv) {
                    (NumVal::ConstQ(_), NumVal::ConstQ(_)) => {
                        let (w, h) = (self.literal_i16(id, wv, "width")?, self.literal_i16(id, hv, "height")?);
                        self.insts.push(Inst::Call { op: CallOp::FTileFlag, dst, args: vec![x, y], scalars: vec![w, h, flag] });
                    }
                    _ => {
                        let (wr, hr) = (self.num_reg(wv), self.num_reg(hv));
                        self.insts.push(Inst::Call { op: CallOp::FTileFlagLanes, dst, args: vec![x, y, wr, hr], scalars: vec![flag] });
                    }
                }
                Value::Bool([MaskVal::Reg(dst), MaskVal::Const(true)])
            }
            other => bail!("node {}: op {:?} is not supported by the doubles' kernels", id, other),
        };
        self.vals[id as usize] = Some(val);
        Ok(())
    }
}
