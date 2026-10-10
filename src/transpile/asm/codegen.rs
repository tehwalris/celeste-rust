//! `transpile::graph::Graph` -> AVX-512 GAS assembly: numeric, interval and
//! tri-state boolean ops, bit-for-bit `celeste_engine::kernel` (checked by
//! `super::tests`). Lower to SSA over vregs (GVN), list-schedule,
//! linear-scan allocate, emit.

use std::collections::HashMap;
use std::fmt::Write;

use anyhow::{bail, Result};

use crate::transpile::graph::{self, Graph, NodeId, Op};

// Registers: zmm26..=31 are reserved scratches (zmm27 unused), 26 homes.
const ZERO: u8 = 31; // constant 0, for Neg
const OPA: u8 = 30; // reload/remat scratch, operand A
const OPB: u8 = 29; // reload/remat scratch, operand B
const H0: u8 = 28; // scratch for a third operand (ternlog)
const RES: u8 = 26; // scratch for a spilled result
const N_ALLOC: u8 = 26; // homes zmm0..=zmm25

// Call-out ABI: callee-saved r13 (inputs), r14 (outputs), r15 (AsmCtx), so a
// `call` may clobber rdi/rsi. AsmCtx offsets: lockstep with callout.rs.
const CTX_DIV: u32 = 0;
const CTX_REM: u32 = 8;
const CTX_SIN: u32 = 16;
const CTX_MGET: u32 = 24;
const CTX_TILE: u32 = 32;
const CTX_TILE_LANES: u32 = 40;
const CTX_ENV: u32 = 48;
const SAVE_BYTES: u32 = 32 * 64; // save all 32 zmm across a call
const ARGBUF_BYTES: u32 = 8 * 64; // up to 6 SIMD args + a result buffer

/// A call-out op (emitted as a `call` through `AsmCtx`).
#[derive(Clone, Copy)]
enum CallOp {
    Div,
    Rem,
    Sin,
    Mget,
    /// Uniform box: `scalars = [w, h, flag]`.
    TileFlag,
    /// Per-lane box: `args = [x, y, w, h]`, `scalars = [flag]`.
    TileFlagLanes,
}

const FLR_MASK: i32 = 0xffff_0000u32 as i32;

type Vreg = u32;

/// A numeric (ZN) node value: a live register or a broadcast constant.
#[derive(Clone, Copy, PartialEq)]
enum NumVal {
    Reg(Vreg),
    ConstI32(i32),
}

/// One plane of a tri-state boolean: a per-lane VECTOR mask (all-ones or
/// zero), not a k-register, so booleans share the zmm allocator.
#[derive(Clone, Copy, PartialEq)]
enum MaskVal {
    Reg(Vreg),
    Const(bool),
}

#[derive(Clone, Copy)]
enum Value {
    Num(NumVal),
    /// `[val, known]` vector-mask planes.
    Bool([MaskVal; 2]),
    /// `[lo, hi]` interval planes.
    Ival([NumVal; 2]),
}

/// A source operand for an emitted instruction.
#[derive(Clone, Copy)]
enum Src {
    Reg(Vreg),
    BI32(i32),
}

#[derive(Clone, Copy)]
enum ROp {
    AddD,
    SubD,
    MinSD,
    MaxSD,
    AndD,
    OrD,
    XorD,
    /// `(~a) & b` (`vpandnd`).
    AndnD,
}

/// A source operand's value identity, for GVN.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum SrcKey {
    Reg(Vreg),
    I32(i32),
}
impl Src {
    fn key(self) -> SrcKey {
        match self {
            Src::Reg(v) => SrcKey::Reg(v),
            Src::BI32(c) => SrcKey::I32(c),
        }
    }
}

/// A pure instruction's value identity for GVN (commutative operands sorted).
#[derive(Clone, PartialEq, Eq, Hash)]
enum Key {
    Load(u32),
    LoadMask(u32),
    BcastD(i32),
    RBin(u8, Vreg, SrcKey),
    Neg(Vreg),
    Abs(Vreg),
    Muldq(Vreg, Vreg),
    Sraq(Vreg, u8),
    Sllq(Vreg, u8),
    ShrD(Vreg, u8, bool),
    BlendImm(u16, Vreg, Vreg),
    Cmp(u8, Vreg, SrcKey),
    Ternlog(Vreg, Vreg, Vreg, u8),
}

/// The SSA instruction stream: each writes exactly one vreg (`dst`).
enum Inst {
    Load { dst: Vreg, off: u32 },
    /// Load a 16-bit lane mask and expand it to a vector mask.
    LoadMask { dst: Vreg, off: u32 },
    BcastD { dst: Vreg, val: i32 },
    RBin { dst: Vreg, op: ROp, a: Vreg, b: Src },
    Neg { dst: Vreg, a: Vreg },
    Abs { dst: Vreg, a: Vreg },
    /// Signed 32x32 -> 64 multiply of the EVEN lanes (`vpmuldq`).
    Muldq { dst: Vreg, a: Vreg, b: Vreg },
    /// Arithmetic 64-bit right shift by an immediate (`vpsraq`).
    Sraq { dst: Vreg, a: Vreg, imm: u8 },
    /// Logical 64-bit left shift by an immediate (`vpsllq`).
    Sllq { dst: Vreg, a: Vreg, imm: u8 },
    /// 32-bit right shift by an immediate: arithmetic (`vpsrad`) or logical
    /// (`vpsrld`).
    ShrD { dst: Vreg, a: Vreg, imm: u8, arith: bool },
    /// Per-lane `mask ? b : a` by a COMPILE-TIME mask (`vpblendmd`).
    BlendImm { dst: Vreg, mask: u16, a: Vreg, b: Vreg },
    /// Signed compare (`vpcmpd` predicate `imm`) to a vector mask.
    Cmp { dst: Vreg, imm: u8, a: Vreg, b: Src },
    /// Three-input bitwise LUT (`vpternlogd`): `dst = LUT_imm(a, b, c)`.
    Ternlog { dst: Vreg, a: Vreg, b: Vreg, c: Vreg, imm: u8 },
    /// A call-out: `dst` is a number, or a vector mask for tile_flag.
    Call { op: CallOp, dst: Vreg, args: Vec<Vreg>, scalars: Vec<i32> },
    Store { off: u32, src: Src },
    /// Store a vector mask as a 16-bit lane mask (boolean roots).
    StoreMask { off: u32, src: Vreg },
}

/// The type of a root value: how to read its output slot.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RootKind {
    Num,
    Bool,
    Ival,
}

/// How an `Op::Cell` input is packed (absent from the map = `Num`):
/// `Num` 16 x i32 (64 B); `Bool` a u16 `val` mask at +0 of 64 B (known);
/// `UBool` `val` at +0, `known` at +2 of 64 B; `Ival` `lo` at +0, `hi` at +64.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CellRepr {
    Num,
    Bool,
    UBool,
    Ival,
}

impl CellRepr {
    /// Bytes this input cell occupies in the input buffer.
    fn size(self) -> u32 {
        match self {
            CellRepr::Num | CellRepr::Bool | CellRepr::UBool => 64,
            CellRepr::Ival => 128,
        }
    }
}

/// The assembly text plus the buffer layouts a caller packs.
pub struct Compiled {
    pub asm: String,
    /// Input cells, in ascending order (layout: `CellRepr`).
    pub input_cells: Vec<u32>,
    /// Byte offset of each input cell (parallel to `input_cells`).
    pub input_offsets: Vec<u32>,
    /// Repr of each input cell (parallel to `input_cells`).
    pub input_reprs: Vec<CellRepr>,
    /// Total bytes the input buffer must be.
    pub input_bytes: u32,
    /// Number of roots.
    pub n_roots: usize,
    /// Root byte offsets: bools first (4 B: `val` u16, `known` at +2), then
    /// numbers (64 B) and intervals (128 B: `lo`, `hi` at +64), 64-aligned.
    pub root_offsets: Vec<u32>,
    /// Bytes the output buffer must be.
    pub out_bytes: u32,
    /// Each root's type.
    pub root_kinds: Vec<RootKind>,
    pub sym: String,
    /// Spill slots used (0 = everything fit).
    pub spill_slots: usize,
    /// The stack frame in bytes, for the caller's stack check.
    pub frame_bytes: u32,
    /// `CELESTE_KERNEL_MIX` only (else empty): per SSA instruction `k`, the
    /// graph node it was lowered from (a root store: the root) and its kind;
    /// the text then marks each instruction's lines with a `#@k` line.
    pub prov: Vec<(NodeId, &'static str)>,
    /// `CELESTE_KERNEL_MIX` only: per SSA instruction, whether plain bit
    /// identities over the stream remove it (`foldable`).
    pub foldable: Vec<bool>,
}

// ---- constant pool ----

#[derive(Default)]
struct Pool {
    d: Vec<i32>,
}
impl Pool {
    fn d(&mut self, v: i32) -> String {
        let i = self.d.iter().position(|x| *x == v).unwrap_or_else(|| {
            self.d.push(v);
            self.d.len() - 1
        });
        format!(".LCd{i}")
    }
}

// ---- lowering: graph -> Inst stream ----

struct Lower<'a> {
    g: &'a Graph,
    vals: Vec<Option<Value>>,
    insts: Vec<Inst>,
    next_vreg: Vreg,
    input_cells: Vec<u32>,
    cell_off: HashMap<u32, u32>,
    /// Per-cell input repr (absent = `Num`); decides how `Op::Cell` loads.
    cell_reprs: &'a HashMap<u32, CellRepr>,
    /// GVN: a pure op's `Key` -> the vreg holding its result.
    memo: HashMap<Key, Vreg>,
}

impl<'a> Lower<'a> {
    fn fresh(&mut self) -> Vreg {
        let v = self.next_vreg;
        self.next_vreg += 1;
        v
    }

    /// Emit a pure instruction unless a vreg already computes `key`.
    fn pure(&mut self, key: Key, mk: impl FnOnce(Vreg) -> Inst) -> Vreg {
        if let Some(v) = self.memo.get(&key) {
            return *v;
        }
        let d = self.fresh();
        self.insts.push(mk(d));
        self.memo.insert(key, d);
        d
    }

    /// A numeric value as a register operand, materializing a constant.
    fn num_reg(&mut self, nv: NumVal) -> Vreg {
        match nv {
            NumVal::Reg(v) => v,
            NumVal::ConstI32(c) => self.pure(Key::BcastD(c), |d| Inst::BcastD { dst: d, val: c }),
        }
    }
    fn num_src(&self, nv: NumVal) -> Src {
        match nv {
            NumVal::Reg(v) => Src::Reg(v),
            NumVal::ConstI32(c) => Src::BI32(c),
        }
    }
    fn muldq(&mut self, a: Vreg, b: Vreg) -> Vreg {
        // vpmuldq is commutative in its two source lanes.
        let (a, b) = if a <= b { (a, b) } else { (b, a) };
        self.pure(Key::Muldq(a, b), move |d| Inst::Muldq { dst: d, a, b })
    }
    fn sraq(&mut self, a: Vreg, imm: u8) -> Vreg {
        self.pure(Key::Sraq(a, imm), move |d| Inst::Sraq { dst: d, a, imm })
    }

    fn sllq(&mut self, a: Vreg, imm: u8) -> Vreg {
        self.pure(Key::Sllq(a, imm), move |d| Inst::Sllq { dst: d, a, imm })
    }
    fn shrd(&mut self, a: Vreg, imm: u8, arith: bool) -> Vreg {
        self.pure(Key::ShrD(a, imm, arith), move |d| Inst::ShrD { dst: d, a, imm, arith })
    }

    /// PICO-8 `x / 2^s` (raw divisor `2^(16+s)`, `0 <= s <= 14`), inline:
    /// `Pico8Num`'s division TRUNCATES toward zero and never saturates for
    /// these divisors, so it is the arithmetic shift of `x` biased by
    /// `2^s - 1` where `x` is negative (`(x + ((x >> 31) >>> (32 - s))) >> s`;
    /// no overflow: the bias is added to negatives only).
    /// `asm::tests::inline_div_rem_by_a_power_of_two_is_pico8s` checks it
    /// bit-exact against `Pico8Num` over the whole i32 range.
    fn div_pow2(&mut self, x: Vreg, s: u8) -> Vreg {
        if s == 0 {
            return x;
        }
        let sign = self.shrd(x, 31, true);
        let bias = self.shrd(sign, 32 - s, false);
        let biased = self.dbin(ROp::AddD, x, bias);
        self.shrd(biased, s, true)
    }

    /// `s` if `id` is the literal `2^(16+s)` with `0 <= s <= 14` (a divisor
    /// `div_pow2` takes).
    fn pow2_divisor(&self, id: NodeId) -> Option<u8> {
        match self.g.get(id).op {
            Op::Const(lo, hi) if lo == hi && lo > 0 && (lo as u32).is_power_of_two() => {
                let m = (lo as u32).trailing_zeros();
                (16..=30).contains(&m).then(|| (m - 16) as u8)
            }
            _ => None,
        }
    }

    /// The mask `2^m - 1` if `id` is the literal `2^m` (raw, `m <= 30`):
    /// PICO-8's `%` is `rem_euclid` on the raw bits, which for a positive power
    /// of two is the low bits, negatives included.
    fn pow2_modulus_mask(&self, id: NodeId) -> Option<i32> {
        match self.g.get(id).op {
            Op::Const(lo, hi) if lo == hi && lo > 0 && (lo as u32).is_power_of_two() => Some(lo - 1),
            _ => None,
        }
    }

    fn blend_imm(&mut self, mask: u16, a: Vreg, b: Vreg) -> Vreg {
        self.pure(Key::BlendImm(mask, a, b), move |d| Inst::BlendImm { dst: d, mask, a, b })
    }

    /// 16.16 multiply, `zn_mul`'s even/odd `vpmuldq` weave.
    fn zn_mul(&mut self, a: Vreg, b: Vreg) -> Vreg {
        let ev = self.muldq(a, b);
        let ev = self.sraq(ev, 16);
        let ah = self.sraq(a, 32);
        let bh = self.sraq(b, 32);
        let od = self.muldq(ah, bh);
        let od = self.sraq(od, 16);
        let od = self.sllq(od, 32);
        self.blend_imm(0xaaaa, ev, od)
    }

    // ---- boolean / mask layer (ZB as vector masks) ----

    /// A mask plane in a register (constants broadcast).
    fn mask_reg(&mut self, m: MaskVal) -> Vreg {
        match m {
            MaskVal::Reg(v) => v,
            MaskVal::Const(b) => {
                let c = if b { -1i32 } else { 0i32 };
                self.pure(Key::BcastD(c), move |d| Inst::BcastD { dst: d, val: c })
            }
        }
    }

    /// A memoized 32-bit-lane binary op (`And/Or/Xor` operands canonical).
    fn dbin(&mut self, op: ROp, a: Vreg, b: Vreg) -> Vreg {
        let tag = match op {
            ROp::AddD => 0u8,
            ROp::SubD => 1,
            ROp::MinSD => 2,
            ROp::MaxSD => 3,
            ROp::AndD => 4,
            ROp::OrD => 5,
            ROp::XorD => 6,
            ROp::AndnD => 7,
        };
        let (a, b) = if matches!(op, ROp::AndD | ROp::OrD | ROp::XorD) && b < a {
            (b, a)
        } else {
            (a, b)
        };
        let key = Key::RBin(tag, a, SrcKey::Reg(b));
        self.pure(key, move |d| Inst::RBin { dst: d, op, a, b: Src::Reg(b) })
    }

    /// Bitwise NOT of a mask (`x ^ all-ones`).
    fn not_mask(&mut self, a: Vreg) -> Vreg {
        let key = Key::RBin(6, a, SrcKey::I32(-1));
        self.pure(key, move |d| Inst::RBin { dst: d, op: ROp::XorD, a, b: Src::BI32(-1) })
    }

    /// Signed 32-bit compare to a vector mask.
    fn cmp(&mut self, imm: u8, a: Vreg, b: Src) -> Vreg {
        self.pure(Key::Cmp(imm, a, b.key()), move |d| Inst::Cmp { dst: d, imm, a, b })
    }

    /// `dst = LUT_imm(a, b, c)` (`vpternlogd`).
    fn ternlog(&mut self, a: Vreg, b: Vreg, c: Vreg, imm: u8) -> Vreg {
        self.pure(Key::Ternlog(a, b, c, imm), move |d| Inst::Ternlog { dst: d, a, b, c, imm })
    }

    /// Vector-mask select `c ? t : f` (one plane), via `vpternlogd 0xca`.
    fn vsel(&mut self, c: Vreg, t: Vreg, f: Vreg) -> Vreg {
        self.ternlog(c, t, f, 0xca)
    }

    // ---- mask planes with the bit identities folded ----
    //
    // A plane that is a compile-time constant (`MaskVal::Const`: all-ones or
    // zero in every lane) or the same register on both sides is folded by an
    // identity that holds BIT FOR BIT in every lane (`x & -1 = x`, `x | -1 =
    // -1`, `x & 0 = 0`, `x & x = x`, `c ? t : t = t`, ...), so the folded
    // kernel computes exactly what the unfolded one did; nothing is decided
    // that was not already a constant.

    /// `p & q`.
    fn m_and(&mut self, p: MaskVal, q: MaskVal) -> MaskVal {
        match (p, q) {
            (MaskVal::Const(false), _) | (_, MaskVal::Const(false)) => MaskVal::Const(false),
            (MaskVal::Const(true), x) | (x, MaskVal::Const(true)) => x,
            (MaskVal::Reg(a), MaskVal::Reg(b)) if a == b => p,
            (MaskVal::Reg(a), MaskVal::Reg(b)) => MaskVal::Reg(self.dbin(ROp::AndD, a, b)),
        }
    }

    /// `p | q`.
    fn m_or(&mut self, p: MaskVal, q: MaskVal) -> MaskVal {
        match (p, q) {
            (MaskVal::Const(true), _) | (_, MaskVal::Const(true)) => MaskVal::Const(true),
            (MaskVal::Const(false), x) | (x, MaskVal::Const(false)) => x,
            (MaskVal::Reg(a), MaskVal::Reg(b)) if a == b => p,
            (MaskVal::Reg(a), MaskVal::Reg(b)) => MaskVal::Reg(self.dbin(ROp::OrD, a, b)),
        }
    }

    /// `~p & q`.
    fn m_andn(&mut self, p: MaskVal, q: MaskVal) -> MaskVal {
        match (p, q) {
            (MaskVal::Const(true), _) | (_, MaskVal::Const(false)) => MaskVal::Const(false),
            (MaskVal::Const(false), x) => x,
            (x, MaskVal::Const(true)) => self.m_not(x),
            (MaskVal::Reg(a), MaskVal::Reg(b)) if a == b => MaskVal::Const(false),
            (MaskVal::Reg(a), MaskVal::Reg(b)) => MaskVal::Reg(self.dbin(ROp::AndnD, a, b)),
        }
    }

    /// `p ^ q`.
    fn m_xor(&mut self, p: MaskVal, q: MaskVal) -> MaskVal {
        match (p, q) {
            (MaskVal::Const(a), MaskVal::Const(b)) => MaskVal::Const(a != b),
            (MaskVal::Const(false), x) | (x, MaskVal::Const(false)) => x,
            (MaskVal::Const(true), x) | (x, MaskVal::Const(true)) => self.m_not(x),
            (MaskVal::Reg(a), MaskVal::Reg(b)) if a == b => MaskVal::Const(false),
            (MaskVal::Reg(a), MaskVal::Reg(b)) => MaskVal::Reg(self.dbin(ROp::XorD, a, b)),
        }
    }

    /// `~p`.
    fn m_not(&mut self, p: MaskVal) -> MaskVal {
        match p {
            MaskVal::Const(b) => MaskVal::Const(!b),
            MaskVal::Reg(a) => MaskVal::Reg(self.not_mask(a)),
        }
    }

    /// `c ? t : f`, one plane.
    fn m_sel(&mut self, c: MaskVal, t: MaskVal, f: MaskVal) -> MaskVal {
        match (c, t, f) {
            (MaskVal::Const(true), t, _) => t,
            (MaskVal::Const(false), _, f) => f,
            (_, t, f) if t == f => t,
            (c, MaskVal::Const(true), MaskVal::Const(false)) => c,
            (c, MaskVal::Const(false), MaskVal::Const(true)) => self.m_not(c),
            (c, MaskVal::Const(true), f) => self.m_or(c, f),
            (c, MaskVal::Const(false), f) => self.m_andn(c, f),
            (c, t, MaskVal::Const(true)) => {
                let nc = self.m_not(c);
                self.m_or(nc, t)
            }
            (c, t, MaskVal::Const(false)) => self.m_and(c, t),
            (MaskVal::Reg(c), MaskVal::Reg(t), MaskVal::Reg(f)) => MaskVal::Reg(self.vsel(c, t, f)),
        }
    }

    /// `c ? t : f` on a numeric plane.
    fn n_sel(&mut self, c: MaskVal, t: NumVal, f: NumVal) -> NumVal {
        match c {
            MaskVal::Const(true) => t,
            MaskVal::Const(false) => f,
            _ if t == f => t,
            MaskVal::Reg(c) => {
                let (tr, fr) = (self.num_reg(t), self.num_reg(f));
                NumVal::Reg(self.vsel(c, tr, fr))
            }
        }
    }

    // ---- interval layer (ZI as two i32 planes) ----

    /// A 32-bit-lane binary op with a broadcast CONSTANT second operand.
    fn dbin_c(&mut self, op: ROp, a: Vreg, c: i32) -> Vreg {
        let tag = match op {
            ROp::AddD => 0u8,
            ROp::SubD => 1,
            ROp::MinSD => 2,
            ROp::MaxSD => 3,
            ROp::AndD => 4,
            ROp::OrD => 5,
            ROp::XorD => 6,
            ROp::AndnD => 7,
        };
        self.pure(Key::RBin(tag, a, SrcKey::I32(c)), move |d| Inst::RBin {
            dst: d,
            op,
            a,
            b: Src::BI32(c),
        })
    }
    fn neg(&mut self, a: Vreg) -> Vreg {
        self.pure(Key::Neg(a), move |d| Inst::Neg { dst: d, a })
    }
    fn absd(&mut self, a: Vreg) -> Vreg {
        self.pure(Key::Abs(a), move |d| Inst::Abs { dst: d, a })
    }
    fn ival_regs(&mut self, iv: [NumVal; 2]) -> [Vreg; 2] {
        [self.num_reg(iv[0]), self.num_reg(iv[1])]
    }
    /// `zn_flr`: `x & 0xffff_0000`.
    fn flr(&mut self, a: Vreg) -> Vreg {
        self.dbin_c(ROp::AndD, a, FLR_MASK)
    }

    /// `mask_eq(a, b)` as a vector mask.
    fn mask_eq(&mut self, a: Vreg, b: Vreg) -> Vreg {
        self.cmp(0, a, Src::Reg(b))
    }

    /// Interval `+`. An overflowing endpoint WRAPS to garbage, so the op has
    /// its own error (`Op::NoWrap`): the lane declines where it is read.
    fn zi_add(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        [self.dbin(ROp::AddD, a[0], b[0]), self.dbin(ROp::AddD, a[1], b[1])]
    }
    /// Interval `-`, as `zi_add`.
    fn zi_sub(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        // Endpoints cross: [a.lo - b.hi, a.hi - b.lo].
        [self.dbin(ROp::SubD, a[0], b[1]), self.dbin(ROp::SubD, a[1], b[0])]
    }
    /// `Op::NoWrap` of interval `+`/`-` with result `r`: overflow iff
    /// `(x^r)&(y^r)` (add) / `(x^y)&(x^r)` (sub) is negative (`kernel::zi_*_wraps`).
    fn zi_arith_no_wrap(&mut self, sub: bool, a: [Vreg; 2], b: [Vreg; 2], r: [Vreg; 2]) -> Vreg {
        let (bl, bh, imm) = if sub { (b[1], b[0], 0x18) } else { (b[0], b[1], 0x42) };
        let ol = self.ternlog(a[0], bl, r[0], imm);
        let oh = self.ternlog(a[1], bh, r[1], imm);
        let any = self.dbin(ROp::OrD, ol, oh);
        let zero = self.num_reg(NumVal::ConstI32(0));
        self.cmp(5, any, Src::Reg(zero)) // any >= 0  (NLT)
    }
    fn zi_min(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        [self.dbin(ROp::MinSD, a[0], b[0]), self.dbin(ROp::MinSD, a[1], b[1])]
    }
    fn zi_max(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        [self.dbin(ROp::MaxSD, a[0], b[0]), self.dbin(ROp::MaxSD, a[1], b[1])]
    }
    /// Interval negation; `-MIN` wraps (checked by `zi_neg_no_wrap`).
    fn zi_neg(&mut self, a: [Vreg; 2]) -> [Vreg; 2] {
        let lo = self.neg(a[1]);
        let hi = self.neg(a[0]);
        [lo, hi]
    }
    /// `Op::NoWrap` of a negation: `lo != MIN` (`kernel::zi_neg_wraps`).
    fn zi_neg_no_wrap(&mut self, a: [Vreg; 2]) -> Vreg {
        let min = self.num_reg(NumVal::ConstI32(i32::MIN));
        self.cmp(4, a[0], Src::Reg(min)) // lo != MIN  (NE)
    }
    /// `Op::NoWrap` of an interval `*` / `/` by a positive literal: the
    /// operation is monotone, so it fits iff `lo >= fits.0` and `hi <=
    /// fits.1` (`graph::Scaled::fits`, `kernel::zi_scale_wraps` /
    /// `zi_div_wraps`). A side the literal cannot overflow is no compare.
    fn zi_scaled_no_wrap(&mut self, a: [Vreg; 2], fits: (i32, i32)) -> MaskVal {
        let lo = if fits.0 == i32::MIN {
            MaskVal::Const(true)
        } else {
            MaskVal::Reg(self.cmp(5, a[0], Src::BI32(fits.0))) // lo >= min  (NLT)
        };
        let hi = if fits.1 == i32::MAX {
            MaskVal::Const(true)
        } else {
            MaskVal::Reg(self.cmp(2, a[1], Src::BI32(fits.1))) // hi <= max  (LE)
        };
        self.m_and(lo, hi)
    }
    /// `zi_abs`: the three-case blend (non-negative / non-positive / straddle).
    fn zi_abs(&mut self, a: [Vreg; 2]) -> [Vreg; 2] {
        let zero = self.num_reg(NumVal::ConstI32(0));
        let pos = self.cmp(5, a[0], Src::Reg(zero)); // lo >= 0  (NLT)
        let neg = self.cmp(2, a[1], Src::Reg(zero)); // hi <= 0  (LE)
        let al = self.absd(a[0]);
        let ah = self.absd(a[1]);
        let m = self.dbin(ROp::MaxSD, al, ah);
        // straddle default: lo=0, hi=max(|lo|,|hi|)
        let mut lo = zero;
        let mut hi = m;
        lo = self.vsel(neg, ah, lo);
        hi = self.vsel(neg, al, hi);
        lo = self.vsel(pos, a[0], lo);
        hi = self.vsel(pos, a[1], hi);
        [lo, hi]
    }
    /// `zi_flr_ok`: does the interval have a unique floor? (val; known=ALL)
    fn zi_flr_ok(&mut self, a: [Vreg; 2]) -> Vreg {
        let fl = self.flr(a[0]);
        let fh = self.flr(a[1]);
        self.mask_eq(fl, fh)
    }
    /// `zi_span_ok(a, ways)`: does it span at most `ways` floors?
    fn zi_span_ok(&mut self, a: [Vreg; 2], ways: u8) -> Vreg {
        let step = 1i32 << 16;
        let fl = self.flr(a[0]);
        let fh = self.flr(a[1]);
        let top = self.dbin_c(ROp::AddD, fl, step * (ways as i32 - 1));
        // fh <= fl + (ways - 1) * step  (vpcmpd LE)
        self.cmp(2, fh, Src::Reg(top))
    }
    /// `zi_fork_flr(a, c)`: the `c`-th floor from the low end's, clipped to
    /// `a`, and whether `a` reaches it.
    fn zi_fork_flr(&mut self, a: [Vreg; 2], c: u8) -> ([Vreg; 2], Vreg) {
        let step = 1i32 << 16;
        let fl = self.flr(a[0]);
        let base = if c == 0 { fl } else { self.dbin_c(ROp::AddD, fl, step * c as i32) };
        let top = self.dbin_c(ROp::AddD, base, step - 1);
        let hi = self.dbin(ROp::MinSD, a[1], top);
        if c == 0 {
            // lo = a.lo (its own cell's base is at or below it); valid = ALL
            let all = self.num_reg(NumVal::ConstI32(-1));
            ([a[0], hi], all)
        } else {
            let fh = self.flr(a[1]);
            let lo = self.dbin(ROp::MaxSD, a[0], base);
            // base <= fh  (vpcmpd LE)
            let valid = self.cmp(2, base, Src::Reg(fh));
            ([lo, hi], valid)
        }
    }
    /// `zi_cmp`: tri-state (val, known); `kind` 0 Lt, 1 Le, 2 Gt, 3 Ge.
    fn zi_cmp(&mut self, kind: u8, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        // imm: LT 1, LE 2, GT 6, GE 5.
        let (t, f) = match kind {
            0 => (self.cmp(1, a[1], Src::Reg(b[0])), self.cmp(5, a[0], Src::Reg(b[1]))),
            1 => (self.cmp(2, a[1], Src::Reg(b[0])), self.cmp(6, a[0], Src::Reg(b[1]))),
            2 => (self.cmp(6, a[0], Src::Reg(b[1])), self.cmp(2, a[1], Src::Reg(b[0]))),
            _ => (self.cmp(5, a[0], Src::Reg(b[1])), self.cmp(1, a[1], Src::Reg(b[0]))),
        };
        let known = self.dbin(ROp::OrD, t, f);
        [t, known]
    }

    /// `id`'s raw value if a positive constant (the only monotone scale/divisor).
    fn pos_const_scalar(&self, id: NodeId) -> Option<i32> {
        match self.g.get(id).op {
            Op::Const(lo, hi) if lo == hi && lo > 0 => Some(lo),
            _ => None,
        }
    }

    fn as_num(&self, id: NodeId) -> Result<NumVal> {
        match self.vals[id as usize] {
            Some(Value::Num(n)) => Ok(n),
            _ => bail!(
                "node {} (op {:?}, domain {}; args {}) is not a numeric value where one was needed; interval source: {}",
                id,
                self.g.get(id).op,
                self.dom(id),
                self.g
                    .get(id)
                    .args
                    .iter()
                    .map(|&a| format!("{a}={:?}/d{}", self.g.get(a).op, self.dom(a)))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.interval_source(id)
            ),
        }
    }

    /// `id`'s subtree to `depth`, with domains (for errors).
    fn describe(&self, id: NodeId, depth: usize) -> String {
        let node = self.g.get(id);
        let known = match self.vals[id as usize] {
            Some(Value::Bool([_, MaskVal::Const(true)])) => "K",
            Some(Value::Bool(_)) => "?",
            _ => "",
        };
        if depth == 0 || node.args.is_empty() {
            return format!("{id}={:?}/d{}{known}", node.op, self.dom(id));
        }
        format!(
            "{id}={:?}/d{}{known}({})",
            node.op,
            self.dom(id),
            node.args.iter().map(|&a| self.describe(a, depth - 1)).collect::<Vec<_>>().join(", ")
        )
    }

    /// The chain of interval operands that made `id` an interval.
    fn interval_source(&self, id: NodeId) -> String {
        let mut out = Vec::new();
        let mut cur = id;
        for _ in 0..64 {
            let node = self.g.get(cur);
            out.push(format!("{cur}={:?}", node.op));
            let next = match node.op {
                Op::Sel => {
                    let arm = node.args[1..].iter().copied().find(|&x| self.dom(x) == 2);
                    if arm.is_none() {
                        out.push(format!("<- condition {}", self.describe(node.args[0], 5)));
                    }
                    arm
                }
                _ => node.args.iter().copied().find(|&x| self.dom(x) == 2),
            };
            match next {
                Some(x) => cur = x,
                None => break,
            }
        }
        out.join(" <- ")
    }

    fn as_bool(&self, id: NodeId) -> Result<[MaskVal; 2]> {
        match self.vals[id as usize] {
            Some(Value::Bool(b)) => Ok(b),
            _ => bail!("node {} is not a boolean value where one was needed", id),
        }
    }

    fn as_ival(&self, id: NodeId) -> Result<[NumVal; 2]> {
        match self.vals[id as usize] {
            Some(Value::Ival(i)) => Ok(i),
            // A number is the degenerate interval [x, x] (zi_of_zn).
            Some(Value::Num(n)) => Ok([n, n]),
            _ => bail!("node {} is not an interval where one was needed", id),
        }
    }

    /// A node's domain: 0 number, 1 boolean, 2 interval.
    fn dom(&self, id: NodeId) -> u8 {
        match self.vals[id as usize] {
            Some(Value::Num(_)) => 0,
            Some(Value::Bool(_)) => 1,
            Some(Value::Ival(_)) => 2,
            None => 0,
        }
    }

    fn lower_node(&mut self, id: NodeId) -> Result<()> {
        let node = self.g.get(id);
        let a = node.args.clone();
        let val = match &node.op {
            Op::Const(lo, hi) => {
                if lo == hi {
                    Value::Num(NumVal::ConstI32(*lo))
                } else {
                    Value::Ival([NumVal::ConstI32(*lo), NumVal::ConstI32(*hi)])
                }
            }
            Op::Cell(c) => {
                let off = *self
                    .cell_off
                    .get(c)
                    .expect("input cell offset assigned before lowering");
                match self.cell_reprs.get(c).copied().unwrap_or(CellRepr::Num) {
                    CellRepr::Num => {
                        let dst =
                            self.pure(Key::Load(off), move |d| Inst::Load { dst: d, off });
                        Value::Num(NumVal::Reg(dst))
                    }
                    CellRepr::Bool => {
                        let dst =
                            self.pure(Key::LoadMask(off), move |d| Inst::LoadMask { dst: d, off });
                        Value::Bool([MaskVal::Reg(dst), MaskVal::Const(true)])
                    }
                    CellRepr::UBool => {
                        let known_off = off + 2;
                        let val = self.pure(Key::LoadMask(off), move |d| Inst::LoadMask { dst: d, off });
                        let known = self.pure(Key::LoadMask(known_off), move |d| Inst::LoadMask { dst: d, off: known_off });
                        Value::Bool([MaskVal::Reg(val), MaskVal::Reg(known)])
                    }
                    CellRepr::Ival => {
                        // Two ZN planes: lo at +0, hi at +64.
                        let hi_off = off + 64;
                        let lo_r = self.pure(Key::Load(off), move |d| Inst::Load { dst: d, off });
                        let hi_r =
                            self.pure(Key::Load(hi_off), move |d| Inst::Load { dst: d, off: hi_off });
                        Value::Ival([NumVal::Reg(lo_r), NumVal::Reg(hi_r)])
                    }
                }
            }
            op @ (Op::Add | Op::Sub | Op::Min | Op::Max) => {
                if self.dom(a[0]) == 2 || self.dom(a[1]) == 2 {
                    let (ia, ib) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (ar, br) = (self.ival_regs(ia), self.ival_regs(ib));
                    let r = match op {
                        Op::Add => self.zi_add(ar, br),
                        Op::Sub => self.zi_sub(ar, br),
                        Op::Min => self.zi_min(ar, br),
                        _ => self.zi_max(ar, br),
                    };
                    self.vals[id as usize] = Some(Value::Ival([NumVal::Reg(r[0]), NumVal::Reg(r[1])]));
                    return Ok(());
                }
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let (rop, tag) = match op {
                    Op::Add => (ROp::AddD, 0u8),
                    Op::Sub => (ROp::SubD, 1),
                    Op::Min => (ROp::MinSD, 2),
                    Op::Max => (ROp::MaxSD, 3),
                    _ => unreachable!(),
                };
                let commutes = matches!(op, Op::Add | Op::Min | Op::Max);
                // Ensure the first operand is a register.
                let (areg, bsrc) = match (x, commutes) {
                    (NumVal::Reg(_), _) => (self.num_reg(x), self.num_src(y)),
                    (NumVal::ConstI32(_), true) if matches!(y, NumVal::Reg(_)) => {
                        (self.num_reg(y), self.num_src(x))
                    }
                    _ => (self.num_reg(x), self.num_src(y)),
                };
                let key = Key::RBin(tag, areg, bsrc.key());
                let dst = self.pure(key, move |d| Inst::RBin { dst: d, op: rop, a: areg, b: bsrc });
                Value::Num(NumVal::Reg(dst))
            }
            Op::Mul => {
                // Interval * positive constant: scale each endpoint. An
                // overflow wraps; it is the op's own error (`Op::NoWrap`,
                // `zi_scaled_no_wrap`), so the lane declines where it is read.
                if self.dom(a[0]) == 2 || self.dom(a[1]) == 2 {
                    let (ivn, scn) = if self.dom(a[0]) == 2 { (a[0], a[1]) } else { (a[1], a[0]) };
                    if self.dom(scn) == 2 {
                        bail!("node {}: Mul of two intervals is not modelled", id);
                    }
                    if self.pos_const_scalar(scn).is_none() {
                        bail!(
                            "node {}: interval Mul by a non-positive / non-constant scalar \
                             is not modelled",
                            id
                        );
                    }
                    let iv = self.as_ival(ivn)?;
                    let ar = self.ival_regs(iv);
                    let s = self.as_num(scn)?;
                    let sreg = self.num_reg(s);
                    let lo = self.zn_mul(ar[0], sreg);
                    let hi = self.zn_mul(ar[1], sreg);
                    Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                } else {
                    let x = self.as_num(a[0])?;
                    let y = self.as_num(a[1])?;
                    let (xr, yr) = (self.num_reg(x), self.num_reg(y));
                    Value::Num(NumVal::Reg(self.zn_mul(xr, yr)))
                }
            }
            // Interval / positive constant: divide each endpoint (before the
            // scalar `Op::Div` arm, which reads `as_num`). The scalar `/`
            // saturates where the interval one has no answer: the op's own
            // error, as `Mul`. A power-of-two divisor (`div_pow2`) is at
            // least 1, so it never does: its `NoWrap` folds to true.
            Op::Div if self.dom(a[0]) == 2 => {
                if self.pos_const_scalar(a[1]).is_none() {
                    bail!(
                        "node {}: interval Div by a non-positive / non-constant scalar \
                         is not modelled",
                        id
                    );
                }
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                if let Some(sh) = self.pow2_divisor(a[1]) {
                    let (lo, hi) = (self.div_pow2(ar[0], sh), self.div_pow2(ar[1], sh));
                    self.vals[id as usize] = Some(Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)]));
                    return Ok(());
                }
                let s = self.as_num(a[1])?;
                let sreg = self.num_reg(s);
                let mut ends = [ar[0]; 2];
                for (k, &e) in ar.iter().enumerate() {
                    let dst = self.fresh();
                    self.insts.push(Inst::Call {
                        op: CallOp::Div,
                        dst,
                        args: vec![e, sreg],
                        scalars: Vec::new(),
                    });
                    ends[k] = dst;
                }
                Value::Ival([NumVal::Reg(ends[0]), NumVal::Reg(ends[1])])
            }
            Op::Neg => {
                if self.dom(a[0]) == 2 {
                    let iv = self.as_ival(a[0])?;
                    let ar = self.ival_regs(iv);
                    let r = self.zi_neg(ar);
                    Value::Ival([NumVal::Reg(r[0]), NumVal::Reg(r[1])])
                } else {
                    let n = self.as_num(a[0])?;
                    let areg = self.num_reg(n);
                    let dst = self.pure(Key::Neg(areg), move |d| Inst::Neg { dst: d, a: areg });
                    Value::Num(NumVal::Reg(dst))
                }
            }
            Op::Abs => {
                if self.dom(a[0]) == 2 {
                    let iv = self.as_ival(a[0])?;
                    let ar = self.ival_regs(iv);
                    let r = self.zi_abs(ar);
                    Value::Ival([NumVal::Reg(r[0]), NumVal::Reg(r[1])])
                } else {
                    let n = self.as_num(a[0])?;
                    let areg = self.num_reg(n);
                    let dst = self.pure(Key::Abs(areg), move |d| Inst::Abs { dst: d, a: areg });
                    Value::Num(NumVal::Reg(dst))
                }
            }
            Op::Flr => {
                // The low endpoint's floor (a number is a degenerate interval).
                let iv = self.as_ival(a[0])?;
                let lo = self.num_reg(iv[0]);
                Value::Num(NumVal::Reg(self.flr(lo)))
            }
            Op::ConstBool(b) => Value::Bool([MaskVal::Const(*b), MaskVal::Const(true)]),
            // Undecided in every lane: the known plane is all zeros.
            Op::UnknownBool(_) => Value::Bool([MaskVal::Const(false), MaskVal::Const(false)]),
            // `emit::bind` keeps it out of every root's cone.
            Op::UnknownNum => bail!("node {}: an unknown number reached the kernel", id),
            op @ (Op::Lt | Op::Le | Op::Gt | Op::Ge) => {
                if self.dom(a[0]) == 2 || self.dom(a[1]) == 2 {
                    // Interval comparison -> tri-state ZB (zi_cmp).
                    let kind = match op {
                        Op::Lt => 0u8,
                        Op::Le => 1,
                        Op::Gt => 2,
                        _ => 3,
                    };
                    let (ia, ib) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let (ar, br) = (self.ival_regs(ia), self.ival_regs(ib));
                    let r = self.zi_cmp(kind, ar, br);
                    Value::Bool([MaskVal::Reg(r[0]), MaskVal::Reg(r[1])])
                } else {
                    // Numeric: known. LT 1, LE 2, GT 6 (NLE), GE 5 (NLT).
                    let imm = match op {
                        Op::Lt => 1u8,
                        Op::Le => 2,
                        Op::Gt => 6,
                        _ => 5,
                    };
                    let x = self.as_num(a[0])?;
                    let y = self.as_num(a[1])?;
                    let xr = self.num_reg(x);
                    let val = self.cmp(imm, xr, self.num_src(y));
                    Value::Bool([MaskVal::Reg(val), MaskVal::Const(true)])
                }
            }
            Op::Eq => {
                match self.dom(a[0]) {
                    _ if self.dom(a[0]) == 2 || self.dom(a[1]) == 2 => {
                        // As `Graph::compare`: decided if disjoint or both points.
                        let (ia, ib) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                        let (ar, br) = (self.ival_regs(ia), self.ival_regs(ib));
                        let a_single = self.cmp(0, ar[0], Src::Reg(ar[1]));
                        let b_single = self.cmp(0, br[0], Src::Reg(br[1]));
                        let both = self.dbin(ROp::AndD, a_single, b_single);
                        let lo_eq = self.cmp(0, ar[0], Src::Reg(br[0]));
                        let val = self.dbin(ROp::AndD, both, lo_eq);
                        // a.lo > b.hi (vpcmpd NLE) or b.lo > a.hi.
                        let d1 = self.cmp(6, ar[0], Src::Reg(br[1]));
                        let d2 = self.cmp(6, br[0], Src::Reg(ar[1]));
                        let disjoint = self.dbin(ROp::OrD, d1, d2);
                        let known = self.dbin(ROp::OrD, both, disjoint);
                        Value::Bool([MaskVal::Reg(val), MaskVal::Reg(known)])
                    }
                    1 => {
                        let (p, q) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                        // val = ~(pv ^ qv), known = pk & qk
                        let x = self.m_xor(p[0], q[0]);
                        let val = self.m_not(x);
                        let known = self.m_and(p[1], q[1]);
                        Value::Bool([val, known])
                    }
                    _ => {
                        let x = self.as_num(a[0])?;
                        let y = self.as_num(a[1])?;
                        let xr = self.num_reg(x);
                        let val = self.cmp(0, xr, self.num_src(y));
                        Value::Bool([MaskVal::Reg(val), MaskVal::Const(true)])
                    }
                }
            }
            Op::Not => {
                let b = self.as_bool(a[0])?;
                let nv = self.m_not(b[0]);
                Value::Bool([nv, b[1]])
            }
            op @ (Op::And | Op::Or) => {
                let (p, q) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                let ([pv, pk], [qv, qk]) = (p, q);
                // Kleene, per lane; the identities fold decided planes.
                let kk = self.m_and(pk, qk);
                if matches!(op, Op::And) {
                    // known = (pk & qk) | (~pv & pk) | (~qv & qk)
                    let val = self.m_and(pv, qv);
                    let kfa = self.m_andn(pv, pk);
                    let kfb = self.m_andn(qv, qk);
                    let kf = self.m_or(kfa, kfb);
                    let known = self.m_or(kk, kf);
                    Value::Bool([val, known])
                } else {
                    // known = (pk & qk) | (pv & pk) | (qv & qk)
                    let val = self.m_or(pv, qv);
                    let kta = self.m_and(pv, pk);
                    let ktb = self.m_and(qv, qk);
                    let kt = self.m_or(kta, ktb);
                    let known = self.m_or(kk, kt);
                    Value::Bool([val, known])
                }
            }
            Op::Known => {
                // `Known(Flr(x))` reads the INTERVAL `x` (zi_flr_ok): the
                // result is exact BY this premise and would answer itself.
                let inner = a[0];
                if let Op::Flr = self.g.get(inner).op {
                    let src = self.g.get(inner).args[0];
                    if self.dom(src) == 2 {
                        let iv = self.as_ival(src)?;
                        let ar = self.ival_regs(iv);
                        let val = self.zi_flr_ok(ar);
                        return {
                            self.vals[id as usize] =
                                Some(Value::Bool([MaskVal::Reg(val), MaskVal::Const(true)]));
                            Ok(())
                        };
                    }
                }
                match self.dom(inner) {
                    1 => {
                        let b = self.as_bool(inner)?;
                        Value::Bool([b[1], MaskVal::Const(true)])
                    }
                    2 => {
                        // A number is decided iff lo == hi.
                        let iv = self.as_ival(inner)?;
                        let ar = self.ival_regs(iv);
                        let val = self.mask_eq(ar[0], ar[1]);
                        Value::Bool([MaskVal::Reg(val), MaskVal::Const(true)])
                    }
                    _ => Value::Bool([MaskVal::Const(true), MaskVal::Const(true)]),
                }
            }
            Op::Lo | Op::Hi => {
                if self.dom(a[0]) == 2 {
                    let iv = self.as_ival(a[0])?;
                    Value::Num(iv[if matches!(node.op, Op::Lo) { 0 } else { 1 }])
                } else {
                    Value::Num(self.as_num(a[0])?)
                }
            }
            Op::Sel => {
                let cv = self.as_bool(a[0])?[0];
                // On the JOINED arm domain (a number beside an interval is [n, n]).
                match self.dom(a[1]).max(self.dom(a[2])) {
                    1 => {
                        let (t, f) = (self.as_bool(a[1])?, self.as_bool(a[2])?);
                        let val = self.m_sel(cv, t[0], f[0]);
                        let known = self.m_sel(cv, t[1], f[1]);
                        Value::Bool([val, known])
                    }
                    2 => {
                        let (t, f) = (self.as_ival(a[1])?, self.as_ival(a[2])?);
                        let lo = self.n_sel(cv, t[0], f[0]);
                        let hi = self.n_sel(cv, t[1], f[1]);
                        Value::Ival([lo, hi])
                    }
                    _ => {
                        let (t, f) = (self.as_num(a[1])?, self.as_num(a[2])?);
                        Value::Num(self.n_sel(cv, t, f))
                    }
                }
            }
            Op::Span => {
                // The hull [lo of arg0, hi of arg1].
                let a0 = self.as_ival(a[0])?;
                let a1 = self.as_ival(a[1])?;
                Value::Ival([a0[0], a1[1]])
            }
            Op::Frag(c) => {
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                let (frag, _) = self.zi_fork_flr(ar, *c);
                Value::Ival([NumVal::Reg(frag[0]), NumVal::Reg(frag[1])])
            }
            // The operand unchanged: the range is the node's own error
            // (`trace::error`), checked on the raw operand.
            Op::Restrict(..) => match self.vals[a[0] as usize] {
                Some(v) => v,
                None => bail!("node {}: its operand was not lowered", id),
            },
            Op::FragOk(c) => {
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                let (_, ok) = self.zi_fork_flr(ar, *c);
                Value::Bool([MaskVal::Reg(ok), MaskVal::Const(true)])
            }
            // The low end of fragment `c` as an EXACT number.
            Op::IntFrag(c) => {
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                let (frag, _) = self.zi_fork_flr(ar, *c);
                Value::Num(NumVal::Reg(frag[0]))
            }
            Op::SplitOk(ways) => {
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                let ok = self.zi_span_ok(ar, *ways);
                Value::Bool([MaskVal::Reg(ok), MaskVal::Const(true)])
            }
            // The own error of an interval `+`, `-` or negation; an EXACT one
            // wraps as PICO-8 does, so it holds.
            Op::NoWrap => {
                let x = a[0];
                let xn = self.g.get(x);
                let ok = match (&xn.op, self.dom(x)) {
                    (op @ (Op::Add | Op::Sub), 2) => {
                        let (p, q) = (xn.args[0], xn.args[1]);
                        let sub = matches!(op, Op::Sub);
                        let (ip, iq, ir) = (self.as_ival(p)?, self.as_ival(q)?, self.as_ival(x)?);
                        let (pr, qr, rr) = (self.ival_regs(ip), self.ival_regs(iq), self.ival_regs(ir));
                        MaskVal::Reg(self.zi_arith_no_wrap(sub, pr, qr, rr))
                    }
                    (Op::Neg, 2) => {
                        let ip = self.as_ival(xn.args[0])?;
                        let pr = self.ival_regs(ip);
                        MaskVal::Reg(self.zi_neg_no_wrap(pr))
                    }
                    (Op::Mul | Op::Div, 2) => {
                        let Some(sc) = graph::scaled(self.g, x) else {
                            bail!("node {}: NoWrap of an interval {:?} by a non-positive / non-constant scalar is not modelled", id, xn.op);
                        };
                        let operand = xn.args[sc.operand];
                        if self.dom(operand) != 2 {
                            bail!("node {}: NoWrap of {:?}: the interval is not the operand `scaled` names", id, xn.op);
                        }
                        let ip = self.as_ival(operand)?;
                        let pr = self.ival_regs(ip);
                        self.zi_scaled_no_wrap(pr, sc.fits())
                    }
                    // Exact, or rewritten into another op (as `Graph::fold`).
                    _ => MaskVal::Const(true),
                };
                Value::Bool([ok, MaskVal::Const(true)])
            }
            Op::Div if self.pow2_divisor(a[1]).is_some() => {
                let sh = self.pow2_divisor(a[1]).expect("guarded");
                let x = self.as_num(a[0])?;
                let xr = self.num_reg(x);
                Value::Num(NumVal::Reg(self.div_pow2(xr, sh)))
            }
            Op::Rem if self.dom(a[0]) != 2 && self.pow2_modulus_mask(a[1]).is_some() => {
                let m = self.pow2_modulus_mask(a[1]).expect("guarded");
                let x = self.as_num(a[0])?;
                let xr = self.num_reg(x);
                Value::Num(NumVal::Reg(self.dbin_c(ROp::AndD, xr, m)))
            }
            Op::Div | Op::Rem | Op::Sin | Op::Mget => {
                let (op, n_args) = match &node.op {
                    Op::Div => (CallOp::Div, 2),
                    Op::Rem => (CallOp::Rem, 2),
                    Op::Sin => (CallOp::Sin, 1),
                    _ => (CallOp::Mget, 2),
                };
                let mut args = Vec::with_capacity(n_args);
                for k in 0..n_args {
                    let n = self.as_num(a[k])?;
                    args.push(self.num_reg(n));
                }
                let dst = self.fresh();
                self.insts.push(Inst::Call { op, dst, args, scalars: Vec::new() });
                Value::Num(NumVal::Reg(dst))
            }
            Op::TileFlagAt => {
                let nx = self.as_num(a[0])?;
                let x = self.num_reg(nx);
                let ny = self.as_num(a[1])?;
                let y = self.num_reg(ny);
                let (wv, hv, fv) = (self.as_num(a[2])?, self.as_num(a[3])?, self.as_num(a[4])?);
                let flag = match fv {
                    NumVal::ConstI32(c) => c,
                    _ => bail!("node {}: tile_flag flag must be an exact constant", id),
                };
                let dst = self.fresh();
                match (wv, hv) {
                    (NumVal::ConstI32(w), NumVal::ConstI32(h)) => {
                        self.insts.push(Inst::Call {
                            op: CallOp::TileFlag,
                            dst,
                            args: vec![x, y],
                            scalars: vec![w, h, flag],
                        });
                    }
                    _ => {
                        let wr = self.num_reg(wv);
                        let hr = self.num_reg(hv);
                        self.insts.push(Inst::Call {
                            op: CallOp::TileFlagLanes,
                            dst,
                            args: vec![x, y, wr, hr],
                            scalars: vec![flag],
                        });
                    }
                }
                Value::Bool([MaskVal::Reg(dst), MaskVal::Const(true)])
            }
            other => bail!("node {}: op {:?} is not supported by the asm slice", id, other),
        };
        self.vals[id as usize] = Some(val);
        Ok(())
    }
}

/// A node lowering order over BATCHES of roots: each batch's unplaced cone
/// depth-major (ILP) while only ~`batch` roots' temporaries are live.
fn schedule(g: &Graph, live: &[bool], roots: &[NodeId], batch: usize) -> Vec<NodeId> {
    // ALAP depth (consumers have larger ids; operands are strictly shallower,
    // so depth-major is a valid order) keeps shared live ranges short.
    let maxd = g.len() as u32;
    let mut depth = vec![maxd; g.len()];
    for id in (0..g.len() as NodeId).rev() {
        if !live[id as usize] {
            continue;
        }
        let d = depth[id as usize];
        for a in &g.get(id).args {
            let e = &mut depth[*a as usize];
            *e = (*e).min(d.saturating_sub(1));
        }
    }
    let batch = batch.max(1);
    let mut scheduled = vec![false; g.len()];
    let mut order: Vec<NodeId> = Vec::new();
    let mut stack: Vec<NodeId> = Vec::new();
    for chunk in roots.chunks(batch) {
        let mut mine: Vec<NodeId> = Vec::new();
        stack.extend_from_slice(chunk);
        let mut seen_local = vec![false; g.len()];
        while let Some(n) = stack.pop() {
            if seen_local[n as usize] {
                continue;
            }
            seen_local[n as usize] = true;
            if scheduled[n as usize] {
                continue; // already placed by an earlier batch (shared)
            }
            if live[n as usize] {
                mine.push(n);
            }
            stack.extend(g.get(n).args.iter().copied());
        }
        mine.sort_by_key(|id| (depth[*id as usize], *id));
        for id in mine {
            if !scheduled[id as usize] {
                scheduled[id as usize] = true;
                order.push(id);
            }
        }
    }
    order
}

/// Nodes reachable from `roots`.
fn reachable(g: &Graph, roots: &[NodeId]) -> Vec<bool> {
    let mut live = vec![false; g.len()];
    let mut stack = roots.to_vec();
    while let Some(n) = stack.pop() {
        if live[n as usize] {
            continue;
        }
        live[n as usize] = true;
        stack.extend(g.get(n).args.iter().copied());
    }
    live
}

// ---- register allocation: linear scan ----

#[derive(Clone, Copy)]
enum Loc {
    Reg(u8),
    Spill(u32),
}

/// Operand vregs used by an instruction (for liveness).
fn inst_uses(inst: &Inst, out: &mut Vec<Vreg>) {
    let push_src = |s: &Src, out: &mut Vec<Vreg>| {
        if let Src::Reg(v) = s {
            out.push(*v);
        }
    };
    match inst {
        Inst::Load { .. } | Inst::LoadMask { .. } | Inst::BcastD { .. } => {}
        Inst::RBin { a, b, .. } => {
            out.push(*a);
            push_src(b, out);
        }
        Inst::Neg { a, .. } | Inst::Abs { a, .. } => out.push(*a),
        Inst::Muldq { a, b, .. } | Inst::BlendImm { a, b, .. } => {
            out.push(*a);
            out.push(*b);
        }
        Inst::Sraq { a, .. } | Inst::Sllq { a, .. } | Inst::ShrD { a, .. } => out.push(*a),
        Inst::Cmp { a, b, .. } => {
            out.push(*a);
            push_src(b, out);
        }
        Inst::Ternlog { a, b, c, .. } => {
            out.push(*a);
            out.push(*b);
            out.push(*c);
        }
        Inst::Call { args, .. } => out.extend(args.iter().copied()),
        Inst::Store { src, .. } => push_src(src, out),
        Inst::StoreMask { src, .. } => out.push(*src),
    }
}

/// List-schedule the SSA stream: highest critical path first, to hide latency.
fn reschedule(insts: Vec<Inst>, n_vregs: Vreg) -> (Vec<Inst>, Vec<u32>) {
    let n = insts.len();
    let mut def_inst = vec![u32::MAX; n_vregs as usize];
    for (i, inst) in insts.iter().enumerate() {
        if let Some(d) = inst_def(inst) {
            def_inst[d as usize] = i as u32;
        }
    }
    // Unique operands, dependencies, consumers.
    let mut uses: Vec<Vec<Vreg>> = vec![Vec::new(); n];
    let mut consumers: Vec<Vec<u32>> = vec![Vec::new(); n];
    let mut indeg = vec![0u32; n];
    let mut buf = Vec::new();
    for (i, inst) in insts.iter().enumerate() {
        buf.clear();
        inst_uses(inst, &mut buf);
        buf.sort_unstable();
        buf.dedup();
        for v in &buf {
            let d = def_inst[*v as usize];
            if d != u32::MAX {
                consumers[d as usize].push(i as u32);
                indeg[i] += 1;
            }
        }
        uses[i] = buf.clone();
    }
    // Critical-path height (consumers have larger original ids).
    let mut height = vec![1u32; n];
    for i in (0..n).rev() {
        let h = consumers[i].iter().map(|c| height[*c as usize]).max().map(|m| m + 1).unwrap_or(1);
        height[i] = h;
    }

    // Above `limit` live registers, FREE registers instead (critical path
    // alone starts every chain and spills). `rem`: unemitted consumers.
    let limit: usize = 16;
    let mut rem: Vec<u32> = vec![0; n_vregs as usize];
    for i in 0..n {
        for v in &uses[i] {
            rem[*v as usize] += 1;
        }
    }
    // The ready set: one lazily invalidated heap per objective, O(n log n)
    // (a flat scan is quadratic: minutes on big kernels).
    use std::cmp::Reverse;
    use std::collections::BinaryHeap;
    let delta = |i: u32, rem: &[u32]| -> i32 {
        let i = i as usize;
        let dies = uses[i].iter().filter(|v| rem[**v as usize] == 1).count() as i32;
        let gains = match inst_def(&insts[i]) {
            Some(_) if !consumers[i].is_empty() => 1,
            _ => 0,
        };
        gains - dies
    };
    // by_h: highest height, then smallest delta, then smallest id.
    let mut by_h: BinaryHeap<(u32, Reverse<i32>, Reverse<u32>)> = BinaryHeap::new();
    // by_d: smallest delta, then highest height, then smallest id.
    let mut by_d: BinaryHeap<(Reverse<i32>, u32, Reverse<u32>)> = BinaryHeap::new();
    let mut done = vec![false; n];
    let push = |i: u32, rem: &[u32], by_h: &mut BinaryHeap<(u32, Reverse<i32>, Reverse<u32>)>, by_d: &mut BinaryHeap<(Reverse<i32>, u32, Reverse<u32>)>| {
        let d = delta(i, rem);
        by_h.push((height[i as usize], Reverse(d), Reverse(i)));
        by_d.push((Reverse(d), height[i as usize], Reverse(i)));
    };
    for i in 0..n as u32 {
        if indeg[i as usize] == 0 {
            push(i, &rem, &mut by_h, &mut by_d);
        }
    }
    let mut order: Vec<u32> = Vec::with_capacity(n);
    let mut live: usize = 0;
    while order.len() < n {
        let tight = live >= limit;
        let i = if tight {
            loop {
                let Some((Reverse(d), h, Reverse(i))) = by_d.pop() else { break None };
                if done[i as usize] {
                    continue;
                }
                let d2 = delta(i, &rem);
                if d2 != d {
                    by_d.push((Reverse(d2), h, Reverse(i)));
                    continue;
                }
                break Some(i);
            }
        } else {
            loop {
                let Some((_, _, Reverse(i))) = by_h.pop() else { break None };
                if done[i as usize] {
                    continue;
                }
                break Some(i);
            }
        };
        let Some(i) = i else { break };
        done[i as usize] = true;
        order.push(i);
        live = (live as i32 + delta(i, &rem)).max(0) as usize;
        for v in &uses[i as usize] {
            rem[*v as usize] -= 1;
            // The last consumer just got cheaper: refresh its pressure key.
            if rem[*v as usize] == 1 {
                let d = def_inst[*v as usize];
                if d != u32::MAX {
                    for &c in &consumers[d as usize] {
                        if !done[c as usize] && indeg[c as usize] == 0 {
                            let dc = delta(c, &rem);
                            by_d.push((Reverse(dc), height[c as usize], Reverse(c)));
                        }
                    }
                }
            }
        }
        for &c in &consumers[i as usize] {
            indeg[c as usize] -= 1;
            if indeg[c as usize] == 0 {
                push(c, &rem, &mut by_h, &mut by_d);
            }
        }
    }
    assert_eq!(order.len(), n, "list scheduler dropped instructions");
    let mut slots: Vec<Option<Inst>> = insts.into_iter().map(Some).collect();
    let out = order.iter().map(|&i| slots[i as usize].take().unwrap()).collect();
    (out, order)
}

fn inst_def(inst: &Inst) -> Option<Vreg> {
    match inst {
        Inst::Load { dst, .. }
        | Inst::LoadMask { dst, .. }
        | Inst::BcastD { dst, .. }
        | Inst::RBin { dst, .. }
        | Inst::Neg { dst, .. }
        | Inst::Abs { dst, .. }
        | Inst::Muldq { dst, .. }
        | Inst::Sraq { dst, .. }
        | Inst::Sllq { dst, .. }
        | Inst::ShrD { dst, .. }
        | Inst::BlendImm { dst, .. }
        | Inst::Cmp { dst, .. }
        | Inst::Ternlog { dst, .. }
        | Inst::Call { dst, .. } => Some(*dst),
        Inst::Store { .. } | Inst::StoreMask { .. } => None,
    }
}

/// Poletto-Sarkar linear scan: every vreg's home and the spill slot count.
fn allocate(insts: &[Inst], n_vregs: Vreg, remat: &[bool]) -> (Vec<Loc>, usize) {
    let mut def = vec![u32::MAX; n_vregs as usize];
    let mut last = vec![0u32; n_vregs as usize];
    let mut buf = Vec::new();
    for (i, inst) in insts.iter().enumerate() {
        if let Some(d) = inst_def(inst) {
            if def[d as usize] == u32::MAX {
                def[d as usize] = i as u32;
            }
            last[d as usize] = last[d as usize].max(i as u32);
        }
        buf.clear();
        inst_uses(inst, &mut buf);
        for v in &buf {
            last[*v as usize] = last[*v as usize].max(i as u32);
        }
    }

    // Intervals for vregs that are actually defined.
    let mut ivs: Vec<(Vreg, u32, u32)> = (0..n_vregs)
        .filter(|v| def[*v as usize] != u32::MAX)
        .map(|v| (v, def[v as usize], last[v as usize]))
        .collect();
    ivs.sort_by_key(|(_, s, _)| *s);

    let mut home = vec![Loc::Spill(u32::MAX); n_vregs as usize];
    let mut free: Vec<u8> = (0..N_ALLOC).rev().collect();
    // active: indices into `ivs`, kept sorted by end ascending.
    let mut active: Vec<usize> = Vec::new();
    let mut n_slots: u32 = 0;
    // Spill slots are reused: a freed slot (with its end) goes only to an
    // interval starting after it (stealing spills intervals already begun).
    let mut free_slots: std::collections::BinaryHeap<std::cmp::Reverse<(u32, u32)>> = Default::default();
    // Spilled intervals holding a slot: (end, slot), earliest end first.
    let mut spilled: std::collections::BinaryHeap<std::cmp::Reverse<(u32, u32)>> = Default::default();
    // A rematerializable value (a load) takes no slot: reloaded at each use.
    let spill_loc = |(vreg, start, end): (Vreg, u32, u32), n_slots: &mut u32, free_slots: &mut std::collections::BinaryHeap<std::cmp::Reverse<(u32, u32)>>, spilled: &mut std::collections::BinaryHeap<std::cmp::Reverse<(u32, u32)>>| -> Loc {
        if remat[vreg as usize] {
            return Loc::Spill(u32::MAX);
        }
        let s = match free_slots.peek() {
            Some(std::cmp::Reverse((freed_at, s))) if *freed_at < start => {
                let s = *s;
                free_slots.pop();
                s
            }
            _ => {
                let s = *n_slots;
                *n_slots += 1;
                s
            }
        };
        spilled.push(std::cmp::Reverse((end, s)));
        Loc::Spill(s)
    };

    for cur in 0..ivs.len() {
        let (vreg, start, _end) = ivs[cur];
        // Expire.
        active.retain(|&ai| {
            if ivs[ai].2 < start {
                if let Loc::Reg(r) = home[ivs[ai].0 as usize] {
                    free.push(r);
                }
                false
            } else {
                true
            }
        });
        while let Some(&std::cmp::Reverse((end, s))) = spilled.peek() {
            if end >= start {
                break;
            }
            spilled.pop();
            free_slots.push(std::cmp::Reverse((end, s)));
        }
        if let Some(r) = free.pop() {
            home[vreg as usize] = Loc::Reg(r);
            active.push(cur);
            active.sort_by_key(|&ai| ivs[ai].2);
        } else {
            // Spill the interval that ends latest (current or an active).
            let spill_ai = *active.last().unwrap();
            if ivs[spill_ai].2 > ivs[cur].2 {
                // Steal its register.
                let stolen = match home[ivs[spill_ai].0 as usize] {
                    Loc::Reg(r) => r,
                    _ => unreachable!(),
                };
                home[vreg as usize] = Loc::Reg(stolen);
                home[ivs[spill_ai].0 as usize] = spill_loc(ivs[spill_ai], &mut n_slots, &mut free_slots, &mut spilled);
                active.pop();
                active.push(cur);
                active.sort_by_key(|&ai| ivs[ai].2);
            } else {
                home[vreg as usize] = spill_loc(ivs[cur], &mut n_slots, &mut free_slots, &mut spilled);
            }
        }
    }
    (home, n_slots as usize)
}

/// Per call (by instruction index), which registers to SAVE before it and
/// RESTORE after it. Live across a call: a register holding a value defined
/// before it and read after it (a vreg keeps one home for its whole interval,
/// `allocate` never splits; a call's operands are marshalled before it, its
/// result written after). A value live across two CONSECUTIVE calls and not
/// read between them (nor as the second's operand) stays in the save area:
/// not restored after the first, not saved again before the second. Its
/// register holds nothing anyone reads in between (no other vreg has it
/// while this one is live), and its save slot (one per register) is written
/// by no other save meanwhile.
fn call_saves(insts: &[Inst], n_vregs: Vreg, home: &[Loc]) -> HashMap<usize, (u32, u32)> {
    let calls: Vec<u32> = (0..insts.len() as u32).filter(|&i| matches!(insts[i as usize], Inst::Call { .. })).collect();
    let mut live = vec![0u32; calls.len()];
    // Bit r of `carry[k]`: register r's value stays saved from call k to k+1.
    let mut carry = vec![0u32; calls.len()];
    let mut def = vec![u32::MAX; n_vregs as usize];
    let mut uses: Vec<Vec<u32>> = vec![Vec::new(); n_vregs as usize];
    let mut buf = Vec::new();
    for (i, inst) in insts.iter().enumerate() {
        if let Some(d) = inst_def(inst) {
            def[d as usize] = def[d as usize].min(i as u32);
        }
        buf.clear();
        inst_uses(inst, &mut buf);
        for v in &buf {
            uses[*v as usize].push(i as u32);
        }
    }
    for v in 0..n_vregs as usize {
        let Loc::Reg(r) = home[v] else { continue };
        let Some(&last) = uses[v].last() else { continue };
        if def[v] == u32::MAX {
            continue;
        }
        // Calls strictly inside (def, last).
        let from = calls.partition_point(|&c| c <= def[v]);
        let to = calls.partition_point(|&c| c < last).max(from);
        for k in from..to {
            live[k] |= 1 << r;
            // Read in (calls[k], calls[k + 1]]?
            if k + 1 < to {
                let at = uses[v].partition_point(|&u| u <= calls[k]);
                if uses[v].get(at).is_none_or(|&u| u > calls[k + 1]) {
                    carry[k] |= 1 << r;
                }
            }
        }
    }
    (0..calls.len())
        .map(|k| {
            let carried_in = if k > 0 { carry[k - 1] } else { 0 };
            (calls[k] as usize, (live[k] & !carried_in, live[k] & !carry[k]))
        })
        .collect()
}

// ---- emission ----

struct Emitter<'a> {
    home: &'a [Loc],
    insts: &'a [Inst],
    remat: &'a [bool],
    def_of: &'a [u32],
    /// rsp offset of the 32-zmm save area used around a call.
    save_off: u32,
    /// Per instruction index of a call: the registers (bit r for zmm r) to
    /// save before it and to restore after it (`call_saves`).
    call_saves: &'a HashMap<usize, (u32, u32)>,
    /// The last instruction reading `ZERO` (a `Neg`): a call before it
    /// re-zeroes it.
    last_neg: Option<usize>,
    /// rsp offset of the call-out argument/result buffers.
    argbuf_off: u32,
    pool: Pool,
    out: String,
}

impl<'a> Emitter<'a> {
    fn slot_mem(slot: u32) -> String {
        format!("{}(%rsp)", slot as usize * 64)
    }

    /// `v` in a register: its home, else `scratch` reloaded or rematerialized.
    fn use_reg(&mut self, v: Vreg, scratch: u8) -> u8 {
        match self.home[v as usize] {
            Loc::Reg(r) => r,
            Loc::Spill(_) if self.remat[v as usize] => {
                self.rematerialize(v, scratch);
                scratch
            }
            Loc::Spill(slot) => {
                writeln!(self.out, "    vmovdqu64 {}, %zmm{}", Self::slot_mem(slot), scratch)
                    .unwrap();
                scratch
            }
        }
    }

    /// Recompute a remat vreg into scratch `dst` from `%r13` alone (outside
    /// its live range no register home may be read).
    fn rematerialize(&mut self, v: Vreg, dst: u8) {
        match &self.insts[self.def_of[v as usize] as usize] {
            Inst::Load { off, .. } => {
                writeln!(self.out, "    vmovdqu64 {}(%r13), %zmm{}", off, dst).unwrap();
            }
            &Inst::BcastD { val, .. } => self.constant(val, dst),
            other => {
                unreachable!("non-rematerializable def marked remat: {:?}", std::mem::discriminant(other))
            }
        }
    }

    /// The constant `val` in every lane of `dst`: zero by the zeroing idiom
    /// (no memory, no dependency), anything else broadcast from the pool (a
    /// load, like the reload it replaces; `vpternlogd $0xff` for all-ones
    /// would take a logic port, the kernels' busiest, and depends on `dst`).
    fn constant(&mut self, val: i32, dst: u8) {
        match val {
            0 => writeln!(self.out, "    vpxord %zmm{dst}, %zmm{dst}, %zmm{dst}").unwrap(),
            _ => {
                let l = self.pool.d(val);
                writeln!(self.out, "    vpbroadcastd {l}(%rip), %zmm{dst}").unwrap();
            }
        }
    }

    /// Where a defined vreg is written: its home reg, or `RES` if spilled.
    fn def_reg(&self, v: Vreg) -> u8 {
        match self.home[v as usize] {
            Loc::Reg(r) => r,
            Loc::Spill(_) => RES,
        }
    }
    fn store_def(&mut self, v: Vreg, reg: u8) {
        if self.remat[v as usize] {
            return;
        }
        if let Loc::Spill(slot) = self.home[v as usize] {
            writeln!(self.out, "    vmovdqu64 %zmm{}, {}", reg, Self::slot_mem(slot)).unwrap();
        }
    }

    /// A spilled remat value's def is skipped (recomputed at every use).
    fn skip_def(&self, v: Vreg) -> bool {
        self.remat[v as usize] && matches!(self.home[v as usize], Loc::Spill(_))
    }

    /// A `Src` as an operand (register or 1to16 broadcast memory).
    fn src_operand(&mut self, s: Src, scratch: u8) -> String {
        match s {
            Src::Reg(v) => format!("%zmm{}", self.use_reg(v, scratch)),
            Src::BI32(c) => {
                let l = self.pool.d(c);
                format!("{l}(%rip){{1to16}}")
            }
        }
    }

    fn emit_inst(&mut self, k: usize, inst: &Inst) {
        match inst {
            Inst::Load { dst, off } => {
                if self.skip_def(*dst) {
                    return;
                }
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vmovdqu64 {}(%r13), %zmm{}", off, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::LoadMask { dst, off } => {
                if self.skip_def(*dst) {
                    return;
                }
                let d = self.def_reg(*dst);
                writeln!(self.out, "    movzwl {}(%r13), %eax", off).unwrap();
                writeln!(self.out, "    kmovw %eax, %k1").unwrap();
                writeln!(self.out, "    vpmovm2d %k1, %zmm{}", d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::BcastD { dst, val } => {
                if self.skip_def(*dst) {
                    return;
                }
                let d = self.def_reg(*dst);
                self.constant(*val, d);
            }
            Inst::RBin { dst, op, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let bop = self.src_operand(*b, OPB);
                let d = self.def_reg(*dst);
                let mn = match op {
                    ROp::AddD => "vpaddd",
                    ROp::SubD => "vpsubd",
                    ROp::MinSD => "vpminsd",
                    ROp::MaxSD => "vpmaxsd",
                    ROp::AndD => "vpandd",
                    ROp::OrD => "vpord",
                    ROp::XorD => "vpxord",
                    ROp::AndnD => "vpandnd",
                };
                writeln!(self.out, "    {} {}, %zmm{}, %zmm{}", mn, bop, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Neg { dst, a } => {
                let ra = self.use_reg(*a, OPA);
                let d = self.def_reg(*dst);
                // dst = 0 - a
                writeln!(self.out, "    vpsubd %zmm{}, %zmm{}, %zmm{}", ra, ZERO, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Abs { dst, a } => {
                let ra = self.use_reg(*a, OPA);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpabsd %zmm{}, %zmm{}", ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Muldq { dst, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let rb = self.use_reg(*b, OPB);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpmuldq %zmm{}, %zmm{}, %zmm{}", rb, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Sraq { dst, a, imm } => {
                let ra = self.use_reg(*a, OPA);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpsraq ${}, %zmm{}, %zmm{}", imm, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Sllq { dst, a, imm } => {
                let ra = self.use_reg(*a, OPA);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpsllq ${}, %zmm{}, %zmm{}", imm, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::ShrD { dst, a, imm, arith } => {
                let ra = self.use_reg(*a, OPA);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    {} ${}, %zmm{}, %zmm{}", if *arith { "vpsrad" } else { "vpsrld" }, imm, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::BlendImm { dst, mask, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let rb = self.use_reg(*b, OPB);
                let d = self.def_reg(*dst);
                // k1 is the scratch mask; set lanes take the second source.
                writeln!(self.out, "    movw ${}, %ax", *mask as i16).unwrap();
                writeln!(self.out, "    kmovw %eax, %k1").unwrap();
                writeln!(self.out, "    vpblendmd %zmm{}, %zmm{}, %zmm{}{{%k1}}", rb, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Cmp { dst, imm, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let bop = self.src_operand(*b, OPB);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpcmpd ${}, {}, %zmm{}, %k1", imm, bop, ra).unwrap();
                writeln!(self.out, "    vpmovm2d %k1, %zmm{}", d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Ternlog { dst, a, b, c, imm } => {
                let ra = self.use_reg(*a, OPA);
                let rb = self.use_reg(*b, OPB);
                let rc = self.use_reg(*c, H0);
                let d = self.def_reg(*dst);
                // dst = LUT(dst, b, c) with dst preloaded from a.
                if d != ra {
                    writeln!(self.out, "    vmovdqa64 %zmm{}, %zmm{}", ra, d).unwrap();
                }
                writeln!(self.out, "    vpternlogd ${}, %zmm{}, %zmm{}, %zmm{}", imm, rc, rb, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Call { op, dst, args, scalars } => {
                // 1. marshal SIMD args to arg buffers (slots 0..).
                for (i, a) in args.iter().enumerate() {
                    let r = self.use_reg(*a, OPA);
                    let off = self.argbuf_off + i as u32 * 64;
                    writeln!(self.out, "    vmovdqu64 %zmm{}, {}(%rsp)", r, off).unwrap();
                }
                let resbuf = self.argbuf_off + 7 * 64;
                // 2. save the registers live across the call and not still
                // saved from the call before (it clobbers every vector
                // register; the scratches hold nothing across an
                // instruction, `ZERO` is re-zeroed below).
                let (saves, restores) = self.call_saves[&k];
                for r in (0..32u32).filter(|r| saves & (1 << r) != 0) {
                    writeln!(self.out, "    vmovdqu64 %zmm{}, {}(%rsp)", r, self.save_off + r * 64)
                        .unwrap();
                }
                let arg = |i: u32| self.argbuf_off + i * 64;
                // 3. C args; `stack_arg`: tile_flag's pushed 7th scalar.
                let (ctx_off, stack_arg): (u32, bool) = match op {
                    CallOp::Div | CallOp::Rem | CallOp::Sin => {
                        writeln!(self.out, "    leaq {}(%rsp), %rdi", resbuf).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rsi", arg(0)).unwrap();
                        if !matches!(op, CallOp::Sin) {
                            writeln!(self.out, "    leaq {}(%rsp), %rdx", arg(1)).unwrap();
                        }
                        let c = match op {
                            CallOp::Div => CTX_DIV,
                            CallOp::Rem => CTX_REM,
                            _ => CTX_SIN,
                        };
                        (c, false)
                    }
                    CallOp::Mget => {
                        writeln!(self.out, "    movq {}(%r15), %rdi", CTX_ENV).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rsi", resbuf).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rdx", arg(0)).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rcx", arg(1)).unwrap();
                        (CTX_MGET, false)
                    }
                    CallOp::TileFlag => {
                        writeln!(self.out, "    movq {}(%r15), %rdi", CTX_ENV).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rsi", resbuf).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rdx", arg(0)).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rcx", arg(1)).unwrap();
                        writeln!(self.out, "    movl ${}, %r8d", scalars[0]).unwrap();
                        writeln!(self.out, "    movl ${}, %r9d", scalars[1]).unwrap();
                        writeln!(self.out, "    subq $16, %rsp").unwrap();
                        writeln!(self.out, "    movl ${}, (%rsp)", scalars[2]).unwrap();
                        (CTX_TILE, true)
                    }
                    CallOp::TileFlagLanes => {
                        writeln!(self.out, "    movq {}(%r15), %rdi", CTX_ENV).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rsi", resbuf).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rdx", arg(0)).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %rcx", arg(1)).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %r8", arg(2)).unwrap();
                        writeln!(self.out, "    leaq {}(%rsp), %r9", arg(3)).unwrap();
                        writeln!(self.out, "    subq $16, %rsp").unwrap();
                        writeln!(self.out, "    movl ${}, (%rsp)", scalars[0]).unwrap();
                        (CTX_TILE_LANES, true)
                    }
                };
                // 4. call through the ctx.
                writeln!(self.out, "    movq {}(%r15), %rax", ctx_off).unwrap();
                writeln!(self.out, "    call *%rax").unwrap();
                if stack_arg {
                    writeln!(self.out, "    addq $16, %rsp").unwrap();
                }
                // 5. restore those read before the next call; `ZERO` again
                // where a `Neg` follows.
                for r in (0..32u32).filter(|r| restores & (1 << r) != 0) {
                    writeln!(self.out, "    vmovdqu64 {}(%rsp), %zmm{}", self.save_off + r * 64, r)
                        .unwrap();
                }
                if self.last_neg.is_some_and(|n| n > k) {
                    writeln!(self.out, "    vpxorq %zmm{Z}, %zmm{Z}, %zmm{Z}", Z = ZERO).unwrap();
                }
                // 6. read the result into dst.
                let d = self.def_reg(*dst);
                if matches!(op, CallOp::TileFlag | CallOp::TileFlagLanes) {
                    writeln!(self.out, "    movzwl {}(%rsp), %eax", resbuf).unwrap();
                    writeln!(self.out, "    kmovw %eax, %k1").unwrap();
                    writeln!(self.out, "    vpmovm2d %k1, %zmm{}", d).unwrap();
                } else {
                    writeln!(self.out, "    vmovdqu64 {}(%rsp), %zmm{}", resbuf, d).unwrap();
                }
                self.store_def(*dst, d);
            }
            Inst::StoreMask { off, src } => {
                let rs = self.use_reg(*src, OPA);
                writeln!(self.out, "    vpmovd2m %zmm{}, %k1", rs).unwrap();
                writeln!(self.out, "    kmovw %k1, %eax").unwrap();
                writeln!(self.out, "    movw %ax, {}(%r14)", off).unwrap();
            }
            Inst::Store { off, src } => {
                let r = match src {
                    Src::Reg(v) => self.use_reg(*v, OPA),
                    Src::BI32(c) => {
                        let l = self.pool.d(*c);
                        writeln!(self.out, "    vpbroadcastd {}(%rip), %zmm{}", l, OPA).unwrap();
                        OPA
                    }
                };
                writeln!(self.out, "    vmovdqu64 %zmm{}, {}(%r14)", r, off).unwrap();
            }
        }
    }
}

/// Drop stack reloads into a register that already holds that slot, and
/// constant materializations (`Emitter::constant`) into a register that
/// already holds that constant. A register holds a slot from its
/// reload/spill, a constant from its materialization, until written (last
/// AT&T operand); a spill stales other holders of its slot; any other stack
/// access, label, jump, call, ret or `vzeroupper` forgets everything.
fn drop_redundant_reloads(body: &str) -> String {
    let reg = |s: &str| -> Option<usize> {
        let s = s.strip_prefix("%zmm").or_else(|| s.strip_prefix("%ymm")).or_else(|| s.strip_prefix("%xmm"))?;
        let n: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();
        n.parse().ok()
    };
    fn slot(s: &str) -> Option<&str> {
        s.strip_suffix("(%rsp)")
    }
    let mut holds: [Option<String>; 32] = Default::default();
    let mut out = String::with_capacity(body.len());
    for line in body.lines() {
        let t = line.trim();
        let (mn, ops) = t.split_once(' ').unwrap_or((t, ""));
        let (a, b) = ops.rsplit_once(", ").unwrap_or(("", ops));
        let forget = |holds: &mut [Option<String>; 32]| *holds = Default::default();
        if mn == "vmovdqu64" && slot(a).is_some() && b.starts_with("%zmm") {
            // A reload: `vmovdqu64 OFF(%rsp), %zmmR`.
            let (s, r) = (slot(a).unwrap().to_string(), reg(b).unwrap_or(usize::MAX));
            if r < 32 && holds[r].as_deref() == Some(s.as_str()) {
                continue;
            }
            if r < 32 {
                holds[r] = Some(s);
            } else {
                forget(&mut holds);
            }
        } else if let Some(k) = constant_key(mn, ops, a).filter(|_| reg(b).is_some_and(|r| r < 32)) {
            // A constant into a register: dropped if it holds it already.
            let r = reg(b).expect("filtered");
            if holds[r].as_deref() == Some(k.as_str()) {
                continue;
            }
            holds[r] = Some(k);
        } else if mn == "vmovdqu64" && a.starts_with("%zmm") && slot(b).is_some() {
            // A spill: `vmovdqu64 %zmmR, OFF(%rsp)`.
            let s = slot(b).unwrap();
            for h in holds.iter_mut() {
                if h.as_deref() == Some(s) {
                    *h = None;
                }
            }
            match reg(a) {
                Some(r) if r < 32 => holds[r] = Some(s.to_string()),
                _ => forget(&mut holds),
            }
        } else if t.contains("(%rsp)") || t.ends_with(':') || mn.starts_with('j') || mn == "call" || mn == "ret" || mn == "vzeroupper" || t.starts_with('.') {
            forget(&mut holds);
        } else if let Some(r) = reg(b) {
            if r < 32 {
                holds[r] = None;
            }
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

/// The constant a line materializes (`Emitter::constant`), as a key no
/// stack slot (a bare offset) can equal: the pool label of a broadcast, or
/// `$zero` for the zeroing idiom.
fn constant_key(mn: &str, ops: &str, a: &str) -> Option<String> {
    match mn {
        "vpbroadcastd" if a.ends_with("(%rip)") => Some(a.to_string()),
        "vpxord" => {
            let mut it = ops.split(", ");
            let first = it.next()?;
            (it.clone().count() == 2 && it.all(|o| o == first)).then(|| "$zero".to_string())
        }
        _ => None,
    }
}

/// Compile `roots` of `g` into AVX-512 assembly, with the buffer layouts.
pub fn compile(
    g: &Graph,
    roots: &[NodeId],
    sym: &str,
    cell_reprs: &HashMap<u32, CellRepr>,
) -> Result<Compiled> {
    let live = reachable(g, roots);

    // Assign input offsets to reachable cells, ascending by cell id.
    let mut cells: Vec<u32> = (0..g.len() as NodeId)
        .filter(|id| live[*id as usize])
        .filter_map(|id| match g.get(id).op {
            Op::Cell(c) => Some(c),
            _ => None,
        })
        .collect();
    cells.sort_unstable();
    cells.dedup();
    let input_reprs: Vec<CellRepr> =
        cells.iter().map(|c| cell_reprs.get(c).copied().unwrap_or(CellRepr::Num)).collect();
    let mut input_offsets: Vec<u32> = Vec::with_capacity(cells.len());
    let mut off = 0u32;
    for r in &input_reprs {
        input_offsets.push(off);
        off += r.size();
    }
    let input_bytes = off;
    let cell_off: HashMap<u32, u32> =
        cells.iter().zip(&input_offsets).map(|(c, o)| (*c, *o)).collect();

    let mut lo = Lower {
        g,
        vals: vec![None; g.len()],
        insts: Vec::new(),
        next_vreg: 0,
        input_cells: cells.clone(),
        cell_off,
        cell_reprs,
        memo: HashMap::new(),
    };
    // A small batch keeps live temporaries well under the 26 homes.
    let batch = 4;
    let t_lower = std::time::Instant::now();
    let order = schedule(g, &live, roots, batch);
    // `CELESTE_ASM_STATS`: which graph op each vreg and instruction came from.
    let stats = std::env::var_os("CELESTE_ASM_STATS").is_some();
    let mut kinds: Vec<String> = Vec::new();
    let mut kind_ix: HashMap<String, u16> = HashMap::new();
    let mut vreg_kind: Vec<u16> = Vec::new();
    let mut insts_by_kind: Vec<usize> = Vec::new();
    let mix = super::mix_on();
    let mut prov_node: Vec<NodeId> = Vec::new();
    for id in order {
        let (v0, i0) = (lo.next_vreg, lo.insts.len());
        lo.lower_node(id)?;
        if mix {
            prov_node.resize(lo.insts.len(), id);
        }
        if stats {
            let name = format!("{:?}", g.get(id).op).split('(').next().unwrap_or("").to_string();
            let k = *kind_ix.entry(name.clone()).or_insert_with(|| {
                kinds.push(name);
                insts_by_kind.push(0);
                (kinds.len() - 1) as u16
            });
            insts_by_kind[k as usize] += lo.insts.len() - i0;
            vreg_kind.resize(lo.next_vreg as usize, u16::MAX);
            for v in v0..lo.next_vreg {
                vreg_kind[v as usize] = k;
            }
        }
    }
    let t_lower = t_lower.elapsed();

    // Roots -> a PACKED output buffer, booleans first (not a line each).
    let kind_of = |v: &Option<Value>| -> Result<RootKind> {
        Ok(match v {
            Some(Value::Num(_)) => RootKind::Num,
            Some(Value::Ival(_)) => RootKind::Ival,
            Some(Value::Bool(_)) => RootKind::Bool,
            None => bail!("a root was never lowered"),
        })
    };
    let kinds_first: Vec<RootKind> = roots.iter().map(|r| kind_of(&lo.vals[*r as usize])).collect::<Result<_>>()?;
    let mut root_offsets: Vec<u32> = vec![0; roots.len()];
    let mut next = 0u32;
    for (ri, k) in kinds_first.iter().enumerate() {
        if *k == RootKind::Bool {
            root_offsets[ri] = next;
            next += 4;
        }
    }
    next = next.div_ceil(64) * 64;
    for (ri, k) in kinds_first.iter().enumerate() {
        match k {
            RootKind::Num => {
                root_offsets[ri] = next;
                next += 64;
            }
            RootKind::Ival => {
                root_offsets[ri] = next;
                next += 128;
            }
            RootKind::Bool => {}
        }
    }
    let out_bytes = next.max(64);
    let mut root_kinds: Vec<RootKind> = Vec::with_capacity(roots.len());
    for (ri, r) in roots.iter().enumerate() {
        let base = root_offsets[ri];
        let kind = match lo.vals[*r as usize] {
            Some(Value::Num(n)) => {
                let src = lo.num_src(n);
                lo.insts.push(Inst::Store { off: base, src });
                RootKind::Num
            }
            Some(Value::Ival(iv)) => {
                for (half, nv) in iv.iter().enumerate() {
                    let src = lo.num_src(*nv);
                    lo.insts.push(Inst::Store { off: base + half as u32 * 64, src });
                }
                RootKind::Ival
            }
            Some(Value::Bool(b)) => {
                let val = lo.mask_reg(b[0]);
                let known = lo.mask_reg(b[1]);
                lo.insts.push(Inst::StoreMask { off: base, src: val });
                lo.insts.push(Inst::StoreMask { off: base + 2, src: known });
                RootKind::Bool
            }
            None => bail!("root {} was never lowered", r),
        };
        root_kinds.push(kind);
        if mix {
            prov_node.resize(lo.insts.len(), *r);
        }
    }

    let t = std::time::Instant::now();
    let (insts, order) = reschedule(std::mem::take(&mut lo.insts), lo.next_vreg);
    lo.insts = insts;
    let prov: Vec<(NodeId, &'static str)> = if mix {
        order.iter().zip(&lo.insts).map(|(&i, inst)| (prov_node[i as usize], inst_kind(inst))).collect()
    } else {
        Vec::new()
    };
    let foldable = if mix { foldable(&lo.insts, lo.next_vreg) } else { Vec::new() };
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build]   {sym}: {} insts, {} vregs: lower {:.1}s, reschedule {:.1}s", lo.insts.len(), lo.next_vreg, t_lower.as_secs_f64(), t.elapsed().as_secs_f64());
    }

    // Rematerializable vregs (spilled for free) and each vreg's def.
    let mut remat = vec![false; lo.next_vreg as usize];
    let mut def_of = vec![0u32; lo.next_vreg as usize];
    for (i, inst) in lo.insts.iter().enumerate() {
        if let Some(d) = inst_def(inst) {
            def_of[d as usize] = i as u32;
            // A load (reads `%r13`) and a constant are rematerializable;
            // nothing else is.
            remat[d as usize] = matches!(inst, Inst::Load { .. } | Inst::BcastD { .. });
        }
    }

    let t_alloc = std::time::Instant::now();
    let (home, spill_slots) = allocate(&lo.insts, lo.next_vreg, &remat);
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build]   {sym}: allocate {:.1}s ({spill_slots} spill slots)", t_alloc.elapsed().as_secs_f64());
    }
    if stats {
        // Per graph op: instructions, spilled values, reloads.
        vreg_kind.resize(lo.next_vreg as usize, u16::MAX);
        let (mut spilled, mut reloads) = (vec![0usize; kinds.len()], vec![0usize; kinds.len()]);
        for v in 0..lo.next_vreg as usize {
            if matches!(home[v], Loc::Spill(_)) && !remat[v] && vreg_kind[v] != u16::MAX {
                spilled[vreg_kind[v] as usize] += 1;
            }
        }
        let mut buf = Vec::new();
        for inst in &lo.insts {
            buf.clear();
            inst_uses(inst, &mut buf);
            for &v in &buf {
                if matches!(home[v as usize], Loc::Spill(_)) && !remat[v as usize] && vreg_kind[v as usize] != u16::MAX {
                    reloads[vreg_kind[v as usize] as usize] += 1;
                }
            }
        }
        let mut rows: Vec<usize> = (0..kinds.len()).collect();
        rows.sort_by_key(|&k| std::cmp::Reverse(reloads[k]));
        let line: Vec<String> = rows
            .iter()
            .take(14)
            .map(|&k| format!("{} {}i/{}s/{}r", kinds[k], insts_by_kind[k], spilled[k], reloads[k]))
            .collect();
        eprintln!("[asm stats] {sym} by op (instructions/spilled values/reloads): {}", line.join(", "));
    }

    let has_calls = lo.insts.iter().any(|i| matches!(i, Inst::Call { .. }));
    let spill_bytes = spill_slots as u32 * 64;
    let save_off = spill_bytes;
    let argbuf_off = spill_bytes + SAVE_BYTES;

    let call_saves = call_saves(&lo.insts, lo.next_vreg, &home);
    let last_neg = lo.insts.iter().rposition(|i| matches!(i, Inst::Neg { .. }));
    let mut em = Emitter {
        home: &home,
        insts: &lo.insts,
        remat: &remat,
        def_of: &def_of,
        save_off,
        call_saves: &call_saves,
        last_neg,
        argbuf_off,
        pool: Pool::default(),
        out: String::new(),
    };
    // Body first (fills the pool), then wrap with prologue/epilogue.
    let mut body = String::new();
    std::mem::swap(&mut em.out, &mut body);
    for (k, inst) in lo.insts.iter().enumerate() {
        if mix {
            // A comment line: assembles to nothing, and `drop_redundant_reloads` passes it.
            writeln!(em.out, "#@{k}").unwrap();
        }
        em.emit_inst(k, inst);
    }
    std::mem::swap(&mut em.out, &mut body); // em.out empty again, body holds the code
    let body = drop_redundant_reloads(&body);

    // Frame: spill slots, then (with call-outs) the zmm save area and arg buffers.
    let extra = if has_calls { SAVE_BYTES + ARGBUF_BYTES } else { 0 };
    let frame = spill_bytes + extra;
    let mut asm = String::new();
    writeln!(asm, ".text").unwrap();
    writeln!(asm, ".globl {sym}").unwrap();
    writeln!(asm, "{sym}:").unwrap();
    // Three pushes 16-align rsp (entry is 8 mod 16); the frame is a 64-multiple.
    writeln!(asm, "    push %r13").unwrap();
    writeln!(asm, "    push %r14").unwrap();
    writeln!(asm, "    push %r15").unwrap();
    writeln!(asm, "    movq %rdi, %r13").unwrap();
    writeln!(asm, "    movq %rsi, %r14").unwrap();
    writeln!(asm, "    movq %rdx, %r15").unwrap();
    if frame > 0 {
        writeln!(asm, "    sub ${}, %rsp", frame).unwrap();
    }
    writeln!(asm, "    vpxorq %zmm{Z}, %zmm{Z}, %zmm{Z}", Z = ZERO).unwrap();
    asm.push_str(&body);
    if mix {
        writeln!(asm, "#@end").unwrap();
    }
    if frame > 0 {
        writeln!(asm, "    add ${}, %rsp", frame).unwrap();
    }
    writeln!(asm, "    pop %r15").unwrap();
    writeln!(asm, "    pop %r14").unwrap();
    writeln!(asm, "    pop %r13").unwrap();
    writeln!(asm, "    vzeroupper").unwrap();
    writeln!(asm, "    ret").unwrap();

    // Constant pool.
    writeln!(asm, ".section .rodata").unwrap();
    writeln!(asm, ".align 64").unwrap();
    for (i, v) in em.pool.d.iter().enumerate() {
        writeln!(asm, ".LCd{i}: .long {}", *v as u32).unwrap();
    }

    Ok(Compiled {
        asm,
        input_cells: lo.input_cells,
        input_offsets,
        input_reprs,
        input_bytes,
        n_roots: roots.len(),
        root_offsets,
        out_bytes,
        root_kinds,
        sym: sym.to_string(),
        spill_slots,
        frame_bytes: frame,
        prov,
        foldable,
    })
}

/// A vreg's value as far as bit identities know it.
#[derive(Clone, Copy, PartialEq)]
enum Known {
    Opaque,
    /// The same i32 in every lane.
    Const(i32),
    /// Equal to another (opaque) vreg.
    Alias(Vreg),
}

/// The instructions plain bit identities REMOVE (`CELESTE_KERNEL_MIX`'s
/// estimate; nothing in the build uses it): constants folded through
/// `x & -1 = x`, `x | -1 = -1`, `x | 0 = x`, `x & 0 = 0`, `c ? t : t = t`, a
/// select or operation on constants, and so on; then every instruction no
/// store needs (through the resolved operands) is removable. A constant a
/// kept instruction still reads keeps its broadcast. Exact per lane: an
/// identity holds for every input, so this drops no check.
fn foldable(insts: &[Inst], n_vregs: Vreg) -> Vec<bool> {
    let mut kn = vec![Known::Opaque; n_vregs as usize];
    let res = |kn: &[Known], v: Vreg| -> Known {
        match kn[v as usize] {
            Known::Opaque => Known::Alias(v),
            k => k,
        }
    };
    let src = |kn: &[Known], s: &Src| -> Known {
        match s {
            Src::Reg(v) => res(kn, *v),
            Src::BI32(c) => Known::Const(*c),
        }
    };
    for inst in insts {
        let (dst, k) = match inst {
            Inst::BcastD { dst, val } => (*dst, Known::Const(*val)),
            Inst::RBin { dst, op, a, b } => {
                let (x, y) = (res(&kn, *a), src(&kn, b));
                let k = match (op, x, y) {
                    (_, Known::Const(p), Known::Const(q)) => Known::Const(match op {
                        ROp::AddD => p.wrapping_add(q),
                        ROp::SubD => p.wrapping_sub(q),
                        ROp::MinSD => p.min(q),
                        ROp::MaxSD => p.max(q),
                        ROp::AndD => p & q,
                        ROp::OrD => p | q,
                        ROp::XorD => p ^ q,
                        ROp::AndnD => !p & q,
                    }),
                    (ROp::AndD, z, Known::Const(-1)) | (ROp::AndD, Known::Const(-1), z) => z,
                    (ROp::AndD, _, Known::Const(0)) | (ROp::AndD, Known::Const(0), _) => Known::Const(0),
                    (ROp::OrD, _, Known::Const(-1)) | (ROp::OrD, Known::Const(-1), _) => Known::Const(-1),
                    (ROp::OrD, z, Known::Const(0)) | (ROp::OrD, Known::Const(0), z) => z,
                    (ROp::AndD | ROp::OrD | ROp::MinSD | ROp::MaxSD, p, q) if p == q => p,
                    (ROp::XorD, z, Known::Const(0)) | (ROp::XorD, Known::Const(0), z) => z,
                    (ROp::XorD | ROp::SubD, p, q) if p == q => Known::Const(0),
                    (ROp::AndnD, Known::Const(-1), _) | (ROp::AndnD, _, Known::Const(0)) => Known::Const(0),
                    (ROp::AndnD, Known::Const(0), z) => z,
                    (ROp::AndnD, p, q) if p == q => Known::Const(0),
                    (ROp::AddD | ROp::SubD, z, Known::Const(0)) => z,
                    (ROp::AddD, Known::Const(0), z) => z,
                    _ => Known::Opaque,
                };
                (*dst, k)
            }
            Inst::Cmp { dst, imm, a, b } => {
                let k = match (res(&kn, *a), src(&kn, b)) {
                    (Known::Const(p), Known::Const(q)) => {
                        let t = match imm {
                            0 => p == q,
                            1 => p < q,
                            2 => p <= q,
                            4 => p != q,
                            5 => p >= q,
                            6 => p > q,
                            _ => return vec![false; insts.len()],
                        };
                        Known::Const(if t { -1 } else { 0 })
                    }
                    (p, q) if p == q => match imm {
                        0 | 2 | 5 => Known::Const(-1),
                        _ => Known::Const(0),
                    },
                    _ => Known::Opaque,
                };
                (*dst, k)
            }
            Inst::Ternlog { dst, a, b, c, imm } => {
                let (x, y, z) = (res(&kn, *a), res(&kn, *b), res(&kn, *c));
                let k = match (x, y, z) {
                    (Known::Const(p), Known::Const(q), Known::Const(r)) => {
                        let mut out = 0i32;
                        for bit in 0..32 {
                            let i = ((p >> bit) & 1) << 2 | ((q >> bit) & 1) << 1 | ((r >> bit) & 1);
                            out |= ((*imm as i32 >> i) & 1) << bit;
                        }
                        Known::Const(out)
                    }
                    _ if *imm == 0xca && y == z => y,
                    (Known::Const(-1), _, _) if *imm == 0xca => y,
                    (Known::Const(0), _, _) if *imm == 0xca => z,
                    _ => Known::Opaque,
                };
                (*dst, k)
            }
            other => match inst_def(other) {
                Some(d) => (d, Known::Opaque),
                None => continue,
            },
        };
        // An alias of itself is just opaque.
        kn[dst as usize] = if k == Known::Alias(dst) { Known::Opaque } else { k };
    }
    // Liveness from the stores through resolved operands.
    let mut def_at = vec![u32::MAX; n_vregs as usize];
    for (i, inst) in insts.iter().enumerate() {
        if let Some(d) = inst_def(inst) {
            def_at[d as usize] = i as u32;
        }
    }
    let mut const_def: HashMap<i32, u32> = HashMap::new();
    for (i, inst) in insts.iter().enumerate() {
        if let Inst::BcastD { val, .. } = inst {
            const_def.entry(*val).or_insert(i as u32);
        }
    }
    let mut needed = vec![false; insts.len()];
    let mut stack: Vec<u32> = Vec::new();
    let mut buf = Vec::new();
    let need_vreg = |v: Vreg, stack: &mut Vec<u32>, needed: &mut Vec<bool>| {
        let at = match res(&kn, v) {
            Known::Alias(w) => def_at[w as usize],
            // A constant operand needs its broadcast (or an embedded one).
            Known::Const(c) => const_def.get(&c).copied().unwrap_or(u32::MAX),
            Known::Opaque => def_at[v as usize],
        };
        if at != u32::MAX && !needed[at as usize] {
            needed[at as usize] = true;
            stack.push(at);
        }
    };
    for (i, inst) in insts.iter().enumerate() {
        if matches!(inst, Inst::Store { .. } | Inst::StoreMask { .. } | Inst::Call { .. }) {
            needed[i] = true;
            stack.push(i as u32);
        }
    }
    while let Some(i) = stack.pop() {
        buf.clear();
        inst_uses(&insts[i as usize], &mut buf);
        for &v in &buf {
            need_vreg(v, &mut stack, &mut needed);
        }
    }
    needed.iter().map(|n| !n).collect()
}

/// An instruction's kind for `Compiled::prov`: the variant, a binary op's
/// operation, and whether its operand is the floor mask or the NOT constant.
fn inst_kind(inst: &Inst) -> &'static str {
    match inst {
        Inst::Load { .. } => "load",
        Inst::LoadMask { .. } => "loadmask",
        Inst::BcastD { .. } => "bcast",
        Inst::RBin { op, b, .. } => match (op, b) {
            (ROp::AndD, Src::BI32(FLR_MASK)) => "flr",
            (ROp::XorD, Src::BI32(-1)) => "not",
            (ROp::AddD, _) => "add",
            (ROp::SubD, _) => "sub",
            (ROp::MinSD, _) => "min",
            (ROp::MaxSD, _) => "max",
            (ROp::AndD, _) => "and",
            (ROp::OrD, _) => "or",
            (ROp::XorD, _) => "xor",
            (ROp::AndnD, _) => "andn",
        },
        Inst::Neg { .. } => "neg",
        Inst::Abs { .. } => "abs",
        Inst::Muldq { .. } => "muldq",
        Inst::Sraq { .. } => "sraq",
        Inst::Sllq { .. } => "sllq",
        Inst::ShrD { .. } => "shrd",
        Inst::BlendImm { .. } => "blendimm",
        Inst::Cmp { .. } => "cmp",
        Inst::Ternlog { imm: 0xca, .. } => "sel",
        Inst::Ternlog { .. } => "ternlog",
        Inst::Call { .. } => "call",
        Inst::Store { .. } => "store",
        Inst::StoreMask { .. } => "storemask",
    }
}
