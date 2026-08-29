//! `transpile::graph::Graph` -> native AVX-512 GAS assembly.
//!
//! The row-key hashing slice: numeric arithmetic (`Add/Sub/Min/Max/Neg/
//! Abs/Flr`) over cells and constants, `Bits` (zw_bits_n), and the `Mix`
//! fold (`zw_mix1`/`zw_mix2`), stored as packed output columns. Every
//! instruction is chosen to match `celeste_engine::kernel` bit-for-bit;
//! `super::tests` proves it. See `plans/asm-backend.md`.

use std::collections::HashMap;
use std::fmt::Write;

use anyhow::{bail, Result};

use crate::transpile::graph::{Graph, NodeId, Op};

// ---- physical register-file partition ----
//
// `mix64` and the `Mix` fold are lowered to PRIMITIVE vreg instructions
// (shift/xor/mul/add), each temporary its own allocator-managed vreg, so
// there is no fixed scratch serializing the independent mix chains. Six
// registers are reserved (ZERO for Neg, two reload-scratch, two remat
// helpers, one spilled-result scratch), leaving 26 homes.
const ZERO: u8 = 31; // constant 0, for Neg
const OPA: u8 = 30; // reload/remat scratch, operand A
const OPB: u8 = 29; // reload/remat scratch, operand B
const H0: u8 = 28; // remat helper (recompute a sub-operand)
const H1: u8 = 27; // remat helper (BitsHi extract temp)
const RES: u8 = 26; // scratch for a spilled result
const N_ALLOC: u8 = 26; // homes zmm0..=zmm25

// Call-out ABI. Loads/stores use callee-saved r13 (inputs) / r14 (outputs);
// r15 holds the AsmCtx pointer - so a mid-DAG `call` may clobber rdi/rsi.
// `AsmCtx` field byte offsets (see callout.rs; keep in lockstep).
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

const C1: u64 = 0xbf58_476d_1ce4_e5b9; // mix64 multiplier 1
const C2: u64 = 0x94d0_49bb_1331_11eb; // mix64 multiplier 2
const FLR_MASK: i32 = 0xffff_0000u32 as i32;
const ONE_FIXED: i32 = 0x0001_0000; // P8::from_i16(1) raw

type Vreg = u32;

/// A numeric (ZN) node value: a live register or a broadcast constant.
#[derive(Clone, Copy)]
enum NumVal {
    Reg(Vreg),
    ConstI32(i32),
}

/// One half of a word (ZW) node value.
#[derive(Clone, Copy)]
enum WordHalf {
    Reg(Vreg),
    ConstU64(u64),
}

/// One plane of a tri-state boolean (`ZB`): a per-lane VECTOR mask (each
/// lane all-ones or zero), not a k-register - so the whole boolean layer
/// stays in the plentiful zmm class and reuses the allocator/scheduler.
/// `Const(true)` = all-ones, `Const(false)` = zero.
#[derive(Clone, Copy)]
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
    Word([WordHalf; 2]),
}

/// A source operand for an emitted instruction.
#[derive(Clone, Copy)]
enum Src {
    Reg(Vreg),
    BI32(i32),
    BU64(u64),
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

/// 64-bit lane binary ops (the row-key word layer).
#[derive(Clone, Copy)]
enum QOp {
    AddQ,
    XorQ,
    MullQ,
}

/// A source operand's value identity, for GVN.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum SrcKey {
    Reg(Vreg),
    I32(i32),
    U64(u64),
}
impl Src {
    fn key(self) -> SrcKey {
        match self {
            Src::Reg(v) => SrcKey::Reg(v),
            Src::BI32(c) => SrcKey::I32(c),
            Src::BU64(c) => SrcKey::U64(c),
        }
    }
}

/// A pure instruction's value identity: its op and operand identities,
/// excluding the destination vreg. Two insts with equal keys compute the
/// same value, so the second reuses the first's vreg (global value
/// numbering). This is what dedups the inner `mix64(bit^c)` shared by every
/// row - the CSE that closes most of the gap to LLVM. Commutative ops sort
/// their register operands so `a^b` and `b^a` share.
#[derive(Clone, PartialEq, Eq, Hash)]
enum Key {
    Load(u32),
    LoadMask(u32),
    BcastD(i32),
    RBin(u8, Vreg, SrcKey),
    Neg(Vreg),
    Abs(Vreg),
    BitsLo(Vreg),
    BitsHi(Vreg),
    Srlq(Vreg, u8),
    QBin(u8, SrcKey, SrcKey),
    Muldq(Vreg, Vreg),
    Sraq(Vreg, u8),
    Sllq(Vreg, u8),
    BlendImm(u16, Vreg, Vreg),
    Cmp(u8, Vreg, SrcKey),
    Ternlog(Vreg, Vreg, Vreg, u8),
}

/// A total order on operand keys so commutative ops canonicalize. Registers
/// sort before constants, which keeps a register as the first operand.
fn key_ord(k: SrcKey) -> (u8, i64) {
    match k {
        SrcKey::Reg(v) => (0, v as i64),
        SrcKey::I32(c) => (1, c as i64),
        SrcKey::U64(c) => (2, c as i64),
    }
}

/// The SSA instruction stream, over vregs. Each writes exactly one vreg
/// (`dst`), which is what makes the allocator a single-register problem.
enum Inst {
    Load { dst: Vreg, off: u32 },
    /// Load a 16-bit `val` mask from the input buffer at `off` and expand it
    /// to a per-lane vector mask (`movzwl`; `kmovw`; `vpmovm2d`). The bool
    /// input path - the reverse of `StoreMask`; `known` is supplied as
    /// `Const(true)` by the lowerer.
    LoadMask { dst: Vreg, off: u32 },
    BcastD { dst: Vreg, val: i32 },
    RBin { dst: Vreg, op: ROp, a: Vreg, b: Src },
    Neg { dst: Vreg, a: Vreg },
    Abs { dst: Vreg, a: Vreg },
    BitsLo { dst: Vreg, src: Vreg },
    BitsHi { dst: Vreg, src: Vreg },
    /// 64-bit right shift by a compile-time immediate (`vpsrlq`).
    Srlq { dst: Vreg, a: Vreg, imm: u8 },
    /// 64-bit lane binary op; `b` may be a broadcast constant.
    QBin { dst: Vreg, op: QOp, a: Vreg, b: Src },
    /// Signed 32x32 -> 64 multiply of the EVEN lanes (`vpmuldq`).
    Muldq { dst: Vreg, a: Vreg, b: Vreg },
    /// Arithmetic 64-bit right shift by an immediate (`vpsraq`).
    Sraq { dst: Vreg, a: Vreg, imm: u8 },
    /// Logical 64-bit left shift by an immediate (`vpsllq`).
    Sllq { dst: Vreg, a: Vreg, imm: u8 },
    /// Blend `a`/`b` per 32-bit lane by a COMPILE-TIME mask: lane takes `b`
    /// where the mask bit is set, else `a` (`kmov` imm -> k1; `vpblendmd`).
    BlendImm { dst: Vreg, mask: u16, a: Vreg, b: Vreg },
    /// Signed 32-bit compare -> per-lane vector mask: `vpcmpd $imm` to k1,
    /// then `vpmovm2d` to `dst`. `imm` is the `vpcmpd` predicate.
    Cmp { dst: Vreg, imm: u8, a: Vreg, b: Src },
    /// Three-input bitwise LUT (`vpternlogd`): `dst = LUT_imm(a, b, c)`.
    Ternlog { dst: Vreg, a: Vreg, b: Vreg, c: Vreg, imm: u8 },
    /// A call-out. `dst` receives the result: a `ZN` for div/rem/sin/mget, a
    /// vector MASK for tile_flag (expanded from the returned u16).
    Call { op: CallOp, dst: Vreg, args: Vec<Vreg>, scalars: Vec<i32> },
    Store { off: u32, src: Src },
    /// Store a vector mask as a 16-bit lane mask: `vpmovd2m` to k1, `kmovw`
    /// to eax, `movw` to memory. Used for `ZB` roots.
    StoreMask { off: u32, src: Vreg },
}

/// The type of a root value, so a caller knows how to read its 128-byte
/// output slot: `Num` = one ZN at +0; `Word`/`Ival` = two 64-byte planes at
/// +0/+64; `Bool` = two u16 masks (val at +0, known at +2).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RootKind {
    Num,
    Bool,
    Ival,
    Word,
}

/// How an `Op::Cell` INPUT column is packed in the input buffer, so the
/// codegen loads it into the right value domain. Every input cell still
/// occupies one 64-byte slot (`input_cells[i]` at `i*64`); the repr only
/// changes how those bytes are interpreted:
/// * `Num` - one `ZN` (64 bytes): 16 x i32 raw `Pico8Num`.
/// * `Bool` - a 16-bit `val` mask in the first 2 bytes of a 64-byte slot;
///   `known` is implicitly all-ones (block bool inputs are fully known,
///   e.g. `has_dashed`).
/// * `Ival` - a `ZI` (128 bytes): the `lo` plane (`ZN`) at +0, the `hi`
///   plane at +64. A per-lane interval, e.g. `player.rem`.
///
/// A cell absent from the repr map defaults to `Num`. Input cells are laid
/// out in `input_cells` order, each at `Compiled::input_offsets[i]`, sized
/// by repr (64 or 128 bytes).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CellRepr {
    Num,
    Bool,
    Ival,
}

impl CellRepr {
    /// Bytes this input cell occupies in the input buffer.
    fn size(self) -> u32 {
        match self {
            CellRepr::Num | CellRepr::Bool => 64,
            CellRepr::Ival => 128,
        }
    }
}

/// The result of lowering: the assembly text plus the two layouts a caller
/// needs to pack its buffers.
pub struct Compiled {
    pub asm: String,
    /// Input cells, in ascending order. Cell `input_cells[i]` occupies the
    /// input buffer at byte `input_offsets[i]`, sized by `input_reprs[i]`
    /// (`Num`/`Bool` 64 bytes, `Ival` 128).
    pub input_cells: Vec<u32>,
    /// Byte offset of each input cell (parallel to `input_cells`).
    pub input_offsets: Vec<u32>,
    /// Repr of each input cell (parallel to `input_cells`).
    pub input_reprs: Vec<CellRepr>,
    /// Total bytes the input buffer must be.
    pub input_bytes: u32,
    /// Number of roots. Root `i` occupies a 128-byte slot at `i*128`.
    pub n_roots: usize,
    /// Each root's type (how to read its slot).
    pub root_kinds: Vec<RootKind>,
    pub sym: String,
    /// How many spill slots the allocator used (0 = everything fit).
    pub spill_slots: usize,
}

// ---- constant pool ----

#[derive(Default)]
struct Pool {
    q: Vec<u64>,
    d: Vec<i32>,
}
impl Pool {
    fn q(&mut self, v: u64) -> String {
        let i = self.q.iter().position(|x| *x == v).unwrap_or_else(|| {
            self.q.push(v);
            self.q.len() - 1
        });
        format!(".LCq{i}")
    }
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
    /// Global value numbering: a pure op's `Key` -> the vreg that already
    /// holds its result. This is the CSE that shares the inner `mix64`
    /// across rows.
    memo: HashMap<Key, Vreg>,
}

impl<'a> Lower<'a> {
    fn fresh(&mut self) -> Vreg {
        let v = self.next_vreg;
        self.next_vreg += 1;
        v
    }

    /// Emit a pure instruction, reusing an existing vreg if one already
    /// computes the same value (`key`). `mk` builds the `Inst` from the
    /// freshly allocated destination.
    fn pure(&mut self, key: Key, mk: impl FnOnce(Vreg) -> Inst) -> Vreg {
        if let Some(v) = self.memo.get(&key) {
            return *v;
        }
        let d = self.fresh();
        self.insts.push(mk(d));
        self.memo.insert(key, d);
        d
    }

    /// A commutative 64-bit binary op with GVN-canonical operand order.
    fn qbin_comm(&mut self, op: QOp, tag: u8, x: Src, y: Src) -> Vreg {
        // Sort the two operand keys so `a op b` and `b op a` share.
        let (mut ka, mut kb) = (x.key(), y.key());
        let (mut a, mut b) = (x, y);
        if key_ord(kb) < key_ord(ka) {
            std::mem::swap(&mut a, &mut b);
            std::mem::swap(&mut ka, &mut kb);
        }
        // The destination form needs `a` to be a register (broadcast is only
        // valid on the memory operand `b`). If after sorting `a` is a
        // constant, both are constants (impossible here) or swap back.
        let (areg, bsrc) = match a {
            Src::Reg(v) => (v, b),
            _ => match b {
                Src::Reg(v) => (v, a),
                _ => unreachable!("qbin over two constants"),
            },
        };
        self.pure(Key::QBin(tag, ka, kb), move |d| Inst::QBin { dst: d, op, a: areg, b: bsrc })
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
    fn word_src(&mut self, wh: WordHalf) -> Src {
        match wh {
            WordHalf::Reg(v) => Src::Reg(v),
            WordHalf::ConstU64(c) => Src::BU64(c),
        }
    }

    /// Emit `mix64_v(x)` as a chain of per-value vreg instructions and
    /// return the result vreg. Every temporary is a fresh vreg, so the
    /// allocator (not a fixed scratch reg) places them and independent
    /// mix chains do not alias.
    fn mix64(&mut self, x: Vreg) -> Vreg {
        let t1 = self.srlq(x, 30);
        let t2 = self.qbin(QOp::XorQ, x, Src::Reg(t1));
        let t3 = self.qbin(QOp::MullQ, t2, Src::BU64(C1));
        let t4 = self.srlq(t3, 27);
        let t5 = self.qbin(QOp::XorQ, t3, Src::Reg(t4));
        let t6 = self.qbin(QOp::MullQ, t5, Src::BU64(C2));
        let t7 = self.srlq(t6, 31);
        self.qbin(QOp::XorQ, t6, Src::Reg(t7))
    }

    fn srlq(&mut self, a: Vreg, imm: u8) -> Vreg {
        self.pure(Key::Srlq(a, imm), move |d| Inst::Srlq { dst: d, a, imm })
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
    fn blend_imm(&mut self, mask: u16, a: Vreg, b: Vreg) -> Vreg {
        self.pure(Key::BlendImm(mask, a, b), move |d| Inst::BlendImm { dst: d, mask, a, b })
    }

    /// 16.16 fixed-point multiply, the even/odd `vpmuldq` weave from
    /// `zn_mul`: even lanes are `(a*b)>>16`, odd lanes the same after an
    /// arithmetic 32-bit right shift brings them into the low i32, then the
    /// odd results are shifted back up and blended over the even ones.
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

    /// Materialize a mask plane into a register: a live vreg, or a broadcast
    /// of the all-ones / all-zero constant for a compile-time mask.
    fn mask_reg(&mut self, m: MaskVal) -> Vreg {
        match m {
            MaskVal::Reg(v) => v,
            MaskVal::Const(b) => {
                let c = if b { -1i32 } else { 0i32 };
                self.pure(Key::BcastD(c), move |d| Inst::BcastD { dst: d, val: c })
            }
        }
    }

    /// A 32-bit-lane binary op (`vpandd/vpord/vpxord/vpandnd/...`), memoized.
    /// `AndnD` is `(~a) & b`, so it is NOT commutative; the others among the
    /// mask ops (`And/Or/Xor`) are and get canonical operand order.
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

    fn zi_add(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        [self.dbin(ROp::AddD, a[0], b[0]), self.dbin(ROp::AddD, a[1], b[1])]
    }
    fn zi_sub(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        // Endpoints cross: [a.lo - b.hi, a.hi - b.lo].
        [self.dbin(ROp::SubD, a[0], b[1]), self.dbin(ROp::SubD, a[1], b[0])]
    }
    fn zi_min(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        [self.dbin(ROp::MinSD, a[0], b[0]), self.dbin(ROp::MinSD, a[1], b[1])]
    }
    fn zi_max(&mut self, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        [self.dbin(ROp::MaxSD, a[0], b[0]), self.dbin(ROp::MaxSD, a[1], b[1])]
    }
    fn zi_neg(&mut self, a: [Vreg; 2]) -> [Vreg; 2] {
        let lo = self.neg(a[1]);
        let hi = self.neg(a[0]);
        [lo, hi]
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
    /// `zi_span_ok`: does it span at most two floors?
    fn zi_span_ok(&mut self, a: [Vreg; 2]) -> Vreg {
        let fl = self.flr(a[0]);
        let fh = self.flr(a[1]);
        let e0 = self.mask_eq(fl, fh);
        let one = self.dbin_c(ROp::AddD, fl, ONE_FIXED);
        let e1 = self.mask_eq(fh, one);
        self.dbin(ROp::OrD, e0, e1)
    }
    /// `zi_fork_flr(a, c)`: (fragment interval, valid mask).
    fn zi_fork_flr(&mut self, a: [Vreg; 2], c: u8) -> ([Vreg; 2], Vreg) {
        let fl = self.flr(a[0]);
        let fh = self.flr(a[1]);
        let one = self.dbin_c(ROp::AddD, fl, ONE_FIXED);
        let two = self.mask_eq(fh, one);
        let all = self.num_reg(NumVal::ConstI32(-1));
        if c == 0 {
            // hi = two ? (fh - 1 raw) : a.hi ; valid = ALL
            let below = self.dbin_c(ROp::AddD, fh, -1);
            let hi = self.vsel(two, below, a[1]);
            ([a[0], hi], all)
        } else {
            // lo = two ? fh : a.lo ; valid = two
            let lo = self.vsel(two, fh, a[0]);
            ([lo, a[1]], two)
        }
    }
    /// `zi_cmp`: tri-state (val, known) for an ORDERED interval comparison.
    /// `kind`: 0 Lt, 1 Le, 2 Gt, 3 Ge.
    fn zi_cmp(&mut self, kind: u8, a: [Vreg; 2], b: [Vreg; 2]) -> [Vreg; 2] {
        // (t, f) as in kernel `zi_cmp`. imm: LT 1, LE 2, GT 6, GE 5.
        let (t, f) = match kind {
            0 => (self.cmp(1, a[1], Src::Reg(b[0])), self.cmp(5, a[0], Src::Reg(b[1]))),
            1 => (self.cmp(2, a[1], Src::Reg(b[0])), self.cmp(6, a[0], Src::Reg(b[1]))),
            2 => (self.cmp(6, a[0], Src::Reg(b[1])), self.cmp(2, a[1], Src::Reg(b[0]))),
            _ => (self.cmp(5, a[0], Src::Reg(b[1])), self.cmp(1, a[1], Src::Reg(b[0]))),
        };
        let known = self.dbin(ROp::OrD, t, f);
        [t, known]
    }

    /// `zw_bits_n`: zero-extend an i32 column to the two u64 ZW halves.
    fn bits_n(&mut self, v: Vreg) -> [Vreg; 2] {
        let lo = self.pure(Key::BitsLo(v), move |d| Inst::BitsLo { dst: d, src: v });
        let hi = self.pure(Key::BitsHi(v), move |d| Inst::BitsHi { dst: d, src: v });
        [lo, hi]
    }
    /// One `zw_bits_i` ZW half: `(lo_half << 32) | hi_half`.
    fn pack_i(&mut self, lo_half: Vreg, hi_half: Vreg) -> Vreg {
        let shifted = self.sllq(lo_half, 32);
        self.dbin(ROp::OrD, shifted, hi_half)
    }
    /// `zw_bits_b`: `(val&1) | ((known&1)<<1)` per lane, as the two ZW halves.
    fn bits_b(&mut self, val: Vreg, known: Vreg) -> (Vreg, Vreg) {
        let vbit = self.dbin_c(ROp::AndD, val, 1);
        let kbit = self.dbin_c(ROp::AndD, known, 1);
        let kshift = self.dbin(ROp::AddD, kbit, kbit); // kbit << 1
        let comb = self.dbin(ROp::OrD, vbit, kshift);
        let h = self.bits_n(comb);
        (h[0], h[1])
    }

    /// `a op b` on words (all three word ops are commutative), memoized and
    /// operand-order-canonical so equal values share a vreg.
    fn qbin(&mut self, op: QOp, a: Vreg, b: Src) -> Vreg {
        let tag = match op {
            QOp::AddQ => 0,
            QOp::XorQ => 1,
            QOp::MullQ => 2,
        };
        self.qbin_comm(op, tag, Src::Reg(a), b)
    }

    /// The raw value of `id` if it is a positive constant scalar (a
    /// degenerate `Op::Const(v, v)` with `v > 0`) - the only scalar an
    /// interval scale/divide is monotone by. Used to gate the interval
    /// `Mul`/`Div` paths, matching `graph.eval`'s exact-positive-scalar
    /// requirement.
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
                "node {} (op {:?}, domain {}) is not a numeric value where one was needed",
                id,
                self.g.get(id).op,
                self.dom(id)
            ),
        }
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

    /// The value domain of a node, for domain-dispatched ops (`Eq`, `Sel`,
    /// `Known`, `Bits`).
    fn dom(&self, id: NodeId) -> u8 {
        match self.vals[id as usize] {
            Some(Value::Num(_)) => 0,
            Some(Value::Bool(_)) => 1,
            Some(Value::Ival(_)) => 2,
            Some(Value::Word(_)) => 3,
            None => 0,
        }
    }
    fn as_word(&self, id: NodeId) -> Result<[WordHalf; 2]> {
        match self.vals[id as usize] {
            Some(Value::Word(w)) => Ok(w),
            _ => bail!("node {} is not a word value where one was needed", id),
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
                        // The val plane is a 16-bit mask expanded per lane;
                        // block bool inputs are fully known.
                        let dst =
                            self.pure(Key::LoadMask(off), move |d| Inst::LoadMask { dst: d, off });
                        Value::Bool([MaskVal::Reg(dst), MaskVal::Const(true)])
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
            Op::Word(w) => Value::Word([WordHalf::ConstU64(*w), WordHalf::ConstU64(*w)]),
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
                // Interval * positive-constant scalar: scale each endpoint
                // (monotone), matching `Pico8NumInterval::scale_positive`
                // and `graph.eval`. The rem-rung widening
                // (`trace::widen::rem_bucket_node`) is the only source; a
                // frame never multiplies an interval otherwise.
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
            // Interval / positive-constant scalar: divide each endpoint
            // (monotone, truncating like the scalar `Div`), matching
            // `Pico8NumInterval::div_positive` and `graph.eval`. Placed
            // before the scalar `Op::Div` Call arm below, which only reads
            // `as_num`. Source: `rem_bucket_node`'s `old / 2^-k` scale.
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
                // zi_flr takes the low endpoint's floor; zn_flr is the same
                // on a degenerate interval, so route both through `as_ival`.
                let iv = self.as_ival(a[0])?;
                let lo = self.num_reg(iv[0]);
                Value::Num(NumVal::Reg(self.flr(lo)))
            }
            Op::Bits => match self.dom(a[0]) {
                2 => {
                    // zw_bits_i: pack (lo << 32) | hi per lane, on each ZW half.
                    let iv = self.as_ival(a[0])?;
                    let ar = self.ival_regs(iv);
                    let lo = self.bits_n(ar[0]);
                    let hi = self.bits_n(ar[1]);
                    let h0 = self.pack_i(lo[0], hi[0]);
                    let h1 = self.pack_i(lo[1], hi[1]);
                    Value::Word([WordHalf::Reg(h0), WordHalf::Reg(h1)])
                }
                1 => {
                    // zw_bits_b: (val bit) | (known bit << 1) per lane.
                    let b = self.as_bool(a[0])?;
                    let (v, k) = (self.mask_reg(b[0]), self.mask_reg(b[1]));
                    let (h0, h1) = self.bits_b(v, k);
                    Value::Word([WordHalf::Reg(h0), WordHalf::Reg(h1)])
                }
                _ => {
                    let n = self.as_num(a[0])?;
                    let src = self.num_reg(n);
                    let lo = self.pure(Key::BitsLo(src), move |d| Inst::BitsLo { dst: d, src });
                    let hi = self.pure(Key::BitsHi(src), move |d| Inst::BitsHi { dst: d, src });
                    Value::Word([WordHalf::Reg(lo), WordHalf::Reg(hi)])
                }
            }
            Op::Mix(c, half) => {
                let h = self.as_word(a[0])?;
                let v = self.as_word(a[1])?;
                let c = *c as u64;
                let mut out = [WordHalf::ConstU64(0); 2];
                for i in 0..2 {
                    let vreg = match v[i] {
                        WordHalf::Reg(r) => r,
                        WordHalf::ConstU64(_) => {
                            bail!("node {}: Mix value operand half {} is a constant, unsupported", id, i)
                        }
                    };
                    // Combine `h` (register or broadcast constant) with `x`.
                    let combine = |lo: &mut Self, op: QOp, hh: WordHalf, x: Vreg| -> Vreg {
                        match hh {
                            WordHalf::Reg(hr) => lo.qbin(op, hr, Src::Reg(x)),
                            WordHalf::ConstU64(hc) => lo.qbin(op, x, Src::BU64(hc)),
                        }
                    };
                    let dst = if *half == 0 {
                        // zw_mix1: mix64(h ^ mix64(v ^ c))
                        let a0 = self.qbin(QOp::XorQ, vreg, Src::BU64(c));
                        let inner = self.mix64(a0);
                        let d = combine(self, QOp::XorQ, h[i], inner);
                        self.mix64(d)
                    } else {
                        // zw_mix2: h + mix64(v * ((c<<1)|1))
                        let k = (c << 1) | 1;
                        let a0 = self.qbin(QOp::MullQ, vreg, Src::BU64(k));
                        let inner = self.mix64(a0);
                        combine(self, QOp::AddQ, h[i], inner)
                    };
                    out[i] = WordHalf::Reg(dst);
                }
                Value::Word(out)
            }
            Op::ConstBool(b) => Value::Bool([MaskVal::Const(*b), MaskVal::Const(true)]),
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
                    // Numeric comparison -> ZB (known = all-ones). `vpcmpd`
                    // predicate: LT 1, LE 2, GT 6 (NLE), GE 5 (NLT).
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
                // Boolean Eq (zb_eq) or numeric Eq (zn_eq), by operand domain.
                match self.dom(a[0]) {
                    1 => {
                        let (p, q) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                        let (pv, qv) = (self.mask_reg(p[0]), self.mask_reg(q[0]));
                        // val = ~(pv ^ qv)
                        let x = self.dbin(ROp::XorD, pv, qv);
                        let val = self.not_mask(x);
                        let (pk, qk) = (self.mask_reg(p[1]), self.mask_reg(q[1]));
                        let known = self.dbin(ROp::AndD, pk, qk);
                        Value::Bool([MaskVal::Reg(val), MaskVal::Reg(known)])
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
                let v = self.mask_reg(b[0]);
                let nv = self.not_mask(v);
                Value::Bool([MaskVal::Reg(nv), b[1]])
            }
            op @ (Op::And | Op::Or) => {
                let (p, q) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                let (pv, qv) = (self.mask_reg(p[0]), self.mask_reg(q[0]));
                let (pk, qk) = (self.mask_reg(p[1]), self.mask_reg(q[1]));
                if matches!(op, Op::And) {
                    // val = pv & qv ; known_false = (~pv & pk) | (~qv & qk)
                    // known = (pk & qk) | known_false
                    let val = self.dbin(ROp::AndD, pv, qv);
                    let kfa = self.dbin(ROp::AndnD, pv, pk);
                    let kfb = self.dbin(ROp::AndnD, qv, qk);
                    let kf = self.dbin(ROp::OrD, kfa, kfb);
                    let kk = self.dbin(ROp::AndD, pk, qk);
                    let known = self.dbin(ROp::OrD, kk, kf);
                    Value::Bool([MaskVal::Reg(val), MaskVal::Reg(known)])
                } else {
                    // val = pv | qv ; known_true = (pv & pk) | (qv & qk)
                    let val = self.dbin(ROp::OrD, pv, qv);
                    let kta = self.dbin(ROp::AndD, pv, pk);
                    let ktb = self.dbin(ROp::AndD, qv, qk);
                    let kt = self.dbin(ROp::OrD, kta, ktb);
                    let kk = self.dbin(ROp::AndD, pk, qk);
                    let known = self.dbin(ROp::OrD, kk, kt);
                    Value::Bool([MaskVal::Reg(val), MaskVal::Reg(known)])
                }
            }
            Op::Known => {
                // Decidedness. `Known(Flr(x))` where x is an interval reads
                // the INTERVAL (zi_flr_ok), because Flr is exact BY this very
                // premise, so asking the result would answer itself.
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
            Op::Sel => {
                let c = self.as_bool(a[0])?;
                let cv = self.mask_reg(c[0]);
                // Dispatch on the JOINED arm domain, so a select of a number
                // and an interval takes the interval path (as_ival coerces the
                // numeric arm to [n, n]) - matching the Rust emitter, which
                // reads both arms at their common representation.
                match self.dom(a[1]).max(self.dom(a[2])) {
                    1 => {
                        let (t, f) = (self.as_bool(a[1])?, self.as_bool(a[2])?);
                        let (tv, fv) = (self.mask_reg(t[0]), self.mask_reg(f[0]));
                        let (tk, fk) = (self.mask_reg(t[1]), self.mask_reg(f[1]));
                        let val = self.vsel(cv, tv, fv);
                        let known = self.vsel(cv, tk, fk);
                        Value::Bool([MaskVal::Reg(val), MaskVal::Reg(known)])
                    }
                    2 => {
                        let (t, f) = (self.as_ival(a[1])?, self.as_ival(a[2])?);
                        let (tl, fl) = (self.num_reg(t[0]), self.num_reg(f[0]));
                        let (th, fh) = (self.num_reg(t[1]), self.num_reg(f[1]));
                        let lo = self.vsel(cv, tl, fl);
                        let hi = self.vsel(cv, th, fh);
                        Value::Ival([NumVal::Reg(lo), NumVal::Reg(hi)])
                    }
                    _ => {
                        let t = self.as_num(a[1])?;
                        let f = self.as_num(a[2])?;
                        let (tr, fr) = (self.num_reg(t), self.num_reg(f));
                        Value::Num(NumVal::Reg(self.vsel(cv, tr, fr)))
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
            Op::FragOk(c) => {
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                let (_, ok) = self.zi_fork_flr(ar, *c);
                Value::Bool([MaskVal::Reg(ok), MaskVal::Const(true)])
            }
            Op::SplitOk => {
                let iv = self.as_ival(a[0])?;
                let ar = self.ival_regs(iv);
                let ok = self.zi_span_ok(ar);
                Value::Bool([MaskVal::Reg(ok), MaskVal::Const(true)])
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

/// Nodes reachable from `roots`.
/// A lowering order that interleaves independent chains in BATCHES.
///
/// Interleaving independent rows by ASAP depth keeps the out-of-order
/// window full of independent work (the multiply ports stay fed), but
/// interleaving ALL rows at once makes every row's accumulator and mix
/// temporaries live simultaneously - hundreds of non-rematerializable
/// spills. So the roots are processed in batches of `batch`: within a
/// batch the (still-unscheduled) cone is ordered depth-major to expose
/// ILP, but only ~`batch` accumulators are ever live at once. Shared nodes
/// (the `Bits`) are scheduled in the first batch that reaches them and,
/// being rematerializable, are recomputed for free in later batches.
///
/// `depth` (longest path from the leaves) needs one forward pass since ids
/// refer downward; an operand's depth is strictly smaller, so depth-major
/// order is a valid evaluation order.
fn schedule(g: &Graph, live: &[bool], roots: &[NodeId], batch: usize) -> Vec<NodeId> {
    // ALAP depth (as-late-as-possible): a node placed just before its
    // earliest consumer. ASAP order computes every shared inner `mix64`
    // up front (their inputs are shallow), so they live the whole program
    // and spill; ALAP places each one right before the cell-step that uses
    // it, so its live range is one step. Backward pass in reverse id order:
    // an operand's consumers all have larger ids, so they are finalized
    // first.
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
        // Nodes reachable from this batch's roots that are not yet placed.
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
        Inst::BitsLo { src, .. } | Inst::BitsHi { src, .. } => out.push(*src),
        Inst::Srlq { a, .. } => out.push(*a),
        Inst::QBin { a, b, .. } => {
            out.push(*a);
            push_src(b, out);
        }
        Inst::Muldq { a, b, .. } | Inst::BlendImm { a, b, .. } => {
            out.push(*a);
            out.push(*b);
        }
        Inst::Sraq { a, .. } | Inst::Sllq { a, .. } => out.push(*a),
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

/// Reorder the flat instruction stream with a critical-path-height list
/// scheduler. Each vreg is defined once (SSA), so ANY topological order is
/// valid; emitting the highest-critical-path instruction among those whose
/// operands are ready spreads the ~24 independent accumulator chains so the
/// multiply port issues without waiting out each `vpmullq`'s ~15-cycle
/// latency. Node-level ordering could not do this: it emits each `Mix` as
/// ~16 contiguous instructions, so the two serial muls inside one `mix64`
/// sit adjacent with nothing to hide their latency. Spills are free here
/// (proven: runtime is flat across 120..671 spills), so pressure is ignored.
fn reschedule(insts: Vec<Inst>, n_vregs: Vreg) -> Vec<Inst> {
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

    // Pressure-aware list scheduling. Emitting the highest-critical-path
    // ready instruction exposes ILP but starts every independent chain at
    // once, spilling catastrophically. So above a live-register threshold we
    // switch objective to FREE registers - prefer the instruction that ends
    // the most operands' live ranges - and only chase ILP while there is
    // room. `rem` counts each vreg's not-yet-emitted consumers; a vreg dies
    // when its last consumer is emitted.
    let limit: usize = std::env::var("CELESTE_ASM_PRESSURE")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(16);
    let mut rem: Vec<u32> = vec![0; n_vregs as usize];
    for i in 0..n {
        for v in &uses[i] {
            rem[*v as usize] += 1;
        }
    }
    let mut ready: Vec<u32> = (0..n as u32).filter(|i| indeg[*i as usize] == 0).collect();
    let mut order: Vec<u32> = Vec::with_capacity(n);
    let mut live: usize = 0;
    while !ready.is_empty() {
        // Live-set delta if `i` is emitted: +1 if its def has consumers,
        // minus the operands whose last consumer is `i`.
        let delta = |i: u32| -> i32 {
            let i = i as usize;
            let dies = uses[i].iter().filter(|v| rem[**v as usize] == 1).count() as i32;
            let gains = match inst_def(&insts[i]) {
                Some(d) if !consumers[d as usize].is_empty() => 1,
                _ => 0,
            };
            gains - dies
        };
        // Pick: under pressure, minimize delta (free registers), breaking
        // ties by higher height; otherwise maximize height, breaking ties by
        // smaller delta and then original order for determinism.
        let tight = live >= limit;
        let best = ready
            .iter()
            .copied()
            .enumerate()
            .min_by(|&(_, a), &(_, b)| {
                let (da, db) = (delta(a), delta(b));
                let (ha, hb) = (height[a as usize], height[b as usize]);
                if tight {
                    da.cmp(&db).then(hb.cmp(&ha)).then(a.cmp(&b))
                } else {
                    hb.cmp(&ha).then(da.cmp(&db)).then(a.cmp(&b))
                }
            })
            .map(|(pos, _)| pos)
            .unwrap();
        let i = ready.swap_remove(best);
        order.push(i);
        live = (live as i32 + delta(i)).max(0) as usize;
        for v in &uses[i as usize] {
            rem[*v as usize] -= 1;
        }
        for &c in &consumers[i as usize] {
            indeg[c as usize] -= 1;
            if indeg[c as usize] == 0 {
                ready.push(c);
            }
        }
    }
    assert_eq!(order.len(), n, "list scheduler dropped instructions");
    let mut slots: Vec<Option<Inst>> = insts.into_iter().map(Some).collect();
    order.into_iter().map(|i| slots[i as usize].take().unwrap()).collect()
}

fn inst_def(inst: &Inst) -> Option<Vreg> {
    match inst {
        Inst::Load { dst, .. }
        | Inst::LoadMask { dst, .. }
        | Inst::BcastD { dst, .. }
        | Inst::RBin { dst, .. }
        | Inst::Neg { dst, .. }
        | Inst::Abs { dst, .. }
        | Inst::BitsLo { dst, .. }
        | Inst::BitsHi { dst, .. }
        | Inst::Srlq { dst, .. }
        | Inst::QBin { dst, .. }
        | Inst::Muldq { dst, .. }
        | Inst::Sraq { dst, .. }
        | Inst::Sllq { dst, .. }
        | Inst::BlendImm { dst, .. }
        | Inst::Cmp { dst, .. }
        | Inst::Ternlog { dst, .. }
        | Inst::Call { dst, .. } => Some(*dst),
        Inst::Store { .. } | Inst::StoreMask { .. } => None,
    }
}

/// Poletto-Sarkar linear scan. Returns a home for every vreg and the
/// number of spill slots used.
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
    // A spilled vreg's location. A rematerializable value (a load, or bits
    // of a load) costs no stack slot: it is recomputed from input memory at
    // each use instead of stored/reloaded. That is what keeps the 48 shared
    // `Bits` off the stack under the interleaved schedule.
    let spill_loc = |vreg: Vreg, n_slots: &mut u32| -> Loc {
        if remat[vreg as usize] {
            Loc::Spill(u32::MAX)
        } else {
            let s = *n_slots;
            *n_slots += 1;
            Loc::Spill(s)
        }
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
                home[ivs[spill_ai].0 as usize] = spill_loc(ivs[spill_ai].0, &mut n_slots);
                active.pop();
                active.push(cur);
                active.sort_by_key(|&ai| ivs[ai].2);
            } else {
                home[vreg as usize] = spill_loc(vreg, &mut n_slots);
            }
        }
    }
    (home, n_slots as usize)
}

// ---- emission ----

struct Emitter<'a> {
    home: &'a [Loc],
    insts: &'a [Inst],
    remat: &'a [bool],
    def_of: &'a [u32],
    /// rsp offset of the 32-zmm save area used around a call.
    save_off: u32,
    /// rsp offset of the call-out argument/result buffers.
    argbuf_off: u32,
    pool: Pool,
    out: String,
}

impl<'a> Emitter<'a> {
    fn slot_mem(slot: u32) -> String {
        format!("{}(%rsp)", slot as usize * 64)
    }

    /// Put vreg `v` into a usable register and return it: its home if it
    /// has one, otherwise `scratch` filled by a reload OR, for a
    /// rematerializable value (a load / bits-of-a-load), by recomputing it
    /// from input memory. Recompute needs at most two helper scratch
    /// registers (`H0`, `H1`) for the sub-operand and the extract temp.
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

    /// Recompute a rematerializable vreg into `dst` (a scratch reg),
    /// SELF-CONTAINED from input memory. Crucially it does NOT read any
    /// operand's register home: a remat happens later than the value's
    /// recorded live range, so that register may already have been reused.
    /// A remat chain bottoms at a `Load` (which reads `%rdi`), so recursing
    /// into the sub-operand and reloading from memory is always valid.
    /// `Bits`'s source is always a `Load` (it is remat only when its source
    /// is), so the recursion is at most one deep and `H0`/`H1` suffice.
    fn rematerialize(&mut self, v: Vreg, dst: u8) {
        match &self.insts[self.def_of[v as usize] as usize] {
            Inst::Load { off, .. } => {
                writeln!(self.out, "    vmovdqu64 {}(%r13), %zmm{}", off, dst).unwrap();
            }
            Inst::BitsLo { src, .. } => {
                let src = *src;
                self.rematerialize(src, H0);
                writeln!(self.out, "    vpmovzxdq %ymm{}, %zmm{}", H0, dst).unwrap();
            }
            Inst::BitsHi { src, .. } => {
                let src = *src;
                self.rematerialize(src, H0);
                writeln!(self.out, "    vextracti64x4 $1, %zmm{}, %ymm{}", H0, H1).unwrap();
                writeln!(self.out, "    vpmovzxdq %ymm{}, %zmm{}", H1, dst).unwrap();
            }
            other => {
                unreachable!("non-rematerializable def marked remat: {:?}", std::mem::discriminant(other))
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
        // A rematerializable value is never stored - it is recomputed at use.
        if self.remat[v as usize] {
            return;
        }
        if let Loc::Spill(slot) = self.home[v as usize] {
            writeln!(self.out, "    vmovdqu64 %zmm{}, {}", reg, Self::slot_mem(slot)).unwrap();
        }
    }

    /// A rematerializable value whose home is a spill slot needs no def-site
    /// computation at all: every use recomputes it. Returns true if the def
    /// of `v` should be skipped entirely.
    fn skip_def(&self, v: Vreg) -> bool {
        self.remat[v as usize] && matches!(self.home[v as usize], Loc::Spill(_))
    }

    /// A `Src` rendered as an instruction operand (register or broadcast
    /// memory), given the broadcast width in lanes (16 for d, 8 for q).
    fn src_operand(&mut self, s: Src, scratch: u8, wide16: bool) -> String {
        match s {
            Src::Reg(v) => format!("%zmm{}", self.use_reg(v, scratch)),
            Src::BI32(c) => {
                let l = self.pool.d(c);
                format!("{l}(%rip){{1to16}}")
            }
            Src::BU64(c) => {
                let l = self.pool.q(c);
                let dec = if wide16 { "1to16" } else { "1to8" };
                format!("{l}(%rip){{{dec}}}")
            }
        }
    }

    fn emit_inst(&mut self, inst: &Inst) {
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
                // Load the 16-bit val mask and expand it to a per-lane vector
                // mask (the reverse of StoreMask).
                writeln!(self.out, "    movzwl {}(%r13), %eax", off).unwrap();
                writeln!(self.out, "    kmovw %eax, %k1").unwrap();
                writeln!(self.out, "    vpmovm2d %k1, %zmm{}", d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::BcastD { dst, val } => {
                let l = self.pool.d(*val);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpbroadcastd {}(%rip), %zmm{}", l, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::RBin { dst, op, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let bop = self.src_operand(*b, OPB, true);
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
            Inst::BitsLo { dst, src } => {
                if self.skip_def(*dst) {
                    return;
                }
                let rs = self.use_reg(*src, OPA);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpmovzxdq %ymm{}, %zmm{}", rs, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::BitsHi { dst, src } => {
                if self.skip_def(*dst) {
                    return;
                }
                let rs = self.use_reg(*src, OPA);
                let d = self.def_reg(*dst);
                // Extract the high 256 bits into OPB's ymm as a temporary.
                writeln!(self.out, "    vextracti64x4 $1, %zmm{}, %ymm{}", rs, OPB).unwrap();
                writeln!(self.out, "    vpmovzxdq %ymm{}, %zmm{}", OPB, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Srlq { dst, a, imm } => {
                let ra = self.use_reg(*a, OPA);
                let d = self.def_reg(*dst);
                writeln!(self.out, "    vpsrlq ${}, %zmm{}, %zmm{}", imm, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::QBin { dst, op, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let bop = self.src_operand(*b, OPB, false);
                let d = self.def_reg(*dst);
                let mn = match op {
                    QOp::AddQ => "vpaddq",
                    QOp::XorQ => "vpxorq",
                    QOp::MullQ => "vpmullq",
                };
                writeln!(self.out, "    {} {}, %zmm{}, %zmm{}", mn, bop, ra, d).unwrap();
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
            Inst::BlendImm { dst, mask, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let rb = self.use_reg(*b, OPB);
                let d = self.def_reg(*dst);
                // k1 is the fixed scratch mask register.
                writeln!(self.out, "    movw ${}, %ax", *mask as i16).unwrap();
                writeln!(self.out, "    kmovw %eax, %k1").unwrap();
                // vpblendmd: lane takes the second source where k is set.
                writeln!(self.out, "    vpblendmd %zmm{}, %zmm{}, %zmm{}{{%k1}}", rb, ra, d).unwrap();
                self.store_def(*dst, d);
            }
            Inst::Cmp { dst, imm, a, b } => {
                let ra = self.use_reg(*a, OPA);
                let bop = self.src_operand(*b, OPB, true);
                let d = self.def_reg(*dst);
                // k1 = (a CMP b); expand mask to a per-lane vector mask.
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
                // 2. save all 32 zmm (the call clobbers every vector reg).
                for r in 0..32u32 {
                    writeln!(self.out, "    vmovdqu64 %zmm{}, {}(%rsp)", r, self.save_off + r * 64)
                        .unwrap();
                }
                let arg = |i: u32| self.argbuf_off + i * 64;
                // 3. set up C args and pick the ctx slot; `stack_arg` = a 7th
                //    scalar pushed for the tile_flag calls.
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
                // 5. restore all 32 zmm.
                for r in 0..32u32 {
                    writeln!(self.out, "    vmovdqu64 {}(%rsp), %zmm{}", self.save_off + r * 64, r)
                        .unwrap();
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
                    Src::BU64(c) => {
                        let l = self.pool.q(*c);
                        writeln!(self.out, "    vpbroadcastq {}(%rip), %zmm{}", l, OPA).unwrap();
                        OPA
                    }
                };
                writeln!(self.out, "    vmovdqu64 %zmm{}, {}(%r14)", r, off).unwrap();
            }
        }
    }
}

/// Compile `roots` (each a word-domain node) of `g` into AVX-512 assembly.
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
    // Repr-aware input layout: each cell sized by its repr (Ival needs two
    // planes), laid out ascending.
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
    // Schedule: lower nodes in DEPTH-major order (ASAP level, then id).
    // The graph is a valid topological order by id, but that emits each
    // independent row's whole chain back-to-back - ~384 instructions apart,
    // outside the reorder-buffer window, so the multiply ports starve.
    // Ordering by (depth, id) interleaves the independent rows step-by-step:
    // at each depth every row's mix-at-that-step is adjacent, so the two
    // mul ports stay fed. Operands always have strictly smaller depth, so
    // this is still a valid evaluation order for the `Value` map.
    // Batch size for the interleaving scheduler. Env-tunable while the
    // sweet spot is being measured; defaults to a value that keeps live
    // accumulators well under the 26 homes.
    let batch = std::env::var("CELESTE_ASM_BATCH")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(4);
    let order = schedule(g, &live, roots, batch);
    for id in order {
        lo.lower_node(id)?;
    }

    // Roots -> typed stores, each into its own 128-byte slot.
    let mut root_kinds: Vec<RootKind> = Vec::with_capacity(roots.len());
    for (ri, r) in roots.iter().enumerate() {
        let base = ri as u32 * 128;
        let kind = match lo.vals[*r as usize] {
            Some(Value::Word(w)) => {
                for half in 0..2 {
                    let src = lo.word_src(w[half]);
                    lo.insts.push(Inst::Store { off: base + half as u32 * 64, src });
                }
                RootKind::Word
            }
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
    }

    // Instruction-level list scheduling to interleave the independent mul
    // chains (see `reschedule`). On by default; CELESTE_ASM_SCHED=0 disables
    // it for A/B.
    if std::env::var("CELESTE_ASM_SCHED").map(|s| s != "0").unwrap_or(true) {
        lo.insts = reschedule(std::mem::take(&mut lo.insts), lo.next_vreg);
    }

    // Which vregs are cheap to recompute from input memory (a load, or bits
    // of a load), and where each is defined. These let the allocator spill
    // them for free (rematerialize instead of store/reload).
    let mut remat = vec![false; lo.next_vreg as usize];
    let mut def_of = vec![0u32; lo.next_vreg as usize];
    for (i, inst) in lo.insts.iter().enumerate() {
        if let Some(d) = inst_def(inst) {
            def_of[d as usize] = i as u32;
            // A load is rematerializable (reads `%rdi`). Bits-of-a-load is
            // too, but Bits-of-arithmetic is NOT - its source is neither a
            // load nor kept live at the remat point. Transitive, and insts
            // are in emission order so the source's flag is already set.
            remat[d as usize] = match inst {
                Inst::Load { .. } => true,
                Inst::BitsLo { src, .. } | Inst::BitsHi { src, .. } => remat[*src as usize],
                _ => false,
            };
        }
    }

    let (home, spill_slots) = allocate(&lo.insts, lo.next_vreg, &remat);

    // Any call-outs? They need a 32-zmm save area and arg buffers above the
    // spill region, and the r13/r14/r15 prologue.
    let has_calls = lo.insts.iter().any(|i| matches!(i, Inst::Call { .. }));
    let spill_bytes = spill_slots as u32 * 64;
    let save_off = spill_bytes;
    let argbuf_off = spill_bytes + SAVE_BYTES;

    let mut em = Emitter {
        home: &home,
        insts: &lo.insts,
        remat: &remat,
        def_of: &def_of,
        save_off,
        argbuf_off,
        pool: Pool::default(),
        out: String::new(),
    };
    // Body first (fills the pool), then wrap with prologue/epilogue.
    let mut body = String::new();
    std::mem::swap(&mut em.out, &mut body);
    for inst in &lo.insts {
        em.emit_inst(inst);
    }
    std::mem::swap(&mut em.out, &mut body); // em.out empty again, body holds the code

    // Frame: spill slots, then (if any call-outs) the zmm save area and the
    // call-out arg buffers. r13/r14/r15 are callee-saved and hold the input,
    // output and ctx pointers so a `call` may clobber rdi/rsi/rdx.
    let extra = if has_calls { SAVE_BYTES + ARGBUF_BYTES } else { 0 };
    let frame = spill_bytes + extra;
    let mut asm = String::new();
    writeln!(asm, ".text").unwrap();
    writeln!(asm, ".globl {sym}").unwrap();
    writeln!(asm, "{sym}:").unwrap();
    // Three pushes make rsp 16-aligned (entry is 8 mod 16); a 64-multiple
    // frame keeps it aligned for calls.
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
    for (i, v) in em.pool.q.iter().enumerate() {
        writeln!(asm, ".LCq{i}: .quad {}", v).unwrap();
    }
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
        root_kinds,
        sym: sym.to_string(),
        spill_slots,
    })
}
