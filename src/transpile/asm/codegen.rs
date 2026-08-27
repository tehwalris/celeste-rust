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

const C1: u64 = 0xbf58_476d_1ce4_e5b9; // mix64 multiplier 1
const C2: u64 = 0x94d0_49bb_1331_11eb; // mix64 multiplier 2
const FLR_MASK: i32 = 0xffff_0000u32 as i32;

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

#[derive(Clone, Copy)]
enum Value {
    Num(NumVal),
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
    BcastD(i32),
    RBin(u8, Vreg, SrcKey),
    Neg(Vreg),
    Abs(Vreg),
    BitsLo(Vreg),
    BitsHi(Vreg),
    Srlq(Vreg, u8),
    QBin(u8, SrcKey, SrcKey),
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
    Store { off: u32, src: Src },
}

/// The result of lowering: the assembly text plus the two layouts a caller
/// needs to pack its buffers.
pub struct Compiled {
    pub asm: String,
    /// Input cells, in the ascending order they occupy the input buffer.
    /// Cell `input_cells[i]` is the `ZN` (64 bytes) at byte offset `i*64`.
    pub input_cells: Vec<u32>,
    /// Number of `ZW` roots. Root `i` is at byte offset `i*128`: half0 at
    /// `i*128`, half1 at `i*128 + 64`.
    pub n_roots: usize,
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

    fn as_num(&self, id: NodeId) -> Result<NumVal> {
        match self.vals[id as usize] {
            Some(Value::Num(n)) => Ok(n),
            _ => bail!("node {} is not a numeric value where one was needed", id),
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
                if lo != hi {
                    bail!("node {}: the asm slice supports only exact Const, got [{lo},{hi}]", id);
                }
                Value::Num(NumVal::ConstI32(*lo))
            }
            Op::Cell(c) => {
                let off = *self
                    .cell_off
                    .get(c)
                    .expect("input cell offset assigned before lowering");
                let dst = self.pure(Key::Load(off), move |d| Inst::Load { dst: d, off });
                Value::Num(NumVal::Reg(dst))
            }
            Op::Word(w) => Value::Word([WordHalf::ConstU64(*w), WordHalf::ConstU64(*w)]),
            op @ (Op::Add | Op::Sub | Op::Min | Op::Max) => {
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
            Op::Neg => {
                let areg = {
                    let n = self.as_num(a[0])?;
                    self.num_reg(n)
                };
                let dst = self.pure(Key::Neg(areg), move |d| Inst::Neg { dst: d, a: areg });
                Value::Num(NumVal::Reg(dst))
            }
            Op::Abs => {
                let areg = {
                    let n = self.as_num(a[0])?;
                    self.num_reg(n)
                };
                let dst = self.pure(Key::Abs(areg), move |d| Inst::Abs { dst: d, a: areg });
                Value::Num(NumVal::Reg(dst))
            }
            Op::Flr => {
                let areg = {
                    let n = self.as_num(a[0])?;
                    self.num_reg(n)
                };
                let key = Key::RBin(4, areg, SrcKey::I32(FLR_MASK));
                let dst = self.pure(key, move |d| Inst::RBin {
                    dst: d,
                    op: ROp::AndD,
                    a: areg,
                    b: Src::BI32(FLR_MASK),
                });
                Value::Num(NumVal::Reg(dst))
            }
            Op::Bits => {
                let n = self.as_num(a[0])?;
                let src = self.num_reg(n);
                let lo = self.pure(Key::BitsLo(src), move |d| Inst::BitsLo { dst: d, src });
                let hi = self.pure(Key::BitsHi(src), move |d| Inst::BitsHi { dst: d, src });
                Value::Word([WordHalf::Reg(lo), WordHalf::Reg(hi)])
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
        Inst::Load { .. } | Inst::BcastD { .. } => {}
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
        Inst::Store { src, .. } => push_src(src, out),
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
        | Inst::BcastD { dst, .. }
        | Inst::RBin { dst, .. }
        | Inst::Neg { dst, .. }
        | Inst::Abs { dst, .. }
        | Inst::BitsLo { dst, .. }
        | Inst::BitsHi { dst, .. }
        | Inst::Srlq { dst, .. }
        | Inst::QBin { dst, .. } => Some(*dst),
        Inst::Store { .. } => None,
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
                writeln!(self.out, "    vmovdqu64 {}(%rdi), %zmm{}", off, dst).unwrap();
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
                writeln!(self.out, "    vmovdqu64 {}(%rdi), %zmm{}", off, d).unwrap();
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
                writeln!(self.out, "    vmovdqu64 %zmm{}, {}(%rsi)", r, off).unwrap();
            }
        }
    }
}

/// Compile `roots` (each a word-domain node) of `g` into AVX-512 assembly.
pub fn compile(g: &Graph, roots: &[NodeId], sym: &str) -> Result<Compiled> {
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
    let cell_off: HashMap<u32, u32> =
        cells.iter().enumerate().map(|(i, c)| (*c, i as u32 * 64)).collect();

    let mut lo = Lower {
        g,
        vals: vec![None; g.len()],
        insts: Vec::new(),
        next_vreg: 0,
        input_cells: cells.clone(),
        cell_off,
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

    // Roots -> stores.
    for (ri, r) in roots.iter().enumerate() {
        let w = lo.as_word(*r)?;
        for half in 0..2 {
            let src = lo.word_src(w[half]);
            let off = ri as u32 * 128 + half as u32 * 64;
            lo.insts.push(Inst::Store { off, src });
        }
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

    let mut em = Emitter {
        home: &home,
        insts: &lo.insts,
        remat: &remat,
        def_of: &def_of,
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

    let frame = spill_slots * 64;
    let mut asm = String::new();
    writeln!(asm, ".text").unwrap();
    writeln!(asm, ".globl {sym}").unwrap();
    writeln!(asm, "{sym}:").unwrap();
    if frame > 0 {
        writeln!(asm, "    sub ${}, %rsp", frame).unwrap();
    }
    writeln!(asm, "    vpxorq %zmm{Z}, %zmm{Z}, %zmm{Z}", Z = ZERO).unwrap();
    asm.push_str(&body);
    if frame > 0 {
        writeln!(asm, "    add ${}, %rsp", frame).unwrap();
    }
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
        n_roots: roots.len(),
        sym: sym.to_string(),
        spill_slots,
    })
}
