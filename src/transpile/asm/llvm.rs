//! PROTOTYPE (branch `llvm-backend`, the TEMPORARY cargo feature `llvm`):
//! `transpile::graph::Graph` -> LLVM IR, built into the shared object the
//! runtime loads (`build`) by an explicit pipeline: `llc` with `LLC_FLAGS`
//! (after `opt` with `OPT_PASSES`, none by default), its text scanned for a
//! fused multiply-add (`jit::refuse_forbidden`), GNU `as` and `ld -shared`.
//! The kernels' ABI and buffer layouts are `codegen`'s (`Compiled`), so
//! `compiled::asm_kernel` calls them unchanged; each op computes what
//! `codegen` computes, bit for bit (`super::tests` checks both backends
//! alike).
//!
//! The IR is the graph's ops in vector types: 16.16 as `<16 x i32>`, the
//! doubles as `<8 x double>` (the body runs twice per call, a loop over the
//! halves); a tri-state boolean is two `<N x i1>` planes `[val, known]`
//! (k-registers, where `codegen` keeps vector masks), an interval two
//! number planes `[lo, hi]`. Value numbering, dead code, scheduling,
//! register allocation and the peephole are LLVM's (`OPT_PASSES`,
//! `LLC_FLAGS`); the emitter folds only constant planes (`Mask::Const`,
//! `Num`'s literals), so a literal stays one where the lowering requires
//! one (`tile_flag_at`'s box).

use std::collections::HashMap;
use std::fmt::Write;
use std::path::PathBuf;

use anyhow::{bail, Result};

use celeste_core::arith::{Arithmetic, Fixed, Float};

use super::codegen::{input_layout, output_layout, schedule, text_digest, Compiled, CTX_DIV, CTX_ENV, CTX_MGET, CTX_REM, CTX_SIN, CTX_TILE, CTX_TILE_LANES};
use crate::transpile::garith::GraphArith;
use crate::transpile::graph::{self, Cut, Graph, NodeId, Op, Ty};

/// THE IR PASSES (`opt -passes`): none. The kernel is one straight-line
/// block, and SelectionDAG builds it hash-consed (value numbering) and
/// folds it (its combiner), dropping what no root reads; `early-cse` before
/// it (`codegen`'s GVN memo and identity folds as an IR pass) left `llc`'s
/// output the same within 1% and cost 13% more (r10's largest kernels). An
/// empty list: `llc` reads the emitted text itself.
pub(crate) const OPT_PASSES: &str = "";

/// THE CODE GENERATOR (`llc`), each flag's reason:
/// - `-O2`: SelectionDAG instruction selection with its combiner (the
///   peephole), the machine scheduler (`codegen::reschedule`'s list
///   scheduler) and the greedy register allocator with rematerialization
///   and spill placement (`allocate`, `call_saves`, `drop_redundant_reloads`);
/// - `-mattr`: AVX-512 F/BW/DQ/VL, what `codegen`'s text uses;
/// - `-relocation-model=pic`: a shared object;
/// - no `-fp-contract`: the X86 backend ignores it and fuses only what IR
///   flags allow (`contract`, `reassoc`), which the emitter never sets; the
///   scan of `llc`'s text refuses a fused multiply-add all the same;
/// - `-x86-use-vzeroupper=false`: no `vzeroupper` before each call-out
///   (`codegen` emits none; the epilogue's stays);
/// - `-no-integrated-as`: text for GNU as (it assembles it after the scan,
///   as `codegen`'s), without LLVM-only directives (`.prefalign`).
///
/// Measured alternatives (2026-10-27): adding `-combiner-disabled
/// -enable-misched=false -pre-RA-sched=list-burr` cut `llc`'s CPU by 35-42%
/// (r10, r62h, r60s) with the kernels' speed unchanged within noise and the
/// check ladder green; `-O1` and `-O3` emit what `-O2` does; `-O0`,
/// `-regalloc=fast` and `-regalloc=basic` cost 15-26% more instructions and
/// twice the spills; `opt -passes=early-cse,instcombine` saves 9% of the
/// instructions for 5x `opt`'s cost.
pub(crate) const LLC_FLAGS: &[&str] = &["-O2", "-mattr=+avx512f,+avx512bw,+avx512dq,+avx512vl", "-relocation-model=pic", "-x86-use-vzeroupper=false", "-no-integrated-as"];

/// The pipeline the builds run: `OPT_PASSES` and `LLC_FLAGS`, or
/// TEMPORARY overrides for the A/B of passes and flags without a rebuild
/// (`LLVM_PROTO_OPT`: the passes, empty for none; `LLVM_PROTO_LLC`: the
/// flags, space-separated). The kernel cache keys on it (`describe`).
fn pipeline() -> &'static (Option<String>, Vec<String>) {
    static P: std::sync::OnceLock<(Option<String>, Vec<String>)> = std::sync::OnceLock::new();
    P.get_or_init(|| {
        let passes = std::env::var("LLVM_PROTO_OPT").unwrap_or_else(|_| OPT_PASSES.to_string());
        let llc = std::env::var("LLVM_PROTO_LLC").map(|s| s.split_whitespace().map(str::to_string).collect()).unwrap_or_else(|_| LLC_FLAGS.iter().map(|s| s.to_string()).collect());
        ((!passes.is_empty()).then_some(passes), llc)
    })
}

/// The toolchain and pipeline, for the kernel cache's key: `llc`'s version,
/// the passes and the flags.
pub(crate) fn describe() -> String {
    let version = std::process::Command::new("llc").arg("--version").output().map(|o| String::from_utf8_lossy(&o.stdout).lines().find(|l| l.contains("version")).unwrap_or("").trim().to_string()).unwrap_or_else(|e| format!("llc unavailable: {e}"));
    let (passes, llc) = pipeline();
    let link = std::env::var("LLVM_PROTO_LINK").unwrap_or_else(|_| "as+ld".to_string());
    format!("{version}; opt -passes={}; llc {}; link {link}", passes.as_deref().unwrap_or("(none)"), llc.join(" "))
}

/// A number plane: an SSA value or a literal (in every lane).
#[derive(Clone, Copy, PartialEq, Debug)]
enum Num {
    Reg(u32),
    I32(i32),
    /// A double, by its bits.
    Q(u64),
}

/// A boolean plane: an `<N x i1>` SSA value or a constant.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Mask {
    Reg(u32),
    Const(bool),
}

#[derive(Clone, Copy)]
enum Value {
    Num(Num),
    /// `[val, known]`.
    Bool([Mask; 2]),
    /// `[lo, hi]`.
    Ival([Num; 2]),
}

/// A number plane as an operand.
fn n(v: Num) -> String {
    match v {
        Num::Reg(r) => format!("%v{r}"),
        Num::I32(c) => format!("splat (i32 {c})"),
        Num::Q(b) => format!("splat (double 0x{b:016X})"),
    }
}

/// An arithmetic's lanes and types.
trait LlArith: GraphArith {
    /// The number vector type.
    const NUM: &'static str;
    /// The boolean vector type.
    const MASK: &'static str;
    /// A boolean plane in memory (one bit a lane).
    const MASK_INT: &'static str;
    fn konst(lit: Self::Lit) -> Num;
}

impl LlArith for Fixed {
    const NUM: &'static str = "<16 x i32>";
    const MASK: &'static str = "<16 x i1>";
    const MASK_INT: &'static str = "i16";
    fn konst(lit: i32) -> Num {
        Num::I32(lit)
    }
}

impl LlArith for Float {
    const NUM: &'static str = "<8 x double>";
    const MASK: &'static str = "<8 x i1>";
    const MASK_INT: &'static str = "i8";
    fn konst(lit: u64) -> Num {
        Num::Q(lit)
    }
}

/// A call-out (`callout::AsmCtx`'s function at `ctx`).
#[derive(Clone, Copy, PartialEq)]
enum Call {
    /// `(out, a, b)`: 16.16's `/`, `%`.
    Arith2(u32),
    /// `(out, a)`: `sin`.
    Arith1(u32),
    /// `(env, out, x, y)`.
    Mget,
    /// `(env, out_mask, x, y, w, h, flag)`, the box whole numbers.
    TileFlag(i32, i32, i32),
    /// `(env, out_mask, x, y, w, h, flag)`, the box per lane.
    TileFlagLanes(i32),
}

/// The emission: one node at a time into `body`, `%v` values.
struct Emit<'a, A: GraphArith> {
    g: &'a Graph<A>,
    vals: Vec<Option<Value>>,
    body: String,
    next: u32,
    cell_off: HashMap<u32, u32>,
    has_calls: bool,
}

macro_rules! ins {
    ($e:expr, $($t:tt)*) => {{
        let v = $e.next;
        $e.next += 1;
        let _ = write!($e.body, "  %v{v} = ");
        let _ = writeln!($e.body, $($t)*);
        v
    }};
}

mod float;

impl<A: LlArith> Emit<'_, A> {
    fn as_num(&self, id: NodeId) -> Result<Num> {
        match self.vals[id as usize] {
            Some(Value::Num(v)) => Ok(v),
            _ => bail!("node {}: not a numeric value where one was needed", crate::transpile::why::Explain::of(self.g).with_args(id)),
        }
    }

    fn as_bool(&self, id: NodeId) -> Result<[Mask; 2]> {
        match self.vals[id as usize] {
            Some(Value::Bool(b)) => Ok(b),
            _ => bail!("node {id} is not a boolean value where one was needed"),
        }
    }

    fn as_ival(&self, id: NodeId) -> Result<[Num; 2]> {
        match self.vals[id as usize] {
            Some(Value::Ival(i)) => Ok(i),
            Some(Value::Num(v)) => Ok([v, v]),
            _ => bail!("node {id} is not an interval where one was needed"),
        }
    }

    fn wide(&self, id: NodeId) -> bool {
        self.g.ty(id) == Ty::Ival
    }

    /// `base + off` (an input or output byte).
    fn at(&mut self, base: &str, off: u32) -> u32 {
        ins!(self, "getelementptr inbounds i8, ptr {base}, i64 {off}")
    }

    fn load(&mut self, off: u32) -> Num {
        let p = self.at("%inp", off);
        Num::Reg(ins!(self, "load {}, ptr %v{p}, align 1, !invariant.load !0", A::NUM))
    }

    /// The lane mask at input byte `off`.
    fn load_mask(&mut self, off: u32) -> Mask {
        let p = self.at("%inp", off);
        let bits = ins!(self, "load {}, ptr %v{p}, align 1, !invariant.load !0", A::MASK_INT);
        Mask::Reg(ins!(self, "bitcast {} %v{bits} to {}", A::MASK_INT, A::MASK))
    }

    /// A boolean op of two planes.
    fn mbin(&mut self, op: &str, p: u32, q: u32) -> Mask {
        Mask::Reg(ins!(self, "{op} {} %v{p}, %v{q}", A::MASK))
    }

    // Constant planes fold by identities that hold in every lane (`x & 1 =
    // x`, `c ? t : t = t`, ...), as `codegen`'s.

    fn m_and(&mut self, p: Mask, q: Mask) -> Mask {
        match (p, q) {
            (Mask::Const(false), _) | (_, Mask::Const(false)) => Mask::Const(false),
            (Mask::Const(true), x) | (x, Mask::Const(true)) => x,
            (Mask::Reg(a), Mask::Reg(b)) if a == b => p,
            (Mask::Reg(a), Mask::Reg(b)) => self.mbin("and", a, b),
        }
    }

    fn m_or(&mut self, p: Mask, q: Mask) -> Mask {
        match (p, q) {
            (Mask::Const(true), _) | (_, Mask::Const(true)) => Mask::Const(true),
            (Mask::Const(false), x) | (x, Mask::Const(false)) => x,
            (Mask::Reg(a), Mask::Reg(b)) if a == b => p,
            (Mask::Reg(a), Mask::Reg(b)) => self.mbin("or", a, b),
        }
    }

    fn m_not(&mut self, p: Mask) -> Mask {
        match p {
            Mask::Const(b) => Mask::Const(!b),
            Mask::Reg(a) => Mask::Reg(ins!(self, "xor {} %v{a}, splat (i1 true)", A::MASK)),
        }
    }

    /// `~p & q`.
    fn m_andn(&mut self, p: Mask, q: Mask) -> Mask {
        let np = self.m_not(p);
        self.m_and(np, q)
    }

    fn m_xor(&mut self, p: Mask, q: Mask) -> Mask {
        match (p, q) {
            (Mask::Const(a), Mask::Const(b)) => Mask::Const(a != b),
            (Mask::Const(false), x) | (x, Mask::Const(false)) => x,
            (Mask::Const(true), x) | (x, Mask::Const(true)) => self.m_not(x),
            (Mask::Reg(a), Mask::Reg(b)) if a == b => Mask::Const(false),
            (Mask::Reg(a), Mask::Reg(b)) => self.mbin("xor", a, b),
        }
    }

    /// `c ? t : f`, one boolean plane.
    fn m_sel(&mut self, c: Mask, t: Mask, f: Mask) -> Mask {
        match (c, t, f) {
            (Mask::Const(true), t, _) => t,
            (Mask::Const(false), _, f) => f,
            (_, t, f) if t == f => t,
            (c, Mask::Const(true), Mask::Const(false)) => c,
            (c, Mask::Const(false), Mask::Const(true)) => self.m_not(c),
            (c, Mask::Const(true), f) => self.m_or(c, f),
            (c, Mask::Const(false), f) => self.m_andn(c, f),
            (c, t, Mask::Const(true)) => {
                let nc = self.m_not(c);
                self.m_or(nc, t)
            }
            (c, t, Mask::Const(false)) => self.m_and(c, t),
            (Mask::Reg(c), Mask::Reg(t), Mask::Reg(f)) => Mask::Reg(ins!(self, "select {} %v{c}, {} %v{t}, {} %v{f}", A::MASK, A::MASK, A::MASK)),
        }
    }

    /// `c ? t : f` on a number plane.
    fn n_sel(&mut self, c: Mask, t: Num, f: Num) -> Num {
        match c {
            Mask::Const(true) => t,
            Mask::Const(false) => f,
            _ if t == f => t,
            Mask::Reg(c) => Num::Reg(ins!(self, "select {} %v{c}, {} {}, {} {}", A::MASK, A::NUM, n(t), A::NUM, n(f))),
        }
    }

    /// A call-out: the number arguments through the stack buffers
    /// `%arg0..`, the result from `%res` (a number, or a lane mask for a
    /// tile test).
    fn call(&mut self, call: Call, args: &[Num]) -> Value {
        self.has_calls = true;
        for (i, a) in args.iter().enumerate() {
            let _ = writeln!(self.body, "  store {} {}, ptr %arg{i}, align 64", A::NUM, n(*a));
        }
        let ctx_off = match call {
            Call::Arith2(o) | Call::Arith1(o) => o,
            Call::Mget => CTX_MGET,
            Call::TileFlag(..) => CTX_TILE,
            Call::TileFlagLanes(_) => CTX_TILE_LANES,
        };
        let fp_at = self.at("%ctx", ctx_off);
        let fp = ins!(self, "load ptr, ptr %v{fp_at}, align 8, !invariant.load !0");
        let mut params = Vec::new();
        if !matches!(call, Call::Arith2(_) | Call::Arith1(_)) {
            let env_at = self.at("%ctx", CTX_ENV);
            let env = ins!(self, "load ptr, ptr %v{env_at}, align 8, !invariant.load !0");
            params.push(format!("ptr %v{env}"));
        }
        params.push("ptr %res".to_string());
        params.extend((0..args.len()).map(|i| format!("ptr %arg{i}")));
        match call {
            Call::TileFlag(w, h, flag) => params.extend([format!("i32 {w}"), format!("i32 {h}"), format!("i32 {flag}")]),
            Call::TileFlagLanes(flag) => params.push(format!("i32 {flag}")),
            _ => {}
        }
        let _ = writeln!(self.body, "  call void %v{fp}({})", params.join(", "));
        match call {
            Call::TileFlag(..) | Call::TileFlagLanes(_) => {
                let bits = ins!(self, "load {}, ptr %res, align 64", A::MASK_INT);
                Value::Bool([Mask::Reg(ins!(self, "bitcast {} %v{bits} to {}", A::MASK_INT, A::MASK)), Mask::Const(true)])
            }
            _ => Value::Num(Num::Reg(ins!(self, "load {}, ptr %res, align 64", A::NUM))),
        }
    }

    /// The ops every arithmetic lowers alike (`codegen`'s `lower_common`);
    /// `None` for the arithmetic's own.
    fn lower_common(&mut self, id: NodeId) -> Result<Option<Value>> {
        let node = self.g.get(id);
        let a = node.args.clone();
        Ok(Some(match &node.op {
            Op::Const(lo, hi) if lo == hi => Value::Num(A::konst(*lo)),
            Op::Const(lo, hi) => Value::Ival([A::konst(*lo), A::konst(*hi)]),
            Op::Cell(c) => {
                let off = *self.cell_off.get(c).expect("input cell offset assigned before lowering");
                match self.g.cell_kind(*c) {
                    Ty::Num => Value::Num(self.load(off)),
                    Ty::Bool => Value::Bool([self.load_mask(off), Mask::Const(true)]),
                    Ty::UBool => {
                        let val = self.load_mask(off);
                        Value::Bool([val, self.load_mask(off + 2)])
                    }
                    Ty::Ival => {
                        let lo = self.load(off);
                        Value::Ival([lo, self.load(off + 64)])
                    }
                }
            }
            Op::Eq if self.g.ty(a[0]).is_bool() => {
                let (p, q) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                let x = self.m_xor(p[0], q[0]);
                let val = self.m_not(x);
                let known = self.m_and(p[1], q[1]);
                Value::Bool([val, known])
            }
            Op::Known if self.g.ty(a[0]).is_bool() => Value::Bool([self.as_bool(a[0])?[1], Mask::Const(true)]),
            Op::ConstBool(b) => Value::Bool([Mask::Const(*b), Mask::Const(true)]),
            Op::UnknownBool(_) => Value::Bool([Mask::Const(false), Mask::Const(false)]),
            Op::Mget => {
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                self.call(Call::Mget, &[x, y])
            }
            Op::TileFlagAt => {
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                let (wv, hv, fv) = (self.as_num(a[2])?, self.as_num(a[3])?, self.as_num(a[4])?);
                let flag = literal_i16::<A>(id, fv, "flag")?;
                if matches!(wv, Num::Reg(_)) || matches!(hv, Num::Reg(_)) {
                    self.call(Call::TileFlagLanes(flag), &[x, y, wv, hv])
                } else {
                    let (w, h) = (literal_i16::<A>(id, wv, "width")?, literal_i16::<A>(id, hv, "height")?);
                    self.call(Call::TileFlag(w, h, flag), &[x, y])
                }
            }
            Op::UnknownNum { .. } => bail!("node {}: an unknown number reached the kernel", id),
            Op::Not => {
                let b = self.as_bool(a[0])?;
                let nv = self.m_not(b[0]);
                Value::Bool([nv, b[1]])
            }
            op @ (Op::And | Op::Or) => {
                let ([pv, pk], [qv, qk]) = (self.as_bool(a[0])?, self.as_bool(a[1])?);
                let kk = self.m_and(pk, qk);
                if matches!(op, Op::And) {
                    // known = (pk & qk) | (~pv & pk) | (~qv & qk)
                    let val = self.m_and(pv, qv);
                    let kfa = self.m_andn(pv, pk);
                    let kfb = self.m_andn(qv, qk);
                    let kf = self.m_or(kfa, kfb);
                    Value::Bool([val, self.m_or(kk, kf)])
                } else {
                    // known = (pk & qk) | (pv & pk) | (qv & qk)
                    let val = self.m_or(pv, qv);
                    let kta = self.m_and(pv, pk);
                    let ktb = self.m_and(qv, qk);
                    let kt = self.m_or(kta, ktb);
                    Value::Bool([val, self.m_or(kk, kt)])
                }
            }
            Op::Lo | Op::Hi => {
                if self.wide(a[0]) {
                    let iv = self.as_ival(a[0])?;
                    Value::Num(iv[if matches!(node.op, Op::Lo) { 0 } else { 1 }])
                } else {
                    Value::Num(self.as_num(a[0])?)
                }
            }
            Op::Sel => {
                let cv = self.as_bool(a[0])?[0];
                match self.g.ty(id) {
                    Ty::Bool | Ty::UBool => {
                        let (t, f) = (self.as_bool(a[1])?, self.as_bool(a[2])?);
                        let val = self.m_sel(cv, t[0], f[0]);
                        Value::Bool([val, self.m_sel(cv, t[1], f[1])])
                    }
                    Ty::Ival => {
                        let (t, f) = (self.as_ival(a[1])?, self.as_ival(a[2])?);
                        let lo = self.n_sel(cv, t[0], f[0]);
                        Value::Ival([lo, self.n_sel(cv, t[1], f[1])])
                    }
                    Ty::Num => {
                        let (t, f) = (self.as_num(a[1])?, self.as_num(a[2])?);
                        Value::Num(self.n_sel(cv, t, f))
                    }
                }
            }
            Op::Span => Value::Ival([self.as_ival(a[0])?[0], self.as_ival(a[1])?[1]]),
            Op::Restrict(..) => match self.vals[a[0] as usize] {
                Some(v) => v,
                None => bail!("node {}: its operand was not lowered", id),
            },
            _ => return Ok(None),
        }))
    }
}

/// A literal lane value as a whole number (`tile_flag_at`'s box and flag).
fn literal_i16<A: GraphArith>(id: NodeId, v: Num, what: &str) -> Result<i32> {
    let w = match v {
        Num::I32(raw) => celeste_core::pico8_num::Pico8Num::from_raw(raw).as_i16(),
        Num::Q(bits) => celeste_core::lua_f64::LuaF64::new(f64::from_bits(bits)).and_then(celeste_core::lua_f64::LuaF64::as_i16),
        Num::Reg(_) => bail!("node {id}: tile_flag's {what} must be an exact constant"),
    };
    w.map(i32::from).ok_or_else(|| anyhow::anyhow!("node {id}: tile_flag's {what} {v:?} is not a whole number"))
}

const STEP: i32 = 1 << 16;
const FLR_MASK: i32 = 0xffff_0000u32 as i32;

impl Emit<'_, Fixed> {
    fn ibin(&mut self, op: &str, a: Num, b: Num) -> Num {
        Num::Reg(ins!(self, "{op} <16 x i32> {}, {}", n(a), n(b)))
    }

    fn intrinsic(&mut self, f: &str, a: Num, b: Num) -> Num {
        Num::Reg(ins!(self, "call <16 x i32> @llvm.{f}.v16i32(<16 x i32> {}, <16 x i32> {})", n(a), n(b)))
    }

    /// A signed (or `eq`) compare.
    fn cmp(&mut self, pred: &str, a: Num, b: Num) -> Mask {
        Mask::Reg(ins!(self, "icmp {pred} <16 x i32> {}, {}", n(a), n(b)))
    }

    fn neg(&mut self, a: Num) -> Num {
        self.ibin("sub", Num::I32(0), a)
    }

    /// `vpabsd`: `abs(MIN)` is `MIN`.
    fn abs(&mut self, a: Num) -> Num {
        Num::Reg(ins!(self, "call <16 x i32> @llvm.abs.v16i32(<16 x i32> {}, i1 false)", n(a)))
    }

    fn flr(&mut self, a: Num) -> Num {
        self.ibin("and", a, Num::I32(FLR_MASK))
    }

    /// 16.16 multiply: the 64-bit product's bits 16..48.
    fn mul(&mut self, a: Num, b: Num) -> Num {
        let wa = ins!(self, "sext <16 x i32> {} to <16 x i64>", n(a));
        let wb = ins!(self, "sext <16 x i32> {} to <16 x i64>", n(b));
        let p = ins!(self, "mul <16 x i64> %v{wa}, %v{wb}");
        let s = ins!(self, "ashr <16 x i64> %v{p}, splat (i64 16)");
        Num::Reg(ins!(self, "trunc <16 x i64> %v{s} to <16 x i32>"))
    }

    /// PICO-8 `x / 2^s` (`codegen::div_pow2`): the arithmetic shift of `x`
    /// biased by `2^s - 1` where `x` is negative.
    fn div_pow2(&mut self, x: Num, s: u8) -> Num {
        if s == 0 {
            return x;
        }
        let sign = self.ibin("ashr", x, Num::I32(31));
        let bias = self.ibin("lshr", sign, Num::I32(32 - s as i32));
        let biased = self.ibin("add", x, bias);
        self.ibin("ashr", biased, Num::I32(s as i32))
    }

    fn pow2_divisor(&self, id: NodeId) -> Option<u8> {
        match self.g.point(id).map(Fixed::lit) {
            Some(lo) if lo > 0 && (lo as u32).is_power_of_two() => {
                let m = (lo as u32).trailing_zeros();
                (16..=30).contains(&m).then(|| (m - 16) as u8)
            }
            _ => None,
        }
    }

    fn pow2_modulus_mask(&self, id: NodeId) -> Option<i32> {
        self.g.point(id).map(Fixed::lit).filter(|&lo| lo > 0 && (lo as u32).is_power_of_two()).map(|lo| lo - 1)
    }

    fn pos_const_scalar(&self, id: NodeId) -> Option<i32> {
        self.g.point(id).map(Fixed::lit).filter(|&lo| lo > 0)
    }

    /// Did `a + b` (`a - b`) wrap into `r`: the sign of `(a^r)&(b^r)`
    /// (`(a^b)&(a^r)`), as a plane whose sign bit is set where it did.
    fn wrap_bits(&mut self, sub: bool, a: Num, b: Num, r: Num) -> Num {
        let (x, y) = if sub { (self.ibin("xor", a, b), self.ibin("xor", a, r)) } else { (self.ibin("xor", a, r), self.ibin("xor", b, r)) };
        self.ibin("and", x, y)
    }

    /// `Op::NoWrap` of `+`/`-` with result `r`, neither operand a literal.
    fn arith_no_wrap(&mut self, sub: bool, a: [Num; 2], b: [Num; 2], r: [Num; 2], wide: bool) -> Mask {
        let any = if wide {
            let (bl, bh) = if sub { (b[1], b[0]) } else { (b[0], b[1]) };
            let ol = self.wrap_bits(sub, a[0], bl, r[0]);
            let oh = self.wrap_bits(sub, a[1], bh, r[1]);
            self.ibin("or", ol, oh)
        } else {
            self.wrap_bits(sub, a[0], b[0], r[0])
        };
        self.cmp("sge", any, Num::I32(0))
    }

    /// `Op::NoWrap` of a one-operand operation whose operand's ends are `a`:
    /// `lo >= fits.0` and `hi <= fits.1`.
    fn one_operand_no_wrap(&mut self, a: [Num; 2], fits: (i32, i32)) -> Mask {
        let lo = if fits.0 == i32::MIN { Mask::Const(true) } else { self.cmp("sge", a[0], Num::I32(fits.0)) };
        let hi = if fits.1 == i32::MAX { Mask::Const(true) } else { self.cmp("sle", a[1], Num::I32(fits.1)) };
        self.m_and(lo, hi)
    }

    /// `zi_abs`: non-negative / non-positive / straddling.
    fn ival_abs(&mut self, a: [Num; 2]) -> [Num; 2] {
        let pos = self.cmp("sge", a[0], Num::I32(0));
        let neg = self.cmp("sle", a[1], Num::I32(0));
        let al = self.abs(a[0]);
        let ah = self.abs(a[1]);
        let mx = self.intrinsic("smax", al, ah);
        let lo = self.n_sel(neg, ah, Num::I32(0));
        let hi = self.n_sel(neg, al, mx);
        [self.n_sel(pos, a[0], lo), self.n_sel(pos, a[1], hi)]
    }

    /// `zi_fork_flr`: the `c`-th floor from the low end's, clipped, and
    /// whether the interval reaches it.
    fn fork_flr(&mut self, a: [Num; 2], c: u8) -> ([Num; 2], Mask) {
        let fl = self.flr(a[0]);
        let base = if c == 0 { fl } else { self.ibin("add", fl, Num::I32(STEP.wrapping_mul(c as i32))) };
        let top = self.ibin("add", base, Num::I32(STEP - 1));
        let hi = self.intrinsic("smin", a[1], top);
        if c == 0 {
            ([a[0], hi], Mask::Const(true))
        } else {
            let fh = self.flr(a[1]);
            let lo = self.intrinsic("smax", a[0], base);
            ([lo, hi], self.cmp("sle", base, fh))
        }
    }

    /// `zi_cmp`: tri-state; `pred` the point compare.
    fn ival_cmp(&mut self, op: &Op<Fixed>, a: [Num; 2], b: [Num; 2]) -> [Mask; 2] {
        let (t, f) = match op {
            Op::Lt => (self.cmp("slt", a[1], b[0]), self.cmp("sge", a[0], b[1])),
            Op::Le => (self.cmp("sle", a[1], b[0]), self.cmp("sgt", a[0], b[1])),
            Op::Gt => (self.cmp("sgt", a[0], b[1]), self.cmp("sle", a[1], b[0])),
            _ => (self.cmp("sge", a[0], b[1]), self.cmp("slt", a[1], b[0])),
        };
        [t, self.m_or(t, f)]
    }

    fn lower_node(&mut self, id: NodeId) -> Result<()> {
        if let Some(v) = self.lower_common(id)? {
            self.vals[id as usize] = Some(v);
            return Ok(());
        }
        let node = self.g.get(id);
        let a = node.args.clone();
        let val = match &node.op {
            op @ (Op::Add | Op::Sub | Op::Min | Op::Max) => {
                let f = |e: &mut Self, x: Num, y: Num| match op {
                    Op::Add => e.ibin("add", x, y),
                    Op::Sub => e.ibin("sub", x, y),
                    Op::Min => e.intrinsic("smin", x, y),
                    _ => e.intrinsic("smax", x, y),
                };
                if self.wide(a[0]) || self.wide(a[1]) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    // Ends cross for `-`: [x.lo - y.hi, x.hi - y.lo].
                    let (yl, yh) = if matches!(op, Op::Sub) { (y[1], y[0]) } else { (y[0], y[1]) };
                    let lo = f(self, x[0], yl);
                    Value::Ival([lo, f(self, x[1], yh)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(f(self, x, y))
                }
            }
            Op::Mul => {
                if self.wide(a[0]) || self.wide(a[1]) {
                    let (ivn, scn) = if self.wide(a[0]) { (a[0], a[1]) } else { (a[1], a[0]) };
                    if self.wide(scn) {
                        bail!("node {}: Mul of two intervals is not modelled", id);
                    }
                    if self.pos_const_scalar(scn).is_none() {
                        bail!("node {}: interval Mul by a non-positive / non-constant scalar is not modelled", id);
                    }
                    let iv = self.as_ival(ivn)?;
                    let s = self.as_num(scn)?;
                    let lo = self.mul(iv[0], s);
                    Value::Ival([lo, self.mul(iv[1], s)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Num(self.mul(x, y))
                }
            }
            Op::Div if self.wide(a[0]) => {
                if self.pos_const_scalar(a[1]).is_none() {
                    bail!("node {}: interval Div by a non-positive / non-constant scalar is not modelled", id);
                }
                let iv = self.as_ival(a[0])?;
                if let Some(sh) = self.pow2_divisor(a[1]) {
                    let lo = self.div_pow2(iv[0], sh);
                    Value::Ival([lo, self.div_pow2(iv[1], sh)])
                } else {
                    let s = self.as_num(a[1])?;
                    let Value::Num(lo) = self.call(Call::Arith2(CTX_DIV), &[iv[0], s]) else { unreachable!() };
                    let Value::Num(hi) = self.call(Call::Arith2(CTX_DIV), &[iv[1], s]) else { unreachable!() };
                    Value::Ival([lo, hi])
                }
            }
            Op::Neg => {
                if self.wide(a[0]) {
                    let iv = self.as_ival(a[0])?;
                    let lo = self.neg(iv[1]);
                    Value::Ival([lo, self.neg(iv[0])])
                } else {
                    let x = self.as_num(a[0])?;
                    Value::Num(self.neg(x))
                }
            }
            Op::Abs => {
                if self.wide(a[0]) {
                    let iv = self.as_ival(a[0])?;
                    Value::Ival(self.ival_abs(iv))
                } else {
                    let x = self.as_num(a[0])?;
                    Value::Num(self.abs(x))
                }
            }
            Op::Flr => {
                let iv = self.as_ival(a[0])?;
                Value::Num(self.flr(iv[0]))
            }
            op @ (Op::Lt | Op::Le | Op::Gt | Op::Ge) => {
                if self.wide(a[0]) || self.wide(a[1]) {
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    Value::Bool(self.ival_cmp(op, x, y))
                } else {
                    let pred = match op {
                        Op::Lt => "slt",
                        Op::Le => "sle",
                        Op::Gt => "sgt",
                        _ => "sge",
                    };
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Bool([self.cmp(pred, x, y), Mask::Const(true)])
                }
            }
            Op::Eq => {
                if self.wide(a[0]) || self.wide(a[1]) {
                    // As `Graph::compare`: decided if disjoint or both points.
                    let (x, y) = (self.as_ival(a[0])?, self.as_ival(a[1])?);
                    let xs = self.cmp("eq", x[0], x[1]);
                    let ys = self.cmp("eq", y[0], y[1]);
                    let both = self.m_and(xs, ys);
                    let lo_eq = self.cmp("eq", x[0], y[0]);
                    let val = self.m_and(both, lo_eq);
                    let d1 = self.cmp("sgt", x[0], y[1]);
                    let d2 = self.cmp("sgt", y[0], x[1]);
                    let disjoint = self.m_or(d1, d2);
                    Value::Bool([val, self.m_or(both, disjoint)])
                } else {
                    let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                    Value::Bool([self.cmp("eq", x, y), Mask::Const(true)])
                }
            }
            Op::Known => {
                let inner = a[0];
                if let Op::Flr = self.g.get(inner).op {
                    let src = self.g.get(inner).args[0];
                    if self.wide(src) {
                        let iv = self.as_ival(src)?;
                        let (fl, fh) = (self.flr(iv[0]), self.flr(iv[1]));
                        self.vals[id as usize] = Some(Value::Bool([self.cmp("eq", fl, fh), Mask::Const(true)]));
                        return Ok(());
                    }
                }
                match self.g.ty(inner) {
                    Ty::Bool | Ty::UBool => unreachable!("a boolean's `Known` is `lower_common`'s"),
                    Ty::Ival => {
                        let iv = self.as_ival(inner)?;
                        Value::Bool([self.cmp("eq", iv[0], iv[1]), Mask::Const(true)])
                    }
                    Ty::Num => Value::Bool([Mask::Const(true), Mask::Const(true)]),
                }
            }
            Op::Frag(Cut::Floors | Cut::Ints, c) | Op::FragOk(Cut::Floors, c) => {
                let iv = self.as_ival(a[0])?;
                let (frag, ok) = self.fork_flr(iv, *c);
                match node.op {
                    Op::FragOk(..) => Value::Bool([ok, Mask::Const(true)]),
                    Op::Frag(Cut::Ints, _) => Value::Num(frag[0]),
                    _ => Value::Ival(frag),
                }
            }
            Op::ForkOk(Cut::Floors, ways) => {
                let iv = self.as_ival(a[0])?;
                let (fl, fh) = (self.flr(iv[0]), self.flr(iv[1]));
                let top = self.ibin("add", fl, Num::I32(STEP.wrapping_mul(*ways as i32 - 1)));
                Value::Bool([self.cmp("sle", fh, top), Mask::Const(true)])
            }
            Op::NoWrap => {
                let x = a[0];
                let xn = self.g.get(x);
                let ok = match (graph::one_operand(self.g, x), &xn.op) {
                    (_, op) if !matches!(op, Op::Add | Op::Sub | Op::Neg | Op::Abs | Op::Mul | Op::Div) => Mask::Const(true),
                    (Some(graph::OneOperand { fits: None, .. }), _) => Mask::Const(false),
                    (Some(graph::OneOperand { operand, fits: Some(fits) }), _) => {
                        let ends = self.as_ival(xn.args[operand])?;
                        self.one_operand_no_wrap(ends, fits)
                    }
                    (None, op @ (Op::Add | Op::Sub)) => {
                        let (p, q) = (xn.args[0], xn.args[1]);
                        let (ip, iq, ir) = (self.as_ival(p)?, self.as_ival(q)?, self.as_ival(x)?);
                        self.arith_no_wrap(matches!(op, Op::Sub), ip, iq, ir, self.wide(x))
                    }
                    (None, op) => bail!(
                        "node {}: NoWrap of {:?} with no literal operand (`{}`) is not modelled: no kernel computes whether it overflows",
                        id,
                        op,
                        crate::transpile::why::Explain::of(self.g).tree(x, 2)
                    ),
                };
                Value::Bool([ok, Mask::Const(true)])
            }
            Op::Div if self.pow2_divisor(a[1]).is_some() => {
                let sh = self.pow2_divisor(a[1]).expect("guarded");
                let x = self.as_num(a[0])?;
                Value::Num(self.div_pow2(x, sh))
            }
            Op::Rem if !self.wide(a[0]) && self.pow2_modulus_mask(a[1]).is_some() => {
                let mask = self.pow2_modulus_mask(a[1]).expect("guarded");
                let x = self.as_num(a[0])?;
                Value::Num(self.ibin("and", x, Num::I32(mask)))
            }
            Op::Div | Op::Rem => {
                let (x, y) = (self.as_num(a[0])?, self.as_num(a[1])?);
                self.call(Call::Arith2(if matches!(node.op, Op::Div) { CTX_DIV } else { CTX_REM }), &[x, y])
            }
            Op::Sin => {
                let x = self.as_num(a[0])?;
                self.call(Call::Arith1(CTX_SIN), &[x])
            }
            other => bail!("node {}: op {:?} is not supported by the LLVM backend", id, other),
        };
        self.vals[id as usize] = Some(val);
        Ok(())
    }
}

/// 16.16's kernel as LLVM IR (`Compiled::asm`; `build` makes its code).
pub(crate) fn compile_fixed(g: &Graph<Fixed>, roots: &[NodeId], sym: &str, _provenance: bool) -> Result<Compiled> {
    compile_with(g, roots, sym, false, |e, id| e.lower_node(id))
}

/// The doubles' kernel, its body over two halves of eight lanes.
pub(crate) fn compile_float(g: &Graph<Float>, roots: &[NodeId], sym: &str, _provenance: bool) -> Result<Compiled> {
    compile_with(g, roots, sym, true, |e, id| e.lower_node_f64(id))
}

fn compile_with<A: LlArith>(g: &Graph<A>, roots: &[NodeId], sym: &str, halves: bool, mut lower: impl FnMut(&mut Emit<'_, A>, NodeId) -> Result<()>) -> Result<Compiled> {
    let live = g.reachable(roots);
    let (cells, input_types, input_offsets, input_bytes) = input_layout(g, &live);
    let cell_off: HashMap<u32, u32> = cells.iter().zip(&input_offsets).map(|(c, o)| (*c, *o)).collect();
    let mut e = Emit { g, vals: vec![None; g.len()], body: String::new(), next: 0, cell_off, has_calls: false };
    // Each root's store goes right after its node (spliced in below, once the
    // roots' types give the output layout), in `codegen`'s batched order.
    let mut root_at: HashMap<NodeId, Vec<usize>> = HashMap::new();
    for (ri, r) in roots.iter().enumerate() {
        root_at.entry(*r).or_default().push(ri);
    }
    let mut marks: Vec<(usize, usize)> = Vec::with_capacity(roots.len());
    for id in schedule(g, &live, roots, 4) {
        lower(&mut e, id)?;
        for &ri in root_at.get(&id).map(Vec::as_slice).unwrap_or(&[]) {
            marks.push((e.body.len(), ri));
        }
    }
    let root_kinds: Vec<Ty> = roots
        .iter()
        .map(|r| match e.vals[*r as usize] {
            Some(Value::Num(_)) => Ok(Ty::Num),
            Some(Value::Ival(_)) => Ok(Ty::Ival),
            Some(Value::Bool(_)) => Ok(Ty::Bool),
            None => bail!("root {} was never lowered", r),
        })
        .collect::<Result<_>>()?;
    let (root_offsets, out_bytes) = output_layout(&root_kinds);
    let mut body = String::with_capacity(e.body.len() + 128 * roots.len());
    let (mut from, mut s) = (0usize, 0u32);
    for (pos, ri) in marks {
        body.push_str(&e.body[from..pos]);
        from = pos;
        let off = root_offsets[ri];
        let planes: Vec<(String, String, u32)> = match e.vals[roots[ri] as usize].expect("every root lowered") {
            Value::Num(v) => vec![(A::NUM.to_string(), n(v), off)],
            Value::Ival([lo, hi]) => vec![(A::NUM.to_string(), n(lo), off), (A::NUM.to_string(), n(hi), off + 64)],
            Value::Bool(planes) => planes
                .iter()
                .enumerate()
                .map(|(k, p)| {
                    let bits = match p {
                        Mask::Const(b) => format!("{}", -(*b as i32)),
                        Mask::Reg(r) => {
                            let _ = writeln!(body, "  %s{s} = bitcast {} %v{r} to {}", A::MASK, A::MASK_INT);
                            s += 1;
                            format!("%s{}", s - 1)
                        }
                    };
                    (A::MASK_INT.to_string(), bits, off + 2 * k as u32)
                })
                .collect(),
        };
        for (ty, v, off) in planes {
            let _ = writeln!(body, "  %s{s} = getelementptr inbounds i8, ptr %outp, i64 {off}\n  store {ty} {v}, ptr %s{s}, align 1");
            s += 1;
        }
    }
    body.push_str(&e.body[from..]);
    let ll = module(sym, &body, e.has_calls, A::NUM, halves.then_some((input_bytes, out_bytes)));
    Ok(Compiled {
        text_digest: text_digest(&ll),
        asm: ll,
        input_cells: cells,
        input_offsets,
        input_types,
        input_bytes,
        n_roots: roots.len(),
        root_offsets,
        out_bytes,
        root_kinds,
        sym: sym.to_string(),
        spill_slots: 0,
        frame_bytes: 0,
        prov: Vec::new(),
        halves,
    })
}

/// The module: the kernel `sym(in, out, ctx)` around `body` (with call-outs,
/// its argument and result buffers; with `halves`, the loop over the two
/// halves, each buffer's size its step).
fn module(sym: &str, body: &str, calls: bool, num: &str, halves: Option<(u32, u32)>) -> String {
    let mut ll = String::with_capacity(body.len() + 1024);
    let _ = writeln!(ll, "target triple = \"x86_64-unknown-linux-gnu\"\n");
    let (inp, outp) = if halves.is_some() { ("%in0", "%out0") } else { ("%inp", "%outp") };
    let _ = writeln!(ll, "define void @{sym}(ptr noalias readonly {inp}, ptr noalias {outp}, ptr readonly %ctx) #0 {{\nentry:");
    if calls {
        for i in 0..4 {
            let _ = writeln!(ll, "  %arg{i} = alloca {num}, align 64");
        }
        let _ = writeln!(ll, "  %res = alloca {num}, align 64");
    }
    match halves {
        None => {
            ll.push_str(body);
            ll.push_str("  ret void\n}\n");
        }
        Some((ib, ob)) => {
            let _ = writeln!(ll, "  br label %half\nhalf:");
            let _ = writeln!(ll, "  %h = phi i32 [ 0, %entry ], [ %h1, %half ]");
            let _ = writeln!(ll, "  %inp = phi ptr [ %in0, %entry ], [ %in1, %half ]");
            let _ = writeln!(ll, "  %outp = phi ptr [ %out0, %entry ], [ %out1, %half ]");
            ll.push_str(body);
            let _ = writeln!(ll, "  %in1 = getelementptr inbounds i8, ptr %inp, i64 {ib}");
            let _ = writeln!(ll, "  %out1 = getelementptr inbounds i8, ptr %outp, i64 {ob}");
            let _ = writeln!(ll, "  %h1 = add i32 %h, 1\n  %again = icmp ult i32 %h1, 2\n  br i1 %again, label %half, label %done\ndone:\n  ret void\n}}");
        }
    }
    let _ = writeln!(ll, "\nattributes #0 = {{ nounwind \"min-legal-vector-width\"=\"512\" \"frame-pointer\"=\"none\" }}\n!0 = !{{}}");
    ll
}

/// Build `ll` (the kernel `sym`) into a shared object: `llc` (after `opt`
/// where the pipeline has passes; its text scanned for a fused
/// multiply-add or an MXCSR write), GNU `as`, `ld -shared` (no C runtime: a
/// kernel calls only through `AsmCtx`; TEMPORARY `LLVM_PROTO_LINK=gcc`: the
/// gcc driver, as `jit::assemble`). Returns the `.so`, the stack frame's
/// bytes (the caller's stack check: `llc`'s frame, its realignment and its
/// pushes) and the 64-byte spill slots `llc` reports.
pub(super) fn build(ll: &str, sym: &str, times: &mut super::jit::ToolTimes) -> Result<(PathBuf, u32, usize)> {
    use std::process::Command;
    let dir = super::jit::scratch_dir();
    let stem = super::jit::unique_stem(sym);
    let [src, bc, asm, obj, so] = ["ll", "bc", "s", "o", "so"].map(|e| dir.join(format!("{stem}.{e}")));
    std::fs::write(&src, ll)?;
    let (passes, llc) = pipeline();
    let input = match passes {
        Some(p) => {
            super::jit::run_tool("opt", Command::new("opt").arg(format!("-passes={p}")).arg("-o").arg(&bc).arg(&src), times)?;
            &bc
        }
        None => &src,
    };
    super::jit::run_tool("llc", Command::new("llc").args(llc).arg("-o").arg(&asm).arg(input), times)?;
    let text = std::fs::read_to_string(&asm)?;
    super::jit::refuse_forbidden(&text)?;
    let (frame, spills) = frame_of(&text);
    if std::env::var("LLVM_PROTO_LINK").is_ok_and(|v| v == "gcc") {
        super::jit::run_tool("gcc", Command::new("gcc").args(["-x", "assembler", "-shared", "-fPIC", "-o"]).arg(&so).arg(&asm), times)?;
    } else {
        super::jit::run_tool("as", Command::new("as").arg("-o").arg(&obj).arg(&asm), times)?;
        super::jit::run_tool("ld", Command::new("ld").args(["-shared", "-o"]).arg(&so).arg(&obj), times)?;
    }
    super::jit::keep(&[&src, &asm]);
    for f in [&src, &bc, &asm, &obj] {
        let _ = std::fs::remove_file(f);
    }
    Ok((so, frame, spills))
}

/// `llc`'s stack frame: the `subq $N, %rsp` of its prologue, plus a
/// realignment's 64 bytes and the pushes; and its 64-byte spill slots
/// (`# 64-byte Spill` lines' distinct offsets).
fn frame_of(text: &str) -> (u32, usize) {
    let mut frame = 64 + 8 * 8;
    let mut slots = std::collections::BTreeSet::new();
    for line in text.lines() {
        let t = line.trim();
        if let Some(rest) = t.strip_prefix("subq\t$") {
            if let Some((v, reg)) = rest.split_once(", ") {
                if reg.starts_with("%rsp") {
                    frame += v.trim().parse::<u32>().unwrap_or(0);
                }
            }
        }
        if t.contains("64-byte Spill") {
            if let Some(slot) = t.split(", ").nth(1).and_then(|s| s.split_whitespace().next()) {
                slots.insert(slot.to_string());
            }
        }
    }
    (frame, slots.len())
}
