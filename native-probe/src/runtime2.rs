//! Columnar abstract runtime (plans/columnar-engine.md).
//!
//! The SAME generated program (gen.rs, via the `Engine` trait) runs over a
//! BLOCK of abstract lanes: heap structure is shared (uniform across lanes
//! by the shape premise), values are per-lane columns, and the
//! lane-multiplying instructions (`expand`, `__split_by_flr`, `__split_at`)
//! append lanes to the block mid-flight instead of forking control flow -
//! which the zero-divergence census makes legal.
//!
//! Per-lane value semantics are ports of the interpreter's scalar-state
//! arms, with file:line pointers:
//!   op.rs:397-666  (binary ops, interval compares -> tri-state)
//!   op.rs:12-49    (unary ops)
//!   flow.rs:343    (branch truthiness; UnknownBool branch = fork, which
//!                   this engine treats as a loud error - the compiled
//!                   shape program has no such branch)
//!   game_runner.rs (builtin interval arms: min/max/abs/flr/sin, splits)
//!
//! Boundary abstraction ports abstraction.rs (rem widening, dash_effect_time
//! clamp, timer pins); the mark walk runs over the shared structure heap.

use std::sync::Arc;

use celeste_rust::cart_data::CartData;
use celeste_rust::collision_cache::CollisionCache;
use celeste_rust::pico8_num::{Pico8Num, Pico8NumInterval};
use rustc_hash::FxHashMap;

use crate::runtime::{Callee, Engine, BUILTIN_NAMES};
use crate::runtime::{
    BI___ARRAY_TABLE_DROP_LAST, BI___NEW_UNKNOWN_BOOLEAN, BI___NEW_VECTOR, BI___PRINT,
    BI___SPLIT_AT, BI___SPLIT_BY_FLR, BI___WIDEN_REM, BI_ABS, BI_ADD, BI_ERROR, BI_FGET, BI_FLR,
    BI_MAX, BI_MGET, BI_MIN, BI_PRINT, BI_SIN, BI_TILE_FLAG_AT,
};

pub type P8 = Pico8Num;

/// One lane's abstract value. `Copy`, 12 bytes + tag.
/// Mirrors `interpreter::value::Value` scalar variants plus intervals.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum AV {
    Num(P8),
    /// Closed interval [low, high] (`Value::NumberInterval`).
    Ival(P8, P8),
    Bool(bool),
    UBool,
    Str(u32),
    Nil,
    Ptr(u32),
    NilPtr,
}

/// A column: one value per lane, or one value for EVERY lane. Uniform is
/// the load-bearing case - appending lanes to it is free, and ops on two
/// uniforms cost one scalar op regardless of width.
#[derive(Clone, Debug)]
pub enum Col {
    U(AV),
    V(Vec<AV>),
    /// An all-Num column stored raw: 4 B/lane, branchless op loops.
    N(Vec<P8>),
    /// An all-interval column stored raw: (low, high) pairs, 8 B/lane.
    I(Vec<(P8, P8)>),
}

impl Col {
    #[inline]
    fn at(&self, lane: usize) -> AV {
        match self {
            Col::U(a) => *a,
            Col::V(v) => v[lane],
            Col::N(v) => AV::Num(v[lane]),
            Col::I(v) => AV::Ival(v[lane].0, v[lane].1),
        }
    }
}

/// Handle into the column arena. What the generated code passes around.
#[derive(Clone, Copy, Debug)]
pub struct ColId(pub u32);

/// Shared heap structure. Value cells' per-lane contents live in
/// `Rt2::cols` at the same index; the other kinds are uniform by the
/// shape premise. Closure captures are SNAPSHOTS of the capture columns
/// (the interpreter copies capture VALUES into the closure).
#[derive(Clone, Debug)]
pub enum Cell2 {
    Val,
    Obj(Vec<(u32, u32)>),
    Arr(Vec<u32>),
    Unk,
    Clo(u32, Box<[Col]>),
    Bi(u32),
}

/// Global ids the boundary walk needs, resolved from gen.rs by the driver
/// so this module does not depend on the generated code.
pub struct BoundaryIds {
    pub g_objects: u32,
    pub g_player: u32,
    pub g_timers: Vec<u32>, // frames, seconds, minutes, deaths
    pub f_type: u32,
    pub f_rem: u32,
    pub f_spd: u32,
    pub f_x: u32,
    pub f_y: u32,
    pub f_dash_effect_time: u32,
}

pub struct Rt2 {
    pub width: usize,
    pub structure: Vec<Cell2>,
    /// Per-cell value columns, parallel to `structure` (meaningful for
    /// `Cell2::Val`).
    pub cols: Vec<Col>,
    /// Local-value arena. `None` = killed (use-after-kill panics loudly).
    /// Cleared at every frame boundary - all locals are dead there.
    pub arena: Vec<Option<Col>>,
    pub globals: Vec<u32>,
    pub strings: Vec<String>,
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
    pub prints: Vec<String>,
    // stats
    pub stat_splits: u64,
    pub stat_appended: u64,
    pub stat_arena_peak: usize,
    /// Frame-start lane each current lane derives from (splits append).
    pub origin: Vec<u32>,
    /// COW lane maps (plans/columnar-engine.md "COW/lane-indirection"):
    /// one entry per widen this frame, (width_before, map current-lane ->
    /// that width's lane space; identity on 0..width_before). A column
    /// whose len is smaller than the block width reads appended lanes
    /// through the matching map instead of being physically widened.
    history: Vec<(usize, Vec<u32>)>,
    /// Recycled column buffers (killed/overwritten varying columns).
    pool: Vec<Vec<AV>>,
    /// Per-category time census (CELESTE_OP_CENSUS=1): name -> (ns, calls,
    /// lanes). The data that decides the next optimization, op_census
    /// doctrine.
    pub census: Option<FxHashMap<&'static str, (u64, u64, u64)>>,
    /// Set by `boundary`: the canonical structure hash (the shape key).
    pub shape_hash: u64,
    /// Set by `boundary`: per-lane 128-bit canonical row keys.
    pub row_keys: Vec<(u64, u64)>,
}

pub const NONE: u32 = u32::MAX;

/// Payload thrown (panic_any) at a genuinely lane-divergent branch: the
/// per-ORIGIN truth of the condition. The driver partitions the
/// frame-start block by it and reruns both sides. Divergence WITHIN one
/// origin lane (mid-frame split alternatives disagreeing at a branch)
/// cannot be partitioned this way and panics hard - none is expected:
/// every known divergent gate reads frame-start data.
pub struct SplitReq {
    pub origin_truth: Vec<bool>,
    pub site: u32,
}

fn num(v: AV) -> P8 {
    match v {
        AV::Num(n) => n,
        other => panic!("expected a number, got {:?}", other),
    }
}

fn iv_of(v: AV) -> Pico8NumInterval {
    match v {
        AV::Num(n) => Pico8NumInterval::from_number(n),
        AV::Ival(a, b) => Pico8NumInterval::new(a, b),
        other => panic!("expected number or interval, got {:?}", other),
    }
}

fn av_of_iv(iv: Pico8NumInterval) -> AV {
    AV::Ival(iv.low, iv.high)
}

/// V -> N / I when every lane is a plain number / interval.
fn compress_num_v(vs: Vec<AV>) -> Col {
    if vs.iter().all(|v| matches!(v, AV::Num(_))) {
        Col::N(
            vs.iter()
                .map(|v| match v {
                    AV::Num(n) => *n,
                    _ => unreachable!(),
                })
                .collect(),
        )
    } else if vs.iter().all(|v| matches!(v, AV::Ival(..))) {
        Col::I(
            vs.iter()
                .map(|v| match v {
                    AV::Ival(a, b) => (*a, *b),
                    _ => unreachable!(),
                })
                .collect(),
        )
    } else {
        Col::V(vs)
    }
}

fn compress_num(c: Col) -> Col {
    match c {
        Col::V(vs) => compress_num_v(vs),
        other => other,
    }
}

// ---- per-lane op ports ----

/// Plus/Minus with the Number/Interval lift (op.rs:406-427, 478-505).
fn av_addsub(l: AV, r: AV, sub: bool) -> AV {
    match (l, r) {
        (AV::Num(a), AV::Num(b)) => AV::Num(if sub { a - b } else { a + b }),
        (AV::Num(_) | AV::Ival(..), AV::Num(_) | AV::Ival(..)) => {
            let (a, b) = (iv_of(l), iv_of(r));
            av_of_iv(if sub { a - b } else { a + b })
        }
        other => panic!("+/- on {:?}", other),
    }
}

/// Star (op.rs:484, 506-516): Number*Number, or Interval * positive scalar.
fn av_mul(l: AV, r: AV) -> AV {
    match (l, r) {
        (AV::Num(a), AV::Num(b)) => AV::Num(a * b),
        (AV::Ival(..), AV::Num(b)) if b > P8::from_i16(0) => {
            av_of_iv(iv_of(l).scale_positive(b))
        }
        other => panic!("* on {:?} (no interpreter arm)", other),
    }
}

/// Slash (op.rs:487, 517-521).
fn av_div(l: AV, r: AV) -> AV {
    match (l, r) {
        (AV::Num(a), AV::Num(b)) => AV::Num(a / b),
        (AV::Ival(..), AV::Num(b)) if b > P8::from_i16(0) => {
            av_of_iv(iv_of(l).div_positive(b))
        }
        other => panic!("/ on {:?} (no interpreter arm)", other),
    }
}

/// Percent (op.rs:490): numbers only; PICO-8 `%` is total.
fn av_rem(l: AV, r: AV) -> AV {
    AV::Num(num(l) % num(r))
}

/// `==` (op.rs:432-467). Intervals can never be equal to anything;
/// UnknownBool against a bool stays unknown.
fn av_eq(l: AV, r: AV, strings: &[String]) -> AV {
    let b = match (l, r) {
        (AV::Num(a), AV::Num(b)) => a == b,
        (AV::Num(_), _) => false,
        (AV::Ival(..), _) => false,
        (AV::Bool(a), AV::Bool(b)) => a == b,
        (AV::Bool(_), AV::UBool) => return AV::UBool,
        (AV::Bool(_), _) => false,
        (AV::UBool, AV::UBool | AV::Bool(_)) => return AV::UBool,
        (AV::UBool, _) => false,
        (AV::Str(a), AV::Str(b)) => a == b || strings[a as usize] == strings[b as usize],
        (AV::Str(_), _) => false,
        (AV::Nil, AV::Nil) => true,
        (AV::Nil, _) => false,
        (AV::Ptr(a), AV::Ptr(b)) => a == b,
        (AV::Ptr(_), _) => false,
        (AV::NilPtr, _) => panic!("== on a nil pointer"),
    };
    AV::Bool(b)
}

fn av_not(v: AV) -> AV {
    // interpret_not (op.rs:12): bools only.
    match v {
        AV::Bool(b) => AV::Bool(!b),
        AV::UBool => AV::UBool,
        other => panic!("not: unsupported value {:?}", other),
    }
}

#[derive(Clone, Copy)]
enum CmpOp {
    Lt,
    Le,
    Gt,
    Ge,
}

/// Ordered compares with the tri-state interval judge (op.rs:523-649).
/// A straddling lane yields UnknownBool - exactly the scalar-state
/// interpreter's answer (a width-1 MaybeBool collapses to UnknownBool).
fn av_cmp(op: CmpOp, l: AV, r: AV) -> AV {
    match (l, r) {
        (AV::Num(a), AV::Num(b)) => AV::Bool(match op {
            CmpOp::Lt => a < b,
            CmpOp::Le => a <= b,
            CmpOp::Gt => a > b,
            CmpOp::Ge => a >= b,
        }),
        (AV::Num(_) | AV::Ival(..), AV::Num(_) | AV::Ival(..)) => {
            let (a, b) = (iv_of(l), iv_of(r));
            // op.rs judge: definite where the intervals are disjoint on the
            // deciding side, None (=> UnknownBool) on a straddle.
            let t = match op {
                CmpOp::Lt => {
                    if a.high < b.low {
                        Some(true)
                    } else if a.low >= b.high {
                        Some(false)
                    } else {
                        None
                    }
                }
                CmpOp::Le => {
                    if a.high <= b.low {
                        Some(true)
                    } else if a.low > b.high {
                        Some(false)
                    } else {
                        None
                    }
                }
                CmpOp::Gt => {
                    if a.low > b.high {
                        Some(true)
                    } else if a.high <= b.low {
                        Some(false)
                    } else {
                        None
                    }
                }
                CmpOp::Ge => {
                    if a.low >= b.high {
                        Some(true)
                    } else if a.high < b.low {
                        Some(false)
                    } else {
                        None
                    }
                }
            };
            match t {
                Some(b) => AV::Bool(b),
                None => AV::UBool,
            }
        }
        other => panic!("ordered compare on {:?}", other),
    }
}

fn av_neg(v: AV) -> AV {
    match v {
        AV::Num(n) => AV::Num(-n),
        AV::Ival(a, b) => AV::Ival(-b, -a),
        other => panic!("unary minus on {:?}", other),
    }
}

/// min/max interval extension is exact - monotone in both args
/// (game_runner.rs builtin_min/builtin_max).
fn av_min(l: AV, r: AV) -> AV {
    match (l, r) {
        (AV::Num(a), AV::Num(b)) => AV::Num(a.min(b)),
        _ => {
            let (a, b) = (iv_of(l), iv_of(r));
            av_of_iv(Pico8NumInterval::new(a.low.min(b.low), a.high.min(b.high)))
        }
    }
}

fn av_max(l: AV, r: AV) -> AV {
    match (l, r) {
        (AV::Num(a), AV::Num(b)) => AV::Num(a.max(b)),
        _ => {
            let (a, b) = (iv_of(l), iv_of(r));
            av_of_iv(Pico8NumInterval::new(a.low.max(b.low), a.high.max(b.high)))
        }
    }
}

/// game_runner.rs interval_abs.
fn av_abs(v: AV) -> AV {
    match v {
        AV::Num(n) => AV::Num(n.abs()),
        AV::Ival(..) => {
            let iv = iv_of(v);
            let zero = P8::from_i16(0);
            av_of_iv(if iv.low >= zero {
                iv
            } else if iv.high <= zero {
                Pico8NumInterval::new(iv.high.abs(), iv.low.abs())
            } else {
                Pico8NumInterval::new(zero, iv.low.abs().max(iv.high.abs()))
            })
        }
        other => panic!("abs on {:?}", other),
    }
}

/// game_runner.rs builtin_flr: an interval must sit on one floor (the
/// __split_by_flr rewrites are what guarantee it).
fn av_flr(v: AV) -> AV {
    match v {
        AV::Num(n) => AV::Num(n.flr()),
        AV::Ival(a, b) => {
            assert!(
                a.flr() == b.flr(),
                "flr of interval [{:?}, {:?}] spans multiple floors - __split_by_flr missing",
                a,
                b
            );
            AV::Num(a.flr())
        }
        other => panic!("flr on {:?}", other),
    }
}

/// game_runner.rs builtin_sin: interval -> the full band [-1, 1].
fn av_sin(v: AV) -> AV {
    match v {
        AV::Num(n) => AV::Num(n.pico8_sin()),
        AV::Ival(..) => AV::Ival(P8::from_i16(-1), P8::from_i16(1)),
        other => panic!("sin on {:?}", other),
    }
}

/// Branch truthiness (flow.rs:343-394) for one lane. UnknownBool would
/// mean a control fork - the compiled shape program has none (0 divergent
/// census); reaching it is a domain exit, loud.
fn av_truthy(v: AV) -> bool {
    match v {
        AV::Bool(b) => b,
        AV::Nil => false,
        AV::Num(_) | AV::Ival(..) | AV::Str(_) | AV::Ptr(_) => true,
        AV::NilPtr => panic!("branch on a nil pointer"),
        AV::UBool => panic!("branch on UnknownBool - control fork; deopt needed"),
    }
}

/// split_interval_by_floor (game_runner.rs:213).
fn split_iv_by_floor(iv: Pico8NumInterval) -> Vec<Pico8NumInterval> {
    let one = P8::from_i16(1);
    let mut results = Vec::new();
    let mut current = iv.low;
    while current <= iv.high {
        let next_floor = current.flr() + one;
        let largest_same = next_floor.next_smallest();
        results.push(Pico8NumInterval::new(current, iv.high.min(largest_same)));
        current = next_floor;
    }
    results
}

impl Rt2 {
    /// Convert the scalar runtime's concrete heap into a width-1 block.
    pub fn from_scalar(rt: &crate::runtime::Rt) -> Rt2 {
        use crate::runtime::{Cell, V};
        fn av_of(v: V) -> AV {
            match v {
                V::Num(n) => AV::Num(n),
                V::Bool(b) => AV::Bool(b),
                V::UBool => AV::UBool,
                V::Str(s) => AV::Str(s),
                V::Nil => AV::Nil,
                V::Ptr(p) => AV::Ptr(p),
                V::NilPtr => AV::NilPtr,
            }
        }
        let mut structure = Vec::with_capacity(rt.heap.len());
        let mut cols = Vec::with_capacity(rt.heap.len());
        for cell in &rt.heap {
            let (s, c) = match cell {
                Cell::Val(v) => (Cell2::Val, Col::U(av_of(*v))),
                Cell::Obj(fields) => (Cell2::Obj(fields.clone()), Col::U(AV::Nil)),
                Cell::Arr(items) => (Cell2::Arr(items.clone()), Col::U(AV::Nil)),
                Cell::Unk => (Cell2::Unk, Col::U(AV::Nil)),
                Cell::Clo(f, caps) => (
                    Cell2::Clo(*f, caps.iter().map(|v| Col::U(av_of(*v))).collect()),
                    Col::U(AV::Nil),
                ),
                Cell::Bi(b) => (Cell2::Bi(*b), Col::U(AV::Nil)),
            };
            structure.push(s);
            cols.push(c);
        }
        Rt2 {
            width: 1,
            structure,
            cols,
            arena: Vec::new(),
            globals: rt.globals.clone(),
            strings: rt.strings.clone(),
            cart: rt.cart.clone(),
            cache: rt.cache.clone(),
            prints: rt.prints.clone(),
            stat_splits: 0,
            stat_appended: 0,
            stat_arena_peak: 0,
            pool: Vec::new(),
            census: std::env::var_os("CELESTE_OP_CENSUS").map(|_| FxHashMap::default()),
            origin: Vec::new(),
            history: Vec::new(),
            shape_hash: 0,
            row_keys: Vec::new(),
        }
    }

    #[inline]
    fn put(&mut self, c: Col) -> ColId {
        let id = self.arena.len() as u32;
        self.arena.push(Some(c));
        ColId(id)
    }

    #[inline]
    fn get(&self, id: ColId) -> &Col {
        self.arena[id.0 as usize]
            .as_ref()
            .unwrap_or_else(|| panic!("use of killed column %{}", id.0))
    }

    /// The unique pointer a column denotes. Receivers/targets are uniform
    /// by the shape premise; a varying-but-equal column is accepted (a
    /// select of the same pointer down both arms).
    fn uptr(&self, id: ColId) -> u32 {
        match self.get(id) {
            Col::U(AV::Ptr(p)) => *p,
            Col::U(other) => panic!("expected a uniform pointer, got {:?}", other),
            Col::V(v) => {
                let AV::Ptr(p) = v[0] else {
                    panic!("expected a pointer, got {:?}", v[0])
                };
                // Stale lanes are copies of physical lanes, so scanning the
                // physical data covers every lane.
                assert!(
                    v.iter().all(|a| matches!(a, AV::Ptr(q) if *q == p)),
                    "lane-varying pointer column - shape premise broken"
                );
                p
            }
            Col::N(_) | Col::I(_) => panic!("expected a pointer column, got numbers"),
        }
    }

    /// Numeric binary fast path: when both operands are all-number
    /// columns (N or uniform Num), run a branchless P8 loop into a raw
    /// N column. Returns None when either side is not purely numeric.
    #[inline]
    fn bin_num(&mut self, l: ColId, r: ColId, f: impl Fn(P8, P8) -> P8) -> Option<ColId> {
        self.resolve(l);
        self.resolve(r);
        enum NV<'a> {
            U(P8),
            N(&'a [P8]),
        }
        fn view(c: &Col) -> Option<NV<'_>> {
            match c {
                Col::U(AV::Num(n)) => Some(NV::U(*n)),
                Col::N(v) => Some(NV::N(v)),
                _ => None,
            }
        }
        let out: Vec<P8> = match (view(self.get(l))?, view(self.get(r))?) {
            (NV::U(a), NV::U(b)) => {
                let v = f(a, b);
                return Some(self.put(Col::U(AV::Num(v))));
            }
            (NV::N(a), NV::N(b)) => a.iter().zip(b).map(|(x, y)| f(*x, *y)).collect(),
            (NV::N(a), NV::U(b)) => a.iter().map(|x| f(*x, b)).collect(),
            (NV::U(a), NV::N(b)) => b.iter().map(|y| f(a, *y)).collect(),
        };
        Some(self.put(Col::N(out)))
    }

    /// Interval add/sub fast path: both operands numeric-or-interval typed
    /// columns, endpoint-wise loop into a raw I column (op.rs interval
    /// Plus/Minus with the Number lift).
    #[inline]
    fn bin_iv(&mut self, l: ColId, r: ColId, sub: bool) -> Option<ColId> {
        self.resolve(l);
        self.resolve(r);
        enum IV<'a> {
            U(P8, P8),
            N(&'a [P8]),
            I(&'a [(P8, P8)]),
        }
        fn view(c: &Col) -> Option<IV<'_>> {
            match c {
                Col::U(AV::Num(n)) => Some(IV::U(*n, *n)),
                Col::U(AV::Ival(a, b)) => Some(IV::U(*a, *b)),
                Col::N(v) => Some(IV::N(v)),
                Col::I(v) => Some(IV::I(v)),
                _ => None,
            }
        }
        #[inline(always)]
        fn get(v: &IV, i: usize) -> (P8, P8) {
            match v {
                IV::U(a, b) => (*a, *b),
                IV::N(v) => (v[i], v[i]),
                IV::I(v) => v[i],
            }
        }
        let (lv, rv) = (view(self.get(l))?, view(self.get(r))?);
        // Both uniform or both plain-number columns are handled elsewhere;
        // here at least one side is interval-typed.
        let any_interval = matches!(lv, IV::U(a, b) if a != b)
            || matches!(rv, IV::U(a, b) if a != b)
            || matches!(lv, IV::I(_))
            || matches!(rv, IV::I(_));
        if !any_interval {
            return None;
        }
        let w = self.width;
        let op = |x: (P8, P8), y: (P8, P8)| -> (P8, P8) {
            let (a, b) = (
                Pico8NumInterval::new(x.0, x.1),
                Pico8NumInterval::new(y.0, y.1),
            );
            let r = if sub { a - b } else { a + b };
            (r.low, r.high)
        };
        if let (IV::U(a, b), IV::U(c, d)) = (&lv, &rv) {
            let r = op((*a, *b), (*c, *d));
            return Some(self.put(Col::U(AV::Ival(r.0, r.1))));
        }
        let out: Vec<(P8, P8)> = (0..w).map(|i| op(get(&lv, i), get(&rv, i))).collect();
        Some(self.put(Col::I(out)))
    }

    /// Numeric compare fast path: bool column out.
    #[inline]
    fn cmp_num(&mut self, l: ColId, r: ColId, f: impl Fn(P8, P8) -> bool) -> Option<ColId> {
        self.resolve(l);
        self.resolve(r);
        enum NV<'a> {
            U(P8),
            N(&'a [P8]),
        }
        fn view(c: &Col) -> Option<NV<'_>> {
            match c {
                Col::U(AV::Num(n)) => Some(NV::U(*n)),
                Col::N(v) => Some(NV::N(v)),
                _ => None,
            }
        }
        let out: Vec<AV> = match (view(self.get(l))?, view(self.get(r))?) {
            (NV::U(a), NV::U(b)) => {
                let v = f(a, b);
                return Some(self.put(Col::U(AV::Bool(v))));
            }
            (NV::N(a), NV::N(b)) => {
                a.iter().zip(b).map(|(x, y)| AV::Bool(f(*x, *y))).collect()
            }
            (NV::N(a), NV::U(b)) => a.iter().map(|x| AV::Bool(f(*x, b))).collect(),
            (NV::U(a), NV::N(b)) => b.iter().map(|y| AV::Bool(f(a, *y))).collect(),
        };
        Some(self.put(Col::V(out)))
    }

    #[inline]
    fn rec(&mut self, name: &'static str, t0: Option<std::time::Instant>) {
        if let (Some(census), Some(t0)) = (self.census.as_mut(), t0) {
            let e = census.entry(name).or_insert((0, 0, 0));
            e.0 += t0.elapsed().as_nanos() as u64;
            e.1 += 1;
        }
    }

    #[inline]
    fn t0(&self) -> Option<std::time::Instant> {
        if self.census.is_some() {
            Some(std::time::Instant::now())
        } else {
            None
        }
    }

    /// Merge a sub-block's census into an accumulator map.
    pub fn drain_census(&mut self, into: &mut FxHashMap<&'static str, (u64, u64, u64)>) {
        if let Some(census) = self.census.take() {
            for (k, v) in census {
                let e = into.entry(k).or_insert((0, 0, 0));
                e.0 += v.0;
                e.1 += v.1;
                e.2 += v.2;
            }
            self.census = Some(FxHashMap::default());
        }
    }

    /// Length of a column's physical data (width means "not stale").
    #[inline]
    fn col_len(c: &Col) -> usize {
        match c {
            Col::U(_) => usize::MAX,
            Col::V(v) => v.len(),
            Col::N(v) => v.len(),
            Col::I(v) => v.len(),
        }
    }

    /// The physical index a (possibly stale) column uses for `lane`:
    /// direct below its length, through the COW map above it.
    #[inline]
    fn phys(&self, len: usize, lane: usize) -> usize {
        if lane < len {
            return lane;
        }
        let (_, map) = self
            .history
            .iter()
            .find(|(w, _)| *w == len)
            .unwrap_or_else(|| panic!("stale column of len {} has no COW map", len));
        map[lane] as usize
    }

    /// Lane read through the COW maps.
    #[inline]
    fn av_at(&self, c: &Col, lane: usize) -> AV {
        match c {
            Col::U(a) => *a,
            Col::V(v) => v[self.phys(v.len(), lane)],
            Col::N(v) => AV::Num(v[self.phys(v.len(), lane)]),
            Col::I(v) => {
                let (a, b) = v[self.phys(v.len(), lane)];
                AV::Ival(a, b)
            }
        }
    }

    /// Physically widen a stale column to the block width.
    fn materialize_col(&self, c: &Col) -> Option<Col> {
        let len = Self::col_len(c);
        if len >= self.width {
            return None;
        }
        Some(match c {
            Col::U(_) => unreachable!(),
            Col::V(v) => {
                let mut nv = Vec::with_capacity(self.width);
                nv.extend_from_slice(v);
                nv.extend((len..self.width).map(|i| v[self.phys(len, i)]));
                Col::V(nv)
            }
            Col::N(v) => {
                let mut nv = Vec::with_capacity(self.width);
                nv.extend_from_slice(v);
                nv.extend((len..self.width).map(|i| v[self.phys(len, i)]));
                Col::N(nv)
            }
            Col::I(v) => {
                let mut nv = Vec::with_capacity(self.width);
                nv.extend_from_slice(v);
                nv.extend((len..self.width).map(|i| v[self.phys(len, i)]));
                Col::I(nv)
            }
        })
    }

    /// Materialize an arena column in place and return a reference.
    fn resolve(&mut self, id: ColId) -> &Col {
        if let Some(m) = self.materialize_col(self.get(id)) {
            self.arena[id.0 as usize] = Some(m);
        }
        self.get(id)
    }

    /// Materialize a heap column in place.
    fn resolve_cell(&mut self, p: usize) {
        if let Some(m) = self.materialize_col(&self.cols[p]) {
            self.cols[p] = m;
        }
    }

    /// A cleared buffer with capacity for the current width (recycled).
    #[inline]
    fn buf(&mut self) -> Vec<AV> {
        match self.pool.pop() {
            Some(mut v) => {
                v.clear();
                v
            }
            None => Vec::with_capacity(self.width + self.width / 2),
        }
    }

    #[inline]
    fn map1(&mut self, a: ColId, f: impl Fn(AV) -> AV) -> ColId {
        let t = self.t0();
        let r = self.map1_inner(a, f);
        self.rec("map1_generic", t);
        r
    }

    #[inline]
    fn map1_inner(&mut self, a: ColId, f: impl Fn(AV) -> AV) -> ColId {
        self.resolve(a);
        match self.get(a) {
            Col::U(x) => {
                let out = Col::U(f(*x));
                self.put(out)
            }
            Col::V(_) => {
                let mut out = self.buf();
                let Col::V(v) = self.get(a) else { unreachable!() };
                out.extend(v.iter().map(|x| f(*x)));
                self.put(Col::V(out))
            }
            Col::N(_) => {
                let mut out = self.buf();
                let Col::N(v) = self.get(a) else { unreachable!() };
                out.extend(v.iter().map(|x| f(AV::Num(*x))));
                self.put(compress_num(Col::V(out)))
            }
            Col::I(_) => {
                let mut out = self.buf();
                let Col::I(v) = self.get(a) else { unreachable!() };
                out.extend(v.iter().map(|x| f(AV::Ival(x.0, x.1))));
                self.put(compress_num(Col::V(out)))
            }
        }
    }

    #[inline]
    fn map2(&mut self, a: ColId, b: ColId, f: impl Fn(AV, AV) -> AV) -> ColId {
        let t = self.t0();
        let r = self.map2_inner(a, b, f);
        self.rec("map2_generic", t);
        r
    }

    #[inline]
    fn map2_inner(&mut self, a: ColId, b: ColId, f: impl Fn(AV, AV) -> AV) -> ColId {
        if let (Col::U(x), Col::U(y)) = (self.get(a), self.get(b)) {
            let out = Col::U(f(*x, *y));
            return self.put(out);
        }
        let mut out = self.buf();
        let w = self.width;
        out.reserve(w);
        for i in 0..w {
            let (x, y) = (self.get(a), self.get(b));
            let v = f(self.av_at_of(x, i), self.av_at_of(y, i));
            out.push(v);
        }
        self.put(Col::V(out))
    }

    /// `av_at` without holding the borrow (per-iteration re-borrow).
    #[inline]
    fn av_at_of(&self, c: &Col, lane: usize) -> AV {
        self.av_at(c, lane)
    }

    fn load_inner(&mut self, v: ColId) -> ColId {
        let p = match self.get(v) {
            Col::U(AV::Ptr(p)) => *p,
            Col::U(AV::NilPtr) => return self.put(Col::U(AV::Nil)),
            _ => self.uptr(v),
        };
        match &self.structure[p as usize] {
            Cell2::Val => {
                self.resolve_cell(p as usize);
                let c = self.cols[p as usize].clone();
                self.put(c)
            }
            _ => self.put(Col::U(AV::Ptr(p))),
        }
    }


    fn select_inner(&mut self, c: ColId, t: ColId, f: ColId) -> ColId {
        let pick = |cv: AV, tv: AV, fv: AV| -> AV {
            match cv {
                AV::Bool(true) => tv,
                AV::Bool(false) => fv,
                other => panic!("select on a non-bool condition: {:?}", other),
            }
        };
        if let (Col::U(cv), Col::U(tv), Col::U(fv)) = (self.get(c), self.get(t), self.get(f)) {
            let out = Col::U(pick(*cv, *tv, *fv));
            return self.put(out);
        }
        let mut out: Vec<AV> = Vec::with_capacity(self.width);
        for i in 0..self.width {
            let (cc, tc, fc) = (self.get(c), self.get(t), self.get(f));
            let v = pick(self.av_at(cc, i), self.av_at(tc, i), self.av_at(fc, i));
            out.push(v);
        }
        self.put(Col::V(out))
    }


    /// Append lanes: `srcs[k]` is the source lane the k-th appended lane
    /// copies. Uniform columns are untouched (appending a copy preserves
    /// uniformity); varying columns extend. Covers every live local column
    /// (the whole call stack - the arena is shared) and every heap value
    /// column.
    fn widen(&mut self, srcs: &[usize]) {
        if srcs.is_empty() {
            return;
        }
        let t = self.t0();
        self.stat_splits += 1;
        self.stat_appended += srcs.len() as u64;
        let old_w = self.width;
        // COW: no column is touched. Extend every existing map, then
        // record this width's map (identity below old_w, srcs above).
        for (_, map) in self.history.iter_mut() {
            map.reserve(srcs.len());
            for &s in srcs {
                let m = map[s];
                map.push(m);
            }
        }
        let mut map: Vec<u32> = Vec::with_capacity(old_w + srcs.len());
        map.extend(0..old_w as u32);
        map.extend(srcs.iter().map(|&s| s as u32));
        self.history.push((old_w, map));
        for &s in srcs {
            let o = self.origin[s];
            self.origin.push(o);
        }
        self.width += srcs.len();
        self.rec("widen", t);
    }

    /// Lane-multiplying op, allocation-free per lane: `alt(lane, k)` is
    /// the lane's k-th alternative (k < n_alts(lane), n_alts >= 1). Lane
    /// keeps alternative 0 in place; the rest append in (lane, alt) order.
    fn split_multi_with(
        &mut self,
        n_alts: impl Fn(usize) -> usize,
        alt: impl Fn(usize, usize) -> AV,
    ) -> ColId {
        let w = self.width;
        let mut srcs: Vec<usize> = Vec::new();
        let mut appended: Vec<AV> = Vec::new();
        for lane in 0..w {
            for k in 1..n_alts(lane) {
                srcs.push(lane);
                appended.push(alt(lane, k));
            }
        }
        if srcs.is_empty() {
            let first = alt(0, 0);
            if (1..w).all(|i| alt(i, 0) == first) {
                return self.put(Col::U(first));
            }
            let mut out = self.buf();
            out.extend((0..w).map(|i| alt(i, 0)));
            return self.put(Col::V(out));
        }
        self.widen(&srcs);
        let mut out = self.buf();
        out.extend((0..w).map(|i| alt(i, 0)));
        out.extend_from_slice(&appended);
        self.put(Col::V(out))
    }
}

impl Engine for Rt2 {
    type V = ColId;

    fn c_num(&mut self, hi: i16, lo: u16) -> ColId {
        self.put(Col::U(AV::Num(P8::from_parts(hi, lo))))
    }
    fn c_bool(&mut self, b: bool) -> ColId {
        self.put(Col::U(AV::Bool(b)))
    }
    fn c_str(&mut self, s: u32) -> ColId {
        self.put(Col::U(AV::Str(s)))
    }
    fn c_nil(&mut self) -> ColId {
        self.put(Col::U(AV::Nil))
    }

    fn alloc_nil(&mut self) -> ColId {
        let id = self.structure.len() as u32;
        self.structure.push(Cell2::Val);
        self.cols.push(Col::U(AV::Nil));
        self.put(Col::U(AV::Ptr(id)))
    }

    fn get_global(&mut self, g: u32, create: bool) -> ColId {
        let cell = self.globals[g as usize];
        if cell != NONE {
            self.put(Col::U(AV::Ptr(cell)))
        } else if create {
            let id = self.structure.len() as u32;
            self.structure.push(Cell2::Val);
            self.cols.push(Col::U(AV::Nil));
            self.globals[g as usize] = id;
            self.put(Col::U(AV::Ptr(id)))
        } else {
            self.put(Col::U(AV::NilPtr))
        }
    }

    fn load(&mut self, v: ColId) -> ColId {
        let t = self.t0();
        let r = self.load_inner(v);
        self.rec("load", t);
        r
    }

    fn store(&mut self, t: ColId, s: ColId) {
        let tm = self.t0();
        let p = self.uptr(t) as usize;
        self.structure[p] = Cell2::Val;
        self.resolve(s);
        self.cols[p] = self.get(s).clone();
        self.rec("store", tm);
    }

    fn store_empty_table(&mut self, t: ColId) {
        let p = self.uptr(t) as usize;
        self.structure[p] = Cell2::Unk;
        self.cols[p] = Col::U(AV::Nil);
    }

    fn store_closure(&mut self, t: ColId, f: u32, caps: &[ColId]) {
        let p = self.uptr(t) as usize;
        for c in caps {
            self.resolve(*c);
        }
        let caps: Box<[Col]> = caps.iter().map(|c| self.get(*c).clone()).collect();
        self.structure[p] = Cell2::Clo(f, caps);
        self.cols[p] = Col::U(AV::Nil);
    }

    fn get_field(&mut self, recv: ColId, f: u32, create: bool, _site: u32) -> ColId {
        let table = self.uptr(recv) as usize;
        let existing = match &self.structure[table] {
            Cell2::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
            Cell2::Unk => None,
            other => panic!("GetField on a non-object cell: {:?}", other),
        };
        if let Some(cell) = existing {
            self.put(Col::U(AV::Ptr(cell)))
        } else if create {
            let cell = self.structure.len() as u32;
            self.structure.push(Cell2::Val);
            self.cols.push(Col::U(AV::Nil));
            match &mut self.structure[table] {
                Cell2::Obj(fields) => fields.push((f, cell)),
                slot @ Cell2::Unk => *slot = Cell2::Obj(vec![(f, cell)]),
                _ => unreachable!(),
            }
            self.put(Col::U(AV::Ptr(cell)))
        } else {
            self.put(Col::U(AV::NilPtr))
        }
    }

    fn get_index(&mut self, recv: ColId, idx: ColId, create: bool, _site: u32) -> ColId {
        let table = self.uptr(recv) as usize;
        let index = match self.get(idx) {
            Col::U(AV::Num(n)) => n.as_i16().expect("GetIndex: non-integer index"),
            other => panic!("GetIndex: lane-varying or non-number index: {:?}", other),
        };
        assert!(index >= 1, "GetIndex: index is less than 1");
        let existing = match &self.structure[table] {
            Cell2::Arr(items) => items.get(index as usize - 1).copied(),
            Cell2::Unk => None,
            other => panic!("GetIndex on a non-array cell: {:?}", other),
        };
        if let Some(cell) = existing {
            self.put(Col::U(AV::Ptr(cell)))
        } else if create {
            let old_len = match &self.structure[table] {
                Cell2::Arr(items) => items.len(),
                Cell2::Unk => 0,
                _ => unreachable!(),
            };
            let mut tail: Vec<u32> = Vec::new();
            for _ in old_len..(index as usize) {
                let gap = self.structure.len() as u32;
                self.structure.push(Cell2::Val);
                self.cols.push(Col::U(AV::Nil));
                tail.push(gap);
            }
            let cell = *tail.last().unwrap();
            match &mut self.structure[table] {
                Cell2::Arr(items) => items.extend_from_slice(&tail),
                slot @ Cell2::Unk => *slot = Cell2::Arr(tail),
                _ => unreachable!(),
            }
            self.put(Col::U(AV::Ptr(cell)))
        } else {
            self.put(Col::U(AV::NilPtr))
        }
    }

    fn op_add(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.bin_num(l, r, |a, b| a + b) {
            return out;
        }
        if let Some(out) = self.bin_iv(l, r, false) {
            return out;
        }
        self.map2(l, r, |a, b| av_addsub(a, b, false))
    }
    fn op_sub(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.bin_num(l, r, |a, b| a - b) {
            return out;
        }
        if let Some(out) = self.bin_iv(l, r, true) {
            return out;
        }
        self.map2(l, r, |a, b| av_addsub(a, b, true))
    }
    fn op_mul(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.bin_num(l, r, |a, b| a * b) {
            return out;
        }
        self.map2(l, r, av_mul)
    }
    fn op_div(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.bin_num(l, r, |a, b| a / b) {
            return out;
        }
        self.map2(l, r, av_div)
    }
    fn op_rem(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.bin_num(l, r, |a, b| a % b) {
            return out;
        }
        self.map2(l, r, av_rem)
    }
    fn op_pow(&mut self, _l: ColId, _r: ColId) -> ColId {
        panic!("^ reached - the interpreter has no arm for it either")
    }
    fn eq(&mut self, l: ColId, r: ColId) -> ColId {
        if let (Col::U(x), Col::U(y)) = (self.get(l), self.get(r)) {
            let out = Col::U(av_eq(*x, *y, &self.strings));
            return self.put(out);
        }
        let mut out: Vec<AV> = Vec::with_capacity(self.width);
        for i in 0..self.width {
            let (x, y) = (self.get(l), self.get(r));
            let v = av_eq(self.av_at(x, i), self.av_at(y, i), &self.strings);
            out.push(v);
        }
        self.put(Col::V(out))
    }
    fn ne(&mut self, l: ColId, r: ColId) -> ColId {
        let e = self.eq(l, r);
        self.map1(e, av_not)
    }
    fn lt(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.cmp_num(l, r, |a, b| a < b) {
            return out;
        }
        self.map2(l, r, |a, b| av_cmp(CmpOp::Lt, a, b))
    }
    fn le(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.cmp_num(l, r, |a, b| a <= b) {
            return out;
        }
        self.map2(l, r, |a, b| av_cmp(CmpOp::Le, a, b))
    }
    fn gt(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.cmp_num(l, r, |a, b| a > b) {
            return out;
        }
        self.map2(l, r, |a, b| av_cmp(CmpOp::Gt, a, b))
    }
    fn ge(&mut self, l: ColId, r: ColId) -> ColId {
        if let Some(out) = self.cmp_num(l, r, |a, b| a >= b) {
            return out;
        }
        self.map2(l, r, |a, b| av_cmp(CmpOp::Ge, a, b))
    }
    fn concat(&mut self, l: ColId, r: ColId) -> ColId {
        let (a, b) = match (self.get(l), self.get(r)) {
            (Col::U(a), Col::U(b)) => (*a, *b),
            other => panic!(".. on lane-varying operands: {:?}", other),
        };
        let s = match (a, b) {
            (AV::Str(x), AV::Str(y)) => {
                format!("{}{}", self.strings[x as usize], self.strings[y as usize])
            }
            (AV::Str(x), AV::Num(n)) => format!(
                "{}{}",
                self.strings[x as usize],
                n.as_i16().expect(".. on a fractional number")
            ),
            (AV::Num(n), AV::Str(y)) => format!(
                "{}{}",
                n.as_i16().expect(".. on a fractional number"),
                self.strings[y as usize]
            ),
            other => panic!(".. on {:?}", other),
        };
        let id = self.strings.len() as u32;
        self.strings.push(s);
        self.put(Col::U(AV::Str(id)))
    }
    fn un_minus(&mut self, v: ColId) -> ColId {
        self.map1(v, av_neg)
    }
    fn un_not(&mut self, v: ColId) -> ColId {
        self.map1(v, av_not)
    }
    fn un_hash(&mut self, v: ColId) -> ColId {
        let out = match self.get(v) {
            Col::U(AV::Str(s)) => AV::Num(P8::from_i16(self.strings[*s as usize].len() as i16)),
            _ => {
                let p = self.uptr(v);
                match &self.structure[p as usize] {
                    Cell2::Arr(items) => AV::Num(P8::from_i16(items.len() as i16)),
                    Cell2::Obj(_) | Cell2::Unk => AV::Num(P8::from_i16(0)),
                    other => panic!("# on non-table pointer: {:?}", other),
                }
            }
        };
        self.put(Col::U(out))
    }

    /// Select (op.rs:158): the condition must BE a bool per lane. A
    /// select on UnknownBool is exactly what rule #96 eliminated; hitting
    /// one is a domain exit.
    fn select(&mut self, c: ColId, t: ColId, f: ColId) -> ColId {
        let tm = self.t0();
        let r = self.select_inner(c, t, f);
        self.rec("select", tm);
        r
    }

    /// Expand (core_interpreter.rs:545): a concrete bool passes through;
    /// UnknownBool forks the lane - the k=2 lane split.
    fn expand(&mut self, v: ColId) -> ColId {
        self.resolve(v);
        let d = self.get(v).clone();
        let w = self.width;
        let any_unknown = match &d {
            Col::U(a) => matches!(a, AV::UBool),
            Col::V(vs) => vs.iter().any(|a| matches!(a, AV::UBool)),
            Col::N(_) | Col::I(_) => panic!("expand on a number column"),
        };
        if !any_unknown {
            // Identity on concrete bools (validated per lane).
            match &d {
                Col::U(a) => assert!(matches!(a, AV::Bool(_)), "expand on {:?}", a),
                Col::V(vs) => {
                    for a in vs {
                        assert!(matches!(a, AV::Bool(_)), "expand on {:?}", a);
                    }
                }
                Col::N(_) | Col::I(_) => unreachable!(),
            }
            return self.put(d);
        }
        self.split_multi_with(
            |i| match d.at(i) {
                AV::Bool(_) => 1,
                AV::UBool => 2,
                other => panic!("expand on {:?}", other),
            },
            |i, k| match (d.at(i), k) {
                (AV::Bool(b), 0) => AV::Bool(b),
                (AV::UBool, 0) => AV::Bool(true),
                (AV::UBool, 1) => AV::Bool(false),
                _ => unreachable!(),
            },
        )
    }

    fn truthy_b(&mut self, v: ColId, site: u32) -> bool {
        self.resolve(v);
        match self.get(v) {
            Col::U(a) => av_truthy(*a),
            Col::N(_) | Col::I(_) => true,
            Col::V(vs) => {
                let t = av_truthy(vs[0]);
                if vs.iter().all(|a| av_truthy(*a) == t) {
                    return t;
                }
                // Genuinely divergent: reduce to per-origin truth and ask
                // the driver to partition the frame-start block.
                let n_origins = self.origin.iter().copied().max().unwrap() as usize + 1;
                let mut truth: Vec<Option<bool>> = vec![None; n_origins];
                for (i, a) in vs.iter().enumerate() {
                    let o = self.origin[i] as usize;
                    let ti = av_truthy(*a);
                    match truth[o] {
                        None => truth[o] = Some(ti),
                        Some(prev) => assert!(
                            prev == ti,
                            "branch at site {} ({}) diverges WITHIN origin lane {} - \
                             mid-frame split alternatives disagree; deopt needed",
                            site,
                            crate::gen::BRANCH_INFO[site as usize],
                            o
                        ),
                    }
                }
                std::panic::panic_any(SplitReq {
                    origin_truth: truth.into_iter().map(|t| t.unwrap_or(false)).collect(),
                    site,
                });
            }
        }
    }

    fn kill(&mut self, vs: &[ColId]) {
        for v in vs {
            if let Some(Col::V(vec)) = self.arena[v.0 as usize].take() {
                if self.pool.len() < 48 {
                    self.pool.push(vec);
                }
            }
        }
    }

    fn assert_closure(&mut self, v: ColId, f: u32, caps: &[ColId], ctx: &str) {
        let p = self.uptr(v);
        match &self.structure[p as usize] {
            Cell2::Clo(cf, cc) if *cf == f && cc.len() == caps.len() => {
                // Captured columns must equal the asserted ones. Captures in
                // the compiled program are self-pointers - uniform - so
                // uniform pointer equality is the check.
                for (stored, want) in cc.iter().zip(caps) {
                    let same = match (stored, self.arena[want.0 as usize].as_ref().unwrap()) {
                        (Col::U(a), Col::U(b)) => a == b,
                        _ => false,
                    };
                    assert!(same, "AssertClosure capture mismatch at {}", ctx);
                }
            }
            other => panic!("AssertClosure(fn {}) failed at {}: {:?}", f, ctx, other),
        }
    }
    fn assert_pointer(&mut self, v: ColId, ctx: &str) {
        match self.get(v) {
            Col::U(AV::Ptr(_)) => {}
            other => panic!("AssertPointer failed at {}: {:?}", ctx, other),
        }
    }
    fn assert_value_cell(&mut self, v: ColId, ctx: &str) {
        let p = self.uptr(v);
        match &self.structure[p as usize] {
            Cell2::Val => {}
            other => panic!("AssertValueCell failed at {}: {:?}", ctx, other),
        }
    }
    fn assert_true(&mut self, v: ColId, ctx: &str) {
        self.resolve(v);
        match self.get(v) {
            Col::U(AV::Bool(true)) => {}
            Col::V(vs) if vs.iter().all(|a| matches!(a, AV::Bool(true))) => {}
            other => panic!("AssertTrue failed at {}: {:?}", ctx, other),
        }
    }
    fn assert_builtin(&mut self, v: ColId, b: u32) {
        let p = self.uptr(v);
        match &self.structure[p as usize] {
            Cell2::Bi(x) if *x == b => {}
            other => panic!(
                "CallBuiltin({}) failed: cell holds {:?}",
                BUILTIN_NAMES[b as usize], other
            ),
        }
    }

    fn bi_min(&mut self, l: ColId, r: ColId) -> ColId {
        self.map2(l, r, av_min)
    }
    fn bi_max(&mut self, l: ColId, r: ColId) -> ColId {
        self.map2(l, r, av_max)
    }
    fn bi_abs(&mut self, v: ColId) -> ColId {
        self.map1(v, av_abs)
    }
    fn bi_flr(&mut self, v: ColId) -> ColId {
        self.map1(v, av_flr)
    }
    fn bi_sin(&mut self, v: ColId) -> ColId {
        self.map1(v, av_sin)
    }
    fn bi_mget(&mut self, x: ColId, y: ColId) -> ColId {
        let cart = self.cart.clone();
        self.map2(x, y, move |a, b| {
            AV::Num(P8::from_i16(
                cart.mget(num(a), num(b)).expect("mget failed") as i16,
            ))
        })
    }
    fn bi_tile_flag_at(&mut self, x: ColId, y: ColId, w: ColId, h: ColId, f: ColId) -> ColId {
        for a in [x, y, w, h, f] {
            self.resolve(a);
        }
        let flag = match self.get(f) {
            Col::U(AV::Num(n)) => n.as_i16().expect("tile_flag_at: flag must be integer"),
            other => panic!("tile_flag_at: lane-varying flag: {:?}", other),
        };
        if flag != 0 {
            return self.put(Col::U(AV::Bool(false)));
        }
        let wi = match self.get(w) {
            Col::U(AV::Num(n)) => n.as_i16().expect("tile_flag_at: w"),
            other => panic!("tile_flag_at: lane-varying w: {:?}", other),
        };
        let hi = match self.get(h) {
            Col::U(AV::Num(n)) => n.as_i16().expect("tile_flag_at: h"),
            other => panic!("tile_flag_at: lane-varying h: {:?}", other),
        };
        let cache = self.cache.clone();
        let cart = self.cart.clone();
        let one = |a: AV, b: AV| -> AV {
            let xi = num(a).as_i16().expect("tile_flag_at: x must be an integer");
            let yi = num(b).as_i16().expect("tile_flag_at: y must be an integer");
            if let Some((map, dx, dy)) = cache.solid_map(wi, hi) {
                if let Some(v) = map.get(xi + dx, yi + dy) {
                    return AV::Bool(v);
                }
            }
            AV::Bool(cache.solid_at(&cart, xi, yi, wi, hi).unwrap_or(false))
        };
        self.map2(x, y, one)
    }

    fn call_builtin(&mut self, b: u32, args: &[ColId]) -> ColId {
        for a in args {
            self.resolve(*a);
        }
        match b {
            BI___PRINT | BI_PRINT => {
                let printed = match args.first().map(|a| self.get(*a)) {
                    None => String::new(),
                    Some(Col::U(AV::Str(s))) => self.strings[*s as usize].clone(),
                    Some(Col::U(AV::Num(n))) => format!("{:?}", n),
                    Some(Col::U(AV::Bool(x))) => x.to_string(),
                    Some(Col::U(AV::Nil)) => "nil".to_string(),
                    Some(other) => panic!("print of a lane-varying value: {:?}", other),
                };
                self.prints.push(printed);
                self.put(Col::U(AV::Nil))
            }
            BI___NEW_UNKNOWN_BOOLEAN => {
                assert!(args.is_empty());
                self.put(Col::U(AV::UBool))
            }
            BI___WIDEN_REM | BI___NEW_VECTOR => panic!("unsupported abstract builtin"),
            BI___ARRAY_TABLE_DROP_LAST => {
                let p = self.uptr(args[0]) as usize;
                match &mut self.structure[p] {
                    Cell2::Arr(items) => {
                        assert!(!items.is_empty(), "drop_last of an empty array");
                        items.pop();
                    }
                    other => panic!("__array_table_drop_last on {:?}", other),
                }
                self.put(Col::U(AV::Nil))
            }
            BI_ERROR => panic!("error() reached - lane death; deopt needed"),
            BI_MIN => self.bi_min(args[0], args[1]),
            BI_MAX => self.bi_max(args[0], args[1]),
            BI_ABS => self.bi_abs(args[0]),
            BI_FLR => self.bi_flr(args[0]),
            BI_SIN => self.bi_sin(args[0]),
            BI_MGET => self.bi_mget(args[0], args[1]),
            BI_FGET => {
                let cart = self.cart.clone();
                self.map2(args[0], args[1], move |a, b| {
                    AV::Bool(cart.fget(num(a), num(b)).expect("fget failed"))
                })
            }
            BI_TILE_FLAG_AT => {
                self.bi_tile_flag_at(args[0], args[1], args[2], args[3], args[4])
            }
            BI___SPLIT_BY_FLR => {
                // game_runner.rs:233 - per lane: a number passes through, an
                // interval splits into per-floor sub-intervals.
                let d = self.get(args[0]).clone();
                let w = self.width;
                let needs = match &d {
                    Col::U(AV::Num(_)) => false,
                    Col::U(AV::Ival(a, b)) => a.flr() != b.flr(),
                    Col::V(vs) => vs.iter().any(|v| match v {
                        AV::Ival(a, b) => a.flr() != b.flr(),
                        AV::Num(_) => false,
                        other => panic!("__split_by_flr on {:?}", other),
                    }),
                    Col::N(_) => false,
                    Col::I(vs) => vs.iter().any(|(a, b)| a.flr() != b.flr()),
                    other => panic!("__split_by_flr on {:?}", other),
                };
                if !needs {
                    return self.put(d);
                }
                let subs = |v: AV| -> Vec<Pico8NumInterval> {
                    match v {
                        AV::Ival(..) => split_iv_by_floor(iv_of(v)),
                        _ => Vec::new(),
                    }
                };
                self.split_multi_with(
                    |i| match d.at(i) {
                        AV::Num(_) => 1,
                        v @ AV::Ival(..) => split_iv_by_floor(iv_of(v)).len(),
                        other => panic!("__split_by_flr on {:?}", other),
                    },
                    |i, k| match d.at(i) {
                        AV::Num(n) => AV::Num(n),
                        v @ AV::Ival(..) => av_of_iv(subs(v)[k]),
                        _ => unreachable!(),
                    },
                )
            }
            BI___SPLIT_AT => {
                // game_runner.rs:326 - three closed sides around c; the
                // point side degenerates to the CONCRETE number.
                let c = match self.get(args[1]) {
                    Col::U(AV::Num(n)) => *n,
                    other => panic!("__split_at: lane-varying threshold: {:?}", other),
                };
                let full_low = P8::from_parts(i16::MIN, 0);
                let full_high = P8::from_parts(i16::MAX, 0xffff);
                let mut sides: Vec<Pico8NumInterval> = Vec::with_capacity(3);
                if c > full_low {
                    sides.push(Pico8NumInterval::new(full_low, c.next_smallest()));
                }
                sides.push(Pico8NumInterval::from_number(c));
                if c < full_high {
                    sides.push(Pico8NumInterval::new(c.next_largest(), full_high));
                }
                let d = self.get(args[0]).clone();
                let w = self.width;
                let lane_alts = |v: AV| -> Vec<AV> {
                    match v {
                        AV::Num(n) => vec![AV::Num(n)],
                        AV::Ival(..) => {
                            let iv = iv_of(v);
                            sides
                                .iter()
                                .filter_map(|s| iv.intersect(s))
                                .map(|clipped| {
                                    if clipped.low == clipped.high {
                                        AV::Num(clipped.low)
                                    } else {
                                        av_of_iv(clipped)
                                    }
                                })
                                .collect()
                        }
                        other => panic!("__split_at on {:?}", other),
                    }
                };
                let needs = match &d {
                    Col::U(v) => lane_alts(*v).len() > 1,
                    Col::V(vs) => vs.iter().any(|v| lane_alts(*v).len() > 1),
                    Col::N(_) => false,
                    Col::I(vs) => vs
                        .iter()
                        .any(|(a, b)| lane_alts(AV::Ival(*a, *b)).len() > 1),
                };
                if !needs {
                    // Still apply the point-degeneration clip per lane.
                    return self.map1(args[0], |v| lane_alts(v)[0]);
                }
                self.split_multi_with(
                    |i| lane_alts(d.at(i)).len(),
                    |i, k| lane_alts(d.at(i))[k],
                )
            }
            BI_ADD => {
                // builtin_add (game_runner.rs:53); dead in practice (the Lua
                // `add` shadows it), but kept faithful.
                let p = self.uptr(args[0]) as usize;
                let value = self.get(args[1]).clone();
                let cell = self.structure.len() as u32;
                self.structure.push(Cell2::Val);
                self.cols.push(value.clone());
                match &mut self.structure[p] {
                    Cell2::Arr(items) => items.push(cell),
                    slot @ Cell2::Unk => *slot = Cell2::Arr(vec![cell]),
                    other => panic!("add on {:?}", other),
                }
                self.put(value)
            }
            other => panic!("unknown builtin id {}", other),
        }
    }

    fn callee_of(&mut self, c: ColId, ctx: &str) -> Callee<ColId> {
        let p = self.uptr(c);
        match &self.structure[p as usize] {
            Cell2::Clo(f, caps) => {
                let f = *f;
                let caps = caps.to_vec();
                Callee::Fn(f, caps.into_iter().map(|c| self.put(c)).collect())
            }
            Cell2::Bi(b) => Callee::Bi(*b),
            other => panic!("call on a non-callable cell at {}: {:?}", ctx, other),
        }
    }
}

// ---- frame boundary (abstraction.rs ports) ----

impl Rt2 {
    fn global_target(&self, g: u32) -> Option<u32> {
        let cell = self.globals[g as usize];
        if cell == NONE {
            return None;
        }
        // Globals hold Val(Ptr(target)) for tables (inspect.rs convention).
        match &self.structure[cell as usize] {
            Cell2::Val => match self.cols[cell as usize] {
                Col::U(AV::Ptr(t)) => Some(t),
                _ => None,
            },
            _ => Some(cell),
        }
    }

    fn obj_field_cell(&self, obj: u32, f: u32) -> Option<u32> {
        match &self.structure[obj as usize] {
            Cell2::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
            _ => None,
        }
    }

    /// The player objects' relevant cells (mark_heap, abstraction.rs:134).
    fn mark_walk(&self, ids: &BoundaryIds) -> (Vec<u32>, Vec<u32>) {
        let mut rem_cells = Vec::new();
        let mut det_cells = Vec::new();
        let Some(arr) = self.global_target(ids.g_objects) else {
            return (rem_cells, det_cells);
        };
        let Cell2::Arr(items) = &self.structure[arr as usize] else {
            return (rem_cells, det_cells);
        };
        let player_type = self.global_target(ids.g_player);
        for item in items {
            // Array items are cells holding Ptr(obj).
            let obj = match &self.structure[*item as usize] {
                Cell2::Val => match self.cols[*item as usize] {
                    Col::U(AV::Ptr(o)) => o,
                    _ => continue,
                },
                _ => *item,
            };
            let Some(type_cell) = self.obj_field_cell(obj, ids.f_type) else {
                continue;
            };
            let is_player = match self.cols[type_cell as usize] {
                Col::U(AV::Ptr(t)) => Some(t) == player_type,
                _ => false,
            };
            if !is_player {
                continue;
            }
            if let Some(rem_ptr_cell) = self.obj_field_cell(obj, ids.f_rem) {
                if let Col::U(AV::Ptr(rem_obj)) = self.cols[rem_ptr_cell as usize] {
                    for f in [ids.f_x, ids.f_y] {
                        if let Some(c) = self.obj_field_cell(rem_obj, f) {
                            rem_cells.push(c);
                        }
                    }
                }
            }
            if let Some(c) = self.obj_field_cell(obj, ids.f_dash_effect_time) {
                det_cells.push(c);
            }
        }
        (rem_cells, det_cells)
    }

    /// Boundary abstraction + canonical row dedup + compaction. Returns
    /// the surviving lane count (= next frame's width).
    ///
    /// Ports, in order (make_state_abstract, abstraction.rs:286):
    ///   1. rem widening at Bits(0): full closed interval [-0.5, 0.5-eps]
    ///      (abstraction.rs:521; other precisions are follow-up work).
    ///   2. spd: Exact (the default) - nothing.
    ///   3. dash_effect_time clamp at 0 from below (abstraction.rs:713).
    ///   4. timer globals pinned to 0 (abstraction.rs:837).
    /// Then: reachability BFS from globals over the shared structure (the
    /// per-frame GC + canonical cell order), per-lane 128-bit row hash
    /// over live value cells, dedup, compact columns to survivors.
    pub fn boundary(&mut self, ids: &BoundaryIds) -> usize {
        // Materialize every stale column - the boundary walks whole
        // columns (BFS pointer scan, hashing, compaction).
        for p in 0..self.cols.len() {
            self.resolve_cell(p);
        }
        for ci in 0..self.structure.len() {
            let stale: Vec<(usize, Col)> = match &self.structure[ci] {
                Cell2::Clo(_, caps) => caps
                    .iter()
                    .enumerate()
                    .filter_map(|(i, c)| self.materialize_col(c).map(|m| (i, m)))
                    .collect(),
                _ => continue,
            };
            if let Cell2::Clo(_, caps) = &mut self.structure[ci] {
                for (i, m) in stale {
                    caps[i] = m;
                }
            }
        }
        self.history.clear();
        let (rem_cells, det_cells) = self.mark_walk(ids);

        // 1. rem widening, Bits(0).
        let half = P8::from_parts(0, 0x8000);
        let neg_half = -half;
        let half_below = half.next_smallest();
        let wide = AV::Ival(neg_half, half_below);
        for c in rem_cells {
            let col = &self.cols[c as usize];
            let check = |v: AV| match v {
                AV::Num(n) => assert!(
                    n >= neg_half && n <= half_below,
                    "player_rem value {:?} not in expected interval",
                    n
                ),
                AV::Ival(a, b) => assert!(
                    a >= neg_half && b <= half_below,
                    "player_rem interval [{:?}, {:?}] not in expected interval",
                    a,
                    b
                ),
                other => panic!("Unexpected value type for player_rem: {:?}", other),
            };
            match col {
                Col::U(v) => check(*v),
                Col::V(vs) => vs.iter().for_each(|v| check(*v)),
                Col::N(vs) => vs.iter().for_each(|n| check(AV::Num(*n))),
                Col::I(vs) => vs.iter().for_each(|(a, b)| check(AV::Ival(*a, *b))),
            }
            self.cols[c as usize] = Col::U(wide);
        }

        // 3. dash_effect_time clamp.
        let zero = P8::from_i16(0);
        for c in det_cells {
            let clamp = |v: AV| match v {
                AV::Num(n) => AV::Num(if n < zero { zero } else { n }),
                other => panic!("player dash_effect_time is not a number: {:?}", other),
            };
            self.cols[c as usize] = match &self.cols[c as usize] {
                Col::U(v) => Col::U(clamp(*v)),
                Col::V(vs) => Col::V(vs.iter().map(|v| clamp(*v)).collect()),
                Col::N(vs) => Col::N(
                    vs.iter().map(|n| if *n < zero { zero } else { *n }).collect(),
                ),
                Col::I(_) => panic!("player dash_effect_time is not a number"),
            };
        }

        // 4. timer pins.
        for &g in &ids.g_timers {
            let cell = self.globals[g as usize];
            assert!(cell != NONE, "timer global missing - pin would silently not apply");
            match &self.cols[cell as usize] {
                Col::U(AV::Num(_)) | Col::V(_) => {
                    self.cols[cell as usize] = Col::U(AV::Num(zero));
                }
                other => panic!("timer global is not a number: {:?}", other),
            }
        }

        assert!(self.prints.is_empty(), "prints at a frame boundary: {:?}", self.prints);

        // Canonical reachability BFS from globals in GLOBAL-INDEX order,
        // fields/items in stored order - the discovery order IS the
        // canonical cell numbering, so two blocks with isomorphic heaps
        // compact to IDENTICAL structures and can be concatenated, and the
        // row hash is block-independent (cross-block dedup is exact).
        let mut order: Vec<u32> = Vec::new();
        let mut new_id = vec![u32::MAX; self.structure.len()];
        {
            let mut queue: std::collections::VecDeque<u32> = Default::default();
            fn enqueue(
                t: u32,
                new_id: &mut [u32],
                queue: &mut std::collections::VecDeque<u32>,
                order: &mut Vec<u32>,
            ) {
                if new_id[t as usize] == u32::MAX {
                    new_id[t as usize] = order.len() as u32;
                    order.push(t);
                    queue.push_back(t);
                }
            }
            for &cell in self.globals.iter() {
                if cell != NONE {
                    enqueue(cell, &mut new_id, &mut queue, &mut order);
                }
            }
            while let Some(c) = queue.pop_front() {
                match &self.structure[c as usize] {
                    Cell2::Val => match &self.cols[c as usize] {
                        Col::U(AV::Ptr(t)) => enqueue(*t, &mut new_id, &mut queue, &mut order),
                        Col::U(_) | Col::N(_) | Col::I(_) => {}
                        Col::V(vs) => {
                            for v in vs {
                                if let AV::Ptr(t) = v {
                                    enqueue(*t, &mut new_id, &mut queue, &mut order);
                                }
                            }
                        }
                    },
                    Cell2::Obj(fields) => {
                        for (_, t) in fields {
                            enqueue(*t, &mut new_id, &mut queue, &mut order);
                        }
                    }
                    Cell2::Arr(items) => {
                        for t in items {
                            enqueue(*t, &mut new_id, &mut queue, &mut order);
                        }
                    }
                    Cell2::Clo(_, caps) => {
                        for cap in caps.iter() {
                            match cap {
                                Col::U(AV::Ptr(t)) => {
                                    enqueue(*t, &mut new_id, &mut queue, &mut order)
                                }
                                Col::V(vs) => {
                                    for v in vs {
                                        if let AV::Ptr(t) = v {
                                            enqueue(*t, &mut new_id, &mut queue, &mut order);
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    Cell2::Unk | Cell2::Bi(_) => {}
                }
            }
        }

        // Compact: rebuild structure and columns over the live cells in
        // canonical order, remapping every pointer (structural and
        // per-lane value pointers).
        let remap_av = |v: AV, new_id: &[u32]| -> AV {
            match v {
                AV::Ptr(t) => AV::Ptr(new_id[t as usize]),
                other => other,
            }
        };
        let remap_col = |c: &Col, new_id: &[u32]| -> Col {
            match c {
                Col::U(v) => Col::U(remap_av(*v, new_id)),
                Col::V(vs) => Col::V(vs.iter().map(|v| remap_av(*v, new_id)).collect()),
                Col::N(vs) => Col::N(vs.clone()),
                Col::I(vs) => Col::I(vs.clone()),
            }
        };
        let mut new_structure: Vec<Cell2> = Vec::with_capacity(order.len());
        let mut new_cols: Vec<Col> = Vec::with_capacity(order.len());
        for &old in &order {
            let cell = match &self.structure[old as usize] {
                Cell2::Val => Cell2::Val,
                Cell2::Obj(fields) => Cell2::Obj(
                    fields.iter().map(|(k, t)| (*k, new_id[*t as usize])).collect(),
                ),
                Cell2::Arr(items) => {
                    Cell2::Arr(items.iter().map(|t| new_id[*t as usize]).collect())
                }
                Cell2::Unk => Cell2::Unk,
                Cell2::Clo(f, caps) => Cell2::Clo(
                    *f,
                    caps.iter().map(|c| remap_col(c, &new_id)).collect(),
                ),
                Cell2::Bi(b) => Cell2::Bi(*b),
            };
            new_structure.push(cell);
            new_cols.push(match &self.structure[old as usize] {
                Cell2::Val => compress_num(remap_col(&self.cols[old as usize], &new_id)),
                _ => Col::U(AV::Nil),
            });
        }
        for g in self.globals.iter_mut() {
            if *g != NONE {
                *g = new_id[*g as usize];
            }
        }
        self.structure = new_structure;
        self.cols = new_cols;

        // Structure hash (uniform across lanes) - the block's shape key.
        use std::hash::Hasher;
        let shape_hash = {
            let mut h = rustc_hash::FxHasher::default();
            for cell in &self.structure {
                match cell {
                    Cell2::Val => h.write_u8(1),
                    Cell2::Obj(fields) => {
                        h.write_u8(2);
                        h.write_usize(fields.len());
                        for (k, t) in fields {
                            h.write_u32(*k);
                            h.write_u32(*t);
                        }
                    }
                    Cell2::Arr(items) => {
                        h.write_u8(3);
                        h.write_usize(items.len());
                        for t in items {
                            h.write_u32(*t);
                        }
                    }
                    Cell2::Unk => h.write_u8(4),
                    Cell2::Clo(f, caps) => {
                        h.write_u8(5);
                        h.write_u32(*f);
                        h.write_usize(caps.len());
                    }
                    Cell2::Bi(b) => {
                        h.write_u8(6);
                        h.write_u32(*b);
                    }
                }
            }
            for g in self.globals.iter() {
                h.write_u32(*g);
            }
            h.finish()
        };
        self.shape_hash = shape_hash;

        // Per-lane 128-bit row hash over the compacted value cells, shape
        // hash mixed into both halves.
        fn hash_av(h: &mut rustc_hash::FxHasher, v: AV) {
            match v {
                AV::Num(n) => {
                    h.write_u8(1);
                    h.write_u32(n.to_bits());
                }
                AV::Ival(a, b) => {
                    h.write_u8(2);
                    h.write_u32(a.to_bits());
                    h.write_u32(b.to_bits());
                }
                AV::Bool(b) => {
                    h.write_u8(3);
                    h.write_u8(b as u8);
                }
                AV::UBool => h.write_u8(4),
                AV::Str(s) => {
                    h.write_u8(5);
                    h.write_u32(s);
                }
                AV::Nil => h.write_u8(6),
                AV::Ptr(p) => {
                    h.write_u8(7);
                    h.write_u32(p);
                }
                AV::NilPtr => h.write_u8(8),
            }
        }
        let w = self.width;
        let mut h1: Vec<rustc_hash::FxHasher> = (0..w)
            .map(|_| {
                let mut h = rustc_hash::FxHasher::default();
                h.write_u64(shape_hash);
                h
            })
            .collect();
        let mut h2: Vec<rustc_hash::FxHasher> = (0..w)
            .map(|_| {
                let mut h = rustc_hash::FxHasher::default();
                h.write_u64(0xa076_1d64_78bd_642f ^ shape_hash);
                h
            })
            .collect();
        for (c, cell) in self.structure.iter().enumerate() {
            if !matches!(cell, Cell2::Val) {
                continue;
            }
            match &self.cols[c] {
                Col::U(v) => {
                    for i in 0..w {
                        hash_av(&mut h1[i], *v);
                        hash_av(&mut h2[i], *v);
                    }
                }
                Col::V(vs) => {
                    for i in 0..w {
                        hash_av(&mut h1[i], vs[i]);
                        hash_av(&mut h2[i], vs[i]);
                    }
                }
                Col::N(vs) => {
                    for i in 0..w {
                        hash_av(&mut h1[i], AV::Num(vs[i]));
                        hash_av(&mut h2[i], AV::Num(vs[i]));
                    }
                }
                Col::I(vs) => {
                    for i in 0..w {
                        hash_av(&mut h1[i], AV::Ival(vs[i].0, vs[i].1));
                        hash_av(&mut h2[i], AV::Ival(vs[i].0, vs[i].1));
                    }
                }
            }
        }
        self.row_keys = (0..w).map(|i| (h1[i].finish(), h2[i].finish())).collect();

        // Dedup within the block, keeping the first lane of each row.
        let mut keep: Vec<u32> = Vec::new();
        let mut seen_rows: FxHashMap<(u64, u64), ()> = FxHashMap::default();
        for i in 0..w {
            if let std::collections::hash_map::Entry::Vacant(e) =
                seen_rows.entry(self.row_keys[i])
            {
                e.insert(());
                keep.push(i as u32);
            }
        }
        self.retain_lanes(&keep);

        // All locals are dead at the boundary; recycle their buffers.
        self.stat_arena_peak = self.stat_arena_peak.max(self.arena.len());
        for slot in self.arena.drain(..) {
            if let Some(Col::V(vec)) = slot {
                if self.pool.len() < 48 {
                    self.pool.push(vec);
                }
            }
        }

        self.width
    }

    /// Per-lane key of the "is the object moving" gates (anonymous_61
    /// __entry: `spd.x ~= 0 or spd.y ~= 0`, evaluated on FRAME-START spd -
    /// obj.move runs before update touches spd). One bit per object in
    /// walk order. Pre-partitioning by this removes the SplitReq rerun
    /// for the known divergent gates; novel gates still throw.
    pub fn moving_key(&self, ids: &BoundaryIds) -> Option<Vec<u8>> {
        let arr = self.global_target(ids.g_objects)?;
        let Cell2::Arr(items) = &self.structure[arr as usize] else {
            return None;
        };
        let mut key = vec![0u8; self.width];
        let zero = P8::from_i16(0);
        let nonzero = |v: AV| -> bool {
            match v {
                AV::Num(n) => n != zero,
                // An interval counts as nonzero exactly when `~= 0` is true,
                // which for `Ival == _ -> false` is always.
                AV::Ival(..) => true,
                other => panic!("moving_key: spd is not numeric: {:?}", other),
            }
        };
        for (bit, item) in items.iter().enumerate() {
            let obj = match &self.structure[*item as usize] {
                Cell2::Val => match self.cols[*item as usize] {
                    Col::U(AV::Ptr(o)) => o,
                    _ => continue,
                },
                _ => *item,
            };
            let Some(spd_cell) = self.obj_field_cell(obj, ids.f_spd) else {
                continue;
            };
            let Col::U(AV::Ptr(spd_obj)) = self.cols[spd_cell as usize] else {
                continue;
            };
            for f in [ids.f_x, ids.f_y] {
                let Some(c) = self.obj_field_cell(spd_obj, f) else { continue };
                match &self.cols[c as usize] {
                    Col::U(v) => {
                        if nonzero(*v) {
                            for k in key.iter_mut() {
                                *k |= 1 << (bit % 8);
                            }
                        }
                    }
                    Col::V(vs) => {
                        for (i, v) in vs.iter().enumerate() {
                            if nonzero(*v) {
                                key[i] |= 1 << (bit % 8);
                            }
                        }
                    }
                    Col::N(vs) => {
                        for (i, n) in vs.iter().enumerate() {
                            if *n != zero {
                                key[i] |= 1 << (bit % 8);
                            }
                        }
                    }
                    // Interval spd: `~= 0` is always true (Ival == _ -> false).
                    Col::I(_) => {
                        for k in key.iter_mut() {
                            *k |= 1 << (bit % 8);
                        }
                    }
                }
            }
        }
        Some(key)
    }

    /// Split into sub-blocks of lanes sharing a key value.
    pub fn partition_by_key(self, key: &[u8]) -> Vec<Rt2> {
        let mut order: Vec<u8> = Vec::new();
        let mut groups: Vec<Vec<u32>> = Vec::new();
        for (i, k) in key.iter().enumerate() {
            match order.iter().position(|o| o == k) {
                Some(g) => groups[g].push(i as u32),
                None => {
                    order.push(*k);
                    groups.push(vec![i as u32]);
                }
            }
        }
        if groups.len() == 1 {
            return vec![self];
        }
        groups
            .into_iter()
            .map(|keep| {
                let mut b = self.clone_block();
                b.retain_lanes(&keep);
                b
            })
            .collect()
    }

    /// Reset per-frame bookkeeping. Call before `f___frame`.
    pub fn begin_frame(&mut self) {
        self.origin = (0..self.width as u32).collect();
        self.history.clear();
    }

    /// Frame-start button expansion (#47). MEASURED OUT (2026-08-18): it
    /// makes each widen nearly free (empty arena) but the whole frame then
    /// runs 64x wide from instruction 0 - f30 serial went 2.2s -> 7.0s.
    /// The interpreter's lazy expansion wins for the same reason. Kept
    /// (unused) as the measurement's artifact; delete on next cleanup.
    #[allow(dead_code)]
    pub fn expand_buttons(&mut self, g_button_states: u32) {
        let arr = match self.global_target(g_button_states) {
            Some(a) => a,
            None => return,
        };
        let items = match &self.structure[arr as usize] {
            Cell2::Arr(items) => items.clone(),
            _ => return,
        };
        for item in items {
            let target = match &self.structure[item as usize] {
                Cell2::Val => match self.cols[item as usize] {
                    Col::U(AV::Ptr(t)) => t,
                    _ => item,
                },
                _ => item,
            };
            if !matches!(self.cols[target as usize], Col::U(AV::UBool)) {
                continue;
            }
            let w = self.width;
            let srcs: Vec<usize> = (0..w).collect();
            self.widen(&srcs);
            let mut col: Vec<AV> = Vec::with_capacity(2 * w);
            col.extend(std::iter::repeat(AV::Bool(true)).take(w));
            col.extend(std::iter::repeat(AV::Bool(false)).take(w));
            self.cols[target as usize] = Col::V(col);
        }
    }

    /// Keep only the given lanes (ascending indices), in every value
    /// column, closure capture and row key.
    pub fn retain_lanes(&mut self, keep: &[u32]) {
        if keep.len() == self.width {
            return;
        }
        let retain_col = |col: &mut Col| match col {
            Col::U(_) => {}
            Col::V(vs) => {
                let nv: Vec<AV> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::V(nv);
            }
            Col::N(vs) => {
                let nv: Vec<P8> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::N(nv);
            }
            Col::I(vs) => {
                let nv: Vec<(P8, P8)> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::I(nv);
            }
        };
        for col in self.cols.iter_mut() {
            retain_col(col);
        }
        for cell in self.structure.iter_mut() {
            if let Cell2::Clo(_, caps) = cell {
                for cap in caps.iter_mut() {
                    retain_col(cap);
                }
            }
        }
        if !self.row_keys.is_empty() {
            self.row_keys = keep.iter().map(|&i| self.row_keys[i as usize]).collect();
        }
        self.width = keep.len();
    }

    /// Split the block into sub-blocks whose lanes agree on the value in
    /// `cell` (the frame-start uniform-branch pre-partition; the freeze
    /// gate is the pm1 precedent). Groups in first-occurrence order.
    pub fn partition_by_cell(self, cell: u32) -> Vec<Rt2> {
        let by_vals = |vals: Vec<AV>| -> Vec<Vec<u32>> {
            let mut order: Vec<AV> = Vec::new();
            let mut groups: Vec<Vec<u32>> = Vec::new();
            for (i, v) in vals.iter().enumerate() {
                match order.iter().position(|o| o == v) {
                    Some(g) => groups[g].push(i as u32),
                    None => {
                        order.push(*v);
                        groups.push(vec![i as u32]);
                    }
                }
            }
            groups
        };
        let groups: Vec<Vec<u32>> = match &self.cols[cell as usize] {
            Col::U(_) => return vec![self],
            Col::N(vs) => by_vals(vs.iter().map(|n| AV::Num(*n)).collect()),
            Col::I(vs) => by_vals(vs.iter().map(|(a, b)| AV::Ival(*a, *b)).collect()),
            Col::V(vs) => {
                let mut order: Vec<AV> = Vec::new();
                let mut groups: Vec<Vec<u32>> = Vec::new();
                for (i, v) in vs.iter().enumerate() {
                    match order.iter().position(|o| o == v) {
                        Some(g) => groups[g].push(i as u32),
                        None => {
                            order.push(*v);
                            groups.push(vec![i as u32]);
                        }
                    }
                }
                groups
            }
        };
        if groups.len() == 1 {
            return vec![self];
        }
        groups
            .into_iter()
            .map(|keep| {
                let mut b = self.clone_block();
                b.retain_lanes(&keep);
                b
            })
            .collect()
    }

    /// A copy of just the lanes in `[lo, hi)` - the chunking fast path
    /// (clone_block + retain_lanes copies the whole block first).
    pub fn slice_lanes(&self, lo: usize, hi: usize) -> Rt2 {
        let slice_col = |c: &Col| -> Col {
            match c {
                Col::U(a) => Col::U(*a),
                Col::V(vs) => Col::V(vs[lo..hi].to_vec()),
                Col::N(vs) => Col::N(vs[lo..hi].to_vec()),
                Col::I(vs) => Col::I(vs[lo..hi].to_vec()),
            }
        };
        Rt2 {
            width: hi - lo,
            structure: self
                .structure
                .iter()
                .map(|cell| match cell {
                    Cell2::Clo(f, caps) => {
                        Cell2::Clo(*f, caps.iter().map(&slice_col).collect())
                    }
                    other => other.clone(),
                })
                .collect(),
            cols: self.cols.iter().map(&slice_col).collect(),
            arena: Vec::new(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cart: self.cart.clone(),
            cache: self.cache.clone(),
            prints: self.prints.clone(),
            stat_splits: self.stat_splits,
            stat_appended: self.stat_appended,
            stat_arena_peak: self.stat_arena_peak,
            pool: Vec::new(),
            census: self.census.as_ref().map(|_| FxHashMap::default()),
            origin: Vec::new(),
            history: Vec::new(),
            shape_hash: self.shape_hash,
            row_keys: Vec::new(),
        }
    }

    /// A clone that shares the immutable context (cart/cache Arcs).
    pub fn clone_block(&self) -> Rt2 {
        Rt2 {
            width: self.width,
            structure: self.structure.clone(),
            cols: self.cols.clone(),
            arena: Vec::new(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cart: self.cart.clone(),
            cache: self.cache.clone(),
            prints: self.prints.clone(),
            stat_splits: self.stat_splits,
            stat_appended: self.stat_appended,
            stat_arena_peak: self.stat_arena_peak,
            pool: Vec::new(),
            census: self.census.as_ref().map(|_| FxHashMap::default()),
            origin: self.origin.clone(),
            history: Vec::new(),
            shape_hash: self.shape_hash,
            row_keys: self.row_keys.clone(),
        }
    }

    /// K-way merge of same-shape blocks (all after `boundary`): one pass
    /// per column, no repeated re-materialization of uniform columns.
    pub fn merge_many(mut blocks: Vec<Rt2>) -> Rt2 {
        if blocks.len() == 1 {
            return blocks.pop().unwrap();
        }
        let total: usize = blocks.iter().map(|b| b.width).sum();
        let mut host = blocks.swap_remove(0);
        for b in &blocks {
            assert_eq!(host.shape_hash, b.shape_hash, "merge of different shapes");
            assert_eq!(host.structure.len(), b.structure.len());
        }
        for (c, col) in host.cols.iter_mut().enumerate() {
            let all_same_uniform = match col {
                Col::U(a) => blocks
                    .iter()
                    .all(|b| matches!(&b.cols[c], Col::U(x) if x == a)),
                Col::V(_) | Col::N(_) | Col::I(_) => false,
            };
            if all_same_uniform {
                continue;
            }
            let mut vs: Vec<AV> = Vec::with_capacity(total);
            let push_col = |vs: &mut Vec<AV>, c: &Col, w: usize| match c {
                Col::U(a) => vs.extend(std::iter::repeat(*a).take(w)),
                Col::V(v) => vs.extend_from_slice(v),
                Col::N(v) => vs.extend(v.iter().map(|n| AV::Num(*n))),
                Col::I(v) => vs.extend(v.iter().map(|(a, b)| AV::Ival(*a, *b))),
            };
            let hw = host.width;
            push_col(&mut vs, col, hw);
            for b in &blocks {
                push_col(&mut vs, &b.cols[c], b.width);
            }
            *col = compress_num_v(vs);
        }
        for b in &blocks {
            host.row_keys.extend_from_slice(&b.row_keys);
        }
        host.width = total;
        host
    }

    /// Append another block's lanes. Only legal after `boundary` on both:
    /// canonical compaction makes isomorphic heaps IDENTICAL, asserted.
    pub fn concat(&mut self, other: &Rt2) {
        assert_eq!(self.shape_hash, other.shape_hash, "concat of different shapes");
        assert_eq!(self.structure.len(), other.structure.len());
        let w = self.width;
        let ow = other.width;
        for (c, col) in self.cols.iter_mut().enumerate() {
            let ocol = &other.cols[c];
            let same_uniform = matches!((&*col, ocol), (Col::U(a), Col::U(b)) if a == b);
            if same_uniform {
                continue;
            }
            let mut vs: Vec<AV> = match col {
                Col::U(a) => vec![*a; w],
                Col::V(v) => std::mem::take(v),
                Col::N(v) => v.iter().map(|n| AV::Num(*n)).collect(),
                Col::I(v) => v.iter().map(|(a, b)| AV::Ival(*a, *b)).collect(),
            };
            match ocol {
                Col::U(b) => vs.extend(std::iter::repeat(*b).take(ow)),
                Col::V(ov) => vs.extend_from_slice(ov),
                Col::N(ov) => vs.extend(ov.iter().map(|n| AV::Num(*n))),
                Col::I(ov) => vs.extend(ov.iter().map(|(a, b)| AV::Ival(*a, *b))),
            }
            *col = compress_num_v(vs);
        }
        self.row_keys.extend_from_slice(&other.row_keys);
        self.width += ow;
        self.stat_splits = self.stat_splits.max(other.stat_splits);
        self.stat_appended = self.stat_appended.max(other.stat_appended);
    }
}

