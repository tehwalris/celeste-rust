//! THE KERNELS AGAINST THE ARITHMETIC, operation by operation, in both
//! arithmetics: every root of an assembled kernel, on every lane, against a
//! scalar evaluation of the graph (`oracle`) with the arithmetic's own point
//! and interval operations (`Arithmetic`: `Pico8Num`'s and
//! `Pico8NumInterval`'s checked operations for 16.16, `LuaF64`'s and
//! `LuaF64Interval`'s for the doubles), bit for bit: `-0` against `0`, ties,
//! subnormals, the range's ends, the forks' cut points.
//!
//! A value is UNDEFINED on a lane where the arithmetic has no answer (an
//! overflow; a fork fragment the lane does not reach) and is not compared
//! there; the operation's `NoWrap` is, so the kernel must decline exactly
//! those lanes. Booleans are compared on both planes (`val` and `known`) on
//! every lane: the oracle spells out the planes' Kleene formulas.

use std::os::raw::c_void;

use celeste_core::arith::{Arithmetic, Fixed, Float};
use celeste_core::cart_data::CART;
use celeste_core::room::Room;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

use crate::compiled::asm_kernel::KernelLanes;
use crate::transpile::asm::{compile, compile_and_load, AsmCtx, CollisionEnv, Compiled, Loaded};
use crate::transpile::graph::{one_operand, Cut, Graph, NodeId, Op, Ty, Unknown};

/// A tiny deterministic PRNG (splitmix64), so the tests need no `rand`.
struct Lcg(u64);
impl Lcg {
    fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }
    fn i32(&mut self) -> i32 {
        self.next_u64() as i32
    }
    fn bit(&mut self) -> bool {
        self.next_u64() & 1 == 1
    }
}

/// What the harness needs of an arithmetic beyond its operations: test
/// values, and the reach of the kernels' fork grid - the one place a lane's
/// answer is not an operation of the arithmetic. 16.16's grid is the raw
/// `i32`, which wraps past the top floor; there the fork's premise
/// (`ForkOk`) is false, so the lane declines.
trait TestArith: KernelLanes {
    /// Numbers every operation is tried on: zeros, both signs, the range's
    /// ends and the operations' thresholds, cut points, random values.
    fn specials() -> Vec<Self>;
    /// A random number; `small`: well inside the range (a sum of two fits).
    fn random(rng: &mut Lcg, small: bool) -> Self;
    /// Literals a product or quotient is tried with (`positive`: only those).
    fn factors(positive: bool) -> Vec<Self>;
    /// Does the fork grid reach floor `c` above `lo`'s inside the range?
    fn grid_ok(lo: Self, c: u8) -> bool;
    /// `ForkOk(Floors, ways)`: the lane spans at most `ways` floors and the grid
    /// reaches them.
    fn split_ok(i: Self::Ival, ways: u8) -> bool;
}

/// Raw 16.16 values at and around every edge of the operations: the
/// range's ends, 0 and +-1, the floors, and the products' and quotients'
/// thresholds (`0xb504f3` is ~sqrt(2^47)).
const EDGES: [i32; 26] = [
    i32::MIN, i32::MIN + 1, -0x4000_0000, -0xb5_04f4, -0xb5_04f3, -0x1_8000, -0x1_0001, -0x1_0000, -0xffff, -0x8000, -1, 0, 1, 0x8000,
    0xffff, 0x1_0000, 0x1_0001, 0x1_8000, 0xb5_04f3, 0xb5_04f4, 0x4000_0000, 0x3fff_ffff, 0x7fff_0000, 0x7ffe_ffff, i32::MAX - 1, i32::MAX,
];

impl TestArith for Fixed {
    fn specials() -> Vec<P8> {
        let mut rng = Lcg(0x1234_5678);
        let mut v: Vec<P8> = EDGES.iter().map(|r| P8::from_raw(*r)).collect();
        for _ in 0..40 {
            v.push(Self::random(&mut rng, false));
            // Small values too: the game's are mostly in [-256, 256].
            v.push(P8::from_i16((rng.next_u64() % 512) as i16 - 256));
        }
        v
    }
    fn random(rng: &mut Lcg, small: bool) -> P8 {
        P8::from_raw(if small { (rng.i32() % 0x8_0000) - 0x4_0000 } else { rng.i32() })
    }
    fn factors(positive: bool) -> Vec<P8> {
        let mut k = vec![0x8000, 0x4000, 0xffff, 0x1_0000, 0x1_0001, 0x1_8000, 0x2_0000, 0x3_0000, 0x8_0000, 1, 0x7fff_0000, i32::MAX];
        if !positive {
            k.extend([0, -1, -0x8000, -0x1_0000, -0x2_0000, i32::MIN]);
        }
        k.into_iter().map(P8::from_raw).collect()
    }
    fn grid_ok(lo: P8, c: u8) -> bool {
        lo.flr().as_raw_u32() as i32 as i64 + ((c as i64) << 16) <= i32::MAX as i64
    }
    fn split_ok(i: <Fixed as Arithmetic>::Ival, ways: u8) -> bool {
        Self::grid_ok(i.low, ways - 1) && Fixed::floors(i).is_some_and(|n| n <= ways as u32)
    }
}

impl TestArith for Float {
    fn specials() -> Vec<LuaF64> {
        let f = |v: f64| LuaF64::new(v).expect("finite");
        let mut v: Vec<LuaF64> = [
            0.0,
            -0.0,
            f64::from_bits(1),
            -f64::from_bits(1),
            f64::MIN_POSITIVE,
            -f64::MIN_POSITIVE,
            f64::MIN_POSITIVE / 2.0,
            -f64::MIN_POSITIVE / 3.0,
            0.5,
            -0.5,
            0.5f64.next_down(),
            (-0.5f64).next_down(),
            1.5,
            -1.5,
            1.0,
            -1.0,
            2.0,
            0.3,
            -0.7,
            2.5,
            -3.25,
            7.0,
            1e300,
            -1e300,
            f64::MAX,
            -f64::MAX,
            123.456,
            -0.21,
            0.05,
        ]
        .into_iter()
        .map(f)
        .collect();
        let mut rng = Lcg(0x2545_f491_4f6c_dd1d);
        for _ in 0..40 {
            v.push(Self::random(&mut rng, true));
        }
        v
    }
    fn random(rng: &mut Lcg, small: bool) -> LuaF64 {
        if small {
            return LuaF64::new((rng.next_u64() % 1_000_000) as f64 / 997.0 - 500.0).expect("finite");
        }
        loop {
            if let Some(v) = LuaF64::new(f64::from_bits(rng.next_u64())) {
                return v;
            }
        }
    }
    fn factors(positive: bool) -> Vec<LuaF64> {
        let mut k = vec![0.5, 0.25, 1.0, 1.5, 2.0, 3.0, 0.1, 1e-300, 1e300, f64::MAX];
        if !positive {
            k.extend([0.0, -0.0, -1.0, -0.5, -3.0, -f64::MAX]);
        }
        k.into_iter().map(|v| LuaF64::new(v).expect("finite")).collect()
    }
    /// The floors are doubles: `floor(lo) + c` is exact below 2^52.
    fn grid_ok(lo: LuaF64, c: u8) -> bool {
        lo.value().floor().abs() + (c as f64) < (1u64 << 52) as f64
    }
    fn split_ok(i: <Float as Arithmetic>::Ival, ways: u8) -> bool {
        Float::floors(i).is_some_and(|n| n <= ways as u32)
    }
}

/// A lane's input cell.
#[derive(Clone, Copy, Debug)]
enum In<A: Arithmetic> {
    N(A),
    /// An interval, `lo <= hi`.
    I(A, A),
    B(bool),
    /// A maybe-unknown boolean: `(val, known)`.
    U(bool, bool),
}

/// A node's value on one lane.
#[derive(Clone, Copy, Debug)]
enum V<A: Arithmetic> {
    N(A),
    I(A::Ival),
    /// `(val, known)`: both planes' bits, `val` meaningful on unknown lanes
    /// too (the kernels' representation).
    B(bool, bool),
}

/// A root's slot on one lane: its raw bits.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Out<R> {
    N(R),
    I(R, R),
    B(bool, bool),
}

fn as_ival<A: Arithmetic>(v: V<A>) -> A::Ival {
    match v {
        V::N(x) => A::point(x),
        V::I(i) => i,
        V::B(..) => panic!("a boolean where a number was needed"),
    }
}

fn as_point<A: Arithmetic>(v: V<A>) -> A {
    match v {
        V::N(x) => x,
        other => panic!("{other:?} where a number was needed"),
    }
}

fn as_bool<A: Arithmetic>(v: V<A>) -> (bool, bool) {
    match v {
        V::B(val, known) => (val, known),
        other => panic!("{other:?} where a boolean was needed"),
    }
}

/// `x op y` in the arithmetic: on two numbers the number's operation, else
/// the interval's (a number is its point); `None` where it has no answer.
fn arith<A: Arithmetic>(op: &Op<A>, x: V<A>, y: V<A>) -> Option<V<A>> {
    if let (V::N(p), V::N(q)) = (x, y) {
        return Some(V::N(match op {
            Op::Add => A::add(p, q)?,
            Op::Sub => A::sub(p, q)?,
            Op::Mul => A::mul(p, q)?,
            Op::Div => A::div(p, q)?,
            Op::Rem => A::rem(p, q)?,
            Op::Min => A::min(p, q),
            Op::Max => A::max(p, q),
            other => panic!("{other:?} is not a binary operation"),
        }));
    }
    let (a, b) = (as_ival(x), as_ival(y));
    Some(V::I(match op {
        Op::Add => A::ival_add(a, b)?,
        Op::Sub => A::ival_sub(a, b)?,
        Op::Mul => A::ival_mul(a, b)?,
        Op::Div => A::ival_div(a, b)?,
        // End by end: each is monotone in both operands.
        Op::Min | Op::Max => {
            let ((al, ah), (bl, bh)) = (A::bounds(a), A::bounds(b));
            let pick = |p, q| if matches!(op, Op::Min) { A::min(p, q) } else { A::max(p, q) };
            A::ival(pick(al, bl), pick(ah, bh))
        }
        other => panic!("{other:?} of an interval never reaches a kernel"),
    }))
}

/// Negation and `abs`, as `arith`.
fn arith1<A: Arithmetic>(op: &Op<A>, x: V<A>) -> Option<V<A>> {
    let neg = matches!(op, Op::Neg);
    Some(match x {
        V::N(p) => V::N(if neg { A::neg(p)? } else { A::abs(p)? }),
        V::I(i) => V::I(if neg { A::ival_neg(i)? } else { A::ival_abs(i)? }),
        V::B(..) => panic!("{op:?} of a boolean"),
    })
}

/// THE ORACLE: every node of `g` on one lane (`row`: its cells), in the
/// arithmetic, `None` where undefined (`arith`, an unreached fragment, a
/// floor that is not one).
fn oracle<A: TestArith>(g: &Graph<A>, row: &[In<A>], env: Option<&CollisionEnv>) -> Vec<Option<V<A>>> {
    let mut out: Vec<Option<V<A>>> = Vec::with_capacity(g.len());
    for id in 0..g.len() as NodeId {
        let v = lane_value(g, &out, id, row, env);
        out.push(v);
    }
    out
}

fn lane_value<A: TestArith>(g: &Graph<A>, out: &[Option<V<A>>], id: NodeId, row: &[In<A>], env: Option<&CollisionEnv>) -> Option<V<A>> {
    let node = g.get(id);
    let arg = |k: usize| out[node.args[k] as usize];
    // Decided where every pair from the two boxes agrees.
    let tri = |t: bool, f: bool| V::B(t, t || f);
    Some(match &node.op {
        Op::Const(lo, hi) if lo == hi => V::N(A::from_lit(*lo)),
        Op::Const(lo, hi) => V::I(A::ival(A::from_lit(*lo), A::from_lit(*hi))),
        Op::ConstBool(b) => V::B(*b, true),
        Op::UnknownBool(_) => V::B(false, false),
        Op::Cell(c) => match row[*c as usize] {
            In::N(x) => V::N(x),
            In::I(lo, hi) => V::I(A::ival(lo, hi)),
            In::B(b) => V::B(b, true),
            In::U(val, known) => V::B(val, known),
        },
        // The operand unchanged: the range is the node's own error.
        Op::Restrict(..) => arg(0)?,
        op @ (Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Min | Op::Max) => arith(op, arg(0)?, arg(1)?)?,
        op @ (Op::Neg | Op::Abs) => arith1(op, arg(0)?)?,
        // An interval's floor where it is one (`Known(Flr(x))`, its premise).
        Op::Flr => match arg(0)? {
            V::N(x) => V::N(A::flr(x)),
            v => {
                let (lo, hi) = A::bounds(as_ival(v));
                if !A::num_eq(A::flr(lo), A::flr(hi)) {
                    return None;
                }
                V::N(A::flr(lo))
            }
        },
        Op::Sin => V::N(A::sin(as_point(arg(0)?))?),
        op @ (Op::Lt | Op::Le | Op::Gt | Op::Ge) => {
            let ((al, ah), (bl, bh)) = (A::bounds(as_ival(arg(0)?)), A::bounds(as_ival(arg(1)?)));
            match op {
                Op::Lt => tri(A::lt(ah, bl), A::le(bh, al)),
                Op::Le => tri(A::le(ah, bl), A::lt(bh, al)),
                Op::Gt => tri(A::lt(bh, al), A::le(ah, bl)),
                _ => tri(A::le(bh, al), A::lt(ah, bl)),
            }
        }
        // Booleans: equal planes where both are known. Numbers: the cart's
        // `==`. Intervals: decided on two points, or two boxes apart.
        Op::Eq => match (arg(0)?, arg(1)?) {
            (V::B(p, pk), V::B(q, qk)) => V::B(p == q, pk && qk),
            (V::N(x), V::N(y)) => V::B(A::num_eq(x, y), true),
            (x, y) => {
                let (x, y) = (as_ival(x), as_ival(y));
                match (A::as_point(x), A::as_point(y)) {
                    (Some(p), Some(q)) => V::B(A::num_eq(p, q), true),
                    _ => {
                        let ((xl, xh), (yl, yh)) = (A::bounds(x), A::bounds(y));
                        V::B(false, A::lt(xh, yl) || A::lt(yh, xl))
                    }
                }
            }
        },
        Op::Same => V::B(A::bits(as_point(arg(0)?)) == A::bits(as_point(arg(1)?)), true),
        Op::Not => {
            let (v, k) = as_bool(arg(0)?);
            V::B(!v, k)
        }
        // Kleene: known where both are, or either is known false (`And`) /
        // true (`Or`).
        Op::And => {
            let ((p, pk), (q, qk)) = (as_bool(arg(0)?), as_bool(arg(1)?));
            V::B(p && q, (pk && qk) || (pk && !p) || (qk && !q))
        }
        Op::Or => {
            let ((p, pk), (q, qk)) = (as_bool(arg(0)?), as_bool(arg(1)?));
            V::B(p || q, (pk && qk) || (pk && p) || (qk && q))
        }
        // `Known(Flr(x))` of an interval: is its floor one?
        Op::Known => {
            let inner = g.get(node.args[0]);
            match (&inner.op, inner.args.first().map(|a| out[*a as usize])) {
                (Op::Flr, Some(Some(V::I(i)))) => {
                    let (lo, hi) = A::bounds(i);
                    V::B(A::num_eq(A::flr(lo), A::flr(hi)), true)
                }
                _ => match arg(0)? {
                    V::B(_, known) => V::B(known, true),
                    V::I(i) => V::B(A::as_point(i).is_some(), true),
                    V::N(_) => V::B(true, true),
                },
            }
        }
        Op::Lo | Op::Hi => {
            let (lo, hi) = A::bounds(as_ival(arg(0)?));
            V::N(if matches!(node.op, Op::Lo) { lo } else { hi })
        }
        // A blend by the condition's `val` plane (an undecided lane blends as
        // it says: the select's own error).
        Op::Sel => {
            if as_bool(arg(0)?).0 {
                arg(1)?
            } else {
                arg(2)?
            }
        }
        Op::Span => {
            let (lo, hi) = (A::bounds(as_ival(arg(0)?)).0, A::bounds(as_ival(arg(1)?)).1);
            if !A::le(lo, hi) {
                return None;
            }
            V::I(A::ival(lo, hi))
        }
        // The floor fork's fragment `c`: defined where the lane reaches it.
        Op::Frag(Cut::Floors, c) | Op::FragOk(Cut::Floors, c) | Op::Frag(Cut::Ints, c) => {
            let i = as_ival(arg(0)?);
            if !A::grid_ok(A::bounds(i).0, *c) {
                return None;
            }
            let reached = *c == 0 || A::floors(i).is_none_or(|n| n > *c as u32);
            match node.op {
                Op::FragOk(..) => V::B(reached, true),
                _ if !reached => return None,
                Op::Frag(Cut::Floors, _) => V::I(A::floor_fragment(i, *c as u32)),
                _ => V::N(A::bounds(A::floor_fragment(i, *c as u32)).0),
            }
        }
        Op::ForkOk(Cut::Floors, ways) => V::B(A::split_ok(as_ival(arg(0)?), *ways), true),
        // The round fork (doubles): as the floor fork's, over the values of
        // `flr(x + 1/2)`; its premise also wants every cut it reads settled.
        Op::Frag(Cut::Rounds, c) | Op::FragOk(Cut::Rounds, c) => {
            let i = as_ival(arg(0)?);
            let reached = A::rounds(i).ok()?.is_none_or(|n| (*c as u32) < n);
            match node.op {
                Op::FragOk(Cut::Rounds, _) => V::B(reached, true),
                _ if !reached => return None,
                _ => V::I(A::round_fragment(i, *c as u32).ok()?),
            }
        }
        Op::ForkOk(Cut::Rounds, ways) => {
            let i = as_ival(arg(0)?);
            let fits = A::rounds(i).ok()?.is_some_and(|n| n <= *ways as u32);
            V::B(fits && (0..*ways).all(|c| A::round_fragment(i, c as u32).is_ok()), true)
        }
        // Did the operation have an answer?
        Op::NoWrap => {
            let x = g.get(node.args[0]);
            let a = |k: usize| out[x.args[k] as usize];
            V::B(
                match &x.op {
                    op @ (Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem) => arith(op, a(0)?, a(1)?).is_some(),
                    op @ (Op::Neg | Op::Abs) => arith1(op, a(0)?).is_some(),
                    _ => true,
                },
                true,
            )
        }
        // The map, on whole-number coordinates.
        Op::Mget => {
            let whole = |v: V<A>| A::as_i16(as_point(v));
            V::N(A::from_i16(CART.mget_whole(whole(arg(0)?)?, whole(arg(1)?)?) as i16))
        }
        Op::TileFlagAt => {
            let env = env.expect("tile_flag_at needs the map");
            let w: Vec<i16> = (0..5).map(|k| arg(k).and_then(|v| A::as_i16(as_point(v)))).collect::<Option<_>>()?;
            V::B(env.room.flag_at(w[0], w[1], w[2], w[3], w[4]).expect("flag_at"), true)
        }
        other => panic!("node {id}: {other:?} never reaches a kernel"),
    })
}

fn build<A: TestArith>(g: &Graph<A>, roots: &[NodeId], tag: &str) -> (Compiled, Loaded) {
    compile_and_load(g, roots, &format!("kernel_{tag}")).expect("compile and load")
}

/// Run up to sixteen lanes (`rows[j]`: lane `j`'s cells; unused lanes repeat
/// lane 0); per lane, per root, its slot.
fn run<A: TestArith>(c: &Compiled, l: &Loaded, rows: &[Vec<In<A>>], env: Option<&CollisionEnv>) -> Vec<Vec<Out<A::Raw>>> {
    assert!(!rows.is_empty() && rows.len() <= 16);
    let halves = if c.halves { 2 } else { 1 };
    let (ib, ob) = (c.input_bytes as usize, c.out_bytes as usize);
    let mut inbuf = vec![0u8; halves * ib];
    let mut out = vec![0u8; halves * ob];
    let row = |j: usize| &rows[if j < rows.len() { j } else { 0 }];
    for (ci, cell) in c.input_cells.iter().enumerate() {
        let off = c.input_offsets[ci] as usize;
        let (mut val, mut known) = (0u16, 0u16);
        for j in 0..16 {
            let put = |buf: &mut Vec<u8>, at: usize, v: A| A::put(buf, ib, at, j, v.raw());
            match (c.input_types[ci], row(j)[*cell as usize]) {
                (Ty::Num, In::N(x)) => put(&mut inbuf, off, x),
                (Ty::Ival, In::I(lo, hi)) => {
                    put(&mut inbuf, off, lo);
                    put(&mut inbuf, off + 64, hi);
                }
                (Ty::Ival, In::N(x)) => {
                    put(&mut inbuf, off, x);
                    put(&mut inbuf, off + 64, x);
                }
                (Ty::Bool, In::B(b)) => val |= (b as u16) << j,
                (Ty::UBool, In::U(v, k)) => {
                    val |= (v as u16) << j;
                    known |= (k as u16) << j;
                }
                (r, v) => panic!("cell {cell}: {v:?} into a {r:?} input"),
            }
        }
        match c.input_types[ci] {
            Ty::Bool => A::put_masks(&mut inbuf, ib, off, val, None),
            Ty::UBool => A::put_masks(&mut inbuf, ib, off, val, Some(known)),
            Ty::Num | Ty::Ival => {}
        }
    }
    A::before_call();
    let ctx = AsmCtx::<A>::new(env.map_or(std::ptr::null(), |e| e as *const CollisionEnv as *const c_void));
    // SAFETY: the buffers have the layout `c` states (`halves` of each).
    unsafe { (l.func)(inbuf.as_ptr(), out.as_mut_ptr(), &ctx as *const AsmCtx<A> as *const c_void) };
    (0..rows.len())
        .map(|j| {
            (0..c.n_roots)
                .map(|r| {
                    let o = c.root_offsets[r] as usize;
                    match c.root_kinds[r] {
                        Ty::Num => Out::N(A::lane(&out, ob, o, j)),
                        Ty::Ival => Out::I(A::lane(&out, ob, o, j), A::lane(&out, ob, o + 64, j)),
                        Ty::Bool | Ty::UBool => {
                            let (val, known) = A::masks(&out, ob, o);
                            Out::B(val >> j & 1 == 1, known >> j & 1 == 1)
                        }
                    }
                })
                .collect()
        })
        .collect()
}

/// Does a root's slot hold the oracle's value?
fn agrees<A: Arithmetic>(got: Out<A::Raw>, want: V<A>) -> bool
where
    A: Arithmetic,
{
    match (got, want) {
        (Out::N(r), V::N(x)) => r == x.raw(),
        (Out::I(lo, hi), V::I(i)) => (lo, hi) == (A::bounds(i).0.raw(), A::bounds(i).1.raw()),
        // A number beside an interval is its point (a select's joined arms).
        (Out::I(lo, hi), V::N(x)) => (lo, hi) == (x.raw(), x.raw()),
        (Out::B(v, k), V::B(val, known)) => (v, k) == (val, known),
        (got, want) => panic!("a root's slot {got:?} is not of the oracle's kind ({want:?})"),
    }
}

/// THE CHECK: `roots` of `g` assembled, run on every lane of `rows`, and
/// every root's slot against the oracle wherever it is defined. Returns the
/// oracle's root values per row (for a test's own coverage counts).
fn check<A: TestArith>(g: &Graph<A>, roots: &[NodeId], rows: &[Vec<In<A>>], env: Option<&CollisionEnv>, tag: &str) -> Vec<Vec<Option<V<A>>>> {
    let (c, l) = build(g, roots, tag);
    let mut compared = 0usize;
    let mut wants = Vec::with_capacity(rows.len());
    for chunk in rows.chunks(16) {
        let got = run::<A>(&c, &l, chunk, env);
        for (row, got) in chunk.iter().zip(&got) {
            let want = oracle(g, row, env);
            let want: Vec<Option<V<A>>> = roots.iter().map(|r| want[*r as usize]).collect();
            for (ri, (g_, w)) in got.iter().zip(&want).enumerate() {
                if let Some(w) = w {
                    assert!(agrees(*g_, *w), "{tag}: root {ri} ({:?}) at {row:?}: the kernel's {g_:?}, the arithmetic's {w:?}", g.get(roots[ri]).op);
                    compared += 1;
                }
            }
            wants.push(want);
        }
    }
    assert!(compared > 0, "{tag}: nothing compared");
    wants
}

/// Every pair of `xs` (`step`: one in `step`, staggered).
fn pairs<T: Copy>(xs: &[T], step: usize) -> Vec<(T, T)> {
    xs.iter().enumerate().flat_map(|(i, a)| xs.iter().skip(i % step).step_by(step).map(move |b| (*a, *b))).collect()
}

/// Intervals from the specials: points, ordered pairs, and the arithmetic's
/// zero both ways.
fn intervals<A: TestArith>() -> Vec<(A, A)> {
    let sp = A::specials();
    let zero = A::from_i16(0);
    let mut out = vec![(zero, zero), (A::neg(zero).expect("-0"), zero)];
    for (i, a) in sp.iter().enumerate() {
        let b = sp[(i * 7 + 3) % sp.len()];
        out.push(if A::le(*a, b) { (*a, b) } else { (b, *a) });
        out.push((*a, *a));
    }
    out.retain(|(lo, hi)| A::le(*lo, *hi));
    out
}

/// Cells `0..`, of the types `tys`.
fn cells<A: TestArith>(g: &mut Graph<A>, tys: &[Ty]) -> Vec<NodeId> {
    g.set_cell_kinds(tys.iter().enumerate().map(|(c, t)| (c as u32, *t)));
    (0..tys.len() as u32).map(|c| g.leaf(Op::Cell(c))).collect()
}

/// A number's type, or an interval's (`wide`).
fn num_or_ival(wide: bool) -> Ty {
    if wide { Ty::Ival } else { Ty::Num }
}

fn lit<A: TestArith>(g: &mut Graph<A>, v: A) -> NodeId {
    let l = A::lit(v);
    g.leaf(Op::Const(l, l))
}

/// Instantiate a generic test in both arithmetics.
macro_rules! both {
    ($name:ident, $fixed:ident, $float:ident) => {
        #[test]
        fn $fixed() {
            $name::<Fixed>();
        }
        #[test]
        fn $float() {
            $name::<Float>();
        }
    };
}

/// `+`, `-`, `min` and `max` of two numbers and of two intervals, with the
/// sum's and difference's own error, over every pair of the specials.
fn sums_differences_and_extremes<A: TestArith>() {
    for wide in [false, true] {
        let mut g = Graph::<A>::default();
        let c = cells(&mut g, &[num_or_ival(wide); 2]);
        let mut roots = Vec::new();
        for op in [Op::Add, Op::Sub, Op::Min, Op::Max] {
            let r = g.add(op.clone(), vec![c[0], c[1]]);
            roots.push(r);
            if matches!(op, Op::Add | Op::Sub) {
                roots.push(g.add(Op::NoWrap, vec![r]));
            }
        }
        let rows: Vec<Vec<In<A>>> = if wide {
            pairs(&intervals::<A>(), 7).into_iter().map(|(a, b)| vec![In::I(a.0, a.1), In::I(b.0, b.1)]).collect()
        } else {
            pairs(&A::specials(), 1).into_iter().map(|(a, b)| vec![In::N(a), In::N(b)]).collect()
        };
        let wants = check(&g, &roots, &rows, None, if wide { "isum" } else { "sum" });
        // Both a fit and an overflow of `+` were compared, where the
        // arithmetic can overflow at all.
        let fits: Vec<bool> = wants.iter().filter_map(|w| w[1].map(|v| as_bool(v).0)).collect();
        assert!(fits.contains(&true), "a sum that fits");
        assert!(A::NAME == "float" && wide || fits.contains(&false), "a sum that overflows");
    }
}
both!(sums_differences_and_extremes, sums_differences_and_extremes_fixed, sums_differences_and_extremes_float);

/// Negation, `abs` (each with its own error), `flr` and its premise, and
/// `sin` of a number.
fn unary_operations<A: TestArith>() {
    for wide in [false, true] {
        let mut g = Graph::<A>::default();
        let x = cells(&mut g, &[num_or_ival(wide)])[0];
        let (neg, abs, flr) = (g.add(Op::Neg, vec![x]), g.add(Op::Abs, vec![x]), g.add(Op::Flr, vec![x]));
        let mut roots = vec![neg, abs, flr, g.add(Op::NoWrap, vec![neg]), g.add(Op::NoWrap, vec![abs]), g.add(Op::Known, vec![flr])];
        if !wide {
            roots.push(g.add(Op::Sin, vec![x]));
        }
        let rows: Vec<Vec<In<A>>> = if wide {
            intervals::<A>().into_iter().map(|(a, b)| vec![In::I(a, b)]).collect()
        } else {
            // `sin` in turns: finite where the cart's arguments are.
            A::specials().into_iter().filter(|v| A::lt(A::abs(*v).unwrap_or(*v), A::from_i16(30000))).map(|v| vec![In::N(v)]).collect()
        };
        check(&g, &roots, &rows, None, if wide { "iunary" } else { "unary" });
    }
}
both!(unary_operations, unary_operations_fixed, unary_operations_float);

/// The comparisons: on numbers the cart's (a double's `-0 == 0`), on
/// intervals tri-state (separated, or two points); a number against an
/// interval; `Same` (bit identity) where `==` is not.
fn comparisons<A: TestArith>() {
    let ops = [Op::Lt, Op::Le, Op::Gt, Op::Ge, Op::Eq];
    for (wide_x, wide_y) in [(false, false), (true, true), (true, false)] {
        let mut g = Graph::<A>::default();
        let c = cells(&mut g, &[num_or_ival(wide_x), num_or_ival(wide_y)]);
        let mut roots: Vec<NodeId> = ops.iter().map(|o| g.add(o.clone(), vec![c[0], c[1]])).collect();
        roots.extend(ops.iter().map(|o| g.add(o.clone(), vec![c[1], c[0]])));
        if !wide_x && !A::EQ_IS_IDENTITY {
            roots.push(g.add(Op::Same, vec![c[0], c[1]]));
        }
        let num = |v: A| In::N(v);
        let ival = |(lo, hi): (A, A)| In::I(lo, hi);
        let rows: Vec<Vec<In<A>>> = match (wide_x, wide_y) {
            (false, false) => pairs(&A::specials(), 1).into_iter().map(|(a, b)| vec![num(a), num(b)]).collect(),
            (true, true) => pairs(&intervals::<A>(), 5).into_iter().map(|(a, b)| vec![ival(a), ival(b)]).collect(),
            _ => intervals::<A>().into_iter().flat_map(|a| A::specials().into_iter().step_by(3).map(move |b| vec![ival(a), num(b)])).collect(),
        };
        check(&g, &roots, &rows, None, "cmp");
    }
}
both!(comparisons, comparisons_fixed, comparisons_float);

/// Products and quotients by a literal, either side, of numbers (any
/// literal) and intervals (a positive one), each with its own error; the
/// remainder by a literal.
fn products_and_quotients_by_a_literal<A: TestArith>() {
    for wide in [false, true] {
        let mut g = Graph::<A>::default();
        let x = cells(&mut g, &[num_or_ival(wide)])[0];
        let mut roots = Vec::new();
        for k in A::factors(wide) {
            let kn = lit(&mut g, k);
            for r in [g.add(Op::Mul, vec![x, kn]), g.add(Op::Mul, vec![kn, x]), g.add(Op::Div, vec![x, kn])] {
                roots.push(r);
                roots.push(g.add(Op::NoWrap, vec![r]));
            }
            if !wide {
                let r = g.add(Op::Rem, vec![x, kn]);
                roots.extend([r, g.add(Op::NoWrap, vec![r])]);
            }
        }
        let rows: Vec<Vec<In<A>>> = if wide {
            intervals::<A>().into_iter().map(|(a, b)| vec![In::I(a, b)]).collect()
        } else {
            A::specials().into_iter().map(|v| vec![In::N(v)]).collect()
        };
        check(&g, &roots, &rows, None, if wide { "iscale" } else { "scale" });
    }
}
both!(products_and_quotients_by_a_literal, products_and_quotients_by_a_literal_fixed, products_and_quotients_by_a_literal_float);

/// The tri-state layer with DECIDED planes (a maybe-unknown input, a bool
/// input, a comparison, the literals, an undecided constant) combined by
/// `And`/`Or`/`Eq`/`Not` and `Sel`: with the decided planes folded by bit
/// identities, every root equals the Kleene formulas on EVERY bit of both
/// planes, not only on the known lanes.
fn the_boolean_layer_is_bit_exact<A: TestArith>() {
    let mut g = Graph::<A>::default();
    let c = cells(&mut g, &[Ty::UBool, Ty::Bool, Ty::Num, Ty::Num]);
    let (u, b, x, y) = (c[0], c[1], c[2], c[3]);
    let cmp = g.add(Op::Lt, vec![x, y]);
    let t = g.leaf(Op::ConstBool(true));
    let f = g.leaf(Op::ConstBool(false));
    let k = g.leaf(Op::UnknownBool(Unknown::Atom(0)));
    let leaves = [u, b, cmp, t, f, k];
    let mut roots = Vec::new();
    for &p in &leaves {
        roots.push(g.add(Op::Not, vec![p]));
        roots.push(g.add(Op::Known, vec![p]));
        roots.push(g.add(Op::Sel, vec![p, x, y]));
        for &q in &leaves {
            for op in [Op::And, Op::Or, Op::Eq] {
                roots.push(g.add(op, vec![p, q]));
            }
            for &r in &leaves {
                roots.push(g.add(Op::Sel, vec![p, q, r]));
            }
        }
    }
    let mut rng = Lcg(0xdec1_dedd);
    let small = |rng: &mut Lcg| A::from_i16((rng.next_u64() % 4) as i16);
    let rows: Vec<Vec<In<A>>> = (0..256).map(|_| vec![In::U(rng.bit(), rng.bit()), In::B(rng.bit()), In::N(small(&mut rng)), In::N(small(&mut rng))]).collect();
    check(&g, &roots, &rows, None, "decided");
}
both!(the_boolean_layer_is_bit_exact, the_boolean_layer_is_bit_exact_fixed, the_boolean_layer_is_bit_exact_float);

/// The forks' fragments per configuration (`Frag`, `FragOk`, an `Ints` `Frag`),
/// their premise at every arity (`ForkOk`), the floor's premise; the
/// doubles' round fork. Around zeros, the floors' and the rounds' cut
/// points, and (16.16) the top floors, where the grid wraps.
fn fork_fragments<A: TestArith>() {
    let mut g = Graph::<A>::default();
    let x = cells(&mut g, &[Ty::Ival])[0];
    let ways = 3u8;
    let mut roots = Vec::new();
    for c in 0..ways {
        roots.push(g.add(Op::Frag(Cut::Floors, c), vec![x]));
        roots.push(g.add(Op::FragOk(Cut::Floors, c), vec![x]));
        roots.push(g.add(Op::Frag(Cut::Ints, c), vec![x]));
        if !A::ROUND_BY_SHIFTED_FLOOR {
            roots.push(g.add(Op::Frag(Cut::Rounds, c), vec![x]));
            roots.push(g.add(Op::FragOk(Cut::Rounds, c), vec![x]));
        }
    }
    for w in 1..=4 {
        roots.push(g.add(Op::ForkOk(Cut::Floors, w), vec![x]));
        if !A::ROUND_BY_SHIFTED_FLOOR {
            roots.push(g.add(Op::ForkOk(Cut::Rounds, w), vec![x]));
        }
    }
    let flr = g.add(Op::Flr, vec![x]);
    roots.push(g.add(Op::Known, vec![flr]));
    // Spans of up to four floors from every special, kept where they fit.
    let quarter = A::div(A::from_i16(1), A::from_i16(4)).expect("1/4");
    let spans: Vec<A> = [0, 1, 2, 3, 5, 7, 9, 12, 15].iter().map(|q| A::mul(A::from_i16(*q), quarter).expect("q/4")).collect();
    let mut rows: Vec<Vec<In<A>>> = Vec::new();
    for lo in A::specials() {
        for s in &spans {
            if let Some(hi) = A::add(lo, *s) {
                rows.push(vec![In::I(lo, hi)]);
            }
        }
    }
    let wants = check(&g, &roots, &rows, None, "fork");
    // Every fragment both reached and not.
    for c in 0..ways as usize {
        let ok: Vec<bool> = wants.iter().filter_map(|w| w[c * if A::ROUND_BY_SHIFTED_FLOOR { 3 } else { 5 } + 1].map(|v| as_bool(v).0)).collect();
        assert!(ok.contains(&true) && (c == 0 || ok.contains(&false)), "fragment {c}: reached {} of {}", ok.iter().filter(|b| **b).count(), ok.len());
    }
}
both!(fork_fragments, fork_fragments_fixed, fork_fragments_float);

/// A literal's `ForkOk` folds (`Graph::fold`) to what the kernel computes
/// on a lane holding the literal, wraps included.
fn a_literals_split_ok_folds_to_the_kernels<A: TestArith>() {
    let mut g = Graph::<A>::default();
    let x = cells(&mut g, &[Ty::Ival])[0];
    let roots: Vec<NodeId> = (1..=4).map(|w| g.add(Op::ForkOk(Cut::Floors, w), vec![x])).collect();
    let (c, l) = build(&g, &roots, "litsplit");
    let lits = intervals::<A>();
    for chunk in lits.chunks(16) {
        let rows: Vec<Vec<In<A>>> = chunk.iter().map(|(lo, hi)| vec![In::I(*lo, *hi)]).collect();
        let got = run::<A>(&c, &l, &rows, None);
        for ((lo, hi), got) in chunk.iter().zip(&got) {
            let k = g.leaf(Op::Const(A::lit(*lo), A::lit(*hi)));
            for (w, got) in (1..=4u8).zip(got) {
                let folded = g.fold(Op::ForkOk(Cut::Floors, w), vec![k]);
                assert_eq!(Out::B(g.get(folded).op == Op::ConstBool(true), true), *got, "ForkOk({w}) of [{lo:?}, {hi:?}]");
            }
        }
    }
}
both!(a_literals_split_ok_folds_to_the_kernels, a_literals_split_ok_folds_to_the_kernels_fixed, a_literals_split_ok_folds_to_the_kernels_float);

/// The map: `mget` (0 off the map) and `tile_flag_at` with a uniform box
/// and a per-lane one, call-outs into the real room, a result
/// feeding arithmetic.
fn collision_lookups<A: TestArith>() {
    let env = CollisionEnv { room: Room::of((1, 0)) };
    let mut g = Graph::<A>::default();
    let c = cells(&mut g, &[Ty::Num; 4]);
    let (w8, h6, flag0) = (lit(&mut g, A::from_i16(8)), lit(&mut g, A::from_i16(6)), lit(&mut g, A::from_i16(0)));
    let m = g.add(Op::Mget, vec![c[0], c[1]]);
    let roots = vec![
        m,
        g.add(Op::Add, vec![m, w8]),
        g.add(Op::TileFlagAt, vec![c[0], c[1], w8, h6, flag0]),
        g.add(Op::TileFlagAt, vec![c[0], c[1], c[2], c[3], flag0]),
    ];
    let mut rng = Lcg(0xc0111_5107);
    let mut coord = |lo: i64, n: u64| A::from_i16((lo + (rng.next_u64() % n) as i64) as i16);
    let rows: Vec<Vec<In<A>>> = (0..512).map(|_| vec![In::N(coord(-8, 144)), In::N(coord(-8, 80)), In::N(coord(1, 9)), In::N(coord(1, 9))]).collect();
    check(&g, &roots, &rows, Some(&env), "collision");
}
both!(collision_lookups, collision_lookups_fixed, collision_lookups_float);

/// Ice (flag 4) through a kernel: the call-out's precomputed player-hitbox
/// map answers the tile scan at every position of an ice room ((3,1)), and
/// is true somewhere.
fn the_ice_map_is_the_tile_scan<A: TestArith>() {
    let env = CollisionEnv { room: Room::of((3, 1)) };
    let p = |v: i16| P8::from_i16(v);
    let scan = |x: i16, y: i16, w: i16, h: i16| -> bool {
        (y.max(0) / 8..=((y + h - 1).div_euclid(8)).min(15)).any(|ty| {
            (x.max(0) / 8..=((x + w - 1).div_euclid(8)).min(15)).any(|tx| {
                let t = CART.mget(p(3 * 16 + tx), p(16 + ty)).expect("mget");
                CART.fget(p(t as i16), p(4)).expect("fget")
            })
        })
    };
    let mut g = Graph::<A>::default();
    let c = cells(&mut g, &[Ty::Num; 2]);
    let (w, h, flag) = (lit(&mut g, A::from_i16(6)), lit(&mut g, A::from_i16(5)), lit(&mut g, A::from_i16(4)));
    let roots = vec![g.add(Op::TileFlagAt, vec![c[0], c[1], w, h, flag])];
    let (k, l) = build(&g, &roots, "ice");
    let mut hits = 0;
    for y in -8..136i16 {
        for xs in (-8..136i16).collect::<Vec<_>>().chunks(16) {
            let rows: Vec<Vec<In<A>>> = xs.iter().map(|x| vec![In::N(A::from_i16(x + 1)), In::N(A::from_i16(y + 3))]).collect();
            let got = run::<A>(&k, &l, &rows, Some(&env));
            for (x, got) in xs.iter().zip(&got) {
                let want = scan(x + 1, y + 3, 6, 5);
                assert_eq!(got[0], Out::B(want, true), "ice at ({}, {})", x + 1, y + 3);
                hits += want as u32;
            }
        }
    }
    assert!(hits > 0, "room (3,1) has ice");
}
both!(the_ice_map_is_the_tile_scan, the_ice_map_is_the_tile_scan_fixed, the_ice_map_is_the_tile_scan_float);

/// The input layouts (by `Ty`): a boolean through `Not`/`Sel`, a
/// maybe-unknown boolean's own known mask, an interval through `+`/`flr`;
/// the slots' order and offsets.
fn input_representations<A: TestArith>() {
    let mut rng = Lcg(0x600d_cafe);
    let mut g = Graph::<A>::default();
    let c = cells(&mut g, &[Ty::Num, Ty::Bool, Ty::Num, Ty::UBool, Ty::Ival]);
    let (num, bool_, num2, ubool, ival) = (c[0], c[1], c[2], c[3], c[4]);
    let notb = g.add(Op::Not, vec![bool_]);
    let sum = g.add(Op::Add, vec![ival, num]);
    let roots = vec![
        g.add(Op::Sel, vec![bool_, num, num2]),
        g.add(Op::Sel, vec![notb, num2, num]),
        notb,
        bool_,
        g.add(Op::Known, vec![ubool]),
        ubool,
        sum,
        g.add(Op::Flr, vec![sum]),
        ival,
    ];
    let rows: Vec<Vec<In<A>>> = (0..64)
        .map(|_| {
            let lo = A::random(&mut rng, true);
            let hi = A::add(lo, A::abs(A::random(&mut rng, true)).expect("small")).expect("small");
            vec![In::N(A::random(&mut rng, false)), In::B(rng.bit()), In::N(A::random(&mut rng, false)), In::U(rng.bit(), rng.bit()), In::I(lo, hi)]
        })
        .collect();
    check(&g, &roots, &rows, None, "inputs");
    let (k, _) = build(&g, &roots, "inputs");
    use Ty::{Bool as B, Ival as I, Num as N};
    assert_eq!(k.root_kinds, vec![N, N, B, B, B, B, I, N, I]);
    assert_eq!(k.input_cells, vec![0, 1, 2, 3, 4]);
    assert_eq!(k.input_offsets, vec![0, 64, 128, 192, 256]);
    assert_eq!(k.input_bytes, 384);
}
both!(input_representations, input_representations_fixed, input_representations_float);

// 16.16's own: its `NoWrap` compares, the inline divisions, the call-outs'
// register saves, rematerialized constants.

/// SCALAR arithmetic's own error: with a literal operand the check is at
/// most two compares (`graph::one_operand`), none where every value fits;
/// two cells' `+`/`-` is one sign test. Every value and error against the
/// arithmetic, one step either side of every bound.
#[test]
fn scalar_overflow_is_at_most_two_compares() {
    let mut g = Graph::new();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    // (op, the literal: None for two cells / unary, the literal first)
    let mut forms: Vec<(Op, Option<i32>, bool)> = vec![(Op::Add, None, false), (Op::Sub, None, false), (Op::Neg, None, false), (Op::Abs, None, false)];
    for k in [0, 1, -1, 0x1_0000, -0x1_0000, 0x7fff_0000, i32::MAX, i32::MIN] {
        forms.extend([(Op::Add, Some(k), false), (Op::Sub, Some(k), false), (Op::Sub, Some(k), true)]);
    }
    for k in [0x2_0000, -0x2_0000, 0x8000, -0x1_0000, 0x7fff_0000] {
        forms.extend([(Op::Mul, Some(k), false), (Op::Mul, Some(k), true)]);
    }
    for k in [0x8000, -0x8000, 0x3_0000, -0x1_0000, 0x2_0000, 0] {
        forms.push((Op::Div, Some(k), false));
    }
    let mut ops = Vec::new();
    for (op, k, first) in &forms {
        let args = match k {
            None if matches!(op, Op::Neg | Op::Abs) => vec![x],
            None => vec![x, y],
            Some(k) => {
                let c = g.leaf(Op::Const(*k, *k));
                if *first { vec![c, x] } else { vec![x, c] }
            }
        };
        ops.push(g.add(op.clone(), args));
    }
    let oks: Vec<NodeId> = ops.iter().map(|&o| g.add(Op::NoWrap, vec![o])).collect();
    let roots: Vec<NodeId> = ops.iter().chain(&oks).copied().collect();
    // One compare per distinct bound a literal form puts on `x`, and one
    // sign test per two-cell `+`/`-`; nothing else compares.
    let text = compile(&g, &roots, "scalarovf", false).expect("compile").asm;
    let compares = text.lines().filter(|l| l.trim_start().starts_with("vpcmpd")).count();
    let mut bounds = std::collections::BTreeSet::new();
    for &o in &ops {
        if let Some(f) = one_operand(&g, o).and_then(|o| o.fits) {
            if f.0 != i32::MIN {
                bounds.insert((0, f.0));
            }
            if f.1 != i32::MAX {
                bounds.insert((1, f.1));
            }
        }
    }
    assert_eq!(compares, bounds.len() + 2, "{text}");
    // `x` at and one step either side of every bound, the extremes and small
    // values; `y` cycling through the extremes and small values.
    let mut xs: Vec<i32> = vec![i32::MIN, i32::MIN + 1, -1, 0, 1, 0x1_0000, i32::MAX - 1, i32::MAX];
    for &(_, b) in &bounds {
        xs.extend([b.saturating_sub(1), b, b.saturating_add(1)]);
    }
    let ys = [i32::MAX, i32::MIN, 1, -1, 0, 0x7fff_0000, -0x7fff_0000, 0x1_0000, i32::MAX - 1, i32::MIN + 1, 3, -0x4000_0000, 0x4000_0000];
    let rows: Vec<Vec<In<Fixed>>> = xs.iter().enumerate().map(|(i, x)| vec![In::N(P8::from_raw(*x)), In::N(P8::from_raw(ys[i % ys.len()]))]).collect();
    let wants = check(&g, &roots, &rows, None, "scalarovf");
    for (f, form) in forms.iter().enumerate() {
        let fits: Vec<bool> = wants.iter().filter_map(|w| w[forms.len() + f].map(|v| as_bool(v).0)).collect();
        let everywhere = one_operand(&g, ops[f]).is_some_and(|o| o.fits == Some((i32::MIN, i32::MAX)));
        assert!(fits.contains(&true) || form.0 == Op::Div && form.1 == Some(0), "{form:?}: some lane fits");
        assert_eq!(!fits.contains(&false), everywhere, "{form:?}: overflows somewhere unless it fits everywhere");
    }
}

/// Interval `+`, `-` and negation whose endpoints OVERFLOW the 16.16 range
/// are an ERROR in that lane, never a value (a silently wrapped endpoint
/// inverts the interval): `NoWrap` is false exactly where the checked
/// operation fails, and every other lane is the exact result.
#[test]
fn interval_overflow_declines() {
    let mut g = Graph::new();
    let (civ, cnum) = (cells(&mut g, &[Ty::Ival, Ty::Num])[0], g.leaf(Op::Cell(1)));
    let add = g.add(Op::Add, vec![civ, cnum]);
    let sub = g.add(Op::Sub, vec![civ, cnum]);
    let neg = g.add(Op::Neg, vec![civ]);
    let ok: Vec<NodeId> = [add, sub, neg].iter().map(|&x| g.add(Op::NoWrap, vec![x])).collect();
    let roots = vec![add, sub, neg, ok[0], ok[1], ok[2]];
    let one = 0x1_0000;
    // Per lane (lo, hi, num): the whole range plus/minus one, a top end that
    // overflows on +1, a bottom end on -1, MIN negated, and lanes right up to
    // the ends without overflowing.
    let lanes: [(i32, i32, i32); 16] = [
        (i32::MIN, i32::MAX, one),
        (i32::MIN, i32::MAX, -one),
        (i32::MAX - 5, i32::MAX, one),
        (i32::MIN, i32::MIN + 5, one),
        (i32::MIN, 0, 0),
        (-one, one, one),
        (0, 3 * one, -one),
        (i32::MIN + one, i32::MAX - one, one),
        (i32::MIN + one, i32::MAX - one, -one),
        (5, 9, 2),
        (i32::MAX, i32::MAX, 1),
        (i32::MIN, i32::MIN, -1),
        (-7 * one, -6 * one, 4 * one),
        (100, 200, 300),
        (i32::MIN + 1, i32::MIN + 1, 0),
        (0, 0, 0),
    ];
    let p = P8::from_raw;
    let rows: Vec<Vec<In<Fixed>>> = lanes.iter().map(|&(lo, hi, n)| vec![In::I(p(lo), p(hi)), In::N(p(n))]).collect();
    let wants = check(&g, &roots, &rows, None, "ivalovf");
    // What the test is about: exactly these lanes overflow.
    let wraps = |k: usize| wants.iter().enumerate().fold(0u16, |m, (l, w)| m | (!as_bool(w[3 + k].expect("decided")).0 as u16) << l);
    assert_eq!(wraps(0), 0x0c07, "add: the whole range +- 1, a top end past MAX, MAX + eps, MIN - eps");
    assert_eq!(wraps(1), 0x000b, "sub: the whole range -+ 1, a bottom end past MIN");
    assert_eq!(wraps(2), 0x081b, "neg: every lane with an endpoint at MIN");
}

/// Interval `*` and `/` by a positive literal: `NoWrap` false exactly where
/// the checked operation fails, one ulp either side of each literal's
/// edges, every other lane exact. A literal that cannot carry an endpoint
/// out (a factor at most 1, a divisor at least 1) has a constant-true
/// `NoWrap`: no compare.
#[test]
fn interval_scale_and_divide_overflow_is_one_compare_a_bound() {
    let fits = |g: &Graph, x: NodeId| one_operand(g, x).expect("one operand").fits.expect("fits somewhere");
    let mut g = Graph::new();
    let civ = cells(&mut g, &[Ty::Ival])[0];
    // (op, raw literal, the literal first, can wrap)
    let forms: [(Op, i32, bool, bool); 10] = [
        (Op::Mul, 0x2_0000, false, true),
        (Op::Mul, 0x3_0000, true, true),
        (Op::Mul, 0x1_8000, false, true),
        (Op::Mul, 0x1_0001, false, true),
        (Op::Mul, 0x8000, false, false),
        (Op::Div, 0x8000, false, true),
        (Op::Div, 0x4000, false, true),
        (Op::Div, 0xffff, false, true),
        (Op::Div, 0x3_0000, false, false),
        (Op::Div, 0x2_0000, false, false), // inline `div_pow2`
    ];
    let mut ops = Vec::new();
    for (op, k, first, _) in &forms {
        let c = g.leaf(Op::Const(*k, *k));
        ops.push(g.add(op.clone(), if *first { vec![c, civ] } else { vec![civ, c] }));
    }
    let oks: Vec<NodeId> = ops.iter().map(|&x| g.add(Op::NoWrap, vec![x])).collect();
    let roots: Vec<NodeId> = ops.iter().chain(&oks).copied().collect();
    let text = compile(&g, &roots, "ivalscale", false).expect("compile").asm;
    let compares = text.lines().filter(|l| l.trim_start().starts_with("vpcmpd")).count();
    // One per distinct bound a literal can cross (`*2` and `/0.5` share
    // theirs, `*1.00002` and `/0.99998` their low one), none for the rest.
    let mut bounds = std::collections::BTreeSet::new();
    for (f, &x) in ops.iter().enumerate() {
        let (lo, hi) = fits(&g, x);
        assert_eq!(lo != i32::MIN && hi != i32::MAX, forms[f].3, "{:?}: both ends or neither", forms[f]);
        if lo != i32::MIN {
            bounds.insert((0, lo));
        }
        if hi != i32::MAX {
            bounds.insert((1, hi));
        }
    }
    assert_eq!(compares, bounds.len(), "one compare per bound:\n{text}");
    // Lanes around every literal's edges, then the extremes and small values.
    let mut lanes: Vec<(i32, i32)> = vec![(i32::MIN, i32::MAX), (0, 0), (-0x10_0000, 0x10_0000), (i32::MIN, 0), (0, i32::MAX)];
    for &x in &ops {
        let (lo, hi) = fits(&g, x);
        for d in [-1i64, 0, 1] {
            let (l, h) = ((lo as i64 + d).clamp(i32::MIN as i64, 0) as i32, (hi as i64 + d).clamp(0, i32::MAX as i64) as i32);
            lanes.extend([(l, 0), (0, h), (l, h), (l, l.saturating_add(3)), (h.saturating_sub(3), h)]);
        }
    }
    let rows: Vec<Vec<In<Fixed>>> = lanes.iter().map(|&(lo, hi)| vec![In::I(P8::from_raw(lo), P8::from_raw(hi))]).collect();
    let wants = check(&g, &roots, &rows, None, "ivalscale");
    for (f, (op, k, _, can)) in forms.iter().enumerate() {
        let fits: Vec<bool> = wants.iter().map(|w| as_bool(w[forms.len() + f].expect("decided")).0).collect();
        assert!(fits.contains(&true) && fits.contains(&false) == *can, "{op:?} {k:#x}: fits {} of {}", fits.iter().filter(|b| **b).count(), fits.len());
    }
}

/// `/ 2^s` and `% 2^m` by a literal are INLINE shifts and masks, not
/// call-outs, and exact over the whole i32 range: the edges (MIN, MAX, 0,
/// +-1, every +-2^k and its neighbours) and random raws; for the interval
/// `/`, both endpoints.
#[test]
fn inline_div_rem_by_a_power_of_two_is_pico8s() {
    let mut g = Graph::new();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    let iv = g.add(Op::Span, vec![x, y]);
    let mut roots = Vec::new();
    for s in 0..=14u32 {
        let d = 1i32 << (16 + s);
        let c = g.leaf(Op::Const(d, d));
        roots.push(g.add(Op::Div, vec![x, c]));
        roots.push(g.add(Op::Div, vec![iv, c]));
    }
    for m in 0..=30u32 {
        let d = 1i32 << m;
        let c = g.leaf(Op::Const(d, d));
        roots.push(g.add(Op::Rem, vec![x, c]));
    }
    let text = compile(&g, &roots, "pow2", false).expect("compile").asm;
    assert!(!text.contains("call"), "a power-of-two divisor is inline");
    let mut edges: Vec<i32> = vec![i32::MIN, i32::MIN + 1, i32::MAX, i32::MAX - 1, 0, 1, -1];
    for k in 0..31 {
        for d in [-1i32, 0, 1] {
            edges.push((1i32 << k).wrapping_add(d));
            edges.push((-(1i64 << k) as i32).wrapping_add(d));
        }
    }
    let mut rng = Lcg(0x0d1f_2e3d);
    edges.extend((0..1024).map(|_| rng.i32()));
    // The interval's high end: the low end plus a random non-negative span.
    let rows: Vec<Vec<In<Fixed>>> = edges
        .iter()
        .map(|x| vec![In::N(P8::from_raw(*x)), In::N(P8::from_raw(x.saturating_add((rng.i32() & 0x7fff_ffff) >> (rng.i32() & 31))))])
        .collect();
    let wants = check(&g, &roots, &rows, None, "pow2");
    assert!(wants.iter().all(|w| w.iter().all(Option::is_some)), "a power-of-two quotient or remainder always fits");
}

/// The call-outs of 16.16 (`Div`, `Rem`, `Sin`): by a literal that is no
/// power of two, of two cells (the console's saturating `/`), a result
/// feeding arithmetic.
#[test]
fn call_outs_are_the_arithmetics() {
    let mut g = Graph::new();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    let three = g.leaf(Op::Const(3 << 16, 3 << 16));
    let mut roots = Vec::new();
    for d in [three, y] {
        let (q, r) = (g.add(Op::Div, vec![x, d]), g.add(Op::Rem, vec![x, d]));
        roots.extend([q, r, g.add(Op::Add, vec![q, r])]);
    }
    roots.push(g.add(Op::Sin, vec![x]));
    let mut rng = Lcg(0xca11_0075);
    let mut rows: Vec<Vec<In<Fixed>>> = pairs(&Fixed::specials(), 3).into_iter().map(|(a, b)| vec![In::N(a), In::N(b)]).collect();
    rows.extend((0..256).map(|_| vec![In::N(Fixed::random(&mut rng, true)), In::N(Fixed::random(&mut rng, true))]));
    check(&g, &roots, &rows, None, "callout");
}

/// Only the registers live across a call-out are saved and restored: a value
/// computed before a call and read after it survives it, fewer than all 32
/// are saved, and `ZERO` is zero again after the call.
#[test]
fn a_call_out_preserves_what_is_live_across_it() {
    let mut g = Graph::new();
    let (x, y) = (g.leaf(Op::Cell(0)), g.leaf(Op::Cell(1)));
    let three = g.leaf(Op::Const(3 << 16, 3 << 16));
    let before = g.add(Op::Add, vec![x, y]); // live across the call
    let q = g.add(Op::Div, vec![before, three]); // a call-out, after `before`
    let n = g.add(Op::Neg, vec![q]); // reads ZERO after the call
    let roots = vec![g.add(Op::Add, vec![before, n]), before];
    let text = compile(&g, &roots, "livecall", false).expect("compile").asm;
    // Stores to the stack: the call's two arguments and the saves.
    let stores = text.lines().filter(|l| l.contains("vmovdqu64 %zmm") && l.contains("(%rsp)")).count();
    assert!(text.contains("call") && (3..32).contains(&stores), "{stores} stack stores around the call");
    let mut rng = Lcg(0x11fe_ca11);
    let rows: Vec<Vec<In<Fixed>>> = (0..256).map(|_| vec![In::N(Fixed::random(&mut rng, true)), In::N(Fixed::random(&mut rng, true))]).collect();
    check(&g, &roots, &rows, None, "livecall");
}

/// Values live across a RUN of call-outs and read only after it stay in the
/// save area between the calls (eight call-outs, four values held across).
#[test]
fn values_held_across_a_run_of_calls_survive_it() {
    let mut g = Graph::new();
    let cs: Vec<NodeId> = (0..8u32).map(|c| g.leaf(Op::Cell(c))).collect();
    let three = g.leaf(Op::Const(3 << 16, 3 << 16));
    let held: Vec<NodeId> = (0..4).map(|i| g.add(Op::Add, vec![cs[i], cs[i + 4]])).collect();
    // Each call reads one held value, so all four are defined first.
    let qs: Vec<NodeId> = (0..8)
        .map(|i| {
            let arg = g.add(Op::Sub, vec![held[i % 4], cs[i]]);
            g.add(Op::Div, vec![arg, three])
        })
        .collect();
    let roots: Vec<NodeId> = held.iter().chain(&qs).copied().collect();
    let text = compile(&g, &roots, "callrun", false).expect("compile").asm;
    assert_eq!(text.matches("call *%rax").count(), 8, "eight call-outs");
    let mut rng = Lcg(0x5a7e_0a11);
    let rows: Vec<Vec<In<Fixed>>> = (0..256).map(|_| (0..8).map(|_| In::N(Fixed::random(&mut rng, true))).collect()).collect();
    check(&g, &roots, &rows, None, "callrun");
}

/// Constants under register pressure: 48 select chains over broadcast
/// constants, still live at the end, are spilled and REMATERIALIZED at their
/// uses, exact.
#[test]
fn rematerialized_constants_under_pressure_are_exact() {
    let mut g = Graph::new();
    let cs: Vec<NodeId> = (0..48u32).map(|c| g.leaf(Op::Cell(c))).collect();
    let ks: Vec<NodeId> = [-1i32, 0, 1, 0x10000, -0x8000, 7, 0x7fff_ffff, i32::MIN].iter().map(|&k| g.leaf(Op::Const(k, k))).collect();
    let firsts: Vec<NodeId> = (0..cs.len())
        .map(|i| {
            let lt = g.add(Op::Lt, vec![cs[i], cs[(i + 1) % cs.len()]]);
            g.add(Op::Sel, vec![lt, ks[i % ks.len()], ks[(i + 3) % ks.len()]])
        })
        .collect();
    // Their minimum, then every first select again beside more constants:
    // all 48 are live until the minimum is known.
    let min = firsts.iter().skip(1).fold(firsts[0], |m, &s| g.add(Op::Min, vec![m, s]));
    let mut roots = vec![min];
    for (i, &s) in firsts.iter().enumerate() {
        let ge = g.add(Op::Ge, vec![cs[i], min]);
        roots.push(g.add(Op::Sel, vec![ge, ks[(i + 5) % ks.len()], s]));
    }
    let text = compile(&g, &roots, "remat", false).expect("compile").asm;
    let consts = text.lines().filter(|l| l.contains("vpbroadcastd") || l.contains("vpternlogd $0xff") || l.trim_start().starts_with("vpxord")).count();
    assert!(text.contains("(%rsp)") && consts > ks.len(), "the test needs spilled constants: {consts} materializations");
    let mut rng = Lcg(0x4e3a_7a7e);
    let rows: Vec<Vec<In<Fixed>>> = (0..64).map(|_| (0..48).map(|_| In::N(Fixed::random(&mut rng, false))).collect()).collect();
    check(&g, &roots, &rows, None, "remat");
}

// The doubles' own.

/// Products, quotients and remainders of two cells (numbers; intervals: the
/// least and most corner in the total order), each with its own error.
#[test]
fn float_products_and_quotients_of_two_cells() {
    for wide in [false, true] {
        let mut g = Graph::<Float>::default();
        let c = cells(&mut g, &[num_or_ival(wide); 2]);
        let mut roots = Vec::new();
        for op in [Op::Mul, Op::Div].into_iter().chain((!wide).then_some(Op::Rem)) {
            let r = g.add(op, vec![c[0], c[1]]);
            roots.extend([r, g.add(Op::NoWrap, vec![r])]);
        }
        let rows: Vec<Vec<In<Float>>> = if wide {
            pairs(&intervals::<Float>(), 9).into_iter().map(|(a, b)| vec![In::I(a.0, a.1), In::I(b.0, b.1)]).collect()
        } else {
            pairs(&Float::specials(), 1).into_iter().map(|(a, b)| vec![In::N(a), In::N(b)]).collect()
        };
        check(&g, &roots, &rows, None, if wide { "fiprod" } else { "fprod" });
    }
}

/// `min`/`max` take the SECOND argument on a tie (LuaJIT's): `min(0, -0)`
/// is `-0`, `min(-0, 0)` is `0`.
#[test]
fn float_min_max_take_the_second_argument_on_a_tie() {
    let mut g = Graph::<Float>::default();
    let c = cells(&mut g, &[Ty::Num; 2]);
    let roots = vec![g.add(Op::Min, vec![c[0], c[1]]), g.add(Op::Max, vec![c[0], c[1]])];
    let z = |v: f64| In::N(LuaF64::new(v).expect("finite"));
    let wants = check(&g, &roots, &[vec![z(0.0), z(-0.0)], vec![z(-0.0), z(0.0)]], None, "ftie");
    for (w, second) in wants.iter().zip([-0.0f64, 0.0]) {
        for v in w {
            assert_eq!(as_point(v.expect("defined")).value().to_bits(), second.to_bits());
        }
    }
}

/// Literal operands (a broadcast constant), and a sum the cart rounds:
/// `0.1 + 0.2` is not `0.3`, and no fused multiply-add joins `x * y + z`.
#[test]
fn float_literals_and_rounding_are_the_carts() {
    let mut g = Graph::<Float>::default();
    let c = cells(&mut g, &[Ty::Num; 2]);
    let tenth = lit(&mut g, LuaF64::new(0.1).expect("finite"));
    let p = g.add(Op::Mul, vec![c[0], c[1]]);
    let roots = vec![g.add(Op::Add, vec![c[0], tenth]), g.add(Op::Add, vec![p, tenth])];
    let third = LuaF64::new(1.0 / 3.0).expect("finite");
    let rows: Vec<Vec<In<Float>>> = Float::specials().into_iter().filter(|v| v.value().abs() < 1e100).map(|v| vec![In::N(v), In::N(third)]).collect();
    check(&g, &roots, &rows, None, "flit");
}

/// A kernel text holding a fused multiply-add or an MXCSR write is refused
/// before it is assembled.
#[test]
fn a_fused_multiply_add_is_refused() {
    for text in ["    vfmadd231pd %zmm1, %zmm2, %zmm3\n", "    vfnmsub213sd %xmm1, %xmm2, %xmm3\n", "    ldmxcsr (%rsp)\n"] {
        let asm = format!(".text\n.globl k\nk:\n{text}    ret\n");
        let e = crate::transpile::asm::assemble(&asm, "refuse").expect_err("refused");
        assert!(format!("{e}").contains("forbidden"), "{e}");
    }
}
