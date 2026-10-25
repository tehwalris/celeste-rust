//! THE REMAINDER MODEL of an arithmetic (plans/float.md "The remainder
//! model and the round fork"): the CIRCLE its remainders are points of
//! (`Rem::C`, `search::arcs`), THE DECODE of a frame's captures into an
//! edge's transfer (`decode`: one rule for both arithmetics), and the
//! transfer's RECORD as the tree's table stores it (`Rem::Axis`: 16.16's
//! `AxisXfer`, the doubles' `AxisF64`; a pair `Pair`). The forward and the
//! storage are generic over it.

use std::fmt::Debug;
use std::hash::Hash;

use celeste_core::arith::Arithmetic;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::mix64;

use super::arcs::{self, Act, Circle, Doubles, Fixed16, Seg, Shift, Transfer, CIRCLE};
use super::hull::{Hull, HullOf};

/// A CIRCLE'S STORED WORD (`Circle::W`: arena words, hull bounds; a
/// point or a count): 16.16's `u16`, the doubles' `u64`.
pub trait Word: Copy + Ord + std::hash::Hash + std::fmt::Debug + Default + Send + Sync + 'static {
    /// Its little-endian bytes on disk.
    const BYTES: usize;
    /// A number (a point, or a count at most `SIZE` or `MAX_SEGS`) as a
    /// word, and back.
    fn of(v: u64) -> Self;
    fn get(self) -> u64;
    fn put(self, out: &mut Vec<u8>);
    fn read(b: &[u8]) -> Self;
    /// The hash of a hull's four words (statistics, fingerprints, the push
    /// cache's slot).
    fn hash4(w: [Self; 4]) -> u64;
    /// Are a hull's four words equal (`PushCache::get`)?
    fn same4(a: [Self; 4], b: [Self; 4]) -> bool;
}

impl Word for u16 {
    const BYTES: usize = 2;
    #[inline]
    fn of(v: u64) -> u16 {
        v as u16
    }
    #[inline]
    fn get(self) -> u64 {
        self as u64
    }
    fn put(self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.to_le_bytes());
    }
    fn read(b: &[u8]) -> u16 {
        u16::from_le_bytes([b[0], b[1]])
    }
    /// The four words in one, low first: exact.
    #[inline]
    fn hash4(w: [u16; 4]) -> u64 {
        w[0] as u64 | (w[1] as u64) << 16 | (w[2] as u64) << 32 | (w[3] as u64) << 48
    }
    /// In one 8-byte compare (field by field cost the push cache's lookup
    /// 2.6% of a big room's frame).
    #[inline]
    fn same4(a: [u16; 4], b: [u16; 4]) -> bool {
        a == b
    }
}

impl Word for u64 {
    const BYTES: usize = 8;
    #[inline]
    fn of(v: u64) -> u64 {
        v
    }
    #[inline]
    fn get(self) -> u64 {
        self
    }
    fn put(self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.to_le_bytes());
    }
    fn read(b: &[u8]) -> u64 {
        u64::from_le_bytes(b[..8].try_into().expect("8 bytes"))
    }
    #[inline]
    fn hash4(w: [u64; 4]) -> u64 {
        use celeste_engine::runtime2::mix64;
        mix64(mix64(w[0] ^ w[1].rotate_left(32)) ^ w[2] ^ w[3].rotate_left(32))
    }
    /// Word by word (a miss differs early).
    #[inline]
    fn same4(a: [u64; 4], b: [u64; 4]) -> bool {
        a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3]
    }
}

/// One axis of what a frame did to the player's remainder, captured at its
/// split (kernels and reference engine): whether this path took it, its
/// argument `pre` and fragment `frag` (16.16: the floor split of `rem + ox +
/// 1/2`; doubles: the round split of `rem + ox`), the speed APPLIED `ox`
/// (not always the stored one), the remainder at the frame's end `fin`
/// before the boundary widening (`None`: no player).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Capture<N> {
    pub(crate) took: bool,
    pub(crate) pre: (N, N),
    pub(crate) frag: (N, N),
    pub(crate) ox: (N, N),
    pub(crate) fin: Option<(N, N)>,
}

pub trait Rem: Arithmetic {
    /// The circle the remainders are points of (hulls, the arcs' sets).
    type C: Circle;
    /// One axis's transfer as the tree's table (`edges/xfer.bin`) holds it.
    /// Its `Ord` orders a slice's new pairs (`XferTable::merge`: the
    /// transfer ids), so it is FROZEN, as its bytes are.
    type Axis: Copy + Debug + Eq + Hash + Ord + Send + Sync + 'static;
    /// Bytes a record takes on disk; a pair, two.
    const AXIS_BYTES: usize;
    const PAIR_BYTES: usize = 2 * Self::AXIS_BYTES;

    /// A remainder as a point of the circle (`None`: not in `[-1/2, 1/2)`).
    fn rem_point(v: Self) -> Option<Point<Self>>;
    /// The remainder at point `p` (`rem_point`'s inverse).
    fn at_point(p: Point<Self>) -> Self;
    /// One axis's guard and action (`decode`'s) as its record.
    fn record(guard: Seg<Point<Self>>, act: Action<Self>) -> Self::Axis;
    /// `decode`'s guard: the points (as numbers, `[lo, hi)`) whose
    /// `fl(r + ox)` is in the round split's fragment `frag`, the circle's
    /// ends moving by `ox` without overflow. The search with the operation
    /// (`guard`), or the arithmetic's closed form: the same points (tested).
    #[inline]
    fn guard(frag: (Self, Self), ox: Self) -> (u64, u64) {
        guard(frag, ox)
    }
    /// A record as the pushes and pulls read it.
    fn transfer(a: &Self::Axis) -> Transfer<Self::C>;
    /// Append `a`'s `AXIS_BYTES`; and back.
    fn encode(out: &mut Vec<u8>, a: &Self::Axis);
    fn decode(b: &[u8]) -> Self::Axis;
    /// A record's hash (`pair_hash`'s part).
    fn axis_hash(a: &Self::Axis) -> u64;

    /// A pair as the pushes and pulls read it.
    #[inline]
    fn xfer(p: &Pair<Self>) -> Xfer<Self> {
        (Self::transfer(&p.0), Self::transfer(&p.1))
    }
    /// Append `p`'s `PAIR_BYTES`; and back.
    fn encode_pair(out: &mut Vec<u8>, p: &Pair<Self>) {
        Self::encode(out, &p.0);
        Self::encode(out, &p.1);
    }
    fn decode_pair(b: &[u8]) -> Pair<Self> {
        (Self::decode(&b[..Self::AXIS_BYTES]), Self::decode(&b[Self::AXIS_BYTES..Self::PAIR_BYTES]))
    }
    /// A pair's hash (the transfer table's index).
    #[inline]
    fn pair_hash(p: &Pair<Self>) -> u64 {
        mix64(Self::axis_hash(&p.0) ^ Self::axis_hash(&p.1).rotate_left(31))
    }
}

/// An edge's transfer pair (x, y) as one edge record's id refers to it.
pub type Pair<N> = (<N as Rem>::Axis, <N as Rem>::Axis);

/// What a frame does to one axis's remainder on its guard (`decode`):
/// `r -> fl(fl(r + ox) - k)` (`move` after the round split; 16.16's a
/// rotation), or the constant point a blocked step or a new player set.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action<N: Rem> {
    Mono { ox: N, k: i32 },
    Const(Point<N>),
}

/// THE DECODE (plans/float.md "The remainder model and the round fork") of
/// one frame's captures, per axis its guard and action, recorded. `move` is
/// `r1 = fl(r + ox)`, `k = flr(fl(r1 + 1/2))`, `r' = fl(r1 - k)`, forked by
/// the round split of `r1` over the node's whole circle. ONE RULE for both
/// arithmetics, in their own operations only (`add`, `sub`, `flr`,
/// `round_fragment`): every comparison BY VALUE (`num_eq`, `le`, `lt`; `-0`
/// is `0`, as every read of a remainder has it), each operation in the
/// cart's operand order (`r + ox`, `r1 - k`). Per axis:
///
/// * No split (a freeze, the spawn): the whole circle; the identity with no
///   player at the end or `fin` the whole circle, `Const(fin)` with `fin` a
///   point (a player made this frame).
/// * A split of ONE point: the remainder was SET before the move (a
///   platform's carry blocked by a wall: `move_x` sets `rem.x = 0` without
///   reading it). Only `move` reads the input remainder, so every one takes
///   the edge: the whole circle, `Const(fin)` (no player at the end: the
///   identity); the point must be some remainder plus the speed.
/// * A split: `ox` a point, the split's argument exactly the circle shifted
///   by it, the fragment the WHOLE piece of one amount `k` (its round
///   fragment); the guard is the points `r` with `fl(r + ox)` in it (an
///   order-interval, `fl(r + ox)` monotone: found exactly by a search with
///   the operation itself, `first`); the action `Mono(ox, k)` where `fin` is its
///   image `fl(frag - k)` (or no player ends the frame, a death: every
///   transfer out of a player-less node has a whole guard, so its winning
///   set is all or nothing and any action into it is exact), `Const(fin)`
///   where `fin` is another point (a blocked step reset it).
///
/// 16.16's split is the floor fork of `r1 + 1/2` (`ROUND_BY_SHIFTED_FLOOR`),
/// its captures shifted back by 1/2 first (exact). Anything else is
/// REFUSED, FATAL in the forward: the record would be an unchecked claim
/// about the remainder.
pub(crate) fn decode<N: Rem>(x: &Capture<N>, y: &Capture<N>) -> Result<Pair<N>, String> {
    let axis = |c: &Capture<N>, name: &str| decode_axis(c).map(|(g, a)| N::record(g, a)).map_err(|e| format!("axis {name} ({c:?}): {e}"));
    Ok((axis(x, "x")?, axis(y, "y")?))
}

/// One axis of `decode`: its guard and action, or why it is refused (the
/// capture's own values are the message's: no formatting on the way).
pub(crate) fn decode_axis<N: Rem>(c: &Capture<N>) -> Result<(Seg<Point<N>>, Action<N>), &'static str> {
    let same = |a: (N, N), b: (N, N)| N::num_eq(a.0, b.0) && N::num_eq(a.1, b.1);
    let is_point = |v: (N, N)| N::num_eq(v.0, v.1);
    let whole = Seg { lo: N::C::narrow(0), hi: N::C::SIZE };
    let identity = Action::Mono { ox: N::from_i16(0), k: 0 };
    let konst = |guard, v: N| N::rem_point(v).map(|p| (guard, Action::Const(p))).ok_or("a constant final remainder outside [-1/2, 1/2)");
    let (bottom, top) = (N::at_point(N::C::narrow(0)), N::at_point(N::C::prev(N::C::SIZE)));
    if !c.took {
        return match c.fin {
            None => Ok((whole, identity)),
            Some(f) if same(f, (bottom, top)) => Ok((whole, identity)),
            Some(f) if is_point(f) => konst(whole, f.0),
            Some(_) => Err("no player split, but the final remainder is neither the input circle nor a point"),
        };
    }
    if !is_point(c.ox) {
        return Err("the applied speed is not exact");
    }
    let ox = c.ox.0;
    let half = N::from_fixed_raw(1 << 15);
    let (pre, frag) = if N::ROUND_BY_SHIFTED_FLOOR {
        let back = |v: (N, N)| N::sub(v.0, half).zip(N::sub(v.1, half)).ok_or("the split's argument or fragment less 1/2 overflows");
        (back(c.pre)?, back(c.frag)?)
    } else {
        (c.pre, c.frag)
    };
    let shifted = N::add(bottom, ox).zip(N::add(top, ox)).ok_or("the remainders plus the speed overflow")?;
    if is_point(pre) {
        if is_point(shifted) {
            return Err("the speed maps every remainder to one point: a reset and a move cannot be told apart");
        }
        if !same(frag, pre) {
            return Err("the fragment is not the split's one-point argument");
        }
        let p = first::<N>(near(pre.0, ox), |p| N::le(pre.0, moved_at(p, ox)));
        if p == N::C::wide(N::C::SIZE) || !N::num_eq(moved_at(p, ox), pre.0) {
            return Err("the split's one-point argument is no remainder in [-1/2, 1/2) plus the speed");
        }
        return match c.fin {
            None => Ok((whole, identity)),
            Some(f) if is_point(f) => konst(whole, f.0),
            Some(_) => Err("the remainder was set before the move, but the final remainder is not a point"),
        };
    }
    if !same(pre, shifted) {
        return Err("the split's argument is not the remainders shifted by the speed (the remainder at the move was not [-1/2, 1/2))");
    }
    if !(N::le(pre.0, frag.0) && N::le(frag.0, frag.1) && N::le(frag.1, pre.1)) {
        return Err("the fragment is not inside the split's argument");
    }
    let round = |v: N| N::add(v, half).map(N::flr).ok_or("a fragment plus 1/2 overflows");
    let k = round(frag.0)?;
    if !N::num_eq(round(frag.1)?, k) {
        return Err("the fragment spans two amounts");
    }
    // The amount's index among the argument's: integers, exactly.
    let nth = N::to_f64(k) - N::to_f64(round(pre.0)?);
    let piece = N::round_fragment(N::ival(pre.0, pre.1), nth as u32).map_err(|_| "the split's argument has no round fragment of the amount")?;
    if !same(frag, N::bounds(piece)) {
        return Err("the fragment is not the whole piece of its amount (its round fragment)");
    }
    let (lo, hi) = N::guard(frag, ox);
    if lo >= hi {
        return Err("no remainder lands in the fragment at the speed");
    }
    let guard = Seg { lo: N::C::narrow(lo), hi: N::C::narrow(hi) };
    let image = match (N::sub(frag.0, k), N::sub(frag.1, k)) {
        (Some(a), Some(b)) if N::rem_point(a).is_some() && N::rem_point(b).is_some() => (a, b),
        _ => return Err("the fragment's image is not in [-1/2, 1/2)"),
    };
    let kf = N::to_f64(k);
    if kf as i32 as f64 != kf {
        return Err("the amount is not an i32");
    }
    let mono = Action::Mono { ox, k: kf as i32 };
    match c.fin {
        None => Ok((guard, mono)),
        Some(f) if same(f, image) => Ok((guard, mono)),
        Some(f) if is_point(f) => konst(guard, f.0),
        Some(_) => Err("the final remainder is neither the fragment's image nor a point"),
    }
}

/// `Rem::guard` by the search with the operation itself (`first`).
pub(crate) fn guard<N: Rem>(frag: (N, N), ox: N) -> (u64, u64) {
    (first::<N>(near(frag.0, ox), |p| N::le(frag.0, moved_at(p, ox))), first::<N>(near(frag.1, ox), |p| N::lt(frag.1, moved_at(p, ox))))
}

/// `fl(r + ox)` of the remainder `r` at point `p` (`p < SIZE`), where the
/// circle's ends move by `ox` without overflow (so every point between
/// them does: the addition is monotone).
#[inline]
fn moved_at<N: Rem>(p: u64, ox: N) -> N {
    N::add(N::at_point(N::C::narrow(p)), ox).expect("between the moved ends: no overflow")
}

/// Where `fl(r + ox)` reaches `v`, about: the point of `v - ox` (16.16:
/// exactly), else the circle's end it is past.
fn near<N: Rem>(v: N, ox: N) -> u64 {
    match N::sub(v, ox) {
        Some(r) => match N::rem_point(r) {
            Some(p) => N::C::wide(p),
            None if N::lt(r, N::from_i16(0)) => 0,
            None => N::C::wide(N::C::SIZE),
        },
        None => N::C::wide(N::C::SIZE) / 2,
    }
}

/// The first point of `N`'s circle (as a number) from which the monotone
/// `p` (false, then true) holds, `SIZE` where none does: from the estimate
/// `at`, galloping until it is bracketed, then bisecting. Exact whatever
/// the estimate (a monotone `p` has one first point), a few evaluations for
/// a close one (`near`'s: 16.16's is the point; a double's within ulps).
fn first<N: Rem>(at: u64, p: impl Fn(u64) -> bool) -> u64 {
    let size = N::C::wide(N::C::SIZE);
    let p = |x: u64| x >= size || p(x);
    // Every point below `lo` fails; `hi` holds.
    let (mut lo, mut hi, mut step) = (0, at.min(size), 1);
    if p(hi) {
        while hi > 0 {
            let x = hi.saturating_sub(step);
            if !p(x) {
                lo = x + 1;
                break;
            }
            hi = x;
            step *= 2;
        }
    } else {
        lo = hi + 1;
        loop {
            let x = (lo - 1 + step).min(size);
            if p(x) {
                hi = x;
                break;
            }
            lo = x + 1;
            step *= 2;
        }
    }
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if p(mid) {
            hi = mid;
        } else {
            lo = mid + 1;
        }
    }
    hi
}

/// A pair as the pushes and pulls read it: per axis the circle's transfer.
pub(crate) type Xfer<N> = (Transfer<<N as Rem>::C>, Transfer<<N as Rem>::C>);

/// A point of `N`'s circle.
pub(crate) type Point<N> = <<N as Rem>::C as Circle>::P;

/// The start's remainder point: the room's load has no player yet, so any
/// point stands for it - the point of 0.
pub(crate) fn start_point<N: Rem>() -> (Point<N>, Point<N>) {
    let p = N::rem_point(N::from_i16(0)).expect("0 is a remainder");
    (p, p)
}

/// The start's hull: its exact remainder (`start_point`).
pub(crate) fn start_hull<N: Rem>() -> HullOf<N> {
    #[cfg(test)]
    if super::hull::WIDEN.load(std::sync::atomic::Ordering::Relaxed) {
        return Hull::full();
    }
    let (x, y) = start_point::<N>();
    Hull::point(x, y)
}

/// Does hull `h` hold the remainder `(x, y)`?
pub(crate) fn holds<N: Rem>(h: &HullOf<N>, x: N, y: N) -> bool {
    N::rem_point(x).zip(N::rem_point(y)).is_some_and(|(x, y)| h.contains(x, y))
}

/// Does an edge of transfer `p` take the remainder `from` and end at `to`
/// (`None`: no player at the frame's end; any action)?
pub(crate) fn maps<N: Rem>(p: &Pair<N>, from: (N, N), to: Option<(N, N)>) -> bool {
    let axis = |a: &Transfer<N::C>, f: N, t: Option<N>| {
        let Some(f) = N::rem_point(f) else { return false };
        if !a.takes(f) {
            return false;
        }
        let Some(t) = t else { return true };
        N::rem_point(t).is_some_and(|t| a.image(Seg { lo: f, hi: N::C::next(f) }) == Some(Seg { lo: t, hi: N::C::next(t) }))
    };
    let (tx, ty) = N::xfer(p);
    axis(&tx, from.0, to.map(|t| t.0)) && axis(&ty, from.1, to.map(|t| t.1))
}

/// 16.16's transfer record (`Rem::Axis`, 13 bytes): the guard `[lo, hi)`
/// and the action (`tag` 0: the rotation by `val`, in `0..CIRCLE`; 1: the
/// constant point `val`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AxisXfer {
    pub(crate) lo: u32,
    pub(crate) hi: u32,
    pub(crate) tag: u8,
    pub(crate) val: i32,
}

impl Rem for P8 {
    type C = Fixed16;
    type Axis = AxisXfer;
    /// `lo`, `hi` (u32), `tag` (u8), `val` (i32).
    const AXIS_BYTES: usize = 13;

    /// Exactly `[frag.lo - ox, frag.hi - ox + 1)` as points (`r + 1/2`).
    #[inline]
    fn guard(frag: (P8, P8), ox: P8) -> (u64, u64) {
        let raw = |v: P8| v.as_raw_u32() as i32 as i64;
        let at = |v: i64| v.clamp(0, 1 << 16) as u64;
        (at(raw(frag.0) - raw(ox) + 32768), at(raw(frag.1) - raw(ox) + 32769))
    }

    fn rem_point(v: P8) -> Option<u32> {
        let r = v.as_raw_u32() as i32;
        (-32768..32768).contains(&r).then(|| arcs::point(r))
    }
    fn at_point(p: u32) -> P8 {
        P8::from_raw(p as i32 - 32768)
    }
    /// A `Mono` as the rotation it is, `ox - k` mod 1 (raw).
    fn record(guard: Seg, act: Action<P8>) -> AxisXfer {
        let (tag, val) = match act {
            Action::Mono { ox, k } => (0, (ox.as_raw_u32() as i32 as i64 - ((k as i64) << 16)).rem_euclid(1 << 16) as i32),
            Action::Const(p) => (1, p as i32),
        };
        AxisXfer { lo: guard.lo, hi: guard.hi, tag, val }
    }
    /// A ROTATION by `val` as the translation it is on the guard (the guard
    /// is one floor's: its image never wraps - checked, FATAL otherwise), or
    /// the constant point.
    #[inline]
    fn transfer(a: &AxisXfer) -> Transfer {
        let guard = Seg { lo: a.lo, hi: a.hi };
        let act = match a.tag {
            0 => {
                let d = if a.lo as i64 + a.val as i64 >= CIRCLE as i64 { a.val - CIRCLE as i32 } else { a.val };
                assert!(a.lo as i64 + d as i64 >= 0 && a.hi as i64 + d as i64 <= CIRCLE as i64, "a transfer whose image wraps: {a:?}");
                Act::Mono(d)
            }
            _ => Act::Const(a.val as u32),
        };
        Transfer { guard, act }
    }
    fn encode(out: &mut Vec<u8>, a: &AxisXfer) {
        out.extend_from_slice(&a.lo.to_le_bytes());
        out.extend_from_slice(&a.hi.to_le_bytes());
        out.push(a.tag);
        out.extend_from_slice(&a.val.to_le_bytes());
    }
    fn decode(b: &[u8]) -> AxisXfer {
        let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().expect("4 bytes"));
        AxisXfer { lo: u32_at(0), hi: u32_at(4), tag: b[8], val: u32_at(9) as i32 }
    }
    #[inline]
    fn axis_hash(a: &AxisXfer) -> u64 {
        mix64((a.lo as u64) << 32 | a.hi as u64) ^ ((a.val as u32 as u64) << 8 | a.tag as u64)
    }
}

/// `AxisF64::tag` of `r -> fl(fl(r + ox) - k)` (`a` the bits of `ox`).
pub(crate) const MONO: u8 = 0;
/// `AxisF64::tag` of the constant point `a`.
pub(crate) const CONST: u8 = 1;

/// The doubles' transfer record (`Rem::Axis`, 29 bytes): the guard `[lo,
/// hi)` of the points (`arcs::index`) and the action (`tag`, `a`, `k`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AxisF64 {
    pub(crate) lo: u64,
    pub(crate) hi: u64,
    pub(crate) tag: u8,
    pub(crate) a: u64,
    pub(crate) k: i32,
}

impl Rem for LuaF64 {
    type C = Doubles;
    type Axis = AxisF64;
    /// `lo`, `hi`, `a` (u64), `tag` (u8), `k` (i32).
    const AXIS_BYTES: usize = 29;

    fn rem_point(v: LuaF64) -> Option<u64> {
        arcs::index(v.value())
    }
    fn at_point(p: u64) -> LuaF64 {
        LuaF64::new(arcs::value(p)).expect("a remainder is finite")
    }
    /// A `Mono` by the bits of its speed.
    fn record(guard: Seg<u64>, act: Action<LuaF64>) -> AxisF64 {
        let (tag, a, k) = match act {
            Action::Mono { ox, k } => (MONO, ox.to_bits(), k),
            Action::Const(p) => (CONST, p, 0),
        };
        AxisF64 { lo: guard.lo, hi: guard.hi, tag, a, k }
    }
    #[inline]
    fn transfer(a: &AxisF64) -> Transfer<Doubles> {
        let act = match a.tag {
            MONO => Act::Mono(Shift { ox: a.a, k: a.k }),
            _ => Act::Const(a.a),
        };
        Transfer { guard: Seg { lo: a.lo, hi: a.hi }, act }
    }
    fn encode(out: &mut Vec<u8>, a: &AxisF64) {
        out.extend_from_slice(&a.lo.to_le_bytes());
        out.extend_from_slice(&a.hi.to_le_bytes());
        out.extend_from_slice(&a.a.to_le_bytes());
        out.push(a.tag);
        out.extend_from_slice(&a.k.to_le_bytes());
    }
    fn decode(b: &[u8]) -> AxisF64 {
        let u = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().expect("8 bytes"));
        AxisF64 { lo: u(0), hi: u(8), a: u(16), tag: b[24], k: i32::from_le_bytes(b[25..29].try_into().expect("4 bytes")) }
    }
    #[inline]
    fn axis_hash(a: &AxisF64) -> u64 {
        mix64(a.lo ^ a.hi.rotate_left(21) ^ mix64(a.a ^ ((a.k as u32 as u64) << 8 | a.tag as u64)))
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::search::arcs::{Piece, Region, POINTS, ZERO};

    pub(crate) fn lcg(s: &mut u64) -> u64 {
        crate::testutil::lcg(s) >> 1
    }

    /// An arithmetic's speeds for the decoder suites (the old suites'
    /// corpora and random ones).
    pub(crate) trait Speeds: Rem {
        fn speeds() -> Vec<Self>;
    }

    impl Speeds for P8 {
        /// Raw: whole pixels and their neighbours, a point piece (`65537`:
        /// the upper piece is the one point `1/2 - 2^-16`), random in [-9, 9] px.
        fn speeds() -> Vec<P8> {
            let mut s = 5u64;
            let mut v = vec![0i32, 1, -1, 39322, 65535, 65536, 65537, -39322, -65536 * 3 + 7, 32768, -32768, 2 * 65536, -5 * 65536 + 13107];
            v.extend((0..40).map(|_| (lcg(&mut s) % (18 * 65536 + 1)) as i32 - 9 * 65536));
            v.into_iter().map(P8::from_raw).collect()
        }
    }

    impl Speeds for LuaF64 {
        /// Whole and half pixels and their neighbours (the top sliver), the
        /// zeros, random.
        fn speeds() -> Vec<LuaF64> {
            let mut s = 99u64;
            let mut v = vec![0.0, -0.0, 0.6, -0.6, 0.35, 2.9999999999999996, -5.0 + 1e-15, 0.1 + 0.2, 3.0000000000000004];
            for i in -5..=5 {
                let w = i as f64;
                v.extend([w, w.next_up(), w.next_down(), w + 0.5, (w + 0.5).next_down()]);
            }
            v.extend((0..40).map(|_| (lcg(&mut s) % 1_000_000) as f64 / 99_991.0 - 5.0));
            v.into_iter().map(n).collect()
        }
    }

    /// `first` finds a monotone predicate's first point (or the circle's
    /// size) from any estimate, near it or not.
    fn first_from_any_estimate<N: Rem>() {
        let size = N::C::wide(N::C::SIZE);
        let mut s = 7u64;
        let mut rnd = move || (crate::testutil::lcg(&mut s) >> 1) % (size + 1);
        for _ in 0..2000 {
            let t = match rnd() % 4 {
                0 => [0, 1, size - 1, size][(rnd() % 4) as usize],
                _ => rnd(),
            };
            for at in [0, size, t.saturating_sub(1), t, (t + 1).min(size), t.saturating_sub(1000), rnd(), rnd()] {
                assert_eq!(first::<N>(at, |x| x >= t), t, "the first of {t} from {at}");
            }
        }
    }

    /// 16.16's closed-form guard is the search's: every speed of a sweep
    /// over [-9, 9] px, fragments inside and past the circle's image.
    #[test]
    fn the_closed_form_guard_is_the_search() {
        let r = P8::from_raw;
        for ox in (-9 * 65536..=9 * 65536i32).step_by(7) {
            for (a, b) in [(-32768, 32767), (-32768, -1), (0, 32767), (5, 5), (-40000, -35000), (30000, 40000), (-70000, 70000)] {
                let frag = (r(ox + a), r(ox + b));
                assert_eq!(P8::guard(frag, r(ox)), guard::<P8>(frag, r(ox)), "ox {ox} frag {a}..{b}");
            }
        }
    }

    #[test]
    fn first_is_exact_from_any_estimate() {
        first_from_any_estimate::<P8>();
        first_from_any_estimate::<LuaF64>();
    }

    pub(crate) fn n(v: f64) -> LuaF64 {
        LuaF64::new(v).unwrap()
    }

    fn half<N: Rem>() -> N {
        N::from_fixed_raw(1 << 15)
    }

    /// The whole circle's ends as remainders.
    fn ends<N: Rem>() -> (N, N) {
        (N::at_point(N::C::narrow(0)), N::at_point(N::C::prev(N::C::SIZE)))
    }

    /// What the tracer captures of a value of the round split's argument
    /// (16.16's split is the floor fork of `rem + ox + 1/2`).
    fn traced<N: Rem>(v: N) -> N {
        if N::ROUND_BY_SHIFTED_FLOOR {
            N::add(v, half::<N>()).unwrap()
        } else {
            v
        }
    }

    /// The captures the tracer makes of `move` at speed `ox` over the whole
    /// circle: per fragment of its split, `fin` the fragment's image.
    pub(crate) fn captures<N: Rem>(ox: N) -> Vec<Capture<N>> {
        let (bottom, top) = ends::<N>();
        let pre = N::ival(N::add(bottom, ox).unwrap(), N::add(top, ox).unwrap());
        let t = |v: (N, N)| (traced(v.0), traced(v.1));
        (0..N::rounds(pre).unwrap().unwrap())
            .map(|w| {
                let f = N::bounds(N::round_fragment(pre, w).unwrap());
                let k = N::flr(N::add(f.0, half::<N>()).unwrap());
                Capture { took: true, pre: t(N::bounds(pre)), frag: t(f), ox: (ox, ox), fin: Some((N::sub(f.0, k).unwrap(), N::sub(f.1, k).unwrap())) }
            })
            .collect()
    }

    /// The decoder's pieces at every speed of `N`'s corpus.
    pub(crate) fn pieces<N: Speeds>() -> Vec<Transfer<N::C>> {
        N::speeds()
            .into_iter()
            .flat_map(captures)
            .map(|c| {
                let (g, a) = decode_axis(&c).unwrap();
                N::transfer(&N::record(g, a))
            })
            .collect()
    }

    /// THE DECODE AGAINST THE CART: at every speed the pieces partition the
    /// circle, and at every probe of a piece (its ends, its middle, random
    /// points) the cart's `move` lands in the fragment and ends where the
    /// action says; just outside it does not land there. With a collision's
    /// final remainder instead, the same guard, the constant. The number of
    /// one-point pieces.
    fn pieces_partition_the_circle_and_act_as_the_cart<N: Speeds>() -> usize {
        let size = N::C::wide(N::C::SIZE);
        let zero = N::from_i16(0);
        let (mut s, mut one_point) = (17u64, 0);
        for ox in N::speeds() {
            let mut at = 0u64;
            for c in captures(ox) {
                let (g, act) = decode_axis(&c).unwrap_or_else(|e| panic!("ox {ox:?}: {e}"));
                assert!(matches!(act, Action::Mono { .. }), "ox {ox:?}: {act:?}");
                let t = N::transfer(&N::record(g, act));
                let (lo, hi) = (N::C::wide(g.lo), N::C::wide(g.hi));
                assert_eq!(lo, at, "ox {ox:?}: the pieces are contiguous");
                at = hi;
                one_point += (hi - lo == 1) as usize;
                let frag = if N::ROUND_BY_SHIFTED_FLOOR { (N::sub(c.frag.0, half::<N>()).unwrap(), N::sub(c.frag.1, half::<N>()).unwrap()) } else { c.frag };
                let moved = |p: u64| N::add(N::at_point(N::C::narrow(p)), ox).unwrap();
                let inside = |v: N| N::le(frag.0, v) && N::le(v, frag.1);
                let mut probes = vec![lo, hi - 1, lo + (hi - lo) / 2];
                probes.extend((0..8).map(|_| lo + lcg(&mut s) % (hi - lo)));
                for p in probes {
                    let r1 = moved(p);
                    assert!(inside(r1), "ox {ox:?}: point {p} is in the guard, but {r1:?} not in the fragment {frag:?}");
                    let k = N::flr(N::add(r1, half::<N>()).unwrap());
                    let want = N::rem_point(N::sub(r1, k).unwrap()).expect("a remainder");
                    let p = N::C::narrow(p);
                    assert_eq!(t.image(Seg { lo: p, hi: N::C::next(p) }), Some(Seg { lo: want, hi: N::C::next(want) }), "ox {ox:?}: point {p:?}");
                }
                for p in [lo.wrapping_sub(1), hi].into_iter().filter(|&p| p < size) {
                    assert!(!inside(moved(p)), "ox {ox:?}: point {p} outside the guard lands in the fragment");
                }
                let hit = decode_axis(&Capture { fin: Some((zero, zero)), ..c }).unwrap();
                if !N::num_eq(c.fin.unwrap().0, zero) || !N::num_eq(c.fin.unwrap().1, zero) {
                    assert_eq!(hit, (g, Action::Const(N::rem_point(zero).unwrap())), "ox {ox:?}: a collision");
                }
            }
            assert_eq!(at, size, "ox {ox:?}: the pieces cover the circle");
        }
        one_point
    }

    #[test]
    fn the_pieces_partition_the_circle_and_act_as_the_cart() {
        assert!(pieces_partition_the_circle_and_act_as_the_cart::<P8>() > 0, "16.16's corpus has a point piece");
        pieces_partition_the_circle_and_act_as_the_cart::<LuaF64>();
    }

    /// No split (a freeze, the spawn): the whole circle, the identity, a new
    /// player's constant, or refused; a remainder set before the move: the
    /// whole circle to the constant it ends at, or refused.
    fn no_split_and_set_before_the_move<N: Rem>() {
        let raw = N::from_fixed_raw;
        let whole = Seg { lo: N::C::narrow(0), hi: N::C::SIZE };
        let identity = Action::Mono { ox: N::from_i16(0), k: 0 };
        let at = |v: N| Action::Const(N::rem_point(v).unwrap());
        let none = Capture { took: false, pre: (raw(0), raw(0)), frag: (raw(0), raw(0)), ox: (raw(0), raw(0)), fin: Some(ends::<N>()) };
        assert_eq!(decode_axis(&none), Ok((whole, identity)));
        assert_eq!(decode_axis(&Capture { fin: None, ..none }), Ok((whole, identity)));
        assert_eq!(decode_axis(&Capture { fin: Some((raw(0), raw(0))), ..none }), Ok((whole, at(raw(0)))));
        assert!(decode_axis(&Capture { fin: Some((raw(0), raw(5))), ..none }).is_err());
        // A platform's carry blocked by a wall set the remainder to 0 (and
        // the speed may move it after): the split's argument is the point
        // 0 plus the speed, every input remainder takes the edge.
        for ox in [raw(0), raw(13107), raw(-70000)] {
            let p = traced(N::add(raw(0), ox).unwrap());
            let set = Capture { took: true, pre: (p, p), frag: (p, p), ox: (ox, ox), fin: Some((raw(0), raw(0))) };
            assert_eq!(decode_axis(&set), Ok((whole, at(raw(0)))), "ox {ox:?}");
            assert_eq!(decode_axis(&Capture { fin: Some((raw(13107), raw(13107))), ..set }), Ok((whole, at(raw(13107)))));
            // A death after it: the whole circle, into the dummy coordinate.
            assert_eq!(decode_axis(&Capture { fin: None, ..set }), Ok((whole, identity)));
            // A fragment off the point, a final remainder not a point, a
            // point no remainder plus the speed is.
            assert!(decode_axis(&Capture { frag: (N::below(p).unwrap(), p), ..set }).is_err());
            assert!(decode_axis(&Capture { fin: Some((raw(0), raw(9))), ..set }).is_err());
            let far = N::add(p, raw(2 << 16)).unwrap();
            assert!(decode_axis(&Capture { pre: (far, far), frag: (far, far), ..set }).is_err(), "ox {ox:?}");
        }
    }

    #[test]
    fn no_split_and_a_remainder_set_before_the_move_are_the_whole_circle() {
        no_split_and_set_before_the_move::<P8>();
        no_split_and_set_before_the_move::<LuaF64>();
    }

    /// What the model does not cover is refused (FATAL in the forward).
    fn refusals<N: Rem>() {
        let raw = N::from_fixed_raw;
        let c = captures(raw(39322))[0];
        let bad = |c: Capture<N>| decode_axis(&c).is_err();
        // An inexact speed; a remainder at the move narrower than the circle.
        assert!(bad(Capture { ox: (c.ox.0, N::add(c.ox.0, raw(10)).unwrap()), ..c }));
        assert!(bad(Capture { pre: (c.pre.0, N::sub(c.pre.1, raw(100)).unwrap()), ..c }));
        // A fragment over two amounts, cut short (not its amount's whole
        // piece), or outside the argument.
        assert!(bad(Capture { frag: c.pre, fin: None, ..c }));
        assert!(bad(Capture { frag: (c.frag.0, N::below(c.frag.1).unwrap()), fin: None, ..c }));
        assert!(bad(Capture { frag: (N::below(c.pre.0).unwrap(), c.frag.1), fin: None, ..c }));
        // A final remainder neither the image nor a point; a constant
        // outside [-1/2, 1/2).
        assert!(bad(Capture { fin: Some((raw(0), raw(16384))), ..c }));
        assert!(bad(Capture { fin: Some((raw(49152), raw(49152))), ..c }));
        // A collision's constant; a death.
        assert!(matches!(decode_axis(&Capture { fin: Some((raw(0), raw(0))), ..c }), Ok((_, Action::Const(_)))));
        assert!(matches!(decode_axis(&Capture { fin: None, ..c }), Ok((_, Action::Mono { .. }))));
    }

    #[test]
    fn what_the_model_does_not_cover_is_refused() {
        refusals::<P8>();
        refusals::<LuaF64>();
    }

    /// Pairs and their records round-trip through their bytes.
    fn pairs_round_trip<N: Speeds>() {
        let ps: Vec<N::Axis> = N::speeds().into_iter().flat_map(captures).map(|c| decode_axis(&c).map(|(g, a)| N::record(g, a)).unwrap()).collect();
        let mut b = Vec::new();
        for w in ps.windows(2) {
            N::encode_pair(&mut b, &(w[0], w[1]));
        }
        assert_eq!(b.len(), (ps.len() - 1) * N::PAIR_BYTES);
        for (k, w) in ps.windows(2).enumerate() {
            assert_eq!(N::decode_pair(&b[k * N::PAIR_BYTES..(k + 1) * N::PAIR_BYTES]), (w[0], w[1]));
        }
    }

    #[test]
    fn pairs_round_trip_through_their_bytes() {
        pairs_round_trip::<P8>();
        pairs_round_trip::<LuaF64>();
    }

    /// A point of `N`'s circle as a number, often near its ends or 0.
    pub(crate) fn probe_point<N: Rem>(s: &mut u64) -> u64 {
        let (size, zero) = (N::C::wide(N::C::SIZE), N::C::wide(N::rem_point(N::from_i16(0)).unwrap()));
        match lcg(s) % 5 {
            0 => lcg(s) % 64,
            1 => size - 1 - lcg(s) % 64,
            2 => zero - 32 + lcg(s) % 64,
            _ => lcg(s) % size,
        }
    }

    /// THE PULL IS THE PREIMAGE, EXACTLY: a region pulled through the
    /// decoder's pieces of real speeds (and a constant), probed at the
    /// result's boundaries and their neighbours, the guards' ends and random
    /// points: a point is in the pull iff the guards take it and its image
    /// is in the region.
    fn a_region_pulls_to_its_exact_preimage<N: Speeds>() {
        let (size, mut s) = (N::C::wide(N::C::SIZE), 17u64);
        let pt = N::C::narrow;
        let mut ts = pieces::<N>();
        ts.push(Transfer { guard: Seg { lo: pt(0), hi: N::C::SIZE }, act: Act::Const(N::rem_point(N::from_i16(0)).unwrap()) });
        let seg = |s: &mut u64| {
            let (a, b) = (probe_point::<N>(s), probe_point::<N>(s));
            Seg { lo: pt(a.min(b)), hi: pt(a.max(b) + 1) }
        };
        let image = |t: &Transfer<N::C>, p: Point<N>| t.image(Seg { lo: p, hi: N::C::next(p) }).expect("in the guard").lo;
        for _ in 0..300 {
            let mut ps: Vec<Piece<_>> = (0..1 + lcg(&mut s) % 6).map(|_| Piece { y: seg(&mut s), x: seg(&mut s) }).collect();
            let r = Region::<N::C>::from_pieces(&mut ps, &mut Vec::new());
            let (tx, ty) = (ts[(lcg(&mut s) % ts.len() as u64) as usize], ts[(lcg(&mut s) % ts.len() as u64) as usize]);
            let mut out = Vec::new();
            r.as_ref().pull(tx, ty, &mut out);
            let pr = Region::<N::C>::from_pieces(&mut out, &mut Vec::new());
            let w = N::C::wide;
            let around = |v: u64| [v.wrapping_sub(1), v, v + 1].into_iter().filter(|&p| p < size);
            let mut px: Vec<u64> = vec![w(tx.guard.lo), w(tx.guard.hi) - 1, probe_point::<N>(&mut s), probe_point::<N>(&mut s)];
            let mut py: Vec<u64> = vec![w(ty.guard.lo), w(ty.guard.hi) - 1, probe_point::<N>(&mut s), probe_point::<N>(&mut s)];
            for (y, xs) in pr.as_ref().slabs() {
                py.extend(around(w(y.lo)).chain(around(w(y.hi))));
                for x in xs {
                    px.extend(around(w(x.lo)).chain(around(w(x.hi))));
                }
            }
            for &x in &px {
                for &y in &py {
                    let (x, y) = (pt(x), pt(y));
                    let want = tx.takes(x) && ty.takes(y) && r.as_ref().contains(image(&tx, x), image(&ty, y));
                    assert_eq!(pr.as_ref().contains(x, y), want, "pull at ({x:?}, {y:?}) through {tx:?} {ty:?}");
                }
            }
        }
    }

    #[test]
    fn a_region_pulls_to_its_exact_preimage_through_the_pieces() {
        a_region_pulls_to_its_exact_preimage::<P8>();
        a_region_pulls_to_its_exact_preimage::<LuaF64>();
    }

    /// The sliver at an integer speed: `fl(r - 2) = -3/2` for the top two
    /// doubles, a piece of its own onto `-1/2` that no 16.16 remainder
    /// takes; 16.16's one piece is the rest. A point hull at the top
    /// reaches only the sliver; one below it, only the rest.
    #[test]
    fn an_integer_speed_has_the_top_sliver() {
        // How many 16.16 remainders (multiples of `2^-16`) the points `[lo, hi)` hold.
        let fixed_points = |lo: u64, hi: u64| {
            let from = |i: u64| if i == POINTS { 32768 } else { (crate::search::arcs::value(i) * 65536.0).ceil() as i64 };
            (from(hi) - from(lo)) as u64
        };
        assert_eq!((fixed_points(0, POINTS), fixed_points(0, 1)), (65536, 1));
        let pieces: Vec<Transfer<Doubles>> = captures(n(-2.0)).iter().map(|c| decode_axis(c).unwrap()).map(|(g, a)| LuaF64::transfer(&LuaF64::record(g, a))).collect();
        assert_eq!(pieces.len(), 2);
        assert_eq!(pieces[1].guard, Seg { lo: POINTS - 2, hi: POINTS });
        assert_eq!(fixed_points(pieces[1].guard.lo, pieces[1].guard.hi), 0);
        assert_eq!(pieces[1].image(pieces[1].guard), Some(Seg { lo: 0, hi: 1 }));
        assert_eq!(fixed_points(pieces[0].guard.lo, pieces[0].guard.hi), 65536);
        let identity = Transfer { guard: Seg { lo: 0, hi: POINTS }, act: Act::Mono(Shift { ox: 0, k: 0 }) };
        let mut acc = crate::search::hull::HullAcc::default();
        assert!(!Hull::point(POINTS - 3, ZERO).push(&(pieces[1], identity), &mut acc));
        assert!(Hull::point(POINTS - 2, ZERO).push(&(pieces[1], identity), &mut acc));
        assert_eq!(acc.hull(), Some(Hull::point(0, ZERO)));
    }
}
