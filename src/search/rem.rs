//! THE REMAINDER MODEL of an arithmetic (plans/float.md "The remainder
//! model and the round fork"): the CIRCLE its remainders are points of
//! (`Rem::C`) and an edge's transfer PAIR as the tree's table stores it. The
//! forward and the storage are generic over it.

use std::fmt::Debug;
use std::hash::Hash;

use anyhow::Result;
use celeste_core::arith::Arithmetic;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

use super::arc_edges::{self, Pair};
use super::arcs::{Circle, Fixed16, Seg, Transfer};
use super::hull::{Hull, HullOf};
use super::rem_f64;

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
    /// An edge's transfer pair as the tree's table records it.
    type Pair: Copy + Debug + PartialEq + Eq + Hash + PartialOrd + Ord + Send + Sync + 'static;
    /// One axis of a pair (`record`).
    type Axis: Copy + Debug + Eq + Hash + Ord + Send + Sync + 'static;
    /// The circle the remainders are points of (hulls, the arcs' sets).
    type C: Circle;
    /// Bytes a pair takes on disk.
    const PAIR_BYTES: usize;

    /// THE DECODE of a frame's captures (x, y): `decode`'s, recorded; `Err`
    /// is a refusal (fatal in the forward).
    fn transfer(x: &Capture<Self>, y: &Capture<Self>) -> Result<Self::Pair, String>;
    /// One axis's guard and action as its record holds them.
    fn record(guard: Seg<Point<Self>>, act: Action<Self>) -> Self::Axis;
    /// `decode`'s guard: the points (as numbers, `[lo, hi)`) whose
    /// `fl(r + ox)` is in the round split's fragment `frag`, the circle's
    /// ends moving by `ox` without overflow. The search with the operation
    /// (`guard`), or the arithmetic's closed form: the same points (tested).
    #[inline]
    fn guard(frag: (Self, Self), ox: Self) -> (u64, u64) {
        guard(frag, ox)
    }
    fn xfer(p: &Self::Pair) -> Xfer<Self>;
    /// A remainder as a point of the circle (`None`: not in `[-1/2, 1/2)`).
    fn rem_point(v: Self) -> Option<Point<Self>>;
    /// The remainder at point `p` (`point`'s inverse).
    fn at_point(p: Point<Self>) -> Self;
    /// Append `p`'s `PAIR_BYTES`.
    fn encode_pair(out: &mut Vec<u8>, p: &Self::Pair);
    fn decode_pair(b: &[u8]) -> Self::Pair;
    /// A hash of a pair (the transfer table's index).
    fn pair_hash(p: &Self::Pair) -> u64;
}

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
pub(crate) fn decode<N: Rem>(x: &Capture<N>, y: &Capture<N>) -> Result<(N::Axis, N::Axis), String> {
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
pub(crate) fn maps<N: Rem>(p: &N::Pair, from: (N, N), to: Option<(N, N)>) -> bool {
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

impl Rem for P8 {
    type Pair = Pair;
    type Axis = arc_edges::AxisXfer;
    type C = Fixed16;
    const PAIR_BYTES: usize = arc_edges::PAIR_BYTES;

    fn transfer(x: &Capture<P8>, y: &Capture<P8>) -> Result<Pair, String> {
        decode(x, y)
    }
    /// A `Mono` as the rotation it is, `ox - k` mod 1 (raw).
    fn record(guard: Seg, act: Action<P8>) -> arc_edges::AxisXfer {
        let (tag, val) = match act {
            Action::Mono { ox, k } => (0, (ox.as_raw_u32() as i32 as i64 - ((k as i64) << 16)).rem_euclid(1 << 16) as i32),
            Action::Const(p) => (1, p as i32),
        };
        arc_edges::AxisXfer { lo: guard.lo, hi: guard.hi, tag, val }
    }
    /// Exactly `[frag.lo - ox, frag.hi - ox + 1)` as points (`r + 1/2`).
    #[inline]
    fn guard(frag: (P8, P8), ox: P8) -> (u64, u64) {
        let raw = |v: P8| v.as_raw_u32() as i32 as i64;
        let at = |v: i64| v.clamp(0, 1 << 16) as u64;
        (at(raw(frag.0) - raw(ox) + 32768), at(raw(frag.1) - raw(ox) + 32769))
    }
    #[inline]
    fn xfer(p: &Pair) -> (Transfer, Transfer) {
        (p.0.transfer(), p.1.transfer())
    }
    fn rem_point(v: P8) -> Option<u32> {
        let r = v.as_raw_u32() as i32;
        (-32768..32768).contains(&r).then(|| super::arcs::point(r))
    }
    fn at_point(p: u32) -> P8 {
        P8::from_raw(p as i32 - 32768)
    }
    fn encode_pair(out: &mut Vec<u8>, p: &Pair) {
        arc_edges::encode_pair(out, p)
    }
    fn decode_pair(b: &[u8]) -> Pair {
        arc_edges::decode_pair(b)
    }
    #[inline]
    fn pair_hash(p: &Pair) -> u64 {
        use celeste_engine::runtime2::mix64;
        let w = |a: &arc_edges::AxisXfer| mix64((a.lo as u64) << 32 | a.hi as u64) ^ ((a.val as u32 as u64) << 8 | a.tag as u64);
        mix64(w(&p.0) ^ w(&p.1).rotate_left(31))
    }
}

impl Rem for LuaF64 {
    type Pair = rem_f64::Pair;
    type Axis = rem_f64::Axis;
    type C = rem_f64::Doubles;
    const PAIR_BYTES: usize = rem_f64::PAIR_BYTES;

    fn transfer(x: &Capture<LuaF64>, y: &Capture<LuaF64>) -> Result<rem_f64::Pair, String> {
        decode(x, y)
    }
    /// A `Mono` by the bits of its speed.
    fn record(guard: Seg<u64>, act: Action<LuaF64>) -> rem_f64::Axis {
        let (tag, a, k) = match act {
            Action::Mono { ox, k } => (rem_f64::MONO, ox.to_bits(), k),
            Action::Const(p) => (rem_f64::CONST, p, 0),
        };
        rem_f64::Axis { lo: guard.lo, hi: guard.hi, tag, a, k }
    }
    #[inline]
    fn xfer(p: &rem_f64::Pair) -> Xfer<LuaF64> {
        (p.0.transfer(), p.1.transfer())
    }
    fn rem_point(v: LuaF64) -> Option<u64> {
        rem_f64::index(v.value())
    }
    fn at_point(p: u64) -> LuaF64 {
        LuaF64::new(rem_f64::value(p)).expect("a remainder is finite")
    }
    fn encode_pair(out: &mut Vec<u8>, p: &rem_f64::Pair) {
        p.0.encode(out);
        p.1.encode(out);
    }
    fn decode_pair(b: &[u8]) -> rem_f64::Pair {
        (rem_f64::Axis::decode(&b[..29]), rem_f64::Axis::decode(&b[29..58]))
    }
    #[inline]
    fn pair_hash(p: &rem_f64::Pair) -> u64 {
        celeste_engine::runtime2::mix64(p.0.mixed() ^ p.1.mixed().rotate_left(31))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::arc_edges::legacy::{self as fixed_legacy, RawAxis};
    use crate::search::rem_f64::{legacy as float_legacy, value, POINTS, ZERO};
    use celeste_core::lua_f64::LuaF64Interval as FI;

    /// One axis through `decode`, recorded.
    fn generic<N: Rem>(c: &Capture<N>) -> Result<N::Axis, String> {
        decode_axis(c).map(|(g, a)| N::record(g, a)).map_err(String::from)
    }

    /// The two decoders on one capture: the same record, or both refuse.
    /// Whether it decoded.
    fn agree<A: PartialEq + Debug>(what: &dyn Debug, legacy: Result<A, String>, generic: Result<A, String>) -> bool {
        match (&legacy, &generic) {
            (Ok(a), Ok(b)) => assert_eq!(a, b, "{what:?}"),
            (Err(_), Err(_)) => {}
            _ => panic!("{what:?}: legacy {legacy:?}, generic {generic:?}"),
        }
        legacy.is_ok()
    }

    fn agree16(r: &RawAxis) -> bool {
        let n = |v: (i32, i32)| (P8::from_raw(v.0), P8::from_raw(v.1));
        let c = Capture { took: r.took, pre: n(r.pre), frag: n(r.frag), ox: n(r.ox), fin: r.fin.map(n) };
        agree(r, fixed_legacy::decode_axis(r), generic(&c))
    }

    /// THE ONE DECODER IS 16.16'S OLD ONE: every raw speed in [-9, 9] px,
    /// each fragment of its floor split with the final remainder its image,
    /// none, the point 0, another point, or neither; fragments cut short;
    /// no split; a remainder set before the move; what neither covers.
    #[test]
    fn the_decoder_is_the_legacy_one_in_16_16() {
        let (mut ok, mut all) = (0u64, 0u64);
        let mut count = |d: bool| {
            all += 1;
            ok += d as u64;
        };
        for ox in -9 * 65536..=9 * 65536i32 {
            let pre = (ox, ox + 65535);
            let cut = (ox.div_euclid(65536) + 1) * 65536;
            let mut frags = vec![(ox, pre.1.min(cut - 1))];
            if pre.1 >= cut {
                frags.push((cut, pre.1));
            }
            let split = |frag, fin| RawAxis { took: true, pre, frag, ox: (ox, ox), fin };
            for frag in frags {
                let f = frag.0.div_euclid(65536);
                let image = (frag.0 - 32768 - f * 65536, frag.1 - 32768 - f * 65536);
                for fin in [Some(image), None, Some((0, 0)), Some((-32768, -32768)), Some((image.0, image.0 + 1)), Some((40000, 40000))] {
                    count(agree16(&split(frag, fin)));
                }
                if ox % 97 == 0 {
                    // Cut short at either end, or inside: no whole piece.
                    for short in [(frag.0 + 1, frag.1), (frag.0, frag.1 - 1), (frag.0 + 1, frag.1 - 1)] {
                        if short.0 <= short.1 {
                            assert!(!agree16(&split(short, Some((short.0 - 32768 - f * 65536, short.1 - 32768 - f * 65536)))), "{short:?} decoded");
                        }
                    }
                }
            }
            if ox % 13 == 0 {
                // Set before the move: the remainder in range or not, the
                // fragment the point or not, the final remainder.
                for set in [-32768, 0, 32767, 32768, -32769, 70000] {
                    let p = set + ox + 32768;
                    for (frag, fin) in [((p, p), Some((0, 0))), ((p, p), None), ((p, p), Some((0, 9))), ((p, p), Some((-32768, 32767))), ((p - 1, p), Some((0, 0)))] {
                        count(agree16(&RawAxis { took: true, pre: (p, p), frag, ox: (ox, ox), fin }));
                    }
                }
                // Inexact speeds, an argument narrower than the circle, a fragment over two floors or outside.
                for r in [
                    RawAxis { took: true, pre: (ox, ox + 65535 + 10), frag: (ox, ox + 10), ox: (ox, ox + 10), fin: None },
                    RawAxis { took: true, pre: (ox, ox + 100), frag: (ox, ox + 100), ox: (ox, ox), fin: None },
                    RawAxis { took: true, pre, frag: (ox, ox + 65535), ox: (ox, ox), fin: None },
                    RawAxis { took: true, pre, frag: (ox - 5, ox), ox: (ox, ox), fin: None },
                ] {
                    count(agree16(&r));
                }
            }
        }
        // No split: the whole circle, none, points, neither.
        for fin in [Some((-32768, 32767)), None, Some((0, 0)), Some((-32768, -32768)), Some((32767, 32767)), Some((32768, 32768)), Some((0, 5))] {
            count(agree16(&RawAxis { took: false, pre: (3, 9), frag: (3, 4), ox: (1, 2), fin }));
        }
        assert!(ok > 7_000_000 && ok < all, "{ok} of {all} decoded");
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

    fn n(v: f64) -> LuaF64 {
        LuaF64::new(v).unwrap()
    }

    /// The captures the reference engine makes of `move` at speed `ox` over
    /// the whole domain: per fragment of its round split, `fin` the image.
    fn captures(ox: f64) -> Vec<Capture<LuaF64>> {
        let pre = FI::new(n(-0.5), n(value(POINTS - 1))).checked_add(FI::point(n(ox))).unwrap();
        (0..pre.rounds().unwrap().unwrap())
            .map(|w| {
                let f = pre.round_fragment(w).unwrap();
                let k = float_legacy::round(f.lo.value());
                Capture { took: true, pre: (pre.lo, pre.hi), frag: (f.lo, f.hi), ox: (n(ox), n(ox)), fin: Some((n(f.lo.value() - k), n(f.hi.value() - k))) }
            })
            .collect()
    }

    /// `v` with the other zero's sign (any other value as it is).
    fn flip(v: LuaF64) -> LuaF64 {
        if v.value() == 0.0 {
            n(-v.value())
        } else {
            v
        }
    }

    /// THE ONE DECODER IS THE DOUBLES' OLD ONE where that one held the
    /// rules 16.16's does: at the old tests' speeds, 1e5 random ones, every
    /// integer in [-9, 9] and its neighbours (the top sliver), each piece
    /// with `fin` its image (its zeros' signs flipped too), none, either
    /// zero, another point or neither; no split. The old one did not check
    /// that a fragment is its amount's whole piece, nor that a remainder set
    /// before the move is a remainder: there, and only there, the new one
    /// refuses what the old one took.
    #[test]
    fn the_decoder_is_the_legacy_one_in_doubles() {
        let mut s = 99u64;
        let rnd = |s: &mut u64| (crate::testutil::lcg(s) >> 11) as f64 / (1u64 << 53) as f64;
        let mut speeds = vec![0.0, -0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -4.0, 0.6, -0.6, 1.5, -1.5, 0.35, 0.5, -0.5, 2.9999999999999996, -5.0 + 1e-15, 0.1 + 0.2];
        for i in -9..=9 {
            let v = i as f64;
            speeds.extend([v, v.next_up(), v.next_down(), v + 0.5, (v + 0.5).next_up(), (v + 0.5).next_down()]);
        }
        speeds.extend((0..100_000).map(|i| if i % 2 == 0 { 18.0 * rnd(&mut s) - 9.0 } else { (rnd(&mut s) - 0.5) * 2f64.powi(-(i % 40)) }));
        let (mut ok, mut refused_new) = (0u64, 0u64);
        for &ox in &speeds {
            for c in captures(ox) {
                let image = c.fin.unwrap();
                let other = n(value(ZERO + 12345));
                for fin in [Some(image), Some((flip(image.0), flip(image.1))), None, Some((n(0.0), n(0.0))), Some((n(-0.0), n(-0.0))), Some((n(-0.0), n(0.0))), Some((other, other)), Some((n(0.0), n(0.25)))] {
                    let c = Capture { fin, ..c };
                    ok += agree(&c, float_legacy::decode_axis(&c), generic(&c)) as u64;
                }
                // Cut short: not the whole piece. Refused; the old one took
                // what still holds a remainder.
                let (lo, hi) = c.frag;
                for short in [(lo.succ().unwrap(), hi), (lo, hi.pred().unwrap())] {
                    if short.0.le(short.1) {
                        let c = Capture { frag: short, fin: None, ..c };
                        assert!(generic(&c).is_err(), "{c:?} decoded");
                        refused_new += float_legacy::decode_axis(&c).is_ok() as u64;
                    }
                }
            }
            // Set before the move: the remainder 0 or -1/2 set, plus the
            // speed; and a point no remainder reaches (the old one took it).
            for set in [0.0, -0.5] {
                let p = n(set + ox);
                for (frag, fin) in [((p, p), Some((n(0.0), n(0.0)))), ((p, p), None), ((p, p), Some((n(0.0), n(0.1))))] {
                    let c = Capture { took: true, pre: (p, p), frag, ox: (n(ox), n(ox)), fin };
                    ok += agree(&c, float_legacy::decode_axis(&c), generic(&c)) as u64;
                }
            }
            let far = n(ox + 3.0);
            let c = Capture { took: true, pre: (far, far), frag: (far, far), ox: (n(ox), n(ox)), fin: None };
            assert!(generic(&c).is_err(), "{c:?} decoded");
            refused_new += float_legacy::decode_axis(&c).is_ok() as u64;
        }
        let top = n(value(POINTS - 1));
        for fin in [Some((n(-0.5), top)), None, Some((n(0.0), n(0.0))), Some((n(-0.0), n(0.0))), Some((n(0.0), n(0.25)))] {
            let c = Capture { took: false, pre: (n(0.0), n(0.0)), frag: (n(0.0), n(0.0)), ox: (n(0.0), n(0.0)), fin };
            ok += agree(&c, float_legacy::decode_axis(&c), generic(&c)) as u64;
        }
        assert!(ok > 1_000_000 && refused_new > 100_000, "{ok} decoded alike; {refused_new} refused only by the new one");
    }
}
