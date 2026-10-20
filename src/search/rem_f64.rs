//! THE REMAINDER MODEL IN DOUBLES (plans/float.md "The remainder model and
//! the round fork"), as `arcs` and `arc_edges` are 16.16's. A POINT is a
//! remainder's INDEX in value order among the doubles of `[-1/2, 1/2)`: `0`
//! is `-1/2`, `ZERO` is `0` (`-0.0` too: no read of a remainder tells the
//! zeros apart), `POINTS - 1` is `1/2 - 2^-54`.
//!
//! `move` is `rem += ox; amount = flr(rem + 0.5); rem -= amount`, forked by
//! `__split_by_round(rem)` at the exact cuts over the node's whole
//! `[-1/2, 1/2)`; `decode_axis` turns a capture (`rem::Capture`) into:
//! * the GUARD: the `r` with `fl(r + ox)` in `frag`, an order-interval
//!   (`fl(r + ox)` is monotone) found EXACTLY by bisection with the
//!   operation itself;
//! * the ACTION `Mono(ox, k)`, `r -> fl(fl(r + ox) - k)` (monotone, not
//!   injective), where `fin` is the fragment's image `fl(frag - k)` or no
//!   player ends the frame (a death: any action is exact, as in
//!   `arc_edges`); `Const(fin)` where a blocked step reset the remainder;
//! * no split (a freeze, the spawn): the whole guard, the identity
//!   `Mono(0, 0)` (`fin` the whole input, or no player) or `Const(fin)` (a
//!   player made this frame);
//! * a split of ONE point (`set_before_the_move`): the whole guard,
//!   `Const(fin)` (no player at the end: the identity).
//! Anything else is REFUSED (fatal in the forward).
//!
//! THE TOP SLIVER: at an integer speed the doubles `{1/2 - 2^-54,
//! 1/2 - 2^-53}` round onto the cut (`fl(r - 2) = -3/2`), a piece no 16.16
//! remainder takes; the hull forward pushes it only where a hull reaches it.

use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::mix64;

use super::arcs::{Act, Circle, Seg, Transfer};
use super::rem::Capture;

/// The index of `0` (and the count of doubles in `[-1/2, 0)`): `1/2`'s bits.
pub(crate) const ZERO: u64 = 0x3FE0_0000_0000_0000;
/// The points: the doubles of `[-1/2, 1/2)` by value.
pub(crate) const POINTS: u64 = 2 * ZERO;

/// The point of remainder `v`; `None` outside `[-1/2, 1/2)`.
#[inline]
pub(crate) fn index(v: f64) -> Option<u64> {
    if !(-0.5..0.5).contains(&v) {
        return None;
    }
    let b = v.abs().to_bits();
    Some(if v > 0.0 { ZERO + b } else { ZERO - b })
}

/// The remainder of point `i` (`+0.0` at `ZERO`).
#[inline]
pub(crate) fn value(i: u64) -> f64 {
    debug_assert!(i < POINTS);
    if i >= ZERO {
        f64::from_bits(i - ZERO)
    } else {
        -f64::from_bits(ZERO - i)
    }
}

/// `Axis::tag` of `r -> fl(fl(r + ox) - k)` (`a` the bits of `ox`).
pub(crate) const MONO: u8 = 0;
/// `Axis::tag` of the constant point `a`.
pub(crate) const CONST: u8 = 1;

/// One axis's decoded transfer: the guard `[lo, hi)` of the points, and
/// the action (`tag`, `a`, `k`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Axis {
    pub(crate) lo: u64,
    pub(crate) hi: u64,
    pub(crate) tag: u8,
    pub(crate) a: u64,
    pub(crate) k: i32,
}

/// A transfer pair (x, y).
pub(crate) type Pair = (Axis, Axis);

/// Bytes per encoded pair: per axis `lo`, `hi`, `a` (u64), `tag` (u8), `k` (i32).
pub(crate) const PAIR_BYTES: usize = 2 * 29;

impl Axis {
    /// The identity over the whole circle.
    pub(crate) fn identity() -> Axis {
        Axis::mono(0, POINTS, 0.0, 0)
    }

    fn mono(lo: u64, hi: u64, ox: f64, k: i32) -> Axis {
        Axis { lo, hi, tag: MONO, a: ox.to_bits(), k }
    }

    fn konst(lo: u64, hi: u64, rem: f64) -> Result<Axis, String> {
        let p = index(rem).ok_or_else(|| format!("a constant remainder {rem:?} outside [-1/2, 1/2)"))?;
        Ok(Axis { lo, hi, tag: CONST, a: p, k: 0 })
    }

    /// The image of point `i` (inside the guard).
    #[cfg(test)]
    pub(crate) fn apply(&self, i: u64) -> u64 {
        match self.transfer().act {
            Act::Mono(m) => Doubles::apply(&m, i),
            Act::Const(c) => c,
        }
    }

    /// As the pushes and pulls read it (`Rem::xfer`).
    #[inline]
    pub(crate) fn transfer(&self) -> Transfer<Doubles> {
        let act = match self.tag {
            MONO => Act::Mono(Shift { ox: self.a, k: self.k }),
            _ => Act::Const(self.a),
        };
        Transfer { guard: Seg { lo: self.lo, hi: self.hi }, act }
    }

    pub(crate) fn encode(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.lo.to_le_bytes());
        out.extend_from_slice(&self.hi.to_le_bytes());
        out.extend_from_slice(&self.a.to_le_bytes());
        out.push(self.tag);
        out.extend_from_slice(&self.k.to_le_bytes());
    }

    pub(crate) fn decode(b: &[u8]) -> Axis {
        let u = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().expect("8 bytes"));
        Axis { lo: u(0), hi: u(8), a: u(16), tag: b[24], k: i32::from_le_bytes(b[25..29].try_into().expect("4 bytes")) }
    }

    pub(crate) fn mixed(&self) -> u64 {
        mix64(self.lo ^ self.hi.rotate_left(21) ^ mix64(self.a ^ ((self.k as u32 as u64) << 8 | self.tag as u64)))
    }
}

/// The doubles' monotone action `r -> fl(fl(r + ox) - k)` (`ox` its bits).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Shift {
    ox: u64,
    k: i32,
}

/// `move` on one axis after the round split: `fl(fl(r + ox) - k)`.
#[inline]
fn mono(r: f64, ox: f64, k: f64) -> f64 {
    (r + ox) - k
}

/// `amount`: `flr(fl(x + 1/2))`.
fn round(x: f64) -> f64 {
    (x + 0.5).floor()
}

/// The first point from which `p` (false, then true) holds, by bisection.
fn first(p: impl Fn(u64) -> bool) -> u64 {
    first_in(0, POINTS, p)
}

/// The first point of `lo..hi` from which `p` (false, then true) holds
/// (`hi`: none).
fn first_in(mut lo: u64, mut hi: u64, p: impl Fn(u64) -> bool) -> u64 {
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if p(mid) {
            hi = mid;
        } else {
            lo = mid + 1;
        }
    }
    lo
}

/// The doubles' circle: `POINTS` points as `u64` words, transfers `Axis`,
/// monotone maps: the PULL of a segment is the guard's points between two
/// bisections, exactly; the image the interval between its ends' images.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Doubles;

impl Circle for Doubles {
    type P = u64;
    type W = u64;
    type Mono = Shift;
    const SIZE: u64 = POINTS;
    const MAX_SEGS: usize = 1 << 20;
    #[inline]
    fn next(p: u64) -> u64 {
        p + 1
    }
    #[inline]
    fn prev(p: u64) -> u64 {
        p - 1
    }
    #[inline]
    fn store(p: u64) -> u64 {
        p
    }
    #[inline]
    fn load(w: u64) -> u64 {
        w
    }
    #[inline]
    fn count(n: usize) -> u64 {
        n as u64
    }
    #[inline]
    fn len(w: u64) -> usize {
        w as usize
    }
    #[inline]
    fn wide(p: u64) -> u64 {
        p
    }
    #[inline]
    fn narrow(v: u64) -> u64 {
        v
    }
    const W_BYTES: usize = 8;
    fn put_word(w: u64, out: &mut Vec<u8>) {
        out.extend_from_slice(&w.to_le_bytes());
    }
    fn get_word(b: &[u8]) -> u64 {
        u64::from_le_bytes(b[..8].try_into().expect("8 bytes"))
    }
    #[inline]
    fn hash4(w: [u64; 4]) -> u64 {
        mix64(mix64(w[0] ^ w[1].rotate_left(32)) ^ w[2] ^ w[3].rotate_left(32))
    }
    #[inline]
    fn same4(a: [u64; 4], b: [u64; 4]) -> bool {
        a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3]
    }
    #[inline]
    fn apply(m: &Shift, i: u64) -> u64 {
        let v = mono(value(i), f64::from_bits(m.ox), m.k as f64);
        index(v).unwrap_or_else(|| panic!("transfer {m:?}: the image {v:?} of {:?} is not a remainder", value(i)))
    }
    /// The guard's points whose image is in `s`: past every point whose
    /// image is below `s.lo`, before the first whose image reaches `s.hi`.
    fn pull_mono(m: &Shift, g: Seg<u64>, s: Seg<u64>) -> Option<Seg<u64>> {
        let lo = first_in(g.lo, g.hi, |r| Self::apply(m, r) >= s.lo);
        let hi = first_in(lo, g.hi, |r| Self::apply(m, r) >= s.hi);
        (lo < hi).then_some(Seg { lo, hi })
    }
    /// A `Mono` collapsed onto its lowest point's image, a constant moved
    /// by one point.
    fn fault(a: &Act<Doubles>, g: Seg<u64>) -> Act<Doubles> {
        match a {
            Act::Mono(m) => Act::Const(Self::apply(m, g.lo)),
            Act::Const(c) => Act::Const((c + 1) % POINTS),
        }
    }
    #[inline]
    fn seg_word(s: Seg<u64>) -> u64 {
        mix64(s.lo) ^ s.hi.rotate_left(32)
    }
    fn mid(s: Seg<u64>) -> u64 {
        s.lo + (s.hi - s.lo) / 2
    }
}

/// Decode one axis (the module doc's rules). `Err` is a refusal.
pub(crate) fn decode_axis(c: &Capture<LuaF64>) -> Result<Axis, String> {
    let v = |p: (LuaF64, LuaF64)| (p.0.value(), p.1.value());
    let top = value(POINTS - 1);
    let whole = (-0.5, top);
    if !c.took {
        return match c.fin.map(v) {
            None => Ok(Axis::identity()),
            Some(f) if f == whole => Ok(Axis::identity()),
            Some((a, b)) if a == b => Axis::konst(0, POINTS, a),
            Some(f) => Err(format!("no player split, but the final remainder {f:?} is neither the input domain nor a point")),
        };
    }
    let (ox, oh) = v(c.ox);
    if ox != oh {
        return Err(format!("the applied speed {:?} is not exact", c.ox));
    }
    let pre = v(c.pre);
    let shifted = (-0.5 + ox, top + ox);
    let frag = v(c.frag);
    if pre.0 == pre.1 {
        if shifted.0 == shifted.1 {
            return Err(format!("the speed {ox:?} maps every remainder to one point: a reset and a move cannot be told apart"));
        }
        return set_before_the_move(pre.0, frag, c.fin.map(v));
    }
    if pre != shifted {
        return Err(format!("the split's argument {pre:?} is not the remainders shifted by the speed {ox:?}, {shifted:?} (the remainder at the move was not [-1/2, 1/2))"));
    }
    if !(pre.0 <= frag.0 && frag.0 <= frag.1 && frag.1 <= pre.1) {
        return Err(format!("the fragment {frag:?} is not inside the split's argument {pre:?}"));
    }
    let k = round(frag.0);
    if round(frag.1) != k {
        return Err(format!("the fragment {frag:?} spans two amounts"));
    }
    let lo = first(|i| value(i) + ox >= frag.0);
    let hi = first(|i| value(i) + ox > frag.1);
    if lo >= hi {
        return Err(format!("no remainder lands in the fragment {frag:?} at the speed {ox:?}"));
    }
    let image = (frag.0 - k, frag.1 - k);
    if index(image.0).is_none() || index(image.1).is_none() {
        return Err(format!("the fragment {frag:?}'s image {image:?} is not in [-1/2, 1/2)"));
    }
    let ki = k as i32;
    if ki as f64 != k {
        return Err(format!("the amount {k:?} is not an i32"));
    }
    match c.fin.map(v) {
        None => Ok(Axis::mono(lo, hi, ox, ki)),
        Some(fin) if fin == image => Ok(Axis::mono(lo, hi, ox, ki)),
        Some((a, b)) if a == b => Axis::konst(lo, hi, a),
        Some(fin) => Err(format!("the final remainder {fin:?} is neither the image {image:?} nor a point")),
    }
}

/// A split of ONE point (as `arc_edges::set_before_the_move`): every input
/// remainder takes the edge, to the constant `fin`.
fn set_before_the_move(pre: f64, frag: (f64, f64), fin: Option<(f64, f64)>) -> Result<Axis, String> {
    if frag != (pre, pre) {
        return Err(format!("the fragment {frag:?} is not the split's one-point argument {pre:?}"));
    }
    match fin {
        None => Ok(Axis::identity()),
        Some((a, b)) if a == b => Axis::konst(0, POINTS, a),
        Some(f) => Err(format!("the remainder was set before the move, but the final remainder {f:?} is not a point")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::lua_f64::LuaF64Interval as FI;

    type Arc = crate::search::hull::Arc<Doubles>;
    type Hull = crate::search::hull::Hull<Doubles>;
    type HullAcc = crate::search::hull::HullAcc<Doubles>;
    const HULL_BYTES: usize = Hull::BYTES;

    /// `s` clipped to the guard of `t`.
    fn clip(s: Seg<u64>, t: &Axis) -> Option<(u64, u64)> {
        let (lo, hi) = (s.lo.max(t.lo), s.hi.min(t.hi));
        (lo < hi).then_some((lo, hi))
    }

    /// The points of arc `a` inside the guard of `t`.
    fn meet(a: Arc, t: &Axis) -> u64 {
        let mut n = 0;
        a.segs(|s| n += clip(s, t).map_or(0, |c| c.1 - c.0));
        n
    }

    /// How many 16.16 remainders (multiples of `2^-16`) the points `[lo, hi)`
    /// hold: 0 for a piece only doubles take.
    fn fixed_points(lo: u64, hi: u64) -> u64 {
        if lo >= hi {
            return 0;
        }
        // The 16.16 remainders at or above `v`: from `ceil(v 2^16)` to `2^15 - 1`.
        let from = |i: u64| if i == POINTS { 32768 } else { (value(i) * 65536.0).ceil() as i64 };
        (from(hi) - from(lo)) as u64
    }

    fn n(v: f64) -> LuaF64 {
        LuaF64::new(v).unwrap()
    }

    fn lcg(s: &mut u64) -> u64 {
        *s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        *s >> 1
    }

    #[test]
    fn points_are_the_remainders_in_order() {
        assert_eq!(value(0), -0.5);
        assert_eq!(value(ZERO).to_bits(), 0);
        assert_eq!(value(POINTS - 1), 0.5 - f64::EPSILON / 4.0);
        assert_eq!((index(-0.0), index(0.0)), (Some(ZERO), Some(ZERO)));
        assert_eq!((index(0.5), index((-0.5f64).next_down())), (None, None));
        let mut s = 7u64;
        for _ in 0..10_000 {
            let (i, j) = (lcg(&mut s) % POINTS, lcg(&mut s) % POINTS);
            assert_eq!(index(value(i)), Some(i));
            assert_eq!(i < j, value(i) < value(j));
            if i + 1 < POINTS {
                assert_eq!(value(i + 1), value(i).next_up(), "{i}: neighbours are neighbours");
            }
        }
        assert_eq!(fixed_points(0, POINTS), 65536);
        assert_eq!(fixed_points(POINTS - 2, POINTS), 0, "the top sliver holds no 16.16 remainder");
        assert_eq!(fixed_points(0, 1), 1);
    }

    /// The capture the reference engine makes of `move` on one axis, over
    /// the whole domain at speed `ox`: per fragment of the round split
    /// (the domain's own `round_fragment`), with the image as `fin`.
    fn captures(ox: f64) -> Vec<Capture<LuaF64>> {
        let pre = FI::new(n(-0.5), n(value(POINTS - 1))).checked_add(FI::point(n(ox))).unwrap();
        let ways = pre.rounds().unwrap().unwrap();
        (0..ways)
            .map(|w| {
                let f = pre.round_fragment(w).unwrap();
                let k = round(f.lo.value());
                Capture { took: true, pre: (pre.lo, pre.hi), frag: (f.lo, f.hi), ox: (n(ox), n(ox)), fin: Some((n(f.lo.value() - k), n(f.hi.value() - k))) }
            })
            .collect()
    }

    /// THE DECODE AGAINST THE CART: the pieces partition the points, and
    /// at every probe of a piece (its ends, their neighbours, random
    /// points) the cart's `move` lands in the fragment and ends where the
    /// action says; just outside, it does not land there.
    #[test]
    fn the_pieces_partition_the_points_and_act_as_the_cart() {
        let mut s = 99u64;
        let mut speeds = vec![0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -4.0, 0.6, -0.6, 1.5, -1.5, 0.35, 2.9999999999999996, -5.0 + 1e-15, 0.1 + 0.2];
        speeds.extend((0..40).map(|_| (lcg(&mut s) % 1_000_000) as f64 / 99_991.0 - 5.0));
        for ox in speeds {
            let mut at = 0u64;
            for c in captures(ox) {
                let a = decode_axis(&c).unwrap_or_else(|e| panic!("ox {ox}: {e}"));
                assert_eq!(a.lo, at, "ox {ox}: the pieces are contiguous");
                at = a.hi;
                assert_eq!(a.tag, MONO);
                let (f0, f1) = (c.frag.0.value(), c.frag.1.value());
                let mut probes = vec![a.lo, a.hi - 1, a.lo + (a.hi - a.lo) / 2];
                probes.extend((0..8).map(|_| a.lo + lcg(&mut s) % (a.hi - a.lo)));
                for p in probes {
                    let r = value(p);
                    let r1 = r + ox;
                    assert!(f0 <= r1 && r1 <= f1, "ox {ox}: {r:?} is in the guard, but {r1:?} not in the fragment");
                    assert_eq!(value(a.apply(p)), r1 - round(r1), "ox {ox}: {r:?}");
                }
                for p in [a.lo.wrapping_sub(1), a.hi].into_iter().filter(|&p| p < POINTS) {
                    let r1 = value(p) + ox;
                    assert!(!(f0 <= r1 && r1 <= f1), "ox {ox}: {:?} outside the guard lands in the fragment", value(p));
                }
            }
            assert_eq!(at, POINTS, "ox {ox}: the pieces cover the points");
        }
    }

    /// The sliver at an integer speed: `fl(r - 2) = -3/2` for the top two
    /// doubles, a piece of its own onto `-1/2` that no 16.16 remainder
    /// takes; 16.16's one piece is the rest.
    #[test]
    fn an_integer_speed_has_the_top_sliver() {
        let pieces: Vec<Axis> = captures(-2.0).iter().map(|c| decode_axis(c).unwrap()).collect();
        assert_eq!(pieces.len(), 2);
        assert_eq!((pieces[1].lo, pieces[1].hi), (POINTS - 2, POINTS));
        assert_eq!(fixed_points(pieces[1].lo, pieces[1].hi), 0);
        assert_eq!((pieces[1].apply(POINTS - 2), pieces[1].apply(POINTS - 1)), (0, 0));
        assert_eq!(fixed_points(pieces[0].lo, pieces[0].hi), 65536);
        // A point hull at the top reaches only the sliver; one below it, only the rest.
        let t = |a: &Axis| (a.transfer(), Axis::identity().transfer());
        let mut acc = HullAcc::default();
        assert!(!Hull::point(POINTS - 3, ZERO).push(&t(&pieces[1]), &mut acc));
        assert!(Hull::point(POINTS - 2, ZERO).push(&t(&pieces[1]), &mut acc));
        assert_eq!(acc.hull(), Some(Hull::point(0, ZERO)));
    }

    #[test]
    fn what_the_model_does_not_cover_is_refused() {
        let c = captures(0.6)[0];
        // An inexact speed; a remainder narrower than the domain; a fragment over two amounts.
        assert!(decode_axis(&Capture { ox: (n(0.6), n(0.7)), ..c }).is_err());
        assert!(decode_axis(&Capture { pre: (n(0.2), n(0.9)), ..c }).is_err());
        assert!(decode_axis(&Capture { frag: (c.pre.0, c.pre.1), ..c }).is_err());
        // A final remainder that is neither the image nor a point.
        assert!(decode_axis(&Capture { fin: Some((n(0.0), n(0.1))), ..c }).is_err());
        // A collision's constant, and a death.
        assert_eq!(decode_axis(&Capture { fin: Some((n(0.0), n(0.0))), ..c }).unwrap().tag, CONST);
        assert_eq!(decode_axis(&Capture { fin: None, ..c }).unwrap().tag, MONO);
        // No split: the identity, a new player, or refused.
        let none = Capture { took: false, fin: Some((n(-0.5), n(value(POINTS - 1)))), ..c };
        assert_eq!(decode_axis(&none).unwrap(), Axis::identity());
        assert_eq!(decode_axis(&Capture { fin: Some((n(0.0), n(0.0))), ..none }).unwrap(), Axis { lo: 0, hi: POINTS, tag: CONST, a: ZERO, k: 0 });
        assert!(decode_axis(&Capture { fin: Some((n(0.0), n(0.25))), ..none }).is_err());
        // Set before the move: the whole guard, the constant.
        let set = Capture { took: true, pre: (n(0.0), n(0.0)), frag: (n(0.0), n(0.0)), ox: (n(0.0), n(0.0)), fin: Some((n(0.0), n(0.0))) };
        assert_eq!(decode_axis(&set).unwrap(), Axis { lo: 0, hi: POINTS, tag: CONST, a: ZERO, k: 0 });
        assert!(decode_axis(&Capture { frag: (n(0.0), n(0.1)), ..set }).is_err());
    }

    fn arc(s: &mut u64) -> Arc {
        let p = |s: &mut u64| match lcg(s) % 4 {
            0 => lcg(s) % 64,
            1 => POINTS - 1 - lcg(s) % 64,
            2 => ZERO - 32 + lcg(s) % 64,
            _ => lcg(s) % POINTS,
        };
        match lcg(s) % 5 {
            0 => Arc::point(p(s)),
            1 => Arc::full(),
            2 => Arc { start: p(s), last: lcg(s) % 256 },
            _ => Arc { start: p(s), last: lcg(s) % POINTS },
        }
    }

    /// THE PUSH IS SOUND: every probe of a hull inside a piece's guard has
    /// its image (the cart's) in the accumulated hull; a hull missing the
    /// guard pushes nothing; and the pushes of a grown hull that
    /// `push_grown` skips add nothing to the smaller one's.
    #[test]
    fn pushes_hold_every_image() {
        let mut s = 5u64;
        let mut pieces: Vec<Axis> = Vec::new();
        for ox in [0.0, -2.0, 1.0, 0.6, -1.3, 2.75, -0.35] {
            pieces.extend(captures(ox).iter().map(|c| decode_axis(c).unwrap()));
        }
        pieces.push(Axis { lo: 0, hi: POINTS, tag: CONST, a: ZERO, k: 0 });
        for _ in 0..3000 {
            let h = Hull { x: arc(&mut s), y: arc(&mut s) };
            let t = (pieces[(lcg(&mut s) % pieces.len() as u64) as usize], pieces[(lcg(&mut s) % pieces.len() as u64) as usize]);
            let mut acc = HullAcc::default();
            let tt = (t.0.transfer(), t.1.transfer());
            let pushed = h.push(&tt, &mut acc);
            assert_eq!(pushed, meet(h.x, &t.0) > 0 && meet(h.y, &t.1) > 0);
            if !pushed {
                assert!(acc.is_empty());
                continue;
            }
            let got = acc.hull().expect("pushed");
            for _ in 0..16 {
                let probe = |a: Arc, g: &Axis, s: &mut u64| {
                    let mut ps = Vec::new();
                    a.segs(|seg| {
                        if let Some((lo, hi)) = clip(seg, g) {
                            ps.extend([lo, hi - 1, lo + lcg(s) % (hi - lo)]);
                        }
                    });
                    ps
                };
                for x in probe(h.x, &t.0, &mut s) {
                    assert!(got.x.contains(t.0.apply(x)), "{h:?} {t:?}: x {x}");
                }
                for y in probe(h.y, &t.1, &mut s) {
                    assert!(got.y.contains(t.1.apply(y)), "{h:?} {t:?}: y {y}");
                }
            }
            // A smaller hull inside: what push_grown skips, its push already held.
            let shrink = |a: Arc, s: &mut u64| if a.last == 0 { a } else { Arc { start: a.start, last: lcg(s) % a.last } };
            let was = Hull { x: shrink(h.x, &mut s), y: shrink(h.y, &mut s) };
            let mut grown = HullAcc::default();
            if !h.push_grown(&was, &tt, &mut grown) {
                let mut old = HullAcc::default();
                assert!(was.push(&tt, &mut old), "{h:?} from {was:?}: skipped, but the smaller one never pushed");
                assert_eq!(old.hull(), acc.hull(), "{h:?} from {was:?}: skipped a push that grows");
            }
        }
    }

    /// THE PULL IS THE PREIMAGE, EXACTLY: a region of the doubles pulled
    /// through real pieces (`decode_axis` of the cart's captures), probed
    /// at the result's boundaries and their neighbours, the guards' ends
    /// and random points: a point is in the pull iff the guards take it and
    /// its image is in the region.
    #[test]
    fn a_doubles_region_pulls_to_its_exact_preimage() {
        use crate::search::arcs::{Piece, Region};
        let mut s = 17u64;
        let mut pieces: Vec<Axis> = Vec::new();
        for ox in [0.0, -2.0, 1.0, 0.6, -1.3, 2.75, -0.35, 3.0000000000000004] {
            pieces.extend(captures(ox).iter().map(|c| decode_axis(c).unwrap()));
        }
        pieces.push(Axis { lo: 0, hi: POINTS, tag: CONST, a: ZERO, k: 0 });
        let near = |s: &mut u64| match lcg(s) % 5 {
            0 => lcg(s) % 64,
            1 => POINTS - 1 - lcg(s) % 64,
            2 => ZERO - 32 + lcg(s) % 64,
            _ => lcg(s) % POINTS,
        };
        let seg = |s: &mut u64| {
            let (a, b) = (near(s), near(s));
            Seg { lo: a.min(b), hi: a.max(b) + 1 }
        };
        for _ in 0..300 {
            let mut ps: Vec<Piece<u64>> = (0..1 + lcg(&mut s) % 6).map(|_| Piece { y: seg(&mut s), x: seg(&mut s) }).collect();
            let r = Region::<Doubles>::from_pieces(&mut ps, &mut Vec::new());
            let (tx, ty) = (pieces[(lcg(&mut s) % pieces.len() as u64) as usize], pieces[(lcg(&mut s) % pieces.len() as u64) as usize]);
            let mut out = Vec::new();
            r.pull(tx.transfer(), ty.transfer(), &mut out);
            let pr = Region::<Doubles>::from_pieces(&mut out, &mut Vec::new());
            let around = |v: u64| [v.wrapping_sub(1), v, v + 1].into_iter().filter(|&p| p < POINTS);
            let mut px: Vec<u64> = vec![tx.lo, tx.hi - 1, near(&mut s), near(&mut s)];
            let mut py: Vec<u64> = vec![ty.lo, ty.hi - 1, near(&mut s), near(&mut s)];
            for (y, xs) in pr.slabs() {
                py.extend(around(y.lo).chain(around(y.hi)));
                for x in xs {
                    px.extend(around(x.lo).chain(around(x.hi)));
                }
            }
            for &x in &px {
                for &y in &py {
                    let takes = |a: &Axis, p: u64| a.lo <= p && p < a.hi;
                    let want = takes(&tx, x) && takes(&ty, y) && r.contains(tx.apply(x), ty.apply(y));
                    assert_eq!(pr.contains(x, y), want, "pull at ({x}, {y}) through {tx:?} {ty:?}");
                }
            }
        }
    }

    #[test]
    fn hulls_and_pairs_round_trip() {
        let mut s = 3u64;
        for _ in 0..100 {
            let h = Hull { x: arc(&mut s), y: arc(&mut s) };
            let mut b = Vec::new();
            h.to_bytes(&mut b);
            assert_eq!(b.len(), HULL_BYTES);
            assert_eq!(Hull::from_bytes(&b), h);
        }
        let p = (decode_axis(&captures(0.6)[1]).unwrap(), Axis { lo: 0, hi: POINTS, tag: CONST, a: 12345, k: 0 });
        let mut b = Vec::new();
        p.0.encode(&mut b);
        p.1.encode(&mut b);
        assert_eq!(b.len(), PAIR_BYTES);
        assert_eq!((Axis::decode(&b[..29]), Axis::decode(&b[29..])), p);
    }

    /// The accumulator is order-free and its hull holds what was added.
    #[test]
    fn the_accumulator_is_order_free() {
        let mut s = 11u64;
        for _ in 0..500 {
            let hs: Vec<Hull> = (0..4).map(|_| Hull { x: arc(&mut s), y: arc(&mut s) }).collect();
            let (mut a, mut b) = (HullAcc::default(), HullAcc::default());
            hs.iter().for_each(|h| a.add_hull(h));
            hs.iter().rev().for_each(|h| b.add_hull(h));
            assert_eq!(a, b);
            let got = a.hull().unwrap();
            for h in &hs {
                for p in [h.x.start, (h.x.start + h.x.last) % POINTS] {
                    assert!(got.x.contains(p));
                }
                for p in [h.y.start, (h.y.start + h.y.last) % POINTS] {
                    assert!(got.y.contains(p));
                }
            }
        }
    }
}
