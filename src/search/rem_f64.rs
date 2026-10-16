//! THE REMAINDER MODEL IN DOUBLES (plans/float.md, "The remainder model and the round fork"): the
//! float tools' sub-pixel remainder, its hulls and its transfers, as
//! `hull` and `arc_edges` are 16.16's.
//!
//! ## Points
//!
//! A remainder is a double of `[-1/2, 1/2)`, a POINT its INDEX in value
//! order: `0` is `-1/2`, `ZERO` is `0` (`-0.0` too: the two zeros are one
//! value, and no read of a remainder tells them apart, section 15), and
//! `POINTS - 1` is `1/2 - 2^-54`. The points form a circle as 16.16's do:
//! a hull is an ARC of it per axis, accumulated order-free in two charts,
//! cut at `-1/2` and at `0` (`hull`'s module doc, with `u64` coordinates).
//!
//! ## The move, and a transfer
//!
//! `move` is the original's `rem += ox; amount = flr(rem + 0.5); rem -=
//! amount`. Over the remainder `r` of a node (the whole `[-1/2, 1/2)`, the
//! level-0 widening) the reference engine forks `__split_by_round(rem)` at
//! the exact cut points, and its arc capture holds, per axis, the split's
//! argument `pre = fl(r + ox)` (an interval), the fragment `frag` the path
//! took (where `flr(fl(x + 1/2))` is one integer `k`), the speed `ox` and
//! the remainder `fin` at the frame's end. Decoded (`decode_axis`):
//!
//! * the GUARD: the remainders `r` with `fl(r + ox)` in `frag` - an
//!   order-interval (`fl(r + ox)` is monotone in `r`), found EXACTLY by
//!   bisection over the indices with the operation itself;
//! * the ACTION: `Mono(ox, k)`, `r -> fl(fl(r + ox) - k)` (the cart's two
//!   operations; monotone, not injective), where `fin` is the fragment's
//!   image `[fl(frag.lo - k), fl(frag.hi - k)]`; `Const(fin)` where a
//!   blocked step reset the remainder to the point `fin`; `Mono` also into
//!   a frame with no player at its end (a death: every transfer out of a
//!   player-less node has the whole guard, so any action is exact).
//! * No split (a freeze, the spawn): the whole guard, and the identity
//!   `Mono(0, 0)` (`fin` the whole input, or no player) or `Const(fin)`
//!   (a player made this frame).
//! * A split of ONE point: the remainder was SET before the move (a
//!   platform's carry blocked by a wall): the whole guard, `Const(fin)`.
//!
//! Anything else is REFUSED (fatal in the forward, as 16.16's).
//!
//! THE TOP SLIVER: with an integer speed `s`, the few doubles just below
//! `1/2` round onto the cut (`fl(r - 2) = -3/2` for `r` in `{1/2 - 2^-54,
//! 1/2 - 2^-53}`), a piece of its own that no 16.16 remainder takes
//! (`fixed_points`: section 15's census found it doubling the widened
//! forward). The hull forward pushes it only where a hull reaches it.

use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::mix64;

use super::arcs::{Circle, Seg};
use super::rem::Capture;

/// The index of `0` (and the count of doubles in `[-1/2, 0)`): `1/2`'s bits.
pub const ZERO: u64 = 0x3FE0_0000_0000_0000;
/// The points: the doubles of `[-1/2, 1/2)` by value.
pub const POINTS: u64 = 2 * ZERO;

/// The point of remainder `v`; `None` outside `[-1/2, 1/2)`.
#[inline]
pub fn index(v: f64) -> Option<u64> {
    if !(-0.5..0.5).contains(&v) {
        return None;
    }
    let b = v.abs().to_bits();
    Some(if v > 0.0 { ZERO + b } else { ZERO - b })
}

/// The remainder of point `i` (`+0.0` at `ZERO`).
#[inline]
pub fn value(i: u64) -> f64 {
    debug_assert!(i < POINTS);
    if i >= ZERO {
        f64::from_bits(i - ZERO)
    } else {
        -f64::from_bits(ZERO - i)
    }
}

/// How many 16.16 remainders (multiples of `2^-16`) the points `[lo, hi)`
/// hold: 0 for a piece only doubles take.
pub fn fixed_points(lo: u64, hi: u64) -> u64 {
    if lo >= hi {
        return 0;
    }
    // The 16.16 remainders at or above `v`: from `ceil(v 2^16)` to `2^15 - 1`.
    let from = |i: u64| if i == POINTS { 32768 } else { (value(i) * 65536.0).ceil() as i64 };
    (from(hi) - from(lo)) as u64
}

/// An ARC of the circle of points: `last + 1` points from `start`,
/// wrapping past the end.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Arc {
    pub start: u64,
    pub last: u64,
}

impl Arc {
    pub fn point(p: u64) -> Arc {
        debug_assert!(p < POINTS);
        Arc { start: p, last: 0 }
    }

    pub fn full() -> Arc {
        Arc { start: 0, last: POINTS - 1 }
    }

    #[inline]
    pub fn len(self) -> u64 {
        self.last + 1
    }

    #[inline]
    pub fn is_full(self) -> bool {
        self.len() == POINTS
    }

    #[inline]
    pub fn contains(self, p: u64) -> bool {
        (p + POINTS - self.start) % POINTS <= self.last
    }

    /// The arc as half-open segments of `0..POINTS` (one, or two across
    /// the end).
    #[inline]
    pub fn segs(self, mut f: impl FnMut((u64, u64))) {
        let (a, n) = (self.start, self.len());
        if a + n <= POINTS {
            f((a, a + n));
        } else {
            f((a, POINTS));
            f((0, a + n - POINTS));
        }
    }
}

/// A state's hull: one arc per axis.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Hull {
    pub x: Arc,
    pub y: Arc,
}

/// Bytes a hull takes on disk: `x.start, x.last, y.start, y.last`, `u64`
/// little-endian.
pub const HULL_BYTES: usize = 32;

impl Hull {
    pub fn point(x: u64, y: u64) -> Hull {
        Hull { x: Arc::point(x), y: Arc::point(y) }
    }

    pub fn full() -> Hull {
        Hull { x: Arc::full(), y: Arc::full() }
    }

    fn words(&self) -> [u64; 4] {
        [self.x.start, self.x.last, self.y.start, self.y.last]
    }

    pub fn to_bytes(&self, out: &mut Vec<u8>) {
        for w in self.words() {
            out.extend_from_slice(&w.to_le_bytes());
        }
    }

    pub fn from_bytes(b: &[u8]) -> Hull {
        let w = |i: usize| u64::from_le_bytes(b[8 * i..8 * i + 8].try_into().expect("8 bytes"));
        Hull { x: Arc { start: w(0), last: w(1) }, y: Arc { start: w(2), last: w(3) } }
    }

    pub fn mixed(&self) -> u64 {
        let w = self.words();
        mix64(mix64(w[0] ^ w[1].rotate_left(32)) ^ w[2] ^ w[3].rotate_left(32))
    }

    /// THE PUSH along an edge of transfer `t` (`hull::Hull::push`): per
    /// axis the arc's points inside the guard, moved by the action, into
    /// `acc`; nothing where either axis's guard misses.
    #[inline]
    pub fn push(&self, t: &(Axis, Axis), acc: &mut HullAcc) -> bool {
        let mut xs = [(0, 0); 2];
        let nx = image(self.x, &t.0, &mut xs);
        if nx == 0 {
            return false;
        }
        let mut ys = [(0, 0); 2];
        let ny = image(self.y, &t.1, &mut ys);
        if ny == 0 {
            return false;
        }
        xs[..nx].iter().for_each(|&s| acc.add_seg(0, s));
        ys[..ny].iter().for_each(|&s| acc.add_seg(1, s));
        true
    }

    /// The push of a GROWN hull (`hull::Hull::push_grown`): only where the
    /// box was empty and is not, or an axis with a `Mono` action meets
    /// more of its guard than `was` did (the same points have the same
    /// image; a constant's image is its point either way).
    #[inline]
    pub fn push_grown(&self, was: &Hull, t: &(Axis, Axis), acc: &mut HullAcc) -> bool {
        let (nx, ny) = (meet(self.x, &t.0), meet(self.y, &t.1));
        if nx == 0 || ny == 0 {
            return false;
        }
        let (ox, oy) = (meet(was.x, &t.0), meet(was.y, &t.1));
        let grows = |a: &Axis, n: u64, o: u64| a.tag == MONO && n != o;
        if ox != 0 && oy != 0 && !grows(&t.0, nx, ox) && !grows(&t.1, ny, oy) {
            return false;
        }
        self.push(t, acc)
    }
}

/// `s` clipped to the guard of `t`.
#[inline]
fn clip(s: (u64, u64), t: &Axis) -> Option<(u64, u64)> {
    let (lo, hi) = (s.0.max(t.lo), s.1.min(t.hi));
    (lo < hi).then_some((lo, hi))
}

/// The points of arc `a` inside the guard of `t`.
#[inline]
fn meet(a: Arc, t: &Axis) -> u64 {
    let mut n = 0;
    a.segs(|s| n += clip(s, t).map_or(0, |c| c.1 - c.0));
    n
}

/// The image of arc `a` under axis transfer `t`, as segments into `out`;
/// their count (0: the guard misses the arc). A `Mono` piece is monotone,
/// so a segment's image lies between its ends' images.
#[inline]
fn image(a: Arc, t: &Axis, out: &mut [(u64, u64); 2]) -> usize {
    let (mut n, mut hit) = (0usize, false);
    a.segs(|s| {
        if let Some((lo, hi)) = clip(s, t) {
            hit = true;
            if t.tag == MONO {
                let (x, y) = (t.apply(lo), t.apply(hi - 1));
                debug_assert!(x <= y, "a Mono transfer is monotone");
                out[n] = (x, y + 1);
                n += 1;
            }
        }
    });
    match t.tag {
        MONO => n,
        _ if hit => {
            out[0] = (t.a, t.a + 1);
            1
        }
        _ => 0,
    }
}

/// The charts' cut points (`hull`'s `CUT`): `-1/2` and `0`.
const CUT: [u64; 2] = [0, ZERO];

/// A HULL BEING ACCUMULATED (`hull::HullAcc`): per axis and chart the
/// linear hull `[lo, hi]` in the chart's coordinates (`lo > hi`: none),
/// merged by min and max.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HullAcc {
    lo: [u64; 4],
    hi: [u64; 4],
}

impl Default for HullAcc {
    fn default() -> Self {
        HullAcc { lo: [u64::MAX; 4], hi: [0; 4] }
    }
}

impl HullAcc {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.lo[0] > self.hi[0] || self.lo[2] > self.hi[2]
    }

    /// Add the half-open segment `s` of axis `axis` (0: x, 1: y).
    #[inline]
    pub fn add_seg(&mut self, axis: usize, s: (u64, u64)) {
        debug_assert!(s.0 < s.1 && s.1 <= POINTS);
        for (c, cut) in CUT.into_iter().enumerate() {
            let a = (s.0 + POINTS - cut) % POINTS;
            let b = (s.1 - 1 + POINTS - cut) % POINTS;
            let (lo, hi) = if a <= b { (a, b) } else { (0, POINTS - 1) };
            let i = axis * 2 + c;
            self.lo[i] = self.lo[i].min(lo);
            self.hi[i] = self.hi[i].max(hi);
        }
    }

    #[inline]
    pub fn add_hull(&mut self, h: &Hull) {
        h.x.segs(|s| self.add_seg(0, s));
        h.y.segs(|s| self.add_seg(1, s));
    }

    #[inline]
    pub fn merge(&mut self, o: &HullAcc) {
        for i in 0..4 {
            self.lo[i] = self.lo[i].min(o.lo[i]);
            self.hi[i] = self.hi[i].max(o.hi[i]);
        }
    }

    /// The hull: per axis the shorter chart's arc (chart 0 on a tie).
    pub fn hull(&self) -> Option<Hull> {
        if self.is_empty() {
            return None;
        }
        let axis = |a: usize| {
            let mut best: Option<Arc> = None;
            for (c, cut) in CUT.into_iter().enumerate() {
                let (lo, hi) = (self.lo[a * 2 + c], self.hi[a * 2 + c]);
                let len = hi - lo + 1;
                if best.is_none_or(|b| len < b.len()) {
                    best = Some(if len == POINTS { Arc::full() } else { Arc { start: (lo + cut) % POINTS, last: len - 1 } });
                }
            }
            best.expect("two charts")
        };
        Some(Hull { x: axis(0), y: axis(1) })
    }
}

/// `Axis::tag` of `r -> fl(fl(r + ox) - k)` (`a` the bits of `ox`).
pub const MONO: u8 = 0;
/// `Axis::tag` of the constant point `a`.
pub const CONST: u8 = 1;

/// One axis's decoded transfer: the guard `[lo, hi)` of the points, and
/// the action (`tag`, `a`, `k`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Axis {
    pub lo: u64,
    pub hi: u64,
    pub tag: u8,
    pub a: u64,
    pub k: i32,
}

/// A transfer pair (x, y).
pub type Pair = (Axis, Axis);

/// Bytes per encoded pair: per axis `lo`, `hi`, `a` (u64), `tag` (u8), `k` (i32).
pub const PAIR_BYTES: usize = 2 * 29;

impl Axis {
    /// The identity over the whole circle.
    pub fn identity() -> Axis {
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
    #[inline]
    pub fn apply(&self, i: u64) -> u64 {
        debug_assert!(self.lo <= i && i < self.hi);
        match self.tag {
            MONO => {
                let v = mono(value(i), f64::from_bits(self.a), self.k as f64);
                index(v).unwrap_or_else(|| panic!("transfer {self:?}: the image {v:?} of {:?} is not a remainder", value(i)))
            }
            _ => self.a,
        }
    }

    pub fn encode(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.lo.to_le_bytes());
        out.extend_from_slice(&self.hi.to_le_bytes());
        out.extend_from_slice(&self.a.to_le_bytes());
        out.push(self.tag);
        out.extend_from_slice(&self.k.to_le_bytes());
    }

    pub fn decode(b: &[u8]) -> Axis {
        let u = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().expect("8 bytes"));
        Axis { lo: u(0), hi: u(8), a: u(16), tag: b[24], k: i32::from_le_bytes(b[25..29].try_into().expect("4 bytes")) }
    }

    pub fn mixed(&self) -> u64 {
        mix64(self.lo ^ self.hi.rotate_left(21) ^ mix64(self.a ^ ((self.k as u32 as u64) << 8 | self.tag as u64)))
    }
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

/// The first point from which `p` holds (`p` false then true over the
/// points: monotone), by bisection.
fn first(p: impl Fn(u64) -> bool) -> u64 {
    first_in(0, POINTS, p)
}

/// The first point of `lo..hi` from which `p` holds (`hi`: none), `p` false
/// then true there.
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

/// The doubles' circle (`arcs::Circle`): the `POINTS` points, stored as
/// `u64` words, the transfers `Axis` - monotone maps, so the PULL of a
/// segment is the guard's points between two bisections, exactly; the
/// image of a segment is the interval between its ends' images.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Doubles;

impl Circle for Doubles {
    type P = u64;
    type W = u64;
    type Act = Axis;
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
    fn guard(a: &Axis) -> Seg<u64> {
        Seg { lo: a.lo, hi: a.hi }
    }
    #[inline]
    fn const_point(a: &Axis) -> Option<u64> {
        (a.tag == CONST).then_some(a.a)
    }
    fn image(a: &Axis, s: Seg<u64>, mut f: impl FnMut(Seg<u64>)) {
        let (lo, hi) = (s.lo.max(a.lo), s.hi.min(a.hi));
        if lo < hi {
            f(if a.tag == MONO { Seg { lo: a.apply(lo), hi: a.apply(hi - 1) + 1 } } else { Seg { lo: a.a, hi: a.a + 1 } });
        }
    }
    fn preimage(a: &Axis, s: Seg<u64>, mut f: impl FnMut(Seg<u64>)) {
        if a.tag != MONO {
            if s.lo <= a.a && a.a < s.hi {
                f(Seg { lo: a.lo, hi: a.hi });
            }
            return;
        }
        // The guard's points whose image is in `s`: past every point whose
        // image is below `s.lo`, before the first whose image reaches `s.hi`.
        let lo = first_in(a.lo, a.hi, |r| a.apply(r) >= s.lo);
        let hi = first_in(lo, a.hi, |r| a.apply(r) >= s.hi);
        if lo < hi {
            f(Seg { lo, hi });
        }
    }
    #[inline]
    fn seg_word(s: Seg<u64>) -> u64 {
        mix64(s.lo) ^ s.hi.rotate_left(32)
    }
    fn mid(s: Seg<u64>) -> u64 {
        s.lo + (s.hi - s.lo) / 2
    }
    fn kind(a: &Axis) -> &'static str {
        if a.tag == MONO {
            "mono"
        } else {
            "const"
        }
    }
    /// The guard's lowest point dropped, or a `Mono` collapsed onto its
    /// lowest point's image (wrong wherever the image moves), a constant
    /// moved by one point.
    fn faulty(a: &Axis, guard: bool) -> Axis {
        let mut t = *a;
        if guard {
            t.lo = (t.lo + 1).min(t.hi - 1);
        } else if t.tag == MONO {
            t = Axis { tag: CONST, a: a.apply(a.lo), k: 0, ..t };
        } else {
            t.a = (t.a + 1) % POINTS;
        }
        t
    }
}

/// Decode one axis (the module doc's rules). `Err` is a refusal.
pub fn decode_axis(c: &Capture<LuaF64>) -> Result<Axis, String> {
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

/// A split of ONE point (`arc_edges`' `set_before_the_move`): the
/// remainder was set before the move, so every input remainder takes the
/// edge, to the constant `fin`.
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
        let t = |a: &Axis| (*a, Axis::identity());
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
            let pushed = h.push(&t, &mut acc);
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
            if !h.push_grown(&was, &t, &mut grown) {
                let mut old = HullAcc::default();
                assert!(was.push(&t, &mut old), "{h:?} from {was:?}: skipped, but the smaller one never pushed");
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
            r.pull(tx, ty, &mut out);
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
