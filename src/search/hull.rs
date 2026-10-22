//! A STATE'S REMAINDER HULL (plans/architecture.md "The forward: hulls and
//! storage"): the remainders a state has been reached with, over-
//! approximated by ONE ARC of the circle per axis, pushed along the recorded
//! edges (`Hull::push`) and grown to cover what a target is pushed
//! (`HullAcc`). Sound: every remainder a concrete path reaches a state with
//! is in its hull (the push of a superset is a superset of the push, the
//! hull of a union a superset of it). 8 B in 16.16, 32 B in doubles.
//!
//! THE MERGE IS ORDER-FREE: the pushes into a target come from region jobs
//! in any order, and "the smallest arc holding two arcs" is not associative
//! on a circle. So a target accumulates per axis two CHARTS (the circle cut
//! at its first point and at its middle, each a linear interval) merged by
//! min and max; the arc is the shorter chart's. That is the smallest arc
//! holding the union when the union fits in half the circle (its largest
//! gap, longer than half, holds a cut), and a superset otherwise.

use super::arcs::{clip, Circle, Fixed16, Seg, Transfer};
#[cfg(test)]
use super::arcs::{Piece, Region};
use super::rem::{Rem, Word};
use celeste_core::pico8_num::Pico8Num as P8;

/// The points of `C`: `0..size`.
#[inline]
fn size<C: Circle>() -> u64 {
    C::wide(C::SIZE)
}

/// A number below `size` as a stored word.
#[inline]
fn word<C: Circle>(v: u64) -> C::W {
    C::store(C::narrow(v))
}

#[inline]
fn num<C: Circle>(w: C::W) -> u64 {
    C::wide(C::load(w))
}

/// An ARC of the circle: `last + 1` points from `start`, wrapping past the
/// end (the whole circle starts at the first point).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[repr(C)]
pub(crate) struct Arc<C: Circle = Fixed16> {
    pub(crate) start: C::W,
    pub(crate) last: C::W,
}

impl<C: Circle> Arc<C> {
    pub(crate) fn point(p: C::P) -> Self {
        debug_assert!(p < C::SIZE);
        Arc { start: C::store(p), last: word::<C>(0) }
    }

    pub(crate) fn full() -> Self {
        Arc { start: word::<C>(0), last: word::<C>(size::<C>() - 1) }
    }

    /// Its number of points.
    #[inline]
    pub(crate) fn len(self) -> u64 {
        num::<C>(self.last) + 1
    }

    #[inline]
    pub(crate) fn is_full(self) -> bool {
        self.len() == size::<C>()
    }

    #[inline]
    pub(crate) fn contains(self, p: C::P) -> bool {
        (C::wide(p) + size::<C>() - num::<C>(self.start)) % size::<C>() <= num::<C>(self.last)
    }

    /// Does it hold every point of segment `s`?
    #[cfg(test)]
    pub(crate) fn contains_seg(self, s: Seg<C::P>) -> bool {
        let at = |p: u64| (p + size::<C>() - num::<C>(self.start)) % size::<C>();
        let (lo, last) = (C::wide(s.lo), C::wide(s.hi) - 1);
        let (a, b) = (at(lo), at(last));
        a <= b && b <= num::<C>(self.last) && b - a == last - lo
    }

    /// The arc as segments of the circle (one, or two across the end).
    #[inline]
    pub(crate) fn segs(self, mut f: impl FnMut(Seg<C::P>)) {
        let (a, n, end) = (num::<C>(self.start), self.len(), size::<C>());
        let seg = |lo: u64, hi: u64| Seg { lo: C::narrow(lo), hi: C::narrow(hi) };
        if a + n <= end {
            f(seg(a, a + n));
        } else {
            f(seg(a, end));
            f(seg(0, a + n - end));
        }
    }
}

/// A state's HULL: one arc per axis, the box of its remainders. Never empty.
#[derive(Clone, Copy, Debug, Hash, PartialOrd, Ord, Default)]
#[repr(C)]
pub struct Hull<C: Circle = Fixed16> {
    pub(crate) x: Arc<C>,
    pub(crate) y: Arc<C>,
}

/// Equal by `Circle::same4` (16.16's one 8-byte compare: field by field
/// cost the push cache's lookup 2.6% of a big room's frame).
impl<C: Circle> PartialEq for Hull<C> {
    #[inline]
    fn eq(&self, o: &Self) -> bool {
        <C::W as Word>::same4(self.words(), o.words())
    }
}

impl<C: Circle> Eq for Hull<C> {}

pub(crate) type HullOf<N> = Hull<<N as Rem>::C>;

pub(crate) type AccOf<N> = HullAcc<<N as Rem>::C>;

impl<C: Circle> Hull<C> {
    /// Bytes a hull takes on disk (`to_bytes`).
    pub(crate) const BYTES: usize = 4 * <C::W as Word>::BYTES;

    /// The point `(x, y)` (a start's exact remainder).
    pub(crate) fn point(x: C::P, y: C::P) -> Self {
        Hull { x: Arc::point(x), y: Arc::point(y) }
    }

    pub(crate) fn full() -> Self {
        Hull { x: Arc::full(), y: Arc::full() }
    }

    pub(crate) fn is_full(&self) -> bool {
        self.x.is_full() && self.y.is_full()
    }

    #[inline]
    pub(crate) fn contains(&self, x: C::P, y: C::P) -> bool {
        self.x.contains(x) && self.y.contains(y)
    }

    /// The hull as a set of the torus (`arcs::Region`).
    #[cfg(test)]
    pub(crate) fn to_region(&self) -> Region<C> {
        let mut pieces = Vec::new();
        self.y.segs(|y| self.x.segs(|x| pieces.push(Piece { y, x })));
        Region::from_pieces(&mut pieces, &mut Vec::new())
    }

    /// THE PUSH along an edge of transfer `t` into `acc`: per axis the arc's
    /// points inside the guard, moved by the action. Whether it pushed (not
    /// where either guard misses the arc: a box with an empty side is empty).
    #[inline]
    pub(crate) fn push(&self, t: &(Transfer<C>, Transfer<C>), acc: &mut HullAcc<C>) -> bool {
        let mut xs = [Seg::default(); 4];
        let nx = image(self.x, &t.0, &mut xs);
        if nx == 0 {
            return false;
        }
        let mut ys = [Seg::default(); 4];
        let ny = image(self.y, &t.1, &mut ys);
        if ny == 0 {
            return false;
        }
        for &s in &xs[..nx] {
            acc.add_seg(0, s);
        }
        for &s in &ys[..ny] {
            acc.add_seg(1, s);
        }
        true
    }

    /// THE PUSH of a GROWN hull (`was`, the hull it last pushed, inside it):
    /// `push`, only where the image grows past `was`'s, whose images were all
    /// pushed then (one dropped by level -1 or the ladder's filter is dropped
    /// at every later frame too). It grows when the box was empty and is not,
    /// or an axis not sent to a constant (its point either way) meets more of
    /// the guard.
    #[inline]
    pub(crate) fn push_grown(&self, was: &Hull<C>, t: &(Transfer<C>, Transfer<C>), acc: &mut HullAcc<C>) -> bool {
        let (nx, ny) = (meet(self.x, &t.0), meet(self.y, &t.1));
        if nx == 0 || ny == 0 {
            return false;
        }
        let (ox, oy) = (meet(was.x, &t.0), meet(was.y, &t.1));
        let grows = |a: &Transfer<C>, n: u64, o: u64| a.const_point().is_none() && n != o;
        if ox != 0 && oy != 0 && !grows(&t.0, nx, ox) && !grows(&t.1, ny, oy) {
            return false;
        }
        self.push(t, acc)
    }

    /// `x.start, x.last, y.start, y.last`.
    #[inline]
    fn words(&self) -> [C::W; 4] {
        [self.x.start, self.x.last, self.y.start, self.y.last]
    }

    /// Its hash (`Circle::hash4`: 16.16's is the four words in one).
    #[inline]
    pub(crate) fn hash(&self) -> u64 {
        <C::W as Word>::hash4(self.words())
    }

    /// Append its `BYTES`: its words, little-endian.
    pub(crate) fn to_bytes(&self, out: &mut Vec<u8>) {
        for w in self.words() {
            w.put(out);
        }
    }

    pub(crate) fn from_bytes(b: &[u8]) -> Self {
        let w = |i: usize| <C::W as Word>::read(&b[i * <C::W as Word>::BYTES..]);
        Hull { x: Arc { start: w(0), last: w(1) }, y: Arc { start: w(2), last: w(3) } }
    }
}

/// The points of arc `a` inside the guard of `t`.
#[inline]
fn meet<C: Circle>(a: Arc<C>, t: &Transfer<C>) -> u64 {
    let mut n = 0;
    a.segs(|s| {
        if let Some(c) = clip(s, t.guard) {
            n += C::wide(c.hi) - C::wide(c.lo);
        }
    });
    n
}

/// The image of arc `a` under `t` into `out`; its count (0: guard missed).
#[inline]
fn image<C: Circle>(a: Arc<C>, t: &Transfer<C>, out: &mut [Seg<C::P>; 4]) -> usize {
    let mut n = 0usize;
    a.segs(|s| {
        if let Some(i) = t.image(s) {
            out[n] = i;
            n += 1;
        }
    });
    n
}

/// A direct-mapped cache `(hull, the hull it last pushed, transfer id) ->
/// the push` (`None`: nothing): sources share hulls and transfers, and a
/// push depends on nothing else.
pub(crate) struct PushCache<N: Rem = P8> {
    slots: Vec<Slot<N::C>>,
}

/// `(hull, last pushed hull, transfer, push)`.
type Slot<C> = (Hull<C>, Hull<C>, u32, Option<HullAcc<C>>);

/// Entries of a `PushCache` (a power of two).
const PUSH_CACHE: usize = 1 << 11;

impl<N: Rem> Default for PushCache<N> {
    fn default() -> Self {
        PushCache { slots: vec![(Hull::default(), Hull::default(), u32::MAX, None); PUSH_CACHE] }
    }
}

impl<N: Rem> PushCache<N> {
    /// The push of `h` (`was`: its last pushed hull; itself for a hull
    /// pushed the first time) along transfer `x`, `f()` on a miss.
    #[inline]
    pub(crate) fn get(&mut self, h: &HullOf<N>, was: &HullOf<N>, x: u32, f: impl FnOnce() -> Option<AccOf<N>>) -> Option<AccOf<N>> {
        let (a, b) = (h.hash(), was.hash());
        let k = ((a ^ b.rotate_left(29) ^ (x as u64) << 45).wrapping_mul(0x9e37_79b9_7f4a_7c15) >> (64 - PUSH_CACHE.trailing_zeros())) as usize;
        let e = &mut self.slots[k];
        if (e.0, e.1, e.2) == (*h, *was, x) {
            return e.3;
        }
        let r = f();
        *e = (*h, *was, x, r);
        r
    }
}

/// TESTS ONLY: every hull (and the start's) WIDENED to the whole circle: the
/// remainder-free forward, which the hull forward must stay inside
/// (`frame::tests`). Process-global: nextest runs each test in its process.
#[cfg(test)]
pub(crate) static WIDEN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Chart `c`'s cut (the first point, the middle): `p`'s coordinate in it is
/// `(p - cut(c)) mod size`.
#[inline]
fn cut<C: Circle>(c: usize) -> u64 {
    c as u64 * (size::<C>() / 2)
}

/// A HULL BEING ACCUMULATED: per axis and chart (`axis * 2 + chart`) the
/// interval `[lo, hi]` of every point added (`lo > hi`: none).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct HullAcc<C: Circle = Fixed16> {
    lo: [C::W; 4],
    hi: [C::W; 4],
}

impl<C: Circle> Default for HullAcc<C> {
    /// Nothing added.
    fn default() -> Self {
        HullAcc { lo: [word::<C>(size::<C>() - 1); 4], hi: [word::<C>(0); 4] }
    }
}

impl<C: Circle> HullAcc<C> {
    /// Empty (on either axis: a box with an empty side is empty)?
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.lo[0] > self.hi[0] || self.lo[2] > self.hi[2]
    }

    /// Add segment `s` of axis `axis` (0: x, 1: y).
    #[inline]
    pub(crate) fn add_seg(&mut self, axis: usize, s: Seg<C::P>) {
        let n = size::<C>();
        let (lo, last) = (C::wide(s.lo), C::wide(s.hi) - 1);
        debug_assert!(lo <= last && last < n);
        for c in 0..2 {
            let a = (lo + n - cut::<C>(c)) % n;
            let b = (last + n - cut::<C>(c)) % n;
            let (lo, hi) = if a <= b { (a, b) } else { (0, n - 1) };
            let i = axis * 2 + c;
            self.lo[i] = self.lo[i].min(word::<C>(lo));
            self.hi[i] = self.hi[i].max(word::<C>(hi));
        }
    }

    /// Add every point of `h`.
    #[inline]
    pub(crate) fn add_hull(&mut self, h: &Hull<C>) {
        h.x.segs(|s| self.add_seg(0, s));
        h.y.segs(|s| self.add_seg(1, s));
    }

    /// Add everything `o` holds.
    #[inline]
    pub(crate) fn merge(&mut self, o: &HullAcc<C>) {
        for i in 0..4 {
            self.lo[i] = self.lo[i].min(o.lo[i]);
            self.hi[i] = self.hi[i].max(o.hi[i]);
        }
    }

    /// Per axis the shorter chart's arc (chart 0 on a tie); `None`: empty.
    pub(crate) fn hull(&self) -> Option<Hull<C>> {
        if self.is_empty() {
            return None;
        }
        #[cfg(test)]
        if WIDEN.load(std::sync::atomic::Ordering::Relaxed) {
            return Some(Hull::full());
        }
        let n = size::<C>();
        let axis = |a: usize| {
            let mut best: Option<Arc<C>> = None;
            for c in 0..2 {
                let (lo, hi) = (num::<C>(self.lo[a * 2 + c]), num::<C>(self.hi[a * 2 + c]));
                let len = hi - lo + 1;
                if best.is_none_or(|b| len < b.len()) {
                    best = Some(if len == n { Arc::full() } else { Arc { start: word::<C>((lo + cut::<C>(c)) % n), last: word::<C>(len - 1) } });
                }
            }
            best.expect("two charts")
        };
        Some(Hull { x: axis(0), y: axis(1) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::arcs::{Act, Transfer, CIRCLE};

    fn lcg(s: &mut u64) -> u32 {
        (crate::testutil::lcg(s) >> 33) as u32
    }

    fn arc(s: &mut u64) -> Arc {
        match lcg(s) % 6 {
            0 => Arc::point(lcg(s) % CIRCLE),
            1 => Arc::full(),
            2 => Arc { start: (lcg(s) % CIRCLE) as u16, last: (lcg(s) % 256) as u16 },
            _ => Arc { start: (lcg(s) % CIRCLE) as u16, last: (lcg(s) % CIRCLE) as u16 },
        }
    }

    fn transfer(s: &mut u64) -> Transfer {
        let (a, b) = (lcg(s) % CIRCLE, lcg(s) % CIRCLE);
        let guard = match lcg(s) % 4 {
            0 => Seg { lo: 0, hi: CIRCLE },
            1 => Seg { lo: 0, hi: a.max(1) },
            2 => Seg { lo: a, hi: CIRCLE },
            _ => Seg { lo: a.min(b), hi: a.max(b) + 1 },
        };
        // A translation keeping the guard inside the circle.
        let room = (CIRCLE - guard.hi + guard.lo) as i64 + 1;
        let act = if lcg(s) % 4 == 0 { Act::Const(lcg(s) % CIRCLE) } else { Act::Mono((lcg(s) as i64 % room - guard.lo as i64) as i32) };
        Transfer { guard, act }
    }

    /// Probes: the arcs' ends and neighbours, and random points.
    fn probes(arcs: &[Arc], s: &mut u64) -> Vec<u32> {
        let mut v: Vec<u32> = (0..16).map(|_| lcg(s) % CIRCLE).collect();
        for a in arcs {
            let (lo, hi) = (a.start as u32, (a.start as u32 + a.last as u32) % CIRCLE);
            for p in [lo, hi] {
                v.extend([(p + CIRCLE - 1) % CIRCLE, p, (p + 1) % CIRCLE]);
            }
        }
        v.sort_unstable();
        v.dedup();
        v
    }

    /// A push is a superset of the exact image (`Region::push` of the
    /// hull's box), empty exactly when the image is; the accumulated hull
    /// does not depend on the order the pushes come in.
    #[test]
    fn a_push_holds_the_image_and_the_merge_is_order_free() {
        let mut s = 11u64;
        for _ in 0..3000 {
            let h = Hull { x: arc(&mut s), y: arc(&mut s) };
            let (tx, ty) = (transfer(&mut s), transfer(&mut s));
            let mut acc = HullAcc::default();
            let pushed = h.push(&(tx, ty), &mut acc);
            let mut img = Vec::new();
            h.to_region().as_ref().push(tx, ty, &mut img);
            let img = Region::<Fixed16>::from_pieces(&mut img, &mut Vec::new());
            assert_eq!(pushed, !img.is_empty(), "{h:?} {tx:?} {ty:?}");
            assert_eq!(acc.is_empty(), !pushed);
            let Some(got) = acc.hull() else { continue };
            // Every point of the image is in the hull: probed at the image's
            // slab and segment ends.
            for (ys, xs) in img.as_ref().slabs() {
                for x in xs {
                    for (px, py) in [(x.lo, ys.lo), (x.hi - 1, ys.hi - 1), (x.lo, ys.hi - 1), (x.hi - 1, ys.lo)] {
                        assert!(got.contains(px, py), "{h:?} {tx:?} {ty:?}: ({px}, {py}) of the image is not in {got:?}");
                    }
                }
            }
        }
        // A grown hull's push: nothing where the image did not grow; else
        // the whole push.
        for _ in 0..3000 {
            let was = Hull { x: arc(&mut s), y: arc(&mut s) };
            let mut grow = HullAcc::default();
            grow.add_hull(&was);
            grow.add_hull(&Hull { x: arc(&mut s), y: arc(&mut s) });
            let h = grow.hull().unwrap();
            let (tx, ty) = (transfer(&mut s), transfer(&mut s));
            let (mut a, mut b, mut old) = (HullAcc::default(), HullAcc::default(), HullAcc::default());
            let full = h.push(&(tx, ty), &mut a);
            let grown = h.push_grown(&was, &(tx, ty), &mut b);
            if grown {
                assert_eq!(a, b, "a grown push is the whole push");
            } else if full {
                // Skipped: the old push's image already holds the new one.
                assert!(was.push(&(tx, ty), &mut old), "skipped a push whose old image was empty");
                let (mut img_new, mut img_old) = (Vec::new(), Vec::new());
                h.to_region().as_ref().push(tx, ty, &mut img_new);
                was.to_region().as_ref().push(tx, ty, &mut img_old);
                assert_eq!(Region::<Fixed16>::from_pieces(&mut img_new, &mut Vec::new()), Region::<Fixed16>::from_pieces(&mut img_old, &mut Vec::new()), "{was:?} -> {h:?} {tx:?} {ty:?}: skipped a push whose image grew");
            }
        }
        // Order: many pushes into one accumulator, forwards and backwards.
        for _ in 0..300 {
            let pushes: Vec<(Hull, Transfer, Transfer)> = (0..1 + lcg(&mut s) % 9).map(|_| (Hull { x: arc(&mut s), y: arc(&mut s) }, transfer(&mut s), transfer(&mut s))).collect();
            let (mut a, mut b) = (HullAcc::default(), HullAcc::default());
            for p in &pushes {
                p.0.push(&(p.1, p.2), &mut a);
            }
            for p in pushes.iter().rev() {
                let mut one = HullAcc::default();
                p.0.push(&(p.1, p.2), &mut one);
                b.merge(&one);
            }
            assert_eq!(a, b);
            assert_eq!(a.hull(), b.hull());
        }
    }

    /// The hull of a union of arcs holds every one of them; it is the
    /// smallest arc holding them where that is under half the circle; a
    /// hull merged with arcs inside it stays itself.
    #[test]
    fn the_hull_of_arcs_is_the_smallest_under_half_the_circle() {
        let mut s = 5u64;
        for _ in 0..3000 {
            let n = 1 + lcg(&mut s) % 6;
            // Arcs inside one window of random size, at a random place.
            let w = 1 + lcg(&mut s) % (CIRCLE - 1);
            let at = lcg(&mut s) % CIRCLE;
            let arcs: Vec<Arc> = (0..n)
                .map(|_| {
                    let off = lcg(&mut s) % w;
                    let len = 1 + lcg(&mut s) % (w - off);
                    Arc { start: ((at + off) % CIRCLE) as u16, last: (len - 1) as u16 }
                })
                .collect();
            let mut acc = HullAcc::<Fixed16>::default();
            for a in &arcs {
                a.segs(|g| acc.add_seg(0, g));
                a.segs(|g| acc.add_seg(1, g));
            }
            let h = acc.hull().expect("points added");
            for p in probes(&arcs, &mut s) {
                if arcs.iter().any(|a| a.contains(p)) {
                    assert!(h.x.contains(p) && h.y.contains(p), "{arcs:?}: {p} not in {h:?}");
                }
            }
            // The smallest arc holding them: the complement of the largest
            // gap between them (brute force over the arcs' starts).
            let smallest = arcs
                .iter()
                .map(|a| {
                    let st = a.start as u32;
                    let end = arcs.iter().map(|b| (b.start as u32 + CIRCLE - st) % CIRCLE + b.last as u32).max().unwrap();
                    end.min(CIRCLE - 1) + 1
                })
                .min()
                .unwrap();
            assert!(h.x.len() >= smallest as u64);
            if smallest < CIRCLE / 2 {
                assert_eq!(h.x.len(), smallest as u64, "{arcs:?}: {h:?} is not the smallest ({smallest})");
            }
            // Merged with arcs inside it, it stays itself (unless it holds
            // both cuts: then it may become the circle).
            let mut again = HullAcc::default();
            again.add_hull(&h);
            for a in &arcs {
                a.segs(|g| again.add_seg(0, g));
                a.segs(|g| again.add_seg(1, g));
            }
            let h2 = again.hull().unwrap();
            if !(h.x.contains(cut::<Fixed16>(0) as u32) && h.x.contains(cut::<Fixed16>(1) as u32)) {
                assert_eq!(h2, h, "{arcs:?}");
            } else {
                assert!(h2 == h || h2.x.is_full());
            }
        }
        let mut b = Vec::new();
        Hull::<Fixed16>::point(3, 70000 % CIRCLE).to_bytes(&mut b);
        assert_eq!((b.len(), Hull::<Fixed16>::from_bytes(&b)), (Hull::<Fixed16>::BYTES, Hull::point(3, 70000 % CIRCLE)));
    }
}
