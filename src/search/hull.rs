//! A STATE'S REMAINDER HULL (the hull forward, plans/architecture.md "The
//! forward frame"): the sub-pixel remainders a state has been reached
//! with so far, over-approximated by ONE ARC of the circle per axis (their
//! box). The forward keeps a hull per state and pushes it along the
//! recorded edges through their transfers (`push`: per axis the arc's
//! points inside the guard, moved by the action); a target's hull grows to
//! cover what it is pushed (`HullAcc`). Sound: every remainder a concrete
//! path reaches a state with is in its hull (the push of a superset is a
//! superset of the push, and the hull of a union a superset of it).
//!
//! Coordinates are `arcs`' (a remainder `r` is the point `r + 32768` of
//! `0..CIRCLE`). An arc may wrap past the end of the circle.
//!
//! THE MERGE IS ORDER-FREE: the pushes into one target within a frame
//! come from region jobs that run in any order, and "the smallest arc
//! holding two arcs" is not associative on a circle. So a target
//! accumulates per axis two CHARTS - the circle cut at 0 and at 1/2, each
//! a linear interval `[lo, hi]` of its own coordinates - merged by min and
//! max (commutative and associative); the arc is then the shorter chart's
//! (`HullAcc::hull`). That is the smallest arc holding the union whenever
//! the union fits in half the circle (its largest gap is then longer than
//! half, so it holds a cut), and a superset of it otherwise.

use super::arcs::{Action, Piece, Region, Seg, Transfer, CIRCLE};
use super::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;

/// An ARC of the circle: `last + 1` points from `start`, wrapping past the
/// end (`last + 1` is at most `CIRCLE`; the whole circle starts at 0).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[repr(C)]
pub struct Arc {
    pub start: u16,
    pub last: u16,
}

impl Arc {
    /// The point `p` of the circle.
    pub fn point(p: u32) -> Arc {
        debug_assert!(p < CIRCLE);
        Arc { start: p as u16, last: 0 }
    }

    /// The whole circle.
    pub fn full() -> Arc {
        Arc { start: 0, last: (CIRCLE - 1) as u16 }
    }

    /// Its points.
    #[inline]
    pub fn len(self) -> u32 {
        self.last as u32 + 1
    }

    #[inline]
    pub fn is_full(self) -> bool {
        self.len() == CIRCLE
    }

    #[inline]
    pub fn contains(self, p: u32) -> bool {
        (p + CIRCLE - self.start as u32) % CIRCLE <= self.last as u32
    }

    /// Does it hold every point of segment `s`?
    pub fn contains_seg(self, s: Seg) -> bool {
        let at = |p: u32| (p + CIRCLE - self.start as u32) % CIRCLE;
        let (a, b) = (at(s.lo), at(s.hi - 1));
        a <= b && b <= self.last as u32 && b - a == s.hi - 1 - s.lo
    }

    /// The arc as segments of `0..CIRCLE` (one, or two across the end).
    #[inline]
    pub fn segs(self, mut f: impl FnMut(Seg)) {
        let (a, n) = (self.start as u32, self.len());
        if a + n <= CIRCLE {
            f(Seg { lo: a, hi: a + n });
        } else {
            f(Seg { lo: a, hi: CIRCLE });
            f(Seg { lo: 0, hi: a + n - CIRCLE });
        }
    }
}

/// A state's HULL: one arc per axis, the box of the remainders it is
/// reached with. Never empty.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[repr(C)]
pub struct Hull {
    pub x: Arc,
    pub y: Arc,
}

/// Bytes a hull takes on disk (`to_bytes`).
pub const HULL_BYTES: usize = 8;

impl Hull {
    /// The point `(x, y)` (a start's exact remainder).
    pub fn point(x: u32, y: u32) -> Hull {
        Hull { x: Arc::point(x), y: Arc::point(y) }
    }

    pub fn full() -> Hull {
        Hull { x: Arc::full(), y: Arc::full() }
    }

    #[inline]
    pub fn contains(&self, x: u32, y: u32) -> bool {
        self.x.contains(x) && self.y.contains(y)
    }

    /// Its points (per axis lengths multiplied).
    pub fn points(&self) -> u64 {
        self.x.len() as u64 * self.y.len() as u64
    }

    /// The hull as a set of the torus (`arcs::Region`).
    pub fn to_region(&self) -> Region {
        let mut pieces = Vec::new();
        self.y.segs(|y| self.x.segs(|x| pieces.push(Piece { y, x })));
        Region::from_pieces(&mut pieces, &mut Vec::new())
    }

    /// THE PUSH along an edge (`tx`, `ty` its transfers): per axis the
    /// arc's points inside the guard, moved by the action, added to `acc`
    /// - nothing where either axis's guard misses the arc (a box's image is
    /// empty with either side). Returns whether it pushed.
    #[inline]
    pub fn push(&self, tx: Transfer, ty: Transfer, acc: &mut HullAcc) -> bool {
        let mut xs = [Seg { lo: 0, hi: 0 }; 4];
        let nx = image(self.x, tx, &mut xs);
        if nx == 0 {
            return false;
        }
        let mut ys = [Seg { lo: 0, hi: 0 }; 4];
        let ny = image(self.y, ty, &mut ys);
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

    /// THE PUSH of a GROWN hull (`self`; `was`, the hull it last pushed,
    /// inside it): `push`, but only where the edge's image grows past
    /// `was`'s - every image of `was` was pushed then (an image dropped
    /// then, by level -1 or the ladder's filter, is dropped at every later
    /// frame too). The image grows when the box was empty and is not, or
    /// when an axis it rotates meets more of the guard than before (a
    /// constant's image is its point either way). Returns whether it pushed.
    #[inline]
    pub fn push_grown(&self, was: &Hull, tx: Transfer, ty: Transfer, acc: &mut HullAcc) -> bool {
        let (nx, ny) = (meet(self.x, tx.guard), meet(self.y, ty.guard));
        if nx == 0 || ny == 0 {
            return false;
        }
        let (ox, oy) = (meet(was.x, tx.guard), meet(was.y, ty.guard));
        let grows = |t: Transfer, n: u32, o: u32| matches!(t.action, Action::Rotate(_)) && n != o;
        if ox != 0 && oy != 0 && !grows(tx, nx, ox) && !grows(ty, ny, oy) {
            return false;
        }
        self.push(tx, ty, acc)
    }

    /// `x.start, x.last, y.start, y.last` in one word, low first
    /// (`to_bytes` little-endian).
    #[inline]
    pub fn word(&self) -> u64 {
        self.x.start as u64 | (self.x.last as u64) << 16 | (self.y.start as u64) << 32 | (self.y.last as u64) << 48
    }

    /// Little-endian `x.start, x.last, y.start, y.last`.
    pub fn to_bytes(&self) -> [u8; HULL_BYTES] {
        let mut b = [0u8; HULL_BYTES];
        for (i, v) in [self.x.start, self.x.last, self.y.start, self.y.last].into_iter().enumerate() {
            b[2 * i..2 * i + 2].copy_from_slice(&v.to_le_bytes());
        }
        b
    }

    pub fn from_bytes(b: &[u8]) -> Hull {
        let v = |i: usize| u16::from_le_bytes([b[2 * i], b[2 * i + 1]]);
        Hull { x: Arc { start: v(0), last: v(1) }, y: Arc { start: v(2), last: v(3) } }
    }
}

/// The points of arc `a` inside segment `g`.
#[inline]
fn meet(a: Arc, g: Seg) -> u32 {
    let mut n = 0;
    a.segs(|s| {
        if let Some(c) = clip(s, g) {
            n += c.hi - c.lo;
        }
    });
    n
}

/// `s` clipped to `g`.
#[inline]
fn clip(s: Seg, g: Seg) -> Option<Seg> {
    let (lo, hi) = (s.lo.max(g.lo), s.hi.min(g.hi));
    (lo < hi).then_some(Seg { lo, hi })
}

/// The image of arc `a` under one axis's transfer `t`: its points inside
/// the guard, moved by the action, as segments into `out`; their count
/// (0: the guard misses the arc).
#[inline]
fn image(a: Arc, t: Transfer, out: &mut [Seg; 4]) -> usize {
    let mut n = 0usize;
    let mut hit = false;
    a.segs(|s| {
        if let Some(c) = clip(s, t.guard) {
            hit = true;
            if let Action::Rotate(v) = t.action {
                let d = v.rem_euclid(CIRCLE as i32) as u32;
                let (lo, hi) = (c.lo + d, c.hi + d);
                if hi <= CIRCLE {
                    out[n] = Seg { lo, hi };
                    n += 1;
                } else if lo >= CIRCLE {
                    out[n] = Seg { lo: lo - CIRCLE, hi: hi - CIRCLE };
                    n += 1;
                } else {
                    out[n] = Seg { lo, hi: CIRCLE };
                    out[n + 1] = Seg { lo: 0, hi: hi - CIRCLE };
                    n += 2;
                }
            }
        }
    });
    match t.action {
        Action::Const(p) if hit => {
            out[0] = Seg { lo: p, hi: p + 1 };
            1
        }
        Action::Const(_) => 0,
        Action::Rotate(_) => n,
    }
}

/// A small direct-mapped cache of pushes, `(hull, the hull it last pushed,
/// transfer) -> the push` (`None`: nothing): a source's cells and a unit's
/// sources share hulls (a full one most of all) and transfers, and a push
/// depends on nothing else. The transfer is the caller's id; start a new
/// `epoch` when the ids change meaning (an entry of an older one never
/// matches).
pub struct PushCache<N: Rem = P8> {
    slots: Vec<(N::Hull, N::Hull, u32, u32, Option<N::Acc>)>,
    epoch: u32,
}

/// Entries of a `PushCache` (a power of two).
const PUSH_CACHE: usize = 1 << 11;

impl<N: Rem> Default for PushCache<N> {
    fn default() -> Self {
        PushCache { slots: vec![(N::Hull::default(), N::Hull::default(), u32::MAX, u32::MAX, None); PUSH_CACHE], epoch: 0 }
    }
}

impl<N: Rem> PushCache<N> {
    /// The transfer ids change meaning from here.
    pub fn next_epoch(&mut self) {
        self.epoch = self.epoch.wrapping_add(1);
        if self.epoch == u32::MAX {
            self.slots.fill((N::Hull::default(), N::Hull::default(), u32::MAX, u32::MAX, None));
            self.epoch = 0;
        }
    }

    /// The push of `h` (`was`: its last pushed hull; itself for a hull
    /// pushed the first time) along transfer `x`, `f()` on a miss.
    #[inline]
    pub fn get(&mut self, h: &N::Hull, was: &N::Hull, x: u32, f: impl FnOnce() -> Option<N::Acc>) -> Option<N::Acc> {
        let (a, b) = (N::hull_hash(h), N::hull_hash(was));
        let k = ((a ^ b.rotate_left(29) ^ (x as u64) << 45).wrapping_mul(0x9e37_79b9_7f4a_7c15) >> (64 - PUSH_CACHE.trailing_zeros())) as usize;
        let e = &mut self.slots[k];
        if (e.0, e.1, e.2, e.3) == (*h, *was, x, self.epoch) {
            return e.4;
        }
        let r = f();
        *e = (*h, *was, x, self.epoch, r);
        r
    }
}

/// TESTS ONLY: every hull the forward accumulates (and the start's) WIDENED
/// to the whole circle - the forward before hulls, every state's remainder
/// widened, which the hull forward must stay inside (`frame::tests`).
/// Process-global: nextest runs each test in its own process.
#[cfg(test)]
pub static WIDEN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// The charts' cut points: chart `c`'s coordinate of point `p` is
/// `(p - CUT[c]) mod CIRCLE`.
const CUT: [u32; 2] = [0, CIRCLE / 2];

/// A HULL BEING ACCUMULATED (module doc): per axis, per chart, the linear
/// hull `[lo, hi]` in the chart's coordinates of every point added (`lo >
/// hi`: none) - `lo` and `hi` by `axis * 2 + chart`. Merging is min and
/// max: the result does not depend on the order things are added in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct HullAcc {
    lo: [u16; 4],
    hi: [u16; 4],
}

impl Default for HullAcc {
    fn default() -> Self {
        HullAcc::EMPTY
    }
}

impl HullAcc {
    pub const EMPTY: HullAcc = HullAcc { lo: [u16::MAX; 4], hi: [0; 4] };

    /// Nothing added (on either axis: a box with an empty side is empty)?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.lo[0] > self.hi[0] || self.lo[2] > self.hi[2]
    }

    /// Add segment `s` of axis `axis` (0: x, 1: y).
    #[inline]
    pub fn add_seg(&mut self, axis: usize, s: Seg) {
        debug_assert!(s.lo < s.hi && s.hi <= CIRCLE);
        for (c, cut) in CUT.into_iter().enumerate() {
            let a = (s.lo + CIRCLE - cut) % CIRCLE;
            let b = (s.hi - 1 + CIRCLE - cut) % CIRCLE;
            let (lo, hi) = if a <= b { (a, b) } else { (0, CIRCLE - 1) };
            let i = axis * 2 + c;
            self.lo[i] = self.lo[i].min(lo as u16);
            self.hi[i] = self.hi[i].max(hi as u16);
        }
    }

    /// Add every point of `h`.
    #[inline]
    pub fn add_hull(&mut self, h: &Hull) {
        h.x.segs(|s| self.add_seg(0, s));
        h.y.segs(|s| self.add_seg(1, s));
    }

    /// Add everything `o` holds.
    #[inline]
    pub fn merge(&mut self, o: &HullAcc) {
        for i in 0..4 {
            self.lo[i] = self.lo[i].min(o.lo[i]);
            self.hi[i] = self.hi[i].max(o.hi[i]);
        }
    }

    /// The hull: per axis the shorter chart's arc (chart 0 on a tie);
    /// `None` when nothing was added.
    pub fn hull(&self) -> Option<Hull> {
        if self.is_empty() {
            return None;
        }
        #[cfg(test)]
        if WIDEN.load(std::sync::atomic::Ordering::Relaxed) {
            return Some(Hull::full());
        }
        let axis = |a: usize| {
            let mut best: Option<Arc> = None;
            for c in 0..2 {
                let (lo, hi) = (self.lo[a * 2 + c], self.hi[a * 2 + c]);
                let len = hi as u32 - lo as u32 + 1;
                if best.is_none_or(|b| len < b.len()) {
                    best = Some(if len == CIRCLE { Arc::full() } else { Arc { start: ((lo as u32 + CUT[c]) % CIRCLE) as u16, last: (len - 1) as u16 } });
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

    fn lcg(s: &mut u64) -> u32 {
        *s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (*s >> 33) as u32
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
        let action = if lcg(s) % 4 == 0 { Action::Const(lcg(s) % CIRCLE) } else { Action::Rotate((lcg(s) % CIRCLE) as i32 - 32768) };
        Transfer { guard, action }
    }

    /// Points to probe a set at: its arcs' ends and neighbours, and some
    /// random ones.
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
            let mut acc = HullAcc::EMPTY;
            let pushed = h.push(tx, ty, &mut acc);
            let mut img = Vec::new();
            h.to_region().push(tx, ty, &mut img);
            let img = Region::from_pieces(&mut img, &mut Vec::new());
            assert_eq!(pushed, !img.is_empty(), "{h:?} {tx:?} {ty:?}");
            assert_eq!(acc.is_empty(), !pushed);
            let Some(got) = acc.hull() else { continue };
            // Every point of the image is in the hull: probed at the image's
            // slab and segment ends.
            for (ys, xs) in img.slabs() {
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
            let mut grow = HullAcc::EMPTY;
            grow.add_hull(&was);
            grow.add_hull(&Hull { x: arc(&mut s), y: arc(&mut s) });
            let h = grow.hull().unwrap();
            let (tx, ty) = (transfer(&mut s), transfer(&mut s));
            let (mut a, mut b, mut old) = (HullAcc::EMPTY, HullAcc::EMPTY, HullAcc::EMPTY);
            let full = h.push(tx, ty, &mut a);
            let grown = h.push_grown(&was, tx, ty, &mut b);
            if grown {
                assert_eq!(a, b, "a grown push is the whole push");
            } else if full {
                // Skipped: the old push's image already holds the new one.
                assert!(was.push(tx, ty, &mut old), "skipped a push whose old image was empty");
                let (mut img_new, mut img_old) = (Vec::new(), Vec::new());
                h.to_region().push(tx, ty, &mut img_new);
                was.to_region().push(tx, ty, &mut img_old);
                assert_eq!(Region::from_pieces(&mut img_new, &mut Vec::new()), Region::from_pieces(&mut img_old, &mut Vec::new()), "{was:?} -> {h:?} {tx:?} {ty:?}: skipped a push whose image grew");
            }
        }
        // Order: many pushes into one accumulator, forwards and backwards.
        for _ in 0..300 {
            let pushes: Vec<(Hull, Transfer, Transfer)> = (0..1 + lcg(&mut s) % 9).map(|_| (Hull { x: arc(&mut s), y: arc(&mut s) }, transfer(&mut s), transfer(&mut s))).collect();
            let (mut a, mut b) = (HullAcc::EMPTY, HullAcc::EMPTY);
            for p in &pushes {
                p.0.push(p.1, p.2, &mut a);
            }
            for p in pushes.iter().rev() {
                let mut one = HullAcc::EMPTY;
                p.0.push(p.1, p.2, &mut one);
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
            let mut acc = HullAcc::EMPTY;
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
            assert!(h.x.len() >= smallest);
            if smallest < CIRCLE / 2 {
                assert_eq!(h.x.len(), smallest, "{arcs:?}: {h:?} is not the smallest ({smallest})");
            }
            // Merged with arcs inside it, it stays itself (unless it holds
            // both cuts: then it may become the circle).
            let mut again = HullAcc::EMPTY;
            again.add_hull(&h);
            for a in &arcs {
                a.segs(|g| again.add_seg(0, g));
                a.segs(|g| again.add_seg(1, g));
            }
            let h2 = again.hull().unwrap();
            if !(h.x.contains(CUT[0]) && h.x.contains(CUT[1])) {
                assert_eq!(h2, h, "{arcs:?}");
            } else {
                assert!(h2 == h || h2.x.is_full());
            }
        }
        assert_eq!(Hull::from_bytes(&Hull::point(3, 70000 % CIRCLE).to_bytes()), Hull::point(3, 70000 % CIRCLE));
    }
}
