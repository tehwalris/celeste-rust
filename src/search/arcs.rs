//! Sets of sub-pixel remainders as ARCS (plans: the rotation graph,
//! 2026-10-04). A frame moves a remainder by `rem := (rem + v) mod 1` into
//! [-1/2, 1/2) - a ROTATION of the circle - and which side of the wrap the
//! rotated value lands on decides the pixel step. So a set of remainders
//! that a set of histories can hold is a union of arcs, a frame along one
//! edge maps it by "intersect with the edge's guard arc, then rotate" (or,
//! on a collision, to a point), and that map distributes over unions:
//! pushing sets through a graph tests every path at once.
//!
//! Coordinates: a remainder `r` (raw 16.16, in [-32768, 32767]) is the
//! point `r + 32768` of `0..CIRCLE`. A set is sorted, disjoint, non-adjacent
//! half-open segments `[lo, hi)` of `0..CIRCLE` (an arc across the wrap is
//! two segments). Two axes: unions of rectangles (`Rects`), each a pair of
//! segments.

/// Points on the circle: 1 px in raw 16.16 units.
pub const CIRCLE: u32 = 1 << 16;

/// A remainder (raw, [-32768, 32767]) as a point of the circle.
pub fn point(rem: i32) -> u32 {
    debug_assert!((-32768..32768).contains(&rem), "a remainder out of range: {rem}");
    (rem + 32768) as u32
}

/// A half-open segment `[lo, hi)` of `0..CIRCLE`, `lo < hi`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Seg {
    pub lo: u32,
    pub hi: u32,
}

/// A union of segments: sorted, disjoint, not touching.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Set(pub Vec<Seg>);

impl Set {
    pub fn empty() -> Self {
        Set(Vec::new())
    }
    pub fn full() -> Self {
        Set(vec![Seg { lo: 0, hi: CIRCLE }])
    }
    pub fn point(p: u32) -> Self {
        Set(vec![Seg { lo: p, hi: p + 1 }])
    }
    pub fn seg(lo: u32, hi: u32) -> Self {
        if lo >= hi {
            Set::empty()
        } else {
            Set(vec![Seg { lo, hi }])
        }
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn contains(&self, p: u32) -> bool {
        self.0.iter().any(|s| s.lo <= p && p < s.hi)
    }
    /// Normalize: sort, merge overlapping and touching segments.
    fn norm(mut v: Vec<Seg>) -> Set {
        v.retain(|s| s.lo < s.hi);
        v.sort();
        let mut out: Vec<Seg> = Vec::with_capacity(v.len());
        for s in v {
            match out.last_mut() {
                Some(l) if s.lo <= l.hi => l.hi = l.hi.max(s.hi),
                _ => out.push(s),
            }
        }
        Set(out)
    }
    pub fn union(&self, o: &Set) -> Set {
        Set::norm(self.0.iter().chain(&o.0).copied().collect())
    }
    pub fn intersect(&self, o: &Set) -> Set {
        let mut out = Vec::new();
        for a in &self.0 {
            for b in &o.0 {
                let (lo, hi) = (a.lo.max(b.lo), a.hi.min(b.hi));
                if lo < hi {
                    out.push(Seg { lo, hi });
                }
            }
        }
        Set::norm(out)
    }
    /// The set rotated by `v` raw units (`p -> (p + v) mod CIRCLE`).
    pub fn rotate(&self, v: i32) -> Set {
        let d = v.rem_euclid(CIRCLE as i32) as u32;
        let mut out = Vec::new();
        for s in &self.0 {
            let (lo, hi) = (s.lo + d, s.hi + d);
            if hi <= CIRCLE {
                out.push(Seg { lo, hi });
            } else if lo >= CIRCLE {
                out.push(Seg { lo: lo - CIRCLE, hi: hi - CIRCLE });
            } else {
                out.push(Seg { lo, hi: CIRCLE });
                out.push(Seg { lo: 0, hi: hi - CIRCLE });
            }
        }
        Set::norm(out)
    }
    /// Arcs: segments, the two touching the wrap counted as one.
    pub fn arcs(&self) -> usize {
        let n = self.0.len();
        if n >= 2 && self.0[0].lo == 0 && self.0[n - 1].hi == CIRCLE {
            n - 1
        } else {
            n
        }
    }
}

/// What a frame does to one axis's remainder along an edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Action {
    /// `rem := rem + v (mod 1)`, raw `v`.
    Rotate(i32),
    /// `rem := c` (a collision resets it): the point `c` of the circle.
    Const(u32),
}

impl Action {
    /// The image of `s` (already inside the edge's guard).
    pub fn image(self, s: &Set) -> Set {
        match self {
            Action::Rotate(v) => s.rotate(v),
            Action::Const(c) => {
                if s.is_empty() {
                    Set::empty()
                } else {
                    Set::point(c)
                }
            }
        }
    }
    /// The preimage of `s`: every point the action takes into `s`.
    pub fn preimage(self, s: &Set) -> Set {
        match self {
            Action::Rotate(v) => s.rotate(-v),
            Action::Const(c) => {
                if s.contains(c) {
                    Set::full()
                } else {
                    Set::empty()
                }
            }
        }
    }
}

/// The two pieces of the circle a speed `v` (raw) cuts it into: the points
/// whose `rem + v + 1/2` stays below the next integer, and the rest (one
/// piece when `v` is a whole number of pixels).
pub fn pieces(v: i32) -> Vec<Set> {
    // u = p - 32768 + v + 32768 = p + v; the amount steps where p + v is a
    // multiple of CIRCLE, at p = (-v) mod CIRCLE.
    let cut = (-v).rem_euclid(CIRCLE as i32) as u32;
    if cut == 0 {
        vec![Set::full()]
    } else {
        vec![Set::seg(0, cut), Set::seg(cut, CIRCLE)]
    }
}

/// A rectangle of the torus: an x set times a y set.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Rect {
    pub x: Set,
    pub y: Set,
}

/// A union of rectangles (two axes; exact, not a product).
#[derive(Clone, Debug, Default)]
pub struct Rects(pub Vec<Rect>);

impl Rects {
    pub fn empty() -> Self {
        Rects(Vec::new())
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn contains(&self, x: u32, y: u32) -> bool {
        self.0.iter().any(|r| r.x.contains(x) && r.y.contains(y))
    }
    /// Add a rectangle, merging it into one with the same x (or y) set.
    pub fn add(&mut self, r: Rect) {
        if r.x.is_empty() || r.y.is_empty() {
            return;
        }
        for q in self.0.iter_mut() {
            if q.x == r.x {
                q.y = q.y.union(&r.y);
                return;
            }
            if q.y == r.y {
                q.x = q.x.union(&r.x);
                return;
            }
        }
        self.0.push(r);
    }
    pub fn extend(&mut self, o: Rects) {
        for r in o.0 {
            self.add(r);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_rotation_wraps_and_an_intersection_cuts() {
        let s = Set::seg(60000, 65000);
        let r = s.rotate(1000);
        assert_eq!(r, Set(vec![Seg { lo: 0, hi: 464 }, Seg { lo: 61000, hi: 65536 }]));
        assert_eq!(r.arcs(), 1, "one arc across the wrap");
        assert_eq!(r.rotate(-1000), s);
        assert_eq!(r.intersect(&Set::seg(0, 100)), Set::seg(0, 100));
    }

    #[test]
    fn a_speed_cuts_the_circle_where_the_amount_steps() {
        // v = 0.6 px: rem + 0.6 + 1/2 reaches 1 at rem = -0.1.
        let v = 39322;
        let p = pieces(v);
        assert_eq!(p.len(), 2);
        let cut = p[1].0[0].lo as i32 - 32768;
        let amount = |rem: i32| (rem + v + 32768).div_euclid(65536);
        assert_eq!(amount(cut - 1), 0);
        assert_eq!(amount(cut), 1);
        assert_eq!(pieces(65536).len(), 1, "a whole pixel cuts nothing");
    }

    #[test]
    fn a_collision_maps_everything_to_a_point_and_back() {
        let a = Action::Const(point(0));
        assert_eq!(a.image(&Set::seg(5, 9)), Set::point(32768));
        assert_eq!(a.preimage(&Set::point(32768)), Set::full());
        assert!(a.preimage(&Set::seg(0, 10)).is_empty());
    }
}
