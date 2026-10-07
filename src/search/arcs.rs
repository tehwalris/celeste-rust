//! Sets of sub-pixel remainders as ARCS (the rotation graph). A frame moves
//! a remainder by `rem := (rem + v) mod 1` into [-1/2, 1/2), a ROTATION of
//! the circle, and the side of the wrap it lands on decides the pixel step.
//! Along an edge a set maps by "intersect with the guard arc, then rotate"
//! (or, on a collision, to a point), which distributes over unions: pushing
//! sets through a graph tests every path at once.
//!
//! Coordinates: a remainder `r` (raw 16.16, in [-32768, 32767]) is the
//! point `r + 32768` of `0..CIRCLE`. A set is sorted, disjoint, non-adjacent
//! half-open segments (an arc across the wrap is two). Two axes: `Rects`
//! (unions of rectangles) and the canonical `Region`.

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

/// One axis of an edge: the remainders that take it (one piece of the
/// circle, never wrapping) and what it does to them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Transfer {
    pub guard: Seg,
    pub action: Action,
}

impl Transfer {
    pub fn guard_set(&self) -> Set {
        Set::seg(self.guard.lo, self.guard.hi)
    }
    pub fn takes(&self, p: u32) -> bool {
        self.guard.lo <= p && p < self.guard.hi
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
}

/// One axis-aligned piece of the torus: `[y.lo, y.hi) x [x.lo, x.hi)`, no
/// wrap on either axis (a wrapping arc is two pieces).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Piece {
    pub y: Seg,
    pub x: Seg,
}

/// A slab of a `Region`: `[lo, hi)` of y, its x set `xs[start..end]` (the
/// start is the previous slab's end).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Slab {
    lo: u32,
    hi: u32,
    end: u32,
}

/// A set of the torus in CANONICAL form: sorted disjoint y slabs, each with
/// a non-empty x set, touching slabs never with the same x set. Equal sets
/// are equal values; a union of many contributions is one sweep.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Region {
    slabs: Vec<Slab>,
    xs: Vec<Seg>,
}

/// `s` rotated by `d` (already in `0..CIRCLE`): one or two segments.
fn rot_seg(s: Seg, d: u32, mut f: impl FnMut(Seg)) {
    let (lo, hi) = (s.lo + d, s.hi + d);
    if hi <= CIRCLE {
        f(Seg { lo, hi });
    } else if lo >= CIRCLE {
        f(Seg { lo: lo - CIRCLE, hi: hi - CIRCLE });
    } else {
        f(Seg { lo, hi: CIRCLE });
        f(Seg { lo: 0, hi: hi - CIRCLE });
    }
}

fn clip(s: Seg, g: Seg) -> Option<Seg> {
    let (lo, hi) = (s.lo.max(g.lo), s.hi.min(g.hi));
    (lo < hi).then_some(Seg { lo, hi })
}

impl Region {
    pub fn full() -> Self {
        Region { slabs: vec![Slab { lo: 0, hi: CIRCLE, end: 1 }], xs: vec![Seg { lo: 0, hi: CIRCLE }] }
    }
    pub fn is_empty(&self) -> bool {
        self.slabs.is_empty()
    }
    /// Slabs (a y segment and its x set), in y order.
    pub fn slabs(&self) -> impl Iterator<Item = (Seg, &[Seg])> + '_ {
        let mut start = 0;
        self.slabs.iter().map(move |s| {
            let xs = &self.xs[start as usize..s.end as usize];
            start = s.end;
            (Seg { lo: s.lo, hi: s.hi }, xs)
        })
    }
    pub fn n_segs(&self) -> usize {
        self.xs.len()
    }
    fn slab_at(&self, y: u32) -> Option<&[Seg]> {
        let i = self.slabs.partition_point(|s| s.hi <= y);
        let s = self.slabs.get(i).filter(|s| s.lo <= y)?;
        let start = if i == 0 { 0 } else { self.slabs[i - 1].end };
        Some(&self.xs[start as usize..s.end as usize])
    }
    pub fn contains(&self, x: u32, y: u32) -> bool {
        self.slab_at(y).is_some_and(|xs| {
            let i = xs.partition_point(|s| s.hi <= x);
            xs.get(i).is_some_and(|s| s.lo <= x)
        })
    }
    /// The union of `pieces`, canonical (one sweep over y). `pieces` is
    /// left in an unspecified order.
    pub fn from_pieces(pieces: &mut [Piece], scratch: &mut Vec<Seg>) -> Region {
        let mut out = Region::default();
        if pieces.is_empty() {
            return out;
        }
        pieces.sort_unstable_by_key(|p| p.y.lo);
        let mut bounds: Vec<u32> = pieces.iter().flat_map(|p| [p.y.lo, p.y.hi]).collect();
        bounds.sort_unstable();
        bounds.dedup();
        let mut active: Vec<Piece> = Vec::new();
        let mut next = 0;
        for w in bounds.windows(2) {
            let (lo, hi) = (w[0], w[1]);
            active.retain(|p| p.y.hi > lo);
            while next < pieces.len() && pieces[next].y.lo == lo {
                active.push(pieces[next]);
                next += 1;
            }
            if active.is_empty() {
                continue;
            }
            scratch.clear();
            scratch.extend(active.iter().map(|p| p.x));
            scratch.sort_unstable();
            let mut merged: Vec<Seg> = Vec::with_capacity(scratch.len());
            for &s in scratch.iter() {
                match merged.last_mut() {
                    Some(l) if s.lo <= l.hi => l.hi = l.hi.max(s.hi),
                    _ => merged.push(s),
                }
            }
            let start = out.slabs.last().map_or(0, |s| s.end) as usize;
            let prev_start = if out.slabs.len() >= 2 { out.slabs[out.slabs.len() - 2].end as usize } else { 0 };
            if let Some(last) = out.slabs.last_mut() {
                if last.hi == lo && out.xs[prev_start..start] == merged[..] {
                    last.hi = hi;
                    continue;
                }
            }
            out.xs.extend_from_slice(&merged);
            out.slabs.push(Slab { lo, hi, end: out.xs.len() as u32 });
        }
        out
    }
    /// The preimage of `self` under one edge, inside the edge's guards, as
    /// pieces appended to `out`: the remainders the edge takes into `self`.
    pub fn pull(&self, x: Transfer, y: Transfer, out: &mut Vec<Piece>) {
        let neg = |v: i32| (-(v as i64)).rem_euclid(CIRCLE as i64) as u32;
        let emit_x = |ys: Seg, xs: &[Seg], out: &mut Vec<Piece>| match x.action {
            Action::Rotate(v) => {
                let d = neg(v);
                for &s in xs {
                    rot_seg(s, d, |r| {
                        if let Some(c) = clip(r, x.guard) {
                            out.push(Piece { y: ys, x: c });
                        }
                    });
                }
            }
            Action::Const(c) => {
                let i = xs.partition_point(|s| s.hi <= c);
                if xs.get(i).is_some_and(|s| s.lo <= c) {
                    out.push(Piece { y: ys, x: x.guard });
                }
            }
        };
        match y.action {
            Action::Rotate(v) => {
                let d = neg(v);
                for (ys, xs) in self.slabs() {
                    rot_seg(ys, d, |r| {
                        if let Some(c) = clip(r, y.guard) {
                            emit_x(c, xs, out);
                        }
                    });
                }
            }
            Action::Const(c) => {
                if let Some(xs) = self.slab_at(c) {
                    emit_x(y.guard, xs, out);
                }
            }
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

    /// A tiny deterministic generator for the property tests.
    fn lcg(s: &mut u64) -> u32 {
        *s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (*s >> 33) as u32
    }

    /// The canonical sweep against the plain union of pieces, and `pull`
    /// against the point-wise preimage, probed at every boundary.
    #[test]
    fn a_region_is_the_union_and_pull_is_the_preimage() {
        let mut s = 7u64;
        let seg = |s: &mut u64| {
            let a = lcg(s) % CIRCLE;
            let b = lcg(s) % CIRCLE;
            let (lo, hi) = (a.min(b), a.max(b) + 1);
            Seg { lo, hi: hi.min(CIRCLE) }
        };
        for _ in 0..200 {
            let n = 1 + lcg(&mut s) % 12;
            let mut ps: Vec<Piece> = (0..n).map(|_| Piece { y: seg(&mut s), x: seg(&mut s) }).collect();
            let orig = ps.clone();
            let r = Region::from_pieces(&mut ps, &mut Vec::new());
            let mut probes: Vec<u32> = orig.iter().flat_map(|p| [p.x.lo, p.x.hi, p.y.lo, p.y.hi]).flat_map(|v| [v.saturating_sub(1), v, (v + 1).min(CIRCLE - 1)]).filter(|&v| v < CIRCLE).collect();
            probes.sort_unstable();
            probes.dedup();
            let inside = |x: u32, y: u32| orig.iter().any(|p| p.x.lo <= x && x < p.x.hi && p.y.lo <= y && y < p.y.hi);
            for &x in &probes {
                for &y in &probes {
                    assert_eq!(r.contains(x, y), inside(x, y), "({x}, {y})");
                }
            }
            // Canonical: the same set from the pieces in another order.
            let mut rev = orig.clone();
            rev.reverse();
            assert_eq!(Region::from_pieces(&mut rev, &mut Vec::new()), r);
            // pull: a point p is in the result iff the guards take it and its image is in r.
            let act = |s: &mut u64| if lcg(s) % 4 == 0 { Action::Const(lcg(s) % CIRCLE) } else { Action::Rotate((lcg(s) % CIRCLE) as i32 - 32768) };
            let (tx, ty) = (Transfer { guard: seg(&mut s), action: act(&mut s) }, Transfer { guard: seg(&mut s), action: act(&mut s) });
            let mut out = Vec::new();
            r.pull(tx, ty, &mut out);
            let pr = Region::from_pieces(&mut out, &mut Vec::new());
            let ap = |a: Action, p: u32| match a {
                Action::Rotate(v) => (p as i64 + v as i64).rem_euclid(CIRCLE as i64) as u32,
                Action::Const(c) => c,
            };
            let mut pp: Vec<u32> = probes.iter().flat_map(|&v| {
                let back = |a: Action| match a {
                    Action::Rotate(d) => (v as i64 - d as i64).rem_euclid(CIRCLE as i64) as u32,
                    Action::Const(_) => v,
                };
                [v, back(tx.action), back(ty.action)]
            }).chain([tx.guard.lo, tx.guard.hi - 1, ty.guard.lo, ty.guard.hi - 1]).collect();
            pp.sort_unstable();
            pp.dedup();
            for &x in &pp {
                for &y in &pp {
                    let want = tx.takes(x) && ty.takes(y) && r.contains(ap(tx.action, x), ap(ty.action, y));
                    assert_eq!(pr.contains(x, y), want, "pull at ({x}, {y})");
                }
            }
        }
    }

    #[test]
    fn a_collision_maps_everything_to_a_point_and_back() {
        let a = Action::Const(point(0));
        assert_eq!(a.image(&Set::seg(5, 9)), Set::point(32768));
        assert_eq!(a.preimage(&Set::point(32768)), Set::full());
        assert!(a.preimage(&Set::seg(0, 10)).is_empty());
    }
}
