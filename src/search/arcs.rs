//! Sets of sub-pixel remainders as ARCS (plans/architecture.md "Arcs: the
//! rotation graph"): along an edge a set maps by "intersect with the guard,
//! then act", which distributes over unions. A remainder `r` (raw 16.16) is
//! the point `r + 32768` of `0..CIRCLE`; a set is sorted, disjoint,
//! non-adjacent half-open segments (an arc across the wrap is two). The
//! two-axis `Region` is generic over the `Circle` (16.16's `Fixed16`, the
//! doubles' `rem_f64::Doubles`); the pull is exact on both.

/// Points on the circle: 1 px in raw 16.16 units.
pub const CIRCLE: u32 = 1 << 16;

/// The most x segments a `Region` holds (its slabs' 16-bit end index).
pub const MAX_SEGS: usize = (1 << 16) - 1;

/// A remainder (raw, [-32768, 32767]) as a point of the circle.
pub fn point(rem: i32) -> u32 {
    debug_assert!((-32768..32768).contains(&rem), "a remainder out of range: {rem}");
    (rem + 32768) as u32
}

/// A half-open segment `[lo, hi)` of a circle's points, `lo < hi`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Seg<P = u32> {
    pub lo: P,
    pub hi: P,
}

/// A CIRCLE of remainder points (`Rem::C`): points, stored words, transfers.
pub trait Circle: Copy + std::fmt::Debug + Default + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash + Send + Sync + 'static {
    type P: Copy + Ord + std::hash::Hash + std::fmt::Debug + Default + Send + Sync + 'static;
    /// A stored point or count (arena words, hull bounds), in point order.
    type W: Copy + Ord + std::hash::Hash + std::fmt::Debug + Default + Send + Sync + 'static;
    /// One axis's transfer: a guard and an action.
    type Act: Copy + std::fmt::Debug + PartialEq + Send + Sync + 'static;
    /// The points are `0..SIZE`.
    const SIZE: Self::P;
    const MAX_SEGS: usize;
    /// `p + 1` and `p - 1`.
    fn next(p: Self::P) -> Self::P;
    fn prev(p: Self::P) -> Self::P;
    /// A point, or a count (at most `SIZE`, or `MAX_SEGS`), as a word.
    fn store(p: Self::P) -> Self::W;
    fn load(w: Self::W) -> Self::P;
    fn count(n: usize) -> Self::W;
    fn len(w: Self::W) -> usize;
    /// A point (or `SIZE`) as a number and back (the hull's arithmetic).
    fn wide(p: Self::P) -> u64;
    fn narrow(v: u64) -> Self::P;
    /// A word's little-endian bytes on disk, and the hash of a hull's four
    /// words (statistics, fingerprints, the push cache's slot).
    const W_BYTES: usize;
    fn put_word(w: Self::W, out: &mut Vec<u8>);
    fn get_word(b: &[u8]) -> Self::W;
    fn hash4(w: [Self::W; 4]) -> u64;
    /// Are a hull's four words equal (`PushCache::get`)? 16.16's in one
    /// 8-byte compare; the doubles' word by word (a miss differs early).
    fn same4(a: [Self::W; 4], b: [Self::W; 4]) -> bool;
    /// The points an edge's axis takes.
    fn guard(a: &Self::Act) -> Seg<Self::P>;
    /// The point a constant action sends every point to.
    fn const_point(a: &Self::Act) -> Option<Self::P>;
    /// The IMAGE of `s`'s points inside the guard: exact for 16.16; for a
    /// monotone action the interval between its ends' images (a superset:
    /// sound where it is used, the pushes of `hull` and `reach`).
    fn image(a: &Self::Act, s: Seg<Self::P>, f: impl FnMut(Seg<Self::P>));
    /// The PREIMAGE of `s` under a non-constant action: the points of the
    /// guard it maps into `s`, EXACTLY.
    fn preimage(a: &Self::Act, s: Seg<Self::P>, f: impl FnMut(Seg<Self::P>));
    /// A segment as one word of the gate's fingerprints (`backward::gate_w_hash`).
    fn seg_word(s: Seg<Self::P>) -> u64;
    /// A point in the middle of `s` (a probe).
    fn mid(s: Seg<Self::P>) -> Self::P;
    fn kind(a: &Self::Act) -> &'static str;
    /// `a` CORRUPTED, to check a check (`rewrite check arcs --fault`): its
    /// guard's lowest point dropped (`guard`), or its image moved.
    fn faulty(a: &Self::Act, guard: bool) -> Self::Act;
}

/// 16.16's circle: 65536 points, its transfers rotations or constants.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fixed16;

impl Circle for Fixed16 {
    type P = u32;
    type W = u16;
    type Act = Transfer;
    const SIZE: u32 = CIRCLE;
    const MAX_SEGS: usize = MAX_SEGS;
    #[inline]
    fn next(p: u32) -> u32 {
        p + 1
    }
    #[inline]
    fn prev(p: u32) -> u32 {
        p - 1
    }
    #[inline]
    fn store(p: u32) -> u16 {
        p as u16
    }
    #[inline]
    fn load(w: u16) -> u32 {
        w as u32
    }
    #[inline]
    fn count(n: usize) -> u16 {
        n as u16
    }
    #[inline]
    fn len(w: u16) -> usize {
        w as usize
    }
    #[inline]
    fn wide(p: u32) -> u64 {
        p as u64
    }
    #[inline]
    fn narrow(v: u64) -> u32 {
        v as u32
    }
    const W_BYTES: usize = 2;
    fn put_word(w: u16, out: &mut Vec<u8>) {
        out.extend_from_slice(&w.to_le_bytes());
    }
    fn get_word(b: &[u8]) -> u16 {
        u16::from_le_bytes([b[0], b[1]])
    }
    /// The four words in one, low first: exact.
    #[inline]
    fn hash4(w: [u16; 4]) -> u64 {
        w[0] as u64 | (w[1] as u64) << 16 | (w[2] as u64) << 32 | (w[3] as u64) << 48
    }
    #[inline]
    fn same4(a: [u16; 4], b: [u16; 4]) -> bool {
        a == b
    }
    #[inline]
    fn guard(a: &Transfer) -> Seg {
        a.guard
    }
    #[inline]
    fn const_point(a: &Transfer) -> Option<u32> {
        match a.action {
            Action::Const(c) => Some(c),
            Action::Rotate(_) => None,
        }
    }
    #[inline]
    fn image(a: &Transfer, s: Seg, mut f: impl FnMut(Seg)) {
        if let Some(c) = clip(s, a.guard) {
            match a.action {
                Action::Rotate(v) => rot_seg(c, v.rem_euclid(CIRCLE as i32) as u32, f),
                Action::Const(p) => f(Seg { lo: p, hi: p + 1 }),
            }
        }
    }
    #[inline]
    fn preimage(a: &Transfer, s: Seg, mut f: impl FnMut(Seg)) {
        match a.action {
            Action::Rotate(v) => rot_seg(s, (-(v as i64)).rem_euclid(CIRCLE as i64) as u32, |r| {
                if let Some(c) = clip(r, a.guard) {
                    f(c);
                }
            }),
            Action::Const(c) => {
                if s.lo <= c && c < s.hi {
                    f(a.guard);
                }
            }
        }
    }
    #[inline]
    fn seg_word(s: Seg) -> u64 {
        (s.lo as u64) << 32 | s.hi as u64
    }
    fn mid(s: Seg) -> u32 {
        (s.lo + s.hi) / 2
    }
    fn kind(a: &Transfer) -> &'static str {
        match a.action {
            Action::Rotate(_) => "rotate",
            Action::Const(_) => "const",
        }
    }
    fn faulty(a: &Transfer, guard: bool) -> Transfer {
        let mut t = *a;
        if guard {
            t.guard.lo = (t.guard.lo + 1).min(t.guard.hi - 1);
        } else {
            t.action = match t.action {
                Action::Rotate(v) => Action::Rotate(v + 1),
                Action::Const(c) => Action::Const((c + 1) % CIRCLE),
            };
        }
        t
    }
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

/// The pieces a speed `v` (raw) cuts the circle into: `rem + v + 1/2` below
/// the next integer, and the rest (one piece for whole pixels).
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
pub struct Piece<P = u32> {
    pub y: Seg<P>,
    pub x: Seg<P>,
}

/// A segment as a `Region` stores it, `[lo, last]` (16.16's whole circle
/// fits 16 bits a bound); `repr(C)`: arena records are viewed as these.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(C)]
struct Stored<W> {
    lo: W,
    last: W,
}

impl<W: Copy> Stored<W> {
    fn of<C: Circle<W = W>>(s: Seg<C::P>) -> Self {
        debug_assert!(s.lo < s.hi && s.hi <= C::SIZE, "a segment of the circle: {s:?}");
        Stored { lo: C::store(s.lo), last: C::store(C::prev(s.hi)) }
    }
    fn seg<C: Circle<W = W>>(self) -> Seg<C::P> {
        Seg { lo: C::load(self.lo), hi: C::next(C::load(self.last)) }
    }
}

/// A `Region`'s slab: its y segment and its x set's end in `xs` (the start
/// is the previous slab's end); `repr(C)`, three words.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(C)]
struct Slab<W> {
    y: Stored<W>,
    end: W,
}

/// A set of the torus in CANONICAL form: sorted disjoint y slabs, each with
/// a non-empty x set, touching slabs never with the same one, so equal sets
/// are equal values; at most `Circle::MAX_SEGS` segments.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Region<C: Circle = Fixed16> {
    slabs: Vec<Slab<C::W>>,
    xs: Vec<Stored<C::W>>,
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

#[inline]
pub(super) fn clip<P: Ord + Copy>(s: Seg<P>, g: Seg<P>) -> Option<Seg<P>> {
    let (lo, hi) = (s.lo.max(g.lo), s.hi.min(g.hi));
    (lo < hi).then_some(Seg { lo, hi })
}

impl<C: Circle> Region<C> {
    pub fn full() -> Self {
        let all = Stored::of::<C>(Seg { lo: C::P::default(), hi: C::SIZE });
        Region { slabs: vec![Slab { y: all, end: C::count(1) }], xs: vec![all] }
    }
    pub fn as_ref(&self) -> RegionRef<'_, C> {
        RegionRef { slabs: &self.slabs, xs: &self.xs }
    }
    pub fn is_empty(&self) -> bool {
        self.slabs.is_empty()
    }
    pub fn n_segs(&self) -> usize {
        self.xs.len()
    }
    pub fn slabs(&self) -> impl Iterator<Item = (Seg<C::P>, impl Iterator<Item = Seg<C::P>> + '_)> + '_ {
        self.as_ref().slabs()
    }
    pub fn contains(&self, x: C::P, y: C::P) -> bool {
        self.as_ref().contains(x, y)
    }
    pub fn pieces(&self, out: &mut Vec<Piece<C::P>>) {
        self.as_ref().pieces(out)
    }
    pub fn push(&self, x: C::Act, y: C::Act, out: &mut Vec<Piece<C::P>>) {
        self.as_ref().push(x, y, out)
    }
    pub fn clip_into(&self, p: Piece<C::P>, out: &mut Vec<Piece<C::P>>) {
        self.as_ref().clip_into(p, out)
    }
    pub fn pull(&self, x: C::Act, y: C::Act, out: &mut Vec<Piece<C::P>>) {
        self.as_ref().pull(x, y, out)
    }
    /// The union of `pieces`, canonical (one sweep over y). `pieces` is
    /// left in an unspecified order.
    pub fn from_pieces(pieces: &mut [Piece<C::P>], scratch: &mut Vec<Seg<C::P>>) -> Self {
        Self::try_from_pieces(pieces, scratch, C::MAX_SEGS).unwrap_or_else(|| panic!("a region past {} segments", C::MAX_SEGS))
    }

    /// `from_pieces`, `None` past `max` x segments (capped at `MAX_SEGS`).
    pub fn try_from_pieces(pieces: &mut [Piece<C::P>], scratch: &mut Vec<Seg<C::P>>, max: usize) -> Option<Self> {
        let max = max.min(C::MAX_SEGS);
        let mut out = Region::default();
        if pieces.is_empty() {
            return Some(out);
        }
        pieces.sort_unstable_by_key(|p| p.y.lo);
        let mut bounds: Vec<C::P> = pieces.iter().flat_map(|p| [p.y.lo, p.y.hi]).collect();
        bounds.sort_unstable();
        bounds.dedup();
        let mut active: Vec<Piece<C::P>> = Vec::new();
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
            let mut merged: Vec<Seg<C::P>> = Vec::with_capacity(scratch.len());
            for &s in scratch.iter() {
                match merged.last_mut() {
                    Some(l) if s.lo <= l.hi => l.hi = l.hi.max(s.hi),
                    _ => merged.push(s),
                }
            }
            let start = out.slabs.last().map_or(0, |s| C::len(s.end));
            let prev_start = if out.slabs.len() >= 2 { C::len(out.slabs[out.slabs.len() - 2].end) } else { 0 };
            if let Some(last) = out.slabs.last_mut() {
                if C::next(C::load(last.y.last)) == lo && out.xs[prev_start..start].iter().map(|s| s.seg::<C>()).eq(merged.iter().copied()) {
                    last.y = Stored::of::<C>(Seg { lo: C::load(last.y.lo), hi });
                    continue;
                }
            }
            out.xs.extend(merged.iter().map(|&s| Stored::of::<C>(s)));
            if out.xs.len() > max {
                return None;
            }
            out.slabs.push(Slab { y: Stored::of::<C>(Seg { lo, hi }), end: C::count(out.xs.len()) });
        }
        Some(out)
    }
}

/// A set of the torus, borrowed: a `Region`'s or a `RegionArena`'s.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct RegionRef<'a, C: Circle = Fixed16> {
    slabs: &'a [Slab<C::W>],
    xs: &'a [Stored<C::W>],
}

impl<C: Circle> Clone for RegionRef<'_, C> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<C: Circle> Copy for RegionRef<'_, C> {}

impl<'a, C: Circle> RegionRef<'a, C> {
    pub fn is_empty(self) -> bool {
        self.slabs.is_empty()
    }
    pub fn n_segs(self) -> usize {
        self.xs.len()
    }
    /// Slabs (a y segment and its x set), in y order.
    pub fn slabs(self) -> impl Iterator<Item = (Seg<C::P>, impl Iterator<Item = Seg<C::P>> + 'a)> + 'a {
        self.stored().map(|(y, xs)| (y, xs.iter().map(|s| s.seg::<C>())))
    }
    fn stored(self) -> impl Iterator<Item = (Seg<C::P>, &'a [Stored<C::W>])> + 'a {
        let mut start = 0;
        self.slabs.iter().map(move |s| {
            let end = C::len(s.end);
            let xs = &self.xs[start..end];
            start = end;
            (s.y.seg::<C>(), xs)
        })
    }
    /// The set as pieces (a slab's y times each x segment), into `out`.
    pub fn pieces(self, out: &mut Vec<Piece<C::P>>) {
        for (y, xs) in self.slabs() {
            out.extend(xs.map(|x| Piece { y, x }));
        }
    }
    fn slab_at(self, y: C::P) -> Option<&'a [Stored<C::W>]> {
        let i = self.slabs.partition_point(|s| C::load(s.y.last) < y);
        let s = self.slabs.get(i).filter(|s| C::load(s.y.lo) <= y)?;
        let start = if i == 0 { 0 } else { C::len(self.slabs[i - 1].end) };
        Some(&self.xs[start..C::len(s.end)])
    }
    fn holds(xs: &[Stored<C::W>], p: C::P) -> bool {
        let i = xs.partition_point(|s| C::load(s.last) < p);
        xs.get(i).is_some_and(|s| C::load(s.lo) <= p)
    }
    pub fn contains(self, x: C::P, y: C::P) -> bool {
        self.slab_at(y).is_some_and(|xs| Self::holds(xs, x))
    }
    /// The IMAGE under one edge, into `out`: the points inside its guards,
    /// moved by its actions (`Circle::image`).
    pub fn push(self, x: C::Act, y: C::Act, out: &mut Vec<Piece<C::P>>) {
        let mut ximg: Vec<Seg<C::P>> = Vec::new();
        for (ys, xs) in self.slabs() {
            ximg.clear();
            for s in xs {
                C::image(&x, s, |r| ximg.push(r));
            }
            if ximg.is_empty() {
                continue;
            }
            C::image(&y, ys, |r| out.extend(ximg.iter().map(|&xr| Piece { y: r, x: xr })));
        }
    }
    /// `p` intersected with `self`, as pieces appended to `out`.
    pub fn clip_into(self, p: Piece<C::P>, out: &mut Vec<Piece<C::P>>) {
        for (ys, xs) in self.slabs() {
            let Some(y) = clip(ys, p.y) else { continue };
            for s in xs {
                if let Some(x) = clip(s, p.x) {
                    out.push(Piece { y, x });
                }
            }
        }
    }
    /// The PREIMAGE under one edge, into `out`: the points inside its guards
    /// it takes into `self` (`Circle::preimage`; a constant axis takes its
    /// whole guard where `self` holds its point).
    pub fn pull(self, x: C::Act, y: C::Act, out: &mut Vec<Piece<C::P>>) {
        let emit_x = |ys: Seg<C::P>, xs: &[Stored<C::W>], out: &mut Vec<Piece<C::P>>| match C::const_point(&x) {
            None => {
                for s in xs {
                    C::preimage(&x, s.seg::<C>(), |c| out.push(Piece { y: ys, x: c }));
                }
            }
            Some(c) => {
                if Self::holds(xs, c) {
                    out.push(Piece { y: ys, x: C::guard(&x) });
                }
            }
        };
        match C::const_point(&y) {
            None => {
                for (ys, xs) in self.stored() {
                    C::preimage(&y, ys, |c| emit_x(c, xs, out));
                }
            }
            Some(c) => {
                if let Some(xs) = self.slab_at(c) {
                    emit_x(C::guard(&y), xs, out);
                }
            }
        }
    }
}

impl<C: Circle> PartialEq<Region<C>> for RegionRef<'_, C> {
    fn eq(&self, o: &Region<C>) -> bool {
        *self == o.as_ref()
    }
}

const _: () = assert!(std::mem::size_of::<Stored<u16>>() == 4 && std::mem::align_of::<Stored<u16>>() == 2, "Stored is two u16s");
const _: () = assert!(std::mem::size_of::<Slab<u16>>() == 6 && std::mem::align_of::<Slab<u16>>() == 2, "Slab is three u16s");
const _: () = assert!(std::mem::size_of::<Stored<u64>>() == 16 && std::mem::size_of::<Slab<u64>>() == 24, "Stored and Slab of u64 words");

/// Words a CHUNK of a `RegionArena` holds at most (64 MB of 16.16's); a
/// record, at most `2 + 5 * MAX_SEGS` words, never straddles two.
const CHUNK_WORDS: usize = 1 << 25;

/// The backward's arena of distinct sets: per set a RECORD of words (slab
/// and segment counts, then 3 words a slab, 2 a segment) in CHUNKS, and its
/// place (8 B). A full chunk is SEALED: kept, or (`spill_to`) mapped from a
/// scratch file (as `backward::Spill`). Records never move, so it grows
/// without copying itself (a doubling list peaks at 90 GB for 50 GB of sets).
pub struct RegionArena<C: Circle = Fixed16> {
    /// Set `i`'s record: its chunk (high 32 bits; `sealed.len()` is the open
    /// one) and its first word there.
    at: Vec<u64>,
    /// A chunk's words at most (`CHUNK_WORDS`; tests seal smaller ones).
    chunk: usize,
    sealed: Vec<Words<C::W>>,
    open: Vec<C::W>,
    /// Words in the sealed chunks, and in those of them mapped.
    sealed_words: u64,
    mapped_words: u64,
    spill: Option<std::path::PathBuf>,
}

enum Words<W> {
    Mem(Vec<W>),
    Map(memmap2::Mmap),
}

impl<W> Words<W> {
    fn words(&self) -> &[W] {
        match self {
            Words::Mem(v) => v,
            // SAFETY: a map starts on a page (aligned for any word) and holds
            // whole words (`seal` writes them); words are plain data.
            Words::Map(m) => unsafe { std::slice::from_raw_parts(m.as_ptr().cast::<W>(), m.len() / std::mem::size_of::<W>()) },
        }
    }
}

/// The set whose record starts `w`.
fn record<C: Circle>(w: &[C::W]) -> RegionRef<'_, C> {
    let (ns, nx) = (C::len(w[0]), C::len(w[1]));
    let (s, x) = (&w[2..2 + 3 * ns], &w[2 + 3 * ns..2 + 3 * ns + 2 * nx]);
    const { assert!(std::mem::size_of::<Slab<C::W>>() == 3 * std::mem::size_of::<C::W>() && std::mem::size_of::<Stored<C::W>>() == 2 * std::mem::size_of::<C::W>()) };
    // SAFETY: `Slab` and `Stored` are `repr(C)` structs of 3 and 2 words
    // (asserted: no padding), any bit pattern valid; the slices are in bounds.
    unsafe { RegionRef { slabs: std::slice::from_raw_parts(s.as_ptr().cast::<Slab<C::W>>(), ns), xs: std::slice::from_raw_parts(x.as_ptr().cast::<Stored<C::W>>(), nx) } }
}

impl<C: Circle> Default for RegionArena<C> {
    fn default() -> Self {
        RegionArena { at: Vec::new(), chunk: CHUNK_WORDS, sealed: Vec::new(), open: Vec::new(), sealed_words: 0, mapped_words: 0, spill: None }
    }
}

impl<C: Circle> RegionArena<C> {
    /// Seal its full chunks into scratch files in `dir` (which must exist).
    pub fn spill_to(&mut self, dir: &std::path::Path) {
        self.spill = Some(dir.to_path_buf());
    }
    pub fn len(&self) -> usize {
        self.at.len()
    }
    pub fn is_empty(&self) -> bool {
        self.at.is_empty()
    }
    /// Append `r`; its index.
    pub fn push(&mut self, r: RegionRef<C>) -> u32 {
        let i = u32::try_from(self.at.len()).expect("a region arena past u32 sets");
        let need = 2 + 3 * r.slabs.len() + 2 * r.xs.len();
        if self.open.len() + need > self.chunk {
            self.seal();
        }
        self.at.push((self.sealed.len() as u64) << 32 | self.open.len() as u64);
        self.open.extend([C::count(r.slabs.len()), C::count(r.xs.len())]);
        for s in r.slabs {
            self.open.extend([s.y.lo, s.y.last, s.end]);
        }
        for x in r.xs {
            self.open.extend([x.lo, x.last]);
        }
        i
    }
    fn seal(&mut self) {
        let open = std::mem::take(&mut self.open);
        self.sealed_words += open.len() as u64;
        let w = match &self.spill {
            None => Words::Mem(open),
            Some(dir) => {
                let path = dir.join(format!("sets-{:05}.bin", self.sealed.len()));
                let map = || -> std::io::Result<memmap2::Mmap> {
                    // SAFETY: plain words as their bytes.
                    std::fs::write(&path, unsafe { std::slice::from_raw_parts(open.as_ptr().cast::<u8>(), std::mem::size_of_val(&open[..])) })?;
                    let f = std::fs::File::open(&path)?;
                    // SAFETY: the file is this process's own, complete, and
                    // unlinked right after: nothing else writes it.
                    let m = unsafe { memmap2::Mmap::map(&f)? };
                    std::fs::remove_file(&path)?;
                    Ok(m)
                };
                let m = map().unwrap_or_else(|e| panic!("FATAL: the arc phase's set chunk {}: {e}", path.display()));
                self.mapped_words += open.len() as u64;
                Words::Map(m)
            }
        };
        self.sealed.push(w);
    }
    pub fn get(&self, i: u32) -> RegionRef<'_, C> {
        let a = self.at[i as usize];
        let c = (a >> 32) as usize;
        let w = if c == self.sealed.len() { &self.open[..] } else { self.sealed[c].words() };
        record::<C>(&w[a as u32 as usize..])
    }
    /// The bytes its sets take (their records and places).
    pub fn bytes(&self) -> usize {
        (self.sealed_words as usize + self.open.len()) * std::mem::size_of::<C::W>() + self.at.len() * 8
    }
    /// The bytes of its sealed chunks mapped from scratch files.
    pub fn mapped_bytes(&self) -> usize {
        self.mapped_words as usize * std::mem::size_of::<C::W>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::backward::tests::apply;

    #[test]
    fn an_arena_returns_its_sets_across_sealed_and_spilled_chunks() {
        // Random regions (a few pieces each), pushed into arenas with small
        // chunks - kept in memory, and spilled to files - read back equal.
        let mut rng = 0x9e37_79b9_7f4a_7c15u64;
        let mut next = move |n: u32| {
            rng ^= rng << 13;
            rng ^= rng >> 7;
            rng ^= rng << 17;
            (rng % n as u64) as u32
        };
        let mut regions = Vec::new();
        let mut scratch = Vec::new();
        for _ in 0..500 {
            let mut pieces: Vec<Piece> = (0..1 + next(6))
                .map(|_| {
                    let (x, y) = (next(CIRCLE - 2), next(CIRCLE - 2));
                    Piece { x: Seg { lo: x, hi: x + 1 + next(CIRCLE - 1 - x) }, y: Seg { lo: y, hi: y + 1 + next(CIRCLE - 1 - y) } }
                })
                .collect();
            regions.push(Region::<Fixed16>::from_pieces(&mut pieces, &mut scratch));
        }
        regions.push(Region::default());
        let dir = std::env::temp_dir().join(format!("arena-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        for spill in [false, true] {
            let mut a = RegionArena { chunk: 64, ..RegionArena::default() };
            if spill {
                a.spill_to(&dir);
            }
            let ids: Vec<u32> = regions.iter().map(|r| a.push(r.as_ref())).collect();
            assert!(a.sealed.len() > 10, "small chunks seal");
            assert_eq!(a.mapped_bytes() > 0, spill);
            for (r, &i) in regions.iter().zip(&ids) {
                assert_eq!(a.get(i), *r);
            }
            assert_eq!(a.bytes(), regions.iter().map(|r| 4 + 6 * r.slabs.len() + 4 * r.xs.len() + 8).sum::<usize>());
        }
        assert_eq!(std::fs::read_dir(&dir).unwrap().count(), 0, "the chunk files are unlinked once mapped");
        std::fs::remove_dir(&dir).unwrap();
    }

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
            let r = Region::<Fixed16>::from_pieces(&mut ps, &mut Vec::new());
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
            assert_eq!(Region::<Fixed16>::from_pieces(&mut rev, &mut Vec::new()), r);
            // pull: a point p is in the result iff the guards take it and its image is in r.
            let act = |s: &mut u64| if lcg(s) % 4 == 0 { Action::Const(lcg(s) % CIRCLE) } else { Action::Rotate((lcg(s) % CIRCLE) as i32 - 32768) };
            let (tx, ty) = (Transfer { guard: seg(&mut s), action: act(&mut s) }, Transfer { guard: seg(&mut s), action: act(&mut s) });
            let mut out = Vec::new();
            r.pull(tx, ty, &mut out);
            let pr = Region::<Fixed16>::from_pieces(&mut out, &mut Vec::new());
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
                    let want = tx.takes(x) && ty.takes(y) && r.contains(apply(tx.action, x), apply(ty.action, y));
                    assert_eq!(pr.contains(x, y), want, "pull at ({x}, {y})");
                }
            }
            // push: a point q is in the image iff some point of r inside the
            // guards maps to it - checked from both sides: every probed point
            // of r maps into the image, and the image's points pull back into r.
            let mut out = Vec::new();
            r.push(tx, ty, &mut out);
            let img = Region::<Fixed16>::from_pieces(&mut out, &mut Vec::new());
            for &x in &pp {
                for &y in &pp {
                    if tx.takes(x) && ty.takes(y) && r.contains(x, y) {
                        assert!(img.contains(apply(tx.action, x), apply(ty.action, y)), "push misses the image of ({x}, {y})");
                    }
                }
            }
            let mut back = Vec::new();
            img.pull(tx, ty, &mut back);
            let mut inter = Vec::new();
            for p in back {
                r.clip_into(p, &mut inter);
            }
            let back = Region::<Fixed16>::from_pieces(&mut inter, &mut Vec::new());
            let mut again = Vec::new();
            back.push(tx, ty, &mut again);
            assert_eq!(Region::<Fixed16>::from_pieces(&mut again, &mut Vec::new()), img, "the image is exactly the image of its preimage in r");
            // clip_into is the intersection.
            let q = Piece { y: seg(&mut s), x: seg(&mut s) };
            let mut cl = Vec::new();
            r.clip_into(q, &mut cl);
            let cr = Region::<Fixed16>::from_pieces(&mut cl, &mut Vec::new());
            for &x in &probes {
                for &y in &probes {
                    let inq = q.x.lo <= x && x < q.x.hi && q.y.lo <= y && y < q.y.hi;
                    assert_eq!(cr.contains(x, y), inq && r.contains(x, y), "clip at ({x}, {y})");
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
