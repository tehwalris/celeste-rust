//! Sets of sub-pixel remainders as ARCS (plans/architecture.md "Arcs: the
//! rotation graph"): along an edge a set maps by "intersect with the guard,
//! then act", which distributes over unions. A remainder `r` (raw 16.16) is
//! the point `r + 32768` of `0..CIRCLE`; a set is sorted, disjoint,
//! non-adjacent half-open segments (an arc across the wrap is two). The
//! two-axis `Region` is generic over the `Circle` (16.16's `Fixed16`, the
//! doubles' `Doubles`), and so is an edge's `Transfer`, one model for both:
//! a guard and a MONOTONE action or a constant; the pull is exact on both.

use super::rem::Word;

/// Points on the circle: 1 px in raw 16.16 units.
pub(crate) const CIRCLE: u32 = 1 << 16;

/// The most x segments a `Region` holds (its slabs' 16-bit end index).
pub(crate) const MAX_SEGS: usize = (1 << 16) - 1;

/// A remainder (raw, [-32768, 32767]) as a point of the circle.
pub(crate) fn point(rem: i32) -> u32 {
    debug_assert!((-32768..32768).contains(&rem), "a remainder out of range: {rem}");
    (rem + 32768) as u32
}

/// A half-open segment `[lo, hi)` of a circle's points, `lo < hi`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Seg<P = u32> {
    pub(crate) lo: P,
    pub(crate) hi: P,
}

/// A CIRCLE of remainder points (`Rem::C`): points, stored words, and the
/// MONOTONE maps its transfers act by.
pub trait Circle: Copy + std::fmt::Debug + Default + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash + Send + Sync + 'static {
    type P: Copy + Ord + std::hash::Hash + std::fmt::Debug + Default + Send + Sync + 'static;
    /// A stored point or count (arena words, hull bounds), in point order.
    type W: Word;
    /// A transfer's monotone action: 16.16's a translation, the doubles'
    /// `r -> fl(fl(r + ox) - k)`.
    type Mono: Copy + std::fmt::Debug + PartialEq + Eq + std::hash::Hash + Send + Sync + 'static;
    /// The points are `0..SIZE`.
    const SIZE: Self::P;
    const MAX_SEGS: usize;
    /// `p + 1` and `p - 1`.
    fn next(p: Self::P) -> Self::P;
    fn prev(p: Self::P) -> Self::P;
    /// A point (or `SIZE`) as a number and back (the hull's arithmetic).
    fn wide(p: Self::P) -> u64;
    fn narrow(v: u64) -> Self::P;
    /// A point as a word, and back.
    #[inline]
    fn store(p: Self::P) -> Self::W {
        Self::W::of(Self::wide(p))
    }
    #[inline]
    fn load(w: Self::W) -> Self::P {
        Self::narrow(w.get())
    }
    /// The image of a guard's point `p` under `m` (inside the circle: the
    /// decode checked it).
    fn apply(m: &Self::Mono, p: Self::P) -> Self::P;
    /// The points of guard `g` that `m` maps into `s`, EXACTLY (`m` is
    /// monotone: one segment).
    fn pull_mono(m: &Self::Mono, g: Seg<Self::P>, s: Seg<Self::P>) -> Option<Seg<Self::P>>;
    /// A segment as one word of the gate's fingerprints (`backward::gate_w_hash`).
    fn seg_word(s: Seg<Self::P>) -> u64;
    /// A point in the middle of `s` (a probe).
    fn mid(s: Seg<Self::P>) -> Self::P;
}

/// What a frame does to one axis's remainder along an edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Act<C: Circle> {
    /// A monotone map of the guard into the circle.
    Mono(C::Mono),
    /// `rem := c` (a collision resets it): the point `c` of the circle.
    Const(C::P),
}

/// ONE AXIS OF AN EDGE (`Rem::xfer`): the remainders that take it (its
/// GUARD: one piece of the circle, never wrapping) and what it does to
/// them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Transfer<C: Circle = Fixed16> {
    pub(crate) guard: Seg<C::P>,
    pub(crate) act: Act<C>,
}

impl<C: Circle> Transfer<C> {
    /// The point a constant action sends every point to.
    #[inline]
    pub(crate) fn const_point(&self) -> Option<C::P> {
        match self.act {
            Act::Const(c) => Some(c),
            Act::Mono(_) => None,
        }
    }

    /// Does it take point `p`?
    #[inline]
    pub(crate) fn takes(&self, p: C::P) -> bool {
        self.guard.lo <= p && p < self.guard.hi
    }

    /// The IMAGE of `s`'s points inside the guard (`None`: none): the
    /// interval between its ends' images (the action is monotone) - exact
    /// for 16.16's translations, a superset for the doubles' (sound where
    /// it is used, the pushes of `hull` and `reach`).
    #[inline]
    pub(crate) fn image(&self, s: Seg<C::P>) -> Option<Seg<C::P>> {
        let c = clip(s, self.guard)?;
        Some(match &self.act {
            Act::Mono(m) => Seg { lo: C::apply(m, c.lo), hi: C::next(C::apply(m, C::prev(c.hi))) },
            Act::Const(p) => Seg { lo: *p, hi: C::next(*p) },
        })
    }

    /// The PREIMAGE of `s`: the points of the guard it takes into `s`,
    /// EXACTLY (a constant's: its whole guard where `s` holds its point).
    #[inline]
    pub(crate) fn pull(&self, s: Seg<C::P>) -> Option<Seg<C::P>> {
        match &self.act {
            Act::Mono(m) => C::pull_mono(m, self.guard, s),
            Act::Const(c) => (s.lo <= *c && *c < s.hi).then_some(self.guard),
        }
    }

    pub(crate) fn kind(&self) -> &'static str {
        match self.act {
            Act::Mono(_) => "mono",
            Act::Const(_) => "const",
        }
    }
}

/// 16.16's circle: 65536 points, its transfers translations (`rem += ox`
/// less the amount: the guard is one floor's, so its image never wraps) or
/// constants.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fixed16;

impl Circle for Fixed16 {
    type P = u32;
    type W = u16;
    type Mono = i32;
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
    fn wide(p: u32) -> u64 {
        p as u64
    }
    #[inline]
    fn narrow(v: u64) -> u32 {
        v as u32
    }
    #[inline]
    fn apply(d: &i32, p: u32) -> u32 {
        (p as i32 + d) as u32
    }
    #[inline]
    fn pull_mono(d: &i32, g: Seg, s: Seg) -> Option<Seg> {
        let lo = (s.lo as i64 - *d as i64).max(g.lo as i64);
        let hi = (s.hi as i64 - *d as i64).min(g.hi as i64);
        (lo < hi).then(|| Seg { lo: lo as u32, hi: hi as u32 })
    }
    #[inline]
    fn seg_word(s: Seg) -> u64 {
        (s.lo as u64) << 32 | s.hi as u64
    }
    fn mid(s: Seg) -> u32 {
        (s.lo + s.hi) / 2
    }
}

/// The index of `0` among the doubles' points (and the count of doubles in
/// `[-1/2, 0)`): `1/2`'s bits.
pub(crate) const ZERO: u64 = 0x3FE0_0000_0000_0000;
/// The doubles' points: the doubles of `[-1/2, 1/2)` by value.
pub(crate) const POINTS: u64 = 2 * ZERO;

/// The doubles' point of remainder `v`, its INDEX in value order among the
/// doubles of `[-1/2, 1/2)` (`0` is `-1/2`, `ZERO` is `0` and `-0.0` too: no
/// read of a remainder tells the zeros apart, `POINTS - 1` is
/// `1/2 - 2^-54`); `None` outside `[-1/2, 1/2)`.
#[inline]
pub(crate) fn index(v: f64) -> Option<u64> {
    if !(-0.5..0.5).contains(&v) {
        return None;
    }
    let b = v.abs().to_bits();
    Some(if v > 0.0 { ZERO + b } else { ZERO - b })
}

/// The remainder of the doubles' point `i` (`+0.0` at `ZERO`).
#[inline]
pub(crate) fn value(i: u64) -> f64 {
    debug_assert!(i < POINTS);
    if i >= ZERO {
        f64::from_bits(i - ZERO)
    } else {
        -f64::from_bits(ZERO - i)
    }
}

/// The doubles' monotone action `r -> fl(fl(r + ox) - k)` (`ox` its bits).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Shift {
    pub(crate) ox: u64,
    pub(crate) k: i32,
}

/// `move` on one axis after the round split: `fl(fl(r + ox) - k)`.
#[inline]
fn shift(r: f64, ox: f64, k: f64) -> f64 {
    (r + ox) - k
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

/// The DOUBLES' circle (plans/float.md "The remainder model and the round
/// fork"): `POINTS` points (`index`) as `u64` words, monotone maps `Shift`:
/// the PULL of a segment is the guard's points between two bisections,
/// exactly; the image the interval between its ends' images.
///
/// THE TOP SLIVER: at an integer speed the doubles `{1/2 - 2^-54,
/// 1/2 - 2^-53}` round onto the cut (`fl(r - 2) = -3/2`), a piece no 16.16
/// remainder takes; the hull forward pushes it only where a hull reaches it.
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
    fn wide(p: u64) -> u64 {
        p
    }
    #[inline]
    fn narrow(v: u64) -> u64 {
        v
    }
    #[inline]
    fn apply(m: &Shift, i: u64) -> u64 {
        let v = shift(value(i), f64::from_bits(m.ox), m.k as f64);
        index(v).unwrap_or_else(|| panic!("transfer {m:?}: the image {v:?} of {:?} is not a remainder", value(i)))
    }
    /// The guard's points whose image is in `s`: past every point whose
    /// image is below `s.lo`, before the first whose image reaches `s.hi`.
    fn pull_mono(m: &Shift, g: Seg<u64>, s: Seg<u64>) -> Option<Seg<u64>> {
        let lo = first_in(g.lo, g.hi, |r| Self::apply(m, r) >= s.lo);
        let hi = first_in(lo, g.hi, |r| Self::apply(m, r) >= s.hi);
        (lo < hi).then_some(Seg { lo, hi })
    }
    #[inline]
    fn seg_word(s: Seg<u64>) -> u64 {
        celeste_engine::runtime2::mix64(s.lo) ^ s.hi.rotate_left(32)
    }
    fn mid(s: Seg<u64>) -> u64 {
        s.lo + (s.hi - s.lo) / 2
    }
}

/// One axis-aligned piece of the torus: `[y.lo, y.hi) x [x.lo, x.hi)`, no
/// wrap on either axis (a wrapping arc is two pieces).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Piece<P = u32> {
    pub(crate) y: Seg<P>,
    pub(crate) x: Seg<P>,
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
pub(crate) struct Region<C: Circle = Fixed16> {
    slabs: Vec<Slab<C::W>>,
    xs: Vec<Stored<C::W>>,
}

#[inline]
pub(super) fn clip<P: Ord + Copy>(s: Seg<P>, g: Seg<P>) -> Option<Seg<P>> {
    let (lo, hi) = (s.lo.max(g.lo), s.hi.min(g.hi));
    (lo < hi).then_some(Seg { lo, hi })
}

impl<C: Circle> Region<C> {
    pub(crate) fn full() -> Self {
        let all = Stored::of::<C>(Seg { lo: C::P::default(), hi: C::SIZE });
        Region { slabs: vec![Slab { y: all, end: C::W::of(1) }], xs: vec![all] }
    }
    pub(crate) fn as_ref(&self) -> RegionRef<'_, C> {
        RegionRef { slabs: &self.slabs, xs: &self.xs }
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.slabs.is_empty()
    }
    pub(crate) fn n_segs(&self) -> usize {
        self.xs.len()
    }
    /// The union of `pieces`, canonical (one sweep over y). `pieces` is
    /// left in an unspecified order.
    pub(crate) fn from_pieces(pieces: &mut [Piece<C::P>], scratch: &mut Vec<Seg<C::P>>) -> Self {
        Self::try_from_pieces(pieces, scratch, C::MAX_SEGS).unwrap_or_else(|| panic!("a region past {} segments", C::MAX_SEGS))
    }

    /// `from_pieces`, `None` past `max` x segments (capped at `MAX_SEGS`).
    pub(crate) fn try_from_pieces(pieces: &mut [Piece<C::P>], scratch: &mut Vec<Seg<C::P>>, max: usize) -> Option<Self> {
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
            let start = out.slabs.last().map_or(0, |s| s.end.get() as usize);
            let prev_start = if out.slabs.len() >= 2 { out.slabs[out.slabs.len() - 2].end.get() as usize } else { 0 };
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
            out.slabs.push(Slab { y: Stored::of::<C>(Seg { lo, hi }), end: C::W::of(out.xs.len() as u64) });
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
    pub(crate) fn n_segs(self) -> usize {
        self.xs.len()
    }
    /// Slabs (a y segment and its x set), in y order.
    pub(crate) fn slabs(self) -> impl Iterator<Item = (Seg<C::P>, impl Iterator<Item = Seg<C::P>> + 'a)> + 'a {
        self.stored().map(|(y, xs)| (y, xs.iter().map(|s| s.seg::<C>())))
    }
    fn stored(self) -> impl Iterator<Item = (Seg<C::P>, &'a [Stored<C::W>])> + 'a {
        let mut start = 0;
        self.slabs.iter().map(move |s| {
            let end = s.end.get() as usize;
            let xs = &self.xs[start..end];
            start = end;
            (s.y.seg::<C>(), xs)
        })
    }
    /// The set as pieces (a slab's y times each x segment), into `out`.
    pub(crate) fn pieces(self, out: &mut Vec<Piece<C::P>>) {
        for (y, xs) in self.slabs() {
            out.extend(xs.map(|x| Piece { y, x }));
        }
    }
    fn slab_at(self, y: C::P) -> Option<&'a [Stored<C::W>]> {
        let i = self.slabs.partition_point(|s| C::load(s.y.last) < y);
        let s = self.slabs.get(i).filter(|s| C::load(s.y.lo) <= y)?;
        let start = if i == 0 { 0 } else { self.slabs[i - 1].end.get() as usize };
        Some(&self.xs[start..s.end.get() as usize])
    }
    fn holds(xs: &[Stored<C::W>], p: C::P) -> bool {
        let i = xs.partition_point(|s| C::load(s.last) < p);
        xs.get(i).is_some_and(|s| C::load(s.lo) <= p)
    }
    pub(crate) fn contains(self, x: C::P, y: C::P) -> bool {
        self.slab_at(y).is_some_and(|xs| Self::holds(xs, x))
    }
    /// The IMAGE under one edge, into `out`: the points inside its guards,
    /// moved by its actions (`Transfer::image`).
    pub(crate) fn push(self, x: Transfer<C>, y: Transfer<C>, out: &mut Vec<Piece<C::P>>) {
        let mut ximg: Vec<Seg<C::P>> = Vec::new();
        for (ys, xs) in self.slabs() {
            ximg.clear();
            ximg.extend(xs.filter_map(|s| x.image(s)));
            if ximg.is_empty() {
                continue;
            }
            if let Some(r) = y.image(ys) {
                out.extend(ximg.iter().map(|&xr| Piece { y: r, x: xr }));
            }
        }
    }
    /// `p` intersected with `self`, as pieces appended to `out`.
    pub(crate) fn clip_into(self, p: Piece<C::P>, out: &mut Vec<Piece<C::P>>) {
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
    /// it takes into `self` (`Transfer::pull`; a constant axis takes its
    /// whole guard where `self` holds its point).
    pub(crate) fn pull(self, x: Transfer<C>, y: Transfer<C>, out: &mut Vec<Piece<C::P>>) {
        let emit_x = |ys: Seg<C::P>, xs: &[Stored<C::W>], out: &mut Vec<Piece<C::P>>| match x.const_point() {
            None => out.extend(xs.iter().filter_map(|s| x.pull(s.seg::<C>())).map(|c| Piece { y: ys, x: c })),
            Some(c) => {
                if Self::holds(xs, c) {
                    out.push(Piece { y: ys, x: x.guard });
                }
            }
        };
        match y.const_point() {
            None => {
                for (ys, xs) in self.stored() {
                    if let Some(c) = y.pull(ys) {
                        emit_x(c, xs, out);
                    }
                }
            }
            Some(c) => {
                if let Some(xs) = self.slab_at(c) {
                    emit_x(y.guard, xs, out);
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
/// scratch file (`storage::scratch`). Records never move, so it grows
/// without copying itself (a doubling list peaks at 90 GB for 50 GB of sets).
pub(crate) struct RegionArena<C: Circle = Fixed16> {
    /// Set `i`'s record: its chunk (high 32 bits; `sealed.len()` is the open
    /// one) and its first word there.
    at: Vec<u64>,
    /// A chunk's words at most (`CHUNK_WORDS`; tests seal smaller ones).
    chunk: usize,
    sealed: Vec<crate::storage::scratch::Words<C::W>>,
    open: Vec<C::W>,
    /// Words in the sealed chunks, and in those of them mapped.
    sealed_words: u64,
    mapped_words: u64,
    spill: Option<std::path::PathBuf>,
}

/// SETS ONCE: an arena of sets and their index by hash. A set equal to
/// one held is found; a new one is pushed (a hash collision with a
/// different set: pushed apart, the index keeps the first).
#[derive(Default)]
pub(crate) struct Interner<C: Circle = Fixed16> {
    arena: RegionArena<C>,
    index: rustc_hash::FxHashMap<u64, u32>,
}

impl<C: Circle> Interner<C> {
    /// Set `r` (its hash `h`, `region_hash`), if held.
    pub(crate) fn find(&self, r: RegionRef<C>, h: u64) -> Option<u32> {
        self.index.get(&h).copied().filter(|&s| self.arena.get(s) == r)
    }
    /// Set `r` held once: found, or pushed.
    pub(crate) fn intern(&mut self, r: RegionRef<C>) -> u32 {
        let h = region_hash(r);
        self.find(r, h).unwrap_or_else(|| self.push_hashed(r, h))
    }
    /// Set `r` pushed (a copy kept apart if held), its hash indexed if free.
    pub(crate) fn push(&mut self, r: RegionRef<C>) -> u32 {
        self.push_hashed(r, region_hash(r))
    }
    fn push_hashed(&mut self, r: RegionRef<C>, h: u64) -> u32 {
        let s = self.arena.push(r);
        self.index.entry(h).or_insert(s);
        s
    }
    pub(crate) fn get(&self, i: u32) -> RegionRef<'_, C> {
        self.arena.get(i)
    }
    pub(crate) fn len(&self) -> usize {
        self.arena.len()
    }
    /// The bytes its sets take (`RegionArena::bytes`).
    pub(crate) fn bytes(&self) -> usize {
        self.arena.bytes()
    }
    pub(crate) fn mapped_bytes(&self) -> usize {
        self.arena.mapped_bytes()
    }
    /// Seal its full chunks into scratch files in `dir` (`RegionArena::spill_to`).
    pub(crate) fn spill_to(&mut self, dir: &std::path::Path) {
        self.arena.spill_to(dir)
    }
    /// Its arena, the index dropped.
    pub(crate) fn into_arena(self) -> RegionArena<C> {
        self.arena
    }
}

/// A set's hash (`Interner`'s index).
pub(crate) fn region_hash<C: Circle>(r: RegionRef<C>) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hs = rustc_hash::FxHasher::default();
    r.hash(&mut hs);
    hs.finish()
}

/// The set whose record starts `w`.
fn record<C: Circle>(w: &[C::W]) -> RegionRef<'_, C> {
    let (ns, nx) = (w[0].get() as usize, w[1].get() as usize);
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
    pub(crate) fn spill_to(&mut self, dir: &std::path::Path) {
        self.spill = Some(dir.to_path_buf());
    }
    pub(crate) fn len(&self) -> usize {
        self.at.len()
    }
    /// Append `r`; its index.
    pub(crate) fn push(&mut self, r: RegionRef<C>) -> u32 {
        let i = u32::try_from(self.at.len()).expect("a region arena past u32 sets");
        let need = 2 + 3 * r.slabs.len() + 2 * r.xs.len();
        if self.open.len() + need > self.chunk {
            self.seal();
        }
        self.at.push((self.sealed.len() as u64) << 32 | self.open.len() as u64);
        self.open.extend([C::W::of(r.slabs.len() as u64), C::W::of(r.xs.len() as u64)]);
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
            None => crate::storage::scratch::Words::Mem(open),
            Some(dir) => {
                let path = dir.join(format!("sets-{:05}.bin", self.sealed.len()));
                self.mapped_words += open.len() as u64;
                crate::storage::scratch::Words::spill(&path, open).unwrap_or_else(|e| panic!("FATAL: the arc phase's set chunk {}: {e:#}", path.display()))
            }
        };
        self.sealed.push(w);
    }
    pub(crate) fn get(&self, i: u32) -> RegionRef<'_, C> {
        let a = self.at[i as usize];
        let c = (a >> 32) as usize;
        let w = if c == self.sealed.len() { &self.open[..] } else { &self.sealed[c][..] };
        record::<C>(&w[a as u32 as usize..])
    }
    /// The bytes its sets take (their records and places).
    pub(crate) fn bytes(&self) -> usize {
        (self.sealed_words as usize + self.open.len()) * std::mem::size_of::<C::W>() + self.at.len() * 8
    }
    /// The bytes of its sealed chunks mapped from scratch files.
    pub(crate) fn mapped_bytes(&self) -> usize {
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
        let mut next = move |n: u32| (crate::testutil::xorshift(&mut rng) % n as u64) as u32;
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
        let dir = crate::testutil::TestDir::new("arena-test");
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

    fn lcg(s: &mut u64) -> u32 {
        (crate::testutil::lcg(s) >> 33) as u32
    }

    /// The doubles' points are the remainders in value order, neighbours
    /// neighbours, the zeros one point.
    #[test]
    fn the_doubles_points_are_the_remainders_in_order() {
        assert_eq!(value(0), -0.5);
        assert_eq!(value(ZERO).to_bits(), 0);
        assert_eq!(value(POINTS - 1), 0.5 - f64::EPSILON / 4.0);
        assert_eq!((index(-0.0), index(0.0)), (Some(ZERO), Some(ZERO)));
        assert_eq!((index(0.5), index((-0.5f64).next_down())), (None, None));
        let mut s = 7u64;
        for _ in 0..10_000 {
            let (i, j) = ((crate::testutil::lcg(&mut s) >> 1) % POINTS, (crate::testutil::lcg(&mut s) >> 1) % POINTS);
            assert_eq!(index(value(i)), Some(i));
            assert_eq!(i < j, value(i) < value(j));
            if i + 1 < POINTS {
                assert_eq!(value(i + 1), value(i).next_up(), "{i}: neighbours are neighbours");
            }
        }
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
                    assert_eq!(r.as_ref().contains(x, y), inside(x, y), "({x}, {y})");
                }
            }
            // Canonical: the same set from the pieces in another order.
            let mut rev = orig.clone();
            rev.reverse();
            assert_eq!(Region::<Fixed16>::from_pieces(&mut rev, &mut Vec::new()), r);
            // pull: a point p is in the result iff the guards take it and its image is in r.
            // A translation keeping its guard inside the circle, or a constant.
            let transfer = |s: &mut u64| {
                let guard = seg(s);
                let room = (CIRCLE - guard.hi + guard.lo) as i64 + 1;
                let act = if lcg(s) % 4 == 0 { Act::Const(lcg(s) % CIRCLE) } else { Act::Mono((lcg(s) as i64 % room - guard.lo as i64) as i32) };
                Transfer { guard, act }
            };
            let (tx, ty) = (transfer(&mut s), transfer(&mut s));
            let mut out = Vec::new();
            r.as_ref().pull(tx, ty, &mut out);
            let pr = Region::<Fixed16>::from_pieces(&mut out, &mut Vec::new());
            let mut pp: Vec<u32> = probes.iter().flat_map(|&v| {
                let back = |t: Transfer| match t.act {
                    Act::Mono(d) => (v as i64 - d as i64).clamp(0, CIRCLE as i64 - 1) as u32,
                    Act::Const(_) => v,
                };
                [v, back(tx), back(ty)]
            }).chain([tx.guard.lo, tx.guard.hi - 1, ty.guard.lo, ty.guard.hi - 1]).collect();
            pp.sort_unstable();
            pp.dedup();
            for &x in &pp {
                for &y in &pp {
                    let want = tx.takes(x) && ty.takes(y) && r.as_ref().contains(apply(tx, x), apply(ty, y));
                    assert_eq!(pr.as_ref().contains(x, y), want, "pull at ({x}, {y})");
                }
            }
            // push: a point q is in the image iff some point of r inside the
            // guards maps to it - checked from both sides: every probed point
            // of r maps into the image, and the image's points pull back into r.
            let mut out = Vec::new();
            r.as_ref().push(tx, ty, &mut out);
            let img = Region::<Fixed16>::from_pieces(&mut out, &mut Vec::new());
            for &x in &pp {
                for &y in &pp {
                    if tx.takes(x) && ty.takes(y) && r.as_ref().contains(x, y) {
                        assert!(img.as_ref().contains(apply(tx, x), apply(ty, y)), "push misses the image of ({x}, {y})");
                    }
                }
            }
            let mut back = Vec::new();
            img.as_ref().pull(tx, ty, &mut back);
            let mut inter = Vec::new();
            for p in back {
                r.as_ref().clip_into(p, &mut inter);
            }
            let back = Region::<Fixed16>::from_pieces(&mut inter, &mut Vec::new());
            let mut again = Vec::new();
            back.as_ref().push(tx, ty, &mut again);
            assert_eq!(Region::<Fixed16>::from_pieces(&mut again, &mut Vec::new()), img, "the image is exactly the image of its preimage in r");
            // clip_into is the intersection.
            let q = Piece { y: seg(&mut s), x: seg(&mut s) };
            let mut cl = Vec::new();
            r.as_ref().clip_into(q, &mut cl);
            let cr = Region::<Fixed16>::from_pieces(&mut cl, &mut Vec::new());
            for &x in &probes {
                for &y in &probes {
                    let inq = q.x.lo <= x && x < q.x.hi && q.y.lo <= y && y < q.y.hi;
                    assert_eq!(cr.as_ref().contains(x, y), inq && r.as_ref().contains(x, y), "clip at ({x}, {y})");
                }
            }
        }
    }

}
