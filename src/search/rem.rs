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
    /// The circle the remainders are points of (hulls, the arcs' sets).
    type C: Circle;
    /// Bytes a pair takes on disk.
    const PAIR_BYTES: usize;

    /// THE DECODE of a frame's captures (x, y); `Err` is a refusal (fatal in
    /// the forward).
    fn transfer(x: &Capture<Self>, y: &Capture<Self>) -> Result<Self::Pair, String>;
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
    type C = Fixed16;
    const PAIR_BYTES: usize = arc_edges::PAIR_BYTES;

    fn transfer(x: &Capture<P8>, y: &Capture<P8>) -> Result<Pair, String> {
        let raw = |v: (P8, P8)| (v.0.as_raw_u32() as i32, v.1.as_raw_u32() as i32);
        let axis = |c: &Capture<P8>, name: &str| {
            let r = arc_edges::RawAxis { took: c.took, pre: raw(c.pre), frag: raw(c.frag), ox: raw(c.ox), fin: c.fin.map(raw) };
            arc_edges::decode_axis(&r).map_err(|e| format!("axis {name} ({r:?}): {e}"))
        };
        Ok((axis(x, "x")?, axis(y, "y")?))
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
    type C = rem_f64::Doubles;
    const PAIR_BYTES: usize = rem_f64::PAIR_BYTES;

    fn transfer(x: &Capture<LuaF64>, y: &Capture<LuaF64>) -> Result<rem_f64::Pair, String> {
        let axis = |c: &Capture<LuaF64>, name: &str| rem_f64::decode_axis(c).map_err(|e| format!("axis {name} ({c:?}): {e}"));
        Ok((axis(x, "x")?, axis(y, "y")?))
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
