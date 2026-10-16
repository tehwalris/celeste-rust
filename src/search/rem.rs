//! THE REMAINDER MODEL of an arithmetic (plans/float.md, "The remainder model and the round fork"):
//! what the hull forward keeps of a state's sub-pixel remainder (its HULL,
//! accumulated order-free in an `Acc`), and what a recorded edge does to it
//! (its transfer PAIR, one per axis, as the tree's table stores it, and as
//! the pushes read it: an `Xfer`). The forward and the storage are generic
//! over it, so a tree's hulls and transfers are its arithmetic's.
//!
//! 16.16's model is the circle of 65536 points (`hull`, `arcs`, `arc_edges`);
//! the doubles' the circle of the doubles of `[-1/2, 1/2)` (`rem_f64`).

use std::fmt::Debug;
use std::hash::Hash;

use anyhow::Result;
use celeste_core::arith::{Arithmetic, EngineNum};
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

use super::arc_edges::{self, Pair};
use super::arcs::{Circle, Fixed16, Set, Transfer};
use super::hull::{Hull, HullAcc};
use super::rem_f64;

/// One axis of what a frame did to the player's remainder, as the
/// reference engine's arc capture holds it (`trace::state::ArcAxis`, read
/// off a leaf), in the arithmetic's numbers: whether this path took the
/// player's split (`took`), the split's argument `pre` and the fragment
/// `frag` it took (16.16: the floor split of `rem + ox + 1/2`; doubles:
/// the round split of `rem + ox`), the move's speed `ox`, and the player's
/// remainder at the frame's end `fin` (`None`: no player). The kernels'
/// `arc_edges::RawAxis` is 16.16's raw form of it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Capture<N> {
    pub took: bool,
    pub pre: (N, N),
    pub frag: (N, N),
    pub ox: (N, N),
    pub fin: Option<(N, N)>,
}

pub trait Rem: EngineNum {
    /// A state's hull: the box of the remainders it has been reached with.
    type Hull: Copy + Debug + PartialEq + Eq + Hash + PartialOrd + Ord + Default + Send + Sync + 'static;
    /// A hull as a cache compares it (`hull::PushCache`): 16.16's packs into
    /// one word.
    type HullKey: Copy + Debug + PartialEq + Eq + Default + Send + Sync + 'static;
    /// A hull being accumulated: merged by an order-free rule (region jobs
    /// run in any order), `Default` the empty one.
    type Acc: Copy + Debug + PartialEq + Eq + Default + Send + Sync + 'static;
    /// An edge's transfer pair as the tree's table records it.
    type Pair: Copy + Debug + PartialEq + Eq + Hash + PartialOrd + Ord + Send + Sync + 'static;
    /// The circle of points the remainders are (`arcs::Circle`): the arcs'
    /// sets, and a pair as the pushes and pulls read it (`Xfer`).
    type C: Circle;
    /// Bytes a hull takes on disk.
    const HULL_BYTES: usize;
    /// Bytes a pair takes on disk.
    const PAIR_BYTES: usize;

    /// The start's hull: its exact remainder (`start_point`).
    fn start_hull(s: &crate::settings::Settings) -> Result<Self::Hull>;
    /// THE DECODE of a frame's captures (x, y) into its transfer pair;
    /// `Err` is a refusal (fatal in the forward).
    fn transfer(x: &Capture<Self>, y: &Capture<Self>) -> Result<Self::Pair, String>;
    /// Does hull `h` hold the remainder `(x, y)`?
    fn hull_holds(h: &Self::Hull, x: Self, y: Self) -> bool;
    /// Does an edge of transfer `p` take the remainder `from` and end at
    /// `to` (`None`: no player at the frame's end; any action)?
    fn maps(p: &Self::Pair, from: (Self, Self), to: Option<(Self, Self)>) -> bool;
    /// Every remainder on both axes.
    fn full() -> Self::Hull;
    fn is_full(h: &Self::Hull) -> bool;
    fn xfer(p: &Self::Pair) -> Xfer<Self>;
    /// A remainder as a point of the circle (`None`: not in `[-1/2, 1/2)`).
    fn point(v: Self) -> Option<Point<Self>>;
    /// The remainder at point `p` (`point`'s inverse).
    fn at_point(p: Point<Self>) -> Self;
    /// THE PUSH of hull `h` along an edge of transfer `t` into `acc`: per
    /// axis its points inside the guard, moved by the action; nothing where
    /// either axis misses. Whether it pushed.
    fn push(h: &Self::Hull, t: &Xfer<Self>, acc: &mut Self::Acc) -> bool;
    /// The push of a GROWN hull `h` (`was` the hull it last pushed), only
    /// where the edge's image grows past `was`'s.
    fn push_grown(h: &Self::Hull, was: &Self::Hull, t: &Xfer<Self>, acc: &mut Self::Acc) -> bool;
    fn acc_add_hull(acc: &mut Self::Acc, h: &Self::Hull);
    fn acc_merge(acc: &mut Self::Acc, o: &Self::Acc);
    /// The accumulated hull; `None` when nothing was added.
    fn acc_hull(acc: &Self::Acc) -> Option<Self::Hull>;
    fn acc_is_empty(acc: &Self::Acc) -> bool;
    /// Append `h`'s `HULL_BYTES` bytes.
    fn hull_to_bytes(h: &Self::Hull, out: &mut Vec<u8>);
    fn hull_from_bytes(b: &[u8]) -> Self::Hull;
    /// A hash of a hull (the hull statistics).
    fn hull_hash(h: &Self::Hull) -> u64;
    /// `h`'s cache key, and the key's hash (a cache's slot).
    fn hull_key(h: &Self::Hull) -> Self::HullKey;
    fn key_hash(k: &Self::HullKey) -> u64;
    /// Append `p`'s `PAIR_BYTES` bytes.
    fn encode_pair(out: &mut Vec<u8>, p: &Self::Pair);
    fn decode_pair(b: &[u8]) -> Self::Pair;
    /// A hash of a pair (the transfer table's index).
    fn pair_hash(p: &Self::Pair) -> u64;
}

/// A pair as the pushes and pulls read it: per axis the circle's transfer.
pub type Xfer<N> = (<<N as Rem>::C as Circle>::Act, <<N as Rem>::C as Circle>::Act);

/// A point of `N`'s circle.
pub type Point<N> = <<N as Rem>::C as Circle>::P;

/// The start state's remainder as a point of the torus: the room's load has
/// no player yet, so any point stands for it - the point of 0.
pub fn start_point<N: Rem>(_s: &crate::settings::Settings) -> anyhow::Result<(Point<N>, Point<N>)> {
    let p = zero_point::<N>();
    Ok((p, p))
}

/// The point of the remainder 0 (a state with no player stands there).
fn zero_point<N: Rem>() -> Point<N> {
    N::point(<N::A as Arithmetic>::from_i16(0)).expect("0 is a remainder")
}

/// A 16.16 remainder's point of the circle.
fn point(v: P8) -> u32 {
    super::arcs::point(v.as_raw_u32() as i32)
}

impl Rem for P8 {
    type Hull = Hull;
    type HullKey = u64;
    type Acc = HullAcc;
    type Pair = Pair;
    type C = Fixed16;
    const HULL_BYTES: usize = super::hull::HULL_BYTES;
    const PAIR_BYTES: usize = arc_edges::PAIR_BYTES;

    fn start_hull(s: &crate::settings::Settings) -> Result<Hull> {
        #[cfg(test)]
        if super::hull::WIDEN.load(std::sync::atomic::Ordering::Relaxed) {
            return Ok(Hull::full());
        }
        let (x, y) = start_point::<P8>(s)?;
        Ok(Hull::point(x, y))
    }
    fn transfer(x: &Capture<P8>, y: &Capture<P8>) -> Result<Pair, String> {
        let raw = |v: (P8, P8)| (v.0.as_raw_u32() as i32, v.1.as_raw_u32() as i32);
        let axis = |c: &Capture<P8>, name: &str| {
            let r = arc_edges::RawAxis { took: c.took, pre: raw(c.pre), frag: raw(c.frag), ox: raw(c.ox), fin: c.fin.map(raw) };
            arc_edges::decode_axis(&r).map_err(|e| format!("axis {name} ({r:?}): {e}"))
        };
        Ok((axis(x, "x")?, axis(y, "y")?))
    }
    fn hull_holds(h: &Hull, x: P8, y: P8) -> bool {
        h.contains(point(x), point(y))
    }
    fn maps(p: &Pair, from: (P8, P8), to: Option<(P8, P8)>) -> bool {
        let axis = |a: &arc_edges::AxisXfer, f: P8, t: Option<P8>| {
            let tr = a.transfer();
            tr.takes(point(f)) && t.is_none_or(|t| tr.action.image(&Set::point(point(f))) == Set::point(point(t)))
        };
        axis(&p.0, from.0, to.map(|t| t.0)) && axis(&p.1, from.1, to.map(|t| t.1))
    }
    fn full() -> Hull {
        Hull::full()
    }
    fn is_full(h: &Hull) -> bool {
        h.x.is_full() && h.y.is_full()
    }
    fn xfer(p: &Pair) -> (Transfer, Transfer) {
        (p.0.transfer(), p.1.transfer())
    }
    fn point(v: P8) -> Option<u32> {
        let r = v.as_raw_u32() as i32;
        (-32768..32768).contains(&r).then(|| point(v))
    }
    fn at_point(p: u32) -> P8 {
        P8::from_raw(p as i32 - 32768)
    }
    #[inline]
    fn push(h: &Hull, t: &(Transfer, Transfer), acc: &mut HullAcc) -> bool {
        h.push(t.0, t.1, acc)
    }
    #[inline]
    fn push_grown(h: &Hull, was: &Hull, t: &(Transfer, Transfer), acc: &mut HullAcc) -> bool {
        h.push_grown(was, t.0, t.1, acc)
    }
    #[inline]
    fn acc_add_hull(acc: &mut HullAcc, h: &Hull) {
        acc.add_hull(h)
    }
    #[inline]
    fn acc_merge(acc: &mut HullAcc, o: &HullAcc) {
        acc.merge(o)
    }
    fn acc_hull(acc: &HullAcc) -> Option<Hull> {
        acc.hull()
    }
    #[inline]
    fn acc_is_empty(acc: &HullAcc) -> bool {
        acc.is_empty()
    }
    fn hull_to_bytes(h: &Hull, out: &mut Vec<u8>) {
        out.extend_from_slice(&h.to_bytes());
    }
    fn hull_from_bytes(b: &[u8]) -> Hull {
        Hull::from_bytes(b)
    }
    #[inline]
    fn hull_hash(h: &Hull) -> u64 {
        h.word()
    }
    /// The hull's four `u16`s in one word: exact.
    #[inline]
    fn hull_key(h: &Hull) -> u64 {
        h.word()
    }
    #[inline]
    fn key_hash(k: &u64) -> u64 {
        *k
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
    type Hull = rem_f64::Hull;
    type HullKey = rem_f64::Hull;
    type Acc = rem_f64::HullAcc;
    type Pair = rem_f64::Pair;
    type C = rem_f64::Doubles;
    const HULL_BYTES: usize = rem_f64::HULL_BYTES;
    const PAIR_BYTES: usize = rem_f64::PAIR_BYTES;

    /// The room's load has no player yet: its remainder is 0 when it spawns.
    fn start_hull(_s: &crate::settings::Settings) -> Result<rem_f64::Hull> {
        Ok(rem_f64::Hull::point(rem_f64::ZERO, rem_f64::ZERO))
    }
    fn transfer(x: &Capture<LuaF64>, y: &Capture<LuaF64>) -> Result<rem_f64::Pair, String> {
        let axis = |c: &Capture<LuaF64>, name: &str| rem_f64::decode_axis(c).map_err(|e| format!("axis {name} ({c:?}): {e}"));
        Ok((axis(x, "x")?, axis(y, "y")?))
    }
    fn hull_holds(h: &rem_f64::Hull, x: LuaF64, y: LuaF64) -> bool {
        let at = |a: rem_f64::Arc, v: LuaF64| rem_f64::index(v.value()).is_some_and(|i| a.contains(i));
        at(h.x, x) && at(h.y, y)
    }
    fn maps(p: &rem_f64::Pair, from: (LuaF64, LuaF64), to: Option<(LuaF64, LuaF64)>) -> bool {
        let axis = |a: &rem_f64::Axis, f: LuaF64, t: Option<LuaF64>| match rem_f64::index(f.value()) {
            Some(i) if a.lo <= i && i < a.hi => t.is_none_or(|t| rem_f64::index(t.value()) == Some(a.apply(i))),
            _ => false,
        };
        axis(&p.0, from.0, to.map(|t| t.0)) && axis(&p.1, from.1, to.map(|t| t.1))
    }
    fn full() -> rem_f64::Hull {
        rem_f64::Hull::full()
    }
    fn is_full(h: &rem_f64::Hull) -> bool {
        h.x.is_full() && h.y.is_full()
    }
    fn xfer(p: &rem_f64::Pair) -> rem_f64::Pair {
        *p
    }
    fn point(v: LuaF64) -> Option<u64> {
        rem_f64::index(v.value())
    }
    fn at_point(p: u64) -> LuaF64 {
        LuaF64::new(rem_f64::value(p)).expect("a remainder is finite")
    }
    #[inline]
    fn push(h: &rem_f64::Hull, t: &rem_f64::Pair, acc: &mut rem_f64::HullAcc) -> bool {
        h.push(t, acc)
    }
    #[inline]
    fn push_grown(h: &rem_f64::Hull, was: &rem_f64::Hull, t: &rem_f64::Pair, acc: &mut rem_f64::HullAcc) -> bool {
        h.push_grown(was, t, acc)
    }
    #[inline]
    fn acc_add_hull(acc: &mut rem_f64::HullAcc, h: &rem_f64::Hull) {
        acc.add_hull(h)
    }
    #[inline]
    fn acc_merge(acc: &mut rem_f64::HullAcc, o: &rem_f64::HullAcc) {
        acc.merge(o)
    }
    fn acc_hull(acc: &rem_f64::HullAcc) -> Option<rem_f64::Hull> {
        acc.hull()
    }
    #[inline]
    fn acc_is_empty(acc: &rem_f64::HullAcc) -> bool {
        acc.is_empty()
    }
    fn hull_to_bytes(h: &rem_f64::Hull, out: &mut Vec<u8>) {
        h.to_bytes(out)
    }
    fn hull_from_bytes(b: &[u8]) -> rem_f64::Hull {
        rem_f64::Hull::from_bytes(b)
    }
    #[inline]
    fn hull_hash(h: &rem_f64::Hull) -> u64 {
        h.mixed()
    }
    #[inline]
    fn hull_key(h: &rem_f64::Hull) -> rem_f64::Hull {
        *h
    }
    #[inline]
    fn key_hash(k: &rem_f64::Hull) -> u64 {
        k.mixed()
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
