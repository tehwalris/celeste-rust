//! THE REMAINDER MODEL of an arithmetic (plans/float.md, "The remainder model and the round fork"):
//! the CIRCLE its remainders are points of (`Rem::C`; a state's HULL is
//! `hull::Hull` over it, accumulated order-free in a `HullAcc`), and what a
//! recorded edge does to a remainder (its transfer PAIR, one per axis, as
//! the tree's table stores it, and as the pushes read it: an `Xfer`). The
//! forward and the storage are generic over it, so a tree's hulls and
//! transfers are its arithmetic's.
//!
//! 16.16's model is the circle of 65536 points (`arcs::Fixed16`,
//! `arc_edges`); the doubles' the circle of the doubles of `[-1/2, 1/2)`
//! (`rem_f64`).

use std::fmt::Debug;
use std::hash::Hash;

use anyhow::Result;
use celeste_core::arith::{Arithmetic, EngineNum};
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

use super::arc_edges::{self, Pair};
use super::arcs::{Circle, Fixed16, Seg, Transfer};
use super::hull::{Hull, HullOf};
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
    /// An edge's transfer pair as the tree's table records it.
    type Pair: Copy + Debug + PartialEq + Eq + Hash + PartialOrd + Ord + Send + Sync + 'static;
    /// The circle of points the remainders are (`arcs::Circle`): the
    /// hulls' (`hull::Hull<C>`) and the arcs' sets, and a pair as the
    /// pushes and pulls read it (`Xfer`).
    type C: Circle;
    /// Bytes a pair takes on disk.
    const PAIR_BYTES: usize;

    /// THE DECODE of a frame's captures (x, y) into its transfer pair;
    /// `Err` is a refusal (fatal in the forward).
    fn transfer(x: &Capture<Self>, y: &Capture<Self>) -> Result<Self::Pair, String>;
    /// A pair as the pushes and pulls read it.
    fn xfer(p: &Self::Pair) -> Xfer<Self>;
    /// A remainder as a point of the circle (`None`: not in `[-1/2, 1/2)`).
    fn point(v: Self) -> Option<Point<Self>>;
    /// The remainder at point `p` (`point`'s inverse).
    fn at_point(p: Point<Self>) -> Self;
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
pub fn start_point<N: Rem>() -> (Point<N>, Point<N>) {
    let p = N::point(<N::A as Arithmetic>::from_i16(0)).expect("0 is a remainder");
    (p, p)
}

/// The start's hull: its exact remainder (`start_point`).
pub fn start_hull<N: Rem>() -> HullOf<N> {
    #[cfg(test)]
    if super::hull::WIDEN.load(std::sync::atomic::Ordering::Relaxed) {
        return Hull::full();
    }
    let (x, y) = start_point::<N>();
    Hull::point(x, y)
}

/// Does hull `h` hold the remainder `(x, y)`?
pub fn holds<N: Rem>(h: &HullOf<N>, x: N, y: N) -> bool {
    N::point(x).zip(N::point(y)).is_some_and(|(x, y)| h.contains(x, y))
}

/// Does an edge of transfer `p` take the remainder `from` and end at `to`
/// (`None`: no player at the frame's end; any action)?
pub fn maps<N: Rem>(p: &N::Pair, from: (N, N), to: Option<(N, N)>) -> bool {
    let axis = |a: &<N::C as Circle>::Act, f: N, t: Option<N>| {
        let Some(f) = N::point(f) else { return false };
        let g = N::C::guard(a);
        if !(g.lo <= f && f < g.hi) {
            return false;
        }
        let Some(t) = t else { return true };
        let mut image = None;
        N::C::image(a, Seg { lo: f, hi: N::C::next(f) }, |s| image = Some(s));
        N::point(t).is_some_and(|t| image == Some(Seg { lo: t, hi: N::C::next(t) }))
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
    fn xfer(p: &Pair) -> (Transfer, Transfer) {
        (p.0.transfer(), p.1.transfer())
    }
    fn point(v: P8) -> Option<u32> {
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
    fn xfer(p: &rem_f64::Pair) -> rem_f64::Pair {
        *p
    }
    fn point(v: LuaF64) -> Option<u64> {
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
