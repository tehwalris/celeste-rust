//! THE REMAINDER MODEL of an arithmetic (plans/float-solver.md, section 6):
//! what the hull forward keeps of a state's sub-pixel remainder (its HULL,
//! accumulated order-free in an `Acc`), and what a recorded edge does to it
//! (its transfer PAIR, one per axis, as the tree's table stores it, and as
//! the pushes read it: an `Xfer`). The forward and the storage are generic
//! over it, so a tree's hulls and transfers are its arithmetic's.
//!
//! 16.16's model is the circle of 65536 points (`hull`, `arcs`, `arc_edges`).

use std::fmt::Debug;
use std::hash::Hash;

use anyhow::Result;
use celeste_core::arith::EngineNum;
use celeste_core::pico8_num::Pico8Num as P8;

use super::arc_edges::{self, Pair};
use super::arcs::Transfer;
use super::hull::{Hull, HullAcc};

pub trait Rem: EngineNum {
    /// A state's hull: the box of the remainders it has been reached with.
    type Hull: Copy + Debug + PartialEq + Eq + Hash + PartialOrd + Ord + Default + Send + Sync + 'static;
    /// A hull being accumulated: merged by an order-free rule (region jobs
    /// run in any order), `Default` the empty one.
    type Acc: Copy + Debug + PartialEq + Eq + Default + Send + Sync + 'static;
    /// An edge's transfer pair as the tree's table records it.
    type Pair: Copy + Debug + PartialEq + Eq + Hash + PartialOrd + Ord + Send + Sync + 'static;
    /// A pair as the pushes read it.
    type Xfer: Copy + Debug + Send + Sync + 'static;
    /// Bytes a hull takes on disk.
    const HULL_BYTES: usize;
    /// Bytes a pair takes on disk.
    const PAIR_BYTES: usize;

    /// The start's hull: its exact remainder (`arc_dp::start_point`).
    fn start_hull(s: &crate::settings::Settings) -> Result<Self::Hull>;
    /// Every remainder on both axes.
    fn full() -> Self::Hull;
    fn is_full(h: &Self::Hull) -> bool;
    fn xfer(p: &Self::Pair) -> Self::Xfer;
    /// THE PUSH of hull `h` along an edge of transfer `t` into `acc`: per
    /// axis its points inside the guard, moved by the action; nothing where
    /// either axis misses. Whether it pushed.
    fn push(h: &Self::Hull, t: &Self::Xfer, acc: &mut Self::Acc) -> bool;
    /// The push of a GROWN hull `h` (`was` the hull it last pushed), only
    /// where the edge's image grows past `was`'s.
    fn push_grown(h: &Self::Hull, was: &Self::Hull, t: &Self::Xfer, acc: &mut Self::Acc) -> bool;
    fn acc_add_hull(acc: &mut Self::Acc, h: &Self::Hull);
    fn acc_merge(acc: &mut Self::Acc, o: &Self::Acc);
    /// The accumulated hull; `None` when nothing was added.
    fn acc_hull(acc: &Self::Acc) -> Option<Self::Hull>;
    fn acc_is_empty(acc: &Self::Acc) -> bool;
    /// Append `h`'s `HULL_BYTES` bytes.
    fn hull_to_bytes(h: &Self::Hull, out: &mut Vec<u8>);
    fn hull_from_bytes(b: &[u8]) -> Self::Hull;
    /// A hash of a hull (a cache's slot, the hull statistics).
    fn hull_hash(h: &Self::Hull) -> u64;
    /// Append `p`'s `PAIR_BYTES` bytes.
    fn encode_pair(out: &mut Vec<u8>, p: &Self::Pair);
    fn decode_pair(b: &[u8]) -> Self::Pair;
    /// A hash of a pair (the transfer table's index).
    fn pair_hash(p: &Self::Pair) -> u64;
}

impl Rem for P8 {
    type Hull = Hull;
    type Acc = HullAcc;
    type Pair = Pair;
    type Xfer = (Transfer, Transfer);
    const HULL_BYTES: usize = super::hull::HULL_BYTES;
    const PAIR_BYTES: usize = arc_edges::PAIR_BYTES;

    fn start_hull(s: &crate::settings::Settings) -> Result<Hull> {
        #[cfg(test)]
        if super::hull::WIDEN.load(std::sync::atomic::Ordering::Relaxed) {
            return Ok(Hull::full());
        }
        let (x, y) = super::arc_dp::start_point(s)?;
        Ok(Hull::point(x, y))
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
