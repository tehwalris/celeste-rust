//! THE TRANSFERS (plans/architecture.md, "Arcs"): per recorded edge of a
//! forward, what the frame did to the player's sub-pixel remainder on each
//! axis, as a GUARD (the piece of the circle the edge is taken from) and an
//! ACTION (a rotation, or a collision's constant).
//!
//! Always on: every traced frame that widens (`trace_frame`'s `widen`, every
//! kernel set) captures, per body, the transfer roots (`trace::verify::FrameOut::arc`);
//! the kernel computes them beside the row without storing them in it (no
//! key, column, dedupe or checkpoint sees them) and decodes each producer's
//! transfer here; the forward interns the (x, y) pair per worker
//! (`ForwardSink::xfer_id`) and the edge record carries its id. The
//! compaction turns the workers' ids into one table per frame
//! (`search::edges`, `xfer/f{frame}.bin`), so a run's records are keyed by
//! (target, base, transfer) and a mask is OR-ed only across equal transfers.
//!
//! ## What the kernel computes per axis (`RawAxis`)
//!
//! `move` does `rem := rem + ox + 1/2; rem := __split_by_flr(rem); amount :=
//! flr(rem); rem := rem - 1/2 - amount`, then steps (a blocked step sets `rem
//! := 0`). Captured at the PLAYER's own split (`Interp::split_site`):
//!
//! * `took` - this path took the player's split on this axis;
//! * `pre` - the split's argument `rem + ox + 1/2` (raw, an interval);
//! * `frag` - the fragment the body's fork configuration took (one floor);
//! * `ox` - the move's argument, the speed APPLIED (not always the stored one);
//! * `fin` - the player's remainder at the frame's end BEFORE the boundary
//!   widening (`None`: no player at the end).
//!
//! The "image" of plans/arcs.md - the remainder right after `rem -= 1/2 +
//! amount` - is `frag - 1/2 - flr(frag)`, derived here rather than in the
//! graph: one floor per fragment is checked, not assumed.
//!
//! ## The decoded transfer (`decode_axis`), every case
//!
//! Circle points are `rem + 1/2` in raw units (`arcs::point`).
//!
//! * No split (`took` false: a freeze, the spawn before the player exists):
//!   the guard is the whole circle, and
//!   - no player at the end, or `fin` the whole input circle `[-1/2, 1/2)`
//!     (the remainder untouched): `Rotate(0)`;
//!   - `fin` a point (a player created this frame, `rem = 0`): `Const(fin)`;
//!   - anything else: REFUSED.
//! * A split: `ox` must be a point and `pre` exactly `[ox, ox + 1)` - that
//!   is, the remainder at the move was the whole circle and the speed exact
//!   (anything else, e.g. a bucketed speed: REFUSED). `frag` must lie in
//!   `pre` within one floor `f` (else REFUSED). Then
//!   - guard: `[frag.lo - ox, frag.hi - ox + 1)` - the input remainders whose
//!     `rem + ox + 1/2` lands in the fragment. It must be the whole circle or
//!     touch one of its ends, with the image touching the OTHER end (the piece
//!     below the cut rotates onto an image ending at +1/2, the piece above
//!     onto one starting at -1/2); a piece touching neither end is REFUSED.
//!     A point guard (a fragment one raw unit wide) is a piece like any other.
//!   - rotation: `(ox - f) mod 1` - the image of `rem` is `rem + ox - f`.
//!   - action: `fin == image`: `Rotate(rotation)`; `fin` a point other than
//!     the image (a blocked step reset the remainder): `Const(fin)` (where the
//!     image is itself a point equal to `fin`, `Rotate` maps the one-point
//!     guard to the same point: the two agree); no player at the end (a
//!     death): `Rotate(rotation)`, the target's remainder being a dummy
//!     coordinate (a node without a player has no remainder; every
//!     transfer out of one has a whole-circle guard, so its winning set is
//!     the whole circle or nothing and any action into it is exact);
//!     `fin` anything else (an interval that is not the image): REFUSED.
//!
//! A refusal is FATAL in the forward (the kernel's append step): the record
//! would be a claim about the remainder that nothing checked.

use super::arcs::{self, Action, Seg, Set, Transfer, CIRCLE};

const HALF: i64 = 32768;
const ONE: i64 = 65536;

/// One axis of a frame's remainder transfer, as the kernel computed it on
/// one lane (raw 16.16, inclusive intervals). See the module doc.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RawAxis {
    pub took: bool,
    pub pre: (i32, i32),
    pub frag: (i32, i32),
    pub ox: (i32, i32),
    pub fin: Option<(i32, i32)>,
}

/// One axis's decoded transfer, compact (what a record stores): the guard
/// `[lo, hi)` of the circle and the action (`tag` 0: `Rotate(val)`, `val` in
/// `0..CIRCLE`; 1: `Const(val)`, a circle point).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AxisXfer {
    pub lo: u32,
    pub hi: u32,
    pub tag: u8,
    pub val: i32,
}

impl AxisXfer {
    pub fn guard(&self) -> Set {
        Set::seg(self.lo, self.hi)
    }
    pub fn action(&self) -> Action {
        match self.tag {
            0 => Action::Rotate(self.val),
            _ => Action::Const(self.val as u32),
        }
    }
    pub fn transfer(&self) -> Transfer {
        Transfer { guard: Seg { lo: self.lo, hi: self.hi }, action: self.action() }
    }
    fn rotate(lo: u32, hi: u32, v: i64) -> Self {
        AxisXfer { lo, hi, tag: 0, val: v.rem_euclid(ONE) as i32 }
    }
    fn konst(lo: u32, hi: u32, rem: i64) -> std::result::Result<Self, String> {
        if !(-HALF..HALF).contains(&rem) {
            return Err(format!("a constant remainder {rem} outside [-1/2, 1/2)"));
        }
        Ok(AxisXfer { lo, hi, tag: 1, val: arcs::point(rem as i32) as i32 })
    }
}

/// A transfer pair (x, y) as one edge record's id refers to it.
pub type Pair = (AxisXfer, AxisXfer);

/// Bytes per encoded pair: per axis `lo`, `hi` (u32), `tag` (u8), `val` (i32).
pub const PAIR_BYTES: usize = 2 * 13;

pub fn encode_pair(out: &mut Vec<u8>, p: &Pair) {
    for a in [p.0, p.1] {
        out.extend_from_slice(&a.lo.to_le_bytes());
        out.extend_from_slice(&a.hi.to_le_bytes());
        out.push(a.tag);
        out.extend_from_slice(&a.val.to_le_bytes());
    }
}

pub fn decode_pair(b: &[u8]) -> Pair {
    let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
    let axis = |o: usize| AxisXfer { lo: u32_at(o), hi: u32_at(o + 4), tag: b[o + 8], val: u32_at(o + 9) as i32 };
    (axis(0), axis(13))
}

/// Decode one axis (the module doc's rules). `Err` is a refusal.
pub fn decode_axis(r: &RawAxis) -> std::result::Result<AxisXfer, String> {
    let whole = (-HALF, HALF - 1);
    let wide = |v: (i32, i32)| (v.0 as i64, v.1 as i64);
    if !r.took {
        return match r.fin.map(wide) {
            None => Ok(AxisXfer::rotate(0, CIRCLE, 0)),
            Some(f) if f == whole => Ok(AxisXfer::rotate(0, CIRCLE, 0)),
            Some((a, b)) if a == b => AxisXfer::konst(0, CIRCLE, a),
            Some(f) => Err(format!("no player split, but the final remainder {f:?} is neither the input circle nor a point")),
        };
    }
    let (ox, oh) = wide(r.ox);
    if ox != oh {
        return Err(format!("the applied speed {:?} is not exact", r.ox));
    }
    let pre = wide(r.pre);
    if pre != (ox, ox + ONE - 1) {
        return Err(format!("the split's argument {pre:?} is not the whole circle shifted by the speed {ox} (the remainder at the move was not [-1/2, 1/2))"));
    }
    let (fl, fh) = wide(r.frag);
    if !(pre.0 <= fl && fl <= fh && fh <= pre.1) {
        return Err(format!("the fragment {:?} is not inside the split's argument {pre:?}", r.frag));
    }
    let f = fl.div_euclid(ONE);
    if fh.div_euclid(ONE) != f {
        return Err(format!("the fragment {:?} spans two floors", r.frag));
    }
    let (glo, ghi) = (fl - ox, fh - ox + 1);
    let image = (fl - HALF - f * ONE, fh - HALF - f * ONE);
    let consistent = if (glo, ghi) == (0, ONE) {
        image == whole
    } else if glo == 0 {
        image.1 == HALF - 1
    } else if ghi == ONE {
        image.0 == -HALF
    } else {
        return Err(format!("the piece [{glo}, {ghi}) touches neither end of the circle"));
    };
    if !consistent {
        return Err(format!("the piece [{glo}, {ghi}) and its image {image:?} do not touch opposite ends of the circle"));
    }
    let rot = ox - f * ONE;
    let (lo, hi) = (glo as u32, ghi as u32);
    match r.fin.map(wide) {
        None => Ok(AxisXfer::rotate(lo, hi, rot)),
        Some(fin) if fin == image => Ok(AxisXfer::rotate(lo, hi, rot)),
        Some((a, b)) if a == b => AxisXfer::konst(lo, hi, a),
        Some(fin) => Err(format!("the final remainder {fin:?} is neither the image {image:?} nor a point")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn split(ox: i32, frag: (i32, i32), fin: Option<(i32, i32)>) -> RawAxis {
        RawAxis { took: true, pre: (ox, ox + 65535), frag, ox: (ox, ox), fin }
    }

    /// The decode against the model itself: for every input remainder in a
    /// piece, `rem + ox + 1/2` lands in the fragment, and the action takes
    /// its point to `rem - 1/2 - amount + ox + 1/2`'s point.
    #[test]
    fn a_split_decodes_to_the_piece_and_the_rotation() {
        for ox in [0i32, 1, 39322, 65535, 65536, -39322, -65536 * 3 + 7, 32768] {
            let (lo, hi) = (ox, ox + 65535);
            let cut = (lo.div_euclid(65536) + 1) * 65536;
            let mut frags = vec![(lo, hi.min(cut - 1))];
            if hi >= cut {
                frags.push((cut, hi));
            }
            let mut covered = 0u32;
            for frag in frags {
                let f = frag.0.div_euclid(65536);
                let image = (frag.0 - 32768 - f * 65536, frag.1 - 32768 - f * 65536);
                let x = decode_axis(&split(ox, frag, Some(image))).unwrap();
                covered += x.hi - x.lo;
                for p in [x.lo, x.hi - 1, (x.lo + x.hi) / 2] {
                    let rem = p as i32 - 32768;
                    let v = rem + ox + 32768;
                    assert!(frag.0 <= v && v <= frag.1, "ox {ox}: rem {rem} is in the guard but not the fragment");
                    let amount = v.div_euclid(65536);
                    let out = v - 32768 - amount * 65536;
                    assert_eq!(x.action().image(&Set::point(p)), Set::point(arcs::point(out)), "ox {ox} rem {rem}");
                }
                // A collision instead: the constant.
                let c = decode_axis(&split(ox, frag, Some((0, 0)))).unwrap();
                if image != (0, 0) {
                    assert_eq!(c.action(), Action::Const(arcs::point(0)));
                }
                assert_eq!((c.lo, c.hi), (x.lo, x.hi));
            }
            assert_eq!(covered, CIRCLE, "ox {ox}: the pieces partition the circle");
        }
    }

    #[test]
    fn a_point_piece_is_a_piece() {
        // ox = 1 raw past a whole pixel: the upper piece is the one point rem = 1/2 - 1 raw.
        let ox = 65536 + 1;
        let x = decode_axis(&split(ox, (131072, 131072), Some((-32768, -32768)))).unwrap();
        assert_eq!((x.lo, x.hi), (65535, 65536));
        assert_eq!(x.action().image(&x.guard()), Set::point(0));
    }

    #[test]
    fn no_split_is_identity_or_a_new_player() {
        let none = RawAxis { took: false, pre: (0, 0), frag: (0, 0), ox: (0, 0), fin: Some((-32768, 32767)) };
        assert_eq!(decode_axis(&none).unwrap().transfer(), Transfer { guard: Seg { lo: 0, hi: CIRCLE }, action: Action::Rotate(0) });
        let born = RawAxis { fin: Some((0, 0)), ..none };
        assert_eq!(decode_axis(&born).unwrap().action(), Action::Const(32768));
        let gone = RawAxis { fin: None, ..none };
        assert_eq!(decode_axis(&gone).unwrap().guard(), Set::full());
        assert!(decode_axis(&RawAxis { fin: Some((0, 5)), ..none }).is_err());
    }

    #[test]
    fn what_the_model_does_not_cover_is_refused() {
        // An inexact speed.
        assert!(decode_axis(&RawAxis { took: true, pre: (0, 65535 + 10), frag: (0, 65535), ox: (0, 10), fin: None }).is_err());
        // A remainder at the move narrower than the circle.
        assert!(decode_axis(&RawAxis { took: true, pre: (0, 100), frag: (0, 100), ox: (0, 0), fin: None }).is_err());
        // A fragment over two floors.
        assert!(decode_axis(&split(100, (100, 65536 + 50), None)).is_err());
        // A final remainder that is neither the image nor a point.
        assert!(decode_axis(&split(0, (0, 65535), Some((0, 7)))).is_err());
        // A piece touching neither end: a fragment strictly inside its floor.
        assert!(decode_axis(&split(100, (200, 300), None)).is_err());
    }

    #[test]
    fn pairs_round_trip() {
        let a = AxisXfer { lo: 3, hi: 9, tag: 0, val: 12 };
        let b = AxisXfer { lo: 0, hi: CIRCLE, tag: 1, val: 32768 };
        let mut buf = Vec::new();
        encode_pair(&mut buf, &(a, b));
        encode_pair(&mut buf, &(b, a));
        assert_eq!(buf.len(), 2 * PAIR_BYTES);
        assert_eq!(decode_pair(&buf[..PAIR_BYTES]), (a, b));
        assert_eq!(decode_pair(&buf[PAIR_BYTES..]), (b, a));
    }
}
