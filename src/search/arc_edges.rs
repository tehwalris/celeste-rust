//! THE TRANSFERS (plans/architecture.md "Arcs: the rotation graph"): per
//! recorded edge and axis, what the frame did to the player's remainder, as
//! a GUARD (the piece of the circle the edge is taken from) and an ACTION (a
//! rotation, or a collision's constant). The transfer roots
//! (`trace::frame::FrameOut::arc`) are computed beside the row (no key,
//! column, dedup or checkpoint sees them), decoded here, and interned into
//! the tree's one content-canonical table (`storage::edges::XferTable`).
//!
//! `move` does `rem := rem + ox + 1/2; rem := __split_by_flr(rem); amount :=
//! flr(rem); rem := rem - 1/2 - amount`, then steps (a blocked step sets
//! `rem := 0`). The image is `frag - 1/2 - flr(frag)`, one floor per
//! fragment checked, not assumed. Circle points are `rem + 1/2` (raw). The
//! decode (`decode_axis`) of a capture (`rem::Capture`; raw: `RawAxis`):
//!
//! * No split (a freeze, the spawn): the whole circle; `Rotate(0)` with no
//!   player at the end or `fin` the whole input circle, `Const(fin)` with
//!   `fin` a point (a player created this frame).
//! * A split of ONE point: the remainder was SET before the move
//!   (`set_before_the_move`): the whole circle, `Const(fin)` (no player at
//!   the end: `Rotate(0)`).
//! * A split: `ox` a point, `pre` exactly `[ox, ox + 1)` (the remainder at
//!   the move the whole circle), `frag` inside `pre` within one floor `f`.
//!   The guard `[frag.lo - ox, frag.hi - ox + 1)` is the whole circle, or a
//!   piece touching one end with its image touching the OTHER (below the
//!   cut onto an image ending at +1/2, above onto one starting at -1/2;
//!   one touching neither is refused); a point guard is a piece like any
//!   other. The action: `Rotate((ox - f) mod 1)` when `fin` is the image (a
//!   point image: the two agree);
//!   `Const(fin)` when `fin` is another point (a blocked step reset it);
//!   with no player at the end (a death) `Rotate` into a dummy coordinate:
//!   every transfer out of a player-less node has a whole-circle guard, so
//!   its winning set is all or nothing and any action into it is exact.
//!
//! Anything else is REFUSED, FATAL in the forward: the record would be an
//! unchecked claim about the remainder.

use super::arcs::{self, Act, Seg, Transfer, CIRCLE};

const HALF: i64 = 32768;
const ONE: i64 = 65536;

/// A `rem::Capture` in raw 16.16 (inclusive intervals).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct RawAxis {
    pub(crate) took: bool,
    pub(crate) pre: (i32, i32),
    pub(crate) frag: (i32, i32),
    pub(crate) ox: (i32, i32),
    pub(crate) fin: Option<(i32, i32)>,
}

/// One axis's decoded transfer as a record stores it: the guard `[lo, hi)`
/// and the action (`tag` 0: `Rotate(val)`, `val` in `0..CIRCLE`; 1: `Const`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AxisXfer {
    pub(crate) lo: u32,
    pub(crate) hi: u32,
    pub(crate) tag: u8,
    pub(crate) val: i32,
}

impl AxisXfer {
    /// As the pushes and pulls read it: a ROTATION by `val` as the
    /// translation it is on the guard (the guard is one floor's: its image
    /// never wraps - checked, FATAL otherwise), or the constant point.
    pub(crate) fn transfer(&self) -> Transfer {
        let guard = Seg { lo: self.lo, hi: self.hi };
        let act = match self.tag {
            0 => {
                let d = if self.lo as i64 + self.val as i64 >= CIRCLE as i64 { self.val - CIRCLE as i32 } else { self.val };
                assert!(self.lo as i64 + d as i64 >= 0 && self.hi as i64 + d as i64 <= CIRCLE as i64, "a transfer whose image wraps: {self:?}");
                Act::Mono(d)
            }
            _ => Act::Const(self.val as u32),
        };
        Transfer { guard, act }
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
pub(crate) type Pair = (AxisXfer, AxisXfer);

/// Bytes per encoded pair: per axis `lo`, `hi` (u32), `tag` (u8), `val` (i32).
pub(crate) const PAIR_BYTES: usize = 2 * 13;

pub(crate) fn encode_pair(out: &mut Vec<u8>, p: &Pair) {
    for a in [p.0, p.1] {
        out.extend_from_slice(&a.lo.to_le_bytes());
        out.extend_from_slice(&a.hi.to_le_bytes());
        out.push(a.tag);
        out.extend_from_slice(&a.val.to_le_bytes());
    }
}

pub(crate) fn decode_pair(b: &[u8]) -> Pair {
    let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
    let axis = |o: usize| AxisXfer { lo: u32_at(o), hi: u32_at(o + 4), tag: b[o + 8], val: u32_at(o + 9) as i32 };
    (axis(0), axis(13))
}

/// Decode one axis (the module doc's rules). `Err` is a refusal.
pub(crate) fn decode_axis(r: &RawAxis) -> std::result::Result<AxisXfer, String> {
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
    if pre.0 == pre.1 {
        return set_before_the_move(pre.0, ox, wide(r.frag), r.fin.map(wide));
    }
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

/// A split of ONE point: the remainder at the move was SET earlier in the
/// frame (a platform's carry blocked by a wall: `move_x`, stepping whole
/// pixels and never reading `rem`, sets `rem.x = 0`). Only `move` reads the
/// input remainder, so every one takes this edge, to the constant `fin`.
fn set_before_the_move(pre: i64, ox: i64, frag: (i64, i64), fin: Option<(i64, i64)>) -> std::result::Result<AxisXfer, String> {
    let rem = pre - ox - HALF;
    if !(-HALF..HALF).contains(&rem) {
        return Err(format!("the split's argument {pre} is the point {rem} + {ox} + 1/2, a remainder outside [-1/2, 1/2)"));
    }
    if frag != (pre, pre) {
        return Err(format!("the fragment {frag:?} is not the split's one-point argument {pre}"));
    }
    match fin {
        None => Ok(AxisXfer::rotate(0, CIRCLE, 0)),
        Some((a, b)) if a == b => AxisXfer::konst(0, CIRCLE, a),
        Some(f) => Err(format!("the remainder was set before the move, but the final remainder {f:?} is not a point")),
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
                    assert_eq!(x.transfer().image(Seg { lo: p, hi: p + 1 }), Some(Seg { lo: arcs::point(out), hi: arcs::point(out) + 1 }), "ox {ox} rem {rem}");
                }
                // A collision instead: the constant.
                let c = decode_axis(&split(ox, frag, Some((0, 0)))).unwrap();
                if image != (0, 0) {
                    assert_eq!(c.transfer().act, Act::Const(arcs::point(0)));
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
        assert_eq!(x.transfer().image(x.transfer().guard), Some(Seg { lo: 0, hi: 1 }));
    }

    /// A platform's carry blocked by a wall set `rem.x = 0` (and `spd.x =
    /// 0`) before the player's move: the split's argument is the point 1/2,
    /// and the edge maps EVERY input remainder to the final one.
    #[test]
    fn a_remainder_set_before_the_move_is_a_constant_from_the_whole_circle() {
        let set = RawAxis { took: true, pre: (32768, 32768), frag: (32768, 32768), ox: (0, 0), fin: Some((0, 0)) };
        let x = decode_axis(&set).unwrap();
        assert_eq!(x.transfer(), Transfer { guard: Seg { lo: 0, hi: CIRCLE }, act: Act::Const(arcs::point(0)) });
        // With a speed: the point is the set remainder plus the speed plus 1/2.
        let moved = RawAxis { took: true, pre: (32768 + 13107, 32768 + 13107), frag: (32768 + 13107, 32768 + 13107), ox: (13107, 13107), fin: Some((13107, 13107)) };
        assert_eq!(decode_axis(&moved).unwrap().transfer().act, Act::Const(arcs::point(13107)));
        // A death after it: the whole circle, into the dummy coordinate.
        assert_eq!(decode_axis(&RawAxis { fin: None, ..set }).unwrap().transfer().guard, Seg { lo: 0, hi: CIRCLE });
        // Not a remainder, a fragment off the point, a final remainder not a point.
        assert!(decode_axis(&RawAxis { pre: (100_000, 100_000), frag: (100_000, 100_000), ..set }).is_err());
        assert!(decode_axis(&RawAxis { frag: (0, 0), ..set }).is_err());
        assert!(decode_axis(&RawAxis { fin: Some((0, 9)), ..set }).is_err());
    }

    #[test]
    fn no_split_is_identity_or_a_new_player() {
        let none = RawAxis { took: false, pre: (0, 0), frag: (0, 0), ox: (0, 0), fin: Some((-32768, 32767)) };
        assert_eq!(decode_axis(&none).unwrap().transfer(), Transfer { guard: Seg { lo: 0, hi: CIRCLE }, act: Act::Mono(0) });
        let born = RawAxis { fin: Some((0, 0)), ..none };
        assert_eq!(decode_axis(&born).unwrap().transfer().act, Act::Const(32768));
        let gone = RawAxis { fin: None, ..none };
        assert_eq!(decode_axis(&gone).unwrap().transfer().guard, Seg { lo: 0, hi: CIRCLE });
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
