//! The byte codes the tree's formats share: LEB128 varints (7 bits a byte,
//! the low ones first), an exact key as one `u128` (its first word high, so
//! the order is the key's), and a sorted stream of states (`put_sk`).

use super::visited::Key;
use super::Sk;

macro_rules! varint {
    ($put:ident, $get:ident, $t:ty) => {
        #[inline]
        pub(crate) fn $put(out: &mut Vec<u8>, mut v: $t) {
            while v >= 0x80 {
                out.push((v as u8) | 0x80);
                v >>= 7;
            }
            out.push(v as u8);
        }

        /// The varint at `b[*pos..]`, `pos` moved past it.
        #[inline]
        pub(crate) fn $get(b: &[u8], pos: &mut usize) -> $t {
            let mut v: $t = 0;
            let mut shift = 0;
            loop {
                let x = b[*pos];
                *pos += 1;
                v |= ((x & 0x7f) as $t) << shift;
                if x & 0x80 == 0 {
                    return v;
                }
                shift += 7;
            }
        }
    };
}

varint!(put_varint, get_varint, u64);
varint!(put_varint128, get_varint128, u128);

#[inline]
pub(crate) fn wide(k: Key) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

#[inline]
pub(crate) fn narrow(w: u128) -> Key {
    ((w >> 64) as u64, w as u64)
}

/// State `s` after `prev` in a sorted stream (`prev <= s`; the stream's
/// first after `Sk::default()`): its region's delta, its key (a delta within
/// the region, raw past it), its cell.
#[inline]
pub(crate) fn put_sk(out: &mut Vec<u8>, prev: Sk, s: Sk) {
    debug_assert!(prev <= s);
    let dr = s.region - prev.region;
    put_varint(out, dr as u64);
    put_varint128(out, if dr == 0 { wide(s.key) - wide(prev.key) } else { wide(s.key) });
    out.push(s.local);
}

/// The state `put_sk` wrote after `prev` at `b[*pos..]`, `pos` moved past it.
#[inline]
pub(crate) fn get_sk(b: &[u8], pos: &mut usize, prev: Sk) -> Sk {
    let dr = get_varint(b, pos) as u32;
    let k = get_varint128(b, pos);
    let k = if dr == 0 { wide(prev.key) + k } else { k };
    let local = b[*pos];
    *pos += 1;
    Sk { region: prev.region + dr, key: narrow(k), local }
}
