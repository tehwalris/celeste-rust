//! The byte codes the tree's formats share: LEB128 varints (7 bits a byte,
//! the low ones first), and an exact key as one `u128` (its first word
//! high, so the order is the key's).

use super::visited::Key;

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
