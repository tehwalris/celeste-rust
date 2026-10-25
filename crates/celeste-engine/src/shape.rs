//! A SHAPE: the canonical structure a block's shape hash names. `sig` is
//! its signature's codec - the words `Rt2::shape_words` writes (what the
//! hash hashes and a key space records) and their one reader.

/// A cell's kind, as the signature names it (`Cell2::kind`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
    Val,
    Obj,
    Arr,
    Unk,
    Clo,
    Bi,
}

/// THE SIGNATURE, word by word: every cell in canonical order, each its
/// kind's tag and what the kind names - an object `OBJ, n, (field, cell)*n`,
/// an array `ARR, n, cell*n`, a closure `CLO, function, captures`, a
/// builtin `BI, builtin` (names as their codes); a value `VAL` and an
/// unknown table `UNK` alone - then `END` and the globals present as
/// `(global, cell)` pairs.
pub mod sig {
    use super::Kind;

    pub const VAL: u32 = 1;
    pub const OBJ: u32 = 2;
    pub const ARR: u32 = 3;
    pub const UNK: u32 = 4;
    pub const CLO: u32 = 5;
    pub const BI: u32 = 6;
    pub const END: u32 = u32::MAX;

    /// The cells' kinds of a signature: its ONE reader. A signature that is
    /// not one (an unknown tag, a cell past its end, no `END`, an odd
    /// globals part) is refused.
    pub fn kinds(sig: &[u32]) -> Result<Vec<Kind>, String> {
        let mut out = Vec::new();
        let mut i = 0;
        let at = |i: usize| sig.get(i).copied().ok_or_else(|| format!("a signature of {} words ends inside a cell, before its END", sig.len()));
        loop {
            let (kind, words) = match at(i)? {
                END => break,
                VAL => (Kind::Val, 1),
                OBJ => (Kind::Obj, 2 + 2 * at(i + 1)? as usize),
                ARR => (Kind::Arr, 2 + at(i + 1)? as usize),
                UNK => (Kind::Unk, 1),
                CLO => (Kind::Clo, 3),
                BI => (Kind::Bi, 2),
                tag => return Err(format!("a signature's cell {} has the unknown tag {tag}", out.len())),
            };
            if i + words > sig.len() {
                return Err(format!("a signature of {} words ends inside cell {}", sig.len(), out.len()));
            }
            out.push(kind);
            i += words;
        }
        if (sig.len() - i - 1) % 2 != 0 {
            return Err(format!("a signature's globals part holds {} words, not (global, cell) pairs", sig.len() - i - 1));
        }
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::sig::{self, kinds};
    use super::Kind;
    use crate::runtime2::{Cell2, Col, Rt2, AV, P8};

    /// Every kind, as `shape_words` writes it, reads back as its cell's.
    #[test]
    fn the_signature_reads_back_its_cells_kinds() {
        let room = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(1, 0));
        let mut b = Rt2::<P8>::empty(1, celeste_names::GLOBAL_NAMES.len(), celeste_names::STRINGS, room);
        let x = celeste_names::field("x");
        b.structure = vec![
            Cell2::Val,
            Cell2::Obj(vec![(x, 2)]),
            Cell2::Val,
            Cell2::Arr(vec![0, 2]),
            Cell2::Arr(vec![]),
            Cell2::Unk,
            Cell2::Clo(0, vec![Col::U(AV::Ptr(1))].into_boxed_slice()),
            Cell2::Clo(1, Box::new([])),
            Cell2::Bi(0),
        ];
        b.cols = vec![Col::U(AV::Nil); b.structure.len()];
        b.globals[celeste_names::global("objects") as usize] = 3;
        let mut words = Vec::new();
        b.shape_words(|w| words.push(w));
        let want: Vec<Kind> = b.structure.iter().map(Cell2::kind).collect();
        assert_eq!(kinds(&words), Ok(want));
    }

    /// What is not a signature is refused, never misread.
    #[test]
    fn a_malformed_signature_is_refused() {
        for (bad, why) in [
            (vec![sig::VAL], "no END"),
            (vec![9, sig::END], "an unknown tag"),
            (vec![sig::OBJ, 2, 7, 0, sig::END], "an object past its end"),
            (vec![sig::ARR], "an array without its length"),
            (vec![sig::VAL, sig::END, 7], "a lone globals word"),
        ] {
            assert!(kinds(&bad).is_err(), "{why}: {bad:?} read as {:?}", kinds(&bad));
        }
        assert_eq!(kinds(&[sig::END]), Ok(vec![]));
        assert_eq!(kinds(&[sig::UNK, sig::END, 3, 0]), Ok(vec![Kind::Unk]));
    }
}
