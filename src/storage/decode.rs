//! ROWS FROM KEYS (plans/rows-from-keys.md): a stored state's row is a
//! function of its shape's SKELETON (structure with closure captures,
//! globals, strings, the non-value cells), its exact key and its cell, so
//! frame files store ids only (`search::checkpoint`) and rows are decoded:
//!
//!   * every value cell is a key field (`exact::ShapeKeys::value_cells`):
//!     its code is the dictionary's at the index the key holds;
//!   * the position object's `x`/`y` keep only their fraction in the key
//!     (`exact::Code::pos`); the whole pixels are the cell's, less the
//!     room offset (`pos_graph::room_offset` of the row's own `room.x/y`,
//!     themselves key fields);
//!   * a column's representation: the code merges `Num(n)` and `Ival(n, n)`
//!     (producers disagree), so the piece keeps per value cell whether its
//!     column holds numbers, intervals or anything (`Repr`); the rest -
//!     uniform or per row - follows from the values (`wave::gather`'s rule).
//!
//! The forward checks every piece it checkpoints decodes to its rows
//! exactly, representation included (`check_piece`): a row its key does
//! not determine is FATAL, never stored lossy. Measured on format-13 trees
//! before the rows went (every row of seven fixtures, 73.9M rows, 0 differ).

use anyhow::{anyhow, ensure, Context, Result};
use celeste_core::pico8_num::Pico8Num as P8;

use celeste_engine::exact::{Code, Key, KeySpace};
use celeste_engine::runtime2::{collapse_uniform, Cell2, Col, Rt2, AV};

use crate::search::pos_graph::{cell_xy, room_offset, NO_CELL};
use crate::storage::StateId;

/// The rows `(key, cell)` of `skel`'s shape, as a block: `skel` gives the
/// structure, globals, strings and non-value cells (its values are not
/// read), `keys` the codes.
pub fn decode_rows(keys: &KeySpace, skel: &Rt2, rows: &[(Key, u32)]) -> Result<Rt2> {
    let s = keys.shape(skel.shape_hash).ok_or_else(|| anyhow!("decode: shape {:016x} is not in the key space", skel.shape_hash))?;
    let n = rows.len();
    let pos = s.record.pos;
    let is_pos = |c: u32| pos.is_some_and(|(x, y)| c == x || c == y);
    // Every value cell but the position's, from the key.
    let mut out = Rt2 {
        width: n,
        structure: skel.structure.clone(),
        cols: Vec::with_capacity(skel.cols.len()),
        globals: skel.globals.clone(),
        strings: skel.strings.clone(),
        cart: skel.cart.clone(),
        cache: skel.cache.clone(),
        prints: skel.prints.clone(),
        shape_hash: skel.shape_hash,
        row_keys: rows.iter().map(|r| r.0).collect(),
    };
    for (c, cell) in skel.structure.iter().enumerate() {
        if !matches!(cell, Cell2::Val) {
            out.cols.push(skel.cols[c].clone());
            continue;
        }
        let f = s.field(c).ok_or_else(|| anyhow!("decode: shape {:016x} cell {c} is not a key field", skel.shape_hash))?;
        if is_pos(c as u32) {
            out.cols.push(Col::U(AV::Nil));
            continue;
        }
        // A field without bits holds its one code in every row.
        let codes: Vec<Code> = if f.bits() == 0 {
            ensure!(f.len() == 1, "decode: shape {:016x} cell {c}: no bits for {} codes", skel.shape_hash, f.len());
            vec![f.code(0)]
        } else {
            let idx: Vec<u32> = rows.iter().map(|r| f.index_in(r.0)).collect();
            ensure!(idx.iter().all(|&i| (i as usize) < f.len()), "decode: shape {:016x} cell {c}: an index past the dictionary's {}", skel.shape_hash, f.len());
            idx.iter().map(|&i| f.code(i)).collect()
        };
        out.cols.push(column(Repr::of(&skel.cols[c]), &codes).with_context(|| format!("decode: shape {:016x} cell {c}", skel.shape_hash))?);
    }
    let Some((px, py)) = pos else {
        ensure!(rows.iter().all(|r| r.1 == NO_CELL), "decode: shape {:016x} has no position object, a row has a cell", skel.shape_hash);
        return Ok(out);
    };
    // The position: the cell's whole pixels less the room offset (of the
    // row's own `room.x/y`, decoded above), plus the key's fraction.
    let ids = crate::compiled::ids();
    let start = (skel.cache.room_x, skel.cache.room_y);
    let room = out.global_target(ids.g_room).ok_or_else(|| anyhow!("decode: no `room` global"))?;
    let axis = |f: u32| -> Result<Vec<i16>> {
        let c = out.obj_field_cell(room, f).ok_or_else(|| anyhow!("decode: no room field {f}"))?;
        (0..n)
            .map(|i| match out.cols[c as usize].at(i) {
                AV::Num(v) => Ok(v.whole_part_as_i16()),
                AV::Ival(a, _) => Ok(a.whole_part_as_i16()),
                v => Err(anyhow!("decode: room coordinate {v:?}")),
            })
            .collect()
    };
    let (rx, ry) = (axis(ids.f_x)?, axis(ids.f_y)?);
    let mut wx = Vec::with_capacity(n);
    let mut wy = Vec::with_capacity(n);
    for (i, &(_, cell)) in rows.iter().enumerate() {
        let (x, y) = cell_xy(cell).ok_or_else(|| anyhow!("decode: shape {:016x} has a position, a row is NO_CELL", skel.shape_hash))?;
        let (ox, oy) = room_offset(start, rx[i], ry[i])?;
        wx.push(((x - ox) as u32) << 16);
        wy.push(((y - oy) as u32) << 16);
    }
    for (c, whole) in [(px, wx), (py, wy)] {
        let f = s.field(c as usize).ok_or_else(|| anyhow!("decode: shape {:016x} position cell {c} is not a key field", skel.shape_hash))?;
        let codes = rows
            .iter()
            .zip(&whole)
            .map(|(r, &d)| {
                let i = f.index_in(r.0);
                ensure!((i as usize) < f.len(), "decode: shape {:016x} cell {c}: index {i} past the dictionary's {}", skel.shape_hash, f.len());
                let code = f.code(i);
                Ok(Code::num(code.a.wrapping_add(d), code.b.wrapping_add(d)))
            })
            .collect::<Result<Vec<Code>>>()?;
        out.cols[c as usize] = column(Repr::of(&skel.cols[c as usize]), &codes).with_context(|| format!("decode: shape {:016x} position cell {c}", skel.shape_hash))?;
    }
    Ok(out)
}

/// How a value cell's column holds its numbers - the one thing the code
/// does not say (`Num(n)` and `Ival(n, n)` are one code). Per piece, from
/// the stored block's column (a frame file's header keeps it).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Repr {
    /// `N`, or uniform `Num`.
    Num,
    /// `I`, or uniform `Ival`: points as `[n, n]`.
    Ival,
    /// `V` or any other uniform value: the gather's rule over the values.
    Other,
}

impl Repr {
    pub fn of(col: &Col) -> Repr {
        match col {
            Col::N(_) | Col::U(AV::Num(_)) => Repr::Num,
            Col::I(_) | Col::U(AV::Ival(..)) => Repr::Ival,
            _ => Repr::Other,
        }
    }

    /// An empty column of the representation (a skeleton's).
    pub fn empty(self) -> Col {
        match self {
            Repr::Num => Col::N(Vec::new()),
            Repr::Ival => Col::I(Vec::new()),
            Repr::Other => Col::V(Vec::new()),
        }
    }
}

/// A column of `codes` (one per row, or one for every row) as `repr` holds
/// it: uniform where every row holds one code (`collapse_uniform`).
fn column(repr: Repr, codes: &[Code]) -> Result<Col> {
    let first = *codes.first().ok_or_else(|| anyhow!("no rows"))?;
    let uniform = codes.iter().all(|&c| c == first);
    let num = |c: Code| (c.kind == 0).then(|| (P8::from_raw(c.a as i32), P8::from_raw(c.b as i32)));
    Ok(match repr {
        Repr::Num => {
            ensure!(codes.iter().all(|&c| num(c).is_some_and(|(a, b)| a == b)), "a number column holds {:?}", codes.iter().find(|&&c| num(c).is_none_or(|(a, b)| a != b)));
            match uniform {
                true => Col::U(AV::Num(num(first).expect("checked").0)),
                false => Col::N(codes.iter().map(|&c| num(c).expect("checked").0).collect()),
            }
        }
        Repr::Ival => {
            ensure!(codes.iter().all(|&c| num(c).is_some()), "an interval column holds {:?}", codes.iter().find(|&&c| num(c).is_none()));
            match uniform {
                true => {
                    let (a, b) = num(first).expect("checked");
                    Col::U(AV::Ival(a, b))
                }
                false => Col::I(codes.iter().map(|&c| num(c).expect("checked")).collect()),
            }
        }
        Repr::Other => {
            let vals: Vec<AV> = codes.iter().map(|c| c.value()).collect();
            let col = if vals.iter().all(|v| matches!(v, AV::Num(_))) {
                Col::N(vals.iter().map(|v| if let AV::Num(x) = v { *x } else { unreachable!() }).collect())
            } else if vals.iter().all(|v| matches!(v, AV::Ival(..))) {
                Col::I(vals.iter().map(|v| if let AV::Ival(a, b) = v { (*a, *b) } else { unreachable!() }).collect())
            } else {
                Col::V(vals)
            };
            collapse_uniform(col)
        }
    })
}

/// Where a decoded block differs from the stored one (`None`: identical,
/// representation included).
pub fn block_diff(stored: &Rt2, decoded: &Rt2) -> Option<String> {
    if stored.shape_hash != decoded.shape_hash || stored.width != decoded.width {
        return Some(format!("shape {:016x}/{:016x}, width {}/{}", stored.shape_hash, decoded.shape_hash, stored.width, decoded.width));
    }
    if stored.structure != decoded.structure {
        return Some("structure (closure captures?)".into());
    }
    if stored.globals != decoded.globals || stored.strings != decoded.strings || stored.prints != decoded.prints {
        return Some("globals, strings or prints".into());
    }
    if stored.row_keys != decoded.row_keys {
        return Some("row keys".into());
    }
    for (c, (a, b)) in stored.cols.iter().zip(&decoded.cols).enumerate() {
        if a == b {
            continue;
        }
        for r in 0..stored.width {
            let (x, y) = (a.at(r), b.at(r));
            if x != y {
                let how = if Code::of(x) == Code::of(y) { "same code, other representation" } else { "other value" };
                return Some(format!("cell {c} row {r}: stored {x:?}, decoded {y:?} ({how})"));
            }
        }
        return Some(format!("cell {c}: the same values, another column kind ({} / {})", kind(a), kind(b)));
    }
    None
}

fn kind(c: &Col) -> &'static str {
    match c {
        Col::U(_) => "U",
        Col::N(_) => "N",
        Col::I(_) => "I",
        Col::V(_) => "V",
    }
}

/// What decoding a stored id needs: the tree's key space and the key of
/// an id's entry (`storage::meta`: a resume's visited set, or a reader's
/// `marks::Resolver`).
pub trait TreeKeys {
    fn space(&self) -> &KeySpace;
    /// The key of `id`'s entry; `shape` is the file's (checked).
    fn key(&self, id: StateId, shape: u64) -> Result<Key>;
}

impl TreeKeys for crate::storage::marks::Resolver {
    fn space(&self) -> &KeySpace {
        &self.space
    }

    fn key(&self, id: StateId, shape: u64) -> Result<Key> {
        let (s, k, _) = self.resolve(id)?;
        ensure!(s == shape, "state {}: of shape {s:016x}, its file's is {shape:016x}", crate::storage::show_id(id));
        Ok(k)
    }
}

impl TreeKeys for crate::storage::visited::VisitedSet {
    fn space(&self) -> &KeySpace {
        &self.keys
    }

    fn key(&self, id: StateId, shape: u64) -> Result<Key> {
        let r = crate::storage::id_region(id);
        let s = r / self.geo.slots;
        ensure!(self.shapes().get(s as usize) == Some(&shape), "state {}: not of its file's shape {shape:016x}", crate::storage::show_id(id));
        let t = self.table(r).ok_or_else(|| anyhow!("state {}: no region table", crate::storage::show_id(id)))?;
        let e = crate::storage::id_entry(id);
        ensure!((e as usize) < t.len(), "state {}: no entry", crate::storage::show_id(id));
        Ok(t.key(e))
    }
}

/// THE GUARD before a piece is stored as ids: its rows decode, from their
/// keys and `cells`, to exactly themselves - every row's every value as the
/// column holds it (`Col::at`: a number column's `Num`, an interval
/// column's `Ival`), and every per-row column not one value throughout (a
/// load collapses it: `collapse_uniform`). Chunks of rows decoded in
/// parallel.
pub fn check_piece(keys: &KeySpace, rt2: &Rt2, cells: &[u32]) -> Result<()> {
    const CHUNK: usize = 1 << 15;
    ensure!(rt2.row_keys.len() == rt2.width && cells.len() == rt2.width, "check_piece: a block without its keys or cells");
    let lossy = |what: String| anyhow!("ROWS FROM KEYS: a piece of shape {:016x} does not decode from its keys and cells ({what}); its rows would be stored lossy", rt2.shape_hash);
    for (c, col) in rt2.cols.iter().enumerate() {
        let one = match col {
            Col::U(_) => false,
            Col::N(v) => v.iter().all(|x| *x == v[0]),
            Col::I(v) => v.iter().all(|x| *x == v[0]),
            Col::V(v) => v.iter().all(|x| *x == v[0]),
        };
        if one {
            return Err(lossy(format!("cell {c}: a per-row column holds one value")));
        }
    }
    // At most 32 chunks a piece (pieces are checked in parallel too).
    let size = CHUNK.max(rt2.width.div_ceil(32));
    let chunk = |lo: usize| -> Result<()> {
        let hi = (lo + size).min(rt2.width);
        let rows: Vec<(Key, u32)> = (lo..hi).map(|r| (rt2.row_keys[r], cells[r])).collect();
        let d = decode_rows(keys, rt2, &rows)?;
        for (c, (a, b)) in rt2.cols.iter().zip(&d.cols).enumerate() {
            // The common cases whole; else row by row.
            let same = match (a, b) {
                (Col::U(x), Col::U(y)) => Some(x == y),
                (Col::N(v), Col::N(w)) => Some(v[lo..hi] == w[..]),
                (Col::I(v), Col::I(w)) => Some(v[lo..hi] == w[..]),
                (Col::V(v), Col::V(w)) => Some(v[lo..hi] == w[..]),
                _ => None,
            };
            if same == Some(true) {
                continue;
            }
            if let Some(r) = (lo..hi).find(|&r| a.at(r) != b.at(r - lo)) {
                let (x, y) = (a.at(r), b.at(r - lo));
                let how = if Code::of(x) == Code::of(y) { "same code, other representation" } else { "other value" };
                return Err(lossy(format!("cell {c} row {r}: stored {x:?}, decoded {y:?} ({how})")));
            }
        }
        Ok(())
    };
    if rt2.width <= size {
        return chunk(0);
    }
    std::thread::scope(|scope| {
        let hs: Vec<_> = (0..rt2.width).step_by(size).map(|lo| scope.spawn(move || chunk(lo))).collect();
        hs.into_iter().map(|h| h.join().expect("a decode check panicked")).collect::<Result<Vec<()>>>()
    })?;
    Ok(())
}
