//! ROWS FROM KEYS: a state's row is a function of its shape's SKELETON
//! (structure with closure captures, globals, strings, the non-value
//! cells), its exact key and its cell (plans/architecture.md "Keys"), so
//! the storage holds keys and cells only and rows are decoded on demand:
//!
//!   * every value cell is a key field (`exact::ShapeKeys::value_cells`):
//!     its code is the dictionary's at the index the key holds;
//!   * the position's `x`/`y` keep only their fraction in the key
//!     (`exact::Code::pos`); the whole pixels are the cell's, less the room
//!     offset (`pos_graph::room_offset` of the row's own `room.x/y`);
//!   * the code merges `Num(n)` and `Ival(n, n)` (producers disagree), so
//!     the skeleton keeps per value cell whether its column holds numbers,
//!     intervals or anything (`Repr`).
//!
//! A row its key does not determine is FATAL, never stored lossy: a shape's
//! first emission per worker and wave is checked against its decode
//! (`Skeletons::check_decodes`; `--key-check`: every emission), the start's
//! rows likewise (`check_piece`).

use anyhow::{anyhow, ensure, Context, Result};
use celeste_core::arith::EngineNum;
use celeste_core::pico8_num::Pico8Num as P8;

use celeste_engine::exact::{Code, FieldDict, Key, KeySpace};
use celeste_engine::runtime2::{Cell2, Col, Rt2, AV};

use crate::search::pos_graph::{cell_xy, room_offset, NO_CELL};

/// The rows `(key, cell)` of `skel`'s shape as a block (`skel`'s values are
/// not read). Rows of one key in a run (an entry's cells) share all but the
/// position: a field's code is looked up once a run, and a column stays
/// uniform until a run differs.
pub fn decode_rows<N: EngineNum>(keys: &KeySpace<N>, skel: &Rt2<N>, rows: &[(Key, u32)]) -> Result<Rt2<N>> {
    let s = keys.shape(skel.shape_hash).ok_or_else(|| anyhow!("decode: shape {:016x} is not in the key space", skel.shape_hash))?;
    let n = rows.len();
    ensure!(n > 0, "decode: no rows");
    let pos = s.record.pos;
    let is_pos = |c: u32| pos.is_some_and(|(x, y)| c == x || c == y);
    // The runs of one key.
    let mut runs: Vec<(Key, usize, usize)> = Vec::new();
    for (i, r) in rows.iter().enumerate() {
        match runs.last_mut() {
            Some(x) if x.0 == r.0 => x.2 = i + 1,
            _ => runs.push((r.0, i, i + 1)),
        }
    }
    let shape = skel.shape_hash;
    let code_in = |f: &FieldDict<N>, c: usize, k: Key| -> Result<Code<N>> {
        let i = f.index_in(k);
        ensure!((i as usize) < f.len(), "decode: shape {shape:016x} cell {c}: index {i} past the dictionary's {}", f.len());
        Ok(f.code(i))
    };
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
    // Every value cell but the position's, from the key.
    for (c, cell) in skel.structure.iter().enumerate() {
        if !matches!(cell, Cell2::Val) {
            out.cols.push(skel.cols[c].clone());
            continue;
        }
        let f = s.field(c).ok_or_else(|| anyhow!("decode: shape {shape:016x} cell {c} is not a key field"))?;
        if is_pos(c as u32) {
            out.cols.push(Col::U(AV::Nil));
            continue;
        }
        let mut b = Builder::new(Repr::of(&skel.cols[c]), n);
        // A field without bits holds its one code in every row.
        if f.bits() == 0 {
            ensure!(f.len() == 1, "decode: shape {shape:016x} cell {c}: no bits for {} codes", f.len());
            b.push(f.code(0), n)
        } else {
            runs.iter().try_for_each(|&(k, lo, hi)| b.push(code_in(f, c, k)?, hi - lo))
        }
        .with_context(|| format!("decode: shape {shape:016x} cell {c}"))?;
        out.cols.push(b.finish());
    }
    let Some((px, py)) = pos else {
        ensure!(rows.iter().all(|r| r.1 == NO_CELL), "decode: shape {shape:016x} has no position object, a row has a cell");
        return Ok(out);
    };
    // The position: the cell's whole pixels less the room offset, plus the
    // key's fraction.
    let ids = crate::compiled::ids();
    let start = (skel.cache.room_x, skel.cache.room_y);
    let room = out.global_target(ids.g_room).ok_or_else(|| anyhow!("decode: no `room` global"))?;
    let room_field = |fid: u32| -> Result<(usize, &FieldDict<N>)> {
        let c = out.obj_field_cell(room, fid).ok_or_else(|| anyhow!("decode: no room field {fid}"))? as usize;
        Ok((c, s.field(c).ok_or_else(|| anyhow!("decode: shape {shape:016x} room cell {c} is not a key field"))?))
    };
    let ((rxc, rxf), (ryc, ryf)) = (room_field(ids.f_x)?, room_field(ids.f_y)?);
    let whole = |code: Code<N>| -> Result<i16> {
        ensure!(code.kind == 0, "decode: room coordinate {:?}", code.value());
        N::from_raw(code.a).and_then(|v| v.whole_i16()).ok_or_else(|| anyhow!("decode: room coordinate {:?}", code.value()))
    };
    let (fx, fy) = (
        s.field(px as usize).ok_or_else(|| anyhow!("decode: shape {shape:016x} position cell {px} is not a key field"))?,
        s.field(py as usize).ok_or_else(|| anyhow!("decode: shape {shape:016x} position cell {py} is not a key field"))?,
    );
    let (mut bx, mut by) = (Builder::new(Repr::of(&skel.cols[px as usize]), n), Builder::new(Repr::of(&skel.cols[py as usize]), n));
    for &(k, lo, hi) in &runs {
        let (ox, oy) = room_offset(start, whole(code_in(rxf, rxc, k)?)?, whole(code_in(ryf, ryc, k)?)?)?;
        let (cx, cy) = (code_in(fx, px as usize, k)?, code_in(fy, py as usize, k)?);
        for &(_, cell) in &rows[lo..hi] {
            let (x, y) = cell_xy(cell).ok_or_else(|| anyhow!("decode: shape {shape:016x} has a position, a row is NO_CELL"))?;
            let (dx, dy) = (x - ox, y - oy);
            bx.push(Code::num(N::plus_whole(cx.a, dx), N::plus_whole(cx.b, dx)), 1).with_context(|| format!("decode: shape {shape:016x} position cell {px}"))?;
            by.push(Code::num(N::plus_whole(cy.a, dy), N::plus_whole(cy.b, dy)), 1).with_context(|| format!("decode: shape {shape:016x} position cell {py}"))?;
        }
    }
    out.cols[px as usize] = bx.finish();
    out.cols[py as usize] = by.finish();
    Ok(out)
}

/// A column being built from codes, as its representation holds them:
/// uniform (one value) until a value differs, then per row.
struct ColB<T> {
    first: Option<T>,
    vals: Option<Vec<T>>,
    n: usize,
    cap: usize,
}

impl<T: Copy + PartialEq> ColB<T> {
    fn new(cap: usize) -> Self {
        ColB { first: None, vals: None, n: 0, cap }
    }

    /// `k` rows of `x`.
    #[inline]
    fn push(&mut self, x: T, k: usize) {
        match (self.first, &mut self.vals) {
            (None, _) => self.first = Some(x),
            (Some(f), None) if f != x => {
                let mut v = Vec::with_capacity(self.cap);
                v.extend(std::iter::repeat_n(f, self.n));
                self.vals = Some(v);
            }
            _ => {}
        }
        if let Some(v) = &mut self.vals {
            v.extend(std::iter::repeat_n(x, k));
        }
        self.n += k;
    }
}

/// A value cell's column in the making, by its `Repr` (`Other`: `N` if
/// every row holds a number, `I` if an interval, else `V`).
enum Builder<N: EngineNum> {
    Num(ColB<N>),
    Ival(ColB<(N, N)>),
    Other(ColB<AV<N>>),
}

impl<N: EngineNum> Builder<N> {
    fn new(repr: Repr, n: usize) -> Builder<N> {
        match repr {
            Repr::Num => Builder::Num(ColB::new(n)),
            Repr::Ival => Builder::Ival(ColB::new(n)),
            Repr::Other => Builder::Other(ColB::new(n)),
        }
    }

    /// `k` rows of the code `c`.
    #[inline]
    fn push(&mut self, c: Code<N>, k: usize) -> Result<()> {
        let num = |r: N::Raw| N::from_raw(r).ok_or_else(|| anyhow!("a code's raw {r:?} is no number"));
        match self {
            Builder::Num(b) => {
                ensure!(c.kind == 0 && c.a == c.b, "a number column holds {c:?}");
                b.push(num(c.a)?, k);
            }
            Builder::Ival(b) => {
                ensure!(c.kind == 0, "an interval column holds {c:?}");
                b.push((num(c.a)?, num(c.b)?), k);
            }
            Builder::Other(b) => b.push(c.value(), k),
        }
        Ok(())
    }

    fn finish(self) -> Col<N> {
        match self {
            Builder::Num(b) => match b.vals {
                Some(v) => Col::N(v),
                None => Col::U(AV::Num(b.first.expect("a row"))),
            },
            Builder::Ival(b) => match b.vals {
                Some(v) => Col::I(v),
                None => {
                    let (lo, hi) = b.first.expect("a row");
                    Col::U(AV::Ival(lo, hi))
                }
            },
            Builder::Other(b) => match b.vals {
                None => Col::U(b.first.expect("a row")),
                Some(vals) => {
                    if vals.iter().all(|v| matches!(v, AV::Num(_))) {
                        Col::N(vals.iter().map(|v| if let AV::Num(x) = v { *x } else { unreachable!() }).collect())
                    } else if vals.iter().all(|v| matches!(v, AV::Ival(..))) {
                        Col::I(vals.iter().map(|v| if let AV::Ival(a, b) = v { (*a, *b) } else { unreachable!() }).collect())
                    } else {
                        Col::V(vals)
                    }
                }
            },
        }
    }
}

/// How a value cell's column holds its numbers - the one thing the code
/// does not say. Per piece, from the stored block's column (a frame file's
/// header keeps it).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Repr {
    /// `N`, or uniform `Num`.
    Num,
    /// `I`, or uniform `Ival`: points as `[n, n]`.
    Ival,
    /// `V` or any other uniform value: the column follows the values.
    Other,
}

impl Repr {
    pub fn of<N>(col: &Col<N>) -> Repr {
        match col {
            Col::N(_) | Col::U(AV::Num(_)) => Repr::Num,
            Col::I(_) | Col::U(AV::Ival(..)) => Repr::Ival,
            _ => Repr::Other,
        }
    }

    /// An empty column of the representation (a skeleton's).
    pub fn empty<N>(self) -> Col<N> {
        match self {
            Repr::Num => Col::N(Vec::new()),
            Repr::Ival => Col::I(Vec::new()),
            Repr::Other => Col::V(Vec::new()),
        }
    }
}

/// THE SHAPES' SKELETONS a wave decodes its layers with: per shape the
/// first registered (a kernel's union template, `asm_kernel::unify`, or a
/// `reference_skeleton`), kept as its `layer::SkelRecord` (what decodes)
/// and that record's block. Another record under one shape (one that would
/// decode otherwise) is FATAL.
pub struct Skeletons<N: EngineNum = P8>(std::sync::RwLock<rustc_hash::FxHashMap<u64, (super::layer::SkelRecord<N>, std::sync::Arc<Rt2<N>>)>>);

impl<N: EngineNum> Default for Skeletons<N> {
    fn default() -> Self {
        Skeletons(Default::default())
    }
}

impl<N: EngineNum> Skeletons<N> {
    /// Register `skel` for its shape (checked against the shape's).
    pub fn register(&self, skel: &Rt2<N>) -> Result<()> {
        let rec = super::layer::SkelRecord::of(skel)?;
        if let Some(a) = self.0.read().expect("skeletons").get(&skel.shape_hash) {
            ensure!(a.0 == rec, "ROWS FROM KEYS: two skeletons of shape {:016x} (another structure, non-value cell or column kind): its rows would decode as the first's", skel.shape_hash);
            return Ok(());
        }
        let rt2 = std::sync::Arc::new(rec.rt2(skel.cart.clone(), skel.cache.clone()));
        self.0.write().expect("skeletons").entry(skel.shape_hash).or_insert((rec, rt2));
        Ok(())
    }

    /// The records of the shapes registered, but not `skip`ped.
    pub fn records(&self, skip: impl Fn(u64) -> bool) -> Vec<super::layer::SkelRecord<N>> {
        let mut v: Vec<super::layer::SkelRecord<N>> = self.0.read().expect("skeletons").iter().filter(|(h, _)| !skip(**h)).map(|(_, r)| r.0.clone()).collect();
        v.sort_by_key(|r| r.shape_hash);
        v
    }

    pub fn shapes(&self) -> Vec<u64> {
        self.0.read().expect("skeletons").keys().copied().collect()
    }

    pub fn get(&self, shape: u64) -> Result<std::sync::Arc<Rt2<N>>> {
        self.0.read().expect("skeletons").get(&shape).map(|x| x.1.clone()).ok_or_else(|| anyhow!("ROWS FROM KEYS: no skeleton of shape {shape:016x} (it was never emitted)"))
    }

    /// THE GUARD: `rows` (their keys, `cells`) decode with their shape's
    /// skeleton to exactly themselves, row by row, representation included
    /// (`Col::at`: a number column's `Num`, an interval column's `Ival`).
    pub fn check_decodes(&self, keys: &KeySpace<N>, rows: &Rt2<N>, cells: &[u32]) -> Result<()> {
        let skel = self.get(rows.shape_hash)?;
        let keyed: Vec<(Key, u32)> = rows.row_keys.iter().copied().zip(cells.iter().copied()).collect();
        let d = decode_rows(keys, &skel, &keyed)?;
        let lossy = |what: String| anyhow!("ROWS FROM KEYS: an emitted row of shape {:016x} is not what its key and cell decode to ({what}): the new layer would hold another state", rows.shape_hash);
        if d.structure != rows.structure || d.globals != rows.globals || d.strings != rows.strings || d.prints != rows.prints {
            return Err(lossy("structure, globals, strings or prints".into()));
        }
        for (c, (a, b)) in rows.cols.iter().zip(&d.cols).enumerate() {
            for r in 0..rows.width {
                let (x, y) = (a.at(r), b.at(r));
                if x != y {
                    let how = if Code::of(x) == Code::of(y) { "same code, other representation" } else { "other value" };
                    return Err(lossy(format!("cell {c} row {r}: emitted {x:?}, decoded {y:?} ({how})")));
                }
            }
        }
        Ok(())
    }
}

/// A reference row's skeleton: every value cell left to the decode
/// (`Repr::Other`: the column follows its values).
pub fn reference_skeleton<N: EngineNum>(row: &Rt2<N>) -> Rt2<N> {
    let mut b = celeste_engine::slots::reshape(row, 0);
    b.cols = row.structure.iter().zip(&row.cols).map(|(cell, c)| if matches!(cell, Cell2::Val) { Col::V(Vec::new()) } else { Col::U(c.at(0)) }).collect();
    b.shape_hash = row.shape_hash;
    b
}

/// THE GUARD on the start's rows (`wave::seed`): they decode from their
/// keys and `cells` to exactly themselves, representation included, and no
/// per-row column holds one value throughout (a load collapses it:
/// `collapse_uniform`). Chunks of rows decoded in parallel.
pub fn check_piece<N: EngineNum>(keys: &KeySpace<N>, rt2: &Rt2<N>, cells: &[u32]) -> Result<()> {
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
    let n = rt2.width.div_ceil(size);
    crate::par::Pool::new(n).map(n, |k| chunk(k * size))?;
    Ok(())
}
