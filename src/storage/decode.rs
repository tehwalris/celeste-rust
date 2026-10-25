//! ROWS FROM KEYS: a state's row is a function of its shape's SKELETON
//! (structure, closure captures, globals, strings, non-value cells), its
//! exact key and its cell (plans/architecture.md "Keys"), so the storage
//! holds keys and cells only:
//!
//!   * every value cell is a key field (`exact::ShapeKeys::value_cells`);
//!   * the position's `x`/`y` keep only their fraction in the key
//!     (`Code::of_pos`); the whole pixels are the cell's, less the room offset
//!     (`cells::room_offset` of the row's own `room.x/y`);
//!   * the code merges `Num(n)` and `Ival(n, n)` (producers disagree), so
//!     the skeleton keeps each value column's `Repr`.
//!
//! A row its key does not determine is FATAL, never stored lossy: checked
//! on a shape's first emission per worker and wave (`--key-check`: every
//! one; `Skeletons::check_decodes`) and on the start (`check_piece`).

use anyhow::{anyhow, ensure, Context, Result};
use celeste_core::arith::Arithmetic;
use celeste_core::pico8_num::Pico8Num as P8;

use celeste_engine::exact::{Code, FieldDict, Key, KeySpace};
use celeste_engine::shape::KeyRole;
use celeste_engine::runtime2::{Cell2, Col, Rt2, AV};

use crate::storage::cells::{cell_xy, room_offset, NO_CELL};

/// The rows `(key, cell)` of `skel`'s shape as a block (`skel`'s values
/// unread). A run of one key's rows differs only in position: codes are
/// looked up once a run, a column stays uniform until a run differs.
pub(crate) fn decode_rows<N: Arithmetic>(keys: &KeySpace<N>, skel: &Rt2<N>, rows: &[(Key, u32)]) -> Result<Rt2<N>> {
    let s = keys.shape(skel.shape_hash).ok_or_else(|| anyhow!("decode: shape {:016x} is not in the key space", skel.shape_hash))?;
    let n = rows.len();
    ensure!(n > 0, "decode: no rows");
    let layout = s.layout();
    ensure!(layout.cells() == skel.structure.len(), "decode: shape {:016x}: a skeleton of {} cells, a layout of {}", skel.shape_hash, skel.structure.len(), layout.cells());
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
        room: skel.room,
        shape_hash: skel.shape_hash,
        row_keys: rows.iter().map(|r| r.0).collect(),
    };
    // Every value cell but the position's, from the key, by its role.
    for c in 0..skel.structure.len() {
        let f = match layout.role(c as u32) {
            KeyRole::Structure => {
                out.cols.push(skel.cols[c].clone());
                continue;
            }
            // Below, from the row's cell.
            KeyRole::Position => {
                out.cols.push(Col::U(AV::Nil));
                continue;
            }
            KeyRole::Value | KeyRole::Leader(_) => s.field(c).ok_or_else(|| anyhow!("decode: shape {shape:016x} cell {c} has no dictionary"))?,
            KeyRole::Follower { dep, slot } => {
                // The code its leader's index recorded.
                let (g, slot) = (dep as usize, slot as usize);
                let mut b = Builder::new(Repr::of(&skel.cols[c]), n);
                let fl = &s.follows()[g];
                let lead = s.field(fl.leader() as usize).ok_or_else(|| anyhow!("decode: shape {shape:016x}: the leader is not keyed"))?;
                runs.iter()
                    .try_for_each(|&(k, lo, hi)| {
                        let i = lead.index_in(k);
                        ensure!((i as usize) < lead.len(), "decode: shape {shape:016x} leader cell {}: index {i} past the dictionary's {}", fl.leader(), lead.len());
                        b.push(fl.code(slot, i), hi - lo)
                    })
                    .with_context(|| format!("decode: shape {shape:016x} follower cell {c}"))?;
                out.cols.push(b.finish());
                continue;
            }
        };
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
    let Some((px, py)) = layout.pos() else {
        ensure!(rows.iter().all(|r| r.1 == NO_CELL), "decode: shape {shape:016x} has no position object, a row has a cell");
        return Ok(out);
    };
    // Position: the cell's pixels less the room offset, plus the fraction.
    let start = (skel.room.x, skel.room.y);
    let [rx, ry] = celeste_engine::shape::Anchors::room_of(&out).ok_or_else(|| anyhow!("decode: no `room` global with `x` and `y`"))?;
    let room_field = |c: u32| -> Result<(usize, &FieldDict<N>)> {
        let c = c as usize;
        Ok((c, s.field(c).ok_or_else(|| anyhow!("decode: shape {shape:016x} room cell {c} is not a key field"))?))
    };
    let ((rxc, rxf), (ryc, ryf)) = (room_field(rx)?, room_field(ry)?);
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
enum Builder<N: Arithmetic> {
    Num(ColB<N>),
    Ival(ColB<(N, N)>),
    Other(ColB<AV<N>>),
}

impl<N: Arithmetic> Builder<N> {
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

/// How a value column holds its numbers, which the code does not say; per
/// piece, from the stored block's column (a frame file's header keeps it).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Repr {
    /// `N`, or uniform `Num`.
    Num,
    /// `I`, or uniform `Ival`: points as `[n, n]`.
    Ival,
    /// `V` or any other uniform value: the column follows the values.
    Other,
}

impl Repr {
    pub(crate) fn of<N>(col: &Col<N>) -> Repr {
        match col {
            Col::N(_) | Col::U(AV::Num(_)) => Repr::Num,
            Col::I(_) | Col::U(AV::Ival(..)) => Repr::Ival,
            _ => Repr::Other,
        }
    }

    /// An empty column of the representation (a skeleton's).
    pub(crate) fn empty<N>(self) -> Col<N> {
        match self {
            Repr::Num => Col::N(Vec::new()),
            Repr::Ival => Col::I(Vec::new()),
            Repr::Other => Col::V(Vec::new()),
        }
    }
}

/// THE SHAPES' SKELETONS: per shape the first registered (a kernel's union
/// template, `asm_kernel::unify`, or a `reference_skeleton`), as its
/// `layer::SkelRecord` and that record's block. Another record under one
/// shape (it would decode otherwise) is FATAL.
pub(crate) struct Skeletons<N: Arithmetic = P8>(std::sync::RwLock<rustc_hash::FxHashMap<u64, (super::layer::SkelRecord<N>, std::sync::Arc<Rt2<N>>)>>);

impl<N: Arithmetic> Default for Skeletons<N> {
    fn default() -> Self {
        Skeletons(Default::default())
    }
}

impl<N: Arithmetic> Skeletons<N> {
    /// Register `skel` for its shape (checked against the shape's).
    pub(crate) fn register(&self, skel: &Rt2<N>) -> Result<()> {
        let rec = super::layer::SkelRecord::of(skel)?;
        if let Some(a) = self.0.read().expect("skeletons").get(&skel.shape_hash) {
            ensure!(a.0 == rec, "ROWS FROM KEYS: two skeletons of shape {:016x} (another structure, non-value cell or column kind): its rows would decode as the first's", skel.shape_hash);
            return Ok(());
        }
        let rt2 = std::sync::Arc::new(rec.rt2(skel.room));
        self.0.write().expect("skeletons").entry(skel.shape_hash).or_insert((rec, rt2));
        Ok(())
    }

    /// The records of the shapes registered, but not `skip`ped.
    pub(crate) fn records(&self, skip: impl Fn(u64) -> bool) -> Vec<super::layer::SkelRecord<N>> {
        let mut v: Vec<super::layer::SkelRecord<N>> = self.0.read().expect("skeletons").iter().filter(|(h, _)| !skip(**h)).map(|(_, r)| r.0.clone()).collect();
        v.sort_by_key(|r| r.shape_hash);
        v
    }

    pub(crate) fn shapes(&self) -> Vec<u64> {
        self.0.read().expect("skeletons").keys().copied().collect()
    }

    pub(crate) fn get(&self, shape: u64) -> Result<std::sync::Arc<Rt2<N>>> {
        self.0.read().expect("skeletons").get(&shape).map(|x| x.1.clone()).ok_or_else(|| anyhow!("ROWS FROM KEYS: no skeleton of shape {shape:016x} (it was never emitted)"))
    }

    /// THE GUARD: `rows` (keys, `cells`) decode to exactly themselves, row
    /// by row, representation included (`Col::at`: `Num` vs `Ival`).
    pub(crate) fn check_decodes(&self, keys: &KeySpace<N>, rows: &Rt2<N>, cells: &[u32]) -> Result<()> {
        let skel = self.get(rows.shape_hash)?;
        let keyed: Vec<(Key, u32)> = rows.row_keys.iter().copied().zip(cells.iter().copied()).collect();
        let d = decode_rows(keys, &skel, &keyed)?;
        let lossy = |what: String| anyhow!("ROWS FROM KEYS: an emitted row of shape {:016x} is not what its key and cell decode to ({what}): the new layer would hold another state", rows.shape_hash);
        if d.structure != rows.structure || d.globals != rows.globals || d.strings != rows.strings {
            return Err(lossy("structure, globals or strings".into()));
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

/// A reference row's skeleton: every value cell left to the decode (`Other`).
pub(crate) fn reference_skeleton<N: Arithmetic>(row: &Rt2<N>) -> Rt2<N> {
    let mut b = celeste_engine::slots::reshape(row, 0);
    b.cols = row.structure.iter().zip(&row.cols).map(|(cell, c)| if matches!(cell, Cell2::Val) { Col::V(Vec::new()) } else { Col::U(c.at(0)) }).collect();
    b.shape_hash = row.shape_hash;
    b
}

/// THE GUARD on the start's rows (`wave::seed`): they decode to exactly
/// themselves, representation included, and no per-row column holds one
/// value throughout (a load collapses it: `collapse_uniform`).
pub(crate) fn check_piece<N: Arithmetic>(keys: &KeySpace<N>, rt2: &Rt2<N>, cells: &[u32]) -> Result<()> {
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
