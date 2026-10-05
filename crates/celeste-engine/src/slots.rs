//! Naming a block's cells by PATH (`objects[0].spd.x`), and building a
//! block from a shape.
//!
//! A cell's id depends on the whole block's shape, so a traced slot is
//! bound by its heap path and resolved against the block it is handed.
//! `trace::bind::resolve` delegates here, so there is one walk.
//!
//! A pointer is a cell of its own: a global holding a table is a
//! `Cell2::Val` whose column is `AV::Ptr(t)`, and `t` is the `Obj`. So a
//! walk dereferences BETWEEN steps but not at the end: the last step lands
//! on the `Val` cell holding the scalar, the cell a kernel reads and writes.

use crate::runtime2::{Cell2, Col, Rt2, AV, NONE};

/// One step of a path. Mirrors the tracer's `iface::Step`, which
/// converts into this rather than duplicating the walk below.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathStep {
    Key(String),
    /// A slot of the ARRAY part, zero-based.
    Idx(usize),
    /// An integer key by its Lua key (1-based), written `[#n]`. Integer
    /// keys live in the array part (`t[3] = v` on an empty table is a dense
    /// array with explicit nils), so `[#3]` and `[2]` name the same cell.
    Int(i16),
}

/// Follow a cell holding a pointer to the cell it points at. A cell that
/// is already a table is returned unchanged, so this is idempotent.
/// (The global `objects` and the array it points at both answer to the
/// name `objects`; callers comparing ids need to pick one.)
pub fn deref(rt2: &Rt2, cell: u32) -> Result<u32, String> {
    match (&rt2.structure[cell as usize], &rt2.cols[cell as usize]) {
        (Cell2::Val, Col::U(AV::Ptr(t))) => Ok(*t),
        (Cell2::Val, Col::V(vs)) => {
            // Pointer topology is lane-uniform in a block, so a per-lane
            // pointer column must have one target.
            let mut it = vs.iter().filter_map(|v| match v {
                AV::Ptr(t) => Some(*t),
                _ => None,
            });
            let first = it.next().ok_or_else(|| format!("cell {} is not a pointer", cell))?;
            if it.any(|t| t != first) {
                return Err(format!("cell {} points at different tables in different lanes", cell));
            }
            Ok(first)
        }
        (Cell2::Val, _) => Err(format!("cell {} holds a scalar, not a table", cell)),
        _ => Ok(cell),
    }
}

/// The canonical cell id a path names in this block.
pub fn resolve_steps(rt2: &Rt2, p: &[PathStep]) -> Result<u32, String> {
    let mut cur: Option<u32> = None;
    for step in p {
        cur = Some(match (cur, step) {
            (None, PathStep::Key(k)) => {
                let gi = celeste_names::GLOBAL_NAMES
                    .iter()
                    .position(|n| n == k)
                    .ok_or_else(|| format!("{}: not a global the boundary names", k))?;
                let c = rt2.globals[gi];
                if c == NONE {
                    return Err(format!("{}: global absent from this block", k));
                }
                c
            }
            (None, s) => return Err(format!("{:?}: the globals table has no such key", s)),
            (Some(c), step) => {
                let t = deref(rt2, c)?;
                match (&rt2.structure[t as usize], step) {
                    (Cell2::Obj(fields), PathStep::Key(k)) => {
                        // Fields outside `FIELD_NAMES` were dropped on
                        // import, so a kernel cannot be asking for one.
                        let f = celeste_names::FIELD_NAMES
                            .iter()
                            .position(|n| n == k)
                            .ok_or_else(|| format!("{}: not a field the boundary names", k))?
                            as u32;
                        fields
                            .iter()
                            .find(|(k2, _)| *k2 == f)
                            .ok_or_else(|| format!("{}: absent from this object", k))?
                            .1
                    }
                    (Cell2::Arr(items), PathStep::Idx(i)) => *items
                        .get(*i)
                        .ok_or_else(|| format!("[{}]: past the end of a {}-item array", i, items.len()))?,
                    // A Lua integer key is array slot k-1.
                    (Cell2::Arr(items), PathStep::Int(k)) => {
                        if *k < 1 {
                            return Err(format!("[#{}]: not a Lua array key", k));
                        }
                        *items.get(*k as usize - 1).ok_or_else(|| {
                            format!("[#{}]: past the end of a {}-item array", k, items.len())
                        })?
                    }
                    (cell, step) => {
                        return Err(format!("{:?}: no such step in a {}", step, kind(cell)))
                    }
                }
            }
        });
    }
    cur.ok_or_else(|| "empty path".to_string())
}

fn kind(c: &Cell2) -> &'static str {
    match c {
        Cell2::Val => "value",
        Cell2::Obj(_) => "object",
        Cell2::Arr(_) => "array",
        Cell2::Unk => "unknown table",
        Cell2::Clo(..) => "closure",
        Cell2::Bi(_) => "builtin",
    }
}

/// A fresh block with the same SHAPE as `src` and a new width.
///
/// Structure, globals and pointer columns (what a shape determines) carry
/// over; every other value cell starts `AV::Nil`. Not a clone: `Rt2` is
/// deliberately not `Clone`, and the values are dropped on purpose.
pub fn reshape(src: &Rt2, width: usize) -> Rt2 {
    let mut b = Rt2::empty(
        width,
        src.globals.len(),
        celeste_names::STRINGS,
        src.cart.clone(),
        src.cache.clone(),
    );
    b.globals = src.globals.clone();
    b.structure = src.structure.clone();
    b.cols = src
        .cols
        .iter()
        .map(|c| match c {
            Col::U(AV::Ptr(t)) => Col::U(AV::Ptr(*t)),
            _ => Col::U(AV::Nil),
        })
        .collect();
    b
}
