//! Naming a block's cells by PATH, and building a block from a shape.
//!
//! A kernel generated from a TRACED frame declares the slots it reads
//! and writes by the path they have in the game's heap
//! (`objects[0].spd.x`), not by a canonical cell id. The id a slot has
//! depends on the whole block's shape, so binding by id means the kernel
//! and the block have to have been numbered by the same walk; binding by
//! path means the kernel is handed a block and finds out.
//!
//! This lives in the engine rather than in the tracer because the
//! GENERATED code calls it, and generated kernels sit below the tracer.
//! `trace::bind::resolve` delegates here, so there is one walk.
//!
//! ## The one thing to get right
//!
//! A pointer is a cell of its own. A global holding a table is a
//! `Cell2::Val` whose column is `AV::Ptr(t)`, and `t` is the `Obj`. So
//! walking a path dereferences BETWEEN steps but not at the end: the
//! last step lands on the `Val` cell that holds the scalar, which is the
//! cell a kernel reads and writes.

use crate::runtime2::{Cell2, Col, Rt2, AV, NONE};

/// One step of a path. Mirrors the tracer's `iface::Step`, which
/// converts into this rather than duplicating the walk below.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathStep {
    Key(String),
    /// A slot of the ARRAY part, zero-based.
    Idx(usize),
    /// An integer key, by its LUA key (1-based), rather than by array
    /// position.
    ///
    /// Written `[#n]`. Both this and `Idx` land in the array part - the
    /// interpreter materialises `t[3] = v` on an empty table as a dense
    /// array with explicit nils, and `compiled::bridge` and
    /// `trace::bind` both follow it - so the difference is only which
    /// numbering the path text uses. `[#3]` and `[2]` name the same
    /// cell.
    Int(i16),
}

/// Parse the textual form a generated kernel carries: `a.b[0].c`, with
/// `[#n]` for an integer key outside the array part.
pub fn parse_path(s: &str) -> Result<Vec<PathStep>, String> {
    let mut out = Vec::new();
    let mut rest = s;
    while !rest.is_empty() {
        if let Some(tail) = rest.strip_prefix('[') {
            let end = tail.find(']').ok_or_else(|| format!("{}: unclosed `[`", s))?;
            let body = &tail[..end];
            let step = match body.strip_prefix('#') {
                Some(n) => PathStep::Int(
                    n.parse().map_err(|_| format!("{}: `{}` is not an integer key", s, body))?,
                ),
                None => PathStep::Idx(
                    body.parse().map_err(|_| format!("{}: `{}` is not an index", s, body))?,
                ),
            };
            out.push(step);
            rest = &tail[end + 1..];
        } else {
            let rest2 = rest.strip_prefix('.').unwrap_or(rest);
            let end = rest2.find(['.', '[']).unwrap_or(rest2.len());
            if end == 0 {
                return Err(format!("{}: empty name", s));
            }
            out.push(PathStep::Key(rest2[..end].to_string()));
            rest = &rest2[end..];
        }
    }
    if out.is_empty() {
        return Err(format!("{}: empty path", s));
    }
    Ok(out)
}

/// Follow a cell holding a pointer to the cell it points at. A cell that
/// is already a table is returned unchanged, so this is idempotent.
///
/// Public because "did this path land ON the slot or on the table the
/// slot points at" is a real question for anything comparing a resolved
/// id against a recorded one - the global `objects` and the array it
/// points at both answer to the name `objects`.
pub fn deref(rt2: &Rt2, cell: u32) -> Result<u32, String> {
    match (&rt2.structure[cell as usize], &rt2.cols[cell as usize]) {
        (Cell2::Val, Col::U(AV::Ptr(t))) => Ok(*t),
        (Cell2::Val, Col::V(vs)) => {
            // Pointer topology is lane-uniform in a block by
            // construction, so a per-lane column of pointers is either
            // all one target or a broken premise. Say which.
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
                    // A Lua integer key is array slot k-1. This arm was
                    // missing for as long as an integer key outside the
                    // array part could not be represented at all; both
                    // importers flatten now, so the only thing left of
                    // the distinction is the 1-based numbering.
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

/// One cell of a kernel's STATIC output shape.
///
/// A traced frame's outcome ends in a fixed heap layout - which cells
/// are objects, which are arrays, and which points where - and that
/// layout is a compile-time constant of the kernel. This is that layout
/// in a form a `const` can hold: `Cell2` owns a `Vec` and a `Box`, so it
/// cannot be one.
#[derive(Clone, Copy, Debug)]
pub enum SCell {
    Val,
    Obj(&'static [(u32, u32)]),
    Arr(&'static [u32]),
    /// A closure cell: its index in `FN_NAMES`, and the cells its
    /// captures point at. No generated code reads through one, but both
    /// parts are hashed into the shape - so a `Clo` that forgot them
    /// would be a different shape from the one the importer builds.
    Clo(u32, &'static [u32]),
    /// A builtin cell, by index into `BUILTIN_NAMES`. Stored IN PLACE at
    /// the slot, not behind a pointer, which is what the importer does.
    Bi(u32),
    /// A table with no kind yet - an empty constructor, which is neither
    /// an object nor an array until something is put in it.
    Unk,
}

/// A fresh block with the same SHAPE as `src` and a new width.
///
/// Structure, globals and pointer columns are what a shape determines,
/// so they carry over; every other value cell starts `AV::Nil`. Used to
/// make an input block to run a kernel against, and to make an output
/// block from an outcome the tracer recorded.
///
/// `Rt2` is deliberately not `Clone` - a block is big and copying one is
/// usually a mistake - and this is not a clone: the values are dropped
/// on purpose.
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
