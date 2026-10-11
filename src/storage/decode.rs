//! ROWS FROM KEYS (plans/rows-from-keys.md): a stored state's row is a
//! function of its shape's SKELETON (structure with closure captures,
//! globals, strings, the non-value cells), its exact key and its cell.
//!
//!   * every value cell is a key field (`exact::ShapeKeys::value_cells`):
//!     its code is the dictionary's at the index the key holds;
//!   * the position object's `x`/`y` keep only their fraction in the key
//!     (`exact::Code::pos`); the whole pixels are the cell's, less the
//!     room offset (`pos_graph::room_offset` of the row's own `room.x/y`,
//!     themselves key fields);
//!   * a column's representation (`N`, `I`, `V`, uniform) is the layer
//!     gather's rule over the decoded values (`wave::gather`).
//!
//! `check_tree` decodes every stored row of a tree from its id alone - the
//! entry's key from the metadata, the cell from the id - and compares it
//! with the stored row exactly (`rewrite decode-check`, and a test).

use anyhow::{anyhow, ensure, Result};
use rustc_hash::FxHashMap;

use celeste_engine::exact::{Code, Key, KeySpace};
use celeste_engine::runtime2::{collapse_uniform, Cell2, Col, Rt2, AV};

use crate::search::pos_graph::{cell_xy, room_offset, NO_CELL};

/// The rows `(key, cell)` of `skel`'s shape, as a block: `skel` gives the
/// structure, globals, strings and non-value cells (its values are not
/// read), `keys` the codes.
pub fn decode_rows(keys: &KeySpace, skel: &Rt2, rows: &[(Key, u32)]) -> Result<Rt2> {
    let s = keys.shape(skel.shape_hash).ok_or_else(|| anyhow!("decode: shape {:016x} is not in the key space", skel.shape_hash))?;
    let n = rows.len();
    let codes_of = |c: u32| -> Result<Vec<Code>> {
        let f = s.field(c as usize).ok_or_else(|| anyhow!("decode: shape {:016x} cell {c} is not a key field", skel.shape_hash))?;
        rows.iter()
            .map(|&(k, _)| {
                let i = f.index_in(k);
                ensure!((i as usize) < f.len(), "decode: shape {:016x} cell {c}: index {i} past the dictionary's {}", skel.shape_hash, f.len());
                Ok(f.code(i))
            })
            .collect()
    };
    // The whole pixels of the position, per row (the cell less the room offset).
    let whole: Option<(u32, u32, Vec<(i32, i32)>)> = match s.record.pos {
        None => {
            ensure!(rows.iter().all(|r| r.1 == NO_CELL), "decode: shape {:016x} has no position object, a row has a cell", skel.shape_hash);
            None
        }
        Some((px, py)) => {
            let ids = crate::compiled::ids();
            let start = (skel.cache.room_x, skel.cache.room_y);
            let room = skel.global_target(ids.g_room).ok_or_else(|| anyhow!("decode: no `room` global"))?;
            let axis = |f: u32| -> Result<Vec<i16>> {
                let c = skel.obj_field_cell(room, f).ok_or_else(|| anyhow!("decode: no room field {f}"))?;
                codes_of(c)?
                    .into_iter()
                    .map(|code| match code.value() {
                        AV::Num(v) => Ok(v.whole_part_as_i16()),
                        AV::Ival(a, _) => Ok(a.whole_part_as_i16()),
                        v => Err(anyhow!("decode: room coordinate {v:?}")),
                    })
                    .collect()
            };
            let (rx, ry) = (axis(ids.f_x)?, axis(ids.f_y)?);
            let w = rows
                .iter()
                .enumerate()
                .map(|(i, &(_, cell))| {
                    let (x, y) = cell_xy(cell).ok_or_else(|| anyhow!("decode: shape {:016x} has a position, a row is NO_CELL", skel.shape_hash))?;
                    let (ox, oy) = room_offset(start, rx[i], ry[i])?;
                    Ok((x - ox, y - oy))
                })
                .collect::<Result<Vec<_>>>()?;
            Some((px, py, w))
        }
    };
    let mut cols = Vec::with_capacity(skel.cols.len());
    for (c, cell) in skel.structure.iter().enumerate() {
        if !matches!(cell, Cell2::Val) {
            cols.push(skel.cols[c].clone());
            continue;
        }
        // A position coordinate: its fraction back on its whole pixels.
        let is_pos = whole.as_ref().is_some_and(|(px, py, _)| *px == c as u32 || *py == c as u32);
        // A field without bits holds its one code in every row.
        if let Some(f) = s.field(c).filter(|f| f.bits() == 0 && !is_pos) {
            ensure!(f.len() == 1, "decode: shape {:016x} cell {c}: no bits for {} codes", skel.shape_hash, f.len());
            cols.push(Col::U(f.code(0).value()));
            continue;
        }
        let f = s.field(c).ok_or_else(|| anyhow!("decode: shape {:016x} cell {c} is not a key field", skel.shape_hash))?;
        let idx: Vec<u32> = rows.iter().map(|r| f.index_in(r.0)).collect();
        ensure!(idx.iter().all(|&i| (i as usize) < f.len()), "decode: shape {:016x} cell {c}: an index past the dictionary's {}", skel.shape_hash, f.len());
        let shift = match &whole {
            Some((px, _, w)) if *px == c as u32 => Some(w.iter().map(|p| (p.0 as u32) << 16).collect::<Vec<u32>>()),
            Some((_, py, w)) if *py == c as u32 => Some(w.iter().map(|p| (p.1 as u32) << 16).collect()),
            _ => None,
        };
        match shift {
            None => {
                // The indices used decide the column's kind (the gather's
                // rule over the values); each row's value from a table.
                let mut used = vec![false; f.len()];
                idx.iter().for_each(|&i| used[i as usize] = true);
                let table: Vec<AV> = (0..f.len() as u32).map(|i| f.code(i).value()).collect();
                let mut kinds = used.iter().zip(&table).filter(|u| *u.0).map(|(_, v)| v);
                let first = *kinds.next().expect("a non-empty block");
                let (mut nums, mut ivals, mut distinct) = (matches!(first, AV::Num(_)), matches!(first, AV::Ival(..)), 1);
                for v in kinds {
                    nums &= matches!(v, AV::Num(_));
                    ivals &= matches!(v, AV::Ival(..));
                    distinct += 1;
                }
                cols.push(if distinct == 1 {
                    Col::U(first)
                } else if nums {
                    Col::N(idx.iter().map(|&i| if let AV::Num(x) = table[i as usize] { x } else { unreachable!() }).collect())
                } else if ivals {
                    Col::I(idx.iter().map(|&i| if let AV::Ival(a, b) = table[i as usize] { (a, b) } else { unreachable!() }).collect())
                } else {
                    Col::V(idx.iter().map(|&i| table[i as usize]).collect())
                });
            }
            Some(sh) => {
                let vals: Vec<AV> = idx
                    .iter()
                    .zip(&sh)
                    .map(|(&i, &d)| {
                        let c = f.code(i);
                        Code::num(c.a.wrapping_add(d), c.b.wrapping_add(d)).value()
                    })
                    .collect();
                cols.push(column(vals));
            }
        }
    }
    Ok(Rt2 {
        width: n,
        structure: skel.structure.clone(),
        cols,
        globals: skel.globals.clone(),
        strings: skel.strings.clone(),
        cart: skel.cart.clone(),
        cache: skel.cache.clone(),
        prints: skel.prints.clone(),
        shape_hash: skel.shape_hash,
        row_keys: rows.iter().map(|r| r.0).collect(),
    })
}

/// Values as the layer gather stores them: all numbers `N`, all intervals
/// `I`, else `V`; one value for every row uniform.
fn column(vals: Vec<AV>) -> Col {
    let col = if vals.iter().all(|v| matches!(v, AV::Num(_))) {
        Col::N(vals.iter().map(|v| if let AV::Num(x) = v { *x } else { unreachable!() }).collect())
    } else if vals.iter().all(|v| matches!(v, AV::Ival(..))) {
        Col::I(vals.iter().map(|v| if let AV::Ival(a, b) = v { (*a, *b) } else { unreachable!() }).collect())
    } else {
        Col::V(vals)
    };
    collapse_uniform(col)
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

/// What `check_tree` found.
#[derive(Default, Debug)]
pub struct Report {
    pub frames: u32,
    pub files: usize,
    pub rows: u64,
    /// Files whose decoded rows differ from the stored ones, with the first
    /// difference: `(frame, file name, what)`.
    pub bad: Vec<(u32, String, String)>,
    /// Rows of the bad files.
    pub bad_rows: u64,
    /// The shapes seen, and their files' skeletons all alike.
    pub shapes: usize,
    /// Time spent decoding (rows from keys), and loading the stored rows.
    pub t_decode: std::time::Duration,
    pub t_load: std::time::Duration,
}

/// Decode every row of the tree in `dir` through frame `last` from its ID
/// (the region's shape and the entry's key from the metadata, the cell from
/// the id) and the shape's skeleton (its first file's), and compare it with
/// the stored row. Also checks that each file's key and cell columns are
/// the ids' and that every file of a shape has one skeleton.
pub fn check_tree(dir: &std::path::Path, from: u32, last: u32, room: (i16, i16)) -> Result<Report> {
    let geo = crate::storage::GEOMETRY;
    let metas = crate::storage::meta::load_tree(dir, last)?;
    let keys = crate::storage::meta::key_space(&metas)?;
    let mut shapes: FxHashMap<u32, u64> = FxHashMap::default();
    let mut entries: FxHashMap<u32, Vec<Key>> = FxHashMap::default();
    let mut all: Vec<(u32, u32, Key)> = Vec::new();
    for m in &metas {
        shapes.extend(m.shapes.iter().copied());
        all.extend(m.entries.iter().copied());
    }
    all.sort_unstable();
    for (r, e, k) in all {
        let v = entries.entry(r).or_default();
        ensure!(e as usize == v.len(), "entry {e} of region {r} after {}", v.len());
        v.push(k);
    }
    let mut skels: FxHashMap<u64, Rt2> = FxHashMap::default();
    let mut rep = Report { frames: last + 1 - from, ..Default::default() };
    for f in from..=last {
        for (_, path) in crate::frame::frame_paths(dir, f)? {
            let file = crate::search::checkpoint::FrameFile::open(&path)?;
            let t = std::time::Instant::now();
            let Some(stored) = file.load_all(room)? else { continue };
            rep.t_load += t.elapsed();
            rep.files += 1;
            rep.rows += stored.width as u64;
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("?").to_string();
            // The rows' (key, cell) from their ids alone.
            let mut rows: Vec<(Key, u32)> = Vec::with_capacity(stored.width);
            for r in 0..file.width() {
                let id = file.id_at(r);
                let region = crate::storage::id_region(id);
                let shape = shapes.get(&(region / geo.slots)).copied().ok_or_else(|| anyhow!("{name}: region {region}'s shape is not numbered"))?;
                ensure!(shape == file.shape_hash(), "{name} row {r}: its id's shape {shape:016x} is not the file's");
                let key = entries.get(&region).and_then(|v| v.get(crate::storage::id_entry(id) as usize)).copied().ok_or_else(|| anyhow!("{name} row {r}: no entry for its id"))?;
                ensure!(key == file.key_at(r), "{name} row {r}: the stored key is not its entry's");
                let cell = crate::storage::id_cell(&geo, id);
                ensure!(cell == file.cell_at(r), "{name} row {r}: the stored cell is not its id's");
                rows.push((key, cell));
            }
            let skel = skels.entry(stored.shape_hash).or_insert_with(|| stored.clone_block());
            let t = std::time::Instant::now();
            let decoded = decode_rows(&keys, skel, &rows);
            rep.t_decode += t.elapsed();
            let what = match decoded {
                Ok(d) => block_diff(&stored, &d),
                Err(e) => Some(format!("{e:#}")),
            };
            if let Some(w) = what {
                rep.bad_rows += stored.width as u64;
                rep.bad.push((f, name, w));
            }
        }
    }
    rep.shapes = skels.len();
    Ok(rep)
}
