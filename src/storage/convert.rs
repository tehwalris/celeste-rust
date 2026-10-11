//! Tree format 13 -> 14 (plans/rows-from-keys.md), in place: a v13 frame
//! file held its rows' raw value columns, keys, ids and cells; a v14 one
//! holds the skeleton (with each numeric column's representation, which
//! only the stored columns say), the win rows and the ids. Every other
//! `C8TB` file (the storage metadata, the pos-graph, level -1 notes, marks)
//! has the same payload in both formats: its version is rewritten.
//!
//! Nothing is converted unchecked: each frame file's v14 replacement is
//! written beside it, DECODED back (the tree's metadata, the ids' cells,
//! the new header) and compared with the v13 rows exactly (`block_diff`:
//! values, column kinds, row keys, structure), its stored keys and cells
//! against its ids' (`marks::Resolver`), before any file of the tree is
//! replaced. A tree that fails is left at format 13 (the value files'
//! versions put back).

use anyhow::{anyhow, bail, ensure, Context, Result};
use celeste_core::pico8_num::Pico8Num as P8;
use serde::Deserialize;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use celeste_engine::runtime2::{Cell2, Col, Rt2, AV};

use crate::search::checkpoint::{FrameFile, FORMAT_VERSION};
use crate::storage::decode::block_diff;
use crate::storage::marks::Resolver;

const MAGIC: &[u8; 4] = b"C8TB";
const FROM: u32 = 13;

/// The v13 frame file's header (`search::checkpoint` at ba23ae9).
#[derive(Deserialize)]
struct Header13 {
    width: u32,
    structure: Vec<Cell2>,
    globals: Vec<u32>,
    strings: Vec<String>,
    prints: Vec<String>,
    shape_hash: u64,
    cols: Vec<ColMeta13>,
    keys: u64,
    ids: u64,
    cells: u64,
    wins: Vec<(u32, u32)>,
    trimmed: bool,
}

#[derive(Deserialize)]
enum ColMeta13 {
    U(AV),
    N(u64),
    I(u64),
    V(u64),
    S(u64),
}

/// A v13 `ColMeta::V`/`S` row's tag and payload words as an `AV`.
fn av13(tag: u8, a: u32, b: u32) -> Result<AV> {
    Ok(match tag {
        0 => AV::Num(P8::from_raw(a as i32)),
        1 => AV::Ival(P8::from_raw(a as i32), P8::from_raw(b as i32)),
        2 => AV::Bool(a != 0),
        3 => AV::UBool,
        4 => AV::Str(a),
        5 => AV::Nil,
        6 => AV::Ptr(a),
        7 => AV::NilPtr,
        8 => AV::UNum,
        t => bail!("unknown AV tag {t}"),
    })
}

/// A v13 frame file whole: its block (row keys included), ids, cells and
/// win rows.
struct File13 {
    rt2: Rt2,
    ids: Vec<u64>,
    cells: Vec<u32>,
    wins: Vec<(u32, u32)>,
}

fn load13(path: &Path, room: (i16, i16)) -> Result<File13> {
    let bytes = std::fs::read(path)?;
    ensure!(bytes.len() >= 16 && &bytes[0..4] == MAGIC, "bad magic");
    let version = u32::from_le_bytes(bytes[4..8].try_into().unwrap());
    ensure!(version == FROM, "format {version}, not {FROM}");
    let hlen = u64::from_le_bytes(bytes[8..16].try_into().unwrap()) as usize;
    ensure!(bytes.len() >= 16 + hlen, "truncated header");
    let h: Header13 = bincode::deserialize(&bytes[16..16 + hlen]).context("the v13 header")?;
    ensure!(!h.trimmed, "TRIMMED (--trim-rows): its rows' values are gone, and with them which numeric columns hold intervals");
    ensure!(h.width > 0, "an empty piece");
    let w = h.width as usize;
    let data = &bytes[16 + hlen..];
    let at = |off: u64, row: usize, n: usize| -> Result<&[u8]> {
        let b = off as usize + row * n;
        data.get(b..b + n).ok_or_else(|| anyhow!("truncated data region"))
    };
    let u32_at = |off: u64, row: usize, n: usize, k: usize| -> Result<u32> { Ok(u32::from_le_bytes(at(off, row, n)?[k..k + 4].try_into().unwrap())) };
    let (cart, cache) = crate::compiled::room_context(room)?;
    let mut rt2 = Rt2::empty(w, h.globals.len(), &[], cart, cache);
    rt2.structure = h.structure;
    rt2.globals = h.globals;
    rt2.strings = h.strings;
    rt2.prints = h.prints;
    rt2.shape_hash = h.shape_hash;
    rt2.cols = h
        .cols
        .iter()
        .map(|m| -> Result<Col> {
            Ok(match *m {
                ColMeta13::U(v) => Col::U(v),
                ColMeta13::N(o) => Col::N((0..w).map(|r| Ok(P8::from_raw(u32_at(o, r, 4, 0)? as i32))).collect::<Result<_>>()?),
                ColMeta13::I(o) => Col::I((0..w).map(|r| Ok((P8::from_raw(u32_at(o, r, 8, 0)? as i32), P8::from_raw(u32_at(o, r, 8, 4)? as i32)))).collect::<Result<_>>()?),
                ColMeta13::V(o) => Col::V((0..w).map(|r| av13(at(o, r, 9)?[0], u32_at(o, r, 9, 1)?, u32_at(o, r, 9, 5)?)).collect::<Result<_>>()?),
                ColMeta13::S(o) => Col::V((0..w).map(|r| { let x = at(o, r, 1)?[0]; av13(x >> 1, (x & 1) as u32, 0) }).collect::<Result<_>>()?),
            })
        })
        .collect::<Result<_>>()?;
    rt2.row_keys = (0..w).map(|r| -> Result<(u64, u64)> {
        let b = at(h.keys, r, 16)?;
        Ok((u64::from_le_bytes(b[0..8].try_into().unwrap()), u64::from_le_bytes(b[8..16].try_into().unwrap())))
    }).collect::<Result<_>>()?;
    let ids = (0..w).map(|r| Ok(u64::from_le_bytes(at(h.ids, r, 8)?.try_into().unwrap()))).collect::<Result<_>>()?;
    let cells = (0..w).map(|r| u32_at(h.cells, r, 4, 0)).collect::<Result<_>>()?;
    Ok(File13 { rt2, ids, cells, wins: h.wins })
}

/// A file's `C8TB` version, if it is one.
fn version_of(path: &Path) -> Result<Option<u32>> {
    let mut f = std::fs::File::open(path)?;
    let mut b = [0u8; 8];
    if f.read(&mut b)? < 8 || &b[0..4] != MAGIC {
        return Ok(None);
    }
    Ok(Some(u32::from_le_bytes(b[4..8].try_into().unwrap())))
}

fn set_version(path: &Path, v: u32) -> Result<()> {
    let mut f = std::fs::OpenOptions::new().write(true).open(path)?;
    f.seek(SeekFrom::Start(4))?;
    f.write_all(&v.to_le_bytes())?;
    f.sync_data()?;
    Ok(())
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    for e in std::fs::read_dir(dir).with_context(|| dir.display().to_string())? {
        let e = e?;
        let t = e.file_type()?;
        if t.is_dir() {
            walk(&e.path(), out)?;
        } else if t.is_file() {
            out.push(e.path());
        }
    }
    Ok(())
}

/// A frame file: `<tree>/frames/fNNN/s*.bin`; its tree and frame.
fn frame_file(p: &Path) -> Option<(PathBuf, u32)> {
    let name = p.file_name()?.to_str()?;
    let fdir = p.parent()?;
    let frames = fdir.parent()?;
    let frame = fdir.file_name()?.to_str()?.strip_prefix('f')?.parse().ok()?;
    (name.starts_with('s') && name.ends_with(".bin") && frames.file_name()? == "frames").then(|| (frames.parent().unwrap().to_path_buf(), frame))
}

#[derive(Default)]
pub struct Report {
    pub trees: usize,
    pub frame_files: usize,
    pub rows: usize,
    pub value_files: usize,
    pub bytes_before: u64,
    pub bytes_after: u64,
}

/// Convert every format-13 tree under `dir` (a level's tree, a search's
/// checkpoint dir, or any dir holding them) to format 14 in place, each
/// checked as above, against the room its `settings.txt` records (`room`
/// for a tree without one: a wrong room fails the check). Refuses a dir
/// holding any other format, and a leftover conversion file, before
/// touching anything.
pub fn convert(dir: &Path, room: (i16, i16), threads: usize) -> Result<Report> {
    let mut files = Vec::new();
    walk(dir, &mut files)?;
    files.sort();
    let mut frames: std::collections::BTreeMap<PathBuf, (u32, Vec<PathBuf>)> = Default::default();
    let mut values = Vec::new();
    for p in &files {
        let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
        ensure!(!name.starts_with("cv-"), "{}: a leftover of an interrupted conversion (its tree is unconverted: delete it and rerun)", p.display());
        let Some(v) = version_of(p)? else { continue };
        match v {
            FROM => {}
            FORMAT_VERSION => continue,
            v => bail!("{}: format {v}: only {FROM} converts to {FORMAT_VERSION}", p.display()),
        }
        match frame_file(p) {
            Some((tree, f)) => {
                let e = frames.entry(tree).or_default();
                e.0 = e.0.max(f);
                e.1.push(p.clone());
            }
            None => values.push(p.clone()),
        }
    }
    let mut r = Report::default();
    // Value files outside the trees being converted (marks), and within.
    let in_tree = |p: &Path| frames.keys().filter(|t| p.starts_with(t)).max_by_key(|t| t.as_os_str().len()).cloned();
    for (tree, (last, paths)) in &frames {
        let mine: Vec<&PathBuf> = values.iter().filter(|p| in_tree(p).as_ref() == Some(tree)).collect();
        let room = tree_room(tree)?.unwrap_or(room);
        convert_tree(tree, *last, paths, &mine, room, threads, &mut r).with_context(|| format!("{}: left at format {FROM}", tree.display()))?;
        r.trees += 1;
    }
    for p in values.iter().filter(|p| in_tree(p).is_none()) {
        set_version(p, FORMAT_VERSION)?;
        r.value_files += 1;
    }
    Ok(r)
}

/// The room a tree records (`settings.txt`): its rows decode against it
/// (the position's whole pixels are the cell's less the room offset).
fn tree_room(tree: &Path) -> Result<Option<(i16, i16)>> {
    let Ok(text) = std::fs::read_to_string(crate::settings::path(tree)) else { return Ok(None) };
    let Some(v) = text.lines().find_map(|l| l.strip_prefix("room ")) else { return Ok(None) };
    let (x, y) = v.split_once(',').ok_or_else(|| anyhow!("{}: room {v}", tree.display()))?;
    Ok(Some((x.trim().parse()?, y.trim().parse()?)))
}

fn convert_tree(tree: &Path, last: u32, paths: &[PathBuf], values: &[&PathBuf], room: (i16, i16), threads: usize, r: &mut Report) -> Result<()> {
    // Its metadata first (the decode reads it at the new version); put back
    // on any failure.
    for p in values {
        set_version(p, FORMAT_VERSION)?;
    }
    let fail = |e: anyhow::Error| -> anyhow::Error {
        for p in values {
            if let Err(x) = set_version(p, FROM) {
                return e.context(format!("AND {}'s version could not be put back: {x}", p.display()));
            }
        }
        for p in paths {
            let _ = std::fs::remove_file(cv_path(p));
        }
        e
    };
    let checked = (|| -> Result<(usize, u64, u64)> {
        let resolver = Resolver::load(tree, last)?;
        let next = AtomicUsize::new(0);
        let (rows, before, after) = std::thread::scope(|scope| {
            let hs: Vec<_> = (0..threads.max(1))
                .map(|_| {
                    scope.spawn(|| -> Result<(usize, u64, u64)> {
                        let mut acc = (0, 0, 0);
                        loop {
                            let i = next.fetch_add(1, Ordering::Relaxed);
                            let Some(p) = paths.get(i) else { return Ok(acc) };
                            let (n, b, a) = convert_file(p, &resolver, room).with_context(|| p.display().to_string())?;
                            acc = (acc.0 + n, acc.1 + b, acc.2 + a);
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a conversion thread panicked")).try_fold((0, 0, 0), |s, x| x.map(|x| (s.0 + x.0, s.1 + x.1, s.2 + x.2)))
        })?;
        Ok((rows, before, after))
    })();
    let (rows, before, after) = checked.map_err(fail)?;
    // Every file checked: replace them.
    for p in paths {
        std::fs::rename(cv_path(p), p)?;
    }
    r.frame_files += paths.len();
    r.rows += rows;
    r.value_files += values.len();
    r.bytes_before += before;
    r.bytes_after += after;
    Ok(())
}

fn cv_path(p: &Path) -> PathBuf {
    p.with_file_name(format!("cv-{}", p.file_name().and_then(|n| n.to_str()).unwrap_or("")))
}

/// One frame file to its checked v14 replacement beside it (`cv-`):
/// `(rows, bytes before, bytes after)`.
fn convert_file(p: &Path, resolver: &Resolver, room: (i16, i16)) -> Result<(usize, u64, u64)> {
    let old = load13(p, room)?;
    let w = old.ids.len();
    for (r, &id) in old.ids.iter().enumerate() {
        let (shape, key, cell) = resolver.resolve(id)?;
        ensure!(
            shape == old.rt2.shape_hash && key == old.rt2.row_keys[r] && cell == old.cells[r],
            "row {r} (state {}): stored (shape, key, cell) is not its id's",
            crate::storage::show_id(id)
        );
    }
    let mut wins = vec![false; w];
    for &(row, cell) in &old.wins {
        ensure!((row as usize) < w && old.cells[row as usize] == cell, "win row {row}: not a row at cell {cell}");
        wins[row as usize] = true;
    }
    let cv = cv_path(p);
    crate::search::checkpoint::save_block(&cv, &old.rt2, &old.ids, &old.cells, &wins)?;
    let new = FrameFile::open(&cv)?;
    ensure!(new.ids() == old.ids && new.win_rows() == old.wins.as_slice(), "the new file's ids or win rows differ");
    let back = new.load_all(room, resolver)?.ok_or_else(|| anyhow!("no rows decoded"))?;
    if let Some(d) = block_diff(&old.rt2, &back) {
        bail!("a decoded row differs from the stored one: {d}");
    }
    Ok((w, std::fs::metadata(p)?.len(), std::fs::metadata(&cv)?.len()))
}
