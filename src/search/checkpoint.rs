//! Frame checkpoints: one file per (frame, shape piece), rows in flush order
//! (a row's position is its id) with an index of per-cell runs, columns RAW
//! and fixed-width, so "the rows in these cells" is a few range copies out
//! of an mmap.
//!
//! Layout: `magic | version u32 | header_len u64 | header (bincode) | data`.
//! The header holds everything not per-row (structure, globals, strings,
//! uniform columns, win rows), the cell index and each varying column's data
//! offset; the data region is the varying columns and the key column, `width`
//! fixed-width entries each. A version mismatch is refused, not migrated.
//!
//! Writes go to a `tmp-` sibling renamed into place, so a crash never leaves
//! a loadable-looking half file.

use anyhow::{anyhow, ensure, Context, Result};
use celeste_core::pico8_num::Pico8Num as P8;
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::Path;

use celeste_engine::runtime2::{Cell2, Col, Rt2, AV};

const MAGIC: &[u8; 4] = b"C8TB";
/// Bump whenever the meaning or layout of ANY checkpoint content changes
/// (older trees are refused; history in git).
pub const FORMAT_VERSION: u32 = 9;

/// Where one column lives: uniform (in the header) or raw in the data
/// region at a byte offset, `width` entries of the kind's fixed width.
#[derive(Serialize, Deserialize, Clone)]
enum ColMeta {
    U(AV),
    /// `Col::N`: 4 bytes per row (the 16.16 bits).
    N(u64),
    /// `Col::I`: 8 bytes per row (low, high).
    I(u64),
    /// `Col::V`: 16 bytes per row (`encode_av`).
    V(u64),
}

const N_BYTES: usize = 4;
const I_BYTES: usize = 8;
const V_BYTES: usize = 16;
const KEY_BYTES: usize = 16;

#[derive(Serialize, Deserialize)]
struct Header {
    width: u32,
    structure: Vec<Cell2>,
    globals: Vec<u32>,
    strings: Vec<String>,
    prints: Vec<String>,
    shape_hash: u64,
    cols: Vec<ColMeta>,
    /// Data offset of the key column, 16 bytes per row.
    keys: u64,
    /// The runs of rows at one cell, `(cell, start, len)` sorted by
    /// `(cell, start)`: one per flush that appended to the piece.
    index: Vec<(u32, u32, u32)>,
    /// The rows that are wins (`Block::wins`), ascending, with their cell.
    wins: Vec<(u32, u32)>,
}

/// A row's `AV` as 16 fixed bytes: tag, two payload words, zero.
fn encode_av(v: AV, out: &mut [u8]) {
    let (tag, a, b): (u32, u32, u32) = match v {
        AV::Num(n) => (0, n.as_raw_u32(), 0),
        AV::Ival(lo, hi) => (1, lo.as_raw_u32(), hi.as_raw_u32()),
        AV::Bool(x) => (2, x as u32, 0),
        AV::UBool => (3, 0, 0),
        AV::Str(s) => (4, s, 0),
        AV::Nil => (5, 0, 0),
        AV::Ptr(p) => (6, p, 0),
        AV::NilPtr => (7, 0, 0),
        AV::UNum => (8, 0, 0),
    };
    out[0..4].copy_from_slice(&tag.to_le_bytes());
    out[4..8].copy_from_slice(&a.to_le_bytes());
    out[8..12].copy_from_slice(&b.to_le_bytes());
    out[12..16].copy_from_slice(&0u32.to_le_bytes());
}

fn decode_av(bytes: &[u8]) -> Result<AV> {
    let w = |i: usize| u32::from_le_bytes(bytes[i * 4..i * 4 + 4].try_into().unwrap());
    let (tag, a, b) = (w(0), w(1), w(2));
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
        t => return Err(anyhow!("checkpoint: unknown AV tag {t}")),
    })
}

/// Save one piece in ITS OWN row order; `cells` per row, `wins` marks the
/// win rows. Atomic.
pub fn save_block(path: &Path, rt2: &Rt2, cells: &[u32], wins: &[bool]) -> Result<()> {
    let width = rt2.width;
    ensure!(rt2.row_keys.len() == width, "checkpointing a block without its key column");
    ensure!(cells.len() == width && wins.len() == width, "save_block: column lengths");
    for cell in &rt2.structure {
        if let Cell2::Clo(_, caps) = cell {
            ensure!(
                caps.iter().all(|c| matches!(c, Col::U(_))),
                "save_block: a closure capture is not uniform"
            );
        }
    }

    // Data region layout, and the header that describes it.
    let mut off: u64 = 0;
    let mut cols = Vec::with_capacity(rt2.cols.len());
    for col in &rt2.cols {
        cols.push(match col {
            Col::U(v) => ColMeta::U(*v),
            Col::N(_) => {
                let m = ColMeta::N(off);
                off += (width * N_BYTES) as u64;
                m
            }
            Col::I(_) => {
                let m = ColMeta::I(off);
                off += (width * I_BYTES) as u64;
                m
            }
            Col::V(_) => {
                let m = ColMeta::V(off);
                off += (width * V_BYTES) as u64;
                m
            }
        });
    }
    let keys_off = off;
    off += (width * KEY_BYTES) as u64;
    let data_len = off as usize;

    let mut index: Vec<(u32, u32, u32)> = Vec::new();
    for (i, &c) in cells.iter().enumerate() {
        match index.last_mut() {
            Some(run) if run.0 == c => run.2 += 1,
            _ => index.push((c, i as u32, 1)),
        }
    }
    index.sort_unstable();
    let header = Header {
        width: width as u32,
        structure: rt2.structure.clone(),
        globals: rt2.globals.clone(),
        strings: rt2.strings.clone(),
        prints: rt2.prints.clone(),
        shape_hash: rt2.shape_hash,
        cols,
        keys: keys_off,
        index,
        wins: wins
            .iter()
            .enumerate()
            .filter_map(|(i, &w)| w.then_some((i as u32, cells[i])))
            .collect(),
    };
    let header_bytes = bincode::serialize(&header).context("serializing checkpoint header")?;

    // The data region, column by column.
    let mut data = vec![0u8; data_len];
    for (col, meta) in rt2.cols.iter().zip(&header.cols) {
        match (col, meta) {
            (Col::N(vs), ColMeta::N(o)) => {
                let o = *o as usize;
                for (i, v) in vs.iter().enumerate() {
                    data[o + i * N_BYTES..o + (i + 1) * N_BYTES]
                        .copy_from_slice(&v.as_raw_u32().to_le_bytes());
                }
            }
            (Col::I(vs), ColMeta::I(o)) => {
                let o = *o as usize;
                for (i, (lo, hi)) in vs.iter().enumerate() {
                    let b = o + i * I_BYTES;
                    data[b..b + 4].copy_from_slice(&lo.as_raw_u32().to_le_bytes());
                    data[b + 4..b + 8].copy_from_slice(&hi.as_raw_u32().to_le_bytes());
                }
            }
            (Col::V(vs), ColMeta::V(o)) => {
                let o = *o as usize;
                for (i, v) in vs.iter().enumerate() {
                    encode_av(*v, &mut data[o + i * V_BYTES..o + (i + 1) * V_BYTES]);
                }
            }
            (Col::U(_), ColMeta::U(_)) => {}
            _ => unreachable!("column meta built from the column"),
        }
    }
    let ko = keys_off as usize;
    for (i, (a, b)) in rt2.row_keys.iter().enumerate() {
        let base = ko + i * KEY_BYTES;
        data[base..base + 8].copy_from_slice(&a.to_le_bytes());
        data[base + 8..base + 16].copy_from_slice(&b.to_le_bytes());
    }

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(
        "tmp-{}",
        path.file_name().and_then(|s| s.to_str()).unwrap_or("block.bin")
    ));
    {
        let mut file = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        file.write_all(MAGIC)?;
        file.write_all(&FORMAT_VERSION.to_le_bytes())?;
        file.write_all(&(header_bytes.len() as u64).to_le_bytes())?;
        file.write_all(&header_bytes)?;
        file.write_all(&data)?;
        file.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// One checkpoint file, mapped, its header decoded; loads gather row ranges
/// into a fresh block.
pub struct FrameFile {
    map: memmap2::Mmap,
    header: Header,
    data: usize,
}

impl FrameFile {
    pub fn open(path: &Path) -> Result<Self> {
        let file = std::fs::File::open(path)?;
        // SAFETY: the file is written once (rename into place) and never
        // modified afterwards; a concurrent truncation is a bug elsewhere.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= 16 && &map[0..4] == MAGIC, "{}: bad magic", path.display());
        let version = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(
            version == FORMAT_VERSION,
            "{}: format version {} != expected {}",
            path.display(),
            version,
            FORMAT_VERSION
        );
        let header_len = u64::from_le_bytes(map[8..16].try_into().unwrap()) as usize;
        ensure!(map.len() >= 16 + header_len, "{}: truncated header", path.display());
        let header: Header = bincode::deserialize(&map[16..16 + header_len])
            .with_context(|| format!("deserializing {} header", path.display()))?;
        let data = 16 + header_len;
        let width = header.width as usize;
        let need = header.keys as usize + width * KEY_BYTES;
        ensure!(map.len() >= data + need, "{}: truncated data region", path.display());
        Ok(FrameFile { map, header, data })
    }

    pub fn width(&self) -> u32 {
        self.header.width
    }

    pub fn shape_hash(&self) -> u64 {
        self.header.shape_hash
    }

    /// `(cell, rows at that cell)` per distinct cell, ascending, off the index.
    pub fn cell_counts(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        // Runs are sorted by cell: sum each cell's.
        let idx = &self.header.index;
        idx.chunk_by(|a, b| a.0 == b.0).map(|runs| (runs[0].0, runs.iter().map(|r| r.2).sum()))
    }

    /// Every row's `(cell, key)`, cell by cell (runs in file order).
    pub fn cell_keys(&self) -> impl Iterator<Item = (u32, (u64, u64))> + '_ {
        self.header.index.iter().flat_map(move |&(cell, start, len)| (start..start + len).map(move |r| (cell, self.key(r))))
    }

    /// `cell_keys` with each row's position (its id within the file).
    pub fn cell_keys_rows(&self) -> impl Iterator<Item = (u32, (u32, (u64, u64)))> + '_ {
        self.header.index.iter().flat_map(move |&(cell, start, len)| (start..start + len).map(move |r| (r, (cell, self.key(r)))))
    }

    /// The cell index: runs `(cell, start, len)` sorted by `(cell, start)`.
    pub fn runs(&self) -> &[(u32, u32, u32)] {
        &self.header.index
    }

    /// The row ranges holding `cell`, ascending (empty if the file has none).
    pub fn rows_of_cell(&self, cell: u32) -> Vec<std::ops::Range<u32>> {
        let idx = &self.header.index;
        let lo = idx.partition_point(|e| e.0 < cell);
        let hi = idx.partition_point(|e| e.0 <= cell);
        idx[lo..hi].iter().map(|&(_, s, n)| s..s + n).collect()
    }

    /// The win rows as `(shape, key, cell)` - the backward's seeds.
    pub fn wins(&self) -> Vec<(u64, (u64, u64), u32)> {
        self.header.wins.iter().map(|&(r, cell)| (self.header.shape_hash, self.key(r), cell)).collect()
    }

    /// The win rows' positions with their cells.
    pub fn win_rows(&self) -> &[(u32, u32)] {
        &self.header.wins
    }

    /// The cell of every row (the run index inverted).
    pub fn row_cells(&self) -> Vec<u32> {
        let mut cells = vec![u32::MAX; self.header.width as usize];
        for &(cell, start, len) in &self.header.index {
            cells[start as usize..(start + len) as usize].fill(cell);
        }
        cells
    }

    pub fn key_at(&self, row: u32) -> (u64, u64) {
        self.key(row)
    }

    fn key(&self, row: u32) -> (u64, u64) {
        let base = self.data + self.header.keys as usize + row as usize * KEY_BYTES;
        (
            u64::from_le_bytes(self.map[base..base + 8].try_into().unwrap()),
            u64::from_le_bytes(self.map[base + 8..base + 16].try_into().unwrap()),
        )
    }

    /// Gather `ranges` (ascending, disjoint) into a block. `None` if they
    /// are all empty.
    pub fn load_rows(&self, ranges: &[std::ops::Range<u32>]) -> Result<Option<Rt2>> {
        let total: usize = ranges.iter().map(|r| r.len()).sum();
        if total == 0 {
            return Ok(None);
        }
        let h = &self.header;
        let (cart, cache) = crate::compiled::room_context()?;
        let mut rt2 = Rt2::empty(total, h.globals.len(), &[], cart, cache);
        rt2.structure = h.structure.clone();
        rt2.globals = h.globals.clone();
        rt2.strings = h.strings.clone();
        rt2.prints = h.prints.clone();
        rt2.shape_hash = h.shape_hash;
        let rows = || ranges.iter().flat_map(|r| r.clone().map(|x| x as usize));
        let at = |off: u64, row: usize, bytes: usize| -> &[u8] {
            let base = self.data + off as usize + row * bytes;
            &self.map[base..base + bytes]
        };
        rt2.cols = h
            .cols
            .iter()
            .map(|meta| -> Result<Col> {
                Ok(match meta {
                    ColMeta::U(v) => Col::U(*v),
                    ColMeta::N(o) => Col::N(
                        rows()
                            .map(|r| P8::from_raw(u32::from_le_bytes(at(*o, r, N_BYTES).try_into().unwrap()) as i32))
                            .collect(),
                    ),
                    ColMeta::I(o) => Col::I(
                        rows()
                            .map(|r| {
                                let b = at(*o, r, I_BYTES);
                                (
                                    P8::from_raw(u32::from_le_bytes(b[0..4].try_into().unwrap()) as i32),
                                    P8::from_raw(u32::from_le_bytes(b[4..8].try_into().unwrap()) as i32),
                                )
                            })
                            .collect(),
                    ),
                    ColMeta::V(o) => {
                        Col::V(rows().map(|r| decode_av(at(*o, r, V_BYTES))).collect::<Result<Vec<_>>>()?)
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?;
        rt2.row_keys = rows().map(|r| self.key(r as u32)).collect();
        Ok(Some(rt2))
    }

    /// The whole file as one block.
    pub fn load_all(&self) -> Result<Option<Rt2>> {
        self.load_rows(&[0..self.header.width])
    }
}

/// Save any serializable value (the marks, the pos-graph) under the same
/// header, uncompressed bincode.
pub fn save_value_to<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(
        "tmp-{}",
        path.file_name().and_then(|s| s.to_str()).unwrap_or("value.bin")
    ));
    let bytes = bincode::serialize(value).context("serializing")?;
    {
        let mut file = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        file.write_all(MAGIC)?;
        file.write_all(&FORMAT_VERSION.to_le_bytes())?;
        file.write_all(&(bytes.len() as u64).to_le_bytes())?;
        file.write_all(&bytes)?;
        file.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// Load a value saved by `save_value_to`.
pub fn load_value_from<T: serde::de::DeserializeOwned>(path: &Path) -> Result<T> {
    let bytes = std::fs::read(path)?;
    bincode::deserialize(value_payload(&bytes, path)?).with_context(|| format!("deserializing {}", path.display()))
}

/// The bincode payload of a `save_value_to` file's bytes, header checked,
/// for readers that walk a large value in place.
pub fn value_payload<'a>(bytes: &'a [u8], path: &Path) -> Result<&'a [u8]> {
    ensure!(bytes.len() >= 16 && &bytes[0..4] == MAGIC, "{}: bad magic", path.display());
    let version = u32::from_le_bytes(bytes[4..8].try_into().unwrap());
    ensure!(
        version == FORMAT_VERSION,
        "{}: format version {} != expected {}",
        path.display(),
        version,
        FORMAT_VERSION
    );
    let len = u64::from_le_bytes(bytes[8..16].try_into().unwrap()) as usize;
    ensure!(bytes.len() == 16 + len, "{}: length mismatch", path.display());
    Ok(&bytes[16..])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn av_round_trips_through_the_fixed_encoding() {
        let vals = [
            AV::Num(P8::from_raw(-0x1_2345)),
            AV::Ival(P8::from_raw(3), P8::from_raw(0x7fff_ffff)),
            AV::Bool(true),
            AV::Bool(false),
            AV::UBool,
            AV::Str(7),
            AV::Nil,
            AV::Ptr(0xdead_beef),
            AV::NilPtr,
        ];
        for v in vals {
            let mut b = [0u8; 16];
            encode_av(v, &mut b);
            assert_eq!(decode_av(&b).unwrap(), v);
        }
    }
}
