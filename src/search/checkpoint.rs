//! Frame checkpoints: one file per (frame, shape piece), rows in ID order
//! (`storage::StateId`: by shape, position, entry). A file holds the shape's
//! SKELETON (structure with closure captures, globals, strings, the
//! non-value cells, which value columns hold numbers or intervals), the
//! win rows and the ids - nothing else: a row is a
//! function of its skeleton, its key and its cell (plans/rows-from-keys.md),
//! and the id names both (its region's shape and entry's key in the storage
//! metadata, its cell). Rows are DECODED when loaded (`storage::decode`),
//! against the tree's key space (`decode::TreeKeys`); the forward checks
//! every piece it writes decodes to its rows exactly (`frame.rs`).
//!
//! Layout: `magic | version u32 | header_len u64 | header (bincode) | data`,
//! the data the id column (8 B a row, so an id is found by a binary search
//! in an mmap). A version mismatch is refused, not migrated.
//!
//! Writes go to a `tmp-` sibling renamed into place, so a crash never leaves
//! a loadable-looking half file.

use anyhow::{ensure, Context, Result};
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::Path;

use celeste_engine::runtime2::{Cell2, Col, Rt2};

use crate::storage::decode::{Repr, TreeKeys};

const MAGIC: &[u8; 4] = b"C8TB";
/// Bump whenever the meaning or layout of ANY checkpoint content changes
/// (older trees are refused; history in git). 13: exact packed keys, the
/// key space and the storage region side in the metadata (plans/exact-keys.md).
/// 14: frame files hold ids and the skeleton only, rows decoded from keys
/// (plans/rows-from-keys.md).
pub const FORMAT_VERSION: u32 = 14;

const ID_BYTES: usize = 8;

#[derive(Serialize, Deserialize)]
struct Header {
    width: u32,
    structure: Vec<Cell2>,
    globals: Vec<u32>,
    strings: Vec<String>,
    prints: Vec<String>,
    shape_hash: u64,
    /// The cells that are not values, with their (uniform) columns.
    others: Vec<(u32, Col)>,
    /// The value cells whose columns hold numbers (`N`, uniform `Num`: 0)
    /// or intervals (`I`, uniform `Ival`: 1), points included; the others
    /// any value (`decode::Repr`).
    numeric: Vec<(u32, u8)>,
    /// The rows that are wins (`Block::wins`), ascending, with their cell.
    wins: Vec<(u32, u32)>,
}

/// Save one piece, its rows in id order; `ids` and `cells` per row, `wins`
/// marks the win rows. Its values are NOT written: the caller has checked
/// they decode from the ids (`storage::decode`). Atomic.
pub fn save_block(path: &Path, rt2: &Rt2, ids: &[u64], cells: &[u32], wins: &[bool]) -> Result<()> {
    let width = rt2.width;
    ensure!(ids.len() == width && cells.len() == width && wins.len() == width, "save_block: column lengths");
    ensure!(ids.windows(2).all(|w| w[0] < w[1]), "save_block: rows not in id order");
    for cell in &rt2.structure {
        if let Cell2::Clo(_, caps) = cell {
            ensure!(caps.iter().all(|c| matches!(c, Col::U(_))), "save_block: a closure capture is not uniform");
        }
    }
    let (mut others, mut numeric) = (Vec::new(), Vec::new());
    for (c, (cell, col)) in rt2.structure.iter().zip(&rt2.cols).enumerate() {
        if !matches!(cell, Cell2::Val) {
            ensure!(matches!(col, Col::U(_)), "save_block: the non-value cell {c} has a varying column");
            others.push((c as u32, col.clone()));
            continue;
        }
        match Repr::of(col) {
            Repr::Num => numeric.push((c as u32, 0)),
            Repr::Ival => numeric.push((c as u32, 1)),
            Repr::Other => {}
        }
    }
    let header = Header {
        width: width as u32,
        structure: rt2.structure.clone(),
        globals: rt2.globals.clone(),
        strings: rt2.strings.clone(),
        prints: rt2.prints.clone(),
        shape_hash: rt2.shape_hash,
        others,
        numeric,
        wins: wins.iter().enumerate().filter_map(|(i, &w)| w.then_some((i as u32, cells[i]))).collect(),
    };
    let header_bytes = bincode::serialize(&header).context("serializing checkpoint header")?;
    let mut data = Vec::with_capacity(width * ID_BYTES);
    for id in ids {
        data.extend_from_slice(&id.to_le_bytes());
    }
    write_file(path, &header_bytes, &data)
}

/// Write a checkpoint file: to a `tmp-` sibling, renamed into place.
fn write_file(path: &Path, header_bytes: &[u8], data: &[u8]) -> Result<()> {
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
        file.write_all(header_bytes)?;
        file.write_all(data)?;
        file.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// One checkpoint file, mapped, its header decoded; loads decode row ranges
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
        ensure!(map.len() == data + header.width as usize * ID_BYTES, "{}: the id column is not {} rows", path.display(), header.width);
        Ok(FrameFile { map, header, data })
    }

    pub fn width(&self) -> u32 {
        self.header.width
    }

    pub fn shape_hash(&self) -> u64 {
        self.header.shape_hash
    }

    /// `(cell, rows at that cell)` per distinct cell, ascending.
    pub fn cell_counts(&self) -> Vec<(u32, u32)> {
        let mut cells = self.row_cells();
        cells.sort_unstable();
        let mut out: Vec<(u32, u32)> = Vec::new();
        for c in cells {
            match out.last_mut() {
                Some((x, n)) if *x == c => *n += 1,
                _ => out.push((c, 1)),
            }
        }
        out
    }

    /// The rows at `cell` as ranges, ascending (empty if the file has none).
    pub fn rows_of_cell(&self, cell: u32) -> Vec<std::ops::Range<u32>> {
        let mut out: Vec<std::ops::Range<u32>> = Vec::new();
        for r in 0..self.header.width {
            if self.cell_at(r) == cell {
                match out.last_mut() {
                    Some(x) if x.end == r => x.end = r + 1,
                    _ => out.push(r..r + 1),
                }
            }
        }
        out
    }

    /// The win rows' positions with their cells.
    pub fn win_rows(&self) -> &[(u32, u32)] {
        &self.header.wins
    }

    /// The cell of every row.
    pub fn row_cells(&self) -> Vec<u32> {
        (0..self.header.width).map(|r| self.cell_at(r)).collect()
    }

    /// Row `row`'s cell (its id's).
    #[inline]
    pub fn cell_at(&self, row: u32) -> u32 {
        crate::storage::id_cell(&crate::storage::GEOMETRY, self.id_at(row))
    }

    /// Row `row`'s id (`storage::StateId`).
    #[inline]
    pub fn id_at(&self, row: u32) -> u64 {
        let base = self.data + row as usize * ID_BYTES;
        u64::from_le_bytes(self.map[base..base + ID_BYTES].try_into().unwrap())
    }

    /// Every row's id, in row (= id) order.
    pub fn ids(&self) -> Vec<u64> {
        (0..self.header.width).map(|r| self.id_at(r)).collect()
    }

    /// The row holding id `id` (rows are in id order).
    pub fn find_row(&self, id: u64) -> Option<u32> {
        let (mut lo, mut hi) = (0u32, self.header.width);
        while lo < hi {
            let mid = (lo + hi) / 2;
            match self.id_at(mid).cmp(&id) {
                std::cmp::Ordering::Less => lo = mid + 1,
                std::cmp::Ordering::Greater => hi = mid,
                std::cmp::Ordering::Equal => return Some(mid),
            }
        }
        None
    }

    /// The shape's skeleton: an empty block of a search under `s` (its
    /// start room's cart and collision cache).
    fn skeleton(&self, s: &crate::settings::Settings) -> Result<Rt2> {
        let h = &self.header;
        let (cart, cache) = crate::compiled::room_context(s)?;
        let mut rt2 = Rt2::empty(0, h.globals.len(), &[], cart, cache);
        rt2.structure = h.structure.clone();
        rt2.globals = h.globals.clone();
        rt2.strings = h.strings.clone();
        rt2.prints = h.prints.clone();
        rt2.shape_hash = h.shape_hash;
        rt2.cols = vec![Repr::Other.empty(); h.structure.len()];
        for (c, col) in &h.others {
            rt2.cols[*c as usize] = col.clone();
        }
        for &(c, r) in &h.numeric {
            rt2.cols[c as usize] = if r == 0 { Repr::Num.empty() } else { Repr::Ival.empty() };
        }
        Ok(rt2)
    }

    /// Decode `ranges` (ascending, disjoint) into a block of a search from
    /// `room` (its cart and collision cache), their keys from `tree` (the
    /// tree's key space and entries). `None` if they are all empty.
    pub fn load_rows(&self, ranges: &[std::ops::Range<u32>], s: &crate::settings::Settings, tree: &dyn TreeKeys) -> Result<Option<Rt2>> {
        let total: usize = ranges.iter().map(|r| r.len()).sum();
        if total == 0 {
            return Ok(None);
        }
        let rows = ranges
            .iter()
            .flat_map(|r| r.clone())
            .map(|r| {
                let id = self.id_at(r);
                Ok((tree.key(id, self.header.shape_hash)?, self.cell_at(r)))
            })
            .collect::<Result<Vec<_>>>()?;
        crate::storage::decode::decode_rows(tree.space(), &self.skeleton(s)?, &rows).map(Some)
    }

    /// The whole file as one block.
    pub fn load_all(&self, s: &crate::settings::Settings, tree: &dyn TreeKeys) -> Result<Option<Rt2>> {
        self.load_rows(&[0..self.header.width], s, tree)
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
