//! THE TREE FILES' ONE CONTAINER (frame and edge files; value
//! files: the marks): a 4-byte magic (the file's kind, in
//! its arithmetic: `magic`), a u32 version (`FORMAT_VERSION`; the edge
//! files' their own), the header's length (u64; both little-endian), the
//! header (bincode 1), then the BLOCKS back to back, zstd -1, placed by the
//! header (`At`). A value file is a header and no block. Written once, to a
//! `NAME.tmp` renamed into place (a crash never leaves a loadable-looking
//! half file); read mapped (`Mapped`), magic, version and length checked.

use anyhow::{ensure, Context, Result};
use serde::{de::DeserializeOwned, Serialize};
use std::io::Write;
use std::path::{Path, PathBuf};

/// Bump whenever the meaning or layout of ANY tree content changes (older
/// trees are refused; what each version changed: git history).
pub(crate) const FORMAT_VERSION: u32 = 22;

/// The value files' magic.
const VALUE: &[u8; 4] = b"C8TB";

/// A tree file's magic in arithmetic `N`: 16.16's as it is, the doubles'
/// last byte `Arithmetic::FILE_TAG`.
pub(crate) fn magic<N: celeste_core::arith::Arithmetic>(m: &[u8; 4]) -> [u8; 4] {
    match N::FILE_TAG {
        0 => *m,
        t => [m[0], m[1], m[2], t],
    }
}

/// Where a block is: its offset past the header, its stored (zstd) and raw
/// lengths; a header holds one per block, inline.
#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct At {
    pub(crate) offset: u64,
    pub(crate) len: u32,
    pub(crate) raw: u32,
}

impl At {
    /// Blocks of these `(stored, raw)` lengths, back to back.
    pub(crate) fn place(lens: impl IntoIterator<Item = (usize, usize)>) -> Vec<At> {
        let mut offset = 0u64;
        lens.into_iter()
            .map(|(len, raw)| {
                let at = At { offset, len: len as u32, raw: raw as u32 };
                offset += len as u64;
                at
            })
            .collect()
    }

    /// Past its last byte.
    pub(crate) fn end(&self) -> u64 {
        self.offset + self.len as u64
    }
}

/// zstd -1 of a raw block, shrunk to its size (the compressor's buffer is
/// the raw bound: held to the frame's end, segments took 5x their bytes).
pub(crate) fn compress(raw: &[u8], z: &mut zstd::bulk::Compressor<'static>) -> Result<Vec<u8>> {
    ensure!(raw.len() < u32::MAX as usize, "a block of {} bytes", raw.len());
    let mut v = z.compress(raw)?;
    v.shrink_to_fit();
    Ok(v)
}

/// A stored block's raw bytes (`raw_len` from its header) into `out`.
pub(crate) fn decompress(stored: &[u8], raw_len: usize, z: &mut zstd::bulk::Decompressor<'static>, out: &mut Vec<u8>) -> Result<()> {
    out.clear();
    out.reserve(raw_len);
    let n = z.decompress_to_buffer(stored, out)?;
    ensure!(n == raw_len, "a block decompressed to {n} bytes, its header says {raw_len}");
    Ok(())
}

/// WRITE a file: `magic`, `version`, `header`, then what `blocks` writes,
/// in the order and places (`At`) the header gives. Atomic. Returns its bytes.
pub(crate) fn write(path: &Path, magic: [u8; 4], version: u32, header: &impl Serialize, blocks: impl FnOnce(&mut dyn Write) -> Result<()>) -> Result<u64> {
    let hb = bincode::serialize(header).with_context(|| format!("{}: its header", path.display()))?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut tmp = path.as_os_str().to_owned();
    tmp.push(".tmp");
    let tmp = PathBuf::from(tmp);
    let mut f = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(&tmp).with_context(|| tmp.display().to_string())?);
    f.write_all(&magic)?;
    f.write_all(&version.to_le_bytes())?;
    f.write_all(&(hb.len() as u64).to_le_bytes())?;
    f.write_all(&hb)?;
    blocks(&mut f)?;
    let file = f.into_inner().map_err(|e| anyhow::anyhow!("{}: {e}", tmp.display()))?;
    let bytes = file.metadata()?.len();
    drop(file);
    std::fs::rename(&tmp, path)?;
    Ok(bytes)
}

/// A file mapped, its magic and version checked.
pub(crate) struct Mapped {
    map: memmap2::Mmap,
    /// Where its blocks start.
    data: usize,
    path: PathBuf,
}

impl Mapped {
    /// Map the file at `path`: a `what` (for the errors) of `magic`, `version`.
    pub(crate) fn open(path: &Path, magic: [u8; 4], version: u32, what: &str) -> Result<Self> {
        let file = std::fs::File::open(path).with_context(|| path.display().to_string())?;
        // SAFETY: a tree file is written once (renamed into place) and never
        // modified.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        let data = 16 + header_len(&map, magic, version, path, what)?;
        Ok(Mapped { map, data, path: path.to_path_buf() })
    }

    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn len(&self) -> u64 {
        self.map.len() as u64
    }

    pub(crate) fn header_bytes(&self) -> &[u8] {
        &self.map[16..self.data]
    }

    pub(crate) fn header<H: DeserializeOwned>(&self) -> Result<H> {
        bincode::deserialize(self.header_bytes()).with_context(|| format!("{}: its header", self.path.display()))
    }

    /// Check that the blocks end at `end` (the last one's `At::end`): the
    /// file holds nothing past them, and all of them.
    pub(crate) fn ends_at(&self, end: u64) -> Result<()> {
        ensure!(self.len() == self.data as u64 + end, "{}: {} bytes, its header says {}", self.path.display(), self.len(), self.data as u64 + end);
        Ok(())
    }

    pub(crate) fn stored(&self, at: At) -> &[u8] {
        &self.map[self.data + at.offset as usize..self.data + at.end() as usize]
    }

    pub(crate) fn block(&self, at: At, z: &mut zstd::bulk::Decompressor<'static>, out: &mut Vec<u8>) -> Result<()> {
        decompress(self.stored(at), at.raw as usize, z, out).with_context(|| self.path.display().to_string())
    }

    /// Drop the resident pages of `bytes`, a part of this file (the page
    /// cache keeps them; a later read faults them back in): a reader
    /// streaming a file does not hold what it has read.
    pub(crate) fn release(&self, bytes: &[u8]) -> Result<()> {
        let at = (bytes.as_ptr() as usize).wrapping_sub(self.map.as_ptr() as usize);
        ensure!(at.checked_add(bytes.len()).is_some_and(|end| end <= self.map.len()), "{}: released bytes outside the file", self.path.display());
        // SAFETY: a read-only shared mapping of a file never modified (see
        // `open`): a page dropped reads back the same.
        unsafe { self.map.unchecked_advise_range(memmap2::UncheckedAdvice::DontNeed, at, bytes.len())? };
        Ok(())
    }
}

/// The header's length in `b`, its magic, version and length checked.
fn header_len(b: &[u8], magic: [u8; 4], version: u32, path: &Path, what: &str) -> Result<usize> {
    ensure!(b.len() >= 16 && b[0..4] == magic, "{}: not {what}", path.display());
    let v = u32::from_le_bytes(b[4..8].try_into().expect("4 bytes"));
    ensure!(v == version, "{}: format version {v}, this binary's is {version}", path.display());
    let n = u64::from_le_bytes(b[8..16].try_into().expect("8 bytes")) as usize;
    ensure!(b.len() >= 16 + n, "{}: truncated header", path.display());
    Ok(n)
}

/// Save a value (the marks): a file of no block.
pub(crate) fn save_value<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    write(path, *VALUE, FORMAT_VERSION, value, |_| Ok(())).map(drop)
}

/// Load a value `save_value` saved.
pub(crate) fn load_value<T: DeserializeOwned>(path: &Path) -> Result<T> {
    open_value(path)?.header()
}

/// A value file mapped, for readers that walk a large value in place (its
/// bincode: `header_bytes`).
pub(crate) fn open_value(path: &Path) -> Result<Mapped> {
    let m = Mapped::open(path, *VALUE, FORMAT_VERSION, "a value file")?;
    m.ends_at(0)?;
    Ok(m)
}
