//! Block serialization for the sharded frontier checkpoint
//! (`frame::forward_run`). Each `.bin` is self-contained: a 16-byte
//! header (magic + format version + payload length) followed by one zstd
//! frame with zstd's content checksum enabled. The reader refuses any
//! mismatch - magic or version - loudly; integrity of the payload comes from
//! the zstd content checksum. "Won't be read wrong across versions" is
//! achieved by strict refusal, not by migration cleverness.
//!
//! Writes are atomic where a stable path matters: files land in a `tmp-`
//! sibling and are renamed into place last, so a crash mid-write never leaves
//! a loadable-looking half file.

use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
use std::path::Path;

use celeste_engine::runtime2::{Cell2, Col, Rt2};

const MAGIC: &[u8; 4] = b"C8TB";
/// Bump whenever the meaning or layout of ANY checkpoint content changes,
/// including the serde shape of `State`.
///
/// Version 3: the 2026-08 hash-breaking batch - `sin` registered
/// unconditionally (every state gains a builtin global, changing every
/// row hash), compile-time builtin pinning, and the fingerprint computed
/// from `CampaignConfig` with every field hashed unconditionally.
///
/// Version 4: the row key became order-independent
/// (`virtual_merge::row_key_hashes`) so a state's uniform columns are
/// folded once instead of once per lane. Every key in every visited set
/// changes, so a v3 checkpoint must be refused rather than resumed - its
/// row ids would refer to keys this build can no longer compute.
/// 4 -> 5: provenance hints (`Nil(Some(_))`, `NilPointer(name)`) are
/// erased at the frame boundary (`erase_provenance_hints`), so row keys
/// differ from the first hinted nil onward - room (1,0) diverges at f25.
/// Not a serde change; the bump exists to refuse pre-erasure checkpoints,
/// whose trajectories are a (very slightly) different search.
/// 5 -> 6: row ids are assigned by a deterministic CONTENT SORT of each
/// frame's new keys (by the engine key), not arrival order (Option 4's racy
/// within-frame skip makes arrival order timing-dependent). Every id changes,
/// so visited.bin and the sweep's id space differ; a v5 checkpoint's ids would
/// be meaningless here.
/// 6 -> 7: the sharded frontier files hold engine BLOCKS (`BlockImage`: the
/// `Rt2` structure, columns and row keys), not interpreter `State`s. Same
/// content, no bridge - a v6 shard cannot be read as a block.
pub const FORMAT_VERSION: u32 = 7;

/// What a checkpointed block is on disk: the parts of an `Rt2` that are
/// state - structure, columns, globals, strings, prints, shape hash and the
/// key column - and none of the parts that are execution scratch (arena,
/// COW history, pool, stats) or process context (cart, cache, which every
/// loaded block gets re-attached from `compiled::room_context`).
#[derive(Serialize, Deserialize)]
struct BlockImage {
    width: usize,
    structure: Vec<Cell2>,
    cols: Vec<Col>,
    globals: Vec<u32>,
    strings: Vec<String>,
    prints: Vec<String>,
    shape_hash: u64,
    row_keys: Vec<(u64, u64)>,
}

/// Save one block to an explicit path (atomic, same header + zstd stream as
/// every other checkpoint file).
pub fn save_block_to(path: &Path, rt2: &Rt2) -> Result<()> {
    assert_eq!(rt2.row_keys.len(), rt2.width, "checkpointing a block without its key column");
    // Borrow, don't clone: the image is written straight from the block.
    #[derive(Serialize)]
    struct Image<'a> {
        width: usize,
        structure: &'a [Cell2],
        cols: &'a [Col],
        globals: &'a [u32],
        strings: &'a [String],
        prints: &'a [String],
        shape_hash: u64,
        row_keys: &'a [(u64, u64)],
    }
    let image = Image {
        width: rt2.width,
        structure: &rt2.structure,
        cols: &rt2.cols,
        globals: &rt2.globals,
        strings: &rt2.strings,
        prints: &rt2.prints,
        shape_hash: rt2.shape_hash,
        row_keys: &rt2.row_keys,
    };
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(
        "tmp-{}",
        path.file_name().and_then(|s| s.to_str()).unwrap_or("block.bin")
    ));
    write_bin(&tmp, |w| bincode::serialize_into(w, &image).context("serializing block"))?;
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// Save any serializable value under the same header + zstd stream.
pub fn save_value_to<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(
        "tmp-{}",
        path.file_name().and_then(|s| s.to_str()).unwrap_or("value.bin")
    ));
    write_bin(&tmp, |w| bincode::serialize_into(w, value).context("serializing"))?;
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// Load a value saved by `save_value_to`.
pub fn load_value_from<T: serde::de::DeserializeOwned>(path: &Path) -> Result<T> {
    let r = read_bin_header_len(path)?;
    bincode::deserialize_from(r).with_context(|| format!("deserializing {}", path.display()))
}

/// Load a block saved by `save_block_to`, attached to the start room's cart
/// and collision cache.
pub fn load_block_from(path: &Path) -> Result<Rt2> {
    let r = read_bin_header_len(path)?;
    let image: BlockImage = bincode::deserialize_from(r)
        .with_context(|| format!("deserializing {}", path.display()))?;
    let (cart, cache) = crate::compiled::room_context()?;
    let mut rt2 = Rt2::empty(image.width, image.globals.len(), &[], cart, cache);
    rt2.structure = image.structure;
    rt2.cols = image.cols;
    rt2.globals = image.globals;
    rt2.strings = image.strings;
    rt2.prints = image.prints;
    rt2.shape_hash = image.shape_hash;
    rt2.row_keys = image.row_keys;
    Ok(rt2)
}

fn write_bin(path: &Path, payload_writer: impl FnOnce(&mut dyn Write) -> Result<()>) -> Result<u64> {
    let file = std::fs::File::create(path)?;
    let mut file = std::io::BufWriter::new(file);
    file.write_all(MAGIC)?;
    file.write_all(&FORMAT_VERSION.to_le_bytes())?;
    // Placeholder for the payload length; patched after writing.
    file.write_all(&0u64.to_le_bytes())?;
    let mut counter = CountingWriter { inner: &mut file, written: 0 };
    let mut zw = zstd::Encoder::new(&mut counter, 1)?;
    zw.include_checksum(true)?;
    payload_writer(&mut zw)?;
    zw.finish()?;
    let payload_len = counter.written;
    let mut file = file.into_inner().map_err(|e| anyhow!("flush: {}", e))?;
    use std::io::Seek;
    file.seek(std::io::SeekFrom::Start(8))?;
    file.write_all(&payload_len.to_le_bytes())?;
    file.flush()?;
    Ok(payload_len)
}

struct CountingWriter<W: Write> {
    inner: W,
    written: u64,
}

impl<W: Write> Write for CountingWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let n = self.inner.write(buf)?;
        self.written += n as u64;
        Ok(n)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

/// `read_bin` trusting the header's own payload length (for self-contained
/// files that have no meta.json entry); integrity comes from the zstd
/// content checksum.
fn read_bin_header_len(path: &Path) -> Result<impl Read> {
    let mut file = std::io::BufReader::new(std::fs::File::open(path)?);
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(anyhow!("{}: bad magic", path.display()));
    }
    let mut buf4 = [0u8; 4];
    file.read_exact(&mut buf4)?;
    let version = u32::from_le_bytes(buf4);
    if version != FORMAT_VERSION {
        return Err(anyhow!(
            "{}: format version {} != expected {}",
            path.display(),
            version,
            FORMAT_VERSION
        ));
    }
    let mut buf8 = [0u8; 8];
    file.read_exact(&mut buf8)?;
    Ok(zstd::Decoder::new(file)?)
}
