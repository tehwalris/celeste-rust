//! Batch serialization of boundary states for the abstract search.
//!
//! The sharded frontier checkpoint (`frame::forward_run`) and the reference
//! gates (`trace::refgate`, `trace::refbridge`) write and read batches of
//! `State`s through these helpers. Each `.bin` is self-contained: a 16-byte
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
use std::io::{Read, Write};
use std::path::Path;

use crate::interpreter::state::State;

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
pub const FORMAT_VERSION: u32 = 6;

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

/// Save one frame's post-subtract boundary states (`<dir>/frames/fNNN.bin`).
/// Self-contained: header + zstd content checksum; no meta entry.
pub fn save_frame_states(dir: &Path, frame: u32, states: &[State]) -> Result<()> {
    let refs: Vec<&State> = states.iter().collect();
    save_frame_state_refs(dir, frame, &refs)
}

/// Same, from borrowed references. The forward frontier is kept as the next
/// frame's input, so it is checkpointed WITHOUT cloning every state. Bincode
/// serializes `&[&State]` byte-identically to `&[State]`, so the file is
/// interchangeable with `save_frame_states`'.
pub fn save_frame_state_refs(dir: &Path, frame: u32, states: &[&State]) -> Result<()> {
    let fdir = dir.join("frames");
    std::fs::create_dir_all(&fdir)?;
    save_states_to(&fdir.join(format!("f{:03}.bin", frame)), states)
}

/// Save a batch of states to an explicit path (atomic: a `tmp-` sibling is
/// written then renamed). Same magic + version + one zstd stream as the frame
/// files, so the sharded per-(frame,shape,cell) checkpoint reuses exactly this.
pub fn save_states_to(path: &Path, states: &[&State]) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!(
        "tmp-{}",
        path.file_name().and_then(|s| s.to_str()).unwrap_or("state.bin")
    ));
    write_bin(&tmp, |w| bincode::serialize_into(w, states).context("serializing states"))?;
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// Load a batch saved by `save_states_to` (or any of the frame savers).
pub fn load_states_from(path: &Path) -> Result<Vec<State>> {
    let r = read_bin_header_len(path)?;
    bincode::deserialize_from(r).with_context(|| format!("deserializing {}", path.display()))
}

/// Load one frame's saved boundary states.
pub fn load_frame_states(dir: &Path, frame: u32) -> Result<Vec<State>> {
    let path = dir.join("frames").join(format!("f{:03}.bin", frame));
    let r = read_bin_header_len(&path)?;
    bincode::deserialize_from(r).with_context(|| format!("deserializing {}", path.display()))
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
