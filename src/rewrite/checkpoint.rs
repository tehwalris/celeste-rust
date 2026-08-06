//! Checkpointing for the abstract search (`AbstractRun`).
//!
//! Format (see plans/refinement-plan.md): JSON strictly for metadata, dense
//! binary for the big tables.
//!
//! * `meta.json` - format version, config fingerprint, frame number, counts,
//!   watermarks, per-file byte lengths. Human-readable; everything a reader
//!   needs to validate before touching a byte of binary.
//! * `visited.bin` - the row table's 128-bit keys as two columnar u64-LE
//!   arrays in dense-id order (the id is the array index, so it is implicit).
//! * `states.bin` - the boundary states (serde/bincode).
//!
//! Every `.bin` starts with an 16-byte header (magic + format version +
//! payload length) followed by one zstd frame with zstd's content checksum
//! enabled. The reader refuses ANY mismatch - magic, version, fingerprint,
//! byte lengths, row counts - loudly. "Won't be read wrong across versions"
//! is achieved by strict refusal, not by migration cleverness.
//!
//! A checkpoint is written atomically per directory: files land in
//! `<dir>/tmp-f<N>/` and the directory is renamed to `<dir>/f<N>/` last, so
//! a crash mid-write never leaves a loadable-looking half checkpoint.

use anyhow::{anyhow, Context, Result};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::interpreter::row_table::RowTable;
use crate::interpreter::state::State;

const MAGIC: &[u8; 4] = b"C8TB";
/// Bump whenever the meaning or layout of ANY checkpoint content changes,
/// including the serde shape of `State`.
pub const FORMAT_VERSION: u32 = 1;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Meta {
    pub format_version: u32,
    /// Fingerprint of everything that determines the search's trajectory:
    /// recipe text, lua sources, partition cells, and the relevant env
    /// flags. A resume under a different configuration must fail loudly.
    pub fingerprint: String,
    /// The last completed frame.
    pub frame: u32,
    pub row_count: u64,
    pub watermarks: Vec<u32>,
    pub state_count: u64,
    pub lane_count: u64,
    pub deopt_states: u64,
    pub deopt_lanes: u64,
    pub visited_bin_len: u64,
    pub states_bin_len: u64,
}

/// What determines the search trajectory; hashed into the fingerprint.
pub fn config_fingerprint(recipe_text: &str) -> String {
    config_fingerprint_with_precision(
        recipe_text,
        crate::interpreter::inspect::rem_precision_from_env(),
    )
}

/// `config_fingerprint` for an explicit precision level - the band loader
/// validates the PREVIOUS level's checkpoints, whose fingerprint differs
/// from the current run's only in the precision component.
pub fn config_fingerprint_with_precision(
    recipe_text: &str,
    precision: crate::interpreter::inspect::RemPrecision,
) -> String {
    use std::hash::{Hash, Hasher};
    let mut h = rustc_hash::FxHasher::default();
    FORMAT_VERSION.hash(&mut h);
    recipe_text.hash(&mut h);
    for path in ["lua/builtin_level_3.lua", "lua/builtin_level_4.lua", "lua/celeste-minimal.lua"] {
        std::fs::read_to_string(path).unwrap_or_default().hash(&mut h);
    }
    for flag in ["CELESTE_FRONTIER_ONLY", "CELESTE_DEOPT_COLLECT_FIRST", "CELESTE_EXACT_REM"] {
        std::env::var_os(flag).is_some().hash(&mut h);
    }
    // The rem precision level changes the reachable set; the VALUE matters.
    // Hashed only when non-default so checkpoints written before the ladder
    // existed (implicitly Bits(0)) remain valid.
    if precision != crate::interpreter::inspect::RemPrecision::Bits(0) {
        format!("{:?}", precision).hash(&mut h);
    }
    format!("{:016x}", h.finish())
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

/// Save one frame's post-subtract boundary states (`<dir>/frames/fNNN.bin`).
/// Self-contained: header + zstd content checksum; no meta entry.
pub fn save_frame_states(dir: &Path, frame: u32, states: &[State]) -> Result<()> {
    let fdir = dir.join("frames");
    std::fs::create_dir_all(&fdir)?;
    let tmp = fdir.join(format!("tmp-f{:03}.bin", frame));
    write_bin(&tmp, |w| bincode::serialize_into(w, states).context("serializing frame states"))?;
    std::fs::rename(&tmp, fdir.join(format!("f{:03}.bin", frame)))?;
    Ok(())
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

fn read_bin(path: &Path, expected_payload_len: u64) -> Result<impl Read> {
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
    let payload_len = u64::from_le_bytes(buf8);
    if payload_len != expected_payload_len {
        return Err(anyhow!(
            "{}: payload length {} != {} from meta.json",
            path.display(),
            payload_len,
            expected_payload_len
        ));
    }
    Ok(zstd::Decoder::new(file)?)
}

/// Write a flat array of (u32, u32) pairs (edge chunks). Same header + zstd
/// envelope as every other .bin.
pub fn write_u32_pairs(path: &Path, pairs: &[(u32, u32)]) -> Result<()> {
    write_bin(path, |w| {
        w.write_all(&(pairs.len() as u64).to_le_bytes())?;
        for (a, b) in pairs {
            w.write_all(&a.to_le_bytes())?;
            w.write_all(&b.to_le_bytes())?;
        }
        Ok(())
    })?;
    Ok(())
}

/// Stream the pairs from `path` through `f` without materializing them.
pub fn stream_u32_pairs(path: &Path, mut f: impl FnMut(u32, u32)) -> Result<u64> {
    let mut r = read_bin_header_len(path)?;
    let mut buf8 = [0u8; 8];
    r.read_exact(&mut buf8)?;
    let count = u64::from_le_bytes(buf8);
    let mut buf = vec![0u8; 1 << 20];
    let mut remaining = (count as usize) * 8;
    while remaining > 0 {
        let take = remaining.min(buf.len());
        // The buffer is a multiple of 8, so reads never split a pair.
        r.read_exact(&mut buf[..take])?;
        remaining -= take;
        for chunk in buf[..take].chunks_exact(8) {
            let a = u32::from_le_bytes(chunk[0..4].try_into().unwrap());
            let b = u32::from_le_bytes(chunk[4..8].try_into().unwrap());
            f(a, b);
        }
    }
    Ok(count)
}

/// Append the pairs from `path` into `out` (which should be pre-reserved).
pub fn read_u32_pairs_into(path: &Path, out: &mut Vec<(u32, u32)>) -> Result<()> {
    let mut r = read_bin_header_len(path)?;
    let mut buf8 = [0u8; 8];
    r.read_exact(&mut buf8)?;
    let count = u64::from_le_bytes(buf8) as usize;
    let mut buf = vec![0u8; 1 << 20];
    let mut remaining = count * 8;
    let mut carry: Vec<u8> = Vec::new();
    while remaining > 0 {
        let take = remaining.min(buf.len());
        r.read_exact(&mut buf[..take])?;
        remaining -= take;
        let mut data = &buf[..take];
        if !carry.is_empty() {
            let need = 8 - carry.len();
            carry.extend_from_slice(&data[..need.min(data.len())]);
            if carry.len() == 8 {
                let a = u32::from_le_bytes(carry[0..4].try_into().unwrap());
                let b = u32::from_le_bytes(carry[4..8].try_into().unwrap());
                out.push((a, b));
                data = &data[need..];
                carry.clear();
            } else {
                continue;
            }
        }
        let whole = data.len() / 8 * 8;
        for chunk in data[..whole].chunks_exact(8) {
            let a = u32::from_le_bytes(chunk[0..4].try_into().unwrap());
            let b = u32::from_le_bytes(chunk[4..8].try_into().unwrap());
            out.push((a, b));
        }
        carry.extend_from_slice(&data[whole..]);
    }
    if !carry.is_empty() {
        return Err(anyhow!("{}: trailing bytes", path.display()));
    }
    Ok(())
}

pub struct Checkpoint {
    pub meta: Meta,
    pub visited: RowTable,
    pub states: Vec<State>,
}

/// Write a checkpoint for `frame` under `dir` (atomically, via rename).
pub fn save(
    dir: &Path,
    frame: u32,
    fingerprint: &str,
    states: &[State],
    visited: &RowTable,
    deopt_events: (usize, usize),
) -> Result<PathBuf> {
    let final_dir = dir.join(format!("f{:03}", frame));
    let tmp_dir = dir.join(format!("tmp-f{:03}", frame));
    if tmp_dir.exists() {
        std::fs::remove_dir_all(&tmp_dir)?;
    }
    std::fs::create_dir_all(&tmp_dir)?;

    let rows = visited.rows_by_id();
    let visited_bin_len = write_bin(&tmp_dir.join("visited.bin"), |w| {
        w.write_all(&(rows.len() as u64).to_le_bytes())?;
        // Columnar: all low halves, then all high halves.
        for (lo, _) in &rows {
            w.write_all(&lo.to_le_bytes())?;
        }
        for (_, hi) in &rows {
            w.write_all(&hi.to_le_bytes())?;
        }
        Ok(())
    })
    .context("writing visited.bin")?;

    let states_bin_len = write_bin(&tmp_dir.join("states.bin"), |w| {
        bincode::serialize_into(w, states).context("serializing states")
    })
    .context("writing states.bin")?;

    let meta = Meta {
        format_version: FORMAT_VERSION,
        fingerprint: fingerprint.to_string(),
        frame,
        row_count: rows.len() as u64,
        watermarks: visited.watermarks().to_vec(),
        state_count: states.len() as u64,
        lane_count: states.iter().map(|s| s.vector_size as u64).sum(),
        deopt_states: deopt_events.0 as u64,
        deopt_lanes: deopt_events.1 as u64,
        visited_bin_len,
        states_bin_len,
    };
    std::fs::write(tmp_dir.join("meta.json"), serde_json::to_string_pretty(&meta)?)?;

    if final_dir.exists() {
        std::fs::remove_dir_all(&final_dir)?;
    }
    std::fs::rename(&tmp_dir, &final_dir)?;
    Ok(final_dir)
}

/// The latest complete checkpoint under `dir`, if any.
pub fn latest(dir: &Path) -> Result<Option<u32>> {
    if !dir.exists() {
        return Ok(None);
    }
    let mut best = None;
    for entry in std::fs::read_dir(dir)? {
        let name = entry?.file_name();
        let name = name.to_string_lossy();
        if let Some(rest) = name.strip_prefix('f') {
            if let Ok(frame) = rest.parse::<u32>() {
                if dir.join(&*name).join("meta.json").exists() {
                    best = best.max(Some(frame));
                }
            }
        }
    }
    Ok(best)
}

/// Load and validate the checkpoint for `frame`. Refuses on any mismatch.
pub fn load(dir: &Path, frame: u32, fingerprint: &str) -> Result<Checkpoint> {
    let cdir = dir.join(format!("f{:03}", frame));
    let meta: Meta = serde_json::from_str(
        &std::fs::read_to_string(cdir.join("meta.json"))
            .with_context(|| format!("reading {}/meta.json", cdir.display()))?,
    )?;
    if meta.format_version != FORMAT_VERSION {
        return Err(anyhow!(
            "checkpoint format version {} != expected {}",
            meta.format_version,
            FORMAT_VERSION
        ));
    }
    if meta.fingerprint != fingerprint {
        return Err(anyhow!(
            "checkpoint fingerprint {} != current configuration {} - the recipe, \
             lua sources or search flags differ from the run that wrote it",
            meta.fingerprint,
            fingerprint
        ));
    }
    if meta.frame != frame {
        return Err(anyhow!("checkpoint says frame {}, directory says {}", meta.frame, frame));
    }

    let mut r = read_bin(&cdir.join("visited.bin"), meta.visited_bin_len)?;
    let mut buf8 = [0u8; 8];
    r.read_exact(&mut buf8)?;
    let count = u64::from_le_bytes(buf8) as usize;
    if count as u64 != meta.row_count {
        return Err(anyhow!("visited.bin row count {} != meta {}", count, meta.row_count));
    }
    let mut lows = vec![0u64; count];
    let mut highs = vec![0u64; count];
    let mut buf = vec![0u8; count * 8];
    r.read_exact(&mut buf)?;
    for (i, chunk) in buf.chunks_exact(8).enumerate() {
        lows[i] = u64::from_le_bytes(chunk.try_into().unwrap());
    }
    r.read_exact(&mut buf)?;
    for (i, chunk) in buf.chunks_exact(8).enumerate() {
        highs[i] = u64::from_le_bytes(chunk.try_into().unwrap());
    }
    let rows_by_id: Vec<(u64, u64)> = lows.into_iter().zip(highs).collect();
    let visited = RowTable::from_parts(rows_by_id, meta.watermarks.clone());

    let r = read_bin(&cdir.join("states.bin"), meta.states_bin_len)?;
    let states: Vec<State> = bincode::deserialize_from(r).context("deserializing states")?;
    if states.len() as u64 != meta.state_count {
        return Err(anyhow!("states.bin count {} != meta {}", states.len(), meta.state_count));
    }
    let lanes: u64 = states.iter().map(|s| s.vector_size as u64).sum();
    if lanes != meta.lane_count {
        return Err(anyhow!("states.bin lanes {} != meta {}", lanes, meta.lane_count));
    }

    Ok(Checkpoint { meta, visited, states })
}
