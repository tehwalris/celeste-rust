//! The tree's FORMAT VERSION and its small values (the marks files, the
//! pos graph): bincode under a magic and the version, written atomically
//! (a `tmp-` sibling renamed into place, so a crash never leaves a
//! loadable-looking half file). The frame files and drop notes are
//! `storage::layer`'s, under the same version.

use anyhow::{ensure, Context, Result};
use serde::Serialize;
use std::io::Write;
use std::path::Path;

const MAGIC: &[u8; 4] = b"C8TB";
/// Bump whenever the meaning or layout of ANY tree content changes (older
/// trees are refused; history in git). 13: exact packed keys, the key space
/// and the storage region side in the metadata (plans/exact-keys.md). 14:
/// frame files hold ids and the skeleton only, rows decoded from keys
/// (plans/rows-from-keys.md). 15: NO IDS - a frame file is its new states'
/// keys and cell masks per region with the frame's codebook additions and
/// skeletons, the drop notes masks per (region, key, horizon)
/// (plans/keys-only.md).
pub const FORMAT_VERSION: u32 = 15;

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
