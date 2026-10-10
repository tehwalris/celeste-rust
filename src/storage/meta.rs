//! Per frame, what the storage created: the shapes it numbered, the
//! entries it appended (region, entry number, key) and what the key space
//! gained (codes with their indices, bit runs with their places:
//! `exact::KeyAdditions`). With the frame files' id columns (the cells) they
//! ARE the visited set: a resume rebuilds it from them, and the backward
//! resolves an id to its (shape, key, cell) with no frame file read
//! (`marks::Resolver`). Each records the storage region side it was built
//! with; a reader on another side refuses it.
//!
//! `frames/f{frame}/meta.bin` for a frame's wave; a raise of the frame
//! adds `meta.r{first_seq}.bin` beside it. Each entry carries its
//! number, so the files may be read in any order.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use super::visited::Key;

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub struct FrameMeta {
    /// The storage region side (`storage::geometry`) the tree is built with.
    pub side: u32,
    /// `(index, hash)` of the shapes numbered at this frame.
    pub shapes: Vec<(u32, u64)>,
    /// `(region, entry, key)` of the entries appended, by (region, entry).
    pub entries: Vec<(u32, u32, Key)>,
    /// What the key space gained.
    pub keys: celeste_engine::exact::KeyAdditions,
}

impl FrameMeta {
    /// An empty record for the process's storage geometry.
    pub fn new() -> Self {
        FrameMeta { side: super::geometry().side, ..Default::default() }
    }
}

/// The frame's metadata file of a wave (`raised`: a raise's, by its first seq).
pub fn path(dir: &Path, frame: u32, raised: Option<u32>) -> PathBuf {
    let fdir = dir.join("frames").join(format!("f{frame:03}"));
    match raised {
        None => fdir.join("meta.bin"),
        Some(seq) => fdir.join(format!("meta.r{seq:04}.bin")),
    }
}

pub fn save(dir: &Path, frame: u32, raised: Option<u32>, meta: &FrameMeta) -> Result<()> {
    crate::search::checkpoint::save_value_to(&path(dir, frame, raised), meta)
}

/// Every metadata file of frame `frame` (the wave's and its raises').
pub fn load_frame(dir: &Path, frame: u32) -> Result<Vec<FrameMeta>> {
    let fdir = dir.join("frames").join(format!("f{frame:03}"));
    let mut paths: Vec<PathBuf> = std::fs::read_dir(&fdir)
        .with_context(|| fdir.display().to_string())?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.file_name().and_then(|n| n.to_str()).is_some_and(|n| n.starts_with("meta") && n.ends_with(".bin")))
        .collect();
    paths.sort();
    anyhow::ensure!(paths.first().is_some_and(|p| p.ends_with("meta.bin")), "{}: no meta.bin (a tree from before storage v2?)", fdir.display());
    let side = super::geometry().side;
    paths
        .iter()
        .map(|p| {
            let m: FrameMeta = crate::search::checkpoint::load_value_from(p).with_context(|| p.display().to_string())?;
            anyhow::ensure!(m.side == side, "{}: built with storage regions of {} cells, this process uses {side} (CELESTE_STORAGE_REGION)", p.display(), m.side);
            Ok(m)
        })
        .collect()
}

/// The key space of the metadata `metas` (any order).
pub fn key_space(metas: &[FrameMeta]) -> Result<celeste_engine::exact::KeySpace> {
    let mut all = celeste_engine::exact::KeyAdditions::default();
    for m in metas {
        all.extend(m.keys.clone());
    }
    celeste_engine::exact::KeySpace::from_additions(all)
}

/// The metadata of frames `0..=last`, frame by frame.
pub fn load_tree(dir: &Path, last: u32) -> Result<Vec<FrameMeta>> {
    let mut out = Vec::new();
    for f in 0..=last {
        out.extend(load_frame(dir, f)?);
    }
    Ok(out)
}

/// The key space of the tree in `dir` through frame `last` (frames past a
/// dead frontier have none).
pub fn tree_keys(dir: &Path, last: u32) -> Result<celeste_engine::exact::KeySpace> {
    let mut metas = Vec::new();
    for f in 0..=last {
        if dir.join("frames").join(format!("f{f:03}")).is_dir() {
            metas.extend(load_frame(dir, f)?);
        }
    }
    key_space(&metas).with_context(|| format!("{}: the key space", dir.display()))
}
