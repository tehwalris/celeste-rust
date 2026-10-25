//! A FINISHED TREE as the commands that read one open it: its run (settings
//! and level) from its record (`Run::of_tree`, never the command line), its
//! last finished frame (the record's trust marker, `record::done`).

use std::path::{Path, PathBuf};

use anyhow::Result;

use crate::settings::{Run, Settings};

pub struct TreeDir {
    pub dir: PathBuf,
    /// The tree's settings and level, with the run's own settings
    /// (threads, diagnostics).
    pub run: Run,
    /// The last finished frame (search step).
    pub(crate) last: u32,
}

impl TreeDir {
    /// The tree in `dir`, the run's own settings from `run`.
    pub fn open(dir: &Path, run: &Settings) -> Result<TreeDir> {
        let tree = Run::of_tree(dir, run)?;
        let last = crate::storage::record::done_frame(dir).ok_or_else(|| anyhow::anyhow!("{}: no finished frame", dir.display()))?;
        Ok(TreeDir { dir: dir.to_path_buf(), run: tree, last })
    }

    /// Frame `f` of the tree (`None`: its last); past the last is refused.
    pub fn frame(&self, f: Option<u32>) -> Result<u32> {
        let f = f.unwrap_or(self.last);
        anyhow::ensure!(f <= self.last, "{}: step {f} is past the tree's last finished one, {}", self.dir.display(), self.last);
        Ok(f)
    }

    pub fn float(&self) -> bool {
        self.run.settings.arith != crate::settings::Arith::Fixed
    }
}
