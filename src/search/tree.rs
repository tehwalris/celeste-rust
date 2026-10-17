//! A FINISHED TREE as the commands that read one open it (`ckhash`,
//! `census`, `check ...`, `bench ...`): its directory, its settings and
//! level from its record (`Settings::of_tree`, never the command line), and
//! its last finished frame (`done.txt`, the forward's trust marker).

use std::path::{Path, PathBuf};

use anyhow::Result;

use crate::abstraction::Level;
use crate::settings::Settings;

pub struct TreeDir {
    pub dir: PathBuf,
    /// The tree's settings, with the run's own (threads, diagnostics,
    /// `--l1-table`).
    pub settings: Settings,
    pub level: Level,
    /// The last finished frame (search step).
    pub last: u32,
}

impl TreeDir {
    /// The tree in `dir`, the run's own settings from `run`.
    pub fn open(dir: &Path, run: &Settings) -> Result<TreeDir> {
        let (settings, level) = Settings::of_tree(dir, run)?;
        let last = crate::storage::edges::done_frame(&dir.join("edges")).ok_or_else(|| anyhow::anyhow!("{}: no finished frame", dir.display()))?;
        Ok(TreeDir { dir: dir.to_path_buf(), settings, level, last })
    }

    /// Frame `f` of the tree (`None`: its last); past the last is refused.
    pub fn frame(&self, f: Option<u32>) -> Result<u32> {
        let f = f.unwrap_or(self.last);
        anyhow::ensure!(f <= self.last, "{}: frame {f} is past the tree's last finished one, {}", self.dir.display(), self.last);
        Ok(f)
    }

    pub fn float(&self) -> bool {
        self.settings.arith != crate::settings::Arith::Fixed
    }
}
