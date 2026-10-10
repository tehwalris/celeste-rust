//! THE TREE'S SETTINGS: every setting that changes what a level's tree holds,
//! recorded in the tree (`<level dir>/settings.txt`) when its frame 0 is
//! written, and compared with the process's on every resume or reuse - a
//! difference is an error, never a silently mixed tree.
//!
//! The level -1 filter is recorded apart (`frame::TreeFilter`,
//! `level_minus_one.txt`): a raise changes it, and a tree serves every
//! horizon up to it. `CELESTE_TRIM_ROWS` is not a setting (the files say
//! which rows are trimmed), nor are threads or diagnostics.
//!
//! The level is the one the tree's rows are widened at (the caller's
//! `Level` value): a tree used at another level is refused like any other
//! difference.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// The record's format: a tree without one, or with another, is refused
/// where its settings are checked (tools/fixtures.sh rebuilds its fixtures
/// on a change).
pub const VERSION: u32 = 1;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Settings {
    /// The start room (`CELESTE_START_ROOM`, `--room`).
    pub room: (i16, i16),
    /// A synthetic win rectangle `(x0, x1, y0, y1)` (`--win-at`).
    pub win_at: Option<(i16, i16, i16, i16)>,
    /// Two search steps a frame (`CELESTE_SPLIT_FRAME`).
    pub split_frame: bool,
    /// The kernels' region grid `(px, S)` (`CELESTE_REGION`; `None`: off).
    pub region: Option<(i32, i32)>,
    /// The storage region side (`CELESTE_STORAGE_REGION`).
    pub storage_region: u32,
    pub hundred: bool,
    pub nodiag: bool,
    pub onlydiag: bool,
    pub gemskip: bool,
    /// The loading frame's object J (`CELESTE_LOADING_JANK`; `None`: IL load).
    pub loading_jank: Option<usize>,
    /// The balloons' fixed phases (`CELESTE_BALLOON_SEEDS`).
    pub balloon_seeds: Option<String>,
}

impl Settings {
    /// This process's settings.
    pub fn current() -> Settings {
        Settings {
            room: crate::game_runner::start_room(),
            win_at: crate::abstraction::synthetic_win_rect(),
            split_frame: crate::frame::steps_per_frame() == 2,
            region: crate::trace::kernel::region_grid().map(|g| (g.px, g.speed)),
            storage_region: crate::storage::geometry().side,
            hundred: crate::game_runner::hundred(),
            nodiag: crate::trace::cart::nodiag(),
            onlydiag: crate::trace::cart::onlydiag(),
            gemskip: crate::game_runner::gemskip(),
            loading_jank: crate::game_runner::loading_jank(),
            balloon_seeds: crate::knobs::value("CELESTE_BALLOON_SEEDS"),
        }
    }

    /// The record's lines, `(key, value)`, in a fixed order, for a tree of
    /// `level`.
    fn lines(&self, level: crate::abstraction::Level) -> Vec<(&'static str, String)> {
        let opt = |v: Option<String>| v.unwrap_or_else(|| "none".to_string());
        vec![
            ("room", format!("{},{}", self.room.0, self.room.1)),
            ("win_at", opt(self.win_at.map(|(x0, x1, y0, y1)| format!("{x0}..{x1},{y0}..{y1}")))),
            ("split_frame", u8::from(self.split_frame).to_string()),
            ("region", opt(self.region.map(|(px, s)| format!("{px},{s}")))),
            ("storage_region", self.storage_region.to_string()),
            ("hundred", u8::from(self.hundred).to_string()),
            ("nodiag", u8::from(self.nodiag).to_string()),
            ("onlydiag", u8::from(self.onlydiag).to_string()),
            ("gemskip", u8::from(self.gemskip).to_string()),
            ("loading_jank", opt(self.loading_jank.map(|j| j.to_string()))),
            ("balloon_seeds", opt(self.balloon_seeds.clone())),
            ("level", level.to_string()),
        ]
    }

    fn text(&self, level: crate::abstraction::Level) -> String {
        let mut s = format!("tree settings {VERSION}\n");
        for (k, v) in self.lines(level) {
            s += &format!("{k} {v}\n");
        }
        s
    }

    /// Record these, for a tree widened at `level`, as the tree's (atomic).
    pub fn write(&self, dir: &Path, level: crate::abstraction::Level) -> Result<()> {
        std::fs::create_dir_all(dir)?;
        let tmp = dir.join("settings.tmp");
        std::fs::write(&tmp, self.text(level))?;
        std::fs::rename(&tmp, path(dir))?;
        Ok(())
    }

    /// The tree in `dir` was built under these settings, at `level`: else an
    /// error that names every difference.
    pub fn check(&self, dir: &Path, level: crate::abstraction::Level) -> Result<()> {
        let p = path(dir);
        let recorded = match std::fs::read_to_string(&p) {
            Ok(s) => s,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => anyhow::bail!(
                "{}: the tree records no settings (built before {}): its room, category, split frame, regions and level are unknown - delete it",
                dir.display(),
                p.display()
            ),
            Err(e) => return Err(e).with_context(|| p.display().to_string()),
        };
        let mut rec = recorded.lines();
        let head = rec.next().unwrap_or("");
        anyhow::ensure!(head == format!("tree settings {VERSION}"), "{}: {head:?} is not a settings record of format {VERSION}", p.display());
        let rec: std::collections::BTreeMap<&str, &str> = rec.filter_map(|l| l.split_once(' ')).collect();
        let mine = self.lines(level);
        let mut diffs: Vec<String> = mine
            .iter()
            .filter(|(k, v)| rec.get(k) != Some(&v.as_str()))
            .map(|(k, v)| format!("{k}: the tree's {}, this run's {v}", rec.get(k).copied().unwrap_or("(missing)")))
            .collect();
        diffs.extend(rec.keys().filter(|k| !mine.iter().any(|(m, _)| m == *k)).map(|k| format!("{k}: in the tree's record, not a setting")));
        anyhow::ensure!(
            diffs.is_empty(),
            "{}: the tree was built under other settings than this run's:\n  {}\n(a tree's frames are one search: delete it, or run with its settings)",
            dir.display(),
            diffs.join("\n  ")
        );
        Ok(())
    }
}

/// The record's file in a level's tree.
pub fn path(dir: &Path) -> PathBuf {
    dir.join("settings.txt")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> Settings {
        Settings {
            room: (6, 2),
            win_at: None,
            split_frame: false,
            region: Some((16, 6)),
            storage_region: 8,
            hundred: true,
            nodiag: false,
            onlydiag: false,
            gemskip: false,
            loading_jank: Some(2),
            balloon_seeds: None,
        }
    }

    /// The record round-trips; every differing setting is named; a tree
    /// without a record is refused.
    #[test]
    fn a_tree_is_refused_under_other_settings() {
        let dir = std::env::temp_dir().join(format!("celeste-settings-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("mkdir");
        let lv = crate::abstraction::Level::parse("r0sxhf").expect("level");
        let e = sample().check(&dir, lv).expect_err("no record");
        assert!(e.to_string().contains("records no settings"), "{e}");
        sample().write(&dir, lv).expect("write");
        sample().check(&dir, lv).expect("the same settings");
        let other = Settings { hundred: false, split_frame: true, win_at: Some((8, 8, 64, 64)), ..sample() };
        let e = other.check(&dir, crate::abstraction::Level::parse("r0sxh").expect("level")).expect_err("other settings").to_string();
        for k in ["hundred: the tree's 1, this run's 0", "split_frame: the tree's 0, this run's 1", "win_at: the tree's none, this run's 8..8,64..64", "level: the tree's r0sxhf, this run's r0sxh"] {
            assert!(e.contains(k), "{k} not in {e}");
        }
        assert!(!e.contains("room:"), "{e}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}
