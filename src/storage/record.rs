//! THE TREE'S RECORD (`<level>/tree.txt`): what a tree is a function of and
//! how far it is trusted, a `key value` line each, the file rewritten whole
//! and atomically (a `.tmp` renamed into place):
//! * the settings that change the tree, its level included
//!   (`Settings::write`; read back by `Settings::of_tree`, checked by
//!   `Settings::check`);
//! * `level_minus_one`: the level -1 filter its frames were built under,
//!   with its table's fingerprint (`frame::TreeFilter`; the table itself is
//!   `l1-table.bin` beside the record: `level_minus_one::TABLE_FILE`);
//! * `filtered_for`: a ladder level's, the coarser level's marks and the
//!   horizon its forward was filtered for (`search::driver`);
//! * `done`: THE TRUST MARKER, the last frame whose files are all complete
//!   and the transfer table's trusted length (`set_done`: a frame's LAST
//!   write; a frame past it is a killed run's, `resume` discards it).
//!
//! Its first line names its format (`settings::VERSION`): a tree without a
//! record, or with another format, is refused.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// The record's file in a level's tree.
pub(crate) const FILE: &str = "tree.txt";

/// The lines that are not settings: what the tree's forward wrote.
pub(crate) const TREE_KEYS: [&str; 3] = ["level_minus_one", "filtered_for", "done"];

/// A tree's record: its lines in order.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct Record {
    lines: Vec<(String, String)>,
}

pub(crate) fn path(dir: &Path) -> PathBuf {
    dir.join(FILE)
}

fn head() -> String {
    format!("tree record {}", crate::settings::VERSION)
}

impl Record {
    /// The tree's record; `None` without one.
    pub(crate) fn read(dir: &Path) -> Result<Option<Record>> {
        let p = path(dir);
        let text = match std::fs::read_to_string(&p) {
            Ok(s) => s,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e).with_context(|| p.display().to_string()),
        };
        let mut lines = text.lines();
        let h = lines.next().unwrap_or("");
        anyhow::ensure!(h == head(), "{}: {h:?} is not a tree record of format {} (a tree from another format: delete it)", p.display(), crate::settings::VERSION);
        let mut rec = Record::default();
        for l in lines {
            let (k, v) = l.split_once(' ').with_context(|| format!("{}: not a `key value` line: {l:?}", p.display()))?;
            anyhow::ensure!(rec.get(k).is_none(), "{}: `{k}` twice", p.display());
            rec.lines.push((k.to_string(), v.to_string()));
        }
        Ok(Some(rec))
    }

    /// The tree's record, refused without one.
    pub(crate) fn of(dir: &Path) -> Result<Record> {
        Record::read(dir)?.with_context(|| format!("{}: the tree has no record ({FILE}): its settings, filter and trusted frames are unknown - delete it", dir.display()))
    }

    pub(crate) fn get(&self, key: &str) -> Option<&str> {
        self.lines.iter().find(|(k, _)| k == key).map(|(_, v)| v.as_str())
    }

    /// Every line, in order.
    pub(crate) fn lines(&self) -> impl Iterator<Item = (&str, &str)> {
        self.lines.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    /// `key`'s value: replaced in place, or a line appended.
    pub(crate) fn set(&mut self, key: &str, value: impl Into<String>) {
        let value = value.into();
        debug_assert!(!key.contains(' ') && !key.is_empty() && !value.contains('\n'), "a record line {key:?} {value:?}");
        match self.lines.iter_mut().find(|(k, _)| k == key) {
            Some(l) => l.1 = value,
            None => self.lines.push((key.to_string(), value)),
        }
    }

    /// Keep only the lines `keep` says to.
    pub(crate) fn retain(&mut self, keep: impl Fn(&str) -> bool) {
        self.lines.retain(|(k, _)| keep(k));
    }

    /// Write it as the tree's record (atomically).
    pub(crate) fn write(&self, dir: &Path) -> Result<()> {
        std::fs::create_dir_all(dir).with_context(|| dir.display().to_string())?;
        let mut text = head();
        text.push('\n');
        for (k, v) in &self.lines {
            text += &format!("{k} {v}\n");
        }
        let tmp = dir.join("tree.tmp");
        std::fs::write(&tmp, text).with_context(|| tmp.display().to_string())?;
        std::fs::rename(&tmp, path(dir)).with_context(|| path(dir).display().to_string())?;
        Ok(())
    }
}

/// Set `key` in the tree's record (made when there is none).
pub(crate) fn update(dir: &Path, key: &str, value: impl Into<String>) -> Result<()> {
    let mut rec = Record::read(dir)?.unwrap_or_default();
    rec.set(key, value);
    rec.write(dir)
}

/// The trust marker: the last frame whose files are all complete - its
/// frame and edge files, both written before it - and the transfer table's
/// trusted length.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Done {
    pub(crate) frame: u32,
    pub(crate) xfers: usize,
}

/// The tree's trust marker; `None`: no frame trusted (or no record).
pub(crate) fn done(dir: &Path) -> Result<Option<Done>> {
    let Some(rec) = Record::read(dir)? else { return Ok(None) };
    let Some(v) = rec.get("done") else { return Ok(None) };
    let bad = || anyhow::anyhow!("{}: not a trust marker: `done {v}`", path(dir).display());
    match v.split_whitespace().collect::<Vec<_>>()[..] {
        [frame, "xfers", n] => Ok(Some(Done { frame: frame.parse().map_err(|_| bad())?, xfers: n.parse().map_err(|_| bad())? })),
        _ => Err(bad()),
    }
}

/// The last frame whose files are complete; `None`: none (or no record).
pub(crate) fn done_frame(dir: &Path) -> Option<u32> {
    done(dir).ok().flatten().map(|d| d.frame)
}

/// Trust frame `frame` and the first `xfers` transfers: the frame's LAST write.
pub(crate) fn set_done(dir: &Path, frame: u32, xfers: usize) -> Result<()> {
    update(dir, "done", format!("{frame} xfers {xfers}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A record reads back as written, line order kept; a line is replaced
    /// in place; the trust marker is its `done` line; another format, a
    /// repeated key or a bare line is refused.
    #[test]
    fn a_record_reads_back_and_refuses_another_format() {
        let dir = std::env::temp_dir().join(format!("celeste-record-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(Record::read(&dir).expect("none"), None);
        assert_eq!(done(&dir).expect("none"), None);
        let mut rec = Record::default();
        rec.set("room", "1,0");
        rec.set("level", "r0sxh");
        rec.write(&dir).expect("write");
        set_done(&dir, 7, 12).expect("done");
        update(&dir, "room", "2,0").expect("update");
        let back = Record::read(&dir).expect("read").expect("some");
        assert_eq!(back.lines().collect::<Vec<_>>(), [("room", "2,0"), ("level", "r0sxh"), ("done", "7 xfers 12")]);
        assert_eq!(done(&dir).expect("done"), Some(Done { frame: 7, xfers: 12 }));
        for bad in ["tree record 0\nroom 1,0\n", "tree settings 2\nroom 1,0\n", "tree record 3\nroom 1,0\nroom 2,0\n", "tree record 3\nroom\n"] {
            let bad = bad.replace("tree record 3", &head());
            std::fs::write(path(&dir), &bad).expect("write");
            assert!(Record::read(&dir).is_err(), "{bad:?} is refused");
        }
        std::fs::remove_dir_all(&dir).expect("clean up");
    }
}
