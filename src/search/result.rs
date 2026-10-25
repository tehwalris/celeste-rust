//! `<search-dir>/result.json`: what `rewrite search` decided, as a
//! record, so nothing parses its log (the category runner, `export-ui`).
//! Written at every outcome, before the exit code says it; a run's start
//! removes the previous run's.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// The record's format; a reader refuses another.
pub(crate) const FORMAT: u32 = 1;

pub(crate) fn path(checkpoint_dir: &Path) -> PathBuf {
    checkpoint_dir.join("result.json")
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Verdict {
    /// A concrete win at the optimum (`optimal`, `witness`).
    Optimal,
    /// No win by the horizon at some level's arc phase: the optimum is past
    /// it. Under `--ceiling` an error (the exit code is non-zero).
    Refuted,
    /// The arc bound holds, but the exhaustive concrete search found no win
    /// by the horizon. Under `--ceiling` an error.
    NoConcreteWin,
    /// `--no-witness`: the last level's arc bound only.
    BoundOnly,
    /// The level -1 table could not be built (`error`): rerun without
    /// `--level-minus-one`. The exit code is non-zero.
    LevelMinusOneRefused,
}

/// One level of `--level`, in order.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct LevelResult {
    pub spec: String,
    /// Its tree, under the search's directory (`level00`).
    pub dir: String,
    /// The forward's wall time in this run (0 for a reused tree), seconds.
    pub(crate) forward_s: f64,
    /// The tree's first win, in search steps.
    pub(crate) first_win: Option<u32>,
    /// The arc phase's bound, in frames (`None`: the horizon refuted).
    pub arc_bound: Option<u32>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SearchResult {
    pub(crate) format: u32,
    pub(crate) room: (i16, i16),
    /// `--level`, as given (`r0sxn,r0sx`).
    pub(crate) level: String,
    /// The horizon in frames, and whether it was a `--ceiling`.
    pub horizon: u32,
    pub ceiling: bool,
    /// Search steps per frame (2 under `--split-frame`): the trees, the
    /// first wins and the marks count steps.
    pub(crate) steps_per_frame: u32,
    /// The levels the search reached, in order.
    pub levels: Vec<LevelResult>,
    pub verdict: Verdict,
    /// The concrete optimum, frames.
    pub optimal: Option<u32>,
    /// Its inputs (`witness_file` holds them too).
    pub(crate) witness: Option<Vec<u8>>,
    pub witness_file: Option<String>,
    /// The search's wall time and peak resident set (`VmHWM`, GB).
    pub wall_s: f64,
    pub(crate) peak_gb: f64,
    /// Why the search stopped short (the level -1 refusal's error).
    pub error: Option<String>,
    /// A DUAL search's (`rewrite dual`; absent otherwise): `optimal` is the
    /// float half's exit (rule (b)), the levels' dirs under `fixed/`, `float/`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub dual: Option<DualResult>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct DualResult {
    /// The float half's arithmetic and tool (`float-celia`), and its start.
    pub(crate) float: String,
    pub(crate) float_loading_jank: Option<usize>,
    /// The frame real play (the fixed half) leaves on, with the witness.
    pub fixed_exit: Option<u32>,
}

impl SearchResult {
    /// Atomic: a reader never sees a partial record.
    pub(crate) fn write(&self, checkpoint_dir: &Path) -> Result<()> {
        let p = path(checkpoint_dir);
        let tmp = p.with_extension("json.tmp");
        std::fs::write(&tmp, serde_json::to_string(self)? + "\n").with_context(|| format!("writing {}", tmp.display()))?;
        std::fs::rename(&tmp, &p).with_context(|| format!("writing {}", p.display()))
    }

    pub(crate) fn read(checkpoint_dir: &Path) -> Result<Self> {
        let p = path(checkpoint_dir);
        let text = std::fs::read_to_string(&p).with_context(|| format!("reading {} (a `rewrite search` directory records its result there)", p.display()))?;
        let r: SearchResult = serde_json::from_str(&text).with_context(|| format!("parsing {}", p.display()))?;
        anyhow::ensure!(r.format == FORMAT, "{}: format {}, this binary reads {FORMAT}", p.display(), r.format);
        Ok(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The record round-trips through its file; the verdicts are snake case
    /// (the runner reads them by name).
    #[test]
    fn a_result_round_trips() {
        let dir = crate::testutil::TestDir::new("result");
        std::fs::create_dir_all(&dir).unwrap();
        let r = SearchResult {
            format: FORMAT,
            room: (4, 2),
            level: "r0sxn,r0sx".into(),
            horizon: 50,
            ceiling: false,
            steps_per_frame: 1,
            levels: vec![LevelResult { spec: "r0sxn".into(), dir: "level00".into(), forward_s: 1.5, first_win: Some(47), arc_bound: Some(47) }],
            verdict: Verdict::LevelMinusOneRefused,
            optimal: None,
            witness: None,
            witness_file: None,
            wall_s: 2.0,
            peak_gb: 0.5,
            error: Some("x".into()),
            dual: None,
        };
        r.write(&dir).unwrap();
        assert_eq!(SearchResult::read(&dir).unwrap(), r);
        assert!(std::fs::read_to_string(path(&dir)).unwrap().contains("\"level_minus_one_refused\""));
        assert!(!std::fs::read_to_string(path(&dir)).unwrap().contains("dual"), "a single search's record is as before");
        let d = SearchResult { dual: Some(DualResult { float: "float-celia".into(), float_loading_jank: Some(1), fixed_exit: Some(115) }), ..r };
        d.write(&dir).unwrap();
        assert_eq!(SearchResult::read(&dir).unwrap(), d);
    }
}
