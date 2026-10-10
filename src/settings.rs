//! THE RUN'S SETTINGS: every setting of a run, from its COMMAND LINE
//! (`SettingsArgs`, flattened into every binary's arguments), in ONE struct
//! installed once per process (`install`, before anything reads it; `get`).
//! Nothing reads the environment: a `CELESTE_*` variable is refused at
//! startup (`refuse_env`, with the flag that replaced it).
//!
//! The settings that change what a level's tree holds are RECORDED in the
//! tree (`<level dir>/settings.txt`) when its frame 0 is written, and
//! compared with the run's on every resume or reuse (`check`): a difference
//! is an error, never a silently mixed tree. A resume does NOT default the
//! flags to the recorded ones: the same command line would then mean another
//! search depending on what is on disk (a deleted tree rerun by "the same"
//! command would silently change category).
//!
//! The level -1 filter is recorded apart (`frame::TreeFilter`,
//! `level_minus_one.txt`): a raise changes it, and a tree serves every
//! horizon up to it. Threads, trimming and the diagnostics never change a
//! tree. The level the tree's rows are widened at (the engine's `Level`) is
//! a line of the record: a tree used at another level is refused like any
//! other difference.
//!
//! Readers deep in the code (the cart patches, the kernel build, the
//! storage geometry) read the installed struct through their accessors
//! (`game_runner::start_room`, `frame::steps_per_frame`, ...); what needs
//! two configurations in one process takes a `&Settings` (`cart::sources`,
//! `RefEngine::with`: the concrete phase's seeded engines, the tests).

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// The record's format: a tree without one, or with another, is refused
/// where its settings are checked (tools/fixtures.sh rebuilds its fixtures
/// on a change).
pub const VERSION: u32 = 1;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Settings {
    // RECORDED in a tree.
    /// The start room (`--room`).
    pub room: (i16, i16),
    /// A synthetic win rectangle `(x0, x1, y0, y1)` (`--win-at`).
    pub win_at: Option<(i16, i16, i16, i16)>,
    /// Two search steps a frame (`--split-frame`).
    pub split_frame: bool,
    /// The kernels' region grid `(px, S)` (`--region`; `None`: off).
    pub region: Option<(i32, i32)>,
    /// The storage region side (`--storage-region`).
    pub storage_region: u32,
    pub hundred: bool,
    pub nodiag: bool,
    pub onlydiag: bool,
    pub gemskip: bool,
    /// The loading frame's object J (`--loading-jank`; `None`: IL load).
    pub loading_jank: Option<usize>,
    /// The balloons' fixed phases (`--balloon-seeds`).
    pub balloon_seeds: Option<String>,
    // NOT recorded.
    /// The level -1 filter `(H, S)`, H in search steps (`--level-minus-one`;
    /// the tree records it as `TreeFilter`).
    pub level_minus_one: Option<(u32, i32)>,
    /// The balloons' phases of the concrete phase only.
    pub concrete_balloon_seeds: Option<String>,
    /// Old frames keep keys, cells and wins only (`--trim-rows`).
    pub trim_rows: bool,
    /// Worker threads (`None`: one per physical core).
    pub threads: Option<usize>,
    /// The forward stops below this much free disk (plus 3x a frame).
    pub min_free_gb: u64,
    /// The level -1 table pinned by its file (the fixtures).
    pub l1_table: Option<PathBuf>,
    pub diag: Diagnostics,
}

/// Self-checks, logs and dumps: none changes a result.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Diagnostics {
    /// Every emitted row's key recomputed by the boundary; every unit's copy
    /// of a new state compared (claims off).
    pub key_check: bool,
    /// Kernels keep their graphs: a decline names its premise.
    pub kernel_explain: bool,
    /// The instruction-mix report per kernel, into this dir.
    pub kernel_mix: Option<PathBuf>,
    /// The kernels' census and slice counters (`bench-frame --metrics`).
    pub kernel_metrics: bool,
    /// The kernel build's and the lattice walk's progress.
    pub build_trace: bool,
    /// The wave workers' phase timings.
    pub phases: bool,
    /// Capture the wave's emissions into this dir (at this frame only).
    pub emit_capture: Option<(PathBuf, Option<u32>)>,
    /// The lattice walk traces only these regions (a probe).
    pub walk_regions: Option<Vec<(i16, i16)>>,
    /// The level -1 probe prints the fastest path from this cell.
    pub l1_path: Option<(i16, i16)>,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            room: (1, 0),
            win_at: None,
            split_frame: false,
            region: Some((16, 6)),
            storage_region: 8,
            hundred: false,
            nodiag: false,
            onlydiag: false,
            gemskip: false,
            loading_jank: None,
            balloon_seeds: None,
            level_minus_one: None,
            concrete_balloon_seeds: None,
            trim_rows: false,
            threads: None,
            min_free_gb: 20,
            l1_table: None,
            diag: Diagnostics::default(),
        }
    }
}

static SETTINGS: std::sync::OnceLock<Settings> = std::sync::OnceLock::new();

/// Install the process's settings: once, before anything reads them (a
/// second install of other settings, or one after a read, panics).
pub fn install(s: Settings) {
    let want = s.clone();
    let got = SETTINGS.get_or_init(|| s);
    assert!(*got == want, "settings installed twice, or read before they were installed: {got:?} then {want:?}");
}

/// The process's settings (the defaults when none were installed: a test).
pub fn get() -> &'static Settings {
    SETTINGS.get_or_init(Settings::default)
}

impl Settings {
    /// The record's lines, `(key, value)`, in a fixed order: what changes a
    /// tree. `level`: the tree's level spec.
    fn recorded(&self, level: &str) -> Vec<(&'static str, String)> {
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

    /// What the cart (so the kernels and the level -1 table) is a function
    /// of, besides the sources: the cache key of a level -1 table.
    pub fn cart_key(&self) -> String {
        let mut s: Vec<String> = self.recorded("").into_iter().filter(|(k, _)| *k != "level").map(|(k, v)| format!("{k}={v}")).collect();
        s.push(format!("walk_regions={:?}", self.diag.walk_regions));
        s.join(" ")
    }

    fn text(&self, level: &str) -> String {
        let mut s = format!("tree settings {VERSION}\n");
        for (k, v) in self.recorded(level) {
            s += &format!("{k} {v}\n");
        }
        s
    }

    /// Record these, for a tree widened at `level`, as the tree's (atomic).
    pub fn write(&self, dir: &Path, level: crate::abstraction::Level) -> Result<()> {
        std::fs::create_dir_all(dir)?;
        let tmp = dir.join("settings.tmp");
        std::fs::write(&tmp, self.text(&level.to_string()))?;
        std::fs::rename(&tmp, path(dir))?;
        Ok(())
    }

    /// The tree in `dir` was built under these settings, at `level`: else an
    /// error that names every difference.
    pub fn check(&self, dir: &Path, level: crate::abstraction::Level) -> Result<()> {
        self.check_at(dir, &level.to_string())
    }

    /// The same, whatever the tree's level (a diagnostic that reads the tree
    /// without a level).
    pub fn check_any_level(&self, dir: &Path) -> Result<()> {
        let rec = std::fs::read_to_string(path(dir)).unwrap_or_default();
        let level = rec.lines().find_map(|l| l.strip_prefix("level ")).unwrap_or("").to_string();
        self.check_at(dir, &level)
    }

    fn check_at(&self, dir: &Path, level: &str) -> Result<()> {
        let p = path(dir);
        let recorded = match std::fs::read_to_string(&p) {
            Ok(s) => s,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => anyhow::bail!(
                "{}: the tree records no settings (built before {}): its room, category, split frame, regions and level are unknown - delete it, or record the ones it was built under (`rewrite adopt-settings` with its flags)",
                dir.display(),
                p.display()
            ),
            Err(e) => return Err(e).with_context(|| p.display().to_string()),
        };
        let mut rec = recorded.lines();
        let head = rec.next().unwrap_or("");
        anyhow::ensure!(head == format!("tree settings {VERSION}"), "{}: {head:?} is not a settings record of format {VERSION}", p.display());
        let rec: std::collections::BTreeMap<&str, &str> = rec.filter_map(|l| l.split_once(' ')).collect();
        let mine = self.recorded(level);
        let mut diffs: Vec<String> = mine
            .iter()
            .filter(|(k, v)| rec.get(k) != Some(&v.as_str()))
            .map(|(k, v)| format!("{k}: the tree's {}, this run's {v}", rec.get(k).copied().unwrap_or("(missing)")))
            .collect();
        diffs.extend(rec.keys().filter(|k| !mine.iter().any(|(m, _)| m == *k)).map(|k| format!("{k}: in the tree's record, not a setting")));
        anyhow::ensure!(
            diffs.is_empty(),
            "{}: the tree was built under other settings than this run's:\n  {}\n(a tree's frames are one search: delete it, or run with its flags)",
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

fn pair<T: std::str::FromStr>(s: &str, what: &str) -> std::result::Result<(T, T), String> {
    let (a, b) = s.split_once(',').ok_or_else(|| format!("{what}: \"a,b\", not {s:?}"))?;
    let p = |t: &str| t.trim().parse::<T>().map_err(|_| format!("{what}: {t:?} is not a number"));
    Ok((p(a)?, p(b)?))
}

/// `x,y` or `x0..x1,y0..y1` (inclusive) -> `(x0, x1, y0, y1)`.
fn parse_win_at(s: &str) -> std::result::Result<(i16, i16, i16, i16), String> {
    let (x, y) = s.split_once(',').ok_or_else(|| format!("--win-at: \"x,y\" or \"x0..x1,y0..y1\", not {s:?}"))?;
    let range = |t: &str| -> std::result::Result<(i16, i16), String> {
        let one = |u: &str| u.trim().parse::<i16>().map_err(|e| format!("--win-at: {u:?}: {e}"));
        let (lo, hi) = match t.split_once("..") {
            Some((lo, hi)) => (one(lo)?, one(hi)?),
            None => (one(t)?, one(t)?),
        };
        if lo > hi {
            return Err(format!("--win-at: {lo}..{hi} is empty"));
        }
        Ok((lo, hi))
    };
    let ((x0, x1), (y0, y1)) = (range(x)?, range(y)?);
    Ok((x0, x1, y0, y1))
}

/// THE FLAGS (every binary flattens them; `global`: given anywhere on a
/// `rewrite` command line).
#[derive(clap::Args, Clone, Debug)]
pub struct SettingsArgs {
    /// The start room "x,y".
    #[arg(long, global = true, default_value = "1,0", value_parser = |s: &str| pair::<i16>(s, "--room"))]
    pub room: (i16, i16),
    /// A synthetic win: the player at "x,y", or in "x0..x1,y0..y1" (a test
    /// configuration: a tree under it is another search).
    #[arg(long, global = true, value_parser = parse_win_at)]
    pub win_at: Option<(i16, i16, i16, i16)>,
    /// Two search steps a frame (the split cart).
    #[arg(long, global = true)]
    pub split_frame: bool,
    /// The kernels' region grid "px,S" (1 <= S <= 7), or `off`.
    #[arg(long, global = true, default_value = "16,6")]
    pub region: String,
    /// The storage region side: 8 or 16 (it must divide the kernels' px).
    #[arg(long, global = true, default_value_t = 8)]
    pub storage_region: u32,
    /// 100%: a room is won only with its berry taken.
    #[arg(long, global = true)]
    pub hundred: bool,
    /// No Diagonal Dashes: a dash may not start diagonal.
    #[arg(long, global = true)]
    pub nodiag: bool,
    /// Only Diagonal Dashes.
    #[arg(long, global = true)]
    pub onlydiag: bool,
    /// The gemskip categories: the orb room won without the orb, one dash after it.
    #[arg(long, global = true)]
    pub gemskip: bool,
    /// The room as real play enters it: its loading frame from object J
    /// (pico8_diff/chain.py), or `none` (the IL load).
    #[arg(long, global = true, default_value = "none")]
    pub loading_jank: String,
    /// The start room's balloons' phases fixed, "s1,s2,..".
    #[arg(long, global = true)]
    pub balloon_seeds: Option<String>,
    /// The balloons' phases of the concrete phase only (a tasdatabase file's
    /// seeds): the witness is one real run under them.
    #[arg(long, global = true)]
    pub concrete_balloon_seeds: Option<String>,
    /// The level -1 filter "H,S": H in search steps, S the table's speed bound.
    #[arg(long, global = true, value_parser = |s: &str| pair::<u32>(s, "--level-minus-one"))]
    pub level_minus_one: Option<(u32, u32)>,
    /// Old frames keep keys, cells and wins only (a big room's tree ~30-40%
    /// smaller; diagnostics that load old rows refuse it).
    #[arg(long, global = true)]
    pub trim_rows: bool,
    /// Worker threads (default: one per physical core).
    #[arg(long, global = true)]
    pub threads: Option<usize>,
    /// Stop the forward below this much free disk (GB, plus 3x the last frame).
    #[arg(long, global = true, default_value_t = 20)]
    pub min_free_gb: u64,
    /// The level -1 table pinned by its FILE (read if it exists, else built
    /// there): the fixtures', never a search's.
    #[arg(long, global = true)]
    pub l1_table: Option<PathBuf>,
    /// DIAGNOSTIC: every emitted row's key recomputed by the boundary, and
    /// every unit's copy of a new state compared (claims off: every copy
    /// reaches the translation).
    #[arg(long, global = true)]
    pub key_check: bool,
    /// DIAGNOSTIC: kernels keep their graphs, so a decline names its premise.
    #[arg(long, global = true)]
    pub kernel_explain: bool,
    /// DIAGNOSTIC: the instruction-mix report per kernel into DIR (keeps the .so files).
    #[arg(long, global = true)]
    pub kernel_mix: Option<PathBuf>,
    /// DIAGNOSTIC: the kernel build's and the lattice walk's progress.
    #[arg(long, global = true)]
    pub build_trace: bool,
    /// DIAGNOSTIC: the wave workers' phase timings.
    #[arg(long, global = true)]
    pub phases: bool,
    /// DIAGNOSTIC: capture the wave's emissions into DIR (`bench-storage`).
    #[arg(long, global = true)]
    pub emit_capture: Option<PathBuf>,
    /// DIAGNOSTIC: capture only this frame (default: every frame).
    #[arg(long, global = true)]
    pub emit_capture_frame: Option<u32>,
    /// PROBE: the lattice walk traces only these regions, "(ix,iy) ..".
    #[arg(long, global = true)]
    pub walk_regions: Option<String>,
    /// DIAGNOSTIC: the level -1 probe prints the table's fastest path from "x,y".
    #[arg(long, global = true, value_parser = |s: &str| pair::<i16>(s, "--l1-path"))]
    pub l1_path: Option<(i16, i16)>,
}

impl SettingsArgs {
    /// The settings these flags give (validated).
    pub fn settings(&self) -> Result<Settings> {
        let region = match self.region.as_str() {
            "off" => None,
            s => {
                let (px, sp) = pair::<i32>(s, "--region").map_err(anyhow::Error::msg)?;
                anyhow::ensure!(px >= 1 && (1..=7).contains(&sp), "--region \"px,S\" with px >= 1 and 1 <= S <= 7, not {s:?}");
                Some((px, sp))
            }
        };
        anyhow::ensure!(matches!(self.storage_region, 8 | 16), "--storage-region {}: 8 or 16", self.storage_region);
        if let Some((px, _)) = region {
            anyhow::ensure!(
                px % self.storage_region as i32 == 0,
                "--storage-region {}: a storage region must nest in one kernel region, and the kernels' regions are {px} px (--region)",
                self.storage_region
            );
        }
        anyhow::ensure!(!(self.nodiag && self.onlydiag), "--nodiag and --onlydiag exclude each other");
        let loading_jank = match self.loading_jank.as_str() {
            "none" => None,
            s => {
                let j: usize = s.trim().parse().map_err(|_| anyhow::anyhow!("--loading-jank: an object index >= 1 or `none`, not {s:?}"))?;
                anyhow::ensure!(j >= 1, "--loading-jank must be >= 1");
                Some(j)
            }
        };
        let level_minus_one = match self.level_minus_one {
            Some((h, s)) => {
                anyhow::ensure!(h < u32::MAX, "--level-minus-one horizon {h}");
                Some((h, i32::try_from(s).context("--level-minus-one speed")?))
            }
            None => None,
        };
        anyhow::ensure!(self.threads != Some(0), "--threads 0");
        let walk_regions = match &self.walk_regions {
            None => None,
            Some(v) => Some(
                v.split_whitespace()
                    .map(|t| pair::<i16>(t.trim_matches(|c| c == '(' || c == ')'), "--walk-regions").map_err(anyhow::Error::msg))
                    .collect::<Result<Vec<_>>>()?,
            ),
        };
        anyhow::ensure!(self.emit_capture_frame.is_none() || self.emit_capture.is_some(), "--emit-capture-frame without --emit-capture");
        Ok(Settings {
            room: self.room,
            win_at: self.win_at,
            split_frame: self.split_frame,
            region,
            storage_region: self.storage_region,
            hundred: self.hundred,
            nodiag: self.nodiag,
            onlydiag: self.onlydiag,
            gemskip: self.gemskip,
            loading_jank,
            balloon_seeds: self.balloon_seeds.clone(),
            level_minus_one,
            concrete_balloon_seeds: self.concrete_balloon_seeds.clone(),
            trim_rows: self.trim_rows,
            threads: self.threads,
            min_free_gb: self.min_free_gb,
            l1_table: self.l1_table.clone(),
            diag: Diagnostics {
                key_check: self.key_check,
                kernel_explain: self.kernel_explain,
                kernel_mix: self.kernel_mix.clone(),
                kernel_metrics: false,
                build_trace: self.build_trace,
                phases: self.phases,
                emit_capture: self.emit_capture.clone().map(|d| (d, self.emit_capture_frame)),
                walk_regions,
                l1_path: self.l1_path,
            },
        })
    }
}

/// A binary's first step: refuse every `CELESTE_*` variable in the
/// environment (the knobs are flags now; a stale script must not run with
/// its knobs silently ignored), naming the flag that replaced each.
/// `CELESTE_FIXTURES` (read by tools/fixtures.sh only) is allowed.
pub fn refuse_env() -> Result<()> {
    const MOVED: &[(&str, &str)] = &[
        ("CELESTE_START_ROOM", "--room X,Y"),
        ("CELESTE_WIN_AT_XY", "--win-at"),
        ("CELESTE_SPLIT_FRAME", "--split-frame"),
        ("CELESTE_REGION", "--region"),
        ("CELESTE_STORAGE_REGION", "--storage-region"),
        ("CELESTE_HUNDRED", "--hundred"),
        ("CELESTE_NODIAG", "--nodiag"),
        ("CELESTE_ONLYDIAG", "--onlydiag"),
        ("CELESTE_GEMSKIP", "--gemskip"),
        ("CELESTE_LOADING_JANK", "--loading-jank"),
        ("CELESTE_BALLOON_SEEDS", "--balloon-seeds"),
        ("CELESTE_CONCRETE_BALLOON_SEEDS", "--concrete-balloon-seeds"),
        ("CELESTE_LEVEL_MINUS_ONE", "--level-minus-one H,S"),
        ("CELESTE_TRIM_ROWS", "--trim-rows"),
        ("CELESTE_THREADS", "--threads"),
        ("CELESTE_MIN_FREE_GB", "--min-free-gb"),
        ("CELESTE_L1_TABLE", "--l1-table"),
        ("CELESTE_KERNEL_KEY_CHECK", "--key-check"),
        ("CELESTE_KERNEL_EXPLAIN", "--kernel-explain"),
        ("CELESTE_KERNEL_MIX", "--kernel-mix"),
        ("CELESTE_KERNEL_METRICS", "bench-frame --metrics"),
        ("CELESTE_BUILD_TRACE", "--build-trace"),
        ("CELESTE_PHASES", "--phases"),
        ("CELESTE_EMIT_CAPTURE", "--emit-capture"),
        ("CELESTE_EMIT_CAPTURE_FRAME", "--emit-capture-frame"),
        ("CELESTE_WALK_REGIONS", "--walk-regions"),
        ("CELESTE_L1_PATH", "--l1-path"),
    ];
    let mut bad: Vec<String> = std::env::vars_os()
        .filter_map(|(k, _)| k.to_str().map(str::to_string))
        .filter(|k| k.starts_with("CELESTE_") && k != "CELESTE_FIXTURES")
        .map(|k| match MOVED.iter().find(|m| m.0 == k) {
            Some((_, flag)) => format!("{k}: now the flag {flag}"),
            None => format!("{k}: no such setting (the flags: --help)"),
        })
        .collect();
    bad.sort();
    anyhow::ensure!(bad.is_empty(), "the environment sets CELESTE_* variables; settings are flags:\n  {}", bad.join("\n  "));
    Ok(())
}

/// A binary's settings from its flags, installed, and logged: one
/// `[settings]` line with every setting not at its default.
pub fn startup(args: &SettingsArgs, adjust: impl FnOnce(&mut Settings)) -> Result<()> {
    refuse_env()?;
    let mut s = args.settings()?;
    adjust(&mut s);
    let d = Settings::default();
    let mut set: Vec<String> = Vec::new();
    let mut note = |name: &str, on: bool, v: String| {
        if on {
            set.push(format!("{name}{v}"));
        }
    };
    note("--room ", s.room != d.room, format!("{},{}", s.room.0, s.room.1));
    note("--win-at ", s.win_at.is_some(), s.win_at.map_or(String::new(), |(x0, x1, y0, y1)| format!("{x0}..{x1},{y0}..{y1}")));
    note("--split-frame", s.split_frame, String::new());
    note("--region ", s.region != d.region, s.region.map_or("off".to_string(), |(p, v)| format!("{p},{v}")));
    note("--storage-region ", s.storage_region != d.storage_region, s.storage_region.to_string());
    note("--hundred", s.hundred, String::new());
    note("--nodiag", s.nodiag, String::new());
    note("--onlydiag", s.onlydiag, String::new());
    note("--gemskip", s.gemskip, String::new());
    note("--loading-jank ", s.loading_jank.is_some(), format!("{}", s.loading_jank.unwrap_or_default()));
    note("--balloon-seeds ", s.balloon_seeds.is_some(), s.balloon_seeds.clone().unwrap_or_default());
    note("--concrete-balloon-seeds ", s.concrete_balloon_seeds.is_some(), s.concrete_balloon_seeds.clone().unwrap_or_default());
    note("--level-minus-one ", s.level_minus_one.is_some(), s.level_minus_one.map_or(String::new(), |(h, v)| format!("{h},{v}")));
    note("--trim-rows", s.trim_rows, String::new());
    note("--threads ", s.threads.is_some(), format!("{}", s.threads.unwrap_or_default()));
    note("--min-free-gb ", s.min_free_gb != d.min_free_gb, s.min_free_gb.to_string());
    note("--l1-table ", s.l1_table.is_some(), format!("{}", s.l1_table.as_deref().unwrap_or(Path::new("")).display()));
    note("--key-check", s.diag.key_check, String::new());
    note("--kernel-explain", s.diag.kernel_explain, String::new());
    note("--kernel-mix ", s.diag.kernel_mix.is_some(), format!("{}", s.diag.kernel_mix.as_deref().unwrap_or(Path::new("")).display()));
    note("(kernel metrics)", s.diag.kernel_metrics, String::new());
    note("--build-trace", s.diag.build_trace, String::new());
    note("--phases", s.diag.phases, String::new());
    note("--emit-capture ", s.diag.emit_capture.is_some(), format!("{:?}", s.diag.emit_capture));
    note("--walk-regions ", s.diag.walk_regions.is_some(), format!("{:?}", s.diag.walk_regions));
    note("--l1-path ", s.diag.l1_path.is_some(), format!("{:?}", s.diag.l1_path));
    if !set.is_empty() {
        eprintln!("[settings] {}", set.join(" "));
    }
    install(s);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[derive(Parser)]
    struct Cli {
        #[command(flatten)]
        s: SettingsArgs,
    }

    fn parse(args: &[&str]) -> Result<Settings> {
        let mut v = vec!["x"];
        v.extend(args);
        Cli::try_parse_from(v).map_err(|e| anyhow::anyhow!("{e}"))?.s.settings()
    }

    /// The flags parse into the settings and refuse what is malformed.
    #[test]
    fn flags_make_the_settings() {
        assert_eq!(parse(&[]).expect("defaults"), Settings::default());
        let s = parse(&["--room", "6,2", "--hundred", "--loading-jank", "2", "--level-minus-one", "94,5", "--win-at", "8..10,64"]).expect("flags");
        assert_eq!((s.room, s.hundred, s.loading_jank, s.level_minus_one, s.win_at), ((6, 2), true, Some(2), Some((94, 5)), Some((8, 10, 64, 64))));
        for bad in [&["--room", "6"][..], &["--region", "16,9"], &["--storage-region", "12"], &["--nodiag", "--onlydiag"], &["--loading-jank", "0"], &["--win-at", "9..8,1"], &["--threads", "0"], &["--region", "12,6", "--storage-region", "8"]] {
            assert!(parse(bad).is_err(), "{bad:?}");
        }
        assert_eq!(parse(&["--region", "off"]).expect("off").region, None);
    }

    /// The record round-trips; every differing setting is named; a tree
    /// without a record is refused; what never changes a tree is not in it.
    #[test]
    fn a_tree_is_refused_under_other_settings() {
        let dir = std::env::temp_dir().join(format!("celeste-settings-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("mkdir");
        let sample = Settings { room: (6, 2), hundred: true, loading_jank: Some(2), ..Settings::default() };
        let e = sample.check_at(&dir, "r0sxhf").expect_err("no record");
        assert!(e.to_string().contains("records no settings"), "{e}");
        std::fs::write(path(&dir), sample.text("r0sxhf")).expect("write");
        sample.check_at(&dir, "r0sxhf").expect("the same settings");
        let harmless = Settings { threads: Some(3), trim_rows: true, level_minus_one: Some((94, 5)), diag: Diagnostics { key_check: true, ..Default::default() }, ..sample.clone() };
        harmless.check_at(&dir, "r0sxhf").expect("threads, trimming, level -1 and diagnostics are not a tree's settings");
        let other = Settings { hundred: false, split_frame: true, win_at: Some((8, 8, 64, 64)), ..sample.clone() };
        let e = other.check_at(&dir, "r0sxh").expect_err("other settings").to_string();
        for k in ["hundred: the tree's 1, this run's 0", "split_frame: the tree's 0, this run's 1", "win_at: the tree's none, this run's 8..8,64..64", "level: the tree's r0sxhf, this run's r0sxh"] {
            assert!(e.contains(k), "{k} not in {e}");
        }
        assert!(!e.contains("room:"), "{e}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}
