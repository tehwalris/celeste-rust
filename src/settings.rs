//! THE RUN'S SETTINGS: every setting of a run, from its COMMAND LINE
//! (`Settings` is the flags' struct, flattened into every binary's
//! arguments), in ONE value (made by `startup`) that the binary PASSES to
//! what reads it. There is no global: an engine (`FrameEngine`,
//! `RefEngine`) owns its settings, and the wave, the storage and the search
//! take them from it or as a parameter; the kernel registry and the level -1
//! table, cached per process, are keyed on them.
//! Nothing reads the environment: a `CELESTE_*` variable is refused at
//! startup (`refuse_env`, with the flag that replaced it).
//!
//! ONE TABLE: the struct's fields are the flags (their docs, defaults and
//! parsers), and `rows` is every setting as text, from which come the
//! tree's record (`recorded`), the startup log, the level -1 table's cache
//! key (`cart_key`) and a tree's record read back (`of_tree`: the record's
//! values are the flags' texts).
//!
//! The settings that change what a level's tree holds are RECORDED in the
//! tree (`<level dir>/settings.txt`) when its frame 0 is written, and
//! compared with the run's on every resume or reuse (`check`): a difference
//! is an error, never a silently mixed tree. A resume does NOT default the
//! flags to the recorded ones: the same command line would then mean another
//! search depending on what is on disk (a deleted tree rerun by "the same"
//! command would silently change category). Only a command that reads a
//! finished tree takes its settings from the record (`of_tree`).
//!
//! The level -1 filter is recorded apart (`frame::TreeFilter`,
//! `level_minus_one.txt`): a tree serves every horizon up to it, and is
//! never raised past it (`frame::grow_tree`). Threads and the diagnostics never change a
//! tree. The level the tree's rows are widened at (the engine's `Level`) is
//! a line of the record: a tree used at another level is refused like any
//! other difference.
//!
//! The deep diagnostics have owners: `--key-check` and `--phases` the
//! wave's sinks (`storage::unit::UnitSink`), `--build-trace`,
//! `--kernel-explain` and the metrics the kernel build's options
//! (`compiled::asm_kernel::BuildOptions`).

use anyhow::{Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};

/// The record's format: a tree without one, or with another, is refused
/// where its settings are checked (tools/fixtures.sh rebuilds its fixtures
/// on a change).
pub const VERSION: u32 = 1;

/// The game's arithmetic (`--arith`, plans/float.md): the real
/// game's 16.16, or the float tools' Lua doubles - with the tool, which
/// decides the room's start and the seeds' meaning.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arith {
    Fixed,
    Float(FloatTool),
}

/// The float tool a float run models (`--arith float-celia|float-uct`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatTool {
    /// UniversalClassicTas: the room loaded alone (the IL load), the file's
    /// seeds set on its balloons and chests (`set_seeds`). A secondary
    /// report (Philippe; plans/float.md).
    Uct,
    /// Celia, THE float tool (the community's): the room entered with its
    /// loading jank (`--loading-jank J`, by default its IL load's:
    /// `starts::celia_jank`), the seeds set after it (`init_seed_objs`,
    /// `set_seed`).
    Celia,
}

impl std::fmt::Display for Arith {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arith::Fixed => write!(f, "fixed"),
            Arith::Float(FloatTool::Uct) => write!(f, "float-uct"),
            Arith::Float(FloatTool::Celia) => write!(f, "float-celia"),
        }
    }
}

/// THE FLAGS (every binary flattens them; `global`: given anywhere on a
/// `rewrite` command line), and the run's settings.
#[derive(clap::Args, Clone, Debug, PartialEq, Eq)]
pub struct Settings {
    // RECORDED in a tree.
    /// The game's arithmetic: `fixed` (PICO-8's 16.16, the real game), or
    /// the TAS tools' Lua doubles (plans/float.md): `float` or `float-celia`
    /// (Celia's float mode: its IL load's loading jank unless
    /// `--loading-jank` is given, its seeds), `float-uct`
    /// (UniversalClassicTas: the IL load, its seeds). Recorded where not
    /// `fixed`.
    #[arg(long, global = true, default_value = "fixed", value_parser = parse_arith)]
    pub arith: Arith,
    /// The start room "x,y".
    #[arg(long, global = true, default_value = "1,0", value_parser = |s: &str| pair::<i16>(s, "--room"))]
    pub room: (i16, i16),
    /// A synthetic win: the player at "x,y", or in "x0..x1,y0..y1" (a test
    /// configuration: a tree under it is another search), or `none`.
    #[arg(long, global = true, default_value = "none", value_parser = parse_win_at)]
    pub win_at: std::option::Option<(i16, i16, i16, i16)>,
    /// Two search steps a frame (the split cart).
    #[arg(long, global = true)]
    pub split_frame: bool,
    /// The kernels' region grid "px,S" (px a multiple of 8, 1 <= S <= 7),
    /// or `off`.
    #[arg(long, global = true, default_value = "16,6", value_parser = parse_region)]
    pub region: std::option::Option<(i32, i32)>,
    /// 100%: a room is won only with its berry taken.
    #[arg(long, global = true)]
    pub hundred: bool,
    /// No Diagonal Dashes: a dash may not start diagonal.
    #[arg(long, global = true)]
    pub nodiag: bool,
    /// Only Diagonal Dashes: a dash must start diagonal (the straight and
    /// facing-only dash arms raise).
    #[arg(long, global = true)]
    pub onlydiag: bool,
    /// The gemskip categories: the orb room won without the orb, one dash after it.
    #[arg(long, global = true)]
    pub gemskip: bool,
    /// The room as real play enters it: its loading frame from object J
    /// (pico8_diff/chain.py), or `none` (the IL load).
    #[arg(long, global = true, default_value = "none", value_parser = parse_jank)]
    pub loading_jank: std::option::Option<usize>,
    /// The start room's balloons' phases fixed, "s1,s2,..", or `none`.
    #[arg(long, global = true, default_value = "none", value_parser = parse_seeds)]
    pub balloon_seeds: std::option::Option<String>,
    // NOT recorded.
    /// The level -1 filter "H,S": H in search steps, S the table's speed
    /// bound (the tree records it as `frame::TreeFilter`).
    #[arg(long, global = true, value_parser = parse_minus_one)]
    pub level_minus_one: Option<(u32, i32)>,
    /// The balloons' phases of the concrete phase only (a tasdatabase file's
    /// seeds): the witness is one real run under them.
    #[arg(long, global = true)]
    pub concrete_balloon_seeds: Option<String>,
    /// Worker threads (default: one per physical core).
    #[arg(long, global = true, value_parser = parse_threads)]
    pub threads: Option<usize>,
    /// The level -1 table pinned by its FILE (read if it exists, else built
    /// there): the fixtures', never a search's.
    #[arg(long, global = true)]
    pub l1_table: Option<PathBuf>,
    /// Use a tree recorded under other settings that do not change what it
    /// holds (only `--region`), under this run's; every difference is logged.
    /// Any other difference (room, win, split frame, category, loading jank,
    /// balloons, level) is still refused. A tree without a record is refused.
    #[arg(long, global = true)]
    pub override_settings: bool,
    #[command(flatten)]
    pub diag: Diagnostics,
}

/// Self-checks, logs and dumps: none changes a result.
#[derive(clap::Args, Clone, Debug, Default, PartialEq, Eq)]
pub struct Diagnostics {
    /// DIAGNOSTIC: every emitted row's key recomputed by the boundary, and
    /// the row decoded back from its key and cell (the new layer is
    /// decoded so) and compared.
    #[arg(long, global = true)]
    pub key_check: bool,
    /// DIAGNOSTIC: kernels keep their graphs, so a decline names its premise.
    #[arg(long, global = true)]
    pub kernel_explain: bool,
    /// The kernels' census and slice counters (`bench frame --metrics`).
    #[arg(skip)]
    pub kernel_metrics: bool,
    /// DIAGNOSTIC: the kernel build's and the lattice walk's progress.
    #[arg(long, global = true)]
    pub build_trace: bool,
    /// DIAGNOSTIC: the wave workers' phase timings.
    #[arg(long, global = true)]
    pub phases: bool,
}

/// `transpile`'s name for the flags.
pub type SettingsArgs = Settings;

/// The flags alone, parsed (the defaults; a record read back).
#[derive(Parser)]
struct Flags {
    #[command(flatten)]
    s: Settings,
}

impl Default for Settings {
    /// The flags' defaults.
    fn default() -> Self {
        Flags::parse_from(["settings"]).s
    }
}

/// One setting as text (`Settings::rows`): its key (the flag is `--key`,
/// `_` as `-`), its value as the flag takes it (a switch's "1" or "0"),
/// and whether a tree records it.
struct Row {
    key: &'static str,
    text: String,
    switch: bool,
    recorded: bool,
}

impl Row {
    /// The flag that gives it (none for a switch that is off).
    fn flag(&self) -> Vec<String> {
        let f = format!("--{}", self.key.replace('_', "-"));
        match (self.switch, self.text.as_str()) {
            (true, "1") => vec![f],
            (true, _) => vec![],
            (false, _) => vec![f, self.text.clone()],
        }
    }
}

impl Settings {
    /// THE TABLE: every setting as text, the recorded ones in the record's
    /// order (`arith`'s line goes after the level's, and only where not
    /// fixed: `recorded`).
    fn rows(&self) -> Vec<Row> {
        let none = |v: Option<String>| v.unwrap_or_else(|| "none".to_string());
        let row = |key, text, recorded| Row { key, text, switch: false, recorded };
        let switch = |key, on: bool, recorded| Row { key, text: u8::from(on).to_string(), switch: true, recorded };
        vec![
            row("room", format!("{},{}", self.room.0, self.room.1), true),
            row("win_at", none(self.win_at.map(|(x0, x1, y0, y1)| format!("{x0}..{x1},{y0}..{y1}"))), true),
            switch("split_frame", self.split_frame, true),
            row("region", none(self.region.map(|(px, s)| format!("{px},{s}"))), true),
            switch("hundred", self.hundred, true),
            switch("nodiag", self.nodiag, true),
            switch("onlydiag", self.onlydiag, true),
            switch("gemskip", self.gemskip, true),
            row("loading_jank", none(self.loading_jank.map(|j| j.to_string())), true),
            row("balloon_seeds", none(self.balloon_seeds.clone()), true),
            row("arith", self.arith.to_string(), true),
            row("level_minus_one", none(self.level_minus_one.map(|(h, s)| format!("{h},{s}"))), false),
            row("concrete_balloon_seeds", none(self.concrete_balloon_seeds.clone()), false),
            row("threads", none(self.threads.map(|t| t.to_string())), false),
            row("l1_table", none(self.l1_table.as_ref().map(|p| p.display().to_string())), false),
            switch("override_settings", self.override_settings, false),
            switch("key_check", self.diag.key_check, false),
            switch("kernel_explain", self.diag.kernel_explain, false),
            switch("build_trace", self.diag.build_trace, false),
            switch("phases", self.diag.phases, false),
        ]
    }

    /// The settings the flags give, validated and resolved: Celia's loading
    /// jank by default its IL load's.
    pub fn resolved(mut self) -> Result<Settings> {
        if let Some((px, _)) = self.region {
            let side = crate::storage::GEOMETRY.side as i32;
            anyhow::ensure!(px % side == 0, "--region {px},..: a kernel region must hold whole storage regions ({side} px)");
        }
        anyhow::ensure!(!(self.nodiag && self.onlydiag), "--nodiag and --onlydiag exclude each other");
        anyhow::ensure!(
            !(self.arith == Arith::Float(FloatTool::Uct) && self.loading_jank.is_some()),
            "--arith float-uct loads a room alone: no --loading-jank (Celia models the jank: --arith float)"
        );
        if self.arith == Arith::Float(FloatTool::Celia) && self.loading_jank.is_none() {
            self.loading_jank = crate::starts::celia_jank(self.room)?;
        }
        Ok(self)
    }

    /// The settings a CONCRETE run uses (the concrete searches, the known
    /// route's check, `l1 check`, the dual witness's replay): the balloons'
    /// phases fixed - `--concrete-balloon-seeds`, else `--balloon-seeds`,
    /// else, for a float tool (which always seeds), every seed 0 - and
    /// whether a concrete state keys through `concrete::lookup_view` (its
    /// balloons are seeded where the tree's hold the phase an interval).
    pub fn concrete(&self) -> (Settings, bool) {
        let float = matches!(self.arith, Arith::Float(_));
        let seeds = self.concrete_balloon_seeds.clone().or_else(|| self.balloon_seeds.clone()).or_else(|| float.then(String::new));
        let seeded = self.concrete_balloon_seeds.is_some() || (float && self.balloon_seeds.is_none());
        (Settings { balloon_seeds: seeds, ..self.clone() }, seeded)
    }

    /// The FLOAT half of a dual search (`rewrite dual`, `search::dual`):
    /// these settings - the fixed half's, real play from its
    /// `--loading-jank` - under the float tool `tool`, from the tool's own
    /// start: Celia's IL load's jank (`starts::celia_jank`) unless `jank`
    /// gives one, UCT's IL load (no jank).
    pub fn float_half(&self, tool: FloatTool, jank: Option<usize>) -> Result<Settings> {
        anyhow::ensure!(self.arith == Arith::Fixed, "the dual search's settings are the fixed half's (no --arith float)");
        let loading_jank = match tool {
            FloatTool::Celia => match jank {
                Some(j) => Some(j),
                None => crate::starts::celia_jank(self.room)?,
            },
            FloatTool::Uct => {
                anyhow::ensure!(jank.is_none(), "UCT loads a room alone: no float loading jank");
                None
            }
        };
        Ok(Settings { arith: Arith::Float(tool), loading_jank, ..self.clone() })
    }

    /// Worker threads: `--threads`, else one per physical core (AVX-512
    /// units are shared by SMT siblings).
    pub fn threads(&self) -> usize {
        self.threads.unwrap_or_else(|| (std::thread::available_parallelism().map(|n| n.get()).unwrap_or(2) / 2).max(1))
    }

    /// Search steps per game frame: 2 under the split frame
    /// (`--split-frame`: part a up to and including the player's move,
    /// part b the rest; step `2k` is the end of frame `k`), else 1.
    pub fn steps_per_frame(&self) -> u32 {
        if self.split_frame {
            2
        } else {
            1
        }
    }

    /// The room that means "won" (`game_runner::win_room` of `--room`).
    pub fn win_room(&self) -> (i16, i16) {
        crate::game_runner::win_room(self.room)
    }

    /// The kernels' region grid (`--region`; `None`: off).
    pub fn region_grid(&self) -> Option<crate::trace::walk::RegionGrid> {
        self.region.map(|(px, speed)| crate::trace::walk::RegionGrid { px, speed })
    }

    /// The run's level -1 filter (`--level-minus-one H,S`), if given.
    pub fn minus_one(&self) -> Option<crate::frame::MinusOne> {
        self.level_minus_one.map(|(h, speed)| crate::frame::MinusOne { h, speed })
    }

    /// The record's lines, `(key, value)`, in a fixed order: what changes a
    /// tree. `level`: the tree's level spec.
    fn recorded(&self, level: &str) -> Vec<(&'static str, String)> {
        let rows = self.rows();
        let mut lines: Vec<(&'static str, String)> = rows.iter().filter(|r| r.recorded && r.key != "arith").map(|r| (r.key, r.text.clone())).collect();
        lines.push(("level", level.to_string()));
        if self.arith != Arith::Fixed {
            lines.push(("arith", self.arith.to_string()));
            // A float tree's balloons: the phase an interval unless seeded
            // (`trace::cart::sources_in`; trees before 2026-10-16 always seeded).
            lines.push(("float_balloon_phase", (if self.balloon_seeds.is_some() { "seeded" } else { "interval" }).to_string()));
        }
        lines
    }

    /// What the cart (so the kernels and the level -1 table) is a function
    /// of, besides the sources: the cache key of a level -1 table.
    pub fn cart_key(&self) -> String {
        let s: Vec<String> = self.recorded("").into_iter().filter(|(k, _)| *k != "level").map(|(k, v)| format!("{k}={v}")).collect();
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

    /// THE SETTINGS OF A FINISHED TREE, for a command that reads it: its
    /// record read back (each line its flag's text) and its level -1 filter
    /// (`frame::TreeFilter`), with the run's own settings from `run` (the
    /// threads, `--l1-table`, the concrete phase's seeds, the diagnostics).
    /// The tree's level with them.
    pub fn of_tree(dir: &Path, run: &Settings) -> Result<(Settings, crate::abstraction::Level)> {
        let rec = read_record(dir)?;
        let mut args = vec!["tree".to_string()];
        for r in Settings::default().rows().into_iter().filter(|r| r.recorded) {
            let text = match rec.get(r.key) {
                Some(v) => v.to_string(),
                // A fixed tree's record has no `arith` line.
                None if r.key == "arith" => "fixed".to_string(),
                None => anyhow::bail!("{}: the record has no `{}`", path(dir).display(), r.key),
            };
            args.extend(Row { text, ..r }.flag());
        }
        let flags = Flags::try_parse_from(&args).map_err(|e| anyhow::anyhow!("{}: the record does not read back: {e}", path(dir).display()))?;
        let level = crate::abstraction::Level::parse(rec.get("level").with_context(|| format!("{}: the record names no level", path(dir).display()))?).map_err(|e| anyhow::anyhow!(e))?;
        let minus_one = match crate::frame::TreeFilter::read(dir)? {
            Some(crate::frame::TreeFilter::MinusOne { m, .. }) => Some((m.h, m.speed)),
            Some(crate::frame::TreeFilter::Off) => None,
            None => anyhow::bail!("{}: no level -1 record (a tree from before records)", dir.display()),
        };
        let s = Settings {
            level_minus_one: minus_one,
            concrete_balloon_seeds: run.concrete_balloon_seeds.clone(),
            threads: run.threads,
            l1_table: run.l1_table.clone(),
            override_settings: false,
            diag: run.diag.clone(),
            ..flags.s
        };
        s.check(dir, level)?;
        Ok((s, level))
    }

    /// The tree in `dir` was built under these settings, at `level`: else an
    /// error that names every difference.
    pub fn check(&self, dir: &Path, level: crate::abstraction::Level) -> Result<()> {
        let level = level.to_string();
        let rec = read_record(dir)?;
        let rec: std::collections::BTreeMap<&str, &str> = rec.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect();
        let mine = self.recorded(&level);
        let mut diffs: Vec<(&str, String)> = mine
            .iter()
            .filter(|(k, v)| rec.get(k) != Some(&v.as_str()))
            .map(|(k, v)| (*k, format!("{k}: the tree's {}, this run's {v}", rec.get(k).copied().unwrap_or("(missing)"))))
            .collect();
        // `storage_region 8`: a line of the records written before the side
        // was fixed at 8 (`storage::GEOMETRY`); any other side is refused.
        let legacy = |k: &str| k == "storage_region" && rec.get(k) == Some(&"8");
        diffs.extend(rec.keys().filter(|k| !mine.iter().any(|(m, _)| m == *k) && !legacy(k)).map(|k| (*k, format!("{k}: in the tree's record, not a setting"))));
        if diffs.is_empty() {
            return Ok(());
        }
        let show = |d: &[(&str, String)]| d.iter().map(|(_, s)| s.as_str()).collect::<Vec<_>>().join("\n  ");
        let (soft, hard): (Vec<_>, Vec<_>) = diffs.into_iter().partition(|(k, _)| OVERRIDABLE.contains(k));
        anyhow::ensure!(
            hard.is_empty(),
            "{}: the tree was built under other settings than this run's:\n  {}\n(a tree's frames are one search: delete it, or run with its flags; --override-settings overrides only {OVERRIDABLE:?})",
            dir.display(),
            show(&[hard, soft].concat())
        );
        anyhow::ensure!(
            self.override_settings,
            "{}: the tree was built under other settings than this run's:\n  {}\n(they do not change what the tree holds: --override-settings proceeds under this run's, or run with the tree's flags)",
            dir.display(),
            show(&soft)
        );
        eprintln!("[settings] OVERRIDE (--override-settings) {}: the tree's record stays, this run proceeds under its own:\n  {}", dir.display(), show(&soft));
        Ok(())
    }
}

/// The settings a tree decides (its record's, and the level -1 filter's),
/// by key: a command that reads a finished tree refuses them as flags.
pub fn tree_keys() -> Vec<&'static str> {
    Settings::default().rows().into_iter().filter(|r| r.recorded).map(|r| r.key).chain(["level_minus_one"]).collect()
}

/// A tree's record (`settings.txt`): its lines, `key value`, by key.
fn read_record(dir: &Path) -> Result<std::collections::BTreeMap<String, String>> {
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
    Ok(rec.filter_map(|l| l.split_once(' ')).map(|(k, v)| (k.to_string(), v.to_string())).collect())
}

/// The recorded settings `--override-settings` may override: those that do
/// not change what a tree holds. The kernels' region grid (`--region`) only
/// specializes the kernels: they are exact on every grid, and a lane outside
/// a grid's bounds is a fatal coverage gap, never another successor (room
/// (1,0) r0sxh to f44 under `16,6` and `off`: `ckhash --edges` identical,
/// 2026-10-10). Every other line - the room, the win, the split frame, the
/// category, the loading jank, the balloons, the level - is the search.
pub const OVERRIDABLE: &[&str] = &["region"];

/// The record's file in a level's tree.
pub fn path(dir: &Path) -> PathBuf {
    dir.join("settings.txt")
}

/// The arithmetic the tree in `dir` was built in (its record's `arith`
/// line; none: fixed point), for a reader that is not a search's run (the
/// UI export, which reads a dual search's halves both).
pub fn recorded_arith(dir: &Path) -> Result<Arith> {
    match read_record(dir)?.get("arith") {
        None => Ok(Arith::Fixed),
        Some(a) => parse_arith(a).map_err(|e| anyhow::anyhow!("{}: {e}", path(dir).display())),
    }
}

fn pair<T: std::str::FromStr>(s: &str, what: &str) -> std::result::Result<(T, T), String> {
    let (a, b) = s.split_once(',').ok_or_else(|| format!("{what}: \"a,b\", not {s:?}"))?;
    let p = |t: &str| t.trim().parse::<T>().map_err(|_| format!("{what}: {t:?} is not a number"));
    Ok((p(a)?, p(b)?))
}

/// `--arith`: `fixed`, `float` (Celia's), `float-celia`, `float-uct`.
fn parse_arith(s: &str) -> std::result::Result<Arith, String> {
    match s {
        "fixed" => Ok(Arith::Fixed),
        "float" | "float-celia" => Ok(Arith::Float(FloatTool::Celia)),
        "float-uct" => Ok(Arith::Float(FloatTool::Uct)),
        _ => Err(format!("--arith: `fixed`, `float` (`float-celia`) or `float-uct`, not {s:?}")),
    }
}

/// `x,y` or `x0..x1,y0..y1` (inclusive) -> `(x0, x1, y0, y1)`; `none`.
fn parse_win_at(s: &str) -> std::result::Result<Option<(i16, i16, i16, i16)>, String> {
    if s == "none" {
        return Ok(None);
    }
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
    Ok(Some((x0, x1, y0, y1)))
}

/// `px,S` with px >= 1 and 1 <= S <= 7; `off` (`none`: the record's).
fn parse_region(s: &str) -> std::result::Result<Option<(i32, i32)>, String> {
    if s == "off" || s == "none" {
        return Ok(None);
    }
    let (px, sp) = pair::<i32>(s, "--region")?;
    if px < 1 || !(1..=7).contains(&sp) {
        return Err(format!("--region \"px,S\" with px >= 1 and 1 <= S <= 7, not {s:?}"));
    }
    Ok(Some((px, sp)))
}

/// An object index >= 1, or `none`.
fn parse_jank(s: &str) -> std::result::Result<Option<usize>, String> {
    match s {
        "none" => Ok(None),
        _ => match s.trim().parse::<usize>() {
            Ok(j) if j >= 1 => Ok(Some(j)),
            _ => Err(format!("--loading-jank: an object index >= 1 or `none`, not {s:?}")),
        },
    }
}

/// "s1,s2,..", or `none`.
fn parse_seeds(s: &str) -> std::result::Result<Option<String>, String> {
    Ok((s != "none").then(|| s.to_string()))
}

/// Worker threads, at least one.
fn parse_threads(s: &str) -> std::result::Result<usize, String> {
    match s.parse::<usize>() {
        Ok(n) if n >= 1 => Ok(n),
        _ => Err(format!("--threads: a count >= 1, not {s:?}")),
    }
}

/// "H,S" (H < 2^32 - 1).
fn parse_minus_one(s: &str) -> std::result::Result<(u32, i32), String> {
    let (h, sp) = pair::<u32>(s, "--level-minus-one")?;
    if h == u32::MAX {
        return Err(format!("--level-minus-one horizon {h}"));
    }
    Ok((h, i32::try_from(sp).map_err(|_| format!("--level-minus-one speed {sp}"))?))
}

/// The long flags a command line defines twice, per (sub)command, the
/// shared settings' global ones included: a binary's own flag must not
/// shadow a setting (`transpile --level-minus-one` once did). Checked by
/// each binary's test, in every profile (`clap`'s own assertion of it runs
/// only with debug assertions).
pub fn duplicate_flags(cmd: &clap::Command) -> Vec<String> {
    fn walk(cmd: &clap::Command, inherited: &[String], out: &mut Vec<String>) {
        let mut seen: Vec<String> = inherited.to_vec();
        let mut globals = inherited.to_vec();
        for a in cmd.get_arguments() {
            let Some(l) = a.get_long() else { continue };
            if inherited.iter().any(|g| g == l) && a.is_global_set() {
                continue;
            }
            if seen.iter().any(|s| s == l) {
                out.push(format!("{}: --{l}", cmd.get_name()));
            }
            seen.push(l.to_string());
            if a.is_global_set() {
                globals.push(l.to_string());
            }
        }
        for sub in cmd.get_subcommands() {
            walk(sub, &globals, out);
        }
    }
    let mut out = Vec::new();
    walk(cmd, &[], &mut out);
    out
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
        ("CELESTE_HUNDRED", "--hundred"),
        ("CELESTE_NODIAG", "--nodiag"),
        ("CELESTE_ONLYDIAG", "--onlydiag"),
        ("CELESTE_GEMSKIP", "--gemskip"),
        ("CELESTE_LOADING_JANK", "--loading-jank"),
        ("CELESTE_BALLOON_SEEDS", "--balloon-seeds"),
        ("CELESTE_CONCRETE_BALLOON_SEEDS", "--concrete-balloon-seeds"),
        ("CELESTE_LEVEL_MINUS_ONE", "--level-minus-one H,S"),
        ("CELESTE_TRIM_ROWS", "nothing: frame files hold keys, never rows (plans/architecture.md)"),
        ("CELESTE_THREADS", "--threads"),
        ("CELESTE_L1_TABLE", "--l1-table"),
        ("CELESTE_KERNEL_KEY_CHECK", "--key-check"),
        ("CELESTE_KERNEL_EXPLAIN", "--kernel-explain"),
        ("CELESTE_KERNEL_METRICS", "bench frame --metrics"),
        ("CELESTE_BUILD_TRACE", "--build-trace"),
        ("CELESTE_PHASES", "--phases"),
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

/// A binary's settings from its flags (the environment refused), logged:
/// one `[settings]` line with every setting not at its default. The binary
/// passes them to everything that reads them; nothing reads a global.
/// `float`: whether the command runs in float (`--arith float`); every
/// other one REFUSES it (the float pipeline is built a milestone at a time,
/// plans/float.md: a command that would run fixed under a float
/// flag is the footgun).
pub fn startup(args: &Settings, float: bool, adjust: impl FnOnce(&mut Settings)) -> Result<Settings> {
    refuse_env()?;
    let mut s = args.clone().resolved()?;
    anyhow::ensure!(float || s.arith == Arith::Fixed, "--arith {}: this command runs in fixed only so far (plans/float.md, Open gaps)", s.arith);
    adjust(&mut s);
    let set: Vec<String> = s.rows().into_iter().zip(Settings::default().rows()).filter(|(a, d)| a.text != d.text).flat_map(|(a, _)| a.flag()).collect();
    if !set.is_empty() {
        eprintln!("[settings] {}", set.join(" "));
    }
    Ok(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(args: &[&str]) -> Result<Settings> {
        let mut v = vec!["x"];
        v.extend(args);
        Flags::try_parse_from(v).map_err(|e| anyhow::anyhow!("{e}"))?.s.resolved()
    }

    /// The flags parse into the settings and refuse what is malformed.
    #[test]
    fn flags_make_the_settings() {
        assert_eq!(parse(&[]).expect("defaults"), Settings::default());
        let s = parse(&["--room", "6,2", "--hundred", "--loading-jank", "2", "--level-minus-one", "94,5", "--win-at", "8..10,64"]).expect("flags");
        assert_eq!((s.room, s.hundred, s.loading_jank, s.level_minus_one, s.win_at), ((6, 2), true, Some(2), Some((94, 5)), Some((8, 10, 64, 64))));
        for bad in [&["--room", "6"][..], &["--region", "16,9"], &["--nodiag", "--onlydiag"], &["--loading-jank", "0"], &["--win-at", "9..8,1"], &["--threads", "0"], &["--region", "12,6"]] {
            assert!(parse(bad).is_err(), "{bad:?}");
        }
        assert_eq!(parse(&["--region", "off"]).expect("off").region, None);
        // Celia by default, from its IL load: room (1,0)'s J is room (0,0)'s
        // two objects; the first room has no jank; `--loading-jank` overrides.
        let celia = parse(&["--arith", "float"]).expect("float");
        assert_eq!((celia.arith, celia.loading_jank), (Arith::Float(FloatTool::Celia), Some(2)));
        assert_eq!(parse(&["--arith", "float", "--room", "0,0"]).expect("float").loading_jank, None);
        assert_eq!(parse(&["--arith", "float-celia", "--loading-jank", "5"]).expect("celia").loading_jank, Some(5));
        let uct = parse(&["--arith", "float-uct"]).expect("uct");
        assert_eq!((uct.arith, uct.loading_jank), (Arith::Float(FloatTool::Uct), None));
        for bad in [&["--arith", "double"][..], &["--arith", "float-pico8"], &["--arith", "float-uct", "--loading-jank", "2"]] {
            assert!(parse(bad).is_err(), "{bad:?}");
        }
    }

    /// A tree's record reads back into the settings it was written from
    /// (`of_tree`), every recorded setting off its default, fixed and float;
    /// the run's own settings come from the run.
    #[test]
    fn a_record_reads_back() {
        let dir = std::env::temp_dir().join(format!("celeste-settings-back-{}", std::process::id()));
        let lvl = crate::abstraction::Level::parse("r0sxhf").expect("a level");
        let run = Settings { threads: Some(3), diag: Diagnostics { key_check: true, ..Default::default() }, ..Settings::default() };
        let all = Settings { room: (6, 2), win_at: Some((8, 10, 64, 64)), split_frame: true, region: None, hundred: true, nodiag: true, gemskip: true, loading_jank: Some(2), balloon_seeds: Some("0,1".into()), ..run.clone() };
        let float = Settings { arith: Arith::Float(FloatTool::Uct), loading_jank: None, onlydiag: true, nodiag: false, ..all.clone() };
        for s in [Settings { threads: Some(3), ..run.clone() }, all, float] {
            let _ = std::fs::remove_dir_all(&dir);
            s.write(&dir, lvl).expect("write");
            std::fs::write(dir.join("level_minus_one.txt"), "off\n").expect("the filter's record");
            let (back, level) = Settings::of_tree(&dir, &run).expect("read back");
            assert_eq!((back, level), (s, lvl));
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// The record round-trips; every differing setting is named; a tree
    /// without a record is refused; what never changes a tree is not in it.
    #[test]
    fn a_tree_is_refused_under_other_settings() {
        let dir = std::env::temp_dir().join(format!("celeste-settings-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("mkdir");
        let lvl = |l: &str| crate::abstraction::Level::parse(l).expect("a level");
        let sample = Settings { room: (6, 2), hundred: true, loading_jank: Some(2), ..Settings::default() };
        let e = sample.check(&dir, lvl("r0sxhf")).expect_err("no record");
        assert!(e.to_string().contains("records no settings"), "{e}");
        std::fs::write(path(&dir), sample.text("r0sxhf")).expect("write");
        sample.check(&dir, lvl("r0sxhf")).expect("the same settings");
        let harmless = Settings { threads: Some(3), level_minus_one: Some((94, 5)), diag: Diagnostics { key_check: true, ..Default::default() }, ..sample.clone() };
        harmless.check(&dir, lvl("r0sxhf")).expect("threads, level -1 and diagnostics are not a tree's settings");
        let other = Settings { hundred: false, split_frame: true, win_at: Some((8, 8, 64, 64)), ..sample.clone() };
        let e = other.check(&dir, lvl("r0sxh")).expect_err("other settings").to_string();
        for k in ["hundred: the tree's 1, this run's 0", "split_frame: the tree's 0, this run's 1", "win_at: the tree's none, this run's 8..8,64..64", "level: the tree's r0sxhf, this run's r0sxh"] {
            assert!(e.contains(k), "{k} not in {e}");
        }
        assert!(!e.contains("room:"), "{e}");
        // The arithmetic: a fixed record has no line for it (what it always
        // was); either way across is refused.
        assert!(!sample.text("r0sxhf").contains("arith"));
        let float = Settings { arith: Arith::Float(FloatTool::Uct), ..sample.clone() };
        let e = float.check(&dir, lvl("r0sxhf")).expect_err("a float run on a fixed tree").to_string();
        assert!(e.contains("arith: the tree's (missing), this run's float-uct"), "{e}");
        std::fs::write(path(&dir), float.text("r0sxhf")).expect("write");
        let e = sample.check(&dir, lvl("r0sxhf")).expect_err("a fixed run on a float tree").to_string();
        assert!(e.contains("arith: in the tree's record"), "{e}");
        // A reader that is not the run (the UI export) reads it back.
        assert_eq!(recorded_arith(&dir).expect("a record"), Arith::Float(FloatTool::Uct));
        std::fs::write(path(&dir), sample.text("r0sxhf")).expect("write");
        assert_eq!(recorded_arith(&dir).expect("a record"), Arith::Fixed);
        // `--override-settings`: only the region grid, and only with the flag.
        let grid = Settings { region: None, ..sample.clone() };
        let e = grid.check(&dir, lvl("r0sxhf")).expect_err("another grid, no override").to_string();
        assert!(e.contains("region: the tree's 16,6, this run's none") && e.contains("--override-settings proceeds"), "{e}");
        Settings { override_settings: true, ..grid.clone() }.check(&dir, lvl("r0sxhf")).expect("the grid overridden");
        let e = Settings { override_settings: true, hundred: false, ..grid }.check(&dir, lvl("r0sxhf")).expect_err("a category is not overridable").to_string();
        assert!(e.contains("hundred: the tree's 1, this run's 0") && e.contains("region:"), "{e}");
        // A record from before the storage side was fixed: `storage_region 8`
        // is accepted, another side is not.
        for (side, ok) in [("8", true), ("16", false)] {
            std::fs::write(path(&dir), format!("{}storage_region {side}\n", sample.text("r0sxhf"))).expect("write");
            assert_eq!(sample.check(&dir, lvl("r0sxhf")).is_ok(), ok, "storage_region {side}");
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
}
