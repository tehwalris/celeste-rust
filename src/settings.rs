//! THE RUN'S SETTINGS: every setting of a run, from its COMMAND LINE
//! (`Settings`, the flags' struct), in ONE value (made by `startup`) that the
//! binary PASSES to what reads it: no global, no environment (`refuse_env`).
//! The kernel registry and the level -1 table, cached per process, are keyed
//! on them.
//!
//! ONE TABLE: the fields are the flags, and `rows` is every setting as text:
//! the tree's record (`recorded`), the startup log, the level -1 table's
//! cache key (`cart_key`), a record read back (`of_tree`).
//!
//! A tree RECORDS the settings that change it, its level included
//! (plans/architecture.md "Per-tree records"), and refuses a run under
//! others (`check`). A resume does NOT default the flags to the record: the
//! same command line would then mean another search depending on what is on
//! disk. Only a command that reads a finished tree takes its settings from
//! the record (`of_tree`).

use anyhow::{Context, Result};
use clap::Parser;
use std::path::{Path, PathBuf};

/// The tree record's format (`storage::record`): a tree without one, or
/// with another, is refused where its settings are checked
/// (tools/fixtures.sh rebuilds its fixtures on a change).
pub const VERSION: u32 = 3;

/// The game's arithmetic (`--arith`): 16.16, or the float tools' doubles
/// with the tool (which decides the room's start and the seeds' meaning).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arith {
    Fixed,
    Float(FloatTool),
}

/// `$body` with `$N` the type of `$arith`'s numbers (`Fixed`: `Pico8Num`,
/// `Float`: `LuaF64`): THE dispatch from a run's setting to the code
/// monomorphized for it.
#[macro_export]
macro_rules! with_arith {
    ($arith:expr, $N:ident => $body:expr) => {
        match $arith {
            $crate::settings::Arith::Fixed => {
                type $N = $crate::arith::Fixed;
                $body
            }
            $crate::settings::Arith::Float(_) => {
                type $N = $crate::arith::Float;
                $body
            }
        }
    };
}

/// The float tool a float run models (`--arith float-celia|float-uct`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatTool {
    /// UniversalClassicTas: the room loaded alone (the IL load), the file's
    /// seeds set on its balloons and chests (`set_seeds`). A secondary
    /// report (plans/float.md).
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

/// THE CATEGORY (`--category`), as the tasdatabase names it: what wins a
/// room (100%: the exit with the berry), which dash starts are moves
/// (nodiag: no diagonal one, onlydiag: only diagonal ones; the forbidden
/// arm raises, so it is no move, exactly) and the dashes past the orb
/// (gemskip: the orb room won without the orb, one dash after it).
#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum Category {
    #[value(name = "any")]
    Any,
    #[value(name = "100")]
    Hundred,
    #[value(name = "nodiag")]
    Nodiag,
    #[value(name = "onlydiag")]
    Onlydiag,
    #[value(name = "gemskipany")]
    GemskipAny,
    #[value(name = "gemskip100")]
    Gemskip100,
    #[value(name = "gemskipnodiag")]
    GemskipNodiag,
}

impl Category {
    /// A room is won only with its berry taken.
    pub fn hundred(self) -> bool {
        matches!(self, Category::Hundred | Category::Gemskip100)
    }
    /// A dash may not start diagonal.
    pub fn nodiag(self) -> bool {
        matches!(self, Category::Nodiag | Category::GemskipNodiag)
    }
    /// A dash must start diagonal.
    pub fn onlydiag(self) -> bool {
        self == Category::Onlydiag
    }
    /// The orb room won without the orb; one dash after it.
    pub fn gemskip(self) -> bool {
        matches!(self, Category::GemskipAny | Category::Gemskip100 | Category::GemskipNodiag)
    }
}

impl std::str::FromStr for Category {
    type Err = String;
    /// A category by its tasdatabase name (as `--category` takes it).
    fn from_str(s: &str) -> std::result::Result<Self, String> {
        <Category as clap::ValueEnum>::from_str(s, false).map_err(|_| format!("{s:?} is not a category the search models (any, 100, nodiag, onlydiag, gemskipany, gemskip100, gemskipnodiag)"))
    }
}

impl std::fmt::Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use clap::ValueEnum;
        f.write_str(self.to_possible_value().expect("no variant is skipped").get_name())
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
    /// Two search steps a frame (the split cart, `Clock`).
    #[arg(long, global = true)]
    pub split_frame: bool,
    /// The kernels' region grid "px,S" (px a multiple of 8, 1 <= S <= 7),
    /// or `off`.
    #[arg(long, global = true, default_value = "16,6", value_parser = parse_region)]
    pub region: std::option::Option<(i32, i32)>,
    /// The category, as the tasdatabase names it (`Category`): `100` and
    /// `gemskip100` win a room only with its berry, `nodiag` and
    /// `gemskipnodiag` forbid a diagonal dash start, `onlydiag` a straight
    /// one, the `gemskip` ones skip the orb (one dash after it).
    #[arg(long, global = true, default_value = "any")]
    pub category: Category,
    /// The room as real play enters it: its loading frame from object J
    /// (pico8_diff/chain.py), or `none` (the IL load).
    #[arg(long, global = true, default_value = "none", value_parser = parse_jank)]
    pub loading_jank: std::option::Option<usize>,
    /// The start room's balloons' phases fixed, "s1,s2,..", or `none`.
    #[arg(long, global = true, default_value = "none", value_parser = parse_seeds)]
    pub balloon_seeds: std::option::Option<String>,
    // NOT recorded.
    /// The level -1 filter "H,S": H in game frames (the search's horizon),
    /// S the table's speed bound (the tree records it, H in steps, as
    /// `frame::TreeL1`: `minus_one`).
    #[arg(long, global = true, value_parser = parse_minus_one)]
    pub level_minus_one: Option<(u32, i32)>,
    /// The balloons' phases of the concrete phase only (a tasdatabase file's
    /// seeds): the witness is one real run under them.
    #[arg(long, global = true)]
    pub concrete_balloon_seeds: Option<String>,
    /// Worker threads (default: one per physical core).
    #[arg(long, global = true, value_parser = parse_threads)]
    pub threads: Option<usize>,
    /// Use a tree recorded under other settings that do not change what it
    /// holds (only `--region`), under this run's; every difference is logged.
    /// Any other difference (room, win, split frame, category, loading jank,
    /// balloons, level) is still refused. A tree without a record is refused.
    #[arg(long, global = true)]
    pub override_settings: bool,
    /// The start room's CHESTS seeded by `--balloon-seeds` too, in 16.16,
    /// as the TAS tools seed them (each one's last shake draws its seed + 1:
    /// `trace::cart::tool_seeds`): a tool file replayed as the tools play it
    /// (`concrete::witness`). Not a flag, and never a tree's (`write`
    /// refuses it).
    #[arg(skip)]
    pub seed_chests: bool,
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
            row("category", self.category.to_string(), true),
            row("loading_jank", none(self.loading_jank.map(|j| j.to_string())), true),
            row("balloon_seeds", none(self.balloon_seeds.clone()), true),
            row("arith", self.arith.to_string(), true),
            row("level_minus_one", none(self.level_minus_one.map(|(h, s)| format!("{h},{s}"))), false),
            row("concrete_balloon_seeds", none(self.concrete_balloon_seeds.clone()), false),
            row("threads", none(self.threads.map(|t| t.to_string())), false),
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
    /// whether a concrete state keys through `search::concrete::lookup_view`
    /// (its balloons are seeded where the tree's hold the phase an interval).
    pub fn concrete(&self) -> (Settings, bool) {
        let float = matches!(self.arith, Arith::Float(_));
        let seeds = self.concrete_balloon_seeds.clone().or_else(|| self.balloon_seeds.clone()).or_else(|| float.then(String::new));
        let seeded = self.concrete_balloon_seeds.is_some() || (float && self.balloon_seeds.is_none());
        (Settings { balloon_seeds: seeds, ..self.clone() }, seeded)
    }

    /// The FLOAT half of a dual search (`search::dual`): these settings (the
    /// fixed half's) under the float tool `tool`, from the tool's own start:
    /// Celia's IL load's jank unless `jank` gives one, UCT's IL load.
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

    /// The run's frames and search steps (`Clock`).
    pub fn clock(&self) -> Clock {
        Clock { per_frame: if self.split_frame { 2 } else { 1 } }
    }

    /// The room that means "won" (`game_runner::win_room` of `--room`).
    pub fn win_room(&self) -> (i16, i16) {
        crate::game_runner::win_room(self.room)
    }

    /// The kernels' region grid (`--region`; `None`: off).
    pub fn region_grid(&self) -> Option<crate::trace::walk::RegionGrid> {
        self.region.map(|(px, speed)| crate::trace::walk::RegionGrid { px, speed })
    }

    /// The run's level -1 filter (`--level-minus-one H,S`), if given: H in
    /// search steps (`Clock`).
    pub fn minus_one(&self) -> Option<crate::frame::MinusOne> {
        self.level_minus_one.map(|(h, speed)| crate::frame::MinusOne { h: self.clock().steps(h), speed })
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
            // (`trace::cart::sources`).
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

    /// Record these, for a tree widened at `level`, as the tree's: its
    /// record's settings lines, the forward's lines kept (atomic).
    pub fn write(&self, dir: &Path, level: crate::abstraction::Level) -> Result<()> {
        use crate::storage::record::{Record, TREE_KEYS};
        anyhow::ensure!(!self.seed_chests, "{}: a tree is never searched with its chests seeded (a witness replay's setting)", dir.display());
        let mut rec = Record::read(dir)?.unwrap_or_default();
        rec.retain(|k| TREE_KEYS.contains(&k));
        let forward: Vec<(String, String)> = rec.lines().map(|(k, v)| (k.to_string(), v.to_string())).collect();
        let mut rec = Record::default();
        for (k, v) in self.recorded(&level.to_string()) {
            rec.set(k, v);
        }
        for (k, v) in forward {
            rec.set(&k, v);
        }
        rec.write(dir)
    }

    /// THE SETTINGS OF A FINISHED TREE, for a command that reads it: its
    /// record read back (each line its flag's text) and its level -1 filter
    /// (`frame::TreeL1`), with the run's own settings from `run` (the
    /// threads, the concrete phase's seeds, the diagnostics). The tree's
    /// level with them.
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
        // The filter's H, recorded in steps, as `--level-minus-one` takes it
        // (frames).
        let per_frame = flags.s.clock().per_frame;
        let minus_one = match crate::frame::TreeL1::recorded(dir)? {
            Some((m, _)) => {
                anyhow::ensure!(m.h % per_frame == 0, "{}: the level -1 horizon {} steps is no whole frame", dir.display(), m.h);
                Some((m.h / per_frame, m.speed))
            }
            None => None,
        };
        let s = Settings {
            level_minus_one: minus_one,
            concrete_balloon_seeds: run.concrete_balloon_seeds.clone(),
            threads: run.threads,
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
        let forward = |k: &str| crate::storage::record::TREE_KEYS.contains(&k);
        diffs.extend(rec.keys().filter(|k| !forward(k) && !mine.iter().any(|(m, _)| m == *k)).map(|k| (*k, format!("{k}: in the tree's record, not a setting"))));
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

/// A tree's record (`storage::record`): its lines, `key value`, by key.
fn read_record(dir: &Path) -> Result<std::collections::BTreeMap<String, String>> {
    let rec = crate::storage::record::Record::of(dir)?;
    Ok(rec.lines().map(|(k, v)| (k.to_string(), v.to_string())).collect())
}

/// The recorded settings `--override-settings` may override: those that do
/// not change what a tree holds. The kernels' region grid (`--region`) only
/// specializes the kernels: they are exact on every grid, and a lane outside
/// a grid's bounds is a fatal coverage gap, never another successor. Every
/// other line - the room, the win, the split frame, the category, the
/// loading jank, the balloons, the level - is the search.
pub const OVERRIDABLE: &[&str] = &["region"];

/// The record's file in a level's tree.
fn path(dir: &Path) -> PathBuf {
    crate::storage::record::path(dir)
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
/// shadow a setting. Checked by the binary's test in every profile
/// (`clap`'s own assertion of it runs only with debug assertions).
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
/// environment (a stale script must not run with its knobs silently
/// ignored; settings are flags). `CELESTE_FIXTURES` (read by
/// tools/fixtures.sh only) is allowed.
pub fn refuse_env() -> Result<()> {
    let mut bad: Vec<String> = std::env::vars_os().filter_map(|(k, _)| k.to_str().map(str::to_string)).filter(|k| k.starts_with("CELESTE_") && k != "CELESTE_FIXTURES").collect();
    bad.sort();
    anyhow::ensure!(bad.is_empty(), "the environment sets {}: settings are flags (--help)", bad.join(", "));
    Ok(())
}

/// A binary's second step: refuse to run without a MEMORY CAP - a finite
/// `memory.max` on the process's cgroup or an ancestor's (cgroup v2), what
/// `./safe-run.sh` gives it (CLAUDE.md "Running safely": an uncapped run
/// once took the machine down). Checked at run time, so no script can
/// forget the wrapper.
pub fn refuse_uncapped() -> Result<()> {
    let own = std::fs::read_to_string("/proc/self/cgroup").context("reading /proc/self/cgroup (the memory cap's cgroup)")?;
    capped(&own, Path::new("/sys/fs/cgroup"))
}

/// `refuse_uncapped` of a `/proc/self/cgroup` text over the cgroup tree at
/// `root`.
fn capped(own: &str, root: &Path) -> Result<()> {
    let path = own.lines().find_map(|l| l.strip_prefix("0::")).with_context(|| format!("no cgroup v2 line in /proc/self/cgroup ({own:?}): the memory cap cannot be checked"))?;
    let mut dir = Some(Path::new(path));
    while let Some(d) = dir {
        let max = std::fs::read_to_string(root.join(d.strip_prefix("/").unwrap_or(d)).join("memory.max")).unwrap_or_default();
        if max.trim().parse::<u64>().is_ok() {
            return Ok(());
        }
        dir = d.parent();
    }
    anyhow::bail!("no memory cap: cgroup {path} and every ancestor have memory.max `max`. Run it under ./safe-run.sh (CLAUDE.md \"Running safely\")")
}

/// A binary's settings from its flags (the environment refused), logged:
/// one `[settings]` line with every setting not at its default. `float`:
/// whether the command runs in float; one that does not REFUSES `--arith
/// float` (it would run fixed under a float flag).
pub fn startup(args: &Settings, float: bool, adjust: impl FnOnce(&mut Settings)) -> Result<Settings> {
    refuse_env()?;
    refuse_uncapped()?;
    let mut s = args.clone().resolved()?;
    anyhow::ensure!(float || s.arith == Arith::Fixed, "--arith {}: this command runs in fixed only so far (plans/float.md, Open gaps)", s.arith);
    adjust(&mut s);
    let set: Vec<String> = s.rows().into_iter().zip(Settings::default().rows()).filter(|(a, d)| a.text != d.text).flat_map(|(a, _)| a.flag()).collect();
    if !set.is_empty() {
        eprintln!("[settings] {}", set.join(" "));
    }
    Ok(s)
}

/// FRAMES AND SEARCH STEPS (plans/architecture.md "The split frame"): a
/// game frame is `per_frame` search steps - 2 under `--split-frame` (part a
/// up to and including the player's move, part b the rest; step `2k` ends
/// frame `k`), else 1. Steps are the internal unit: the tree and its
/// files, the edges, W, the marks and level -1's recorded `H` count them
/// (a mid-frame state is a node of its own), as the flags named so do
/// (`--steps`, `--step`, `--from-step`, `--to-step`). The search's horizon
/// (`--frames`), `--level-minus-one`'s `H`, its bounds, witnesses and
/// `result.json` count frames. Every conversion is here.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Clock {
    pub per_frame: u32,
}

impl Clock {
    /// `frames` frames in steps.
    pub fn steps(self, frames: u32) -> u32 {
        frames * self.per_frame
    }
    /// The frame step `step` ends or is inside (a mid-frame step's frame
    /// is the one it completes).
    pub fn frame_of(self, step: u32) -> u32 {
        step.div_ceil(self.per_frame)
    }
    /// Does step `step` end a frame?
    pub fn boundary(self, step: u32) -> bool {
        step % self.per_frame == 0
    }
    /// Step `step` for a report: "frame F", or "mid-frame F".
    pub fn show(self, step: u32) -> String {
        if self.boundary(step) {
            format!("frame {}", step / self.per_frame)
        } else {
            format!("mid-frame {}", self.frame_of(step))
        }
    }
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
        let s = parse(&["--room", "6,2", "--category", "100", "--loading-jank", "2", "--level-minus-one", "94,5", "--win-at", "8..10,64"]).expect("flags");
        assert_eq!((s.room, s.category, s.loading_jank, s.level_minus_one, s.win_at), ((6, 2), Category::Hundred, Some(2), Some((94, 5)), Some((8, 10, 64, 64))));
        // Every category by its tasdatabase name, and what it means.
        let rules = |c: &str| {
            let c = parse(&["--category", c]).expect("a category").category;
            (c.to_string(), c.hundred(), c.nodiag(), c.onlydiag(), c.gemskip())
        };
        for (name, hundred, nodiag, onlydiag, gemskip) in [
            ("any", false, false, false, false),
            ("100", true, false, false, false),
            ("nodiag", false, true, false, false),
            ("onlydiag", false, false, true, false),
            ("gemskipany", false, false, false, true),
            ("gemskip100", true, false, false, true),
            ("gemskipnodiag", false, true, false, true),
        ] {
            assert_eq!(rules(name), (name.to_string(), hundred, nodiag, onlydiag, gemskip));
        }
        for bad in [&["--room", "6"][..], &["--region", "16,9"], &["--category", "hundred"], &["--category", "mindashes"], &["--hundred"], &["--loading-jank", "0"], &["--win-at", "9..8,1"], &["--threads", "0"], &["--region", "12,6"]] {
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
        let all = Settings { room: (6, 2), win_at: Some((8, 10, 64, 64)), split_frame: true, region: None, category: Category::GemskipNodiag, loading_jank: Some(2), balloon_seeds: Some("0,1".into()), ..run.clone() };
        let float = Settings { arith: Arith::Float(FloatTool::Uct), loading_jank: None, category: Category::Onlydiag, ..all.clone() };
        for s in [Settings { threads: Some(3), ..run.clone() }, all, float] {
            let _ = std::fs::remove_dir_all(&dir);
            s.write(&dir, lvl).expect("write");
            crate::storage::record::update(&dir, "level_minus_one", "off").expect("the filter's record");
            let (back, level) = Settings::of_tree(&dir, &run).expect("read back");
            assert_eq!((back, level), (s, lvl));
        }
        // Level -1's H: frames on the command line, steps in the record (two
        // a frame under the split frame).
        let split = Settings { split_frame: true, level_minus_one: Some((84, 5)), ..run.clone() };
        assert_eq!(split.minus_one().map(|m| (m.h, m.speed)), Some((168, 5)));
        let _ = std::fs::remove_dir_all(&dir);
        split.write(&dir, lvl).expect("write");
        for (record, back) in [("h 168 speed 5 table 0", Ok(Some((84, 5)))), ("h 167 speed 5 table 0", Err("no whole frame"))] {
            crate::storage::record::update(&dir, "level_minus_one", record).expect("the filter's record");
            match (Settings::of_tree(&dir, &run), back) {
                (Ok((s, _)), Ok(want)) => assert_eq!(s.level_minus_one, want),
                (Err(e), Err(why)) => assert!(e.to_string().contains(why), "{e}"),
                (got, want) => panic!("{record:?}: {:?}, wanted {want:?}", got.map(|(s, _)| s.level_minus_one)),
            }
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// A process runs only under a memory cap: its cgroup's, or an
    /// ancestor's, finite `memory.max`.
    #[test]
    fn a_run_without_a_memory_cap_is_refused() {
        let root = std::env::temp_dir().join(format!("celeste-cgroup-{}", std::process::id()));
        let scope = root.join("user.slice/run.scope");
        std::fs::create_dir_all(&scope).expect("mkdir");
        let own = "0::/user.slice/run.scope\n";
        for (slice, run, ok) in [("max", "max", false), ("max", "2147483648", true), ("8589934592", "max", true)] {
            std::fs::write(root.join("user.slice/memory.max"), format!("{slice}\n")).expect("write");
            std::fs::write(scope.join("memory.max"), format!("{run}\n")).expect("write");
            assert_eq!(capped(own, &root).is_ok(), ok, "slice {slice}, scope {run}");
        }
        assert!(capped("1:name=systemd:/x\n", &root).is_err(), "no cgroup v2 line");
        let _ = std::fs::remove_dir_all(&root);
    }

    /// The record round-trips; every differing setting is named; a tree
    /// without a record is refused; what never changes a tree is not in it.
    #[test]
    fn a_tree_is_refused_under_other_settings() {
        let dir = std::env::temp_dir().join(format!("celeste-settings-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("mkdir");
        let lvl = |l: &str| crate::abstraction::Level::parse(l).expect("a level");
        let sample = Settings { room: (6, 2), category: Category::Hundred, loading_jank: Some(2), ..Settings::default() };
        let e = sample.check(&dir, lvl("r0sxhf")).expect_err("no record");
        assert!(e.to_string().contains("has no record"), "{e}");
        sample.write(&dir, lvl("r0sxhf")).expect("write");
        sample.check(&dir, lvl("r0sxhf")).expect("the same settings");
        let harmless = Settings { threads: Some(3), level_minus_one: Some((94, 5)), diag: Diagnostics { key_check: true, ..Default::default() }, ..sample.clone() };
        harmless.check(&dir, lvl("r0sxhf")).expect("threads, level -1 and diagnostics are not a tree's settings");
        let other = Settings { category: Category::Any, split_frame: true, win_at: Some((8, 8, 64, 64)), ..sample.clone() };
        let e = other.check(&dir, lvl("r0sxh")).expect_err("other settings").to_string();
        for k in ["category: the tree's 100, this run's any", "split_frame: the tree's 0, this run's 1", "win_at: the tree's none, this run's 8..8,64..64", "level: the tree's r0sxhf, this run's r0sxh"] {
            assert!(e.contains(k), "{k} not in {e}");
        }
        assert!(!e.contains("room:"), "{e}");
        // The arithmetic: a fixed record has no line for it; either way
        // across is refused.
        assert!(!sample.recorded("r0sxhf").iter().any(|(k, _)| *k == "arith"));
        let float = Settings { arith: Arith::Float(FloatTool::Uct), ..sample.clone() };
        let e = float.check(&dir, lvl("r0sxhf")).expect_err("a float run on a fixed tree").to_string();
        assert!(e.contains("arith: the tree's (missing), this run's float-uct"), "{e}");
        float.write(&dir, lvl("r0sxhf")).expect("write");
        let e = sample.check(&dir, lvl("r0sxhf")).expect_err("a fixed run on a float tree").to_string();
        assert!(e.contains("arith: in the tree's record"), "{e}");
        // A reader that is not the run (the UI export) reads it back.
        assert_eq!(recorded_arith(&dir).expect("a record"), Arith::Float(FloatTool::Uct));
        sample.write(&dir, lvl("r0sxhf")).expect("write");
        assert_eq!(recorded_arith(&dir).expect("a record"), Arith::Fixed);
        // `--override-settings`: only the region grid, and only with the flag.
        let grid = Settings { region: None, ..sample.clone() };
        let e = grid.check(&dir, lvl("r0sxhf")).expect_err("another grid, no override").to_string();
        assert!(e.contains("region: the tree's 16,6, this run's none") && e.contains("--override-settings proceeds"), "{e}");
        Settings { override_settings: true, ..grid.clone() }.check(&dir, lvl("r0sxhf")).expect("the grid overridden");
        let e = Settings { override_settings: true, category: Category::Any, ..grid }.check(&dir, lvl("r0sxhf")).expect_err("a category is not overridable").to_string();
        assert!(e.contains("category: the tree's 100, this run's any") && e.contains("region:"), "{e}");
        // A line that is no setting is refused.
        crate::storage::record::update(&dir, "storage_region", "8").expect("write");
        let e = sample.check(&dir, lvl("r0sxhf")).expect_err("an unknown line").to_string();
        assert!(e.contains("storage_region: in the tree's record, not a setting"), "{e}");
        let _ = std::fs::remove_dir_all(&dir);
    }
}
