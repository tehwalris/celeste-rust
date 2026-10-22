//! UniversalClassicTas and Celia, the community's TAS tools, headless under
//! LOVE 11 (`rewrite validate uct|celia`): a tasdatabase file played as the
//! tool plays it, and whether it finished.
//!
//! - UCT (`gonengazit/UniversalClassicTas`): the level reached with F,
//!   the file loaded (W), the room restarted (D), clean-saved (U: played
//!   back, trimmed at the level's end); its input count.
//! - Celia (`gonengazit/Celia`, cctas: the tool that models the LOADING
//!   JANK UCT does not): the level reached as a user does (F, Celia's
//!   default jank), the file loaded (Shift+W), clean-saved (U); Celia's
//!   frame count.
//!
//! A playback that DIES before the exit is a failure, whatever the tool
//! saved afterwards (the drivers, `tools/{uct,celia}/driver.lua`). Each run
//! copies the tool into a scratch game directory with its own LOVE save
//! identity (no clash with a concurrent run); the saved file is named on
//! the result line. The run's options reach the Lua through an opts file.

use anyhow::{bail, ensure, Context, Result};
use std::path::{Path, PathBuf};

/// The tool: the float tools the search models (`--arith float-uct|float-celia`).
pub use crate::settings::FloatTool as Tool;

/// The tools' drivers (in the binary, as the cart is).
const UCT_DRIVER: &str = include_str!("../../tools/uct/driver.lua");
const UCT_EXACT: &str = include_str!("../../tools/uct/exact.lua");
const CELIA_DRIVER: &str = include_str!("../../tools/celia/driver.lua");
const CELIA_EXACT: &str = include_str!("../../tools/celia/exact.lua");
const STATE: &str = include_str!("../../tools/love/state.lua");

/// Appended to UCT's `main.lua`.
const UCT_HOOK: &str = r#"
if love.filesystem.getInfo("driver_opts.lua") then
  -- A Lua error quits at once (LOVE's own handler shows it and waits).
  love.errorhandler = function(msg) print("[driver] LUA ERROR: " .. tostring(msg) .. "\n" .. debug.traceback()); os.exit(3) end
  local opts = require("driver_opts")
  local drv = require("driver")
  drv.TAS = TAS
  local orig_update = love.update
  love.update = function(dt) orig_update(dt); drv.tick() end
  if opts.exact then require("exact").install(drv) end
end
"#;

/// Appended to Celia's `main.lua`.
const CELIA_HOOK: &str = r#"
if love.filesystem.getInfo("celia_opts.lua") then
  -- A Lua error quits at once (LOVE's own handler shows it and waits).
  love.errorhandler = function(msg) print("[celia] LUA ERROR: " .. tostring(msg) .. "\n" .. debug.traceback()); os.exit(3) end
  local opts = require("celia_opts")
  local drv = require("celia_driver")
  local orig_update = love.update
  local exact = opts.exact and require("celia_exact")
  if exact then exact.install(drv) end
  love.update = function(dt)
    if exact and tastool then exact.wrap(tastool) end
    orig_update(dt); drv.tas = tastool; drv.tick()
  end
end
"#;

/// How a tool runs.
#[derive(Clone, Debug, Default, clap::Args)]
pub struct Opts {
    /// The dash count (UCT's dash key 0-3; Celia's `max_djump`): the gemskip
    /// categories 1.
    #[arg(long)]
    pub dashes: Option<u8>,
    /// Celia only: its loading-jank offset (default 0: key A).
    #[arg(long, allow_hyphen_values = true)]
    pub jank_offset: Option<i64>,
    /// Celia only: 16.16 fixed point instead of Lua doubles.
    #[arg(long)]
    pub fixp: bool,
    /// (One LEVEL FILE pair) the cart's exact state after every update of
    /// the playback, to FILE (`tools/*/exact.lua`: the float oracle's ground
    /// truth, `rewrite golden`).
    #[arg(long)]
    pub exact: Option<PathBuf>,
    /// The drivers' per-update `[trace]` lines.
    #[arg(long)]
    pub trace: bool,
    /// All of LOVE's output.
    #[arg(long)]
    pub log: bool,
    /// The tool's clone (default ~/src/github.com/gonengazit/{UniversalClassicTas,Celia}).
    #[arg(long)]
    pub clone: Option<PathBuf>,
    /// LOVE 11.x (default /var/tmp/love/squashfs-root/AppRun: the official
    /// AppImage, extracted).
    #[arg(long)]
    pub love: Option<PathBuf>,
}

/// One file's run.
#[derive(Clone, Debug)]
pub struct Outcome {
    /// Played to the level's end, no death.
    pub finished: bool,
    /// `finished, clean save` (UCT), `finished` (Celia), or `DID NOT FINISH
    /// (why)`; no comma inside the why.
    pub status: String,
    /// UCT: the saved file's inputs; Celia: its frame count (0 unfinished).
    pub count: u32,
    /// The lines the run prints: the trace, the log, Celia's level line,
    /// then the result line (`level L FILE: STATUS, ...`).
    pub lines: Vec<String>,
}

fn home() -> String {
    std::env::var("HOME").unwrap_or_default()
}

/// A Lua string literal.
fn lua_str(s: &str) -> String {
    format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
}

/// `src` copied into `dst` (which must not exist).
fn copy_dir(src: &Path, dst: &Path) -> Result<()> {
    std::fs::create_dir_all(dst)?;
    for e in std::fs::read_dir(src).with_context(|| format!("reading {}", src.display()))? {
        let e = e?;
        let to = dst.join(e.file_name());
        if e.file_type()?.is_dir() {
            copy_dir(&e.path(), &to)?;
        } else {
            std::fs::copy(e.path(), &to).with_context(|| format!("copying {}", e.path().display()))?;
        }
    }
    Ok(())
}

/// `pairs` (level, file) played by `tool`, in order, in one game directory.
pub fn run(tool: Tool, o: &Opts, pairs: &[(i16, PathBuf)]) -> Result<Vec<Outcome>> {
    // The knobs are flags: an environment that still sets one is refused.
    let stale: Vec<String> = std::env::vars().map(|(k, _)| k).filter(|k| ["UCT", "LOVE", "CELIA"].contains(&k.as_str()) || k.starts_with("UCT_") || k.starts_with("CELIA_")).collect();
    ensure!(stale.is_empty(), "{} in the environment: these are flags now (rewrite validate --help)", stale.join(", "));
    ensure!(o.exact.is_none() || pairs.len() == 1, "--exact takes one LEVEL FILE pair");
    ensure!(tool == Tool::Celia || (o.jank_offset.is_none() && !o.fixp), "--jank-offset and --fixp are Celia's");
    if let Some(d) = o.dashes {
        ensure!(tool == Tool::Celia || d <= 3, "--dashes 0-3 (UCT's dash key), not {d}");
    }
    let (name, clone) = match tool {
        Tool::Uct => ("uct", format!("{}/src/github.com/gonengazit/UniversalClassicTas", home())),
        Tool::Celia => ("celia", format!("{}/src/github.com/gonengazit/Celia", home())),
    };
    let clone = o.clone.clone().unwrap_or_else(|| PathBuf::from(clone));
    let love = o.love.clone().unwrap_or_else(|| PathBuf::from("/var/tmp/love/squashfs-root/AppRun"));
    let saves = PathBuf::from(format!("{}/.local/share/love", home()));
    // Last day's save identities go (a run's own is kept: its saved file).
    if let Ok(rd) = std::fs::read_dir(&saves) {
        for e in rd.flatten() {
            let old = e.metadata().ok().and_then(|m| m.modified().ok()).and_then(|t| t.elapsed().ok()).is_some_and(|a| a.as_secs() > 86_400);
            if old && e.file_name().to_string_lossy().starts_with(&format!("{name}-validate-tmp.")) {
                let _ = std::fs::remove_dir_all(e.path());
            }
        }
    }
    let run = super::scratch_dir(&format!("{name}-validate"))?;
    let id = format!("{name}-validate-tmp.{}", run.file_name().expect("a name").to_string_lossy().trim_start_matches(&format!("{name}-validate-")));
    let save = saves.join(&id);
    let game = run.join("game");
    let result = (|| {
        match tool {
            Tool::Uct => copy_dir(&clone.join("CelesteTAS"), &game)?,
            Tool::Celia => {
                copy_dir(&clone, &game)?;
                let _ = std::fs::remove_dir_all(game.join(".git"));
            }
        }
        let conf = game.join("conf.lua");
        let (from, to) = match tool {
            Tool::Uct => ("t.identity=\"CelesteTAS\"", format!("t.identity=\"{id}\"")),
            Tool::Celia => ("t.identity = \"Celia\"", format!("t.identity = \"{id}\"")),
        };
        let text = std::fs::read_to_string(&conf)?;
        ensure!(text.contains(from), "{}: could not set the save identity", conf.display());
        std::fs::write(&conf, text.replace(from, &to))?;
        let files: &[(&str, &str)] = match tool {
            Tool::Uct => &[("driver.lua", UCT_DRIVER), ("exact.lua", UCT_EXACT), ("celeste_state.lua", STATE)],
            Tool::Celia => &[("celia_driver.lua", CELIA_DRIVER), ("celia_exact.lua", CELIA_EXACT), ("celeste_state.lua", STATE)],
        };
        for (f, text) in files {
            std::fs::write(game.join(f), text)?;
        }
        let main = game.join("main.lua");
        let text = std::fs::read_to_string(&main)? + if tool == Tool::Uct { UCT_HOOK } else { CELIA_HOOK };
        std::fs::write(&main, text)?;
        let tas_dir = save.join(if tool == Tool::Uct { "TAS" } else { "celeste" });
        std::fs::create_dir_all(&tas_dir)?;
        pairs.iter().map(|(level, file)| one(tool, o, &run, &love, &save, &tas_dir, *level, file)).collect()
    })();
    let _ = std::fs::remove_dir_all(&run);
    result
}

/// One (level, file) pair of `run`.
#[allow(clippy::too_many_arguments)]
fn one(tool: Tool, o: &Opts, run: &Path, love: &Path, save: &Path, tas_dir: &Path, level: i16, file: &Path) -> Result<Outcome> {
    let nil = |v: Option<String>| v.unwrap_or_else(|| "nil".to_string());
    let exact = nil(o.exact.as_ref().map(|p| lua_str(&p.to_string_lossy())));
    let dashes = nil(o.dashes.map(|d| d.to_string()));
    let (opts_file, opts) = match tool {
        Tool::Uct => ("driver_opts.lua", format!("return {{level = {level}, dashes = {dashes}, exact = {exact}, trace = {}}}\n", o.trace)),
        Tool::Celia => ("celia_opts.lua", format!("return {{level = {level}, dashes = {dashes}, jank_offset = {}, exact = {exact}, trace = {}}}\n", nil(o.jank_offset.map(|j| j.to_string())), o.trace)),
    };
    std::fs::write(run.join("game").join(opts_file), opts)?;
    let saved = match tool {
        Tool::Uct => save.join(format!("TAS{level}.tas")),
        Tool::Celia => tas_dir.join(format!("TAS{level}.tas")),
    };
    std::fs::copy(file, tas_dir.join(format!("TAS{level}.tas"))).with_context(|| format!("copying {}", file.display()))?;
    if tool == Tool::Uct {
        let _ = std::fs::remove_file(&saved);
    }
    // stdout and stderr in one file, in the order they were written.
    let log_path = run.join("love.log");
    let log_file = std::fs::File::create(&log_path)?;
    let mut cmd = std::process::Command::new(love);
    cmd.current_dir(run).arg("game");
    if o.fixp {
        cmd.arg("--fixp");
    }
    if tool == Tool::Celia {
        cmd.arg("cctas");
    }
    cmd.arg("celeste.p8").env("SDL_VIDEODRIVER", "offscreen").env("SDL_AUDIODRIVER", "dummy").env("ALSOFT_DRIVERS", "null").stdout(log_file.try_clone()?).stderr(log_file);
    super::wait_logged(&mut cmd, std::time::Duration::from_secs(300))?;
    let log = String::from_utf8_lossy(&std::fs::read(&log_path)?).trim_end_matches('\n').to_string();
    let mut lines = Vec::new();
    if o.trace {
        lines.extend(log.lines().filter(|l| l.starts_with("[trace]")).map(str::to_string));
    }
    if o.log {
        lines.push(log.clone());
    }
    // The first match of `pat` (up to the line's end, or `stop`).
    let first = |pat: &str, stop: Option<char>| -> Option<String> {
        log.lines().find_map(|l| {
            let m = &l[l.find(pat)?..];
            Some(stop.and_then(|c| m.find(c)).map_or(m, |i| &m[..i]).to_string())
        })
    };
    // No comma inside a status: callers read "STATUS, N".
    let f = file.display();
    let outcome = match tool {
        Tool::Uct => {
            let n = match std::fs::read_to_string(&saved) {
                Ok(t) => crate::concrete::parse_tas(&t).with_context(|| format!("UCT's saved {}", saved.display()))?.1.len() as u32,
                Err(_) => 0,
            };
            let status = if let Some(err) = first("LUA ERROR: ", None) {
                format!("DID NOT FINISH ({})", err.replace(',', " "))
            } else if let Some(died) = first("DIED at ", Some(';')) {
                format!("DID NOT FINISH ({})", died.replace(',', " ").replacen("DIED", "died", 1))
            } else if log.contains("Saved compressed") {
                "finished, clean save".to_string()
            } else {
                "DID NOT FINISH (raw save only)".to_string()
            };
            let legacy = PathBuf::from(format!("{}/.local/share/love/CelesteTAS", home()));
            let _ = std::fs::create_dir_all(&legacy).and_then(|_| std::fs::copy(&saved, legacy.join(format!("TAS{level}.tas"))));
            lines.push(format!("level {level} {f}: {status}, {n} inputs = {}f -> {}", n as i64 - 1, saved.display()));
            Outcome { finished: status.starts_with("finished"), status, count: n, lines }
        }
        Tool::Celia => {
            lines.extend(log.lines().filter(|l| l.starts_with("[celia] level")).map(|l| format!("# {l}")));
            let fin = first("[celia] FINISHED ", None).and_then(|m| m["[celia] FINISHED ".len()..].split('f').next().filter(|d| !d.is_empty() && d.bytes().all(|b| b.is_ascii_digit())).map(str::to_string));
            let bad = log.lines().find_map(|l| {
                let at = ["DIED", "TIMEOUT", "ERROR", "LUA ERROR"].iter().filter_map(|w| l.find(&format!("[celia] {w}"))).min()?;
                Some(l[at + "[celia] ".len()..].replace(',', " "))
            });
            match fin {
                Some(n) => {
                    lines.push(format!("level {level} {f}: finished, {n}f -> {}", saved.display()));
                    Outcome { finished: true, status: "finished".into(), count: n.parse()?, lines }
                }
                None => {
                    let status = format!("DID NOT FINISH ({})", bad.unwrap_or_else(|| "no result".into()));
                    lines.push(format!("level {level} {f}: {status}, 0f"));
                    Outcome { finished: false, status, count: 0, lines }
                }
            }
        }
    };
    Ok(outcome)
}

/// A file played by `tool`, with one dash where `one_dash` (the gemskip
/// categories).
pub fn play(tool: Tool, level: i16, file: &Path, one_dash: bool, exact: Option<&Path>) -> Result<Outcome> {
    let o = Opts { dashes: one_dash.then_some(1), exact: exact.map(Path::to_path_buf), ..Default::default() };
    let mut out = run(tool, &o, &[(level, file.to_path_buf())])?;
    out.pop().map_or_else(|| bail!("no outcome"), Ok)
}

/// The `[seeds]` list with every BALLOON's phase moved by k * 0.0001, k =
/// -1, +1, -2, +2, ... (`slots`: the kinds the list runs over, in order). A
/// seed does not mean exactly the same in UCT and PICO-8: the phase
/// advances 0.01 a frame in UCT's doubles and 0x0.028f (0.0099945) in
/// 16.16, so where a balloon's y meets an integer (seed 0: y = start EXACTLY
/// at offset 0.5) the touch can differ. Every candidate must be checked in
/// BOTH; chests (a berry position) are kept.
pub fn nudged_seeds(seeds: &[String], slots: &[&str], steps: u32) -> Result<Vec<Vec<String>>> {
    let mut vals: Vec<String> = seeds.to_vec();
    while vals.len() < slots.len() {
        vals.push("0".into());
    }
    let mut out = Vec::new();
    for k in 1..=steps {
        for sign in [-1.0f64, 1.0] {
            let mut list = Vec::with_capacity(vals.len());
            for (i, v) in vals.iter().enumerate() {
                if slots.get(i) == Some(&"balloon") {
                    let x: f64 = v.trim().parse().with_context(|| format!("a seed {v:?}"))?;
                    let r = (x + sign * f64::from(k) * 1e-4).rem_euclid(1.0);
                    let s = format!("{:.4}", if r == 0.0 { 0.0 } else { r });
                    let s = s.trim_end_matches('0').trim_end_matches('.');
                    list.push(if s.is_empty() { "0".to_string() } else { s.to_string() });
                } else {
                    list.push(v.clone());
                }
            }
            out.push(list);
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    /// The nudges as the runner has always tried them.
    #[test]
    fn balloon_seeds_are_nudged_and_chests_kept() {
        let n = super::nudged_seeds(&["0".into()], &["balloon", "chest"], 2).expect("seeds");
        assert_eq!(n, [["0.9999", "0"], ["0.0001", "0"], ["0.9998", "0"], ["0.0002", "0"]]);
        let n = super::nudged_seeds(&["0.5".into(), "1".into(), "7".into()], &["chest", "balloon"], 1).expect("seeds");
        assert_eq!(n, [["0.5", "0.9999", "7"], ["0.5", "0.0001", "7"]]);
    }
}
