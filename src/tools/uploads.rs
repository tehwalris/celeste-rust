//! TASDATABASE FILES IN EVERY TOOL AND START MODE (`rewrite validate
//! uploads`): UniversalClassicTas, Celia, and the original cart on a real
//! PICO-8 - loaded alone (the IL load), and in REAL PLAY: through the real
//! transition from the previous room, and from the game's boot.
//!
//! One tab-separated row per file:
//! - ours: the file's count (inputs - 1), and the database's;
//! - IL: PICO-8, the room loaded directly (UCT's IL load), its count;
//! - UCT: finished on the FIRST attempt (a death is a failure);
//! - Celia: Celia's IL load with its loading jank;
//! - chain: PICO-8 from the previous room, played with the database's file,
//!   through the real transition; boot: the same from 100m;
//! - VALID: the boot chain (real play, the ground truth) exits by the
//!   file's count (100%: with the berry), and Celia agrees. The chain from
//!   the previous room is informative: that room itself starts there by an
//!   IL load, so its own database file can fail ("PREVIOUS ROOM FAILED").
//!
//! With `nudge`, where UCT did not finish: the balloon seed nudges
//! (`love::nudged_seeds`), each checked in PICO-8 (the IL exit must not
//! move) and UCT.

use anyhow::{Context, Result};
use std::path::Path;

use super::chain;
use super::love::{self, Tool};
use super::pico8::Runner;
use super::tasdb;
use super::tasdb::Cat;

/// A file's category: `--cat`, else its directory's name (`CAT/TASn.tas`,
/// or `CAT/upload/TASn.tas`).
pub fn cat_of(path: &Path, cat: Option<Cat>) -> Result<Cat> {
    if let Some(c) = cat {
        return Ok(c);
    }
    let abs = std::path::absolute(path)?;
    let d = abs.parent().context("a file in a directory")?;
    let d = if d.file_name().is_some_and(|n| n == "upload") { d.parent().context("upload's parent")? } else { d };
    let name = d.file_name().and_then(|n| n.to_str()).unwrap_or("");
    name.parse::<Cat>().map_err(|e| anyhow::anyhow!("{}: category {name:?} unknown (--cat): {e}", path.display()))
}

/// A chain's last segment: its (count, berry) if it exits, and its summary.
fn result(lines: &[String], nsegs: usize) -> (Option<(i64, bool)>, String) {
    let (s, last) = chain::summary("", lines, nsegs);
    let r = last.map(|e| (e.get("count").and_then(|c| c.parse().ok()).unwrap_or(-1), e.get("berry").map(String::as_str) == Some("true")));
    (r, s.trim_matches(|c| c == ':' || c == ' ').to_string())
}

/// The row of `path`.
pub fn check(r: &Runner, path: &Path, nudge: bool, cat: Option<Cat>) -> Result<String> {
    let cat = cat_of(path, cat)?;
    let level = tasdb::level_of_file(path)?;
    let room = tasdb::room_of(level);
    let hundred = cat.search()?.hundred();
    let (seeds, inputs) = tasdb::read(path)?;
    let ours = inputs.len() as i64 - 1;
    let dbn = tasdb::entries(&cat.to_string())?.into_iter().find(|e| e.file.as_deref() == Some(tasdb::file_of_level(level).as_str())).and_then(|e| e.frames);
    let (il, il_s) = result(&chain::single(r, cat, level, &seeds, &inputs, None, false)?, 1);
    let uct = love::play(Tool::Uct, level, path, cat.one_dash(), None)?;
    let n = (uct.count > 0).then(|| uct.count - 1);
    let celia = love::play(Tool::Celia, level, path, cat.one_dash(), None)?;
    let (_, ch_s) = result(&chain::chain(r, cat, level, &seeds, &inputs, (level - 1).max(1), false, true)?, if level > 1 { 2 } else { 1 });
    let (first, bt, bt_s) = boot(r, cat, level, &seeds, &inputs)?;
    // The file's count is what the tasdatabase bot takes (inputs - 1); a
    // trailing input after the exit does not invalidate it, an exit after it does.
    let ok = |r: Option<(i64, bool)>| r.is_some_and(|(count, berry)| count <= ours && (berry || !hundred));
    let celia_ok = celia.status.starts_with("finished") && i64::from(celia.count) <= ours;
    let valid = ok(bt) && celia_ok;
    let none = |v: Option<String>| v.unwrap_or_else(|| "None".to_string());
    let mut row = vec![
        format!("{cat} TAS{level} room {},{}", room.0, room.1),
        format!("ours {ours} db {}", none(dbn.map(|n| n.to_string()))),
        format!("IL {il_s}"),
        format!("UCT {} {}", uct.status, none(n.map(|n| n.to_string()))),
        format!("Celia {} {}", celia.status, celia.count),
        format!("chain {ch_s}"),
        format!("boot {bt_s}"),
        if valid { "VALID".to_string() } else { format!("INVALID{}", if first > 1 { " (boot chain broken by a database file)" } else { "" }) },
    ];
    if nudge && !uct.status.starts_with("finished") {
        let slots: Vec<&str> = crate::trace::cart::room_objects(room).into_iter().filter(|o| o.seeded).map(|o| o.kind).collect();
        let mut found = None;
        for sd in love::nudged_seeds(&seeds, &slots, 10)? {
            if result(&chain::single(r, cat, level, &sd, &inputs, None, false)?, 1).0 != il {
                continue;
            }
            let tmp = std::env::temp_dir().join(format!("check_uploads-{}-{level}.tas", std::process::id()));
            std::fs::write(&tmp, tasdb::text(&sd, &inputs))?;
            let u = love::play(Tool::Uct, level, &tmp, cat.one_dash(), None);
            let _ = std::fs::remove_file(&tmp);
            let u = u?;
            if u.finished {
                found = Some(format!("nudge [{}] finishes ({})", sd.join(","), i64::from(u.count) - 1));
                break;
            }
        }
        row.push(found.unwrap_or_else(|| "no nudge works".into()));
    }
    Ok(row.join("\t"))
}

/// The boot chain to `level`: its first level, the last segment's result
/// and summary.
///
/// FENCED (plans/correctness.md C9's code path): a database file that dies
/// on PICO-8, or never exits there (it was made in a tool computing in
/// doubles), BREAKS the boot chain: it restarts at the level after it, by an
/// IL load, and says so. (`chain::entry_jank`'s loop is the other one; they
/// differ, and stay apart until C9 is settled.)
fn boot(r: &Runner, cat: Cat, level: i16, seeds: &[String], inputs: &[u8]) -> Result<(i16, Option<(i64, bool)>, String)> {
    let mut first = 1;
    loop {
        let nsegs = (level - first + 1) as usize;
        let lines = chain::chain(r, cat, level, seeds, inputs, first, false, true)?;
        let (bt, mut bt_s) = result(&lines, nsegs);
        // (Each restart's summary replaces the one before: the row shows the
        // last chain's, and where it started.)
        let (exits, bad, _) = chain::parse(&lines);
        let mut early: Vec<usize> = bad.iter().filter_map(|l| chain::death_seg(l)).filter(|&k| k < nsegs).collect();
        if early.is_empty() && bad.is_empty() && exits.len() + 1 < nsegs {
            early = vec![exits.len() + 1];
        }
        let Some(&e) = early.first() else {
            if first > 1 {
                bt_s = format!("(from level {first}) {bt_s}");
            }
            return Ok((first, bt, bt_s));
        };
        first += e as i16;
    }
}

/// `files` checked `jobs` at a time; their rows in order, each printed as
/// soon as it and those before it are done.
pub fn check_all(files: &[std::path::PathBuf], nudge: bool, cat: Option<Cat>, jobs: usize, pico8: Option<&Path>) -> Result<()> {
    let next = std::sync::atomic::AtomicUsize::new(0);
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::scope(|s| -> Result<()> {
        for _ in 0..jobs.max(1).min(files.len().max(1)) {
            let tx = tx.clone();
            let next = &next;
            s.spawn(move || {
                let r = Runner::new(pico8, None);
                loop {
                    let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    let Some(f) = files.get(i) else { break };
                    let row = r.as_ref().map_err(|e| anyhow::anyhow!("{e}")).and_then(|r| check(r, f, nudge, cat)).with_context(|| f.display().to_string());
                    if tx.send((i, row)).is_err() {
                        break;
                    }
                }
            });
        }
        drop(tx);
        let mut done = std::collections::BTreeMap::new();
        let mut printed = 0;
        for (i, row) in rx {
            done.insert(i, row);
            while let Some(row) = done.remove(&printed) {
                println!("{}", row?);
                printed += 1;
            }
        }
        Ok(())
    })
}
