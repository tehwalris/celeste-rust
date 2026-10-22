//! THE FLOAT ORACLE'S GOLDENS (plans/float.md): UniversalClassicTas's and
//! Celia's exact traces of the tasdatabase's files (`rewrite golden uct|
//! celia`), which `trace::oracle`'s tests replay through the float
//! reference engine.
//!
//! Per file: played in the tool with the exact trace (`love::Opts::exact`),
//! its BUFFERED jumps re-timed (`canonical_jumps`) and, where any moved,
//! the re-timed file played too and checked to make the same run (the
//! player's position, remainder and speed after every update, bit for bit)
//! with no buffered jump left. The trace, the draw-only objects dropped,
//! goes to `DIR/KEY_LEVEL.trace.zst` (zstd -19) and a line to
//! `DIR/manifest.txt`:
//!
//! ```text
//! KEY LEVEL room=X,Y seeds=S1,S2|- gemskip=0|1 updates=N uct=STATUS retimed=K
//! KEY LEVEL room=X,Y seeds=S1,S2|- gemskip=0|1 jank=J|- updates=N celia=STATUS retimed=K
//! ```
//!
//! (Celia's J: its IL load's loading jank, `starts::celia_jank`.) A file
//! the tool errors on is skipped. The jobs: a category's levels (`CAT
//! LEVEL...`, `CAT all`); `random N LEVEL...|all`, N random files a level
//! (`random_inputs`: they reach states no community file does - deaths,
//! walls and spikes at odd remainders - as category `randomI`); `witness
//! all`, our validated runs (gates/witnesses/witnesses.txt: `KEY CATEGORY
//! LEVEL FILE`; a run valid in fixed point only dies in the tool, and the
//! golden holds that death); `local all`, our UNSUBMITTED runs, never in
//! the repository (`/var/tmp/float-goldens`, replayed by
//! `trace::oracle::tests::the_local_goldens`). READ THE DIFF before
//! committing: a golden that moved means the tool's run did.

use anyhow::{bail, ensure, Context, Result};
use std::path::{Path, PathBuf};

use super::love::{self, Tool};
use super::tasdb;

/// Our unsubmitted routes' goldens: outside the repository.
pub const LOCAL: &str = "/var/tmp/float-goldens";

const JUMP: u8 = 16;

/// The repository (the binary's checkout).
fn repo() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

/// One exact-trace update: its input (`in`, -1: none), `kf`, the sounds,
/// and the first player's `jbuffer` and `p_jump` after it (`None`: no player).
struct Update {
    input: i64,
    kf: i64,
    sounds: Vec<i64>,
    player: Option<std::collections::BTreeMap<String, String>>,
}

fn read_trace(text: &str) -> Result<Vec<Update>> {
    let mut ups: Vec<Update> = Vec::new();
    let mut obj = false;
    for line in text.lines() {
        let w: Vec<&str> = line.split_whitespace().collect();
        let int = |i: usize| -> Result<i64> { w.get(i).and_then(|s| s.parse().ok()).with_context(|| format!("a trace line {line:?}")) };
        match w.first() {
            None => {}
            Some(&"u") => {
                ups.push(Update { input: int(5)?, kf: int(3)?, sounds: Vec::new(), player: None });
                obj = false;
            }
            Some(&"s") => {
                let s = int(1)?;
                ups.last_mut().context("a sound before the first update")?.sounds.push(s);
            }
            Some(&"o") => {
                let up = ups.last_mut().context("an object before the first update")?;
                obj = w.get(2) == Some(&"player") && up.player.is_none();
                if obj {
                    up.player = Some(Default::default());
                }
            }
            Some(&"f") if obj && matches!(w.get(1), Some(&"jbuffer") | Some(&"p_jump")) => {
                let p = ups.last_mut().and_then(|u| u.player.as_mut()).context("a field outside a player")?;
                p.insert(w[1].to_string(), w.get(2).context("a field without a value")?.to_string());
            }
            _ => {}
        }
    }
    Ok(ups)
}

/// The original cart's objects the traced program does not have: they only
/// draw (smoke, the title, the lifeup's "1000"), or only the summit's flag
/// and room (3,1)'s memorial (`trace::cart::room_objects`). The goldens
/// leave them out, and their reader (`trace::oracle`) drops them.
pub const DRAW_ONLY: &[&str] = &["smoke", "room_title", "lifeup", "message", "flag"];

/// A C99 hex float as LuaJIT's `string.format("%a")` writes it
/// (`-0x1.999999999999ap-4`, `0x0p+0`): exact.
pub fn hexfloat(s: &str) -> Result<f64> {
    let (neg, t) = match s.strip_prefix('-') {
        Some(t) => (true, t),
        None => (false, s),
    };
    let t = t.strip_prefix("0x").ok_or_else(|| anyhow::anyhow!("{s:?}: not a hex float"))?;
    let (mant, exp) = t.split_once('p').ok_or_else(|| anyhow::anyhow!("{s:?}: no exponent"))?;
    let (int, frac) = mant.split_once('.').unwrap_or((mant, ""));
    ensure!(int.len() == 1 && frac.len() <= 13, "{s:?}: not a normalized double");
    let m = u64::from_str_radix(&format!("{int}{frac}"), 16).with_context(|| format!("{s:?}: the mantissa"))?;
    let e: i32 = exp.parse().with_context(|| format!("{s:?}: the exponent"))?;
    let e = e - 4 * frac.len() as i32;
    ensure!(m < (1 << 53) && (-1000..1000).contains(&e), "{s:?}: out of the modelled range");
    // m < 2^53 converts exactly; a power of two scales exactly.
    let v = m as f64 * f64::powi(2.0, e);
    Ok(if neg { -v } else { v })
}

/// Whether `up` jumped: its player's `jbuffer` positive at the jump check,
/// 0 after it. A player new in `up` starts with `jbuffer` 0 and no `p_jump`
/// (it updates in the frame it is made).
fn jumped(prev: &Update, up: &Update) -> Result<bool> {
    let fresh_player = [("p_jump", "false"), ("jbuffer", "0x0p+0")].iter().map(|(k, v)| (k.to_string(), v.to_string())).collect();
    let a = prev.player.as_ref().filter(|p| !p.is_empty()).unwrap_or(&fresh_player);
    let Some(b) = &up.player else { return Ok(false) };
    let get = |p: &std::collections::BTreeMap<String, String>, k: &str| p.get(k).cloned().with_context(|| format!("a player without `{k}`"));
    let fresh = up.input.max(0) as u8 & JUMP != 0 && get(a, "p_jump")? == "false";
    if fresh && get(b, "p_jump")? == "false" {
        // The player's update did not run (`freeze`, `pause_player`): it
        // would have set `p_jump`.
        return Ok(false);
    }
    let jb = hexfloat(&get(a, "jbuffer")?)?;
    let expected = if fresh { 4.0 } else { (jb - 1.0).max(0.0) };
    Ok(expected > 0.0 && hexfloat(&get(b, "jbuffer")?)? == 0.0)
}

/// A re-timing that cannot be made (`canonical_jumps`' refusal).
#[derive(Debug)]
pub struct Retime(pub String);

/// A TAS file (tasdatabase format) with its BUFFERED jumps re-timed, from
/// `tool`'s exact trace of it. FENCED with plans/correctness.md C7: the
/// original cart buffers a jump press for 4 frames (`jbuffer`); the traced
/// program jumps only on a fresh press. A jump the buffer made - a jump on
/// an update with no FRESH jump press - is moved to its own frame: the
/// press it came from and every input until the jump release the button,
/// the jump's input presses it. Every other input is kept; a buffered jump
/// past the file's end (the tool plays no buttons there) extends the file
/// up to its press. A JUMP is read from the player's state (`jumped`); UCT's
/// traces also record the jump sounds (`psfx` 1 and 2), and the two must
/// agree on every update that leaves a player. The update that consumed
/// file input k: UCT's (k+1)th update with an input; Celia's with `kf` k + 1.
/// `name` names the file in a refusal.
pub fn canonical_jumps(tool: Tool, trace: &str, tas: &str, name: &str) -> Result<std::result::Result<String, Retime>> {
    let ups = read_trace(trace)?;
    let text = tas.trim();
    let head = text.split_once(']').map_or(text, |(h, _)| h);
    let inputs: Vec<i64> = crate::concrete::parse_tas(text)?.1.into_iter().map(i64::from).collect();
    let refuse = |why: String| Ok(Err(Retime(why)));
    // The file input each update consumed, and what the tool played for it.
    let k_of: Vec<(usize, i64)> = match tool {
        Tool::Uct => ups.iter().enumerate().filter(|(_, u)| u.input >= 0).enumerate().map(|(k, (n, _))| (n, k as i64)).collect(),
        Tool::Celia => ups.iter().enumerate().map(|(n, u)| (n, u.kf - 1)).collect(),
    };
    // Python's list indexing, negative from the end.
    let at = |v: &[i64], k: i64| -> Option<i64> { usize::try_from(if k < 0 { v.len() as i64 + k } else { k }).ok().and_then(|i| v.get(i).copied()) };
    let mut given: Vec<(i64, i64)> = Vec::new();
    for &(n, k) in &k_of {
        match given.iter().find(|(g, _)| *g == k) {
            Some(&(_, v)) if v != ups[n].input => return refuse(format!("{name}: input {k} played as {v} and {}", ups[n].input)),
            Some(_) => {}
            None => given.push((k, ups[n].input)),
        }
    }
    if given.iter().any(|&(k, v)| v != if k < inputs.len() as i64 { at(&inputs, k).unwrap_or(i64::MIN) } else { 0 }) {
        return refuse(format!("{name}: the trace's inputs are not the file's"));
    }
    let top = given.iter().map(|g| g.0).max().unwrap_or(-1);
    let mut original = inputs.clone();
    while (original.len() as i64) < top + 1 {
        original.push(0);
    }
    let mut out = original.clone();
    let mut end = inputs.len();
    // Presses as the ORIGINAL run made them (a re-timed jump must not hide
    // the press of the next one).
    let held = |k: i64| k >= 0 && at(&original, k).is_some_and(|v| v as u8 & JUMP != 0);
    for n in 1..ups.len() {
        let jump = jumped(&ups[n - 1], &ups[n])?;
        // (A player killed, or a room left, in the update may still jump: no
        // player is left to read, and nothing is to re-time.)
        if tool == Tool::Uct && ups[n].player.is_some() && jump != ups[n].sounds.iter().any(|s| *s == 1 || *s == 2) {
            return refuse(format!("{name}: update {n}: the player's state says {} jump, the sounds do not", if jump { "a" } else { "no" }));
        }
        let Some(&(_, k)) = k_of.iter().find(|(m, _)| *m == n) else { continue };
        if !jump || (held(k) && !held(k - 1)) {
            continue;
        }
        // The press it came from: the original's latest fresh press before.
        let Some(p) = (0..k).rev().find(|&q| held(q) && !held(q - 1)) else {
            return refuse(format!("{name}: a buffered jump at update {n} with no press before it"));
        };
        for q in p..k {
            out[q as usize] &= !i64::from(JUMP);
        }
        out[k as usize] |= i64::from(JUMP);
        end = end.max(k as usize + 1);
    }
    Ok(Ok(format!("{head}]{}", out[..end].iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","))))
}

/// The player's (x, y, rem, spd) after every update.
fn player_path(trace: &str) -> Vec<std::collections::BTreeMap<String, String>> {
    let (mut out, mut obj) = (Vec::new(), "");
    for line in trace.lines() {
        let w: Vec<&str> = line.split_whitespace().collect();
        match w.first() {
            Some(&"u") => out.push(std::collections::BTreeMap::new()),
            Some(&"o") => obj = w.get(2).copied().unwrap_or(""),
            Some(&"f") if obj == "player" && matches!(w.get(1), Some(&"x") | Some(&"y") | Some(&"rem.x") | Some(&"rem.y") | Some(&"spd.x") | Some(&"spd.y")) => {
                if let (Some(cur), Some(v)) = (out.last_mut(), w.get(2)) {
                    cur.insert(w[1].to_string(), v.to_string());
                }
            }
            _ => {}
        }
    }
    out
}

/// The trace without the draw-only objects (`DRAW_ONLY`,
/// which its reader drops too).
fn strip(trace: &str) -> String {
    let mut keep = true;
    let mut out = String::new();
    for line in trace.split_inclusive('\n') {
        let w: Vec<&str> = line.split_whitespace().collect();
        match w.first() {
            Some(&"o") => keep = !DRAW_ONLY.contains(&w.get(2).copied().unwrap_or("")),
            Some(&"u") | Some(&"g") | Some(&"s") => keep = true,
            _ => {}
        }
        if keep {
            out += line;
        }
    }
    out
}

/// The trace's first `n` updates.
fn cut(text: &str, n: usize) -> String {
    let (mut out, mut k) = (String::new(), -1i64);
    for line in text.split_inclusive('\n') {
        if line.starts_with("u ") {
            k += 1;
            if k >= n as i64 {
                break;
            }
        }
        out += line;
    }
    out
}

/// Random file `i` of `level`: a direction held 2-10 frames (biased right
/// and up, where most exits are), jump pressed on 12% of frames, dash on 6%.
pub fn random_inputs(i: usize, level: i16) -> Vec<u8> {
    let mut r = super::pyrandom::Random::new(&format!("uct-random-{i}-{level}"));
    let mut out = Vec::new();
    while out.len() < 90 {
        let d = r.choice(&[0u8, 1, 2, 2, 2, 4, 5, 6, 6, 8, 9, 10]);
        for _ in 0..r.randint(2, 10) {
            out.push(d | if r.random() < 0.12 { 16 } else { 0 } | if r.random() < 0.06 { 32 } else { 0 });
        }
    }
    out.truncate(90);
    out
}

/// A file no golden is made of, and why.
pub struct Skip(pub String);

/// What `canonical` found.
pub struct Canonical {
    pub seeds: Vec<String>,
    pub inputs: Vec<u8>,
    /// The tool's trace of the file played (the re-timed one where any moved).
    pub trace: PathBuf,
    pub status: String,
    pub retimed: usize,
}

/// The tool's run of `path` as category `cat` (`one_dash`), its exact trace
/// to `trace`: its status as the goldens record it (`finished`, `DID NOT
/// FINISH (...)`).
fn play(tool: Tool, level: i16, path: &Path, one_dash: bool, trace: &Path) -> String {
    let o = love::Opts { dashes: one_dash.then_some(1), exact: Some(trace.to_path_buf()), ..Default::default() };
    match love::run(tool, &o, &[(level, path.to_path_buf())]) {
        Ok(mut v) => v.pop().map_or_else(|| "no result".into(), |o| o.status.split(',').next().unwrap_or("").to_string()),
        Err(e) => format!("no result ({e})"),
    }
}

/// The category's file for `level` (a `randomI` category: random file I;
/// `path`: that file, played as category `cat`) as `tool` plays it, its
/// buffered jumps re-timed.
pub fn canonical(tool: Tool, cat: &str, level: i16, tmp: &Path, path: Option<&Path>) -> Result<std::result::Result<Canonical, Skip>> {
    let skip = |why: String| Ok(Err(Skip(why)));
    // The goldens' categories include our own (`random0`, a witness's key):
    // those are any%.
    let one_dash = cat.parse::<crate::settings::Category>().is_ok_and(crate::settings::Category::gemskip);
    let path = match (path, cat.strip_prefix("random")) {
        (Some(p), _) => p.to_path_buf(),
        (None, Some(i)) => {
            let p = tmp.join("random.tas");
            std::fs::write(&p, tasdb::text(&[], &random_inputs(i.parse()?, level)))?;
            p
        }
        (None, None) => match tasdb::entries(cat)?.into_iter().find(|e| e.file.as_deref() == Some(tasdb::file_of_level(level).as_str())) {
            Some(e) => tasdb::path(cat, &e)?,
            None => return skip("no file".into()),
        },
    };
    let (seeds, inputs) = tasdb::read(&path)?;
    let tname = match tool {
        Tool::Uct => "uct",
        Tool::Celia => "celia",
    };
    let orig = tmp.join("orig.trace");
    let status = play(tool, level, &path, one_dash, &orig);
    if status.contains("LUA ERROR") || std::fs::metadata(&orig).map_or(true, |m| m.len() == 0) {
        return skip(format!("{tname}: {status}"));
    }
    let orig_text = std::fs::read_to_string(&orig)?;
    let canon_text = match canonical_jumps(tool, &orig_text, &std::fs::read_to_string(&path)?, &path.display().to_string())? {
        Ok(t) => t,
        Err(Retime(why)) => return skip(format!("re-timing: {why}")),
    };
    let canon = tmp.join("canon.tas");
    std::fs::write(&canon, &canon_text)?;
    let (_, canon_inputs) = tasdb::read(&canon)?;
    let retimed = inputs.iter().zip(&canon_inputs).filter(|(a, b)| a != b).count();
    if retimed == 0 {
        return Ok(Ok(Canonical { seeds, inputs, trace: orig, status, retimed }));
    }
    let fin = tmp.join("canon.trace");
    let status2 = play(tool, level, &canon, one_dash, &fin);
    let fin_text = std::fs::read_to_string(&fin).unwrap_or_default();
    let (a, b) = (player_path(&orig_text), player_path(&fin_text));
    // A file extended past its end (a buffered jump there) runs out a
    // driver's clock later: the same run, longer, where neither finished
    // nor died.
    let extended = canon_inputs.len() > inputs.len();
    let clock = |st: &str| !st.contains("finished") && !st.to_lowercase().contains("died");
    let longest = a.len().max(b.len());
    let n = if extended && clock(&status) && clock(&status2) { a.len().min(b.len()) } else { longest };
    if a.len() < n || b.len() < n || a[..n] != b[..n] || (status2 != status && n == longest) {
        let first = a.iter().zip(&b).position(|(x, y)| x != y).unwrap_or(a.len().min(b.len()));
        return skip(format!("the re-timed file runs differently (update {first}; {status} -> {status2})"));
    }
    match canonical_jumps(tool, &fin_text, &canon_text, &canon.display().to_string())? {
        Ok(again) if again == canon_text => {}
        Ok(_) => return skip("a buffered jump is left after re-timing".into()),
        Err(Retime(why)) => bail!("re-timing the re-timed file: {why}"),
    }
    Ok(Ok(Canonical { seeds, inputs: canon_inputs, trace: fin, status: status2, retimed }))
}

/// `trace` written as a golden to `out` (zstd -19), the draw-only objects
/// dropped; a playback that never ends (the summit has no exit to
/// clean-save at) runs to the driver's timeout: kept to 30 updates past the
/// update that played the file's last input (`last`, given the `u` lines'
/// words). The file is rewritten only where its content changed. The
/// updates kept.
fn write_golden(trace: &Path, out: &Path, last: impl Fn(&[Vec<&str>]) -> Result<Option<usize>>) -> Result<usize> {
    let mut text = strip(&std::fs::read_to_string(trace)?);
    let ups: Vec<Vec<&str>> = text.lines().filter(|l| l.starts_with("u ")).map(|l| l.split_whitespace().collect()).collect();
    if let Some(end) = last(&ups)? {
        text = cut(&text, end + 1 + 30);
    }
    let same = std::fs::read(out).ok().and_then(|z| zstd::decode_all(&z[..]).ok()).is_some_and(|old| old == text.as_bytes());
    if !same {
        let mut enc = zstd::stream::Encoder::new(Vec::new(), 19)?;
        enc.include_checksum(true)?;
        std::io::Write::write_all(&mut enc, text.as_bytes())?;
        std::fs::write(out, enc.finish()?)?;
    }
    Ok(text.lines().filter(|l| l.starts_with("u ")).count())
}

/// One job: (manifest key, level, category, the file or none).
pub type Job = (String, i16, String, Option<PathBuf>);

/// One golden of `tool`: its manifest line, or why it was skipped.
fn one(tool: Tool, job: &Job, tmp: &Path, out: &Path) -> Result<std::result::Result<String, String>> {
    let (key, level, cat, path) = job;
    let c = match canonical(tool, cat, *level, tmp, path.as_deref())? {
        Ok(c) => c,
        Err(Skip(why)) => return Ok(Err(why)),
    };
    let room = tasdb::room_of(*level);
    let n = c.inputs.len();
    let golden = out.join(format!("{key}_{level}.trace.zst"));
    let word = |w: &[&str], i: usize| -> Result<i64> { w.get(i).and_then(|s| s.parse().ok()).context("an update line") };
    let updates = match tool {
        // The update that played UCT's file's last input, if the trace goes
        // on past it (its `in` is -1 where no input is played).
        Tool::Uct => write_golden(&c.trace, &golden, |ups| {
            let played: Vec<usize> = ups.iter().map(|w| word(w, 5)).collect::<Result<Vec<_>>>()?.into_iter().enumerate().filter(|(_, v)| *v >= 0).map(|(i, _)| i).collect();
            Ok((played.len() > n).then(|| played[n.max(1) - 1]))
        })?,
        // The step that played Celia's file's last input, if the trace goes
        // on past it (its `kf` is the input's index).
        Tool::Celia => write_golden(&c.trace, &golden, |ups| {
            let kf: Vec<i64> = ups.iter().map(|w| word(w, 3)).collect::<Result<_>>()?;
            if kf.iter().all(|&k| k <= n as i64) {
                return Ok(None);
            }
            Ok(Some(kf.iter().rposition(|&k| k <= n as i64).context("no step within the file")?))
        })?,
    };
    let seeds = if c.seeds.is_empty() { "-".to_string() } else { c.seeds.join(",") };
    let gemskip = u8::from(cat.parse::<crate::settings::Category>().is_ok_and(crate::settings::Category::gemskip));
    let status = c.status.replace(' ', "_");
    Ok(Ok(match tool {
        Tool::Uct => format!("{key} {level} room={},{} seeds={seeds} gemskip={gemskip} updates={updates} uct={status} retimed={}", room.0, room.1, c.retimed),
        Tool::Celia => {
            let j = crate::starts::celia_jank(room)?.map_or("-".to_string(), |j| j.to_string());
            format!("{key} {level} room={},{} seeds={seeds} gemskip={gemskip} jank={j} updates={updates} celia={status} retimed={}", room.0, room.1, c.retimed)
        }
    }))
}

/// The jobs of the command line `args`.
pub fn jobs(args: &[String]) -> Result<Vec<Job>> {
    let (cat, levels) = args.split_first().context("the jobs: CAT LEVEL..., CAT all, random N LEVEL...|all, witness all, local all")?;
    if cat == "witness" || cat == "local" {
        let (listing, base) = if cat == "witness" { (repo().join("gates/witnesses/witnesses.txt"), repo().to_path_buf()) } else { (Path::new(LOCAL).join("witnesses/witnesses.txt"), PathBuf::from(LOCAL)) };
        ensure!(levels == ["all"], "{cat} all: the witnesses are {}'s", listing.display());
        let mut out = Vec::new();
        for line in std::fs::read_to_string(&listing).with_context(|| format!("reading {}", listing.display()))?.lines() {
            let w: Vec<&str> = line.split_whitespace().collect();
            if w.first().is_some_and(|w| !w.starts_with('#')) {
                ensure!(w.len() >= 4, "{}: {line:?} is not KEY CATEGORY LEVEL FILE", listing.display());
                out.push((w[0].to_string(), w[2].parse()?, w[1].to_string(), Some(base.join(w[3]))));
            }
        }
        return Ok(out);
    }
    let level_list = |ls: &[String]| -> Result<Vec<i16>> { ls.iter().map(|l| l.parse::<i16>().with_context(|| format!("a level {l:?}"))).collect() };
    let (cats, levels): (Vec<String>, Vec<i16>) = if cat == "random" {
        let (n, ls) = levels.split_first().context("random N LEVEL...|all")?;
        let cats = (0..n.parse::<usize>()?).map(|i| format!("random{i}")).collect();
        (cats, if ls == ["all"] { (1..32).collect() } else { level_list(ls)? })
    } else if levels == ["all"] {
        let mut ls: Vec<i16> = tasdb::entries(cat)?.iter().filter_map(|e| e.file.as_ref()).map(|f| tasdb::level_of_file(Path::new(f))).collect::<Result<_>>()?;
        ls.sort();
        ls.dedup();
        (vec![cat.clone()], ls)
    } else {
        (vec![cat.clone()], level_list(levels)?)
    };
    Ok(cats.iter().flat_map(|c| levels.iter().map(move |&l| (c.clone(), l, c.clone(), None))).collect())
}

/// Where `args`' goldens go: the repository's gates/TOOL, or the local
/// store's for `local`.
pub fn out_dir(tool: Tool, args: &[String]) -> PathBuf {
    let name = match tool {
        Tool::Uct => "uct",
        Tool::Celia => "celia",
    };
    if args.first().map(String::as_str) == Some("local") {
        Path::new(LOCAL).join(name)
    } else {
        repo().join("gates").join(name)
    }
}

/// The goldens of `args` made into their directory, its manifest updated:
/// per job its line, or why it was skipped, printed.
pub fn run(tool: Tool, args: &[String]) -> Result<()> {
    let out = out_dir(tool, args);
    std::fs::create_dir_all(&out)?;
    let manifest = out.join("manifest.txt");
    let mut lines: Vec<String> = std::fs::read_to_string(&manifest).map(|t| t.lines().map(str::to_string).collect()).unwrap_or_default();
    for job in jobs(args)? {
        let tmp = tempdir_in("/var/tmp")?;
        let res = one(tool, &job, &tmp, &out);
        let _ = std::fs::remove_dir_all(&tmp);
        let (key, level) = (&job.0, job.1.to_string());
        lines.retain(|l| l.split_whitespace().take(2).ne([key.as_str(), level.as_str()]));
        match res? {
            Ok(line) => {
                println!("{line}");
                lines.push(line);
            }
            Err(why) => println!("{key} {level}: skipped: {why}"),
        }
    }
    let sort_key = |l: &String| -> (String, i64) {
        let mut w = l.split_whitespace();
        (w.next().unwrap_or("").to_string(), w.next().and_then(|v| v.parse().ok()).unwrap_or(0))
    };
    lines.sort_by_key(sort_key);
    std::fs::write(&manifest, lines.join("\n") + "\n")?;
    Ok(())
}

/// A fresh directory under `dir`.
fn tempdir_in(dir: &str) -> Result<PathBuf> {
    static N: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let d = Path::new(dir).join(format!("celeste-golden-{}-{}", std::process::id(), N.fetch_add(1, std::sync::atomic::Ordering::Relaxed)));
    let _ = std::fs::remove_dir_all(&d);
    std::fs::create_dir_all(&d)?;
    Ok(d)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hex_floats_read_as_python_reads_them() {
        for (s, v) in [("0x0p+0", 0.0), ("0x1p+2", 4.0), ("0x1.8p+1", 3.0), ("-0x1p-1", -0.5), ("0x1.999999999999ap-4", 0.1)] {
            assert_eq!(hexfloat(s).expect("hex"), v, "{s}");
        }
    }

    /// A jump the buffer made moves to its own frame: pressed at file input
    /// 1 (the player's update did not jump), jumping at input 3.
    #[test]
    fn a_buffered_jump_is_retimed() {
        let up = |n: usize, input: i64, jb: &str, pj: &str, sounds: &str| format!("u {n} kf {} in {input}\n{sounds}o 1 player\nf jbuffer {jb}\nf p_jump {pj}\n", n + 1);
        let trace = [up(0, 0, "0x0p+0", "false", ""), up(1, 16, "0x1p+2", "true", ""), up(2, 16, "0x1.8p+1", "true", ""), up(3, 0, "0x0p+0", "false", "s 1\n")].concat();
        let out = canonical_jumps(Tool::Uct, &trace, "[0.5]0,16,16,0", "x").expect("parses").map_err(|r| r.0).expect("re-timed");
        assert_eq!(out, "[0.5]0,0,0,16");
        let bad = canonical_jumps(Tool::Uct, &trace, "[0.5]0,16,16,1", "x").expect("parses").map_err(|r| r.0).expect_err("not the file");
        assert!(bad.contains("the trace's inputs are not the file's"), "{bad}");
    }
}
