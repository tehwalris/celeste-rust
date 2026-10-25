//! THE FLOAT ORACLE'S GOLDENS (plans/float.md): UniversalClassicTas's and
//! Celia's exact traces of the tasdatabase's files (`rewrite golden uct|
//! celia`), which `trace::oracle`'s tests replay through the float
//! reference engine.
//!
//! Per file: played in the tool with the exact trace (`love::Opts::exact`),
//! AS IT IS (the traced cart buffers a jump press as the original does,
//! plans/correctness.md C7). The trace, the draw-only objects dropped, goes
//! to `DIR/KEY_LEVEL.trace.zst` (zstd -19) and a line to
//! `DIR/manifest.txt`:
//!
//! ```text
//! KEY LEVEL room=X,Y seeds=S1,S2|- gemskip=0|1 updates=N uct=STATUS
//! KEY LEVEL room=X,Y seeds=S1,S2|- gemskip=0|1 jank=J|- updates=N celia=STATUS
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

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use super::love::{self, Tool};
use super::tasdb;

/// Our unsubmitted routes' goldens: outside the repository.
pub const LOCAL: &str = "/var/tmp/float-goldens";

/// The repository (the binary's checkout).
fn repo() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
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

/// What `played` found.
pub struct Played {
    pub seeds: Vec<String>,
    pub inputs: Vec<u8>,
    /// The tool's trace of the file.
    pub trace: PathBuf,
    pub status: String,
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
/// `path`: that file, played as category `cat`) as `tool` plays it.
pub fn played(tool: Tool, cat: &str, level: i16, tmp: &Path, path: Option<&Path>) -> Result<std::result::Result<Played, Skip>> {
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
    let trace = tmp.join("file.trace");
    let status = play(tool, level, &path, one_dash, &trace);
    if status.contains("LUA ERROR") || std::fs::metadata(&trace).map_or(true, |m| m.len() == 0) {
        return skip(format!("{tname}: {status}"));
    }
    Ok(Ok(Played { seeds, inputs, trace, status }))
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
    let c = match played(tool, cat, *level, tmp, path.as_deref())? {
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
        Tool::Uct => format!("{key} {level} room={},{} seeds={seeds} gemskip={gemskip} updates={updates} uct={status}", room.0, room.1),
        Tool::Celia => {
            let j = crate::starts::celia_jank(room)?.map_or("-".to_string(), |j| j.to_string());
            format!("{key} {level} room={},{} seeds={seeds} gemskip={gemskip} jank={j} updates={updates} celia={status}", room.0, room.1)
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
}
