//! THE CATEGORY RUNNER (`rewrite category JOBS OUTDIR`): a queue of
//! (category, room) searches against the community tasdatabase, every
//! result verified end to end.
//!
//! JOBS is a JSON list of `{"cat": "nodiag", "room": "0,0", "name": "100m",
//! "offset": 27, "levels": "r0sxhn,r0sxh", "l1": true, "mem": "60G"}`;
//! optional: `flags` (more flags for the search, e.g. `["--split-frame"]`),
//! `tag`, `timeout` (s), `reuse` (a level-0 tree directory, moved in when it
//! exists and moved back there when the job ends: jobs can count horizons
//! up on one kept tree), `to` (a horizon H instead of the reference:
//! `--frames H` without `--ceiling`, level -1 at H, and a refutation is a
//! result: the optimum is above H), `keep_tree` (leave the tree in OUTDIR
//! for a later job's `reuse`), `witness` (a file of our inputs to verify, no
//! search), `jank` (J or `none` instead of the boot chain's). The file is
//! re-read before every job, so jobs can be appended while it runs; a job
//! whose key is in OUTDIR/results.jsonl is skipped; a job with an unknown
//! key is refused. OUTDIR/STOP stops the queue before the next job.
//!
//! Per job:
//! 0. the room's start as real play enters it: its loading frame from
//!    object J (`chain::entry_jank`: the category's boot chain; a job's
//!    `jank`), for the search (`--loading-jank`) and every replay; every
//!    other start class (`starts`) a search-only job;
//! 1. the reference: the community TAS replayed in the ORIGINAL cart behind
//!    the room's prologue (the earliest offset around `offset` that exits);
//! 2. `rewrite search --frames REF --ceiling --prefer <the community TAS>`
//!    with the category and the level -1 filter if `l1` (retried without it
//!    if its table refuses); its outcome its `result.json`;
//! 3. our witness replayed in the original cart (it must exit at the
//!    optimum; no diagonal dash in a nodiag category, every dash diagonal in
//!    an onlydiag one, as the cart's own dash branch reports them);
//! 4. the upload (OUTDIR/upload/CAT/TAS<n>.tas): the first candidate cut
//!    VALID in REAL PLAY (`uploads::check`: the boot chain on a real PICO-8
//!    and Celia), `real_play` in the result; both files through
//!    UniversalClassicTas for information;
//! 5. an improvement gets a web UI run (`rewrite export-ui`, `runs.json`).
//!
//! The results: one JSON line per job in OUTDIR/results.jsonl, its keys in
//! the order the job set them.

use anyhow::{bail, Context, Result};
use serde_json::{json, Value};
use std::path::{Path, PathBuf};

use super::chain;
use super::love::{self, Tool};
use super::pico8::{self, original_cart, Cart, Play, Runner, Spec, FENCED};
use super::tasdb;
use crate::settings::{Category, Settings};

/// The web UI's data.
const UI: &str = "/var/tmp/celeste-ui/data";

/// A job's keys: what a jobs file gives, and what `expand` adds.
const JOB_KEYS: &[&str] = &["cat", "room", "name", "offset", "levels", "l1", "mem", "flags", "tag", "timeout", "reuse", "to", "keep_tree", "witness", "jank"];

/// A JSON object in the order its keys were first set, written as Python's
/// `json.dumps` writes one (`, `, `: `, ASCII).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Row(pub Vec<(String, Value)>);

impl Row {
    pub fn set(&mut self, k: &str, v: impl Into<Value>) {
        let v = v.into();
        match self.0.iter_mut().find(|(key, _)| key == k) {
            Some(e) => e.1 = v,
            None => self.0.push((k.to_string(), v)),
        }
    }
    pub fn get(&self, k: &str) -> Option<&Value> {
        self.0.iter().find(|(key, _)| key == k).map(|e| &e.1)
    }
    /// `{**self, k: v}`.
    fn with(&self, k: &str, v: impl Into<Value>) -> Row {
        let mut r = self.clone();
        r.set(k, v);
        r
    }
    pub fn dumps(&self) -> String {
        format!("{{{}}}", self.0.iter().map(|(k, v)| format!("{}: {}", py_json(&Value::String(k.clone())), py_json(v))).collect::<Vec<_>>().join(", "))
    }
}

/// `v` as Python's `json.dumps` writes it.
pub fn py_json(v: &Value) -> String {
    match v {
        Value::Null => "null".into(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => match n.as_f64() {
            Some(f) if n.is_f64() => pico8::py_float(&f.to_string()).unwrap_or_else(|_| n.to_string()),
            _ => n.to_string(),
        },
        Value::String(s) => {
            let mut out = String::from("\"");
            for c in s.chars() {
                match c {
                    '"' => out += "\\\"",
                    '\\' => out += "\\\\",
                    '\n' => out += "\\n",
                    '\r' => out += "\\r",
                    '\t' => out += "\\t",
                    c if (c as u32) < 0x20 || (c as u32) > 0x7e => {
                        let mut buf = [0u16; 2];
                        for u in c.encode_utf16(&mut buf) {
                            out += &format!("\\u{u:04x}");
                        }
                    }
                    c => out.push(c),
                }
            }
            out + "\""
        }
        Value::Array(xs) => format!("[{}]", xs.iter().map(py_json).collect::<Vec<_>>().join(", ")),
        Value::Object(m) => format!("{{{}}}", m.iter().map(|(k, v)| format!("{}: {}", py_json(&Value::String(k.clone())), py_json(v))).collect::<Vec<_>>().join(", ")),
    }
}

/// The local time `HH:MM`.
fn hhmm() -> String {
    // SAFETY: `localtime_r` fills `tm` from a valid `time_t`.
    unsafe {
        let t = libc::time(std::ptr::null_mut());
        let mut tm: libc::tm = std::mem::zeroed();
        libc::localtime_r(&t, &mut tm);
        format!("{:02}:{:02}", tm.tm_hour, tm.tm_min)
    }
}

/// A job as the jobs file gives it, `expand`'s keys added.
#[derive(Clone, Debug)]
pub struct Job(pub serde_json::Map<String, Value>);

impl Job {
    fn str(&self, k: &str) -> Result<&str> {
        self.0.get(k).and_then(Value::as_str).with_context(|| format!("job {}: no string `{k}`", self.key()))
    }
    fn opt_str(&self, k: &str) -> Option<&str> {
        self.0.get(k).and_then(Value::as_str)
    }
    fn truthy(&self, k: &str) -> bool {
        self.0.get(k).is_some_and(|v| match v {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Number(n) => n.as_f64() != Some(0.0),
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Object(o) => !o.is_empty(),
        })
    }
    /// `cat/nameTAG`.
    pub fn key(&self) -> String {
        let s = |k: &str| self.0.get(k).and_then(Value::as_str).unwrap_or("");
        format!("{}/{}{}", s("cat"), s("name"), s("tag"))
    }
    fn cat(&self) -> Result<Category> {
        self.str("cat")?.parse().map_err(|e: String| anyhow::anyhow!("job {}: {e}", self.key()))
    }
    fn room(&self) -> Result<(i16, i16)> {
        let r = self.str("room")?;
        let (x, y) = r.split_once(',').with_context(|| format!("job {}: room {r:?}", self.key()))?;
        Ok((x.trim().parse()?, y.trim().parse()?))
    }
}

/// A job from the jobs file, refused with a key it does not know (a typo is
/// not silently ignored).
pub fn check_job(job: Job) -> Result<Job> {
    let mut bad: Vec<String> = job.0.keys().filter(|k| !JOB_KEYS.contains(&k.as_str())).cloned().collect();
    bad.sort();
    if !bad.is_empty() {
        let s = |k: &str| job.0.get(k).and_then(Value::as_str).map_or("None".to_string(), str::to_string);
        bail!("job {}/{}: unknown keys {} (the module's doc lists them)", s("cat"), s("name"), chain::py_list(&bad));
    }
    Ok(job)
}

/// `inputs` replayed in the ORIGINAL cart, the room started as the search
/// starts it (its loading frame from object `jank`), under `seeds` (none:
/// every balloon 0): the frame the room changes during, and the frames.
pub fn replay_exit(r: &Runner, room: (i16, i16), inputs: &[u8], seeds: &[String], cat: Category, jank: Option<usize>) -> Result<(Option<u32>, Vec<pico8::Frame>)> {
    let seeds: Vec<String> = if seeds.is_empty() { vec!["0".into()] } else { seeds.to_vec() };
    let seeds = seeds.iter().map(|s| pico8::py_float(s)).collect::<Result<Vec<_>>>()?;
    let spec = Spec { cart: Cart::Original(original_cart()), play: Play::Room { room, inputs: inputs.to_vec(), seeds: Some(seeds) }, frames: inputs.len() + 3, one_dash: cat.gemskip(), jank: jank.filter(|&j| j > 0), load_draw: false, dump: false, trace_objects: false, fenced: FENCED };
    let fs = pico8::frames(&r.run(&spec)?);
    Ok((pico8::exit_frame(&fs, room), fs))
}

/// The settings of `room`'s witness and starts (`concrete::witness`, `starts`).
fn settings(room: (i16, i16), cat: Category, jank: Option<usize>, seeds: &[String]) -> Result<Settings> {
    let s = Settings { room, category: cat, loading_jank: jank, balloon_seeds: Some(if seeds.is_empty() { "0".into() } else { seeds.join(",") }), ..Settings::default() };
    s.resolved()
}

/// Where the runner runs: its directory, the search binary, the repository.
pub struct Ctx<'a> {
    pub outdir: &'a Path,
    /// The `rewrite` the searches run (a copy, so a rebuild cannot swap it mid-queue).
    pub binary: &'a Path,
    pub runner: Runner,
}

fn safe_run() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("safe-run.sh")
}

/// One job, end to end (module doc): its result row.
pub fn run(job: &Job, cx: &Ctx) -> Result<Row> {
    let (cat, room_s, name) = (job.cat()?, job.str("room")?.to_string(), job.str("name")?.to_string());
    let room = job.room()?;
    let dbcat = tasdb::Cat::try_from(cat)?;
    let r = &cx.runner;
    let tag = job.opt_str("tag").unwrap_or("").to_string();
    let jd = cx.outdir.join(cat.to_string()).join(format!("{name}{tag}"));
    let _ = std::fs::remove_dir_all(&jd);
    std::fs::create_dir_all(&jd)?;
    let mut res = Row::default();
    res.set("key", job.key());
    res.set("cat", cat.to_string());
    res.set("room", room_s.clone());
    res.set("name", name.clone());
    res.set("start", hhmm());
    let Some(entry) = tasdb::entry(&cat.to_string(), &name)? else { return Ok(res.with("status", "no database entry")) };
    let dbpath = tasdb::path(&cat.to_string(), &entry)?;
    let (mut seeds, dbin) = tasdb::read(&dbpath)?;
    let level = tasdb::level_of_name(&name);
    res.set("db_file", entry.file.clone());
    res.set("db_frames", entry.frames);
    // 0. the room's start as REAL PLAY enters it (the category's boot chain
    // measures J; a job's "jank" overrides it, "none": the IL load). The
    // search, every replay and the reference start so.
    let jank: Option<usize> = match job.0.get("jank") {
        None => chain::entry_jank(r, dbcat, tasdb::level_of(room))?,
        Some(Value::String(s)) if s == "none" => None,
        Some(v) => Some(v.as_u64().with_context(|| format!("job {}: jank {v}", job.key()))? as usize).filter(|&j| j > 0),
    };
    res.set("jank", jank);
    // The verdict needs the witness valid at the boot chain's J AND at
    // Celia's; where they are different start classes, Celia's class is
    // also searched (a search-only job) and its optimum recorded.
    if job.truthy("jank_classes") {
        res.set("jank_classes", job.0["jank_classes"].clone());
        res.set("celia_jank", job.0.get("celia_jank").cloned().unwrap_or(Value::Null));
    }
    let offset = job.0.get("offset").and_then(Value::as_i64).with_context(|| format!("job {}: no offset", job.key()))?;
    let to = job.0.get("to").map(|v| v.as_u64().context("`to`: frames")).transpose()?;
    // 1. the reference
    let mut reference: Option<(u32, i64)> = None;
    for off in offset - 2..offset + 3 {
        let ins: Vec<u8> = std::iter::repeat(0).take(off.max(0) as usize).chain(dbin.iter().copied()).collect();
        if let (Some(e), _) = replay_exit(r, room, &ins, &seeds, cat, jank)? {
            reference = Some((e, off));
            break;
        }
    }
    let (ref_frame, prologue) = match reference {
        Some((e, off)) => (Some(e), off),
        None => {
            // The community file finishes in UniversalClassicTas but not on
            // a real PICO-8 (the tool is a reimplementation): its count is
            // the reference.
            let (ok, n) = match level {
                Some(l) => {
                    let o = love::play(Tool::Uct, l, &dbpath, cat.gemskip(), None)?;
                    (o.finished, o.count)
                }
                None => (false, 0),
            };
            if !ok && to.is_none() {
                return Ok(res.with("status", "the community TAS does not exit in the original cart nor in UniversalClassicTas"));
            }
            res.set("ref_source", if ok { "UniversalClassicTas only (does not finish on a real PICO-8)" } else { "none: the community TAS finishes nowhere" });
            (ok.then(|| (offset + i64::from(n)) as u32), offset)
        }
    };
    res.set("ref", ref_frame);
    res.set("prologue", prologue);
    let prefer = jd.join("prefer.txt");
    let pre: Vec<u8> = std::iter::repeat(0).take(prologue.max(0) as usize).chain(dbin.iter().copied()).collect();
    std::fs::write(&prefer, join(&pre))?;
    let horizon = to.map(|t| t as u32).or(ref_frame);
    let tree = jd.join("tree");
    let reuse = job.opt_str("reuse").map(PathBuf::from);
    // The job's tree deleted; its level 0 back to `reuse` (or there for the
    // first time).
    let release_tree = || {
        if let Some(back) = &reuse {
            if tree.join("level00").is_dir() && !back.exists() {
                let _ = std::fs::rename(tree.join("level00"), back);
            }
        }
        let _ = std::fs::remove_dir_all(&tree);
    };
    let keep = job.truthy("keep_tree");
    let drop_tree = || {
        if !keep {
            release_tree();
        }
    };
    // 2. the search.
    let search = |l1: bool| -> Result<Option<(i32, Option<Value>, f64)>> {
        let horizon = horizon.context("no horizon")?;
        let mut flags: Vec<String> = vec!["--category".into(), cat.to_string()];
        flags.extend(job.0.get("flags").and_then(Value::as_array).into_iter().flatten().filter_map(Value::as_str).map(str::to_string));
        flags.extend(["--loading-jank".into(), jank.map_or("none".into(), |j| j.to_string())]);
        // The concrete steps under the community file's balloon seeds ([]:
        // 0): the witness is then one real run under them.
        flags.push(format!("--concrete-balloon-seeds={}", seeds.join(",")));
        if l1 {
            flags.extend(["--level-minus-one".into(), format!("{horizon},5")]);
        }
        release_tree();
        // `reuse`: a level-0 tree of this room and level, moved in so the
        // search reuses it.
        if let Some(back) = reuse.as_ref().filter(|b| b.is_dir()) {
            std::fs::create_dir_all(&tree)?;
            std::fs::rename(back, tree.join("level00"))?;
        }
        let mem = job.opt_str("mem").unwrap_or("60G");
        let mut cmd = std::process::Command::new(safe_run());
        cmd.args(["--memory", mem, "--"]).arg(cx.binary).args(["search", "--room", &room_s, "--frames", &horizon.to_string()]);
        if to.is_none() {
            cmd.arg("--ceiling");
        }
        cmd.args(["--level", job.str("levels")?]).arg("--prefer").arg(&prefer).arg("--save-marks").arg(jd.join("marks")).arg("--search-dir").arg(&tree).args(&flags);
        let log = std::fs::File::create(jd.join("search.log"))?;
        cmd.current_dir(env!("CARGO_MANIFEST_DIR")).stdout(log.try_clone()?).stderr(log);
        let t = std::time::Instant::now();
        let limit = job.0.get("timeout").and_then(Value::as_u64).unwrap_or(4 * 3600);
        let Some(status) = super::wait_logged(&mut cmd, std::time::Duration::from_secs(limit))? else { return Ok(None) };
        // The search's record (`search::result`): none when it stopped on an
        // error before deciding.
        let rec = std::fs::read_to_string(tree.join("result.json")).ok().map(|t| serde_json::from_str::<Value>(&t)).transpose()?;
        Ok(Some((status.code().unwrap_or(-1), rec, t.elapsed().as_secs_f64())))
    };
    let (opt, mut ours): (u32, Vec<u8>) = match job.opt_str("witness").filter(|w| !w.is_empty()) {
        Some(w) => {
            let ours = crate::concrete::read_inputs(w)?;
            res.set("witness_from", w);
            (ours.len() as u32, ours)
        }
        None => {
            let l1 = job.0.get("l1").and_then(Value::as_bool).unwrap_or(true);
            let mut done = search(l1)?;
            if let Some((rc, Some(rec), _)) = &done {
                if *rc != 0 && l1 && rec["verdict"] == "level_minus_one_refused" {
                    res.set("l1", "refused");
                    done = search(false)?;
                }
            }
            let Some((rc, rec, secs)) = done else {
                drop_tree();
                return Ok(res.with("status", "search timed out"));
            };
            res.set("search_s", secs.round_ties_even() as i64);
            res.set("peak_gb", rec.as_ref().and_then(|r| r["peak_gb"].as_f64()).map(|g| format!("{g:.2}").parse::<f64>().expect("a number")));
            let bounds: Vec<Value> = rec.as_ref().and_then(|r| r["levels"].as_array()).into_iter().flatten().map(|l| l["arc_bound"].clone()).filter(|b| !b.is_null()).collect();
            res.set("arc_bounds", bounds);
            let verdict = rec.as_ref().and_then(|r| r["verdict"].as_str()).unwrap_or("");
            // Without `--ceiling` a refutation is a result: no win by H at all.
            if rc == 0 && rec.is_some() && (verdict == "refuted" || verdict == "no_concrete_win") && to.is_some() {
                drop_tree();
                let mut out = res.with("status", format!("refuted at {}", horizon.map_or("None".into(), |h| h.to_string())));
                out.set("detail", verdict);
                return Ok(out);
            }
            if rc != 0 || rec.is_none() || verdict != "optimal" {
                drop_tree();
                let log = String::from_utf8_lossy(&std::fs::read(jd.join("search.log"))?).to_string();
                let lines: Vec<&str> = log.lines().filter(|l| !l.trim().is_empty()).collect();
                let tail: Vec<String> = lines[lines.len().saturating_sub(3)..].iter().map(|s| s.to_string()).collect();
                let mut out = res.with("status", format!("search failed (exit {rc}{})", rec.as_ref().map_or(String::new(), |_| format!(", {verdict}"))));
                out.set("tail", tail);
                return Ok(out);
            }
            let rec = rec.expect("a record");
            let w: Vec<u8> = rec["witness"].as_array().context("result.json: no witness")?.iter().map(|b| b.as_u64().map(|b| b as u8).context("an input byte")).collect::<Result<_>>()?;
            (rec["optimal"].as_u64().context("result.json: no optimum")? as u32, w)
        }
    };
    res.set("ours", opt);
    if job.truthy("search_only") {
        drop_tree();
        return Ok(res.with("status", "search only: another start class of the room (the widened optimum is the least)"));
    }
    std::fs::write(jd.join("witness.txt"), join(&ours))?;
    // 3. verification in the original cart. The search steps a chest's last
    // shake as every draw (`rnd` is an interval; only the balloons are
    // seeded), so its witness may take the berry where the file's chest seed
    // does not put it: then the other chest seeds are tried (`concrete::
    // witness`'s, in a 100% category), and the upload carries the one that
    // works.
    let hundred = cat.hundred();
    let witness = |sd: &[String]| crate::concrete::witness(&ours, &settings(room, cat, jank, sd)?);
    let mut candidates = vec![seeds.clone()];
    candidates.extend(witness(&seeds)?.chest_seed_variants);
    let (mut mine, mut e, mut fs, mut berry, mut sd) = (ours.clone(), None, Vec::new(), false, seeds.clone());
    for cand in candidates {
        sd = cand;
        mine = ours.clone();
        (e, fs) = replay_exit(r, room, &ours, &sd, cat, jank)?;
        if e != Some(opt) {
            // A jump press that does nothing in the minimal cart can fire
            // later in the original (its jump buffer, C7): keep only the
            // presses that fire.
            let canon = witness(&sd)?.canonical;
            let (e2, fs2) = replay_exit(r, room, &canon, &sd, cat, jank)?;
            res.set("jump_canonicalized", format!("original cart exit {} -> {}", py_opt(e), py_opt(e2)));
            if e2 == Some(opt) {
                (mine, e, fs) = (canon, e2, fs2);
            }
        }
        // The berry taken before the exit: a `lifeup` appears where it was.
        berry = e.is_some_and(|e| e != 0 && fs.iter().any(|fr| fr.lifeup && fr.f < e));
        if e == Some(opt) && (berry || !hundred) {
            break;
        }
    }
    if e == Some(opt) && sd != seeds {
        res.set("chest_seeds", format!("[{}] -> [{}]", seeds.join(","), sd.join(",")));
        seeds = sd;
    }
    if e == Some(opt) {
        ours = mine;
        std::fs::write(jd.join("witness.txt"), join(&ours))?;
    }
    res.set("ours_original_cart_exit", e);
    if hundred {
        res.set("berry", berry);
    }
    // The dashes as the cart started them (its own dash branch).
    let started: Vec<&pico8::Frame> = fs.iter().filter(|fr| fr.dash.is_some() && e.is_none_or(|e| fr.f <= e)).collect();
    let diagonal = |fr: &pico8::Frame| pico8::DIAGONAL.contains(&fr.dash.as_deref().unwrap_or(""));
    if cat.nodiag() {
        res.set("diagonal_dashes", started.iter().filter(|fr| diagonal(fr)).map(|fr| fr.f).collect::<Vec<_>>());
    }
    if cat.onlydiag() {
        res.set("straight_dashes", started.iter().filter(|fr| !diagonal(fr)).map(|fr| fr.f).collect::<Vec<_>>());
    }
    let lead = ours.iter().position(|&b| b != 0).unwrap_or(ours.len());
    res.set("first_input", lead);
    // 4. The upload, chosen by REAL PLAY. It starts at the room's first
    // controllable frame: usually the community file's earliest exiting
    // offset, but a witness may press on the frame before (the player exists
    // and updates on the frame it is created), so that cut is tried too; a
    // cut never drops a press, and the witness ends at the exit (no trailing
    // input). UniversalClassicTas has no loading frame and computes in
    // doubles: information only. When the file's seeds die, the balloons'
    // phases are nudged (still optimal: the bound holds for every seed).
    if let Some(level) = level {
        let db = love::play(Tool::Uct, level, &dbpath, cat.gemskip(), None)?;
        res.set("uct_db", format!("{} {} inputs", if db.finished { "finished" } else { "NOT finished" }, db.count));
        let mut cuts = vec![prologue];
        if (lead as i64) < prologue {
            cuts.push(lead as i64);
        }
        cuts.push(prologue - 1);
        cuts.retain(|&c| !py_slice(&ours, c).iter().any(|&b| b != 0));
        let slots: Vec<&str> = crate::trace::cart::room_objects(room).into_iter().filter(|o| o.seeded).map(|o| o.kind).collect();
        let mut tries: Vec<(Vec<String>, i64)> = cuts.iter().map(|&c| (seeds.clone(), c)).collect();
        for n in love::nudged_seeds(&seeds, &slots, 10)? {
            tries.extend(cuts.iter().map(|&c| (n.clone(), c)));
        }
        // Each candidate once: a room without balloons nudges nothing.
        let mut seen: Vec<(Vec<String>, i64)> = Vec::new();
        tries.retain(|t| {
            let new = !seen.contains(t);
            seen.push(t.clone());
            new
        });
        // Every candidate's row; without a VALID one the record shows the
        // first (the file's seeds at the reference's cut), not the last tried.
        let mut tried: Vec<(i64, Vec<String>, String)> = Vec::new();
        let file = entry.file.clone().context("an entry with a file")?;
        for (sd, cut) in tries {
            if sd != seeds && replay_exit(r, room, &ours, &sd, cat, jank)?.0 != Some(opt) {
                continue;
            }
            let mine = jd.join(format!("ours-{file}"));
            std::fs::write(&mine, tasdb::text(&sd, py_tail(&ours, cut)))?;
            // A replay or tool run that fails fails the job.
            let row = super::uploads::check(r, &mine, false, Some(dbcat))?;
            tried.push((cut, sd.clone(), row.clone()));
            if row.ends_with("\tVALID") {
                res.set("real_play", row);
                res.set("upload_cut", cut);
                if sd != seeds {
                    res.set("upload_seeds", sd.join(","));
                }
                let up = cx.outdir.join("upload").join(cat.to_string());
                std::fs::create_dir_all(&up)?;
                std::fs::copy(&mine, up.join(&file))?;
                res.set("upload", up.join(&file).to_string_lossy().to_string());
                let o = love::play(Tool::Uct, level, &mine, cat.gemskip(), None)?;
                res.set("uct_ours", format!("{} {} inputs (information only)", if o.finished { "finished" } else { o.status.as_str() }, o.count));
                break;
            }
        }
        if !tried.is_empty() && res.get("upload").is_none() {
            res.set("real_play", tried[0].2.clone());
            res.set("real_play_cut", tried[0].0);
            let mut cuts: Vec<i64> = tried.iter().map(|t| t.0).collect();
            cuts.sort();
            cuts.dedup();
            let mut lists: Vec<&Vec<String>> = tried.iter().map(|t| &t.1).collect();
            lists.sort();
            lists.dedup();
            let cuts = format!("[{}]", cuts.iter().map(|c| c.to_string()).collect::<Vec<_>>().join(", "));
            res.set("real_play_tried", format!("{} candidates (cuts {cuts}, {} seed lists), none VALID", tried.len(), lists.len()));
        }
    }
    let status = match ref_frame {
        None => "no reference",
        Some(rf) if opt < rf => "IMPROVED",
        Some(rf) if opt == rf => "tie",
        Some(_) => "WORSE?",
    };
    res.set("status", status);
    let verified = e == Some(opt) && !res.get("diagonal_dashes").is_some_and(|d| d.as_array().is_some_and(|a| !a.is_empty())) && res.get("berry").is_none_or(|b| b == &Value::Bool(true)) && res.get("upload").is_some();
    res.set("verified", verified);
    // 5. the UI
    if ref_frame.is_some_and(|rf| opt < rf) && !job.truthy("witness") {
        if let Err(ex) = ui(cx, &jd, &entry, &res, cat, &room_s, room, &name, prologue, opt, ref_frame.expect("a reference"), &seeds, jank) {
            res.set("ui_error", format!("{ex:#}").chars().take(200).collect::<String>());
        } else {
            res.set("ui", format!("room{}{cat}", room_s.replace(',', "")));
        }
    }
    drop_tree();
    Ok(res)
}

/// Python's `xs[:c]` (`c` negative: from the end).
fn py_slice(xs: &[u8], c: i64) -> &[u8] {
    let n = xs.len() as i64;
    &xs[..(if c < 0 { (n + c).max(0) } else { c.min(n) }) as usize]
}

/// Python's `xs[c:]`.
fn py_tail(xs: &[u8], c: i64) -> &[u8] {
    let n = xs.len() as i64;
    &xs[(if c < 0 { (n + c).max(0) } else { c.min(n) }) as usize..]
}

fn join(xs: &[u8]) -> String {
    xs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",")
}

fn py_opt(x: Option<u32>) -> String {
    x.map_or("None".into(), |v| v.to_string())
}

/// Step 5: the improvement's web UI run (`export-ui` with both runs'
/// paths, `runs.json`).
#[allow(clippy::too_many_arguments)]
fn ui(cx: &Ctx, jd: &Path, entry: &tasdb::Entry, res: &Row, cat: Category, room_s: &str, room: (i16, i16), name: &str, prologue: i64, opt: u32, reference: u32, seeds: &[String], jank: Option<usize>) -> Result<()> {
    let paths = jd.join("paths");
    // The paths start the room as the search and the replays do (its loading frame).
    std::fs::write(jd.join("align.txt"), align(&cx.runner, room, cat, name, prologue, &crate::concrete::read_inputs(&jd.join("witness.txt").to_string_lossy())?, Some(&paths), seeds, jank)?)?;
    let rid = format!("room{}{cat}", room_s.replace(',', ""));
    let mut cmd = std::process::Command::new(safe_run());
    cmd.arg("--").arg(cx.binary).args(["export-ui", "--search-dir"]).arg(jd.join("tree")).arg("--out").arg(Path::new(UI).join(&rid)).arg("--arc").arg(jd.join("marks")).arg("--witness").arg(paths.join("ours.txt")).arg("--reference").arg(paths.join("reference.txt"));
    // The UI's download IS the upload file.
    if let Some(up) = res.get("upload").and_then(Value::as_str) {
        cmd.args(["--download", up]);
    }
    let ex = super::run_with_timeout(cmd.current_dir(env!("CARGO_MANIFEST_DIR")), std::time::Duration::from_secs(3600))?.context("export-ui timed out")?;
    if !ex.status.success() {
        let err = String::from_utf8_lossy(&ex.stderr);
        bail!("export-ui: {}", err.trim().lines().last().unwrap_or("export-ui failed"));
    }
    let runs_path = Path::new(UI).join("runs.json");
    let mut runs: Value = serde_json::from_str(&std::fs::read_to_string(&runs_path)?)?;
    let list = runs["runs"].as_array_mut().context("runs.json: no runs")?;
    list.retain(|r| r["id"] != Value::String(rid.clone()));
    let tasn = entry.file.as_deref().unwrap_or("").replace(".tas", "");
    let label = tasdb::label(cat);
    let verified = res.get("verified") == Some(&Value::Bool(true));
    list.push(json!({"id": rid, "label": ui_label(room_s, label, opt, &tasn, reference, res), "category": label, "pick": format!("ours {opt} vs TAS {reference}{}", if verified { "" } else { " (not validated)" })}));
    let mut out = Vec::new();
    serde::Serialize::serialize(&runs, &mut serde_json::Serializer::with_formatter(&mut out, serde_json::ser::PrettyFormatter::with_indent(b" ")))?;
    std::fs::write(&runs_path, out)?;
    Ok(())
}

/// The web UI's label of a run that beats the reference: an unverified one
/// says so and why, never only "exit at ... against ...".
pub fn ui_label(room: &str, label: &str, opt: u32, tasn: &str, reference: u32, res: &Row) -> String {
    let head = format!("Room ({room}) · {label}: exit at frame {opt} against {tasn}'s {reference}");
    let tail = " (community TAS drawn as the reference path)";
    if res.get("verified") == Some(&Value::Bool(true)) {
        return head + tail;
    }
    let mut why = Vec::new();
    let exit = res.get("ours_original_cart_exit");
    if exit.and_then(Value::as_u64) != Some(u64::from(opt)) {
        why.push(format!("the original cart exits at {}", exit.and_then(Value::as_u64).map_or("None".into(), |e| e.to_string())));
    }
    if let Some(d) = res.get("diagonal_dashes").and_then(Value::as_array).filter(|d| !d.is_empty()) {
        why.push(format!("diagonal dashes at {}", py_json(&Value::Array(d.clone()))));
    }
    if res.get("berry") == Some(&Value::Bool(false)) {
        why.push("no berry".into());
    }
    match res.get("real_play").and_then(Value::as_str).filter(|r| !r.is_empty()) {
        Some(row) => {
            let cols: Vec<&str> = row.split('\t').collect();
            let keep: Vec<&str> = cols[1..cols.len().saturating_sub(1).max(1)].iter().copied().filter(|c| ["ours", "UCT", "Celia", "boot"].iter().any(|p| c.starts_with(p))).collect();
            why.push(format!("real play {} ({})", cols.last().unwrap_or(&""), keep.join("; ")));
        }
        None => why.push("no real-play check".into()),
    }
    format!("{head} - NOT VALIDATED: {}{tail}", why.join("; "))
}

/// The community TAS and ours aligned frame by frame in the ORIGINAL cart
/// (both started as the search starts the room, under `seeds`): the table,
/// and in `outdir` (if any) both paths for `export-ui --witness ours.txt --reference
/// reference.txt` (label, inputs, the dash starts as the cart reports them,
/// then `f x y` per frame from 0 to the exit).
#[allow(clippy::too_many_arguments)]
pub fn align(r: &Runner, room: (i16, i16), cat: Category, name: &str, prologue: i64, ours: &[u8], outdir: Option<&Path>, seeds: &[String], jank: Option<usize>) -> Result<String> {
    let e = tasdb::entry(&cat.to_string(), name)?.with_context(|| format!("align: no {cat} database entry {name}"))?;
    let tas: Vec<u8> = std::iter::repeat(0).take(prologue.max(0) as usize).chain(tasdb::read(&tasdb::path(&cat.to_string(), &e)?)?.1).collect();
    let run = |seq: &[u8]| -> Result<std::collections::BTreeMap<u32, pico8::Frame>> {
        let seeds: Vec<String> = if seeds.is_empty() { vec!["0".into()] } else { seeds.to_vec() };
        let spec = Spec { cart: Cart::Original(original_cart()), play: Play::Room { room, inputs: seq.to_vec(), seeds: Some(seeds.iter().map(|s| pico8::py_float(s)).collect::<Result<_>>()?) }, frames: seq.len() + 1, one_dash: cat.gemskip(), jank: jank.filter(|&j| j > 0), load_draw: false, dump: false, trace_objects: false, fenced: FENCED };
        Ok(pico8::frames(&r.run(&spec)?).into_iter().map(|fr| (fr.f, fr)).collect())
    };
    let btn = |b: u8| {
        let s: String = "LRUDJX".chars().enumerate().filter(|(i, _)| b >> i & 1 == 1).map(|(_, c)| c).collect();
        if s.is_empty() { "-".to_string() } else { s }
    };
    // The replay runs one frame past the inputs (a path file holds at most
    // that many); a run that has not left by then is not a route under these seeds.
    let ex = |rr: &std::collections::BTreeMap<u32, pico8::Frame>| -> Result<u32> {
        rr.values().filter(|fr| fr.room != room).map(|fr| fr.f).min().with_context(|| format!("does not leave room {},{} by f{} (the balloon seeds {}?)", room.0, room.1, rr.keys().max().copied().unwrap_or(0), chain::py_list(seeds)))
    };
    let (a, b) = (run(&tas)?, run(ours)?);
    let (ea, eb) = (ex(&a)?, ex(&b)?);
    let mut out = format!("{name}: community exits f{ea}, ours f{eb}\n f   community(input  x,y)        ours(input  x,y)\n");
    let show = |p: Option<(f64, f64)>, fr: Option<&pico8::Frame>| match (p, fr) {
        (Some((x, y)), _) => format!("{x:.0},{y:.0}"),
        (None, Some(fr)) => format!("{},{}", fr.room.0, fr.room.1),
        (None, None) => "?".into(),
    };
    for f in prologue.max(0) as u32..=ea.max(eb) {
        let (fa, fb) = (a.get(&f), b.get(&f));
        let (pa, pb) = (fa.and_then(pico8::Frame::at), fb.and_then(pico8::Frame::at));
        let input = |seq: &[u8]| if f >= 1 && (f as usize) <= seq.len() { btn(seq[f as usize - 1]) } else { String::new() };
        let (ia, ib) = (input(&tas), input(ours));
        let mark = if pa == pb && ia == ib { "" } else { " *" };
        out += &format!("{f:3} {ia:>6} {:>9}      {ib:>6} {:>9}{mark}\n", show(pa, fa), show(pb, fb));
    }
    let Some(outdir) = outdir else { return Ok(out) };
    std::fs::create_dir_all(outdir)?;
    let dashes = |rr: &std::collections::BTreeMap<u32, pico8::Frame>, end: u32| rr.iter().filter(|(f, fr)| fr.dash.is_some() && **f <= end).map(|(f, fr)| format!("{f}:{}", fr.dash.as_deref().unwrap_or(""))).collect::<Vec<_>>().join(" ");
    let file = e.file.as_deref().unwrap_or("");
    for (path, label, seq, rr) in [(outdir.join("reference.txt"), format!("{} (community, {name}, original cart): exits f{ea}", &file[..file.len().saturating_sub(4)]), &tas[..], &a), (outdir.join("ours.txt"), format!("ours (search witness, original cart): exits f{eb}"), ours, &b)] {
        let end = ex(rr)?;
        let mut text = format!("label {label}\ninputs {}\ndashes {}\n0 - -\n", join(seq), dashes(rr, end));
        for f in 1..=end {
            match rr.get(&f).filter(|fr| fr.room == room).and_then(pico8::Frame::at) {
                Some((x, y)) => text += &format!("{f} {x:.0} {y:.0}\n"),
                None => text += &format!("{f} - -\n"),
            }
        }
        std::fs::write(&path, text)?;
        out += &format!("wrote {}: {label}, dashes {}\n", path.display(), dashes(rr, end));
    }
    Ok(out)
}

/// `run`, where any failure - a replay that crashed or printed nothing, a
/// tool without a result, a chain that cannot run - FAILS the job: a
/// `runner error` status, never a result built on a missing one.
pub fn run_job(job: &Job, cx: &Ctx) -> Row {
    run(job, cx).unwrap_or_else(|ex| {
        let mut r = Row::default();
        r.set("key", job.key());
        r.set("status", format!("runner error: {ex:#}").chars().take(300).collect::<String>());
        r
    })
}

/// A job is run as real play enters its room: its loading frame from the J
/// of the category's boot chain (`chain::entry_jank`). Where another play
/// of the previous room gives the room another start (`starts`: every
/// feasible J, grouped by the start state), one SEARCH-ONLY job per other
/// start, tagged `-J<j>`: the history-WIDENED optimum is the least of them
/// all; the main job's witness is valid after the database's previous
/// rooms. A job with its own `jank`, or a `witness`, is run as it is.
pub fn expand(job: Job, cache: &mut std::collections::HashMap<String, Vec<Job>>, r: &Runner) -> Result<Vec<Job>> {
    if job.0.contains_key("jank") || job.truthy("witness") {
        return Ok(vec![job]);
    }
    let key = job.key();
    if !cache.contains_key(&key) {
        let (room, cat) = (job.room()?, job.cat()?);
        let with = |extra: &[(&str, Value)]| {
            let mut j = job.clone();
            for (k, v) in extra {
                j.0.insert(k.to_string(), v.clone());
            }
            j
        };
        let jobs = match chain::entry_jank(r, tasdb::Cat::try_from(cat)?, tasdb::level_of(room))? {
            None => vec![with(&[("jank", "none".into())])],
            Some(j) => {
                let st = crate::starts::starts(&settings(room, cat, None, &[])?)?;
                let mut out = vec![with(&[("jank", j.into()), ("jank_classes", json!(st.classes)), ("celia_jank", json!(st.celia))])];
                let tag = job.opt_str("tag").unwrap_or("");
                for c in st.classes.iter().filter(|c| !c.contains(&j)) {
                    out.push(with(&[("jank", c[0].into()), ("tag", format!("{tag}-J{}", c[0]).into()), ("search_only", true.into())]));
                }
                out
            }
        };
        cache.insert(key.clone(), jobs);
    }
    Ok(cache[&key].clone())
}

/// THE QUEUE: the jobs file re-read before every job, each job not yet in
/// OUTDIR/results.jsonl run in turn, its row appended.
pub fn queue(jobs: &Path, outdir: &Path, binary: &Path) -> Result<()> {
    std::fs::create_dir_all(outdir)?;
    let results = outdir.join("results.jsonl");
    let cx = Ctx { outdir, binary, runner: Runner::new(None, None)? };
    let mut cache = Default::default();
    loop {
        let done: std::collections::HashSet<String> = match std::fs::read_to_string(&results) {
            Ok(t) => t.lines().filter(|l| !l.trim().is_empty()).map(|l| Ok(serde_json::from_str::<Value>(l)?["key"].as_str().unwrap_or("").to_string())).collect::<Result<_>>()?,
            Err(_) => Default::default(),
        };
        let list: Vec<serde_json::Map<String, Value>> = serde_json::from_str(&std::fs::read_to_string(jobs).with_context(|| format!("reading {}", jobs.display()))?).with_context(|| format!("parsing {}", jobs.display()))?;
        let mut todo = Vec::new();
        for j in list {
            for x in expand(check_job(Job(j))?, &mut cache, &cx.runner)? {
                if !done.contains(&x.key()) {
                    todo.push(x);
                }
            }
        }
        let Some(job) = todo.into_iter().next() else {
            println!("all jobs done");
            return Ok(());
        };
        // OUTDIR/STOP: exit before the next job (a restart that cannot
        // overlap a running search: two searches in one tree corrupt its
        // edge records).
        if outdir.join("STOP").exists() {
            std::fs::remove_file(outdir.join("STOP"))?;
            println!("stopped by OUTDIR/STOP");
            return Ok(());
        }
        println!("[{}] {} {} {}", hhmm(), job.0.get("cat").and_then(Value::as_str).unwrap_or(""), job.0.get("name").and_then(Value::as_str).unwrap_or(""), job.0.get("room").and_then(Value::as_str).unwrap_or(""));
        let mut res = run_job(&job, &cx);
        res.set("end", hhmm());
        let line = res.dumps();
        std::io::Write::write_all(&mut std::fs::OpenOptions::new().create(true).append(true).open(&results)?, format!("{line}\n").as_bytes())?;
        println!("    {}", line.chars().take(400).collect::<String>());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A job with a key the runner does not know is refused.
    #[test]
    fn an_unknown_job_key_is_refused() {
        let job: serde_json::Map<String, Value> = serde_json::from_str(r#"{"cat": "100", "room": "4,0", "name": "500m", "offset": 25, "levels": "r0sxh", "flgs": []}"#).expect("json");
        let e = check_job(Job(job)).expect_err("a typo").to_string();
        assert_eq!(e, "job 100/500m: unknown keys ['flgs'] (the module's doc lists them)");
    }

    /// The rows are written as the runner has always written them.
    #[test]
    fn rows_are_written_as_python_wrote_them() {
        let mut r = Row::default();
        r.set("key", "100/500m");
        r.set("jank", Option::<usize>::None);
        r.set("classes", json!([[2], [3, 4]]));
        r.set("peak_gb", 1.0);
        r.set("real_play", "a\tb · c");
        r.set("key", "x");
        assert_eq!(r.dumps(), r#"{"key": "x", "jank": null, "classes": [[2], [3, 4]], "peak_gb": 1.0, "real_play": "a\tb \u00b7 c"}"#);
    }

    /// An unverified result is labelled so in the web UI.
    #[test]
    fn an_unverified_label_says_why() {
        let mut ok = Row::default();
        ok.set("verified", true);
        assert_eq!(ui_label("0,3", "gemskip100", 141, "TAS25", 142, &ok), "Room (0,3) · gemskip100: exit at frame 141 against TAS25's 142 (community TAS drawn as the reference path)");
        let mut bad = Row::default();
        bad.set("verified", false);
        bad.set("ours_original_cart_exit", 141);
        bad.set("real_play", "gemskip100 TAS25 room 0,3\tours 116 db 117\tIL exit 116f berry true\tUCT DID NOT FINISH (died at keypress frame 81) 116\tCelia DID NOT FINISH (DIED at frame 80) 0\tchain exit 116f berry true\tboot exit 116f berry true\tINVALID");
        assert_eq!(ui_label("0,3", "gemskip100", 141, "TAS25", 142, &bad), "Room (0,3) · gemskip100: exit at frame 141 against TAS25's 142 - NOT VALIDATED: real play INVALID (ours 116 db 117; UCT DID NOT FINISH (died at keypress frame 81) 116; Celia DID NOT FINISH (DIED at frame 80) 0; boot exit 116f berry true) (community TAS drawn as the reference path)");
    }
}
