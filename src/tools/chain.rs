//! A room's TAS in REAL PLAY on a real PICO-8: the original cart, played
//! through the real room transitions from the rooms before it with the
//! community's tasdatabase files for those rooms (`rewrite pico8 chain`).
//!
//! The start modes (all on the original cart, all counting a room's frames
//! from the frame its player is created, as UniversalClassicTas and Celia
//! do):
//! 1. IL: the room loaded directly (UCT's IL load);
//! 2. jank: the room loaded directly, then its LOADING FRAME as a real
//!    transition runs it - `_update`'s `foreach` goes on over the new
//!    room's objects from the leaving player's index J, one update each
//!    (move, then update), then the frame draws. Celia's IL load does this
//!    with J = the previous room's object count; here J is the one the
//!    chain measured;
//! 3. chain (the ground truth): the rooms before it played with the
//!    database's files (`tasdb::chain_file`), then the room's file.

use anyhow::{bail, Result};
use std::collections::BTreeMap;

use super::pico8::{original_cart, Cart, Play, Runner, Segment, Spec, FENCED};
use super::tasdb;
use super::tasdb::Cat;

/// An `exit` line's words as pairs (`count`, `player_idx`, `of`, `berry`,
/// ...: `exit seg 1 lvl 2 f93 ...` read two words at a time).
pub type Exit = BTreeMap<String, String>;

/// A chain's output: its exits, its failures (`death`, `unexpected`), and
/// its room-entry dump (`dump entry` lines, the prefix dropped).
pub fn parse(lines: &[String]) -> (Vec<Exit>, Vec<String>, Vec<String>) {
    let exits = lines.iter().filter(|l| l.starts_with("exit seg ")).map(|l| pairs(l)).collect();
    let bad = lines.iter().filter(|l| l.starts_with("death ") || l.starts_with("unexpected ")).cloned().collect();
    let dump = lines.iter().filter_map(|l| l.strip_prefix("dump entry ")).map(str::to_string).collect();
    (exits, bad, dump)
}

fn pairs(line: &str) -> Exit {
    let w: Vec<&str> = line.split_whitespace().collect();
    w.chunks_exact(2).map(|p| (p[0].to_string(), p[1].to_string())).collect()
}

/// A dump line as (head, its fields): `obj 2 fly_fruit k=v ...` or
/// `globals k v ...`.
fn fields(line: &str) -> Result<(String, BTreeMap<String, String>)> {
    let w: Vec<&str> = line.split_whitespace().collect();
    if w.first() == Some(&"globals") {
        return Ok(("globals".to_string(), pairs(&w[1..].join(" "))));
    }
    anyhow::ensure!(w.len() >= 3, "a dump line {line:?}");
    let f = w[3..].iter().map(|kv| kv.split_once('=').map(|(k, v)| (k.to_string(), v.to_string())).ok_or_else(|| anyhow::anyhow!("a dump field {kv:?}"))).collect::<Result<_>>()?;
    Ok((w[..3].join(" "), f))
}

/// The fields that differ between two room-entry dumps (`b`: the reference).
pub fn diff_dumps(a: &[String], b: &[String]) -> Result<Vec<String>> {
    let da: BTreeMap<String, BTreeMap<String, String>> = a.iter().map(|l| fields(l)).collect::<Result<_>>()?;
    let db: BTreeMap<String, BTreeMap<String, String>> = b.iter().map(|l| fields(l)).collect::<Result<_>>()?;
    let heads: std::collections::BTreeSet<&String> = da.keys().chain(db.keys()).collect();
    let mut out = Vec::new();
    for head in heads {
        let (Some(fa), Some(fb)) = (da.get(head), db.get(head)) else {
            out.push(format!("{head}: only in {}", if db.contains_key(head) { "reference" } else { "this mode" }));
            continue;
        };
        let keys: std::collections::BTreeSet<&String> = fa.keys().chain(fb.keys()).collect();
        for k in keys {
            if fa.get(k) != fb.get(k) {
                let show = |v: Option<&String>| v.map_or("None".to_string(), |s| s.clone());
                out.push(format!("{head} {k}: {} vs {}", show(fa.get(k)), show(fb.get(k))));
            }
        }
    }
    Ok(out)
}

/// The original cart's spec of a chain or a lone room.
fn spec(cat: Cat, play: Play, frames: usize, jank: Option<usize>, load_draw: bool, dump: bool, trace_objects: bool) -> Spec {
    Spec { cart: Cart::Original(original_cart()), play, frames, one_dash: cat.one_dash(), jank, load_draw, dump, trace_objects, fenced: FENCED }
}

/// Levels `first..level` played with the database's files, then `(seeds,
/// inputs)` in `level`: the output lines.
#[allow(clippy::too_many_arguments)]
pub fn chain(r: &Runner, cat: Cat, level: i16, seeds: &[String], inputs: &[u8], first: i16, trace_objects: bool, dump: bool) -> Result<Vec<String>> {
    let mut segs = Vec::new();
    for l in first..level {
        let (_, path) = tasdb::chain_file(cat, l)?;
        let (sd, ins) = tasdb::read(&path)?;
        segs.push(Segment { room: tasdb::room_of(l), seeds: sd, inputs: ins });
    }
    segs.push(Segment { room: tasdb::room_of(level), seeds: seeds.to_vec(), inputs: inputs.to_vec() });
    let frames = segs.iter().map(|s| s.inputs.len() + 80).sum();
    // From 100m it is the game's boot: the title screen's `_update` calls
    // begin_game AFTER its objects' updates, then the frame draws (the room
    // title's first tick) - a loading frame that updates no object.
    r.run(&spec(cat, Play::Chain(segs), frames, None, first == 1, dump, trace_objects))
}

/// The room alone (mode 1), or with its loading frame from object `jank`
/// (mode 2).
pub fn single(r: &Runner, cat: Cat, level: i16, seeds: &[String], inputs: &[u8], jank: Option<usize>, trace_objects: bool) -> Result<Vec<String>> {
    let play = Play::Chain(vec![Segment { room: tasdb::room_of(level), seeds: seeds.to_vec(), inputs: inputs.to_vec() }]);
    r.run(&spec(cat, play, inputs.len() + 80, jank, false, true, trace_objects))
}

/// The last segment's result (`nsegs` segments): a line, and its exit
/// (`None`: no exit).
pub fn summary(name: &str, lines: &[String], nsegs: usize) -> (String, Option<Exit>) {
    let (exits, bad, _) = parse(lines);
    let last = exits.get(nsegs - 1).cloned();
    let mut s = format!("{name}: ");
    if !bad.is_empty() {
        // A death in an earlier segment: a database file that fails there.
        let early = bad.iter().any(|b| death_seg(b).is_some_and(|k| k < nsegs));
        s += &format!("{}{})", if early { "PREVIOUS ROOM FAILED (" } else { "FAILED (" }, bad.join("; "));
    }
    if let Some(e) = &last {
        s += &format!("exit {}f berry {}", e.get("count").map_or("", String::as_str), e.get("berry").map_or("", String::as_str));
    } else if bad.is_empty() && exits.len() + 1 < nsegs {
        // An earlier segment's database file never exits on PICO-8 (it
        // finishes only in UCT/Celia, which compute in doubles): this room
        // is never reached.
        s += &format!("PREVIOUS ROOM DID NOT EXIT (segment {} of {nsegs})", exits.len() + 1);
    } else if bad.is_empty() {
        s += "NO EXIT";
    }
    (s, last)
}

/// The segment a `death seg K fF` line names.
pub fn death_seg(line: &str) -> Option<usize> {
    line.strip_prefix("death seg ")?.split_whitespace().next()?.parse().ok()
}

/// Python's `repr` of a list of strings, as this tool has always printed
/// its failures.
pub fn py_list(xs: &[String]) -> String {
    let one = |s: &String| if s.contains('\'') && !s.contains('"') { format!("\"{s}\"") } else { format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'")) };
    format!("[{}]", xs.iter().map(one).collect::<Vec<_>>().join(", "))
}

/// The loading-frame index J real play enters `level` with: the leaving
/// player's index in the category's boot chain (its database files from
/// 100m); `None` for 100m (the boot's loading frame updates no object).
///
/// FENCED (plans/correctness.md C9's code path): a database file that dies
/// on PICO-8, or never exits there, BREAKS the chain: it restarts at the
/// level after it (an IL load), and where that is `level` itself, `None`.
/// (`check_uploads::boot`'s loop is the other one; they differ, and stay
/// apart until C9 is settled.)
pub fn entry_jank(r: &Runner, cat: Cat, level: i16) -> Result<Option<usize>> {
    if level == 1 {
        return Ok(None);
    }
    let mut first = 1;
    loop {
        let (exits, bad, _) = parse(&chain(r, cat, level, &[], &[0], first, false, false)?);
        let want = (level - first) as usize;
        if exits.len() >= want {
            let j = exits[want - 1].get("player_idx").map_or("", String::as_str);
            return Ok(Some(j.parse().map_err(|_| anyhow::anyhow!("a chain's exit without its player's index: {:?}", exits[want - 1]))?));
        }
        let dead: Vec<usize> = bad.iter().filter_map(|b| death_seg(b)).collect();
        // A file that dies, or that never exits on PICO-8 (it finishes only
        // in UCT/Celia, which compute in doubles), breaks the chain there.
        let stuck = (bad.is_empty() && exits.len() < want).then_some(exits.len());
        first += match (dead.first(), stuck) {
            (Some(&d), _) => d as i16,
            (None, Some(s)) => s as i16 + 1,
            (None, None) => bail!("the {cat} boot chain to level {level} failed: {}", py_list(&bad)),
        };
        if first >= level {
            // The previous room's file is what breaks: `level` is IL-loaded.
            return Ok(None);
        }
    }
}

/// THE SEARCH'S START against real play: `ours` (`rewrite run
/// --dump-start` under the boot chain's J, `j`) against the boot chain's
/// room entry, field by field - the objects both carts have (the minimal
/// one lacks room_title, message and flag), the fields both have. The
/// differences, and the fields only one side has.
pub fn check_start(reference: &[String], ours: &[String]) -> Result<(Vec<String>, Vec<String>)> {
    const SKIP: &[&str] = &["room_title", "message", "flag"];
    let kind = |h: &str| h.split_whitespace().nth(2).unwrap_or("").to_string();
    let pico: Vec<(String, BTreeMap<String, String>)> = reference.iter().filter(|l| l.starts_with("obj ") && !SKIP.contains(&l.split_whitespace().nth(2).unwrap_or(""))).map(|l| fields(l)).collect::<Result<_>>()?;
    let mine: Vec<(String, BTreeMap<String, String>)> = ours.iter().filter(|l| l.starts_with("obj ")).map(|l| fields(l)).collect::<Result<_>>()?;
    let (mut diffs, mut only) = (Vec::new(), std::collections::BTreeSet::new());
    if pico.iter().map(|(h, _)| kind(h)).ne(mine.iter().map(|(h, _)| kind(h))) {
        let heads = |v: &[(String, BTreeMap<String, String>)]| py_list(&v.iter().map(|(h, _)| h.clone()).collect::<Vec<_>>());
        diffs.push(format!("object lists differ: {} vs {}", heads(&pico), heads(&mine)));
    }
    for ((_, fp), (hm, fm)) in pico.iter().zip(&mine) {
        let keys: std::collections::BTreeSet<&String> = fp.keys().chain(fm.keys()).collect();
        for k in keys {
            match (fp.get(k), fm.get(k)) {
                (Some(p), Some(m)) if p != m => diffs.push(format!("{hm} {k}: search {m} vs PICO-8 {p}")),
                (Some(_), Some(_)) => {}
                (p, _) => {
                    only.insert(format!("{}.{k} ({} only)", kind(hm), if p.is_some() { "PICO-8" } else { "search" }));
                }
            }
        }
    }
    Ok((diffs, only.into_iter().collect()))
}

/// The room-entry state of `level` after each category's boot chain
/// (different plays of the rooms before it): per category J or the
/// failure, and the fields that differ between the categories that got
/// there (field -> the categories that differ from the first).
#[allow(clippy::type_complexity)]
pub fn vary(r: &Runner, level: i16, cats: &[Cat]) -> Result<(Vec<(Cat, String, Option<Vec<String>>)>, BTreeMap<String, std::collections::BTreeSet<String>>)> {
    let mut runs = Vec::new();
    for &c in cats {
        let lines = match chain(r, c, level, &[], &[0], 1, false, true) {
            Err(e) if e.downcast_ref::<tasdb::NoFile>().is_some() => {
                runs.push((c, format!("no files ({e})"), None));
                continue;
            }
            other => other?,
        };
        let (exits, bad, dump) = parse(&lines);
        let k = level as usize - 2;
        runs.push(match exits.get(k) {
            Some(e) => (c, format!("J {} of {}", e.get("player_idx").map_or("", String::as_str), e.get("of").map_or("", String::as_str)), Some(dump).filter(|d| !d.is_empty())),
            None => (c, format!("failed: {}", py_list(&bad)), None),
        });
    }
    let mut differ: BTreeMap<String, std::collections::BTreeSet<String>> = BTreeMap::new();
    if let Some(first) = runs.iter().find_map(|(_, _, d)| d.clone()) {
        for (c, _, d) in &runs {
            let Some(d) = d else { continue };
            for line in diff_dumps(d, &first)? {
                differ.entry(line.split(':').next().unwrap_or("").to_string()).or_default().insert(c.to_string());
            }
        }
    }
    Ok((runs, differ))
}

/// Globals that only draw, sound or count time (`pico8::DUMP_GLOBALS`): no
/// object's update reads them.
pub const COSMETIC: &[&str] = &["frames", "seconds", "minutes", "music_timer", "sfx_timer", "new_bg", "flash_bg", "deaths", "shake"];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chain_output_parses() {
        let lines: Vec<String> = ["exit seg 1 lvl 1 f93 start 2 count 91 player_idx 2 of 4 berry false freeze 0 max_djump 1", "death seg 2 f20", "dump entry globals max_djump 1 deaths 0", "dump entry obj 1 player_spawn x=0x0008.0000 spd={x=0x0000.0000,}", "f1 room 0,0"].map(String::from).to_vec();
        let (exits, bad, dump) = parse(&lines);
        assert_eq!((exits[0]["count"].as_str(), exits[0]["player_idx"].as_str(), exits[0]["berry"].as_str()), ("91", "2", "false"));
        assert_eq!((bad.len(), dump.len()), (1, 2));
        let other = ["globals max_djump 2 deaths 0".to_string(), "obj 1 player_spawn x=0x0008.0000".into()];
        assert_eq!(diff_dumps(&dump, &other).expect("dumps"), ["globals max_djump: 1 vs 2", "obj 1 player_spawn spd: {x=0x0000.0000,} vs None"]);
        assert_eq!(summary("chain", &lines, 2).0, "chain: FAILED (death seg 2 f20)");
        assert_eq!(summary("chain", &lines, 3).0, "chain: PREVIOUS ROOM FAILED (death seg 2 f20)");
        assert_eq!(summary("IL", &lines[..1], 1).0, "IL: exit 91f berry false");
        assert_eq!(summary("x", &[], 3).0, "x: PREVIOUS ROOM DID NOT EXIT (segment 1 of 3)");
        assert_eq!(py_list(&["a".into(), "it's".into()]), "['a', \"it's\"]");
    }
}
