//! THE CELESTECLASSIC TASDATABASE (a clone at `~/src/github.com/
//! CelesteClassic/tasdatabase`): `database.json`, its files, which file a
//! category's full run plays in a level.
//!
//! A file is `[s1,s2]i1,i2,...`: the `[seeds]` (balloons and chests in
//! creation order; UniversalClassicTas `set_seeds`), then one input byte a
//! frame from the player's first frame; the database counts it as its
//! inputs - 1. Our own witnesses (`tas/`) are the same without the header,
//! from the room's load. `concrete::parse_tas` reads both, `text` writes
//! the database's.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

use crate::settings::Category;

/// The clone.
pub fn root() -> PathBuf {
    PathBuf::from(format!("{}/src/github.com/CelesteClassic/tasdatabase", std::env::var("HOME").unwrap_or_default()))
}

/// One `database.json` entry of a category: `100m`, `TAS1.tas`, its count.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Entry {
    pub name: String,
    /// `None`: an entry without a file.
    pub file: Option<String>,
    pub frames: Option<u64>,
}

/// The database's `classic` entries of `cat` (none for a category it lacks).
pub fn entries(cat: &str) -> Result<Vec<Entry>> {
    let path = root().join("database.json");
    let db: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?).with_context(|| format!("parsing {}", path.display()))?;
    let Some(list) = db["classic"].get(cat) else { return Ok(Vec::new()) };
    list.as_array()
        .with_context(|| format!("{}: classic.{cat} is not a list", path.display()))?
        .iter()
        .map(|e| {
            Ok(Entry {
                name: e["name"].as_str().with_context(|| format!("{}: an entry of {cat} without a name", path.display()))?.to_string(),
                file: e.get("file").and_then(|f| f.as_str()).map(str::to_string),
                frames: e.get("frames").and_then(|f| f.as_u64()),
            })
        })
        .collect()
}

/// The entry of `cat` named `name` (`2900m`).
pub fn entry(cat: &str, name: &str) -> Result<Option<Entry>> {
    Ok(entries(cat)?.into_iter().find(|e| e.name == name))
}

/// An entry's file.
pub fn path(cat: &str, e: &Entry) -> Result<PathBuf> {
    Ok(root().join("classic").join(cat).join(e.file.as_deref().with_context(|| format!("the {cat} entry {} has no file", e.name))?))
}

/// `2900m` -> 29 (`None`: a name that is no level).
pub fn level_of_name(name: &str) -> Option<i16> {
    let n = name.strip_suffix("00m")?;
    (!n.is_empty() && n.bytes().all(|b| b.is_ascii_digit())).then(|| n.parse().ok()).flatten()
}

/// `.../TAS29.tas` -> 29 (`ours-TAS29.tas` too: the name ends so).
pub fn level_of_file(path: &Path) -> Result<i16> {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    let level = || {
        let stem = name.strip_suffix(".tas")?;
        let digits = stem.len() - stem.bytes().rev().take_while(u8::is_ascii_digit).count();
        (digits < stem.len() && stem[..digits].ends_with("TAS")).then(|| stem[digits..].parse().ok()).flatten()
    };
    level().with_context(|| format!("{}: not a TAS<level>.tas", path.display()))
}

pub fn file_of_level(level: i16) -> String {
    format!("TAS{level}.tas")
}

/// The room of LEVEL `level` (`game_runner::room_of_level`).
pub fn room_of(level: i16) -> (i16, i16) {
    crate::game_runner::room_of_level(level)
}

/// The level of `room` (the tasdatabase's, 1-based).
pub fn level_of(room: (i16, i16)) -> i16 {
    crate::game_runner::level_index(room.0, room.1) + 1
}

/// A file's text: the one writer of the format (every upload).
pub fn text(seeds: &[String], inputs: &[u8]) -> String {
    format!("[{}]{}", seeds.join(","), inputs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","))
}

/// A tasdatabase file: its `[seeds]` as written and its inputs.
pub fn read(path: &Path) -> Result<(Vec<String>, Vec<u8>)> {
    let text = std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let (seeds, inputs) = crate::concrete::parse_tas(&text).with_context(|| path.display().to_string())?;
    Ok((seeds.unwrap_or_default(), inputs))
}

/// A tasdatabase category (`classic`'s), by name: the files a full run of
/// it plays where it has none of its own (`fallback`), and one dash past
/// the orb where it is a gemskip one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cat(&'static str);

/// The categories and their FALLBACK chains: the 100% ones play any% where
/// a room has no berry; the gemskip ones their parent category's before
/// the orb room (level 22).
const CATS: &[(&str, &[&str])] = &[
    ("any", &["any"]),
    ("100", &["100", "any"]),
    ("nodiag", &["nodiag", "any"]),
    ("gemskipany", &["gemskipany", "any"]),
    ("gemskipnodiag", &["gemskipnodiag", "nodiag", "any"]),
    ("gemskip100", &["gemskip100", "gemskipany", "100", "any"]),
    ("mindashes", &["mindashes", "any"]),
    ("minjumps", &["minjumps", "any"]),
    ("key", &["key", "any"]),
];

impl Cat {
    /// Every category with a full run, in the database's order.
    pub fn all() -> impl Iterator<Item = Cat> {
        CATS.iter().map(|(c, _)| Cat(c))
    }
    pub fn name(self) -> &'static str {
        self.0
    }
    /// The categories whose files a full run plays, in order of preference.
    pub fn fallback(self) -> &'static [&'static str] {
        CATS.iter().find(|(c, _)| *c == self.0).expect("a known category").1
    }
    /// One dash past the orb (what PICO-8, UCT and Celia are told).
    pub fn one_dash(self) -> bool {
        self.0.starts_with("gemskip")
    }
    /// What the category means to the search (`--category`).
    pub fn search(self) -> Result<Category> {
        self.0.parse().map_err(|e: String| anyhow::anyhow!(e))
    }
}

impl std::str::FromStr for Cat {
    type Err = String;
    fn from_str(s: &str) -> std::result::Result<Cat, String> {
        CATS.iter().find(|(c, _)| *c == s).map(|(c, _)| Cat(c)).ok_or_else(|| format!("{s:?} is not a tasdatabase category with a full run ({})", CATS.iter().map(|c| c.0).collect::<Vec<_>>().join(", ")))
    }
}

impl TryFrom<Category> for Cat {
    type Error = anyhow::Error;
    fn try_from(c: Category) -> Result<Cat> {
        c.to_string().parse().map_err(|e: String| anyhow::anyhow!(e))
    }
}

impl std::fmt::Display for Cat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

/// The category's name on the web UI.
pub fn label(cat: Category) -> &'static str {
    use Category::*;
    match cat {
        Any => "any",
        Hundred => "100%",
        Nodiag => "No Diagonal Dashes",
        Onlydiag => "Only Diagonal Dashes",
        GemskipAny => "Gemskip any%",
        GemskipNodiag => "Gemskip No Diagonal Dashes",
        Gemskip100 => "Gemskip 100%",
    }
}

/// Rooms whose database file is not PICO-8 valid (made in a tool computing
/// in doubles, it never exits on a real PICO-8), played in a category's
/// chain with OUR file instead, so the chain runs on unbroken: (category,
/// level) -> a file relative to the repo. Only files verified like any
/// submission (`check uploads`: VALID).
const OVERRIDES: &[(&str, i16, &str)] = &[
    // The database's 164 finishes in Celia only; ours exits on PICO-8.
    ("gemskip100", 28, "tas/tasdatabase/gemskip100/upload/TAS28.tas"),
];

/// No database file for a level in a category's fallbacks.
#[derive(Debug)]
pub struct NoFile(pub String);

impl std::fmt::Display for NoFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for NoFile {}

/// The file a full run of `cat` plays in `level` (an OVERRIDES entry, else
/// the category's database file, else its fallbacks'): its category and path.
pub fn chain_file(cat: Cat, level: i16) -> Result<(&'static str, PathBuf)> {
    if let Some((_, _, f)) = OVERRIDES.iter().find(|(c, l, _)| *c == cat.0 && *l == level) {
        return Ok((cat.0, Path::new(env!("CARGO_MANIFEST_DIR")).join(f)));
    }
    let cats = cat.fallback();
    for &c in cats {
        for e in entries(c)? {
            if e.file.as_deref() == Some(file_of_level(level).as_str()) {
                let p = path(c, &e)?;
                if p.exists() {
                    return Ok((c, p));
                }
            }
        }
    }
    let names: Vec<String> = cats.iter().map(|c| format!("'{c}'")).collect();
    Err(NoFile(format!("tasdb: no database file for level {level} in [{}]", names.join(", "))).into())
}

#[cfg(test)]
mod tests {
    #[test]
    fn levels_and_files() {
        use super::*;
        assert_eq!((level_of_name("2900m"), level_of_name("100m"), level_of_name("summit"), level_of_name("00m")), (Some(29), Some(1), None, None));
        assert_eq!(level_of_file(Path::new("a/TAS29.tas")).expect("a level"), 29);
        assert_eq!(level_of_file(Path::new("a/ours-TAS4.tas")).expect("a level"), 4);
        assert!(level_of_file(Path::new("a/TASx.tas")).is_err());
        assert_eq!((room_of(1), room_of(9), level_of((7, 3))), ((0, 0), (0, 1), 32));
        assert_eq!(text(&["0.5".into(), "0".into()], &[1, 2]), "[0.5,0]1,2");
    }
}
