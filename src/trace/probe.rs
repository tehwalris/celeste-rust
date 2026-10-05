//! Differential corpora against REAL PICO-8, for questions another model
//! could get wrong the same way (`#` on a table with a hole depends on how
//! the table was BUILT, not on what it holds).
//!
//! Each `lua/probe/*.lua` runs unchanged in the tracer and in PICO-8; its
//! real stdout is checked in beside it (`./regen-pico8-golden.sh` needs a
//! PICO-8 install, the tests do not). The corpus prints only through
//! `printh`, one value per call: the tracer has no `tostr` or `..`.

use anyhow::{bail, Result};

use super::cart::{fresh_state, run_chunk};
use super::domain::Symbolic;
use super::interp::Interp;

/// Run one corpus file in the tracer and return what it printed.
pub fn run_probe(name: &str) -> Result<Vec<String>> {
    let src = std::fs::read_to_string(format!("lua/probe/{}.lua", name))?;
    let ast = full_moon::parse(&src).map_err(|e| anyhow::anyhow!("parse: {}", e))?;
    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let st = fresh_state::<Symbolic>(&mut it.d);
    run_chunk(&mut it, &ast, st)?;
    Ok(std::mem::take(&mut it.prints))
}

/// The checked-in PICO-8 output of one corpus.
pub fn golden(name: &str) -> Result<Vec<String>> {
    let text = std::fs::read_to_string(format!("lua/probe/{}.expected", name))?;
    Ok(text.lines().map(|l| l.to_string()).collect())
}

/// Compare, and report the FIRST divergence with the label that preceded
/// it, since the corpus prints a label before each case.
pub fn compare(name: &str) -> Result<usize> {
    let got = run_probe(name)?;
    let want = golden(name)?;
    let mut label = "<start>".to_string();
    for (i, w) in want.iter().enumerate() {
        // A label is any line that is not a value; the corpus only prints
        // numbers, booleans, `[nil]` and its own case names.
        if w.contains('-') || (!w.starts_with('[') && w.parse::<f64>().is_err() && w != "true" && w != "false") {
            label = w.clone();
        }
        match got.get(i) {
            Some(g) if g == w => {}
            Some(g) => bail!(
                "{}: line {} (case {:?}): tracer says {:?}, PICO-8 says {:?}",
                name,
                i + 1,
                label,
                g,
                w
            ),
            None => bail!(
                "{}: tracer stopped after {} lines, PICO-8 printed {} (case {:?})",
                name,
                got.len(),
                want.len(),
                label
            ),
        }
    }
    if got.len() > want.len() {
        bail!("{}: tracer printed {} lines, PICO-8 {}", name, got.len(), want.len());
    }
    Ok(want.len())
}

/// Corpora the tracer must reproduce EXACTLY.
pub const MATCHING: &[&str] = &["tables"];

/// Corpora the tracer must REFUSE: each is a `#` that depends on the table's
/// rehash history, not its keys (`heap::Table::len`). Their golden files
/// still record what PICO-8 says.
pub const REFUSED: &[&str] = &[
    "len_add_to_hole",
    "len_after_sparse_write",
    "len_constructor_hole",
    "len_foreach_hole",
    "len_for_over_hole",
    "len_interior_hole",
    "len_nested_sparse",
    "len_sparse",
];

/// Every corpus on disk, so a new one cannot escape the checks.
pub fn all_corpora() -> Result<Vec<String>> {
    let mut out = Vec::new();
    for e in std::fs::read_dir("lua/probe")? {
        let path = e?.path();
        if path.extension().and_then(|x| x.to_str()) == Some("lua") {
            out.push(path.file_stem().unwrap().to_string_lossy().into_owned());
        }
    }
    out.sort();
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_corpus_is_classified() {
        let on_disk = all_corpora().expect("read lua/probe");
        let mut known: Vec<String> =
            MATCHING.iter().chain(REFUSED.iter()).map(|s| s.to_string()).collect();
        known.sort();
        assert_eq!(
            on_disk, known,
            "a corpus file is neither in MATCHING nor in REFUSED - it is not being checked"
        );
    }

    #[test]
    fn corpora_match_pico8() {
        for name in MATCHING {
            match compare(name) {
                Ok(n) => eprintln!("[probe] {}: {} lines agree with real PICO-8", name, n),
                Err(e) => panic!("{:#}", e),
            }
        }
    }

    /// The other half of "exactly match PICO-8 or raise": these must RAISE.
    #[test]
    fn corpora_that_cannot_be_answered_are_refused() {
        for name in REFUSED {
            let got = run_probe(name);
            let want = golden(name).expect("golden");
            match got {
                Ok(lines) => panic!(
                    "{}: the tracer answered {:?}; PICO-8 says {:?}, and this model \
                     cannot know which - it must refuse",
                    name, lines, want
                ),
                Err(e) => {
                    let msg = format!("{:#}", e);
                    assert!(
                        msg.contains("length") || msg.contains("#"),
                        "{}: refused, but for the wrong reason: {}",
                        name,
                        msg
                    );
                    eprintln!("[probe] {}: refused (PICO-8 would say {:?})", name, want);
                }
            }
        }
    }

    /// Is the checked-in golden file still what PICO-8 says?
    ///
    /// Compares bytes either side of a regeneration (not the VCS status: a
    /// staged or unadded file is not stale). Without PICO-8 it skips,
    /// loudly.
    #[test]
    fn the_golden_files_are_current() {
        let p8 = std::env::var("PICO8").unwrap_or_else(|_| {
            format!("{}/pico-8/pico8", std::env::var("HOME").unwrap_or_default())
        });
        if !std::path::Path::new(&p8).exists() {
            eprintln!("[probe] no PICO-8 at {} - NOT checking the golden files", p8);
            return;
        }
        let names = all_corpora().expect("read lua/probe");
        let before: Vec<String> = names
            .iter()
            .map(|n| std::fs::read_to_string(format!("lua/probe/{}.expected", n)).unwrap())
            .collect();
        let out = std::process::Command::new("./regen-pico8-golden.sh")
            .output()
            .expect("run regen-pico8-golden.sh");
        assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
        for (i, n) in names.iter().enumerate() {
            let after = std::fs::read_to_string(format!("lua/probe/{}.expected", n)).unwrap();
            // The file on disk is now the NEW output, for the diff.
            assert_eq!(
                before[i], after,
                "lua/probe/{}.expected is stale - real PICO-8 now says something else, \
                 and the file has been rewritten with it so the diff is readable",
                n
            );
        }
        eprintln!("[probe] golden files match real PICO-8");
    }
}
