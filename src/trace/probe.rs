//! Differential corpora against REAL PICO-8.
//!
//! The tracer's oracle has always been another model - the interpreter,
//! and before that the OCaml one. That is enough to catch drift between
//! them and nothing at all to catch a question they both get wrong.
//! `#` on a table with a hole in it is exactly such a question: it is
//! usually described as "undefined", which invites picking an answer and
//! moving on, and the answer PICO-8 actually gives depends on how the
//! table was BUILT rather than on what it holds.
//!
//! So: a corpus in `lua/probe/*.lua` that runs unchanged in both places,
//! and its real PICO-8 stdout checked in beside it. `./regen-pico8-golden.sh`
//! regenerates the golden files and needs a PICO-8 install; the tests do
//! not, which is the point of checking them in.
//!
//! The corpus talks to the outside world only through `printh`, one value
//! per call - no `tostr` and no `..`, because the tracer implements
//! neither and the source has to be the same source.

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_semantics_match_pico8() {
        match compare("tables") {
            Ok(n) => eprintln!("[probe] tables: {} lines agree with real PICO-8", n),
            Err(e) => panic!("{:#}", e),
        }
    }

    /// Is the checked-in golden file still what PICO-8 says?
    ///
    /// Compares BYTES either side of a regeneration rather than asking
    /// git: a golden file that is merely staged, or not yet added, is not
    /// stale, and `git status --porcelain` calls both of those a change.
    ///
    /// Only runnable where PICO-8 is installed, so it SKIPS rather than
    /// fails - but it skips loudly, since a silent skip on the machine
    /// that has the oracle would be the whole check quietly not running.
    #[test]
    fn the_golden_files_are_current() {
        let p8 = std::env::var("PICO8").unwrap_or_else(|_| {
            format!("{}/pico-8/pico8", std::env::var("HOME").unwrap_or_default())
        });
        if !std::path::Path::new(&p8).exists() {
            eprintln!("[probe] no PICO-8 at {} - NOT checking the golden files", p8);
            return;
        }
        let names = ["tables"];
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
            // The file on disk is now the NEW output, which is what you
            // want to look at if this fires.
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
