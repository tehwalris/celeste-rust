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

/// Corpora the tracer must reproduce EXACTLY.
pub const MATCHING: &[&str] = &["tables"];

/// Corpora the tracer must REFUSE. Each is a `#` whose value depends on
/// the array part's capacity, and so on the table's rehash history rather
/// than on its keys - see `heap::Table::len`. Their golden files record
/// what PICO-8 actually says, so what we are declining to guess stays
/// written down and stays checked.
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

/// Every corpus on disk, so adding one cannot be forgotten by the
/// currency check.
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

    /// The other half of "exactly match PICO-8 or raise": these RAISE, and
    /// that has to be checked, or the rule is just a comment. A silent
    /// wrong answer and a refusal are very different things, and only one
    /// of them is allowed.
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

#[cfg(test)]
mod capture_probe {
    /// Does the interpreter's row key ever distinguish two rows by a
    /// CLOSURE CAPTURE?
    ///
    /// It can: `import_block` gives every closure its captured values as
    /// columns, and the boundary hashes them into the row key. The
    /// tracer models none of that - `structure_of` writes
    /// `Cell2::Clo(0, [])` - so if a capture ever varies, the two
    /// engines dedup differently and no amount of agreeing on game state
    /// would make their row keys match.
    ///
    /// The cart's captures are `init_object`'s `obj`, `type`, `x` and
    /// `y`, and the last two are spawn coordinates, so the expectation is
    /// that nothing varies. That is a claim about a program, which is
    /// what a measurement is for.
    #[test]
    #[ignore]
    fn do_closure_captures_ever_vary() {
        use celeste_engine::runtime2::{Cell2, Col};

        if !std::path::Path::new("rewrites.jsonl").exists() {
            return;
        }
        let recipe = crate::rewrite::recipe::Recipe::load("rewrites.jsonl").expect("recipe");
        let (program, _) = crate::rewrite::recipe::build(&recipe).expect("build");
        let engine =
            crate::compiled::FrameEngine::new_for_start_room(&program).expect("engine");
        let mut run = crate::rewrite::verify::AbstractRun::start(&program).expect("start");

        let mut varying = 0usize;
        let mut distinct: std::collections::BTreeSet<String> = Default::default();
        let mut closures = 0usize;
        for frame in 1..=30 {
            run.step().unwrap_or_else(|e| panic!("frame {}: {:#}", frame, e));
            for s in run.states() {
                if s.vector_size == 0 {
                    continue;
                }
                let mut b = crate::compiled::bridge::import_block(
                    s,
                    engine.cart(),
                    engine.cache(),
                );
                b.boundary(engine.ids());
                for cell in &b.structure {
                    let Cell2::Clo(f, caps) = cell else { continue };
                    closures += 1;
                    for (i, c) in caps.iter().enumerate() {
                        let vals: Vec<_> = (0..b.width).map(|l| c.at(l)).collect();
                        if vals.windows(2).any(|w| w[0] != w[1]) {
                            varying += 1;
                        }
                        // Uniform within a block is not the whole story:
                        // two blocks whose captures differ are two rows
                        // to the interpreter and one to the tracer.
                        distinct.insert(format!("{}:{}:{:?}", f, i, vals[0]));
                        let _ = matches!(c, Col::U(_));
                    }
                }
            }
        }
        eprintln!(
            "[captures] {} closure cells over 30 frames; {} capture columns vary WITHIN a block; \
             {} distinct (fn, slot, value) triples",
            closures, varying, distinct.len()
        );
        let mut by_slot: std::collections::BTreeMap<String, usize> = Default::default();
        for d in &distinct {
            let mut it = d.splitn(3, ':');
            let k = format!("{}:{}", it.next().unwrap(), it.next().unwrap());
            *by_slot.entry(k).or_default() += 1;
        }
        for (k, n) in by_slot.iter().filter(|(_, n)| **n > 1) {
            eprintln!("[captures] (fn, slot) {} takes {} distinct values", k, n);
        }
        for d in &distinct {
            eprintln!("[captures] triple {}", d);
        }
    }
}
