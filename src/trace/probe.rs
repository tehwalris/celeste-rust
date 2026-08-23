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

#[cfg(test)]
mod encoding {
    /// The first path from the globals to each cell. A structural
    /// difference reported as a number is a puzzle; reported as
    /// `objects[0].hitbox` it is an answer.
    fn cell_paths(
        b: &celeste_engine::Rt2,
    ) -> std::collections::HashMap<u32, String> {
        use celeste_engine::runtime2::{Cell2, AV};
        let mut out: std::collections::HashMap<u32, String> = Default::default();
        let mut queue: Vec<(u32, String)> = Vec::new();
        for (g, cell) in b.globals.iter().enumerate() {
            if *cell == celeste_engine::runtime2::NONE {
                continue;
            }
            let name = celeste_names::gen::GLOBAL_NAMES
                .get(g)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("g{}", g));
            queue.push((*cell, name));
        }
        let mut i = 0;
        while i < queue.len() {
            let (c, p) = queue[i].clone();
            i += 1;
            if out.contains_key(&c) {
                continue;
            }
            out.insert(c, p.clone());
            match &b.structure[c as usize] {
                Cell2::Val => {
                    if let celeste_engine::runtime2::Col::U(AV::Ptr(t)) = &b.cols[c as usize] {
                        queue.push((*t, p.clone()));
                    }
                }
                Cell2::Obj(fields) => {
                    for (f, t) in fields {
                        let name = celeste_names::gen::FIELD_NAMES
                            .get(*f as usize)
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| format!("f{}", f));
                        queue.push((*t, format!("{}.{}", p, name)));
                    }
                }
                Cell2::Arr(items) => {
                    for (k, t) in items.iter().enumerate() {
                        queue.push((*t, format!("{}[{}]", p, k)));
                    }
                }
                _ => {}
            }
        }
        out
    }

    /// `structure_of` and `bridge::import_block` are two encoders of one
    /// thing, and they have to agree.
    ///
    /// The engine's cell numbering, its shape hash and its row key are
    /// all functions of the block's structure, so an encoding difference
    /// makes two runs incomparable even when they agree about every
    /// value in the game. That is not hypothetical: the traced frame
    /// loop's first row-key mismatch was 87 of 274 cells disagreeing at
    /// frame 1 with no game state in dispute at all.
    ///
    /// Checked on the state after `_init`, where both sides can be
    /// produced from nothing, so a divergence is reported at its source
    /// rather than twenty frames downstream.
    #[test]
    #[ignore]
    fn the_tracer_encodes_a_block_the_way_the_importer_does() {
        use crate::trace::domain::Symbolic;
        use crate::trace::interp::Interp;
        use crate::trace::verify::run_one;
        use crate::trace::cart;

        if !std::path::Path::new("rewrites.jsonl").exists() {
            return;
        }
        let recipe = crate::rewrite::recipe::Recipe::load("rewrites.jsonl").expect("recipe");
        let (program, _) = crate::rewrite::recipe::build(&recipe).expect("build");
        let engine =
            crate::compiled::FrameEngine::new_for_start_room(&program).expect("engine");
        let run = crate::rewrite::verify::AbstractRun::start(&program).expect("start");
        let states = run.states();
        assert_eq!(states.len(), 1, "_init should leave exactly one state");
        let mut theirs =
            crate::compiled::bridge::import_block(&states[0], engine.cart(), engine.cache());
        // CANONICAL on both sides. `import_block` numbers cells in its
        // own discovery order and the boundary renumbers them; comparing
        // the raw outputs compares two orderings, not two encodings.
        theirs.canonicalize_ids();

        let root = std::path::Path::new(".");
        let src = cart::sources_in(root).expect("sources");
        let top = full_moon::parse(&src).expect("parse cart");
        let init = full_moon::parse("_init()").expect("parse _init");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        it.cart = Some(engine.cart());
        it.cache = Some(engine.cache());
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let st = run_one(&mut it, &init, st).expect("_init");
        let mut mine = crate::trace::bind::structure_of(&st, engine.cart(), engine.cache())
            .expect("structure_of");
        // A no-op if `structure_of` is right about the rule
        // (`the_structure_a_traced_state_becomes_is_already_canonical`),
        // applied anyway so that this test compares encodings only.
        mine.canonicalize_ids();

        let mut bad = Vec::new();
        if mine.structure.len() != theirs.structure.len() {
            bad.push(format!(
                "{} cells vs {}",
                mine.structure.len(),
                theirs.structure.len()
            ));
        }
        let path = cell_paths(&mine);
        for c in 0..mine.structure.len().min(theirs.structure.len()) {
            if mine.structure[c] != theirs.structure[c] {
                bad.push(format!(
                    "cell {} ({}): {:?} vs {:?}",
                    c,
                    path.get(&(c as u32)).map(|s| s.as_str()).unwrap_or("?"),
                    mine.structure[c],
                    theirs.structure[c]
                ));
            }
        }
        if mine.globals != theirs.globals {
            for (g, (x, y)) in mine.globals.iter().zip(theirs.globals.iter()).enumerate() {
                if x != y {
                    bad.push(format!(
                        "global {} ({:?}): {:?} vs {:?}",
                        g,
                        celeste_names::gen::GLOBAL_NAMES.get(g),
                        x,
                        y
                    ));
                }
            }
        }
        if !bad.is_empty() {
            let n = bad.len();
            bad.truncate(20);
            panic!(
                "the tracer and the importer encode the post-_init state differently \
                 ({} differences):\n{}",
                n,
                bad.join("\n")
            );
        }
    }
}
