//! Differential corpora against REAL PICO-8: the numerics the concrete
//! search relies on (16.16 `+ - * / %`, `flr`, `abs`, `min`/`max`, `sin`,
//! literals), the sequence builtins (`add`, `del`, `count`, `foreach`) and
//! questions another model could get wrong the same way (`#` on a table with
//! a hole depends on how the table was BUILT, not on what it holds).
//!
//! Each `lua/probe/*.lua` runs unchanged in the tracer (after the game's
//! Lua builtins, `lua/builtin_level_3.lua` and `_4`, as the cart does) and
//! in PICO-8; its real stdout is checked in beside it
//! (`./regen-pico8-golden.sh` needs a PICO-8 install, the tests do not).
//! A corpus prints one value per call: `printh` for strings, booleans and
//! integers (the tracer has no `tostr` or `..`), and `__hex(x)` for any
//! number, its exact 16.16 bits as `tostr(x, true)` prints them
//! (`0x0000.5555`): native here, `printh(tostr(x, true))` in the cart the
//! regen script builds. Printing the raw bits runs the value through no
//! arithmetic; decomposing it with `flr`, `*` and `-` inside the corpus
//! would put the operations under test into the print.
//!
//! A corpus uses only what the tracer has (`cart::NATIVE`, the Lua
//! builtins; no `sqrt`, `cos`, `sgn`, `mid`, `..`), runs concretely (it must
//! end in exactly one state), and keeps one divergence per
//! file, so a known-bad line cannot hide the good ones after it.
//!
//! OVERFLOW RAISES (plans/architecture.md, "Every overflow is an error"):
//! where PICO-8 wraps or saturates, the interpreter refuses. The cases that
//! pin the console there are the `*_overflow` corpora (`OVERFLOW`), split
//! off the numerics corpora: every `__hex` in them must raise, and their
//! golden values pin `Pico8Num`'s explicit `wrapping_*` / `saturating_*`
//! (the console's semantics, which the kernels compute on the lanes they
//! then decline).

use anyhow::{bail, Result};

use super::cart::{fresh_state, run_chunk};
use super::domain::Symbolic;
use super::heap::Value;
use super::interp::Interp;

/// Run one corpus file in the tracer, after the game's Lua builtins, and
/// return what it printed.
pub fn run_probe(name: &str) -> Result<Vec<String>> {
    run_corpus_source(&read(&format!("lua/probe/{name}.lua"))?)
}

fn read(p: &str) -> Result<String> {
    std::fs::read_to_string(p).map_err(|e| anyhow::anyhow!("{p}: {e}"))
}

/// `run_probe` of a corpus's source text.
fn run_corpus_source(corpus: &str) -> Result<Vec<String>> {
    let src = format!("{}\n{}\n{}", read("lua/builtin_level_3.lua")?, read("lua/builtin_level_4.lua")?, corpus);
    let ast = full_moon::parse(&src).map_err(|e| anyhow::anyhow!("parse: {}", e))?;
    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let mut st = fresh_state::<Symbolic>(&mut it.d);
    st.heap.tables.get_mut(&st.globals).expect("the globals table").hash.insert("__hex".to_string(), Value::Builtin("__hex"));
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
        // numbers (decimal or `__hex`'s), booleans, `[nil]` and its own case
        // names.
        if !w.starts_with('[') && !w.starts_with("0x") && w.parse::<f64>().is_err() && w != "true" && w != "false" {
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
pub const MATCHING: &[&str] = &[
    "fmt_basics",
    "num_abs",
    "num_abs_min",
    "num_arith",
    "num_div",
    "num_flr",
    "num_literals",
    "num_literals_precision",
    "num_minmax",
    "num_mod",
    "num_mod_signs",
    "num_sin",
    "num_sin_fruit40",
    "seq_add_del_count",
    "seq_foreach",
    "tables",
];

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

/// Corpora whose every `__hex` case OVERFLOWS: PICO-8 wraps or saturates,
/// the interpreter must RAISE on each. Their golden files record the
/// console's value, which `Pico8Num`'s explicit operations reproduce.
pub const OVERFLOW: &[&str] = &["num_abs_min_overflow", "num_arith_overflow", "num_div_saturate_overflow", "num_literals_overflow"];

/// A corpus's `__hex` cases (its lines that are one, trimmed), in order:
/// one golden line each.
pub fn hex_cases(src: &str) -> Vec<&str> {
    src.lines().map(str::trim).filter(|l| l.starts_with("__hex(")).collect()
}

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
            MATCHING.iter().chain(REFUSED).chain(OVERFLOW).map(|s| s.to_string()).collect();
        known.sort();
        assert_eq!(
            on_disk, known,
            "a corpus file is in none of MATCHING, REFUSED and OVERFLOW - it is not being checked"
        );
    }

    #[test]
    fn corpora_match_pico8() {
        let mut bad = Vec::new();
        for name in MATCHING {
            match compare(name) {
                Ok(n) => eprintln!("[probe] {}: {} lines agree with real PICO-8", name, n),
                Err(e) => bad.push(format!("{:#}", e)),
            }
        }
        assert!(bad.is_empty(), "{}", bad.join("\n"));
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

    /// Every case of an OVERFLOW corpus RAISES, as an overflow: each `__hex`
    /// line run on its own (the corpus with the other cases dropped, its
    /// setup kept), so the first refusal cannot hide the rest. The setup
    /// alone runs.
    #[test]
    fn overflowing_corpora_raise_on_every_case() {
        for name in OVERFLOW {
            let src = read(&format!("lua/probe/{name}.lua")).expect("corpus");
            let cases = hex_cases(&src);
            let want = golden(name).expect("golden");
            assert_eq!(cases.len(), want.len(), "{name}: one golden line per case");
            let without = |keep: Option<usize>| -> String {
                let mut k = 0;
                let mut out = String::new();
                for l in src.lines() {
                    if l.trim().starts_with("__hex(") {
                        k += 1;
                        if keep != Some(k - 1) {
                            continue;
                        }
                    }
                    out += l;
                    out += "\n";
                }
                out
            };
            assert_eq!(run_corpus_source(&without(None)).expect("the setup runs"), Vec::<String>::new(), "{name}: the setup prints nothing");
            for (k, case) in cases.iter().enumerate() {
                match run_corpus_source(&without(Some(k))) {
                    Ok(lines) => panic!("{name}: `{case}` answered {lines:?} where PICO-8 overflows to {}: it must raise", want[k]),
                    Err(e) => {
                        let msg = format!("{e:#}");
                        assert!(msg.contains("leaves the 16.16 range"), "{name}: `{case}` raised, but not as an overflow: {msg}");
                    }
                }
            }
            eprintln!("[probe] {name}: all {} cases raise (PICO-8 would say {want:?})", cases.len());
        }
    }

    /// THE CONSOLE'S OVERFLOW, pinned: every case of the OVERFLOW corpora,
    /// written with `Pico8Num`'s explicit `wrapping_*` / `saturating_*` (and
    /// the console's literal reading), is the golden value real PICO-8
    /// printed. The interpreter raises on these; this keeps what PICO-8
    /// does there documented and checked.
    #[test]
    fn the_explicit_console_operations_give_the_golden_overflow_values() {
        use crate::pico8_num::Pico8Num as P8;
        let n = |s: &str| s.parse::<P8>().expect("an in-range literal");
        let mn = n("-32767").wrapping_sub(n("1"));
        let neg = |s: &str| n(s).wrapping_neg();
        let cases: [(&str, &[(&str, P8)]); 4] = [
            (
                "num_abs_min_overflow",
                &[
                    ("__hex(abs(mn))", mn.saturating_abs()),
                    ("__hex(abs(0 - 32767 - 1))", n("0").wrapping_sub(n("32767")).wrapping_sub(n("1")).saturating_abs()),
                    ("__hex(abs(mn) - 1)", mn.saturating_abs().wrapping_sub(n("1"))),
                ],
            ),
            (
                "num_arith_overflow",
                &[
                    ("__hex(32767 + 1)", n("32767").wrapping_add(n("1"))),
                    ("__hex(32767 + 32767)", n("32767").wrapping_add(n("32767"))),
                    ("__hex(-32767 - 2)", neg("32767").wrapping_sub(n("2"))),
                    ("__hex(-32767 - 32767)", neg("32767").wrapping_sub(n("32767"))),
                    ("__hex(32767 * 2)", n("32767").wrapping_mul(n("2"))),
                    ("__hex(mn * -1)", mn.wrapping_mul(neg("1"))),
                    ("__hex(mn * 2)", mn.wrapping_mul(n("2"))),
                    ("__hex(256 * 256)", n("256").wrapping_mul(n("256"))),
                    ("__hex(-256 * 256)", neg("256").wrapping_mul(n("256"))),
                ],
            ),
            (
                "num_div_saturate_overflow",
                &[
                    ("__hex(32767 / 0.5)", n("32767").saturating_div(n("0.5"))),
                    ("__hex(-32767 / 0.5)", neg("32767").saturating_div(n("0.5"))),
                    ("__hex(1 / 0.00002)", n("1").saturating_div(n("0.00002"))),
                    ("__hex(mn / 0.5)", mn.saturating_div(n("0.5"))),
                    ("__hex(32767 / 0.00002)", n("32767").saturating_div(n("0.00002"))),
                    ("__hex(1 / 0)", n("1").saturating_div(n("0"))),
                    ("__hex(-1 / 0)", neg("1").saturating_div(n("0"))),
                    ("__hex(0 / 0)", n("0").saturating_div(n("0"))),
                    ("__hex(0.5 / 0)", n("0.5").saturating_div(n("0"))),
                    ("__hex(-0.5 / 0)", neg("0.5").saturating_div(n("0"))),
                    ("__hex(32767 / 0)", n("32767").saturating_div(n("0"))),
                    ("__hex(mn / 0)", mn.saturating_div(n("0"))),
                ],
            ),
            (
                "num_literals_overflow",
                &[
                    ("__hex(32768)", P8::wrapping_from_str("32768").expect("a literal")),
                    // Lua has no negative literals: `-` of the wrapped 32768.
                    ("__hex(-32768)", P8::wrapping_from_str("32768").expect("a literal").wrapping_neg()),
                ],
            ),
        ];
        assert_eq!(cases.iter().map(|c| c.0).collect::<Vec<_>>(), OVERFLOW, "every OVERFLOW corpus, in order");
        for (name, written) in cases {
            let src = read(&format!("lua/probe/{name}.lua")).expect("corpus");
            let want = golden(name).expect("golden");
            assert_eq!(written.iter().map(|w| w.0).collect::<Vec<_>>(), hex_cases(&src), "{name}: the cases as the corpus writes them");
            for ((case, v), w) in written.iter().zip(&want) {
                let raw = v.to_bits();
                assert_eq!(&format!("0x{:04x}.{:04x}", raw >> 16, raw & 0xffff), w, "{name}: `{case}`");
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
