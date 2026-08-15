//! Is the compiled program the SAME program, up to renaming its locals?
//!
//! The gate for the recipe-stability work (plans/recipe-stability-plan.md).
//! That refactor changes how `LocalId`s are ASSIGNED, not what program comes
//! out, so the check it has to pass is exactly:
//!
//! > the final rewritten program, before and after, is identical up to a
//! > bijective renaming of `LocalId`s - and slot-identical.
//!
//! Both halves matter and they fail differently.
//!
//! * **Isomorphism** catches a refactor that changed the program's meaning.
//! * **Slot identity** catches one that did not, but permuted the runtime
//!   slot assignment anyway. Row keys are computed from slots, so every
//!   checkpoint and every certified `g` depends on it: a change can be
//!   perfectly isomorphic and still invalidate two certified rooms.
//!
//! This is NOT general graph isomorphism. The IR is ordered, so a canonical
//! walk assigns each local a number the first time it is SEEN, and two
//! programs are isomorphic exactly when their canonical forms are equal. A
//! difference is reported as the first differing line, which names the
//! function and instruction rather than just saying "not isomorphic".
//!
//! Beyond the refactor this is the general "did my change alter the compiled
//! program at all" check - the thing that was missing when a fourteen-
//! instruction edit to `foreach` turned out to move 371+ recipe entries.

use std::collections::BTreeMap;
use std::fmt::Write as _;

use anyhow::{anyhow, Result};

use crate::rewrite::program::Program;

/// The canonical form of a program: every local replaced by the order in
/// which the walk first meets it, so two isomorphic programs render equal.
pub struct Canonical {
    pub text: String,
    /// Per function (by name), raw `LocalId` -> canonical index. Needed so
    /// the SLOT comparison can be keyed the same way; keying it by raw id
    /// would reject exactly the renaming this gate exists to permit.
    pub maps: BTreeMap<String, BTreeMap<usize, usize>>,
}

impl Canonical {
    pub fn digest(&self) -> String {
        use std::hash::{Hash, Hasher};
        let mut h = rustc_hash::FxHasher::default();
        self.text.hash(&mut h);
        format!("{:016x}", h.finish())
    }
}

/// Renumber the `%n` in a printed line to canonical indices, allocating a
/// new index the first time an id is seen.
///
/// Textual rather than a second serializer on purpose: `print` is the
/// format the rest of the tooling already reads, and a hand-written
/// structural serializer could disagree with it about what an instruction
/// IS - which would make the gate wrong in the one direction that matters.
fn canonicalize_ids(line: &str, map: &mut BTreeMap<usize, usize>) -> String {
    let mut out = String::with_capacity(line.len());
    let bytes = line.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
            let start = i + 1;
            let mut j = start;
            while j < bytes.len() && bytes[j].is_ascii_digit() {
                j += 1;
            }
            let raw: usize = line[start..j].parse().expect("digits");
            let next = map.len();
            let canon = *map.entry(raw).or_insert(next);
            let _ = write!(out, "%c{}", canon);
            i = j;
        } else {
            out.push(bytes[i] as char);
            i += 1;
        }
    }
    out
}

/// Canonical form of the whole program.
///
/// Functions are emitted in NAME order and blocks in LABEL order, not in
/// the order the maps happen to iterate: `Cfg.named` is an `FxHashMap`, and
/// while its iteration is stable for a fixed insertion sequence, a refactor
/// that merely reorders insertions would otherwise show up as a spurious
/// difference. The local numbering is per function, so an id in one
/// function cannot alias one in another.
pub fn canonical(program: &Program) -> Canonical {
    let printed = crate::rewrite::print::format_program(program);
    let mut text = String::with_capacity(printed.len());
    let mut maps: BTreeMap<String, BTreeMap<usize, usize>> = BTreeMap::new();

    // Split the printed program into per-function chunks, then sort by the
    // function header line.
    let mut chunks: Vec<(String, Vec<String>)> = Vec::new();
    for line in printed.lines() {
        if line.starts_with("fn ") {
            chunks.push((line.to_string(), vec![line.to_string()]));
        } else if let Some(last) = chunks.last_mut() {
            last.1.push(line.to_string());
        }
    }
    chunks.sort_by(|a, b| a.0.cmp(&b.0));

    for (fn_header, lines) in chunks {
        // Group the function's lines into blocks so the blocks can be
        // ordered by label. A block header is the only line that ends in
        // ':' at two-space indentation.
        let mut header: Vec<String> = Vec::new();
        let mut blocks: Vec<(String, Vec<String>)> = Vec::new();
        for line in lines {
            let is_block_header = line.starts_with("  ")
                && !line.starts_with("    ")
                && line.trim_end().ends_with(':');
            if is_block_header {
                blocks.push((line.trim().to_string(), vec![line.clone()]));
            } else if let Some(last) = blocks.last_mut() {
                last.1.push(line);
            } else {
                header.push(line);
            }
        }
        blocks.sort_by(|a, b| a.0.cmp(&b.0));

        // One numbering per function.
        let mut map: BTreeMap<usize, usize> = BTreeMap::new();
        for line in header {
            text.push_str(&canonicalize_ids(&line, &mut map));
            text.push('\n');
        }
        for (_, lines) in blocks {
            for line in lines {
                text.push_str(&canonicalize_ids(&line, &mut map));
                text.push('\n');
            }
        }
        // "fn name(%a, %b)" -> "name"
        let name = fn_header
            .trim_start_matches("fn ")
            .split('(')
            .next()
            .unwrap_or("")
            .trim()
            .to_string();
        maps.insert(name, map);
    }
    Canonical { text, maps }
}

/// The slot assignment, keyed by canonical local index rather than raw id,
/// so it can be compared across a renaming.
///
/// Emitted separately from the program text because the two failures want
/// different responses: an isomorphism failure means the refactor changed
/// the program, a slot failure means it did not but moved the runtime
/// layout, which silently invalidates every checkpoint.
pub fn canonical_slots(program: &Program, canon: &Canonical) -> String {
    let mut out = String::new();
    let mut names: Vec<_> = program.functions.keys().map(|g| g.as_str().to_string()).collect();
    names.sort();
    for name in names {
        let (_, fun) = program
            .functions
            .iter()
            .find(|(g, _)| g.as_str() == name)
            .expect("name came from the map");
        let map = match canon.maps.get(&name) {
            Some(m) => m,
            // A function the printer did not emit cannot be compared; say so
            // rather than silently skipping it.
            None => {
                let _ = writeln!(out, "fn {} <NOT PRINTED>", name);
                continue;
            }
        };
        // Keyed by CANONICAL index, and sorted by it, so the comparison
        // survives a renaming. An id the walk never saw is reported rather
        // than dropped - it would be a printer/IR disagreement.
        let mut rows: Vec<(String, usize)> = Vec::new();
        for id in crate::rewrite::slots::defined_ids(fun) {
            let key = match map.get(&usize::from(id)) {
                Some(c) => format!("c{:06}", c),
                None => format!("UNSEEN-raw{}", usize::from(id)),
            };
            rows.push((key, fun.cfg.slots.slot_of(id)));
        }
        rows.sort();
        let _ = writeln!(out, "fn {}", name);
        for (key, slot) in rows {
            let _ = writeln!(out, "  {} -> {}", key, slot);
        }
    }
    out
}

/// Compare two canonical forms and report the first difference with its
/// line number and context, rather than a bare boolean.
pub fn first_difference(a: &str, b: &str) -> Option<String> {
    let (mut la, mut lb) = (a.lines(), b.lines());
    let mut n = 0usize;
    loop {
        n += 1;
        match (la.next(), lb.next()) {
            (None, None) => return None,
            (x, y) if x == y => continue,
            (x, y) => {
                return Some(format!(
                    "line {}:\n  baseline: {}\n  current:  {}",
                    n,
                    x.unwrap_or("<end of file>"),
                    y.unwrap_or("<end of file>")
                ))
            }
        }
    }
}

/// Write the baseline files.
pub fn save_baseline(program: &Program, dir: &std::path::Path) -> Result<()> {
    std::fs::create_dir_all(dir)?;
    let canon = canonical(program);
    std::fs::write(dir.join("canonical.txt"), &canon.text)?;
    std::fs::write(dir.join("slots.txt"), canonical_slots(program, &canon))?;
    Ok(())
}

/// Check a program against a saved baseline. Returns the number of lines
/// compared, or an error naming the first difference.
pub fn check_against_baseline(program: &Program, dir: &std::path::Path) -> Result<usize> {
    let want_prog = std::fs::read_to_string(dir.join("canonical.txt"))
        .map_err(|e| anyhow!("{}/canonical.txt: {} - run `isocheck --save` first", dir.display(), e))?;
    let want_slots = std::fs::read_to_string(dir.join("slots.txt"))
        .map_err(|e| anyhow!("{}/slots.txt: {}", dir.display(), e))?;

    let got_prog = canonical(program);
    if let Some(diff) = first_difference(&want_prog, &got_prog.text) {
        return Err(anyhow!(
            "THE PROGRAM CHANGED, not just its numbering.\n{}\n\nThis is the \
             gate for plans/recipe-stability-plan.md: a change to how locals \
             are assigned must leave the program isomorphic.",
            diff
        ));
    }
    let got_slots = canonical_slots(program, &got_prog);
    if let Some(diff) = first_difference(&want_slots, &got_slots) {
        return Err(anyhow!(
            "The program is isomorphic but THE SLOT ASSIGNMENT MOVED.\n{}\n\n\
             Row keys are computed from slots, so this invalidates every \
             checkpoint and every certified g even though the program means \
             the same thing. Canonicalise the allocation rather than \
             accepting the churn.",
            diff
        ));
    }
    Ok(want_prog.lines().count())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The property the whole gate rests on: two renderings that differ ONLY
    /// in which numbers the locals got must canonicalize to the same text.
    /// Without this the gate would reject the very refactor it exists to
    /// permit - a renaming - and we would learn that only after changing the
    /// type everything indexes by.
    #[test]
    fn a_pure_renaming_canonicalizes_identically() {
        let a = ["%0 = alloc", "%1 = store %0 <- %0", "%2 = br %1 ? x : y"];
        let b = ["%40 = alloc", "%7 = store %40 <- %40", "%99 = br %7 ? x : y"];
        let mut ma = BTreeMap::new();
        let mut mb = BTreeMap::new();
        let ca: Vec<_> = a.iter().map(|l| canonicalize_ids(l, &mut ma)).collect();
        let cb: Vec<_> = b.iter().map(|l| canonicalize_ids(l, &mut mb)).collect();
        assert_eq!(ca, cb, "a renaming must be invisible");
        assert_eq!(ca[1], "%c1 = store %c0 <- %c0", "same id renders the same twice");
    }

    /// And the other direction, or the gate proves nothing: a program that
    /// genuinely differs must NOT canonicalize equal. Here the second line
    /// reads a different local, which no renaming can account for.
    #[test]
    fn a_real_difference_survives_canonicalization() {
        let a = ["%0 = alloc", "%1 = alloc", "%2 = store %0 <- %1"];
        let b = ["%0 = alloc", "%1 = alloc", "%2 = store %1 <- %0"];
        let mut ma = BTreeMap::new();
        let mut mb = BTreeMap::new();
        let ca: Vec<_> = a.iter().map(|l| canonicalize_ids(l, &mut ma)).collect();
        let cb: Vec<_> = b.iter().map(|l| canonicalize_ids(l, &mut mb)).collect();
        assert_ne!(ca, cb, "swapping two operands is a real difference");
    }

    /// Numbering is per line-sequence in first-SEEN order, so an id used
    /// before it is defined (a phi referring forward) still maps stably.
    #[test]
    fn forward_references_number_stably() {
        let mut m = BTreeMap::new();
        let out = canonicalize_ids("%5 = phi [b: %9, c: %5]", &mut m);
        assert_eq!(out, "%c0 = phi [b: %c1, c: %c0]");
    }

    #[test]
    fn first_difference_reports_a_line() {
        assert!(first_difference("a\nb", "a\nb").is_none());
        let d = first_difference("a\nb", "a\nc").expect("differs");
        assert!(d.contains("line 2"), "{}", d);
    }
}
