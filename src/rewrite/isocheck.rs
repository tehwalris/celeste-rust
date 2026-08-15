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
//! BLOCK LABELS are canonicalised the same way, by reverse postorder from
//! the entry. They have to be: unlike `LocalId`s, which `Stream::build`
//! re-densifies per function, labels come from a generator that is global to
//! the whole program, so any edit renames most of them. Without this the
//! gate would answer "the program changed" to a change that renamed things
//! and nothing else - the one distinction it exists to draw.
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

/// `kill` names a SET of locals, but prints a list, and `kill_dead` sorts
/// that list by raw `LocalId` so the recipe replays byte-for-byte. A sort by
/// raw id is not stable under a renaming - which is the one thing this whole
/// module abstracts away - so the printed order leaks the numbering back in.
///
/// Re-sort it by canonical index instead. Only `kill` gets this: every other
/// instruction's operand order is meaningful.
fn normalize_kill_order(line: &str) -> String {
    let Some(at) = line.find(" = kill ") else {
        return line.to_string();
    };
    let (head, tail) = line.split_at(at + " = kill ".len());
    let mut operands: Vec<&str> = tail.split(", ").map(str::trim).collect();
    operands.sort_by_key(|token| {
        token.trim_start_matches("%c").parse::<usize>().unwrap_or(usize::MAX)
    });
    format!("{}{}", head, operands.join(", "))
}

/// Canonical form of the whole program.
///
/// Functions are emitted in NAME order and blocks in LABEL order, not in
/// the order the maps happen to iterate: `Cfg.named` is an `FxHashMap`, and
/// while its iteration is stable for a fixed insertion sequence, a refactor
/// that merely reorders insertions would otherwise show up as a spurious
/// difference. The local numbering is per function, so an id in one
/// function cannot alias one in another.
/// Rename every block to `b{n}`, numbered by the order a depth-first walk
/// from the entry FINISHES with it - reverse postorder, the same order a
/// dominance computation would use.
///
/// Needed because block labels carry a number from a generator that is
/// GLOBAL to the whole program (frontend.rs: one `LabelGenerator` for every
/// function, unlike `LocalId`s, which `Stream::build` re-densifies per
/// function). So adding eight blocks to `foreach` renames 525 of 695
/// labels, and without this the gate would report "the program changed" for
/// a change that renamed things and nothing else - which is precisely the
/// distinction it exists to make.
///
/// Structural, not textual: the walk depends only on the entry and on each
/// terminator's successor order, so it is identical for two programs that
/// differ only in what the blocks are called.
///
/// Unreachable blocks have no reverse-postorder position. They are numbered
/// after the reachable ones, ordered by their original label, and COUNTED -
/// see `unreachable_blocks`. A final program should have none (`dce`
/// removes them), and if one appears, the gate's insensitivity to naming
/// stops holding for it, so silence would be the wrong answer.
fn canonical_labels(cfg: &crate::ir::Cfg) -> (crate::ir::Cfg, usize) {
    use crate::ir::Label;

    // Iterative DFS, postorder. `__entry` is unnamed and always first, so it
    // is not in the map and needs no number.
    let mut order: Vec<Label> = Vec::new();
    let mut seen: std::collections::HashSet<Label> = Default::default();
    // (label, whether its successors have been pushed yet)
    let mut stack: Vec<(Option<Label>, bool)> = vec![(None, false)];
    while let Some((label, expanded)) = stack.pop() {
        let block = match &label {
            None => &cfg.entry,
            Some(l) => match cfg.named.get(l) {
                Some(b) => b,
                // A dangling branch target is `validate`'s business, not
                // ours; skip it rather than panicking inside the gate.
                None => continue,
            },
        };
        if expanded {
            if let Some(l) = label {
                order.push(l);
            }
            continue;
        }
        if let Some(l) = &label {
            if !seen.insert(l.clone()) {
                continue;
            }
        }
        stack.push((label, true));
        // Pushed in reverse so the first successor is visited first.
        for successor in block.terminator.1.successor_labels().into_iter().rev() {
            if !seen.contains(successor) {
                stack.push((Some(successor.clone()), false));
            }
        }
    }
    order.reverse();

    let mut unreachable: Vec<&Label> = cfg.named.keys().filter(|l| !seen.contains(*l)).collect();
    unreachable.sort();
    let unreachable_count = unreachable.len();

    let mut map: rustc_hash::FxHashMap<Label, Label> = Default::default();
    for (index, label) in order.iter().chain(unreachable).enumerate() {
        // Zero-padded: `canonical` orders blocks by label string, and
        // unpadded `b10` would sort before `b2`. Harmless but unreadable.
        map.insert(label.clone(), Label::from(format!("b{:05}", index)));
    }
    (cfg.map_labels(&|l: &Label| map.get(l).cloned().unwrap_or_else(|| l.clone())), unreachable_count)
}

/// How many blocks the canonical form could not place by reachability.
/// Zero for any program that has been through `dce`.
pub fn unreachable_blocks(program: &Program) -> usize {
    program.functions.values().map(|f| canonical_labels(&f.cfg).1).sum()
}

/// Every block as `function`, `canonical position`, `label` - the label's
/// STRUCTURAL address next to the name it currently has.
///
/// Two builds that differ only in how labels are named produce the same
/// canonical positions, so joining two of these dumps on (function,
/// position) recovers the old -> new renaming without any of the compiler
/// having to remember it. That is how the recipe's block references were
/// migrated when label numbering became per-function; see
/// plans/recipe-stability-plan.md.
pub fn label_positions(program: &Program) -> Vec<(String, usize, String)> {
    let mut out = Vec::new();
    let mut names: Vec<String> = program.functions.keys().map(|g| g.as_str().to_string()).collect();
    names.sort();
    for name in names {
        let fun = program.get(&name).expect("name came from the map");
        let (canon, _) = canonical_labels(&fun.cfg);
        // `canonical_labels` renames to `b{position}`, so the canonical
        // CFG's labels ARE the positions; pairing them back to the original
        // needs the same walk over the original, which `map_labels`
        // preserves the order of.
        let original = crate::rewrite::print::blocks_in_order(&fun.cfg);
        let renamed = crate::rewrite::print::blocks_in_order(&canon);
        debug_assert_eq!(original.len(), renamed.len());
        // Both are sorted by label, and the rename is a bijection, so the
        // two sorted lists do NOT correspond position-by-position. Pair
        // them through the block bodies instead: a block's terminator id is
        // unique within a function.
        let position_of: BTreeMap<usize, String> = renamed
            .iter()
            .map(|(label, block)| (usize::from(block.terminator_id()), label.clone()))
            .collect();
        for (label, block) in original {
            if label == "__entry" {
                continue;
            }
            let canonical_label = position_of
                .get(&usize::from(block.terminator_id()))
                .expect("the rename preserves terminator ids");
            let position: usize = canonical_label
                .trim_start_matches('b')
                .parse()
                .expect("canonical labels are b{n}");
            out.push((name.clone(), position, label));
        }
    }
    out
}

pub fn canonical(program: &Program) -> Canonical {
    let mut relabelled = program.clone();
    for fun in relabelled.functions.values_mut() {
        fun.cfg = canonical_labels(&fun.cfg).0;
    }
    let program = &relabelled;
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

        // One numbering per function, allocated in two passes: everything
        // except `kill` first, then `kill`.
        //
        // A `kill` list's printed order comes from a raw-id sort, so letting
        // it allocate canonical indices would make the numbering depend on
        // the raw ids - the exact leak `normalize_kill_order` exists to
        // close. Every killed local is defined somewhere in the function, so
        // the second pass finds almost nothing; it is there so that an id
        // reachable ONLY through a back edge still gets a number rather than
        // rendering as unmapped.
        let ordered: Vec<String> =
            header.into_iter().chain(blocks.into_iter().flat_map(|(_, l)| l)).collect();
        let mut map: BTreeMap<usize, usize> = BTreeMap::new();
        let is_kill = |line: &str| line.contains(" = kill ");
        for line in ordered.iter().filter(|l| !is_kill(l)) {
            canonicalize_ids(line, &mut map);
        }
        for line in ordered.iter().filter(|l| is_kill(l)) {
            canonicalize_ids(line, &mut map);
        }
        for line in &ordered {
            text.push_str(&normalize_kill_order(&canonicalize_ids(line, &mut map)));
            text.push('\n');
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

    /// Build a tiny diamond whose blocks are called whatever the caller
    /// says: entry branches to `a`/`b`, both jump to `j`, which returns
    /// with a phi naming both incoming edges.
    fn diamond(a: &str, b: &str, j: &str) -> crate::ir::Cfg {
        use crate::ir::*;
        let id = |n: usize| LocalId::from(n);
        let block = |instructions: Vec<(LocalId, Instruction)>, terminator| Block {
            instructions,
            terminator,
            hint_normalize: false,
        };
        let mut named = new_label_map();
        named.insert(
            Label::from(a.to_string()),
            block(vec![], (id(10), Terminator::UnconditionalBranch { target: Label::from(j.to_string()) })),
        );
        named.insert(
            Label::from(b.to_string()),
            block(vec![], (id(11), Terminator::UnconditionalBranch { target: Label::from(j.to_string()) })),
        );
        named.insert(
            Label::from(j.to_string()),
            block(
                vec![(
                    id(12),
                    Instruction::Phi {
                        branches: vec![
                            (Label::from(a.to_string()), id(1)),
                            (Label::from(b.to_string()), id(1)),
                        ],
                    },
                )],
                (id(13), Terminator::Return { value: Some(id(12)) }),
            ),
        );
        Cfg::new(
            block(
                vec![(id(1), Instruction::Alloc)],
                (
                    id(2),
                    Terminator::ConditionalBranch {
                        condition: id(1),
                        true_target: Label::from(a.to_string()),
                        false_target: Label::from(b.to_string()),
                    },
                ),
            ),
            named,
        )
    }

    /// The property the label half of the gate rests on. Block labels carry
    /// a number from a program-global generator, so an edit anywhere renames
    /// them wholesale; the canonical form must not notice.
    #[test]
    fn renaming_every_block_canonicalizes_identically() {
        let (x, _) = canonical_labels(&diamond("if_body_7", "if_else_8", "if_join_9"));
        let (y, _) = canonical_labels(&diamond("if_body_512", "if_else_513", "if_join_514"));
        assert_eq!(
            crate::rewrite::print::format_cfg(&x),
            crate::rewrite::print::format_cfg(&y),
            "a wholesale label renaming must be invisible"
        );
    }

    /// And the other direction, or it proves nothing: swapping which arm the
    /// condition takes is a real difference, and no renaming can hide it.
    #[test]
    fn swapping_the_branch_arms_survives_label_canonicalization() {
        let straight = diamond("a", "b", "j");
        let mut swapped = diamond("a", "b", "j");
        if let crate::ir::Terminator::ConditionalBranch { true_target, false_target, .. } =
            &mut swapped.entry.terminator.1
        {
            std::mem::swap(true_target, false_target);
        } else {
            panic!("the fixture branches");
        }
        assert_ne!(
            crate::rewrite::print::format_cfg(&canonical_labels(&straight).0),
            crate::rewrite::print::format_cfg(&canonical_labels(&swapped).0),
        );
    }

    /// A block nothing branches to has no reverse-postorder position. It
    /// must be reported rather than silently placed, because the gate's
    /// naming-insensitivity does not extend to it.
    #[test]
    fn unreachable_blocks_are_counted() {
        use crate::ir::*;
        let mut cfg = diamond("a", "b", "j");
        cfg.named.insert(
            Label::from("orphan".to_string()),
            Block {
                instructions: vec![],
                terminator: (LocalId::from(20), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        assert_eq!(canonical_labels(&cfg).1, 1);
        assert_eq!(canonical_labels(&diamond("a", "b", "j")).1, 0);
    }

    #[test]
    fn first_difference_reports_a_line() {
        assert!(first_difference("a\nb", "a\nb").is_none());
        let d = first_difference("a\nb", "a\nc").expect("differs");
        assert!(d.contains("line 2"), "{}", d);
    }
}
