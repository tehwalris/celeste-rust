//! Specialization-set fusion (plans/shape-tag-plan.md, step 3).
//!
//! v1: the SHARING CENSUS - lower every member of a specialization set
//! through the kernel emitter's walk (`KernelGraph`), value-number the
//! `Line::Let` nodes globally, and report how much of the members' work
//! is shared. This is the measurement that steers the fusion emission
//! (BENCHMARK_DATA.md "Fusion sharing census": {steady, dying-spikes,
//! dying-fall} fuse to 919 distinct nodes, +5.5% over steady alone), and
//! the value-numbering core is the same one the fused-artifact emission
//! will use.
//!
//! Numbering is CANONICAL-SEQUENTIAL: an expression's identity is its
//! operator text with every known variable replaced by its own canonical
//! id; anything unrecognized (witness field accesses `u.cN`/`rin.cN`,
//! button bits `kbK`, constants, fork configs `c0`/`c1`) is a leaf shared
//! across members by construction. The `&mut dp` side-effect argument is
//! stripped - per-member deopt masks are the fusion output's job, not an
//! identity. One genuinely divergent leaf therefore poisons its whole
//! downstream cone, so shared counts are LOWER bounds.

use std::collections::HashMap;

use anyhow::Result;

use super::kernel::{emit_kernel_parts, KernelGraph, Line};
use crate::rewrite::program::Program;

/// One member: a label plus its lowered graph.
pub struct Member {
    pub label: String,
    pub graph: KernelGraph,
}

/// Lower `(label, program)` members against one shape witness.
pub fn lower_members(
    members: &[(String, Program)],
    witness_path: &str,
) -> Result<Vec<Member>> {
    members
        .iter()
        .map(|(label, program)| {
            let (_, graph) = emit_kernel_parts(program, witness_path)?;
            Ok(Member { label: label.clone(), graph })
        })
        .collect()
}

/// Canonicalize one member's nodes; returns the set of canonical ids its
/// `Let` nodes produce (interning into the shared table).
fn number_member(
    graph: &KernelGraph,
    interned: &mut HashMap<String, u32>,
) -> Vec<u32> {
    // var name -> its node's canonical id, per member.
    let mut vn: HashMap<String, u32> = HashMap::new();
    let mut out = Vec::new();
    for line in graph.pre.iter().chain(graph.suf.iter()) {
        let Line::Let { name, expr, .. } = line else { continue };
        let stripped = expr.replace(", &mut dp", "");
        let canon = canonicalize(&stripped, &vn);
        let next = interned.len() as u32;
        let id = *interned.entry(canon).or_insert(next);
        vn.insert(name.clone(), id);
        out.push(id);
    }
    out
}

/// Replace every whole identifier that has a canonical id with `<id>`.
fn canonicalize(expr: &str, vn: &HashMap<String, u32>) -> String {
    let bytes = expr.as_bytes();
    let mut out = String::with_capacity(expr.len());
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if c.is_ascii_alphabetic() || c == '_' {
            let start = i;
            while i < bytes.len()
                && ((bytes[i] as char).is_ascii_alphanumeric() || bytes[i] == b'_')
            {
                i += 1;
            }
            let ident = &expr[start..i];
            match vn.get(ident) {
                Some(id) => {
                    out.push('<');
                    out.push_str(&id.to_string());
                    out.push('>');
                }
                None => out.push_str(ident),
            }
        } else {
            out.push(c);
            i += 1;
        }
    }
    out
}

/// The census: per-member node counts, pairwise sharing, and the union.
pub fn census(members: &[Member]) -> Result<()> {
    let mut interned: HashMap<String, u32> = HashMap::new();
    let sets: Vec<(String, std::collections::BTreeSet<u32>, usize)> = members
        .iter()
        .map(|m| {
            let ids = number_member(&m.graph, &mut interned);
            let n = ids.len();
            (m.label.clone(), ids.into_iter().collect(), n)
        })
        .collect();
    for (label, set, n) in &sets {
        println!("{}: {} nodes, {} distinct", label, n, set.len());
    }
    for i in 0..sets.len() {
        for j in i + 1..sets.len() {
            let shared = sets[i].1.intersection(&sets[j].1).count();
            println!("{} n {}: {} shared", sets[i].0, sets[j].0, shared);
        }
    }
    let mut union = std::collections::BTreeSet::new();
    let mut all: Option<std::collections::BTreeSet<u32>> = None;
    for (_, set, _) in &sets {
        union.extend(set.iter().copied());
        all = Some(match all {
            None => set.clone(),
            Some(a) => a.intersection(set).copied().collect(),
        });
    }
    println!(
        "all members: {} shared; union {} distinct (vs {} summed)",
        all.map(|a| a.len()).unwrap_or(0),
        union.len(),
        sets.iter().map(|(_, s, _)| s.len()).sum::<usize>()
    );
    Ok(())
}
