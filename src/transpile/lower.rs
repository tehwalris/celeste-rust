//! Graph -> the fused, fork-free frame (`plans/multi-output-fusion.md`).
//!
//! The tracer's walk records what each value MEANS as a node in
//! `transpile::graph`; `specialize_frame` resolves every (button, fork)
//! configuration of that graph into ONE hash-consed arena, and
//! `lower_outcomes` reads the per-outcome constants the kernel needs off
//! the result. The ASM backend (`trace::emit::asm_fused`) assembles the
//! same arena. (The Rust-source renderer that used to live here went with
//! the generated kernel crates; nothing rendered text any more, 2026-09-08.)

use std::collections::{BTreeMap, BTreeSet};

use super::graph::{Graph, NodeId, Op};
use super::kernel::{Emit, OutFields};

/// Nodes reachable from `roots`, walking operands.
fn reachable(g: &Graph, roots: &[NodeId]) -> Vec<bool> {
    let mut live = vec![false; g.len()];
    let mut stack: Vec<NodeId> = roots.to_vec();
    while let Some(n) = stack.pop() {
        if live[n as usize] {
            continue;
        }
        live[n as usize] = true;
        stack.extend(g.get(n).args.iter().copied());
    }
    live
}

/// One output shape: the cells it writes, and the two booleans that say
/// which lanes reach it (`live`) and which of those the kernel may keep
/// (`ok`).
pub(crate) struct Outcome {
    pub(crate) of: OutFields,
    pub(crate) ok: NodeId,
    pub(crate) live: NodeId,
}

/// Specialize a traced frame's graph into ONE fused, hash-consed arena.
///
/// Every (button, fork configuration) is resolved: `Free` becomes a
/// constant and `Split`/`SplitValid` become `Frag`/`FragOk` (ordinary
/// unary ops), so NO fork node survives and configurations that agree
/// share nodes ("duplicate, then fuse again"). Steps 1-4 of the frame
/// lowering. Returns the fused graph and one body per DISTINCT (outcome,
/// roots): `(outcome, frees, splits, roots)`, where `roots` is that
/// outcome's fields in order, then `ok`, then `live`, as node ids in the
/// fused graph.
///
/// This is the SINGLE source of the specialized compute: the AVX-512
/// backend assembles it (`trace::emit::asm_fused`) and `lower_outcomes`
/// reads the per-outcome constants off it, so both see byte-for-byte the
/// same thing - there is no parallel specialization.
pub(crate) fn specialize_frame(
    graph: &Graph,
    outs: &[(Vec<NodeId>, NodeId, NodeId)],
    forks: u8,
    decide: bool,
    room: Option<&crate::transpile::graph::Room>,
) -> (Graph, Vec<(usize, u8, u64, Vec<NodeId>)>) {
    // --- 1. which forks each outcome actually depends on ---
    let cones = graph.split_cones();
    let bits_of = |fields: &[NodeId], ok: NodeId, live: NodeId| -> Vec<u8> {
        let mut m = cones[ok as usize] | cones[live as usize];
        for &f in fields {
            m |= cones[f as usize];
        }
        (0..forks).filter(|d| m & (1u64 << d) != 0).collect()
    };

    // --- 2. specialize, per outcome, over (button, its own forks) ---
    let mut sp = Graph::new();
    // (outcome, button, fork configuration, roots) where roots is the
    // outcome's fields in order, then `ok`, then `live`.
    let mut cands: Vec<(usize, u8, u64, Vec<NodeId>)> = Vec::new();
    for (oi, (fields, ok, live)) in outs.iter().enumerate() {
        let mut want: Vec<NodeId> = fields.clone();
        want.push(*ok);
        want.push(*live);
        // An outcome reaches a fraction of the graph, and mapping the
        // whole arena once per (button, configuration) is most of the
        // work and none of the answer.
        let need = reachable(graph, &want);
        let bits = bits_of(fields, *ok, *live);
        // Buttons first, with the forks left standing. If two
        // assignments agree before the forks are resolved they agree
        // after - resolving is substitution, and substitution preserves
        // equality - so this prunes the product soundly, and cheaply
        // enough to be worth a separate pass.
        let reps: Vec<u8> = {
            let mut probe = Graph::new();
            let mut seen: BTreeMap<Vec<NodeId>, u8> = BTreeMap::new();
            for m in 0u8..64 {
                let map = graph.specialize_subset_into(m, None, Some(&need), &mut probe);
                let sig: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                seen.entry(sig).or_insert(m);
            }
            let mut v: Vec<u8> = seen.into_values().collect();
            v.sort_unstable();
            v
        };
        for m in reps {
            for k in 0..(1u64 << bits.len()) {
                let mut sm = 0u64;
                for (i, d) in bits.iter().enumerate() {
                    if k & (1 << i) != 0 {
                        sm |= 1u64 << d;
                    }
                }
                let map = graph.specialize_subset_into(m, Some(sm), Some(&need), &mut sp);
                let roots: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                cands.push((oi, m, sm, roots));
            }
        }
    }

    // --- 3. decide the boolean layer, on the RESOLVED graph ---
    if decide {
        let all: Vec<NodeId> = cands.iter().flat_map(|c| c.3.iter().copied()).collect();
        let (g1, m1, _) =
            crate::transpile::ival::fold(&sp, &all, room).expect("interval fold");
        let r1: Vec<NodeId> = all.iter().map(|x| m1[*x as usize]).collect();
        let (g2, m2, _) = crate::transpile::bdd::simplify_until_stable(&g1, &r1, 1 << 22, 4);
        let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
        let (g3, m3, _) =
            crate::transpile::ival::fold(&g2, &r2, room).expect("interval fold 2");
        let mut it = r2.iter().map(|x| m3[*x as usize]);
        for c in cands.iter_mut() {
            for r in c.3.iter_mut() {
                *r = it.next().expect("one decided root per root");
            }
        }
        sp = g3;
    }

    // --- 4. identical roots are the same body ---
    let bodies: Vec<(usize, u8, u64, Vec<NodeId>)> = {
        let mut seen: BTreeSet<(usize, Vec<NodeId>)> = BTreeSet::new();
        cands
            .into_iter()
            .filter(|c| seen.insert((c.0, c.3.clone())))
            .collect()
    };
    (sp, bodies)
}

/// The fused bodies' constant-output analysis. For each outcome field,
/// `konst_av` is the `AV` it holds in EVERY row: set when every body of
/// that outcome computes the same node for it (button- and
/// fork-independent) and that node is a compile-time constant. The ASM
/// accumulator writes such a column ONCE as `Col::U` instead of pushing
/// it per row, and the within-chunk dedup fold skips it. Returns the
/// number of distinct bodies (a size measure for the probes).
pub(crate) fn lower_outcomes(e: &Emit, outs: &mut [Outcome]) -> usize {
    let outs_spec: Vec<(Vec<NodeId>, NodeId, NodeId)> = outs
        .iter()
        .map(|o| (o.of.fields.iter().map(|f| f.node).collect(), o.ok, o.live))
        .collect();
    let (sp, bodies) =
        specialize_frame(&e.graph, &outs_spec, e.fork_depth as u8, e.decide, e.room.as_ref());
    for (oi, o) in outs.iter_mut().enumerate() {
        let mine: Vec<&Vec<NodeId>> = bodies.iter().filter(|b| b.0 == oi).map(|b| &b.3).collect();
        let Some(first) = mine.first() else {
            continue;
        };
        for (fi, f) in o.of.fields.iter_mut().enumerate() {
            let node = first[fi];
            // The arena is hash-consed, so "every body writes the same
            // node" is "every body writes the same value".
            let agreed = mine.iter().all(|r| r[fi] == node);
            f.konst_av = if !agreed {
                None
            } else {
                use celeste_core::pico8_num::Pico8Num;
                use celeste_engine::runtime2::AV;
                match sp.get(node).op {
                    Op::Const(lo, hi) if lo == hi => Some(AV::Num(Pico8Num::from_raw(lo))),
                    Op::Const(lo, hi) => {
                        Some(AV::Ival(Pico8Num::from_raw(lo), Pico8Num::from_raw(hi)))
                    }
                    Op::ConstBool(b) => Some(AV::Bool(b)),
                    _ => None,
                }
            };
        }
    }
    bodies.len()
}
