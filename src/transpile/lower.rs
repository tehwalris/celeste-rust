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
    /// Fields keyed on another node than they store: `(field index, key
    /// node)`. The key nodes are roots too, after `ok` and `live`.
    pub(crate) keys: Vec<(usize, NodeId)>,
}

/// Specialize a traced frame's graph into ONE fused, hash-consed arena.
///
/// Every (button, fork configuration) is resolved: `Free` becomes a
/// constant and `Split`/`SplitValid` become `Frag`/`FragOk` (ordinary
/// unary ops), so NO fork node survives and configurations that agree
/// share nodes ("duplicate, then fuse again"). Steps 1-4 of the frame
/// lowering. Returns the fused graph and one body per DISTINCT (outcome,
/// roots): `(outcome, frees, splits, roots)`, where `roots` is that
/// outcome's fields in order, then `ok`, then `live`, then its key
/// nodes (`Outcome::keys`), as node ids in the fused graph.
///
/// This is the SINGLE source of the specialized compute: the AVX-512
/// backend assembles it (`trace::emit::asm_fused`) and `lower_outcomes`
/// reads the per-outcome constants off it, so both see byte-for-byte the
/// same thing - there is no parallel specialization.
/// One specialized body: `(outcome, frees, splits, roots)`.
pub(crate) type SpecializedBody = (usize, u8, Vec<u8>, Vec<NodeId>);

pub(crate) fn specialize_frame(
    graph: &Graph,
    outs: &[(Vec<NodeId>, NodeId, NodeId, Vec<NodeId>)],
    forks: u8,
    decide: bool,
    room: Option<&crate::transpile::graph::Room>,
    ranges: &std::collections::HashMap<u32, (i32, i32)>,
) -> (Graph, Vec<SpecializedBody>) {
    let trace = std::env::var_os("CELESTE_BUILD_TRACE").is_some();
    let t0 = std::time::Instant::now();
    // --- 1. which forks each outcome actually depends on ---
    let cones = graph.split_cones();
    let bits_of = |fields: &[NodeId], ok: NodeId, live: NodeId, keys: &[NodeId]| -> Vec<u8> {
        let mut m = cones[ok as usize] | cones[live as usize];
        for &f in fields.iter().chain(keys) {
            m |= cones[f as usize];
        }
        (0..forks).filter(|d| m & (1u64 << d) != 0).collect()
    };

    // --- 2. specialize, per outcome, over (button, its own forks) ---
    let mut sp = graph.like();
    // (outcome, button, fork configuration, roots) where roots is the
    // outcome's fields in order, then `ok`, then `live`.
    let mut cands: Vec<SpecializedBody> = Vec::new();
    // The table forks' operands, per fork: which fragments a button rep
    // can reach is decided from the operand's range under that rep
    // (`ranges`: the bucket dispatch's specialization), so the product
    // below runs over the reachable fragments only.
    let table_operands: Vec<Vec<NodeId>> = (0..forks)
        .map(|d| {
            (0..graph.len() as NodeId)
                .filter(|&n| matches!(graph.get(n).op, Op::SplitTab(dd) if dd == d))
                .map(|n| graph.get(n).args[0])
                .collect()
        })
        .collect();
    for (oi, (fields, ok, live, keys)) in outs.iter().enumerate() {
        let mut want: Vec<NodeId> = fields.clone();
        want.push(*ok);
        want.push(*live);
        want.extend(keys.iter().copied());
        // An outcome reaches a fraction of the graph, and mapping the
        // whole arena once per (button, configuration) is most of the
        // work and none of the answer.
        let need = reachable(graph, &want);
        let bits = bits_of(fields, *ok, *live, keys);
        // Buttons first, with the forks left standing. If two
        // assignments agree before the forks are resolved they agree
        // after - resolving is substitution, and substitution preserves
        // equality - so this prunes the product soundly, and cheaply
        // enough to be worth a separate pass.
        let reps: Vec<u8> = {
            let mut probe = graph.like();
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
        let mut total_cfgs = 0u64;
        for m in reps.iter().copied() {
            // Per fork of this outcome, the fragments this rep can reach.
            let mut probe = graph.like();
            let map = graph.specialize_subset_into(m, None, Some(&need), &mut probe);
            // The seeded cells, by node, in the rep's graph; the pieces
            // analysis over them decides which fragments the rep reaches.
            let seeds: std::collections::HashMap<NodeId, (i64, i64)> = (0..probe.len() as NodeId)
                .filter_map(|n| match probe.get(n).op {
                    Op::Cell(c) => ranges.get(&c).map(|(lo, hi)| (n, (*lo as i64, *hi as i64))),
                    _ => None,
                })
                .collect();
            let mut memo = std::collections::HashMap::new();
            let valid: Vec<Vec<u8>> = bits
                .iter()
                .map(|&d| {
                    let ways = graph.fork_ways(d);
                    let table = graph.fork_table(d);
                    if table.is_empty() {
                        return (0..ways).collect();
                    }
                    let mut ok: Vec<bool> = vec![true; table.len()];
                    for &operand in &table_operands[d as usize] {
                        let mapped = map[operand as usize];
                        if mapped == NodeId::MAX || !need[operand as usize] {
                            continue;
                        }
                        if let Some(pieces) = crate::transpile::graph::pieces_of(&probe, &seeds, &mut memo, mapped) {
                            for (c, (tlo, thi)) in table.iter().enumerate() {
                                if !pieces.iter().any(|p| (*tlo as i64) <= p.1 && (*thi as i64) >= p.0) {
                                    ok[c] = false;
                                }
                            }
                        }
                    }
                    (0..table.len() as u8).filter(|&c| ok[c as usize]).collect()
                })
                .collect();
            let total: u64 = valid.iter().map(|v| v.len() as u64).product();
            total_cfgs += total;
            for k in 0..total {
                let mut cfg = vec![0u8; forks as usize];
                let mut r = k;
                for (i, d) in bits.iter().enumerate() {
                    let n = valid[i].len() as u64;
                    cfg[*d as usize] = valid[i][(r % n) as usize];
                    r /= n;
                }
                let map = graph.specialize_subset_into(m, Some(&cfg), Some(&need), &mut sp);
                let roots: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                cands.push((oi, m, cfg, roots));
            }
        }
        if trace {
            let ways: Vec<u8> = bits.iter().map(|&d| graph.fork_ways(d)).collect();
            eprintln!("[build]   outcome {oi}: forks {bits:?} ways {ways:?} -> {total_cfgs} configurations over {} button reps", reps.len());
        }
    }

    let t_spec = t0.elapsed();
    let n_cands = cands.len();
    let t1 = std::time::Instant::now();
    // --- 3. decide the boolean layer, on the RESOLVED graph ---
    if decide {
        let all: Vec<NodeId> = cands.iter().flat_map(|c| c.3.iter().copied()).collect();
        let (g1, m1, _) =
            crate::transpile::ival::fold_with(&sp, &all, room, ranges).expect("interval fold");
        let r1: Vec<NodeId> = all.iter().map(|x| m1[*x as usize]).collect();
        let (g2, m2, _) = crate::transpile::bdd::simplify_until_stable(&g1, &r1, 1 << 22, 4);
        let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
        let (g3, m3, _) =
            crate::transpile::ival::fold_with(&g2, &r2, room, ranges).expect("interval fold 2");
        let mut it = r2.iter().map(|x| m3[*x as usize]);
        for c in cands.iter_mut() {
            for r in c.3.iter_mut() {
                *r = it.next().expect("one decided root per root");
            }
        }
        sp = g3;
    }

    let t_decide = t1.elapsed();
    if trace {
        eprintln!(
            "[build] specialize {} candidates, {} nodes: {:.1}s; decide: {:.1}s ({} nodes after)",
            n_cands,
            sp.len(),
            t_spec.as_secs_f64(),
            t_decide.as_secs_f64(),
            sp.len()
        );
    }
    // --- 4. identical roots are the same body; a body live nowhere
    // (its guard decided false: a table-fork fragment the specialized
    // input range never reaches) is no body ---
    let bodies: Vec<SpecializedBody> = {
        let mut seen: BTreeSet<(usize, Vec<NodeId>)> = BTreeSet::new();
        cands
            .into_iter()
            .filter(|c| {
                let nfields = outs[c.0].0.len();
                !matches!(sp.get(c.3[nfields + 1]).op, Op::ConstBool(false))
            })
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
pub(crate) fn lower_outcomes(e: &Emit, outs: &mut [Outcome]) -> (Graph, Vec<SpecializedBody>) {
    let outs_spec: Vec<(Vec<NodeId>, NodeId, NodeId, Vec<NodeId>)> = outs
        .iter()
        .map(|o| (o.of.fields.iter().map(|f| f.node).collect(), o.ok, o.live, o.keys.iter().map(|(_, n)| *n).collect()))
        .collect();
    let (sp, bodies) =
        specialize_frame(&e.graph, &outs_spec, e.fork_depth as u8, e.decide, e.room.as_ref(), &e.ranges);
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
    (sp, bodies)
}
