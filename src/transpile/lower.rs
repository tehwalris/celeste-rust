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

/// The kernel build's phases, for the accounting below.
pub(crate) const BUILD_PHASES: [&str; 9] = [
    "fixpoint trace",
    "fixpoint successors",
    "bind",
    "specialize",
    "decide: interval fold",
    "decide: boolean simplification",
    "decide: interval fold 2",
    "fuse + key chains",
    "assemble (gcc + load)",
];

/// Build-time accounting (2026-09-15): nanoseconds per phase of
/// `BUILD_PHASES`, summed over workers and over every build since the last
/// `build_profile`, so a set's build time can be attributed.
static BUILD_NS: [std::sync::atomic::AtomicU64; 9] = [const { std::sync::atomic::AtomicU64::new(0) }; 9];

/// Add the time since `t` to phase `phase`.
pub(crate) fn build_add(phase: usize, t: std::time::Instant) {
    BUILD_NS[phase].fetch_add(t.elapsed().as_nanos() as u64, std::sync::atomic::Ordering::Relaxed);
}

/// Add `d` to phase `phase`.
pub(crate) fn build_add_duration(phase: usize, d: std::time::Duration) {
    BUILD_NS[phase].fetch_add(d.as_nanos() as u64, std::sync::atomic::Ordering::Relaxed);
}

/// The phases' totals since the last call, reset.
pub(crate) fn build_profile() -> String {
    BUILD_PHASES
        .iter()
        .zip(&BUILD_NS)
        .map(|(name, ns)| format!("{name} {:.1} s", ns.swap(0, std::sync::atomic::Ordering::Relaxed) as f64 / 1e9))
        .collect::<Vec<_>>()
        .join(", ")
}

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
    // The table forks' operands, per fork: how many fragments a button
    // rep's lanes can need, and which entries they can reach, is decided
    // from the operand's range under that rep (`ranges`: the bucket
    // dispatch's specialization).
    let fork_operands: Vec<Vec<NodeId>> = (0..forks)
        .map(|d| {
            (0..graph.len() as NodeId)
                .filter(|&n| matches!(graph.get(n).op, Op::SplitTab(dd) | Op::SplitValidTab(dd) | Op::SplitKeyTab(dd) | Op::SplitOkTab(dd) if dd == d))
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
                let map = graph.specialize_subset_into(m, None, None, Some(&need), &mut probe);
                let sig: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                seen.entry(sig).or_insert(m);
            }
            let mut v: Vec<u8> = seen.into_values().collect();
            v.sort_unstable();
            v
        };
        let mut total_cfgs = 0u64;
        // The most fragments one rep enumerates per fork (for the trace).
        let mut ways_max = vec![0u8; bits.len()];
        for m in reps.iter().copied() {
            let mut probe = graph.like();
            let map = graph.specialize_subset_into(m, None, None, Some(&need), &mut probe);
            // The seeded cells, by node, in the rep's graph.
            let seeds: std::collections::HashMap<NodeId, (i64, i64)> = (0..probe.len() as NodeId)
                .filter_map(|n| match probe.get(n).op {
                    Op::Cell(c) => ranges.get(&c).map(|(lo, hi)| (n, (*lo as i64, *hi as i64))),
                    _ => None,
                })
                .collect();
            let mut memo = std::collections::HashMap::new();
            // Per table fork, what THIS rep's lanes can need: the entries its
            // operand's pieces reach, and the most entries one piece crosses
            // - the arity (a lane's interval lies within one piece). The
            // fork is resolved with exactly these, so `SplitOkTab` checks
            // the arity the configurations enumerate: a lane the analysis
            // got wrong declines, it is never dropped.
            let mut tabs: Vec<(u8, Vec<(i32, i32)>)> = vec![(0, Vec::new()); forks as usize];
            for &d in &bits {
                let table = graph.fork_table(d);
                if table.is_empty() {
                    continue;
                }
                let mut reach = vec![false; table.len()];
                let mut arity = 0usize;
                let mut known = true;
                for &operand in &fork_operands[d as usize] {
                    let mapped = map[operand as usize];
                    if mapped == NodeId::MAX || !need[operand as usize] {
                        continue;
                    }
                    match crate::transpile::graph::pieces_of(&probe, &seeds, &mut memo, mapped) {
                        None => known = false,
                        Some(ps) => {
                            for p in &ps {
                                let mut n = 0usize;
                                for (c, (tlo, thi)) in table.iter().enumerate() {
                                    if (*tlo as i64) <= p.1 && (*thi as i64) >= p.0 {
                                        reach[c] = true;
                                        n += 1;
                                    }
                                }
                                arity = arity.max(n);
                            }
                        }
                    }
                }
                tabs[d as usize] = if known && arity > 0 {
                    (arity as u8, table.iter().zip(&reach).filter(|(_, r)| **r).map(|(e, _)| *e).collect())
                } else {
                    (graph.fork_ways(d), table.to_vec())
                };
            }
            // Move forks keep their traced arity (`Domain::flr_ways`: from
            // the same ranges, and button-independent - `move` runs before
            // `update`), which their `SplitOk` premise checks.
            let valid: Vec<u8> = bits
                .iter()
                .map(|&d| if graph.fork_table(d).is_empty() { graph.fork_ways(d) } else { tabs[d as usize].0 })
                .collect();
            let total: u64 = valid.iter().map(|&n| n as u64).product();
            total_cfgs += total;
            for (i, n) in valid.iter().enumerate() {
                ways_max[i] = ways_max[i].max(*n);
            }
            for k in 0..total {
                let mut cfg = vec![0u8; forks as usize];
                let mut r = k;
                for (i, d) in bits.iter().enumerate() {
                    let n = valid[i] as u64;
                    cfg[*d as usize] = (r % n) as u8;
                    r /= n;
                }
                let map = graph.specialize_subset_into(m, Some(&cfg), Some(&tabs), Some(&need), &mut sp);
                let roots: Vec<NodeId> = want.iter().map(|r| map[*r as usize]).collect();
                cands.push((oi, m, cfg, roots));
            }
        }
        if trace {
            let ways: Vec<u8> = bits.iter().map(|&d| graph.fork_ways(d)).collect();
            eprintln!(
                "[build]   outcome {oi}: forks {bits:?} traced ways {ways:?}, at most {ways_max:?} in one rep -> {total_cfgs} configurations over {} button reps",
                reps.len()
            );
        }
    }

    let t_spec = t0.elapsed();
    build_add_duration(3, t_spec);
    let n_cands = cands.len();
    let t1 = std::time::Instant::now();
    // --- 3. decide the boolean layer, on the RESOLVED graph ---
    if decide {
        let all: Vec<NodeId> = cands.iter().flat_map(|c| c.3.iter().copied()).collect();
        let t_phase = std::time::Instant::now();
        let (g1, m1, _) =
            crate::transpile::ival::fold_with(&sp, &all, room, ranges).expect("interval fold");
        build_add(4, t_phase);
        let r1: Vec<NodeId> = all.iter().map(|x| m1[*x as usize]).collect();
        // DIAGNOSTIC (CELESTE_BUILD_TRACE): a body whose `ok` folded to
        // false while it is live somewhere declines every lane it takes.
        // Say which conjunct: the leaves of the unfolded `ok`'s And-tree
        // the interval evaluator decided false.
        if trace {
            let cells = crate::transpile::ival::seed_cells(&sp, ranges);
            let vals = match room {
                Some(r) => sp.eval_lenient_in(&cells, r),
                None => sp.eval_lenient(&cells),
            }
            .expect("lenient eval");
            let mut shown = 0;
            for c in &cands {
                let nfields = outs[c.0].0.len();
                let (ok, live) = (c.3[nfields], c.3[nfields + 1]);
                let ok_false = matches!(g1.get(m1[ok as usize]).op, Op::ConstBool(false));
                let live_false = matches!(g1.get(m1[live as usize]).op, Op::ConstBool(false));
                if !ok_false || live_false || shown >= 3 {
                    continue;
                }
                shown += 1;
                let mut stack = vec![ok];
                let mut leaves = Vec::new();
                while let Some(n) = stack.pop() {
                    let nd = sp.get(n);
                    if matches!(nd.op, Op::And) {
                        stack.extend(nd.args.iter().copied());
                    } else if matches!(vals[n as usize], crate::transpile::graph::Val::Bool(Some(false))) {
                        leaves.push(format!("{n}={:?}({})", nd.op, nd.args.iter().map(|a| format!("{a}={:?}={:?}", sp.get(*a).op, vals[*a as usize])).collect::<Vec<_>>().join(", ")));
                        // Where an unbounded value came from: down the
                        // first full-range operand to the node that made it.
                        let is_top = |v: &crate::transpile::graph::Val| matches!(v, crate::transpile::graph::Val::Num(iv) if iv.low.as_raw_u32() == 0x8000_0000 || iv.high.as_raw_u32() == 0x7fff_ffff);
                        for a in nd.args.iter().copied() {
                            let mut cur = a;
                            let mut chain = Vec::new();
                            for _ in 0..40 {
                                let cn = sp.get(cur);
                                chain.push(format!("{cur}={:?}", cn.op));
                                match cn.args.iter().copied().find(|x| is_top(&vals[*x as usize])) {
                                    Some(x) => cur = x,
                                    None => {
                                        chain.push(format!("<- args {}", cn.args.iter().map(|x| format!("{x}={:?}={:?}", sp.get(*x).op, vals[*x as usize])).collect::<Vec<_>>().join(", ")));
                                        break;
                                    }
                                }
                            }
                            if chain.len() > 1 {
                                leaves.push(format!("top source: {}", chain.join(" <- ")));
                            }
                        }
                    }
                }
                eprintln!("[build]   outcome {} rep {} cfg {:?}: ok folds FALSE while live; false conjuncts: {}", c.0, c.1, c.2, leaves.join(" | "));
            }
        }
        // The boolean layer, simplified locally (`bdd::simplify_local`: one
        // pass, per node on a small BDD of its own cone).
        let t_phase = std::time::Instant::now();
        let (g2, m2, bst) =
            crate::transpile::bdd::simplify_local(&g1, &r1, crate::transpile::bdd::LOCAL_CAP, crate::transpile::bdd::LOCAL_EXPAND);
        build_add(5, t_phase);
        if trace {
            eprintln!(
                "[build]   boolean simplification: {:.2} s, {} -> {} nodes, {} constants, {} to atoms, {} merged ({} simple complements), {} duplicate leaves dropped, {} unanalysed (local table full)",
                t_phase.elapsed().as_secs_f64(),
                bst.before,
                bst.after,
                bst.constants,
                bst.to_atom,
                bst.merged,
                bst.merged_simple,
                bst.deduped,
                bst.unanalysed
            );
        }
        let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
        let t_phase = std::time::Instant::now();
        let (g3, m3, _) =
            crate::transpile::ival::fold_with(&g2, &r2, room, ranges).expect("interval fold 2");
        build_add(6, t_phase);
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
