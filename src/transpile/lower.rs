//! Graph -> the fused, fork-free frame (`plans/multi-output-fusion.md`).
//!
//! The tracer's walk records what each value MEANS as a node in
//! `transpile::graph`; `specialize_frame` resolves every fork
//! configuration of that graph into ONE hash-consed arena, and
//! `lower_outcomes` reads the per-outcome constants the kernel needs off
//! the result. The ASM backend (`trace::emit::asm_fused`) assembles the
//! same arena. (The Rust-source renderer that used to live here went with
//! the generated kernel crates; nothing rendered text any more, 2026-09-08.)

use std::collections::BTreeMap;

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
/// which lanes reach it (`live`) and on which of those its row is undefined
/// (`error`, `trace::error`).
pub(crate) struct Outcome {
    pub(crate) of: OutFields,
    pub(crate) error: NodeId,
    pub(crate) live: NodeId,
    /// Fields keyed on another node than they store: `(field index, key
    /// node)`. The key nodes are roots too, after `error` and `live`.
    pub(crate) keys: Vec<(usize, NodeId)>,
}

/// Specialize a traced frame's graph into ONE fused, hash-consed arena.
///
/// Every fork configuration is resolved: `Split`/`SplitValid`/`SplitInt`
/// and the table forks become `Frag`/`FragOk`/`IntFrag` and select chains
/// (ordinary ops), so NO fork node survives and configurations that agree
/// share nodes ("duplicate, then fuse again"). The six buttons are forks
/// like any other (`Symbolic::both_values`): a configuration of theirs is a
/// set of inputs, and it folds them to constants. Returns the fused graph
/// and one body per DISTINCT (outcome, row): `(outcome, splits, roots)`,
/// where `roots` is that outcome's fields in order, then `error`, then
/// `live`, then its key nodes (`Outcome::keys`), as node ids in the fused
/// graph.
///
/// This is the SINGLE source of the specialized compute: the AVX-512
/// backend assembles it (`trace::emit::asm_fused`) and `lower_outcomes`
/// reads the per-outcome constants off it, so both see byte-for-byte the
/// same thing - there is no parallel specialization.
/// One specialized body: `(outcome, splits, roots)` - `splits` the fragment
/// per fork (`graph::ANY_VALID` for a dead one; 0 for a fork outside the
/// outcome's cone).
pub(crate) type SpecializedBody = (usize, Vec<u8>, Vec<NodeId>);

/// Per fork, a table fork's arity and the entries a configuration's lanes
/// can reach (`Graph::specialize_subset_into`'s `tabs`).
type Tabs = Vec<(u8, Vec<(i32, i32)>)>;

/// `want`'s images under the configuration `cfg`, specialized into `out`.
fn roots_under(graph: &Graph, cfg: &[u8], tabs: &Tabs, need: &[bool], want: &[NodeId], out: &mut Graph) -> Vec<NodeId> {
    let map = graph.specialize_subset_into(cfg, Some(tabs), Some(need), out);
    want.iter().map(|r| map[*r as usize]).collect()
}

/// The roots of each configuration of `cfgs` DECIDED (the interval fold with
/// the region's ranges and the map), in one fresh arena so equal results are
/// one node: a test of the player against one world's platform folds only
/// with the ranges, and before that every world is a different node.
fn decided_roots(
    graph: &Graph,
    cfgs: &[Vec<u8>],
    (tabs, need, want): (&Tabs, &[bool], &[NodeId]),
    room: Option<&crate::transpile::graph::Room>,
    ranges: &std::collections::HashMap<u32, (i32, i32)>,
) -> Vec<Vec<NodeId>> {
    let mut probe = graph.like();
    let mut decided = graph.like();
    cfgs.iter()
        .map(|c| {
            let sig = roots_under(graph, c, tabs, need, want, &mut probe);
            let (dm, _) = crate::transpile::ival::fold_with_into(&probe, &sig, room, ranges, &mut decided).expect("interval fold");
            sig.iter().map(|r| dm[*r as usize]).collect()
        })
        .collect()
}

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
    // --- 1. which forks each outcome actually depends on: the forks in its
    // cone, read off the reachable set it needs anyway (no per-node mask, so
    // no limit on how many forks a frame has) ---
    let bits_of = |need: &[bool]| -> Vec<u8> {
        let mut forks: std::collections::BTreeSet<u8> = Default::default();
        for (i, n) in need.iter().enumerate() {
            if *n {
                if let Op::Split(d) | Op::SplitValid(d) | Op::SplitInt(d) | Op::SplitTab(d) | Op::SplitValidTab(d) | Op::SplitKeyTab(d) | Op::SplitOkTab(d) = graph.get(i as NodeId).op {
                    forks.insert(d);
                }
            }
        }
        forks.into_iter().collect()
    };

    // --- 2. per outcome, its configurations ---
    //
    // The forks in the outcome's cone are resolved ONE AT A TIME, in fork
    // order, with the later ones left standing (`graph::OPEN`), and a
    // configuration is kept only where its roots differ from every one kept
    // before it at that step. Resolving is substitution, and substitution
    // preserves equality, so two configurations that agree with the rest
    // standing agree in every completion, and one of them is all the kernel
    // needs. This is what folds the buttons' 64 assignments to the ones
    // that differ (left and right together being neither, a jump nothing
    // can take being no jump), and it does the same for every other fork.
    let mut sp = graph.like();
    let mut cands: Vec<SpecializedBody> = Vec::new();
    // The table forks' operands, per fork: how many fragments a
    // configuration's lanes can need, and which entries they can reach, is
    // decided from the operand's range under the forks resolved before it
    // (`ranges`: the bucket dispatch's specialization).
    let fork_operands: Vec<Vec<NodeId>> = (0..forks)
        .map(|d| {
            (0..graph.len() as NodeId)
                .filter(|&n| matches!(graph.get(n).op, Op::SplitTab(dd) | Op::SplitValidTab(dd) | Op::SplitKeyTab(dd) | Op::SplitOkTab(dd) if dd == d))
                .map(|n| graph.get(n).args[0])
                .collect()
        })
        .collect();
    for (oi, (fields, error, live, keys)) in outs.iter().enumerate() {
        let mut want: Vec<NodeId> = fields.clone();
        want.push(*error);
        want.push(*live);
        want.extend(keys.iter().copied());
        // An outcome reaches a fraction of the graph, and mapping the
        // whole arena once per configuration is most of the work and none
        // of the answer.
        let need = reachable(graph, &want);
        let bits = bits_of(&need);
        // The forks whose validity the outcome reads.
        let validity_read: std::collections::BTreeSet<u8> = (0..graph.len())
            .filter(|&n| need[n])
            .filter_map(|n| match graph.get(n as NodeId).op {
                Op::SplitValid(d) => Some(d),
                _ => None,
            })
            .collect();
        // Every configuration's roots land in ONE arena, so a comparison of
        // roots is a comparison of node ids. The forks outside the cone are
        // unread: 0.
        let mut probe = graph.like();
        let mut root_cfg = vec![0u8; forks as usize];
        for &d in &bits {
            root_cfg[d as usize] = crate::transpile::graph::OPEN;
        }
        let root_tabs: Tabs = vec![(0, Vec::new()); forks as usize];
        let map0 = graph.specialize_subset_into(&root_cfg, Some(&root_tabs), Some(&need), &mut probe);
        // The seeded cells, by node in `probe` (a cell is the same node under
        // every configuration).
        let seeds: std::collections::HashMap<NodeId, (i64, i64)> = (0..graph.len() as NodeId)
            .filter(|&n| need[n as usize])
            .filter_map(|n| match graph.get(n).op {
                Op::Cell(c) => ranges.get(&c).map(|(lo, hi)| (map0[n as usize], (*lo as i64, *hi as i64))),
                _ => None,
            })
            .collect();
        let mut memo = std::collections::HashMap::new();
        let mut classes: Vec<(Vec<u8>, Tabs)> = vec![(root_cfg, root_tabs)];
        // The most fragments one configuration enumerates per fork (for the
        // trace).
        let mut ways_max = vec![0u8; bits.len()];
        for (i, &d) in bits.iter().enumerate() {
            let table = graph.fork_table(d);
            let mut next: Vec<(Vec<u8>, Tabs)> = Vec::new();
            let mut seen: std::collections::HashSet<Vec<NodeId>> = Default::default();
            for (cfg, mut tabs) in classes {
                // A table fork: what THIS configuration's lanes can need -
                // the entries its operand's pieces reach, and the most
                // entries one piece crosses, the arity (a lane's interval
                // lies within one piece). The fork is resolved with exactly
                // these, so `SplitOkTab` checks the arity the configurations
                // enumerate: a lane the analysis got wrong declines, it is
                // never dropped.
                if !table.is_empty() {
                    let map = graph.specialize_subset_into(&cfg, Some(&tabs), Some(&need), &mut probe);
                    let mut reach = vec![false; table.len()];
                    let mut arity = 0usize;
                    let mut known = true;
                    for &operand in &fork_operands[d as usize] {
                        if !need[operand as usize] {
                            continue;
                        }
                        match crate::transpile::graph::pieces_of(&probe, &seeds, &mut memo, map[operand as usize]) {
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
                // Move and button forks keep their traced arity
                // (`Domain::flr_ways`, `Symbolic::both_values`), which their
                // `SplitOk` premise checks.
                let ways = if table.is_empty() { graph.fork_ways(d) } else { tabs[d as usize].0 };
                ways_max[i] = ways_max[i].max(ways);
                let with = |v: u8| {
                    let mut c = cfg.clone();
                    c[d as usize] = v;
                    c
                };
                let this = (&tabs, need.as_slice(), want.as_slice());
                // The children, each with its roots.
                let mut kids: Vec<(Vec<u8>, Vec<NodeId>)> = Vec::new();
                let plain: Vec<Vec<u8>> = (0..ways).map(with).collect();
                let mut raw: Option<Vec<Vec<NodeId>>> = None;
                // A DEAD fork: every fragment gives the same fields, keys and
                // error, and the same `live` once the fork's own validity is
                // set aside - it is taken once, validity true
                // (`graph::ANY_VALID`): the OR of its fragments' validities
                // is the lane covered. Room (6,0)'s no-player frames move all
                // ten platforms through a floor fork each and then widen
                // where they stand: 1024 configurations of one row per
                // outcome (2026-09-28). Grid forks only. A fork whose
                // validity the outcome does not read (a button has none) has
                // nothing to set aside: its plain values are the probe.
                if table.is_empty() && ways >= 2 {
                    let any: Vec<Vec<u8>> = (0..ways).map(|v| with(v | crate::transpile::graph::ANY_VALID)).collect();
                    let reads_validity = validity_read.contains(&d);
                    let probed: Vec<Vec<NodeId>> =
                        (if reads_validity { &any } else { &plain }).iter().map(|c| roots_under(graph, c, &tabs, &need, &want, &mut probe)).collect();
                    let dead = probed.iter().all(|r| *r == probed[0])
                        || (decide && {
                            let dec = decided_roots(graph, if reads_validity { &any } else { &plain }, this, room, ranges);
                            dec.iter().all(|r| *r == dec[0])
                        });
                    if dead {
                        kids.push((any[0].clone(), probed[0].clone()));
                    } else if !reads_validity {
                        raw = Some(probed);
                    }
                }
                if kids.is_empty() {
                    let raw: Vec<Vec<NodeId>> =
                        raw.unwrap_or_else(|| plain.iter().map(|c| roots_under(graph, c, &tabs, &need, &want, &mut probe)).collect());
                    // PER FORK, ITS VALUES THAT AGREE, compared DECIDED when
                    // the body is (`decide`). Built for a 128-way fork over
                    // the platform worlds (since replaced by deciding per
                    // world at compile time, `verify::Points`), which from
                    // one region mostly looked alike. Two values of a 2-way
                    // fork that agreed would have made it dead.
                    let dec = (decide && ways > 2).then(|| decided_roots(graph, &plain, this, room, ranges));
                    for (v, (c, r)) in plain.into_iter().zip(raw).enumerate() {
                        if !dec.as_ref().is_some_and(|dec| dec[..v].contains(&dec[v])) {
                            kids.push((c, r));
                        }
                    }
                }
                for (c, r) in kids {
                    if seen.insert(r) {
                        next.push((c, tabs.clone()));
                    }
                }
            }
            classes = next;
        }
        if trace {
            let ways: Vec<u8> = bits.iter().map(|&d| graph.fork_ways(d)).collect();
            eprintln!("[build]   outcome {oi}: forks {bits:?} traced ways {ways:?}, at most {ways_max:?} -> {} configurations", classes.len());
        }
        for (cfg, tabs) in classes {
            let roots = roots_under(graph, &cfg, &tabs, &need, &want, &mut sp);
            cands.push((oi, cfg, roots));
        }
    }

    let t_spec = t0.elapsed();

    build_add_duration(3, t_spec);
    let n_cands = cands.len();
    let t1 = std::time::Instant::now();
    // --- 3. decide the boolean layer, on the RESOLVED graph ---
    if decide {
        let all: Vec<NodeId> = cands.iter().flat_map(|c| c.2.iter().copied()).collect();
        let t_phase = std::time::Instant::now();
        let (g1, m1, _) =
            crate::transpile::ival::fold_with(&sp, &all, room, ranges).expect("interval fold");
        build_add(4, t_phase);
        let r1: Vec<NodeId> = all.iter().map(|x| m1[*x as usize]).collect();
        // DIAGNOSTIC (CELESTE_BUILD_TRACE): a body whose `error` folded to
        // true while it is live somewhere declines every lane it takes.
        // Say which disjunct: the leaves of the unfolded `error`'s Or-tree
        // the interval evaluator decided true.
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
                let (error, live) = (c.2[nfields], c.2[nfields + 1]);
                let error_true = matches!(g1.get(m1[error as usize]).op, Op::ConstBool(true));
                let live_false = matches!(g1.get(m1[live as usize]).op, Op::ConstBool(false));
                if !error_true || live_false || shown >= 3 {
                    continue;
                }
                shown += 1;
                let mut stack = vec![error];
                let mut leaves = Vec::new();
                while let Some(n) = stack.pop() {
                    let nd = sp.get(n);
                    if matches!(nd.op, Op::Or) {
                        stack.extend(nd.args.iter().copied());
                    } else if matches!(vals[n as usize], crate::transpile::graph::Val::Bool(Some(true))) {
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
                eprintln!("[build]   outcome {} cfg {:?}: error folds TRUE while live; true disjuncts: {}", c.0, c.1, leaves.join(" | "));
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
            for r in c.2.iter_mut() {
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
    // --- 4. candidates that write the same row are ONE body; a body live
    // nowhere (its guard decided false: a table-fork fragment the
    // specialized input range never reaches) is no body ---
    //
    // Candidates of one outcome with the same fields and keys write the
    // same row wherever they are live, so they fuse: `live` the OR of
    // theirs, and `error` - where the members' errors are one node, as they
    // are when error comes only from the row's own values - that node
    // unchanged (plans/graph-model.md section 4: fusion never touches
    // error), else `OR_i (live_i & error_i)`. Either way a lane declines
    // exactly where some candidate declined (`L & E` is `OR_i (L_i & E_i)`
    // when every `E_i` is `E`, and by definition otherwise; in the kernels'
    // three-valued masks too, both read where they MAY hold), and is kept
    // where some candidate kept it; the door keeps one copy of the row
    // either way. Before, a per-configuration `ok` or `live` node kept them
    // apart: room (3,0) with the fall floors unknown, 336 bodies over 14
    // distinct rows per outcome (2026-09-18).
    let bodies: Vec<SpecializedBody> = {
        let mut groups: Vec<Vec<SpecializedBody>> = Vec::new();
        let mut index: BTreeMap<(usize, Vec<NodeId>), usize> = BTreeMap::new();
        for c in cands {
            let nfields = outs[c.0].0.len();
            if matches!(sp.get(c.2[nfields + 1]).op, Op::ConstBool(false)) {
                continue;
            }
            let row: Vec<NodeId> = c.2[..nfields].iter().chain(&c.2[nfields + 2..]).copied().collect();
            match index.get(&(c.0, row.clone())) {
                Some(&gi) => groups[gi].push(c),
                None => {
                    index.insert((c.0, row), groups.len());
                    groups.push(vec![c]);
                }
            }
        }
        groups
            .into_iter()
            .map(|members| {
                let nfields = outs[members[0].0].0.len();
                let shared = members.iter().all(|m| m.2[nfields] == members[0].2[nfields]);
                let fused_error = if shared {
                    members[0].2[nfields]
                } else {
                    members.iter().fold(sp.leaf(Op::ConstBool(false)), |e, m| {
                        let here = sp.fold(Op::And, vec![m.2[nfields + 1], m.2[nfields]]);
                        sp.fold(Op::Or, vec![e, here])
                    })
                };
                let fused_live = members.iter().skip(1).fold(members[0].2[nfields + 1], |l, m| sp.fold(Op::Or, vec![l, m.2[nfields + 1]]));
                let mut b = members.into_iter().next().expect("a group has a member");
                b.2[nfields] = fused_error;
                b.2[nfields + 1] = fused_live;
                b
            })
            .collect()
    };

    (sp, bodies)
}

/// The fused bodies' constant-output analysis. For each outcome field,
/// `konst_av` is the `AV` it holds in EVERY row: set when every body of
/// that outcome computes the same node for it (configuration-
/// independent) and that node is a compile-time constant. The ASM
/// accumulator writes such a column ONCE as `Col::U` instead of pushing
/// it per row, and the within-chunk dedup fold skips it. Returns the
/// number of distinct bodies (a size measure for the probes).
pub(crate) fn lower_outcomes(e: &Emit, outs: &mut [Outcome]) -> (Graph, Vec<SpecializedBody>) {
    let outs_spec: Vec<(Vec<NodeId>, NodeId, NodeId, Vec<NodeId>)> = outs
        .iter()
        .map(|o| (o.of.fields.iter().map(|f| f.node).collect(), o.error, o.live, o.keys.iter().map(|(_, n)| *n).collect()))
        .collect();
    let (sp, bodies) =
        specialize_frame(&e.graph, &outs_spec, e.fork_depth as u8, e.decide, e.room.as_ref(), &e.ranges);
    for (oi, o) in outs.iter_mut().enumerate() {
        let mine: Vec<&Vec<NodeId>> = bodies.iter().filter(|b| b.0 == oi).map(|b| &b.2).collect();
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
