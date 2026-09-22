//! Graph -> the fused, fork-free frame (`plans/multi-output-fusion.md`).
//!
//! The tracer's walk records what each value MEANS as a node in
//! `transpile::graph`; `specialize_frame` resolves every (button, fork)
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
        // --- 1. which forks this outcome actually depends on ---
        let bits: Vec<u8> = {
            let mut s = std::collections::BTreeSet::new();
            for (i, n) in need.iter().enumerate() {
                if *n {
                    if let Some(d) = crate::transpile::graph::fork_of(&graph.get(i as NodeId).op) {
                        s.insert(d);
                    }
                }
            }
            s.into_iter().collect()
        };
        // The row (fields and keys) and `live`: what the tree's internal
        // nodes specialize to decide which open forks to split on.
        let nf = fields.len();
        let mut probe_roots: Vec<NodeId> = fields.clone();
        probe_roots.extend(keys.iter().copied());
        probe_roots.push(*live);
        let need_probe = reachable(graph, &probe_roots);
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
        let mut product_cfgs = 0f64;
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
            let ways: Vec<u8> = (0..forks)
                .map(|d| if graph.fork_table(d).is_empty() { graph.fork_ways(d) } else { tabs[d as usize].0 })
                .collect();
            product_cfgs += bits.iter().map(|&d| ways[d as usize] as f64).product::<f64>();
            for (i, d) in bits.iter().enumerate() {
                ways_max[i] = ways_max[i].max(ways[*d as usize]);
            }
            // --- the TREE: a fork is split only where the row reads it ---
            let mut cfg = vec![0u8; forks as usize];
            for &d in &bits {
                cfg[d as usize] = crate::transpile::graph::OPEN;
            }
            let ctx = Tree { graph, m, tabs: &tabs, need: &need, need_probe: &need_probe, want: &want, probe_roots: &probe_roots, nf, bits: &bits, ways: &ways };
            let mut probe = graph.like();
            let mut leaves = Vec::new();
            tree(&ctx, &mut cfg, &mut probe, &mut sp, &mut leaves);
            total_cfgs += leaves.len() as u64;
            for (cfg, roots) in leaves {
                cands.push((oi, m, cfg, roots));
            }
        }
        if trace {
            let ways: Vec<u8> = bits.iter().map(|&d| graph.fork_ways(d)).collect();
            eprintln!(
                "[build]   outcome {oi}: forks {bits:?} traced ways {ways:?}, at most {ways_max:?} in one rep -> {total_cfgs} bodies of {product_cfgs:.0} configurations over {} button reps",
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
    // --- 4. candidates that write the same row are ONE body; a body live
    // nowhere (its guard decided false: a table-fork fragment the
    // specialized input range never reaches) is no body ---
    //
    // Candidates of one outcome with the same fields and keys write the
    // same row wherever they are live, so they fuse: `live` the OR of
    // theirs, `ok` the conjunction of `live_i -> ok_i`. A lane then
    // declines exactly where some candidate declined (`L & !O` is
    // `OR_i (L_i & !O_i)`, in the kernels' three-valued masks too: an
    // unknown `live` reads as live, an unknown `ok` does not hold), and is
    // kept where some candidate kept it; the door keeps one copy of the
    // row either way. Before, a per-configuration `ok` or `live` node kept
    // them apart: room (3,0) with the fall floors unknown, 336 bodies over
    // 14 distinct rows per outcome (2026-09-18).
    let bodies: Vec<SpecializedBody> = {
        let mut groups: Vec<(SpecializedBody, usize)> = Vec::new();
        let mut index: BTreeMap<(usize, Vec<NodeId>), usize> = BTreeMap::new();
        for c in cands {
            let nfields = outs[c.0].0.len();
            let (ok, live) = (c.3[nfields], c.3[nfields + 1]);
            if matches!(sp.get(live).op, Op::ConstBool(false)) {
                continue;
            }
            let row: Vec<NodeId> = c.3[..nfields].iter().chain(&c.3[nfields + 2..]).copied().collect();
            match index.get(&(c.0, row.clone())) {
                Some(&gi) => {
                    let (g, members) = &mut groups[gi];
                    let (g_ok, g_live) = (g.3[nfields], g.3[nfields + 1]);
                    // The first member's `ok` joins guarded by its own `live`.
                    let g_ok = if *members == 1 { guarded_ok(&mut sp, g_live, g_ok) } else { g_ok };
                    let mine = guarded_ok(&mut sp, live, ok);
                    g.3[nfields] = sp.fold(Op::And, vec![g_ok, mine]);
                    g.3[nfields + 1] = sp.fold(Op::Or, vec![g_live, live]);
                    *members += 1;
                }
                None => {
                    index.insert((c.0, row), groups.len());
                    groups.push((c, 1));
                }
            }
        }
        groups.into_iter().map(|(b, _)| b).collect()
    };
    (sp, bodies)
}

/// `live -> ok`: what a fused body's `ok` conjoins per member (`specialize_frame`).
fn guarded_ok(sp: &mut Graph, live: NodeId, ok: NodeId) -> NodeId {
    let dead = sp.fold(Op::Not, vec![live]);
    sp.fold(Op::Or, vec![dead, ok])
}

/// One (outcome, button rep)'s fork tree (`tree`).
struct Tree<'a> {
    graph: &'a Graph,
    m: u8,
    tabs: &'a [(u8, Vec<(i32, i32)>)],
    /// What the outcome's roots reach, and what the row and `live` reach.
    need: &'a [bool],
    need_probe: &'a [bool],
    /// The outcome's roots: fields, `ok`, `live`, keys.
    want: &'a [NodeId],
    /// Fields, keys, then `live`.
    probe_roots: &'a [NodeId],
    nf: usize,
    /// The outcome's forks, and every fork's arity in this rep.
    bits: &'a [u8],
    ways: &'a [u8],
}

/// The bodies of one (outcome, button rep), as a TREE over its forks
/// rather than their full product (plans/platforms-unknown.md, 2026-09-22).
///
/// At each node, `cfg` holds the forks fixed so far and the rest `OPEN`.
/// The row (fields and keys) and `live` are specialized under it: a node
/// whose `live` folds false has no body under it; otherwise it splits on
/// the highest open fork the ROW still reads (the order the product
/// enumerated in, when every fork is read). Where the row reads no open
/// fork, the configurations below write one row, so they are one body:
/// the forks only `ok` or `live` read are quantified out of those by the
/// fusion rule (`quantify`) - what step 4 does to candidates with the same
/// row, without enumerating them. Exact: the same rows, `live` and `ok`
/// as the product fused.
fn tree(t: &Tree, cfg: &mut Vec<u8>, probe: &mut Graph, sp: &mut Graph, out: &mut Vec<(Vec<u8>, Vec<NodeId>)>) {
    use crate::transpile::graph::OPEN;
    let open: Vec<u8> = t.bits.iter().copied().filter(|d| cfg[*d as usize] == OPEN).collect();
    if !open.is_empty() {
        let map = t.graph.specialize_subset_into(t.m, Some(cfg), Some(t.tabs), Some(t.need_probe), probe);
        let live = map[t.probe_roots[t.probe_roots.len() - 1] as usize];
        if matches!(probe.get(live).op, Op::ConstBool(false)) {
            return;
        }
        let row: Vec<NodeId> = t.probe_roots[..t.probe_roots.len() - 1].iter().map(|r| map[*r as usize]).collect();
        if let Some(&d) = forks_read(probe, &row, &open).iter().next_back() {
            for v in 0..t.ways[d as usize] {
                cfg[d as usize] = v;
                tree(t, cfg, probe, sp, out);
            }
            cfg[d as usize] = OPEN;
            return;
        }
    }
    let map = t.graph.specialize_subset_into(t.m, Some(cfg), Some(t.tabs), Some(t.need), sp);
    let mut roots: Vec<NodeId> = t.want.iter().map(|r| map[*r as usize]).collect();
    if !open.is_empty() {
        let (ok, live) = quantify(sp, roots[t.nf], roots[t.nf + 1], &open, t.ways, t.tabs);
        roots[t.nf] = ok;
        roots[t.nf + 1] = live;
    }
    out.push((cfg.clone(), roots));
}

/// The forks of `among` whose nodes `roots` reach in `g`.
fn forks_read(g: &Graph, roots: &[NodeId], among: &[u8]) -> std::collections::BTreeSet<u8> {
    let mut found = std::collections::BTreeSet::new();
    let mut seen = std::collections::HashSet::new();
    let mut stack = roots.to_vec();
    while let Some(n) = stack.pop() {
        if !seen.insert(n) {
            continue;
        }
        let node = g.get(n);
        if let Some(d) = crate::transpile::graph::fork_of(&node.op) {
            if among.contains(&d) {
                found.insert(d);
                if found.len() == among.len() {
                    break;
                }
            }
        }
        stack.extend(node.args.iter().copied());
    }
    found
}

/// `ok` and `live` with the forks of `open` they read quantified out, one
/// at a time: `live` the OR over the fork's fragments, `ok` the AND of
/// `live_c -> ok_c` - the fusion rule of `specialize_frame`'s step 4, over
/// the configurations that write this row.
fn quantify(sp: &mut Graph, ok: NodeId, live: NodeId, open: &[u8], ways: &[u8], tabs: &[(u8, Vec<(i32, i32)>)]) -> (NodeId, NodeId) {
    let (mut ok, mut live) = (ok, live);
    for d in forks_read(sp, &[ok, live], open) {
        let (mut l_all, mut o_all): (Option<NodeId>, Option<NodeId>) = (None, None);
        for c in 0..ways[d as usize] {
            let r = resolve(sp, &[live, ok], d, c, tabs);
            let g = guarded_ok(sp, r[0], r[1]);
            l_all = Some(match l_all {
                None => r[0],
                Some(x) => sp.fold(Op::Or, vec![x, r[0]]),
            });
            o_all = Some(match o_all {
                None => g,
                Some(x) => sp.fold(Op::And, vec![x, g]),
            });
        }
        live = l_all.expect("a fork has a fragment");
        ok = o_all.expect("a fork has a fragment");
    }
    (ok, live)
}

/// `roots` rebuilt in `sp` with fork `d` resolved to fragment `c` (operands
/// precede their node, so the cone in ascending order is one pass).
fn resolve(sp: &mut Graph, roots: &[NodeId], d: u8, c: u8, tabs: &[(u8, Vec<(i32, i32)>)]) -> Vec<NodeId> {
    let mut cone = Vec::new();
    let mut seen = std::collections::HashSet::new();
    let mut stack = roots.to_vec();
    while let Some(n) = stack.pop() {
        if seen.insert(n) {
            cone.push(n);
            stack.extend(sp.get(n).args.iter().copied());
        }
    }
    cone.sort_unstable();
    let mut map: std::collections::HashMap<NodeId, NodeId> = std::collections::HashMap::new();
    for n in cone {
        let (op, args) = {
            let x = sp.get(n);
            (x.op.clone(), x.args.clone())
        };
        let a: Vec<NodeId> = args.iter().map(|y| map[y]).collect();
        let out = if crate::transpile::graph::fork_of(&op) == Some(d) {
            sp.resolve_fork(&op, a[0], c, Some(tabs))
        } else if a != args {
            sp.fold(op, a)
        } else {
            n
        };
        map.insert(n, out);
    }
    roots.iter().map(|r| map[r]).collect()
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
