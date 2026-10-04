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
    /// The transfer roots (`trace::emit::FrameOutcome::arc`), after `error`
    /// and `live`: computed per body, stored in no row.
    pub(crate) arc: Vec<NodeId>,
}

/// Specialize a traced frame's graph into ONE fused, hash-consed arena.
///
/// Every fork configuration is resolved: `Split`/`SplitValid`/`SplitInt`
/// become `Frag`/`FragOk`/`IntFrag` (ordinary ops), so NO fork node survives and configurations that agree
/// share nodes ("duplicate, then fuse again"). The six buttons are forks
/// like any other (`Symbolic::both_values`): a configuration of theirs is a
/// set of inputs, and it folds them to constants. Returns the fused graph
/// and one body per DISTINCT (outcome, row): `(outcome, splits, roots)`,
/// where `roots` is that outcome's fields in order, then `error`, then
/// `live`, then its extra roots (the transfer roots, `Outcome::arc`), as
/// node ids in the fused graph.
///
/// This is the SINGLE source of the specialized compute: the AVX-512
/// backend assembles it (`trace::emit::asm_fused`) and `lower_outcomes`
/// reads the per-outcome constants off it, so both see byte-for-byte the
/// same thing - there is no parallel specialization.
/// One specialized body: `(outcome, splits, roots)` - `splits` the fragment
/// per fork (`graph::ANY_VALID` for a dead one; 0 for a fork outside the
/// outcome's cone).
pub(crate) type SpecializedBody = (usize, Vec<u8>, Vec<NodeId>);

/// `want`'s images under the configuration `cfg`, specialized into `out`.
fn roots_under(graph: &Graph, cfg: &[u8], need: &[bool], want: &[NodeId], out: &mut Graph) -> Vec<NodeId> {
    let map = graph.specialize_subset_into(cfg, Some(need), out);
    want.iter().map(|r| map[*r as usize]).collect()
}

/// The roots of each configuration of `cfgs` DECIDED (the interval fold with
/// the region's ranges and the map), in one fresh arena so equal results are
/// one node: a test of the player against one world's platform folds only
/// with the ranges, and before that every world is a different node.
fn decided_roots(
    graph: &Graph,
    cfgs: &[Vec<u8>],
    (need, want): (&[bool], &[NodeId]),
    room: Option<&crate::transpile::graph::Room>,
    ranges: &std::collections::HashMap<u32, (i32, i32)>,
) -> Vec<Vec<NodeId>> {
    let mut probe = graph.like();
    let mut decided = graph.like();
    cfgs.iter()
        .map(|c| {
            let sig = roots_under(graph, c, need, want, &mut probe);
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
                if let Op::Split(d) | Op::SplitValid(d) | Op::SplitInt(d) = graph.get(i as NodeId).op {
                    forks.insert(d);
                }
            }
        }
        forks.into_iter().collect()
    };

    // --- 2. per outcome, its configurations ---
    //
    // Two kinds of fork, by what a configuration of it MEANS:
    //
    // * A fork every lane takes both ways (`Symbolic::both_values`: the
    //   buttons, the held trails, the escaped atoms - the outcome reads no
    //   validity of it). A configuration of these is a set of INPUTS, and
    //   two that give the same roots are one: resolved ONE AT A TIME with
    //   the rest standing (`graph::OPEN`), a configuration is kept only
    //   where its roots differ from every one kept before it at that step.
    //   Resolving is substitution, and substitution preserves equality, so
    //   two that agree with the rest standing agree in every completion. This
    //   folds the buttons' 64 assignments to the ones that differ (left and
    //   right together being neither, a jump nothing can take being no jump).
    // * A fork that PARTITIONS the lanes (its validity read). Its
    //   fragments' `live` differ by that validity, so two of them
    //   agree only where the fork is dead; per fork, with every other fork
    //   standing, its dead-ness and its values that agree are decided (per
    //   outcome and per class of the first kind), and the configurations are
    //   the product. A fork whose validity the outcome does not read (a move
    //   whose fragment does not decide `live`) is of the first kind: every
    //   lane takes both of its fragments' rows.
    let mut sp = graph.like();
    let mut cands: Vec<SpecializedBody> = Vec::new();
    for (oi, (fields, error, live, extra)) in outs.iter().enumerate() {
        let mut want: Vec<NodeId> = fields.clone();
        want.push(*error);
        want.push(*live);
        want.extend(extra.iter().copied());
        // An outcome reaches a fraction of the graph, and mapping the
        // whole arena once per configuration is most of the work and none
        // of the answer.
        let need = reachable(graph, &want);
        let bits = bits_of(&need);
        let validity_read: std::collections::BTreeSet<u8> = (0..graph.len())
            .filter(|&n| need[n])
            .filter_map(|n| match graph.get(n as NodeId).op {
                Op::SplitValid(d) => Some(d),
                _ => None,
            })
            .collect();
        let (parts, takes_both): (Vec<u8>, Vec<u8>) = bits.iter().partition(|&&d| validity_read.contains(&d));
        // Every configuration's roots land in ONE arena, so a comparison of
        // roots is a comparison of node ids. The forks outside the cone are
        // unread: 0.
        let mut open = vec![0u8; forks as usize];
        for &d in &bits {
            open[d as usize] = crate::transpile::graph::OPEN;
        }
        // The classes of the forks every lane takes both ways.
        let mut classes: Vec<Vec<u8>> = vec![open.clone()];
        let mut probe = graph.like();
        for &d in &takes_both {
            let mut next = Vec::new();
            let mut seen: std::collections::HashSet<Vec<NodeId>> = Default::default();
            for cfg in classes {
                for v in 0..graph.fork_ways(d) {
                    let mut c = cfg.clone();
                    c[d as usize] = v;
                    if seen.insert(roots_under(graph, &c, &need, &want, &mut probe)) {
                        next.push(c);
                    }
                }
            }
            classes = next;
        }
        let n_classes = classes.len();
        // A DEAD fork: every fragment gives the same fields and error,
        // and the same `live` once the fork's own validity is set aside - it
        // is taken once, validity true (`graph::ANY_VALID`): the OR of its
        // fragments' validities is the lane covered. Room (6,0)'s no-player
        // frames move all ten platforms through a floor fork each and then
        // widen where they stand: 1024 configurations of one row per outcome
        // (2026-09-28). Decided here once, with every other
        // fork standing and compared DECIDED where the body is: dead so, it is
        // dead in every class (resolving is substitution). A fork this misses
        // is checked per class below; one both miss is enumerated, which
        // costs candidates and never a row.
        let dead_everywhere: std::collections::BTreeSet<u8> = parts
            .iter()
            .copied()
            .filter(|&d| graph.fork_ways(d) >= 2)
            .filter(|&d| {
                let any: Vec<Vec<u8>> = (0..graph.fork_ways(d))
                    .map(|v| {
                        let mut c = open.clone();
                        c[d as usize] = v | crate::transpile::graph::ANY_VALID;
                        c
                    })
                    .collect();
                let raw: Vec<Vec<NodeId>> = any.iter().map(|c| roots_under(graph, c, &need, &want, &mut probe)).collect();
                raw.iter().all(|r| *r == raw[0])
                    || (decide && {
                        let dec = decided_roots(graph, &any, (&need, &want), room, ranges);
                        dec.iter().all(|r| *r == dec[0])
                    })
            })
            .collect();
        let mut total_cfgs = 0u64;
        // The most fragments one class enumerates per partitioning fork (for
        // the trace).
        let mut ways_max = vec![0u8; parts.len()];
        for m in classes {
            // A class's own arena: what it builds is its own, and one shared
            // across the classes grew with each.
            let mut probe = graph.like();
            // Move forks keep their traced arity (`Domain::flr_ways`: from
            // the same ranges), which their `SplitOk` premise checks.
            let valid: Vec<u8> = parts.iter().map(|&d| graph.fork_ways(d)).collect();
            for (i, n) in valid.iter().enumerate() {
                ways_max[i] = ways_max[i].max(*n);
            }
            let this = (need.as_slice(), want.as_slice());
            let values: Vec<Vec<u8>> = parts
                .iter()
                .enumerate()
                .map(|(i, &d)| {
                    let with = |v: u8| {
                        let mut c = m.clone();
                        c[d as usize] = v;
                        c
                    };
                    // Dead for the whole outcome (above), or for this class:
                    // compared as built here, since dead-ness only a decided
                    // comparison shows is read once, with every fork standing.
                    if dead_everywhere.contains(&d) {
                        return vec![crate::transpile::graph::ANY_VALID];
                    }
                    if valid[i] >= 2 {
                        let raw: Vec<Vec<NodeId>> = (0..valid[i]).map(|v| roots_under(graph, &with(v | crate::transpile::graph::ANY_VALID), &need, &want, &mut probe)).collect();
                        if raw.iter().all(|r| *r == raw[0]) {
                            return vec![crate::transpile::graph::ANY_VALID];
                        }
                    }
                    if valid[i] <= 2 {
                        return (0..valid[i]).collect();
                    }
                    // PER FORK, ITS VALUES THAT AGREE, compared DECIDED when
                    // the body is: a test of the player against one world's
                    // platform folds only with the region's ranges, and before
                    // that every world is a different node. Built for a
                    // 128-way fork over the platform worlds (since replaced by
                    // deciding per world at compile time, `verify::Points`),
                    // which from one region mostly looked alike.
                    let plain: Vec<Vec<u8>> = (0..valid[i]).map(with).collect();
                    let sigs = if decide {
                        decided_roots(graph, &plain, this, room, ranges)
                    } else {
                        plain.iter().map(|c| roots_under(graph, c, &need, &want, &mut probe)).collect()
                    };
                    (0..valid[i]).filter(|&v| !sigs[..v as usize].contains(&sigs[v as usize])).collect()
                })
                .collect();
            let total: u64 = values.iter().map(|v| v.len() as u64).product();
            total_cfgs += total;
            for k in 0..total {
                let mut cfg = m.clone();
                let mut r = k;
                for (i, d) in parts.iter().enumerate() {
                    let n = values[i].len() as u64;
                    cfg[*d as usize] = values[i][(r % n) as usize];
                    r /= n;
                }
                let roots = roots_under(graph, &cfg, &need, &want, &mut sp);
                cands.push((oi, cfg, roots));
            }
        }
        if trace {
            let ways: Vec<u8> = parts.iter().map(|&d| graph.fork_ways(d)).collect();
            eprintln!(
                "[build]   outcome {oi}: {} forks taken both ways -> {n_classes} classes; partitioning forks {parts:?} traced ways {ways:?}, at most {ways_max:?} in one class -> {total_cfgs} configurations",
                takes_both.len()
            );
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
    // nowhere (its guard decided false) is no body ---
    //
    // Candidates of one outcome with the same fields write the
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
        .map(|o| (o.of.fields.iter().map(|f| f.node).collect(), o.error, o.live, o.arc.clone()))
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
                    // A point in an INTERVAL field is the interval `[v, v]`:
                    // the field's column is an interval column, which keys a
                    // point as one (`asm_kernel::KeyRead::NumAsIval`).
                    Op::Const(lo, hi) if lo == hi && f.ty == "ZI" => Some(AV::Ival(Pico8Num::from_raw(lo), Pico8Num::from_raw(lo))),
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
