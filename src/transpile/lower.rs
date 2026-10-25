//! Graph -> the fused, fork-free frame: `specialize_frame` resolves every
//! fork configuration of the traced graph into ONE hash-consed arena, and
//! `konst_fields` reads the per-outcome constants the kernel needs off the
//! result. The kernel registry (`compiled::asm_kernel`) assembles it.

use std::collections::BTreeMap;

use super::garith::GraphArith;
use super::graph::{Graph, NodeId, Op};
use super::kernel::{Body, OutField, Roots};

/// `want`'s images under the configuration `cfg`, specialized into `out`.
fn roots_under<A: GraphArith>(graph: &Graph<A>, cfg: &[u8], need: &[bool], want: &[NodeId], out: &mut Graph<A>) -> Vec<NodeId> {
    let map = graph.specialize_subset_into(cfg, Some(need), out);
    want.iter().map(|r| map[*r as usize]).collect()
}

/// The roots of each configuration of `cfgs` DECIDED (the interval fold with
/// the restrictions' ranges and the map), in one fresh arena so equal results
/// are one node.
fn decided_roots<A: GraphArith>(
    graph: &Graph<A>,
    cfgs: &[Vec<u8>],
    (need, want): (&[bool], &[NodeId]),
    room: Option<&crate::transpile::graph::Room>,
) -> Vec<Vec<NodeId>> {
    let mut probe = graph.like();
    let mut decided = graph.like();
    cfgs.iter()
        .map(|c| {
            let sig = roots_under(graph, c, need, want, &mut probe);
            let (dm, _) = crate::transpile::ival::fold_into(&probe, &sig, room, &mut decided).expect("interval fold");
            sig.iter().map(|r| dm[*r as usize]).collect()
        })
        .collect()
}

/// Specialize a traced frame's graph into ONE fused, hash-consed arena:
/// every fork configuration resolved (`Fork`/`ForkValid` become
/// `Frag`/`FragOk`), so no fork node survives and configurations that agree
/// share nodes, the boolean layer decided, and candidates that write the
/// same row fused. Returns the fused graph and one body per distinct
/// (outcome, row).
pub(crate) fn specialize_frame<A: GraphArith>(
    graph: &Graph<A>,
    outs: &[Roots],
    forks: u8,
    room: Option<&crate::transpile::graph::Room>,
) -> (Graph<A>, Vec<Body>) {
    let mut sp = graph.like();
    let mut cands = Vec::new();
    for (oi, out) in outs.iter().enumerate() {
        cands.extend(configurations(graph, oi, out, forks, room, &mut sp));
    }
    let sp = decide(&sp, &mut cands, room);
    fuse(sp, cands)
}

/// THE CONFIGURATIONS of outcome `oi`, each specialized into `sp` as a
/// candidate body. Two kinds of fork, by what a configuration of it MEANS:
/// * A fork every lane takes both ways (buttons, held trails, escaped atoms;
///   the outcome reads no validity of it): a configuration of these is a set
///   of INPUTS. They are resolved one at a time with the rest standing
///   (`graph::OPEN`), keeping a configuration only where its roots differ
///   from every one kept before. Sound because substitution preserves
///   equality: two that agree with the rest standing agree in every
///   completion.
/// * A fork that PARTITIONS the lanes (its validity is read). Per fork, with
///   every other fork standing, its dead-ness and its agreeing values are
///   decided (per outcome and per class of the first kind); the
///   configurations are the product.
fn configurations<A: GraphArith>(
    graph: &Graph<A>,
    oi: usize,
    out: &Roots,
    forks: u8,
    room: Option<&crate::transpile::graph::Room>,
    sp: &mut Graph<A>,
) -> Vec<Body> {
    let mut cands = Vec::new();
    let want = out.flat();
    let need = graph.reachable(&want);
    // The forks the outcome depends on: those in its cone (read off the
    // reachable set, so no limit on a frame's fork count).
    let bits: Vec<u8> = (0..graph.len())
        .filter(|&n| need[n])
        .filter_map(|n| match graph.get(n as NodeId).op {
            Op::Fork(_, d) | Op::ForkValid(_, d) => Some(d),
            _ => None,
        })
        .collect::<std::collections::BTreeSet<u8>>()
        .into_iter()
        .collect();
    let validity_read: std::collections::BTreeSet<u8> = (0..graph.len())
        .filter(|&n| need[n])
        .filter_map(|n| match graph.get(n as NodeId).op {
            Op::ForkValid(_, d) => Some(d),
            _ => None,
        })
        .collect();
    let (parts, takes_both): (Vec<u8>, Vec<u8>) = bits.iter().partition(|&&d| validity_read.contains(&d));
    // Every configuration's roots land in ONE arena, so comparing roots is
    // comparing node ids. Forks outside the cone are unread: 0.
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
    // A DEAD fork (every fragment gives the same fields and error, and the
    // same `live` once its own validity is set aside) is taken once, validity
    // true (`graph::ANY_VALID`): the OR of its fragments' validities is the lane
    // covered. Decided here once, compared decided: dead so, it is dead in every
    // class. A fork this misses is checked per class below; one both miss is
    // enumerated, which costs candidates and never a row.
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
            raw.iter().all(|r| *r == raw[0]) || {
                let dec = decided_roots(graph, &any, (&need, &want), room);
                dec.iter().all(|r| *r == dec[0])
            }
        })
        .collect();
    for m in classes {
        // A class's own arena (one shared across classes grew with each).
        let mut probe = graph.like();
        // Move forks keep their traced arity, which their `ForkOk` premise checks.
        let valid: Vec<u8> = parts.iter().map(|&d| graph.fork_ways(d)).collect();
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
                // Dead for the whole outcome (above), or for this class.
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
                // Per fork, its values that agree, compared decided as the body is.
                let plain: Vec<Vec<u8>> = (0..valid[i]).map(with).collect();
                let sigs = decided_roots(graph, &plain, this, room);
                (0..valid[i]).filter(|&v| !sigs[..v as usize].contains(&sigs[v as usize])).collect()
            })
            .collect();
        let total: u64 = values.iter().map(|v| v.len() as u64).product();
        for k in 0..total {
            let mut cfg = m.clone();
            let mut r = k;
            for (i, d) in parts.iter().enumerate() {
                let n = values[i].len() as u64;
                cfg[*d as usize] = values[i][(r % n) as usize];
                r /= n;
            }
            let map = graph.specialize_subset_into(&cfg, Some(&need), sp);
            cands.push(Body { outcome: oi, splits: cfg, roots: out.map(|n| map[n as usize]) });
        }
    }
    cands
}

/// DECIDE the boolean layer of the candidates, on the RESOLVED graph: the
/// interval fold, the local BDD (`bdd::simplify_local`), the interval fold
/// again; the candidates' roots follow into the result.
fn decide<A: GraphArith>(sp: &Graph<A>, cands: &mut [Body], room: Option<&crate::transpile::graph::Room>) -> Graph<A> {
    let all: Vec<NodeId> = cands.iter().flat_map(|c| c.roots.flat()).collect();
    let (g1, m1, _) = crate::transpile::ival::fold(sp, &all, room).expect("interval fold");
    let r1: Vec<NodeId> = all.iter().map(|x| m1[*x as usize]).collect();
    // The boolean layer, simplified locally (`bdd::simplify_local`).
    let (g2, m2, _) = crate::transpile::bdd::simplify_local(&g1, &r1, crate::transpile::bdd::LOCAL_CAP, crate::transpile::bdd::LOCAL_EXPAND);
    let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
    let (g3, m3, _) = crate::transpile::ival::fold(&g2, &r2, room).expect("interval fold 2");
    for c in cands.iter_mut() {
        c.roots = c.roots.map(|r| m3[m2[m1[r as usize] as usize] as usize]);
    }
    g3
}

/// FUSE: candidates that write the same row are ONE body; a body live
/// nowhere is no body. Candidates of one outcome with the same fields fuse:
/// `live` the OR of theirs, `error` the members' shared node when they share
/// one, else `OR_i (live_i & error_i)`. Either way a lane declines exactly
/// where some candidate declined (also in the three-valued masks, both read
/// where they MAY hold) and is kept where some candidate kept it.
fn fuse<A: GraphArith>(mut sp: Graph<A>, cands: Vec<Body>) -> (Graph<A>, Vec<Body>) {
    let bodies: Vec<Body> = {
        let mut groups: Vec<Vec<Body>> = Vec::new();
        let mut index: BTreeMap<(usize, Vec<NodeId>), usize> = BTreeMap::new();
        for c in cands {
            if matches!(sp.get(c.roots.live).op, Op::ConstBool(false)) {
                continue;
            }
            let key = (c.outcome, c.roots.row());
            match index.get(&key) {
                Some(&gi) => groups[gi].push(c),
                None => {
                    index.insert(key, groups.len());
                    groups.push(vec![c]);
                }
            }
        }
        groups
            .into_iter()
            .map(|members| {
                let shared = members.iter().all(|m| m.roots.error == members[0].roots.error);
                let fused_error = if shared {
                    members[0].roots.error
                } else {
                    members.iter().fold(sp.leaf(Op::ConstBool(false)), |e, m| {
                        let here = sp.fold(Op::And, vec![m.roots.live, m.roots.error]);
                        sp.fold(Op::Or, vec![e, here])
                    })
                };
                let fused_live = members.iter().skip(1).fold(members[0].roots.live, |l, m| sp.fold(Op::Or, vec![l, m.roots.live]));
                let mut b = members.into_iter().next().expect("a group has a member");
                b.roots.error = fused_error;
                b.roots.live = fused_live;
                b
            })
            .collect()
    };

    (sp, bodies)
}

/// Outcome `oi`'s field constants: a field every body of the outcome
/// computes as one compile-time constant node holds that `AV` in EVERY row
/// (`OutField::uniform`; a widened field keeps its unknown number).
pub(crate) fn konst_fields<A: GraphArith>(fused: &Graph<A>, bodies: &[Body], oi: usize, fields: &mut [OutField<A>]) {
    use celeste_engine::runtime2::AV;
    let mine: Vec<&Vec<NodeId>> = bodies.iter().filter(|b| b.outcome == oi).map(|b| &b.roots.fields).collect();
    let Some(first) = mine.first() else {
        return;
    };
    for (fi, f) in fields.iter_mut().enumerate() {
        let node = first[fi];
        // The arena is hash-consed: same node is same value.
        if f.uniform.is_some() || !mine.iter().all(|r| r[fi] == node) {
            continue;
        }
        f.uniform = match fused.get(node).op {
            // A point in an INTERVAL field is the interval `[v, v]`: an interval
            // column keys a point as one (`asm_kernel::emit::KeyField`).
            Op::Const(lo, hi) if lo == hi && f.ty == super::graph::Ty::Ival => Some(AV::Ival(A::from_lit(lo), A::from_lit(lo))),
            Op::Const(lo, hi) if lo == hi => Some(AV::Num(A::from_lit(lo))),
            Op::Const(lo, hi) => Some(AV::Ival(A::from_lit(lo), A::from_lit(hi))),
            Op::ConstBool(b) => Some(AV::Bool(b)),
            _ => None,
        };
    }
}
