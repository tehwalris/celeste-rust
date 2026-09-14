//! GUARDED REGIONS of a fused kernel (2026-09-14).
//!
//! `lower::specialize_frame` resolves every fork configuration into one
//! hash-consed graph, and the straight-line kernel evaluates ALL of it for
//! every slice - in room (2,0) only 15% of the (body, slice) pairs took a
//! lane, and a shape's nodes are 80% specific to one of its 4-16 fork
//! configurations. So the graph is partitioned into REGIONS: a region is a
//! partial assignment of forks (a set of `(fork, resolution)` pairs), a
//! node belongs to the intersection of the assignments of everything that
//! consumes it (the roots of a body carry the body's own configuration),
//! and the emitter tests, before a region's instructions, whether any
//! lane of the slice could take a body consistent with it - skipping the
//! block otherwise.
//!
//! The guard of a region is `Or` over the configurations consistent with
//! it of `Or` over their bodies of `And` over the body's fork premises of
//! `premise or not Known(premise)`: a lane whose premise is still
//! UNDECIDED keeps the region alive, so the body runs, declines the lane
//! and the coverage gap surfaces exactly as before. Guards are ordinary
//! graph nodes (interned, folded), evaluated in the root region and
//! stored as extra roots; the run loop masks each body's `live` by its
//! region's mask, so a skipped body's stale outputs are never read.
//!
//! Regions nest by set inclusion (a more specific assignment implies the
//! less specific one's guard), and every node's arguments lie in less
//! specific regions than its own, so emitting regions in order of
//! specificity keeps every value defined before the block that reads it -
//! whenever that block runs.

use super::super::graph::{Graph, NodeId, Op};
use std::collections::{BTreeMap, BTreeSet};

/// A partial fork assignment, sorted by fork.
pub type Assignment = Vec<(u8, u8)>;

pub struct Regions {
    /// Per region (0 = root, the empty assignment): its assignment.
    pub keys: Vec<Assignment>,
    /// Per fused node: its region (`usize::MAX` if not live).
    pub of_node: Vec<usize>,
    /// Per body: its region.
    pub of_body: Vec<usize>,
    /// Per region: its guard node (`None` for the root).
    pub guard: Vec<Option<NodeId>>,
    /// Regions in emission order: root first, then by specificity.
    pub order: Vec<usize>,
}

fn intersect(a: &Assignment, b: &Assignment) -> Assignment {
    a.iter().filter(|p| b.contains(p)).copied().collect()
}

impl Regions {
    /// `body_roots[b]` are the roots of body `b` (its fields, ok, live and
    /// its key chains); `premises[b]` its fork premises; `config[b]` its
    /// assignment. Adds the guard nodes to `g`.
    pub fn build(
        g: &mut Graph,
        body_roots: &[Vec<NodeId>],
        premises: &[Vec<(u8, NodeId)>],
        config: &[Assignment],
    ) -> Regions {
        let n = g.len();
        // --- node assignments: intersection over consumers, roots from
        // their bodies (ids refer downward, so a descending pass sees every
        // consumer before its operands) ---
        let mut asg: Vec<Option<Assignment>> = vec![None; n];
        for (b, roots) in body_roots.iter().enumerate() {
            for &r in roots {
                asg[r as usize] = Some(match asg[r as usize].take() {
                    Some(a) => intersect(&a, &config[b]),
                    None => config[b].clone(),
                });
            }
        }
        for id in (0..n).rev() {
            let Some(a) = asg[id].clone() else { continue };
            for &arg in &g.get(id as NodeId).args {
                let e = &mut asg[arg as usize];
                *e = Some(match e.take() {
                    Some(x) => intersect(&x, &a),
                    None => a.clone(),
                });
            }
        }
        // --- intern the regions ---
        let mut ids: BTreeMap<Assignment, usize> = BTreeMap::new();
        ids.insert(Vec::new(), 0);
        let mut keys: Vec<Assignment> = vec![Vec::new()];
        let intern = |a: &Assignment, ids: &mut BTreeMap<Assignment, usize>, keys: &mut Vec<Assignment>| -> usize {
            if let Some(&i) = ids.get(a) {
                return i;
            }
            let i = keys.len();
            keys.push(a.clone());
            ids.insert(a.clone(), i);
            i
        };
        let mut of_node = vec![usize::MAX; n];
        for id in 0..n {
            if let Some(a) = &asg[id] {
                of_node[id] = intern(a, &mut ids, &mut keys);
            }
        }
        let of_body: Vec<usize> = config.iter().map(|c| intern(c, &mut ids, &mut keys)).collect();
        // --- guards: present[b] = And over premises of (p or not Known(p));
        // per configuration the Or over its bodies; per region the Or over
        // the configurations consistent with it ---
        let mut present_config: BTreeMap<Assignment, NodeId> = BTreeMap::new();
        for (b, ps) in premises.iter().enumerate() {
            let mut acc: Option<NodeId> = None;
            for &(_, p) in ps {
                let known = g.fold(Op::Known, vec![p]);
                let nk = g.fold(Op::Not, vec![known]);
                let term = g.fold(Op::Or, vec![p, nk]);
                acc = Some(match acc {
                    Some(x) => g.fold(Op::And, vec![x, term]),
                    None => term,
                });
            }
            let present = acc.unwrap_or_else(|| g.leaf(Op::ConstBool(true)));
            let e = present_config.entry(config[b].clone()).or_insert(present);
            if *e != present {
                *e = g.fold(Op::Or, vec![*e, present]);
            }
        }
        let n_regions = keys.len();
        let mut guard: Vec<Option<NodeId>> = vec![None; n_regions];
        for r in 1..n_regions {
            let mut acc: Option<NodeId> = None;
            for (c, &pc) in &present_config {
                if keys[r].iter().all(|p| c.contains(p)) {
                    acc = Some(match acc {
                        Some(x) => g.fold(Op::Or, vec![x, pc]),
                        None => pc,
                    });
                }
            }
            guard[r] = Some(acc.expect("a region has at least one consistent configuration"));
        }
        // The guards' cones are evaluated unconditionally: root region.
        let mut of_node = {
            let mut v = of_node;
            v.resize(g.len(), usize::MAX);
            v
        };
        let mut stack: Vec<NodeId> = guard.iter().flatten().copied().collect();
        let mut seen = vec![false; g.len()];
        while let Some(x) = stack.pop() {
            if seen[x as usize] {
                continue;
            }
            seen[x as usize] = true;
            of_node[x as usize] = 0;
            stack.extend(g.get(x).args.iter().copied());
        }
        let mut order: Vec<usize> = (0..n_regions).collect();
        order.sort_by_key(|&r| (keys[r].len(), keys[r].clone()));
        Regions { keys, of_node, of_body, guard, order }
    }

    /// A one-line census: regions, and the share of live nodes in the root
    /// (unskippable) region.
    pub fn census(&self, g: &Graph, live: &[bool]) -> String {
        let n_live = live.iter().filter(|&&x| x).count();
        let root = (0..g.len()).filter(|&i| live[i] && self.of_node[i] == 0).count();
        let mut per_size: BTreeMap<usize, (usize, usize)> = BTreeMap::new();
        for r in 0..self.keys.len() {
            let nodes = (0..g.len()).filter(|&i| live[i] && self.of_node[i] == r).count();
            let e = per_size.entry(self.keys[r].len()).or_default();
            e.0 += 1;
            e.1 += nodes;
        }
        let _ = BTreeSet::<usize>::new();
        format!(
            "{} regions, {} live nodes, root {} ({:.0}%); per assignment size (regions, nodes): {:?}",
            self.keys.len(),
            n_live,
            root,
            100.0 * root as f64 / n_live.max(1) as f64,
            per_size
        )
    }
}
