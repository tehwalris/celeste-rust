//! WHY a graph computes what it does, for diagnostics: a kernel's decline
//! (`check::explain`), the binder's and the codegen's refusals. A node is
//! rendered with its value where one evaluation is at hand (else its type);
//! around that, the moves they all make: the disjuncts of an `Or`-tree, a
//! chain followed down the graph, a subtree, the path from a root to a node.

use std::collections::HashSet;

use super::garith::GraphArith;
use super::graph::{Graph, NodeId, Op, Val};

/// A graph, and its nodes' values from one evaluation where there is one.
pub(crate) struct Explain<'a, A: GraphArith> {
    pub(crate) g: &'a Graph<A>,
    pub(crate) vals: Option<&'a [Val<A>]>,
}

impl<'a, A: GraphArith> Explain<'a, A> {
    /// The bare graph: nodes rendered with their types.
    pub(crate) fn of(g: &'a Graph<A>) -> Self {
        Explain { g, vals: None }
    }

    /// Node `n` as `n=Op<value>` (`n=Op/type` without values).
    pub(crate) fn node(&self, n: NodeId) -> String {
        let op = &self.g.get(n).op;
        match self.vals {
            Some(v) => format!("{n}={op:?}<{:?}>", v[n as usize]),
            None => format!("{n}={op:?}/{:?}", self.g.ty(n)),
        }
    }

    /// `n` and its operands: `n=Op<value>; args a=..., b=...`.
    pub(crate) fn with_args(&self, n: NodeId) -> String {
        let args: Vec<String> = self.g.get(n).args.iter().map(|&a| self.node(a)).collect();
        format!("{}; args {}", self.node(n), args.join(", "))
    }

    /// `n`'s subtree to `depth`, each node as `node`.
    pub(crate) fn tree(&self, n: NodeId, depth: usize) -> String {
        let args = &self.g.get(n).args;
        if depth == 0 || args.is_empty() {
            return self.node(n);
        }
        let kids: Vec<String> = args.iter().map(|&a| self.tree(a, depth - 1)).collect();
        format!("{}({})", self.node(n), kids.join(", "))
    }

    /// The disjuncts of `root`'s `Or`-tree, each once, depth first.
    pub(crate) fn disjuncts(&self, root: NodeId) -> Vec<NodeId> {
        let (mut stack, mut seen, mut out) = (vec![root], HashSet::new(), Vec::new());
        while let Some(n) = stack.pop() {
            if !seen.insert(n) {
                continue;
            }
            let node = self.g.get(n);
            if node.op == Op::Or {
                stack.extend(node.args.iter().copied());
            } else {
                out.push(n);
            }
        }
        out
    }

    /// From `from`, `step` taken while it gives a node, at most `limit`
    /// steps: every node visited, `from` first.
    pub(crate) fn chain(&self, from: NodeId, limit: usize, mut step: impl FnMut(NodeId) -> Option<NodeId>) -> Vec<NodeId> {
        let mut out = vec![from];
        while out.len() <= limit {
            match step(out[out.len() - 1]) {
                Some(x) => out.push(x),
                None => break,
            }
        }
        out
    }

    /// The nodes of `chain` as text, `<-` between.
    pub(crate) fn show_chain(&self, chain: &[NodeId]) -> String {
        chain.iter().map(|&n| self.node(n)).collect::<Vec<_>>().join(" <- ")
    }

    /// How the first of `roots` that reaches `target` does so:
    /// `root #k: op -> op -> ...`.
    pub(crate) fn path(&self, roots: &[NodeId], target: NodeId) -> String {
        for (k, r) in roots.iter().enumerate() {
            let mut parent: std::collections::HashMap<NodeId, NodeId> = Default::default();
            let mut seen: HashSet<NodeId> = HashSet::from([*r]);
            let mut stack = vec![*r];
            while let Some(n) = stack.pop() {
                if n == target {
                    let mut chain = vec![target];
                    while let Some(p) = parent.get(&chain[chain.len() - 1]) {
                        chain.push(*p);
                    }
                    let ops: Vec<String> = chain.iter().rev().map(|n| format!("{:?}", self.g.get(*n).op)).collect();
                    return format!("root #{k}: {}", ops.join(" -> "));
                }
                for a in self.g.get(n).args.iter().copied() {
                    if seen.insert(a) {
                        parent.insert(a, n);
                        stack.push(a);
                    }
                }
            }
        }
        "no root".to_string()
    }
}
