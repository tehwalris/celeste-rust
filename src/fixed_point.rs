use std::collections::{BTreeSet, HashMap};
use std::fmt::Debug;
use std::hash::Hash;

use anyhow::Result;
use petgraph::prelude::GraphMap;
use petgraph::visit::IntoEdgeReferences;
use petgraph::Directed;

pub trait Analysis<V: Debug, D: Debug> {
    fn new_empty() -> D;
    fn is_empty(data: &D) -> bool;

    fn is_input(vertex: &V) -> bool;
    fn is_output(vertex: &V) -> bool;
    fn hint_normalize(vertex: &V) -> bool;

    fn join(existing: &mut D, new: &D);
    fn accumulate(accumulated: &D, potentially_new: &D) -> (D, D); // (accumulated, actually_new)

    fn analyze(source_vertex: &V, target_vertex: &V, source_data: &D) -> D; // target_data
}

struct PreparedNode<V> {
    original_node: V,
    pred_edges: Vec<(usize, (V, V))>,
    succ_nodes: Vec<usize>,
    succ_edges: Vec<usize>,
}

pub struct PreparedAnalysis<V, D, A>
where
    V: Debug + Eq + Hash + Copy + Ord,
    D: Debug + Clone,
    A: Analysis<V, D>,
{
    _a: std::marker::PhantomData<A>,
    m_data_acc: Vec<Option<D>>,
    m_data_new: Vec<D>,
    nodes: Vec<PreparedNode<V>>,
    nodes_by_original: HashMap<V, usize>,
    wl: BTreeSet<usize>,
}

pub fn prepare_analysis<V, D, A>(
    analysis: A,
    graph: &GraphMap<V, (), Directed>,
) -> Result<PreparedAnalysis<V, D, A>>
where
    V: Debug + Eq + Hash + Copy + Ord,
    D: Debug + Clone,
    A: Analysis<V, D>,
{
    let nodes_by_original: HashMap<V, usize> = petgraph::algo::toposort(graph, None)
        .map_err(|_| anyhow::anyhow!("toposort failed due to cycle"))?
        .iter()
        .enumerate()
        .map(|(i, v)| (*v, i))
        .collect();
    let edges_by_original: HashMap<(V, V), usize> = graph
        .edge_references()
        .enumerate()
        .map(|(i, (s, t, _))| ((s, t), i))
        .collect();

    let node_count = nodes_by_original.len();
    let edge_count = edges_by_original.len();

    let mut m_data_acc = vec![None; node_count];
    let m_data_new = vec![A::new_empty(); edge_count];

    let mut nodes: Vec<Option<PreparedNode<V>>> =
        (0..nodes_by_original.len()).map(|_| None).collect();
    let mut wl = BTreeSet::new();

    for (&original_node, &node) in nodes_by_original.iter() {
        if A::is_input(&original_node) {
            wl.insert(node);
        }
        if A::is_output(&original_node) || A::hint_normalize(&original_node) {
            m_data_acc[node] = Some(A::new_empty());
        }
        nodes[node] = Some(PreparedNode {
            original_node,
            pred_edges: graph
                .edges_directed(original_node, petgraph::Direction::Incoming)
                .map(|(s, t, _)| (edges_by_original[&(s, t)], (s, t)))
                .collect(),
            succ_nodes: graph
                .neighbors(original_node)
                .map(|n| nodes_by_original[&n])
                .collect(),
            succ_edges: graph
                .edges_directed(original_node, petgraph::Direction::Outgoing)
                .map(|(s, t, _)| edges_by_original[&(s, t)])
                .collect(),
        });
    }

    Ok(PreparedAnalysis {
        _a: std::marker::PhantomData,
        m_data_acc,
        m_data_new,
        nodes: nodes.into_iter().map(|n| n.unwrap()).collect(),
        nodes_by_original,
        wl,
    })
}

impl<V: Debug + Eq + Hash + Copy + Ord, D: Debug + Clone, A: Analysis<V, D>>
    PreparedAnalysis<V, D, A>
{
    pub fn run<I: Fn(&V) -> D>(&self, initialize: I) {
        let mut m_data_acc = self.m_data_acc.clone();
        let mut m_data_new = self.m_data_new.clone();
        let mut wl = self.wl.clone();

        for prepared_node in &self.nodes {
            for &succ_edge in &prepared_node.succ_edges {
                m_data_new[succ_edge] = initialize(&prepared_node.original_node);
            }
        }

        while let Some(wl_node) = wl.pop_first() {
            let prepared_wl_node = &self.nodes[wl_node];

            for &node in &prepared_wl_node.succ_nodes {
                let prepared_node = &self.nodes[node];

                let mut potentially_new = A::new_empty();
                for (pred_edge, (s, t)) in &prepared_node.pred_edges {
                    let potentially_new_part = A::analyze(s, t, &m_data_new[*pred_edge]);
                    m_data_new[*pred_edge] = A::new_empty();
                    A::join(&mut potentially_new, &potentially_new_part);
                }

                let mut process_successors = |data: D| {
                    for &succ_edge in &prepared_node.succ_edges {
                        A::join(&mut m_data_new[succ_edge], &data);
                    }
                    for &succ_node in &prepared_wl_node.succ_nodes {
                        wl.insert(succ_node);
                    }
                };

                if let Some(data_acc) = m_data_acc[wl_node].as_mut() {
                    let (data_acc_new, actually_new) = A::accumulate(&data_acc, &potentially_new);
                    if !A::is_empty(&actually_new) {
                        *data_acc = data_acc_new;
                        process_successors(actually_new);
                    }
                } else {
                    if !A::is_empty(&potentially_new) {
                        process_successors(potentially_new);
                    }
                }
            }
        }
    }
}
