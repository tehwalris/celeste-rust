//! INTERVAL folding: decide what the graph computes without knowing any
//! of its inputs.
//!
//! `transpile::bdd` decides the BOOLEAN layer, and it is blind by
//! construction to why a comparison is constant: every comparison is an
//! independent variable, so `0 > abs(x)` is a free variable that could
//! go either way. It cannot: `abs` is non-negative, so that node is
//! false for every input there has ever been.
//!
//! `Graph::eval` already knows this - it evaluates over
//! `Pico8NumInterval` and models `Abs` with its sign case split - and
//! until now it existed only in tests. This pass is the missing
//! consumer: run the interval evaluator with every input at TOP, and
//! every node it pins down is a constant, unconditionally.
//!
//! **Why this is exact rather than merely sound.** A node the evaluator
//! reports as `Bool(Some(b))` under TOP inputs is `b` under every
//! assignment those inputs admit, because the interval domain is an
//! over-approximation: the concrete value is always in the abstract one.
//! Replacing it with the constant therefore changes nothing the graph
//! computes. It can only IMPROVE the abstract precision downstream,
//! which is the property that makes it safe where a general
//! equality-substitution is not (see `bdd`'s "proved and deliberately
//! not acted on").
//!
//! The measurement that motivated it: after the BDD had run on a traced
//! frame of room (0,0), 1,141 of 2,916 surviving nodes still evaluated
//! to the same value at all 193 probe points, and the two biggest such
//! buckets were led by `Gt(Const(0), Abs(..))` and `Le(Const(0),
//! Abs(..))`.

use std::collections::{HashMap, HashSet};

use anyhow::Result;

use crate::pico8_num::{Pico8Num, Pico8NumInterval};

use super::graph::{Graph, NodeId, Op, Val};

/// The sentinel `simplify` uses for "this node was not reachable".
pub use super::bdd::UNREACHABLE;

#[derive(Debug, Default, Clone, Copy)]
pub struct Stats {
    pub before: usize,
    pub after: usize,
    /// Booleans decided with no knowledge of the inputs.
    pub bools: usize,
    /// Numbers pinned to a single point.
    pub nums: usize,
}

/// Which cells hold BOOLEANS, by where they are used.
///
/// The evaluator needs a kind per input to give it a top, and the graph
/// does not carry one: `Op::Cell(i)` is just a leaf. Use position - a
/// cell under `Not`, under either side of `And`/`Or`, or in a `Sel`'s
/// condition is a boolean - and default to number. A cell used in both
/// positions would be a boundary bug, and shows up here as an evaluator
/// type error rather than as a wrong answer.
fn bool_cells(g: &Graph) -> HashSet<u32> {
    let mut out = HashSet::new();
    let mark = |g: &Graph, id: NodeId, out: &mut HashSet<u32>| {
        if let Op::Cell(c) = g.get(id).op {
            out.insert(c);
        }
    };
    for id in 0..g.len() {
        let node = g.get(id as NodeId);
        match node.op {
            Op::Not | Op::Known => mark(g, node.args[0], &mut out),
            Op::And | Op::Or => {
                mark(g, node.args[0], &mut out);
                mark(g, node.args[1], &mut out);
            }
            Op::Sel => mark(g, node.args[0], &mut out),
            _ => {}
        }
    }
    out
}

/// Every node the interval evaluator can pin down, replaced by that
/// constant, and the graph rebuilt over the nodes `roots` reach.
///
/// Returns the new graph, a FULL-length node map (`UNREACHABLE` where a
/// node was not reached), and what it found.
pub fn fold(g: &Graph, roots: &[NodeId]) -> Result<(Graph, Vec<NodeId>, Stats)> {
    let need = super::bdd::reachable(g, roots);
    let bools = bool_cells(g);
    let full = Val::Num(Pico8NumInterval::new(
        Pico8Num::from_raw(i32::MIN),
        Pico8Num::from_raw(i32::MAX),
    ));
    let mut cells: HashMap<u32, Val> = HashMap::new();
    for id in 0..g.len() {
        if let Op::Cell(c) = g.get(id as NodeId).op {
            cells.insert(c, if bools.contains(&c) { Val::Bool(None) } else { full });
        }
    }
    let vals = g.eval_lenient(&cells)?;

    let mut out = Graph::new();
    let mut map: Vec<NodeId> = vec![UNREACHABLE; g.len()];
    let mut st = Stats {
        before: need.iter().filter(|x| **x).count(),
        ..Default::default()
    };
    for id in 0..g.len() {
        if !need[id] {
            continue;
        }
        let node = g.get(id as NodeId);
        let new = match vals[id] {
            Val::Bool(Some(b)) if !matches!(node.op, Op::ConstBool(_)) => {
                st.bools += 1;
                out.leaf(Op::ConstBool(b))
            }
            Val::Num(i) if i.to_number().is_some() && !matches!(node.op, Op::Const(..)) => {
                st.nums += 1;
                let raw = i.to_number().unwrap().as_raw_u32() as i32;
                out.leaf(Op::Const(raw, raw))
            }
            _ => {
                let args: Vec<NodeId> = node.args.iter().map(|x| map[*x as usize]).collect();
                out.fold(node.op.clone(), args)
            }
        };
        map[id] = new;
    }
    st.after = out.len();
    Ok((out, map, st))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The finding that motivated the pass, as a test: `abs` is
    /// non-negative, so a comparison of it against zero is not a free
    /// variable however opaque its operand is.
    #[test]
    fn a_comparison_of_abs_against_zero_is_not_a_free_variable() {
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(0));
        let y = g.leaf(Op::Cell(1));
        let sum = g.fold(Op::Add, vec![x, y]);
        let a = g.fold(Op::Abs, vec![sum]);
        let zero = g.leaf(Op::Const(0, 0));
        let gt = g.fold(Op::Gt, vec![zero, a]);
        let le = g.fold(Op::Le, vec![zero, a]);
        let (out, map, st) = fold(&g, &[gt, le]).expect("folds");
        assert_eq!(out.get(map[gt as usize]).op, Op::ConstBool(false), "0 > abs(x)");
        assert_eq!(out.get(map[le as usize]).op, Op::ConstBool(true), "0 <= abs(x)");
        assert_eq!(st.bools, 2);
    }

    /// A node it CANNOT model must not be decided, and must not poison
    /// the nodes that do not depend on it.
    #[test]
    fn an_unmodelled_op_becomes_top_rather_than_an_error() {
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(0));
        let w = g.leaf(Op::Const(65536, 65536));
        let tile = g.fold(Op::TileFlagAt, vec![x, x, w, w, w]);
        let a = g.fold(Op::Abs, vec![x]);
        let zero = g.leaf(Op::Const(0, 0));
        let le = g.fold(Op::Le, vec![zero, a]);
        let both = g.fold(Op::And, vec![tile, le]);
        let (out, map, _) = fold(&g, &[both]).expect("folds");
        // The tile lookup stays; the arithmetic fact next to it does not.
        assert_eq!(out.get(map[le as usize]).op, Op::ConstBool(true));
        assert_eq!(out.get(map[both as usize]).op, Op::TileFlagAt);
    }

    /// Folding must not change what the graph computes. Sampled, because
    /// the property is universal quantification over inputs and the
    /// point of the pass is that it holds at all of them.
    #[test]
    fn folding_does_not_change_what_the_graph_computes() {
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(0));
        let y = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let mut roots = Vec::new();
        let zero = g.leaf(Op::Const(0, 0));
        let four = g.leaf(Op::Const(4 * 65536, 4 * 65536));
        let sum = g.fold(Op::Add, vec![x, y]);
        let ab = g.fold(Op::Abs, vec![sum]);
        let fl = g.fold(Op::Flr, vec![ab]);
        roots.push(g.fold(Op::Ge, vec![fl, zero]));
        roots.push(g.fold(Op::Lt, vec![ab, four]));
        roots.push(g.fold(Op::Sel, vec![b, ab, fl]));
        let ff = g.leaf(Op::ConstBool(false));
        roots.push(g.fold(Op::Or, vec![b, ff]));
        let (out, map, _) = fold(&g, &roots).expect("folds");
        for i in -3i16..4 {
            for j in -3i16..4 {
                for bv in [false, true] {
                    let mut cells: HashMap<u32, Val> = HashMap::new();
                    cells.insert(0, Val::exact_num(Pico8Num::from_i16(i)));
                    cells.insert(1, Val::exact_num(Pico8Num::from_i16(j)));
                    cells.insert(2, Val::Bool(Some(bv)));
                    let before = g.eval_lenient(&cells).expect("before");
                    let after = out.eval_lenient(&cells).expect("after");
                    for r in &roots {
                        assert_eq!(
                            before[*r as usize],
                            after[map[*r as usize] as usize],
                            "root {} at ({}, {}, {})",
                            r,
                            i,
                            j,
                            bv
                        );
                    }
                }
            }
        }
    }
}
