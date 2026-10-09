//! INTERVAL folding: decide what the graph computes without knowing its
//! inputs.
//!
//! `transpile::bdd` treats every comparison as an independent variable, so
//! it cannot see that `0 > abs(x)` is always false. This pass runs the
//! interval evaluator (`Graph::eval`) with every input at TOP, and every node
//! it pins down is a constant, unconditionally.
//!
//! **Exact, not merely sound.** The interval domain over-approximates, so a
//! node evaluated to `Bool(Some(b))` under TOP inputs is `b` under every
//! assignment - every assignment the body is defined on: an
//! `Op::Restrict` narrows its operand, and a lane outside its range is in
//! error (the restriction's own, charged to every outcome). Replacing it changes nothing the graph computes and can only
//! improve precision downstream (unlike a general equality substitution,
//! see `bdd`'s "proved and deliberately not acted on").

use std::collections::HashMap;

use anyhow::Result;

use crate::pico8_num::{Pico8Num, Pico8NumInterval};

use super::graph::{Graph, NodeId, Op, Room, Val};

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

/// Every node the interval evaluator can pin down, replaced by that
/// constant, and the graph rebuilt over the nodes `roots` reach.
///
/// Returns the new graph, a FULL-length node map (`UNREACHABLE` where a
/// node was not reached), and what it found.
pub fn fold(g: &Graph, roots: &[NodeId], room: Option<&Room>) -> Result<(Graph, Vec<NodeId>, Stats)> {
    let mut out = g.like();
    let (map, st) = fold_into(g, roots, room, &mut out)?;
    Ok((out, map, st))
}

/// Every input cell at its weakest value - a bool cell unknown, a number
/// cell the full range or, when `ranges` bounds it, that range.
/// The kind is the graph's (`Graph::cell_kind`, recorded at `symbolize`).
pub(crate) fn seed_cells(g: &Graph, ranges: &HashMap<u32, (i32, i32)>) -> HashMap<u32, Val> {
    let full = Val::Num(Pico8NumInterval::new(
        Pico8Num::from_raw(i32::MIN),
        Pico8Num::from_raw(i32::MAX),
    ));
    let mut cells: HashMap<u32, Val> = HashMap::new();
    for id in 0..g.len() {
        if let Op::Cell(c) = g.get(id as NodeId).op {
            let v = if g.cell_kind(c) == super::graph::CellKind::Bool {
                Val::Bool(None)
            } else if let Some((lo, hi)) = ranges.get(&c) {
                Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(*lo), Pico8Num::from_raw(*hi)))
            } else {
                full
            };
            cells.insert(c, v);
        }
    }
    cells
}

/// `fold`, writing into `out`: what several graphs fold to lands in one
/// hash-consed arena, so equal results are the same node. A bounded input's
/// range comes from its `Op::Restrict`, never from a seed: a fact assumed
/// here would be checked nowhere.
pub fn fold_into(
    g: &Graph,
    roots: &[NodeId],
    room: Option<&Room>,
    out: &mut Graph,
) -> Result<(Vec<NodeId>, Stats)> {
    let need = super::bdd::reachable(g, roots);
    let cells = seed_cells(g, &HashMap::new());
    let vals = match room {
        Some(r) => g.eval_lenient_in(&cells, r)?,
        None => g.eval_lenient(&cells)?,
    };
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
    Ok((map, st))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `abs` is non-negative, so a comparison of it against zero is not a free
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
        let (out, map, st) = fold(&g, &[gt, le], None).expect("folds");
        assert_eq!(out.get(map[gt as usize]).op, Op::ConstBool(false), "0 > abs(x)");
        assert_eq!(out.get(map[le as usize]).op, Op::ConstBool(true), "0 <= abs(x)");
        assert_eq!(st.bools, 2);
    }

    /// The map is a constant, so a collision test at a known position is
    /// decidable; at an unknown position it genuinely is not (the player could
    /// be anywhere). Deciding those needs bounds on the position inputs.
    #[test]
    fn a_collision_test_is_decided_at_a_known_position_and_not_at_an_unknown_one() {
        let cart = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = crate::game_runner::start_room();
        let cache = std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cart, rx, ry).expect("cache"),
        );
        let room = Room { cart: cart.clone(), cache: cache.clone() };

        let at = |x: i16, y: i16| -> Option<bool> {
            let mut g = Graph::new();
            let n = |g: &mut Graph, v: i16| {
                let r = Pico8Num::from_i16(v).as_raw_u32() as i32;
                g.leaf(Op::Const(r, r))
            };
            let (px, py) = (n(&mut g, x), n(&mut g, y));
            let (w, h) = (n(&mut g, 8), n(&mut g, 8));
            let f = n(&mut g, 0);
            let t = g.fold(Op::TileFlagAt, vec![px, py, w, h, f]);
            let (out, map, _) = fold(&g, &[t], Some(&room)).expect("folds");
            match out.get(map[t as usize]).op {
                Op::ConstBool(b) => Some(b),
                _ => None,
            }
        };
        // Every exact position must be decided and agree with the concrete
        // collision routine.
        let mut solid_seen = false;
        let mut open_seen = false;
        for y in (0..128).step_by(8) {
            for x in (0..128).step_by(8) {
                let want = cache.solid_at(&cart, x, y, 8, 8).expect("solid_at");
                assert_eq!(at(x, y), Some(want), "at ({}, {})", x, y);
                solid_seen |= want;
                open_seen |= !want;
            }
        }
        assert!(solid_seen && open_seen, "the room has both walls and space");

        // An UNKNOWN position is not decidable.
        let mut g = Graph::new();
        let cell = g.leaf(Op::Cell(0));
        let n = |g: &mut Graph, v: i16| {
            let r = Pico8Num::from_i16(v).as_raw_u32() as i32;
            g.leaf(Op::Const(r, r))
        };
        let (w, h) = (n(&mut g, 8), n(&mut g, 8));
        let f = n(&mut g, 0);
        let t = g.fold(Op::TileFlagAt, vec![cell, cell, w, h, f]);
        let (out, map, _) = fold(&g, &[t], Some(&room)).expect("folds");
        assert_eq!(out.get(map[t as usize]).op, Op::TileFlagAt, "unknown stays unknown");
    }

    /// A collision test with ONE coordinate at TOP is decided false only where
    /// no position along that axis is solid, in every level room. (Capping the
    /// derived rectangle's corner and size apart once read TOP as tile row 0
    /// only and put a spawning player in the air on the ground.)
    #[test]
    fn a_collision_test_at_a_top_coordinate_folds_only_to_what_every_position_gives() {
        let cart = std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let raw = |v: i16| Pico8Num::from_i16(v).as_raw_u32() as i32;
        let (mut checked, mut decided) = (0usize, 0usize);
        for ry in 0..4i16 {
            for rx in 0..8i16 {
                if (rx, ry) == (7, 3) {
                    continue; // the summit: no level room
                }
                let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart, rx, ry).expect("cache"));
                let room = Room { cart: cart.clone(), cache: cache.clone() };
                // The player's hitbox (6x5) and a whole tile.
                for (w, h) in [(6i16, 5i16), (8, 8)] {
                    for at in -8..136i16 {
                        for top_is_y in [true, false] {
                            let mut g = Graph::new();
                            let exact = g.leaf(Op::Const(raw(at), raw(at)));
                            let top = g.leaf(Op::Cell(0));
                            let (px, py) = if top_is_y { (exact, top) } else { (top, exact) };
                            let (gw, gh, f) = (g.leaf(Op::Const(raw(w), raw(w))), g.leaf(Op::Const(raw(h), raw(h))), g.leaf(Op::Const(0, 0)));
                            let t = g.fold(Op::TileFlagAt, vec![px, py, gw, gh, f]);
                            let (out, map, _) = fold(&g, &[t], Some(&room)).expect("folds");
                            let got = match out.get(map[t as usize]).op {
                                Op::ConstBool(b) => Some(b),
                                _ => None,
                            };
                            // What the concrete test gives along the TOP axis
                            // (beyond [-16, 144] every rectangle misses the room).
                            let any_solid = (-16..=144i16).any(|v| {
                                let (x, y) = if top_is_y { (at, v) } else { (v, at) };
                                cache.solid_at(&cart, x, y, w, h).expect("solid_at")
                            });
                            checked += 1;
                            decided += got.is_some() as usize;
                            assert!(got != Some(true), "room ({rx},{ry}) {w}x{h} at {at} ({}) folded to true", if top_is_y { "x; y TOP" } else { "y; x TOP" });
                            assert!(
                                got != Some(false) || !any_solid,
                                "room ({rx},{ry}) {w}x{h} at {} = {at}, the other axis TOP: folded to false, but a position along it is solid",
                                if top_is_y { "x" } else { "y" }
                            );
                        }
                    }
                }
            }
        }
        // Open columns ARE decided false: the test is not vacuous.
        assert!(decided > 0 && decided < checked, "{decided} of {checked} decided");
    }

    /// A node it cannot model must not be decided, and must not poison the
    /// nodes that do not depend on it.
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
        let (out, map, _) = fold(&g, &[both], None).expect("folds");
        // The tile lookup stays; the arithmetic fact next to it does not.
        assert_eq!(out.get(map[le as usize]).op, Op::ConstBool(true));
        assert_eq!(out.get(map[both as usize]).op, Op::TileFlagAt);
    }

    /// Fork validity is a per-LANE fact, and the interval pass sees a hull over
    /// every lane. A hull spanning many floors contains lanes that span exactly
    /// two, so `FragOk(1)` must stay undecided there (deciding it false loses
    /// rows). A hull inside ONE floor decides it: no lane can straddle.
    #[test]
    fn fragment_validity_is_not_decided_from_a_wide_hull() {
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(0));
        let ok1 = g.fold(Op::FragOk(1), vec![x]);
        let frag1 = g.fold(Op::Frag(1), vec![x]);
        let (out, map, _) = fold(&g, &[ok1, frag1], None).expect("folds");
        assert_eq!(out.get(map[ok1 as usize]).op, Op::FragOk(1), "wide hull: undecided");
        assert_eq!(out.get(map[frag1 as usize]).op, Op::Frag(1), "the fragment stays a fragment");

        // Inside one floor nothing straddles, so fragment 1 is never valid.
        let mut g = Graph::new();
        let lo = g.leaf(Op::Const(65536 * 3 + 1000, 65536 * 3 + 1000));
        let hi = g.leaf(Op::Const(65536 * 3 + 50000, 65536 * 3 + 50000));
        let x = g.fold(Op::Span, vec![lo, hi]);
        let ok1 = g.fold(Op::FragOk(1), vec![x]);
        let (out, map, _) = fold(&g, &[ok1], None).expect("folds");
        assert_eq!(out.get(map[ok1 as usize]).op, Op::ConstBool(false));
    }

    /// `Known` is a per-lane fact too: a hull spanning several floors contains
    /// lanes whose floor IS decided, so `Known(Flr(hull))` must stay undecided,
    /// not fold to false (which refuses every lane).
    #[test]
    fn known_of_a_wide_hull_is_undecided() {
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(0));
        let fl = g.fold(Op::Flr, vec![x]);
        let known = g.fold(Op::Known, vec![fl]);
        let (out, map, _) = fold(&g, &[known], None).expect("folds");
        assert_eq!(out.get(map[known as usize]).op, Op::Known, "wide hull: undecided");

        // A point hull is decided everywhere.
        let mut g = Graph::new();
        let p = g.leaf(Op::Const(65536 * 3 + 1000, 65536 * 3 + 1000));
        let fl = g.fold(Op::Flr, vec![p]);
        let known = g.fold(Op::Known, vec![fl]);
        let (out, map, _) = fold(&g, &[known], None).expect("folds");
        assert_eq!(out.get(map[known as usize]).op, Op::ConstBool(true));
    }

    /// Folding must not change what the graph computes (sampled).
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
        let (out, map, _) = fold(&g, &roots, None).expect("folds");
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
