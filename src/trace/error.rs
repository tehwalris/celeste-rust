//! ERROR, DERIVED from the operators (plans/graph-model.md section 4).
//!
//! Error is a property of a value: an operator whose input has error has
//! error, and an operator may add error of its own as a function of its
//! inputs. So an outcome's error is the error of what it stores and of the
//! guard that says where it is live, and nothing carries it while the frame
//! is traced. It is derived once, here, from the graph - so an obligation of
//! a value nothing reads contributes nothing.
//!
//! The operators with an own error are the ones the kernel computes only
//! partially:
//!
//! | operator | own error | why the kernel needs it |
//! |---|---|---|
//! | `Flr(x)`, `x` a set not within one integer | `not Known(Flr(x))` | the kernel floors the low end (`zi_flr_ok` is the test) |
//! | a fork's fragment (`Split`, `SplitInt`) of a set | not covered: `not SplitOk(ways)` | a lane spanning more parts than are enumerated has none to go to |
//! | `Sel(c, t, f)`, `c` lane-undecidable | `not Known(c)` | the kernel picks an arm by `c`'s value bit |
//! | `Add`, `Sub`, `Neg` of an interval, not bounded inside the 16.16 range by the static ranges | `not NoWrap(op)` | the kernel computes each endpoint in wrapping i32: an overflow wraps it apart from the other |
//!
//! A fork's VALIDITY (`SplitValid*`) owns nothing: whether this
//! configuration's part is non-empty is defined on every lane. What an
//! uncovered lane loses is the states in the parts nobody enumerated, and
//! that is only wrong where something reads a part's VALUE - a row that does
//! not depend on the fragment is the right row for the missing part too.
//!
//! AN OWN ERROR HOLDS WHERE ITS OPERATOR IS EVALUATED. The kernel computes
//! every node on every lane, but the tracer built each one on a path, and
//! off that path its operands are whatever the lane's other path left in
//! them: a `move` fork's operand spans three floors on a lane that dashed
//! the other way. So a source's error is `own and at`, `at` the OR of the
//! path guards where the tracer evaluated it (`Symbolic::evaluated`). Error
//! derived without it - strict through every `And`/`Or` - declined room
//! (1,0) at f25 on the first try (2026-09-27); derived EXACTLY instead
//! (Kleene-lazy `And`/`Or`/`Sel`, which is what the kernel computes) it was
//! a copy of the guard and value DAG above every source, +23% frame time on
//! room (1,0) f0-f44 for four sources a frame. Where the tracer never said
//! where a source was evaluated, `at` is true: strict, so it declines rather
//! than drops.
//!
//! What is NOT here is anything an operator in the graph does not carry: an
//! output widening's containment, the kernel's admissible inputs, an unrolled
//! loop that had not finished. Those are stated by the tracer where they
//! arise and OR-ed into the outcome's error beside what this derives
//! (`verify::trace_frame`).

use crate::transpile::graph::{NodeId, Op};

use super::domain::{Domain, Symbolic};

/// The error of the values `roots` (`for_outcomes`, one outcome).
pub fn of(d: &mut Symbolic, roots: &[NodeId]) -> NodeId {
    for_outcomes(d, &[roots.to_vec()])[0]
}

/// The error of each outcome's values: the OR, over every operator with an
/// own error that they reach, of `own and at` (the module doc). The
/// conditions those operators were evaluated under are read too, so a
/// source THEY reach counts as well.
///
/// One bottom-up pass for all outcomes: each node's set of sources below it,
/// interned (most nodes share their operands' set, most of those the empty
/// one). Walking each outcome's cone separately was outcomes x graph, and
/// room (3,0)'s trace went from 25 s to 448 s with it (2026-09-27).
pub fn for_outcomes(d: &mut Symbolic, outcomes: &[Vec<NodeId>]) -> Vec<NodeId> {
    let sites: Vec<NodeId> = d.evaluated.values().copied().collect();
    let all: Vec<NodeId> = outcomes.iter().flatten().copied().chain(sites).collect();
    let reach = crate::transpile::bdd::reachable(&d.graph, &all);
    // The sources and their own errors, in node order.
    let mut sources: Vec<(NodeId, NodeId)> = Vec::new();
    let mut index: rustc_hash::FxHashMap<NodeId, usize> = Default::default();
    for n in 0..reach.len() as NodeId {
        if reach[n as usize] {
            if let Some(own) = own_error(d, n) {
                index.insert(n, sources.len());
                sources.push((n, own));
            }
        }
    }
    let words = sources.len().div_ceil(64);
    // Per node, the id of its source set in `sets`; set 0 is the empty one.
    let mut sets: Vec<Vec<u64>> = vec![vec![0; words]];
    let mut interned: rustc_hash::FxHashMap<Vec<u64>, u32> = Default::default();
    interned.insert(vec![0; words], 0);
    let mut set_of: Vec<u32> = vec![0; reach.len()];
    for n in 0..reach.len() {
        if !reach[n] {
            continue;
        }
        let args = &d.graph.get(n as NodeId).args;
        let mine = index.get(&(n as NodeId)).copied();
        let first = args.first().map_or(0, |a| set_of[*a as usize]);
        if mine.is_none() && args.iter().all(|a| set_of[*a as usize] == first) {
            set_of[n] = first;
            continue;
        }
        let mut u = sets[first as usize].clone();
        for a in args {
            for (w, x) in u.iter_mut().zip(&sets[set_of[*a as usize] as usize]) {
                *w |= x;
            }
        }
        if let Some(i) = mine {
            u[i / 64] |= 1 << (i % 64);
        }
        set_of[n] = *interned.entry(u.clone()).or_insert_with(|| {
            sets.push(u);
            (sets.len() - 1) as u32
        });
    }
    let at: Vec<Option<NodeId>> = sources.iter().map(|(n, _)| d.evaluated.get(n).copied()).collect();
    outcomes
        .iter()
        .map(|roots| {
            let mut have = vec![0u64; words];
            for r in roots {
                for (w, x) in have.iter_mut().zip(&sets[set_of[*r as usize] as usize]) {
                    *w |= x;
                }
            }
            // Close over the conditions the sources were evaluated under.
            let mut done = vec![0u64; words];
            while have != done {
                let fresh: Vec<usize> = (0..sources.len()).filter(|i| have[i / 64] >> (i % 64) & 1 == 1 && done[i / 64] >> (i % 64) & 1 == 0).collect();
                done.clone_from(&have);
                for i in fresh {
                    if let Some(a) = at[i] {
                        for (w, x) in have.iter_mut().zip(&sets[set_of[a as usize] as usize]) {
                            *w |= x;
                        }
                    }
                }
            }
            let mut error = d.graph.leaf(Op::ConstBool(false));
            for (i, (_, own)) in sources.iter().enumerate() {
                if have[i / 64] >> (i % 64) & 1 == 0 {
                    continue;
                }
                let term = match at[i] {
                    Some(a) => d.graph.fold(Op::And, vec![a, *own]),
                    None => *own,
                };
                error = d.graph.fold(Op::Or, vec![error, term]);
            }
            error
        })
        .collect()
}

/// `n`'s own error, where it has one.
fn own_error(d: &mut Symbolic, n: NodeId) -> Option<NodeId> {
    let node = d.graph.get(n);
    let (op, a) = (node.op.clone(), node.args.first().copied());
    // "Can a lane's value here be a SET?" is asked with every other
    // operator's own error assumed to hold - `abstract_beneath_lane_ops`,
    // which takes an inner `Flr` as the singleton it is on pain of its own
    // error - because where one does not hold, that operator is itself a
    // source in the same cone and its error is counted. Plain `abstractness`
    // made every floor of the player's position after a `move` a runtime
    // check (`spikes_at`'s `flr((x + 3) / 8)` of `x + flr(fragment)`): four
    // sources a frame the old premises never had, and on room (3,0)'s
    // largest kernel 1M nodes of fused error, because no two of its 3.3M
    // configurations then shared one (2026-09-27).
    let own = match op {
        Op::Flr if d.abstract_beneath_lane_ops(a?) && !within_one_integer(d, a?) => undecided(d, n),
        Op::Split(k) | Op::SplitInt(k) if d.abstract_beneath_lane_ops(a?) => {
            let ways = d.graph.fork_ways(k);
            let covered = d.graph.fold(Op::SplitOk(ways), vec![a?]);
            d.graph.fold(Op::Not, vec![covered])
        }
        // Only where a LANE can hold `c` both ways: a condition the kernel
        // decides per lane - by instruction (`TileFlagAt`), or over a floor
        // whose own error covers it - owns nothing here
        // (`Symbolic::lane_undecidable`, the fork trigger: after
        // `fork_undecided_selects` such a select survives only where the
        // level keeps it, level -1).
        Op::Sel if d.lane_undecidable(a?) => undecided(d, a?),
        // Overflow is an ASSERTION (2026-10-03): an interval whose endpoint
        // wrapped is not the result, so the lane declines - never wraps
        // silently, never widens silently. Only where the result is an
        // interval in the kernel (an exact operation wraps as PICO-8 does)
        // and the static ranges do not already bound it inside the range.
        Op::Add | Op::Sub | Op::Neg if d.is_interval(&n) && !bounded(d, n) => {
            let fits = d.graph.fold(Op::NoWrap, vec![n]);
            d.graph.fold(Op::Not, vec![fits])
        }
        _ => return None,
    };
    (d.graph.get(own).op != Op::ConstBool(false)).then_some(own)
}

/// Does every lane's value of `n` lie within one integer, so that its floor
/// is exact by construction? A floor fork's fragment does (a grid cell is at
/// most one integer wide - `move`'s `flr(__split_by_flr(rem))` is the case),
/// a literal whose ends share a floor does, a select of such values does
/// (its condition's own error is the select's), and so does a value that is
/// one number on the lane. Anything else is checked per lane.
fn within_one_integer(d: &Symbolic, n: NodeId) -> bool {
    let node = d.graph.get(n);
    match node.op {
        Op::Split(_) => true,
        Op::Const(lo, hi) => lo >> 16 == hi >> 16,
        Op::Sel => within_one_integer(d, node.args[1]) && within_one_integer(d, node.args[2]),
        _ => !d.abstract_beneath_lane_ops(n),
    }
}

/// Do the static ranges (`Symbolic::range_of`, the bucket dispatch's
/// runtime-guarded seeds) put every value of `n` inside the 16.16 range? In
/// i64, so a bound past it is seen rather than wrapped.
fn bounded(d: &mut Symbolic, n: NodeId) -> bool {
    d.range_of(n).is_some_and(|pieces| pieces.iter().all(|&(lo, hi)| lo >= i32::MIN as i64 && hi <= i32::MAX as i64))
}

/// `not Known(b)`: the lane holds `b` undecided.
fn undecided(d: &mut Symbolic, b: NodeId) -> NodeId {
    let k = d.graph.fold(Op::Known, vec![b]);
    d.graph.fold(Op::Not, vec![k])
}

#[cfg(test)]
mod tests {
    use super::super::domain::Domain;
    use super::*;

    fn cell(d: &mut Symbolic, i: u32, interval: bool) -> NodeId {
        if interval {
            d.ival_cells.insert(i);
            d.forget_intervals();
        }
        d.graph.leaf(Op::Cell(i))
    }

    #[test]
    fn an_exact_computation_has_no_error() {
        let mut d = Symbolic::default();
        let (a, b) = (cell(&mut d, 0, false), cell(&mut d, 1, false));
        let s = d.graph.fold(Op::Add, vec![a, b]);
        let f = d.graph.fold(Op::Flr, vec![s]);
        let c = d.graph.fold(Op::Lt, vec![a, b]);
        let sel = d.graph.fold(Op::Sel, vec![c, f, a]);
        let e = of(&mut d, &[sel]);
        assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
    }

    #[test]
    fn flr_of_a_set_owns_its_span_claim_and_it_reaches_what_reads_it() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let f = d.graph.fold(Op::Flr, vec![x]);
        let one = d.graph.leaf(Op::Const(1 << 16, 1 << 16));
        let y = d.graph.fold(Op::Add, vec![f, one]);
        let e = of(&mut d, &[y]);
        assert_eq!(e, undecided(&mut d, f), "error(flr(x) + 1) is flr's own error, nothing else");
    }

    #[test]
    fn an_own_error_holds_where_its_operator_was_evaluated() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let f = d.graph.fold(Op::Flr, vec![x]);
        let at = cell(&mut d, 1, false);
        d.evaluated_at(&f, &at);
        let e = of(&mut d, &[f]);
        let own = undecided(&mut d, f);
        assert_eq!(e, d.graph.fold(Op::And, vec![at, own]));
    }

    #[test]
    fn the_guard_a_source_was_evaluated_under_is_read_for_sources_too() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let f = d.graph.fold(Op::Flr, vec![x]);
        let y = cell(&mut d, 1, true);
        let g = d.graph.fold(Op::Flr, vec![y]);
        let zero = d.graph.leaf(Op::Const(0, 0));
        let at = d.graph.fold(Op::Gt, vec![g, zero]);
        d.evaluated_at(&f, &at);
        let e = of(&mut d, &[f]);
        // `f`'s site reads `g`, whose own error counts too: where `g` is
        // undefined, so is the site.
        let own_f = undecided(&mut d, f);
        let own_g = undecided(&mut d, g);
        let term = d.graph.fold(Op::And, vec![at, own_f]);
        assert_eq!(e, d.graph.fold(Op::Or, vec![term, own_g]));
    }

    #[test]
    fn a_select_on_a_set_owns_its_condition_being_decided() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let zero = d.graph.leaf(Op::Const(0, 0));
        let c = d.graph.fold(Op::Gt, vec![x, zero]);
        let (p, q) = (cell(&mut d, 1, false), cell(&mut d, 2, false));
        let sel = d.graph.fold(Op::Sel, vec![c, p, q]);
        let e = of(&mut d, &[sel]);
        assert_eq!(e, undecided(&mut d, c));
    }

    #[test]
    fn a_fork_owns_its_coverage_and_a_fork_of_a_literal_is_covered() {
        let mut d = Symbolic::default();
        d.graph.set_fork_ways(0, 2);
        let x = cell(&mut d, 0, true);
        let v = d.graph.fold(Op::Split(0), vec![x]);
        let e = of(&mut d, &[v]);
        let ok = d.graph.fold(Op::SplitOk(2), vec![x]);
        assert_eq!(e, d.graph.fold(Op::Not, vec![ok]));
        // `both_values`' fork: of the literal [0, 1], two parts always cover.
        let b = d.both_values("test");
        let e = of(&mut d, &[b]);
        assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
    }

    #[test]
    fn the_floor_of_a_floor_forks_fragment_is_exact() {
        let mut d = Symbolic::default();
        d.graph.set_fork_ways(0, 2);
        let x = cell(&mut d, 0, true);
        let frag = d.graph.fold(Op::Split(0), vec![x]);
        let f = d.graph.fold(Op::Flr, vec![frag]);
        // Only the fork's coverage: the floor itself owns nothing.
        assert_eq!(of(&mut d, &[f]), of(&mut d, &[frag]));
    }

    #[test]
    fn the_floor_of_a_value_moved_by_a_floor_is_exact() {
        let mut d = Symbolic::default();
        d.graph.set_fork_ways(0, 2);
        let rem = cell(&mut d, 0, true);
        let frag = d.graph.fold(Op::Split(0), vec![rem]);
        let amount = d.graph.fold(Op::Flr, vec![frag]);
        let x = cell(&mut d, 1, false);
        let moved = d.graph.fold(Op::Add, vec![x, amount]);
        let eight = d.graph.leaf(Op::Const(8 << 16, 8 << 16));
        let tile = d.graph.fold(Op::Div, vec![moved, eight]);
        let f = d.graph.fold(Op::Flr, vec![tile]);
        // `flr((x + flr(frag)) / 8)`: one number per lane wherever the
        // fragment is covered, so only the fork's coverage.
        assert_eq!(of(&mut d, &[f]), of(&mut d, &[frag]));
    }

    #[test]
    fn interval_arithmetic_owns_its_no_wrap_and_exact_arithmetic_does_not() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let y = cell(&mut d, 1, false);
        for op in [Op::Add, Op::Sub] {
            let r = d.graph.fold(op, vec![x, y]);
            let e = of(&mut d, &[r]);
            let fits = d.graph.fold(Op::NoWrap, vec![r]);
            assert_eq!(e, d.graph.fold(Op::Not, vec![fits]));
        }
        let n = d.graph.fold(Op::Neg, vec![x]);
        let e = of(&mut d, &[n]);
        let fits = d.graph.fold(Op::NoWrap, vec![n]);
        assert_eq!(e, d.graph.fold(Op::Not, vec![fits]));
        // Exact: PICO-8's own wrap, one number.
        let z = cell(&mut d, 2, false);
        let s = d.graph.fold(Op::Add, vec![y, z]);
        let e = of(&mut d, &[s]);
        assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
    }

    #[test]
    fn interval_arithmetic_the_static_ranges_bound_owns_nothing() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        d.ranges.insert(x, (-(4 << 16), 4 << 16));
        let one = d.graph.leaf(Op::Const(1 << 16, 1 << 16));
        let s = d.graph.fold(Op::Add, vec![x, one]);
        let e = of(&mut d, &[s]);
        assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
        // A range that reaches past the 16.16 range is still checked.
        let y = cell(&mut d, 1, true);
        d.ranges.insert(y, (0, i32::MAX as i64));
        let t = d.graph.fold(Op::Add, vec![y, one]);
        let fits = d.graph.fold(Op::NoWrap, vec![t]);
        assert_eq!(of(&mut d, &[t]), d.graph.fold(Op::Not, vec![fits]));
    }

    #[test]
    fn a_forks_validity_owns_nothing() {
        let mut d = Symbolic::default();
        d.graph.set_fork_ways(0, 2);
        let x = cell(&mut d, 0, true);
        let valid = d.graph.fold(Op::SplitValid(0), vec![x]);
        let e = of(&mut d, &[valid]);
        assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
    }
}
