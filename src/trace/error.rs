//! ERROR, derived from the operators.
//!
//! Error is a property of a value: an operator whose input has error has
//! error, and an operator may add an own error as a function of its inputs.
//! An outcome's error is the error of what it stores and of its live guard;
//! nothing carries error while the frame is traced, so an obligation of a
//! value nothing reads contributes nothing.
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
//! | `Mul`, `Div` of an interval by a positive literal that can carry an endpoint out of range (or a scalar no literal yet), the operand's static range not inside `Scaled::fits` | `not NoWrap(op)` | as `Add`: `*` wraps, `/` saturates, where `Pico8NumInterval`'s checked operation has no answer |
//! | `Restrict(lo, hi)(x)` | `Lo(x) < lo or Hi(x) > hi`, on the raw `x` | the body was specialized on the range: the static ranges folded comparisons with it |
//!
//! A RESTRICTION's own error is charged to EVERY outcome of the frame, not
//! only to those that reach the node: a comparison its range decided folded
//! to a constant that no longer reads it (in the tracer, `Points` and the
//! lowering's interval fold alike), so reachability cannot see the use. It
//! reads only the raw input, which nothing seeds, so no fold removes it.
//!
//! A fork's VALIDITY (`SplitValid*`) owns nothing: it is defined on every
//! lane, and a row that does not read a fragment's value is the right row
//! for an unenumerated part too.
//!
//! AN OWN ERROR HOLDS WHERE ITS OPERATOR IS EVALUATED. The kernel computes
//! every node on every lane, but off the tracer's path a node's operands are
//! whatever the other path left (a `move` fork's operand spans three floors
//! on a lane that dashed the other way). So a source's error is `own and
//! at`, `at` the OR of the path guards it was evaluated under
//! (`Symbolic::evaluated`); without a recorded site `at` is true, so it
//! declines rather than drops. This is cheaper than an exact Kleene-lazy
//! derivation, which would copy the guard and value DAG above every source.
//!
//! Not here: what no graph operator carries (an output widening's
//! containment, the pins, an unfinished unrolled loop). The tracer states those and ORs them into the outcome's error
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
/// interned (most nodes share their operands' set). A cone walk per outcome
/// would be outcomes x graph.
pub fn for_outcomes(d: &mut Symbolic, outcomes: &[Vec<NodeId>]) -> Vec<NodeId> {
    let sites: Vec<NodeId> = d.evaluated.values().copied().collect();
    // Every outcome reads every restriction (the module doc).
    let assumed = d.restricts.clone();
    let all: Vec<NodeId> = outcomes.iter().flatten().copied().chain(sites).chain(assumed.iter().copied()).collect();
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
            for r in roots.iter().chain(&assumed) {
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
    // "Can a lane's value here be a SET?" assumes every other operator's own
    // error holds (`abstract_beneath_lane_ops` takes an inner `Flr` as a
    // singleton): where one does not, that operator is itself a source in
    // the same cone and is counted. Plain abstractness would make every
    // floor of the player's position after a `move` a runtime check.
    let own = match op {
        Op::Flr if d.abstract_beneath_lane_ops(a?) && !within_one_integer(d, a?) => undecided(d, n),
        Op::Split(k) | Op::SplitInt(k) if d.abstract_beneath_lane_ops(a?) => {
            let ways = d.graph.fork_ways(k);
            let covered = d.graph.fold(Op::SplitOk(ways), vec![a?]);
            d.graph.fold(Op::Not, vec![covered])
        }
        // Only where a LANE can hold `c` both ways (`Symbolic::lane_undecidable`):
        // a condition the kernel decides per lane owns nothing.
        Op::Sel if d.lane_undecidable(a?) => undecided(d, a?),
        // Overflow is an assertion: a wrapped interval endpoint is not the
        // result, so the lane declines, never wraps or widens silently. Only
        // for interval results (an exact operation wraps as PICO-8 does) the
        // static ranges do not bound.
        Op::Add | Op::Sub | Op::Neg if d.is_interval(&n) && !bounded(d, n) => {
            let fits = d.graph.fold(Op::NoWrap, vec![n]);
            d.graph.fold(Op::Not, vec![fits])
        }
        // The same for a scale or divide, bounded by the OPERAND's range: the
        // product's range is unknown past 16.16 anyway, and a saturated
        // quotient's always lies inside it. `NoWrap` folds to true where the
        // literal cannot overflow (a factor at most 1, a divisor at least 1).
        Op::Mul | Op::Div if d.is_interval(&n) && !scaled_bounded(d, n) => {
            let fits = d.graph.fold(Op::NoWrap, vec![n]);
            d.graph.fold(Op::Not, vec![fits])
        }
        // The assumption, checked on the raw operand: `Lo`/`Hi` of a number
        // are the number.
        Op::Restrict(lo, hi) => {
            let x = a?;
            let (klo, khi) = (d.graph.leaf(Op::Const(lo, lo)), d.graph.leaf(Op::Const(hi, hi)));
            let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Hi, vec![x]));
            let below = d.graph.fold(Op::Lt, vec![vlo, klo]);
            let above = d.graph.fold(Op::Gt, vec![vhi, khi]);
            d.graph.fold(Op::Or, vec![below, above])
        }
        _ => return None,
    };
    (d.graph.get(own).op != Op::ConstBool(false)).then_some(own)
}

/// Does every lane's value of `n` lie within one integer, so that its floor
/// is exact by construction? True for a floor fork's fragment, a literal
/// whose ends share a floor, a select of such values, and a value that is
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

/// Do the static ranges (`Symbolic::range_of`) put every value of `n` inside
/// the 16.16 range? In i64, so a bound past it is seen rather than wrapped.
fn bounded(d: &mut Symbolic, n: NodeId) -> bool {
    d.range_of(n).is_some_and(|pieces| pieces.iter().all(|&(lo, hi)| lo >= i32::MIN as i64 && hi <= i32::MAX as i64))
}

/// Do the static ranges put every value of a scale / divide's interval
/// operand where its image fits (`Scaled::fits`)? False while the scalar is
/// no positive literal: its `NoWrap` stays for a specialization to decide
/// (the kernel models no other interval `*` or `/`).
fn scaled_bounded(d: &mut Symbolic, n: NodeId) -> bool {
    let Some(s) = crate::transpile::graph::scaled(&d.graph, n) else { return false };
    let (lo, hi) = s.fits();
    let operand = d.graph.get(n).args[s.operand];
    d.range_of(operand).is_some_and(|pieces| pieces.iter().all(|&(a, b)| a >= lo as i64 && b <= hi as i64))
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
    fn interval_scale_and_divide_own_their_no_wrap_where_the_literal_can_overflow() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let k = |d: &mut Symbolic, raw: i32| d.graph.leaf(Op::Const(raw, raw));
        for (op, raw, owns) in [
            (Op::Mul, 0x2_0000, true),
            (Op::Mul, 0x1_0000, false),
            (Op::Mul, 0x4000, false),
            (Op::Div, 0x4000, true),
            (Op::Div, 0x8_0000, false),
        ] {
            let c = k(&mut d, raw);
            let r = d.graph.fold(op.clone(), vec![x, c]);
            let e = of(&mut d, &[r]);
            if owns {
                let fits = d.graph.fold(Op::NoWrap, vec![r]);
                assert_eq!(e, d.graph.fold(Op::Not, vec![fits]), "{op:?} {raw:#x}");
            } else {
                assert_eq!(d.graph.get(e).op, Op::ConstBool(false), "{op:?} {raw:#x}");
            }
        }
        let two = k(&mut d, 0x2_0000);
        // Exact: PICO-8's own wrap (before any restriction, which every
        // outcome owes).
        let y = cell(&mut d, 1, false);
        let m = d.graph.fold(Op::Mul, vec![y, two]);
        let e = of(&mut d, &[m]);
        assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
        // The operand's static range inside `fits`: nothing beyond the
        // restriction's own check. One ulp past it: the product owes it.
        let inside = d.restrict(x, i32::MIN / 2, i32::MAX / 2);
        let m = d.graph.fold(Op::Mul, vec![inside, two]);
        assert_eq!(of(&mut d, &[m]), of(&mut d, &[inside]));
        let past = d.restrict(x, i32::MIN / 2, i32::MAX / 2 + 1);
        let m = d.graph.fold(Op::Mul, vec![past, two]);
        let fits = d.graph.fold(Op::NoWrap, vec![m]);
        let e = of(&mut d, &[m]);
        assert!(crate::transpile::bdd::reachable(&d.graph, &[e])[fits as usize], "the product's NoWrap is owed");
    }

    #[test]
    fn interval_arithmetic_the_static_ranges_bound_owns_nothing() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let x = d.restrict(x, -(4 << 16), 4 << 16);
        let one = d.graph.leaf(Op::Const(1 << 16, 1 << 16));
        let s = d.graph.fold(Op::Add, vec![x, one]);
        // Nothing beyond the restriction's own check.
        let e = of(&mut d, &[s]);
        assert_eq!(e, of(&mut d, &[x]));
        assert_ne!(d.graph.get(e).op, Op::ConstBool(false), "the restriction itself is checked");
        // A range that reaches past the 16.16 range is still checked.
        let y = cell(&mut d, 1, true);
        let y = d.restrict(y, 0, i32::MAX);
        let t = d.graph.fold(Op::Add, vec![y, one]);
        let fits = d.graph.fold(Op::NoWrap, vec![t]);
        let e = of(&mut d, &[t]);
        assert!(crate::transpile::bdd::reachable(&d.graph, &[e])[fits as usize], "the sum's NoWrap is owed");
    }

    #[test]
    fn a_restriction_is_checked_on_the_raw_input_in_every_outcome() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, false);
        let r = d.restrict(x, -(6 << 16), 6 << 16);
        // A comparison its range decides folds and reads nothing...
        let seven = d.graph.leaf(Op::Const(7 << 16, 7 << 16));
        let decided = d.compare(super::super::domain::Cmp::Lt, &r, &seven).expect("compare");
        assert_eq!(d.graph.get(decided).op, Op::ConstBool(true), "the range decides it");
        // ...yet every outcome, one reading nothing at all, owes the bound.
        let (klo, khi) = (d.graph.leaf(Op::Const(-(6 << 16), -(6 << 16))), d.graph.leaf(Op::Const(6 << 16, 6 << 16)));
        let (lo, hi) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Hi, vec![x]));
        let below = d.graph.fold(Op::Lt, vec![lo, klo]);
        let above = d.graph.fold(Op::Gt, vec![hi, khi]);
        let own = d.graph.fold(Op::Or, vec![below, above]);
        let errs = for_outcomes(&mut d, &[vec![decided], vec![]]);
        assert_eq!(errs, vec![own, own]);
        // The check reads the raw cell, which no range covers.
        assert_eq!(d.range_of(x), None);
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
