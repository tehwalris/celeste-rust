//! ERROR, derived from the operators (plans/architecture.md, "Error is
//! derived, never carried", has the table of own errors). An operator whose
//! input has error has error, and an operator the kernel computes only
//! partially adds an own error (`own_error`). An outcome's error is that of
//! what it stores, of its guard and of its transfer roots; nothing carries
//! error while the frame is traced, so an obligation of a value nothing
//! reads contributes nothing.
//!
//! AN OWN ERROR HOLDS WHERE ITS OPERATOR IS EVALUATED. The kernel computes
//! every node on every lane, but off the tracer's path a node's operands are
//! whatever the other path left (a `move` fork's operand spans three floors
//! on a lane that dashed the other way). So a source's error is `own and
//! at`, `at` the OR of the path guards it was evaluated under
//! (`Symbolic::evaluated`); without a recorded site `at` is true, so it
//! declines rather than drops.
//!
//! A RESTRICTION's own error is charged to EVERY outcome of the frame: a
//! comparison its range decided folded to a constant that no longer reads
//! it, so reachability cannot see the use. It reads only the raw input,
//! which nothing seeds, so no fold removes it. A PIN has no data output
//! (every use of its input reads the constant): its check is the frame's own
//! (`pins_hold`), ORed into every outcome's error by `verify::trace_frame`.
//! A fork's VALIDITY (`SplitValid*`) owns nothing: a row that does not read
//! a fragment's value is the right row for an unenumerated part too.

use crate::transpile::graph::{NodeId, Op};

use super::domain::Symbolic;
use crate::transpile::garith::GraphArith;

/// Every PIN of the frame holds (`Symbolic::pin`, in the order made): the
/// conjunction of `x` inside `c` on the raw inputs; `true` without pins. Its
/// negation is an error of the whole frame, never a guard: a disagreeing lane
/// is real and must decline loudly.
pub fn pins_hold<A: GraphArith>(d: &mut Symbolic<A>) -> NodeId {
    use crate::transpile::graph::Pinned;
    let pins: Vec<(NodeId, Pinned<A>)> = d
        .assumptions
        .iter()
        .filter_map(|a| match d.graph.get(*a).op {
            Op::Pin(c) => Some((d.graph.get(*a).args[0], c)),
            _ => None,
        })
        .collect();
    let mut acc = d.graph.leaf(Op::ConstBool(true));
    for (x, c) in pins {
        let t = match c {
            Pinned::Num(v, w) if v == w => {
                let k = d.graph.leaf(Op::Const(v, v));
                d.graph.fold(if A::EQ_IS_IDENTITY { Op::Eq } else { Op::Same }, vec![x, k])
            }
            // Inside a literal interval (`widen::pin_literal`: the body reads
            // the literal, the input disconnected): both ends, on the raw
            // input, by value (a double's `-0` lies in a range from `-1/2`).
            Pinned::Num(lo, hi) => {
                let (klo, khi) = (d.graph.leaf(Op::Const(lo, lo)), d.graph.leaf(Op::Const(hi, hi)));
                let (xlo, xhi) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Hi, vec![x]));
                let above = d.graph.fold(Op::Ge, vec![xlo, klo]);
                let below = d.graph.fold(Op::Le, vec![xhi, khi]);
                d.graph.fold(Op::And, vec![above, below])
            }
            Pinned::Bool(true) => x,
            Pinned::Bool(false) => d.graph.fold(Op::Not, vec![x]),
        };
        acc = d.graph.fold(Op::And, vec![acc, t]);
    }
    acc
}

/// The error of the values `roots` (`for_outcomes`, one outcome).
pub fn of<A: GraphArith>(d: &mut Symbolic<A>, roots: &[NodeId]) -> NodeId {
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
pub fn for_outcomes<A: GraphArith>(d: &mut Symbolic<A>, outcomes: &[Vec<NodeId>]) -> Vec<NodeId> {
    let sites: Vec<NodeId> = d.evaluated.values().copied().collect();
    // Every outcome reads every restriction (the module doc); the pins are
    // charged once for the frame (`pins_hold`).
    let assumed: Vec<NodeId> = d.assumptions.iter().copied().filter(|a| matches!(d.graph.get(*a).op, Op::Restrict(..))).collect();
    let all: Vec<NodeId> = outcomes.iter().flatten().copied().chain(sites).chain(assumed.iter().copied()).collect();
    let reach = d.graph.reachable(&all);
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
fn own_error<A: GraphArith>(d: &mut Symbolic<A>, n: NodeId) -> Option<NodeId> {
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
        Op::RSplit(k) if d.abstract_beneath_lane_ops(a?) => {
            let ways = d.graph.fork_ways(k);
            let covered = d.graph.fold(Op::RSplitOk(ways), vec![a?]);
            d.graph.fold(Op::Not, vec![covered])
        }
        // Only where a LANE can hold `c` both ways (`Symbolic::lane_undecidable`):
        // a condition the kernel decides per lane owns nothing.
        Op::Sel if d.lane_undecidable(a?) => undecided(d, a?),
        // Overflow is an assertion: a wrapped or saturated result (or
        // interval endpoint) is not the result, so the lane declines, never
        // wraps or widens silently - a point as an interval, as the
        // reference engine raises. Not where the static ranges put the exact
        // result inside the 16.16 range; `NoWrap` folds to true where the
        // literal operand cannot overflow (`x * 0.5`, `x / 2`: `one_operand`)
        // and the lowering's interval fold decides it where the hulls do.
        ref op if A::range_error(op) && !d.bounded(n) => {
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
fn within_one_integer<A: GraphArith>(d: &Symbolic<A>, n: NodeId) -> bool {
    let node = d.graph.get(n);
    match node.op {
        Op::Split(_) => true,
        Op::Const(lo, hi) => A::num_eq(A::flr(A::from_lit(lo)), A::flr(A::from_lit(hi))),
        Op::Sel => within_one_integer(d, node.args[1]) && within_one_integer(d, node.args[2]),
        _ => !d.abstract_beneath_lane_ops(n),
    }
}

/// `not Known(b)`: the lane holds `b` undecided.
fn undecided<A: GraphArith>(d: &mut Symbolic<A>, b: NodeId) -> NodeId {
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

    /// `not NoWrap(n)`: `n`'s own overflow error.
    fn wraps(d: &mut Symbolic, n: NodeId) -> NodeId {
        let fits = d.graph.fold(Op::NoWrap, vec![n]);
        d.graph.fold(Op::Not, vec![fits])
    }

    /// `a or b`, as `for_outcomes` builds it.
    fn or(d: &mut Symbolic, a: NodeId, b: NodeId) -> NodeId {
        d.graph.fold(Op::Or, vec![a, b])
    }

    #[test]
    fn an_exact_computation_owes_only_its_overflow() {
        let mut d = Symbolic::default();
        let (a, b) = (cell(&mut d, 0, false), cell(&mut d, 1, false));
        let s = d.graph.fold(Op::Add, vec![a, b]);
        let f = d.graph.fold(Op::Flr, vec![s]);
        let c = d.graph.fold(Op::Lt, vec![a, b]);
        let sel = d.graph.fold(Op::Sel, vec![c, f, a]);
        let e = of(&mut d, &[sel]);
        assert_eq!(e, wraps(&mut d, s), "two numbers' sum may leave the range; nothing else is partial");
        // Without arithmetic, nothing.
        let sel = d.graph.fold(Op::Sel, vec![c, b, a]);
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
        let (own_f, own_y) = (undecided(&mut d, f), wraps(&mut d, y));
        assert_eq!(e, or(&mut d, own_f, own_y), "error(flr(x) + 1) is flr's own error and the sum's overflow");
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
        // fragment is covered, so the fork's coverage, and the sum's
        // overflow (`/ 8` never overflows).
        let (cover, sum) = (of(&mut d, &[frag]), wraps(&mut d, moved));
        assert_eq!(of(&mut d, &[f]), or(&mut d, cover, sum));
    }

    /// Every operation that can leave the 16.16 range owns its `NoWrap`,
    /// on intervals and on points (where PICO-8 would wrap or saturate):
    /// `+ - * /`, negation, `abs`.
    #[test]
    fn arithmetic_owns_its_no_wrap_on_intervals_and_on_points() {
        let mut d = Symbolic::default();
        let x = cell(&mut d, 0, true);
        let y = cell(&mut d, 1, false);
        let z = cell(&mut d, 2, false);
        for (a, b) in [(x, y), (y, z)] {
            for op in [Op::Add, Op::Sub, Op::Mul, Op::Div] {
                if matches!(op, Op::Mul | Op::Div) && a == x {
                    continue; // an interval times a cell: not modelled
                }
                let r = d.graph.fold(op.clone(), vec![a, b]);
                let e = of(&mut d, &[r]);
                assert_eq!(e, wraps(&mut d, r), "{op:?}");
            }
            for op in [Op::Neg, Op::Abs] {
                let r = d.graph.fold(op.clone(), vec![a]);
                let e = of(&mut d, &[r]);
                assert_eq!(e, wraps(&mut d, r), "{op:?}");
            }
        }
        // `min`, `max`, `%` and `flr` never leave the range.
        for op in [Op::Min, Op::Max, Op::Rem] {
            let r = d.graph.fold(op, vec![y, z]);
            let e = of(&mut d, &[r]);
            assert_eq!(d.graph.get(e).op, Op::ConstBool(false));
        }
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
        // A point as an interval (before any restriction, which every
        // outcome owes).
        let y = cell(&mut d, 1, false);
        let m = d.graph.fold(Op::Mul, vec![y, two]);
        let e = of(&mut d, &[m]);
        assert_eq!(e, wraps(&mut d, m));
        // The operand's static range inside `fits`: nothing beyond the
        // restriction's own check. One ulp past it: the product owes it.
        let inside = d.restrict(x, i32::MIN / 2, i32::MAX / 2);
        let m = d.graph.fold(Op::Mul, vec![inside, two]);
        assert_eq!(of(&mut d, &[m]), of(&mut d, &[inside]));
        let past = d.restrict(x, i32::MIN / 2, i32::MAX / 2 + 1);
        let m = d.graph.fold(Op::Mul, vec![past, two]);
        let fits = d.graph.fold(Op::NoWrap, vec![m]);
        let e = of(&mut d, &[m]);
        assert!(d.graph.reachable(&[e])[fits as usize], "the product's NoWrap is owed");
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
        assert!(d.graph.reachable(&[e])[fits as usize], "the sum's NoWrap is owed");
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
