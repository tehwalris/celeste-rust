//! A traced state, and the operation the whole design rests on: MERGE.
//!
//! At a branch the tracer cannot decide, it runs both arms from a copy of
//! the state and then tries to put the results back together. Two states
//! whose SHAPES agree - same objects, same fields, same lengths, same
//! closures - differ only in symbolic values, so they merge into one state
//! with a `Sel` on the branch condition at every slot that differs. Slots
//! that agree stay one node, which `Graph::fold` does for free and which
//! is what stops a per-cell merge from costing a select per heap cell.
//!
//! Two states whose shapes DIFFER are genuinely different successors and
//! both survive. That is not a failure case: it is how a frame that kills
//! the player produces two output shapes without anyone having to write a
//! specialization set to express it.

use std::collections::BTreeMap;

use anyhow::{bail, Result};

use super::domain::Domain;
use super::heap::{push_value, Heap, Root, ScopeId, Shape, TableId, Value};

pub struct State<D: Domain> {
    pub heap: Heap<D>,
    /// The globals table - always root 0, so canonical numbering starts
    /// from something both sides of a merge agree on.
    pub globals: TableId,
    /// The scope the interpreter is currently executing in.
    pub scope: ScopeId,
    /// The scopes of the callers waiting on this one - the CALL STACK.
    ///
    /// They have to be GC roots. A callee's frame has the closure's
    /// captured scope as its parent, NOT the caller's, so the caller's
    /// scope is unreachable from the current one. Without this a merge
    /// inside a call collects the scope the caller is about to return to,
    /// and the restored id points at nothing - which showed up as `this`
    /// being nil in a function whose body had branched.
    pub stack: Vec<ScopeId>,
    /// WHEN does this state apply? One boolean, built up as `g and c` /
    /// `g and not c` at every branch and OR-ed back together at every
    /// merge.
    ///
    /// Downstream it is the emitted lane mask (`Emit::live`). Nothing
    /// decides anything by looking inside it - an earlier design carried
    /// a list of assumed literals so that a re-tested condition could be
    /// decided syntactically; measuring showed it firing 10 times in 25
    /// frames, and all 10 disappeared once `and`/`or` stopped handing on
    /// the node they had just split on.
    ///
    /// It is NOT what a merge selects on. It used to be, and that put the
    /// whole path guard inside every merged value: `freeze' = Sel(guard,
    /// 2, freeze)` where `guard` was three levels of `(A & p) | (A & !p)`
    /// from the dash block's direction arms, a different node per button
    /// combination, so one value that only ever takes two forms was 37
    /// distinct nodes and 37 emitted bodies (plans/graph-audit.md). The
    /// merge now selects on the one decision in `path` that separates
    /// the two arms.
    ///
    /// INVARIANT: the guards of the outcomes in one frontier are pairwise
    /// DISJOINT. Every fan-out is a split on some condition, so this holds
    /// by construction - and `merge` relies on it: on a lane where the
    /// merged guard holds, exactly one side's guard holds, and the
    /// separating decision is KNOWN there, so the select picks that side.
    pub guard: D::Bool,
    pub ok: D::Bool,
    /// The branch decisions this state took since the frame started, in
    /// order: `(condition, which way)`. `split` pushes one; `merge` keeps
    /// the common prefix of the two sides.
    ///
    /// Its one job is to find the condition a merge should select on.
    /// Two states being merged diverged at exactly one split, and
    /// everything before it is common to both, so the first entry where
    /// their paths differ IS that split: the same condition, taken both
    /// ways. Selecting on it, rather than on the full guard, keeps the
    /// guard algebra out of the value layer - on the lanes where the
    /// merged state applies the two choices agree everywhere else.
    ///
    /// The conjunction of the path's literals is NOT the guard: a fork
    /// (`__split_by_flr`) narrows `guard` by its validity without a
    /// split, and merging ORs two guards together. The guard stays the
    /// authority on WHEN; the path only says WHICH WAY.
    pub path: Vec<(D::Bool, bool)>,
}

/// How many leading decisions two states share.
pub fn common_prefix<D: Domain>(a: &State<D>, b: &State<D>) -> usize {
    a.path.iter().zip(b.path.iter()).take_while(|(x, y)| x == y).count()
}

/// The order in which `collapse` should TRY to merge pairs: every
/// joinable `(i, j)` with `i < j`, siblings first.
///
/// Two states that diverged at the most recent split share the longest
/// decision prefix, and merging them first is what lets `merge` find a
/// single separating decision to select on. Merging a state with its
/// cousin before its sibling leaves a pair whose paths do not split at
/// one shared condition, and that pair has to fall back to the full
/// guard. Ties break on `(i, j)` so the order - and the graph - is
/// deterministic.
pub fn merge_order<D: Domain>(
    states: &[&State<D>],
    joinable: impl Fn(usize, usize) -> bool,
) -> Vec<(usize, usize)> {
    let mut pairs: Vec<(usize, usize, usize)> = Vec::new();
    for i in 0..states.len() {
        for j in (i + 1)..states.len() {
            if joinable(i, j) {
                pairs.push((common_prefix(states[i], states[j]), i, j));
            }
        }
    }
    pairs.sort_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)).then(a.2.cmp(&b.2)));
    pairs.into_iter().map(|(_, i, j)| (i, j)).collect()
}

/// What `merge` produced.
pub struct Merged<D: Domain> {
    pub state: State<D>,
    /// The condition every merged value selects on - true on `t`'s lanes.
    pub cond: D::Bool,
    /// The two paths did not diverge at one shared decision (they can
    /// fail to when `collapse` pairs two states that are not siblings),
    /// so `cond` is `t.guard`, the always-correct choice the merge used
    /// to make unconditionally. Counted so its frequency is a number.
    pub fell_back: bool,
}

impl<D: Domain> Clone for State<D> {
    fn clone(&self) -> Self {
        State {
            heap: self.heap.clone(),
            globals: self.globals,
            scope: self.scope,
            stack: self.stack.clone(),
            guard: self.guard.clone(),
            ok: self.ok.clone(),
            path: self.path.clone(),
        }
    }
}

impl<D: Domain> State<D> {
    pub fn roots(&self) -> Vec<Root> {
        let mut r = vec![Root::Table(self.globals), Root::Scope(self.scope)];
        r.extend(self.stack.iter().map(|s| Root::Scope(*s)));
        r
    }
    pub fn gc(&mut self) {
        let r = self.roots();
        self.heap.gc(&r);
    }
    pub fn shape(&self) -> Result<Shape> {
        self.heap.shape(&self.roots())
    }
}

/// Merge `t` (condition true) and `f` (condition false) if their shapes
/// allow it. `None` means they are different successors.
///
/// Both sides are GC'd first, because two states that did the same thing
/// by different routes leave different garbage behind and would otherwise
/// look like different shapes.
/// Merge two states. The condition is `t`'s own guard - the merged state
/// happens when EITHER side would have, and picks t's value exactly where
/// t applies. This needs the two guards to be disjoint; see the invariant
/// on `State::guard`.
pub fn merge<D: Domain>(
    d: &mut D,
    mut t: State<D>,
    mut f: State<D>,
) -> Result<Option<Merged<D>>> {
    t.gc();
    f.gc();
    if t.shape()? != f.shape()? {
        return Ok(None);
    }

    // The decision that separates the two sides - see `State::path`.
    let l = common_prefix(&t, &f);
    let (cond, fell_back) = match (t.path.get(l), f.path.get(l)) {
        (Some((ct, pt)), Some((cf, pf))) if ct == cf && pt != pf => {
            (if *pt { ct.clone() } else { d.not(ct) }, false)
        }
        _ => (t.guard.clone(), true),
    };
    let cond = &cond;
    let path: Vec<(D::Bool, bool)> = t.path[..l].to_vec();

    // Rebuild into the T SIDE'S NUMBERING, not a fresh canonical one.
    //
    // Both sides descend from the same pre-branch state, so every id the
    // CALLER is holding - the scope to return to, a table it is midway
    // through building - is valid in `t` and unchanged there. Renumbering
    // into a fresh canonical space invalidated all of them, which showed
    // up as `this` being nil inside a function whose body had merged: the
    // caller restored a scope id that no longer existed.
    //
    // Canonical order is still what PAIRS the two sides, so allocation
    // history cannot make equal states look different. It just is not
    // what the result is numbered by.
    let t_order = canonical_order(&t);
    let f_order = canonical_order(&f);
    if t_order.len() != f_order.len() {
        // Equal shapes should guarantee this; if it ever fires, the shape
        // is not describing what merging actually depends on.
        bail!("merge: canonical orders disagree after equal shapes");
    }

    let mut heap = t.heap.clone();
    for (rt, rf) in t_order.iter().zip(f_order.iter()) {
        match (rt, rf) {
            (Root::Table(a), Root::Table(b)) => {
                let tb = &f.heap.tables[b];
                let keys: Vec<String> = heap.tables[a].hash.keys().cloned().collect();
                for k in keys {
                    let va = heap.tables[a].hash[&k].clone();
                    let vb = tb.hash.get(&k).ok_or_else(|| {
                        anyhow::anyhow!("merge: key {:?} missing after equal shapes", k)
                    })?;
                    let j = join(d, cond, &va, vb)?;
                    heap.tables.get_mut(a).unwrap().hash.insert(k, j);
                }
                for i in 0..heap.tables[a].arr.len() {
                    let va = heap.tables[a].arr[i].clone();
                    let j = join(d, cond, &va, &tb.arr[i])?;
                    heap.tables.get_mut(a).unwrap().arr[i] = j;
                }
                let ikeys: Vec<i16> = heap.tables[a].ints.keys().copied().collect();
                for k in ikeys {
                    let va = heap.tables[a].ints[&k].clone();
                    let vb = tb.ints.get(&k).ok_or_else(|| {
                        anyhow::anyhow!("merge: index {} missing after equal shapes", k)
                    })?;
                    let j = join(d, cond, &va, vb)?;
                    heap.tables.get_mut(a).unwrap().ints.insert(k, j);
                }
            }
            (Root::Scope(a), Root::Scope(b)) => {
                let sb = &f.heap.scopes[b];
                let keys: Vec<String> = heap.scopes[a].vars.keys().cloned().collect();
                for k in keys {
                    let va = heap.scopes[a].vars[&k].clone();
                    let vb = sb.vars.get(&k).ok_or_else(|| {
                        anyhow::anyhow!("merge: local {:?} missing after equal shapes", k)
                    })?;
                    let j = join(d, cond, &va, vb)?;
                    heap.scopes.get_mut(a).unwrap().vars.insert(k, j);
                }
            }
            // A closure has no mutable content - which body it is and
            // what it captured are both fixed at creation - so pairing
            // the two sides is all there is to do. It has to be in the
            // traversal so the scope it captured gets visited.
            (Root::Closure(_), Root::Closure(_)) => {}
            _ => bail!("merge: canonical orders disagree in kind after equal shapes"),
        }
    }

    let state = State {
        heap,
        globals: t.globals,
        scope: t.scope,
        stack: t.stack.clone(),
        guard: d.or(&t.guard, &f.guard),
        // Obligations are per-CASE: a lane only has to satisfy what the
        // arm it actually took required, so this selects rather than
        // conjoining. Conjoining would be sound but would deopt lanes for
        // an obligation incurred on a path they did not take.
        ok: d.sel_bool(cond, &t.ok, &f.ok),
        path,
    };
    Ok(Some(Merged { state, cond: cond.clone(), fell_back }))
}

/// Merge one slot. Only numbers and booleans can actually differ - the
/// shapes agreed about everything else - so this is where the `Sel` nodes
/// come from, and `Graph::fold` collapses the ones whose arms are equal,
/// which is the overwhelming majority.
fn join<D: Domain>(d: &mut D, cond: &D::Bool, a: &Value<D>, b: &Value<D>) -> Result<Value<D>> {
    Ok(match (a, b) {
        (Value::Num(x), Value::Num(y)) => Value::Num(d.sel_num(cond, x, y)),
        (Value::Bool(x), Value::Bool(y)) => Value::Bool(d.sel_bool(cond, x, y)),
        // The shapes agreed about everything else, and the result keeps
        // the t side's structure, so t's own value is already right.
        (x, Value::Table(_) | Value::Func(_) | Value::Nil | Value::Str(_)
            | Value::Builtin(_)) => x.clone(),
        (x, y) => bail!("merge: {:?} and {:?} after equal shapes", x, y),
    })
}

/// Canonical BFS order of the reachable objects. `roots()` seeds it with
/// the globals table and the current scope, so two heaps built by
/// different allocation histories still number the same objects the same.
fn canonical_order<D: Domain>(s: &State<D>) -> Vec<Root> {

    let mut queue: Vec<Root> = s.roots();
    let mut seen: BTreeMap<Root, u32> = BTreeMap::new();
    let mut order: Vec<Root> = Vec::new();
    let mut i = 0;
    while i < queue.len() {
        let r = queue[i];
        i += 1;
        if seen.contains_key(&r) {
            continue;
        }
        seen.insert(r, order.len() as u32);
        order.push(r);
        match r {
            Root::Table(t) => {
                if let Some(tab) = s.heap.tables.get(&t) {
                    for v in tab.values() {
                        push_value(v, &mut queue);
                    }
                }
            }
            Root::Scope(sc) => {
                if let Some(scope) = s.heap.scopes.get(&sc) {
                    for v in scope.vars.values() {
                        push_value(v, &mut queue);
                    }
                    if let Some(p) = scope.parent {
                        queue.push(Root::Scope(p));
                    }
                }
            }
            Root::Closure(c) => {
                if let Some(cl) = s.heap.closures.get(&c) {
                    queue.push(Root::Scope(cl.env));
                }
            }
        }
    }
    order
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num as P8;
    use crate::trace::domain::{Cmp, Symbolic};
    use crate::transpile::graph::Op;

    /// Two states that differ in one field merge into one state with one
    /// select, and everything they agree on stays a single node.
    #[test]
    fn merging_costs_a_select_only_where_the_arms_disagree() {
        let mut d = Symbolic::default();
        let mut s: State<Symbolic> = State {
            heap: Heap::default(),
            globals: 0,
            scope: 0,
            stack: Vec::new(),
            guard: d.boolean(true),
            ok: d.boolean(true),
            path: Vec::new(),
        };
        s.globals = s.heap.new_table();
        s.scope = s.heap.new_scope(None);
        let player = s.heap.new_table();
        s.heap.tables.get_mut(&s.globals).unwrap().hash.insert("p".into(), Value::Table(player));

        let same = d.num(P8::from_i16(7));
        let a = d.num(P8::from_i16(1));
        let b = d.num(P8::from_i16(2));
        {
            let t = s.heap.tables.get_mut(&player).unwrap();
            t.hash.insert("untouched".into(), Value::Num(same));
            t.hash.insert("x".into(), Value::Num(a));
        }
        let mut f = s.clone();
        f.heap.tables.get_mut(&player).unwrap().hash.insert("x".into(), Value::Num(b));

        // An undecided condition, as a real branch would have.
        let sym = d.graph.leaf(Op::Cell(1));
        let zero = d.num(P8::from_i16(0));
        let cond = d.compare(Cmp::Gt, &sym, &zero).unwrap();
        assert_eq!(d.decide(&cond), None);
        // A real branch leaves each side guarded by the literal it took.
        s.guard = cond.clone();
        f.guard = d.not(&cond);

        let m = merge(&mut d, s, f).unwrap().expect("same shape merges").state;
        let player_m = match m.heap.tables[&m.globals].hash["p"] {
            Value::Table(t) => t,
            ref v => panic!("expected a table, got {:?}", v),
        };
        let tab = &m.heap.tables[&player_m];
        // The field they agreed on is the SAME node, not a select of it
        // with itself - this is what stops a per-cell merge costing a
        // select per heap cell.
        assert_eq!(tab.hash["untouched"], Value::Num(same));
        // The field they disagreed on became one.
        let x = match tab.hash["x"] {
            Value::Num(n) => n,
            ref v => panic!("expected a number, got {:?}", v),
        };
        assert_eq!(d.graph.get(x).op, Op::Sel);
        assert_eq!(d.graph.get(x).args, vec![cond, a, b]);
    }

    /// The merge selects on the DECISION that separates the two sides,
    /// not on their full guards. Here both sides sit under an outer
    /// guard `g`; the select must name `c`, and the merged guard must
    /// still be the OR of the two guards.
    #[test]
    fn a_merge_selects_on_the_separating_decision_not_the_guard() {
        let mut d = Symbolic::default();
        let mut s: State<Symbolic> = State { heap: Heap::default(), globals: 0, scope: 0, stack: Vec::new(), guard: d.boolean(true), ok: d.boolean(true), path: Vec::new() };
        s.globals = s.heap.new_table();
        s.scope = s.heap.new_scope(None);
        let a = d.num(P8::from_i16(1));
        let b = d.num(P8::from_i16(2));
        s.heap.tables.get_mut(&s.globals).unwrap().hash.insert("x".into(), Value::Num(a));
        let zero = d.num(P8::from_i16(0));
        let cell1 = d.graph.leaf(Op::Cell(1));
        let cell2 = d.graph.leaf(Op::Cell(2));
        let g = d.compare(Cmp::Gt, &cell1, &zero).unwrap();
        let c = d.compare(Cmp::Gt, &cell2, &zero).unwrap();
        s.guard = g;
        s.path.push((g, true));
        let (t, f) = split(&mut d, s, &c);
        let (mut t, f) = (t.unwrap(), f.unwrap());
        t.heap.tables.get_mut(&t.globals).unwrap().hash.insert("x".into(), Value::Num(b));
        let (tg, fg) = (t.guard, f.guard);

        let m = merge(&mut d, t, f).unwrap().expect("same shape merges");
        assert!(!m.fell_back, "siblings share a split, so no fallback");
        assert_eq!(m.cond, c, "the select condition is the branch condition");
        let x = match m.state.heap.tables[&m.state.globals].hash["x"] {
            Value::Num(n) => n,
            ref v => panic!("expected a number, got {:?}", v),
        };
        assert_eq!(d.graph.get(x).args, vec![c, b, a]);
        assert_eq!(m.state.guard, d.or(&tg, &fg), "the guard is still the OR of both sides");
        assert_eq!(m.state.path, vec![(g, true)], "the merged path is the common prefix");

        // Two states whose paths do not diverge at one shared decision
        // fall back to the full guard, which is always correct.
        let mut p: State<Symbolic> = State { heap: Heap::default(), globals: 0, scope: 0, stack: Vec::new(), guard: g, ok: d.boolean(true), path: vec![(g, true)] };
        p.globals = p.heap.new_table();
        p.scope = p.heap.new_scope(None);
        let mut q = p.clone();
        q.guard = c;
        q.path = vec![(c, false)];
        let m = merge(&mut d, p, q).unwrap().expect("same shape merges");
        assert!(m.fell_back);
        assert_eq!(m.cond, g);
    }

    /// Different shapes are different successors, not a merge failure.
    #[test]
    fn differing_shapes_do_not_merge() {
        let mut d = Symbolic::default();
        let mut s: State<Symbolic> = State { heap: Heap::default(), globals: 0, scope: 0, stack: Vec::new(), guard: d.boolean(true), ok: d.boolean(true), path: Vec::new() };
        s.globals = s.heap.new_table();
        s.scope = s.heap.new_scope(None);
        let obj = s.heap.new_table();
        s.heap.tables.get_mut(&s.globals).unwrap().arr.push(Value::Table(obj));

        // The other arm killed it - the array is shorter, so the heaps
        // are not the same shape and both states survive.
        let mut f = s.clone();
        f.heap.tables.get_mut(&f.globals).unwrap().arr.clear();

        let sym = d.graph.leaf(Op::Cell(1));
        let zero = d.num(P8::from_i16(0));
        let cond = d.compare(Cmp::Gt, &sym, &zero).unwrap();
        s.guard = cond.clone();
        f.guard = d.not(&cond);
        assert!(merge(&mut d, s, f).unwrap().is_none());
    }

    /// GC before comparing: two states that reached the same place by
    /// different routes leave different garbage, and without collecting it
    /// they would look like different shapes and never merge.
    #[test]
    fn garbage_does_not_prevent_a_merge() {
        let mut d = Symbolic::default();
        let mut s: State<Symbolic> = State { heap: Heap::default(), globals: 0, scope: 0, stack: Vec::new(), guard: d.boolean(true), ok: d.boolean(true), path: Vec::new() };
        s.globals = s.heap.new_table();
        s.scope = s.heap.new_scope(None);
        let mut f = s.clone();
        // One arm allocated a temporary nothing points at.
        let _garbage = f.heap.new_table();
        assert_ne!(s.heap.tables.len(), f.heap.tables.len());

        let sym = d.graph.leaf(Op::Cell(1));
        let zero = d.num(P8::from_i16(0));
        let cond = d.compare(Cmp::Gt, &sym, &zero).unwrap();
        s.guard = cond.clone();
        f.guard = d.not(&cond);
        assert!(merge(&mut d, s, f).unwrap().is_some());
    }
}

/// Split a state on an undecided condition. Each side's guard picks up
/// the literal; a side whose guard folds to false is DEAD and is not
/// returned, so the caller never explores it.
pub fn split<D: Domain>(
    d: &mut D,
    s: State<D>,
    cond: &D::Bool,
) -> (Option<State<D>>, Option<State<D>>) {
    let ncond = d.not(cond);
    let gt = d.and(&s.guard, cond);
    let gf = d.and(&s.guard, &ncond);
    let live = |d: &D, g: &D::Bool| d.decide(g) != Some(false);
    let t = if live(d, &gt) {
        let mut x = s.clone();
        x.guard = gt;
        x.path.push((cond.clone(), true));
        Some(x)
    } else {
        None
    };
    let f = if live(d, &gf) {
        let mut x = s;
        x.guard = gf;
        x.path.push((cond.clone(), false));
        Some(x)
    } else {
        None
    };
    (t, f)
}
