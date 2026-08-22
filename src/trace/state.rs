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
use super::heap::{Heap, Root, ScopeId, Shape, TableId, Value};

pub struct State<D: Domain> {
    pub heap: Heap<D>,
    /// The globals table - always root 0, so canonical numbering starts
    /// from something both sides of a merge agree on.
    pub globals: TableId,
    /// The scope the interpreter is currently executing in.
    pub scope: ScopeId,
}

impl<D: Domain> Clone for State<D> {
    fn clone(&self) -> Self {
        State { heap: self.heap.clone(), globals: self.globals, scope: self.scope }
    }
}

impl<D: Domain> State<D> {
    pub fn roots(&self) -> Vec<Root> {
        vec![Root::Table(self.globals), Root::Scope(self.scope)]
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
pub fn merge<D: Domain>(
    d: &mut D,
    cond: &D::Bool,
    mut t: State<D>,
    mut f: State<D>,
) -> Result<Option<State<D>>> {
    t.gc();
    f.gc();
    if t.shape()? != f.shape()? {
        return Ok(None);
    }

    // Walk both heaps in the same canonical BFS order and rebuild. The ids
    // in the two heaps are unrelated - each arm allocated its own - so the
    // merged heap gets fresh ids in canonical order. Tables and scopes
    // draw from ONE id counter, so canonical index i is literally the id
    // object i gets, and no second renumbering pass is needed.
    let t_order = canonical_order(&t);
    let f_order = canonical_order(&f);
    if t_order.len() != f_order.len() {
        // Equal shapes should guarantee this; if it ever fires, the shape
        // is not describing what merging actually depends on.
        bail!("merge: canonical orders disagree after equal shapes");
    }
    let t_idx: BTreeMap<Root, u32> = index_of(&t_order);

    let mut heap: Heap<D> = Heap::default();
    for r in &t_order {
        match r {
            Root::Table(_) => {
                heap.new_table();
            }
            Root::Scope(_) => {
                heap.new_scope(None);
            }
        }
    }

    for (i, (rt, rf)) in t_order.iter().zip(f_order.iter()).enumerate() {
        match (rt, rf) {
            (Root::Table(a), Root::Table(b)) => {
                let (ta, tb) = (&t.heap.tables[a], &f.heap.tables[b]);
                let mut hash = BTreeMap::new();
                for (k, va) in &ta.hash {
                    let vb = tb.hash.get(k).ok_or_else(|| {
                        anyhow::anyhow!("merge: key {:?} missing after equal shapes", k)
                    })?;
                    hash.insert(k.clone(), join(d, cond, va, vb, &t_idx)?);
                }
                let mut arr = Vec::with_capacity(ta.arr.len());
                for (va, vb) in ta.arr.iter().zip(tb.arr.iter()) {
                    arr.push(join(d, cond, va, vb, &t_idx)?);
                }
                let tab = heap.tables.get_mut(&(i as u32)).unwrap();
                tab.hash = hash;
                tab.arr = arr;
            }
            (Root::Scope(a), Root::Scope(b)) => {
                let (sa, sb) = (&t.heap.scopes[a], &f.heap.scopes[b]);
                let mut vars = BTreeMap::new();
                for (k, va) in &sa.vars {
                    let vb = sb.vars.get(k).ok_or_else(|| {
                        anyhow::anyhow!("merge: local {:?} missing after equal shapes", k)
                    })?;
                    vars.insert(k.clone(), join(d, cond, va, vb, &t_idx)?);
                }
                let parent = sa.parent.map(|p| t_idx[&Root::Scope(p)]);
                let sc = heap.scopes.get_mut(&(i as u32)).unwrap();
                sc.vars = vars;
                sc.parent = parent;
            }
            _ => bail!("merge: canonical orders disagree in kind after equal shapes"),
        }
    }

    // roots() puts the globals table first and the current scope second,
    // and canonical numbering starts from roots, so these are fixed.
    Ok(Some(State { heap, globals: t_idx[&Root::Table(t.globals)], scope: t_idx[&Root::Scope(t.scope)] }))
}

/// Merge one slot. Only numbers and booleans can actually differ - the
/// shapes agreed about everything else - so this is where the `Sel` nodes
/// come from, and `Graph::fold` collapses the ones whose arms are equal,
/// which is the overwhelming majority.
fn join<D: Domain>(
    d: &mut D,
    cond: &D::Bool,
    a: &Value<D>,
    b: &Value<D>,
    idx: &BTreeMap<Root, u32>,
) -> Result<Value<D>> {
    Ok(match (a, b) {
        (Value::Num(x), Value::Num(y)) => Value::Num(d.sel_num(cond, x, y)),
        (Value::Bool(x), Value::Bool(y)) => Value::Bool(d.sel_bool(cond, x, y)),
        (Value::Table(x), Value::Table(_)) => Value::Table(idx[&Root::Table(*x)]),
        (Value::Func { body, env }, Value::Func { .. }) => {
            Value::Func { body: *body, env: idx[&Root::Scope(*env)] }
        }
        (Value::Nil, Value::Nil) => Value::Nil,
        (Value::Str(x), Value::Str(_)) => Value::Str(x.clone()),
        (Value::Builtin(x), Value::Builtin(_)) => Value::Builtin(x),
        (x, y) => bail!("merge: {:?} and {:?} after equal shapes", x, y),
    })
}

fn index_of(order: &[Root]) -> BTreeMap<Root, u32> {
    order.iter().enumerate().map(|(i, r)| (*r, i as u32)).collect()
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
                    for v in tab.hash.values().chain(tab.arr.iter()) {
                        match v {
                            Value::Table(x) => queue.push(Root::Table(*x)),
                            Value::Func { env, .. } => queue.push(Root::Scope(*env)),
                            _ => {}
                        }
                    }
                }
            }
            Root::Scope(sc) => {
                if let Some(scope) = s.heap.scopes.get(&sc) {
                    for v in scope.vars.values() {
                        match v {
                            Value::Table(x) => queue.push(Root::Table(*x)),
                            Value::Func { env, .. } => queue.push(Root::Scope(*env)),
                            _ => {}
                        }
                    }
                    if let Some(p) = scope.parent {
                        queue.push(Root::Scope(p));
                    }
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

        let m = merge(&mut d, &cond, s, f).unwrap().expect("same shape merges");
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

    /// Different shapes are different successors, not a merge failure.
    #[test]
    fn differing_shapes_do_not_merge() {
        let mut d = Symbolic::default();
        let mut s: State<Symbolic> = State { heap: Heap::default(), globals: 0, scope: 0 };
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
        assert!(merge(&mut d, &cond, s, f).unwrap().is_none());
    }

    /// GC before comparing: two states that reached the same place by
    /// different routes leave different garbage, and without collecting it
    /// they would look like different shapes and never merge.
    #[test]
    fn garbage_does_not_prevent_a_merge() {
        let mut d = Symbolic::default();
        let mut s: State<Symbolic> = State { heap: Heap::default(), globals: 0, scope: 0 };
        s.globals = s.heap.new_table();
        s.scope = s.heap.new_scope(None);
        let mut f = s.clone();
        // One arm allocated a temporary nothing points at.
        let _garbage = f.heap.new_table();
        assert_ne!(s.heap.tables.len(), f.heap.tables.len());

        let sym = d.graph.leaf(Op::Cell(1));
        let zero = d.num(P8::from_i16(0));
        let cond = d.compare(Cmp::Gt, &sym, &zero).unwrap();
        assert!(merge(&mut d, &cond, s, f).unwrap().is_some());
    }
}
