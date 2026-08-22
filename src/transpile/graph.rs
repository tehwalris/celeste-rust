//! The minimal graph IR (`plans/multi-output-fusion.md`, P1' stage 1).
//!
//! A member of a specialization set is a pure DAG:
//!
//! ```text
//! Node   = (op, [operand ids])
//! Leaf   = input cell | literal
//! Member = { outputs: cell -> node, valid: node }
//! ```
//!
//! Three things are DELIBERATELY absent, and their absence is the point:
//!
//! * **Lanes.** The graph describes ONE lane. `zn_splat`/`zb_splat`/
//!   `zi_splat` are 35% of the emitted steady kernel's nodes and carry no
//!   meaning - they only move a uniform value into vector-land. Uniformity
//!   is recovered afterwards by a forward pass from the leaves. It is also
//!   a source of spurious divergence between members: 20 of the 31
//!   divergence roots measured between the steady and pinned-dying members
//!   were splats and constants.
//! * **Effects.** No `&mut dp`. Validity is an ordinary boolean node:
//!   a guard is `valid := valid AND c`, and a select on an unknown
//!   condition is `valid := valid AND known(c)`. The graph is therefore
//!   purely functional, which is what makes optimizing over it tractable.
//! * **Types beyond bool/number.** Exactness (is this number a singleton?)
//!   and uniformity are DERIVED PROPERTIES of a value, propagated over the
//!   finished graph, not distinct node types. So `zn_mul` and `zi_mul_pos`
//!   are one op, as are `zsel_n`/`zsel_b`/`zsel_i` and `if c {a} else {b}`.

use std::collections::{BTreeMap, HashMap};

use anyhow::{bail, Result};

use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};

/// An abstract value for ONE lane.
///
/// Exactness and knownness live here, in the value, not in the type: an
/// exact number is a singleton interval, and a known boolean is `Some`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Val {
    Num(Pico8NumInterval),
    /// `None` = unknown, the abstract interpreter's third boolean.
    Bool(Option<bool>),
}

impl Val {
    pub fn exact_num(n: Pico8Num) -> Self {
        Val::Num(Pico8NumInterval::from_number(n))
    }
    /// The number this value is, if it is a number and it is exact.
    fn as_exact(self) -> Option<Pico8Num> {
        match self {
            Val::Num(i) => i.to_number(),
            Val::Bool(_) => None,
        }
    }
    fn as_num(self, op: &str) -> Result<Pico8NumInterval> {
        match self {
            Val::Num(i) => Ok(i),
            Val::Bool(_) => bail!("{}: expected a number, got a boolean", op),
        }
    }
    fn as_bool(self, op: &str) -> Result<Option<bool>> {
        match self {
            Val::Bool(b) => Ok(b),
            Val::Num(_) => bail!("{}: expected a boolean, got a number", op),
        }
    }
}

pub type NodeId = u32;

/// The op vocabulary. Semantic, not representational: one variant per
/// MEANING, however many emitted forms that meaning currently has.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Op {
    // ---- leaves ----
    /// A literal number, as raw 16.16 bit patterns for its low and high
    /// bound. An EXACT literal is the singleton `Const(x, x)` - exactness
    /// is a property of the value, not a separate kind of node, so the
    /// widened literals the emitter produces (e.g. sin of a non-exact
    /// input, which it emits as the constant [-1, 1]) need no special
    /// case. Raw bit patterns rather than `Pico8Num` so `Op` is `Hash`
    /// without imposing anything on the numeric type.
    Const(i32, i32),
    ConstBool(bool),
    /// An input cell. NOT split into uniform/per-lane: that is a derived
    /// property, computed after the graph exists.
    Cell(u32),
    /// One of the six button bits the suffix is specialized on.
    Button(u8),
    /// `Fork(d)` over one operand: the value narrowed to configuration
    /// `c{d}` of the d-th concretization fork. A value the program could
    /// not keep symbolic, so it runs once per possible outcome.
    Fork(u8),

    // ---- number -> number ----
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    Abs,
    Flr,
    Sin,
    Min,
    Max,

    // ---- number -> bool ----
    Lt,
    Le,
    Gt,
    Ge,
    Eq,

    // ---- bool -> bool ----
    Not,
    And,
    Or,

    /// `Sel(cond, then, else)` - the former `if`, in every width.
    Sel,

    /// `Known(b)`: is this abstract boolean decided? The operator that
    /// lets validity be an ordinary value instead of a side channel.
    Known,

    // ---- cart lookups ----
    Mget,
    TileFlagAt,

}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Node {
    pub op: Op,
    pub args: Vec<NodeId>,
}

/// A hash-consed arena. Interning is the whole sharing mechanism: two
/// nodes are the same node iff they are the same op over the same operand
/// ids, and operand ids are themselves interned, so identity composes
/// bottom-up.
#[derive(Default)]
pub struct Graph {
    nodes: Vec<Node>,
    intern: HashMap<Node, NodeId>,
}

impl Graph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, op: Op, args: Vec<NodeId>) -> NodeId {
        let node = Node { op, args };
        if let Some(id) = self.intern.get(&node) {
            return *id;
        }
        let id = self.nodes.len() as NodeId;
        self.nodes.push(node.clone());
        self.intern.insert(node, id);
        id
    }

    pub fn leaf(&mut self, op: Op) -> NodeId {
        self.add(op, Vec::new())
    }

    pub fn get(&self, id: NodeId) -> &Node {
        &self.nodes[id as usize]
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// For every node, which button bits can influence it - a bitmask,
    /// computed bottom-up in one pass (operands always precede their
    /// node). A button that reaches no OUTPUT cannot affect any lane's
    /// result, so the 2^6 button variants collapse by a factor of two for
    /// each such bit. That is a plain reachability fact, decided once for
    /// the whole kernel, not per lane.
    pub fn button_cones(&self) -> Vec<u8> {
        let mut mask = vec![0u8; self.nodes.len()];
        for (i, node) in self.nodes.iter().enumerate() {
            let mut m = match node.op {
                Op::Button(b) => 1u8 << b,
                _ => 0,
            };
            for a in &node.args {
                m |= mask[*a as usize];
            }
            mask[i] = m;
        }
        mask
    }

    /// Resolve an emitted operand spelling to a node: a name the emitter
    /// already bound, a witness field access (`u.cN` / `rin.cN`), or a
    /// literal. Anything else is an error - silently inventing a node here
    /// would make the graph disagree with the program it claims to model.
    pub fn operand(&mut self, s: &str, named: &HashMap<String, NodeId>) -> Result<NodeId> {
        if let Some(id) = named.get(s) {
            return Ok(*id);
        }
        // Uniform cells read from the witness binding, per-lane columns read
        // from the row struct, and the `let r_cN = ...` loads the emitter
        // hoists for them. All three are the same thing to the graph: an
        // input cell. Which of them is uniform is a DERIVED property.
        for p in ["u.c", "rin.c", "r_c"] {
            if let Some(rest) = s.strip_prefix(p) {
                if let Ok(cell) = rest.parse::<u32>() {
                    return Ok(self.leaf(Op::Cell(cell)));
                }
            }
        }
        // kb0..kb5: the button bits the suffix is specialized on.
        if let Some(rest) = s.strip_prefix("kb") {
            if let Ok(bit) = rest.parse::<u8>() {
                return Ok(self.leaf(Op::Button(bit)));
            }
        }
        if let Some(rest) = s.strip_prefix("P8::from_raw(") {
            if let Some(num) = rest.strip_suffix("i32)") {
                if let Ok(raw) = num.parse::<i32>() {
                    return Ok(self.leaf(Op::Const(raw, raw)));
                }
            }
        }
        if let Some(rest) = s.strip_prefix("P8::from_i16(") {
            if let Some(num) = rest.strip_suffix(')') {
                if let Ok(v) = num.parse::<i16>() {
                    let raw = (v as i32) << 16;
                    return Ok(self.leaf(Op::Const(raw, raw)));
                }
            }
        }
        match s {
            "true" => return Ok(self.leaf(Op::ConstBool(true))),
            "false" => return Ok(self.leaf(Op::ConstBool(false))),
            _ => {}
        }
        bail!("operand {:?} is not a bound name, a witness cell or a literal", s)
    }

    /// Evaluate `roots` given the input cells, in one pass over the arena.
    /// Nodes are appended after their operands, so a forward sweep is a
    /// valid evaluation order.
    pub fn eval(&self, cells: &HashMap<u32, Val>) -> Result<Vec<Val>> {
        let mut out: Vec<Val> = Vec::with_capacity(self.nodes.len());
        for (i, node) in self.nodes.iter().enumerate() {
            let a = |k: usize| -> Val { out[node.args[k] as usize] };
            let v = match &node.op {
                Op::Const(lo, hi) => Val::Num(Pico8NumInterval::new(
                    Pico8Num::from_raw(*lo),
                    Pico8Num::from_raw(*hi),
                )),
                Op::ConstBool(b) => Val::Bool(Some(*b)),
                Op::Button(b) => bail!("node {}: button bit {} has no value outside a variant", i, b),
                Op::Fork(d) => bail!("node {}: fork {} has no value outside a configuration", i, d),
                Op::Cell(c) => match cells.get(c) {
                    Some(v) => *v,
                    None => bail!("node {}: input cell {} was not supplied", i, c),
                },
                Op::Add => Val::Num(a(0).as_num("Add")? + a(1).as_num("Add")?),
                Op::Sub => Val::Num(a(0).as_num("Sub")? - a(1).as_num("Sub")?),
                Op::Neg => {
                    let x = a(0).as_num("Neg")?;
                    Val::Num(Pico8NumInterval::new(-x.high, -x.low))
                }
                Op::Mul | Op::Div | Op::Rem => Self::arith(&node.op, a(0), a(1))?,
                Op::Abs => {
                    let x = a(0).as_num("Abs")?;
                    let zero = Pico8Num::from_i16(0);
                    if x.low >= zero {
                        Val::Num(x)
                    } else if x.high <= zero {
                        Val::Num(Pico8NumInterval::new(-x.high, -x.low))
                    } else {
                        // Straddles zero: the result is [0, max(|lo|,|hi|)].
                        let m = if -x.low > x.high { -x.low } else { x.high };
                        Val::Num(Pico8NumInterval::new(zero, m))
                    }
                }
                // flr is monotone, so endpoints suffice.
                Op::Flr => {
                    let x = a(0).as_num("Flr")?;
                    Val::Num(Pico8NumInterval::new(x.low.flr(), x.high.flr()))
                }
                // sin is NOT monotone; only exact inputs are sound here.
                Op::Sin => match a(0).as_exact() {
                    Some(n) => Val::exact_num(n.pico8_sin()),
                    None => bail!("Sin over a non-exact interval is not modelled"),
                },
                Op::Min | Op::Max => {
                    let (x, y) = (a(0).as_num("MinMax")?, a(1).as_num("MinMax")?);
                    let pick = |p: Pico8Num, q: Pico8Num| {
                        if matches!(node.op, Op::Min) {
                            if p < q { p } else { q }
                        } else if p > q {
                            p
                        } else {
                            q
                        }
                    };
                    Val::Num(Pico8NumInterval::new(pick(x.low, y.low), pick(x.high, y.high)))
                }
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => {
                    Self::compare(&node.op, a(0), a(1))?
                }
                Op::Not => Val::Bool(a(0).as_bool("Not")?.map(|b| !b)),
                Op::And | Op::Or => {
                    let (x, y) = (a(0).as_bool("And/Or")?, a(1).as_bool("And/Or")?);
                    let is_and = matches!(node.op, Op::And);
                    // Short-circuit on a decided absorbing element, so
                    // `false AND unknown` is false, not unknown.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(if is_and { p && q } else { p || q }),
                        (Some(p), None) | (None, Some(p)) => {
                            if is_and && !p {
                                Some(false)
                            } else if !is_and && p {
                                Some(true)
                            } else {
                                None
                            }
                        }
                        (None, None) => None,
                    })
                }
                Op::Sel => match a(0).as_bool("Sel")? {
                    Some(true) => a(1),
                    Some(false) => a(2),
                    // Undecided: the lane is invalid (its `valid` node
                    // carries `Known(cond)`), so any SOUND value serves.
                    // The join is the sound one.
                    None => Self::join(a(1), a(2))?,
                },
                Op::Known => Val::Bool(Some(a(0).as_bool("Known")?.is_some())),
                Op::Mget | Op::TileFlagAt => {
                    bail!("{:?} needs the cart; not supported by the pure evaluator yet", node.op)
                }
            };
            out.push(v);
        }
        Ok(out)
    }

    fn arith(op: &Op, x: Val, y: Val) -> Result<Val> {
        // Exact op exact is always exact.
        if let (Some(p), Some(q)) = (x.as_exact(), y.as_exact()) {
            return Ok(Val::exact_num(match op {
                Op::Mul => p * q,
                Op::Div => p / q,
                Op::Rem => p % q,
                _ => unreachable!(),
            }));
        }
        let xi = x.as_num("arith")?;
        let scalar = match y.as_exact() {
            Some(q) => q,
            None => bail!("{:?} of two non-exact intervals is not modelled", op),
        };
        // The interval helpers are monotone only for a positive scalar.
        if scalar <= Pico8Num::from_i16(0) {
            bail!("{:?} by a non-positive scalar is not modelled", op);
        }
        Ok(Val::Num(match op {
            Op::Mul => xi.scale_positive(scalar),
            Op::Div => xi.div_positive(scalar),
            Op::Rem => bail!("Rem over an interval is not modelled"),
            _ => unreachable!(),
        }))
    }

    fn compare(op: &Op, x: Val, y: Val) -> Result<Val> {
        // Booleans only compare with Eq.
        if let (Val::Bool(p), Val::Bool(q)) = (x, y) {
            if !matches!(op, Op::Eq) {
                bail!("{:?} is not defined on booleans", op);
            }
            return Ok(Val::Bool(match (p, q) {
                (Some(a), Some(b)) => Some(a == b),
                _ => None,
            }));
        }
        let (a, b) = (x.as_num("compare")?, y.as_num("compare")?);
        // Decided iff the answer is the same for every pair in the boxes.
        let all = |f: &dyn Fn(Pico8Num, Pico8Num) -> bool| -> Option<bool> {
            let corners = [
                f(a.low, b.low),
                f(a.low, b.high),
                f(a.high, b.low),
                f(a.high, b.high),
            ];
            // For the order comparisons the extremes bound the whole box;
            // equality needs the disjointness test below instead.
            if corners.iter().all(|c| *c) {
                Some(true)
            } else if corners.iter().all(|c| !*c) {
                Some(false)
            } else {
                None
            }
        };
        Ok(Val::Bool(match op {
            Op::Lt => all(&|p, q| p < q),
            Op::Le => all(&|p, q| p <= q),
            Op::Gt => all(&|p, q| p > q),
            Op::Ge => all(&|p, q| p >= q),
            Op::Eq => {
                if a.low > b.high || b.low > a.high {
                    Some(false) // disjoint
                } else if let (Some(p), Some(q)) = (a.to_number(), b.to_number()) {
                    Some(p == q)
                } else {
                    None // overlapping, at least one inexact
                }
            }
            _ => unreachable!(),
        }))
    }

    fn join(x: Val, y: Val) -> Result<Val> {
        Ok(match (x, y) {
            (Val::Num(a), Val::Num(b)) => Val::Num(a.union(&b)),
            (Val::Bool(a), Val::Bool(b)) => Val::Bool(if a == b { a } else { None }),
            _ => bail!("cannot join a number with a boolean"),
        })
    }
}

/// One member of a specialization set: what it writes, and whether the
/// result counts. That is the whole interface.
pub struct Member {
    pub label: String,
    pub outputs: BTreeMap<u32, NodeId>,
    pub valid: NodeId,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn n(v: i16) -> Pico8Num {
        Pico8Num::from_i16(v)
    }

    #[test]
    fn interning_shares_identical_nodes() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let s1 = g.add(Op::Add, vec![a, b]);
        let s2 = g.add(Op::Add, vec![a, b]);
        assert_eq!(s1, s2, "identical op over identical operands is one node");
        assert_eq!(g.len(), 3);
        // Operand order matters: Add is commutative in VALUE but this is
        // structural interning, which is exactly the property that made
        // the pinned member fail to fuse.
        let s3 = g.add(Op::Add, vec![b, a]);
        assert_ne!(s1, s3);
    }

    #[test]
    fn evaluates_exact_arithmetic() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let k = g.leaf(Op::Const(n(3).as_raw_u32() as i32, n(3).as_raw_u32() as i32));
        let sum = g.add(Op::Add, vec![a, k]);
        let cells = HashMap::from([(1u32, Val::exact_num(n(4)))]);
        let out = g.eval(&cells).unwrap();
        assert_eq!(out[sum as usize], Val::exact_num(n(7)));
    }

    #[test]
    fn comparison_of_overlapping_intervals_is_unknown() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let lt = g.add(Op::Lt, vec![a, b]);
        // [0,10] < [5,15] is neither always true nor always false.
        let cells = HashMap::from([
            (1u32, Val::Num(Pico8NumInterval::new(n(0), n(10)))),
            (2u32, Val::Num(Pico8NumInterval::new(n(5), n(15)))),
        ]);
        assert_eq!(g.eval(&cells).unwrap()[lt as usize], Val::Bool(None));
        // Disjoint boxes decide it.
        let cells = HashMap::from([
            (1u32, Val::Num(Pico8NumInterval::new(n(0), n(3)))),
            (2u32, Val::Num(Pico8NumInterval::new(n(5), n(15)))),
        ]);
        assert_eq!(g.eval(&cells).unwrap()[lt as usize], Val::Bool(Some(true)));
    }

    #[test]
    fn validity_is_an_ordinary_node() {
        // The whole point of P1': a guard is `valid AND c`, and a select on
        // an undecided condition is `valid AND known(c)`. No side channel.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let t = g.leaf(Op::Const(n(1).as_raw_u32() as i32, n(1).as_raw_u32() as i32));
        let f = g.leaf(Op::Const(n(2).as_raw_u32() as i32, n(2).as_raw_u32() as i32));
        let sel = g.add(Op::Sel, vec![c, t, f]);
        let known = g.add(Op::Known, vec![c]);
        let all = g.leaf(Op::ConstBool(true));
        let valid = g.add(Op::And, vec![all, known]);

        // Decided condition: the lane is valid and picks an arm.
        let cells = HashMap::from([(1u32, Val::Bool(Some(true)))]);
        let out = g.eval(&cells).unwrap();
        assert_eq!(out[valid as usize], Val::Bool(Some(true)));
        assert_eq!(out[sel as usize], Val::exact_num(n(1)));

        // Undecided: the lane is INVALID, and the value is the sound join.
        let cells = HashMap::from([(1u32, Val::Bool(None))]);
        let out = g.eval(&cells).unwrap();
        assert_eq!(out[valid as usize], Val::Bool(Some(false)));
        assert_eq!(
            out[sel as usize],
            Val::Num(Pico8NumInterval::new(n(1), n(2)))
        );
    }

    #[test]
    fn and_short_circuits_through_unknown() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let and = g.add(Op::And, vec![a, b]);
        // false AND unknown is FALSE, not unknown - precision that matters
        // because validity is built out of these.
        let cells = HashMap::from([(1u32, Val::Bool(Some(false))), (2u32, Val::Bool(None))]);
        assert_eq!(g.eval(&cells).unwrap()[and as usize], Val::Bool(Some(false)));
    }

    #[test]
    fn unmodelled_operations_are_loud() {
        // Correctness beats cleverness: an op we cannot evaluate EXACTLY
        // must error, never return an approximation.
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        let m = g.add(Op::Mul, vec![a, b]);
        let _ = m;
        let cells = HashMap::from([
            (1u32, Val::Num(Pico8NumInterval::new(n(0), n(10)))),
            (2u32, Val::Num(Pico8NumInterval::new(n(2), n(3)))),
        ]);
        let err = g.eval(&cells).unwrap_err().to_string();
        assert!(err.contains("not modelled"), "unexpected error: {}", err);
    }
}
