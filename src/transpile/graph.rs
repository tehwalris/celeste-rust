//! The graph IR: one traced frame as a pure, hash-consed DAG of
//! `(op, [operand ids])` over input cells and literals. It describes ONE
//! lane (uniformity is derived later), has no effects (a body's `live` and
//! `error` are boolean nodes), and no types beyond bool/number: exactness is
//! a property of the value, so one op covers every width.

use std::collections::HashMap;

use std::sync::Arc;

use anyhow::{anyhow, bail, Result};

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};

/// An abstract value for ONE lane: an exact number is a singleton interval,
/// a known boolean is `Some`.
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

/// In a `splits` vector: leave this fork standing.
pub const OPEN: u8 = u8::MAX;

/// OR-ed into a `splits` fragment index: that fragment with validity TRUE, for
/// a DEAD fork whose fragments all give the same row (the OR of their
/// validities is the lane covered).
pub const ANY_VALID: u8 = 0x80;

/// The map, for the evaluator (`CollisionCache` knows its room).
#[derive(Clone)]
pub struct Room {
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
}

/// The op vocabulary: one variant per MEANING, not per emitted form.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Op {
    // ---- leaves ----
    /// A literal: raw 16.16 low and high bounds (raw so `Op` is `Hash`); an
    /// exact literal is `Const(x, x)`.
    Const(i32, i32),
    ConstBool(bool),
    /// An input cell.
    Cell(u32),
    /// `Split(d)`: the operand RESTRICTED to fork d's outcome, eliminated by
    /// specialization.
    Split(u8),

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
    /// `Span(lo, hi)`: from `lo`'s low to `hi`'s high bound, for a widening
    /// with per-lane bounds (a fruit's bob band). Exactly the hull; containment
    /// is the widening's own error.
    Span,
    /// `Restrict(lo, hi)(x)`: `x`, ASSUMED to lie in `[lo, hi]` (raw 16.16,
    /// inclusive). A bounded kernel input (the region's square, speed and
    /// remainder) enters the body only through one, so the range analyses
    /// (`eval`'s hull, `pieces_of`) take the range from this node and never
    /// from a seeded environment. The kernel computes `x` unchanged; the
    /// assumption is the node's OWN ERROR, `Lo(x) < lo or Hi(x) > hi` on the
    /// raw operand (`trace::error`), charged to every outcome of the frame,
    /// because a comparison the range decided no longer reads the node.
    Restrict(i32, i32),
    /// THE UNKNOWN NUMBER. Not the full-range interval (interval arithmetic
    /// at the extremes raises): it stays unknown under every operation and a
    /// comparison with it is an `UnknownBool`. It never reaches a kernel: it
    /// is stored only as `AV::UNum`, and `bind` refuses a root reading it.
    UnknownNum,

    // ---- number -> bool ----
    Lt,
    Le,
    Gt,
    Ge,
    Eq,

    // ---- bool -> bool ----
    Not,
    /// Tri-state AND: a decided `false` wins over an unknown.
    And,
    /// Tri-state OR: a decided `true` wins (its own node so `a or b` and
    /// `b or a` intern alike).
    Or,
    /// An UNDECIDED boolean, the same in every lane. The index separates
    /// sites, so simplification never treats two independent unknowns as one.
    UnknownBool(u32),

    /// `Sel(cond, then, else)`, in every width.
    Sel,

    /// `Known(v)`: is the value DETERMINED? Survives only in derived errors.
    Known,
    /// `SplitValid(d)`: which lanes fall in the chosen fragment (fragments
    /// PARTITION the lanes). Dropped for forks every lane takes both ways.
    SplitValid(u8),
    /// `Split(d)` specialized: fragment `c` is the `c`-th grid cell from the
    /// one the low end lies in, clipped to the interval. `Frag` narrows to it,
    /// `FragOk` says whether the interval reaches it (together `zi_fork_flr`).
    /// Ordinary unary ops, so configurations SHARE every node they agree on.
    Frag(u8),
    FragOk(u8),
    /// A fork over WHOLE numbers whose fragments are the numbers, EXACT:
    /// resolves to `IntFrag(c)`, fragment `c`'s low end. Validity and premise
    /// are shared with `Split`.
    SplitInt(u8),
    IntFrag(u8),
    /// `SplitOk(n)`: does this lane's interval span at most `n` floors (the
    /// arity)? Otherwise the lane declines. Shared by every configuration.
    SplitOk(u8),
    /// `NoWrap(x)`, `x` an interval `+`, `-` or negation: did no endpoint
    /// OVERFLOW on this lane? The kernel wraps in i32, so an overflow is the
    /// operation's OWN ERROR and the lane declines. True of anything else.
    NoWrap,
    /// An interval's endpoints as plain numbers (of a number, the number), so
    /// a fork at a TABLE of cuts is ordinary arithmetic once specialized.
    Lo,
    Hi,
    // ---- cart lookups ----
    Mget,
    TileFlagAt,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Node {
    pub op: Op,
    pub args: Vec<NodeId>,
}

/// A hash-consed arena: same op over same operand ids is the same node.
#[derive(Default, Clone)]
pub struct Graph {
    nodes: Vec<Node>,
    /// rustc-hash 2 (1.1's Fx made probing dominate). Never iterated.
    intern: rustc_hash2::FxHashMap<Node, NodeId>,
    /// Per fork `d`, its fragment count (absent = 2).
    fork_ways: Vec<u8>,
    /// Each input cell's KIND, recorded where made and carried through every
    /// rebuild: the evaluator's weakest value for it. Not guessed from uses.
    cell_kinds: rustc_hash::FxHashMap<u32, CellKind>,
}

/// The kind of an input cell (`Graph::cell_kind`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CellKind {
    Num,
    Ival,
    Bool,
}

/// The most fragments one fork can have (a byte names a fragment).
pub const MAX_WAYS: usize = 255;

/// One fork-grid step (the integers) in raw 16.16 units.
const GRID_STEP: i32 = 1 << 16;

impl Graph {
    pub fn new() -> Self {
        Self::default()
    }

    /// An empty graph on the same fork grid and cell kinds.
    pub fn like(&self) -> Self {
        Graph {
            fork_ways: self.fork_ways.clone(),
            cell_kinds: self.cell_kinds.clone(),
            ..Default::default()
        }
    }

    pub fn set_cell_kind(&mut self, cell: u32, kind: CellKind) {
        self.cell_kinds.insert(cell, kind);
    }

    /// The recorded kind of `cell`; an undeclared cell is a number.
    pub fn cell_kind(&self, cell: u32) -> CellKind {
        self.cell_kinds.get(&cell).copied().unwrap_or(CellKind::Num)
    }

    /// The kinds, for a rebuild that renumbers the cells.
    pub fn cell_kinds(&self) -> impl Iterator<Item = (u32, CellKind)> + '_ {
        self.cell_kinds.iter().map(|(c, k)| (*c, *k))
    }

    /// Forget every fork's arity: a new frame's forks restart at 0.
    pub fn reset_forks(&mut self) {
        self.fork_ways.clear();
    }

    /// Fork `d`'s arity: how many fragments `Split(d)` resolves to.
    pub fn fork_ways(&self, d: u8) -> u8 {
        self.fork_ways.get(d as usize).copied().unwrap_or(2)
    }

    /// Give fork `d` at least `ways` fragments. Monotone: each site's own
    /// `SplitOk` bounds the lanes it admits.
    pub fn set_fork_ways(&mut self, d: u8, ways: u8) {
        debug_assert!(ways >= 1, "fork arity {ways}");
        if self.fork_ways.len() <= d as usize {
            self.fork_ways.resize(d as usize + 1, 2);
        }
        self.fork_ways[d as usize] = self.fork_ways[d as usize].max(ways);
    }

    /// The fork grid: one step in raw 16.16 and the mask flooring to it.
    fn grid(&self) -> (i32, i32) {
        (GRID_STEP, !(GRID_STEP - 1))
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

    /// Is `n` a fork over a LITERAL, one every lane takes both ways?
    pub fn is_literal_fork(&self, n: NodeId) -> bool {
        let node = self.get(n);
        matches!(node.op, Op::Split(_) | Op::SplitInt(_)) && matches!(self.get(node.args[0]).op, Op::Const(lo, hi) if lo != hi)
    }

    /// Rebuild into `out` with forks RESOLVED per `splits` (`OPEN` leaves one
    /// standing), over a SUBSET (`need[i]` false maps `i` to `UNBUILT`),
    /// folding as it goes; returns old -> new. `out` is shared and
    /// hash-consed, so configurations computing the same thing share node ids.
    /// Ids only refer downward, so one forward pass suffices.
    pub fn specialize_subset_into(&self, splits: &[u8], need: Option<&[bool]>, out: &mut Graph) -> Vec<NodeId> {
        out.fork_ways = self.fork_ways.clone();
        const UNBUILT: NodeId = NodeId::MAX;
        let mut map: Vec<NodeId> = Vec::with_capacity(self.nodes.len());
        for (i, node) in self.nodes.iter().enumerate() {
            if let Some(need) = need {
                if !need[i] {
                    map.push(UNBUILT);
                    continue;
                }
            }
            let arg = |map: &Vec<NodeId>, k: usize| map[node.args[k] as usize];
            let s = splits;
            let id = match node.op.clone() {
                Op::Split(d) if s[d as usize] != OPEN => out.fold(Op::Frag(s[d as usize] & !ANY_VALID), vec![arg(&map, 0)]),
                Op::SplitValid(d) if s[d as usize] != OPEN && s[d as usize] & ANY_VALID != 0 => out.leaf(Op::ConstBool(true)),
                Op::SplitValid(d) if s[d as usize] != OPEN => out.fold(Op::FragOk(s[d as usize]), vec![arg(&map, 0)]),
                Op::SplitInt(d) if s[d as usize] != OPEN => out.fold(Op::IntFrag(s[d as usize] & !ANY_VALID), vec![arg(&map, 0)]),
                _ => {
                    let args: Vec<NodeId> =
                        node.args.iter().map(|a| map[*a as usize]).collect();
                    out.fold(node.op.clone(), args)
                }
            };
            map.push(id);
        }
        map
    }

    /// Is operand order meaningless? Commuting ops get a canonical order since
    /// interning is the only sharing. Not `Mul`: `arith` is monotone only with
    /// the EXACT side second.
    fn commutes(op: &Op) -> bool {
        matches!(op, Op::Add | Op::Min | Op::Max | Op::Eq | Op::And | Op::Or)
    }

    /// Add `op(args)`, normalizing and folding. Every rule is EXACT w.r.t.
    /// `eval`, not merely sound: a refining rule (`x and not x` -> false)
    /// would change which lanes are in error
    /// (`folding_is_exact_not_merely_sound`).
    pub fn fold(&mut self, op: Op, mut args: Vec<NodeId>) -> NodeId {
        if args.len() == 2 {
            if Self::commutes(&op) {
                if args[0] > args[1] {
                    args.swap(0, 1);
                }
            } else if op == Op::Mul {
                let konst = |g: &Graph, a: NodeId| matches!(g.nodes[a as usize].op, Op::Const(..));
                if konst(self, args[0]) && !konst(self, args[1]) {
                    args.swap(0, 1);
                }
            }
        }
        let cbool = |g: &Graph, i: usize| -> Option<bool> {
            args.get(i).and_then(|a| match g.nodes[*a as usize].op {
                Op::ConstBool(b) => Some(b),
                _ => None,
            })
        };
        match op {
            // Fragment 0 is never empty, so `FragOk(0)` is true (as in `eval`),
            // making configuration 0 as cheap as no fork.
            Op::FragOk(0) => return self.leaf(Op::ConstBool(true)),
            // The span premise over a LITERAL is decided once (`zi_span_ok`).
            Op::SplitOk(n) => {
                if let Op::Const(lo, hi) = self.nodes[args[0] as usize].op {
                    // In the kernel's own i32 arithmetic, wrap included.
                    let step = GRID_STEP;
                    let (fl, fh) = (lo & !(step - 1), hi & !(step - 1));
                    let top = fl.wrapping_add(step.wrapping_mul(n as i32 - 1));
                    return self.leaf(Op::ConstBool(fh <= top));
                }
            }
            // `IntFrag(c)` of a literal is a constant (as in `eval`): buttons
            // become constants.
            Op::IntFrag(c) if matches!(self.nodes[args[0] as usize].op, Op::Const(..)) => {
                let Op::Const(lo, _) = self.nodes[args[0] as usize].op else { unreachable!("guarded above") };
                let (step, mask) = self.grid();
                let base = (lo & mask) as i64 + c as i64 * step as i64;
                let v = (lo as i64).max(base).clamp(i32::MIN as i64, i32::MAX as i64) as i32;
                return self.leaf(Op::Const(v, v));
            }
            // No-wrap of anything but an interval `+`/`-`/negation holds (as in
            // `eval`).
            Op::NoWrap if !matches!(self.nodes[args[0] as usize].op, Op::Add | Op::Sub | Op::Neg) => {
                return self.leaf(Op::ConstBool(true));
            }
            // Arithmetic on literal POINTS folds (PICO-8 arithmetic), so a
            // select on it folds instead of being split both ways.
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Min | Op::Max => {
                let point = |g: &Graph, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) if lo == hi => Some(Pico8Num::from_raw(lo)),
                    _ => None,
                };
                if let (Some(x), Some(y)) = (point(self, 0), point(self, 1)) {
                    let r = match op {
                        Op::Add => Some(x + y),
                        Op::Sub => Some(x - y),
                        Op::Mul => Some(x * y),
                        Op::Div if y.as_raw_u32() != 0 => Some(x / y),
                        Op::Div => None,
                        Op::Min => Some(if y < x { y } else { x }),
                        _ => Some(if y > x { y } else { x }),
                    };
                    if let Some(r) = r {
                        let raw = r.as_raw_u32() as i32;
                        return self.leaf(Op::Const(raw, raw));
                    }
                }
            }
            Op::Neg | Op::Abs | Op::Flr => {
                if let Op::Const(lo, hi) = self.nodes[args[0] as usize].op {
                    if lo == hi {
                        let x = Pico8Num::from_raw(lo);
                        let r = match op {
                            Op::Neg => -x,
                            Op::Abs => x.abs(),
                            _ => x.flr(),
                        };
                        let raw = r.as_raw_u32() as i32;
                        return self.leaf(Op::Const(raw, raw));
                    }
                }
            }
            // A comparison of two literal POINTS is a constant.
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => {
                let point = |g: &Graph, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) if lo == hi => Some(lo),
                    _ => None,
                };
                if let (Some(x), Some(y)) = (point(self, 0), point(self, 1)) {
                    let r = match op {
                        Op::Lt => x < y,
                        Op::Le => x <= y,
                        Op::Gt => x > y,
                        Op::Ge => x >= y,
                        _ => x == y,
                    };
                    return self.leaf(Op::ConstBool(r));
                }
            }
            // A span of two literals IS a literal interval (as in `eval`).
            Op::Span => {
                let lit = |g: &Graph, i: usize| match g.nodes[args[i] as usize].op {
                    Op::Const(lo, hi) => Some((lo, hi)),
                    _ => None,
                };
                if let (Some((lo, _)), Some((_, hi))) = (lit(self, 0), lit(self, 1)) {
                    // Tolerant of `lo > hi` (an unreached table-fork fragment).
                    return self.leaf(Op::Const(lo.min(hi), lo.max(hi)));
                }
            }
            // A decided condition picks its arm.
            Op::Sel => {
                if let Some(c) = cbool(self, 0) {
                    return if c { args[1] } else { args[2] };
                }
                // Same arms: the condition cannot matter (keeps joins cheap).
                if args[1] == args[2] {
                    return args[1];
                }
                // A boolean select with a constant arm is boolean algebra,
                // exact in Kleene (`sel(c, true, x)` = `c or x`).
                let (c, t, f) = (args[0], args[1], args[2]);
                match (cbool(self, 1), cbool(self, 2)) {
                    (Some(true), Some(false)) => return c,
                    (Some(false), Some(true)) => return self.fold(Op::Not, vec![c]),
                    (Some(true), None) => return self.fold(Op::Or, vec![c, f]),
                    (Some(false), None) => {
                        let n = self.fold(Op::Not, vec![c]);
                        return self.fold(Op::And, vec![n, f]);
                    }
                    (None, Some(true)) => {
                        let n = self.fold(Op::Not, vec![c]);
                        return self.fold(Op::Or, vec![n, t]);
                    }
                    (None, Some(false)) => return self.fold(Op::And, vec![c, t]),
                    _ => {}
                }
            }
            Op::Not => {
                if let Some(b) = cbool(self, 0) {
                    return self.leaf(Op::ConstBool(!b));
                }
                let inner = self.nodes[args[0] as usize].clone();
                // Involution.
                if inner.op == Op::Not {
                    return inner.args[0];
                }
                // A negated comparison is the opposite comparison (exact on
                // intervals): `x < y` and `not (x >= y)` are one node.
                let flip = match inner.op {
                    Op::Lt => Some(Op::Ge),
                    Op::Le => Some(Op::Gt),
                    Op::Gt => Some(Op::Le),
                    Op::Ge => Some(Op::Lt),
                    _ => None,
                };
                if let Some(f) = flip {
                    return self.fold(f, inner.args);
                }
            }
            Op::Known => {
                if cbool(self, 0).is_some() {
                    return self.leaf(Op::ConstBool(true));
                }
                // Negation preserves decidedness.
                let inner = self.nodes[args[0] as usize].clone();
                if inner.op == Op::Not {
                    return self.fold(Op::Known, inner.args);
                }
            }
            Op::And => {
                if args[0] == args[1] {
                    return args[0];
                }
                let (x, y) = (cbool(self, 0), cbool(self, 1));
                // `false AND anything` is false.
                if x == Some(false) || y == Some(false) {
                    return self.leaf(Op::ConstBool(false));
                }
                match (x, y) {
                    (Some(_), Some(_)) => return self.leaf(Op::ConstBool(true)),
                    (Some(_), None) => return args[1],
                    (None, Some(_)) => return args[0],
                    _ => {}
                }
            }
            Op::Or => {
                if args[0] == args[1] {
                    return args[0];
                }
                let (x, y) = (cbool(self, 0), cbool(self, 1));
                // `true OR anything` is true.
                if x == Some(true) || y == Some(true) {
                    return self.leaf(Op::ConstBool(true));
                }
                match (x, y) {
                    (Some(_), Some(_)) => return self.leaf(Op::ConstBool(false)),
                    (Some(_), None) => return args[1],
                    (None, Some(_)) => return args[0],
                    _ => {}
                }
                // `(g and c) or (g and not c)` is `g`, exactly; otherwise `live`
                // would pull every split fork into the configuration product.
                let and_args = |g: &Graph, n: NodeId| -> Option<[NodeId; 2]> {
                    let node = &g.nodes[n as usize];
                    (node.op == Op::And).then(|| [node.args[0], node.args[1]])
                };
                if let (Some(a), Some(b)) = (and_args(self, args[0]), and_args(self, args[1])) {
                    for i in 0..2 {
                        for j in 0..2 {
                            if a[i] == b[j] && self.complements(a[1 - i], b[1 - j]) {
                                return a[i];
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        self.add(op, args)
    }

    /// Is `y` the negation of `x` (`Not(x)` or the opposite comparison)?
    fn complements(&self, x: NodeId, y: NodeId) -> bool {
        let (nx, ny) = (&self.nodes[x as usize], &self.nodes[y as usize]);
        if (nx.op == Op::Not && nx.args[0] == y) || (ny.op == Op::Not && ny.args[0] == x) {
            return true;
        }
        let opposite = matches!(
            (&nx.op, &ny.op),
            (Op::Lt, Op::Ge) | (Op::Ge, Op::Lt) | (Op::Le, Op::Gt) | (Op::Gt, Op::Le)
        );
        opposite && nx.args == ny.args
    }

    /// Evaluate every node from the input cells in one forward pass. Exact or
    /// an error.
    pub fn eval(&self, cells: &HashMap<u32, Val>) -> Result<Vec<Val>> {
        self.eval_inner(cells, false, true, None)
    }

    /// As `eval`, but an unmodelled node is TOP for its kind (sound) instead
    /// of an error. The same match, so the two cannot diverge.
    pub fn eval_lenient(&self, cells: &HashMap<u32, Val>) -> Result<Vec<Val>> {
        self.eval_inner(cells, true, false, None)
    }

    /// `eval_lenient` with the map, deciding `TileFlagAt`.
    pub fn eval_lenient_in(&self, cells: &HashMap<u32, Val>, room: &Room) -> Result<Vec<Val>> {
        self.eval_inner(cells, true, false, Some(room))
    }

    /// Forks resolved via `Frag`; an unmodelled node (`Mget`, roomless
    /// `TileFlagAt`) is TOP.
    pub fn eval_narrow_top_in(&self, cells: &HashMap<u32, Val>, room: &Room) -> Result<Vec<Val>> {
        self.eval_inner(cells, false, false, Some(room))
    }

    /// `eval_narrow_top_in` without a room, for bare arithmetic sub-DAGs.
    pub fn eval_narrow_top(&self, cells: &HashMap<u32, Val>) -> Result<Vec<Val>> {
        self.eval_inner(cells, false, false, None)
    }

    fn eval_inner(
        &self,
        cells: &HashMap<u32, Val>,
        frag_lenient: bool,
        strict_err: bool,
        room: Option<&Room>,
    ) -> Result<Vec<Val>> {
        let lenient = frag_lenient;
        let full = Val::Num(Pico8NumInterval::new(
            Pico8Num::from_raw(i32::MIN),
            Pico8Num::from_raw(i32::MAX),
        ));
        let mut out: Vec<Val> = Vec::with_capacity(self.nodes.len());
        for (i, node) in self.nodes.iter().enumerate() {
            macro_rules! a {
                ($k:expr) => {
                    out[node.args[$k] as usize]
                };
            }
            let computed = (|| -> Result<Val> {
                Ok(match &node.op {
                Op::Const(lo, hi) => Val::Num(Pico8NumInterval::new(
                    Pico8Num::from_raw(*lo),
                    Pico8Num::from_raw(*hi),
                )),
                Op::ConstBool(b) => Val::Bool(Some(*b)),
                // Nothing is known, in every lane.
                Op::UnknownBool(_) => Val::Bool(None),
                // No interval holds it: unmodelled (TOP when lenient).
                Op::UnknownNum => bail!("node {}: an unknown number has no interval", i),
                // The hull (`fold`'s two-constant rule agrees). Tolerant of `lo > hi`.
                Op::Span => {
                    let (lo, hi) = (a!(0).as_num("Span")?.low, a!(1).as_num("Span")?.high);
                    Val::Num(Pico8NumInterval::new(lo.min(hi), lo.max(hi)))
                }
                // The operand inside the assumed range: a lane outside is in
                // error (the node's own), so the value need not cover it.
                // Disjoint, every lane is in error: the operand, as the kernel.
                Op::Restrict(lo, hi) => {
                    let x = a!(0).as_num("Restrict")?;
                    let (lo, hi) = (x.low.max(Pico8Num::from_raw(*lo)), x.high.min(Pico8Num::from_raw(*hi)));
                    Val::Num(if lo <= hi { Pico8NumInterval::new(lo, hi) } else { x })
                }
                // A split RESTRICTS its operand: lenient, the operand covers it.
                Op::Split(d) if lenient => {
                    let _ = d;
                    a!(0)
                }
                Op::SplitInt(_) if lenient => a!(0),
                Op::Split(d) | Op::SplitValid(d) | Op::SplitInt(d) => {
                    bail!("node {}: split {} has no value outside an outcome", i, d)
                }
                // ONE lane's end, over the lanes: anywhere in the hull (except a
                // span's or a literal's). Never the hull's end as a point, or
                // the fold decides premises on it.
                Op::Lo | Op::Hi => {
                    let low = matches!(node.op, Op::Lo);
                    let arg = &self.nodes[node.args[0] as usize];
                    match arg.op {
                        Op::Span => Val::Num(out[arg.args[if low { 0 } else { 1 }] as usize].as_num("Lo/Hi of Span")?),
                        Op::Const(l, h) => {
                            let v = Pico8Num::from_raw(if low { l } else { h });
                            Val::Num(Pico8NumInterval::new(v, v))
                        }
                        _ => Val::Num(a!(0).as_num("Lo/Hi")?),
                    }
                }
                // Fragment `c`'s low end, exact (lenient: its hull over lanes).
                Op::IntFrag(c) => {
                    let iv = a!(0).as_num("IntFrag")?;
                    let (step, mask) = self.grid();
                    let gflr = |p: Pico8Num| p.as_raw_u32() as i32 as i64 & mask as i64;
                    let (fl, fh) = (gflr(iv.low), gflr(iv.high));
                    // Clamped: the full-range hull's high end plus a step would wrap.
                    let clamp = |v: i64| v.clamp(i32::MIN as i64, i32::MAX as i64) as i32;
                    let base = fl + *c as i64 * step as i64;
                    let lo = clamp((iv.low.as_raw_u32() as i32 as i64).max(base));
                    if lenient {
                        let hi = clamp((iv.high.as_raw_u32() as i32 as i64).max(fh + *c as i64 * step as i64));
                        Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(lo), Pico8Num::from_raw(hi.max(lo))))
                    } else {
                        Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(lo), Pico8Num::from_raw(lo)))
                    }
                }
                // `zi_span_ok`: high end's floor at most `ways - 1` cells above
                // the low end's. On a hull: fits => every lane fits, else unknown.
                Op::SplitOk(ways) => {
                    let iv = a!(0).as_num("SplitOk")?;
                    let step = GRID_STEP as i64;
                    let floor = |v: Pico8Num| (v.as_raw_u32() as i32 as i64).div_euclid(step) * step;
                    if floor(iv.high) <= floor(iv.low) + (*ways as i64 - 1) * step {
                        Val::Bool(Some(true))
                    } else if !strict_err {
                        Val::Bool(None)
                    } else {
                        bail!("node {}: SplitOk of an interval that may not fit", i)
                    }
                }
                // No-wrap in i64. On a hull: no wrap is no wrap on any lane; a
                // wrap is unknown per lane.
                Op::NoWrap => {
                    let x = &self.nodes[node.args[0] as usize];
                    let fits = match x.op {
                        Op::Add | Op::Sub => {
                            let (p, q) = (out[x.args[0] as usize].as_num("NoWrap")?, out[x.args[1] as usize].as_num("NoWrap")?);
                            if matches!(x.op, Op::Add) { p.checked_add(q) } else { p.checked_sub(q) }.is_some()
                        }
                        Op::Neg => out[x.args[0] as usize].as_num("NoWrap")?.checked_neg().is_some(),
                        _ => true,
                    };
                    Val::Bool(if fits || !lenient { Some(fits) } else { None })
                }
                // The RESOLVED fork: `zi_fork_flr` on one interval.
                Op::Frag(c) | Op::FragOk(c) => {
                    let iv = a!(0).as_num("Frag")?;
                    let (step, mask) = self.grid();
                    let gflr = |p: Pico8Num| Pico8Num::from_raw(p.as_raw_u32() as i32 & mask);
                    let (fl, fh) = (gflr(iv.low), gflr(iv.high));
                    // Cells spanned minus one, in i64 (a full-range hull overflows).
                    let n = (fh.as_raw_u32() as i32 as i64 - fl.as_raw_u32() as i32 as i64) / step as i64;
                    let c = *c as i64;
                    // Lenient, the operand is a HULL over lanes: a lane in a wide
                    // hull may still span two floors, so `FragOk(c)` is decided
                    // only where the hull rules it out. The value is the hull.
                    if lenient {
                        return Ok(match (&node.op, c) {
                            (Op::Frag(_), _) => a!(0),
                            (_, 0) => Val::Bool(Some(true)),
                            // Too few cells: no lane reaches fragment `c`.
                            _ if n < c => Val::Bool(Some(false)),
                            _ => Val::Bool(None),
                        });
                    }
                    // Valid iff the interval reaches cell `c`; an invalid fragment
                    // keeps the whole value (never taken).
                    let valid = n >= c;
                    match &node.op {
                        Op::Frag(_) if valid => {
                            let base = fl.as_raw_u32() as i32 as i64 + c * step as i64;
                            let lo = (iv.low.as_raw_u32() as i32 as i64).max(base);
                            let hi = (iv.high.as_raw_u32() as i32 as i64).min(base + step as i64 - 1);
                            Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(lo as i32), Pico8Num::from_raw(hi as i32)))
                        }
                        Op::Frag(_) => a!(0),
                        _ => Val::Bool(Some(valid)),
                    }
                }
                Op::Cell(c) => match cells.get(c) {
                    Some(v) => *v,
                    None => bail!("node {}: input cell {} was not supplied", i, c),
                },
                // CHECKED: inputs run at TOP on purpose (`transpile::ival`), so
                // a wrap is expected.
                Op::Add => Val::Num(
                    a!(0)
                        .as_num("Add")?
                        .checked_add(a!(1).as_num("Add")?)
                        .ok_or_else(|| anyhow!("node {}: Add wrapped", i))?,
                ),
                Op::Sub => Val::Num(
                    a!(0)
                        .as_num("Sub")?
                        .checked_sub(a!(1).as_num("Sub")?)
                        .ok_or_else(|| anyhow!("node {}: Sub wrapped", i))?,
                ),
                Op::Neg => Val::Num(
                    a!(0)
                        .as_num("Neg")?
                        .checked_neg()
                        .ok_or_else(|| anyhow!("node {}: Neg wrapped", i))?,
                ),
                Op::Mul | Op::Div | Op::Rem => Self::arith(&node.op, a!(0), a!(1))?,
                Op::Abs => {
                    let x = a!(0).as_num("Abs")?;
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
                    let x = a!(0).as_num("Flr")?;
                    Val::Num(Pico8NumInterval::new(x.low.flr(), x.high.flr()))
                }
                // sin is NOT monotone; only exact inputs are sound here.
                Op::Sin => match a!(0).as_exact() {
                    Some(n) => Val::exact_num(n.pico8_sin()),
                    None => bail!("Sin over a non-exact interval is not modelled"),
                },
                Op::Min | Op::Max => {
                    let (x, y) = (a!(0).as_num("MinMax")?, a!(1).as_num("MinMax")?);
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
                    Self::compare(&node.op, a!(0), a!(1))?
                }
                Op::Not => Val::Bool(a!(0).as_bool("Not")?.map(|b| !b)),
                Op::And => {
                    let (x, y) = (a!(0).as_bool("And")?, a!(1).as_bool("And")?);
                    // Short-circuit: `false AND unknown` is false.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(p && q),
                        (Some(false), None) | (None, Some(false)) => Some(false),
                        (Some(_), None) | (None, Some(_)) | (None, None) => None,
                    })
                }
                Op::Or => {
                    let (x, y) = (a!(0).as_bool("Or")?, a!(1).as_bool("Or")?);
                    // Short-circuit on a decided true, mirroring And.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(p || q),
                        (Some(true), None) | (None, Some(true)) => Some(true),
                        (Some(_), None) | (None, Some(_)) | (None, None) => None,
                    })
                }
                Op::Sel => match a!(0).as_bool("Sel")? {
                    Some(true) => a!(1),
                    Some(false) => a!(2),
                    // Undecided: the lane is in error (the select's own), so the
                    // join serves.
                    None => Self::join(a!(1), a!(2))?,
                },
                // Per-LANE decidedness. On a hull: decided means every lane is;
                // undecided says nothing (unknown, NOT false).
                Op::Known => {
                    let decided = match a!(0) {
                        Val::Bool(b) => b.is_some(),
                        Val::Num(i) => i.to_number().is_some(),
                    };
                    if decided {
                        Val::Bool(Some(true))
                    } else if lenient {
                        Val::Bool(None)
                    } else {
                        Val::Bool(Some(false))
                    }
                }
                Op::TileFlagAt if room.is_some() => {
                    Self::tile_flag_over(room.unwrap(), a!(0), a!(1), a!(2), a!(3), a!(4))?
                }
                // `mget` on EXACT coordinates only, as in the kernels.
                Op::Mget if room.is_some() => {
                    let (x, y) = (a!(0).as_num("Mget")?, a!(1).as_num("Mget")?);
                    if x.low != x.high || y.low != y.high {
                        bail!("node {}: mget over an interval coordinate", i);
                    }
                    let t = room.unwrap().cart.mget(x.low, y.low)?;
                    let t = Pico8Num::from_i16(t as i16);
                    Val::Num(Pico8NumInterval::new(t, t))
                }
                Op::Mget | Op::TileFlagAt => {
                    bail!("{:?} needs the cart; not supported by the pure evaluator yet", node.op)
                }
                })
            })();
                // The ONE place `lenient` acts: TOP for its kind is sound
                // whatever the reason.
            let v = match computed {
                Ok(v) => v,
                Err(e) => {
                    if strict_err {
                        return Err(e);
                    }
                    Self::top_of(&node.op, &node.args, &out, full)
                }
            };
            out.push(v);
        }
        Ok(out)
    }

    /// The weakest value of this op: `Bool(None)` for booleans, the full
    /// range for numbers; `Sel` follows its arms.
    fn top_of(op: &Op, args: &[NodeId], out: &[Val], full: Val) -> Val {
        match op {
            Op::ConstBool(_)
            | Op::SplitValid(_)
            | Op::SplitOk(_)
            | Op::NoWrap
            | Op::FragOk(_)
            | Op::Lt
            | Op::Le
            | Op::Gt
            | Op::Ge
            | Op::Eq
            | Op::Not
            | Op::And
            | Op::Or
            | Op::Known
            | Op::UnknownBool(_)
            | Op::TileFlagAt => Val::Bool(None),
            Op::Sel => match args.get(1).map(|x| out[*x as usize]) {
                Some(Val::Bool(_)) => Val::Bool(None),
                _ => full,
            },
            _ => full,
        }
    }

    /// `tile_flag_at` over INTERVALS: FALSE if the UNION of the rectangles
    /// holds no flagged tile, TRUE if their INTERSECTION holds one, unknown
    /// between. Two `solid_at` calls on derived rectangles.
    fn tile_flag_over(room: &Room, x: Val, y: Val, w: Val, h: Val, flag: Val) -> Result<Val> {
        // The size and flag must be exact.
        let ex = |v: Val, what: &str| -> Result<i32> {
            v.as_exact()
                .and_then(|n| n.as_i16())
                .map(|n| n as i32)
                .ok_or_else(|| anyhow!("tile_flag_at: {} is not an exact integer", what))
        };
        let (w, h) = (ex(w, "w")?, ex(h, "h")?);
        let flag = ex(flag, "flag")? as i16;
        // Every integer from floor(low) to floor(high); non-integers fail
        // concretely, so covering them is conservative.
        let span = |v: Val, what: &str| -> Result<(i32, i32)> {
            let i = v.as_num(what)?;
            let lo = i.low.flr().as_i16().ok_or_else(|| anyhow!("{}: unbounded", what))? as i32;
            let hi = i.high.flr().as_i16().ok_or_else(|| anyhow!("{}: unbounded", what))? as i32;
            Ok((lo, hi))
        };
        let (xlo, xhi) = span(x, "x")?;
        let (ylo, yhi) = span(y, "y")?;
        // Clamp the rectangle's EDGES to the 16-tile room, never corner and
        // size apart: at TOP that leaves only row 0, reading "nothing solid".
        let edges = |lo: i32, size: i32| -> (i16, i16) {
            let hi = lo + size - 1;
            let (lo, hi) = (lo.clamp(-4096, 4096), hi.clamp(-4096, 4096));
            (lo as i16, (hi - lo + 1) as i16)
        };
        let (dx, dy) = (xhi - xlo, yhi - ylo);
        let solid = |px: i32, py: i32, pw: i32, ph: i32| -> Result<bool> {
            let ((px, pw), (py, ph)) = (edges(px, pw), edges(py, ph));
            room.cache.flag_at(&room.cart, px, py, pw, ph, flag)
        };
        if !solid(xlo, ylo, w + dx, h + dy)? {
            return Ok(Val::Bool(Some(false)));
        }
        if dx < w && dy < h && solid(xhi, yhi, w - dx, h - dy)? {
            return Ok(Val::Bool(Some(true)));
        }
        Ok(Val::Bool(None))
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
        // Checked, as for `Add`: at TOP a scale wraps.
        Ok(Val::Num(match op {
            Op::Mul => xi
                .checked_scale_positive(scalar)
                .ok_or_else(|| anyhow!("Mul over an interval wrapped"))?,
            Op::Div => xi
                .checked_div_positive(scalar)
                .ok_or_else(|| anyhow!("Div over an interval wrapped"))?,
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
            // Order comparisons: corners bound the box; equality needs disjointness.
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
        // `add` does not normalize (`fold` does).
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
        // Validity is an ordinary node: a select is valid only where `known(c)`.
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
        // false AND unknown is FALSE, not unknown.
        let cells = HashMap::from([(1u32, Val::Bool(Some(false))), (2u32, Val::Bool(None))]);
        assert_eq!(g.eval(&cells).unwrap()[and as usize], Val::Bool(Some(false)));
    }

    /// `SplitOk(n)` over a literal folds to what `zi_span_ok` computes.
    #[test]
    fn split_ok_of_a_literal_is_what_zi_span_ok_says() {
        use celeste_engine::kernel::{zi_span_ok, zn_splat, ALL, ZI};
        let raws = [i32::MIN, -0x2_8000, -0x1_0000, -1, 0, 1, 0x8000, 0xffff, 0x1_0000, 0x1_0001, 0x2_0000, 0x3_7fff, i32::MAX - 0x1_0000, i32::MAX];
        let mut g = Graph::new();
        for &lo in &raws {
            for &hi in raws.iter().filter(|h| **h >= lo) {
                let c = g.leaf(Op::Const(lo, hi));
                for ways in 1..=4u8 {
                    let got = g.fold(Op::SplitOk(ways), vec![c]);
                    let iv = ZI { lo: zn_splat(Pico8Num::from_raw(lo)), hi: zn_splat(Pico8Num::from_raw(hi)) };
                    let want = zi_span_ok(iv, ways).val == ALL;
                    assert_eq!(g.get(got).op, Op::ConstBool(want), "ways {ways} [{lo:#x}, {hi:#x}]");
                }
            }
        }
    }

    #[test]
    fn folding_is_exact_not_merely_sound() {
        // Every `fold` rule must be EXACT: `fold` and `add` evaluate alike at
        // every leaf assignment (hence no `x and not x -> false`).
        let mut g = Graph::new();
        let b: Vec<NodeId> = (0..3).map(|i| g.leaf(Op::Cell(i))).collect();
        let nums: Vec<NodeId> = (10..12).map(|i| g.leaf(Op::Cell(i))).collect();
        let tt = g.leaf(Op::ConstBool(true));
        let ff = g.leaf(Op::ConstBool(false));

        // Boolean subexpressions built unfolded, for the rules to bite on.
        let not0 = g.add(Op::Not, vec![b[0]]);
        let nn1 = g.add(Op::Not, vec![b[1]]);
        let notnot1 = g.add(Op::Not, vec![nn1]);
        let lt = g.add(Op::Lt, vec![nums[0], nums[1]]);
        let ge = g.add(Op::Ge, vec![nums[0], nums[1]]);
        let and01 = g.add(Op::And, vec![b[0], b[1]]);
        let kn = g.add(Op::Known, vec![b[2]]);
        let bools = vec![b[0], b[1], b[2], tt, ff, not0, notnot1, lt, ge, and01, kn];
        let numexprs = vec![
            nums[0],
            nums[1],
            g.add(Op::Add, vec![nums[0], nums[1]]),
            g.leaf(Op::Const(n(3).as_raw_u32() as i32, n(3).as_raw_u32() as i32)),
        ];

        // (folded, unfolded) pairs to compare.
        let mut pairs: Vec<(NodeId, NodeId)> = Vec::new();
        let both = |g: &mut Graph, op: Op, args: Vec<NodeId>| {
            let f = g.fold(op.clone(), args.clone());
            let p = g.add(op, args);
            (f, p)
        };
        for x in bools.clone() {
            for op in [Op::Not, Op::Known] {
                pairs.push(both(&mut g, op, vec![x]));
            }
            for y in bools.clone() {
                for op in [Op::And, Op::Or, Op::Eq] {
                    pairs.push(both(&mut g, op, vec![x, y]));
                }
                for z in bools.clone() {
                    pairs.push(both(&mut g, Op::Sel, vec![x, y, z]));
                }
            }
        }
        for c in bools.clone() {
            for x in numexprs.clone() {
                for y in numexprs.clone() {
                    pairs.push(both(&mut g, Op::Sel, vec![c, x, y]));
                    for op in [Op::Add, Op::Min, Op::Max, Op::Lt, Op::Le, Op::Gt, Op::Ge, Op::Eq] {
                        pairs.push(both(&mut g, op, vec![x, y]));
                    }
                }
            }
        }

        let tri = [Some(true), Some(false), None];
        let ivals = [
            Pico8NumInterval::from_number(n(0)),
            Pico8NumInterval::from_number(n(1)),
            Pico8NumInterval::new(n(0), n(2)),
        ];
        let mut checked = 0usize;
        for p0 in tri {
            for p1 in tri {
                for p2 in tri {
                    for i0 in ivals {
                        for i1 in ivals {
                            let cells = HashMap::from([
                                (0u32, Val::Bool(p0)),
                                (1u32, Val::Bool(p1)),
                                (2u32, Val::Bool(p2)),
                                (10u32, Val::Num(i0)),
                                (11u32, Val::Num(i1)),
                            ]);
                            let out = g.eval(&cells).expect("every op in the pool evaluates");
                            for (f, p) in &pairs {
                                assert_eq!(
                                    out[*f as usize],
                                    out[*p as usize],
                                    "fold changed the value of {:?} at {:?}",
                                    g.get(*p),
                                    cells
                                );
                                checked += 1;
                            }
                        }
                    }
                }
            }
        }
        // Guard against the test silently checking nothing.
        assert!(pairs.len() > 1_000, "pool collapsed to {} pairs", pairs.len());
        let fired = pairs.iter().filter(|(f, p)| f != p).count();
        assert!(fired > 500, "only {} of {} pairs were folded", fired, pairs.len());
        eprintln!(
            "[fold] {} pairs, {} folded, {} value comparisons",
            pairs.len(),
            fired,
            checked
        );
    }

    #[test]
    fn commutative_operands_are_put_in_a_canonical_order() {
        // `add` stays the raw constructor (the control above).
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let b = g.leaf(Op::Cell(2));
        for op in [Op::Add, Op::Min, Op::Max, Op::Eq, Op::And, Op::Or] {
            assert_eq!(
                g.fold(op.clone(), vec![a, b]),
                g.fold(op.clone(), vec![b, a]),
                "{:?} did not normalize its operand order",
                op
            );
        }
        // Subtraction does not commute, and must not be normalized.
        assert_ne!(g.fold(Op::Sub, vec![a, b]), g.fold(Op::Sub, vec![b, a]));
        // `Mul` commutes but `eval` is monotone only with the exact side
        // second: a constant moves right.
        let k = g.leaf(Op::Const(0, 0));
        assert_eq!(g.fold(Op::Mul, vec![k, a]), g.fold(Op::Mul, vec![a, k]));
        assert_ne!(g.fold(Op::Mul, vec![a, b]), g.fold(Op::Mul, vec![b, a]));
    }

    #[test]
    fn a_select_between_boolean_constants_is_boolean_algebra() {
        // `if c then true else x` is `c or x`.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let x = g.leaf(Op::Cell(2));
        let tt = g.leaf(Op::ConstBool(true));
        let ff = g.leaf(Op::ConstBool(false));
        assert_eq!(g.fold(Op::Sel, vec![c, tt, ff]), c, "select of true/false is the condition");
        assert_eq!(
            g.fold(Op::Sel, vec![c, ff, tt]),
            g.fold(Op::Not, vec![c]),
            "select of false/true is its negation"
        );
        assert_eq!(g.fold(Op::Sel, vec![c, tt, x]), g.fold(Op::Or, vec![c, x]));
        assert_eq!(g.fold(Op::Sel, vec![c, x, ff]), g.fold(Op::And, vec![c, x]));
        let nc = g.fold(Op::Not, vec![c]);
        assert_eq!(g.fold(Op::Sel, vec![c, ff, x]), g.fold(Op::And, vec![nc, x]));
        assert_eq!(g.fold(Op::Sel, vec![c, x, tt]), g.fold(Op::Or, vec![nc, x]));
    }

    #[test]
    fn a_split_guard_merged_back_is_the_guard() {
        // `(g and c) or (g and not c)` is `g` and must not read `c`.
        let mut g = Graph::new();
        let guard = g.leaf(Op::Cell(1));
        let c = g.leaf(Op::Cell(2));
        let nc = g.fold(Op::Not, vec![c]);
        let t = g.fold(Op::And, vec![guard, c]);
        let f = g.fold(Op::And, vec![nc, guard]);
        assert_eq!(g.fold(Op::Or, vec![t, f]), guard);
        // The negation of a comparison folds into the opposite comparison.
        let (a, b) = (g.leaf(Op::Cell(3)), g.leaf(Op::Cell(4)));
        let gt = g.fold(Op::Gt, vec![a, b]);
        let le = g.fold(Op::Not, vec![gt]);
        let t = g.fold(Op::And, vec![guard, gt]);
        let f = g.fold(Op::And, vec![guard, le]);
        assert_eq!(g.fold(Op::Or, vec![t, f]), guard);
        // Two different conditions are not complements.
        let d = g.leaf(Op::Cell(5));
        let f = g.fold(Op::And, vec![guard, d]);
        assert_ne!(g.fold(Op::Or, vec![t, f]), guard);
    }

    #[test]
    fn a_negated_comparison_is_the_opposite_comparison() {
        // Makes `x < y` and `not (x >= y)` the SAME node.
        let mut g = Graph::new();
        let (x, y) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)));
        for (op, opp) in [
            (Op::Lt, Op::Ge),
            (Op::Le, Op::Gt),
            (Op::Gt, Op::Le),
            (Op::Ge, Op::Lt),
        ] {
            let a = g.fold(op.clone(), vec![x, y]);
            assert_eq!(g.fold(Op::Not, vec![a]), g.fold(opp.clone(), vec![x, y]));
        }
        // Equality has no negation in the vocabulary, so it keeps its Not.
        let e = g.fold(Op::Eq, vec![x, y]);
        let ne = g.fold(Op::Not, vec![e]);
        assert_eq!(g.get(ne).op, Op::Not);
        // ... and double negation is the identity.
        assert_eq!(g.fold(Op::Not, vec![ne]), e);
    }

    #[test]
    fn a_select_between_equal_arms_is_not_a_select() {
        // A join proposes a select per cell, almost all untouched: free.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let x = g.leaf(Op::Cell(2));
        assert_eq!(g.fold(Op::Sel, vec![c, x, x]), x);
        // A genuine difference still costs a node.
        let y = g.leaf(Op::Cell(3));
        let sel = g.fold(Op::Sel, vec![c, x, y]);
        assert_ne!(sel, x);
        assert_ne!(sel, y);
        // And a decided condition still picks its arm.
        let t = g.leaf(Op::ConstBool(true));
        assert_eq!(g.fold(Op::Sel, vec![t, x, y]), x);
    }

    #[test]
    fn specializing_the_button_forks_collapses_the_configurations_that_agree() {
        // `input` is Sel(right, 1, Sel(left, -1, 0)) over two buttons: with
        // right = true, {left,right} and {right} share a node.
        let mut d = crate::trace::domain::Symbolic::default();
        let left = d.both_values("left");
        let right = d.both_values("right");
        // Its coverage premise (`trace::error`) holds on every lane.
        let fork = d.graph.get(left).args[0];
        assert!(d.graph.is_literal_fork(fork), "a button is a fork over a literal");
        let literal = d.graph.get(fork).args[0];
        let covered = d.graph.fold(Op::SplitOk(2), vec![literal]);
        assert_eq!(d.graph.get(covered).op, Op::ConstBool(true), "the literal fits two ways");
        let g = &mut d.graph;
        let one = g.leaf(Op::Const(n(1).as_raw_u32() as i32, n(1).as_raw_u32() as i32));
        let neg = g.leaf(Op::Const(n(-1).as_raw_u32() as i32, n(-1).as_raw_u32() as i32));
        let zero = g.leaf(Op::Const(0, 0));
        let inner = g.add(Op::Sel, vec![left, neg, zero]);
        let input = g.add(Op::Sel, vec![right, one, inner]);

        let mut shared = Graph::new();
        let sig = |l: u8, r: u8, shared: &mut Graph| {
            let map = d.graph.specialize_subset_into(&[l, r], None, shared);
            (map[input as usize], map[left as usize], map[right as usize])
        };
        let r_only = sig(0, 1, &mut shared);
        let both = sig(1, 1, &mut shared);
        assert_eq!(r_only.0, both.0, "right dominates left; these are one configuration");
        let l_only = sig(1, 0, &mut shared);
        let none = sig(0, 0, &mut shared);
        assert_ne!(l_only.0, none.0);
        assert_ne!(l_only.0, both.0);
        // Each configuration resolves a button to a constant.
        for (cfg, want) in [(none, (false, false)), (l_only, (true, false)), (both, (true, true))] {
            assert_eq!(shared.get(cfg.1).op, Op::ConstBool(want.0), "left");
            assert_eq!(shared.get(cfg.2).op, Op::ConstBool(want.1), "right");
        }
    }

    #[test]
    fn unmodelled_operations_are_loud() {
        // An op we cannot evaluate EXACTLY must error, never approximate.
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

    /// A select on a non-constant condition is the UNION of its arms.
    #[test]
    fn pieces_keep_a_select_as_a_union() {
        let mut g = Graph::new();
        let s = g.leaf(Op::Cell(0));
        let five = g.leaf(Op::Const(5 << 16, 5 << 16));
        let cond = g.leaf(Op::Cell(1));
        let sel = g.add(Op::Sel, vec![cond, five, s]);
        let neg = g.add(Op::Neg, vec![sel]);
        let seeds: HashMap<NodeId, (i64, i64)> = HashMap::from([(s, (-(1i64 << 16), 1i64 << 16))]);
        let mut memo = HashMap::new();
        assert_eq!(pieces_of(&g, &seeds, &mut memo, sel), Some(vec![(-(1 << 16), 1 << 16), (5 << 16, 5 << 16)]));
        assert_eq!(pieces_of(&g, &seeds, &mut memo, neg), Some(vec![(-(5 << 16), -(5 << 16)), (-(1 << 16), 1 << 16)]));
        // An unseeded cell is unknown, and so is anything over it.
        let other = g.leaf(Op::Cell(2));
        let sum = g.add(Op::Add, vec![s, other]);
        assert_eq!(pieces_of(&g, &seeds, &mut memo, sum), None);
    }

    /// `Lo`/`Hi` of a hull are not points, so nothing may decide a comparison
    /// one lane could fail.
    #[test]
    fn the_ends_of_a_hull_are_not_points() {
        let mut g = Graph::new();
        let v = g.leaf(Op::Cell(0));
        let hi = g.add(Op::Hi, vec![v]);
        let one = g.leaf(Op::Const(1 << 16, 1 << 16));
        let le = g.add(Op::Le, vec![hi, one]);
        let hull = Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(0), Pico8Num::from_raw(3 << 16)));
        let vals = g.eval_lenient(&HashMap::from([(0u32, hull)])).expect("eval");
        assert_eq!(vals[hi as usize], hull, "Hi of a hull is the hull");
        assert_eq!(vals[le as usize], Val::Bool(None), "a lane [0, 0.5] has Hi <= 1, a lane [2, 3] does not");
        let seeds: HashMap<NodeId, (i64, i64)> = HashMap::from([(v, (0, 3 << 16))]);
        assert_eq!(pieces_of(&g, &seeds, &mut HashMap::new(), hi), Some(vec![(0, 3 << 16)]));
        let r = g.add(Op::Restrict(0, 3 << 16), vec![v]);
        let hi_r = g.add(Op::Hi, vec![r]);
        let le_r = g.add(Op::Le, vec![hi_r, one]);
        let (out, map, _) = crate::transpile::ival::fold(&g, &[le_r], None).expect("fold");
        assert_eq!(out.get(map[le_r as usize]).op, Op::Le, "the fold leaves it to the lanes");
    }
}

/// A static range as sorted, disjoint pieces (raw 16.16, inclusive).
pub type Pieces = Vec<(i64, i64)>;

/// The most pieces a range keeps; past that it is one hull.
pub const MAX_PIECES: usize = 64;

/// Sort, merge overlapping/adjacent, refuse a 16.16 overflow ("unknown"),
/// and cap the count.
pub fn normalize_pieces(mut v: Pieces) -> Option<Pieces> {
    v.retain(|(lo, hi)| lo <= hi);
    if v.is_empty() || v.iter().any(|(lo, hi)| *lo < i32::MIN as i64 || *hi > i32::MAX as i64) {
        return None;
    }
    v.sort_unstable();
    let mut out: Pieces = Vec::with_capacity(v.len());
    for (lo, hi) in v {
        match out.last_mut() {
            Some(last) if lo <= last.1 + 1 => last.1 = last.1.max(hi),
            _ => out.push((lo, hi)),
        }
    }
    if out.len() > MAX_PIECES {
        out = vec![(out[0].0, out[out.len() - 1].1)];
    }
    Some(out)
}

/// The static range of `n` (raw 16.16) as PIECES, from `Op::Restrict` nodes
/// and seeded input cells through the arithmetic; a select on a non-constant
/// condition is the union of its arms (the dash's ±5 beside a run speed
/// under 1). `None` is unknown, never wrong; a restriction's own error, and
/// the runtime guard on a seed, make it a fact. `memo` is valid for one
/// graph and seed set.
pub fn pieces_of(
    g: &Graph,
    seeds: &HashMap<NodeId, (i64, i64)>,
    memo: &mut HashMap<NodeId, Option<Pieces>>,
    n: NodeId,
) -> Option<Pieces> {
    if let Some(r) = memo.get(&n) {
        return r.clone();
    }
    let node = g.get(n);
    let (op, args) = (node.op.clone(), node.args.clone());
    let konst = |g: &Graph, n: NodeId| -> Option<Pico8Num> {
        match g.get(n).op {
            Op::Const(lo, hi) if lo == hi => Some(Pico8Num::from_raw(lo)),
            _ => None,
        }
    };
    let raw = |p: Pico8Num| p.as_raw_u32() as i32 as i64;
    let p8 = |v: i64| Pico8Num::from_raw(v as i32);
    let rec = |memo: &mut HashMap<NodeId, Option<Pieces>>, k: usize| pieces_of(g, seeds, memo, args[k]);
    let r: Option<Pieces> = match op {
        Op::Const(lo, hi) => Some(vec![(lo as i64, hi as i64)]),
        Op::Cell(_) => seeds.get(&n).map(|r| vec![*r]),
        // The assumed range (as `eval`): its own error makes it a fact.
        Op::Restrict(lo, hi) => {
            let (lo, hi) = (lo as i64, hi as i64);
            match rec(memo, 0) {
                None => Some(vec![(lo, hi)]),
                Some(a) => {
                    let inside: Pieces = a.iter().map(|p| (p.0.max(lo), p.1.min(hi))).filter(|p| p.0 <= p.1).collect();
                    if inside.is_empty() { Some(a) } else { normalize_pieces(inside) }
                }
            }
        }
        Op::Add | Op::Sub | Op::Min | Op::Max => {
            let (a, b) = (rec(memo, 0)?, rec(memo, 1)?);
            let mut out = Vec::new();
            for x in &a {
                for y in &b {
                    out.push(match op {
                        Op::Add => (x.0 + y.0, x.1 + y.1),
                        Op::Sub => (x.0 - y.1, x.1 - y.0),
                        Op::Min => (x.0.min(y.0), x.1.min(y.1)),
                        _ => (x.0.max(y.0), x.1.max(y.1)),
                    });
                }
            }
            normalize_pieces(out)
        }
        Op::Neg => normalize_pieces(rec(memo, 0)?.into_iter().map(|a| (-a.1, -a.0)).collect()),
        Op::Abs => normalize_pieces(
            rec(memo, 0)?
                .into_iter()
                .map(|a| {
                    let lo = if a.0 <= 0 && a.1 >= 0 { 0 } else { a.0.abs().min(a.1.abs()) };
                    (lo, a.0.abs().max(a.1.abs()))
                })
                .collect(),
        ),
        // Product/quotient by a CONSTANT is monotone in the other operand.
        Op::Mul | Op::Div => {
            let (ka, kb) = (konst(g, args[0]), konst(g, args[1]));
            let (k_of, k, is_div) = match (ka, kb) {
                (_, Some(k)) => (0usize, k, matches!(op, Op::Div)),
                (Some(k), None) if matches!(op, Op::Mul) => (1, k, false),
                _ => {
                    memo.insert(n, None);
                    return None;
                }
            };
            if is_div && k.as_raw_u32() == 0 {
                memo.insert(n, None);
                return None;
            }
            let a = rec(memo, k_of)?;
            let f = |x: i64| -> i64 { raw(if is_div { p8(x) / k } else { p8(x) * k }) };
            let mut out = Vec::new();
            for a in a {
                let (x, y) = (f(a.0), f(a.1));
                out.push((x.min(y), x.max(y)));
            }
            normalize_pieces(out)
        }
        Op::Flr => {
            let a = rec(memo, 0)?;
            normalize_pieces(a.into_iter().map(|a| (raw(p8(a.0).flr()), raw(p8(a.1).flr()))).collect())
        }
        Op::Sin => Some(vec![(-(1i64 << 16), 1i64 << 16)]),
        Op::Sel => match g.get(args[0]).op {
            Op::ConstBool(true) => rec(memo, 1),
            Op::ConstBool(false) => rec(memo, 2),
            _ => {
                let (mut a, b) = (rec(memo, 1)?, rec(memo, 2)?);
                a.extend(b);
                normalize_pieces(a)
            }
        },
        Op::Span => {
            let (a, b) = (rec(memo, 0)?, rec(memo, 1)?);
            Some(vec![(a[0].0, b[b.len() - 1].1)])
        }
        // A fragment lies within its operand.
        Op::Split(_) | Op::SplitInt(_) | Op::Frag(_) | Op::IntFrag(_) => rec(memo, 0),
        // A lane's end, over the lanes (see `eval`'s `Lo`).
        Op::Lo | Op::Hi => {
            let low = matches!(op, Op::Lo);
            let arg = g.get(args[0]).clone();
            match arg.op {
                Op::Span => pieces_of(g, seeds, memo, arg.args[if low { 0 } else { 1 }]),
                Op::Const(l, h) => {
                    let v = if low { l } else { h } as i64;
                    Some(vec![(v, v)])
                }
                _ => rec(memo, 0),
            }
        }
        _ => None,
    };
    memo.insert(n, r.clone());
    r
}
