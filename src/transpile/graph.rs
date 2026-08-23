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

use std::sync::Arc;

use anyhow::{anyhow, bail, Result};

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

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

/// A SPECIALIZATION POINT: something the program could not keep symbolic,
/// so the whole downstream graph is rebuilt once per possible outcome.
///
/// The two kinds are one MECHANISM and two SEMANTICS, and the difference
/// is exactly why `Split` carries a validity mask and `Free` does not:
///
/// * `Free` - a symbolic boolean input, one of the six buttons. Every
///   outcome is live for every lane; the search enumerates them, and the
///   frame genuinely has 2^6 successors.
/// * `Split` - a value the abstract domain could not represent, so the
///   program enumerates the cases. The outcomes PARTITION the lanes, and
///   one can be empty.
///
/// Both are eliminated by `specialize_into`, and neither survives into
/// emitted code as a value: a `Free` becomes a constant, a `Split`
/// becomes a concrete narrowing.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Choice {
    Free(u8),
    Split(u8),
}

/// A set of choices, as a bitmask. Frees occupy the low 6 bits (one per
/// button), splits the rest.
pub type ChoiceSet = u16;
pub const N_FREE: u8 = 6;

impl Choice {
    pub fn bit(self) -> ChoiceSet {
        1 << match self {
            Choice::Free(b) => b,
            Choice::Split(d) => N_FREE + d,
        }
    }
    pub fn all_in(set: ChoiceSet) -> Vec<Choice> {
        (0..16u8)
            .filter(|i| set & (1 << i) != 0)
            .map(|i| {
                if i < N_FREE {
                    Choice::Free(i)
                } else {
                    Choice::Split(i - N_FREE)
                }
            })
            .collect()
    }
}

/// The map, for the evaluator. `CollisionCache` already carries which
/// room it is for, so this is just the pair the cart queries need.
#[derive(Clone)]
pub struct Room {
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
}

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
    /// `Free(b)`: the outcome of free choice b - one of the six button
    /// bits. A leaf, because nothing computes it: the search does.
    Free(u8),
    /// `Split(d)` over one operand: that value RESTRICTED to the outcome
    /// of split choice d. Unlike a free choice this is not a constant -
    /// the narrowing is a real operation on the operand - but it is
    /// eliminated by the same specialization step.
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
    /// Tri-state OR: a decided `true` wins over an unknown.
    ///
    /// This used to be deliberately absent, on the grounds that nothing
    /// constructed one. The TRACER constructs them: `a or b` is a Lua
    /// operator, and expressing it as `not (not a and not b)` costs three
    /// nodes instead of one and - worse - hides the symmetry, so `a or b`
    /// and `b or a` interned as different subgraphs. Measured on outcome 2
    /// of a traced frame, `Not` was the single most common op in the
    /// emitted body.
    Or,

    /// `Sel(cond, then, else)` - the former `if`, in every width.
    Sel,

    /// `Known(v)`: is this value DETERMINED - a decided boolean, or a
    /// number whose interval is a singleton? One op for both domains,
    /// because it asks the same question of both: is this abstract set a
    /// single element? It is what lets validity be an ordinary value
    /// instead of a side channel.
    Known,
    /// `SplitValid(d)` over the same operand as `Split(d)`: which lanes
    /// fall in the chosen outcome. This is what a free choice has no
    /// analogue of - the outcomes of a split PARTITION the lanes, so the
    /// primitive returns a (value, validity) pair and it is two nodes.
    SplitValid(u8),
    /// `Split(d)` after specialization: fragment `c` of an interval,
    /// `c` being a CONCRETE fork configuration rather than a runtime
    /// loop variable. `Frag` narrows, `FragOk` says whether the fragment
    /// is non-empty - together they are exactly `zi_fork_flr(x, c)`.
    ///
    /// Ordinary unary ops, which is the whole point: they intern and
    /// fold like anything else, so the configurations SHARE every node
    /// they agree on instead of the emitter running the tail of the
    /// frame once per configuration. See `specialize_into`.
    Frag(u8),
    FragOk(u8),
    /// `SplitOk` over the same operand: does this lane's interval span at
    /// most TWO floors? Below that the split is exact; above it there is
    /// no third outcome to put the remaining fragment in, so the lane
    /// deopts. OUTCOME-INDEPENDENT, hence no index - and unindexed on
    /// purpose, so two splits of the same operand share the one node. It
    /// exists so the validity chain accounts for every lane
    /// `zi_fork_flr` gives up on.
    SplitOk,

    // ---- cart lookups ----
    Mget,
    TileFlagAt,

    // ---- row key ----
    //
    // The one part of the graph that is not an abstract game value.
    // These read the REPRESENTATION of a value and produce machine
    // words; nothing feeds back the other way, so the value layer stays
    // exactly what the interpreter can be checked against.
    //
    // They are here rather than in a scalar loop inside `append` because
    // the fold is over CELLS - each step needs the last - while the
    // parallelism lives across LANES, and a graph node is already 16
    // lanes wide. Hash-consing then shares the whole prefix of the fold
    // over the cells that do not depend on the buttons, which is the
    // other half of the win. See `plans/successors.md`.
    /// A machine-word literal: the fold's seed.
    Word(u64),
    /// The representation BITS of one value, as a machine word per lane.
    /// Not a hash - just the bit pattern, packed so that two abstract
    /// values which differ differ here too.
    Bits,
    /// `Mix(cell, half)(acc, bits)` - one step of the fold. `half` picks
    /// which of the two accumulators (and so which mixing) this is; the
    /// pair of them is the 128-bit key. `cell` goes into the mixing, so
    /// the key is not invariant under moving a value between fields.
    Mix(u32, u8),
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
#[derive(Default, Clone)]
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

    /// For every node, which CHOICES can influence it - one bitmask,
    /// computed bottom-up in one pass (operands always precede their
    /// node). This used to be two functions over two vocabularies; it is
    /// one reachability question, and answering it once is what lets
    /// placement stop distinguishing "the button suffix" from "the fork
    /// loop nest".
    ///
    /// A choice that reaches no OUTPUT cannot affect any lane's result, so
    /// its outcomes collapse - a plain reachability fact, decided once for
    /// the whole kernel rather than per lane.
    pub fn choice_cones(&self) -> Vec<ChoiceSet> {
        let mut mask = vec![0 as ChoiceSet; self.nodes.len()];
        for (i, node) in self.nodes.iter().enumerate() {
            let mut m = match node.op {
                Op::Free(b) => Choice::Free(b).bit(),
                Op::Split(d) | Op::SplitValid(d) => Choice::Split(d).bit(),
                _ => 0,
            };
            for a in &node.args {
                m |= mask[*a as usize];
            }
            mask[i] = m;
        }
        mask
    }

    /// The cone restricted to FREE choices - the buttons, which every lane
    /// enumerates. Splits are excluded because their outcomes partition
    /// lanes rather than multiplying them, so the two answer different
    /// questions at an emit site even though one pass computes both.
    pub fn free_cones(&self) -> Vec<u8> {
        self.choice_cones().iter().map(|m| (*m & 0x3f) as u8).collect()
    }

    /// The cone restricted to SPLIT choices.
    pub fn split_cones(&self) -> Vec<u8> {
        self.choice_cones().iter().map(|m| (*m >> N_FREE) as u8).collect()
    }

    /// Rebuild this graph into `out` with the button bits replaced by
    /// constants, folding as it goes. Returns the mapping old -> new.
    ///
    /// This is the "duplicate, then fuse again" step: `out` is SHARED
    /// across all 2^6 specializations and hash-consed, so two button
    /// combinations that compute the same thing land on the SAME node ids,
    /// and comparing their output tuples decides whether they are the same
    /// successor state for every lane. E.g. `input` is
    /// `Sel(right, 1, Sel(left, -1, 0))`, so with right = true the whole
    /// thing folds to 1 whatever left is: {left,right} and {right} collapse.
    pub fn specialize_into(&self, frees: u8, out: &mut Graph) -> Vec<NodeId> {
        self.specialize_config_into(frees, None, out)
    }

    /// As `specialize_into`, and ALSO resolve the splits.
    ///
    /// `splits` is one bit per fork level, or `None` to leave
    /// `Op::Split`/`Op::SplitValid` standing - which is what the walk's
    /// kernels do, because they emit a fork as a runtime loop and their
    /// generated form is checked in byte-for-byte.
    ///
    /// Resolving a split here rather than at runtime is the same trade
    /// as resolving a button. A `Free` becomes a CONSTANT, which is why
    /// button assignments collapse so well; a `Split` cannot - it is a
    /// narrowing of a runtime value, not a constant - but it does not
    /// need to be. It needs to be ORDINARY ARITHMETIC, and `Frag` /
    /// `FragOk` are. They intern, they fold, and every node the
    /// configurations agree on is therefore ONE node, where the runtime
    /// loop re-executed the whole tail of the frame per configuration.
    ///
    /// The cost is the nodes that genuinely differ: those are emitted
    /// once per configuration, so a fork early in a frame duplicates
    /// whatever downstream of it actually depends on the fragment.
    /// That is the trade, and it is a measurement rather than an
    /// argument - see `plans/tracing.md`.
    pub fn specialize_config_into(
        &self,
        frees: u8,
        splits: Option<u8>,
        out: &mut Graph,
    ) -> Vec<NodeId> {
        let mut map: Vec<NodeId> = Vec::with_capacity(self.nodes.len());
        for node in &self.nodes {
            let arg = |map: &Vec<NodeId>, k: usize| map[node.args[k] as usize];
            let id = match (node.op.clone(), splits) {
                (Op::Free(b), _) => out.leaf(Op::ConstBool(frees & (1 << b) != 0)),
                (Op::Split(d), Some(s)) => {
                    out.fold(Op::Frag((s >> d) & 1), vec![arg(&map, 0)])
                }
                (Op::SplitValid(d), Some(s)) => {
                    out.fold(Op::FragOk((s >> d) & 1), vec![arg(&map, 0)])
                }
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

    /// Is operand order meaningless for this op? Structural interning is
    /// the ONLY sharing mechanism, so `a op b` and `b op a` are two nodes
    /// unless something puts them in a canonical order first.
    ///
    /// `Mul` is absent on purpose even though multiplication commutes:
    /// `arith` below is only monotone with the EXACT side second, so
    /// swapping can turn a form it evaluates into one it refuses. It gets
    /// its own one-directional rule instead.
    fn commutes(op: &Op) -> bool {
        matches!(op, Op::Add | Op::Min | Op::Max | Op::Eq | Op::And | Op::Or)
    }

    /// Add `op(args)`, normalizing and folding.
    ///
    /// Every rule here is EXACT with respect to `eval`: the node this
    /// returns evaluates to the same abstract value as the unfolded node
    /// would have, for every assignment of the leaves. Not "sound" -
    /// exact. A rule that merely refined the result would still be
    /// correct in isolation but would change which lanes survive `ok`,
    /// and `folding_is_exact` enumerates the tri-state assignments to
    /// keep that honest. Rules that WOULD refine (`x and not x` is false
    /// concretely but unknown in Kleene) are therefore left out.
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
            // Fragment 0 of any interval is non-empty, always: a single
            // floor keeps the whole interval, two floors keep the lower
            // one, and more than two floors also route the lane through
            // configuration 0 (`zi_span_ok` is what takes it off the
            // kernel). So `FragOk(0)` is a constant, and folding it is
            // what makes configuration 0 as cheap as no fork at all.
            //
            // EXACT, not a refinement: `eval`'s `FragOk(0)` arm returns
            // `Some(true)` for every operand, and the two are checked
            // against each other by `folding_is_exact`.
            Op::FragOk(0) => return self.leaf(Op::ConstBool(true)),
            // The one that matters: a decided condition picks its arm, so
            // the other arm (and whatever only it used) disappears.
            Op::Sel => {
                if let Some(c) = cbool(self, 0) {
                    return if c { args[1] } else { args[2] };
                }
                // Both arms the same value: the condition cannot matter,
                // decided or not. This is the fold that makes MERGING AT A
                // JOIN affordable - a per-cell merge of two branch outcomes
                // emits a select for every cell in the heap, and the
                // overwhelming majority of cells were not touched by either
                // arm.
                if args[1] == args[2] {
                    return args[1];
                }
                // A select between BOOLEANS with a constant arm is not a
                // select at all, it is boolean algebra - and this is the
                // shape the tracer produces at EVERY merge of a boolean
                // whose branch condition it could not decide, which is
                // most of the guard algebra in a frame. Each case is
                // exact in Kleene: with an undecided condition the select
                // joins its arms, and the join of `true` with x is
                // precisely `true or x`.
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
                // A negated comparison is the opposite comparison. Exact
                // on intervals as well as on numbers, because both are
                // decided by the same four corners. This is worth more
                // than the node it saves: it makes `x < y` and
                // `not (x >= y)` the SAME node, which structural
                // interning could never do.
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
                // Negation preserves decidedness, so asking of `not x` is
                // asking of `x`.
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
                // `false AND anything` is false even when the other side
                // is symbolic.
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
                // `true OR anything` is true even when the other side is
                // symbolic - the mirror of the AND rule above, and the
                // reason an OR is worth a node of its own.
                if x == Some(true) || y == Some(true) {
                    return self.leaf(Op::ConstBool(true));
                }
                match (x, y) {
                    (Some(_), Some(_)) => return self.leaf(Op::ConstBool(false)),
                    (Some(_), None) => return args[1],
                    (None, Some(_)) => return args[0],
                    _ => {}
                }
            }
            _ => {}
        }
        self.add(op, args)
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
        // kb0..kb5: the free choices the suffix is specialized on.
        if let Some(rest) = s.strip_prefix("kb") {
            if let Ok(bit) = rest.parse::<u8>() {
                return Ok(self.leaf(Op::Free(bit)));
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
        self.eval_inner(cells, false, None)
    }

    /// The same evaluator, but a node it cannot model becomes TOP for its
    /// kind instead of an error.
    ///
    /// `eval` is exact-or-nothing, which is right for a check and useless
    /// for a transformation: one `TileFlagAt` anywhere makes the whole
    /// graph unevaluable, and every real traced graph has hundreds. Top
    /// is the sound answer - "this node could be anything" - so every
    /// value derived from one stays sound, and the nodes that do NOT
    /// depend on it are still decided.
    ///
    /// Deliberately the same match arm-for-arm rather than a second
    /// evaluator: two implementations of `Abs` over intervals is exactly
    /// the kind of divergence that is impossible to notice.
    pub fn eval_lenient(&self, cells: &HashMap<u32, Val>) -> Result<Vec<Val>> {
        self.eval_inner(cells, true, None)
    }

    /// `eval_lenient` WITH the map, so `TileFlagAt` is decided instead of
    /// becoming top.
    ///
    /// The map is a constant. A collision test is only unknown because
    /// the evaluator was never given the room, not because the answer
    /// depends on anything it cannot see - and `TileFlagAt` is one of the
    /// two ops through which TOP enters a traced graph at all.
    pub fn eval_lenient_in(&self, cells: &HashMap<u32, Val>, room: &Room) -> Result<Vec<Val>> {
        self.eval_inner(cells, true, Some(room))
    }

    fn eval_inner(
        &self,
        cells: &HashMap<u32, Val>,
        lenient: bool,
        room: Option<&Room>,
    ) -> Result<Vec<Val>> {
        let full = Val::Num(Pico8NumInterval::new(
            Pico8Num::from_raw(i32::MIN),
            Pico8Num::from_raw(i32::MAX),
        ));
        let mut out: Vec<Val> = Vec::with_capacity(self.nodes.len());
        for (i, node) in self.nodes.iter().enumerate() {
            let a = |k: usize| -> Val { out[node.args[k] as usize] };
            let computed = (|| -> Result<Val> {
                Ok(match &node.op {
                Op::Const(lo, hi) => Val::Num(Pico8NumInterval::new(
                    Pico8Num::from_raw(*lo),
                    Pico8Num::from_raw(*hi),
                )),
                Op::ConstBool(b) => Val::Bool(Some(*b)),
                Op::Free(b) => bail!("node {}: free choice {} has no value outside a variant", i, b),
                // A split RESTRICTS its operand, so under `lenient` the
                // operand's own range still contains the result - which
                // is strictly better than top and costs nothing.
                Op::Split(d) if lenient => {
                    let _ = d;
                    a(0)
                }
                Op::Split(d) | Op::SplitValid(d) => {
                    bail!("node {}: split {} has no value outside an outcome", i, d)
                }
                Op::SplitOk => bail!("node {}: SplitOk needs the interval's floor span", i),
                // The RESOLVED fork: `zi_fork_flr`'s three cases, on one
                // interval instead of sixteen lanes. Exact, and it is
                // the definition `fold`'s `FragOk(0)` rule is checked
                // against.
                Op::Frag(c) | Op::FragOk(c) => {
                    let iv = a(0).as_num("Frag")?;
                    let (fl, fh) = (iv.low.flr(), iv.high.flr());
                    let two = fh == fl + Pico8Num::from_i16(1);
                    match (&node.op, *c) {
                        // Exactly two floors: fragment 0 is everything
                        // below the boundary, fragment 1 everything from
                        // it up. One floor, or more than two, leaves the
                        // interval alone in both fragments - fragment 1
                        // is simply not valid there.
                        (Op::Frag(_), 0) if two => {
                            Val::Num(Pico8NumInterval::new(iv.low, fh.next_smallest()))
                        }
                        (Op::Frag(_), _) if two => Val::Num(Pico8NumInterval::new(fh, iv.high)),
                        (Op::Frag(_), _) => a(0),
                        (_, 0) => Val::Bool(Some(true)),
                        _ => Val::Bool(Some(two)),
                    }
                }
                Op::Cell(c) => match cells.get(c) {
                    Some(v) => *v,
                    None => bail!("node {}: input cell {} was not supplied", i, c),
                },
                // CHECKED, because this evaluator runs with inputs at
                // TOP on purpose (`transpile::ival`) and a wrap there is
                // expected rather than a modeling bug. An evaluator that
                // panics on its own intended input is not usable as a
                // transformation.
                Op::Add => Val::Num(
                    a(0)
                        .as_num("Add")?
                        .checked_add(a(1).as_num("Add")?)
                        .ok_or_else(|| anyhow!("node {}: Add wrapped", i))?,
                ),
                Op::Sub => Val::Num(
                    a(0)
                        .as_num("Sub")?
                        .checked_sub(a(1).as_num("Sub")?)
                        .ok_or_else(|| anyhow!("node {}: Sub wrapped", i))?,
                ),
                Op::Neg => Val::Num(
                    a(0)
                        .as_num("Neg")?
                        .checked_neg()
                        .ok_or_else(|| anyhow!("node {}: Neg wrapped", i))?,
                ),
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
                Op::And => {
                    let (x, y) = (a(0).as_bool("And")?, a(1).as_bool("And")?);
                    // Short-circuit on a decided false, so `false AND
                    // unknown` is false rather than unknown.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(p && q),
                        (Some(false), None) | (None, Some(false)) => Some(false),
                        (Some(_), None) | (None, Some(_)) | (None, None) => None,
                    })
                }
                Op::Or => {
                    let (x, y) = (a(0).as_bool("Or")?, a(1).as_bool("Or")?);
                    // Short-circuit on a decided true, mirroring And.
                    Val::Bool(match (x, y) {
                        (Some(p), Some(q)) => Some(p || q),
                        (Some(true), None) | (None, Some(true)) => Some(true),
                        (Some(_), None) | (None, Some(_)) | (None, None) => None,
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
                Op::Known => Val::Bool(Some(match a(0) {
                    Val::Bool(b) => b.is_some(),
                    Val::Num(i) => i.to_number().is_some(),
                })),
                Op::TileFlagAt if room.is_some() => {
                    Self::tile_flag_over(room.unwrap(), a(0), a(1), a(2), a(3), a(4))?
                }
                Op::Mget | Op::TileFlagAt => {
                    bail!("{:?} needs the cart; not supported by the pure evaluator yet", node.op)
                }
                // The row-key layer has no abstract value: it reads the
                // REPRESENTATION of one, and `Val` is the value. Nothing
                // asks - the fold is built after `ival::fold` has run,
                // which is the only caller - and if something ever does,
                // the honest answer is that this evaluator is the wrong
                // tool rather than that the answer is top.
                Op::Word(_) | Op::Bits | Op::Mix(..) => {
                    bail!("node {}: {:?} is a row-key node, not an abstract value", i, node.op)
                }
                })
            })();
            // ONE place where `lenient` acts, rather than one per arm:
            // whatever the reason a node cannot be modelled, TOP for its
            // kind is the sound answer, and centralizing it means a new
            // unmodelled case cannot forget to be sound.
            let v = match computed {
                Ok(v) => v,
                Err(e) => {
                    if !lenient {
                        return Err(e);
                    }
                    Self::top_of(&node.op, &node.args, &out, full)
                }
            };
            out.push(v);
        }
        Ok(out)
    }

    /// The weakest value a node of this op could have: `Bool(None)` for
    /// the ops that produce booleans, the full numeric range otherwise.
    /// `Sel` follows its branches, since it produces whatever they do.
    fn top_of(op: &Op, args: &[NodeId], out: &[Val], full: Val) -> Val {
        match op {
            Op::ConstBool(_)
            | Op::Free(_)
            | Op::SplitValid(_)
            | Op::SplitOk
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
            | Op::TileFlagAt => Val::Bool(None),
            Op::Sel => match args.get(1).map(|x| out[*x as usize]) {
                Some(Val::Bool(_)) => Val::Bool(None),
                _ => full,
            },
            _ => full,
        }
    }

    /// `tile_flag_at` over INTERVALS of x and y.
    ///
    /// For one concrete (x, y) the answer is "does any tile in the
    /// w-by-h rectangle carry the flag". Over a BOX of positions there
    /// are two sound one-sided tests, and unknown lies between them.
    ///
    /// FALSE everywhere, if the UNION of all those rectangles holds no
    /// flagged tile. That union is itself one rectangle: start at the
    /// lowest corner, grow by how far the box spans.
    ///
    /// TRUE everywhere, if the INTERSECTION holds a flagged tile. That
    /// is also one rectangle: start at the highest corner, shrink by the
    /// span. It is empty once the box spans further than the rectangle
    /// is wide, which is why the size check comes first.
    ///
    /// Each test is one ordinary `solid_at` call on a derived rectangle,
    /// so this costs two map queries and introduces no new machinery.
    fn tile_flag_over(room: &Room, x: Val, y: Val, w: Val, h: Val, flag: Val) -> Result<Val> {
        // The size and flag must be exact. A symbolic hitbox is a
        // different question, and only flag 0 (solid) ever reaches the
        // graph - `trace::eval` raises on anything else.
        let ex = |v: Val, what: &str| -> Result<i32> {
            v.as_exact()
                .and_then(|n| n.as_i16())
                .map(|n| n as i32)
                .ok_or_else(|| anyhow!("tile_flag_at: {} is not an exact integer", what))
        };
        let (w, h) = (ex(w, "w")?, ex(h, "h")?);
        if ex(flag, "flag")? != 0 {
            bail!("tile_flag_at: only flag 0 (solid) is modelled");
        }
        // An interval covers every integer from floor(low) to
        // floor(high). Non-integer coordinates fail concretely, so
        // covering them here is conservative rather than wrong.
        let span = |v: Val, what: &str| -> Result<(i32, i32)> {
            let i = v.as_num(what)?;
            let lo = i.low.flr().as_i16().ok_or_else(|| anyhow!("{}: unbounded", what))? as i32;
            let hi = i.high.flr().as_i16().ok_or_else(|| anyhow!("{}: unbounded", what))? as i32;
            Ok((lo, hi))
        };
        let (xlo, xhi) = span(x, "x")?;
        let (ylo, yhi) = span(y, "y")?;
        // A room is 16 tiles across and `solid_at` clamps to it, so any
        // span past the room edge is the whole room. Clamping also keeps
        // the derived rectangle inside i16 when an input is at TOP.
        let cap = |n: i32| -> i16 { n.clamp(-4096, 4096) as i16 };
        let (dx, dy) = (xhi - xlo, yhi - ylo);
        let solid = |px: i32, py: i32, pw: i32, ph: i32| -> Result<bool> {
            room.cache.solid_at(&room.cart, cap(px), cap(py), cap(pw), cap(ph))
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
        // Checked for the same reason as `Add`: at TOP a scale wraps,
        // and the caller that wants that has a sound answer for it.
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
    fn folding_is_exact_not_merely_sound() {
        // Every rule in `fold` has to be EXACT: `fold(op, args)` and
        // `add(op, args)` must evaluate to the SAME abstract value at
        // every assignment of the leaves. A rule that merely refined the
        // answer would be correct in isolation and would still change
        // which lanes survive `ok` and which rows dedup together, so "it
        // can only help" is not a defence. This enumerates rather than
        // arguing - it is the reason `x and not x -> false` is absent,
        // since Kleene says unknown there and `false` would be a
        // refinement.
        let mut g = Graph::new();
        let b: Vec<NodeId> = (0..3).map(|i| g.leaf(Op::Cell(i))).collect();
        let nums: Vec<NodeId> = (10..12).map(|i| g.leaf(Op::Cell(i))).collect();
        let tt = g.leaf(Op::ConstBool(true));
        let ff = g.leaf(Op::ConstBool(false));

        // A pool of boolean subexpressions, built with `add` so that the
        // pool itself is unfolded and the rules have something to bite
        // on: a `Not`, a comparison to flip, a nested `Not`.
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
        // A guard against the test silently checking nothing, and against
        // the pool shrinking to the point where no rule fires.
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
        // Structural interning is the only sharing mechanism, so without
        // this `a + b` and `b + a` are two nodes and everything built on
        // them diverges. `add` is the raw constructor and still does not
        // normalize - that is what makes it usable as the control in
        // `folding_is_exact_not_merely_sound`.
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
        // Multiplication DOES commute, but `eval` is only monotone with
        // the exact side second, so the rule is one-directional: a
        // constant moves right, and two symbolic operands are left alone.
        let k = g.leaf(Op::Const(0, 0));
        assert_eq!(g.fold(Op::Mul, vec![k, a]), g.fold(Op::Mul, vec![a, k]));
        assert_ne!(g.fold(Op::Mul, vec![a, b]), g.fold(Op::Mul, vec![b, a]));
    }

    #[test]
    fn a_select_between_boolean_constants_is_boolean_algebra() {
        // The shape the tracer produces at every merge of a boolean whose
        // branch condition it could not decide. `if c then true else x`
        // is `c or x`, and saying so costs one node instead of three.
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
    fn a_negated_comparison_is_the_opposite_comparison() {
        // Worth more than the `Not` it saves: it makes `x < y` and
        // `not (x >= y)` the SAME node, which structural interning could
        // never do on its own.
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
        // The fold that makes merging at a join affordable. A per-cell
        // merge of two branch outcomes proposes a select for EVERY cell in
        // the heap; almost none of them were touched by either arm, and
        // without this each one would cost a node - and, at the emit site,
        // a `Known(cond)` validity conjunct that would deopt lanes over a
        // value that cannot depend on the condition.
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
    fn one_cone_pass_answers_for_both_kinds_of_choice() {
        // Free choices and splits used to need two reachability passes over
        // two vocabularies. They are one question - "which specialization
        // points reach this node" - and the answer must stay separable,
        // because the two are emitted differently: frees multiply the
        // variants, splits partition the lanes.
        let mut g = Graph::new();
        let c = g.leaf(Op::Cell(1));
        let kb = g.leaf(Op::Free(3));
        let iv = g.add(Op::Split(1), vec![c]);
        let both = g.add(Op::Sel, vec![kb, iv, c]);
        let cones = g.choice_cones();
        assert_eq!(
            Choice::all_in(cones[both as usize]),
            vec![Choice::Free(3), Choice::Split(1)]
        );
        assert_eq!(g.free_cones()[both as usize], 1 << 3);
        assert_eq!(g.split_cones()[both as usize], 1 << 1);
        // A node under neither is under neither.
        assert_eq!(cones[c as usize], 0);
    }

    #[test]
    fn specializing_a_free_choice_collapses_the_variants_that_agree() {
        // `input` is Sel(right, 1, Sel(left, -1, 0)): with right = true the
        // whole thing is 1 whatever left is, so {left,right} and {right}
        // land on the same node and their successor states are the same.
        let mut g = Graph::new();
        let right = g.leaf(Op::Free(1));
        let left = g.leaf(Op::Free(0));
        let one = g.leaf(Op::Const(n(1).as_raw_u32() as i32, n(1).as_raw_u32() as i32));
        let neg = g.leaf(Op::Const(n(-1).as_raw_u32() as i32, n(-1).as_raw_u32() as i32));
        let zero = g.leaf(Op::Const(0, 0));
        let inner = g.add(Op::Sel, vec![left, neg, zero]);
        let input = g.add(Op::Sel, vec![right, one, inner]);

        let mut shared = Graph::new();
        let sig = |m: u8, shared: &mut Graph| g.specialize_into(m, shared)[input as usize];
        let r_only = sig(0b10, &mut shared);
        let both = sig(0b11, &mut shared);
        assert_eq!(r_only, both, "right dominates left; these are one variant");
        let l_only = sig(0b01, &mut shared);
        let none = sig(0b00, &mut shared);
        assert_ne!(l_only, none);
        assert_ne!(l_only, both);
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
