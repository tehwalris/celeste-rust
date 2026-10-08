//! The value domain the tracer is generic over. One interpreter, two
//! instantiations, so the oracle cannot drift from the tracer: `Concrete`
//! (`Pico8Num`/`bool`, the ORACLE, which also builds the initial heap) and
//! `Symbolic` (`NodeId`s into a `transpile::graph::Graph`, the TRACER).
//! `decide` answers a condition NOW; on `None` the interpreter traces both
//! arms and merges with `sel_*`. The HEAP stays concrete (`count(objects)`,
//! `#t`, field presence are facts); only game data is symbolic.

use std::fmt::Debug;

use anyhow::{bail, Result};

use crate::pico8_num::Pico8Num as P8;
use crate::transpile::graph::{Graph, NodeId, Op};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
}

/// A numeric builtin that is the same shape in both domains.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Fun1 {
    Neg,
    Abs,
    Flr,
    Sin,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Fun2 {
    Min,
    Max,
}

/// Which op family carries a fork (the memo's second key): `Floors` and
/// `Ints` over one value resolve differently and may not share a choice.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ForkKind {
    Floors,
    Ints,
}

/// The origin of the forks `Domain::unknown_bool` mints: the six buttons, in
/// `__reset_button_states`' order (how `verify::at_buttons` finds them).
pub const UNKNOWN_BOOL_ORIGIN: &str = "__new_unknown_boolean";

/// HOW a fork partitions its value's set into configurations.
pub enum Partition {
    /// Cut an interval at the fork grid's cell edges (`Split` -> `Frag`), so
    /// `flr` of each is one number.
    Floors { ways: u8 },
    /// Cut an interval of whole numbers into the numbers, each EXACT
    /// (`SplitInt` -> `IntFrag`). `memo` false keeps two forks of one node
    /// independent (`p_jump`/`p_dash` both fork `[0, 1<<16]`).
    Ints { ways: u8, memo: bool },
}

impl Partition {
    fn kind(&self) -> ForkKind {
        match self {
            Partition::Floors { .. } => ForkKind::Floors,
            Partition::Ints { .. } => ForkKind::Ints,
        }
    }

    /// The fork's arity.
    fn ways(&self) -> u8 {
        match self {
            Partition::Floors { ways } | Partition::Ints { ways, .. } => *ways,
        }
    }

    fn memoized(&self) -> bool {
        match self {
            Partition::Floors { .. } => true,
            Partition::Ints { memo, .. } => *memo,
        }
    }
}

/// What a fork hands back: this configuration's fragment, which lanes fall in
/// it, and the choice dimension minted.
pub struct Fork {
    pub value: NodeId,
    pub valid: NodeId,
    pub id: u8,
}

pub trait Domain {
    type Num: Clone + Debug + PartialEq;
    type Bool: Clone + Debug + PartialEq;

    fn num(&mut self, v: P8) -> Self::Num;
    fn boolean(&mut self, b: bool) -> Self::Bool;

    fn arith(&mut self, op: Arith, a: &Self::Num, b: &Self::Num) -> Result<Self::Num>;
    fn fun1(&mut self, f: Fun1, a: &Self::Num) -> Result<Self::Num>;
    fn fun2(&mut self, f: Fun2, a: &Self::Num, b: &Self::Num) -> Result<Self::Num>;
    fn compare(&mut self, op: Cmp, a: &Self::Num, b: &Self::Num) -> Result<Self::Bool>;
    fn not(&mut self, a: &Self::Bool) -> Self::Bool;
    fn and(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool;
    /// De Morgan by default; `Symbolic` builds `Op::Or` (one node, interns
    /// symmetrically).
    fn or(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool {
        let (na, nb) = (self.not(a), self.not(b));
        let both = self.and(&na, &nb);
        self.not(&both)
    }

    /// The value of this condition, if known; `None` runs both arms.
    fn decide(&self, c: &Self::Bool) -> Option<bool>;

    /// Merge two values under an undecided condition (never reached by
    /// `Concrete`).
    fn sel_num(&mut self, c: &Self::Bool, t: &Self::Num, f: &Self::Num) -> Self::Num;
    fn sel_bool(&mut self, c: &Self::Bool, t: &Self::Bool, f: &Self::Bool) -> Self::Bool;

    /// `mget(x, y)` with coordinates unknown at trace time (`zn_mget`).
    fn mget(&mut self, x: &Self::Num, y: &Self::Num) -> Result<Self::Num>;

    /// `tile_flag_at(x, y, w, h, flag)` with coordinates unknown at trace
    /// time: a primitive (`zn_tile_flag_at`), since the Lua's scan bounds
    /// derive from x and y. The caller folds the known case.
    fn tile_flag_at(
        &mut self,
        x: &Self::Num,
        y: &Self::Num,
        w: &Self::Num,
        h: &Self::Num,
        flag: &Self::Num,
    ) -> Result<Self::Bool>;

    /// A fresh UNKNOWN boolean, a free choice of the search (`Concrete` refuses).
    fn unknown_bool(&mut self) -> Result<Self::Bool>;

    /// A number known only to lie in `[lo, hi]` (`rnd`); `Concrete` refuses.
    fn range_num(&mut self, lo: P8, hi: P8) -> Result<Self::Num> {
        bail!("a number in [{lo:?}, {hi:?}] has no concrete value - this domain runs real inputs only")
    }

    /// How much graph there is, for the tracer's budget check.
    fn node_count(&self) -> usize {
        0
    }

    /// A number this value definitely is. Needed where the HEAP depends on it
    /// (index, key, loop bound); unknown is a refusal.
    fn as_const(&self, v: &Self::Num) -> Option<P8>;

    /// What this value IS, for an error message.
    fn describe(&self, _v: &Self::Num) -> String {
        "<opaque>".to_string()
    }

    /// Is this value an INTERVAL (a set of numbers)? Forking a single number
    /// buys nothing.
    fn is_interval(&self, _v: &Self::Num) -> bool {
        false
    }

    /// Is a merged value a SELECT the kernel reads by its condition (`Op::Sel`)?
    /// Only a select reads the merge's condition, so only it can make the
    /// merge refuse (`state::merge`).
    fn is_select_num(&self, _v: &Self::Num) -> bool {
        false
    }

    /// `is_select_num` for a boolean.
    fn is_select_bool(&self, _b: &Self::Bool) -> bool {
        false
    }

    /// Fork at `flr` (`__split_by_flr`): `flr` of an interval is not a
    /// function, so the fragment is a choice of arity `ways`, enumerated by
    /// specialization. Returns the restricted value and which lanes fall in
    /// it; the identity on exact values.
    fn fork_flr(&mut self, v: &Self::Num, _ways: u8) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// `n`, an operator with an own error (`trace::error`), was computed
    /// where `at` holds; its error holds only there.
    fn evaluated_at(&mut self, _n: &Self::Num, _at: &Self::Bool) {}

    /// The arity of `__split_by_flr` on `v`: `MOVE_WAYS`, or with a known
    /// static range the most grid cells one piece crosses (`SplitOk` checks).
    fn flr_ways(&mut self, _v: &Self::Num) -> u8 {
        MOVE_WAYS
    }

    /// The join of a merge on a condition NO LANE can decide, with arms every
    /// lane holds alike: the hull of two literals, or the unknown number.
    /// `None` keeps the ordinary select.
    fn join_num_independent(&mut self, _c: &Self::Bool, _t: &Self::Num, _f: &Self::Num) -> Option<Self::Num> {
        None
    }

    /// `join_num_independent` for booleans: different arms join to an atom.
    fn join_bool_independent(&mut self, _c: &Self::Bool, _t: &Self::Bool, _f: &Self::Bool) -> Option<Self::Bool> {
        None
    }

    /// Does undecided `c` read an unknown atom (a countdown's does not count:
    /// `Symbolic::countdown_atom`)? No lane decides one, so a merge that
    /// would select on `c` is two successors instead.
    fn reads_unknown_atom(&mut self, _c: &Self::Bool) -> bool {
        false
    }

    /// The next comparisons read a COUNTDOWN (the interpreter's hint, from
    /// the field read: `COUNTDOWN_FIELDS`): an unknown number there mints a
    /// countdown atom (`Symbolic::countdown_atom`) also with the fly fruit
    /// unknown, where an unknown number may be the fruit's.
    fn set_countdown_hint(&mut self, _on: bool) {}

    /// `__split_by_flr` of a literal interval: its integer fragments, run as
    /// separate trace states and rejoined when the call returns. `None`: an
    /// ordinary per-lane fork.
    fn literal_fragments(&mut self, _v: &Self::Num) -> Result<Option<Vec<Self::Num>>> {
        Ok(None)
    }

    /// A fresh undecided boolean every lane holds alike (rejoined fragments
    /// merge on it).
    fn undecided_atom(&mut self) -> Result<Self::Bool> {
        bail!("this domain has no undecided booleans")
    }

    /// How many undecided atoms this frame handed out (marked per call).
    fn atoms_minted(&self) -> u32 {
        0
    }

    /// A boolean reading an atom minted at or after `since` that ESCAPES its
    /// call becomes a fork, restricted per lane to its possible values, so
    /// later reads agree per configuration. `None` if it reads no such atom.
    fn escaped_atom(&mut self, _b: &Self::Bool, _since: u32, _origin: &dyn Fn(&Self) -> String) -> Option<Self::Bool> {
        None
    }

    /// A number for a diagnostic (a fork's origin): its value if known.
    fn describe_num(&self, n: &Self::Num) -> String {
        format!("{n:?}")
    }
}

/// The tag of a countdown's atom id (`Symbolic::countdown_atom`).
const COUNTDOWN_ATOM: u32 = 1 << 31;

/// Is atom id `k` a countdown's (three-valued)? `u32::MAX` is the shared
/// output unknown (`Symbolic::unknown_bool_output`), not an atom.
fn is_countdown_atom(k: u32) -> bool {
    k != u32::MAX && k & COUNTDOWN_ATOM != 0
}

/// How many atoms an escaping value may read before its fork is left
/// unrestricted (the restriction enumerates their assignments).
const ESCAPE_ATOMS: usize = 6;

// ---------------------------------------------------------------- concrete

#[derive(Default)]
pub struct Concrete;

impl Domain for Concrete {
    type Num = P8;
    type Bool = bool;

    fn num(&mut self, v: P8) -> P8 {
        v
    }
    fn boolean(&mut self, b: bool) -> bool {
        b
    }
    fn arith(&mut self, op: Arith, a: &P8, b: &P8) -> Result<P8> {
        Ok(match op {
            Arith::Add => *a + *b,
            Arith::Sub => *a - *b,
            Arith::Mul => *a * *b,
            Arith::Div => *a / *b,
            Arith::Rem => *a % *b,
        })
    }
    fn fun1(&mut self, f: Fun1, a: &P8) -> Result<P8> {
        Ok(match f {
            Fun1::Neg => -*a,
            Fun1::Abs => a.abs(),
            Fun1::Flr => a.flr(),
            Fun1::Sin => a.pico8_sin(),
        })
    }
    fn fun2(&mut self, f: Fun2, a: &P8, b: &P8) -> Result<P8> {
        Ok(match f {
            Fun2::Min => (*a).min(*b),
            Fun2::Max => (*a).max(*b),
        })
    }
    fn compare(&mut self, op: Cmp, a: &P8, b: &P8) -> Result<bool> {
        Ok(match op {
            Cmp::Lt => a < b,
            Cmp::Le => a <= b,
            Cmp::Gt => a > b,
            Cmp::Ge => a >= b,
            Cmp::Eq => a == b,
        })
    }
    fn not(&mut self, a: &bool) -> bool {
        !*a
    }
    fn and(&mut self, a: &bool, b: &bool) -> bool {
        *a && *b
    }
    fn decide(&self, c: &bool) -> Option<bool> {
        Some(*c)
    }
    fn sel_num(&mut self, c: &bool, t: &P8, f: &P8) -> P8 {
        if *c {
            *t
        } else {
            *f
        }
    }
    fn sel_bool(&mut self, c: &bool, t: &bool, f: &bool) -> bool {
        if *c {
            *t
        } else {
            *f
        }
    }
    fn mget(&mut self, _: &P8, _: &P8) -> Result<P8> {
        bail!("mget with unknown coordinates cannot happen concretely")
    }
    fn tile_flag_at(&mut self, _: &P8, _: &P8, _: &P8, _: &P8, _: &P8) -> Result<bool> {
        bail!("tile_flag_at with unknown coordinates cannot happen concretely")
    }
    fn unknown_bool(&mut self) -> Result<bool> {
        bail!("the concrete domain has no unknown booleans - supply real inputs")
    }
    fn as_const(&self, v: &P8) -> Option<P8> {
        Some(*v)
    }

    fn describe(&self, v: &P8) -> String {
        format!("{:?}", v)
    }
}

// ---------------------------------------------------------------- symbolic

/// The fields the cart counts down and compares with 0, a near level's
/// unknown numbers (`widen::floor_timer_paths`, `widen::phase_paths`): a
/// comparison reading one is a countdown's (`Domain::set_countdown_hint`).
pub const COUNTDOWN_FIELDS: [&str; 4] = ["delay", "timer", "hide_in", "hide_for"];

/// The tracing domain. Owns the graph it is building.
#[derive(Default, Clone)]
pub struct Symbolic {
    pub graph: Graph,
    /// FORK choices handed out this frame (the fork ids), buttons included.
    pub forks: u8,
    /// `flr_ways` takes a static range's full width (level -1, whose speed
    /// is a range at every node).
    pub uncapped_ways: bool,
    /// Held buttons unknown (`Level::held`).
    pub held_unknown: bool,
    /// The fly fruit unknown (`Level::fruit`): inputs replaced, literal
    /// arithmetic folds, undecidable merges join literal arms.
    pub fruit_unknown: bool,
    /// Fall floors widened except where the player overlaps one
    /// (`Level::floors_near`, `widen::widen_near_floors`).
    pub floors_near: bool,
    /// Moving platforms unknown (`Level::platforms`).
    pub platforms_unknown: bool,
    /// How many `Op::UnknownBool` atoms this frame handed out.
    pub unknown_atoms: u32,
    /// `Domain::set_countdown_hint`.
    countdown_hint: bool,
    /// `escaped_atom`'s memo, cleared with `unknown_atoms` (ids restart).
    pub escaped: rustc_hash::FxHashMap<NodeId, NodeId>,
    /// What each `both_values` fork was made for, for the kernel dump.
    pub fork_origins: Vec<(u8, String)>,
    /// WHERE each node with an own error was evaluated (the OR of its path
    /// guards). Per trace.
    pub evaluated: rustc_hash::FxHashMap<NodeId, NodeId>,
    /// Keep undecided selects as selects (level -1); off, they become forks.
    pub no_known_forks: bool,
    /// THE PLATFORM WORLDS: every arrangement of the moving platforms' `(x,
    /// last, rem.x, spd.x)`. The split pass decides comparisons per world.
    pub worlds: Option<std::sync::Arc<Vec<Vec<[i32; crate::concrete::WORLD_FIELDS]>>>>,
    /// This frame's platforms' `x` input cells, in the worlds' order.
    pub platform_cells: Vec<NodeId>,
    /// `lane_independent`'s memo; structural, so it outlives a frame.
    lane_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// `reads_unknown_atom`'s memo: structural, like `lane_memo`.
    atom_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// Fork choices handed out THIS FRAME by `(value, kind)`: a second fork
    /// of one value reuses the first's choice instead of an empty dimension.
    /// Cleared with `forks`.
    fork_memo: std::collections::HashMap<(NodeId, ForkKind), (u8, (NodeId, NodeId))>,
    /// Input cells holding an INTERVAL (`rem.x`/`rem.y`); a value is an
    /// interval exactly when computed from one.
    pub ival_cells: std::collections::BTreeSet<u32>,
    /// `is_interval`'s memo. This and the four below depend on `ival_cells`
    /// (dropped by `forget_intervals`).
    ival_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `abstractness`'s memo.
    abstract_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `lane_undecidable`'s memo.
    undecidable_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `abstract_beneath_lane_ops`'s memo.
    beneath_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `may_answers`'s memo: `(may_true, may_false)`.
    may_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, (NodeId, NodeId)>>,
    /// STATIC RANGES of input cells, and `range_of`'s memo; a comparison they
    /// decide folds. Per frame.
    pub ranges: std::collections::HashMap<NodeId, (i64, i64)>,
    range_memo: std::collections::HashMap<NodeId, Option<Pieces>>,
    /// Comparisons decided from ranges this frame (a probe stat).
    pub range_folds: u64,
}

pub use crate::transpile::graph::Pieces;

impl Symbolic {
    /// Forget the static ranges (a new frame, new cells).
    pub fn clear_ranges(&mut self) {
        self.ranges.clear();
        self.range_memo.clear();
        self.range_folds = 0;
    }

    /// `ival_cells` changed: `is_interval`'s memo no longer holds.
    pub fn forget_intervals(&mut self) {
        self.ival_memo.get_mut().clear();
        self.abstract_memo.get_mut().clear();
        self.undecidable_memo.get_mut().clear();
        self.beneath_memo.get_mut().clear();
        self.may_memo.get_mut().clear();
    }

    /// THE FORK TRIGGER: can ONE LANE hold this condition both ways? Not
    /// `abstractness`: a set-valued quantity may still be one value per lane.
    /// `TileFlagAt`/`Mget` are lane-decidable by instruction; `Flr` by
    /// assertion (`zi_flr_ok`, its own error, not a type fact, so
    /// `Known(Flr(..))` must not fold away). Exhaustive over `Op`: no
    /// catch-all to hide the next undecidable op.
    pub fn lane_undecidable(&self, b: NodeId) -> bool {
        fn go(d: &Symbolic, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(x) = memo.get(&n) {
                return *x;
            }
            let node = d.graph.get(n);
            let args = node.args.clone();
            let any = |memo: &mut rustc_hash::FxHashMap<NodeId, bool>, xs: &[NodeId]| xs.iter().any(|x| go(d, memo, *x));
            let r = match node.op {
                // THE source: an operand spans values, judged BENEATH the
                // lane-decidable operators, or the exclusions never bite.
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => args.iter().any(|a| d.abstract_beneath_lane_ops(*a)),
                Op::UnknownBool(_) => true,
                Op::Not | Op::And | Op::Or | Op::Sel => any(memo, &args),
                // Lane-decidable by instruction.
                Op::TileFlagAt | Op::Mget => false,
                // Lane-decidable by assertion (`own_error`, not a type fact).
                Op::Flr => false,
                // A mask query.
                Op::Known => false,
                // Validity/coverage masks: per-lane comparisons.
                Op::SplitValid(_) | Op::SplitOk(_) | Op::FragOk(_) | Op::NoWrap => false,
                Op::ConstBool(_) => false,
                // Not a condition.
                _ => false,
            };
            memo.insert(n, r);
            r
        }
        go(self, &mut self.undecidable_memo.borrow_mut(), b)
    }

    /// `abstractness`, STOPPING at operators a lane decides itself (`Flr`,
    /// `TileFlagAt`, `Mget`): how comparison operands are judged, without
    /// spurious forks.
    pub fn abstract_beneath_lane_ops(&self, n: NodeId) -> bool {
        fn go(d: &Symbolic, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(x) = memo.get(&n) {
                return *x;
            }
            let node = d.graph.get(n);
            let args = node.args.clone();
            let r = match node.op {
                // A lane decides these whatever their operands.
                Op::Flr | Op::TileFlagAt | Op::Mget => false,
                Op::Cell(c) => d.ival_cells.contains(&c),
                Op::Const(lo, hi) => lo != hi,
                Op::Span => true,
                Op::UnknownNum | Op::UnknownBool(_) => true,
                Op::Split(_) | Op::Frag(_) => true,
                Op::SplitInt(_) | Op::IntFrag(_) | Op::Lo | Op::Hi => false,
                Op::ConstBool(_) | Op::Known | Op::NoWrap => false,
                // THE CONDITION DOES NOT MAKE THE RESULT A SET: `Sel(c, 5, 7)`
                // is one of two numbers, chosen by forking `c` (the opposite of
                // `lane_undecidable` for booleans, deliberately).
                Op::Sel => args[1..].iter().any(|a| go(d, memo, *a)),
                _ => args.iter().any(|a| go(d, memo, *a)),
            };
            memo.insert(n, r);
            r
        }
        go(self, &mut self.beneath_memo.borrow_mut(), n)
    }

    /// TYPE PROPAGATION: does this node denote a SET of concrete values? Not
    /// `is_interval` (ZI/ZN typing): here a `Sel` counts its CONDITION (what
    /// makes it unevaluable), and `Flr` FOLLOWS ITS OPERAND (a singleton only
    /// on pain of its own error; reading that in would fold `Known(Flr(..))`
    /// away unsoundly). Every `Op` listed, no catch-all.
    pub fn abstractness(&self, n: NodeId) -> bool {
        fn go(g: &Graph, ival: &std::collections::BTreeSet<u32>, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(b) = memo.get(&n) {
                return *b;
            }
            let node = g.get(n);
            let any = |memo: &mut rustc_hash::FxHashMap<NodeId, bool>, xs: &[NodeId]| xs.iter().any(|x| go(g, ival, memo, *x));
            let args = node.args.clone();
            let r = match node.op {
                // Singletons by construction.
                Op::ConstBool(_) => false,
                Op::Const(lo, hi) => lo != hi,
                Op::Cell(c) => ival.contains(&c),
                // The genuinely unknown atoms.
                Op::UnknownNum | Op::UnknownBool(_) => true,
                // A span exists because its bounds are different nodes.
                Op::Span => true,
                // A fragment is a narrower interval; validity/coverage masks
                // follow the operand.
                Op::Split(_) | Op::Frag(_) => true,
                Op::SplitInt(_) | Op::IntFrag(_) => false,
                Op::SplitValid(_) | Op::SplitOk(_) | Op::FragOk(_) => any(memo, &args),
                // The ends of an interval are exact numbers.
                Op::Lo | Op::Hi => false,
                // A premise is a mask query: every lane decides it.
                Op::Known | Op::NoWrap => false,
                // PARTIAL: a singleton only on pain of error.
                Op::Flr => any(memo, &args),
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Min | Op::Max | Op::Sin => any(memo, &args),
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => any(memo, &args),
                Op::Not | Op::And | Op::Or => any(memo, &args),
                // The condition counts.
                Op::Sel => any(memo, &args),
                // Data, addressed by coordinates that may be sets.
                Op::Mget | Op::TileFlagAt => any(memo, &args),
            };
            memo.insert(n, r);
            r
        }
        go(&self.graph, &self.ival_cells, &mut self.abstract_memo.borrow_mut(), n)
    }

    /// The static range of `n` (`graph::pieces_of` over the seeded cells).
    pub fn range_of(&mut self, n: NodeId) -> Option<Pieces> {
        crate::transpile::graph::pieces_of(&self.graph, &self.ranges, &mut self.range_memo, n)
    }

    /// The unknown number (`Op::UnknownNum`).
    pub fn unknown_num(&mut self) -> NodeId {
        self.graph.leaf(Op::UnknownNum)
    }

    fn is_unknown_num(&self, n: NodeId) -> bool {
        matches!(self.graph.get(n).op, Op::UnknownNum)
    }

    /// `n` as `Sel(c, t, f)` with an arm the unknown number (a countdown reset
    /// on some lanes). Operations distribute over the arms, so the unknown is
    /// never a graph arithmetic operand (no kernel computes that). EXACT:
    /// `op(Sel(c, t, f)) = Sel(c, op(t), op(f))`, same error.
    fn unknown_select(&self, n: NodeId) -> Option<(NodeId, NodeId, NodeId)> {
        fn holds(g: &Graph, n: NodeId) -> bool {
            let node = g.get(n);
            match node.op {
                Op::UnknownNum => true,
                Op::Sel => holds(g, node.args[1]) || holds(g, node.args[2]),
                _ => false,
            }
        }
        let node = self.graph.get(n);
        (node.op == Op::Sel && holds(&self.graph, n)).then(|| (node.args[0], node.args[1], node.args[2]))
    }

    /// Is anything unknown in the traced set (fly fruit, platforms)? Enables
    /// literal folding, independent joins and literal splits.
    pub fn unknowns(&self) -> bool {
        self.fruit_unknown || self.platforms_unknown
    }

    /// `n` as `base + literal` (either side), else `n + 0`.
    fn base_plus_literal(&self, n: NodeId) -> (NodeId, (i32, i32)) {
        let node = self.graph.get(n);
        if let Op::Add = node.op {
            for (b, k) in [(node.args[0], node.args[1]), (node.args[1], node.args[0])] {
                if let Op::Const(lo, hi) = self.graph.get(k).op {
                    return (b, (lo, hi));
                }
            }
        }
        (n, (0, 0))
    }

    /// Does boolean `n` read a comparison with an INTERVAL operand (one a lane
    /// can hold undecided)?
    pub fn reads_interval_cmp(&self, n: NodeId, memo: &mut std::collections::HashMap<NodeId, bool>) -> bool {
        if let Some(b) = memo.get(&n) {
            return *b;
        }
        let node = self.graph.get(n);
        let r = match node.op {
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => node.args.iter().any(|a| self.is_interval(a)),
            Op::Not | Op::And | Op::Or | Op::Sel => {
                let args = node.args.clone();
                args.iter().any(|a| self.reads_interval_cmp(*a, memo))
            }
            _ => false,
        };
        memo.insert(n, r);
        r
    }

    /// For boolean `n`: where a lane CAN answer true, and where false (a
    /// cover). Comparisons read operand ends (`a < b` may hold iff `lo(a) <
    /// hi(b)`), lane-decided nodes give `(n, not n)`, connectives combine.
    /// Judged by `lane_undecidable`, THE SAME PREDICATE THAT DECIDES WHAT TO
    /// FORK: this is a fork's validity, so the two must agree.
    pub fn may_answers(&mut self, n: NodeId) -> (NodeId, NodeId) {
        // Memoised: a DAG, re-walked per path otherwise.
        if let Some(hit) = self.may_memo.borrow().get(&n) {
            return *hit;
        }
        let out = self.may_answers_uncached(n);
        self.may_memo.borrow_mut().insert(n, out);
        out
    }

    fn may_answers_uncached(&mut self, n: NodeId) -> (NodeId, NodeId) {
        if !self.lane_undecidable(n) {
            let nn = self.graph.fold(Op::Not, vec![n]);
            return (n, nn);
        }
        let (op, args) = {
            let node = self.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        let yes = self.graph.leaf(Op::ConstBool(true));
        match op {
            Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                let (alo, ahi) = (self.graph.fold(Op::Lo, vec![args[0]]), self.graph.fold(Op::Hi, vec![args[0]]));
                let (blo, bhi) = (self.graph.fold(Op::Lo, vec![args[1]]), self.graph.fold(Op::Hi, vec![args[1]]));
                let ((t, ta, tb), (f, fa, fb)) = match op {
                    Op::Lt => ((Op::Lt, alo, bhi), (Op::Ge, ahi, blo)),
                    Op::Le => ((Op::Le, alo, bhi), (Op::Gt, ahi, blo)),
                    Op::Gt => ((Op::Gt, ahi, blo), (Op::Le, alo, bhi)),
                    _ => ((Op::Ge, ahi, blo), (Op::Lt, alo, bhi)),
                };
                (self.graph.fold(t, vec![ta, tb]), self.graph.fold(f, vec![fa, fb]))
            }
            // `a == b` may hold iff the ranges meet, and fail iff they are not
            // one single number. Only where neither operand reads an unknown
            // (`Op::UnknownNum`/`UnknownBool`), so `Lo`/`Hi` are an interval
            // number's ends. Per node, not per frame: with the fly fruit
            // unknown a near floor's `state == 2` on an exact hidden floor
            // gave both sides live, the solid one with the player inside
            // (room (3,0) `r0sxhfn` f42, a coverage gap).
            Op::Eq if !self.reads_unknown(&args) => {
                let (alo, ahi) = (self.graph.fold(Op::Lo, vec![args[0]]), self.graph.fold(Op::Hi, vec![args[0]]));
                let (blo, bhi) = (self.graph.fold(Op::Lo, vec![args[1]]), self.graph.fold(Op::Hi, vec![args[1]]));
                let (meet_a, meet_b) = (self.graph.fold(Op::Le, vec![alo, bhi]), self.graph.fold(Op::Le, vec![blo, ahi]));
                let (below, above) = (self.graph.fold(Op::Lt, vec![alo, bhi]), self.graph.fold(Op::Gt, vec![ahi, blo]));
                (self.graph.fold(Op::And, vec![meet_a, meet_b]), self.graph.fold(Op::Or, vec![below, above]))
            }
            Op::Not => {
                let (t, f) = self.may_answers(args[0]);
                (f, t)
            }
            Op::And | Op::Or => {
                let parts: Vec<(NodeId, NodeId)> = args.iter().map(|a| self.may_answers(*a)).collect();
                let (join_t, join_f) = if op == Op::And { (Op::And, Op::Or) } else { (Op::Or, Op::And) };
                let t = self.graph.fold(join_t, parts.iter().map(|p| p.0).collect());
                let f = self.graph.fold(join_f, parts.iter().map(|p| p.1).collect());
                (t, f)
            }
            _ => (yes, yes),
        }
    }

    /// Does any of `ns` read an unknown number or boolean atom?
    fn reads_unknown(&self, ns: &[NodeId]) -> bool {
        crate::trace::verify::cone(&self.graph, ns).into_iter().any(|n| matches!(self.graph.get(n).op, Op::UnknownNum | Op::UnknownBool(_)))
    }

    /// `a - r` where `a` is `r + c`: `c` EXACTLY (true in wrapping 16.16),
    /// also under a select's arms (a platform's `x - last`). Here, not in
    /// `Graph::fold`: over ranges `(r + c) - r` evaluates wider, and `fold`
    /// must agree with the evaluator. `None` where nothing cancels.
    fn cancel_sub(&mut self, a: NodeId, r: NodeId) -> Result<Option<NodeId>> {
        if a == r {
            return Ok(Some(self.konst(P8::from_raw(0))));
        }
        let op = self.graph.get(a).op.clone();
        let args = self.graph.get(a).args.clone();
        match op {
            Op::Add if args[0] == r => Ok(Some(args[1])),
            Op::Add if args[1] == r => Ok(Some(args[0])),
            Op::Sel => {
                let (c, t, f) = (args[0], args[1], args[2]);
                let (tc, fc) = (self.cancel_sub(t, r)?, self.cancel_sub(f, r)?);
                if tc.is_none() && fc.is_none() {
                    return Ok(None);
                }
                let t2 = match tc {
                    Some(n) => n,
                    None => self.arith(Arith::Sub, &t, &r)?,
                };
                let f2 = match fc {
                    Some(n) => n,
                    None => self.arith(Arith::Sub, &f, &r)?,
                };
                Ok(Some(self.graph.fold(Op::Sel, vec![c, t2, f2])))
            }
            _ => Ok(None),
        }
    }

    /// A boolean every lane holds in BOTH values (buttons, held trails,
    /// escaped atoms): an unmemoized `Ints` fork of `[0, 1]` with no validity,
    /// so `SplitOk(2)` folds true and the fragments to `0` and `1`.
    pub fn both_values(&mut self, origin: &str) -> NodeId {
        let choices = self.graph.leaf(Op::Const(0, 1 << 16));
        let f = self.fork(&choices, Partition::Ints { ways: 2, memo: false }, origin);
        let zero = self.graph.leaf(Op::Const(0, 0));
        self.graph.fold(Op::Gt, vec![f.value, zero])
    }

    /// Forget this frame's fork choices (beside `forks = 0` and
    /// `Graph::reset_forks`).
    pub fn clear_fork_memo(&mut self) {
        self.fork_memo.clear();
    }

    /// THE fork: partition `v`'s set, mint a choice dimension, and hand back
    /// this configuration's piece and its lanes. Every fork comes through here.
    /// Partition op families must stay distinct: `specialize` resolves
    /// `Split` to `Frag` and `SplitInt` to `IntFrag`.
    fn fork(&mut self, v: &NodeId, p: Partition, origin: &str) -> Fork {
        let kind = p.kind();
        // Already forked this value this frame (hash-consed)? Reuse the choice.
        if p.memoized() {
            if let Some(hit) = self.fork_memo.get(&(*v, kind)) {
                let (d, out) = *hit;
                // RAISED, not set: each site's `SplitOk` bounds its own lanes.
                self.graph.set_fork_ways(d, p.ways());
                return Fork { value: out.0, valid: out.1, id: d };
            }
        }
        let d = self.forks;
        self.forks += 1;
        self.fork_origins.push((d, origin.to_string()));
        let (value, valid) = match &p {
            Partition::Floors { ways } => {
                self.graph.set_fork_ways(d, *ways);
                (self.graph.fold(Op::Split(d), vec![*v]), self.graph.fold(Op::SplitValid(d), vec![*v]))
            }
            Partition::Ints { ways, .. } => {
                self.graph.set_fork_ways(d, *ways);
                (self.graph.fold(Op::SplitInt(d), vec![*v]), self.graph.fold(Op::SplitValid(d), vec![*v]))
            }
        };
        if p.memoized() {
            self.fork_memo.insert((*v, kind), (d, (value, valid)));
        }
        Fork { value, valid, id: d }
    }

    /// THE unknown boolean an output widening writes: one hash-consed node,
    /// stored as `AV::UBool`. Nothing in the frame reads an output, so sharing
    /// correlates nothing; inputs and comparisons take fresh atoms.
    pub fn unknown_bool_output(&mut self) -> NodeId {
        self.graph.leaf(Op::UnknownBool(u32::MAX))
    }

    /// Is `b` the shared output unknown (`unknown_bool_output`)?
    pub fn is_unknown_output(&self, b: NodeId) -> bool {
        matches!(self.graph.get(b).op, Op::UnknownBool(u32::MAX))
    }

    /// `root` with `subst`'s nodes replaced and everything above re-folded.
    pub fn substitute(&mut self, root: NodeId, subst: &rustc_hash::FxHashMap<NodeId, NodeId>) -> NodeId {
        let mut done: rustc_hash::FxHashMap<NodeId, NodeId> = subst.clone();
        let mut stack: Vec<(NodeId, bool)> = vec![(root, false)];
        while let Some((n, expanded)) = stack.pop() {
            if done.contains_key(&n) {
                continue;
            }
            let (op, args) = {
                let node = self.graph.get(n);
                (node.op.clone(), node.args.clone())
            };
            if !expanded {
                stack.push((n, true));
                stack.extend(args.iter().filter(|a| !done.contains_key(a)).map(|a| (*a, false)));
                continue;
            }
            let new_args: Vec<NodeId> = args.iter().map(|a| done[a]).collect();
            let out = if new_args == args { n } else { self.graph.fold(op, new_args) };
            done.insert(n, out);
        }
        done[&root]
    }

    pub fn unknown_bool_atom(&mut self) -> NodeId {
        let k = self.unknown_atoms;
        self.unknown_atoms += 1;
        assert!(k < COUNTDOWN_ATOM, "atom ids ran into the countdown tag");
        self.graph.leaf(Op::UnknownBool(k))
    }

    /// A COUNTDOWN's atom: a comparison of the unknown number where it is a
    /// countdown (a near level's floor `delay`, balloon `timer`, spring
    /// timers): every comparison of it without the fly fruit unknown, one
    /// the interpreter hints at (`set_countdown_hint`) with it. It stays THREE-VALUED wherever the set has other unknowns too (the
    /// platforms): decided per lane where it is read and split by the
    /// split pass, as at a near level alone - never a merge refusal
    /// (`reads_unknown_atom`) or an escape fork (`escaped_atom`). Treated
    /// as the platforms' atoms, every fall floor's `delay <= 0` made two
    /// successors that never merged: room (2,1) `r0sxhnp` traced 354 states
    /// at the spawn against 5 at `r0sxhn` (over the 256 limit) since
    /// `b47b118` made the countdowns the unknown number. With the fruit
    /// unknown, room (3,0) `r0sxhfn`'s 12 floors' `delay <= 0` as fruit atoms
    /// kept every floor's two paths apart (2^13 trace states after the spawn
    /// frame's `foreach`).
    fn countdown_atom(&mut self) -> NodeId {
        let k = self.unknown_atoms;
        self.unknown_atoms += 1;
        assert!(k < COUNTDOWN_ATOM - 1, "atom ids overflow into the countdown tag");
        self.graph.leaf(Op::UnknownBool(COUNTDOWN_ATOM | k))
    }

    /// Does no lane's data reach `n` - no input cell or fork?
    pub fn lane_independent(&mut self, n: NodeId) -> bool {
        let mut stack: Vec<(NodeId, bool)> = vec![(n, false)];
        while let Some((x, expanded)) = stack.pop() {
            if self.lane_memo.contains_key(&x) {
                continue;
            }
            let node = self.graph.get(x);
            let dependent = matches!(
                node.op,
                Op::Cell(_) | Op::Split(_) | Op::SplitValid(_) | Op::SplitInt(_)
            );
            if dependent || node.args.is_empty() {
                self.lane_memo.insert(x, !dependent);
                continue;
            }
            let args = node.args.clone();
            if expanded {
                let r = args.iter().all(|a| self.lane_memo[a]);
                self.lane_memo.insert(x, r);
            } else {
                stack.push((x, true));
                stack.extend(args.iter().filter(|a| !self.lane_memo.contains_key(a)).map(|a| (*a, false)));
            }
        }
        self.lane_memo[&n]
    }

    /// `op` over LITERAL operands (one an interval) in `Graph::eval`'s
    /// semantics (one definition); `None` where eval does not model it.
    fn eval_literal(&self, op: &Op, args: &[NodeId]) -> Option<crate::transpile::graph::Val> {
        if !self.unknowns() {
            return None;
        }
        let lits: Vec<(i32, i32)> = args
            .iter()
            .map(|a| match self.graph.get(*a).op {
                Op::Const(lo, hi) => Some((lo, hi)),
                _ => None,
            })
            .collect::<Option<_>>()?;
        if lits.iter().all(|(lo, hi)| lo == hi) {
            return None;
        }
        let mut g = Graph::new();
        let leaves: Vec<NodeId> = lits.iter().map(|(lo, hi)| g.leaf(Op::Const(*lo, *hi))).collect();
        let n = g.add(op.clone(), leaves);
        g.eval(&std::collections::HashMap::new()).ok().map(|v| v[n as usize])
    }

    /// `eval_literal`'s number as a literal node.
    fn literal_num(&mut self, op: Op, args: &[NodeId]) -> Option<NodeId> {
        match self.eval_literal(&op, args)? {
            crate::transpile::graph::Val::Num(iv) => {
                Some(self.graph.leaf(Op::Const(iv.low.as_raw_u32() as i32, iv.high.as_raw_u32() as i32)))
            }
            crate::transpile::graph::Val::Bool(_) => None,
        }
    }

    fn konst(&mut self, v: P8) -> NodeId {
        let raw = v.as_raw_u32() as i32;
        self.graph.leaf(Op::Const(raw, raw))
    }
    /// The constant a node is, if it is one.
    fn as_p8(&self, n: NodeId) -> Option<P8> {
        match self.graph.get(n).op {
            Op::Const(lo, hi) if lo == hi => Some(P8::from_raw(lo)),
            _ => None,
        }
    }
}

impl Domain for Symbolic {
    type Num = NodeId;
    type Bool = NodeId;

    fn num(&mut self, v: P8) -> NodeId {
        self.konst(v)
    }
    fn boolean(&mut self, b: bool) -> NodeId {
        self.graph.leaf(Op::ConstBool(b))
    }
    fn arith(&mut self, op: Arith, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        // Fold known operands, so what the heap depends on stays concrete.
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            return Ok(self.konst(c.arith(op, &x, &y)?));
        }
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(self.unknown_num());
        }
        if let Some((c, t, f)) = self.unknown_select(*a) {
            let (x, y) = (self.arith(op, &t, b)?, self.arith(op, &f, b)?);
            return Ok(self.sel_num(&c, &x, &y));
        }
        if let Some((c, t, f)) = self.unknown_select(*b) {
            let (x, y) = (self.arith(op, a, &t)?, self.arith(op, a, &f)?);
            return Ok(self.sel_num(&c, &x, &y));
        }
        if let Arith::Sub = op {
            if let Some(n) = self.cancel_sub(*a, *b)? {
                return Ok(n);
            }
        }
        let g = match op {
            Arith::Add => Op::Add,
            Arith::Sub => Op::Sub,
            Arith::Mul => Op::Mul,
            Arith::Div => Op::Div,
            Arith::Rem => Op::Rem,
        };
        if let Some(n) = self.literal_num(g.clone(), &[*a, *b]) {
            return Ok(n);
        }
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn fun1(&mut self, f: Fun1, a: &NodeId) -> Result<NodeId> {
        if let Some(x) = self.as_p8(*a) {
            let mut c = Concrete;
            return Ok(self.konst(c.fun1(f, &x)?));
        }
        // `sin` of anything is in [-1, 1]; other builtins of unknown: unknown.
        if self.is_unknown_num(*a) {
            return Ok(match f {
                Fun1::Sin => self.graph.leaf(Op::Const(-0x1_0000, 0x1_0000)),
                _ => self.unknown_num(),
            });
        }
        if let Some((c, t, e)) = self.unknown_select(*a) {
            let (x, y) = (self.fun1(f, &t)?, self.fun1(f, &e)?);
            return Ok(self.sel_num(&c, &x, &y));
        }
        // `sin` of an interval is [-1, 1]: the emitter cannot lower `Sin`
        // over a ZI.
        if matches!(f, Fun1::Sin) && self.is_interval(a) {
            let lo = self.konst(P8::from_i16(-1));
            let hi = self.konst(P8::from_i16(1));
            return Ok(self.graph.fold(Op::Span, vec![lo, hi]));
        }
        let g = match f {
            Fun1::Neg => Op::Neg,
            Fun1::Abs => Op::Abs,
            Fun1::Flr => Op::Flr,
            Fun1::Sin => Op::Sin,
        };
        if let Some(n) = self.literal_num(g.clone(), &[*a]) {
            return Ok(n);
        }
        Ok(self.graph.fold(g, vec![*a]))
    }
    fn fun2(&mut self, f: Fun2, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            return Ok(self.konst(c.fun2(f, &x, &y)?));
        }
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(self.unknown_num());
        }
        if let Some((c, t, e)) = self.unknown_select(*a) {
            let (x, y) = (self.fun2(f, &t, b)?, self.fun2(f, &e, b)?);
            return Ok(self.sel_num(&c, &x, &y));
        }
        if let Some((c, t, e)) = self.unknown_select(*b) {
            let (x, y) = (self.fun2(f, a, &t)?, self.fun2(f, a, &e)?);
            return Ok(self.sel_num(&c, &x, &y));
        }
        let g = match f {
            Fun2::Min => Op::Min,
            Fun2::Max => Op::Max,
        };
        if let Some(n) = self.literal_num(g.clone(), &[*a, *b]) {
            return Ok(n);
        }
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn compare(&mut self, op: Cmp, a: &NodeId, b: &NodeId) -> Result<NodeId> {
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            let r = c.compare(op, &x, &y)?;
            return Ok(self.graph.leaf(Op::ConstBool(r)));
        }
        // With an unknown number nothing is decided.
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(if self.countdown_hint || !self.fruit_unknown { self.countdown_atom() } else { self.unknown_bool_atom() });
        }
        if let Some((c, t, f)) = self.unknown_select(*a) {
            let (x, y) = (self.compare(op, &t, b)?, self.compare(op, &f, b)?);
            return Ok(self.sel_bool(&c, &x, &y));
        }
        if let Some((c, t, f)) = self.unknown_select(*b) {
            let (x, y) = (self.compare(op, a, &t)?, self.compare(op, a, &f)?);
            return Ok(self.sel_bool(&c, &x, &y));
        }
        // Two literals: decided by their intervals, or a fresh atom of their own.
        let lit = match op {
            Cmp::Lt => Op::Lt,
            Cmp::Le => Op::Le,
            Cmp::Gt => Op::Gt,
            Cmp::Ge => Op::Ge,
            Cmp::Eq => Op::Eq,
        };
        match self.eval_literal(&lit, &[*a, *b]) {
            Some(crate::transpile::graph::Val::Bool(Some(r))) => return Ok(self.graph.leaf(Op::ConstBool(r))),
            Some(crate::transpile::graph::Val::Bool(None)) => return Ok(self.unknown_bool_atom()),
            _ => {}
        }
        // Decided by static ranges iff every pair of pieces agrees.
        if !self.ranges.is_empty() {
            if let (Some(xs), Some(ys)) = (self.range_of(*a), self.range_of(*b)) {
                let one = |x: (i64, i64), y: (i64, i64)| -> Option<bool> {
                    match op {
                        Cmp::Lt => if x.1 < y.0 { Some(true) } else if x.0 >= y.1 { Some(false) } else { None },
                        Cmp::Le => if x.1 <= y.0 { Some(true) } else if x.0 > y.1 { Some(false) } else { None },
                        Cmp::Gt => if x.0 > y.1 { Some(true) } else if x.1 <= y.0 { Some(false) } else { None },
                        Cmp::Ge => if x.0 >= y.1 { Some(true) } else if x.1 < y.0 { Some(false) } else { None },
                        Cmp::Eq => if x.0 == x.1 && y.0 == y.1 && x.0 == y.0 { Some(true) } else if x.1 < y.0 || y.1 < x.0 { Some(false) } else { None },
                    }
                };
                let mut decided: Option<Option<bool>> = None;
                'pairs: for x in &xs {
                    for y in &ys {
                        let r = one(*x, *y);
                        match (decided, r) {
                            (None, r) => decided = Some(r),
                            (Some(p), r) if p == r => {}
                            _ => {
                                decided = Some(None);
                                break 'pairs;
                            }
                        }
                    }
                }
                if let Some(Some(r)) = decided {
                    self.range_folds += 1;
                    return Ok(self.graph.leaf(Op::ConstBool(r)));
                }
            }
        }
        let g = match op {
            Cmp::Lt => Op::Lt,
            Cmp::Le => Op::Le,
            Cmp::Gt => Op::Gt,
            Cmp::Ge => Op::Ge,
            Cmp::Eq => Op::Eq,
        };
        Ok(self.graph.fold(g, vec![*a, *b]))
    }
    fn not(&mut self, a: &NodeId) -> NodeId {
        self.graph.fold(Op::Not, vec![*a])
    }
    fn and(&mut self, a: &NodeId, b: &NodeId) -> NodeId {
        self.graph.fold(Op::And, vec![*a, *b])
    }
    fn or(&mut self, a: &NodeId, b: &NodeId) -> NodeId {
        self.graph.fold(Op::Or, vec![*a, *b])
    }
    fn decide(&self, c: &NodeId) -> Option<bool> {
        match self.graph.get(*c).op {
            Op::ConstBool(b) => Some(b),
            _ => None,
        }
    }
    fn sel_num(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> NodeId {
        self.graph.fold(Op::Sel, vec![*c, *t, *f])
    }
    fn sel_bool(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> NodeId {
        self.graph.fold(Op::Sel, vec![*c, *t, *f])
    }
    fn mget(&mut self, x: &NodeId, y: &NodeId) -> Result<NodeId> {
        Ok(self.graph.fold(Op::Mget, vec![*x, *y]))
    }
    fn tile_flag_at(
        &mut self,
        x: &NodeId,
        y: &NodeId,
        w: &NodeId,
        h: &NodeId,
        flag: &NodeId,
    ) -> Result<NodeId> {
        Ok(self.graph.fold(Op::TileFlagAt, vec![*x, *y, *w, *h, *flag]))
    }
    fn range_num(&mut self, lo: P8, hi: P8) -> Result<NodeId> {
        Ok(self.graph.leaf(Op::Const(lo.as_raw_u32() as i32, hi.as_raw_u32() as i32)))
    }
    /// A button: a fork both of whose configurations every lane takes.
    fn unknown_bool(&mut self) -> Result<NodeId> {
        Ok(self.both_values(UNKNOWN_BOOL_ORIGIN))
    }
    fn node_count(&self) -> usize {
        self.graph.len()
    }
    /// Is this value an interval? A type rule, not a cone query, and the same
    /// rule as `lower`'s `Repr::wide` (a select is one when an ARM is; `flr`
    /// and cart lookups never are), or lowering fails ("wants a ZN but the
    /// graph computes a ZI").
    fn is_interval(&self, v: &NodeId) -> bool {
        fn go(g: &Graph, ival: &std::collections::BTreeSet<u32>, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(b) = memo.get(&n) {
                return *b;
            }
            let node = g.get(n);
            let a = &node.args;
            let any = |memo: &mut rustc_hash::FxHashMap<NodeId, bool>, xs: &[NodeId]| {
                xs.iter().any(|x| go(g, ival, memo, *x))
            };
            let r = match node.op {
                Op::Cell(c) => ival.contains(&c),
                Op::Const(lo, hi) => lo != hi,
                Op::Split(_) => true,
                // An exact whole number per configuration.
                Op::SplitInt(_) | Op::Lo | Op::Hi => false,
                // A span's bounds differ (`fold` collapses a literal span).
                Op::Span => true,
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs
                | Op::Min | Op::Max => any(memo, a),
                // The CONDITION does not make the result an interval.
                Op::Sel => any(memo, &a[1..]),
                // Exact where the lane survives (else its own error).
                Op::Flr => false,
                // `fun1` replaces `sin` of an interval with its range.
                Op::Sin => any(memo, a),
                _ => false,
            };
            memo.insert(n, r);
            r
        }
        // No early-out on empty `ival_cells`: widenings write literal intervals.
        go(&self.graph, &self.ival_cells, &mut self.ival_memo.borrow_mut(), *v)
    }

    fn fork_flr(&mut self, v: &NodeId, ways: u8) -> (NodeId, NodeId) {
        let f = self.fork(v, Partition::Floors { ways }, "a floor fork (`move`)");
        (f.value, f.valid)
    }

    fn flr_ways(&mut self, v: &NodeId) -> u8 {
        // Only seeded ranges know a static range; otherwise the fixed arity.
        if self.ranges.is_empty() {
            return MOVE_WAYS;
        }
        // At most `MOVE_WAYS`: the static range covers ALL lanes, but one
        // lane spans at most that many floors (too small declines via
        // `SplitOk`). Level -1 (`uncapped_ways`) takes the HULL: its lanes
        // are ranges.
        match self.range_of(*v) {
            Some(ps) => {
                let sh = 16;
                let w = if self.uncapped_ways {
                    let lo = ps.iter().map(|p| p.0 >> sh).min().unwrap_or(0);
                    let hi = ps.iter().map(|p| p.1 >> sh).max().unwrap_or(0);
                    (hi - lo + 1).min(i64::from(u8::MAX))
                } else {
                    ps.iter().map(|p| (p.1 >> sh) - (p.0 >> sh) + 1).max().unwrap_or(1).min(MOVE_WAYS as i64)
                };
                w.max(1) as u8
            }
            None => MOVE_WAYS,
        }
    }

    fn is_select_num(&self, v: &NodeId) -> bool {
        matches!(self.graph.get(*v).op, Op::Sel)
    }

    fn is_select_bool(&self, b: &NodeId) -> bool {
        matches!(self.graph.get(*b).op, Op::Sel)
    }

    fn evaluated_at(&mut self, n: &NodeId, at: &NodeId) {
        let at = match self.evaluated.get(n) {
            Some(before) => self.graph.fold(Op::Or, vec![*before, *at]),
            None => *at,
        };
        self.evaluated.insert(*n, at);
    }

    fn join_num_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        // A condition that reads no atom but a countdown's (`countdown_atom`)
        // is a select, as at a near level alone.
        if !self.reads_unknown_atom(c) || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        if self.is_unknown_num(*t) || self.is_unknown_num(*f) {
            return Some(self.unknown_num());
        }
        match (self.graph.get(*t).op.clone(), self.graph.get(*f).op.clone()) {
            (Op::Const(a0, a1), Op::Const(b0, b1)) => Some(self.graph.leaf(Op::Const(a0.min(b0), a1.max(b1)))),
            // One value plus a literal on each side: that value plus the
            // literals' hull, so `x - last` still cancels.
            _ => {
                let (tb, tk) = self.base_plus_literal(*t);
                let (fb, fk) = self.base_plus_literal(*f);
                if tb != fb || matches!(self.graph.get(tb).op, Op::Const(..)) {
                    return None;
                }
                let k = self.graph.leaf(Op::Const(tk.0.min(fk.0), tk.1.max(fk.1)));
                Some(self.graph.fold(Op::Add, vec![tb, k]))
            }
        }
    }

    fn reads_unknown_atom(&mut self, c: &NodeId) -> bool {
        if !self.unknowns() || self.decide(c).is_some() {
            return false;
        }
        // Post-order and memoized: a frame's conditions share their cones.
        let mut stack: Vec<(NodeId, bool)> = vec![(*c, false)];
        while let Some((x, expanded)) = stack.pop() {
            if self.atom_memo.contains_key(&x) {
                continue;
            }
            let node = self.graph.get(x);
            if matches!(node.op, Op::UnknownBool(_)) || node.args.is_empty() {
                let atom = matches!(node.op, Op::UnknownBool(k) if !is_countdown_atom(k));
                self.atom_memo.insert(x, atom);
                continue;
            }
            let args = node.args.clone();
            if expanded {
                let r = args.iter().any(|a| self.atom_memo[a]);
                self.atom_memo.insert(x, r);
            } else {
                stack.push((x, true));
                stack.extend(args.iter().filter(|a| !self.atom_memo.contains_key(a)).map(|a| (*a, false)));
            }
        }
        self.atom_memo[c]
    }

    fn set_countdown_hint(&mut self, on: bool) {
        self.countdown_hint = on;
    }

    fn join_bool_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        // A condition that reads no atom but a countdown's (`countdown_atom`)
        // is a select, as at a near level alone.
        if !self.reads_unknown_atom(c) || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        // Arms every lane holds alike: a fresh atom (escapes as a fork).
        if self.lane_independent(*t) && self.lane_independent(*f) {
            return Some(self.unknown_bool_atom());
        }
        // An arm reading lane data joins EXACTLY in three-valued logic; a
        // fresh atom would throw the lane's data away.
        let ct = self.and(c, t);
        let nc = self.not(c);
        let cf = self.and(&nc, f);
        Some(self.or(&ct, &cf))
    }

    fn atoms_minted(&self) -> u32 {
        self.unknown_atoms
    }

    fn describe_num(&self, n: &NodeId) -> String {
        match self.graph.get(*n).op {
            Op::Const(lo, hi) if lo == hi => format!("{}", lo as f64 / 65536.0),
            Op::Const(lo, hi) => format!("{}..{}", lo as f64 / 65536.0, hi as f64 / 65536.0),
            _ => "?".to_string(),
        }
    }

    fn escaped_atom(&mut self, b: &NodeId, since: u32, origin: &dyn Fn(&Self) -> String) -> Option<NodeId> {
        // Only with unknowns, and never a countdown's atom (`countdown_atom`):
        // those stay three-valued (decided where read); forking adds a
        // dimension even if unread.
        if !self.unknowns() {
            return None;
        }
        if let Some(f) = self.escaped.get(b) {
            return Some(*f);
        }
        // The atoms of this call that `b` reads. Only those make a fork; every
        // countdown's `b` reads is enumerated with them all the same, so the
        // restriction below reads no atom (one left in it is unknown on every
        // lane: the fork's coverage would decline them all).
        let atoms: Vec<NodeId> = crate::trace::verify::cone(&self.graph, &[*b])
            .into_iter()
            .filter(|n| matches!(self.graph.get(*n).op, Op::UnknownBool(k) if is_countdown_atom(k) || (k >= since && k != u32::MAX)))
            .collect();
        if !atoms.iter().any(|n| matches!(self.graph.get(*n).op, Op::UnknownBool(k) if !is_countdown_atom(k))) {
            return None;
        }
        let name = origin(self);
        let g = self.both_values(&name);
        // A bare atom is both values everywhere. An atom joined with lane data
        // is the fork RESTRICTED per lane to `b`'s possible values, so every
        // later read sees one value per configuration.
        let out = if atoms.len() == 1 && atoms[0] == *b {
            g
        } else if atoms.len() > ESCAPE_ATOMS {
            g
        } else {
            let (mut may_t, mut may_f) = (self.graph.leaf(Op::ConstBool(false)), self.graph.leaf(Op::ConstBool(false)));
            for bits in 0u32..(1 << atoms.len()) {
                let subst: rustc_hash::FxHashMap<NodeId, NodeId> =
                    atoms.iter().enumerate().map(|(i, a)| (*a, self.graph.leaf(Op::ConstBool(bits >> i & 1 == 1)))).collect();
                let v = self.substitute(*b, &subst);
                let nv = self.graph.fold(Op::Not, vec![v]);
                may_t = self.graph.fold(Op::Or, vec![may_t, v]);
                may_f = self.graph.fold(Op::Or, vec![may_f, nv]);
            }
            let not_f = self.graph.fold(Op::Not, vec![may_f]);
            let only_t = self.graph.fold(Op::And, vec![may_t, not_f]);
            let both = self.graph.fold(Op::And, vec![may_t, may_f]);
            let either = self.graph.fold(Op::And, vec![both, g]);
            self.graph.fold(Op::Or, vec![only_t, either])
        };
        self.escaped.insert(*b, out);
        Some(out)
    }

    fn literal_fragments(&mut self, v: &NodeId) -> Result<Option<Vec<NodeId>>> {
        if !self.unknowns() {
            return Ok(None);
        }
        let Op::Const(lo, hi) = self.graph.get(*v).op else { return Ok(None) };
        if lo == hi {
            return Ok(None);
        }
        // On the INTEGERS: a fragment need only make `flr` exact, and they
        // rejoin into one hull (`move` reads it only via its floor and a
        // linear `rem - 0.5 - amount`).
        let step = 1i64 << 16;
        let (lo, hi) = (lo as i64, hi as i64);
        let (fl, fh) = (lo.div_euclid(step) * step, hi.div_euclid(step) * step);
        // Too many is refused at trace time (else a body erroring on every lane).
        anyhow::ensure!(
            (fh - fl) / step < crate::transpile::graph::MAX_WAYS as i64,
            "__split_by_flr of the literal [{}, {}] spans {} integers, more than {} fragments",
            lo as f64 / 65536.0,
            hi as f64 / 65536.0,
            (fh - fl) / step + 1,
            crate::transpile::graph::MAX_WAYS
        );
        let mut out = Vec::new();
        let mut base = fl;
        while base <= fh {
            let (a, b) = (lo.max(base), hi.min(base + step - 1));
            out.push(self.graph.leaf(Op::Const(a as i32, b as i32)));
            base += step;
        }
        Ok(Some(out))
    }

    fn undecided_atom(&mut self) -> Result<NodeId> {
        Ok(self.unknown_bool_atom())
    }

    fn as_const(&self, v: &NodeId) -> Option<P8> {
        self.as_p8(*v)
    }

    fn describe(&self, v: &NodeId) -> String {
        // One level of operands too: whether a select's arms are constants.
        let n = self.graph.get(*v);
        let args: Vec<String> = n
            .args
            .iter()
            .map(|a| format!("{:?}", self.graph.get(*a).op))
            .collect();
        format!("node {} = {:?}({})", v, n.op, args.join(", "))
    }
}

/// The `move` fork's arity: exact speed plus a rem within one grid cell
/// spans at most two floors.
pub const MOVE_WAYS: u8 = 2;

/// A refusal instead of a guess: where the heap would have become symbolic.
pub fn refuse_unknown(what: &str) -> anyhow::Error {
    anyhow::anyhow!(
        "{} is not known at trace time - the heap cannot depend on a symbolic value",
        what
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(v: i16) -> P8 {
        P8::from_i16(v)
    }

    /// The fake wall's `spd.x = -sign(spd.x)*1.5` before `move`: three pieces
    /// two floors wide each; a kernel lane holds one (2-way), level -1 the
    /// HULL (five floors).
    #[test]
    fn level_minus_one_sizes_a_move_fork_by_the_hull_of_its_pieces() {
        let mut d = Symbolic::default();
        let rem = d.graph.leaf(Op::Cell(0));
        let spd = d.graph.leaf(Op::Cell(1));
        d.ranges.insert(rem, (-0x8000, 0x7fff));
        d.ranges.insert(spd, (-5 << 16, 5 << 16));
        let zero = d.graph.leaf(Op::Const(0, 0));
        let k = |d: &mut Symbolic, v: i32| d.graph.leaf(Op::Const(v, v));
        let (neg, pos, half) = (k(&mut d, -0x18000), k(&mut d, 0x18000), k(&mut d, 0x8000));
        let gt = d.graph.fold(Op::Gt, vec![spd, zero]);
        let lt = d.graph.fold(Op::Lt, vec![spd, zero]);
        let inner = d.graph.fold(Op::Sel, vec![lt, pos, zero]);
        let wall = d.graph.fold(Op::Sel, vec![gt, neg, inner]);
        let moved = d.graph.fold(Op::Add, vec![rem, wall]);
        let operand = d.graph.fold(Op::Add, vec![half, moved]);
        assert_eq!(d.flr_ways(&operand), 2, "a kernel lane takes one piece");
        d.uncapped_ways = true;
        d.range_memo.clear();
        assert_eq!(d.flr_ways(&operand), 5, "level -1 joins the pieces: floors -2..=2");
    }

    /// The FORK TRIGGER's contract on hand-built graphs.
    #[test]
    fn the_fork_trigger_excludes_what_a_lane_decides_for_itself() {
        let mut d = Symbolic::default();
        let ival = d.graph.leaf(Op::Cell(0));
        let exact = d.graph.leaf(Op::Cell(1));
        d.ival_cells.insert(0);
        d.forget_intervals();
        let zero = d.graph.leaf(Op::Const(0, 0));

        // A comparison on an interval operand IS the source of forking.
        let cmp = d.graph.fold(Op::Gt, vec![ival, zero]);
        assert!(d.lane_undecidable(cmp), "a comparison on an interval must fork");
        // On an exact operand it is not.
        let exact_cmp = d.graph.fold(Op::Gt, vec![exact, zero]);
        assert!(!d.lane_undecidable(exact_cmp));

        // `Flr` is lane-decidable BY ASSERTION, not a fork trigger.
        let flr = d.graph.fold(Op::Flr, vec![ival]);
        let flr_cmp = d.graph.fold(Op::Gt, vec![flr, zero]);
        assert!(!d.lane_undecidable(flr_cmp), "Flr is decided per lane by its assertion");

        // `TileFlagAt` is lane-decidable BY INSTRUCTION.
        let w = d.graph.leaf(Op::Const(8 << 16, 8 << 16));
        let tile = d.graph.fold(Op::TileFlagAt, vec![ival, ival, w, w, zero]);
        assert!(!d.lane_undecidable(tile), "TileFlagAt is decided per lane");
        // And a boolean tree over it stays decidable.
        let not_tile = d.graph.fold(Op::Not, vec![tile]);
        assert!(!d.lane_undecidable(not_tile));

        // THE `Sel` ASYMMETRY: a NUMERIC select ignores the condition.
        let five = d.graph.leaf(Op::Const(5 << 16, 5 << 16));
        let seven = d.graph.leaf(Op::Const(7 << 16, 7 << 16));
        let num_sel = d.graph.fold(Op::Sel, vec![cmp, five, seven]);
        let sel_cmp = d.graph.fold(Op::Gt, vec![num_sel, zero]);
        assert!(
            !d.lane_undecidable(sel_cmp),
            "a select's condition does not make its numeric result span values"
        );
        // A BOOLEAN select straddles exactly when its condition does.
        let t = d.graph.leaf(Op::ConstBool(true));
        let f = d.graph.leaf(Op::ConstBool(false));
        let bool_sel = d.graph.fold(Op::Sel, vec![cmp, t, f]);
        assert!(d.lane_undecidable(bool_sel), "a boolean select straddles with its condition");
    }

    /// A countdown set on some lanes and unknown on others: `delay - 1` and
    /// `delay - 1 <= 0` distribute over the select, so set lanes stay exact.
    #[test]
    fn an_operation_on_a_select_of_the_unknown_number_is_distributed() {
        let mut d = Symbolic::default();
        let c = d.graph.leaf(Op::Cell(0));
        let (sixty, one, zero) = (d.num(p(60)), d.num(p(1)), d.num(p(0)));
        let u = d.unknown_num();
        let delay = d.sel_num(&c, &sixty, &u);
        let less = d.arith(Arith::Sub, &delay, &one).unwrap();
        let fifty_nine = d.num(p(59));
        assert_eq!(less, d.sel_num(&c, &fifty_nine, &u));
        let done = d.compare(Cmp::Le, &less, &zero).unwrap();
        let node = d.graph.get(done).clone();
        assert!(crate::trace::verify::cone(&d.graph, &[done]).iter().all(|n| d.graph.get(*n).op != Op::UnknownNum), "{node:?}");
        assert!(crate::trace::verify::cone(&d.graph, &[done]).iter().any(|n| matches!(d.graph.get(*n).op, Op::UnknownBool(_))), "the unknown lanes: an atom");
    }

    /// `abstractness` and `lane_undecidable` answer different questions.
    #[test]
    fn abstractness_is_not_the_fork_trigger() {
        let mut d = Symbolic::default();
        let ival = d.graph.leaf(Op::Cell(0));
        d.ival_cells.insert(0);
        d.forget_intervals();
        let flr = d.graph.fold(Op::Flr, vec![ival]);
        assert!(d.abstractness(flr), "Flr of an interval denotes a set (partial: singleton on pain of error)");
        let zero = d.graph.leaf(Op::Const(0, 0));
        let cmp = d.graph.fold(Op::Gt, vec![flr, zero]);
        assert!(d.abstractness(cmp), "so a comparison on it is abstract too");
        assert!(!d.lane_undecidable(cmp), "yet a lane decides it, so it must not fork");
    }

    /// `x == k` on an interval `x` may be true where `k` is in `x` and false
    /// where `x` is not exactly `k` (`[0, 2]`, `[1, 1]`, `[0, 0]` vs `k = 1`),
    /// also with the fly fruit unknown elsewhere in the frame (a hidden near
    /// floor's exact `state == 2` was "both ways" there: room (3,0) `r0sxhfn`).
    #[test]
    fn an_equality_on_an_interval_may_answer_as_its_ends_allow() {
        use crate::transpile::graph::Val;
        let one = 1 << 16;
        for fruit_unknown in [false, true] {
            let answers = |lo: i32, hi: i32| {
                let mut d = Symbolic { fruit_unknown, ..Default::default() };
                let x = d.graph.leaf(Op::Const(lo, hi));
                let k = d.graph.leaf(Op::Const(one, one));
                let eq = d.graph.fold(Op::Eq, vec![x, k]);
                let (t, f) = d.may_answers(eq);
                let v = d.graph.eval(&Default::default()).unwrap();
                let b = |n: NodeId| match v[n as usize] {
                    Val::Bool(b) => b,
                    Val::Num(_) => panic!("a number"),
                };
                (b(t), b(f))
            };
            assert_eq!(answers(0, 2 * one), (Some(true), Some(true)), "[0, 2] both ways");
            assert_eq!(answers(one, one), (Some(true), Some(false)), "[1, 1] only equal");
            assert_eq!(answers(0, 0), (Some(false), Some(true)), "[0, 0] only unequal");
        }
        // An operand reading an unknown: no ends to read, both ways.
        let mut d = Symbolic { fruit_unknown: true, ..Default::default() };
        let u = d.unknown_bool_atom();
        let (a, b) = (d.graph.leaf(Op::Const(0, 2 * one)), d.graph.leaf(Op::Const(one, one)));
        let x = d.graph.fold(Op::Sel, vec![u, a, b]);
        let k = d.graph.leaf(Op::Const(one, one));
        let eq = d.graph.fold(Op::Eq, vec![x, k]);
        let yes = d.graph.leaf(Op::ConstBool(true));
        assert_eq!(d.may_answers(eq), (yes, yes));
    }
}
