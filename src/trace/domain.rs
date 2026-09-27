//! The value domain the tracer is generic over.
//!
//! One interpreter, two instantiations:
//!
//! * `Concrete` - `Num = Pico8Num`, `Bool = bool`. This is the ORACLE.
//!   It also builds the initial heap by running the cart's top level.
//! * `Symbolic` - `Num = Bool = NodeId` into a `transpile::graph::Graph`.
//!   This is the TRACER; what it leaves behind is the graph.
//!
//! They are the same code because that is the only way they cannot drift.
//! An oracle that is a separate implementation is a second definition of
//! what the program means, and then two things have to be kept true at
//! once (`plans/tracing.md`).
//!
//! ## The one method that matters
//!
//! `decide` asks "can you tell me this condition's value RIGHT NOW".
//! `Concrete` always can. `Symbolic` can only when the node folded to a
//! constant. `None` is exactly the case where the interpreter stops being
//! an interpreter and starts being a compiler: it traces both arms and
//! merges them with `sel_*`.
//!
//! So the concrete instantiation never calls `sel_*` at all, and the
//! symbolic one calls it only where the program genuinely branches on
//! something it cannot know - which, because the HEAP stays concrete, is
//! far less often than it sounds. `count(objects)` is a number, `#t` is a
//! number, a table field's presence is a fact; only game data is symbolic.

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

/// Which op family carries a fork, and so the memo's second key: `Floors` and
/// `Ints` over one value resolve to different fragments and may not share a
/// choice dimension.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ForkKind {
    Floors,
    Ints,
    Table,
}

/// HOW a fork partitions its value's set - the one thing that differs between
/// the forks a trace takes (plans/graph-model.md section 3). A configuration
/// index becomes a value by this rule.
pub enum Partition {
    /// Cut an interval at the fork grid's cell edges: fragment `c` is the
    /// `c`-th cell (`Op::Split` -> `Frag`), which is what `flr` of an
    /// interval needs, since `flr` is not a function on a set whose points
    /// have different floors.
    Floors { ways: u8 },
    /// Cut an interval of WHOLE numbers into the numbers themselves, each
    /// EXACT (`Op::SplitInt` -> `IntFrag`): a position under a pixel bucket.
    ///
    /// `memo` is false where two forks over the SAME node must stay
    /// independent: the held trails `p_jump` / `p_dash` both fork the constant
    /// `[0, 1<<16]`, and sharing one choice would tie them together, so the
    /// pair could never take opposite values.
    Ints { ways: u8, memo: bool },
    /// Cut at a TABLE of ranges, RELATIVE to the entry the low end lies in:
    /// fragment `c` is the value clipped to the `c`-th entry from there
    /// (`Op::SplitTab` -> a select chain). Never memoised: the ranges are part
    /// of the partition, so two sites forking one value at different tables
    /// are different forks, and `set_fork_table` would overwrite the first.
    Table { ranges: Vec<(i32, i32)>, arity: u8 },
}

impl Partition {
    fn kind(&self) -> ForkKind {
        match self {
            Partition::Floors { .. } => ForkKind::Floors,
            Partition::Ints { .. } => ForkKind::Ints,
            Partition::Table { .. } => ForkKind::Table,
        }
    }

    /// The fork's arity: how many fragments a configuration chooses between.
    fn ways(&self) -> u8 {
        match self {
            Partition::Floors { ways } | Partition::Ints { ways, .. } => *ways,
            Partition::Table { arity, .. } => *arity,
        }
    }

    fn memoized(&self) -> bool {
        match self {
            Partition::Floors { .. } => true,
            Partition::Ints { memo, .. } => *memo,
            Partition::Table { .. } => false,
        }
    }
}

/// What a fork hands back: the fragment this configuration takes, which lanes
/// fall in it, and the choice dimension it minted (which the caller needs to
/// name the fork's other ops - a table fork's key and coverage).
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
    /// De Morgan by default, which is all `Concrete` needs. `Symbolic`
    /// overrides it to build `Op::Or` directly: in a GRAPH the difference
    /// is three nodes against one, and the detour also destroys the
    /// symmetry, so `a or b` and `b or a` fail to intern together.
    fn or(&mut self, a: &Self::Bool, b: &Self::Bool) -> Self::Bool {
        let (na, nb) = (self.not(a), self.not(b));
        let both = self.and(&na, &nb);
        self.not(&both)
    }

    /// The value of this condition, if the domain knows it. `None` sends
    /// the interpreter down both arms.
    fn decide(&self, c: &Self::Bool) -> Option<bool>;

    /// Merge two values under an undecided condition. `Concrete` never
    /// reaches these, because `decide` never returns `None` for it.
    fn sel_num(&mut self, c: &Self::Bool, t: &Self::Num, f: &Self::Num) -> Self::Num;
    fn sel_bool(&mut self, c: &Self::Bool, t: &Self::Bool, f: &Self::Bool) -> Self::Bool;

    /// `mget(x, y)` with coordinates not known at trace time. The map is
    /// data and it is concrete, so this only arises inside a tile scan
    /// whose bounds came out symbolic - and there the emitted kernel does
    /// it in one instruction (`zn_mget`) rather than a lookup table.
    fn mget(&mut self, x: &Self::Num, y: &Self::Num) -> Result<Self::Num>;

    /// `tile_flag_at(x, y, w, h, flag)` where the coordinates are NOT
    /// known at trace time.
    ///
    /// This has to be a primitive rather than traced into, for the same
    /// reason the IR pipeline makes it one (`zn_tile_flag_at`): the Lua
    /// version scans a tile range with a loop whose bounds are derived
    /// from x and y, so tracing it with a symbolic x would need the
    /// unroll machinery for something the emitted kernel does in one
    /// call. Four of the cart's six symbolic loops are this function.
    ///
    /// The caller folds it when everything IS known, so the concrete
    /// domain never reaches here.
    fn tile_flag_at(
        &mut self,
        x: &Self::Num,
        y: &Self::Num,
        w: &Self::Num,
        h: &Self::Num,
        flag: &Self::Num,
    ) -> Result<Self::Bool>;

    /// A fresh UNKNOWN boolean - one of the search's free choices.
    ///
    /// The oracle has no such thing: to run a frame concretely you supply
    /// the actual button values, so `Concrete` refuses rather than
    /// inventing one. That asymmetry is real and worth the loud failure -
    /// it is the difference between running the game and compiling it.
    fn unknown_bool(&mut self) -> Result<Self::Bool>;

    /// A number known only to lie in `[lo, hi]` - `rnd`'s value. Like
    /// `unknown_bool` this is a thing only an abstract domain has: the
    /// concrete oracle refuses rather than inventing a draw.
    fn range_num(&mut self, lo: P8, hi: P8) -> Result<Self::Num> {
        bail!("a number in [{lo:?}, {hi:?}] has no concrete value - this domain runs real inputs only")
    }

    /// How much graph there is, for the tracer's budget check. Zero for a
    /// domain that does not build one.
    fn node_count(&self) -> usize {
        0
    }

    /// A number this value definitely is, if the domain knows it. The
    /// interpreter needs this for things the HEAP depends on - an array
    /// index, a table key, a loop bound - where an unknown is a refusal
    /// rather than a branch.
    fn as_const(&self, v: &Self::Num) -> Option<P8>;

    /// What this value IS, for an error message.
    ///
    /// A refusal that says "`start` is symbolic" leaves the reader
    /// guessing between an input cell, a fold that did not fire, and a
    /// genuine expression - three completely different fixes. Costs
    /// nothing until something refuses.
    fn describe(&self, _v: &Self::Num) -> String {
        "<opaque>".to_string()
    }

    /// Is this value an INTERVAL - a set of numbers rather than one?
    ///
    /// Asked before forking, because forking a value that is already a
    /// single number costs an outcome and buys nothing. Every object
    /// calls `move`, so a room with n moving objects would get 2^2n fork
    /// configurations for the sake of one player.
    fn is_interval(&self, _v: &Self::Num) -> bool {
        false
    }

    /// Is a merged value a SELECT the kernel reads by its condition's value
    /// bit (`Op::Sel`), rather than a value the merge folded away (equal
    /// arms, a decided condition) or into Kleene boolean algebra, which the
    /// kernel evaluates exactly on (value, known) masks? Only the former
    /// reads the merge's condition, so only it can make the merge refuse
    /// (`state::merge`). A concrete merge never selects.
    fn is_select_num(&self, _v: &Self::Num) -> bool {
        false
    }

    /// `is_select_num` for a boolean.
    fn is_select_bool(&self, _b: &Self::Bool) -> bool {
        false
    }

    /// Fork at `flr`: the value restricted to a fresh fork choice, and
    /// which lanes fall in the chosen fragment.
    ///
    /// `flr` of an interval is not a function - the lane holds points
    /// whose floors differ - so the cart marks the place with
    /// `__split_by_flr` and the program enumerates the cases. This
    /// returns ONE node, not two states: the fragment is a choice, like
    /// a button, and specialization enumerates it (`Choice::Split`).
    ///
    /// `ways` is the fork's arity - how many floors the fragments cover.
    ///
    /// The default is the identity, which is what an exact value needs:
    /// one fragment, always valid.
    fn fork_flr(&mut self, v: &Self::Num, _ways: u8) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// A fork over an interval of WHOLE numbers whose fragments are the
    /// numbers themselves, each EXACT (`Op::SplitInt`): the player's
    /// position under a bucket. Same validity and premise as `fork_flr`.
    fn fork_int(&mut self, v: &Self::Num, _ways: u8) -> (Self::Num, Self::Bool) {
        (v.clone(), self.boolean(true))
    }

    /// `n` was computed where `at` holds - a fork's fragment, a floor: the
    /// operators with an own error (`trace::error`), which holds only on the
    /// lanes that evaluate them. Off its path a node's operands are whatever
    /// the lane's own path left there. Nothing to record for a domain that
    /// has no graph.
    fn evaluated_at(&mut self, _n: &Self::Num, _at: &Self::Bool) {}

    /// The arity of the fork `__split_by_flr` takes on `v`: `move_ways`,
    /// or where the trace knows `v`'s static range (a kernel specialized
    /// on its speed key) the most grid cells one piece of it crosses -
    /// a lane's interval lies within one piece, and `SplitOk` checks it.
    fn flr_ways(&mut self, _v: &Self::Num) -> u8 {
        self.move_ways()
    }

    /// The arity of the `move` fork (`__split_by_flr`): 2 unless the
    /// traced set buckets the player's speed, where an object updating
    /// before the player (the spring: `hit.spd.x *= 0.2`) can hand
    /// `move` a bucket no longer aligned to the grid, and `rem + spd +
    /// 0.5` then spans THREE floors (room (2,0) f36, 2026-09-14).
    fn move_ways(&self) -> u8 {
        2
    }

    /// The join of a merge on a condition NO LANE can decide, where the arms
    /// are values every lane holds alike (plans/fly-fruit.md): the hull of
    /// two literal intervals, or the unknown number. `None` keeps the
    /// ordinary select (and its `Known` premise). Only a fruit-unknown set
    /// has such conditions; the concrete domain never merges.
    fn join_num_independent(&mut self, _c: &Self::Bool, _t: &Self::Num, _f: &Self::Num) -> Option<Self::Num> {
        None
    }

    /// `join_num_independent` for booleans: two different arms join to an
    /// undecided atom.
    fn join_bool_independent(&mut self, _c: &Self::Bool, _t: &Self::Bool, _f: &Self::Bool) -> Option<Self::Bool> {
        None
    }

    /// Does the undecided condition `c` read an unknown atom (`Op::UnknownBool`)?
    /// No lane ever decides an atom, so wherever `c` depends on one it stays
    /// undecided: a merge on `c` that would leave a select is not a merge but two
    /// successors, since those lanes would decline the select's `Known(c)`
    /// premise (`state::merge`).
    fn reads_unknown_atom(&mut self, _c: &Self::Bool) -> bool {
        false
    }

    /// `__split_by_flr` of a value every lane holds alike (a literal interval):
    /// its fragments on the fork grid, as literals, for the interpreter to run
    /// as separate trace states and rejoin when the enclosing call returns
    /// (`Interp::rejoin_fragments`). `None`: an ordinary per-lane fork.
    fn literal_fragments(&mut self, _v: &Self::Num) -> Option<Vec<Self::Num>> {
        None
    }

    /// A fresh undecided boolean every lane holds alike: what a literal split's
    /// fragments are merged on when they rejoin.
    fn undecided_atom(&mut self) -> Result<Self::Bool> {
        bail!("this domain has no undecided booleans")
    }

    /// How many undecided atoms this frame has handed out: `Interp` marks it
    /// where a call begins (`Interp::fork_escaped_atoms`).
    fn atoms_minted(&self) -> u32 {
        0
    }

    /// An atom handed out at or after `since` that ESCAPES the call that made
    /// it (held in the heap, or returned) becomes a fork of both values, one per
    /// atom. Inside the call it stays an atom; after it, a read by lane data (a
    /// fall floor's `collideable`, joined under undecided `state` branches, read
    /// by the player's collisions) decides per configuration instead of leaving
    /// every such decision undecided and its arms apart. Fresh, so nothing read
    /// it before; both values contain the atom's. `None` for anything else.
    fn escaped_atom(&mut self, _b: &Self::Bool, _since: u32, _origin: &dyn Fn(&Self) -> String) -> Option<Self::Bool> {
        None
    }

    /// A number for a diagnostic (a fork's origin): its value if known.
    fn describe_num(&self, n: &Self::Num) -> String {
        format!("{n:?}")
    }
}

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

/// The tracing domain. Owns the graph it is building.
#[derive(Default, Clone)]
pub struct Symbolic {
    pub graph: Graph,
    /// How many free choices have been handed out. `__reset_button_states`
    /// asks for six, in slot order, which is what makes these line up with
    /// the kb0..kb5 the kernels already speak.
    pub frees: u8,
    /// How many FORK choices have been handed out this frame: the fork ids,
    /// beside the six buttons.
    pub forks: u8,
    /// The arity of the `move` fork for the set being traced (see
    /// `Domain::move_ways`); `trace_frame` sets it from the widen mode.
    /// 0 (the default) reads as 2.
    pub move_ways: u8,
    /// `flr_ways` takes a static range's FULL width, not capped at
    /// `move_ways`: level -1, whose speed is a range at every node rather
    /// than exact per lane (`level_minus_one`). Off everywhere else.
    pub uncapped_ways: bool,
    /// Held buttons unknown for the set being traced
    /// (`abstraction::HeldPrecision`): `trace_frame` forks the player's
    /// `p_jump` / `p_dash` (`widen::fork_held_inputs`). Set by the walk.
    pub held_unknown: bool,
    /// The fly fruit unknown for the set being traced
    /// (`abstraction::FruitPrecision`, plans/fly-fruit.md): `trace_frame`
    /// replaces the fruit's inputs (`widen::fork_fruit_inputs`), arithmetic on
    /// literal intervals folds to literals, and a merge on a condition no lane
    /// decides joins its literal arms (`join_num_independent`). Set by the walk.
    pub fruit_unknown: bool,
    /// The fall floors unknown (`abstraction::FloorsPrecision`,
    /// plans/fall-floors.md): `trace_frame` replaces every fall floor's
    /// `state`, `delay` and `collideable` (`widen::fork_floor_inputs`), with the
    /// same literal and atom machinery as the fruit (`unknowns`). Set by the walk.
    pub floors_unknown: bool,
    /// The moving platforms unknown (`abstraction::PlatformsPrecision`,
    /// plans/platforms-unknown.md): `trace_frame` widens their inputs
    /// (`widen::fork_platform_inputs`), every outcome their outputs.
    pub platforms_unknown: bool,
    /// How many `Op::UnknownBool` atoms this frame handed out.
    pub unknown_atoms: u32,
    /// `escaped_atom`'s memo: the fork each escaped atom became, this frame.
    /// Cleared with `unknown_atoms` (atom ids restart per frame).
    pub escaped: rustc_hash::FxHashMap<NodeId, NodeId>,
    /// What each fork `both_values` made was made for (a held trail, an
    /// escaped atom's slot), for the kernel dump. Cleared with `escaped`.
    pub fork_origins: Vec<(u8, String)>,
    /// WHERE each node with an own error was evaluated: the OR of the path
    /// guards the tracer built it under (`Domain::evaluated_at`), because
    /// its error holds only there (`trace::error`). Per trace, like the
    /// fork numbering.
    pub evaluated: rustc_hash::FxHashMap<NodeId, NodeId>,
    /// Keep a traced frame's undecided selects as selects: level -1, whose
    /// evaluator joins an undecided select's arms, sets it around its trace.
    /// Off, the frame's surviving selects on a condition a lane can hold
    /// undecided become forks (`verify::fork_undecided_selects`).
    pub no_known_forks: bool,
    /// The moving platforms' `x` input values at a platforms-unknown level
    /// THE PLATFORM WORLDS (`concrete::platform_worlds`): every arrangement
    /// of the start room's moving platforms a search can meet, each
    /// platform's `(x, last, rem.x, spd.x)`. What a platforms-unknown frame
    /// reads its platforms from (`widen::fork_platform_inputs`).
    pub worlds: Option<std::sync::Arc<Vec<Vec<[i32; crate::concrete::WORLD_FIELDS]>>>>,
    /// `lane_independent`'s memo. Structural (a node's op and operands never
    /// change), so it outlives a frame.
    lane_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// `reads_unknown_atom`'s memo: structural, like `lane_memo`.
    atom_memo: rustc_hash::FxHashMap<NodeId, bool>,
    /// Fork choices already handed out THIS FRAME, by the value forked.
    ///
    /// Two call sites that floor the same value do not need two choice
    /// dimensions: forking one value twice yields the same fragments,
    /// so the second choice is determined by the first and every
    /// configuration where they disagree is empty. The runtime masks
    /// prune those, so it was never wrong - it was two extra levels of
    /// loop nest and a block of emitted nodes each.
    ///
    /// Measured on room (2,0) before this existed: `kernel1` and
    /// `kernel4` forked 16 times over 14 distinct values, which is
    /// ~10,600 of their 83,710 nodes (12.7%) and 65,536 runtime
    /// configurations instead of 16,384.
    ///
    /// Per frame, like `forks` itself - cleared beside it.
    ///
    /// Keyed by `(value, kind)`, NOT by value: `Floors` and `Ints` over one
    /// value are different ops (`Split` against `SplitInt`) resolving to
    /// different fragments, so they are different forks and may not share a
    /// choice. `Table` is absent from this map by design - see `Partition`.
    fork_memo: std::collections::HashMap<(NodeId, ForkKind), (u8, (NodeId, NodeId))>,
    /// Input cells that hold an INTERVAL rather than a number - the
    /// player's `rem.x`/`rem.y`, which the boundary widens.
    ///
    /// By CELL, in the tracer's own dense numbering, because that is what
    /// a graph node names. `is_interval` is a cone query against this
    /// set: a value is an interval exactly when it was computed from one.
    pub ival_cells: std::collections::BTreeSet<u32>,
    /// `is_interval`'s memo, valid for the current `ival_cells` (nodes never
    /// change): asked of every select's condition at the end of a frame
    /// (`verify::fork_undecided_selects`), and a graph-sized memo per call was
    /// fine only while forks were the one caller. Dropped with
    /// `forget_intervals` wherever `ival_cells` changes.
    ival_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `abstractness`'s memo - the TYPE PROPAGATION pass, which asks whether a
    /// node's value is a SET rather than a single value. Valid for the current
    /// `ival_cells` exactly as `ival_memo` is, and dropped beside it in
    /// `forget_intervals`.
    abstract_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `lane_undecidable`'s memo - THE FORK TRIGGER. Derived from
    /// `abstractness`, so it depends on `ival_cells` too and is dropped with
    /// the other two.
    undecidable_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `abstract_beneath_lane_ops`'s memo, dropped with the rest.
    beneath_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, bool>>,
    /// `may_answers`'s memo: a condition's `(may_true, may_false)` pair.
    /// Depends on `lane_undecidable` and so on `ival_cells`, so it is dropped
    /// with the other three in `forget_intervals`.
    may_memo: std::cell::RefCell<rustc_hash::FxHashMap<NodeId, (NodeId, NodeId)>>,
    /// STATIC RANGES (2026-09-15, the bucket dispatch): input cells whose
    /// value is known to lie in a range - a body specialized on the
    /// player's speed BUCKET - and the memo of the range analysis over
    /// them (`range_of`). A comparison both of whose operands have ranges
    /// that decide it folds to a constant (`compare`), so the branch is
    /// never traced and the merge never built. Per frame, like `forks`.
    pub ranges: std::collections::HashMap<NodeId, (i64, i64)>,
    range_memo: std::collections::HashMap<NodeId, Option<Pieces>>,
    /// Comparisons `compare` decided from ranges this frame (a probe stat).
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

    /// THE FORK TRIGGER (plans/graph-model.md section 2): can ONE LANE hold
    /// this condition both ways?
    ///
    /// Not `abstractness`, and the difference is the whole point. A lane is
    /// itself a set of states and the kernel computes on interval
    /// representations, so a value can denote a set and still be one computed
    /// quantity per lane that the kernel can branch on. What forces a fork is
    /// narrower: the condition's value differs ACROSS THE CONCRETE STATES ONE
    /// LANE STANDS FOR.
    ///
    /// Exhaustive over every `Op`, because the two exclusions below are the
    /// load-bearing part and a catch-all hides them (`reads_interval_cmp`'s
    /// `_ => false` gets them right by accident and would get the next
    /// genuinely undecidable op wrong):
    ///
    /// * `TileFlagAt` and `Mget` are LANE-DECIDABLE BY INSTRUCTION. They lower
    ///   to one call over number registers, so a lane gets a single answer
    ///   however abstract its coordinates are.
    /// * `Flr` is LANE-DECIDABLE BY ASSERTION - `zi_flr_ok` reads decidedness
    ///   off the interval - and that assertion is the operator's own error
    ///   (section 4), not a type fact. This is the one place the two
    ///   justifications differ, and conflating them is what made me fold
    ///   `Known(Flr(..))` away unsoundly.
    ///
    /// Measured on room (6,0): triggering on `abstractness` instead would mint
    /// 50-90 extra forks a frame, all of them these two families.
    pub fn lane_undecidable(&self, b: NodeId) -> bool {
        fn go(d: &Symbolic, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(x) = memo.get(&n) {
                return *x;
            }
            let node = d.graph.get(n);
            let args = node.args.clone();
            let any = |memo: &mut rustc_hash::FxHashMap<NodeId, bool>, xs: &[NodeId]| xs.iter().any(|x| go(d, memo, *x));
            let r = match node.op {
                // THE source: a lane's states answer a comparison both ways
                // exactly when an operand spans values - but "spans values"
                // here must IGNORE the lane-decidable operators, or the
                // exclusions below never bite. `Gt(Flr(Split(..)), 0)` has an
                // abstract operand by `abstractness`, yet the kernel decides
                // it per lane because the span assertion makes the floor
                // unique. Same for a coordinate that feeds `TileFlagAt`.
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => args.iter().any(|a| d.abstract_beneath_lane_ops(*a)),
                Op::UnknownBool(_) => true,
                Op::Not | Op::And | Op::Or | Op::Sel => any(memo, &args),
                // Lane-decidable by instruction.
                Op::TileFlagAt | Op::Mget => false,
                // Lane-decidable by assertion (`own_error`, not a type fact).
                Op::Flr => false,
                // A mask query; every lane decides it.
                Op::Known => false,
                // The validity and coverage masks are per-lane comparisons the
                // kernel evaluates, not conditions a lane straddles.
                Op::SplitValid(_) | Op::SplitValidTab(_) | Op::SplitOk(_) | Op::SplitOkTab(_) | Op::FragOk(_) => false,
                Op::ConstBool(_) => false,
                // Not boolean-valued, so not a condition.
                _ => false,
            };
            memo.insert(n, r);
            r
        }
        go(self, &mut self.undecidable_memo.borrow_mut(), b)
    }

    /// `abstractness`, but STOPPING at the operators a lane decides for
    /// itself: `Flr` (unique by its span assertion) and `TileFlagAt`/`Mget`
    /// (one instruction, one answer per lane).
    ///
    /// This is what a comparison's operands must be judged by. Asking plain
    /// `abstractness` there makes `Gt(Flr(Split(..)), 0)` a fork trigger, and
    /// measured on room (6,0) that is 38-50 spurious forks a frame - the
    /// difference between this predicate and the `Known` premises it replaces.
    pub fn abstract_beneath_lane_ops(&self, n: NodeId) -> bool {
        fn go(d: &Symbolic, memo: &mut rustc_hash::FxHashMap<NodeId, bool>, n: NodeId) -> bool {
            if let Some(x) = memo.get(&n) {
                return *x;
            }
            let node = d.graph.get(n);
            let args = node.args.clone();
            let r = match node.op {
                // A lane decides these whatever their operands, so nothing
                // beneath them can make a comparison straddle.
                Op::Flr | Op::TileFlagAt | Op::Mget => false,
                Op::Cell(c) => d.ival_cells.contains(&c),
                Op::Const(lo, hi) => lo != hi,
                Op::Span => true,
                Op::UnknownNum | Op::UnknownBool(_) => true,
                Op::Split(_) | Op::SplitTab(_) | Op::SplitKeyTab(_) | Op::Frag(_) => true,
                Op::SplitInt(_) | Op::IntFrag(_) | Op::Lo | Op::Hi => false,
                Op::ConstBool(_) | Op::Free(_) | Op::Known => false,
                // THE CONDITION DOES NOT MAKE THE RESULT A SET. `Sel(c, 5, 7)`
                // is one of two exact numbers whatever `c` is; that a lane may
                // take either arm is a BRANCHING question, answered by forking
                // `c` itself, not by calling this operand's value a range.
                // Counting the condition here (my first version did, via the
                // catch-all) over-triggered on 31-33 conditions a frame in room
                // (6,0). `is_interval` has always had this rule; note that
                // `lane_undecidable` deliberately does the OPPOSITE for
                // booleans, where a select genuinely straddles when its
                // condition does.
                Op::Sel => args[1..].iter().any(|a| go(d, memo, *a)),
                _ => args.iter().any(|a| go(d, memo, *a)),
            };
            memo.insert(n, r);
            r
        }
        go(self, &mut self.beneath_memo.borrow_mut(), n)
    }

    /// TYPE PROPAGATION (plans/graph-model.md section 2): does this node's
    /// value denote a SET of concrete values rather than a single one?
    ///
    /// This is the whole of what decidedness means, and it is static. A node
    /// whose value is a singleton is one every lane decides; a node whose
    /// value is a set is one no lane need decide, so a branch on it is what
    /// stage 2 has to fork.
    ///
    /// NOT `is_interval`, which answers a different question and must keep
    /// doing so: whether a NUMBER is an interval, for the lowering's ZI/ZN
    /// typing and for the widenings. Two differences are load-bearing:
    ///
    /// * a `Sel` counts its CONDITION, because an undecided condition is
    ///   precisely what makes the select unevaluable, where for interval-ness
    ///   only the arms matter;
    /// * `Flr` FOLLOWS ITS OPERAND. `is_interval` calls it exact because
    ///   the lane survives only where the floor is unique, i.e. it assumes
    ///   the very obligation. Here `Flr` is a PARTIAL operator: a singleton
    ///   only on pain of error, and that error is the operator's own
    ///   (`trace::error`), not a type fact. Reading
    ///   the shortcut into this pass is what made me fold `Known(Flr(..))`
    ///   away unsoundly on 2026-09-26.
    ///
    /// Every `Op` is listed on purpose - no catch-all. `reads_interval_cmp`
    /// has a `_ => false` arm, which silently calls `TileFlagAt` over
    /// interval coordinates decidable; harmless for a diagnostic, unsound
    /// the moment it decides whether to fork.
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
                // Resolved to a constant per body, so decided.
                Op::Free(_) => false,
                // The genuinely unknown atoms.
                Op::UnknownNum | Op::UnknownBool(_) => true,
                // A span exists because its bounds are different nodes.
                Op::Span => true,
                // A fragment of an interval is a narrower interval; a
                // fragment of the integer grid is one exact number. The
                // validity and coverage masks are per-lane comparisons on
                // the operand, so they follow it.
                Op::Split(_) | Op::SplitTab(_) | Op::SplitKeyTab(_) | Op::Frag(_) => true,
                Op::SplitInt(_) | Op::IntFrag(_) => false,
                Op::SplitValid(_) | Op::SplitValidTab(_) | Op::SplitOk(_) | Op::SplitOkTab(_) | Op::FragOk(_) => any(memo, &args),
                // The ends of an interval are exact numbers.
                Op::Lo | Op::Hi => false,
                // A premise is a mask query: every lane decides it.
                Op::Known => false,
                // PARTIAL: a singleton only on pain of error. See above.
                Op::Flr => any(memo, &args),
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Min | Op::Max | Op::Sin => any(memo, &args),
                Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => any(memo, &args),
                Op::Not | Op::And | Op::Or => any(memo, &args),
                // The condition counts: that is what cannot be evaluated.
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

    /// A fresh undecided atom (`Op::UnknownBool`), distinct from every other
    /// this frame.
    /// Is anything unknown in the set being traced (the fly fruit, the fall
    /// floors)? The literal folding, the independent joins and the literal
    /// splits switch on with it.
    pub fn unknowns(&self) -> bool {
        self.fruit_unknown || self.floors_unknown || self.platforms_unknown
    }

    /// `n` as `base + literal`: an `Add` with a literal operand (either side),
    /// else `n + 0`.
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

    /// Does boolean `n` read a comparison with an INTERVAL operand - a
    /// condition a lane can hold undecided, its states answering both ways
    /// (`verify::fork_undecided_selects`)?
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

    /// For a boolean `n`: where a lane CAN answer true, and where false, over
    /// the concrete states it stands for - a cover, both may hold. A
    /// comparison reads its operands' ends (`a < b` can hold iff `lo(a) <
    /// hi(b)` and fail iff `hi(a) >= lo(b)`; a number is its own ends); a node
    /// no interval reaches is decided per lane, `(n, not n)`; connectives
    /// combine; anything else may answer either way.
    /// Judged by `lane_undecidable`, THE SAME PREDICATE THAT DECIDES WHAT TO
    /// FORK. It has to be: this computes a fork's validity, so if the two
    /// disagreed about which conditions a lane can hold both ways, a fork's
    /// answers would be described by a rule other than the one that created
    /// it. The old `reads_interval_cmp` says `Gt(Flr(interval), 0)` is
    /// undecidable, where a lane decides it (`zi_flr_ok`) - so it would build
    /// endpoint comparisons for a condition whose answers are exactly `n` and
    /// `not n`.
    pub fn may_answers(&mut self, n: NodeId) -> (NodeId, NodeId) {
        // MEMOISED, like every other pass here, because it is a pure function
        // of the graph - and because without it this was 99.96% of
        // `fork_undecided_selects` and half of room (6,0)'s lattice walk.
        //
        // The recursion below descends `And`/`Or`/`Not`, and the graph is a
        // DAG, so an unmemoised walk recomputes every shared subtree once per
        // path that reaches it - and each recomputation re-runs
        // `lane_undecidable`, which walks cones of its own. Measured before
        // this: 14.7 s in one trace for 91 conditions, ~160 ms each, against
        // 0.6 s for the whole ascending rebuild beside it.
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

    /// `a - r` where `a` is `r + c` (either operand order): `c`, EXACTLY - the
    /// identity holds in 16.16 wrapping arithmetic - and `r - r` is 0. Also
    /// under the arms of a select `a`, where an arm cancels (a platform's carry
    /// `x - last`, with `last` its own input `x` and the wrap's select between,
    /// plans/platforms-unknown.md). In the tracer's arithmetic, not
    /// `Graph::fold`: over ranges `(r + c) - r` evaluates wider than `c`, and
    /// `fold` must agree with the evaluator exactly. `None` where nothing
    /// cancels.
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

    /// A boolean every lane holds in BOTH values: a 2-way fork with no
    /// validity (every configuration applies to every lane), `choice > 0` over
    /// the whole grid - the held buttons' fork (`widen::fork_held_inputs`).
    ///
    /// The fork over the platform worlds (`widen::fork_platform_inputs`):
    /// `n` ways over the literal `[0, n - 1]`, the configuration's world
    /// index as a whole number. Every configuration applies to every lane,
    /// so there is no validity.
    pub fn world_choice(&mut self, n: usize) -> Result<NodeId> {
        anyhow::ensure!((1..=crate::transpile::graph::MAX_WAYS).contains(&n), "{n} platform worlds, more than a fork holds ({})", crate::transpile::graph::MAX_WAYS);
        let choices = self.graph.leaf(Op::Const(0, ((n - 1) as i32) << 16));
        Ok(self.fork(&choices, Partition::Ints { ways: n as u8, memo: false }, "the platform world").value)
    }

    /// An `Ints` fork over a CONSTANT, so it goes through `fork` like the
    /// rest; `Partition::Ints { memo: false }` because the two held trails are
    /// independent and must not share one choice even though the operand node
    /// is the same for both (see `Partition`). The validity is dropped rather
    /// than ignored: every configuration applies to every lane.
    pub fn both_values(&mut self, origin: &str) -> NodeId {
        let choices = self.graph.leaf(Op::Const(0, 1 << 16));
        let f = self.fork(&choices, Partition::Ints { ways: 2, memo: false }, origin);
        let zero = self.graph.leaf(Op::Const(0, 0));
        self.graph.fold(Op::Gt, vec![f.value, zero])
    }

    /// Forget this frame's fork choices: a new frame's forks start at 0.
    /// Called beside `forks = 0` and `Graph::reset_forks`.
    pub fn clear_fork_memo(&mut self) {
        self.fork_memo.clear();
    }

    /// A table fork (`Partition::Table`), the one partition only `Symbolic`
    /// can take: the row's bucket and the coverage premise are further ops
    /// naming the fork (`SplitKeyTab` / `SplitOkTab` in
    /// `widen::spd_table_node`), so the caller needs its id.
    ///
    /// Not on `Domain`: a fork id is a graph notion, and the reference domain
    /// forks by walking one fragment at a time through its cursor instead.
    pub fn fork_table_at(&mut self, v: &NodeId, ranges: &[(i32, i32)], arity: u8) -> Fork {
        self.fork(v, Partition::Table { ranges: ranges.to_vec(), arity }, "a speed bucket table")
    }

    /// THE fork: partition `v`'s set, mint a choice dimension for the pieces,
    /// and hand back the piece this configuration takes plus which lanes fall
    /// in it (plans/graph-model.md section 3 - "forking is one operation").
    ///
    /// Every fork in a trace comes through here, so the bookkeeping that used
    /// to be copied into four functions is stated once: the fork id, its
    /// arity, its origin (for the kernel dump), and whether the choice is
    /// shared with an earlier fork of the same value.
    ///
    /// What differs between partitions is only how a configuration index
    /// becomes a value, which is the `Partition` - and on the graph, which op
    /// family carries it. Those ops must stay distinct: `Graph::specialize`
    /// resolves `Split` to `Frag`, `SplitInt` to `IntFrag` and the `SplitTab`
    /// family to per-lane select chains, so they are three different
    /// computations, not three spellings of one.
    fn fork(&mut self, v: &NodeId, p: Partition, origin: &str) -> Fork {
        let kind = p.kind();
        // Already forked this exact value this frame? Reuse the choice.
        //
        // Hash-consing is what makes this sound and also what makes it FIRE:
        // two `move` calls on values that are structurally the same
        // expression are the same node id. Forking one value twice yields the
        // same fragments, so the second choice is determined by the first and
        // every configuration where they disagree is empty.
        if p.memoized() {
            if let Some(hit) = self.fork_memo.get(&(*v, kind)) {
                let (d, out) = *hit;
                // RAISED, not set: each site's own `SplitOk` premise is what
                // bounds the lanes it admits, so the wider request wins.
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
            Partition::Table { ranges, arity } => {
                // SET, not raised, and it registers the table too: a table
                // fork's arity is its entry count, and the arena outlives a
                // trace, so fork `d` of the previous one may have had a
                // bigger table.
                self.graph.set_fork_table(d, ranges.to_vec(), *arity);
                (self.graph.fold(Op::SplitTab(d), vec![*v]), self.graph.fold(Op::SplitValidTab(d), vec![*v]))
            }
        };
        if p.memoized() {
            self.fork_memo.insert((*v, kind), (d, (value, valid)));
        }
        Fork { value, valid, id: d }
    }

    /// THE unknown boolean an output widening writes (held trails, the fly
    /// fruit's `fly`, the fall floors' `collideable`): one hash-consed node, so
    /// every body writing it writes the same thing, and `verify::out_fields`
    /// stores it as the uniform `AV::UBool` rather than as a root. Nothing in
    /// the frame reads an output, so sharing it between fields correlates
    /// nothing; inputs and comparisons take fresh atoms (`unknown_bool_atom`).
    pub fn unknown_bool_output(&mut self) -> NodeId {
        self.graph.leaf(Op::UnknownBool(u32::MAX))
    }

    pub fn is_unknown_output(&self, b: NodeId) -> bool {
        matches!(self.graph.get(b).op, Op::UnknownBool(u32::MAX))
    }

    pub fn unknown_bool_atom(&mut self) -> NodeId {
        let k = self.unknown_atoms;
        self.unknown_atoms += 1;
        self.graph.leaf(Op::UnknownBool(k))
    }

    /// Does no lane's data reach `n` - no input cell, button or fork?
    pub fn lane_independent(&mut self, n: NodeId) -> bool {
        let mut stack: Vec<(NodeId, bool)> = vec![(n, false)];
        while let Some((x, expanded)) = stack.pop() {
            if self.lane_memo.contains_key(&x) {
                continue;
            }
            let node = self.graph.get(x);
            let dependent = matches!(
                node.op,
                Op::Cell(_) | Op::Free(_) | Op::Split(_) | Op::SplitValid(_) | Op::SplitInt(_) | Op::SplitTab(_) | Op::SplitValidTab(_) | Op::SplitKeyTab(_) | Op::SplitOkTab(_)
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

    /// `op` over LITERAL operands, at least one an interval, in the graph's
    /// own interval semantics (`Graph::eval` on the literals: one definition,
    /// not a second): the value, or `None` where eval does not model it (a
    /// wrap at the 16.16 extremes, a non-positive scale). A fruit-unknown set
    /// only.
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
        // Fold where both sides are known, so that everything the heap
        // depends on - indices, counts, loop bounds - stays concrete
        // without the interpreter having to ask.
        if let (Some(x), Some(y)) = (self.as_p8(*a), self.as_p8(*b)) {
            let mut c = Concrete;
            return Ok(self.konst(c.arith(op, &x, &y)?));
        }
        if self.is_unknown_num(*a) || self.is_unknown_num(*b) {
            return Ok(self.unknown_num());
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
        // `sin` of anything is in [-1, 1]; every other builtin of an unknown
        // number is unknown.
        if self.is_unknown_num(*a) {
            return Ok(match f {
                Fun1::Sin => self.graph.leaf(Op::Const(-0x1_0000, 0x1_0000)),
                _ => self.unknown_num(),
            });
        }
        // `sin` of an interval is its full range [-1, 1], matching the
        // interpreter's `builtin_sin` (game_runner.rs) exactly. Emit the
        // constant range so the emitter never has to lower a `Sin` over a
        // ZI (it cannot). This is the fruit bob's `sin((1+off)/40)` once
        // the boundary has widened `off` - see plans/specialize.md.
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
            return Ok(self.unknown_bool_atom());
        }
        // Two literals: decided by their intervals, or an undecided atom of
        // its own (the same in every lane, and never one atom with another).
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
        // Decided by the static ranges (the bucket dispatch): a branch
        // the specialized body never takes is never traced. Decided iff
        // every pair of pieces decides it the same way.
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
    fn unknown_bool(&mut self) -> Result<NodeId> {
        if self.frees >= 6 {
            bail!("more than six free choices - the kernels only model six buttons");
        }
        let b = self.frees;
        self.frees += 1;
        Ok(self.graph.leaf(Op::Free(b)))
    }
    fn node_count(&self) -> usize {
        self.graph.len()
    }
    /// Is this value an interval?
    ///
    /// NOT a cone query. "An interval input is reachable" is the wrong
    /// question and it was the first thing I wrote: `dash_effect_time`
    /// is a `Sel` whose CONDITION compares a position derived from
    /// `rem`, so an interval reaches it - but its arms are numbers and so
    /// is the result. Typing it as an interval made the kernel write an
    /// `AV::Ival` into a column the boundary then refused, at
    /// `player dash_effect_time is not a number`.
    ///
    /// So this is a proper type rule, and deliberately the same one
    /// `transpile::lower`'s `Repr::wide` applies - a select is an
    /// interval when an ARM is, `flr` never is (it is exact by guard),
    /// and a cart lookup never is. The two are checked against each
    /// other by construction: disagree, and lowering fails with "output
    /// cell wants a ZN but the graph computes a ZI".
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
                Op::Split(_) | Op::SplitTab(_) | Op::SplitKeyTab(_) => true,
                // An exact whole number per configuration.
                Op::SplitInt(_) | Op::Lo | Op::Hi => false,
                // Unconditionally, like a non-degenerate `Const`: a
                // span exists precisely because its two bounds are
                // different nodes. (`fold` collapses a span of two
                // literals to a `Const`, so the degenerate case is
                // already gone by the time anything asks.) The literal
                // interval and the computed one are the same kind of
                // value, and this is the sibling of `Op::Const(lo, hi)
                // if lo != hi` above.
                Op::Span => true,
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs
                | Op::Min | Op::Max => any(memo, a),
                // The CONDITION does not make the result an interval.
                Op::Sel => any(memo, &a[1..]),
                // Exact where the lane survives: a floor that is not unique
                // is the operator's own error (`trace::error`).
                Op::Flr => false,
                // The walk replaces `sin` of an inexact input with its
                // RANGE as a constant, so this follows its operand.
                Op::Sin => any(memo, a),
                _ => false,
            };
            memo.insert(n, r);
            r
        }
        // NO early-out on an empty `ival_cells`. A value can be an
        // interval without any interval INPUT reaching it: the widening
        // writes `Op::Const(lo, hi)` with `lo != hi`, a literal
        // interval. Returning false for those typed a widened `rem` as
        // a `ZN` and the lowering refused it - "output cell 278 wants a
        // ZN but the graph computes a (P8, P8)".
        go(&self.graph, &self.ival_cells, &mut self.ival_memo.borrow_mut(), *v)
    }

    fn fork_flr(&mut self, v: &NodeId, ways: u8) -> (NodeId, NodeId) {
        let f = self.fork(v, Partition::Floors { ways }, "a floor fork (`move`)");
        (f.value, f.valid)
    }

    fn fork_int(&mut self, v: &NodeId, ways: u8) -> (NodeId, NodeId) {
        let f = self.fork(v, Partition::Ints { ways, memo: true }, "a bucketed position");
        (f.value, f.valid)
    }

    fn flr_ways(&mut self, v: &NodeId) -> u8 {
        // Only a trace with seeded ranges knows a static range: an
        // unspecialized trace keeps the fixed arity (and its gates).
        if self.ranges.is_empty() {
            return self.move_ways();
        }
        // At most `move_ways`: the static range is over ALL lanes, and one
        // lane's interval spans at most that many floors. A region kernel's
        // speed range [-S, S] is 2S + 1 floors wide while each lane's speed is
        // exact, and taking the range's width made the move forks 13-way
        // (room (3,0) square (5,13) on an 8 px grid: 1,236 bodies -> 35,502,
        // 2026-09-18). Too small an arity declines loudly (`SplitOk`).
        // Level -1 (`uncapped_ways`) needs the full width: its lanes ARE
        // ranges.
        match self.range_of(*v) {
            Some(ps) => {
                let sh = 16 - self.graph.fork_bits() as u32;
                let cap = if self.uncapped_ways { i64::from(u8::MAX) } else { self.move_ways() as i64 };
                ps.iter().map(|p| (p.1 >> sh) - (p.0 >> sh) + 1).max().unwrap_or(1).clamp(1, cap) as u8
            }
            None => self.move_ways(),
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

    fn move_ways(&self) -> u8 {
        self.move_ways.max(2)
    }

    fn join_num_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        if !self.unknowns() || self.decide(c).is_some() || !self.lane_independent(*c) {
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
            // The SAME value plus a literal on each side (a platform's `x0` and
            // `x0 + 1` from its move's two fragments, plans/platforms-unknown.md):
            // that value plus the literals' hull. A lane holds one of the two,
            // both lie in it, and the value stays itself - so what reads it
            // later still knows it (`x - last` cancels, `Symbolic::cancel_sub`).
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
        // Post-order, every node walked memoized (`lane_independent`'s walk):
        // the conditions a frame merges on share most of their cones.
        let mut stack: Vec<(NodeId, bool)> = vec![(*c, false)];
        while let Some((x, expanded)) = stack.pop() {
            if self.atom_memo.contains_key(&x) {
                continue;
            }
            let node = self.graph.get(x);
            if matches!(node.op, Op::UnknownBool(_)) || node.args.is_empty() {
                let atom = matches!(node.op, Op::UnknownBool(_));
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

    fn join_bool_independent(&mut self, c: &NodeId, t: &NodeId, f: &NodeId) -> Option<NodeId> {
        if !self.unknowns() || self.decide(c).is_some() || !self.lane_independent(*c) {
            return None;
        }
        if t == f {
            return Some(*t);
        }
        Some(self.unknown_bool_atom())
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
        match self.graph.get(*b).op {
            Op::UnknownBool(k) if k >= since => {
                if let Some(f) = self.escaped.get(b) {
                    return Some(*f);
                }
                let name = origin(self);
                let f = self.both_values(&name);
                self.escaped.insert(*b, f);
                Some(f)
            }
            _ => None,
        }
    }

    fn literal_fragments(&mut self, v: &NodeId) -> Option<Vec<NodeId>> {
        if !self.unknowns() {
            return None;
        }
        let Op::Const(lo, hi) = self.graph.get(*v).op else { return None };
        if lo == hi {
            return None;
        }
        let step = 1i64 << (16 - self.graph.fork_bits() as i64);
        let (lo, hi) = (lo as i64, hi as i64);
        let (fl, fh) = (lo.div_euclid(step) * step, hi.div_euclid(step) * step);
        if (fh - fl) / step + 1 > crate::transpile::graph::MAX_WAYS as i64 {
            return None;
        }
        let mut out = Vec::new();
        let mut base = fl;
        while base <= fh {
            let (a, b) = (lo.max(base), hi.min(base + step - 1));
            out.push(self.graph.leaf(Op::Const(a as i32, b as i32)));
            base += step;
        }
        Some(out)
    }

    fn undecided_atom(&mut self) -> Result<NodeId> {
        Ok(self.unknown_bool_atom())
    }

    fn as_const(&self, v: &NodeId) -> Option<P8> {
        self.as_p8(*v)
    }

    fn describe(&self, v: &NodeId) -> String {
        // One level of operands as well as the op. A bare `Sel/3` says
        // "a select" and leaves open the thing that decides whether a
        // refusal is easy to lift - whether its ARMS are constants.
        let n = self.graph.get(*v);
        let args: Vec<String> = n
            .args
            .iter()
            .map(|a| format!("{:?}", self.graph.get(*a).op))
            .collect();
        format!("node {} = {:?}({})", v, n.op, args.join(", "))
    }
}

/// A refusal the tracer makes rather than guessing. Kept separate from
/// ordinary errors because these are the interesting ones: each is a place
/// the heap would have had to become symbolic.
pub fn refuse_unknown(what: &str) -> anyhow::Error {
    anyhow::anyhow!(
        "{} is not known at trace time - the heap cannot depend on a symbolic value",
        what
    )
}

#[allow(dead_code)]
fn _assert_object_safe(_: &dyn Fn(&mut Symbolic)) {}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(v: i16) -> P8 {
        P8::from_i16(v)
    }

    /// The FORK TRIGGER's contract, pinned on hand-built graphs.
    ///
    /// `lane_undecidable` currently agrees with the `Known` premises it is
    /// about to replace - measured on room (5,0) exactly, and on room (6,0) up
    /// to the phantom forks it deliberately drops. That agreement is what
    /// licenses the substitution, and once `Known` is gone NOTHING ELSE PINS
    /// IT. So the four rules that took four attempts to get right are asserted
    /// here rather than left to a diagnostic that will be deleted.
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

        // `Flr` is lane-decidable BY ASSERTION (`zi_flr_ok`), so a comparison
        // on the floor of an interval does not fork - the span claim is an
        // `own_error`, not a fork trigger. Reading this the other way is what
        // made me fold `Known(Flr(..))` away unsoundly.
        let flr = d.graph.fold(Op::Flr, vec![ival]);
        let flr_cmp = d.graph.fold(Op::Gt, vec![flr, zero]);
        assert!(!d.lane_undecidable(flr_cmp), "Flr is decided per lane by its assertion");

        // `TileFlagAt` is lane-decidable BY INSTRUCTION: one call over number
        // registers, one answer per lane, however abstract the coordinates.
        let w = d.graph.leaf(Op::Const(8 << 16, 8 << 16));
        let tile = d.graph.fold(Op::TileFlagAt, vec![ival, ival, w, w, zero]);
        assert!(!d.lane_undecidable(tile), "TileFlagAt is decided per lane");
        // And a boolean tree over it stays decidable.
        let not_tile = d.graph.fold(Op::Not, vec![tile]);
        assert!(!d.lane_undecidable(not_tile));

        // THE `Sel` ASYMMETRY, which is easy to "tidy" away wrongly. For a
        // NUMERIC operand the condition is irrelevant: `Sel(c, 5, 7)` is one of
        // two exact numbers whatever `c` is, so a comparison on it does not
        // fork. Counting the condition here over-triggered on 31-33 conditions
        // a frame in room (6,0).
        let five = d.graph.leaf(Op::Const(5 << 16, 5 << 16));
        let seven = d.graph.leaf(Op::Const(7 << 16, 7 << 16));
        let num_sel = d.graph.fold(Op::Sel, vec![cmp, five, seven]);
        let sel_cmp = d.graph.fold(Op::Gt, vec![num_sel, zero]);
        assert!(
            !d.lane_undecidable(sel_cmp),
            "a select's condition does not make its numeric result span values"
        );
        // For a BOOLEAN select it is the opposite: it straddles exactly when
        // its condition does.
        let t = d.graph.leaf(Op::ConstBool(true));
        let f = d.graph.leaf(Op::ConstBool(false));
        let bool_sel = d.graph.fold(Op::Sel, vec![cmp, t, f]);
        assert!(d.lane_undecidable(bool_sel), "a boolean select straddles with its condition");
    }

    /// `abstractness` and `lane_undecidable` answer DIFFERENT questions, and
    /// conflating them cost a day. A value can denote a set and still be one
    /// computed quantity per lane.
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

    #[test]
    fn the_concrete_domain_always_decides() {
        let mut d = Concrete;
        let a = d.num(p(3));
        let b = d.num(p(4));
        let lt = d.compare(Cmp::Lt, &a, &b).unwrap();
        assert_eq!(d.decide(&lt), Some(true));
        // Which is why it never merges: `decide` returning Some is exactly
        // the condition under which the interpreter takes one arm.
        let sum = d.arith(Arith::Add, &a, &b).unwrap();
        assert_eq!(d.as_const(&sum), Some(p(7)));
    }

    #[test]
    fn the_symbolic_domain_folds_what_it_can() {
        let mut d = Symbolic::default();
        let a = d.num(p(3));
        let b = d.num(p(4));
        let sum = d.arith(Arith::Add, &a, &b).unwrap();
        // Constant-folded, so the HEAP can still index with it. This is
        // what keeps `count(objects)`, `#t` and loop bounds concrete
        // without the interpreter special-casing them.
        assert_eq!(d.as_const(&sum), Some(p(7)));
        let lt = d.compare(Cmp::Lt, &a, &b).unwrap();
        assert_eq!(d.decide(&lt), Some(true));
    }

    #[test]
    fn an_unknown_condition_is_what_makes_the_interpreter_branch() {
        let mut d = Symbolic::default();
        let x = d.graph.leaf(Op::Cell(1)); // stands in for game data
        let k = d.num(p(0));
        let gt = d.compare(Cmp::Gt, &x, &k).unwrap();
        assert_eq!(d.decide(&gt), None, "this is the case that merges");
        // And the merge itself collapses when the arms agree.
        let t = d.num(p(1));
        assert_eq!(d.sel_num(&gt, &t, &t), t);
        assert_ne!(d.sel_num(&gt, &t, &k), t);
    }
}
