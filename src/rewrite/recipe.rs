//! The recipe: an ordered list of rewrite instructions, checked into git.
//!
//! The program is always *derived* - `lua sources -> compile -> apply recipe`.
//! Nothing is ever edited by hand, so the recipe is the artifact we author and
//! review, and replaying it from scratch always reproduces the same program.
//!
//! Each entry carries a stable `id` chosen by the author, not its position in
//! the list. Ids are what generated names are derived from, so inserting an
//! entry in the middle does not invalidate the ones after it.
//!
//! `shape` names the heap shape the entry was authored against, for rules whose
//! side conditions are discharged against a shape rather than proved (see
//! plans/rewrite-plan.md section 2b). Bulk rules leave it null.

use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};

use crate::ir::LocalId;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RewriteEntry {
    /// Stable identifier, unique within the recipe. Author-chosen.
    pub id: String,
    #[serde(flatten)]
    pub rule: Rule,
    /// Heap shape this entry was authored against, if the rule needs one.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub shape: Option<String>,
    /// Free-text justification. Required for rules that are not
    /// semantics-preserving on their own.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub why: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "rule", rename_all = "snake_case")]
pub enum Rule {
    /// Remove dead instructions and unreachable blocks.
    Dce,
    /// One instruction per value within a basic block. A prerequisite for
    /// promoting object fields, which needs each cell to have a single
    /// accessor.
    Cse {
        /// Opt-in store-to-load forwarding with field-name alias refinement
        /// and heap-oblivious call exemption - see the `forward` section of
        /// the rule's docs. Off by default so old entries replay
        /// byte-identically.
        #[serde(default, skip_serializing_if = "is_false")]
        forward: bool,
        /// Opt-in (requires `forward`): pairs for loads of IR-`alloc` cells
        /// may cross block edges, so a store in one block forwards to a load
        /// in another. Sound because an `alloc` names exactly one cell, so
        /// the alias mask already computed per block is exact for it; a new
        /// flag so existing `forward` entries replay byte-identically.
        #[serde(default, skip_serializing_if = "is_false")]
        cells: bool,
    },
    /// Repack locals so that values with disjoint live ranges share a slot.
    /// Changes no instructions - only where they are stored.
    AllocateSlots,
    KillDead,
    /// Remove every derived `Kill` annotation. Member overlays open with
    /// this so structural rules can move code freely, and close with
    /// `kill_dead` to re-derive liveness on the final CFG.
    StripKills,
    /// Merge blocks into single-successor predecessors.
    MergeBlocks,
    /// Local simplifications: constant conditions, degenerate phis.
    Fold,
    /// A comparison of a number constant with itself becomes the constant
    /// reflexivity dictates. Kept apart from `fold` so older `fold` entries
    /// replay byte-identically.
    FoldReflexive {
        /// Opt-in for the pointer families: witnessed-pointer reflexive
        /// equality, nil against nil, witnessed pointer against nil.
        /// Off by default so entries written before these existed keep
        /// replaying byte-identically.
        #[serde(default, skip_serializing_if = "is_false")]
        pointers: bool,
    },
    /// Resolve selects and branches whose condition's truthiness is static:
    /// an `and` chain poisoned by a constant `false` is falsy all the way
    /// down, whatever the opaque links hold. Kept apart from `fold` for the
    /// same replay reason.
    FoldSelect,
    /// Splice a callee's body into one call site, guarded by an
    /// `assert_closure`.
    Inline {
        #[serde(rename = "fn")]
        function: String,
        /// The `call` instruction, as `%N`.
        at: String,
        /// The function to splice in.
        callee: String,
        /// Locals holding the callee's captured values, one per capture, as
        /// `%N`. The guard asserts the closure really did capture these, so a
        /// wrong guess fails loudly rather than reading the wrong object.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        captures: Vec<String>,
    },
    /// Collapse a branch whose arm is safe to run unconditionally into a
    /// `select` at the named join block.
    IfConvert {
        #[serde(rename = "fn")]
        function: String,
        /// The block the two paths join at.
        join: String,
    },
    /// Make every closure of a function capture a value rather than the cell
    /// holding it. Keyed by the callee, because one `FunDef` is shared by all
    /// its creation sites and they must change together.
    PromoteCapture {
        #[serde(rename = "fn")]
        function: String,
        /// Which capture position, counting from 0.
        index: usize,
    },
    /// Name the callee of a call to a pure builtin, so that `if_convert` can
    /// speculate it and `cse` can see through it. Guarded: the call fails if
    /// the callee is not the builtin named.
    PinBuiltin {
        #[serde(rename = "fn")]
        function: String,
        /// The `call` instruction, as `%N`.
        at: String,
        /// Which builtin. Must be one of `fixed_env::PURE_BUILTINS`.
        name: String,
    },
    /// Insert a state-splitting `__split_at(value, c)` call after `at`'s
    /// definition, redirecting later uses to the split result - the
    /// split-before-compare fix for interval values straddling a game
    /// decision threshold (plans/spd-rung.md). Semantically neutral on
    /// concrete values; a refinement split on intervals.
    SplitAt {
        #[serde(rename = "fn")]
        function: String,
        /// The value's defining instruction, as `%N` or a stable name.
        at: String,
        /// The threshold, raw 16.16 hex ("0x0000" = 0, "0x1_0000" = 1).
        threshold: String,
    },
    /// Turn one creating accessor into a plain read, guarded by an
    /// `assert_pointer`. Not semantics-preserving: it claims the field already
    /// exists there, and the guard is what makes a wrong claim loud instead of
    /// silent. Screen at full depth - see the rule's docs.
    DemoteCreate {
        #[serde(rename = "fn")]
        function: String,
        /// The accessor, as `%N`.
        at: String,
    },
    /// Delete an allocated cell nothing ever reads - its `alloc` plus every
    /// store into it. The whole claim (no loads, no escapes, no other use)
    /// is checked syntactically at apply time; see the rule's docs for why
    /// this is a pointed rule and not a widening of `dce`.
    DropDeadCell {
        #[serde(rename = "fn")]
        function: String,
        /// The cell's alloc, as `%N`.
        cell: String,
    },
    /// Convert the two joins Lua's `a and b or c` compiles to into one select
    /// on the original condition, in one step - one at a time is impossible,
    /// because the intermediate phi mixes a bool with a number. Requires the
    /// `and` arm's value to be statically truthy (`pin_builtin` supplies
    /// that for the `appr` sites).
    ConvertTernary {
        #[serde(rename = "fn")]
        function: String,
        /// The outer (`or`) join block.
        join: String,
    },
    /// Split a mixed `and`/`or` select cascade into (truthiness, value) pairs
    /// of homogeneous selects, so no select ever combines a Bool with a
    /// Number. Requires the cascade's root to be `x or k` with `k` statically
    /// truthy; the root keeps its id, every interior id dies.
    DecomposeTruthy {
        #[serde(rename = "fn")]
        function: String,
        /// The cascade's root select, as `%N`.
        root: String,
    },
    /// Hoist an arm's speculatable instructions into the head, leaving only
    /// its stores behind - the state `sink_store` / `absorb_stores` needs.
    /// Refused unless every hoist commutes with every store it crosses.
    Speculate {
        #[serde(rename = "fn")]
        function: String,
        /// The block the two paths join at.
        join: String,
        /// For a *diamond* at `join`: which of its two arms to hoist from.
        /// Absent for a triangle, which has only one.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        arm: Option<String>,
        /// Runtime-distinctness guards: each permits one hoisted load (or
        /// `assert_value_cell`) to cross one store whose cell the rule cannot
        /// prove distinct, paid for with an emitted pointer compare plus
        /// `assert_true` in the head. If the cells ever alias, the run dies
        /// loudly instead of reading the wrong value silently. Opt-in, so
        /// entries without guards emit byte-identically to before.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        guards: Vec<SpeculateGuard>,
    },
    /// Replace a branch whose arms are bare stores with a select-store in the
    /// head. Both arms storing to the same local needs no guard; a one-arm
    /// triangle uses the `sink_store` load-back trick and its
    /// `assert_value_cell` guard. The join is untouched, so it may have any
    /// number of other predecessors - but no phis. Screen at full depth.
    AbsorbStores {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is absorbed.
        head: String,
    },
    /// Run a pure single-entry single-exit subgraph (loops included)
    /// unconditionally instead of behind a branch: the head's branch becomes
    /// a jump into the region, the region is untouched, the join's phis
    /// become selects. Loops must have the guarded counter shape; the bound
    /// gets a loud `assert_true` range guard. Screen at full depth.
    SpeculateRegion {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is removed.
        head: String,
        /// The region's entry: the branch target that is not the join.
        arm: String,
        /// Diamond mode: where both branch targets' regions meet. The two
        /// regions are serialized, named arm first, and this join's phis
        /// become selects. Absent for a triangle, whose join is the head's
        /// other branch target.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        join: Option<String>,
        /// Allow stores in the region: each becomes an assert-load-select-
        /// store masked by the head's condition, landing only on the lanes
        /// that took its region. Opt-in so store-free entries refuse stores
        /// loudly, as before.
        #[serde(default, skip_serializing_if = "is_false")]
        mask: bool,
        /// Allow `expand` plus its concretization store - the load-adjacent
        /// store of the expanded vector back into the very cell the value
        /// was loaded from. Both run unmasked: the store writes the
        /// exhaustive lane-split of exactly what the cell holds, a per-lane
        /// refinement whether or not the lane took the region. Opt-in
        /// because lanes that skipped the region still double.
        #[serde(default, skip_serializing_if = "is_false")]
        expand: bool,
        /// Allow calls to `fixed_env::REFINEMENT_BUILTINS` (`__split_by_flr`,
        /// `__split_at`) in the region, the callee proven by its
        /// `load(get_global(..))` def chain. A refinement's fragments
        /// jointly represent exactly the input state, so running one on
        /// lanes that skipped the region is sound - the cost is
        /// fragmentation, priced by the screen. Opt-in so call-free entries
        /// keep refusing calls loudly.
        #[serde(default, skip_serializing_if = "is_false")]
        splits: bool,
        /// Mask the region's `assert_true`s on the head's condition
        /// (`assert_true(select(c, cond, true))`): a straightened arm's
        /// trace guards hold only on lanes that entered it, and unmasked
        /// they would spuriously deopt every lane that skipped. Opt-in
        /// because default behavior (asserts accepted unmasked) is what
        /// existing entries replay.
        #[serde(default, skip_serializing_if = "is_false")]
        guards: bool,
    },
    /// Give a loop with a per-lane trip count a uniform constant trip count:
    /// the head branches on a fresh counter against `limit` (per-state
    /// uniform, so it stops splitting), per-lane progress becomes mask data,
    /// every latch/break store is masked, and the break edge is deleted. A
    /// loud `assert_true(bound <= limit)` guard covers the claim that
    /// `limit` iterations are enough. Screen at full depth.
    MaskLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The loop header: exactly a counter phi, `counter <= bound`, and
        /// the conditional branch on it.
        head: String,
        /// The uniform trip count. Iterations run for counters 0..=limit;
        /// a state whose per-lane bound exceeds it fails loudly. Exactly one
        /// of `limit` and `span` must be given.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        limit: Option<i16>,
        /// Like `limit`, but for a loop whose *init* is per-lane too: the
        /// loop runs `span + 1` uniform iterations, guarded by
        /// `assert_true(init >= 0)` and `assert_true(bound - init < span + 1)`
        /// (strict - a Lua `for` bound may be fractional).
        #[serde(default, skip_serializing_if = "Option::is_none")]
        span: Option<i16>,
        /// Opt-in: the loop's break edge leads to this external join instead
        /// of the exit (a multi-level `break`, as in the inlined
        /// `spikes_at`). The break value - a bool constant in the join's
        /// phis - is re-routed through the exit: as a conditional branch on
        /// the loop's `active` flag, or merged by select when the exit
        /// already falls through to the join.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        break_to: Option<String>,
    },
    /// Merge two consecutive early-exit branches into one: the second test
    /// block's instructions go eager into the head, the conditions combine
    /// through an `or` select, and the first break block goes unreachable
    /// for a later `dce` to sweep. Both break blocks must feed the join's
    /// phis the same bool constant, so the switch of edges is invisible.
    FuseBreaks {
        #[serde(rename = "fn")]
        function: String,
        /// The block holding the earlier of the two early-exit branches.
        head: String,
    },
    /// Move a triangle arm's trailing store past the join: a guarded load
    /// before the branch, a phi at the join, the store after it. The
    /// `assert_value_cell` guard makes the one non-identity case - a closure
    /// or table cell - fail loudly. Screen at full depth.
    SinkStore {
        #[serde(rename = "fn")]
        function: String,
        /// The store, as `%N`.
        at: String,
    },
    /// Replace one non-escaping, single-store heap cell with SSA values.
    PromoteCell {
        #[serde(rename = "fn")]
        function: String,
        /// The `alloc` that defines the cell, as `%N`.
        cell: String,
    },
    /// Branch on an `and`'s condition instead of its mixed value:
    /// `%p = select %c ? %k : %c; br %p` becomes `br %c`, with the true
    /// target's phis taking `%k` on that edge and the select deleted.
    /// Requires `%k` statically truthy. The branch-side counterpart of
    /// `decompose_truthy`, for short-circuits whose skipped path has side
    /// effects and so must remain a branch.
    DecomposeBranch {
        #[serde(rename = "fn")]
        function: String,
        /// The mixed select, as `%N`.
        at: String,
    },
    /// Replace a bool-concretization diamond (`btn`'s tail: branch on an
    /// unknown bool, each arm storing its constant back into the cell) with
    /// an `expand` instruction: lanes duplicate instead of the state
    /// splitting, and the per-lane bool is stored back through the arms'
    /// accessor. The expand fails loudly if the branched-on value is ever
    /// not a bool. Screen at full depth.
    ExpandBool {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is removed.
        head: String,
    },
    /// Replace a conditional branch with a premise guard (`assert_true` on
    /// the condition, behind a `not` when the recorded edge is the false
    /// one) and an unconditional jump to the recorded side. The premise is
    /// "every lane takes the recorded edge"; a lane that falsifies it
    /// deopts to the plain program via the standard `assert_true` capture.
    /// The workhorse of trace straightening: derived mechanically from the
    /// branch census, screened at full depth for deopt counts.
    GuardBranch {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is replaced (`__entry` names
        /// the entry block).
        head: String,
        /// The recorded direction: `true` = every lane takes the true edge.
        taken: bool,
    },
    /// Replace an inlined `__assert` failure diamond (`if not cond then
    /// __print(...) ... error(...) end`) with a single `assert_true` on the
    /// condition. Every path through the diamond aborts the run, exactly
    /// like a failing assert, so only the branch disappears - a uniform,
    /// never-taken branch that still blocked speculation.
    ConvertAssert {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch into the diamond is removed.
        head: String,
    },
    /// State that two locals hold the same value - `%g = b == a;
    /// assert_true %g` right after `b`'s definition - and replace every
    /// other use of `b` with `a`. For equalities no local analysis can see:
    /// the object read back out of the singleton table is the object being
    /// updated. The premise is checked on every lane of every execution and
    /// fails loudly. Screen at full depth.
    AssumeEq {
        #[serde(rename = "fn")]
        function: String,
        /// The local that survives, as `%N`. Must dominate `b`.
        a: String,
        /// The local that is replaced, as `%N`.
        b: String,
    },
    /// Collapse a counted loop to one guarded execution of its body: a loud
    /// `assert_true(bound == init)` in the preheader states that the trip
    /// count is exactly one, the counter is replaced by its initial value,
    /// and the head disappears - the preheader falls into the body, the
    /// latch falls out to the exit. For the loops bounded by `#objects`,
    /// which room (1, 0) keeps at exactly one object for the whole search.
    /// Screen at full depth.
    CollapseLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The loop header: exactly a counter phi, `counter <= bound`, and
        /// the conditional branch on it.
        head: String,
    },
    /// Duplicate a dynamic call under a two-way case split on
    /// `on == load(get_global(global))`, so each copy can be pinned by its
    /// own `assert_closure` and inlined. Semantically neutral by itself:
    /// both arms perform the original call, and the discriminating branch
    /// is a pure per-state-uniform pointer compare. The premises live in
    /// the `inline` entries that follow, one per arm.
    SplitCall {
        #[serde(rename = "fn")]
        function: String,
        /// The `call` instruction, as `%N`.
        at: String,
        /// The discriminant local, as `%N` - e.g. the object's type
        /// pointer.
        on: String,
        /// The global whose value the discriminant is compared against.
        global: String,
    },
    /// Collapse a sentinel-bounded loop whose real exit is an in-body
    /// `#tbl < i` break - the inlined `foreach`/`del` shape - guarded by
    /// runtime asserts that iteration 1 does not break and iteration 2
    /// does (the break check is re-materialized after the payload, reading
    /// `# t` again so mid-payload table mutations count). The head and the
    /// break block disappear. Screen at full depth.
    CollapseBreakLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The sentinel loop header.
        head: String,
    },
    /// Collapse the `all()` iterator's sentinel loop (the post-#112 foreach
    /// shape: advance diamond + nil check through cells, no length read),
    /// guarded by runtime asserts that iteration 1 does not break and
    /// iteration 2 does. Iteration 2's check chain is re-materialized as a
    /// fresh copy after the payload, re-reading the cells on the real heap.
    /// The head and the break block disappear. Screen at full depth.
    CollapseAllLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The sentinel loop header.
        head: String,
        /// Maximum table size the collapse claims: that many payload peels
        /// (each keeping its REAL nil-check branch, so smaller tables break
        /// early through the shared trampoline) and an asserted break at
        /// iteration `trip`+1. Absent means 2, the rule's historical shape -
        /// existing recipes replay byte-identically.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trip: Option<usize>,
    },
    /// Replace a counted loop whose trip count is statically known (constant
    /// init, step and bound on the branch-deciding counter - the shape
    /// `mask_loop` leaves behind) with that many renamed copies of its body,
    /// laid out in a straight line. Purely a renaming plus an independently
    /// re-simulated trip count; no runtime guard is needed.
    UnrollLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The loop header.
        head: String,
        /// Guard mode: the bound is DYNAMIC (e.g. `#objects`); unroll
        /// exactly this many iterations and assert the premise
        /// `bound == init + (trip-1)*step` in the first head copy. The
        /// collapse_loop doctrine at N > 1: a lane whose bound differs
        /// fails the guard loudly and deopts. Omitted = the original
        /// constant-bound mode, no runtime guard.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        trip: Option<usize>,
        /// Early-exit arms: single-predecessor, phi-free blocks a chain
        /// block may conditionally branch to, leaving the loop (the
        /// `check`-loop "found" exits). Copied per iteration; outside
        /// join phis gain one edge per copy. Naming them in the recipe is
        /// what makes the chain walk deterministic.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        exits: Vec<String>,
        /// Inverted head sense: the loop CONTINUES on the false target and
        /// exits on the true target (`br i > bound ? exit : body`, the
        /// inlined-`del` scan shape). The continue-condition is the negated
        /// compare, so trip mode's `<=`-equality premise argument applies
        /// after negation exactly as in the plain sense.
        #[serde(default, skip_serializing_if = "is_false")]
        invert: bool,
    },
    /// Delete any `assert_true`/`assert_pointer`/`assert_closure` that is
    /// dominated by an identical assert on the same SSA operands. Those
    /// asserts are deterministic functions of immutable values, so the
    /// dominated copy can never be the first to fire. `assert_value_cell`
    /// reads a heap cell and is excluded.
    DedupGuards,
    /// Designate the merge-partition cells (field-path patterns).
    #[serde(rename = "partition_merge")]
    PartitionMerge { cells: Vec<String> },
}

impl Rule {
    pub fn name(&self) -> &'static str {
        match self {
            Rule::Dce => "dce",
            Rule::Cse { .. } => "cse",
            Rule::AllocateSlots => "allocate_slots",
            Rule::KillDead => "kill_dead",
            Rule::StripKills => "strip_kills",
            Rule::MergeBlocks => "merge_blocks",
            Rule::Fold => "fold",
            Rule::FoldReflexive { .. } => "fold_reflexive",
            Rule::FoldSelect => "fold_select",
            Rule::Inline { .. } => "inline",
            Rule::IfConvert { .. } => "if_convert",
            Rule::PromoteCapture { .. } => "promote_capture",
            Rule::PinBuiltin { .. } => "pin_builtin",
            Rule::SplitAt { .. } => "split_at",
            Rule::DemoteCreate { .. } => "demote_create",
            Rule::DropDeadCell { .. } => "drop_dead_cell",
            Rule::ConvertTernary { .. } => "convert_ternary",
            Rule::DecomposeTruthy { .. } => "decompose_truthy",
            Rule::Speculate { .. } => "speculate",
            Rule::SpeculateRegion { .. } => "speculate_region",
            Rule::MaskLoop { .. } => "mask_loop",
            Rule::FuseBreaks { .. } => "fuse_breaks",
            Rule::SinkStore { .. } => "sink_store",
            Rule::AbsorbStores { .. } => "absorb_stores",
            Rule::PromoteCell { .. } => "promote_cell",
            Rule::DecomposeBranch { .. } => "decompose_branch",
            Rule::ExpandBool { .. } => "expand_bool",
            Rule::GuardBranch { .. } => "guard_branch",
            Rule::ConvertAssert { .. } => "convert_assert",
            Rule::CollapseLoop { .. } => "collapse_loop",
            Rule::CollapseBreakLoop { .. } => "collapse_break_loop",
            Rule::CollapseAllLoop { .. } => "collapse_all_loop",
            Rule::UnrollLoop { .. } => "unroll_loop",
            Rule::DedupGuards => "dedup_guards",
            Rule::PartitionMerge { .. } => "partition_merge",
            Rule::AssumeEq { .. } => "assume_eq",
            Rule::SplitCall { .. } => "split_call",
        }
    }

    /// Every field of this entry that names a LOCAL, with the function it is
    /// resolved against - the exact set `resolve_cell` is called on.
    ///
    /// One place, so that migrating the recipe from `%N` to stable names is a
    /// walk rather than thirty-five hand-written cases that can silently omit
    /// one. `PartitionMerge`'s `cells` are field-path patterns, not locals,
    /// and are deliberately absent.
    ///
    /// Returns the function name by value: the borrow checker will not let a
    /// shared read of `function` coexist with the mutable field borrows, and
    /// cloning one short string per entry is not worth a lifetime dance.
    pub fn cell_fields_mut(&mut self) -> Option<(String, Vec<&mut String>)> {
        let (function, fields): (&mut String, Vec<&mut String>) = match self {
            Rule::Inline { function, at, captures, .. } => {
                let mut fields = vec![at];
                fields.extend(captures.iter_mut());
                (function, fields)
            }
            Rule::SplitAt { function, at, .. }
            | Rule::PinBuiltin { function, at, .. }
            | Rule::DemoteCreate { function, at }
            | Rule::SinkStore { function, at }
            | Rule::DecomposeBranch { function, at } => (function, vec![at]),
            Rule::DecomposeTruthy { function, root } => (function, vec![root]),
            Rule::PromoteCell { function, cell } => (function, vec![cell]),
            Rule::DropDeadCell { function, cell } => (function, vec![cell]),
            Rule::AssumeEq { function, a, b } => (function, vec![a, b]),
            Rule::SplitCall { function, at, on, .. } => (function, vec![at, on]),
            Rule::Speculate { function, guards, .. } => {
                let mut fields = Vec::new();
                for guard in guards.iter_mut() {
                    fields.push(&mut guard.load);
                    fields.push(&mut guard.store);
                }
                (function, fields)
            }
            _ => return None,
        };
        Some((function.clone(), fields))
    }
}

/// One declared crossing for `Rule::Speculate`'s `guards` field.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpeculateGuard {
    /// The hoisted cell-reading instruction, as `%N`.
    pub load: String,
    /// The crossed store's target local, as `%N`.
    pub store: String,
}

/// serde helper: keep serialized entries free of `"mask":false` noise.
fn is_false(value: &bool) -> bool {
    !*value
}

/// Every function's name bindings, taken BEFORE the rule mutates anything.
///
/// A snapshot rather than a live borrow because the rule needs `&mut
/// Program` and resolving against it at the call site would conflict. It
/// also happens to be the right semantics: an entry addresses the program
/// AS IT FOUND IT, so a name the entry itself creates cannot be referred to
/// by that same entry.
pub type NameSnapshot = rustc_hash::FxHashMap<String, rustc_hash::FxHashMap<String, LocalId>>;






#[derive(Debug, Default)]
pub struct Recipe {
    pub entries: Vec<RewriteEntry>,
}

impl Recipe {
    pub fn parse(text: &str) -> Result<Self> {
        let mut entries = Vec::new();
        let mut seen = std::collections::HashSet::new();
        for (n, line) in text.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with("//") {
                continue;
            }
            let entry: RewriteEntry = serde_json::from_str(line)
                .with_context(|| format!("recipe line {}", n + 1))?;
            if !seen.insert(entry.id.clone()) {
                return Err(anyhow!(
                    "recipe line {}: duplicate id {:?}. Ids must be unique - \
                     generated names are derived from them.",
                    n + 1,
                    entry.id
                ));
            }
            entries.push(entry);
        }
        Ok(Self { entries })
    }

    pub fn load(path: &str) -> Result<Self> {
        if !std::path::Path::new(path).exists() {
            return Ok(Self::default());
        }
        Self::parse(&std::fs::read_to_string(path)?)
    }

    pub fn to_text(&self) -> Result<String> {
        let mut out = String::new();
        for entry in &self.entries {
            out.push_str(&serde_json::to_string(entry)?);
            out.push('\n');
        }
        Ok(out)
    }
}

pub struct StepReport {
    pub id: String,
    pub rule: &'static str,
    pub changes: usize,
    pub instructions_before: usize,
    pub instructions_after: usize,
    pub blocks_before: usize,
    pub blocks_after: usize,
    /// Where the time went. Replaying the recipe is the prefix of every command,
    /// so it is the number that decides how fast this project is to work on -
    /// it is worth being able to see it broken down rather than guessed at.
    pub timing: StepTiming,
}

#[derive(Default, Clone, Copy)]
pub struct StepTiming {
    pub clone: std::time::Duration,
    pub apply: std::time::Duration,
    pub validate: std::time::Duration,
    pub verify: std::time::Duration,
}

impl StepTiming {
    pub fn add(&mut self, other: &StepTiming) {
        self.clone += other.clone;
        self.apply += other.apply;
        self.validate += other.validate;
        self.verify += other.verify;
    }

    pub fn total(&self) -> std::time::Duration {
        self.clone + self.apply + self.validate + self.verify
    }
}
