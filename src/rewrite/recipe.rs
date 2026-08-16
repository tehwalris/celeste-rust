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

use super::program::Program;
use super::rules::{
    absorb_stores, add_hint, allocate_slots, assume_eq, collapse_break_loop, collapse_loop,
    kill_dead,
    convert_assert, convert_ternary,
    cse, dce, decompose_branch, dedup_guards,
    decompose_truthy, demote_create,
    expand_bool, fold, fold_reflexive, fold_select, fuse_breaks, if_convert, inline,
    mask_loop, merge_blocks,
    pin_builtin,
    promote_capture,
    promote_cell, sink_store, speculate, speculate_region, split_call, unroll_loop, partition_merge, remove_hint, widen_buttons, widen_rem,
};
use crate::ir::LocalId;
use super::validate::{validate_function, validate_program};

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
    },
    /// Repack locals so that values with disjoint live ranges share a slot.
    /// Changes no instructions - only where they are stored.
    AllocateSlots,
    KillDead,
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
    },
    /// Delete any `assert_true`/`assert_pointer`/`assert_closure` that is
    /// dominated by an identical assert on the same SSA operands. Those
    /// asserts are deterministic functions of immutable values, so the
    /// dominated copy can never be the first to fire. `assert_value_cell`
    /// reads a heap cell and is excluded.
    DedupGuards,
    /// Mark a block as an early normalize point: states arriving there are
    /// accumulated, vectorized by shape, and row-deduped before execution
    /// continues - the same merge the frame boundary performs, scheduled
    /// earlier. Reduces fragments, not lanes; see the rule's docs.
    AddHint {
        #[serde(rename = "fn")]
        function: String,
        /// The block to flag.
        block: String,
    },
    /// Designate the merge-partition cells (field-path patterns).
    #[serde(rename = "partition_merge")]
    PartitionMerge { cells: Vec<String> },
    /// Unmark a block as an early normalize point (inverse of `add_hint`).
    #[serde(rename = "remove_hint")]
    RemoveHint {
        #[serde(rename = "fn")]
        function: String,
        /// The block to unflag.
        block: String,
    },
    /// Insert the in-place equivalent of `__reset_button_states()` at the
    /// head of a block: store a fresh unknown boolean into each of the six
    /// `__button_states` cells. Sound only if every `btn` read of the frame
    /// precedes the block - claimed, not proven; a violated claim diverges
    /// from the original program and the differential screen catches it.
    WidenButtons {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose head widens the cells.
        block: String,
    },
    /// Insert `make_state_abstract`'s rem widening at the head of a block:
    /// `player.rem.x/.y` each pass through the `__widen_rem` builtin (assert
    /// containment in [-0.5, 0.5), return the whole interval) and store
    /// back. Sound only where rem is dead (inside the update body - `move`
    /// runs before `type.update` and is rem's only reader). Claimed, not
    /// proven; screened.
    WidenRem {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose head widens rem.
        block: String,
        /// The player instance (the update body's `this`), as `%N`.
        object: String,
    },
}

impl Rule {
    pub fn name(&self) -> &'static str {
        match self {
            Rule::Dce => "dce",
            Rule::Cse { .. } => "cse",
            Rule::AllocateSlots => "allocate_slots",
            Rule::KillDead => "kill_dead",
            Rule::MergeBlocks => "merge_blocks",
            Rule::Fold => "fold",
            Rule::FoldReflexive { .. } => "fold_reflexive",
            Rule::FoldSelect => "fold_select",
            Rule::Inline { .. } => "inline",
            Rule::IfConvert { .. } => "if_convert",
            Rule::PromoteCapture { .. } => "promote_capture",
            Rule::PinBuiltin { .. } => "pin_builtin",
            Rule::DemoteCreate { .. } => "demote_create",
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
            Rule::ConvertAssert { .. } => "convert_assert",
            Rule::CollapseLoop { .. } => "collapse_loop",
            Rule::CollapseBreakLoop { .. } => "collapse_break_loop",
            Rule::UnrollLoop { .. } => "unroll_loop",
            Rule::DedupGuards => "dedup_guards",
            Rule::AddHint { .. } => "add_hint",
            Rule::RemoveHint { .. } => "remove_hint",
            Rule::PartitionMerge { .. } => "partition_merge",
            Rule::WidenButtons { .. } => "widen_buttons",
            Rule::WidenRem { .. } => "widen_rem",
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
            Rule::PinBuiltin { function, at, .. }
            | Rule::DemoteCreate { function, at }
            | Rule::SinkStore { function, at }
            | Rule::DecomposeBranch { function, at } => (function, vec![at]),
            Rule::DecomposeTruthy { function, root } => (function, vec![root]),
            Rule::PromoteCell { function, cell } => (function, vec![cell]),
            Rule::WidenRem { function, object, .. } => (function, vec![object]),
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

fn take_name_snapshot(program: &Program) -> NameSnapshot {
    program
        .functions
        .iter()
        .map(|(g, f)| {
            (
                g.as_str().to_string(),
                f.cfg.names.iter().map(|(id, n)| (n.to_string(), id)).collect(),
            )
        })
        .collect()
}

/// Resolve a recipe's cell reference: either `%17` (a source local, whose
/// numbering is already stable) or `%some.stable.name` (anything a rewrite
/// created). Both spellings stay valid so the recipe can migrate one entry
/// at a time - see plans/recipe-stability-plan.md.
fn resolve_cell(names: &NameSnapshot, function: &str, text: &str) -> Result<LocalId> {
    if let Some(id) = super::print::parse_local_name(text) {
        return Ok(id);
    }
    let bare = text.strip_prefix('%').unwrap_or(text);
    names
        .get(function)
        .and_then(|m| m.get(bare))
        .copied()
        .ok_or_else(|| {
            anyhow!(
                "cell {:?} is neither a %N local nor a name bound in {} - a \
                 name that no longer resolves means the entry that created \
                 it did not run, or ran differently",
                text,
                function
            )
        })
}

/// `resolve_cell` against one function's live name table rather than a
/// whole-program snapshot. Used by the migration, which holds the program.
fn resolve_cell_in(names: &crate::ir::Names, function: &str, text: &str) -> Result<LocalId> {
    if let Some(id) = super::print::parse_local_name(text) {
        return Ok(id);
    }
    let bare = text.strip_prefix('%').unwrap_or(text);
    names.lookup(bare).ok_or_else(|| {
        anyhow!("cell {:?} is neither a %N local nor a name bound in {}", text, function)
    })
}

fn cells(names: &NameSnapshot, function: &str, texts: &[String]) -> Result<Vec<LocalId>> {
    texts.iter().map(|t| resolve_cell(names, function, t)).collect()
}

fn guards_in(
    names: &NameSnapshot,
    function: &str,
    guards: &[SpeculateGuard],
) -> Result<Vec<(LocalId, LocalId)>> {
    guards
        .iter()
        .map(|g| Ok((resolve_cell(names, function, &g.load)?, resolve_cell(names, function, &g.store)?)))
        .collect()
}

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

/// Applies one entry, then validates and verifies it.
///
/// Verification is per-rule and deliberately written independently of the
/// applier. Structural validation (including dominance) runs on top, because it
/// catches whole classes of mistake that no individual rule verifier would
/// think to look for.
pub fn apply_entry(program: &mut Program, entry: &RewriteEntry) -> Result<StepReport> {
    let mut timing = StepTiming::default();
    let instructions_before = program.instruction_count();
    let blocks_before = program.block_count();

    // Every rule but `allocate_slots` runs under the identity slot map.
    //
    // A slot map is built for a particular set of instructions and says nothing
    // about instructions minted later, so any rule that adds a `LocalId` after
    // `allocate_slots` leaves the map stale. `validate` catches that - which is
    // how it was found, when `screen` appended candidates to a recipe whose last
    // entry was `allocate_slots` and 36 of 36 died with structural errors - but
    // failing loudly on something avoidable is worse than not creating it.
    // Resetting here makes "the map matches the program" an invariant of the
    // recipe rather than an ordering rule an author has to remember.
    //
    // This happens *before* `before` is taken, so that `before` is the program
    // the rule actually saw. Taking the snapshot first was a real bug: a rule
    // whose verifier checks that it changed nothing else - `pin_builtin` and
    // `demote_create` both do - then sees every untouched function differ,
    // because `FunDef` carries the slot map and the reset had just changed all
    // of them. It only showed up under `screen`, where the base program has been
    // through `allocate_slots`; in a recipe these rules run before it, where the
    // reset is a no-op.
    if !matches!(entry.rule, Rule::AllocateSlots) {
        for fun in program.functions.values_mut() {
            fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
        }
    }

    let names = take_name_snapshot(program);

    let clock = std::time::Instant::now();
    let before = program.clone();
    timing.clone = clock.elapsed();
    let clock = std::time::Instant::now();

    let changes = match &entry.rule {
        Rule::Dce => dce::apply(program),
        Rule::Cse { forward } => cse::apply(program, *forward),
        Rule::AllocateSlots => allocate_slots::apply(program),
        Rule::KillDead => kill_dead::apply(program),
        Rule::MergeBlocks => merge_blocks::apply(program),
        Rule::Fold => fold::apply(program),
        Rule::FoldReflexive { pointers } => fold_reflexive::apply(program, *pointers),
        Rule::FoldSelect => fold_select::apply(program),
        Rule::IfConvert { function, join } => if_convert::apply(program, function, join),
        Rule::PromoteCapture { function, index } => {
            promote_capture::apply(program, function, *index)
        }
        Rule::PinBuiltin { function, at, name } => {
            pin_builtin::apply(program, function, resolve_cell(&names, function, at)?, name)
        }
        Rule::DemoteCreate { function, at } => {
            demote_create::apply(program, function, resolve_cell(&names, function, at)?)
        }
        Rule::ConvertTernary { function, join } => {
            convert_ternary::apply(program, function, join)
        }
        Rule::DecomposeTruthy { function, root } => {
            decompose_truthy::apply(program, function, resolve_cell(&names, function, root)?)
        }
        Rule::Speculate { function, join, arm, guards } => {
            speculate::apply(program, function, join, arm.as_deref(), &guards_in(&names, function, guards)?)
        }
        Rule::SpeculateRegion { function, head, arm, join, mask, expand } => {
            speculate_region::apply(program, function, head, arm, join.as_deref(), *mask, *expand)
        }
        Rule::MaskLoop { function, head, limit, span, break_to } => {
            mask_loop::apply(program, function, head, *limit, *span, break_to.as_deref())
        }
        Rule::FuseBreaks { function, head } => fuse_breaks::apply(program, function, head),
        Rule::AbsorbStores { function, head } => absorb_stores::apply(program, function, head),
        Rule::SinkStore { function, at } => {
            sink_store::apply(program, function, resolve_cell(&names, function, at)?)
        }
        Rule::PromoteCell { function, cell } => {
            promote_cell::apply(program, function, resolve_cell(&names, function, cell)?)
        }
        Rule::DecomposeBranch { function, at } => {
            decompose_branch::apply(program, function, resolve_cell(&names, function, at)?)
        }
        Rule::ExpandBool { function, head } => expand_bool::apply(program, function, head),
        Rule::ConvertAssert { function, head } => convert_assert::apply(program, function, head),
        Rule::CollapseLoop { function, head } => collapse_loop::apply(program, function, head),
        Rule::CollapseBreakLoop { function, head } => {
            collapse_break_loop::apply(program, function, head)
        }
        Rule::UnrollLoop { function, head } => unroll_loop::apply(program, function, head),
        Rule::DedupGuards => dedup_guards::apply(program),
        Rule::AddHint { function, block } => add_hint::apply(program, function, block),
        Rule::RemoveHint { function, block } => remove_hint::apply(program, function, block),
        Rule::PartitionMerge { cells } => partition_merge::apply(program, cells),
        Rule::WidenButtons { function, block } => widen_buttons::apply(program, function, block),
        Rule::WidenRem { function, block, object } => {
            widen_rem::apply(program, function, block, resolve_cell(&names, function, object)?)
        }
        Rule::AssumeEq { function, a, b } => {
            assume_eq::apply(program, function, resolve_cell(&names, function, a)?, resolve_cell(&names, function, b)?)
        }
        Rule::SplitCall { function, at, on, global } => {
            split_call::apply(program, function, resolve_cell(&names, function, at)?, resolve_cell(&names, function, on)?, global)
        }
        Rule::Inline { function, at, callee, captures } => inline::apply(
            program,
            &entry.id,
            function,
            resolve_cell(&names, function, at)?,
            callee,
            &cells(&names, function, captures)?,
        ),
    }
    .with_context(|| format!("applying {} ({})", entry.id, entry.rule.name()))?;
    timing.apply = clock.elapsed();

    // Validate the functions this entry actually changed, found by comparing
    // against the copy we already had to take.
    //
    // Whole-program validation after every entry was 97% of recipe replay once
    // inlining had grown the program: dominance is superlinear in block count
    // and `player.update_21` is now hundreds of blocks, so 554 entries x 77
    // functions dominated everything. Validating a function that did not change
    // cannot find anything - `validate_function` reads only that function - and
    // the comparison is exact rather than a guess about what each rule touches,
    // so nothing is taken on trust.
    // Before validating, not after: `validate` rejects a name whose local no
    // longer exists, and a rule that deleted instructions has just created
    // exactly that. Running here also means every later stage - the rule's
    // own verifier, the report - sees the names the entry actually
    // established.
    name_new_locals(&before, program, &entry.id)?;

    let clock = std::time::Instant::now();
    let mut errors = Vec::new();
    for (name, after_fun) in program.functions.iter() {
        if before.functions.get(name) == Some(after_fun) {
            continue;
        }
        errors.extend(validate_function(after_fun));
    }
    timing.validate = clock.elapsed();
    if !errors.is_empty() {
        let shown: Vec<String> = errors.iter().take(10).map(|e| e.to_string()).collect();
        return Err(anyhow!(
            "{} ({}) produced {} structural error(s):\n  {}",
            entry.id,
            entry.rule.name(),
            errors.len(),
            shown.join("\n  ")
        ));
    }

    let clock = std::time::Instant::now();
    match &entry.rule {
        Rule::Dce => dce::verify(&before, program),
        Rule::Cse { forward } => cse::verify(&before, program, *forward),
        Rule::AllocateSlots => allocate_slots::verify(&before, program),
        Rule::KillDead => kill_dead::verify(&before, program),
        Rule::MergeBlocks => merge_blocks::verify(&before, program),
        Rule::Fold => fold::verify(&before, program),
        Rule::FoldReflexive { pointers } => {
            fold_reflexive::verify(&before, program, *pointers)
        }
        Rule::FoldSelect => fold_select::verify(&before, program),
        Rule::IfConvert { function, join } => {
            if_convert::verify(&before, program, function, join)
        }
        Rule::PromoteCapture { function, index } => {
            promote_capture::verify(&before, program, function, *index)
        }
        Rule::PinBuiltin { function, at, name } => {
            pin_builtin::verify(&before, program, function, resolve_cell(&names, function, at)?, name)
        }
        Rule::DemoteCreate { function, at } => {
            demote_create::verify(&before, program, function, resolve_cell(&names, function, at)?)
        }
        Rule::ConvertTernary { function, join } => {
            convert_ternary::verify(&before, program, function, join)
        }
        Rule::DecomposeTruthy { function, root } => {
            decompose_truthy::verify(&before, program, function, resolve_cell(&names, function, root)?)
        }
        Rule::Speculate { function, join, arm, guards } => {
            speculate::verify(&before, program, function, join, arm.as_deref(), &guards_in(&names, function, guards)?)
        }
        Rule::SpeculateRegion { function, head, arm, join, mask, expand } => {
            speculate_region::verify(
                &before,
                program,
                function,
                head,
                arm,
                join.as_deref(),
                *mask,
                *expand,
            )
        }
        Rule::MaskLoop { function, head, limit, span, break_to } => {
            mask_loop::verify(&before, program, function, head, *limit, *span, break_to.as_deref())
        }
        Rule::FuseBreaks { function, head } => {
            fuse_breaks::verify(&before, program, function, head)
        }
        Rule::AbsorbStores { function, head } => {
            absorb_stores::verify(&before, program, function, head)
        }
        Rule::SinkStore { function, at } => {
            sink_store::verify(&before, program, function, resolve_cell(&names, function, at)?)
        }
        Rule::PromoteCell { function, cell } => {
            promote_cell::verify(&before, program, function, resolve_cell(&names, function, cell)?)
        }
        Rule::DecomposeBranch { function, at } => {
            decompose_branch::verify(&before, program, function, resolve_cell(&names, function, at)?)
        }
        Rule::ExpandBool { function, head } => {
            expand_bool::verify(&before, program, function, head)
        }
        Rule::ConvertAssert { function, head } => {
            convert_assert::verify(&before, program, function, head)
        }
        Rule::CollapseLoop { function, head } => {
            collapse_loop::verify(&before, program, function, head)
        }
        Rule::CollapseBreakLoop { function, head } => {
            collapse_break_loop::verify(&before, program, function, head)
        }
        Rule::UnrollLoop { function, head } => {
            unroll_loop::verify(&before, program, function, head)
        }
        Rule::DedupGuards => dedup_guards::verify(&before, program),
        Rule::AddHint { function, block } => add_hint::verify(&before, program, function, block),
        Rule::RemoveHint { function, block } => {
            remove_hint::verify(&before, program, function, block)
        }
        Rule::PartitionMerge { cells } => partition_merge::verify(&before, program, cells),
        Rule::WidenButtons { function, block } => {
            widen_buttons::verify(&before, program, function, block)
        }
        Rule::WidenRem { function, block, object } => {
            widen_rem::verify(&before, program, function, block, resolve_cell(&names, function, object)?)
        }
        Rule::AssumeEq { function, a, b } => {
            assume_eq::verify(&before, program, function, resolve_cell(&names, function, a)?, resolve_cell(&names, function, b)?)
        }
        Rule::SplitCall { function, at, on, global } => {
            split_call::verify(&before, program, function, resolve_cell(&names, function, at)?, resolve_cell(&names, function, on)?, global)
        }
        Rule::Inline { function, at, callee, captures } => inline::verify(
            &before,
            program,
            &entry.id,
            function,
            resolve_cell(&names, function, at)?,
            callee,
            &cells(&names, function, captures)?,
        ),
    }
    .with_context(|| format!("verifying {} ({})", entry.id, entry.rule.name()))?;
    timing.verify = clock.elapsed();

    Ok(StepReport {
        id: entry.id.clone(),
        rule: entry.rule.name(),
        timing,
        changes,
        instructions_before,
        instructions_after: program.instruction_count(),
        blocks_before,
        blocks_after: program.block_count(),
    })
}


/// Give every local this entry just created a name derived from the ENTRY
/// that created it, so a later recipe entry can address it stably.
///
/// Done here, once, rather than in each of the seventeen rule files that
/// mint ids: `apply_entry` is the only place that holds both the before and
/// after programs, so "which locals are new" is a subtraction rather than
/// seventeen rules each remembering to report. No rule signature changes,
/// and a rule added later is covered without being told to be.
///
/// The order is CANONICAL - by block label, then by position within the
/// block - not the order the ids happen to number. That is the whole point:
/// if it were id order, the names would inherit exactly the instability the
/// ids have, and an unrelated edit upstream would renumber them.
///
/// Source locals are deliberately left unnamed. They are already stable
/// (`LocalIdGenerator` numbers per function at compile time, so editing one
/// function cannot move another's), `%n` keeps addressing them, and naming
/// 20,000 instructions that do not need it would be noise.
fn name_new_locals(before: &Program, after: &mut Program, entry_id: &str) -> Result<()> {
    for (global, fun) in after.functions.iter_mut() {
        // PRUNE FIRST. `Cfg::map_blocks` carries names through a rewrite, so
        // a rule that deleted an instruction leaves its name behind; that is
        // deliberate (erasing them all would destroy the addressing) but it
        // makes pruning this function's job. Doing it here rather than in
        // each rule keeps it symmetric with the naming below - one place
        // that knows what the entry did, instead of every rule remembering.
        //
        // Without this, `validate` rejects the program outright, which is
        // how the need showed up rather than a name silently resolving to a
        // deleted local.
        let live: rustc_hash::FxHashSet<LocalId> =
            crate::rewrite::slots::defined_ids(fun).into_iter().collect();
        fun.cfg.names.retain_ids(&|id| live.contains(&id));

        let old_ids: rustc_hash::FxHashSet<LocalId> = match before.functions.get(global) {
            Some(old) => crate::rewrite::slots::defined_ids(old).into_iter().collect(),
            // A function this entry created outright: everything in it is new.
            None => Default::default(),
        };

        // The name is `{entry}.{block}.{position}` - a coordinate in the
        // program, not a counter over the entry's output.
        //
        // A running index over everything the entry created was the first
        // version, and it is not local enough. `inline` splices a whole
        // callee body, so growing the callee - which happens whenever a
        // function inlined INTO it grows - shifts the index of every
        // instruction after the growth, and names that had nothing to do
        // with the change rebind to different instructions. That is exactly
        // the failure mode `%N` had, one level down: `i1_033.133` stopped
        // being a `call` and became a `br`.
        //
        // Keyed by block, a change is confined to the block it happens in,
        // and blocks created by inlining carry the inlining entry's id in
        // their label (`in_i1_033_...`), so the label is itself provenance.
        // A name still moves if an instruction is inserted EARLIER IN ITS
        // OWN BLOCK - there is no free lunch - but that is a change to the
        // very code the name points into.
        //
        // `Cfg.named` is an FxHashMap, so its iteration order must never
        // leak into a name; the labels are sorted.
        let mut fresh: Vec<(LocalId, String)> = Vec::new();
        let collect = |label: &str, block: &crate::ir::Block, fresh: &mut Vec<_>| {
            for (position, (id, _)) in block.instructions.iter().enumerate() {
                if !old_ids.contains(id) {
                    fresh.push((*id, format!("{}.{}.{}", entry_id, label, position)));
                }
            }
            let terminator = block.terminator_id();
            if !old_ids.contains(&terminator) {
                fresh.push((terminator, format!("{}.{}.t", entry_id, label)));
            }
        };
        collect("__entry", &fun.cfg.entry, &mut fresh);
        let mut labels: Vec<&crate::ir::Label> = fun.cfg.named.keys().collect();
        labels.sort();
        for label in labels {
            collect(label.as_str(), &fun.cfg.named[label], &mut fresh);
        }

        for (id, name) in fresh {
            // Already named means this id survived from an earlier entry;
            // leave the first name alone, because a later entry moving an
            // instruction between blocks must not silently re-address it.
            if fun.cfg.names.get(id).is_some() {
                continue;
            }
            fun.cfg
                .names
                .insert(id, name)
                .map_err(|e| anyhow!("naming locals created by {}: {}", entry_id, e))?;
        }
    }
    Ok(())
}

/// What one `migrate_to_names` run changed.
pub struct MigrationReport {
    /// Cell fields rewritten from `%N` to a stable name.
    pub migrated: usize,
    /// Cell fields left as `%N` because the local has no name - i.e. it is a
    /// SOURCE local, which is already stable and deliberately unnamed.
    pub source: usize,
    /// Cell fields that were already names.
    pub already: usize,
}

/// Re-address every recipe cell that points at a rewrite-created local, from
/// `%N` to the stable name of whatever created it.
///
/// Done by replay rather than by editing text: an entry's `%N` only means
/// anything against the program AS THAT ENTRY FINDS IT, so the only way to
/// know which local `%325` is, is to build the program up to that entry. So
/// this walks the recipe exactly as `build` does, and rewrites each entry
/// just before applying it.
///
/// Every rewrite is checked to resolve back to the id it replaced, and the
/// whole thing is gated by `rewrite isocheck`: if migration changed which
/// instruction an entry addressed, the final program would differ.
pub fn migrate_to_names(recipe: &mut Recipe) -> Result<MigrationReport> {
    let mut program = Program::compile_from_disk()?;
    let mut report = MigrationReport { migrated: 0, source: 0, already: 0 };

    for entry in recipe.entries.iter_mut() {
        let id = entry.id.clone();
        if let Some((function, fields)) = entry.rule.cell_fields_mut() {
            let names = &program
                .get(&function)
                .with_context(|| format!("entry {}", id))?
                .cfg
                .names;
            for field in fields {
                // Resolve first, exactly as `resolve_cell` would, then
                // re-emit. Resolving an already-named cell rather than
                // skipping it is what lets this pass REGENERATE the recipe
                // when the naming scheme itself changes - otherwise the
                // first scheme would be permanent.
                let local = resolve_cell_in(names, &function, field)
                    .with_context(|| format!("entry {}", id))?;
                match names.get(local) {
                    Some(name) => {
                        let replacement = format!("%{}", name);
                        // The name must round-trip to the id it replaced.
                        // Cheap, and it is the whole claim of this pass.
                        if names.lookup(name) != Some(local) {
                            return Err(anyhow!(
                                "entry {}: name {:?} does not resolve back to {} in {}",
                                id,
                                name,
                                usize::from(local),
                                function
                            ));
                        }
                        if *field == replacement {
                            report.already += 1;
                        } else {
                            report.migrated += 1;
                        }
                        *field = replacement;
                    }
                    // Unnamed means a source local: `LocalIdGenerator`
                    // numbers those per function at compile time, so an edit
                    // to one function cannot move another's. `%N` is already
                    // the stable spelling for them.
                    None => report.source += 1,
                }
            }
        }
        apply_entry(&mut program, entry)
            .with_context(|| format!("replaying entry {} during name migration", id))?;
    }
    Ok(report)
}

/// Builds the program from source and replays the whole recipe.
pub fn build(recipe: &Recipe) -> Result<(Program, Vec<StepReport>)> {
    let mut program = Program::compile_from_disk()?;

    let errors = validate_program(&program);
    if !errors.is_empty() {
        let shown: Vec<String> = errors.iter().take(10).map(|e| e.to_string()).collect();
        return Err(anyhow!(
            "the freshly compiled program has {} structural error(s), before any rewrites:\n  {}",
            errors.len(),
            shown.join("\n  ")
        ));
    }

    let mut reports = Vec::new();
    for entry in &recipe.entries {
        reports.push(apply_entry(&mut program, entry)?);
    }
    Ok((program, reports))
}

#[cfg(test)]
mod checked_in_recipe_tests {
    /// EVERY checked-in recipe must replay, not just the default one.
    ///
    /// `rewrites-room00.jsonl` sat broken through a commit and a full test
    /// run because nothing exercised it: `isocheck` and the verify tests all
    /// name `rewrites.jsonl` explicitly. The label densification invalidated
    /// room (0,0)'s `%N` addressing exactly as it invalidated the base
    /// recipe's, and only the base recipe had been migrated to stable names.
    ///
    /// Discovering that by hand, one room later, is the expensive way.
    #[test]
    fn every_checked_in_recipe_replays() {
        for path in ["rewrites.jsonl", "rewrites-room00.jsonl"] {
            if !std::path::Path::new(path).exists() {
                // Run from a different working directory; the other tests
                // that need the tree skip the same way.
                continue;
            }
            let recipe = super::Recipe::load(path).unwrap_or_else(|e| panic!("{}: {:#}", path, e));
            super::build(&recipe)
                .unwrap_or_else(|e| panic!("{} does not replay: {:#}", path, e));
        }
    }
}
