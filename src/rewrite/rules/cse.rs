//! `cse` - one instruction per value, across the whole function.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! # Why this exists
//!
//! It is a prerequisite for promoting object fields to SSA values. That rule's
//! core precondition is *"no other instruction in this function can produce a
//! pointer to this cell"* - without it, promoting one accessor leaves the others
//! reading and writing the real cell behind SSA's back, which is exactly how the
//! previous attempt's heap elimination went wrong.
//!
//! The program badly violates that today. `player.update_21` alone contains
//! `get_field %2.hitbox` **124 times**, all denoting one cell, and 416 redundant
//! accessors in total; program-wide there are 1675. Inlining created most of
//! them, since each spliced copy of a callee re-derives the object's fields from
//! scratch.
//!
//! # What counts as the same value
//!
//! Two instructions are interchangeable when they are structurally identical
//! *after* substituting the replacements found so far, so that chains collapse:
//!
//! ```text
//!   %683 = get_field %2.spd     %706 = get_field %2.spd     -> folds to %683
//!   %684 = load %683            %707 = load %706            -> becomes load %683,
//!                                                              folds to %684
//! ```
//!
//! # What makes it safe: barriers
//!
//! Only pure arithmetic is unconditionally repeatable. The heap accessors are
//! repeatable only while nothing has disturbed what they read:
//!
//! * `get_field` / `get_index` / `get_global` return a pointer to a *cell*.
//!   Which cell that is can change if the field did not exist and something
//!   created it, or if the table was reset.
//! * `load` returns the cell's contents, which any store can change.
//!
//! So each kind carries a set of instructions that invalidate it, and the scan
//! forgets the affected entries when one is seen. `Call` invalidates everything,
//! because a callee can do anything. Nothing is assumed about aliasing: a
//! `create_if_missing` accessor for *any* receiver invalidates every accessor,
//! not just ones with a matching receiver.
//!
//! # Why this is a dataflow analysis rather than a scan
//!
//! Within one block, "no barrier between the two" is an interval check. Across
//! blocks it is not: the replacement must have run on *every* path reaching the
//! use, and no barrier may have run since on *any* of them. That is exactly the
//! textbook **available expressions** must-analysis, so that is what this is.
//!
//! The lattice element is a set of `(Key, LocalId)` **pairs**, not of keys. The
//! id matters: knowing only that *some* computation of the key is available is
//! not enough to name a replacement, and picking a dominating one is unsound.
//!
//! ```text
//!   A: %a = load %p          ; dominates D, and the key is available at D...
//!   C: store %p, 5
//!      %c = load %p          ; ...but only because of this one
//!   D: %d = load %p          ; %d = %a would be wrong; %d = %c is right
//! ```
//!
//! Carrying the id through the meet also makes dominance fall out for free: if
//! `(K, %a)` survives the intersection at a block, then `%a` was computed on
//! every path from the entry to it, which is the definition of dominating it.
//! `validate` re-checks that independently afterwards, so a bug here is a loud
//! failure rather than a silently broken program.
//!
//! Because kills are always "forget everything of this kind" - never a
//! kill of one particular entry - a block's whole effect collapses to a
//! `(mask, gen)` pair, and the fixpoint is bitset AND/OR per block.
//!
//! # What is deliberately *not* reused across blocks
//!
//! Pure values - arithmetic and constants - stay block-local, even though they
//! are the safest thing there is to reuse. See `crosses_blocks`: keeping one
//! live across the blocks in between costs more in the interpreter than
//! recomputing it, and the measurement is not close.
//!
//! # The `forward` mode (opt-in per recipe entry)
//!
//! The paragraph above - "nothing is assumed about aliasing" - makes every
//! store a fence for every load, and `player.update_21` has ~150 stores. The
//! `forward` mode replaces the blanket kills with ones the heap model actually
//! justifies, and adds store-to-load forwarding. It is opt-in because old
//! `cse` entries must replay byte-identically; new entries say
//! `"forward": true`.
//!
//! Three facts about the interpreter's heap carry the refinement, all checked
//! against `core_interpreter.rs`:
//!
//! 1. **A field cell belongs to exactly one `(table, name)` pair.** The only
//!    way a cell id enters an `ObjectTable`'s map is `get_field create`, which
//!    always inserts a *freshly allocated* cell. So a cell obtained via
//!    `get_field _.x` can never be the cell behind `get_field _.y`, a
//!    `get_global`, a `get_index` (those live in `ArrayTable`s, which a table
//!    cannot simultaneously be), or an IR-level `alloc`. Builtins hold the
//!    invariant too: `add` allocates a fresh cell for the pushed value,
//!    `del`/`__array_table_drop_last` only remove. A store through a
//!    `get_field`-derived pointer therefore kills only loads of same-name
//!    field cells - plus loads through pointers of *unknown* provenance
//!    (phi/arg/load-derived), which could be anything.
//!
//! 2. **A `create` accessor never changes an existing cell.** It allocates a
//!    missing cell holding nil; contents of every existing cell are untouched,
//!    and a remembered load's source local is immutable SSA. So creates kill
//!    no `Load` keys at all - only same-name accessor keys, whose re-execution
//!    could now find a cell where they previously got a `NilPointer`. The one
//!    cross-kill: a field-create can flip an `UnknownTable` into an
//!    `ObjectTable`, after which a remembered `get_index` on it would *error*
//!    rather than repeat its `NilPointer` - reusing the memory would suppress
//!    that error, so field-creates kill index accessors and vice versa.
//!
//! 3. **`store %c <- %v` makes `load %c` return exactly the value of `%v`.**
//!    (`StoreClosure` does not - loading a closure cell returns the cell's own
//!    pointer - so it kills but never forwards.) Forwarding a later load to
//!    `%v` is then just load-load CSE with the store as the first "load", and
//!    the same must-analysis pair machinery makes it sound.
//!
//! Calls stay kill-everything with one exception: a callee that provably is a
//! *heap-oblivious* builtin - `error` (aborts the run), `__print` (reads its
//! argument values, appends to the print log), `__split_by_flr` (a function of
//! its argument that splits the state) - touches no heap cell and creates no
//! global, so it invalidates nothing. "Provably" has two halves: the callee's
//! def chain is `load (get_global NAME)`, and a whole-program scan shows every
//! `get_global NAME` result is used only by `Load` - so the global can never
//! have been shadowed, structurally rather than by assumption.
//!
//! `StoreEmptyTable` remains a kill-everything barrier: it replaces a table
//! wholesale, and it is rare (fresh-table construction in the smoke arm).
//!
//! **Everything forward mode finds is block-local.** The unrestricted version
//! was built and measured: 631 folds against 270, K (loops kept) 3602 against
//! 3815 - and `player.update_21` went from 33 to 47 live slots, because a
//! load that used to be re-executed near its use became one value kept alive
//! across the whole stretch. The interpreter charges merge, dedup and filter
//! for the entire env per state, so at frame 37 that was ~2% *slower* despite
//! 361 fewer instructions, exactly the `crosses_blocks` trade. Until folding
//! is live-range-aware, the extra reach is a loss in the harness that
//! actually runs.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

use crate::ir::{Block, Cfg, Instruction, LocalId};

use super::super::program::Program;
use super::super::validate::{all_blocks, successors, Dominance};
use super::require;

/// What a surviving instruction is remembered by. Equality of these is equality
/// of the value produced, given that no barrier has intervened.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Key {
    Field { receiver: LocalId, field: String, create: bool },
    Index { receiver: LocalId, index: LocalId, create: bool },
    Global { name: String, create: bool },
    Load { source: LocalId },
    Pure(String),
}

/// Which barriers each kind of key cares about.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Kind {
    /// Produces a pointer to a cell. Disturbed by anything that can add a field
    /// to a table or replace a table wholesale.
    Accessor,
    /// Reads a cell. Disturbed by anything that can write one.
    Load,
    /// A function of its operands alone. SSA means those never change, so
    /// nothing disturbs it.
    Pure,
}

fn kind_of(key: &Key) -> Kind {
    match key {
        Key::Field { .. } | Key::Index { .. } | Key::Global { .. } => Kind::Accessor,
        Key::Load { .. } => Kind::Load,
        Key::Pure(_) => Kind::Pure,
    }
}

/// Which cell a pointer-valued local denotes, as far as its defining
/// instruction reveals. The heap model (see the module doc) makes the four
/// known provenances permanently disjoint cell populations; `Unknown` (a
/// phi, an argument, a loaded pointer) may alias anything.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum CellProv {
    Field(String),
    Global(String),
    Index,
    Alloc(LocalId),
    Unknown,
}

/// Could a store through a `store`-provenance pointer change what a load
/// through a `load`-provenance pointer reads?
fn cells_may_alias(store: &CellProv, load: &CellProv) -> bool {
    use CellProv::*;
    match (store, load) {
        (Unknown, _) | (_, Unknown) => true,
        (Field(a), Field(b)) => a == b,
        (Global(a), Global(b)) => a == b,
        (Index, Index) => true,
        (Alloc(a), Alloc(b)) => a == b,
        _ => false,
    }
}

/// What one pair in the universe is vulnerable to, precomputed at build time.
#[derive(Debug)]
enum PairMeta {
    AccField(String),
    AccIndex,
    AccGlobal(String),
    Load(CellProv),
    Pure,
}

/// The invalidation one instruction performs, under `forward` mode. Memoizing
/// a bitmask per distinct barrier keeps applying one O(words), as before.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Barrier {
    None,
    All,
    Store(CellProv),
    CreateField(String),
    CreateIndex,
    CreateGlobal(String),
}

fn kills(barrier: &Barrier, meta: &PairMeta) -> bool {
    match (barrier, meta) {
        (Barrier::None, _) | (_, PairMeta::Pure) => false,
        (Barrier::All, _) => true,
        (Barrier::Store(p), PairMeta::Load(lp)) => cells_may_alias(p, lp),
        (Barrier::Store(_), _) => false,
        (Barrier::CreateField(n), PairMeta::AccField(m)) => n == m,
        // An UnknownTable this create turns into an ObjectTable would make a
        // remembered get_index on it error instead of repeating a NilPointer.
        (Barrier::CreateField(_), PairMeta::AccIndex) => true,
        (Barrier::CreateField(_), _) => false,
        (Barrier::CreateIndex, PairMeta::AccIndex) => true,
        // The same flip in the other direction.
        (Barrier::CreateIndex, PairMeta::AccField(_)) => true,
        (Barrier::CreateIndex, _) => false,
        (Barrier::CreateGlobal(n), PairMeta::AccGlobal(m)) => n == m,
        (Barrier::CreateGlobal(_), _) => false,
    }
}

/// Builtins that read and write no heap cell and create no global. `error`
/// aborts the run, `__print` formats its arguments and appends to the print
/// log, `__split_by_flr` is a function of its argument that splits the state.
/// A call provably reaching one of these invalidates nothing. Keep this list
/// in sync with the implementations in `game_runner.rs`.
const HEAP_OBLIVIOUS: &[&str] = &["error", "__print", "__split_by_flr"];

/// The subset of `HEAP_OBLIVIOUS` whose global can never have been shadowed:
/// every `get_global NAME` result in the whole program is used only by `Load`.
/// A name that fails the scan simply stays opaque.
fn oblivious_globals(program: &Program) -> rustc_hash::FxHashSet<String> {
    let mut bad: rustc_hash::FxHashSet<&str> = Default::default();
    for fun in program.functions.values() {
        let mut cells: FxHashMap<LocalId, &str> = FxHashMap::default();
        for (_, block) in all_blocks(&fun.cfg) {
            for (id, instr) in &block.instructions {
                if let Instruction::GetGlobal { name, .. } = instr {
                    if let Some(&n) = HEAP_OBLIVIOUS.iter().find(|&&n| n == name) {
                        cells.insert(*id, n);
                    }
                }
            }
        }
        if cells.is_empty() {
            continue;
        }
        for (_, block) in all_blocks(&fun.cfg) {
            for (_, instr) in &block.instructions {
                if let Instruction::Load { source } = instr {
                    if cells.contains_key(source) {
                        continue;
                    }
                }
                for used in instr.get_used_locals() {
                    if let Some(&n) = cells.get(&used) {
                        bad.insert(n);
                    }
                }
            }
            for used in block.terminator_kind().get_used_locals() {
                if let Some(&n) = cells.get(&used) {
                    bad.insert(n);
                }
            }
        }
    }
    HEAP_OBLIVIOUS
        .iter()
        .filter(|n| !bad.contains(*n))
        .map(|s| s.to_string())
        .collect()
}

/// Everything `forward` mode needs beyond the classic analysis: the defining
/// instruction of each id (for provenance) and the proven-unshadowed
/// heap-oblivious globals.
struct Fwd<'a> {
    defs: FxHashMap<LocalId, &'a Instruction>,
    oblivious: &'a rustc_hash::FxHashSet<String>,
}

impl<'a> Fwd<'a> {
    fn of(cfg: &'a Cfg, oblivious: &'a rustc_hash::FxHashSet<String>) -> Self {
        let mut defs = FxHashMap::default();
        for (_, block) in all_blocks(cfg) {
            for (id, instr) in &block.instructions {
                defs.insert(*id, instr);
            }
        }
        Self { defs, oblivious }
    }

    /// `id` must already be resolved through the substitution.
    fn cell_prov(&self, id: LocalId) -> CellProv {
        match self.defs.get(&id) {
            Some(Instruction::GetField { field, .. }) => CellProv::Field(field.clone()),
            Some(Instruction::GetGlobal { name, .. }) => CellProv::Global(name.clone()),
            Some(Instruction::GetIndex { .. }) => CellProv::Index,
            Some(Instruction::Alloc) => CellProv::Alloc(id),
            _ => CellProv::Unknown,
        }
    }

    /// Does `callee` provably hold a heap-oblivious builtin? True only when
    /// its def chain is `load (get_global NAME)` for a proven-unshadowed NAME.
    fn is_oblivious_callee(&self, callee: LocalId, resolve: &impl Fn(LocalId) -> LocalId) -> bool {
        let Some(Instruction::Load { source }) = self.defs.get(&callee) else {
            return false;
        };
        let Some(Instruction::GetGlobal { name, .. }) = self.defs.get(&resolve(*source)) else {
            return false;
        };
        self.oblivious.contains(name)
    }

    fn barrier_of(&self, instr: &Instruction, resolve: &impl Fn(LocalId) -> LocalId) -> Barrier {
        match instr {
            Instruction::Store { target, .. } | Instruction::StoreClosure { target, .. } => {
                Barrier::Store(self.cell_prov(resolve(*target)))
            }
            Instruction::StoreEmptyTable { .. } => Barrier::All,
            Instruction::Call { closure, .. } => {
                if self.is_oblivious_callee(resolve(*closure), resolve) {
                    Barrier::None
                } else {
                    Barrier::All
                }
            }
            Instruction::GetField { create_if_missing: true, field, .. } => {
                Barrier::CreateField(field.clone())
            }
            Instruction::GetIndex { create_if_missing: true, .. } => Barrier::CreateIndex,
            Instruction::GetGlobal { create_if_missing: true, name, .. } => {
                Barrier::CreateGlobal(name.clone())
            }
            _ => Barrier::None,
        }
    }

    fn meta_of(&self, key: &Key) -> PairMeta {
        match key {
            Key::Field { field, .. } => PairMeta::AccField(field.clone()),
            Key::Index { .. } => PairMeta::AccIndex,
            Key::Global { name, .. } => PairMeta::AccGlobal(name.clone()),
            Key::Load { source } => PairMeta::Load(self.cell_prov(*source)),
            Key::Pure(_) => PairMeta::Pure,
        }
    }
}

/// Applies `barrier` to `state`, memoizing one mask per distinct barrier.
fn forward_barrier(
    state: &mut Bits,
    metas: &[PairMeta],
    cache: &mut FxHashMap<Barrier, Bits>,
    barrier: &Barrier,
) {
    if matches!(barrier, Barrier::None) {
        return;
    }
    if !cache.contains_key(barrier) {
        let mut mask = bits_full(metas.len());
        for (i, meta) in metas.iter().enumerate() {
            if kills(barrier, meta) {
                bit_clear(&mut mask, i);
            }
        }
        cache.insert(barrier.clone(), mask);
    }
    bits_and(state, &cache[barrier]);
}

/// The key an instruction is remembered by, if it is a candidate at all.
///
/// `resolve` maps an operand to its surviving equivalent, so that a chain of
/// duplicated instructions collapses over successive rounds.
fn key_of(instr: &Instruction, resolve: &impl Fn(LocalId) -> LocalId) -> Option<Key> {
    let resolved = instr.map_local_ids(|id| resolve(id));
    Some(match &resolved {
        Instruction::GetField { receiver, field, create_if_missing } => Key::Field {
            receiver: *receiver,
            field: field.clone(),
            create: *create_if_missing,
        },
        Instruction::GetIndex { receiver, index, create_if_missing } => Key::Index {
            receiver: *receiver,
            index: *index,
            create: *create_if_missing,
        },
        Instruction::GetGlobal { name, create_if_missing } => Key::Global {
            name: name.clone(),
            create: *create_if_missing,
        },
        Instruction::Load { source } => Key::Load { source: *source },
        // Pure functions of their operands. Keyed by their debug form, which is
        // exact for these variants because every field is an operand or a
        // literal.
        Instruction::UnaryOp { .. }
        | Instruction::BinaryOp { .. }
        | Instruction::Select { .. }
        | Instruction::NumberConstant { .. }
        | Instruction::BoolConstant { .. }
        | Instruction::StringConstant { .. }
        | Instruction::NilConstant => Key::Pure(format!("{:?}", resolved)),
        // Deliberately excluded. `Alloc` produces a fresh cell every time.
        // `Phi` belongs to a control-flow join. The stores and `AssertClosure`
        // have effects and produce nothing.
        _ => return None,
    })
}

/// Is a value of this kind worth remembering past a block boundary?
///
/// Not a soundness question. `Pure` is the *safest* kind to reuse - nothing can
/// disturb a function of SSA operands - so this is purely about cost. Reusing a
/// value defined in an earlier block keeps it live in the interpreter's
/// `LocalEnv` across everything in between, and every per-state operation
/// (merge, gc, dedup, shape grouping) is charged for it. Those dominate the
/// frame; running an instruction does not.
///
/// For a heap accessor the trade is clearly worth it - the alternative is real
/// work against the heap. For arithmetic and constants it is clearly not, and
/// measurably so. Letting `Pure` cross blocks as well removes a further 998
/// instructions, takes `player.update_21` from 24 live slots to 29, and costs
/// 5% of frame time: 4.33s -> 4.55s at frame 34, with the lane count identical.
/// Rematerialising a constant is cheaper than carrying it.
fn crosses_blocks(kind: Kind) -> bool {
    match kind {
        Kind::Accessor | Kind::Load => true,
        Kind::Pure => false,
    }
}

/// Does this instruction invalidate remembered values of `kind`?
///
/// Conservative on purpose: no aliasing is assumed anywhere. A
/// `create_if_missing` accessor on any receiver invalidates every accessor, and
/// a store through any pointer invalidates every load.
fn invalidates(instr: &Instruction, kind: Kind) -> bool {
    match kind {
        Kind::Pure => false,
        Kind::Accessor => match instr {
            // A callee can create fields or replace tables.
            Instruction::Call { .. } => true,
            // Creates a cell where there was none, so an earlier accessor for
            // the same field may have returned a nil pointer instead.
            Instruction::GetGlobal { create_if_missing, .. }
            | Instruction::GetField { create_if_missing, .. }
            | Instruction::GetIndex { create_if_missing, .. } => *create_if_missing,
            // Replaces a table, discarding every field it had.
            Instruction::StoreEmptyTable { .. } => true,
            _ => false,
        },
        Kind::Load => match instr {
            Instruction::Call { .. } => true,
            Instruction::Store { .. }
            | Instruction::StoreEmptyTable { .. }
            | Instruction::StoreClosure { .. } => true,
            // A load reads a cell, and a `create` accessor can turn a nil
            // pointer into a real one.
            Instruction::GetGlobal { create_if_missing, .. }
            | Instruction::GetField { create_if_missing, .. }
            | Instruction::GetIndex { create_if_missing, .. } => *create_if_missing,
            _ => false,
        },
    }
}

type Bits = Vec<u64>;

fn bits_empty(bits: usize) -> Bits {
    vec![0u64; bits.div_ceil(64).max(1)]
}

/// All `bits` low bits set, the padding above them clear. The optimistic
/// starting point of the fixpoint.
fn bits_full(bits: usize) -> Bits {
    let mut out = vec![u64::MAX; bits.div_ceil(64).max(1)];
    let tail = bits % 64;
    if tail != 0 {
        *out.last_mut().unwrap() = (1u64 << tail) - 1;
    } else if bits == 0 {
        out[0] = 0;
    }
    out
}

fn bit_get(set: &[u64], i: usize) -> bool {
    set[i / 64] & (1u64 << (i % 64)) != 0
}

fn bit_set(set: &mut [u64], i: usize) {
    set[i / 64] |= 1u64 << (i % 64);
}

fn bit_clear(set: &mut [u64], i: usize) {
    set[i / 64] &= !(1u64 << (i % 64));
}

fn bits_and(into: &mut [u64], other: &[u64]) {
    for (word, mask) in into.iter_mut().zip(other) {
        *word &= *mask;
    }
}

fn bits_or(into: &mut [u64], other: &[u64]) {
    for (word, mask) in into.iter_mut().zip(other) {
        *word |= *mask;
    }
}

/// Everything one round of the analysis needs about the shape of the function.
/// The CFG does not change while the substitution is being computed, so this is
/// built once and reused by every round.
struct Layout<'a> {
    /// Blocks by the index `Dominance` gives them.
    blocks: Vec<Option<&'a Block>>,
    /// Predecessors by index, reachable ones only.
    preds: Vec<Vec<usize>>,
    /// Reachable blocks in reverse postorder.
    order: Vec<usize>,
    entry: Option<usize>,
}

impl<'a> Layout<'a> {
    fn of(cfg: &'a Cfg) -> Self {
        let dominance = Dominance::of(cfg);
        let all = all_blocks(cfg);
        let mut blocks: Vec<Option<&Block>> = vec![None; all.len()];
        let mut preds: Vec<Vec<usize>> = vec![Vec::new(); all.len()];
        for (key, block) in &all {
            if let Some(i) = dominance.index_of(key) {
                blocks[i] = Some(*block);
            }
        }
        for (key, block) in &all {
            if !dominance.is_reachable(key) {
                continue;
            }
            let Some(from) = dominance.index_of(key) else { continue };
            for succ in successors(block) {
                if let Some(to) = dominance.index_of(&succ) {
                    preds[to].push(from);
                }
            }
        }
        Self {
            blocks,
            preds,
            order: dominance.order().to_vec(),
            entry: dominance.index_of(&None),
        }
    }
}

/// The universe the bitsets range over: one bit per keyed instruction that is
/// still present, in reverse-postorder-then-program order.
struct Universe {
    pairs: Vec<(Key, LocalId)>,
    bit_of: FxHashMap<LocalId, usize>,
    /// Bits sharing a key, ascending, so the earliest candidate is picked.
    by_key: FxHashMap<Key, Vec<usize>>,
    /// Zero exactly at the bits an accessor barrier forgets, so `state &= this`
    /// applies the barrier.
    keep_accessor: Bits,
    keep_load: Bits,
    /// Zero at the bits of kinds that do not survive a block boundary, so
    /// `state &= this` at a block entry applies `crosses_blocks`.
    keep_across_edges: Bits,
}

impl Universe {
    fn keep_mask(&self, kind: Kind) -> Option<&Bits> {
        match kind {
            Kind::Accessor => Some(&self.keep_accessor),
            Kind::Load => Some(&self.keep_load),
            // Nothing disturbs a function of SSA operands.
            Kind::Pure => None,
        }
    }

    fn len(&self) -> usize {
        self.pairs.len()
    }
}

/// The instructions of a block that a round still sees. Ones already folded
/// away by an earlier round are gone: they neither produce a value nor act as a
/// barrier. That is sound because a barrier instruction is never itself folded
/// away - the only keyed instructions that are barriers are the `create`
/// accessors, and one of those invalidates its own key before it is looked up.
fn live_instructions<'a>(
    block: &'a Block,
    subst: &'a FxHashMap<LocalId, LocalId>,
) -> impl Iterator<Item = &'a (LocalId, Instruction)> {
    block
        .instructions
        .iter()
        .filter(move |(id, _)| !subst.contains_key(id))
}

/// Applies to `state` every barrier `instr` represents.
fn apply_barriers(state: &mut Bits, universe: &Universe, instr: &Instruction) {
    for kind in [Kind::Accessor, Kind::Load, Kind::Pure] {
        if !invalidates(instr, kind) {
            continue;
        }
        if let Some(mask) = universe.keep_mask(kind) {
            bits_and(state, mask);
        }
    }
}

/// The substitution this rule performs on one function: every removed id mapped
/// to the surviving instruction that computes the same value.
///
/// Written once and used by both halves - see the note on `verify`.
///
/// Rounds exist so that chains collapse: once `%706 = get_field %2.spd` folds
/// into `%683`, the `load %706` that follows becomes a `load %683` and is itself
/// a duplicate. Each round re-keys the instructions through the substitution
/// found so far and treats the ones already removed as gone, so a round can only
/// add. That terminates because the number of instructions is finite.
fn substitution(
    cfg: &Cfg,
    oblivious: Option<&rustc_hash::FxHashSet<String>>,
) -> FxHashMap<LocalId, LocalId> {
    let layout = Layout::of(cfg);
    let fwd = oblivious.map(|o| Fwd::of(cfg, o));
    let mut subst: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    loop {
        let found = redundancies(&layout, &subst, fwd.as_ref());
        let before = subst.len();
        for (from, to) in found {
            subst.entry(from).or_insert(to);
        }
        if subst.len() == before {
            return subst;
        }
        compress(&mut subst);
    }
}

/// Makes every entry point at an instruction that survives.
///
/// A round records the *earliest* available candidate, which may itself have
/// been folded into something earlier still in a later round. Chains are
/// necessarily acyclic - `%b -> %a` requires `%a` to run before `%b` on the path
/// from the entry, so `%a -> %b` cannot also hold - but rather than rely on
/// that, the walk is bounded and a chain that fails to terminate is left
/// pointing at a removed instruction, which `verify` rejects loudly.
fn compress(subst: &mut FxHashMap<LocalId, LocalId>) {
    let limit = subst.len();
    let sources: Vec<LocalId> = subst.keys().copied().collect();
    for source in sources {
        let mut target = subst[&source];
        for _ in 0..limit {
            match subst.get(&target) {
                Some(&next) if next != target => target = next,
                _ => break,
            }
        }
        subst.insert(source, target);
    }
}

/// One pass of available-expressions over the function, returning the
/// instructions that are redundant given the substitutions already found.
fn redundancies(
    layout: &Layout,
    subst: &FxHashMap<LocalId, LocalId>,
    fwd: Option<&Fwd>,
) -> FxHashMap<LocalId, LocalId> {
    let resolve = |id: LocalId| *subst.get(&id).unwrap_or(&id);

    let mut universe = Universe {
        pairs: Vec::new(),
        bit_of: FxHashMap::default(),
        by_key: FxHashMap::default(),
        keep_accessor: Bits::new(),
        keep_load: Bits::new(),
        keep_across_edges: Bits::new(),
    };
    for &b in &layout.order {
        let Some(block) = layout.blocks[b] else { continue };
        for (id, instr) in live_instructions(block, subst) {
            // In forward mode a store is itself a source of the cell's value:
            // it enters the universe under the key a load of that cell would
            // have, paired with the *stored* id. It is never folded away
            // itself - the replay skips stores when looking for duplicates.
            let (key, value) = if let (Some(_), Instruction::Store { target, source }) =
                (fwd, instr)
            {
                (Key::Load { source: resolve(*target) }, resolve(*source))
            } else {
                match key_of(instr, &resolve) {
                    Some(key) => (key, *id),
                    None => continue,
                }
            };
            universe.bit_of.insert(*id, universe.pairs.len());
            universe
                .by_key
                .entry(key.clone())
                .or_default()
                .push(universe.pairs.len());
            universe.pairs.push((key, value));
        }
    }
    // Metadata for the refined kills, aligned with `pairs`. Only in forward
    // mode; empty otherwise.
    let metas: Vec<PairMeta> = match fwd {
        Some(f) => universe.pairs.iter().map(|(key, _)| f.meta_of(key)).collect(),
        None => Vec::new(),
    };
    let bits = universe.len();
    if bits == 0 {
        return FxHashMap::default();
    }
    universe.keep_accessor = bits_full(bits);
    universe.keep_load = bits_full(bits);
    universe.keep_across_edges = bits_full(bits);
    for (i, (key, _)) in universe.pairs.iter().enumerate() {
        let kind = kind_of(key);
        match kind {
            Kind::Accessor => bit_clear(&mut universe.keep_accessor, i),
            Kind::Load => bit_clear(&mut universe.keep_load, i),
            Kind::Pure => {}
        }
        if !crosses_blocks(kind) {
            bit_clear(&mut universe.keep_across_edges, i);
        }
    }
    // In forward mode nothing crosses a block boundary at all. The earlier
    // classic `cse` entry already reuses accessors and loads across blocks
    // under the strong kills; what forward mode adds on top - store
    // forwarding, loads surviving stores/creates/oblivious calls - would
    // otherwise keep values live across long stretches, and every per-state
    // operation (merge, dedup, filter) is charged for the whole env. Measured
    // directly: letting the new folds cross raised `player.update_21` from 33
    // to 47 live slots and was ~2% slower at frame 37 with the same fragment
    // count, eating the win the removed instructions bought.
    if fwd.is_some() {
        universe.keep_across_edges = bits_empty(bits);
    }

    // Summarise each block as `out = (in & mask) | gen`. Valid because every
    // kill is a constant mask, so a sequence of them composes into one.
    let mut mask_cache: FxHashMap<Barrier, Bits> = FxHashMap::default();
    let mut barriers = |state: &mut Bits, other: Option<&mut Bits>, instr: &Instruction| {
        match fwd {
            None => {
                apply_barriers(state, &universe, instr);
                if let Some(other) = other {
                    apply_barriers(other, &universe, instr);
                }
            }
            Some(f) => {
                let barrier = f.barrier_of(instr, &resolve);
                forward_barrier(state, &metas, &mut mask_cache, &barrier);
                if let Some(other) = other {
                    forward_barrier(other, &metas, &mut mask_cache, &barrier);
                }
            }
        }
    };
    let count = layout.blocks.len();
    let mut mask: Vec<Bits> = vec![Bits::new(); count];
    let mut gen: Vec<Bits> = vec![Bits::new(); count];
    for &b in &layout.order {
        let Some(block) = layout.blocks[b] else { continue };
        let mut m = bits_full(bits);
        let mut g = bits_empty(bits);
        for (id, instr) in live_instructions(block, subst) {
            // Barriers first, so an instruction never matches something its own
            // effects invalidated - a `create` accessor forgets itself, and a
            // store forgets the pairs of earlier writes to the same cell.
            barriers(&mut m, Some(&mut g), instr);
            if let Some(&bit) = universe.bit_of.get(id) {
                bit_set(&mut g, bit);
            }
        }
        mask[b] = m;
        gen[b] = g;
    }

    // Fixpoint, started optimistically so that loops converge to the largest
    // consistent answer rather than to nothing.
    let mut inn: Vec<Bits> = vec![bits_empty(bits); count];
    let mut out: Vec<Bits> = vec![bits_empty(bits); count];
    for &b in &layout.order {
        out[b] = if Some(b) == layout.entry {
            gen[b].clone()
        } else {
            bits_full(bits)
        };
    }
    let mut scratch = bits_empty(bits);
    let mut changed = true;
    while changed {
        changed = false;
        for &b in &layout.order {
            // The entry block is reached by no path, so nothing is available.
            if Some(b) == layout.entry {
                continue;
            }
            let mut any = false;
            for &p in &layout.preds[b] {
                if any {
                    bits_and(&mut scratch, &out[p]);
                } else {
                    scratch.copy_from_slice(&out[p]);
                    any = true;
                }
            }
            if !any {
                scratch.fill(0);
            }
            // Values that are not worth carrying between blocks are dropped
            // here, at the edge, rather than never recorded - within a block
            // they are still folded.
            bits_and(&mut scratch, &universe.keep_across_edges);
            inn[b].copy_from_slice(&scratch);
            bits_and(&mut scratch, &mask[b]);
            bits_or(&mut scratch, &gen[b]);
            if scratch != out[b] {
                out[b].copy_from_slice(&scratch);
                changed = true;
            }
        }
    }

    // Replay each block against the fixpoint and read off the duplicates.
    let mut found = FxHashMap::default();
    for &b in &layout.order {
        let Some(block) = layout.blocks[b] else { continue };
        let mut state = inn[b].clone();
        for (id, instr) in live_instructions(block, subst) {
            barriers(&mut state, None, instr);
            let Some(&bit) = universe.bit_of.get(id) else { continue };
            // A store contributes its pair but is never itself a duplicate.
            if !matches!(instr, Instruction::Store { .. }) {
                let candidates = &universe.by_key[&universe.pairs[bit].0];
                if let Some(&other) = candidates
                    .iter()
                    .find(|&&c| c != bit && bit_get(&state, c))
                {
                    found.insert(*id, universe.pairs[other].1);
                }
            }
            bit_set(&mut state, bit);
        }
    }
    found
}

pub fn apply(program: &mut Program, forward: bool) -> Result<usize> {
    let oblivious = if forward { Some(oblivious_globals(program)) } else { None };
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        let subst = substitution(&fun.cfg, oblivious.as_ref());
        if subst.is_empty() {
            continue;
        }
        changes += subst.len();
        let resolve = |id: LocalId| *subst.get(&id).unwrap_or(&id);
        for block in blocks_mut(&mut fun.cfg) {
            block.instructions.retain(|(id, _)| !subst.contains_key(id));
            for (_, instr) in block.instructions.iter_mut() {
                *instr = instr.map_local_ids(resolve);
            }
            block.terminator.1 = block.terminator.1.map_local_ids(resolve);
        }
    }
    Ok(changes)
}

fn blocks_mut(cfg: &mut Cfg) -> impl Iterator<Item = &mut Block> {
    std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut())
}

/// Independent check.
///
/// Like the other bulk rules, this verifies by re-deriving the answer from the
/// before program and insisting the after program is exactly that. Re-derivation
/// is the right strategy here because the rule is *canonical*: its result is a
/// function of the input alone, so an independent computation of that function
/// is a complete check. What it does not do - and what would defeat the purpose
/// - is trust anything the applier recorded about what it did.
///
/// The one piece of shared code is `substitution`, which is the specification of
/// the rule rather than an implementation detail of the applier. What is checked
/// separately here is everything the applier could get wrong on top of it:
/// removing an instruction that was not in the substitution, keeping one that
/// was, failing to substitute a use, reordering, or touching a terminator.
pub fn verify(before: &Program, after: &Program, forward: bool) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "cse changed the set of functions",
    )?;
    let oblivious = if forward { Some(oblivious_globals(before)) } else { None };

    for (name, before_fun) in &before.functions {
        let after_fun = after.get(name.as_str())?;
        let subst = substitution(&before_fun.cfg, oblivious.as_ref());
        let resolve = |id: LocalId| *subst.get(&id).unwrap_or(&id);

        // A removed instruction must be replaced by one that survives, so the
        // substitution can never chain into a hole.
        for (removed, replacement) in &subst {
            require(
                !subst.contains_key(replacement),
                format!(
                    "cse in {}: %{} was replaced by %{}, which was itself removed",
                    name.as_str(),
                    usize::from(*removed),
                    usize::from(*replacement)
                ),
            )?;
        }

        for key in super::blocks_sorted(&before_fun.cfg) {
            let before_block = super::get_block(&before_fun.cfg, &key)
                .ok_or_else(|| anyhow!("cse: missing block in the before program"))?;
            let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
                anyhow!(
                    "cse removed block '{}' from {}",
                    super::super::validate::block_label(&key),
                    name.as_str()
                )
            })?;

            let mut after_iter = after_block.instructions.iter();
            for (id, instr) in &before_block.instructions {
                if subst.contains_key(id) {
                    continue;
                }
                let (after_id, after_instr) = after_iter.next().ok_or_else(|| {
                    anyhow!(
                        "cse dropped %{} from '{}' in {}",
                        usize::from(*id),
                        super::super::validate::block_label(&key),
                        name.as_str()
                    )
                })?;
                require(
                    after_id == id,
                    format!(
                        "cse reordered '{}' in {}: expected %{}, found %{}",
                        super::super::validate::block_label(&key),
                        name.as_str(),
                        usize::from(*id),
                        usize::from(*after_id)
                    ),
                )?;
                let want = instr.map_local_ids(resolve);
                require(
                    want == *after_instr,
                    format!(
                        "cse changed %{} in '{}' of {} unexpectedly:\n  want {}\n  got  {}",
                        usize::from(*id),
                        super::super::validate::block_label(&key),
                        name.as_str(),
                        super::super::print::format_instruction(&want),
                        super::super::print::format_instruction(after_instr)
                    ),
                )?;
            }
            require(
                after_iter.next().is_none(),
                format!(
                    "cse added instructions to '{}' in {}",
                    super::super::validate::block_label(&key),
                    name.as_str()
                ),
            )?;

            let want_term = before_block.terminator_kind().map_local_ids(resolve);
            require(
                format!("{:?}", want_term) == format!("{:?}", after_block.terminator_kind()),
                format!(
                    "cse changed the terminator of '{}' in {}",
                    super::super::validate::block_label(&key),
                    name.as_str()
                ),
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{FunDef, GlobalId, Label, Terminator};
    use crate::rewrite::print::format_function;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn field(receiver: usize, name: &str, create: bool) -> Instruction {
        Instruction::GetField {
            receiver: id(receiver),
            field: name.to_string(),
            create_if_missing: create,
        }
    }

    fn program_of(instructions: Vec<(LocalId, Instruction)>, ret: LocalId) -> Program {
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2))],
            cfg: Cfg::new(
                Block {
                    instructions,
                    terminator: (id(900), Terminator::Return { value: Some(ret) }),
                    hint_normalize: false,
                },
                Default::default(),
            ),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    fn run(p: &mut Program) -> String {
        let before = p.clone();
        apply(p, false).unwrap();
        verify(&before, p, false).unwrap();
        format_function(p.get("f").unwrap())
    }

    fn run_forward(p: &mut Program) -> String {
        let before = p.clone();
        apply(p, true).unwrap();
        verify(&before, p, true).unwrap();
        format_function(p.get("f").unwrap())
    }

    #[test]
    fn collapses_repeated_field_reads() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(!text.contains("%11"), "{}", text);
        assert!(text.contains("%12 = load %10"), "{}", text);
    }

    /// The chain must collapse in one pass: once the accessor folds, the load
    /// of it becomes a duplicate too.
    #[test]
    fn collapses_a_chain_in_one_pass() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "spd", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), field(2, "spd", false)),
                (id(13), Instruction::Load { source: id(12) }),
                (id(14), field(13, "x", false)),
            ],
            id(14),
        );
        let text = run(&mut p);
        assert!(!text.contains("%12"), "{}", text);
        assert!(!text.contains("%13"), "{}", text);
        assert!(text.contains("%14 = get_field %11.x"), "{}", text);
    }

    /// A store between two loads of the same cell means the second may see a
    /// different value.
    #[test]
    fn a_store_stops_a_load_from_being_reused() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), Instruction::Store { target: id(10), source: id(2) }),
                (id(13), Instruction::Load { source: id(10) }),
            ],
            id(13),
        );
        let text = run(&mut p);
        assert!(text.contains("%13 = load %10"), "{}", text);
    }

    /// ...but a store does not stop the *accessor* from being reused, because a
    /// store writes a cell rather than adding a field.
    #[test]
    fn a_store_does_not_stop_an_accessor_from_being_reused() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Store { target: id(10), source: id(2) }),
                (id(12), field(2, "x", false)),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(!text.contains("%12"), "{}", text);
    }

    /// A creating accessor may bring a field into existence, so an earlier
    /// non-creating read of it might have returned a nil pointer instead.
    #[test]
    fn a_creating_accessor_stops_reuse() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "y", true)),
                (id(12), field(2, "x", false)),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(text.contains("%12 = get_field %2.x"), "{}", text);
    }

    /// A call can do anything at all.
    #[test]
    fn a_call_stops_reuse() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Call { closure: id(2), args: vec![] }),
                (id(12), field(2, "x", false)),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(text.contains("%12 = get_field %2.x"), "{}", text);
    }

    /// Reading a field and creating it are different instructions and must not
    /// be conflated in either direction.
    #[test]
    fn reading_and_creating_are_different_values() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "x", true)),
            ],
            id(11),
        );
        let text = run(&mut p);
        assert!(text.contains("%11 = get_field %2.x create"), "{}", text);
    }

    /// `alloc` produces a fresh cell every time and must never be merged.
    #[test]
    fn allocs_are_never_merged() {
        let mut p = program_of(
            vec![(id(10), Instruction::Alloc), (id(11), Instruction::Alloc)],
            id(11),
        );
        let text = run(&mut p);
        assert!(text.contains("%10 = alloc") && text.contains("%11 = alloc"), "{}", text);
    }

    /// Applying the rule twice must change nothing the second time.
    #[test]
    fn is_idempotent() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
            ],
            id(12),
        );
        run(&mut p);
        let once = format_function(p.get("f").unwrap());
        assert_eq!(apply(&mut p, false).unwrap(), 0);
        assert_eq!(once, format_function(p.get("f").unwrap()));
    }

    // -- across blocks --------------------------------------------------
    //
    // The interesting cases are all about *which* path, so these build real
    // CFGs rather than one straight-line block.

    fn goto(target: &str) -> Terminator {
        Terminator::UnconditionalBranch { target: Label::from(target.to_string()) }
    }

    fn branch(cond: usize, t: &str, f: &str) -> Terminator {
        Terminator::ConditionalBranch {
            condition: id(cond),
            true_target: Label::from(t.to_string()),
            false_target: Label::from(f.to_string()),
        }
    }

    /// `blocks` is the entry block followed by named ones. Terminator ids are
    /// generated, so they never clash with instruction ids.
    fn cfg_of(blocks: Vec<(&str, Vec<(LocalId, Instruction)>, Terminator)>) -> Program {
        let mut made: Vec<Block> = Vec::new();
        for (n, (_, instructions, terminator)) in blocks.iter().enumerate() {
            made.push(Block {
                instructions: instructions.clone(),
                terminator: (id(900 + n), terminator.clone()),
                hint_normalize: false,
            });
        }
        let mut named = rustc_hash::FxHashMap::default();
        for ((label, _, _), block) in blocks.iter().zip(made.iter()).skip(1) {
            named.insert(Label::from(label.to_string()), block.clone());
        }
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2)), Some(id(3))],
            cfg: Cfg::new(made[0].clone(), named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    /// The basic cross-block case: the entry dominates the successor and
    /// nothing in between disturbs the value.
    #[test]
    fn reuses_a_value_from_a_dominating_block() {
        let mut p = cfg_of(vec![
            ("__entry", vec![(id(10), field(2, "x", false))], goto("b")),
            ("b", vec![(id(11), field(2, "x", false))], Terminator::Return { value: Some(id(11)) }),
        ]);
        let text = run(&mut p);
        assert!(!text.contains("%11 ="), "{}", text);
        assert!(text.contains("return %10"), "{}", text);
    }

    /// A definition in one arm does not dominate the join, so it is not
    /// available there however identical it looks.
    #[test]
    fn does_not_reuse_a_value_from_only_one_arm() {
        let mut p = cfg_of(vec![
            ("__entry", vec![], branch(3, "t", "e")),
            ("t", vec![(id(10), field(2, "x", false))], goto("j")),
            ("e", vec![], goto("j")),
            ("j", vec![(id(11), field(2, "x", false))], Terminator::Return { value: Some(id(11)) }),
        ]);
        let text = run(&mut p);
        assert!(text.contains("%11 = get_field %2.x"), "{}", text);
    }

    /// Available on one path is not available. The store on the other arm must
    /// veto reuse at the join even though the entry's load dominates it.
    #[test]
    fn a_barrier_on_one_path_stops_reuse_at_the_join() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![(id(10), field(2, "x", false)), (id(11), Instruction::Load { source: id(10) })],
                branch(3, "t", "e"),
            ),
            ("t", vec![(id(12), Instruction::Store { target: id(10), source: id(3) })], goto("j")),
            ("e", vec![], goto("j")),
            ("j", vec![(id(13), Instruction::Load { source: id(10) })], Terminator::Return { value: Some(id(13)) }),
        ]);
        let text = run(&mut p);
        assert!(text.contains("%13 = load %10"), "{}", text);
    }

    /// The case the module doc calls out: the key is available at the join, but
    /// only because of the recomputation *after* the barrier. Picking the
    /// dominating definition instead would read a stale value.
    #[test]
    fn picks_the_available_definition_not_the_dominating_one() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![(id(10), field(2, "x", false)), (id(11), Instruction::Load { source: id(10) })],
                goto("c"),
            ),
            (
                "c",
                vec![
                    (id(12), Instruction::Store { target: id(10), source: id(3) }),
                    (id(13), Instruction::Load { source: id(10) }),
                ],
                goto("j"),
            ),
            ("j", vec![(id(14), Instruction::Load { source: id(10) })], Terminator::Return { value: Some(id(14)) }),
        ]);
        let text = run(&mut p);
        assert!(!text.contains("%14 ="), "{}", text);
        assert!(text.contains("return %13"), "{}", text);
        assert!(text.contains("%11 = load %10"), "{}", text);
    }

    /// A loop body that writes the cell it reads. The read cannot be hoisted
    /// out of the loop by reusing the entry's, because the back edge carries the
    /// store. The optimistic start of the fixpoint is exactly what would get
    /// this wrong.
    #[test]
    fn a_store_on_the_back_edge_stops_reuse_in_the_loop() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![(id(10), field(2, "x", false)), (id(11), Instruction::Load { source: id(10) })],
                goto("body"),
            ),
            (
                "body",
                vec![
                    (id(12), Instruction::Load { source: id(10) }),
                    (id(13), Instruction::Store { target: id(10), source: id(3) }),
                ],
                branch(3, "body", "done"),
            ),
            ("done", vec![], Terminator::Return { value: Some(id(12)) }),
        ]);
        let text = run(&mut p);
        assert!(text.contains("%12 = load %10"), "{}", text);
    }

    /// ...but a loop that only reads is fine: the entry's load reaches every
    /// iteration.
    #[test]
    fn a_read_only_loop_reuses_the_value_from_before_it() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![(id(10), field(2, "x", false)), (id(11), Instruction::Load { source: id(10) })],
                goto("body"),
            ),
            (
                "body",
                vec![(id(12), Instruction::Load { source: id(10) })],
                branch(3, "body", "done"),
            ),
            ("done", vec![], Terminator::Return { value: Some(id(12)) }),
        ]);
        let text = run(&mut p);
        assert!(!text.contains("%12 ="), "{}", text);
        assert!(text.contains("return %11"), "{}", text);
    }

    /// Pure values stay block-local on purpose - see `crosses_blocks`. The
    /// accessor beside them proves this is a policy about kinds and not a
    /// failure to see across the edge.
    #[test]
    fn pure_values_are_not_reused_across_blocks_but_accessors_are() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![
                    (id(10), Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(7) }),
                    (id(11), field(2, "x", false)),
                ],
                goto("b"),
            ),
            (
                "b",
                vec![
                    (id(12), Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(7) }),
                    (id(13), field(2, "x", false)),
                ],
                Terminator::Return { value: Some(id(12)) },
            ),
        ]);
        let text = run(&mut p);
        assert!(text.contains("%12 = "), "the constant should be recomputed:\n{}", text);
        assert!(!text.contains("%13 = "), "the accessor should be reused:\n{}", text);
    }

    /// Chains still collapse when the links are in different blocks, which is
    /// what the rounds are for.
    #[test]
    fn collapses_a_chain_across_blocks() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![(id(10), field(2, "spd", false)), (id(11), Instruction::Load { source: id(10) })],
                goto("b"),
            ),
            (
                "b",
                vec![
                    (id(12), field(2, "spd", false)),
                    (id(13), Instruction::Load { source: id(12) }),
                    (id(14), field(13, "x", false)),
                ],
                Terminator::Return { value: Some(id(14)) },
            ),
        ]);
        let text = run(&mut p);
        assert!(!text.contains("%12 =") && !text.contains("%13 ="), "{}", text);
        assert!(text.contains("%14 = get_field %11.x"), "{}", text);
    }

    /// `verify` must reject an applier that removed an instruction it had no
    /// licence to remove.
    #[test]
    fn verify_rejects_an_unjustified_removal() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Call { closure: id(2), args: vec![] }),
                (id(12), field(2, "x", false)),
            ],
            id(10),
        );
        let before = p.clone();
        // Pretend the call was not a barrier.
        let fun = p.get_mut("f").unwrap();
        fun.cfg.entry.instructions.retain(|(i, _)| *i != id(12));
        fun.cfg.entry.terminator.1 = Terminator::Return { value: Some(id(10)) };
        assert!(verify(&before, &p, false).is_err());
    }

    // --- forward mode ---

    /// The heart of the mode: a load after a store of the same cell is the
    /// stored value.
    #[test]
    fn forwards_a_store_to_a_load() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Store { target: id(10), source: id(2) }),
                (id(12), Instruction::Load { source: id(10) }),
            ],
            id(12),
        );
        let text = run_forward(&mut p);
        assert!(!text.contains("%12"), "the load should fold to %2:\n{}", text);
        assert!(text.contains("return %2"), "{}", text);
    }

    /// Classic mode must not forward - old recipe entries replay unchanged.
    #[test]
    fn classic_mode_does_not_forward() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Store { target: id(10), source: id(2) }),
                (id(12), Instruction::Load { source: id(10) }),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(text.contains("%12 = load %10"), "{}", text);
    }

    /// A store to a differently-named field cannot touch this cell, so the
    /// load is still available.
    #[test]
    fn a_store_of_one_field_spares_loads_of_another() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "y", false)),
                (id(12), Instruction::Load { source: id(11) }),
                (id(13), Instruction::Store { target: id(10), source: id(2) }),
                (id(14), Instruction::Load { source: id(11) }),
            ],
            id(14),
        );
        let text = run_forward(&mut p);
        assert!(!text.contains("%14"), "the y load should be reused:\n{}", text);
    }

    /// ...but a store to the *same* field name through another receiver may
    /// alias, and must still kill.
    #[test]
    fn a_same_name_store_still_kills() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "o", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), field(11, "x", false)),
                (id(13), field(2, "x", false)),
                (id(14), Instruction::Load { source: id(13) }),
                (id(15), Instruction::Store { target: id(12), source: id(2) }),
                (id(16), Instruction::Load { source: id(13) }),
            ],
            id(16),
        );
        let text = run_forward(&mut p);
        assert!(text.contains("%16 = load %13"), "{}", text);
    }

    /// A store through a pointer of unknown provenance (here: the argument
    /// itself) could write any cell at all.
    #[test]
    fn a_store_through_an_unknown_pointer_kills_everything() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), Instruction::Store { target: id(2), source: id(10) }),
                (id(13), Instruction::Load { source: id(10) }),
            ],
            id(13),
        );
        let text = run_forward(&mut p);
        assert!(text.contains("%13 = load %10"), "{}", text);
    }

    /// A `create` accessor only allocates a *missing* cell; the contents of
    /// every existing cell - and every immutable SSA source local - are
    /// untouched, so loads survive it in forward mode.
    #[test]
    fn a_create_no_longer_kills_loads() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), field(2, "y", true)),
                (id(13), Instruction::Load { source: id(10) }),
            ],
            id(13),
        );
        let text = run_forward(&mut p);
        assert!(!text.contains("%13"), "the load should be reused:\n{}", text);
    }

    /// A create still kills *accessors* of the same name - an earlier
    /// non-creating read may have returned a nil pointer.
    #[test]
    fn a_create_still_kills_same_name_accessors() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "y", false)),
                (id(11), field(2, "y", true)),
                (id(12), field(2, "y", false)),
            ],
            id(12),
        );
        let text = run_forward(&mut p);
        assert!(text.contains("%12 = get_field %2.y"), "{}", text);
    }

    /// ...but no longer kills accessors of *other* names, which denote
    /// provably different map entries.
    #[test]
    fn a_create_spares_other_names() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "y", false)),
                (id(11), field(2, "z", true)),
                (id(12), field(2, "y", false)),
            ],
            id(12),
        );
        let text = run_forward(&mut p);
        assert!(!text.contains("%12"), "the accessor should be reused:\n{}", text);
    }

    /// A field-create can flip an UnknownTable into an ObjectTable, after
    /// which a remembered get_index on it would error rather than repeat its
    /// NilPointer - so the cross-kill stays.
    #[test]
    fn a_field_create_kills_index_accessors() {
        let mut p = program_of(
            vec![
                (id(9), Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(1) }),
                (id(10), Instruction::GetIndex { receiver: id(2), index: id(9), create_if_missing: false }),
                (id(11), field(2, "z", true)),
                (id(12), Instruction::GetIndex { receiver: id(2), index: id(9), create_if_missing: false }),
            ],
            id(12),
        );
        let text = run_forward(&mut p);
        assert!(text.contains("%12 = get_index"), "{}", text);
    }

    /// A call whose callee provably is a heap-oblivious builtin invalidates
    /// nothing.
    #[test]
    fn an_oblivious_call_is_transparent() {
        let mut p = program_of(
            vec![
                (id(9), Instruction::GetGlobal { name: "error".to_string(), create_if_missing: false }),
                (id(10), Instruction::Load { source: id(9) }),
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
                (id(13), Instruction::Call { closure: id(10), args: vec![] }),
                (id(14), Instruction::Load { source: id(11) }),
            ],
            id(14),
        );
        let text = run_forward(&mut p);
        assert!(!text.contains("%14"), "the load should survive the error call:\n{}", text);
    }

    /// If anything anywhere in the program uses the oblivious global's cell
    /// other than loading it, the name could be shadowed and the exemption is
    /// off.
    #[test]
    fn a_shadowed_oblivious_global_is_opaque() {
        let mut p = program_of(
            vec![
                (id(9), Instruction::GetGlobal { name: "error".to_string(), create_if_missing: false }),
                (id(10), Instruction::Load { source: id(9) }),
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
                (id(13), Instruction::Call { closure: id(10), args: vec![] }),
                (id(14), Instruction::Load { source: id(11) }),
                // The shadowing store, after everything else so it changes
                // no availability itself: its provenance is Global("error"),
                // which does not alias the field cell.
                (id(15), Instruction::Store { target: id(9), source: id(2) }),
            ],
            id(14),
        );
        let text = run_forward(&mut p);
        assert!(text.contains("%14 = load %11"), "{}", text);
    }

    /// A call to anything else still kills.
    #[test]
    fn an_ordinary_call_still_kills_in_forward_mode() {
        let mut p = program_of(
            vec![
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
                (id(13), Instruction::Call { closure: id(2), args: vec![] }),
                (id(14), Instruction::Load { source: id(11) }),
            ],
            id(14),
        );
        let text = run_forward(&mut p);
        assert!(text.contains("%14 = load %11"), "{}", text);
    }

    /// Nothing forward mode finds crosses a block boundary - the live-range
    /// cost was measured to eat the win (see the note in `redundancies`).
    #[test]
    fn forward_mode_folds_stay_block_local() {
        let mut p = cfg_of(vec![
            (
                "__entry",
                vec![
                    (id(10), field(2, "x", false)),
                    (id(11), Instruction::Store { target: id(10), source: id(2) }),
                ],
                goto("b"),
            ),
            (
                "b",
                vec![(id(12), Instruction::Load { source: id(10) })],
                Terminator::Return { value: Some(id(12)) },
            ),
        ]);
        let text = run_forward(&mut p);
        assert!(text.contains("%12 = load %10"), "{}", text);
    }

    /// Two stores to the same cell: a load between them forwards from the
    /// first, a load after them from the second.
    #[test]
    fn a_second_store_replaces_the_first() {
        let mut p = program_of(
            vec![
                (id(9), Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(7) }),
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Store { target: id(10), source: id(2) }),
                (id(12), Instruction::Load { source: id(10) }),
                (id(13), Instruction::Store { target: id(10), source: id(9) }),
                (id(14), Instruction::Load { source: id(10) }),
            ],
            id(14),
        );
        let text = run_forward(&mut p);
        assert!(!text.contains("%12") && !text.contains("%14"), "{}", text);
        assert!(text.contains("return %9"), "{}", text);
    }
}
