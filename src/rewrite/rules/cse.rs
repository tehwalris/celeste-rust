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
fn substitution(cfg: &Cfg) -> FxHashMap<LocalId, LocalId> {
    let layout = Layout::of(cfg);
    let mut subst: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    loop {
        let found = redundancies(&layout, &subst);
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
            let Some(key) = key_of(instr, &resolve) else { continue };
            universe.bit_of.insert(*id, universe.pairs.len());
            universe
                .by_key
                .entry(key.clone())
                .or_default()
                .push(universe.pairs.len());
            universe.pairs.push((key, *id));
        }
    }
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

    // Summarise each block as `out = (in & mask) | gen`. Valid because every
    // kill is a constant mask, so a sequence of them composes into one.
    let count = layout.blocks.len();
    let mut mask: Vec<Bits> = vec![Bits::new(); count];
    let mut gen: Vec<Bits> = vec![Bits::new(); count];
    for &b in &layout.order {
        let Some(block) = layout.blocks[b] else { continue };
        let mut m = bits_full(bits);
        let mut g = bits_empty(bits);
        for (id, instr) in live_instructions(block, subst) {
            // Barriers first, so an instruction never matches something its own
            // effects invalidated - a `create` accessor forgets itself.
            apply_barriers(&mut m, &universe, instr);
            apply_barriers(&mut g, &universe, instr);
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
            apply_barriers(&mut state, &universe, instr);
            let Some(&bit) = universe.bit_of.get(id) else { continue };
            let candidates = &universe.by_key[&universe.pairs[bit].0];
            if let Some(&other) = candidates
                .iter()
                .find(|&&c| c != bit && bit_get(&state, c))
            {
                found.insert(*id, universe.pairs[other].1);
            }
            bit_set(&mut state, bit);
        }
    }
    found
}

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        let subst = substitution(&fun.cfg);
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
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "cse changed the set of functions",
    )?;

    for (name, before_fun) in &before.functions {
        let after_fun = after.get(name.as_str())?;
        let subst = substitution(&before_fun.cfg);
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
        apply(p).unwrap();
        verify(&before, p).unwrap();
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
        assert_eq!(apply(&mut p).unwrap(), 0);
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
        assert!(verify(&before, &p).is_err());
    }
}
