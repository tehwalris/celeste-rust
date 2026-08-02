//! `decompose_truthy` - split a mixed `and`/`or` select cascade into
//! (truthiness, value) pairs of homogeneous selects.
//!
//! # The problem
//!
//! The early `if_convert`s left Lua's `and`/`or` idioms as *mixed* selects:
//! `c and y` became `select c ? y : c` (a Number on one side, a Bool on the
//! other) and `x or z` became `select x ? x : z` on operands that are
//! themselves mixed. `Select` carries one type tag per value, so such a
//! select executes only while its condition happens to be uniform per state -
//! which today is guaranteed by an *upstream splitting branch*. Every one of
//! these selects is a mine: remove that split and the select must combine a
//! Bool with a Number for real, and fails. This is what stopped the two
//! remaining `convert_ternary` sites.
//!
//! # The decomposition
//!
//! Any value in such a cascade is described exactly by a pair:
//!
//!   * `t` - is it truthy? (a Bool, or anything with the same truthiness)
//!   * `n` - its value *on the lanes where it is truthy* (a Number)
//!
//! with `n` arbitrary-but-well-typed elsewhere. The three node shapes become:
//!
//! ```text
//!   c and y   (y statically truthy)   t = c                        n = y
//!   x or z                            t = select t_x ? t_x : t_z   n = select t_x ? n_x : n_z
//!   plain k   (k statically truthy)   t = bool true                n = k
//! ```
//!
//! Every select is then Bool/Bool or Number/Number; the mixed forms never
//! exist. A phi over cascade values splits into a `t`-phi and an `n`-phi.
//!
//! # Soundness
//!
//! The invariant, by induction over the chain: `truthy(t_v) == truthy(v)` on
//! every lane, and `n_v == v` on every lane where `v` is truthy.
//!
//!   * `c and y`: if `c` is truthy the value is `y` (truthy, since the rule
//!     demands `y` statically truthy - this is the same fact `convert_ternary`
//!     rests on); if falsy, the value is `c` itself. So `t = c` matches, and
//!     where truthy the value is exactly `y = n`. If `y` could be falsy, `t`
//!     and `truthy(v)` would part ways, so the rule refuses.
//!   * `x or z`: truthy iff either side is, value is `x` where `x` is truthy,
//!     else `z` - which is exactly what the two selects compute from the
//!     children's pairs.
//!   * a phi edge carrying a statically truthy leaf: `t = true`, `n = it`.
//!   * a phi edge carrying a Bool: sound only when the value is provably
//!     falsy on that edge, which the rule checks structurally - the
//!     predecessor must branch on *exactly that value* and reach the phi on
//!     its false side. Then `t = it` (falsy, correct) and `n` is never read
//!     on this edge; a minted dead zero keeps the `n`-phi well-typed.
//!
//! The root must be `select x ? x : k` with `k` statically truthy: then the
//! whole value is never falsy, so it equals its own `n` on *every* lane, and
//! the rule can redefine the root's id as the `n`-select - consumers see the
//! identical value and nothing outside the chain is touched. Every interior
//! id must be consumed exactly once, by its parent; those ids die.
//!
//! Unlike a conversion, this changes no branching and runs nothing that did
//! not already run: on a chain with no phi it even deletes one instruction
//! per `and`. Its purpose is to defuse the mines so the splitting branches
//! that feed these cascades can be if-converted.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{FunDef, Instruction, Label, LocalId, Terminator};
use crate::pico8_num::Pico8Num;

use super::super::program::Program;
use super::super::validate::{block_label, BlockKey};
use super::convert_ternary::statically_truthy;
use super::{blocks_sorted, entry_label, get_block, require, LocalIdAllocator};

/// One value in the cascade, classified by its defining instruction. Interior
/// ids (the `Or`, `And` and `Phi` ids) die with the decomposition; everything
/// they reference stays.
pub enum Node {
    /// `select x ? x : z` - Lua's `x or z`.
    Or { id: LocalId, x: Box<Node>, z: Operand },
    /// `select c ? y : c` - Lua's `c and y`, with `y` statically truthy.
    And { id: LocalId, condition: LocalId, value: LocalId },
    /// A phi whose edges are all leaves.
    Phi { id: LocalId, block: Label, edges: Vec<(Label, PhiEdge)> },
}

/// The else-operand of an `Or`: another cascade node, or a statically truthy
/// value that stays in place.
pub enum Operand {
    Node(Box<Node>),
    Truthy(LocalId),
}

pub enum PhiEdge {
    /// Statically truthy: `t` is a minted true constant in the predecessor,
    /// `n` is the value itself.
    Truthy(LocalId),
    /// The predecessor branches on exactly this value and reaches the phi
    /// only on its false side: `t` is the value (falsy on this edge), `n` is
    /// a minted dead zero in the predecessor.
    ProvablyFalsy(LocalId),
}

impl Node {
    fn id(&self) -> LocalId {
        match self {
            Node::Or { id, .. } | Node::And { id, .. } | Node::Phi { id, .. } => *id,
        }
    }
}

fn definition(fun: &FunDef, id: LocalId) -> Option<(BlockKey, &Instruction)> {
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).unwrap();
        for (candidate, instr) in &block.instructions {
            if *candidate == id {
                return Some((key, instr));
            }
        }
    }
    None
}

fn node(fun: &FunDef, id: LocalId, visited: &mut FxHashSet<LocalId>) -> Result<Node> {
    require(
        visited.insert(id),
        format!("%{} appears twice in the cascade", usize::from(id)),
    )?;
    let (key, instr) = definition(fun, id)
        .ok_or_else(|| anyhow!("%{} is not defined by an instruction", usize::from(id)))?;
    match instr {
        Instruction::Select { condition, if_true, if_false } if condition == if_true => {
            let x = Box::new(node(fun, *condition, visited)?);
            let z = if statically_truthy(fun, *if_false) {
                Operand::Truthy(*if_false)
            } else {
                Operand::Node(Box::new(node(fun, *if_false, visited)?))
            };
            Ok(Node::Or { id, x, z })
        }
        Instruction::Select { condition, if_true, if_false } if condition == if_false => {
            require(
                statically_truthy(fun, *if_true),
                format!(
                    "%{} = `select %c ? %{} : %c` is an `and`, but its value is \
                     not statically truthy, so its truthiness would not be its \
                     condition's",
                    usize::from(id),
                    usize::from(*if_true)
                ),
            )?;
            Ok(Node::And { id, condition: *condition, value: *if_true })
        }
        Instruction::Phi { branches } => {
            let block = match &key {
                Some(l) => l.clone(),
                None => return Err(anyhow!("a phi cannot be in the entry block")),
            };
            let mut edges = Vec::new();
            for (label, value) in branches {
                let edge = if statically_truthy(fun, *value) {
                    PhiEdge::Truthy(*value)
                } else {
                    let pred_key =
                        if *label == entry_label() { None } else { Some(label.clone()) };
                    let pred = get_block(&fun.cfg, &pred_key).ok_or_else(|| {
                        anyhow!("phi %{} names a missing block", usize::from(id))
                    })?;
                    let falsy_here = matches!(
                        pred.terminator_kind(),
                        Terminator::ConditionalBranch { condition, true_target, false_target }
                            if condition == value
                                && false_target == &block
                                && true_target != &block
                    );
                    require(
                        falsy_here,
                        format!(
                            "the '{}' edge of phi %{} carries %{}, which is neither \
                             statically truthy nor provably falsy on that edge \
                             (the predecessor would have to branch on exactly it, \
                             reaching the phi only on the false side)",
                            label.as_str(),
                            usize::from(id),
                            usize::from(*value)
                        ),
                    )?;
                    PhiEdge::ProvablyFalsy(*value)
                };
                edges.push((label.clone(), edge));
            }
            Ok(Node::Phi { id, block, edges })
        }
        other => Err(anyhow!(
            "%{} = `{}` is neither an `and`/`or` select nor a phi, and is not \
             statically truthy",
            usize::from(id),
            super::super::print::format_instruction(other)
        )),
    }
}

fn interior_parents(node: &Node, out: &mut FxHashMap<LocalId, LocalId>) {
    match node {
        Node::Or { id, x, z } => {
            out.insert(x.id(), *id);
            interior_parents(x, out);
            if let Operand::Node(z) = z {
                out.insert(z.id(), *id);
                interior_parents(z, out);
            }
        }
        Node::And { .. } | Node::Phi { .. } => {}
    }
}

/// The whole cascade rooted at `root`, with every side condition checked.
/// Shared, meaning-level: `apply`, `verify` and `candidates` all re-derive
/// the same chain from the same before-program.
fn chain(fun: &FunDef, root: LocalId) -> Result<Node> {
    let (_, instr) = definition(fun, root)
        .ok_or_else(|| anyhow!("%{} is not defined by an instruction", usize::from(root)))?;
    let Instruction::Select { condition, if_true, if_false } = instr else {
        return Err(anyhow!(
            "the root %{} must be a select, found `{}`",
            usize::from(root),
            super::super::print::format_instruction(instr)
        ));
    };
    require(
        condition == if_true,
        format!("the root %{} must be `select x ? x : z` (an `or`)", usize::from(root)),
    )?;
    // The always-truthy fact that lets the root keep its id: with `z`
    // statically truthy the whole cascade can never be falsy, so its value is
    // its own `n` on every lane.
    require(
        statically_truthy(fun, *if_false),
        format!(
            "the root's else-value %{} is not statically truthy, so the cascade \
             could yield a falsy value that the decomposition cannot represent",
            usize::from(*if_false)
        ),
    )?;
    let mut visited = FxHashSet::default();
    visited.insert(root);
    let root_node = Node::Or {
        id: root,
        x: Box::new(node(fun, *condition, &mut visited)?),
        z: Operand::Truthy(*if_false),
    };

    // Every interior id must be consumed exactly once, by its parent. The
    // root's id is external - it survives - and leaves are unrestricted.
    let mut parents = FxHashMap::default();
    interior_parents(&root_node, &mut parents);
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).unwrap();
        for (id, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                if let Some(parent) = parents.get(&used) {
                    require(
                        id == parent,
                        format!(
                            "%{} is also used by %{} in '{}'; the decomposition \
                             must be able to delete it",
                            usize::from(used),
                            usize::from(*id),
                            block_label(&key)
                        ),
                    )?;
                }
            }
        }
        for used in block.terminator_kind().get_used_locals() {
            require(
                !parents.contains_key(&used),
                format!(
                    "%{} is also used by the terminator of '{}'",
                    usize::from(used),
                    block_label(&key)
                ),
            )?;
        }
    }
    Ok(root_node)
}

/// What `apply` builds while walking the chain: in-place replacements for the
/// interior nodes' instructions, and constants appended to predecessor blocks
/// for the leaf phi edges.
#[derive(Default)]
struct Emission {
    replacements: FxHashMap<LocalId, Vec<(LocalId, Instruction)>>,
    appends: FxHashMap<BlockKey, Vec<(LocalId, Instruction)>>,
}

fn true_const() -> Instruction {
    Instruction::BoolConstant { value: true }
}

fn dead_zero() -> Instruction {
    Instruction::NumberConstant { value: Pico8Num::from_i16(0) }
}

/// Emit a non-root node, returning its `(t, n)` ids.
fn emit(node: &Node, ids: &mut LocalIdAllocator, out: &mut Emission) -> (LocalId, LocalId) {
    match node {
        Node::And { id, condition, value } => {
            out.replacements.insert(*id, vec![]);
            (*condition, *value)
        }
        Node::Or { id, x, z } => {
            let (tx, nx) = emit(x, ids, out);
            let mut instrs = Vec::new();
            let (tz, nz) = match z {
                Operand::Node(z) => emit(z, ids, out),
                Operand::Truthy(v) => {
                    let tc = ids.fresh();
                    instrs.push((tc, true_const()));
                    (tc, *v)
                }
            };
            let t = ids.fresh();
            instrs.push((t, Instruction::Select { condition: tx, if_true: tx, if_false: tz }));
            let n = ids.fresh();
            instrs.push((n, Instruction::Select { condition: tx, if_true: nx, if_false: nz }));
            out.replacements.insert(*id, instrs);
            (t, n)
        }
        Node::Phi { id, block: _, edges } => {
            let mut t_edges = Vec::new();
            let mut n_edges = Vec::new();
            for (label, edge) in edges {
                let pred_key =
                    if *label == entry_label() { None } else { Some(label.clone()) };
                match edge {
                    PhiEdge::Truthy(v) => {
                        let tc = ids.fresh();
                        out.appends.entry(pred_key).or_default().push((tc, true_const()));
                        t_edges.push((label.clone(), tc));
                        n_edges.push((label.clone(), *v));
                    }
                    PhiEdge::ProvablyFalsy(v) => {
                        let zc = ids.fresh();
                        out.appends.entry(pred_key).or_default().push((zc, dead_zero()));
                        t_edges.push((label.clone(), *v));
                        n_edges.push((label.clone(), zc));
                    }
                }
            }
            let t = ids.fresh();
            let n = ids.fresh();
            out.replacements.insert(
                *id,
                vec![
                    (t, Instruction::Phi { branches: t_edges }),
                    (n, Instruction::Phi { branches: n_edges }),
                ],
            );
            (t, n)
        }
    }
}

pub fn apply(program: &mut Program, function: &str, root: LocalId) -> Result<usize> {
    let fun = program.get(function)?;
    let root_node = chain(fun, root)?;
    let mut ids = LocalIdAllocator::for_function(fun);
    let mut out = Emission::default();
    let Node::Or { id, x, z: Operand::Truthy(z) } = &root_node else {
        unreachable!("chain() only builds roots of this shape");
    };
    let (tx, nx) = emit(x, &mut ids, &mut out);
    out.replacements.insert(
        *id,
        vec![(*id, Instruction::Select { condition: tx, if_true: nx, if_false: *z })],
    );

    let count = out.replacements.len() + out.appends.values().map(Vec::len).sum::<usize>();
    let cfg = &mut program.get_mut(function)?.cfg;
    for key in blocks_sorted(cfg) {
        let touched = {
            let block = get_block(cfg, &key).unwrap();
            block.instructions.iter().any(|(id, _)| out.replacements.contains_key(id))
                || out.appends.contains_key(&key)
        };
        if !touched {
            continue;
        }
        let block = super::get_block_mut(cfg, &key).unwrap();
        let mut instructions = Vec::with_capacity(block.instructions.len());
        for (id, instr) in block.instructions.drain(..) {
            match out.replacements.remove(&id) {
                Some(replacement) => instructions.extend(replacement),
                None => instructions.push((id, instr)),
            }
        }
        if let Some(extra) = out.appends.remove(&key) {
            instructions.extend(extra);
        }
        block.instructions = instructions;
    }
    require(
        out.replacements.is_empty() && out.appends.is_empty(),
        "decompose_truthy failed to place every emitted instruction",
    )?;

    // Fresh ids exist; any slot allocation is stale.
    cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(count)
}

/// What `verify` reads out of the after program before checking consistency.
#[derive(Default)]
struct Extracted {
    /// Interior `Or`/`Phi` id -> the `(t, n)` ids found at its position.
    pairs: FxHashMap<LocalId, (LocalId, LocalId)>,
    /// Raw replacement instructions found at each interior node's position.
    instrs: FxHashMap<LocalId, Vec<(LocalId, Instruction)>>,
    /// Minted constants appended at the end of blocks: id -> (block, instr).
    consts: FxHashMap<LocalId, (BlockKey, Instruction)>,
    minted: Vec<LocalId>,
}

fn collect_nodes<'a>(node: &'a Node, out: &mut FxHashMap<LocalId, &'a Node>) {
    out.insert(node.id(), node);
    if let Node::Or { x, z, .. } = node {
        collect_nodes(x, out);
        if let Operand::Node(z) = z {
            collect_nodes(z, out);
        }
    }
}

/// How many instructions stand at this node's old position afterwards.
fn replacement_len(node: &Node, is_root: bool) -> usize {
    match node {
        Node::And { .. } => 0,
        Node::Or { .. } if is_root => 1,
        Node::Or { z: Operand::Truthy(_), .. } => 3,
        Node::Or { .. } => 2,
        Node::Phi { .. } => 2,
    }
}

/// Independent check: re-derives the cascade and its side conditions from the
/// *before* program, reads the minted ids out of the after program, and
/// insists every block is exactly the prescribed rewrite and nothing else
/// moved.
pub fn verify(before: &Program, after: &Program, function: &str, root: LocalId) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let root_node = chain(before_fun, root)?;

    let mut nodes: FxHashMap<LocalId, &Node> = FxHashMap::default();
    collect_nodes(&root_node, &mut nodes);

    // How many appended constants each block must have gained: one per leaf
    // phi edge from it.
    let mut expected_appends: FxHashMap<BlockKey, usize> = FxHashMap::default();
    for node in nodes.values() {
        if let Node::Phi { edges, .. } = node {
            for (label, _) in edges {
                let pred_key =
                    if *label == entry_label() { None } else { Some(label.clone()) };
                *expected_appends.entry(pred_key).or_default() += 1;
            }
        }
    }

    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len()
            && before_fun
                .cfg
                .named
                .keys()
                .all(|l| after_fun.cfg.named.contains_key(l)),
        "decompose_truthy changed the set of blocks",
    )?;

    // Pass 1: walk every block, matching untouched instructions exactly and
    // pulling the replacement instructions out by position.
    let mut found = Extracted::default();
    for key in blocks_sorted(&before_fun.cfg) {
        let before_block = get_block(&before_fun.cfg, &key).unwrap();
        let after_block = get_block(&after_fun.cfg, &key).unwrap();
        require(
            after_block.terminator == before_block.terminator,
            format!("decompose_truthy changed the terminator of '{}'", block_label(&key)),
        )?;
        let mut j = 0;
        for (id, instr) in &before_block.instructions {
            match nodes.get(id) {
                Some(node) => {
                    let len = replacement_len(node, *id == root);
                    let got = after_block.instructions.get(j..j + len).ok_or_else(|| {
                        anyhow!(
                            "'{}' ends before the replacement of %{}",
                            block_label(&key),
                            usize::from(*id)
                        )
                    })?;
                    found.instrs.insert(*id, got.to_vec());
                    j += len;
                }
                None => {
                    require(
                        after_block.instructions.get(j) == Some(&(*id, instr.clone())),
                        format!(
                            "'{}' does not keep %{} = `{}` in place",
                            block_label(&key),
                            usize::from(*id),
                            super::super::print::format_instruction(instr)
                        ),
                    )?;
                    j += 1;
                }
            }
        }
        // Whatever follows must be exactly the appended constants.
        let appended = &after_block.instructions[j..];
        require(
            appended.len() == expected_appends.get(&key).copied().unwrap_or(0),
            format!(
                "'{}' gained {} trailing instruction(s), expected {}",
                block_label(&key),
                appended.len(),
                expected_appends.get(&key).copied().unwrap_or(0)
            ),
        )?;
        for (id, instr) in appended {
            require(
                *instr == true_const() || *instr == dead_zero(),
                format!(
                    "'{}' gained `{}`, which is neither a true constant nor a dead zero",
                    block_label(&key),
                    super::super::print::format_instruction(instr)
                ),
            )?;
            found.consts.insert(*id, (key.clone(), instr.clone()));
            found.minted.push(*id);
        }
    }

    // Pass 2: check each node's replacement against the prescribed shape,
    // resolving child (t, n) pairs. Children are resolved before parents by
    // recursing first.
    fn check(
        node: &Node,
        is_root: bool,
        found: &mut Extracted,
    ) -> Result<(LocalId, LocalId)> {
        if let Node::And { id, condition, value } = node {
            require(
                found.instrs.get(id).is_some_and(Vec::is_empty),
                format!("the `and` %{} must be deleted", usize::from(*id)),
            )?;
            return Ok((*condition, *value));
        }
        if let Some(&pair) = found.pairs.get(&node.id()) {
            return Ok(pair);
        }
        let pair = match node {
            Node::And { .. } => unreachable!("handled above"),
            Node::Or { id, x, z } => {
                let (tx, nx) = check(x, false, found)?;
                let instrs = found.instrs.get(id).cloned().unwrap();
                let mut cursor = instrs.iter();
                let (tz, nz) = match z {
                    Operand::Node(z) => check(z, false, found)?,
                    Operand::Truthy(v) if is_root => (LocalId::from(usize::MAX), *v),
                    Operand::Truthy(v) => {
                        let (tc, instr) = cursor.next().ok_or_else(|| {
                            anyhow!("%{} lost its true constant", usize::from(*id))
                        })?;
                        require(
                            *instr == true_const(),
                            format!(
                                "the first replacement of %{} must be a true constant, \
                                 found `{}`",
                                usize::from(*id),
                                super::super::print::format_instruction(instr)
                            ),
                        )?;
                        found.minted.push(*tc);
                        (*tc, *v)
                    }
                };
                if is_root {
                    let want =
                        Instruction::Select { condition: tx, if_true: nx, if_false: nz };
                    require(
                        cursor.next() == Some(&(*id, want.clone())),
                        format!(
                            "the root must become %{} = `{}`",
                            usize::from(*id),
                            super::super::print::format_instruction(&want)
                        ),
                    )?;
                    (*id, *id)
                } else {
                    let want_t =
                        Instruction::Select { condition: tx, if_true: tx, if_false: tz };
                    let (t, got_t) = cursor.next().ok_or_else(|| {
                        anyhow!("%{} lost its truthiness select", usize::from(*id))
                    })?;
                    require(
                        *got_t == want_t,
                        format!(
                            "the truthiness select of %{} must be `{}`, found `{}`",
                            usize::from(*id),
                            super::super::print::format_instruction(&want_t),
                            super::super::print::format_instruction(got_t)
                        ),
                    )?;
                    let want_n =
                        Instruction::Select { condition: tx, if_true: nx, if_false: nz };
                    let (n, got_n) = cursor.next().ok_or_else(|| {
                        anyhow!("%{} lost its value select", usize::from(*id))
                    })?;
                    require(
                        *got_n == want_n,
                        format!(
                            "the value select of %{} must be `{}`, found `{}`",
                            usize::from(*id),
                            super::super::print::format_instruction(&want_n),
                            super::super::print::format_instruction(got_n)
                        ),
                    )?;
                    found.minted.push(*t);
                    found.minted.push(*n);
                    (*t, *n)
                }
            }
            Node::Phi { id, block: _, edges } => {
                let instrs = found.instrs.get(id).cloned().unwrap();
                let [(t, Instruction::Phi { branches: t_edges }), (n, Instruction::Phi { branches: n_edges })] =
                    &instrs[..]
                else {
                    return Err(anyhow!(
                        "phi %{} must become exactly a truthiness phi and a value phi",
                        usize::from(*id)
                    ));
                };
                require(
                    t_edges.len() == edges.len() && n_edges.len() == edges.len(),
                    format!("the split phis of %{} changed their edge count", usize::from(*id)),
                )?;
                for (i, (label, edge)) in edges.iter().enumerate() {
                    require(
                        t_edges[i].0 == *label && n_edges[i].0 == *label,
                        format!(
                            "the split phis of %{} reordered the '{}' edge",
                            usize::from(*id),
                            label.as_str()
                        ),
                    )?;
                    let pred_key =
                        if *label == entry_label() { None } else { Some(label.clone()) };
                    let phi_id = *id;
                    let minted_const = |const_id: LocalId, want: Instruction| -> Result<()> {
                        let found_const = found.consts.get(&const_id);
                        require(
                            found_const == Some(&(pred_key.clone(), want.clone())),
                            format!(
                                "the '{}' edge of the split phis of %{} must use a \
                                 `{}` minted in that predecessor",
                                label.as_str(),
                                usize::from(phi_id),
                                super::super::print::format_instruction(&want)
                            ),
                        )
                    };
                    match edge {
                        PhiEdge::Truthy(v) => {
                            minted_const(t_edges[i].1, true_const())?;
                            require(
                                n_edges[i].1 == *v,
                                format!(
                                    "the value phi of %{} must carry %{} on the '{}' \
                                     edge",
                                    usize::from(*id),
                                    usize::from(*v),
                                    label.as_str()
                                ),
                            )?;
                        }
                        PhiEdge::ProvablyFalsy(v) => {
                            require(
                                t_edges[i].1 == *v,
                                format!(
                                    "the truthiness phi of %{} must carry %{} on the \
                                     '{}' edge",
                                    usize::from(*id),
                                    usize::from(*v),
                                    label.as_str()
                                ),
                            )?;
                            minted_const(n_edges[i].1, dead_zero())?;
                        }
                    }
                }
                found.minted.push(*t);
                found.minted.push(*n);
                (*t, *n)
            }
        };
        found.pairs.insert(node.id(), pair);
        Ok(pair)
    }
    check(&root_node, true, &mut found)?;

    // The minted ids must be new and distinct.
    let mut seen = FxHashSet::default();
    for id in &found.minted {
        require(
            definition(before_fun, *id).is_none(),
            format!(
                "decompose_truthy reuses %{}, which the function already defines",
                usize::from(*id)
            ),
        )?;
        require(
            seen.insert(*id),
            format!("decompose_truthy minted %{} twice", usize::from(*id)),
        )?;
    }

    // Nothing else.
    require(
        before.functions.len() == after.functions.len(),
        "decompose_truthy changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("decompose_truthy on {} also changed {}", function, name.as_str()),
        )?;
    }
    Ok(())
}

/// Roots where the whole cascade qualifies. A qualifying root that is itself
/// interior to a larger qualifying cascade is dropped - the outer
/// decomposition consumes it.
pub fn candidates(program: &Program) -> Vec<(String, LocalId)> {
    let mut found: Vec<(String, LocalId, FxHashSet<LocalId>)> = Vec::new();
    for (name, fun) in &program.functions {
        for key in blocks_sorted(&fun.cfg) {
            let block = get_block(&fun.cfg, &key).unwrap();
            for (id, instr) in &block.instructions {
                let Instruction::Select { condition, if_true, .. } = instr else { continue };
                if condition != if_true {
                    continue;
                }
                if let Ok(root_node) = chain(fun, *id) {
                    let mut parents = FxHashMap::default();
                    interior_parents(&root_node, &mut parents);
                    found.push((
                        name.as_str().to_string(),
                        *id,
                        parents.keys().copied().collect(),
                    ));
                }
            }
        }
    }
    let mut out: Vec<(String, LocalId)> = found
        .iter()
        .filter(|(name, id, _)| {
            !found
                .iter()
                .any(|(other_name, _, interior)| other_name == name && interior.contains(id))
        })
        .map(|(name, id, _)| (name.clone(), *id))
        .collect();
    out.sort_by(|a, b| (&a.0, usize::from(a.1)).cmp(&(&b.0, usize::from(b.1))));
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, FunDef, GlobalId};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: Pico8Num::from_i16(n) }
    }

    fn cmp(left: usize, right: usize) -> Instruction {
        Instruction::BinaryOp {
            left: id(left),
            op: crate::ir::BinaryOp::GreaterThan,
            right: id(right),
        }
    }

    fn select(c: usize, t: usize, f: usize) -> Instruction {
        Instruction::Select { condition: id(c), if_true: id(t), if_false: id(f) }
    }

    fn fun_of(instructions: Vec<(LocalId, Instruction)>, ret: usize) -> Program {
        let cfg = Cfg::new(
            Block {
                instructions,
                terminator: (id(99), Terminator::Return { value: Some(id(ret)) }),
                hint_normalize: false,
            },
            crate::ir::new_label_map(),
        );
        let mut functions = IndexMap::new();
        functions.insert(
            GlobalId::from("f".to_string()),
            FunDef {
                name: GlobalId::from("f".to_string()),
                capture_ids: vec![],
                arg_ids: vec![Some(id(1)), Some(id(2))],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    /// The inlined `sign()` cascade: `v>0 and 1 or (v<0 and -1 or 0)`, as the
    /// early if_converts left it - three mixed selects and a root.
    fn sign_program() -> Program {
        fun_of(
            vec![
                (id(10), cmp(1, 2)),           // v > 0
                (id(11), num(1)),              // 1
                (id(12), select(10, 11, 10)),  // and: mixed
                (id(13), cmp(2, 1)),           // v < 0
                (id(14), num(-1)),             // -1
                (id(15), select(13, 14, 13)),  // and: mixed
                (id(16), select(12, 12, 15)),  // or: mixed
                (id(17), num(0)),              // 0
                (id(18), select(16, 16, 17)),  // root or: always truthy
            ],
            18,
        )
    }

    #[test]
    fn decomposes_the_sign_cascade_and_verifies() {
        let before = sign_program();
        let mut after = before.clone();
        // Interior nodes: two `and`s deleted, the inner `or` split, the root
        // redefined - 4 replacements, no appends.
        assert_eq!(apply(&mut after, "f", id(18)).unwrap(), 4);
        verify(&before, &after, "f", id(18)).unwrap();

        // One instruction fewer: 2 selects for the inner or, 1 for the root,
        // where before there were 4.
        let instrs = &after.get("f").unwrap().cfg.entry.instructions;
        assert_eq!(instrs.len(), before.get("f").unwrap().cfg.entry.instructions.len() - 1);
        // Every select in the result is homogeneous: conditions and t-selects
        // over bools (%10, %13), n-selects over numbers (%11, %14, %17).
        let root = instrs.iter().find(|(i, _)| *i == id(18)).unwrap();
        let Instruction::Select { condition, if_true, if_false } = root.1 else {
            panic!("root must stay a select");
        };
        assert_eq!(if_false, id(17));
        // The root's n-operand chain bottoms out in the and values.
        let n16 = instrs.iter().find(|(i, _)| *i == if_true).unwrap();
        assert!(
            matches!(
                n16.1,
                Instruction::Select { condition: c, if_true: t, if_false: f }
                    if c == id(10) && t == id(11) && f == id(14)
            ),
            "{:?}",
            n16.1
        );
        let t16 = instrs.iter().find(|(i, _)| *i == condition).unwrap();
        assert!(
            matches!(
                t16.1,
                Instruction::Select { condition: c, if_true: t, if_false: f }
                    if c == id(10) && t == id(10) && f == id(13)
            ),
            "{:?}",
            t16.1
        );
    }

    /// `-1` as an `and` value is `unary minus of a constant`: arithmetic
    /// produces a Number or fails loudly, and no number is falsy - the
    /// `and_or_join_199` arm.
    #[test]
    fn arithmetic_counts_as_truthy() {
        let before = fun_of(
            vec![
                (id(9), num(1)),
                (id(10), cmp(1, 2)),
                (id(11), Instruction::UnaryOp { op: crate::ir::UnaryOp::Minus, arg: id(9) }),
                (id(12), select(10, 11, 10)),
                (id(17), num(0)),
                (id(18), select(12, 12, 17)),
            ],
            18,
        );
        let mut after = before.clone();
        apply(&mut after, "f", id(18)).unwrap();
        verify(&before, &after, "f", id(18)).unwrap();
    }

    /// Without a statically truthy else-value the cascade can be falsy, and
    /// the root could not keep its id.
    #[test]
    fn refuses_a_root_without_a_truthy_else() {
        let mut p = fun_of(
            vec![
                (id(10), cmp(1, 2)),
                (id(11), num(1)),
                (id(12), select(10, 11, 10)),
                (id(13), cmp(2, 1)),
                (id(18), select(12, 12, 13)), // else-value is a bool
            ],
            18,
        );
        let error = apply(&mut p, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("not statically truthy"), "{}", error);
    }

    /// An `and` whose value could be falsy breaks the invariant that `t`
    /// mirrors the original truthiness.
    #[test]
    fn refuses_an_and_with_a_falsy_value() {
        let mut p = fun_of(
            vec![
                (id(10), cmp(1, 2)),
                (id(11), Instruction::BoolConstant { value: true }), // falsy-capable class
                (id(12), select(10, 11, 10)),
                (id(17), num(0)),
                (id(18), select(12, 12, 17)),
            ],
            18,
        );
        let error = apply(&mut p, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("not statically truthy"), "{}", error);
    }

    /// An interior value with a use outside the cascade cannot be deleted.
    #[test]
    fn refuses_an_interior_id_with_an_outside_use() {
        let mut p = sign_program();
        p.get_mut("f").unwrap().cfg.entry.instructions.push((
            id(20),
            Instruction::UnaryOp { op: crate::ir::UnaryOp::Not, arg: id(16) },
        ));
        let error = apply(&mut p, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("also used"), "{}", error);
    }

    /// The `or`'s truthy side must be a cascade node, not a bare bool - its
    /// value would escape as a Bool on the lanes where it is truthy.
    #[test]
    fn refuses_a_bool_as_an_or_operand() {
        let mut p = fun_of(
            vec![
                (id(10), cmp(1, 2)),
                (id(17), num(0)),
                (id(18), select(10, 10, 17)), // bool or 0
            ],
            18,
        );
        let error = apply(&mut p, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("neither an `and`/`or` select nor a phi"), "{}", error);
    }

    /// The tail-site shape: a triangle's phi mixing the branch condition with
    /// a number, feeding an `or` cascade.
    ///
    /// ```text
    ///   entry: %10 = %1 > %2; br %10 ? arm : join
    ///   arm:   %11 = 1; br join
    ///   join:  %12 = phi [__entry: %10, arm: %11]
    ///          %13 = %1 < %2; %14 = -1; %15 = select %13 ? %14 : %13
    ///          %16 = select %12 ? %12 : %15
    ///          %17 = 0;       %18 = select %16 ? %16 : %17
    /// ```
    fn phi_program() -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("arm"),
            Block {
                instructions: vec![(id(11), num(1))],
                terminator: (id(30), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("join"),
            Block {
                instructions: vec![
                    (
                        id(12),
                        Instruction::Phi {
                            branches: vec![(label("__entry"), id(10)), (label("arm"), id(11))],
                        },
                    ),
                    (id(13), cmp(2, 1)),
                    (id(14), num(-1)),
                    (id(15), select(13, 14, 13)),
                    (id(16), select(12, 12, 15)),
                    (id(17), num(0)),
                    (id(18), select(16, 16, 17)),
                ],
                terminator: (id(31), Terminator::Return { value: Some(id(18)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(id(10), cmp(1, 2))],
                terminator: (
                    id(32),
                    Terminator::ConditionalBranch {
                        condition: id(10),
                        true_target: label("arm"),
                        false_target: label("join"),
                    },
                ),
                hint_normalize: false,
            },
            named,
        );
        let mut functions = IndexMap::new();
        functions.insert(
            GlobalId::from("f".to_string()),
            FunDef {
                name: GlobalId::from("f".to_string()),
                capture_ids: vec![],
                arg_ids: vec![Some(id(1)), Some(id(2))],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    #[test]
    fn decomposes_across_a_phi_and_verifies() {
        let before = phi_program();
        let mut after = before.clone();
        // Replacements: the phi, the and, the or, the root; appends: a dead
        // zero in the entry, a true constant in the arm.
        assert_eq!(apply(&mut after, "f", id(18)).unwrap(), 6);
        verify(&before, &after, "f", id(18)).unwrap();

        let fun = after.get("f").unwrap();
        // The entry gained the dead zero for the falsy edge.
        let entry_tail = fun.cfg.entry.instructions.last().unwrap();
        assert_eq!(entry_tail.1, dead_zero());
        // The arm gained the true constant for the truthy edge.
        let arm_tail = fun.cfg.named.get(&label("arm")).unwrap().instructions.last().unwrap();
        assert_eq!(arm_tail.1, true_const());
        // The join starts with the two split phis: truthiness carries the
        // branch condition on the fall-through edge, value carries the dead
        // zero there.
        let join = fun.cfg.named.get(&label("join")).unwrap();
        let Instruction::Phi { branches: t_edges } = &join.instructions[0].1 else {
            panic!("expected the truthiness phi first");
        };
        assert_eq!(t_edges[0], (label("__entry"), id(10)));
        assert_eq!(t_edges[1].0, label("arm"));
        assert_eq!(t_edges[1].1, arm_tail.0);
        let Instruction::Phi { branches: n_edges } = &join.instructions[1].1 else {
            panic!("expected the value phi second");
        };
        assert_eq!(n_edges[0].0, label("__entry"));
        assert_eq!(n_edges[0].1, entry_tail.0);
        assert_eq!(n_edges[1], (label("arm"), id(11)));
    }

    /// A phi edge that carries a bool the predecessor does *not* branch to us
    /// on cannot be proven falsy, and its `n` half would be read for real.
    #[test]
    fn refuses_a_phi_edge_without_a_falsity_proof() {
        let mut p = phi_program();
        // Point the entry's branch at a different condition, so %10 on the
        // fall-through edge is no longer provably falsy.
        let f = p.get_mut("f").unwrap();
        f.cfg.entry.instructions.push((id(20), cmp(2, 1)));
        f.cfg.entry.terminator = (
            id(32),
            Terminator::ConditionalBranch {
                condition: id(20),
                true_target: label("arm"),
                false_target: label("join"),
            },
        );
        let error = apply(&mut p, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("provably falsy"), "{}", error);
    }

    /// The verifier must notice a wrong operand in a rebuilt select - the
    /// consistency of minted ids is the whole point of pass 2.
    #[test]
    fn verify_rejects_a_tampered_select() {
        let before = sign_program();
        let mut after = before.clone();
        apply(&mut after, "f", id(18)).unwrap();
        let instrs = &mut after.get_mut("f").unwrap().cfg.entry.instructions;
        let root = instrs.iter_mut().find(|(i, _)| *i == id(18)).unwrap();
        let Instruction::Select { if_false, .. } = &mut root.1 else { unreachable!() };
        *if_false = id(11); // the wrong number - types still fine, value wrong
        let error = verify(&before, &after, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("root must become"), "{}", error);
    }

    /// The verifier must notice an interior node left behind.
    #[test]
    fn verify_rejects_an_unremoved_and() {
        let before = sign_program();
        let after = before.clone(); // apply never ran
        let error = verify(&before, &after, "f", id(18)).unwrap_err().to_string();
        assert!(error.contains("does not keep"), "{}", error);
    }

    /// `candidates` reports the outermost root only.
    #[test]
    fn candidates_reports_the_root_once() {
        let p = sign_program();
        assert_eq!(candidates(&p), vec![("f".to_string(), id(18))]);
    }
}
