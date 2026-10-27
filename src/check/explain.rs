//! WHY A KERNEL DECLINES A ROW (`rewrite explain`, the decline report's
//! command): the row's kernel REBUILT as the registry builds it (the walk's
//! frame, its slots and symbol; the text's digest checked against the
//! running kernel's, `Kernels::digest_of`), its fused graph evaluated on the
//! row (`eval::eval`, one lane), and per body that declines it (`live` and
//! `error` both may hold, as the kernel reads them) the error's disjuncts
//! that are not false, each followed down to its first undecided condition,
//! and its FAILING premises.

use std::collections::HashMap;
use std::fmt::Write as _;

use anyhow::{Context, Result};
use celeste_engine::runtime2::{Rt2, AV};

use crate::compiled::{KernelLanes, Kernels};
use crate::search::tree::TreeDir;
use crate::settings::Run;
use crate::storage::layer::Tree;
use crate::storage::Sk;
use crate::transpile::eval::{Lanes, Val};
use crate::transpile::graph::{Graph, NodeId, Op};

/// State `state` of the tree `t` (a state of its layers through step
/// `step`, where the forward ran it) explained: `explain_row` of its row.
pub fn explain<N: KernelLanes>(t: &TreeDir, step: u32, state: Sk) -> Result<String> {
    let tree = Tree::<N>::open(&t.dir, step)?;
    let f = tree.layer_of(state, step)?.ok_or_else(|| anyhow::anyhow!("state {}: in no layer of {} through step {step}", state.show(), t.dir.display()))?;
    let row = tree.row(f, state, t.run.settings.room)?;
    explain_row::<N>(&t.run, &row)
}

/// Why `run`'s kernels decline the one-row block `row` (as the forward
/// decodes it), as text: per declining body its error's disjuncts and
/// failing premises; that none declines, if none does.
pub fn explain_row<N: KernelLanes>(run: &Run, row: &Rt2<N>) -> Result<String> {
    anyhow::ensure!(row.width == 1, "explain one row, not {}", row.width);
    let cell = crate::storage::cells::block_cells(row)?[0];
    let region = run.settings.region_grid().filter(|_| !row.player_objects().is_empty()).and_then(|g| g.of_cell(cell));
    let key = (row.shape_hash(), region);
    let kernels = Kernels::<N>::new(run)?;
    let digest = kernels.digest_of(key).ok_or_else(|| anyhow::anyhow!("shape {:#x} region {region:?}: no kernel of the set ({}) takes the row: a miss, not a decline", row.shape_hash(), run.level))?;
    // The walk recurses deeply (as the kernel build does).
    std::thread::scope(|sc| {
        std::thread::Builder::new()
            .stack_size(256 << 20)
            .spawn_scoped(sc, || rebuilt_explains::<N>(run, row, key, digest))
            .context("spawn the explanation")?
            .join()
            .map_err(|_| anyhow::anyhow!("the explanation panicked"))?
    })
}

/// `explain_row`'s rebuild and evaluation (a big-stack thread).
fn rebuilt_explains<N: KernelLanes>(run: &Run, row: &Rt2<N>, key: (u64, Option<crate::trace::walk::Region>), digest: u64) -> Result<String> {
    let refs = crate::trace::lower::lattice_kernel_refs::<N>(run).context("retracing the start room for the explanation")?;
    let si = refs.iter().position(|(region, r)| (r.frame.in_rt2.shape_hash(), *region) == key).ok_or_else(|| anyhow::anyhow!("the walk traced no frame of shape {:#x} region {:?}", key.0, key.1))?;
    let r = &refs[si].1;
    let (fused, bodies) = (&r.lowered.fused, &r.lowered.bodies);
    let (flat_roots, _) = crate::compiled::asm_kernel::root_slots(bodies);
    let sym = crate::compiled::asm_kernel::kernel_sym(key.0, si);
    let compiled = crate::transpile::asm::compile(fused, &flat_roots, &sym, false)?;
    anyhow::ensure!(compiled.text_digest == digest, "{sym}: the rebuilt kernel's text ({:016x}) is not the running one's ({digest:016x})", compiled.text_digest);
    let mut cells: HashMap<u32, Val<N>> = HashMap::new();
    for &c in &compiled.input_cells {
        let v = match row.cols[c as usize].at(0) {
            AV::Num(p) => Val::Num(N::point(p)),
            AV::Ival(a, b) => Val::Num(N::ival(a, b)),
            AV::Bool(b) => Val::Bool(Some(b)),
            AV::UBool => Val::Bool(None),
            _ => continue,
        };
        cells.insert(c, v);
    }
    let vals = crate::transpile::eval::eval(fused, &cells, Lanes::One, Some(row.room));
    // As the kernel reads the masks: where they MAY hold.
    let may = |n: NodeId| vals[n as usize] != Val::Bool(Some(false));
    let mut out = String::new();
    let mut declined = 0;
    for (bi, b) in bodies.iter().enumerate().filter(|(_, b)| may(b.roots.live) && may(b.roots.error)) {
        declined += 1;
        writeln!(out, "[explain] {sym} body {bi} (outcome {}, forks {:?}) declines the row: live {:?}, error {:?}", b.outcome, b.splits, vals[b.roots.live as usize], vals[b.roots.error as usize])?;
        explain_error(fused, &vals, b.roots.error, &mut out)?;
    }
    if declined == 0 {
        writeln!(out, "[explain] {sym}: no body declines the row ({} bodies)", bodies.len())?;
    }
    Ok(out)
}

/// Why `error_node` holds under `vals`: its disjuncts not false, each
/// followed down to its first undecided condition, and its failing
/// premises - a bound's check named by the `Restrict` it checks (its
/// assumption on the same operand: `trace::error`'s own error of a
/// `Restrict`).
fn explain_error<N: KernelLanes>(g: &Graph<N>, vals: &[Val<N>], error_node: NodeId, out: &mut String) -> std::fmt::Result {
    let why = crate::transpile::why::Explain { g, vals: Some(vals) };
    let val = |n: NodeId| vals[n as usize];
    // Per operand, the assumptions on it.
    let mut restricts: HashMap<NodeId, Vec<NodeId>> = HashMap::new();
    for n in 0..g.len() as NodeId {
        if matches!(g.get(n).op, Op::Restrict(..)) {
            restricts.entry(g.get(n).args[0]).or_default().push(n);
        }
    }
    let operand = |a: NodeId| match g.get(a).op {
        Op::Lo | Op::Hi => g.get(a).args[0],
        _ => a,
    };
    for n in why.disjuncts(error_node).into_iter().filter(|&n| val(n) != Val::Bool(Some(false))).take(6) {
        writeln!(out, "[explain]   error disjunct {}", why.with_args(n))?;
        // Down Sels (and undecided booleans) to the first undecided condition.
        let chain = why.chain(n, 12, |cur| {
            let nd = g.get(cur);
            match nd.op {
                Op::Sel => match val(nd.args[0]) {
                    Val::Bool(Some(b)) => Some(nd.args[if b { 1 } else { 2 }]),
                    _ => None,
                },
                Op::Le | Op::Ge | Op::Lt | Op::Gt | Op::Eq | Op::Same | Op::ForkOk(..) => nd.args.iter().copied().find(|&a| g.get(a).op == Op::Sel),
                Op::And | Op::Or | Op::Not => nd.args.iter().copied().find(|&a| val(a) == Val::Bool(None)),
                _ => None,
            }
        });
        let end = chain[chain.len() - 1];
        if chain.len() <= 12 {
            let nd = g.get(end);
            if nd.op == Op::Sel {
                // Its condition is undecided, and one level into the comparison's operands.
                let c = nd.args[0];
                writeln!(out, "[explain]     undecided condition {}", why.with_args(c))?;
                for &a in g.get(c).args.iter().filter(|&&a| g.get(a).op == Op::Sel) {
                    writeln!(out, "[explain]       operand {}", why.with_args(a))?;
                }
            } else {
                writeln!(out, "[explain]     undecided at {}", why.with_args(end))?;
                writeln!(out, "[explain]       = {}", why.tree(end, 6))?;
            }
        }
        // The FAILING premise: false conjuncts through the strict merges
        // down to the leaves decided false.
        let mut stack = vec![n];
        let mut walked = std::collections::HashSet::new();
        let mut leaves = 0;
        while let Some(x) = stack.pop() {
            if !walked.insert(x) || leaves >= 8 {
                continue;
            }
            let nd = g.get(x);
            match nd.op {
                Op::And | Op::Or => stack.extend(nd.args.iter().copied().filter(|&a| val(a) == Val::Bool(Some(false)))),
                Op::Sel => {
                    if let Val::Bool(Some(b)) = val(nd.args[0]) {
                        stack.push(nd.args[if b { 1 } else { 2 }]);
                    }
                }
                _ => {
                    leaves += 1;
                    writeln!(out, "[explain]     failing premise {}", why.with_args(x))?;
                    if matches!(nd.op, Op::Lt | Op::Le | Op::Gt | Op::Ge) {
                        for r in nd.args.iter().filter_map(|&a| restricts.get(&operand(a))).flatten() {
                            writeln!(out, "[explain]       the bound it checks: {}", why.with_args(*r))?;
                        }
                    }
                    // A `Known` premise: its undecided operands.
                    if nd.op == Op::Known {
                        let mut todo = vec![(nd.args[0], 0usize)];
                        let mut shown = std::collections::HashSet::new();
                        while let Some((y, depth)) = todo.pop() {
                            if depth > 6 || !shown.insert(y) || val(y) != Val::Bool(None) {
                                continue;
                            }
                            writeln!(out, "[explain]       undecided {}{}", "  ".repeat(depth), why.with_args(y))?;
                            todo.extend(g.get(y).args.iter().map(|&a| (a, depth + 1)));
                        }
                    }
                }
            }
        }
    }
    Ok(())
}
