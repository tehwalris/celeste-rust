//! THE KERNEL CENSUS (`bench frame --metrics`, plans/devloop.md: the exact
//! metrics): per kernel, at build time (`report`), its static counts - the
//! traced and fused graphs' nodes by kind, the roots by role, the guards'
//! terms, and its machine instructions by CATEGORY - and the slices it
//! ran. A kernel is straight-line code, so its dynamic instruction count is
//! exactly slices x its static count (call-outs' callees aside). Nothing in
//! the search reads it.
//!
//! CATEGORIES (of a machine instruction, by the SSA instruction it came
//! from: `Compiled::prov`): `a.*` numeric work on values; `b.*` boolean /
//! mask / guard work (compares, logic, selects, the mask-to-vector
//! conversions the vector-mask booleans need); `c.*` data movement and
//! overhead (input loads, constant broadcasts, spills and reloads, register
//! moves, call-outs, the frame); `d.*` the stores of the roots (fields,
//! error, live, transfer).
//!
//! ROLE CLASS (of a root, by what it is): `value` (a field or transfer
//! root), `guard-E` / `guard-L` / `guard-EL` (a body's error / live root),
//! `shared` (both).

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::Result;

use crate::transpile::asm::Compiled;
use crate::transpile::graph::{Graph, NodeId, Op};

/// A root's roles (bits).
pub(crate) const ROLE_FIELD: u8 = 1;
pub(crate) const ROLE_ERROR: u8 = 2;
pub(crate) const ROLE_LIVE: u8 = 4;
pub(crate) const ROLE_ARC: u8 = 8;

/// One kernel's census (`report`): its static counts, `key -> value`.
pub type Census = BTreeMap<String, u64>;

/// A kernel built with the census on: its symbol, slice counter and census.
type Counted = (String, Arc<AtomicU64>, Census);

/// Every kernel built with the census on (a process-wide stats sink, like
/// the dispatch's hit counters).
static KERNELS: Mutex<Vec<Counted>> = Mutex::new(Vec::new());

/// A fresh slice counter for kernel `sym`, with its census.
pub(crate) fn counter(sym: &str, census: Census) -> Arc<AtomicU64> {
    let c = Arc::new(AtomicU64::new(0));
    KERNELS.lock().unwrap().push((sym.to_string(), c.clone(), census));
    c
}

/// Every kernel built so far: its symbol, the slices it ran, its census
/// (`insts` the static instruction count), by symbol.
pub fn kernel_metrics() -> Vec<(String, u64, Census)> {
    let mut v: Vec<(String, u64, Census)> = KERNELS.lock().unwrap().iter().map(|(s, c, m)| (s.clone(), c.load(Ordering::Relaxed), m.clone())).collect();
    v.sort_by(|a, b| a.0.cmp(&b.0));
    v
}

/// Zero every kernel's slice counter (a bench's warm-up done).
pub fn reset_slices() {
    for (_, c, _) in KERNELS.lock().unwrap().iter() {
        c.store(0, Ordering::Relaxed);
    }
}

/// A graph op's KIND, for the metrics: `leaf` (literals, cells, unknowns),
/// `num` (number -> number, an interval's ends, a fork's fragments), `cmp`
/// (number -> bool), `bool` (bool -> bool, the errors' validity tests),
/// `sel`, `restrict`, `call` (a cart lookup: a call-out in the kernel).
pub(crate) fn op_kind(op: &Op) -> &'static str {
    match op {
        Op::Const(..) | Op::ConstBool(_) | Op::Cell(_) | Op::UnknownNum | Op::UnknownBool(_) => "leaf",
        Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Same => "cmp",
        Op::Not | Op::And | Op::Or | Op::Known | Op::SplitValid(_) | Op::FragOk(_) | Op::SplitOk(_) | Op::NoWrap => "bool",
        Op::RSplitValid(_) | Op::RFragOk(_) | Op::RSplitOk(_) => "bool",
        Op::Sel => "sel",
        Op::Restrict(..) | Op::Pin(_) => "restrict",
        Op::Mget | Op::TileFlagAt => "call",
        Op::Split(_) | Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Flr | Op::Sin | Op::Min | Op::Max | Op::Span | Op::Frag(_) | Op::SplitInt(_) | Op::IntFrag(_) | Op::Lo | Op::Hi => "num",
        Op::RSplit(_) | Op::RFrag(_) => "num",
    }
}

/// One kernel's inputs to the census.
pub struct Input<'a, A: crate::transpile::garith::GraphArith = celeste_core::arith::Fixed> {
    /// The traced frame's graph, before specialization and fusion.
    pub traced: &'a Graph<A>,
    pub fused: &'a Graph<A>,
    /// The distinct root slots and each one's roles (bits).
    pub roots: &'a [NodeId],
    pub roles: &'a [u8],
    /// Per body, its (error, live) root nodes.
    pub bodies: &'a [(NodeId, NodeId)],
    pub compiled: &'a Compiled,
}

fn class(m: u8) -> &'static str {
    let guard = m & (ROLE_ERROR | ROLE_LIVE);
    let value = m & (ROLE_FIELD | ROLE_ARC);
    match (value != 0, guard) {
        (true, 0) => "value",
        (true, _) => "shared",
        (false, ROLE_ERROR) => "guard-E",
        (false, ROLE_LIVE) => "guard-L",
        (false, _) if guard != 0 => "guard-EL",
        _ => "none",
    }
}

/// The disjuncts of an OR tree (or the conjuncts of an AND tree).
fn spread(g: &Graph, root: NodeId, op: &Op, out: &mut Vec<NodeId>) {
    let mut stack = vec![root];
    while let Some(n) = stack.pop() {
        let node = g.get(n);
        if &node.op == op {
            stack.extend(node.args.iter().copied());
        } else if node.op != Op::ConstBool(*op == Op::And) {
            out.push(n);
        }
    }
}

fn category(mn: &str, ops: &str, kind: &str, node_op: &Op, store_role: &str) -> String {
    let reload = mn == "vmovdqu64" && ops.contains("(%rsp), %zmm");
    let spill = mn == "vmovdqu64" && ops.starts_with("%zmm") && ops.ends_with("(%rsp)");
    let input = mn == "vmovdqu64" && ops.contains("(%r13)");
    let c = match kind {
        "call" => "c.callout",
        _ if reload => "c.reload",
        _ if spill => "c.spill",
        _ if input => {
            if kind == "load" {
                "c.load"
            } else {
                "c.remat"
            }
        }
        // A constant, at its def or rematerialized at a use (`Emitter::constant`).
        _ if mn == "vpbroadcastd" => "c.const",
        _ if mn == "vpxord" && ops.split(", ").all(|o| Some(o) == ops.split(", ").next()) => "c.const",
        _ if mn == "vmovdqa64" => "c.move",
        "store" | "storemask" => return format!("d.store-{store_role}"),
        "load" | "loadmask" => "c.load",
        "bcast" => "c.const",
        "blendimm" => {
            if mn == "vpblendmd" {
                "a.num"
            } else {
                "c.kset"
            }
        }
        "cmp" => {
            if mn == "vpcmpd" {
                "b.cmp"
            } else {
                "b.m2v"
            }
        }
        "sel" => {
            if *node_op == Op::Sel {
                "b.select"
            } else {
                "a.num"
            }
        }
        "ternlog" | "and" | "or" | "xor" | "andn" | "not" => "b.logic",
        _ => "a.num",
    };
    c.to_string()
}

pub(crate) fn report(inp: &Input) -> Result<Census> {
    let (g, comp) = (inp.fused, inp.compiled);
    let n = g.len();
    let mut t: Census = BTreeMap::new();
    let bump = |t: &mut Census, k: String, v: u64| *t.entry(k).or_default() += v;

    // Roles, top-down (operands have smaller ids): the nodes the roots reach.
    let mut role = vec![0u8; n];
    let mut root_role: std::collections::HashMap<NodeId, u8> = Default::default();
    for (&r, &m) in inp.roots.iter().zip(inp.roles) {
        role[r as usize] |= m;
        *root_role.entry(r).or_default() |= m;
    }
    for id in (0..n).rev() {
        let m = role[id];
        if m != 0 {
            for &a in &g.get(id as NodeId).args {
                role[a as usize] |= m;
            }
        }
    }
    for id in (0..n).filter(|&id| role[id] != 0) {
        bump(&mut t, "nodes".into(), 1);
        bump(&mut t, format!("kind.{}", op_kind(&g.get(id as NodeId).op)), 1);
    }
    bump(&mut t, "fused_nodes".into(), n as u64);
    bump(&mut t, "traced.nodes".into(), inp.traced.len() as u64);
    for id in 0..inp.traced.len() {
        bump(&mut t, format!("traced.kind.{}", op_kind(&inp.traced.get(id as NodeId).op)), 1);
    }
    // The guards: each body's error root an OR of terms, its live root an
    // AND of conjuncts.
    bump(&mut t, "bodies".into(), inp.bodies.len() as u64);
    let distinct = |f: &dyn Fn(&(NodeId, NodeId)) -> NodeId| inp.bodies.iter().map(f).collect::<std::collections::HashSet<_>>().len() as u64;
    bump(&mut t, "bodies.distinct_error".into(), distinct(&|b| b.0));
    bump(&mut t, "bodies.distinct_live".into(), distinct(&|b| b.1));
    for &(err, live) in inp.bodies {
        let (mut terms, mut conj) = (Vec::new(), Vec::new());
        spread(g, err, &Op::Or, &mut terms);
        spread(g, live, &Op::And, &mut conj);
        bump(&mut t, "error_terms".into(), terms.len() as u64);
        bump(&mut t, "live_conjuncts".into(), conj.len() as u64);
    }
    bump(&mut t, "roots".into(), inp.roots.len() as u64);
    for &m in inp.roles {
        bump(&mut t, format!("rootrole.{}", class(m)), 1);
    }
    bump(&mut t, "ssa".into(), comp.prov.len() as u64);
    bump(&mut t, "spill_slots".into(), comp.spill_slots as u64);

    // The machine instructions, each after its SSA instruction's `#@k` line.
    let mut cur: Option<usize> = None;
    let mut started = false;
    for line in comp.asm.lines() {
        if line.starts_with(".section") {
            break;
        }
        if let Some(k) = line.strip_prefix("#@") {
            started = true;
            cur = k.parse().ok();
            continue;
        }
        let Some(body) = line.strip_prefix("    ") else { continue };
        if body.starts_with('.') {
            continue;
        }
        let (mn, ops) = body.split_once(' ').unwrap_or((body, ""));
        let cat = match cur {
            Some(k) if started => {
                let (node, kind) = comp.prov[k];
                let rr = root_role.get(&node).copied().unwrap_or(0);
                let store_role = if rr & ROLE_FIELD != 0 {
                    "field"
                } else if rr & ROLE_ARC != 0 {
                    "arc"
                } else if rr & ROLE_ERROR != 0 {
                    "error"
                } else {
                    "live"
                };
                category(mn, ops, kind, &g.get(node).op, store_role)
            }
            // The prologue, and a started `#@end`'s epilogue.
            _ => "c.frame".to_string(),
        };
        bump(&mut t, "insts".into(), 1);
        bump(&mut t, format!("cat.{cat}"), 1);
    }
    Ok(t)
}
