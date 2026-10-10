//! `CELESTE_KERNEL_MIX=DIR`: what the assembled kernels spend their
//! instructions on (plans/kernel-mix.md). A diagnostic only: nothing in the
//! search reads it.
//!
//! Per kernel, at build time (`report`), into `DIR`:
//! - `<sym>.s`, `<sym>.so`: the assembly with a `#@k` line before the lines
//!   of SSA instruction `k`, and the object it assembled to;
//! - `<sym>.lines`: one row per machine instruction, in text order (so in
//!   `objdump` order): its category, the role class of the graph node it
//!   came from, that node's guard family, its op, and whether bit identities
//!   remove it (`codegen::foldable`) - what a `perf` profile is mapped by;
//! - `<sym>.tsv`: the static counts (`key<TAB>value`).
//!
//! At the end of a run (`write_calls`): `DIR/calls.tsv`, the slices each
//! kernel ran. A kernel is straight-line code, so its dynamic instruction
//! count is exactly slices x its static count (call-outs' callees aside).
//!
//! CATEGORIES (of a machine instruction): `a.*` numeric work on values;
//! `b.*` boolean / mask / guard work (compares, logic, selects, the
//! mask-to-vector conversions the vector-mask booleans need); `c.*` data
//! movement and overhead (input loads, constant broadcasts, spills and
//! reloads, register moves, call-outs, the frame); `d.*` the stores of the
//! roots (fields, error, live, transfer).
//!
//! ROLE CLASS (of a graph node, by which roots reach it): `value` (only
//! fields and transfer roots), `guard-E` / `guard-L` / `guard-EL` (only the
//! bodies' error / live roots), `shared` (both).
//!
//! GUARD FAMILIES: an error root is an OR of terms (`trace::error`), each an
//! operator's own error (maybe AND the guard it was evaluated under): a
//! term is classified by the first characteristic node under it (`Known` of
//! a floor: `flr-span`; `Known` of a boolean: `sel-undecided`; `SplitOk`:
//! `fork-cover`; `NoWrap`: `nowrap`; `Lo`/`Hi` against a literal:
//! `restrict`; `guarded-` before it when the term is an AND with a big
//! cone), else what the tracer states itself: `pin` (reads inputs
//! only), `contain` (a computed value against a literal: a widening's
//! containment), `stated-other` (an unfinished unrolled loop, a raise). A
//! live root's conjuncts are `live`. A guard-only node is charged to the one
//! family that reaches it, or to `multi`; and, separately, by its LEAF
//! CLASS, what its cone reads: a tile-flag call-out `T`, an `mget` call-out
//! `M` (in this program only `spikes_at`'s), a fork's fragment `F`,
//! arithmetic `A`; `-` inputs only.

use std::collections::{BTreeMap, HashMap};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};

use crate::transpile::asm::Compiled;
use crate::transpile::graph::{Graph, NodeId, Op};

/// A root's roles (bits).
pub(crate) const ROLE_FIELD: u8 = 1;
pub(crate) const ROLE_ERROR: u8 = 2;
pub(crate) const ROLE_LIVE: u8 = 4;
pub(crate) const ROLE_ARC: u8 = 8;

/// The directory, when on.
pub(crate) fn dir() -> Option<PathBuf> {
    std::env::var_os("CELESTE_KERNEL_MIX").map(PathBuf::from)
}

/// `CELESTE_KERNEL_METRICS=1`: the census and the slice counters without
/// the files (`rewrite bench-frame --metrics`: `kernel_metrics`).
pub fn metrics_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_KERNEL_METRICS").is_ok_and(|v| v == "1"))
}

/// One kernel's census (`report`): its static counts, `key -> value`.
pub type Census = BTreeMap<String, u64>;

/// Every kernel built while on: its symbol, its slice counter, its census.
static KERNELS: Mutex<Vec<(String, Arc<AtomicU64>, Census)>> = Mutex::new(Vec::new());

/// A fresh slice counter for kernel `sym`, with its census.
pub(crate) fn counter(sym: &str, census: Census) -> Arc<AtomicU64> {
    let c = Arc::new(AtomicU64::new(0));
    KERNELS.lock().unwrap().push((sym.to_string(), c.clone(), census));
    c
}

/// Every kernel built so far: its symbol, the slices it ran, its census
/// (`insts` the static instruction count: a kernel is straight-line code, so
/// slices x insts is its dynamic count, call-outs' callees aside), by symbol.
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
        Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => "cmp",
        Op::Not | Op::And | Op::Or | Op::Known | Op::SplitValid(_) | Op::FragOk(_) | Op::SplitOk(_) | Op::NoWrap => "bool",
        Op::Sel => "sel",
        Op::Restrict(..) => "restrict",
        Op::Mget | Op::TileFlagAt => "call",
        Op::Split(_) | Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Flr | Op::Sin | Op::Min | Op::Max | Op::Span | Op::Frag(_) | Op::SplitInt(_) | Op::IntFrag(_) | Op::Lo | Op::Hi => "num",
    }
}

/// `DIR/calls.tsv`: per kernel the slices it ran so far.
pub(crate) fn write_calls() {
    let Some(dir) = dir() else { return };
    let mut out = String::from("sym\tslices\n");
    for (sym, c, _) in KERNELS.lock().unwrap().iter() {
        writeln!(out, "{sym}\t{}", c.load(Ordering::Relaxed)).unwrap();
    }
    if let Err(e) = std::fs::write(dir.join("calls.tsv"), out) {
        eprintln!("[mix] writing calls.tsv: {e}");
    }
}

/// One kernel's inputs to the report.
pub(crate) struct Input<'a> {
    /// The traced frame's graph, before specialization and fusion.
    pub traced: &'a Graph,
    pub fused: &'a Graph,
    /// The distinct root slots and each one's roles (bits).
    pub roots: &'a [NodeId],
    pub roles: &'a [u8],
    /// Per body, its (error, live) root nodes.
    pub bodies: &'a [(NodeId, NodeId)],
    pub compiled: &'a Compiled,
    pub so: &'a Path,
    /// Input cell -> its traced path.
    pub names: &'a HashMap<u32, String>,
    pub cart: &'a celeste_core::cart_data::CartData,
}

/// A node's role class.
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

fn op_name(op: &Op) -> String {
    format!("{op:?}").split('(').next().unwrap_or("").to_string()
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

/// An error term's kind; `guarded-` where it is an AND with a big cone: an
/// own error under its site's guard, or a fused candidate's `live & error`
/// (`lower::specialize_frame`, step 4), whose cost is the guard's.
fn error_kind(g: &Graph, term: NodeId) -> String {
    let k = error_kind_inner(g, term);
    if g.get(term).op == Op::And && cone(g, term) > 40 {
        format!("err:guarded-{}", &k[4..])
    } else {
        k.to_string()
    }
}

/// An error term's own kind: the first characteristic node, breadth first.
fn error_kind_inner(g: &Graph, term: NodeId) -> &'static str {
    let mut queue = std::collections::VecDeque::from([term]);
    let mut seen = std::collections::HashSet::new();
    while let Some(n) = queue.pop_front() {
        if !seen.insert(n) || seen.len() > 4096 {
            continue;
        }
        let node = g.get(n);
        match node.op {
            Op::Known => {
                return match g.get(node.args[0]).op {
                    Op::Flr => "err:flr-span",
                    _ => "err:sel-undecided",
                }
            }
            Op::SplitOk(_) => return "err:fork-cover",
            Op::NoWrap => return "err:nowrap",
            Op::Lt | Op::Gt
                if node.args.iter().any(|a| matches!(g.get(*a).op, Op::Lo | Op::Hi))
                    && node.args.iter().any(|a| matches!(g.get(*a).op, Op::Const(..))) =>
            {
                return "err:restrict"
            }
            _ => {}
        }
        queue.extend(node.args.iter().copied());
    }
    // Stated by the tracer: a PIN (reads inputs only: the kernel's pinned
    // inputs), a CONTAINMENT (a computed value against a literal bound: an
    // output widening's), else the rest (an unrolled loop's end, a raise).
    let mut stack = vec![term];
    let mut seen = std::collections::HashSet::new();
    let mut computed = false;
    while let Some(x) = stack.pop() {
        if !seen.insert(x) {
            continue;
        }
        let node = g.get(x);
        if !matches!(node.op, Op::Cell(_) | Op::Const(..) | Op::ConstBool(_) | Op::Eq | Op::Not | Op::And | Op::Or | Op::Lo | Op::Hi | Op::Restrict(..)) {
            computed = true;
            break;
        }
        stack.extend(node.args.iter().copied());
    }
    let top = g.get(term);
    if !computed {
        "err:pin"
    } else if matches!(top.op, Op::Lt | Op::Gt | Op::Le | Op::Ge) && top.args.iter().any(|a| matches!(g.get(*a).op, Op::Const(..))) {
        "err:contain"
    } else {
        "err:stated-other"
    }
}

/// `n`'s subtree to `depth` (cells by name), for `<sym>.guards`.
fn describe(g: &Graph, n: NodeId, depth: usize, names: &HashMap<u32, String>) -> String {
    let node = g.get(n);
    match &node.op {
        Op::Cell(c) => return names.get(c).cloned().unwrap_or_else(|| format!("cell{c}")),
        Op::Const(lo, hi) if lo == hi => return format!("{}", *lo as f64 / 65536.0),
        _ => {}
    }
    if depth == 0 || node.args.is_empty() {
        return format!("{:?}#{n}", node.op);
    }
    let args: Vec<String> = node.args.iter().map(|&a| describe(g, a, depth - 1, names)).collect();
    let mut s = format!("{:?}({})", node.op, args.join(", "));
    if s.len() > 600 {
        s.truncate(s.floor_char_boundary(600));
        s.push_str("...");
    }
    s
}

/// Nodes in `root`'s cone.
fn cone(g: &Graph, root: NodeId) -> usize {
    let mut seen = std::collections::HashSet::new();
    let mut stack = vec![root];
    while let Some(n) = stack.pop() {
        if seen.insert(n) {
            stack.extend(g.get(n).args.iter().copied());
        }
    }
    seen.len()
}

/// A machine instruction's category.
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

/// One kernel's census and, with `CELESTE_KERNEL_MIX=DIR`, its report.
pub(crate) fn report(inp: &Input) -> Result<Census> {
    let dir = dir();
    if let Some(dir) = &dir {
        std::fs::create_dir_all(dir)?;
    }
    let (g, comp) = (inp.fused, inp.compiled);
    let sym = &comp.sym;
    let n = g.len();

    // Roles, top-down (operands have smaller ids).
    let mut role = vec![0u8; n];
    let mut root_role: HashMap<NodeId, u8> = HashMap::new();
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

    // Guard terms and conjuncts, and their families.
    let mut fam_names: Vec<String> = Vec::new();
    let mut fam_ix: HashMap<String, usize> = HashMap::new();
    let mut fam_terms: Vec<u64> = Vec::new();
    let mut seeds: Vec<(NodeId, usize)> = Vec::new();
    let mut n_terms = 0usize;
    let mut n_conj = 0usize;
    for &(err, live) in inp.bodies {
        let mut add = |name: String, t: NodeId, seeds: &mut Vec<(NodeId, usize)>| {
            let k = *fam_ix.entry(name.clone()).or_insert_with(|| {
                fam_names.push(name);
                fam_terms.push(0);
                fam_names.len() - 1
            });
            fam_terms[k] += 1;
            seeds.push((t, k));
        };
        let mut terms = Vec::new();
        spread(g, err, &Op::Or, &mut terms);
        n_terms += terms.len();
        for t in terms {
            add(error_kind(g, t), t, &mut seeds);
        }
        let mut conj = Vec::new();
        spread(g, live, &Op::And, &mut conj);
        n_conj += conj.len();
        for c in conj {
            add("live".to_string(), c, &mut seeds);
        }
    }
    // `<sym>.guards`: each distinct error root's terms and live root, readable.
    if let Some(dir) = &dir {
        let mut out = String::new();
        let mut done = std::collections::HashSet::new();
        for &(err, live) in inp.bodies {
            if done.insert(err) {
                let mut terms = Vec::new();
                spread(g, err, &Op::Or, &mut terms);
                writeln!(out, "ERROR #{err}: {} terms, cone {}", terms.len(), cone(g, err)).unwrap();
                for t in terms {
                    writeln!(out, "  [{}] cone {} {}", error_kind(g, t), cone(g, t), describe(g, t, 5, inp.names)).unwrap();
                }
            }
            if done.insert(live) {
                writeln!(out, "LIVE #{live}: cone {} {}", cone(g, live), describe(g, live, 6, inp.names)).unwrap();
            }
        }
        std::fs::write(dir.join(format!("{sym}.guards")), out)?;
    }
    // The 63 most used families keep a bit; the rest share `other`.
    let mut by_use: Vec<usize> = (0..fam_names.len()).collect();
    by_use.sort_by_key(|&k| std::cmp::Reverse(fam_terms[k]));
    let mut bit = vec![63u32; fam_names.len()];
    for (rank, &k) in by_use.iter().enumerate().take(63) {
        bit[k] = rank as u32;
    }
    let bit_name = |b: u32| -> String {
        if b == 63 {
            "other".to_string()
        } else {
            fam_names[by_use[b as usize]].clone()
        }
    };
    let mut fam = vec![0u64; n];
    for &(t, k) in &seeds {
        fam[t as usize] |= 1u64 << bit[k];
    }
    for id in (0..n).rev() {
        let f = fam[id];
        if f != 0 {
            for &a in &g.get(id as NodeId).args {
                fam[a as usize] |= f;
            }
        }
    }
    let fam_of = |id: NodeId| -> String {
        let f = fam[id as usize];
        let c = class(role[id as usize]);
        if !c.starts_with("guard") {
            return "-".into();
        }
        match f.count_ones() {
            0 => "-".into(),
            1 => bit_name(f.trailing_zeros()),
            _ => "multi".into(),
        }
    };

    // Per node, the bodies whose guards reach it: one, or several.
    const MANY: u32 = u32::MAX - 1;
    let mut gbody = vec![u32::MAX; n];
    let merge = |v: &mut u32, b: u32| {
        if *v == u32::MAX {
            *v = b;
        } else if *v != b {
            *v = MANY;
        }
    };
    for (bi, &(err, live)) in inp.bodies.iter().enumerate() {
        merge(&mut gbody[err as usize], bi as u32);
        merge(&mut gbody[live as usize], bi as u32);
    }
    for id in (0..n).rev() {
        let gb = gbody[id];
        if gb != u32::MAX {
            for &a in &g.get(id as NodeId).args {
                merge(&mut gbody[a as usize], gb);
            }
        }
    }

    let mut t: BTreeMap<String, u64> = BTreeMap::new();
    let bump = |t: &mut BTreeMap<String, u64>, k: String, v: u64| *t.entry(k).or_default() += v;

    // The graph census (nodes the roots reach).
    let mut reach_n = 0u64;
    for id in 0..n {
        let m = role[id];
        if m == 0 {
            continue;
        }
        reach_n += 1;
        let node = g.get(id as NodeId);
        let c = class(m);
        bump(&mut t, format!("node.{}.{c}", op_name(&node.op)), 1);
        bump(&mut t, format!("nodecls.{c}"), 1);
        if c.starts_with("guard") {
            bump(&mut t, format!("famnodes.{}", fam_of(id as NodeId)), 1);
            let priv_ = match gbody[id] {
                MANY => "shared",
                u32::MAX => "none",
                _ => "private",
            };
            bump(&mut t, format!("guardbodies.{priv_}"), 1);
        }
    }
    bump(&mut t, "nodes".into(), reach_n);
    for id in 0..n {
        if role[id] != 0 {
            bump(&mut t, format!("kind.{}", op_kind(&g.get(id as NodeId).op)), 1);
        }
    }
    bump(&mut t, "fused_nodes".into(), n as u64);
    bump(&mut t, "traced.nodes".into(), inp.traced.len() as u64);
    for id in 0..inp.traced.len() {
        bump(&mut t, format!("traced.kind.{}", op_kind(&inp.traced.get(id as NodeId).op)), 1);
    }
    bump(&mut t, "bodies".into(), inp.bodies.len() as u64);
    let distinct = |f: &dyn Fn(&(NodeId, NodeId)) -> NodeId| inp.bodies.iter().map(f).collect::<std::collections::HashSet<_>>().len() as u64;
    bump(&mut t, "bodies.distinct_error".into(), distinct(&|b| b.0));
    bump(&mut t, "bodies.distinct_live".into(), distinct(&|b| b.1));
    bump(&mut t, "error_terms".into(), n_terms as u64);
    bump(&mut t, "live_conjuncts".into(), n_conj as u64);
    for (k, name) in fam_names.iter().enumerate() {
        bump(&mut t, format!("famterms.{name}"), fam_terms[k]);
    }
    bump(&mut t, "roots".into(), inp.roots.len() as u64);
    for &m in inp.roles {
        bump(&mut t, format!("rootrole.{}", class(m)), 1);
    }
    bump(&mut t, "ssa".into(), comp.prov.len() as u64);
    bump(&mut t, "spill_slots".into(), comp.spill_slots as u64);

    // What each node's cone reads (bottom-up): the leaf class.
    let mut leaf = vec![0u8; n];
    for id in 0..n {
        let node = g.get(id as NodeId);
        let mut b = node.args.iter().fold(0u8, |m, a| m | leaf[*a as usize]);
        b |= match node.op {
            Op::TileFlagAt => 1,
            Op::Mget => 2,
            Op::FragOk(_) | Op::Frag(_) | Op::IntFrag(_) | Op::SplitOk(_) => 4,
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Flr | Op::Min | Op::Max | Op::Neg | Op::Abs => 8,
            _ => 0,
        };
        leaf[id] = b;
    }
    // `T` a tile-flag call, `M` an mget call, `F` a fork's fragment, `A`
    // arithmetic; `-` inputs only.
    const LEAF: [&str; 16] = ["-", "T", "M", "TM", "F", "TF", "MF", "TMF", "A", "TA", "MA", "TMA", "FA", "TFA", "MFA", "TMFA"];
    let leaf_name = |id: NodeId| -> &'static str { LEAF[leaf[id as usize] as usize] };
    // `spikes_at`'s tile tests: `Eq(k, Mget(..))` per k.
    for id in 0..n {
        let node = g.get(id as NodeId);
        if role[id] != 0 && node.op == Op::Eq && node.args.iter().any(|a| g.get(*a).op == Op::Mget) {
            for a in &node.args {
                if let Op::Const(k, _) = g.get(*a).op {
                    bump(&mut t, format!("mgeteq.{}", k >> 16), 1);
                }
            }
        }
    }
    for id in 0..n {
        if role[id] != 0 {
            bump(&mut t, format!("leafnodes.{}.{}", class(role[id]), leaf_name(id as NodeId)), 1);
        }
    }
    // The error OR trees: their distinct OR nodes, against one shared
    // premise of the terms that read only inputs (pins, restrictions).
    {
        let mut ors: std::collections::HashSet<NodeId> = Default::default();
        let mut input_terms: std::collections::HashSet<NodeId> = Default::default();
        let mut rest = 0u64;
        let mut done = std::collections::HashSet::new();
        for &(err, _) in inp.bodies {
            if !done.insert(err) {
                continue;
            }
            let mut stack = vec![err];
            while let Some(x) = stack.pop() {
                let node = g.get(x);
                if node.op == Op::Or {
                    ors.insert(x);
                    stack.extend(node.args.iter().copied());
                } else if node.op != Op::ConstBool(false) {
                    if leaf_name(x) == "-" {
                        input_terms.insert(x);
                    } else {
                        rest += 1;
                    }
                }
            }
            rest += 1; // OR with the premise
        }
        bump(&mut t, "err_or_nodes".into(), ors.len() as u64);
        bump(&mut t, "err_or_hoisted".into(), input_terms.len().saturating_sub(1) as u64 + rest);
        bump(&mut t, "err_input_terms".into(), input_terms.len() as u64);
    }

    // `spikes_at` folded away: the nodes still needed if every
    // `Eq(k, Mget(..))` is false (constants propagated through the boolean
    // layer), and whether the player's region could reach a spike tile at
    // all (its restricted x, y with 16 px of margin) - where it cannot, an
    // interval fold of `mget` over the coordinates' ranges proves that.
    let spike_keep: Vec<bool> = {
        #[derive(Clone, Copy, PartialEq)]
        enum V {
            Same,
            Const(bool),
            Alias(NodeId),
        }
        let mut v = vec![V::Same; n];
        let res = |v: &[V], x: NodeId| -> V {
            match v[x as usize] {
                V::Same => V::Alias(x),
                k => k,
            }
        };
        for id in 0..n {
            let node = g.get(id as NodeId);
            let a: Vec<V> = node.args.iter().map(|&x| res(&v, x)).collect();
            v[id] = match node.op {
                Op::Eq if node.args.iter().any(|x| g.get(*x).op == Op::Mget) => V::Const(false),
                Op::Not => match a[0] {
                    V::Const(b) => V::Const(!b),
                    _ => V::Same,
                },
                Op::And | Op::Or => {
                    let unit = node.op == Op::And;
                    let rest: Vec<V> = a.iter().copied().filter(|x| *x != V::Const(unit)).collect();
                    if a.contains(&V::Const(!unit)) {
                        V::Const(!unit)
                    } else if rest.is_empty() {
                        V::Const(unit)
                    } else if rest.len() == 1 {
                        rest[0]
                    } else {
                        V::Same
                    }
                }
                Op::Sel => match a[0] {
                    V::Const(true) => a[1],
                    V::Const(false) => a[2],
                    _ if a[1] == a[2] => a[1],
                    _ => V::Same,
                },
                _ => V::Same,
            };
            if v[id] == V::Alias(id as NodeId) {
                v[id] = V::Same;
            }
        }
        let mut keep = vec![false; n];
        let mut stack: Vec<NodeId> = inp.roots.iter().map(|&r| match res(&v, r) {
            V::Alias(x) => x,
            _ => r,
        }).collect();
        while let Some(x) = stack.pop() {
            if keep[x as usize] {
                continue;
            }
            keep[x as usize] = true;
            if matches!(v[x as usize], V::Const(_)) {
                continue;
            }
            for &a in &g.get(x).args {
                match res(&v, a) {
                    V::Alias(y) => stack.push(y),
                    _ => {}
                }
            }
        }
        keep
    };
    {
        let kept = (0..n).filter(|&i| role[i] != 0 && spike_keep[i]).count();
        bump(&mut t, "spikefold.nodes_kept".into(), kept as u64);
        // The player's restricted whole-pixel position.
        let (mut xr, mut yr) = (None, None);
        for id in 0..n {
            if let Op::Restrict(lo, hi) = g.get(id as NodeId).op {
                if let Op::Cell(c) = g.get(g.get(id as NodeId).args[0]).op {
                    let name = inp.names.get(&c).map(String::as_str).unwrap_or("");
                    let field = name.rsplit('.').next().unwrap_or("");
                    let whole = !name.contains("rem.") && !name.contains("spd.") && !name.contains("room.");
                    if whole && field == "x" {
                        xr = Some((lo >> 16, hi >> 16));
                    } else if whole && field == "y" {
                        yr = Some((lo >> 16, hi >> 16));
                    }
                }
            }
        }
        let reach = match (xr, yr) {
            (Some((x0, x1)), Some((y0, y1))) => {
                let (rx, ry) = crate::game_runner::start_room();
                let mut any = false;
                for ty in ((y0 - 16).max(0) / 8)..=((y1 + 16).min(127) / 8) {
                    for tx in ((x0 - 16).max(0) / 8)..=((x1 + 16).min(127) / 8) {
                        let tile = inp.cart.mget_whole(rx * 16 + tx as i16, ry * 16 + ty as i16);
                        any |= matches!(tile, 17 | 27 | 43 | 59);
                    }
                }
                if any { "spikes" } else { "none" }
            }
            _ => "unknown",
        };
        bump(&mut t, format!("spikefold.reach.{reach}"), 1);
    }

    // The machine instructions.
    let mut lines = String::new();
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
        let (cat, cls, fam_s, opn, fold) = match cur {
            Some(k) if started => {
                let (node, kind) = comp.prov[k];
                let op = &g.get(node).op;
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
                let cls = if kind == "store" || kind == "storemask" { format!("store-{store_role}") } else { class(role[node as usize]).to_string() };
                (category(mn, ops, kind, op, store_role), cls, fam_of(node), format!("{}/{kind}", op_name(op)), comp.foldable[k])
            }
            _ => ("c.frame".to_string(), "frame".to_string(), "-".to_string(), "-".to_string(), false),
        };
        if ops.contains("{1to16}") {
            bump(&mut t, format!("constop.{cls}"), 1);
            bump(&mut t, "constop".into(), 1);
        }
        bump(&mut t, "insts".into(), 1);
        bump(&mut t, format!("cat.{cat}"), 1);
        bump(&mut t, format!("catcls.{cat}.{cls}"), 1);
        bump(&mut t, format!("cls.{cls}"), 1);
        if cls.starts_with("guard") {
            bump(&mut t, format!("famlines.{fam_s}"), 1);
        }
        if let Some(k) = cur.filter(|_| started) {
            bump(&mut t, format!("leaflines.{cls}.{}", leaf_name(comp.prov[k].0)), 1);
        }
        if let Some(k) = cur.filter(|_| started) {
            if !spike_keep[comp.prov[k].0 as usize] {
                bump(&mut t, "spikefold.lines".into(), 1);
                bump(&mut t, format!("spikefoldcat.{cat}"), 1);
            } else if fold {
                bump(&mut t, "fold_after_spike.lines".into(), 1);
            }
        }
        if fold {
            bump(&mut t, "fold.lines".into(), 1);
            bump(&mut t, format!("foldcat.{cat}"), 1);
            bump(&mut t, format!("foldcls.{cls}"), 1);
        }
        bump(&mut t, format!("opkind.{opn}"), 1);
        let spiked = cur.filter(|_| started).is_some_and(|k| !spike_keep[comp.prov[k].0 as usize]);
        writeln!(lines, "{cat}\t{cls}\t{fam_s}\t{opn}\t{}\t{}", fold as u8, spiked as u8).unwrap();
    }
    bump(&mut t, "fold.ssa".into(), comp.foldable.iter().filter(|f| **f).count() as u64);
    // A started `#@end` after the body: the epilogue is `c.frame` (`cur` None).
    if let Some(dir) = &dir {
        let mut tsv = String::new();
        for (k, v) in &t {
            writeln!(tsv, "{k}\t{v}").unwrap();
        }
        std::fs::write(dir.join(format!("{sym}.tsv")), tsv)?;
        std::fs::write(dir.join(format!("{sym}.lines")), lines)?;
        std::fs::write(dir.join(format!("{sym}.s")), &comp.asm)?;
        std::fs::copy(inp.so, dir.join(format!("{sym}.so"))).with_context(|| format!("copying {}", inp.so.display()))?;
    }
    Ok(t)
}
