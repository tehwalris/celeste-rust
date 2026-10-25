//! THE KERNEL CENSUS (`bench frame --metrics`, plans/devloop.md: the exact
//! metrics): per kernel, its static counts - the traced and fused graphs'
//! nodes by kind, the roots by role, the guards' terms, and its machine
//! instructions by CATEGORY (`report`) - and, joined with the slices it ran
//! (`counted`: `Kernels::counting`'s counters), its dynamic instructions. A
//! kernel is straight-line code, so its dynamic instruction count is
//! exactly slices x its static count (call-outs' callees aside).
//!
//! A TOOL, not a build option: the census REBUILDS the level's kernels
//! (`census`: the walk, the lowering and the codegen with instruction
//! provenance, no gcc), each one's text digest checked against the running
//! set's (`Kernels::digest_of`), and keeps them in the kernel cache beside
//! the set's own entry (`<key>-census`), so a build of the sources pays for
//! it once. 16.16's alone.
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

use anyhow::{Context, Result};
use celeste_core::arith::Fixed;

use crate::abstraction::Level;
use crate::compiled::{KernelLanes, Kernels};
use crate::settings::{Arith, Settings};
use crate::trace::walk::Region;
use crate::transpile::asm::Compiled;
use crate::transpile::graph::{Graph, NodeId, Op};

/// A root's roles (bits).
const ROLE_FIELD: u8 = 1;
const ROLE_ERROR: u8 = 2;
const ROLE_LIVE: u8 = 4;
const ROLE_ARC: u8 = 8;

/// One kernel's static counts (`report`), `key -> value`.
pub type Census = BTreeMap<String, u64>;

/// One kernel's census, as rebuilt: its (shape, region), its symbol, the
/// digest of its text (`Compiled::text_digest`) and its counts.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct KernelCensus {
    pub key: (u64, Option<Region>),
    pub sym: String,
    pub digest: u64,
    pub census: Census,
}

/// THE CENSUS of `level`'s kernels under `s` (16.16's alone): from the kernel
/// cache (`<key>-census`, beside the set's own entry), else rebuilt and
/// kept there.
pub fn census(level: Level, s: &Settings) -> Result<Vec<KernelCensus>> {
    census_in(level, s, &crate::compiled::cache::Store::default())
}

/// `census` in the kernel cache `store`.
fn census_in(level: Level, s: &Settings, store: &crate::compiled::cache::Store) -> Result<Vec<KernelCensus>> {
    anyhow::ensure!(s.arith == Arith::Fixed, "the kernel census is 16.16's: not under --arith {}", s.arith);
    let key = crate::compiled::cache::key::<Fixed>(level, s)?;
    let name = format!("{}-census", key.name);
    if let Some(files) = store.get(&name, &key)? {
        let bytes = files.get("census.bin").ok_or_else(|| anyhow::anyhow!("the census entry {name} holds no census.bin"))?;
        return bincode::deserialize(bytes).context("the cached census");
    }
    let t0 = std::time::Instant::now();
    // The walk and the lowering recurse deeply (as the kernel build does).
    let s2 = s.clone();
    let out = std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(move || rebuild(level, &s2))
        .context("spawn the census")?
        .join()
        .map_err(|_| anyhow::anyhow!("the census panicked"))??;
    eprintln!("[census] {} kernels rebuilt in {:.2}s, kept as {name}", out.len(), t0.elapsed().as_secs_f64());
    store.put(&name, &key, &[("census.bin".to_string(), bincode::serialize(&out)?)])?;
    Ok(out)
}

/// Every kernel of the set rebuilt as the registry builds it
/// (`asm_kernel::build_one_shape`: the same frames in the same order, the
/// same slots and symbols), its text with provenance, censused.
fn rebuild(level: Level, s: &Settings) -> Result<Vec<KernelCensus>> {
    let refs = crate::trace::lower::lattice_kernel_refs::<Fixed>(level, s).context("retracing the start room for the census")?;
    crate::par::Pool::per_core().stack(128 << 20).map(refs.len(), |si| {
        let (region, r) = &refs[si];
        let shape = r.frame.in_rt2.shape_hash_of();
        let (fused, bodies) = (&r.lowered.fused, &r.lowered.bodies);
        let (flat_roots, slot_of) = crate::compiled::asm_kernel::root_slots(bodies);
        let sym = crate::compiled::asm_kernel::kernel_sym(shape, si);
        let compiled = crate::transpile::asm::compile(fused, &flat_roots, &sym, true).with_context(|| format!("the census's build of {sym}"))?;
        let slot = |n: NodeId| slot_of[&n] as usize;
        let mut roles = vec![0u8; flat_roots.len()];
        for b in bodies {
            for &n in &b.roots.fields {
                roles[slot(n)] |= ROLE_FIELD;
            }
            roles[slot(b.roots.error)] |= ROLE_ERROR;
            roles[slot(b.roots.live)] |= ROLE_LIVE;
            for n in b.roots.arc.iter().flat_map(|a| a.nodes()) {
                roles[slot(n)] |= ROLE_ARC;
            }
        }
        let guards: Vec<(NodeId, NodeId)> = bodies.iter().map(|b| (b.roots.error, b.roots.live)).collect();
        let census = report(&Input { traced: &r.bound.graph, fused, roots: &flat_roots, roles: &roles, bodies: &guards, compiled: &compiled }).with_context(|| format!("the census of {sym}"))?;
        Ok(KernelCensus { key: (shape, *region), sym, digest: compiled.text_digest, census })
    })
}

/// THE KERNELS `k` RAN, CENSUSED: per kernel of its set (by symbol) the
/// slices it ran (`Kernels::counting`'s counters) and its census, the
/// rebuilt kernel's text the running one's. A set that is not counting, or
/// a kernel without its census, fails.
pub fn counted<N: KernelLanes>(k: &Kernels<N>, level: Level, s: &Settings) -> Result<Vec<(String, u64, Census)>> {
    let mut by_key: std::collections::HashMap<(u64, Option<Region>), KernelCensus> = census(level, s)?.into_iter().map(|c| (c.key, c)).collect();
    let counts = k.slice_counts();
    anyhow::ensure!(counts.len() == by_key.len(), "the census holds {} kernels, the running set counts slices of {} (a set without `Kernels::counting` counts none)", by_key.len(), counts.len());
    let mut out = Vec::with_capacity(counts.len());
    for (key, sym, slices) in counts {
        let c = by_key.remove(&key).ok_or_else(|| anyhow::anyhow!("kernel {sym} ({key:?}): not in the census"))?;
        anyhow::ensure!(c.sym == sym, "kernel {key:?}: the census's is {}, the running one {sym}", c.sym);
        anyhow::ensure!(k.digest_of(key) == Some(c.digest), "kernel {sym}: the census rebuilt another text ({:016x}) than the running kernel's ({:?})", c.digest, k.digest_of(key));
        out.push((sym, slices, c.census));
    }
    out.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(out)
}
/// A graph op's KIND, for the metrics: `leaf` (literals, cells, unknowns),
/// `num` (number -> number, an interval's ends, a fork's fragments), `cmp`
/// (number -> bool), `bool` (bool -> bool, the errors' validity tests),
/// `sel`, `restrict`, `call` (a cart lookup: a call-out in the kernel).
fn op_kind(op: &Op<Fixed>) -> &'static str {
    match op {
        Op::Const(..) | Op::ConstBool(_) | Op::Cell(_) | Op::UnknownNum { .. } | Op::UnknownBool(_) => "leaf",
        Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Same => "cmp",
        Op::Not | Op::And | Op::Or | Op::Known | Op::ForkValid(..) | Op::FragOk(..) | Op::ForkOk(..) | Op::NoWrap => "bool",
        Op::Sel => "sel",
        Op::Restrict(..) | Op::Pin(_) => "restrict",
        Op::Mget | Op::TileFlagAt => "call",
        Op::Fork(..) | Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Flr | Op::Sin | Op::Min | Op::Max | Op::Span | Op::Frag(..) | Op::Lo | Op::Hi => "num",
    }
}

/// One kernel's inputs to the census.
struct Input<'a> {
    /// The traced frame's graph, before specialization and fusion.
    traced: &'a Graph<Fixed>,
    fused: &'a Graph<Fixed>,
    /// The distinct root slots and each one's roles (bits).
    roots: &'a [NodeId],
    roles: &'a [u8],
    /// Per body, its (error, live) root nodes.
    bodies: &'a [(NodeId, NodeId)],
    compiled: &'a Compiled,
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
fn spread(g: &Graph<Fixed>, root: NodeId, op: &Op<Fixed>, out: &mut Vec<NodeId>) {
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

fn category(mn: &str, ops: &str, kind: &str, node_op: &Op<Fixed>, store_role: &str) -> String {
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

/// One kernel's static counts.
fn report(inp: &Input) -> Result<Census> {
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

#[cfg(test)]
mod tests {
    use crate::abstraction::Level;
    use crate::compiled::cache::Store;
    use crate::settings::Settings;

    /// THE CACHED CENSUS IS THE FRESH ONE: room (1,0) r0sxh's census rebuilt
    /// into two empty kernel caches is the same twice (a function of the
    /// sources), and loaded from one it is the same again; every kernel
    /// counted instructions. Refused in float.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels rebuilt twice for their census, ~30 CPU-s"]
    fn the_cached_census_is_the_fresh_one() {
        let (level, s) = (Level::parse("r0sxh").expect("level"), Settings::default());
        let roots: Vec<std::path::PathBuf> = (0..2).map(|i| std::env::temp_dir().join(format!("celeste-census-{}-{i}", std::process::id()))).collect();
        for r in &roots {
            let _ = std::fs::remove_dir_all(r);
        }
        let stores: Vec<Store> = roots.iter().map(|r| Store::at(r)).collect();
        let fresh: Vec<Vec<super::KernelCensus>> = stores.iter().map(|st| super::census_in(level, &s, st).expect("a fresh census")).collect();
        assert!(fresh[0] == fresh[1], "two fresh censuses differ");
        assert!(!fresh[0].is_empty() && fresh[0].iter().all(|c| c.census.get("insts").is_some_and(|&n| n > 0)), "every kernel has instructions");
        let key = crate::compiled::cache::key::<celeste_core::arith::Fixed>(level, &s).expect("key");
        assert!(stores[0].get(&format!("{}-census", key.name), &key).expect("get").is_some(), "the census is kept beside the set");
        assert!(super::census_in(level, &s, &stores[0]).expect("the cached census") == fresh[0], "the cached census is not the fresh one");
        let float = Settings { arith: crate::settings::Arith::Float(crate::settings::FloatTool::Celia), ..s.clone() };
        assert!(super::census_in(level, &float, &stores[0]).is_err(), "a float census");
        for r in &roots {
            let _ = std::fs::remove_dir_all(r);
        }
    }
}
