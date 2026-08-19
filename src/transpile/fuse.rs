//! Specialization-set FUSION (plans/shape-tag-plan.md, step 3/4).
//!
//! Input: n members of one specialization set - premise-specialized
//! recipes over the SAME shape witness (member 0 is the PRIMARY, whose
//! boundary path materializes rows; the rest are "dying"-style members
//! whose boundary rows are provably block-uniform). Each member lowers
//! through the kernel emitter's walk (`kernel::emit_walk`); this module
//! value-numbers the members' nodes against ONE shared table and emits a
//! SINGLE fused kernel artifact:
//!
//!   - shared nodes once; a node reachable from a strict subset of
//!     members routes its deopt side effects to a MEMBER-SET register
//!     (`dp_m{bitmask}`), so each member's coverage mask is the OR of
//!     the registers whose set contains it;
//!   - guard-as-selector is IMPLICIT: a lane belongs to the first member
//!     (fixed priority order = argument order) whose own deopt mask is
//!     clear. Complementary guards (the kill branch pinned false in
//!     steady, true in dy-spikes) make coverage disjoint where the
//!     selector value is known; lanes no member covers deopt to the
//!     interpreter, loudly counted. The plan's static truth-table check
//!     is subsumed by that runtime count plus the H=68 set-identity gate.
//!   - non-primary members produce NO materialization code. Their
//!     boundary rows are proven block-uniform at fuse time (`reachable_
//!     cells` of the member's final heap topology contains no per-lane
//!     data - the deleted player's cells dropped out), so the EXECUTOR
//!     collapses covered lanes to one representative per distinct
//!     uniform-output tuple and routes only representatives through the
//!     existing interpreter deopt path. Row sets are preserved exactly:
//!     dropped lanes' rows are member-certified identical to the
//!     representative's.
//!
//! Anything this pass cannot prove is REFUSED with an error at emission
//! time - there is no partially-fused output.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Write as _;

use anyhow::{anyhow, bail, Result};

use super::kernel::{
    compute_out_fields, emit_interface, emit_key_cell, emit_walk, key_cells, mentions_ident,
    reachable_cells, render_lines, word_used, Emit, Line, OutFields,
};
use crate::rewrite::program::Program;

/// Placeholder the renderable form carries where `, &mut dp` sat; the
/// emitter substitutes the node's member-set register.
const DP: &str = "__DP__";

/// One value node of the shared graph.
struct NodeDef {
    ty: &'static str,
    /// Canonical renderable expr: operands as `<id>`, dp as `__DP__`.
    render: String,
    /// Which members contain this node (bit i = member i).
    members: u8,
    has_dp: bool,
}

/// A structural item of one member's line stream, in stream order.
enum PItem {
    Node(u32),
    /// `zguard(op, &mut dp)` - op by canonical id.
    Guard(u32),
    /// A `*bd = true` statement - by canonical text id.
    Bail(u32),
    /// `for cK { ... zi_fork_flr ... }` opening group - fork node id.
    ForkOpen(u32),
    /// `let (v, v_pt) = zi_split_at(op, at, &mut dp)` - split node id.
    SplitAt(u32),
}

struct ForkDef {
    depth: usize,
    op: u32,
}

struct SplitDef {
    op: u32,
    at: String,
}

/// The shared numbering across all members.
struct Ctx {
    intern: HashMap<String, u32>,
    nodes: BTreeMap<u32, NodeDef>,
    forks: BTreeMap<u32, ForkDef>,
    splits: BTreeMap<u32, SplitDef>,
    /// Guarded operand id -> member set.
    guards: BTreeMap<u32, u8>,
    /// Bail canonical text id -> (renderable text, member set).
    bails: BTreeMap<u32, (String, u8)>,
}

impl Ctx {
    fn intern(&mut self, canon: &str) -> u32 {
        let next = self.intern.len() as u32;
        *self.intern.entry(canon.to_string()).or_insert(next)
    }
}

/// One parsed member.
struct PMember {
    label: String,
    e: Emit,
    of: OutFields,
    /// Pre items split into fork segments: segment k = items before the
    /// k-th fork open; the fork open itself starts segment k+1.
    pre_segs: Vec<Vec<PItem>>,
    suf: Vec<PItem>,
    /// r_cN input-load lines, verbatim (must match across members).
    loads: Vec<String>,
    /// member-local var name -> canonical id.
    vn: HashMap<String, u32>,
}

/// Replace every whole identifier that has a canonical id with `<id>`.
fn canonicalize(expr: &str, vn: &HashMap<String, u32>) -> String {
    let bytes = expr.as_bytes();
    let mut out = String::with_capacity(expr.len());
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if c.is_ascii_alphabetic() || c == '_' {
            let start = i;
            while i < bytes.len()
                && ((bytes[i] as char).is_ascii_alphanumeric() || bytes[i] == b'_')
            {
                i += 1;
            }
            let ident = &expr[start..i];
            match vn.get(ident) {
                Some(id) => {
                    out.push('<');
                    out.push_str(&id.to_string());
                    out.push('>');
                }
                None => out.push_str(ident),
            }
        } else {
            out.push(c);
            i += 1;
        }
    }
    out
}

/// Substitute `<id>` operand refs with fused names `n{id}`. Strict: only
/// `<` + digits + `>` is a ref (comparison operators pass through - a
/// bare `< 123>` cannot occur, constants render as `P8::from_raw(..)`).
fn fused_expr(render: &str) -> String {
    let mut out = String::with_capacity(render.len());
    let bytes = render.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'<' {
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_digit() {
                j += 1;
            }
            if j > i + 1 && j < bytes.len() && bytes[j] == b'>' {
                out.push('n');
                out.push_str(&render[i + 1..j]);
                i = j + 1;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

/// Parse one member's walk into the shared numbering. `mi` = member index.
fn parse_member(label: &str, e: Emit, mi: usize, ctx: &mut Ctx) -> Result<PMember> {
    let of = compute_out_fields(&e)?;
    let mbit = 1u8 << mi;
    let mut vn: HashMap<String, u32> = HashMap::new();
    let mut loads: Vec<String> = Vec::new();
    let mut pre_segs: Vec<Vec<PItem>> = vec![Vec::new()];
    let mut suf: Vec<PItem> = Vec::new();

    // The fork group is 6 consecutive Raw lines; track how many of the
    // fixed header lines remain to swallow after a fork opening.
    for (region, lines) in [(0u8, &e.pre), (1u8, &e.suf)] {
        let mut iter = lines.iter().peekable();
        while let Some(line) = iter.next() {
            let items: &mut Vec<PItem> = if region == 0 {
                pre_segs.last_mut().unwrap()
            } else {
                &mut suf
            };
            match line {
                Line::Let { name, ty, expr } => {
                    let stripped = expr.replace(", &mut dp", "");
                    let has_dp = stripped.len() != expr.len();
                    let canon = canonicalize(&stripped, &vn);
                    let id = ctx.intern(&canon);
                    let render = canonicalize(&expr.replace(", &mut dp", &format!(", &mut {}", DP)), &vn);
                    // The same canonical node may recur across regions
                    // (constants, untainted CSE); lockstep emission places
                    // it at its first - pre-most - encounter, which is
                    // sound: a pre-emitted node the suffix reads becomes a
                    // crossing var, and an untainted node's dp effect is
                    // variant-independent.
                    match ctx.nodes.get_mut(&id) {
                        Some(def) => {
                            if def.ty != *ty || def.render != render {
                                bail!(
                                    "member {} node {} ({}) conflicts with an earlier member's \
                                     (ty {} vs {})",
                                    label, id, name, ty, def.ty
                                );
                            }
                            def.members |= mbit;
                        }
                        None => {
                            ctx.nodes.insert(id, NodeDef { ty, render, members: mbit, has_dp });
                        }
                    }
                    vn.insert(name.clone(), id);
                    items.push(PItem::Node(id));
                }
                Line::Raw(s) => {
                    if s.starts_with("let r_c") {
                        loads.push(s.clone());
                    } else if let Some(rest) = s.strip_prefix("zguard(") {
                        let opname = rest
                            .strip_suffix(", &mut dp);")
                            .ok_or_else(|| anyhow!("unparsed guard {:?}", s))?;
                        let op = *vn
                            .get(opname)
                            .ok_or_else(|| anyhow!("guard on unknown var {:?}", s))?;
                        *ctx.guards.entry(op).or_insert(0) |= mbit;
                        items.push(PItem::Guard(op));
                    } else if s.contains("*bd = true") {
                        let render = canonicalize(s, &vn);
                        let id = ctx.intern(&format!("bail:{}", render));
                        ctx.bails.entry(id).or_insert((render, 0)).1 |= mbit;
                        items.push(PItem::Bail(id));
                    } else if s.starts_with("for c") && s.ends_with(" in 0..2usize {") {
                        if region != 0 {
                            bail!("member {}: fork in the button suffix", label);
                        }
                        // Swallow the fixed header and the three tail lines.
                        for want in ["let mut dp = dp;", "let mut bd_l: bool = *bd;", "let bd: &mut bool = &mut bd_l;"] {
                            match iter.next() {
                                Some(Line::Raw(t)) if t == want => {}
                                other => bail!("member {}: fork header line {:?}, wanted {:?}", label, other.map(|l| format!("{:?}", l)), want),
                            }
                        }
                        let fork_line = match iter.next() {
                            Some(Line::Raw(t)) if t.contains("zi_fork_flr(") => t.clone(),
                            other => bail!("member {}: missing zi_fork_flr after fork open ({:?})", label, other.map(|l| format!("{:?}", l))),
                        };
                        // let (vN, vN_fv): (ZI, u16) = zi_fork_flr(vM, cK, &mut dp);
                        let name = fork_line
                            .strip_prefix("let (")
                            .and_then(|t| t.split(',').next())
                            .ok_or_else(|| anyhow!("unparsed fork {:?}", fork_line))?
                            .to_string();
                        let args = fork_line
                            .split("zi_fork_flr(")
                            .nth(1)
                            .and_then(|t| t.strip_suffix(", &mut dp);"))
                            .ok_or_else(|| anyhow!("unparsed fork {:?}", fork_line))?;
                        let mut parts = args.split(", ");
                        let opname = parts.next().unwrap();
                        let cfg = parts.next().ok_or_else(|| anyhow!("unparsed fork {:?}", fork_line))?;
                        let depth: usize = cfg
                            .strip_prefix('c')
                            .and_then(|d| d.parse().ok())
                            .ok_or_else(|| anyhow!("unparsed fork config {:?}", fork_line))?;
                        let op = *vn
                            .get(opname)
                            .ok_or_else(|| anyhow!("fork on unknown var {:?}", fork_line))?;
                        let id = ctx.intern(&format!("fork(c{}, <{}>)", depth, op));
                        let fv = ctx.intern(&format!("fork_fv(c{}, <{}>)", depth, op));
                        ctx.forks.entry(id).or_insert(ForkDef { depth, op });
                        vn.insert(name.clone(), id);
                        vn.insert(format!("{}_fv", name), fv);
                        // valid line + continue line.
                        match iter.next() {
                            Some(Line::Raw(t)) if t.starts_with(&format!("let valid{}: u16 = ", depth)) => {}
                            other => bail!("member {}: missing valid after fork ({:?})", label, other.map(|l| format!("{:?}", l))),
                        }
                        match iter.next() {
                            Some(Line::Raw(t)) if t == &format!("if valid{} == 0 {{ continue; }}", depth) => {}
                            other => bail!("member {}: missing continue after fork ({:?})", label, other.map(|l| format!("{:?}", l))),
                        }
                        pre_segs.last_mut().unwrap().push(PItem::ForkOpen(id));
                        pre_segs.push(Vec::new());
                    } else if s.contains("zi_split_at(") {
                        // let (vN, vN_pt): (ZI, u16) = zi_split_at(vM, P8, &mut dp);
                        let name = s
                            .strip_prefix("let (")
                            .and_then(|t| t.split(',').next())
                            .ok_or_else(|| anyhow!("unparsed split {:?}", s))?
                            .to_string();
                        let args = s
                            .split("zi_split_at(")
                            .nth(1)
                            .and_then(|t| t.strip_suffix(", &mut dp);"))
                            .ok_or_else(|| anyhow!("unparsed split {:?}", s))?;
                        let (opname, at) = args
                            .split_once(", ")
                            .ok_or_else(|| anyhow!("unparsed split {:?}", s))?;
                        let op = *vn
                            .get(opname)
                            .ok_or_else(|| anyhow!("split on unknown var {:?}", s))?;
                        let id = ctx.intern(&format!("split_at(<{}>, {})", op, at));
                        let pt = ctx.intern(&format!("split_at_pt(<{}>, {})", op, at));
                        ctx.splits.entry(id).or_insert(SplitDef { op, at: at.to_string() });
                        vn.insert(name.clone(), id);
                        vn.insert(format!("{}_pt", name), pt);
                        items.push(PItem::SplitAt(id));
                    } else {
                        bail!("member {}: unclassified raw line {:?}", label, s);
                    }
                }
            }
        }
    }
    Ok(PMember { label: label.to_string(), e, of, pre_segs, suf, loads, vn })
}

fn lower(members: &[(String, Program)], witness_path: &str, ctx: &mut Ctx) -> Result<Vec<PMember>> {
    let mut out = Vec::new();
    for (mi, (label, program)) in members.iter().enumerate() {
        if mi >= 8 {
            bail!("more than 8 members - member sets are u8 bitmasks");
        }
        let e = emit_walk(program, witness_path)?;
        out.push(parse_member(label, e, mi, ctx)?);
    }
    Ok(out)
}

/// The sharing census: per-member node counts, pairwise sharing, union.
pub fn fuse_census(members: &[(String, Program)], witness_path: &str) -> Result<()> {
    let mut ctx = Ctx {
        intern: HashMap::new(),
        nodes: BTreeMap::new(),
        forks: BTreeMap::new(),
        splits: BTreeMap::new(),
        guards: BTreeMap::new(),
        bails: BTreeMap::new(),
    };
    let pms = lower(members, witness_path, &mut ctx)?;
    let sets: Vec<(String, BTreeSet<u32>, usize)> = pms
        .iter()
        .enumerate()
        .map(|(mi, pm)| {
            let mbit = 1u8 << mi;
            let ids: Vec<u32> = ctx
                .nodes
                .iter()
                .filter(|(_, d)| d.members & mbit != 0)
                .map(|(id, _)| *id)
                .collect();
            let n: usize = pm.pre_segs.iter().map(|s| s.len()).sum::<usize>()
                + pm.suf.len();
            (pm.label.clone(), ids.into_iter().collect(), n)
        })
        .collect();
    for (label, set, n) in &sets {
        println!("{}: {} items, {} distinct nodes", label, n, set.len());
    }
    for i in 0..sets.len() {
        for j in i + 1..sets.len() {
            let shared = sets[i].1.intersection(&sets[j].1).count();
            println!("{} n {}: {} shared", sets[i].0, sets[j].0, shared);
        }
    }
    let mut union: BTreeSet<u32> = BTreeSet::new();
    let mut all: Option<BTreeSet<u32>> = None;
    for (_, set, _) in &sets {
        union.extend(set.iter().copied());
        all = Some(match all {
            None => set.clone(),
            Some(a) => a.intersection(set).copied().collect(),
        });
    }
    println!(
        "all members: {} shared; union {} distinct (vs {} summed)",
        all.map(|a| a.len()).unwrap_or(0),
        union.len(),
        sets.iter().map(|(_, s, _)| s.len()).sum::<usize>()
    );
    Ok(())
}

/// Emit the fused artifact. Member 0 is the primary (its boundary path
/// materializes rows); the rest must have block-uniform boundary rows.
pub fn emit_fused(members: &[(String, Program)], witness_path: &str) -> Result<String> {
    if members.len() < 2 {
        bail!("fusion needs at least two members");
    }
    let mut ctx = Ctx {
        intern: HashMap::new(),
        nodes: BTreeMap::new(),
        forks: BTreeMap::new(),
        splits: BTreeMap::new(),
        guards: BTreeMap::new(),
        bails: BTreeMap::new(),
    };
    let pms = lower(members, witness_path, &mut ctx)?;
    let n_members = pms.len();
    let all_mask: u8 = ((1u16 << n_members) - 1) as u8;
    let primary = &pms[0];

    // --- interface premises: one bind serves every member ---
    for pm in &pms[1..] {
        if pm.e.shape_hash != primary.e.shape_hash {
            bail!("member {} shape hash differs", pm.label);
        }
        if pm.e.uni != primary.e.uni {
            bail!("member {} uniform-cell set differs", pm.label);
        }
        if pm.e.vary_in != primary.e.vary_in {
            bail!("member {} varying-cell set differs", pm.label);
        }
        if pm.loads != primary.loads {
            bail!("member {} input loads differ", pm.label);
        }
        if !pm.e.pins.is_subset(&primary.e.pins) {
            bail!(
                "member {} pins cells the primary does not ({:?} vs {:?}) - bind would not check them",
                pm.label, pm.e.pins, primary.e.pins
            );
        }
        for id in &pm.e.pins {
            if pm.e.stable.get(id) != primary.e.stable.get(id) {
                bail!("member {} pin {} disagrees on the stable value", pm.label, id);
            }
        }
        if pm.e.fork_depth != primary.e.fork_depth || pm.e.valid_expr != primary.e.valid_expr {
            bail!("member {} fork structure differs", pm.label);
        }
        if pm.pre_segs.len() != primary.pre_segs.len() {
            bail!("member {} fork segment count differs", pm.label);
        }
    }
    // Fork skeletons: the k-th fork of every member must be the same node.
    let fork_seq = |pm: &PMember| -> Vec<u32> {
        pm.pre_segs
            .iter()
            .flatten()
            .filter_map(|it| match it {
                PItem::ForkOpen(id) => Some(*id),
                _ => None,
            })
            .collect()
    };
    let primary_forks = fork_seq(primary);
    for pm in &pms[1..] {
        if fork_seq(pm) != primary_forks {
            bail!("member {} fork skeleton differs from the primary's", pm.label);
        }
    }

    // --- non-primary members: prove the boundary rows are collapse-keyable ---
    // The member's final heap reachable set must contain no free per-lane
    // data. Each reachable dirty cell's output value is either
    //   - a uniform scalar: it joins the member's TUPLE (one value per
    //     (config, variant) callback - button-TAINTED scalars are fine,
    //     `Dy` is built inside the callback after the kb bindings), or
    //   - a per-lane column (Z*) PROVEN value-identical to the primary's
    //     own out column for the same cell (same fused node after CSE):
    //     it joins the VARY set, and the executor reads its per-lane
    //     value from KOut/KOutShared and extends the collapse key with
    //     it (e.g. the corpse dash-start effects freeze / has_dashed,
    //     whose gate reads the post-refill djump - lane-varying, but the
    //     identical computation the live dash-start blend does).
    // Two lanes the same callback covers with equal (tuple, vary-at-lane)
    // keys therefore have identical boundary rows.
    let mut tuples: Vec<Vec<(u32, &'static str, String)>> = Vec::new();
    let mut varys: Vec<Vec<(u32, &'static str)>> = Vec::new();
    for pm in &pms[1..] {
        let reach = reachable_cells(&pm.e);
        for id in pm.e.vary_in.keys() {
            if reach.contains(id) {
                bail!(
                    "member {}: varying input cell {} survives into the boundary - \
                     its rows are not block-uniform",
                    pm.label, id
                );
            }
        }
        let mut tup: Vec<(u32, &'static str, String)> = Vec::new();
        let mut vt: Vec<(u32, &'static str)> = Vec::new();
        for (id, ty, expr, _tainted) in &pm.of.fields {
            if !reach.contains(id) {
                continue;
            }
            match *ty {
                "P8" | "bool" | "(P8, P8)" => {
                    tup.push((*id, ty, canonicalize(expr, &pm.vn)));
                }
                "ZN" | "ZB" | "ZI" => {
                    let Some((pty, pexpr)) = primary
                        .of
                        .fields
                        .iter()
                        .find(|f| f.0 == *id)
                        .map(|f| (f.1, &f.2))
                    else {
                        bail!(
                            "member {}: per-lane boundary cell {} has no primary out column",
                            pm.label, id
                        );
                    };
                    if pty != *ty {
                        bail!(
                            "member {}: per-lane boundary cell {} is {} but the primary's is {}",
                            pm.label, id, ty, pty
                        );
                    }
                    let a = canonicalize(expr, &pm.vn);
                    let b = canonicalize(pexpr, &primary.vn);
                    if a != b {
                        bail!(
                            "member {}: per-lane boundary cell {} is not the primary's value\n  \
                             member:  {}\n  primary: {}",
                            pm.label, id, a, b
                        );
                    }
                    vt.push((*id, ty));
                }
                other => bail!(
                    "member {}: boundary cell {} has unkeyable type {}",
                    pm.label, id, other
                ),
            }
        }
        if let Some(first) = tuples.first() {
            let a: Vec<u32> = first.iter().map(|(id, _, _)| *id).collect();
            let b: Vec<u32> = tup.iter().map(|(id, _, _)| *id).collect();
            if a != b {
                bail!(
                    "members disagree on the boundary tuple cells ({:?} vs {:?})",
                    a, b
                );
            }
        }
        if let Some(first) = varys.first() {
            if *first != vt {
                bail!(
                    "members disagree on the per-lane vary cells ({:?} vs {:?})",
                    first, vt
                );
            }
        }
        tuples.push(tup);
        varys.push(vt);
    }
    let vary_cells: Vec<(u32, &'static str)> = varys.first().cloned().unwrap_or_default();

    // --- deopt registers: one per distinct member set with effects ---
    let mut reg_masks: BTreeSet<u8> = BTreeSet::new();
    reg_masks.insert(all_mask); // forks/splits and shared guards live here
    for def in ctx.nodes.values() {
        if def.has_dp {
            reg_masks.insert(def.members);
        }
    }
    for set in ctx.guards.values() {
        reg_masks.insert(*set);
    }
    let reg = |mask: u8| format!("dp_m{}", mask);

    // --- fused line streams (lockstep interleave by fork segment) ---
    let mut pre: Vec<Line> = Vec::new();
    let mut suf: Vec<Line> = Vec::new();
    let mut emitted: BTreeSet<u32> = BTreeSet::new();
    let mut emitted_guards: BTreeSet<u32> = BTreeSet::new();
    let mut emitted_bails: BTreeSet<u32> = BTreeSet::new();
    let mut var_ty: HashMap<String, &'static str> = HashMap::new();
    let mut pre_defs: BTreeSet<String> = BTreeSet::new();
    for load in &primary.loads {
        pre.push(Line::Raw(load.clone()));
    }
    for (id, kind) in &primary.e.vary_in {
        let ty = if *kind == "num" { "ZN" } else { "ZB" };
        var_ty.insert(format!("r_c{}", id), ty);
        pre_defs.insert(format!("r_c{}", id));
    }

    // The split's dp effect belongs to the members that CONTAIN the split;
    // splits/forks are structural raws, so track their member sets from
    // the per-member streams.
    let mut split_members: BTreeMap<u32, u8> = BTreeMap::new();
    let mut fork_members: BTreeMap<u32, u8> = BTreeMap::new();
    for (mi, pm) in pms.iter().enumerate() {
        for it in pm.pre_segs.iter().flatten().chain(pm.suf.iter()) {
            match it {
                PItem::SplitAt(id) => *split_members.entry(*id).or_insert(0) |= 1 << mi,
                PItem::ForkOpen(id) => *fork_members.entry(*id).or_insert(0) |= 1 << mi,
                _ => {}
            }
        }
    }
    for (id, set) in &fork_members {
        if *set != all_mask {
            bail!("fork {} is not shared by all members ({})", id, set);
        }
    }
    for set in split_members.values() {
        reg_masks.insert(*set);
    }

    let mut valid_stack: Vec<String> = vec!["ALL".to_string()];
    let mut fork_count = 0usize;
    {
        // Emit one item; returns lines pushed into the right stream.
        let mut emit_item = |it: &PItem,
                             region: u8,
                             out: &mut Vec<Line>,
                             pre_defs: &mut BTreeSet<String>,
                             var_ty: &mut HashMap<String, &'static str>,
                             valid_stack: &mut Vec<String>,
                             fork_count: &mut usize|
         -> Result<()> {
            match it {
                PItem::Node(id) => {
                    if !emitted.insert(*id) {
                        return Ok(());
                    }
                    let def = &ctx.nodes[id];
                    let name = format!("n{}", id);
                    let expr = fused_expr(&def.render).replace(DP, &reg(def.members));
                    var_ty.insert(name.clone(), def.ty);
                    if region == 0 {
                        pre_defs.insert(name.clone());
                    }
                    out.push(Line::Let { name, ty: def.ty, expr });
                }
                PItem::Guard(op) => {
                    if !emitted_guards.insert(*op) {
                        return Ok(());
                    }
                    let set = ctx.guards[op];
                    out.push(Line::Raw(format!("zguard(n{}, &mut {});", op, reg(set))));
                }
                PItem::Bail(id) => {
                    if !emitted_bails.insert(*id) {
                        return Ok(());
                    }
                    let (render, _) = &ctx.bails[id];
                    out.push(Line::Raw(fused_expr(render)));
                }
                PItem::SplitAt(id) => {
                    if !emitted.insert(*id) {
                        return Ok(());
                    }
                    let def = &ctx.splits[id];
                    let set = split_members[id];
                    let name = format!("n{}", id);
                    out.push(Line::Raw(format!(
                        "let ({n}, {n}_pt): (ZI, u16) = zi_split_at(n{op}, {at}, &mut {r});",
                        n = name,
                        op = def.op,
                        at = def.at,
                        r = reg(set)
                    )));
                    var_ty.insert(name.clone(), "ZI");
                    var_ty.insert(format!("{}_pt", name), "u16");
                    if region == 0 {
                        pre_defs.insert(name.clone());
                        pre_defs.insert(format!("{}_pt", name));
                    }
                }
                PItem::ForkOpen(id) => {
                    if !emitted.insert(*id) {
                        return Ok(());
                    }
                    let def = &ctx.forks[id];
                    let name = format!("n{}", id);
                    out.push(Line::Raw(format!("for c{} in 0..2usize {{", def.depth)));
                    for mask in reg_masks.iter() {
                        out.push(Line::Raw(format!("let mut {r} = {r};", r = reg(*mask))));
                    }
                    out.push(Line::Raw("let mut bd_l: bool = *bd;".to_string()));
                    out.push(Line::Raw("let bd: &mut bool = &mut bd_l;".to_string()));
                    out.push(Line::Raw(format!(
                        "let ({n}, {n}_fv): (ZI, u16) = zi_fork_flr(n{op}, c{d}, &mut {r});",
                        n = name,
                        op = def.op,
                        d = def.depth,
                        r = reg(all_mask)
                    )));
                    let valid = format!("valid{}", def.depth);
                    out.push(Line::Raw(format!(
                        "let {}: u16 = {} & {}_fv;",
                        valid,
                        valid_stack.last().unwrap(),
                        name
                    )));
                    out.push(Line::Raw(format!("if {} == 0 {{ continue; }}", valid)));
                    var_ty.insert(name.clone(), "ZI");
                    var_ty.insert(format!("{}_fv", name), "u16");
                    var_ty.insert(valid.clone(), "u16");
                    pre_defs.insert(name.clone());
                    pre_defs.insert(format!("{}_fv", name));
                    pre_defs.insert(valid.clone());
                    valid_stack.push(valid);
                    *fork_count += 1;
                }
            }
            Ok(())
        };

        // Prefix: segment s of every member, then the shared fork open.
        let n_segs = primary.pre_segs.len();
        for s in 0..n_segs {
            // Non-fork items of this segment, member by member.
            for pm in &pms {
                for it in &pm.pre_segs[s] {
                    if matches!(it, PItem::ForkOpen(_)) {
                        continue;
                    }
                    emit_item(it, 0, &mut pre, &mut pre_defs, &mut var_ty, &mut valid_stack, &mut fork_count)?;
                }
            }
            // The fork closing this segment (identical across members).
            if let Some(PItem::ForkOpen(id)) = primary.pre_segs[s]
                .iter()
                .find(|it| matches!(it, PItem::ForkOpen(_)))
            {
                emit_item(&PItem::ForkOpen(*id), 0, &mut pre, &mut pre_defs, &mut var_ty, &mut valid_stack, &mut fork_count)?;
            }
        }
        // Suffix: single segment.
        for pm in &pms {
            for it in &pm.suf {
                emit_item(it, 1, &mut suf, &mut pre_defs, &mut var_ty, &mut valid_stack, &mut fork_count)?;
            }
        }
    }
    if fork_count != primary.e.fork_depth {
        bail!("fused fork count {} != member fork depth {}", fork_count, primary.e.fork_depth);
    }
    let valid_expr = valid_stack.last().unwrap().clone();
    if valid_expr != primary.e.valid_expr {
        bail!(
            "fused validity mask {} != member's {} - fork numbering drifted",
            valid_expr, primary.e.valid_expr
        );
    }
    let member_dp = |mi: usize| -> String {
        let terms: Vec<String> = reg_masks
            .iter()
            .filter(|m| **m & (1 << mi) != 0)
            .map(|m| reg(*m))
            .collect();
        terms.join(" | ")
    };

    // --- fused out fields (primary member's, exprs renamed) ---
    let fused_of = OutFields {
        fields: primary
            .of
            .fields
            .iter()
            .map(|(id, ty, expr, tainted)| {
                (*id, *ty, fused_expr(&canonicalize(expr, &primary.vn)), *tainted)
            })
            .collect(),
        ubool: primary.of.ubool.clone(),
    };

    // --- assemble the artifact ---
    let mut out = String::new();
    writeln!(
        out,
        "// GENERATED by `transpile --fuse` (plans/shape-tag-plan.md). Do not\n\
         // edit, do not commit: the fused artifact is built per campaign\n\
         // (feature `fused`). Members, in lane-priority order:"
    )?;
    for (mi, pm) in pms.iter().enumerate() {
        writeln!(out, "//   {}: {}", mi, pm.label)?;
    }
    writeln!(
        out,
        "// Member 0 is the primary: its rows materialize through apply/\n\
         // append_out as in any class kernel. The rest cover lanes whose\n\
         // boundary rows are block-uniform; `Dy` reports their coverage\n\
         // masks and uniform-output tuples, and the executor collapses\n\
         // covered lanes to one interpreter representative per tuple.\n\
         //"
    )?;
    emit_interface(&mut out, &primary.e, &fused_of)?;

    // --- M1 stage 1 (task #152): hoist kb-independent suffix lets ---
    // The member emitters cut prefix/suffix POSITIONALLY (at the first
    // button-reading line), so the suffix carries nodes whose reads never
    // reach a kb bit - identical in all 64 variants, recomputed 64 times
    // (73 of 178 suffix lets in the 2026-08-19 artifact). Hoist them, in
    // order, to the end of the prefix. Soundness: a hoisted let reads
    // only prefix names or earlier hoisted lets (anything defined by a
    // kb-tainted or unparsed line is treated as tainted, which blocks
    // its dependents); its dp effect enters the shared registers once
    // and flows into every variant's copy of `p.dp_m*`, exactly what 64
    // identical OR-ins produced. Raw lines (guards, bails, splits) are
    // never hoisted, and any name they define is conservatively tainted.
    fn idents(s: &str) -> Vec<String> {
        let mut v = Vec::new();
        let mut cur = String::new();
        for ch in s.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                cur.push(ch);
            } else if !cur.is_empty() {
                if !cur.chars().next().unwrap().is_ascii_digit() {
                    v.push(std::mem::take(&mut cur));
                } else {
                    cur.clear();
                }
            }
        }
        if !cur.is_empty() && !cur.chars().next().unwrap().is_ascii_digit() {
            v.push(cur);
        }
        v
    }
    let hoisted = {
        let mut tainted: BTreeSet<String> =
            (0..6).map(|k| format!("kb{}", k)).collect();
        let mut keep: Vec<Line> = Vec::new();
        let mut hoist: Vec<Line> = Vec::new();
        for line in std::mem::take(&mut suf) {
            match &line {
                Line::Let { name, expr, .. } => {
                    if idents(expr).iter().any(|w| tainted.contains(w)) {
                        tainted.insert(name.clone());
                        keep.push(line);
                    } else {
                        hoist.push(line);
                    }
                }
                Line::Raw(text) => {
                    let t = text.trim_start();
                    // Splits and guards are pure except their dp/bd
                    // effect, which commutes with the hoist (see above) -
                    // a kb-free one moves, and its outputs stay clean.
                    let kb_free_reads = |s: &str| {
                        idents(s).iter().all(|w| !tainted.contains(w))
                    };
                    if (t.starts_with("zguard(") && kb_free_reads(t))
                        || (t.starts_with("let (")
                            && t.contains("zi_split_at(")
                            && kb_free_reads(t.split('=').nth(1).unwrap_or("")))
                    {
                        hoist.push(line);
                        continue;
                    }
                    // Any `let`-bound names in an unhoisted raw line are
                    // tainted - the raw stays in the suffix, so
                    // dependents must too.
                    if let Some(rest) = t.strip_prefix("let ") {
                        if let Some(binders) = rest.split('=').next() {
                            for w in idents(binders.split(':').next().unwrap_or("")) {
                                if w != "mut" {
                                    tainted.insert(w);
                                }
                            }
                        }
                    }
                    keep.push(line);
                }
            }
        }
        suf = keep;
        let n = hoist.len();
        for line in hoist {
            if let Line::Let { name, .. } = &line {
                pre_defs.insert(name.clone());
            }
            pre.push(line);
        }
        n
    };
    eprintln!("[fused] hoisted {} kb-independent suffix lets into the prefix", hoisted);

    let pre_text = render_lines(&pre);
    let suf_text = render_lines(&suf);

    // Dy: coverage masks + tuples for the non-primary members.
    let tuple_cells = &tuples[0];
    writeln!(out, "/// Non-primary member count (lane-priority order).")?;
    writeln!(out, "pub const N_DY: usize = {};", n_members - 1)?;
    writeln!(out, "/// The block-uniform boundary tuple of a non-primary member.")?;
    writeln!(out, "#[derive(Clone, Copy, PartialEq, Eq)]")?;
    writeln!(out, "pub struct DyTuple {{")?;
    for (id, ty, _) in tuple_cells {
        writeln!(out, "    pub c{}: {},", id, ty)?;
    }
    writeln!(out, "}}\n")?;
    writeln!(out, "impl DyTuple {{")?;
    writeln!(out, "    /// Raw-bits collapse key (BTreeMap-friendly).")?;
    let key_width: usize = tuple_cells
        .iter()
        .map(|(_, ty, _)| if *ty == "(P8, P8)" { 2 } else { 1 })
        .sum();
    writeln!(out, "    pub fn key(&self) -> [u32; {}] {{", key_width)?;
    writeln!(out, "        [")?;
    for (id, ty, _) in tuple_cells {
        match *ty {
            "bool" => writeln!(out, "            self.c{} as u32,", id)?,
            "P8" => writeln!(out, "            self.c{}.as_raw_u32(),", id)?,
            "(P8, P8)" => {
                writeln!(out, "            self.c{}.0.as_raw_u32(),", id)?;
                writeln!(out, "            self.c{}.1.as_raw_u32(),", id)?;
            }
            _ => unreachable!(),
        }
    }
    writeln!(out, "        ]")?;
    writeln!(out, "    }}")?;
    writeln!(out, "}}\n")?;
    writeln!(
        out,
        "/// Per-lane extension of the collapse key: reachable boundary cells\n\
         /// of the non-primary members whose per-lane value IS the primary's\n\
         /// own out column (proven the same fused node at emission time), so\n\
         /// the executor reads them straight out of KOut/KOutShared."
    )?;
    writeln!(out, "pub const N_DY_VARY: usize = {};", vary_cells.len())?;
    writeln!(
        out,
        "#[allow(unused_variables)]\n\
         pub fn dy_vary_key(sh: &KOutShared, kv: &KOut, lane: usize) -> [u64; N_DY_VARY] {{"
    )?;
    writeln!(out, "    [")?;
    for (id, ty) in &vary_cells {
        let tainted = fused_of
            .fields
            .iter()
            .find(|f| f.0 == *id)
            .map(|f| f.3)
            .unwrap_or(false);
        let src = if tainted { "kv" } else { "sh" };
        match *ty {
            "ZN" => writeln!(out, "        {}.c{}[lane].as_raw_u32() as u64,", src, id)?,
            "ZB" => writeln!(
                out,
                "        ({src}.c{id}.val >> lane & 1) as u64 | (({src}.c{id}.known >> lane & 1) as u64) << 1,",
                src = src,
                id = id
            )?,
            "ZI" => writeln!(
                out,
                "        {src}.c{id}.lo[lane].as_raw_u32() as u64 | ({src}.c{id}.hi[lane].as_raw_u32() as u64) << 32,",
                src = src,
                id = id
            )?,
            _ => unreachable!(),
        }
    }
    writeln!(out, "    ]")?;
    writeln!(out, "}}\n")?;
    writeln!(
        out,
        "/// Per-(config, variant) coverage of the non-primary members.\n\
         pub struct Dy {{\n\
         \x20   /// Lanes covered by member i+1 (valid, primary-deopt, own-\n\
         \x20   /// coverage clear, earlier members excluded).\n\
         \x20   pub covered: [u16; N_DY],\n\
         \x20   pub tuples: [DyTuple; N_DY],\n\
         }}\n"
    )?;

    // Crossing: prefix names the suffix side reads. The suffix side is
    // the fused suffix text plus every epilogue expr (tainted KOut
    // fields, the Dy tuples, the member dp combinations read `p.valid`).
    let mut epi_text = String::new();
    for (_, _, expr, tainted) in &fused_of.fields {
        if *tainted {
            epi_text.push('\n');
            epi_text.push_str(expr);
        } else if (0..6).any(|k| mentions_ident(expr, &format!("kb{}", k))) {
            bail!("output cell reads a button bit but is marked untainted");
        }
    }
    for tup in &tuples {
        for (_, _, expr) in tup {
            // Tuple exprs may read button bits (they are rendered inside
            // the per-variant callback, after the kb bindings) - they are
            // suffix-side by construction.
            epi_text.push('\n');
            epi_text.push_str(&fused_expr(expr));
        }
    }
    let mut suffix_scan = suf_text.clone();
    suffix_scan.push_str(&epi_text);
    for k in 0..6 {
        if mentions_ident(&pre_text, &format!("kb{}", k)) {
            bail!("button bit kb{} leaked into the fused prefix", k);
        }
    }
    let mut crossing: BTreeSet<String> = BTreeSet::new();
    for name in &pre_defs {
        if word_used(&suffix_scan, name) {
            crossing.insert(name.clone());
        }
    }
    // (No special case for bare-name out exprs: a TAINTED bare name is in
    // suffix_scan via the epilogue exprs, so the word_used loop above
    // catches it; an UNTAINTED one is consumed by `osh` inside frame(),
    // where the name is a local. Force-adding untainted ones minted Pre
    // fields nothing read once the stage-1 hoist moved their last
    // suffix users into the prefix.)

    // --- M1 stage 2 (task #152): support-segment emission ---
    // Each remaining suffix line depends on a SUBSET of the button bits
    // (its "support"): transitively, the kb bits its expr mentions plus
    // the supports of the suffix names it reads. A line with support S
    // has only 2^|S| distinct evaluations across the 2^|used| variants,
    // so lines are grouped by support into segment fns `seg_k::<A>` that
    // frame() evaluates once per assignment of S, caching the results;
    // each variant call reads every segment through the variant's
    // projection onto S, picked at emission time. Soundness: dp
    // registers and `bd` are write-only OR-accumulators in every lane
    // primitive (celeste-engine kernel.rs), so a segment accumulates its
    // own deltas from zero and the variant ORs them in - same lanes as
    // inline evaluation, in a different (commutative) order. A raw line
    // that is not a recognized zguard / zi_split_at / `*bd` bail is
    // forced into the full-support residual and taints its binders,
    // exactly like the stage-1 hoist.
    let used_mask: u8 = (0..6)
        .filter(|k| mentions_ident(&suffix_scan, &format!("kb{}", k)))
        .map(|k| 1u8 << k)
        .sum();
    struct LineInfo {
        sup: u8,
        binds: Vec<String>,
    }
    // name_sup outlives the line scan: stage 3 reuses it to compute the
    // supports of the out-cell EXPRS (the row-key factoring below).
    let mut name_sup: HashMap<String, u8> = HashMap::new();
    let sup_of = |s: &str, name_sup: &HashMap<String, u8>| -> u8 {
        let mut m = 0u8;
        for w in idents(s) {
            if let Some(k) = w.strip_prefix("kb").and_then(|r| r.parse::<u8>().ok()) {
                if k < 6 {
                    m |= 1 << k;
                    continue;
                }
            }
            m |= name_sup.get(&w).copied().unwrap_or(0);
        }
        m
    };
    let infos: Vec<LineInfo> = {
        let mut infos = Vec::new();
        for line in &suf {
            let info = match line {
                Line::Let { name, expr, .. } => {
                    let sup = sup_of(expr, &name_sup);
                    name_sup.insert(name.clone(), sup);
                    LineInfo { sup, binds: vec![name.clone()] }
                }
                Line::Raw(text) => {
                    let t = text.trim_start();
                    if t.starts_with("zguard(") {
                        LineInfo { sup: sup_of(t, &name_sup), binds: vec![] }
                    } else if t.starts_with("if !") && t.ends_with("{ *bd = true; }") {
                        LineInfo { sup: sup_of(t, &name_sup), binds: vec![] }
                    } else if t.starts_with("let (") && t.contains("zi_split_at(") {
                        let sup = sup_of(t.split('=').nth(1).unwrap_or(""), &name_sup);
                        let binds = idents(
                            t.strip_prefix("let (").unwrap().split(')').next().unwrap_or(""),
                        );
                        for b in &binds {
                            name_sup.insert(b.clone(), sup);
                        }
                        LineInfo { sup, binds }
                    } else {
                        // Unrecognized raw: full support, binders tainted.
                        let mut binds = Vec::new();
                        if let Some(rest) = t.strip_prefix("let ") {
                            for w in idents(rest.split('=').next().unwrap_or("")) {
                                if w != "mut"
                                    && !matches!(w.as_str(), "ZI" | "ZN" | "ZB" | "u16" | "P8" | "bool")
                                {
                                    name_sup.insert(w.clone(), used_mask);
                                    binds.push(w);
                                }
                            }
                        }
                        LineInfo { sup: used_mask, binds }
                    }
                }
            };
            infos.push(info);
        }
        infos
    };
    let mut seg_groups: BTreeMap<u8, Vec<usize>> = BTreeMap::new();
    let mut residual_idx: Vec<usize> = Vec::new();
    for (i, info) in infos.iter().enumerate() {
        if info.sup == used_mask {
            residual_idx.push(i);
        } else {
            seg_groups.entry(info.sup).or_default().push(i);
        }
    }
    // A segment below MIN_SEG_LINES folds into the cheapest superset
    // segment (fewest assignments), or the residual when none exists:
    // a few extra evaluations of those lines beat a struct + fn + cache
    // array per tiny group. Sound because the target's support contains
    // the source's, so every value the source needs is still fixed per
    // assignment, and relative line order is restored by the sort below.
    // The target must remain visible to every group that reads the
    // moved binds: a reader group U necessarily has U ⊇ S (a reading
    // line's support includes the bind's), and after the merge it reads
    // from T, which it can only do if T ⊆ U (strict-subset dep) or
    // T == U (intra-segment). The residual can read any segment but no
    // group can read the residual, so that fallback needs zero group
    // readers. Tiny groups with conflicting readers just stay.
    const MIN_SEG_LINES: usize = 3;
    let mut unmergeable: BTreeSet<u8> = BTreeSet::new();
    loop {
        let texts: BTreeMap<u8, String> = seg_groups
            .iter()
            .map(|(s, v)| {
                let mut ix = v.clone();
                ix.sort_unstable();
                (*s, render_lines(&ix.iter().map(|&i| suf[i].clone()).collect::<Vec<_>>()))
            })
            .collect();
        let small = seg_groups
            .iter()
            .filter(|(s, v)| v.len() < MIN_SEG_LINES && !unmergeable.contains(s))
            .map(|(s, v)| (v.len(), *s))
            .min();
        let Some((_, s)) = small else { break };
        let binds: Vec<&String> = seg_groups[&s]
            .iter()
            .flat_map(|i| infos[*i].binds.iter())
            .collect();
        let readers: Vec<u8> = seg_groups
            .keys()
            .copied()
            .filter(|t| *t != s && binds.iter().any(|n| word_used(&texts[t], n)))
            .collect();
        let target = seg_groups
            .keys()
            .copied()
            .filter(|t| *t != s && t & s == s)
            .filter(|t| readers.iter().all(|u| u == t || u & t == *t))
            .min_by_key(|t| t.count_ones());
        if target.is_none() && !readers.is_empty() {
            unmergeable.insert(s);
            continue;
        }
        let idxs = seg_groups.remove(&s).unwrap();
        match target {
            Some(t) => seg_groups.get_mut(&t).unwrap().extend(idxs),
            None => residual_idx.extend(idxs),
        }
    }
    residual_idx.sort_unstable();
    let residual_lines: Vec<Line> = residual_idx.iter().map(|&i| suf[i].clone()).collect();
    let residual_text = render_lines(&residual_lines);
    let residual_epi = format!("{}\n{}", residual_text, epi_text);
    struct Seg2 {
        bits: Vec<u8>,
        text: String,
        exports: Vec<(String, &'static str)>,
        dp: Vec<u8>,
        bd: bool,
        deps: Vec<usize>,
        pass: bool,
    }
    let mut seg_order: Vec<u8> = seg_groups.keys().copied().collect();
    seg_order.sort_by_key(|s| (s.count_ones(), *s));
    let seg_texts: Vec<String> = seg_order
        .iter()
        .map(|s| {
            let mut idxs = seg_groups[s].clone();
            idxs.sort_unstable();
            render_lines(&idxs.iter().map(|&i| suf[i].clone()).collect::<Vec<_>>())
        })
        .collect();
    let mut segs: Vec<Seg2> = Vec::new();
    for (k, &sup) in seg_order.iter().enumerate() {
        let mut idxs = seg_groups[&sup].clone();
        idxs.sort_unstable();
        let text = seg_texts[k].clone();
        let outside_uses = |name: &str| -> bool {
            seg_texts[k + 1..].iter().any(|t| word_used(t, name))
                || word_used(&residual_epi, name)
        };
        let mut exports: Vec<(String, &'static str)> = Vec::new();
        for i in &idxs {
            for name in &infos[*i].binds {
                if outside_uses(name) {
                    let ty = *var_ty
                        .get(name)
                        .ok_or_else(|| anyhow!("no type for segment export {}", name))?;
                    exports.push((name.clone(), ty));
                }
            }
        }
        let dp: Vec<u8> = reg_masks
            .iter()
            .copied()
            .filter(|m| word_used(&text, &reg(*m)))
            .collect();
        let bd = word_used(&text, "bd");
        if exports.is_empty() && dp.is_empty() && !bd {
            bail!("suffix segment {:#08b} has no observable effect", sup);
        }
        let deps: Vec<usize> = (0..k)
            .filter(|&j| segs[j].exports.iter().any(|(n, _)| word_used(&text, n)))
            .collect();
        let pass = !dp.is_empty()
            || bd
            || exports.iter().any(|(n, _)| word_used(&residual_epi, n));
        let bits: Vec<u8> = (0..6).filter(|b| sup >> b & 1 != 0).collect();
        segs.push(Seg2 { bits, text, exports, dp, bd, deps, pass });
    }
    eprintln!(
        "[fused] M1 segments: {} ({:?} lines), residual {} of {} suffix lines",
        segs.len(),
        seg_order.iter().map(|s| seg_groups[s].len()).collect::<Vec<_>>(),
        residual_idx.len(),
        suf.len()
    );
    // Assignment plumbing: `expand` scatters a dense assignment index
    // over the segment's bit positions (the const A passed to seg fns);
    // `pack` projects a full 6-bit variant/assignment pattern back to a
    // cache index. Both resolve at emission time.
    let expand = |i: usize, bits: &[u8]| -> u8 {
        bits.iter().enumerate().map(|(j, b)| (((i >> j) & 1) as u8) << b).sum()
    };
    let pack = |a: u8, bits: &[u8]| -> usize {
        bits.iter().enumerate().map(|(j, b)| (((a >> b) & 1) as usize) << j).sum()
    };

    writeln!(out, "pub struct Pre {{")?;
    for name in &crossing {
        let ty = var_ty
            .get(name)
            .ok_or_else(|| anyhow!("no type for crossing var {}", name))?;
        writeln!(out, "    {}: {},", name, ty)?;
    }
    writeln!(out, "    valid: u16,")?;
    for mask in &reg_masks {
        writeln!(out, "    {}: u16,", reg(*mask))?;
    }
    writeln!(out, "    bd: bool,")?;
    writeln!(out, "}}\n")?;

    // Segment structs + fns (M1 stage 2). Exported fields are only the
    // names some LATER segment, the residual, or the epilogue reads;
    // dp/bd fields are the segment's own deltas, accumulated from zero.
    for (k, sg) in segs.iter().enumerate() {
        writeln!(
            out,
            "/// Suffix nodes supported by button bits {:?} only: {} distinct\n\
             /// evaluations, computed once each in frame() and read per variant.",
            sg.bits,
            1usize << sg.bits.len()
        )?;
        writeln!(out, "struct Seg{} {{", k)?;
        for (n, ty) in &sg.exports {
            writeln!(out, "    {}: {},", n, ty)?;
        }
        for m in &sg.dp {
            writeln!(out, "    {}: u16,", reg(*m))?;
        }
        if sg.bd {
            writeln!(out, "    bd: bool,")?;
        }
        writeln!(out, "}}")?;
        write!(
            out,
            "#[allow(unused_variables)]\n\
             #[inline(never)]\n\
             fn seg_{}<const A: u8>(u: &Uni, g: &G, p: &Pre",
            k
        )?;
        for j in &sg.deps {
            write!(out, ", s{j}: &Seg{j}", j = j)?;
        }
        writeln!(out, ") -> Seg{} {{", k)?;
        for name in &crossing {
            if word_used(&sg.text, name) {
                writeln!(out, "    let {n} = p.{n};", n = name)?;
            }
        }
        for j in &sg.deps {
            for (n, _) in &segs[*j].exports {
                if word_used(&sg.text, n) {
                    writeln!(out, "    let {n} = s{j}.{n};", n = n, j = j)?;
                }
            }
        }
        for m in &sg.dp {
            writeln!(out, "    let mut {}: u16 = 0;", reg(*m))?;
        }
        if sg.bd {
            writeln!(out, "    let mut bd_flag: bool = false;")?;
            writeln!(out, "    let bd: &mut bool = &mut bd_flag;")?;
        }
        for b in &sg.bits {
            writeln!(out, "    let kb{b}: bool = (A >> {b}) & 1 != 0;", b = b)?;
        }
        out.push_str(&sg.text);
        writeln!(out, "    Seg{} {{", k)?;
        for (n, _) in &sg.exports {
            writeln!(out, "        {},", n)?;
        }
        for m in &sg.dp {
            writeln!(out, "        {},", reg(*m))?;
        }
        if sg.bd {
            writeln!(out, "        bd: bd_flag,")?;
        }
        writeln!(out, "    }}")?;
        writeln!(out, "}}\n")?;
    }

    // frame()
    // The callback's leading `cfg` is a 1-based counter of PREFIX FORK
    // combinations: everything the variant sweep reads (p, osh, segment
    // caches) is recomputed per combination, so suffix::<B> runs once per
    // (cfg, B) and any executor-side per-variant caching must key on cfg
    // too. Missing this was a real bug (2026-08-20): key partials cached
    // per group leaked across fork combos.
    writeln!(
        out,
        "#[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, out: &mut impl FnMut(u32, u8, &KOutShared, &KOut, &Dy)) {{\n\
         \x20   let mut cfg: u32 = 0;"
    )?;
    for mask in &reg_masks {
        writeln!(out, "    let mut {}: u16 = 0;", reg(*mask))?;
    }
    writeln!(
        out,
        "    let mut bd_flag: bool = false;\n\
         \x20   let bd: &mut bool = &mut bd_flag;"
    )?;
    out.push_str(&pre_text);
    writeln!(out, "    let p = Pre {{")?;
    for name in &crossing {
        writeln!(out, "        {},", name)?;
    }
    writeln!(out, "        valid: {},", valid_expr)?;
    for mask in &reg_masks {
        writeln!(out, "        {},", reg(*mask))?;
    }
    writeln!(out, "        bd: *bd,")?;
    writeln!(out, "    }};")?;
    writeln!(out, "    let osh = KOutShared {{")?;
    for (id, _, expr, tainted) in &fused_of.fields {
        if !*tainted {
            writeln!(out, "        c{}: {},", id, expr)?;
        }
    }
    writeln!(out, "    }};")?;
    writeln!(out, "    cfg += 1;")?;
    // Segment caches: one array per support set, one element per
    // assignment of that set. Dependencies (strict-subset supports) are
    // read through the assignment's projection, resolved right here.
    for (k, sg) in segs.iter().enumerate() {
        let n_asg = 1usize << sg.bits.len();
        writeln!(out, "    let sc{}: [Seg{}; {}] = [", k, k, n_asg)?;
        for i in 0..n_asg {
            let a = expand(i, &sg.bits);
            write!(out, "        seg_{}::<{}>(u, g, &p", k, a)?;
            for j in &sg.deps {
                write!(out, ", &sc{}[{}]", j, pack(a, &segs[*j].bits))?;
            }
            writeln!(out, "),")?;
        }
        writeln!(out, "    ];")?;
    }
    let used: Vec<u32> = (0..6)
        .filter(|k| mentions_ident(&suffix_scan, &format!("kb{}", k)))
        .collect();
    let n_variants = 1usize << used.len();
    eprintln!(
        "[fused] suffix observes button bits {:?} -> {} variant call(s)",
        used, n_variants
    );
    writeln!(
        out,
        "    // suffix observes button bits {:?}: {} distinct variant(s)",
        used, n_variants
    )?;
    for i in 0..n_variants {
        let b: u8 = used
            .iter()
            .enumerate()
            .filter(|(j, _)| i >> j & 1 != 0)
            .map(|(_, k)| 1u8 << k)
            .sum();
        write!(out, "    suffix::<{}>(u, g, &p, &osh", b)?;
        for (k, sg) in segs.iter().enumerate() {
            if sg.pass {
                write!(out, ", &sc{}[{}]", k, pack(b, &sg.bits))?;
            }
        }
        writeln!(out, ", cfg, out);")?;
    }
    for _ in 0..fork_count {
        writeln!(out, "    }}")?;
    }
    writeln!(out, "}}\n")?;

    // suffix(): per-variant residual. Crossing/kb/segment-name binds are
    // emitted only where the residual or the epilogue reads them; each
    // dp register starts from the shared prefix value OR the passed
    // segments' deltas at this variant's projections.
    write!(
        out,
        "#[allow(unused_variables)]\n\
         #[inline(never)]\n\
         fn suffix<const B: u8>(u: &Uni, g: &G, p: &Pre, osh: &KOutShared"
    )?;
    for (k, sg) in segs.iter().enumerate() {
        if sg.pass {
            write!(out, ", s{k}: &Seg{k}", k = k)?;
        }
    }
    writeln!(out, ", cfg: u32, out: &mut impl FnMut(u32, u8, &KOutShared, &KOut, &Dy)) {{")?;
    for name in &crossing {
        if word_used(&residual_epi, name) {
            writeln!(out, "    let {} = p.{};", name, name)?;
        }
    }
    for mask in &reg_masks {
        let m = if residual_text.contains(&format!("&mut {})", reg(*mask))) {
            "mut "
        } else {
            ""
        };
        write!(out, "    let {m}{r}: u16 = p.{r}", m = m, r = reg(*mask))?;
        for (k, sg) in segs.iter().enumerate() {
            if sg.pass && sg.dp.contains(mask) {
                write!(out, " | s{}.{}", k, reg(*mask))?;
            }
        }
        writeln!(out, ";")?;
    }
    write!(out, "    let mut bd_flag: bool = p.bd")?;
    for (k, sg) in segs.iter().enumerate() {
        if sg.pass && sg.bd {
            write!(out, " | s{}.bd", k)?;
        }
    }
    writeln!(out, ";")?;
    writeln!(out, "    let bd: &mut bool = &mut bd_flag;")?;
    for k in 0..6 {
        writeln!(out, "    let kb{}: bool = (B >> {}) & 1 != 0;", k, k)?;
    }
    for (k, sg) in segs.iter().enumerate() {
        if !sg.pass {
            continue;
        }
        for (n, _) in &sg.exports {
            if word_used(&residual_epi, n) {
                writeln!(out, "    let {n} = s{k}.{n};", n = n, k = k)?;
            }
        }
    }
    out.push_str(&residual_text);
    for mi in 0..n_members {
        writeln!(out, "    let dp_of_{}: u16 = {};", mi, member_dp(mi))?;
    }
    for mi in 1..n_members {
        let earlier: String = (1..mi)
            .map(|j| format!(" & !cov_{}", j))
            .collect::<Vec<_>>()
            .join("");
        writeln!(
            out,
            "    let cov_{m}: u16 = p.valid & dp_of_0 & !dp_of_{m}{e};",
            m = mi,
            e = earlier
        )?;
    }
    writeln!(out, "    out(cfg, B, osh, &KOut {{")?;
    writeln!(out, "        valid: p.valid,")?;
    writeln!(out, "        deopt: dp_of_0,")?;
    writeln!(out, "        bd: *bd,")?;
    for (id, _, expr, tainted) in &fused_of.fields {
        if *tainted {
            writeln!(out, "        c{}: {},", id, expr)?;
        }
    }
    writeln!(out, "    }}, &Dy {{")?;
    write!(out, "        covered: [")?;
    for mi in 1..n_members {
        write!(out, "cov_{}, ", mi)?;
    }
    writeln!(out, "],")?;
    writeln!(out, "        tuples: [")?;
    for tup in &tuples {
        write!(out, "            DyTuple {{ ")?;
        for (id, _, expr) in tup {
            write!(out, "c{}: {}, ", id, fused_expr(expr))?;
        }
        writeln!(out, "}},")?;
    }
    writeln!(out, "        ],")?;
    writeln!(out, "    }});")?;
    writeln!(out, "}}")?;

    // --- M1 stage 3 (task #152): support-factored row keys ---
    // The pre-dedup key is a commutative per-cell SUM (kernel.rs
    // emit_key_cell): h[i] += mix64(cell_const ^ code(v)), finalized by
    // one mix64. Regrouping the cells therefore yields byte-identical
    // keys, as long as every cell keeps its KEY_CELLS index j (which the
    // runtime KeyPlan masks address). Cells split three ways by source:
    //   base  - sh / chunk only: identical across variants, computed
    //           once per 16-lane group;
    //   class - kv cells whose expr has support S below the full mask:
    //           identical for variants agreeing on S, cached per
    //           assignment of S (KEY_CLASS_SUPS names the classes);
    //   var   - full-support kv cells: genuinely per variant.
    // The executor (dispatch.rs run_fused) assembles
    //   keys = fin(base + class partials + var)
    // with wrapping_add, the commutative sum above. The monolithic
    // row_keys stays as-is for one-shot callers.
    let cells = key_cells(&primary.e, &fused_of)?;
    let mut base_cells: Vec<(usize, u32, &'static str, bool)> = Vec::new();
    let mut class_cells: BTreeMap<u8, Vec<(usize, u32, &'static str, bool)>> = BTreeMap::new();
    let mut var_cells: Vec<(usize, u32, &'static str, bool)> = Vec::new();
    for (j, (id, ty, tainted)) in cells.iter().enumerate() {
        if !*tainted || matches!(*ty, "IN_N" | "IN_B") {
            // sh- or chunk-sourced: never reads the per-variant KOut.
            base_cells.push((j, *id, ty, *tainted));
            continue;
        }
        let expr = fused_of
            .fields
            .iter()
            .find(|(fid, _, _, _)| fid == id)
            .map(|(_, _, e, _)| e.as_str())
            .ok_or_else(|| anyhow!("tainted key cell {} is not an out field", id))?;
        let mut sup = sup_of(expr, &name_sup);
        // dp registers and bd are per-variant accumulations name_sup
        // does not model; an out expr reading one is full-support.
        if reg_masks.iter().any(|m| word_used(expr, &reg(*m))) || word_used(expr, "bd") {
            sup = used_mask;
        }
        if sup == used_mask {
            var_cells.push((j, *id, ty, *tainted));
        } else {
            class_cells.entry(sup).or_default().push((j, *id, ty, *tainted));
        }
    }
    eprintln!(
        "[fused] M1 key cells: {} base, {} class(es) {:?} with {:?} cells, {} full of {}",
        base_cells.len(),
        class_cells.len(),
        class_cells.keys().map(|s| format!("{:#08b}", s)).collect::<Vec<_>>(),
        class_cells.values().map(|v| v.len()).collect::<Vec<_>>(),
        var_cells.len(),
        cells.len()
    );
    writeln!(
        out,
        "/// Variant-independent key cells (sh/chunk sources): the shared base\n\
         /// of the support-factored pre-dedup key. ADDS into h1/h2.\n\
         #[allow(unused_variables)]\n\
         pub fn row_keys_base(chunk: &Rt2, lo: usize, n: usize, sh: &KOutShared, plan: &KeyPlan, h1: &mut [u64; W], h2: &mut [u64; W]) {{"
    )?;
    for (j, id, ty, tainted) in &base_cells {
        emit_key_cell(&mut out, *j, *id, ty, *tainted)?;
    }
    writeln!(out, "}}\n")?;
    writeln!(
        out,
        "/// Button-bit support of each non-full kv key-cell class, in\n\
         /// `row_keys_class` arm order."
    )?;
    write!(out, "pub const KEY_CLASS_SUPS: &[u8] = &[")?;
    for s in class_cells.keys() {
        write!(out, "{:#08b}, ", s)?;
    }
    writeln!(out, "];")?;
    writeln!(
        out,
        "/// One class's kv cells: identical for variants agreeing on the\n\
         /// class's support, so the executor caches the result per\n\
         /// assignment. ADDS into h1/h2.\n\
         #[allow(unused_variables)]\n\
         pub fn row_keys_class(class: usize, kv: &KOut, plan: &KeyPlan, h1: &mut [u64; W], h2: &mut [u64; W]) {{\n\
         \x20   match class {{"
    )?;
    for (a, (s, ccells)) in class_cells.iter().enumerate() {
        writeln!(out, "    {} => {{ // support {:#08b}", a, s)?;
        for (j, id, ty, tainted) in ccells {
            emit_key_cell(&mut out, *j, *id, ty, *tainted)?;
        }
        writeln!(out, "    }}")?;
    }
    writeln!(
        out,
        "    _ => unreachable!(),\n\
         \x20   }}\n\
         }}\n"
    )?;
    writeln!(
        out,
        "/// Full-support kv cells: the only per-variant hashing. ADDS into h1/h2.\n\
         #[allow(unused_variables)]\n\
         pub fn row_keys_var(kv: &KOut, plan: &KeyPlan, h1: &mut [u64; W], h2: &mut [u64; W]) {{"
    )?;
    for (j, id, ty, tainted) in &var_cells {
        emit_key_cell(&mut out, *j, *id, ty, *tainted)?;
    }
    writeln!(out, "}}\n")?;
    writeln!(
        out,
        "/// Finalize a factored key: the same final mix as `row_keys`.\n\
         pub fn row_keys_fin(h1: &[u64; W], h2: &[u64; W], keys: &mut [(u64, u64); W]) {{\n\
         \x20   for i in 0..W {{\n\
         \x20       keys[i] = (mix64(h1[i]), mix64(h2[i]));\n\
         \x20   }}\n\
         }}"
    )?;

    // Self-fingerprint: a hash of everything emitted ABOVE this line, so
    // the artifact compiled into a binary names itself. The campaign
    // fingerprint hashes this const when the fused engine is active -
    // change any member recipe, the witness, or this emitter and the
    // artifact bytes change, so checkpoints from different fused engines
    // can never be resumed into each other (plans/shape-tag-plan.md).
    // Hashing the emitted STRING (not the on-disk file at runtime) is
    // deliberate: it fingerprints what the binary RUNS, not what happens
    // to be on disk next to it.
    let fp = {
        use std::hash::{Hash, Hasher};
        let mut h = rustc_hash::FxHasher::default();
        out.hash(&mut h);
        h.finish()
    };
    writeln!(out, "\n/// Hash of this artifact's own bytes above this line.")?;
    writeln!(out, "pub const FUSED_FINGERPRINT: u64 = {:#018x};", fp)?;

    eprintln!(
        "fused: {} members, {} nodes ({} shared by all), {} guards, {} dp registers, prefix {} lines, suffix {} lines",
        n_members,
        ctx.nodes.len(),
        ctx.nodes.values().filter(|d| d.members == all_mask).count(),
        ctx.guards.len(),
        reg_masks.len(),
        pre_text.lines().count(),
        suf_text.lines().count(),
    );
    Ok(out)
}
