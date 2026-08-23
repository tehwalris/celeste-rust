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
    emit_interface, emit_key_cell, emit_walk, key_cells, lower_walk, reachable_cells,
    render_lines, Emit, Line, OutField, OutFields,
};
use crate::program::Program;


/// One value node of the shared graph.
struct NodeDef {
    ty: &'static str,
    /// Canonical renderable expr, operands as `<id>`.
    render: String,
    /// Which members contain this node (bit i = member i).
    members: u8,
}

/// A structural item of one member's line stream, in stream order.
///
/// It used to need four variants. Guards (`zguard`), bails (`*bd = true`)
/// and `zi_split_at` are all gone: they were STATEMENTS with side effects,
/// and once validity became a value the conditions behind them became
/// ordinary nodes that fuse like any other. What is left is values, and
/// the one genuine control structure.
enum PItem {
    Node(u32),
    /// `for cK { ... zi_fork_flr ... }` opening group - fork node id.
    ForkOpen(u32),
}

struct ForkDef {
    depth: usize,
    op: u32,
}

/// The shared numbering across all members.
struct Ctx {
    intern: HashMap<String, u32>,
    nodes: BTreeMap<u32, NodeDef>,
    forks: BTreeMap<u32, ForkDef>,
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
    /// Items split into fork segments: segment k = items before the k-th
    /// fork open; the fork open itself starts segment k+1. There is no
    /// second region any more - the free choices are eliminated before
    /// this ever sees the body, so there is no button suffix to keep
    /// apart from a prefix.
    segs: Vec<Vec<PItem>>,
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
fn parse_member(label: &str, mut e: Emit, mi: usize, ctx: &mut Ctx) -> Result<PMember> {
    let of = lower_walk(&mut e)?;
    let mbit = 1u8 << mi;
    let mut vn: HashMap<String, u32> = HashMap::new();
    let mut loads: Vec<String> = Vec::new();
    let mut segs: Vec<Vec<PItem>> = vec![Vec::new()];

    {
        let mut iter = e.body.iter().peekable();
        while let Some(line) = iter.next() {
            let items: &mut Vec<PItem> = segs.last_mut().unwrap();
            match line {
                Line::Let { name, ty, expr } => {
                    if expr.contains(", &mut dp") {
                        bail!(
                            "member {}: node {} still threads a deopt channel ({:?}). \
                             Validity is a value; a node with a side effect cannot be \
                             shared between members.",
                            label, name, expr
                        );
                    }
                    let canon = canonicalize(expr, &vn);
                    let id = ctx.intern(&canon);
                    let render = canon.clone();
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
                            ctx.nodes.insert(id, NodeDef { ty, render, members: mbit });
                        }
                    }
                    vn.insert(name.clone(), id);
                    items.push(PItem::Node(id));
                }
                Line::Raw(s) => {
                    if s.starts_with("let r_c") {
                        loads.push(s.clone());
                    } else if s.starts_with("for c") && s.ends_with(" in 0..2usize {") {
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
                            .and_then(|t| t.strip_suffix(");"))
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
                        segs.last_mut().unwrap().push(PItem::ForkOpen(id));
                        segs.push(Vec::new());
                    } else {
                        bail!("member {}: unclassified raw line {:?}", label, s);
                    }
                }
            }
        }
    }
    Ok(PMember { label: label.to_string(), e, of, segs, loads, vn })
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
            let n: usize = pm.segs.iter().map(|s| s.len()).sum::<usize>();
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
        if pm.segs.len() != primary.segs.len() {
            bail!("member {} fork segment count differs", pm.label);
        }
    }
    // Fork skeletons: the k-th fork of every member must be the same node.
    let fork_seq = |pm: &PMember| -> Vec<u32> {
        pm.segs
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
        for OutField { cell: id, ty, expr, .. } in &pm.of.fields {
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
                        .find(|f| f.cell == *id)
                        .map(|f| (f.ty, &f.expr))
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

    // No deopt registers. Each member's validity is its own `ok_out`
    // node in the shared numbering, so "which lanes did member m get
    // right" is a value to read rather than a side effect to route. The
    // `dp_m{memberset}` registers existed only because a shared node
    // could write a deopt that belonged to a subset of its members.

    // --- fused line streams (lockstep interleave by fork segment) ---
    let mut pre: Vec<Line> = Vec::new();
    let mut emitted: BTreeSet<u32> = BTreeSet::new();
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
    let mut fork_members: BTreeMap<u32, u8> = BTreeMap::new();
    for (mi, pm) in pms.iter().enumerate() {
        for it in pm.segs.iter().flatten() {
            match it {
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
                    let expr = fused_expr(&def.render);
                    var_ty.insert(name.clone(), def.ty);
                    if region == 0 {
                        pre_defs.insert(name.clone());
                    }
                    out.push(Line::Let { name, ty: def.ty, expr });
                }
                PItem::ForkOpen(id) => {
                    if !emitted.insert(*id) {
                        return Ok(());
                    }
                    let def = &ctx.forks[id];
                    let name = format!("n{}", id);
                    out.push(Line::Raw(format!("for c{} in 0..2usize {{", def.depth)));
                    out.push(Line::Raw(format!(
                        "let ({n}, {n}_fv): (ZI, u16) = zi_fork_flr(n{op}, c{d});",
                        n = name,
                        op = def.op,
                        d = def.depth
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
        let n_segs = primary.segs.len();
        for s in 0..n_segs {
            // Non-fork items of this segment, member by member.
            for pm in &pms {
                for it in &pm.segs[s] {
                    if matches!(it, PItem::ForkOpen(_)) {
                        continue;
                    }
                    emit_item(it, 0, &mut pre, &mut pre_defs, &mut var_ty, &mut valid_stack, &mut fork_count)?;
                }
            }
            // The fork closing this segment (identical across members).
            if let Some(PItem::ForkOpen(id)) = primary.segs[s]
                .iter()
                .find(|it| matches!(it, PItem::ForkOpen(_)))
            {
                emit_item(&PItem::ForkOpen(*id), 0, &mut pre, &mut pre_defs, &mut var_ty, &mut valid_stack, &mut fork_count)?;
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
    // --- fused out fields (primary member's, exprs renamed) ---
    let fused_of = OutFields {
        fields: primary
            .of
            .fields
            .iter()
            .map(|f| OutField {
                cell: f.cell,
                ty: f.ty,
                expr: fused_expr(&canonicalize(&f.expr, &primary.vn)),
                tainted: f.tainted,
                node: f.node,
                konst: f.konst.clone(),
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

    let body_text = render_lines(&pre);

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
            .find(|f| f.cell == *id)
            .map(|f| f.tainted)
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

    // --- frame(): one flat body, one call per FUSED variant ---
    //
    // The fused variant set is COARSER than any single member's. Member i
    // collapsed 64 assignments onto its own representatives; two
    // assignments are interchangeable HERE only if every member agrees,
    // so the fused set is keyed on the tuple of the members' reps.
    let mut fused_sig: BTreeMap<Vec<u8>, u8> = BTreeMap::new();
    for m in 0u8..64 {
        let sig: Vec<u8> = pms.iter().map(|pm| pm.e.rep_of[m as usize]).collect();
        fused_sig.entry(sig).or_insert(m);
    }
    let mut fused_reps: Vec<u8> = fused_sig.values().copied().collect();
    fused_reps.sort_unstable();
    eprintln!(
        "[fused] {} distinct assignments of 64 (members alone: {:?})",
        fused_reps.len(),
        pms.iter().map(|pm| pm.e.variants.len()).collect::<Vec<_>>()
    );

    // Member mi's result under assignment m, renamed into the shared
    // numbering. A member that bailed the whole slice covers nothing.
    let var_of = |mi: usize, m: u8| -> Result<&crate::transpile::lower::Variant> {
        let rep = pms[mi].e.rep_of[m as usize];
        pms[mi]
            .e
            .variants
            .iter()
            .find(|v| v.mask == rep)
            .ok_or_else(|| anyhow!("member {} has no variant {}", pms[mi].label, rep))
    };
    let shared_name = |mi: usize, local: &str| -> Result<String> {
        let id = pms[mi].vn.get(local).ok_or_else(|| {
            anyhow!("member {} has no {} in the shared numbering", pms[mi].label, local)
        })?;
        Ok(format!("n{}", id))
    };
    let renamed = |mi: usize, expr: &str| -> String {
        fused_expr(&canonicalize(expr, &pms[mi].vn))
    };

    writeln!(
        out,
        "#[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, out: &mut impl FnMut(u32, u8, &KOutShared, &KOut, &Dy)) {{\n\
         \x20   let mut cfg: u32 = 0;"
    )?;
    out.push_str(&body_text);
    writeln!(out, "    let osh = KOutShared {{")?;
    for OutField { cell: id, expr, tainted, .. } in &fused_of.fields {
        if !*tainted {
            writeln!(out, "        c{}: {},", id, expr)?;
        }
    }
    writeln!(out, "    }};")?;
    writeln!(out, "    cfg += 1;")?;
    for m in &fused_reps {
        // Guard-as-selector, over VALUES: a lane belongs to the first
        // member (argument order) that got it right.
        for mi in 0..n_members {
            let rep = pms[mi].e.rep_of[*m as usize];
            writeln!(
                out,
                "    let ok_of_{mi}: u16 = if {bd} {{ 0 }} else {{ {ok} }};",
                mi = mi,
                bd = shared_name(mi, &format!("bd_v{}", rep))?,
                ok = shared_name(mi, &format!("ok_v{}", rep))?
            )?;
        }
        for mi in 1..n_members {
            let earlier: String = (1..mi).map(|j| format!(" & !cov_{}", j)).collect();
            writeln!(
                out,
                "    let cov_{m}: u16 = {v} & !ok_of_0 & ok_of_{m}{e};",
                m = mi,
                v = valid_expr,
                e = earlier
            )?;
        }
        writeln!(out, "    out(cfg, {}, &osh, &KOut {{", m)?;
        writeln!(out, "        valid: {},", valid_expr)?;
        writeln!(out, "        deopt: !ok_of_0,")?;
        writeln!(
            out,
            "        bd: {},",
            shared_name(0, &format!("bd_v{}", pms[0].e.rep_of[*m as usize]))?
        )?;
        let pv = var_of(0, *m)?;
        for OutField { cell: id, tainted, .. } in &fused_of.fields {
            if *tainted {
                writeln!(out, "        c{}: {},", id, renamed(0, &pv.per[0].outputs[id]))?;
            }
        }
        writeln!(out, "    }}, &Dy {{")?;
        write!(out, "        covered: [")?;
        for mi in 1..n_members {
            write!(out, "cov_{}, ", mi)?;
        }
        writeln!(out, "],")?;
        writeln!(out, "        tuples: [")?;
        for mi in 1..n_members {
            let mv = var_of(mi, *m)?;
            write!(out, "            DyTuple {{ ")?;
            for (id, _, _) in tuple_cells {
                write!(out, "c{}: {}, ", id, renamed(mi, &mv.per[0].outputs[id]))?;
            }
            writeln!(out, "}},")?;
        }
        writeln!(out, "        ],")?;
        writeln!(out, "    }});")?;
    }
    for _ in 0..fork_count {
        writeln!(out, "    }}")?;
    }
    writeln!(out, "}}\n")?;

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
    let class_cells: BTreeMap<u8, Vec<(usize, u32, &'static str, bool)>> = BTreeMap::new();
    let mut var_cells: Vec<(usize, u32, &'static str, bool)> = Vec::new();
    for (j, (id, ty, tainted)) in cells.iter().enumerate() {
        if !*tainted || matches!(*ty, "IN_N" | "IN_B") {
            // sh- or chunk-sourced: never reads the per-variant KOut.
            base_cells.push((j, *id, ty, *tainted));
            continue;
        }
        // Tainted means the VARIANTS DISAGREE about this cell - that is
        // now an exact fact from specialization, not a support analysis
        // over button bits, so there is no middle class left. A cell is
        // either the same for every variant (base, hashed once per
        // 16-lane group) or genuinely per variant.
        var_cells.push((j, *id, ty, *tainted));
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
        "fused: {} members, {} nodes ({} shared by all), body {} lines",
        n_members,
        ctx.nodes.len(),
        ctx.nodes.values().filter(|d| d.members == all_mask).count(),
        body_text.lines().count(),
    );
    Ok(out)
}
