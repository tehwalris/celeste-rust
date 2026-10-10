//! `rewrite storage-census`: a finished tree's recorded edges counted
//! against the alternative target layouts of plans/storage-unify.md - the
//! built one (per frame and unit, a lid table), Philippe's unified one (per
//! source region ONE persistent set of own and target-only patterns, a
//! target named by (pattern, region delta)), and the hybrids between them.
//! Counting only: nothing here is timed, and nothing is written but the
//! report.
//!
//! Per frame, every edge `(src, (Q, e_Q, c))` with the source's region `R`:
//!   * `P`: the distinct `(R, Q, e_Q)` - the lids if units were exactly
//!     source regions (`P_in`: `Q == R`, the target is one of R's own
//!     entries; `P_x`: a neighbour's or another shape's);
//!   * the unified set's patterns: `(R, g)` for `Q != R`, `g` the target's
//!     interned (shape, key) - an own entry of R (shared), or target-only,
//!     new this frame or reused from an earlier one;
//!   * the translations `(R, Q, e_Q)`, `Q != R`: new this frame or reused
//!     (persistent across frames);
//!   * per built unit, its ghost lids (targets some source of the unit
//!     reaches from another region).

use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};

use super::edges::EdgeStore;
use super::visited::Key;
use super::id_region;

#[derive(Default, Clone, Copy)]
struct Totals {
    edges: u64,
    units: u64,
    lids: u64,
    ghost_lids_units: u64,
    p: u64,
    p_in: u64,
    p_x: u64,
    edges_in: u64,
    edges_x_shape: u64,
    targets: u64,
    d_shared: u64,
    d_t_new: u64,
    d_t_reused: u64,
    d_converted: u64,
    x_new: u64,
    x_reused: u64,
    // Bytes of the built files.
    index_bytes: u64,
    blk_bytes: u64,
    starts_bytes: u64,
    xfer_bytes: u64,
    src_bytes: u64,
    owner_bytes: u64,
    owner_varint: u64,
    namers_units: u64,
    namers_regions: u64,
    new_entries: u64,
    /// Direct names in the built units: group headers (Q, e) delta-coded
    /// with a varint length each; lids naming an entry new this frame.
    hdr_direct: u64,
    pending: u64,
    /// The unified layout: group headers per source region (length taken
    /// as 2 B), and its reverse index `(Q, e, R, rank)` delta-coded.
    hdr_unified: u64,
    rev_unified: u64,
    /// Shape-global key numbers: group headers (Q, g); lids naming a key
    /// new to its SHAPE this frame.
    hdr_global: u64,
    pending_global: u64,
    /// The built `starts` as varint lengths.
    starts_varint: u64,
}

impl Totals {
    fn add(&mut self, o: &Totals) {
        macro_rules! sum {
            ($($f:ident),*) => { $( self.$f += o.$f; )* };
        }
        sum!(edges, units, lids, ghost_lids_units, p, p_in, p_x, edges_in, edges_x_shape, targets, d_shared, d_t_new, d_t_reused, d_converted, x_new, x_reused, index_bytes, blk_bytes, starts_bytes, xfer_bytes, src_bytes, owner_bytes, owner_varint, namers_units, namers_regions, new_entries, hdr_direct, pending, hdr_unified, rev_unified, hdr_global, pending_global, starts_varint);
    }
}

fn varint_len(mut v: u64) -> u64 {
    let mut n = 1;
    while v >= 0x80 {
        v >>= 7;
        n += 1;
    }
    n
}

/// A sorted index `(Q, e, owner, rank)` delta-coded: a new region its delta
/// and the entry, a new entry in the region its delta, then the owner (a
/// unit or a source region) and its rank there, varints.
fn index_bytes(v: &[(u32, u32, u32, u32)]) -> u64 {
    let (mut n, mut pq, mut pe) = (0u64, u32::MAX, 0u32);
    let mut last = (u32::MAX, u32::MAX);
    for &(q, e, o, r) in v {
        if (q, e) != last {
            if q != pq {
                n += varint_len(q.wrapping_sub(pq) as u64 & 0xffff_ffff) + varint_len(e as u64);
            } else {
                n += 1 + varint_len((e - pe) as u64);
            }
            (pq, pe) = (q, e);
            last = (q, e);
        } else {
            n += 1;
        }
        n += varint_len(o as u64) + varint_len(r as u64);
    }
    n
}

/// Group headers in `(Q, e)` order with each group's byte length.
fn header_bytes(groups: impl Iterator<Item = (u32, u32, u32)>) -> u64 {
    let (mut n, mut pq, mut pe) = (0u64, u32::MAX, 0u32);
    for (q, e, len) in groups {
        if q != pq {
            n += varint_len(q.wrapping_sub(pq) as u64 & 0xffff_ffff) + varint_len(e as u64);
        } else {
            n += varint_len((e - pe) as u64);
        }
        (pq, pe) = (q, e);
        n += varint_len(len as u64);
    }
    n
}

/// One unit's census: its distinct `(R, Q, e_Q)`, its ghost lids, edge
/// classes, sizes, and its owner index entries `(Q, e_Q, lid)`.
struct UnitCensus {
    refs: Vec<(u32, u32, u32)>,
    owners: Vec<(u32, u32, u32)>,
    ghost_lids: u64,
    hdr_direct: u64,
    pending: u64,
    hdr_global: u64,
    pending_global: u64,
    starts_varint: u64,
    edges: u64,
    edges_in: u64,
    edges_x_shape: u64,
    blk: u64,
    starts: u64,
    xfers: u64,
    src: u64,
}

/// The unified layout's edges of one frame, encoded as the built blocks are
/// (`edges::encode_block`'s scheme), per SOURCE REGION: groups `(Q, e_Q)` in
/// order, cell, source, transfer as its rank by use in the region. The
/// source (`mode`): 0 its rank among the region's sources this frame; 1 the
/// same with the region cut into parts of 4096 sources (a block and its
/// group headers per part, as a heavy region's units); 2 its persistent
/// local name `entry << 6 | cell` in the region; 3 as 1, the transfers
/// ranked per part (as a unit ranks its own). Returns (block bytes,
/// groups, distinct transfers summed over regions, the group headers' bytes).
fn unified_blocks(edges: &[(u32, u32, u32, u8, u64, u32)], mode: u32) -> (u64, u64, u64, u64) {
    let (mut bytes, mut groups, mut xfers, mut hdr) = (0u64, 0u64, 0u64, 0u64);
    let mut a = 0;
    while a < edges.len() {
        let r = edges[a].0;
        let b = a + edges[a..].partition_point(|e| e.0 == r);
        let part = &edges[a..b];
        let mut srcs: Vec<u64> = part.iter().map(|e| e.4).collect();
        srcs.sort_unstable();
        srcs.dedup();
        let mut use_: FxHashMap<u32, u32> = FxHashMap::default();
        for e in part {
            *use_.entry(e.5).or_default() += 1;
        }
        let mut by_use: Vec<(u32, u32)> = use_.iter().map(|(&x, &n)| (n, x)).collect();
        by_use.sort_unstable_by(|p, q| q.0.cmp(&p.0).then(p.1.cmp(&q.1)));
        let mut rank: FxHashMap<u32, u32> = by_use.iter().enumerate().map(|(i, &(_, x))| (x, i as u32)).collect();
        if mode != 3 {
            xfers += rank.len() as u64;
        }
        // Mode 3: mode 1 with the transfers ranked per part.
        let mut part_rank: FxHashMap<(u32, u32), u32> = FxHashMap::default();
        if mode == 3 {
            let mut uses: FxHashMap<(u32, u32), u32> = FxHashMap::default();
            for e in part {
                let k = srcs.binary_search(&e.4).unwrap() as u32;
                *uses.entry((k / 4096, e.5)).or_default() += 1;
            }
            let mut v: Vec<((u32, u32), u32)> = uses.into_iter().collect();
            v.sort_unstable_by(|p, q| p.0 .0.cmp(&q.0 .0).then(q.1.cmp(&p.1)).then(p.0 .1.cmp(&q.0 .1)));
            let mut i = 0;
            while i < v.len() {
                let pt = v[i].0 .0;
                let j = i + v[i..].partition_point(|x| x.0 .0 == pt);
                for (r, x) in v[i..j].iter().enumerate() {
                    part_rank.insert(x.0, r as u32);
                }
                xfers += (j - i) as u64;
                i = j;
            }
            rank.clear();
        }
        // (part, Q, e_Q, cell, source, transfer rank), sorted.
        let mut tuples: Vec<(u32, u32, u32, u8, u32, u32)> = part
            .iter()
            .map(|e| {
                let k = srcs.binary_search(&e.4).unwrap() as u32;
                let (pt, src) = match mode {
                    0 => (0, k),
                    1 | 3 => (k / 4096, k % 4096),
                    _ => (0, super::id_entry(e.4) << 6 | super::id_local(e.4)),
                };
                let x = if mode == 3 { part_rank[&(pt, e.5)] } else { rank[&e.5] };
                (pt, e.1, e.2, e.3, src, x)
            })
            .collect();
        tuples.sort_unstable();
        let mut k = 0;
        while k < tuples.len() {
            let pt = tuples[k].0;
            let mut heads: Vec<(u32, u32, u32)> = Vec::new();
            while k < tuples.len() && tuples[k].0 == pt {
                let g = (tuples[k].1, tuples[k].2);
                groups += 1;
                let at = bytes;
                let (mut prev_c, mut prev_s) = (u32::MAX, 0u32);
                while k < tuples.len() && (tuples[k].0, tuples[k].1, tuples[k].2) == (pt, g.0, g.1) {
                    let (_, _, _, c, s, x) = tuples[k];
                    let c = c as u32;
                    if prev_c == u32::MAX {
                        bytes += 1 + varint_len(s as u64);
                    } else if c == prev_c {
                        bytes += varint_len(((s - prev_s) as u64) << 1);
                    } else {
                        bytes += varint_len(((c - prev_c) as u64) << 1 | 1) + varint_len(s as u64);
                    }
                    bytes += varint_len(x as u64);
                    (prev_c, prev_s) = (c, s);
                    k += 1;
                }
                heads.push((g.0, g.1, (bytes - at) as u32));
            }
            hdr += header_bytes(heads.into_iter());
        }
        a = b;
    }
    (bytes, groups, xfers, hdr)
}

pub struct CensusArgs<'a> {
    pub dir: &'a std::path::Path,
    pub to: u32,
    /// Frames whose unified edges are encoded exactly (each holds the frame's
    /// edges in memory, 24 B each).
    pub encode: &'a [u32],
}

pub fn census(a: &CensusArgs) -> Result<()> {
    let dir = a.dir;
    let geo = *super::geometry();
    let eg = EdgeStore::open(&dir.join("edges"), a.to)?;
    let threads = crate::frame::threads();
    // (shape index, key) -> g; per region its entries' g; (R, g) -> own entry.
    let mut gkeys: FxHashMap<(u32, Key), u32> = FxHashMap::default();
    let mut ent_g: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
    let mut own: FxHashSet<(u32, u32)> = FxHashSet::default();
    // Target-only patterns: (R, g) -> (first frame, frames used, converted).
    let mut tonly: FxHashMap<(u32, u32), (u16, u16, bool)> = FxHashMap::default();
    // Persistent translations (R, Q, e_Q), Q != R: frames used.
    let mut xlat: FxHashMap<(u32, u32, u32), (u16, u16)> = FxHashMap::default();
    let mut all = Totals::default();
    let mut entries_total = 0u64;
    println!(
        "#frame edges units lids ghostlids_units | P P_in P_x | edges_in edges_xshape targets | D_shared D_tnew D_treused D_conv | X_new X_reused | own_entries_cum tonly_cum xlat_cum | bytes: index blk starts xfers src owner owner_varint | namers units regions | hdr_direct pending hdr_unified rev_unified hdr_global pending_global starts_varint"
    );
    for f in 0..=a.to {
        // The frame's new entries (end of frame f: a target new this frame
        // is an own entry by then).
        let before: FxHashMap<u32, u32> = ent_g.iter().map(|(&r, v)| (r, v.len() as u32)).collect();
        let g_before = gkeys.len() as u32;
        let mut new_entries = 0u64;
        if dir.join("frames").join(format!("f{f:03}")).is_dir() {
            for m in super::meta::load_frame(dir, f)? {
                let mut ents = m.entries;
                ents.sort_unstable();
                for (r, e, k) in ents {
                    let s = r / geo.slots;
                    let n = gkeys.len() as u32;
                    let g = *gkeys.entry((s, k)).or_insert(n);
                    let v = ent_g.entry(r).or_default();
                    anyhow::ensure!(e as usize == v.len(), "region {r}: entry {e} after {}", v.len());
                    v.push(g);
                    own.insert((r, g));
                    new_entries += 1;
                }
            }
        }
        entries_total += new_entries;
        if f == 0 || !eg.has_frame(f) {
            continue;
        }
        let owners = eg.owners(f);
        let units = eg.units(f);
        let encode = a.encode.contains(&f);
        let cens: Vec<(UnitCensus, Vec<(u32, u32, u32, u8, u64, u32)>)> = super::wave::par_map(&units, threads, |&(fi, ui)| {
            let u = eg.unit(f, &owners, fi, ui);
            let (blk, n_x, explicit) = u.layout();
            let mut refs: FxHashSet<(u32, u32, u32)> = FxHashSet::default();
            let mut ghost: FxHashSet<(u32, u32)> = FxHashSet::default();
            let (mut edges, mut edges_in, mut edges_x_shape) = (0u64, 0u64, 0u64);
            let mut raw = Vec::new();
            let srcs: Vec<u64> = (0..u.n_sources() as u32).map(|s| u.source(s)).collect();
            u.edges(|lid, c, s, x| {
                let (q, e) = u.lid_owner(lid).expect("an edge names an owned lid");
                let src = srcs[s as usize];
                let r = id_region(src);
                edges += 1;
                if q == r {
                    edges_in += 1;
                } else {
                    ghost.insert((q, e));
                    if q / geo.slots != r / geo.slots {
                        edges_x_shape += 1;
                    }
                }
                refs.insert((r, q, e));
                if encode {
                    raw.push((r, q, e, c as u8, src, x));
                }
            });
            let mut owners_v: Vec<(u32, u32, u32)> = (0..u.n_lids() as u32).filter_map(|l| u.lid_owner(l).map(|(q, e)| (q, e, l))).collect();
            owners_v.sort_unstable();
            let hdr_direct = header_bytes(owners_v.iter().map(|&(q, e, l)| (q, e, u.lid_bytes(l))));
            let pending = owners_v.iter().filter(|&&(q, e, _)| e >= before.get(&q).copied().unwrap_or(0)).count() as u64;
            let mut by_g: Vec<(u32, u32, u32)> = owners_v.iter().map(|&(q, e, l)| (q, ent_g[&q][e as usize], u.lid_bytes(l))).collect();
            by_g.sort_unstable();
            let hdr_global = header_bytes(by_g.iter().copied());
            let pending_global = by_g.iter().filter(|x| x.1 >= g_before).count() as u64;
            let starts_varint: u64 = (0..u.n_lids() as u32).map(|l| varint_len(u.lid_bytes(l) as u64)).sum();
            let mut refs: Vec<(u32, u32, u32)> = refs.into_iter().collect();
            refs.sort_unstable();
            (
                UnitCensus {
                    refs,
                    owners: owners_v,
                    ghost_lids: ghost.len() as u64,
                    hdr_direct,
                    pending,
                    hdr_global,
                    pending_global,
                    starts_varint,
                    edges,
                    edges_in,
                    edges_x_shape,
                    blk,
                    starts: 4 * (u.n_lids() as u64 + 1),
                    xfers: 4 * n_x as u64,
                    src: if explicit { 8 * u.n_sources() as u64 } else { 0 },
                },
                raw,
            )
        });
        let mut t = Totals { units: cens.len() as u64, new_entries, ..Default::default() };
        let mut p: Vec<(u32, u32, u32)> = Vec::new();
        let mut owner_idx: Vec<(u32, u32, u32, u32)> = Vec::new();
        let mut raw_all = Vec::new();
        for (ui, (c, raw)) in cens.into_iter().enumerate() {
            t.edges += c.edges;
            t.edges_in += c.edges_in;
            t.edges_x_shape += c.edges_x_shape;
            t.lids += c.owners.len() as u64;
            t.ghost_lids_units += c.ghost_lids;
            t.hdr_direct += c.hdr_direct;
            t.pending += c.pending;
            t.hdr_global += c.hdr_global;
            t.pending_global += c.pending_global;
            t.starts_varint += c.starts_varint;
            t.blk_bytes += c.blk;
            t.starts_bytes += c.starts;
            t.xfer_bytes += c.xfers;
            t.src_bytes += c.src;
            p.extend(c.refs);
            owner_idx.extend(c.owners.into_iter().map(|(q, e, l)| (q, e, ui as u32, l)));
            raw_all.extend(raw);
        }
        for (bytes, n_owner, _) in eg.file_sizes(f) {
            t.index_bytes += bytes;
            t.owner_bytes += 16 * n_owner;
        }
        // The owner index delta-coded (`index_bytes`).
        owner_idx.sort_unstable();
        t.owner_varint = index_bytes(&owner_idx);
        t.targets = {
            let mut n = 0u64;
            let mut last = (u32::MAX, u32::MAX);
            for &(q, e, ..) in &owner_idx {
                if (q, e) != last {
                    n += 1;
                    last = (q, e);
                }
            }
            n
        };
        t.namers_units = owner_idx.len() as u64;
        drop(owner_idx);
        p.sort_unstable();
        p.dedup();
        t.p = p.len() as u64;
        // Namers per target in the region layout: (R, target) pairs.
        t.namers_regions = t.p;
        {
            let mut rev: Vec<(u32, u32, u32, u32)> = Vec::with_capacity(p.len());
            let mut i = 0;
            while i < p.len() {
                let r = p[i].0;
                let j = i + p[i..].partition_point(|x| x.0 == r);
                t.hdr_unified += header_bytes(p[i..j].iter().map(|&(_, q, e)| (q, e, 200)));
                rev.extend(p[i..j].iter().enumerate().map(|(k, &(_, q, e))| (q, e, r, k as u32)));
                i = j;
            }
            rev.sort_unstable();
            t.rev_unified = index_bytes(&rev);
        }
        let mut d_f: FxHashSet<(u32, u32)> = FxHashSet::default();
        for &(r, q, e) in &p {
            if q == r {
                t.p_in += 1;
                continue;
            }
            t.p_x += 1;
            let g = ent_g[&q][e as usize];
            d_f.insert((r, g));
            match xlat.get_mut(&(r, q, e)) {
                Some(x) => {
                    t.x_reused += 1;
                    x.1 += 1;
                }
                None => {
                    t.x_new += 1;
                    xlat.insert((r, q, e), (f as u16, 1));
                }
            }
        }
        for (r, g) in d_f {
            let is_own = own.contains(&(r, g));
            match tonly.get_mut(&(r, g)) {
                Some(x) => {
                    if is_own {
                        if !x.2 {
                            x.2 = true;
                            t.d_converted += 1;
                        } else {
                            t.d_shared += 1;
                        }
                    } else {
                        t.d_t_reused += 1;
                        x.1 += 1;
                    }
                }
                None if is_own => t.d_shared += 1,
                None => {
                    t.d_t_new += 1;
                    tonly.insert((r, g), (f as u16, 1, false));
                }
            }
        }
        let tonly_alive = tonly.values().filter(|x| !x.2).count();
        let enc = if encode {
            let n = raw_all.len();
            raw_all.sort_unstable();
            raw_all.dedup();
            let mut out = String::new();
            for mode in 0..4 {
                let (bytes, groups, xf, hdr) = unified_blocks(&raw_all, mode);
                out += &format!(
                    " | unified{mode} f{f}: {n} edges, blocks {bytes} B ({:.2} B an edge), groups {groups} (headers {hdr} B), region transfer ranks {xf} ({} B)",
                    bytes as f64 / n.max(1) as f64,
                    4 * xf
                );
            }
            out
        } else {
            String::new()
        };
        println!(
            "f{f:03} {} {} {} {} | {} {} {} | {} {} {} | {} {} {} {} | {} {} | {} {} {} | bytes: {} {} {} {} {} {} {} | namers {} {} | {} {} {} {} {} {} {}{enc}",
            t.edges,
            t.units,
            t.lids,
            t.ghost_lids_units,
            t.p,
            t.p_in,
            t.p_x,
            t.edges_in,
            t.edges_x_shape,
            t.targets,
            t.d_shared,
            t.d_t_new,
            t.d_t_reused,
            t.d_converted,
            t.x_new,
            t.x_reused,
            entries_total,
            tonly_alive,
            xlat.len(),
            t.index_bytes,
            t.blk_bytes,
            t.starts_bytes,
            t.xfer_bytes,
            t.src_bytes,
            t.owner_bytes,
            t.owner_varint,
            t.namers_units,
            t.namers_regions,
            t.hdr_direct,
            t.pending,
            t.hdr_unified,
            t.rev_unified,
            t.hdr_global,
            t.pending_global,
            t.starts_varint,
        );
        all.add(&t);
    }
    // Reuse of the persistent structures over the forward.
    let once_t = tonly.values().filter(|x| x.1 == 1 && !x.2).count();
    let conv = tonly.values().filter(|x| x.2).count();
    let once_x = xlat.values().filter(|x| x.1 == 1).count();
    let uses_x: u64 = xlat.values().map(|x| x.1 as u64).sum();
    let uses_t: u64 = tonly.values().map(|x| x.1 as u64).sum();
    let mut shapes_keys: FxHashMap<u32, u64> = FxHashMap::default();
    for &(s, _) in gkeys.keys() {
        *shapes_keys.entry(s).or_default() += 1;
    }
    let mut sk: Vec<(u32, u64)> = shapes_keys.into_iter().collect();
    sk.sort_unstable();
    println!(
        "TOTAL f1-f{}: edges {} units {} lids {} ghost_lids_units {} | P {} P_in {} P_x {} | edges_in {} edges_xshape {} targets {} | D_shared {} D_tnew {} D_treused {} D_conv {} | X_new {} X_reused {} | bytes: index {} blk {} starts {} xfers {} src {} owner {} owner_varint {} | namers units {} regions {} | hdr_direct {} pending {} hdr_unified {} rev_unified {} hdr_global {} pending_global {} starts_varint {}",
        a.to, all.edges, all.units, all.lids, all.ghost_lids_units, all.p, all.p_in, all.p_x, all.edges_in, all.edges_x_shape, all.targets, all.d_shared, all.d_t_new, all.d_t_reused, all.d_converted, all.x_new, all.x_reused, all.index_bytes, all.blk_bytes, all.starts_bytes, all.xfer_bytes, all.src_bytes, all.owner_bytes, all.owner_varint, all.namers_units, all.namers_regions, all.hdr_direct, all.pending, all.hdr_unified, all.rev_unified, all.hdr_global, all.pending_global, all.starts_varint
    );
    println!(
        "PERSISTENT: own entries {entries_total}; target-only patterns {} ({} used in one frame only, {} later own, {} uses); translations {} ({} used once, {} uses); distinct keys per shape {:?} (global key ids, against {entries_total} entries)",
        tonly.len(),
        once_t,
        conv,
        uses_t,
        xlat.len(),
        once_x,
        uses_x,
        sk
    );
    Ok(())
}
