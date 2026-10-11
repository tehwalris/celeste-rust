//! `rewrite bundle-census` (plans/edge-bundles.md): a finished tree's
//! recorded edges counted as EDGE BUNDLES - the visited set's layout (per
//! (shape, region) a position-free entry and a cell mask) applied to edges.
//! A bundle is one SOURCE ENTRY's edges to one target at one transfer and one
//! cell OFFSET (target cell minus source cell), with a mask of the source
//! cells it holds at: nothing in it names a position but the mask. Counting,
//! encoding and timing only: nothing is written but the report.
//!
//! Bundles are formed per recorded UNIT (a run of frontier rows in id order:
//! a few neighbouring source regions, `storage::wave`), as a forward would
//! form them at a unit's end. Two namings of the target:
//!   * A, the target ENTRY (the unit's lid, a `(region, entry)`): a source
//!     mask whose shifted cells fall in two target regions is two bundles;
//!   * B, the target KEY (shape and position-free key, any region): the A
//!     bundles of one (source entry, key, transfer, offset) merged.
//! Variants: the transfer out of the key, (source entry, offset) as the key,
//! and whether a source entry's cells all have one out-set ("the whole entry
//! moves together"). The A bundles encoded per unit block, source-major (S:
//! by source entry, then lid) and target-major (T: by lid), as varints and
//! as bit-packed fixed width, each with zstd -1; against the built blocks
//! (`edges::encode_block`, re-encoded from the same edges); and the reverse
//! walk's index in each order.
//!
//! `--marked H`: only the edges between the remainder-free BFS's marks at
//! horizon H, the edges `arc_dp::load` keeps (its CSR, 12 B an edge), for the
//! in-RAM graph's size; `--w-share`, the backward's winning sets on that
//! graph, how many distinct ones an entry's cells hold (`w_share`). `--time-reps N`: the unit's END timed both ways on
//! each unit's real edges - the built counting sort and `encode_block`
//! against grouping into bundles and their encoding.

use anyhow::{ensure, Context, Result};
use rustc_hash::FxHashMap;
use std::path::Path;

use super::census::{index_bytes, varint_len};
use super::edges::{encode_block, frame_of_name, put_varint, EdgeStore, UnitView};
use super::marks::MarkRanks;
use super::unit::{pack_edge, unpack_edge, XFER_BITS};
use super::visited::Key;
use super::{id_entry, id_local, id_region, state_id, Geometry, StateId};
use crate::search::pos_graph::{GRID, NO_CELL};

pub struct BundleArgs<'a> {
    /// The level's tree (`frames/`, `edges/`).
    pub dir: &'a Path,
    /// Frames whose edges are counted.
    pub from: u32,
    pub to: u32,
    pub threads: usize,
    /// Only the edges between the BFS's marks at this horizon.
    pub marked: Option<u32>,
    /// Reps of the unit-end timing (0: not timed).
    pub time_reps: u32,
    /// With `marked`: the backward run on the loaded graph, its winning sets
    /// counted per entry (`w_share`).
    pub w_share: bool,
}

/// An offset as a code: `(dx + 512) << 10 | (dy + 512)` between two cells,
/// `TO_NONE` into `NO_CELL`, `NONE_NONE` from it into it, `FROM_NONE | cell`
/// out of it (the target cell itself: no position to be relative to).
const TO_NONE: u32 = 1 << 20;
const NONE_NONE: u32 = 1 << 20 | 1;
const FROM_NONE: u32 = 1 << 21;

fn offset(a: u32, b: u32) -> u32 {
    match (a == NO_CELL, b == NO_CELL) {
        (false, false) => {
            let g = GRID as u32;
            let dx = (b % g) as i32 - (a % g) as i32;
            let dy = (b / g) as i32 - (a / g) as i32;
            ((dx + 512) as u32) << 10 | (dy + 512) as u32
        }
        (false, true) => TO_NONE,
        (true, true) => NONE_NONE,
        (true, false) => FROM_NONE | b,
    }
}

/// An offset's `(dx, dy)`, where both cells exist.
fn delta(off: u32) -> Option<(i32, i32)> {
    (off < TO_NONE).then(|| ((off >> 10) as i32 - 512, (off & 1023) as i32 - 512))
}

/// An offset in a block: one byte `(dx + 8) << 4 | (dy + 8)` for |dx|, |dy|
/// <= 7 (never 0), else 0 and the code's varint.
fn put_offset(out: &mut Vec<u8>, off: u32) {
    match delta(off) {
        Some((dx, dy)) if dx.abs() <= 7 && dy.abs() <= 7 => out.push(((dx + 8) << 4 | (dy + 8)) as u8),
        _ => {
            out.push(0);
            put_varint(out, off as u64);
        }
    }
}

/// Bits to name one of `n` values.
fn bits_for(n: usize) -> u64 {
    if n <= 1 {
        0
    } else {
        64 - ((n - 1) as u64).leading_zeros() as u64
    }
}

fn zstd1(b: &[u8]) -> u64 {
    if b.is_empty() {
        return 0;
    }
    zstd::bulk::compress(b, 1).expect("zstd -1 of a block").len() as u64
}

/// `f` on each run of consecutive elements `same` holds between.
fn each_run<'a, T>(v: &'a [T], same: impl Fn(&T, &T) -> bool, mut f: impl FnMut(&'a [T])) {
    let mut i = 0;
    while i < v.len() {
        let mut j = i + 1;
        while j < v.len() && same(&v[i], &v[j]) {
            j += 1;
        }
        f(&v[i..j]);
        i = j;
    }
}

/// Per value its rank by use (most used first, then by value).
fn ranks(uses: impl Iterator<Item = (u32, u64)>) -> FxHashMap<u32, u32> {
    let mut n: FxHashMap<u32, u64> = FxHashMap::default();
    for (x, k) in uses {
        *n.entry(x).or_default() += k;
    }
    let mut v: Vec<(u64, u32)> = n.into_iter().map(|(x, k)| (k, x)).collect();
    v.sort_unstable_by(|p, q| q.0.cmp(&p.0).then(p.1.cmp(&q.1)));
    v.into_iter().enumerate().map(|(r, (_, x))| (x, r as u32)).collect()
}

/// Bundles by size (source cells = edges): `0[k]` bundles of `k` edges.
#[derive(Clone)]
struct Hist([u64; 65]);

impl Default for Hist {
    fn default() -> Self {
        Hist([0; 65])
    }
}

impl Hist {
    fn add(&mut self, o: &Hist) {
        for k in 0..65 {
            self.0[k] += o.0[k];
        }
    }
    fn bundles(&self) -> u64 {
        self.0.iter().sum()
    }
    fn edges(&self) -> u64 {
        self.0.iter().enumerate().map(|(k, n)| k as u64 * n).sum()
    }
    /// The smallest size `k` with `q` of the bundles (`by_edges`: of the
    /// edges) in bundles of at most `k`.
    fn quantile(&self, q: f64, by_edges: bool) -> usize {
        let w = |k: usize| if by_edges { k as u64 * self.0[k] } else { self.0[k] };
        let total: u64 = (0..65).map(w).sum();
        let mut acc = 0u64;
        for k in 0..65 {
            acc += w(k);
            if acc as f64 >= q * total as f64 {
                return k;
            }
        }
        64
    }
    fn line(&self) -> String {
        let (b, e) = (self.bundles(), self.edges());
        format!(
            "{b} bundles, {:.2} edges each (median {}, p90 {}; edge-weighted median {}, p90 {}); single-cell {:.1}% of bundles, {:.1}% of edges",
            e as f64 / b.max(1) as f64,
            self.quantile(0.5, false),
            self.quantile(0.9, false),
            self.quantile(0.5, true),
            self.quantile(0.9, true),
            pct(self.0[1], b),
            pct(self.0[1], e)
        )
    }
}

fn pct(a: u64, b: u64) -> f64 {
    100.0 * a as f64 / b.max(1) as f64
}

/// Everything counted, summed over units and frames (`add`).
#[derive(Default, Clone)]
struct Stats {
    units: u64,
    unsorted_units: u64,
    sources: u64,
    sources_no_edge: u64,
    src_entries: u64,
    lids: u64,
    edges: u64,
    /// Bundles A (target entry) and B (target key).
    a: Hist,
    b: Hist,
    /// B bundles by the target regions (A bundles) they merge: 1, 2, 3, 4, more.
    b_regions: [u64; 5],
    /// Groups (source entry, target, transfer) and those with one offset.
    shift_a: u64,
    shift_a_uni: u64,
    shift_a_uni_edges: u64,
    shift_b: u64,
    shift_b_uni: u64,
    shift_b_uni_edges: u64,
    /// Transfer-free bundles (source entry, target, offset), and those whose
    /// edges all have one transfer.
    a_nox: u64,
    a_nox_uni: u64,
    a_nox_uni_edges: u64,
    b_nox: u64,
    b_nox_uni: u64,
    b_nox_uni_edges: u64,
    /// Groups (source entry, offset): count, one transfer, the transfer's
    /// conditional entropy (bits summed over edges).
    so: u64,
    so_uni: u64,
    so_uni_edges: u64,
    so_h_bits: f64,
    /// (source entry, offset) with the cells split into classes of equal
    /// (lid, transfer) sets: classes and the items they list.
    so_classes: u64,
    so_class_items: u64,
    /// Source entries with edges at one cell / several; of the latter, those
    /// whose cells all have one (target key, transfer, offset) set; classes
    /// of equal sets and the items they list.
    ent_one: u64,
    ent_one_edges: u64,
    ent_multi: u64,
    ent_multi_edges: u64,
    ent_together: u64,
    ent_together_edges: u64,
    ent_classes: u64,
    ent_class_items: u64,
    /// The built layout: block, per-lid starts, transfer ranks, explicit
    /// sources, owner index (16 B a lid); the block re-encoded from the
    /// edges read (a check of the decode), and its zstd -1.
    cur_blk: u64,
    cur_starts: u64,
    cur_xfers: u64,
    cur_src: u64,
    cur_owner: u64,
    cur_reenc: u64,
    cur_zstd: u64,
    /// The A bundles encoded per unit: S order (relative masks; u64 masks),
    /// T order (relative masks), each with zstd -1; S bit-packed fixed width
    /// (u64 masks) and its zstd -1; the unit's transfer table (4 B a rank)
    /// and offsets; S's and T's bytes by field (group, other, transfer,
    /// offset, mask).
    s_rel: u64,
    s_rel_zstd: u64,
    s_64: u64,
    s_64_zstd: u64,
    t_rel: u64,
    t_rel_zstd: u64,
    fixed: u64,
    fixed_zstd: u64,
    xfer_ranks: u64,
    s_fields: [u64; 5],
    t_fields: [u64; 5],
    /// Reverse walk inside a unit: S order, per lid its bundles' ranks
    /// (varints); T order, per lid its bytes (varints).
    s_lid_lists: u64,
    t_lid_starts: u64,
}

impl Stats {
    fn add(&mut self, o: &Stats) {
        macro_rules! sum {
            ($($f:ident),*) => { $( self.$f += o.$f; )* };
        }
        sum!(
            units, unsorted_units, sources, sources_no_edge, src_entries, lids, edges, shift_a, shift_a_uni, shift_a_uni_edges, shift_b, shift_b_uni, shift_b_uni_edges, a_nox, a_nox_uni, a_nox_uni_edges, b_nox, b_nox_uni,
            b_nox_uni_edges, so, so_uni, so_uni_edges, so_h_bits, so_classes, so_class_items, ent_one, ent_one_edges, ent_multi, ent_multi_edges, ent_together, ent_together_edges, ent_classes, ent_class_items, cur_blk, cur_starts,
            cur_xfers, cur_src, cur_owner, cur_reenc, cur_zstd, s_rel, s_rel_zstd, s_64, s_64_zstd, t_rel, t_rel_zstd, fixed, fixed_zstd, xfer_ranks, s_lid_lists, t_lid_starts
        );
        self.a.add(&o.a);
        self.b.add(&o.b);
        for k in 0..5 {
            self.b_regions[k] += o.b_regions[k];
            self.s_fields[k] += o.s_fields[k];
            self.t_fields[k] += o.t_fields[k];
        }
    }
}

/// One edge of a unit: its source entry (the unit's rank of it) and cell,
/// its lane, target lid and cell, target key id, transfer, offset.
#[derive(Clone, Copy)]
struct Rec {
    se: u32,
    sc: u32,
    lane: u32,
    lid: u32,
    tc: u32,
    g: u32,
    x: u32,
    off: u32,
}

/// An A bundle: (source entry, lid, transfer, offset) and its source cells
/// (`mask` by local cell; `rel` by the cell's place among the entry's cells
/// in the unit).
#[derive(Clone, Copy)]
struct Bundle {
    se: u32,
    lid: u32,
    g: u32,
    x: u32,
    off: u32,
    mask: u64,
    rel: u64,
    n: u32,
}

/// Bundles as a block: per bundle the group field (S: source entry, T: lid)
/// as a delta (0: the same group), the other as a delta inside the group or
/// itself on a new one, the transfer's rank, the offset, the mask (`rel`: the
/// relative mask in `ceil(k / 8)` bytes, `k` the entry's cells in the unit,
/// none when 1; else the u64). Returns the block and each group's first byte.
fn encode_bundles(bs: &[Bundle], target_major: bool, xr: &FxHashMap<u32, u32>, k_of: &[u32], rel: bool) -> (Vec<u8>, Vec<usize>, [u64; 5]) {
    let mut out = Vec::with_capacity(bs.len() * 4);
    let mut starts = Vec::new();
    // Bytes per field: group, the other field, transfer, offset, mask.
    let mut fields = [0u64; 5];
    let mut at = 0usize;
    let mut field = |out: &Vec<u8>, k: usize| {
        fields[k] += (out.len() - at) as u64;
        at = out.len();
    };
    let (mut p1, mut p2) = (u32::MAX, 0u32);
    for b in bs {
        let (f1, f2) = if target_major { (b.lid, b.se) } else { (b.se, b.lid) };
        if f1 != p1 {
            starts.push(out.len());
            put_varint(&mut out, if p1 == u32::MAX { f1 as u64 + 1 } else { (f1 - p1) as u64 });
            field(&out, 0);
            put_varint(&mut out, f2 as u64);
        } else {
            put_varint(&mut out, 0);
            field(&out, 0);
            put_varint(&mut out, (f2 - p2) as u64);
        }
        field(&out, 1);
        (p1, p2) = (f1, f2);
        put_varint(&mut out, xr[&b.x] as u64);
        field(&out, 2);
        put_offset(&mut out, b.off);
        field(&out, 3);
        if rel {
            let k = k_of[b.se as usize] as usize;
            if k > 1 {
                out.extend_from_slice(&b.rel.to_le_bytes()[..k.div_ceil(8)]);
            }
        } else {
            out.extend_from_slice(&b.mask.to_le_bytes());
        }
        field(&out, 4);
    }
    (out, starts, fields)
}

/// S-order bundles bit-packed at the unit's widths (source entry, lid,
/// transfer rank, offset by the unit's dictionary of them) plus a u64 mask;
/// the dictionaries after (4 B a transfer, 4 B an offset).
fn encode_fixed(bs: &[Bundle], n_se: usize, n_lids: usize, xr: &FxHashMap<u32, u32>) -> Vec<u8> {
    let offs: FxHashMap<u32, u32> = ranks(bs.iter().map(|b| (b.off, 1)));
    let w = [bits_for(n_se), bits_for(n_lids), bits_for(xr.len()), bits_for(offs.len())];
    let mut out = Vec::with_capacity(bs.len() * 12 + 4 * (xr.len() + offs.len()) + 8);
    let (mut acc, mut nacc) = (0u128, 0u64);
    let mut put = |out: &mut Vec<u8>, v: u64, bits: u64| {
        if bits == 0 {
            return;
        }
        acc |= (v as u128) << nacc;
        nacc += bits;
        while nacc >= 8 {
            out.push(acc as u8);
            acc >>= 8;
            nacc -= 8;
        }
    };
    for b in bs {
        put(&mut out, b.se as u64, w[0]);
        put(&mut out, b.lid as u64, w[1]);
        put(&mut out, xr[&b.x] as u64, w[2]);
        put(&mut out, offs[&b.off] as u64, w[3]);
        put(&mut out, b.mask & 0xffff_ffff, 32);
        put(&mut out, b.mask >> 32, 32);
    }
    put(&mut out, 0, 7);
    let mut by_rank: Vec<(u32, u32)> = xr.iter().map(|(&x, &r)| (r, x)).collect();
    by_rank.sort_unstable();
    for (_, x) in by_rank {
        out.extend_from_slice(&x.to_le_bytes());
    }
    let mut by_rank: Vec<(u32, u32)> = offs.iter().map(|(&o, &r)| (r, o)).collect();
    by_rank.sort_unstable();
    for (_, o) in by_rank {
        out.extend_from_slice(&o.to_le_bytes());
    }
    out
}

/// What a unit's census returns: its counts, its owner index entries
/// `(region, entry, unit, lid)`, its first and last source.
struct UnitResult {
    st: Stats,
    owners: Vec<(u32, u32, u32, u32)>,
    first: Option<StateId>,
    last: Option<StateId>,
}

/// What every unit's census reads.
struct Ctx<'a> {
    geo: Geometry,
    /// Per region its entries' target key ids.
    ent_g: &'a FxHashMap<u32, Vec<u32>>,
    marks: Option<&'a MarkRanks>,
}

/// A unit's sources: per lane its source entry (the unit's rank, in lane
/// order) and the cell's place among the entry's cells in the unit; per
/// source entry its cells there.
struct Sources {
    ids: Vec<StateId>,
    se: Vec<u32>,
    rel: Vec<u8>,
    k_of: Vec<u32>,
}

fn sources(u: &UnitView) -> Sources {
    let ids: Vec<StateId> = (0..u.n_sources() as u32).map(|s| u.source(s)).collect();
    let mut ents: FxHashMap<(u32, u32), u32> = FxHashMap::default();
    let mut cells: Vec<Vec<u32>> = Vec::new();
    let mut se = Vec::with_capacity(ids.len());
    for &id in &ids {
        let next = ents.len() as u32;
        let e = *ents.entry((id_region(id), id_entry(id))).or_insert(next);
        if e == next {
            cells.push(Vec::new());
        }
        cells[e as usize].push(id_local(id));
        se.push(e);
    }
    for c in &mut cells {
        c.sort_unstable();
    }
    let rel = ids.iter().zip(&se).map(|(&id, &e)| cells[e as usize].binary_search(&id_local(id)).expect("a source's own cell") as u8).collect();
    Sources { ids, se, rel, k_of: cells.iter().map(|c| c.len() as u32).collect() }
}

/// The edges of a unit (between marks, under `--marked`).
fn unit_edges(u: &UnitView, src: &Sources, cx: &Ctx) -> Vec<Rec> {
    let geo = cx.geo;
    let owners: Vec<Option<(u32, u32)>> = (0..u.n_lids() as u32).map(|l| u.lid_owner(l)).collect();
    let mut r = Vec::new();
    u.edges(|lid, c, s, x| {
        let (rt, et) = owners[lid as usize].expect("an edge names an owned lid");
        let id = src.ids[s as usize];
        if let Some(m) = cx.marks {
            if m.rank(id).is_none() || m.rank(state_id(rt, et, c)).is_none() {
                return;
            }
        }
        let (rs, cs) = (id_region(id), id_local(id));
        let g = *cx.ent_g.get(&rt).and_then(|v| v.get(et as usize)).unwrap_or_else(|| panic!("no key for target entry r{rt}.e{et}"));
        let off = offset(geo.cell(rs % geo.slots, cs), geo.cell(rt % geo.slots, c));
        r.push(Rec { se: src.se[s as usize], sc: cs, lane: s, lid, tc: c, g, x, off });
    });
    r
}

/// One unit's census (module doc).
fn unit_census(u: &UnitView, ui: u32, cx: &Ctx) -> UnitResult {
    let src = sources(u);
    let n_lids = u.n_lids();
    let mut st = Stats { units: 1, lids: n_lids as u64, sources: src.ids.len() as u64, src_entries: src.k_of.len() as u64, ..Default::default() };
    if !src.ids.windows(2).all(|w| w[0] < w[1]) {
        st.unsorted_units = 1;
    }
    let mut r = unit_edges(u, &src, cx);
    st.edges = r.len() as u64;
    let mut has = vec![false; src.ids.len()];
    for e in &r {
        has[e.lane as usize] = true;
    }
    st.sources_no_edge = has.iter().filter(|&&h| !h).count() as u64;

    // The built layout, as recorded and re-encoded from these edges.
    let (blk, n_x, explicit) = u.layout();
    let owned: Vec<(u32, u32, u32, u32)> = (0..n_lids as u32).filter_map(|l| u.lid_owner(l).map(|(q, e)| (q, e, ui, l))).collect();
    st.cur_blk = blk;
    st.cur_starts = 4 * (n_lids as u64 + 1);
    st.cur_xfers = 4 * n_x as u64;
    st.cur_src = if explicit { 8 * src.ids.len() as u64 } else { 0 };
    st.cur_owner = 16 * owned.len() as u64;
    {
        let xr = ranks(r.iter().map(|e| (e.x, 1)));
        let mut packed: Vec<u64> = r.iter().map(|e| pack_edge(e.lid, e.tc, e.lane, xr[&e.x])).collect();
        packed.sort_unstable();
        packed.dedup();
        let (b, _) = encode_block(&packed, n_lids);
        st.cur_reenc = b.len() as u64;
        st.cur_zstd = zstd1(&b);
    }

    // A bundles, S order: (source entry, lid, transfer, offset) -> cells.
    r.sort_unstable_by_key(|e| (e.se, e.lid, e.x, e.off, e.sc));
    let mut a: Vec<Bundle> = Vec::new();
    each_run(&r, |p, q| (p.se, p.lid, p.x, p.off) == (q.se, q.lid, q.x, q.off), |run| {
        let (mut mask, mut rel) = (0u64, 0u64);
        for e in run {
            assert!(mask >> e.sc & 1 == 0, "two edges of one bundle at one source cell");
            mask |= 1 << e.sc;
            rel |= 1 << src.rel[e.lane as usize];
        }
        let e = run[0];
        a.push(Bundle { se: e.se, lid: e.lid, g: e.g, x: e.x, off: e.off, mask, rel, n: run.len() as u32 });
    });
    for b in &a {
        st.a.0[b.n as usize] += 1;
    }
    each_run(&a, |p, q| (p.se, p.lid, p.x) == (q.se, q.lid, q.x), |run| {
        st.shift_a += 1;
        if run.len() == 1 {
            st.shift_a_uni += 1;
            st.shift_a_uni_edges += run[0].n as u64;
        }
    });

    // Encodings of the A bundles.
    let xr = ranks(a.iter().map(|b| (b.x, 1)));
    st.xfer_ranks = 4 * xr.len() as u64;
    let (s_rel, _, s_fields) = encode_bundles(&a, false, &xr, &src.k_of, true);
    st.s_fields = s_fields;
    st.s_rel = s_rel.len() as u64;
    st.s_rel_zstd = zstd1(&s_rel);
    let (s_64, ..) = encode_bundles(&a, false, &xr, &src.k_of, false);
    st.s_64 = s_64.len() as u64;
    st.s_64_zstd = zstd1(&s_64);
    let fixed = encode_fixed(&a, src.k_of.len(), n_lids, &xr);
    st.fixed = fixed.len() as u64;
    st.fixed_zstd = zstd1(&fixed);
    // S order's reverse walk: per lid its bundles' ranks.
    let mut by_lid: Vec<(u32, u32)> = a.iter().enumerate().map(|(i, b)| (b.lid, i as u32)).collect();
    by_lid.sort_unstable();
    each_run(&by_lid, |p, q| p.0 == q.0, |run| {
        st.s_lid_lists += varint_len(run.len() as u64);
        let mut prev = 0u32;
        for &(_, i) in run {
            st.s_lid_lists += varint_len((i - prev) as u64);
            prev = i;
        }
    });
    let mut t = a.clone();
    t.sort_unstable_by_key(|b| (b.lid, b.se, b.x, b.off));
    let (t_rel, t_starts, t_fields) = encode_bundles(&t, true, &xr, &src.k_of, true);
    st.t_fields = t_fields;
    st.t_rel = t_rel.len() as u64;
    st.t_rel_zstd = zstd1(&t_rel);
    st.t_lid_starts = t_starts.iter().zip(t_starts.iter().skip(1).chain(std::iter::once(&t_rel.len()))).map(|(&p, &q)| varint_len((q - p) as u64)).sum();
    drop(t);

    // Without the transfer: (source entry, lid, offset).
    let mut v = a.clone();
    v.sort_unstable_by_key(|b| (b.se, b.lid, b.off, b.x));
    each_run(&v, |p, q| (p.se, p.lid, p.off) == (q.se, q.lid, q.off), |run| {
        st.a_nox += 1;
        if run.len() == 1 {
            st.a_nox_uni += 1;
            st.a_nox_uni_edges += run[0].n as u64;
        }
    });
    // (source entry, offset): its transfers.
    v.sort_unstable_by_key(|b| (b.se, b.off, b.x, b.lid));
    each_run(&v, |p, q| (p.se, p.off) == (q.se, q.off), |run| {
        st.so += 1;
        let tot: u64 = run.iter().map(|b| b.n as u64).sum();
        let mut nx = 0;
        each_run(run, |p, q| p.x == q.x, |xs| {
            nx += 1;
            let k: u64 = xs.iter().map(|b| b.n as u64).sum();
            st.so_h_bits += k as f64 * (tot as f64 / k as f64).log2();
        });
        if nx == 1 {
            st.so_uni += 1;
            st.so_uni_edges += tot;
        }
    });
    // B: (source entry, key, transfer, offset), A bundles merged.
    v.sort_unstable_by_key(|b| (b.se, b.g, b.x, b.off, b.lid));
    let mut bb: Vec<(u32, u32, u32, u32, u32)> = Vec::new();
    each_run(&v, |p, q| (p.se, p.g, p.x, p.off) == (q.se, q.g, q.x, q.off), |run| {
        let n: u32 = run.iter().map(|b| b.n).sum();
        assert!(n <= 64 && run.iter().fold(0u64, |m, b| m | b.mask).count_ones() == n, "a B bundle's A bundles overlap");
        st.b.0[n as usize] += 1;
        st.b_regions[(run.len() - 1).min(4)] += 1;
        bb.push((run[0].se, run[0].g, run[0].x, run[0].off, n));
    });
    each_run(&bb, |p, q| (p.0, p.1, p.2) == (q.0, q.1, q.2), |run| {
        st.shift_b += 1;
        if run.len() == 1 {
            st.shift_b_uni += 1;
            st.shift_b_uni_edges += run[0].4 as u64;
        }
    });
    bb.sort_unstable_by_key(|b| (b.0, b.1, b.3, b.2));
    each_run(&bb, |p, q| (p.0, p.1, p.3) == (q.0, q.1, q.3), |run| {
        st.b_nox += 1;
        if run.len() == 1 {
            st.b_nox_uni += 1;
            st.b_nox_uni_edges += run[0].4 as u64;
        }
    });

    // (source entry, offset) with its cells in classes of equal (lid,
    // transfer) sets.
    r.sort_unstable_by_key(|e| (e.se, e.off, e.sc, e.lid, e.x));
    let same_lx = |p: &[Rec], q: &[Rec]| p.iter().map(|e| (e.lid, e.x)).eq(q.iter().map(|e| (e.lid, e.x)));
    each_run(&r, |p, q| (p.se, p.off) == (q.se, q.off), |grp| {
        let mut cells: Vec<&[Rec]> = Vec::new();
        each_run(grp, |p, q| p.sc == q.sc, |c| cells.push(c));
        cells.sort_by(|p, q| p.iter().map(|e| (e.lid, e.x)).cmp(q.iter().map(|e| (e.lid, e.x))));
        cells.dedup_by(|p, q| same_lx(p, q));
        st.so_classes += cells.len() as u64;
        st.so_class_items += cells.iter().map(|c| c.len() as u64).sum::<u64>();
    });
    // Does the whole entry move together: one (key, transfer, offset) set
    // at every cell with edges?
    r.sort_unstable_by_key(|e| (e.se, e.sc, e.g, e.x, e.off));
    let key3 = |e: &Rec| (e.g, e.x, e.off);
    each_run(&r, |p, q| p.se == q.se, |ent| {
        let mut cells: Vec<&[Rec]> = Vec::new();
        each_run(ent, |p, q| p.sc == q.sc, |c| cells.push(c));
        if cells.len() < 2 {
            st.ent_one += 1;
            st.ent_one_edges += ent.len() as u64;
            return;
        }
        st.ent_multi += 1;
        st.ent_multi_edges += ent.len() as u64;
        cells.sort_by(|p, q| p.iter().map(key3).cmp(q.iter().map(key3)));
        cells.dedup_by(|p, q| p.iter().map(key3).eq(q.iter().map(key3)));
        st.ent_classes += cells.len() as u64;
        st.ent_class_items += cells.iter().map(|c| c.len() as u64).sum::<u64>();
        if cells.len() == 1 {
            st.ent_together += 1;
            st.ent_together_edges += ent.len() as u64;
        }
    });
    UnitResult { st, owners: owned, first: src.ids.first().copied(), last: src.ids.last().copied() }
}

/// Per region its entries' TARGET KEY ids: `(shape index, key)` interned over
/// the tree's metadata through frame `last`.
fn key_ids(dir: &Path, last: u32, geo: &Geometry) -> Result<(FxHashMap<u32, Vec<u32>>, u64)> {
    let mut gkeys: FxHashMap<(u32, Key), u32> = FxHashMap::default();
    let mut ent_g: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
    let mut entries = 0u64;
    for f in 0..=last {
        if !dir.join("frames").join(format!("f{f:03}")).is_dir() {
            continue;
        }
        let mut ents: Vec<(u32, u32, Key)> = super::meta::load_frame(dir, f)?.into_iter().flat_map(|m| m.entries).collect();
        ents.sort_unstable();
        for (r, e, k) in ents {
            let n = gkeys.len() as u32;
            let g = *gkeys.entry((r / geo.slots, k)).or_insert(n);
            let v = ent_g.entry(r).or_default();
            ensure!(e as usize == v.len(), "{}: region {r}'s entry {e} after {} (a raised tree?)", dir.display(), v.len());
            v.push(g);
            entries += 1;
        }
    }
    Ok((ent_g, entries))
}

/// The remainder-free BFS's marks at `horizon` (as `arc_dp::load`: the win
/// rows' states and the start), ranked; their entries.
fn bfs_marks(dir: &Path, eg: &EdgeStore, horizon: u32, threads: usize) -> Result<(MarkRanks, u64, u64)> {
    let mut wins: Vec<(StateId, u32)> = Vec::new();
    let mut start = None;
    for f in 0..=horizon {
        if !dir.join("frames").join(format!("f{f:03}")).is_dir() {
            continue;
        }
        for (_, file) in crate::frame::frame_files(dir, f)? {
            if f == 0 {
                start = Some(file.id_at(0));
            } else {
                wins.extend(file.win_rows().iter().map(|&(row, _)| (file.id_at(row), f)));
            }
        }
    }
    let start = start.with_context(|| format!("{}: no frame-0 row", dir.display()))?;
    ensure!(!wins.is_empty(), "{}: no win rows through f{horizon}", dir.display());
    let (mut m, _) = crate::search::edges::bfs(eg, horizon, wins, threads);
    m.insert(start, 0, 0);
    let (ranks, members) = m.into_ranked();
    let mut entries = 0u64;
    let mut last = None;
    for &(id, ..) in &members {
        let k = (id_region(id), id_entry(id));
        if last != Some(k) {
            entries += 1;
            last = Some(k);
        }
    }
    Ok((ranks, members.len() as u64, entries))
}

/// Bytes on disk of the files under `dir` whose name `keep` admits.
fn bytes_of(dir: &Path, keep: impl Fn(&str) -> bool) -> u64 {
    std::fs::read_dir(dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter(|e| e.file_name().to_str().is_some_and(&keep))
        .filter_map(|e| e.metadata().ok())
        .filter(|m| m.is_file())
        .map(|m| m.len())
        .sum()
}

/// The unit-end timing's sums (ns), per input order (by source lane;
/// shuffled): built, bundles in S and in T order; and what each produced.
#[derive(Default, Clone, Copy)]
struct Timing {
    edges: u64,
    built_ns: [u64; 2],
    s_ns: [u64; 2],
    t_ns: [u64; 2],
    built_bytes: u64,
    bundles: u64,
    s_bytes: u64,
    t_bytes: u64,
}

impl Timing {
    fn add(&mut self, o: &Timing) {
        self.edges += o.edges;
        for k in 0..2 {
            self.built_ns[k] += o.built_ns[k];
            self.s_ns[k] += o.s_ns[k];
            self.t_ns[k] += o.t_ns[k];
        }
        self.built_bytes += o.built_bytes;
        self.bundles += o.bundles;
        self.s_bytes += o.s_bytes;
        self.t_bytes += o.t_bytes;
    }
    fn line(&self) -> String {
        let ns = |x: u64| x as f64 / self.edges.max(1) as f64;
        let b = |x: u64| x as f64 / self.edges.max(1) as f64;
        format!(
            "{} edges | built {:.1} / {:.1} ns an edge (by lane / shuffled), {:.3} B an edge | bundles S {:.1} / {:.1}, {:.3} B; T {:.1} / {:.1}, {:.3} B; {} bundles",
            self.edges,
            ns(self.built_ns[0]),
            ns(self.built_ns[1]),
            b(self.built_bytes),
            ns(self.s_ns[0]),
            ns(self.s_ns[1]),
            b(self.s_bytes),
            ns(self.t_ns[0]),
            ns(self.t_ns[1]),
            b(self.t_bytes),
            self.bundles
        )
    }
}

/// A worker's scratch for the timing, kept across units (as a sink keeps its own).
#[derive(Default)]
struct Scratch {
    sort_at: Vec<u32>,
    cur: Vec<u32>,
    sorted: Vec<u64>,
    xfer_use: Vec<u32>,
    used: Vec<u32>,
    se_at: Vec<u32>,
    keys: Vec<u64>,
}

/// Transfer ranks by use into `s.xfer_use` (`used` the transfers seen);
/// `x` of each edge by `xfer`.
fn rank_xfers(edges: &[u64], s: &mut Scratch) {
    s.used.clear();
    for &e in edges {
        let x = unpack_edge(e).3 as usize;
        if s.xfer_use[x] == 0 {
            s.used.push(x as u32);
        }
        s.xfer_use[x] += 1;
    }
    let xu = &s.xfer_use;
    s.used.sort_unstable_by_key(|&x| (std::cmp::Reverse(xu[x as usize]), x));
    for (r, &x) in s.used.iter().enumerate() {
        s.xfer_use[x as usize] = r as u32;
    }
}

/// THE BUILT UNIT END (`UnitSink::end`, the same steps): a counting sort by
/// (lid, cell) with the transfer's rank in place, each target's few edges
/// sorted, duplicates dropped, `encode_block`.
fn built_end(edges: &[u64], n_lids: usize, cells: usize, s: &mut Scratch) -> usize {
    s.sort_at.clear();
    s.sort_at.resize(n_lids * cells + 1, 0);
    for &e in edges {
        let (lid, local, ..) = unpack_edge(e);
        s.sort_at[lid as usize * cells + local as usize + 1] += 1;
    }
    rank_xfers(edges, s);
    for k in 0..n_lids * cells {
        s.sort_at[k + 1] += s.sort_at[k];
    }
    s.sorted.clear();
    s.sorted.resize(edges.len(), 0);
    let xmask = (1u64 << XFER_BITS) - 1;
    s.cur.clear();
    s.cur.extend_from_slice(&s.sort_at);
    for &e in edges {
        let (lid, local, _, x) = unpack_edge(e);
        let k = lid as usize * cells + local as usize;
        s.sorted[s.cur[k] as usize] = e & !xmask | s.xfer_use[x as usize] as u64;
        s.cur[k] += 1;
    }
    for &x in &s.used {
        s.xfer_use[x as usize] = 0;
    }
    for k in 0..n_lids * cells {
        let (a, b) = (s.sort_at[k] as usize, s.sort_at[k + 1] as usize);
        if b - a > 1 {
            s.sorted[a..b].sort_unstable();
        }
    }
    s.sorted.dedup();
    encode_block(&s.sorted, n_lids).0.len()
}

/// THE BUNDLED UNIT END: transfer ranks by use; a counting sort by the
/// group field (S: the source entry, per lane known at the unit's start
/// with its cell's relative bit; T: the lid); per group its edges as keys
/// `other field | rank | local dx, dy | relative cell` sorted and grouped
/// into masks (the source and target regions are fixed by entry and lid, so
/// the local deltas ARE the offset); each bundle encoded as
/// `encode_bundles` does (relative masks). Returns (bundles, bytes).
#[allow(clippy::too_many_arguments)]
fn bundle_end(edges: &[u64], target_major: bool, src: &Sources, lane_local: &[u8], lid_slot: &[u32], src_slot: &[u32], geo: &Geometry, s: &mut Scratch, out: &mut Vec<u8>) -> (u64, usize) {
    let (side, none) = (geo.side as i32, geo.no_cell_slot());
    let n_groups = if target_major { lid_slot.len() } else { src.k_of.len() };
    let group = |lid: u32, lane: u32| if target_major { lid } else { src.se[lane as usize] };
    rank_xfers(edges, s);
    s.se_at.clear();
    s.se_at.resize(n_groups + 1, 0);
    for &e in edges {
        let (lid, _, lane, _) = unpack_edge(e);
        s.se_at[group(lid, lane) as usize + 1] += 1;
    }
    for k in 0..n_groups {
        s.se_at[k + 1] += s.se_at[k];
    }
    s.keys.clear();
    s.keys.resize(edges.len(), 0);
    s.cur.clear();
    s.cur.extend_from_slice(&s.se_at);
    for &e in edges {
        let (lid, local, lane, x) = unpack_edge(e);
        // Into `NO_CELL` the offset is one code whatever the source cell.
        let sl = lane_local[lane as usize] as i32;
        let (dx, dy) = if lid_slot[lid as usize] == none { (0, 0) } else { (local as i32 % side - sl % side, local as i32 / side - sl / side) };
        let other = if target_major { src.se[lane as usize] } else { lid };
        let key = (other as u64) << 43 | (s.xfer_use[x as usize] as u64) << 20 | ((dx + 16) as u64) << 11 | ((dy + 16) as u64) << 6 | src.rel[lane as usize] as u64;
        let g = group(lid, lane) as usize;
        s.keys[s.cur[g] as usize] = key;
        s.cur[g] += 1;
    }
    for &x in &s.used {
        s.xfer_use[x as usize] = 0;
    }
    out.clear();
    let mut bundles = 0u64;
    let mut prev_g = u32::MAX;
    for g in 0..n_groups {
        let (a, b) = (s.se_at[g] as usize, s.se_at[g + 1] as usize);
        let seg = &mut s.keys[a..b];
        seg.sort_unstable();
        let mut i = 0;
        let mut prev_o = 0u64;
        while i < seg.len() {
            let head = seg[i] >> 6;
            let mut rel = 0u64;
            while i < seg.len() && seg[i] >> 6 == head {
                rel |= 1 << (seg[i] & 63);
                i += 1;
            }
            let o = head >> 37;
            if g as u32 != prev_g {
                put_varint(out, if prev_g == u32::MAX { g as u64 + 1 } else { (g as u32 - prev_g) as u64 });
                put_varint(out, o);
                prev_g = g as u32;
            } else {
                put_varint(out, 0);
                put_varint(out, o - prev_o);
            }
            prev_o = o;
            put_varint(out, head >> 14 & ((1 << 23) - 1));
            // The offset: the regions' delta (entry and lid) plus the local delta.
            let (lid, se) = if target_major { (g, o as usize) } else { (o as usize, g) };
            let (ldx, ldy) = ((head >> 5 & 31) as i32 - 16, (head & 31) as i32 - 16);
            let (ts, ss) = (lid_slot[lid], src_slot[se]);
            let cell = |slot: u32| geo.cell(slot, 0);
            put_offset(out, match (cell(ss), cell(ts)) {
                (a, b) if a != NO_CELL && b != NO_CELL => (offset(a, b) as i32 + ldx * 1024 + ldy) as u32,
                (a, b) => offset(a, b),
            });
            let k = src.k_of[se] as usize;
            if k > 1 {
                out.extend_from_slice(&rel.to_le_bytes()[..k.div_ceil(8)]);
            }
            bundles += 1;
        }
    }
    (bundles, out.len())
}

/// A unit's end timed both ways (`Timing`), `reps` times each (the minimum
/// per unit summed), its edges in two orders: by source lane (as the kernels
/// emit them, a lane's emissions together) and shuffled.
fn time_unit(u: &UnitView, cx: &Ctx, reps: u32, s: &mut Scratch) -> Timing {
    let geo = cx.geo;
    let src = sources(u);
    let r = unit_edges(u, &src, cx);
    let n_lids = u.n_lids();
    let lid_slot: Vec<u32> = (0..n_lids as u32).map(|l| u.lid_owner(l).map_or(0, |(q, _)| q % geo.slots)).collect();
    let mut src_slot = vec![0u32; src.k_of.len()];
    for (lane, &id) in src.ids.iter().enumerate() {
        src_slot[src.se[lane] as usize] = id_region(id) % geo.slots;
    }
    let lane_local: Vec<u8> = src.ids.iter().map(|&id| id_local(id) as u8).collect();
    let max_x = r.iter().map(|e| e.x).max().unwrap_or(0) as usize;
    if s.xfer_use.len() <= max_x {
        s.xfer_use.resize(max_x + 1, 0);
    }
    let mut by_lane: Vec<u64> = r.iter().map(|e| pack_edge(e.lid, e.tc, e.lane, e.x)).collect();
    by_lane.sort_unstable_by_key(|&e| (unpack_edge(e).2, e));
    let mut shuffled = by_lane.clone();
    let mut v = 0x9e37_79b9_7f4a_7c15u64 ^ shuffled.len() as u64;
    for i in (1..shuffled.len()).rev() {
        v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        shuffled.swap(i, (v >> 33) as usize % (i + 1));
    }
    let cells = (geo.side * geo.side) as usize;
    let mut t = Timing { edges: r.len() as u64, ..Default::default() };
    let mut out = Vec::new();
    let (mut s_bundles, mut t_bundles) = (0, 0);
    for (o, edges) in [&by_lane, &shuffled].into_iter().enumerate() {
        let mut best = [u64::MAX; 3];
        for rep in 0..reps.max(1) {
            // Rotate which runs first.
            for k in 0..3 {
                let t0 = std::time::Instant::now();
                match (k + rep as usize) % 3 {
                    0 => {
                        t.built_bytes = built_end(edges, n_lids, cells, s) as u64;
                        best[0] = best[0].min(t0.elapsed().as_nanos() as u64);
                    }
                    w => {
                        let (b, n) = bundle_end(edges, w == 2, &src, &lane_local, &lid_slot, &src_slot, &geo, s, &mut out);
                        best[w] = best[w].min(t0.elapsed().as_nanos() as u64);
                        if w == 1 {
                            (s_bundles, t.s_bytes) = (b, n as u64);
                        } else {
                            (t_bundles, t.t_bytes) = (b, n as u64);
                        }
                    }
                }
            }
        }
        (t.built_ns[o], t.s_ns[o], t.t_ns[o]) = (best[0], best[1], best[2]);
    }
    assert_eq!(s_bundles, t_bundles, "S and T group the same bundles");
    t.bundles = s_bundles;
    t
}

fn mb(b: u64) -> f64 {
    b as f64 / 1e6
}

pub fn bundle_census(a: &BundleArgs) -> Result<()> {
    let dir = a.dir;
    let geo = super::GEOMETRY;
    let horizon = a.marked.unwrap_or(a.to).max(a.to);
    let eg = EdgeStore::open(&dir.join("edges"), horizon)?;
    let (ent_g, n_entries) = key_ids(dir, horizon, &geo)?;
    let marks = match a.marked {
        Some(h) => {
            let (m, nodes, entries) = bfs_marks(dir, &eg, h, a.threads)?;
            println!("marked h{h}: {nodes} nodes in {entries} entries ({:.2} cells an entry)", nodes as f64 / entries.max(1) as f64);
            Some((m, nodes, entries))
        }
        None => None,
    };
    let cx = Ctx { geo, ent_g: &ent_g, marks: marks.as_ref().map(|m| &m.0) };
    println!("{}: frames {}-{}, {n_entries} entries through f{horizon}{}", dir.display(), a.from, a.to, if a.marked.is_some() { ", edges between marks only" } else { "" });
    println!("#frame edges states_prev | A bundles e/b single% | B bundles e/b | so_uniq% H(x|se,off) | together% | B/edge: built(disk) built_blk S_rel S_rel_zstd T_rel T_rel_zstd fixed | units split_entries");
    let (mut all, mut tim) = (Stats::default(), Timing::default());
    let (mut disk_edges, mut disk_frames, mut states, mut owner_varint) = (0u64, 0u64, 0u64, 0u64);
    let mut split_total = 0u64;
    for f in a.from.max(1)..=a.to {
        if !eg.has_frame(f) {
            continue;
        }
        let owners = eg.owners(f, a.threads);
        let units: Vec<(u32, (u32, u32))> = eg.units(f).into_iter().enumerate().map(|(i, u)| (i as u32, u)).collect();
        let res: Vec<UnitResult> = super::wave::par_map(&units, a.threads, |&(i, (fi, ui))| unit_census(&eg.unit(f, &owners, fi, ui), i, &cx));
        let mut st = Stats::default();
        let mut idx: Vec<(u32, u32, u32, u32)> = Vec::new();
        let mut spans: Vec<(StateId, StateId)> = Vec::new();
        for r in res {
            st.add(&r.st);
            idx.extend(r.owners);
            if let (Some(p), Some(q)) = (r.first, r.last) {
                spans.push((p, q));
            }
        }
        idx.sort_unstable();
        owner_varint += index_bytes(&idx);
        // Source entries cut by a unit boundary.
        spans.sort_unstable();
        let split = spans.windows(2).filter(|w| (id_region(w[0].1), id_entry(w[0].1)) == (id_region(w[1].0), id_entry(w[1].0))).count() as u64;
        split_total += split;
        if a.time_reps > 0 {
            let next = std::sync::atomic::AtomicUsize::new(0);
            let parts: Vec<Timing> = std::thread::scope(|sc| {
                let hs: Vec<_> = (0..a.threads.max(1))
                    .map(|_| {
                        let (next, units, owners, cx, eg) = (&next, &units, &owners, &cx, &eg);
                        sc.spawn(move || {
                            let mut s = Scratch::default();
                            let mut t = Timing::default();
                            loop {
                                let k = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                let Some(&(_, (fi, ui))) = units.get(k) else { return t };
                                t.add(&time_unit(&eg.unit(f, owners, fi, ui), cx, a.time_reps, &mut s));
                            }
                        })
                    })
                    .collect();
                hs.into_iter().map(|h| h.join().expect("a timing worker panicked")).collect()
            });
            let mut ft = Timing::default();
            for p in parts {
                ft.add(&p);
            }
            ensure!(ft.bundles == st.a.bundles(), "f{f}: the timed unit ends made {} bundles, the census {}", ft.bundles, st.a.bundles());
            println!("  time f{f:03}: {}", ft.line());
            tim.add(&ft);
        }
        let de = bytes_of(&dir.join("edges"), |n| frame_of_name(n) == Some(f) && (n.ends_with(".bin") || n.ends_with(".blk")));
        disk_edges += de;
        let prev_states: u64 = st.sources;
        let fdir = dir.join("frames").join(format!("f{f:03}"));
        disk_frames += bytes_of(&fdir, |_| true);
        for (_, file) in crate::frame::frame_files(dir, f)? {
            states += file.width() as u64;
        }
        let e = st.edges.max(1) as f64;
        println!(
            "f{f:03} {} {prev_states} | A {} {:.2} {:.1}% | B {} {:.2} | {:.1}% {:.3} | {:.1}% | {:.2} {:.2} {:.2} {:.2} {:.2} {:.2} {:.2} | {} {split}",
            st.edges,
            st.a.bundles(),
            st.edges as f64 / st.a.bundles().max(1) as f64,
            pct(st.a.0[1], st.a.bundles()),
            st.b.bundles(),
            st.edges as f64 / st.b.bundles().max(1) as f64,
            pct(st.so_uni_edges, st.edges),
            st.so_h_bits / e,
            pct(st.ent_together_edges, st.ent_multi_edges),
            de as f64 / e,
            st.cur_blk as f64 / e,
            st.s_rel as f64 / e,
            st.s_rel_zstd as f64 / e,
            st.t_rel as f64 / e,
            st.t_rel_zstd as f64 / e,
            st.fixed as f64 / e,
            st.units
        );
        all.add(&st);
    }
    // Frame 0's states (no edges leave them... the start) and the files of
    // frames before `from` are not counted.
    let e = all.edges.max(1) as f64;
    let per = |b: u64| b as f64 / e;
    println!("TOTAL f{}-f{}: {} edges, {} units ({} with sources out of id order), {} sources ({} without an edge) in {} source entries ({:.2} sources an entry), {} lids; {split_total} source entries cut by a unit boundary",
        a.from.max(1), a.to, all.edges, all.units, all.unsorted_units, all.sources, all.sources_no_edge, all.src_entries, all.sources as f64 / all.src_entries.max(1) as f64, all.lids);
    println!("A (target entry): {}", all.a.line());
    println!("B (target key):   {}", all.b.line());
    println!(
        "B bundles by target regions merged (1/2/3/4/5+): {:?}; A bundles per B bundle {:.3}",
        all.b_regions,
        all.a.bundles() as f64 / all.b.bundles().max(1) as f64
    );
    println!(
        "uniform offset: (src entry, target entry, transfer) groups {} with one offset {:.1}% ({:.1}% of edges); by target key {} groups, {:.1}% ({:.1}% of edges)",
        all.shift_a,
        pct(all.shift_a_uni, all.shift_a),
        pct(all.shift_a_uni_edges, all.edges),
        all.shift_b,
        pct(all.shift_b_uni, all.shift_b),
        pct(all.shift_b_uni_edges, all.edges)
    );
    println!(
        "transfer out of the key: (src entry, target entry, offset) {} bundles ({:.3} of A), one transfer {:.1}% ({:.1}% of edges); by key {} ({:.3} of B), one transfer {:.1}% ({:.1}% of edges)",
        all.a_nox,
        all.a_nox as f64 / all.a.bundles().max(1) as f64,
        pct(all.a_nox_uni, all.a_nox),
        pct(all.a_nox_uni_edges, all.edges),
        all.b_nox,
        all.b_nox as f64 / all.b.bundles().max(1) as f64,
        pct(all.b_nox_uni, all.b_nox),
        pct(all.b_nox_uni_edges, all.edges)
    );
    println!(
        "(src entry, offset): {} groups ({:.2} edges each), one transfer {:.1}% ({:.1}% of edges), H(transfer | src entry, offset) {:.3} bits an edge; cells in classes of equal (target, transfer) sets: {} classes listing {} items ({:.2} edges an item)",
        all.so,
        all.edges as f64 / all.so.max(1) as f64,
        pct(all.so_uni, all.so),
        pct(all.so_uni_edges, all.edges),
        all.so_h_bits / e,
        all.so_classes,
        all.so_class_items,
        all.edges as f64 / all.so_class_items.max(1) as f64
    );
    println!(
        "whole entry: {} source entries with edges at one cell ({:.1}% of edges), {} at several; of those {:.1}% move together ({:.1}% of their edges); classes {} ({:.2} an entry) listing {} (key, transfer, offset) items ({:.2} edges an item)",
        all.ent_one,
        pct(all.ent_one_edges, all.edges),
        all.ent_multi,
        pct(all.ent_together, all.ent_multi),
        pct(all.ent_together_edges, all.ent_multi_edges),
        all.ent_classes,
        all.ent_classes as f64 / all.ent_multi.max(1) as f64,
        all.ent_class_items,
        all.ent_multi_edges as f64 / all.ent_class_items.max(1) as f64
    );
    let cur_tables = all.cur_starts + all.cur_xfers + all.cur_src + all.cur_owner;
    println!(
        "bytes an edge, built: on disk {:.3} (edge files, {:.1} MB) = block {:.3} + starts {:.3} + transfer ranks {:.3} + explicit sources {:.3} + owner index {:.3} (+ headers); block re-encoded {:.3} ({}), zstd -1 {:.3}",
        per(disk_edges),
        mb(disk_edges),
        per(all.cur_blk),
        per(all.cur_starts),
        per(all.cur_xfers),
        per(all.cur_src),
        per(all.cur_owner),
        per(all.cur_reenc),
        if all.cur_reenc == all.cur_blk { "equal" } else { "differs" },
        per(all.cur_zstd)
    );
    println!(
        "bytes an edge, A bundles per unit block: S varint relative masks {:.3} (by field: entry {:.3} lid {:.3} transfer {:.3} offset {:.3} mask {:.3}; zstd -1 {:.3}), S varint u64 masks {:.3} (zstd {:.3}), T varint relative masks {:.3} (lid {:.3} entry {:.3} transfer {:.3} offset {:.3} mask {:.3}; zstd {:.3}), S fixed width u64 masks {:.3} (zstd {:.3}); transfer ranks {:.3}",
        per(all.s_rel),
        per(all.s_fields[0]),
        per(all.s_fields[1]),
        per(all.s_fields[2]),
        per(all.s_fields[3]),
        per(all.s_fields[4]),
        per(all.s_rel_zstd),
        per(all.s_64),
        per(all.s_64_zstd),
        per(all.t_rel),
        per(all.t_fields[0]),
        per(all.t_fields[1]),
        per(all.t_fields[2]),
        per(all.t_fields[3]),
        per(all.t_fields[4]),
        per(all.t_rel_zstd),
        per(all.fixed),
        per(all.fixed_zstd),
        per(all.xfer_ranks)
    );
    println!(
        "tables an edge: built's (starts, ranks, sources, owner index) {:.3}; owner index delta-coded {:.3}; reverse walk in a unit: S per-lid bundle lists {:.3}, T per-lid starts (varint) {:.3}",
        per(cur_tables),
        per(owner_varint),
        per(all.s_lid_lists),
        per(all.t_lid_starts)
    );
    println!(
        "totals an edge (block + transfer ranks + explicit sources + owner index delta-coded + reverse walk): built {:.3}; S {:.3} (zstd blocks {:.3}); T {:.3} (zstd blocks {:.3})",
        per(all.cur_blk + all.cur_xfers + all.cur_src + owner_varint + all.cur_starts),
        per(all.s_rel + all.xfer_ranks + all.cur_src + owner_varint + all.s_lid_lists),
        per(all.s_rel_zstd + all.xfer_ranks + all.cur_src + owner_varint + all.s_lid_lists),
        per(all.t_rel + all.xfer_ranks + all.cur_src + owner_varint + all.t_lid_starts),
        per(all.t_rel_zstd + all.xfer_ranks + all.cur_src + owner_varint + all.t_lid_starts)
    );
    println!(
        "states (layers f{}-f{}): {states}, frames on disk {:.1} MB = {:.2} B a state; edges {:.1} an edge source state ({:.1} a state), on disk {:.1} B a state",
        a.from.max(1),
        a.to,
        mb(disk_frames),
        disk_frames as f64 / states.max(1) as f64,
        all.edges as f64 / all.sources.max(1) as f64,
        all.edges as f64 / states.max(1) as f64,
        disk_edges as f64 / states.max(1) as f64
    );
    if let Some((_, nodes, entries)) = &marks {
        // The in-RAM graph: arc_dp's CSR against bundles.
        let csr = 12 * all.edges + 16 * nodes;
        // A bundle: target entry (its node base), transfer, offset, mask
        // (20 B) and its place in its target's list (4 B); a marked entry:
        // its first node and its marks (16 B), its first bundle (8 B).
        let bundle = (20 + 4) * all.a.bundles() + (16 + 8) * entries;
        println!(
            "in RAM (marked): CSR {:.1} MB (12 B an edge + 16 B a node) against bundles {:.1} MB (24 B an A bundle + 24 B a marked entry): {:.2}x",
            mb(csr),
            mb(bundle),
            csr as f64 / bundle.max(1) as f64
        );
    }
    if let (Some(h), true) = (a.marked, a.w_share) {
        w_share(dir, h, a.threads)?;
    }
    if a.time_reps > 0 {
        println!("unit end timed ({} reps, the minimum per unit, summed; {} threads): {}", a.time_reps, a.threads, tim.line());
    }
    Ok(())
}

/// `--w-share`: the loaded graph (`arc_dp::load`) and its backward. Per
/// frame `t`, the nodes with a winning set, their entries, and the distinct
/// (entry, set) pairs - what "W per entry, with a cell mask per distinct
/// set" would hold; and the graph's A bundles (per source entry: target
/// entry, transfer, offset), per `t` the distinct `W_{t+1}` among each
/// bundle's targets against its edges with a target set - the pulls a
/// bundle needs when equal sets pull once. Sets compared as the backward's
/// arena entries (equal sets are interned to one: at most the distinct
/// count).
fn w_share(dir: &Path, horizon: u32, threads: usize) -> Result<()> {
    let l = crate::search::arc_dp::load(dir, horizon, threads)?;
    let g = &l.graph;
    let w = crate::search::arc_dp::backward(g, horizon, threads);
    let n = g.len();
    let entry = |i: u32| {
        let id = g.id(i);
        (id_region(id), id_entry(id))
    };
    // Per frame its (node, set) pairs.
    let mut by_t: Vec<Vec<(u32, usize)>> = vec![Vec::new(); horizon as usize + 2];
    for (node, lo, hi, set) in w.spans() {
        for t in lo..=hi {
            by_t[t as usize].push((node, set as *const _ as usize));
        }
    }
    // The graph's A bundles: per source entry, (target entry, transfer,
    // offset) -> the source nodes and their targets.
    let geo = super::GEOMETRY;
    let cell = |i: u32| super::id_cell(&geo, g.id(i));
    let mut bundles: Vec<Vec<(u32, u32)>> = Vec::new();
    let mut i = 0u32;
    while (i as usize) < n {
        let e = entry(i);
        let mut j = i;
        let mut edges: Vec<((u32, u32), u32, u32, u32, u32)> = Vec::new();
        while (j as usize) < n && entry(j) == e {
            for o in g.out_of(j) {
                edges.push((entry(o.dst), o.xfer, offset(cell(j), cell(o.dst)), j, o.dst));
            }
            j += 1;
        }
        edges.sort_unstable();
        each_run(&edges, |p, q| (p.0, p.1, p.2) == (q.0, q.1, q.2), |run| bundles.push(run.iter().map(|x| (x.3, x.4)).collect()));
        i = j;
    }
    let n_edges: usize = bundles.iter().map(Vec::len).sum();
    let (mut nodes_t, mut entries_t, mut pairs_t, mut pulls, mut pulls_shared) = (0u64, 0u64, 0u64, 0u64, 0u64);
    let mut next: Vec<usize> = vec![0; n];
    let mut cur: Vec<usize> = vec![0; n];
    for t in (0..=horizon as usize).rev() {
        cur.iter_mut().for_each(|x| *x = 0);
        let mut v = std::mem::take(&mut by_t[t]);
        v.sort_unstable();
        for &(node, set) in &v {
            cur[node as usize] = set;
        }
        nodes_t += v.len() as u64;
        each_run(&v, |p, q| entry(p.0) == entry(q.0), |run| {
            entries_t += 1;
            let mut sets: Vec<usize> = run.iter().map(|x| x.1).collect();
            sets.sort_unstable();
            sets.dedup();
            pairs_t += sets.len() as u64;
        });
        if t < horizon as usize {
            // Pulling W_t from W_{t+1} (`next`) along each bundle.
            for b in &bundles {
                let mut sets: Vec<usize> = b.iter().map(|&(_, d)| next[d as usize]).filter(|&s| s != 0).collect();
                pulls += sets.len() as u64;
                sets.sort_unstable();
                sets.dedup();
                pulls_shared += sets.len() as u64;
            }
        }
        std::mem::swap(&mut cur, &mut next);
    }
    println!(
        "W per entry (h{horizon}, {n} nodes, {} A bundles of {n_edges} edges): over frames, {nodes_t} (node, t) with a set in {entries_t} (entry, t); distinct (entry, set) {pairs_t} ({:.3} a node: cells sharing a set); pulls along edges with a target set {pulls}, distinct target sets per bundle {pulls_shared} ({:.3} of the pulls)",
        bundles.len(),
        pairs_t as f64 / nodes_t.max(1) as f64,
        pulls_shared as f64 / pulls.max(1) as f64
    );
    Ok(())
}
