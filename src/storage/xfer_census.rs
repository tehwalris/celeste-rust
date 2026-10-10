//! DIAGNOSTIC (`rewrite xfer-census`): how the recorded edges' TRANSFERS
//! (`search::arc_edges::Pair`) are distributed - what an edge format could
//! exploit. Per transfer its edge count (distinct, top-k coverage,
//! singletons, entropy, the most common decoded), the factorisation into
//! the x and y parts (distinct parts, joint entropy against the marginals,
//! guard and action kinds), the entropy CONDITIONED on simple context (the
//! source's shape, the shift, the (source entry, target entry) pair, ...),
//! and what storage v2's per-unit rank field costs.
//!
//! Two inputs: a storage-v2 level dir (every frame, or one), and an
//! emission capture of the `emit-capture` branch (`DIR/w{n}.bin`: 48-byte
//! records `(src u64, key0 u64, key1 u64, shape u64, cell u32, xfer u32,
//! flags u32, pad u32)`, flags 0 an edge, 1 dropped by level -1, 2 a unit
//! starts; `DIR/x{n}.bin`: worker `n`'s transfer table, `encode_pair` each;
//! `DIR/door.bin`: 40-byte records `(shape u64, cell u32, pad u32, key0,
//! key1, id u64)`). The capture's transfer ids are worker-local: they are
//! interned by content here.
//!
//! Conditional entropies are accumulated per BATCH (a frame) as `sum n log n`
//! over contexts and over (context, transfer): over a whole forward they
//! condition on the frame too. A context seen once costs nothing in
//! `H(T | C)`, so a fine context (the entry pair) understates what an
//! encoder would pay: its context and singleton counts are printed beside it.

use anyhow::{ensure, Context, Result};
use rustc_hash::FxHashMap;
use std::path::Path;

use crate::search::arc_edges::{decode_pair, AxisXfer, Pair, PAIR_BYTES};
use crate::search::arcs::CIRCLE;
use crate::search::pos_graph::{cell_xy, NO_CELL};

/// One edge as the census sees it: its transfer (canonical index) and its
/// context, hashed where wide.
#[derive(Clone, Copy)]
pub struct EdgeCtx {
    pub xfer: u32,
    pub shape: u32,
    pub dx: i16,
    pub dy: i16,
    /// The source state.
    pub src: u64,
    /// The source's entry: (shape, storage region, non-position key).
    pub src_entry: u64,
    /// The target's entry.
    pub dst_entry: u64,
}

#[inline]
fn mix(mut z: u64) -> u64 {
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^ (z >> 31)
}

#[inline]
pub fn hash(parts: &[u64]) -> u64 {
    parts.iter().fold(0x9e3779b97f4a7c15u64, |h, &p| mix(h ^ mix(p.wrapping_add(0x632be59bd9b4e019))))
}

/// The contexts conditioned on (name, the context's hash of an edge).
type CtxFn = fn(&EdgeCtx) -> u64;
const CONTEXTS: [(&str, CtxFn); 10] = [
    ("source shape", |e| e.shape as u64),
    ("shift (dx, dy)", |e| hash(&[e.dx as u64, e.dy as u64])),
    ("source shape + shift", |e| hash(&[e.shape as u64, e.dx as u64, e.dy as u64])),
    ("source entry", |e| e.src_entry),
    ("target entry", |e| e.dst_entry),
    ("(source entry, shift)", |e| hash(&[e.src_entry, e.dx as u64, e.dy as u64])),
    ("(target entry, shift)", |e| hash(&[e.dst_entry, e.dx as u64, e.dy as u64])),
    ("(source entry, target entry)", |e| hash(&[e.src_entry, e.dst_entry])),
    ("(source entry, target entry, shift)", |e| hash(&[e.src_entry, e.dst_entry, e.dx as u64, e.dy as u64])),
    ("source state", |e| e.src),
];

#[derive(Default, Clone, Copy)]
struct CtxAcc {
    /// `sum n_c log2 n_c` over contexts, `sum n_ct log2 n_ct` over (context, transfer).
    s_c: f64,
    s_ct: f64,
    contexts: u64,
    single_edge: u64,
    /// Contexts whose edges all carry one transfer, and their edges.
    pure: u64,
    pure_edges: u64,
}

fn nlogn(n: u64) -> f64 {
    if n <= 1 {
        0.0
    } else {
        n as f64 * (n as f64).log2()
    }
}

/// The census's running totals.
pub struct Census {
    pairs: Vec<Pair>,
    intern: FxHashMap<Pair, u32>,
    counts: Vec<u64>,
    edges: u64,
    ctx: [CtxAcc; CONTEXTS.len()],
    /// Storage v2's transfer field: rank varint bytes, table entries; and
    /// `sum n log n` per unit (`H(T | unit)`).
    rank_bytes: u64,
    table_entries: u64,
    s_unit: f64,
    units: u64,
    batches: u64,
    scratch: Vec<u128>,
}

impl Default for Census {
    fn default() -> Self {
        Census {
            pairs: Vec::new(),
            intern: FxHashMap::default(),
            counts: Vec::new(),
            edges: 0,
            ctx: [CtxAcc::default(); CONTEXTS.len()],
            rank_bytes: 0,
            table_entries: 0,
            s_unit: 0.0,
            units: 0,
            batches: 0,
            scratch: Vec::new(),
        }
    }
}

fn varint_len(v: u64) -> u64 {
    (64 - (v | 1).leading_zeros() as u64).div_ceil(7)
}

impl Census {
    /// The canonical index of a pair (interned by content).
    pub fn intern(&mut self, p: Pair) -> u32 {
        if let Some(&i) = self.intern.get(&p) {
            return i;
        }
        let i = self.pairs.len() as u32;
        self.pairs.push(p);
        self.counts.push(0);
        self.intern.insert(p, i);
        i
    }

    /// One unit's edges' transfer field as storage v2 writes it: `ranks`
    /// the field's values, `n_table` its table's entries; `xfers` the
    /// edges' canonical transfers (for `H(T | unit)`).
    pub fn unit(&mut self, ranks: impl Iterator<Item = u32>, n_table: usize, xfers: &mut [u32]) {
        self.rank_bytes += ranks.map(|r| varint_len(r as u64)).sum::<u64>();
        self.table_entries += n_table as u64;
        self.units += 1;
        xfers.sort_unstable();
        let mut i = 0;
        while i < xfers.len() {
            let mut j = i + 1;
            while j < xfers.len() && xfers[j] == xfers[i] {
                j += 1;
            }
            self.s_unit += nlogn((j - i) as u64);
            i = j;
        }
        self.s_unit -= nlogn(xfers.len() as u64);
    }

    /// `unit` for a unit's edges without their ranks (a capture): ranked
    /// as `unit::UnitSink` ranks them, by count in the unit, then id.
    /// Clears `xs`.
    pub fn unit_ranked_here(&mut self, xs: &mut Vec<u32>) {
        if xs.is_empty() {
            return;
        }
        let mut cnt: FxHashMap<u32, u32> = FxHashMap::default();
        for &x in xs.iter() {
            *cnt.entry(x).or_default() += 1;
        }
        let mut order: Vec<(u32, u32)> = cnt.iter().map(|(&x, &k)| (x, k)).collect();
        order.sort_unstable_by_key(|&(x, k)| (std::cmp::Reverse(k), x));
        let rank: FxHashMap<u32, u32> = order.iter().enumerate().map(|(r, &(x, _))| (x, r as u32)).collect();
        let ranks: Vec<u32> = xs.iter().map(|x| rank[x]).collect();
        self.unit(ranks.into_iter(), order.len(), xs);
        xs.clear();
    }

    /// A batch of edges (one frame): counts and the conditional sums.
    pub fn batch(&mut self, edges: &[EdgeCtx]) {
        self.batches += 1;
        self.edges += edges.len() as u64;
        for e in edges {
            self.counts[e.xfer as usize] += 1;
        }
        let v = &mut self.scratch;
        for (k, (_, f)) in CONTEXTS.iter().enumerate() {
            // (context, transfer), sorted: runs of the whole are (context,
            // transfer) groups, runs of the high half contexts.
            v.clear();
            v.extend(edges.iter().map(|e| (f(e) as u128) << 32 | e.xfer as u128));
            v.sort_unstable();
            let a = &mut self.ctx[k];
            let mut i = 0;
            while i < v.len() {
                let mut j = i + 1;
                let mut groups = 1;
                let mut g = i;
                while j < v.len() && v[j] >> 32 == v[i] >> 32 {
                    if v[j] != v[g] {
                        a.s_ct += nlogn((j - g) as u64);
                        groups += 1;
                        g = j;
                    }
                    j += 1;
                }
                a.s_ct += nlogn((j - g) as u64);
                let n = (j - i) as u64;
                a.s_c += nlogn(n);
                a.contexts += 1;
                a.single_edge += (n == 1) as u64;
                if groups == 1 {
                    a.pure += 1;
                    a.pure_edges += n;
                }
                i = j;
            }
        }
    }

    /// The report.
    pub fn print(&self) {
        let n = self.edges as f64;
        let used: Vec<usize> = (0..self.pairs.len()).filter(|&i| self.counts[i] > 0).collect();
        println!("== {} edges in {} batches, {} distinct transfers (table {}, {} B a pair)", self.edges, self.batches, used.len(), self.pairs.len(), PAIR_BYTES);
        if self.edges == 0 {
            return;
        }
        let h = |counts: &mut dyn Iterator<Item = u64>| -> f64 { (n * n.log2() - counts.map(nlogn).sum::<f64>()) / n };
        let h_t = h(&mut used.iter().map(|&i| self.counts[i]));
        let mut by: Vec<usize> = used.clone();
        by.sort_unstable_by_key(|&i| (std::cmp::Reverse(self.counts[i]), i));

        // Edges per transfer.
        println!("-- edges per transfer");
        let singles = used.iter().filter(|&&i| self.counts[i] == 1).count();
        let mut buckets = [0u64; 12];
        let mut bucket_edges = [0u64; 12];
        for &i in &used {
            let b = (64 - self.counts[i].leading_zeros() as usize).div_ceil(3).min(11);
            buckets[b] += 1;
            bucket_edges[b] += self.counts[i];
        }
        println!("  singletons {singles} ({:.1}% of transfers, {:.4}% of edges); mean {:.0} edges a transfer, median {}", 100.0 * singles as f64 / used.len() as f64, 100.0 * singles as f64 / n, n / used.len() as f64, self.counts[by[by.len() / 2]]);
        for b in 1..12 {
            if buckets[b] > 0 {
                let (lo, hi) = (1u64 << (3 * b - 3), (1u64 << (3 * b)) - 1);
                println!("  {lo:>10}..{hi:<10} edges: {:>7} transfers, {:>6.2}% of edges", buckets[b], 100.0 * bucket_edges[b] as f64 / n);
            }
        }
        let mut acc = 0u64;
        let mut cover = Vec::new();
        let mut targets = [0.5, 0.9, 0.99, 0.999].into_iter().peekable();
        for (k, &i) in by.iter().enumerate() {
            acc += self.counts[i];
            while let Some(&t) = targets.peek() {
                if acc as f64 >= t * n {
                    cover.push(format!("{:.1}%: {}", 100.0 * t, k + 1));
                    targets.next();
                } else {
                    break;
                }
            }
        }
        println!("  top-k covering: {}", cover.join(", "));
        println!("  entropy H(T) = {h_t:.3} bits an edge (log2 distinct = {:.2})", (used.len() as f64).log2());

        println!("-- the 20 most common (guard: the remainders taking the edge; action)");
        for &i in by.iter().take(20) {
            let (x, y) = self.pairs[i];
            println!("  {:>6.2}%  x {:<34} y {}", 100.0 * self.counts[i] as f64 / n, show_axis(&x), show_axis(&y));
        }

        // Factorisation.
        println!("-- factorisation");
        let mut xs: FxHashMap<AxisXfer, u64> = FxHashMap::default();
        let mut ys: FxHashMap<AxisXfer, u64> = FxHashMap::default();
        for &i in &used {
            *xs.entry(self.pairs[i].0).or_default() += self.counts[i];
            *ys.entry(self.pairs[i].1).or_default() += self.counts[i];
        }
        let (hx, hy) = (h(&mut xs.values().copied()), h(&mut ys.values().copied()));
        println!(
            "  distinct x parts {}, y parts {} (product {}; pairs used {}); H(X) {hx:.3} + H(Y) {hy:.3} = {:.3} bits against H(X,Y) {h_t:.3}: mutual information {:.3} bits",
            xs.len(),
            ys.len(),
            xs.len() as u64 * ys.len() as u64,
            used.len(),
            hx + hy,
            hx + hy - h_t
        );
        for (name, parts) in [("x", &xs), ("y", &ys)] {
            let mut kinds: std::collections::BTreeMap<(&str, &str), (u64, u64)> = Default::default();
            let mut rots = rustc_hash::FxHashSet::default();
            let mut consts = rustc_hash::FxHashSet::default();
            let mut cuts = rustc_hash::FxHashSet::default();
            for (a, &c) in parts.iter() {
                let k = kinds.entry((guard_kind(a), action_kind(a))).or_default();
                k.0 += 1;
                k.1 += c;
                if a.tag == 0 {
                    rots.insert(a.val);
                } else {
                    consts.insert(a.val);
                }
                if (a.lo, a.hi) != (0, CIRCLE) {
                    cuts.insert(if a.lo == 0 { a.hi } else { a.lo });
                }
            }
            println!("  {name}: {} rotation values, {} constants, {} cut points; (guard, action) kinds - parts, % of edges:", rots.len(), consts.len(), cuts.len());
            let mut v: Vec<_> = kinds.into_iter().collect();
            v.sort_by_key(|x| std::cmp::Reverse(x.1 .1));
            for ((g, a), (p, c)) in v {
                println!("    {g:<13} {a:<14} {p:>7} parts  {:>6.2}%", 100.0 * c as f64 / n);
            }
        }

        println!("-- conditional entropy H(T | C), bits an edge (per batch; contexts once-seen cost 0 here)");
        println!("  {:<38} {:>8} {:>12} {:>12} {:>16}", "context C", "H(T|C)", "contexts", "single-edge", "pure (1 xfer) % edges");
        println!("  {:<38} {:>8.3}", "(none)", h_t);
        for (k, (name, _)) in CONTEXTS.iter().enumerate() {
            let a = &self.ctx[k];
            println!("  {name:<38} {:>8.3} {:>12} {:>12} {:>15.1}%", (a.s_c - a.s_ct) / n, a.contexts, a.single_edge, 100.0 * a.pure_edges as f64 / n);
        }
        if self.units > 0 {
            println!("-- storage v2's transfer field (per-unit rank, by unit-local frequency)");
            println!(
                "  {} units; rank varints {:.3} bits an edge, tables {} entries = {:.3} bits an edge at 4 B; together {:.3} bits an edge. H(T | unit) {:.3} bits an edge",
                self.units,
                8.0 * self.rank_bytes as f64 / n,
                self.table_entries,
                32.0 * self.table_entries as f64 / n,
                8.0 * (self.rank_bytes + 4 * self.table_entries) as f64 / n,
                -self.s_unit / n
            );
        }
    }
}

fn guard_kind(a: &AxisXfer) -> &'static str {
    match (a.lo, a.hi) {
        (0, CIRCLE) => "full",
        (lo, hi) if hi - lo == 1 => "point",
        (0, _) => "low [0,c)",
        (_, CIRCLE) => "high [c,1)",
        _ => "inner",
    }
}

fn action_kind(a: &AxisXfer) -> &'static str {
    match (a.tag, a.val) {
        (0, 0) => "identity",
        (0, _) => "rotate",
        (_, 32768) => "set rem 0",
        _ => "set rem c",
    }
}

/// A raw 16.16 remainder in px.
fn px(raw: i64) -> String {
    format!("{:.4}", raw as f64 / 65536.0)
}

fn show_axis(a: &AxisXfer) -> String {
    let guard = if (a.lo, a.hi) == (0, CIRCLE) { "all".to_string() } else { format!("[{}, {})", px(a.lo as i64 - 32768), px(a.hi as i64 - 32768)) };
    let action = match a.tag {
        0 if a.val == 0 => "id".to_string(),
        0 => {
            let v = if a.val >= 32768 { a.val as i64 - 65536 } else { a.val as i64 };
            format!("rot {}", px(v))
        }
        _ => format!("set {}", px(a.val as i64 - 32768)),
    };
    format!("{guard} {action}")
}

fn cell_dxy(src: u32, dst: u32) -> (i16, i16) {
    match (cell_xy(src), cell_xy(dst)) {
        (Some((a, b)), Some((c, d))) => ((c - a) as i16, (d - b) as i16),
        // A cell-less side (no player): a shift no real one has.
        _ => (i16::MIN, (src == NO_CELL) as i16 * 2 + (dst == NO_CELL) as i16),
    }
}

/// A storage-v2 level dir: frames `from..=to` of `DIR/edges`. `simulate`:
/// rank each unit's transfers here (`Census::unit_ranked_here`) instead of
/// reading the file's ranks - a check of that simulation.
pub fn census_tree(level_dir: &Path, from: u32, to: u32, simulate: bool) -> Result<Census> {
    use super::{geometry, id_cell, id_entry, id_region, state_id};
    let store = super::edges::EdgeStore::open(&level_dir.join("edges"), to)?;
    let geo = *geometry();
    let mut c = Census::default();
    let canon: Vec<u32> = store.pairs().to_vec().into_iter().map(|p| c.intern(p)).collect();
    ensure!(canon.len() == store.pairs().len(), "the tree's transfer table is not content-canonical");
    let entry = |id: u64| hash(&[id_region(id) as u64, id_entry(id) as u64]);
    let t = std::time::Instant::now();
    let mut edges: Vec<EdgeCtx> = Vec::new();
    let (mut ranks, mut xs): (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
    for frame in from..=to {
        if !store.has_frame(frame) {
            continue;
        }
        edges.clear();
        let owners = store.owners(frame);
        for (fi, ui) in store.units(frame) {
            let u = store.unit(frame, &owners, fi, ui);
            ranks.clear();
            xs.clear();
            u.edges_ranked(|lid, cell, s, rank, x| {
                let (region, e) = u.lid_owner(lid).expect("an edge names an owned lid");
                let (src, dst) = (u.source(s), state_id(region, e, cell));
                let (dx, dy) = cell_dxy(id_cell(&geo, src), id_cell(&geo, dst));
                let xfer = canon[x as usize];
                edges.push(EdgeCtx { xfer, shape: id_region(src) / geo.slots, dx, dy, src, src_entry: entry(src), dst_entry: entry(dst) });
                ranks.push(rank);
                xs.push(xfer);
            });
            if simulate {
                c.unit_ranked_here(&mut xs);
            } else {
                c.unit(ranks.iter().copied(), u.n_xfers(), &mut xs);
            }
        }
        c.batch(&edges);
        eprintln!("[xfer-census] f{frame:03}: {} edges ({} total), {:.1} s", edges.len(), c.edges, t.elapsed().as_secs_f64());
    }
    Ok(c)
}

/// An emission capture of the `emit-capture` branch (module doc).
pub fn census_capture48(dir: &Path) -> Result<Census> {
    let t = std::time::Instant::now();
    let map = |p: &Path| -> Result<memmap2::Mmap> {
        let f = std::fs::File::open(p).with_context(|| p.display().to_string())?;
        Ok(unsafe { memmap2::Mmap::map(&f)? })
    };
    let door = map(&dir.join("door.bin"))?;
    ensure!(door.len() % 40 == 0, "door.bin: not 40-byte records");
    let u64_at = |b: &[u8], o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
    let u32_at = |b: &[u8], o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
    // Source id -> door row; shapes numbered.
    let n_door = door.len() / 40;
    let mut row_of: FxHashMap<u64, u32> = FxHashMap::default();
    row_of.reserve(n_door);
    let mut shapes: FxHashMap<u64, u32> = FxHashMap::default();
    // Is the key position-free? (shape, key) against (shape, key, cell).
    let (mut sk, mut skc): (Vec<u64>, Vec<u64>) = (Vec::with_capacity(n_door), Vec::with_capacity(n_door));
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        row_of.insert(u64_at(b, 32), i as u32);
        let n = shapes.len() as u32;
        shapes.entry(u64_at(b, 0)).or_insert(n);
        let h = hash(&[u64_at(b, 0), u64_at(b, 16), u64_at(b, 24)]);
        sk.push(h);
        skc.push(hash(&[h, u32_at(b, 8) as u64]));
    }
    sk.sort_unstable();
    sk.dedup();
    skc.sort_unstable();
    skc.dedup();
    println!("[capture] door {n_door} states: {} distinct (shape, key), {} distinct (shape, key, cell); {} shapes", sk.len(), skc.len(), shapes.len());
    if sk.len() == skc.len() {
        println!("[capture] NOTE: the capture's keys hold the position, so an \"entry\" below is a STATE (no position-free key to group by)");
    }
    drop((sk, skc));
    let region = |cell: u32| -> u64 {
        match cell_xy(cell) {
            Some((x, y)) => hash(&[x.div_euclid(8) as u64, y.div_euclid(8) as u64]),
            None => u64::MAX,
        }
    };
    let entry = |shape: u64, cell: u32, k0: u64, k1: u64| hash(&[shape, region(cell), k0, k1]);
    let mut c = Census::default();
    let mut edges: Vec<EdgeCtx> = Vec::new();
    let mut xs: Vec<u32> = Vec::new();
    let mut dropped = 0u64;
    for n in 0.. {
        let w = dir.join(format!("w{n:03}.bin"));
        if !w.exists() {
            break;
        }
        let table = std::fs::read(dir.join(format!("x{n:03}.bin")))?;
        ensure!(table.len() % PAIR_BYTES == 0, "x{n:03}.bin: not whole pairs");
        let local: Vec<u32> = table.chunks_exact(PAIR_BYTES).map(|b| c.intern(decode_pair(b))).collect();
        let m = map(&w)?;
        ensure!(m.len() % 48 == 0, "{}: not 48-byte records", w.display());
        for r in m.chunks_exact(48) {
            match u32_at(r, 40) {
                0 => {}
                2 => {
                    c.unit_ranked_here(&mut xs);
                    continue;
                }
                _ => {
                    dropped += 1;
                    continue;
                }
            }
            let src_id = u64_at(r, 0);
            let s = &door[*row_of.get(&src_id).with_context(|| format!("source {src_id} not in the door"))? as usize * 40..][..40];
            let (s_shape, s_cell) = (u64_at(s, 0), u32_at(s, 8));
            let (t_shape, t_cell) = (u64_at(r, 24), u32_at(r, 32));
            let (dx, dy) = cell_dxy(s_cell, t_cell);
            let xfer = local[u32_at(r, 36) as usize];
            edges.push(EdgeCtx {
                xfer,
                shape: shapes[&s_shape],
                dx,
                dy,
                src: src_id,
                src_entry: entry(s_shape, s_cell, u64_at(s, 16), u64_at(s, 24)),
                dst_entry: entry(t_shape, t_cell, u64_at(r, 8), u64_at(r, 16)),
            });
            xs.push(xfer);
        }
        c.unit_ranked_here(&mut xs);
        eprintln!("[xfer-census] w{n:03}: {} edges so far, {:.1} s", edges.len(), t.elapsed().as_secs_f64());
    }
    println!("[capture] {} edges, {dropped} dropped emissions skipped", edges.len());
    drop(row_of);
    c.batch(&edges);
    eprintln!("[xfer-census] done, {:.1} s", t.elapsed().as_secs_f64());
    Ok(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn axis(lo: u32, hi: u32, val: i32) -> AxisXfer {
        AxisXfer { lo, hi, tag: 0, val }
    }

    /// Two transfers, each its own source shape: one bit an edge without
    /// context, none given the shape, one given the shift (shared); a unit
    /// ranks the commoner transfer 0.
    #[test]
    fn conditional_entropy_and_ranks_count_what_they_say() {
        let mut c = Census::default();
        let a = c.intern((axis(0, CIRCLE, 0), axis(0, CIRCLE, 0)));
        let b = c.intern((axis(0, 100, 7), axis(0, CIRCLE, 0)));
        assert_eq!(c.intern((axis(0, 100, 7), axis(0, CIRCLE, 0))), b, "interned by content");
        let e = |xfer: u32, shape: u32| EdgeCtx { xfer, shape, dx: 1, dy: 0, src: shape as u64, src_entry: shape as u64, dst_entry: 9 };
        let edges = [e(a, 0), e(a, 0), e(b, 1), e(b, 1)];
        c.batch(&edges);
        let h = |k: usize| (c.ctx[k].s_c - c.ctx[k].s_ct) / c.edges as f64;
        assert_eq!(h(0), 0.0, "the shape decides the transfer");
        assert!((h(1) - 1.0).abs() < 1e-12, "the shift does not: one bit");
        assert_eq!((c.ctx[0].contexts, c.ctx[0].pure, c.ctx[1].pure), (2, 2, 0));
        let mut xs = vec![b, a, b];
        c.unit_ranked_here(&mut xs);
        assert!(xs.is_empty());
        assert_eq!((c.rank_bytes, c.table_entries), (3, 2));
        assert!((-c.s_unit / 3.0 - (3f64.log2() - 2.0 / 3.0)).abs() < 1e-12, "H(T | unit) of (2, 1)");
        assert_eq!((varint_len(0), varint_len(127), varint_len(128)), (1, 1, 2));
    }
}
