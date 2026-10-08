//! `rewrite format-study`: one finished tree re-encoded, as drafts, in
//! candidate formats for the level-0 graph (plans/format-study.md).
//!
//! Nothing here is written anywhere: every format is SIZED (and, where
//! cheap, timed) and printed. States: per `(shape, cell)` shard, each
//! column's dictionary or range, the bit-packed exact state, its sorted
//! deltas and Elias-Fano. Edges: per recorded frame, dense ids in two
//! orders, fixed width, varint by target and by source, Elias-Fano, and
//! the transfer coded several ways.

use anyhow::{ensure, Result};
use rustc_hash::FxHashMap;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use crate::frame::{frame_files, id_layer, id_row, id_seq};
use crate::search::checkpoint::{ColView, FrameFile};

/// Bytes of a LEB128 varint of `v`.
#[inline]
fn vbytes(v: u64) -> u64 {
    (64 - (v | 1).leading_zeros() as u64).div_ceil(7)
}

#[inline]
fn zigzag(d: i64) -> u64 {
    ((d << 1) ^ (d >> 63)) as u64
}

fn ceil_log2(n: u64) -> u32 {
    if n <= 1 {
        0
    } else {
        64 - (n - 1).leading_zeros()
    }
}

/// Elias-Fano bits for `n` sorted values in a universe of `2^log2u`.
fn ef_bits(n: u64, log2u: f64) -> f64 {
    if n == 0 {
        return 0.0;
    }
    let l = (log2u - (n as f64).log2()).floor().max(0.0);
    n as f64 * l + n as f64 + (log2u - l).exp2()
}

/// A shard's rows: `(file index, start, len)` runs.
type Runs = Vec<(u32, u32, u32)>;

/// What one shard contributes.
#[derive(Default, Clone)]
struct ShardOut {
    rows: u64,
    /// Bits of the packed state: per column the smaller of the dictionary's
    /// and the range's width.
    bits: u64,
    /// The same with the dictionary only, and with the range only.
    bits_dict: u64,
    bits_range: u64,
    /// sum log2(card): the mixed-radix width.
    radix: f64,
    /// Rows whose packed state another row of the shard also has.
    dups: u64,
    /// Bytes of the sorted packed states' varint deltas.
    delta_bytes: u64,
    /// Elias-Fano bits of the whole shard, universe 2^bits and the radix.
    ef: f64,
    ef_radix: f64,
    /// Elias-Fano bits of the shard as one run per layer (ids implicit).
    ef_layers: f64,
    /// Dictionary entries (8 B each in a draft).
    dict_entries: u64,
    /// Per varying column: (column, card, bits).
    cols: Vec<(u32, u64, u32)>,
    /// Today's door index words (`door::index_bits`).
    door_index_words: u64,
}

/// Per column of a shard: the values of every row as codes.
fn shard_codes(files: &[FrameFile], runs: &Runs, c: usize, n: usize) -> Result<Option<Vec<u64>>> {
    // Uniform everywhere with one value: no column.
    let mut uni: Option<u64> = None;
    let mut all_uniform = true;
    for &(f, _, _) in runs {
        match files[f as usize].col_view(c) {
            ColView::U(v) => {
                let code = celeste_engine::runtime2::av_code(v);
                if uni.is_some_and(|u| u != code) {
                    all_uniform = false;
                }
                uni = Some(code);
            }
            _ => all_uniform = false,
        }
    }
    if all_uniform {
        return Ok(None);
    }
    let mut out = Vec::with_capacity(n);
    for &(f, s, l) in runs {
        let v = files[f as usize].col_view(c);
        for r in s..s + l {
            out.push(v.code(r as usize)?);
        }
    }
    Ok(Some(out))
}

/// k := k << bits | v over `w` words, most significant first.
#[inline]
fn push_bits(k: &mut [u64], bits: u32, v: u64) {
    if bits == 0 {
        return;
    }
    if bits == 64 {
        for i in 0..k.len() - 1 {
            k[i] = k[i + 1];
        }
        *k.last_mut().unwrap() = v;
        return;
    }
    let mut carry = v;
    for w in k.iter_mut().rev() {
        let out = *w >> (64 - bits);
        *w = (*w << bits) | carry;
        carry = out;
    }
}

/// Bit length of `b - a` (b >= a), words most significant first.
fn diff_bits(a: &[u64], b: &[u64]) -> u64 {
    let w = a.len();
    let mut d = vec![0u64; w];
    let mut borrow = 0u64;
    for i in (0..w).rev() {
        let (x, o1) = b[i].overflowing_sub(a[i]);
        let (y, o2) = x.overflowing_sub(borrow);
        d[i] = y;
        borrow = (o1 || o2) as u64;
    }
    for (i, &x) in d.iter().enumerate() {
        if x != 0 {
            return (w - i - 1) as u64 * 64 + (64 - x.leading_zeros() as u64);
        }
    }
    0
}

fn study_shard(files: &[FrameFile], layer_of_file: &[u32], runs: &Runs, n_cols: usize, ranks: &mut Vec<(u32, u32, u32)>) -> Result<ShardOut> {
    let n: usize = runs.iter().map(|r| r.2 as usize).sum();
    let mut out = ShardOut { rows: n as u64, ..Default::default() };
    let bits = (n / 8).max(1).next_power_of_two().trailing_zeros();
    out.door_index_words = (1u64 << bits) + 1;
    // Per varying column: its codes mapped to small integers.
    let mut packed_cols: Vec<(u64, u32, Vec<u32>)> = Vec::new();
    for c in 0..n_cols {
        let Some(codes) = shard_codes(files, runs, c, n)? else { continue };
        let mut dict = codes.clone();
        dict.sort_unstable();
        dict.dedup();
        let card = dict.len() as u64;
        if card <= 1 {
            continue;
        }
        let dbits = ceil_log2(card);
        // The range, if every value is a number: (max - min) / gcd.
        let nums = dict.iter().all(|&v| v >> 56 == 1);
        let rbits = if nums {
            let vals: Vec<i64> = dict.iter().map(|&v| v as u32 as i32 as i64).collect();
            let (lo, hi) = (*vals.iter().min().unwrap(), *vals.iter().max().unwrap());
            let g = vals.iter().fold(0i64, |g, &v| gcd(g, v - lo));
            ceil_log2(((hi - lo) / g.max(1) + 1) as u64)
        } else {
            dbits
        };
        let b = dbits.min(rbits);
        out.bits += b as u64;
        out.bits_dict += dbits as u64;
        out.bits_range += rbits as u64;
        out.radix += (card as f64).log2();
        out.dict_entries += card;
        out.cols.push((c as u32, card, b));
        let idx: Vec<u32> = codes.iter().map(|v| dict.binary_search(v).unwrap() as u32).collect();
        packed_cols.push((card, dbits, idx));
    }
    // The packed state: the lowest-cardinality column most significant.
    packed_cols.sort_by_key(|c| c.0);
    let w = (out.bits_dict as usize).div_ceil(64).max(1);
    let mut keys = vec![0u64; n * w];
    for (_, b, idx) in &packed_cols {
        for (r, &v) in idx.iter().enumerate() {
            push_bits(&mut keys[r * w..(r + 1) * w], *b, v as u64);
        }
    }
    drop(packed_cols);
    let key = |r: usize| &keys[r * w..(r + 1) * w];
    let mut order: Vec<u32> = (0..n as u32).collect();
    order.sort_unstable_by(|&a, &b| key(a as usize).cmp(key(b as usize)));
    let zero = vec![0u64; w];
    let mut prev: &[u64] = &zero;
    for (i, &r) in order.iter().enumerate() {
        let k = key(r as usize);
        if i > 0 && k == prev {
            out.dups += 1;
        }
        out.delta_bytes += diff_bits(prev, k).div_ceil(7).max(1);
        prev = k;
    }
    out.ef = ef_bits(n as u64, out.bits_dict as f64);
    out.ef_radix = ef_bits(n as u64, out.radix);
    // Per row: its layer and its rank in the shard.
    let mut layer_rows: FxHashMap<u32, u64> = FxHashMap::default();
    let mut row_layer: Vec<u32> = Vec::with_capacity(n);
    for &(f, _, l) in runs {
        for _ in 0..l {
            row_layer.push(layer_of_file[f as usize]);
        }
        *layer_rows.entry(layer_of_file[f as usize]).or_default() += l as u64;
    }
    out.ef_layers = layer_rows.values().map(|&m| ef_bits(m, out.bits_dict as f64)).sum();
    // (row, rank) for the caller to map to ids.
    ranks.clear();
    for (rank, &r) in order.iter().enumerate() {
        ranks.push((r, rank as u32, row_layer[r as usize]));
    }
    Ok(out)
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a
}

/// A histogram in power-of-two buckets: 0, 1, 2, 3-4, 5-8, ...
#[derive(Default)]
struct Hist(Vec<u64>);

impl XStats {
    fn add(&mut self, o: &XStats) {
        self.edges += o.edges;
        self.h_pair += o.h_pair;
        self.h_x += o.h_x;
        self.h_y += o.h_y;
        self.h_given_src += o.h_given_src;
        self.h_given_tgt += o.h_given_tgt;
        self.local_src += o.local_src;
        self.local_tgt += o.local_tgt;
        self.distinct_src += o.distinct_src;
        self.sources += o.sources;
        self.distinct_tgt += o.distinct_tgt;
        self.targets += o.targets;
    }
}

impl Hist {
    fn add(&mut self, v: u64) {
        let b = if v == 0 { 0 } else { 1 + ceil_log2(v) as usize };
        if self.0.len() <= b {
            self.0.resize(b + 1, 0);
        }
        self.0[b] += 1;
    }
    fn show(&self) -> String {
        let total: u64 = self.0.iter().sum();
        let label = |b: usize| match b {
            0 => "0".to_string(),
            1 => "1".to_string(),
            2 => "2".to_string(),
            b => format!("{}-{}", (1u64 << (b - 2)) + 1, 1u64 << (b - 1)),
        };
        self.0
            .iter()
            .enumerate()
            .filter(|(_, &c)| c > 0)
            .map(|(b, &c)| format!("{}:{:.1}%", label(b), 100.0 * c as f64 / total as f64))
            .collect::<Vec<_>>()
            .join(" ")
    }
}

/// Edge-format sizes, bytes (or bits where named), summed over frames.
#[derive(Default, Debug, Clone)]
struct EdgeSizes {
    fixed_frame_bits: f64,
    fixed_global_bits: f64,
    by_target: u64,
    attached: u64,
    by_source: u64,
    ef_pairs_bits: f64,
    ef_lists_bits: f64,
    /// Transfer: rank varint bytes, order-0 entropy bits, fixed per-frame and
    /// global bits.
    x_rank: u64,
    x_entropy: f64,
    x_fixed_frame: f64,
    x_fixed_global: f64,
}

/// One frame's edges in dense ids: `(target, source, transfer)`, targets
/// in `0..n_t` (the layers up to the frame), sources in `0..n_s` (layer
/// `frame - 1`, as an offset from its first id), transfers by frequency rank.
fn size_edges(e: &mut [(u32, u32, u32)], n_t: u64, n_s: u64, n_x_global: u64, sz: &mut EdgeSizes, n_x: u64) {
    let m = e.len() as u64;
    if m == 0 {
        return;
    }
    let (bt, bs, bx) = (ceil_log2(n_t) as f64, ceil_log2(n_s) as f64, ceil_log2(n_x) as f64);
    sz.fixed_frame_bits += m as f64 * (bt + bs + bx);
    // Transfers: rank, entropy.
    let mut uses = vec![0u64; n_x as usize];
    for r in e.iter() {
        uses[r.2 as usize] += 1;
    }
    let mut ranked: Vec<u32> = (0..n_x as u32).collect();
    ranked.sort_by_key(|&x| std::cmp::Reverse(uses[x as usize]));
    let mut rank = vec![0u32; n_x as usize];
    for (i, &x) in ranked.iter().enumerate() {
        rank[x as usize] = i as u32;
    }
    for r in e.iter_mut() {
        r.2 = rank[r.2 as usize];
    }
    sz.x_rank += e.iter().map(|r| vbytes(r.2 as u64)).sum::<u64>();
    sz.x_entropy += uses.iter().filter(|&&u| u > 0).map(|&u| -(u as f64) * (u as f64 / m as f64).log2()).sum::<f64>();
    sz.x_fixed_frame += m as f64 * bx;
    sz.x_fixed_global += m as f64 * ceil_log2(n_x_global) as f64;
    // By target (today's run scheme, dense ids, no blocks).
    e.sort_unstable();
    let mut prev_t = 0u64;
    let mut i = 0;
    let mut n_targets = 0u64;
    let mut pairs = 0u64;
    while i < e.len() {
        let t = e[i].0;
        let mut j = i;
        while j < e.len() && e[j].0 == t {
            j += 1;
        }
        n_targets += 1;
        sz.by_target += vbytes(((t as u64 - prev_t) << 1) | 1) + vbytes(e[i].1 as u64);
        sz.attached += vbytes((j - i) as u64) + vbytes(e[i].1 as u64);
        let mut prev_s = e[i].1 as u64;
        let mut list_pairs = 1u64;
        for k in i + 1..j {
            let d = e[k].1 as u64 - prev_s;
            sz.by_target += vbytes(d << 1);
            sz.attached += vbytes(d);
            if d > 0 {
                list_pairs += 1;
            }
            prev_s = e[k].1 as u64;
        }
        pairs += list_pairs;
        sz.ef_lists_bits += ef_bits(list_pairs, (n_s as f64).log2().max(0.0));
        prev_t = t as u64;
        i = j;
    }
    sz.ef_lists_bits += ef_bits(n_targets, (n_t as f64).log2()) + ef_bits(n_targets, (pairs as f64).log2().max(0.0));
    sz.ef_pairs_bits += ef_bits(pairs, (n_t as f64).log2() + (n_s as f64).log2());
    // By source: per source (every one of the layer, implicit) its
    // out-degree, the first target zigzag from the previous source's first,
    // then deltas.
    e.sort_unstable_by_key(|r| (r.1, r.0, r.2));
    let mut i = 0;
    let mut prev_first = 0i64;
    let mut src = 0u32;
    while i < e.len() {
        let s = e[i].1;
        sz.by_source += (s - src) as u64; // the sources without edges: degree 0
        let mut j = i;
        while j < e.len() && e[j].1 == s {
            j += 1;
        }
        sz.by_source += vbytes((j - i) as u64) + vbytes(zigzag(e[i].0 as i64 - prev_first));
        prev_first = e[i].0 as i64;
        for k in i + 1..j {
            sz.by_source += vbytes((e[k].0 - e[k - 1].0) as u64);
        }
        src = s + 1;
        i = j;
    }
    sz.by_source += n_s - src as u64;
}

/// The transfer's information, summed over frames (bits).
#[derive(Default)]
struct XStats {
    edges: u64,
    /// Order-0 entropy of the pair, of its x part, of its y part.
    h_pair: f64,
    h_x: f64,
    h_y: f64,
    /// Conditional entropy given the source, given the target.
    h_given_src: f64,
    h_given_tgt: f64,
    /// A local table per source (its distinct transfers named at the
    /// frame's fixed width) plus a local index per edge; the same per target.
    local_src: f64,
    local_tgt: f64,
    distinct_src: u64,
    sources: u64,
    distinct_tgt: u64,
    targets: u64,
}

/// H of a list of counts summing to `m`.
fn entropy(counts: impl Iterator<Item = u64>, m: u64) -> f64 {
    counts.filter(|&c| c > 0).map(|c| -(c as f64) * (c as f64 / m as f64).log2()).sum()
}

fn xfer_study(e: &[(u32, u32, u32)], pairs: &[crate::search::arc_edges::Pair], st: &mut XStats) {
    let m = e.len() as u64;
    let n_x = pairs.len() as u64;
    st.edges += m;
    let mut c = vec![0u64; pairs.len()];
    for r in e {
        c[r.2 as usize] += 1;
    }
    st.h_pair += entropy(c.iter().copied(), m);
    for axis in 0..2 {
        let mut by: FxHashMap<crate::search::arc_edges::AxisXfer, u64> = FxHashMap::default();
        for (i, &k) in c.iter().enumerate() {
            let a = if axis == 0 { pairs[i].0 } else { pairs[i].1 };
            *by.entry(a).or_default() += k;
        }
        let h = entropy(by.values().copied(), m);
        if axis == 0 {
            st.h_x += h;
        } else {
            st.h_y += h;
        }
    }
    let bx = ceil_log2(n_x) as f64;
    for by_src in [true, false] {
        let mut k: Vec<u64> = e.iter().map(|r| ((if by_src { r.1 } else { r.0 }) as u64) << 32 | r.2 as u64).collect();
        k.sort_unstable();
        let (mut h, mut local, mut distinct, mut groups) = (0.0f64, 0.0f64, 0u64, 0u64);
        let mut i = 0;
        while i < k.len() {
            let g = k[i] >> 32;
            let mut j = i;
            let mut counts: Vec<u64> = Vec::new();
            while j < k.len() && k[j] >> 32 == g {
                let mut l = j;
                while l < k.len() && k[l] == k[j] {
                    l += 1;
                }
                counts.push((l - j) as u64);
                j = l;
            }
            let n = (j - i) as u64;
            h += entropy(counts.iter().copied(), n);
            local += counts.len() as f64 * bx + n as f64 * ceil_log2(counts.len() as u64) as f64;
            distinct += counts.len() as u64;
            groups += 1;
            i = j;
        }
        if by_src {
            st.h_given_src += h;
            st.local_src += local;
            st.distinct_src += distinct;
            st.sources += groups;
        } else {
            st.h_given_tgt += h;
            st.local_tgt += local;
            st.distinct_tgt += distinct;
            st.targets += groups;
        }
    }
}

pub fn run(dir: &Path, to: u32, threads: usize) -> Result<()> {
    let t0 = std::time::Instant::now();
    // Every layer's files, by (layer, seq): dense id A = base + row.
    let mut files: Vec<FrameFile> = Vec::new();
    let mut file_layer: Vec<u32> = Vec::new();
    let mut file_seq: Vec<u32> = Vec::new();
    let mut disk_bytes: Vec<u64> = Vec::new();
    for layer in 0..=to {
        let Ok(fs) = frame_files(dir, layer) else { continue };
        let mut fs = fs;
        fs.sort_by_key(|(s, _)| *s);
        for (seq, f) in fs {
            ensure!(!f.trimmed(), "a trimmed tree");
            files.push(f);
            file_layer.push(layer);
            file_seq.push(seq);
        }
    }
    for layer in 0..=to {
        let d = dir.join("frames").join(format!("f{:03}", layer));
        let mut b = 0u64;
        for e in std::fs::read_dir(&d).into_iter().flatten().flatten() {
            b += e.metadata().map(|m| m.len()).unwrap_or(0);
        }
        disk_bytes.push(b);
    }
    let mut base: Vec<u64> = Vec::with_capacity(files.len() + 1);
    let mut acc = 0u64;
    for f in &files {
        base.push(acc);
        acc += f.width() as u64;
    }
    base.push(acc);
    let n_total = acc;
    ensure!(n_total < u32::MAX as u64, "{n_total} states past a u32");
    let mut layer_n = vec![0u64; to as usize + 1];
    let mut layer_first = vec![u64::MAX; to as usize + 1];
    for (i, f) in files.iter().enumerate() {
        layer_n[file_layer[i] as usize] += f.width() as u64;
        layer_first[file_layer[i] as usize] = layer_first[file_layer[i] as usize].min(base[i]);
    }
    let file_of: FxHashMap<(u32, u32), usize> = (0..files.len()).map(|i| ((file_layer[i], file_seq[i]), i)).collect();
    let dense_a = |id: u64| -> u32 { (base[file_of[&(id_layer(id), id_seq(id))]] + id_row(id) as u64) as u32 };
    println!("== {} files, {} states, layers 0..={to}; opened in {:.1} s", files.len(), n_total, t0.elapsed().as_secs_f64());
    for l in 0..=to as usize {
        if layer_n[l] > 0 {
            print!("L{l}:{} ", layer_n[l]);
        }
    }
    println!();

    // ---- Today: frames on disk, the frontier as blocks.
    let disk_total: u64 = disk_bytes.iter().sum();
    let mut col_disk = 0u64;
    let mut ram_front = 0u64;
    let av = std::mem::size_of::<celeste_engine::runtime2::AV>() as u64;
    for (i, f) in files.iter().enumerate() {
        let mut row = 0u64;
        let mut ram = 0u64;
        for c in 0..f.n_cols() {
            let v = f.col_view(c);
            row += v.disk_bytes() as u64;
            ram += match v {
                ColView::U(_) => 0,
                ColView::N(_) => 4,
                ColView::I(_) => 8,
                ColView::V(_) | ColView::S(_) => av,
            };
        }
        col_disk += row * f.width() as u64;
        if file_layer[i] == to {
            // columns + key 16 + id 8 + cell 4
            ram_front += (ram + 28) * f.width() as u64;
        }
    }
    println!(
        "today: frames on disk {:.1} B/state ({:.2} GB; columns {:.1}, key 16); frontier L{to} {} rows as blocks {:.1} B/row (AV {av} B)",
        disk_total as f64 / n_total as f64,
        disk_total as f64 / 1e9,
        col_disk as f64 / n_total as f64,
        layer_n[to as usize],
        ram_front as f64 / layer_n[to as usize] as f64
    );

    // ---- States: shards.
    let mut shards: FxHashMap<(u64, u32), Runs> = FxHashMap::default();
    for (i, f) in files.iter().enumerate() {
        for &(cell, s, l) in f.runs() {
            shards.entry((f.shape_hash(), cell)).or_default().push((i as u32, s, l));
        }
    }
    let mut shard_list: Vec<((u64, u32), Runs)> = shards.into_iter().collect();
    shard_list.sort_by_key(|(k, _)| *k);
    let n_shards = shard_list.len();
    let mut by_size: Vec<usize> = (0..n_shards).collect();
    by_size.sort_by_key(|&i| std::cmp::Reverse(shard_list[i].1.iter().map(|r| r.2 as u64).sum::<u64>()));
    let next = AtomicUsize::new(0);
    let outs: Mutex<Vec<Option<ShardOut>>> = Mutex::new(vec![None; n_shards]);
    // Order B: per state (A) its (layer, shard, rank) key.
    let b_key: Vec<std::sync::atomic::AtomicU64> = (0..n_total).map(|_| std::sync::atomic::AtomicU64::new(0)).collect();
    let t1 = std::time::Instant::now();
    std::thread::scope(|scope| -> Result<()> {
        let hs: Vec<_> = (0..threads)
            .map(|_| {
                let (files, shard_list, by_size, next, outs, b_key, base, file_layer) = (&files, &shard_list, &by_size, &next, &outs, &b_key, &base, &file_layer);
                scope.spawn(move || -> Result<()> {
                    let mut ranks = Vec::new();
                    loop {
                        let k = next.fetch_add(1, Ordering::Relaxed);
                        let Some(&si) = by_size.get(k) else { return Ok(()) };
                        let (_, runs) = &shard_list[si];
                        let n_cols = files[runs[0].0 as usize].n_cols();
                        for r in runs {
                            ensure!(files[r.0 as usize].n_cols() == n_cols, "a shape's files disagree on columns");
                        }
                        let o = study_shard(files, file_layer, runs, n_cols, &mut ranks)?;
                        // Map shard rows to A ids.
                        let mut row_a: Vec<u64> = Vec::with_capacity(o.rows as usize);
                        for &(f, s, l) in runs {
                            for r in s..s + l {
                                row_a.push(base[f as usize] + r as u64);
                            }
                        }
                        for &(r, rank, layer) in &ranks {
                            b_key[row_a[r as usize] as usize].store((layer as u64) << 56 | (si as u64) << 32 | rank as u64, Ordering::Relaxed);
                        }
                        outs.lock().unwrap()[si] = Some(o);
                    }
                })
            })
            .collect();
        for h in hs {
            h.join().expect("shard worker")?;
        }
        Ok(())
    })?;
    let outs: Vec<ShardOut> = outs.into_inner().unwrap().into_iter().map(|o| o.expect("every shard studied")).collect();
    println!("== states: {n_shards} shards studied in {:.1} s", t1.elapsed().as_secs_f64());
    let tot = |f: &dyn Fn(&ShardOut) -> f64| outs.iter().map(f).sum::<f64>();
    let n = n_total as f64;
    let rows_bits = tot(&|o| o.rows as f64 * o.bits as f64);
    let rows_bits_dict = tot(&|o| o.rows as f64 * o.bits_dict as f64);
    let rows_bits_range = tot(&|o| o.rows as f64 * o.bits_range as f64);
    let rows_radix = tot(&|o| o.rows as f64 * o.radix);
    let dups = tot(&|o| o.dups as f64);
    let delta = tot(&|o| o.delta_bytes as f64);
    let ef = tot(&|o| o.ef);
    let ef_radix = tot(&|o| o.ef_radix);
    let ef_layers = tot(&|o| o.ef_layers);
    let dict = tot(&|o| o.dict_entries as f64);
    let byte_keys = tot(&|o| o.rows as f64 * (o.bits_dict as f64 / 8.0).ceil());
    let u64_keys = tot(&|o| o.rows as f64 * 8.0 * (o.bits_dict as f64 / 64.0).ceil().max(1.0));
    let door_today = n * 24.0 + tot(&|o| o.door_index_words as f64 * 4.0);
    println!("packed state, bits/state: min(dict, range) {:.1}; dict {:.1}; range {:.1}; mixed radix {:.1}; duplicate packed states {dups}", rows_bits / n, rows_bits_dict / n, rows_bits_range / n, rows_radix / n);
    println!(
        "sorted per shard: varint deltas {:.2} B/state; Elias-Fano {:.2} B/state (radix universe {:.2}); one EF run per (shard, layer) {:.2} B/state; dictionaries {:.0} entries ({:.3} B/state at 8 B)",
        delta / n,
        ef / 8.0 / n,
        ef_radix / 8.0 / n,
        ef_layers / 8.0 / n,
        dict,
        dict * 8.0 / n
    );
    // Key width distribution, by rows.
    let mut width_hist: std::collections::BTreeMap<u64, u64> = Default::default();
    for o in &outs {
        *width_hist.entry(o.bits_dict.div_ceil(16) * 16).or_default() += o.rows;
    }
    println!(
        "packed key width (dict), share of states by bits rounded up to 16: {}",
        width_hist.iter().map(|(b, r)| format!("<={b}:{:.1}%", 100.0 * *r as f64 / n)).collect::<Vec<_>>().join(" ")
    );
    let max_bits = outs.iter().map(|o| o.bits_dict).max().unwrap_or(0);
    println!(
        "door per entry: today {:.2} B (24 B + index); packed key bytes {:.2} B (+4 B id = {:.2}); in u64 words {:.2} B (+4 = {:.2}); EF no id {:.2} B; EF per (shard, layer) no id {:.2} B; widest key {max_bits} bits",
        door_today / n,
        byte_keys / n,
        byte_keys / n + 4.0,
        u64_keys / n,
        u64_keys / n + 4.0,
        ef / 8.0 / n,
        ef_layers / 8.0 / n
    );
    // Shard sizes.
    let mut sh = Hist::default();
    for o in &outs {
        sh.add(o.rows);
    }
    println!("shard sizes (rows): {}", sh.show());
    // Column cardinalities per shape: per column the max over shards, the
    // row-weighted bits.
    let mut per_shape: std::collections::BTreeMap<u64, FxHashMap<u32, (u64, f64, u64)>> = Default::default();
    let mut shape_rows: FxHashMap<u64, u64> = FxHashMap::default();
    for (o, ((shape, _), _)) in outs.iter().zip(&shard_list) {
        *shape_rows.entry(*shape).or_default() += o.rows;
        let m = per_shape.entry(*shape).or_default();
        for &(c, card, b) in &o.cols {
            let e = m.entry(c).or_default();
            e.0 = e.0.max(card);
            e.1 += b as f64 * o.rows as f64;
            e.2 += o.rows;
        }
    }
    let names: FxHashMap<u64, std::collections::HashMap<usize, String>> = per_shape
        .keys()
        .filter_map(|&shape| {
            let f = files.iter().find(|f| f.shape_hash() == shape && f.width() > 0)?;
            let rt2 = f.load_rows(&[0..1]).ok()??;
            Some((shape, crate::search::inspect::cell_names(&rt2, crate::compiled::ids())))
        })
        .collect();
    for (shape, m) in &per_shape {
        let rows = shape_rows[shape] as f64;
        let mut v: Vec<(&u32, &(u64, f64, u64))> = m.iter().collect();
        v.sort_by(|a, b| (b.1 .1).total_cmp(&a.1 .1));
        println!("shape {shape:#018x}: {} states, {} varying columns; bits/state by column (max card over shards):", rows, v.len());
        let line: Vec<String> = v
            .iter()
            .map(|(c, (card, bits, _))| {
                let nm = names.get(shape).and_then(|n| n.get(&(**c as usize))).cloned().unwrap_or_else(|| format!("c{c}"));
                format!("{nm}={:.2}b({card})", bits / rows)
            })
            .collect();
        for chunk in line.chunks(6) {
            println!("    {}", chunk.join("  "));
        }
    }

    // ---- Order B: dense ids layer-major, (shard, packed rank) within.
    let mut perm: Vec<(u64, u32)> = (0..n_total as u32).map(|a| (b_key[a as usize].load(Ordering::Relaxed), a)).collect();
    drop(b_key);
    perm.sort_unstable();
    let mut a_to_b = vec![0u32; n_total as usize];
    for (b, &(_, a)) in perm.iter().enumerate() {
        a_to_b[a as usize] = b as u32;
    }
    drop(perm);
    // Order B keeps layers contiguous at the same offsets as A.

    // ---- Edges.
    let g = crate::search::edges::EdgeGraph::open(&dir.join("edges"), to)?;
    let mut global_x: rustc_hash::FxHashSet<crate::search::arc_edges::Pair> = Default::default();
    for f in 0..=to {
        global_x.extend(g.pairs(f).iter().copied());
    }
    let n_xg = global_x.len() as u64;
    let mut run_bytes = vec![0u64; to as usize + 1];
    for e in std::fs::read_dir(dir.join("edges")).into_iter().flatten().flatten() {
        let p = e.path();
        if !p.is_dir() || !p.file_name().and_then(|s| s.to_str()).is_some_and(|s| s.starts_with('l')) {
            continue;
        }
        for r in std::fs::read_dir(&p).into_iter().flatten().flatten() {
            let n = r.file_name();
            let Some(f) = n.to_str().and_then(|s| s.strip_prefix('f')).and_then(|s| s.split('.').next()).and_then(|s| s.parse::<usize>().ok()) else { continue };
            if f <= to as usize {
                run_bytes[f] += r.metadata().map(|m| m.len()).unwrap_or(0);
            }
        }
    }
    println!("== edges: {} records, {:.2} GB of runs; {n_xg} distinct transfer pairs over all frames", g.records, g.bytes as f64 / 1e9);
    println!("frame | sources | edges | out/src | targets | %old | xfers | run B/e | A: tgt B/e src B/e attach B/e | B: tgt src attach | fixed b/e | EF pairs b/e | x H b/e");
    let mut sz_a = EdgeSizes::default();
    let mut sz_b = EdgeSizes::default();
    let mut out_deg = Hist::default();
    let mut in_deg_frame = Hist::default();
    let mut in_deg_total = vec![0u32; n_total as usize];
    let mut layer_diff: std::collections::BTreeMap<u32, u64> = Default::default();
    let mut total_edges = 0u64;
    let mut xs = XStats::default();
    let mut timing: Option<u64> = None;
    for frame in 1..=to {
        let n_s = layer_n[frame as usize - 1];
        if n_s == 0 {
            continue;
        }
        let s_first = layer_first[frame as usize - 1];
        let mut e: Vec<(u32, u32, u32)> = Vec::new();
        let mut bad = 0u64;
        g.scan(frame, |ed| {
            debug_assert_eq!(ed.mask, 1);
            if id_layer(ed.base) != frame - 1 {
                bad += 1;
            }
            e.push((dense_a(ed.target), (dense_a(ed.base) as u64 - s_first) as u32, ed.xfer));
        });
        ensure!(bad == 0, "frame {frame}: {bad} edges whose source is not in layer {}", frame - 1);
        if e.is_empty() {
            continue;
        }
        let m = e.len() as u64;
        total_edges += m;
        let n_t: u64 = layer_n[..=frame as usize].iter().sum();
        let n_x = g.pairs(frame).len() as u64;
        // Degrees, layer differences.
        let mut deg = vec![0u32; n_s as usize];
        let mut old = 0u64;
        let mut tdeg: FxHashMap<u32, u32> = FxHashMap::default();
        for &(t, s, _) in &e {
            deg[s as usize] += 1;
            in_deg_total[t as usize] += 1;
            *tdeg.entry(t).or_default() += 1;
            let tl = file_layer[base.partition_point(|&b| b <= t as u64) - 1];
            if tl < frame {
                old += 1;
            }
            *layer_diff.entry(frame - tl).or_default() += 1;
        }
        for &d in &deg {
            out_deg.add(d as u64);
        }
        for &d in tdeg.values() {
            in_deg_frame.add(d as u64);
        }
        let n_targets = tdeg.len();
        drop(tdeg);
        let mut xf = XStats::default();
        xfer_study(&e, g.pairs(frame), &mut xf);
        println!(
            "      xfer f{frame}: H pair {:.2} x {:.2} y {:.2} | H given source {:.2} given target {:.2} | local table per source {:.2} per target {:.2} b/e | distinct per source {:.1} per target {:.1}",
            xf.h_pair / m as f64, xf.h_x / m as f64, xf.h_y / m as f64, xf.h_given_src / m as f64, xf.h_given_tgt / m as f64, xf.local_src / m as f64, xf.local_tgt / m as f64,
            xf.distinct_src as f64 / xf.sources as f64, xf.distinct_tgt as f64 / xf.targets as f64
        );
        xs.add(&xf);
        let mut a = EdgeSizes::default();
        let mut eb: Vec<(u32, u32, u32)> = e.iter().map(|&(t, s, x)| (a_to_b[t as usize], a_to_b[(s as u64 + s_first) as usize] - s_first as u32, x)).collect();
        let tt = std::time::Instant::now();
        size_edges(&mut e, n_t, n_s, n_xg, &mut a, n_x);
        let t_size = tt.elapsed().as_secs_f64();
        let mut b = EdgeSizes::default();
        size_edges(&mut eb, n_t, n_s, n_xg, &mut b, n_x);
        drop(eb);
        let timed = frame == to || timing.is_none_or(|t| t < m);
        if timed {
            // Encode + decode by target (e is sorted by source now).
            e.sort_unstable();
            let te = std::time::Instant::now();
            let mut buf: Vec<u8> = Vec::with_capacity(e.len() * 4);
            let (mut pt, mut ps) = (0u32, 0u32);
            for &(t, s, x) in &e {
                if t != pt || buf.is_empty() {
                    put(&mut buf, ((t - pt) as u64) << 1 | 1);
                    put(&mut buf, s as u64);
                } else {
                    put(&mut buf, ((s - ps) as u64) << 1);
                }
                put(&mut buf, x as u64);
                pt = t;
                ps = s;
            }
            let enc = te.elapsed().as_secs_f64();
            let td = std::time::Instant::now();
            let (mut pos, mut t, mut s, mut chk) = (0usize, 0u64, 0u64, 0u64);
            while pos < buf.len() {
                let h = get(&buf, &mut pos);
                if h & 1 == 1 {
                    t += h >> 1;
                    s = get(&buf, &mut pos);
                } else {
                    s += h >> 1;
                }
                let x = get(&buf, &mut pos);
                chk = chk.wrapping_add(t ^ s ^ x);
            }
            let dec = td.elapsed().as_secs_f64();
            let want = e.iter().fold(0u64, |c, &(t, s, x)| c.wrapping_add(t as u64 ^ s as u64 ^ x as u64));
            ensure!(chk == want, "the varint round trip disagrees");
            {
                println!(
                    "  [speed f{frame}] varint by target, 1 thread: encode {:.1} ns/edge, decode {:.1} ns/edge ({:.2} B/e); sizing all formats {:.1} ns/edge",
                    enc * 1e9 / m as f64,
                    dec * 1e9 / m as f64,
                    buf.len() as f64 / m as f64,
                    t_size * 1e9 / m as f64
                );
            }
            timing = Some(timing.map_or(m, |t| t.max(m)));
        }
        let per = |x: f64| x / m as f64;
        println!(
            "{frame:>5} | {n_s:>8} | {m:>9} | {:>5.2} | {:>8} | {:>4.1} | {n_x:>5} | {:>5.2} | {:>5.2} {:>5.2} {:>5.2} | {:>5.2} {:>5.2} {:>5.2} | {:>5.1} | {:>5.1} | {:>4.2}",
            m as f64 / n_s as f64,
            n_targets,
            100.0 * old as f64 / m as f64,
            per(run_bytes[frame as usize] as f64),
            per((a.by_target + a.x_rank) as f64),
            per((a.by_source + a.x_rank) as f64),
            per((a.attached + a.x_rank) as f64),
            per((b.by_target + b.x_rank) as f64),
            per((b.by_source + b.x_rank) as f64),
            per((b.attached + b.x_rank) as f64),
            per(a.fixed_frame_bits),
            per(a.ef_pairs_bits),
            per(a.x_entropy),
        );
        for (acc, x) in [(&mut sz_a, &a), (&mut sz_b, &b)] {
            acc.fixed_frame_bits += x.fixed_frame_bits;
            acc.by_target += x.by_target;
            acc.attached += x.attached;
            acc.by_source += x.by_source;
            acc.ef_pairs_bits += x.ef_pairs_bits;
            acc.ef_lists_bits += x.ef_lists_bits;
            acc.x_rank += x.x_rank;
            acc.x_entropy += x.x_entropy;
            acc.x_fixed_frame += x.x_fixed_frame;
            acc.x_fixed_global += x.x_fixed_global;
        }
    }
    let m = total_edges as f64;
    sz_a.fixed_global_bits = m * (2.0 * ceil_log2(n_total) as f64 + ceil_log2(n_xg) as f64);
    println!("== edges total {total_edges} ({:.1} per state); runs on disk {:.2} B/edge; raw records 16 B/edge", m / n, g.bytes as f64 / m);
    for (name, s) in [("A (pack_id order)", &sz_a), ("B (layer, shard, packed rank)", &sz_b)] {
        println!(
            "{name}: B/edge incl. transfer rank varint: by target {:.2}, attached to target {:.2}, by source {:.2}; ids only: by target {:.2}, attached {:.2}, by source {:.2}; EF pairs {:.2}, EF CSR lists {:.2} (ids only)",
            (s.by_target + s.x_rank) as f64 / m,
            (s.attached + s.x_rank) as f64 / m,
            (s.by_source + s.x_rank) as f64 / m,
            s.by_target as f64 / m,
            s.attached as f64 / m,
            s.by_source as f64 / m,
            s.ef_pairs_bits / 8.0 / m,
            s.ef_lists_bits / 8.0 / m
        );
    }
    println!(
        "fixed width: per-frame widths {:.2} B/edge, global widths (2 x {} + {} bits) {:.2} B/edge",
        sz_a.fixed_frame_bits / 8.0 / m,
        ceil_log2(n_total),
        ceil_log2(n_xg),
        sz_a.fixed_global_bits / 8.0 / m
    );
    println!(
        "transfer per edge: rank varint {:.3} B, order-0 entropy {:.3} bits, fixed per-frame {:.2} bits, global {:.2} bits",
        sz_a.x_rank as f64 / m,
        sz_a.x_entropy / m,
        sz_a.x_fixed_frame / m,
        sz_a.x_fixed_global / m
    );
    let me = xs.edges as f64;
    println!(
        "transfer bits/edge over all frames: H pair {:.2} (x {:.2} + y {:.2}); H given source {:.2}, given target {:.2}; local table per source {:.2}, per target {:.2}; distinct per source {:.2}, per target {:.2}",
        xs.h_pair / me, xs.h_x / me, xs.h_y / me, xs.h_given_src / me, xs.h_given_tgt / me, xs.local_src / me, xs.local_tgt / me,
        xs.distinct_src as f64 / xs.sources as f64, xs.distinct_tgt as f64 / xs.targets as f64
    );
    println!("out-degree (per source, each frame): {}", out_deg.show());
    println!("in-degree within a frame (per touched target): {}", in_deg_frame.show());
    let mut it = Hist::default();
    for &d in &in_deg_total {
        it.add(d as u64);
    }
    println!("in-degree over all frames (per state): {}", it.show());
    println!(
        "frame - target layer (0: a new state): {}",
        layer_diff.iter().map(|(d, c)| format!("{d}:{:.2}%", 100.0 * *c as f64 / m)).collect::<Vec<_>>().join(" ")
    );
    println!("arc Graph in RAM if over every node: {:.1} B/state + {:.1} B/edge = {:.1} B/state", 33.0, 12.0, 33.0 + 12.0 * m / n);
    println!("done in {:.1} s", t0.elapsed().as_secs_f64());
    Ok(())
}

#[inline]
fn put(out: &mut Vec<u8>, mut v: u64) {
    while v >= 0x80 {
        out.push((v as u8) | 0x80);
        v >>= 7;
    }
    out.push(v as u8);
}

#[inline]
fn get(b: &[u8], pos: &mut usize) -> u64 {
    let mut v = 0u64;
    let mut shift = 0;
    loop {
        let x = b[*pos];
        *pos += 1;
        v |= ((x & 0x7f) as u64) << shift;
        if x & 0x80 == 0 {
            return v;
        }
        shift += 7;
    }
}
