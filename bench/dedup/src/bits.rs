//! The bit-packed state + bitset visited set (Philippe's 2022 hard-coded
//! version, `67e341d`: a dense `PosMap` by position, the player's flags
//! mixed-radix compressed with the dash fields as one joint "valid combos"
//! digit), applied to the capture's ABSTRACT states.
//!
//! Inputs: `rewrite field-dump` of the capture's tree (DIR/fields): every
//! stored row's key, shape, cell, frame and, per varying value cell, its index
//! into the cell's sorted dictionary of `av_code`s (what the row key hashes).

use rustc_hash::{FxHashMap, FxHashSet};

pub struct Field { pub name: String, pub codes: Vec<u64>, pub shows: Vec<String> }

pub struct Fields {
    pub rows: memmap2::Mmap,
    pub rec: usize,
    pub k: usize,
    pub n: usize,
    /// Per shape: its varying fields, in the record's order.
    pub shapes: Vec<Vec<Field>>,
}

impl Fields {
    pub fn open(dir: &str) -> Fields {
        let fd = format!("{dir}/fields");
        let layout = std::fs::read_to_string(format!("{fd}/layout.txt")).unwrap();
        let get = |k: &str| -> usize { layout.lines().find_map(|l| l.strip_prefix(k)).unwrap().trim().parse().unwrap() };
        let (k, rec, n) = (get("kmax "), get("record "), get("rows "));
        let census = std::fs::read_to_string(format!("{fd}/census.txt")).unwrap();
        let n_shapes = census.lines().filter(|l| l.starts_with("shape ")).count();
        let mut shapes: Vec<Vec<Field>> = (0..n_shapes).map(|_| Vec::new()).collect();
        let dicts = std::fs::read_to_string(format!("{fd}/dicts.txt")).unwrap();
        let mut cur: Option<usize> = None;
        for l in dicts.lines() {
            if let Some(rest) = l.strip_prefix("  ") {
                let t: Vec<&str> = rest.split_whitespace().collect();
                let f = shapes[cur.unwrap()].last_mut().unwrap();
                f.codes.push(u64::from_str_radix(t[0].trim_start_matches("0x"), 16).unwrap());
                f.shows.push(t[1..t.len() - 1].join(" "));
            } else {
                let t: Vec<&str> = l.split_whitespace().collect();
                let si: usize = t[0].parse().unwrap();
                cur = Some(si);
                shapes[si].push(Field { name: t[3].to_string(), codes: Vec::new(), shows: Vec::new() });
            }
        }
        let rows = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{fd}/rows.bin")).unwrap()).unwrap() };
        assert_eq!(rows.len(), rec * n);
        Fields { rows, rec, k, n, shapes }
    }
    #[inline]
    pub fn key(&self, i: usize) -> u128 {
        let b = &self.rows[i * self.rec..];
        (u64::from_le_bytes(b[0..8].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[8..16].try_into().unwrap()) as u128) << 64)
    }
    #[inline]
    pub fn hdr(&self, i: usize) -> (usize, u32, u32) {
        let b = &self.rows[i * self.rec + 16..];
        let u = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
        (u(0) as usize, u(4), u(8))
    }
    #[inline]
    pub fn val(&self, i: usize, j: usize) -> u32 {
        let o = i * self.rec + 28 + 4 * j;
        u32::from_le_bytes(self.rows[o..o + 4].try_into().unwrap())
    }
    pub fn find(&self, s: usize, name: &str) -> Option<usize> { self.shapes[s].iter().position(|f| f.name == name || f.name.ends_with(&format!("].{name}"))) }
}

fn quant(mut v: Vec<u64>) -> String {
    if v.is_empty() { return "-".into(); }
    v.sort_unstable();
    let q = |p: f64| v[((v.len() - 1) as f64 * p) as usize];
    format!("n {} min {} p50 {} p90 {} p99 {} max {} mean {:.1}", v.len(), v[0], q(0.5), q(0.9), q(0.99), v[v.len() - 1], v.iter().sum::<u64>() as f64 / v.len() as f64)
}

/// The census that the packing is designed from.
pub fn census(dir: &str) {
    let f = Fields::open(dir);
    eprintln!("[census] {} rows, record {} B, {} varying cells at most", f.n, f.rec, f.k);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    for (s, fs) in f.shapes.iter().enumerate() {
        let rows = &by_shape[s];
        println!("shape {s}: {} rows; varying: {}", rows.len(), fs.iter().map(|x| format!("{}({})", x.name, x.codes.len())).collect::<Vec<_>>().join(" "));
        if rows.len() < 1000 { continue; }
        let (xi, yi) = (f.find(s, "player.x").unwrap(), f.find(s, "player.y").unwrap());
        let sx = f.find(s, "spd.x"); let sy = f.find(s, "spd.y");
        // cell <-> (x, y)
        let mut c2xy: FxHashMap<u32, (u32, u32)> = Default::default();
        let mut xy2c: FxHashMap<(u32, u32), u32> = Default::default();
        let mut bad = 0u64;
        for &i in rows {
            let c = f.hdr(i).1; let xy = (f.val(i, xi), f.val(i, yi));
            if *c2xy.entry(c).or_insert(xy) != xy || *xy2c.entry(xy).or_insert(c) != c { bad += 1; }
        }
        let xs = &fs[xi].shows; let ys = &fs[yi].shows;
        println!("  positions: {} cells, cell<->(x,y) violations {bad}; x {}..{} ({} values), y {}..{} ({}): bbox {}",
            c2xy.len(), xs[0], xs[xs.len() - 1], xs.len(), ys[0], ys[ys.len() - 1], ys.len(), xs.len() * ys.len());
        let flags: Vec<usize> = (0..fs.len()).filter(|&j| j != xi && j != yi && Some(j) != sx && Some(j) != sy).collect();
        let radix = |js: &[usize]| js.iter().map(|&j| fs[j].codes.len() as u64).product::<u64>();
        let tuple = |i: usize, js: &[usize]| js.iter().fold(0u64, |a, &j| a * fs[j].codes.len() as u64 + f.val(i, j) as u64);
        let spd: Vec<usize> = [sx, sy].into_iter().flatten().collect();
        let distinct = |js: &[usize]| { let mut h: FxHashSet<u64> = Default::default(); for &i in rows { h.insert(tuple(i, js)); } h.len() };
        println!("  flags {:?}: product {} distinct {}", flags.iter().map(|&j| &fs[j].name).collect::<Vec<_>>(), radix(&flags), distinct(&flags));
        println!("  spd: product {} distinct {}", radix(&spd), distinct(&spd));
        // Candidate joint groups.
        let named = |ns: &[&str]| -> Vec<usize> { ns.iter().filter_map(|n| f.find(s, n)).collect() };
        for (label, g) in [
            ("dash (time, effect, target, accel)", named(&["dash_time", "dash_effect_time", "dash_target.x", "dash_target.y", "dash_accel.x", "dash_accel.y"])),
            ("dash + has_dashed", named(&["dash_time", "dash_effect_time", "dash_target.x", "dash_target.y", "dash_accel.x", "dash_accel.y", "has_dashed"])),
            ("dash + has_dashed + djump", named(&["dash_time", "dash_effect_time", "dash_target.x", "dash_target.y", "dash_accel.x", "dash_accel.y", "has_dashed", "djump"])),
            ("freeze, grace, flip", named(&["freeze", "grace", "flip.x"])),
            ("spd.x", named(&["spd.x"])),
        ] { println!("  group {label}: product {} distinct {}", radix(&g), distinct(&g)); }
        let all: Vec<usize> = flags.iter().chain(spd.iter()).copied().collect();
        println!("  flags x spd: product {} distinct {}", radix(&all), distinct(&all));
        // Per shard (cell) occupancy and the sub-structure.
        let mut per_cell: FxHashMap<u32, u64> = Default::default();
        let mut cf: FxHashMap<(u32, u64), u64> = Default::default();
        let mut cs: FxHashMap<(u32, u64), u64> = Default::default();
        for &i in rows {
            let c = f.hdr(i).1;
            *per_cell.entry(c).or_default() += 1;
            *cf.entry((c, tuple(i, &flags))).or_default() += 1;
            *cs.entry((c, tuple(i, &spd))).or_default() += 1;
        }
        println!("  states per cell: {}", quant(per_cell.values().copied().collect()));
        println!("  (cell, flags) prefixes {}: spd per prefix {}", cf.len(), quant(cf.values().copied().collect()));
        println!("  (cell, spd) prefixes {}: flags per prefix {}", cs.len(), quant(cs.values().copied().collect()));
    }
}

/// The packing's digits for one shape: each a field, or the dash fields as
/// ONE joint digit (the old code's `VALID_DASH_COMBOS`: a lookup table from
/// the fields' product to the combo's index).
pub struct Digit { pub name: String, pub radix: u64, pub fields: Vec<usize>, pub table: Option<Vec<u32>> }

pub fn digits(f: &Fields, s: usize, rows: &[usize]) -> Vec<Digit> {
    let fs = &f.shapes[s];
    let dash: Vec<usize> = ["dash_time", "dash_effect_time", "dash_target.x", "dash_target.y", "dash_accel.x", "dash_accel.y", "has_dashed"].iter().filter_map(|n| f.find(s, n)).collect();
    let mut out = Vec::new();
    for (j, fd) in fs.iter().enumerate() {
        if dash.contains(&j) || fd.name == "player.x" || fd.name == "player.y" { continue; }
        out.push(Digit { name: fd.name.rsplit("].").next().unwrap().to_string(), radix: fd.codes.len() as u64, fields: vec![j], table: None });
    }
    if !dash.is_empty() {
        let prod: u64 = dash.iter().map(|&j| fs[j].codes.len() as u64).product();
        let mut table = vec![u32::MAX; prod as usize];
        let mut n = 0u32;
        for &i in rows {
            let t = dash.iter().fold(0u64, |a, &j| a * fs[j].codes.len() as u64 + f.val(i, j) as u64) as usize;
            if table[t] == u32::MAX { table[t] = n; n += 1; }
        }
        out.push(Digit { name: format!("dash combo ({} fields)", dash.len()), radix: n as u64, fields: dash, table: Some(table) });
    }
    out
}

impl Digit {
    #[inline]
    pub fn of(&self, f: &Fields, i: usize) -> u64 {
        match &self.table {
            None => f.val(i, self.fields[0]) as u64,
            Some(t) => {
                let fs = &f.shapes[f.hdr(i).0];
                t[self.fields.iter().fold(0u64, |a, &j| a * fs[j].codes.len() as u64 + f.val(i, j) as u64) as usize] as u64
            }
        }
    }
}

/// For shape 2 (95.5% of the rows): every split of the digits into a LOW part
/// (a dense bitset per prefix) and a HIGH part (cell + the other digits, a
/// directory of prefixes): fill and memory.
pub fn splits(dir: &str) {
    let f = Fields::open(dir);
    let s = 2;
    let rows: Vec<usize> = (0..f.n).filter(|&i| f.hdr(i).0 == s).collect();
    let ds = digits(&f, s, &rows);
    println!("shape {s}: {} rows; digits: {}", rows.len(), ds.iter().map(|d| format!("{}({})", d.name, d.radix)).collect::<Vec<_>>().join(" "));
    let vals: Vec<Vec<u16>> = ds.iter().map(|d| rows.iter().map(|&i| d.of(&f, i) as u16).collect()).collect();
    let cells: Vec<u32> = rows.iter().map(|&i| f.hdr(i).1).collect();
    let nd = ds.len();
    let mut res = Vec::new();
    for mask in 1u32..(1 << nd) {
        if mask.count_ones() > 3 { continue; }
        let low: Vec<usize> = (0..nd).filter(|&d| mask >> d & 1 == 1).collect();
        let high: Vec<usize> = (0..nd).filter(|&d| mask >> d & 1 == 0).collect();
        let lr: u64 = low.iter().map(|&d| ds[d].radix).product();
        if lr > 1 << 20 { continue; }
        let mut p: FxHashSet<u64> = Default::default();
        for r in 0..rows.len() {
            let h = high.iter().fold(cells[r] as u64, |a, &d| a * ds[d].radix + vals[d][r] as u64);
            p.insert(h);
        }
        let words = lr.div_ceil(64);
        let fill = rows.len() as f64 / (p.len() as f64 * lr as f64);
        res.push((p.len() as u64 * words * 8, format!("low {:<40} radix {:>7} ({:>4} words): {:>9} prefixes, {:>6.1} states/prefix, fill {:>6.2}%, bitsets {:>8.0} MB",
            low.iter().map(|&d| ds[d].name.clone()).collect::<Vec<_>>().join(" x "), lr, words, p.len(), rows.len() as f64 / p.len() as f64, 100.0 * fill, (p.len() as u64 * words * 8) as f64 / 1e6)));
    }
    res.sort();
    for (_, l) in res { println!("{l}"); }
}

// ---------------------------------------------------------------------------
// The packing, the prepared inputs, and the timed bitset variants.

/// Per shape: the LOW digit (a 128-bit bitset per directory entry: `spd.y`,
/// <= 96 values here) and the HIGH digits (the rest but the position, which
/// the shard (shape, cell) already is), mixed radix as in `67e341d`.
pub struct Packing { pub digits: Vec<Digit>, pub low: Option<usize>, pub high: Vec<usize>, pub high_radix: u64 }

pub fn packing(f: &Fields, s: usize, rows: &[usize]) -> Packing {
    let digits = digits(f, s, rows);
    let low = digits.iter().position(|d| d.name.ends_with("spd.y") && d.radix <= 128);
    let high: Vec<usize> = (0..digits.len()).filter(|&d| Some(d) != low).collect();
    let high_radix: u64 = high.iter().map(|&d| digits[d].radix).product();
    assert!(high_radix < u32::MAX as u64, "shape {s}: high digits {high_radix} do not fit 32 bits");
    Packing { digits, low, high, high_radix }
}

impl Packing {
    #[inline]
    pub fn pack(&self, f: &Fields, i: usize) -> (u32, u32) {
        let h = self.high.iter().fold(0u64, |a, &d| a * self.digits[d].radix + self.digits[d].of(f, i));
        (h as u32, self.low.map_or(0, |d| self.digits[d].of(f, i) as u32))
    }
}

/// A prepared query: as `Q`, the 16-B key replaced by the packed state (32 B, so
/// the stream is the same size as v3c's).
#[repr(C)]
#[derive(Clone, Copy)]
pub struct QB { pub shard: u32, pub src: u32, pub xfer: u32, pub low: u32, pub high: u32, _p: [u32; 3] }

fn bits_of(x: u64) -> f64 { (x as f64).log2() }

/// `prepbits`: the packing from the census, the injectivity check, and
/// DIR/prep/{qb.bin, db.bin, pcnt.bin, refid.bin} aligned with q.bin / door.bin.
pub fn prep_bits(dir: &str, door: &[u8]) {
    let t = std::time::Instant::now();
    let f = Fields::open(dir);
    let census = std::fs::read_to_string(format!("{dir}/fields/census.txt")).unwrap();
    let shape_hash: Vec<u64> = census.lines().filter(|l| l.starts_with("shape ")).map(|l| u64::from_str_radix(l.split_whitespace().nth(2).unwrap().trim_end_matches(':').trim_start_matches("0x"), 16).unwrap()).collect();
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<Packing> = (0..f.shapes.len()).map(|s| packing(&f, s, &by_shape[s])).collect();
    // The bit budget.
    for (s, p) in packs.iter().enumerate() {
        let rows = &by_shape[s];
        let cells: FxHashSet<u32> = rows.iter().map(|&i| f.hdr(i).1).collect();
        println!("shape {s} ({:#x}): {} rows, {} cells", shape_hash[s], rows.len(), cells.len());
        for (d, dg) in p.digits.iter().enumerate() {
            let role = if Some(d) == p.low { "LOW " } else { "high" };
            let fields: Vec<String> = dg.fields.iter().map(|&j| format!("{}({})", f.shapes[s][j].name, f.shapes[s][j].codes.len())).collect();
            let prod: u64 = dg.fields.iter().map(|&j| f.shapes[s][j].codes.len() as u64).product();
            println!("  {role} {:<28} radix {:>5} = {:>5.2} bits{}", dg.name, dg.radix, bits_of(dg.radix),
                if dg.fields.len() > 1 { format!(" (joint over {}: product {prod})", fields.join(" ")) } else { String::new() });
        }
        let low_r = p.low.map_or(1, |d| p.digits[d].radix);
        println!("  high {} = {:.2} bits (u32), low {} = {:.2} bits; within a shard {:.2} bits; with the cell ({} occupied) {:.2} bits",
            p.high_radix, bits_of(p.high_radix), low_r, bits_of(low_r), bits_of(p.high_radix * low_r), cells.len(), bits_of(p.high_radix * low_r) + bits_of(cells.len() as u64));
    }
    // key -> row; the packed states distinct.
    let mut row_of: FxHashMap<u128, u32> = FxHashMap::default();
    row_of.reserve(f.n);
    let mut packed: FxHashSet<(usize, u32, u32, u32)> = FxHashSet::default();
    packed.reserve(f.n);
    let mut pk: Vec<(u32, u32)> = Vec::with_capacity(f.n);
    for i in 0..f.n {
        let (s, c, _) = f.hdr(i);
        let (h, l) = packs[s].pack(&f, i);
        pk.push((h, l));
        assert!(row_of.insert(f.key(i), i as u32).is_none(), "row {i}: a key twice in the tree");
        assert!(packed.insert((s, c, h, l)), "row {i}: TWO KEYS PACK TO ONE STATE");
    }
    drop(packed);
    eprintln!("[prepbits] {} rows: {} distinct keys, {} distinct packed states (injective); {:.1} s", f.n, row_of.len(), f.n, t.elapsed().as_secs_f64());
    // The door, in door.bin order.
    let n_door = door.len() / 40;
    let mut db: Vec<(u32, u32)> = Vec::with_capacity(n_door);
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        let r = row_of[&k] as usize;
        let (s, c, fr) = f.hdr(r);
        assert!(shape_hash[s] == u64::from_le_bytes(b[0..8].try_into().unwrap()) && c == u32::from_le_bytes(b[8..12].try_into().unwrap()) && fr <= 56, "door entry {i}: not its tree row");
        db.push(pk[r]);
    }
    // The queries, in q.bin's sweep order; the reference ids (v3c's: door index, then new in sweep order).
    let pd = format!("{dir}/prep");
    let qm = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/q.bin")).unwrap()).unwrap() };
    let qs: &[crate::Q] = crate::from_bytes(&qm);
    let mut ref_of: FxHashMap<u128, u32> = FxHashMap::default();
    ref_of.reserve(n_door + 7_000_000);
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        ref_of.insert((u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64), i as u32);
    }
    let mut qb: Vec<QB> = Vec::with_capacity(qs.len());
    let mut refid: Vec<u32> = Vec::with_capacity(qs.len());
    let dsh: Vec<u32> = crate::from_bytes::<u32>(&std::fs::read(format!("{pd}/dshard.bin")).unwrap()).to_vec();
    let mut shard_sc: Vec<(usize, u32)> = vec![(usize::MAX, 0); 1 << 16];
    for i in 0..n_door { let b = &door[i * 40..]; let s = shape_hash.iter().position(|&h| h == u64::from_le_bytes(b[0..8].try_into().unwrap())).unwrap(); shard_sc[dsh[i] as usize] = (s, u32::from_le_bytes(b[8..12].try_into().unwrap())); }
    let mut next = n_door as u32;
    for q in qs {
        let k = q.key;
        let r = row_of[&k] as usize;
        let (s, c, fr) = f.hdr(r);
        let sc = &mut shard_sc[q.shard as usize];
        if sc.0 == usize::MAX { *sc = (s, c); }
        assert!(*sc == (s, c), "query shard is not the row's (shape, cell)");
        let id = *ref_of.entry(k).or_insert_with(|| { assert_eq!(fr, 57, "a new target not of f57"); next += 1; next - 1 });
        refid.push(id);
        let (h, l) = pk[r];
        qb.push(QB { shard: q.shard, src: q.src, xfer: q.xfer, low: l, high: h, _p: [0; 3] });
    }
    assert_eq!(next as usize - n_door, 6_735_699, "new states");
    // Directory entries per shard: distinct (shard, high) over the door and the frame.
    let n_shards = std::fs::metadata(format!("{pd}/cnt.bin")).unwrap().len() as usize / 8;
    let mut pre: FxHashSet<(u32, u32)> = FxHashSet::default();
    for i in 0..n_door { pre.insert((dsh[i], db[i].0)); }
    let n_old_pre = pre.len();
    for q in &qb { pre.insert((q.shard, q.high)); }
    let mut pcnt = vec![0u64; n_shards];
    for &(s, _) in &pre { pcnt[s as usize] += 1; }
    eprintln!("[prepbits] directory entries: {n_old_pre} at the frame start, {} at its end ({:.2} states each)", pre.len(), (n_door + 6_735_699) as f64 / pre.len() as f64);
    std::fs::write(format!("{pd}/qb.bin"), crate::as_bytes(&qb)).unwrap();
    std::fs::write(format!("{pd}/db.bin"), crate::as_bytes(&db)).unwrap();
    std::fs::write(format!("{pd}/pcnt.bin"), crate::as_bytes(&pcnt)).unwrap();
    std::fs::write(format!("{pd}/refid.bin"), crate::as_bytes(&refid)).unwrap();
    let sshape: Vec<u32> = (0..n_shards).map(|i| shard_sc[i].0 as u32).collect();
    let scell: Vec<u32> = (0..n_shards).map(|i| shard_sc[i].1).collect();
    std::fs::write(format!("{pd}/scell.bin"), crate::as_bytes(&scell)).unwrap();
    std::fs::write(format!("{pd}/sshape.bin"), crate::as_bytes(&sshape)).unwrap();
    eprintln!("[prepbits] wrote {pd}/qb.bin, db.bin, pcnt.bin, refid.bin; {:.1} s", t.elapsed().as_secs_f64());
}

/// One directory entry: the high digits (+1; 0 = empty) and the low digit's
/// bitset. `rank`: plus the door's rank base and its bits at the frame start.
#[repr(C)]
#[derive(Clone, Copy)]
struct E { hk: u32, base: u32, bits: [u64; 2] }
#[repr(C)]
#[derive(Clone, Copy)]
struct ER { hk: u32, base: u32, old: [u64; 2], new: [u64; 2] }

#[inline(always)]
fn slot_of(hk: u32, (base, mask): (u32, u32)) -> u32 { base + (hk.wrapping_mul(0x9E37_79B1).rotate_left(16) & mask) }

/// `bits` / `bitsr`: the timed loop over the same 257.7M sweep-ordered
/// lookups, each a directory probe in its shard + one bit test-and-set.
/// `bits`: the id is the packed slot `entry << 7 | low`. `bitsr`: the id is a
/// RANK - an old state's its rank in the frame-start set (`base` + popcount
/// below), a new state's resolved at the frame's end by a prefix pass.
pub fn run_bits(dir: &str, n_door: usize, rank: bool) {
    let t = std::time::Instant::now();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, dm, pm, sm, dsm) = (map("qb.bin"), map("d.bin"), map("pcnt.bin"), map("dshard.bin"), map("db.bin"));
    let qs: &[QB] = crate::from_bytes(&qm); let ds: &[u32] = crate::from_bytes(&dm); let pcnt: &[u64] = crate::from_bytes(&pm);
    let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dsm);
    let n_src: usize = std::fs::read_to_string(format!("{pd}/nsrc.txt")).unwrap().trim().parse().unwrap();
    let mut off: Vec<(u32, u32)> = Vec::with_capacity(pcnt.len());
    let mut total = 0u64;
    for &c in pcnt { let cap = (2 * c.max(1)).next_power_of_two(); off.push((total as u32, (cap - 1) as u32)); total += cap; }
    assert!(total << 7 < 1 << 32 && (total * 96 + n_door as u64) < (1 << 32), "slot ids must fit 32 bits");
    assert!(qs.iter().all(|q| q.low < 96));
    let tag = if rank { "bitsr" } else { "bits" };
    let mut frontier: Vec<u8> = Vec::with_capacity(7_000_000 * crate::PAYLOAD);
    let mut edges: Vec<(u32, u32, u32)> = Vec::with_capacity(qs.len());
    let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
    let (dt, t_drops, t_end, new, mem);
    let mut old_ids: FxHashSet<u32> = FxHashSet::default();
    macro_rules! probe { ($tab:expr, $sh:expr, $hk:expr) => {{
        let (b, m) = off[$sh as usize];
        let mut s = slot_of($hk, (b, m));
        loop { let e = &mut $tab[s as usize]; if e.hk == $hk { break; } if e.hk == 0 { e.hk = $hk; break; } s = b + ((s - b + 1) & m); }
        s
    }}; }
    if !rank {
        let mut tab: Vec<E> = vec![E { hk: 0, base: 0, bits: [0; 2] }; total as usize];
        for i in 0..n_door { let (h, l) = db[i]; let s = probe!(tab, dsh[i], h + 1); tab[s as usize].bits[(l >> 6) as usize] |= 1 << (l & 63); old_ids.insert(s << 7 | l); }
        mem = total as f64 * std::mem::size_of::<E>() as f64;
        eprintln!("[{tag}] setup {:.1} s: directory {} slots x {} B = {:.2} GB", t.elapsed().as_secs_f64(), total, std::mem::size_of::<E>(), mem / 1e9);
        crate::perf_on(true);
        let t = std::time::Instant::now();
        for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
        t_drops = t.elapsed().as_secs_f64();
        let mut nw = 0u64;
        for q in qs {
            let s = probe!(tab, q.shard, q.high + 1);
            let w = &mut tab[s as usize].bits[(q.low >> 6) as usize];
            let b = 1u64 << (q.low & 63);
            if *w & b == 0 { *w |= b; nw += 1; frontier.extend_from_slice(&[0u8; crate::PAYLOAD]); }
            edges.push((q.src, s << 7 | q.low, q.xfer));
        }
        dt = t.elapsed().as_secs_f64(); new = nw; t_end = 0.0; crate::perf_on(false);
    } else {
        let mut tab: Vec<ER> = vec![ER { hk: 0, base: 0, old: [0; 2], new: [0; 2] }; total as usize];
        for i in 0..n_door { let (h, l) = db[i]; let s = probe!(tab, dsh[i], h + 1); tab[s as usize].old[(l >> 6) as usize] |= 1 << (l & 63); }
        // Rank bases over the frame-start set, in arena (= shard, slot) order.
        let mut acc = 0u32;
        for e in tab.iter_mut() { e.base = acc; acc += e.old[0].count_ones() + e.old[1].count_ones(); }
        assert_eq!(acc as usize, n_door);
        mem = total as f64 * std::mem::size_of::<ER>() as f64;
        eprintln!("[{tag}] setup {:.1} s: directory {} slots x {} B = {:.2} GB", t.elapsed().as_secs_f64(), total, std::mem::size_of::<ER>(), mem / 1e9);
        crate::perf_on(true);
        let t = std::time::Instant::now();
        for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
        t_drops = t.elapsed().as_secs_f64();
        let mut nw = 0u64;
        let nd = n_door as u32;
        for q in qs {
            let s = probe!(tab, q.shard, q.high + 1);
            let e = &mut tab[s as usize];
            let (wi, b) = ((q.low >> 6) as usize, 1u64 << (q.low & 63));
            let id = if e.old[wi] & b != 0 {
                let below = if wi == 0 { (e.old[0] & (b - 1)).count_ones() } else { e.old[0].count_ones() + (e.old[1] & (b - 1)).count_ones() };
                e.base + below
            } else {
                if e.new[wi] & b == 0 { e.new[wi] |= b; nw += 1; frontier.extend_from_slice(&[0u8; crate::PAYLOAD]); }
                nd + s * 96 + q.low
            };
            edges.push((q.src, id, q.xfer));
        }
        dt = t.elapsed().as_secs_f64(); new = nw; crate::perf_on(false);
        // The frame's end: new states ranked in arena order, their edges resolved.
        let t = std::time::Instant::now();
        let mut acc = n_door as u32;
        for e in tab.iter_mut() { e.base = acc; acc += e.new[0].count_ones() + e.new[1].count_ones(); }
        for ed in edges.iter_mut() {
            if ed.1 >= nd {
                let (s, l) = ((ed.1 - nd) / 96, (ed.1 - nd) % 96);
                let e = &tab[s as usize];
                let b = 1u64 << (l & 63);
                ed.1 = e.base + if l < 64 { (e.new[0] & (b - 1)).count_ones() } else { e.new[0].count_ones() + (e.new[1] & (b - 1)).count_ones() };
            }
        }
        t_end = t.elapsed().as_secs_f64();
    }
    std::hint::black_box((&frontier, &dropmin));
    eprintln!("[{tag}] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new {new} ({}), {:.1} ns per query{}; structure {:.2} GB",
        if new == 6735699 { "OK" } else { "MISMATCH" }, (dt - t_drops) * 1e9 / qs.len() as f64,
        if rank { format!("; frame-end rank pass {t_end:.2} s") } else { String::new() }, mem / 1e9);
    if std::env::var_os("BENCH_NOCHECK").is_some() { return; }
    if rank { check(tag, &pd, &edges, n_door, &|m| (m as usize) < n_door, true) } else { check(tag, &pd, &edges, n_door, &|m| old_ids.contains(&m), false) }
}

/// UNTIMED: every lookup's decision and target against the reference (v3c's
/// ids: door index, new states numbered in sweep order); the fingerprint is
/// over THIS variant's decisions.
pub fn check(tag: &str, pd: &str, edges: &[(u32, u32, u32)], n_door: usize, is_old: &dyn Fn(u32) -> bool, rank: bool) {
    let rm = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/refid.bin")).unwrap()).unwrap() };
    let refid: &[u32] = crate::from_bytes(&rm);
    let mut mine_of_ref: Vec<u32> = vec![u32::MAX; n_door + 6_735_699];
    let mut ref_of_mine: FxHashMap<u32, u32> = FxHashMap::default();
    let (mut bad_map, mut bad_dec) = (0u64, 0u64);
    let mut seen_ref = vec![false; n_door + 6_735_699];
    let mut seen_mine: FxHashSet<u32> = FxHashSet::default();
    let (mut fp, mut new) = (0u64, 0u64);
    for (i, ed) in edges.iter().enumerate() {
        let (r, m) = (refid[i], ed.1);
        let slot = &mut mine_of_ref[r as usize];
        if *slot == u32::MAX { *slot = m; } else if *slot != m { bad_map += 1; }
        if *ref_of_mine.entry(m).or_insert(r) != r { bad_map += 1; }
        let ref_new = r as usize >= n_door && !seen_ref[r as usize];
        seen_ref[r as usize] = true;
        let mine_new = !is_old(m) && seen_mine.insert(m);
        if ref_new != mine_new { bad_dec += 1; }
        new += mine_new as u64;
        fp = fp.wrapping_mul(0x100_0000_01b3) ^ (mine_new as u64);
    }
    if rank {
        let dense = edges.iter().all(|e| (e.1 as usize) < n_door + 6_735_699);
        eprintln!("[{tag}] ids dense in [0, {}): {dense}", n_door + 6_735_699);
    }
    eprintln!("[{tag}] CHECK {} lookups: {new} new, id bijection violations {bad_map}, decision mismatches {bad_dec}; decision fingerprint {fp:016x}", edges.len());
}

/// How one field's raw 32-bit value becomes its digit: an affine map
/// (`(v - min) >> k`, a full progression, as the old code's `as_i16` ranges)
/// or a binary search in its sorted values (`spd`, odd constants).
enum Raw { Affine { min: i32, k: u32 }, Search(Vec<i32>) }

#[inline(always)]
pub fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    x ^ (x >> 31)
}

/// `packtime`: shape 2's rows (95.5%) from their RAW field values: the
/// packing (`Raw` per field, the dash combo table, mixed radix) against the
/// production row key over the same fields (two `mix64` a field), and a bare
/// read of the values. Each a single-thread pass, best of 3.
pub fn pack_time(dir: &str) {
    let f = Fields::open(dir);
    let s = 2;
    let fs = &f.shapes[s];
    let rows: Vec<usize> = (0..f.n).filter(|&i| f.hdr(i).0 == s).collect();
    let p = packing(&f, s, &rows);
    let nf = fs.len();
    // Raw values, row-major, and each field's code tag.
    let tags: Vec<u64> = fs.iter().map(|x| { let t = x.codes[0] >> 32; assert!(x.codes.iter().all(|c| c >> 32 == t)); t << 32 }).collect();
    let mut raw: Vec<u32> = Vec::with_capacity(rows.len() * nf);
    let mut cells: Vec<u32> = Vec::with_capacity(rows.len());
    for &i in &rows { cells.push(f.hdr(i).1); for j in 0..nf { raw.push(fs[j].codes[f.val(i, j) as usize] as u32); } }
    let conv: Vec<Raw> = fs.iter().map(|x| {
        let mut v: Vec<i32> = x.codes.iter().map(|&c| c as u32 as i32).collect();
        v.sort_unstable();
        let min = v[0];
        let g = v.iter().fold(0u32, |a, &y| a | (y - min) as u32);
        let k = if g == 0 { 0 } else { g.trailing_zeros() };
        if v.iter().enumerate().all(|(i, &y)| ((y - min) >> k) as usize == i) { Raw::Affine { min, k } } else { Raw::Search(v) }
    }).collect();
    for (j, c) in conv.iter().enumerate() { println!("  {:<28} {}", fs[j].name, match c { Raw::Affine { min, k } => format!("affine (v - {min}) >> {k}"), Raw::Search(v) => format!("binary search over {}", v.len()) }); }
    let digit = |j: usize, v: u32| -> u64 { match &conv[j] { Raw::Affine { min, k } => ((v as i32 - min) >> k) as u64, Raw::Search(t) => t.partition_point(|&y| y < v as i32) as u64 } };
    // The dash combo table over raw digits (its own labels; same radix).
    let dash = p.digits.iter().find(|d| d.table.is_some()).unwrap();
    let dprod: u64 = dash.fields.iter().map(|&j| fs[j].codes.len() as u64).product();
    let mut dtab = vec![u32::MAX; dprod as usize];
    let mut nd = 0u32;
    for r in 0..rows.len() {
        let t = dash.fields.iter().fold(0u64, |a, &j| a * fs[j].codes.len() as u64 + digit(j, raw[r * nf + j])) as usize;
        if dtab[t] == u32::MAX { dtab[t] = nd; nd += 1; }
    }
    assert_eq!(nd as u64, dash.radix);
    // The digit plan in packing order: (field or dash, radix).
    let plan: Vec<(Option<usize>, u64)> = p.high.iter().map(|&d| (if p.digits[d].table.is_some() { None } else { Some(p.digits[d].fields[0]) }, p.digits[d].radix)).collect();
    let low_j = p.low.map(|d| p.digits[d].fields[0]).unwrap();
    let dfields = dash.fields.clone();
    let radices: Vec<u64> = fs.iter().map(|x| x.codes.len() as u64).collect();
    let pack = |r: &[u32]| -> (u32, u32) {
        let mut h = 0u64;
        for &(fj, rad) in &plan {
            let d = match fj { Some(j) => digit(j, r[j]), None => dtab[dfields.iter().fold(0u64, |a, &j| a * radices[j] + digit(j, r[j])) as usize] as u64 };
            h = h * rad + d;
        }
        (h as u32, digit(low_j, r[low_j]) as u32)
    };
    // Correct: the raw packing is the prep packing relabelled (a bijection per shard).
    let mut fwd: FxHashMap<(u32, u32, u32), (u32, u32)> = FxHashMap::default();
    let mut bwd: FxHashMap<(u32, u32, u32), (u32, u32)> = FxHashMap::default();
    let mut bad = 0u64;
    for (r, &i) in rows.iter().enumerate() {
        let a = p.pack(&f, i); let b = pack(&raw[r * nf..(r + 1) * nf]);
        if *fwd.entry((cells[r], a.0, a.1)).or_insert(b) != b { bad += 1; }
        if *bwd.entry((cells[r], b.0, b.1)).or_insert(a) != a { bad += 1; }
    }
    println!("packtime: {} rows of shape {s}, {nf} varying fields; raw packing vs prep packing: {bad} bijection violations", rows.len());
    drop(fwd); drop(bwd);
    let c1: Vec<u64> = (0..nf).map(|j| 0x5bf0_3635u64 ^ (j as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15)).collect();
    let c2: Vec<u64> = (0..nf).map(|j| 0x27d4_eb2fu64 ^ (j as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15)).collect();
    let n = rows.len() as f64;
    for rep in 0..3 {
        let t = std::time::Instant::now();
        let mut acc = 0u64;
        for r in raw.chunks_exact(nf) { acc = acc.wrapping_add(r.iter().fold(0u64, |a, &v| a.wrapping_add(v as u64))); }
        let t_read = t.elapsed().as_secs_f64(); std::hint::black_box(acc);
        let t = std::time::Instant::now();
        let mut acc = 0u64;
        for r in raw.chunks_exact(nf) {
            let (mut h1, mut h2) = (0u64, 0u64);
            for j in 0..nf { let code = tags[j] | r[j] as u64; h1 = h1.wrapping_add(mix64(c1[j] ^ code)); h2 = h2.wrapping_add(mix64(c2[j] ^ code)); }
            acc ^= mix64(h1) ^ mix64(h2).rotate_left(7);
        }
        let t_key = t.elapsed().as_secs_f64(); std::hint::black_box(acc);
        let t = std::time::Instant::now();
        let mut acc = 0u64;
        for r in raw.chunks_exact(nf) { let (h, l) = pack(r); acc = acc.wrapping_add((h as u64) << 7 | l as u64); }
        let t_pack = t.elapsed().as_secs_f64(); std::hint::black_box(acc);
        println!("  rep {rep}: read {:.2} ns/row, KEY (2 x {nf} mix64) {:.2} ns/row, PACK {:.2} ns/row", t_read * 1e9 / n, t_key * 1e9 / n, t_pack * 1e9 / n);
    }
}

/// `cellcensus`: every field's dictionary built PER (shape, cell). Per cell:
/// the local distinct counts of spd.x, spd.y, the dash combo, the flags combo
/// and flags+dash joint; the dense local product under several groupings;
/// its occupancy, and the total memory with a dense bitmask per cell.
pub fn cell_census(dir: &str) {
    let f = Fields::open(dir);
    // Per row: (shape, cell, flags tuple, dash tuple, spd.x, spd.y).
    let mut v: Vec<(u32, u32, u64, u64, u32, u32)> = Vec::with_capacity(f.n);
    let plan: Vec<(Vec<usize>, Vec<usize>, Option<usize>, Option<usize>)> = (0..f.shapes.len()).map(|s| {
        let dash: Vec<usize> = ["dash_time", "dash_effect_time", "dash_target.x", "dash_target.y", "dash_accel.x", "dash_accel.y", "has_dashed"].iter().filter_map(|n| f.find(s, n)).collect();
        let (sx, sy) = (f.find(s, "spd.x"), f.find(s, "spd.y"));
        let flags = (0..f.shapes[s].len()).filter(|&j| !dash.contains(&j) && Some(j) != sx && Some(j) != sy && f.shapes[s][j].name != "player.x" && f.shapes[s][j].name != "player.y").collect();
        (flags, dash, sx, sy)
    }).collect();
    for i in 0..f.n {
        let (s, c, _) = f.hdr(i);
        let (fl, da, sx, sy) = &plan[s];
        let tup = |js: &[usize]| js.iter().fold(0u64, |a, &j| a * f.shapes[s][j].codes.len() as u64 + f.val(i, j) as u64);
        v.push((s as u32, c, tup(fl), tup(da), sx.map_or(0, |j| f.val(i, j)), sy.map_or(0, |j| f.val(i, j))));
    }
    v.sort_unstable();
    // Room-wide flags+dash joint dictionary size per shape.
    let mut room_fd = vec![0u64; f.shapes.len()];
    { let mut h: FxHashSet<(u32, u64, u64)> = Default::default(); for x in &v { h.insert((x.0, x.2, x.3)); } for &(s, _, _) in &h { room_fd[s as usize] += 1; } }
    println!("room-wide flags+dash joint per shape: {room_fd:?}");
    // Groupings: name, and per cell the local product from the distinct counts.
    struct Cell { n: u64, sx: u64, sy: u64, d: u64, fl: u64, fd: u64, sxy: u64, fdx: u64, e1: u64, flr: u64, fdr: u64 }
    let mut cells: Vec<Cell> = Vec::new();
    let mut i = 0;
    let distinct = |mut k: Vec<u64>| { k.sort_unstable(); k.dedup(); k.len() as u64 };
    while i < v.len() {
        let mut j = i; while j < v.len() && (v[j].0, v[j].1) == (v[i].0, v[i].1) { j += 1; }
        let r = &v[i..j];
        let fd = |x: &(u32, u32, u64, u64, u32, u32)| x.2.wrapping_mul(0x1_0000_0001) ^ x.3;
        cells.push(Cell { n: r.len() as u64,
            sx: distinct(r.iter().map(|x| x.4 as u64).collect()), sy: distinct(r.iter().map(|x| x.5 as u64).collect()),
            d: distinct(r.iter().map(|x| x.3).collect()), fl: distinct(r.iter().map(|x| x.2).collect()),
            fd: distinct(r.iter().map(fd).collect()), sxy: distinct(r.iter().map(|x| (x.4 as u64) << 32 | x.5 as u64).collect()),
            fdx: distinct(r.iter().map(|x| fd(x) ^ (x.4 as u64) << 40).collect()),
            e1: distinct(r.iter().map(|x| ((x.4 as u64) << 32 | x.5 as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15) ^ x.3).collect()),
            flr: plan[r[0].0 as usize].0.iter().map(|&j| f.shapes[r[0].0 as usize][j].codes.len() as u64).product(),
            fdr: room_fd[r[0].0 as usize] });
        i = j;
    }
    let total: u64 = cells.iter().map(|c| c.n).sum();
    println!("cellcensus: {} (shape, cell) shards, {total} states", cells.len());
    // Distribution, unweighted and weighted by states.
    let dist = |name: &str, g: &dyn Fn(&Cell) -> f64| {
        let mut a: Vec<(f64, u64)> = cells.iter().map(|c| (g(c), c.n)).collect();
        a.sort_by(|x, y| x.0.partial_cmp(&y.0).unwrap());
        let q = |p: f64| a[((a.len() - 1) as f64 * p) as usize].0;
        let wq = |p: f64| { let t = (total as f64 * p) as u64; let mut acc = 0; for &(x, n) in &a { acc += n; if acc >= t { return x; } } a[a.len() - 1].0 };
        println!("  {name:<34} cells p50 {:>9.3} p90 {:>9.3} max {:>9.3} | by states p10 {:>9.3} p50 {:>9.3} p90 {:>9.3} p99 {:>9.3}", q(0.5), q(0.9), q(1.0), wq(0.1), wq(0.5), wq(0.9), wq(0.99));
    };
    dist("distinct spd.x", &|c| c.sx as f64);
    dist("distinct spd.y", &|c| c.sy as f64);
    dist("distinct (spd.x, spd.y) joint", &|c| c.sxy as f64);
    dist("distinct dash combo", &|c| c.d as f64);
    dist("distinct flags combo", &|c| c.fl as f64);
    dist("distinct flags+dash joint", &|c| c.fd as f64);
    dist("states", &|c| c.n as f64);
    let groupings: Vec<(&str, Box<dyn Fn(&Cell) -> u64>)> = vec![
        ("A flags x dash x spd.x x spd.y", Box::new(|c: &Cell| c.fl * c.d * c.sx * c.sy)),
        ("B (flags+dash) x spd.x x spd.y", Box::new(|c: &Cell| c.fd * c.sx * c.sy)),
        ("C (flags+dash) x (spd.x,spd.y)", Box::new(|c: &Cell| c.fd * c.sxy)),
        ("D ((flags+dash),spd.x) x spd.y", Box::new(|c: &Cell| c.fdx * c.sy)),
    ];
    // `bitspd`: a directory keyed by speed, a mask over the flags.
    let w = |b: u64| b.div_ceil(64) * 64;
    let e1: u64 = cells.iter().map(|c| c.e1).sum();
    let e2: u64 = cells.iter().map(|c| c.sxy).sum();
    let m1: u64 = cells.iter().map(|c| c.e1 * w(c.flr)).sum();
    let m2l: u64 = cells.iter().map(|c| c.sxy * w(c.fd)).sum();
    let m2r: u64 = cells.iter().map(|c| c.sxy * w(c.fdr)).sum();
    println!("S1 key (cell, spd.x, spd.y, dash combo), mask over freeze/djump/grace/flip (room radix): {e1} entries, {:.2} states/entry, occupancy {:.2}%, masks {:.3} GB",
        total as f64 / e1 as f64, 100.0 * total as f64 / m1 as f64, m1 as f64 / 8e9);
    println!("S2 key (cell, spd.x, spd.y), mask over flags+dash: {e2} entries, {:.2} states/entry; per-cell dict: occupancy {:.2}%, masks {:.3} GB; room-wide dict: occupancy {:.3}%, masks {:.3} GB",
        total as f64 / e2 as f64, 100.0 * total as f64 / m2l as f64, m2l as f64 / 8e9, 100.0 * total as f64 / m2r as f64, m2r as f64 / 8e9);
    dist("S1 states per entry (cell mean)", &|c| c.n as f64 / c.e1 as f64);
    dist("S2 states per entry (cell mean)", &|c| c.n as f64 / c.sxy as f64);
    dist("S2 per-cell mask bits", &|c| w(c.fd) as f64);
    for (name, g) in &groupings {
        println!("{name}:");
        dist("  product bits", &|c| (g(c) as f64).log2());
        dist("  occupancy %", &|c| 100.0 * c.n as f64 / g(c) as f64);
        let bits: u64 = cells.iter().map(|c| g(c).div_ceil(64) * 64).sum();
        println!("    TOTAL dense bitmasks {:.3} GB ({:.2}% occupied overall); states/product overall {:.2}%", bits as f64 / 8e9, 100.0 * total as f64 / bits as f64, 100.0 * total as f64 / cells.iter().map(|c| g(c)).sum::<u64>() as f64);
    }
}

/// `bitcell` / `bitspd` / `bitspd2`: one 64-bit mask word per directory
/// entry, the entry's key carrying the word's index. Per (shape, cell) shard:
/// - `bitcell`: key = the high digits (freeze, djump, grace, flip, spd.x, dash
///   combo), mask over spd.y's PER-CELL dictionary (<= 75 values here);
/// - `bitspd`: key = (spd.x, spd.y, dash combo), mask over freeze x djump x
///   grace x flip (room radix, 126 for shape 2);
/// - `bitspd2`: key = (spd.x, spd.y), mask over the PER-CELL dictionary of
///   the (flags, dash combo) joint.
/// The per-cell dictionaries come from the data (door + the frame); the
/// (key, bit) of every state is precomputed untimed, as `bits`' packing is.
pub fn run_words(dir: &str, n_door: usize, mode: &str) {
    let t = std::time::Instant::now();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, dm, sm, dsm, shm) = (map("qb.bin"), map("d.bin"), map("dshard.bin"), map("db.bin"), map("sshape.bin"));
    let qs: &[QB] = crate::from_bytes(&qm); let ds: &[u32] = crate::from_bytes(&dm);
    let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dsm); let sshape: &[u32] = crate::from_bytes(&shm);
    let n_src: usize = std::fs::read_to_string(format!("{pd}/nsrc.txt")).unwrap().trim().parse().unwrap();
    let n_shards = sshape.len();
    // posmask{R}: the directory per (shape, R x R region) on (high, spd.y), the mask over the region's cells.
    let pr: i32 = mode.strip_prefix("posmask").map_or(0, |r| r.parse().unwrap());
    let scm = map("scell.bin");
    let scell: &[u32] = crate::from_bytes(&scm);
    let mut gid: FxHashMap<(u32, i32, i32), u32> = FxHashMap::default();
    let (grp, ppos): (Vec<u32>, Vec<u32>) = (0..n_shards).map(|s| {
        if pr == 0 { return (s as u32, 0); }
        let (x, y) = crate::structure::cell_xy(scell[s]);
        let n = gid.len() as u32;
        (*gid.entry((sshape[s], x.div_euclid(pr), y.div_euclid(pr))).or_insert(n), (x.rem_euclid(pr) + pr * y.rem_euclid(pr)) as u32)
    }).unzip();
    let n_groups = if pr == 0 { n_shards } else { gid.len() };
    // The packing's digits per shape (as prepbits built them).
    let f = Fields::open(dir);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<Packing> = (0..f.shapes.len()).map(|s| packing(&f, s, &by_shape[s])).collect();
    drop(by_shape);
    // Per shape: the high digits' radices and roles (0 flag, 1 spd.x, 2 dash).
    let roles: Vec<Vec<(u64, u8)>> = packs.iter().map(|p| p.high.iter().map(|&d| (p.digits[d].radix, if p.digits[d].name == "spd.x" { 1 } else if p.digits[d].table.is_some() { 2 } else { 0 })).collect()).collect();
    let low_r: Vec<u64> = packs.iter().map(|p| p.low.map_or(1, |d| p.digits[d].radix)).collect();
    // A state (shard, high, low) -> (mask key, mask index) before per-cell dictionaries.
    let split = |sh: u32, high: u32, low: u32| -> (u64, u64) {
        let s = sshape[sh as usize] as usize;
        let mut h = high as u64;
        let mut ds_: Vec<(u64, u64, u8)> = Vec::with_capacity(8);
        for &(r, role) in roles[s].iter().rev() { ds_.push((h % r, r, role)); h /= r; }
        ds_.reverse();
        let (mut fl, mut sx, mut da) = (0u64, 0u64, 0u64);
        for &(d, r, role) in &ds_ { match role { 0 => fl = fl * r + d, 1 => sx = sx * r + d, _ => da = da * r + d } }
        let dar: u64 = roles[s].iter().filter(|x| x.1 == 2).map(|x| x.0).product();
        let spd = sx * low_r[s] + low as u64;
        match mode {
            "bitcell" => (high as u64, low as u64),
            "bitspd" => (spd * dar + da, fl),
            _ if pr > 0 => (high as u64 * low_r[s] + low as u64, ppos[sh as usize] as u64),
            _ => (spd, fl * dar + da),
        }
    };
    // Per-cell dictionaries over the mask index (bitcell: spd.y; bitspd2: the
    // flags+dash joint); bitspd keeps the room radix.
    let all = || db.iter().enumerate().map(|(i, &(h, l))| (dsh[i], h, l)).chain(qs.iter().map(|q| (q.shard, q.high, q.low)));
    let mut local: Vec<FxHashMap<u64, u32>> = vec![Default::default(); n_shards];
    let mut width: Vec<u64> = vec![0; n_shards];
    let mut keyed: FxHashMap<(u32, u32, u32), (u32, u32)> = FxHashMap::default(); // (shard, high, low) -> (hk, bit), memo
    if pr > 0 {
        for s in 0..n_shards { width[s] = (pr * pr) as u64; }
    } else if mode == "bitspd" {
        for s in 0..n_shards { let sp = sshape[s] as usize; width[s] = roles[sp].iter().filter(|x| x.1 == 0).map(|x| x.0).product(); }
    } else {
        for (sh, h, l) in all() { let (_, m) = split(sh, h, l); let d = &mut local[sh as usize]; let n = d.len() as u32; d.entry(m).or_insert(n); }
        for s in 0..n_shards { width[s] = local[s].len().max(1) as u64; }
    }
    let words: Vec<u64> = width.iter().map(|w| w.div_ceil(64)).collect();
    let mut pre: Vec<FxHashSet<u32>> = vec![Default::default(); n_groups];
    let mut key_of = |sh: u32, h: u32, l: u32| -> (u32, u32) {
        *keyed.entry((sh, h, l)).or_insert_with(|| {
            let (k, m) = split(sh, h, l);
            let m = if mode == "bitspd" || pr > 0 { m } else { local[sh as usize][&m] as u64 };
            let hk = k * words[sh as usize] + (m >> 6) + 1;
            assert!(hk < u32::MAX as u64);
            (hk as u32, (m & 63) as u32)
        })
    };
    let dq: Vec<(u32, u32)> = (0..n_door).map(|i| key_of(dsh[i], db[i].0, db[i].1)).collect();
    let qw: Vec<QB> = qs.iter().map(|q| { let (hk, b) = key_of(q.shard, q.high, q.low); QB { shard: grp[q.shard as usize], src: q.src, xfer: q.xfer, low: b, high: hk, _p: [0; 3] } }).collect();
    let dsh: Vec<u32> = dsh.iter().map(|&s| grp[s as usize]).collect();
    drop(keyed);
    for (i, &(hk, _)) in dq.iter().enumerate() { pre[dsh[i] as usize].insert(hk); }
    let n_old = pre.iter().map(|p| p.len()).sum::<usize>();
    for q in &qw { pre[q.shard as usize].insert(q.high); }
    let pcnt: Vec<u64> = pre.iter().map(|p| p.len() as u64).collect();
    drop(pre);
    let entries: u64 = pcnt.iter().sum();
    let mut load = 0.5;
    let cap_of = |c: u64, load: f64| ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two();
    if pcnt.iter().map(|&c| cap_of(c, load)).sum::<u64>() * 64 >= 1 << 32 { load = 0.8; }
    let mut off: Vec<(u32, u32)> = Vec::with_capacity(n_groups);
    let mut total = 0u64;
    for &c in &pcnt { let cap = cap_of(c, load); off.push((total as u32, (cap - 1) as u32)); total += cap; }
    assert!(total * 64 < 1 << 32, "ids slot << 6 | bit must fit 32 bits");
    #[repr(C, packed)]
    #[derive(Clone, Copy)]
    struct W { hk: u32, bits: u64 }
    let mut tab: Vec<W> = vec![W { hk: 0, bits: 0 }; total as usize];
    huge(&mut tab);
    macro_rules! probe { ($sh:expr, $hk:expr) => {{
        let (b, m) = off[$sh as usize];
        let mut s = slot_of($hk, (b, m));
        loop { let e = &mut tab[s as usize]; let k = e.hk; if k == $hk { break; } if k == 0 { e.hk = $hk; break; } s = b + ((s - b + 1) & m); }
        s
    }}; }
    let mut old_ids: FxHashSet<u32> = FxHashSet::default();
    for i in 0..n_door { let (hk, b) = dq[i]; let s = probe!(dsh[i], hk); let e = &mut tab[s as usize]; let w = e.bits; e.bits = w | 1 << b; old_ids.insert(s << 6 | b); }
    drop(dq);
    let mem = total as f64 * std::mem::size_of::<W>() as f64;
    let wsum = |g: &dyn Fn(usize) -> u64| (0..n_groups).map(g).sum::<u64>();
    eprintln!("[{mode}] setup {:.1} s (untimed): mask widths: {} directory groups, mean {:.1} bits (by entries); {n_old} entries at the frame start, {entries} at its end ({:.2} states each, mask fill {:.2}%); directory {} slots x {} B at load {load} = {:.2} GB",
        t.elapsed().as_secs_f64(), n_groups, wsum(&|s| pcnt[s] * width[s].min(64)) as f64 / entries as f64, (n_door + 6_735_699) as f64 / entries as f64,
        100.0 * (n_door + 6_735_699) as f64 / wsum(&|s| pcnt[s] * 64) as f64, total, std::mem::size_of::<W>(), mem / 1e9);
    let mut frontier: Vec<u8> = Vec::with_capacity(7_000_000 * crate::PAYLOAD);
    let mut edges: Vec<(u32, u32, u32)> = Vec::with_capacity(qw.len());
    let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
    crate::perf_on(true);
    let t = std::time::Instant::now();
    for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
    let t_drops = t.elapsed().as_secs_f64();
    let mut nw = 0u64;
    for q in &qw {
        let s = probe!(q.shard, q.high);
        let e = &mut tab[s as usize];
        let (w, b) = (e.bits, 1u64 << q.low);
        if w & b == 0 { e.bits = w | b; nw += 1; frontier.extend_from_slice(&[0u8; crate::PAYLOAD]); }
        edges.push((q.src, s << 6 | q.low, q.xfer));
    }
    let dt = t.elapsed().as_secs_f64();
    crate::perf_on(false);
    std::hint::black_box((&frontier, &dropmin));
    eprintln!("[{mode}] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new {nw} ({}), {:.1} ns per query; structure {:.2} GB",
        if nw == 6735699 { "OK" } else { "MISMATCH" }, (dt - t_drops) * 1e9 / qw.len() as f64, mem / 1e9);
    if std::env::var_os("BENCH_NOCHECK").is_some() { return; }
    check(mode, &pd, &edges, n_door, &|m| old_ids.contains(&m), false);
}

/// `bitintern`: `bits` with the 128-bit spd.y mask INTERNED - a directory
/// entry is (high digits, id of a shared mask); the room's distinct masks
/// (~80k at f57) sit in one small table, hash-consed on insert.
pub fn run_intern(dir: &str, n_door: usize) {
    let t = std::time::Instant::now();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, dm, pm, sm, dsm) = (map("qb.bin"), map("d.bin"), map("pcnt.bin"), map("dshard.bin"), map("db.bin"));
    let qs: &[QB] = crate::from_bytes(&qm); let ds: &[u32] = crate::from_bytes(&dm); let pcnt: &[u64] = crate::from_bytes(&pm);
    let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dsm);
    let n_src: usize = std::fs::read_to_string(format!("{pd}/nsrc.txt")).unwrap().trim().parse().unwrap();
    let mut off: Vec<(u32, u32)> = Vec::with_capacity(pcnt.len());
    let mut total = 0u64;
    for &c in pcnt { let cap = (2 * c.max(1)).next_power_of_two(); off.push((total as u32, (cap - 1) as u32)); total += cap; }
    assert!(total * 128 < 1 << 32);
    #[repr(C)] #[derive(Clone, Copy)] struct E { hk: u32, set: u32 }
    let mut tab: Vec<E> = vec![E { hk: 0, set: 0 }; total as usize];
    let mut masks: Vec<u128> = vec![0];
    let mut intern: FxHashMap<u128, u32> = FxHashMap::default();
    intern.insert(0, 0);
    macro_rules! probe { ($sh:expr, $hk:expr) => {{
        let (b, m) = off[$sh as usize];
        let mut s = slot_of($hk, (b, m));
        loop { let e = &mut tab[s as usize]; if e.hk == $hk { break; } if e.hk == 0 { e.hk = $hk; break; } s = b + ((s - b + 1) & m); }
        s
    }}; }
    // The door: each entry's mask, then interned.
    let mut acc: Vec<u128> = vec![0; total as usize];
    let mut old_ids: FxHashSet<u32> = FxHashSet::default();
    for i in 0..n_door { let (h, l) = db[i]; let s = probe!(dsh[i], h + 1); acc[s as usize] |= 1u128 << l; old_ids.insert(s << 7 | l); }
    for (s, m) in acc.iter().enumerate() { if *m != 0 { let n = masks.len() as u32; let id = *intern.entry(*m).or_insert_with(|| { masks.push(*m); n }); tab[s].set = id; } }
    drop(acc);
    let n_old_masks = masks.len();
    let mem = total as f64 * 8.0;
    eprintln!("[bitintern] setup {:.1} s: directory {total} slots x 8 B = {:.2} GB; {n_old_masks} distinct masks at the frame start ({:.1} MB)", t.elapsed().as_secs_f64(), mem / 1e9, n_old_masks as f64 * 16.0 / 1e6);
    let mut frontier: Vec<u8> = Vec::with_capacity(7_000_000 * crate::PAYLOAD);
    let mut edges: Vec<(u32, u32, u32)> = Vec::with_capacity(qs.len());
    let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
    crate::perf_on(true);
    let t = std::time::Instant::now();
    for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
    let t_drops = t.elapsed().as_secs_f64();
    let mut nw = 0u64;
    for q in qs {
        let s = probe!(q.shard, q.high + 1);
        let e = &mut tab[s as usize];
        let m = masks[e.set as usize];
        let b = 1u128 << q.low;
        if m & b == 0 {
            let nm = m | b;
            let n = masks.len() as u32;
            e.set = *intern.entry(nm).or_insert_with(|| { masks.push(nm); n });
            nw += 1; frontier.extend_from_slice(&[0u8; crate::PAYLOAD]);
        }
        edges.push((q.src, s << 7 | q.low, q.xfer));
    }
    let dt = t.elapsed().as_secs_f64();
    crate::perf_on(false);
    std::hint::black_box((&frontier, &dropmin));
    eprintln!("[bitintern] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new {nw} ({}), {:.1} ns per query; directory {:.2} GB + masks {} ({:.1} MB) at the frame end",
        if nw == 6735699 { "OK" } else { "MISMATCH" }, (dt - t_drops) * 1e9 / qs.len() as f64, mem / 1e9, masks.len(), masks.len() as f64 * 16.0 / 1e6);
    if std::env::var_os("BENCH_NOCHECK").is_some() { return; }
    check("bitintern", &pd, &edges, n_door, &|m| old_ids.contains(&m), false);
}

/// MADV_HUGEPAGE over a fresh (untouched) table.
pub fn huge<T>(v: &mut [T]) {
    let (p, n) = (v.as_mut_ptr() as usize, std::mem::size_of_val(v));
    let a = (p + 4095) & !4095;
    if n > 8192 { unsafe { libc::madvise(a as *mut libc::c_void, (p + n - a) & !4095, libc::MADV_HUGEPAGE); } }
}
