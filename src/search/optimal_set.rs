//! THE OPTIMAL-PATH SET of a finished tree, measured (`rewrite
//! optimal-set`, plans/optimal-set.md): how big an output that describes
//! every winning path would be, instead of one witness.
//!
//! At a horizon `h`, the rotation graph's winning sets `W` (`arc_dp::backward`)
//! and the start's remainder pushed forward inside them give `R_t(n)`: the
//! remainders with which node `n` is reached at frame `t` from the start and
//! still wins by `h` (`arc_dp::reach`'s sets). At the optimum `h = H*` every
//! point of `R` is on an optimal path of the level's model: one that won
//! before `h - t` more frames would make the start win before `H*`. At
//! `h > H*` it holds the paths that win by `h`.
//!
//! `R_0` is one point and an edge maps a point to a point (a rotation or a
//! constant), so every `R_t(n)` is a FINITE set of points. They are
//! computed here exactly, point by point (`arc_dp::reach` keeps canonical
//! regions and widens a set past 32 segments to `W`: the objects ladder's
//! filter only needs a superset; this does not widen).
//!
//! Measured per horizon `H*..=H* + slack`: the set's nodes, (node, frame)
//! pairs, points, canonical segments and the edges between consecutive
//! pairs, per frame and in total, against the tree, the remainder-free marks
//! and `W`'s support; the bytes of its encodings; with `--concrete`, the
//! concrete states a breadth-first search inside the set visits (all of
//! them, to the horizon: what a selection search constrained by the set
//! costs at most), also inside the time-collapsed set (per node a frame
//! range and the union of its remainders) with `--collapsed`.

use super::arc_dp::{apply, backward, concrete_engines, exact_state, load, lookup_keys, optimum, par_chunks, rem_of, Covered, Graph, Loaded, NodeKeys, Winning};
use super::arcs::{point, Piece, Region, Seg};
use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};

/// A point of the reached set at a frame: (node, x, y).
type Pt = (u32, u32, u32);

/// The reached set, exactly: `layers[t]` is `R_t` as points sorted by
/// (node, x, y); `used[t]` the node-level edges `(src, dst, transfer pair)`
/// that carry a point of `R_t` into `R_{t+1}`, sorted; `point_edges[t]`
/// the distinct (point, point) transitions from `t` to `t + 1`.
struct Reached {
    layers: Vec<Vec<Pt>>,
    used: Vec<Vec<(u32, u32, u32)>>,
    point_edges: Vec<u64>,
}

/// `R_t` for `t = 0..=h`: `R_0 = {(start, p0)}` if `p0` is in `W_0(start)`,
/// `R_{t+1} = {(dst, e(p)) : (i, p) in R_t, i not a win, e out of i takes
/// p, e(p) in W_{t+1}(dst)}`. Points in parallel chunks.
fn reach_points(g: &Graph, w: &Winning, start: u32, p0: (u32, u32), h: u32, threads: usize) -> Result<Reached> {
    let inside = |t: u32, i: u32, x: u32, y: u32| w.at(t, i).is_some_and(|r| r.contains(x, y));
    let mut cur: Vec<Pt> = if inside(0, start, p0.0, p0.1) { vec![(start, p0.0, p0.1)] } else { Vec::new() };
    let (mut layers, mut used, mut point_edges) = (Vec::new(), Vec::new(), Vec::new());
    for t in 0..h {
        let parts = par_chunks(cur.len(), threads, |lo, hi| {
            let (mut out, mut edges, mut pe, mut tgt) = (Vec::new(), Vec::new(), 0u64, Vec::new());
            for &(i, x, y) in &cur[lo..hi] {
                if g.is_win(i) {
                    continue;
                }
                tgt.clear();
                for e in g.out_of(i) {
                    let (tx, ty) = g.xfer(e);
                    if !(tx.takes(x) && ty.takes(y)) {
                        continue;
                    }
                    let (qx, qy) = (apply(tx.action, x), apply(ty.action, y));
                    if inside(t + 1, e.dst, qx, qy) {
                        tgt.push((e.dst, qx, qy));
                        edges.push((i, e.dst, e.xfer));
                    }
                }
                tgt.sort_unstable();
                tgt.dedup();
                pe += tgt.len() as u64;
                out.extend_from_slice(&tgt);
            }
            Ok((out, edges, pe))
        })?;
        let (mut next, mut edges, mut pe) = (Vec::new(), Vec::new(), 0u64);
        for (o, e, p) in parts {
            next.extend(o);
            edges.extend(e);
            pe += p;
        }
        next.sort_unstable();
        next.dedup();
        edges.sort_unstable();
        edges.dedup();
        layers.push(std::mem::replace(&mut cur, next));
        used.push(edges);
        point_edges.push(pe);
    }
    layers.push(cur);
    Ok(Reached { layers, used, point_edges })
}

/// One node's points at a frame (sorted by (x, y)) as a canonical region.
fn region_of(pts: &[Pt], pieces: &mut Vec<Piece>, scratch: &mut Vec<Seg>) -> Region {
    pieces.clear();
    pieces.extend(pts.iter().map(|&(_, x, y)| Piece { y: Seg { lo: y, hi: y + 1 }, x: Seg { lo: x, hi: x + 1 } }));
    Region::from_pieces(pieces, scratch)
}

/// The node groups of a sorted layer: `(node, its points)`.
fn groups(layer: &[Pt]) -> impl Iterator<Item = (u32, &[Pt])> + '_ {
    layer.chunk_by(|a, b| a.0 == b.0).map(|c| (c[0].0, c))
}

/// The TIME-COLLAPSED set: per node its first and last frame in `R` and
/// the union of its points over them (sorted (x, y)).
struct Collapsed(FxHashMap<u32, (u32, u32, Vec<(u32, u32)>)>);

impl Collapsed {
    fn of(r: &Reached) -> Self {
        let mut m: FxHashMap<u32, (u32, u32, Vec<(u32, u32)>)> = FxHashMap::default();
        for (t, layer) in r.layers.iter().enumerate() {
            for &(i, x, y) in layer {
                let e = m.entry(i).or_insert((t as u32, t as u32, Vec::new()));
                e.1 = t as u32;
                e.2.push((x, y));
            }
        }
        for e in m.values_mut() {
            e.2.sort_unstable();
            e.2.dedup();
        }
        Collapsed(m)
    }
    fn admits(&self, t: u32, i: u32, q: (u32, u32)) -> bool {
        self.0.get(&i).is_some_and(|(lo, hi, pts)| *lo <= t && t <= *hi && pts.binary_search(&q).is_ok())
    }
}

/// What a concrete breadth-first search admits at frame `t`.
enum Admit<'a> {
    /// The exact set: `(node, q)` in `R_t`.
    Exact(&'a [Vec<Pt>]),
    /// The time-collapsed set.
    Collapsed(&'a Collapsed),
}

impl Admit<'_> {
    fn admits(&self, t: u32, i: u32, q: (u32, u32)) -> bool {
        match self {
            Admit::Exact(layers) => layers.get(t as usize).is_some_and(|l| l.binary_search(&(i, q.0, q.1)).is_ok()),
            Admit::Collapsed(c) => c.admits(t, i, q),
        }
    }
    fn name(&self) -> &'static str {
        match self {
            Admit::Exact(_) => "exact",
            Admit::Collapsed(_) => "collapsed",
        }
    }
}

/// A breadth-first search over CONCRETE states (the reference engine, every
/// input, every `rnd` leaf, as `arc_dp::concrete_search`) from the start to
/// the horizon, a successor kept only where `admit` holds its projection and
/// exact remainder, each exact state once a layer; every layer, not just to
/// the first win. Prints per layer the states and wins, then how many states
/// are on a winning path (the parent links kept, a backward mark). Checks
/// that a state inside `W` is inside the exact set (it must be: the BFS
/// starts at the start). Stops past `budget` states in a layer.
#[allow(clippy::too_many_arguments)]
fn concrete_bfs(level: crate::abstraction::Level, h: u32, node: &NodeKeys, keys: &celeste_engine::exact::KeySpace, w: &Winning, admit: &Admit, budget: usize, s: &crate::settings::Settings) -> Result<()> {
    use crate::frame::{wins_of, Block};
    use crate::trace::refengine::RefEngine;
    use celeste_engine::runtime2::Rt2;
    let workers = s.threads().max(1);
    let (engines, seeded) = concrete_engines(workers, s)?;
    let engines: Vec<std::sync::Mutex<RefEngine>> = engines.into_iter().map(std::sync::Mutex::new).collect();
    let start = Block::canonical(engines[0].lock().expect("an engine").initial()?)?;
    let t0 = std::time::Instant::now();
    /// A successor `admit` keeps, or a win (no state).
    struct Succ {
        parent: u32,
        win: bool,
        exact: Option<celeste_engine::exact::ExactRow>,
        row: Option<Rt2>,
    }
    /// A worker's counts: frames run, successors, (state, input) frames
    /// with more than one leaf, successors inside W but not admitted.
    #[derive(Default)]
    struct Counts {
        frames: u64,
        succs: u64,
        forks: u64,
        w_not_admitted: u64,
        admitted_not_w: u64,
    }
    let mut cur: Vec<Rt2> = vec![start.into_rt2()];
    // Per layer k >= 1: its states' parent links (parent in layer k-1,
    // child in layer k), and the parents with a winning successor.
    let mut links: Vec<Vec<(u32, u32)>> = Vec::new();
    let mut win_parents: Vec<Vec<u32>> = Vec::new();
    let mut sizes: Vec<usize> = vec![1];
    let mut total = Counts::default();
    let mut stopped = false;
    for k in 0..h {
        let t = std::time::Instant::now();
        const CHUNK: usize = 16;
        let next_chunk = std::sync::atomic::AtomicUsize::new(0);
        let parts: Vec<Result<(Vec<(usize, Vec<Succ>)>, Counts)>> = std::thread::scope(|sc| {
            let hs: Vec<_> = engines
                .iter()
                .map(|engine| {
                    let (cur, next_chunk) = (&cur, &next_chunk);
                    sc.spawn(move || -> Result<(Vec<(usize, Vec<Succ>)>, Counts)> {
                        let mut eng = engine.lock().expect("an engine");
                        let (mut out, mut c) = (Vec::new(), Counts::default());
                        loop {
                            let lo = next_chunk.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cur.len() {
                                return Ok((out, c));
                            }
                            // A state seen before in the chunk keeps no row (the
                            // merge keeps the first in order, which has one).
                            let (mut got, mut seen) = (Vec::new(), FxHashSet::default());
                            for (p, row) in cur.iter().enumerate().skip(lo).take(CHUNK) {
                                let mut ran = Covered::default();
                                for byte in 0u8..64 {
                                    if ran.covers(byte) {
                                        continue;
                                    }
                                    let (succ, read) = eng.frame_reads(row, byte)?;
                                    ran.push(byte, read);
                                    c.frames += 1;
                                    c.forks += (succ.len() > 1) as u64;
                                    for b in succ {
                                        c.succs += 1;
                                        if wins_of(b.rt2(), s)?.iter().any(|&x| x) {
                                            got.push(Succ { parent: p as u32, win: true, exact: None, row: None });
                                            continue;
                                        }
                                        let (shape, nk, cells) = lookup_keys(&b, level, seeded, keys)?;
                                        let Some(i) = node.get(shape, nk[0], cells[0]) else { continue };
                                        let q = rem_of(b.rt2())?;
                                        let in_w = w.at(k + 1, i).is_some_and(|r| r.contains(q.0, q.1));
                                        let ok = admit.admits(k + 1, i, q);
                                        c.w_not_admitted += (in_w && !ok) as u64;
                                        c.admitted_not_w += (ok && !in_w) as u64;
                                        if ok {
                                            let exact = exact_state(&b, b.positions()?[0]);
                                            let row = seen.insert(exact.clone()).then(|| b.into_rt2());
                                            got.push(Succ { parent: p as u32, win: false, exact: Some(exact), row });
                                        }
                                    }
                                }
                            }
                            out.push((lo, got));
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a concrete worker panicked")).collect()
        });
        let mut chunks: Vec<(usize, Vec<Succ>)> = Vec::new();
        let mut c = Counts::default();
        for p in parts {
            let (ch, pc) = p?;
            chunks.extend(ch);
            c.frames += pc.frames;
            c.succs += pc.succs;
            c.forks += pc.forks;
            c.w_not_admitted += pc.w_not_admitted;
            c.admitted_not_w += pc.admitted_not_w;
        }
        chunks.sort_unstable_by_key(|ch| ch.0);
        let mut index: FxHashMap<celeste_engine::exact::ExactRow, u32> = FxHashMap::default();
        let (mut next, mut lk, mut wp) = (Vec::new(), Vec::new(), Vec::new());
        for sc in chunks.into_iter().flat_map(|ch| ch.1) {
            if sc.win {
                wp.push(sc.parent);
                continue;
            }
            let exact = sc.exact.expect("an admitted state keeps its key");
            let n = next.len() as u32;
            let child = *index.entry(exact).or_insert(n);
            if child == n {
                next.push(sc.row.expect("an admitted state keeps its row"));
            }
            lk.push((sc.parent, child));
        }
        lk.sort_unstable();
        lk.dedup();
        wp.sort_unstable();
        wp.dedup();
        eprintln!(
            "[optimal-set] concrete ({}) layer {:3}: {} states -> {} kept, {} with a win; {} frames run, {} successors, {} forked; inside W but not admitted {}, admitted outside W {}; {:.1} s, rss {:.1} GB",
            admit.name(),
            k + 1,
            cur.len(),
            next.len(),
            wp.len(),
            c.frames,
            c.succs,
            c.forks,
            c.w_not_admitted,
            c.admitted_not_w,
            t.elapsed().as_secs_f64(),
            crate::metrics::current_rss_gb()
        );
        total.frames += c.frames;
        total.succs += c.succs;
        total.forks += c.forks;
        total.w_not_admitted += c.w_not_admitted;
        total.admitted_not_w += c.admitted_not_w;
        sizes.push(next.len());
        links.push(lk);
        win_parents.push(wp);
        cur = next;
        if cur.is_empty() {
            break;
        }
        if cur.len() > budget {
            println!("[optimal-set] concrete ({}): STOPPED at layer {} ({} states, over the budget {budget})", admit.name(), k + 1, cur.len());
            stopped = true;
            break;
        }
    }
    // On a winning path: a state with a winning successor, or a link to one.
    let layers = links.len();
    let mut alive: Vec<Vec<bool>> = sizes.iter().map(|&n| vec![false; n]).collect();
    for k in (0..layers).rev() {
        // links[k] / win_parents[k]: from layer k to layer k + 1.
        for &p in &win_parents[k] {
            alive[k][p as usize] = true;
        }
        for &(p, ch) in &links[k] {
            if alive[k + 1][ch as usize] {
                alive[k][p as usize] = true;
            }
        }
    }
    let states: usize = sizes.iter().sum();
    let on_path: usize = alive.iter().map(|a| a.iter().filter(|&&x| x).count()).sum();
    let n_links: usize = links.iter().map(Vec::len).sum::<usize>() + win_parents.iter().map(Vec::len).sum::<usize>();
    println!(
        "[optimal-set] h{h} concrete ({}): {states} states over {} layers (largest {}), {on_path} on a winning path, {n_links} links; {} frames run, {} successors, {} forked by rnd; inside W but not admitted {}, admitted outside W {}; {:.1} s{}",
        admit.name(),
        sizes.len(),
        sizes.iter().max().copied().unwrap_or(0),
        total.frames,
        total.succs,
        total.forks,
        total.w_not_admitted,
        total.admitted_not_w,
        t0.elapsed().as_secs_f64(),
        if stopped { " (STOPPED)" } else { "" }
    );
    let per: Vec<String> = alive.iter().zip(&sizes).map(|(a, n)| format!("{}/{n}", a.iter().filter(|&&x| x).count())).collect();
    println!("[optimal-set] h{h} concrete ({}) on-path/kept per layer: {}", admit.name(), per.join(" "));
    if let Admit::Exact(_) = admit {
        anyhow::ensure!(total.w_not_admitted == 0, "{} concrete states inside W are outside the reached set: the reach is not exact", total.w_not_admitted);
    }
    Ok(())
}

/// What to measure (`rewrite optimal-set`).
pub struct Opts<'a> {
    /// The tree's horizon (search steps): the optimum is read off it.
    pub horizon: u32,
    /// Also the horizons `H* + 1 ..= H* + slack` (within the tree's).
    pub slack: u32,
    /// The concrete breadth-first search inside the exact set.
    pub concrete: bool,
    /// The same inside the time-collapsed set.
    pub collapsed: bool,
    /// The concrete search stops past this many states in a layer.
    pub budget: usize,
    /// Write the exact set (regions form) here, per horizon N with the
    /// extension `.hN`.
    pub write: Option<&'a std::path::Path>,
    /// Print the per-frame profile.
    pub profile: bool,
}

/// Measure the optimal-path set of the tree in `dir` at the optimum read off
/// its horizon, and at up to `slack` frames more.
pub fn run(dir: &std::path::Path, level: crate::abstraction::Level, o: &Opts, s: &crate::settings::Settings) -> Result<()> {
    s.check(dir, level)?;
    anyhow::ensure!(s.steps_per_frame() == 1, "optimal-set: the split frame is not supported");
    let threads = s.threads();
    let p0 = (point(0), point(0));
    // The optimum, off the tree's horizon.
    let star = {
        let Loaded { graph, start, .. } = load(dir, o.horizon, threads)?;
        let w = backward(&graph, o.horizon, threads);
        optimum(&graph, &w, start, p0, o.horizon).map(|f| f.frame)
    };
    let Some(star) = star else {
        println!("[optimal-set] h{}: REFUTED, no optimal set", o.horizon);
        return Ok(());
    };
    println!("[optimal-set] the optimum H* = f{star} (tree horizon f{})", o.horizon);
    // The tree's states per frame.
    let mut tree_layer: Vec<u64> = Vec::new();
    for f in 0..=o.horizon {
        let files = if dir.join("frames").join(format!("f{f:03}")).is_dir() { crate::frame::frame_files(dir, f)? } else { Vec::new() };
        tree_layer.push(files.iter().map(|(_, file)| file.width() as u64).sum());
    }
    for h in star..=(star + o.slack).min(o.horizon) {
        measure(dir, level, h, star, &tree_layer, o, s)?;
    }
    Ok(())
}

fn measure(dir: &std::path::Path, level: crate::abstraction::Level, h: u32, star: u32, tree_layer: &[u64], o: &Opts, s: &crate::settings::Settings) -> Result<()> {
    let threads = s.threads();
    let p0 = (point(0), point(0));
    let t0 = std::time::Instant::now();
    let Loaded { graph, start, marks, edges, .. } = load(dir, h, threads)?;
    let g = &graph;
    let w = backward(g, h, threads);
    let t_arc = t0.elapsed().as_secs_f64();
    let found = optimum(g, &w, start, p0, h).map(|f| f.frame);
    anyhow::ensure!(found == Some(star), "h{h}: the optimum read off it is {found:?}, not {star}");
    let t1 = std::time::Instant::now();
    let r = reach_points(g, &w, start, p0, h, threads)?;
    let t_reach = t1.elapsed().as_secs_f64();
    let hu = h as usize;
    // W's support and the remainder-free marks, per frame.
    let (mut w_nodes, mut w_segs) = (vec![0u64; hu + 1], vec![0u64; hu + 1]);
    let (mut w_pairs, mut w_seg_total, mut w_spans) = (0u64, 0u64, 0u64);
    let mut w_support: FxHashSet<u32> = FxHashSet::default();
    for (i, lo, hi, reg) in w.spans() {
        w_spans += 1;
        w_support.insert(i);
        let segs = reg.n_segs() as u64;
        for t in lo..=hi.min(h) {
            w_nodes[t as usize] += 1;
            w_segs[t as usize] += segs;
            w_pairs += 1;
            w_seg_total += segs;
        }
    }
    // A marked node is live from its layer to its deadline.
    let mut live = vec![0i64; hu + 2];
    for (i, &(_, d)) in marks.iter().enumerate() {
        let lo = g.layer_of(i as u32) as usize;
        let hi = if g.is_win(i as u32) { hu } else { (d as usize).min(hu) };
        if lo <= hi {
            live[lo] += 1;
            live[hi + 1] -= 1;
        }
    }
    for t in 1..live.len() {
        live[t] += live[t - 1];
    }
    // The set, per frame.
    let (mut pieces, mut scratch) = (Vec::new(), Vec::new());
    let mut nodes_all: FxHashSet<u32> = FxHashSet::default();
    let mut edges_all: FxHashSet<(u32, u32, u32)> = FxHashSet::default();
    let mut node_pairs_all: FxHashSet<(u32, u32)> = FxHashSet::default();
    let (mut pairs, mut points, mut segs, mut slabs, mut node_edges, mut point_edges) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    let mut wins_at: Vec<(u32, u64)> = Vec::new();
    let mut max_points_node = 0usize;
    let (mut w_at_set_segs, mut w_at_set_slabs) = (0u64, 0u64);
    let mut rows: Vec<String> = Vec::new();
    let mut regions: Vec<Vec<(u32, Region)>> = Vec::with_capacity(hu + 1);
    for (t, layer) in r.layers.iter().enumerate() {
        let (mut n, mut sg, mut sl, mut wn) = (0u64, 0u64, 0u64, 0u64);
        let mut regs = Vec::new();
        for (i, pts) in groups(layer) {
            n += 1;
            nodes_all.insert(i);
            max_points_node = max_points_node.max(pts.len());
            let reg = region_of(pts, &mut pieces, &mut scratch);
            sg += reg.n_segs() as u64;
            sl += reg.slabs().count() as u64;
            if g.is_win(i) {
                wn += pts.len() as u64;
            }
            // W at the set's node-frames: what a margin (`--robust`) reads.
            if let Some(wr) = w.at(t as u32, i) {
                w_at_set_segs += wr.n_segs() as u64;
                w_at_set_slabs += wr.slabs().count() as u64;
            }
            if o.write.is_some() {
                regs.push((i, reg));
            }
        }
        regions.push(regs);
        if wn > 0 {
            wins_at.push((t as u32, wn));
        }
        let (ne, pe) = match r.used.get(t) {
            Some(u) => {
                for &e in u {
                    edges_all.insert(e);
                    node_pairs_all.insert((e.0, e.1));
                }
                (u.len() as u64, r.point_edges[t])
            }
            None => (0, 0),
        };
        pairs += n;
        points += layer.len() as u64;
        segs += sg;
        slabs += sl;
        node_edges += ne;
        point_edges += pe;
        rows.push(format!(
            "| {t} | {} | {} | {} | {n} | {} | {sg} | {ne} | {pe} | {} | {} | {} |",
            tree_layer.get(t).copied().unwrap_or(0),
            live[t],
            w_nodes[t],
            layer.len(),
            if wn > 0 { wn.to_string() } else { String::new() },
            w_segs[t],
            if n > 0 { format!("{:.2}", layer.len() as f64 / n as f64) } else { String::new() },
        ));
    }
    let collapsed = Collapsed::of(&r);
    let union_points: u64 = collapsed.0.values().map(|c| c.2.len() as u64).sum();
    let collapsed_volume: u64 = collapsed.0.values().map(|c| (c.1 - c.0 + 1) as u64 * c.2.len() as u64).sum();
    let frames_per_node: f64 = pairs as f64 / nodes_all.len().max(1) as f64;
    // Bytes, per encoding: a node is its 8 B content hash in a node table,
    // referenced by a 4 B index; a frame groups its entries (4 B count).
    let table = 8 * nodes_all.len() as u64;
    let frames_hdr = 4 * (hu as u64 + 1);
    let points_form = table + frames_hdr + pairs * (4 + 4) + points * 4;
    let regions_form = table + frames_hdr + pairs * (4 + 2) + slabs * (4 + 2) + segs * 4;
    let collapsed_form = collapsed.0.len() as u64 * (8 + 2 + 2 + 4) + union_points * 4;
    let edges_form = edges_all.len() as u64 * 12;
    let w_at_set_form = pairs * 2 + w_at_set_slabs * (4 + 2) + w_at_set_segs * 4;
    let tree_total: u64 = tree_layer.iter().take(hu + 1).sum();
    let tag = if h == star { "H*".to_string() } else { format!("H*+{}", h - star) };
    println!(
        "[optimal-set] h{h} ({tag}): tree {tree_total} states f0-f{h}; marks {} nodes, {edges} edges; W {} nodes, {w_pairs} (node, frame) pairs, {w_spans} spans, {w_seg_total} segments (node-frames)",
        marks.len(),
        w_support.len()
    );
    println!(
        "[optimal-set] h{h} ({tag}): SET {} nodes, {pairs} (node, frame) pairs ({frames_per_node:.2} frames a node), {points} points (largest {max_points_node} a node-frame), {segs} segments in {slabs} slabs; edges {node_edges} (frame, edge) carrying points, {} distinct (src, dst, transfer), {} distinct (src, dst); {point_edges} point transitions",
        nodes_all.len(),
        edges_all.len(),
        node_pairs_all.len()
    );
    println!(
        "[optimal-set] h{h} ({tag}): wins (frame, points): {}",
        wins_at.iter().map(|(t, n)| format!("f{t}:{n}")).collect::<Vec<_>>().join(" ")
    );
    println!(
        "[optimal-set] h{h} ({tag}): collapsed {} nodes, {union_points} union points, {collapsed_volume} (node, frame, point) admitted vs {points} exact",
        collapsed.0.len()
    );
    println!(
        "[optimal-set] h{h} ({tag}): bytes: points form {points_form}, regions form {regions_form}, collapsed form {collapsed_form}, + node edges {edges_form}, + W at the set's node-frames {w_at_set_form} ({w_at_set_segs} segments in {w_at_set_slabs} slabs); arc {t_arc:.1} s, reach {t_reach:.2} s",
    );
    if o.profile {
        println!("[optimal-set] h{h} profile:");
        println!("| t | tree layer | marks live | W nodes | set nodes | set points | set segs | edges | point edges | win points | W segs | points/node |");
        println!("|---|---|---|---|---|---|---|---|---|---|---|---|");
        for row in rows {
            println!("{row}");
        }
    }
    if let Some(base) = o.write {
        let path = base.with_extension(format!("h{h}"));
        write_regions(&path, dir, h, g, &regions)?;
        println!("[optimal-set] h{h}: wrote {} ({} B)", path.display(), std::fs::metadata(&path)?.len());
    }
    drop(regions);
    if o.concrete || o.collapsed {
        let resolver = crate::storage::marks::Resolver::load(dir, h)?;
        let mut keys = Vec::with_capacity(g.len());
        for (k, &(id, _)) in marks.iter().enumerate() {
            let (shape, key, cell) = resolver.resolve(id)?;
            keys.push((shape, key, cell, k as u32));
        }
        let node = NodeKeys::new(keys);
        drop(marks);
        drop(graph);
        if o.concrete {
            concrete_bfs(level, h, &node, &resolver.space, &w, &Admit::Exact(&r.layers), o.budget, s)?;
        }
        if o.collapsed {
            concrete_bfs(level, h, &node, &resolver.space, &w, &Admit::Collapsed(&collapsed), o.budget, s)?;
        }
    }
    Ok(())
}

/// The exact set in its regions form, raw (to measure the bytes and what a
/// compressor makes of them): a node table (each node's content hash, `u64`,
/// in node order), then per frame the count of its entries and per entry
/// the node's index (`u32`), its slab count (`u16`), and per slab its y
/// segment (`u16` lo, `u16` length - 1), its x segment count (`u16`) and
/// the x segments (`u16` lo, `u16` length - 1). Little-endian.
fn write_regions(path: &std::path::Path, dir: &std::path::Path, h: u32, g: &Graph, regions: &[Vec<(u32, Region)>]) -> Result<()> {
    let resolver = crate::storage::marks::Resolver::load(dir, h)?;
    let content = resolver.space.content_hash();
    let mut nodes: Vec<u32> = regions.iter().flatten().map(|(i, _)| *i).collect();
    nodes.sort_unstable();
    nodes.dedup();
    let mut out: Vec<u8> = Vec::new();
    out.extend_from_slice(&(nodes.len() as u32).to_le_bytes());
    for &i in &nodes {
        let (shape, key, cell) = resolver.resolve(g.id(i))?;
        out.extend_from_slice(&content.state(shape, key, cell).to_le_bytes());
    }
    let u16_of = |v: u32| -> Result<[u8; 2]> { Ok(u16::try_from(v).map_err(|_| anyhow::anyhow!("{v} past u16"))?.to_le_bytes()) };
    for frame in regions {
        out.extend_from_slice(&(frame.len() as u32).to_le_bytes());
        for (i, reg) in frame {
            let k = nodes.binary_search(i).expect("a node of the table") as u32;
            out.extend_from_slice(&k.to_le_bytes());
            out.extend_from_slice(&u16_of(reg.slabs().count() as u32)?);
            for (y, xs) in reg.slabs() {
                out.extend_from_slice(&u16_of(y.lo)?);
                out.extend_from_slice(&u16_of(y.hi - y.lo - 1)?);
                out.extend_from_slice(&u16_of(xs.len() as u32)?);
                for x in xs {
                    out.extend_from_slice(&u16_of(x.lo)?);
                    out.extend_from_slice(&u16_of(x.hi - x.lo - 1)?);
                }
            }
        }
    }
    Ok(std::fs::write(path, out)?)
}
