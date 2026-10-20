//! THE DIAGNOSTICS of a finished tree and of concrete runs: a tree's
//! fingerprints (`ckhash`) and its states counted (`census`), a concrete
//! route that follows given positions (`trajectory`), a float witness
//! against the host libm (`robust`).

use anyhow::Result;
use celeste_engine::runtime2::{Rt2, P8};
use celeste_core::lua_f64::LuaF64;
use celeste_rust::frame::wins_of;
use celeste_rust::search::inspect::cell_names;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::{Arith, Settings};
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use celeste_rust::engines::reference::RefEngine;
use std::path::PathBuf;

use crate::{prefixes, where_, Rows};

#[derive(clap::Args)]
pub struct CkhashArgs {
    /// The tree (a forward's `--tree`, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
    /// The last step fingerprinted (default: the tree's last).
    #[arg(long)]
    steps: Option<u32>,
    /// Also list the N most populated player positions at step `steps`
    /// (to pick a synthetic `--win-at` target with real fan-out).
    #[arg(long)]
    cells: Option<usize>,
    /// Also fingerprint each frame's recorded edges (`e{frame} count
    /// hash`) by the states they join and their transfers: two trees
    /// with the same lines have the same graph, whatever ids they gave
    /// its transfers.
    #[arg(long)]
    edges: bool,
    /// Also fingerprint each frame's FRONTIER (`h{frame} count hash`,
    /// its hull file): every state whose remainder hull grew there, by
    /// its (shape, key, cell), layer and hull.
    #[arg(long)]
    hulls: bool,
    /// With `--edges`: print step S's edges (target, source, transfer) on
    /// stderr, to diff two trees' edges at the step whose line differs.
    #[arg(long)]
    dump_edges: Option<u32>,
}

#[derive(clap::Args)]
pub struct CensusArgs {
    /// The tree (a forward's `--tree`, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
    #[command(flatten)]
    rows: Rows,
    /// Stop counting a column's distinct values past this many.
    #[arg(long, default_value_t = 1 << 24)]
    cap: usize,
    /// Named-cell prefixes left out of a state, comma-separated (e.g.
    /// `spd.,rem.,player[3].dash`).
    #[arg(long)]
    erase: Option<String>,
    /// The saturation every this many steps.
    #[arg(long, default_value_t = 4)]
    every: u32,
    /// The player positions with the most states, this many.
    #[arg(long)]
    cells: Option<usize>,
}

#[derive(clap::Args)]
pub struct TrajectoryArgs {
    /// One line per frame: `x,y`, or `-` (any).
    #[arg(long)]
    trajectory: String,
    /// Stop once a win is reached (default), else follow to the end.
    #[arg(long, default_value_t = true)]
    stop_at_win: bool,
}

#[derive(clap::Args)]
pub struct RobustArgs {
    #[arg(long)]
    inputs: String,
    #[arg(long, default_value_t = 8)]
    random: u64,
}

pub fn ckhash(run: &Settings, a: CkhashArgs) -> Result<()> {
    let t = TreeDir::open(&a.tree, run)?;
    let (dir, to, cfg) = (&t.dir, t.frame(a.steps)?, &t.settings);
    let (top_cells, edges, hulls, dump_edges) = (a.cells, a.edges, a.hulls, a.dump_edges);
    let lines = match cfg.arith {
        Arith::Fixed => celeste_rust::storage::fingerprint::<P8>(dir, to, edges, hulls, cfg.threads())?,
        Arith::Float(_) => {
            anyhow::ensure!(dump_edges.is_none() && top_cells.is_none(), "--dump-edges, --cells: fixed trees only so far");
            celeste_rust::storage::fingerprint::<LuaF64>(dir, to, edges, hulls, cfg.threads())?
        }
    };
    for line in lines {
        println!("{line}");
    }
    let geo = celeste_rust::storage::GEOMETRY;
    if let Some(frame) = dump_edges {
        let tree = Tree::<P8>::open(dir, to)?;
        let content = tree.space.content_hash();
        let eg = celeste_rust::storage::edges::EdgeStore::<P8>::open(&dir.join("edges"), to)?;
        let node = |s: Sk| content.state(tree.shapes[s.shape(&geo) as usize], s.key, s.cell(&geo));
        for e in eg.edges_at(frame, cfg.threads())? {
            eprintln!("edge {:016x} {:016x} {:?}", node(e.dst), node(e.src), eg.pair(e.xfer));
        }
    }
    if let Some(top) = top_cells {
        let tree = Tree::<P8>::open(dir, to)?;
        let mut by_cell: rustc_hash::FxHashMap<u32, usize> = Default::default();
        for s in tree.layer(to, None, cfg.threads())?.0.states_iter() {
            *by_cell.entry(s.cell(&geo)).or_default() += 1;
        }
        let mut v: Vec<(u32, usize)> = by_cell.into_iter().collect();
        v.sort_by_key(|&(c, n)| (std::cmp::Reverse(n), c));
        for (c, n) in v.into_iter().take(top) {
            match celeste_rust::search::pos_graph::cell_xy(c) {
                Some((x, y)) => println!("cell {c}: player ({x},{y}) x{n}"),
                None => println!("cell {c}: no player x{n}"),
            }
        }
    }
    Ok(())
}

pub fn census(run: &Settings, a: CensusArgs) -> Result<()> {
    use celeste_engine::exact::Code;
    use celeste_engine::runtime2::{Cell2, Col, AV};
    let t = TreeDir::open(&a.tree, run)?;
    anyhow::ensure!(!t.float(), "census: a 16.16 tree only so far");
    let cfg = &t.settings;
    let (frame, only) = a.rows.of(&t)?;
    let tree = Tree::<P8>::open(&t.dir, frame)?;
    let ids = celeste_rust::compiled::ids();
    let geo = celeste_rust::storage::GEOMETRY;
    let keep = |s: Sk| only.is_none_or(|c| s.cell(&geo) == c);
    // Per shape: its rows, per column its distinct codes (capped), whether
    // it varies, the rows holding an interval or an unknown bool, the named
    // cells and the distinct (spd.x, spd.y) pairs.
    struct S {
        rows: u64,
        cols: Vec<rustc_hash::FxHashSet<Code>>,
        varying: Vec<bool>,
        ivals: Vec<u64>,
        ubools: Vec<u64>,
        names: std::collections::HashMap<usize, String>,
        spd: rustc_hash::FxHashSet<(Code, Code)>,
    }
    let mut by_shape: std::collections::BTreeMap<u64, S> = Default::default();
    for (_, rt2) in tree.rows(frame, cfg.room, 1 << 20, &keep, cfg.threads())? {
        let n = rt2.cols.len();
        let st = by_shape.entry(rt2.shape_hash).or_insert_with(|| S { rows: 0, cols: vec![Default::default(); n], varying: vec![false; n], ivals: vec![0; n], ubools: vec![0; n], names: cell_names(&rt2, ids), spd: Default::default() });
        st.rows += rt2.width as u64;
        for (c, col) in rt2.cols.iter().enumerate() {
            if !matches!(rt2.structure[c], Cell2::Val) {
                continue;
            }
            if let Col::U(v) = col {
                if st.cols[c].len() < a.cap {
                    st.cols[c].insert(Code::of(*v));
                }
                continue;
            }
            st.varying[c] = true;
            for r in 0..rt2.width {
                let v = col.at(r);
                match v {
                    AV::Ival(lo, hi) if lo != hi => st.ivals[c] += 1,
                    AV::UBool => st.ubools[c] += 1,
                    _ => {}
                }
                if st.cols[c].len() < a.cap {
                    st.cols[c].insert(Code::of(v));
                }
            }
        }
        let named = |want: &str| st.names.iter().find(|(_, n)| n.as_str() == want).map(|(c, _)| *c);
        if let (Some(sx), Some(sy)) = (named("spd.x"), named("spd.y")) {
            for r in 0..rt2.width {
                if st.spd.len() >= a.cap {
                    break;
                }
                st.spd.insert((Code::of(rt2.cols[sx].at(r)), Code::of(rt2.cols[sy].at(r))));
            }
        }
    }
    for (shape, st) in &by_shape {
        let mut cards: Vec<(usize, usize)> = (0..st.cols.len()).filter(|&c| st.cols[c].len() > 1 || st.varying[c]).map(|c| (st.cols[c].len(), c)).collect();
        cards.sort_unstable_by(|a, b| b.cmp(a));
        let product: f64 = cards.iter().map(|&(n, _)| n as f64).product();
        println!("shape {shape:#x}: {} rows, {} varying columns, cardinality product {:.2e}, spd pairs {}", st.rows, cards.len(), product, st.spd.len());
        for &(n, c) in cards.iter().take(24) {
            println!("  c{c:<4} {n:>10} {:<18} intervals {:>10} unknown-bools {:>10}", st.names.get(&c).map_or("", |s| s.as_str()), st.ivals[c], st.ubools[c]);
            // One position's census lists the numbers themselves.
            if only.is_some() && n <= 64 {
                let mut vals: Vec<f64> = st.cols[c].iter().filter(|v| v.kind == 0 && v.a == v.b).map(|v| v.a as i32 as f64 / 65536.0).collect();
                vals.sort_by(|a, b| a.total_cmp(b));
                if !vals.is_empty() {
                    println!("        {}", vals.iter().map(|v| format!("{v:.4}")).collect::<Vec<_>>().join(" "));
                }
            }
        }
    }
    // SATURATION: the distinct states with the erased cells left out, frame
    // by frame (pointers count as structure).
    if a.erase.is_some() || only.is_some() {
        let erase = prefixes(a.erase.as_deref().unwrap_or(""));
        let code = |v: AV| -> Code { Code::of(if let AV::Ptr(_) = v { AV::Ptr(0) } else { v }) };
        let mut through: rustc_hash::FxHashSet<Box<[u8]>> = Default::default();
        println!("frame | new | distinct new | distinct through");
        for f in 0..=frame {
            let (mut new, mut here) = (0u64, rustc_hash::FxHashSet::<Box<[u8]>>::default());
            for (_, rt2) in tree.rows(f, cfg.room, 1 << 20, &keep, cfg.threads())? {
                new += rt2.width as u64;
                let names = cell_names(&rt2, ids);
                let mut cells: Vec<(String, usize)> = (0..rt2.cols.len())
                    .filter(|&c| matches!(rt2.structure[c], Cell2::Val))
                    .map(|c| (names.get(&c).cloned().unwrap_or_else(|| format!("cell{c}")), c))
                    .filter(|(nm, _)| !erase.iter().any(|p| nm.starts_with(p.as_str())))
                    .collect();
                cells.sort();
                for r in 0..rt2.width {
                    let mut b: Vec<u8> = Vec::new();
                    for (nm, c) in &cells {
                        let k = code(rt2.cols[*c].at(r));
                        b.extend_from_slice(nm.as_bytes());
                        b.push(0);
                        b.push(k.kind);
                        b.extend_from_slice(&k.a.to_le_bytes());
                        b.extend_from_slice(&k.b.to_le_bytes());
                    }
                    here.insert(b.into_boxed_slice());
                }
            }
            let distinct = here.len();
            through.extend(here);
            if !through.is_empty() && (f % a.every.max(1) == 0 || f == frame) {
                println!("f{f:03} | {new:>9} | {distinct:>9} | {:>9}", through.len());
            }
        }
    }
    // THE POSITIONS: per player cell, its new states per frame.
    if let Some(top) = a.cells {
        let n_frames = frame as usize + 1;
        let mut per: rustc_hash::FxHashMap<u32, Vec<u64>> = Default::default();
        for f in 0..=frame {
            for s in tree.layer(f, None, cfg.threads())?.0.states_iter() {
                per.entry(s.cell(&geo)).or_insert_with(|| vec![0; n_frames])[f as usize] += 1;
            }
        }
        let total = |v: &[u64]| v.iter().sum::<u64>();
        let mut by_total: Vec<(u32, u64)> = per.iter().map(|(c, v)| (*c, total(v))).collect();
        by_total.sort_by_key(|&(c, n)| (std::cmp::Reverse(n), c));
        let shown = n_frames.min(12);
        println!("position: states through f{:03} | new per frame, f{:03}..f{:03}", frame, n_frames - shown, frame);
        for &(c, n) in by_total.iter().take(top) {
            let tail: Vec<String> = per[&c][n_frames - shown..].iter().map(|n| n.to_string()).collect();
            println!("{}: {n} | {}", where_(c), tail.join(" "));
        }
        // Every position's new states at the frame as a share of its states.
        let edges = [0.01, 0.05, 0.20];
        let (mut cells_in, mut states_in) = ([0usize; 4], [0u64; 4]);
        for v in per.values() {
            let share = v[n_frames - 1] as f64 / total(v).max(1) as f64;
            let b = edges.iter().filter(|&&e| share >= e).count();
            cells_in[b] += 1;
            states_in[b] += total(v);
        }
        println!(
            "{} positions; new at f{:03} as a share of their states: under 1%: {} positions ({} states), 1-5%: {} ({}), 5-20%: {} ({}), 20% or more: {} ({})",
            per.len(),
            frame,
            cells_in[0],
            states_in[0],
            cells_in[1],
            states_in[1],
            cells_in[2],
            states_in[2],
            cells_in[3],
            states_in[3]
        );
    }
    Ok(())
}

/// `trajectory`, under the run's settings `cfg`.
pub fn trajectory(cfg: &Settings, a: TrajectoryArgs) -> Result<()> {
    let TrajectoryArgs { trajectory, stop_at_win } = a;
    let traj: Vec<Option<(i32, i32)>> = std::fs::read_to_string(&trajectory)?
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| -> Result<Option<(i32, i32)>> {
            if l == "-" {
                return Ok(None);
            }
            let (x, y) = l.split_once(',').ok_or_else(|| anyhow::anyhow!("{trajectory}: {l:?} is not \"x,y\" or \"-\""))?;
            Ok(Some((x.trim().parse()?, y.trim().parse()?)))
        })
        .collect::<Result<_>>()?;
    let mut eng = RefEngine::new(&cfg)?;
    // (state, the inputs that led to it)
    let mut layer: Vec<(Rt2, Vec<u8>)> = vec![(eng.initial()?, Vec::new())];
    for (i, want) in traj.iter().enumerate() {
        let f = i as u32 + 1;
        let mut next: Vec<(Rt2, Vec<u8>)> = Vec::new();
        let mut seen: rustc_hash::FxHashSet<celeste_engine::exact::ExactRow> = Default::default();
        let mut tried = 0usize;
        // Where the successors went: reported when none is at `want`.
        let mut reached: std::collections::BTreeMap<Option<(i32, i32)>, usize> = Default::default();
        // A `-` frame (the spawn) takes only the idle input: inputs
        // before the player exists cannot matter.
        let inputs: std::ops::Range<u8> = if want.is_some() { 0..64 } else { 0..1 };
        for (st, path) in &layer {
            for byte in inputs.clone() {
                // Every leaf at the wanted position is a candidate;
                // the real-PICO-8 replay settles it.
                for block in eng.step(st, byte)? {
                    tried += 1;
                    let cell = celeste_rust::search::pos_graph::block_cells(&block)?[0];
                    let pos = celeste_rust::search::pos_graph::cell_xy(cell);
                    if let Some(w) = want {
                        if pos != Some(*w) {
                            *reached.entry(pos).or_default() += 1;
                            continue;
                        }
                    }
                    // Deduplicated on the EXACT state.
                    let mut exact = celeste_engine::exact::exact_rows(&block, celeste_rust::compiled::ids()).swap_remove(0).into_vec();
                    exact.extend_from_slice(&cell.to_le_bytes());
                    if !seen.insert(exact.into_boxed_slice()) {
                        continue;
                    }
                    let mut p = path.clone();
                    p.push(byte);
                    if stop_at_win && wins_of(&block, &cfg)?.iter().any(|&w| w) {
                        println!("[trajectory] WIN at f{f} after {} frames; inputs:", p.len());
                        println!("{}", p.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
                        return Ok(());
                    }
                    next.push((block, p));
                }
            }
        }
        eprintln!(
            "[trajectory] f{f:03} want {:?}: {} states ({tried} steps)",
            want,
            next.len()
        );
        if next.is_empty() {
            eprintln!("[trajectory] f{f}: the successors reached {} positions instead:", reached.len());
            for (pos, n) in &reached {
                eprintln!("[trajectory]   {pos:?}: {n}");
            }
            anyhow::bail!("the trajectory cannot be followed at f{f} (wanted {:?})", want);
        }
        layer = next;
    }
    println!("[trajectory] followed to the end: {} states; one input sequence:", layer.len());
    println!("{}", layer[0].1.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
    Ok(())
}

/// `robust`, under the run's settings `cfg`.
pub fn robust(cfg: &Settings, a: RobustArgs) -> Result<()> {
    anyhow::ensure!(matches!(cfg.arith, Arith::Float(_)), "rewrite robust: a float witness (--arith float)");
    let bytes = celeste_rust::concrete::read_inputs(&a.inputs)?;
    let rows = celeste_rust::search::robust::report(&bytes, cfg, a.random)?;
    let base = rows[0].1;
    let fragile: Vec<&str> = rows.iter().filter(|r| r.1 != base).map(|r| r.0.as_str()).collect();
    for (name, w) in &rows {
        println!("[robust] {name}: first win {w:?}{}", if *w == base { "" } else { "  <- DIFFERS" });
    }
    if fragile.is_empty() {
        println!("[robust] ROBUST: every variant wins on glibc's frame ({} variants)", rows.len() - 1);
    } else {
        println!("[robust] FRAGILE: {} of {} variants differ ({})", fragile.len(), rows.len() - 1, fragile.join(", "));
    }
    Ok(())
}
