//! THE DUAL SEARCH (plans/float-solver.md sections 10 and 15): the shortest
//! input file valid in BOTH arithmetics, under Philippe's rule (b): the
//! file's length is the FLOAT exit (Celia's float mode, the count), and real
//! play (16.16, PICO-8) must have left the room by then. Two halves, each a
//! finished arc phase of its own arithmetic to the same horizon (`Arcs`),
//! each with its own start (real play's loading jank; the float tool's).
//!
//! A DUAL STATE is (the fixed half's exact state, or EXITED; the float
//! half's exact state). Under an input both halves step through their
//! reference engines (every `rnd` leaf of each, paired), and a successor is
//! admitted iff every live half lies in ITS OWN winning sets at that frame.
//! A fixed half that wins is EXITED from then on (only the float half
//! steps); a float half that wins is the dual win if the fixed half has
//! exited by then, and is pruned otherwise (the file would end before real
//! play leaves).
//!
//! Sound: a both-valid run is valid in each arithmetic, so each half stays
//! inside its W, and nothing else prunes - the search is exhaustive inside
//! both W. Dedup and the depth-first try's memo are on the FULL dual state
//! (both exact rows and the exit flag), never a projection (`522de36`). An
//! input is skipped as equivalent only where it agrees with one already run
//! on the buttons BOTH halves read. The bound is the larger of the two arc
//! bounds (the float exit is at least the float's, and at least the fixed
//! exit, itself at least the fixed's), so the breadth-first layers' first
//! win is the optimal dual run, and none by the horizon refutes every count
//! up to it.

use anyhow::{anyhow, ensure, Result};
use rustc_hash::FxHashSet;

use celeste_engine::exact::ExactRow;
use celeste_engine::runtime2::Rt2;

use super::arc_dp::{concrete_engines, exact_state, input_order, lookup_keys, rem_of, Arcs, Covered, NodeKeys};
use super::rem::Rem;
use crate::frame::{wins_of, Block};
use crate::settings::Settings;
use crate::trace::refengine::ReferenceEngine;

/// One half: its arcs, the settings its engines run under, and how its
/// states key (`seeded`: `--concrete-balloon-seeds`).
pub struct Half<'a, N: Rem> {
    pub arcs: &'a Arcs<N>,
    pub settings: &'a Settings,
}

/// A half as the search reads it.
struct Side<'a, N: Rem> {
    arcs: &'a Arcs<N>,
    node: NodeKeys<'a, N::C>,
    s: &'a Settings,
    seeded: bool,
}

/// What a successor of one half is at a frame.
enum Fate {
    /// It wins (the room's win, `wins_of`).
    Win,
    /// A node of the half's tree whose W at the frame holds its remainder.
    Inside,
    /// Neither: no win by the horizon from it in this half.
    Out,
}

impl<N: Rem> Side<'_, N> {
    /// `b`'s fate against W at step `t`.
    fn fate(&self, b: &Block<N>, t: u32) -> Result<Fate> {
        if wins_of(b.rt2(), self.s)?.iter().any(|&x| x) {
            return Ok(Fate::Win);
        }
        let (shape, nk, cells) = lookup_keys(b, self.arcs.level, self.seeded, &self.arcs.space)?;
        let Some(i) = self.node.get(shape, nk[0], cells[0]) else { return Ok(Fate::Out) };
        let q = rem_of::<N>(b.rt2())?;
        Ok(if self.arcs.w.at(t, i).is_some_and(|r| r.contains(q.0, q.1)) { Fate::Inside } else { Fate::Out })
    }
}

/// A dual state: the fixed half's row (`None`: it has exited) and the
/// float half's.
struct Pair<X: Rem, F: Rem> {
    fixed: Option<Rt2<X>>,
    float: Rt2<F>,
}

/// A dual state's identity: both halves' exact rows (the fixed half's
/// `None` once it has exited) and cells.
type Key = (Option<ExactRow>, ExactRow);

/// A dual successor of one pair under one input.
enum Next<X: Rem, F: Rem> {
    /// The float half wins with the fixed half exited (at this frame or
    /// before): the dual win. `fixed_won`: the fixed half exits at this
    /// frame (else earlier).
    Win { fixed_won: bool },
    /// Both live halves inside their W (the fixed half maybe exited now:
    /// `fixed_won`).
    Pair { pair: Pair<X, F>, key: Key, fixed_won: bool },
}

/// The dual successors of `p` under `byte`, admitted at W step `t`, and
/// the buttons either half's frame read.
fn successors<X: Rem, F: Rem>(
    ex: &mut ReferenceEngine<X::A>,
    ef: &mut ReferenceEngine<F::A>,
    sx: &Side<X>,
    sf: &Side<F>,
    p: &Pair<X, F>,
    byte: u8,
    t: u32,
    steps: &mut u64,
) -> Result<(Vec<Next<X, F>>, u8)> {
    let (fl, read_f) = ef.frame_reads(&p.float, byte)?;
    // The fixed half's admitted outcomes: `None` exited (now: `true`).
    let mut fixed: Vec<(Option<(Rt2<X>, ExactRow)>, bool)> = Vec::new();
    let read_x = match &p.fixed {
        None => {
            fixed.push((None, false));
            0
        }
        Some(row) => {
            let (succ, read) = ex.frame_reads(row, byte)?;
            for b in succ {
                *steps += 1;
                match sx.fate(&b, t)? {
                    // Exited: one entry, whichever leaf.
                    Fate::Win if fixed.iter().any(|(x, _)| x.is_none()) => {}
                    Fate::Win => fixed.push((None, true)),
                    Fate::Inside => {
                        let cell = b.positions()?[0];
                        let exact = exact_state(&b, cell);
                        fixed.push((Some((b.into_rt2(), exact)), false));
                    }
                    Fate::Out => {}
                }
            }
            read
        }
    };
    let mut out = Vec::new();
    if fixed.is_empty() {
        return Ok((out, read_f | read_x));
    }
    for b in fl {
        *steps += 1;
        match sf.fate(&b, t)? {
            Fate::Out => {}
            Fate::Win => {
                // Rule (b): the float exit counts only after real play's.
                if let Some(&(_, fixed_won)) = fixed.iter().find(|(x, _)| x.is_none()) {
                    out.push(Next::Win { fixed_won });
                }
            }
            Fate::Inside => {
                let cell = b.positions()?[0];
                let exact = exact_state(&b, cell);
                let row = b.into_rt2();
                for (x, fixed_won) in &fixed {
                    let (fixed_row, fixed_key) = match x {
                        Some((r, e)) => (Some(r.clone_block()), Some(e.clone())),
                        None => (None, None),
                    };
                    out.push(Next::Pair { pair: Pair { fixed: fixed_row, float: row.clone_block() }, key: (fixed_key, exact.clone()), fixed_won: *fixed_won });
                }
            }
        }
    }
    Ok((out, read_f | read_x))
}

/// The optimal dual run's inputs and the frame the fixed half exits.
pub struct DualWitness {
    /// The file: one input a frame, the float half exiting on the last.
    pub inputs: Vec<u8>,
    /// The frame (1-based, `<= inputs.len()`) real play leaves on.
    pub fixed_exit: u32,
}

impl DualWitness {
    /// The inputs as a comma list.
    pub fn inputs_text(&self) -> String {
        self.inputs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",")
    }
}

/// THE DUAL SEARCH between `bound` (the larger arc bound, frames) and the
/// halves' common horizon: the depth-first try at the bound, then the
/// breadth-first layers (module doc). `prefer` orders the inputs (a known
/// file first). `None`: no dual run by the horizon.
pub fn search<X: Rem, F: Rem>(fixed: &Half<X>, float: &Half<F>, bound: u32, prefer: Option<&[u8]>) -> Result<Option<DualWitness>> {
    let horizon = fixed.arcs.horizon;
    ensure!(float.arcs.horizon == horizon, "the halves' horizons differ ({horizon} and {} steps)", float.arcs.horizon);
    let spf = fixed.settings.steps_per_frame();
    ensure!(float.settings.steps_per_frame() == spf, "the halves step frames differently");
    ensure!(horizon % spf == 0, "the horizon {horizon} is not a whole number of frames ({spf} steps each)");
    let frames = horizon / spf;
    ensure!(bound <= frames, "the bound f{bound} is past the horizon f{frames}");
    let workers = fixed.settings.threads().max(1);
    let (ex, seeded_x) = concrete_engines::<X>(workers, fixed.settings)?;
    let (ef, seeded_f) = concrete_engines::<F>(workers, float.settings)?;
    let sx = Side { arcs: fixed.arcs, node: NodeKeys::new(&fixed.arcs.w, &fixed.arcs.shapes)?, s: fixed.settings, seeded: seeded_x };
    let sf = Side { arcs: float.arcs, node: NodeKeys::new(&float.arcs.w, &float.arcs.shapes)?, s: float.settings, seeded: seeded_f };
    let start = Pair { fixed: Some(Block::canonical(ex[0].initial()?)?.into_rt2()), float: Block::canonical(ef[0].initial()?)?.into_rt2() };
    let engines: Vec<std::sync::Mutex<(ReferenceEngine<X::A>, ReferenceEngine<F::A>)>> = ex.into_iter().zip(ef).map(std::sync::Mutex::new).collect();
    let t0 = std::time::Instant::now();
    eprintln!("[dual] W {} fixed and {} float nodes; {workers} workers; the bound f{bound}", fixed.arcs.w.len(), float.arcs.w.len());

    // THE FAST PATH: depth-first at the bound, inside `W_{H - bound + k}`
    // of both halves, on one engine pair; a win here is optimal.
    {
        struct Dfs<'a, X: Rem, F: Rem> {
            ex: &'a mut ReferenceEngine<X::A>,
            ef: &'a mut ReferenceEngine<F::A>,
            sx: &'a Side<'a, X>,
            sf: &'a Side<'a, F>,
            from: u32,
            frames: u32,
            spf: u32,
            dead: FxHashSet<(Key, u32)>,
            path: Vec<u8>,
            fixed_exit: Option<u32>,
            steps: u64,
            prefer: Option<&'a [u8]>,
        }
        /// `Ok(None)`: out of budget.
        fn dfs<X: Rem, F: Rem>(cx: &mut Dfs<X, F>, p: &Pair<X, F>, k: u32) -> Result<Option<bool>> {
            if k >= cx.frames {
                return Ok(Some(false));
            }
            let mut ran = Covered::default();
            for byte in input_order(cx.prefer, k) {
                if ran.covers(byte) {
                    continue;
                }
                let t = cx.spf * (cx.from + k + 1);
                let (next, read) = successors(cx.ex, cx.ef, cx.sx, cx.sf, p, byte, t, &mut cx.steps).map_err(|e| e.context(format!("the frame after {k} frames of the depth-first try, with input {byte}")))?;
                ran.push(byte, read);
                if cx.steps > DFS_BUDGET {
                    return Ok(None);
                }
                for n in next {
                    match n {
                        Next::Win { fixed_won } => {
                            ensure!(k + 1 == cx.frames, "a dual win at f{} before the bound f{}: a half's backward lost a path", k + 1, cx.frames);
                            cx.path.push(byte);
                            if fixed_won {
                                cx.fixed_exit = Some(k + 1);
                            }
                            return Ok(Some(true));
                        }
                        Next::Pair { pair, key, fixed_won } => {
                            let memo = (key, k + 1);
                            if cx.dead.contains(&memo) {
                                continue;
                            }
                            cx.path.push(byte);
                            match dfs(cx, &pair, k + 1)? {
                                Some(true) => {
                                    if fixed_won {
                                        cx.fixed_exit.get_or_insert(k + 1);
                                    }
                                    return Ok(Some(true));
                                }
                                None => return Ok(None),
                                Some(false) => {}
                            }
                            cx.path.pop();
                            cx.dead.insert(memo);
                        }
                    }
                }
            }
            Ok(Some(false))
        }
        /// Concrete steps (both halves') the fast path may take.
        const DFS_BUDGET: u64 = 400_000;
        let t = std::time::Instant::now();
        let mut pair = engines[0].lock().expect("an engine pair");
        let (ex0, ef0) = &mut *pair;
        let mut cx = Dfs { ex: ex0, ef: ef0, sx: &sx, sf: &sf, from: frames - bound, frames: bound, spf, dead: FxHashSet::default(), path: Vec::new(), fixed_exit: None, steps: 0, prefer };
        let found = dfs(&mut cx, &start, 0)?;
        eprintln!(
            "[dual] depth-first at the bound f{bound}: {} after {} steps, {:.1} s",
            match found {
                Some(true) => "a win",
                Some(false) => "no win (exhaustive)",
                None => "out of budget",
            },
            cx.steps,
            t.elapsed().as_secs_f64()
        );
        if found == Some(true) {
            let fixed_exit = cx.fixed_exit.ok_or_else(|| anyhow!("a dual win whose fixed half never exited"))?;
            return Ok(Some(DualWitness { inputs: cx.path, fixed_exit }));
        }
    }

    // Per layer k >= 1, each pair's (parent index in layer k-1, input, the
    // fixed half exiting at this frame).
    let mut back: Vec<Vec<(u32, u8, bool)>> = Vec::new();
    let mut cur: Vec<Pair<X, F>> = vec![start];
    let mut steps = 0u64;
    /// A successor in (parent, input, leaf) order within its chunk.
    struct Succ<X: Rem, F: Rem> {
        parent: u32,
        byte: u8,
        fixed_won: bool,
        /// `None`: the dual win.
        next: Option<(Pair<X, F>, Key)>,
    }
    for k in 0..frames {
        let t = std::time::Instant::now();
        let t_w = spf * (k + 1);
        const CHUNK: usize = 16;
        let next_chunk = std::sync::atomic::AtomicUsize::new(0);
        let parts: Vec<Result<(Vec<(usize, Vec<Succ<X, F>>)>, u64)>> = std::thread::scope(|sc| {
            let hs: Vec<_> = engines
                .iter()
                .map(|engine| {
                    let (cur, next_chunk, sx, sf) = (&cur, &next_chunk, &sx, &sf);
                    sc.spawn(move || -> Result<(Vec<(usize, Vec<Succ<X, F>>)>, u64)> {
                        let mut pair = engine.lock().expect("an engine pair");
                        let (ex, ef) = &mut *pair;
                        let (mut out, mut steps) = (Vec::new(), 0u64);
                        loop {
                            let lo = next_chunk.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cur.len() {
                                return Ok((out, steps));
                            }
                            let mut got = Vec::new();
                            let mut chunk_seen: FxHashSet<Key> = FxHashSet::default();
                            'chunk: for (pi, p) in cur.iter().enumerate().skip(lo).take(CHUNK) {
                                let mut ran = Covered::default();
                                for byte in input_order(prefer, k) {
                                    if ran.covers(byte) {
                                        continue;
                                    }
                                    let (next, read) = successors(ex, ef, sx, sf, p, byte, t_w, &mut steps).map_err(|e| e.context(format!("the frame after layer {k} with input {byte}")))?;
                                    ran.push(byte, read);
                                    for n in next {
                                        match n {
                                            Next::Win { fixed_won } => {
                                                // The chunk ends at its first win: the
                                                // earliest (chunks merge in order) ends the search.
                                                got.push(Succ { parent: pi as u32, byte, fixed_won, next: None });
                                                break 'chunk;
                                            }
                                            Next::Pair { pair, key, fixed_won } => {
                                                if chunk_seen.insert(key.clone()) {
                                                    got.push(Succ { parent: pi as u32, byte, fixed_won, next: Some((pair, key)) });
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            out.push((lo, got));
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a dual worker panicked")).collect()
        });
        let mut chunks: Vec<(usize, Vec<Succ<X, F>>)> = Vec::new();
        for p in parts {
            let (c, s) = p?;
            chunks.extend(c);
            steps += s;
        }
        chunks.sort_unstable_by_key(|c| c.0);
        // Each dual state once, in order; the first win ends the search.
        let mut seen: FxHashSet<Key> = FxHashSet::default();
        let (mut next, mut links): (Vec<Pair<X, F>>, Vec<(u32, u8, bool)>) = (Vec::new(), Vec::new());
        let mut won: Option<(u32, u8, bool)> = None;
        for s in chunks.into_iter().flat_map(|c| c.1) {
            match s.next {
                None => {
                    won = Some((s.parent, s.byte, s.fixed_won));
                    break;
                }
                Some((pair, key)) => {
                    if seen.insert(key) {
                        next.push(pair);
                        links.push((s.parent, s.byte, s.fixed_won));
                    }
                }
            }
        }
        let exited = next.iter().filter(|p| p.fixed.is_none()).count();
        eprintln!(
            "[dual] layer {:3}: {} pairs -> {} inside both W ({exited} with real play exited){}; {steps} steps so far, {:.1} s, rss {:.1} GB",
            k + 1,
            cur.len(),
            next.len(),
            if won.is_some() { ", A DUAL WIN" } else { "" },
            t.elapsed().as_secs_f64(),
            crate::metrics::current_rss_gb()
        );
        if let Some((parent, byte, fixed_won)) = won {
            ensure!(k + 1 >= bound, "a dual win at f{} before the bound f{bound}: a half's backward lost a path", k + 1);
            // The path back through the layers, and the frame the fixed
            // half exited on (the earliest link that says so).
            let mut inputs = vec![byte];
            let mut fixed_exit = fixed_won.then_some(k + 1);
            let mut p = parent;
            for (layer, links) in back.iter().enumerate().rev() {
                let (pp, b, w) = links[p as usize];
                inputs.push(b);
                if w {
                    fixed_exit = Some(layer as u32 + 1);
                }
                p = pp;
            }
            inputs.reverse();
            let fixed_exit = fixed_exit.ok_or_else(|| anyhow!("a dual win whose fixed half never exited"))?;
            eprintln!("[dual] a win at f{} (real play leaves at f{fixed_exit}) after {steps} concrete steps, {:.1} s", inputs.len(), t0.elapsed().as_secs_f64());
            return Ok(Some(DualWitness { inputs, fixed_exit }));
        }
        if next.is_empty() {
            println!("NO DUAL WIN by f{frames}: no dual state inside both W past layer {k} ({steps} steps)");
            return Ok(None);
        }
        back.push(links);
        cur = next;
    }
    println!("NO DUAL WIN by f{frames} ({steps} steps)");
    Ok(None)
}

/// The first frame some lineage of `inputs` wins on, from `s`'s start, as
/// a concrete run (`Settings::concrete`: a float tool's seeds), every `rnd`
/// leaf followed; `None`: none by the inputs' end.
pub fn first_win<N: Rem>(inputs: &[u8], s: &Settings) -> Result<Option<u32>> {
    let mut eng = ReferenceEngine::<N::A>::new(&s.concrete().0)?;
    let mut cur: Vec<Rt2<N>> = vec![eng.initial()?];
    for (f, &byte) in inputs.iter().enumerate() {
        let mut next: Vec<Rt2<N>> = Vec::new();
        let mut seen: FxHashSet<ExactRow> = FxHashSet::default();
        for row in &cur {
            for b in eng.frame(row, byte)? {
                if wins_of(b.rt2(), s)?.iter().any(|&x| x) {
                    return Ok(Some(f as u32 + 1));
                }
                let cell = b.positions()?[0];
                if seen.insert(exact_state(&b, cell)) {
                    next.push(b.into_rt2());
                }
            }
        }
        cur = next;
    }
    Ok(None)
}

/// THE WITNESS, REPLAYED (a guard, not trusted): `inputs` from each half's
/// start through its reference engine, every `rnd` leaf followed. The float
/// half must first win on the last frame (the file's length); the fixed half
/// must win by `fixed_exit` (some leaf: real play's `rnd` is the player's
/// luck, the stated best case). Each half is one run of its own tool, so
/// each is checked on its own.
pub fn replay<X: Rem, F: Rem>(w: &DualWitness, fixed: &Settings, float: &Settings) -> Result<()> {
    let c = w.inputs.len() as u32;
    let f = first_win::<F>(&w.inputs, float)?;
    ensure!(f == Some(c), "the dual witness replayed: the float half first wins at {f:?}, not f{c}");
    let x = first_win::<X>(&w.inputs, fixed)?;
    ensure!(x.is_some_and(|x| x <= w.fixed_exit), "the dual witness replayed: the fixed half first wins at {x:?}, not by f{} as the search found", w.fixed_exit);
    Ok(())
}
