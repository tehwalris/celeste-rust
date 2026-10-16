//! THE DUAL SEARCH (plans/float.md, "The dual search and rule (b)"): the shortest
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

use super::driver::Arcs;
use super::concrete::{concrete_engines, dfs_budget, exact_state, Concrete, Next, NodeKeys, Side};
use super::rem::Rem;
use crate::frame::{wins_of, Block};
use crate::settings::Settings;
use crate::engines::reference::ReferenceEngine;

/// One half: its arcs, and the settings its engines run under.
pub struct Half<'a, N: Rem> {
    pub arcs: &'a Arcs<N>,
    pub settings: &'a Settings,
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

/// `b`'s fate in half `side` against W at step `t`.
fn fate<N: Rem>(side: &Side<N>, b: &Block<N>, t: u32) -> Result<Fate> {
    Ok(if side.wins(b)? {
        Fate::Win
    } else if side.inside(b, t)? {
        Fate::Inside
    } else {
        Fate::Out
    })
}

/// A dual state: the fixed half's row (`None`: it has exited) and the
/// float half's.
pub(super) struct Pair<X: Rem, F: Rem> {
    fixed: Option<Rt2<X>>,
    float: Rt2<F>,
}

/// A dual state's identity: both halves' exact rows (the fixed half's
/// `None` once it has exited) and cells.
type Key = (Option<ExactRow>, ExactRow);

/// The dual search's states for `concrete::search`: a state a `Pair`, its
/// identity both halves' exact rows, an edge's label whether real play
/// exits at it.
struct Pairs<'a, X: Rem, F: Rem> {
    sx: Side<'a, X>,
    sf: Side<'a, F>,
}

impl<X: Rem, F: Rem> Concrete for Pairs<'_, X, F> {
    type Engine = (ReferenceEngine<X::A>, ReferenceEngine<F::A>);
    type State = Pair<X, F>;
    type Key = Key;
    type Label = bool;
    const NAME: &'static str = "dual";

    fn engines(&self, n: usize) -> Result<Vec<Self::Engine>> {
        let (ex, _) = concrete_engines::<X>(n, self.sx.s)?;
        let (ef, _) = concrete_engines::<F>(n, self.sf.s)?;
        Ok(ex.into_iter().zip(ef).collect())
    }

    fn start(&self, (ex, ef): &mut Self::Engine) -> Result<(Pair<X, F>, bool)> {
        Ok((Pair { fixed: Some(Block::canonical(ex.initial()?)?.into_rt2()), float: Block::canonical(ef.initial()?)?.into_rt2() }, false))
    }

    /// Both halves step under `byte` (every `rnd` leaf of each, paired); a
    /// successor is admitted iff every live half lies in its own W at `t`.
    /// A fixed half that wins is EXITED from then on; a float half that wins
    /// is the dual win if the fixed half has exited by then (rule (b)).
    fn successors(&self, (ex, ef): &mut Self::Engine, p: &Pair<X, F>, byte: u8, t: u32, _: &dyn Fn(&Key) -> bool, steps: &mut u64) -> Result<(Vec<Next<Self>>, u8)> {
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
                    match fate(&self.sx, &b, t)? {
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
            match fate(&self.sf, &b, t)? {
                Fate::Out => {}
                Fate::Win => {
                    // Rule (b): the float exit counts only after real play's.
                    if let Some(&(_, fixed_won)) = fixed.iter().find(|(x, _)| x.is_none()) {
                        out.push(Next::Win(fixed_won));
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
                        out.push(Next::State(Pair { fixed: fixed_row, float: row.clone_block() }, (fixed_key, exact.clone()), *fixed_won));
                    }
                }
            }
        }
        Ok((out, read_f | read_x))
    }

    /// A pair's steps count both halves' leaves: twice a single search's.
    fn budget(&self) -> u64 {
        2 * dfs_budget()
    }

    fn note(&self, layer: &[Pair<X, F>]) -> String {
        format!(" of both halves ({} with real play exited)", layer.iter().filter(|p| p.fixed.is_none()).count())
    }
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
/// halves' common horizon (`concrete::search` over pairs; the module doc).
/// `prefer` orders the inputs (a known file first). `None`: no dual run by
/// the horizon.
pub fn search<X: Rem, F: Rem>(fixed: &Half<X>, float: &Half<F>, bound: u32, prefer: Option<&[u8]>) -> Result<Option<DualWitness>> {
    let horizon = fixed.arcs.horizon;
    ensure!(float.arcs.horizon == horizon, "the halves' horizons differ ({horizon} and {} steps)", float.arcs.horizon);
    let clock = fixed.settings.clock();
    ensure!(float.settings.clock() == clock, "the halves step frames differently");
    ensure!(clock.boundary(horizon), "the horizon {horizon} is not a whole number of frames ({} steps each)", clock.per_frame);
    fn side<'a, N: Rem>(h: &Half<'a, N>) -> Result<Side<'a, N>> {
        Ok(Side { w: &h.arcs.w, node: NodeKeys::new(&h.arcs.w, &h.arcs.shapes)?, keys: &h.arcs.space, level: h.arcs.level, s: h.settings, seeded: h.settings.concrete().1 })
    }
    let workers = fixed.settings.threads().max(1);
    eprintln!("[dual] W {} fixed and {} float nodes; {workers} workers; the bound f{bound}", fixed.arcs.w.len(), float.arcs.w.len());
    let Some(path) = super::concrete::search(&Pairs { sx: side(fixed)?, sf: side(float)? }, clock.frame_of(horizon), bound, clock, prefer, workers)? else { return Ok(None) };
    // Real play leaves at the first edge that says so (one does at most).
    let fixed_exit = path.labels.iter().position(|&w| w).ok_or_else(|| anyhow!("a dual win whose fixed half never exited"))? as u32;
    Ok(Some(DualWitness { inputs: path.inputs, fixed_exit }))
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

/// The player's cell at every frame of `inputs` (the start's first, a
/// player-less frame `pos_graph::NO_CELL`), as a concrete run of `s`
/// (`Settings::concrete`) - the UI's path of a dual witness, its float
/// half's. A frame that forks (`rnd`) follows its first leaf.
pub fn path_cells<N: Rem>(inputs: &[u8], s: &Settings) -> Result<Vec<u32>> {
    let mut eng = ReferenceEngine::<N::A>::new(&s.concrete().0)?;
    let start = Block::canonical(eng.initial()?)?;
    let mut cells = vec![start.positions()?[0]];
    let mut row = start.into_rt2();
    for &byte in inputs {
        let b = eng.frame(&row, byte)?.into_iter().next().ok_or_else(|| anyhow!("a frame with no successor"))?;
        cells.push(b.positions()?[0]);
        row = b.into_rt2();
    }
    Ok(cells)
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
