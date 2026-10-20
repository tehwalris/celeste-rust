//! THE DUAL SEARCH (plans/float.md, "The dual search and rule (b)"): the
//! shortest input file valid in BOTH arithmetics. Rule (b): its length is
//! the FLOAT exit, and real play (16.16) must have left by then. Two halves,
//! finished arc phases to one horizon (`Arcs`), each from its own start.
//!
//! A DUAL STATE is (the fixed half's exact state, or EXITED; the float
//! half's). Both halves step through their reference engines (`rnd` leaves
//! paired); a successor is admitted iff every live half lies in ITS OWN W.
//! A fixed half that wins is EXITED from then on; a float win is the dual
//! win if the fixed half has exited, else pruned (the file would end early).
//!
//! Sound: a both-valid run is valid in each arithmetic, so each half stays
//! inside its W, and nothing else prunes - the search is exhaustive inside
//! both W. Dedup and the depth-first try's memo are on the FULL dual state
//! (both exact rows and the exit flag), never a projection. An input is
//! skipped as equivalent only where it agrees with one already run on the
//! buttons BOTH halves read. The bound is the larger arc bound (the float
//! exit is at least the float's, and at least the fixed exit, itself at
//! least the fixed's), so the first breadth-first win is the optimal dual
//! run, and none by the horizon refutes every count up to it.

use anyhow::{anyhow, ensure, Result};

use celeste_engine::exact::ExactRow;
use celeste_engine::runtime2::Rt2;

use super::driver::Arcs;
use super::concrete::{concrete_engines, dfs_budget, exact_state, Concrete, Next, NodeKeys, Side};
use super::rem::Rem;

use crate::settings::Settings;
use crate::engines::reference::ReferenceEngine;

pub(crate) struct Half<'a, N: Rem> {
    pub(crate) arcs: &'a Arcs<N>,
    pub(crate) settings: &'a Settings,
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
fn fate<N: Rem>(side: &Side<N>, b: &Rt2<N>, t: u32) -> Result<Fate> {
    Ok(if side.wins(b)? {
        Fate::Win
    } else if side.inside(b, t)? {
        Fate::Inside
    } else {
        Fate::Out
    })
}

/// A dual state: the fixed half's row (`None`: exited) and the float half's.
pub(super) struct Pair<X: Rem, F: Rem> {
    fixed: Option<Rt2<X>>,
    float: Rt2<F>,
}

/// A dual state's identity (the fixed half's `None` once exited).
type Key = (Option<ExactRow>, ExactRow);

/// The dual search for `concrete::search`: a state a `Pair`, an edge's
/// label whether real play exits at it.
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
        Ok((Pair { fixed: Some(ex.initial()?.canonicalized()), float: ef.initial()?.canonicalized() }, false))
    }

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
                            let cell = crate::search::pos_graph::block_cells(&b)?[0];
                            let exact = exact_state(&b, cell);
                            fixed.push((Some((b, exact)), false));
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
                    let cell = crate::search::pos_graph::block_cells(&b)?[0];
                    let exact = exact_state(&b, cell);
                    for (x, fixed_won) in &fixed {
                        let (fixed_row, fixed_key) = match x {
                            Some((r, e)) => (Some(r.clone_block()), Some(e.clone())),
                            None => (None, None),
                        };
                        out.push(Next::State(Pair { fixed: fixed_row, float: b.clone_block() }, (fixed_key, exact.clone()), *fixed_won));
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

pub(crate) struct DualWitness {
    /// The file: one input a frame, the float half exiting on the last.
    pub(crate) inputs: Vec<u8>,
    /// The frame (1-based, `<= inputs.len()`) real play leaves on.
    pub(crate) fixed_exit: u32,
}

/// THE DUAL SEARCH from `bound` (the larger arc bound, frames) to the
/// halves' common horizon (`concrete::search` over pairs). `prefer` orders
/// the inputs (a known file first). `None`: no dual run by the horizon.
pub(crate) fn search<X: Rem, F: Rem>(fixed: &Half<X>, float: &Half<F>, bound: u32, prefer: Option<&[u8]>) -> Result<Option<DualWitness>> {
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

/// The first frame some lineage of `inputs` wins on from `s`'s start, a
/// concrete run (`Settings::concrete`), every `rnd` leaf followed.
pub(crate) fn first_win<N: Rem>(inputs: &[u8], s: &Settings) -> Result<Option<u32>> {
    Ok(super::replay::Replay::<N>::of(inputs, None, s)?.exit_frame(s.clock()))
}

/// The player's cell at every frame of `inputs` (the start's first;
/// `pos_graph::NO_CELL` without a player) along its first winning lineage,
/// a concrete run of `s`: the UI's path of a dual witness's float half.
pub(crate) fn path_cells<N: Rem>(inputs: &[u8], s: &Settings) -> Result<Vec<u32>> {
    let run = super::replay::Replay::<N>::of(inputs, None, s)?;
    let (step, won) = run.exit.as_ref().ok_or_else(|| anyhow!("the path never wins"))?;
    let (parent, leaf) = &won[0];
    let clock = s.clock();
    let last = run.layers.len() - 1;
    let idx = run.lineage(last, *parent as usize);
    let mut cells = Vec::new();
    for (t, &i) in idx.iter().enumerate() {
        if clock.boundary(t as u32) {
            cells.push(crate::search::pos_graph::block_cells(&run.layers[t][i].1)?[0]);
        }
    }
    ensure!(clock.boundary(*step), "the path wins mid-frame");
    cells.push(crate::search::pos_graph::block_cells(&leaf)?[0]);
    Ok(cells)
}

/// THE WITNESS, REPLAYED (a guard, not trusted), each half on its own
/// through its reference engine, every `rnd` leaf followed: the float half
/// must first win on the last frame (the file's length), the fixed half by
/// `fixed_exit` (some leaf: real play's `rnd` is the stated best case).
pub(crate) fn replay<X: Rem, F: Rem>(w: &DualWitness, fixed: &Settings, float: &Settings) -> Result<()> {
    let c = w.inputs.len() as u32;
    let f = first_win::<F>(&w.inputs, float)?;
    ensure!(f == Some(c), "the dual witness replayed: the float half first wins at {f:?}, not f{c}");
    let x = first_win::<X>(&w.inputs, fixed)?;
    ensure!(x.is_some_and(|x| x <= w.fixed_exit), "the dual witness replayed: the fixed half first wins at {x:?}, not by f{} as the search found", w.fixed_exit);
    Ok(())
}
