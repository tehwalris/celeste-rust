//! A KNOWN INPUT FILE REPLAYED: stepped through the reference engine under
//! the run's concrete settings (`Settings::concrete`: its seeds), every
//! `rnd` leaf followed, a step's states deduplicated by exact state
//! (`concrete::exact_state`), up to the first step a leaf wins. The one
//! stepper of the known-route check (`known`), the dual witness's replay
//! and path (`dual`), the level -1 check of a known solution
//! (`level_minus_one::check_route`) and the robustness report (`robust`).

use anyhow::Result;
use celeste_engine::exact::ExactRow;
use celeste_engine::runtime2::Rt2;

use super::concrete::{exact_state, layers, Concrete, Next, Policy};
use super::rem::Rem;
use crate::engines::reference::ReferenceEngine;
use crate::frame::wins_of;
use celeste_core::arith::Arithmetic;

/// The states of a replay, per search step.
pub(crate) struct Replay<N: Arithmetic> {
    /// Per step up to the step before the exit, its states, each with its
    /// FIRST parent (the index of the state in the step before it was first
    /// made from; the start's is 0).
    pub(crate) layers: Vec<Vec<(u32, Rt2<N>)>>,
    /// Per step, the parents dedup dropped: `(state, another parent)`.
    pub(crate) merged: Vec<Vec<(u32, u32)>>,
    /// The step a leaf first wins at, and the winning leaves with their
    /// parents (`None`: no win by the horizon or the inputs' end).
    pub(crate) exit: Option<(u32, Vec<(u32, Rt2<N>)>)>,
    /// Whether the run is seeded (`Settings::concrete`).
    pub(crate) seeded: bool,
}

/// A route stepped as a layered search (`concrete::layers`): a SEARCH STEP
/// a layer, every leaf, a win a leaf the room's win takes.
struct Route<'s, N>(&'s crate::settings::Settings, std::marker::PhantomData<N>);

impl<N: Rem> Concrete for Route<'_, N> {
    type Engine = ReferenceEngine<N>;
    type State = Rt2<N>;
    type Key = ExactRow;
    type Label = ();
    const NAME: &'static str = "replay";

    fn engines(&self, n: usize) -> Result<Vec<Self::Engine>> {
        Ok(super::concrete::concrete_engines::<N>(n, self.0)?.0)
    }

    fn start(&self, eng: &mut Self::Engine) -> Result<(Rt2<N>, ())> {
        Ok((eng.initial()?.canonicalized(), ()))
    }

    fn successors(&self, eng: &mut Self::Engine, st: &Rt2<N>, byte: u8, _t: u32, _: &dyn Fn(&ExactRow) -> bool, steps: &mut u64) -> Result<(Vec<Next<Self>>, u8)> {
        let mut out = Vec::new();
        for c in eng.step(st, byte)? {
            *steps += 1;
            if wins_of(&c, self.0)?.iter().any(|&x| x) {
                out.push(Next::Win(c, ()));
                continue;
            }
            let key = exact_state(&c, crate::storage::cells::block_cells(&c)?[0]);
            out.push(Next::State(c, key, ()));
        }
        Ok((out, u8::MAX))
    }

    fn budget(&self) -> u64 {
        0
    }
}

impl<N: Rem> Replay<N> {
    /// `inputs` from the room's start under `s`, through step `horizon`
    /// (`None`: every input), or to the first win.
    pub(crate) fn of(inputs: &[u8], horizon: Option<u32>, s: &crate::settings::Settings) -> Result<Self> {
        let clock = s.clock();
        let route = Route::<N>(s, std::marker::PhantomData);
        let mut eng = route.engines(1)?.remove(0);
        let (start, ()) = route.start(&mut eng)?;
        let mut engines = [eng];
        let last = horizon.unwrap_or(clock.steps(inputs.len() as u32));
        // The step to `k + 1` is in frame `frame_of(k + 1)`: its input.
        let policy = Policy { inputs: &|k| inputs.get(clock.frame_of(k + 1) as usize - 1).map(|&b| vec![b]), replay: true };
        let ls = layers(&route, &mut engines, start, last, &policy, |_, _, _, _, _| {})?;
        let layers = ls.states.into_iter().enumerate().map(|(k, st)| st.into_iter().enumerate().map(|(i, b)| (if k == 0 { 0 } else { ls.back[k - 1][i].0 }, b)).collect()).collect();
        let exit = ls.won.map(|(step, wins)| (step, wins.into_iter().map(|(p, _, _, leaf)| (p, leaf)).collect()));
        Ok(Replay { layers, merged: ls.merged, exit, seeded: s.concrete().1 })
    }

    /// The first frame a leaf wins in (`None`: none).
    pub(crate) fn exit_frame(&self, clock: crate::settings::Clock) -> Option<u32> {
        self.exit.as_ref().map(|e| clock.frame_of(e.0))
    }

    /// The player's cell at every frame along the first winning lineage
    /// (the start's first; `cells::NO_CELL` without a player): the UI's
    /// path of a dual witness's float half.
    pub(crate) fn win_cells(&self, clock: crate::settings::Clock) -> Result<Vec<u32>> {
        let (step, won) = self.exit.as_ref().ok_or_else(|| anyhow::anyhow!("the path never wins"))?;
        let (parent, leaf) = &won[0];
        let idx = self.lineage(self.layers.len() - 1, *parent as usize);
        let mut cells = Vec::new();
        for (t, &i) in idx.iter().enumerate() {
            if clock.boundary(t as u32) {
                cells.push(crate::storage::cells::block_cells(&self.layers[t][i].1)?[0]);
            }
        }
        anyhow::ensure!(clock.boundary(*step), "the path wins mid-frame");
        cells.push(crate::storage::cells::block_cells(leaf)?[0]);
        Ok(cells)
    }

    /// The state indices of the lineage ending at step `step`'s state `i`,
    /// along first parents, from the start.
    pub(crate) fn lineage(&self, step: usize, i: usize) -> Vec<usize> {
        let mut idx = vec![0usize; step + 1];
        idx[step] = i;
        for s in (1..=step).rev() {
            idx[s - 1] = self.layers[s][idx[s]].0 as usize;
        }
        idx
    }

    /// Per step and state: does some lineage through it (along any parent)
    /// end in a winning leaf?
    pub(crate) fn on_a_win(&self) -> Vec<Vec<bool>> {
        let mut on: Vec<Vec<bool>> = self.layers.iter().map(|l| vec![false; l.len()]).collect();
        let Some((_, won)) = &self.exit else { return on };
        let last = self.layers.len() - 1;
        for &(p, _) in won {
            on[last][p as usize] = true;
        }
        for s in (1..=last).rev() {
            for (i, &(p, _)) in self.layers[s].iter().enumerate() {
                if on[s][i] {
                    on[s - 1][p as usize] = true;
                }
            }
            for &(i, p) in &self.merged[s] {
                if on[s][i as usize] {
                    on[s - 1][p as usize] = true;
                }
            }
        }
        on
    }
}
