//! A KNOWN INPUT FILE REPLAYED: stepped through the reference engine under
//! the run's concrete settings (`Settings::concrete`: its seeds), every
//! `rnd` leaf followed, a step's states deduplicated by exact state
//! (`concrete::exact_state`), up to the first step a leaf wins. The one
//! stepper of the known-route check (`known`), the dual witness's replay
//! and path (`dual`), the level -1 check of a known solution
//! (`level_minus_one::check_route`) and the robustness report (`robust`).

use anyhow::Result;
use celeste_engine::runtime2::Rt2;

use super::concrete::exact_state;
use super::rem::Rem;
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

impl<N: Rem> Replay<N> {
    /// `inputs` from the room's start under `s`, through step `horizon`
    /// (`None`: every input), or to the first win.
    pub(crate) fn of(inputs: &[u8], horizon: Option<u32>, s: &crate::settings::Settings) -> Result<Self> {
        let clock = s.clock();
        let (mut engines, seeded) = super::concrete::concrete_engines::<N>(1, s)?;
        let eng = &mut engines[0];
        let mut layers: Vec<Vec<(u32, Rt2<N>)>> = vec![vec![(0, eng.initial()?.canonicalized())]];
        let mut merged: Vec<Vec<(u32, u32)>> = vec![Vec::new()];
        let last = horizon.unwrap_or(clock.steps(inputs.len() as u32));
        for step in 0..last {
            // The step to `step + 1` is in frame `frame_of(step + 1)`: its input.
            let Some(&byte) = inputs.get(clock.frame_of(step + 1) as usize - 1) else { break };
            let (mut next, mut more, mut won) = (Vec::new(), Vec::new(), Vec::new());
            let mut seen: rustc_hash::FxHashMap<celeste_engine::exact::ExactRow, u32> = Default::default();
            for (p, (_, b)) in layers.last().expect("the start").iter().enumerate() {
                for c in eng.step(b, byte)? {
                    if wins_of(&c, s)?.iter().any(|&x| x) {
                        won.push((p as u32, c));
                        continue;
                    }
                    match seen.entry(exact_state(&c, crate::storage::cells::block_cells(&c)?[0])) {
                        std::collections::hash_map::Entry::Occupied(e) => more.push((*e.get(), p as u32)),
                        std::collections::hash_map::Entry::Vacant(e) => {
                            e.insert(next.len() as u32);
                            next.push((p as u32, c));
                        }
                    }
                }
            }
            if !won.is_empty() {
                return Ok(Replay { layers, merged, exit: Some((step + 1, won)), seeded });
            }
            if next.is_empty() {
                break;
            }
            layers.push(next);
            merged.push(more);
        }
        Ok(Replay { layers, merged, exit: None, seeded })
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
