//! THE KERNELS' TREE AGAINST THE REFERENCE ENGINE'S (`rewrite check
//! reference`): the tree's steps grown again from the start by the
//! reference engine as a frame step (`engines::reference::RefFrameStep`,
//! under the tree's settings and level), their fingerprints compared line by
//! line (`storage::fingerprint`: each step's states, edges with their
//! transfers, and hulls). The reference reads and stores its rows through
//! the widening table's block side, so at every level but a near one under
//! the split frame (refused) its tree is the kernels'.

use anyhow::Result;
use std::path::Path;

use crate::engines::reference::{RefFrameStep, ReferenceEngine};
use crate::search::rem::Rem;
use crate::settings::Run;

/// `check reference` of the tree in `dir` (of `run`) to step `to`, the
/// reference's tree grown in `scratch`: an error where a line differs.
pub fn run<N: Rem>(dir: &Path, run: &Run, to: u32, scratch: &Path) -> Result<()> {
    let tm = std::time::Instant::now();
    let (lines, bad) = compare::<N>(dir, run, to, scratch, RefFrameStep::<N>::new(run)?)?;
    println!("[check reference] {}: steps 0-{to} at {}, {lines} fingerprint lines, {bad} differ ({:.1} s)", dir.display(), run.level, tm.elapsed().as_secs_f64());
    super::differential::verdict(lines, bad, || format!("check reference: {bad} of the kernels' fingerprint lines are not the reference engine's"))
}

/// The tree in `dir` and `engine`'s, grown in `scratch`, to step `to`: the
/// kernels' fingerprint lines and how many differ (each shown).
fn compare<N: Rem>(dir: &Path, run: &Run, to: u32, scratch: &Path, engine: RefFrameStep<N>) -> Result<(usize, usize)> {
    let s = &*run.settings;
    let engine: Box<dyn crate::frame::FrameStep<N>> = Box::new(engine);
    crate::frame::grow_tree(scratch, to, s.minus_one(), None, run, || Ok(engine), || Ok(vec![ReferenceEngine::<N>::new(s)?.initial()?.canonicalized()]))?;
    let kernels = crate::storage::fingerprint::<N>(dir, to, true, true, s.threads())?;
    let reference = crate::storage::fingerprint::<N>(scratch, to, true, true, s.threads())?;
    let mut bad = kernels.len().abs_diff(reference.len());
    for (k, r) in kernels.iter().zip(&reference) {
        if k != r {
            bad += 1;
            println!("[check reference] the kernels' {k}\n[check reference] the reference's {r}");
        }
    }
    Ok((kernels.len(), bad))
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::Pico8Num as P8;

    /// THE FRUIT LEVEL IS CHECKED, AND THE CHECK IS LIVE (plans/correctness.md
    /// C5): room (6,0) split at `r0sxf`, the kernels' tree to step 6 (the
    /// fly fruit widened from step 1: the kernels read its `fly` as an atom
    /// and its `step`, `y` as unknowns, the start's own values aside) is the
    /// reference's; the reference reading and storing at the exact level
    /// instead (the check's blind spot until C5) differs from step 1.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (6,0)'s split kernels and three short forwards, ~40 CPU-s"]
    fn the_reference_widens_as_the_kernels_do_at_the_fruit_level() {
        let s = crate::settings::Settings { room: (6, 0), split_frame: true, threads: Some(4), ..Default::default() };
        let run = Run::new(&s, crate::abstraction::Level::parse("r0sxf").expect("level"));
        let base = crate::testutil::TestDir::new("reference-fruit");
        let (kernels, to) = (base.join("kernels"), 6);
        let engine = crate::compiled::FrameEngine::new(&run).expect("kernels");
        crate::frame::forward_run(&engine, vec![ReferenceEngine::<P8>::new(&s).expect("engine").initial().expect("the start").canonicalized()], &kernels, to).expect("the kernels' forward");
        let (lines, bad) = compare::<P8>(&kernels, &run, to, &base.join("reference"), RefFrameStep::<P8>::new(&run).expect("engines")).expect("compare");
        // A step's lines: its states, hulls and edges (the start's no edges).
        assert_eq!((lines, bad), (3 * to as usize + 2, 0), "the reference's tree is not the kernels' at r0sxf");
        let fault = RefFrameStep::<P8>::new(&run).expect("engines").widened_at(crate::abstraction::Level::EXACT);
        let (_, bad) = compare::<P8>(&kernels, &run, to, &base.join("fault"), fault).expect("compare");
        assert_eq!(bad, 3 * to as usize, "every step but the start differs (states, edges, hulls) with the exact widening");
    }
}
