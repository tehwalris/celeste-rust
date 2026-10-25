//! THE ARC GATE'S FINGERPRINTS (`rewrite gate --search-dir D`; CLAUDE.md
//! "Gates"): a finished search's `[gate]` lines, from its record and its
//! trees. Per level it ran (`search::driver::levels_of`, a dual search's
//! halves in its order), the arc phase rerun over its tree (`ArcPhase::of`,
//! as the search ran it): W's SUPPORT (its nodes and a content hash over
//! their (shape, key, cell)), per frame the nodes with a set and the sum of
//! their (node, set) hashes (`w_hash`), the arc optimum in steps; then the
//! search's concrete optimum and witness (`result.json`), or the dual
//! search's. Content only: the same at every thread count and over a
//! reused tree.

use std::path::Path;

use anyhow::Result;
use celeste_engine::runtime2::mix64;

use crate::search::arcs::{Circle, Doubles, Fixed16, RegionRef, Seg};
use crate::search::backward::BLOCK;
use crate::search::driver::ArcPhase;
use crate::search::rem::Rem;
use crate::settings::Settings;

/// A circle's segment as one word of the gate's fingerprints (`w_hash`).
pub trait GateWord: Circle {
    fn seg_word(s: Seg<Self::P>) -> u64;
}

impl GateWord for Fixed16 {
    #[inline]
    fn seg_word(s: Seg) -> u64 {
        (s.lo as u64) << 32 | s.hi as u64
    }
}

impl GateWord for Doubles {
    #[inline]
    fn seg_word(s: Seg<u64>) -> u64 {
        mix64(s.lo) ^ s.hi.rotate_left(32)
    }
}

/// One node's term of the gate's `W f{t}` line: its content hash
/// (`exact::ContentHash::state`) folded with its set's rectangles.
pub fn w_hash<C: GateWord>(node: u64, r: RegionRef<C>) -> u64 {
    let mut h = node;
    for (y, xs) in r.slabs() {
        h = mix64(h ^ C::seg_word(y));
        for x in xs {
            h = mix64(h ^ C::seg_word(x));
        }
    }
    h
}

/// THE `[gate]` LINES of the search in `search_dir` (`run`: the run's own
/// settings, the threads): per level the arc phase's, then the concrete
/// optimum's (`[gate] hH concrete optimum F inputs ..`, H in steps) or a dual
/// search's (`[gate] hH dual optimum F fixed exit X inputs ..`, H in frames).
pub fn search_lines(search_dir: &Path, run: &Settings) -> Result<Vec<String>> {
    use crate::search::result::Verdict;
    let (res, trees) = crate::search::driver::levels_of(search_dir, run)?;
    // The backward's scratch: on disk, never inside a tree.
    let scratch = std::path::PathBuf::from(format!("/var/tmp/celeste-gate-{}", std::process::id()));
    let mut out = Vec::new();
    for (dir, s, level) in &trees {
        let steps = s.clock().steps(res.horizon);
        let lines = crate::with_arith!(s.arith, N => level_lines::<N>(dir, *level, steps, s, &scratch));
        let _ = std::fs::remove_dir_all(&scratch);
        out.extend(lines?);
    }
    let inputs = crate::search::concrete::inputs_text;
    if let (Verdict::Optimal, Some(w), Some((_, s, _))) = (res.verdict, &res.witness, trees.last()) {
        match &res.dual {
            None => {
                let steps = s.clock().steps(res.horizon);
                out.push(format!("[gate] h{steps} concrete optimum {} inputs {}", w.len(), inputs(w)));
            }
            Some(d) => {
                let exit = d.fixed_exit.ok_or_else(|| anyhow::anyhow!("{}: a dual witness without its fixed exit", search_dir.display()))?;
                out.push(format!("[gate] h{} dual optimum {} fixed exit {exit} inputs {}", res.horizon, w.len(), inputs(w)));
            }
        }
    }
    Ok(out)
}

/// One level's lines: its arc phase rerun over the tree in `dir` to step
/// `horizon`, W's support, W per frame and the optimum. Each span is added
/// at its first frame and taken off past its last (the sums commute, so the
/// nodes go in parallel).
fn level_lines<N: Rem>(dir: &Path, level: crate::abstraction::Level, horizon: u32, s: &Settings, scratch: &Path) -> Result<Vec<String>>
where
    N::C: GateWord,
{
    let ap = ArcPhase::<N>::of(dir, level, horizon, s, scratch)?;
    let w = &ap.arcs.w;
    let content = ap.arcs.space.content_hash();
    let parts = crate::par::Pool::new(s.threads()).chunks(w.len().div_ceil(BLOCK), 4096, |lo, hi| {
        let (mut fp, mut delta) = (0u64, vec![(0i64, 0u64); horizon as usize + 2]);
        for x in w.nodes_from(lo * BLOCK, hi * BLOCK) {
            let (shape, key, cell) = ap.seeds.state(x.id);
            let h = content.state(shape, key, cell);
            fp = fp.wrapping_add(h);
            for (lo, hi, r) in x.spans() {
                let g = w_hash(h, r);
                delta[lo as usize] = (delta[lo as usize].0 + 1, delta[lo as usize].1.wrapping_add(g));
                delta[hi as usize + 1] = (delta[hi as usize + 1].0 - 1, delta[hi as usize + 1].1.wrapping_sub(g));
            }
        }
        (fp, delta)
    });
    let (mut fp, mut delta) = (0u64, vec![(0i64, 0u64); horizon as usize + 2]);
    for (f, d) in parts {
        fp = fp.wrapping_add(f);
        for (a, b) in delta.iter_mut().zip(d) {
            *a = (a.0 + b.0, a.1.wrapping_add(b.1));
        }
    }
    let mut out = vec![format!("[gate] h{horizon} support {} {fp:016x}", w.len())];
    let (mut n, mut acc) = (0i64, 0u64);
    for (t, d) in delta[..=horizon as usize].iter().enumerate() {
        (n, acc) = (n + d.0, acc.wrapping_add(d.1));
        out.push(format!("[gate] h{horizon} W f{t:03} {n} {acc:016x}"));
    }
    out.push(format!("[gate] h{horizon} arc optimum {}", ap.arcs.steps.map_or("none".to_string(), |f| f.to_string())));
    Ok(out)
}
