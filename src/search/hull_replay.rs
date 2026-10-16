//! THE HULL FORWARD REPLAYED over a tree's recorded graph: from the start's
//! hull, every hull pushed along the recorded edges through their
//! transfers, frame by frame, re-pushed where it grows - what the forward
//! did, without the engine (a node's successors do not depend on its hull:
//! it is expanded with its remainder widened; the hull only decides which
//! pushes land). With nothing dropped it reaches exactly the tree's states
//! at the tree's frames (checked: a disagreement is an error). With some
//! transfers DROPPED it is the forward that never takes them - a
//! DIAGNOSTIC (not sound): what those edges cost, as the states only they
//! reach. The float tree's top sliver (`rem_f64`'s module doc) is the case
//! it was written for (`rewrite hull-replay`).

use anyhow::{ensure, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use super::rem::Rem;
use crate::storage::edges::EdgeStore;
use crate::storage::layer::Tree;
use crate::storage::Sk;

/// One frame of a replay.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Frame {
    /// States first reached at this frame: the tree's, the replay's with
    /// every edge, and with the dropped ones dropped.
    pub tree: u64,
    pub all: u64,
    pub kept: u64,
    /// Recorded edges into this frame, those of a dropped transfer, and
    /// the dropped ones that pushed something in the full replay.
    pub edges: u64,
    pub dropped: u64,
    pub fired: u64,
}

/// A tree's recorded graph: out-edges by source (target, transfer, the
/// frame recorded at), its frame-0 states, its transfers, and per frame its
/// states (first reached there) and edges.
struct Graph<N: Rem> {
    succ: FxHashMap<Sk, Vec<(Sk, u32, u32)>>,
    starts: Vec<Sk>,
    pairs: Vec<N::Pair>,
    states: Vec<u64>,
    edges: Vec<u64>,
}

impl<N: Rem> Graph<N> {
    fn load(dir: &std::path::Path, to: u32, threads: usize) -> Result<Self> {
        let tree = Tree::<N>::open(dir, to)?;
        let store = EdgeStore::<N>::open(&dir.join("edges"), to)?;
        let mut g = Graph { succ: FxHashMap::default(), starts: tree.layer(0, None, threads)?.0.states_iter().collect(), pairs: store.pairs().to_vec(), states: Vec::new(), edges: vec![0] };
        for f in 0..=to {
            g.states.push(tree.layer(f, None, threads)?.0.states());
            if f == 0 {
                continue;
            }
            ensure!(store.has_frame(f), "{}: no recorded edges at frame {f}", dir.display());
            let es = store.edges_at(f, threads)?;
            g.edges.push(es.len() as u64);
            for e in es {
                g.succ.entry(e.src).or_default().push((e.dst, e.xfer, f));
            }
        }
        Ok(g)
    }

    /// THE PROPAGATION of model `m` from `start` on the frame-0 states,
    /// frame by frame through `to`: every state that grew pushes along its
    /// out-edges (`on_push(source, target, transfer, frame recorded,
    /// pushed)` after each), and the targets merge. Every state reached,
    /// with its hull and the frame it was first reached at.
    fn propagate<M: Model>(&self, m: &M, start: M::H, to: u32, mut on_push: impl FnMut(Sk, Sk, u32, u32, bool)) -> FxHashMap<Sk, (M::H, u32)> {
        let mut hull: FxHashMap<Sk, (M::H, u32)> = self.starts.iter().map(|&s| (s, (start, 0))).collect();
        let mut grown: Vec<Sk> = self.starts.clone();
        for f in 1..=to {
            let mut acc: FxHashMap<Sk, M::A> = FxHashMap::default();
            for s in &grown {
                let h = hull[s].0;
                for &(d, x, at) in self.succ.get(s).map_or(&[][..], |v| &v[..]) {
                    let pushed = m.push(&h, x, acc.entry(d).or_default());
                    on_push(*s, d, x, at, pushed);
                }
            }
            grown.clear();
            for (d, a) in acc {
                let old = hull.get(&d).map(|e| e.0);
                if let Some(h) = m.merge(a, old.as_ref()) {
                    hull.entry(d).or_insert((h, f)).0 = h;
                    grown.push(d);
                }
            }
            grown.sort_unstable();
        }
        hull
    }
}

/// What a replay propagates: a hull per state, pushed into accumulators.
trait Model {
    type H: Copy;
    type A: Default;
    /// Push `h` along transfer `x` into `a`; whether it pushed.
    fn push(&self, h: &Self::H, x: u32, a: &mut Self::A) -> bool;
    /// The target's hull after `a` merged into `old` (`None` for a new
    /// target): `None` where it did not grow.
    fn merge(&self, a: Self::A, old: Option<&Self::H>) -> Option<Self::H>;
}

/// The hull forward's own model, its transfers `skip`ped where marked.
struct Hulls<'a, N: Rem> {
    xfers: &'a [crate::search::rem::Xfer<N>],
    skip: Option<&'a [bool]>,
}

impl<N: Rem> Model for Hulls<'_, N> {
    type H = N::Hull;
    type A = N::Acc;
    fn push(&self, h: &N::Hull, x: u32, a: &mut N::Acc) -> bool {
        !self.skip.is_some_and(|k| k[x as usize]) && N::push(h, &self.xfers[x as usize], a)
    }
    fn merge(&self, a: N::Acc, old: Option<&N::Hull>) -> Option<N::Hull> {
        let pushed = N::acc_hull(&a)?;
        let Some(old) = old else { return Some(pushed) };
        let mut m = a;
        N::acc_add_hull(&mut m, old);
        let merged = N::acc_hull(&m).expect("not empty");
        (merged != *old).then_some(merged)
    }
}

/// Replay the tree at `dir` through frame `to` from `start`'s hull on its
/// layer-0 states, both with every edge and without the edges whose
/// transfer `drop` says; per frame its counts (index 0 the start).
pub fn replay<N: Rem>(dir: &std::path::Path, to: u32, start: N::Hull, drop: impl Fn(&N::Pair) -> bool, threads: usize) -> Result<Vec<Frame>> {
    let g = Graph::<N>::load(dir, to, threads)?;
    let dropped: Vec<bool> = g.pairs.iter().map(&drop).collect();
    let xfers: Vec<crate::search::rem::Xfer<N>> = g.pairs.iter().map(N::xfer).collect();
    let mut out: Vec<Frame> = (0..=to as usize).map(|f| Frame { tree: g.states[f], edges: g.edges[f], ..Default::default() }).collect();
    for v in g.succ.values() {
        for &(_, x, at) in v {
            out[at as usize].dropped += dropped[x as usize] as u64;
        }
    }
    // The dropped edges that pushed something (the full replay).
    let mut fired: FxHashSet<(Sk, Sk, u32)> = FxHashSet::default();
    let all = g.propagate(&Hulls::<N> { xfers: &xfers, skip: None }, start, to, |s, d, x, at, pushed| {
        if pushed && dropped[x as usize] && fired.insert((s, d, x)) {
            out[at as usize].fired += 1;
        }
    });
    let kept = g.propagate(&Hulls::<N> { xfers: &xfers, skip: Some(&dropped) }, start, to, |_, _, _, _, _| {});
    for &(_, f) in all.values() {
        out[f as usize].all += 1;
    }
    for &(_, f) in kept.values() {
        out[f as usize].kept += 1;
    }
    for (f, o) in out.iter().enumerate() {
        ensure!(o.all == o.tree, "{}: frame {f}: the replay reaches {} states first, the tree holds {}", dir.display(), o.all, o.tree);
    }
    Ok(out)
}

/// The top doubles below 1/2 from which an integer speed's sum can round
/// onto the cut (`rem_f64`'s sliver): `[1/2 - 2^-40, 1/2)`, no 16.16
/// remainder among them.
fn sliver_from() -> u64 {
    super::rem_f64::index(0.5 - (2f64).powi(-40)).expect("a remainder")
}

/// Option (c) of plans/float-solver.md, section 15, simulated on a float
/// tree's graph: per state a hull and per axis one EXACT bit, "a remainder
/// in the top sliver `[1/2 - 2^-40, 1/2)` reaches it" - a push takes the
/// sliver's points of a hull only where its bit is set, and sets the
/// target's bit only where an image reaches the sliver (exactly: a `Mono`
/// piece is monotone, so where its largest point's image does). Per frame
/// the states first reached, and those whose bit is set at the end (the
/// sliver parts (c) would hold as nodes of their own).
pub fn sliver_bit(dir: &std::path::Path, to: u32, threads: usize) -> Result<Vec<(u64, u64)>> {
    use super::rem_f64::{Hull, HullAcc, Pair, MONO, POINTS};
    struct Bit<'a> {
        xfers: &'a [Pair],
        s0: u64,
    }
    impl Model for Bit<'_> {
        type H = (Hull, [bool; 2]);
        type A = (HullAcc, [bool; 2]);
        fn push(&self, h: &Self::H, x: u32, a: &mut Self::A) -> bool {
            let t = &self.xfers[x as usize];
            let mut segs: [Vec<(u64, u64)>; 2] = Default::default();
            for (axis, (arc, piece)) in [(h.0.x, &t.0), (h.0.y, &t.1)].into_iter().enumerate() {
                let top = if h.1[axis] { POINTS } else { self.s0 };
                arc.segs(|(lo, hi)| {
                    let (lo, hi) = (lo.max(piece.lo), hi.min(piece.hi).min(top));
                    if lo < hi {
                        segs[axis].push((lo, hi));
                    }
                });
            }
            if segs[0].is_empty() || segs[1].is_empty() {
                return false;
            }
            for (axis, piece) in [&t.0, &t.1].into_iter().enumerate() {
                for &(lo, hi) in &segs[axis] {
                    let img = if piece.tag == MONO { (piece.apply(lo), piece.apply(hi - 1) + 1) } else { (piece.a, piece.a + 1) };
                    a.0.add_seg(axis, img);
                    a.1[axis] |= img.1 > self.s0;
                }
            }
            true
        }
        fn merge(&self, a: Self::A, old: Option<&Self::H>) -> Option<Self::H> {
            let pushed = a.0.hull()?;
            let Some(old) = old else { return Some((pushed, a.1)) };
            let mut m = a.0;
            m.add_hull(&old.0);
            let merged = (m.hull().expect("not empty"), [a.1[0] || old.1[0], a.1[1] || old.1[1]]);
            (merged != *old).then_some(merged)
        }
    }
    let g = Graph::<celeste_core::lua_f64::LuaF64>::load(dir, to, threads)?;
    let zero = super::rem_f64::ZERO;
    let reached = g.propagate(&Bit { xfers: &g.pairs, s0: sliver_from() }, (Hull::point(zero, zero), [false; 2]), to, |_, _, _, _, _| {});
    let mut out = vec![(0u64, 0u64); to as usize + 1];
    for (h, f) in reached.values() {
        out[*f as usize].0 += 1;
        out[*f as usize].1 += (h.1[0] || h.1[1]) as u64;
    }
    Ok(out)
}

/// THE HULL FORWARD'S CLAIM ON A KNOWN ROUTE: `inputs` stepped concretely
/// (the reference engine of the tree's arithmetic, every leaf) through the
/// tree at `dir` (level `level`) to frame `to`. At every frame each state
/// of the route must be in the tree by then (its projection a state of
/// layer <= the frame), its exact remainder inside that state's hull, and
/// the step from its parent a recorded edge whose transfer takes the
/// parent's remainder to its own. Its exit frame (`None`: no exit by `to`);
/// the first step that breaks the claim is an error.
pub fn route<N: Rem>(dir: &std::path::Path, to: u32, level: crate::abstraction::Level, inputs: &[u8], s: &crate::settings::Settings) -> Result<Option<u32>> {
    use crate::frame::{widened_keys, wins_of, Block};
    use celeste_engine::runtime2::AV;
    anyhow::ensure!(s.steps_per_frame() == 1, "the route check: not under the split frame");
    let tree = Tree::<N>::open(dir, to)?;
    let (visited, ..) = crate::frame::restore_visited::<N>(dir, to, s.room, s.threads())?;
    let edges = EdgeStore::<N>::open(&dir.join("edges"), to)?;
    let mut eng = crate::trace::refengine::ReferenceEngine::<N::A>::new(s)?;
    let ids = crate::compiled::ids();
    // The player's remainder (`None`: no player).
    let rem = |b: &Block<N>| -> Result<Option<(N, N)>> {
        let Some((cx, cy)) = b.rt2().player_xy_cells(ids, ids.f_rem) else { return Ok(None) };
        let num = |c: usize| match b.rt2().cols[c].at(0) {
            AV::Num(n) => Ok(n),
            other => Err(anyhow::anyhow!("a concrete remainder expected, got {other:?}")),
        };
        Ok(Some((num(cx)?, num(cy)?)))
    };
    let zero = <N::A as celeste_core::arith::Arithmetic>::from_i16(0);
    // The start: the tree's one frame-0 state (stored as it is).
    let starts: Vec<Sk> = tree.layer(0, None, s.threads())?.0.states_iter().collect();
    ensure!(starts.len() == 1, "{}: {} states at frame 0", dir.display(), starts.len());
    let mut cur: Vec<(Block<N>, Sk)> = vec![(Block::canonical(eng.initial()?)?, starts[0])];
    for f in 1..=to {
        let Some(&byte) = inputs.get(f as usize - 1) else { return Ok(None) };
        let mut next = Vec::new();
        for (b, ps) in &cur {
            let from = rem(b)?.unwrap_or((zero, zero));
            let p_layer = tree.layer_of(*ps, f - 1)?.expect("a route state is in the tree");
            for c in eng.frame(b.rt2(), byte)? {
                if wins_of(c.rt2(), s)?.iter().any(|&w| w) {
                    return Ok(Some(f));
                }
                let (shape, keys, cells) = widened_keys(&c, level, &tree.space)?;
                let r = rem(&c)?;
                let at = |what: String| anyhow::anyhow!("{}: the route at frame {f} (input {byte}): {what}; its player at {:?}, remainder {r:?}", dir.display(), super::pos_graph::cell_xy(cells[0]));
                let sk = keys[0].and_then(|k| tree.sk(shape, k, cells[0])).ok_or_else(|| at("NOT IN THE TREE (no such state)".into()))?;
                tree.layer_of(sk, f)?.ok_or_else(|| at(format!("NOT IN THE TREE by frame {f} (state {})", sk.show())))?;
                let (x, y) = r.unwrap_or((zero, zero));
                if let Some(info) = visited.state(sk.region, sk.key, sk.local as u32) {
                    ensure!(N::hull_holds(&info.hull, x, y), at(format!("its remainder ({x:?}, {y:?}) is OUTSIDE its state's hull {:?} (state {})", info.hull, sk.show())));
                }
                let preds = edges.preds_at(sk, p_layer + 1)?;
                let mapped = preds.iter().filter(|(src, _)| src == ps).any(|&(_, x)| edges.pair(x).is_some_and(|p| N::maps(&p, from, r)));
                ensure!(
                    mapped,
                    at(format!(
                        "no edge from its parent (state {}, layer {p_layer}) takes the parent's remainder {from:?} to {r:?}: {:?}",
                        ps.show(),
                        preds.iter().filter(|(src, _)| src == ps).map(|&(_, x)| edges.pair(x)).collect::<Vec<_>>()
                    ))
                );
                next.push((c, sk));
            }
        }
        ensure!(next.len() <= 4096, "the route forks into {} states at frame {f}", next.len());
        cur = next;
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::abstraction::Level;
    use crate::frame::{forward_run, Block};
    use crate::settings::{Arith, FloatTool, Settings};
    use celeste_core::lua_f64::LuaF64;
    use celeste_core::pico8_num::Pico8Num as P8;

    /// A Celia golden's inputs (gates/celia/NAME.trace.zst): each update's
    /// `in`, 0 where it has none.
    fn celia_inputs(name: &str) -> Vec<u8> {
        let p = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("gates/celia").join(format!("{name}.trace.zst"));
        let text = String::from_utf8(zstd::decode_all(&std::fs::read(&p).expect("the golden")[..]).expect("zstd")).expect("utf-8");
        text.lines()
            .filter(|l| l.starts_with("u "))
            .map(|l| {
                let w: Vec<&str> = l.split_whitespace().collect();
                w.iter().position(|&t| t == "in").map_or(0, |i| w[i + 1].parse().expect("an input"))
            })
            .collect()
    }

    /// THE HULL FORWARD'S CLAIM ON ROOM (1,0), in both arithmetics: each
    /// tree replays exactly from its recorded graph (`replay` checks every
    /// frame), and a known route holds in it at every frame (`route`: in
    /// the tree, its remainder in its hull, every step a recorded edge
    /// mapping it) - in 16.16 the gate's witness through the kernels' tree,
    /// in doubles Celia's any% through the reference engine's.
    #[test]
    fn room_10_trees_replay_and_hold_their_routes() {
        let base = std::env::temp_dir().join(format!("celeste-hull-route-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let s = Settings::default();
        let dir = base.join("fixed");
        let init = vec![Block::canonical(crate::trace::refengine::RefEngine::new(&s).expect("engine").initial().expect("init")).expect("block")];
        forward_run(&crate::compiled::FrameEngine::new(&s, Level::EXACT).expect("kernels"), init, &dir, 33, false).expect("the fixed forward");
        let frames = replay::<P8>(&dir, 33, P8::start_hull(&s).expect("start"), |_| false, 4).expect("the fixed replay");
        assert!(frames[33].tree > 1000, "f33 holds {} states", frames[33].tree);
        let gate = crate::concrete::read_inputs(concat!(env!("CARGO_MANIFEST_DIR"), "/gates/known_room10_win9-101.txt")).expect("the gate's route");
        assert_eq!(route::<P8>(&dir, 33, Level::EXACT, &gate, &s).expect("the gate's route holds"), None);

        let s = Settings { arith: Arith::Float(FloatTool::Celia), loading_jank: crate::starts::celia_jank(s.room).expect("jank"), ..s };
        let dir = base.join("float");
        let init = vec![Block::canonical(crate::trace::refengine::FloatRefEngine::new(&s).expect("engine").initial().expect("init")).expect("block")];
        forward_run(&crate::trace::refengine::RefFrameStep::<LuaF64>::new(&s, Level::EXACT).expect("engines"), init, &dir, 28, false).expect("the float forward");
        let frames = replay::<LuaF64>(&dir, 28, LuaF64::start_hull(&s).expect("start"), |_| false, 4).expect("the float replay");
        assert!(frames[28].tree > 1000, "f28 holds {} states", frames[28].tree);
        assert_eq!(route::<LuaF64>(&dir, 28, Level::EXACT, &celia_inputs("any_2")[..28], &s).expect("Celia's any% holds"), None);
        let _ = std::fs::remove_dir_all(&base);
    }
}
