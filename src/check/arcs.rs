//! THE TRANSFERS AGAINST THE REFERENCE ENGINE (`rewrite check arcs`): every
//! record of frames `from..=to` has a transfer, and a sample per frame is
//! probed. From the source row at a remainder INSIDE its guards (corners and
//! middle: the cut points), the 64 inputs must reach the target at the
//! remainder the action predicts, and only at remainders some record of the
//! pair predicts; one point past a guard's end (doubles: one ulp), OUTSIDE
//! every guard of the pair, not at all. The probe steps the row at the level
//! (`ReferenceEngine::step_at`: unknowns forked as the kernels do).

use anyhow::{anyhow, Result};
use celeste_engine::runtime2::{Col, Rt2, AV};

use crate::search::arcs::{Act, Circle, Doubles, Fixed16, Seg, Transfer, CIRCLE, POINTS};
use crate::search::rem::{Point, Rem, Xfer};
use crate::abstraction::Level;
use crate::frame::widened_keys;
use crate::storage::edges::EdgeStore;
use crate::storage::layer::Tree;
use crate::storage::Sk;
use crate::engines::reference::ReferenceEngine;

/// A circle's transfers CORRUPTED, to check the check (`--fault`): the
/// check's own, not the circle's (nothing else corrupts a transfer).
pub trait Faulty: Circle {
    /// `a` with its images moved (guard `g`).
    fn fault(a: &Act<Self>, g: Seg<Self::P>) -> Act<Self>;
}

impl Faulty for Fixed16 {
    /// A translation by one more point (one less at the circle's top), a
    /// constant moved by one point.
    fn fault(a: &Act<Fixed16>, g: Seg) -> Act<Fixed16> {
        match *a {
            Act::Mono(d) => Act::Mono(if g.hi as i64 + d as i64 + 1 <= CIRCLE as i64 { d + 1 } else { d - 1 }),
            Act::Const(c) => Act::Const((c + 1) % CIRCLE),
        }
    }
}

impl Faulty for Doubles {
    /// A `Mono` collapsed onto its lowest point's image, a constant moved
    /// by one point.
    fn fault(a: &Act<Doubles>, g: Seg<u64>) -> Act<Doubles> {
        match a {
            Act::Mono(m) => Act::Const(Doubles::apply(m, g.lo)),
            Act::Const(c) => Act::Const((c + 1) % POINTS),
        }
    }
}

/// `t` CORRUPTED: its guard's lowest point dropped (`guard`), or its images
/// moved.
fn faulty<C: Faulty>(t: &Transfer<C>, guard: bool) -> Transfer<C> {
    let mut t = *t;
    if guard {
        if C::next(t.guard.lo) < t.guard.hi {
            t.guard.lo = C::next(t.guard.lo);
        }
    } else {
        t.act = C::fault(&t.act, t.guard);
    }
    t
}

pub struct Opts<'a> {
    pub dir: &'a std::path::Path,
    pub from: u32,
    pub to: u32,
    pub samples: usize,
    pub level: Level,
    /// Check the CHECK: corrupt every decoded x transfer (`action`: its
    /// image moved; `guard`: its lowest point dropped), which must fail.
    pub fault: Option<&'a str>,
    /// At most this many reference fork paths a step (a sample past it is
    /// SKIPPED and counted).
    pub path_cap: Option<usize>,
    pub settings: &'a crate::settings::Settings,
}

struct Rec<N: Rem> {
    target: Sk,
    src: Sk,
    t: Xfer<N>,
}

/// One sampled record's verdicts, and what it printed.
#[derive(Default)]
struct Sampled {
    inside: u64,
    inside_bad: u64,
    unpredicted: u64,
    outside: u64,
    outside_bad: u64,
    no_player: u64,
    steps: u64,
    /// Over the path cap (`path_cap`): not checked.
    skipped: u64,
    msgs: Vec<String>,
}

/// A successor: its projection onto the level (shape, key, cell) and its
/// player's remainder as points (`None`: no player).
type Succ<N> = ((u64, Option<(u64, u64)>, u32), Option<(Point<N>, Point<N>)>);

/// The image of point `p` under one axis's transfer, if its guard takes it.
fn image_of<C: Circle>(a: &Transfer<C>, p: C::P) -> Option<C::P> {
    a.image(Seg { lo: p, hi: C::next(p) }).map(|s| s.lo)
}

/// Does `t` take `(x, y)` to `q` (`None`: no player at the end, so any)?
fn predicts<C: Circle>(t: &(Transfer<C>, Transfer<C>), p: (C::P, C::P), q: Option<(C::P, C::P)>) -> bool {
    match (image_of::<C>(&t.0, p.0), image_of::<C>(&t.1, p.1)) {
        (Some(x), Some(y)) => q.is_none_or(|q| q == (x, y)),
        _ => false,
    }
}

pub fn run<N: Rem>(o: &Opts) -> Result<()>
where
    N::C: Faulty,
{
    let (dir, to, level, cfg) = (o.dir, o.to, o.level, o.settings);
    cfg.check(dir, level)?;
    let ids = crate::compiled::ids();
    let graph = EdgeStore::<N>::open(&dir, to)?;
    let rem_cells = |rt2: &Rt2<N>| rt2.player_xy_cells(ids.f_rem);
    // One stored state's row, as a one-lane block.
    let tree = Tree::<N>::open(dir, to)?;
    let row_of = |s: Sk| -> Result<Rt2<N>> {
        let f = tree.layer_of(s, to)?.ok_or_else(|| anyhow!("state {}: in no frame through f{to}", s.show()))?;
        Ok(tree.row(f, s, cfg.room)?)
    };
    // The tree's key space: projections keyed as its rows (`None`: a code
    // it never stored).
    let space = &tree.space;
    // Every successor of `row` at remainder `rem` over all 64 inputs.
    let successors = |eng: &mut ReferenceEngine<N>, row: &Rt2<N>, rem: (Point<N>, Point<N>)| -> Result<(Vec<Succ<N>>, u64)> {
        // As the kernels read it: projected onto the level (the start row is
        // stored exact), then the probe's remainder.
        let mut rt2 = row.clone_block();
        rt2.widen_to(level);
        if let Some((cx, cy)) = rem_cells(&rt2) {
            rt2.cols[cx] = Col::U(AV::Num(N::at_point(rem.0)));
            rt2.cols[cy] = Col::U(AV::Num(N::at_point(rem.1)));
        }
        let (mut out, mut steps) = (Vec::new(), 0u64);
        // An input that agrees with one already run on the buttons it read
        // makes the same successors (`ReferenceEngine::step_reads`).
        let mut ran: Vec<(u8, u8)> = Vec::new();
        for b in 0..64u8 {
            if ran.iter().any(|(r, m)| (r ^ b) & m == 0) {
                continue;
            }
            let (blocks, read) = eng.step_at(&rt2, b, level)?;
            ran.push((b, read));
            for blk in blocks {
                steps += 1;
                let q = match rem_cells(&blk) {
                    Some((cx, cy)) => {
                        let at = |c: usize| -> Result<Point<N>> {
                            match blk.cols[c].at(0) {
                                AV::Num(n) => N::rem_point(n).ok_or_else(|| anyhow!("check arcs: a remainder {n:?} outside [-1/2, 1/2)")),
                                other => anyhow::bail!("check arcs: a concrete remainder expected, got {other:?}"),
                            }
                        };
                        Some((at(cx)?, at(cy)?))
                    }
                    None => None,
                };
                let (shape, keys, cells) = widened_keys(&blk, level, space)?;
                out.push(((shape, keys[0], cells[0]), q));
            }
        }
        Ok((out, steps))
    };
    let corrupt = |t: Xfer<N>| -> Result<Xfer<N>> {
        Ok(match o.fault {
            None => t,
            Some(k @ ("action" | "guard")) => (faulty(&t.0, k == "guard"), t.1),
            Some(other) => anyhow::bail!("--fault {other}: `action` or `guard`"),
        })
    };
    let check = |eng: &mut ReferenceEngine<N>, frame: u32, r: &Rec<N>, recs: &[Rec<N>]| -> Result<Sampled> {
        let mut o = Sampled::default();
        let src = r.src;
        let src_row = row_of(src)?;
        // The target by what it is (a hull tree records edges into states
        // it reaches later or never: not every target has a frame).
        let target = (tree.shape_of(r.target.region)?, Some(r.target.key), r.target.cell());
        let (gx, gy) = (r.t.0.guard, r.t.1.guard);
        let pair: Vec<&Rec<N>> = recs.iter().filter(|q| q.target == r.target && q.src == src).collect();
        let in_union = |x: Point<N>, y: Point<N>| pair.iter().any(|q| image_of::<N::C>(&q.t.0, x).is_some() && image_of::<N::C>(&q.t.1, y).is_some());
        let has_player = rem_cells(&src_row).is_some();
        if !has_player {
            o.no_player += 1;
        }
        let last = |g: Seg<Point<N>>| N::C::prev(g.hi);
        let probes: Vec<(Point<N>, Point<N>)> = if has_player {
            vec![(gx.lo, gy.lo), (last(gx), last(gy)), (N::C::mid(gx), N::C::mid(gy)), (gx.lo, last(gy))]
        } else {
            let z = crate::search::rem::start_point::<N>();
            vec![z]
        };
        for p in probes {
            o.inside += 1;
            let (succs, steps) = successors(eng, &src_row, p)?;
            o.steps += steps;
            let ok = succs.iter().any(|(id, q)| *id == target && predicts::<N::C>(&r.t, p, *q));
            // Every successor on the target lands where a record of the pair
            // that takes `p` predicts.
            let stray: Vec<(Point<N>, Point<N>)> = succs.iter().filter(|(id, _)| *id == target).filter_map(|(_, q)| *q).filter(|q| !pair.iter().any(|o| predicts::<N::C>(&o.t, p, Some(*q)))).collect();
            if !stray.is_empty() {
                o.unpredicted += 1;
                o.msgs.push(format!("[check arcs] f{frame} pred {} -> {}: inside {p:?} the reference reaches the target at {stray:?}, which no record of the pair taking the point predicts", src.show(), r.target.show()));
            }
            if !ok {
                o.inside_bad += 1;
                let reached: Vec<&Option<(Point<N>, Point<N>)>> = succs.iter().filter(|(id, _)| *id == target).map(|(_, q)| q).collect();
                o.msgs.push(format!(
                    "[check arcs] f{frame} pred {} -> {}: inside {p:?} the transfer x {:?} y {:?} predicts ({:?}, {:?}); the reference reaches the target with remainders {reached:?}",
                    src.show(),
                    r.target.show(),
                    r.t.0,
                    r.t.1,
                    image_of::<N::C>(&r.t.0, p.0),
                    image_of::<N::C>(&r.t.1, p.1)
                ));
            }
        }
        if has_player {
            let zero = Point::<N>::default();
            let before = |v: Point<N>| (v > zero).then(|| N::C::prev(v));
            let after = |v: Point<N>| (v < N::C::SIZE).then_some(v);
            let (mx, my) = (N::C::mid(gx), N::C::mid(gy));
            let mut outs: Vec<(Point<N>, Point<N>)> = Vec::new();
            for (x, y) in [(before(gx.lo), Some(my)), (after(gx.hi), Some(my)), (Some(mx), before(gy.lo)), (Some(mx), after(gy.hi)), (after(gx.hi), after(gy.hi))] {
                if let (Some(x), Some(y)) = (x, y) {
                    if !in_union(x, y) {
                        outs.push((x, y));
                    }
                }
            }
            for p in outs {
                o.outside += 1;
                let (succs, steps) = successors(eng, &src_row, p)?;
                o.steps += steps;
                if succs.iter().any(|(id, _)| *id == target) {
                    o.outside_bad += 1;
                    o.msgs.push(format!("[check arcs] f{frame} pred {} -> {}: OUTSIDE every guard of the pair at {p:?}, the reference reaches the target", src.show(), r.target.show()));
                }
            }
        }
        Ok(o)
    };
    let (mut missing_total, mut records_total, mut sampled) = (0u64, 0u64, 0u64);
    let mut total = Sampled::default();
    let mut kinds = std::collections::BTreeMap::<String, u64>::new();
    let t0 = std::time::Instant::now();
    let workers = cfg.threads();
    for frame in o.from..=to {
        let all = graph.edges_at(frame, cfg.threads())?;
        let missing = all.iter().filter(|e| graph.pair(e.xfer).is_none()).count() as u64;
        missing_total += missing;
        let recs: Vec<Rec<N>> = all
            .iter()
            .filter_map(|e| graph.pair(e.xfer).map(|p| (e, p)))
            .map(|(e, p)| Ok(Rec { target: e.dst, src: e.src, t: corrupt(N::xfer(&p))? }))
            .collect::<Result<_>>()?;
        records_total += recs.len() as u64;
        for r in &recs {
            for (axis, t) in [("x", &r.t.0), ("y", &r.t.1)] {
                let whole = t.guard == Seg { lo: Point::<N>::default(), hi: N::C::SIZE };
                let k = format!("{axis} {} {}", if whole { "whole" } else { "piece" }, t.kind());
                *kinds.entry(k).or_default() += 1;
            }
        }
        // `samples` records evenly spaced, checked in parallel (a reference
        // engine per worker), reported in record order.
        let picked: Vec<&Rec<N>> = super::differential::stride(recs.len(), o.samples).into_iter().map(|k| &recs[k]).collect();
        let done = super::differential::run(
            workers,
            picked.len(),
            || {
                let mut eng = ReferenceEngine::<N>::new(cfg)?;
                if let Some(cap) = o.path_cap {
                    eng.set_path_cap(cap);
                }
                Ok(eng)
            },
            |eng, k| check(eng, frame, picked[k], &recs),
        )?;
        for (k, o) in done.into_iter().enumerate() {
            let o = match o {
                super::differential::Ran::Done(o) => o,
                super::differential::Ran::Skipped(why) => {
                    let r = picked[k];
                    Sampled { skipped: 1, msgs: vec![format!("[check arcs] f{frame} pred {} -> {}: SKIPPED ({why})", r.src.show(), r.target.show())], ..Default::default() }
                }
            };
            for m in &o.msgs {
                eprintln!("{m}");
            }
            sampled += 1;
            total.skipped += o.skipped;
            total.inside += o.inside;
            total.inside_bad += o.inside_bad;
            total.unpredicted += o.unpredicted;
            total.outside += o.outside;
            total.outside_bad += o.outside_bad;
            total.no_player += o.no_player;
            total.steps += o.steps;
        }
        eprintln!(
            "[check arcs] f{frame:03}: {} records, {missing} without a transfer; so far inside {} ({} bad, {} unpredicted), outside {} ({} bad), {} ref steps, {:.1} s",
            recs.len(),
            total.inside,
            total.inside_bad,
            total.unpredicted,
            total.outside,
            total.outside_bad,
            total.steps,
            t0.elapsed().as_secs_f64()
        );
    }
    println!("check arcs f{}-f{to}: {records_total} records; transfers per axis {kinds:?}", o.from);
    println!("completeness: {missing_total} recorded edges without a transfer");
    println!(
        "agreement: {sampled} sampled records ({} from a row without a player, {} SKIPPED over the path cap); inside the guard {} probes, {} disagree, {} reach the target unpredicted; outside every guard {} probes, {} reach the target; {} reference steps",
        total.no_player, total.skipped, total.inside, total.inside_bad, total.unpredicted, total.outside, total.outside_bad, total.steps
    );
    let bad = missing_total + total.inside_bad + total.unpredicted + total.outside_bad;
    super::differential::verdict((sampled - total.skipped) as usize, bad as usize, || "check arcs: disagreement".into())
}
