//! PLAYABILITY (`rewrite playable`): among the input sequences that win at
//! the optimum, the one a human can most easily play in real time.
//!
//! The set is `Dag`: every optimal path, as `rewrite search --robust
//! --save-dag` finds them (the concrete search's layers inside the sets
//! that win AT the optimum hold every state of every optimal path; per state
//! the DAG keeps every input byte that leads to a state still on one). A
//! sequence wins at the optimum iff it is a path of the DAG ending in the
//! win, so testing a candidate is a table walk, no engine. (An `rnd` fork,
//! one input with two successors, is refused: the room must be
//! deterministic under its seeds.)
//!
//! THE MEASURE (`score`). A sequence's EVENTS are its button edges: a press
//! or a release of one of the six buttons. An event's WINDOW is the range of
//! shifts `[lo, hi]` (`lo <= 0 <= hi`, at most `SHIFT_CAP` frames either
//! way) by which that edge ALONE can move - every other edge fixed, the
//! button's neighbouring edges not crossed - with the sequence still winning
//! at the optimum. Frame-perfect: `lo == hi == 0`. A human aiming at the
//! event's frame with a timing error `e ~ N(0, sigma)` frames, rounded to a
//! frame, hits the window with probability
//! `p = Phi((hi + 1/2) / sigma) - Phi((lo - 1/2) / sigma)` (a side at the
//! cap counts as open); the COST is `sum -ln p` - minus the log of the
//! chance to play every event right, the errors independent. So a frame-
//! perfect event costs most (`sigma = 1`: p = 0.38), a window of 3 centred
//! on the aim little (0.87), an edge whose timing does not matter nothing,
//! and every event something: holding a button through a stretch beats
//! tapping it. The conventions need no special cases: an input on a freeze
//! frame, or one that changes nothing, is a byte the DAG treats like its
//! neighbours (the same successor), so the optimizer picks the one that makes
//! no edge; jump and dash act on a PRESS (`p_jump`, `p_dash` are state), so a
//! held one does nothing new and a release is free up to the next press.
//!
//! THE OPTIMIZER (`optimize`): seeds from a dynamic programme over (state,
//! previous byte) for the FEWEST events (`fewest_events`, ties broken
//! differently per seed), plus any given sequences; each polished by steepest
//! descent (`polish`) over moves that keep it winning: setting a button over
//! a range of frames (shifts an edge, adds or removes a tap, fills a gap),
//! replacing one frame's byte, shifting a whole hold or a chord (the edges
//! of one frame); then kicks (random winning moves) and polishing again.
//! Ordered by (cost, events).
//!
//! UNAVOIDABLE frame-perfect events (`forced`): where every optimal path has
//! a button released at frame k-1 and held at k (or the reverse), every
//! optimal path has that edge there, and moving it by one frame leaves the
//! set: frame-perfect on every optimal path.

use anyhow::{ensure, Context, Result};


/// A win (the child of the last layer's links).
pub const WIN: u32 = u32::MAX;
/// No optimal path takes this byte.
const NONE: u32 = u32::MAX - 1;
/// How far a window is measured either way (frames).
pub const SHIFT_CAP: i32 = 8;
/// The input bits (`btn`): 0 left, 1 right, 2 up, 3 down, 4 jump, 5 dash.
pub const BUTTONS: [&str; 6] = ["left", "right", "up", "down", "jump", "dash"];

/// Every optimal path: `next[k][s][byte]`, the state at layer `k + 1` that
/// input `byte` leads to from state `s` of layer `k` (`WIN` at the last
/// layer, `NONE` off every optimal path). Layer 0 is the start (state 0).
pub struct Dag {
    pub opt: u32,
    next: Vec<Vec<[u32; 64]>>,
}

impl Dag {
    /// From links `(layer, parent, child, bytes)`, `child` `None` for the
    /// win; links to states that never reach the win are dropped.
    pub fn from_links(opt: u32, links: impl IntoIterator<Item = (u32, u32, Option<u32>, u64)>) -> Result<Dag> {
        ensure!(opt > 0, "an empty optimum");
        let mut next: Vec<Vec<[u32; 64]>> = vec![Vec::new(); opt as usize];
        for (k, p, c, m) in links {
            ensure!(k < opt, "a link at layer {k}, past the optimum {opt}");
            let c = match c {
                Some(c) => {
                    ensure!(k + 1 < opt, "a link from the last layer that does not win");
                    c
                }
                None => {
                    ensure!(k + 1 == opt, "a win at f{} before the optimum f{opt}", k + 1);
                    WIN
                }
            };
            let l = &mut next[k as usize];
            if l.len() <= p as usize {
                l.resize(p as usize + 1, [NONE; 64]);
            }
            for b in (0..64).filter(|b| m >> b & 1 == 1) {
                let e = &mut l[p as usize][b];
                ensure!(*e == NONE || *e == c, "input {b} from state {p} of layer {k} has two successors (an `rnd` fork): the room is not deterministic");
                *e = c;
            }
        }
        // Backward: keep the links into states that reach the win.
        let mut alive_next: Vec<bool> = Vec::new();
        for k in (0..opt as usize).rev() {
            let mut alive = vec![false; next[k].len()];
            for (s, row) in next[k].iter_mut().enumerate() {
                for e in row.iter_mut() {
                    let ok = *e == WIN || (*e != NONE && alive_next.get(*e as usize).copied().unwrap_or(false));
                    if !ok {
                        *e = NONE;
                    }
                    alive[s] |= ok;
                }
            }
            alive_next = alive;
        }
        ensure!(alive_next.first().copied().unwrap_or(false), "no path from the start wins at f{opt}");
        Ok(Dag { opt, next })
    }

    /// The text form: `opt N`, then `layer parent child bytes` per link
    /// (`child` `W` for the win, `bytes` a hex mask).
    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        use std::fmt::Write;
        let mut out = format!("# the optimal-path DAG (rewrite search --robust --save-dag)\nopt {}\n", self.opt);
        for (k, l) in self.next.iter().enumerate() {
            for (s, row) in l.iter().enumerate() {
                let mut by: Vec<(u32, u64)> = Vec::new();
                for (b, &c) in row.iter().enumerate().filter(|e| *e.1 != NONE) {
                    match by.iter_mut().find(|e| e.0 == c) {
                        Some(e) => e.1 |= 1 << b,
                        None => by.push((c, 1 << b)),
                    }
                }
                for (c, m) in by {
                    let c = if c == WIN { "W".to_string() } else { c.to_string() };
                    writeln!(out, "{k} {s} {c} {m:x}")?;
                }
            }
        }
        std::fs::write(path, out).with_context(|| format!("writing {}", path.display()))
    }

    pub fn load(path: &std::path::Path) -> Result<Dag> {
        let text = std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
        let mut lines = text.lines().filter(|l| !l.starts_with('#'));
        let opt: u32 = lines.next().and_then(|l| l.strip_prefix("opt ")).context("no `opt N` line")?.parse()?;
        let mut links = Vec::new();
        for l in lines {
            let f: Vec<&str> = l.split_whitespace().collect();
            ensure!(f.len() == 4, "a link line {l:?}");
            let c = if f[2] == "W" { None } else { Some(f[2].parse()?) };
            links.push((f[0].parse()?, f[1].parse()?, c, u64::from_str_radix(f[3], 16)?));
        }
        Dag::from_links(opt, links)
    }

    /// States per layer (some may be dead ends' leftovers: no byte).
    pub fn states(&self) -> usize {
        self.next.iter().map(|l| l.iter().filter(|r| r.iter().any(|&c| c != NONE)).count()).sum()
    }

    /// Whether `x` wins at the optimum.
    pub fn wins(&self, x: &[u8]) -> bool {
        if x.len() != self.opt as usize {
            return false;
        }
        let mut s = 0u32;
        for (k, &b) in x.iter().enumerate() {
            let Some(row) = self.next[k].get(s as usize) else { return false };
            s = row[b as usize & 63];
            if s == NONE || b >= 64 {
                return false;
            }
        }
        s == WIN
    }

    /// Per input index, the bits every optimal path has SET there and the
    /// bits every optimal path has CLEAR there.
    pub fn forced(&self) -> Vec<(u8, u8)> {
        self.next
            .iter()
            .map(|l| {
                let (mut and, mut or) = (63u8, 0u8);
                for row in l {
                    for (b, &c) in row.iter().enumerate() {
                        if c != NONE {
                            and &= b as u8;
                            or |= b as u8;
                        }
                    }
                }
                (and, !or & 63)
            })
            .collect()
    }

    /// The sequence with the FEWEST events among the optimal paths: a DP
    /// over (state, previous byte), the cost of a frame the bits that change.
    /// `seed` 0: ties to the lowest byte; otherwise to a pseudo-random one
    /// (a different optimum per seed, for the optimizer's restarts).
    pub fn fewest_events(&self, seed: u64) -> Vec<u8> {
        use celeste_engine::runtime2::mix64;
        const INF: u32 = u32::MAX;
        // events * 1024 + the tie-break noise (< 8 per frame, < 1024 in all).
        let noise = |k: usize, b: usize| if seed == 0 { 0 } else { (mix64(seed.wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ (k as u64) << 6 ^ b as u64) % 8) as u32 };
        let opt = self.opt as usize;
        // cost[k][s][prev]; back[k][s][prev] = (parent, its prev).
        let mut cost: Vec<Vec<[u32; 64]>> = Vec::with_capacity(opt);
        let mut back: Vec<Vec<[(u32, u8); 64]>> = Vec::with_capacity(opt);
        let mut c0 = vec![[INF; 64]; self.next[0].len().max(1)];
        c0[0][0] = 0;
        cost.push(c0);
        back.push(vec![[(0, 0); 64]; self.next[0].len().max(1)]);
        let mut best_win = (INF, 0u32, 0u8, 0u8);
        for k in 0..opt {
            let n_next = if k + 1 < opt { self.next[k + 1].len() } else { 0 };
            let mut nc = vec![[INF; 64]; n_next];
            let mut nb = vec![[(0u32, 0u8); 64]; n_next];
            for (s, row) in self.next[k].iter().enumerate() {
                for prev in 0..64usize {
                    let base = cost[k][s][prev];
                    if base == INF {
                        continue;
                    }
                    for (b, &c) in row.iter().enumerate() {
                        if c == NONE {
                            continue;
                        }
                        let v = base + ((prev ^ b) as u32).count_ones() * 1024 + noise(k, b);
                        if c == WIN {
                            if v < best_win.0 {
                                best_win = (v, s as u32, prev as u8, b as u8);
                            }
                        } else if v < nc[c as usize][b] {
                            nc[c as usize][b] = v;
                            nb[c as usize][b] = (s as u32, prev as u8);
                        }
                    }
                }
            }
            if k + 1 < opt {
                cost.push(nc);
                back.push(nb);
            }
        }
        let (_, mut s, mut prev, last) = best_win;
        let mut x = vec![0u8; opt];
        x[opt - 1] = last;
        for k in (1..opt).rev() {
            x[k - 1] = prev;
            let (ps, pp) = back[k][s as usize][prev as usize];
            (s, prev) = (ps, pp);
        }
        debug_assert!(self.wins(&x));
        x
    }
}

/// A button edge: at input index `at`, `button` goes down (`press`) or up.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Event {
    pub at: usize,
    pub button: usize,
    pub press: bool,
}

fn bit(x: &[u8], i: usize, b: usize) -> bool {
    x[i] >> b & 1 == 1
}

/// The edges of `x`, by index then button (before index 0: nothing held).
pub fn events(x: &[u8]) -> Vec<Event> {
    let mut out = Vec::new();
    for i in 0..x.len() {
        for b in 0..6 {
            let was = i > 0 && bit(x, i - 1, b);
            if bit(x, i, b) != was {
                out.push(Event { at: i, button: b, press: !was });
            }
        }
    }
    out
}

fn set_range(x: &mut [u8], b: usize, lo: usize, hi: usize, on: bool) {
    let hi = hi.min(x.len());
    for v in &mut x[lo..hi] {
        if on {
            *v |= 1 << b;
        } else {
            *v &= !(1 << b);
        }
    }
}

/// `x` with event `e` moved by `s` frames, every other edge kept: `None`
/// where it would reach the button's neighbouring edge (a press may not
/// merge into the previous hold nor vanish; a release may not merge into the
/// next hold - past the end it is the same as at the end).
pub fn shift(x: &[u8], e: Event, s: i32) -> Option<Vec<u8>> {
    let (n, b, i) = (x.len() as i64, e.button, e.at as i64);
    let to = i + s as i64;
    // The button's previous and next edges.
    let prev = (0..e.at).rev().find(|&j| (j > 0 && bit(x, j - 1, b)) != bit(x, j, b)).map(|j| j as i64);
    let next = (e.at + 1..x.len()).find(|&j| bit(x, j - 1, b) != bit(x, j, b)).map(|j| j as i64);
    let mut y = x.to_vec();
    if e.press {
        // Held on [i, next): the start moves within (prev, next).
        if s < 0 {
            if to < prev.map_or(0, |p| p + 1) {
                return None;
            }
            set_range(&mut y, b, to as usize, i as usize, true);
        } else {
            if to >= next.unwrap_or(n) {
                return None;
            }
            set_range(&mut y, b, i as usize, to as usize, false);
        }
    } else if s < 0 {
        // Released at i, held on [prev, i): the end moves within (prev, next).
        if to < prev? + 1 {
            return None;
        }
        set_range(&mut y, b, to as usize, i as usize, false);
    } else {
        if next.is_some_and(|nx| to >= nx) {
            return None;
        }
        set_range(&mut y, b, i as usize, to.min(n) as usize, true);
    }
    Some(y)
}

/// An event's window `[lo, hi]` (shifts that still win, contiguous around
/// 0, capped at `SHIFT_CAP`).
pub fn window(win: &impl Fn(&[u8]) -> bool, x: &[u8], e: Event) -> (i32, i32) {
    let side = |dir: i32| {
        let mut w = 0;
        for s in 1..=SHIFT_CAP {
            match shift(x, e, dir * s) {
                Some(y) if win(&y) => w = s,
                _ => break,
            }
        }
        w
    };
    (-side(-1), side(1))
}

/// The standard normal CDF (Abramowitz-Stegun 7.1.26: |error| < 1.5e-7).
fn phi(z: f64) -> f64 {
    let x = z.abs() / std::f64::consts::SQRT_2;
    let t = 1.0 / (1.0 + 0.327_591_1 * x);
    let poly = t * (0.254_829_592 + t * (-0.284_496_736 + t * (1.421_413_741 + t * (-1.453_152_027 + t * 1.061_405_429))));
    let erf = 1.0 - poly * (-x * x).exp();
    0.5 * (1.0 + if z < 0.0 { -erf } else { erf })
}

/// The chance that an aim at shift 0 with error `N(0, sigma)`, rounded,
/// lands in `[lo, hi]` (a side at the cap open).
pub fn p_hit(lo: i32, hi: i32, sigma: f64) -> f64 {
    let up = if hi >= SHIFT_CAP { 1.0 } else { phi((hi as f64 + 0.5) / sigma) };
    let down = if lo <= -SHIFT_CAP { 0.0 } else { phi((lo as f64 - 0.5) / sigma) };
    up - down
}

/// A sequence's events with their windows, and its cost.
#[derive(Clone)]
pub struct Score {
    pub events: Vec<(Event, i32, i32)>,
    pub cost: f64,
}

impl Score {
    pub fn frame_perfect(&self) -> usize {
        self.events.iter().filter(|e| e.1 == 0 && e.2 == 0).count()
    }
    pub fn min_window(&self) -> i32 {
        self.events.iter().map(|e| e.2 - e.1 + 1).min().unwrap_or(0)
    }
    pub fn log2_windows(&self) -> f64 {
        self.events.iter().map(|e| ((e.2 - e.1 + 1) as f64).log2()).sum()
    }
    /// Better: less cost (beyond rounding), then fewer events.
    pub fn better(&self, o: &Score) -> bool {
        if (self.cost - o.cost).abs() > 1e-9 {
            return self.cost < o.cost;
        }
        self.events.len() < o.events.len()
    }
}

pub fn score(win: &impl Fn(&[u8]) -> bool, x: &[u8], sigma: f64) -> Score {
    let events: Vec<(Event, i32, i32)> = events(x)
        .into_iter()
        .map(|e| {
            let (lo, hi) = window(win, x, e);
            (e, lo, hi)
        })
        .collect();
    let cost = events.iter().map(|&(_, lo, hi)| -p_hit(lo, hi, sigma).max(1e-300).ln()).sum();
    Score { events, cost }
}

/// Every candidate move of `polish` from `x` (winning or not).
fn moves(x: &[u8]) -> Vec<Vec<u8>> {
    let n = x.len();
    let mut out = Vec::new();
    // A button set or cleared over a range of up to 2 caps.
    for b in 0..6 {
        for lo in 0..n {
            for len in 1..=(2 * SHIFT_CAP as usize).min(n - lo) {
                for on in [false, true] {
                    if (lo..lo + len).any(|i| bit(x, i, b) != on) {
                        let mut y = x.to_vec();
                        set_range(&mut y, b, lo, lo + len, on);
                        out.push(y);
                    }
                }
            }
        }
    }
    // One frame's byte replaced.
    for i in 0..n {
        for v in 0..64u8 {
            if v != x[i] {
                let mut y = x.to_vec();
                y[i] = v;
                out.push(y);
            }
        }
    }
    let ev = events(x);
    // A chord (every edge of one frame) shifted together.
    let mut at: Vec<usize> = ev.iter().map(|e| e.at).collect();
    at.dedup();
    for &i in &at {
        let chord: Vec<Event> = ev.iter().filter(|e| e.at == i).copied().collect();
        if chord.len() < 2 {
            continue;
        }
        for s in (-SHIFT_CAP..=SHIFT_CAP).filter(|&s| s != 0) {
            let mut y = Some(x.to_vec());
            for e in &chord {
                y = y.and_then(|y| shift(&y, *e, s));
            }
            out.extend(y);
        }
    }
    // A whole hold (press to release) shifted.
    for (k, e) in ev.iter().enumerate().filter(|(_, e)| e.press) {
        let Some(r) = ev[k + 1..].iter().find(|r| r.button == e.button) else { continue };
        for s in (-SHIFT_CAP..=SHIFT_CAP).filter(|&s| s != 0) {
            let lo = e.at as i64 + s as i64;
            let hi = r.at as i64 + s as i64;
            if lo < 0 || hi > n as i64 {
                continue;
            }
            let mut y = x.to_vec();
            set_range(&mut y, e.button, e.at, r.at, false);
            set_range(&mut y, e.button, lo as usize, hi as usize, true);
            out.push(y);
        }
    }
    out
}

/// Steepest descent from `x` (which must win) over `moves`.
pub fn polish(dag: &Dag, x: Vec<u8>, sigma: f64) -> (Vec<u8>, Score) {
    let win = |y: &[u8]| dag.wins(y);
    let mut cur = x;
    let mut sc = score(&win, &cur, sigma);
    loop {
        let mut best: Option<(Vec<u8>, Score)> = None;
        for y in moves(&cur) {
            if !dag.wins(&y) {
                continue;
            }
            let s = score(&win, &y, sigma);
            if s.better(best.as_ref().map_or(&sc, |b| &b.1)) {
                best = Some((y, s));
            }
        }
        match best {
            Some((y, s)) => (cur, sc) = (y, s),
            None => return (cur, sc),
        }
    }
}

/// The optimizer: `seeds` fewest-event DP optima and the `given`
/// sequences, each polished; then `kicks` rounds of 1-3 random winning
/// moves from the best, polished, kept when better. Deterministic.
pub fn optimize(dag: &Dag, given: &[Vec<u8>], seeds: u64, kicks: u64, sigma: f64) -> (Vec<u8>, Score) {
    use celeste_engine::runtime2::mix64;
    let mut starts: Vec<Vec<u8>> = (0..seeds).map(|s| dag.fewest_events(s)).collect();
    starts.extend(given.iter().filter(|x| dag.wins(x)).cloned());
    starts.sort();
    starts.dedup();
    let mut best: Option<(Vec<u8>, Score)> = None;
    for x in starts {
        let (y, s) = polish(dag, x, sigma);
        if best.as_ref().is_none_or(|b| s.better(&b.1)) {
            best = Some((y, s));
        }
    }
    let (mut bx, mut bs) = best.expect("at least one seed");
    eprintln!("[playable] polished seeds: cost {:.4}, {} events, {} frame-perfect", bs.cost, bs.events.len(), bs.frame_perfect());
    let mut rng = 0x5eed_u64;
    let mut next = || {
        rng = mix64(rng.wrapping_add(0x9e37_79b9_7f4a_7c15));
        rng
    };
    for k in 0..kicks {
        let mut y = bx.clone();
        for _ in 0..1 + next() % 3 {
            let ok: Vec<Vec<u8>> = moves(&y).into_iter().filter(|z| dag.wins(z)).collect();
            if ok.is_empty() {
                break;
            }
            y = ok[(next() % ok.len() as u64) as usize].clone();
        }
        let (z, s) = polish(dag, y, sigma);
        if s.better(&bs) {
            eprintln!("[playable] kick {k}: cost {:.4} -> {:.4}, {} events, {} frame-perfect", bs.cost, s.cost, s.events.len(), s.frame_perfect());
            (bx, bs) = (z, s);
        }
    }
    (bx, bs)
}

/// The chance that `x` is played right when EVERY edge is off by its own
/// rounded `N(0, sigma)` error at once (`trials` samples; an edge moved past
/// its button's neighbour fails). Unlike `Score::cost` this sees edges that
/// fail together.
pub fn joint_success(dag: &Dag, x: &[u8], sigma: f64, trials: u32) -> f64 {
    use celeste_engine::runtime2::mix64;
    let ev = events(x);
    let mut rng = 0x7a1a_u64;
    let mut unif = || {
        rng = mix64(rng.wrapping_add(0x9e37_79b9_7f4a_7c15));
        ((rng >> 11) as f64 + 0.5) / (1u64 << 53) as f64
    };
    let mut ok = 0u32;
    'trial: for _ in 0..trials {
        // Per button its edges, moved.
        let mut y = vec![0u8; x.len()];
        for b in 0..6 {
            let mut at: Vec<i64> = Vec::new();
            for e in ev.iter().filter(|e| e.button == b) {
                let g = (-2.0 * unif().ln()).sqrt() * (2.0 * std::f64::consts::PI * unif()).cos();
                at.push(e.at as i64 + (g * sigma).round() as i64);
            }
            // A first press moved before the start is held from it.
            if let Some(a) = at.first_mut() {
                *a = (*a).max(0);
            }
            if at.windows(2).any(|w| w[0] >= w[1]) {
                continue 'trial;
            }
            for pair in at.chunks(2) {
                let lo = pair[0].min(x.len() as i64) as usize;
                let hi = pair.get(1).map_or(x.len(), |&h| h.min(x.len() as i64) as usize);
                set_range(&mut y, b, lo, hi, true);
            }
        }
        if dag.wins(&y) {
            ok += 1;
        }
    }
    ok as f64 / trials as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A three-frame room: frame 0 anything, frame 1 the dash (bit 5)
    /// pressed exactly there, frame 2 right (bit 1) held or not.
    fn toy() -> Dag {
        let any = u64::MAX;
        let dash: u64 = (0..64).filter(|b| b & 32 != 0).fold(0, |m, b| m | 1 << b);
        // Layer 0: no dash yet (a dash at frame 0 loses).
        let links = vec![(0, 0, Some(0), !dash), (1, 0, Some(0), dash), (2, 0, None, any)];
        Dag::from_links(3, links).expect("a DAG")
    }

    #[test]
    fn a_frame_perfect_press_has_window_one_and_a_free_release_is_open() {
        let d = toy();
        let win = |y: &[u8]| d.wins(y);
        let x = [0u8, 32, 32];
        assert!(d.wins(&x));
        let s = score(&win, &x, 1.0);
        // The press at 1: one frame (0 and 2 lose); no release in range.
        assert_eq!(s.events.len(), 1);
        assert_eq!((s.events[0].1, s.events[0].2), (0, 0));
        assert_eq!(s.frame_perfect(), 1);
        // Released at 2: free up to the end (moving it later is the same).
        let y = [0u8, 32, 0];
        let s = score(&win, &y, 1.0);
        assert_eq!(s.events.len(), 2);
        assert_eq!((s.events[1].1, s.events[1].2), (0, SHIFT_CAP));
        // The dash at frame 1 is forced: unavoidable.
        let f = d.forced();
        assert_eq!(f[0].1 & 32, 32);
        assert_eq!(f[1].0 & 32, 32);
    }

    #[test]
    fn the_fewest_events_path_holds_through_dont_care_frames() {
        let d = toy();
        let x = d.fewest_events(0);
        assert_eq!(events(&x).len(), 1, "{x:?}");
        let (y, s) = optimize(&d, &[vec![1, 33, 2]], 2, 3, 1.0);
        assert_eq!(s.events.len(), 1, "{y:?}");
        assert!(d.wins(&y));
    }

    #[test]
    fn an_rnd_fork_is_refused() {
        let r = Dag::from_links(2, vec![(0, 0, Some(0), 1), (0, 0, Some(1), 1), (1, 0, None, 1), (1, 1, None, 1)]);
        assert!(r.is_err());
    }

    #[test]
    fn p_hit_is_the_rounded_normal() {
        assert!((p_hit(0, 0, 1.0) - 0.3829).abs() < 1e-3);
        assert!((p_hit(-1, 1, 1.0) - 0.8664).abs() < 1e-3);
        assert!((p_hit(-SHIFT_CAP, SHIFT_CAP, 1.0) - 1.0).abs() < 1e-12);
    }
}
