//! `rewrite beam`: a BEAM SEARCH for ANY route out of the start room, over
//! EXACT concrete states (the reference engine, every input, every `rnd`
//! leaf). NOT a proof and NOT part of it: it keeps the `width` best states
//! a frame by a heuristic and drops the rest, so its route is real (every
//! state is a concrete game state) but NOT optimal, and failing to find one
//! proves nothing. For a room the proof pipeline cannot finish in time (a
//! map mod's room, plans/map-mod.md); `rewrite search --to H` on the route's
//! length afterwards is the proof, if it fits.
//!
//! The heuristic: level -1's d (`--level-minus-one`'s table: the fewest
//! frames to the exit from the state's (shape, cell), a lower bound; `None`
//! off the table ranks last, no exit dropped), then a dash available, then
//! height (smaller y), then speed. `per_cell` caps the states kept at one
//! player cell, for diversity (the remainder, the speed and the objects
//! otherwise fill the beam at a few cells).

use anyhow::Result;
use celeste_engine::runtime2::{Rt2, AV};
use rustc_hash::FxHashSet;

use crate::frame::{wins_of, Block};

/// What `beam` runs.
pub struct BeamArgs {
    /// States kept a frame.
    pub width: usize,
    /// At most this many kept states at one player cell (0: no cap).
    pub per_cell: usize,
    /// Give up after this many frames.
    pub max_frames: u32,
    /// The level whose shapes the level -1 table is keyed by (the table's
    /// rows are a forward's, widened to it).
    pub level: crate::abstraction::Level,
}

/// A found route: the inputs from the start (frame 1 = `inputs[0]`); the
/// room is left on the last one's frame.
pub struct Route {
    pub inputs: Vec<u8>,
}

/// Per kept state: its rank (smaller is better), its row, its link back.
struct Cand {
    rank: (u32, u8, i32, i32),
    cell: u32,
    row: Rt2,
    parent: u32,
    byte: u8,
}

/// The heuristic's rank of a state `b` at cell `cell`: `None` drops it (no
/// exit from its node, or a DEATH: no player once one has existed,
/// `spawned` - a route that dies is no TAS).
fn rank(b: &Block, cell: u32, args: &BeamArgs, table: Option<&crate::trace::level_minus_one::CostToGo>, seeded: bool, spawned: bool) -> Result<Option<(u32, u8, i32, i32)>> {
    if spawned && b.rt2().player_objects(crate::compiled::ids()).is_empty() {
        return Ok(None);
    }
    let d = match table {
        Some(t) => {
            let mut w = super::arc_dp::lookup_view(b.rt2(), seeded)?;
            crate::frame::widen_rt2_to(&mut w, args.level);
            w.canonical();
            match t.d_of(w.shape_hash, cell) {
                Some(u32::MAX) => return Ok(None),
                Some(d) => d,
                None => u32::MAX - 1,
            }
        }
        None => 0,
    };
    let rt2 = b.rt2();
    let ids = crate::compiled::ids();
    let num = |c: usize| match rt2.cols[c].at(0) {
        AV::Num(n) => n.as_raw_u32() as i32,
        _ => 0,
    };
    let (mut dash, mut y, mut speed) = (0u8, i32::MAX, 0i32);
    if let Some(&obj) = rt2.player_objects(ids).first() {
        let djump = celeste_names::field_id("djump").expect("the `djump` field");
        if let Some(c) = rt2.obj_field_cell(obj, djump) {
            dash = u8::from(num(c as usize) > 0);
        }
        if let Some((_, cy)) = rt2.player_xy_cells(ids, ids.f_y) {
            y = num(cy);
        }
        if let Some((sx, sy)) = rt2.player_xy_cells(ids, ids.f_spd) {
            speed = num(sx).abs() + num(sy).abs();
        }
    }
    Ok(Some((d, 1 - dash, y, -speed)))
}

/// The beam search from the start room under settings `s` (`--cart`, the
/// concrete balloon seeds, the room). `Ok(None)`: no route within
/// `max_frames` (proves nothing).
pub fn beam(args: &BeamArgs, s: &crate::settings::Settings) -> Result<Option<Route>> {
    use crate::trace::refengine::RefEngine;
    let workers = s.threads().max(1);
    let (engines, seeded) = super::arc_dp::concrete_engines(workers, s)?;
    let engines: Vec<std::sync::Mutex<RefEngine>> = engines.into_iter().map(std::sync::Mutex::new).collect();
    let table = s.minus_one().map(|m| m.table(s));
    if let Some(t) = table {
        eprintln!("[beam] level -1 table: {} entries, the start's d {}", t.len(), t.start_d);
    }
    let start = Block::canonical(engines[0].lock().expect("an engine").initial()?)?;
    let mut back: Vec<Vec<(u32, u8)>> = Vec::new();
    let mut cur: Vec<Rt2> = vec![start.into_rt2()];
    let t0 = std::time::Instant::now();
    let mut spawned = false;
    for k in 0..args.max_frames {
        let t = std::time::Instant::now();
        const CHUNK: usize = 16;
        let next_chunk = std::sync::atomic::AtomicUsize::new(0);
        type Part = Result<(Vec<Cand>, Option<(u32, u8)>)>;
        let parts: Vec<Part> = std::thread::scope(|sc| {
            let hs: Vec<_> = engines
                .iter()
                .map(|engine| {
                    let (cur, next_chunk) = (&cur, &next_chunk);
                    sc.spawn(move || -> Part {
                        let mut eng = engine.lock().expect("an engine");
                        let mut out = Vec::new();
                        loop {
                            let lo = next_chunk.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cur.len() {
                                return Ok((out, None));
                            }
                            for (p, row) in cur.iter().enumerate().skip(lo).take(CHUNK) {
                                let mut ran = super::arc_dp::Covered::default();
                                for byte in 0u8..64 {
                                    if ran.covers(byte) {
                                        continue;
                                    }
                                    let (succ, read) = eng.frame_reads(row, byte)?;
                                    ran.push(byte, read);
                                    for b in succ {
                                        if wins_of(b.rt2(), s)?.iter().any(|&x| x) {
                                            return Ok((out, Some((p as u32, byte))));
                                        }
                                        let cell = b.positions()?[0];
                                        let Some(r) = rank(&b, cell, args, table, seeded, spawned)? else { continue };
                                        out.push(Cand { rank: r, cell, row: b.into_rt2(), parent: p as u32, byte });
                                    }
                                }
                            }
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a beam worker panicked")).collect()
        });
        let mut cands: Vec<Cand> = Vec::new();
        let mut won: Option<(u32, u8)> = None;
        for p in parts {
            let (c, w) = p?;
            cands.extend(c);
            // The smallest (parent, input) win, for determinism.
            if let Some(w) = w {
                won = Some(won.map_or(w, |o: (u32, u8)| o.min(w)));
            }
        }
        if let Some((parent, byte)) = won {
            let mut inputs = vec![byte];
            let mut p = parent;
            for layer in back.iter().rev() {
                let (pp, b) = layer[p as usize];
                inputs.push(b);
                p = pp;
            }
            inputs.reverse();
            eprintln!("[beam] EXIT on frame {} ({:.1} s)", inputs.len(), t0.elapsed().as_secs_f64());
            return Ok(Some(Route { inputs }));
        }
        // Best first; ties by (parent, input) order: deterministic.
        cands.sort_by(|a, b| a.rank.cmp(&b.rank).then(a.parent.cmp(&b.parent)).then(a.byte.cmp(&b.byte)));
        let n = cands.len();
        let mut seen: FxHashSet<celeste_engine::exact::ExactRow> = FxHashSet::default();
        let mut at_cell: rustc_hash::FxHashMap<u32, usize> = Default::default();
        let (mut next, mut links) = (Vec::new(), Vec::new());
        let mut best = None;
        for c in cands {
            if next.len() >= args.width {
                break;
            }
            if args.per_cell > 0 && at_cell.get(&c.cell).copied().unwrap_or(0) >= args.per_cell {
                continue;
            }
            let b = Block::from_rt2(c.row);
            if !seen.insert(super::arc_dp::exact_state(&b, c.cell)) {
                continue;
            }
            *at_cell.entry(c.cell).or_default() += 1;
            best.get_or_insert((c.rank, c.cell));
            next.push(b.into_rt2());
            links.push((c.parent, c.byte));
        }
        let (r, cell) = best.unwrap_or(((u32::MAX, 0, 0, 0), 0));
        eprintln!(
            "[beam] f{:3}: {} successors -> {} kept ({} cells); best d {} dash {} at {:?}; {:.1} s",
            k + 1,
            n,
            next.len(),
            at_cell.len(),
            r.0,
            1 - r.1,
            super::pos_graph::cell_xy(cell),
            t.elapsed().as_secs_f64()
        );
        if next.is_empty() {
            eprintln!("[beam] no state left after f{}", k + 1);
            return Ok(None);
        }
        spawned = spawned || next.iter().any(|r| !r.player_objects(crate::compiled::ids()).is_empty());
        back.push(links);
        cur = next;
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    /// Room (1,0), the original map, the arc gate's synthetic win (9,101)
    /// (optimum 33): a narrow beam (the reference engine, every input),
    /// ranked by level -1's d, finds a route, and it replays to the win on
    /// its last frame.
    #[test]
    fn a_beam_finds_a_route_to_room_10s_gate_win() {
        let s = crate::settings::Settings { threads: Some(4), win_at: Some((9, 9, 101, 101)), level_minus_one: Some((60, 5)), ..Default::default() };
        let args = super::BeamArgs { width: 64, per_cell: 2, max_frames: 60, level: crate::abstraction::Level::EXACT };
        let r = super::beam(&args, &s).expect("beam").expect("a route to (9,101)");
        assert!(r.inputs.len() >= 33, "no route beats the optimum 33: {}", r.inputs.len());
        let mut eng = crate::trace::refengine::RefEngine::new(&s).expect("engine");
        let mut row = crate::frame::Block::canonical(eng.initial().expect("initial")).expect("canonical").into_rt2();
        for (i, &b) in r.inputs.iter().enumerate() {
            let next = eng.step_one(&row, b).expect("one successor");
            let won = crate::frame::wins_of(next.rt2(), &s).expect("wins")[0];
            assert_eq!(won, i + 1 == r.inputs.len(), "the route wins on its last frame, not frame {}", i + 1);
            row = next.into_rt2();
        }
    }
}
