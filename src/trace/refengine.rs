//! `RefEngine`: the reference interpreter (`Interp<RefDomain>`) as a frame
//! step on blocks. One lane in, every fork leaf out as a keyed one-row block,
//! so a reference successor keys like a kernel row. Slow by design (one lane,
//! one path at a time).

use anyhow::{ensure, Result};
use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::Rt2;
use full_moon::ast;

use crate::frame::Block;
use crate::abstraction::{current_level, Level};
use crate::trace::interp::Interp;
use crate::trace::refbridge::{fn_info_of, from_block, set_buttons, to_block, FnInfo};
use crate::trace::refdomain::RefDomain;
use crate::trace::refdriver::{fresh_interp, run_frame_all};
use crate::pico8_num::{Pico8Num as P8, Pico8NumInterval as Iv};
use crate::trace::heap::Value;
use crate::trace::iface;
use crate::trace::state::State;
use crate::trace::widen::{field, objects_of_type};
use crate::trace::verify::run_one;

pub struct RefEngine {
    it: Interp<'static, RefDomain>,
    /// `__reset_button_states();_update();_draw()`: the buttons are forks.
    body: &'static ast::Ast,
    /// `_update();_draw()` with no reset: the caller has set the buttons.
    body_concrete: &'static ast::Ast,
    /// The post-`_init` state: the room's start, and what a block does not carry.
    base: State<RefDomain>,
    fn_info: FnInfo,
    /// The most fork paths one frame may take (`refdriver::TooManyPaths`).
    path_cap: usize,
}

// The raw pointers inside (`Interp`'s AST references) point into the
// `'static` ASTs leaked in `new`, immutable and never freed, so moving the
// engine to another thread is sound.
unsafe impl Send for RefEngine {}

impl RefEngine {
    /// Parse the cart, run its toplevel and `_init` for the start room.
    pub fn new() -> Result<Self> {
        let leak = |src: &str| -> Result<&'static ast::Ast> { Ok(Box::leak(Box::new(full_moon::parse(src)?))) };
        let top = leak(&crate::trace::cart::sources()?)?;
        crate::trace::cart::check_absent_fields(top)?;
        let init = leak("_init()")?;
        let body = leak("__reset_button_states()\n_update()\n_draw()")?;
        let body_concrete = leak("_update()\n_draw()")?;

        let cart = Arc::new(CartData::load("cart")?);
        let (rx, ry) = crate::game_runner::start_room();
        let cache = Arc::new(CollisionCache::new(&cart, rx, ry)?);

        let mut it = fresh_interp(cart, cache);
        let st0 = crate::trace::cart::fresh_state::<RefDomain>(&mut it.d);
        let mut st0 = run_one(&mut it, top, st0)?;
        crate::trace::cart::inject_tile_flag_at(&mut st0);
        let base = run_one(&mut it, init, st0)?;
        let fn_info = fn_info_of(&base)?;
        Ok(RefEngine { it, body, body_concrete, base, fn_info, path_cap: 1_000_000 })
    }

    /// At most `cap` fork paths a frame, else `refdriver::TooManyPaths`
    /// (default 1M): a sampled check's bound on one step's time and memory.
    pub fn set_path_cap(&mut self, cap: usize) {
        self.path_cap = cap;
    }

    /// The room's start (post-`_init`) as a one-row block, without keys.
    pub fn initial(&self) -> Result<Rt2> {
        to_block(&self.base)
    }

    /// One CONCRETE frame of row 0 of `row` under input `byte`: every leaf,
    /// keyed. It forks only where no input decides (`rnd`); unknown fields of
    /// the row are read as their placeholder, not forked.
    pub fn step(&mut self, row: &Rt2, byte: u8) -> Result<Vec<Block>> {
        Ok(self.step_reads(row, byte)?.0)
    }

    /// `step`, and the mask of the buttons it READ (`Interp::button_reads`,
    /// over every fork path). The successors are a function of the row and
    /// of the buttons read alone - the output writes every button `UBool`
    /// (`refbridge`) - so an input that agrees with `byte` on the mask has
    /// the same successors.
    fn step_reads(&mut self, row: &Rt2, byte: u8) -> Result<(Vec<Block>, u8)> {
        let (mut st, _) = from_block(row, 0, &self.fn_info, &self.base)?;
        let buttons = set_buttons(&mut st, byte)?;
        self.it.button_reads = Some((buttons, 0));
        let leaves = run_frame_all(&mut self.it, self.body_concrete, &st, &[], Level::EXACT, self.path_cap);
        let (_, mask) = self.it.button_reads.take().expect("set above");
        let out = leaves?.iter().map(|l| Block::canonical(to_block(l)?)).collect::<Result<_>>()?;
        Ok((out, mask))
    }

    /// One whole GAME frame: `step` once, or under the split frame
    /// (`frame::steps_per_frame`) its two parts, both under `byte` (the
    /// buttons are the frame's; part a reads none). Every leaf of both.
    pub fn frame(&mut self, row: &Rt2, byte: u8) -> Result<Vec<Block>> {
        Ok(self.frame_reads(row, byte)?.0)
    }

    /// `frame`, and the buttons it read (`step_reads`; the union over its
    /// steps and their leaves): every input that agrees with `byte` there
    /// has the same successors, so the concrete search runs one of them.
    pub fn frame_reads(&mut self, row: &Rt2, byte: u8) -> Result<(Vec<Block>, u8)> {
        let (mut out, mut mask) = self.step_reads(row, byte)?;
        for _ in 1..crate::frame::steps_per_frame() {
            let mut next = Vec::new();
            for b in &out {
                let (o, m) = self.step_reads(b.rt2(), byte)?;
                next.extend(o);
                mask |= m;
            }
            out = next;
        }
        Ok((out, mask))
    }

    /// `step` at an abstract `level` (`rewrite arc-check`): the row's
    /// unknowns are forked as the level's kernels fork them (each unknown
    /// boolean both ways, a near floor's `state` per value, a comparison
    /// that reads an unknown number or a straddling range both ways), so the
    /// leaves PROJECTED onto `level` are every successor the level's kernels
    /// can make of the row under `byte`, not only its placeholder's. Also
    /// the buttons read (`step_reads`).
    ///
    /// Two pins, exact for a projected successor, keep the paths few: at a
    /// fruit level the fly fruit's `spd.y` and `rem.y` are 0
    /// (`pin_fly_fruit_motion`), at a near level a floor isolated from the
    /// player is in state 0 (`pin_isolated_floors`).
    pub fn step_at(&mut self, row: &Rt2, byte: u8, level: Level) -> Result<(Vec<Block>, u8)> {
        self.step_at_pinned(row, byte, level, true)
    }

    /// `step_at`, the pins (`pin_fly_fruit_motion`, `pin_isolated_floors`)
    /// optional: the test compares the two.
    fn step_at_pinned(&mut self, row: &Rt2, byte: u8, level: Level, pin: bool) -> Result<(Vec<Block>, u8)> {
        let (mut st, mut unknown) = from_block(row, 0, &self.fn_info, &self.base)?;
        let mut pinned_floors = false;
        if level.floors_near {
            // `concretize_near_floors` writes every floor's `collideable` from
            // its `state`: forking a stored unknown one first only repeats paths.
            let floors: Vec<_> = objects_of_type(&st, "fall_floor").iter().filter_map(|o| match iface::get(&st, o) {
                Some(Value::Table(t)) => Some(t),
                _ => None,
            }).collect();
            unknown.retain(|(t, k)| !(k == "collideable" && floors.contains(t)));
            if pin {
                pinned_floors = pin_isolated_floors(&mut st)?;
            }
        }
        if pin && level.fruit {
            pin_fly_fruit_motion(&mut st)?;
        }
        let buttons = set_buttons(&mut st, byte)?;
        self.it.button_reads = Some((buttons, 0));
        let leaves = run_frame_all(&mut self.it, self.body_concrete, &st, &unknown, level, self.path_cap);
        let (_, mask) = self.it.button_reads.take().expect("set above");
        let leaves = leaves?;
        if pinned_floors {
            // What the floors' pin assumes: no player went far this frame.
            let before = players(&st)?;
            for leaf in &leaves {
                for (x, y) in players(leaf)? {
                    ensure!(
                        before.iter().any(|&(bx, by)| (x - bx).abs() <= PLAYER_STEP && (y - by).abs() <= PLAYER_STEP),
                        "pinned floors: a player at ({x:?}, {y:?}) is more than {PLAYER_STEP:?} from every player of the input {before:?}"
                    );
                }
            }
        }
        let out = leaves.iter().map(|l| Block::canonical(to_block(l)?)).collect::<Result<_>>()?;
        Ok((out, mask))
    }

    /// `step` where the frame must not fork: the one successor.
    pub fn step_one(&mut self, row: &Rt2, byte: u8) -> Result<Block> {
        let mut out = self.step(row, byte)?;
        ensure!(out.len() == 1, "concrete frame produced {} leaves (expected exactly 1)", out.len());
        Ok(out.remove(0))
    }

    /// One frame of lane `lane` at the current level: every fork leaf (the
    /// buttons, and each unknown boolean of the row, are forked per path).
    pub fn run_lane(&mut self, block: &Rt2, lane: usize) -> Result<Vec<Block>> {
        let (st, unknown) = from_block(block, lane, &self.fn_info, &self.base)?;
        let leaves = run_frame_all(&mut self.it, self.body, &st, &unknown, current_level(), self.path_cap)?;
        leaves.iter().map(|l| Block::canonical(to_block(l)?)).collect()
    }
}

/// A fly fruit's `spd.y` and `rem.y` (at a fruit level the ranges
/// `FLY_FRUIT_SPD_Y`/`FLY_FRUIT_REM_Y`) as the point 0, inside both. Exact
/// for a successor projected onto the level: the two decide only whether
/// `move` runs (`spd ~= 0`) and its `y` amount, added to `y` (the unknown
/// number, which stays unknown under any addition: `RefDomain::arith`), and
/// the fruit's own next `spd.y`/`rem.y`, which the projection widens to the
/// ranges again (`y` and `step` must be the unknown number, checked). Over the
/// ranges the reference forks `move`'s `__split_by_flr` five ways and
/// `appr`'s comparison two for successors that project the same (room (6,2)
/// 100% f27: ~140 leaves a step); the test
/// `pinning_the_fly_fruits_motion_keeps_the_projected_successors` checks the
/// sets agree.
fn pin_fly_fruit_motion(st: &mut State<RefDomain>) -> Result<()> {
    use celeste_engine::widening::{Flag, Stored, TABLE};
    for e in TABLE.iter().filter(|e| e.flag == Flag::Fruit) {
        for paths in crate::trace::widen::entry_paths(st, e) {
            for (s, p) in e.slots.iter().zip(&paths) {
                match s.stored {
                    Stored::UnknownNum => ensure!(
                        matches!(iface::get(st, p), Some(Value::Num(v)) if v.low == P8::from_raw(i32::MIN) && v.high == P8::from_raw(i32::MAX)),
                        "pinning the fly fruit's motion: {} is not the unknown number",
                        iface::show(p)
                    ),
                    Stored::Range { .. } => iface::set(st, p, Value::Num(Iv::from_number(P8::from_i16(0))))?,
                    _ => {}
                }
            }
        }
    }
    Ok(())
}

/// How far a player (or its spawn) moves in one frame at most, per axis
/// (the dash's 5 and the remainder's carry; a spring's snap stays inside
/// it), checked on every leaf of a step that pinned floors
/// (`step_at_pinned`).
const PLAYER_STEP: P8 = P8::from_i16(8);
/// A floor is ISOLATED from a player at least this far (Chebyshev) at the
/// frame's start: through the frame it stays 12 away, and the player's box
/// (x+1..x+7, y+3..y+8) probed 3 to a side and 1 up or down
/// (`widen::PLAYER_PROBE`, the floor's own `check(player, +-1)`) reaches a
/// floor's 8x8 only within 10 across and 9 down or 6 up.
const PLAYER_FAR: P8 = P8::from_i16(20);
/// ... and with no object but floors (and the fly fruit) closer than this
/// (a spring on it reads its `collideable` and breaks with it).
const OTHERS_FAR: P8 = P8::from_i16(16);

/// The positions of every player and player spawn, concrete.
fn players(st: &State<RefDomain>) -> Result<Vec<(P8, P8)>> {
    let mut out = Vec::new();
    for ty in ["player", "player_spawn"] {
        for o in objects_of_type(st, ty) {
            out.push(position(st, &o)?.ok_or_else(|| anyhow::anyhow!("{}: a player position that is not a point", iface::show(&o)))?);
        }
    }
    Ok(out)
}

/// An object's `(x, y)`, when both are points.
fn position(st: &State<RefDomain>, obj: &iface::Path) -> Result<Option<(P8, P8)>> {
    let num = |f: &str| match iface::get(st, &field(obj, &[f])) {
        Some(Value::Num(v)) => Ok(v.to_number()),
        other => Err(anyhow::anyhow!("{}.{f}: not a number: {other:?}", iface::show(obj))),
    };
    Ok(num("x")?.zip(num("y")?))
}

/// At a near level, every widened fall floor (`state` a range) ISOLATED -
/// every player and spawn `PLAYER_FAR` away, no other object but floors and
/// the fly fruit within `OTHERS_FAR` - pinned to `state` 0 (`collideable`
/// true, as `concretize_near_floors` derives it). Exact for a successor
/// projected onto the level: such a floor's update writes only its own fields
/// (`check(player)` misses, so it never breaks itself or a spring), which
/// the projection widens again (no player overlaps it at the end); the
/// others read only its `collideable` in `collide`, whose position test
/// then misses (only the player moves among solid objects, and a spring
/// would sit within `OTHERS_FAR`). Unpinned, the reference enumerates each
/// such floor's three states as a PRODUCT (room (3,0), 12 floors: over 1M
/// paths a step). Whether any floor was pinned; the caller checks
/// `PLAYER_STEP` on the leaves.
fn pin_isolated_floors(st: &mut State<RefDomain>) -> Result<bool> {
    let ppos = players(st)?;
    let floors = objects_of_type(st, "fall_floor");
    // The fly fruit is not solid and collides with the player alone (its `y`
    // is unknown at a fruit level).
    let fly_fruits = objects_of_type(st, "fly_fruit");
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return Ok(false) };
    let n = st.heap.tables[&objects].arr.len();
    let mut others = Vec::new();
    for i in 0..n {
        let o = vec![iface::key("objects"), iface::Step::Idx(i)];
        // (A destroyed object's slot is nil until `del` closes it.)
        let live = matches!(iface::get(st, &o), Some(Value::Table(_)));
        if live && !floors.contains(&o) && !fly_fruits.contains(&o) {
            // An object not at a point is near everything.
            others.push(position(st, &o)?);
        }
    }
    let apart = |a: (P8, P8), b: (P8, P8), d: P8| (a.0 - b.0).abs() >= d || (a.1 - b.1).abs() >= d;
    let mut any = false;
    for f in &floors {
        let ps = field(f, &["state"]);
        let Some(Value::Num(state)) = iface::get(st, &ps) else { anyhow::bail!("{}: not a number", iface::show(&ps)) };
        let Some(at) = position(st, f)? else { continue };
        if state.to_number().is_some()
            || !ppos.iter().all(|&p| apart(p, at, PLAYER_FAR))
            || !others.iter().all(|o| o.is_some_and(|o| ppos.contains(&o) || apart(o, at, OTHERS_FAR)))
        {
            continue;
        }
        iface::set(st, &ps, Value::Num(Iv::from_number(P8::from_i16(0))))?;
        iface::set(st, &field(f, &["collideable"]), Value::Bool(true))?;
        any = true;
    }
    Ok(any)
}

/// The interpreter as the trusted `FrameStep`, behind a lock (the `Interp` is
/// reused). Each fork leaf is one row, emitted with its source lane's cell.
impl crate::frame::FrameStep for std::sync::Mutex<RefEngine> {
    fn run(
        &self,
        block: &Block,
        cell_in: &[u32],
        lanes: &[usize],
        sink: &mut crate::storage::unit::UnitSink,
    ) -> Result<()> {
        let mut engine = self.lock().expect("reference engine lock");
        for &lane in lanes {
            for b in engine.run_lane(block.rt2(), lane)? {
                let cell_out = b.positions()?[0];
                if sink.edges_on {
                    sink.pos_edges.insert((cell_in[lane], cell_out));
                }
                sink.emit_row(b.rt2(), cell_out)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The concrete search runs one input per class of `frame_reads`
    /// (`arc_dp::Covered`): every input that agrees with a run input on the
    /// buttons that run read must make the same successors, exactly. Checked
    /// for all 64 inputs at states along room (1,0)'s 99-frame exit.
    #[test]
    fn inputs_agreeing_on_the_buttons_read_make_the_same_successors() {
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        let inputs = crate::concrete::read_inputs("tas/room_1_0_exit_frame_99.txt").expect("the exit's inputs");
        let mut eng = RefEngine::new().expect("ref engine");
        let exact = |bs: Vec<Block>| -> Vec<(celeste_engine::exact::ExactRow, u32)> { bs.iter().map(|b| (celeste_engine::exact::exact_rows(b.rt2(), crate::compiled::ids()).swap_remove(0), b.positions().expect("cells")[0])).collect() };
        let mut row = eng.initial().expect("initial state");
        let (mut runs, mut checked) = (0, 0);
        for (f, &byte) in inputs.iter().enumerate() {
            if f % 4 == 0 {
                let mut ran: Vec<(u8, u8, Vec<(celeste_engine::exact::ExactRow, u32)>)> = Vec::new();
                for b in 0..64u8 {
                    let (succ, read) = eng.frame_reads(&row, b).expect("frame");
                    let succ = exact(succ);
                    if let Some((r, _, s)) = ran.iter().find(|(r, m, _)| (r ^ b) & m == 0) {
                        assert_eq!(&succ, s, "f{f}: input {b} agrees with input {r} on the buttons read, but not in its successors");
                        checked += 1;
                    } else {
                        ran.push((b, read, succ));
                    }
                }
                runs += ran.len();
            }
            row = eng.step_one(&row, byte).expect("frame").into_rt2();
        }
        // The point of it: most inputs are repeats (the spawn and freeze
        // frames read no button; up/down only matter where a dash starts).
        eprintln!("{runs} inputs run, {checked} covered");
        assert!(checked > 2 * runs, "{checked} inputs covered, {runs} run");
    }

    /// Along a route, every `stride`-th state projected onto `level` makes
    /// the same successors projected onto it, for every input (`all_inputs`,
    /// else the route's own), with `step_at`'s pins as without them. Returns
    /// (states with a `what`, leaves pinned, leaves without).
    fn pins_keep_the_projected_successors(room: &str, env: &[(&str, &str)], route: &str, stride: usize, all_inputs: bool, level: &str, what: &str) -> (usize, usize, usize) {
        std::env::set_var("CELESTE_START_ROOM", room);
        for (k, v) in env {
            std::env::set_var(k, v);
        }
        let level = Level::parse(level).expect("level");
        let inputs = crate::concrete::read_inputs(route).expect("the route's inputs");
        let mut eng = RefEngine::new().expect("ref engine");
        // The projections compared EXACTLY (their canonical bytes).
        let projected = |bs: &[Block]| -> std::collections::BTreeSet<(celeste_engine::exact::ExactRow, u32)> {
            bs.iter()
                .map(|b| {
                    let mut w = b.rt2().clone_block();
                    crate::frame::widen_rt2_to(&mut w, level);
                    w.canonical();
                    (celeste_engine::exact::exact_rows(&w, crate::compiled::ids()).swap_remove(0), crate::search::pos_graph::block_cells(&w).expect("cells")[0])
                })
                .collect()
        };
        // The route's sampled states (concrete steps, cheap), then each
        // checked on its own engine (the unpinned steps are the cost).
        let mut row = eng.initial().expect("initial state");
        let mut with = 0;
        let mut sampled: Vec<(usize, Rt2)> = Vec::new();
        for (f, &byte) in inputs.iter().enumerate() {
            if f % stride == 0 {
                let mut abs = row.clone_block();
                crate::frame::widen_rt2_to(&mut abs, level);
                if !objects_of_type(&from_block(&abs, 0, &eng.fn_info, &eng.base).expect("state").0, what).is_empty() {
                    with += 1;
                }
                sampled.push((f, abs));
            }
            row = eng.step_one(&row, byte).expect("frame").into_rt2();
        }
        let next = std::sync::atomic::AtomicUsize::new(0);
        let workers = std::thread::available_parallelism().map_or(4, |n| n.get()).min(16).min(sampled.len()).max(1);
        let (leaves_pinned, leaves_full) = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..workers)
                .map(|_| {
                    sc.spawn(|| {
                        let mut eng = RefEngine::new().expect("ref engine");
                        let (mut pinned_n, mut full_n) = (0, 0);
                        while let Some((f, abs)) = sampled.get(next.fetch_add(1, std::sync::atomic::Ordering::Relaxed)) {
                            let mut ran: Vec<(u8, u8)> = Vec::new();
                            for b in if all_inputs { 0..64u8 } else { inputs[*f]..inputs[*f] + 1 } {
                                if ran.iter().any(|(r, m)| (r ^ b) & m == 0) {
                                    continue;
                                }
                                let (pinned, read) = eng.step_at_pinned(abs, b, level, true).expect("pinned step");
                                let (full, read_full) = eng.step_at_pinned(abs, b, level, false).expect("full step");
                                assert_eq!(projected(&pinned), projected(&full), "f{f} input {b}: the pins changed the projected successors");
                                ran.push((b, read | read_full));
                                pinned_n += pinned.len();
                                full_n += full.len();
                            }
                        }
                        (pinned_n, full_n)
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a pin worker panicked")).fold((0, 0), |a, b| (a.0 + b.0, a.1 + b.1))
        });
        eprintln!("{with} sampled states with a {what}; {leaves_pinned} leaves pinned, {leaves_full} without");
        (with, leaves_pinned, leaves_full)
    }

    /// `pin_fly_fruit_motion`, along room (6,2)'s 100% route (the fly fruit
    /// waits, takes off at the dash and is collected) at `r0sxhf` - and it
    /// saves most of the reference's paths, which is its point.
    #[test]
    fn pinning_the_fly_fruits_motion_keeps_the_projected_successors() {
        let env = [("CELESTE_HUNDRED", "1"), ("CELESTE_LOADING_JANK", "2")];
        let (with, pinned, full) = pins_keep_the_projected_successors("6,2", &env, "tas/room_6_2_hundred_frame_93.txt", 5, true, "r0sxhf", "fly_fruit");
        assert!(with >= 8, "only {with} sampled states have the fly fruit");
        assert!(4 * pinned < full, "pinning saved little: {pinned} leaves against {full}");
    }

    /// `pin_isolated_floors`, along room (1,1)'s exit (four fall floors, the
    /// route passes by them) at `r0sxhn`, under the route's inputs (unpinned,
    /// the four floors make ~600 paths a step, ~2500 beside the player: 97k
    /// leaves, ~200 s, hence ignored; run it when the pin or the cart's floor
    /// changes).
    #[test]
    #[ignore]
    fn pinning_isolated_floors_keeps_the_projected_successors() {
        let (with, pinned, full) = pins_keep_the_projected_successors("1,1", &[], "tas/room_1_1_exit_frame_94.txt", 4, false, "r0sxhn", "fall_floor");
        assert!(with >= 20, "only {with} sampled states have a fall floor");
        assert!(10 * pinned < full, "pinning saved little: {pinned} leaves against {full}");
    }
}
