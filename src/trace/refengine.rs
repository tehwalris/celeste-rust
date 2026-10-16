//! `RefEngine`: the reference interpreter (`Interp<RefDomain>`) as a frame
//! step on blocks. One lane in, every fork leaf out as a keyed one-row block,
//! so a reference successor keys like a kernel row. Slow by design (one lane,
//! one path at a time).

use anyhow::{ensure, Result};

use celeste_engine::runtime2::Rt2;
use full_moon::ast;

use crate::frame::Block;
use crate::abstraction::Level;
use crate::trace::interp::Interp;
use crate::trace::block::{fn_info_of, FnInfo};
use crate::trace::refbridge::{from_block, set_buttons, to_block};
use crate::trace::refdomain::RefDomain;
use crate::trace::refdriver::run_frame_all;
use celeste_core::arith::{Arithmetic, EngineNum, Fixed, Float};
use crate::trace::heap::Value;
use crate::trace::iface;
use crate::trace::state::State;
use crate::trace::widen::{field, objects_of_type};
use crate::trace::state::ArcAxis;
use crate::search::rem::{Capture, Rem};
use celeste_core::pico8_num::Pico8Num;

/// The reference engine of arithmetic `A`: `RefEngine` is the real game's
/// (16.16), the oracle of the kernels; `FloatRefEngine` the float tools'
/// (UniversalClassicTas, Celia), the float oracle (plans/float.md).
pub struct ReferenceEngine<A: Arithmetic> {
    it: Interp<'static, RefDomain<A>>,
    /// `__reset_button_states();_update();_draw()`: the buttons are forks.
    body: &'static ast::Ast,
    /// `_update();_draw()` with no reset: the caller has set the buttons.
    body_concrete: &'static ast::Ast,
    /// The post-`_init` state: the room's start, and what a block does not carry.
    base: State<RefDomain<A>>,
    fn_info: FnInfo,
    /// The most fork paths one frame may take (`refdriver::TooManyPaths`).
    path_cap: usize,
    /// The level `run_lane` (the engine as a `FrameStep`) forks a row's
    /// unknowns at: `Level::EXACT` unless `at_level`.
    level: Level,
    /// The settings it was made under (its room, category, split frame).
    settings: std::sync::Arc<crate::settings::Settings>,
}

/// The real game's reference engine (16.16).
pub type RefEngine = ReferenceEngine<Fixed>;
/// The float tools' reference engine (Lua doubles).
pub type FloatRefEngine = ReferenceEngine<Float>;


impl<A: Arithmetic> ReferenceEngine<A> {
    /// Parse the cart, run its toplevel and `_init` for the start room of
    /// settings `s`, the engine's settings from then on.
    pub fn new(s: &crate::settings::Settings) -> Result<Self> {
        let root = std::path::Path::new(".");
        let chunks = crate::trace::cart::Chunks::parse(root, s)?.leak();
        let body: &'static ast::Ast = Box::leak(Box::new(full_moon::parse(&format!("__reset_button_states()\n{}", crate::trace::cart::FRAME_CODE))?));
        let mut it = Interp::new(RefDomain::new());
        let base = crate::trace::cart::boot(&mut it, chunks, root, s.room)?;
        let fn_info = fn_info_of(&base)?;
        Ok(ReferenceEngine { it, body, body_concrete: &chunks.frame, base, fn_info, path_cap: 1_000_000, level: Level::EXACT, settings: std::sync::Arc::new(s.clone()) })
    }

    /// The engine with `run_lane` at `level`.
    pub fn at_level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }

    /// At most `cap` fork paths a frame, else `refdriver::TooManyPaths`
    /// (default 1M): a sampled check's bound on one step's time and memory.
    pub fn set_path_cap(&mut self, cap: usize) {
        self.path_cap = cap;
    }

    /// The room's start (post-`_init`) as a state.
    pub fn start_state(&self) -> &State<RefDomain<A>> {
        &self.base
    }

    /// One whole GAME frame of a state under EVERY input: the buttons are
    /// forks (the cursor takes each value a read needs), as are the state's
    /// intervals where a branch reads them (`move`'s remainder fragments).
    /// Every leaf (`rewrite census`). Not under the split frame.
    pub fn frame_all_inputs(&mut self, st: &State<RefDomain<A>>) -> Result<Vec<State<RefDomain<A>>>> {
        anyhow::ensure!(self.settings.steps_per_frame() == 1, "frame_all_inputs: not under the split frame");
        run_frame_all(&mut self.it, self.body, st, &[], Level::EXACT, self.path_cap)
    }

    /// One whole GAME frame of a CONCRETE state under input `byte` (bit 0
    /// left, ... 5 dash): every leaf (it forks only where no input decides,
    /// `rnd`), over the split frame's two steps where it is on.
    pub fn frame_states(&mut self, st: &State<RefDomain<A>>, byte: u8) -> Result<Vec<State<RefDomain<A>>>> {
        let mut out = vec![st.clone()];
        for _ in 0..self.settings.steps_per_frame() {
            let mut next = Vec::new();
            for st in &out {
                let mut st = st.clone();
                set_buttons(&mut st, byte)?;
                next.extend(run_frame_all(&mut self.it, self.body_concrete, &st, &[], Level::EXACT, self.path_cap)?);
            }
            out = next;
        }
        Ok(out)
    }
}

impl<A: Arithmetic> ReferenceEngine<A>
where
    A::Num: EngineNum<A = A>,
{
    /// The room's start (post-`_init`) as a one-row block, without keys.
    pub fn initial(&self) -> Result<Rt2<A::Num>> {
        to_block(&self.base, self.settings.room)
    }

    /// One CONCRETE frame of row 0 of `row` under input `byte`: every leaf,
    /// keyed. It forks only where no input decides (`rnd`); unknown fields of
    /// the row are read as their placeholder, not forked.
    pub fn step(&mut self, row: &Rt2<A::Num>, byte: u8) -> Result<Vec<Block<A::Num>>> {
        Ok(self.step_reads(row, byte)?.0)
    }

    /// `step`, and the mask of the buttons it READ (`Interp::button_reads`,
    /// over every fork path). The successors are a function of the row and
    /// of the buttons read alone - the output writes every button `UBool`
    /// (`block::to_rt2`) - so an input that agrees with `byte` on the mask has
    /// the same successors.
    fn step_reads(&mut self, row: &Rt2<A::Num>, byte: u8) -> Result<(Vec<Block<A::Num>>, u8)> {
        let (mut st, _) = from_block(row, 0, &self.fn_info, &self.base)?;
        let (leaves, mask) = self.under_buttons(&mut st, byte, &[], Level::EXACT)?;
        Ok((self.blocks(&leaves)?, mask))
    }

    /// One frame of `st` under input `byte`, its `unknown` fields forked
    /// at `level`: every leaf, and the buttons the frame read.
    fn under_buttons(&mut self, st: &mut State<RefDomain<A>>, byte: u8, unknown: &[(crate::trace::heap::TableId, String)], level: Level) -> Result<(Vec<State<RefDomain<A>>>, u8)> {
        let buttons = set_buttons(st, byte)?;
        self.it.button_reads = Some((buttons, 0));
        let leaves = run_frame_all(&mut self.it, self.body_concrete, st, unknown, level, self.path_cap);
        let (_, mask) = self.it.button_reads.take().expect("set above");
        Ok((leaves.map_err(|e| e.context(ran_from(st, Some(byte))))?, mask))
    }

    /// Leaves as keyed one-row blocks.
    fn blocks(&self, leaves: &[State<RefDomain<A>>]) -> Result<Vec<Block<A::Num>>> {
        leaves.iter().map(|l| Block::canonical(to_block(l, self.settings.room)?)).collect()
    }

    /// One whole GAME frame: `step` once, or under the split frame
    /// (`Settings::steps_per_frame`) its two parts, both under `byte` (the
    /// buttons are the frame's; part a reads none). Every leaf of both.
    pub fn frame(&mut self, row: &Rt2<A::Num>, byte: u8) -> Result<Vec<Block<A::Num>>> {
        Ok(self.frame_reads(row, byte)?.0)
    }

    /// `frame`, and the buttons it read (`step_reads`; the union over its
    /// steps and their leaves): every input that agrees with `byte` there
    /// has the same successors, so the concrete search runs one of them.
    pub fn frame_reads(&mut self, row: &Rt2<A::Num>, byte: u8) -> Result<(Vec<Block<A::Num>>, u8)> {
        let (mut out, mut mask) = self.step_reads(row, byte)?;
        for _ in 1..self.settings.steps_per_frame() {
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
    pub fn step_at(&mut self, row: &Rt2<A::Num>, byte: u8, level: Level) -> Result<(Vec<Block<A::Num>>, u8)> {
        self.step_at_pinned(row, byte, level, true)
    }

    /// `step_at`, the pins (`pin_fly_fruit_motion`, `pin_isolated_floors`)
    /// optional: the test compares the two.
    fn step_at_pinned(&mut self, row: &Rt2<A::Num>, byte: u8, level: Level, pin: bool) -> Result<(Vec<Block<A::Num>>, u8)> {
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
        let (leaves, mask) = self.under_buttons(&mut st, byte, &unknown, level)?;
        if pinned_floors {
            // What the floors' pin assumes: no player went far this frame.
            let before = players(&st)?;
            for leaf in &leaves {
                for (x, y) in players(leaf)? {
                    ensure!(
                        before.iter().any(|&(bx, by)| within::<A>(x, bx, PLAYER_STEP) && within::<A>(y, by, PLAYER_STEP)),
                        "pinned floors: a player at ({x:?}, {y:?}) is more than {PLAYER_STEP} px from every player of the input {before:?}"
                    );
                }
            }
        }
        Ok((self.blocks(&leaves)?, mask))
    }

    /// `step` where the frame must not fork: the one successor.
    pub fn step_one(&mut self, row: &Rt2<A::Num>, byte: u8) -> Result<Block<A::Num>> {
        let mut out = self.step(row, byte)?;
        ensure!(out.len() == 1, "concrete frame produced {} leaves (expected exactly 1)", out.len());
        Ok(out.remove(0))
    }
}

impl<A: Arithmetic> ReferenceEngine<A>
where
    A::Num: Rem<A = A>,
{
    /// One frame of lane `lane` at the engine's level (`at_level`): every
    /// fork leaf (the buttons, and each unknown boolean of the row, are
    /// forked per path), each with its remainder TRANSFER - the
    /// interpreter's arc capture (`Interp::arc_capture`: the player's split
    /// per axis, as the tracer captures it for the kernels), decoded by the
    /// arithmetic's decoder (`Rem::transfer`: 16.16's is the kernels'
    /// `search::arc_edges::decode_axis`; a refusal is fatal, as theirs).
    pub fn run_lane(&mut self, block: &Rt2<A::Num>, lane: usize) -> Result<Vec<(Block<A::Num>, <A::Num as Rem>::Pair)>> {
        let (mut st, unknown) = from_block(block, lane, &self.fn_info, &self.base)?;
        let zero = A::point(A::from_i16(0));
        let none = || ArcAxis { took: false, pre: zero, frag: zero, ox: zero };
        st.arc = Some(Box::new([none(), none()]));
        self.it.arc_capture = true;
        let leaves = run_frame_all(&mut self.it, self.body, &st, &unknown, self.level, self.path_cap);
        self.it.arc_capture = false;
        let leaves = leaves.map_err(|e| e.context(ran_from(&st, None)))?;
        leaves
            .iter()
            .map(|l| {
                let pair = transfer_of::<A::Num>(l).map_err(|e| e.context(ran_from(&st, None)))?;
                Ok((Block::canonical(to_block(l, self.settings.room)?)?, pair))
            })
            .collect()
    }
}

impl<A: crate::transpile::garith::GraphArith> ReferenceEngine<A> {
    /// The one converter agrees across the two sides on every lane of `row`
    /// (`block::check_agreement`, from this engine's base state).
    pub fn check_agreement(&self, row: &Rt2<A::Num>) -> Result<()> {
        crate::trace::block::check_agreement::<A>(row, &self.fn_info, &self.base, self.settings.room)
    }
}

/// A leaf's remainder transfer: its arc capture and the player's remainder
/// at the frame's end (before any widening: the leaf is the interpreter's),
/// per axis a `Capture`, decoded (`Rem::transfer`).
fn transfer_of<N: Rem>(leaf: &State<RefDomain<N::A>>) -> Result<N::Pair> {
    let axes = leaf.arc.as_deref().ok_or_else(|| anyhow::anyhow!("arc capture: a leaf without one"))?;
    let players = objects_of_type(leaf, "player");
    ensure!(players.len() <= 1, "arc capture: {} player objects at the frame's end", players.len());
    let ends = |v: &<N::A as Arithmetic>::Ival| <N::A as Arithmetic>::bounds(*v);
    let axis = |k: usize| -> Result<Capture<N>> {
        let a = &axes[k];
        let fin = match players.first() {
            Some(pl) => match iface::get(leaf, &field(pl, &["rem", ["x", "y"][k]])) {
                Some(Value::Num(n)) => Some(ends(&n)),
                other => anyhow::bail!("arc capture: the player's rem.{} is {other:?}", ["x", "y"][k]),
            },
            None => None,
        };
        Ok(Capture { took: a.took, pre: ends(&a.pre), frag: ends(&a.frag), ox: ends(&a.ox), fin })
    };
    N::transfer(&axis(0)?, &axis(1)?).map_err(|e| anyhow::anyhow!("the reference's transfer is refused: {e}"))
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
fn pin_fly_fruit_motion<A: Arithmetic>(st: &mut State<RefDomain<A>>) -> Result<()> {
    use celeste_engine::widening::{Flag, Stored, TABLE};
    for e in TABLE.iter().filter(|e| e.flag == Flag::Fruit) {
        for paths in crate::trace::widen::entry_paths(st, e) {
            for (s, p) in e.slots.iter().zip(&paths) {
                match s.stored {
                    Stored::UnknownNum => ensure!(
                        matches!(iface::get(st, p), Some(Value::Num(v)) if v == A::unknown()),
                        "pinning the fly fruit's motion: {} is not the unknown number",
                        iface::show(p)
                    ),
                    Stored::Range { .. } => iface::set(st, p, Value::Num(A::point(A::from_i16(0))))?,
                    _ => {}
                }
            }
        }
    }
    Ok(())
}

/// How far a player (or its spawn) moves in one frame at most, per axis, in
/// pixels (the dash's 5 and the remainder's carry; a spring's snap stays
/// inside it), checked on every leaf of a step that pinned floors
/// (`step_at_pinned`).
const PLAYER_STEP: f64 = 8.0;
/// A floor is ISOLATED from a player at least this far (Chebyshev) at the
/// frame's start: through the frame it stays 12 away, and the player's box
/// (x+1..x+7, y+3..y+8) probed 3 to a side and 1 up or down
/// (`widen::PLAYER_PROBE`, the floor's own `check(player, +-1)`) reaches a
/// floor's 8x8 only within 10 across and 9 down or 6 up.
const PLAYER_FAR: f64 = 20.0;
/// ... and with no object but floors (and the fly fruit) closer than this
/// (a spring on it reads its `collideable` and breaks with it).
const OTHERS_FAR: f64 = 16.0;

/// `|a - b|` against `d` EXACTLY: the difference as the rounded `s` and its
/// error `e` (Knuth's TwoSum; `s + e` is the difference exactly). 16.16's
/// values are doubles whose differences are exact (`e = 0`), so this is
/// its raw comparison.
fn distance_cmp<A: Arithmetic>(a: A::Num, b: A::Num, d: f64) -> std::cmp::Ordering
where
    A::Num: EngineNum,
{
    let (x, y) = (a.to_f64(), b.to_f64());
    let (x, y) = if x >= y { (x, y) } else { (y, x) };
    // `s + e == x - y` exactly, `s >= 0`.
    let s = x - y;
    let bb = s - x;
    let e = (x - (s - bb)) + (-y - bb);
    match s.partial_cmp(&d).expect("finite coordinates") {
        std::cmp::Ordering::Equal => e.partial_cmp(&0.0).expect("finite"),
        o => o,
    }
}

/// `|a - b| >= d`, exactly.
fn far<A: Arithmetic>(a: A::Num, b: A::Num, d: f64) -> bool
where
    A::Num: EngineNum,
{
    distance_cmp::<A>(a, b, d) != std::cmp::Ordering::Less
}

/// `|a - b| <= d`, exactly.
fn within<A: Arithmetic>(a: A::Num, b: A::Num, d: f64) -> bool
where
    A::Num: EngineNum,
{
    distance_cmp::<A>(a, b, d) != std::cmp::Ordering::Greater
}

/// What a reference frame ran from, for its error: the input (`None`: the
/// buttons forked) and where every player and player spawn was.
fn ran_from<A: Arithmetic>(st: &State<RefDomain<A>>, byte: Option<u8>) -> String {
    let at: Vec<String> = ["player", "player_spawn"]
        .iter()
        .flat_map(|ty| objects_of_type(st, ty))
        .map(|o| {
            let n = |f: &str| match iface::get(st, &field(&o, &[f])) {
                Some(Value::Num(v)) => crate::trace::refdomain::show::<A>(&v),
                other => format!("{other:?}"),
            };
            format!("{} at ({}, {})", iface::show(&o), n("x"), n("y"))
        })
        .collect();
    let input = byte.map_or("every input".to_string(), |b| format!("input {b:#04x}"));
    format!("the reference frame under {input} from a state with {}", if at.is_empty() { "no player".to_string() } else { at.join(", ") })
}

/// The positions of every player and player spawn, concrete.
fn players<A: Arithmetic>(st: &State<RefDomain<A>>) -> Result<Vec<(A::Num, A::Num)>> {
    let mut out = Vec::new();
    for ty in ["player", "player_spawn"] {
        for o in objects_of_type(st, ty) {
            out.push(position(st, &o)?.ok_or_else(|| anyhow::anyhow!("{}: a player position that is not a point", iface::show(&o)))?);
        }
    }
    Ok(out)
}

/// An object's `(x, y)`, when both are points.
fn position<A: Arithmetic>(st: &State<RefDomain<A>>, obj: &iface::Path) -> Result<Option<(A::Num, A::Num)>> {
    let num = |f: &str| match iface::get(st, &field(obj, &[f])) {
        Some(Value::Num(v)) => Ok(A::as_point(v)),
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
fn pin_isolated_floors<A: Arithmetic>(st: &mut State<RefDomain<A>>) -> Result<bool>
where
    A::Num: EngineNum,
{
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
    let apart = |a: (A::Num, A::Num), b: (A::Num, A::Num), d: f64| far::<A>(a.0, b.0, d) || far::<A>(a.1, b.1, d);
    let mut any = false;
    for f in &floors {
        let ps = field(f, &["state"]);
        let Some(Value::Num(state)) = iface::get(st, &ps) else { anyhow::bail!("{}: not a number", iface::show(&ps)) };
        let Some(at) = position(st, f)? else { continue };
        if A::as_point(state).is_some()
            || !ppos.iter().all(|&p| apart(p, at, PLAYER_FAR))
            || !others.iter().all(|o| o.is_some_and(|o| ppos.contains(&o) || apart(o, at, OTHERS_FAR)))
        {
            continue;
        }
        iface::set(st, &ps, Value::Num(A::point(A::from_i16(0))))?;
        iface::set(st, &field(f, &["collideable"]), Value::Bool(true))?;
        any = true;
    }
    Ok(any)
}

/// THE REFERENCE ENGINE AS THE FORWARD'S `FrameStep` (`rewrite forward
/// --reference`, the tests): one engine a worker (an `Interp` is not
/// shared), every fork leaf one row with its source lane's cell and its
/// transfer - so its tree is the kernels': frames, edges and hulls
/// (`ckhash --edges --hulls`).
pub struct RefFrameStep<N: Rem = Pico8Num> {
    engines: Vec<std::sync::Mutex<ReferenceEngine<N::A>>>,
    level: Level,
    settings: std::sync::Arc<crate::settings::Settings>,
}

impl<N: Rem> RefFrameStep<N> {
    /// `s.threads()` engines at `level`.
    pub fn new(s: &crate::settings::Settings, level: Level) -> Result<Self> {
        let engines = (0..s.threads().max(1)).map(|_| Ok(std::sync::Mutex::new(ReferenceEngine::<N::A>::new(s)?.at_level(level)))).collect::<Result<_>>()?;
        Ok(RefFrameStep { engines, level, settings: std::sync::Arc::new(s.clone()) })
    }

    /// An engine no other worker holds (there are as many as workers; a
    /// caller past them waits on one).
    fn engine(&self) -> std::sync::MutexGuard<'_, ReferenceEngine<N::A>> {
        let start = {
            use std::hash::{Hash, Hasher};
            let mut h = rustc_hash::FxHasher::default();
            std::thread::current().id().hash(&mut h);
            h.finish() as usize
        };
        let n = self.engines.len();
        (0..n)
            .find_map(|i| self.engines[(start + i) % n].try_lock().ok())
            .unwrap_or_else(|| self.engines[start % n].lock().expect("reference engine lock"))
    }
}

impl<N: Rem> crate::frame::FrameStep<N> for RefFrameStep<N> {
    fn run(
        &self,
        block: &Block<N>,
        _cell_in: &[u32],
        lanes: &[usize],
        sink: &mut crate::storage::unit::UnitSink<N>,
    ) -> Result<()> {
        // The lanes in parallel, each on an engine of the pool (a frame of
        // one lane is milliseconds, and a unit is often a whole frontier),
        // emitted in order a chunk at a time (a lane's leaves are hundreds
        // of rows: a whole unit's held at once was gigabytes).
        let n = self.engines.len();
        for chunk in lanes.chunks(4 * n) {
            let rows = crate::storage::wave::par_map(chunk, n, |&lane| self.engine().run_lane(block.rt2(), lane));
            for (&lane, rows) in chunk.iter().zip(rows) {
                for (b, pair) in rows? {
                    sink.emit_row(b.rt2(), lane, b.positions()?[0], pair)?;
                }
            }
        }
        Ok(())
    }

    fn level(&self) -> Level {
        self.level
    }

    fn settings(&self) -> std::sync::Arc<crate::settings::Settings> {
        self.settings.clone()
    }
}

#[cfg(test)]
mod tests {
    /// The pins' distance tests are EXACT in doubles, where `a - b` rounds:
    /// TwoSum's error decides a rounded difference equal to the bound.
    #[test]
    fn distances_compare_exactly_in_doubles() {
        use celeste_core::arith::{Fixed, Float};
        use celeste_core::lua_f64::LuaF64;
        use celeste_core::pico8_num::Pico8Num as P8;
        let f = |v: f64| LuaF64::new(v).expect("finite");
        // 8 + 2^-49 is 8 apart from 0 by a hair: not within 8, far by 8.
        let a = f(8.0 + 2f64.powi(-49));
        assert!(!super::within::<Float>(a, f(0.0), 8.0) && super::far::<Float>(a, f(0.0), 8.0));
        // 2^53 - 0.5 rounds to 2^53: the exact difference is below it.
        let (x, y) = (f(2f64.powi(53)), f(0.5));
        assert_eq!(x.value() - y.value(), 2f64.powi(53), "the rounded difference");
        assert!(super::within::<Float>(x, y, 2f64.powi(53)) && !super::far::<Float>(x, y, 2f64.powi(53)));
        assert!(super::within::<Float>(y, x, 2f64.powi(53)), "symmetric");
        // 16.16: the raw comparison it replaced.
        let (p, q) = (P8::from_raw(8 << 16), P8::from_raw(-1));
        assert!(!super::within::<Fixed>(p, q, 8.0) && super::far::<Fixed>(p, q, 8.0));
        assert!(super::within::<Fixed>(p, P8::from_raw(0), 8.0) && super::far::<Fixed>(p, P8::from_raw(0), 8.0));
    }

    use super::*;

    /// A BUILTIN HELD IN SEVERAL SLOTS is a `Bi` cell at each, on both
    /// sides. The two converters disagreed here: the reference's made the
    /// first slot in traversal order the builtin's cell and pointed the
    /// others at it, the tracer's wrote it in place at every slot, so such a
    /// state keyed differently as a kernel row and as a reference row.
    #[test]
    fn a_builtin_in_several_slots_converts_alike_on_both_sides() {
        use celeste_engine::runtime2::{Cell2, Col, AV};
        let eng = RefEngine::new(&Default::default()).expect("ref engine");
        let mut st = eng.base.clone();
        let g = st.globals;
        // `abs` also in the global `flr` and in `room.x`.
        let globals = &mut st.heap.tables.get_mut(&g).expect("globals").hash;
        globals.insert("flr".to_string(), Value::Builtin("abs"));
        let Some(Value::Table(room)) = globals.get("room").cloned() else { panic!("no room table") };
        st.heap.tables.get_mut(&room).expect("room").hash.insert("x".to_string(), Value::Builtin("abs"));
        let row = to_block(&st, eng.settings.room).expect("block");
        let abs = crate::builtins::BUILTIN_NAMES.iter().position(|n| *n == "abs").expect("abs") as u32;
        assert_eq!(row.structure.iter().filter(|c| **c == Cell2::Bi(abs)).count(), 3, "a Bi cell at each of the three slots");
        let points_at_builtin = |c: &Col| matches!(c, Col::U(AV::Ptr(p)) if matches!(row.structure[*p as usize], Cell2::Bi(_)));
        assert!(!row.cols.iter().any(points_at_builtin), "no slot points at a builtin's cell");
        eng.check_agreement(&row).expect("both sides convert it alike");
    }

    /// The concrete search runs one input per class of `frame_reads`
    /// (`arc_dp::Covered`): every input that agrees with a run input on the
    /// buttons that run read must make the same successors, exactly. Checked
    /// for all 64 inputs at states along room (1,0)'s 99-frame exit.
    #[test]
    fn inputs_agreeing_on_the_buttons_read_make_the_same_successors() {
        let inputs = crate::concrete::read_inputs("tas/room_1_0_exit_frame_99.txt").expect("the exit's inputs");
        let mut eng = RefEngine::new(&Default::default()).expect("ref engine");
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
    fn pins_keep_the_projected_successors(settings: crate::settings::Settings, route: &str, stride: usize, all_inputs: bool, level: &str, what: &str) -> (usize, usize, usize) {
        let level = Level::parse(level).expect("level");
        let inputs = crate::concrete::read_inputs(route).expect("the route's inputs");
        let mut eng = RefEngine::new(&settings).expect("ref engine");
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
        let (leaves_pinned, leaves_full) = crate::par::Pool::new(16)
            .map_with(
                sampled.len(),
                || RefEngine::new(&settings),
                |eng, k| {
                    let (f, abs) = &sampled[k];
                    let (mut pinned_n, mut full_n) = (0, 0);
                    let mut ran: Vec<(u8, u8)> = Vec::new();
                    for b in if all_inputs { 0..64u8 } else { inputs[*f]..inputs[*f] + 1 } {
                        if ran.iter().any(|(r, m)| (r ^ b) & m == 0) {
                            continue;
                        }
                        let (pinned, read) = eng.step_at_pinned(abs, b, level, true)?;
                        let (full, read_full) = eng.step_at_pinned(abs, b, level, false)?;
                        assert_eq!(projected(&pinned), projected(&full), "f{f} input {b}: the pins changed the projected successors");
                        ran.push((b, read | read_full));
                        pinned_n += pinned.len();
                        full_n += full.len();
                    }
                    Ok((pinned_n, full_n))
                },
            )
            .expect("every sampled state")
            .into_iter()
            .fold((0, 0), |a, b| (a.0 + b.0, a.1 + b.1));
        eprintln!("{with} sampled states with a {what}; {leaves_pinned} leaves pinned, {leaves_full} without");
        (with, leaves_pinned, leaves_full)
    }

    /// `pin_fly_fruit_motion`, along room (6,2)'s 100% route (the fly fruit
    /// waits, takes off at the dash and is collected) at `r0sxhf` - and it
    /// saves most of the reference's paths, which is its point.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: the reference along room (6,2)'s route, ~30 CPU-s"]
    fn pinning_the_fly_fruits_motion_keeps_the_projected_successors() {
        let settings = crate::settings::Settings { room: (6, 2), hundred: true, loading_jank: Some(2), ..Default::default() };
        let (with, pinned, full) = pins_keep_the_projected_successors(settings, "tas/room_6_2_hundred_frame_93.txt", 5, true, "r0sxhf", "fly_fruit");
        assert!(with >= 8, "only {with} sampled states have the fly fruit");
        assert!(4 * pinned < full, "pinning saved little: {pinned} leaves against {full}");
    }

    /// `pin_isolated_floors`, along room (1,1)'s exit (four fall floors, the
    /// route passes by them) at `r0sxhn`, under the route's inputs (unpinned,
    /// the four floors make ~600 paths a step, ~2500 beside the player: 97k
    /// leaves, ~200 s, hence ignored; run it when the pin or the cart's floor
    /// changes).
    #[test]
    #[ignore = "pre-merge, ./check.sh all runs it: ~200 CPU-s and 16 GB"]
    fn pinning_isolated_floors_keeps_the_projected_successors() {
        let (with, pinned, full) = pins_keep_the_projected_successors(crate::settings::Settings { room: (1, 1), ..Default::default() }, "tas/room_1_1_exit_frame_94.txt", 4, false, "r0sxhn", "fall_floor");
        assert!(with >= 20, "only {with} sampled states have a fall floor");
        assert!(10 * pinned < full, "pinning saved little: {pinned} leaves against {full}");
    }
}
