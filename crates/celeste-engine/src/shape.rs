//! A SHAPE: the canonical structure a block's shape hash names, and what a
//! key is made of. One home each:
//!
//!   * `Shape`   - the structure, its `Cell`s and globals, interned: every
//!                 block of a shape shares one (`Rt2::shape`); a second
//!                 structure under a hash is FATAL
//!   * `sig`     - the signature's codec: the words a shape hash hashes and a
//!                 key space records, their one reader, and the process's
//!                 one registry of them (two signatures under a hash: FATAL)
//!   * `Anchors` - the cells a row's cell is computed from: the POSITION
//!                 object's `x`/`y`, the `room`'s, the first player's
//!   * `Layout`  - a shape's key as a key space lays it out: per cell its
//!                 `KeyRole` (structure, value, position, a dependency's
//!                 leader or follower), the record it is written as

use std::sync::Arc;

use celeste_core::arith::Arithmetic;
use serde::{Deserialize, Serialize};

use crate::exact::{Code, Deps, ShapeRecord};
use crate::runtime2::{names, Rt2, AV};

/// A cell's kind, as the signature names it (`Cell::kind`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
    Val,
    Obj,
    Arr,
    Unk,
    Clo,
    Bi,
}

/// THE SIGNATURE, word by word: every cell in canonical order, each its
/// kind's tag and what the kind names - an object `OBJ, n, (field, cell)*n`,
/// an array `ARR, n, cell*n`, a closure `CLO, function, captures`, a
/// builtin `BI, builtin` (names as their codes); a value `VAL` and an
/// unknown table `UNK` alone - then `END` and the globals present as
/// `(global, cell)` pairs. A shape's identity is the names it holds, so
/// appending a name to a table moves no shape (an absent global is not in
/// it).
pub mod sig {
    use std::sync::{Arc, RwLock};

    use super::{Cell, Kind};
    use crate::runtime2::NONE;

    pub const VAL: u32 = 1;
    pub const OBJ: u32 = 2;
    pub const ARR: u32 = 3;
    pub const UNK: u32 = 4;
    pub const CLO: u32 = 5;
    pub const BI: u32 = 6;
    pub const END: u32 = u32::MAX;

    /// Each name table's names as a signature spells them: a name's FNV-1a
    /// hash, distinct within its table (checked once), so a signature is as
    /// exact as the names themselves and independent of where a name sits in
    /// its table.
    struct NameCodes {
        fields: Vec<u32>,
        globals: Vec<u32>,
        fns: Vec<u32>,
        builtins: Vec<u32>,
    }

    fn name_codes() -> &'static NameCodes {
        static C: std::sync::OnceLock<NameCodes> = std::sync::OnceLock::new();
        C.get_or_init(|| {
            let codes = |what: &str, names: &[&str]| -> Vec<u32> {
                let fnv = |n: &str| n.bytes().fold(0x811c_9dc5u32, |h, b| (h ^ b as u32).wrapping_mul(0x0100_0193));
                let out: Vec<u32> = names.iter().map(|n| fnv(n)).collect();
                let distinct: std::collections::HashSet<u32> = out.iter().copied().collect();
                assert!(distinct.len() == out.len(), "two {what} names share a signature code");
                out
            };
            NameCodes {
                fields: codes("field", celeste_names::FIELD_NAMES),
                globals: codes("global", celeste_names::GLOBAL_NAMES),
                fns: codes("function", celeste_names::gen::FN_NAMES),
                builtins: codes("builtin", &celeste_core::builtins::BUILTIN_NAMES),
            }
        })
    }

    /// The signature of `cells` and `globals`, word by word.
    pub fn write(cells: &[Cell], globals: &[u32], mut w: impl FnMut(u32)) {
        let c = name_codes();
        for cell in cells {
            match cell {
                Cell::Val => w(VAL),
                Cell::Obj(fields) => {
                    w(OBJ);
                    w(fields.len() as u32);
                    for (k, t) in fields {
                        w(c.fields[*k as usize]);
                        w(*t);
                    }
                }
                Cell::Arr(items) => {
                    w(ARR);
                    w(items.len() as u32);
                    items.iter().for_each(|t| w(*t));
                }
                Cell::Unk => w(UNK),
                Cell::Clo { f, caps } => {
                    w(CLO);
                    w(c.fns[*f as usize]);
                    w(caps.len() as u32);
                }
                Cell::Bi(b) => {
                    w(BI);
                    w(c.builtins[*b as usize]);
                }
            }
        }
        w(END);
        for (g, &cell) in globals.iter().enumerate() {
            if cell != NONE {
                w(c.globals[g]);
                w(cell);
            }
        }
    }

    /// The signature of `cells` and `globals`.
    pub fn of(cells: &[Cell], globals: &[u32]) -> Vec<u32> {
        let mut s = Vec::with_capacity(cells.len() * 2 + 16);
        write(cells, globals, |x| s.push(x));
        s
    }

    /// The shape hash of `cells` and `globals`: their signature, hashed.
    pub fn hash(cells: &[Cell], globals: &[u32]) -> u64 {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        write(cells, globals, |x| h.write_u32(x));
        h.finish()
    }

    /// The cells' kinds of a signature: its ONE reader. A signature that is
    /// not one (an unknown tag, a cell past its end, no `END`, an odd
    /// globals part) is refused.
    pub fn kinds(sig: &[u32]) -> Result<Vec<Kind>, String> {
        let mut out = Vec::new();
        let mut i = 0;
        let at = |i: usize| sig.get(i).copied().ok_or_else(|| format!("a signature of {} words ends inside a cell, before its END", sig.len()));
        loop {
            let (kind, words) = match at(i)? {
                END => break,
                VAL => (Kind::Val, 1),
                OBJ => (Kind::Obj, 2 + 2 * at(i + 1)? as usize),
                ARR => (Kind::Arr, 2 + at(i + 1)? as usize),
                UNK => (Kind::Unk, 1),
                CLO => (Kind::Clo, 3),
                BI => (Kind::Bi, 2),
                tag => return Err(format!("a signature's cell {} has the unknown tag {tag}", out.len())),
            };
            if i + words > sig.len() {
                return Err(format!("a signature of {} words ends inside cell {}", sig.len(), out.len()));
            }
            out.push(kind);
            i += words;
        }
        if (sig.len() - i - 1) % 2 != 0 {
            return Err(format!("a signature's globals part holds {} words, not (global, cell) pairs", sig.len() - i - 1));
        }
        Ok(out)
    }

    /// The process's signatures by shape hash.
    type Registry = RwLock<rustc_hash::FxHashMap<u64, Arc<[u32]>>>;

    fn registry() -> &'static Registry {
        static R: std::sync::OnceLock<Registry> = std::sync::OnceLock::new();
        R.get_or_init(Default::default)
    }

    thread_local! {
        /// The last signature this thread found, by hash (a worker meets one
        /// shape many times running).
        static LAST: std::cell::RefCell<Option<(u64, Arc<[u32]>)>> = const { std::cell::RefCell::new(None) };
    }

    #[cold]
    #[inline(never)]
    fn collision(hash: u64) -> ! {
        panic!("exact keys: two shapes under the hash {hash:#x} (a shape-hash collision)");
    }

    /// The registered signature under `hash` that `same` accepts, else
    /// `words()` registered: THE two-shapes-under-a-hash check, process-wide
    /// (another signature under a registered hash is FATAL).
    fn intern_by(hash: u64, same: impl Fn(&[u32]) -> bool, words: impl FnOnce() -> Vec<u32>) -> Arc<[u32]> {
        let hit = LAST.with(|l| l.borrow().as_ref().filter(|(h, _)| *h == hash).map(|(_, s)| s.clone()));
        let s = match hit {
            Some(s) => s,
            None => {
                let known = registry().read().expect("the signatures").get(&hash).cloned();
                let s = match known {
                    Some(s) => s,
                    None => registry().write().expect("the signatures").entry(hash).or_insert_with(|| words().into()).clone(),
                };
                LAST.with(|l| *l.borrow_mut() = Some((hash, s.clone())));
                s
            }
        };
        if !same(&s) {
            collision(hash);
        }
        s
    }

    /// `words` as the signature under `hash` (`intern_by`).
    pub fn intern(hash: u64, words: Vec<u32>) -> Arc<[u32]> {
        intern_by(hash, |s| *s == words[..], || words.clone())
    }
}

/// One cell of a shape. A value (`Val`) holds its block's column at the
/// cell's index (`Rt2::cols`); the others are the shape's: an object's
/// fields `(name, cell)`, an array's items, a closure's function and its
/// captures' targets (each the table holding it: `trace::block`), a
/// builtin's name.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Cell {
    Val,
    Obj(Box<[(u32, u32)]>),
    Arr(Box<[u32]>),
    Unk,
    Clo { f: u32, caps: Box<[u32]> },
    Bi(u32),
}

impl Cell {
    /// The cell's kind, as its shape's signature names it.
    pub fn kind(&self) -> Kind {
        match self {
            Cell::Val => Kind::Val,
            Cell::Obj(_) => Kind::Obj,
            Cell::Arr(_) => Kind::Arr,
            Cell::Unk => Kind::Unk,
            Cell::Clo { .. } => Kind::Clo,
            Cell::Bi(_) => Kind::Bi,
        }
    }

    /// What the cell is, for messages.
    pub fn kind_name(&self) -> &'static str {
        match self.kind() {
            Kind::Val => "value",
            Kind::Obj => "object",
            Kind::Arr => "array",
            Kind::Unk => "unknown table",
            Kind::Clo => "closure",
            Kind::Bi => "builtin",
        }
    }
}

/// A SHAPE: a block's structure - its cells and the cells its globals are
/// at (`NONE`: absent) - its signature (`sig`) and its hash, the shape's
/// name. Interned: every block of a shape shares the one `Arc<Shape>`, and
/// a second structure under a hash is FATAL (`Shape::intern`).
#[derive(Debug, PartialEq, Eq)]
pub struct Shape {
    hash: u64,
    sig: Arc<[u32]>,
    cells: Box<[Cell]>,
    globals: Box<[u32]>,
}

/// The process's shapes by hash.
type Shapes = std::sync::RwLock<rustc_hash::FxHashMap<u64, Arc<Shape>>>;

fn shapes() -> &'static Shapes {
    static S: std::sync::OnceLock<Shapes> = std::sync::OnceLock::new();
    S.get_or_init(Default::default)
}

thread_local! {
    /// The last shape this thread interned (a worker meets one shape many
    /// times running).
    static LAST_SHAPE: std::cell::RefCell<Option<Arc<Shape>>> = const { std::cell::RefCell::new(None) };
}

impl Shape {
    /// THE shape of `cells` and `globals`: the registered one if they are
    /// its structure, else them registered (their signature interned:
    /// `sig::intern`). Another structure under a registered hash - another
    /// signature, or capture targets the signature does not name - is FATAL.
    pub fn intern(cells: Vec<Cell>, globals: Vec<u32>) -> Arc<Shape> {
        let hash = sig::hash(&cells, &globals);
        let collision = || -> ! { panic!("shape: two structures under the hash {hash:#x} (a shape-hash collision, or capture targets its signature does not name)") };
        let last = LAST_SHAPE.with(|l| l.borrow().as_ref().filter(|s| s.hash == hash).cloned());
        let known = last.or_else(|| shapes().read().expect("the shapes").get(&hash).cloned());
        let s = match known {
            Some(s) => {
                if s.cells[..] != cells[..] || s.globals[..] != globals[..] {
                    collision();
                }
                s
            }
            None => {
                let sig = sig::intern(hash, sig::of(&cells, &globals));
                let new = Arc::new(Shape { hash, sig, cells: cells.into_boxed_slice(), globals: globals.into_boxed_slice() });
                let s = shapes().write().expect("the shapes").entry(hash).or_insert_with(|| new.clone()).clone();
                if !Arc::ptr_eq(&s, &new) && *s != *new {
                    collision();
                }
                s
            }
        };
        LAST_SHAPE.with(|l| *l.borrow_mut() = Some(s.clone()));
        s
    }

    /// The shape's name: its signature's hash.
    #[inline]
    pub fn hash(&self) -> u64 {
        self.hash
    }

    #[inline]
    pub fn sig(&self) -> &Arc<[u32]> {
        &self.sig
    }

    #[inline]
    pub fn cells(&self) -> &[Cell] {
        &self.cells
    }

    /// Per global (`celeste_names::GLOBAL_NAMES`), the cell it is at
    /// (`NONE`: absent).
    #[inline]
    pub fn globals(&self) -> &[u32] {
        &self.globals
    }
}

/// The POSITION object (the first player instance, else the first
/// `player_spawn`, in `objects` by its `type`) and its `x`/`y` cells: the
/// whole pixels of a row's cell, its key the fraction (`KeyRole::Position`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Position {
    pub obj: u32,
    pub x: u32,
    pub y: u32,
}

/// THE CELLS A ROW'S CELL AND KEY ARE COMPUTED FROM, by the one rule each:
/// the position object's (`Position`), the `room` global's `x`/`y` (the
/// room a row is in, relative to the start room), and the first player
/// INSTANCE (the global `player` is the type table), whose remainder the
/// arcs read (not `player_spawn`'s, which is real state).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Anchors {
    pub position: Option<Position>,
    pub room: Option<[u32; 2]>,
    pub player: Option<u32>,
}

impl Anchors {
    pub fn of<N: Arithmetic>(b: &Rt2<N>) -> Anchors {
        let n = names();
        let player = b.objects_of_type(n.player).first().copied();
        let obj = player.or_else(|| b.objects_of_type(n.player_spawn).first().copied());
        let position = obj.and_then(|obj| Some(Position { obj, x: b.obj_field_cell(obj, n.x)?, y: b.obj_field_cell(obj, n.y)? }));
        Anchors { position, room: Anchors::room_of(b), player }
    }

    /// `of(b).room` alone (no object scanned).
    pub fn room_of<N: Arithmetic>(b: &Rt2<N>) -> Option<[u32; 2]> {
        let n = names();
        let r = b.global_target(n.room)?;
        Some([b.obj_field_cell(r, n.x)?, b.obj_field_cell(r, n.y)?])
    }

    /// The position's `x`/`y` cells.
    pub fn pos(&self) -> Option<(u32, u32)> {
        self.position.map(|p| (p.x, p.y))
    }

    /// The first player's table field `f`'s `x`/`y` cells (`rem`, `spd`)
    /// in `b`, the block these are the anchors of.
    pub fn player_xy<N: Arithmetic>(&self, b: &Rt2<N>, f: u32) -> Option<(u32, u32)> {
        b.xy_cells(self.player?, f)
    }
}

/// What a cell is to its shape's key (`Layout`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum KeyRole {
    /// Not a value: the shape's own (no code).
    Structure,
    /// A value keyed by its code (`Code::of`).
    Value,
    /// A position coordinate, keyed by its value less its low end's whole
    /// pixels (`Code::of_pos`: the row's cell holds those).
    Position,
    /// A value keyed by its code that its dependency's followers' codes are
    /// a function of (`exact::Deps`; the dependency's index).
    Leader(u8),
    /// A value of dependency `dep` (its `slot`-th follower): no key bits, its
    /// code recorded per leader index (`exact::Follow`) and checked.
    Follower { dep: u8, slot: u16 },
}

impl KeyRole {
    /// The code of a value `v` of a cell in this role: the code rule's one
    /// site.
    #[inline]
    pub fn code<N: Arithmetic>(self, v: AV<N>) -> Code<N> {
        match self {
            KeyRole::Position => Code::of_pos(v),
            _ => Code::of(v),
        }
    }

    /// Has the cell a dictionary and key bits?
    #[inline]
    pub fn keyed(self) -> bool {
        matches!(self, KeyRole::Value | KeyRole::Position | KeyRole::Leader(_))
    }
}

/// A SHAPE'S KEY LAYOUT: its name (hash), its signature (interned: `sig`),
/// its position cells and key dependencies, which it is recorded as
/// (`record`, a key space's `ShapeRecord`) - and per cell its `KeyRole`.
/// Two producers of one shape under different layouts are told apart like a
/// collision (`exact::KeySpace::add_shape`).
#[derive(Debug, PartialEq)]
pub struct Layout {
    hash: u64,
    sig: Arc<[u32]>,
    pos: Option<(u32, u32)>,
    deps: Vec<Deps>,
    roles: Box<[KeyRole]>,
    /// The keyed cells, in order.
    keyed: Box<[u32]>,
}

impl Layout {
    /// Block `b`'s shape (its `shape_hash` set), its key laid out as it
    /// declares (`widening::key_deps`). A layout that is none is FATAL.
    pub fn of<N: Arithmetic>(b: &Rt2<N>) -> Layout {
        Layout::of_deps(b, crate::widening::key_deps(b))
    }

    /// `of`, with the dependencies `deps` (none: every value keyed).
    pub fn of_deps<N: Arithmetic>(b: &Rt2<N>, deps: Vec<Deps>) -> Layout {
        Layout::new(b.shape.hash(), b.shape.sig().clone(), Anchors::of(b).pos(), deps).unwrap_or_else(|e| panic!("{e}"))
    }

    /// A key space's record of a shape, as its layout.
    pub fn from_record(r: &ShapeRecord) -> Result<Layout, String> {
        Layout::new(r.hash, sig::intern(r.hash, r.sig.clone()), r.pos, r.deps.clone())
    }

    /// The layout, if it is one: the position cells value cells, every
    /// dependency's followers sorted, distinct and nonempty, its leader and
    /// followers value cells, none a position, none in two places.
    fn new(hash: u64, sig: Arc<[u32]>, pos: Option<(u32, u32)>, deps: Vec<Deps>) -> Result<Layout, String> {
        let kinds = sig::kinds(&sig).map_err(|e| format!("exact keys: shape {hash:#x}: {e}"))?;
        let mut roles: Vec<KeyRole> = kinds.iter().map(|&k| if k == Kind::Val { KeyRole::Value } else { KeyRole::Structure }).collect();
        let mut set = |c: u32, role: KeyRole, what: &str| -> Result<(), String> {
            match roles.get_mut(c as usize) {
                Some(r) if *r == KeyRole::Value => {
                    *r = role;
                    Ok(())
                }
                _ => Err(format!("exact keys: shape {hash:#x}: {what} cell {c} is not a value cell, or in two places")),
            }
        };
        if let Some((x, y)) = pos {
            set(x, KeyRole::Position, "the position")?;
            set(y, KeyRole::Position, "the position")?;
        }
        for (g, d) in deps.iter().enumerate() {
            if d.followers.is_empty() || d.followers.windows(2).any(|w| w[0] >= w[1]) {
                return Err(format!("exact keys: shape {hash:#x}: followers {:?} not sorted, distinct and nonempty", d.followers));
            }
            let dep = u8::try_from(g).map_err(|_| format!("exact keys: shape {hash:#x}: {} key dependencies", deps.len()))?;
            set(d.leader, KeyRole::Leader(dep), "a dependency's")?;
            for (slot, &c) in d.followers.iter().enumerate() {
                let slot = u16::try_from(slot).map_err(|_| format!("exact keys: shape {hash:#x}: {} followers", d.followers.len()))?;
                set(c, KeyRole::Follower { dep, slot }, "a dependency's")?;
            }
        }
        let keyed = (0..roles.len() as u32).filter(|&c| roles[c as usize].keyed()).collect();
        Ok(Layout { hash, sig, pos, deps, roles: roles.into_boxed_slice(), keyed })
    }

    /// As a key space records it (byte for byte the record it was made of).
    pub fn record(&self) -> ShapeRecord {
        ShapeRecord { hash: self.hash, sig: self.sig.to_vec(), pos: self.pos, deps: self.deps.clone() }
    }

    #[inline]
    pub fn hash(&self) -> u64 {
        self.hash
    }

    #[inline]
    pub fn sig(&self) -> &Arc<[u32]> {
        &self.sig
    }

    /// The position's `x`/`y` cells (`Anchors::position`).
    #[inline]
    pub fn pos(&self) -> Option<(u32, u32)> {
        self.pos
    }

    /// The key dependencies (empty: none), in the record's order.
    #[inline]
    pub fn deps(&self) -> &[Deps] {
        &self.deps
    }

    #[inline]
    pub fn role(&self, cell: u32) -> KeyRole {
        self.roles[cell as usize]
    }

    /// `role`, `None` past the shape's cells.
    #[inline]
    pub fn get(&self, cell: u32) -> Option<KeyRole> {
        self.roles.get(cell as usize).copied()
    }

    #[inline]
    pub fn cells(&self) -> usize {
        self.roles.len()
    }

    /// The keyed cells (a dictionary each), in order.
    #[inline]
    pub fn keyed(&self) -> &[u32] {
        &self.keyed
    }

    /// Every value cell, keyed or follower, in order.
    pub fn value_cells(&self) -> impl Iterator<Item = u32> + '_ {
        (0..self.roles.len() as u32).filter(move |&c| self.roles[c as usize] != KeyRole::Structure)
    }

    /// The code of value cell `cell`'s value `v` (`KeyRole::code`).
    #[inline]
    pub fn code<N: Arithmetic>(&self, cell: u32, v: AV<N>) -> Code<N> {
        self.role(cell).code(v)
    }
}

#[cfg(test)]
mod tests {
    use super::sig::{self, kinds};
    use super::{Cell, KeyRole, Kind, Layout, Shape};
    use crate::exact::{Deps, ShapeRecord};
    use crate::runtime2::{AV, NONE, P8};

    /// Every kind, as `sig::write` writes it, reads back as its cell's; a
    /// shape's signature and hash are its cells' and globals'.
    #[test]
    fn the_signature_reads_back_its_cells_kinds() {
        let x = celeste_names::field("x");
        let cells = vec![Cell::Val, Cell::Obj(Box::new([(x, 2)])), Cell::Val, Cell::Arr(Box::new([0, 2])), Cell::Arr(Box::new([])), Cell::Unk, Cell::Clo { f: 0, caps: Box::new([1]) }, Cell::Clo { f: 1, caps: Box::new([]) }, Cell::Bi(0)];
        let mut globals = vec![NONE; celeste_names::GLOBAL_NAMES.len()];
        globals[celeste_names::global("objects") as usize] = 3;
        let shape = Shape::intern(cells.clone(), globals.clone());
        assert_eq!((shape.hash(), &shape.sig()[..]), (sig::hash(&cells, &globals), &sig::of(&cells, &globals)[..]));
        let want: Vec<Kind> = cells.iter().map(Cell::kind).collect();
        assert_eq!(kinds(shape.sig()), Ok(want));
        assert!(std::sync::Arc::ptr_eq(&shape, &Shape::intern(cells, globals)), "one shape, interned");
    }

    /// A second structure under an interned hash is FATAL, a capture target
    /// the signature does not name included.
    #[test]
    #[should_panic(expected = "two structures under the hash")]
    fn a_second_structure_under_a_shapes_hash_is_fatal() {
        let globals = vec![NONE; celeste_names::GLOBAL_NAMES.len()];
        Shape::intern(vec![Cell::Unk, Cell::Clo { f: 0, caps: Box::new([0]) }], globals.clone());
        Shape::intern(vec![Cell::Unk, Cell::Clo { f: 0, caps: Box::new([1]) }], globals);
    }

    /// What is not a signature is refused, never misread.
    #[test]
    fn a_malformed_signature_is_refused() {
        for (bad, why) in [
            (vec![sig::VAL], "no END"),
            (vec![9, sig::END], "an unknown tag"),
            (vec![sig::OBJ, 2, 7, 0, sig::END], "an object past its end"),
            (vec![sig::ARR], "an array without its length"),
            (vec![sig::VAL, sig::END, 7], "a lone globals word"),
        ] {
            assert!(kinds(&bad).is_err(), "{why}: {bad:?} read as {:?}", kinds(&bad));
        }
        assert_eq!(kinds(&[sig::END]), Ok(vec![]));
        assert_eq!(kinds(&[sig::UNK, sig::END, 3, 0]), Ok(vec![Kind::Unk]));
    }

    /// A second signature under an interned hash is FATAL; the same one is
    /// the one registered.
    #[test]
    #[should_panic(expected = "two shapes under the hash 0x5eed")]
    fn a_second_signature_under_a_hash_is_fatal() {
        let a = sig::intern(0x5eed, vec![sig::VAL, sig::END]);
        assert!(std::sync::Arc::ptr_eq(&a, &sig::intern(0x5eed, vec![sig::VAL, sig::END])));
        sig::intern(0x5eed, vec![sig::UNK, sig::END]);
    }

    /// A layout's roles: the position, a dependency's leader and followers
    /// (slots in order), the other values keyed, the rest structure; its
    /// record is the one it was made of; what is no layout is refused.
    #[test]
    fn a_layout_gives_every_cell_its_role() {
        let sig = vec![sig::VAL, sig::UNK, sig::VAL, sig::VAL, sig::VAL, sig::VAL, sig::END];
        let rec = ShapeRecord { hash: 0x1a, sig: sig.clone(), pos: Some((2, 3)), deps: vec![Deps { leader: 4, followers: vec![0, 5] }] };
        let l = Layout::from_record(&rec).expect("a layout");
        let roles: Vec<KeyRole> = (0..6).map(|c| l.role(c)).collect();
        assert_eq!(roles, [KeyRole::Follower { dep: 0, slot: 0 }, KeyRole::Structure, KeyRole::Position, KeyRole::Position, KeyRole::Leader(0), KeyRole::Follower { dep: 0, slot: 1 }]);
        assert_eq!(l.keyed(), [2, 3, 4]);
        assert_eq!(l.value_cells().collect::<Vec<_>>(), [0, 2, 3, 4, 5]);
        assert_eq!(l.record(), rec);
        let x = P8::from_raw(0x0003_8000);
        assert_eq!(l.code(2, AV::Num(x)), crate::exact::Code::of_pos(AV::Num(x)));
        assert_eq!(l.code(4, AV::Num(x)), crate::exact::Code::of(AV::Num(x)));
        for (pos, deps, why) in [
            (Some((1, 2)), vec![], "a position that is no value"),
            (Some((2, 3)), vec![Deps { leader: 2, followers: vec![4] }], "a position leading"),
            (None, vec![Deps { leader: 0, followers: vec![5, 4] }], "followers unsorted"),
            (None, vec![Deps { leader: 0, followers: vec![] }], "no followers"),
            (None, vec![Deps { leader: 0, followers: vec![4] }, Deps { leader: 5, followers: vec![4] }], "a follower in two places"),
        ] {
            assert!(Layout::from_record(&ShapeRecord { hash: 0x1a, sig: sig.clone(), pos, deps }).is_err(), "{why}");
        }
    }
}
