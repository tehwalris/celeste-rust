//! THE WIDENING TABLE: every field a level stores as something other than
//! its exact value, what it stores, what that owes, and why it is sound.
//!
//! One declarative table (`TABLE`), interpreted twice: the tracer turns each
//! entry into graph ops and owed errors at the frame's end
//! (`trace::widen`), and the block side evaluates the same entry over
//! columns (`Rt2::widen_to`: the concrete search's node lookup, the objects
//! ladder's projection, the boundary). Neither side has a widening the table
//! does not name. A LEVEL (`Level`) is the set of entries whose `Flag` it
//! has; `Flag::Always` entries apply at every level.
//!
//! Two kinds (`Kind`): an EXACT canonicalization loses nothing the search
//! observes (each says why in `why`); an OVER-APPROXIMATION is refuted by
//! something exact (the arcs for the remainder, the concrete search for the
//! rest; CLAUDE.md "Never widen a field without something EXACT that refutes
//! it").
//!
//! What a table entry cannot say is a HOOK (`Hook`), named in its entry and
//! documented where it is implemented: the near floors (the stored value
//! depends on the player's position per lane) and the platforms' input
//! (bound to the platform worlds).

use crate::runtime2::{collapse_uniform, BoundaryIds, Col, Rt2, AV, NONE, P8};

/// A LEVEL: which objects the forward widens (plans/abstractions.md). Each
/// widening over-approximates; the concrete search refutes it.
/// `Default` is `Level::EXACT`.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct Level {
    /// `h`: `p_jump` / `p_dash` unknown at the boundary, forked both ways in
    /// the frame (a held button may then re-trigger a press).
    pub held: bool,
    /// `f`: the fly fruit's `step` and `y` unknown numbers, `spd.y` / `rem.y`
    /// their whole ranges, `fly` unknown.
    pub fruit: bool,
    /// `n`: the countdowns unknown, each fall floor's `state` [0, 2] and
    /// `collideable` unknown except where the player overlaps it at the
    /// frame's end, the objects' phases their ranges.
    pub floors_near: bool,
    /// `p`: every moving platform's `x` and `last` the interval of its whole
    /// path, its `rem.x` the whole remainder (its phase forgotten).
    pub platforms: bool,
}

impl Level {
    /// Every object exact (`r0sx`).
    pub const EXACT: Level = Level { held: false, fruit: false, floors_near: false, platforms: false };

    /// A level from `r0sx[h][f][n][p]`: the fixed prefix, then the flags in
    /// that order (each absent = exact).
    pub fn parse(spec: &str) -> Result<Level, String> {
        let s = spec.trim();
        let mut rest = s
            .strip_prefix("r0sx")
            .ok_or_else(|| format!("level {spec:?}: expected r0sx[h][f][n][p] (the remainder is the arcs', the speed exact)"))?;
        let mut flag = |c: char| -> bool {
            match rest.strip_prefix(c) {
                Some(t) => {
                    rest = t;
                    true
                }
                None => false,
            }
        };
        let level = Level { held: flag('h'), fruit: flag('f'), floors_near: flag('n'), platforms: flag('p') };
        if !rest.is_empty() {
            return Err(format!("level {spec:?}: unexpected {rest:?} (flags are h, f, n, p in that order)"));
        }
        Ok(level)
    }

    /// Does this level apply the entries of `flag`?
    pub fn has(&self, flag: Flag) -> bool {
        match flag {
            Flag::Always => true,
            Flag::Held => self.held,
            Flag::Fruit => self.fruit,
            Flag::Near => self.floors_near,
            Flag::Platforms => self.platforms,
        }
    }

    /// The table's entries this level applies, in table order.
    pub fn entries(self) -> impl Iterator<Item = &'static Entry> {
        TABLE.iter().filter(move |e| self.has(e.flag))
    }
}

impl std::fmt::Display for Level {
    /// The spec `parse` reads.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r0sx")?;
        for (on, c) in [(self.held, 'h'), (self.fruit, 'f'), (self.floors_near, 'n'), (self.platforms, 'p')] {
            if on {
                write!(f, "{c}")?;
            }
        }
        Ok(())
    }
}

/// Which level flag turns an entry on (`Level::has`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Flag {
    /// Every level, `r0sx` included.
    Always,
    /// `h`.
    Held,
    /// `f`.
    Fruit,
    /// `n`.
    Near,
    /// `p`.
    Platforms,
}

/// Where an entry's slots live.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Target {
    /// Every instance in `objects` whose `type` is this global's table; a
    /// slot's field path is below the instance.
    Objects(&'static str),
    /// Globals: a slot's field path starts at a global.
    Globals,
}

/// How a containment obligation is discharged in the traced graph. On
/// blocks every one is a per-lane check (an assertion).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Proof {
    /// Checked per lane, always (the domain's own comparisons).
    PerLane,
    /// Proved from the graph where it can be (literals, bounded inputs,
    /// sums, select arms under their branches' facts), else per lane.
    Static,
    /// Proved where every value a lane can hold is a literal inside (a
    /// select tree of literals), else per lane on the value's bounds.
    Literals,
}

/// What a slot STORES at the frame's end.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Stored {
    /// The interval `[lo, hi]` (raw 16.16); owed: the value lies inside.
    Range { lo: i32, hi: i32, proof: Proof },
    /// `[s - radius, s + radius]` around the object's number field
    /// `around` (`s`); owed: the value lies inside.
    Band { around: &'static str, radius: i32, proof: Proof },
    /// A PHASE: an integer read only through a function periodic in it, as
    /// the interval `[lo, hi]` of one period. Nothing owed: the entry's
    /// `why` names the periodicity.
    Phase { lo: i32, hi: i32 },
    /// `max(v, lo)`: every value at most `lo` reads alike.
    AtLeast(i32),
    /// The unknown number (`AV::UNum`), which contains every value.
    UnknownNum,
    /// The unknown boolean (`AV::UBool`).
    UnknownBool,
    /// A constant: the slot is DEAD (nothing observed reads it).
    Num(i32),
    /// A constant boolean: the slot is dead.
    Bool(bool),
    /// A full-period interval (width at least `period`, raw) stored as the
    /// canonical `[0, period]`; owed: the width. A point is left as it is.
    FullPeriod(i32),
    /// The stored value of the entry's slot named here; owed: equal to it.
    SameAs(&'static str),
    /// An absent field written as the number 0, in every frame's outcome
    /// whether the level widens or not (it is heap SHAPE): `trace::widen::
    /// materialize_absent_fields`. Nothing on blocks (a row a frame made
    /// carries it; a start row lacks it on both sides).
    AbsentAsZero,
}

/// How the frame READS a slot (the input side, `trace::widen::read_inputs`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Input {
    /// As stored: the input cell (an interval cell where the stored value is
    /// an interval or the unknown boolean).
    Stored,
    /// Both values on every lane: a 2-way fork with no validity (a decided
    /// boolean only over-approximates, so every lane may take both).
    BothWays,
    /// An undecided boolean atom, forked where it reaches a select.
    Atom,
    /// The unknown number (an absent slot stays absent).
    Unknown,
    /// The stored literal itself (the `Range`), so lanes merge across it;
    /// owed per lane on the raw input: the lane's own value lies inside.
    Literal,
    /// The entry's hook reads it.
    Hook,
}

/// An entry's soundness class.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
    /// State-preserving for everything the search observes (`why` is the
    /// proof sketch).
    Exact,
    /// An over-approximation (`why` says what refutes it).
    Over,
}

/// What a table entry cannot say, implemented by name on both sides.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Hook {
    /// The near floors (`n`): `state`/`collideable` stored as their slots
    /// say EXCEPT where a player certainly overlaps the floor at the frame's
    /// end (a per-lane condition on ANOTHER object's fields); there the
    /// cart's invariant (hidden: `state` 2, `collideable` false) is stored,
    /// owed (with `p`: the computed value); mid split frame the computed
    /// `collideable` is kept in the player's probe window; and the input
    /// derives `collideable` from `state` (`state ~= 2`). Both sides:
    /// `Rt2::widen_near_floors`, `trace::widen::widen_near_floors`.
    NearFloor,
    /// The moving platforms' INPUT (`p`): which world platform each is
    /// (matched by its constant `y` and `dir`), `x` restricted to the path
    /// and recorded as the world's cell, `spd.x` bound by the worlds, `rem.x`
    /// the owed literal: `trace::widen::platform_inputs`. Their stored
    /// values are plain entries.
    PlatformInputs,
}

/// One slot of an entry: a field path below the target, what it stores,
/// how the frame reads it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Slot {
    pub field: &'static [&'static str],
    pub stored: Stored,
    pub input: Input,
    /// The field may be absent (written by a later update only; see the
    /// `AbsentAsZero` entries); else an absent field is an error.
    pub optional: bool,
}

/// One widening: its slots, on which objects, at which levels, why sound.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Entry {
    pub name: &'static str,
    pub flag: Flag,
    pub target: Target,
    pub slots: &'static [Slot],
    pub kind: Kind,
    pub hook: Option<Hook>,
    /// For an exact entry the proof sketch; for an over-approximation what
    /// it loses and what refutes it.
    pub why: &'static str,
}

const fn slot(field: &'static [&'static str], stored: Stored, input: Input) -> Slot {
    Slot { field, stored, input, optional: false }
}

/// The whole remainder `[-0.5, 0.5)`, raw.
pub const REM: (i32, i32) = (-0x8000, 0x7fff);

/// A full `sin` period as an inclusive raw interval width (`rnd(1)`'s
/// range); the balloon's canonical phase is `[0, BALLOON_PERIOD_RAW]`.
pub const BALLOON_PERIOD_RAW: i32 = 0xffff;

/// A fall floor's widened `state` (0 idle, 1 shaking, 2 hidden), an
/// interval so a lane may hold it or an exact state. Raw.
pub const FLOOR_STATE_RANGE: (i32, i32) = (0, 2 << 16);

/// A spring's widened `spr` (0 hidden, 18 ready, 19 compressed).
pub const SPRING_SPR_RANGE: (i32, i32) = (0, 19 << 16);

/// A balloon's widened `spr` (0 popped, 22 present).
pub const BALLOON_SPR_RANGE: (i32, i32) = (0, 22 << 16);

/// A balloon's bob radius (`y = start + sin(offset) * 2`).
pub const BALLOON_BOB_RAW: i32 = 2 << 16;

/// The strawberry's bob radius (`y = start + sin(off / 40) * 2.5`).
pub const FRUIT_BOB_RAW: i32 = 0x2_8000;

/// The strawberry's bob phase `off`, one period (`sin(off / 40)`).
pub const FRUIT_OFF: (i32, i32) = (0, 39 << 16);

/// Hitboxes `(x, y, w, h)`, checked by the tracer (`widen::widen_near_floors`).
pub const PLAYER_HITBOX: [i16; 4] = [1, 3, 6, 5];
pub const FLOOR_HITBOX: [i16; 4] = [0, 0, 8, 8];

/// The cart's `floor.collide(player, 0, 0)` solved for the player: OPEN
/// windows `(lo, hi)` for `x` and `y`.
pub fn floor_player_window(floor: (P8, P8)) -> [(P8, P8); 2] {
    let [px, py, pw, ph] = PLAYER_HITBOX;
    let [fx, fy, fw, fh] = FLOOR_HITBOX;
    let at = |base: P8, d: i16| base + P8::from_i16(d);
    [(at(floor.0, fx - px - pw), at(floor.0, fx + fw - px)), (at(floor.1, fy - py - ph), at(floor.1, fy + fh - py))]
}

/// Does a player in ranges `x`, `y` CERTAINLY overlap? A straddling range
/// does not (the floor stays widened, the safe side). Shared with the tracer.
pub fn player_overlaps_floor(window: [(P8, P8); 2], x: (P8, P8), y: (P8, P8)) -> bool {
    let [(xlo, xhi), (ylo, yhi)] = window;
    x.0 > xlo && x.1 < xhi && y.0 > ylo && y.1 < yhi
}

/// A moving platform's `x` (and `last`) range in pixels: it wraps between
/// -16 and 128.
pub const PLATFORM_PATH: (i16, i16) = (-16, 128);

/// The fly fruit's widened `spd.y` (raw, inclusive).
pub const FLY_FRUIT_SPD_Y: (i32, i32) = (-0x3_8000, 0x8000);

const PATH_RAW: (i32, i32) = ((PLATFORM_PATH.0 as i32) << 16, (PLATFORM_PATH.1 as i32) << 16);

/// THE TABLE, in the order both sides apply it (the tracer's node order
/// follows it). plans/abstractions.md lists exactly these entries
/// (`render_markdown`, checked by a test).
pub static TABLE: &[Entry] = &[
    Entry {
        name: "held buttons",
        flag: Flag::Held,
        target: Target::Objects("player"),
        slots: &[slot(&["p_jump"], Stored::UnknownBool, Input::BothWays), slot(&["p_dash"], Stored::UnknownBool, Input::BothWays)],
        kind: Kind::Over,
        hook: None,
        why: "The previous frame's buttons (read only to detect a press) unknown, each read both ways: \
              a held button may re-trigger (a ground jump then a wall jump on consecutive frames). \
              Refuted by the CONCRETE SEARCH.",
    },
    Entry {
        name: "player remainder",
        flag: Flag::Always,
        target: Target::Objects("player"),
        slots: &[
            slot(&["rem", "x"], Stored::Range { lo: REM.0, hi: REM.1, proof: Proof::PerLane }, Input::Stored),
            slot(&["rem", "y"], Stored::Range { lo: REM.0, hi: REM.1, proof: Proof::PerLane }, Input::Stored),
        ],
        kind: Kind::Over,
        hook: None,
        why: "The sub-pixel remainder, to the whole [-0.5, 0.5) at the boundary (the region's \
              `Restrict` bounds the input too). Refuted by the ARCS: every recorded edge carries the \
              frame's exact transfer of the remainder, and the backward over them is exact in it.",
    },
    Entry {
        name: "dash effect",
        flag: Flag::Always,
        target: Target::Objects("player"),
        slots: &[slot(&["dash_effect_time"], Stored::AtLeast(0), Input::Stored)],
        kind: Kind::Exact,
        hook: None,
        why: "`dash_effect_time` decrements every frame and is read only as `dash_effect_time > 0`, \
              so every value <= 0 behaves alike and `max(v, 0)` keeps every read.",
    },
    Entry {
        name: "strawberry bob",
        flag: Flag::Always,
        target: Target::Objects("fruit"),
        slots: &[
            slot(&["y"], Stored::Band { around: "start", radius: FRUIT_BOB_RAW, proof: Proof::PerLane }, Input::Stored),
            slot(&["off"], Stored::Phase { lo: FRUIT_OFF.0, hi: FRUIT_OFF.1 }, Input::Stored),
        ],
        kind: Kind::Over,
        hook: None,
        why: "A live strawberry's bob forgotten, `off` and `y` TOGETHER (one without the other is a \
              row no level has). `off` is a nonnegative integer read only as `sin(off / 40)`, \
              periodic in it with period 40 bit-exactly (`pico8_num::test_pico8_sin_period_40_bit_exact`), \
              so one period [0, 39] covers every phase; `y` is the band `start +- 2.5` the bob stays \
              in, owed. The fruit's collision reads `y`, so a collect becomes possible at any phase: \
              refuted by the CONCRETE SEARCH.",
    },
    Entry {
        name: "timers",
        flag: Flag::Always,
        target: Target::Globals,
        slots: &[
            slot(&["frames"], Stored::Num(0), Input::Stored),
            slot(&["seconds"], Stored::Num(0), Input::Stored),
            slot(&["minutes"], Stored::Num(0), Input::Stored),
            slot(&["deaths"], Stored::Num(0), Input::Stored),
        ],
        kind: Kind::Exact,
        hook: None,
        why: "DEAD fields: the cart reads `frames`, `seconds`, `minutes` and `deaths` only to update \
              each other and the key's sprite (below), never in anything a state's successors or the \
              search's tests depend on, so every value of them behaves alike.",
    },
    Entry {
        name: "key sprite",
        flag: Flag::Always,
        target: Target::Objects("key"),
        slots: &[slot(&["spr"], Stored::Num(8 << 16), Input::Stored), slot(&["flip", "x"], Stored::Bool(false), Input::Stored)],
        kind: Kind::Exact,
        hook: None,
        why: "DEAD fields: the key's `spr` (from `frames`) and `flip.x` (from `spr`) are read only by \
              each other and the draw, so every value behaves alike.",
    },
    Entry {
        name: "fly fruit",
        flag: Flag::Fruit,
        target: Target::Objects("fly_fruit"),
        slots: &[
            slot(&["step"], Stored::UnknownNum, Input::Unknown),
            slot(&["y"], Stored::UnknownNum, Input::Unknown),
            slot(&["spd", "y"], Stored::Range { lo: FLY_FRUIT_SPD_Y.0, hi: FLY_FRUIT_SPD_Y.1, proof: Proof::Literals }, Input::Literal),
            slot(&["rem", "y"], Stored::Range { lo: REM.0, hi: REM.1, proof: Proof::Literals }, Input::Literal),
            slot(&["fly"], Stored::UnknownBool, Input::Atom),
        ],
        kind: Kind::Over,
        hook: None,
        why: "While waiting the fly fruit's `step += 0.05` and bob mean no fruit state recurs across \
              frames; once flying, rows split by the frame of the dash. `step`/`y` the unknown number, \
              `spd.y`/`rem.y` their ranges (read as the literals, owed per lane on the raw input), \
              `fly` an unknown boolean. Refuted by the objects ladder and the CONCRETE SEARCH.",
    },
    Entry {
        name: "fall floor countdown",
        flag: Flag::Near,
        target: Target::Objects("fall_floor"),
        slots: &[Slot { field: &["delay"], stored: Stored::UnknownNum, input: Input::Unknown, optional: true }],
        kind: Kind::Over,
        hook: None,
        why: "The countdown, the unknown number (never the interval [MIN, MAX]: `delay - 1` of it \
              would overflow, `Op::NoWrap`). The cart only decrements it and compares it with 0. \
              Refuted by the CONCRETE SEARCH (or the ladder's exact level).",
    },
    Entry {
        name: "balloon countdown",
        flag: Flag::Near,
        target: Target::Objects("balloon"),
        slots: &[slot(&["timer"], Stored::UnknownNum, Input::Unknown)],
        kind: Kind::Over,
        hook: None,
        why: "As the fall floor's countdown.",
    },
    Entry {
        name: "spring phase",
        flag: Flag::Near,
        target: Target::Objects("spring"),
        slots: &[
            slot(&["spr"], Stored::Range { lo: SPRING_SPR_RANGE.0, hi: SPRING_SPR_RANGE.1, proof: Proof::Static }, Input::Stored),
            Slot { field: &["delay"], stored: Stored::UnknownNum, input: Input::Unknown, optional: true },
            slot(&["hide_in"], Stored::UnknownNum, Input::Unknown),
            slot(&["hide_for"], Stored::UnknownNum, Input::Unknown),
        ],
        kind: Kind::Over,
        hook: None,
        why: "The spring's sprite (0 hidden, 18 ready, 19 compressed) its range, so `spr == 18` splits \
              like a floor's state; its countdowns the unknown number. Its update becomes \"maybe \
              bounce\". Refuted by the CONCRETE SEARCH.",
    },
    Entry {
        name: "balloon phase",
        flag: Flag::Near,
        target: Target::Objects("balloon"),
        slots: &[
            slot(&["spr"], Stored::Range { lo: BALLOON_SPR_RANGE.0, hi: BALLOON_SPR_RANGE.1, proof: Proof::Static }, Input::Stored),
            slot(&["y"], Stored::Band { around: "start", radius: BALLOON_BOB_RAW, proof: Proof::Static }, Input::Stored),
        ],
        kind: Kind::Over,
        hook: None,
        why: "The balloon's sprite (0 popped, 22 present) its range and its `y` the bob band \
              `start +- 2` (the bob runs only when `spr == 22`, and an exact `y` would give every \
              state a twin). Its update becomes \"maybe refill the dash\". Refuted by the CONCRETE \
              SEARCH.",
    },
    Entry {
        name: "near fall floor",
        flag: Flag::Near,
        target: Target::Objects("fall_floor"),
        slots: &[
            slot(&["state"], Stored::Range { lo: FLOOR_STATE_RANGE.0, hi: FLOOR_STATE_RANGE.1, proof: Proof::Static }, Input::Hook),
            slot(&["collideable"], Stored::UnknownBool, Input::Hook),
        ],
        kind: Kind::Over,
        hook: Some(Hook::NearFloor),
        why: "Each fall floor's `state` [0, 2] and `collideable` unknown, EXCEPT where a player \
              certainly overlaps the floor at the frame's end (there the cart's invariant, hidden, \
              owed; with `p` the computed value). The input derives `collideable` as `state ~= 2` \
              (the cart keeps them in step). Refuted by the CONCRETE SEARCH or the ladder \
              `r0sxhn,r0sxh`.",
    },
    Entry {
        name: "platform path",
        flag: Flag::Platforms,
        target: Target::Objects("platform"),
        slots: &[
            slot(&["x"], Stored::Range { lo: PATH_RAW.0, hi: PATH_RAW.1, proof: Proof::Static }, Input::Hook),
            slot(&["last"], Stored::SameAs("x"), Input::Hook),
        ],
        kind: Kind::Over,
        hook: Some(Hook::PlatformInputs),
        why: "Every moving platform's `x` the interval of its whole path [-16, 128] (the wrap keeps \
              it inside, proved on the traced select or owed), `last` the same node (`last == x` at \
              every frame boundary, checked). The worlds (`concrete::platform_worlds`) keep the \
              platforms mutually consistent (`verify::Points`). Refuted by the CONCRETE SEARCH (the \
              reference engine refuses `p`).",
    },
    Entry {
        name: "platform remainder",
        flag: Flag::Platforms,
        target: Target::Objects("platform"),
        slots: &[slot(&["rem", "x"], Stored::Range { lo: REM.0, hi: REM.1, proof: Proof::Static }, Input::Hook)],
        kind: Kind::Over,
        hook: Some(Hook::PlatformInputs),
        why: "A platform's `rem.x` the whole remainder, read as the literal (owed per lane): a \
              pixel of slack at this level. Refuted by the CONCRETE SEARCH.",
    },
    Entry {
        name: "balloon offset",
        flag: Flag::Always,
        target: Target::Objects("balloon"),
        slots: &[Slot { field: &["offset"], stored: Stored::FullPeriod(BALLOON_PERIOD_RAW), input: Input::Stored, optional: true }],
        kind: Kind::Exact,
        hook: None,
        why: "The balloon's `rnd` phase is read only through `sin(offset)`, and `sin` of a full period \
              is [-1, 1] whatever the interval's ends, so a full-period interval is stored as the \
              canonical [0, 1) (a point, a seeded phase, is left). Room (5,0) reaches the same \
              balloon-free states with it.",
    },
    Entry {
        name: "absent fall floor delay",
        flag: Flag::Always,
        target: Target::Objects("fall_floor"),
        slots: &[Slot { field: &["delay"], stored: Stored::AbsentAsZero, input: Input::Stored, optional: true }],
        kind: Kind::Exact,
        hook: None,
        why: "A fall floor gains `delay` only when it first breaks, which made \"which floors ever \
              broke\" heap shape. A missing `delay` is written as 0: the cart can tell nil from 0 \
              only by raising (`delay - 1`, `delay <= 0` halt PICO-8 on nil), and \
              `cart::check_absent_fields` refuses a cart that reads it any other way. Exact on every \
              execution that does not raise.",
    },
    Entry {
        name: "absent spring delay",
        flag: Flag::Always,
        target: Target::Objects("spring"),
        slots: &[Slot { field: &["delay"], stored: Stored::AbsentAsZero, input: Input::Stored, optional: true }],
        kind: Kind::Exact,
        hook: None,
        why: "As the fall floor's: a spring gains `delay` when it first breaks.",
    },
];

/// The `AbsentAsZero` slots: `(type global, field)`.
pub fn absent_as_zero() -> impl Iterator<Item = (&'static str, &'static str)> {
    TABLE.iter().filter_map(|e| match (e.target, e.slots) {
        (Target::Objects(ty), [Slot { field: [f], stored: Stored::AbsentAsZero, .. }]) => Some((ty, *f)),
        _ => None,
    })
}

// ------------------------------------------------------------ on blocks

/// An entry's names resolved to the engine's ids (`celeste_names`).
struct Resolved {
    /// The type global (`Target::Objects`).
    ty: Option<u32>,
    /// Per slot, its field path as ids (a global's id first for `Globals`).
    fields: Vec<Vec<u32>>,
    /// Per slot, a `Band`'s `around` field id.
    around: Vec<Option<u32>>,
}

fn resolved() -> &'static [Resolved] {
    static R: std::sync::OnceLock<Vec<Resolved>> = std::sync::OnceLock::new();
    R.get_or_init(|| {
        let g = |n: &str| celeste_names::global_id(n).unwrap_or_else(|| panic!("widening table: no global {n}"));
        let f = |n: &str| celeste_names::field_id(n).unwrap_or_else(|| panic!("widening table: no field {n}"));
        TABLE
            .iter()
            .map(|e| Resolved {
                ty: match e.target {
                    Target::Objects(t) => Some(g(t)),
                    Target::Globals => None,
                },
                fields: e
                    .slots
                    .iter()
                    .map(|s| {
                        s.field
                            .iter()
                            .enumerate()
                            .map(|(i, n)| if i == 0 && e.target == Target::Globals { g(n) } else { f(n) })
                            .collect()
                    })
                    .collect(),
                around: e
                    .slots
                    .iter()
                    .map(|s| match s.stored {
                        Stored::Band { around, .. } => Some(f(around)),
                        _ => None,
                    })
                    .collect(),
            })
            .collect()
    })
}

/// `(lo, hi)` of a numeric lane value, else `None`.
fn span(v: AV) -> Option<(P8, P8)> {
    match v {
        AV::Num(n) => Some((n, n)),
        AV::Ival(a, b) => Some((a, b)),
        _ => None,
    }
}

impl Rt2 {
    /// A level's widenings (`TABLE`), as its kernels bake them
    /// (`trace::widen`), so a finer row can be looked up as the level's
    /// node. Each asserts what its entry owes.
    pub fn widen_to(&mut self, ids: &BoundaryIds, level: Level) {
        for (e, r) in TABLE.iter().zip(resolved()) {
            if !level.has(e.flag) {
                continue;
            }
            if e.hook == Some(Hook::NearFloor) {
                self.widen_near_floors(ids, r);
                continue;
            }
            let bases: Vec<Option<u32>> = match r.ty {
                Some(ty) => self.objects_of_type(ids, ty).into_iter().map(Some).collect(),
                None => vec![None],
            };
            for base in bases {
                // A `SameAs` slot equals its twin BEFORE either is widened.
                for (k, s) in e.slots.iter().enumerate() {
                    if let Stored::SameAs(other) = s.stored {
                        let j = e.slots.iter().position(|t| t.field == [other]).expect("SameAs names a slot of its entry");
                        let (a, b) = (self.slot_cell(base, &r.fields[k]), self.slot_cell(base, &r.fields[j]));
                        let (Some(a), Some(b)) = (a, b) else { panic!("widening {:?}: no `{}` or `{other}`", e.name, s.field.join(".")) };
                        assert!(
                            (0..self.width).all(|lane| self.cols[a as usize].at(lane) == self.cols[b as usize].at(lane)),
                            "widening {:?}: a row where `{}` is not `{other}`",
                            e.name,
                            s.field.join(".")
                        );
                    }
                }
                for (k, s) in e.slots.iter().enumerate() {
                    let cell = self.slot_cell(base, &r.fields[k]);
                    let Some(c) = cell else {
                        assert!(s.optional, "widening {:?}: no `{}`", e.name, s.field.join("."));
                        continue;
                    };
                    let around = r.around[k].map(|f| {
                        let obj = base.expect("a band is around an object's field");
                        self.obj_field_cell(obj, f).unwrap_or_else(|| panic!("widening {:?}: no band centre", e.name))
                    });
                    let same = match s.stored {
                        Stored::SameAs(other) => {
                            let j = e.slots.iter().position(|t| t.field == [other]).expect("SameAs names a slot of its entry");
                            Some(self.slot_cell(base, &r.fields[j]).expect("SameAs's slot"))
                        }
                        _ => None,
                    };
                    self.widen_cell(e.name, s, c, around, same);
                }
            }
        }
    }

    /// The cell of a slot: `field` below the object `base`, or from the
    /// globals (`base` `None`).
    fn slot_cell(&self, base: Option<u32>, field: &[u32]) -> Option<u32> {
        let (mut cell, rest) = match base {
            Some(obj) => (self.obj_field_cell(obj, field[0])?, &field[1..]),
            None => {
                let c = self.globals[field[0] as usize];
                (c, &field[1..])
            }
        };
        if cell == NONE {
            return None;
        }
        for f in rest {
            let Col::U(AV::Ptr(sub)) = self.cols[cell as usize] else { return None };
            cell = self.obj_field_cell(sub, *f)?;
        }
        Some(cell)
    }

    /// Every lane of cell `c` satisfies `ok`, or panic naming the entry.
    fn check_lanes(&self, name: &str, c: u32, what: &str, ok: impl Fn(AV) -> bool) {
        for lane in 0..self.width {
            let v = self.cols[c as usize].at(lane);
            assert!(ok(v), "widening {name:?}: lane {lane} holds {v:?}, {what}");
        }
    }

    /// One slot's stored value written into cell `c` (`around`: a band's
    /// centre cell; `same`: a `SameAs`'s cell, before it was widened).
    fn widen_cell(&mut self, name: &str, s: &Slot, c: u32, around: Option<u32>, same: Option<u32>) {
        let inside = |lo: P8, hi: P8| move |v: AV| span(v).is_some_and(|(a, b)| lo <= a && b <= hi);
        let numeric = |v: AV| span(v).is_some();
        let new = match s.stored {
            Stored::Range { lo, hi, .. } => {
                let (lo, hi) = (P8::from_raw(lo), P8::from_raw(hi));
                self.check_lanes(name, c, "outside the widening", inside(lo, hi));
                Col::U(AV::Ival(lo, hi))
            }
            Stored::Band { radius, .. } => {
                let centre = around.expect("a band's centre");
                let r = P8::from_raw(radius);
                let band: Vec<(P8, P8)> = (0..self.width)
                    .map(|lane| match self.cols[centre as usize].at(lane) {
                        AV::Num(s) => (s - r, s + r),
                        other => panic!("widening {name:?}: the band's centre is {other:?}"),
                    })
                    .collect();
                for (lane, &(lo, hi)) in band.iter().enumerate() {
                    let v = self.cols[c as usize].at(lane);
                    assert!(inside(lo, hi)(v), "widening {name:?}: lane {lane} holds {v:?}, outside the band [{lo:?}, {hi:?}]");
                }
                collapse_uniform(Col::I(band))
            }
            Stored::Phase { lo, hi } => {
                self.check_lanes(name, c, "not a number", numeric);
                Col::U(AV::Ival(P8::from_raw(lo), P8::from_raw(hi)))
            }
            Stored::AtLeast(k) => {
                let k = P8::from_raw(k);
                let at_least = |v: AV| match v {
                    AV::Num(n) => AV::Num(n.max(k)),
                    AV::Ival(a, b) => AV::Ival(a.max(k), b.max(k)),
                    other => panic!("widening {name:?}: {other:?} is not a number"),
                };
                match &self.cols[c as usize] {
                    Col::U(v) => Col::U(at_least(*v)),
                    Col::N(vs) => Col::N(vs.iter().map(|n| (*n).max(k)).collect()),
                    col => collapse_uniform(crate::runtime2::compress_num_v((0..self.width).map(|l| at_least(col.at(l))).collect())),
                }
            }
            Stored::UnknownNum => {
                self.check_lanes(name, c, "not a number", |v| numeric(v) || v == AV::UNum);
                Col::U(AV::UNum)
            }
            Stored::UnknownBool => {
                self.check_lanes(name, c, "not a boolean", |v| matches!(v, AV::Bool(_) | AV::UBool));
                Col::U(AV::UBool)
            }
            Stored::Num(k) => {
                self.check_lanes(name, c, "not a number", numeric);
                Col::U(AV::Num(P8::from_raw(k)))
            }
            Stored::Bool(b) => {
                self.check_lanes(name, c, "not a boolean", |v| matches!(v, AV::Bool(_) | AV::UBool));
                Col::U(AV::Bool(b))
            }
            Stored::FullPeriod(p) => {
                if (0..self.width).all(|lane| matches!(self.cols[c as usize].at(lane), AV::Num(_))) {
                    return;
                }
                let full = |v: AV| matches!(v, AV::Ival(lo, hi) if hi.as_raw_u32() as i32 - lo.as_raw_u32() as i32 >= p);
                self.check_lanes(name, c, "not a full period", full);
                Col::U(AV::Ival(P8::from_raw(0), P8::from_raw(p)))
            }
            // Checked equal before the entry (`widen_to`); the twin's slot
            // comes first in its entry, so it is widened already.
            Stored::SameAs(_) => self.cols[same.expect("SameAs's cell") as usize].clone(),
            Stored::AbsentAsZero => return,
        };
        self.cols[c as usize] = new;
    }

    /// `Hook::NearFloor` on blocks (`trace::widen::widen_near_floors`):
    /// `state` and `collideable` widened except where a player certainly
    /// overlaps the floor (kept as they are: the kernels store the cart's
    /// invariant there, or with `p` the computed value - either is what a
    /// concrete row holds). `state` is an interval column, as the kernels
    /// store it.
    fn widen_near_floors(&mut self, ids: &BoundaryIds, r: &Resolved) {
        let (slo, shi) = (P8::from_raw(FLOOR_STATE_RANGE.0), P8::from_raw(FLOOR_STATE_RANGE.1));
        let span_of = |v: AV, what: &str| span(v).unwrap_or_else(|| panic!("near floor widening: {what} is {v:?}"));
        let players: Vec<(u32, u32)> = self
            .player_objects(ids)
            .into_iter()
            .map(|o| {
                let cell = |f: u32, what: &str| self.obj_field_cell(o, f).unwrap_or_else(|| panic!("near floor widening: the player has no `{what}`"));
                (cell(ids.f_x, "x"), cell(ids.f_y, "y"))
            })
            .collect();
        let ty = r.ty.expect("the near floors are objects");
        for obj in self.objects_of_type(ids, ty) {
            // Floors never move: one position in every lane.
            let at = |f: u32| {
                let col = self.obj_field_cell(obj, f).map(|c| &self.cols[c as usize]);
                col.and_then(|c| c.uniform_num(self.width)).unwrap_or_else(|| panic!("near floor widening: a fall floor's position is {col:?}, not one number"))
            };
            let window = floor_player_window((at(ids.f_x), at(ids.f_y)));
            let overlap: Vec<bool> = (0..self.width)
                .map(|lane| {
                    players.iter().any(|&(cx, cy)| {
                        player_overlaps_floor(window, span_of(self.cols[cx as usize].at(lane), "the player's x"), span_of(self.cols[cy as usize].at(lane), "the player's y"))
                    })
                })
                .collect();
            let field = |k: usize| self.slot_cell(Some(obj), &r.fields[k]).unwrap_or_else(|| panic!("near floor widening: the fall floor has no `{:?}`", r.fields[k]));
            let (cs, cc) = (field(0), field(1));
            let state: Vec<(P8, P8)> = (0..self.width)
                .map(|lane| {
                    let (a, b) = span_of(self.cols[cs as usize].at(lane), "a fall floor's `state`");
                    if overlap[lane] {
                        (a, b)
                    } else {
                        assert!(slo <= a && b <= shi, "near floor widening: lane {lane} of `state` is [{a:?}, {b:?}], which the widening does not contain");
                        (slo, shi)
                    }
                })
                .collect();
            let coll: Vec<AV> = (0..self.width)
                .map(|lane| {
                    let v = self.cols[cc as usize].at(lane);
                    assert!(matches!(v, AV::Bool(_) | AV::UBool), "near floor widening: lane {lane} of `collideable` is {v:?}");
                    if overlap[lane] {
                        v
                    } else {
                        AV::UBool
                    }
                })
                .collect();
            self.cols[cs as usize] = collapse_uniform(Col::I(state));
            self.cols[cc as usize] = collapse_uniform(Col::V(coll));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels_parse_and_print() {
        assert_eq!(Level::parse("r0sx").unwrap(), Level::EXACT);
        for spec in ["r0sx", "r0sxh", "r0sxhf", "r0sxhn", "r0sxhnp", "r0sxn", "r0sxf"] {
            assert_eq!(Level::parse(spec).unwrap().to_string(), spec);
        }
        let hn = Level::parse("r0sxhn").unwrap();
        assert!(hn.held && hn.floors_near && !hn.fruit && !hn.platforms);
        // Only the `r0sx` prefix parses.
        assert!(Level::parse("r1sx").is_err());
        assert!(Level::parse("rxsx").is_err());
        assert!(Level::parse("r0s16").is_err());
        assert!(Level::parse("y2r0sx").is_err());
        assert!(Level::parse("r0sxhb").is_err(), "floors unknown `b` is gone");
        assert!(Level::parse("r0sxph").is_err(), "flags in order");
    }

    /// `floor_player_window` is the cart's `floor.collide(player, 0, 0)` at
    /// every whole-pixel position around a floor.
    #[test]
    fn the_overlap_window_is_the_carts_collide() {
        let (fx, fy) = (48i16, 112i16);
        let window = floor_player_window((P8::from_i16(fx), P8::from_i16(fy)));
        let [px_, py_, pw, ph] = PLAYER_HITBOX;
        let [fx_, fy_, fw, fh] = FLOOR_HITBOX;
        for x in fx - 20..fx + 20 {
            for y in fy - 20..fy + 20 {
                let collide = x + px_ + pw > fx + fx_ && y + py_ + ph > fy + fy_ && x + px_ < fx + fx_ + fw && y + py_ < fy + fy_ + fh;
                let (px, py) = (P8::from_i16(x), P8::from_i16(y));
                assert_eq!(player_overlaps_floor(window, (px, px), (py, py)), collide, "player at ({x}, {y})");
            }
        }
        // A bucket straddling the window's edge is no overlap.
        let (a, b) = (P8::from_i16(fx - 7), P8::from_i16(fx - 6));
        assert!(!player_overlaps_floor(window, (a, b), (P8::from_i16(fy), P8::from_i16(fy))));
    }

    /// Every name in the table resolves, and a `SameAs` names an earlier
    /// slot of its entry (the block side reads its widened value).
    #[test]
    fn the_table_resolves() {
        assert_eq!(resolved().len(), TABLE.len());
        for e in TABLE {
            for (k, s) in e.slots.iter().enumerate() {
                if let Stored::SameAs(other) = s.stored {
                    let j = e.slots.iter().position(|t| t.field == [other]).expect("SameAs names a slot");
                    assert!(j < k, "{}: SameAs({other}) must follow it", e.name);
                }
            }
        }
    }
}
