//! THE WIDENING TABLE: every field a level stores as something other than
//! its exact value, what it stores, what that owes, and why it is sound.
//!
//! One declarative table (`TABLE`), its output side interpreted ONCE
//! (`widen_level`) against two targets (`Widener`): the tracer turns each
//! entry into graph ops and owed errors at the frame's end (`trace::widen`),
//! and the block side evaluates the same entry over columns
//! (`Rt2::widen_to`: the concrete search's node lookup, the objects ladder's
//! projection, the boundary). Neither side has a widening the table does not
//! name. A LEVEL is the set of entries whose `Flag` it has.
//!
//! Two kinds (`Kind`): an EXACT canonicalization loses nothing the search
//! observes (each says why in `why`); an OVER-APPROXIMATION is refuted by
//! something exact (the arcs for the remainder, the concrete search for the
//! rest; CLAUDE.md "Never widen a field without something EXACT that refutes
//! it").
//!
//! What a table entry cannot say is a HOOK (`Hook`), named in its entry and
//! documented where it is implemented.

use celeste_core::arith::Arithmetic;

use crate::runtime2::{collapse_uniform, names, Col, Rt2, AV, NONE};
use crate::shape::Cell;

/// A LEVEL: which objects the forward widens (plans/abstractions.md), on
/// top of what every level widens (`Flag::Always`: the remainder, and the
/// buttons' memory). Each widening over-approximates; the concrete search
/// refutes it. `Default` is `Level::EXACT`.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct Level {
    /// `f`: the fly fruit's `step` and `y` unknown numbers, `spd.y` / `rem.y`
    /// their whole ranges, `fly` unknown.
    pub fruit: bool,
    /// `n`: the countdowns unknown, each fall floor's `state` [0, 2] and
    /// `collideable` unknown except where the player overlaps it at the
    /// frame's end, the objects' phases their ranges.
    pub floors_near: bool,
}

impl Level {
    /// Every object exact (`r0sx`).
    pub const EXACT: Level = Level { fruit: false, floors_near: false };

    /// A level from `r0sx[f][n]`: the fixed prefix, then the flags in
    /// that order (each absent = exact).
    pub fn parse(spec: &str) -> Result<Level, String> {
        let s = spec.trim();
        let mut rest = s
            .strip_prefix("r0sx")
            .ok_or_else(|| format!("level {spec:?}: expected r0sx[f][n] (the remainder is the arcs', the speed exact)"))?;
        if rest.contains('h') {
            return Err(format!("level {spec:?}: `h` was removed: held buttons are widened at every level (plans/correctness.md C7)"));
        }
        let mut flag = |c: char| -> bool {
            match rest.strip_prefix(c) {
                Some(t) => {
                    rest = t;
                    true
                }
                None => false,
            }
        };
        let level = Level { fruit: flag('f'), floors_near: flag('n') };
        if rest.contains('p') {
            return Err(format!("level {spec:?}: `p` (widened platforms) was removed: platforms are exact (plans/correctness.md C13)"));
        }
        if !rest.is_empty() {
            return Err(format!("level {spec:?}: unexpected {rest:?} (flags are f, n in that order)"));
        }
        Ok(level)
    }

    pub fn has(&self, flag: Flag) -> bool {
        match flag {
            Flag::Always => true,
            Flag::Fruit => self.fruit,
            Flag::Near => self.floors_near,
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
        for (on, c) in [(self.fruit, 'f'), (self.floors_near, 'n')] {
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
    /// `f`.
    Fruit,
    /// `n`.
    Near,
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

/// What a slot STORES at the frame's end.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Stored {
    /// The interval `[lo, hi]` (raw 16.16); owed: the value lies inside
    /// (proved from the graph where it can be, else checked per lane; on
    /// blocks an assertion).
    Range { lo: i32, hi: i32 },
    /// `[s - radius, s + radius]` around the object's number field
    /// `around` (`s`, a constant of the shape); owed as `Range`.
    Band { around: &'static str, radius: i32 },
    /// A PHASE: an integer read only through a function periodic in it, as
    /// the interval `[lo, hi]` of one period. Nothing owed: the entry's
    /// `why` names the periodicity.
    Phase { lo: i32, hi: i32 },
    /// `max(v, lo)`: every value at most `lo` reads alike.
    AtLeast(i32),
    /// The unknown number (`AV::UNum`), which contains every value; a
    /// `countdown`'s (a number the cart only counts down and compares with
    /// 0) reads its comparisons as countdown atoms (`Op::UnknownNum`).
    UnknownNum { countdown: bool },
    /// The unknown boolean (`AV::UBool`).
    UnknownBool,
    /// A constant: the slot is DEAD (nothing observed reads it).
    Num(i32),
    /// A constant boolean: the slot is dead.
    Bool(bool),
    /// A full-period interval (width at least `period`, raw) stored as the
    /// canonical `[0, period]`; owed: the width. A point is left as it is.
    FullPeriod(i32),
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
    /// The near floors (`n`): `state`/`collideable` stored as their slots say
    /// EXCEPT where a player certainly overlaps the floor at the frame's end (a
    /// per-lane condition on ANOTHER object's fields); there the cart's
    /// invariant (hidden: `state` 2, `collideable` false) is stored, owed; mid
    /// split frame the computed `collideable` is kept in the player's probe
    /// window; the input derives `collideable` from `state`. Both sides:
    /// `Rt2::widen_near_floors`, `trace::widen::widen_near_floors`.
    NearFloor,
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
/// windows `(lo, hi)` for `x` and `y`; `None` where one leaves the 16.16
/// range (never: a floor's position is its map tile's).
pub fn floor_player_window<N: Arithmetic>(floor: (N, N)) -> Option<[(N, N); 2]> {
    let [px, py, pw, ph] = PLAYER_HITBOX;
    let [fx, fy, fw, fh] = FLOOR_HITBOX;
    let at = |base: N, d: i16| N::add(base, N::from_i16(d));
    Some([(at(floor.0, fx - px - pw)?, at(floor.0, fx + fw - px)?), (at(floor.1, fy - py - ph)?, at(floor.1, fy + fh - py)?)])
}

/// Does a player in ranges `x`, `y` CERTAINLY overlap? A straddling range
/// does not (the floor stays widened, the safe side). Shared with the tracer.
pub fn player_overlaps_floor<N: Arithmetic>(window: [(N, N); 2], x: (N, N), y: (N, N)) -> bool {
    let [(xlo, xhi), (ylo, yhi)] = window;
    let lt = N::lt;
    lt(xlo, x.0) && lt(x.1, xhi) && lt(ylo, y.0) && lt(y.1, yhi)
}

/// The fly fruit's widened `spd.y` (raw, inclusive).
pub const FLY_FRUIT_SPD_Y: (i32, i32) = (-0x3_8000, 0x8000);

/// THE TABLE, in the order both sides apply it (the tracer's node order
/// follows it). plans/abstractions.md lists exactly these entries
/// (`render_markdown`, checked by a test).
pub static TABLE: &[Entry] = &[
    Entry {
        name: "held buttons",
        flag: Flag::Always,
        target: Target::Objects("player"),
        slots: &[
            slot(&["p_jump"], Stored::UnknownBool, Input::BothWays),
            slot(&["p_dash"], Stored::UnknownBool, Input::BothWays),
            slot(&["jbuffer"], Stored::UnknownNum { countdown: true }, Input::Unknown),
        ],
        kind: Kind::Over,
        hook: None,
        why: "THE BUTTONS' MEMORY, at every level (plans/correctness.md C7): the previous frame's \
              buttons (read only to detect a press) unknown, each read both ways, and the jump \
              buffer (`jbuffer`, a press kept for 4 updates) the unknown countdown: a jump may fire \
              on any update its grace or a wall allows, which covers every buffered and \
              re-triggered jump (a ground jump then a wall jump on consecutive updates). Refuted by \
              the CONCRETE SEARCH, which runs the cart's buttons exactly.",
    },
    Entry {
        name: "player remainder",
        flag: Flag::Always,
        target: Target::Objects("player"),
        slots: &[
            slot(&["rem", "x"], Stored::Range { lo: REM.0, hi: REM.1 }, Input::Stored),
            slot(&["rem", "y"], Stored::Range { lo: REM.0, hi: REM.1 }, Input::Stored),
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
        why: "`dash_effect_time` decrements every frame; the traced cart reads it only to decrement \
              it (and the original cart only as `dash_effect_time > 0`), so every value <= 0 behaves \
              alike and `max(v, 0)` keeps every read.",
    },
    Entry {
        name: "strawberry bob",
        flag: Flag::Always,
        target: Target::Objects("fruit"),
        slots: &[
            slot(&["y"], Stored::Band { around: "start", radius: FRUIT_BOB_RAW }, Input::Stored),
            slot(&["off"], Stored::Phase { lo: FRUIT_OFF.0, hi: FRUIT_OFF.1 }, Input::Stored),
        ],
        kind: Kind::Over,
        hook: None,
        why: "A live strawberry's bob forgotten, `off` and `y` TOGETHER (one without the other is a \
              row no level has). `off` is a nonnegative integer read only as `sin(off / 40)`, \
              periodic in it with period 40 bit-exactly (`pico8_num::test_pico8_sin_period_40_bit_exact`), \
              so one period [0, 39] covers every phase (in doubles `sin` is not bit-periodic, but \
              `off` feeds only `y`, and [0, 39] makes every `y` of the band); `y` is the band \
              `start +- 2.5` the bob stays in, owed. The fruit's collision reads `y`, so a collect \
              becomes possible at any phase: refuted by the CONCRETE SEARCH.",
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
              search's tests depend on, so every value of them behaves alike. Checked: traced as free \
              input cells along room (4,0)'s and (6,2)'s 100% routes and room (5,2)'s (the orb), no \
              other stored field, guard, error or raise reads them, but for an error's overflow check of their own arithmetic (`minutes + 1`), \
              which holds at the stored 0 (`trace::widen::tests::dead_fields_are_unread`).",
    },
    Entry {
        name: "key sprite",
        flag: Flag::Always,
        target: Target::Objects("key"),
        slots: &[slot(&["spr"], Stored::Num(8 << 16), Input::Stored), slot(&["flip", "x"], Stored::Bool(false), Input::Stored)],
        kind: Kind::Exact,
        hook: None,
        why: "DEAD fields: the key's `spr` (from `frames`) and `flip.x` (from `spr`) are read only by \
              each other and the draw, so every value behaves alike. Checked with the timers.",
    },
    Entry {
        name: "sound timers",
        flag: Flag::Always,
        target: Target::Globals,
        slots: &[slot(&["sfx_timer"], Stored::Num(0), Input::Stored), slot(&["music_timer"], Stored::Num(0), Input::Stored)],
        kind: Kind::Exact,
        hook: None,
        why: "DEAD fields: each read only to count itself down and to decide a call of the no-op \
              `sfx`/`music`, so every value behaves alike. Checked with the timers.",
    },
    Entry {
        name: "fly fruit sound delay",
        flag: Flag::Always,
        target: Target::Objects("fly_fruit"),
        slots: &[slot(&["sfx_delay"], Stored::Num(0), Input::Stored)],
        kind: Kind::Exact,
        hook: None,
        why: "DEAD field: read only to count itself down, then to call the no-op `sfx` and set the \
              dead `sfx_timer`, so every value behaves alike. Checked with the timers.",
    },
    Entry {
        name: "screen shake",
        flag: Flag::Always,
        target: Target::Globals,
        slots: &[slot(&["shake"], Stored::Num(0), Input::Stored)],
        kind: Kind::Exact,
        hook: None,
        why: "DEAD field: read only to count itself down and to call the no-op `camera` (with `rnd` \
              offsets, unread), so every value behaves alike. Checked with the timers.",
    },
    Entry {
        name: "fly fruit",
        flag: Flag::Fruit,
        target: Target::Objects("fly_fruit"),
        slots: &[
            slot(&["step"], Stored::UnknownNum { countdown: false }, Input::Unknown),
            slot(&["y"], Stored::UnknownNum { countdown: false }, Input::Unknown),
            slot(&["spd", "y"], Stored::Range { lo: FLY_FRUIT_SPD_Y.0, hi: FLY_FRUIT_SPD_Y.1 }, Input::Literal),
            slot(&["rem", "y"], Stored::Range { lo: REM.0, hi: REM.1 }, Input::Literal),
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
        slots: &[Slot { field: &["delay"], stored: Stored::UnknownNum { countdown: true }, input: Input::Unknown, optional: true }],
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
        slots: &[slot(&["timer"], Stored::UnknownNum { countdown: true }, Input::Unknown)],
        kind: Kind::Over,
        hook: None,
        why: "As the fall floor's countdown.",
    },
    Entry {
        name: "spring phase",
        flag: Flag::Near,
        target: Target::Objects("spring"),
        slots: &[
            slot(&["spr"], Stored::Range { lo: SPRING_SPR_RANGE.0, hi: SPRING_SPR_RANGE.1 }, Input::Stored),
            Slot { field: &["delay"], stored: Stored::UnknownNum { countdown: true }, input: Input::Unknown, optional: true },
            slot(&["hide_in"], Stored::UnknownNum { countdown: true }, Input::Unknown),
            slot(&["hide_for"], Stored::UnknownNum { countdown: true }, Input::Unknown),
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
            slot(&["spr"], Stored::Range { lo: BALLOON_SPR_RANGE.0, hi: BALLOON_SPR_RANGE.1 }, Input::Stored),
            slot(&["y"], Stored::Band { around: "start", radius: BALLOON_BOB_RAW }, Input::Stored),
            slot(&["offset"], Stored::Phase { lo: 0, hi: BALLOON_PERIOD_RAW }, Input::Stored),
        ],
        kind: Kind::Over,
        hook: None,
        why: "The balloon's sprite (0 popped, 22 present) its range and its `y` the bob band \
              `start +- 2` (the bob runs only when `spr == 22`, and an exact `y` would give every \
              state a twin). Its update becomes \"maybe refill the dash\". The phase `offset` is read \
              only as `sin(offset)` into `y`, so one period [0, 1) covers every `y` the bob makes \
              (an unseeded `rnd` phase is that period already; a SEEDED one, which the float tools \
              always set, would otherwise split on every undecided `spr == 22`: room (4,2)'s six \
              balloons made f^6 states before the player). Refuted by the CONCRETE SEARCH.",
    },
    Entry {
        name: "near fall floor",
        flag: Flag::Near,
        target: Target::Objects("fall_floor"),
        slots: &[
            slot(&["state"], Stored::Range { lo: FLOOR_STATE_RANGE.0, hi: FLOOR_STATE_RANGE.1 }, Input::Hook),
            slot(&["collideable"], Stored::UnknownBool, Input::Hook),
        ],
        kind: Kind::Over,
        hook: Some(Hook::NearFloor),
        why: "Each fall floor's `state` [0, 2] and `collideable` unknown, EXCEPT where a player \
              certainly overlaps the floor at the frame's end (there the cart's invariant, hidden, \
              owed). The input derives `collideable` as `state ~= 2` \
              (the cart keeps them in step). Refuted by the CONCRETE SEARCH or the ladder \
              `r0sxn,r0sx`.",
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

/// A CLOCK: objects whose every field is a function of the time since the
/// room loaded (each moves on its own, nothing else writes it). They are
/// EXACT at every level; where a shape holds two or more, a few fields of
/// the FIRST instance key them all (`leads`, in order): every other value
/// cell of every instance is a FOLLOWER of one of them (`exact::Deps`,
/// `key_deps`) and carries no key bits. Nothing is widened: a follower's
/// code is recorded per leader code and every row is checked against it
/// wherever a key is formed, a difference FATAL (`exact`'s module doc), so
/// `why` is checked, never trusted.
pub struct Clock {
    pub objects: &'static str,
    pub leads: &'static [Lead],
    pub why: &'static str,
}

/// A leader: a field of a clock's first instance, and what follows it.
pub struct Lead {
    pub field: &'static [&'static str],
    pub follows: Follows,
}

pub enum Follows {
    /// The same field of every other instance.
    SameField,
    /// Every value cell of every instance that no earlier lead holds.
    Rest,
}

pub const CLOCKS: &[Clock] = &[Clock {
    objects: "platform",
    leads: &[Lead { field: &["spd", "x"], follows: Follows::SameField }, Lead { field: &["rem", "x"], follows: Follows::Rest }],
    why: "A platform moves `dir * 0.65` an update (`move`: `rem.x` the sub-pixel remainder, `x` \
          the pixels, wrapping between -16 and 128), `last` is `x` after its update, `spd.x` is \
          set from `dir` by the update; nothing but its own update writes them, so every \
          platform is a function of the updates since the room loaded (a restart reloads them \
          all). `rem.x` steps by +-0x0000a666 modulo 1, so no value repeats within 32768 \
          updates - except the first: `move` reads `spd.x` before the first update sets it, so \
          that update leaves `rem.x` (and everything but `spd.x`) as loaded. `spd.x` is 0 only \
          before it: `spd.x` tells those two apart and leads the other `spd.x`s, `rem.x` leads \
          the rest.",
}];

/// The key dependencies `rt2`'s shape declares (`CLOCKS`), from its names:
/// none where no clock has two or more instances.
pub fn key_deps<N: Arithmetic>(rt2: &Rt2<N>) -> Vec<crate::exact::Deps> {
    let mut out = Vec::new();
    for clock in CLOCKS {
        let ty = celeste_names::global(clock.objects);
        let objs = rt2.objects_of_type(ty);
        if objs.len() < 2 {
            continue;
        }
        let path = |field: &[&str]| -> Vec<u32> { field.iter().map(|n| celeste_names::field(n)).collect() };
        let cell = |obj: u32, field: &[&str]| rt2.slot_cell(Some(obj), &path(field)).unwrap_or_else(|| panic!("clock {}: an instance has no `{}`", clock.objects, field.join(".")));
        let leaders: Vec<u32> = clock.leads.iter().map(|l| cell(objs[0], l.field)).collect();
        let mut taken: std::collections::BTreeSet<u32> = leaders.iter().copied().collect();
        for (lead, &leader) in clock.leads.iter().zip(&leaders) {
            let mut followers: Vec<u32> = match lead.follows {
                Follows::SameField => objs[1..].iter().map(|&o| cell(o, lead.field)).collect(),
                Follows::Rest => objs.iter().flat_map(|&o| rt2.own_value_cells(o)).collect(),
            };
            followers.sort_unstable();
            followers.dedup();
            followers.retain(|c| !taken.contains(c));
            taken.extend(followers.iter().copied());
            out.push(crate::exact::Deps { leader, followers });
        }
    }
    out
}

impl<N: Arithmetic> Rt2<N> {
    /// An object's own value cells: its value fields and those of the
    /// tables they point to (`rem`, `spd`, `hitbox`, ...), not its `type`'s.
    fn own_value_cells(&self, obj: u32) -> Vec<u32> {
        let val = |c: u32| matches!(self.cells()[c as usize], Cell::Val);
        let mut out = Vec::new();
        let Cell::Obj(fields) = &self.cells()[obj as usize] else { return out };
        for &(name, cell) in fields.iter().filter(|f| val(f.1)) {
            out.push(cell);
            if name == names().type_ {
                continue;
            }
            if let Col::U(AV::Ptr(sub)) = self.cols[cell as usize] {
                if let Cell::Obj(sub_fields) = &self.cells()[sub as usize] {
                    out.extend(sub_fields.iter().map(|f| f.1).filter(|&c| val(c)));
                }
            }
        }
        out
    }
}

/// The `AbsentAsZero` slots: `(type global, field)`.
pub fn absent_as_zero() -> impl Iterator<Item = (&'static str, &'static str)> {
    TABLE.iter().filter_map(|e| match (e.target, e.slots) {
        (Target::Objects(ty), [Slot { field: [f], stored: Stored::AbsentAsZero, .. }]) => Some((ty, *f)),
        _ => None,
    })
}

/// A raw 16.16 number for reading: an integer, else its decimal value.
fn show_raw(r: i32) -> String {
    if r % 0x1_0000 == 0 {
        format!("{}", r >> 16)
    } else {
        let v = format!("{:.5}", r as f64 / 65536.0);
        v.trim_end_matches('0').to_string()
    }
}

impl std::fmt::Display for Stored {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stored::Range { lo, hi } => write!(f, "[{}, {}]; owed: inside", show_raw(*lo), show_raw(*hi)),
            Stored::Band { around, radius } => write!(f, "`{around}` +- {}; owed: inside", show_raw(*radius)),
            Stored::Phase { lo, hi } => write!(f, "one period [{}, {}]", show_raw(*lo), show_raw(*hi)),
            Stored::AtLeast(k) => write!(f, "max(v, {})", show_raw(*k)),
            Stored::UnknownNum { countdown: false } => write!(f, "the unknown number"),
            Stored::UnknownNum { countdown: true } => write!(f, "the unknown number, a countdown"),
            Stored::UnknownBool => write!(f, "the unknown boolean"),
            Stored::Num(k) => write!(f, "{} (dead)", show_raw(*k)),
            Stored::Bool(b) => write!(f, "{b} (dead)"),
            Stored::FullPeriod(p) => write!(f, "a full period as [0, {}]; owed: the width", show_raw(*p)),
            Stored::AbsentAsZero => write!(f, "0 where absent"),
        }
    }
}

impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Input::Stored => "as stored",
            Input::BothWays => "both ways, every lane",
            Input::Atom => "an undecided atom",
            Input::Unknown => "the unknown number",
            Input::Literal => "the literal; owed: inside",
            Input::Hook => "by the hook",
        })
    }
}

/// THE TABLE as plans/abstractions.md lists it (a test keeps the two equal).
pub fn render_markdown() -> String {
    let mut out = String::new();
    out += "| entry | level | objects | slot | stored | read as | kind |\n|---|---|---|---|---|---|---|\n";
    for e in TABLE {
        let flag = match e.flag {
            Flag::Always => "every",
            Flag::Fruit => "`f`",
            Flag::Near => "`n`",
        };
        let target = match e.target {
            Target::Objects(t) => format!("`{t}`"),
            Target::Globals => "globals".to_string(),
        };
        let kind = match (e.kind, e.hook) {
            (Kind::Exact, _) => "exact".to_string(),
            (Kind::Over, None) => "over".to_string(),
            (Kind::Over, Some(h)) => format!("over, hook `{h:?}`"),
        };
        for (k, s) in e.slots.iter().enumerate() {
            let (name, flag, target, kind) = if k == 0 { (e.name, flag, target.as_str(), kind.as_str()) } else { ("", "", "", "") };
            let opt = if s.optional { " (may be absent)" } else { "" };
            out += &format!("| {name} | {flag} | {target} | `{}`{opt} | {} | {} | {kind} |\n", s.field.join("."), s.stored, s.input);
        }
    }
    out += "\n";
    for e in TABLE {
        let kind = match e.kind {
            Kind::Exact => "EXACT",
            Kind::Over => "over-approximation",
        };
        out += &format!("- **{}** ({kind}). {}\n", e.name, e.why.split_whitespace().collect::<Vec<_>>().join(" "));
    }
    out
}

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
        let (g, f) = (celeste_names::global, celeste_names::field);
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
fn span<N: Copy>(v: AV<N>) -> Option<(N, N)> {
    match v {
        AV::Num(n) => Some((n, n)),
        AV::Ival(a, b) => Some((a, b)),
        _ => None,
    }
}

/// A table constant (raw 16.16) in arithmetic `N`, exactly (every 16.16
/// value is a double).
pub fn constant<N: Arithmetic>(raw: i32) -> N {
    N::from_fixed_raw(raw)
}

/// A stored range's ends in `N`. `REM` is the HALF-OPEN remainder
/// [-1/2, 1/2): its last value is the arithmetic's last below 1/2 (16.16's
/// `1/2 - 2^-16`, a double's `pred(1/2)`); every other range is its
/// inclusive ends.
pub fn range<N: Arithmetic>(lo: i32, hi: i32) -> (N, N) {
    if (lo, hi) == REM {
        let half = constant::<N>(REM.1 + 1);
        (constant(lo), N::below(half).expect("a value below 1/2"))
    } else {
        (constant(lo), constant(hi))
    }
}

/// What a slot must hold before an entry overwrites it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Holds {
    Number,
    /// A number, or (blocks) the unknown one.
    NumberOrUnknown,
    Boolean,
}

/// A value an entry writes.
#[derive(Clone, Copy, Debug)]
pub enum Lit<N> {
    /// `[lo, hi]`.
    Ival(N, N),
    Num(N),
    Bool(bool),
    /// The unknown number (`countdown`: a countdown's).
    UnknownNum { countdown: bool },
    UnknownBool,
}

/// A TARGET of the table's output side (`widen_level`): the block side
/// (`Rt2`, below) or the tracer's state and graph (`trace::widen`). The
/// interpreter decides what each `Stored` checks, owes and writes, and in
/// which order (the tracer's node order follows it); a target says what a
/// value, a check and an obligation are in its terms. `ei` is the entry's
/// index in `TABLE`. An `Err` is a modelling surprise (the tracer refuses;
/// the block side panics).
pub trait Widener {
    /// An instance of an entry's target (an object; the globals).
    type Instance;
    /// Where a slot lives.
    type Place;
    type Val;
    type N: Arithmetic;

    fn instances(&mut self, ei: usize) -> anyhow::Result<Vec<Self::Instance>>;
    /// Slot `k` of the instance (`None`: absent).
    fn slot(&mut self, ei: usize, at: &Self::Instance, k: usize) -> anyhow::Result<Option<Self::Place>>;
    fn write(&mut self, p: &Self::Place, v: Self::Val) -> anyhow::Result<()>;
    fn check(&mut self, ei: usize, p: &Self::Place, holds: Holds) -> anyhow::Result<()>;
    fn value(&mut self, v: Lit<Self::N>) -> Self::Val;
    /// `max(v, m)` of the slot's number, the cart's (a double's tie, `-0`
    /// against `0`, gives `m`).
    fn at_least(&mut self, ei: usize, p: &Self::Place, m: Self::N) -> anyhow::Result<Self::Val>;
    /// Owe the slot's number inside `[lo, hi]` (nothing where proved).
    fn owe_inside(&mut self, ei: usize, p: &Self::Place, lo: Self::N, hi: Self::N) -> anyhow::Result<()>;
    /// Slot `k`'s `Band` (`radius` around the instance's constant field
    /// `around`), owed as `owe_inside`; the band.
    fn band(&mut self, ei: usize, k: usize, at: &Self::Instance, p: &Self::Place, radius: Self::N) -> anyhow::Result<Self::Val>;
    /// Is the slot's number a point on every lane?
    fn exact(&mut self, ei: usize, p: &Self::Place) -> anyhow::Result<bool>;
    /// Owe the slot's width at least `period`.
    fn owe_width(&mut self, ei: usize, p: &Self::Place, period: Self::N) -> anyhow::Result<()>;
    /// `Hook::NearFloor`, per side (they differ mid split frame; their
    /// comments).
    fn near_floors(&mut self, ei: usize) -> anyhow::Result<()>;
}

/// THE OUTPUT SIDE, both targets: `level`'s entries (`TABLE`, in order)
/// applied to `w`, per instance, per slot in order.
pub fn widen_level<W: Widener>(w: &mut W, level: Level) -> anyhow::Result<()> {
    /// A slot checked, then overwritten by a literal.
    fn checked<W: Widener>(w: &mut W, ei: usize, p: &W::Place, holds: Holds, v: Lit<W::N>) -> anyhow::Result<Option<W::Val>> {
        w.check(ei, p, holds)?;
        Ok(Some(w.value(v)))
    }
    for (ei, e) in TABLE.iter().enumerate().filter(|(_, e)| level.has(e.flag)) {
        if e.hook == Some(Hook::NearFloor) {
            w.near_floors(ei)?;
            continue;
        }
        for at in w.instances(ei)? {
            let places: Vec<Option<W::Place>> = (0..e.slots.len()).map(|k| w.slot(ei, &at, k)).collect::<anyhow::Result<_>>()?;
            for (k, s) in e.slots.iter().enumerate() {
                let Some(p) = &places[k] else {
                    anyhow::ensure!(s.optional, "widening {:?}: no `{}`", e.name, s.field.join("."));
                    continue;
                };
                let new = match s.stored {
                    Stored::Range { lo, hi } => {
                        let (lo, hi) = range::<W::N>(lo, hi);
                        w.owe_inside(ei, p, lo, hi)?;
                        Some(w.value(Lit::Ival(lo, hi)))
                    }
                    Stored::Band { radius, .. } => Some(w.band(ei, k, &at, p, constant(radius))?),
                    Stored::AtLeast(m) => Some(w.at_least(ei, p, constant(m))?),
                    Stored::FullPeriod(_) if w.exact(ei, p)? => None,
                    Stored::FullPeriod(period) => {
                        w.owe_width(ei, p, constant(period))?;
                        Some(w.value(Lit::Ival(constant(0), constant(period))))
                    }
                    Stored::AbsentAsZero => None,
                    Stored::Phase { lo, hi } => checked(w, ei, p, Holds::Number, Lit::Ival(constant(lo), constant(hi)))?,
                    Stored::UnknownNum { countdown } => checked(w, ei, p, Holds::NumberOrUnknown, Lit::UnknownNum { countdown })?,
                    Stored::UnknownBool => checked(w, ei, p, Holds::Boolean, Lit::UnknownBool)?,
                    Stored::Num(n) => checked(w, ei, p, Holds::Number, Lit::Num(constant(n)))?,
                    Stored::Bool(b) => checked(w, ei, p, Holds::Boolean, Lit::Bool(b))?,
                };
                if let Some(v) = new {
                    w.write(p, v)?;
                }
            }
        }
    }
    Ok(())
}

impl<N: Arithmetic> Rt2<N> {
    /// A level's widenings (`TABLE`, `widen_level`), as its kernels bake them
    /// (`trace::widen`), so a finer row can be looked up as the level's
    /// node. Each asserts what its entry owes.
    pub fn widen_to(&mut self, level: Level) {
        widen_level(self, level).unwrap_or_else(|e| panic!("{e:#}"));
    }

    /// The cell of a slot: `field` below the object `base`, else from the
    /// globals.
    fn slot_cell(&self, base: Option<u32>, field: &[u32]) -> Option<u32> {
        let (mut cell, rest) = match base {
            Some(obj) => (self.obj_field_cell(obj, field[0])?, &field[1..]),
            None => {
                let c = self.shape.globals()[field[0] as usize];
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

    /// Every lane of cell `c` satisfies `ok`, or an error naming the entry.
    fn check_lanes(&self, ei: usize, c: u32, what: &str, ok: impl Fn(AV<N>) -> bool) -> anyhow::Result<()> {
        for lane in 0..self.width {
            let v = self.cols[c as usize].at(lane);
            anyhow::ensure!(ok(v), "widening {:?}: lane {lane} holds {v:?}, {what}", TABLE[ei].name);
        }
        Ok(())
    }
}

/// The block side: a slot is a cell, a value a column; what is owed is
/// asserted per lane.
impl<N: Arithmetic> Widener for Rt2<N> {
    type Instance = Option<u32>;
    type Place = u32;
    type Val = Col<N>;
    type N = N;

    fn instances(&mut self, ei: usize) -> anyhow::Result<Vec<Option<u32>>> {
        Ok(match resolved()[ei].ty {
            Some(ty) => self.objects_of_type(ty).into_iter().map(Some).collect(),
            None => vec![None],
        })
    }
    fn slot(&mut self, ei: usize, at: &Option<u32>, k: usize) -> anyhow::Result<Option<u32>> {
        Ok(self.slot_cell(*at, &resolved()[ei].fields[k]))
    }
    fn write(&mut self, c: &u32, v: Col<N>) -> anyhow::Result<()> {
        self.cols[*c as usize] = v;
        Ok(())
    }
    fn check(&mut self, ei: usize, c: &u32, holds: Holds) -> anyhow::Result<()> {
        match holds {
            Holds::Number => self.check_lanes(ei, *c, "not a number", |v| span(v).is_some()),
            Holds::NumberOrUnknown => self.check_lanes(ei, *c, "not a number", |v| span(v).is_some() || v == AV::UNum),
            Holds::Boolean => self.check_lanes(ei, *c, "not a boolean", |v| matches!(v, AV::Bool(_) | AV::UBool)),
        }
    }
    fn value(&mut self, v: Lit<N>) -> Col<N> {
        Col::U(match v {
            Lit::Ival(lo, hi) => AV::Ival(lo, hi),
            Lit::Num(n) => AV::Num(n),
            Lit::Bool(b) => AV::Bool(b),
            Lit::UnknownNum { .. } => AV::UNum,
            Lit::UnknownBool => AV::UBool,
        })
    }
    fn at_least(&mut self, ei: usize, c: &u32, m: N) -> anyhow::Result<Col<N>> {
        let max = |n: N| N::max(n, m);
        let at_least = |v: AV<N>| match v {
            AV::Num(n) => Ok(AV::Num(max(n))),
            AV::Ival(a, b) => Ok(AV::Ival(max(a), max(b))),
            other => Err(anyhow::anyhow!("widening {:?}: {other:?} is not a number", TABLE[ei].name)),
        };
        Ok(match &self.cols[*c as usize] {
            Col::U(v) => Col::U(at_least(*v)?),
            Col::N(vs) => Col::N(vs.iter().map(|n| max(*n)).collect()),
            col => collapse_uniform(crate::runtime2::compress_num_v((0..self.width).map(|l| at_least(col.at(l))).collect::<anyhow::Result<_>>()?)),
        })
    }
    fn owe_inside(&mut self, ei: usize, c: &u32, lo: N, hi: N) -> anyhow::Result<()> {
        let le = N::le;
        self.check_lanes(ei, *c, "outside the widening", |v| span(v).is_some_and(|(a, b)| le(lo, a) && le(b, hi)))
    }
    fn band(&mut self, ei: usize, k: usize, at: &Option<u32>, c: &u32, r: N) -> anyhow::Result<Col<N>> {
        let name = TABLE[ei].name;
        let le = N::le;
        let f = resolved()[ei].around[k].expect("a band's centre field");
        let obj = at.ok_or_else(|| anyhow::anyhow!("widening {name:?}: a band is around an object's field"))?;
        let centre = self.obj_field_cell(obj, f).ok_or_else(|| anyhow::anyhow!("widening {name:?}: no band centre"))?;
        let mut band = Vec::with_capacity(self.width);
        for lane in 0..self.width {
            let (lo, hi) = match self.cols[centre as usize].at(lane) {
                AV::Num(s) => match (N::sub(s, r), N::add(s, r)) {
                    (Some(lo), Some(hi)) => (lo, hi),
                    _ => anyhow::bail!("widening {name:?}: lane {lane}'s band {s:?} +- {r:?} leaves the arithmetic's range"),
                },
                other => anyhow::bail!("widening {name:?}: the band's centre is {other:?}"),
            };
            let v = self.cols[*c as usize].at(lane);
            anyhow::ensure!(span(v).is_some_and(|(a, b)| le(lo, a) && le(b, hi)), "widening {name:?}: lane {lane} holds {v:?}, outside the band [{lo:?}, {hi:?}]");
            band.push((lo, hi));
        }
        Ok(collapse_uniform(Col::I(band)))
    }
    fn exact(&mut self, _ei: usize, c: &u32) -> anyhow::Result<bool> {
        Ok((0..self.width).all(|lane| matches!(self.cols[*c as usize].at(lane), AV::Num(_))))
    }
    fn owe_width(&mut self, ei: usize, c: &u32, period: N) -> anyhow::Result<()> {
        let le = N::le;
        self.check_lanes(ei, *c, "not a full period", |v| matches!(v, AV::Ival(lo, hi) if N::sub(hi, lo).is_some_and(|w| le(period, w))))
    }
    fn near_floors(&mut self, ei: usize) -> anyhow::Result<()> {
        self.widen_near_floors(&resolved()[ei]);
        Ok(())
    }
}

impl<N: Arithmetic> Rt2<N> {
    /// `Hook::NearFloor` on blocks (`trace::widen::widen_near_floors`): `state`
    /// and `collideable` widened except where a player certainly overlaps the
    /// floor (kept as they are: the kernels store the cart's invariant there,
    /// which is what a concrete row holds).
    fn widen_near_floors(&mut self, r: &Resolved) {
        let le = N::le;
        let (slo, shi) = range::<N>(FLOOR_STATE_RANGE.0, FLOOR_STATE_RANGE.1);
        let span_of = |v: AV<N>, what: &str| span(v).unwrap_or_else(|| panic!("near floor widening: {what} is {v:?}"));
        let players: Vec<(u32, u32)> = self
            .player_objects()
            .into_iter()
            .map(|o| {
                let cell = |f: u32, what: &str| self.obj_field_cell(o, f).unwrap_or_else(|| panic!("near floor widening: the player has no `{what}`"));
                (cell(names().x, "x"), cell(names().y, "y"))
            })
            .collect();
        let ty = r.ty.expect("the near floors are objects");
        for obj in self.objects_of_type(ty) {
            // Floors never move: one position in every lane.
            let at = |f: u32| {
                let col = self.obj_field_cell(obj, f).map(|c| &self.cols[c as usize]);
                col.and_then(|c| c.uniform_num(self.width)).unwrap_or_else(|| panic!("near floor widening: a fall floor's position is {col:?}, not one number"))
            };
            let (fx, fy) = (at(names().x), at(names().y));
            let window = floor_player_window((fx, fy)).unwrap_or_else(|| panic!("near floor widening: a fall floor at ({fx:?}, {fy:?}): its window leaves the arithmetic's range"));
            let overlap: Vec<bool> = (0..self.width)
                .map(|lane| {
                    players.iter().any(|&(cx, cy)| {
                        player_overlaps_floor(window, span_of(self.cols[cx as usize].at(lane), "the player's x"), span_of(self.cols[cy as usize].at(lane), "the player's y"))
                    })
                })
                .collect();
            let field = |k: usize| self.slot_cell(Some(obj), &r.fields[k]).unwrap_or_else(|| panic!("near floor widening: the fall floor has no `{:?}`", r.fields[k]));
            let (cs, cc) = (field(0), field(1));
            let state: Vec<(N, N)> = (0..self.width)
                .map(|lane| {
                    let (a, b) = span_of(self.cols[cs as usize].at(lane), "a fall floor's `state`");
                    if overlap[lane] {
                        (a, b)
                    } else {
                        assert!(le(slo, a) && le(b, shi), "near floor widening: lane {lane} of `state` is [{a:?}, {b:?}], which the widening does not contain");
                        (slo, shi)
                    }
                })
                .collect();
            let coll: Vec<AV<N>> = (0..self.width)
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
    use crate::runtime2::P8;

    #[test]
    fn levels_parse_and_print() {
        assert_eq!(Level::parse("r0sx").unwrap(), Level::EXACT);
        for spec in ["r0sx", "r0sxf", "r0sxn", "r0sxfn"] {
            assert_eq!(Level::parse(spec).unwrap().to_string(), spec);
        }
        let n = Level::parse("r0sxn").unwrap();
        assert!(n.floors_near && !n.fruit);
        // `h` is gone: the buttons are widened at every level.
        for spec in ["r0sxh", "r0sxhn", "r0sxnh", "r0sxfhn"] {
            let e = Level::parse(spec).expect_err("`h`");
            assert!(e.contains("`h` was removed: held buttons are widened at every level (plans/correctness.md C7)"), "{e}");
        }
        // Only the `r0sx` prefix parses.
        assert!(Level::parse("r1sx").is_err());
        assert!(Level::parse("rxsx").is_err());
        assert!(Level::parse("r0s16").is_err());
        assert!(Level::parse("y2r0sx").is_err());
        assert!(Level::parse("r0sxb").is_err(), "floors unknown `b` is gone");
        assert!(Level::parse("r0sxnf").is_err(), "flags in order");
        for spec in ["r0sxnp", "r0sxfp", "r0sxp"] {
            let e = Level::parse(spec).expect_err("`p` is gone");
            assert!(e.contains("`p` (widened platforms) was removed: platforms are exact"), "{spec}: {e}");
        }
    }

    /// `floor_player_window` is the cart's `floor.collide(player, 0, 0)` at
    /// every whole-pixel position around a floor.
    #[test]
    fn the_overlap_window_is_the_carts_collide() {
        let (fx, fy) = (48i16, 112i16);
        let window = floor_player_window((P8::from_i16(fx), P8::from_i16(fy))).expect("in range");
        assert_eq!(floor_player_window((P8::from_raw(i32::MAX), P8::from_i16(fy))), None, "a window past the range is no window");
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

    /// plans/abstractions.md lists exactly the table (`render_markdown`,
    /// between its markers). `CELESTE_REGENERATE=1` rewrites the doc's copy.
    #[test]
    fn the_abstractions_doc_lists_the_table() {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../plans/abstractions.md");
        let doc = std::fs::read_to_string(path).expect("plans/abstractions.md");
        let (begin, end) = ("<!-- THE WIDENING TABLE", "<!-- END OF THE WIDENING TABLE -->");
        let a = doc.find(begin).expect("the table's begin marker");
        let a = a + doc[a..].find('\n').expect("the marker's line") + 1;
        let b = doc.find(end).expect("the table's end marker");
        let want = render_markdown();
        if std::env::var_os("CELESTE_REGENERATE").is_some() {
            std::fs::write(path, format!("{}{want}{}", &doc[..a], &doc[b..])).expect("rewrite plans/abstractions.md");
            return;
        }
        assert!(doc[a..b] == want, "plans/abstractions.md does not list the widening table; regenerate with CELESTE_REGENERATE=1 (this test), then review:\n{want}");
    }

    /// Every name in the table resolves.
    #[test]
    fn the_table_resolves() {
        assert_eq!(resolved().len(), TABLE.len());
    }
}
