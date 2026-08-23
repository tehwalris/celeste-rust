// GENERATED from a TRACED frame (shape 0). Do not edit.
//
// One input shape, 4 output shapes, 24 distinct button
// assignments. See `trace::kernel` for what this interface is
// and why it is not the walk's.
#![allow(unused_variables, unused_mut, unused_imports, clippy::all)]
use celeste_engine::kernel::*;
use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval as IV};
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::{Rt2, Col, AV};
use celeste_engine::slots::{build_block, resolve_path, SCell};
use std::sync::Arc;

pub struct G<'a> { pub cart: &'a CartData, pub cache: &'a CollisionCache }

/// The canonical shape this kernel was traced for.
pub const SHAPE: u64 = 4684018979372124476;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c264: P8,
    pub c265: P8,
    pub c266: P8,
    pub c267: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("has_dashed", "bool"),
    ("has_key", "bool"),
    ("max_djump", "num"),
    ("objects[0].collideable", "bool"),
    ("objects[0].delay", "num"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].state", "num"),
    ("objects[0].target.x", "num"),
    ("objects[0].target.y", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("pause_player", "bool"),
    ("seconds", "num"),
    ("will_restart", "bool"),
];

/// Per-lane inputs, in `ROW_SLOTS` order.
pub struct RowsIn {
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c232: u16,
    pub c233: ZN,
    pub c262: u16,
    pub c263: u16,
    pub c268: ZN,
    pub c269: ZN,
    pub c242: u16,
    pub c270: ZN,
    pub c271: ZN,
    pub c244: ZN,
    pub c245: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c43: u16,
    pub c85: ZN,
    pub c38: u16,
}

/// Cell ids for `ROW_SLOTS` in the bound block.
pub struct RowSlots {
    pub c39: u32,
    pub c84: u32,
    pub c20: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c232: u32,
    pub c233: u32,
    pub c262: u32,
    pub c263: u32,
    pub c268: u32,
    pub c269: u32,
    pub c242: u32,
    pub c270: u32,
    pub c271: u32,
    pub c244: u32,
    pub c245: u32,
    pub c272: u32,
    pub c273: u32,
    pub c248: u32,
    pub c249: u32,
    pub c43: u32,
    pub c85: u32,
    pub c38: u32,
}

/// Bind to a block by PATH. `None` means this block does not
/// have a slot the kernel needs, or holds it at the wrong kind -
/// either way the block takes the interpreter path.
pub fn bind(b: &Rt2) -> Option<(Uni, RowSlots)> {
    let cell = |p: &str| resolve_path(b, p).ok();
    let u = Uni {
        c264: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c265: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c266: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c267: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c232: cell("objects[0].collideable")?,
        c233: cell("objects[0].delay")?,
        c262: cell("objects[0].flip.x")?,
        c263: cell("objects[0].flip.y")?,
        c268: cell("objects[0].rem.x")?,
        c269: cell("objects[0].rem.y")?,
        c242: cell("objects[0].solids")?,
        c270: cell("objects[0].spd.x")?,
        c271: cell("objects[0].spd.y")?,
        c244: cell("objects[0].spr")?,
        c245: cell("objects[0].state")?,
        c272: cell("objects[0].target.x")?,
        c273: cell("objects[0].target.y")?,
        c248: cell("objects[0].x")?,
        c249: cell("objects[0].y")?,
        c43: cell("pause_player")?,
        c85: cell("seconds")?,
        c38: cell("will_restart")?,
    };
    Some((u, s))
}

/// Which slot stopped `bind`. `None` means it would have bound.
pub fn bind_why(b: &Rt2) -> Option<String> {
    for (p, kind) in UNI_SLOTS {
        let Ok(c) = resolve_path(b, p) else {
            return Some(format!("{}: no such slot", p));
        };
        let col = &b.cols[c as usize];
        let ok = matches!(
            (*kind, col),
            ("num", Col::U(AV::Num(_))) | ("bool", Col::U(AV::Bool(_)))
        );
        if !ok {
            return Some(format!(
                "{}: block-uniform {} slot holds {:?}",
                p, kind, col
            ));
        }
    }
    for (p, kind) in ROW_SLOTS {
        let Ok(c) = resolve_path(b, p) else {
            return Some(format!("{}: no such slot", p));
        };
        let col = &b.cols[c as usize];
        let lanes = (0..b.width).map(|i| col.at(i));
        let ok = match *kind {
            "num" => lanes.clone().all(|v| matches!(v, AV::Num(_))),
            _ => lanes.clone().all(|v| matches!(v, AV::Bool(_))),
        };
        if !ok {
            let bad = lanes.enumerate().find(|(_, v)| match *kind {
                "num" => !matches!(v, AV::Num(_)),
                _ => !matches!(v, AV::Bool(_)),
            });
            return Some(format!(
                "{}: per-lane {} slot holds {:?} at lane {:?}",
                p, kind, bad.map(|(_, v)| v), bad.map(|(i, _)| i)
            ));
        }
    }
    // Every slot resolves and every lane is the right kind, so
    // what stopped `rows` is REPRESENTATION: a column whose
    // values are right but whose storage the gather does not
    // accept (a `Col::V` of numbers where it wants `Col::N`).
    // `collapse_uniform_cols` is the usual missing step.
    None
}

/// Gather rows [lo, lo+16) into lane arrays. Short slices pad by
/// repeating the last row; padded lanes are ignored by width.
pub fn rows(b: &Rt2, s: &RowSlots, lo: usize) -> Option<RowsIn> {
    let at = |i: usize| -> usize { (lo + i).min(b.width - 1) };
    Some(RowsIn {
        c39: match &b.cols[s.c39 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c84: match &b.cols[s.c84 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c20: match &b.cols[s.c20 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c41: match &b.cols[s.c41 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c42: match &b.cols[s.c42 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c88: match &b.cols[s.c88 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c232: match &b.cols[s.c232 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c233: match &b.cols[s.c233 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c262: match &b.cols[s.c262 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c263: match &b.cols[s.c263 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c268: match &b.cols[s.c268 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c269: match &b.cols[s.c269 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c242: match &b.cols[s.c242 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c270: match &b.cols[s.c270 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c244: match &b.cols[s.c244 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c245: match &b.cols[s.c245 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c273: match &b.cols[s.c273 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c249: match &b.cols[s.c249 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c43: match &b.cols[s.c43 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c85: match &b.cols[s.c85 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c38: match &b.cols[s.c38 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
    })
}

// ---------------- outcome 0 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_0: &[(u32, &str)] = &[
    (174, "balloon.tile"),
    (201, "big_chest.tile"),
    (193, "chest.if_not_fruit"),
    (195, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (187, "fake_wall.if_not_fruit"),
    (188, "fake_wall.tile"),
    (177, "fall_floor.tile"),
    (183, "fly_fruit.if_not_fruit"),
    (185, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (179, "fruit.if_not_fruit"),
    (181, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (190, "key.if_not_fruit"),
    (191, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (43, "pause_player"),
    (156, "player_spawn.tile"),
    (158, "room.x"),
    (159, "room.y"),
    (85, "seconds"),
    (171, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_0: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_0: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151]),
    SCell::Obj(&[(7, 152), (5, 153), (6, 154)]),
    SCell::Obj(&[(5, 155), (8, 156), (6, 157)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 158), (2, 159)]),
    SCell::Arr(&[160, 161, 162, 163, 164, 165, 166, 167, 168, 169]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 170), (8, 171), (6, 172)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 179), (5, 180), (8, 181), (6, 182)]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (5, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(5, 197), (6, 198)]),
    SCell::Obj(&[(7, 199), (5, 200), (8, 201)]),
    SCell::Obj(&[(7, 202), (5, 203)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
];

pub const OUT_GLOBALS_0: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_0: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (152, 204),
    (153, 205),
    (154, 206),
    (155, 207),
    (157, 208),
    (160, 94),
    (161, 116),
    (162, 118),
    (163, 119),
    (164, 121),
    (165, 122),
    (166, 123),
    (167, 124),
    (168, 125),
    (169, 127),
    (170, 209),
    (172, 210),
    (173, 211),
    (175, 212),
    (176, 213),
    (178, 214),
    (180, 215),
    (182, 216),
    (184, 217),
    (186, 218),
    (189, 219),
    (192, 220),
    (194, 221),
    (196, 222),
    (197, 223),
    (198, 224),
    (199, 225),
    (200, 226),
    (202, 227),
    (203, 228),
];

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c42: ZB,
    pub c88: ZN,
    pub c43: ZB,
}

/// Outcome 0's per-assignment values and lane masks.
/// The cells of outcome 0 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared0`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut0 {
    pub c20: ZN,
    pub c41: ZB,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 1 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_1: &[(u32, &str)] = &[
    (174, "balloon.tile"),
    (201, "big_chest.tile"),
    (193, "chest.if_not_fruit"),
    (195, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (187, "fake_wall.if_not_fruit"),
    (188, "fake_wall.tile"),
    (177, "fall_floor.tile"),
    (183, "fly_fruit.if_not_fruit"),
    (185, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (179, "fruit.if_not_fruit"),
    (181, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (190, "key.if_not_fruit"),
    (191, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (232, "objects[0].collideable"),
    (268, "objects[0].dash_accel.x"),
    (269, "objects[0].dash_accel.y"),
    (234, "objects[0].dash_effect_time"),
    (270, "objects[0].dash_target.x"),
    (271, "objects[0].dash_target.y"),
    (236, "objects[0].dash_time"),
    (237, "objects[0].djump"),
    (272, "objects[0].flip.x"),
    (273, "objects[0].flip.y"),
    (239, "objects[0].grace"),
    (274, "objects[0].hitbox.h"),
    (275, "objects[0].hitbox.w"),
    (276, "objects[0].hitbox.x"),
    (277, "objects[0].hitbox.y"),
    (246, "objects[0].p_dash"),
    (247, "objects[0].p_jump"),
    (278, "objects[0].rem.x"),
    (279, "objects[0].rem.y"),
    (249, "objects[0].solids"),
    (280, "objects[0].spd.x"),
    (281, "objects[0].spd.y"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (43, "pause_player"),
    (156, "player_spawn.tile"),
    (158, "room.x"),
    (159, "room.y"),
    (85, "seconds"),
    (171, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_1: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_1: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151]),
    SCell::Obj(&[(7, 152), (5, 153), (6, 154)]),
    SCell::Obj(&[(5, 155), (8, 156), (6, 157)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 158), (2, 159)]),
    SCell::Arr(&[160, 161, 162, 163, 164, 165, 166, 167, 168, 169]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 170), (8, 171), (6, 172)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 179), (5, 180), (8, 181), (6, 182)]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (5, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(5, 197), (6, 198)]),
    SCell::Obj(&[(7, 199), (5, 200), (8, 201)]),
    SCell::Obj(&[(7, 202), (5, 203)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 230), (20, 231), (11, 232), (28, 233), (33, 234), (27, 235), (26, 236), (30, 237), (14, 238), (29, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (32, 246), (31, 247), (4, 248), (12, 249), (3, 250), (13, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[204]),
    SCell::Clo(25, &[204]),
    SCell::Obj(&[(1, 268), (2, 269)]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Obj(&[(17, 274), (16, 275), (1, 276), (2, 277)]),
    SCell::Clo(24, &[204]),
    SCell::Clo(23, &[204]),
    SCell::Clo(27, &[204]),
    SCell::Clo(28, &[204]),
    SCell::Clo(29, &[204]),
    SCell::Obj(&[(1, 278), (2, 279)]),
    SCell::Obj(&[(1, 280), (2, 281)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_1: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_1: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 204),
    (152, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (157, 209),
    (160, 94),
    (161, 116),
    (162, 118),
    (163, 119),
    (164, 121),
    (165, 122),
    (166, 123),
    (167, 124),
    (168, 125),
    (169, 127),
    (170, 210),
    (172, 211),
    (173, 212),
    (175, 213),
    (176, 214),
    (178, 215),
    (180, 216),
    (182, 217),
    (184, 218),
    (186, 219),
    (189, 220),
    (192, 221),
    (194, 222),
    (196, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (202, 228),
    (203, 229),
    (230, 255),
    (231, 256),
    (233, 257),
    (235, 258),
    (238, 259),
    (240, 260),
    (241, 261),
    (242, 262),
    (243, 263),
    (244, 264),
    (245, 265),
    (248, 266),
    (250, 267),
    (252, 93),
];

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c39: ZN,
    pub c42: ZB,
    pub c88: ZN,
    pub c254: ZN,
    pub c43: ZB,
    pub c38: ZB,
}

/// Outcome 1's per-assignment values and lane masks.
/// The cells of outcome 1 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared1`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut1 {
    pub c20: ZN,
    pub c41: ZB,
    pub c268: ZN,
    pub c269: ZN,
    pub c234: ZN,
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c237: ZN,
    pub c272: ZB,
    pub c239: ZN,
    pub c246: ZB,
    pub c247: ZB,
    pub c280: ZN,
    pub c281: ZN,
    pub c253: ZN,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 2 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_2: &[(u32, &str)] = &[
    (177, "balloon.tile"),
    (204, "big_chest.tile"),
    (196, "chest.if_not_fruit"),
    (198, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (190, "fake_wall.if_not_fruit"),
    (191, "fake_wall.tile"),
    (180, "fall_floor.tile"),
    (186, "fly_fruit.if_not_fruit"),
    (188, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (182, "fruit.if_not_fruit"),
    (184, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (193, "key.if_not_fruit"),
    (194, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (238, "objects[0].collideable"),
    (358, "objects[0].flip.x"),
    (359, "objects[0].flip.y"),
    (360, "objects[0].hitbox.h"),
    (361, "objects[0].hitbox.w"),
    (362, "objects[0].hitbox.x"),
    (363, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (364, "objects[0].rem.x"),
    (365, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (366, "objects[0].spd.x"),
    (367, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (258, "objects[1].delay"),
    (368, "objects[1].flip.x"),
    (369, "objects[1].flip.y"),
    (370, "objects[1].hitbox.h"),
    (371, "objects[1].hitbox.w"),
    (372, "objects[1].hitbox.x"),
    (373, "objects[1].hitbox.y"),
    (374, "objects[1].rem.x"),
    (375, "objects[1].rem.y"),
    (267, "objects[1].solids"),
    (376, "objects[1].spd.x"),
    (377, "objects[1].spd.y"),
    (269, "objects[1].spr"),
    (270, "objects[1].state"),
    (378, "objects[1].target.x"),
    (379, "objects[1].target.y"),
    (159, "objects[1].type.tile"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (277, "objects[2].collideable"),
    (380, "objects[2].flip.x"),
    (381, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (382, "objects[2].hitbox.h"),
    (383, "objects[2].hitbox.w"),
    (384, "objects[2].hitbox.x"),
    (385, "objects[2].hitbox.y"),
    (386, "objects[2].rem.x"),
    (387, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (388, "objects[2].spd.x"),
    (389, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (174, "objects[2].type.tile"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (296, "objects[3].collideable"),
    (390, "objects[3].flip.x"),
    (391, "objects[3].flip.y"),
    (298, "objects[3].hide_for"),
    (299, "objects[3].hide_in"),
    (392, "objects[3].hitbox.h"),
    (393, "objects[3].hitbox.w"),
    (394, "objects[3].hitbox.x"),
    (395, "objects[3].hitbox.y"),
    (396, "objects[3].rem.x"),
    (397, "objects[3].rem.y"),
    (307, "objects[3].solids"),
    (398, "objects[3].spd.x"),
    (399, "objects[3].spd.y"),
    (309, "objects[3].spr"),
    (311, "objects[3].x"),
    (312, "objects[3].y"),
    (43, "pause_player"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_2: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_2: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151, 152, 153, 154]),
    SCell::Obj(&[(7, 155), (5, 156), (6, 157)]),
    SCell::Obj(&[(5, 158), (8, 159), (6, 160)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 161), (2, 162)]),
    SCell::Arr(&[163, 164, 165, 166, 167, 168, 169, 170, 171, 172]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 182), (5, 183), (8, 184), (6, 185)]),
    SCell::Obj(&[(9, 186), (5, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (5, 197), (8, 198), (6, 199)]),
    SCell::Obj(&[(5, 200), (6, 201)]),
    SCell::Obj(&[(7, 202), (5, 203), (8, 204)]),
    SCell::Obj(&[(7, 205), (5, 206)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 236), (20, 237), (11, 238), (14, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (41, 246), (4, 247), (12, 248), (3, 249), (13, 250), (38, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (35, 258), (14, 259), (15, 260), (19, 261), (18, 262), (22, 263), (23, 264), (24, 265), (4, 266), (12, 267), (3, 268), (13, 269), (25, 270), (34, 271), (0, 272), (1, 273), (2, 274)]),
    SCell::Obj(&[(21, 275), (20, 276), (11, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
    SCell::Obj(&[(21, 294), (20, 295), (11, 296), (14, 297), (45, 298), (42, 299), (15, 300), (19, 301), (18, 302), (22, 303), (23, 304), (24, 305), (4, 306), (12, 307), (3, 308), (13, 309), (0, 310), (1, 311), (2, 312)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(17, 360), (16, 361), (1, 362), (2, 363)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 364), (2, 365)]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Obj(&[(17, 370), (16, 371), (1, 372), (2, 373)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 374), (2, 375)]),
    SCell::Obj(&[(1, 376), (2, 377)]),
    SCell::Obj(&[(1, 378), (2, 379)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 380), (2, 381)]),
    SCell::Obj(&[(17, 382), (16, 383), (1, 384), (2, 385)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 386), (2, 387)]),
    SCell::Obj(&[(1, 388), (2, 389)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 390), (2, 391)]),
    SCell::Obj(&[(17, 392), (16, 393), (1, 394), (2, 395)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 396), (2, 397)]),
    SCell::Obj(&[(1, 398), (2, 399)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_2: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_2: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 207),
    (152, 208),
    (153, 209),
    (154, 210),
    (155, 211),
    (156, 212),
    (157, 213),
    (158, 214),
    (160, 215),
    (163, 94),
    (164, 116),
    (165, 118),
    (166, 119),
    (167, 121),
    (168, 122),
    (169, 123),
    (170, 124),
    (171, 125),
    (172, 127),
    (173, 216),
    (175, 217),
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (183, 222),
    (185, 223),
    (187, 224),
    (189, 225),
    (192, 226),
    (195, 227),
    (197, 228),
    (199, 229),
    (200, 230),
    (201, 231),
    (202, 232),
    (203, 233),
    (205, 234),
    (206, 235),
    (236, 313),
    (237, 314),
    (239, 315),
    (240, 316),
    (241, 317),
    (242, 318),
    (243, 319),
    (244, 320),
    (245, 321),
    (247, 322),
    (249, 323),
    (252, 121),
    (255, 324),
    (256, 325),
    (259, 326),
    (260, 327),
    (261, 328),
    (262, 329),
    (263, 330),
    (264, 331),
    (265, 332),
    (266, 333),
    (268, 334),
    (271, 335),
    (272, 94),
    (275, 336),
    (276, 337),
    (278, 338),
    (281, 339),
    (282, 340),
    (283, 341),
    (284, 342),
    (285, 343),
    (286, 344),
    (287, 345),
    (289, 346),
    (291, 116),
    (294, 347),
    (295, 348),
    (297, 349),
    (300, 350),
    (301, 351),
    (302, 352),
    (303, 353),
    (304, 354),
    (305, 355),
    (306, 356),
    (308, 357),
    (310, 116),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c88: ZN,
    pub c43: ZB,
}

/// Outcome 2's per-assignment values and lane masks.
/// The cells of outcome 2 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared2`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut2 {
    pub c39: ZN,
    pub c20: ZN,
    pub c38: ZB,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 3 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_3: &[(u32, &str)] = &[
    (174, "balloon.tile"),
    (201, "big_chest.tile"),
    (193, "chest.if_not_fruit"),
    (195, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (187, "fake_wall.if_not_fruit"),
    (188, "fake_wall.tile"),
    (177, "fall_floor.tile"),
    (183, "fly_fruit.if_not_fruit"),
    (185, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (179, "fruit.if_not_fruit"),
    (181, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (190, "key.if_not_fruit"),
    (191, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (232, "objects[0].collideable"),
    (233, "objects[0].delay"),
    (262, "objects[0].flip.x"),
    (263, "objects[0].flip.y"),
    (264, "objects[0].hitbox.h"),
    (265, "objects[0].hitbox.w"),
    (266, "objects[0].hitbox.x"),
    (267, "objects[0].hitbox.y"),
    (268, "objects[0].rem.x"),
    (269, "objects[0].rem.y"),
    (242, "objects[0].solids"),
    (270, "objects[0].spd.x"),
    (271, "objects[0].spd.y"),
    (244, "objects[0].spr"),
    (245, "objects[0].state"),
    (272, "objects[0].target.x"),
    (273, "objects[0].target.y"),
    (156, "objects[0].type.tile"),
    (248, "objects[0].x"),
    (249, "objects[0].y"),
    (43, "pause_player"),
    (158, "room.x"),
    (159, "room.y"),
    (85, "seconds"),
    (171, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_3: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_3: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151]),
    SCell::Obj(&[(7, 152), (5, 153), (6, 154)]),
    SCell::Obj(&[(5, 155), (8, 156), (6, 157)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 158), (2, 159)]),
    SCell::Arr(&[160, 161, 162, 163, 164, 165, 166, 167, 168, 169]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 170), (8, 171), (6, 172)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 179), (5, 180), (8, 181), (6, 182)]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (5, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(5, 197), (6, 198)]),
    SCell::Obj(&[(7, 199), (5, 200), (8, 201)]),
    SCell::Obj(&[(7, 202), (5, 203)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 230), (20, 231), (11, 232), (35, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (4, 241), (12, 242), (3, 243), (13, 244), (25, 245), (34, 246), (0, 247), (1, 248), (2, 249)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[204]),
    SCell::Clo(25, &[204]),
    SCell::Obj(&[(1, 262), (2, 263)]),
    SCell::Obj(&[(17, 264), (16, 265), (1, 266), (2, 267)]),
    SCell::Clo(24, &[204]),
    SCell::Clo(23, &[204]),
    SCell::Clo(27, &[204]),
    SCell::Clo(28, &[204]),
    SCell::Clo(29, &[204]),
    SCell::Obj(&[(1, 268), (2, 269)]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_3: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_3: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 204),
    (152, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (157, 209),
    (160, 94),
    (161, 116),
    (162, 118),
    (163, 119),
    (164, 121),
    (165, 122),
    (166, 123),
    (167, 124),
    (168, 125),
    (169, 127),
    (170, 210),
    (172, 211),
    (173, 212),
    (175, 213),
    (176, 214),
    (178, 215),
    (180, 216),
    (182, 217),
    (184, 218),
    (186, 219),
    (189, 220),
    (192, 221),
    (194, 222),
    (196, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (202, 228),
    (203, 229),
    (230, 250),
    (231, 251),
    (234, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (243, 260),
    (246, 261),
    (247, 94),
];

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c39: ZN,
    pub c20: ZN,
    pub c41: ZB,
    pub c42: ZB,
    pub c88: ZN,
    pub c232: ZB,
    pub c233: ZN,
    pub c262: ZB,
    pub c263: ZB,
    pub c264: ZN,
    pub c265: ZN,
    pub c266: ZN,
    pub c267: ZN,
    pub c268: ZN,
    pub c269: ZN,
    pub c242: ZB,
    pub c270: ZN,
    pub c271: ZN,
    pub c244: ZN,
    pub c245: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c43: ZB,
    pub c38: ZB,
}

/// Outcome 3's per-assignment values and lane masks.
/// The cells of outcome 3 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared3`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut3 {
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

/// An EMPTY accumulator with outcome 0's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append0`.
pub fn acc0(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_0, OUT_GLOBALS_0, OUT_PTRS_0, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::V(Vec::new());
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[43] = Col::V(Vec::new());
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_0 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 0 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[42] {
            v.push(if sh.c42.known & (1 << i) != 0 {
                AV::Bool(sh.c42.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::V(Vec::new());
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[232] = Col::U(AV::Bool(true));
    b.cols[268] = Col::N(Vec::new());
    b.cols[269] = Col::N(Vec::new());
    b.cols[234] = Col::N(Vec::new());
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[237] = Col::N(Vec::new());
    b.cols[272] = Col::V(Vec::new());
    b.cols[273] = Col::U(AV::Bool(false));
    b.cols[239] = Col::N(Vec::new());
    b.cols[274] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[246] = Col::V(Vec::new());
    b.cols[247] = Col::V(Vec::new());
    b.cols[278] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[279] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[249] = Col::U(AV::Bool(true));
    b.cols[280] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[254] = Col::N(Vec::new());
    b.cols[43] = Col::V(Vec::new());
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_1 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 1 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[42] {
            v.push(if sh.c42.known & (1 << i) != 0 {
                AV::Bool(sh.c42.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(kv.c268.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(kv.c269.lane(i)); }
        if let Col::N(v) = &mut acc.cols[234] { v.push(kv.c234.lane(i)); }
        if let Col::N(v) = &mut acc.cols[270] { v.push(kv.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(kv.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[236] { v.push(kv.c236.lane(i)); }
        if let Col::N(v) = &mut acc.cols[237] { v.push(kv.c237.lane(i)); }
        if let Col::V(v) = &mut acc.cols[272] {
            v.push(if kv.c272.known & (1 << i) != 0 {
                AV::Bool(kv.c272.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[239] { v.push(kv.c239.lane(i)); }
        if let Col::V(v) = &mut acc.cols[246] {
            v.push(if kv.c246.known & (1 << i) != 0 {
                AV::Bool(kv.c246.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[247] {
            v.push(if kv.c247.known & (1 << i) != 0 {
                AV::Bool(kv.c247.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[280] { v.push(kv.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(kv.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(kv.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[182] = Col::U(AV::Bool(true));
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[358] = Col::U(AV::Bool(false));
    b.cols[359] = Col::U(AV::Bool(false));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[362] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Ival(P8::from_raw(0i32), P8::from_raw(2555904i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Ival(P8::from_raw(2981888i32), P8::from_raw(3309568i32)));
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[258] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[368] = Col::U(AV::Bool(false));
    b.cols[369] = Col::U(AV::Bool(false));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[375] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Bool(false));
    b.cols[376] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[377] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[277] = Col::U(AV::Bool(true));
    b.cols[380] = Col::U(AV::Bool(false));
    b.cols[381] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[385] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[390] = Col::U(AV::Bool(false));
    b.cols[391] = Col::U(AV::Bool(false));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[395] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[396] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[397] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Bool(true));
    b.cols[398] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[399] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::V(Vec::new());
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_2 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 2 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if kv.c38.known & (1 << i) != 0 {
                AV::Bool(kv.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::V(Vec::new());
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[232] = Col::V(Vec::new());
    b.cols[233] = Col::N(Vec::new());
    b.cols[262] = Col::V(Vec::new());
    b.cols[263] = Col::V(Vec::new());
    b.cols[264] = Col::N(Vec::new());
    b.cols[265] = Col::N(Vec::new());
    b.cols[266] = Col::N(Vec::new());
    b.cols[267] = Col::N(Vec::new());
    b.cols[268] = Col::N(Vec::new());
    b.cols[269] = Col::N(Vec::new());
    b.cols[242] = Col::V(Vec::new());
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[244] = Col::N(Vec::new());
    b.cols[245] = Col::N(Vec::new());
    b.cols[272] = Col::N(Vec::new());
    b.cols[273] = Col::N(Vec::new());
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[248] = Col::N(Vec::new());
    b.cols[249] = Col::N(Vec::new());
    b.cols[43] = Col::V(Vec::new());
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_3 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 3 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
    n: usize, seen: &mut RowSet,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if sh.c41.known & (1 << i) != 0 {
                AV::Bool(sh.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[42] {
            v.push(if sh.c42.known & (1 << i) != 0 {
                AV::Bool(sh.c42.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::V(v) = &mut acc.cols[232] {
            v.push(if sh.c232.known & (1 << i) != 0 {
                AV::Bool(sh.c232.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[233] { v.push(sh.c233.lane(i)); }
        if let Col::V(v) = &mut acc.cols[262] {
            v.push(if sh.c262.known & (1 << i) != 0 {
                AV::Bool(sh.c262.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[263] {
            v.push(if sh.c263.known & (1 << i) != 0 {
                AV::Bool(sh.c263.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[264] { v.push(sh.c264.lane(i)); }
        if let Col::N(v) = &mut acc.cols[265] { v.push(sh.c265.lane(i)); }
        if let Col::N(v) = &mut acc.cols[266] { v.push(sh.c266.lane(i)); }
        if let Col::N(v) = &mut acc.cols[267] { v.push(sh.c267.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(sh.c268.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(sh.c269.lane(i)); }
        if let Col::V(v) = &mut acc.cols[242] {
            v.push(if sh.c242.known & (1 << i) != 0 {
                AV::Bool(sh.c242.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[270] { v.push(sh.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(sh.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[244] { v.push(sh.c244.lane(i)); }
        if let Col::N(v) = &mut acc.cols[245] { v.push(sh.c245.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(sh.c272.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[248] { v.push(sh.c248.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 4;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        _ => panic!("outcome {} of 4", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        _ => panic!("outcome {} of 4", i),
    }
}

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0]);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1]);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2]);
    }
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3]);
    }
}

pub fn step(
    b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],
) -> Option<u16> {
    let (u, s) = bind(b)?;
    let rin = rows(b, &s, lo)?;
    let g = G { cart: &b.cart, cache: &b.cache };
    // The row sets are the CALLER's and reset in O(1) here.
    // Slice-local because that is where the duplication is:
    // it comes from configurations agreeing on one lane, and
    // a lane lives in one slice.
    seen.iter_mut().for_each(|s| s.next_slice());
    let mut sink = Append { accs, seen, n };
    Some(frame(&u, &rin, &g, &mut sink))
}

/// Where a frame's rows go. One call per (outcome, GROUP), not
/// per outcome per variant: variants that write identical
/// values are one call whose `take` is the union of their
/// lanes. `mask` is the group's REPRESENTATIVE assignment -
/// every member computes the same values, so any of them
/// identifies the row for a caller that wants to check it
/// against the graph.
pub trait Sink {
    fn o0(&mut self, mask: u8, take: u16, sh: &KShared0, v: &KOut0);
    fn o1(&mut self, mask: u8, take: u16, sh: &KShared1, v: &KOut1);
    fn o2(&mut self, mask: u8, take: u16, sh: &KShared2, v: &KOut2);
    fn o3(&mut self, mask: u8, take: u16, sh: &KShared3, v: &KOut3);
}

/// Run one frame over 16 lanes. Returns the lanes DECLINED -
/// live but not `ok` - which the never-deopt doctrine turns
/// into a stopped run.
#[inline(never)]
pub fn frame(u: &Uni, rin: &RowsIn, g: &G, sink: &mut dyn Sink) -> u16 {
    let mut declined: u16 = 0;
    let r_c20: ZN = rin.c20;
    let r_c38: ZB = ZB { val: rin.c38, known: ALL };
    let r_c39: ZN = rin.c39;
    let r_c41: ZB = ZB { val: rin.c41, known: ALL };
    let r_c42: ZB = ZB { val: rin.c42, known: ALL };
    let r_c43: ZB = ZB { val: rin.c43, known: ALL };
    let r_c84: ZN = rin.c84;
    let r_c85: ZN = rin.c85;
    let r_c88: ZN = rin.c88;
    let r_c232: ZB = ZB { val: rin.c232, known: ALL };
    let r_c233: ZN = rin.c233;
    let r_c242: ZB = ZB { val: rin.c242, known: ALL };
    let r_c244: ZN = rin.c244;
    let r_c245: ZN = rin.c245;
    let r_c248: ZN = rin.c248;
    let r_c249: ZN = rin.c249;
    let r_c262: ZB = ZB { val: rin.c262, known: ALL };
    let r_c263: ZB = ZB { val: rin.c263, known: ALL };
    let r_c268: ZN = rin.c268;
    let r_c269: ZN = rin.c269;
    let r_c270: ZN = rin.c270;
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let n56: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n57: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n58: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n59: ZB = zb_and(r_c38, n57);
    let n60: ZB = zn_gt(r_c39, zn_splat(P8::from_raw(0i32)));
    let n61: ZB = zb_and(n59, n60);
    let n62: ZB = zb_and(n57, n61);
    let n63: ZN = zn_sub(r_c39, zn_splat(P8::from_raw(65536i32)));
    let n64: ZB = zn_le(n63, zn_splat(P8::from_raw(0i32)));
    let n65: ZB = zn_gt(n63, zn_splat(P8::from_raw(0i32)));
    let n66: ZB = zb_and(n62, n64);
    let n67: ZB = zb_and(n62, n65);
    let n68: ZB = zb_not(n66);
    let n69: ZB = zb_and(r_c41, n68);
    let n70: ZB = zb_and(r_c42, n68);
    let n71: ZB = zb_and(r_c38, n68);
    let n72: ZB = zb_or(r_c232, n66);
    let n73: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), r_c233);
    let n74: ZB = zb_and(r_c242, n68);
    let n75: ZN = zsel_n(n66, zn_splat(P8::from_raw(196608i32)), r_c244);
    let n76: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), r_c245);
    let n77: ZN = zsel_n(n66, zn_splat(P8::from_raw(524288i32)), r_c248);
    let n78: ZN = zsel_n(n66, zn_splat(P8::from_raw(8388608i32)), r_c249);
    let n79: ZB = zb_and(r_c262, n68);
    let n80: ZB = zb_and(r_c263, n68);
    let n81: ZN = zsel_n(n66, zn_splat(P8::from_raw(524288i32)), zn_splat(u.c264));
    let n82: ZN = zsel_n(n66, zn_splat(P8::from_raw(524288i32)), zn_splat(u.c265));
    let n83: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), zn_splat(u.c266));
    let n84: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), zn_splat(u.c267));
    let n85: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), r_c268);
    let n86: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), r_c269);
    let n87: ZN = zsel_n(n66, zn_splat(P8::from_raw(0i32)), r_c270);
    let n88: ZN = zsel_n(n66, zn_splat(P8::from_raw(-262144i32)), r_c271);
    let n89: ZN = zsel_n(n66, zn_splat(P8::from_raw(524288i32)), r_c272);
    let n90: ZN = zsel_n(n66, zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n91: ZB = zb_or(n66, n67);
    let n92: ZN = zsel_n(n91, n63, r_c39);
    let n93: ZB = zsel_b(n91, n69, r_c41);
    let n94: ZB = zsel_b(n91, n70, r_c42);
    let n95: ZB = zsel_b(n91, n71, r_c38);
    let n96: ZB = zsel_b(n91, n72, r_c232);
    let n97: ZN = zsel_n(n91, n73, r_c233);
    let n98: ZB = zsel_b(n91, n74, r_c242);
    let n99: ZN = zsel_n(n91, n75, r_c244);
    let n100: ZN = zsel_n(n91, n76, r_c245);
    let n101: ZN = zsel_n(n91, n77, r_c248);
    let n102: ZN = zsel_n(n91, n78, r_c249);
    let n103: ZB = zsel_b(n91, n79, r_c262);
    let n104: ZB = zsel_b(n91, n80, r_c263);
    let n105: ZN = zsel_n(n91, n81, zn_splat(u.c264));
    let n106: ZN = zsel_n(n91, n82, zn_splat(u.c265));
    let n107: ZN = zsel_n(n91, n83, zn_splat(u.c266));
    let n108: ZN = zsel_n(n91, n84, zn_splat(u.c267));
    let n109: ZN = zsel_n(n91, n85, r_c268);
    let n110: ZN = zsel_n(n91, n86, r_c269);
    let n111: ZN = zsel_n(n91, n87, r_c270);
    let n112: ZN = zsel_n(n91, n88, r_c271);
    let n113: ZN = zsel_n(n91, n89, r_c272);
    let n114: ZN = zsel_n(n91, n90, r_c273);
    let n115: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n111);
    let n116: ZB = zb_and(n57, n115);
    let n117: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n112);
    let n118: ZB = zb_not(n117);
    let n119: ZB = zb_not(n116);
    let n120: ZB = zb_or(n118, n119);
    let n121: ZB = zb_and(n57, n120);
    let n122: ZN = zn_add(n109, n111);
    let n124: ZN = zn_add(n122, zn_splat(P8::from_raw(32768i32)));
    let n125: ZN = zn_flr(n124);
    let n126: ZN = zn_sub(n124, zn_splat(P8::from_raw(32768i32)));
    let n127: ZN = zn_sub(n126, n125);
    let n128: ZB = zb_not(n98);
    let n129: ZB = zb_and(n98, n121);
    let n130: ZB = zb_and(n121, n128);
    let n131: ZB = zn_gt(n125, zn_splat(P8::from_raw(0i32)));
    let n132: ZB = zn_le(n125, zn_splat(P8::from_raw(0i32)));
    let n133: ZB = zb_and(n129, n131);
    let n134: ZB = zb_and(n129, n132);
    let n135: ZB = zn_lt(n125, zn_splat(P8::from_raw(0i32)));
    let n136: ZB = zn_ge(n125, zn_splat(P8::from_raw(0i32)));
    let n137: ZB = zb_and(n134, n135);
    let n138: ZB = zb_and(n134, n136);
    let n140: ZN = zsel_n(n133, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(-65536i32)));
    let n141: ZB = zb_or(n133, n137);
    let n142: ZN = zsel_n(n138, zn_splat(P8::from_raw(0i32)), n140);
    let n143: ZB = zb_or(n138, n141);
    let n144: ZN = zn_abs(n125);
    let n145: ZN = zn_add(n101, n107);
    let n146: ZN = zn_add(n142, n145);
    let n147: ZN = zn_add(n102, n108);
    let n148: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n147);
    let n149: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n146, n148, n106, n105, P8::from_raw(0i32));
    let n150: ZB = zb_not(n149);
    let n151: ZB = zb_and(n143, n150);
    let n152: ZB = zb_and(n143, n149);
    let n153: ZB = zb_or(n151, n152);
    let n154: ZB = zb_not(n151);
    let n155: ZB = zb_and(n151, n153);
    let n156: ZB = zb_and(n153, n154);
    let n157: ZB = zb_or(n155, n156);
    let n158: ZB = zb_not(n155);
    let n159: ZB = zb_and(n155, n157);
    let n160: ZB = zb_and(n157, n158);
    let n161: ZN = zn_add(n101, n142);
    let n162: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n144);
    let n163: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n144);
    let n164: ZB = zb_and(n159, n162);
    let n165: ZB = zb_and(n159, n163);
    let n166: ZN = zn_add(n107, n161);
    let n167: ZN = zn_add(n142, n166);
    let n168: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n167, n148, n106, n105, P8::from_raw(0i32));
    let n169: ZB = zb_not(n168);
    let n170: ZB = zb_and(n164, n169);
    let n171: ZB = zb_and(n164, n168);
    let n172: ZB = zb_or(n170, n171);
    let n173: ZB = zb_not(n170);
    let n174: ZB = zb_and(n170, n172);
    let n175: ZB = zb_and(n172, n173);
    let n176: ZB = zb_or(n174, n175);
    let n177: ZB = zb_not(n174);
    let n178: ZB = zb_and(n174, n176);
    let n179: ZB = zb_and(n176, n177);
    let n180: ZN = zn_add(n142, n161);
    let n181: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n144);
    let n182: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n144);
    let n183: ZB = zb_and(n178, n181);
    let n184: ZB = zb_and(n178, n182);
    let n185: ZN = zn_add(n107, n180);
    let n186: ZN = zn_add(n142, n185);
    let n187: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n186, n148, n106, n105, P8::from_raw(0i32));
    let n188: ZB = zb_not(n187);
    let n189: ZB = zb_and(n183, n188);
    let n190: ZB = zb_and(n183, n187);
    let n191: ZB = zb_or(n189, n190);
    let n192: ZB = zb_not(n189);
    let n193: ZB = zb_and(n189, n191);
    let n194: ZB = zb_and(n191, n192);
    let n195: ZB = zb_or(n193, n194);
    let n196: ZB = zb_not(n193);
    let n197: ZB = zb_and(n193, n195);
    let n198: ZB = zb_and(n195, n196);
    let n199: ZN = zn_add(n142, n180);
    let n200: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n144);
    let n201: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n144);
    let n202: ZB = zb_and(n197, n200);
    let n203: ZB = zb_and(n197, n201);
    let n204: ZN = zn_add(n107, n199);
    let n205: ZN = zn_add(n142, n204);
    let n206: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n205, n148, n106, n105, P8::from_raw(0i32));
    let n207: ZB = zb_not(n206);
    let n208: ZB = zb_and(n202, n207);
    let n209: ZB = zb_and(n202, n206);
    let n210: ZB = zb_or(n208, n209);
    let n211: ZB = zb_not(n208);
    let n212: ZB = zb_and(n208, n210);
    let n213: ZB = zb_and(n210, n211);
    let n214: ZB = zb_or(n212, n213);
    let n215: ZB = zb_not(n212);
    let n216: ZB = zb_and(n212, n214);
    let n217: ZB = zb_and(n214, n215);
    let n218: ZN = zn_add(n142, n199);
    let n219: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n144);
    let n220: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n144);
    let n221: ZB = zb_and(n216, n219);
    let n222: ZB = zb_and(n216, n220);
    let n223: ZN = zn_add(n107, n218);
    let n224: ZN = zn_add(n142, n223);
    let n225: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n224, n148, n106, n105, P8::from_raw(0i32));
    let n226: ZB = zb_not(n225);
    let n227: ZB = zb_and(n221, n226);
    let n228: ZB = zb_and(n221, n225);
    let n229: ZB = zb_or(n227, n228);
    let n230: ZB = zb_not(n227);
    let n231: ZB = zb_and(n227, n229);
    let n232: ZB = zb_and(n229, n230);
    let n233: ZB = zb_or(n231, n232);
    let n234: ZB = zb_not(n231);
    let n235: ZB = zb_and(n231, n233);
    let n236: ZB = zb_and(n233, n234);
    let n237: ZN = zn_add(n142, n218);
    let n238: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n144);
    let n239: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n144);
    let n240: ZB = zb_and(n235, n238);
    let n241: ZB = zb_and(n235, n239);
    let n242: ZN = zn_add(n107, n237);
    let n243: ZN = zn_add(n142, n242);
    let n244: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n243, n148, n106, n105, P8::from_raw(0i32));
    let n245: ZB = zb_not(n244);
    let n246: ZB = zb_and(n240, n245);
    let n247: ZB = zb_and(n240, n244);
    let n248: ZB = zb_or(n246, n247);
    let n249: ZB = zb_not(n246);
    let n250: ZB = zb_and(n246, n248);
    let n251: ZB = zb_and(n248, n249);
    let n252: ZB = zb_or(n250, n251);
    let n253: ZB = zb_not(n250);
    let n254: ZB = zb_and(n250, n252);
    let n255: ZB = zb_and(n252, n253);
    let n256: ZN = zn_add(n142, n237);
    let n257: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n144);
    let n258: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n144);
    let n259: ZB = zb_and(n254, n257);
    let n260: ZB = zb_and(n254, n258);
    let n261: ZN = zn_add(n107, n256);
    let n262: ZN = zn_add(n142, n261);
    let n263: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n262, n148, n106, n105, P8::from_raw(0i32));
    let n264: ZB = zb_not(n263);
    let n265: ZB = zb_and(n259, n264);
    let n266: ZB = zb_and(n259, n263);
    let n267: ZB = zb_or(n265, n266);
    let n268: ZB = zb_not(n265);
    let n269: ZB = zb_and(n265, n267);
    let n270: ZB = zb_and(n267, n268);
    let n271: ZB = zb_or(n269, n270);
    let n272: ZB = zb_not(n269);
    let n273: ZB = zb_and(n269, n271);
    let n274: ZB = zb_and(n271, n272);
    let n275: ZN = zn_add(n142, n256);
    let n276: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n144);
    let n277: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n144);
    let n278: ZB = zb_and(n273, n276);
    let n279: ZB = zb_and(n273, n277);
    let n280: ZN = zn_add(n107, n275);
    let n281: ZN = zn_add(n142, n280);
    let n282: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n281, n148, n106, n105, P8::from_raw(0i32));
    let n283: ZB = zb_not(n282);
    let n284: ZB = zb_and(n278, n283);
    let n285: ZB = zb_and(n278, n282);
    let n286: ZB = zb_or(n284, n285);
    let n287: ZB = zb_not(n284);
    let n288: ZB = zb_and(n284, n286);
    let n289: ZB = zb_and(n286, n287);
    let n290: ZB = zb_or(n288, n289);
    let n291: ZB = zb_not(n288);
    let n292: ZB = zb_and(n288, n290);
    let n293: ZB = zb_and(n290, n291);
    let n294: ZN = zn_add(n142, n275);
    let n295: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n144);
    let n296: ZN = zsel_n(n165, n127, zn_splat(P8::from_raw(0i32)));
    let n297: ZN = zsel_n(n165, n111, zn_splat(P8::from_raw(0i32)));
    let n298: ZB = zb_or(n165, n179);
    let n299: ZN = zsel_n(n184, n127, zn_splat(P8::from_raw(0i32)));
    let n300: ZN = zsel_n(n184, n111, zn_splat(P8::from_raw(0i32)));
    let n301: ZB = zb_or(n184, n198);
    let n302: ZN = zsel_n(n203, n127, zn_splat(P8::from_raw(0i32)));
    let n303: ZN = zsel_n(n203, n111, zn_splat(P8::from_raw(0i32)));
    let n304: ZB = zb_or(n203, n217);
    let n305: ZN = zsel_n(n222, n127, zn_splat(P8::from_raw(0i32)));
    let n306: ZN = zsel_n(n222, n111, zn_splat(P8::from_raw(0i32)));
    let n307: ZB = zb_or(n222, n236);
    let n308: ZN = zsel_n(n241, n127, zn_splat(P8::from_raw(0i32)));
    let n309: ZN = zsel_n(n241, n111, zn_splat(P8::from_raw(0i32)));
    let n310: ZB = zb_or(n241, n255);
    let n311: ZN = zsel_n(n260, n127, zn_splat(P8::from_raw(0i32)));
    let n312: ZN = zsel_n(n260, n111, zn_splat(P8::from_raw(0i32)));
    let n313: ZB = zb_or(n260, n274);
    let n314: ZN = zsel_n(n279, n127, zn_splat(P8::from_raw(0i32)));
    let n315: ZN = zsel_n(n279, n111, zn_splat(P8::from_raw(0i32)));
    let n316: ZB = zb_or(n279, n293);
    let n317: ZN = zsel_n(n292, n294, n101);
    let n318: ZN = zsel_n(n292, n127, zn_splat(P8::from_raw(0i32)));
    let n319: ZN = zsel_n(n292, n111, zn_splat(P8::from_raw(0i32)));
    let n320: ZB = zb_or(n160, n292);
    let n321: ZB = zb_not(n292);
    let n322: ZB = zb_or(n295, n321);
    let n323: ZN = zsel_n(n298, n161, n180);
    let n324: ZN = zsel_n(n298, n296, n299);
    let n325: ZN = zsel_n(n298, n297, n300);
    let n326: ZB = zb_or(n298, n301);
    let n327: ZN = zsel_n(n304, n199, n218);
    let n328: ZN = zsel_n(n304, n302, n305);
    let n329: ZN = zsel_n(n304, n303, n306);
    let n330: ZB = zb_or(n304, n307);
    let n331: ZN = zsel_n(n310, n237, n256);
    let n332: ZN = zsel_n(n310, n308, n311);
    let n333: ZN = zsel_n(n310, n309, n312);
    let n334: ZB = zb_or(n310, n313);
    let n335: ZN = zsel_n(n316, n275, n317);
    let n336: ZN = zsel_n(n316, n314, n318);
    let n337: ZN = zsel_n(n316, n315, n319);
    let n338: ZB = zb_or(n316, n320);
    let n339: ZB = zb_or(n316, n322);
    let n340: ZN = zsel_n(n326, n323, n327);
    let n341: ZN = zsel_n(n326, n324, n328);
    let n342: ZN = zsel_n(n326, n325, n329);
    let n343: ZB = zb_or(n326, n330);
    let n344: ZN = zsel_n(n334, n331, n335);
    let n345: ZN = zsel_n(n334, n332, n336);
    let n346: ZN = zsel_n(n334, n333, n337);
    let n347: ZB = zb_or(n334, n338);
    let n348: ZB = zb_or(n334, n339);
    let n349: ZN = zsel_n(n343, n340, n344);
    let n350: ZN = zsel_n(n343, n341, n345);
    let n351: ZN = zsel_n(n343, n342, n346);
    let n352: ZB = zb_or(n343, n347);
    let n353: ZB = zb_or(n343, n348);
    let n354: ZN = zn_add(n101, n125);
    let n355: ZN = zsel_n(n352, n349, n354);
    let n356: ZN = zsel_n(n352, n350, n127);
    let n357: ZN = zsel_n(n352, n351, n111);
    let n358: ZB = zb_or(n130, n352);
    let n359: ZB = zb_not(n352);
    let n360: ZB = zb_or(n353, n359);
    let n361: ZN = zn_add(n110, n112);
    let n362: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n361);
    let n363: ZN = zn_flr(n362);
    let n364: ZN = zn_sub(n362, zn_splat(P8::from_raw(32768i32)));
    let n365: ZN = zn_sub(n364, n363);
    let n366: ZB = zb_and(n98, n358);
    let n367: ZB = zb_and(n128, n358);
    let n368: ZB = zn_gt(n363, zn_splat(P8::from_raw(0i32)));
    let n369: ZB = zn_le(n363, zn_splat(P8::from_raw(0i32)));
    let n370: ZB = zb_and(n366, n368);
    let n371: ZB = zb_and(n366, n369);
    let n372: ZB = zn_lt(n363, zn_splat(P8::from_raw(0i32)));
    let n373: ZB = zn_ge(n363, zn_splat(P8::from_raw(0i32)));
    let n374: ZB = zb_and(n371, n372);
    let n375: ZB = zb_and(n371, n373);
    let n376: ZN = zsel_n(n370, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(-65536i32)));
    let n377: ZB = zb_or(n370, n374);
    let n378: ZN = zsel_n(n375, zn_splat(P8::from_raw(0i32)), n376);
    let n379: ZB = zb_or(n375, n377);
    let n380: ZN = zn_abs(n363);
    let n381: ZB = zn_gt(n378, zn_splat(P8::from_raw(0i32)));
    let n382: ZB = zn_le(n378, zn_splat(P8::from_raw(0i32)));
    let n383: ZB = zb_and(n379, n381);
    let n384: ZB = zb_and(n379, n382);
    let n385: ZB = zb_or(n383, n384);
    let n386: ZB = zb_not(n383);
    let n387: ZB = zb_and(n383, n385);
    let n388: ZB = zb_and(n385, n386);
    let n389: ZB = zb_or(n387, n388);
    let n390: ZN = zn_add(n107, n355);
    let n391: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n390);
    let n392: ZN = zn_add(n147, n378);
    let n393: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n392, n106, n105, P8::from_raw(0i32));
    let n394: ZB = zb_not(n393);
    let n395: ZB = zb_and(n389, n394);
    let n396: ZB = zb_and(n389, n393);
    let n397: ZB = zb_or(n395, n396);
    let n398: ZB = zb_not(n395);
    let n399: ZB = zb_and(n395, n397);
    let n400: ZB = zb_and(n397, n398);
    let n401: ZB = zb_or(n399, n400);
    let n402: ZB = zb_not(n399);
    let n403: ZB = zb_and(n399, n401);
    let n404: ZB = zb_and(n401, n402);
    let n405: ZN = zn_add(n102, n378);
    let n406: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n380);
    let n407: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n380);
    let n408: ZB = zb_and(n403, n406);
    let n409: ZB = zb_and(n403, n407);
    let n410: ZB = zb_and(n381, n408);
    let n411: ZB = zb_and(n382, n408);
    let n412: ZB = zb_or(n410, n411);
    let n413: ZB = zb_not(n410);
    let n414: ZB = zb_and(n410, n412);
    let n415: ZB = zb_and(n412, n413);
    let n416: ZB = zb_or(n414, n415);
    let n417: ZN = zn_add(n108, n405);
    let n418: ZN = zn_add(n378, n417);
    let n419: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n418, n106, n105, P8::from_raw(0i32));
    let n420: ZB = zb_not(n419);
    let n421: ZB = zb_and(n416, n420);
    let n422: ZB = zb_and(n416, n419);
    let n423: ZB = zb_or(n421, n422);
    let n424: ZB = zb_not(n421);
    let n425: ZB = zb_and(n421, n423);
    let n426: ZB = zb_and(n423, n424);
    let n427: ZB = zb_or(n425, n426);
    let n428: ZB = zb_not(n425);
    let n429: ZB = zb_and(n425, n427);
    let n430: ZB = zb_and(n427, n428);
    let n431: ZN = zn_add(n378, n405);
    let n432: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n380);
    let n433: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n380);
    let n434: ZB = zb_and(n429, n432);
    let n435: ZB = zb_and(n429, n433);
    let n436: ZB = zb_and(n381, n434);
    let n437: ZB = zb_and(n382, n434);
    let n438: ZB = zb_or(n436, n437);
    let n439: ZB = zb_not(n436);
    let n440: ZB = zb_and(n436, n438);
    let n441: ZB = zb_and(n438, n439);
    let n442: ZB = zb_or(n440, n441);
    let n443: ZN = zn_add(n108, n431);
    let n444: ZN = zn_add(n378, n443);
    let n445: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n444, n106, n105, P8::from_raw(0i32));
    let n446: ZB = zb_not(n445);
    let n447: ZB = zb_and(n442, n446);
    let n448: ZB = zb_and(n442, n445);
    let n449: ZB = zb_or(n447, n448);
    let n450: ZB = zb_not(n447);
    let n451: ZB = zb_and(n447, n449);
    let n452: ZB = zb_and(n449, n450);
    let n453: ZB = zb_or(n451, n452);
    let n454: ZB = zb_not(n451);
    let n455: ZB = zb_and(n451, n453);
    let n456: ZB = zb_and(n453, n454);
    let n457: ZN = zn_add(n378, n431);
    let n458: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n380);
    let n459: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n380);
    let n460: ZB = zb_and(n455, n458);
    let n461: ZB = zb_and(n455, n459);
    let n462: ZB = zb_and(n381, n460);
    let n463: ZB = zb_and(n382, n460);
    let n464: ZB = zb_or(n462, n463);
    let n465: ZB = zb_not(n462);
    let n466: ZB = zb_and(n462, n464);
    let n467: ZB = zb_and(n464, n465);
    let n468: ZB = zb_or(n466, n467);
    let n469: ZN = zn_add(n108, n457);
    let n470: ZN = zn_add(n378, n469);
    let n471: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n470, n106, n105, P8::from_raw(0i32));
    let n472: ZB = zb_not(n471);
    let n473: ZB = zb_and(n468, n472);
    let n474: ZB = zb_and(n468, n471);
    let n475: ZB = zb_or(n473, n474);
    let n476: ZB = zb_not(n473);
    let n477: ZB = zb_and(n473, n475);
    let n478: ZB = zb_and(n475, n476);
    let n479: ZB = zb_or(n477, n478);
    let n480: ZB = zb_not(n477);
    let n481: ZB = zb_and(n477, n479);
    let n482: ZB = zb_and(n479, n480);
    let n483: ZN = zn_add(n378, n457);
    let n484: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n380);
    let n485: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n380);
    let n486: ZB = zb_and(n481, n484);
    let n487: ZB = zb_and(n481, n485);
    let n488: ZB = zb_and(n381, n486);
    let n489: ZB = zb_and(n382, n486);
    let n490: ZB = zb_or(n488, n489);
    let n491: ZB = zb_not(n488);
    let n492: ZB = zb_and(n488, n490);
    let n493: ZB = zb_and(n490, n491);
    let n494: ZB = zb_or(n492, n493);
    let n495: ZN = zn_add(n108, n483);
    let n496: ZN = zn_add(n378, n495);
    let n497: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n496, n106, n105, P8::from_raw(0i32));
    let n498: ZB = zb_not(n497);
    let n499: ZB = zb_and(n494, n498);
    let n500: ZB = zb_and(n494, n497);
    let n501: ZB = zb_or(n499, n500);
    let n502: ZB = zb_not(n499);
    let n503: ZB = zb_and(n499, n501);
    let n504: ZB = zb_and(n501, n502);
    let n505: ZB = zb_or(n503, n504);
    let n506: ZB = zb_not(n503);
    let n507: ZB = zb_and(n503, n505);
    let n508: ZB = zb_and(n505, n506);
    let n509: ZN = zn_add(n378, n483);
    let n510: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n380);
    let n511: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n380);
    let n512: ZB = zb_and(n507, n510);
    let n513: ZB = zb_and(n507, n511);
    let n514: ZB = zb_and(n381, n512);
    let n515: ZB = zb_and(n382, n512);
    let n516: ZB = zb_or(n514, n515);
    let n517: ZB = zb_not(n514);
    let n518: ZB = zb_and(n514, n516);
    let n519: ZB = zb_and(n516, n517);
    let n520: ZB = zb_or(n518, n519);
    let n521: ZN = zn_add(n108, n509);
    let n522: ZN = zn_add(n378, n521);
    let n523: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n522, n106, n105, P8::from_raw(0i32));
    let n524: ZB = zb_not(n523);
    let n525: ZB = zb_and(n520, n524);
    let n526: ZB = zb_and(n520, n523);
    let n527: ZB = zb_or(n525, n526);
    let n528: ZB = zb_not(n525);
    let n529: ZB = zb_and(n525, n527);
    let n530: ZB = zb_and(n527, n528);
    let n531: ZB = zb_or(n529, n530);
    let n532: ZB = zb_not(n529);
    let n533: ZB = zb_and(n529, n531);
    let n534: ZB = zb_and(n531, n532);
    let n535: ZN = zn_add(n378, n509);
    let n536: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n380);
    let n537: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n380);
    let n538: ZB = zb_and(n533, n536);
    let n539: ZB = zb_and(n533, n537);
    let n540: ZB = zb_and(n381, n538);
    let n541: ZB = zb_and(n382, n538);
    let n542: ZB = zb_or(n540, n541);
    let n543: ZB = zb_not(n540);
    let n544: ZB = zb_and(n540, n542);
    let n545: ZB = zb_and(n542, n543);
    let n546: ZB = zb_or(n544, n545);
    let n547: ZN = zn_add(n108, n535);
    let n548: ZN = zn_add(n378, n547);
    let n549: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n548, n106, n105, P8::from_raw(0i32));
    let n550: ZB = zb_not(n549);
    let n551: ZB = zb_and(n546, n550);
    let n552: ZB = zb_and(n546, n549);
    let n553: ZB = zb_or(n551, n552);
    let n554: ZB = zb_not(n551);
    let n555: ZB = zb_and(n551, n553);
    let n556: ZB = zb_and(n553, n554);
    let n557: ZB = zb_or(n555, n556);
    let n558: ZB = zb_not(n555);
    let n559: ZB = zb_and(n555, n557);
    let n560: ZB = zb_and(n557, n558);
    let n561: ZN = zn_add(n378, n535);
    let n562: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n380);
    let n563: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n380);
    let n564: ZB = zb_and(n559, n562);
    let n565: ZB = zb_and(n559, n563);
    let n566: ZB = zb_and(n381, n564);
    let n567: ZB = zb_and(n382, n564);
    let n568: ZB = zb_or(n566, n567);
    let n569: ZB = zb_not(n566);
    let n570: ZB = zb_and(n566, n568);
    let n571: ZB = zb_and(n568, n569);
    let n572: ZB = zb_or(n570, n571);
    let n573: ZN = zn_add(n108, n561);
    let n574: ZN = zn_add(n378, n573);
    let n575: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n391, n574, n106, n105, P8::from_raw(0i32));
    let n576: ZB = zb_not(n575);
    let n577: ZB = zb_and(n572, n576);
    let n578: ZB = zb_and(n572, n575);
    let n579: ZB = zb_or(n577, n578);
    let n580: ZB = zb_not(n577);
    let n581: ZB = zb_and(n577, n579);
    let n582: ZB = zb_and(n579, n580);
    let n583: ZB = zb_or(n581, n582);
    let n584: ZB = zb_not(n581);
    let n585: ZB = zb_and(n581, n583);
    let n586: ZB = zb_and(n583, n584);
    let n587: ZN = zn_add(n378, n561);
    let n588: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n380);
    let n589: ZB = zb_and(n360, n588);
    let n590: ZN = zsel_n(n409, n365, zn_splat(P8::from_raw(0i32)));
    let n591: ZN = zsel_n(n409, n112, zn_splat(P8::from_raw(0i32)));
    let n592: ZB = zb_or(n409, n430);
    let n593: ZN = zsel_n(n435, n365, zn_splat(P8::from_raw(0i32)));
    let n594: ZN = zsel_n(n435, n112, zn_splat(P8::from_raw(0i32)));
    let n595: ZB = zb_or(n435, n456);
    let n596: ZN = zsel_n(n461, n365, zn_splat(P8::from_raw(0i32)));
    let n597: ZN = zsel_n(n461, n112, zn_splat(P8::from_raw(0i32)));
    let n598: ZB = zb_or(n461, n482);
    let n599: ZN = zsel_n(n487, n365, zn_splat(P8::from_raw(0i32)));
    let n600: ZN = zsel_n(n487, n112, zn_splat(P8::from_raw(0i32)));
    let n601: ZB = zb_or(n487, n508);
    let n602: ZN = zsel_n(n513, n365, zn_splat(P8::from_raw(0i32)));
    let n603: ZN = zsel_n(n513, n112, zn_splat(P8::from_raw(0i32)));
    let n604: ZB = zb_or(n513, n534);
    let n605: ZN = zsel_n(n539, n365, zn_splat(P8::from_raw(0i32)));
    let n606: ZN = zsel_n(n539, n112, zn_splat(P8::from_raw(0i32)));
    let n607: ZB = zb_or(n539, n560);
    let n608: ZN = zsel_n(n565, n365, zn_splat(P8::from_raw(0i32)));
    let n609: ZN = zsel_n(n565, n112, zn_splat(P8::from_raw(0i32)));
    let n610: ZB = zb_or(n565, n586);
    let n611: ZN = zsel_n(n585, n587, n102);
    let n612: ZN = zsel_n(n585, n365, zn_splat(P8::from_raw(0i32)));
    let n613: ZN = zsel_n(n585, n112, zn_splat(P8::from_raw(0i32)));
    let n614: ZB = zb_or(n404, n585);
    let n615: ZB = zsel_b(n585, n589, n360);
    let n616: ZN = zsel_n(n592, n405, n431);
    let n617: ZN = zsel_n(n592, n590, n593);
    let n618: ZN = zsel_n(n592, n591, n594);
    let n619: ZB = zb_or(n592, n595);
    let n620: ZN = zsel_n(n598, n457, n483);
    let n621: ZN = zsel_n(n598, n596, n599);
    let n622: ZN = zsel_n(n598, n597, n600);
    let n623: ZB = zb_or(n598, n601);
    let n624: ZN = zsel_n(n604, n509, n535);
    let n625: ZN = zsel_n(n604, n602, n605);
    let n626: ZN = zsel_n(n604, n603, n606);
    let n627: ZB = zb_or(n604, n607);
    let n628: ZN = zsel_n(n610, n561, n611);
    let n629: ZN = zsel_n(n610, n608, n612);
    let n630: ZN = zsel_n(n610, n609, n613);
    let n631: ZB = zb_or(n610, n614);
    let n632: ZB = zsel_b(n610, n360, n615);
    let n633: ZN = zsel_n(n619, n616, n620);
    let n634: ZN = zsel_n(n619, n617, n621);
    let n635: ZN = zsel_n(n619, n618, n622);
    let n636: ZB = zb_or(n619, n623);
    let n637: ZN = zsel_n(n627, n624, n628);
    let n638: ZN = zsel_n(n627, n625, n629);
    let n639: ZN = zsel_n(n627, n626, n630);
    let n640: ZB = zb_or(n627, n631);
    let n641: ZB = zsel_b(n627, n360, n632);
    let n642: ZN = zsel_n(n636, n633, n637);
    let n643: ZN = zsel_n(n636, n634, n638);
    let n644: ZN = zsel_n(n636, n635, n639);
    let n645: ZB = zb_or(n636, n640);
    let n646: ZB = zsel_b(n636, n360, n641);
    let n647: ZN = zn_add(n102, n363);
    let n648: ZN = zsel_n(n645, n642, n647);
    let n649: ZN = zsel_n(n645, n643, n365);
    let n650: ZN = zsel_n(n645, n644, n112);
    let n651: ZB = zb_or(n367, n645);
    let n652: ZB = zsel_b(n645, n646, n360);
    let n653: ZN = zsel_n(n651, n355, n101);
    let n654: ZN = zsel_n(n651, n648, n102);
    let n655: ZN = zsel_n(n651, n356, n109);
    let n656: ZN = zsel_n(n651, n649, n110);
    let n657: ZN = zsel_n(n651, n357, n111);
    let n658: ZN = zsel_n(n651, n650, n112);
    let n659: ZB = zb_not(n651);
    let n660: ZB = zb_or(n652, n659);
    let n661: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n100);
    let n662: ZB = zb_not(n661);
    let n663: ZB = zb_and(n57, n661);
    let n664: ZB = zb_and(n57, n662);
    let n665: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n114);
    let n666: ZB = zn_lt(n654, n665);
    let n667: ZB = zn_ge(n654, n665);
    let n668: ZB = zb_and(n663, n666);
    let n669: ZB = zb_and(n663, n667);
    let n670: ZN = zsel_n(n668, zn_splat(P8::from_raw(196608i32)), n97);
    let n671: ZN = zsel_n(n668, zn_splat(P8::from_raw(65536i32)), n100);
    let n672: ZB = zb_or(n668, n669);
    let n673: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), n100);
    let n674: ZB = zb_not(n673);
    let n675: ZB = zb_and(n664, n673);
    let n676: ZB = zb_and(n664, n674);
    let n677: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n658);
    let n678: ZB = zn_gt(n677, zn_splat(P8::from_raw(0i32)));
    let n679: ZB = zn_le(n677, zn_splat(P8::from_raw(0i32)));
    let n680: ZB = zb_and(n675, n678);
    let n681: ZB = zb_and(n675, n679);
    let n682: ZB = zn_gt(n97, zn_splat(P8::from_raw(0i32)));
    let n683: ZB = zb_or(n680, n681);
    let n684: ZB = zb_and(n680, n682);
    let n685: ZB = zb_not(n684);
    let n686: ZB = zb_and(n683, n684);
    let n687: ZB = zb_and(n683, n685);
    let n688: ZN = zn_sub(n97, zn_splat(P8::from_raw(65536i32)));
    let n689: ZN = zsel_n(n686, n688, n97);
    let n690: ZN = zsel_n(n686, zn_splat(P8::from_raw(0i32)), n677);
    let n691: ZB = zb_or(n686, n687);
    let n692: ZB = zn_gt(n690, zn_splat(P8::from_raw(0i32)));
    let n693: ZB = zn_le(n690, zn_splat(P8::from_raw(0i32)));
    let n694: ZB = zb_and(n691, n692);
    let n695: ZB = zb_and(n691, n693);
    let n696: ZB = zn_gt(n654, n114);
    let n697: ZB = zb_or(n694, n695);
    let n698: ZB = zb_and(n694, n696);
    let n699: ZB = zb_not(n698);
    let n700: ZB = zb_and(n697, n698);
    let n701: ZB = zb_and(n697, n699);
    let n702: ZN = zsel_n(n700, zn_splat(P8::from_raw(327680i32)), n689);
    let n703: ZN = zsel_n(n700, zn_splat(P8::from_raw(131072i32)), n100);
    let n704: ZN = zsel_n(n700, n114, n654);
    let n705: ZN = zsel_n(n700, zn_splat(P8::from_raw(0i32)), n657);
    let n706: ZN = zsel_n(n700, zn_splat(P8::from_raw(0i32)), n690);
    let n707: ZB = zb_or(n700, n701);
    let n708: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), n100);
    let n709: ZB = zb_not(n708);
    let n710: ZB = zb_and(n676, n708);
    let n711: ZB = zb_and(n676, n709);
    let n712: ZB = zn_lt(n688, zn_splat(P8::from_raw(0i32)));
    let n713: ZB = zn_ge(n688, zn_splat(P8::from_raw(0i32)));
    let n714: ZB = zb_and(n710, n712);
    let n715: ZB = zb_and(n710, n713);
    let n716: ZN = zsel_n(n715, n688, n97);
    let n717: ZN = zsel_n(n715, zn_splat(P8::from_raw(393216i32)), n99);
    let n718: ZB = zb_or(n711, n715);
    let n719: ZN = zsel_n(n707, n702, n716);
    let n720: ZN = zsel_n(n707, n99, n717);
    let n721: ZN = zsel_n(n707, n703, n100);
    let n722: ZN = zsel_n(n707, n704, n654);
    let n723: ZN = zsel_n(n707, n705, n657);
    let n724: ZN = zsel_n(n707, n706, n658);
    let n725: ZB = zb_or(n707, n718);
    let n726: ZN = zsel_n(n672, n670, n719);
    let n727: ZN = zsel_n(n672, n99, n720);
    let n728: ZN = zsel_n(n672, n671, n721);
    let n729: ZN = zsel_n(n672, n654, n722);
    let n730: ZN = zsel_n(n672, n657, n723);
    let n731: ZN = zsel_n(n672, n658, n724);
    let n732: ZB = zb_or(n672, n725);
    let n733: ZB = zb_not(r_c43);
    let n734: ZB = zb_and(r_c43, n714);
    let n735: ZB = zb_and(n714, n733);
    let n736: ZN = zsel_n(n735, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-65536i32)));
    let n737: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n653);
    let n738: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n654);
    let n739: ZN = zn_div(n737, zn_splat(P8::from_raw(524288i32)));
    let n740: ZN = zn_flr(n739);
    let n741: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n740);
    let n742: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n737);
    let n743: ZN = zn_sub(n742, zn_splat(P8::from_raw(65536i32)));
    let n744: ZN = zn_div(n743, zn_splat(P8::from_raw(524288i32)));
    let n745: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n744);
    let n746: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n741);
    let n747: ZB = zn_le(n746, n745);
    let n748: ZB = zn_gt(n746, n745);
    let n749: ZB = zb_and(n735, n747);
    let n750: ZB = zb_and(n735, n748);
    let n751: ZN = zn_div(n738, zn_splat(P8::from_raw(524288i32)));
    let n752: ZN = zn_flr(n751);
    let n753: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n752);
    let n754: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n738);
    let n755: ZN = zn_sub(n754, zn_splat(P8::from_raw(65536i32)));
    let n756: ZN = zn_div(n755, zn_splat(P8::from_raw(524288i32)));
    let n757: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n756);
    let n758: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n753);
    let n759: ZB = zn_le(n758, n757);
    let n760: ZB = zn_gt(n758, n757);
    let n761: ZB = zb_and(n749, n759);
    let n762: ZB = zb_and(n749, n760);
    let n763: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n746);
    let n764: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n758);
    let n765: ZN = zn_mget(g.cart, n763, n764);
    let n766: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n765);
    let n767: ZB = zb_not(n766);
    let n768: ZB = zb_and(n761, n766);
    let n769: ZB = zb_and(n761, n767);
    let n770: ZN = zn_rem(n755, zn_splat(P8::from_raw(524288i32)));
    let n771: ZB = zn_ge(n770, zn_splat(P8::from_raw(393216i32)));
    let n772: ZB = zn_lt(n770, zn_splat(P8::from_raw(393216i32)));
    let n773: ZB = zb_and(n768, n772);
    let n774: ZB = zb_and(n768, n771);
    let n775: ZN = zn_mul(n758, zn_splat(P8::from_raw(524288i32)));
    let n776: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n775);
    let n777: ZB = zn_eq(n754, n776);
    let n778: ZB = zb_or(n773, n774);
    let n779: ZB = zb_not(n773);
    let n780: ZB = zb_or(n777, n779);
    let n781: ZB = zb_or(n769, n778);
    let n782: ZB = zb_and(n778, n780);
    let n783: ZB = zb_not(n782);
    let n784: ZB = zb_and(n781, n782);
    let n785: ZB = zb_and(n781, n783);
    let n786: ZB = zb_or(n784, n785);
    let n787: ZB = zb_not(n784);
    let n788: ZB = zb_and(n784, n786);
    let n789: ZB = zb_and(n786, n787);
    let n790: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n765);
    let n791: ZB = zb_not(n790);
    let n792: ZB = zb_and(n789, n790);
    let n793: ZB = zb_and(n789, n791);
    let n794: ZN = zn_rem(n738, zn_splat(P8::from_raw(524288i32)));
    let n795: ZB = zn_le(n794, zn_splat(P8::from_raw(131072i32)));
    let n796: ZB = zb_or(n792, n793);
    let n797: ZB = zb_and(n792, n795);
    let n798: ZB = zb_not(n797);
    let n799: ZB = zb_and(n796, n797);
    let n800: ZB = zb_and(n796, n798);
    let n801: ZB = zb_or(n799, n800);
    let n802: ZB = zb_not(n799);
    let n803: ZB = zb_and(n799, n801);
    let n804: ZB = zb_and(n801, n802);
    let n806: ZB = zn_eq(n765, zn_splat(P8::from_raw(2818048i32)));
    let n807: ZB = zb_not(n806);
    let n808: ZB = zb_and(n804, n806);
    let n809: ZB = zb_and(n804, n807);
    let n810: ZN = zn_rem(n737, zn_splat(P8::from_raw(524288i32)));
    let n811: ZB = zn_le(n810, zn_splat(P8::from_raw(131072i32)));
    let n812: ZB = zb_or(n808, n809);
    let n813: ZB = zb_and(n808, n811);
    let n814: ZB = zb_not(n813);
    let n815: ZB = zb_and(n812, n813);
    let n816: ZB = zb_and(n812, n814);
    let n817: ZB = zb_or(n815, n816);
    let n818: ZB = zb_not(n815);
    let n819: ZB = zb_and(n815, n817);
    let n820: ZB = zb_and(n817, n818);
    let n822: ZB = zn_eq(n765, zn_splat(P8::from_raw(3866624i32)));
    let n823: ZB = zb_not(n822);
    let n824: ZB = zb_and(n820, n822);
    let n825: ZB = zb_and(n820, n823);
    let n826: ZN = zn_rem(n743, zn_splat(P8::from_raw(524288i32)));
    let n827: ZB = zn_ge(n826, zn_splat(P8::from_raw(393216i32)));
    let n828: ZB = zn_lt(n826, zn_splat(P8::from_raw(393216i32)));
    let n829: ZB = zb_and(n824, n828);
    let n830: ZB = zb_and(n824, n827);
    let n831: ZN = zn_mul(n746, zn_splat(P8::from_raw(524288i32)));
    let n832: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n831);
    let n833: ZB = zn_eq(n742, n832);
    let n834: ZB = zb_or(n829, n830);
    let n835: ZB = zb_not(n829);
    let n836: ZB = zb_or(n833, n835);
    let n837: ZB = zb_or(n825, n834);
    let n838: ZB = zb_and(n834, n836);
    let n839: ZB = zb_not(n838);
    let n840: ZB = zb_and(n837, n838);
    let n841: ZB = zb_and(n837, n839);
    let n842: ZB = zb_or(n840, n841);
    let n843: ZB = zb_not(n840);
    let n844: ZB = zb_and(n840, n842);
    let n845: ZB = zb_and(n842, n843);
    let n846: ZB = zb_or(n819, n844);
    let n847: ZB = zb_or(n803, n846);
    let n848: ZB = zb_or(n788, n847);
    let n849: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n753);
    let n850: ZB = zn_le(n849, n757);
    let n851: ZB = zn_gt(n849, n757);
    let n852: ZB = zb_and(n845, n850);
    let n853: ZB = zb_and(n845, n851);
    let n854: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n849);
    let n855: ZN = zn_mget(g.cart, n763, n854);
    let n856: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n855);
    let n857: ZB = zb_not(n856);
    let n858: ZB = zb_and(n852, n856);
    let n859: ZB = zb_and(n852, n857);
    let n860: ZB = zb_and(n772, n858);
    let n861: ZB = zb_and(n771, n858);
    let n862: ZN = zn_mul(n849, zn_splat(P8::from_raw(524288i32)));
    let n863: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n862);
    let n864: ZB = zn_eq(n754, n863);
    let n865: ZB = zb_or(n860, n861);
    let n866: ZB = zb_not(n860);
    let n867: ZB = zb_or(n864, n866);
    let n868: ZB = zb_or(n859, n865);
    let n869: ZB = zb_and(n865, n867);
    let n870: ZB = zb_not(n869);
    let n871: ZB = zb_and(n868, n869);
    let n872: ZB = zb_and(n868, n870);
    let n873: ZB = zb_or(n871, n872);
    let n874: ZB = zb_not(n871);
    let n875: ZB = zb_and(n871, n873);
    let n876: ZB = zb_and(n873, n874);
    let n877: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n855);
    let n878: ZB = zb_not(n877);
    let n879: ZB = zb_and(n876, n877);
    let n880: ZB = zb_and(n876, n878);
    let n881: ZB = zb_or(n879, n880);
    let n882: ZB = zb_and(n795, n879);
    let n883: ZB = zb_not(n882);
    let n884: ZB = zb_and(n881, n882);
    let n885: ZB = zb_and(n881, n883);
    let n886: ZB = zb_or(n884, n885);
    let n887: ZB = zb_not(n884);
    let n888: ZB = zb_and(n884, n886);
    let n889: ZB = zb_and(n886, n887);
    let n890: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n855);
    let n891: ZB = zb_not(n890);
    let n892: ZB = zb_and(n889, n890);
    let n893: ZB = zb_and(n889, n891);
    let n894: ZB = zb_or(n892, n893);
    let n895: ZB = zb_and(n811, n892);
    let n896: ZB = zb_not(n895);
    let n897: ZB = zb_and(n894, n895);
    let n898: ZB = zb_and(n894, n896);
    let n899: ZB = zb_or(n897, n898);
    let n900: ZB = zb_not(n897);
    let n901: ZB = zb_and(n897, n899);
    let n902: ZB = zb_and(n899, n900);
    let n903: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n855);
    let n904: ZB = zb_not(n903);
    let n905: ZB = zb_and(n902, n903);
    let n906: ZB = zb_and(n902, n904);
    let n907: ZB = zb_and(n828, n905);
    let n908: ZB = zb_and(n827, n905);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_not(n907);
    let n911: ZB = zb_or(n833, n910);
    let n912: ZB = zb_or(n906, n909);
    let n913: ZB = zb_and(n909, n911);
    let n914: ZB = zb_not(n913);
    let n915: ZB = zb_and(n912, n913);
    let n916: ZB = zb_and(n912, n914);
    let n917: ZB = zb_or(n915, n916);
    let n918: ZB = zb_not(n915);
    let n919: ZB = zb_and(n915, n917);
    let n920: ZB = zb_and(n917, n918);
    let n921: ZB = zb_or(n901, n919);
    let n922: ZB = zb_or(n888, n921);
    let n923: ZB = zb_or(n875, n922);
    let n924: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n753);
    let n925: ZB = zn_le(n924, n757);
    let n926: ZB = zn_gt(n924, n757);
    let n927: ZB = zb_and(n920, n925);
    let n928: ZB = zb_and(n920, n926);
    let n929: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n924);
    let n930: ZN = zn_mget(g.cart, n763, n929);
    let n931: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n930);
    let n932: ZB = zb_not(n931);
    let n933: ZB = zb_and(n927, n931);
    let n934: ZB = zb_and(n927, n932);
    let n935: ZB = zb_and(n772, n933);
    let n936: ZB = zb_and(n771, n933);
    let n937: ZN = zn_mul(n924, zn_splat(P8::from_raw(524288i32)));
    let n938: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n937);
    let n939: ZB = zn_eq(n754, n938);
    let n940: ZB = zb_or(n935, n936);
    let n941: ZB = zb_not(n935);
    let n942: ZB = zb_or(n939, n941);
    let n943: ZB = zb_or(n934, n940);
    let n944: ZB = zb_and(n940, n942);
    let n945: ZB = zb_not(n944);
    let n946: ZB = zb_and(n943, n944);
    let n947: ZB = zb_and(n943, n945);
    let n948: ZB = zb_or(n946, n947);
    let n949: ZB = zb_not(n946);
    let n950: ZB = zb_and(n946, n948);
    let n951: ZB = zb_and(n948, n949);
    let n952: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n930);
    let n953: ZB = zb_not(n952);
    let n954: ZB = zb_and(n951, n952);
    let n955: ZB = zb_and(n951, n953);
    let n956: ZB = zb_or(n954, n955);
    let n957: ZB = zb_and(n795, n954);
    let n958: ZB = zb_not(n957);
    let n959: ZB = zb_and(n956, n957);
    let n960: ZB = zb_and(n956, n958);
    let n961: ZB = zb_or(n959, n960);
    let n962: ZB = zb_not(n959);
    let n963: ZB = zb_and(n959, n961);
    let n964: ZB = zb_and(n961, n962);
    let n965: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n930);
    let n966: ZB = zb_not(n965);
    let n967: ZB = zb_and(n964, n965);
    let n968: ZB = zb_and(n964, n966);
    let n969: ZB = zb_or(n967, n968);
    let n970: ZB = zb_and(n811, n967);
    let n971: ZB = zb_not(n970);
    let n972: ZB = zb_and(n969, n970);
    let n973: ZB = zb_and(n969, n971);
    let n974: ZB = zb_or(n972, n973);
    let n975: ZB = zb_not(n972);
    let n976: ZB = zb_and(n972, n974);
    let n977: ZB = zb_and(n974, n975);
    let n978: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n930);
    let n979: ZB = zb_not(n978);
    let n980: ZB = zb_and(n977, n978);
    let n981: ZB = zb_and(n977, n979);
    let n982: ZB = zb_and(n828, n980);
    let n983: ZB = zb_and(n827, n980);
    let n984: ZB = zb_or(n982, n983);
    let n985: ZB = zb_not(n982);
    let n986: ZB = zb_or(n833, n985);
    let n987: ZB = zb_or(n981, n984);
    let n988: ZB = zb_and(n984, n986);
    let n989: ZB = zb_not(n988);
    let n990: ZB = zb_and(n987, n988);
    let n991: ZB = zb_and(n987, n989);
    let n992: ZB = zb_or(n990, n991);
    let n993: ZB = zb_not(n990);
    let n994: ZB = zb_and(n990, n992);
    let n995: ZB = zb_and(n992, n993);
    let n996: ZB = zb_or(n976, n994);
    let n997: ZB = zb_or(n963, n996);
    let n998: ZB = zb_or(n950, n997);
    let n999: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n753);
    let n1000: ZB = zn_gt(n999, n757);
    let n1001: ZB = zb_and(n660, n1000);
    let n1002: ZB = zb_or(n762, n853);
    let n1003: ZB = zb_or(n848, n923);
    let n1004: ZB = zb_or(n928, n995);
    let n1005: ZB = zsel_b(n928, n660, n1001);
    let n1006: ZB = zb_or(n998, n1003);
    let n1007: ZB = zb_or(n1002, n1004);
    let n1008: ZB = zsel_b(n1002, n660, n1005);
    let n1009: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n741);
    let n1010: ZB = zn_le(n1009, n745);
    let n1011: ZB = zn_gt(n1009, n745);
    let n1012: ZB = zb_and(n1007, n1010);
    let n1013: ZB = zb_and(n1007, n1011);
    let n1014: ZB = zb_and(n759, n1012);
    let n1015: ZB = zb_and(n760, n1012);
    let n1016: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n1009);
    let n1017: ZN = zn_mget(g.cart, n1016, n764);
    let n1018: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1017);
    let n1019: ZB = zb_not(n1018);
    let n1020: ZB = zb_and(n1014, n1018);
    let n1021: ZB = zb_and(n1014, n1019);
    let n1022: ZB = zb_and(n772, n1020);
    let n1023: ZB = zb_and(n771, n1020);
    let n1024: ZB = zb_or(n1022, n1023);
    let n1025: ZB = zb_not(n1022);
    let n1026: ZB = zb_or(n777, n1025);
    let n1027: ZB = zb_or(n1021, n1024);
    let n1028: ZB = zb_and(n1024, n1026);
    let n1029: ZB = zb_not(n1028);
    let n1030: ZB = zb_and(n1027, n1028);
    let n1031: ZB = zb_and(n1027, n1029);
    let n1032: ZB = zb_or(n1030, n1031);
    let n1033: ZB = zb_not(n1030);
    let n1034: ZB = zb_and(n1030, n1032);
    let n1035: ZB = zb_and(n1032, n1033);
    let n1036: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1017);
    let n1037: ZB = zb_not(n1036);
    let n1038: ZB = zb_and(n1035, n1036);
    let n1039: ZB = zb_and(n1035, n1037);
    let n1040: ZB = zb_or(n1038, n1039);
    let n1041: ZB = zb_and(n795, n1038);
    let n1042: ZB = zb_not(n1041);
    let n1043: ZB = zb_and(n1040, n1041);
    let n1044: ZB = zb_and(n1040, n1042);
    let n1045: ZB = zb_or(n1043, n1044);
    let n1046: ZB = zb_not(n1043);
    let n1047: ZB = zb_and(n1043, n1045);
    let n1048: ZB = zb_and(n1045, n1046);
    let n1049: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1017);
    let n1050: ZB = zb_not(n1049);
    let n1051: ZB = zb_and(n1048, n1049);
    let n1052: ZB = zb_and(n1048, n1050);
    let n1053: ZB = zb_or(n1051, n1052);
    let n1054: ZB = zb_and(n811, n1051);
    let n1055: ZB = zb_not(n1054);
    let n1056: ZB = zb_and(n1053, n1054);
    let n1057: ZB = zb_and(n1053, n1055);
    let n1058: ZB = zb_or(n1056, n1057);
    let n1059: ZB = zb_not(n1056);
    let n1060: ZB = zb_and(n1056, n1058);
    let n1061: ZB = zb_and(n1058, n1059);
    let n1062: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1017);
    let n1063: ZB = zb_not(n1062);
    let n1064: ZB = zb_and(n1061, n1062);
    let n1065: ZB = zb_and(n1061, n1063);
    let n1066: ZB = zb_and(n828, n1064);
    let n1067: ZB = zb_and(n827, n1064);
    let n1068: ZN = zn_mul(n1009, zn_splat(P8::from_raw(524288i32)));
    let n1069: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1068);
    let n1070: ZB = zn_eq(n742, n1069);
    let n1071: ZB = zb_or(n1066, n1067);
    let n1072: ZB = zb_not(n1066);
    let n1073: ZB = zb_or(n1070, n1072);
    let n1074: ZB = zb_or(n1065, n1071);
    let n1075: ZB = zb_and(n1071, n1073);
    let n1076: ZB = zb_not(n1075);
    let n1077: ZB = zb_and(n1074, n1075);
    let n1078: ZB = zb_and(n1074, n1076);
    let n1079: ZB = zb_or(n1077, n1078);
    let n1080: ZB = zb_not(n1077);
    let n1081: ZB = zb_and(n1077, n1079);
    let n1082: ZB = zb_and(n1079, n1080);
    let n1083: ZB = zb_or(n1060, n1081);
    let n1084: ZB = zb_or(n1047, n1083);
    let n1085: ZB = zb_or(n1034, n1084);
    let n1086: ZB = zb_and(n850, n1082);
    let n1087: ZB = zb_and(n851, n1082);
    let n1088: ZN = zn_mget(g.cart, n1016, n854);
    let n1089: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1088);
    let n1090: ZB = zb_not(n1089);
    let n1091: ZB = zb_and(n1086, n1089);
    let n1092: ZB = zb_and(n1086, n1090);
    let n1093: ZB = zb_and(n772, n1091);
    let n1094: ZB = zb_and(n771, n1091);
    let n1095: ZB = zb_or(n1093, n1094);
    let n1096: ZB = zb_not(n1093);
    let n1097: ZB = zb_or(n864, n1096);
    let n1098: ZB = zb_or(n1092, n1095);
    let n1099: ZB = zb_and(n1095, n1097);
    let n1100: ZB = zb_not(n1099);
    let n1101: ZB = zb_and(n1098, n1099);
    let n1102: ZB = zb_and(n1098, n1100);
    let n1103: ZB = zb_or(n1101, n1102);
    let n1104: ZB = zb_not(n1101);
    let n1105: ZB = zb_and(n1101, n1103);
    let n1106: ZB = zb_and(n1103, n1104);
    let n1107: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1088);
    let n1108: ZB = zb_not(n1107);
    let n1109: ZB = zb_and(n1106, n1107);
    let n1110: ZB = zb_and(n1106, n1108);
    let n1111: ZB = zb_or(n1109, n1110);
    let n1112: ZB = zb_and(n795, n1109);
    let n1113: ZB = zb_not(n1112);
    let n1114: ZB = zb_and(n1111, n1112);
    let n1115: ZB = zb_and(n1111, n1113);
    let n1116: ZB = zb_or(n1114, n1115);
    let n1117: ZB = zb_not(n1114);
    let n1118: ZB = zb_and(n1114, n1116);
    let n1119: ZB = zb_and(n1116, n1117);
    let n1120: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1088);
    let n1121: ZB = zb_not(n1120);
    let n1122: ZB = zb_and(n1119, n1120);
    let n1123: ZB = zb_and(n1119, n1121);
    let n1124: ZB = zb_or(n1122, n1123);
    let n1125: ZB = zb_and(n811, n1122);
    let n1126: ZB = zb_not(n1125);
    let n1127: ZB = zb_and(n1124, n1125);
    let n1128: ZB = zb_and(n1124, n1126);
    let n1129: ZB = zb_or(n1127, n1128);
    let n1130: ZB = zb_not(n1127);
    let n1131: ZB = zb_and(n1127, n1129);
    let n1132: ZB = zb_and(n1129, n1130);
    let n1133: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1088);
    let n1134: ZB = zb_not(n1133);
    let n1135: ZB = zb_and(n1132, n1133);
    let n1136: ZB = zb_and(n1132, n1134);
    let n1137: ZB = zb_and(n828, n1135);
    let n1138: ZB = zb_and(n827, n1135);
    let n1139: ZB = zb_or(n1137, n1138);
    let n1140: ZB = zb_not(n1137);
    let n1141: ZB = zb_or(n1070, n1140);
    let n1142: ZB = zb_or(n1136, n1139);
    let n1143: ZB = zb_and(n1139, n1141);
    let n1144: ZB = zb_not(n1143);
    let n1145: ZB = zb_and(n1142, n1143);
    let n1146: ZB = zb_and(n1142, n1144);
    let n1147: ZB = zb_or(n1145, n1146);
    let n1148: ZB = zb_not(n1145);
    let n1149: ZB = zb_and(n1145, n1147);
    let n1150: ZB = zb_and(n1147, n1148);
    let n1151: ZB = zb_or(n1131, n1149);
    let n1152: ZB = zb_or(n1118, n1151);
    let n1153: ZB = zb_or(n1105, n1152);
    let n1154: ZB = zb_and(n925, n1150);
    let n1155: ZB = zb_and(n926, n1150);
    let n1156: ZN = zn_mget(g.cart, n1016, n929);
    let n1157: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1156);
    let n1158: ZB = zb_not(n1157);
    let n1159: ZB = zb_and(n1154, n1157);
    let n1160: ZB = zb_and(n1154, n1158);
    let n1161: ZB = zb_and(n772, n1159);
    let n1162: ZB = zb_and(n771, n1159);
    let n1163: ZB = zb_or(n1161, n1162);
    let n1164: ZB = zb_not(n1161);
    let n1165: ZB = zb_or(n939, n1164);
    let n1166: ZB = zb_or(n1160, n1163);
    let n1167: ZB = zb_and(n1163, n1165);
    let n1168: ZB = zb_not(n1167);
    let n1169: ZB = zb_and(n1166, n1167);
    let n1170: ZB = zb_and(n1166, n1168);
    let n1171: ZB = zb_or(n1169, n1170);
    let n1172: ZB = zb_not(n1169);
    let n1173: ZB = zb_and(n1169, n1171);
    let n1174: ZB = zb_and(n1171, n1172);
    let n1175: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1156);
    let n1176: ZB = zb_not(n1175);
    let n1177: ZB = zb_and(n1174, n1175);
    let n1178: ZB = zb_and(n1174, n1176);
    let n1179: ZB = zb_or(n1177, n1178);
    let n1180: ZB = zb_and(n795, n1177);
    let n1181: ZB = zb_not(n1180);
    let n1182: ZB = zb_and(n1179, n1180);
    let n1183: ZB = zb_and(n1179, n1181);
    let n1184: ZB = zb_or(n1182, n1183);
    let n1185: ZB = zb_not(n1182);
    let n1186: ZB = zb_and(n1182, n1184);
    let n1187: ZB = zb_and(n1184, n1185);
    let n1188: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1156);
    let n1189: ZB = zb_not(n1188);
    let n1190: ZB = zb_and(n1187, n1188);
    let n1191: ZB = zb_and(n1187, n1189);
    let n1192: ZB = zb_or(n1190, n1191);
    let n1193: ZB = zb_and(n811, n1190);
    let n1194: ZB = zb_not(n1193);
    let n1195: ZB = zb_and(n1192, n1193);
    let n1196: ZB = zb_and(n1192, n1194);
    let n1197: ZB = zb_or(n1195, n1196);
    let n1198: ZB = zb_not(n1195);
    let n1199: ZB = zb_and(n1195, n1197);
    let n1200: ZB = zb_and(n1197, n1198);
    let n1201: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1156);
    let n1202: ZB = zb_not(n1201);
    let n1203: ZB = zb_and(n1200, n1201);
    let n1204: ZB = zb_and(n1200, n1202);
    let n1205: ZB = zb_and(n828, n1203);
    let n1206: ZB = zb_and(n827, n1203);
    let n1207: ZB = zb_or(n1205, n1206);
    let n1208: ZB = zb_not(n1205);
    let n1209: ZB = zb_or(n1070, n1208);
    let n1210: ZB = zb_or(n1204, n1207);
    let n1211: ZB = zb_and(n1207, n1209);
    let n1212: ZB = zb_not(n1211);
    let n1213: ZB = zb_and(n1210, n1211);
    let n1214: ZB = zb_and(n1210, n1212);
    let n1215: ZB = zb_or(n1213, n1214);
    let n1216: ZB = zb_not(n1213);
    let n1217: ZB = zb_and(n1213, n1215);
    let n1218: ZB = zb_and(n1215, n1216);
    let n1219: ZB = zb_or(n1199, n1217);
    let n1220: ZB = zb_or(n1186, n1219);
    let n1221: ZB = zb_or(n1173, n1220);
    let n1222: ZB = zb_and(n1000, n1008);
    let n1223: ZB = zb_or(n1015, n1087);
    let n1224: ZB = zb_or(n1085, n1153);
    let n1225: ZB = zb_or(n1155, n1218);
    let n1226: ZB = zsel_b(n1155, n1008, n1222);
    let n1227: ZB = zb_or(n1221, n1224);
    let n1228: ZB = zb_or(n1223, n1225);
    let n1229: ZB = zsel_b(n1223, n1008, n1226);
    let n1230: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n741);
    let n1231: ZB = zn_le(n1230, n745);
    let n1232: ZB = zn_gt(n1230, n745);
    let n1233: ZB = zb_and(n1228, n1231);
    let n1234: ZB = zb_and(n1228, n1232);
    let n1235: ZB = zb_and(n759, n1233);
    let n1236: ZB = zb_and(n760, n1233);
    let n1237: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n1230);
    let n1238: ZN = zn_mget(g.cart, n1237, n764);
    let n1239: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1238);
    let n1240: ZB = zb_not(n1239);
    let n1241: ZB = zb_and(n1235, n1239);
    let n1242: ZB = zb_and(n1235, n1240);
    let n1243: ZB = zb_and(n772, n1241);
    let n1244: ZB = zb_and(n771, n1241);
    let n1245: ZB = zb_or(n1243, n1244);
    let n1246: ZB = zb_not(n1243);
    let n1247: ZB = zb_or(n777, n1246);
    let n1248: ZB = zb_or(n1242, n1245);
    let n1249: ZB = zb_and(n1245, n1247);
    let n1250: ZB = zb_not(n1249);
    let n1251: ZB = zb_and(n1248, n1249);
    let n1252: ZB = zb_and(n1248, n1250);
    let n1253: ZB = zb_or(n1251, n1252);
    let n1254: ZB = zb_not(n1251);
    let n1255: ZB = zb_and(n1251, n1253);
    let n1256: ZB = zb_and(n1253, n1254);
    let n1257: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1238);
    let n1258: ZB = zb_not(n1257);
    let n1259: ZB = zb_and(n1256, n1257);
    let n1260: ZB = zb_and(n1256, n1258);
    let n1261: ZB = zb_or(n1259, n1260);
    let n1262: ZB = zb_and(n795, n1259);
    let n1263: ZB = zb_not(n1262);
    let n1264: ZB = zb_and(n1261, n1262);
    let n1265: ZB = zb_and(n1261, n1263);
    let n1266: ZB = zb_or(n1264, n1265);
    let n1267: ZB = zb_not(n1264);
    let n1268: ZB = zb_and(n1264, n1266);
    let n1269: ZB = zb_and(n1266, n1267);
    let n1270: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1238);
    let n1271: ZB = zb_not(n1270);
    let n1272: ZB = zb_and(n1269, n1270);
    let n1273: ZB = zb_and(n1269, n1271);
    let n1274: ZB = zb_or(n1272, n1273);
    let n1275: ZB = zb_and(n811, n1272);
    let n1276: ZB = zb_not(n1275);
    let n1277: ZB = zb_and(n1274, n1275);
    let n1278: ZB = zb_and(n1274, n1276);
    let n1279: ZB = zb_or(n1277, n1278);
    let n1280: ZB = zb_not(n1277);
    let n1281: ZB = zb_and(n1277, n1279);
    let n1282: ZB = zb_and(n1279, n1280);
    let n1283: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1238);
    let n1284: ZB = zb_not(n1283);
    let n1285: ZB = zb_and(n1282, n1283);
    let n1286: ZB = zb_and(n1282, n1284);
    let n1287: ZB = zb_and(n828, n1285);
    let n1288: ZB = zb_and(n827, n1285);
    let n1289: ZN = zn_mul(n1230, zn_splat(P8::from_raw(524288i32)));
    let n1290: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1289);
    let n1291: ZB = zn_eq(n742, n1290);
    let n1292: ZB = zb_or(n1287, n1288);
    let n1293: ZB = zb_not(n1287);
    let n1294: ZB = zb_or(n1291, n1293);
    let n1295: ZB = zb_or(n1286, n1292);
    let n1296: ZB = zb_and(n1292, n1294);
    let n1297: ZB = zb_not(n1296);
    let n1298: ZB = zb_and(n1295, n1296);
    let n1299: ZB = zb_and(n1295, n1297);
    let n1300: ZB = zb_or(n1298, n1299);
    let n1301: ZB = zb_not(n1298);
    let n1302: ZB = zb_and(n1298, n1300);
    let n1303: ZB = zb_and(n1300, n1301);
    let n1304: ZB = zb_or(n1281, n1302);
    let n1305: ZB = zb_or(n1268, n1304);
    let n1306: ZB = zb_or(n1255, n1305);
    let n1307: ZB = zb_and(n850, n1303);
    let n1308: ZB = zb_and(n851, n1303);
    let n1309: ZN = zn_mget(g.cart, n1237, n854);
    let n1310: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1309);
    let n1311: ZB = zb_not(n1310);
    let n1312: ZB = zb_and(n1307, n1310);
    let n1313: ZB = zb_and(n1307, n1311);
    let n1314: ZB = zb_and(n772, n1312);
    let n1315: ZB = zb_and(n771, n1312);
    let n1316: ZB = zb_or(n1314, n1315);
    let n1317: ZB = zb_not(n1314);
    let n1318: ZB = zb_or(n864, n1317);
    let n1319: ZB = zb_or(n1313, n1316);
    let n1320: ZB = zb_and(n1316, n1318);
    let n1321: ZB = zb_not(n1320);
    let n1322: ZB = zb_and(n1319, n1320);
    let n1323: ZB = zb_and(n1319, n1321);
    let n1324: ZB = zb_or(n1322, n1323);
    let n1325: ZB = zb_not(n1322);
    let n1326: ZB = zb_and(n1322, n1324);
    let n1327: ZB = zb_and(n1324, n1325);
    let n1328: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1309);
    let n1329: ZB = zb_not(n1328);
    let n1330: ZB = zb_and(n1327, n1328);
    let n1331: ZB = zb_and(n1327, n1329);
    let n1332: ZB = zb_or(n1330, n1331);
    let n1333: ZB = zb_and(n795, n1330);
    let n1334: ZB = zb_not(n1333);
    let n1335: ZB = zb_and(n1332, n1333);
    let n1336: ZB = zb_and(n1332, n1334);
    let n1337: ZB = zb_or(n1335, n1336);
    let n1338: ZB = zb_not(n1335);
    let n1339: ZB = zb_and(n1335, n1337);
    let n1340: ZB = zb_and(n1337, n1338);
    let n1341: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1309);
    let n1342: ZB = zb_not(n1341);
    let n1343: ZB = zb_and(n1340, n1341);
    let n1344: ZB = zb_and(n1340, n1342);
    let n1345: ZB = zb_or(n1343, n1344);
    let n1346: ZB = zb_and(n811, n1343);
    let n1347: ZB = zb_not(n1346);
    let n1348: ZB = zb_and(n1345, n1346);
    let n1349: ZB = zb_and(n1345, n1347);
    let n1350: ZB = zb_or(n1348, n1349);
    let n1351: ZB = zb_not(n1348);
    let n1352: ZB = zb_and(n1348, n1350);
    let n1353: ZB = zb_and(n1350, n1351);
    let n1354: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1309);
    let n1355: ZB = zb_not(n1354);
    let n1356: ZB = zb_and(n1353, n1354);
    let n1357: ZB = zb_and(n1353, n1355);
    let n1358: ZB = zb_and(n828, n1356);
    let n1359: ZB = zb_and(n827, n1356);
    let n1360: ZB = zb_or(n1358, n1359);
    let n1361: ZB = zb_not(n1358);
    let n1362: ZB = zb_or(n1291, n1361);
    let n1363: ZB = zb_or(n1357, n1360);
    let n1364: ZB = zb_and(n1360, n1362);
    let n1365: ZB = zb_not(n1364);
    let n1366: ZB = zb_and(n1363, n1364);
    let n1367: ZB = zb_and(n1363, n1365);
    let n1368: ZB = zb_or(n1366, n1367);
    let n1369: ZB = zb_not(n1366);
    let n1370: ZB = zb_and(n1366, n1368);
    let n1371: ZB = zb_and(n1368, n1369);
    let n1372: ZB = zb_or(n1352, n1370);
    let n1373: ZB = zb_or(n1339, n1372);
    let n1374: ZB = zb_or(n1326, n1373);
    let n1375: ZB = zb_and(n925, n1371);
    let n1376: ZB = zb_and(n926, n1371);
    let n1377: ZN = zn_mget(g.cart, n1237, n929);
    let n1378: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1377);
    let n1379: ZB = zb_not(n1378);
    let n1380: ZB = zb_and(n1375, n1378);
    let n1381: ZB = zb_and(n1375, n1379);
    let n1382: ZB = zb_and(n772, n1380);
    let n1383: ZB = zb_and(n771, n1380);
    let n1384: ZB = zb_or(n1382, n1383);
    let n1385: ZB = zb_not(n1382);
    let n1386: ZB = zb_or(n939, n1385);
    let n1387: ZB = zb_or(n1381, n1384);
    let n1388: ZB = zb_and(n1384, n1386);
    let n1389: ZB = zb_not(n1388);
    let n1390: ZB = zb_and(n1387, n1388);
    let n1391: ZB = zb_and(n1387, n1389);
    let n1392: ZB = zb_or(n1390, n1391);
    let n1393: ZB = zb_not(n1390);
    let n1394: ZB = zb_and(n1390, n1392);
    let n1395: ZB = zb_and(n1392, n1393);
    let n1396: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1377);
    let n1397: ZB = zb_not(n1396);
    let n1398: ZB = zb_and(n1395, n1396);
    let n1399: ZB = zb_and(n1395, n1397);
    let n1400: ZB = zb_or(n1398, n1399);
    let n1401: ZB = zb_and(n795, n1398);
    let n1402: ZB = zb_not(n1401);
    let n1403: ZB = zb_and(n1400, n1401);
    let n1404: ZB = zb_and(n1400, n1402);
    let n1405: ZB = zb_or(n1403, n1404);
    let n1406: ZB = zb_not(n1403);
    let n1407: ZB = zb_and(n1403, n1405);
    let n1408: ZB = zb_and(n1405, n1406);
    let n1409: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1377);
    let n1410: ZB = zb_not(n1409);
    let n1411: ZB = zb_and(n1408, n1409);
    let n1412: ZB = zb_and(n1408, n1410);
    let n1413: ZB = zb_or(n1411, n1412);
    let n1414: ZB = zb_and(n811, n1411);
    let n1415: ZB = zb_not(n1414);
    let n1416: ZB = zb_and(n1413, n1414);
    let n1417: ZB = zb_and(n1413, n1415);
    let n1418: ZB = zb_or(n1416, n1417);
    let n1419: ZB = zb_not(n1416);
    let n1420: ZB = zb_and(n1416, n1418);
    let n1421: ZB = zb_and(n1418, n1419);
    let n1422: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1377);
    let n1423: ZB = zb_not(n1422);
    let n1424: ZB = zb_and(n1421, n1422);
    let n1425: ZB = zb_and(n1421, n1423);
    let n1426: ZB = zb_and(n828, n1424);
    let n1427: ZB = zb_and(n827, n1424);
    let n1428: ZB = zb_or(n1426, n1427);
    let n1429: ZB = zb_not(n1426);
    let n1430: ZB = zb_or(n1291, n1429);
    let n1431: ZB = zb_or(n1425, n1428);
    let n1432: ZB = zb_and(n1428, n1430);
    let n1433: ZB = zb_not(n1432);
    let n1434: ZB = zb_and(n1431, n1432);
    let n1435: ZB = zb_and(n1431, n1433);
    let n1436: ZB = zb_or(n1434, n1435);
    let n1437: ZB = zb_not(n1434);
    let n1438: ZB = zb_and(n1434, n1436);
    let n1439: ZB = zb_and(n1436, n1437);
    let n1440: ZB = zb_or(n1420, n1438);
    let n1441: ZB = zb_or(n1407, n1440);
    let n1442: ZB = zb_or(n1394, n1441);
    let n1443: ZB = zb_and(n1000, n1229);
    let n1444: ZB = zb_or(n1236, n1308);
    let n1445: ZB = zb_or(n1306, n1374);
    let n1446: ZB = zb_or(n1376, n1439);
    let n1447: ZB = zsel_b(n1376, n1229, n1443);
    let n1448: ZB = zb_or(n1442, n1445);
    let n1449: ZB = zb_or(n1444, n1446);
    let n1450: ZB = zsel_b(n1444, n1229, n1447);
    let n1451: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n741);
    let n1452: ZB = zn_gt(n1451, n745);
    let n1453: ZB = zb_and(n1450, n1452);
    let n1454: ZB = zb_or(n750, n1013);
    let n1455: ZB = zsel_b(n750, n660, n1008);
    let n1456: ZB = zb_or(n1006, n1227);
    let n1457: ZB = zsel_b(n1006, n660, n1008);
    let n1458: ZB = zb_or(n1234, n1449);
    let n1459: ZB = zsel_b(n1234, n1229, n1453);
    let n1460: ZB = zb_or(n1448, n1456);
    let n1461: ZB = zsel_b(n1448, n1229, n1457);
    let n1462: ZB = zb_or(n1454, n1458);
    let n1463: ZB = zsel_b(n1454, n1455, n1459);
    let n1464: ZB = zn_gt(n654, zn_splat(P8::from_raw(8388608i32)));
    let n1465: ZB = zn_le(n654, zn_splat(P8::from_raw(8388608i32)));
    let n1466: ZB = zb_and(n1460, n1464);
    let n1467: ZB = zb_and(n1460, n1465);
    let n1468: ZB = zb_or(n1466, n1467);
    let n1469: ZB = zb_and(n1462, n1464);
    let n1470: ZB = zb_and(n1462, n1465);
    let n1471: ZB = zb_or(n1468, n1469);
    let n1472: ZB = zsel_b(n1468, n1461, n1463);
    let n1473: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n737);
    let n1474: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n738);
    let n1475: ZB = zn_tile_flag_at(g.cache, g.cart, n1473, n1474, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1476: ZB = zb_not(n1475);
    let n1477: ZB = zb_and(n1470, n1476);
    let n1478: ZB = zb_and(n1470, n1475);
    let n1479: ZB = zb_or(n1477, n1478);
    let n1480: ZB = zb_not(n1477);
    let n1481: ZB = zb_and(n1477, n1479);
    let n1482: ZB = zb_and(n1479, n1480);
    let n1483: ZB = zb_or(n1481, n1482);
    let n1484: ZB = zb_not(n1481);
    let n1485: ZB = zb_and(n1471, n1476);
    let n1486: ZB = zb_and(n1471, n1475);
    let n1487: ZB = zb_or(n1485, n1486);
    let n1488: ZB = zb_not(n1485);
    let n1489: ZB = zb_and(n1485, n1487);
    let n1490: ZB = zb_and(n1487, n1488);
    let n1491: ZB = zb_or(n1489, n1490);
    let n1492: ZB = zb_not(n1489);
    let n1493: ZB = zb_and(n1483, n1484);
    let n1494: ZB = zb_and(n1481, n1483);
    let n1495: ZB = zn_lt(r_c88, r_c88);
    let n1496: ZB = zn_ge(r_c88, r_c88);
    let n1497: ZB = zb_and(n1493, n1495);
    let n1498: ZB = zb_and(n1493, n1496);
    let n1499: ZB = zb_or(n1497, n1498);
    let n1500: ZN = zsel_n(n1499, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n1501: ZB = zb_or(n1494, n1499);
    let n1502: ZB = zb_and(n1491, n1492);
    let n1503: ZB = zb_and(n1489, n1491);
    let n1504: ZB = zb_and(n1495, n1502);
    let n1505: ZB = zb_and(n1496, n1502);
    let n1506: ZB = zb_or(n1504, n1505);
    let n1507: ZB = zb_or(n1503, n1506);
    let n1509: ZB = zb_and(n1481, n1501);
    let n1510: ZB = zb_and(n1484, n1501);
    let n1512: ZN = zsel_n(n1509, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1513: ZB = zb_or(n1509, n1510);
    let n1514: ZN = zn_mul(n736, zn_splat(P8::from_raw(65536i32)));
    let n1515: ZB = zn_gt(zn_splat(P8::from_raw(0i32)), n1514);
    let n1516: ZB = zn_le(zn_splat(P8::from_raw(0i32)), n1514);
    let n1517: ZB = zb_and(n1513, n1515);
    let n1518: ZB = zb_and(n1513, n1516);
    let n1519: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n1512);
    let n1520: ZN = zn_max(n1514, n1519);
    let n1521: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1512);
    let n1522: ZN = zn_min(n1514, n1521);
    let n1523: ZN = zsel_n(n1517, n1520, n1522);
    let n1524: ZB = zb_or(n1517, n1518);
    let n1525: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1523);
    let n1526: ZB = zb_not(n1525);
    let n1527: ZB = zb_and(n1524, n1526);
    let n1528: ZB = zb_and(n1524, n1525);
    let n1529: ZB = zn_lt(n1523, zn_splat(P8::from_raw(0i32)));
    let n1530: ZB = zb_and(n1527, n1529);
    let n1531: ZB = zb_or(n1527, n1528);
    let n1533: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n736);
    let n1534: ZB = zb_not(n1533);
    let n1535: ZB = zb_and(n1531, n1534);
    let n1536: ZB = zb_and(n1531, n1533);
    let n1537: ZN = zn_add(n736, n737);
    let n1538: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n738);
    let n1539: ZB = zn_tile_flag_at(g.cache, g.cart, n1537, n1538, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1540: ZB = zb_not(n1539);
    let n1541: ZB = zb_and(n1535, n1540);
    let n1542: ZB = zb_and(n1535, n1539);
    let n1543: ZB = zb_or(n1541, n1542);
    let n1544: ZB = zb_not(n1541);
    let n1545: ZB = zb_and(n1541, n1543);
    let n1546: ZB = zb_and(n1543, n1544);
    let n1547: ZB = zb_or(n1545, n1546);
    let n1548: ZB = zb_not(n1545);
    let n1549: ZB = zb_or(n1536, n1547);
    let n1550: ZB = zb_and(n1547, n1548);
    let n1551: ZB = zb_not(n1550);
    let n1552: ZB = zb_and(n1549, n1550);
    let n1553: ZB = zb_and(n1549, n1551);
    let n1554: ZB = zb_or(n1552, n1553);
    let n1555: ZB = zb_not(n1552);
    let n1556: ZB = zb_and(n1552, n1554);
    let n1557: ZB = zb_and(n1554, n1555);
    let n1558: ZB = zb_or(n1556, n1557);
    let n1559: ZB = zb_and(n1481, n1558);
    let n1560: ZB = zb_and(n1484, n1558);
    let n1561: ZN = zsel_n(n1559, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n1562: ZB = zb_or(n1559, n1560);
    let n1565: ZN = zn_add(n737, zn_splat(P8::from_raw(-196608i32)));
    let n1566: ZB = zn_tile_flag_at(g.cache, g.cart, n1565, n1538, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1567: ZB = zb_not(n1566);
    let n1568: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n737);
    let n1569: ZB = zn_tile_flag_at(g.cache, g.cart, n1568, n1538, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1570: ZB = zb_not(n1569);
    let n1572: ZB = zn_gt(r_c88, zn_splat(P8::from_raw(0i32)));
    let n1573: ZB = zn_le(r_c88, zn_splat(P8::from_raw(0i32)));
    let n1574: ZB = zb_and(n1562, n1572);
    let n1575: ZB = zb_and(n1562, n1573);
    let n1576: ZB = zb_or(n1574, n1575);
    let n1577: ZN = zn_sub(r_c88, zn_splat(P8::from_raw(65536i32)));
    let n1578: ZN = zn_mul(n736, zn_splat(P8::from_raw(231700i32)));
    let n1579: ZN = zn_mul(n736, zn_splat(P8::from_raw(327680i32)));
    let n1583: ZB = zb_and(n1489, n1507);
    let n1584: ZB = zb_and(n1492, n1507);
    let n1585: ZN = zsel_n(n1583, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1586: ZB = zb_or(n1583, n1584);
    let n1587: ZB = zb_and(n1515, n1586);
    let n1588: ZB = zb_and(n1516, n1586);
    let n1589: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n1585);
    let n1590: ZN = zn_max(n1514, n1589);
    let n1591: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1585);
    let n1592: ZN = zn_min(n1514, n1591);
    let n1593: ZN = zsel_n(n1587, n1590, n1592);
    let n1594: ZB = zb_or(n1587, n1588);
    let n1595: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1593);
    let n1596: ZB = zb_not(n1595);
    let n1597: ZB = zb_and(n1594, n1596);
    let n1598: ZB = zb_and(n1594, n1595);
    let n1599: ZB = zb_or(n1597, n1598);
    let n1600: ZB = zb_and(n1534, n1599);
    let n1601: ZB = zb_and(n1533, n1599);
    let n1602: ZB = zb_and(n1540, n1600);
    let n1603: ZB = zb_and(n1539, n1600);
    let n1604: ZB = zb_or(n1602, n1603);
    let n1605: ZB = zb_not(n1602);
    let n1606: ZB = zb_and(n1602, n1604);
    let n1607: ZB = zb_and(n1604, n1605);
    let n1608: ZB = zb_or(n1606, n1607);
    let n1609: ZB = zb_not(n1606);
    let n1610: ZB = zb_or(n1601, n1608);
    let n1611: ZB = zb_and(n1608, n1609);
    let n1612: ZB = zb_not(n1611);
    let n1613: ZB = zb_and(n1610, n1611);
    let n1614: ZB = zb_and(n1610, n1612);
    let n1615: ZB = zb_or(n1613, n1614);
    let n1616: ZB = zb_not(n1613);
    let n1617: ZB = zb_and(n1613, n1615);
    let n1618: ZB = zb_and(n1615, n1616);
    let n1619: ZB = zb_or(n1617, n1618);
    let n1620: ZB = zb_and(n1489, n1619);
    let n1621: ZB = zb_and(n1492, n1619);
    let n1622: ZB = zb_or(n1620, n1621);
    let n1623: ZB = zb_and(n1572, n1622);
    let n1624: ZB = zb_and(n1573, n1622);
    let n1625: ZB = zb_or(n1623, n1624);
    let n1626: ZB = zn_lt(n654, zn_splat(P8::from_raw(-262144i32)));
    let n1627: ZB = zn_ge(n654, zn_splat(P8::from_raw(-262144i32)));
    let n1628: ZB = zb_and(n1576, n1626);
    let n1629: ZB = zb_and(n1576, n1627);
    let n1630: ZB = zb_or(n1628, n1629);
    let n1631: ZB = zb_not(n1628);
    let n1632: ZB = zb_and(n1628, n1630);
    let n1633: ZB = zb_and(n1630, n1631);
    let n1634: ZB = zb_and(n1625, n1626);
    let n1635: ZB = zb_and(n1625, n1627);
    let n1636: ZB = zb_or(n1634, n1635);
    let n1637: ZB = zb_not(n1634);
    let n1638: ZB = zb_and(n1634, n1636);
    let n1640: ZN = zsel_n(n1632, n92, zn_splat(P8::from_raw(983040i32)));
    let n1641: ZB = zb_not(n1632);
    let n1642: ZB = zb_or(n95, n1641);
    let n1643: ZB = zb_or(n1632, n1638);
    let n1644: ZB = zsel_b(n1632, n1463, n1472);
    let n1645: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1500);
    let n1646: ZB = zb_not(n734);
    let n1647: ZB = zb_and(n1530, n1646);
    let n1648: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1523);
    let n1649: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1561);
    let n1650: ZB = zb_or(n734, n1633);
    let n1651: ZB = zsel_b(n734, n660, n1463);
    let n1653: ZN = zsel_n(n56, r_c39, n92);
    let n1654: ZN = zsel_n(n56, n58, r_c20);
    let n1655: ZB = zsel_b(n56, r_c41, n93);
    let n1656: ZB = zsel_b(n56, r_c42, n94);
    let n1657: ZB = zsel_b(n56, r_c38, n95);
    let n1658: ZB = zsel_b(n56, r_c232, n96);
    let n1659: ZN = zsel_n(n56, r_c233, n726);
    let n1660: ZB = zsel_b(n56, r_c242, n98);
    let n1661: ZN = zsel_n(n56, r_c244, n727);
    let n1662: ZN = zsel_n(n56, r_c245, n728);
    let n1663: ZN = zsel_n(n56, r_c248, n653);
    let n1664: ZN = zsel_n(n56, r_c249, n729);
    let n1665: ZB = zsel_b(n56, r_c262, n103);
    let n1666: ZB = zsel_b(n56, r_c263, n104);
    let n1667: ZN = zsel_n(n56, zn_splat(u.c264), n105);
    let n1668: ZN = zsel_n(n56, zn_splat(u.c265), n106);
    let n1669: ZN = zsel_n(n56, zn_splat(u.c266), n107);
    let n1670: ZN = zsel_n(n56, zn_splat(u.c267), n108);
    let n1671: ZN = zsel_n(n56, r_c268, n655);
    let n1672: ZN = zsel_n(n56, r_c269, n656);
    let n1673: ZN = zsel_n(n56, r_c270, n730);
    let n1674: ZN = zsel_n(n56, r_c271, n731);
    let n1675: ZN = zsel_n(n56, r_c272, n113);
    let n1676: ZN = zsel_n(n56, r_c273, n114);
    let n1677: ZB = zb_or(n56, n732);
    let n1678: ZB = zb_or(n56, n660);
    let n1680: ZB = zb_and(n57, n1650);
    let n1681: ZB = zn_lt(n653, zn_splat(P8::from_raw(-65536i32)));
    let n1682: ZB = zn_ge(n653, zn_splat(P8::from_raw(-65536i32)));
    let n1683: ZB = zb_and(n1680, n1682);
    let n1684: ZB = zb_and(n1680, n1681);
    let n1686: ZB = zn_gt(n653, zn_splat(P8::from_raw(7929856i32)));
    let n1687: ZB = zb_or(n1683, n1684);
    let n1688: ZB = zb_not(n1683);
    let n1689: ZB = zb_or(n1686, n1688);
    let n1690: ZB = zb_not(n1689);
    let n1691: ZB = zb_and(n1687, n1689);
    let n1692: ZB = zb_and(n1687, n1690);
    let n1693: ZN = zn_min(n653, zn_splat(P8::from_raw(7929856i32)));
    let n1694: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1693);
    let n1695: ZN = zsel_n(n1691, n1694, n653);
    let n1696: ZN = zsel_n(n1691, zn_splat(P8::from_raw(0i32)), n1648);
    let n1697: ZB = zb_or(n1691, n1692);
    let n1699: ZB = zn_gt(n1654, zn_splat(P8::from_raw(0i32)));
    let n1700: ZB = zn_le(n1654, zn_splat(P8::from_raw(0i32)));
    let n1701: ZB = zb_and(n1677, n1699);
    let n1702: ZB = zb_and(n1677, n1700);
    let n1703: ZB = zb_or(n1701, n1702);
    let n1707: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1519);
    let n1708: ZN = zsel_n(n1513, n1707, zn_splat(P8::from_raw(-65536i32)));
    let n1709: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n737);
    let n1710: ZB = zn_tile_flag_at(g.cache, g.cart, n1709, n1538, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1711: ZB = zb_not(n1710);
    let n1712: ZB = zb_and(n1513, n1711);
    let n1713: ZB = zb_and(n1513, n1710);
    let n1714: ZB = zb_or(n1712, n1713);
    let n1715: ZB = zb_not(n1712);
    let n1716: ZB = zb_and(n1712, n1714);
    let n1717: ZB = zb_and(n1714, n1715);
    let n1718: ZB = zb_or(n1716, n1717);
    let n1719: ZB = zb_not(n1716);
    let n1720: ZB = zb_and(n1718, n1719);
    let n1721: ZB = zb_not(n1720);
    let n1722: ZB = zb_and(n1718, n1720);
    let n1723: ZB = zb_and(n1718, n1721);
    let n1724: ZB = zb_or(n1722, n1723);
    let n1725: ZB = zb_not(n1722);
    let n1726: ZB = zb_and(n1722, n1724);
    let n1727: ZB = zb_and(n1724, n1725);
    let n1728: ZB = zb_or(n1726, n1727);
    let n1729: ZB = zb_and(n1481, n1728);
    let n1730: ZB = zb_and(n1484, n1728);
    let n1731: ZN = zsel_n(n1729, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n1732: ZB = zb_or(n1729, n1730);
    let n1733: ZB = zb_and(n1572, n1732);
    let n1734: ZB = zb_and(n1573, n1732);
    let n1735: ZB = zb_or(n1733, n1734);
    let n1740: ZB = zb_and(n1586, n1711);
    let n1741: ZB = zb_and(n1586, n1710);
    let n1742: ZB = zb_or(n1740, n1741);
    let n1743: ZB = zb_not(n1740);
    let n1744: ZB = zb_and(n1740, n1742);
    let n1745: ZB = zb_and(n1742, n1743);
    let n1746: ZB = zb_or(n1744, n1745);
    let n1747: ZB = zb_not(n1744);
    let n1748: ZB = zb_and(n1746, n1747);
    let n1749: ZB = zb_not(n1748);
    let n1750: ZB = zb_and(n1746, n1748);
    let n1751: ZB = zb_and(n1746, n1749);
    let n1752: ZB = zb_or(n1750, n1751);
    let n1753: ZB = zb_not(n1750);
    let n1754: ZB = zb_and(n1750, n1752);
    let n1755: ZB = zb_and(n1752, n1753);
    let n1756: ZB = zb_or(n1754, n1755);
    let n1757: ZB = zb_and(n1489, n1756);
    let n1758: ZB = zb_and(n1492, n1756);
    let n1759: ZB = zb_or(n1757, n1758);
    let n1760: ZB = zb_and(n1572, n1759);
    let n1761: ZB = zb_and(n1573, n1759);
    let n1762: ZB = zb_or(n1760, n1761);
    let n1763: ZB = zb_and(n1626, n1735);
    let n1764: ZB = zb_and(n1627, n1735);
    let n1765: ZB = zb_or(n1763, n1764);
    let n1766: ZB = zb_not(n1763);
    let n1767: ZB = zb_and(n1763, n1765);
    let n1768: ZB = zb_and(n1765, n1766);
    let n1769: ZB = zb_and(n1626, n1762);
    let n1770: ZB = zb_and(n1627, n1762);
    let n1771: ZB = zb_or(n1769, n1770);
    let n1772: ZB = zb_not(n1769);
    let n1773: ZB = zb_and(n1769, n1771);
    let n1775: ZN = zsel_n(n1767, n92, zn_splat(P8::from_raw(983040i32)));
    let n1776: ZB = zb_not(n1767);
    let n1777: ZB = zb_or(n95, n1776);
    let n1778: ZB = zb_or(n1767, n1773);
    let n1779: ZB = zsel_b(n1767, n1463, n1472);
    let n1780: ZB = zb_and(n1513, n1646);
    let n1781: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1708);
    let n1782: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1731);
    let n1783: ZB = zb_or(n734, n1768);
    let n1785: ZB = zb_and(n57, n1783);
    let n1786: ZB = zb_and(n1682, n1785);
    let n1787: ZB = zb_and(n1681, n1785);
    let n1788: ZB = zb_or(n1786, n1787);
    let n1789: ZB = zb_not(n1786);
    let n1790: ZB = zb_or(n1686, n1789);
    let n1791: ZB = zb_not(n1790);
    let n1792: ZB = zb_and(n1788, n1790);
    let n1793: ZB = zb_and(n1788, n1791);
    let n1794: ZN = zsel_n(n1792, n1694, n653);
    let n1795: ZN = zsel_n(n1792, zn_splat(P8::from_raw(0i32)), n1781);
    let n1796: ZB = zb_or(n1792, n1793);
    let n1798: ZN = zsel_n(n735, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(-65536i32)));
    let n1799: ZN = zn_mul(n1798, zn_splat(P8::from_raw(65536i32)));
    let n1800: ZB = zn_gt(zn_splat(P8::from_raw(0i32)), n1799);
    let n1801: ZB = zn_le(zn_splat(P8::from_raw(0i32)), n1799);
    let n1802: ZB = zb_and(n1513, n1800);
    let n1803: ZB = zb_and(n1513, n1801);
    let n1804: ZN = zn_max(n1519, n1799);
    let n1805: ZN = zn_min(n1521, n1799);
    let n1806: ZN = zsel_n(n1802, n1804, n1805);
    let n1807: ZB = zb_or(n1802, n1803);
    let n1808: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1806);
    let n1809: ZB = zb_not(n1808);
    let n1810: ZB = zb_and(n1807, n1809);
    let n1811: ZB = zb_and(n1807, n1808);
    let n1812: ZB = zn_lt(n1806, zn_splat(P8::from_raw(0i32)));
    let n1813: ZB = zb_and(n1810, n1812);
    let n1814: ZB = zb_or(n1810, n1811);
    let n1815: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1798);
    let n1816: ZB = zb_not(n1815);
    let n1817: ZB = zb_and(n1814, n1816);
    let n1818: ZB = zb_and(n1814, n1815);
    let n1819: ZN = zn_add(n737, n1798);
    let n1820: ZB = zn_tile_flag_at(g.cache, g.cart, n1819, n1538, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1821: ZB = zb_not(n1820);
    let n1822: ZB = zb_and(n1817, n1821);
    let n1823: ZB = zb_and(n1817, n1820);
    let n1824: ZB = zb_or(n1822, n1823);
    let n1825: ZB = zb_not(n1822);
    let n1826: ZB = zb_and(n1822, n1824);
    let n1827: ZB = zb_and(n1824, n1825);
    let n1828: ZB = zb_or(n1826, n1827);
    let n1829: ZB = zb_not(n1826);
    let n1830: ZB = zb_or(n1818, n1828);
    let n1831: ZB = zb_and(n1828, n1829);
    let n1832: ZB = zb_not(n1831);
    let n1833: ZB = zb_and(n1830, n1831);
    let n1834: ZB = zb_and(n1830, n1832);
    let n1835: ZB = zb_or(n1833, n1834);
    let n1836: ZB = zb_not(n1833);
    let n1837: ZB = zb_and(n1833, n1835);
    let n1838: ZB = zb_and(n1835, n1836);
    let n1839: ZB = zb_or(n1837, n1838);
    let n1840: ZB = zb_and(n1481, n1839);
    let n1841: ZB = zb_and(n1484, n1839);
    let n1842: ZN = zsel_n(n1840, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n1843: ZB = zb_or(n1840, n1841);
    let n1844: ZB = zb_and(n1572, n1843);
    let n1845: ZB = zb_and(n1573, n1843);
    let n1846: ZB = zb_or(n1844, n1845);
    let n1847: ZN = zn_mul(n1798, zn_splat(P8::from_raw(231700i32)));
    let n1848: ZN = zn_mul(n1798, zn_splat(P8::from_raw(327680i32)));
    let n1849: ZB = zb_and(n1586, n1800);
    let n1850: ZB = zb_and(n1586, n1801);
    let n1851: ZN = zn_max(n1589, n1799);
    let n1852: ZN = zn_min(n1591, n1799);
    let n1853: ZN = zsel_n(n1849, n1851, n1852);
    let n1854: ZB = zb_or(n1849, n1850);
    let n1855: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1853);
    let n1856: ZB = zb_not(n1855);
    let n1857: ZB = zb_and(n1854, n1856);
    let n1858: ZB = zb_and(n1854, n1855);
    let n1859: ZB = zb_or(n1857, n1858);
    let n1860: ZB = zb_and(n1816, n1859);
    let n1861: ZB = zb_and(n1815, n1859);
    let n1862: ZB = zb_and(n1821, n1860);
    let n1863: ZB = zb_and(n1820, n1860);
    let n1864: ZB = zb_or(n1862, n1863);
    let n1865: ZB = zb_not(n1862);
    let n1866: ZB = zb_and(n1862, n1864);
    let n1867: ZB = zb_and(n1864, n1865);
    let n1868: ZB = zb_or(n1866, n1867);
    let n1869: ZB = zb_not(n1866);
    let n1870: ZB = zb_or(n1861, n1868);
    let n1871: ZB = zb_and(n1868, n1869);
    let n1872: ZB = zb_not(n1871);
    let n1873: ZB = zb_and(n1870, n1871);
    let n1874: ZB = zb_and(n1870, n1872);
    let n1875: ZB = zb_or(n1873, n1874);
    let n1876: ZB = zb_not(n1873);
    let n1877: ZB = zb_and(n1873, n1875);
    let n1878: ZB = zb_and(n1875, n1876);
    let n1879: ZB = zb_or(n1877, n1878);
    let n1880: ZB = zb_and(n1489, n1879);
    let n1881: ZB = zb_and(n1492, n1879);
    let n1882: ZB = zb_or(n1880, n1881);
    let n1883: ZB = zb_and(n1572, n1882);
    let n1884: ZB = zb_and(n1573, n1882);
    let n1885: ZB = zb_or(n1883, n1884);
    let n1886: ZB = zb_and(n1626, n1846);
    let n1887: ZB = zb_and(n1627, n1846);
    let n1888: ZB = zb_or(n1886, n1887);
    let n1889: ZB = zb_not(n1886);
    let n1890: ZB = zb_and(n1886, n1888);
    let n1891: ZB = zb_and(n1888, n1889);
    let n1892: ZB = zb_and(n1626, n1885);
    let n1893: ZB = zb_and(n1627, n1885);
    let n1894: ZB = zb_or(n1892, n1893);
    let n1895: ZB = zb_not(n1892);
    let n1896: ZB = zb_and(n1892, n1894);
    let n1898: ZN = zsel_n(n1890, n92, zn_splat(P8::from_raw(983040i32)));
    let n1899: ZB = zb_not(n1890);
    let n1900: ZB = zb_or(n95, n1899);
    let n1901: ZB = zb_or(n1890, n1896);
    let n1902: ZB = zsel_b(n1890, n1463, n1472);
    let n1903: ZB = zb_and(n1646, n1813);
    let n1904: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1806);
    let n1905: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1842);
    let n1906: ZB = zb_or(n734, n1891);
    let n1908: ZB = zb_and(n57, n1906);
    let n1909: ZB = zb_and(n1682, n1908);
    let n1910: ZB = zb_and(n1681, n1908);
    let n1911: ZB = zb_or(n1909, n1910);
    let n1912: ZB = zb_not(n1909);
    let n1913: ZB = zb_or(n1686, n1912);
    let n1914: ZB = zb_not(n1913);
    let n1915: ZB = zb_and(n1911, n1913);
    let n1916: ZB = zb_and(n1911, n1914);
    let n1917: ZN = zsel_n(n1915, n1694, n653);
    let n1918: ZN = zsel_n(n1915, zn_splat(P8::from_raw(0i32)), n1904);
    let n1919: ZB = zb_or(n1915, n1916);
    let n1921: ZN = zsel_n(n1506, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n1922: ZB = zb_and(n1483, n1562);
    let n1923: ZB = zn_gt(n1500, zn_splat(P8::from_raw(0i32)));
    let n1924: ZB = zn_le(n1500, zn_splat(P8::from_raw(0i32)));
    let n1925: ZB = zb_and(n1922, n1923);
    let n1926: ZB = zb_and(n1922, n1924);
    let n1927: ZB = zb_and(n1567, n1926);
    let n1928: ZB = zb_and(n1566, n1926);
    let n1929: ZB = zb_or(n1927, n1928);
    let n1930: ZB = zb_not(n1927);
    let n1931: ZB = zb_and(n1927, n1929);
    let n1932: ZB = zb_and(n1929, n1930);
    let n1933: ZB = zb_or(n1931, n1932);
    let n1934: ZB = zb_not(n1931);
    let n1935: ZB = zb_and(n1933, n1934);
    let n1936: ZB = zb_and(n1931, n1933);
    let n1937: ZB = zb_and(n1570, n1936);
    let n1938: ZB = zb_and(n1569, n1936);
    let n1939: ZB = zb_or(n1937, n1938);
    let n1940: ZB = zb_not(n1937);
    let n1941: ZB = zb_and(n1937, n1939);
    let n1942: ZB = zb_and(n1939, n1940);
    let n1943: ZB = zb_or(n1941, n1942);
    let n1944: ZB = zb_not(n1941);
    let n1945: ZB = zb_and(n1943, n1944);
    let n1946: ZB = zb_and(n1941, n1943);
    let n1947: ZN = zsel_n(n1935, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1948: ZB = zb_or(n1935, n1945);
    let n1949: ZN = zsel_n(n1946, zn_splat(P8::from_raw(0i32)), n1947);
    let n1950: ZB = zb_or(n1946, n1948);
    let n1951: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1949);
    let n1952: ZB = zb_not(n1951);
    let n1953: ZB = zb_and(n1950, n1952);
    let n1954: ZB = zb_and(n1950, n1951);
    let n1955: ZN = zn_neg(n1949);
    let n1956: ZN = zn_mul(n1955, zn_splat(P8::from_raw(131072i32)));
    let n1957: ZN = zsel_n(n1953, n1956, n1523);
    let n1958: ZN = zsel_n(n1953, zn_splat(P8::from_raw(-131072i32)), n1561);
    let n1959: ZB = zb_or(n1953, n1954);
    let n1960: ZN = zsel_n(n1925, zn_splat(P8::from_raw(0i32)), n1500);
    let n1961: ZN = zsel_n(n1925, n1523, n1957);
    let n1962: ZN = zsel_n(n1925, zn_splat(P8::from_raw(-131072i32)), n1958);
    let n1963: ZB = zb_or(n1925, n1959);
    let n1964: ZN = zsel_n(n1963, n1960, n1500);
    let n1965: ZN = zsel_n(n1963, n1961, n1523);
    let n1966: ZN = zsel_n(n1963, n1962, n1561);
    let n1967: ZB = zb_and(n1572, n1963);
    let n1968: ZB = zb_and(n1573, n1963);
    let n1969: ZB = zb_or(n1967, n1968);
    let n1970: ZB = zb_not(n1530);
    let n1971: ZB = zn_lt(n1593, zn_splat(P8::from_raw(0i32)));
    let n1972: ZB = zb_and(n1597, n1971);
    let n1973: ZB = zb_and(n1491, n1622);
    let n1974: ZB = zn_gt(n1921, zn_splat(P8::from_raw(0i32)));
    let n1975: ZB = zn_le(n1921, zn_splat(P8::from_raw(0i32)));
    let n1976: ZB = zb_and(n1973, n1974);
    let n1977: ZB = zb_and(n1973, n1975);
    let n1978: ZB = zb_and(n1567, n1977);
    let n1979: ZB = zb_and(n1566, n1977);
    let n1980: ZB = zb_or(n1978, n1979);
    let n1981: ZB = zb_not(n1978);
    let n1982: ZB = zb_and(n1978, n1980);
    let n1983: ZB = zb_and(n1980, n1981);
    let n1984: ZB = zb_or(n1982, n1983);
    let n1985: ZB = zb_not(n1982);
    let n1986: ZB = zb_and(n1984, n1985);
    let n1987: ZB = zb_and(n1982, n1984);
    let n1988: ZB = zb_and(n1570, n1987);
    let n1989: ZB = zb_and(n1569, n1987);
    let n1990: ZB = zb_or(n1988, n1989);
    let n1991: ZB = zb_not(n1988);
    let n1992: ZB = zb_and(n1988, n1990);
    let n1993: ZB = zb_and(n1990, n1991);
    let n1994: ZB = zb_or(n1992, n1993);
    let n1995: ZB = zb_not(n1992);
    let n1996: ZB = zb_and(n1994, n1995);
    let n1997: ZB = zb_and(n1992, n1994);
    let n1998: ZN = zsel_n(n1986, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1999: ZB = zb_or(n1986, n1996);
    let n2000: ZN = zsel_n(n1997, zn_splat(P8::from_raw(0i32)), n1998);
    let n2001: ZB = zb_or(n1997, n1999);
    let n2002: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2000);
    let n2003: ZB = zb_not(n2002);
    let n2004: ZB = zb_and(n2001, n2003);
    let n2005: ZB = zb_and(n2001, n2002);
    let n2006: ZB = zb_or(n2004, n2005);
    let n2007: ZB = zb_or(n1976, n2006);
    let n2008: ZB = zb_and(n1572, n2007);
    let n2009: ZB = zb_and(n1573, n2007);
    let n2010: ZB = zb_or(n2008, n2009);
    let n2011: ZB = zb_not(n1972);
    let n2012: ZB = zb_and(n1626, n1969);
    let n2013: ZB = zb_and(n1627, n1969);
    let n2014: ZB = zb_or(n2012, n2013);
    let n2015: ZB = zb_not(n2012);
    let n2016: ZB = zb_and(n2012, n2014);
    let n2017: ZB = zb_and(n2014, n2015);
    let n2018: ZB = zb_and(n1626, n2010);
    let n2019: ZB = zb_and(n1627, n2010);
    let n2020: ZB = zb_or(n2018, n2019);
    let n2021: ZB = zb_not(n2018);
    let n2022: ZB = zb_and(n2018, n2020);
    let n2024: ZN = zsel_n(n2016, n92, zn_splat(P8::from_raw(983040i32)));
    let n2025: ZB = zb_not(n2016);
    let n2026: ZB = zb_or(n95, n2025);
    let n2027: ZB = zb_or(n2016, n2022);
    let n2028: ZB = zsel_b(n2016, n1463, n1472);
    let n2029: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1964);
    let n2030: ZB = zb_and(n1483, n1646);
    let n2031: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1965);
    let n2032: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n1966);
    let n2033: ZB = zb_or(n734, n2017);
    let n2035: ZB = zb_and(n57, n2033);
    let n2036: ZB = zb_and(n1682, n2035);
    let n2037: ZB = zb_and(n1681, n2035);
    let n2038: ZB = zb_or(n2036, n2037);
    let n2039: ZB = zb_not(n2036);
    let n2040: ZB = zb_or(n1686, n2039);
    let n2041: ZB = zb_not(n2040);
    let n2042: ZB = zb_and(n2038, n2040);
    let n2043: ZB = zb_and(n2038, n2041);
    let n2044: ZN = zsel_n(n2042, n1694, n653);
    let n2045: ZN = zsel_n(n2042, zn_splat(P8::from_raw(0i32)), n2031);
    let n2046: ZB = zb_or(n2042, n2043);
    let n2048: ZB = zb_and(n1483, n1732);
    let n2049: ZB = zb_and(n1923, n2048);
    let n2050: ZB = zb_and(n1924, n2048);
    let n2051: ZB = zb_and(n1567, n2050);
    let n2052: ZB = zb_and(n1566, n2050);
    let n2053: ZB = zb_or(n2051, n2052);
    let n2054: ZB = zb_not(n2051);
    let n2055: ZB = zb_and(n2051, n2053);
    let n2056: ZB = zb_and(n2053, n2054);
    let n2057: ZB = zb_or(n2055, n2056);
    let n2058: ZB = zb_not(n2055);
    let n2059: ZB = zb_and(n2057, n2058);
    let n2060: ZB = zb_and(n2055, n2057);
    let n2061: ZB = zb_and(n1570, n2060);
    let n2062: ZB = zb_and(n1569, n2060);
    let n2063: ZB = zb_or(n2061, n2062);
    let n2064: ZB = zb_not(n2061);
    let n2065: ZB = zb_and(n2061, n2063);
    let n2066: ZB = zb_and(n2063, n2064);
    let n2067: ZB = zb_or(n2065, n2066);
    let n2068: ZB = zb_not(n2065);
    let n2069: ZB = zb_and(n2067, n2068);
    let n2070: ZB = zb_and(n2065, n2067);
    let n2071: ZN = zsel_n(n2059, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2072: ZB = zb_or(n2059, n2069);
    let n2073: ZN = zsel_n(n2070, zn_splat(P8::from_raw(0i32)), n2071);
    let n2074: ZB = zb_or(n2070, n2072);
    let n2075: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2073);
    let n2076: ZB = zb_not(n2075);
    let n2077: ZB = zb_and(n2074, n2076);
    let n2078: ZB = zb_and(n2074, n2075);
    let n2079: ZN = zn_neg(n2073);
    let n2080: ZN = zn_mul(n2079, zn_splat(P8::from_raw(131072i32)));
    let n2081: ZN = zsel_n(n2077, n2080, n1708);
    let n2082: ZN = zsel_n(n2077, zn_splat(P8::from_raw(-131072i32)), n1731);
    let n2083: ZB = zb_or(n2077, n2078);
    let n2084: ZN = zsel_n(n2049, zn_splat(P8::from_raw(0i32)), n1500);
    let n2085: ZN = zsel_n(n2049, n1708, n2081);
    let n2086: ZN = zsel_n(n2049, zn_splat(P8::from_raw(-131072i32)), n2082);
    let n2087: ZB = zb_or(n2049, n2083);
    let n2088: ZN = zsel_n(n2087, n2084, n1500);
    let n2089: ZN = zsel_n(n2087, n2085, n1708);
    let n2090: ZN = zsel_n(n2087, n2086, n1731);
    let n2091: ZB = zb_and(n1572, n2087);
    let n2092: ZB = zb_and(n1573, n2087);
    let n2093: ZB = zb_or(n2091, n2092);
    let n2095: ZB = zb_and(n1491, n1759);
    let n2096: ZB = zb_and(n1974, n2095);
    let n2097: ZB = zb_and(n1975, n2095);
    let n2098: ZB = zb_and(n1567, n2097);
    let n2099: ZB = zb_and(n1566, n2097);
    let n2100: ZB = zb_or(n2098, n2099);
    let n2101: ZB = zb_not(n2098);
    let n2102: ZB = zb_and(n2098, n2100);
    let n2103: ZB = zb_and(n2100, n2101);
    let n2104: ZB = zb_or(n2102, n2103);
    let n2105: ZB = zb_not(n2102);
    let n2106: ZB = zb_and(n2104, n2105);
    let n2107: ZB = zb_and(n2102, n2104);
    let n2108: ZB = zb_and(n1570, n2107);
    let n2109: ZB = zb_and(n1569, n2107);
    let n2110: ZB = zb_or(n2108, n2109);
    let n2111: ZB = zb_not(n2108);
    let n2112: ZB = zb_and(n2108, n2110);
    let n2113: ZB = zb_and(n2110, n2111);
    let n2114: ZB = zb_or(n2112, n2113);
    let n2115: ZB = zb_not(n2112);
    let n2116: ZB = zb_and(n2114, n2115);
    let n2117: ZB = zb_and(n2112, n2114);
    let n2118: ZN = zsel_n(n2106, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2119: ZB = zb_or(n2106, n2116);
    let n2120: ZN = zsel_n(n2117, zn_splat(P8::from_raw(0i32)), n2118);
    let n2121: ZB = zb_or(n2117, n2119);
    let n2122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2120);
    let n2123: ZB = zb_not(n2122);
    let n2124: ZB = zb_and(n2121, n2123);
    let n2125: ZB = zb_and(n2121, n2122);
    let n2126: ZB = zb_or(n2124, n2125);
    let n2127: ZB = zb_or(n2096, n2126);
    let n2128: ZB = zb_and(n1572, n2127);
    let n2129: ZB = zb_and(n1573, n2127);
    let n2130: ZB = zb_or(n2128, n2129);
    let n2132: ZB = zb_and(n1626, n2093);
    let n2133: ZB = zb_and(n1627, n2093);
    let n2134: ZB = zb_or(n2132, n2133);
    let n2135: ZB = zb_not(n2132);
    let n2136: ZB = zb_and(n2132, n2134);
    let n2137: ZB = zb_and(n2134, n2135);
    let n2138: ZB = zb_and(n1626, n2130);
    let n2139: ZB = zb_and(n1627, n2130);
    let n2140: ZB = zb_or(n2138, n2139);
    let n2141: ZB = zb_not(n2138);
    let n2142: ZB = zb_and(n2138, n2140);
    let n2144: ZN = zsel_n(n2136, n92, zn_splat(P8::from_raw(983040i32)));
    let n2145: ZB = zb_not(n2136);
    let n2146: ZB = zb_or(n95, n2145);
    let n2147: ZB = zb_or(n2136, n2142);
    let n2148: ZB = zsel_b(n2136, n1463, n1472);
    let n2149: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2088);
    let n2150: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2089);
    let n2151: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2090);
    let n2152: ZB = zb_or(n734, n2137);
    let n2154: ZB = zb_and(n57, n2152);
    let n2155: ZB = zb_and(n1682, n2154);
    let n2156: ZB = zb_and(n1681, n2154);
    let n2157: ZB = zb_or(n2155, n2156);
    let n2158: ZB = zb_not(n2155);
    let n2159: ZB = zb_or(n1686, n2158);
    let n2160: ZB = zb_not(n2159);
    let n2161: ZB = zb_and(n2157, n2159);
    let n2162: ZB = zb_and(n2157, n2160);
    let n2163: ZN = zsel_n(n2161, n1694, n653);
    let n2164: ZN = zsel_n(n2161, zn_splat(P8::from_raw(0i32)), n2150);
    let n2165: ZB = zb_or(n2161, n2162);
    let n2167: ZB = zb_and(n1483, n1843);
    let n2168: ZB = zb_and(n1923, n2167);
    let n2169: ZB = zb_and(n1924, n2167);
    let n2170: ZB = zb_and(n1567, n2169);
    let n2171: ZB = zb_and(n1566, n2169);
    let n2172: ZB = zb_or(n2170, n2171);
    let n2173: ZB = zb_not(n2170);
    let n2174: ZB = zb_and(n2170, n2172);
    let n2175: ZB = zb_and(n2172, n2173);
    let n2176: ZB = zb_or(n2174, n2175);
    let n2177: ZB = zb_not(n2174);
    let n2178: ZB = zb_and(n2176, n2177);
    let n2179: ZB = zb_and(n2174, n2176);
    let n2180: ZB = zb_and(n1570, n2179);
    let n2181: ZB = zb_and(n1569, n2179);
    let n2182: ZB = zb_or(n2180, n2181);
    let n2183: ZB = zb_not(n2180);
    let n2184: ZB = zb_and(n2180, n2182);
    let n2185: ZB = zb_and(n2182, n2183);
    let n2186: ZB = zb_or(n2184, n2185);
    let n2187: ZB = zb_not(n2184);
    let n2188: ZB = zb_and(n2186, n2187);
    let n2189: ZB = zb_and(n2184, n2186);
    let n2190: ZN = zsel_n(n2178, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2191: ZB = zb_or(n2178, n2188);
    let n2192: ZN = zsel_n(n2189, zn_splat(P8::from_raw(0i32)), n2190);
    let n2193: ZB = zb_or(n2189, n2191);
    let n2194: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2192);
    let n2195: ZB = zb_not(n2194);
    let n2196: ZB = zb_and(n2193, n2195);
    let n2197: ZB = zb_and(n2193, n2194);
    let n2198: ZN = zn_neg(n2192);
    let n2199: ZN = zn_mul(n2198, zn_splat(P8::from_raw(131072i32)));
    let n2200: ZN = zsel_n(n2196, n2199, n1806);
    let n2201: ZN = zsel_n(n2196, zn_splat(P8::from_raw(-131072i32)), n1842);
    let n2202: ZB = zb_or(n2196, n2197);
    let n2203: ZN = zsel_n(n2168, zn_splat(P8::from_raw(0i32)), n1500);
    let n2204: ZN = zsel_n(n2168, n1806, n2200);
    let n2205: ZN = zsel_n(n2168, zn_splat(P8::from_raw(-131072i32)), n2201);
    let n2206: ZB = zb_or(n2168, n2202);
    let n2207: ZN = zsel_n(n2206, n2203, n1500);
    let n2208: ZN = zsel_n(n2206, n2204, n1806);
    let n2209: ZN = zsel_n(n2206, n2205, n1842);
    let n2210: ZB = zb_and(n1572, n2206);
    let n2211: ZB = zb_and(n1573, n2206);
    let n2212: ZB = zb_or(n2210, n2211);
    let n2213: ZB = zb_not(n1813);
    let n2214: ZB = zn_lt(n1853, zn_splat(P8::from_raw(0i32)));
    let n2215: ZB = zb_and(n1857, n2214);
    let n2216: ZB = zb_and(n1491, n1882);
    let n2217: ZB = zb_and(n1974, n2216);
    let n2218: ZB = zb_and(n1975, n2216);
    let n2219: ZB = zb_and(n1567, n2218);
    let n2220: ZB = zb_and(n1566, n2218);
    let n2221: ZB = zb_or(n2219, n2220);
    let n2222: ZB = zb_not(n2219);
    let n2223: ZB = zb_and(n2219, n2221);
    let n2224: ZB = zb_and(n2221, n2222);
    let n2225: ZB = zb_or(n2223, n2224);
    let n2226: ZB = zb_not(n2223);
    let n2227: ZB = zb_and(n2225, n2226);
    let n2228: ZB = zb_and(n2223, n2225);
    let n2229: ZB = zb_and(n1570, n2228);
    let n2230: ZB = zb_and(n1569, n2228);
    let n2231: ZB = zb_or(n2229, n2230);
    let n2232: ZB = zb_not(n2229);
    let n2233: ZB = zb_and(n2229, n2231);
    let n2234: ZB = zb_and(n2231, n2232);
    let n2235: ZB = zb_or(n2233, n2234);
    let n2236: ZB = zb_not(n2233);
    let n2237: ZB = zb_and(n2235, n2236);
    let n2238: ZB = zb_and(n2233, n2235);
    let n2239: ZN = zsel_n(n2227, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2240: ZB = zb_or(n2227, n2237);
    let n2241: ZN = zsel_n(n2238, zn_splat(P8::from_raw(0i32)), n2239);
    let n2242: ZB = zb_or(n2238, n2240);
    let n2243: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2241);
    let n2244: ZB = zb_not(n2243);
    let n2245: ZB = zb_and(n2242, n2244);
    let n2246: ZB = zb_and(n2242, n2243);
    let n2247: ZB = zb_or(n2245, n2246);
    let n2248: ZB = zb_or(n2217, n2247);
    let n2249: ZB = zb_and(n1572, n2248);
    let n2250: ZB = zb_and(n1573, n2248);
    let n2251: ZB = zb_or(n2249, n2250);
    let n2252: ZB = zb_not(n2215);
    let n2253: ZB = zb_and(n1626, n2212);
    let n2254: ZB = zb_and(n1627, n2212);
    let n2255: ZB = zb_or(n2253, n2254);
    let n2256: ZB = zb_not(n2253);
    let n2257: ZB = zb_and(n2253, n2255);
    let n2258: ZB = zb_and(n2255, n2256);
    let n2259: ZB = zb_and(n1626, n2251);
    let n2260: ZB = zb_and(n1627, n2251);
    let n2261: ZB = zb_or(n2259, n2260);
    let n2262: ZB = zb_not(n2259);
    let n2263: ZB = zb_and(n2259, n2261);
    let n2265: ZN = zsel_n(n2257, n92, zn_splat(P8::from_raw(983040i32)));
    let n2266: ZB = zb_not(n2257);
    let n2267: ZB = zb_or(n95, n2266);
    let n2268: ZB = zb_or(n2257, n2263);
    let n2269: ZB = zsel_b(n2257, n1463, n1472);
    let n2270: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2207);
    let n2271: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2208);
    let n2272: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2209);
    let n2273: ZB = zb_or(n734, n2258);
    let n2275: ZB = zb_and(n57, n2273);
    let n2276: ZB = zb_and(n1682, n2275);
    let n2277: ZB = zb_and(n1681, n2275);
    let n2278: ZB = zb_or(n2276, n2277);
    let n2279: ZB = zb_not(n2276);
    let n2280: ZB = zb_or(n1686, n2279);
    let n2281: ZB = zb_not(n2280);
    let n2282: ZB = zb_and(n2278, n2280);
    let n2283: ZB = zb_and(n2278, n2281);
    let n2284: ZN = zsel_n(n2282, n1694, n653);
    let n2285: ZN = zsel_n(n2282, zn_splat(P8::from_raw(0i32)), n2271);
    let n2286: ZB = zb_or(n2282, n2283);
    let n2288: ZB = zb_and(n1483, n1574);
    let n2289: ZB = zb_not(n2288);
    let n2290: ZB = zb_and(n1576, n2288);
    let n2291: ZB = zb_and(n1576, n2289);
    let n2292: ZN = zsel_n(n2290, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2293: ZB = zb_and(n1534, n2290);
    let n2294: ZB = zb_and(n1533, n2290);
    let n2295: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2292);
    let n2296: ZB = zb_not(n2295);
    let n2297: ZB = zb_and(n2293, n2296);
    let n2298: ZB = zb_and(n2293, n2295);
    let n2299: ZN = zn_mul(n2292, zn_splat(P8::from_raw(231700i32)));
    let n2300: ZN = zsel_n(n2297, n1578, n1579);
    let n2301: ZN = zsel_n(n2297, n2299, zn_splat(P8::from_raw(0i32)));
    let n2302: ZB = zb_or(n2297, n2298);
    let n2303: ZB = zb_and(n2294, n2296);
    let n2304: ZB = zb_and(n2294, n2295);
    let n2305: ZN = zn_mul(n2292, zn_splat(P8::from_raw(327680i32)));
    let n2306: ZB = zb_and(n1530, n2304);
    let n2307: ZB = zb_and(n1970, n2304);
    let n2308: ZN = zsel_n(n2306, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2309: ZB = zb_or(n2306, n2307);
    let n2310: ZN = zsel_n(n2303, zn_splat(P8::from_raw(0i32)), n2308);
    let n2311: ZN = zsel_n(n2303, n2305, zn_splat(P8::from_raw(0i32)));
    let n2312: ZB = zb_or(n2303, n2309);
    let n2313: ZN = zsel_n(n2302, n2300, n2310);
    let n2314: ZN = zsel_n(n2302, n2301, n2311);
    let n2315: ZB = zb_or(n2302, n2312);
    let n2316: ZB = zn_gt(n2313, zn_splat(P8::from_raw(0i32)));
    let n2317: ZB = zn_le(n2313, zn_splat(P8::from_raw(0i32)));
    let n2318: ZB = zb_and(n2315, n2316);
    let n2319: ZB = zb_and(n2315, n2317);
    let n2320: ZB = zn_lt(n2313, zn_splat(P8::from_raw(0i32)));
    let n2321: ZB = zn_ge(n2313, zn_splat(P8::from_raw(0i32)));
    let n2322: ZB = zb_and(n2319, n2320);
    let n2323: ZB = zb_and(n2319, n2321);
    let n2324: ZN = zsel_n(n2318, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2325: ZB = zb_or(n2318, n2322);
    let n2326: ZN = zsel_n(n2323, zn_splat(P8::from_raw(0i32)), n2324);
    let n2327: ZB = zb_or(n2323, n2325);
    let n2328: ZB = zn_gt(n2314, zn_splat(P8::from_raw(0i32)));
    let n2329: ZB = zn_le(n2314, zn_splat(P8::from_raw(0i32)));
    let n2330: ZB = zb_and(n2327, n2328);
    let n2331: ZB = zb_and(n2327, n2329);
    let n2332: ZN = zsel_n(n2330, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2333: ZN = zsel_n(n2331, zn_splat(P8::from_raw(0i32)), n2332);
    let n2334: ZB = zb_or(n2330, n2331);
    let n2335: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2314);
    let n2336: ZB = zb_not(n2335);
    let n2337: ZB = zb_and(n2334, n2336);
    let n2338: ZB = zb_and(n2334, n2335);
    let n2339: ZN = zsel_n(n2337, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2340: ZB = zb_or(n2337, n2338);
    let n2341: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2313);
    let n2342: ZB = zb_not(n2341);
    let n2343: ZB = zb_and(n2340, n2342);
    let n2344: ZB = zb_and(n2340, n2341);
    let n2345: ZN = zsel_n(n2343, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2346: ZB = zb_or(n2343, n2344);
    let n2347: ZN = zsel_n(n2346, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2348: ZB = zb_or(n93, n2346);
    let n2349: ZN = zsel_n(n2346, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n2350: ZN = zsel_n(n2346, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n2351: ZN = zsel_n(n2346, n1577, r_c88);
    let n2352: ZN = zsel_n(n2346, n2339, zn_splat(P8::from_raw(0i32)));
    let n2353: ZN = zsel_n(n2346, n2345, zn_splat(P8::from_raw(0i32)));
    let n2354: ZN = zsel_n(n2346, n2326, zn_splat(P8::from_raw(0i32)));
    let n2355: ZN = zsel_n(n2346, n2333, zn_splat(P8::from_raw(0i32)));
    let n2356: ZN = zsel_n(n2346, n2313, n1523);
    let n2357: ZN = zsel_n(n2346, n2314, n1561);
    let n2358: ZB = zb_or(n2291, n2346);
    let n2359: ZB = zb_and(n1491, n1623);
    let n2360: ZB = zb_not(n2359);
    let n2361: ZB = zb_and(n1625, n2359);
    let n2362: ZB = zb_and(n1625, n2360);
    let n2363: ZN = zsel_n(n2361, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2364: ZB = zb_and(n1534, n2361);
    let n2365: ZB = zb_and(n1533, n2361);
    let n2366: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2363);
    let n2367: ZB = zb_not(n2366);
    let n2368: ZB = zb_and(n2364, n2367);
    let n2369: ZB = zb_and(n2364, n2366);
    let n2370: ZN = zn_mul(n2363, zn_splat(P8::from_raw(231700i32)));
    let n2371: ZN = zsel_n(n2368, n1578, n1579);
    let n2372: ZN = zsel_n(n2368, n2370, zn_splat(P8::from_raw(0i32)));
    let n2373: ZB = zb_or(n2368, n2369);
    let n2374: ZB = zb_and(n2365, n2367);
    let n2375: ZB = zb_and(n2365, n2366);
    let n2376: ZN = zn_mul(n2363, zn_splat(P8::from_raw(327680i32)));
    let n2377: ZB = zb_and(n1972, n2375);
    let n2378: ZB = zb_and(n2011, n2375);
    let n2379: ZN = zsel_n(n2377, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2380: ZB = zb_or(n2377, n2378);
    let n2381: ZN = zsel_n(n2374, zn_splat(P8::from_raw(0i32)), n2379);
    let n2382: ZN = zsel_n(n2374, n2376, zn_splat(P8::from_raw(0i32)));
    let n2383: ZB = zb_or(n2374, n2380);
    let n2384: ZN = zsel_n(n2373, n2371, n2381);
    let n2385: ZN = zsel_n(n2373, n2372, n2382);
    let n2386: ZB = zb_or(n2373, n2383);
    let n2387: ZB = zn_gt(n2384, zn_splat(P8::from_raw(0i32)));
    let n2388: ZB = zn_le(n2384, zn_splat(P8::from_raw(0i32)));
    let n2389: ZB = zb_and(n2386, n2387);
    let n2390: ZB = zb_and(n2386, n2388);
    let n2391: ZB = zn_lt(n2384, zn_splat(P8::from_raw(0i32)));
    let n2392: ZB = zn_ge(n2384, zn_splat(P8::from_raw(0i32)));
    let n2393: ZB = zb_and(n2390, n2391);
    let n2394: ZB = zb_and(n2390, n2392);
    let n2395: ZB = zb_or(n2389, n2393);
    let n2396: ZB = zb_or(n2394, n2395);
    let n2397: ZB = zn_gt(n2385, zn_splat(P8::from_raw(0i32)));
    let n2398: ZB = zn_le(n2385, zn_splat(P8::from_raw(0i32)));
    let n2399: ZB = zb_and(n2396, n2397);
    let n2400: ZB = zb_and(n2396, n2398);
    let n2401: ZB = zb_or(n2399, n2400);
    let n2402: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2385);
    let n2403: ZB = zb_not(n2402);
    let n2404: ZB = zb_and(n2401, n2403);
    let n2405: ZB = zb_and(n2401, n2402);
    let n2406: ZB = zb_or(n2404, n2405);
    let n2407: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2384);
    let n2408: ZB = zb_not(n2407);
    let n2409: ZB = zb_and(n2406, n2408);
    let n2410: ZB = zb_and(n2406, n2407);
    let n2411: ZB = zb_or(n2409, n2410);
    let n2412: ZN = zsel_n(n2411, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2413: ZB = zb_or(n93, n2411);
    let n2414: ZB = zb_or(n2362, n2411);
    let n2415: ZB = zb_and(n1626, n2358);
    let n2416: ZB = zb_and(n1627, n2358);
    let n2417: ZB = zb_or(n2415, n2416);
    let n2418: ZB = zb_not(n2415);
    let n2419: ZB = zb_and(n2415, n2417);
    let n2420: ZB = zb_and(n2417, n2418);
    let n2421: ZB = zb_and(n1626, n2414);
    let n2422: ZB = zb_and(n1627, n2414);
    let n2423: ZB = zb_or(n2421, n2422);
    let n2424: ZB = zb_not(n2421);
    let n2425: ZB = zb_and(n2421, n2423);
    let n2426: ZB = zb_and(n2423, n2424);
    let n2427: ZN = zsel_n(n2419, n92, zn_splat(P8::from_raw(983040i32)));
    let n2428: ZN = zsel_n(n2419, n2347, n2412);
    let n2429: ZB = zb_not(n2419);
    let n2430: ZB = zb_or(n95, n2429);
    let n2431: ZB = zb_or(n2419, n2425);
    let n2432: ZB = zsel_b(n2419, n1463, n1472);
    let n2433: ZN = zsel_n(n734, r_c20, n2347);
    let n2434: ZB = zsel_b(n734, n93, n2348);
    let n2435: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2349);
    let n2436: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2350);
    let n2437: ZN = zsel_n(n734, r_c88, n2351);
    let n2438: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2352);
    let n2439: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2353);
    let n2440: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2354);
    let n2441: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2355);
    let n2442: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2356);
    let n2443: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2357);
    let n2444: ZB = zb_or(n734, n2420);
    let n2445: ZB = zn_gt(n2412, zn_splat(P8::from_raw(0i32)));
    let n2446: ZB = zn_le(n2412, zn_splat(P8::from_raw(0i32)));
    let n2447: ZB = zb_and(n2426, n2445);
    let n2448: ZB = zb_and(n2426, n2446);
    let n2449: ZB = zn_gt(n2433, zn_splat(P8::from_raw(0i32)));
    let n2450: ZB = zn_le(n2433, zn_splat(P8::from_raw(0i32)));
    let n2451: ZB = zb_and(n2444, n2449);
    let n2452: ZB = zb_and(n2444, n2450);
    let n2453: ZB = zb_and(n1682, n2452);
    let n2454: ZB = zb_and(n1681, n2452);
    let n2455: ZB = zb_or(n2453, n2454);
    let n2456: ZB = zb_not(n2453);
    let n2457: ZB = zb_or(n1686, n2456);
    let n2458: ZB = zb_not(n2457);
    let n2459: ZB = zb_and(n2455, n2457);
    let n2460: ZB = zb_and(n2455, n2458);
    let n2461: ZN = zsel_n(n2459, n1694, n653);
    let n2462: ZN = zsel_n(n2459, zn_splat(P8::from_raw(0i32)), n2442);
    let n2463: ZB = zb_or(n2459, n2460);
    let n2464: ZB = zn_gt(n2428, zn_splat(P8::from_raw(0i32)));
    let n2465: ZB = zn_le(n2428, zn_splat(P8::from_raw(0i32)));
    let n2466: ZB = zb_and(n2431, n2464);
    let n2467: ZB = zb_and(n2431, n2465);
    let n2468: ZB = zb_or(n2447, n2448);
    let n2469: ZN = zsel_n(n2451, n653, n2461);
    let n2470: ZN = zsel_n(n2451, n2442, n2462);
    let n2471: ZB = zb_or(n2451, n2463);
    let n2472: ZB = zb_or(n2466, n2467);
    let n2473: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2435);
    let n2474: ZB = zb_and(n1483, n1733);
    let n2475: ZB = zb_not(n2474);
    let n2476: ZB = zb_and(n1735, n2474);
    let n2477: ZB = zb_and(n1735, n2475);
    let n2478: ZN = zsel_n(n2476, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2479: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2478);
    let n2480: ZB = zb_not(n2479);
    let n2481: ZB = zb_and(n2476, n2480);
    let n2482: ZB = zb_and(n2476, n2479);
    let n2483: ZN = zn_mul(n2478, zn_splat(P8::from_raw(231700i32)));
    let n2484: ZN = zsel_n(n2481, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n2485: ZN = zsel_n(n2481, n2483, zn_splat(P8::from_raw(0i32)));
    let n2486: ZB = zb_or(n2481, n2482);
    let n2488: ZN = zsel_n(n2486, n2484, zn_splat(P8::from_raw(65536i32)));
    let n2489: ZN = zsel_n(n2486, n2485, zn_splat(P8::from_raw(0i32)));
    let n2490: ZB = zn_gt(n2488, zn_splat(P8::from_raw(0i32)));
    let n2491: ZB = zn_le(n2488, zn_splat(P8::from_raw(0i32)));
    let n2492: ZB = zb_and(n2486, n2490);
    let n2493: ZB = zb_and(n2486, n2491);
    let n2494: ZB = zn_lt(n2488, zn_splat(P8::from_raw(0i32)));
    let n2495: ZB = zn_ge(n2488, zn_splat(P8::from_raw(0i32)));
    let n2496: ZB = zb_and(n2493, n2494);
    let n2497: ZB = zb_and(n2493, n2495);
    let n2498: ZN = zsel_n(n2492, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2499: ZB = zb_or(n2492, n2496);
    let n2500: ZN = zsel_n(n2497, zn_splat(P8::from_raw(0i32)), n2498);
    let n2501: ZB = zb_or(n2497, n2499);
    let n2502: ZB = zn_gt(n2489, zn_splat(P8::from_raw(0i32)));
    let n2503: ZB = zn_le(n2489, zn_splat(P8::from_raw(0i32)));
    let n2504: ZB = zb_and(n2501, n2502);
    let n2505: ZB = zb_and(n2501, n2503);
    let n2506: ZN = zsel_n(n2504, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2507: ZN = zsel_n(n2505, zn_splat(P8::from_raw(0i32)), n2506);
    let n2508: ZB = zb_or(n2504, n2505);
    let n2509: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2489);
    let n2510: ZB = zb_not(n2509);
    let n2511: ZB = zb_and(n2508, n2510);
    let n2512: ZB = zb_and(n2508, n2509);
    let n2513: ZN = zsel_n(n2511, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2514: ZB = zb_or(n2511, n2512);
    let n2515: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2488);
    let n2516: ZB = zb_not(n2515);
    let n2517: ZB = zb_and(n2514, n2516);
    let n2518: ZB = zb_and(n2514, n2515);
    let n2519: ZN = zsel_n(n2517, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2520: ZB = zb_or(n2517, n2518);
    let n2521: ZN = zsel_n(n2520, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2522: ZB = zb_or(n93, n2520);
    let n2523: ZN = zsel_n(n2520, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n2524: ZN = zsel_n(n2520, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n2525: ZN = zsel_n(n2520, n1577, r_c88);
    let n2526: ZN = zsel_n(n2520, n2513, zn_splat(P8::from_raw(0i32)));
    let n2527: ZN = zsel_n(n2520, n2519, zn_splat(P8::from_raw(0i32)));
    let n2528: ZN = zsel_n(n2520, n2500, zn_splat(P8::from_raw(0i32)));
    let n2529: ZN = zsel_n(n2520, n2507, zn_splat(P8::from_raw(0i32)));
    let n2530: ZN = zsel_n(n2520, n2488, n1708);
    let n2531: ZN = zsel_n(n2520, n2489, n1731);
    let n2532: ZB = zb_or(n2477, n2520);
    let n2533: ZB = zb_and(n1491, n1760);
    let n2534: ZB = zb_not(n2533);
    let n2535: ZB = zb_and(n1762, n2533);
    let n2536: ZB = zb_and(n1762, n2534);
    let n2537: ZN = zsel_n(n2535, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2538: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2537);
    let n2539: ZB = zb_not(n2538);
    let n2540: ZB = zb_and(n2535, n2539);
    let n2541: ZB = zb_and(n2535, n2538);
    let n2542: ZN = zn_mul(n2537, zn_splat(P8::from_raw(231700i32)));
    let n2543: ZN = zsel_n(n2540, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n2544: ZN = zsel_n(n2540, n2542, zn_splat(P8::from_raw(0i32)));
    let n2545: ZB = zb_or(n2540, n2541);
    let n2547: ZN = zsel_n(n2545, n2543, zn_splat(P8::from_raw(65536i32)));
    let n2548: ZN = zsel_n(n2545, n2544, zn_splat(P8::from_raw(0i32)));
    let n2549: ZB = zn_gt(n2547, zn_splat(P8::from_raw(0i32)));
    let n2550: ZB = zn_le(n2547, zn_splat(P8::from_raw(0i32)));
    let n2551: ZB = zb_and(n2545, n2549);
    let n2552: ZB = zb_and(n2545, n2550);
    let n2553: ZB = zn_lt(n2547, zn_splat(P8::from_raw(0i32)));
    let n2554: ZB = zn_ge(n2547, zn_splat(P8::from_raw(0i32)));
    let n2555: ZB = zb_and(n2552, n2553);
    let n2556: ZB = zb_and(n2552, n2554);
    let n2557: ZB = zb_or(n2551, n2555);
    let n2558: ZB = zb_or(n2556, n2557);
    let n2559: ZB = zn_gt(n2548, zn_splat(P8::from_raw(0i32)));
    let n2560: ZB = zn_le(n2548, zn_splat(P8::from_raw(0i32)));
    let n2561: ZB = zb_and(n2558, n2559);
    let n2562: ZB = zb_and(n2558, n2560);
    let n2563: ZB = zb_or(n2561, n2562);
    let n2564: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2548);
    let n2565: ZB = zb_not(n2564);
    let n2566: ZB = zb_and(n2563, n2565);
    let n2567: ZB = zb_and(n2563, n2564);
    let n2568: ZB = zb_or(n2566, n2567);
    let n2569: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2547);
    let n2570: ZB = zb_not(n2569);
    let n2571: ZB = zb_and(n2568, n2570);
    let n2572: ZB = zb_and(n2568, n2569);
    let n2573: ZB = zb_or(n2571, n2572);
    let n2574: ZN = zsel_n(n2573, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2575: ZB = zb_or(n93, n2573);
    let n2576: ZB = zb_or(n2536, n2573);
    let n2577: ZB = zb_and(n1626, n2532);
    let n2578: ZB = zb_and(n1627, n2532);
    let n2579: ZB = zb_or(n2577, n2578);
    let n2580: ZB = zb_not(n2577);
    let n2581: ZB = zb_and(n2577, n2579);
    let n2582: ZB = zb_and(n2579, n2580);
    let n2583: ZB = zb_and(n1626, n2576);
    let n2584: ZB = zb_and(n1627, n2576);
    let n2585: ZB = zb_or(n2583, n2584);
    let n2586: ZB = zb_not(n2583);
    let n2587: ZB = zb_and(n2583, n2585);
    let n2588: ZB = zb_and(n2585, n2586);
    let n2589: ZN = zsel_n(n2581, n92, zn_splat(P8::from_raw(983040i32)));
    let n2590: ZN = zsel_n(n2581, n2521, n2574);
    let n2591: ZB = zb_not(n2581);
    let n2592: ZB = zb_or(n95, n2591);
    let n2593: ZB = zb_or(n2581, n2587);
    let n2594: ZB = zsel_b(n2581, n1463, n1472);
    let n2595: ZN = zsel_n(n734, r_c20, n2521);
    let n2596: ZB = zsel_b(n734, n93, n2522);
    let n2597: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2523);
    let n2598: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2524);
    let n2599: ZN = zsel_n(n734, r_c88, n2525);
    let n2600: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2526);
    let n2601: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2527);
    let n2602: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2528);
    let n2603: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2529);
    let n2604: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2530);
    let n2605: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2531);
    let n2606: ZB = zb_or(n734, n2582);
    let n2607: ZB = zn_gt(n2574, zn_splat(P8::from_raw(0i32)));
    let n2608: ZB = zn_le(n2574, zn_splat(P8::from_raw(0i32)));
    let n2609: ZB = zb_and(n2588, n2607);
    let n2610: ZB = zb_and(n2588, n2608);
    let n2611: ZB = zn_gt(n2595, zn_splat(P8::from_raw(0i32)));
    let n2612: ZB = zn_le(n2595, zn_splat(P8::from_raw(0i32)));
    let n2613: ZB = zb_and(n2606, n2611);
    let n2614: ZB = zb_and(n2606, n2612);
    let n2615: ZB = zb_and(n1682, n2614);
    let n2616: ZB = zb_and(n1681, n2614);
    let n2617: ZB = zb_or(n2615, n2616);
    let n2618: ZB = zb_not(n2615);
    let n2619: ZB = zb_or(n1686, n2618);
    let n2620: ZB = zb_not(n2619);
    let n2621: ZB = zb_and(n2617, n2619);
    let n2622: ZB = zb_and(n2617, n2620);
    let n2623: ZN = zsel_n(n2621, n1694, n653);
    let n2624: ZN = zsel_n(n2621, zn_splat(P8::from_raw(0i32)), n2604);
    let n2625: ZB = zb_or(n2621, n2622);
    let n2626: ZB = zn_gt(n2590, zn_splat(P8::from_raw(0i32)));
    let n2627: ZB = zn_le(n2590, zn_splat(P8::from_raw(0i32)));
    let n2628: ZB = zb_and(n2593, n2626);
    let n2629: ZB = zb_and(n2593, n2627);
    let n2630: ZB = zb_or(n2609, n2610);
    let n2631: ZN = zsel_n(n2613, n653, n2623);
    let n2632: ZN = zsel_n(n2613, n2604, n2624);
    let n2633: ZB = zb_or(n2613, n2625);
    let n2634: ZB = zb_or(n2628, n2629);
    let n2635: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2597);
    let n2636: ZB = zb_and(n1483, n1844);
    let n2637: ZB = zb_not(n2636);
    let n2638: ZB = zb_and(n1846, n2636);
    let n2639: ZB = zb_and(n1846, n2637);
    let n2640: ZN = zsel_n(n2638, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2641: ZB = zb_and(n1816, n2638);
    let n2642: ZB = zb_and(n1815, n2638);
    let n2643: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2640);
    let n2644: ZB = zb_not(n2643);
    let n2645: ZB = zb_and(n2641, n2644);
    let n2646: ZB = zb_and(n2641, n2643);
    let n2647: ZN = zn_mul(n2640, zn_splat(P8::from_raw(231700i32)));
    let n2648: ZN = zsel_n(n2645, n1847, n1848);
    let n2649: ZN = zsel_n(n2645, n2647, zn_splat(P8::from_raw(0i32)));
    let n2650: ZB = zb_or(n2645, n2646);
    let n2651: ZB = zb_and(n2642, n2644);
    let n2652: ZB = zb_and(n2642, n2643);
    let n2653: ZN = zn_mul(n2640, zn_splat(P8::from_raw(327680i32)));
    let n2654: ZB = zb_and(n1813, n2652);
    let n2655: ZB = zb_and(n2213, n2652);
    let n2656: ZN = zsel_n(n2654, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2657: ZB = zb_or(n2654, n2655);
    let n2658: ZN = zsel_n(n2651, zn_splat(P8::from_raw(0i32)), n2656);
    let n2659: ZN = zsel_n(n2651, n2653, zn_splat(P8::from_raw(0i32)));
    let n2660: ZB = zb_or(n2651, n2657);
    let n2661: ZN = zsel_n(n2650, n2648, n2658);
    let n2662: ZN = zsel_n(n2650, n2649, n2659);
    let n2663: ZB = zb_or(n2650, n2660);
    let n2664: ZB = zn_gt(n2661, zn_splat(P8::from_raw(0i32)));
    let n2665: ZB = zn_le(n2661, zn_splat(P8::from_raw(0i32)));
    let n2666: ZB = zb_and(n2663, n2664);
    let n2667: ZB = zb_and(n2663, n2665);
    let n2668: ZB = zn_lt(n2661, zn_splat(P8::from_raw(0i32)));
    let n2669: ZB = zn_ge(n2661, zn_splat(P8::from_raw(0i32)));
    let n2670: ZB = zb_and(n2667, n2668);
    let n2671: ZB = zb_and(n2667, n2669);
    let n2672: ZN = zsel_n(n2666, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2673: ZB = zb_or(n2666, n2670);
    let n2674: ZN = zsel_n(n2671, zn_splat(P8::from_raw(0i32)), n2672);
    let n2675: ZB = zb_or(n2671, n2673);
    let n2676: ZB = zn_gt(n2662, zn_splat(P8::from_raw(0i32)));
    let n2677: ZB = zn_le(n2662, zn_splat(P8::from_raw(0i32)));
    let n2678: ZB = zb_and(n2675, n2676);
    let n2679: ZB = zb_and(n2675, n2677);
    let n2680: ZN = zsel_n(n2678, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2681: ZN = zsel_n(n2679, zn_splat(P8::from_raw(0i32)), n2680);
    let n2682: ZB = zb_or(n2678, n2679);
    let n2683: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2662);
    let n2684: ZB = zb_not(n2683);
    let n2685: ZB = zb_and(n2682, n2684);
    let n2686: ZB = zb_and(n2682, n2683);
    let n2687: ZN = zsel_n(n2685, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2688: ZB = zb_or(n2685, n2686);
    let n2689: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2661);
    let n2690: ZB = zb_not(n2689);
    let n2691: ZB = zb_and(n2688, n2690);
    let n2692: ZB = zb_and(n2688, n2689);
    let n2693: ZN = zsel_n(n2691, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2694: ZB = zb_or(n2691, n2692);
    let n2695: ZN = zsel_n(n2694, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2696: ZB = zb_or(n93, n2694);
    let n2697: ZN = zsel_n(n2694, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n2698: ZN = zsel_n(n2694, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n2699: ZN = zsel_n(n2694, n1577, r_c88);
    let n2700: ZN = zsel_n(n2694, n2687, zn_splat(P8::from_raw(0i32)));
    let n2701: ZN = zsel_n(n2694, n2693, zn_splat(P8::from_raw(0i32)));
    let n2702: ZN = zsel_n(n2694, n2674, zn_splat(P8::from_raw(0i32)));
    let n2703: ZN = zsel_n(n2694, n2681, zn_splat(P8::from_raw(0i32)));
    let n2704: ZN = zsel_n(n2694, n2661, n1806);
    let n2705: ZN = zsel_n(n2694, n2662, n1842);
    let n2706: ZB = zb_or(n2639, n2694);
    let n2707: ZB = zb_and(n1491, n1883);
    let n2708: ZB = zb_not(n2707);
    let n2709: ZB = zb_and(n1885, n2707);
    let n2710: ZB = zb_and(n1885, n2708);
    let n2711: ZN = zsel_n(n2709, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2712: ZB = zb_and(n1816, n2709);
    let n2713: ZB = zb_and(n1815, n2709);
    let n2714: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2711);
    let n2715: ZB = zb_not(n2714);
    let n2716: ZB = zb_and(n2712, n2715);
    let n2717: ZB = zb_and(n2712, n2714);
    let n2718: ZN = zn_mul(n2711, zn_splat(P8::from_raw(231700i32)));
    let n2719: ZN = zsel_n(n2716, n1847, n1848);
    let n2720: ZN = zsel_n(n2716, n2718, zn_splat(P8::from_raw(0i32)));
    let n2721: ZB = zb_or(n2716, n2717);
    let n2722: ZB = zb_and(n2713, n2715);
    let n2723: ZB = zb_and(n2713, n2714);
    let n2724: ZN = zn_mul(n2711, zn_splat(P8::from_raw(327680i32)));
    let n2725: ZB = zb_and(n2215, n2723);
    let n2726: ZB = zb_and(n2252, n2723);
    let n2727: ZN = zsel_n(n2725, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2728: ZB = zb_or(n2725, n2726);
    let n2729: ZN = zsel_n(n2722, zn_splat(P8::from_raw(0i32)), n2727);
    let n2730: ZN = zsel_n(n2722, n2724, zn_splat(P8::from_raw(0i32)));
    let n2731: ZB = zb_or(n2722, n2728);
    let n2732: ZN = zsel_n(n2721, n2719, n2729);
    let n2733: ZN = zsel_n(n2721, n2720, n2730);
    let n2734: ZB = zb_or(n2721, n2731);
    let n2735: ZB = zn_gt(n2732, zn_splat(P8::from_raw(0i32)));
    let n2736: ZB = zn_le(n2732, zn_splat(P8::from_raw(0i32)));
    let n2737: ZB = zb_and(n2734, n2735);
    let n2738: ZB = zb_and(n2734, n2736);
    let n2739: ZB = zn_lt(n2732, zn_splat(P8::from_raw(0i32)));
    let n2740: ZB = zn_ge(n2732, zn_splat(P8::from_raw(0i32)));
    let n2741: ZB = zb_and(n2738, n2739);
    let n2742: ZB = zb_and(n2738, n2740);
    let n2743: ZB = zb_or(n2737, n2741);
    let n2744: ZB = zb_or(n2742, n2743);
    let n2745: ZB = zn_gt(n2733, zn_splat(P8::from_raw(0i32)));
    let n2746: ZB = zn_le(n2733, zn_splat(P8::from_raw(0i32)));
    let n2747: ZB = zb_and(n2744, n2745);
    let n2748: ZB = zb_and(n2744, n2746);
    let n2749: ZB = zb_or(n2747, n2748);
    let n2750: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2733);
    let n2751: ZB = zb_not(n2750);
    let n2752: ZB = zb_and(n2749, n2751);
    let n2753: ZB = zb_and(n2749, n2750);
    let n2754: ZB = zb_or(n2752, n2753);
    let n2755: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2732);
    let n2756: ZB = zb_not(n2755);
    let n2757: ZB = zb_and(n2754, n2756);
    let n2758: ZB = zb_and(n2754, n2755);
    let n2759: ZB = zb_or(n2757, n2758);
    let n2760: ZN = zsel_n(n2759, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2761: ZB = zb_or(n93, n2759);
    let n2762: ZB = zb_or(n2710, n2759);
    let n2763: ZB = zb_and(n1626, n2706);
    let n2764: ZB = zb_and(n1627, n2706);
    let n2765: ZB = zb_or(n2763, n2764);
    let n2766: ZB = zb_not(n2763);
    let n2767: ZB = zb_and(n2763, n2765);
    let n2768: ZB = zb_and(n2765, n2766);
    let n2769: ZB = zb_and(n1626, n2762);
    let n2770: ZB = zb_and(n1627, n2762);
    let n2771: ZB = zb_or(n2769, n2770);
    let n2772: ZB = zb_not(n2769);
    let n2773: ZB = zb_and(n2769, n2771);
    let n2774: ZB = zb_and(n2771, n2772);
    let n2775: ZN = zsel_n(n2767, n92, zn_splat(P8::from_raw(983040i32)));
    let n2776: ZN = zsel_n(n2767, n2695, n2760);
    let n2777: ZB = zb_not(n2767);
    let n2778: ZB = zb_or(n95, n2777);
    let n2779: ZB = zb_or(n2767, n2773);
    let n2780: ZB = zsel_b(n2767, n1463, n1472);
    let n2781: ZN = zsel_n(n734, r_c20, n2695);
    let n2782: ZB = zsel_b(n734, n93, n2696);
    let n2783: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2697);
    let n2784: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2698);
    let n2785: ZN = zsel_n(n734, r_c88, n2699);
    let n2786: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2700);
    let n2787: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2701);
    let n2788: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2702);
    let n2789: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2703);
    let n2790: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2704);
    let n2791: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2705);
    let n2792: ZB = zb_or(n734, n2768);
    let n2793: ZB = zn_gt(n2760, zn_splat(P8::from_raw(0i32)));
    let n2794: ZB = zn_le(n2760, zn_splat(P8::from_raw(0i32)));
    let n2795: ZB = zb_and(n2774, n2793);
    let n2796: ZB = zb_and(n2774, n2794);
    let n2797: ZB = zn_gt(n2781, zn_splat(P8::from_raw(0i32)));
    let n2798: ZB = zn_le(n2781, zn_splat(P8::from_raw(0i32)));
    let n2799: ZB = zb_and(n2792, n2797);
    let n2800: ZB = zb_and(n2792, n2798);
    let n2801: ZB = zb_and(n1682, n2800);
    let n2802: ZB = zb_and(n1681, n2800);
    let n2803: ZB = zb_or(n2801, n2802);
    let n2804: ZB = zb_not(n2801);
    let n2805: ZB = zb_or(n1686, n2804);
    let n2806: ZB = zb_not(n2805);
    let n2807: ZB = zb_and(n2803, n2805);
    let n2808: ZB = zb_and(n2803, n2806);
    let n2809: ZN = zsel_n(n2807, n1694, n653);
    let n2810: ZN = zsel_n(n2807, zn_splat(P8::from_raw(0i32)), n2790);
    let n2811: ZB = zb_or(n2807, n2808);
    let n2812: ZB = zn_gt(n2776, zn_splat(P8::from_raw(0i32)));
    let n2813: ZB = zn_le(n2776, zn_splat(P8::from_raw(0i32)));
    let n2814: ZB = zb_and(n2779, n2812);
    let n2815: ZB = zb_and(n2779, n2813);
    let n2816: ZB = zb_or(n2795, n2796);
    let n2817: ZN = zsel_n(n2799, n653, n2809);
    let n2818: ZN = zsel_n(n2799, n2790, n2810);
    let n2819: ZB = zb_or(n2799, n2811);
    let n2820: ZB = zb_or(n2814, n2815);
    let n2821: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2783);
    let n2822: ZN = zsel_n(n2290, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2823: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2822);
    let n2824: ZB = zb_not(n2823);
    let n2825: ZB = zb_and(n2293, n2824);
    let n2826: ZB = zb_and(n2293, n2823);
    let n2827: ZN = zn_mul(n2822, zn_splat(P8::from_raw(231700i32)));
    let n2828: ZN = zsel_n(n2825, n1578, n1579);
    let n2829: ZN = zsel_n(n2825, n2827, zn_splat(P8::from_raw(0i32)));
    let n2830: ZB = zb_or(n2825, n2826);
    let n2831: ZB = zb_and(n2294, n2824);
    let n2832: ZB = zb_and(n2294, n2823);
    let n2833: ZN = zn_mul(n2822, zn_splat(P8::from_raw(327680i32)));
    let n2834: ZB = zb_and(n1530, n2832);
    let n2835: ZB = zb_and(n1970, n2832);
    let n2836: ZN = zsel_n(n2834, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2837: ZB = zb_or(n2834, n2835);
    let n2838: ZN = zsel_n(n2831, zn_splat(P8::from_raw(0i32)), n2836);
    let n2839: ZN = zsel_n(n2831, n2833, zn_splat(P8::from_raw(0i32)));
    let n2840: ZB = zb_or(n2831, n2837);
    let n2841: ZN = zsel_n(n2830, n2828, n2838);
    let n2842: ZN = zsel_n(n2830, n2829, n2839);
    let n2843: ZB = zb_or(n2830, n2840);
    let n2844: ZB = zn_gt(n2841, zn_splat(P8::from_raw(0i32)));
    let n2845: ZB = zn_le(n2841, zn_splat(P8::from_raw(0i32)));
    let n2846: ZB = zb_and(n2843, n2844);
    let n2847: ZB = zb_and(n2843, n2845);
    let n2848: ZB = zn_lt(n2841, zn_splat(P8::from_raw(0i32)));
    let n2849: ZB = zn_ge(n2841, zn_splat(P8::from_raw(0i32)));
    let n2850: ZB = zb_and(n2847, n2848);
    let n2851: ZB = zb_and(n2847, n2849);
    let n2852: ZN = zsel_n(n2846, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2853: ZB = zb_or(n2846, n2850);
    let n2854: ZN = zsel_n(n2851, zn_splat(P8::from_raw(0i32)), n2852);
    let n2855: ZB = zb_or(n2851, n2853);
    let n2856: ZB = zn_gt(n2842, zn_splat(P8::from_raw(0i32)));
    let n2857: ZB = zn_le(n2842, zn_splat(P8::from_raw(0i32)));
    let n2858: ZB = zb_and(n2855, n2856);
    let n2859: ZB = zb_and(n2855, n2857);
    let n2860: ZB = zn_lt(n2842, zn_splat(P8::from_raw(0i32)));
    let n2861: ZB = zn_ge(n2842, zn_splat(P8::from_raw(0i32)));
    let n2862: ZB = zb_and(n2859, n2860);
    let n2863: ZB = zb_and(n2859, n2861);
    let n2864: ZN = zsel_n(n2858, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n2865: ZB = zb_or(n2858, n2862);
    let n2866: ZN = zsel_n(n2863, zn_splat(P8::from_raw(0i32)), n2864);
    let n2867: ZB = zb_or(n2863, n2865);
    let n2868: ZB = zb_and(n2860, n2867);
    let n2869: ZB = zb_and(n2861, n2867);
    let n2870: ZN = zn_mul(n2866, zn_splat(P8::from_raw(49152i32)));
    let n2871: ZN = zsel_n(n2868, n2870, n2866);
    let n2872: ZB = zb_or(n2868, n2869);
    let n2873: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2842);
    let n2874: ZB = zb_not(n2873);
    let n2875: ZB = zb_and(n2872, n2874);
    let n2876: ZB = zb_and(n2872, n2873);
    let n2877: ZN = zsel_n(n2875, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2878: ZB = zb_or(n2875, n2876);
    let n2879: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2841);
    let n2880: ZB = zb_not(n2879);
    let n2881: ZB = zb_and(n2878, n2880);
    let n2882: ZB = zb_and(n2878, n2879);
    let n2883: ZN = zsel_n(n2881, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2884: ZB = zb_or(n2881, n2882);
    let n2885: ZN = zsel_n(n2884, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2886: ZB = zb_or(n93, n2884);
    let n2887: ZN = zsel_n(n2884, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n2888: ZN = zsel_n(n2884, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n2889: ZN = zsel_n(n2884, n1577, r_c88);
    let n2890: ZN = zsel_n(n2884, n2877, zn_splat(P8::from_raw(0i32)));
    let n2891: ZN = zsel_n(n2884, n2883, zn_splat(P8::from_raw(0i32)));
    let n2892: ZN = zsel_n(n2884, n2854, zn_splat(P8::from_raw(0i32)));
    let n2893: ZN = zsel_n(n2884, n2871, zn_splat(P8::from_raw(0i32)));
    let n2894: ZN = zsel_n(n2884, n2841, n1523);
    let n2895: ZN = zsel_n(n2884, n2842, n1561);
    let n2896: ZB = zb_or(n2291, n2884);
    let n2897: ZN = zsel_n(n2361, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2898: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2897);
    let n2899: ZB = zb_not(n2898);
    let n2900: ZB = zb_and(n2364, n2899);
    let n2901: ZB = zb_and(n2364, n2898);
    let n2902: ZN = zn_mul(n2897, zn_splat(P8::from_raw(231700i32)));
    let n2903: ZN = zsel_n(n2900, n1578, n1579);
    let n2904: ZN = zsel_n(n2900, n2902, zn_splat(P8::from_raw(0i32)));
    let n2905: ZB = zb_or(n2900, n2901);
    let n2906: ZB = zb_and(n2365, n2899);
    let n2907: ZB = zb_and(n2365, n2898);
    let n2908: ZN = zn_mul(n2897, zn_splat(P8::from_raw(327680i32)));
    let n2909: ZB = zb_and(n1972, n2907);
    let n2910: ZB = zb_and(n2011, n2907);
    let n2911: ZN = zsel_n(n2909, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2912: ZB = zb_or(n2909, n2910);
    let n2913: ZN = zsel_n(n2906, zn_splat(P8::from_raw(0i32)), n2911);
    let n2914: ZN = zsel_n(n2906, n2908, zn_splat(P8::from_raw(0i32)));
    let n2915: ZB = zb_or(n2906, n2912);
    let n2916: ZN = zsel_n(n2905, n2903, n2913);
    let n2917: ZN = zsel_n(n2905, n2904, n2914);
    let n2918: ZB = zb_or(n2905, n2915);
    let n2919: ZB = zn_gt(n2916, zn_splat(P8::from_raw(0i32)));
    let n2920: ZB = zn_le(n2916, zn_splat(P8::from_raw(0i32)));
    let n2921: ZB = zb_and(n2918, n2919);
    let n2922: ZB = zb_and(n2918, n2920);
    let n2923: ZB = zn_lt(n2916, zn_splat(P8::from_raw(0i32)));
    let n2924: ZB = zn_ge(n2916, zn_splat(P8::from_raw(0i32)));
    let n2925: ZB = zb_and(n2922, n2923);
    let n2926: ZB = zb_and(n2922, n2924);
    let n2927: ZB = zb_or(n2921, n2925);
    let n2928: ZB = zb_or(n2926, n2927);
    let n2929: ZB = zn_gt(n2917, zn_splat(P8::from_raw(0i32)));
    let n2930: ZB = zn_le(n2917, zn_splat(P8::from_raw(0i32)));
    let n2931: ZB = zb_and(n2928, n2929);
    let n2932: ZB = zb_and(n2928, n2930);
    let n2933: ZB = zn_lt(n2917, zn_splat(P8::from_raw(0i32)));
    let n2934: ZB = zn_ge(n2917, zn_splat(P8::from_raw(0i32)));
    let n2935: ZB = zb_and(n2932, n2933);
    let n2936: ZB = zb_and(n2932, n2934);
    let n2937: ZB = zb_or(n2931, n2935);
    let n2938: ZB = zb_or(n2936, n2937);
    let n2939: ZB = zb_and(n2933, n2938);
    let n2940: ZB = zb_and(n2934, n2938);
    let n2941: ZB = zb_or(n2939, n2940);
    let n2942: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2917);
    let n2943: ZB = zb_not(n2942);
    let n2944: ZB = zb_and(n2941, n2943);
    let n2945: ZB = zb_and(n2941, n2942);
    let n2946: ZB = zb_or(n2944, n2945);
    let n2947: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2916);
    let n2948: ZB = zb_not(n2947);
    let n2949: ZB = zb_and(n2946, n2948);
    let n2950: ZB = zb_and(n2946, n2947);
    let n2951: ZB = zb_or(n2949, n2950);
    let n2952: ZN = zsel_n(n2951, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2953: ZB = zb_or(n93, n2951);
    let n2954: ZB = zb_or(n2362, n2951);
    let n2955: ZB = zb_and(n1626, n2896);
    let n2956: ZB = zb_and(n1627, n2896);
    let n2957: ZB = zb_or(n2955, n2956);
    let n2958: ZB = zb_not(n2955);
    let n2959: ZB = zb_and(n2955, n2957);
    let n2960: ZB = zb_and(n2957, n2958);
    let n2961: ZB = zb_and(n1626, n2954);
    let n2962: ZB = zb_and(n1627, n2954);
    let n2963: ZB = zb_or(n2961, n2962);
    let n2964: ZB = zb_not(n2961);
    let n2965: ZB = zb_and(n2961, n2963);
    let n2966: ZB = zb_and(n2963, n2964);
    let n2967: ZN = zsel_n(n2959, n92, zn_splat(P8::from_raw(983040i32)));
    let n2968: ZN = zsel_n(n2959, n2885, n2952);
    let n2969: ZB = zb_not(n2959);
    let n2970: ZB = zb_or(n95, n2969);
    let n2971: ZB = zb_or(n2959, n2965);
    let n2972: ZB = zsel_b(n2959, n1463, n1472);
    let n2973: ZN = zsel_n(n734, r_c20, n2885);
    let n2974: ZB = zsel_b(n734, n93, n2886);
    let n2975: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2887);
    let n2976: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2888);
    let n2977: ZN = zsel_n(n734, r_c88, n2889);
    let n2978: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2890);
    let n2979: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2891);
    let n2980: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2892);
    let n2981: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2893);
    let n2982: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2894);
    let n2983: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n2895);
    let n2984: ZB = zb_or(n734, n2960);
    let n2985: ZB = zn_gt(n2952, zn_splat(P8::from_raw(0i32)));
    let n2986: ZB = zn_le(n2952, zn_splat(P8::from_raw(0i32)));
    let n2987: ZB = zb_and(n2966, n2985);
    let n2988: ZB = zb_and(n2966, n2986);
    let n2989: ZB = zn_gt(n2973, zn_splat(P8::from_raw(0i32)));
    let n2990: ZB = zn_le(n2973, zn_splat(P8::from_raw(0i32)));
    let n2991: ZB = zb_and(n2984, n2989);
    let n2992: ZB = zb_and(n2984, n2990);
    let n2993: ZB = zb_and(n1682, n2992);
    let n2994: ZB = zb_and(n1681, n2992);
    let n2995: ZB = zb_or(n2993, n2994);
    let n2996: ZB = zb_not(n2993);
    let n2997: ZB = zb_or(n1686, n2996);
    let n2998: ZB = zb_not(n2997);
    let n2999: ZB = zb_and(n2995, n2997);
    let n3000: ZB = zb_and(n2995, n2998);
    let n3001: ZN = zsel_n(n2999, n1694, n653);
    let n3002: ZN = zsel_n(n2999, zn_splat(P8::from_raw(0i32)), n2982);
    let n3003: ZB = zb_or(n2999, n3000);
    let n3004: ZB = zn_gt(n2968, zn_splat(P8::from_raw(0i32)));
    let n3005: ZB = zn_le(n2968, zn_splat(P8::from_raw(0i32)));
    let n3006: ZB = zb_and(n2971, n3004);
    let n3007: ZB = zb_and(n2971, n3005);
    let n3008: ZB = zb_or(n2987, n2988);
    let n3009: ZN = zsel_n(n2991, n653, n3001);
    let n3010: ZN = zsel_n(n2991, n2982, n3002);
    let n3011: ZB = zb_or(n2991, n3003);
    let n3012: ZB = zb_or(n3006, n3007);
    let n3013: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2975);
    let n3014: ZN = zsel_n(n2476, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3015: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3014);
    let n3016: ZB = zb_not(n3015);
    let n3017: ZB = zb_and(n2476, n3016);
    let n3018: ZB = zb_and(n2476, n3015);
    let n3019: ZN = zn_mul(n3014, zn_splat(P8::from_raw(231700i32)));
    let n3020: ZN = zsel_n(n3017, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n3021: ZN = zsel_n(n3017, n3019, zn_splat(P8::from_raw(0i32)));
    let n3022: ZB = zb_or(n3017, n3018);
    let n3024: ZN = zsel_n(n3022, n3020, zn_splat(P8::from_raw(65536i32)));
    let n3025: ZN = zsel_n(n3022, n3021, zn_splat(P8::from_raw(0i32)));
    let n3026: ZB = zn_gt(n3024, zn_splat(P8::from_raw(0i32)));
    let n3027: ZB = zn_le(n3024, zn_splat(P8::from_raw(0i32)));
    let n3028: ZB = zb_and(n3022, n3026);
    let n3029: ZB = zb_and(n3022, n3027);
    let n3030: ZB = zn_lt(n3024, zn_splat(P8::from_raw(0i32)));
    let n3031: ZB = zn_ge(n3024, zn_splat(P8::from_raw(0i32)));
    let n3032: ZB = zb_and(n3029, n3030);
    let n3033: ZB = zb_and(n3029, n3031);
    let n3034: ZN = zsel_n(n3028, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3035: ZB = zb_or(n3028, n3032);
    let n3036: ZN = zsel_n(n3033, zn_splat(P8::from_raw(0i32)), n3034);
    let n3037: ZB = zb_or(n3033, n3035);
    let n3038: ZB = zn_gt(n3025, zn_splat(P8::from_raw(0i32)));
    let n3039: ZB = zn_le(n3025, zn_splat(P8::from_raw(0i32)));
    let n3040: ZB = zb_and(n3037, n3038);
    let n3041: ZB = zb_and(n3037, n3039);
    let n3042: ZB = zn_lt(n3025, zn_splat(P8::from_raw(0i32)));
    let n3043: ZB = zn_ge(n3025, zn_splat(P8::from_raw(0i32)));
    let n3044: ZB = zb_and(n3041, n3042);
    let n3045: ZB = zb_and(n3041, n3043);
    let n3046: ZN = zsel_n(n3040, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3047: ZB = zb_or(n3040, n3044);
    let n3048: ZN = zsel_n(n3045, zn_splat(P8::from_raw(0i32)), n3046);
    let n3049: ZB = zb_or(n3045, n3047);
    let n3050: ZB = zb_and(n3042, n3049);
    let n3051: ZB = zb_and(n3043, n3049);
    let n3052: ZN = zn_mul(n3048, zn_splat(P8::from_raw(49152i32)));
    let n3053: ZN = zsel_n(n3050, n3052, n3048);
    let n3054: ZB = zb_or(n3050, n3051);
    let n3055: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3025);
    let n3056: ZB = zb_not(n3055);
    let n3057: ZB = zb_and(n3054, n3056);
    let n3058: ZB = zb_and(n3054, n3055);
    let n3059: ZN = zsel_n(n3057, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3060: ZB = zb_or(n3057, n3058);
    let n3061: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3024);
    let n3062: ZB = zb_not(n3061);
    let n3063: ZB = zb_and(n3060, n3062);
    let n3064: ZB = zb_and(n3060, n3061);
    let n3065: ZN = zsel_n(n3063, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3066: ZB = zb_or(n3063, n3064);
    let n3067: ZN = zsel_n(n3066, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3068: ZB = zb_or(n93, n3066);
    let n3069: ZN = zsel_n(n3066, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n3070: ZN = zsel_n(n3066, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n3071: ZN = zsel_n(n3066, n1577, r_c88);
    let n3072: ZN = zsel_n(n3066, n3059, zn_splat(P8::from_raw(0i32)));
    let n3073: ZN = zsel_n(n3066, n3065, zn_splat(P8::from_raw(0i32)));
    let n3074: ZN = zsel_n(n3066, n3036, zn_splat(P8::from_raw(0i32)));
    let n3075: ZN = zsel_n(n3066, n3053, zn_splat(P8::from_raw(0i32)));
    let n3076: ZN = zsel_n(n3066, n3024, n1708);
    let n3077: ZN = zsel_n(n3066, n3025, n1731);
    let n3078: ZB = zb_or(n2477, n3066);
    let n3079: ZN = zsel_n(n2535, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3080: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3079);
    let n3081: ZB = zb_not(n3080);
    let n3082: ZB = zb_and(n2535, n3081);
    let n3083: ZB = zb_and(n2535, n3080);
    let n3084: ZN = zn_mul(n3079, zn_splat(P8::from_raw(231700i32)));
    let n3085: ZN = zsel_n(n3082, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n3086: ZN = zsel_n(n3082, n3084, zn_splat(P8::from_raw(0i32)));
    let n3087: ZB = zb_or(n3082, n3083);
    let n3089: ZN = zsel_n(n3087, n3085, zn_splat(P8::from_raw(65536i32)));
    let n3090: ZN = zsel_n(n3087, n3086, zn_splat(P8::from_raw(0i32)));
    let n3091: ZB = zn_gt(n3089, zn_splat(P8::from_raw(0i32)));
    let n3092: ZB = zn_le(n3089, zn_splat(P8::from_raw(0i32)));
    let n3093: ZB = zb_and(n3087, n3091);
    let n3094: ZB = zb_and(n3087, n3092);
    let n3095: ZB = zn_lt(n3089, zn_splat(P8::from_raw(0i32)));
    let n3096: ZB = zn_ge(n3089, zn_splat(P8::from_raw(0i32)));
    let n3097: ZB = zb_and(n3094, n3095);
    let n3098: ZB = zb_and(n3094, n3096);
    let n3099: ZB = zb_or(n3093, n3097);
    let n3100: ZB = zb_or(n3098, n3099);
    let n3101: ZB = zn_gt(n3090, zn_splat(P8::from_raw(0i32)));
    let n3102: ZB = zn_le(n3090, zn_splat(P8::from_raw(0i32)));
    let n3103: ZB = zb_and(n3100, n3101);
    let n3104: ZB = zb_and(n3100, n3102);
    let n3105: ZB = zn_lt(n3090, zn_splat(P8::from_raw(0i32)));
    let n3106: ZB = zn_ge(n3090, zn_splat(P8::from_raw(0i32)));
    let n3107: ZB = zb_and(n3104, n3105);
    let n3108: ZB = zb_and(n3104, n3106);
    let n3109: ZB = zb_or(n3103, n3107);
    let n3110: ZB = zb_or(n3108, n3109);
    let n3111: ZB = zb_and(n3105, n3110);
    let n3112: ZB = zb_and(n3106, n3110);
    let n3113: ZB = zb_or(n3111, n3112);
    let n3114: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3090);
    let n3115: ZB = zb_not(n3114);
    let n3116: ZB = zb_and(n3113, n3115);
    let n3117: ZB = zb_and(n3113, n3114);
    let n3118: ZB = zb_or(n3116, n3117);
    let n3119: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3089);
    let n3120: ZB = zb_not(n3119);
    let n3121: ZB = zb_and(n3118, n3120);
    let n3122: ZB = zb_and(n3118, n3119);
    let n3123: ZB = zb_or(n3121, n3122);
    let n3124: ZN = zsel_n(n3123, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3125: ZB = zb_or(n93, n3123);
    let n3126: ZB = zb_or(n2536, n3123);
    let n3127: ZB = zb_and(n1626, n3078);
    let n3128: ZB = zb_and(n1627, n3078);
    let n3129: ZB = zb_or(n3127, n3128);
    let n3130: ZB = zb_not(n3127);
    let n3131: ZB = zb_and(n3127, n3129);
    let n3132: ZB = zb_and(n3129, n3130);
    let n3133: ZB = zb_and(n1626, n3126);
    let n3134: ZB = zb_and(n1627, n3126);
    let n3135: ZB = zb_or(n3133, n3134);
    let n3136: ZB = zb_not(n3133);
    let n3137: ZB = zb_and(n3133, n3135);
    let n3138: ZB = zb_and(n3135, n3136);
    let n3139: ZN = zsel_n(n3131, n92, zn_splat(P8::from_raw(983040i32)));
    let n3140: ZN = zsel_n(n3131, n3067, n3124);
    let n3141: ZB = zb_not(n3131);
    let n3142: ZB = zb_or(n95, n3141);
    let n3143: ZB = zb_or(n3131, n3137);
    let n3144: ZB = zsel_b(n3131, n1463, n1472);
    let n3145: ZN = zsel_n(n734, r_c20, n3067);
    let n3146: ZB = zsel_b(n734, n93, n3068);
    let n3147: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3069);
    let n3148: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3070);
    let n3149: ZN = zsel_n(n734, r_c88, n3071);
    let n3150: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3072);
    let n3151: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3073);
    let n3152: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3074);
    let n3153: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3075);
    let n3154: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3076);
    let n3155: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3077);
    let n3156: ZB = zb_or(n734, n3132);
    let n3157: ZB = zn_gt(n3124, zn_splat(P8::from_raw(0i32)));
    let n3158: ZB = zn_le(n3124, zn_splat(P8::from_raw(0i32)));
    let n3159: ZB = zb_and(n3138, n3157);
    let n3160: ZB = zb_and(n3138, n3158);
    let n3161: ZB = zn_gt(n3145, zn_splat(P8::from_raw(0i32)));
    let n3162: ZB = zn_le(n3145, zn_splat(P8::from_raw(0i32)));
    let n3163: ZB = zb_and(n3156, n3161);
    let n3164: ZB = zb_and(n3156, n3162);
    let n3165: ZB = zb_and(n1682, n3164);
    let n3166: ZB = zb_and(n1681, n3164);
    let n3167: ZB = zb_or(n3165, n3166);
    let n3168: ZB = zb_not(n3165);
    let n3169: ZB = zb_or(n1686, n3168);
    let n3170: ZB = zb_not(n3169);
    let n3171: ZB = zb_and(n3167, n3169);
    let n3172: ZB = zb_and(n3167, n3170);
    let n3173: ZN = zsel_n(n3171, n1694, n653);
    let n3174: ZN = zsel_n(n3171, zn_splat(P8::from_raw(0i32)), n3154);
    let n3175: ZB = zb_or(n3171, n3172);
    let n3176: ZB = zn_gt(n3140, zn_splat(P8::from_raw(0i32)));
    let n3177: ZB = zn_le(n3140, zn_splat(P8::from_raw(0i32)));
    let n3178: ZB = zb_and(n3143, n3176);
    let n3179: ZB = zb_and(n3143, n3177);
    let n3180: ZB = zb_or(n3159, n3160);
    let n3181: ZN = zsel_n(n3163, n653, n3173);
    let n3182: ZN = zsel_n(n3163, n3154, n3174);
    let n3183: ZB = zb_or(n3163, n3175);
    let n3184: ZB = zb_or(n3178, n3179);
    let n3185: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3147);
    let n3186: ZN = zsel_n(n2638, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3187: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3186);
    let n3188: ZB = zb_not(n3187);
    let n3189: ZB = zb_and(n2641, n3188);
    let n3190: ZB = zb_and(n2641, n3187);
    let n3191: ZN = zn_mul(n3186, zn_splat(P8::from_raw(231700i32)));
    let n3192: ZN = zsel_n(n3189, n1847, n1848);
    let n3193: ZN = zsel_n(n3189, n3191, zn_splat(P8::from_raw(0i32)));
    let n3194: ZB = zb_or(n3189, n3190);
    let n3195: ZB = zb_and(n2642, n3188);
    let n3196: ZB = zb_and(n2642, n3187);
    let n3197: ZN = zn_mul(n3186, zn_splat(P8::from_raw(327680i32)));
    let n3198: ZB = zb_and(n1813, n3196);
    let n3199: ZB = zb_and(n2213, n3196);
    let n3200: ZN = zsel_n(n3198, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3201: ZB = zb_or(n3198, n3199);
    let n3202: ZN = zsel_n(n3195, zn_splat(P8::from_raw(0i32)), n3200);
    let n3203: ZN = zsel_n(n3195, n3197, zn_splat(P8::from_raw(0i32)));
    let n3204: ZB = zb_or(n3195, n3201);
    let n3205: ZN = zsel_n(n3194, n3192, n3202);
    let n3206: ZN = zsel_n(n3194, n3193, n3203);
    let n3207: ZB = zb_or(n3194, n3204);
    let n3208: ZB = zn_gt(n3205, zn_splat(P8::from_raw(0i32)));
    let n3209: ZB = zn_le(n3205, zn_splat(P8::from_raw(0i32)));
    let n3210: ZB = zb_and(n3207, n3208);
    let n3211: ZB = zb_and(n3207, n3209);
    let n3212: ZB = zn_lt(n3205, zn_splat(P8::from_raw(0i32)));
    let n3213: ZB = zn_ge(n3205, zn_splat(P8::from_raw(0i32)));
    let n3214: ZB = zb_and(n3211, n3212);
    let n3215: ZB = zb_and(n3211, n3213);
    let n3216: ZN = zsel_n(n3210, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3217: ZB = zb_or(n3210, n3214);
    let n3218: ZN = zsel_n(n3215, zn_splat(P8::from_raw(0i32)), n3216);
    let n3219: ZB = zb_or(n3215, n3217);
    let n3220: ZB = zn_gt(n3206, zn_splat(P8::from_raw(0i32)));
    let n3221: ZB = zn_le(n3206, zn_splat(P8::from_raw(0i32)));
    let n3222: ZB = zb_and(n3219, n3220);
    let n3223: ZB = zb_and(n3219, n3221);
    let n3224: ZB = zn_lt(n3206, zn_splat(P8::from_raw(0i32)));
    let n3225: ZB = zn_ge(n3206, zn_splat(P8::from_raw(0i32)));
    let n3226: ZB = zb_and(n3223, n3224);
    let n3227: ZB = zb_and(n3223, n3225);
    let n3228: ZN = zsel_n(n3222, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3229: ZB = zb_or(n3222, n3226);
    let n3230: ZN = zsel_n(n3227, zn_splat(P8::from_raw(0i32)), n3228);
    let n3231: ZB = zb_or(n3227, n3229);
    let n3232: ZB = zb_and(n3224, n3231);
    let n3233: ZB = zb_and(n3225, n3231);
    let n3234: ZN = zn_mul(n3230, zn_splat(P8::from_raw(49152i32)));
    let n3235: ZN = zsel_n(n3232, n3234, n3230);
    let n3236: ZB = zb_or(n3232, n3233);
    let n3237: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3206);
    let n3238: ZB = zb_not(n3237);
    let n3239: ZB = zb_and(n3236, n3238);
    let n3240: ZB = zb_and(n3236, n3237);
    let n3241: ZN = zsel_n(n3239, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3242: ZB = zb_or(n3239, n3240);
    let n3243: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3205);
    let n3244: ZB = zb_not(n3243);
    let n3245: ZB = zb_and(n3242, n3244);
    let n3246: ZB = zb_and(n3242, n3243);
    let n3247: ZN = zsel_n(n3245, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3248: ZB = zb_or(n3245, n3246);
    let n3249: ZN = zsel_n(n3248, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3250: ZB = zb_or(n93, n3248);
    let n3251: ZN = zsel_n(n3248, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n3252: ZN = zsel_n(n3248, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n3253: ZN = zsel_n(n3248, n1577, r_c88);
    let n3254: ZN = zsel_n(n3248, n3241, zn_splat(P8::from_raw(0i32)));
    let n3255: ZN = zsel_n(n3248, n3247, zn_splat(P8::from_raw(0i32)));
    let n3256: ZN = zsel_n(n3248, n3218, zn_splat(P8::from_raw(0i32)));
    let n3257: ZN = zsel_n(n3248, n3235, zn_splat(P8::from_raw(0i32)));
    let n3258: ZN = zsel_n(n3248, n3205, n1806);
    let n3259: ZN = zsel_n(n3248, n3206, n1842);
    let n3260: ZB = zb_or(n2639, n3248);
    let n3261: ZN = zsel_n(n2709, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3262: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3261);
    let n3263: ZB = zb_not(n3262);
    let n3264: ZB = zb_and(n2712, n3263);
    let n3265: ZB = zb_and(n2712, n3262);
    let n3266: ZN = zn_mul(n3261, zn_splat(P8::from_raw(231700i32)));
    let n3267: ZN = zsel_n(n3264, n1847, n1848);
    let n3268: ZN = zsel_n(n3264, n3266, zn_splat(P8::from_raw(0i32)));
    let n3269: ZB = zb_or(n3264, n3265);
    let n3270: ZB = zb_and(n2713, n3263);
    let n3271: ZB = zb_and(n2713, n3262);
    let n3272: ZN = zn_mul(n3261, zn_splat(P8::from_raw(327680i32)));
    let n3273: ZB = zb_and(n2215, n3271);
    let n3274: ZB = zb_and(n2252, n3271);
    let n3275: ZN = zsel_n(n3273, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3276: ZB = zb_or(n3273, n3274);
    let n3277: ZN = zsel_n(n3270, zn_splat(P8::from_raw(0i32)), n3275);
    let n3278: ZN = zsel_n(n3270, n3272, zn_splat(P8::from_raw(0i32)));
    let n3279: ZB = zb_or(n3270, n3276);
    let n3280: ZN = zsel_n(n3269, n3267, n3277);
    let n3281: ZN = zsel_n(n3269, n3268, n3278);
    let n3282: ZB = zb_or(n3269, n3279);
    let n3283: ZB = zn_gt(n3280, zn_splat(P8::from_raw(0i32)));
    let n3284: ZB = zn_le(n3280, zn_splat(P8::from_raw(0i32)));
    let n3285: ZB = zb_and(n3282, n3283);
    let n3286: ZB = zb_and(n3282, n3284);
    let n3287: ZB = zn_lt(n3280, zn_splat(P8::from_raw(0i32)));
    let n3288: ZB = zn_ge(n3280, zn_splat(P8::from_raw(0i32)));
    let n3289: ZB = zb_and(n3286, n3287);
    let n3290: ZB = zb_and(n3286, n3288);
    let n3291: ZB = zb_or(n3285, n3289);
    let n3292: ZB = zb_or(n3290, n3291);
    let n3293: ZB = zn_gt(n3281, zn_splat(P8::from_raw(0i32)));
    let n3294: ZB = zn_le(n3281, zn_splat(P8::from_raw(0i32)));
    let n3295: ZB = zb_and(n3292, n3293);
    let n3296: ZB = zb_and(n3292, n3294);
    let n3297: ZB = zn_lt(n3281, zn_splat(P8::from_raw(0i32)));
    let n3298: ZB = zn_ge(n3281, zn_splat(P8::from_raw(0i32)));
    let n3299: ZB = zb_and(n3296, n3297);
    let n3300: ZB = zb_and(n3296, n3298);
    let n3301: ZB = zb_or(n3295, n3299);
    let n3302: ZB = zb_or(n3300, n3301);
    let n3303: ZB = zb_and(n3297, n3302);
    let n3304: ZB = zb_and(n3298, n3302);
    let n3305: ZB = zb_or(n3303, n3304);
    let n3306: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3281);
    let n3307: ZB = zb_not(n3306);
    let n3308: ZB = zb_and(n3305, n3307);
    let n3309: ZB = zb_and(n3305, n3306);
    let n3310: ZB = zb_or(n3308, n3309);
    let n3311: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3280);
    let n3312: ZB = zb_not(n3311);
    let n3313: ZB = zb_and(n3310, n3312);
    let n3314: ZB = zb_and(n3310, n3311);
    let n3315: ZB = zb_or(n3313, n3314);
    let n3316: ZN = zsel_n(n3315, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3317: ZB = zb_or(n93, n3315);
    let n3318: ZB = zb_or(n2710, n3315);
    let n3319: ZB = zb_and(n1626, n3260);
    let n3320: ZB = zb_and(n1627, n3260);
    let n3321: ZB = zb_or(n3319, n3320);
    let n3322: ZB = zb_not(n3319);
    let n3323: ZB = zb_and(n3319, n3321);
    let n3324: ZB = zb_and(n3321, n3322);
    let n3325: ZB = zb_and(n1626, n3318);
    let n3326: ZB = zb_and(n1627, n3318);
    let n3327: ZB = zb_or(n3325, n3326);
    let n3328: ZB = zb_not(n3325);
    let n3329: ZB = zb_and(n3325, n3327);
    let n3330: ZB = zb_and(n3327, n3328);
    let n3331: ZN = zsel_n(n3323, n92, zn_splat(P8::from_raw(983040i32)));
    let n3332: ZN = zsel_n(n3323, n3249, n3316);
    let n3333: ZB = zb_not(n3323);
    let n3334: ZB = zb_or(n95, n3333);
    let n3335: ZB = zb_or(n3323, n3329);
    let n3336: ZB = zsel_b(n3323, n1463, n1472);
    let n3337: ZN = zsel_n(n734, r_c20, n3249);
    let n3338: ZB = zsel_b(n734, n93, n3250);
    let n3339: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3251);
    let n3340: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3252);
    let n3341: ZN = zsel_n(n734, r_c88, n3253);
    let n3342: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3254);
    let n3343: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3255);
    let n3344: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3256);
    let n3345: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3257);
    let n3346: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3258);
    let n3347: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3259);
    let n3348: ZB = zb_or(n734, n3324);
    let n3349: ZB = zn_gt(n3316, zn_splat(P8::from_raw(0i32)));
    let n3350: ZB = zn_le(n3316, zn_splat(P8::from_raw(0i32)));
    let n3351: ZB = zb_and(n3330, n3349);
    let n3352: ZB = zb_and(n3330, n3350);
    let n3353: ZB = zn_gt(n3337, zn_splat(P8::from_raw(0i32)));
    let n3354: ZB = zn_le(n3337, zn_splat(P8::from_raw(0i32)));
    let n3355: ZB = zb_and(n3348, n3353);
    let n3356: ZB = zb_and(n3348, n3354);
    let n3357: ZB = zb_and(n1682, n3356);
    let n3358: ZB = zb_and(n1681, n3356);
    let n3359: ZB = zb_or(n3357, n3358);
    let n3360: ZB = zb_not(n3357);
    let n3361: ZB = zb_or(n1686, n3360);
    let n3362: ZB = zb_not(n3361);
    let n3363: ZB = zb_and(n3359, n3361);
    let n3364: ZB = zb_and(n3359, n3362);
    let n3365: ZN = zsel_n(n3363, n1694, n653);
    let n3366: ZN = zsel_n(n3363, zn_splat(P8::from_raw(0i32)), n3346);
    let n3367: ZB = zb_or(n3363, n3364);
    let n3368: ZB = zn_gt(n3332, zn_splat(P8::from_raw(0i32)));
    let n3369: ZB = zn_le(n3332, zn_splat(P8::from_raw(0i32)));
    let n3370: ZB = zb_and(n3335, n3368);
    let n3371: ZB = zb_and(n3335, n3369);
    let n3372: ZB = zb_or(n3351, n3352);
    let n3373: ZN = zsel_n(n3355, n653, n3365);
    let n3374: ZN = zsel_n(n3355, n3346, n3366);
    let n3375: ZB = zb_or(n3355, n3367);
    let n3376: ZB = zb_or(n3370, n3371);
    let n3377: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3339);
    let n3378: ZN = zsel_n(n2293, n1578, n1579);
    let n3379: ZN = zsel_n(n2293, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n3380: ZN = zsel_n(n2294, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3381: ZN = zsel_n(n2294, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n3382: ZN = zsel_n(n2293, n3378, n3380);
    let n3383: ZN = zsel_n(n2293, n3379, n3381);
    let n3384: ZB = zb_or(n2293, n2294);
    let n3385: ZB = zn_gt(n3382, zn_splat(P8::from_raw(0i32)));
    let n3386: ZB = zn_le(n3382, zn_splat(P8::from_raw(0i32)));
    let n3387: ZB = zb_and(n3384, n3385);
    let n3388: ZB = zb_and(n3384, n3386);
    let n3389: ZB = zn_lt(n3382, zn_splat(P8::from_raw(0i32)));
    let n3390: ZB = zn_ge(n3382, zn_splat(P8::from_raw(0i32)));
    let n3391: ZB = zb_and(n3388, n3389);
    let n3392: ZB = zb_and(n3388, n3390);
    let n3393: ZN = zsel_n(n3387, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3394: ZB = zb_or(n3387, n3391);
    let n3395: ZN = zsel_n(n3392, zn_splat(P8::from_raw(0i32)), n3393);
    let n3396: ZB = zb_or(n3392, n3394);
    let n3397: ZB = zn_gt(n3383, zn_splat(P8::from_raw(0i32)));
    let n3398: ZB = zn_le(n3383, zn_splat(P8::from_raw(0i32)));
    let n3399: ZB = zb_and(n3396, n3397);
    let n3400: ZB = zb_and(n3396, n3398);
    let n3401: ZN = zsel_n(n3399, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3402: ZN = zsel_n(n3400, zn_splat(P8::from_raw(0i32)), n3401);
    let n3403: ZB = zb_or(n3399, n3400);
    let n3404: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3383);
    let n3405: ZB = zb_not(n3404);
    let n3406: ZB = zb_and(n3403, n3405);
    let n3407: ZB = zb_and(n3403, n3404);
    let n3408: ZN = zsel_n(n3406, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3409: ZB = zb_or(n3406, n3407);
    let n3410: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3382);
    let n3411: ZB = zb_not(n3410);
    let n3412: ZB = zb_and(n3409, n3411);
    let n3413: ZB = zb_and(n3409, n3410);
    let n3414: ZN = zsel_n(n3412, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3415: ZB = zb_or(n3412, n3413);
    let n3416: ZN = zsel_n(n3415, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3417: ZB = zb_or(n93, n3415);
    let n3418: ZN = zsel_n(n3415, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n3419: ZN = zsel_n(n3415, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n3420: ZN = zsel_n(n3415, n1577, r_c88);
    let n3421: ZN = zsel_n(n3415, n3408, zn_splat(P8::from_raw(0i32)));
    let n3422: ZN = zsel_n(n3415, n3414, zn_splat(P8::from_raw(0i32)));
    let n3423: ZN = zsel_n(n3415, n3395, zn_splat(P8::from_raw(0i32)));
    let n3424: ZN = zsel_n(n3415, n3402, zn_splat(P8::from_raw(0i32)));
    let n3425: ZN = zsel_n(n3415, n3382, n1523);
    let n3426: ZN = zsel_n(n3415, n3383, n1561);
    let n3427: ZB = zb_or(n2291, n3415);
    let n3428: ZN = zsel_n(n2364, n1578, n1579);
    let n3429: ZN = zsel_n(n2364, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n3430: ZN = zsel_n(n2365, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3431: ZN = zsel_n(n2365, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n3432: ZN = zsel_n(n2364, n3428, n3430);
    let n3433: ZN = zsel_n(n2364, n3429, n3431);
    let n3434: ZB = zb_or(n2364, n2365);
    let n3435: ZB = zn_gt(n3432, zn_splat(P8::from_raw(0i32)));
    let n3436: ZB = zn_le(n3432, zn_splat(P8::from_raw(0i32)));
    let n3437: ZB = zb_and(n3434, n3435);
    let n3438: ZB = zb_and(n3434, n3436);
    let n3439: ZB = zn_lt(n3432, zn_splat(P8::from_raw(0i32)));
    let n3440: ZB = zn_ge(n3432, zn_splat(P8::from_raw(0i32)));
    let n3441: ZB = zb_and(n3438, n3439);
    let n3442: ZB = zb_and(n3438, n3440);
    let n3443: ZB = zb_or(n3437, n3441);
    let n3444: ZB = zb_or(n3442, n3443);
    let n3445: ZB = zn_gt(n3433, zn_splat(P8::from_raw(0i32)));
    let n3446: ZB = zn_le(n3433, zn_splat(P8::from_raw(0i32)));
    let n3447: ZB = zb_and(n3444, n3445);
    let n3448: ZB = zb_and(n3444, n3446);
    let n3449: ZB = zb_or(n3447, n3448);
    let n3450: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3433);
    let n3451: ZB = zb_not(n3450);
    let n3452: ZB = zb_and(n3449, n3451);
    let n3453: ZB = zb_and(n3449, n3450);
    let n3454: ZB = zb_or(n3452, n3453);
    let n3455: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3432);
    let n3456: ZB = zb_not(n3455);
    let n3457: ZB = zb_and(n3454, n3456);
    let n3458: ZB = zb_and(n3454, n3455);
    let n3459: ZB = zb_or(n3457, n3458);
    let n3460: ZN = zsel_n(n3459, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3461: ZB = zb_or(n93, n3459);
    let n3462: ZB = zb_or(n2362, n3459);
    let n3463: ZB = zb_and(n1626, n3427);
    let n3464: ZB = zb_and(n1627, n3427);
    let n3465: ZB = zb_or(n3463, n3464);
    let n3466: ZB = zb_not(n3463);
    let n3467: ZB = zb_and(n3463, n3465);
    let n3468: ZB = zb_and(n3465, n3466);
    let n3469: ZB = zb_and(n1626, n3462);
    let n3470: ZB = zb_and(n1627, n3462);
    let n3471: ZB = zb_or(n3469, n3470);
    let n3472: ZB = zb_not(n3469);
    let n3473: ZB = zb_and(n3469, n3471);
    let n3474: ZB = zb_and(n3471, n3472);
    let n3475: ZN = zsel_n(n3467, n92, zn_splat(P8::from_raw(983040i32)));
    let n3476: ZN = zsel_n(n3467, n3416, n3460);
    let n3477: ZB = zb_not(n3467);
    let n3478: ZB = zb_or(n95, n3477);
    let n3479: ZB = zb_or(n3467, n3473);
    let n3480: ZB = zsel_b(n3467, n1463, n1472);
    let n3481: ZN = zsel_n(n734, r_c20, n3416);
    let n3482: ZB = zsel_b(n734, n93, n3417);
    let n3483: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3418);
    let n3484: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3419);
    let n3485: ZN = zsel_n(n734, r_c88, n3420);
    let n3486: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3421);
    let n3487: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3422);
    let n3488: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3423);
    let n3489: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3424);
    let n3490: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3425);
    let n3491: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3426);
    let n3492: ZB = zb_or(n734, n3468);
    let n3493: ZB = zn_gt(n3460, zn_splat(P8::from_raw(0i32)));
    let n3494: ZB = zn_le(n3460, zn_splat(P8::from_raw(0i32)));
    let n3495: ZB = zb_and(n3474, n3493);
    let n3496: ZB = zb_and(n3474, n3494);
    let n3497: ZB = zn_gt(n3481, zn_splat(P8::from_raw(0i32)));
    let n3498: ZB = zn_le(n3481, zn_splat(P8::from_raw(0i32)));
    let n3499: ZB = zb_and(n3492, n3497);
    let n3500: ZB = zb_and(n3492, n3498);
    let n3501: ZB = zb_and(n1682, n3500);
    let n3502: ZB = zb_and(n1681, n3500);
    let n3503: ZB = zb_or(n3501, n3502);
    let n3504: ZB = zb_not(n3501);
    let n3505: ZB = zb_or(n1686, n3504);
    let n3506: ZB = zb_not(n3505);
    let n3507: ZB = zb_and(n3503, n3505);
    let n3508: ZB = zb_and(n3503, n3506);
    let n3509: ZN = zsel_n(n3507, n1694, n653);
    let n3510: ZN = zsel_n(n3507, zn_splat(P8::from_raw(0i32)), n3490);
    let n3511: ZB = zb_or(n3507, n3508);
    let n3512: ZB = zn_gt(n3476, zn_splat(P8::from_raw(0i32)));
    let n3513: ZB = zn_le(n3476, zn_splat(P8::from_raw(0i32)));
    let n3514: ZB = zb_and(n3479, n3512);
    let n3515: ZB = zb_and(n3479, n3513);
    let n3516: ZB = zb_or(n3495, n3496);
    let n3517: ZN = zsel_n(n3499, n653, n3509);
    let n3518: ZN = zsel_n(n3499, n3490, n3510);
    let n3519: ZB = zb_or(n3499, n3511);
    let n3520: ZB = zb_or(n3514, n3515);
    let n3521: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3483);
    let n3522: ZN = zsel_n(n2476, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n3523: ZN = zsel_n(n2476, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n3524: ZN = zsel_n(n2476, n3522, zn_splat(P8::from_raw(65536i32)));
    let n3525: ZN = zsel_n(n2476, n3523, zn_splat(P8::from_raw(0i32)));
    let n3526: ZB = zn_gt(n3524, zn_splat(P8::from_raw(0i32)));
    let n3527: ZB = zn_le(n3524, zn_splat(P8::from_raw(0i32)));
    let n3528: ZB = zb_and(n2476, n3526);
    let n3529: ZB = zb_and(n2476, n3527);
    let n3530: ZB = zn_lt(n3524, zn_splat(P8::from_raw(0i32)));
    let n3531: ZB = zn_ge(n3524, zn_splat(P8::from_raw(0i32)));
    let n3532: ZB = zb_and(n3529, n3530);
    let n3533: ZB = zb_and(n3529, n3531);
    let n3534: ZN = zsel_n(n3528, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3535: ZB = zb_or(n3528, n3532);
    let n3536: ZN = zsel_n(n3533, zn_splat(P8::from_raw(0i32)), n3534);
    let n3537: ZB = zb_or(n3533, n3535);
    let n3538: ZB = zn_gt(n3525, zn_splat(P8::from_raw(0i32)));
    let n3539: ZB = zn_le(n3525, zn_splat(P8::from_raw(0i32)));
    let n3540: ZB = zb_and(n3537, n3538);
    let n3541: ZB = zb_and(n3537, n3539);
    let n3542: ZN = zsel_n(n3540, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3543: ZN = zsel_n(n3541, zn_splat(P8::from_raw(0i32)), n3542);
    let n3544: ZB = zb_or(n3540, n3541);
    let n3545: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3525);
    let n3546: ZB = zb_not(n3545);
    let n3547: ZB = zb_and(n3544, n3546);
    let n3548: ZB = zb_and(n3544, n3545);
    let n3549: ZN = zsel_n(n3547, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3550: ZB = zb_or(n3547, n3548);
    let n3551: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3524);
    let n3552: ZB = zb_not(n3551);
    let n3553: ZB = zb_and(n3550, n3552);
    let n3554: ZB = zb_and(n3550, n3551);
    let n3555: ZN = zsel_n(n3553, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3556: ZB = zb_or(n3553, n3554);
    let n3557: ZN = zsel_n(n3556, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3558: ZB = zb_or(n93, n3556);
    let n3559: ZN = zsel_n(n3556, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n3560: ZN = zsel_n(n3556, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n3561: ZN = zsel_n(n3556, n1577, r_c88);
    let n3562: ZN = zsel_n(n3556, n3549, zn_splat(P8::from_raw(0i32)));
    let n3563: ZN = zsel_n(n3556, n3555, zn_splat(P8::from_raw(0i32)));
    let n3564: ZN = zsel_n(n3556, n3536, zn_splat(P8::from_raw(0i32)));
    let n3565: ZN = zsel_n(n3556, n3543, zn_splat(P8::from_raw(0i32)));
    let n3566: ZN = zsel_n(n3556, n3524, n1708);
    let n3567: ZN = zsel_n(n3556, n3525, n1731);
    let n3568: ZB = zb_or(n2477, n3556);
    let n3569: ZN = zsel_n(n2535, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n3570: ZN = zsel_n(n2535, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n3571: ZN = zsel_n(n2535, n3569, zn_splat(P8::from_raw(65536i32)));
    let n3572: ZN = zsel_n(n2535, n3570, zn_splat(P8::from_raw(0i32)));
    let n3573: ZB = zn_gt(n3571, zn_splat(P8::from_raw(0i32)));
    let n3574: ZB = zn_le(n3571, zn_splat(P8::from_raw(0i32)));
    let n3575: ZB = zb_and(n2535, n3573);
    let n3576: ZB = zb_and(n2535, n3574);
    let n3577: ZB = zn_lt(n3571, zn_splat(P8::from_raw(0i32)));
    let n3578: ZB = zn_ge(n3571, zn_splat(P8::from_raw(0i32)));
    let n3579: ZB = zb_and(n3576, n3577);
    let n3580: ZB = zb_and(n3576, n3578);
    let n3581: ZB = zb_or(n3575, n3579);
    let n3582: ZB = zb_or(n3580, n3581);
    let n3583: ZB = zn_gt(n3572, zn_splat(P8::from_raw(0i32)));
    let n3584: ZB = zn_le(n3572, zn_splat(P8::from_raw(0i32)));
    let n3585: ZB = zb_and(n3582, n3583);
    let n3586: ZB = zb_and(n3582, n3584);
    let n3587: ZB = zb_or(n3585, n3586);
    let n3588: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3572);
    let n3589: ZB = zb_not(n3588);
    let n3590: ZB = zb_and(n3587, n3589);
    let n3591: ZB = zb_and(n3587, n3588);
    let n3592: ZB = zb_or(n3590, n3591);
    let n3593: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3571);
    let n3594: ZB = zb_not(n3593);
    let n3595: ZB = zb_and(n3592, n3594);
    let n3596: ZB = zb_and(n3592, n3593);
    let n3597: ZB = zb_or(n3595, n3596);
    let n3598: ZN = zsel_n(n3597, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3599: ZB = zb_or(n93, n3597);
    let n3600: ZB = zb_or(n2536, n3597);
    let n3601: ZB = zb_and(n1626, n3568);
    let n3602: ZB = zb_and(n1627, n3568);
    let n3603: ZB = zb_or(n3601, n3602);
    let n3604: ZB = zb_not(n3601);
    let n3605: ZB = zb_and(n3601, n3603);
    let n3606: ZB = zb_and(n3603, n3604);
    let n3607: ZB = zb_and(n1626, n3600);
    let n3608: ZB = zb_and(n1627, n3600);
    let n3609: ZB = zb_or(n3607, n3608);
    let n3610: ZB = zb_not(n3607);
    let n3611: ZB = zb_and(n3607, n3609);
    let n3612: ZB = zb_and(n3609, n3610);
    let n3613: ZN = zsel_n(n3605, n92, zn_splat(P8::from_raw(983040i32)));
    let n3614: ZN = zsel_n(n3605, n3557, n3598);
    let n3615: ZB = zb_not(n3605);
    let n3616: ZB = zb_or(n95, n3615);
    let n3617: ZB = zb_or(n3605, n3611);
    let n3618: ZB = zsel_b(n3605, n1463, n1472);
    let n3619: ZN = zsel_n(n734, r_c20, n3557);
    let n3620: ZB = zsel_b(n734, n93, n3558);
    let n3621: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3559);
    let n3622: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3560);
    let n3623: ZN = zsel_n(n734, r_c88, n3561);
    let n3624: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3562);
    let n3625: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3563);
    let n3626: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3564);
    let n3627: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3565);
    let n3628: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3566);
    let n3629: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3567);
    let n3630: ZB = zb_or(n734, n3606);
    let n3631: ZB = zn_gt(n3598, zn_splat(P8::from_raw(0i32)));
    let n3632: ZB = zn_le(n3598, zn_splat(P8::from_raw(0i32)));
    let n3633: ZB = zb_and(n3612, n3631);
    let n3634: ZB = zb_and(n3612, n3632);
    let n3635: ZB = zn_gt(n3619, zn_splat(P8::from_raw(0i32)));
    let n3636: ZB = zn_le(n3619, zn_splat(P8::from_raw(0i32)));
    let n3637: ZB = zb_and(n3630, n3635);
    let n3638: ZB = zb_and(n3630, n3636);
    let n3639: ZB = zb_and(n1682, n3638);
    let n3640: ZB = zb_and(n1681, n3638);
    let n3641: ZB = zb_or(n3639, n3640);
    let n3642: ZB = zb_not(n3639);
    let n3643: ZB = zb_or(n1686, n3642);
    let n3644: ZB = zb_not(n3643);
    let n3645: ZB = zb_and(n3641, n3643);
    let n3646: ZB = zb_and(n3641, n3644);
    let n3647: ZN = zsel_n(n3645, n1694, n653);
    let n3648: ZN = zsel_n(n3645, zn_splat(P8::from_raw(0i32)), n3628);
    let n3649: ZB = zb_or(n3645, n3646);
    let n3650: ZB = zn_gt(n3614, zn_splat(P8::from_raw(0i32)));
    let n3651: ZB = zn_le(n3614, zn_splat(P8::from_raw(0i32)));
    let n3652: ZB = zb_and(n3617, n3650);
    let n3653: ZB = zb_and(n3617, n3651);
    let n3654: ZB = zb_or(n3633, n3634);
    let n3655: ZN = zsel_n(n3637, n653, n3647);
    let n3656: ZN = zsel_n(n3637, n3628, n3648);
    let n3657: ZB = zb_or(n3637, n3649);
    let n3658: ZB = zb_or(n3652, n3653);
    let n3659: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3621);
    let n3660: ZN = zsel_n(n2641, n1847, n1848);
    let n3661: ZN = zsel_n(n2641, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n3662: ZN = zsel_n(n2642, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3663: ZN = zsel_n(n2642, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n3664: ZN = zsel_n(n2641, n3660, n3662);
    let n3665: ZN = zsel_n(n2641, n3661, n3663);
    let n3666: ZB = zb_or(n2641, n2642);
    let n3667: ZB = zn_gt(n3664, zn_splat(P8::from_raw(0i32)));
    let n3668: ZB = zn_le(n3664, zn_splat(P8::from_raw(0i32)));
    let n3669: ZB = zb_and(n3666, n3667);
    let n3670: ZB = zb_and(n3666, n3668);
    let n3671: ZB = zn_lt(n3664, zn_splat(P8::from_raw(0i32)));
    let n3672: ZB = zn_ge(n3664, zn_splat(P8::from_raw(0i32)));
    let n3673: ZB = zb_and(n3670, n3671);
    let n3674: ZB = zb_and(n3670, n3672);
    let n3675: ZN = zsel_n(n3669, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3676: ZB = zb_or(n3669, n3673);
    let n3677: ZN = zsel_n(n3674, zn_splat(P8::from_raw(0i32)), n3675);
    let n3678: ZB = zb_or(n3674, n3676);
    let n3679: ZB = zn_gt(n3665, zn_splat(P8::from_raw(0i32)));
    let n3680: ZB = zn_le(n3665, zn_splat(P8::from_raw(0i32)));
    let n3681: ZB = zb_and(n3678, n3679);
    let n3682: ZB = zb_and(n3678, n3680);
    let n3683: ZN = zsel_n(n3681, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3684: ZN = zsel_n(n3682, zn_splat(P8::from_raw(0i32)), n3683);
    let n3685: ZB = zb_or(n3681, n3682);
    let n3686: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3665);
    let n3687: ZB = zb_not(n3686);
    let n3688: ZB = zb_and(n3685, n3687);
    let n3689: ZB = zb_and(n3685, n3686);
    let n3690: ZN = zsel_n(n3688, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3691: ZB = zb_or(n3688, n3689);
    let n3692: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3664);
    let n3693: ZB = zb_not(n3692);
    let n3694: ZB = zb_and(n3691, n3693);
    let n3695: ZB = zb_and(n3691, n3692);
    let n3696: ZN = zsel_n(n3694, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3697: ZB = zb_or(n3694, n3695);
    let n3698: ZN = zsel_n(n3697, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3699: ZB = zb_or(n93, n3697);
    let n3700: ZN = zsel_n(n3697, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n3701: ZN = zsel_n(n3697, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n3702: ZN = zsel_n(n3697, n1577, r_c88);
    let n3703: ZN = zsel_n(n3697, n3690, zn_splat(P8::from_raw(0i32)));
    let n3704: ZN = zsel_n(n3697, n3696, zn_splat(P8::from_raw(0i32)));
    let n3705: ZN = zsel_n(n3697, n3677, zn_splat(P8::from_raw(0i32)));
    let n3706: ZN = zsel_n(n3697, n3684, zn_splat(P8::from_raw(0i32)));
    let n3707: ZN = zsel_n(n3697, n3664, n1806);
    let n3708: ZN = zsel_n(n3697, n3665, n1842);
    let n3709: ZB = zb_or(n2639, n3697);
    let n3710: ZN = zsel_n(n2712, n1847, n1848);
    let n3711: ZN = zsel_n(n2712, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n3712: ZN = zsel_n(n2713, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3713: ZN = zsel_n(n2713, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n3714: ZN = zsel_n(n2712, n3710, n3712);
    let n3715: ZN = zsel_n(n2712, n3711, n3713);
    let n3716: ZB = zb_or(n2712, n2713);
    let n3717: ZB = zn_gt(n3714, zn_splat(P8::from_raw(0i32)));
    let n3718: ZB = zn_le(n3714, zn_splat(P8::from_raw(0i32)));
    let n3719: ZB = zb_and(n3716, n3717);
    let n3720: ZB = zb_and(n3716, n3718);
    let n3721: ZB = zn_lt(n3714, zn_splat(P8::from_raw(0i32)));
    let n3722: ZB = zn_ge(n3714, zn_splat(P8::from_raw(0i32)));
    let n3723: ZB = zb_and(n3720, n3721);
    let n3724: ZB = zb_and(n3720, n3722);
    let n3725: ZB = zb_or(n3719, n3723);
    let n3726: ZB = zb_or(n3724, n3725);
    let n3727: ZB = zn_gt(n3715, zn_splat(P8::from_raw(0i32)));
    let n3728: ZB = zn_le(n3715, zn_splat(P8::from_raw(0i32)));
    let n3729: ZB = zb_and(n3726, n3727);
    let n3730: ZB = zb_and(n3726, n3728);
    let n3731: ZB = zb_or(n3729, n3730);
    let n3732: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3715);
    let n3733: ZB = zb_not(n3732);
    let n3734: ZB = zb_and(n3731, n3733);
    let n3735: ZB = zb_and(n3731, n3732);
    let n3736: ZB = zb_or(n3734, n3735);
    let n3737: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3714);
    let n3738: ZB = zb_not(n3737);
    let n3739: ZB = zb_and(n3736, n3738);
    let n3740: ZB = zb_and(n3736, n3737);
    let n3741: ZB = zb_or(n3739, n3740);
    let n3742: ZN = zsel_n(n3741, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3743: ZB = zb_or(n93, n3741);
    let n3744: ZB = zb_or(n2710, n3741);
    let n3745: ZB = zb_and(n1626, n3709);
    let n3746: ZB = zb_and(n1627, n3709);
    let n3747: ZB = zb_or(n3745, n3746);
    let n3748: ZB = zb_not(n3745);
    let n3749: ZB = zb_and(n3745, n3747);
    let n3750: ZB = zb_and(n3747, n3748);
    let n3751: ZB = zb_and(n1626, n3744);
    let n3752: ZB = zb_and(n1627, n3744);
    let n3753: ZB = zb_or(n3751, n3752);
    let n3754: ZB = zb_not(n3751);
    let n3755: ZB = zb_and(n3751, n3753);
    let n3756: ZB = zb_and(n3753, n3754);
    let n3757: ZN = zsel_n(n3749, n92, zn_splat(P8::from_raw(983040i32)));
    let n3758: ZN = zsel_n(n3749, n3698, n3742);
    let n3759: ZB = zb_not(n3749);
    let n3760: ZB = zb_or(n95, n3759);
    let n3761: ZB = zb_or(n3749, n3755);
    let n3762: ZB = zsel_b(n3749, n1463, n1472);
    let n3763: ZN = zsel_n(n734, r_c20, n3698);
    let n3764: ZB = zsel_b(n734, n93, n3699);
    let n3765: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3700);
    let n3766: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3701);
    let n3767: ZN = zsel_n(n734, r_c88, n3702);
    let n3768: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3703);
    let n3769: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3704);
    let n3770: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3705);
    let n3771: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3706);
    let n3772: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3707);
    let n3773: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3708);
    let n3774: ZB = zb_or(n734, n3750);
    let n3775: ZB = zn_gt(n3742, zn_splat(P8::from_raw(0i32)));
    let n3776: ZB = zn_le(n3742, zn_splat(P8::from_raw(0i32)));
    let n3777: ZB = zb_and(n3756, n3775);
    let n3778: ZB = zb_and(n3756, n3776);
    let n3779: ZB = zn_gt(n3763, zn_splat(P8::from_raw(0i32)));
    let n3780: ZB = zn_le(n3763, zn_splat(P8::from_raw(0i32)));
    let n3781: ZB = zb_and(n3774, n3779);
    let n3782: ZB = zb_and(n3774, n3780);
    let n3783: ZB = zb_and(n1682, n3782);
    let n3784: ZB = zb_and(n1681, n3782);
    let n3785: ZB = zb_or(n3783, n3784);
    let n3786: ZB = zb_not(n3783);
    let n3787: ZB = zb_or(n1686, n3786);
    let n3788: ZB = zb_not(n3787);
    let n3789: ZB = zb_and(n3785, n3787);
    let n3790: ZB = zb_and(n3785, n3788);
    let n3791: ZN = zsel_n(n3789, n1694, n653);
    let n3792: ZN = zsel_n(n3789, zn_splat(P8::from_raw(0i32)), n3772);
    let n3793: ZB = zb_or(n3789, n3790);
    let n3794: ZB = zn_gt(n3758, zn_splat(P8::from_raw(0i32)));
    let n3795: ZB = zn_le(n3758, zn_splat(P8::from_raw(0i32)));
    let n3796: ZB = zb_and(n3761, n3794);
    let n3797: ZB = zb_and(n3761, n3795);
    let n3798: ZB = zb_or(n3777, n3778);
    let n3799: ZN = zsel_n(n3781, n653, n3791);
    let n3800: ZN = zsel_n(n3781, n3772, n3792);
    let n3801: ZB = zb_or(n3781, n3793);
    let n3802: ZB = zb_or(n3796, n3797);
    let n3803: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3765);
    let n3804: ZB = zb_and(n1483, n1967);
    let n3805: ZB = zb_not(n3804);
    let n3806: ZB = zb_and(n1969, n3804);
    let n3807: ZB = zb_and(n1969, n3805);
    let n3808: ZN = zsel_n(n3806, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3809: ZB = zb_and(n1534, n3806);
    let n3810: ZB = zb_and(n1533, n3806);
    let n3811: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3808);
    let n3812: ZB = zb_not(n3811);
    let n3813: ZB = zb_and(n3809, n3812);
    let n3814: ZB = zb_and(n3809, n3811);
    let n3815: ZN = zn_mul(n3808, zn_splat(P8::from_raw(231700i32)));
    let n3816: ZN = zsel_n(n3813, n1578, n1579);
    let n3817: ZN = zsel_n(n3813, n3815, zn_splat(P8::from_raw(0i32)));
    let n3818: ZB = zb_or(n3813, n3814);
    let n3819: ZB = zb_and(n3810, n3812);
    let n3820: ZB = zb_and(n3810, n3811);
    let n3821: ZN = zn_mul(n3808, zn_splat(P8::from_raw(327680i32)));
    let n3822: ZB = zb_and(n1530, n3820);
    let n3823: ZB = zb_and(n1970, n3820);
    let n3824: ZN = zsel_n(n3822, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3825: ZB = zb_or(n3822, n3823);
    let n3826: ZN = zsel_n(n3819, zn_splat(P8::from_raw(0i32)), n3824);
    let n3827: ZN = zsel_n(n3819, n3821, zn_splat(P8::from_raw(0i32)));
    let n3828: ZB = zb_or(n3819, n3825);
    let n3829: ZN = zsel_n(n3818, n3816, n3826);
    let n3830: ZN = zsel_n(n3818, n3817, n3827);
    let n3831: ZB = zb_or(n3818, n3828);
    let n3832: ZB = zn_gt(n3829, zn_splat(P8::from_raw(0i32)));
    let n3833: ZB = zn_le(n3829, zn_splat(P8::from_raw(0i32)));
    let n3834: ZB = zb_and(n3831, n3832);
    let n3835: ZB = zb_and(n3831, n3833);
    let n3836: ZB = zn_lt(n3829, zn_splat(P8::from_raw(0i32)));
    let n3837: ZB = zn_ge(n3829, zn_splat(P8::from_raw(0i32)));
    let n3838: ZB = zb_and(n3835, n3836);
    let n3839: ZB = zb_and(n3835, n3837);
    let n3840: ZN = zsel_n(n3834, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3841: ZB = zb_or(n3834, n3838);
    let n3842: ZN = zsel_n(n3839, zn_splat(P8::from_raw(0i32)), n3840);
    let n3843: ZB = zb_or(n3839, n3841);
    let n3844: ZB = zn_gt(n3830, zn_splat(P8::from_raw(0i32)));
    let n3845: ZB = zn_le(n3830, zn_splat(P8::from_raw(0i32)));
    let n3846: ZB = zb_and(n3843, n3844);
    let n3847: ZB = zb_and(n3843, n3845);
    let n3848: ZN = zsel_n(n3846, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n3849: ZN = zsel_n(n3847, zn_splat(P8::from_raw(0i32)), n3848);
    let n3850: ZB = zb_or(n3846, n3847);
    let n3851: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3830);
    let n3852: ZB = zb_not(n3851);
    let n3853: ZB = zb_and(n3850, n3852);
    let n3854: ZB = zb_and(n3850, n3851);
    let n3855: ZN = zsel_n(n3853, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3856: ZB = zb_or(n3853, n3854);
    let n3857: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3829);
    let n3858: ZB = zb_not(n3857);
    let n3859: ZB = zb_and(n3856, n3858);
    let n3860: ZB = zb_and(n3856, n3857);
    let n3861: ZN = zsel_n(n3859, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n3862: ZB = zb_or(n3859, n3860);
    let n3863: ZN = zsel_n(n3862, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3864: ZB = zb_or(n93, n3862);
    let n3865: ZN = zsel_n(n3862, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n3866: ZN = zsel_n(n3862, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n3867: ZN = zsel_n(n3862, n1577, r_c88);
    let n3868: ZN = zsel_n(n3862, n3855, zn_splat(P8::from_raw(0i32)));
    let n3869: ZN = zsel_n(n3862, n3861, zn_splat(P8::from_raw(0i32)));
    let n3870: ZN = zsel_n(n3862, n3842, zn_splat(P8::from_raw(0i32)));
    let n3871: ZN = zsel_n(n3862, n3849, zn_splat(P8::from_raw(0i32)));
    let n3872: ZN = zsel_n(n3862, n3829, n1965);
    let n3873: ZN = zsel_n(n3862, n3830, n1966);
    let n3874: ZB = zb_or(n3807, n3862);
    let n3875: ZB = zb_and(n1491, n2008);
    let n3876: ZB = zb_not(n3875);
    let n3877: ZB = zb_and(n2010, n3875);
    let n3878: ZB = zb_and(n2010, n3876);
    let n3879: ZN = zsel_n(n3877, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3880: ZB = zb_and(n1534, n3877);
    let n3881: ZB = zb_and(n1533, n3877);
    let n3882: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3879);
    let n3883: ZB = zb_not(n3882);
    let n3884: ZB = zb_and(n3880, n3883);
    let n3885: ZB = zb_and(n3880, n3882);
    let n3886: ZN = zn_mul(n3879, zn_splat(P8::from_raw(231700i32)));
    let n3887: ZN = zsel_n(n3884, n1578, n1579);
    let n3888: ZN = zsel_n(n3884, n3886, zn_splat(P8::from_raw(0i32)));
    let n3889: ZB = zb_or(n3884, n3885);
    let n3890: ZB = zb_and(n3881, n3883);
    let n3891: ZB = zb_and(n3881, n3882);
    let n3892: ZN = zn_mul(n3879, zn_splat(P8::from_raw(327680i32)));
    let n3893: ZB = zb_and(n1972, n3891);
    let n3894: ZB = zb_and(n2011, n3891);
    let n3895: ZN = zsel_n(n3893, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n3896: ZB = zb_or(n3893, n3894);
    let n3897: ZN = zsel_n(n3890, zn_splat(P8::from_raw(0i32)), n3895);
    let n3898: ZN = zsel_n(n3890, n3892, zn_splat(P8::from_raw(0i32)));
    let n3899: ZB = zb_or(n3890, n3896);
    let n3900: ZN = zsel_n(n3889, n3887, n3897);
    let n3901: ZN = zsel_n(n3889, n3888, n3898);
    let n3902: ZB = zb_or(n3889, n3899);
    let n3903: ZB = zn_gt(n3900, zn_splat(P8::from_raw(0i32)));
    let n3904: ZB = zn_le(n3900, zn_splat(P8::from_raw(0i32)));
    let n3905: ZB = zb_and(n3902, n3903);
    let n3906: ZB = zb_and(n3902, n3904);
    let n3907: ZB = zn_lt(n3900, zn_splat(P8::from_raw(0i32)));
    let n3908: ZB = zn_ge(n3900, zn_splat(P8::from_raw(0i32)));
    let n3909: ZB = zb_and(n3906, n3907);
    let n3910: ZB = zb_and(n3906, n3908);
    let n3911: ZB = zb_or(n3905, n3909);
    let n3912: ZB = zb_or(n3910, n3911);
    let n3913: ZB = zn_gt(n3901, zn_splat(P8::from_raw(0i32)));
    let n3914: ZB = zn_le(n3901, zn_splat(P8::from_raw(0i32)));
    let n3915: ZB = zb_and(n3912, n3913);
    let n3916: ZB = zb_and(n3912, n3914);
    let n3917: ZB = zb_or(n3915, n3916);
    let n3918: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3901);
    let n3919: ZB = zb_not(n3918);
    let n3920: ZB = zb_and(n3917, n3919);
    let n3921: ZB = zb_and(n3917, n3918);
    let n3922: ZB = zb_or(n3920, n3921);
    let n3923: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3900);
    let n3924: ZB = zb_not(n3923);
    let n3925: ZB = zb_and(n3922, n3924);
    let n3926: ZB = zb_and(n3922, n3923);
    let n3927: ZB = zb_or(n3925, n3926);
    let n3928: ZN = zsel_n(n3927, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3929: ZB = zb_or(n93, n3927);
    let n3930: ZB = zb_or(n3878, n3927);
    let n3931: ZB = zb_and(n1626, n3874);
    let n3932: ZB = zb_and(n1627, n3874);
    let n3933: ZB = zb_or(n3931, n3932);
    let n3934: ZB = zb_not(n3931);
    let n3935: ZB = zb_and(n3931, n3933);
    let n3936: ZB = zb_and(n3933, n3934);
    let n3937: ZB = zb_and(n1626, n3930);
    let n3938: ZB = zb_and(n1627, n3930);
    let n3939: ZB = zb_or(n3937, n3938);
    let n3940: ZB = zb_not(n3937);
    let n3941: ZB = zb_and(n3937, n3939);
    let n3942: ZB = zb_and(n3939, n3940);
    let n3943: ZN = zsel_n(n3935, n92, zn_splat(P8::from_raw(983040i32)));
    let n3944: ZN = zsel_n(n3935, n3863, n3928);
    let n3945: ZB = zb_not(n3935);
    let n3946: ZB = zb_or(n95, n3945);
    let n3947: ZB = zb_or(n3935, n3941);
    let n3948: ZB = zsel_b(n3935, n1463, n1472);
    let n3949: ZN = zsel_n(n734, r_c20, n3863);
    let n3950: ZB = zsel_b(n734, n93, n3864);
    let n3951: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3865);
    let n3952: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3866);
    let n3953: ZN = zsel_n(n734, r_c88, n3867);
    let n3954: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3868);
    let n3955: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3869);
    let n3956: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3870);
    let n3957: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3871);
    let n3958: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3872);
    let n3959: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n3873);
    let n3960: ZB = zb_or(n734, n3936);
    let n3961: ZB = zn_gt(n3928, zn_splat(P8::from_raw(0i32)));
    let n3962: ZB = zn_le(n3928, zn_splat(P8::from_raw(0i32)));
    let n3963: ZB = zb_and(n3942, n3961);
    let n3964: ZB = zb_and(n3942, n3962);
    let n3965: ZB = zn_gt(n3949, zn_splat(P8::from_raw(0i32)));
    let n3966: ZB = zn_le(n3949, zn_splat(P8::from_raw(0i32)));
    let n3967: ZB = zb_and(n3960, n3965);
    let n3968: ZB = zb_and(n3960, n3966);
    let n3969: ZB = zb_and(n1682, n3968);
    let n3970: ZB = zb_and(n1681, n3968);
    let n3971: ZB = zb_or(n3969, n3970);
    let n3972: ZB = zb_not(n3969);
    let n3973: ZB = zb_or(n1686, n3972);
    let n3974: ZB = zb_not(n3973);
    let n3975: ZB = zb_and(n3971, n3973);
    let n3976: ZB = zb_and(n3971, n3974);
    let n3977: ZN = zsel_n(n3975, n1694, n653);
    let n3978: ZN = zsel_n(n3975, zn_splat(P8::from_raw(0i32)), n3958);
    let n3979: ZB = zb_or(n3975, n3976);
    let n3980: ZB = zn_gt(n3944, zn_splat(P8::from_raw(0i32)));
    let n3981: ZB = zn_le(n3944, zn_splat(P8::from_raw(0i32)));
    let n3982: ZB = zb_and(n3947, n3980);
    let n3983: ZB = zb_and(n3947, n3981);
    let n3984: ZB = zb_or(n3963, n3964);
    let n3985: ZN = zsel_n(n3967, n653, n3977);
    let n3986: ZN = zsel_n(n3967, n3958, n3978);
    let n3987: ZB = zb_or(n3967, n3979);
    let n3988: ZB = zb_or(n3982, n3983);
    let n3989: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n3951);
    let n3990: ZB = zb_and(n1483, n2091);
    let n3991: ZB = zb_not(n3990);
    let n3992: ZB = zb_and(n2093, n3990);
    let n3993: ZB = zb_and(n2093, n3991);
    let n3994: ZN = zsel_n(n3992, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n3995: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n3994);
    let n3996: ZB = zb_not(n3995);
    let n3997: ZB = zb_and(n3992, n3996);
    let n3998: ZB = zb_and(n3992, n3995);
    let n3999: ZN = zn_mul(n3994, zn_splat(P8::from_raw(231700i32)));
    let n4000: ZN = zsel_n(n3997, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n4001: ZN = zsel_n(n3997, n3999, zn_splat(P8::from_raw(0i32)));
    let n4002: ZB = zb_or(n3997, n3998);
    let n4004: ZN = zsel_n(n4002, n4000, zn_splat(P8::from_raw(65536i32)));
    let n4005: ZN = zsel_n(n4002, n4001, zn_splat(P8::from_raw(0i32)));
    let n4006: ZB = zn_gt(n4004, zn_splat(P8::from_raw(0i32)));
    let n4007: ZB = zn_le(n4004, zn_splat(P8::from_raw(0i32)));
    let n4008: ZB = zb_and(n4002, n4006);
    let n4009: ZB = zb_and(n4002, n4007);
    let n4010: ZB = zn_lt(n4004, zn_splat(P8::from_raw(0i32)));
    let n4011: ZB = zn_ge(n4004, zn_splat(P8::from_raw(0i32)));
    let n4012: ZB = zb_and(n4009, n4010);
    let n4013: ZB = zb_and(n4009, n4011);
    let n4014: ZN = zsel_n(n4008, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4015: ZB = zb_or(n4008, n4012);
    let n4016: ZN = zsel_n(n4013, zn_splat(P8::from_raw(0i32)), n4014);
    let n4017: ZB = zb_or(n4013, n4015);
    let n4018: ZB = zn_gt(n4005, zn_splat(P8::from_raw(0i32)));
    let n4019: ZB = zn_le(n4005, zn_splat(P8::from_raw(0i32)));
    let n4020: ZB = zb_and(n4017, n4018);
    let n4021: ZB = zb_and(n4017, n4019);
    let n4022: ZN = zsel_n(n4020, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4023: ZN = zsel_n(n4021, zn_splat(P8::from_raw(0i32)), n4022);
    let n4024: ZB = zb_or(n4020, n4021);
    let n4025: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4005);
    let n4026: ZB = zb_not(n4025);
    let n4027: ZB = zb_and(n4024, n4026);
    let n4028: ZB = zb_and(n4024, n4025);
    let n4029: ZN = zsel_n(n4027, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4030: ZB = zb_or(n4027, n4028);
    let n4031: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4004);
    let n4032: ZB = zb_not(n4031);
    let n4033: ZB = zb_and(n4030, n4032);
    let n4034: ZB = zb_and(n4030, n4031);
    let n4035: ZN = zsel_n(n4033, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4036: ZB = zb_or(n4033, n4034);
    let n4037: ZN = zsel_n(n4036, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4038: ZB = zb_or(n93, n4036);
    let n4039: ZN = zsel_n(n4036, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n4040: ZN = zsel_n(n4036, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n4041: ZN = zsel_n(n4036, n1577, r_c88);
    let n4042: ZN = zsel_n(n4036, n4029, zn_splat(P8::from_raw(0i32)));
    let n4043: ZN = zsel_n(n4036, n4035, zn_splat(P8::from_raw(0i32)));
    let n4044: ZN = zsel_n(n4036, n4016, zn_splat(P8::from_raw(0i32)));
    let n4045: ZN = zsel_n(n4036, n4023, zn_splat(P8::from_raw(0i32)));
    let n4046: ZN = zsel_n(n4036, n4004, n2089);
    let n4047: ZN = zsel_n(n4036, n4005, n2090);
    let n4048: ZB = zb_or(n3993, n4036);
    let n4049: ZB = zb_and(n1491, n2128);
    let n4050: ZB = zb_not(n4049);
    let n4051: ZB = zb_and(n2130, n4049);
    let n4052: ZB = zb_and(n2130, n4050);
    let n4053: ZN = zsel_n(n4051, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n4054: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4053);
    let n4055: ZB = zb_not(n4054);
    let n4056: ZB = zb_and(n4051, n4055);
    let n4057: ZB = zb_and(n4051, n4054);
    let n4058: ZN = zn_mul(n4053, zn_splat(P8::from_raw(231700i32)));
    let n4059: ZN = zsel_n(n4056, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n4060: ZN = zsel_n(n4056, n4058, zn_splat(P8::from_raw(0i32)));
    let n4061: ZB = zb_or(n4056, n4057);
    let n4063: ZN = zsel_n(n4061, n4059, zn_splat(P8::from_raw(65536i32)));
    let n4064: ZN = zsel_n(n4061, n4060, zn_splat(P8::from_raw(0i32)));
    let n4065: ZB = zn_gt(n4063, zn_splat(P8::from_raw(0i32)));
    let n4066: ZB = zn_le(n4063, zn_splat(P8::from_raw(0i32)));
    let n4067: ZB = zb_and(n4061, n4065);
    let n4068: ZB = zb_and(n4061, n4066);
    let n4069: ZB = zn_lt(n4063, zn_splat(P8::from_raw(0i32)));
    let n4070: ZB = zn_ge(n4063, zn_splat(P8::from_raw(0i32)));
    let n4071: ZB = zb_and(n4068, n4069);
    let n4072: ZB = zb_and(n4068, n4070);
    let n4073: ZB = zb_or(n4067, n4071);
    let n4074: ZB = zb_or(n4072, n4073);
    let n4075: ZB = zn_gt(n4064, zn_splat(P8::from_raw(0i32)));
    let n4076: ZB = zn_le(n4064, zn_splat(P8::from_raw(0i32)));
    let n4077: ZB = zb_and(n4074, n4075);
    let n4078: ZB = zb_and(n4074, n4076);
    let n4079: ZB = zb_or(n4077, n4078);
    let n4080: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4064);
    let n4081: ZB = zb_not(n4080);
    let n4082: ZB = zb_and(n4079, n4081);
    let n4083: ZB = zb_and(n4079, n4080);
    let n4084: ZB = zb_or(n4082, n4083);
    let n4085: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4063);
    let n4086: ZB = zb_not(n4085);
    let n4087: ZB = zb_and(n4084, n4086);
    let n4088: ZB = zb_and(n4084, n4085);
    let n4089: ZB = zb_or(n4087, n4088);
    let n4090: ZN = zsel_n(n4089, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4091: ZB = zb_or(n93, n4089);
    let n4092: ZB = zb_or(n4052, n4089);
    let n4093: ZB = zb_and(n1626, n4048);
    let n4094: ZB = zb_and(n1627, n4048);
    let n4095: ZB = zb_or(n4093, n4094);
    let n4096: ZB = zb_not(n4093);
    let n4097: ZB = zb_and(n4093, n4095);
    let n4098: ZB = zb_and(n4095, n4096);
    let n4099: ZB = zb_and(n1626, n4092);
    let n4100: ZB = zb_and(n1627, n4092);
    let n4101: ZB = zb_or(n4099, n4100);
    let n4102: ZB = zb_not(n4099);
    let n4103: ZB = zb_and(n4099, n4101);
    let n4104: ZB = zb_and(n4101, n4102);
    let n4105: ZN = zsel_n(n4097, n92, zn_splat(P8::from_raw(983040i32)));
    let n4106: ZN = zsel_n(n4097, n4037, n4090);
    let n4107: ZB = zb_not(n4097);
    let n4108: ZB = zb_or(n95, n4107);
    let n4109: ZB = zb_or(n4097, n4103);
    let n4110: ZB = zsel_b(n4097, n1463, n1472);
    let n4111: ZN = zsel_n(n734, r_c20, n4037);
    let n4112: ZB = zsel_b(n734, n93, n4038);
    let n4113: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4039);
    let n4114: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4040);
    let n4115: ZN = zsel_n(n734, r_c88, n4041);
    let n4116: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4042);
    let n4117: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4043);
    let n4118: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4044);
    let n4119: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4045);
    let n4120: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4046);
    let n4121: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4047);
    let n4122: ZB = zb_or(n734, n4098);
    let n4123: ZB = zn_gt(n4090, zn_splat(P8::from_raw(0i32)));
    let n4124: ZB = zn_le(n4090, zn_splat(P8::from_raw(0i32)));
    let n4125: ZB = zb_and(n4104, n4123);
    let n4126: ZB = zb_and(n4104, n4124);
    let n4127: ZB = zn_gt(n4111, zn_splat(P8::from_raw(0i32)));
    let n4128: ZB = zn_le(n4111, zn_splat(P8::from_raw(0i32)));
    let n4129: ZB = zb_and(n4122, n4127);
    let n4130: ZB = zb_and(n4122, n4128);
    let n4131: ZB = zb_and(n1682, n4130);
    let n4132: ZB = zb_and(n1681, n4130);
    let n4133: ZB = zb_or(n4131, n4132);
    let n4134: ZB = zb_not(n4131);
    let n4135: ZB = zb_or(n1686, n4134);
    let n4136: ZB = zb_not(n4135);
    let n4137: ZB = zb_and(n4133, n4135);
    let n4138: ZB = zb_and(n4133, n4136);
    let n4139: ZN = zsel_n(n4137, n1694, n653);
    let n4140: ZN = zsel_n(n4137, zn_splat(P8::from_raw(0i32)), n4120);
    let n4141: ZB = zb_or(n4137, n4138);
    let n4142: ZB = zn_gt(n4106, zn_splat(P8::from_raw(0i32)));
    let n4143: ZB = zn_le(n4106, zn_splat(P8::from_raw(0i32)));
    let n4144: ZB = zb_and(n4109, n4142);
    let n4145: ZB = zb_and(n4109, n4143);
    let n4146: ZB = zb_or(n4125, n4126);
    let n4147: ZN = zsel_n(n4129, n653, n4139);
    let n4148: ZN = zsel_n(n4129, n4120, n4140);
    let n4149: ZB = zb_or(n4129, n4141);
    let n4150: ZB = zb_or(n4144, n4145);
    let n4151: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4113);
    let n4152: ZB = zb_and(n1483, n2210);
    let n4153: ZB = zb_not(n4152);
    let n4154: ZB = zb_and(n2212, n4152);
    let n4155: ZB = zb_and(n2212, n4153);
    let n4156: ZN = zsel_n(n4154, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n4157: ZB = zb_and(n1816, n4154);
    let n4158: ZB = zb_and(n1815, n4154);
    let n4159: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4156);
    let n4160: ZB = zb_not(n4159);
    let n4161: ZB = zb_and(n4157, n4160);
    let n4162: ZB = zb_and(n4157, n4159);
    let n4163: ZN = zn_mul(n4156, zn_splat(P8::from_raw(231700i32)));
    let n4164: ZN = zsel_n(n4161, n1847, n1848);
    let n4165: ZN = zsel_n(n4161, n4163, zn_splat(P8::from_raw(0i32)));
    let n4166: ZB = zb_or(n4161, n4162);
    let n4167: ZB = zb_and(n4158, n4160);
    let n4168: ZB = zb_and(n4158, n4159);
    let n4169: ZN = zn_mul(n4156, zn_splat(P8::from_raw(327680i32)));
    let n4170: ZB = zb_and(n1813, n4168);
    let n4171: ZB = zb_and(n2213, n4168);
    let n4172: ZN = zsel_n(n4170, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4173: ZB = zb_or(n4170, n4171);
    let n4174: ZN = zsel_n(n4167, zn_splat(P8::from_raw(0i32)), n4172);
    let n4175: ZN = zsel_n(n4167, n4169, zn_splat(P8::from_raw(0i32)));
    let n4176: ZB = zb_or(n4167, n4173);
    let n4177: ZN = zsel_n(n4166, n4164, n4174);
    let n4178: ZN = zsel_n(n4166, n4165, n4175);
    let n4179: ZB = zb_or(n4166, n4176);
    let n4180: ZB = zn_gt(n4177, zn_splat(P8::from_raw(0i32)));
    let n4181: ZB = zn_le(n4177, zn_splat(P8::from_raw(0i32)));
    let n4182: ZB = zb_and(n4179, n4180);
    let n4183: ZB = zb_and(n4179, n4181);
    let n4184: ZB = zn_lt(n4177, zn_splat(P8::from_raw(0i32)));
    let n4185: ZB = zn_ge(n4177, zn_splat(P8::from_raw(0i32)));
    let n4186: ZB = zb_and(n4183, n4184);
    let n4187: ZB = zb_and(n4183, n4185);
    let n4188: ZN = zsel_n(n4182, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4189: ZB = zb_or(n4182, n4186);
    let n4190: ZN = zsel_n(n4187, zn_splat(P8::from_raw(0i32)), n4188);
    let n4191: ZB = zb_or(n4187, n4189);
    let n4192: ZB = zn_gt(n4178, zn_splat(P8::from_raw(0i32)));
    let n4193: ZB = zn_le(n4178, zn_splat(P8::from_raw(0i32)));
    let n4194: ZB = zb_and(n4191, n4192);
    let n4195: ZB = zb_and(n4191, n4193);
    let n4196: ZN = zsel_n(n4194, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4197: ZN = zsel_n(n4195, zn_splat(P8::from_raw(0i32)), n4196);
    let n4198: ZB = zb_or(n4194, n4195);
    let n4199: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4178);
    let n4200: ZB = zb_not(n4199);
    let n4201: ZB = zb_and(n4198, n4200);
    let n4202: ZB = zb_and(n4198, n4199);
    let n4203: ZN = zsel_n(n4201, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4204: ZB = zb_or(n4201, n4202);
    let n4205: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4177);
    let n4206: ZB = zb_not(n4205);
    let n4207: ZB = zb_and(n4204, n4206);
    let n4208: ZB = zb_and(n4204, n4205);
    let n4209: ZN = zsel_n(n4207, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4210: ZB = zb_or(n4207, n4208);
    let n4211: ZN = zsel_n(n4210, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4212: ZB = zb_or(n93, n4210);
    let n4213: ZN = zsel_n(n4210, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n4214: ZN = zsel_n(n4210, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n4215: ZN = zsel_n(n4210, n1577, r_c88);
    let n4216: ZN = zsel_n(n4210, n4203, zn_splat(P8::from_raw(0i32)));
    let n4217: ZN = zsel_n(n4210, n4209, zn_splat(P8::from_raw(0i32)));
    let n4218: ZN = zsel_n(n4210, n4190, zn_splat(P8::from_raw(0i32)));
    let n4219: ZN = zsel_n(n4210, n4197, zn_splat(P8::from_raw(0i32)));
    let n4220: ZN = zsel_n(n4210, n4177, n2208);
    let n4221: ZN = zsel_n(n4210, n4178, n2209);
    let n4222: ZB = zb_or(n4155, n4210);
    let n4223: ZB = zb_and(n1491, n2249);
    let n4224: ZB = zb_not(n4223);
    let n4225: ZB = zb_and(n2251, n4223);
    let n4226: ZB = zb_and(n2251, n4224);
    let n4227: ZN = zsel_n(n4225, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n4228: ZB = zb_and(n1816, n4225);
    let n4229: ZB = zb_and(n1815, n4225);
    let n4230: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4227);
    let n4231: ZB = zb_not(n4230);
    let n4232: ZB = zb_and(n4228, n4231);
    let n4233: ZB = zb_and(n4228, n4230);
    let n4234: ZN = zn_mul(n4227, zn_splat(P8::from_raw(231700i32)));
    let n4235: ZN = zsel_n(n4232, n1847, n1848);
    let n4236: ZN = zsel_n(n4232, n4234, zn_splat(P8::from_raw(0i32)));
    let n4237: ZB = zb_or(n4232, n4233);
    let n4238: ZB = zb_and(n4229, n4231);
    let n4239: ZB = zb_and(n4229, n4230);
    let n4240: ZN = zn_mul(n4227, zn_splat(P8::from_raw(327680i32)));
    let n4241: ZB = zb_and(n2215, n4239);
    let n4242: ZB = zb_and(n2252, n4239);
    let n4243: ZN = zsel_n(n4241, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4244: ZB = zb_or(n4241, n4242);
    let n4245: ZN = zsel_n(n4238, zn_splat(P8::from_raw(0i32)), n4243);
    let n4246: ZN = zsel_n(n4238, n4240, zn_splat(P8::from_raw(0i32)));
    let n4247: ZB = zb_or(n4238, n4244);
    let n4248: ZN = zsel_n(n4237, n4235, n4245);
    let n4249: ZN = zsel_n(n4237, n4236, n4246);
    let n4250: ZB = zb_or(n4237, n4247);
    let n4251: ZB = zn_gt(n4248, zn_splat(P8::from_raw(0i32)));
    let n4252: ZB = zn_le(n4248, zn_splat(P8::from_raw(0i32)));
    let n4253: ZB = zb_and(n4250, n4251);
    let n4254: ZB = zb_and(n4250, n4252);
    let n4255: ZB = zn_lt(n4248, zn_splat(P8::from_raw(0i32)));
    let n4256: ZB = zn_ge(n4248, zn_splat(P8::from_raw(0i32)));
    let n4257: ZB = zb_and(n4254, n4255);
    let n4258: ZB = zb_and(n4254, n4256);
    let n4259: ZB = zb_or(n4253, n4257);
    let n4260: ZB = zb_or(n4258, n4259);
    let n4261: ZB = zn_gt(n4249, zn_splat(P8::from_raw(0i32)));
    let n4262: ZB = zn_le(n4249, zn_splat(P8::from_raw(0i32)));
    let n4263: ZB = zb_and(n4260, n4261);
    let n4264: ZB = zb_and(n4260, n4262);
    let n4265: ZB = zb_or(n4263, n4264);
    let n4266: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4249);
    let n4267: ZB = zb_not(n4266);
    let n4268: ZB = zb_and(n4265, n4267);
    let n4269: ZB = zb_and(n4265, n4266);
    let n4270: ZB = zb_or(n4268, n4269);
    let n4271: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4248);
    let n4272: ZB = zb_not(n4271);
    let n4273: ZB = zb_and(n4270, n4272);
    let n4274: ZB = zb_and(n4270, n4271);
    let n4275: ZB = zb_or(n4273, n4274);
    let n4276: ZN = zsel_n(n4275, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4277: ZB = zb_or(n93, n4275);
    let n4278: ZB = zb_or(n4226, n4275);
    let n4279: ZB = zb_and(n1626, n4222);
    let n4280: ZB = zb_and(n1627, n4222);
    let n4281: ZB = zb_or(n4279, n4280);
    let n4282: ZB = zb_not(n4279);
    let n4283: ZB = zb_and(n4279, n4281);
    let n4284: ZB = zb_and(n4281, n4282);
    let n4285: ZB = zb_and(n1626, n4278);
    let n4286: ZB = zb_and(n1627, n4278);
    let n4287: ZB = zb_or(n4285, n4286);
    let n4288: ZB = zb_not(n4285);
    let n4289: ZB = zb_and(n4285, n4287);
    let n4290: ZB = zb_and(n4287, n4288);
    let n4291: ZN = zsel_n(n4283, n92, zn_splat(P8::from_raw(983040i32)));
    let n4292: ZN = zsel_n(n4283, n4211, n4276);
    let n4293: ZB = zb_not(n4283);
    let n4294: ZB = zb_or(n95, n4293);
    let n4295: ZB = zb_or(n4283, n4289);
    let n4296: ZB = zsel_b(n4283, n1463, n1472);
    let n4297: ZN = zsel_n(n734, r_c20, n4211);
    let n4298: ZB = zsel_b(n734, n93, n4212);
    let n4299: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4213);
    let n4300: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4214);
    let n4301: ZN = zsel_n(n734, r_c88, n4215);
    let n4302: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4216);
    let n4303: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4217);
    let n4304: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4218);
    let n4305: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4219);
    let n4306: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4220);
    let n4307: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4221);
    let n4308: ZB = zb_or(n734, n4284);
    let n4309: ZB = zn_gt(n4276, zn_splat(P8::from_raw(0i32)));
    let n4310: ZB = zn_le(n4276, zn_splat(P8::from_raw(0i32)));
    let n4311: ZB = zb_and(n4290, n4309);
    let n4312: ZB = zb_and(n4290, n4310);
    let n4313: ZB = zn_gt(n4297, zn_splat(P8::from_raw(0i32)));
    let n4314: ZB = zn_le(n4297, zn_splat(P8::from_raw(0i32)));
    let n4315: ZB = zb_and(n4308, n4313);
    let n4316: ZB = zb_and(n4308, n4314);
    let n4317: ZB = zb_and(n1682, n4316);
    let n4318: ZB = zb_and(n1681, n4316);
    let n4319: ZB = zb_or(n4317, n4318);
    let n4320: ZB = zb_not(n4317);
    let n4321: ZB = zb_or(n1686, n4320);
    let n4322: ZB = zb_not(n4321);
    let n4323: ZB = zb_and(n4319, n4321);
    let n4324: ZB = zb_and(n4319, n4322);
    let n4325: ZN = zsel_n(n4323, n1694, n653);
    let n4326: ZN = zsel_n(n4323, zn_splat(P8::from_raw(0i32)), n4306);
    let n4327: ZB = zb_or(n4323, n4324);
    let n4328: ZB = zn_gt(n4292, zn_splat(P8::from_raw(0i32)));
    let n4329: ZB = zn_le(n4292, zn_splat(P8::from_raw(0i32)));
    let n4330: ZB = zb_and(n4295, n4328);
    let n4331: ZB = zb_and(n4295, n4329);
    let n4332: ZB = zb_or(n4311, n4312);
    let n4333: ZN = zsel_n(n4315, n653, n4325);
    let n4334: ZN = zsel_n(n4315, n4306, n4326);
    let n4335: ZB = zb_or(n4315, n4327);
    let n4336: ZB = zb_or(n4330, n4331);
    let n4337: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4299);
    let n4338: ZN = zsel_n(n3806, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4339: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4338);
    let n4340: ZB = zb_not(n4339);
    let n4341: ZB = zb_and(n3809, n4340);
    let n4342: ZB = zb_and(n3809, n4339);
    let n4343: ZN = zn_mul(n4338, zn_splat(P8::from_raw(231700i32)));
    let n4344: ZN = zsel_n(n4341, n1578, n1579);
    let n4345: ZN = zsel_n(n4341, n4343, zn_splat(P8::from_raw(0i32)));
    let n4346: ZB = zb_or(n4341, n4342);
    let n4347: ZB = zb_and(n3810, n4340);
    let n4348: ZB = zb_and(n3810, n4339);
    let n4349: ZN = zn_mul(n4338, zn_splat(P8::from_raw(327680i32)));
    let n4350: ZB = zb_and(n1530, n4348);
    let n4351: ZB = zb_and(n1970, n4348);
    let n4352: ZN = zsel_n(n4350, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4353: ZB = zb_or(n4350, n4351);
    let n4354: ZN = zsel_n(n4347, zn_splat(P8::from_raw(0i32)), n4352);
    let n4355: ZN = zsel_n(n4347, n4349, zn_splat(P8::from_raw(0i32)));
    let n4356: ZB = zb_or(n4347, n4353);
    let n4357: ZN = zsel_n(n4346, n4344, n4354);
    let n4358: ZN = zsel_n(n4346, n4345, n4355);
    let n4359: ZB = zb_or(n4346, n4356);
    let n4360: ZB = zn_gt(n4357, zn_splat(P8::from_raw(0i32)));
    let n4361: ZB = zn_le(n4357, zn_splat(P8::from_raw(0i32)));
    let n4362: ZB = zb_and(n4359, n4360);
    let n4363: ZB = zb_and(n4359, n4361);
    let n4364: ZB = zn_lt(n4357, zn_splat(P8::from_raw(0i32)));
    let n4365: ZB = zn_ge(n4357, zn_splat(P8::from_raw(0i32)));
    let n4366: ZB = zb_and(n4363, n4364);
    let n4367: ZB = zb_and(n4363, n4365);
    let n4368: ZN = zsel_n(n4362, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4369: ZB = zb_or(n4362, n4366);
    let n4370: ZN = zsel_n(n4367, zn_splat(P8::from_raw(0i32)), n4368);
    let n4371: ZB = zb_or(n4367, n4369);
    let n4372: ZB = zn_gt(n4358, zn_splat(P8::from_raw(0i32)));
    let n4373: ZB = zn_le(n4358, zn_splat(P8::from_raw(0i32)));
    let n4374: ZB = zb_and(n4371, n4372);
    let n4375: ZB = zb_and(n4371, n4373);
    let n4376: ZB = zn_lt(n4358, zn_splat(P8::from_raw(0i32)));
    let n4377: ZB = zn_ge(n4358, zn_splat(P8::from_raw(0i32)));
    let n4378: ZB = zb_and(n4375, n4376);
    let n4379: ZB = zb_and(n4375, n4377);
    let n4380: ZN = zsel_n(n4374, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4381: ZB = zb_or(n4374, n4378);
    let n4382: ZN = zsel_n(n4379, zn_splat(P8::from_raw(0i32)), n4380);
    let n4383: ZB = zb_or(n4379, n4381);
    let n4384: ZB = zb_and(n4376, n4383);
    let n4385: ZB = zb_and(n4377, n4383);
    let n4386: ZN = zn_mul(n4382, zn_splat(P8::from_raw(49152i32)));
    let n4387: ZN = zsel_n(n4384, n4386, n4382);
    let n4388: ZB = zb_or(n4384, n4385);
    let n4389: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4358);
    let n4390: ZB = zb_not(n4389);
    let n4391: ZB = zb_and(n4388, n4390);
    let n4392: ZB = zb_and(n4388, n4389);
    let n4393: ZN = zsel_n(n4391, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4394: ZB = zb_or(n4391, n4392);
    let n4395: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4357);
    let n4396: ZB = zb_not(n4395);
    let n4397: ZB = zb_and(n4394, n4396);
    let n4398: ZB = zb_and(n4394, n4395);
    let n4399: ZN = zsel_n(n4397, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4400: ZB = zb_or(n4397, n4398);
    let n4401: ZN = zsel_n(n4400, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4402: ZB = zb_or(n93, n4400);
    let n4403: ZN = zsel_n(n4400, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n4404: ZN = zsel_n(n4400, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n4405: ZN = zsel_n(n4400, n1577, r_c88);
    let n4406: ZN = zsel_n(n4400, n4393, zn_splat(P8::from_raw(0i32)));
    let n4407: ZN = zsel_n(n4400, n4399, zn_splat(P8::from_raw(0i32)));
    let n4408: ZN = zsel_n(n4400, n4370, zn_splat(P8::from_raw(0i32)));
    let n4409: ZN = zsel_n(n4400, n4387, zn_splat(P8::from_raw(0i32)));
    let n4410: ZN = zsel_n(n4400, n4357, n1965);
    let n4411: ZN = zsel_n(n4400, n4358, n1966);
    let n4412: ZB = zb_or(n3807, n4400);
    let n4413: ZN = zsel_n(n3877, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4414: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4413);
    let n4415: ZB = zb_not(n4414);
    let n4416: ZB = zb_and(n3880, n4415);
    let n4417: ZB = zb_and(n3880, n4414);
    let n4418: ZN = zn_mul(n4413, zn_splat(P8::from_raw(231700i32)));
    let n4419: ZN = zsel_n(n4416, n1578, n1579);
    let n4420: ZN = zsel_n(n4416, n4418, zn_splat(P8::from_raw(0i32)));
    let n4421: ZB = zb_or(n4416, n4417);
    let n4422: ZB = zb_and(n3881, n4415);
    let n4423: ZB = zb_and(n3881, n4414);
    let n4424: ZN = zn_mul(n4413, zn_splat(P8::from_raw(327680i32)));
    let n4425: ZB = zb_and(n1972, n4423);
    let n4426: ZB = zb_and(n2011, n4423);
    let n4427: ZN = zsel_n(n4425, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4428: ZB = zb_or(n4425, n4426);
    let n4429: ZN = zsel_n(n4422, zn_splat(P8::from_raw(0i32)), n4427);
    let n4430: ZN = zsel_n(n4422, n4424, zn_splat(P8::from_raw(0i32)));
    let n4431: ZB = zb_or(n4422, n4428);
    let n4432: ZN = zsel_n(n4421, n4419, n4429);
    let n4433: ZN = zsel_n(n4421, n4420, n4430);
    let n4434: ZB = zb_or(n4421, n4431);
    let n4435: ZB = zn_gt(n4432, zn_splat(P8::from_raw(0i32)));
    let n4436: ZB = zn_le(n4432, zn_splat(P8::from_raw(0i32)));
    let n4437: ZB = zb_and(n4434, n4435);
    let n4438: ZB = zb_and(n4434, n4436);
    let n4439: ZB = zn_lt(n4432, zn_splat(P8::from_raw(0i32)));
    let n4440: ZB = zn_ge(n4432, zn_splat(P8::from_raw(0i32)));
    let n4441: ZB = zb_and(n4438, n4439);
    let n4442: ZB = zb_and(n4438, n4440);
    let n4443: ZB = zb_or(n4437, n4441);
    let n4444: ZB = zb_or(n4442, n4443);
    let n4445: ZB = zn_gt(n4433, zn_splat(P8::from_raw(0i32)));
    let n4446: ZB = zn_le(n4433, zn_splat(P8::from_raw(0i32)));
    let n4447: ZB = zb_and(n4444, n4445);
    let n4448: ZB = zb_and(n4444, n4446);
    let n4449: ZB = zn_lt(n4433, zn_splat(P8::from_raw(0i32)));
    let n4450: ZB = zn_ge(n4433, zn_splat(P8::from_raw(0i32)));
    let n4451: ZB = zb_and(n4448, n4449);
    let n4452: ZB = zb_and(n4448, n4450);
    let n4453: ZB = zb_or(n4447, n4451);
    let n4454: ZB = zb_or(n4452, n4453);
    let n4455: ZB = zb_and(n4449, n4454);
    let n4456: ZB = zb_and(n4450, n4454);
    let n4457: ZB = zb_or(n4455, n4456);
    let n4458: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4433);
    let n4459: ZB = zb_not(n4458);
    let n4460: ZB = zb_and(n4457, n4459);
    let n4461: ZB = zb_and(n4457, n4458);
    let n4462: ZB = zb_or(n4460, n4461);
    let n4463: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4432);
    let n4464: ZB = zb_not(n4463);
    let n4465: ZB = zb_and(n4462, n4464);
    let n4466: ZB = zb_and(n4462, n4463);
    let n4467: ZB = zb_or(n4465, n4466);
    let n4468: ZN = zsel_n(n4467, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4469: ZB = zb_or(n93, n4467);
    let n4470: ZB = zb_or(n3878, n4467);
    let n4471: ZB = zb_and(n1626, n4412);
    let n4472: ZB = zb_and(n1627, n4412);
    let n4473: ZB = zb_or(n4471, n4472);
    let n4474: ZB = zb_not(n4471);
    let n4475: ZB = zb_and(n4471, n4473);
    let n4476: ZB = zb_and(n4473, n4474);
    let n4477: ZB = zb_and(n1626, n4470);
    let n4478: ZB = zb_and(n1627, n4470);
    let n4479: ZB = zb_or(n4477, n4478);
    let n4480: ZB = zb_not(n4477);
    let n4481: ZB = zb_and(n4477, n4479);
    let n4482: ZB = zb_and(n4479, n4480);
    let n4483: ZN = zsel_n(n4475, n92, zn_splat(P8::from_raw(983040i32)));
    let n4484: ZN = zsel_n(n4475, n4401, n4468);
    let n4485: ZB = zb_not(n4475);
    let n4486: ZB = zb_or(n95, n4485);
    let n4487: ZB = zb_or(n4475, n4481);
    let n4488: ZB = zsel_b(n4475, n1463, n1472);
    let n4489: ZN = zsel_n(n734, r_c20, n4401);
    let n4490: ZB = zsel_b(n734, n93, n4402);
    let n4491: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4403);
    let n4492: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4404);
    let n4493: ZN = zsel_n(n734, r_c88, n4405);
    let n4494: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4406);
    let n4495: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4407);
    let n4496: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4408);
    let n4497: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4409);
    let n4498: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4410);
    let n4499: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4411);
    let n4500: ZB = zb_or(n734, n4476);
    let n4501: ZB = zn_gt(n4468, zn_splat(P8::from_raw(0i32)));
    let n4502: ZB = zn_le(n4468, zn_splat(P8::from_raw(0i32)));
    let n4503: ZB = zb_and(n4482, n4501);
    let n4504: ZB = zb_and(n4482, n4502);
    let n4505: ZB = zn_gt(n4489, zn_splat(P8::from_raw(0i32)));
    let n4506: ZB = zn_le(n4489, zn_splat(P8::from_raw(0i32)));
    let n4507: ZB = zb_and(n4500, n4505);
    let n4508: ZB = zb_and(n4500, n4506);
    let n4509: ZB = zb_and(n1682, n4508);
    let n4510: ZB = zb_and(n1681, n4508);
    let n4511: ZB = zb_or(n4509, n4510);
    let n4512: ZB = zb_not(n4509);
    let n4513: ZB = zb_or(n1686, n4512);
    let n4514: ZB = zb_not(n4513);
    let n4515: ZB = zb_and(n4511, n4513);
    let n4516: ZB = zb_and(n4511, n4514);
    let n4517: ZN = zsel_n(n4515, n1694, n653);
    let n4518: ZN = zsel_n(n4515, zn_splat(P8::from_raw(0i32)), n4498);
    let n4519: ZB = zb_or(n4515, n4516);
    let n4520: ZB = zn_gt(n4484, zn_splat(P8::from_raw(0i32)));
    let n4521: ZB = zn_le(n4484, zn_splat(P8::from_raw(0i32)));
    let n4522: ZB = zb_and(n4487, n4520);
    let n4523: ZB = zb_and(n4487, n4521);
    let n4524: ZB = zb_or(n4503, n4504);
    let n4525: ZN = zsel_n(n4507, n653, n4517);
    let n4526: ZN = zsel_n(n4507, n4498, n4518);
    let n4527: ZB = zb_or(n4507, n4519);
    let n4528: ZB = zb_or(n4522, n4523);
    let n4529: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4491);
    let n4530: ZN = zsel_n(n3992, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4531: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4530);
    let n4532: ZB = zb_not(n4531);
    let n4533: ZB = zb_and(n3992, n4532);
    let n4534: ZB = zb_and(n3992, n4531);
    let n4535: ZN = zn_mul(n4530, zn_splat(P8::from_raw(231700i32)));
    let n4536: ZN = zsel_n(n4533, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n4537: ZN = zsel_n(n4533, n4535, zn_splat(P8::from_raw(0i32)));
    let n4538: ZB = zb_or(n4533, n4534);
    let n4540: ZN = zsel_n(n4538, n4536, zn_splat(P8::from_raw(65536i32)));
    let n4541: ZN = zsel_n(n4538, n4537, zn_splat(P8::from_raw(0i32)));
    let n4542: ZB = zn_gt(n4540, zn_splat(P8::from_raw(0i32)));
    let n4543: ZB = zn_le(n4540, zn_splat(P8::from_raw(0i32)));
    let n4544: ZB = zb_and(n4538, n4542);
    let n4545: ZB = zb_and(n4538, n4543);
    let n4546: ZB = zn_lt(n4540, zn_splat(P8::from_raw(0i32)));
    let n4547: ZB = zn_ge(n4540, zn_splat(P8::from_raw(0i32)));
    let n4548: ZB = zb_and(n4545, n4546);
    let n4549: ZB = zb_and(n4545, n4547);
    let n4550: ZN = zsel_n(n4544, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4551: ZB = zb_or(n4544, n4548);
    let n4552: ZN = zsel_n(n4549, zn_splat(P8::from_raw(0i32)), n4550);
    let n4553: ZB = zb_or(n4549, n4551);
    let n4554: ZB = zn_gt(n4541, zn_splat(P8::from_raw(0i32)));
    let n4555: ZB = zn_le(n4541, zn_splat(P8::from_raw(0i32)));
    let n4556: ZB = zb_and(n4553, n4554);
    let n4557: ZB = zb_and(n4553, n4555);
    let n4558: ZB = zn_lt(n4541, zn_splat(P8::from_raw(0i32)));
    let n4559: ZB = zn_ge(n4541, zn_splat(P8::from_raw(0i32)));
    let n4560: ZB = zb_and(n4557, n4558);
    let n4561: ZB = zb_and(n4557, n4559);
    let n4562: ZN = zsel_n(n4556, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4563: ZB = zb_or(n4556, n4560);
    let n4564: ZN = zsel_n(n4561, zn_splat(P8::from_raw(0i32)), n4562);
    let n4565: ZB = zb_or(n4561, n4563);
    let n4566: ZB = zb_and(n4558, n4565);
    let n4567: ZB = zb_and(n4559, n4565);
    let n4568: ZN = zn_mul(n4564, zn_splat(P8::from_raw(49152i32)));
    let n4569: ZN = zsel_n(n4566, n4568, n4564);
    let n4570: ZB = zb_or(n4566, n4567);
    let n4571: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4541);
    let n4572: ZB = zb_not(n4571);
    let n4573: ZB = zb_and(n4570, n4572);
    let n4574: ZB = zb_and(n4570, n4571);
    let n4575: ZN = zsel_n(n4573, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4576: ZB = zb_or(n4573, n4574);
    let n4577: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4540);
    let n4578: ZB = zb_not(n4577);
    let n4579: ZB = zb_and(n4576, n4578);
    let n4580: ZB = zb_and(n4576, n4577);
    let n4581: ZN = zsel_n(n4579, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4582: ZB = zb_or(n4579, n4580);
    let n4583: ZN = zsel_n(n4582, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4584: ZB = zb_or(n93, n4582);
    let n4585: ZN = zsel_n(n4582, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n4586: ZN = zsel_n(n4582, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n4587: ZN = zsel_n(n4582, n1577, r_c88);
    let n4588: ZN = zsel_n(n4582, n4575, zn_splat(P8::from_raw(0i32)));
    let n4589: ZN = zsel_n(n4582, n4581, zn_splat(P8::from_raw(0i32)));
    let n4590: ZN = zsel_n(n4582, n4552, zn_splat(P8::from_raw(0i32)));
    let n4591: ZN = zsel_n(n4582, n4569, zn_splat(P8::from_raw(0i32)));
    let n4592: ZN = zsel_n(n4582, n4540, n2089);
    let n4593: ZN = zsel_n(n4582, n4541, n2090);
    let n4594: ZB = zb_or(n3993, n4582);
    let n4595: ZN = zsel_n(n4051, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4596: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4595);
    let n4597: ZB = zb_not(n4596);
    let n4598: ZB = zb_and(n4051, n4597);
    let n4599: ZB = zb_and(n4051, n4596);
    let n4600: ZN = zn_mul(n4595, zn_splat(P8::from_raw(231700i32)));
    let n4601: ZN = zsel_n(n4598, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n4602: ZN = zsel_n(n4598, n4600, zn_splat(P8::from_raw(0i32)));
    let n4603: ZB = zb_or(n4598, n4599);
    let n4605: ZN = zsel_n(n4603, n4601, zn_splat(P8::from_raw(65536i32)));
    let n4606: ZN = zsel_n(n4603, n4602, zn_splat(P8::from_raw(0i32)));
    let n4607: ZB = zn_gt(n4605, zn_splat(P8::from_raw(0i32)));
    let n4608: ZB = zn_le(n4605, zn_splat(P8::from_raw(0i32)));
    let n4609: ZB = zb_and(n4603, n4607);
    let n4610: ZB = zb_and(n4603, n4608);
    let n4611: ZB = zn_lt(n4605, zn_splat(P8::from_raw(0i32)));
    let n4612: ZB = zn_ge(n4605, zn_splat(P8::from_raw(0i32)));
    let n4613: ZB = zb_and(n4610, n4611);
    let n4614: ZB = zb_and(n4610, n4612);
    let n4615: ZB = zb_or(n4609, n4613);
    let n4616: ZB = zb_or(n4614, n4615);
    let n4617: ZB = zn_gt(n4606, zn_splat(P8::from_raw(0i32)));
    let n4618: ZB = zn_le(n4606, zn_splat(P8::from_raw(0i32)));
    let n4619: ZB = zb_and(n4616, n4617);
    let n4620: ZB = zb_and(n4616, n4618);
    let n4621: ZB = zn_lt(n4606, zn_splat(P8::from_raw(0i32)));
    let n4622: ZB = zn_ge(n4606, zn_splat(P8::from_raw(0i32)));
    let n4623: ZB = zb_and(n4620, n4621);
    let n4624: ZB = zb_and(n4620, n4622);
    let n4625: ZB = zb_or(n4619, n4623);
    let n4626: ZB = zb_or(n4624, n4625);
    let n4627: ZB = zb_and(n4621, n4626);
    let n4628: ZB = zb_and(n4622, n4626);
    let n4629: ZB = zb_or(n4627, n4628);
    let n4630: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4606);
    let n4631: ZB = zb_not(n4630);
    let n4632: ZB = zb_and(n4629, n4631);
    let n4633: ZB = zb_and(n4629, n4630);
    let n4634: ZB = zb_or(n4632, n4633);
    let n4635: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4605);
    let n4636: ZB = zb_not(n4635);
    let n4637: ZB = zb_and(n4634, n4636);
    let n4638: ZB = zb_and(n4634, n4635);
    let n4639: ZB = zb_or(n4637, n4638);
    let n4640: ZN = zsel_n(n4639, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4641: ZB = zb_or(n93, n4639);
    let n4642: ZB = zb_or(n4052, n4639);
    let n4643: ZB = zb_and(n1626, n4594);
    let n4644: ZB = zb_and(n1627, n4594);
    let n4645: ZB = zb_or(n4643, n4644);
    let n4646: ZB = zb_not(n4643);
    let n4647: ZB = zb_and(n4643, n4645);
    let n4648: ZB = zb_and(n4645, n4646);
    let n4649: ZB = zb_and(n1626, n4642);
    let n4650: ZB = zb_and(n1627, n4642);
    let n4651: ZB = zb_or(n4649, n4650);
    let n4652: ZB = zb_not(n4649);
    let n4653: ZB = zb_and(n4649, n4651);
    let n4654: ZB = zb_and(n4651, n4652);
    let n4655: ZN = zsel_n(n4647, n92, zn_splat(P8::from_raw(983040i32)));
    let n4656: ZN = zsel_n(n4647, n4583, n4640);
    let n4657: ZB = zb_not(n4647);
    let n4658: ZB = zb_or(n95, n4657);
    let n4659: ZB = zb_or(n4647, n4653);
    let n4660: ZB = zsel_b(n4647, n1463, n1472);
    let n4661: ZN = zsel_n(n734, r_c20, n4583);
    let n4662: ZB = zsel_b(n734, n93, n4584);
    let n4663: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4585);
    let n4664: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4586);
    let n4665: ZN = zsel_n(n734, r_c88, n4587);
    let n4666: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4588);
    let n4667: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4589);
    let n4668: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4590);
    let n4669: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4591);
    let n4670: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4592);
    let n4671: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4593);
    let n4672: ZB = zb_or(n734, n4648);
    let n4673: ZB = zn_gt(n4640, zn_splat(P8::from_raw(0i32)));
    let n4674: ZB = zn_le(n4640, zn_splat(P8::from_raw(0i32)));
    let n4675: ZB = zb_and(n4654, n4673);
    let n4676: ZB = zb_and(n4654, n4674);
    let n4677: ZB = zn_gt(n4661, zn_splat(P8::from_raw(0i32)));
    let n4678: ZB = zn_le(n4661, zn_splat(P8::from_raw(0i32)));
    let n4679: ZB = zb_and(n4672, n4677);
    let n4680: ZB = zb_and(n4672, n4678);
    let n4681: ZB = zb_and(n1682, n4680);
    let n4682: ZB = zb_and(n1681, n4680);
    let n4683: ZB = zb_or(n4681, n4682);
    let n4684: ZB = zb_not(n4681);
    let n4685: ZB = zb_or(n1686, n4684);
    let n4686: ZB = zb_not(n4685);
    let n4687: ZB = zb_and(n4683, n4685);
    let n4688: ZB = zb_and(n4683, n4686);
    let n4689: ZN = zsel_n(n4687, n1694, n653);
    let n4690: ZN = zsel_n(n4687, zn_splat(P8::from_raw(0i32)), n4670);
    let n4691: ZB = zb_or(n4687, n4688);
    let n4692: ZB = zn_gt(n4656, zn_splat(P8::from_raw(0i32)));
    let n4693: ZB = zn_le(n4656, zn_splat(P8::from_raw(0i32)));
    let n4694: ZB = zb_and(n4659, n4692);
    let n4695: ZB = zb_and(n4659, n4693);
    let n4696: ZB = zb_or(n4675, n4676);
    let n4697: ZN = zsel_n(n4679, n653, n4689);
    let n4698: ZN = zsel_n(n4679, n4670, n4690);
    let n4699: ZB = zb_or(n4679, n4691);
    let n4700: ZB = zb_or(n4694, n4695);
    let n4701: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4663);
    let n4702: ZN = zsel_n(n4154, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4703: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4702);
    let n4704: ZB = zb_not(n4703);
    let n4705: ZB = zb_and(n4157, n4704);
    let n4706: ZB = zb_and(n4157, n4703);
    let n4707: ZN = zn_mul(n4702, zn_splat(P8::from_raw(231700i32)));
    let n4708: ZN = zsel_n(n4705, n1847, n1848);
    let n4709: ZN = zsel_n(n4705, n4707, zn_splat(P8::from_raw(0i32)));
    let n4710: ZB = zb_or(n4705, n4706);
    let n4711: ZB = zb_and(n4158, n4704);
    let n4712: ZB = zb_and(n4158, n4703);
    let n4713: ZN = zn_mul(n4702, zn_splat(P8::from_raw(327680i32)));
    let n4714: ZB = zb_and(n1813, n4712);
    let n4715: ZB = zb_and(n2213, n4712);
    let n4716: ZN = zsel_n(n4714, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4717: ZB = zb_or(n4714, n4715);
    let n4718: ZN = zsel_n(n4711, zn_splat(P8::from_raw(0i32)), n4716);
    let n4719: ZN = zsel_n(n4711, n4713, zn_splat(P8::from_raw(0i32)));
    let n4720: ZB = zb_or(n4711, n4717);
    let n4721: ZN = zsel_n(n4710, n4708, n4718);
    let n4722: ZN = zsel_n(n4710, n4709, n4719);
    let n4723: ZB = zb_or(n4710, n4720);
    let n4724: ZB = zn_gt(n4721, zn_splat(P8::from_raw(0i32)));
    let n4725: ZB = zn_le(n4721, zn_splat(P8::from_raw(0i32)));
    let n4726: ZB = zb_and(n4723, n4724);
    let n4727: ZB = zb_and(n4723, n4725);
    let n4728: ZB = zn_lt(n4721, zn_splat(P8::from_raw(0i32)));
    let n4729: ZB = zn_ge(n4721, zn_splat(P8::from_raw(0i32)));
    let n4730: ZB = zb_and(n4727, n4728);
    let n4731: ZB = zb_and(n4727, n4729);
    let n4732: ZN = zsel_n(n4726, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4733: ZB = zb_or(n4726, n4730);
    let n4734: ZN = zsel_n(n4731, zn_splat(P8::from_raw(0i32)), n4732);
    let n4735: ZB = zb_or(n4731, n4733);
    let n4736: ZB = zn_gt(n4722, zn_splat(P8::from_raw(0i32)));
    let n4737: ZB = zn_le(n4722, zn_splat(P8::from_raw(0i32)));
    let n4738: ZB = zb_and(n4735, n4736);
    let n4739: ZB = zb_and(n4735, n4737);
    let n4740: ZB = zn_lt(n4722, zn_splat(P8::from_raw(0i32)));
    let n4741: ZB = zn_ge(n4722, zn_splat(P8::from_raw(0i32)));
    let n4742: ZB = zb_and(n4739, n4740);
    let n4743: ZB = zb_and(n4739, n4741);
    let n4744: ZN = zsel_n(n4738, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4745: ZB = zb_or(n4738, n4742);
    let n4746: ZN = zsel_n(n4743, zn_splat(P8::from_raw(0i32)), n4744);
    let n4747: ZB = zb_or(n4743, n4745);
    let n4748: ZB = zb_and(n4740, n4747);
    let n4749: ZB = zb_and(n4741, n4747);
    let n4750: ZN = zn_mul(n4746, zn_splat(P8::from_raw(49152i32)));
    let n4751: ZN = zsel_n(n4748, n4750, n4746);
    let n4752: ZB = zb_or(n4748, n4749);
    let n4753: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4722);
    let n4754: ZB = zb_not(n4753);
    let n4755: ZB = zb_and(n4752, n4754);
    let n4756: ZB = zb_and(n4752, n4753);
    let n4757: ZN = zsel_n(n4755, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4758: ZB = zb_or(n4755, n4756);
    let n4759: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4721);
    let n4760: ZB = zb_not(n4759);
    let n4761: ZB = zb_and(n4758, n4760);
    let n4762: ZB = zb_and(n4758, n4759);
    let n4763: ZN = zsel_n(n4761, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4764: ZB = zb_or(n4761, n4762);
    let n4765: ZN = zsel_n(n4764, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4766: ZB = zb_or(n93, n4764);
    let n4767: ZN = zsel_n(n4764, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n4768: ZN = zsel_n(n4764, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n4769: ZN = zsel_n(n4764, n1577, r_c88);
    let n4770: ZN = zsel_n(n4764, n4757, zn_splat(P8::from_raw(0i32)));
    let n4771: ZN = zsel_n(n4764, n4763, zn_splat(P8::from_raw(0i32)));
    let n4772: ZN = zsel_n(n4764, n4734, zn_splat(P8::from_raw(0i32)));
    let n4773: ZN = zsel_n(n4764, n4751, zn_splat(P8::from_raw(0i32)));
    let n4774: ZN = zsel_n(n4764, n4721, n2208);
    let n4775: ZN = zsel_n(n4764, n4722, n2209);
    let n4776: ZB = zb_or(n4155, n4764);
    let n4777: ZN = zsel_n(n4225, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4778: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4777);
    let n4779: ZB = zb_not(n4778);
    let n4780: ZB = zb_and(n4228, n4779);
    let n4781: ZB = zb_and(n4228, n4778);
    let n4782: ZN = zn_mul(n4777, zn_splat(P8::from_raw(231700i32)));
    let n4783: ZN = zsel_n(n4780, n1847, n1848);
    let n4784: ZN = zsel_n(n4780, n4782, zn_splat(P8::from_raw(0i32)));
    let n4785: ZB = zb_or(n4780, n4781);
    let n4786: ZB = zb_and(n4229, n4779);
    let n4787: ZB = zb_and(n4229, n4778);
    let n4788: ZN = zn_mul(n4777, zn_splat(P8::from_raw(327680i32)));
    let n4789: ZB = zb_and(n2215, n4787);
    let n4790: ZB = zb_and(n2252, n4787);
    let n4791: ZN = zsel_n(n4789, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n4792: ZB = zb_or(n4789, n4790);
    let n4793: ZN = zsel_n(n4786, zn_splat(P8::from_raw(0i32)), n4791);
    let n4794: ZN = zsel_n(n4786, n4788, zn_splat(P8::from_raw(0i32)));
    let n4795: ZB = zb_or(n4786, n4792);
    let n4796: ZN = zsel_n(n4785, n4783, n4793);
    let n4797: ZN = zsel_n(n4785, n4784, n4794);
    let n4798: ZB = zb_or(n4785, n4795);
    let n4799: ZB = zn_gt(n4796, zn_splat(P8::from_raw(0i32)));
    let n4800: ZB = zn_le(n4796, zn_splat(P8::from_raw(0i32)));
    let n4801: ZB = zb_and(n4798, n4799);
    let n4802: ZB = zb_and(n4798, n4800);
    let n4803: ZB = zn_lt(n4796, zn_splat(P8::from_raw(0i32)));
    let n4804: ZB = zn_ge(n4796, zn_splat(P8::from_raw(0i32)));
    let n4805: ZB = zb_and(n4802, n4803);
    let n4806: ZB = zb_and(n4802, n4804);
    let n4807: ZB = zb_or(n4801, n4805);
    let n4808: ZB = zb_or(n4806, n4807);
    let n4809: ZB = zn_gt(n4797, zn_splat(P8::from_raw(0i32)));
    let n4810: ZB = zn_le(n4797, zn_splat(P8::from_raw(0i32)));
    let n4811: ZB = zb_and(n4808, n4809);
    let n4812: ZB = zb_and(n4808, n4810);
    let n4813: ZB = zn_lt(n4797, zn_splat(P8::from_raw(0i32)));
    let n4814: ZB = zn_ge(n4797, zn_splat(P8::from_raw(0i32)));
    let n4815: ZB = zb_and(n4812, n4813);
    let n4816: ZB = zb_and(n4812, n4814);
    let n4817: ZB = zb_or(n4811, n4815);
    let n4818: ZB = zb_or(n4816, n4817);
    let n4819: ZB = zb_and(n4813, n4818);
    let n4820: ZB = zb_and(n4814, n4818);
    let n4821: ZB = zb_or(n4819, n4820);
    let n4822: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4797);
    let n4823: ZB = zb_not(n4822);
    let n4824: ZB = zb_and(n4821, n4823);
    let n4825: ZB = zb_and(n4821, n4822);
    let n4826: ZB = zb_or(n4824, n4825);
    let n4827: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4796);
    let n4828: ZB = zb_not(n4827);
    let n4829: ZB = zb_and(n4826, n4828);
    let n4830: ZB = zb_and(n4826, n4827);
    let n4831: ZB = zb_or(n4829, n4830);
    let n4832: ZN = zsel_n(n4831, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4833: ZB = zb_or(n93, n4831);
    let n4834: ZB = zb_or(n4226, n4831);
    let n4835: ZB = zb_and(n1626, n4776);
    let n4836: ZB = zb_and(n1627, n4776);
    let n4837: ZB = zb_or(n4835, n4836);
    let n4838: ZB = zb_not(n4835);
    let n4839: ZB = zb_and(n4835, n4837);
    let n4840: ZB = zb_and(n4837, n4838);
    let n4841: ZB = zb_and(n1626, n4834);
    let n4842: ZB = zb_and(n1627, n4834);
    let n4843: ZB = zb_or(n4841, n4842);
    let n4844: ZB = zb_not(n4841);
    let n4845: ZB = zb_and(n4841, n4843);
    let n4846: ZB = zb_and(n4843, n4844);
    let n4847: ZN = zsel_n(n4839, n92, zn_splat(P8::from_raw(983040i32)));
    let n4848: ZN = zsel_n(n4839, n4765, n4832);
    let n4849: ZB = zb_not(n4839);
    let n4850: ZB = zb_or(n95, n4849);
    let n4851: ZB = zb_or(n4839, n4845);
    let n4852: ZB = zsel_b(n4839, n1463, n1472);
    let n4853: ZN = zsel_n(n734, r_c20, n4765);
    let n4854: ZB = zsel_b(n734, n93, n4766);
    let n4855: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4767);
    let n4856: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4768);
    let n4857: ZN = zsel_n(n734, r_c88, n4769);
    let n4858: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4770);
    let n4859: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4771);
    let n4860: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4772);
    let n4861: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4773);
    let n4862: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4774);
    let n4863: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4775);
    let n4864: ZB = zb_or(n734, n4840);
    let n4865: ZB = zn_gt(n4832, zn_splat(P8::from_raw(0i32)));
    let n4866: ZB = zn_le(n4832, zn_splat(P8::from_raw(0i32)));
    let n4867: ZB = zb_and(n4846, n4865);
    let n4868: ZB = zb_and(n4846, n4866);
    let n4869: ZB = zn_gt(n4853, zn_splat(P8::from_raw(0i32)));
    let n4870: ZB = zn_le(n4853, zn_splat(P8::from_raw(0i32)));
    let n4871: ZB = zb_and(n4864, n4869);
    let n4872: ZB = zb_and(n4864, n4870);
    let n4873: ZB = zb_and(n1682, n4872);
    let n4874: ZB = zb_and(n1681, n4872);
    let n4875: ZB = zb_or(n4873, n4874);
    let n4876: ZB = zb_not(n4873);
    let n4877: ZB = zb_or(n1686, n4876);
    let n4878: ZB = zb_not(n4877);
    let n4879: ZB = zb_and(n4875, n4877);
    let n4880: ZB = zb_and(n4875, n4878);
    let n4881: ZN = zsel_n(n4879, n1694, n653);
    let n4882: ZN = zsel_n(n4879, zn_splat(P8::from_raw(0i32)), n4862);
    let n4883: ZB = zb_or(n4879, n4880);
    let n4884: ZB = zn_gt(n4848, zn_splat(P8::from_raw(0i32)));
    let n4885: ZB = zn_le(n4848, zn_splat(P8::from_raw(0i32)));
    let n4886: ZB = zb_and(n4851, n4884);
    let n4887: ZB = zb_and(n4851, n4885);
    let n4888: ZB = zb_or(n4867, n4868);
    let n4889: ZN = zsel_n(n4871, n653, n4881);
    let n4890: ZN = zsel_n(n4871, n4862, n4882);
    let n4891: ZB = zb_or(n4871, n4883);
    let n4892: ZB = zb_or(n4886, n4887);
    let n4893: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4855);
    let n4894: ZN = zsel_n(n3809, n1578, n1579);
    let n4895: ZN = zsel_n(n3809, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n4896: ZN = zsel_n(n3810, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n4897: ZN = zsel_n(n3810, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n4898: ZN = zsel_n(n3809, n4894, n4896);
    let n4899: ZN = zsel_n(n3809, n4895, n4897);
    let n4900: ZB = zb_or(n3809, n3810);
    let n4901: ZB = zn_gt(n4898, zn_splat(P8::from_raw(0i32)));
    let n4902: ZB = zn_le(n4898, zn_splat(P8::from_raw(0i32)));
    let n4903: ZB = zb_and(n4900, n4901);
    let n4904: ZB = zb_and(n4900, n4902);
    let n4905: ZB = zn_lt(n4898, zn_splat(P8::from_raw(0i32)));
    let n4906: ZB = zn_ge(n4898, zn_splat(P8::from_raw(0i32)));
    let n4907: ZB = zb_and(n4904, n4905);
    let n4908: ZB = zb_and(n4904, n4906);
    let n4909: ZN = zsel_n(n4903, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4910: ZB = zb_or(n4903, n4907);
    let n4911: ZN = zsel_n(n4908, zn_splat(P8::from_raw(0i32)), n4909);
    let n4912: ZB = zb_or(n4908, n4910);
    let n4913: ZB = zn_gt(n4899, zn_splat(P8::from_raw(0i32)));
    let n4914: ZB = zn_le(n4899, zn_splat(P8::from_raw(0i32)));
    let n4915: ZB = zb_and(n4912, n4913);
    let n4916: ZB = zb_and(n4912, n4914);
    let n4917: ZN = zsel_n(n4915, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n4918: ZN = zsel_n(n4916, zn_splat(P8::from_raw(0i32)), n4917);
    let n4919: ZB = zb_or(n4915, n4916);
    let n4920: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4899);
    let n4921: ZB = zb_not(n4920);
    let n4922: ZB = zb_and(n4919, n4921);
    let n4923: ZB = zb_and(n4919, n4920);
    let n4924: ZN = zsel_n(n4922, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4925: ZB = zb_or(n4922, n4923);
    let n4926: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4898);
    let n4927: ZB = zb_not(n4926);
    let n4928: ZB = zb_and(n4925, n4927);
    let n4929: ZB = zb_and(n4925, n4926);
    let n4930: ZN = zsel_n(n4928, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n4931: ZB = zb_or(n4928, n4929);
    let n4932: ZN = zsel_n(n4931, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4933: ZB = zb_or(n93, n4931);
    let n4934: ZN = zsel_n(n4931, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n4935: ZN = zsel_n(n4931, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n4936: ZN = zsel_n(n4931, n1577, r_c88);
    let n4937: ZN = zsel_n(n4931, n4924, zn_splat(P8::from_raw(0i32)));
    let n4938: ZN = zsel_n(n4931, n4930, zn_splat(P8::from_raw(0i32)));
    let n4939: ZN = zsel_n(n4931, n4911, zn_splat(P8::from_raw(0i32)));
    let n4940: ZN = zsel_n(n4931, n4918, zn_splat(P8::from_raw(0i32)));
    let n4941: ZN = zsel_n(n4931, n4898, n1965);
    let n4942: ZN = zsel_n(n4931, n4899, n1966);
    let n4943: ZB = zb_or(n3807, n4931);
    let n4944: ZN = zsel_n(n3880, n1578, n1579);
    let n4945: ZN = zsel_n(n3880, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n4946: ZN = zsel_n(n3881, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n4947: ZN = zsel_n(n3881, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n4948: ZN = zsel_n(n3880, n4944, n4946);
    let n4949: ZN = zsel_n(n3880, n4945, n4947);
    let n4950: ZB = zb_or(n3880, n3881);
    let n4951: ZB = zn_gt(n4948, zn_splat(P8::from_raw(0i32)));
    let n4952: ZB = zn_le(n4948, zn_splat(P8::from_raw(0i32)));
    let n4953: ZB = zb_and(n4950, n4951);
    let n4954: ZB = zb_and(n4950, n4952);
    let n4955: ZB = zn_lt(n4948, zn_splat(P8::from_raw(0i32)));
    let n4956: ZB = zn_ge(n4948, zn_splat(P8::from_raw(0i32)));
    let n4957: ZB = zb_and(n4954, n4955);
    let n4958: ZB = zb_and(n4954, n4956);
    let n4959: ZB = zb_or(n4953, n4957);
    let n4960: ZB = zb_or(n4958, n4959);
    let n4961: ZB = zn_gt(n4949, zn_splat(P8::from_raw(0i32)));
    let n4962: ZB = zn_le(n4949, zn_splat(P8::from_raw(0i32)));
    let n4963: ZB = zb_and(n4960, n4961);
    let n4964: ZB = zb_and(n4960, n4962);
    let n4965: ZB = zb_or(n4963, n4964);
    let n4966: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4949);
    let n4967: ZB = zb_not(n4966);
    let n4968: ZB = zb_and(n4965, n4967);
    let n4969: ZB = zb_and(n4965, n4966);
    let n4970: ZB = zb_or(n4968, n4969);
    let n4971: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n4948);
    let n4972: ZB = zb_not(n4971);
    let n4973: ZB = zb_and(n4970, n4972);
    let n4974: ZB = zb_and(n4970, n4971);
    let n4975: ZB = zb_or(n4973, n4974);
    let n4976: ZN = zsel_n(n4975, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n4977: ZB = zb_or(n93, n4975);
    let n4978: ZB = zb_or(n3878, n4975);
    let n4979: ZB = zb_and(n1626, n4943);
    let n4980: ZB = zb_and(n1627, n4943);
    let n4981: ZB = zb_or(n4979, n4980);
    let n4982: ZB = zb_not(n4979);
    let n4983: ZB = zb_and(n4979, n4981);
    let n4984: ZB = zb_and(n4981, n4982);
    let n4985: ZB = zb_and(n1626, n4978);
    let n4986: ZB = zb_and(n1627, n4978);
    let n4987: ZB = zb_or(n4985, n4986);
    let n4988: ZB = zb_not(n4985);
    let n4989: ZB = zb_and(n4985, n4987);
    let n4990: ZB = zb_and(n4987, n4988);
    let n4991: ZN = zsel_n(n4983, n92, zn_splat(P8::from_raw(983040i32)));
    let n4992: ZN = zsel_n(n4983, n4932, n4976);
    let n4993: ZB = zb_not(n4983);
    let n4994: ZB = zb_or(n95, n4993);
    let n4995: ZB = zb_or(n4983, n4989);
    let n4996: ZB = zsel_b(n4983, n1463, n1472);
    let n4997: ZN = zsel_n(n734, r_c20, n4932);
    let n4998: ZB = zsel_b(n734, n93, n4933);
    let n4999: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4934);
    let n5000: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4935);
    let n5001: ZN = zsel_n(n734, r_c88, n4936);
    let n5002: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4937);
    let n5003: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4938);
    let n5004: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4939);
    let n5005: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4940);
    let n5006: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4941);
    let n5007: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n4942);
    let n5008: ZB = zb_or(n734, n4984);
    let n5009: ZB = zn_gt(n4976, zn_splat(P8::from_raw(0i32)));
    let n5010: ZB = zn_le(n4976, zn_splat(P8::from_raw(0i32)));
    let n5011: ZB = zb_and(n4990, n5009);
    let n5012: ZB = zb_and(n4990, n5010);
    let n5013: ZB = zn_gt(n4997, zn_splat(P8::from_raw(0i32)));
    let n5014: ZB = zn_le(n4997, zn_splat(P8::from_raw(0i32)));
    let n5015: ZB = zb_and(n5008, n5013);
    let n5016: ZB = zb_and(n5008, n5014);
    let n5017: ZB = zb_and(n1682, n5016);
    let n5018: ZB = zb_and(n1681, n5016);
    let n5019: ZB = zb_or(n5017, n5018);
    let n5020: ZB = zb_not(n5017);
    let n5021: ZB = zb_or(n1686, n5020);
    let n5022: ZB = zb_not(n5021);
    let n5023: ZB = zb_and(n5019, n5021);
    let n5024: ZB = zb_and(n5019, n5022);
    let n5025: ZN = zsel_n(n5023, n1694, n653);
    let n5026: ZN = zsel_n(n5023, zn_splat(P8::from_raw(0i32)), n5006);
    let n5027: ZB = zb_or(n5023, n5024);
    let n5028: ZB = zn_gt(n4992, zn_splat(P8::from_raw(0i32)));
    let n5029: ZB = zn_le(n4992, zn_splat(P8::from_raw(0i32)));
    let n5030: ZB = zb_and(n4995, n5028);
    let n5031: ZB = zb_and(n4995, n5029);
    let n5032: ZB = zb_or(n5011, n5012);
    let n5033: ZN = zsel_n(n5015, n653, n5025);
    let n5034: ZN = zsel_n(n5015, n5006, n5026);
    let n5035: ZB = zb_or(n5015, n5027);
    let n5036: ZB = zb_or(n5030, n5031);
    let n5037: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4999);
    let n5038: ZN = zsel_n(n3992, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n5039: ZN = zsel_n(n3992, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n5040: ZN = zsel_n(n3992, n5038, zn_splat(P8::from_raw(65536i32)));
    let n5041: ZN = zsel_n(n3992, n5039, zn_splat(P8::from_raw(0i32)));
    let n5042: ZB = zn_gt(n5040, zn_splat(P8::from_raw(0i32)));
    let n5043: ZB = zn_le(n5040, zn_splat(P8::from_raw(0i32)));
    let n5044: ZB = zb_and(n3992, n5042);
    let n5045: ZB = zb_and(n3992, n5043);
    let n5046: ZB = zn_lt(n5040, zn_splat(P8::from_raw(0i32)));
    let n5047: ZB = zn_ge(n5040, zn_splat(P8::from_raw(0i32)));
    let n5048: ZB = zb_and(n5045, n5046);
    let n5049: ZB = zb_and(n5045, n5047);
    let n5050: ZN = zsel_n(n5044, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n5051: ZB = zb_or(n5044, n5048);
    let n5052: ZN = zsel_n(n5049, zn_splat(P8::from_raw(0i32)), n5050);
    let n5053: ZB = zb_or(n5049, n5051);
    let n5054: ZB = zn_gt(n5041, zn_splat(P8::from_raw(0i32)));
    let n5055: ZB = zn_le(n5041, zn_splat(P8::from_raw(0i32)));
    let n5056: ZB = zb_and(n5053, n5054);
    let n5057: ZB = zb_and(n5053, n5055);
    let n5058: ZN = zsel_n(n5056, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n5059: ZN = zsel_n(n5057, zn_splat(P8::from_raw(0i32)), n5058);
    let n5060: ZB = zb_or(n5056, n5057);
    let n5061: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5041);
    let n5062: ZB = zb_not(n5061);
    let n5063: ZB = zb_and(n5060, n5062);
    let n5064: ZB = zb_and(n5060, n5061);
    let n5065: ZN = zsel_n(n5063, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n5066: ZB = zb_or(n5063, n5064);
    let n5067: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5040);
    let n5068: ZB = zb_not(n5067);
    let n5069: ZB = zb_and(n5066, n5068);
    let n5070: ZB = zb_and(n5066, n5067);
    let n5071: ZN = zsel_n(n5069, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n5072: ZB = zb_or(n5069, n5070);
    let n5073: ZN = zsel_n(n5072, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n5074: ZB = zb_or(n93, n5072);
    let n5075: ZN = zsel_n(n5072, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n5076: ZN = zsel_n(n5072, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n5077: ZN = zsel_n(n5072, n1577, r_c88);
    let n5078: ZN = zsel_n(n5072, n5065, zn_splat(P8::from_raw(0i32)));
    let n5079: ZN = zsel_n(n5072, n5071, zn_splat(P8::from_raw(0i32)));
    let n5080: ZN = zsel_n(n5072, n5052, zn_splat(P8::from_raw(0i32)));
    let n5081: ZN = zsel_n(n5072, n5059, zn_splat(P8::from_raw(0i32)));
    let n5082: ZN = zsel_n(n5072, n5040, n2089);
    let n5083: ZN = zsel_n(n5072, n5041, n2090);
    let n5084: ZB = zb_or(n3993, n5072);
    let n5085: ZN = zsel_n(n4051, zn_splat(P8::from_raw(-231700i32)), zn_splat(P8::from_raw(-327680i32)));
    let n5086: ZN = zsel_n(n4051, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n5087: ZN = zsel_n(n4051, n5085, zn_splat(P8::from_raw(65536i32)));
    let n5088: ZN = zsel_n(n4051, n5086, zn_splat(P8::from_raw(0i32)));
    let n5089: ZB = zn_gt(n5087, zn_splat(P8::from_raw(0i32)));
    let n5090: ZB = zn_le(n5087, zn_splat(P8::from_raw(0i32)));
    let n5091: ZB = zb_and(n4051, n5089);
    let n5092: ZB = zb_and(n4051, n5090);
    let n5093: ZB = zn_lt(n5087, zn_splat(P8::from_raw(0i32)));
    let n5094: ZB = zn_ge(n5087, zn_splat(P8::from_raw(0i32)));
    let n5095: ZB = zb_and(n5092, n5093);
    let n5096: ZB = zb_and(n5092, n5094);
    let n5097: ZB = zb_or(n5091, n5095);
    let n5098: ZB = zb_or(n5096, n5097);
    let n5099: ZB = zn_gt(n5088, zn_splat(P8::from_raw(0i32)));
    let n5100: ZB = zn_le(n5088, zn_splat(P8::from_raw(0i32)));
    let n5101: ZB = zb_and(n5098, n5099);
    let n5102: ZB = zb_and(n5098, n5100);
    let n5103: ZB = zb_or(n5101, n5102);
    let n5104: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5088);
    let n5105: ZB = zb_not(n5104);
    let n5106: ZB = zb_and(n5103, n5105);
    let n5107: ZB = zb_and(n5103, n5104);
    let n5108: ZB = zb_or(n5106, n5107);
    let n5109: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5087);
    let n5110: ZB = zb_not(n5109);
    let n5111: ZB = zb_and(n5108, n5110);
    let n5112: ZB = zb_and(n5108, n5109);
    let n5113: ZB = zb_or(n5111, n5112);
    let n5114: ZN = zsel_n(n5113, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n5115: ZB = zb_or(n93, n5113);
    let n5116: ZB = zb_or(n4052, n5113);
    let n5117: ZB = zb_and(n1626, n5084);
    let n5118: ZB = zb_and(n1627, n5084);
    let n5119: ZB = zb_or(n5117, n5118);
    let n5120: ZB = zb_not(n5117);
    let n5121: ZB = zb_and(n5117, n5119);
    let n5122: ZB = zb_and(n5119, n5120);
    let n5123: ZB = zb_and(n1626, n5116);
    let n5124: ZB = zb_and(n1627, n5116);
    let n5125: ZB = zb_or(n5123, n5124);
    let n5126: ZB = zb_not(n5123);
    let n5127: ZB = zb_and(n5123, n5125);
    let n5128: ZB = zb_and(n5125, n5126);
    let n5129: ZN = zsel_n(n5121, n92, zn_splat(P8::from_raw(983040i32)));
    let n5130: ZN = zsel_n(n5121, n5073, n5114);
    let n5131: ZB = zb_not(n5121);
    let n5132: ZB = zb_or(n95, n5131);
    let n5133: ZB = zb_or(n5121, n5127);
    let n5134: ZB = zsel_b(n5121, n1463, n1472);
    let n5135: ZN = zsel_n(n734, r_c20, n5073);
    let n5136: ZB = zsel_b(n734, n93, n5074);
    let n5137: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5075);
    let n5138: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5076);
    let n5139: ZN = zsel_n(n734, r_c88, n5077);
    let n5140: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5078);
    let n5141: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5079);
    let n5142: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5080);
    let n5143: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5081);
    let n5144: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5082);
    let n5145: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5083);
    let n5146: ZB = zb_or(n734, n5122);
    let n5147: ZB = zn_gt(n5114, zn_splat(P8::from_raw(0i32)));
    let n5148: ZB = zn_le(n5114, zn_splat(P8::from_raw(0i32)));
    let n5149: ZB = zb_and(n5128, n5147);
    let n5150: ZB = zb_and(n5128, n5148);
    let n5151: ZB = zn_gt(n5135, zn_splat(P8::from_raw(0i32)));
    let n5152: ZB = zn_le(n5135, zn_splat(P8::from_raw(0i32)));
    let n5153: ZB = zb_and(n5146, n5151);
    let n5154: ZB = zb_and(n5146, n5152);
    let n5155: ZB = zb_and(n1682, n5154);
    let n5156: ZB = zb_and(n1681, n5154);
    let n5157: ZB = zb_or(n5155, n5156);
    let n5158: ZB = zb_not(n5155);
    let n5159: ZB = zb_or(n1686, n5158);
    let n5160: ZB = zb_not(n5159);
    let n5161: ZB = zb_and(n5157, n5159);
    let n5162: ZB = zb_and(n5157, n5160);
    let n5163: ZN = zsel_n(n5161, n1694, n653);
    let n5164: ZN = zsel_n(n5161, zn_splat(P8::from_raw(0i32)), n5144);
    let n5165: ZB = zb_or(n5161, n5162);
    let n5166: ZB = zn_gt(n5130, zn_splat(P8::from_raw(0i32)));
    let n5167: ZB = zn_le(n5130, zn_splat(P8::from_raw(0i32)));
    let n5168: ZB = zb_and(n5133, n5166);
    let n5169: ZB = zb_and(n5133, n5167);
    let n5170: ZB = zb_or(n5149, n5150);
    let n5171: ZN = zsel_n(n5153, n653, n5163);
    let n5172: ZN = zsel_n(n5153, n5144, n5164);
    let n5173: ZB = zb_or(n5153, n5165);
    let n5174: ZB = zb_or(n5168, n5169);
    let n5175: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n5137);
    let n5176: ZN = zsel_n(n4157, n1847, n1848);
    let n5177: ZN = zsel_n(n4157, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n5178: ZN = zsel_n(n4158, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5179: ZN = zsel_n(n4158, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n5180: ZN = zsel_n(n4157, n5176, n5178);
    let n5181: ZN = zsel_n(n4157, n5177, n5179);
    let n5182: ZB = zb_or(n4157, n4158);
    let n5183: ZB = zn_gt(n5180, zn_splat(P8::from_raw(0i32)));
    let n5184: ZB = zn_le(n5180, zn_splat(P8::from_raw(0i32)));
    let n5185: ZB = zb_and(n5182, n5183);
    let n5186: ZB = zb_and(n5182, n5184);
    let n5187: ZB = zn_lt(n5180, zn_splat(P8::from_raw(0i32)));
    let n5188: ZB = zn_ge(n5180, zn_splat(P8::from_raw(0i32)));
    let n5189: ZB = zb_and(n5186, n5187);
    let n5190: ZB = zb_and(n5186, n5188);
    let n5191: ZN = zsel_n(n5185, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n5192: ZB = zb_or(n5185, n5189);
    let n5193: ZN = zsel_n(n5190, zn_splat(P8::from_raw(0i32)), n5191);
    let n5194: ZB = zb_or(n5190, n5192);
    let n5195: ZB = zn_gt(n5181, zn_splat(P8::from_raw(0i32)));
    let n5196: ZB = zn_le(n5181, zn_splat(P8::from_raw(0i32)));
    let n5197: ZB = zb_and(n5194, n5195);
    let n5198: ZB = zb_and(n5194, n5196);
    let n5199: ZN = zsel_n(n5197, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(-131072i32)));
    let n5200: ZN = zsel_n(n5198, zn_splat(P8::from_raw(0i32)), n5199);
    let n5201: ZB = zb_or(n5197, n5198);
    let n5202: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5181);
    let n5203: ZB = zb_not(n5202);
    let n5204: ZB = zb_and(n5201, n5203);
    let n5205: ZB = zb_and(n5201, n5202);
    let n5206: ZN = zsel_n(n5204, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n5207: ZB = zb_or(n5204, n5205);
    let n5208: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5180);
    let n5209: ZB = zb_not(n5208);
    let n5210: ZB = zb_and(n5207, n5209);
    let n5211: ZB = zb_and(n5207, n5208);
    let n5212: ZN = zsel_n(n5210, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n5213: ZB = zb_or(n5210, n5211);
    let n5214: ZN = zsel_n(n5213, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n5215: ZB = zb_or(n93, n5213);
    let n5216: ZN = zsel_n(n5213, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n5217: ZN = zsel_n(n5213, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n5218: ZN = zsel_n(n5213, n1577, r_c88);
    let n5219: ZN = zsel_n(n5213, n5206, zn_splat(P8::from_raw(0i32)));
    let n5220: ZN = zsel_n(n5213, n5212, zn_splat(P8::from_raw(0i32)));
    let n5221: ZN = zsel_n(n5213, n5193, zn_splat(P8::from_raw(0i32)));
    let n5222: ZN = zsel_n(n5213, n5200, zn_splat(P8::from_raw(0i32)));
    let n5223: ZN = zsel_n(n5213, n5180, n2208);
    let n5224: ZN = zsel_n(n5213, n5181, n2209);
    let n5225: ZB = zb_or(n4155, n5213);
    let n5226: ZN = zsel_n(n4228, n1847, n1848);
    let n5227: ZN = zsel_n(n4228, zn_splat(P8::from_raw(231700i32)), zn_splat(P8::from_raw(0i32)));
    let n5228: ZN = zsel_n(n4229, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n5229: ZN = zsel_n(n4229, zn_splat(P8::from_raw(327680i32)), zn_splat(P8::from_raw(0i32)));
    let n5230: ZN = zsel_n(n4228, n5226, n5228);
    let n5231: ZN = zsel_n(n4228, n5227, n5229);
    let n5232: ZB = zb_or(n4228, n4229);
    let n5233: ZB = zn_gt(n5230, zn_splat(P8::from_raw(0i32)));
    let n5234: ZB = zn_le(n5230, zn_splat(P8::from_raw(0i32)));
    let n5235: ZB = zb_and(n5232, n5233);
    let n5236: ZB = zb_and(n5232, n5234);
    let n5237: ZB = zn_lt(n5230, zn_splat(P8::from_raw(0i32)));
    let n5238: ZB = zn_ge(n5230, zn_splat(P8::from_raw(0i32)));
    let n5239: ZB = zb_and(n5236, n5237);
    let n5240: ZB = zb_and(n5236, n5238);
    let n5241: ZB = zb_or(n5235, n5239);
    let n5242: ZB = zb_or(n5240, n5241);
    let n5243: ZB = zn_gt(n5231, zn_splat(P8::from_raw(0i32)));
    let n5244: ZB = zn_le(n5231, zn_splat(P8::from_raw(0i32)));
    let n5245: ZB = zb_and(n5242, n5243);
    let n5246: ZB = zb_and(n5242, n5244);
    let n5247: ZB = zb_or(n5245, n5246);
    let n5248: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5231);
    let n5249: ZB = zb_not(n5248);
    let n5250: ZB = zb_and(n5247, n5249);
    let n5251: ZB = zb_and(n5247, n5248);
    let n5252: ZB = zb_or(n5250, n5251);
    let n5253: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5230);
    let n5254: ZB = zb_not(n5253);
    let n5255: ZB = zb_and(n5252, n5254);
    let n5256: ZB = zb_and(n5252, n5253);
    let n5257: ZB = zb_or(n5255, n5256);
    let n5258: ZN = zsel_n(n5257, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n5259: ZB = zb_or(n93, n5257);
    let n5260: ZB = zb_or(n4226, n5257);
    let n5261: ZB = zb_and(n1626, n5225);
    let n5262: ZB = zb_and(n1627, n5225);
    let n5263: ZB = zb_or(n5261, n5262);
    let n5264: ZB = zb_not(n5261);
    let n5265: ZB = zb_and(n5261, n5263);
    let n5266: ZB = zb_and(n5263, n5264);
    let n5267: ZB = zb_and(n1626, n5260);
    let n5268: ZB = zb_and(n1627, n5260);
    let n5269: ZB = zb_or(n5267, n5268);
    let n5270: ZB = zb_not(n5267);
    let n5271: ZB = zb_and(n5267, n5269);
    let n5272: ZB = zb_and(n5269, n5270);
    let n5273: ZN = zsel_n(n5265, n92, zn_splat(P8::from_raw(983040i32)));
    let n5274: ZN = zsel_n(n5265, n5214, n5258);
    let n5275: ZB = zb_not(n5265);
    let n5276: ZB = zb_or(n95, n5275);
    let n5277: ZB = zb_or(n5265, n5271);
    let n5278: ZB = zsel_b(n5265, n1463, n1472);
    let n5279: ZN = zsel_n(n734, r_c20, n5214);
    let n5280: ZB = zsel_b(n734, n93, n5215);
    let n5281: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5216);
    let n5282: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5217);
    let n5283: ZN = zsel_n(n734, r_c88, n5218);
    let n5284: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5219);
    let n5285: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5220);
    let n5286: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5221);
    let n5287: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5222);
    let n5288: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5223);
    let n5289: ZN = zsel_n(n734, zn_splat(P8::from_raw(0i32)), n5224);
    let n5290: ZB = zb_or(n734, n5266);
    let n5291: ZB = zn_gt(n5258, zn_splat(P8::from_raw(0i32)));
    let n5292: ZB = zn_le(n5258, zn_splat(P8::from_raw(0i32)));
    let n5293: ZB = zb_and(n5272, n5291);
    let n5294: ZB = zb_and(n5272, n5292);
    let n5295: ZB = zn_gt(n5279, zn_splat(P8::from_raw(0i32)));
    let n5296: ZB = zn_le(n5279, zn_splat(P8::from_raw(0i32)));
    let n5297: ZB = zb_and(n5290, n5295);
    let n5298: ZB = zb_and(n5290, n5296);
    let n5299: ZB = zb_and(n1682, n5298);
    let n5300: ZB = zb_and(n1681, n5298);
    let n5301: ZB = zb_or(n5299, n5300);
    let n5302: ZB = zb_not(n5299);
    let n5303: ZB = zb_or(n1686, n5302);
    let n5304: ZB = zb_not(n5303);
    let n5305: ZB = zb_and(n5301, n5303);
    let n5306: ZB = zb_and(n5301, n5304);
    let n5307: ZN = zsel_n(n5305, n1694, n653);
    let n5308: ZN = zsel_n(n5305, zn_splat(P8::from_raw(0i32)), n5288);
    let n5309: ZB = zb_or(n5305, n5306);
    let n5310: ZB = zn_gt(n5274, zn_splat(P8::from_raw(0i32)));
    let n5311: ZB = zn_le(n5274, zn_splat(P8::from_raw(0i32)));
    let n5312: ZB = zb_and(n5277, n5310);
    let n5313: ZB = zb_and(n5277, n5311);
    let n5314: ZB = zb_or(n5293, n5294);
    let n5315: ZN = zsel_n(n5297, n653, n5307);
    let n5316: ZN = zsel_n(n5297, n5288, n5308);
    let n5317: ZB = zb_or(n5297, n5309);
    let n5318: ZB = zb_or(n5312, n5313);
    let n5319: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n5281);
    let n5322: ZW = zw_bits_b(n94);
    let n5323: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5322, 42u64);
    let n5324: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5322, 42u64);
    let n5325: ZW = zw_bits_b(r_c43);
    let n5326: ZW = zw_mix1(n5323, n5325, 43u64);
    let n5327: ZW = zw_mix2(n5324, n5325, 43u64);
    let n5328: ZW = zw_bits_n(r_c88);
    let n5329: ZW = zw_mix1(n5326, n5328, 88u64);
    let n5330: ZW = zw_mix2(n5327, n5328, 88u64);
    let n5331: ZW = zw_bits_n(r_c20);
    let n5332: ZW = zw_mix1(n5329, n5331, 20u64);
    let n5333: ZW = zw_mix2(n5330, n5331, 20u64);
    let n5334: ZW = zw_bits_b(n93);
    let n5335: ZW = zw_mix1(n5332, n5334, 41u64);
    let n5336: ZW = zw_mix2(n5333, n5334, 41u64);
    let n5337: ZW = zw_bits_n(n2412);
    let n5338: ZW = zw_mix1(n5329, n5337, 20u64);
    let n5339: ZW = zw_mix2(n5330, n5337, 20u64);
    let n5340: ZW = zw_bits_b(n2413);
    let n5341: ZW = zw_mix1(n5338, n5340, 41u64);
    let n5342: ZW = zw_mix2(n5339, n5340, 41u64);
    let n5343: ZW = zw_bits_n(n2574);
    let n5344: ZW = zw_mix1(n5329, n5343, 20u64);
    let n5345: ZW = zw_mix2(n5330, n5343, 20u64);
    let n5346: ZW = zw_bits_b(n2575);
    let n5347: ZW = zw_mix1(n5344, n5346, 41u64);
    let n5348: ZW = zw_mix2(n5345, n5346, 41u64);
    let n5349: ZW = zw_bits_n(n2760);
    let n5350: ZW = zw_mix1(n5329, n5349, 20u64);
    let n5351: ZW = zw_mix2(n5330, n5349, 20u64);
    let n5352: ZW = zw_bits_b(n2761);
    let n5353: ZW = zw_mix1(n5350, n5352, 41u64);
    let n5354: ZW = zw_mix2(n5351, n5352, 41u64);
    let n5355: ZW = zw_bits_n(n2952);
    let n5356: ZW = zw_mix1(n5329, n5355, 20u64);
    let n5357: ZW = zw_mix2(n5330, n5355, 20u64);
    let n5358: ZW = zw_bits_b(n2953);
    let n5359: ZW = zw_mix1(n5356, n5358, 41u64);
    let n5360: ZW = zw_mix2(n5357, n5358, 41u64);
    let n5361: ZW = zw_bits_n(n3124);
    let n5362: ZW = zw_mix1(n5329, n5361, 20u64);
    let n5363: ZW = zw_mix2(n5330, n5361, 20u64);
    let n5364: ZW = zw_bits_b(n3125);
    let n5365: ZW = zw_mix1(n5362, n5364, 41u64);
    let n5366: ZW = zw_mix2(n5363, n5364, 41u64);
    let n5367: ZW = zw_bits_n(n3316);
    let n5368: ZW = zw_mix1(n5329, n5367, 20u64);
    let n5369: ZW = zw_mix2(n5330, n5367, 20u64);
    let n5370: ZW = zw_bits_b(n3317);
    let n5371: ZW = zw_mix1(n5368, n5370, 41u64);
    let n5372: ZW = zw_mix2(n5369, n5370, 41u64);
    let n5373: ZW = zw_bits_n(n3460);
    let n5374: ZW = zw_mix1(n5329, n5373, 20u64);
    let n5375: ZW = zw_mix2(n5330, n5373, 20u64);
    let n5376: ZW = zw_bits_b(n3461);
    let n5377: ZW = zw_mix1(n5374, n5376, 41u64);
    let n5378: ZW = zw_mix2(n5375, n5376, 41u64);
    let n5379: ZW = zw_bits_n(n3598);
    let n5380: ZW = zw_mix1(n5329, n5379, 20u64);
    let n5381: ZW = zw_mix2(n5330, n5379, 20u64);
    let n5382: ZW = zw_bits_b(n3599);
    let n5383: ZW = zw_mix1(n5380, n5382, 41u64);
    let n5384: ZW = zw_mix2(n5381, n5382, 41u64);
    let n5385: ZW = zw_bits_n(n3742);
    let n5386: ZW = zw_mix1(n5329, n5385, 20u64);
    let n5387: ZW = zw_mix2(n5330, n5385, 20u64);
    let n5388: ZW = zw_bits_b(n3743);
    let n5389: ZW = zw_mix1(n5386, n5388, 41u64);
    let n5390: ZW = zw_mix2(n5387, n5388, 41u64);
    let n5391: ZW = zw_bits_n(n3928);
    let n5392: ZW = zw_mix1(n5329, n5391, 20u64);
    let n5393: ZW = zw_mix2(n5330, n5391, 20u64);
    let n5394: ZW = zw_bits_b(n3929);
    let n5395: ZW = zw_mix1(n5392, n5394, 41u64);
    let n5396: ZW = zw_mix2(n5393, n5394, 41u64);
    let n5397: ZW = zw_bits_n(n4090);
    let n5398: ZW = zw_mix1(n5329, n5397, 20u64);
    let n5399: ZW = zw_mix2(n5330, n5397, 20u64);
    let n5400: ZW = zw_bits_b(n4091);
    let n5401: ZW = zw_mix1(n5398, n5400, 41u64);
    let n5402: ZW = zw_mix2(n5399, n5400, 41u64);
    let n5403: ZW = zw_bits_n(n4276);
    let n5404: ZW = zw_mix1(n5329, n5403, 20u64);
    let n5405: ZW = zw_mix2(n5330, n5403, 20u64);
    let n5406: ZW = zw_bits_b(n4277);
    let n5407: ZW = zw_mix1(n5404, n5406, 41u64);
    let n5408: ZW = zw_mix2(n5405, n5406, 41u64);
    let n5409: ZW = zw_bits_n(n4468);
    let n5410: ZW = zw_mix1(n5329, n5409, 20u64);
    let n5411: ZW = zw_mix2(n5330, n5409, 20u64);
    let n5412: ZW = zw_bits_b(n4469);
    let n5413: ZW = zw_mix1(n5410, n5412, 41u64);
    let n5414: ZW = zw_mix2(n5411, n5412, 41u64);
    let n5415: ZW = zw_bits_n(n4640);
    let n5416: ZW = zw_mix1(n5329, n5415, 20u64);
    let n5417: ZW = zw_mix2(n5330, n5415, 20u64);
    let n5418: ZW = zw_bits_b(n4641);
    let n5419: ZW = zw_mix1(n5416, n5418, 41u64);
    let n5420: ZW = zw_mix2(n5417, n5418, 41u64);
    let n5421: ZW = zw_bits_n(n4832);
    let n5422: ZW = zw_mix1(n5329, n5421, 20u64);
    let n5423: ZW = zw_mix2(n5330, n5421, 20u64);
    let n5424: ZW = zw_bits_b(n4833);
    let n5425: ZW = zw_mix1(n5422, n5424, 41u64);
    let n5426: ZW = zw_mix2(n5423, n5424, 41u64);
    let n5427: ZW = zw_bits_n(n4976);
    let n5428: ZW = zw_mix1(n5329, n5427, 20u64);
    let n5429: ZW = zw_mix2(n5330, n5427, 20u64);
    let n5430: ZW = zw_bits_b(n4977);
    let n5431: ZW = zw_mix1(n5428, n5430, 41u64);
    let n5432: ZW = zw_mix2(n5429, n5430, 41u64);
    let n5433: ZW = zw_bits_n(n5114);
    let n5434: ZW = zw_mix1(n5329, n5433, 20u64);
    let n5435: ZW = zw_mix2(n5330, n5433, 20u64);
    let n5436: ZW = zw_bits_b(n5115);
    let n5437: ZW = zw_mix1(n5434, n5436, 41u64);
    let n5438: ZW = zw_mix2(n5435, n5436, 41u64);
    let n5439: ZW = zw_bits_n(n5258);
    let n5440: ZW = zw_mix1(n5329, n5439, 20u64);
    let n5441: ZW = zw_mix2(n5330, n5439, 20u64);
    let n5442: ZW = zw_bits_b(n5259);
    let n5443: ZW = zw_mix1(n5440, n5442, 41u64);
    let n5444: ZW = zw_mix2(n5441, n5442, 41u64);
    let n5445: ZW = zw_bits_b(n95);
    let n5446: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5445, 38u64);
    let n5447: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5445, 38u64);
    let n5448: ZW = zw_bits_n(n92);
    let n5449: ZW = zw_mix1(n5446, n5448, 39u64);
    let n5450: ZW = zw_mix2(n5447, n5448, 39u64);
    let n5451: ZW = zw_mix1(n5449, n5322, 42u64);
    let n5452: ZW = zw_mix2(n5450, n5322, 42u64);
    let n5453: ZW = zw_mix1(n5451, n5325, 43u64);
    let n5454: ZW = zw_mix2(n5452, n5325, 43u64);
    let n5455: ZW = zw_mix1(n5453, n5328, 88u64);
    let n5456: ZW = zw_mix2(n5454, n5328, 88u64);
    let n5457: ZW = zw_bits_n(n654);
    let n5458: ZW = zw_mix1(n5455, n5457, 254u64);
    let n5459: ZW = zw_mix2(n5456, n5457, 254u64);
    let n5460: ZW = zw_mix1(n5458, n5331, 20u64);
    let n5461: ZW = zw_mix2(n5459, n5331, 20u64);
    let n5462: ZW = zw_mix1(n5460, n5334, 41u64);
    let n5463: ZW = zw_mix2(n5461, n5334, 41u64);
    let n5464: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n5465: ZW = zw_mix1(n5462, zw_splat(n5464), 234u64);
    let n5466: ZW = zw_mix2(n5463, zw_splat(n5464), 234u64);
    let n5467: ZW = zw_mix1(n5465, zw_splat(n5464), 236u64);
    let n5468: ZW = zw_mix2(n5466, zw_splat(n5464), 236u64);
    let n5469: ZW = zw_mix1(n5467, n5328, 237u64);
    let n5470: ZW = zw_mix2(n5468, n5328, 237u64);
    let n5471: ZW = zw_bits_n(n1645);
    let n5472: ZW = zw_mix1(n5469, n5471, 239u64);
    let n5473: ZW = zw_mix2(n5470, n5471, 239u64);
    let n5474: u64 = false as u64;
    let n5475: ZW = zw_mix1(n5472, zw_splat(n5474), 246u64);
    let n5476: ZW = zw_mix2(n5473, zw_splat(n5474), 246u64);
    let n5477: ZW = zw_mix1(n5475, zw_splat(n5474), 247u64);
    let n5478: ZW = zw_mix2(n5476, zw_splat(n5474), 247u64);
    let n5479: ZW = zw_bits_n(n1695);
    let n5480: ZW = zw_mix1(n5477, n5479, 253u64);
    let n5481: ZW = zw_mix2(n5478, n5479, 253u64);
    let n5482: ZW = zw_mix1(n5480, zw_splat(n5464), 268u64);
    let n5483: ZW = zw_mix2(n5481, zw_splat(n5464), 268u64);
    let n5484: ZW = zw_mix1(n5482, zw_splat(n5464), 269u64);
    let n5485: ZW = zw_mix2(n5483, zw_splat(n5464), 269u64);
    let n5486: ZW = zw_mix1(n5484, zw_splat(n5464), 270u64);
    let n5487: ZW = zw_mix2(n5485, zw_splat(n5464), 270u64);
    let n5488: ZW = zw_mix1(n5486, zw_splat(n5464), 271u64);
    let n5489: ZW = zw_mix2(n5487, zw_splat(n5464), 271u64);
    let n5490: ZW = zw_bits_b(n1647);
    let n5491: ZW = zw_mix1(n5488, n5490, 272u64);
    let n5492: ZW = zw_mix2(n5489, n5490, 272u64);
    let n5493: ZW = zw_bits_n(n1696);
    let n5494: ZW = zw_mix1(n5491, n5493, 280u64);
    let n5495: ZW = zw_mix2(n5492, n5493, 280u64);
    let n5496: ZW = zw_bits_n(n1649);
    let n5497: ZW = zw_mix1(n5494, n5496, 281u64);
    let n5498: ZW = zw_mix2(n5495, n5496, 281u64);
    let n5499: ZW = zw_bits_n(n1794);
    let n5500: ZW = zw_mix1(n5477, n5499, 253u64);
    let n5501: ZW = zw_mix2(n5478, n5499, 253u64);
    let n5502: ZW = zw_mix1(n5500, zw_splat(n5464), 268u64);
    let n5503: ZW = zw_mix2(n5501, zw_splat(n5464), 268u64);
    let n5504: ZW = zw_mix1(n5502, zw_splat(n5464), 269u64);
    let n5505: ZW = zw_mix2(n5503, zw_splat(n5464), 269u64);
    let n5506: ZW = zw_mix1(n5504, zw_splat(n5464), 270u64);
    let n5507: ZW = zw_mix2(n5505, zw_splat(n5464), 270u64);
    let n5508: ZW = zw_mix1(n5506, zw_splat(n5464), 271u64);
    let n5509: ZW = zw_mix2(n5507, zw_splat(n5464), 271u64);
    let n5510: ZW = zw_bits_b(n1780);
    let n5511: ZW = zw_mix1(n5508, n5510, 272u64);
    let n5512: ZW = zw_mix2(n5509, n5510, 272u64);
    let n5513: ZW = zw_bits_n(n1795);
    let n5514: ZW = zw_mix1(n5511, n5513, 280u64);
    let n5515: ZW = zw_mix2(n5512, n5513, 280u64);
    let n5516: ZW = zw_bits_n(n1782);
    let n5517: ZW = zw_mix1(n5514, n5516, 281u64);
    let n5518: ZW = zw_mix2(n5515, n5516, 281u64);
    let n5519: ZW = zw_bits_n(n1917);
    let n5520: ZW = zw_mix1(n5477, n5519, 253u64);
    let n5521: ZW = zw_mix2(n5478, n5519, 253u64);
    let n5522: ZW = zw_mix1(n5520, zw_splat(n5464), 268u64);
    let n5523: ZW = zw_mix2(n5521, zw_splat(n5464), 268u64);
    let n5524: ZW = zw_mix1(n5522, zw_splat(n5464), 269u64);
    let n5525: ZW = zw_mix2(n5523, zw_splat(n5464), 269u64);
    let n5526: ZW = zw_mix1(n5524, zw_splat(n5464), 270u64);
    let n5527: ZW = zw_mix2(n5525, zw_splat(n5464), 270u64);
    let n5528: ZW = zw_mix1(n5526, zw_splat(n5464), 271u64);
    let n5529: ZW = zw_mix2(n5527, zw_splat(n5464), 271u64);
    let n5530: ZW = zw_bits_b(n1903);
    let n5531: ZW = zw_mix1(n5528, n5530, 272u64);
    let n5532: ZW = zw_mix2(n5529, n5530, 272u64);
    let n5533: ZW = zw_bits_n(n1918);
    let n5534: ZW = zw_mix1(n5531, n5533, 280u64);
    let n5535: ZW = zw_mix2(n5532, n5533, 280u64);
    let n5536: ZW = zw_bits_n(n1905);
    let n5537: ZW = zw_mix1(n5534, n5536, 281u64);
    let n5538: ZW = zw_mix2(n5535, n5536, 281u64);
    let n5539: ZW = zw_bits_n(n2029);
    let n5540: ZW = zw_mix1(n5469, n5539, 239u64);
    let n5541: ZW = zw_mix2(n5470, n5539, 239u64);
    let n5542: ZW = zw_mix1(n5540, zw_splat(n5474), 246u64);
    let n5543: ZW = zw_mix2(n5541, zw_splat(n5474), 246u64);
    let n5544: ZW = zw_bits_b(n2030);
    let n5545: ZW = zw_mix1(n5542, n5544, 247u64);
    let n5546: ZW = zw_mix2(n5543, n5544, 247u64);
    let n5547: ZW = zw_bits_n(n2044);
    let n5548: ZW = zw_mix1(n5545, n5547, 253u64);
    let n5549: ZW = zw_mix2(n5546, n5547, 253u64);
    let n5550: ZW = zw_mix1(n5548, zw_splat(n5464), 268u64);
    let n5551: ZW = zw_mix2(n5549, zw_splat(n5464), 268u64);
    let n5552: ZW = zw_mix1(n5550, zw_splat(n5464), 269u64);
    let n5553: ZW = zw_mix2(n5551, zw_splat(n5464), 269u64);
    let n5554: ZW = zw_mix1(n5552, zw_splat(n5464), 270u64);
    let n5555: ZW = zw_mix2(n5553, zw_splat(n5464), 270u64);
    let n5556: ZW = zw_mix1(n5554, zw_splat(n5464), 271u64);
    let n5557: ZW = zw_mix2(n5555, zw_splat(n5464), 271u64);
    let n5558: ZW = zw_mix1(n5556, n5490, 272u64);
    let n5559: ZW = zw_mix2(n5557, n5490, 272u64);
    let n5560: ZW = zw_bits_n(n2045);
    let n5561: ZW = zw_mix1(n5558, n5560, 280u64);
    let n5562: ZW = zw_mix2(n5559, n5560, 280u64);
    let n5563: ZW = zw_bits_n(n2032);
    let n5564: ZW = zw_mix1(n5561, n5563, 281u64);
    let n5565: ZW = zw_mix2(n5562, n5563, 281u64);
    let n5566: ZW = zw_bits_n(n2149);
    let n5567: ZW = zw_mix1(n5469, n5566, 239u64);
    let n5568: ZW = zw_mix2(n5470, n5566, 239u64);
    let n5569: ZW = zw_mix1(n5567, zw_splat(n5474), 246u64);
    let n5570: ZW = zw_mix2(n5568, zw_splat(n5474), 246u64);
    let n5571: ZW = zw_mix1(n5569, n5544, 247u64);
    let n5572: ZW = zw_mix2(n5570, n5544, 247u64);
    let n5573: ZW = zw_bits_n(n2163);
    let n5574: ZW = zw_mix1(n5571, n5573, 253u64);
    let n5575: ZW = zw_mix2(n5572, n5573, 253u64);
    let n5576: ZW = zw_mix1(n5574, zw_splat(n5464), 268u64);
    let n5577: ZW = zw_mix2(n5575, zw_splat(n5464), 268u64);
    let n5578: ZW = zw_mix1(n5576, zw_splat(n5464), 269u64);
    let n5579: ZW = zw_mix2(n5577, zw_splat(n5464), 269u64);
    let n5580: ZW = zw_mix1(n5578, zw_splat(n5464), 270u64);
    let n5581: ZW = zw_mix2(n5579, zw_splat(n5464), 270u64);
    let n5582: ZW = zw_mix1(n5580, zw_splat(n5464), 271u64);
    let n5583: ZW = zw_mix2(n5581, zw_splat(n5464), 271u64);
    let n5584: ZW = zw_mix1(n5582, n5510, 272u64);
    let n5585: ZW = zw_mix2(n5583, n5510, 272u64);
    let n5586: ZW = zw_bits_n(n2164);
    let n5587: ZW = zw_mix1(n5584, n5586, 280u64);
    let n5588: ZW = zw_mix2(n5585, n5586, 280u64);
    let n5589: ZW = zw_bits_n(n2151);
    let n5590: ZW = zw_mix1(n5587, n5589, 281u64);
    let n5591: ZW = zw_mix2(n5588, n5589, 281u64);
    let n5592: ZW = zw_bits_n(n2270);
    let n5593: ZW = zw_mix1(n5469, n5592, 239u64);
    let n5594: ZW = zw_mix2(n5470, n5592, 239u64);
    let n5595: ZW = zw_mix1(n5593, zw_splat(n5474), 246u64);
    let n5596: ZW = zw_mix2(n5594, zw_splat(n5474), 246u64);
    let n5597: ZW = zw_mix1(n5595, n5544, 247u64);
    let n5598: ZW = zw_mix2(n5596, n5544, 247u64);
    let n5599: ZW = zw_bits_n(n2284);
    let n5600: ZW = zw_mix1(n5597, n5599, 253u64);
    let n5601: ZW = zw_mix2(n5598, n5599, 253u64);
    let n5602: ZW = zw_mix1(n5600, zw_splat(n5464), 268u64);
    let n5603: ZW = zw_mix2(n5601, zw_splat(n5464), 268u64);
    let n5604: ZW = zw_mix1(n5602, zw_splat(n5464), 269u64);
    let n5605: ZW = zw_mix2(n5603, zw_splat(n5464), 269u64);
    let n5606: ZW = zw_mix1(n5604, zw_splat(n5464), 270u64);
    let n5607: ZW = zw_mix2(n5605, zw_splat(n5464), 270u64);
    let n5608: ZW = zw_mix1(n5606, zw_splat(n5464), 271u64);
    let n5609: ZW = zw_mix2(n5607, zw_splat(n5464), 271u64);
    let n5610: ZW = zw_mix1(n5608, n5530, 272u64);
    let n5611: ZW = zw_mix2(n5609, n5530, 272u64);
    let n5612: ZW = zw_bits_n(n2285);
    let n5613: ZW = zw_mix1(n5610, n5612, 280u64);
    let n5614: ZW = zw_mix2(n5611, n5612, 280u64);
    let n5615: ZW = zw_bits_n(n2272);
    let n5616: ZW = zw_mix1(n5613, n5615, 281u64);
    let n5617: ZW = zw_mix2(n5614, n5615, 281u64);
    let n5618: ZW = zw_bits_n(n2433);
    let n5619: ZW = zw_mix1(n5458, n5618, 20u64);
    let n5620: ZW = zw_mix2(n5459, n5618, 20u64);
    let n5621: ZW = zw_bits_b(n2434);
    let n5622: ZW = zw_mix1(n5619, n5621, 41u64);
    let n5623: ZW = zw_mix2(n5620, n5621, 41u64);
    let n5624: ZW = zw_bits_n(n2473);
    let n5625: ZW = zw_mix1(n5622, n5624, 234u64);
    let n5626: ZW = zw_mix2(n5623, n5624, 234u64);
    let n5627: ZW = zw_bits_n(n2436);
    let n5628: ZW = zw_mix1(n5625, n5627, 236u64);
    let n5629: ZW = zw_mix2(n5626, n5627, 236u64);
    let n5630: ZW = zw_bits_n(n2437);
    let n5631: ZW = zw_mix1(n5628, n5630, 237u64);
    let n5632: ZW = zw_mix2(n5629, n5630, 237u64);
    let n5633: ZW = zw_mix1(n5631, n5471, 239u64);
    let n5634: ZW = zw_mix2(n5632, n5471, 239u64);
    let n5635: ZW = zw_mix1(n5633, n5544, 246u64);
    let n5636: ZW = zw_mix2(n5634, n5544, 246u64);
    let n5637: ZW = zw_mix1(n5635, zw_splat(n5474), 247u64);
    let n5638: ZW = zw_mix2(n5636, zw_splat(n5474), 247u64);
    let n5639: ZW = zw_bits_n(n2469);
    let n5640: ZW = zw_mix1(n5637, n5639, 253u64);
    let n5641: ZW = zw_mix2(n5638, n5639, 253u64);
    let n5642: ZW = zw_bits_n(n2438);
    let n5643: ZW = zw_mix1(n5640, n5642, 268u64);
    let n5644: ZW = zw_mix2(n5641, n5642, 268u64);
    let n5645: ZW = zw_bits_n(n2439);
    let n5646: ZW = zw_mix1(n5643, n5645, 269u64);
    let n5647: ZW = zw_mix2(n5644, n5645, 269u64);
    let n5648: ZW = zw_bits_n(n2440);
    let n5649: ZW = zw_mix1(n5646, n5648, 270u64);
    let n5650: ZW = zw_mix2(n5647, n5648, 270u64);
    let n5651: ZW = zw_bits_n(n2441);
    let n5652: ZW = zw_mix1(n5649, n5651, 271u64);
    let n5653: ZW = zw_mix2(n5650, n5651, 271u64);
    let n5654: ZW = zw_mix1(n5652, n5490, 272u64);
    let n5655: ZW = zw_mix2(n5653, n5490, 272u64);
    let n5656: ZW = zw_bits_n(n2470);
    let n5657: ZW = zw_mix1(n5654, n5656, 280u64);
    let n5658: ZW = zw_mix2(n5655, n5656, 280u64);
    let n5659: ZW = zw_bits_n(n2443);
    let n5660: ZW = zw_mix1(n5657, n5659, 281u64);
    let n5661: ZW = zw_mix2(n5658, n5659, 281u64);
    let n5662: ZW = zw_bits_n(n2595);
    let n5663: ZW = zw_mix1(n5458, n5662, 20u64);
    let n5664: ZW = zw_mix2(n5459, n5662, 20u64);
    let n5665: ZW = zw_bits_b(n2596);
    let n5666: ZW = zw_mix1(n5663, n5665, 41u64);
    let n5667: ZW = zw_mix2(n5664, n5665, 41u64);
    let n5668: ZW = zw_bits_n(n2635);
    let n5669: ZW = zw_mix1(n5666, n5668, 234u64);
    let n5670: ZW = zw_mix2(n5667, n5668, 234u64);
    let n5671: ZW = zw_bits_n(n2598);
    let n5672: ZW = zw_mix1(n5669, n5671, 236u64);
    let n5673: ZW = zw_mix2(n5670, n5671, 236u64);
    let n5674: ZW = zw_bits_n(n2599);
    let n5675: ZW = zw_mix1(n5672, n5674, 237u64);
    let n5676: ZW = zw_mix2(n5673, n5674, 237u64);
    let n5677: ZW = zw_mix1(n5675, n5471, 239u64);
    let n5678: ZW = zw_mix2(n5676, n5471, 239u64);
    let n5679: ZW = zw_mix1(n5677, n5544, 246u64);
    let n5680: ZW = zw_mix2(n5678, n5544, 246u64);
    let n5681: ZW = zw_mix1(n5679, zw_splat(n5474), 247u64);
    let n5682: ZW = zw_mix2(n5680, zw_splat(n5474), 247u64);
    let n5683: ZW = zw_bits_n(n2631);
    let n5684: ZW = zw_mix1(n5681, n5683, 253u64);
    let n5685: ZW = zw_mix2(n5682, n5683, 253u64);
    let n5686: ZW = zw_bits_n(n2600);
    let n5687: ZW = zw_mix1(n5684, n5686, 268u64);
    let n5688: ZW = zw_mix2(n5685, n5686, 268u64);
    let n5689: ZW = zw_bits_n(n2601);
    let n5690: ZW = zw_mix1(n5687, n5689, 269u64);
    let n5691: ZW = zw_mix2(n5688, n5689, 269u64);
    let n5692: ZW = zw_bits_n(n2602);
    let n5693: ZW = zw_mix1(n5690, n5692, 270u64);
    let n5694: ZW = zw_mix2(n5691, n5692, 270u64);
    let n5695: ZW = zw_bits_n(n2603);
    let n5696: ZW = zw_mix1(n5693, n5695, 271u64);
    let n5697: ZW = zw_mix2(n5694, n5695, 271u64);
    let n5698: ZW = zw_mix1(n5696, n5510, 272u64);
    let n5699: ZW = zw_mix2(n5697, n5510, 272u64);
    let n5700: ZW = zw_bits_n(n2632);
    let n5701: ZW = zw_mix1(n5698, n5700, 280u64);
    let n5702: ZW = zw_mix2(n5699, n5700, 280u64);
    let n5703: ZW = zw_bits_n(n2605);
    let n5704: ZW = zw_mix1(n5701, n5703, 281u64);
    let n5705: ZW = zw_mix2(n5702, n5703, 281u64);
    let n5706: ZW = zw_bits_n(n2781);
    let n5707: ZW = zw_mix1(n5458, n5706, 20u64);
    let n5708: ZW = zw_mix2(n5459, n5706, 20u64);
    let n5709: ZW = zw_bits_b(n2782);
    let n5710: ZW = zw_mix1(n5707, n5709, 41u64);
    let n5711: ZW = zw_mix2(n5708, n5709, 41u64);
    let n5712: ZW = zw_bits_n(n2821);
    let n5713: ZW = zw_mix1(n5710, n5712, 234u64);
    let n5714: ZW = zw_mix2(n5711, n5712, 234u64);
    let n5715: ZW = zw_bits_n(n2784);
    let n5716: ZW = zw_mix1(n5713, n5715, 236u64);
    let n5717: ZW = zw_mix2(n5714, n5715, 236u64);
    let n5718: ZW = zw_bits_n(n2785);
    let n5719: ZW = zw_mix1(n5716, n5718, 237u64);
    let n5720: ZW = zw_mix2(n5717, n5718, 237u64);
    let n5721: ZW = zw_mix1(n5719, n5471, 239u64);
    let n5722: ZW = zw_mix2(n5720, n5471, 239u64);
    let n5723: ZW = zw_mix1(n5721, n5544, 246u64);
    let n5724: ZW = zw_mix2(n5722, n5544, 246u64);
    let n5725: ZW = zw_mix1(n5723, zw_splat(n5474), 247u64);
    let n5726: ZW = zw_mix2(n5724, zw_splat(n5474), 247u64);
    let n5727: ZW = zw_bits_n(n2817);
    let n5728: ZW = zw_mix1(n5725, n5727, 253u64);
    let n5729: ZW = zw_mix2(n5726, n5727, 253u64);
    let n5730: ZW = zw_bits_n(n2786);
    let n5731: ZW = zw_mix1(n5728, n5730, 268u64);
    let n5732: ZW = zw_mix2(n5729, n5730, 268u64);
    let n5733: ZW = zw_bits_n(n2787);
    let n5734: ZW = zw_mix1(n5731, n5733, 269u64);
    let n5735: ZW = zw_mix2(n5732, n5733, 269u64);
    let n5736: ZW = zw_bits_n(n2788);
    let n5737: ZW = zw_mix1(n5734, n5736, 270u64);
    let n5738: ZW = zw_mix2(n5735, n5736, 270u64);
    let n5739: ZW = zw_bits_n(n2789);
    let n5740: ZW = zw_mix1(n5737, n5739, 271u64);
    let n5741: ZW = zw_mix2(n5738, n5739, 271u64);
    let n5742: ZW = zw_mix1(n5740, n5530, 272u64);
    let n5743: ZW = zw_mix2(n5741, n5530, 272u64);
    let n5744: ZW = zw_bits_n(n2818);
    let n5745: ZW = zw_mix1(n5742, n5744, 280u64);
    let n5746: ZW = zw_mix2(n5743, n5744, 280u64);
    let n5747: ZW = zw_bits_n(n2791);
    let n5748: ZW = zw_mix1(n5745, n5747, 281u64);
    let n5749: ZW = zw_mix2(n5746, n5747, 281u64);
    let n5750: ZW = zw_bits_n(n2973);
    let n5751: ZW = zw_mix1(n5458, n5750, 20u64);
    let n5752: ZW = zw_mix2(n5459, n5750, 20u64);
    let n5753: ZW = zw_bits_b(n2974);
    let n5754: ZW = zw_mix1(n5751, n5753, 41u64);
    let n5755: ZW = zw_mix2(n5752, n5753, 41u64);
    let n5756: ZW = zw_bits_n(n3013);
    let n5757: ZW = zw_mix1(n5754, n5756, 234u64);
    let n5758: ZW = zw_mix2(n5755, n5756, 234u64);
    let n5759: ZW = zw_bits_n(n2976);
    let n5760: ZW = zw_mix1(n5757, n5759, 236u64);
    let n5761: ZW = zw_mix2(n5758, n5759, 236u64);
    let n5762: ZW = zw_bits_n(n2977);
    let n5763: ZW = zw_mix1(n5760, n5762, 237u64);
    let n5764: ZW = zw_mix2(n5761, n5762, 237u64);
    let n5765: ZW = zw_mix1(n5763, n5471, 239u64);
    let n5766: ZW = zw_mix2(n5764, n5471, 239u64);
    let n5767: ZW = zw_mix1(n5765, n5544, 246u64);
    let n5768: ZW = zw_mix2(n5766, n5544, 246u64);
    let n5769: ZW = zw_mix1(n5767, zw_splat(n5474), 247u64);
    let n5770: ZW = zw_mix2(n5768, zw_splat(n5474), 247u64);
    let n5771: ZW = zw_bits_n(n3009);
    let n5772: ZW = zw_mix1(n5769, n5771, 253u64);
    let n5773: ZW = zw_mix2(n5770, n5771, 253u64);
    let n5774: ZW = zw_bits_n(n2978);
    let n5775: ZW = zw_mix1(n5772, n5774, 268u64);
    let n5776: ZW = zw_mix2(n5773, n5774, 268u64);
    let n5777: ZW = zw_bits_n(n2979);
    let n5778: ZW = zw_mix1(n5775, n5777, 269u64);
    let n5779: ZW = zw_mix2(n5776, n5777, 269u64);
    let n5780: ZW = zw_bits_n(n2980);
    let n5781: ZW = zw_mix1(n5778, n5780, 270u64);
    let n5782: ZW = zw_mix2(n5779, n5780, 270u64);
    let n5783: ZW = zw_bits_n(n2981);
    let n5784: ZW = zw_mix1(n5781, n5783, 271u64);
    let n5785: ZW = zw_mix2(n5782, n5783, 271u64);
    let n5786: ZW = zw_mix1(n5784, n5490, 272u64);
    let n5787: ZW = zw_mix2(n5785, n5490, 272u64);
    let n5788: ZW = zw_bits_n(n3010);
    let n5789: ZW = zw_mix1(n5786, n5788, 280u64);
    let n5790: ZW = zw_mix2(n5787, n5788, 280u64);
    let n5791: ZW = zw_bits_n(n2983);
    let n5792: ZW = zw_mix1(n5789, n5791, 281u64);
    let n5793: ZW = zw_mix2(n5790, n5791, 281u64);
    let n5794: ZW = zw_bits_n(n3145);
    let n5795: ZW = zw_mix1(n5458, n5794, 20u64);
    let n5796: ZW = zw_mix2(n5459, n5794, 20u64);
    let n5797: ZW = zw_bits_b(n3146);
    let n5798: ZW = zw_mix1(n5795, n5797, 41u64);
    let n5799: ZW = zw_mix2(n5796, n5797, 41u64);
    let n5800: ZW = zw_bits_n(n3185);
    let n5801: ZW = zw_mix1(n5798, n5800, 234u64);
    let n5802: ZW = zw_mix2(n5799, n5800, 234u64);
    let n5803: ZW = zw_bits_n(n3148);
    let n5804: ZW = zw_mix1(n5801, n5803, 236u64);
    let n5805: ZW = zw_mix2(n5802, n5803, 236u64);
    let n5806: ZW = zw_bits_n(n3149);
    let n5807: ZW = zw_mix1(n5804, n5806, 237u64);
    let n5808: ZW = zw_mix2(n5805, n5806, 237u64);
    let n5809: ZW = zw_mix1(n5807, n5471, 239u64);
    let n5810: ZW = zw_mix2(n5808, n5471, 239u64);
    let n5811: ZW = zw_mix1(n5809, n5544, 246u64);
    let n5812: ZW = zw_mix2(n5810, n5544, 246u64);
    let n5813: ZW = zw_mix1(n5811, zw_splat(n5474), 247u64);
    let n5814: ZW = zw_mix2(n5812, zw_splat(n5474), 247u64);
    let n5815: ZW = zw_bits_n(n3181);
    let n5816: ZW = zw_mix1(n5813, n5815, 253u64);
    let n5817: ZW = zw_mix2(n5814, n5815, 253u64);
    let n5818: ZW = zw_bits_n(n3150);
    let n5819: ZW = zw_mix1(n5816, n5818, 268u64);
    let n5820: ZW = zw_mix2(n5817, n5818, 268u64);
    let n5821: ZW = zw_bits_n(n3151);
    let n5822: ZW = zw_mix1(n5819, n5821, 269u64);
    let n5823: ZW = zw_mix2(n5820, n5821, 269u64);
    let n5824: ZW = zw_bits_n(n3152);
    let n5825: ZW = zw_mix1(n5822, n5824, 270u64);
    let n5826: ZW = zw_mix2(n5823, n5824, 270u64);
    let n5827: ZW = zw_bits_n(n3153);
    let n5828: ZW = zw_mix1(n5825, n5827, 271u64);
    let n5829: ZW = zw_mix2(n5826, n5827, 271u64);
    let n5830: ZW = zw_mix1(n5828, n5510, 272u64);
    let n5831: ZW = zw_mix2(n5829, n5510, 272u64);
    let n5832: ZW = zw_bits_n(n3182);
    let n5833: ZW = zw_mix1(n5830, n5832, 280u64);
    let n5834: ZW = zw_mix2(n5831, n5832, 280u64);
    let n5835: ZW = zw_bits_n(n3155);
    let n5836: ZW = zw_mix1(n5833, n5835, 281u64);
    let n5837: ZW = zw_mix2(n5834, n5835, 281u64);
    let n5838: ZW = zw_bits_n(n3337);
    let n5839: ZW = zw_mix1(n5458, n5838, 20u64);
    let n5840: ZW = zw_mix2(n5459, n5838, 20u64);
    let n5841: ZW = zw_bits_b(n3338);
    let n5842: ZW = zw_mix1(n5839, n5841, 41u64);
    let n5843: ZW = zw_mix2(n5840, n5841, 41u64);
    let n5844: ZW = zw_bits_n(n3377);
    let n5845: ZW = zw_mix1(n5842, n5844, 234u64);
    let n5846: ZW = zw_mix2(n5843, n5844, 234u64);
    let n5847: ZW = zw_bits_n(n3340);
    let n5848: ZW = zw_mix1(n5845, n5847, 236u64);
    let n5849: ZW = zw_mix2(n5846, n5847, 236u64);
    let n5850: ZW = zw_bits_n(n3341);
    let n5851: ZW = zw_mix1(n5848, n5850, 237u64);
    let n5852: ZW = zw_mix2(n5849, n5850, 237u64);
    let n5853: ZW = zw_mix1(n5851, n5471, 239u64);
    let n5854: ZW = zw_mix2(n5852, n5471, 239u64);
    let n5855: ZW = zw_mix1(n5853, n5544, 246u64);
    let n5856: ZW = zw_mix2(n5854, n5544, 246u64);
    let n5857: ZW = zw_mix1(n5855, zw_splat(n5474), 247u64);
    let n5858: ZW = zw_mix2(n5856, zw_splat(n5474), 247u64);
    let n5859: ZW = zw_bits_n(n3373);
    let n5860: ZW = zw_mix1(n5857, n5859, 253u64);
    let n5861: ZW = zw_mix2(n5858, n5859, 253u64);
    let n5862: ZW = zw_bits_n(n3342);
    let n5863: ZW = zw_mix1(n5860, n5862, 268u64);
    let n5864: ZW = zw_mix2(n5861, n5862, 268u64);
    let n5865: ZW = zw_bits_n(n3343);
    let n5866: ZW = zw_mix1(n5863, n5865, 269u64);
    let n5867: ZW = zw_mix2(n5864, n5865, 269u64);
    let n5868: ZW = zw_bits_n(n3344);
    let n5869: ZW = zw_mix1(n5866, n5868, 270u64);
    let n5870: ZW = zw_mix2(n5867, n5868, 270u64);
    let n5871: ZW = zw_bits_n(n3345);
    let n5872: ZW = zw_mix1(n5869, n5871, 271u64);
    let n5873: ZW = zw_mix2(n5870, n5871, 271u64);
    let n5874: ZW = zw_mix1(n5872, n5530, 272u64);
    let n5875: ZW = zw_mix2(n5873, n5530, 272u64);
    let n5876: ZW = zw_bits_n(n3374);
    let n5877: ZW = zw_mix1(n5874, n5876, 280u64);
    let n5878: ZW = zw_mix2(n5875, n5876, 280u64);
    let n5879: ZW = zw_bits_n(n3347);
    let n5880: ZW = zw_mix1(n5877, n5879, 281u64);
    let n5881: ZW = zw_mix2(n5878, n5879, 281u64);
    let n5882: ZW = zw_bits_n(n3481);
    let n5883: ZW = zw_mix1(n5458, n5882, 20u64);
    let n5884: ZW = zw_mix2(n5459, n5882, 20u64);
    let n5885: ZW = zw_bits_b(n3482);
    let n5886: ZW = zw_mix1(n5883, n5885, 41u64);
    let n5887: ZW = zw_mix2(n5884, n5885, 41u64);
    let n5888: ZW = zw_bits_n(n3521);
    let n5889: ZW = zw_mix1(n5886, n5888, 234u64);
    let n5890: ZW = zw_mix2(n5887, n5888, 234u64);
    let n5891: ZW = zw_bits_n(n3484);
    let n5892: ZW = zw_mix1(n5889, n5891, 236u64);
    let n5893: ZW = zw_mix2(n5890, n5891, 236u64);
    let n5894: ZW = zw_bits_n(n3485);
    let n5895: ZW = zw_mix1(n5892, n5894, 237u64);
    let n5896: ZW = zw_mix2(n5893, n5894, 237u64);
    let n5897: ZW = zw_mix1(n5895, n5471, 239u64);
    let n5898: ZW = zw_mix2(n5896, n5471, 239u64);
    let n5899: ZW = zw_mix1(n5897, n5544, 246u64);
    let n5900: ZW = zw_mix2(n5898, n5544, 246u64);
    let n5901: ZW = zw_mix1(n5899, zw_splat(n5474), 247u64);
    let n5902: ZW = zw_mix2(n5900, zw_splat(n5474), 247u64);
    let n5903: ZW = zw_bits_n(n3517);
    let n5904: ZW = zw_mix1(n5901, n5903, 253u64);
    let n5905: ZW = zw_mix2(n5902, n5903, 253u64);
    let n5906: ZW = zw_bits_n(n3486);
    let n5907: ZW = zw_mix1(n5904, n5906, 268u64);
    let n5908: ZW = zw_mix2(n5905, n5906, 268u64);
    let n5909: ZW = zw_bits_n(n3487);
    let n5910: ZW = zw_mix1(n5907, n5909, 269u64);
    let n5911: ZW = zw_mix2(n5908, n5909, 269u64);
    let n5912: ZW = zw_bits_n(n3488);
    let n5913: ZW = zw_mix1(n5910, n5912, 270u64);
    let n5914: ZW = zw_mix2(n5911, n5912, 270u64);
    let n5915: ZW = zw_bits_n(n3489);
    let n5916: ZW = zw_mix1(n5913, n5915, 271u64);
    let n5917: ZW = zw_mix2(n5914, n5915, 271u64);
    let n5918: ZW = zw_mix1(n5916, n5490, 272u64);
    let n5919: ZW = zw_mix2(n5917, n5490, 272u64);
    let n5920: ZW = zw_bits_n(n3518);
    let n5921: ZW = zw_mix1(n5918, n5920, 280u64);
    let n5922: ZW = zw_mix2(n5919, n5920, 280u64);
    let n5923: ZW = zw_bits_n(n3491);
    let n5924: ZW = zw_mix1(n5921, n5923, 281u64);
    let n5925: ZW = zw_mix2(n5922, n5923, 281u64);
    let n5926: ZW = zw_bits_n(n3619);
    let n5927: ZW = zw_mix1(n5458, n5926, 20u64);
    let n5928: ZW = zw_mix2(n5459, n5926, 20u64);
    let n5929: ZW = zw_bits_b(n3620);
    let n5930: ZW = zw_mix1(n5927, n5929, 41u64);
    let n5931: ZW = zw_mix2(n5928, n5929, 41u64);
    let n5932: ZW = zw_bits_n(n3659);
    let n5933: ZW = zw_mix1(n5930, n5932, 234u64);
    let n5934: ZW = zw_mix2(n5931, n5932, 234u64);
    let n5935: ZW = zw_bits_n(n3622);
    let n5936: ZW = zw_mix1(n5933, n5935, 236u64);
    let n5937: ZW = zw_mix2(n5934, n5935, 236u64);
    let n5938: ZW = zw_bits_n(n3623);
    let n5939: ZW = zw_mix1(n5936, n5938, 237u64);
    let n5940: ZW = zw_mix2(n5937, n5938, 237u64);
    let n5941: ZW = zw_mix1(n5939, n5471, 239u64);
    let n5942: ZW = zw_mix2(n5940, n5471, 239u64);
    let n5943: ZW = zw_mix1(n5941, n5544, 246u64);
    let n5944: ZW = zw_mix2(n5942, n5544, 246u64);
    let n5945: ZW = zw_mix1(n5943, zw_splat(n5474), 247u64);
    let n5946: ZW = zw_mix2(n5944, zw_splat(n5474), 247u64);
    let n5947: ZW = zw_bits_n(n3655);
    let n5948: ZW = zw_mix1(n5945, n5947, 253u64);
    let n5949: ZW = zw_mix2(n5946, n5947, 253u64);
    let n5950: ZW = zw_bits_n(n3624);
    let n5951: ZW = zw_mix1(n5948, n5950, 268u64);
    let n5952: ZW = zw_mix2(n5949, n5950, 268u64);
    let n5953: ZW = zw_bits_n(n3625);
    let n5954: ZW = zw_mix1(n5951, n5953, 269u64);
    let n5955: ZW = zw_mix2(n5952, n5953, 269u64);
    let n5956: ZW = zw_bits_n(n3626);
    let n5957: ZW = zw_mix1(n5954, n5956, 270u64);
    let n5958: ZW = zw_mix2(n5955, n5956, 270u64);
    let n5959: ZW = zw_bits_n(n3627);
    let n5960: ZW = zw_mix1(n5957, n5959, 271u64);
    let n5961: ZW = zw_mix2(n5958, n5959, 271u64);
    let n5962: ZW = zw_mix1(n5960, n5510, 272u64);
    let n5963: ZW = zw_mix2(n5961, n5510, 272u64);
    let n5964: ZW = zw_bits_n(n3656);
    let n5965: ZW = zw_mix1(n5962, n5964, 280u64);
    let n5966: ZW = zw_mix2(n5963, n5964, 280u64);
    let n5967: ZW = zw_bits_n(n3629);
    let n5968: ZW = zw_mix1(n5965, n5967, 281u64);
    let n5969: ZW = zw_mix2(n5966, n5967, 281u64);
    let n5970: ZW = zw_bits_n(n3763);
    let n5971: ZW = zw_mix1(n5458, n5970, 20u64);
    let n5972: ZW = zw_mix2(n5459, n5970, 20u64);
    let n5973: ZW = zw_bits_b(n3764);
    let n5974: ZW = zw_mix1(n5971, n5973, 41u64);
    let n5975: ZW = zw_mix2(n5972, n5973, 41u64);
    let n5976: ZW = zw_bits_n(n3803);
    let n5977: ZW = zw_mix1(n5974, n5976, 234u64);
    let n5978: ZW = zw_mix2(n5975, n5976, 234u64);
    let n5979: ZW = zw_bits_n(n3766);
    let n5980: ZW = zw_mix1(n5977, n5979, 236u64);
    let n5981: ZW = zw_mix2(n5978, n5979, 236u64);
    let n5982: ZW = zw_bits_n(n3767);
    let n5983: ZW = zw_mix1(n5980, n5982, 237u64);
    let n5984: ZW = zw_mix2(n5981, n5982, 237u64);
    let n5985: ZW = zw_mix1(n5983, n5471, 239u64);
    let n5986: ZW = zw_mix2(n5984, n5471, 239u64);
    let n5987: ZW = zw_mix1(n5985, n5544, 246u64);
    let n5988: ZW = zw_mix2(n5986, n5544, 246u64);
    let n5989: ZW = zw_mix1(n5987, zw_splat(n5474), 247u64);
    let n5990: ZW = zw_mix2(n5988, zw_splat(n5474), 247u64);
    let n5991: ZW = zw_bits_n(n3799);
    let n5992: ZW = zw_mix1(n5989, n5991, 253u64);
    let n5993: ZW = zw_mix2(n5990, n5991, 253u64);
    let n5994: ZW = zw_bits_n(n3768);
    let n5995: ZW = zw_mix1(n5992, n5994, 268u64);
    let n5996: ZW = zw_mix2(n5993, n5994, 268u64);
    let n5997: ZW = zw_bits_n(n3769);
    let n5998: ZW = zw_mix1(n5995, n5997, 269u64);
    let n5999: ZW = zw_mix2(n5996, n5997, 269u64);
    let n6000: ZW = zw_bits_n(n3770);
    let n6001: ZW = zw_mix1(n5998, n6000, 270u64);
    let n6002: ZW = zw_mix2(n5999, n6000, 270u64);
    let n6003: ZW = zw_bits_n(n3771);
    let n6004: ZW = zw_mix1(n6001, n6003, 271u64);
    let n6005: ZW = zw_mix2(n6002, n6003, 271u64);
    let n6006: ZW = zw_mix1(n6004, n5530, 272u64);
    let n6007: ZW = zw_mix2(n6005, n5530, 272u64);
    let n6008: ZW = zw_bits_n(n3800);
    let n6009: ZW = zw_mix1(n6006, n6008, 280u64);
    let n6010: ZW = zw_mix2(n6007, n6008, 280u64);
    let n6011: ZW = zw_bits_n(n3773);
    let n6012: ZW = zw_mix1(n6009, n6011, 281u64);
    let n6013: ZW = zw_mix2(n6010, n6011, 281u64);
    let n6014: ZW = zw_bits_n(n3949);
    let n6015: ZW = zw_mix1(n5458, n6014, 20u64);
    let n6016: ZW = zw_mix2(n5459, n6014, 20u64);
    let n6017: ZW = zw_bits_b(n3950);
    let n6018: ZW = zw_mix1(n6015, n6017, 41u64);
    let n6019: ZW = zw_mix2(n6016, n6017, 41u64);
    let n6020: ZW = zw_bits_n(n3989);
    let n6021: ZW = zw_mix1(n6018, n6020, 234u64);
    let n6022: ZW = zw_mix2(n6019, n6020, 234u64);
    let n6023: ZW = zw_bits_n(n3952);
    let n6024: ZW = zw_mix1(n6021, n6023, 236u64);
    let n6025: ZW = zw_mix2(n6022, n6023, 236u64);
    let n6026: ZW = zw_bits_n(n3953);
    let n6027: ZW = zw_mix1(n6024, n6026, 237u64);
    let n6028: ZW = zw_mix2(n6025, n6026, 237u64);
    let n6029: ZW = zw_mix1(n6027, n5539, 239u64);
    let n6030: ZW = zw_mix2(n6028, n5539, 239u64);
    let n6031: ZW = zw_mix1(n6029, n5544, 246u64);
    let n6032: ZW = zw_mix2(n6030, n5544, 246u64);
    let n6033: ZW = zw_mix1(n6031, n5544, 247u64);
    let n6034: ZW = zw_mix2(n6032, n5544, 247u64);
    let n6035: ZW = zw_bits_n(n3985);
    let n6036: ZW = zw_mix1(n6033, n6035, 253u64);
    let n6037: ZW = zw_mix2(n6034, n6035, 253u64);
    let n6038: ZW = zw_bits_n(n3954);
    let n6039: ZW = zw_mix1(n6036, n6038, 268u64);
    let n6040: ZW = zw_mix2(n6037, n6038, 268u64);
    let n6041: ZW = zw_bits_n(n3955);
    let n6042: ZW = zw_mix1(n6039, n6041, 269u64);
    let n6043: ZW = zw_mix2(n6040, n6041, 269u64);
    let n6044: ZW = zw_bits_n(n3956);
    let n6045: ZW = zw_mix1(n6042, n6044, 270u64);
    let n6046: ZW = zw_mix2(n6043, n6044, 270u64);
    let n6047: ZW = zw_bits_n(n3957);
    let n6048: ZW = zw_mix1(n6045, n6047, 271u64);
    let n6049: ZW = zw_mix2(n6046, n6047, 271u64);
    let n6050: ZW = zw_mix1(n6048, n5490, 272u64);
    let n6051: ZW = zw_mix2(n6049, n5490, 272u64);
    let n6052: ZW = zw_bits_n(n3986);
    let n6053: ZW = zw_mix1(n6050, n6052, 280u64);
    let n6054: ZW = zw_mix2(n6051, n6052, 280u64);
    let n6055: ZW = zw_bits_n(n3959);
    let n6056: ZW = zw_mix1(n6053, n6055, 281u64);
    let n6057: ZW = zw_mix2(n6054, n6055, 281u64);
    let n6058: ZW = zw_bits_n(n4111);
    let n6059: ZW = zw_mix1(n5458, n6058, 20u64);
    let n6060: ZW = zw_mix2(n5459, n6058, 20u64);
    let n6061: ZW = zw_bits_b(n4112);
    let n6062: ZW = zw_mix1(n6059, n6061, 41u64);
    let n6063: ZW = zw_mix2(n6060, n6061, 41u64);
    let n6064: ZW = zw_bits_n(n4151);
    let n6065: ZW = zw_mix1(n6062, n6064, 234u64);
    let n6066: ZW = zw_mix2(n6063, n6064, 234u64);
    let n6067: ZW = zw_bits_n(n4114);
    let n6068: ZW = zw_mix1(n6065, n6067, 236u64);
    let n6069: ZW = zw_mix2(n6066, n6067, 236u64);
    let n6070: ZW = zw_bits_n(n4115);
    let n6071: ZW = zw_mix1(n6068, n6070, 237u64);
    let n6072: ZW = zw_mix2(n6069, n6070, 237u64);
    let n6073: ZW = zw_mix1(n6071, n5566, 239u64);
    let n6074: ZW = zw_mix2(n6072, n5566, 239u64);
    let n6075: ZW = zw_mix1(n6073, n5544, 246u64);
    let n6076: ZW = zw_mix2(n6074, n5544, 246u64);
    let n6077: ZW = zw_mix1(n6075, n5544, 247u64);
    let n6078: ZW = zw_mix2(n6076, n5544, 247u64);
    let n6079: ZW = zw_bits_n(n4147);
    let n6080: ZW = zw_mix1(n6077, n6079, 253u64);
    let n6081: ZW = zw_mix2(n6078, n6079, 253u64);
    let n6082: ZW = zw_bits_n(n4116);
    let n6083: ZW = zw_mix1(n6080, n6082, 268u64);
    let n6084: ZW = zw_mix2(n6081, n6082, 268u64);
    let n6085: ZW = zw_bits_n(n4117);
    let n6086: ZW = zw_mix1(n6083, n6085, 269u64);
    let n6087: ZW = zw_mix2(n6084, n6085, 269u64);
    let n6088: ZW = zw_bits_n(n4118);
    let n6089: ZW = zw_mix1(n6086, n6088, 270u64);
    let n6090: ZW = zw_mix2(n6087, n6088, 270u64);
    let n6091: ZW = zw_bits_n(n4119);
    let n6092: ZW = zw_mix1(n6089, n6091, 271u64);
    let n6093: ZW = zw_mix2(n6090, n6091, 271u64);
    let n6094: ZW = zw_mix1(n6092, n5510, 272u64);
    let n6095: ZW = zw_mix2(n6093, n5510, 272u64);
    let n6096: ZW = zw_bits_n(n4148);
    let n6097: ZW = zw_mix1(n6094, n6096, 280u64);
    let n6098: ZW = zw_mix2(n6095, n6096, 280u64);
    let n6099: ZW = zw_bits_n(n4121);
    let n6100: ZW = zw_mix1(n6097, n6099, 281u64);
    let n6101: ZW = zw_mix2(n6098, n6099, 281u64);
    let n6102: ZW = zw_bits_n(n4297);
    let n6103: ZW = zw_mix1(n5458, n6102, 20u64);
    let n6104: ZW = zw_mix2(n5459, n6102, 20u64);
    let n6105: ZW = zw_bits_b(n4298);
    let n6106: ZW = zw_mix1(n6103, n6105, 41u64);
    let n6107: ZW = zw_mix2(n6104, n6105, 41u64);
    let n6108: ZW = zw_bits_n(n4337);
    let n6109: ZW = zw_mix1(n6106, n6108, 234u64);
    let n6110: ZW = zw_mix2(n6107, n6108, 234u64);
    let n6111: ZW = zw_bits_n(n4300);
    let n6112: ZW = zw_mix1(n6109, n6111, 236u64);
    let n6113: ZW = zw_mix2(n6110, n6111, 236u64);
    let n6114: ZW = zw_bits_n(n4301);
    let n6115: ZW = zw_mix1(n6112, n6114, 237u64);
    let n6116: ZW = zw_mix2(n6113, n6114, 237u64);
    let n6117: ZW = zw_mix1(n6115, n5592, 239u64);
    let n6118: ZW = zw_mix2(n6116, n5592, 239u64);
    let n6119: ZW = zw_mix1(n6117, n5544, 246u64);
    let n6120: ZW = zw_mix2(n6118, n5544, 246u64);
    let n6121: ZW = zw_mix1(n6119, n5544, 247u64);
    let n6122: ZW = zw_mix2(n6120, n5544, 247u64);
    let n6123: ZW = zw_bits_n(n4333);
    let n6124: ZW = zw_mix1(n6121, n6123, 253u64);
    let n6125: ZW = zw_mix2(n6122, n6123, 253u64);
    let n6126: ZW = zw_bits_n(n4302);
    let n6127: ZW = zw_mix1(n6124, n6126, 268u64);
    let n6128: ZW = zw_mix2(n6125, n6126, 268u64);
    let n6129: ZW = zw_bits_n(n4303);
    let n6130: ZW = zw_mix1(n6127, n6129, 269u64);
    let n6131: ZW = zw_mix2(n6128, n6129, 269u64);
    let n6132: ZW = zw_bits_n(n4304);
    let n6133: ZW = zw_mix1(n6130, n6132, 270u64);
    let n6134: ZW = zw_mix2(n6131, n6132, 270u64);
    let n6135: ZW = zw_bits_n(n4305);
    let n6136: ZW = zw_mix1(n6133, n6135, 271u64);
    let n6137: ZW = zw_mix2(n6134, n6135, 271u64);
    let n6138: ZW = zw_mix1(n6136, n5530, 272u64);
    let n6139: ZW = zw_mix2(n6137, n5530, 272u64);
    let n6140: ZW = zw_bits_n(n4334);
    let n6141: ZW = zw_mix1(n6138, n6140, 280u64);
    let n6142: ZW = zw_mix2(n6139, n6140, 280u64);
    let n6143: ZW = zw_bits_n(n4307);
    let n6144: ZW = zw_mix1(n6141, n6143, 281u64);
    let n6145: ZW = zw_mix2(n6142, n6143, 281u64);
    let n6146: ZW = zw_bits_n(n4489);
    let n6147: ZW = zw_mix1(n5458, n6146, 20u64);
    let n6148: ZW = zw_mix2(n5459, n6146, 20u64);
    let n6149: ZW = zw_bits_b(n4490);
    let n6150: ZW = zw_mix1(n6147, n6149, 41u64);
    let n6151: ZW = zw_mix2(n6148, n6149, 41u64);
    let n6152: ZW = zw_bits_n(n4529);
    let n6153: ZW = zw_mix1(n6150, n6152, 234u64);
    let n6154: ZW = zw_mix2(n6151, n6152, 234u64);
    let n6155: ZW = zw_bits_n(n4492);
    let n6156: ZW = zw_mix1(n6153, n6155, 236u64);
    let n6157: ZW = zw_mix2(n6154, n6155, 236u64);
    let n6158: ZW = zw_bits_n(n4493);
    let n6159: ZW = zw_mix1(n6156, n6158, 237u64);
    let n6160: ZW = zw_mix2(n6157, n6158, 237u64);
    let n6161: ZW = zw_mix1(n6159, n5539, 239u64);
    let n6162: ZW = zw_mix2(n6160, n5539, 239u64);
    let n6163: ZW = zw_mix1(n6161, n5544, 246u64);
    let n6164: ZW = zw_mix2(n6162, n5544, 246u64);
    let n6165: ZW = zw_mix1(n6163, n5544, 247u64);
    let n6166: ZW = zw_mix2(n6164, n5544, 247u64);
    let n6167: ZW = zw_bits_n(n4525);
    let n6168: ZW = zw_mix1(n6165, n6167, 253u64);
    let n6169: ZW = zw_mix2(n6166, n6167, 253u64);
    let n6170: ZW = zw_bits_n(n4494);
    let n6171: ZW = zw_mix1(n6168, n6170, 268u64);
    let n6172: ZW = zw_mix2(n6169, n6170, 268u64);
    let n6173: ZW = zw_bits_n(n4495);
    let n6174: ZW = zw_mix1(n6171, n6173, 269u64);
    let n6175: ZW = zw_mix2(n6172, n6173, 269u64);
    let n6176: ZW = zw_bits_n(n4496);
    let n6177: ZW = zw_mix1(n6174, n6176, 270u64);
    let n6178: ZW = zw_mix2(n6175, n6176, 270u64);
    let n6179: ZW = zw_bits_n(n4497);
    let n6180: ZW = zw_mix1(n6177, n6179, 271u64);
    let n6181: ZW = zw_mix2(n6178, n6179, 271u64);
    let n6182: ZW = zw_mix1(n6180, n5490, 272u64);
    let n6183: ZW = zw_mix2(n6181, n5490, 272u64);
    let n6184: ZW = zw_bits_n(n4526);
    let n6185: ZW = zw_mix1(n6182, n6184, 280u64);
    let n6186: ZW = zw_mix2(n6183, n6184, 280u64);
    let n6187: ZW = zw_bits_n(n4499);
    let n6188: ZW = zw_mix1(n6185, n6187, 281u64);
    let n6189: ZW = zw_mix2(n6186, n6187, 281u64);
    let n6190: ZW = zw_bits_n(n4661);
    let n6191: ZW = zw_mix1(n5458, n6190, 20u64);
    let n6192: ZW = zw_mix2(n5459, n6190, 20u64);
    let n6193: ZW = zw_bits_b(n4662);
    let n6194: ZW = zw_mix1(n6191, n6193, 41u64);
    let n6195: ZW = zw_mix2(n6192, n6193, 41u64);
    let n6196: ZW = zw_bits_n(n4701);
    let n6197: ZW = zw_mix1(n6194, n6196, 234u64);
    let n6198: ZW = zw_mix2(n6195, n6196, 234u64);
    let n6199: ZW = zw_bits_n(n4664);
    let n6200: ZW = zw_mix1(n6197, n6199, 236u64);
    let n6201: ZW = zw_mix2(n6198, n6199, 236u64);
    let n6202: ZW = zw_bits_n(n4665);
    let n6203: ZW = zw_mix1(n6200, n6202, 237u64);
    let n6204: ZW = zw_mix2(n6201, n6202, 237u64);
    let n6205: ZW = zw_mix1(n6203, n5566, 239u64);
    let n6206: ZW = zw_mix2(n6204, n5566, 239u64);
    let n6207: ZW = zw_mix1(n6205, n5544, 246u64);
    let n6208: ZW = zw_mix2(n6206, n5544, 246u64);
    let n6209: ZW = zw_mix1(n6207, n5544, 247u64);
    let n6210: ZW = zw_mix2(n6208, n5544, 247u64);
    let n6211: ZW = zw_bits_n(n4697);
    let n6212: ZW = zw_mix1(n6209, n6211, 253u64);
    let n6213: ZW = zw_mix2(n6210, n6211, 253u64);
    let n6214: ZW = zw_bits_n(n4666);
    let n6215: ZW = zw_mix1(n6212, n6214, 268u64);
    let n6216: ZW = zw_mix2(n6213, n6214, 268u64);
    let n6217: ZW = zw_bits_n(n4667);
    let n6218: ZW = zw_mix1(n6215, n6217, 269u64);
    let n6219: ZW = zw_mix2(n6216, n6217, 269u64);
    let n6220: ZW = zw_bits_n(n4668);
    let n6221: ZW = zw_mix1(n6218, n6220, 270u64);
    let n6222: ZW = zw_mix2(n6219, n6220, 270u64);
    let n6223: ZW = zw_bits_n(n4669);
    let n6224: ZW = zw_mix1(n6221, n6223, 271u64);
    let n6225: ZW = zw_mix2(n6222, n6223, 271u64);
    let n6226: ZW = zw_mix1(n6224, n5510, 272u64);
    let n6227: ZW = zw_mix2(n6225, n5510, 272u64);
    let n6228: ZW = zw_bits_n(n4698);
    let n6229: ZW = zw_mix1(n6226, n6228, 280u64);
    let n6230: ZW = zw_mix2(n6227, n6228, 280u64);
    let n6231: ZW = zw_bits_n(n4671);
    let n6232: ZW = zw_mix1(n6229, n6231, 281u64);
    let n6233: ZW = zw_mix2(n6230, n6231, 281u64);
    let n6234: ZW = zw_bits_n(n4853);
    let n6235: ZW = zw_mix1(n5458, n6234, 20u64);
    let n6236: ZW = zw_mix2(n5459, n6234, 20u64);
    let n6237: ZW = zw_bits_b(n4854);
    let n6238: ZW = zw_mix1(n6235, n6237, 41u64);
    let n6239: ZW = zw_mix2(n6236, n6237, 41u64);
    let n6240: ZW = zw_bits_n(n4893);
    let n6241: ZW = zw_mix1(n6238, n6240, 234u64);
    let n6242: ZW = zw_mix2(n6239, n6240, 234u64);
    let n6243: ZW = zw_bits_n(n4856);
    let n6244: ZW = zw_mix1(n6241, n6243, 236u64);
    let n6245: ZW = zw_mix2(n6242, n6243, 236u64);
    let n6246: ZW = zw_bits_n(n4857);
    let n6247: ZW = zw_mix1(n6244, n6246, 237u64);
    let n6248: ZW = zw_mix2(n6245, n6246, 237u64);
    let n6249: ZW = zw_mix1(n6247, n5592, 239u64);
    let n6250: ZW = zw_mix2(n6248, n5592, 239u64);
    let n6251: ZW = zw_mix1(n6249, n5544, 246u64);
    let n6252: ZW = zw_mix2(n6250, n5544, 246u64);
    let n6253: ZW = zw_mix1(n6251, n5544, 247u64);
    let n6254: ZW = zw_mix2(n6252, n5544, 247u64);
    let n6255: ZW = zw_bits_n(n4889);
    let n6256: ZW = zw_mix1(n6253, n6255, 253u64);
    let n6257: ZW = zw_mix2(n6254, n6255, 253u64);
    let n6258: ZW = zw_bits_n(n4858);
    let n6259: ZW = zw_mix1(n6256, n6258, 268u64);
    let n6260: ZW = zw_mix2(n6257, n6258, 268u64);
    let n6261: ZW = zw_bits_n(n4859);
    let n6262: ZW = zw_mix1(n6259, n6261, 269u64);
    let n6263: ZW = zw_mix2(n6260, n6261, 269u64);
    let n6264: ZW = zw_bits_n(n4860);
    let n6265: ZW = zw_mix1(n6262, n6264, 270u64);
    let n6266: ZW = zw_mix2(n6263, n6264, 270u64);
    let n6267: ZW = zw_bits_n(n4861);
    let n6268: ZW = zw_mix1(n6265, n6267, 271u64);
    let n6269: ZW = zw_mix2(n6266, n6267, 271u64);
    let n6270: ZW = zw_mix1(n6268, n5530, 272u64);
    let n6271: ZW = zw_mix2(n6269, n5530, 272u64);
    let n6272: ZW = zw_bits_n(n4890);
    let n6273: ZW = zw_mix1(n6270, n6272, 280u64);
    let n6274: ZW = zw_mix2(n6271, n6272, 280u64);
    let n6275: ZW = zw_bits_n(n4863);
    let n6276: ZW = zw_mix1(n6273, n6275, 281u64);
    let n6277: ZW = zw_mix2(n6274, n6275, 281u64);
    let n6278: ZW = zw_bits_n(n4997);
    let n6279: ZW = zw_mix1(n5458, n6278, 20u64);
    let n6280: ZW = zw_mix2(n5459, n6278, 20u64);
    let n6281: ZW = zw_bits_b(n4998);
    let n6282: ZW = zw_mix1(n6279, n6281, 41u64);
    let n6283: ZW = zw_mix2(n6280, n6281, 41u64);
    let n6284: ZW = zw_bits_n(n5037);
    let n6285: ZW = zw_mix1(n6282, n6284, 234u64);
    let n6286: ZW = zw_mix2(n6283, n6284, 234u64);
    let n6287: ZW = zw_bits_n(n5000);
    let n6288: ZW = zw_mix1(n6285, n6287, 236u64);
    let n6289: ZW = zw_mix2(n6286, n6287, 236u64);
    let n6290: ZW = zw_bits_n(n5001);
    let n6291: ZW = zw_mix1(n6288, n6290, 237u64);
    let n6292: ZW = zw_mix2(n6289, n6290, 237u64);
    let n6293: ZW = zw_mix1(n6291, n5539, 239u64);
    let n6294: ZW = zw_mix2(n6292, n5539, 239u64);
    let n6295: ZW = zw_mix1(n6293, n5544, 246u64);
    let n6296: ZW = zw_mix2(n6294, n5544, 246u64);
    let n6297: ZW = zw_mix1(n6295, n5544, 247u64);
    let n6298: ZW = zw_mix2(n6296, n5544, 247u64);
    let n6299: ZW = zw_bits_n(n5033);
    let n6300: ZW = zw_mix1(n6297, n6299, 253u64);
    let n6301: ZW = zw_mix2(n6298, n6299, 253u64);
    let n6302: ZW = zw_bits_n(n5002);
    let n6303: ZW = zw_mix1(n6300, n6302, 268u64);
    let n6304: ZW = zw_mix2(n6301, n6302, 268u64);
    let n6305: ZW = zw_bits_n(n5003);
    let n6306: ZW = zw_mix1(n6303, n6305, 269u64);
    let n6307: ZW = zw_mix2(n6304, n6305, 269u64);
    let n6308: ZW = zw_bits_n(n5004);
    let n6309: ZW = zw_mix1(n6306, n6308, 270u64);
    let n6310: ZW = zw_mix2(n6307, n6308, 270u64);
    let n6311: ZW = zw_bits_n(n5005);
    let n6312: ZW = zw_mix1(n6309, n6311, 271u64);
    let n6313: ZW = zw_mix2(n6310, n6311, 271u64);
    let n6314: ZW = zw_mix1(n6312, n5490, 272u64);
    let n6315: ZW = zw_mix2(n6313, n5490, 272u64);
    let n6316: ZW = zw_bits_n(n5034);
    let n6317: ZW = zw_mix1(n6314, n6316, 280u64);
    let n6318: ZW = zw_mix2(n6315, n6316, 280u64);
    let n6319: ZW = zw_bits_n(n5007);
    let n6320: ZW = zw_mix1(n6317, n6319, 281u64);
    let n6321: ZW = zw_mix2(n6318, n6319, 281u64);
    let n6322: ZW = zw_bits_n(n5135);
    let n6323: ZW = zw_mix1(n5458, n6322, 20u64);
    let n6324: ZW = zw_mix2(n5459, n6322, 20u64);
    let n6325: ZW = zw_bits_b(n5136);
    let n6326: ZW = zw_mix1(n6323, n6325, 41u64);
    let n6327: ZW = zw_mix2(n6324, n6325, 41u64);
    let n6328: ZW = zw_bits_n(n5175);
    let n6329: ZW = zw_mix1(n6326, n6328, 234u64);
    let n6330: ZW = zw_mix2(n6327, n6328, 234u64);
    let n6331: ZW = zw_bits_n(n5138);
    let n6332: ZW = zw_mix1(n6329, n6331, 236u64);
    let n6333: ZW = zw_mix2(n6330, n6331, 236u64);
    let n6334: ZW = zw_bits_n(n5139);
    let n6335: ZW = zw_mix1(n6332, n6334, 237u64);
    let n6336: ZW = zw_mix2(n6333, n6334, 237u64);
    let n6337: ZW = zw_mix1(n6335, n5566, 239u64);
    let n6338: ZW = zw_mix2(n6336, n5566, 239u64);
    let n6339: ZW = zw_mix1(n6337, n5544, 246u64);
    let n6340: ZW = zw_mix2(n6338, n5544, 246u64);
    let n6341: ZW = zw_mix1(n6339, n5544, 247u64);
    let n6342: ZW = zw_mix2(n6340, n5544, 247u64);
    let n6343: ZW = zw_bits_n(n5171);
    let n6344: ZW = zw_mix1(n6341, n6343, 253u64);
    let n6345: ZW = zw_mix2(n6342, n6343, 253u64);
    let n6346: ZW = zw_bits_n(n5140);
    let n6347: ZW = zw_mix1(n6344, n6346, 268u64);
    let n6348: ZW = zw_mix2(n6345, n6346, 268u64);
    let n6349: ZW = zw_bits_n(n5141);
    let n6350: ZW = zw_mix1(n6347, n6349, 269u64);
    let n6351: ZW = zw_mix2(n6348, n6349, 269u64);
    let n6352: ZW = zw_bits_n(n5142);
    let n6353: ZW = zw_mix1(n6350, n6352, 270u64);
    let n6354: ZW = zw_mix2(n6351, n6352, 270u64);
    let n6355: ZW = zw_bits_n(n5143);
    let n6356: ZW = zw_mix1(n6353, n6355, 271u64);
    let n6357: ZW = zw_mix2(n6354, n6355, 271u64);
    let n6358: ZW = zw_mix1(n6356, n5510, 272u64);
    let n6359: ZW = zw_mix2(n6357, n5510, 272u64);
    let n6360: ZW = zw_bits_n(n5172);
    let n6361: ZW = zw_mix1(n6358, n6360, 280u64);
    let n6362: ZW = zw_mix2(n6359, n6360, 280u64);
    let n6363: ZW = zw_bits_n(n5145);
    let n6364: ZW = zw_mix1(n6361, n6363, 281u64);
    let n6365: ZW = zw_mix2(n6362, n6363, 281u64);
    let n6366: ZW = zw_bits_n(n5279);
    let n6367: ZW = zw_mix1(n5458, n6366, 20u64);
    let n6368: ZW = zw_mix2(n5459, n6366, 20u64);
    let n6369: ZW = zw_bits_b(n5280);
    let n6370: ZW = zw_mix1(n6367, n6369, 41u64);
    let n6371: ZW = zw_mix2(n6368, n6369, 41u64);
    let n6372: ZW = zw_bits_n(n5319);
    let n6373: ZW = zw_mix1(n6370, n6372, 234u64);
    let n6374: ZW = zw_mix2(n6371, n6372, 234u64);
    let n6375: ZW = zw_bits_n(n5282);
    let n6376: ZW = zw_mix1(n6373, n6375, 236u64);
    let n6377: ZW = zw_mix2(n6374, n6375, 236u64);
    let n6378: ZW = zw_bits_n(n5283);
    let n6379: ZW = zw_mix1(n6376, n6378, 237u64);
    let n6380: ZW = zw_mix2(n6377, n6378, 237u64);
    let n6381: ZW = zw_mix1(n6379, n5592, 239u64);
    let n6382: ZW = zw_mix2(n6380, n5592, 239u64);
    let n6383: ZW = zw_mix1(n6381, n5544, 246u64);
    let n6384: ZW = zw_mix2(n6382, n5544, 246u64);
    let n6385: ZW = zw_mix1(n6383, n5544, 247u64);
    let n6386: ZW = zw_mix2(n6384, n5544, 247u64);
    let n6387: ZW = zw_bits_n(n5315);
    let n6388: ZW = zw_mix1(n6385, n6387, 253u64);
    let n6389: ZW = zw_mix2(n6386, n6387, 253u64);
    let n6390: ZW = zw_bits_n(n5284);
    let n6391: ZW = zw_mix1(n6388, n6390, 268u64);
    let n6392: ZW = zw_mix2(n6389, n6390, 268u64);
    let n6393: ZW = zw_bits_n(n5285);
    let n6394: ZW = zw_mix1(n6391, n6393, 269u64);
    let n6395: ZW = zw_mix2(n6392, n6393, 269u64);
    let n6396: ZW = zw_bits_n(n5286);
    let n6397: ZW = zw_mix1(n6394, n6396, 270u64);
    let n6398: ZW = zw_mix2(n6395, n6396, 270u64);
    let n6399: ZW = zw_bits_n(n5287);
    let n6400: ZW = zw_mix1(n6397, n6399, 271u64);
    let n6401: ZW = zw_mix2(n6398, n6399, 271u64);
    let n6402: ZW = zw_mix1(n6400, n5530, 272u64);
    let n6403: ZW = zw_mix2(n6401, n5530, 272u64);
    let n6404: ZW = zw_bits_n(n5316);
    let n6405: ZW = zw_mix1(n6402, n6404, 280u64);
    let n6406: ZW = zw_mix2(n6403, n6404, 280u64);
    let n6407: ZW = zw_bits_n(n5289);
    let n6408: ZW = zw_mix1(n6405, n6407, 281u64);
    let n6409: ZW = zw_mix2(n6406, n6407, 281u64);
    let n6410: ZW = zw_mix1(zw_splat(11400714819323198485u64), n5325, 43u64);
    let n6411: ZW = zw_mix2(zw_splat(11562461410679940143u64), n5325, 43u64);
    let n6412: ZW = zw_mix1(n6410, n5328, 88u64);
    let n6413: ZW = zw_mix2(n6411, n5328, 88u64);
    let n6414: ZW = zw_mix1(n6412, n5331, 20u64);
    let n6415: ZW = zw_mix2(n6413, n5331, 20u64);
    let n6416: ZW = zw_bits_b(n1642);
    let n6417: ZW = zw_mix1(n6414, n6416, 38u64);
    let n6418: ZW = zw_mix2(n6415, n6416, 38u64);
    let n6419: ZW = zw_bits_n(n1640);
    let n6420: ZW = zw_mix1(n6417, n6419, 39u64);
    let n6421: ZW = zw_mix2(n6418, n6419, 39u64);
    let n6422: ZW = zw_bits_b(n1777);
    let n6423: ZW = zw_mix1(n6414, n6422, 38u64);
    let n6424: ZW = zw_mix2(n6415, n6422, 38u64);
    let n6425: ZW = zw_bits_n(n1775);
    let n6426: ZW = zw_mix1(n6423, n6425, 39u64);
    let n6427: ZW = zw_mix2(n6424, n6425, 39u64);
    let n6428: ZW = zw_bits_b(n1900);
    let n6429: ZW = zw_mix1(n6414, n6428, 38u64);
    let n6430: ZW = zw_mix2(n6415, n6428, 38u64);
    let n6431: ZW = zw_bits_n(n1898);
    let n6432: ZW = zw_mix1(n6429, n6431, 39u64);
    let n6433: ZW = zw_mix2(n6430, n6431, 39u64);
    let n6434: ZW = zw_bits_b(n2026);
    let n6435: ZW = zw_mix1(n6414, n6434, 38u64);
    let n6436: ZW = zw_mix2(n6415, n6434, 38u64);
    let n6437: ZW = zw_bits_n(n2024);
    let n6438: ZW = zw_mix1(n6435, n6437, 39u64);
    let n6439: ZW = zw_mix2(n6436, n6437, 39u64);
    let n6440: ZW = zw_bits_b(n2146);
    let n6441: ZW = zw_mix1(n6414, n6440, 38u64);
    let n6442: ZW = zw_mix2(n6415, n6440, 38u64);
    let n6443: ZW = zw_bits_n(n2144);
    let n6444: ZW = zw_mix1(n6441, n6443, 39u64);
    let n6445: ZW = zw_mix2(n6442, n6443, 39u64);
    let n6446: ZW = zw_bits_b(n2267);
    let n6447: ZW = zw_mix1(n6414, n6446, 38u64);
    let n6448: ZW = zw_mix2(n6415, n6446, 38u64);
    let n6449: ZW = zw_bits_n(n2265);
    let n6450: ZW = zw_mix1(n6447, n6449, 39u64);
    let n6451: ZW = zw_mix2(n6448, n6449, 39u64);
    let n6452: ZW = zw_bits_n(n2428);
    let n6453: ZW = zw_mix1(n6412, n6452, 20u64);
    let n6454: ZW = zw_mix2(n6413, n6452, 20u64);
    let n6455: ZW = zw_bits_b(n2430);
    let n6456: ZW = zw_mix1(n6453, n6455, 38u64);
    let n6457: ZW = zw_mix2(n6454, n6455, 38u64);
    let n6458: ZW = zw_bits_n(n2427);
    let n6459: ZW = zw_mix1(n6456, n6458, 39u64);
    let n6460: ZW = zw_mix2(n6457, n6458, 39u64);
    let n6461: ZW = zw_bits_n(n2590);
    let n6462: ZW = zw_mix1(n6412, n6461, 20u64);
    let n6463: ZW = zw_mix2(n6413, n6461, 20u64);
    let n6464: ZW = zw_bits_b(n2592);
    let n6465: ZW = zw_mix1(n6462, n6464, 38u64);
    let n6466: ZW = zw_mix2(n6463, n6464, 38u64);
    let n6467: ZW = zw_bits_n(n2589);
    let n6468: ZW = zw_mix1(n6465, n6467, 39u64);
    let n6469: ZW = zw_mix2(n6466, n6467, 39u64);
    let n6470: ZW = zw_bits_n(n2776);
    let n6471: ZW = zw_mix1(n6412, n6470, 20u64);
    let n6472: ZW = zw_mix2(n6413, n6470, 20u64);
    let n6473: ZW = zw_bits_b(n2778);
    let n6474: ZW = zw_mix1(n6471, n6473, 38u64);
    let n6475: ZW = zw_mix2(n6472, n6473, 38u64);
    let n6476: ZW = zw_bits_n(n2775);
    let n6477: ZW = zw_mix1(n6474, n6476, 39u64);
    let n6478: ZW = zw_mix2(n6475, n6476, 39u64);
    let n6479: ZW = zw_bits_n(n2968);
    let n6480: ZW = zw_mix1(n6412, n6479, 20u64);
    let n6481: ZW = zw_mix2(n6413, n6479, 20u64);
    let n6482: ZW = zw_bits_b(n2970);
    let n6483: ZW = zw_mix1(n6480, n6482, 38u64);
    let n6484: ZW = zw_mix2(n6481, n6482, 38u64);
    let n6485: ZW = zw_bits_n(n2967);
    let n6486: ZW = zw_mix1(n6483, n6485, 39u64);
    let n6487: ZW = zw_mix2(n6484, n6485, 39u64);
    let n6488: ZW = zw_bits_n(n3140);
    let n6489: ZW = zw_mix1(n6412, n6488, 20u64);
    let n6490: ZW = zw_mix2(n6413, n6488, 20u64);
    let n6491: ZW = zw_bits_b(n3142);
    let n6492: ZW = zw_mix1(n6489, n6491, 38u64);
    let n6493: ZW = zw_mix2(n6490, n6491, 38u64);
    let n6494: ZW = zw_bits_n(n3139);
    let n6495: ZW = zw_mix1(n6492, n6494, 39u64);
    let n6496: ZW = zw_mix2(n6493, n6494, 39u64);
    let n6497: ZW = zw_bits_n(n3332);
    let n6498: ZW = zw_mix1(n6412, n6497, 20u64);
    let n6499: ZW = zw_mix2(n6413, n6497, 20u64);
    let n6500: ZW = zw_bits_b(n3334);
    let n6501: ZW = zw_mix1(n6498, n6500, 38u64);
    let n6502: ZW = zw_mix2(n6499, n6500, 38u64);
    let n6503: ZW = zw_bits_n(n3331);
    let n6504: ZW = zw_mix1(n6501, n6503, 39u64);
    let n6505: ZW = zw_mix2(n6502, n6503, 39u64);
    let n6506: ZW = zw_bits_n(n3476);
    let n6507: ZW = zw_mix1(n6412, n6506, 20u64);
    let n6508: ZW = zw_mix2(n6413, n6506, 20u64);
    let n6509: ZW = zw_bits_b(n3478);
    let n6510: ZW = zw_mix1(n6507, n6509, 38u64);
    let n6511: ZW = zw_mix2(n6508, n6509, 38u64);
    let n6512: ZW = zw_bits_n(n3475);
    let n6513: ZW = zw_mix1(n6510, n6512, 39u64);
    let n6514: ZW = zw_mix2(n6511, n6512, 39u64);
    let n6515: ZW = zw_bits_n(n3614);
    let n6516: ZW = zw_mix1(n6412, n6515, 20u64);
    let n6517: ZW = zw_mix2(n6413, n6515, 20u64);
    let n6518: ZW = zw_bits_b(n3616);
    let n6519: ZW = zw_mix1(n6516, n6518, 38u64);
    let n6520: ZW = zw_mix2(n6517, n6518, 38u64);
    let n6521: ZW = zw_bits_n(n3613);
    let n6522: ZW = zw_mix1(n6519, n6521, 39u64);
    let n6523: ZW = zw_mix2(n6520, n6521, 39u64);
    let n6524: ZW = zw_bits_n(n3758);
    let n6525: ZW = zw_mix1(n6412, n6524, 20u64);
    let n6526: ZW = zw_mix2(n6413, n6524, 20u64);
    let n6527: ZW = zw_bits_b(n3760);
    let n6528: ZW = zw_mix1(n6525, n6527, 38u64);
    let n6529: ZW = zw_mix2(n6526, n6527, 38u64);
    let n6530: ZW = zw_bits_n(n3757);
    let n6531: ZW = zw_mix1(n6528, n6530, 39u64);
    let n6532: ZW = zw_mix2(n6529, n6530, 39u64);
    let n6533: ZW = zw_bits_n(n3944);
    let n6534: ZW = zw_mix1(n6412, n6533, 20u64);
    let n6535: ZW = zw_mix2(n6413, n6533, 20u64);
    let n6536: ZW = zw_bits_b(n3946);
    let n6537: ZW = zw_mix1(n6534, n6536, 38u64);
    let n6538: ZW = zw_mix2(n6535, n6536, 38u64);
    let n6539: ZW = zw_bits_n(n3943);
    let n6540: ZW = zw_mix1(n6537, n6539, 39u64);
    let n6541: ZW = zw_mix2(n6538, n6539, 39u64);
    let n6542: ZW = zw_bits_n(n4106);
    let n6543: ZW = zw_mix1(n6412, n6542, 20u64);
    let n6544: ZW = zw_mix2(n6413, n6542, 20u64);
    let n6545: ZW = zw_bits_b(n4108);
    let n6546: ZW = zw_mix1(n6543, n6545, 38u64);
    let n6547: ZW = zw_mix2(n6544, n6545, 38u64);
    let n6548: ZW = zw_bits_n(n4105);
    let n6549: ZW = zw_mix1(n6546, n6548, 39u64);
    let n6550: ZW = zw_mix2(n6547, n6548, 39u64);
    let n6551: ZW = zw_bits_n(n4292);
    let n6552: ZW = zw_mix1(n6412, n6551, 20u64);
    let n6553: ZW = zw_mix2(n6413, n6551, 20u64);
    let n6554: ZW = zw_bits_b(n4294);
    let n6555: ZW = zw_mix1(n6552, n6554, 38u64);
    let n6556: ZW = zw_mix2(n6553, n6554, 38u64);
    let n6557: ZW = zw_bits_n(n4291);
    let n6558: ZW = zw_mix1(n6555, n6557, 39u64);
    let n6559: ZW = zw_mix2(n6556, n6557, 39u64);
    let n6560: ZW = zw_bits_n(n4484);
    let n6561: ZW = zw_mix1(n6412, n6560, 20u64);
    let n6562: ZW = zw_mix2(n6413, n6560, 20u64);
    let n6563: ZW = zw_bits_b(n4486);
    let n6564: ZW = zw_mix1(n6561, n6563, 38u64);
    let n6565: ZW = zw_mix2(n6562, n6563, 38u64);
    let n6566: ZW = zw_bits_n(n4483);
    let n6567: ZW = zw_mix1(n6564, n6566, 39u64);
    let n6568: ZW = zw_mix2(n6565, n6566, 39u64);
    let n6569: ZW = zw_bits_n(n4656);
    let n6570: ZW = zw_mix1(n6412, n6569, 20u64);
    let n6571: ZW = zw_mix2(n6413, n6569, 20u64);
    let n6572: ZW = zw_bits_b(n4658);
    let n6573: ZW = zw_mix1(n6570, n6572, 38u64);
    let n6574: ZW = zw_mix2(n6571, n6572, 38u64);
    let n6575: ZW = zw_bits_n(n4655);
    let n6576: ZW = zw_mix1(n6573, n6575, 39u64);
    let n6577: ZW = zw_mix2(n6574, n6575, 39u64);
    let n6578: ZW = zw_bits_n(n4848);
    let n6579: ZW = zw_mix1(n6412, n6578, 20u64);
    let n6580: ZW = zw_mix2(n6413, n6578, 20u64);
    let n6581: ZW = zw_bits_b(n4850);
    let n6582: ZW = zw_mix1(n6579, n6581, 38u64);
    let n6583: ZW = zw_mix2(n6580, n6581, 38u64);
    let n6584: ZW = zw_bits_n(n4847);
    let n6585: ZW = zw_mix1(n6582, n6584, 39u64);
    let n6586: ZW = zw_mix2(n6583, n6584, 39u64);
    let n6587: ZW = zw_bits_n(n4992);
    let n6588: ZW = zw_mix1(n6412, n6587, 20u64);
    let n6589: ZW = zw_mix2(n6413, n6587, 20u64);
    let n6590: ZW = zw_bits_b(n4994);
    let n6591: ZW = zw_mix1(n6588, n6590, 38u64);
    let n6592: ZW = zw_mix2(n6589, n6590, 38u64);
    let n6593: ZW = zw_bits_n(n4991);
    let n6594: ZW = zw_mix1(n6591, n6593, 39u64);
    let n6595: ZW = zw_mix2(n6592, n6593, 39u64);
    let n6596: ZW = zw_bits_n(n5130);
    let n6597: ZW = zw_mix1(n6412, n6596, 20u64);
    let n6598: ZW = zw_mix2(n6413, n6596, 20u64);
    let n6599: ZW = zw_bits_b(n5132);
    let n6600: ZW = zw_mix1(n6597, n6599, 38u64);
    let n6601: ZW = zw_mix2(n6598, n6599, 38u64);
    let n6602: ZW = zw_bits_n(n5129);
    let n6603: ZW = zw_mix1(n6600, n6602, 39u64);
    let n6604: ZW = zw_mix2(n6601, n6602, 39u64);
    let n6605: ZW = zw_bits_n(n5274);
    let n6606: ZW = zw_mix1(n6412, n6605, 20u64);
    let n6607: ZW = zw_mix2(n6413, n6605, 20u64);
    let n6608: ZW = zw_bits_b(n5276);
    let n6609: ZW = zw_mix1(n6606, n6608, 38u64);
    let n6610: ZW = zw_mix2(n6607, n6608, 38u64);
    let n6611: ZW = zw_bits_n(n5273);
    let n6612: ZW = zw_mix1(n6609, n6611, 39u64);
    let n6613: ZW = zw_mix2(n6610, n6611, 39u64);
    let n6614: ZW = zw_bits_n(n1654);
    let n6615: ZW = zw_mix1(zw_splat(11400714819323198485u64), n6614, 20u64);
    let n6616: ZW = zw_mix2(zw_splat(11562461410679940143u64), n6614, 20u64);
    let n6617: ZW = zw_bits_b(n1657);
    let n6618: ZW = zw_mix1(n6615, n6617, 38u64);
    let n6619: ZW = zw_mix2(n6616, n6617, 38u64);
    let n6620: ZW = zw_bits_n(n1653);
    let n6621: ZW = zw_mix1(n6618, n6620, 39u64);
    let n6622: ZW = zw_mix2(n6619, n6620, 39u64);
    let n6623: ZW = zw_bits_b(n1655);
    let n6624: ZW = zw_mix1(n6621, n6623, 41u64);
    let n6625: ZW = zw_mix2(n6622, n6623, 41u64);
    let n6626: ZW = zw_bits_b(n1656);
    let n6627: ZW = zw_mix1(n6624, n6626, 42u64);
    let n6628: ZW = zw_mix2(n6625, n6626, 42u64);
    let n6629: ZW = zw_mix1(n6627, n5325, 43u64);
    let n6630: ZW = zw_mix2(n6628, n5325, 43u64);
    let n6631: ZW = zw_mix1(n6629, n5328, 88u64);
    let n6632: ZW = zw_mix2(n6630, n5328, 88u64);
    let n6633: ZW = zw_bits_b(n1658);
    let n6634: ZW = zw_mix1(n6631, n6633, 232u64);
    let n6635: ZW = zw_mix2(n6632, n6633, 232u64);
    let n6636: ZW = zw_bits_n(n1659);
    let n6637: ZW = zw_mix1(n6634, n6636, 233u64);
    let n6638: ZW = zw_mix2(n6635, n6636, 233u64);
    let n6639: ZW = zw_bits_b(n1660);
    let n6640: ZW = zw_mix1(n6637, n6639, 242u64);
    let n6641: ZW = zw_mix2(n6638, n6639, 242u64);
    let n6642: ZW = zw_bits_n(n1661);
    let n6643: ZW = zw_mix1(n6640, n6642, 244u64);
    let n6644: ZW = zw_mix2(n6641, n6642, 244u64);
    let n6645: ZW = zw_bits_n(n1662);
    let n6646: ZW = zw_mix1(n6643, n6645, 245u64);
    let n6647: ZW = zw_mix2(n6644, n6645, 245u64);
    let n6648: ZW = zw_bits_n(n1663);
    let n6649: ZW = zw_mix1(n6646, n6648, 248u64);
    let n6650: ZW = zw_mix2(n6647, n6648, 248u64);
    let n6651: ZW = zw_bits_n(n1664);
    let n6652: ZW = zw_mix1(n6649, n6651, 249u64);
    let n6653: ZW = zw_mix2(n6650, n6651, 249u64);
    let n6654: ZW = zw_bits_b(n1665);
    let n6655: ZW = zw_mix1(n6652, n6654, 262u64);
    let n6656: ZW = zw_mix2(n6653, n6654, 262u64);
    let n6657: ZW = zw_bits_b(n1666);
    let n6658: ZW = zw_mix1(n6655, n6657, 263u64);
    let n6659: ZW = zw_mix2(n6656, n6657, 263u64);
    let n6660: ZW = zw_bits_n(n1667);
    let n6661: ZW = zw_mix1(n6658, n6660, 264u64);
    let n6662: ZW = zw_mix2(n6659, n6660, 264u64);
    let n6663: ZW = zw_bits_n(n1668);
    let n6664: ZW = zw_mix1(n6661, n6663, 265u64);
    let n6665: ZW = zw_mix2(n6662, n6663, 265u64);
    let n6666: ZW = zw_bits_n(n1669);
    let n6667: ZW = zw_mix1(n6664, n6666, 266u64);
    let n6668: ZW = zw_mix2(n6665, n6666, 266u64);
    let n6669: ZW = zw_bits_n(n1670);
    let n6670: ZW = zw_mix1(n6667, n6669, 267u64);
    let n6671: ZW = zw_mix2(n6668, n6669, 267u64);
    let n6672: ZW = zw_bits_n(n1671);
    let n6673: ZW = zw_mix1(n6670, n6672, 268u64);
    let n6674: ZW = zw_mix2(n6671, n6672, 268u64);
    let n6675: ZW = zw_bits_n(n1672);
    let n6676: ZW = zw_mix1(n6673, n6675, 269u64);
    let n6677: ZW = zw_mix2(n6674, n6675, 269u64);
    let n6678: ZW = zw_bits_n(n1673);
    let n6679: ZW = zw_mix1(n6676, n6678, 270u64);
    let n6680: ZW = zw_mix2(n6677, n6678, 270u64);
    let n6681: ZW = zw_bits_n(n1674);
    let n6682: ZW = zw_mix1(n6679, n6681, 271u64);
    let n6683: ZW = zw_mix2(n6680, n6681, 271u64);
    let n6684: ZW = zw_bits_n(n1675);
    let n6685: ZW = zw_mix1(n6682, n6684, 272u64);
    let n6686: ZW = zw_mix2(n6683, n6684, 272u64);
    let n6687: ZW = zw_bits_n(n1676);
    let n6688: ZW = zw_mix1(n6685, n6687, 273u64);
    let n6689: ZW = zw_mix2(n6686, n6687, 273u64);
    let ok_v0_o0: u16 = ALL & zb_holds(n1472);
    let bd_v0_o0: bool = false;
    let live_v0_o0: u16 = ALL & zb_holds(n57) & zb_holds(n1636) & zb_holds(n1637);
    let ok_v0_o1: u16 = ALL & zb_holds(n1651);
    let bd_v0_o1: bool = false;
    let live_v0_o1: u16 = ALL & zb_holds(n1697);
    let ok_v0_o2: u16 = ALL & zb_holds(n1644);
    let bd_v0_o2: bool = false;
    let live_v0_o2: u16 = ALL & zb_holds(n57) & zb_holds(n1643);
    let ok_v0_o3: u16 = ALL & zb_holds(n1678);
    let bd_v0_o3: bool = false;
    let live_v0_o3: u16 = ALL & zb_holds(n1703);
    let ok_v1_o0: u16 = ALL & zb_holds(n1472);
    let bd_v1_o0: bool = false;
    let live_v1_o0: u16 = ALL & zb_holds(n57) & zb_holds(n1771) & zb_holds(n1772);
    let ok_v1_o1: u16 = ALL & zb_holds(n1651);
    let bd_v1_o1: bool = false;
    let live_v1_o1: u16 = ALL & zb_holds(n1796);
    let ok_v1_o2: u16 = ALL & zb_holds(n1779);
    let bd_v1_o2: bool = false;
    let live_v1_o2: u16 = ALL & zb_holds(n57) & zb_holds(n1778);
    let ok_v1_o3: u16 = ALL & zb_holds(n1678);
    let bd_v1_o3: bool = false;
    let live_v1_o3: u16 = ALL & zb_holds(n1703);
    let ok_v2_o0: u16 = ALL & zb_holds(n1472);
    let bd_v2_o0: bool = false;
    let live_v2_o0: u16 = ALL & zb_holds(n57) & zb_holds(n1894) & zb_holds(n1895);
    let ok_v2_o1: u16 = ALL & zb_holds(n1651);
    let bd_v2_o1: bool = false;
    let live_v2_o1: u16 = ALL & zb_holds(n1919);
    let ok_v2_o2: u16 = ALL & zb_holds(n1902);
    let bd_v2_o2: bool = false;
    let live_v2_o2: u16 = ALL & zb_holds(n57) & zb_holds(n1901);
    let ok_v2_o3: u16 = ALL & zb_holds(n1678);
    let bd_v2_o3: bool = false;
    let live_v2_o3: u16 = ALL & zb_holds(n1703);
    let ok_v16_o0: u16 = ALL & zb_holds(n1472);
    let bd_v16_o0: bool = false;
    let live_v16_o0: u16 = ALL & zb_holds(n57) & zb_holds(n2020) & zb_holds(n2021);
    let ok_v16_o1: u16 = ALL & zb_holds(n1651);
    let bd_v16_o1: bool = false;
    let live_v16_o1: u16 = ALL & zb_holds(n2046);
    let ok_v16_o2: u16 = ALL & zb_holds(n2028);
    let bd_v16_o2: bool = false;
    let live_v16_o2: u16 = ALL & zb_holds(n57) & zb_holds(n2027);
    let ok_v16_o3: u16 = ALL & zb_holds(n1678);
    let bd_v16_o3: bool = false;
    let live_v16_o3: u16 = ALL & zb_holds(n1703);
    let ok_v17_o0: u16 = ALL & zb_holds(n1472);
    let bd_v17_o0: bool = false;
    let live_v17_o0: u16 = ALL & zb_holds(n57) & zb_holds(n2140) & zb_holds(n2141);
    let ok_v17_o1: u16 = ALL & zb_holds(n1651);
    let bd_v17_o1: bool = false;
    let live_v17_o1: u16 = ALL & zb_holds(n2165);
    let ok_v17_o2: u16 = ALL & zb_holds(n2148);
    let bd_v17_o2: bool = false;
    let live_v17_o2: u16 = ALL & zb_holds(n57) & zb_holds(n2147);
    let ok_v17_o3: u16 = ALL & zb_holds(n1678);
    let bd_v17_o3: bool = false;
    let live_v17_o3: u16 = ALL & zb_holds(n1703);
    let ok_v18_o0: u16 = ALL & zb_holds(n1472);
    let bd_v18_o0: bool = false;
    let live_v18_o0: u16 = ALL & zb_holds(n57) & zb_holds(n2261) & zb_holds(n2262);
    let ok_v18_o1: u16 = ALL & zb_holds(n1651);
    let bd_v18_o1: bool = false;
    let live_v18_o1: u16 = ALL & zb_holds(n2286);
    let ok_v18_o2: u16 = ALL & zb_holds(n2269);
    let bd_v18_o2: bool = false;
    let live_v18_o2: u16 = ALL & zb_holds(n57) & zb_holds(n2268);
    let ok_v18_o3: u16 = ALL & zb_holds(n1678);
    let bd_v18_o3: bool = false;
    let live_v18_o3: u16 = ALL & zb_holds(n1703);
    let ok_v32_o0: u16 = ALL & zb_holds(n1472);
    let bd_v32_o0: bool = false;
    let live_v32_o0: u16 = ALL & zb_holds(n2468);
    let ok_v32_o1: u16 = ALL & zb_holds(n1651);
    let bd_v32_o1: bool = false;
    let live_v32_o1: u16 = ALL & zb_holds(n2471);
    let ok_v32_o2: u16 = ALL & zb_holds(n2432);
    let bd_v32_o2: bool = false;
    let live_v32_o2: u16 = ALL & zb_holds(n2472);
    let ok_v32_o3: u16 = ALL & zb_holds(n1678);
    let bd_v32_o3: bool = false;
    let live_v32_o3: u16 = ALL & zb_holds(n1703);
    let ok_v33_o0: u16 = ALL & zb_holds(n1472);
    let bd_v33_o0: bool = false;
    let live_v33_o0: u16 = ALL & zb_holds(n2630);
    let ok_v33_o1: u16 = ALL & zb_holds(n1651);
    let bd_v33_o1: bool = false;
    let live_v33_o1: u16 = ALL & zb_holds(n2633);
    let ok_v33_o2: u16 = ALL & zb_holds(n2594);
    let bd_v33_o2: bool = false;
    let live_v33_o2: u16 = ALL & zb_holds(n2634);
    let ok_v33_o3: u16 = ALL & zb_holds(n1678);
    let bd_v33_o3: bool = false;
    let live_v33_o3: u16 = ALL & zb_holds(n1703);
    let ok_v34_o0: u16 = ALL & zb_holds(n1472);
    let bd_v34_o0: bool = false;
    let live_v34_o0: u16 = ALL & zb_holds(n2816);
    let ok_v34_o1: u16 = ALL & zb_holds(n1651);
    let bd_v34_o1: bool = false;
    let live_v34_o1: u16 = ALL & zb_holds(n2819);
    let ok_v34_o2: u16 = ALL & zb_holds(n2780);
    let bd_v34_o2: bool = false;
    let live_v34_o2: u16 = ALL & zb_holds(n2820);
    let ok_v34_o3: u16 = ALL & zb_holds(n1678);
    let bd_v34_o3: bool = false;
    let live_v34_o3: u16 = ALL & zb_holds(n1703);
    let ok_v36_o0: u16 = ALL & zb_holds(n1472);
    let bd_v36_o0: bool = false;
    let live_v36_o0: u16 = ALL & zb_holds(n3008);
    let ok_v36_o1: u16 = ALL & zb_holds(n1651);
    let bd_v36_o1: bool = false;
    let live_v36_o1: u16 = ALL & zb_holds(n3011);
    let ok_v36_o2: u16 = ALL & zb_holds(n2972);
    let bd_v36_o2: bool = false;
    let live_v36_o2: u16 = ALL & zb_holds(n3012);
    let ok_v36_o3: u16 = ALL & zb_holds(n1678);
    let bd_v36_o3: bool = false;
    let live_v36_o3: u16 = ALL & zb_holds(n1703);
    let ok_v37_o0: u16 = ALL & zb_holds(n1472);
    let bd_v37_o0: bool = false;
    let live_v37_o0: u16 = ALL & zb_holds(n3180);
    let ok_v37_o1: u16 = ALL & zb_holds(n1651);
    let bd_v37_o1: bool = false;
    let live_v37_o1: u16 = ALL & zb_holds(n3183);
    let ok_v37_o2: u16 = ALL & zb_holds(n3144);
    let bd_v37_o2: bool = false;
    let live_v37_o2: u16 = ALL & zb_holds(n3184);
    let ok_v37_o3: u16 = ALL & zb_holds(n1678);
    let bd_v37_o3: bool = false;
    let live_v37_o3: u16 = ALL & zb_holds(n1703);
    let ok_v38_o0: u16 = ALL & zb_holds(n1472);
    let bd_v38_o0: bool = false;
    let live_v38_o0: u16 = ALL & zb_holds(n3372);
    let ok_v38_o1: u16 = ALL & zb_holds(n1651);
    let bd_v38_o1: bool = false;
    let live_v38_o1: u16 = ALL & zb_holds(n3375);
    let ok_v38_o2: u16 = ALL & zb_holds(n3336);
    let bd_v38_o2: bool = false;
    let live_v38_o2: u16 = ALL & zb_holds(n3376);
    let ok_v38_o3: u16 = ALL & zb_holds(n1678);
    let bd_v38_o3: bool = false;
    let live_v38_o3: u16 = ALL & zb_holds(n1703);
    let ok_v40_o0: u16 = ALL & zb_holds(n1472);
    let bd_v40_o0: bool = false;
    let live_v40_o0: u16 = ALL & zb_holds(n3516);
    let ok_v40_o1: u16 = ALL & zb_holds(n1651);
    let bd_v40_o1: bool = false;
    let live_v40_o1: u16 = ALL & zb_holds(n3519);
    let ok_v40_o2: u16 = ALL & zb_holds(n3480);
    let bd_v40_o2: bool = false;
    let live_v40_o2: u16 = ALL & zb_holds(n3520);
    let ok_v40_o3: u16 = ALL & zb_holds(n1678);
    let bd_v40_o3: bool = false;
    let live_v40_o3: u16 = ALL & zb_holds(n1703);
    let ok_v41_o0: u16 = ALL & zb_holds(n1472);
    let bd_v41_o0: bool = false;
    let live_v41_o0: u16 = ALL & zb_holds(n3654);
    let ok_v41_o1: u16 = ALL & zb_holds(n1651);
    let bd_v41_o1: bool = false;
    let live_v41_o1: u16 = ALL & zb_holds(n3657);
    let ok_v41_o2: u16 = ALL & zb_holds(n3618);
    let bd_v41_o2: bool = false;
    let live_v41_o2: u16 = ALL & zb_holds(n3658);
    let ok_v41_o3: u16 = ALL & zb_holds(n1678);
    let bd_v41_o3: bool = false;
    let live_v41_o3: u16 = ALL & zb_holds(n1703);
    let ok_v42_o0: u16 = ALL & zb_holds(n1472);
    let bd_v42_o0: bool = false;
    let live_v42_o0: u16 = ALL & zb_holds(n3798);
    let ok_v42_o1: u16 = ALL & zb_holds(n1651);
    let bd_v42_o1: bool = false;
    let live_v42_o1: u16 = ALL & zb_holds(n3801);
    let ok_v42_o2: u16 = ALL & zb_holds(n3762);
    let bd_v42_o2: bool = false;
    let live_v42_o2: u16 = ALL & zb_holds(n3802);
    let ok_v42_o3: u16 = ALL & zb_holds(n1678);
    let bd_v42_o3: bool = false;
    let live_v42_o3: u16 = ALL & zb_holds(n1703);
    let ok_v48_o0: u16 = ALL & zb_holds(n1472);
    let bd_v48_o0: bool = false;
    let live_v48_o0: u16 = ALL & zb_holds(n3984);
    let ok_v48_o1: u16 = ALL & zb_holds(n1651);
    let bd_v48_o1: bool = false;
    let live_v48_o1: u16 = ALL & zb_holds(n3987);
    let ok_v48_o2: u16 = ALL & zb_holds(n3948);
    let bd_v48_o2: bool = false;
    let live_v48_o2: u16 = ALL & zb_holds(n3988);
    let ok_v48_o3: u16 = ALL & zb_holds(n1678);
    let bd_v48_o3: bool = false;
    let live_v48_o3: u16 = ALL & zb_holds(n1703);
    let ok_v49_o0: u16 = ALL & zb_holds(n1472);
    let bd_v49_o0: bool = false;
    let live_v49_o0: u16 = ALL & zb_holds(n4146);
    let ok_v49_o1: u16 = ALL & zb_holds(n1651);
    let bd_v49_o1: bool = false;
    let live_v49_o1: u16 = ALL & zb_holds(n4149);
    let ok_v49_o2: u16 = ALL & zb_holds(n4110);
    let bd_v49_o2: bool = false;
    let live_v49_o2: u16 = ALL & zb_holds(n4150);
    let ok_v49_o3: u16 = ALL & zb_holds(n1678);
    let bd_v49_o3: bool = false;
    let live_v49_o3: u16 = ALL & zb_holds(n1703);
    let ok_v50_o0: u16 = ALL & zb_holds(n1472);
    let bd_v50_o0: bool = false;
    let live_v50_o0: u16 = ALL & zb_holds(n4332);
    let ok_v50_o1: u16 = ALL & zb_holds(n1651);
    let bd_v50_o1: bool = false;
    let live_v50_o1: u16 = ALL & zb_holds(n4335);
    let ok_v50_o2: u16 = ALL & zb_holds(n4296);
    let bd_v50_o2: bool = false;
    let live_v50_o2: u16 = ALL & zb_holds(n4336);
    let ok_v50_o3: u16 = ALL & zb_holds(n1678);
    let bd_v50_o3: bool = false;
    let live_v50_o3: u16 = ALL & zb_holds(n1703);
    let ok_v52_o0: u16 = ALL & zb_holds(n1472);
    let bd_v52_o0: bool = false;
    let live_v52_o0: u16 = ALL & zb_holds(n4524);
    let ok_v52_o1: u16 = ALL & zb_holds(n1651);
    let bd_v52_o1: bool = false;
    let live_v52_o1: u16 = ALL & zb_holds(n4527);
    let ok_v52_o2: u16 = ALL & zb_holds(n4488);
    let bd_v52_o2: bool = false;
    let live_v52_o2: u16 = ALL & zb_holds(n4528);
    let ok_v52_o3: u16 = ALL & zb_holds(n1678);
    let bd_v52_o3: bool = false;
    let live_v52_o3: u16 = ALL & zb_holds(n1703);
    let ok_v53_o0: u16 = ALL & zb_holds(n1472);
    let bd_v53_o0: bool = false;
    let live_v53_o0: u16 = ALL & zb_holds(n4696);
    let ok_v53_o1: u16 = ALL & zb_holds(n1651);
    let bd_v53_o1: bool = false;
    let live_v53_o1: u16 = ALL & zb_holds(n4699);
    let ok_v53_o2: u16 = ALL & zb_holds(n4660);
    let bd_v53_o2: bool = false;
    let live_v53_o2: u16 = ALL & zb_holds(n4700);
    let ok_v53_o3: u16 = ALL & zb_holds(n1678);
    let bd_v53_o3: bool = false;
    let live_v53_o3: u16 = ALL & zb_holds(n1703);
    let ok_v54_o0: u16 = ALL & zb_holds(n1472);
    let bd_v54_o0: bool = false;
    let live_v54_o0: u16 = ALL & zb_holds(n4888);
    let ok_v54_o1: u16 = ALL & zb_holds(n1651);
    let bd_v54_o1: bool = false;
    let live_v54_o1: u16 = ALL & zb_holds(n4891);
    let ok_v54_o2: u16 = ALL & zb_holds(n4852);
    let bd_v54_o2: bool = false;
    let live_v54_o2: u16 = ALL & zb_holds(n4892);
    let ok_v54_o3: u16 = ALL & zb_holds(n1678);
    let bd_v54_o3: bool = false;
    let live_v54_o3: u16 = ALL & zb_holds(n1703);
    let ok_v56_o0: u16 = ALL & zb_holds(n1472);
    let bd_v56_o0: bool = false;
    let live_v56_o0: u16 = ALL & zb_holds(n5032);
    let ok_v56_o1: u16 = ALL & zb_holds(n1651);
    let bd_v56_o1: bool = false;
    let live_v56_o1: u16 = ALL & zb_holds(n5035);
    let ok_v56_o2: u16 = ALL & zb_holds(n4996);
    let bd_v56_o2: bool = false;
    let live_v56_o2: u16 = ALL & zb_holds(n5036);
    let ok_v56_o3: u16 = ALL & zb_holds(n1678);
    let bd_v56_o3: bool = false;
    let live_v56_o3: u16 = ALL & zb_holds(n1703);
    let ok_v57_o0: u16 = ALL & zb_holds(n1472);
    let bd_v57_o0: bool = false;
    let live_v57_o0: u16 = ALL & zb_holds(n5170);
    let ok_v57_o1: u16 = ALL & zb_holds(n1651);
    let bd_v57_o1: bool = false;
    let live_v57_o1: u16 = ALL & zb_holds(n5173);
    let ok_v57_o2: u16 = ALL & zb_holds(n5134);
    let bd_v57_o2: bool = false;
    let live_v57_o2: u16 = ALL & zb_holds(n5174);
    let ok_v57_o3: u16 = ALL & zb_holds(n1678);
    let bd_v57_o3: bool = false;
    let live_v57_o3: u16 = ALL & zb_holds(n1703);
    let ok_v58_o0: u16 = ALL & zb_holds(n1472);
    let bd_v58_o0: bool = false;
    let live_v58_o0: u16 = ALL & zb_holds(n5314);
    let ok_v58_o1: u16 = ALL & zb_holds(n1651);
    let bd_v58_o1: bool = false;
    let live_v58_o1: u16 = ALL & zb_holds(n5317);
    let ok_v58_o2: u16 = ALL & zb_holds(n5278);
    let bd_v58_o2: bool = false;
    let live_v58_o2: u16 = ALL & zb_holds(n5318);
    let ok_v58_o3: u16 = ALL & zb_holds(n1678);
    let bd_v58_o3: bool = false;
    let live_v58_o3: u16 = ALL & zb_holds(n1703);
    let sh0 = KShared0 {
        c42: n94,
        c88: r_c88,
        c43: r_c43,
    };
    let sh1 = KShared1 {
        c39: n92,
        c42: n94,
        c88: r_c88,
        c254: n654,
        c43: r_c43,
        c38: n95,
    };
    let sh2 = KShared2 {
        c88: r_c88,
        c43: r_c43,
    };
    let sh3 = KShared3 {
        c39: n1653,
        c20: n1654,
        c41: n1655,
        c42: n1656,
        c88: r_c88,
        c232: n1658,
        c233: n1659,
        c262: n1665,
        c263: n1666,
        c264: n1667,
        c265: n1668,
        c266: n1669,
        c267: n1670,
        c268: n1671,
        c269: n1672,
        c242: n1660,
        c270: n1673,
        c271: n1674,
        c244: n1661,
        c245: n1662,
        c272: n1675,
        c273: n1676,
        c248: n1663,
        c249: n1664,
        c43: r_c43,
        c38: n1657,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
    let mut take_0_2: u16 = 0;
    let mut take_0_3: u16 = 0;
    let mut take_0_4: u16 = 0;
    let mut take_0_5: u16 = 0;
    let mut take_0_6: u16 = 0;
    let mut take_0_7: u16 = 0;
    let mut take_0_8: u16 = 0;
    let mut take_0_9: u16 = 0;
    let mut take_0_10: u16 = 0;
    let mut take_0_11: u16 = 0;
    let mut take_0_12: u16 = 0;
    let mut take_0_13: u16 = 0;
    let mut take_0_14: u16 = 0;
    let mut take_0_15: u16 = 0;
    let mut take_0_16: u16 = 0;
    let mut take_0_17: u16 = 0;
    let mut take_0_18: u16 = 0;
    let mut take_1_0: u16 = 0;
    let mut take_1_1: u16 = 0;
    let mut take_1_2: u16 = 0;
    let mut take_1_3: u16 = 0;
    let mut take_1_4: u16 = 0;
    let mut take_1_5: u16 = 0;
    let mut take_1_6: u16 = 0;
    let mut take_1_7: u16 = 0;
    let mut take_1_8: u16 = 0;
    let mut take_1_9: u16 = 0;
    let mut take_1_10: u16 = 0;
    let mut take_1_11: u16 = 0;
    let mut take_1_12: u16 = 0;
    let mut take_1_13: u16 = 0;
    let mut take_1_14: u16 = 0;
    let mut take_1_15: u16 = 0;
    let mut take_1_16: u16 = 0;
    let mut take_1_17: u16 = 0;
    let mut take_1_18: u16 = 0;
    let mut take_1_19: u16 = 0;
    let mut take_1_20: u16 = 0;
    let mut take_1_21: u16 = 0;
    let mut take_1_22: u16 = 0;
    let mut take_1_23: u16 = 0;
    let mut take_2_0: u16 = 0;
    let mut take_2_1: u16 = 0;
    let mut take_2_2: u16 = 0;
    let mut take_2_3: u16 = 0;
    let mut take_2_4: u16 = 0;
    let mut take_2_5: u16 = 0;
    let mut take_2_6: u16 = 0;
    let mut take_2_7: u16 = 0;
    let mut take_2_8: u16 = 0;
    let mut take_2_9: u16 = 0;
    let mut take_2_10: u16 = 0;
    let mut take_2_11: u16 = 0;
    let mut take_2_12: u16 = 0;
    let mut take_2_13: u16 = 0;
    let mut take_2_14: u16 = 0;
    let mut take_2_15: u16 = 0;
    let mut take_2_16: u16 = 0;
    let mut take_2_17: u16 = 0;
    let mut take_2_18: u16 = 0;
    let mut take_2_19: u16 = 0;
    let mut take_2_20: u16 = 0;
    let mut take_2_21: u16 = 0;
    let mut take_2_22: u16 = 0;
    let mut take_2_23: u16 = 0;
    let mut take_3_0: u16 = 0;
    // 24 distinct button assignments; per outcome they fall
    // into [19, 24, 24, 1] groups that write identical values.
    declined |= live_v0_o0 & !ok_v0_o0;
    take_0_0 |= live_v0_o0 & ok_v0_o0;
    declined |= live_v0_o1 & !ok_v0_o1;
    take_1_0 |= live_v0_o1 & ok_v0_o1;
    let o1 = KOut1 {
        c20: r_c20,
        c41: n93,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n1647,
        c239: n1645,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1696,
        c281: n1649,
        c253: n1695,
        h1: n5497, h2: n5498,
    };
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v0_o2 & !ok_v0_o2;
    take_2_0 |= live_v0_o2 & ok_v0_o2;
    let o2 = KOut2 {
        c39: n1640,
        c20: r_c20,
        c38: n1642,
        h1: n6420, h2: n6421,
    };
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v0_o3 & !ok_v0_o3;
    take_3_0 |= live_v0_o3 & ok_v0_o3;
    declined |= live_v1_o0 & !ok_v1_o0;
    take_0_0 |= live_v1_o0 & ok_v1_o0;
    declined |= live_v1_o1 & !ok_v1_o1;
    take_1_1 |= live_v1_o1 & ok_v1_o1;
    let o1 = KOut1 {
        c20: r_c20,
        c41: n93,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n1780,
        c239: n1645,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1795,
        c281: n1782,
        c253: n1794,
        h1: n5517, h2: n5518,
    };
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v1_o2 & !ok_v1_o2;
    take_2_1 |= live_v1_o2 & ok_v1_o2;
    let o2 = KOut2 {
        c39: n1775,
        c20: r_c20,
        c38: n1777,
        h1: n6426, h2: n6427,
    };
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v1_o3 & !ok_v1_o3;
    take_3_0 |= live_v1_o3 & ok_v1_o3;
    declined |= live_v2_o0 & !ok_v2_o0;
    take_0_0 |= live_v2_o0 & ok_v2_o0;
    declined |= live_v2_o1 & !ok_v2_o1;
    take_1_2 |= live_v2_o1 & ok_v2_o1;
    let o1 = KOut1 {
        c20: r_c20,
        c41: n93,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n1903,
        c239: n1645,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1918,
        c281: n1905,
        c253: n1917,
        h1: n5537, h2: n5538,
    };
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v2_o2 & !ok_v2_o2;
    take_2_2 |= live_v2_o2 & ok_v2_o2;
    let o2 = KOut2 {
        c39: n1898,
        c20: r_c20,
        c38: n1900,
        h1: n6432, h2: n6433,
    };
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v2_o3 & !ok_v2_o3;
    take_3_0 |= live_v2_o3 & ok_v2_o3;
    declined |= live_v16_o0 & !ok_v16_o0;
    take_0_0 |= live_v16_o0 & ok_v16_o0;
    declined |= live_v16_o1 & !ok_v16_o1;
    take_1_3 |= live_v16_o1 & ok_v16_o1;
    let o1 = KOut1 {
        c20: r_c20,
        c41: n93,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n1647,
        c239: n2029,
        c246: zb_splat(false),
        c247: n2030,
        c280: n2045,
        c281: n2032,
        c253: n2044,
        h1: n5564, h2: n5565,
    };
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v16_o2 & !ok_v16_o2;
    take_2_3 |= live_v16_o2 & ok_v16_o2;
    let o2 = KOut2 {
        c39: n2024,
        c20: r_c20,
        c38: n2026,
        h1: n6438, h2: n6439,
    };
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v16_o3 & !ok_v16_o3;
    take_3_0 |= live_v16_o3 & ok_v16_o3;
    declined |= live_v17_o0 & !ok_v17_o0;
    take_0_0 |= live_v17_o0 & ok_v17_o0;
    declined |= live_v17_o1 & !ok_v17_o1;
    take_1_4 |= live_v17_o1 & ok_v17_o1;
    let o1 = KOut1 {
        c20: r_c20,
        c41: n93,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n1780,
        c239: n2149,
        c246: zb_splat(false),
        c247: n2030,
        c280: n2164,
        c281: n2151,
        c253: n2163,
        h1: n5590, h2: n5591,
    };
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v17_o2 & !ok_v17_o2;
    take_2_4 |= live_v17_o2 & ok_v17_o2;
    let o2 = KOut2 {
        c39: n2144,
        c20: r_c20,
        c38: n2146,
        h1: n6444, h2: n6445,
    };
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v17_o3 & !ok_v17_o3;
    take_3_0 |= live_v17_o3 & ok_v17_o3;
    declined |= live_v18_o0 & !ok_v18_o0;
    take_0_0 |= live_v18_o0 & ok_v18_o0;
    let o0 = KOut0 {
        c20: r_c20,
        c41: n93,
        h1: n5335, h2: n5336,
    };
    sink.o0(18, take_0_0, &sh0, &o0);
    declined |= live_v18_o1 & !ok_v18_o1;
    take_1_5 |= live_v18_o1 & ok_v18_o1;
    let o1 = KOut1 {
        c20: r_c20,
        c41: n93,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n1903,
        c239: n2270,
        c246: zb_splat(false),
        c247: n2030,
        c280: n2285,
        c281: n2272,
        c253: n2284,
        h1: n5616, h2: n5617,
    };
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v18_o2 & !ok_v18_o2;
    take_2_5 |= live_v18_o2 & ok_v18_o2;
    let o2 = KOut2 {
        c39: n2265,
        c20: r_c20,
        c38: n2267,
        h1: n6450, h2: n6451,
    };
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v18_o3 & !ok_v18_o3;
    take_3_0 |= live_v18_o3 & ok_v18_o3;
    declined |= live_v32_o0 & !ok_v32_o0;
    take_0_1 |= live_v32_o0 & ok_v32_o0;
    let o0 = KOut0 {
        c20: n2412,
        c41: n2413,
        h1: n5341, h2: n5342,
    };
    sink.o0(32, take_0_1, &sh0, &o0);
    declined |= live_v32_o1 & !ok_v32_o1;
    take_1_6 |= live_v32_o1 & ok_v32_o1;
    let o1 = KOut1 {
        c20: n2433,
        c41: n2434,
        c268: n2438,
        c269: n2439,
        c234: n2473,
        c270: n2440,
        c271: n2441,
        c236: n2436,
        c237: n2437,
        c272: n1647,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n2470,
        c281: n2443,
        c253: n2469,
        h1: n5660, h2: n5661,
    };
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v32_o2 & !ok_v32_o2;
    take_2_6 |= live_v32_o2 & ok_v32_o2;
    let o2 = KOut2 {
        c39: n2427,
        c20: n2428,
        c38: n2430,
        h1: n6459, h2: n6460,
    };
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v32_o3 & !ok_v32_o3;
    take_3_0 |= live_v32_o3 & ok_v32_o3;
    declined |= live_v33_o0 & !ok_v33_o0;
    take_0_2 |= live_v33_o0 & ok_v33_o0;
    let o0 = KOut0 {
        c20: n2574,
        c41: n2575,
        h1: n5347, h2: n5348,
    };
    sink.o0(33, take_0_2, &sh0, &o0);
    declined |= live_v33_o1 & !ok_v33_o1;
    take_1_7 |= live_v33_o1 & ok_v33_o1;
    let o1 = KOut1 {
        c20: n2595,
        c41: n2596,
        c268: n2600,
        c269: n2601,
        c234: n2635,
        c270: n2602,
        c271: n2603,
        c236: n2598,
        c237: n2599,
        c272: n1780,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n2632,
        c281: n2605,
        c253: n2631,
        h1: n5704, h2: n5705,
    };
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v33_o2 & !ok_v33_o2;
    take_2_7 |= live_v33_o2 & ok_v33_o2;
    let o2 = KOut2 {
        c39: n2589,
        c20: n2590,
        c38: n2592,
        h1: n6468, h2: n6469,
    };
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v33_o3 & !ok_v33_o3;
    take_3_0 |= live_v33_o3 & ok_v33_o3;
    declined |= live_v34_o0 & !ok_v34_o0;
    take_0_3 |= live_v34_o0 & ok_v34_o0;
    let o0 = KOut0 {
        c20: n2760,
        c41: n2761,
        h1: n5353, h2: n5354,
    };
    sink.o0(34, take_0_3, &sh0, &o0);
    declined |= live_v34_o1 & !ok_v34_o1;
    take_1_8 |= live_v34_o1 & ok_v34_o1;
    let o1 = KOut1 {
        c20: n2781,
        c41: n2782,
        c268: n2786,
        c269: n2787,
        c234: n2821,
        c270: n2788,
        c271: n2789,
        c236: n2784,
        c237: n2785,
        c272: n1903,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n2818,
        c281: n2791,
        c253: n2817,
        h1: n5748, h2: n5749,
    };
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v34_o2 & !ok_v34_o2;
    take_2_8 |= live_v34_o2 & ok_v34_o2;
    let o2 = KOut2 {
        c39: n2775,
        c20: n2776,
        c38: n2778,
        h1: n6477, h2: n6478,
    };
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v34_o3 & !ok_v34_o3;
    take_3_0 |= live_v34_o3 & ok_v34_o3;
    declined |= live_v36_o0 & !ok_v36_o0;
    take_0_4 |= live_v36_o0 & ok_v36_o0;
    let o0 = KOut0 {
        c20: n2952,
        c41: n2953,
        h1: n5359, h2: n5360,
    };
    sink.o0(36, take_0_4, &sh0, &o0);
    declined |= live_v36_o1 & !ok_v36_o1;
    take_1_9 |= live_v36_o1 & ok_v36_o1;
    let o1 = KOut1 {
        c20: n2973,
        c41: n2974,
        c268: n2978,
        c269: n2979,
        c234: n3013,
        c270: n2980,
        c271: n2981,
        c236: n2976,
        c237: n2977,
        c272: n1647,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n3010,
        c281: n2983,
        c253: n3009,
        h1: n5792, h2: n5793,
    };
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v36_o2 & !ok_v36_o2;
    take_2_9 |= live_v36_o2 & ok_v36_o2;
    let o2 = KOut2 {
        c39: n2967,
        c20: n2968,
        c38: n2970,
        h1: n6486, h2: n6487,
    };
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v36_o3 & !ok_v36_o3;
    take_3_0 |= live_v36_o3 & ok_v36_o3;
    declined |= live_v37_o0 & !ok_v37_o0;
    take_0_5 |= live_v37_o0 & ok_v37_o0;
    let o0 = KOut0 {
        c20: n3124,
        c41: n3125,
        h1: n5365, h2: n5366,
    };
    sink.o0(37, take_0_5, &sh0, &o0);
    declined |= live_v37_o1 & !ok_v37_o1;
    take_1_10 |= live_v37_o1 & ok_v37_o1;
    let o1 = KOut1 {
        c20: n3145,
        c41: n3146,
        c268: n3150,
        c269: n3151,
        c234: n3185,
        c270: n3152,
        c271: n3153,
        c236: n3148,
        c237: n3149,
        c272: n1780,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n3182,
        c281: n3155,
        c253: n3181,
        h1: n5836, h2: n5837,
    };
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v37_o2 & !ok_v37_o2;
    take_2_10 |= live_v37_o2 & ok_v37_o2;
    let o2 = KOut2 {
        c39: n3139,
        c20: n3140,
        c38: n3142,
        h1: n6495, h2: n6496,
    };
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v37_o3 & !ok_v37_o3;
    take_3_0 |= live_v37_o3 & ok_v37_o3;
    declined |= live_v38_o0 & !ok_v38_o0;
    take_0_6 |= live_v38_o0 & ok_v38_o0;
    let o0 = KOut0 {
        c20: n3316,
        c41: n3317,
        h1: n5371, h2: n5372,
    };
    sink.o0(38, take_0_6, &sh0, &o0);
    declined |= live_v38_o1 & !ok_v38_o1;
    take_1_11 |= live_v38_o1 & ok_v38_o1;
    let o1 = KOut1 {
        c20: n3337,
        c41: n3338,
        c268: n3342,
        c269: n3343,
        c234: n3377,
        c270: n3344,
        c271: n3345,
        c236: n3340,
        c237: n3341,
        c272: n1903,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n3374,
        c281: n3347,
        c253: n3373,
        h1: n5880, h2: n5881,
    };
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v38_o2 & !ok_v38_o2;
    take_2_11 |= live_v38_o2 & ok_v38_o2;
    let o2 = KOut2 {
        c39: n3331,
        c20: n3332,
        c38: n3334,
        h1: n6504, h2: n6505,
    };
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v38_o3 & !ok_v38_o3;
    take_3_0 |= live_v38_o3 & ok_v38_o3;
    declined |= live_v40_o0 & !ok_v40_o0;
    take_0_7 |= live_v40_o0 & ok_v40_o0;
    let o0 = KOut0 {
        c20: n3460,
        c41: n3461,
        h1: n5377, h2: n5378,
    };
    sink.o0(40, take_0_7, &sh0, &o0);
    declined |= live_v40_o1 & !ok_v40_o1;
    take_1_12 |= live_v40_o1 & ok_v40_o1;
    let o1 = KOut1 {
        c20: n3481,
        c41: n3482,
        c268: n3486,
        c269: n3487,
        c234: n3521,
        c270: n3488,
        c271: n3489,
        c236: n3484,
        c237: n3485,
        c272: n1647,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n3518,
        c281: n3491,
        c253: n3517,
        h1: n5924, h2: n5925,
    };
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v40_o2 & !ok_v40_o2;
    take_2_12 |= live_v40_o2 & ok_v40_o2;
    let o2 = KOut2 {
        c39: n3475,
        c20: n3476,
        c38: n3478,
        h1: n6513, h2: n6514,
    };
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v40_o3 & !ok_v40_o3;
    take_3_0 |= live_v40_o3 & ok_v40_o3;
    declined |= live_v41_o0 & !ok_v41_o0;
    take_0_8 |= live_v41_o0 & ok_v41_o0;
    let o0 = KOut0 {
        c20: n3598,
        c41: n3599,
        h1: n5383, h2: n5384,
    };
    sink.o0(41, take_0_8, &sh0, &o0);
    declined |= live_v41_o1 & !ok_v41_o1;
    take_1_13 |= live_v41_o1 & ok_v41_o1;
    let o1 = KOut1 {
        c20: n3619,
        c41: n3620,
        c268: n3624,
        c269: n3625,
        c234: n3659,
        c270: n3626,
        c271: n3627,
        c236: n3622,
        c237: n3623,
        c272: n1780,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n3656,
        c281: n3629,
        c253: n3655,
        h1: n5968, h2: n5969,
    };
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v41_o2 & !ok_v41_o2;
    take_2_13 |= live_v41_o2 & ok_v41_o2;
    let o2 = KOut2 {
        c39: n3613,
        c20: n3614,
        c38: n3616,
        h1: n6522, h2: n6523,
    };
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v41_o3 & !ok_v41_o3;
    take_3_0 |= live_v41_o3 & ok_v41_o3;
    declined |= live_v42_o0 & !ok_v42_o0;
    take_0_9 |= live_v42_o0 & ok_v42_o0;
    let o0 = KOut0 {
        c20: n3742,
        c41: n3743,
        h1: n5389, h2: n5390,
    };
    sink.o0(42, take_0_9, &sh0, &o0);
    declined |= live_v42_o1 & !ok_v42_o1;
    take_1_14 |= live_v42_o1 & ok_v42_o1;
    let o1 = KOut1 {
        c20: n3763,
        c41: n3764,
        c268: n3768,
        c269: n3769,
        c234: n3803,
        c270: n3770,
        c271: n3771,
        c236: n3766,
        c237: n3767,
        c272: n1903,
        c239: n1645,
        c246: n2030,
        c247: zb_splat(false),
        c280: n3800,
        c281: n3773,
        c253: n3799,
        h1: n6012, h2: n6013,
    };
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v42_o2 & !ok_v42_o2;
    take_2_14 |= live_v42_o2 & ok_v42_o2;
    let o2 = KOut2 {
        c39: n3757,
        c20: n3758,
        c38: n3760,
        h1: n6531, h2: n6532,
    };
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v42_o3 & !ok_v42_o3;
    take_3_0 |= live_v42_o3 & ok_v42_o3;
    declined |= live_v48_o0 & !ok_v48_o0;
    take_0_10 |= live_v48_o0 & ok_v48_o0;
    let o0 = KOut0 {
        c20: n3928,
        c41: n3929,
        h1: n5395, h2: n5396,
    };
    sink.o0(48, take_0_10, &sh0, &o0);
    declined |= live_v48_o1 & !ok_v48_o1;
    take_1_15 |= live_v48_o1 & ok_v48_o1;
    let o1 = KOut1 {
        c20: n3949,
        c41: n3950,
        c268: n3954,
        c269: n3955,
        c234: n3989,
        c270: n3956,
        c271: n3957,
        c236: n3952,
        c237: n3953,
        c272: n1647,
        c239: n2029,
        c246: n2030,
        c247: n2030,
        c280: n3986,
        c281: n3959,
        c253: n3985,
        h1: n6056, h2: n6057,
    };
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v48_o2 & !ok_v48_o2;
    take_2_15 |= live_v48_o2 & ok_v48_o2;
    let o2 = KOut2 {
        c39: n3943,
        c20: n3944,
        c38: n3946,
        h1: n6540, h2: n6541,
    };
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v48_o3 & !ok_v48_o3;
    take_3_0 |= live_v48_o3 & ok_v48_o3;
    declined |= live_v49_o0 & !ok_v49_o0;
    take_0_11 |= live_v49_o0 & ok_v49_o0;
    let o0 = KOut0 {
        c20: n4090,
        c41: n4091,
        h1: n5401, h2: n5402,
    };
    sink.o0(49, take_0_11, &sh0, &o0);
    declined |= live_v49_o1 & !ok_v49_o1;
    take_1_16 |= live_v49_o1 & ok_v49_o1;
    let o1 = KOut1 {
        c20: n4111,
        c41: n4112,
        c268: n4116,
        c269: n4117,
        c234: n4151,
        c270: n4118,
        c271: n4119,
        c236: n4114,
        c237: n4115,
        c272: n1780,
        c239: n2149,
        c246: n2030,
        c247: n2030,
        c280: n4148,
        c281: n4121,
        c253: n4147,
        h1: n6100, h2: n6101,
    };
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v49_o2 & !ok_v49_o2;
    take_2_16 |= live_v49_o2 & ok_v49_o2;
    let o2 = KOut2 {
        c39: n4105,
        c20: n4106,
        c38: n4108,
        h1: n6549, h2: n6550,
    };
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v49_o3 & !ok_v49_o3;
    take_3_0 |= live_v49_o3 & ok_v49_o3;
    declined |= live_v50_o0 & !ok_v50_o0;
    take_0_12 |= live_v50_o0 & ok_v50_o0;
    let o0 = KOut0 {
        c20: n4276,
        c41: n4277,
        h1: n5407, h2: n5408,
    };
    sink.o0(50, take_0_12, &sh0, &o0);
    declined |= live_v50_o1 & !ok_v50_o1;
    take_1_17 |= live_v50_o1 & ok_v50_o1;
    let o1 = KOut1 {
        c20: n4297,
        c41: n4298,
        c268: n4302,
        c269: n4303,
        c234: n4337,
        c270: n4304,
        c271: n4305,
        c236: n4300,
        c237: n4301,
        c272: n1903,
        c239: n2270,
        c246: n2030,
        c247: n2030,
        c280: n4334,
        c281: n4307,
        c253: n4333,
        h1: n6144, h2: n6145,
    };
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v50_o2 & !ok_v50_o2;
    take_2_17 |= live_v50_o2 & ok_v50_o2;
    let o2 = KOut2 {
        c39: n4291,
        c20: n4292,
        c38: n4294,
        h1: n6558, h2: n6559,
    };
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v50_o3 & !ok_v50_o3;
    take_3_0 |= live_v50_o3 & ok_v50_o3;
    declined |= live_v52_o0 & !ok_v52_o0;
    take_0_13 |= live_v52_o0 & ok_v52_o0;
    let o0 = KOut0 {
        c20: n4468,
        c41: n4469,
        h1: n5413, h2: n5414,
    };
    sink.o0(52, take_0_13, &sh0, &o0);
    declined |= live_v52_o1 & !ok_v52_o1;
    take_1_18 |= live_v52_o1 & ok_v52_o1;
    let o1 = KOut1 {
        c20: n4489,
        c41: n4490,
        c268: n4494,
        c269: n4495,
        c234: n4529,
        c270: n4496,
        c271: n4497,
        c236: n4492,
        c237: n4493,
        c272: n1647,
        c239: n2029,
        c246: n2030,
        c247: n2030,
        c280: n4526,
        c281: n4499,
        c253: n4525,
        h1: n6188, h2: n6189,
    };
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v52_o2 & !ok_v52_o2;
    take_2_18 |= live_v52_o2 & ok_v52_o2;
    let o2 = KOut2 {
        c39: n4483,
        c20: n4484,
        c38: n4486,
        h1: n6567, h2: n6568,
    };
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v52_o3 & !ok_v52_o3;
    take_3_0 |= live_v52_o3 & ok_v52_o3;
    declined |= live_v53_o0 & !ok_v53_o0;
    take_0_14 |= live_v53_o0 & ok_v53_o0;
    let o0 = KOut0 {
        c20: n4640,
        c41: n4641,
        h1: n5419, h2: n5420,
    };
    sink.o0(53, take_0_14, &sh0, &o0);
    declined |= live_v53_o1 & !ok_v53_o1;
    take_1_19 |= live_v53_o1 & ok_v53_o1;
    let o1 = KOut1 {
        c20: n4661,
        c41: n4662,
        c268: n4666,
        c269: n4667,
        c234: n4701,
        c270: n4668,
        c271: n4669,
        c236: n4664,
        c237: n4665,
        c272: n1780,
        c239: n2149,
        c246: n2030,
        c247: n2030,
        c280: n4698,
        c281: n4671,
        c253: n4697,
        h1: n6232, h2: n6233,
    };
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v53_o2 & !ok_v53_o2;
    take_2_19 |= live_v53_o2 & ok_v53_o2;
    let o2 = KOut2 {
        c39: n4655,
        c20: n4656,
        c38: n4658,
        h1: n6576, h2: n6577,
    };
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v53_o3 & !ok_v53_o3;
    take_3_0 |= live_v53_o3 & ok_v53_o3;
    declined |= live_v54_o0 & !ok_v54_o0;
    take_0_15 |= live_v54_o0 & ok_v54_o0;
    let o0 = KOut0 {
        c20: n4832,
        c41: n4833,
        h1: n5425, h2: n5426,
    };
    sink.o0(54, take_0_15, &sh0, &o0);
    declined |= live_v54_o1 & !ok_v54_o1;
    take_1_20 |= live_v54_o1 & ok_v54_o1;
    let o1 = KOut1 {
        c20: n4853,
        c41: n4854,
        c268: n4858,
        c269: n4859,
        c234: n4893,
        c270: n4860,
        c271: n4861,
        c236: n4856,
        c237: n4857,
        c272: n1903,
        c239: n2270,
        c246: n2030,
        c247: n2030,
        c280: n4890,
        c281: n4863,
        c253: n4889,
        h1: n6276, h2: n6277,
    };
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v54_o2 & !ok_v54_o2;
    take_2_20 |= live_v54_o2 & ok_v54_o2;
    let o2 = KOut2 {
        c39: n4847,
        c20: n4848,
        c38: n4850,
        h1: n6585, h2: n6586,
    };
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v54_o3 & !ok_v54_o3;
    take_3_0 |= live_v54_o3 & ok_v54_o3;
    declined |= live_v56_o0 & !ok_v56_o0;
    take_0_16 |= live_v56_o0 & ok_v56_o0;
    let o0 = KOut0 {
        c20: n4976,
        c41: n4977,
        h1: n5431, h2: n5432,
    };
    sink.o0(56, take_0_16, &sh0, &o0);
    declined |= live_v56_o1 & !ok_v56_o1;
    take_1_21 |= live_v56_o1 & ok_v56_o1;
    let o1 = KOut1 {
        c20: n4997,
        c41: n4998,
        c268: n5002,
        c269: n5003,
        c234: n5037,
        c270: n5004,
        c271: n5005,
        c236: n5000,
        c237: n5001,
        c272: n1647,
        c239: n2029,
        c246: n2030,
        c247: n2030,
        c280: n5034,
        c281: n5007,
        c253: n5033,
        h1: n6320, h2: n6321,
    };
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v56_o2 & !ok_v56_o2;
    take_2_21 |= live_v56_o2 & ok_v56_o2;
    let o2 = KOut2 {
        c39: n4991,
        c20: n4992,
        c38: n4994,
        h1: n6594, h2: n6595,
    };
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v56_o3 & !ok_v56_o3;
    take_3_0 |= live_v56_o3 & ok_v56_o3;
    declined |= live_v57_o0 & !ok_v57_o0;
    take_0_17 |= live_v57_o0 & ok_v57_o0;
    let o0 = KOut0 {
        c20: n5114,
        c41: n5115,
        h1: n5437, h2: n5438,
    };
    sink.o0(57, take_0_17, &sh0, &o0);
    declined |= live_v57_o1 & !ok_v57_o1;
    take_1_22 |= live_v57_o1 & ok_v57_o1;
    let o1 = KOut1 {
        c20: n5135,
        c41: n5136,
        c268: n5140,
        c269: n5141,
        c234: n5175,
        c270: n5142,
        c271: n5143,
        c236: n5138,
        c237: n5139,
        c272: n1780,
        c239: n2149,
        c246: n2030,
        c247: n2030,
        c280: n5172,
        c281: n5145,
        c253: n5171,
        h1: n6364, h2: n6365,
    };
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v57_o2 & !ok_v57_o2;
    take_2_22 |= live_v57_o2 & ok_v57_o2;
    let o2 = KOut2 {
        c39: n5129,
        c20: n5130,
        c38: n5132,
        h1: n6603, h2: n6604,
    };
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v57_o3 & !ok_v57_o3;
    take_3_0 |= live_v57_o3 & ok_v57_o3;
    declined |= live_v58_o0 & !ok_v58_o0;
    take_0_18 |= live_v58_o0 & ok_v58_o0;
    let o0 = KOut0 {
        c20: n5258,
        c41: n5259,
        h1: n5443, h2: n5444,
    };
    sink.o0(58, take_0_18, &sh0, &o0);
    declined |= live_v58_o1 & !ok_v58_o1;
    take_1_23 |= live_v58_o1 & ok_v58_o1;
    let o1 = KOut1 {
        c20: n5279,
        c41: n5280,
        c268: n5284,
        c269: n5285,
        c234: n5319,
        c270: n5286,
        c271: n5287,
        c236: n5282,
        c237: n5283,
        c272: n1903,
        c239: n2270,
        c246: n2030,
        c247: n2030,
        c280: n5316,
        c281: n5289,
        c253: n5315,
        h1: n6408, h2: n6409,
    };
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v58_o2 & !ok_v58_o2;
    take_2_23 |= live_v58_o2 & ok_v58_o2;
    let o2 = KOut2 {
        c39: n5273,
        c20: n5274,
        c38: n5276,
        h1: n6612, h2: n6613,
    };
    sink.o2(58, take_2_23, &sh2, &o2);
    declined |= live_v58_o3 & !ok_v58_o3;
    take_3_0 |= live_v58_o3 & ok_v58_o3;
    let o3 = KOut3 {
        h1: n6688, h2: n6689,
    };
    sink.o3(58, take_3_0, &sh3, &o3);
    declined
}
