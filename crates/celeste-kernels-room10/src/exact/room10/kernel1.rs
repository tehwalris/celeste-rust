// GENERATED from a TRACED frame (shape 1). Do not edit.
//
// One input shape, 3 output shapes, 28 distinct button
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
pub const SHAPE: u64 = 14202976571271913900;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c274: P8,
    pub c275: P8,
    pub c276: P8,
    pub c277: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("deaths", "num"),
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("has_dashed", "bool"),
    ("has_key", "bool"),
    ("max_djump", "num"),
    ("minutes", "num"),
    ("objects[0].collideable", "bool"),
    ("objects[0].dash_accel.x", "num"),
    ("objects[0].dash_accel.y", "num"),
    ("objects[0].dash_effect_time", "num"),
    ("objects[0].dash_target.x", "num"),
    ("objects[0].dash_target.y", "num"),
    ("objects[0].dash_time", "num"),
    ("objects[0].djump", "num"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].grace", "num"),
    ("objects[0].p_dash", "bool"),
    ("objects[0].p_jump", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("pause_player", "bool"),
    ("seconds", "num"),
    ("will_restart", "bool"),
];

/// Per-lane inputs, in `ROW_SLOTS` order.
pub struct RowsIn {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c232: u16,
    pub c268: ZN,
    pub c269: ZN,
    pub c234: ZN,
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c237: ZN,
    pub c272: u16,
    pub c273: u16,
    pub c239: ZN,
    pub c246: u16,
    pub c247: u16,
    pub c278: ZN,
    pub c279: ZN,
    pub c249: u16,
    pub c280: ZN,
    pub c281: ZN,
    pub c253: ZN,
    pub c254: ZN,
    pub c43: u16,
    pub c85: ZN,
    pub c38: u16,
}

/// Cell ids for `ROW_SLOTS` in the bound block.
pub struct RowSlots {
    pub c87: u32,
    pub c39: u32,
    pub c84: u32,
    pub c20: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c86: u32,
    pub c232: u32,
    pub c268: u32,
    pub c269: u32,
    pub c234: u32,
    pub c270: u32,
    pub c271: u32,
    pub c236: u32,
    pub c237: u32,
    pub c272: u32,
    pub c273: u32,
    pub c239: u32,
    pub c246: u32,
    pub c247: u32,
    pub c278: u32,
    pub c279: u32,
    pub c249: u32,
    pub c280: u32,
    pub c281: u32,
    pub c253: u32,
    pub c254: u32,
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
        c274: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c275: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c276: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c277: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c87: cell("deaths")?,
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c86: cell("minutes")?,
        c232: cell("objects[0].collideable")?,
        c268: cell("objects[0].dash_accel.x")?,
        c269: cell("objects[0].dash_accel.y")?,
        c234: cell("objects[0].dash_effect_time")?,
        c270: cell("objects[0].dash_target.x")?,
        c271: cell("objects[0].dash_target.y")?,
        c236: cell("objects[0].dash_time")?,
        c237: cell("objects[0].djump")?,
        c272: cell("objects[0].flip.x")?,
        c273: cell("objects[0].flip.y")?,
        c239: cell("objects[0].grace")?,
        c246: cell("objects[0].p_dash")?,
        c247: cell("objects[0].p_jump")?,
        c278: cell("objects[0].rem.x")?,
        c279: cell("objects[0].rem.y")?,
        c249: cell("objects[0].solids")?,
        c280: cell("objects[0].spd.x")?,
        c281: cell("objects[0].spd.y")?,
        c253: cell("objects[0].x")?,
        c254: cell("objects[0].y")?,
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
        c87: match &b.cols[s.c87 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
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
        c86: match &b.cols[s.c86 as usize] {
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
        c234: match &b.cols[s.c234 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c236: match &b.cols[s.c236 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c237: match &b.cols[s.c237 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
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
        c273: match &b.cols[s.c273 as usize] {
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
        c239: match &b.cols[s.c239 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c246: match &b.cols[s.c246 as usize] {
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
        c247: match &b.cols[s.c247 as usize] {
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
        c278: match &b.cols[s.c278 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c279: match &b.cols[s.c279 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c249: match &b.cols[s.c249 as usize] {
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
        c280: match &b.cols[s.c280 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c281: match &b.cols[s.c281 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c254: match &b.cols[s.c254 as usize] {
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
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
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

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
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

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c278: ZN,
    pub c279: ZN,
    pub c254: ZN,
    pub c85: ZN,
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

/// An EMPTY accumulator with outcome 0's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append0`.
pub fn acc0(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_0, OUT_GLOBALS_0, OUT_PTRS_0, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_0 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_0: u64 = 1165628102232147319;
pub const KPART2_0: u64 = 4665604857065634973;

/// Append this assignment's lanes that TAKE outcome 0 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_0.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_0.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[358] = Col::U(AV::Bool(false));
    b.cols[359] = Col::U(AV::Bool(false));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[362] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(3120073i32)));
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
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_1 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_1: u64 = 3885141205346150141;
pub const KPART2_1: u64 = 2992409829084096670;

/// Append this assignment's lanes that TAKE outcome 1 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_1.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_1.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
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
    b.cols[278] = Col::N(Vec::new());
    b.cols[279] = Col::N(Vec::new());
    b.cols[249] = Col::U(AV::Bool(true));
    b.cols[280] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[254] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_2 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_2: u64 = 4028447185332398888;
pub const KPART2_2: u64 = 9155460916912633436;

/// Append this assignment's lanes that TAKE outcome 2 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_2.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_2.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
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
        if let Col::N(v) = &mut acc.cols[278] { v.push(sh.c278.lane(i)); }
        if let Col::N(v) = &mut acc.cols[279] { v.push(sh.c279.lane(i)); }
        if let Col::N(v) = &mut acc.cols[280] { v.push(kv.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(kv.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(kv.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

pub const OUTCOMES: usize = 3;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        _ => panic!("outcome {} of 3", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        _ => panic!("outcome {} of 3", i),
    }
}

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32], skip: &'a dyn Fn((u64, u64)) -> bool }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0], self.org, self.skip);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1], self.org, self.skip);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2], self.org, self.skip);
    }
}

pub fn step(
    b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],
    skip: &dyn Fn((u64, u64)) -> bool,
) -> Option<u16> {
    let (u, s) = bind(b)?;
    let rin = rows(b, &s, lo)?;
    let g = G { cart: &b.cart, cache: &b.cache };
    // The row sets are the CALLER's and reset in O(1) here.
    // Slice-local because that is where the duplication is:
    // it comes from configurations agreeing on one lane, and
    // a lane lives in one slice.
    seen.iter_mut().for_each(|s| s.next_slice());
    // Engine-carried origin metadata for this slice's lanes
    // (empty = untracked); see `Rt2::origin`.
    let org: &[u32] = if b.origin.is_empty() { &[] } else { &b.origin[lo..lo + n] };
    let mut sink = Append { accs, seen, n, org, skip };
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
    let r_c86: ZN = rin.c86;
    let r_c87: ZN = rin.c87;
    let r_c88: ZN = rin.c88;
    let r_c232: ZB = ZB { val: rin.c232, known: ALL };
    let r_c234: ZN = rin.c234;
    let r_c236: ZN = rin.c236;
    let r_c237: ZN = rin.c237;
    let r_c239: ZN = rin.c239;
    let r_c246: ZB = ZB { val: rin.c246, known: ALL };
    let r_c247: ZB = ZB { val: rin.c247, known: ALL };
    let r_c249: ZB = ZB { val: rin.c249, known: ALL };
    let r_c253: ZN = rin.c253;
    let r_c254: ZN = rin.c254;
    let r_c268: ZN = rin.c268;
    let r_c269: ZN = rin.c269;
    let r_c270: ZN = rin.c270;
    let r_c271: ZN = rin.c271;
    let r_c272: ZB = ZB { val: rin.c272, known: ALL };
    let r_c273: ZB = ZB { val: rin.c273, known: ALL };
    let r_c278: ZN = rin.c278;
    let r_c279: ZN = rin.c279;
    let r_c280: ZN = rin.c280;
    let r_c281: ZN = rin.c281;
    let n52: ZB = zb_not(r_c42);
    let n53: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n54: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c280);
    let n56: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n57: ZN = zn_rem(n56, zn_splat(P8::from_raw(1966080i32)));
    let n58: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n57);
    let n68: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n70: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n76: ZB = zb_not(r_c273);
    let n77: bool = P8::from_raw(327680i32) == u.c274;
    let n78: bool = P8::from_raw(393216i32) == u.c275;
    let n79: bool = P8::from_raw(65536i32) == u.c276;
    let n80: bool = P8::from_raw(196608i32) == u.c277;
    let n81: ZB = zb_not(r_c43);
    let n82: ZB = zb_not(r_c38);
    let n83: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n84: ZN = zn_rem(n83, zn_splat(P8::from_raw(3932160i32)));
    let n85: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n84);
    let n86: ZN = zsel_n(n58, n84, r_c85);
    let n87: ZB = zb_not(n54);
    let n88: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c281);
    let n89: ZB = zb_not(n88);
    let n90: ZB = zb_or(n87, n89);
    let n91: ZB = zb_not(n90);
    let n92: ZN = zn_add(r_c278, r_c280);
    let n93: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n92);
    let n94: ZN = zn_flr(n93);
    let n95: ZB = zn_gt(n94, zn_splat(P8::from_raw(0i32)));
    let n96: ZB = zn_lt(n94, zn_splat(P8::from_raw(0i32)));
    let n97: ZN = zsel_n(n96, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n98: ZN = zsel_n(n95, zn_splat(P8::from_raw(65536i32)), n97);
    let n99: ZN = zn_abs(n94);
    let n100: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c254);
    let n101: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n100);
    let n102: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n99);
    let n103: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n99);
    let n104: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n99);
    let n105: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n99);
    let n106: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n99);
    let n107: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n99);
    let n108: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n99);
    let n109: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n99);
    let n110: ZN = zn_add(r_c279, r_c281);
    let n111: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n110);
    let n112: ZN = zn_flr(n111);
    let n113: ZB = zn_gt(n112, zn_splat(P8::from_raw(0i32)));
    let n114: ZB = zn_lt(n112, zn_splat(P8::from_raw(0i32)));
    let n115: ZN = zsel_n(n114, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n116: ZN = zsel_n(n113, zn_splat(P8::from_raw(65536i32)), n115);
    let n117: ZN = zn_abs(n112);
    let n118: ZN = zn_add(n100, n116);
    let n119: ZN = zn_add(r_c254, n116);
    let n120: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n117);
    let n121: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n119);
    let n122: ZN = zn_add(n116, n121);
    let n123: ZN = zn_add(n116, n119);
    let n124: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n117);
    let n125: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n123);
    let n126: ZN = zn_add(n116, n125);
    let n127: ZN = zn_add(n116, n123);
    let n128: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n117);
    let n129: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n127);
    let n130: ZN = zn_add(n116, n129);
    let n131: ZN = zn_add(n116, n127);
    let n132: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n117);
    let n133: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n131);
    let n134: ZN = zn_add(n116, n133);
    let n135: ZN = zn_add(n116, n131);
    let n136: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n117);
    let n137: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n135);
    let n138: ZN = zn_add(n116, n137);
    let n139: ZN = zn_add(n116, n135);
    let n140: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n117);
    let n141: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n139);
    let n142: ZN = zn_add(n116, n141);
    let n143: ZN = zn_add(n116, n139);
    let n144: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n117);
    let n145: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n143);
    let n146: ZN = zn_add(n116, n145);
    let n147: ZN = zn_add(n116, n143);
    let n148: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n117);
    let n149: ZB = zb_not(r_c247);
    let n150: ZB = zb_not(r_c246);
    let n151: ZB = zn_lt(r_c237, zn_splat(P8::from_raw(65536i32)));
    let n152: ZN = zsel_n(n151, zn_splat(P8::from_raw(65536i32)), r_c237);
    let n153: ZB = zn_gt(r_c239, zn_splat(P8::from_raw(0i32)));
    let n154: ZN = zn_sub(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n155: ZN = zsel_n(n153, n154, r_c239);
    let n156: ZB = zn_gt(r_c236, zn_splat(P8::from_raw(0i32)));
    let n157: ZN = zsel_n(n85, n70, r_c86);
    let n158: ZN = zsel_n(n58, n157, r_c86);
    let n159: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c253);
    let n160: ZN = zn_add(n98, n159);
    let n161: ZB = zn_tile_flag_at(g.cache, g.cart, n160, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n162: ZN = zn_add(r_c253, n98);
    let n163: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n162);
    let n164: ZN = zn_add(n98, n163);
    let n165: ZB = zn_tile_flag_at(g.cache, g.cart, n164, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n166: ZN = zn_add(n98, n162);
    let n167: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n166);
    let n168: ZN = zn_add(n98, n167);
    let n169: ZB = zn_tile_flag_at(g.cache, g.cart, n168, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n170: ZN = zn_add(n98, n166);
    let n171: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n170);
    let n172: ZN = zn_add(n98, n171);
    let n173: ZB = zn_tile_flag_at(g.cache, g.cart, n172, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n174: ZN = zn_add(n98, n170);
    let n175: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n174);
    let n176: ZN = zn_add(n98, n175);
    let n177: ZB = zn_tile_flag_at(g.cache, g.cart, n176, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n178: ZN = zn_add(n98, n174);
    let n179: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n178);
    let n180: ZN = zn_add(n98, n179);
    let n181: ZB = zn_tile_flag_at(g.cache, g.cart, n180, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n182: ZN = zn_add(n98, n178);
    let n183: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n182);
    let n184: ZN = zn_add(n98, n183);
    let n185: ZB = zn_tile_flag_at(g.cache, g.cart, n184, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n186: ZN = zn_add(n98, n182);
    let n187: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n186);
    let n188: ZN = zn_add(n98, n187);
    let n189: ZB = zn_tile_flag_at(g.cache, g.cart, n188, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n190: ZN = zn_add(n98, n186);
    let n191: ZN = zsel_n(n189, n186, n190);
    let n192: ZN = zsel_n(n189, zn_splat(P8::from_raw(0i32)), r_c280);
    let n193: ZB = zb_or(n109, n189);
    let n194: ZN = zsel_n(n108, n186, n191);
    let n195: ZN = zsel_n(n108, r_c280, n192);
    let n196: ZB = zb_or(n108, n193);
    let n197: ZN = zsel_n(n185, n182, n194);
    let n198: ZN = zsel_n(n185, zn_splat(P8::from_raw(0i32)), n195);
    let n199: ZB = zb_or(n185, n196);
    let n200: ZN = zsel_n(n107, n182, n197);
    let n201: ZN = zsel_n(n107, r_c280, n198);
    let n202: ZB = zb_or(n107, n199);
    let n203: ZN = zsel_n(n181, n178, n200);
    let n204: ZN = zsel_n(n181, zn_splat(P8::from_raw(0i32)), n201);
    let n205: ZB = zb_or(n181, n202);
    let n206: ZN = zsel_n(n106, n178, n203);
    let n207: ZN = zsel_n(n106, r_c280, n204);
    let n208: ZB = zb_or(n106, n205);
    let n209: ZN = zsel_n(n177, n174, n206);
    let n210: ZN = zsel_n(n177, zn_splat(P8::from_raw(0i32)), n207);
    let n211: ZB = zb_or(n177, n208);
    let n212: ZN = zsel_n(n105, n174, n209);
    let n213: ZN = zsel_n(n105, r_c280, n210);
    let n214: ZB = zb_or(n105, n211);
    let n215: ZN = zsel_n(n173, n170, n212);
    let n216: ZN = zsel_n(n173, zn_splat(P8::from_raw(0i32)), n213);
    let n217: ZB = zb_or(n173, n214);
    let n218: ZN = zsel_n(n104, n170, n215);
    let n219: ZN = zsel_n(n104, r_c280, n216);
    let n220: ZB = zb_or(n104, n217);
    let n221: ZN = zsel_n(n169, n166, n218);
    let n222: ZN = zsel_n(n169, zn_splat(P8::from_raw(0i32)), n219);
    let n223: ZB = zb_or(n169, n220);
    let n224: ZN = zsel_n(n103, n166, n221);
    let n225: ZN = zsel_n(n103, r_c280, n222);
    let n226: ZB = zb_or(n103, n223);
    let n227: ZN = zsel_n(n165, n162, n224);
    let n228: ZN = zsel_n(n165, zn_splat(P8::from_raw(0i32)), n225);
    let n229: ZB = zb_or(n165, n226);
    let n230: ZN = zsel_n(n102, n162, n227);
    let n231: ZN = zsel_n(n102, r_c280, n228);
    let n232: ZB = zb_or(n102, n229);
    let n233: ZN = zsel_n(n161, r_c253, n230);
    let n234: ZN = zsel_n(n161, zn_splat(P8::from_raw(0i32)), n231);
    let n235: ZB = zb_or(n161, n232);
    let n236: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n233);
    let n237: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n236);
    let n238: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n118, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n239: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n240: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n126, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n241: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n130, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n242: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n134, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n243: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n138, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n244: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n142, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n245: ZB = zn_tile_flag_at(g.cache, g.cart, n237, n146, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n246: ZB = zb_and(n148, n235);
    let n247: ZN = zsel_n(n245, n143, n147);
    let n248: ZN = zsel_n(n245, zn_splat(P8::from_raw(0i32)), r_c281);
    let n249: ZB = zsel_b(n245, n235, n246);
    let n250: ZN = zsel_n(n144, n143, n247);
    let n251: ZN = zsel_n(n144, r_c281, n248);
    let n252: ZB = zsel_b(n144, n235, n249);
    let n253: ZN = zsel_n(n244, n139, n250);
    let n254: ZN = zsel_n(n244, zn_splat(P8::from_raw(0i32)), n251);
    let n255: ZB = zsel_b(n244, n235, n252);
    let n256: ZN = zsel_n(n140, n139, n253);
    let n257: ZN = zsel_n(n140, r_c281, n254);
    let n258: ZB = zsel_b(n140, n235, n255);
    let n259: ZN = zsel_n(n243, n135, n256);
    let n260: ZN = zsel_n(n243, zn_splat(P8::from_raw(0i32)), n257);
    let n261: ZB = zsel_b(n243, n235, n258);
    let n262: ZN = zsel_n(n136, n135, n259);
    let n263: ZN = zsel_n(n136, r_c281, n260);
    let n264: ZB = zsel_b(n136, n235, n261);
    let n265: ZN = zsel_n(n242, n131, n262);
    let n266: ZN = zsel_n(n242, zn_splat(P8::from_raw(0i32)), n263);
    let n267: ZB = zsel_b(n242, n235, n264);
    let n268: ZN = zsel_n(n132, n131, n265);
    let n269: ZN = zsel_n(n132, r_c281, n266);
    let n270: ZB = zsel_b(n132, n235, n267);
    let n271: ZN = zsel_n(n241, n127, n268);
    let n272: ZN = zsel_n(n241, zn_splat(P8::from_raw(0i32)), n269);
    let n273: ZB = zsel_b(n241, n235, n270);
    let n274: ZN = zsel_n(n128, n127, n271);
    let n275: ZN = zsel_n(n128, r_c281, n272);
    let n276: ZB = zsel_b(n128, n235, n273);
    let n277: ZN = zsel_n(n240, n123, n274);
    let n278: ZN = zsel_n(n240, zn_splat(P8::from_raw(0i32)), n275);
    let n279: ZB = zsel_b(n240, n235, n276);
    let n280: ZN = zsel_n(n124, n123, n277);
    let n281: ZN = zsel_n(n124, r_c281, n278);
    let n282: ZB = zsel_b(n124, n235, n279);
    let n283: ZN = zsel_n(n239, n119, n280);
    let n284: ZN = zsel_n(n239, zn_splat(P8::from_raw(0i32)), n281);
    let n285: ZB = zsel_b(n239, n235, n282);
    let n286: ZN = zsel_n(n120, n119, n283);
    let n287: ZN = zsel_n(n120, r_c281, n284);
    let n288: ZB = zsel_b(n120, n235, n285);
    let n289: ZN = zsel_n(n238, r_c254, n286);
    let n290: ZN = zsel_n(n238, zn_splat(P8::from_raw(0i32)), n287);
    let n291: ZB = zsel_b(n238, n235, n288);
    let n292: ZN = zsel_n(n90, n233, r_c253);
    let n293: ZN = zsel_n(n90, n289, r_c254);
    let n294: ZN = zsel_n(n90, n234, r_c280);
    let n295: ZN = zsel_n(n90, n290, r_c281);
    let n296: ZB = zb_or(n91, n291);
    let n297: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n292);
    let n298: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n293);
    let n299: ZN = zn_div(n297, zn_splat(P8::from_raw(524288i32)));
    let n300: ZN = zn_flr(n299);
    let n301: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n300);
    let n302: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n297);
    let n303: ZN = zn_sub(n302, zn_splat(P8::from_raw(65536i32)));
    let n304: ZN = zn_div(n303, zn_splat(P8::from_raw(524288i32)));
    let n305: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n304);
    let n306: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n301);
    let n307: ZB = zn_le(n306, n305);
    let n308: ZB = zn_gt(n306, n305);
    let n309: ZB = zb_and(n68, n307);
    let n310: ZB = zb_and(n68, n308);
    let n311: ZN = zn_div(n298, zn_splat(P8::from_raw(524288i32)));
    let n312: ZN = zn_flr(n311);
    let n313: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n312);
    let n314: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n298);
    let n315: ZN = zn_sub(n314, zn_splat(P8::from_raw(65536i32)));
    let n316: ZN = zn_div(n315, zn_splat(P8::from_raw(524288i32)));
    let n317: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n316);
    let n318: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n313);
    let n319: ZB = zn_le(n318, n317);
    let n320: ZB = zn_gt(n318, n317);
    let n321: ZB = zb_and(n309, n319);
    let n322: ZB = zb_and(n309, n320);
    let n323: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n306);
    let n324: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n318);
    let n325: ZN = zn_mget(g.cart, n323, n324);
    let n326: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n325);
    let n327: ZN = zn_rem(n315, zn_splat(P8::from_raw(524288i32)));
    let n328: ZB = zn_ge(n327, zn_splat(P8::from_raw(393216i32)));
    let n329: ZN = zn_mul(n318, zn_splat(P8::from_raw(524288i32)));
    let n330: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n329);
    let n331: ZB = zn_eq(n314, n330);
    let n332: ZB = zb_or(n328, n331);
    let n333: ZB = zb_and(n326, n332);
    let n334: ZB = zn_ge(n295, zn_splat(P8::from_raw(0i32)));
    let n335: ZB = zb_and(n333, n334);
    let n336: ZB = zb_not(n335);
    let n337: ZB = zb_and(n321, n335);
    let n338: ZB = zb_and(n321, n336);
    let n339: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n325);
    let n340: ZN = zn_rem(n298, zn_splat(P8::from_raw(524288i32)));
    let n341: ZB = zn_le(n340, zn_splat(P8::from_raw(131072i32)));
    let n342: ZB = zb_and(n339, n341);
    let n343: ZB = zn_le(n295, zn_splat(P8::from_raw(0i32)));
    let n344: ZB = zb_and(n342, n343);
    let n345: ZB = zb_not(n344);
    let n346: ZB = zb_and(n338, n344);
    let n347: ZB = zb_and(n338, n345);
    let n348: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n325);
    let n349: ZN = zn_rem(n297, zn_splat(P8::from_raw(524288i32)));
    let n350: ZB = zn_le(n349, zn_splat(P8::from_raw(131072i32)));
    let n351: ZB = zb_and(n348, n350);
    let n352: ZB = zn_le(n294, zn_splat(P8::from_raw(0i32)));
    let n353: ZB = zb_and(n351, n352);
    let n354: ZB = zb_not(n353);
    let n355: ZB = zb_and(n347, n353);
    let n356: ZB = zb_and(n347, n354);
    let n357: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n325);
    let n358: ZN = zn_rem(n303, zn_splat(P8::from_raw(524288i32)));
    let n359: ZB = zn_ge(n358, zn_splat(P8::from_raw(393216i32)));
    let n360: ZN = zn_mul(n306, zn_splat(P8::from_raw(524288i32)));
    let n361: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n360);
    let n362: ZB = zn_eq(n302, n361);
    let n363: ZB = zb_or(n359, n362);
    let n364: ZB = zb_and(n357, n363);
    let n365: ZB = zn_ge(n294, zn_splat(P8::from_raw(0i32)));
    let n366: ZB = zb_and(n364, n365);
    let n367: ZB = zb_not(n366);
    let n368: ZB = zb_and(n356, n366);
    let n369: ZB = zb_and(n356, n367);
    let n370: ZB = zb_or(n355, n368);
    let n371: ZB = zb_or(n346, n370);
    let n372: ZB = zb_or(n337, n371);
    let n373: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n313);
    let n374: ZB = zn_le(n373, n317);
    let n375: ZB = zn_gt(n373, n317);
    let n376: ZB = zb_and(n369, n374);
    let n377: ZB = zb_and(n369, n375);
    let n378: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n373);
    let n379: ZN = zn_mget(g.cart, n323, n378);
    let n380: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n379);
    let n381: ZN = zn_mul(n373, zn_splat(P8::from_raw(524288i32)));
    let n382: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n381);
    let n383: ZB = zn_eq(n314, n382);
    let n384: ZB = zb_or(n328, n383);
    let n385: ZB = zb_and(n380, n384);
    let n386: ZB = zb_and(n334, n385);
    let n387: ZB = zb_not(n386);
    let n388: ZB = zb_and(n376, n386);
    let n389: ZB = zb_and(n376, n387);
    let n390: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n379);
    let n391: ZB = zb_and(n341, n390);
    let n392: ZB = zb_and(n343, n391);
    let n393: ZB = zb_not(n392);
    let n394: ZB = zb_and(n389, n392);
    let n395: ZB = zb_and(n389, n393);
    let n396: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n379);
    let n397: ZB = zb_and(n350, n396);
    let n398: ZB = zb_and(n352, n397);
    let n399: ZB = zb_not(n398);
    let n400: ZB = zb_and(n395, n398);
    let n401: ZB = zb_and(n395, n399);
    let n402: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n379);
    let n403: ZB = zb_and(n363, n402);
    let n404: ZB = zb_and(n365, n403);
    let n405: ZB = zb_not(n404);
    let n406: ZB = zb_and(n401, n404);
    let n407: ZB = zb_and(n401, n405);
    let n408: ZB = zb_or(n400, n406);
    let n409: ZB = zb_or(n394, n408);
    let n410: ZB = zb_or(n388, n409);
    let n411: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n313);
    let n412: ZB = zn_le(n411, n317);
    let n413: ZB = zn_gt(n411, n317);
    let n414: ZB = zb_and(n407, n412);
    let n415: ZB = zb_and(n407, n413);
    let n416: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n411);
    let n417: ZN = zn_mget(g.cart, n323, n416);
    let n418: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n417);
    let n419: ZN = zn_mul(n411, zn_splat(P8::from_raw(524288i32)));
    let n420: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n419);
    let n421: ZB = zn_eq(n314, n420);
    let n422: ZB = zb_or(n328, n421);
    let n423: ZB = zb_and(n418, n422);
    let n424: ZB = zb_and(n334, n423);
    let n425: ZB = zb_not(n424);
    let n426: ZB = zb_and(n414, n424);
    let n427: ZB = zb_and(n414, n425);
    let n428: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n417);
    let n429: ZB = zb_and(n341, n428);
    let n430: ZB = zb_and(n343, n429);
    let n431: ZB = zb_not(n430);
    let n432: ZB = zb_and(n427, n430);
    let n433: ZB = zb_and(n427, n431);
    let n434: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n417);
    let n435: ZB = zb_and(n350, n434);
    let n436: ZB = zb_and(n352, n435);
    let n437: ZB = zb_not(n436);
    let n438: ZB = zb_and(n433, n436);
    let n439: ZB = zb_and(n433, n437);
    let n440: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n417);
    let n441: ZB = zb_and(n363, n440);
    let n442: ZB = zb_and(n365, n441);
    let n443: ZB = zb_not(n442);
    let n444: ZB = zb_and(n439, n442);
    let n445: ZB = zb_and(n439, n443);
    let n446: ZB = zb_or(n438, n444);
    let n447: ZB = zb_or(n432, n446);
    let n448: ZB = zb_or(n426, n447);
    let n449: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n313);
    let n450: ZB = zn_gt(n449, n317);
    let n451: ZB = zb_and(n296, n450);
    let n452: ZB = zb_or(n415, n445);
    let n453: ZB = zsel_b(n413, n296, n451);
    let n454: ZB = zb_or(n410, n448);
    let n455: ZB = zb_or(n377, n452);
    let n456: ZB = zsel_b(n375, n296, n453);
    let n457: ZB = zb_or(n372, n454);
    let n458: ZB = zb_or(n322, n455);
    let n459: ZB = zsel_b(n320, n296, n456);
    let n460: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n301);
    let n461: ZB = zn_le(n460, n305);
    let n462: ZB = zn_gt(n460, n305);
    let n463: ZB = zb_and(n458, n461);
    let n464: ZB = zb_and(n458, n462);
    let n465: ZB = zb_and(n320, n463);
    let n466: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n460);
    let n467: ZN = zn_mget(g.cart, n466, n324);
    let n468: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n467);
    let n469: ZB = zb_and(n319, n458);
    let n470: ZB = zb_and(n461, n469);
    let n471: ZB = zb_and(n332, n468);
    let n472: ZB = zb_and(n334, n471);
    let n473: ZB = zb_not(n472);
    let n474: ZB = zb_and(n470, n472);
    let n475: ZB = zb_and(n470, n473);
    let n476: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n467);
    let n477: ZB = zb_and(n341, n476);
    let n478: ZB = zb_and(n343, n477);
    let n479: ZB = zb_not(n478);
    let n480: ZB = zb_and(n475, n478);
    let n481: ZB = zb_and(n475, n479);
    let n482: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n467);
    let n483: ZB = zb_and(n350, n482);
    let n484: ZB = zb_and(n352, n483);
    let n485: ZB = zb_not(n484);
    let n486: ZB = zb_and(n481, n484);
    let n487: ZB = zb_and(n481, n485);
    let n488: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n467);
    let n489: ZN = zn_mul(n460, zn_splat(P8::from_raw(524288i32)));
    let n490: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n489);
    let n491: ZB = zn_eq(n302, n490);
    let n492: ZB = zb_or(n359, n491);
    let n493: ZB = zb_and(n488, n492);
    let n494: ZB = zb_and(n365, n493);
    let n495: ZB = zb_not(n494);
    let n496: ZB = zb_and(n487, n494);
    let n497: ZB = zb_and(n487, n495);
    let n498: ZB = zb_or(n486, n496);
    let n499: ZB = zb_or(n480, n498);
    let n500: ZB = zb_or(n474, n499);
    let n501: ZB = zb_and(n375, n497);
    let n502: ZN = zn_mget(g.cart, n466, n378);
    let n503: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n502);
    let n504: ZB = zb_and(n374, n487);
    let n505: ZB = zb_and(n495, n504);
    let n506: ZB = zb_and(n384, n503);
    let n507: ZB = zb_and(n334, n506);
    let n508: ZB = zb_not(n507);
    let n509: ZB = zb_and(n505, n507);
    let n510: ZB = zb_and(n505, n508);
    let n511: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n502);
    let n512: ZB = zb_and(n341, n511);
    let n513: ZB = zb_and(n343, n512);
    let n514: ZB = zb_not(n513);
    let n515: ZB = zb_and(n510, n513);
    let n516: ZB = zb_and(n510, n514);
    let n517: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n502);
    let n518: ZB = zb_and(n350, n517);
    let n519: ZB = zb_and(n352, n518);
    let n520: ZB = zb_not(n519);
    let n521: ZB = zb_and(n516, n519);
    let n522: ZB = zb_and(n516, n520);
    let n523: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n502);
    let n524: ZB = zb_and(n492, n523);
    let n525: ZB = zb_and(n365, n524);
    let n526: ZB = zb_not(n525);
    let n527: ZB = zb_and(n522, n525);
    let n528: ZB = zb_and(n522, n526);
    let n529: ZB = zb_or(n521, n527);
    let n530: ZB = zb_or(n515, n529);
    let n531: ZB = zb_or(n509, n530);
    let n532: ZB = zb_and(n413, n528);
    let n533: ZN = zn_mget(g.cart, n466, n416);
    let n534: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n533);
    let n535: ZB = zb_and(n412, n522);
    let n536: ZB = zb_and(n526, n535);
    let n537: ZB = zb_and(n422, n534);
    let n538: ZB = zb_and(n334, n537);
    let n539: ZB = zb_not(n538);
    let n540: ZB = zb_and(n536, n538);
    let n541: ZB = zb_and(n536, n539);
    let n542: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n533);
    let n543: ZB = zb_and(n341, n542);
    let n544: ZB = zb_and(n343, n543);
    let n545: ZB = zb_not(n544);
    let n546: ZB = zb_and(n541, n544);
    let n547: ZB = zb_and(n541, n545);
    let n548: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n533);
    let n549: ZB = zb_and(n350, n548);
    let n550: ZB = zb_and(n352, n549);
    let n551: ZB = zb_not(n550);
    let n552: ZB = zb_and(n547, n550);
    let n553: ZB = zb_and(n547, n551);
    let n554: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n533);
    let n555: ZB = zb_and(n492, n554);
    let n556: ZB = zb_and(n365, n555);
    let n557: ZB = zb_not(n556);
    let n558: ZB = zb_and(n553, n556);
    let n559: ZB = zb_and(n553, n557);
    let n560: ZB = zb_or(n552, n558);
    let n561: ZB = zb_or(n546, n560);
    let n562: ZB = zb_or(n540, n561);
    let n563: ZB = zb_and(n450, n459);
    let n564: ZB = zb_or(n532, n559);
    let n565: ZB = zsel_b(n413, n459, n563);
    let n566: ZB = zb_or(n531, n562);
    let n567: ZB = zb_or(n501, n564);
    let n568: ZB = zsel_b(n375, n459, n565);
    let n569: ZB = zb_or(n500, n566);
    let n570: ZB = zb_or(n465, n567);
    let n571: ZB = zsel_b(n320, n459, n568);
    let n572: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n301);
    let n573: ZB = zn_le(n572, n305);
    let n574: ZB = zn_gt(n572, n305);
    let n575: ZB = zb_and(n570, n573);
    let n576: ZB = zb_and(n570, n574);
    let n577: ZB = zb_and(n320, n575);
    let n578: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n572);
    let n579: ZN = zn_mget(g.cart, n578, n324);
    let n580: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n579);
    let n581: ZB = zb_and(n319, n570);
    let n582: ZB = zb_and(n573, n581);
    let n583: ZB = zb_and(n332, n580);
    let n584: ZB = zb_and(n334, n583);
    let n585: ZB = zb_not(n584);
    let n586: ZB = zb_and(n582, n584);
    let n587: ZB = zb_and(n582, n585);
    let n588: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n579);
    let n589: ZB = zb_and(n341, n588);
    let n590: ZB = zb_and(n343, n589);
    let n591: ZB = zb_not(n590);
    let n592: ZB = zb_and(n587, n590);
    let n593: ZB = zb_and(n587, n591);
    let n594: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n579);
    let n595: ZB = zb_and(n350, n594);
    let n596: ZB = zb_and(n352, n595);
    let n597: ZB = zb_not(n596);
    let n598: ZB = zb_and(n593, n596);
    let n599: ZB = zb_and(n593, n597);
    let n600: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n579);
    let n601: ZN = zn_mul(n572, zn_splat(P8::from_raw(524288i32)));
    let n602: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n601);
    let n603: ZB = zn_eq(n302, n602);
    let n604: ZB = zb_or(n359, n603);
    let n605: ZB = zb_and(n600, n604);
    let n606: ZB = zb_and(n365, n605);
    let n607: ZB = zb_not(n606);
    let n608: ZB = zb_and(n599, n606);
    let n609: ZB = zb_and(n599, n607);
    let n610: ZB = zb_or(n598, n608);
    let n611: ZB = zb_or(n592, n610);
    let n612: ZB = zb_or(n586, n611);
    let n613: ZB = zb_and(n375, n609);
    let n614: ZN = zn_mget(g.cart, n578, n378);
    let n615: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n614);
    let n616: ZB = zb_and(n374, n599);
    let n617: ZB = zb_and(n607, n616);
    let n618: ZB = zb_and(n384, n615);
    let n619: ZB = zb_and(n334, n618);
    let n620: ZB = zb_not(n619);
    let n621: ZB = zb_and(n617, n619);
    let n622: ZB = zb_and(n617, n620);
    let n623: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n614);
    let n624: ZB = zb_and(n341, n623);
    let n625: ZB = zb_and(n343, n624);
    let n626: ZB = zb_not(n625);
    let n627: ZB = zb_and(n622, n625);
    let n628: ZB = zb_and(n622, n626);
    let n629: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n614);
    let n630: ZB = zb_and(n350, n629);
    let n631: ZB = zb_and(n352, n630);
    let n632: ZB = zb_not(n631);
    let n633: ZB = zb_and(n628, n631);
    let n634: ZB = zb_and(n628, n632);
    let n635: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n614);
    let n636: ZB = zb_and(n604, n635);
    let n637: ZB = zb_and(n365, n636);
    let n638: ZB = zb_not(n637);
    let n639: ZB = zb_and(n634, n637);
    let n640: ZB = zb_and(n634, n638);
    let n641: ZB = zb_or(n633, n639);
    let n642: ZB = zb_or(n627, n641);
    let n643: ZB = zb_or(n621, n642);
    let n644: ZB = zb_and(n413, n640);
    let n645: ZN = zn_mget(g.cart, n578, n416);
    let n646: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n645);
    let n647: ZB = zb_and(n412, n634);
    let n648: ZB = zb_and(n638, n647);
    let n649: ZB = zb_and(n422, n646);
    let n650: ZB = zb_and(n334, n649);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zb_and(n648, n650);
    let n653: ZB = zb_and(n648, n651);
    let n654: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n645);
    let n655: ZB = zb_and(n341, n654);
    let n656: ZB = zb_and(n343, n655);
    let n657: ZB = zb_not(n656);
    let n658: ZB = zb_and(n653, n656);
    let n659: ZB = zb_and(n653, n657);
    let n660: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n645);
    let n661: ZB = zb_and(n350, n660);
    let n662: ZB = zb_and(n352, n661);
    let n663: ZB = zb_not(n662);
    let n664: ZB = zb_and(n659, n662);
    let n665: ZB = zb_and(n659, n663);
    let n666: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n645);
    let n667: ZB = zb_and(n604, n666);
    let n668: ZB = zb_and(n365, n667);
    let n669: ZB = zb_not(n668);
    let n670: ZB = zb_and(n665, n668);
    let n671: ZB = zb_and(n665, n669);
    let n672: ZB = zb_or(n664, n670);
    let n673: ZB = zb_or(n658, n672);
    let n674: ZB = zb_or(n652, n673);
    let n675: ZB = zb_and(n450, n571);
    let n676: ZB = zb_or(n644, n671);
    let n677: ZB = zsel_b(n413, n571, n675);
    let n678: ZB = zb_or(n643, n674);
    let n679: ZB = zb_or(n613, n676);
    let n680: ZB = zsel_b(n375, n571, n677);
    let n681: ZB = zb_or(n612, n678);
    let n682: ZB = zb_or(n577, n679);
    let n683: ZB = zsel_b(n320, n571, n680);
    let n684: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n301);
    let n685: ZB = zn_gt(n684, n305);
    let n686: ZB = zb_and(n683, n685);
    let n687: ZB = zb_or(n569, n681);
    let n688: ZB = zsel_b(n569, n459, n571);
    let n689: ZB = zb_or(n576, n682);
    let n690: ZB = zsel_b(n574, n571, n686);
    let n691: ZB = zb_or(n457, n687);
    let n692: ZB = zsel_b(n457, n296, n688);
    let n693: ZB = zb_or(n464, n689);
    let n694: ZB = zsel_b(n462, n459, n690);
    let n695: ZB = zb_or(n310, n693);
    let n696: ZB = zsel_b(n308, n296, n694);
    let n697: ZB = zn_gt(n293, zn_splat(P8::from_raw(8388608i32)));
    let n698: ZB = zn_le(n293, zn_splat(P8::from_raw(8388608i32)));
    let n699: ZB = zb_and(n695, n697);
    let n700: ZB = zb_or(n691, n699);
    let n701: ZB = zsel_b(n691, n692, n696);
    let n702: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n297);
    let n703: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n298);
    let n704: ZB = zn_tile_flag_at(g.cache, g.cart, n702, n703, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n705: ZB = zb_not(n704);
    let n706: ZN = zsel_n(n704, n152, r_c237);
    let n707: ZN = zsel_n(n704, zn_splat(P8::from_raw(393216i32)), n155);
    let n708: ZB = zn_gt(n294, r_c270);
    let n709: ZB = zn_gt(n295, r_c271);
    let n710: ZN = zsel_n(n705, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n711: ZN = zn_abs(n294);
    let n712: ZB = zn_gt(n711, zn_splat(P8::from_raw(65536i32)));
    let n713: ZB = zn_gt(n294, zn_splat(P8::from_raw(0i32)));
    let n714: ZB = zn_lt(n294, zn_splat(P8::from_raw(0i32)));
    let n715: ZB = zn_gt(n294, zn_splat(P8::from_raw(65536i32)));
    let n716: ZN = zn_sub(n294, zn_splat(P8::from_raw(9830i32)));
    let n717: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n716);
    let n718: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n294);
    let n719: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n718);
    let n720: ZB = zn_gt(n294, zn_splat(P8::from_raw(-65536i32)));
    let n721: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n716);
    let n722: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n718);
    let n723: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n716);
    let n724: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n718);
    let n725: ZN = zsel_n(n720, n721, n722);
    let n726: ZN = zsel_n(n713, n723, n724);
    let n727: ZN = zsel_n(n715, n717, n719);
    let n728: ZN = zsel_n(n714, n725, n726);
    let n729: ZN = zsel_n(n713, n727, n728);
    let n730: ZN = zn_sub(n294, n710);
    let n731: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n730);
    let n732: ZN = zn_add(n294, n710);
    let n733: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n732);
    let n734: ZN = zsel_n(n713, n731, n733);
    let n735: ZN = zsel_n(n712, n729, n734);
    let n736: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n735);
    let n737: ZB = zb_not(n736);
    let n738: ZB = zn_lt(n735, zn_splat(P8::from_raw(0i32)));
    let n739: ZB = zsel_b(n737, n738, r_c272);
    let n740: ZN = zn_abs(n295);
    let n741: ZB = zn_le(n740, zn_splat(P8::from_raw(9830i32)));
    let n742: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n298);
    let n743: ZB = zn_gt(n295, zn_splat(P8::from_raw(131072i32)));
    let n744: ZB = zn_gt(n707, zn_splat(P8::from_raw(0i32)));
    let n745: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n297);
    let n746: ZB = zn_tile_flag_at(g.cache, g.cart, n745, n742, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n747: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n297);
    let n748: ZB = zn_tile_flag_at(g.cache, g.cart, n747, n742, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n749: ZN = zsel_n(n748, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n750: ZN = zsel_n(n746, zn_splat(P8::from_raw(-65536i32)), n749);
    let n751: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n750);
    let n752: ZB = zb_not(n751);
    let n753: ZB = zn_gt(n706, zn_splat(P8::from_raw(0i32)));
    let n754: ZN = zsel_n(n739, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n755: ZB = zn_gt(n754, zn_splat(P8::from_raw(0i32)));
    let n756: ZB = zn_lt(n754, zn_splat(P8::from_raw(0i32)));
    let n757: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n754);
    let n758: ZB = zb_not(n757);
    let n759: ZB = zn_lt(n293, zn_splat(P8::from_raw(-262144i32)));
    let n760: ZB = zn_ge(n293, zn_splat(P8::from_raw(-262144i32)));
    let n761: ZB = zb_and(n700, n759);
    let n763: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n764: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n765: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n764);
    let n775: ZN = zsel_n(n697, n765, n764);
    let n776: ZN = zsel_n(n691, n775, n764);
    let n780: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n730);
    let n781: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n732);
    let n782: ZN = zsel_n(n720, n780, n781);
    let n783: ZN = zsel_n(n712, n729, n782);
    let n784: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n783);
    let n785: ZB = zb_not(n784);
    let n786: ZB = zn_lt(n783, zn_splat(P8::from_raw(0i32)));
    let n787: ZB = zsel_b(n785, n786, r_c272);
    let n788: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n297);
    let n789: ZB = zn_tile_flag_at(g.cache, g.cart, n788, n742, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n790: ZN = zsel_n(n789, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n791: ZB = zn_gt(n295, n790);
    let n792: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n730);
    let n793: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n732);
    let n794: ZN = zsel_n(n715, n792, n793);
    let n795: ZN = zsel_n(n712, n729, n794);
    let n796: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n795);
    let n797: ZB = zb_not(n796);
    let n798: ZB = zn_lt(n795, zn_splat(P8::from_raw(0i32)));
    let n799: ZB = zsel_b(n797, n798, r_c272);
    let n800: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n297);
    let n801: ZB = zn_tile_flag_at(g.cache, g.cart, n800, n742, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n802: ZN = zsel_n(n801, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n803: ZB = zn_gt(n295, n802);
    let n804: ZB = zb_and(n150, n753);
    let n805: ZN = zsel_n(n804, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n806: ZB = zb_or(r_c41, n804);
    let n807: ZN = zsel_n(n156, r_c20, n805);
    let n808: ZB = zsel_b(n156, r_c41, n806);
    let n816: ZB = zb_and(n695, n698);
    let n817: ZB = zb_and(n759, n816);
    let n818: ZB = zb_and(n760, n816);
    let n819: ZB = zb_not(n817);
    let n820: ZB = zb_or(n761, n817);
    let n821: ZB = zsel_b(n817, n696, n701);
    let n822: ZN = zsel_n(n817, r_c87, n776);
    let n823: ZN = zsel_n(n817, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n835: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n836: ZN = zn_sub(n93, zn_splat(P8::from_raw(32768i32)));
    let n837: ZN = zn_sub(n836, n94);
    let n838: ZN = zn_sub(n111, zn_splat(P8::from_raw(32768i32)));
    let n839: ZN = zn_sub(n838, n112);
    let n840: ZN = zn_sub(r_c234, zn_splat(P8::from_raw(65536i32)));
    let n841: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n842: ZB = zb_and(r_c246, n763);
    let n843: ZB = zb_and(r_c247, n763);
    let n844: ZN = zsel_n(n189, zn_splat(P8::from_raw(0i32)), n837);
    let n845: ZN = zsel_n(n108, n837, n844);
    let n846: ZN = zsel_n(n185, zn_splat(P8::from_raw(0i32)), n845);
    let n847: ZN = zsel_n(n107, n837, n846);
    let n848: ZN = zsel_n(n181, zn_splat(P8::from_raw(0i32)), n847);
    let n849: ZN = zsel_n(n106, n837, n848);
    let n850: ZN = zsel_n(n177, zn_splat(P8::from_raw(0i32)), n849);
    let n851: ZN = zsel_n(n105, n837, n850);
    let n852: ZN = zsel_n(n173, zn_splat(P8::from_raw(0i32)), n851);
    let n853: ZN = zsel_n(n104, n837, n852);
    let n854: ZN = zsel_n(n169, zn_splat(P8::from_raw(0i32)), n853);
    let n855: ZN = zsel_n(n103, n837, n854);
    let n856: ZN = zsel_n(n165, zn_splat(P8::from_raw(0i32)), n855);
    let n857: ZN = zsel_n(n102, n837, n856);
    let n858: ZN = zsel_n(n161, zn_splat(P8::from_raw(0i32)), n857);
    let n859: ZN = zsel_n(n245, zn_splat(P8::from_raw(0i32)), n839);
    let n860: ZN = zsel_n(n144, n839, n859);
    let n861: ZN = zsel_n(n244, zn_splat(P8::from_raw(0i32)), n860);
    let n862: ZN = zsel_n(n140, n839, n861);
    let n863: ZN = zsel_n(n243, zn_splat(P8::from_raw(0i32)), n862);
    let n864: ZN = zsel_n(n136, n839, n863);
    let n865: ZN = zsel_n(n242, zn_splat(P8::from_raw(0i32)), n864);
    let n866: ZN = zsel_n(n132, n839, n865);
    let n867: ZN = zsel_n(n241, zn_splat(P8::from_raw(0i32)), n866);
    let n868: ZN = zsel_n(n128, n839, n867);
    let n869: ZN = zsel_n(n240, zn_splat(P8::from_raw(0i32)), n868);
    let n870: ZN = zsel_n(n124, n839, n869);
    let n871: ZN = zsel_n(n239, zn_splat(P8::from_raw(0i32)), n870);
    let n872: ZN = zsel_n(n120, n839, n871);
    let n873: ZN = zsel_n(n238, zn_splat(P8::from_raw(0i32)), n872);
    let n874: ZN = zsel_n(n90, n858, r_c278);
    let n875: ZN = zsel_n(n90, n873, r_c279);
    let n876: ZN = zn_sub(n294, r_c268);
    let n877: ZN = zn_max(r_c270, n876);
    let n878: ZN = zn_add(n294, r_c268);
    let n879: ZN = zn_min(r_c270, n878);
    let n880: ZN = zsel_n(n708, n877, n879);
    let n881: ZN = zn_sub(n295, r_c269);
    let n882: ZN = zn_max(r_c271, n881);
    let n883: ZN = zn_add(n295, r_c269);
    let n884: ZN = zn_min(r_c271, n883);
    let n885: ZN = zsel_n(n709, n882, n884);
    let n886: ZN = zsel_n(n741, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n887: ZN = zn_sub(n295, n886);
    let n888: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n887);
    let n889: ZN = zn_add(n295, n886);
    let n890: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n889);
    let n891: ZN = zsel_n(n743, n888, n890);
    let n892: ZN = zsel_n(n705, n891, n295);
    let n893: ZN = zn_neg(n750);
    let n894: ZN = zn_mul(n893, zn_splat(P8::from_raw(131072i32)));
    let n895: ZN = zsel_n(n752, n894, n735);
    let n896: ZN = zsel_n(n752, zn_splat(P8::from_raw(-131072i32)), n892);
    let n897: ZN = zsel_n(n744, zn_splat(P8::from_raw(0i32)), n707);
    let n898: ZN = zsel_n(n744, n735, n895);
    let n899: ZN = zsel_n(n744, zn_splat(P8::from_raw(-131072i32)), n896);
    let n900: ZN = zn_sub(n706, zn_splat(P8::from_raw(65536i32)));
    let n901: ZN = zsel_n(n756, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n902: ZN = zsel_n(n755, zn_splat(P8::from_raw(131072i32)), n901);
    let n903: ZN = zsel_n(n758, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n904: ZN = zsel_n(n156, n841, r_c236);
    let n905: ZB = zsel_b(n156, r_c272, n739);
    let n906: ZN = zsel_n(n156, n880, n735);
    let n907: ZN = zsel_n(n156, n885, n892);
    let n908: ZN = zsel_n(n763, n835, r_c20);
    let n909: ZN = zsel_n(n763, r_c234, n840);
    let n910: ZN = zsel_n(n763, r_c236, n904);
    let n911: ZN = zsel_n(n763, r_c237, n706);
    let n912: ZN = zsel_n(n763, r_c239, n707);
    let n913: ZN = zsel_n(n763, r_c253, n292);
    let n914: ZN = zsel_n(n763, r_c254, n293);
    let n915: ZB = zsel_b(n763, r_c272, n905);
    let n916: ZN = zsel_n(n763, r_c278, n874);
    let n917: ZN = zsel_n(n763, r_c279, n875);
    let n918: ZN = zsel_n(n763, r_c280, n906);
    let n919: ZN = zsel_n(n763, r_c281, n907);
    let n920: ZB = zb_or(n763, n818);
    let n921: ZB = zb_or(n696, n763);
    let n922: ZB = zn_gt(n908, zn_splat(P8::from_raw(0i32)));
    let n923: ZB = zn_lt(n913, zn_splat(P8::from_raw(-65536i32)));
    let n924: ZB = zn_gt(n913, zn_splat(P8::from_raw(7929856i32)));
    let n925: ZB = zb_or(n923, n924);
    let n926: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n913);
    let n927: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n926);
    let n928: ZN = zsel_n(n925, n927, n913);
    let n929: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n918);
    let n930: ZN = zsel_n(n922, n913, n928);
    let n931: ZN = zsel_n(n922, n918, n929);
    let n933: ZN = zn_max(n790, n887);
    let n934: ZN = zn_min(n790, n889);
    let n935: ZN = zsel_n(n791, n933, n934);
    let n936: ZN = zsel_n(n705, n935, n295);
    let n937: ZN = zsel_n(n752, n894, n783);
    let n938: ZN = zsel_n(n752, zn_splat(P8::from_raw(-131072i32)), n936);
    let n939: ZN = zsel_n(n744, n783, n937);
    let n940: ZN = zsel_n(n744, zn_splat(P8::from_raw(-131072i32)), n938);
    let n941: ZB = zsel_b(n156, r_c272, n787);
    let n942: ZN = zsel_n(n156, n880, n783);
    let n943: ZN = zsel_n(n156, n885, n936);
    let n944: ZB = zsel_b(n763, r_c272, n941);
    let n945: ZN = zsel_n(n763, r_c280, n942);
    let n946: ZN = zsel_n(n763, r_c281, n943);
    let n947: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n945);
    let n948: ZN = zsel_n(n922, n945, n947);
    let n949: ZN = zn_max(n802, n887);
    let n950: ZN = zn_min(n802, n889);
    let n951: ZN = zsel_n(n803, n949, n950);
    let n952: ZN = zsel_n(n705, n951, n295);
    let n953: ZN = zsel_n(n752, n894, n795);
    let n954: ZN = zsel_n(n752, zn_splat(P8::from_raw(-131072i32)), n952);
    let n955: ZN = zsel_n(n744, n795, n953);
    let n956: ZN = zsel_n(n744, zn_splat(P8::from_raw(-131072i32)), n954);
    let n957: ZB = zsel_b(n156, r_c272, n799);
    let n958: ZN = zsel_n(n156, n880, n795);
    let n959: ZN = zsel_n(n156, n885, n952);
    let n960: ZB = zsel_b(n763, r_c272, n957);
    let n961: ZN = zsel_n(n763, r_c280, n958);
    let n962: ZN = zsel_n(n763, r_c281, n959);
    let n963: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n961);
    let n964: ZN = zsel_n(n922, n961, n963);
    let n965: ZB = zb_or(r_c247, n68);
    let n966: ZN = zsel_n(n149, n897, n707);
    let n967: ZN = zsel_n(n149, n898, n735);
    let n968: ZN = zsel_n(n149, n899, n892);
    let n969: ZN = zsel_n(n156, n707, n966);
    let n970: ZN = zsel_n(n156, n880, n967);
    let n971: ZN = zsel_n(n156, n885, n968);
    let n972: ZN = zsel_n(n763, r_c239, n969);
    let n973: ZN = zsel_n(n763, r_c280, n970);
    let n974: ZN = zsel_n(n763, r_c281, n971);
    let n975: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n973);
    let n976: ZN = zsel_n(n922, n973, n975);
    let n977: ZN = zsel_n(n149, n939, n783);
    let n978: ZN = zsel_n(n149, n940, n936);
    let n979: ZN = zsel_n(n156, n880, n977);
    let n980: ZN = zsel_n(n156, n885, n978);
    let n981: ZN = zsel_n(n763, r_c280, n979);
    let n982: ZN = zsel_n(n763, r_c281, n980);
    let n983: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n981);
    let n984: ZN = zsel_n(n922, n981, n983);
    let n985: ZN = zsel_n(n149, n955, n795);
    let n986: ZN = zsel_n(n149, n956, n952);
    let n987: ZN = zsel_n(n156, n880, n985);
    let n988: ZN = zsel_n(n156, n885, n986);
    let n989: ZN = zsel_n(n763, r_c280, n987);
    let n990: ZN = zsel_n(n763, r_c281, n988);
    let n991: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n989);
    let n992: ZN = zsel_n(n922, n989, n991);
    let n993: ZB = zb_or(r_c246, n68);
    let n994: ZN = zsel_n(n804, zn_splat(P8::from_raw(655360i32)), n840);
    let n995: ZN = zsel_n(n804, zn_splat(P8::from_raw(262144i32)), r_c236);
    let n996: ZN = zsel_n(n804, n900, n706);
    let n997: ZN = zsel_n(n804, zn_splat(P8::from_raw(98304i32)), r_c268);
    let n998: ZN = zsel_n(n804, n903, r_c269);
    let n999: ZN = zsel_n(n804, n902, r_c270);
    let n1000: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), r_c271);
    let n1001: ZN = zsel_n(n804, n754, n735);
    let n1002: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n892);
    let n1003: ZN = zsel_n(n156, n840, n994);
    let n1004: ZN = zsel_n(n156, n841, n995);
    let n1005: ZN = zsel_n(n156, n706, n996);
    let n1006: ZN = zsel_n(n156, r_c268, n997);
    let n1007: ZN = zsel_n(n156, r_c269, n998);
    let n1008: ZN = zsel_n(n156, r_c270, n999);
    let n1009: ZN = zsel_n(n156, r_c271, n1000);
    let n1010: ZN = zsel_n(n156, n880, n1001);
    let n1011: ZN = zsel_n(n156, n885, n1002);
    let n1012: ZN = zsel_n(n763, n835, n807);
    let n1013: ZB = zsel_b(n763, r_c41, n808);
    let n1014: ZN = zsel_n(n763, r_c234, n1003);
    let n1015: ZN = zsel_n(n763, r_c236, n1004);
    let n1016: ZN = zsel_n(n763, r_c237, n1005);
    let n1017: ZN = zsel_n(n763, r_c268, n1006);
    let n1018: ZN = zsel_n(n763, r_c269, n1007);
    let n1019: ZN = zsel_n(n763, r_c270, n1008);
    let n1020: ZN = zsel_n(n763, r_c271, n1009);
    let n1021: ZN = zsel_n(n763, r_c280, n1010);
    let n1022: ZN = zsel_n(n763, r_c281, n1011);
    let n1023: ZB = zn_gt(n1012, zn_splat(P8::from_raw(0i32)));
    let n1024: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1021);
    let n1025: ZN = zsel_n(n1023, n913, n928);
    let n1026: ZN = zsel_n(n1023, n1021, n1024);
    let n1027: ZN = zsel_n(n804, zn_splat(P8::from_raw(69510i32)), r_c269);
    let n1028: ZN = zsel_n(n804, zn_splat(P8::from_raw(-131072i32)), r_c270);
    let n1029: ZN = zsel_n(n804, zn_splat(P8::from_raw(-327680i32)), n783);
    let n1030: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n936);
    let n1031: ZN = zsel_n(n156, r_c269, n1027);
    let n1032: ZN = zsel_n(n156, r_c270, n1028);
    let n1033: ZN = zsel_n(n156, n880, n1029);
    let n1034: ZN = zsel_n(n156, n885, n1030);
    let n1035: ZN = zsel_n(n763, r_c269, n1031);
    let n1036: ZN = zsel_n(n763, r_c270, n1032);
    let n1037: ZN = zsel_n(n763, r_c280, n1033);
    let n1038: ZN = zsel_n(n763, r_c281, n1034);
    let n1039: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1037);
    let n1040: ZN = zsel_n(n1023, n1037, n1039);
    let n1041: ZN = zsel_n(n804, zn_splat(P8::from_raw(131072i32)), r_c270);
    let n1042: ZN = zsel_n(n804, zn_splat(P8::from_raw(327680i32)), n795);
    let n1043: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n952);
    let n1044: ZN = zsel_n(n156, r_c270, n1041);
    let n1045: ZN = zsel_n(n156, n880, n1042);
    let n1046: ZN = zsel_n(n156, n885, n1043);
    let n1047: ZN = zsel_n(n763, r_c270, n1044);
    let n1048: ZN = zsel_n(n763, r_c280, n1045);
    let n1049: ZN = zsel_n(n763, r_c281, n1046);
    let n1050: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1048);
    let n1051: ZN = zsel_n(n1023, n1048, n1050);
    let n1053: ZN = zsel_n(n804, zn_splat(P8::from_raw(69510i32)), r_c268);
    let n1054: ZN = zsel_n(n804, zn_splat(P8::from_raw(98304i32)), r_c269);
    let n1055: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), r_c270);
    let n1056: ZN = zsel_n(n804, zn_splat(P8::from_raw(-98304i32)), r_c271);
    let n1057: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n735);
    let n1058: ZN = zsel_n(n804, zn_splat(P8::from_raw(-327680i32)), n892);
    let n1059: ZN = zsel_n(n156, r_c268, n1053);
    let n1060: ZN = zsel_n(n156, r_c269, n1054);
    let n1061: ZN = zsel_n(n156, r_c270, n1055);
    let n1062: ZN = zsel_n(n156, r_c271, n1056);
    let n1063: ZN = zsel_n(n156, n880, n1057);
    let n1064: ZN = zsel_n(n156, n885, n1058);
    let n1065: ZN = zsel_n(n763, r_c268, n1059);
    let n1066: ZN = zsel_n(n763, r_c269, n1060);
    let n1067: ZN = zsel_n(n763, r_c270, n1061);
    let n1068: ZN = zsel_n(n763, r_c271, n1062);
    let n1069: ZN = zsel_n(n763, r_c280, n1063);
    let n1070: ZN = zsel_n(n763, r_c281, n1064);
    let n1071: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1069);
    let n1072: ZN = zsel_n(n1023, n1069, n1071);
    let n1073: ZN = zsel_n(n804, zn_splat(P8::from_raw(-231700i32)), n783);
    let n1074: ZN = zsel_n(n804, zn_splat(P8::from_raw(-231700i32)), n936);
    let n1075: ZN = zsel_n(n156, n880, n1073);
    let n1076: ZN = zsel_n(n156, n885, n1074);
    let n1077: ZN = zsel_n(n763, r_c280, n1075);
    let n1078: ZN = zsel_n(n763, r_c281, n1076);
    let n1079: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1077);
    let n1080: ZN = zsel_n(n1023, n1077, n1079);
    let n1081: ZN = zsel_n(n804, zn_splat(P8::from_raw(231700i32)), n795);
    let n1082: ZN = zsel_n(n804, zn_splat(P8::from_raw(-231700i32)), n952);
    let n1083: ZN = zsel_n(n156, n880, n1081);
    let n1084: ZN = zsel_n(n156, n885, n1082);
    let n1085: ZN = zsel_n(n763, r_c280, n1083);
    let n1086: ZN = zsel_n(n763, r_c281, n1084);
    let n1087: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1085);
    let n1088: ZN = zsel_n(n1023, n1085, n1087);
    let n1089: ZN = zsel_n(n804, zn_splat(P8::from_raw(131072i32)), r_c271);
    let n1090: ZN = zsel_n(n804, zn_splat(P8::from_raw(327680i32)), n892);
    let n1091: ZN = zsel_n(n156, r_c271, n1089);
    let n1092: ZN = zsel_n(n156, n885, n1090);
    let n1093: ZN = zsel_n(n763, r_c271, n1091);
    let n1094: ZN = zsel_n(n763, r_c281, n1092);
    let n1095: ZN = zsel_n(n804, zn_splat(P8::from_raw(231700i32)), n936);
    let n1096: ZN = zsel_n(n156, n885, n1095);
    let n1097: ZN = zsel_n(n763, r_c281, n1096);
    let n1098: ZN = zsel_n(n804, zn_splat(P8::from_raw(231700i32)), n952);
    let n1099: ZN = zsel_n(n156, n885, n1098);
    let n1100: ZN = zsel_n(n763, r_c281, n1099);
    let n1101: ZN = zsel_n(n804, n754, n967);
    let n1102: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n968);
    let n1103: ZN = zsel_n(n156, n880, n1101);
    let n1104: ZN = zsel_n(n156, n885, n1102);
    let n1105: ZN = zsel_n(n763, r_c280, n1103);
    let n1106: ZN = zsel_n(n763, r_c281, n1104);
    let n1107: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1105);
    let n1108: ZN = zsel_n(n1023, n1105, n1107);
    let n1109: ZN = zsel_n(n804, zn_splat(P8::from_raw(-327680i32)), n977);
    let n1110: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n978);
    let n1111: ZN = zsel_n(n156, n880, n1109);
    let n1112: ZN = zsel_n(n156, n885, n1110);
    let n1113: ZN = zsel_n(n763, r_c280, n1111);
    let n1114: ZN = zsel_n(n763, r_c281, n1112);
    let n1115: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1113);
    let n1116: ZN = zsel_n(n1023, n1113, n1115);
    let n1117: ZN = zsel_n(n804, zn_splat(P8::from_raw(327680i32)), n985);
    let n1118: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n986);
    let n1119: ZN = zsel_n(n156, n880, n1117);
    let n1120: ZN = zsel_n(n156, n885, n1118);
    let n1121: ZN = zsel_n(n763, r_c280, n1119);
    let n1122: ZN = zsel_n(n763, r_c281, n1120);
    let n1123: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1121);
    let n1124: ZN = zsel_n(n1023, n1121, n1123);
    let n1125: ZN = zsel_n(n804, zn_splat(P8::from_raw(0i32)), n967);
    let n1126: ZN = zsel_n(n804, zn_splat(P8::from_raw(-327680i32)), n968);
    let n1127: ZN = zsel_n(n156, n880, n1125);
    let n1128: ZN = zsel_n(n156, n885, n1126);
    let n1129: ZN = zsel_n(n763, r_c280, n1127);
    let n1130: ZN = zsel_n(n763, r_c281, n1128);
    let n1131: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1129);
    let n1132: ZN = zsel_n(n1023, n1129, n1131);
    let n1133: ZN = zsel_n(n804, zn_splat(P8::from_raw(-231700i32)), n977);
    let n1134: ZN = zsel_n(n804, zn_splat(P8::from_raw(-231700i32)), n978);
    let n1135: ZN = zsel_n(n156, n880, n1133);
    let n1136: ZN = zsel_n(n156, n885, n1134);
    let n1137: ZN = zsel_n(n763, r_c280, n1135);
    let n1138: ZN = zsel_n(n763, r_c281, n1136);
    let n1139: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1137);
    let n1140: ZN = zsel_n(n1023, n1137, n1139);
    let n1141: ZN = zsel_n(n804, zn_splat(P8::from_raw(231700i32)), n985);
    let n1142: ZN = zsel_n(n804, zn_splat(P8::from_raw(-231700i32)), n986);
    let n1143: ZN = zsel_n(n156, n880, n1141);
    let n1144: ZN = zsel_n(n156, n885, n1142);
    let n1145: ZN = zsel_n(n763, r_c280, n1143);
    let n1146: ZN = zsel_n(n763, r_c281, n1144);
    let n1147: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1145);
    let n1148: ZN = zsel_n(n1023, n1145, n1147);
    let n1149: ZN = zsel_n(n804, zn_splat(P8::from_raw(327680i32)), n968);
    let n1150: ZN = zsel_n(n156, n885, n1149);
    let n1151: ZN = zsel_n(n763, r_c281, n1150);
    let n1152: ZN = zsel_n(n804, zn_splat(P8::from_raw(231700i32)), n978);
    let n1153: ZN = zsel_n(n156, n885, n1152);
    let n1154: ZN = zsel_n(n763, r_c281, n1153);
    let n1155: ZN = zsel_n(n804, zn_splat(P8::from_raw(231700i32)), n986);
    let n1156: ZN = zsel_n(n156, n885, n1155);
    let n1157: ZN = zsel_n(n763, r_c281, n1156);
    let n1159: ZW = zw_cellmix_n(84u64, n57, 1542469173u64);
    let n1160: ZW = zw_cellmix_n(84u64, n57, 668265263u64);
    let n1161: ZW = zw_add(zw_splat(0u64), n1159);
    let n1162: ZW = zw_add(zw_splat(0u64), n1160);
    let n1163: ZW = zw_cellmix_n(85u64, n86, 1542469173u64);
    let n1164: ZW = zw_cellmix_n(85u64, n86, 668265263u64);
    let n1165: ZW = zw_add(n1161, n1163);
    let n1166: ZW = zw_add(n1162, n1164);
    let n1167: ZW = zw_cellmix_n(86u64, n158, 1542469173u64);
    let n1168: ZW = zw_cellmix_n(86u64, n158, 668265263u64);
    let n1169: ZW = zw_add(n1165, n1167);
    let n1170: ZW = zw_add(n1166, n1168);
    let n1171: ZW = zw_cellmix_n(87u64, n776, 1542469173u64);
    let n1172: ZW = zw_cellmix_n(87u64, n776, 668265263u64);
    let n1173: ZW = zw_add(n1169, n1171);
    let n1174: ZW = zw_add(n1170, n1172);
    let n1175: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n1176: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n1177: ZW = zw_add(n1173, n1175);
    let n1178: ZW = zw_add(n1174, n1176);
    let n1179: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n1180: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n1181: ZW = zw_add(n1177, n1179);
    let n1182: ZW = zw_add(n1178, n1180);
    let n1183: ZW = zw_cellmix_n(20u64, n807, 1542469173u64);
    let n1184: ZW = zw_cellmix_n(20u64, n807, 668265263u64);
    let n1185: ZW = zw_add(n1173, n1183);
    let n1186: ZW = zw_add(n1174, n1184);
    let n1187: ZW = zw_cellmix_b(41u64, n808, 1542469173u64);
    let n1188: ZW = zw_cellmix_b(41u64, n808, 668265263u64);
    let n1189: ZW = zw_add(n1185, n1187);
    let n1190: ZW = zw_add(n1186, n1188);
    let n1191: ZW = zw_cellmix_b(38u64, n819, 1542469173u64);
    let n1192: ZW = zw_cellmix_b(38u64, n819, 668265263u64);
    let n1193: ZW = zw_add(zw_splat(0u64), n1191);
    let n1194: ZW = zw_add(zw_splat(0u64), n1192);
    let n1195: ZW = zw_cellmix_n(39u64, n823, 1542469173u64);
    let n1196: ZW = zw_cellmix_n(39u64, n823, 668265263u64);
    let n1197: ZW = zw_add(n1193, n1195);
    let n1198: ZW = zw_add(n1194, n1196);
    let n1199: ZW = zw_add(n1197, n1159);
    let n1200: ZW = zw_add(n1198, n1160);
    let n1201: ZW = zw_add(n1199, n1163);
    let n1202: ZW = zw_add(n1200, n1164);
    let n1203: ZW = zw_add(n1201, n1167);
    let n1204: ZW = zw_add(n1202, n1168);
    let n1205: ZW = zw_cellmix_n(87u64, n822, 1542469173u64);
    let n1206: ZW = zw_cellmix_n(87u64, n822, 668265263u64);
    let n1207: ZW = zw_add(n1203, n1205);
    let n1208: ZW = zw_add(n1204, n1206);
    let n1209: ZW = zw_add(n1207, n1175);
    let n1210: ZW = zw_add(n1208, n1176);
    let n1211: ZW = zw_add(n1207, n1183);
    let n1212: ZW = zw_add(n1208, n1184);
    let n1213: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n1214: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n1215: ZW = zw_add(zw_splat(0u64), n1213);
    let n1216: ZW = zw_add(zw_splat(0u64), n1214);
    let n1217: ZW = zw_add(n1215, n1159);
    let n1218: ZW = zw_add(n1216, n1160);
    let n1219: ZW = zw_add(n1217, n1163);
    let n1220: ZW = zw_add(n1218, n1164);
    let n1221: ZW = zw_add(n1219, n1167);
    let n1222: ZW = zw_add(n1220, n1168);
    let n1223: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n1224: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n1225: ZW = zw_add(n1221, n1223);
    let n1226: ZW = zw_add(n1222, n1224);
    let n1227: ZW = zw_cellmix_n(254u64, n914, 1542469173u64);
    let n1228: ZW = zw_cellmix_n(254u64, n914, 668265263u64);
    let n1229: ZW = zw_add(n1225, n1227);
    let n1230: ZW = zw_add(n1226, n1228);
    let n1231: ZW = zw_cellmix_n(278u64, n916, 1542469173u64);
    let n1232: ZW = zw_cellmix_n(278u64, n916, 668265263u64);
    let n1233: ZW = zw_add(n1229, n1231);
    let n1234: ZW = zw_add(n1230, n1232);
    let n1235: ZW = zw_cellmix_n(279u64, n917, 1542469173u64);
    let n1236: ZW = zw_cellmix_n(279u64, n917, 668265263u64);
    let n1237: ZW = zw_add(n1233, n1235);
    let n1238: ZW = zw_add(n1234, n1236);
    let n1239: ZW = zw_cellmix_n(20u64, n908, 1542469173u64);
    let n1240: ZW = zw_cellmix_n(20u64, n908, 668265263u64);
    let n1241: ZW = zw_add(n1237, n1239);
    let n1242: ZW = zw_add(n1238, n1240);
    let n1243: ZW = zw_add(n1241, n1179);
    let n1244: ZW = zw_add(n1242, n1180);
    let n1245: ZW = zw_cellmix_n(234u64, n909, 1542469173u64);
    let n1246: ZW = zw_cellmix_n(234u64, n909, 668265263u64);
    let n1247: ZW = zw_add(n1243, n1245);
    let n1248: ZW = zw_add(n1244, n1246);
    let n1249: ZW = zw_cellmix_n(236u64, n910, 1542469173u64);
    let n1250: ZW = zw_cellmix_n(236u64, n910, 668265263u64);
    let n1251: ZW = zw_add(n1247, n1249);
    let n1252: ZW = zw_add(n1248, n1250);
    let n1253: ZW = zw_cellmix_n(237u64, n911, 1542469173u64);
    let n1254: ZW = zw_cellmix_n(237u64, n911, 668265263u64);
    let n1255: ZW = zw_add(n1251, n1253);
    let n1256: ZW = zw_add(n1252, n1254);
    let n1257: ZW = zw_cellmix_n(239u64, n912, 1542469173u64);
    let n1258: ZW = zw_cellmix_n(239u64, n912, 668265263u64);
    let n1259: ZW = zw_add(n1255, n1257);
    let n1260: ZW = zw_add(n1256, n1258);
    let n1261: ZW = zw_cellmix_b(246u64, n842, 1542469173u64);
    let n1262: ZW = zw_cellmix_b(246u64, n842, 668265263u64);
    let n1263: ZW = zw_add(n1259, n1261);
    let n1264: ZW = zw_add(n1260, n1262);
    let n1265: ZW = zw_cellmix_b(247u64, n843, 1542469173u64);
    let n1266: ZW = zw_cellmix_b(247u64, n843, 668265263u64);
    let n1267: ZW = zw_add(n1263, n1265);
    let n1268: ZW = zw_add(n1264, n1266);
    let n1269: ZW = zw_cellmix_n(253u64, n930, 1542469173u64);
    let n1270: ZW = zw_cellmix_n(253u64, n930, 668265263u64);
    let n1271: ZW = zw_add(n1267, n1269);
    let n1272: ZW = zw_add(n1268, n1270);
    let n1273: ZW = zw_cellmix_n(268u64, r_c268, 1542469173u64);
    let n1274: ZW = zw_cellmix_n(268u64, r_c268, 668265263u64);
    let n1275: ZW = zw_add(n1271, n1273);
    let n1276: ZW = zw_add(n1272, n1274);
    let n1277: ZW = zw_cellmix_n(269u64, r_c269, 1542469173u64);
    let n1278: ZW = zw_cellmix_n(269u64, r_c269, 668265263u64);
    let n1279: ZW = zw_add(n1275, n1277);
    let n1280: ZW = zw_add(n1276, n1278);
    let n1281: ZW = zw_cellmix_n(270u64, r_c270, 1542469173u64);
    let n1282: ZW = zw_cellmix_n(270u64, r_c270, 668265263u64);
    let n1283: ZW = zw_add(n1279, n1281);
    let n1284: ZW = zw_add(n1280, n1282);
    let n1285: ZW = zw_cellmix_n(271u64, r_c271, 1542469173u64);
    let n1286: ZW = zw_cellmix_n(271u64, r_c271, 668265263u64);
    let n1287: ZW = zw_add(n1283, n1285);
    let n1288: ZW = zw_add(n1284, n1286);
    let n1289: ZW = zw_cellmix_b(272u64, n915, 1542469173u64);
    let n1290: ZW = zw_cellmix_b(272u64, n915, 668265263u64);
    let n1291: ZW = zw_add(n1287, n1289);
    let n1292: ZW = zw_add(n1288, n1290);
    let n1293: ZW = zw_cellmix_n(280u64, n931, 1542469173u64);
    let n1294: ZW = zw_cellmix_n(280u64, n931, 668265263u64);
    let n1295: ZW = zw_add(n1291, n1293);
    let n1296: ZW = zw_add(n1292, n1294);
    let n1297: ZW = zw_cellmix_n(281u64, n919, 1542469173u64);
    let n1298: ZW = zw_cellmix_n(281u64, n919, 668265263u64);
    let n1299: ZW = zw_add(n1295, n1297);
    let n1300: ZW = zw_add(n1296, n1298);
    let n1301: ZW = zw_cellmix_b(272u64, n944, 1542469173u64);
    let n1302: ZW = zw_cellmix_b(272u64, n944, 668265263u64);
    let n1303: ZW = zw_add(n1287, n1301);
    let n1304: ZW = zw_add(n1288, n1302);
    let n1305: ZW = zw_cellmix_n(280u64, n948, 1542469173u64);
    let n1306: ZW = zw_cellmix_n(280u64, n948, 668265263u64);
    let n1307: ZW = zw_add(n1303, n1305);
    let n1308: ZW = zw_add(n1304, n1306);
    let n1309: ZW = zw_cellmix_n(281u64, n946, 1542469173u64);
    let n1310: ZW = zw_cellmix_n(281u64, n946, 668265263u64);
    let n1311: ZW = zw_add(n1307, n1309);
    let n1312: ZW = zw_add(n1308, n1310);
    let n1313: ZW = zw_cellmix_b(272u64, n960, 1542469173u64);
    let n1314: ZW = zw_cellmix_b(272u64, n960, 668265263u64);
    let n1315: ZW = zw_add(n1287, n1313);
    let n1316: ZW = zw_add(n1288, n1314);
    let n1317: ZW = zw_cellmix_n(280u64, n964, 1542469173u64);
    let n1318: ZW = zw_cellmix_n(280u64, n964, 668265263u64);
    let n1319: ZW = zw_add(n1315, n1317);
    let n1320: ZW = zw_add(n1316, n1318);
    let n1321: ZW = zw_cellmix_n(281u64, n962, 1542469173u64);
    let n1322: ZW = zw_cellmix_n(281u64, n962, 668265263u64);
    let n1323: ZW = zw_add(n1319, n1321);
    let n1324: ZW = zw_add(n1320, n1322);
    let n1325: ZW = zw_cellmix_n(239u64, n972, 1542469173u64);
    let n1326: ZW = zw_cellmix_n(239u64, n972, 668265263u64);
    let n1327: ZW = zw_add(n1255, n1325);
    let n1328: ZW = zw_add(n1256, n1326);
    let n1329: ZW = zw_add(n1327, n1261);
    let n1330: ZW = zw_add(n1328, n1262);
    let n1331: ZW = zw_cellmix_b(247u64, n965, 1542469173u64);
    let n1332: ZW = zw_cellmix_b(247u64, n965, 668265263u64);
    let n1333: ZW = zw_add(n1329, n1331);
    let n1334: ZW = zw_add(n1330, n1332);
    let n1335: ZW = zw_add(n1333, n1269);
    let n1336: ZW = zw_add(n1334, n1270);
    let n1337: ZW = zw_add(n1335, n1273);
    let n1338: ZW = zw_add(n1336, n1274);
    let n1339: ZW = zw_add(n1337, n1277);
    let n1340: ZW = zw_add(n1338, n1278);
    let n1341: ZW = zw_add(n1339, n1281);
    let n1342: ZW = zw_add(n1340, n1282);
    let n1343: ZW = zw_add(n1341, n1285);
    let n1344: ZW = zw_add(n1342, n1286);
    let n1345: ZW = zw_add(n1343, n1289);
    let n1346: ZW = zw_add(n1344, n1290);
    let n1347: ZW = zw_cellmix_n(280u64, n976, 1542469173u64);
    let n1348: ZW = zw_cellmix_n(280u64, n976, 668265263u64);
    let n1349: ZW = zw_add(n1345, n1347);
    let n1350: ZW = zw_add(n1346, n1348);
    let n1351: ZW = zw_cellmix_n(281u64, n974, 1542469173u64);
    let n1352: ZW = zw_cellmix_n(281u64, n974, 668265263u64);
    let n1353: ZW = zw_add(n1349, n1351);
    let n1354: ZW = zw_add(n1350, n1352);
    let n1355: ZW = zw_add(n1343, n1301);
    let n1356: ZW = zw_add(n1344, n1302);
    let n1357: ZW = zw_cellmix_n(280u64, n984, 1542469173u64);
    let n1358: ZW = zw_cellmix_n(280u64, n984, 668265263u64);
    let n1359: ZW = zw_add(n1355, n1357);
    let n1360: ZW = zw_add(n1356, n1358);
    let n1361: ZW = zw_cellmix_n(281u64, n982, 1542469173u64);
    let n1362: ZW = zw_cellmix_n(281u64, n982, 668265263u64);
    let n1363: ZW = zw_add(n1359, n1361);
    let n1364: ZW = zw_add(n1360, n1362);
    let n1365: ZW = zw_add(n1343, n1313);
    let n1366: ZW = zw_add(n1344, n1314);
    let n1367: ZW = zw_cellmix_n(280u64, n992, 1542469173u64);
    let n1368: ZW = zw_cellmix_n(280u64, n992, 668265263u64);
    let n1369: ZW = zw_add(n1365, n1367);
    let n1370: ZW = zw_add(n1366, n1368);
    let n1371: ZW = zw_cellmix_n(281u64, n990, 1542469173u64);
    let n1372: ZW = zw_cellmix_n(281u64, n990, 668265263u64);
    let n1373: ZW = zw_add(n1369, n1371);
    let n1374: ZW = zw_add(n1370, n1372);
    let n1375: ZW = zw_cellmix_n(20u64, n1012, 1542469173u64);
    let n1376: ZW = zw_cellmix_n(20u64, n1012, 668265263u64);
    let n1377: ZW = zw_add(n1237, n1375);
    let n1378: ZW = zw_add(n1238, n1376);
    let n1379: ZW = zw_cellmix_b(41u64, n1013, 1542469173u64);
    let n1380: ZW = zw_cellmix_b(41u64, n1013, 668265263u64);
    let n1381: ZW = zw_add(n1377, n1379);
    let n1382: ZW = zw_add(n1378, n1380);
    let n1383: ZW = zw_cellmix_n(234u64, n1014, 1542469173u64);
    let n1384: ZW = zw_cellmix_n(234u64, n1014, 668265263u64);
    let n1385: ZW = zw_add(n1381, n1383);
    let n1386: ZW = zw_add(n1382, n1384);
    let n1387: ZW = zw_cellmix_n(236u64, n1015, 1542469173u64);
    let n1388: ZW = zw_cellmix_n(236u64, n1015, 668265263u64);
    let n1389: ZW = zw_add(n1385, n1387);
    let n1390: ZW = zw_add(n1386, n1388);
    let n1391: ZW = zw_cellmix_n(237u64, n1016, 1542469173u64);
    let n1392: ZW = zw_cellmix_n(237u64, n1016, 668265263u64);
    let n1393: ZW = zw_add(n1389, n1391);
    let n1394: ZW = zw_add(n1390, n1392);
    let n1395: ZW = zw_add(n1393, n1257);
    let n1396: ZW = zw_add(n1394, n1258);
    let n1397: ZW = zw_cellmix_b(246u64, n993, 1542469173u64);
    let n1398: ZW = zw_cellmix_b(246u64, n993, 668265263u64);
    let n1399: ZW = zw_add(n1395, n1397);
    let n1400: ZW = zw_add(n1396, n1398);
    let n1401: ZW = zw_add(n1399, n1265);
    let n1402: ZW = zw_add(n1400, n1266);
    let n1403: ZW = zw_cellmix_n(253u64, n1025, 1542469173u64);
    let n1404: ZW = zw_cellmix_n(253u64, n1025, 668265263u64);
    let n1405: ZW = zw_add(n1401, n1403);
    let n1406: ZW = zw_add(n1402, n1404);
    let n1407: ZW = zw_cellmix_n(268u64, n1017, 1542469173u64);
    let n1408: ZW = zw_cellmix_n(268u64, n1017, 668265263u64);
    let n1409: ZW = zw_add(n1405, n1407);
    let n1410: ZW = zw_add(n1406, n1408);
    let n1411: ZW = zw_cellmix_n(269u64, n1018, 1542469173u64);
    let n1412: ZW = zw_cellmix_n(269u64, n1018, 668265263u64);
    let n1413: ZW = zw_add(n1409, n1411);
    let n1414: ZW = zw_add(n1410, n1412);
    let n1415: ZW = zw_cellmix_n(270u64, n1019, 1542469173u64);
    let n1416: ZW = zw_cellmix_n(270u64, n1019, 668265263u64);
    let n1417: ZW = zw_add(n1413, n1415);
    let n1418: ZW = zw_add(n1414, n1416);
    let n1419: ZW = zw_cellmix_n(271u64, n1020, 1542469173u64);
    let n1420: ZW = zw_cellmix_n(271u64, n1020, 668265263u64);
    let n1421: ZW = zw_add(n1417, n1419);
    let n1422: ZW = zw_add(n1418, n1420);
    let n1423: ZW = zw_add(n1421, n1289);
    let n1424: ZW = zw_add(n1422, n1290);
    let n1425: ZW = zw_cellmix_n(280u64, n1026, 1542469173u64);
    let n1426: ZW = zw_cellmix_n(280u64, n1026, 668265263u64);
    let n1427: ZW = zw_add(n1423, n1425);
    let n1428: ZW = zw_add(n1424, n1426);
    let n1429: ZW = zw_cellmix_n(281u64, n1022, 1542469173u64);
    let n1430: ZW = zw_cellmix_n(281u64, n1022, 668265263u64);
    let n1431: ZW = zw_add(n1427, n1429);
    let n1432: ZW = zw_add(n1428, n1430);
    let n1433: ZW = zw_cellmix_n(269u64, n1035, 1542469173u64);
    let n1434: ZW = zw_cellmix_n(269u64, n1035, 668265263u64);
    let n1435: ZW = zw_add(n1409, n1433);
    let n1436: ZW = zw_add(n1410, n1434);
    let n1437: ZW = zw_cellmix_n(270u64, n1036, 1542469173u64);
    let n1438: ZW = zw_cellmix_n(270u64, n1036, 668265263u64);
    let n1439: ZW = zw_add(n1435, n1437);
    let n1440: ZW = zw_add(n1436, n1438);
    let n1441: ZW = zw_add(n1439, n1419);
    let n1442: ZW = zw_add(n1440, n1420);
    let n1443: ZW = zw_add(n1441, n1301);
    let n1444: ZW = zw_add(n1442, n1302);
    let n1445: ZW = zw_cellmix_n(280u64, n1040, 1542469173u64);
    let n1446: ZW = zw_cellmix_n(280u64, n1040, 668265263u64);
    let n1447: ZW = zw_add(n1443, n1445);
    let n1448: ZW = zw_add(n1444, n1446);
    let n1449: ZW = zw_cellmix_n(281u64, n1038, 1542469173u64);
    let n1450: ZW = zw_cellmix_n(281u64, n1038, 668265263u64);
    let n1451: ZW = zw_add(n1447, n1449);
    let n1452: ZW = zw_add(n1448, n1450);
    let n1453: ZW = zw_cellmix_n(270u64, n1047, 1542469173u64);
    let n1454: ZW = zw_cellmix_n(270u64, n1047, 668265263u64);
    let n1455: ZW = zw_add(n1435, n1453);
    let n1456: ZW = zw_add(n1436, n1454);
    let n1457: ZW = zw_add(n1455, n1419);
    let n1458: ZW = zw_add(n1456, n1420);
    let n1459: ZW = zw_add(n1457, n1313);
    let n1460: ZW = zw_add(n1458, n1314);
    let n1461: ZW = zw_cellmix_n(280u64, n1051, 1542469173u64);
    let n1462: ZW = zw_cellmix_n(280u64, n1051, 668265263u64);
    let n1463: ZW = zw_add(n1459, n1461);
    let n1464: ZW = zw_add(n1460, n1462);
    let n1465: ZW = zw_cellmix_n(281u64, n1049, 1542469173u64);
    let n1466: ZW = zw_cellmix_n(281u64, n1049, 668265263u64);
    let n1467: ZW = zw_add(n1463, n1465);
    let n1468: ZW = zw_add(n1464, n1466);
    let n1469: ZW = zw_cellmix_n(268u64, n1065, 1542469173u64);
    let n1470: ZW = zw_cellmix_n(268u64, n1065, 668265263u64);
    let n1471: ZW = zw_add(n1405, n1469);
    let n1472: ZW = zw_add(n1406, n1470);
    let n1473: ZW = zw_cellmix_n(269u64, n1066, 1542469173u64);
    let n1474: ZW = zw_cellmix_n(269u64, n1066, 668265263u64);
    let n1475: ZW = zw_add(n1471, n1473);
    let n1476: ZW = zw_add(n1472, n1474);
    let n1477: ZW = zw_cellmix_n(270u64, n1067, 1542469173u64);
    let n1478: ZW = zw_cellmix_n(270u64, n1067, 668265263u64);
    let n1479: ZW = zw_add(n1475, n1477);
    let n1480: ZW = zw_add(n1476, n1478);
    let n1481: ZW = zw_cellmix_n(271u64, n1068, 1542469173u64);
    let n1482: ZW = zw_cellmix_n(271u64, n1068, 668265263u64);
    let n1483: ZW = zw_add(n1479, n1481);
    let n1484: ZW = zw_add(n1480, n1482);
    let n1485: ZW = zw_add(n1483, n1289);
    let n1486: ZW = zw_add(n1484, n1290);
    let n1487: ZW = zw_cellmix_n(280u64, n1072, 1542469173u64);
    let n1488: ZW = zw_cellmix_n(280u64, n1072, 668265263u64);
    let n1489: ZW = zw_add(n1485, n1487);
    let n1490: ZW = zw_add(n1486, n1488);
    let n1491: ZW = zw_cellmix_n(281u64, n1070, 1542469173u64);
    let n1492: ZW = zw_cellmix_n(281u64, n1070, 668265263u64);
    let n1493: ZW = zw_add(n1489, n1491);
    let n1494: ZW = zw_add(n1490, n1492);
    let n1495: ZW = zw_add(n1471, n1433);
    let n1496: ZW = zw_add(n1472, n1434);
    let n1497: ZW = zw_add(n1495, n1437);
    let n1498: ZW = zw_add(n1496, n1438);
    let n1499: ZW = zw_add(n1497, n1481);
    let n1500: ZW = zw_add(n1498, n1482);
    let n1501: ZW = zw_add(n1499, n1301);
    let n1502: ZW = zw_add(n1500, n1302);
    let n1503: ZW = zw_cellmix_n(280u64, n1080, 1542469173u64);
    let n1504: ZW = zw_cellmix_n(280u64, n1080, 668265263u64);
    let n1505: ZW = zw_add(n1501, n1503);
    let n1506: ZW = zw_add(n1502, n1504);
    let n1507: ZW = zw_cellmix_n(281u64, n1078, 1542469173u64);
    let n1508: ZW = zw_cellmix_n(281u64, n1078, 668265263u64);
    let n1509: ZW = zw_add(n1505, n1507);
    let n1510: ZW = zw_add(n1506, n1508);
    let n1511: ZW = zw_add(n1495, n1453);
    let n1512: ZW = zw_add(n1496, n1454);
    let n1513: ZW = zw_add(n1511, n1481);
    let n1514: ZW = zw_add(n1512, n1482);
    let n1515: ZW = zw_add(n1513, n1313);
    let n1516: ZW = zw_add(n1514, n1314);
    let n1517: ZW = zw_cellmix_n(280u64, n1088, 1542469173u64);
    let n1518: ZW = zw_cellmix_n(280u64, n1088, 668265263u64);
    let n1519: ZW = zw_add(n1515, n1517);
    let n1520: ZW = zw_add(n1516, n1518);
    let n1521: ZW = zw_cellmix_n(281u64, n1086, 1542469173u64);
    let n1522: ZW = zw_cellmix_n(281u64, n1086, 668265263u64);
    let n1523: ZW = zw_add(n1519, n1521);
    let n1524: ZW = zw_add(n1520, n1522);
    let n1525: ZW = zw_cellmix_n(271u64, n1093, 1542469173u64);
    let n1526: ZW = zw_cellmix_n(271u64, n1093, 668265263u64);
    let n1527: ZW = zw_add(n1479, n1525);
    let n1528: ZW = zw_add(n1480, n1526);
    let n1529: ZW = zw_add(n1527, n1289);
    let n1530: ZW = zw_add(n1528, n1290);
    let n1531: ZW = zw_add(n1529, n1487);
    let n1532: ZW = zw_add(n1530, n1488);
    let n1533: ZW = zw_cellmix_n(281u64, n1094, 1542469173u64);
    let n1534: ZW = zw_cellmix_n(281u64, n1094, 668265263u64);
    let n1535: ZW = zw_add(n1531, n1533);
    let n1536: ZW = zw_add(n1532, n1534);
    let n1537: ZW = zw_add(n1497, n1525);
    let n1538: ZW = zw_add(n1498, n1526);
    let n1539: ZW = zw_add(n1537, n1301);
    let n1540: ZW = zw_add(n1538, n1302);
    let n1541: ZW = zw_add(n1539, n1503);
    let n1542: ZW = zw_add(n1540, n1504);
    let n1543: ZW = zw_cellmix_n(281u64, n1097, 1542469173u64);
    let n1544: ZW = zw_cellmix_n(281u64, n1097, 668265263u64);
    let n1545: ZW = zw_add(n1541, n1543);
    let n1546: ZW = zw_add(n1542, n1544);
    let n1547: ZW = zw_add(n1511, n1525);
    let n1548: ZW = zw_add(n1512, n1526);
    let n1549: ZW = zw_add(n1547, n1313);
    let n1550: ZW = zw_add(n1548, n1314);
    let n1551: ZW = zw_add(n1549, n1517);
    let n1552: ZW = zw_add(n1550, n1518);
    let n1553: ZW = zw_cellmix_n(281u64, n1100, 1542469173u64);
    let n1554: ZW = zw_cellmix_n(281u64, n1100, 668265263u64);
    let n1555: ZW = zw_add(n1551, n1553);
    let n1556: ZW = zw_add(n1552, n1554);
    let n1557: ZW = zw_add(n1393, n1325);
    let n1558: ZW = zw_add(n1394, n1326);
    let n1559: ZW = zw_add(n1557, n1397);
    let n1560: ZW = zw_add(n1558, n1398);
    let n1561: ZW = zw_add(n1559, n1331);
    let n1562: ZW = zw_add(n1560, n1332);
    let n1563: ZW = zw_add(n1561, n1403);
    let n1564: ZW = zw_add(n1562, n1404);
    let n1565: ZW = zw_add(n1563, n1407);
    let n1566: ZW = zw_add(n1564, n1408);
    let n1567: ZW = zw_add(n1565, n1411);
    let n1568: ZW = zw_add(n1566, n1412);
    let n1569: ZW = zw_add(n1567, n1415);
    let n1570: ZW = zw_add(n1568, n1416);
    let n1571: ZW = zw_add(n1569, n1419);
    let n1572: ZW = zw_add(n1570, n1420);
    let n1573: ZW = zw_add(n1571, n1289);
    let n1574: ZW = zw_add(n1572, n1290);
    let n1575: ZW = zw_cellmix_n(280u64, n1108, 1542469173u64);
    let n1576: ZW = zw_cellmix_n(280u64, n1108, 668265263u64);
    let n1577: ZW = zw_add(n1573, n1575);
    let n1578: ZW = zw_add(n1574, n1576);
    let n1579: ZW = zw_cellmix_n(281u64, n1106, 1542469173u64);
    let n1580: ZW = zw_cellmix_n(281u64, n1106, 668265263u64);
    let n1581: ZW = zw_add(n1577, n1579);
    let n1582: ZW = zw_add(n1578, n1580);
    let n1583: ZW = zw_add(n1565, n1433);
    let n1584: ZW = zw_add(n1566, n1434);
    let n1585: ZW = zw_add(n1583, n1437);
    let n1586: ZW = zw_add(n1584, n1438);
    let n1587: ZW = zw_add(n1585, n1419);
    let n1588: ZW = zw_add(n1586, n1420);
    let n1589: ZW = zw_add(n1587, n1301);
    let n1590: ZW = zw_add(n1588, n1302);
    let n1591: ZW = zw_cellmix_n(280u64, n1116, 1542469173u64);
    let n1592: ZW = zw_cellmix_n(280u64, n1116, 668265263u64);
    let n1593: ZW = zw_add(n1589, n1591);
    let n1594: ZW = zw_add(n1590, n1592);
    let n1595: ZW = zw_cellmix_n(281u64, n1114, 1542469173u64);
    let n1596: ZW = zw_cellmix_n(281u64, n1114, 668265263u64);
    let n1597: ZW = zw_add(n1593, n1595);
    let n1598: ZW = zw_add(n1594, n1596);
    let n1599: ZW = zw_add(n1583, n1453);
    let n1600: ZW = zw_add(n1584, n1454);
    let n1601: ZW = zw_add(n1599, n1419);
    let n1602: ZW = zw_add(n1600, n1420);
    let n1603: ZW = zw_add(n1601, n1313);
    let n1604: ZW = zw_add(n1602, n1314);
    let n1605: ZW = zw_cellmix_n(280u64, n1124, 1542469173u64);
    let n1606: ZW = zw_cellmix_n(280u64, n1124, 668265263u64);
    let n1607: ZW = zw_add(n1603, n1605);
    let n1608: ZW = zw_add(n1604, n1606);
    let n1609: ZW = zw_cellmix_n(281u64, n1122, 1542469173u64);
    let n1610: ZW = zw_cellmix_n(281u64, n1122, 668265263u64);
    let n1611: ZW = zw_add(n1607, n1609);
    let n1612: ZW = zw_add(n1608, n1610);
    let n1613: ZW = zw_add(n1563, n1469);
    let n1614: ZW = zw_add(n1564, n1470);
    let n1615: ZW = zw_add(n1613, n1473);
    let n1616: ZW = zw_add(n1614, n1474);
    let n1617: ZW = zw_add(n1615, n1477);
    let n1618: ZW = zw_add(n1616, n1478);
    let n1619: ZW = zw_add(n1617, n1481);
    let n1620: ZW = zw_add(n1618, n1482);
    let n1621: ZW = zw_add(n1619, n1289);
    let n1622: ZW = zw_add(n1620, n1290);
    let n1623: ZW = zw_cellmix_n(280u64, n1132, 1542469173u64);
    let n1624: ZW = zw_cellmix_n(280u64, n1132, 668265263u64);
    let n1625: ZW = zw_add(n1621, n1623);
    let n1626: ZW = zw_add(n1622, n1624);
    let n1627: ZW = zw_cellmix_n(281u64, n1130, 1542469173u64);
    let n1628: ZW = zw_cellmix_n(281u64, n1130, 668265263u64);
    let n1629: ZW = zw_add(n1625, n1627);
    let n1630: ZW = zw_add(n1626, n1628);
    let n1631: ZW = zw_add(n1613, n1433);
    let n1632: ZW = zw_add(n1614, n1434);
    let n1633: ZW = zw_add(n1631, n1437);
    let n1634: ZW = zw_add(n1632, n1438);
    let n1635: ZW = zw_add(n1633, n1481);
    let n1636: ZW = zw_add(n1634, n1482);
    let n1637: ZW = zw_add(n1635, n1301);
    let n1638: ZW = zw_add(n1636, n1302);
    let n1639: ZW = zw_cellmix_n(280u64, n1140, 1542469173u64);
    let n1640: ZW = zw_cellmix_n(280u64, n1140, 668265263u64);
    let n1641: ZW = zw_add(n1637, n1639);
    let n1642: ZW = zw_add(n1638, n1640);
    let n1643: ZW = zw_cellmix_n(281u64, n1138, 1542469173u64);
    let n1644: ZW = zw_cellmix_n(281u64, n1138, 668265263u64);
    let n1645: ZW = zw_add(n1641, n1643);
    let n1646: ZW = zw_add(n1642, n1644);
    let n1647: ZW = zw_add(n1631, n1453);
    let n1648: ZW = zw_add(n1632, n1454);
    let n1649: ZW = zw_add(n1647, n1481);
    let n1650: ZW = zw_add(n1648, n1482);
    let n1651: ZW = zw_add(n1649, n1313);
    let n1652: ZW = zw_add(n1650, n1314);
    let n1653: ZW = zw_cellmix_n(280u64, n1148, 1542469173u64);
    let n1654: ZW = zw_cellmix_n(280u64, n1148, 668265263u64);
    let n1655: ZW = zw_add(n1651, n1653);
    let n1656: ZW = zw_add(n1652, n1654);
    let n1657: ZW = zw_cellmix_n(281u64, n1146, 1542469173u64);
    let n1658: ZW = zw_cellmix_n(281u64, n1146, 668265263u64);
    let n1659: ZW = zw_add(n1655, n1657);
    let n1660: ZW = zw_add(n1656, n1658);
    let n1661: ZW = zw_add(n1617, n1525);
    let n1662: ZW = zw_add(n1618, n1526);
    let n1663: ZW = zw_add(n1661, n1289);
    let n1664: ZW = zw_add(n1662, n1290);
    let n1665: ZW = zw_add(n1663, n1623);
    let n1666: ZW = zw_add(n1664, n1624);
    let n1667: ZW = zw_cellmix_n(281u64, n1151, 1542469173u64);
    let n1668: ZW = zw_cellmix_n(281u64, n1151, 668265263u64);
    let n1669: ZW = zw_add(n1665, n1667);
    let n1670: ZW = zw_add(n1666, n1668);
    let n1671: ZW = zw_add(n1633, n1525);
    let n1672: ZW = zw_add(n1634, n1526);
    let n1673: ZW = zw_add(n1671, n1301);
    let n1674: ZW = zw_add(n1672, n1302);
    let n1675: ZW = zw_add(n1673, n1639);
    let n1676: ZW = zw_add(n1674, n1640);
    let n1677: ZW = zw_cellmix_n(281u64, n1154, 1542469173u64);
    let n1678: ZW = zw_cellmix_n(281u64, n1154, 668265263u64);
    let n1679: ZW = zw_add(n1675, n1677);
    let n1680: ZW = zw_add(n1676, n1678);
    let n1681: ZW = zw_add(n1647, n1525);
    let n1682: ZW = zw_add(n1648, n1526);
    let n1683: ZW = zw_add(n1681, n1313);
    let n1684: ZW = zw_add(n1682, n1314);
    let n1685: ZW = zw_add(n1683, n1653);
    let n1686: ZW = zw_add(n1684, n1654);
    let n1687: ZW = zw_cellmix_n(281u64, n1157, 1542469173u64);
    let n1688: ZW = zw_cellmix_n(281u64, n1157, 668265263u64);
    let n1689: ZW = zw_add(n1685, n1687);
    let n1690: ZW = zw_add(n1686, n1688);
    let ok_v0_b0: u16 = ALL & zb_holds(n701) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v0_b0: bool = !n80 || !n79 || !n78 || !n77;
    let live_v0_b0: u16 = ALL & zb_holds(n700) & zb_holds(n760);
    let ok_v32_b1: u16 = ALL & zb_holds(n701) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v32_b1: bool = !n80 || !n79 || !n78 || !n77;
    let live_v32_b1: u16 = ALL & zb_holds(n700) & zb_holds(n760);
    let ok_v0_b2: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n821);
    let bd_v0_b2: bool = !n80 || !n79 || !n78 || !n77;
    let live_v0_b2: u16 = ALL & zb_holds(n820);
    let ok_v32_b3: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n821);
    let bd_v32_b3: bool = !n80 || !n79 || !n78 || !n77;
    let live_v32_b3: u16 = ALL & zb_holds(n820);
    let ok_v0_b4: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v0_b4: bool = !n80 || !n79 || !n78 || !n77;
    let live_v0_b4: u16 = ALL & zb_holds(n920);
    let ok_v1_b5: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v1_b5: bool = !n80 || !n79 || !n78 || !n77;
    let live_v1_b5: u16 = ALL & zb_holds(n920);
    let ok_v2_b6: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v2_b6: bool = !n80 || !n79 || !n78 || !n77;
    let live_v2_b6: u16 = ALL & zb_holds(n920);
    let ok_v16_b7: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v16_b7: bool = !n80 || !n79 || !n78 || !n77;
    let live_v16_b7: u16 = ALL & zb_holds(n920);
    let ok_v17_b8: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v17_b8: bool = !n80 || !n79 || !n78 || !n77;
    let live_v17_b8: u16 = ALL & zb_holds(n920);
    let ok_v18_b9: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v18_b9: bool = !n80 || !n79 || !n78 || !n77;
    let live_v18_b9: u16 = ALL & zb_holds(n920);
    let ok_v32_b10: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v32_b10: bool = !n80 || !n79 || !n78 || !n77;
    let live_v32_b10: u16 = ALL & zb_holds(n920);
    let ok_v33_b11: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v33_b11: bool = !n80 || !n79 || !n78 || !n77;
    let live_v33_b11: u16 = ALL & zb_holds(n920);
    let ok_v34_b12: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v34_b12: bool = !n80 || !n79 || !n78 || !n77;
    let live_v34_b12: u16 = ALL & zb_holds(n920);
    let ok_v36_b13: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v36_b13: bool = !n80 || !n79 || !n78 || !n77;
    let live_v36_b13: u16 = ALL & zb_holds(n920);
    let ok_v37_b14: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v37_b14: bool = !n80 || !n79 || !n78 || !n77;
    let live_v37_b14: u16 = ALL & zb_holds(n920);
    let ok_v38_b15: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v38_b15: bool = !n80 || !n79 || !n78 || !n77;
    let live_v38_b15: u16 = ALL & zb_holds(n920);
    let ok_v40_b16: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v40_b16: bool = !n80 || !n79 || !n78 || !n77;
    let live_v40_b16: u16 = ALL & zb_holds(n920);
    let ok_v41_b17: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v41_b17: bool = !n80 || !n79 || !n78 || !n77;
    let live_v41_b17: u16 = ALL & zb_holds(n920);
    let ok_v42_b18: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v42_b18: bool = !n80 || !n79 || !n78 || !n77;
    let live_v42_b18: u16 = ALL & zb_holds(n920);
    let ok_v48_b19: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v48_b19: bool = !n80 || !n79 || !n78 || !n77;
    let live_v48_b19: u16 = ALL & zb_holds(n920);
    let ok_v49_b20: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v49_b20: bool = !n80 || !n79 || !n78 || !n77;
    let live_v49_b20: u16 = ALL & zb_holds(n920);
    let ok_v50_b21: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v50_b21: bool = !n80 || !n79 || !n78 || !n77;
    let live_v50_b21: u16 = ALL & zb_holds(n920);
    let ok_v52_b22: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v52_b22: bool = !n80 || !n79 || !n78 || !n77;
    let live_v52_b22: u16 = ALL & zb_holds(n920);
    let ok_v53_b23: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v53_b23: bool = !n80 || !n79 || !n78 || !n77;
    let live_v53_b23: u16 = ALL & zb_holds(n920);
    let ok_v54_b24: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v54_b24: bool = !n80 || !n79 || !n78 || !n77;
    let live_v54_b24: u16 = ALL & zb_holds(n920);
    let ok_v56_b25: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v56_b25: bool = !n80 || !n79 || !n78 || !n77;
    let live_v56_b25: u16 = ALL & zb_holds(n920);
    let ok_v57_b26: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v57_b26: bool = !n80 || !n79 || !n78 || !n77;
    let live_v57_b26: u16 = ALL & zb_holds(n920);
    let ok_v58_b27: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n921);
    let bd_v58_b27: bool = !n80 || !n79 || !n78 || !n77;
    let live_v58_b27: u16 = ALL & zb_holds(n920);
    let sh0 = KShared0 {
        c87: n776,
        c84: n57,
        c86: n158,
        c85: n86,
    };
    let sh1 = KShared1 {
        c87: n822,
        c39: n823,
        c84: n57,
        c86: n158,
        c85: n86,
        c38: n819,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n57,
        c86: n158,
        c278: n916,
        c279: n917,
        c254: n914,
        c85: n86,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
    let mut take_1_0: u16 = 0;
    let mut take_1_1: u16 = 0;
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
    // 28 distinct button assignments; per outcome they fall
    // into [2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        h1: n1181, h2: n1182,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v32_b1 & (if bd_v32_b1 { ALL } else { !ok_v32_b1 });
    take_0_1 |= live_v32_b1 & ok_v32_b1 & (if bd_v32_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n807,
        c41: n808,
        h1: n1189, h2: n1190,
    };
    // body 1: buttons 0x20, forks 0x0
    sink.o0(32, take_0_1, &sh0, &o0);
    declined |= live_v0_b2 & (if bd_v0_b2 { ALL } else { !ok_v0_b2 });
    take_1_0 |= live_v0_b2 & ok_v0_b2 & (if bd_v0_b2 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        h1: n1209, h2: n1210,
    };
    // body 2: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b3 & (if bd_v32_b3 { ALL } else { !ok_v32_b3 });
    take_1_1 |= live_v32_b3 & ok_v32_b3 & (if bd_v32_b3 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n807,
        h1: n1211, h2: n1212,
    };
    // body 3: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b4 & (if bd_v0_b4 { ALL } else { !ok_v0_b4 });
    take_2_0 |= live_v0_b4 & ok_v0_b4 & (if bd_v0_b4 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n908,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n909,
        c270: r_c270,
        c271: r_c271,
        c236: n910,
        c237: n911,
        c272: n915,
        c239: n912,
        c246: n842,
        c247: n843,
        c280: n931,
        c281: n919,
        c253: n930,
        h1: n1299, h2: n1300,
    };
    // body 4: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b5 & (if bd_v1_b5 { ALL } else { !ok_v1_b5 });
    take_2_1 |= live_v1_b5 & ok_v1_b5 & (if bd_v1_b5 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n908,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n909,
        c270: r_c270,
        c271: r_c271,
        c236: n910,
        c237: n911,
        c272: n944,
        c239: n912,
        c246: n842,
        c247: n843,
        c280: n948,
        c281: n946,
        c253: n930,
        h1: n1311, h2: n1312,
    };
    // body 5: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b6 & (if bd_v2_b6 { ALL } else { !ok_v2_b6 });
    take_2_2 |= live_v2_b6 & ok_v2_b6 & (if bd_v2_b6 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n908,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n909,
        c270: r_c270,
        c271: r_c271,
        c236: n910,
        c237: n911,
        c272: n960,
        c239: n912,
        c246: n842,
        c247: n843,
        c280: n964,
        c281: n962,
        c253: n930,
        h1: n1323, h2: n1324,
    };
    // body 6: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b7 & (if bd_v16_b7 { ALL } else { !ok_v16_b7 });
    take_2_3 |= live_v16_b7 & ok_v16_b7 & (if bd_v16_b7 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n908,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n909,
        c270: r_c270,
        c271: r_c271,
        c236: n910,
        c237: n911,
        c272: n915,
        c239: n972,
        c246: n842,
        c247: n965,
        c280: n976,
        c281: n974,
        c253: n930,
        h1: n1353, h2: n1354,
    };
    // body 7: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b8 & (if bd_v17_b8 { ALL } else { !ok_v17_b8 });
    take_2_4 |= live_v17_b8 & ok_v17_b8 & (if bd_v17_b8 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n908,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n909,
        c270: r_c270,
        c271: r_c271,
        c236: n910,
        c237: n911,
        c272: n944,
        c239: n972,
        c246: n842,
        c247: n965,
        c280: n984,
        c281: n982,
        c253: n930,
        h1: n1363, h2: n1364,
    };
    // body 8: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b9 & (if bd_v18_b9 { ALL } else { !ok_v18_b9 });
    take_2_5 |= live_v18_b9 & ok_v18_b9 & (if bd_v18_b9 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n908,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n909,
        c270: r_c270,
        c271: r_c271,
        c236: n910,
        c237: n911,
        c272: n960,
        c239: n972,
        c246: n842,
        c247: n965,
        c280: n992,
        c281: n990,
        c253: n930,
        h1: n1373, h2: n1374,
    };
    // body 9: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b10 & (if bd_v32_b10 { ALL } else { !ok_v32_b10 });
    take_2_6 |= live_v32_b10 & ok_v32_b10 & (if bd_v32_b10 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1017,
        c269: n1018,
        c234: n1014,
        c270: n1019,
        c271: n1020,
        c236: n1015,
        c237: n1016,
        c272: n915,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1026,
        c281: n1022,
        c253: n1025,
        h1: n1431, h2: n1432,
    };
    // body 10: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b11 & (if bd_v33_b11 { ALL } else { !ok_v33_b11 });
    take_2_7 |= live_v33_b11 & ok_v33_b11 & (if bd_v33_b11 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1017,
        c269: n1035,
        c234: n1014,
        c270: n1036,
        c271: n1020,
        c236: n1015,
        c237: n1016,
        c272: n944,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1040,
        c281: n1038,
        c253: n1025,
        h1: n1451, h2: n1452,
    };
    // body 11: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b12 & (if bd_v34_b12 { ALL } else { !ok_v34_b12 });
    take_2_8 |= live_v34_b12 & ok_v34_b12 & (if bd_v34_b12 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1017,
        c269: n1035,
        c234: n1014,
        c270: n1047,
        c271: n1020,
        c236: n1015,
        c237: n1016,
        c272: n960,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1051,
        c281: n1049,
        c253: n1025,
        h1: n1467, h2: n1468,
    };
    // body 12: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b13 & (if bd_v36_b13 { ALL } else { !ok_v36_b13 });
    take_2_9 |= live_v36_b13 & ok_v36_b13 & (if bd_v36_b13 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1066,
        c234: n1014,
        c270: n1067,
        c271: n1068,
        c236: n1015,
        c237: n1016,
        c272: n915,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1072,
        c281: n1070,
        c253: n1025,
        h1: n1493, h2: n1494,
    };
    // body 13: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b14 & (if bd_v37_b14 { ALL } else { !ok_v37_b14 });
    take_2_10 |= live_v37_b14 & ok_v37_b14 & (if bd_v37_b14 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1036,
        c271: n1068,
        c236: n1015,
        c237: n1016,
        c272: n944,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1080,
        c281: n1078,
        c253: n1025,
        h1: n1509, h2: n1510,
    };
    // body 14: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b15 & (if bd_v38_b15 { ALL } else { !ok_v38_b15 });
    take_2_11 |= live_v38_b15 & ok_v38_b15 & (if bd_v38_b15 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1047,
        c271: n1068,
        c236: n1015,
        c237: n1016,
        c272: n960,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1088,
        c281: n1086,
        c253: n1025,
        h1: n1523, h2: n1524,
    };
    // body 15: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b16 & (if bd_v40_b16 { ALL } else { !ok_v40_b16 });
    take_2_12 |= live_v40_b16 & ok_v40_b16 & (if bd_v40_b16 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1066,
        c234: n1014,
        c270: n1067,
        c271: n1093,
        c236: n1015,
        c237: n1016,
        c272: n915,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1072,
        c281: n1094,
        c253: n1025,
        h1: n1535, h2: n1536,
    };
    // body 16: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b17 & (if bd_v41_b17 { ALL } else { !ok_v41_b17 });
    take_2_13 |= live_v41_b17 & ok_v41_b17 & (if bd_v41_b17 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1036,
        c271: n1093,
        c236: n1015,
        c237: n1016,
        c272: n944,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1080,
        c281: n1097,
        c253: n1025,
        h1: n1545, h2: n1546,
    };
    // body 17: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b18 & (if bd_v42_b18 { ALL } else { !ok_v42_b18 });
    take_2_14 |= live_v42_b18 & ok_v42_b18 & (if bd_v42_b18 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1047,
        c271: n1093,
        c236: n1015,
        c237: n1016,
        c272: n960,
        c239: n912,
        c246: n993,
        c247: n843,
        c280: n1088,
        c281: n1100,
        c253: n1025,
        h1: n1555, h2: n1556,
    };
    // body 18: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b19 & (if bd_v48_b19 { ALL } else { !ok_v48_b19 });
    take_2_15 |= live_v48_b19 & ok_v48_b19 & (if bd_v48_b19 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1017,
        c269: n1018,
        c234: n1014,
        c270: n1019,
        c271: n1020,
        c236: n1015,
        c237: n1016,
        c272: n915,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1108,
        c281: n1106,
        c253: n1025,
        h1: n1581, h2: n1582,
    };
    // body 19: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b20 & (if bd_v49_b20 { ALL } else { !ok_v49_b20 });
    take_2_16 |= live_v49_b20 & ok_v49_b20 & (if bd_v49_b20 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1017,
        c269: n1035,
        c234: n1014,
        c270: n1036,
        c271: n1020,
        c236: n1015,
        c237: n1016,
        c272: n944,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1116,
        c281: n1114,
        c253: n1025,
        h1: n1597, h2: n1598,
    };
    // body 20: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b21 & (if bd_v50_b21 { ALL } else { !ok_v50_b21 });
    take_2_17 |= live_v50_b21 & ok_v50_b21 & (if bd_v50_b21 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1017,
        c269: n1035,
        c234: n1014,
        c270: n1047,
        c271: n1020,
        c236: n1015,
        c237: n1016,
        c272: n960,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1124,
        c281: n1122,
        c253: n1025,
        h1: n1611, h2: n1612,
    };
    // body 21: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b22 & (if bd_v52_b22 { ALL } else { !ok_v52_b22 });
    take_2_18 |= live_v52_b22 & ok_v52_b22 & (if bd_v52_b22 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1066,
        c234: n1014,
        c270: n1067,
        c271: n1068,
        c236: n1015,
        c237: n1016,
        c272: n915,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1132,
        c281: n1130,
        c253: n1025,
        h1: n1629, h2: n1630,
    };
    // body 22: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b23 & (if bd_v53_b23 { ALL } else { !ok_v53_b23 });
    take_2_19 |= live_v53_b23 & ok_v53_b23 & (if bd_v53_b23 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1036,
        c271: n1068,
        c236: n1015,
        c237: n1016,
        c272: n944,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1140,
        c281: n1138,
        c253: n1025,
        h1: n1645, h2: n1646,
    };
    // body 23: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b24 & (if bd_v54_b24 { ALL } else { !ok_v54_b24 });
    take_2_20 |= live_v54_b24 & ok_v54_b24 & (if bd_v54_b24 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1047,
        c271: n1068,
        c236: n1015,
        c237: n1016,
        c272: n960,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1148,
        c281: n1146,
        c253: n1025,
        h1: n1659, h2: n1660,
    };
    // body 24: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b25 & (if bd_v56_b25 { ALL } else { !ok_v56_b25 });
    take_2_21 |= live_v56_b25 & ok_v56_b25 & (if bd_v56_b25 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1066,
        c234: n1014,
        c270: n1067,
        c271: n1093,
        c236: n1015,
        c237: n1016,
        c272: n915,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1132,
        c281: n1151,
        c253: n1025,
        h1: n1669, h2: n1670,
    };
    // body 25: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b26 & (if bd_v57_b26 { ALL } else { !ok_v57_b26 });
    take_2_22 |= live_v57_b26 & ok_v57_b26 & (if bd_v57_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1036,
        c271: n1093,
        c236: n1015,
        c237: n1016,
        c272: n944,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1140,
        c281: n1154,
        c253: n1025,
        h1: n1679, h2: n1680,
    };
    // body 26: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b27 & (if bd_v58_b27 { ALL } else { !ok_v58_b27 });
    take_2_23 |= live_v58_b27 & ok_v58_b27 & (if bd_v58_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1012,
        c41: n1013,
        c268: n1065,
        c269: n1035,
        c234: n1014,
        c270: n1047,
        c271: n1093,
        c236: n1015,
        c237: n1016,
        c272: n960,
        c239: n972,
        c246: n993,
        c247: n965,
        c280: n1148,
        c281: n1157,
        c253: n1025,
        h1: n1689, h2: n1690,
    };
    // body 27: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined
}
