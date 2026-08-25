// GENERATED from a TRACED frame (shape 1). Do not edit.
//
// One input shape, 3 output shapes, 52 distinct button
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
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
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
    pub c87: ZN,
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(kv.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if kv.c38.known & (1 << i) != 0 {
                AV::Bool(kv.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
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

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32] }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0], self.org);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1], self.org);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2], self.org);
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
    // Engine-carried origin metadata for this slice's lanes
    // (empty = untracked); see `Rt2::origin`.
    let org: &[u32] = if b.origin.is_empty() { &[] } else { &b.origin[lo..lo + n] };
    let mut sink = Append { accs, seen, n, org };
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
    let n152: ZB = zn_ge(r_c237, zn_splat(P8::from_raw(65536i32)));
    let n153: ZN = zsel_n(n151, zn_splat(P8::from_raw(65536i32)), r_c237);
    let n154: ZB = zn_gt(r_c239, zn_splat(P8::from_raw(0i32)));
    let n155: ZB = zn_le(r_c239, zn_splat(P8::from_raw(0i32)));
    let n156: ZN = zn_sub(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n157: ZN = zsel_n(n154, n156, r_c239);
    let n158: ZB = zn_gt(r_c236, zn_splat(P8::from_raw(0i32)));
    let n159: ZB = zn_le(r_c236, zn_splat(P8::from_raw(0i32)));
    let n160: ZN = zsel_n(n85, n70, r_c86);
    let n161: ZN = zsel_n(n58, n160, r_c86);
    let n162: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c253);
    let n163: ZN = zn_add(n98, n162);
    let n164: ZB = zn_tile_flag_at(g.cache, g.cart, n163, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n165: ZN = zn_add(r_c253, n98);
    let n166: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n165);
    let n167: ZN = zn_add(n98, n166);
    let n168: ZB = zn_tile_flag_at(g.cache, g.cart, n167, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n169: ZN = zn_add(n98, n165);
    let n170: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n169);
    let n171: ZN = zn_add(n98, n170);
    let n172: ZB = zn_tile_flag_at(g.cache, g.cart, n171, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n173: ZN = zn_add(n98, n169);
    let n174: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n173);
    let n175: ZN = zn_add(n98, n174);
    let n176: ZB = zn_tile_flag_at(g.cache, g.cart, n175, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n177: ZN = zn_add(n98, n173);
    let n178: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n177);
    let n179: ZN = zn_add(n98, n178);
    let n180: ZB = zn_tile_flag_at(g.cache, g.cart, n179, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n181: ZN = zn_add(n98, n177);
    let n182: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n181);
    let n183: ZN = zn_add(n98, n182);
    let n184: ZB = zn_tile_flag_at(g.cache, g.cart, n183, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n185: ZN = zn_add(n98, n181);
    let n186: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n185);
    let n187: ZN = zn_add(n98, n186);
    let n188: ZB = zn_tile_flag_at(g.cache, g.cart, n187, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n189: ZN = zn_add(n98, n185);
    let n190: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n189);
    let n191: ZN = zn_add(n98, n190);
    let n192: ZB = zn_tile_flag_at(g.cache, g.cart, n191, n101, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n193: ZN = zn_add(n98, n189);
    let n194: ZN = zsel_n(n192, n189, n193);
    let n195: ZN = zsel_n(n192, zn_splat(P8::from_raw(0i32)), r_c280);
    let n196: ZB = zb_or(n109, n192);
    let n197: ZN = zsel_n(n108, n189, n194);
    let n198: ZN = zsel_n(n108, r_c280, n195);
    let n199: ZB = zb_or(n108, n196);
    let n200: ZN = zsel_n(n188, n185, n197);
    let n201: ZN = zsel_n(n188, zn_splat(P8::from_raw(0i32)), n198);
    let n202: ZB = zb_or(n188, n199);
    let n203: ZN = zsel_n(n107, n185, n200);
    let n204: ZN = zsel_n(n107, r_c280, n201);
    let n205: ZB = zb_or(n107, n202);
    let n206: ZN = zsel_n(n184, n181, n203);
    let n207: ZN = zsel_n(n184, zn_splat(P8::from_raw(0i32)), n204);
    let n208: ZB = zb_or(n184, n205);
    let n209: ZN = zsel_n(n106, n181, n206);
    let n210: ZN = zsel_n(n106, r_c280, n207);
    let n211: ZB = zb_or(n106, n208);
    let n212: ZN = zsel_n(n180, n177, n209);
    let n213: ZN = zsel_n(n180, zn_splat(P8::from_raw(0i32)), n210);
    let n214: ZB = zb_or(n180, n211);
    let n215: ZN = zsel_n(n105, n177, n212);
    let n216: ZN = zsel_n(n105, r_c280, n213);
    let n217: ZB = zb_or(n105, n214);
    let n218: ZN = zsel_n(n176, n173, n215);
    let n219: ZN = zsel_n(n176, zn_splat(P8::from_raw(0i32)), n216);
    let n220: ZB = zb_or(n176, n217);
    let n221: ZN = zsel_n(n104, n173, n218);
    let n222: ZN = zsel_n(n104, r_c280, n219);
    let n223: ZB = zb_or(n104, n220);
    let n224: ZN = zsel_n(n172, n169, n221);
    let n225: ZN = zsel_n(n172, zn_splat(P8::from_raw(0i32)), n222);
    let n226: ZB = zb_or(n172, n223);
    let n227: ZN = zsel_n(n103, n169, n224);
    let n228: ZN = zsel_n(n103, r_c280, n225);
    let n229: ZB = zb_or(n103, n226);
    let n230: ZN = zsel_n(n168, n165, n227);
    let n231: ZN = zsel_n(n168, zn_splat(P8::from_raw(0i32)), n228);
    let n232: ZB = zb_or(n168, n229);
    let n233: ZN = zsel_n(n102, n165, n230);
    let n234: ZN = zsel_n(n102, r_c280, n231);
    let n235: ZB = zb_or(n102, n232);
    let n236: ZN = zsel_n(n164, r_c253, n233);
    let n237: ZN = zsel_n(n164, zn_splat(P8::from_raw(0i32)), n234);
    let n238: ZB = zb_or(n164, n235);
    let n239: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n236);
    let n240: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n239);
    let n241: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n118, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n242: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n243: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n126, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n244: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n130, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n245: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n134, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n246: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n138, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n247: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n142, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n248: ZB = zn_tile_flag_at(g.cache, g.cart, n240, n146, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n249: ZB = zb_and(n148, n238);
    let n250: ZN = zsel_n(n248, n143, n147);
    let n251: ZN = zsel_n(n248, zn_splat(P8::from_raw(0i32)), r_c281);
    let n252: ZB = zsel_b(n248, n238, n249);
    let n253: ZN = zsel_n(n144, n143, n250);
    let n254: ZN = zsel_n(n144, r_c281, n251);
    let n255: ZB = zsel_b(n144, n238, n252);
    let n256: ZN = zsel_n(n247, n139, n253);
    let n257: ZN = zsel_n(n247, zn_splat(P8::from_raw(0i32)), n254);
    let n258: ZB = zsel_b(n247, n238, n255);
    let n259: ZN = zsel_n(n140, n139, n256);
    let n260: ZN = zsel_n(n140, r_c281, n257);
    let n261: ZB = zsel_b(n140, n238, n258);
    let n262: ZN = zsel_n(n246, n135, n259);
    let n263: ZN = zsel_n(n246, zn_splat(P8::from_raw(0i32)), n260);
    let n264: ZB = zsel_b(n246, n238, n261);
    let n265: ZN = zsel_n(n136, n135, n262);
    let n266: ZN = zsel_n(n136, r_c281, n263);
    let n267: ZB = zsel_b(n136, n238, n264);
    let n268: ZN = zsel_n(n245, n131, n265);
    let n269: ZN = zsel_n(n245, zn_splat(P8::from_raw(0i32)), n266);
    let n270: ZB = zsel_b(n245, n238, n267);
    let n271: ZN = zsel_n(n132, n131, n268);
    let n272: ZN = zsel_n(n132, r_c281, n269);
    let n273: ZB = zsel_b(n132, n238, n270);
    let n274: ZN = zsel_n(n244, n127, n271);
    let n275: ZN = zsel_n(n244, zn_splat(P8::from_raw(0i32)), n272);
    let n276: ZB = zsel_b(n244, n238, n273);
    let n277: ZN = zsel_n(n128, n127, n274);
    let n278: ZN = zsel_n(n128, r_c281, n275);
    let n279: ZB = zsel_b(n128, n238, n276);
    let n280: ZN = zsel_n(n243, n123, n277);
    let n281: ZN = zsel_n(n243, zn_splat(P8::from_raw(0i32)), n278);
    let n282: ZB = zsel_b(n243, n238, n279);
    let n283: ZN = zsel_n(n124, n123, n280);
    let n284: ZN = zsel_n(n124, r_c281, n281);
    let n285: ZB = zsel_b(n124, n238, n282);
    let n286: ZN = zsel_n(n242, n119, n283);
    let n287: ZN = zsel_n(n242, zn_splat(P8::from_raw(0i32)), n284);
    let n288: ZB = zsel_b(n242, n238, n285);
    let n289: ZN = zsel_n(n120, n119, n286);
    let n290: ZN = zsel_n(n120, r_c281, n287);
    let n291: ZB = zsel_b(n120, n238, n288);
    let n292: ZN = zsel_n(n241, r_c254, n289);
    let n293: ZN = zsel_n(n241, zn_splat(P8::from_raw(0i32)), n290);
    let n294: ZB = zsel_b(n241, n238, n291);
    let n295: ZN = zsel_n(n90, n236, r_c253);
    let n296: ZN = zsel_n(n90, n292, r_c254);
    let n297: ZN = zsel_n(n90, n237, r_c280);
    let n298: ZN = zsel_n(n90, n293, r_c281);
    let n299: ZB = zb_or(n91, n294);
    let n300: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n295);
    let n301: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n296);
    let n302: ZN = zn_div(n300, zn_splat(P8::from_raw(524288i32)));
    let n303: ZN = zn_flr(n302);
    let n304: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n303);
    let n305: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n300);
    let n306: ZN = zn_sub(n305, zn_splat(P8::from_raw(65536i32)));
    let n307: ZN = zn_div(n306, zn_splat(P8::from_raw(524288i32)));
    let n308: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n307);
    let n309: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n304);
    let n310: ZB = zn_le(n309, n308);
    let n311: ZB = zn_gt(n309, n308);
    let n312: ZB = zb_and(n68, n310);
    let n313: ZB = zb_and(n68, n311);
    let n314: ZN = zn_div(n301, zn_splat(P8::from_raw(524288i32)));
    let n315: ZN = zn_flr(n314);
    let n316: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n315);
    let n317: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n301);
    let n318: ZN = zn_sub(n317, zn_splat(P8::from_raw(65536i32)));
    let n319: ZN = zn_div(n318, zn_splat(P8::from_raw(524288i32)));
    let n320: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n319);
    let n321: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n316);
    let n322: ZB = zn_le(n321, n320);
    let n323: ZB = zn_gt(n321, n320);
    let n324: ZB = zb_and(n312, n322);
    let n325: ZB = zb_and(n312, n323);
    let n326: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n309);
    let n327: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n321);
    let n328: ZN = zn_mget(g.cart, n326, n327);
    let n329: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n328);
    let n330: ZB = zb_not(n329);
    let n331: ZB = zb_and(n324, n329);
    let n332: ZB = zb_and(n324, n330);
    let n333: ZN = zn_rem(n318, zn_splat(P8::from_raw(524288i32)));
    let n334: ZB = zn_ge(n333, zn_splat(P8::from_raw(393216i32)));
    let n335: ZB = zn_lt(n333, zn_splat(P8::from_raw(393216i32)));
    let n336: ZB = zb_and(n331, n335);
    let n337: ZB = zb_and(n331, n334);
    let n338: ZN = zn_mul(n321, zn_splat(P8::from_raw(524288i32)));
    let n339: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n338);
    let n340: ZB = zn_eq(n317, n339);
    let n341: ZB = zb_or(n336, n337);
    let n342: ZB = zb_or(n334, n340);
    let n343: ZB = zb_or(n332, n341);
    let n344: ZB = zb_and(n329, n342);
    let n345: ZB = zb_not(n344);
    let n346: ZB = zb_and(n343, n344);
    let n347: ZB = zb_and(n343, n345);
    let n348: ZB = zn_ge(n298, zn_splat(P8::from_raw(0i32)));
    let n349: ZB = zb_or(n346, n347);
    let n350: ZB = zb_and(n344, n348);
    let n351: ZB = zb_not(n350);
    let n352: ZB = zb_and(n349, n350);
    let n353: ZB = zb_and(n349, n351);
    let n354: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n328);
    let n355: ZB = zb_not(n354);
    let n356: ZB = zb_and(n353, n354);
    let n357: ZB = zb_and(n353, n355);
    let n358: ZN = zn_rem(n301, zn_splat(P8::from_raw(524288i32)));
    let n359: ZB = zn_le(n358, zn_splat(P8::from_raw(131072i32)));
    let n360: ZB = zb_or(n356, n357);
    let n361: ZB = zb_and(n354, n359);
    let n362: ZB = zb_not(n361);
    let n363: ZB = zb_and(n360, n361);
    let n364: ZB = zb_and(n360, n362);
    let n365: ZB = zn_le(n298, zn_splat(P8::from_raw(0i32)));
    let n366: ZB = zb_or(n363, n364);
    let n367: ZB = zb_and(n361, n365);
    let n368: ZB = zb_not(n367);
    let n369: ZB = zb_and(n366, n367);
    let n370: ZB = zb_and(n366, n368);
    let n371: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n328);
    let n372: ZB = zb_not(n371);
    let n373: ZB = zb_and(n370, n371);
    let n374: ZB = zb_and(n370, n372);
    let n375: ZN = zn_rem(n300, zn_splat(P8::from_raw(524288i32)));
    let n376: ZB = zn_le(n375, zn_splat(P8::from_raw(131072i32)));
    let n377: ZB = zb_or(n373, n374);
    let n378: ZB = zb_and(n371, n376);
    let n379: ZB = zb_not(n378);
    let n380: ZB = zb_and(n377, n378);
    let n381: ZB = zb_and(n377, n379);
    let n382: ZB = zn_le(n297, zn_splat(P8::from_raw(0i32)));
    let n383: ZB = zb_or(n380, n381);
    let n384: ZB = zb_and(n378, n382);
    let n385: ZB = zb_not(n384);
    let n386: ZB = zb_and(n383, n384);
    let n387: ZB = zb_and(n383, n385);
    let n388: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n328);
    let n389: ZB = zb_not(n388);
    let n390: ZB = zb_and(n387, n388);
    let n391: ZB = zb_and(n387, n389);
    let n392: ZN = zn_rem(n306, zn_splat(P8::from_raw(524288i32)));
    let n393: ZB = zn_ge(n392, zn_splat(P8::from_raw(393216i32)));
    let n394: ZB = zn_lt(n392, zn_splat(P8::from_raw(393216i32)));
    let n395: ZB = zb_and(n390, n394);
    let n396: ZB = zb_and(n390, n393);
    let n397: ZN = zn_mul(n309, zn_splat(P8::from_raw(524288i32)));
    let n398: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n397);
    let n399: ZB = zn_eq(n305, n398);
    let n400: ZB = zb_or(n395, n396);
    let n401: ZB = zb_or(n393, n399);
    let n402: ZB = zb_or(n391, n400);
    let n403: ZB = zb_and(n388, n401);
    let n404: ZB = zb_not(n403);
    let n405: ZB = zb_and(n402, n403);
    let n406: ZB = zb_and(n402, n404);
    let n407: ZB = zn_ge(n297, zn_splat(P8::from_raw(0i32)));
    let n408: ZB = zb_or(n405, n406);
    let n409: ZB = zb_and(n403, n407);
    let n410: ZB = zb_not(n409);
    let n411: ZB = zb_and(n408, n409);
    let n412: ZB = zb_and(n408, n410);
    let n413: ZB = zb_or(n386, n411);
    let n414: ZB = zb_or(n369, n413);
    let n415: ZB = zb_or(n352, n414);
    let n416: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n316);
    let n417: ZB = zn_le(n416, n320);
    let n418: ZB = zn_gt(n416, n320);
    let n419: ZB = zb_and(n412, n417);
    let n420: ZB = zb_and(n412, n418);
    let n421: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n416);
    let n422: ZN = zn_mget(g.cart, n326, n421);
    let n423: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n422);
    let n424: ZB = zb_not(n423);
    let n425: ZB = zb_and(n419, n423);
    let n426: ZB = zb_and(n419, n424);
    let n427: ZB = zb_and(n335, n425);
    let n428: ZB = zb_and(n334, n425);
    let n429: ZN = zn_mul(n416, zn_splat(P8::from_raw(524288i32)));
    let n430: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n429);
    let n431: ZB = zn_eq(n317, n430);
    let n432: ZB = zb_or(n427, n428);
    let n433: ZB = zb_or(n334, n431);
    let n434: ZB = zb_or(n426, n432);
    let n435: ZB = zb_and(n423, n433);
    let n436: ZB = zb_not(n435);
    let n437: ZB = zb_and(n434, n435);
    let n438: ZB = zb_and(n434, n436);
    let n439: ZB = zb_or(n437, n438);
    let n440: ZB = zb_and(n348, n435);
    let n441: ZB = zb_not(n440);
    let n442: ZB = zb_and(n439, n440);
    let n443: ZB = zb_and(n439, n441);
    let n444: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n422);
    let n445: ZB = zb_not(n444);
    let n446: ZB = zb_and(n443, n444);
    let n447: ZB = zb_and(n443, n445);
    let n448: ZB = zb_or(n446, n447);
    let n449: ZB = zb_and(n359, n444);
    let n450: ZB = zb_not(n449);
    let n451: ZB = zb_and(n448, n449);
    let n452: ZB = zb_and(n448, n450);
    let n453: ZB = zb_or(n451, n452);
    let n454: ZB = zb_and(n365, n449);
    let n455: ZB = zb_not(n454);
    let n456: ZB = zb_and(n453, n454);
    let n457: ZB = zb_and(n453, n455);
    let n458: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n422);
    let n459: ZB = zb_not(n458);
    let n460: ZB = zb_and(n457, n458);
    let n461: ZB = zb_and(n457, n459);
    let n462: ZB = zb_or(n460, n461);
    let n463: ZB = zb_and(n376, n458);
    let n464: ZB = zb_not(n463);
    let n465: ZB = zb_and(n462, n463);
    let n466: ZB = zb_and(n462, n464);
    let n467: ZB = zb_or(n465, n466);
    let n468: ZB = zb_and(n382, n463);
    let n469: ZB = zb_not(n468);
    let n470: ZB = zb_and(n467, n468);
    let n471: ZB = zb_and(n467, n469);
    let n472: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n422);
    let n473: ZB = zb_not(n472);
    let n474: ZB = zb_and(n471, n472);
    let n475: ZB = zb_and(n471, n473);
    let n476: ZB = zb_and(n394, n474);
    let n477: ZB = zb_and(n393, n474);
    let n478: ZB = zb_or(n476, n477);
    let n479: ZB = zb_or(n475, n478);
    let n480: ZB = zb_and(n401, n472);
    let n481: ZB = zb_not(n480);
    let n482: ZB = zb_and(n479, n480);
    let n483: ZB = zb_and(n479, n481);
    let n484: ZB = zb_or(n482, n483);
    let n485: ZB = zb_and(n407, n480);
    let n486: ZB = zb_not(n485);
    let n487: ZB = zb_and(n484, n485);
    let n488: ZB = zb_and(n484, n486);
    let n489: ZB = zb_or(n470, n487);
    let n490: ZB = zb_or(n456, n489);
    let n491: ZB = zb_or(n442, n490);
    let n492: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n316);
    let n493: ZB = zn_le(n492, n320);
    let n494: ZB = zn_gt(n492, n320);
    let n495: ZB = zb_and(n488, n493);
    let n496: ZB = zb_and(n488, n494);
    let n497: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n492);
    let n498: ZN = zn_mget(g.cart, n326, n497);
    let n499: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n498);
    let n500: ZB = zb_not(n499);
    let n501: ZB = zb_and(n495, n499);
    let n502: ZB = zb_and(n495, n500);
    let n503: ZB = zb_and(n335, n501);
    let n504: ZB = zb_and(n334, n501);
    let n505: ZN = zn_mul(n492, zn_splat(P8::from_raw(524288i32)));
    let n506: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n505);
    let n507: ZB = zn_eq(n317, n506);
    let n508: ZB = zb_or(n503, n504);
    let n509: ZB = zb_or(n334, n507);
    let n510: ZB = zb_or(n502, n508);
    let n511: ZB = zb_and(n499, n509);
    let n512: ZB = zb_not(n511);
    let n513: ZB = zb_and(n510, n511);
    let n514: ZB = zb_and(n510, n512);
    let n515: ZB = zb_or(n513, n514);
    let n516: ZB = zb_and(n348, n511);
    let n517: ZB = zb_not(n516);
    let n518: ZB = zb_and(n515, n516);
    let n519: ZB = zb_and(n515, n517);
    let n520: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n498);
    let n521: ZB = zb_not(n520);
    let n522: ZB = zb_and(n519, n520);
    let n523: ZB = zb_and(n519, n521);
    let n524: ZB = zb_or(n522, n523);
    let n525: ZB = zb_and(n359, n520);
    let n526: ZB = zb_not(n525);
    let n527: ZB = zb_and(n524, n525);
    let n528: ZB = zb_and(n524, n526);
    let n529: ZB = zb_or(n527, n528);
    let n530: ZB = zb_and(n365, n525);
    let n531: ZB = zb_not(n530);
    let n532: ZB = zb_and(n529, n530);
    let n533: ZB = zb_and(n529, n531);
    let n534: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n498);
    let n535: ZB = zb_not(n534);
    let n536: ZB = zb_and(n533, n534);
    let n537: ZB = zb_and(n533, n535);
    let n538: ZB = zb_or(n536, n537);
    let n539: ZB = zb_and(n376, n534);
    let n540: ZB = zb_not(n539);
    let n541: ZB = zb_and(n538, n539);
    let n542: ZB = zb_and(n538, n540);
    let n543: ZB = zb_or(n541, n542);
    let n544: ZB = zb_and(n382, n539);
    let n545: ZB = zb_not(n544);
    let n546: ZB = zb_and(n543, n544);
    let n547: ZB = zb_and(n543, n545);
    let n548: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n498);
    let n549: ZB = zb_not(n548);
    let n550: ZB = zb_and(n547, n548);
    let n551: ZB = zb_and(n547, n549);
    let n552: ZB = zb_and(n394, n550);
    let n553: ZB = zb_and(n393, n550);
    let n554: ZB = zb_or(n552, n553);
    let n555: ZB = zb_or(n551, n554);
    let n556: ZB = zb_and(n401, n548);
    let n557: ZB = zb_not(n556);
    let n558: ZB = zb_and(n555, n556);
    let n559: ZB = zb_and(n555, n557);
    let n560: ZB = zb_or(n558, n559);
    let n561: ZB = zb_and(n407, n556);
    let n562: ZB = zb_not(n561);
    let n563: ZB = zb_and(n560, n561);
    let n564: ZB = zb_and(n560, n562);
    let n565: ZB = zb_or(n546, n563);
    let n566: ZB = zb_or(n532, n565);
    let n567: ZB = zb_or(n518, n566);
    let n568: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n316);
    let n569: ZB = zn_gt(n568, n320);
    let n570: ZB = zb_and(n299, n569);
    let n571: ZB = zb_or(n496, n564);
    let n572: ZB = zsel_b(n494, n299, n570);
    let n573: ZB = zb_or(n491, n567);
    let n574: ZB = zb_or(n420, n571);
    let n575: ZB = zsel_b(n418, n299, n572);
    let n576: ZB = zb_or(n415, n573);
    let n577: ZB = zb_or(n325, n574);
    let n578: ZB = zsel_b(n323, n299, n575);
    let n579: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n304);
    let n580: ZB = zn_le(n579, n308);
    let n581: ZB = zn_gt(n579, n308);
    let n582: ZB = zb_and(n577, n580);
    let n583: ZB = zb_and(n577, n581);
    let n584: ZB = zb_and(n322, n582);
    let n585: ZB = zb_and(n323, n582);
    let n586: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n579);
    let n587: ZN = zn_mget(g.cart, n586, n327);
    let n588: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n587);
    let n589: ZB = zb_not(n588);
    let n590: ZB = zb_and(n584, n588);
    let n591: ZB = zb_and(n584, n589);
    let n592: ZB = zb_and(n335, n590);
    let n593: ZB = zb_and(n334, n590);
    let n594: ZB = zb_or(n592, n593);
    let n595: ZB = zb_or(n591, n594);
    let n596: ZB = zb_and(n342, n588);
    let n597: ZB = zb_not(n596);
    let n598: ZB = zb_and(n595, n596);
    let n599: ZB = zb_and(n595, n597);
    let n600: ZB = zb_or(n598, n599);
    let n601: ZB = zb_and(n348, n596);
    let n602: ZB = zb_not(n601);
    let n603: ZB = zb_and(n600, n601);
    let n604: ZB = zb_and(n600, n602);
    let n605: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n587);
    let n606: ZB = zb_not(n605);
    let n607: ZB = zb_and(n604, n605);
    let n608: ZB = zb_and(n604, n606);
    let n609: ZB = zb_or(n607, n608);
    let n610: ZB = zb_and(n359, n605);
    let n611: ZB = zb_not(n610);
    let n612: ZB = zb_and(n609, n610);
    let n613: ZB = zb_and(n609, n611);
    let n614: ZB = zb_or(n612, n613);
    let n615: ZB = zb_and(n365, n610);
    let n616: ZB = zb_not(n615);
    let n617: ZB = zb_and(n614, n615);
    let n618: ZB = zb_and(n614, n616);
    let n619: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n587);
    let n620: ZB = zb_not(n619);
    let n621: ZB = zb_and(n618, n619);
    let n622: ZB = zb_and(n618, n620);
    let n623: ZB = zb_or(n621, n622);
    let n624: ZB = zb_and(n376, n619);
    let n625: ZB = zb_not(n624);
    let n626: ZB = zb_and(n623, n624);
    let n627: ZB = zb_and(n623, n625);
    let n628: ZB = zb_or(n626, n627);
    let n629: ZB = zb_and(n382, n624);
    let n630: ZB = zb_not(n629);
    let n631: ZB = zb_and(n628, n629);
    let n632: ZB = zb_and(n628, n630);
    let n633: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n587);
    let n634: ZB = zb_not(n633);
    let n635: ZB = zb_and(n632, n633);
    let n636: ZB = zb_and(n632, n634);
    let n637: ZB = zb_and(n394, n635);
    let n638: ZB = zb_and(n393, n635);
    let n639: ZN = zn_mul(n579, zn_splat(P8::from_raw(524288i32)));
    let n640: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n639);
    let n641: ZB = zn_eq(n305, n640);
    let n642: ZB = zb_or(n637, n638);
    let n643: ZB = zb_or(n393, n641);
    let n644: ZB = zb_or(n636, n642);
    let n645: ZB = zb_and(n633, n643);
    let n646: ZB = zb_not(n645);
    let n647: ZB = zb_and(n644, n645);
    let n648: ZB = zb_and(n644, n646);
    let n649: ZB = zb_or(n647, n648);
    let n650: ZB = zb_and(n407, n645);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zb_and(n649, n650);
    let n653: ZB = zb_and(n649, n651);
    let n654: ZB = zb_or(n631, n652);
    let n655: ZB = zb_or(n617, n654);
    let n656: ZB = zb_or(n603, n655);
    let n657: ZB = zb_and(n417, n653);
    let n658: ZB = zb_and(n418, n653);
    let n659: ZN = zn_mget(g.cart, n586, n421);
    let n660: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n659);
    let n661: ZB = zb_not(n660);
    let n662: ZB = zb_and(n657, n660);
    let n663: ZB = zb_and(n657, n661);
    let n664: ZB = zb_and(n335, n662);
    let n665: ZB = zb_and(n334, n662);
    let n666: ZB = zb_or(n664, n665);
    let n667: ZB = zb_or(n663, n666);
    let n668: ZB = zb_and(n433, n660);
    let n669: ZB = zb_not(n668);
    let n670: ZB = zb_and(n667, n668);
    let n671: ZB = zb_and(n667, n669);
    let n672: ZB = zb_or(n670, n671);
    let n673: ZB = zb_and(n348, n668);
    let n674: ZB = zb_not(n673);
    let n675: ZB = zb_and(n672, n673);
    let n676: ZB = zb_and(n672, n674);
    let n677: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n659);
    let n678: ZB = zb_not(n677);
    let n679: ZB = zb_and(n676, n677);
    let n680: ZB = zb_and(n676, n678);
    let n681: ZB = zb_or(n679, n680);
    let n682: ZB = zb_and(n359, n677);
    let n683: ZB = zb_not(n682);
    let n684: ZB = zb_and(n681, n682);
    let n685: ZB = zb_and(n681, n683);
    let n686: ZB = zb_or(n684, n685);
    let n687: ZB = zb_and(n365, n682);
    let n688: ZB = zb_not(n687);
    let n689: ZB = zb_and(n686, n687);
    let n690: ZB = zb_and(n686, n688);
    let n691: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n659);
    let n692: ZB = zb_not(n691);
    let n693: ZB = zb_and(n690, n691);
    let n694: ZB = zb_and(n690, n692);
    let n695: ZB = zb_or(n693, n694);
    let n696: ZB = zb_and(n376, n691);
    let n697: ZB = zb_not(n696);
    let n698: ZB = zb_and(n695, n696);
    let n699: ZB = zb_and(n695, n697);
    let n700: ZB = zb_or(n698, n699);
    let n701: ZB = zb_and(n382, n696);
    let n702: ZB = zb_not(n701);
    let n703: ZB = zb_and(n700, n701);
    let n704: ZB = zb_and(n700, n702);
    let n705: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n659);
    let n706: ZB = zb_not(n705);
    let n707: ZB = zb_and(n704, n705);
    let n708: ZB = zb_and(n704, n706);
    let n709: ZB = zb_and(n394, n707);
    let n710: ZB = zb_and(n393, n707);
    let n711: ZB = zb_or(n709, n710);
    let n712: ZB = zb_or(n708, n711);
    let n713: ZB = zb_and(n643, n705);
    let n714: ZB = zb_not(n713);
    let n715: ZB = zb_and(n712, n713);
    let n716: ZB = zb_and(n712, n714);
    let n717: ZB = zb_or(n715, n716);
    let n718: ZB = zb_and(n407, n713);
    let n719: ZB = zb_not(n718);
    let n720: ZB = zb_and(n717, n718);
    let n721: ZB = zb_and(n717, n719);
    let n722: ZB = zb_or(n703, n720);
    let n723: ZB = zb_or(n689, n722);
    let n724: ZB = zb_or(n675, n723);
    let n725: ZB = zb_and(n493, n721);
    let n726: ZB = zb_and(n494, n721);
    let n727: ZN = zn_mget(g.cart, n586, n497);
    let n728: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n727);
    let n729: ZB = zb_not(n728);
    let n730: ZB = zb_and(n725, n728);
    let n731: ZB = zb_and(n725, n729);
    let n732: ZB = zb_and(n335, n730);
    let n733: ZB = zb_and(n334, n730);
    let n734: ZB = zb_or(n732, n733);
    let n735: ZB = zb_or(n731, n734);
    let n736: ZB = zb_and(n509, n728);
    let n737: ZB = zb_not(n736);
    let n738: ZB = zb_and(n735, n736);
    let n739: ZB = zb_and(n735, n737);
    let n740: ZB = zb_or(n738, n739);
    let n741: ZB = zb_and(n348, n736);
    let n742: ZB = zb_not(n741);
    let n743: ZB = zb_and(n740, n741);
    let n744: ZB = zb_and(n740, n742);
    let n745: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n727);
    let n746: ZB = zb_not(n745);
    let n747: ZB = zb_and(n744, n745);
    let n748: ZB = zb_and(n744, n746);
    let n749: ZB = zb_or(n747, n748);
    let n750: ZB = zb_and(n359, n745);
    let n751: ZB = zb_not(n750);
    let n752: ZB = zb_and(n749, n750);
    let n753: ZB = zb_and(n749, n751);
    let n754: ZB = zb_or(n752, n753);
    let n755: ZB = zb_and(n365, n750);
    let n756: ZB = zb_not(n755);
    let n757: ZB = zb_and(n754, n755);
    let n758: ZB = zb_and(n754, n756);
    let n759: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n727);
    let n760: ZB = zb_not(n759);
    let n761: ZB = zb_and(n758, n759);
    let n762: ZB = zb_and(n758, n760);
    let n763: ZB = zb_or(n761, n762);
    let n764: ZB = zb_and(n376, n759);
    let n765: ZB = zb_not(n764);
    let n766: ZB = zb_and(n763, n764);
    let n767: ZB = zb_and(n763, n765);
    let n768: ZB = zb_or(n766, n767);
    let n769: ZB = zb_and(n382, n764);
    let n770: ZB = zb_not(n769);
    let n771: ZB = zb_and(n768, n769);
    let n772: ZB = zb_and(n768, n770);
    let n773: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n727);
    let n774: ZB = zb_not(n773);
    let n775: ZB = zb_and(n772, n773);
    let n776: ZB = zb_and(n772, n774);
    let n777: ZB = zb_and(n394, n775);
    let n778: ZB = zb_and(n393, n775);
    let n779: ZB = zb_or(n777, n778);
    let n780: ZB = zb_or(n776, n779);
    let n781: ZB = zb_and(n643, n773);
    let n782: ZB = zb_not(n781);
    let n783: ZB = zb_and(n780, n781);
    let n784: ZB = zb_and(n780, n782);
    let n785: ZB = zb_or(n783, n784);
    let n786: ZB = zb_and(n407, n781);
    let n787: ZB = zb_not(n786);
    let n788: ZB = zb_and(n785, n786);
    let n789: ZB = zb_and(n785, n787);
    let n790: ZB = zb_or(n771, n788);
    let n791: ZB = zb_or(n757, n790);
    let n792: ZB = zb_or(n743, n791);
    let n793: ZB = zb_and(n569, n578);
    let n794: ZB = zb_or(n726, n789);
    let n795: ZB = zsel_b(n494, n578, n793);
    let n796: ZB = zb_or(n724, n792);
    let n797: ZB = zb_or(n658, n794);
    let n798: ZB = zsel_b(n418, n578, n795);
    let n799: ZB = zb_or(n656, n796);
    let n800: ZB = zb_or(n585, n797);
    let n801: ZB = zsel_b(n323, n578, n798);
    let n802: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n304);
    let n803: ZB = zn_le(n802, n308);
    let n804: ZB = zn_gt(n802, n308);
    let n805: ZB = zb_and(n800, n803);
    let n806: ZB = zb_and(n800, n804);
    let n807: ZB = zb_and(n322, n805);
    let n808: ZB = zb_and(n323, n805);
    let n809: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n802);
    let n810: ZN = zn_mget(g.cart, n809, n327);
    let n811: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n810);
    let n812: ZB = zb_not(n811);
    let n813: ZB = zb_and(n807, n811);
    let n814: ZB = zb_and(n807, n812);
    let n815: ZB = zb_and(n335, n813);
    let n816: ZB = zb_and(n334, n813);
    let n817: ZB = zb_or(n815, n816);
    let n818: ZB = zb_or(n814, n817);
    let n819: ZB = zb_and(n342, n811);
    let n820: ZB = zb_not(n819);
    let n821: ZB = zb_and(n818, n819);
    let n822: ZB = zb_and(n818, n820);
    let n823: ZB = zb_or(n821, n822);
    let n824: ZB = zb_and(n348, n819);
    let n825: ZB = zb_not(n824);
    let n826: ZB = zb_and(n823, n824);
    let n827: ZB = zb_and(n823, n825);
    let n828: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n810);
    let n829: ZB = zb_not(n828);
    let n830: ZB = zb_and(n827, n828);
    let n831: ZB = zb_and(n827, n829);
    let n832: ZB = zb_or(n830, n831);
    let n833: ZB = zb_and(n359, n828);
    let n834: ZB = zb_not(n833);
    let n835: ZB = zb_and(n832, n833);
    let n836: ZB = zb_and(n832, n834);
    let n837: ZB = zb_or(n835, n836);
    let n838: ZB = zb_and(n365, n833);
    let n839: ZB = zb_not(n838);
    let n840: ZB = zb_and(n837, n838);
    let n841: ZB = zb_and(n837, n839);
    let n842: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n810);
    let n843: ZB = zb_not(n842);
    let n844: ZB = zb_and(n841, n842);
    let n845: ZB = zb_and(n841, n843);
    let n846: ZB = zb_or(n844, n845);
    let n847: ZB = zb_and(n376, n842);
    let n848: ZB = zb_not(n847);
    let n849: ZB = zb_and(n846, n847);
    let n850: ZB = zb_and(n846, n848);
    let n851: ZB = zb_or(n849, n850);
    let n852: ZB = zb_and(n382, n847);
    let n853: ZB = zb_not(n852);
    let n854: ZB = zb_and(n851, n852);
    let n855: ZB = zb_and(n851, n853);
    let n856: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n810);
    let n857: ZB = zb_not(n856);
    let n858: ZB = zb_and(n855, n856);
    let n859: ZB = zb_and(n855, n857);
    let n860: ZB = zb_and(n394, n858);
    let n861: ZB = zb_and(n393, n858);
    let n862: ZN = zn_mul(n802, zn_splat(P8::from_raw(524288i32)));
    let n863: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n862);
    let n864: ZB = zn_eq(n305, n863);
    let n865: ZB = zb_or(n860, n861);
    let n866: ZB = zb_or(n393, n864);
    let n867: ZB = zb_or(n859, n865);
    let n868: ZB = zb_and(n856, n866);
    let n869: ZB = zb_not(n868);
    let n870: ZB = zb_and(n867, n868);
    let n871: ZB = zb_and(n867, n869);
    let n872: ZB = zb_or(n870, n871);
    let n873: ZB = zb_and(n407, n868);
    let n874: ZB = zb_not(n873);
    let n875: ZB = zb_and(n872, n873);
    let n876: ZB = zb_and(n872, n874);
    let n877: ZB = zb_or(n854, n875);
    let n878: ZB = zb_or(n840, n877);
    let n879: ZB = zb_or(n826, n878);
    let n880: ZB = zb_and(n417, n876);
    let n881: ZB = zb_and(n418, n876);
    let n882: ZN = zn_mget(g.cart, n809, n421);
    let n883: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n882);
    let n884: ZB = zb_not(n883);
    let n885: ZB = zb_and(n880, n883);
    let n886: ZB = zb_and(n880, n884);
    let n887: ZB = zb_and(n335, n885);
    let n888: ZB = zb_and(n334, n885);
    let n889: ZB = zb_or(n887, n888);
    let n890: ZB = zb_or(n886, n889);
    let n891: ZB = zb_and(n433, n883);
    let n892: ZB = zb_not(n891);
    let n893: ZB = zb_and(n890, n891);
    let n894: ZB = zb_and(n890, n892);
    let n895: ZB = zb_or(n893, n894);
    let n896: ZB = zb_and(n348, n891);
    let n897: ZB = zb_not(n896);
    let n898: ZB = zb_and(n895, n896);
    let n899: ZB = zb_and(n895, n897);
    let n900: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n882);
    let n901: ZB = zb_not(n900);
    let n902: ZB = zb_and(n899, n900);
    let n903: ZB = zb_and(n899, n901);
    let n904: ZB = zb_or(n902, n903);
    let n905: ZB = zb_and(n359, n900);
    let n906: ZB = zb_not(n905);
    let n907: ZB = zb_and(n904, n905);
    let n908: ZB = zb_and(n904, n906);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_and(n365, n905);
    let n911: ZB = zb_not(n910);
    let n912: ZB = zb_and(n909, n910);
    let n913: ZB = zb_and(n909, n911);
    let n914: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n882);
    let n915: ZB = zb_not(n914);
    let n916: ZB = zb_and(n913, n914);
    let n917: ZB = zb_and(n913, n915);
    let n918: ZB = zb_or(n916, n917);
    let n919: ZB = zb_and(n376, n914);
    let n920: ZB = zb_not(n919);
    let n921: ZB = zb_and(n918, n919);
    let n922: ZB = zb_and(n918, n920);
    let n923: ZB = zb_or(n921, n922);
    let n924: ZB = zb_and(n382, n919);
    let n925: ZB = zb_not(n924);
    let n926: ZB = zb_and(n923, n924);
    let n927: ZB = zb_and(n923, n925);
    let n928: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n882);
    let n929: ZB = zb_not(n928);
    let n930: ZB = zb_and(n927, n928);
    let n931: ZB = zb_and(n927, n929);
    let n932: ZB = zb_and(n394, n930);
    let n933: ZB = zb_and(n393, n930);
    let n934: ZB = zb_or(n932, n933);
    let n935: ZB = zb_or(n931, n934);
    let n936: ZB = zb_and(n866, n928);
    let n937: ZB = zb_not(n936);
    let n938: ZB = zb_and(n935, n936);
    let n939: ZB = zb_and(n935, n937);
    let n940: ZB = zb_or(n938, n939);
    let n941: ZB = zb_and(n407, n936);
    let n942: ZB = zb_not(n941);
    let n943: ZB = zb_and(n940, n941);
    let n944: ZB = zb_and(n940, n942);
    let n945: ZB = zb_or(n926, n943);
    let n946: ZB = zb_or(n912, n945);
    let n947: ZB = zb_or(n898, n946);
    let n948: ZB = zb_and(n493, n944);
    let n949: ZB = zb_and(n494, n944);
    let n950: ZN = zn_mget(g.cart, n809, n497);
    let n951: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n950);
    let n952: ZB = zb_not(n951);
    let n953: ZB = zb_and(n948, n951);
    let n954: ZB = zb_and(n948, n952);
    let n955: ZB = zb_and(n335, n953);
    let n956: ZB = zb_and(n334, n953);
    let n957: ZB = zb_or(n955, n956);
    let n958: ZB = zb_or(n954, n957);
    let n959: ZB = zb_and(n509, n951);
    let n960: ZB = zb_not(n959);
    let n961: ZB = zb_and(n958, n959);
    let n962: ZB = zb_and(n958, n960);
    let n963: ZB = zb_or(n961, n962);
    let n964: ZB = zb_and(n348, n959);
    let n965: ZB = zb_not(n964);
    let n966: ZB = zb_and(n963, n964);
    let n967: ZB = zb_and(n963, n965);
    let n968: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n950);
    let n969: ZB = zb_not(n968);
    let n970: ZB = zb_and(n967, n968);
    let n971: ZB = zb_and(n967, n969);
    let n972: ZB = zb_or(n970, n971);
    let n973: ZB = zb_and(n359, n968);
    let n974: ZB = zb_not(n973);
    let n975: ZB = zb_and(n972, n973);
    let n976: ZB = zb_and(n972, n974);
    let n977: ZB = zb_or(n975, n976);
    let n978: ZB = zb_and(n365, n973);
    let n979: ZB = zb_not(n978);
    let n980: ZB = zb_and(n977, n978);
    let n981: ZB = zb_and(n977, n979);
    let n982: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n950);
    let n983: ZB = zb_not(n982);
    let n984: ZB = zb_and(n981, n982);
    let n985: ZB = zb_and(n981, n983);
    let n986: ZB = zb_or(n984, n985);
    let n987: ZB = zb_and(n376, n982);
    let n988: ZB = zb_not(n987);
    let n989: ZB = zb_and(n986, n987);
    let n990: ZB = zb_and(n986, n988);
    let n991: ZB = zb_or(n989, n990);
    let n992: ZB = zb_and(n382, n987);
    let n993: ZB = zb_not(n992);
    let n994: ZB = zb_and(n991, n992);
    let n995: ZB = zb_and(n991, n993);
    let n996: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n950);
    let n997: ZB = zb_not(n996);
    let n998: ZB = zb_and(n995, n996);
    let n999: ZB = zb_and(n995, n997);
    let n1000: ZB = zb_and(n394, n998);
    let n1001: ZB = zb_and(n393, n998);
    let n1002: ZB = zb_or(n1000, n1001);
    let n1003: ZB = zb_or(n999, n1002);
    let n1004: ZB = zb_and(n866, n996);
    let n1005: ZB = zb_not(n1004);
    let n1006: ZB = zb_and(n1003, n1004);
    let n1007: ZB = zb_and(n1003, n1005);
    let n1008: ZB = zb_or(n1006, n1007);
    let n1009: ZB = zb_and(n407, n1004);
    let n1010: ZB = zb_not(n1009);
    let n1011: ZB = zb_and(n1008, n1009);
    let n1012: ZB = zb_and(n1008, n1010);
    let n1013: ZB = zb_or(n994, n1011);
    let n1014: ZB = zb_or(n980, n1013);
    let n1015: ZB = zb_or(n966, n1014);
    let n1016: ZB = zb_and(n569, n801);
    let n1017: ZB = zb_or(n949, n1012);
    let n1018: ZB = zsel_b(n494, n801, n1016);
    let n1019: ZB = zb_or(n947, n1015);
    let n1020: ZB = zb_or(n881, n1017);
    let n1021: ZB = zsel_b(n418, n801, n1018);
    let n1022: ZB = zb_or(n879, n1019);
    let n1023: ZB = zb_or(n808, n1020);
    let n1024: ZB = zsel_b(n323, n801, n1021);
    let n1025: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n304);
    let n1026: ZB = zn_gt(n1025, n308);
    let n1027: ZB = zb_and(n1024, n1026);
    let n1028: ZB = zb_or(n799, n1022);
    let n1029: ZB = zsel_b(n799, n578, n801);
    let n1030: ZB = zb_or(n806, n1023);
    let n1031: ZB = zsel_b(n804, n801, n1027);
    let n1032: ZB = zb_or(n576, n1028);
    let n1033: ZB = zsel_b(n576, n299, n1029);
    let n1034: ZB = zb_or(n583, n1030);
    let n1035: ZB = zsel_b(n581, n578, n1031);
    let n1036: ZB = zb_or(n313, n1034);
    let n1037: ZB = zsel_b(n311, n299, n1035);
    let n1038: ZB = zn_gt(n296, zn_splat(P8::from_raw(8388608i32)));
    let n1039: ZB = zn_le(n296, zn_splat(P8::from_raw(8388608i32)));
    let n1040: ZB = zb_and(n1032, n1038);
    let n1041: ZB = zb_and(n1032, n1039);
    let n1042: ZB = zb_or(n1040, n1041);
    let n1043: ZB = zb_and(n1036, n1038);
    let n1044: ZB = zb_or(n1042, n1043);
    let n1045: ZB = zsel_b(n1042, n1033, n1037);
    let n1046: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n300);
    let n1047: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n301);
    let n1048: ZB = zn_tile_flag_at(g.cache, g.cart, n1046, n1047, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1049: ZB = zb_not(n1048);
    let n1050: ZB = zb_and(n1044, n1049);
    let n1051: ZB = zb_and(n1044, n1048);
    let n1052: ZB = zb_or(n1050, n1051);
    let n1053: ZB = zb_and(n1049, n1052);
    let n1054: ZB = zb_and(n1048, n1052);
    let n1055: ZB = zb_or(n1053, n1054);
    let n1056: ZN = zsel_n(n1048, n153, r_c237);
    let n1057: ZN = zsel_n(n1048, zn_splat(P8::from_raw(393216i32)), n157);
    let n1058: ZB = zb_and(n1048, n1055);
    let n1059: ZB = zb_and(n1049, n1055);
    let n1060: ZB = zb_and(n151, n1058);
    let n1061: ZB = zb_and(n152, n1058);
    let n1062: ZB = zb_or(n1060, n1061);
    let n1063: ZB = zb_and(n154, n1059);
    let n1064: ZB = zb_and(n155, n1059);
    let n1065: ZB = zb_or(n1063, n1064);
    let n1066: ZB = zb_or(n1062, n1065);
    let n1067: ZB = zn_gt(n297, r_c270);
    let n1068: ZB = zn_le(n297, r_c270);
    let n1069: ZB = zn_gt(n298, r_c271);
    let n1070: ZB = zn_le(n298, r_c271);
    let n1071: ZN = zsel_n(n1049, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1072: ZN = zn_abs(n297);
    let n1073: ZB = zn_gt(n1072, zn_splat(P8::from_raw(65536i32)));
    let n1074: ZB = zn_le(n1072, zn_splat(P8::from_raw(65536i32)));
    let n1075: ZB = zn_gt(n297, zn_splat(P8::from_raw(0i32)));
    let n1076: ZB = zn_lt(n297, zn_splat(P8::from_raw(0i32)));
    let n1077: ZB = zn_gt(n297, zn_splat(P8::from_raw(65536i32)));
    let n1078: ZB = zn_le(n297, zn_splat(P8::from_raw(65536i32)));
    let n1079: ZN = zn_sub(n297, zn_splat(P8::from_raw(9830i32)));
    let n1080: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1079);
    let n1081: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n297);
    let n1082: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1081);
    let n1083: ZB = zn_gt(n297, zn_splat(P8::from_raw(-65536i32)));
    let n1084: ZB = zn_le(n297, zn_splat(P8::from_raw(-65536i32)));
    let n1085: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1079);
    let n1086: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1081);
    let n1087: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1079);
    let n1088: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1081);
    let n1089: ZN = zsel_n(n1083, n1085, n1086);
    let n1090: ZN = zsel_n(n1075, n1087, n1088);
    let n1091: ZN = zsel_n(n1077, n1080, n1082);
    let n1092: ZN = zsel_n(n1076, n1089, n1090);
    let n1093: ZN = zsel_n(n1075, n1091, n1092);
    let n1094: ZN = zn_sub(n297, n1071);
    let n1095: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1094);
    let n1096: ZN = zn_add(n297, n1071);
    let n1097: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1096);
    let n1098: ZN = zsel_n(n1075, n1095, n1097);
    let n1099: ZN = zsel_n(n1073, n1093, n1098);
    let n1100: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1099);
    let n1101: ZB = zb_not(n1100);
    let n1102: ZB = zn_lt(n1099, zn_splat(P8::from_raw(0i32)));
    let n1103: ZB = zsel_b(n1101, n1102, r_c272);
    let n1104: ZN = zn_abs(n298);
    let n1105: ZB = zn_le(n1104, zn_splat(P8::from_raw(9830i32)));
    let n1106: ZB = zn_gt(n1104, zn_splat(P8::from_raw(9830i32)));
    let n1107: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n301);
    let n1108: ZB = zn_gt(n298, zn_splat(P8::from_raw(131072i32)));
    let n1109: ZB = zn_le(n298, zn_splat(P8::from_raw(131072i32)));
    let n1110: ZB = zn_gt(n1057, zn_splat(P8::from_raw(0i32)));
    let n1111: ZB = zn_le(n1057, zn_splat(P8::from_raw(0i32)));
    let n1112: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n300);
    let n1113: ZB = zn_tile_flag_at(g.cache, g.cart, n1112, n1107, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1114: ZB = zb_not(n1113);
    let n1115: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n300);
    let n1116: ZB = zn_tile_flag_at(g.cache, g.cart, n1115, n1107, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1117: ZB = zb_not(n1116);
    let n1118: ZN = zsel_n(n1116, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1119: ZN = zsel_n(n1113, zn_splat(P8::from_raw(-65536i32)), n1118);
    let n1120: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1119);
    let n1121: ZB = zb_not(n1120);
    let n1122: ZB = zn_gt(n1056, zn_splat(P8::from_raw(0i32)));
    let n1123: ZB = zn_le(n1056, zn_splat(P8::from_raw(0i32)));
    let n1124: ZB = zb_not(n1103);
    let n1125: ZN = zsel_n(n1103, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1126: ZB = zn_gt(n1125, zn_splat(P8::from_raw(0i32)));
    let n1127: ZB = zn_le(n1125, zn_splat(P8::from_raw(0i32)));
    let n1128: ZB = zn_lt(n1125, zn_splat(P8::from_raw(0i32)));
    let n1129: ZB = zn_ge(n1125, zn_splat(P8::from_raw(0i32)));
    let n1130: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1125);
    let n1131: ZB = zb_not(n1130);
    let n1132: ZB = zb_and(n158, n1066);
    let n1133: ZB = zb_and(n159, n1066);
    let n1134: ZB = zb_and(n1067, n1132);
    let n1135: ZB = zb_and(n1068, n1132);
    let n1136: ZB = zb_or(n1134, n1135);
    let n1137: ZB = zb_and(n1069, n1136);
    let n1138: ZB = zb_and(n1070, n1136);
    let n1139: ZB = zb_or(n1137, n1138);
    let n1140: ZB = zb_and(n1049, n1133);
    let n1141: ZB = zb_and(n1048, n1133);
    let n1142: ZB = zb_or(n1140, n1141);
    let n1143: ZB = zb_and(n1073, n1142);
    let n1144: ZB = zb_and(n1074, n1142);
    let n1145: ZB = zb_and(n1075, n1143);
    let n1146: ZB = zb_and(n382, n1143);
    let n1147: ZB = zb_and(n1076, n1146);
    let n1148: ZB = zb_and(n407, n1146);
    let n1149: ZB = zb_and(n1077, n1145);
    let n1150: ZB = zb_and(n1078, n1145);
    let n1151: ZB = zb_and(n1083, n1147);
    let n1152: ZB = zb_and(n1084, n1147);
    let n1153: ZB = zb_and(n382, n1148);
    let n1154: ZB = zb_or(n1151, n1152);
    let n1155: ZB = zb_or(n1149, n1150);
    let n1156: ZB = zb_or(n1153, n1154);
    let n1157: ZB = zb_or(n1155, n1156);
    let n1158: ZB = zb_and(n1075, n1144);
    let n1159: ZB = zb_and(n382, n1144);
    let n1160: ZB = zb_or(n1158, n1159);
    let n1161: ZB = zb_or(n1157, n1160);
    let n1162: ZB = zb_and(n1101, n1161);
    let n1163: ZB = zb_and(n1100, n1161);
    let n1164: ZB = zb_or(n1162, n1163);
    let n1165: ZB = zb_and(n1105, n1164);
    let n1166: ZB = zb_and(n1106, n1164);
    let n1167: ZB = zb_or(n1165, n1166);
    let n1168: ZB = zb_and(n1049, n1167);
    let n1169: ZB = zb_and(n1048, n1167);
    let n1170: ZB = zb_and(n1108, n1168);
    let n1171: ZB = zb_and(n1109, n1168);
    let n1172: ZB = zb_or(n1170, n1171);
    let n1173: ZB = zb_or(n1169, n1172);
    let n1174: ZB = zb_and(n1122, n1173);
    let n1175: ZB = zb_and(n1123, n1173);
    let n1176: ZB = zb_or(n1174, n1175);
    let n1177: ZB = zb_or(n1139, n1176);
    let n1178: ZB = zn_lt(n296, zn_splat(P8::from_raw(-262144i32)));
    let n1179: ZB = zn_ge(n296, zn_splat(P8::from_raw(-262144i32)));
    let n1180: ZB = zb_and(n1177, n1178);
    let n1181: ZB = zb_and(n1177, n1179);
    let n1182: ZB = zb_or(n1180, n1181);
    let n1184: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1186: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1187: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1186);
    let n1197: ZN = zsel_n(n1038, n1187, n1186);
    let n1198: ZN = zsel_n(n1042, n1197, n1186);
    let n1202: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1094);
    let n1203: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1096);
    let n1204: ZN = zsel_n(n1083, n1202, n1203);
    let n1205: ZN = zsel_n(n1073, n1093, n1204);
    let n1206: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1205);
    let n1207: ZB = zb_not(n1206);
    let n1208: ZB = zn_lt(n1205, zn_splat(P8::from_raw(0i32)));
    let n1209: ZB = zsel_b(n1207, n1208, r_c272);
    let n1210: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n300);
    let n1211: ZB = zn_tile_flag_at(g.cache, g.cart, n1210, n1107, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1212: ZB = zb_not(n1211);
    let n1213: ZN = zsel_n(n1211, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1214: ZB = zn_gt(n298, n1213);
    let n1215: ZB = zn_le(n298, n1213);
    let n1216: ZB = zb_and(n1083, n1144);
    let n1217: ZB = zb_and(n1084, n1144);
    let n1218: ZB = zb_or(n1216, n1217);
    let n1219: ZB = zb_or(n1157, n1218);
    let n1220: ZB = zb_and(n1207, n1219);
    let n1221: ZB = zb_and(n1206, n1219);
    let n1222: ZB = zb_or(n1220, n1221);
    let n1223: ZB = zb_and(n1105, n1222);
    let n1224: ZB = zb_and(n1106, n1222);
    let n1225: ZB = zb_or(n1223, n1224);
    let n1226: ZB = zb_and(n1212, n1225);
    let n1227: ZB = zb_and(n1211, n1225);
    let n1228: ZB = zb_or(n1226, n1227);
    let n1229: ZB = zb_and(n1212, n1228);
    let n1230: ZB = zb_and(n1211, n1228);
    let n1231: ZB = zb_or(n1229, n1230);
    let n1232: ZB = zb_and(n1211, n1231);
    let n1233: ZB = zb_and(n1212, n1231);
    let n1234: ZB = zb_or(n1232, n1233);
    let n1235: ZB = zb_and(n1211, n1234);
    let n1236: ZB = zb_and(n1212, n1234);
    let n1237: ZB = zb_or(n1235, n1236);
    let n1238: ZB = zb_and(n1049, n1237);
    let n1239: ZB = zb_and(n1048, n1237);
    let n1240: ZB = zb_and(n1214, n1238);
    let n1241: ZB = zb_and(n1215, n1238);
    let n1242: ZB = zb_or(n1240, n1241);
    let n1243: ZB = zb_or(n1239, n1242);
    let n1244: ZB = zb_and(n1122, n1243);
    let n1245: ZB = zb_and(n1123, n1243);
    let n1246: ZB = zb_or(n1244, n1245);
    let n1247: ZB = zb_or(n1139, n1246);
    let n1248: ZB = zb_and(n1178, n1247);
    let n1249: ZB = zb_and(n1179, n1247);
    let n1250: ZB = zb_or(n1248, n1249);
    let n1253: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1094);
    let n1254: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1096);
    let n1255: ZN = zsel_n(n1077, n1253, n1254);
    let n1256: ZN = zsel_n(n1073, n1093, n1255);
    let n1257: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1256);
    let n1258: ZB = zb_not(n1257);
    let n1259: ZB = zn_lt(n1256, zn_splat(P8::from_raw(0i32)));
    let n1260: ZB = zsel_b(n1258, n1259, r_c272);
    let n1261: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n300);
    let n1262: ZB = zn_tile_flag_at(g.cache, g.cart, n1261, n1107, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1263: ZB = zb_not(n1262);
    let n1264: ZN = zsel_n(n1262, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1265: ZB = zn_gt(n298, n1264);
    let n1266: ZB = zn_le(n298, n1264);
    let n1267: ZB = zb_and(n1077, n1144);
    let n1268: ZB = zb_and(n1078, n1144);
    let n1269: ZB = zb_or(n1267, n1268);
    let n1270: ZB = zb_or(n1157, n1269);
    let n1271: ZB = zb_and(n1258, n1270);
    let n1272: ZB = zb_and(n1257, n1270);
    let n1273: ZB = zb_or(n1271, n1272);
    let n1274: ZB = zb_and(n1105, n1273);
    let n1275: ZB = zb_and(n1106, n1273);
    let n1276: ZB = zb_or(n1274, n1275);
    let n1277: ZB = zb_and(n1263, n1276);
    let n1278: ZB = zb_and(n1262, n1276);
    let n1279: ZB = zb_or(n1277, n1278);
    let n1280: ZB = zb_and(n1263, n1279);
    let n1281: ZB = zb_and(n1262, n1279);
    let n1282: ZB = zb_or(n1280, n1281);
    let n1283: ZB = zb_and(n1262, n1282);
    let n1284: ZB = zb_and(n1263, n1282);
    let n1285: ZB = zb_or(n1283, n1284);
    let n1286: ZB = zb_and(n1262, n1285);
    let n1287: ZB = zb_and(n1263, n1285);
    let n1288: ZB = zb_or(n1286, n1287);
    let n1289: ZB = zb_and(n1049, n1288);
    let n1290: ZB = zb_and(n1048, n1288);
    let n1291: ZB = zb_and(n1265, n1289);
    let n1292: ZB = zb_and(n1266, n1289);
    let n1293: ZB = zb_or(n1291, n1292);
    let n1294: ZB = zb_or(n1290, n1293);
    let n1295: ZB = zb_and(n1122, n1294);
    let n1296: ZB = zb_and(n1123, n1294);
    let n1297: ZB = zb_or(n1295, n1296);
    let n1298: ZB = zb_or(n1139, n1297);
    let n1299: ZB = zb_and(n1178, n1298);
    let n1300: ZB = zb_and(n1179, n1298);
    let n1301: ZB = zb_or(n1299, n1300);
    let n1304: ZB = zb_and(n149, n1173);
    let n1305: ZB = zb_and(r_c247, n1173);
    let n1306: ZB = zb_and(n1110, n1304);
    let n1307: ZB = zb_and(n1111, n1304);
    let n1308: ZB = zb_and(n1114, n1307);
    let n1309: ZB = zb_and(n1113, n1307);
    let n1310: ZB = zb_or(n1308, n1309);
    let n1311: ZB = zb_and(n1114, n1310);
    let n1312: ZB = zb_and(n1113, n1310);
    let n1313: ZB = zb_or(n1311, n1312);
    let n1314: ZB = zb_and(n1113, n1313);
    let n1315: ZB = zb_and(n1114, n1313);
    let n1316: ZB = zb_and(n1117, n1315);
    let n1317: ZB = zb_and(n1116, n1315);
    let n1318: ZB = zb_or(n1316, n1317);
    let n1319: ZB = zb_and(n1117, n1318);
    let n1320: ZB = zb_and(n1116, n1318);
    let n1321: ZB = zb_or(n1319, n1320);
    let n1322: ZB = zb_and(n1116, n1321);
    let n1323: ZB = zb_and(n1117, n1321);
    let n1324: ZB = zb_or(n1322, n1323);
    let n1325: ZB = zb_or(n1314, n1324);
    let n1326: ZB = zb_and(n1121, n1325);
    let n1327: ZB = zb_and(n1120, n1325);
    let n1328: ZB = zb_or(n1326, n1327);
    let n1329: ZB = zb_or(n1306, n1328);
    let n1330: ZB = zb_or(n1305, n1329);
    let n1331: ZB = zb_and(n1122, n1330);
    let n1332: ZB = zb_and(n1123, n1330);
    let n1333: ZB = zb_or(n1331, n1332);
    let n1334: ZB = zb_or(n1139, n1333);
    let n1335: ZB = zb_and(n1178, n1334);
    let n1336: ZB = zb_and(n1179, n1334);
    let n1337: ZB = zb_or(n1335, n1336);
    let n1340: ZB = zb_and(n149, n1243);
    let n1341: ZB = zb_and(r_c247, n1243);
    let n1342: ZB = zb_and(n1110, n1340);
    let n1343: ZB = zb_and(n1111, n1340);
    let n1344: ZB = zb_and(n1114, n1343);
    let n1345: ZB = zb_and(n1113, n1343);
    let n1346: ZB = zb_or(n1344, n1345);
    let n1347: ZB = zb_and(n1114, n1346);
    let n1348: ZB = zb_and(n1113, n1346);
    let n1349: ZB = zb_or(n1347, n1348);
    let n1350: ZB = zb_and(n1113, n1349);
    let n1351: ZB = zb_and(n1114, n1349);
    let n1352: ZB = zb_and(n1117, n1351);
    let n1353: ZB = zb_and(n1116, n1351);
    let n1354: ZB = zb_or(n1352, n1353);
    let n1355: ZB = zb_and(n1117, n1354);
    let n1356: ZB = zb_and(n1116, n1354);
    let n1357: ZB = zb_or(n1355, n1356);
    let n1358: ZB = zb_and(n1116, n1357);
    let n1359: ZB = zb_and(n1117, n1357);
    let n1360: ZB = zb_or(n1358, n1359);
    let n1361: ZB = zb_or(n1350, n1360);
    let n1362: ZB = zb_and(n1121, n1361);
    let n1363: ZB = zb_and(n1120, n1361);
    let n1364: ZB = zb_or(n1362, n1363);
    let n1365: ZB = zb_or(n1342, n1364);
    let n1366: ZB = zb_or(n1341, n1365);
    let n1367: ZB = zb_and(n1122, n1366);
    let n1368: ZB = zb_and(n1123, n1366);
    let n1369: ZB = zb_or(n1367, n1368);
    let n1370: ZB = zb_or(n1139, n1369);
    let n1371: ZB = zb_and(n1178, n1370);
    let n1372: ZB = zb_and(n1179, n1370);
    let n1373: ZB = zb_or(n1371, n1372);
    let n1376: ZB = zb_and(n149, n1294);
    let n1377: ZB = zb_and(r_c247, n1294);
    let n1378: ZB = zb_and(n1110, n1376);
    let n1379: ZB = zb_and(n1111, n1376);
    let n1380: ZB = zb_and(n1114, n1379);
    let n1381: ZB = zb_and(n1113, n1379);
    let n1382: ZB = zb_or(n1380, n1381);
    let n1383: ZB = zb_and(n1114, n1382);
    let n1384: ZB = zb_and(n1113, n1382);
    let n1385: ZB = zb_or(n1383, n1384);
    let n1386: ZB = zb_and(n1113, n1385);
    let n1387: ZB = zb_and(n1114, n1385);
    let n1388: ZB = zb_and(n1117, n1387);
    let n1389: ZB = zb_and(n1116, n1387);
    let n1390: ZB = zb_or(n1388, n1389);
    let n1391: ZB = zb_and(n1117, n1390);
    let n1392: ZB = zb_and(n1116, n1390);
    let n1393: ZB = zb_or(n1391, n1392);
    let n1394: ZB = zb_and(n1116, n1393);
    let n1395: ZB = zb_and(n1117, n1393);
    let n1396: ZB = zb_or(n1394, n1395);
    let n1397: ZB = zb_or(n1386, n1396);
    let n1398: ZB = zb_and(n1121, n1397);
    let n1399: ZB = zb_and(n1120, n1397);
    let n1400: ZB = zb_or(n1398, n1399);
    let n1401: ZB = zb_or(n1378, n1400);
    let n1402: ZB = zb_or(n1377, n1401);
    let n1403: ZB = zb_and(n1122, n1402);
    let n1404: ZB = zb_and(n1123, n1402);
    let n1405: ZB = zb_or(n1403, n1404);
    let n1406: ZB = zb_or(n1139, n1405);
    let n1407: ZB = zb_and(n1178, n1406);
    let n1408: ZB = zb_and(n1179, n1406);
    let n1409: ZB = zb_or(n1407, n1408);
    let n1412: ZB = zb_and(n150, n1122);
    let n1413: ZB = zb_not(n1412);
    let n1414: ZN = zsel_n(n1412, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1415: ZB = zb_or(r_c41, n1412);
    let n1416: ZN = zsel_n(n158, r_c20, n1414);
    let n1417: ZB = zsel_b(n158, r_c41, n1415);
    let n1418: ZB = zb_and(n1176, n1412);
    let n1419: ZB = zb_and(n1176, n1413);
    let n1420: ZB = zb_and(n1103, n1418);
    let n1421: ZB = zb_and(n1124, n1418);
    let n1422: ZB = zb_or(n1420, n1421);
    let n1423: ZB = zb_and(n1126, n1422);
    let n1424: ZB = zb_and(n1127, n1422);
    let n1425: ZB = zb_and(n1128, n1424);
    let n1426: ZB = zb_and(n1129, n1424);
    let n1427: ZB = zb_or(n1425, n1426);
    let n1428: ZB = zb_or(n1423, n1427);
    let n1429: ZB = zb_and(n1131, n1428);
    let n1430: ZB = zb_and(n1130, n1428);
    let n1431: ZB = zb_or(n1429, n1430);
    let n1432: ZB = zb_or(n1419, n1431);
    let n1433: ZB = zb_or(n1139, n1432);
    let n1434: ZB = zb_and(n1178, n1433);
    let n1435: ZB = zb_and(n1179, n1433);
    let n1436: ZB = zb_or(n1434, n1435);
    let n1437: ZB = zb_and(n1179, n1436);
    let n1438: ZB = zn_gt(n1416, zn_splat(P8::from_raw(0i32)));
    let n1439: ZB = zn_le(n1416, zn_splat(P8::from_raw(0i32)));
    let n1440: ZB = zb_and(n1437, n1438);
    let n1441: ZB = zb_and(n1437, n1439);
    let n1442: ZB = zb_or(n1440, n1441);
    let n1443: ZB = zb_and(n1246, n1412);
    let n1444: ZB = zb_and(n1246, n1413);
    let n1445: ZB = zb_or(n1443, n1444);
    let n1446: ZB = zb_or(n1139, n1445);
    let n1447: ZB = zb_and(n1178, n1446);
    let n1448: ZB = zb_and(n1179, n1446);
    let n1449: ZB = zb_or(n1447, n1448);
    let n1450: ZB = zb_and(n1179, n1449);
    let n1451: ZB = zb_and(n1438, n1450);
    let n1452: ZB = zb_and(n1439, n1450);
    let n1453: ZB = zb_or(n1451, n1452);
    let n1454: ZB = zb_and(n1297, n1412);
    let n1455: ZB = zb_and(n1297, n1413);
    let n1456: ZB = zb_or(n1454, n1455);
    let n1457: ZB = zb_or(n1139, n1456);
    let n1458: ZB = zb_and(n1178, n1457);
    let n1459: ZB = zb_and(n1179, n1457);
    let n1460: ZB = zb_or(n1458, n1459);
    let n1461: ZB = zb_and(n1179, n1460);
    let n1462: ZB = zb_and(n1438, n1461);
    let n1463: ZB = zb_and(n1439, n1461);
    let n1464: ZB = zb_or(n1462, n1463);
    let n1465: ZB = zb_or(n1418, n1419);
    let n1466: ZB = zb_or(n1139, n1465);
    let n1467: ZB = zb_and(n1178, n1466);
    let n1468: ZB = zb_and(n1179, n1466);
    let n1469: ZB = zb_or(n1467, n1468);
    let n1470: ZB = zb_and(n1179, n1469);
    let n1471: ZB = zb_and(n1438, n1470);
    let n1472: ZB = zb_and(n1439, n1470);
    let n1473: ZB = zb_or(n1471, n1472);
    let n1474: ZB = zb_and(n1333, n1412);
    let n1475: ZB = zb_and(n1333, n1413);
    let n1476: ZB = zb_and(n1103, n1474);
    let n1477: ZB = zb_and(n1124, n1474);
    let n1478: ZB = zb_or(n1476, n1477);
    let n1479: ZB = zb_and(n1126, n1478);
    let n1480: ZB = zb_and(n1127, n1478);
    let n1481: ZB = zb_and(n1128, n1480);
    let n1482: ZB = zb_and(n1129, n1480);
    let n1483: ZB = zb_or(n1481, n1482);
    let n1484: ZB = zb_or(n1479, n1483);
    let n1485: ZB = zb_and(n1131, n1484);
    let n1486: ZB = zb_and(n1130, n1484);
    let n1487: ZB = zb_or(n1485, n1486);
    let n1488: ZB = zb_or(n1475, n1487);
    let n1489: ZB = zb_or(n1139, n1488);
    let n1490: ZB = zb_and(n1178, n1489);
    let n1491: ZB = zb_and(n1179, n1489);
    let n1492: ZB = zb_or(n1490, n1491);
    let n1493: ZB = zb_and(n1179, n1492);
    let n1494: ZB = zb_and(n1438, n1493);
    let n1495: ZB = zb_and(n1439, n1493);
    let n1496: ZB = zb_or(n1494, n1495);
    let n1497: ZB = zb_and(n1369, n1412);
    let n1498: ZB = zb_and(n1369, n1413);
    let n1499: ZB = zb_or(n1497, n1498);
    let n1500: ZB = zb_or(n1139, n1499);
    let n1501: ZB = zb_and(n1178, n1500);
    let n1502: ZB = zb_and(n1179, n1500);
    let n1503: ZB = zb_or(n1501, n1502);
    let n1504: ZB = zb_and(n1179, n1503);
    let n1505: ZB = zb_and(n1438, n1504);
    let n1506: ZB = zb_and(n1439, n1504);
    let n1507: ZB = zb_or(n1505, n1506);
    let n1508: ZB = zb_and(n1405, n1412);
    let n1509: ZB = zb_and(n1405, n1413);
    let n1510: ZB = zb_or(n1508, n1509);
    let n1511: ZB = zb_or(n1139, n1510);
    let n1512: ZB = zb_and(n1178, n1511);
    let n1513: ZB = zb_and(n1179, n1511);
    let n1514: ZB = zb_or(n1512, n1513);
    let n1515: ZB = zb_and(n1179, n1514);
    let n1516: ZB = zb_and(n1438, n1515);
    let n1517: ZB = zb_and(n1439, n1515);
    let n1518: ZB = zb_or(n1516, n1517);
    let n1519: ZB = zb_or(n1474, n1475);
    let n1520: ZB = zb_or(n1139, n1519);
    let n1521: ZB = zb_and(n1178, n1520);
    let n1522: ZB = zb_and(n1179, n1520);
    let n1523: ZB = zb_or(n1521, n1522);
    let n1524: ZB = zb_and(n1179, n1523);
    let n1525: ZB = zb_and(n1438, n1524);
    let n1526: ZB = zb_and(n1439, n1524);
    let n1527: ZB = zb_or(n1525, n1526);
    let n1535: ZB = zb_and(n1036, n1039);
    let n1536: ZB = zb_and(n1049, n1535);
    let n1537: ZB = zb_and(n1048, n1535);
    let n1538: ZB = zb_or(n1536, n1537);
    let n1539: ZB = zb_and(n1049, n1538);
    let n1540: ZB = zb_and(n1048, n1538);
    let n1541: ZB = zb_or(n1539, n1540);
    let n1542: ZB = zb_and(n1048, n1541);
    let n1543: ZB = zb_and(n1049, n1541);
    let n1544: ZB = zb_and(n151, n1542);
    let n1545: ZB = zb_and(n152, n1542);
    let n1546: ZB = zb_or(n1544, n1545);
    let n1547: ZB = zb_and(n154, n1543);
    let n1548: ZB = zb_and(n155, n1543);
    let n1549: ZB = zb_or(n1547, n1548);
    let n1550: ZB = zb_or(n1546, n1549);
    let n1551: ZB = zb_and(n158, n1550);
    let n1552: ZB = zb_and(n159, n1550);
    let n1553: ZB = zb_and(n1067, n1551);
    let n1554: ZB = zb_and(n1068, n1551);
    let n1555: ZB = zb_or(n1553, n1554);
    let n1556: ZB = zb_and(n1069, n1555);
    let n1557: ZB = zb_and(n1070, n1555);
    let n1558: ZB = zb_or(n1556, n1557);
    let n1559: ZB = zb_and(n1049, n1552);
    let n1560: ZB = zb_and(n1048, n1552);
    let n1561: ZB = zb_or(n1559, n1560);
    let n1562: ZB = zb_and(n1073, n1561);
    let n1563: ZB = zb_and(n1074, n1561);
    let n1564: ZB = zb_and(n1075, n1562);
    let n1565: ZB = zb_and(n382, n1562);
    let n1566: ZB = zb_and(n1076, n1565);
    let n1567: ZB = zb_and(n407, n1565);
    let n1568: ZB = zb_and(n1077, n1564);
    let n1569: ZB = zb_and(n1078, n1564);
    let n1570: ZB = zb_and(n1083, n1566);
    let n1571: ZB = zb_and(n1084, n1566);
    let n1572: ZB = zb_and(n382, n1567);
    let n1573: ZB = zb_or(n1570, n1571);
    let n1574: ZB = zb_or(n1568, n1569);
    let n1575: ZB = zb_or(n1572, n1573);
    let n1576: ZB = zb_or(n1574, n1575);
    let n1577: ZB = zb_and(n1075, n1563);
    let n1578: ZB = zb_and(n382, n1563);
    let n1579: ZB = zb_or(n1577, n1578);
    let n1580: ZB = zb_or(n1576, n1579);
    let n1581: ZB = zb_and(n1101, n1580);
    let n1582: ZB = zb_and(n1100, n1580);
    let n1583: ZB = zb_or(n1581, n1582);
    let n1584: ZB = zb_and(n1105, n1583);
    let n1585: ZB = zb_and(n1106, n1583);
    let n1586: ZB = zb_or(n1584, n1585);
    let n1587: ZB = zb_and(n1049, n1586);
    let n1588: ZB = zb_and(n1048, n1586);
    let n1589: ZB = zb_and(n1108, n1587);
    let n1590: ZB = zb_and(n1109, n1587);
    let n1591: ZB = zb_or(n1589, n1590);
    let n1592: ZB = zb_or(n1588, n1591);
    let n1593: ZB = zb_and(n1122, n1592);
    let n1594: ZB = zb_and(n1123, n1592);
    let n1595: ZB = zb_or(n1593, n1594);
    let n1596: ZB = zb_or(n1558, n1595);
    let n1597: ZB = zb_and(n1178, n1596);
    let n1598: ZB = zb_and(n1179, n1596);
    let n1599: ZB = zb_or(n1597, n1598);
    let n1600: ZB = zb_and(n1178, n1599);
    let n1601: ZB = zb_and(n1178, n1182);
    let n1602: ZB = zb_not(n1600);
    let n1603: ZB = zb_or(n1600, n1601);
    let n1604: ZB = zsel_b(n1600, n1037, n1045);
    let n1606: ZN = zsel_n(n1600, r_c87, n1198);
    let n1607: ZN = zsel_n(n1600, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1609: ZB = zb_and(n1083, n1563);
    let n1610: ZB = zb_and(n1084, n1563);
    let n1611: ZB = zb_or(n1609, n1610);
    let n1612: ZB = zb_or(n1576, n1611);
    let n1613: ZB = zb_and(n1207, n1612);
    let n1614: ZB = zb_and(n1206, n1612);
    let n1615: ZB = zb_or(n1613, n1614);
    let n1616: ZB = zb_and(n1105, n1615);
    let n1617: ZB = zb_and(n1106, n1615);
    let n1618: ZB = zb_or(n1616, n1617);
    let n1619: ZB = zb_and(n1212, n1618);
    let n1620: ZB = zb_and(n1211, n1618);
    let n1621: ZB = zb_or(n1619, n1620);
    let n1622: ZB = zb_and(n1212, n1621);
    let n1623: ZB = zb_and(n1211, n1621);
    let n1624: ZB = zb_or(n1622, n1623);
    let n1625: ZB = zb_and(n1211, n1624);
    let n1626: ZB = zb_and(n1212, n1624);
    let n1627: ZB = zb_or(n1625, n1626);
    let n1628: ZB = zb_and(n1211, n1627);
    let n1629: ZB = zb_and(n1212, n1627);
    let n1630: ZB = zb_or(n1628, n1629);
    let n1631: ZB = zb_and(n1049, n1630);
    let n1632: ZB = zb_and(n1048, n1630);
    let n1633: ZB = zb_and(n1214, n1631);
    let n1634: ZB = zb_and(n1215, n1631);
    let n1635: ZB = zb_or(n1633, n1634);
    let n1636: ZB = zb_or(n1632, n1635);
    let n1637: ZB = zb_and(n1122, n1636);
    let n1638: ZB = zb_and(n1123, n1636);
    let n1639: ZB = zb_or(n1637, n1638);
    let n1640: ZB = zb_or(n1558, n1639);
    let n1641: ZB = zb_and(n1178, n1640);
    let n1642: ZB = zb_and(n1179, n1640);
    let n1643: ZB = zb_or(n1641, n1642);
    let n1644: ZB = zb_and(n1178, n1643);
    let n1645: ZB = zb_and(n1178, n1250);
    let n1646: ZB = zb_not(n1644);
    let n1647: ZB = zb_or(n1644, n1645);
    let n1648: ZB = zsel_b(n1644, n1037, n1045);
    let n1650: ZN = zsel_n(n1644, r_c87, n1198);
    let n1651: ZN = zsel_n(n1644, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1653: ZB = zb_and(n1077, n1563);
    let n1654: ZB = zb_and(n1078, n1563);
    let n1655: ZB = zb_or(n1653, n1654);
    let n1656: ZB = zb_or(n1576, n1655);
    let n1657: ZB = zb_and(n1258, n1656);
    let n1658: ZB = zb_and(n1257, n1656);
    let n1659: ZB = zb_or(n1657, n1658);
    let n1660: ZB = zb_and(n1105, n1659);
    let n1661: ZB = zb_and(n1106, n1659);
    let n1662: ZB = zb_or(n1660, n1661);
    let n1663: ZB = zb_and(n1263, n1662);
    let n1664: ZB = zb_and(n1262, n1662);
    let n1665: ZB = zb_or(n1663, n1664);
    let n1666: ZB = zb_and(n1263, n1665);
    let n1667: ZB = zb_and(n1262, n1665);
    let n1668: ZB = zb_or(n1666, n1667);
    let n1669: ZB = zb_and(n1262, n1668);
    let n1670: ZB = zb_and(n1263, n1668);
    let n1671: ZB = zb_or(n1669, n1670);
    let n1672: ZB = zb_and(n1262, n1671);
    let n1673: ZB = zb_and(n1263, n1671);
    let n1674: ZB = zb_or(n1672, n1673);
    let n1675: ZB = zb_and(n1049, n1674);
    let n1676: ZB = zb_and(n1048, n1674);
    let n1677: ZB = zb_and(n1265, n1675);
    let n1678: ZB = zb_and(n1266, n1675);
    let n1679: ZB = zb_or(n1677, n1678);
    let n1680: ZB = zb_or(n1676, n1679);
    let n1681: ZB = zb_and(n1122, n1680);
    let n1682: ZB = zb_and(n1123, n1680);
    let n1683: ZB = zb_or(n1681, n1682);
    let n1684: ZB = zb_or(n1558, n1683);
    let n1685: ZB = zb_and(n1178, n1684);
    let n1686: ZB = zb_and(n1179, n1684);
    let n1687: ZB = zb_or(n1685, n1686);
    let n1688: ZB = zb_and(n1178, n1687);
    let n1689: ZB = zb_and(n1178, n1301);
    let n1690: ZB = zb_not(n1688);
    let n1691: ZB = zb_or(n1688, n1689);
    let n1692: ZB = zsel_b(n1688, n1037, n1045);
    let n1694: ZN = zsel_n(n1688, r_c87, n1198);
    let n1695: ZN = zsel_n(n1688, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1697: ZB = zb_and(n149, n1592);
    let n1698: ZB = zb_and(r_c247, n1592);
    let n1699: ZB = zb_and(n1110, n1697);
    let n1700: ZB = zb_and(n1111, n1697);
    let n1701: ZB = zb_and(n1114, n1700);
    let n1702: ZB = zb_and(n1113, n1700);
    let n1703: ZB = zb_or(n1701, n1702);
    let n1704: ZB = zb_and(n1114, n1703);
    let n1705: ZB = zb_and(n1113, n1703);
    let n1706: ZB = zb_or(n1704, n1705);
    let n1707: ZB = zb_and(n1113, n1706);
    let n1708: ZB = zb_and(n1114, n1706);
    let n1709: ZB = zb_and(n1117, n1708);
    let n1710: ZB = zb_and(n1116, n1708);
    let n1711: ZB = zb_or(n1709, n1710);
    let n1712: ZB = zb_and(n1117, n1711);
    let n1713: ZB = zb_and(n1116, n1711);
    let n1714: ZB = zb_or(n1712, n1713);
    let n1715: ZB = zb_and(n1116, n1714);
    let n1716: ZB = zb_and(n1117, n1714);
    let n1717: ZB = zb_or(n1715, n1716);
    let n1718: ZB = zb_or(n1707, n1717);
    let n1719: ZB = zb_and(n1121, n1718);
    let n1720: ZB = zb_and(n1120, n1718);
    let n1721: ZB = zb_or(n1719, n1720);
    let n1722: ZB = zb_or(n1699, n1721);
    let n1723: ZB = zb_or(n1698, n1722);
    let n1724: ZB = zb_and(n1122, n1723);
    let n1725: ZB = zb_and(n1123, n1723);
    let n1726: ZB = zb_or(n1724, n1725);
    let n1727: ZB = zb_or(n1558, n1726);
    let n1728: ZB = zb_and(n1178, n1727);
    let n1729: ZB = zb_and(n1179, n1727);
    let n1730: ZB = zb_or(n1728, n1729);
    let n1731: ZB = zb_and(n1178, n1730);
    let n1732: ZB = zb_and(n1178, n1337);
    let n1733: ZB = zb_not(n1731);
    let n1734: ZB = zb_or(n1731, n1732);
    let n1735: ZB = zsel_b(n1731, n1037, n1045);
    let n1737: ZN = zsel_n(n1731, r_c87, n1198);
    let n1738: ZN = zsel_n(n1731, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1740: ZB = zb_and(n149, n1636);
    let n1741: ZB = zb_and(r_c247, n1636);
    let n1742: ZB = zb_and(n1110, n1740);
    let n1743: ZB = zb_and(n1111, n1740);
    let n1744: ZB = zb_and(n1114, n1743);
    let n1745: ZB = zb_and(n1113, n1743);
    let n1746: ZB = zb_or(n1744, n1745);
    let n1747: ZB = zb_and(n1114, n1746);
    let n1748: ZB = zb_and(n1113, n1746);
    let n1749: ZB = zb_or(n1747, n1748);
    let n1750: ZB = zb_and(n1113, n1749);
    let n1751: ZB = zb_and(n1114, n1749);
    let n1752: ZB = zb_and(n1117, n1751);
    let n1753: ZB = zb_and(n1116, n1751);
    let n1754: ZB = zb_or(n1752, n1753);
    let n1755: ZB = zb_and(n1117, n1754);
    let n1756: ZB = zb_and(n1116, n1754);
    let n1757: ZB = zb_or(n1755, n1756);
    let n1758: ZB = zb_and(n1116, n1757);
    let n1759: ZB = zb_and(n1117, n1757);
    let n1760: ZB = zb_or(n1758, n1759);
    let n1761: ZB = zb_or(n1750, n1760);
    let n1762: ZB = zb_and(n1121, n1761);
    let n1763: ZB = zb_and(n1120, n1761);
    let n1764: ZB = zb_or(n1762, n1763);
    let n1765: ZB = zb_or(n1742, n1764);
    let n1766: ZB = zb_or(n1741, n1765);
    let n1767: ZB = zb_and(n1122, n1766);
    let n1768: ZB = zb_and(n1123, n1766);
    let n1769: ZB = zb_or(n1767, n1768);
    let n1770: ZB = zb_or(n1558, n1769);
    let n1771: ZB = zb_and(n1178, n1770);
    let n1772: ZB = zb_and(n1179, n1770);
    let n1773: ZB = zb_or(n1771, n1772);
    let n1774: ZB = zb_and(n1178, n1773);
    let n1775: ZB = zb_and(n1178, n1373);
    let n1776: ZB = zb_not(n1774);
    let n1777: ZB = zb_or(n1774, n1775);
    let n1778: ZB = zsel_b(n1774, n1037, n1045);
    let n1780: ZN = zsel_n(n1774, r_c87, n1198);
    let n1781: ZN = zsel_n(n1774, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1783: ZB = zb_and(n149, n1680);
    let n1784: ZB = zb_and(r_c247, n1680);
    let n1785: ZB = zb_and(n1110, n1783);
    let n1786: ZB = zb_and(n1111, n1783);
    let n1787: ZB = zb_and(n1114, n1786);
    let n1788: ZB = zb_and(n1113, n1786);
    let n1789: ZB = zb_or(n1787, n1788);
    let n1790: ZB = zb_and(n1114, n1789);
    let n1791: ZB = zb_and(n1113, n1789);
    let n1792: ZB = zb_or(n1790, n1791);
    let n1793: ZB = zb_and(n1113, n1792);
    let n1794: ZB = zb_and(n1114, n1792);
    let n1795: ZB = zb_and(n1117, n1794);
    let n1796: ZB = zb_and(n1116, n1794);
    let n1797: ZB = zb_or(n1795, n1796);
    let n1798: ZB = zb_and(n1117, n1797);
    let n1799: ZB = zb_and(n1116, n1797);
    let n1800: ZB = zb_or(n1798, n1799);
    let n1801: ZB = zb_and(n1116, n1800);
    let n1802: ZB = zb_and(n1117, n1800);
    let n1803: ZB = zb_or(n1801, n1802);
    let n1804: ZB = zb_or(n1793, n1803);
    let n1805: ZB = zb_and(n1121, n1804);
    let n1806: ZB = zb_and(n1120, n1804);
    let n1807: ZB = zb_or(n1805, n1806);
    let n1808: ZB = zb_or(n1785, n1807);
    let n1809: ZB = zb_or(n1784, n1808);
    let n1810: ZB = zb_and(n1122, n1809);
    let n1811: ZB = zb_and(n1123, n1809);
    let n1812: ZB = zb_or(n1810, n1811);
    let n1813: ZB = zb_or(n1558, n1812);
    let n1814: ZB = zb_and(n1178, n1813);
    let n1815: ZB = zb_and(n1179, n1813);
    let n1816: ZB = zb_or(n1814, n1815);
    let n1817: ZB = zb_and(n1178, n1816);
    let n1818: ZB = zb_and(n1178, n1409);
    let n1819: ZB = zb_not(n1817);
    let n1820: ZB = zb_or(n1817, n1818);
    let n1821: ZB = zsel_b(n1817, n1037, n1045);
    let n1823: ZN = zsel_n(n1817, r_c87, n1198);
    let n1824: ZN = zsel_n(n1817, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1826: ZB = zb_and(n1412, n1595);
    let n1827: ZB = zb_and(n1413, n1595);
    let n1828: ZB = zb_and(n1103, n1826);
    let n1829: ZB = zb_and(n1124, n1826);
    let n1830: ZB = zb_or(n1828, n1829);
    let n1831: ZB = zb_and(n1126, n1830);
    let n1832: ZB = zb_and(n1127, n1830);
    let n1833: ZB = zb_and(n1128, n1832);
    let n1834: ZB = zb_and(n1129, n1832);
    let n1835: ZB = zb_or(n1833, n1834);
    let n1836: ZB = zb_or(n1831, n1835);
    let n1837: ZB = zb_and(n1131, n1836);
    let n1838: ZB = zb_and(n1130, n1836);
    let n1839: ZB = zb_or(n1837, n1838);
    let n1840: ZB = zb_or(n1827, n1839);
    let n1841: ZB = zb_or(n1558, n1840);
    let n1842: ZB = zb_and(n1178, n1841);
    let n1843: ZB = zb_and(n1179, n1841);
    let n1844: ZB = zb_or(n1842, n1843);
    let n1845: ZB = zb_and(n1178, n1844);
    let n1846: ZB = zb_and(n1178, n1436);
    let n1847: ZB = zb_not(n1845);
    let n1848: ZB = zb_or(n1845, n1846);
    let n1849: ZB = zsel_b(n1845, n1037, n1045);
    let n1850: ZB = zb_and(n1438, n1848);
    let n1851: ZB = zb_and(n1439, n1848);
    let n1852: ZB = zb_or(n1850, n1851);
    let n1853: ZN = zsel_n(n1845, r_c87, n1198);
    let n1854: ZN = zsel_n(n1845, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1856: ZB = zb_and(n1412, n1639);
    let n1857: ZB = zb_and(n1413, n1639);
    let n1858: ZB = zb_or(n1856, n1857);
    let n1859: ZB = zb_or(n1558, n1858);
    let n1860: ZB = zb_and(n1178, n1859);
    let n1861: ZB = zb_and(n1179, n1859);
    let n1862: ZB = zb_or(n1860, n1861);
    let n1863: ZB = zb_and(n1178, n1862);
    let n1864: ZB = zb_and(n1178, n1449);
    let n1865: ZB = zb_not(n1863);
    let n1866: ZB = zb_or(n1863, n1864);
    let n1867: ZB = zsel_b(n1863, n1037, n1045);
    let n1868: ZB = zb_and(n1438, n1866);
    let n1869: ZB = zb_and(n1439, n1866);
    let n1870: ZB = zb_or(n1868, n1869);
    let n1871: ZN = zsel_n(n1863, r_c87, n1198);
    let n1872: ZN = zsel_n(n1863, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1874: ZB = zb_and(n1412, n1683);
    let n1875: ZB = zb_and(n1413, n1683);
    let n1876: ZB = zb_or(n1874, n1875);
    let n1877: ZB = zb_or(n1558, n1876);
    let n1878: ZB = zb_and(n1178, n1877);
    let n1879: ZB = zb_and(n1179, n1877);
    let n1880: ZB = zb_or(n1878, n1879);
    let n1881: ZB = zb_and(n1178, n1880);
    let n1882: ZB = zb_and(n1178, n1460);
    let n1883: ZB = zb_not(n1881);
    let n1884: ZB = zb_or(n1881, n1882);
    let n1885: ZB = zsel_b(n1881, n1037, n1045);
    let n1886: ZB = zb_and(n1438, n1884);
    let n1887: ZB = zb_and(n1439, n1884);
    let n1888: ZB = zb_or(n1886, n1887);
    let n1889: ZN = zsel_n(n1881, r_c87, n1198);
    let n1890: ZN = zsel_n(n1881, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1892: ZB = zb_or(n1826, n1827);
    let n1893: ZB = zb_or(n1558, n1892);
    let n1894: ZB = zb_and(n1178, n1893);
    let n1895: ZB = zb_and(n1179, n1893);
    let n1896: ZB = zb_or(n1894, n1895);
    let n1897: ZB = zb_and(n1178, n1896);
    let n1898: ZB = zb_and(n1178, n1469);
    let n1899: ZB = zb_not(n1897);
    let n1900: ZB = zb_or(n1897, n1898);
    let n1901: ZB = zsel_b(n1897, n1037, n1045);
    let n1902: ZB = zb_and(n1438, n1900);
    let n1903: ZB = zb_and(n1439, n1900);
    let n1904: ZB = zb_or(n1902, n1903);
    let n1905: ZN = zsel_n(n1897, r_c87, n1198);
    let n1906: ZN = zsel_n(n1897, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1908: ZB = zb_and(n1412, n1726);
    let n1909: ZB = zb_and(n1413, n1726);
    let n1910: ZB = zb_and(n1103, n1908);
    let n1911: ZB = zb_and(n1124, n1908);
    let n1912: ZB = zb_or(n1910, n1911);
    let n1913: ZB = zb_and(n1126, n1912);
    let n1914: ZB = zb_and(n1127, n1912);
    let n1915: ZB = zb_and(n1128, n1914);
    let n1916: ZB = zb_and(n1129, n1914);
    let n1917: ZB = zb_or(n1915, n1916);
    let n1918: ZB = zb_or(n1913, n1917);
    let n1919: ZB = zb_and(n1131, n1918);
    let n1920: ZB = zb_and(n1130, n1918);
    let n1921: ZB = zb_or(n1919, n1920);
    let n1922: ZB = zb_or(n1909, n1921);
    let n1923: ZB = zb_or(n1558, n1922);
    let n1924: ZB = zb_and(n1178, n1923);
    let n1925: ZB = zb_and(n1179, n1923);
    let n1926: ZB = zb_or(n1924, n1925);
    let n1927: ZB = zb_and(n1178, n1926);
    let n1928: ZB = zb_and(n1178, n1492);
    let n1929: ZB = zb_not(n1927);
    let n1930: ZB = zb_or(n1927, n1928);
    let n1931: ZB = zsel_b(n1927, n1037, n1045);
    let n1932: ZB = zb_and(n1438, n1930);
    let n1933: ZB = zb_and(n1439, n1930);
    let n1934: ZB = zb_or(n1932, n1933);
    let n1935: ZN = zsel_n(n1927, r_c87, n1198);
    let n1936: ZN = zsel_n(n1927, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1938: ZB = zb_and(n1412, n1769);
    let n1939: ZB = zb_and(n1413, n1769);
    let n1940: ZB = zb_or(n1938, n1939);
    let n1941: ZB = zb_or(n1558, n1940);
    let n1942: ZB = zb_and(n1178, n1941);
    let n1943: ZB = zb_and(n1179, n1941);
    let n1944: ZB = zb_or(n1942, n1943);
    let n1945: ZB = zb_and(n1178, n1944);
    let n1946: ZB = zb_and(n1178, n1503);
    let n1947: ZB = zb_not(n1945);
    let n1948: ZB = zb_or(n1945, n1946);
    let n1949: ZB = zsel_b(n1945, n1037, n1045);
    let n1950: ZB = zb_and(n1438, n1948);
    let n1951: ZB = zb_and(n1439, n1948);
    let n1952: ZB = zb_or(n1950, n1951);
    let n1953: ZN = zsel_n(n1945, r_c87, n1198);
    let n1954: ZN = zsel_n(n1945, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1956: ZB = zb_and(n1412, n1812);
    let n1957: ZB = zb_and(n1413, n1812);
    let n1958: ZB = zb_or(n1956, n1957);
    let n1959: ZB = zb_or(n1558, n1958);
    let n1960: ZB = zb_and(n1178, n1959);
    let n1961: ZB = zb_and(n1179, n1959);
    let n1962: ZB = zb_or(n1960, n1961);
    let n1963: ZB = zb_and(n1178, n1962);
    let n1964: ZB = zb_and(n1178, n1514);
    let n1965: ZB = zb_not(n1963);
    let n1966: ZB = zb_or(n1963, n1964);
    let n1967: ZB = zsel_b(n1963, n1037, n1045);
    let n1968: ZB = zb_and(n1438, n1966);
    let n1969: ZB = zb_and(n1439, n1966);
    let n1970: ZB = zb_or(n1968, n1969);
    let n1971: ZN = zsel_n(n1963, r_c87, n1198);
    let n1972: ZN = zsel_n(n1963, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1974: ZB = zb_or(n1908, n1909);
    let n1975: ZB = zb_or(n1558, n1974);
    let n1976: ZB = zb_and(n1178, n1975);
    let n1977: ZB = zb_and(n1179, n1975);
    let n1978: ZB = zb_or(n1976, n1977);
    let n1979: ZB = zb_and(n1178, n1978);
    let n1980: ZB = zb_and(n1178, n1523);
    let n1981: ZB = zb_not(n1979);
    let n1982: ZB = zb_or(n1979, n1980);
    let n1983: ZB = zsel_b(n1979, n1037, n1045);
    let n1984: ZB = zb_and(n1438, n1982);
    let n1985: ZB = zb_and(n1439, n1982);
    let n1986: ZB = zb_or(n1984, n1985);
    let n1987: ZN = zsel_n(n1979, r_c87, n1198);
    let n1988: ZN = zsel_n(n1979, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2000: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2001: ZN = zn_sub(n93, zn_splat(P8::from_raw(32768i32)));
    let n2002: ZN = zn_sub(n2001, n94);
    let n2003: ZN = zn_sub(n111, zn_splat(P8::from_raw(32768i32)));
    let n2004: ZN = zn_sub(n2003, n112);
    let n2005: ZN = zn_sub(r_c234, zn_splat(P8::from_raw(65536i32)));
    let n2006: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n2007: ZB = zb_and(r_c246, n1184);
    let n2008: ZB = zb_and(r_c247, n1184);
    let n2009: ZN = zsel_n(n192, zn_splat(P8::from_raw(0i32)), n2002);
    let n2010: ZN = zsel_n(n108, n2002, n2009);
    let n2011: ZN = zsel_n(n188, zn_splat(P8::from_raw(0i32)), n2010);
    let n2012: ZN = zsel_n(n107, n2002, n2011);
    let n2013: ZN = zsel_n(n184, zn_splat(P8::from_raw(0i32)), n2012);
    let n2014: ZN = zsel_n(n106, n2002, n2013);
    let n2015: ZN = zsel_n(n180, zn_splat(P8::from_raw(0i32)), n2014);
    let n2016: ZN = zsel_n(n105, n2002, n2015);
    let n2017: ZN = zsel_n(n176, zn_splat(P8::from_raw(0i32)), n2016);
    let n2018: ZN = zsel_n(n104, n2002, n2017);
    let n2019: ZN = zsel_n(n172, zn_splat(P8::from_raw(0i32)), n2018);
    let n2020: ZN = zsel_n(n103, n2002, n2019);
    let n2021: ZN = zsel_n(n168, zn_splat(P8::from_raw(0i32)), n2020);
    let n2022: ZN = zsel_n(n102, n2002, n2021);
    let n2023: ZN = zsel_n(n164, zn_splat(P8::from_raw(0i32)), n2022);
    let n2024: ZN = zsel_n(n248, zn_splat(P8::from_raw(0i32)), n2004);
    let n2025: ZN = zsel_n(n144, n2004, n2024);
    let n2026: ZN = zsel_n(n247, zn_splat(P8::from_raw(0i32)), n2025);
    let n2027: ZN = zsel_n(n140, n2004, n2026);
    let n2028: ZN = zsel_n(n246, zn_splat(P8::from_raw(0i32)), n2027);
    let n2029: ZN = zsel_n(n136, n2004, n2028);
    let n2030: ZN = zsel_n(n245, zn_splat(P8::from_raw(0i32)), n2029);
    let n2031: ZN = zsel_n(n132, n2004, n2030);
    let n2032: ZN = zsel_n(n244, zn_splat(P8::from_raw(0i32)), n2031);
    let n2033: ZN = zsel_n(n128, n2004, n2032);
    let n2034: ZN = zsel_n(n243, zn_splat(P8::from_raw(0i32)), n2033);
    let n2035: ZN = zsel_n(n124, n2004, n2034);
    let n2036: ZN = zsel_n(n242, zn_splat(P8::from_raw(0i32)), n2035);
    let n2037: ZN = zsel_n(n120, n2004, n2036);
    let n2038: ZN = zsel_n(n241, zn_splat(P8::from_raw(0i32)), n2037);
    let n2039: ZN = zsel_n(n90, n2023, r_c278);
    let n2040: ZN = zsel_n(n90, n2038, r_c279);
    let n2041: ZN = zn_sub(n297, r_c268);
    let n2042: ZN = zn_max(r_c270, n2041);
    let n2043: ZN = zn_add(n297, r_c268);
    let n2044: ZN = zn_min(r_c270, n2043);
    let n2045: ZN = zsel_n(n1067, n2042, n2044);
    let n2046: ZN = zn_sub(n298, r_c269);
    let n2047: ZN = zn_max(r_c271, n2046);
    let n2048: ZN = zn_add(n298, r_c269);
    let n2049: ZN = zn_min(r_c271, n2048);
    let n2050: ZN = zsel_n(n1069, n2047, n2049);
    let n2051: ZN = zsel_n(n1105, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2052: ZN = zn_sub(n298, n2051);
    let n2053: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2052);
    let n2054: ZN = zn_add(n298, n2051);
    let n2055: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2054);
    let n2056: ZN = zsel_n(n1108, n2053, n2055);
    let n2057: ZN = zsel_n(n1049, n2056, n298);
    let n2058: ZN = zn_neg(n1119);
    let n2059: ZN = zn_mul(n2058, zn_splat(P8::from_raw(131072i32)));
    let n2060: ZN = zsel_n(n1121, n2059, n1099);
    let n2061: ZN = zsel_n(n1121, zn_splat(P8::from_raw(-131072i32)), n2057);
    let n2062: ZN = zsel_n(n1110, zn_splat(P8::from_raw(0i32)), n1057);
    let n2063: ZN = zsel_n(n1110, n1099, n2060);
    let n2064: ZN = zsel_n(n1110, zn_splat(P8::from_raw(-131072i32)), n2061);
    let n2065: ZN = zn_sub(n1056, zn_splat(P8::from_raw(65536i32)));
    let n2066: ZN = zsel_n(n1128, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2067: ZN = zsel_n(n1126, zn_splat(P8::from_raw(131072i32)), n2066);
    let n2068: ZN = zsel_n(n1131, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2069: ZN = zsel_n(n158, n2006, r_c236);
    let n2070: ZB = zsel_b(n158, r_c272, n1103);
    let n2071: ZN = zsel_n(n158, n2045, n1099);
    let n2072: ZN = zsel_n(n158, n2050, n2057);
    let n2073: ZB = zb_and(n1179, n1599);
    let n2074: ZN = zsel_n(n1184, n2000, r_c20);
    let n2075: ZN = zsel_n(n1184, r_c234, n2005);
    let n2076: ZN = zsel_n(n1184, r_c236, n2069);
    let n2077: ZN = zsel_n(n1184, r_c237, n1056);
    let n2078: ZN = zsel_n(n1184, r_c239, n1057);
    let n2079: ZN = zsel_n(n1184, r_c253, n295);
    let n2080: ZN = zsel_n(n1184, r_c254, n296);
    let n2081: ZB = zsel_b(n1184, r_c272, n2070);
    let n2082: ZN = zsel_n(n1184, r_c278, n2039);
    let n2083: ZN = zsel_n(n1184, r_c279, n2040);
    let n2084: ZN = zsel_n(n1184, r_c280, n2071);
    let n2085: ZN = zsel_n(n1184, r_c281, n2072);
    let n2086: ZB = zb_or(n1184, n2073);
    let n2087: ZB = zb_or(n1037, n1184);
    let n2088: ZB = zn_gt(n2074, zn_splat(P8::from_raw(0i32)));
    let n2089: ZB = zn_le(n2074, zn_splat(P8::from_raw(0i32)));
    let n2090: ZB = zb_and(n2086, n2088);
    let n2091: ZB = zb_and(n2086, n2089);
    let n2092: ZB = zn_lt(n2079, zn_splat(P8::from_raw(-65536i32)));
    let n2093: ZB = zn_ge(n2079, zn_splat(P8::from_raw(-65536i32)));
    let n2094: ZB = zb_and(n2091, n2093);
    let n2095: ZB = zb_and(n2091, n2092);
    let n2096: ZB = zn_gt(n2079, zn_splat(P8::from_raw(7929856i32)));
    let n2097: ZB = zb_or(n2094, n2095);
    let n2098: ZB = zb_or(n2092, n2096);
    let n2099: ZB = zb_not(n2098);
    let n2100: ZB = zb_and(n2097, n2098);
    let n2101: ZB = zb_and(n2097, n2099);
    let n2102: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2079);
    let n2103: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2102);
    let n2104: ZN = zsel_n(n2098, n2103, n2079);
    let n2105: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2084);
    let n2106: ZB = zb_or(n2100, n2101);
    let n2107: ZN = zsel_n(n2088, n2079, n2104);
    let n2108: ZN = zsel_n(n2088, n2084, n2105);
    let n2109: ZB = zb_or(n2090, n2106);
    let n2111: ZN = zn_max(n1213, n2052);
    let n2112: ZN = zn_min(n1213, n2054);
    let n2113: ZN = zsel_n(n1214, n2111, n2112);
    let n2114: ZN = zsel_n(n1049, n2113, n298);
    let n2115: ZN = zsel_n(n1121, n2059, n1205);
    let n2116: ZN = zsel_n(n1121, zn_splat(P8::from_raw(-131072i32)), n2114);
    let n2117: ZN = zsel_n(n1110, n1205, n2115);
    let n2118: ZN = zsel_n(n1110, zn_splat(P8::from_raw(-131072i32)), n2116);
    let n2119: ZB = zsel_b(n158, r_c272, n1209);
    let n2120: ZN = zsel_n(n158, n2045, n1205);
    let n2121: ZN = zsel_n(n158, n2050, n2114);
    let n2122: ZB = zb_and(n1179, n1643);
    let n2123: ZB = zsel_b(n1184, r_c272, n2119);
    let n2124: ZN = zsel_n(n1184, r_c280, n2120);
    let n2125: ZN = zsel_n(n1184, r_c281, n2121);
    let n2126: ZB = zb_or(n1184, n2122);
    let n2127: ZB = zb_and(n2088, n2126);
    let n2128: ZB = zb_and(n2089, n2126);
    let n2129: ZB = zb_and(n2093, n2128);
    let n2130: ZB = zb_and(n2092, n2128);
    let n2131: ZB = zb_or(n2129, n2130);
    let n2132: ZB = zb_and(n2098, n2131);
    let n2133: ZB = zb_and(n2099, n2131);
    let n2134: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2124);
    let n2135: ZB = zb_or(n2132, n2133);
    let n2136: ZN = zsel_n(n2088, n2124, n2134);
    let n2137: ZB = zb_or(n2127, n2135);
    let n2138: ZN = zn_max(n1264, n2052);
    let n2139: ZN = zn_min(n1264, n2054);
    let n2140: ZN = zsel_n(n1265, n2138, n2139);
    let n2141: ZN = zsel_n(n1049, n2140, n298);
    let n2142: ZN = zsel_n(n1121, n2059, n1256);
    let n2143: ZN = zsel_n(n1121, zn_splat(P8::from_raw(-131072i32)), n2141);
    let n2144: ZN = zsel_n(n1110, n1256, n2142);
    let n2145: ZN = zsel_n(n1110, zn_splat(P8::from_raw(-131072i32)), n2143);
    let n2146: ZB = zsel_b(n158, r_c272, n1260);
    let n2147: ZN = zsel_n(n158, n2045, n1256);
    let n2148: ZN = zsel_n(n158, n2050, n2141);
    let n2149: ZB = zb_and(n1179, n1687);
    let n2150: ZB = zsel_b(n1184, r_c272, n2146);
    let n2151: ZN = zsel_n(n1184, r_c280, n2147);
    let n2152: ZN = zsel_n(n1184, r_c281, n2148);
    let n2153: ZB = zb_or(n1184, n2149);
    let n2154: ZB = zb_and(n2088, n2153);
    let n2155: ZB = zb_and(n2089, n2153);
    let n2156: ZB = zb_and(n2093, n2155);
    let n2157: ZB = zb_and(n2092, n2155);
    let n2158: ZB = zb_or(n2156, n2157);
    let n2159: ZB = zb_and(n2098, n2158);
    let n2160: ZB = zb_and(n2099, n2158);
    let n2161: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2151);
    let n2162: ZB = zb_or(n2159, n2160);
    let n2163: ZN = zsel_n(n2088, n2151, n2161);
    let n2164: ZB = zb_or(n2154, n2162);
    let n2165: ZB = zb_or(r_c247, n68);
    let n2166: ZN = zsel_n(n149, n2062, n1057);
    let n2167: ZN = zsel_n(n149, n2063, n1099);
    let n2168: ZN = zsel_n(n149, n2064, n2057);
    let n2169: ZN = zsel_n(n158, n1057, n2166);
    let n2170: ZN = zsel_n(n158, n2045, n2167);
    let n2171: ZN = zsel_n(n158, n2050, n2168);
    let n2172: ZB = zb_and(n1179, n1730);
    let n2173: ZN = zsel_n(n1184, r_c239, n2169);
    let n2174: ZN = zsel_n(n1184, r_c280, n2170);
    let n2175: ZN = zsel_n(n1184, r_c281, n2171);
    let n2176: ZB = zb_or(n1184, n2172);
    let n2177: ZB = zb_and(n2088, n2176);
    let n2178: ZB = zb_and(n2089, n2176);
    let n2179: ZB = zb_and(n2093, n2178);
    let n2180: ZB = zb_and(n2092, n2178);
    let n2181: ZB = zb_or(n2179, n2180);
    let n2182: ZB = zb_and(n2098, n2181);
    let n2183: ZB = zb_and(n2099, n2181);
    let n2184: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2174);
    let n2185: ZB = zb_or(n2182, n2183);
    let n2186: ZN = zsel_n(n2088, n2174, n2184);
    let n2187: ZB = zb_or(n2177, n2185);
    let n2188: ZN = zsel_n(n149, n2117, n1205);
    let n2189: ZN = zsel_n(n149, n2118, n2114);
    let n2190: ZN = zsel_n(n158, n2045, n2188);
    let n2191: ZN = zsel_n(n158, n2050, n2189);
    let n2192: ZB = zb_and(n1179, n1773);
    let n2193: ZN = zsel_n(n1184, r_c280, n2190);
    let n2194: ZN = zsel_n(n1184, r_c281, n2191);
    let n2195: ZB = zb_or(n1184, n2192);
    let n2196: ZB = zb_and(n2088, n2195);
    let n2197: ZB = zb_and(n2089, n2195);
    let n2198: ZB = zb_and(n2093, n2197);
    let n2199: ZB = zb_and(n2092, n2197);
    let n2200: ZB = zb_or(n2198, n2199);
    let n2201: ZB = zb_and(n2098, n2200);
    let n2202: ZB = zb_and(n2099, n2200);
    let n2203: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2193);
    let n2204: ZB = zb_or(n2201, n2202);
    let n2205: ZN = zsel_n(n2088, n2193, n2203);
    let n2206: ZB = zb_or(n2196, n2204);
    let n2207: ZN = zsel_n(n149, n2144, n1256);
    let n2208: ZN = zsel_n(n149, n2145, n2141);
    let n2209: ZN = zsel_n(n158, n2045, n2207);
    let n2210: ZN = zsel_n(n158, n2050, n2208);
    let n2211: ZB = zb_and(n1179, n1816);
    let n2212: ZN = zsel_n(n1184, r_c280, n2209);
    let n2213: ZN = zsel_n(n1184, r_c281, n2210);
    let n2214: ZB = zb_or(n1184, n2211);
    let n2215: ZB = zb_and(n2088, n2214);
    let n2216: ZB = zb_and(n2089, n2214);
    let n2217: ZB = zb_and(n2093, n2216);
    let n2218: ZB = zb_and(n2092, n2216);
    let n2219: ZB = zb_or(n2217, n2218);
    let n2220: ZB = zb_and(n2098, n2219);
    let n2221: ZB = zb_and(n2099, n2219);
    let n2222: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2212);
    let n2223: ZB = zb_or(n2220, n2221);
    let n2224: ZN = zsel_n(n2088, n2212, n2222);
    let n2225: ZB = zb_or(n2215, n2223);
    let n2226: ZB = zb_or(r_c246, n68);
    let n2227: ZN = zsel_n(n1412, zn_splat(P8::from_raw(655360i32)), n2005);
    let n2228: ZN = zsel_n(n1412, zn_splat(P8::from_raw(262144i32)), r_c236);
    let n2229: ZN = zsel_n(n1412, n2065, n1056);
    let n2230: ZN = zsel_n(n1412, zn_splat(P8::from_raw(98304i32)), r_c268);
    let n2231: ZN = zsel_n(n1412, n2068, r_c269);
    let n2232: ZN = zsel_n(n1412, n2067, r_c270);
    let n2233: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), r_c271);
    let n2234: ZN = zsel_n(n1412, n1125, n1099);
    let n2235: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2057);
    let n2236: ZN = zsel_n(n158, n2005, n2227);
    let n2237: ZN = zsel_n(n158, n2006, n2228);
    let n2238: ZN = zsel_n(n158, n1056, n2229);
    let n2239: ZN = zsel_n(n158, r_c268, n2230);
    let n2240: ZN = zsel_n(n158, r_c269, n2231);
    let n2241: ZN = zsel_n(n158, r_c270, n2232);
    let n2242: ZN = zsel_n(n158, r_c271, n2233);
    let n2243: ZN = zsel_n(n158, n2045, n2234);
    let n2244: ZN = zsel_n(n158, n2050, n2235);
    let n2245: ZB = zb_and(n1179, n1844);
    let n2246: ZN = zsel_n(n1184, n2000, n1416);
    let n2247: ZB = zsel_b(n1184, r_c41, n1417);
    let n2248: ZN = zsel_n(n1184, r_c234, n2236);
    let n2249: ZN = zsel_n(n1184, r_c236, n2237);
    let n2250: ZN = zsel_n(n1184, r_c237, n2238);
    let n2251: ZN = zsel_n(n1184, r_c268, n2239);
    let n2252: ZN = zsel_n(n1184, r_c269, n2240);
    let n2253: ZN = zsel_n(n1184, r_c270, n2241);
    let n2254: ZN = zsel_n(n1184, r_c271, n2242);
    let n2255: ZN = zsel_n(n1184, r_c280, n2243);
    let n2256: ZN = zsel_n(n1184, r_c281, n2244);
    let n2257: ZB = zb_or(n1184, n2245);
    let n2258: ZB = zn_gt(n2246, zn_splat(P8::from_raw(0i32)));
    let n2259: ZB = zn_le(n2246, zn_splat(P8::from_raw(0i32)));
    let n2260: ZB = zb_and(n2257, n2258);
    let n2261: ZB = zb_and(n2257, n2259);
    let n2262: ZB = zb_and(n2093, n2261);
    let n2263: ZB = zb_and(n2092, n2261);
    let n2264: ZB = zb_or(n2262, n2263);
    let n2265: ZB = zb_and(n2098, n2264);
    let n2266: ZB = zb_and(n2099, n2264);
    let n2267: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2255);
    let n2268: ZB = zb_or(n2265, n2266);
    let n2269: ZN = zsel_n(n2258, n2079, n2104);
    let n2270: ZN = zsel_n(n2258, n2255, n2267);
    let n2271: ZB = zb_or(n2260, n2268);
    let n2272: ZN = zsel_n(n1412, zn_splat(P8::from_raw(69510i32)), r_c269);
    let n2273: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-131072i32)), r_c270);
    let n2274: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-327680i32)), n1205);
    let n2275: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2114);
    let n2276: ZN = zsel_n(n158, r_c269, n2272);
    let n2277: ZN = zsel_n(n158, r_c270, n2273);
    let n2278: ZN = zsel_n(n158, n2045, n2274);
    let n2279: ZN = zsel_n(n158, n2050, n2275);
    let n2280: ZB = zb_and(n1179, n1862);
    let n2281: ZN = zsel_n(n1184, r_c269, n2276);
    let n2282: ZN = zsel_n(n1184, r_c270, n2277);
    let n2283: ZN = zsel_n(n1184, r_c280, n2278);
    let n2284: ZN = zsel_n(n1184, r_c281, n2279);
    let n2285: ZB = zb_or(n1184, n2280);
    let n2286: ZB = zb_and(n2258, n2285);
    let n2287: ZB = zb_and(n2259, n2285);
    let n2288: ZB = zb_and(n2093, n2287);
    let n2289: ZB = zb_and(n2092, n2287);
    let n2290: ZB = zb_or(n2288, n2289);
    let n2291: ZB = zb_and(n2098, n2290);
    let n2292: ZB = zb_and(n2099, n2290);
    let n2293: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2283);
    let n2294: ZB = zb_or(n2291, n2292);
    let n2295: ZN = zsel_n(n2258, n2283, n2293);
    let n2296: ZB = zb_or(n2286, n2294);
    let n2297: ZN = zsel_n(n1412, zn_splat(P8::from_raw(131072i32)), r_c270);
    let n2298: ZN = zsel_n(n1412, zn_splat(P8::from_raw(327680i32)), n1256);
    let n2299: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2141);
    let n2300: ZN = zsel_n(n158, r_c270, n2297);
    let n2301: ZN = zsel_n(n158, n2045, n2298);
    let n2302: ZN = zsel_n(n158, n2050, n2299);
    let n2303: ZB = zb_and(n1179, n1880);
    let n2304: ZN = zsel_n(n1184, r_c270, n2300);
    let n2305: ZN = zsel_n(n1184, r_c280, n2301);
    let n2306: ZN = zsel_n(n1184, r_c281, n2302);
    let n2307: ZB = zb_or(n1184, n2303);
    let n2308: ZB = zb_and(n2258, n2307);
    let n2309: ZB = zb_and(n2259, n2307);
    let n2310: ZB = zb_and(n2093, n2309);
    let n2311: ZB = zb_and(n2092, n2309);
    let n2312: ZB = zb_or(n2310, n2311);
    let n2313: ZB = zb_and(n2098, n2312);
    let n2314: ZB = zb_and(n2099, n2312);
    let n2315: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2305);
    let n2316: ZB = zb_or(n2313, n2314);
    let n2317: ZN = zsel_n(n2258, n2305, n2315);
    let n2318: ZB = zb_or(n2308, n2316);
    let n2320: ZN = zsel_n(n1412, zn_splat(P8::from_raw(69510i32)), r_c268);
    let n2321: ZN = zsel_n(n1412, zn_splat(P8::from_raw(98304i32)), r_c269);
    let n2322: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), r_c270);
    let n2323: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-98304i32)), r_c271);
    let n2324: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n1099);
    let n2325: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-327680i32)), n2057);
    let n2326: ZN = zsel_n(n158, r_c268, n2320);
    let n2327: ZN = zsel_n(n158, r_c269, n2321);
    let n2328: ZN = zsel_n(n158, r_c270, n2322);
    let n2329: ZN = zsel_n(n158, r_c271, n2323);
    let n2330: ZN = zsel_n(n158, n2045, n2324);
    let n2331: ZN = zsel_n(n158, n2050, n2325);
    let n2332: ZB = zb_and(n1179, n1896);
    let n2333: ZN = zsel_n(n1184, r_c268, n2326);
    let n2334: ZN = zsel_n(n1184, r_c269, n2327);
    let n2335: ZN = zsel_n(n1184, r_c270, n2328);
    let n2336: ZN = zsel_n(n1184, r_c271, n2329);
    let n2337: ZN = zsel_n(n1184, r_c280, n2330);
    let n2338: ZN = zsel_n(n1184, r_c281, n2331);
    let n2339: ZB = zb_or(n1184, n2332);
    let n2340: ZB = zb_and(n2258, n2339);
    let n2341: ZB = zb_and(n2259, n2339);
    let n2342: ZB = zb_and(n2093, n2341);
    let n2343: ZB = zb_and(n2092, n2341);
    let n2344: ZB = zb_or(n2342, n2343);
    let n2345: ZB = zb_and(n2098, n2344);
    let n2346: ZB = zb_and(n2099, n2344);
    let n2347: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2337);
    let n2348: ZB = zb_or(n2345, n2346);
    let n2349: ZN = zsel_n(n2258, n2337, n2347);
    let n2350: ZB = zb_or(n2340, n2348);
    let n2351: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-231700i32)), n1205);
    let n2352: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-231700i32)), n2114);
    let n2353: ZN = zsel_n(n158, n2045, n2351);
    let n2354: ZN = zsel_n(n158, n2050, n2352);
    let n2355: ZN = zsel_n(n1184, r_c280, n2353);
    let n2356: ZN = zsel_n(n1184, r_c281, n2354);
    let n2357: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2355);
    let n2358: ZN = zsel_n(n2258, n2355, n2357);
    let n2359: ZN = zsel_n(n1412, zn_splat(P8::from_raw(231700i32)), n1256);
    let n2360: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-231700i32)), n2141);
    let n2361: ZN = zsel_n(n158, n2045, n2359);
    let n2362: ZN = zsel_n(n158, n2050, n2360);
    let n2363: ZN = zsel_n(n1184, r_c280, n2361);
    let n2364: ZN = zsel_n(n1184, r_c281, n2362);
    let n2365: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2363);
    let n2366: ZN = zsel_n(n2258, n2363, n2365);
    let n2367: ZN = zsel_n(n1412, zn_splat(P8::from_raw(131072i32)), r_c271);
    let n2368: ZN = zsel_n(n1412, zn_splat(P8::from_raw(327680i32)), n2057);
    let n2369: ZN = zsel_n(n158, r_c271, n2367);
    let n2370: ZN = zsel_n(n158, n2050, n2368);
    let n2371: ZN = zsel_n(n1184, r_c271, n2369);
    let n2372: ZN = zsel_n(n1184, r_c281, n2370);
    let n2373: ZN = zsel_n(n1412, zn_splat(P8::from_raw(231700i32)), n2114);
    let n2374: ZN = zsel_n(n158, n2050, n2373);
    let n2375: ZN = zsel_n(n1184, r_c281, n2374);
    let n2376: ZN = zsel_n(n1412, zn_splat(P8::from_raw(231700i32)), n2141);
    let n2377: ZN = zsel_n(n158, n2050, n2376);
    let n2378: ZN = zsel_n(n1184, r_c281, n2377);
    let n2379: ZN = zsel_n(n1412, n1125, n2167);
    let n2380: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2168);
    let n2381: ZN = zsel_n(n158, n2045, n2379);
    let n2382: ZN = zsel_n(n158, n2050, n2380);
    let n2383: ZB = zb_and(n1179, n1926);
    let n2384: ZN = zsel_n(n1184, r_c280, n2381);
    let n2385: ZN = zsel_n(n1184, r_c281, n2382);
    let n2386: ZB = zb_or(n1184, n2383);
    let n2387: ZB = zb_and(n2258, n2386);
    let n2388: ZB = zb_and(n2259, n2386);
    let n2389: ZB = zb_and(n2093, n2388);
    let n2390: ZB = zb_and(n2092, n2388);
    let n2391: ZB = zb_or(n2389, n2390);
    let n2392: ZB = zb_and(n2098, n2391);
    let n2393: ZB = zb_and(n2099, n2391);
    let n2394: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2384);
    let n2395: ZB = zb_or(n2392, n2393);
    let n2396: ZN = zsel_n(n2258, n2384, n2394);
    let n2397: ZB = zb_or(n2387, n2395);
    let n2398: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-327680i32)), n2188);
    let n2399: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2189);
    let n2400: ZN = zsel_n(n158, n2045, n2398);
    let n2401: ZN = zsel_n(n158, n2050, n2399);
    let n2402: ZB = zb_and(n1179, n1944);
    let n2403: ZN = zsel_n(n1184, r_c280, n2400);
    let n2404: ZN = zsel_n(n1184, r_c281, n2401);
    let n2405: ZB = zb_or(n1184, n2402);
    let n2406: ZB = zb_and(n2258, n2405);
    let n2407: ZB = zb_and(n2259, n2405);
    let n2408: ZB = zb_and(n2093, n2407);
    let n2409: ZB = zb_and(n2092, n2407);
    let n2410: ZB = zb_or(n2408, n2409);
    let n2411: ZB = zb_and(n2098, n2410);
    let n2412: ZB = zb_and(n2099, n2410);
    let n2413: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2403);
    let n2414: ZB = zb_or(n2411, n2412);
    let n2415: ZN = zsel_n(n2258, n2403, n2413);
    let n2416: ZB = zb_or(n2406, n2414);
    let n2417: ZN = zsel_n(n1412, zn_splat(P8::from_raw(327680i32)), n2207);
    let n2418: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2208);
    let n2419: ZN = zsel_n(n158, n2045, n2417);
    let n2420: ZN = zsel_n(n158, n2050, n2418);
    let n2421: ZB = zb_and(n1179, n1962);
    let n2422: ZN = zsel_n(n1184, r_c280, n2419);
    let n2423: ZN = zsel_n(n1184, r_c281, n2420);
    let n2424: ZB = zb_or(n1184, n2421);
    let n2425: ZB = zb_and(n2258, n2424);
    let n2426: ZB = zb_and(n2259, n2424);
    let n2427: ZB = zb_and(n2093, n2426);
    let n2428: ZB = zb_and(n2092, n2426);
    let n2429: ZB = zb_or(n2427, n2428);
    let n2430: ZB = zb_and(n2098, n2429);
    let n2431: ZB = zb_and(n2099, n2429);
    let n2432: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2422);
    let n2433: ZB = zb_or(n2430, n2431);
    let n2434: ZN = zsel_n(n2258, n2422, n2432);
    let n2435: ZB = zb_or(n2425, n2433);
    let n2436: ZN = zsel_n(n1412, zn_splat(P8::from_raw(0i32)), n2167);
    let n2437: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-327680i32)), n2168);
    let n2438: ZN = zsel_n(n158, n2045, n2436);
    let n2439: ZN = zsel_n(n158, n2050, n2437);
    let n2440: ZB = zb_and(n1179, n1978);
    let n2441: ZN = zsel_n(n1184, r_c280, n2438);
    let n2442: ZN = zsel_n(n1184, r_c281, n2439);
    let n2443: ZB = zb_or(n1184, n2440);
    let n2444: ZB = zb_and(n2258, n2443);
    let n2445: ZB = zb_and(n2259, n2443);
    let n2446: ZB = zb_and(n2093, n2445);
    let n2447: ZB = zb_and(n2092, n2445);
    let n2448: ZB = zb_or(n2446, n2447);
    let n2449: ZB = zb_and(n2098, n2448);
    let n2450: ZB = zb_and(n2099, n2448);
    let n2451: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2441);
    let n2452: ZB = zb_or(n2449, n2450);
    let n2453: ZN = zsel_n(n2258, n2441, n2451);
    let n2454: ZB = zb_or(n2444, n2452);
    let n2455: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-231700i32)), n2188);
    let n2456: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-231700i32)), n2189);
    let n2457: ZN = zsel_n(n158, n2045, n2455);
    let n2458: ZN = zsel_n(n158, n2050, n2456);
    let n2459: ZN = zsel_n(n1184, r_c280, n2457);
    let n2460: ZN = zsel_n(n1184, r_c281, n2458);
    let n2461: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2459);
    let n2462: ZN = zsel_n(n2258, n2459, n2461);
    let n2463: ZN = zsel_n(n1412, zn_splat(P8::from_raw(231700i32)), n2207);
    let n2464: ZN = zsel_n(n1412, zn_splat(P8::from_raw(-231700i32)), n2208);
    let n2465: ZN = zsel_n(n158, n2045, n2463);
    let n2466: ZN = zsel_n(n158, n2050, n2464);
    let n2467: ZN = zsel_n(n1184, r_c280, n2465);
    let n2468: ZN = zsel_n(n1184, r_c281, n2466);
    let n2469: ZN = zsel_n(n2098, zn_splat(P8::from_raw(0i32)), n2467);
    let n2470: ZN = zsel_n(n2258, n2467, n2469);
    let n2471: ZN = zsel_n(n1412, zn_splat(P8::from_raw(327680i32)), n2168);
    let n2472: ZN = zsel_n(n158, n2050, n2471);
    let n2473: ZN = zsel_n(n1184, r_c281, n2472);
    let n2474: ZN = zsel_n(n1412, zn_splat(P8::from_raw(231700i32)), n2189);
    let n2475: ZN = zsel_n(n158, n2050, n2474);
    let n2476: ZN = zsel_n(n1184, r_c281, n2475);
    let n2477: ZN = zsel_n(n1412, zn_splat(P8::from_raw(231700i32)), n2208);
    let n2478: ZN = zsel_n(n158, n2050, n2477);
    let n2479: ZN = zsel_n(n1184, r_c281, n2478);
    let n2482: ZW = zw_bits_n(n57);
    let n2483: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2482, 84u64);
    let n2484: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2482, 84u64);
    let n2485: ZW = zw_bits_n(n86);
    let n2486: ZW = zw_mix1(n2483, n2485, 85u64);
    let n2487: ZW = zw_mix2(n2484, n2485, 85u64);
    let n2488: ZW = zw_bits_n(n161);
    let n2489: ZW = zw_mix1(n2486, n2488, 86u64);
    let n2490: ZW = zw_mix2(n2487, n2488, 86u64);
    let n2491: ZW = zw_bits_n(n1198);
    let n2492: ZW = zw_mix1(n2489, n2491, 87u64);
    let n2493: ZW = zw_mix2(n2490, n2491, 87u64);
    let n2494: ZW = zw_bits_n(r_c20);
    let n2495: ZW = zw_mix1(n2492, n2494, 20u64);
    let n2496: ZW = zw_mix2(n2493, n2494, 20u64);
    let n2497: ZW = zw_bits_b(r_c41);
    let n2498: ZW = zw_mix1(n2495, n2497, 41u64);
    let n2499: ZW = zw_mix2(n2496, n2497, 41u64);
    let n2500: ZW = zw_bits_n(n1416);
    let n2501: ZW = zw_mix1(n2492, n2500, 20u64);
    let n2502: ZW = zw_mix2(n2493, n2500, 20u64);
    let n2503: ZW = zw_bits_b(n1417);
    let n2504: ZW = zw_mix1(n2501, n2503, 41u64);
    let n2505: ZW = zw_mix2(n2502, n2503, 41u64);
    let n2506: ZW = zw_mix1(n2489, n2494, 20u64);
    let n2507: ZW = zw_mix2(n2490, n2494, 20u64);
    let n2508: ZW = zw_bits_b(n1602);
    let n2509: ZW = zw_mix1(n2506, n2508, 38u64);
    let n2510: ZW = zw_mix2(n2507, n2508, 38u64);
    let n2511: ZW = zw_bits_n(n1607);
    let n2512: ZW = zw_mix1(n2509, n2511, 39u64);
    let n2513: ZW = zw_mix2(n2510, n2511, 39u64);
    let n2514: ZW = zw_bits_n(n1606);
    let n2515: ZW = zw_mix1(n2512, n2514, 87u64);
    let n2516: ZW = zw_mix2(n2513, n2514, 87u64);
    let n2517: ZW = zw_bits_b(n1646);
    let n2518: ZW = zw_mix1(n2506, n2517, 38u64);
    let n2519: ZW = zw_mix2(n2507, n2517, 38u64);
    let n2520: ZW = zw_bits_n(n1651);
    let n2521: ZW = zw_mix1(n2518, n2520, 39u64);
    let n2522: ZW = zw_mix2(n2519, n2520, 39u64);
    let n2523: ZW = zw_bits_n(n1650);
    let n2524: ZW = zw_mix1(n2521, n2523, 87u64);
    let n2525: ZW = zw_mix2(n2522, n2523, 87u64);
    let n2526: ZW = zw_bits_b(n1690);
    let n2527: ZW = zw_mix1(n2506, n2526, 38u64);
    let n2528: ZW = zw_mix2(n2507, n2526, 38u64);
    let n2529: ZW = zw_bits_n(n1695);
    let n2530: ZW = zw_mix1(n2527, n2529, 39u64);
    let n2531: ZW = zw_mix2(n2528, n2529, 39u64);
    let n2532: ZW = zw_bits_n(n1694);
    let n2533: ZW = zw_mix1(n2530, n2532, 87u64);
    let n2534: ZW = zw_mix2(n2531, n2532, 87u64);
    let n2535: ZW = zw_bits_b(n1733);
    let n2536: ZW = zw_mix1(n2506, n2535, 38u64);
    let n2537: ZW = zw_mix2(n2507, n2535, 38u64);
    let n2538: ZW = zw_bits_n(n1738);
    let n2539: ZW = zw_mix1(n2536, n2538, 39u64);
    let n2540: ZW = zw_mix2(n2537, n2538, 39u64);
    let n2541: ZW = zw_bits_n(n1737);
    let n2542: ZW = zw_mix1(n2539, n2541, 87u64);
    let n2543: ZW = zw_mix2(n2540, n2541, 87u64);
    let n2544: ZW = zw_bits_b(n1776);
    let n2545: ZW = zw_mix1(n2506, n2544, 38u64);
    let n2546: ZW = zw_mix2(n2507, n2544, 38u64);
    let n2547: ZW = zw_bits_n(n1781);
    let n2548: ZW = zw_mix1(n2545, n2547, 39u64);
    let n2549: ZW = zw_mix2(n2546, n2547, 39u64);
    let n2550: ZW = zw_bits_n(n1780);
    let n2551: ZW = zw_mix1(n2548, n2550, 87u64);
    let n2552: ZW = zw_mix2(n2549, n2550, 87u64);
    let n2553: ZW = zw_bits_b(n1819);
    let n2554: ZW = zw_mix1(n2506, n2553, 38u64);
    let n2555: ZW = zw_mix2(n2507, n2553, 38u64);
    let n2556: ZW = zw_bits_n(n1824);
    let n2557: ZW = zw_mix1(n2554, n2556, 39u64);
    let n2558: ZW = zw_mix2(n2555, n2556, 39u64);
    let n2559: ZW = zw_bits_n(n1823);
    let n2560: ZW = zw_mix1(n2557, n2559, 87u64);
    let n2561: ZW = zw_mix2(n2558, n2559, 87u64);
    let n2562: ZW = zw_mix1(n2489, n2500, 20u64);
    let n2563: ZW = zw_mix2(n2490, n2500, 20u64);
    let n2564: ZW = zw_bits_b(n1847);
    let n2565: ZW = zw_mix1(n2562, n2564, 38u64);
    let n2566: ZW = zw_mix2(n2563, n2564, 38u64);
    let n2567: ZW = zw_bits_n(n1854);
    let n2568: ZW = zw_mix1(n2565, n2567, 39u64);
    let n2569: ZW = zw_mix2(n2566, n2567, 39u64);
    let n2570: ZW = zw_bits_n(n1853);
    let n2571: ZW = zw_mix1(n2568, n2570, 87u64);
    let n2572: ZW = zw_mix2(n2569, n2570, 87u64);
    let n2573: ZW = zw_bits_b(n1865);
    let n2574: ZW = zw_mix1(n2562, n2573, 38u64);
    let n2575: ZW = zw_mix2(n2563, n2573, 38u64);
    let n2576: ZW = zw_bits_n(n1872);
    let n2577: ZW = zw_mix1(n2574, n2576, 39u64);
    let n2578: ZW = zw_mix2(n2575, n2576, 39u64);
    let n2579: ZW = zw_bits_n(n1871);
    let n2580: ZW = zw_mix1(n2577, n2579, 87u64);
    let n2581: ZW = zw_mix2(n2578, n2579, 87u64);
    let n2582: ZW = zw_bits_b(n1883);
    let n2583: ZW = zw_mix1(n2562, n2582, 38u64);
    let n2584: ZW = zw_mix2(n2563, n2582, 38u64);
    let n2585: ZW = zw_bits_n(n1890);
    let n2586: ZW = zw_mix1(n2583, n2585, 39u64);
    let n2587: ZW = zw_mix2(n2584, n2585, 39u64);
    let n2588: ZW = zw_bits_n(n1889);
    let n2589: ZW = zw_mix1(n2586, n2588, 87u64);
    let n2590: ZW = zw_mix2(n2587, n2588, 87u64);
    let n2591: ZW = zw_bits_b(n1899);
    let n2592: ZW = zw_mix1(n2562, n2591, 38u64);
    let n2593: ZW = zw_mix2(n2563, n2591, 38u64);
    let n2594: ZW = zw_bits_n(n1906);
    let n2595: ZW = zw_mix1(n2592, n2594, 39u64);
    let n2596: ZW = zw_mix2(n2593, n2594, 39u64);
    let n2597: ZW = zw_bits_n(n1905);
    let n2598: ZW = zw_mix1(n2595, n2597, 87u64);
    let n2599: ZW = zw_mix2(n2596, n2597, 87u64);
    let n2600: ZW = zw_bits_b(n1929);
    let n2601: ZW = zw_mix1(n2562, n2600, 38u64);
    let n2602: ZW = zw_mix2(n2563, n2600, 38u64);
    let n2603: ZW = zw_bits_n(n1936);
    let n2604: ZW = zw_mix1(n2601, n2603, 39u64);
    let n2605: ZW = zw_mix2(n2602, n2603, 39u64);
    let n2606: ZW = zw_bits_n(n1935);
    let n2607: ZW = zw_mix1(n2604, n2606, 87u64);
    let n2608: ZW = zw_mix2(n2605, n2606, 87u64);
    let n2609: ZW = zw_bits_b(n1947);
    let n2610: ZW = zw_mix1(n2562, n2609, 38u64);
    let n2611: ZW = zw_mix2(n2563, n2609, 38u64);
    let n2612: ZW = zw_bits_n(n1954);
    let n2613: ZW = zw_mix1(n2610, n2612, 39u64);
    let n2614: ZW = zw_mix2(n2611, n2612, 39u64);
    let n2615: ZW = zw_bits_n(n1953);
    let n2616: ZW = zw_mix1(n2613, n2615, 87u64);
    let n2617: ZW = zw_mix2(n2614, n2615, 87u64);
    let n2618: ZW = zw_bits_b(n1965);
    let n2619: ZW = zw_mix1(n2562, n2618, 38u64);
    let n2620: ZW = zw_mix2(n2563, n2618, 38u64);
    let n2621: ZW = zw_bits_n(n1972);
    let n2622: ZW = zw_mix1(n2619, n2621, 39u64);
    let n2623: ZW = zw_mix2(n2620, n2621, 39u64);
    let n2624: ZW = zw_bits_n(n1971);
    let n2625: ZW = zw_mix1(n2622, n2624, 87u64);
    let n2626: ZW = zw_mix2(n2623, n2624, 87u64);
    let n2627: ZW = zw_bits_b(n1981);
    let n2628: ZW = zw_mix1(n2562, n2627, 38u64);
    let n2629: ZW = zw_mix2(n2563, n2627, 38u64);
    let n2630: ZW = zw_bits_n(n1988);
    let n2631: ZW = zw_mix1(n2628, n2630, 39u64);
    let n2632: ZW = zw_mix2(n2629, n2630, 39u64);
    let n2633: ZW = zw_bits_n(n1987);
    let n2634: ZW = zw_mix1(n2631, n2633, 87u64);
    let n2635: ZW = zw_mix2(n2632, n2633, 87u64);
    let n2636: ZW = zw_bits_n(r_c39);
    let n2637: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2636, 39u64);
    let n2638: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2636, 39u64);
    let n2639: ZW = zw_mix1(n2637, n2482, 84u64);
    let n2640: ZW = zw_mix2(n2638, n2482, 84u64);
    let n2641: ZW = zw_mix1(n2639, n2485, 85u64);
    let n2642: ZW = zw_mix2(n2640, n2485, 85u64);
    let n2643: ZW = zw_mix1(n2641, n2488, 86u64);
    let n2644: ZW = zw_mix2(n2642, n2488, 86u64);
    let n2645: ZW = zw_bits_n(r_c87);
    let n2646: ZW = zw_mix1(n2643, n2645, 87u64);
    let n2647: ZW = zw_mix2(n2644, n2645, 87u64);
    let n2648: ZW = zw_bits_n(n2080);
    let n2649: ZW = zw_mix1(n2646, n2648, 254u64);
    let n2650: ZW = zw_mix2(n2647, n2648, 254u64);
    let n2651: ZW = zw_bits_n(n2082);
    let n2652: ZW = zw_mix1(n2649, n2651, 278u64);
    let n2653: ZW = zw_mix2(n2650, n2651, 278u64);
    let n2654: ZW = zw_bits_n(n2083);
    let n2655: ZW = zw_mix1(n2652, n2654, 279u64);
    let n2656: ZW = zw_mix2(n2653, n2654, 279u64);
    let n2657: ZW = zw_bits_n(n2074);
    let n2658: ZW = zw_mix1(n2655, n2657, 20u64);
    let n2659: ZW = zw_mix2(n2656, n2657, 20u64);
    let n2660: ZW = zw_mix1(n2658, n2497, 41u64);
    let n2661: ZW = zw_mix2(n2659, n2497, 41u64);
    let n2662: ZW = zw_bits_n(n2075);
    let n2663: ZW = zw_mix1(n2660, n2662, 234u64);
    let n2664: ZW = zw_mix2(n2661, n2662, 234u64);
    let n2665: ZW = zw_bits_n(n2076);
    let n2666: ZW = zw_mix1(n2663, n2665, 236u64);
    let n2667: ZW = zw_mix2(n2664, n2665, 236u64);
    let n2668: ZW = zw_bits_n(n2077);
    let n2669: ZW = zw_mix1(n2666, n2668, 237u64);
    let n2670: ZW = zw_mix2(n2667, n2668, 237u64);
    let n2671: ZW = zw_bits_n(n2078);
    let n2672: ZW = zw_mix1(n2669, n2671, 239u64);
    let n2673: ZW = zw_mix2(n2670, n2671, 239u64);
    let n2674: ZW = zw_bits_b(n2007);
    let n2675: ZW = zw_mix1(n2672, n2674, 246u64);
    let n2676: ZW = zw_mix2(n2673, n2674, 246u64);
    let n2677: ZW = zw_bits_b(n2008);
    let n2678: ZW = zw_mix1(n2675, n2677, 247u64);
    let n2679: ZW = zw_mix2(n2676, n2677, 247u64);
    let n2680: ZW = zw_bits_n(n2107);
    let n2681: ZW = zw_mix1(n2678, n2680, 253u64);
    let n2682: ZW = zw_mix2(n2679, n2680, 253u64);
    let n2683: ZW = zw_bits_n(r_c268);
    let n2684: ZW = zw_mix1(n2681, n2683, 268u64);
    let n2685: ZW = zw_mix2(n2682, n2683, 268u64);
    let n2686: ZW = zw_bits_n(r_c269);
    let n2687: ZW = zw_mix1(n2684, n2686, 269u64);
    let n2688: ZW = zw_mix2(n2685, n2686, 269u64);
    let n2689: ZW = zw_bits_n(r_c270);
    let n2690: ZW = zw_mix1(n2687, n2689, 270u64);
    let n2691: ZW = zw_mix2(n2688, n2689, 270u64);
    let n2692: ZW = zw_bits_n(r_c271);
    let n2693: ZW = zw_mix1(n2690, n2692, 271u64);
    let n2694: ZW = zw_mix2(n2691, n2692, 271u64);
    let n2695: ZW = zw_bits_b(n2081);
    let n2696: ZW = zw_mix1(n2693, n2695, 272u64);
    let n2697: ZW = zw_mix2(n2694, n2695, 272u64);
    let n2698: ZW = zw_bits_n(n2108);
    let n2699: ZW = zw_mix1(n2696, n2698, 280u64);
    let n2700: ZW = zw_mix2(n2697, n2698, 280u64);
    let n2701: ZW = zw_bits_n(n2085);
    let n2702: ZW = zw_mix1(n2699, n2701, 281u64);
    let n2703: ZW = zw_mix2(n2700, n2701, 281u64);
    let n2704: ZW = zw_bits_b(n2123);
    let n2705: ZW = zw_mix1(n2693, n2704, 272u64);
    let n2706: ZW = zw_mix2(n2694, n2704, 272u64);
    let n2707: ZW = zw_bits_n(n2136);
    let n2708: ZW = zw_mix1(n2705, n2707, 280u64);
    let n2709: ZW = zw_mix2(n2706, n2707, 280u64);
    let n2710: ZW = zw_bits_n(n2125);
    let n2711: ZW = zw_mix1(n2708, n2710, 281u64);
    let n2712: ZW = zw_mix2(n2709, n2710, 281u64);
    let n2713: ZW = zw_bits_b(n2150);
    let n2714: ZW = zw_mix1(n2693, n2713, 272u64);
    let n2715: ZW = zw_mix2(n2694, n2713, 272u64);
    let n2716: ZW = zw_bits_n(n2163);
    let n2717: ZW = zw_mix1(n2714, n2716, 280u64);
    let n2718: ZW = zw_mix2(n2715, n2716, 280u64);
    let n2719: ZW = zw_bits_n(n2152);
    let n2720: ZW = zw_mix1(n2717, n2719, 281u64);
    let n2721: ZW = zw_mix2(n2718, n2719, 281u64);
    let n2722: ZW = zw_bits_n(n2173);
    let n2723: ZW = zw_mix1(n2669, n2722, 239u64);
    let n2724: ZW = zw_mix2(n2670, n2722, 239u64);
    let n2725: ZW = zw_mix1(n2723, n2674, 246u64);
    let n2726: ZW = zw_mix2(n2724, n2674, 246u64);
    let n2727: ZW = zw_bits_b(n2165);
    let n2728: ZW = zw_mix1(n2725, n2727, 247u64);
    let n2729: ZW = zw_mix2(n2726, n2727, 247u64);
    let n2730: ZW = zw_mix1(n2728, n2680, 253u64);
    let n2731: ZW = zw_mix2(n2729, n2680, 253u64);
    let n2732: ZW = zw_mix1(n2730, n2683, 268u64);
    let n2733: ZW = zw_mix2(n2731, n2683, 268u64);
    let n2734: ZW = zw_mix1(n2732, n2686, 269u64);
    let n2735: ZW = zw_mix2(n2733, n2686, 269u64);
    let n2736: ZW = zw_mix1(n2734, n2689, 270u64);
    let n2737: ZW = zw_mix2(n2735, n2689, 270u64);
    let n2738: ZW = zw_mix1(n2736, n2692, 271u64);
    let n2739: ZW = zw_mix2(n2737, n2692, 271u64);
    let n2740: ZW = zw_mix1(n2738, n2695, 272u64);
    let n2741: ZW = zw_mix2(n2739, n2695, 272u64);
    let n2742: ZW = zw_bits_n(n2186);
    let n2743: ZW = zw_mix1(n2740, n2742, 280u64);
    let n2744: ZW = zw_mix2(n2741, n2742, 280u64);
    let n2745: ZW = zw_bits_n(n2175);
    let n2746: ZW = zw_mix1(n2743, n2745, 281u64);
    let n2747: ZW = zw_mix2(n2744, n2745, 281u64);
    let n2748: ZW = zw_mix1(n2738, n2704, 272u64);
    let n2749: ZW = zw_mix2(n2739, n2704, 272u64);
    let n2750: ZW = zw_bits_n(n2205);
    let n2751: ZW = zw_mix1(n2748, n2750, 280u64);
    let n2752: ZW = zw_mix2(n2749, n2750, 280u64);
    let n2753: ZW = zw_bits_n(n2194);
    let n2754: ZW = zw_mix1(n2751, n2753, 281u64);
    let n2755: ZW = zw_mix2(n2752, n2753, 281u64);
    let n2756: ZW = zw_mix1(n2738, n2713, 272u64);
    let n2757: ZW = zw_mix2(n2739, n2713, 272u64);
    let n2758: ZW = zw_bits_n(n2224);
    let n2759: ZW = zw_mix1(n2756, n2758, 280u64);
    let n2760: ZW = zw_mix2(n2757, n2758, 280u64);
    let n2761: ZW = zw_bits_n(n2213);
    let n2762: ZW = zw_mix1(n2759, n2761, 281u64);
    let n2763: ZW = zw_mix2(n2760, n2761, 281u64);
    let n2764: ZW = zw_bits_n(n2246);
    let n2765: ZW = zw_mix1(n2655, n2764, 20u64);
    let n2766: ZW = zw_mix2(n2656, n2764, 20u64);
    let n2767: ZW = zw_bits_b(n2247);
    let n2768: ZW = zw_mix1(n2765, n2767, 41u64);
    let n2769: ZW = zw_mix2(n2766, n2767, 41u64);
    let n2770: ZW = zw_bits_n(n2248);
    let n2771: ZW = zw_mix1(n2768, n2770, 234u64);
    let n2772: ZW = zw_mix2(n2769, n2770, 234u64);
    let n2773: ZW = zw_bits_n(n2249);
    let n2774: ZW = zw_mix1(n2771, n2773, 236u64);
    let n2775: ZW = zw_mix2(n2772, n2773, 236u64);
    let n2776: ZW = zw_bits_n(n2250);
    let n2777: ZW = zw_mix1(n2774, n2776, 237u64);
    let n2778: ZW = zw_mix2(n2775, n2776, 237u64);
    let n2779: ZW = zw_mix1(n2777, n2671, 239u64);
    let n2780: ZW = zw_mix2(n2778, n2671, 239u64);
    let n2781: ZW = zw_bits_b(n2226);
    let n2782: ZW = zw_mix1(n2779, n2781, 246u64);
    let n2783: ZW = zw_mix2(n2780, n2781, 246u64);
    let n2784: ZW = zw_mix1(n2782, n2677, 247u64);
    let n2785: ZW = zw_mix2(n2783, n2677, 247u64);
    let n2786: ZW = zw_bits_n(n2269);
    let n2787: ZW = zw_mix1(n2784, n2786, 253u64);
    let n2788: ZW = zw_mix2(n2785, n2786, 253u64);
    let n2789: ZW = zw_bits_n(n2251);
    let n2790: ZW = zw_mix1(n2787, n2789, 268u64);
    let n2791: ZW = zw_mix2(n2788, n2789, 268u64);
    let n2792: ZW = zw_bits_n(n2252);
    let n2793: ZW = zw_mix1(n2790, n2792, 269u64);
    let n2794: ZW = zw_mix2(n2791, n2792, 269u64);
    let n2795: ZW = zw_bits_n(n2253);
    let n2796: ZW = zw_mix1(n2793, n2795, 270u64);
    let n2797: ZW = zw_mix2(n2794, n2795, 270u64);
    let n2798: ZW = zw_bits_n(n2254);
    let n2799: ZW = zw_mix1(n2796, n2798, 271u64);
    let n2800: ZW = zw_mix2(n2797, n2798, 271u64);
    let n2801: ZW = zw_mix1(n2799, n2695, 272u64);
    let n2802: ZW = zw_mix2(n2800, n2695, 272u64);
    let n2803: ZW = zw_bits_n(n2270);
    let n2804: ZW = zw_mix1(n2801, n2803, 280u64);
    let n2805: ZW = zw_mix2(n2802, n2803, 280u64);
    let n2806: ZW = zw_bits_n(n2256);
    let n2807: ZW = zw_mix1(n2804, n2806, 281u64);
    let n2808: ZW = zw_mix2(n2805, n2806, 281u64);
    let n2809: ZW = zw_bits_n(n2281);
    let n2810: ZW = zw_mix1(n2790, n2809, 269u64);
    let n2811: ZW = zw_mix2(n2791, n2809, 269u64);
    let n2812: ZW = zw_bits_n(n2282);
    let n2813: ZW = zw_mix1(n2810, n2812, 270u64);
    let n2814: ZW = zw_mix2(n2811, n2812, 270u64);
    let n2815: ZW = zw_mix1(n2813, n2798, 271u64);
    let n2816: ZW = zw_mix2(n2814, n2798, 271u64);
    let n2817: ZW = zw_mix1(n2815, n2704, 272u64);
    let n2818: ZW = zw_mix2(n2816, n2704, 272u64);
    let n2819: ZW = zw_bits_n(n2295);
    let n2820: ZW = zw_mix1(n2817, n2819, 280u64);
    let n2821: ZW = zw_mix2(n2818, n2819, 280u64);
    let n2822: ZW = zw_bits_n(n2284);
    let n2823: ZW = zw_mix1(n2820, n2822, 281u64);
    let n2824: ZW = zw_mix2(n2821, n2822, 281u64);
    let n2825: ZW = zw_bits_n(n2304);
    let n2826: ZW = zw_mix1(n2810, n2825, 270u64);
    let n2827: ZW = zw_mix2(n2811, n2825, 270u64);
    let n2828: ZW = zw_mix1(n2826, n2798, 271u64);
    let n2829: ZW = zw_mix2(n2827, n2798, 271u64);
    let n2830: ZW = zw_mix1(n2828, n2713, 272u64);
    let n2831: ZW = zw_mix2(n2829, n2713, 272u64);
    let n2832: ZW = zw_bits_n(n2317);
    let n2833: ZW = zw_mix1(n2830, n2832, 280u64);
    let n2834: ZW = zw_mix2(n2831, n2832, 280u64);
    let n2835: ZW = zw_bits_n(n2306);
    let n2836: ZW = zw_mix1(n2833, n2835, 281u64);
    let n2837: ZW = zw_mix2(n2834, n2835, 281u64);
    let n2838: ZW = zw_bits_n(n2333);
    let n2839: ZW = zw_mix1(n2787, n2838, 268u64);
    let n2840: ZW = zw_mix2(n2788, n2838, 268u64);
    let n2841: ZW = zw_bits_n(n2334);
    let n2842: ZW = zw_mix1(n2839, n2841, 269u64);
    let n2843: ZW = zw_mix2(n2840, n2841, 269u64);
    let n2844: ZW = zw_bits_n(n2335);
    let n2845: ZW = zw_mix1(n2842, n2844, 270u64);
    let n2846: ZW = zw_mix2(n2843, n2844, 270u64);
    let n2847: ZW = zw_bits_n(n2336);
    let n2848: ZW = zw_mix1(n2845, n2847, 271u64);
    let n2849: ZW = zw_mix2(n2846, n2847, 271u64);
    let n2850: ZW = zw_mix1(n2848, n2695, 272u64);
    let n2851: ZW = zw_mix2(n2849, n2695, 272u64);
    let n2852: ZW = zw_bits_n(n2349);
    let n2853: ZW = zw_mix1(n2850, n2852, 280u64);
    let n2854: ZW = zw_mix2(n2851, n2852, 280u64);
    let n2855: ZW = zw_bits_n(n2338);
    let n2856: ZW = zw_mix1(n2853, n2855, 281u64);
    let n2857: ZW = zw_mix2(n2854, n2855, 281u64);
    let n2858: ZW = zw_mix1(n2839, n2809, 269u64);
    let n2859: ZW = zw_mix2(n2840, n2809, 269u64);
    let n2860: ZW = zw_mix1(n2858, n2812, 270u64);
    let n2861: ZW = zw_mix2(n2859, n2812, 270u64);
    let n2862: ZW = zw_mix1(n2860, n2847, 271u64);
    let n2863: ZW = zw_mix2(n2861, n2847, 271u64);
    let n2864: ZW = zw_mix1(n2862, n2704, 272u64);
    let n2865: ZW = zw_mix2(n2863, n2704, 272u64);
    let n2866: ZW = zw_bits_n(n2358);
    let n2867: ZW = zw_mix1(n2864, n2866, 280u64);
    let n2868: ZW = zw_mix2(n2865, n2866, 280u64);
    let n2869: ZW = zw_bits_n(n2356);
    let n2870: ZW = zw_mix1(n2867, n2869, 281u64);
    let n2871: ZW = zw_mix2(n2868, n2869, 281u64);
    let n2872: ZW = zw_mix1(n2858, n2825, 270u64);
    let n2873: ZW = zw_mix2(n2859, n2825, 270u64);
    let n2874: ZW = zw_mix1(n2872, n2847, 271u64);
    let n2875: ZW = zw_mix2(n2873, n2847, 271u64);
    let n2876: ZW = zw_mix1(n2874, n2713, 272u64);
    let n2877: ZW = zw_mix2(n2875, n2713, 272u64);
    let n2878: ZW = zw_bits_n(n2366);
    let n2879: ZW = zw_mix1(n2876, n2878, 280u64);
    let n2880: ZW = zw_mix2(n2877, n2878, 280u64);
    let n2881: ZW = zw_bits_n(n2364);
    let n2882: ZW = zw_mix1(n2879, n2881, 281u64);
    let n2883: ZW = zw_mix2(n2880, n2881, 281u64);
    let n2884: ZW = zw_bits_n(n2371);
    let n2885: ZW = zw_mix1(n2845, n2884, 271u64);
    let n2886: ZW = zw_mix2(n2846, n2884, 271u64);
    let n2887: ZW = zw_mix1(n2885, n2695, 272u64);
    let n2888: ZW = zw_mix2(n2886, n2695, 272u64);
    let n2889: ZW = zw_mix1(n2887, n2852, 280u64);
    let n2890: ZW = zw_mix2(n2888, n2852, 280u64);
    let n2891: ZW = zw_bits_n(n2372);
    let n2892: ZW = zw_mix1(n2889, n2891, 281u64);
    let n2893: ZW = zw_mix2(n2890, n2891, 281u64);
    let n2894: ZW = zw_mix1(n2860, n2884, 271u64);
    let n2895: ZW = zw_mix2(n2861, n2884, 271u64);
    let n2896: ZW = zw_mix1(n2894, n2704, 272u64);
    let n2897: ZW = zw_mix2(n2895, n2704, 272u64);
    let n2898: ZW = zw_mix1(n2896, n2866, 280u64);
    let n2899: ZW = zw_mix2(n2897, n2866, 280u64);
    let n2900: ZW = zw_bits_n(n2375);
    let n2901: ZW = zw_mix1(n2898, n2900, 281u64);
    let n2902: ZW = zw_mix2(n2899, n2900, 281u64);
    let n2903: ZW = zw_mix1(n2872, n2884, 271u64);
    let n2904: ZW = zw_mix2(n2873, n2884, 271u64);
    let n2905: ZW = zw_mix1(n2903, n2713, 272u64);
    let n2906: ZW = zw_mix2(n2904, n2713, 272u64);
    let n2907: ZW = zw_mix1(n2905, n2878, 280u64);
    let n2908: ZW = zw_mix2(n2906, n2878, 280u64);
    let n2909: ZW = zw_bits_n(n2378);
    let n2910: ZW = zw_mix1(n2907, n2909, 281u64);
    let n2911: ZW = zw_mix2(n2908, n2909, 281u64);
    let n2912: ZW = zw_mix1(n2777, n2722, 239u64);
    let n2913: ZW = zw_mix2(n2778, n2722, 239u64);
    let n2914: ZW = zw_mix1(n2912, n2781, 246u64);
    let n2915: ZW = zw_mix2(n2913, n2781, 246u64);
    let n2916: ZW = zw_mix1(n2914, n2727, 247u64);
    let n2917: ZW = zw_mix2(n2915, n2727, 247u64);
    let n2918: ZW = zw_mix1(n2916, n2786, 253u64);
    let n2919: ZW = zw_mix2(n2917, n2786, 253u64);
    let n2920: ZW = zw_mix1(n2918, n2789, 268u64);
    let n2921: ZW = zw_mix2(n2919, n2789, 268u64);
    let n2922: ZW = zw_mix1(n2920, n2792, 269u64);
    let n2923: ZW = zw_mix2(n2921, n2792, 269u64);
    let n2924: ZW = zw_mix1(n2922, n2795, 270u64);
    let n2925: ZW = zw_mix2(n2923, n2795, 270u64);
    let n2926: ZW = zw_mix1(n2924, n2798, 271u64);
    let n2927: ZW = zw_mix2(n2925, n2798, 271u64);
    let n2928: ZW = zw_mix1(n2926, n2695, 272u64);
    let n2929: ZW = zw_mix2(n2927, n2695, 272u64);
    let n2930: ZW = zw_bits_n(n2396);
    let n2931: ZW = zw_mix1(n2928, n2930, 280u64);
    let n2932: ZW = zw_mix2(n2929, n2930, 280u64);
    let n2933: ZW = zw_bits_n(n2385);
    let n2934: ZW = zw_mix1(n2931, n2933, 281u64);
    let n2935: ZW = zw_mix2(n2932, n2933, 281u64);
    let n2936: ZW = zw_mix1(n2920, n2809, 269u64);
    let n2937: ZW = zw_mix2(n2921, n2809, 269u64);
    let n2938: ZW = zw_mix1(n2936, n2812, 270u64);
    let n2939: ZW = zw_mix2(n2937, n2812, 270u64);
    let n2940: ZW = zw_mix1(n2938, n2798, 271u64);
    let n2941: ZW = zw_mix2(n2939, n2798, 271u64);
    let n2942: ZW = zw_mix1(n2940, n2704, 272u64);
    let n2943: ZW = zw_mix2(n2941, n2704, 272u64);
    let n2944: ZW = zw_bits_n(n2415);
    let n2945: ZW = zw_mix1(n2942, n2944, 280u64);
    let n2946: ZW = zw_mix2(n2943, n2944, 280u64);
    let n2947: ZW = zw_bits_n(n2404);
    let n2948: ZW = zw_mix1(n2945, n2947, 281u64);
    let n2949: ZW = zw_mix2(n2946, n2947, 281u64);
    let n2950: ZW = zw_mix1(n2936, n2825, 270u64);
    let n2951: ZW = zw_mix2(n2937, n2825, 270u64);
    let n2952: ZW = zw_mix1(n2950, n2798, 271u64);
    let n2953: ZW = zw_mix2(n2951, n2798, 271u64);
    let n2954: ZW = zw_mix1(n2952, n2713, 272u64);
    let n2955: ZW = zw_mix2(n2953, n2713, 272u64);
    let n2956: ZW = zw_bits_n(n2434);
    let n2957: ZW = zw_mix1(n2954, n2956, 280u64);
    let n2958: ZW = zw_mix2(n2955, n2956, 280u64);
    let n2959: ZW = zw_bits_n(n2423);
    let n2960: ZW = zw_mix1(n2957, n2959, 281u64);
    let n2961: ZW = zw_mix2(n2958, n2959, 281u64);
    let n2962: ZW = zw_mix1(n2918, n2838, 268u64);
    let n2963: ZW = zw_mix2(n2919, n2838, 268u64);
    let n2964: ZW = zw_mix1(n2962, n2841, 269u64);
    let n2965: ZW = zw_mix2(n2963, n2841, 269u64);
    let n2966: ZW = zw_mix1(n2964, n2844, 270u64);
    let n2967: ZW = zw_mix2(n2965, n2844, 270u64);
    let n2968: ZW = zw_mix1(n2966, n2847, 271u64);
    let n2969: ZW = zw_mix2(n2967, n2847, 271u64);
    let n2970: ZW = zw_mix1(n2968, n2695, 272u64);
    let n2971: ZW = zw_mix2(n2969, n2695, 272u64);
    let n2972: ZW = zw_bits_n(n2453);
    let n2973: ZW = zw_mix1(n2970, n2972, 280u64);
    let n2974: ZW = zw_mix2(n2971, n2972, 280u64);
    let n2975: ZW = zw_bits_n(n2442);
    let n2976: ZW = zw_mix1(n2973, n2975, 281u64);
    let n2977: ZW = zw_mix2(n2974, n2975, 281u64);
    let n2978: ZW = zw_mix1(n2962, n2809, 269u64);
    let n2979: ZW = zw_mix2(n2963, n2809, 269u64);
    let n2980: ZW = zw_mix1(n2978, n2812, 270u64);
    let n2981: ZW = zw_mix2(n2979, n2812, 270u64);
    let n2982: ZW = zw_mix1(n2980, n2847, 271u64);
    let n2983: ZW = zw_mix2(n2981, n2847, 271u64);
    let n2984: ZW = zw_mix1(n2982, n2704, 272u64);
    let n2985: ZW = zw_mix2(n2983, n2704, 272u64);
    let n2986: ZW = zw_bits_n(n2462);
    let n2987: ZW = zw_mix1(n2984, n2986, 280u64);
    let n2988: ZW = zw_mix2(n2985, n2986, 280u64);
    let n2989: ZW = zw_bits_n(n2460);
    let n2990: ZW = zw_mix1(n2987, n2989, 281u64);
    let n2991: ZW = zw_mix2(n2988, n2989, 281u64);
    let n2992: ZW = zw_mix1(n2978, n2825, 270u64);
    let n2993: ZW = zw_mix2(n2979, n2825, 270u64);
    let n2994: ZW = zw_mix1(n2992, n2847, 271u64);
    let n2995: ZW = zw_mix2(n2993, n2847, 271u64);
    let n2996: ZW = zw_mix1(n2994, n2713, 272u64);
    let n2997: ZW = zw_mix2(n2995, n2713, 272u64);
    let n2998: ZW = zw_bits_n(n2470);
    let n2999: ZW = zw_mix1(n2996, n2998, 280u64);
    let n3000: ZW = zw_mix2(n2997, n2998, 280u64);
    let n3001: ZW = zw_bits_n(n2468);
    let n3002: ZW = zw_mix1(n2999, n3001, 281u64);
    let n3003: ZW = zw_mix2(n3000, n3001, 281u64);
    let n3004: ZW = zw_mix1(n2966, n2884, 271u64);
    let n3005: ZW = zw_mix2(n2967, n2884, 271u64);
    let n3006: ZW = zw_mix1(n3004, n2695, 272u64);
    let n3007: ZW = zw_mix2(n3005, n2695, 272u64);
    let n3008: ZW = zw_mix1(n3006, n2972, 280u64);
    let n3009: ZW = zw_mix2(n3007, n2972, 280u64);
    let n3010: ZW = zw_bits_n(n2473);
    let n3011: ZW = zw_mix1(n3008, n3010, 281u64);
    let n3012: ZW = zw_mix2(n3009, n3010, 281u64);
    let n3013: ZW = zw_mix1(n2980, n2884, 271u64);
    let n3014: ZW = zw_mix2(n2981, n2884, 271u64);
    let n3015: ZW = zw_mix1(n3013, n2704, 272u64);
    let n3016: ZW = zw_mix2(n3014, n2704, 272u64);
    let n3017: ZW = zw_mix1(n3015, n2986, 280u64);
    let n3018: ZW = zw_mix2(n3016, n2986, 280u64);
    let n3019: ZW = zw_bits_n(n2476);
    let n3020: ZW = zw_mix1(n3017, n3019, 281u64);
    let n3021: ZW = zw_mix2(n3018, n3019, 281u64);
    let n3022: ZW = zw_mix1(n2992, n2884, 271u64);
    let n3023: ZW = zw_mix2(n2993, n2884, 271u64);
    let n3024: ZW = zw_mix1(n3022, n2713, 272u64);
    let n3025: ZW = zw_mix2(n3023, n2713, 272u64);
    let n3026: ZW = zw_mix1(n3024, n2998, 280u64);
    let n3027: ZW = zw_mix2(n3025, n2998, 280u64);
    let n3028: ZW = zw_bits_n(n2479);
    let n3029: ZW = zw_mix1(n3026, n3028, 281u64);
    let n3030: ZW = zw_mix2(n3027, n3028, 281u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v0_b0: bool = !n80 || !n79 || !n78 || !n77;
    let live_v0_b0: u16 = ALL & zb_holds(n68) & zb_holds(n1179) & zb_holds(n1182);
    let ok_v1_b1: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v1_b1: bool = !n80 || !n79 || !n78 || !n77;
    let live_v1_b1: u16 = ALL & zb_holds(n68) & zb_holds(n1179) & zb_holds(n1250);
    let ok_v2_b2: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v2_b2: bool = !n80 || !n79 || !n78 || !n77;
    let live_v2_b2: u16 = ALL & zb_holds(n68) & zb_holds(n1179) & zb_holds(n1301);
    let ok_v16_b3: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v16_b3: bool = !n80 || !n79 || !n78 || !n77;
    let live_v16_b3: u16 = ALL & zb_holds(n68) & zb_holds(n1179) & zb_holds(n1337);
    let ok_v17_b4: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v17_b4: bool = !n80 || !n79 || !n78 || !n77;
    let live_v17_b4: u16 = ALL & zb_holds(n68) & zb_holds(n1179) & zb_holds(n1373);
    let ok_v18_b5: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v18_b5: bool = !n80 || !n79 || !n78 || !n77;
    let live_v18_b5: u16 = ALL & zb_holds(n68) & zb_holds(n1179) & zb_holds(n1409);
    let ok_v32_b6: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v32_b6: bool = !n80 || !n79 || !n78 || !n77;
    let live_v32_b6: u16 = ALL & zb_holds(n1442);
    let ok_v33_b7: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v33_b7: bool = !n80 || !n79 || !n78 || !n77;
    let live_v33_b7: u16 = ALL & zb_holds(n1453);
    let ok_v34_b8: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v34_b8: bool = !n80 || !n79 || !n78 || !n77;
    let live_v34_b8: u16 = ALL & zb_holds(n1464);
    let ok_v36_b9: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v36_b9: bool = !n80 || !n79 || !n78 || !n77;
    let live_v36_b9: u16 = ALL & zb_holds(n1473);
    let ok_v48_b10: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v48_b10: bool = !n80 || !n79 || !n78 || !n77;
    let live_v48_b10: u16 = ALL & zb_holds(n1496);
    let ok_v49_b11: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v49_b11: bool = !n80 || !n79 || !n78 || !n77;
    let live_v49_b11: u16 = ALL & zb_holds(n1507);
    let ok_v50_b12: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v50_b12: bool = !n80 || !n79 || !n78 || !n77;
    let live_v50_b12: u16 = ALL & zb_holds(n1518);
    let ok_v52_b13: u16 = ALL & zb_holds(n1045) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53);
    let bd_v52_b13: bool = !n80 || !n79 || !n78 || !n77;
    let live_v52_b13: u16 = ALL & zb_holds(n1527);
    let ok_v0_b14: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1604);
    let bd_v0_b14: bool = !n80 || !n79 || !n78 || !n77;
    let live_v0_b14: u16 = ALL & zb_holds(n68) & zb_holds(n1603);
    let ok_v1_b15: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1648);
    let bd_v1_b15: bool = !n80 || !n79 || !n78 || !n77;
    let live_v1_b15: u16 = ALL & zb_holds(n68) & zb_holds(n1647);
    let ok_v2_b16: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1692);
    let bd_v2_b16: bool = !n80 || !n79 || !n78 || !n77;
    let live_v2_b16: u16 = ALL & zb_holds(n68) & zb_holds(n1691);
    let ok_v16_b17: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1735);
    let bd_v16_b17: bool = !n80 || !n79 || !n78 || !n77;
    let live_v16_b17: u16 = ALL & zb_holds(n68) & zb_holds(n1734);
    let ok_v17_b18: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1778);
    let bd_v17_b18: bool = !n80 || !n79 || !n78 || !n77;
    let live_v17_b18: u16 = ALL & zb_holds(n68) & zb_holds(n1777);
    let ok_v18_b19: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1821);
    let bd_v18_b19: bool = !n80 || !n79 || !n78 || !n77;
    let live_v18_b19: u16 = ALL & zb_holds(n68) & zb_holds(n1820);
    let ok_v32_b20: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1849);
    let bd_v32_b20: bool = !n80 || !n79 || !n78 || !n77;
    let live_v32_b20: u16 = ALL & zb_holds(n1852);
    let ok_v33_b21: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1867);
    let bd_v33_b21: bool = !n80 || !n79 || !n78 || !n77;
    let live_v33_b21: u16 = ALL & zb_holds(n1870);
    let ok_v34_b22: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1885);
    let bd_v34_b22: bool = !n80 || !n79 || !n78 || !n77;
    let live_v34_b22: u16 = ALL & zb_holds(n1888);
    let ok_v36_b23: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1901);
    let bd_v36_b23: bool = !n80 || !n79 || !n78 || !n77;
    let live_v36_b23: u16 = ALL & zb_holds(n1904);
    let ok_v48_b24: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1931);
    let bd_v48_b24: bool = !n80 || !n79 || !n78 || !n77;
    let live_v48_b24: u16 = ALL & zb_holds(n1934);
    let ok_v49_b25: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1949);
    let bd_v49_b25: bool = !n80 || !n79 || !n78 || !n77;
    let live_v49_b25: u16 = ALL & zb_holds(n1952);
    let ok_v50_b26: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1967);
    let bd_v50_b26: bool = !n80 || !n79 || !n78 || !n77;
    let live_v50_b26: u16 = ALL & zb_holds(n1970);
    let ok_v52_b27: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n1983);
    let bd_v52_b27: bool = !n80 || !n79 || !n78 || !n77;
    let live_v52_b27: u16 = ALL & zb_holds(n1986);
    let ok_v0_b28: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v0_b28: bool = !n80 || !n79 || !n78 || !n77;
    let live_v0_b28: u16 = ALL & zb_holds(n2109);
    let ok_v1_b29: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v1_b29: bool = !n80 || !n79 || !n78 || !n77;
    let live_v1_b29: u16 = ALL & zb_holds(n2137);
    let ok_v2_b30: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v2_b30: bool = !n80 || !n79 || !n78 || !n77;
    let live_v2_b30: u16 = ALL & zb_holds(n2164);
    let ok_v16_b31: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v16_b31: bool = !n80 || !n79 || !n78 || !n77;
    let live_v16_b31: u16 = ALL & zb_holds(n2187);
    let ok_v17_b32: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v17_b32: bool = !n80 || !n79 || !n78 || !n77;
    let live_v17_b32: u16 = ALL & zb_holds(n2206);
    let ok_v18_b33: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v18_b33: bool = !n80 || !n79 || !n78 || !n77;
    let live_v18_b33: u16 = ALL & zb_holds(n2225);
    let ok_v32_b34: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v32_b34: bool = !n80 || !n79 || !n78 || !n77;
    let live_v32_b34: u16 = ALL & zb_holds(n2271);
    let ok_v33_b35: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v33_b35: bool = !n80 || !n79 || !n78 || !n77;
    let live_v33_b35: u16 = ALL & zb_holds(n2296);
    let ok_v34_b36: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v34_b36: bool = !n80 || !n79 || !n78 || !n77;
    let live_v34_b36: u16 = ALL & zb_holds(n2318);
    let ok_v36_b37: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v36_b37: bool = !n80 || !n79 || !n78 || !n77;
    let live_v36_b37: u16 = ALL & zb_holds(n2350);
    let ok_v37_b38: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v37_b38: bool = !n80 || !n79 || !n78 || !n77;
    let live_v37_b38: u16 = ALL & zb_holds(n2296);
    let ok_v38_b39: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v38_b39: bool = !n80 || !n79 || !n78 || !n77;
    let live_v38_b39: u16 = ALL & zb_holds(n2318);
    let ok_v40_b40: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v40_b40: bool = !n80 || !n79 || !n78 || !n77;
    let live_v40_b40: u16 = ALL & zb_holds(n2350);
    let ok_v41_b41: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v41_b41: bool = !n80 || !n79 || !n78 || !n77;
    let live_v41_b41: u16 = ALL & zb_holds(n2296);
    let ok_v42_b42: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v42_b42: bool = !n80 || !n79 || !n78 || !n77;
    let live_v42_b42: u16 = ALL & zb_holds(n2318);
    let ok_v48_b43: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v48_b43: bool = !n80 || !n79 || !n78 || !n77;
    let live_v48_b43: u16 = ALL & zb_holds(n2397);
    let ok_v49_b44: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v49_b44: bool = !n80 || !n79 || !n78 || !n77;
    let live_v49_b44: u16 = ALL & zb_holds(n2416);
    let ok_v50_b45: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v50_b45: bool = !n80 || !n79 || !n78 || !n77;
    let live_v50_b45: u16 = ALL & zb_holds(n2435);
    let ok_v52_b46: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v52_b46: bool = !n80 || !n79 || !n78 || !n77;
    let live_v52_b46: u16 = ALL & zb_holds(n2454);
    let ok_v53_b47: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v53_b47: bool = !n80 || !n79 || !n78 || !n77;
    let live_v53_b47: u16 = ALL & zb_holds(n2416);
    let ok_v54_b48: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v54_b48: bool = !n80 || !n79 || !n78 || !n77;
    let live_v54_b48: u16 = ALL & zb_holds(n2435);
    let ok_v56_b49: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v56_b49: bool = !n80 || !n79 || !n78 || !n77;
    let live_v56_b49: u16 = ALL & zb_holds(n2454);
    let ok_v57_b50: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v57_b50: bool = !n80 || !n79 || !n78 || !n77;
    let live_v57_b50: u16 = ALL & zb_holds(n2416);
    let ok_v58_b51: u16 = ALL & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c249) & zb_holds(n76) & zb_holds(r_c232) & zb_holds(n52) & zb_holds(n53) & zb_holds(n2087);
    let bd_v58_b51: bool = !n80 || !n79 || !n78 || !n77;
    let live_v58_b51: u16 = ALL & zb_holds(n2435);
    let sh0 = KShared0 {
        c87: n1198,
        c84: n57,
        c86: n161,
        c85: n86,
    };
    let sh1 = KShared1 {
        c84: n57,
        c86: n161,
        c85: n86,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n57,
        c86: n161,
        c278: n2082,
        c279: n2083,
        c254: n2080,
        c85: n86,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
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
    // 52 distinct button assignments; per outcome they fall
    // into [2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_0 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_0 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_0 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_0 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_0 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        h1: n2498, h2: n2499,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_0, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_1 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_1 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_1 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_1 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    declined |= live_v48_b10 & (if bd_v48_b10 { ALL } else { !ok_v48_b10 });
    take_0_1 |= live_v48_b10 & ok_v48_b10 & (if bd_v48_b10 { 0 } else { ALL });
    declined |= live_v49_b11 & (if bd_v49_b11 { ALL } else { !ok_v49_b11 });
    take_0_1 |= live_v49_b11 & ok_v49_b11 & (if bd_v49_b11 { 0 } else { ALL });
    declined |= live_v50_b12 & (if bd_v50_b12 { ALL } else { !ok_v50_b12 });
    take_0_1 |= live_v50_b12 & ok_v50_b12 & (if bd_v50_b12 { 0 } else { ALL });
    declined |= live_v52_b13 & (if bd_v52_b13 { ALL } else { !ok_v52_b13 });
    take_0_1 |= live_v52_b13 & ok_v52_b13 & (if bd_v52_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1416,
        c41: n1417,
        h1: n2504, h2: n2505,
    };
    // body 13: buttons 0x34, forks 0x0
    sink.o0(52, take_0_1, &sh0, &o0);
    declined |= live_v0_b14 & (if bd_v0_b14 { ALL } else { !ok_v0_b14 });
    take_1_0 |= live_v0_b14 & ok_v0_b14 & (if bd_v0_b14 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1606,
        c39: n1607,
        c20: r_c20,
        c38: n1602,
        h1: n2515, h2: n2516,
    };
    // body 14: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b15 & (if bd_v1_b15 { ALL } else { !ok_v1_b15 });
    take_1_1 |= live_v1_b15 & ok_v1_b15 & (if bd_v1_b15 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1650,
        c39: n1651,
        c20: r_c20,
        c38: n1646,
        h1: n2524, h2: n2525,
    };
    // body 15: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b16 & (if bd_v2_b16 { ALL } else { !ok_v2_b16 });
    take_1_2 |= live_v2_b16 & ok_v2_b16 & (if bd_v2_b16 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1694,
        c39: n1695,
        c20: r_c20,
        c38: n1690,
        h1: n2533, h2: n2534,
    };
    // body 16: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b17 & (if bd_v16_b17 { ALL } else { !ok_v16_b17 });
    take_1_3 |= live_v16_b17 & ok_v16_b17 & (if bd_v16_b17 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1737,
        c39: n1738,
        c20: r_c20,
        c38: n1733,
        h1: n2542, h2: n2543,
    };
    // body 17: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b18 & (if bd_v17_b18 { ALL } else { !ok_v17_b18 });
    take_1_4 |= live_v17_b18 & ok_v17_b18 & (if bd_v17_b18 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1780,
        c39: n1781,
        c20: r_c20,
        c38: n1776,
        h1: n2551, h2: n2552,
    };
    // body 18: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b19 & (if bd_v18_b19 { ALL } else { !ok_v18_b19 });
    take_1_5 |= live_v18_b19 & ok_v18_b19 & (if bd_v18_b19 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1823,
        c39: n1824,
        c20: r_c20,
        c38: n1819,
        h1: n2560, h2: n2561,
    };
    // body 19: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b20 & (if bd_v32_b20 { ALL } else { !ok_v32_b20 });
    take_1_6 |= live_v32_b20 & ok_v32_b20 & (if bd_v32_b20 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1853,
        c39: n1854,
        c20: n1416,
        c38: n1847,
        h1: n2571, h2: n2572,
    };
    // body 20: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b21 & (if bd_v33_b21 { ALL } else { !ok_v33_b21 });
    take_1_7 |= live_v33_b21 & ok_v33_b21 & (if bd_v33_b21 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1871,
        c39: n1872,
        c20: n1416,
        c38: n1865,
        h1: n2580, h2: n2581,
    };
    // body 21: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b22 & (if bd_v34_b22 { ALL } else { !ok_v34_b22 });
    take_1_8 |= live_v34_b22 & ok_v34_b22 & (if bd_v34_b22 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1889,
        c39: n1890,
        c20: n1416,
        c38: n1883,
        h1: n2589, h2: n2590,
    };
    // body 22: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b23 & (if bd_v36_b23 { ALL } else { !ok_v36_b23 });
    take_1_9 |= live_v36_b23 & ok_v36_b23 & (if bd_v36_b23 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1905,
        c39: n1906,
        c20: n1416,
        c38: n1899,
        h1: n2598, h2: n2599,
    };
    // body 23: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v48_b24 & (if bd_v48_b24 { ALL } else { !ok_v48_b24 });
    take_1_10 |= live_v48_b24 & ok_v48_b24 & (if bd_v48_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1935,
        c39: n1936,
        c20: n1416,
        c38: n1929,
        h1: n2607, h2: n2608,
    };
    // body 24: buttons 0x30, forks 0x0
    sink.o1(48, take_1_10, &sh1, &o1);
    declined |= live_v49_b25 & (if bd_v49_b25 { ALL } else { !ok_v49_b25 });
    take_1_11 |= live_v49_b25 & ok_v49_b25 & (if bd_v49_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1953,
        c39: n1954,
        c20: n1416,
        c38: n1947,
        h1: n2616, h2: n2617,
    };
    // body 25: buttons 0x31, forks 0x0
    sink.o1(49, take_1_11, &sh1, &o1);
    declined |= live_v50_b26 & (if bd_v50_b26 { ALL } else { !ok_v50_b26 });
    take_1_12 |= live_v50_b26 & ok_v50_b26 & (if bd_v50_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1971,
        c39: n1972,
        c20: n1416,
        c38: n1965,
        h1: n2625, h2: n2626,
    };
    // body 26: buttons 0x32, forks 0x0
    sink.o1(50, take_1_12, &sh1, &o1);
    declined |= live_v52_b27 & (if bd_v52_b27 { ALL } else { !ok_v52_b27 });
    take_1_13 |= live_v52_b27 & ok_v52_b27 & (if bd_v52_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1987,
        c39: n1988,
        c20: n1416,
        c38: n1981,
        h1: n2634, h2: n2635,
    };
    // body 27: buttons 0x34, forks 0x0
    sink.o1(52, take_1_13, &sh1, &o1);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_2_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2074,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2075,
        c270: r_c270,
        c271: r_c271,
        c236: n2076,
        c237: n2077,
        c272: n2081,
        c239: n2078,
        c246: n2007,
        c247: n2008,
        c280: n2108,
        c281: n2085,
        c253: n2107,
        h1: n2702, h2: n2703,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b29 & (if bd_v1_b29 { ALL } else { !ok_v1_b29 });
    take_2_1 |= live_v1_b29 & ok_v1_b29 & (if bd_v1_b29 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2074,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2075,
        c270: r_c270,
        c271: r_c271,
        c236: n2076,
        c237: n2077,
        c272: n2123,
        c239: n2078,
        c246: n2007,
        c247: n2008,
        c280: n2136,
        c281: n2125,
        c253: n2107,
        h1: n2711, h2: n2712,
    };
    // body 29: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b30 & (if bd_v2_b30 { ALL } else { !ok_v2_b30 });
    take_2_2 |= live_v2_b30 & ok_v2_b30 & (if bd_v2_b30 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2074,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2075,
        c270: r_c270,
        c271: r_c271,
        c236: n2076,
        c237: n2077,
        c272: n2150,
        c239: n2078,
        c246: n2007,
        c247: n2008,
        c280: n2163,
        c281: n2152,
        c253: n2107,
        h1: n2720, h2: n2721,
    };
    // body 30: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b31 & (if bd_v16_b31 { ALL } else { !ok_v16_b31 });
    take_2_3 |= live_v16_b31 & ok_v16_b31 & (if bd_v16_b31 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2074,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2075,
        c270: r_c270,
        c271: r_c271,
        c236: n2076,
        c237: n2077,
        c272: n2081,
        c239: n2173,
        c246: n2007,
        c247: n2165,
        c280: n2186,
        c281: n2175,
        c253: n2107,
        h1: n2746, h2: n2747,
    };
    // body 31: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b32 & (if bd_v17_b32 { ALL } else { !ok_v17_b32 });
    take_2_4 |= live_v17_b32 & ok_v17_b32 & (if bd_v17_b32 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2074,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2075,
        c270: r_c270,
        c271: r_c271,
        c236: n2076,
        c237: n2077,
        c272: n2123,
        c239: n2173,
        c246: n2007,
        c247: n2165,
        c280: n2205,
        c281: n2194,
        c253: n2107,
        h1: n2754, h2: n2755,
    };
    // body 32: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b33 & (if bd_v18_b33 { ALL } else { !ok_v18_b33 });
    take_2_5 |= live_v18_b33 & ok_v18_b33 & (if bd_v18_b33 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2074,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2075,
        c270: r_c270,
        c271: r_c271,
        c236: n2076,
        c237: n2077,
        c272: n2150,
        c239: n2173,
        c246: n2007,
        c247: n2165,
        c280: n2224,
        c281: n2213,
        c253: n2107,
        h1: n2762, h2: n2763,
    };
    // body 33: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b34 & (if bd_v32_b34 { ALL } else { !ok_v32_b34 });
    take_2_6 |= live_v32_b34 & ok_v32_b34 & (if bd_v32_b34 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2251,
        c269: n2252,
        c234: n2248,
        c270: n2253,
        c271: n2254,
        c236: n2249,
        c237: n2250,
        c272: n2081,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2270,
        c281: n2256,
        c253: n2269,
        h1: n2807, h2: n2808,
    };
    // body 34: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b35 & (if bd_v33_b35 { ALL } else { !ok_v33_b35 });
    take_2_7 |= live_v33_b35 & ok_v33_b35 & (if bd_v33_b35 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2251,
        c269: n2281,
        c234: n2248,
        c270: n2282,
        c271: n2254,
        c236: n2249,
        c237: n2250,
        c272: n2123,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2295,
        c281: n2284,
        c253: n2269,
        h1: n2823, h2: n2824,
    };
    // body 35: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b36 & (if bd_v34_b36 { ALL } else { !ok_v34_b36 });
    take_2_8 |= live_v34_b36 & ok_v34_b36 & (if bd_v34_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2251,
        c269: n2281,
        c234: n2248,
        c270: n2304,
        c271: n2254,
        c236: n2249,
        c237: n2250,
        c272: n2150,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2317,
        c281: n2306,
        c253: n2269,
        h1: n2836, h2: n2837,
    };
    // body 36: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b37 & (if bd_v36_b37 { ALL } else { !ok_v36_b37 });
    take_2_9 |= live_v36_b37 & ok_v36_b37 & (if bd_v36_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2334,
        c234: n2248,
        c270: n2335,
        c271: n2336,
        c236: n2249,
        c237: n2250,
        c272: n2081,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2349,
        c281: n2338,
        c253: n2269,
        h1: n2856, h2: n2857,
    };
    // body 37: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b38 & (if bd_v37_b38 { ALL } else { !ok_v37_b38 });
    take_2_10 |= live_v37_b38 & ok_v37_b38 & (if bd_v37_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2282,
        c271: n2336,
        c236: n2249,
        c237: n2250,
        c272: n2123,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2358,
        c281: n2356,
        c253: n2269,
        h1: n2870, h2: n2871,
    };
    // body 38: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b39 & (if bd_v38_b39 { ALL } else { !ok_v38_b39 });
    take_2_11 |= live_v38_b39 & ok_v38_b39 & (if bd_v38_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2304,
        c271: n2336,
        c236: n2249,
        c237: n2250,
        c272: n2150,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2366,
        c281: n2364,
        c253: n2269,
        h1: n2882, h2: n2883,
    };
    // body 39: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b40 & (if bd_v40_b40 { ALL } else { !ok_v40_b40 });
    take_2_12 |= live_v40_b40 & ok_v40_b40 & (if bd_v40_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2334,
        c234: n2248,
        c270: n2335,
        c271: n2371,
        c236: n2249,
        c237: n2250,
        c272: n2081,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2349,
        c281: n2372,
        c253: n2269,
        h1: n2892, h2: n2893,
    };
    // body 40: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b41 & (if bd_v41_b41 { ALL } else { !ok_v41_b41 });
    take_2_13 |= live_v41_b41 & ok_v41_b41 & (if bd_v41_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2282,
        c271: n2371,
        c236: n2249,
        c237: n2250,
        c272: n2123,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2358,
        c281: n2375,
        c253: n2269,
        h1: n2901, h2: n2902,
    };
    // body 41: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b42 & (if bd_v42_b42 { ALL } else { !ok_v42_b42 });
    take_2_14 |= live_v42_b42 & ok_v42_b42 & (if bd_v42_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2304,
        c271: n2371,
        c236: n2249,
        c237: n2250,
        c272: n2150,
        c239: n2078,
        c246: n2226,
        c247: n2008,
        c280: n2366,
        c281: n2378,
        c253: n2269,
        h1: n2910, h2: n2911,
    };
    // body 42: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b43 & (if bd_v48_b43 { ALL } else { !ok_v48_b43 });
    take_2_15 |= live_v48_b43 & ok_v48_b43 & (if bd_v48_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2251,
        c269: n2252,
        c234: n2248,
        c270: n2253,
        c271: n2254,
        c236: n2249,
        c237: n2250,
        c272: n2081,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2396,
        c281: n2385,
        c253: n2269,
        h1: n2934, h2: n2935,
    };
    // body 43: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b44 & (if bd_v49_b44 { ALL } else { !ok_v49_b44 });
    take_2_16 |= live_v49_b44 & ok_v49_b44 & (if bd_v49_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2251,
        c269: n2281,
        c234: n2248,
        c270: n2282,
        c271: n2254,
        c236: n2249,
        c237: n2250,
        c272: n2123,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2415,
        c281: n2404,
        c253: n2269,
        h1: n2948, h2: n2949,
    };
    // body 44: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b45 & (if bd_v50_b45 { ALL } else { !ok_v50_b45 });
    take_2_17 |= live_v50_b45 & ok_v50_b45 & (if bd_v50_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2251,
        c269: n2281,
        c234: n2248,
        c270: n2304,
        c271: n2254,
        c236: n2249,
        c237: n2250,
        c272: n2150,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2434,
        c281: n2423,
        c253: n2269,
        h1: n2960, h2: n2961,
    };
    // body 45: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b46 & (if bd_v52_b46 { ALL } else { !ok_v52_b46 });
    take_2_18 |= live_v52_b46 & ok_v52_b46 & (if bd_v52_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2334,
        c234: n2248,
        c270: n2335,
        c271: n2336,
        c236: n2249,
        c237: n2250,
        c272: n2081,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2453,
        c281: n2442,
        c253: n2269,
        h1: n2976, h2: n2977,
    };
    // body 46: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b47 & (if bd_v53_b47 { ALL } else { !ok_v53_b47 });
    take_2_19 |= live_v53_b47 & ok_v53_b47 & (if bd_v53_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2282,
        c271: n2336,
        c236: n2249,
        c237: n2250,
        c272: n2123,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2462,
        c281: n2460,
        c253: n2269,
        h1: n2990, h2: n2991,
    };
    // body 47: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b48 & (if bd_v54_b48 { ALL } else { !ok_v54_b48 });
    take_2_20 |= live_v54_b48 & ok_v54_b48 & (if bd_v54_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2304,
        c271: n2336,
        c236: n2249,
        c237: n2250,
        c272: n2150,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2470,
        c281: n2468,
        c253: n2269,
        h1: n3002, h2: n3003,
    };
    // body 48: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b49 & (if bd_v56_b49 { ALL } else { !ok_v56_b49 });
    take_2_21 |= live_v56_b49 & ok_v56_b49 & (if bd_v56_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2334,
        c234: n2248,
        c270: n2335,
        c271: n2371,
        c236: n2249,
        c237: n2250,
        c272: n2081,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2453,
        c281: n2473,
        c253: n2269,
        h1: n3011, h2: n3012,
    };
    // body 49: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b50 & (if bd_v57_b50 { ALL } else { !ok_v57_b50 });
    take_2_22 |= live_v57_b50 & ok_v57_b50 & (if bd_v57_b50 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2282,
        c271: n2371,
        c236: n2249,
        c237: n2250,
        c272: n2123,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2462,
        c281: n2476,
        c253: n2269,
        h1: n3020, h2: n3021,
    };
    // body 50: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b51 & (if bd_v58_b51 { ALL } else { !ok_v58_b51 });
    take_2_23 |= live_v58_b51 & ok_v58_b51 & (if bd_v58_b51 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2246,
        c41: n2247,
        c268: n2333,
        c269: n2281,
        c234: n2248,
        c270: n2304,
        c271: n2371,
        c236: n2249,
        c237: n2250,
        c272: n2150,
        c239: n2173,
        c246: n2226,
        c247: n2165,
        c280: n2470,
        c281: n2479,
        c253: n2269,
        h1: n3029, h2: n3030,
    };
    // body 51: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined
}
