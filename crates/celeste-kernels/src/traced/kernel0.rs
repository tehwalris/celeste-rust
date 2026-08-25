// GENERATED from a TRACED frame (shape 0). Do not edit.
//
// One input shape, 4 output shapes, 49 distinct button
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
    pub c88: ZN,
    pub c43: ZB,
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
    pub c39: ZN,
    pub c42: ZB,
    pub c88: ZN,
    pub c254: ZN,
    pub c43: ZB,
    pub c38: ZB,
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
        if !org.is_empty() { acc.origin.push(org[i]); }
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
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
        if !org.is_empty() { acc.origin.push(org[i]); }
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
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3], self.org);
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
    let n45: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n46: ZB = zn_gt(r_c39, zn_splat(P8::from_raw(0i32)));
    let n47: ZB = zb_and(r_c38, n46);
    let n48: ZN = zn_sub(r_c39, zn_splat(P8::from_raw(65536i32)));
    let n49: ZB = zn_le(n48, zn_splat(P8::from_raw(0i32)));
    let n50: ZB = zn_gt(n48, zn_splat(P8::from_raw(0i32)));
    let n51: ZB = zb_and(r_c41, n50);
    let n52: ZB = zb_and(r_c42, n50);
    let n53: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), r_c233);
    let n54: ZB = zb_and(r_c242, n50);
    let n55: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), r_c245);
    let n56: ZN = zsel_n(n49, zn_splat(P8::from_raw(524288i32)), r_c248);
    let n57: ZN = zsel_n(n49, zn_splat(P8::from_raw(8388608i32)), r_c249);
    let n58: ZN = zsel_n(n49, zn_splat(P8::from_raw(524288i32)), zn_splat(u.c264));
    let n59: ZN = zsel_n(n49, zn_splat(P8::from_raw(524288i32)), zn_splat(u.c265));
    let n60: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), zn_splat(u.c266));
    let n61: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), zn_splat(u.c267));
    let n62: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), r_c268);
    let n63: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), r_c269);
    let n64: ZN = zsel_n(n49, zn_splat(P8::from_raw(0i32)), r_c270);
    let n65: ZN = zsel_n(n49, zn_splat(P8::from_raw(-262144i32)), r_c271);
    let n66: ZB = zsel_b(n47, n51, r_c41);
    let n67: ZB = zsel_b(n47, n52, r_c42);
    let n68: ZN = zsel_n(n47, n53, r_c233);
    let n69: ZB = zsel_b(n47, n54, r_c242);
    let n70: ZN = zsel_n(n47, n55, r_c245);
    let n71: ZN = zsel_n(n47, n56, r_c248);
    let n72: ZN = zsel_n(n47, n57, r_c249);
    let n73: ZN = zsel_n(n47, n58, zn_splat(u.c264));
    let n74: ZN = zsel_n(n47, n59, zn_splat(u.c265));
    let n75: ZN = zsel_n(n47, n60, zn_splat(u.c266));
    let n76: ZN = zsel_n(n47, n61, zn_splat(u.c267));
    let n77: ZN = zsel_n(n47, n62, r_c268);
    let n78: ZN = zsel_n(n47, n63, r_c269);
    let n79: ZN = zsel_n(n47, n64, r_c270);
    let n80: ZN = zsel_n(n47, n65, r_c271);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n79);
    let n82: ZB = zb_not(n81);
    let n83: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n80);
    let n84: ZB = zb_not(n83);
    let n85: ZB = zb_or(n82, n84);
    let n86: ZB = zb_not(n85);
    let n87: ZN = zn_add(n77, n79);
    let n89: ZN = zn_add(n87, zn_splat(P8::from_raw(32768i32)));
    let n90: ZN = zn_flr(n89);
    let n91: ZB = zb_not(n69);
    let n92: ZB = zn_gt(n90, zn_splat(P8::from_raw(0i32)));
    let n93: ZB = zn_lt(n90, zn_splat(P8::from_raw(0i32)));
    let n95: ZN = zsel_n(n93, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n96: ZN = zsel_n(n92, zn_splat(P8::from_raw(65536i32)), n95);
    let n97: ZN = zn_abs(n90);
    let n98: ZN = zn_add(n71, n75);
    let n99: ZN = zn_add(n96, n98);
    let n100: ZN = zn_add(n72, n76);
    let n101: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n100);
    let n102: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n99, n101, n74, n73, P8::from_raw(0i32));
    let n103: ZN = zn_add(n71, n96);
    let n104: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n97);
    let n105: ZN = zn_add(n75, n103);
    let n106: ZN = zn_add(n96, n105);
    let n107: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n106, n101, n74, n73, P8::from_raw(0i32));
    let n108: ZN = zn_add(n96, n103);
    let n109: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n97);
    let n110: ZN = zn_add(n75, n108);
    let n111: ZN = zn_add(n96, n110);
    let n112: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n111, n101, n74, n73, P8::from_raw(0i32));
    let n113: ZN = zn_add(n96, n108);
    let n114: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n97);
    let n115: ZN = zn_add(n75, n113);
    let n116: ZN = zn_add(n96, n115);
    let n117: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n116, n101, n74, n73, P8::from_raw(0i32));
    let n118: ZN = zn_add(n96, n113);
    let n119: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n97);
    let n120: ZN = zn_add(n75, n118);
    let n121: ZN = zn_add(n96, n120);
    let n122: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n121, n101, n74, n73, P8::from_raw(0i32));
    let n123: ZN = zn_add(n96, n118);
    let n124: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n97);
    let n125: ZN = zn_add(n75, n123);
    let n126: ZN = zn_add(n96, n125);
    let n127: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n126, n101, n74, n73, P8::from_raw(0i32));
    let n128: ZN = zn_add(n96, n123);
    let n129: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n97);
    let n130: ZN = zn_add(n75, n128);
    let n131: ZN = zn_add(n96, n130);
    let n132: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n131, n101, n74, n73, P8::from_raw(0i32));
    let n133: ZN = zn_add(n96, n128);
    let n134: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n97);
    let n135: ZN = zn_add(n75, n133);
    let n136: ZN = zn_add(n96, n135);
    let n137: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n136, n101, n74, n73, P8::from_raw(0i32));
    let n138: ZN = zn_add(n96, n133);
    let n139: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n97);
    let n140: ZN = zsel_n(n137, n133, n138);
    let n141: ZB = zb_or(n137, n139);
    let n142: ZN = zsel_n(n134, n133, n140);
    let n143: ZB = zb_or(n134, n141);
    let n144: ZN = zsel_n(n132, n128, n142);
    let n145: ZB = zb_or(n132, n143);
    let n146: ZN = zsel_n(n129, n128, n144);
    let n147: ZB = zb_or(n129, n145);
    let n148: ZN = zsel_n(n127, n123, n146);
    let n149: ZB = zb_or(n127, n147);
    let n150: ZN = zsel_n(n124, n123, n148);
    let n151: ZB = zb_or(n124, n149);
    let n152: ZN = zsel_n(n122, n118, n150);
    let n153: ZB = zb_or(n122, n151);
    let n154: ZN = zsel_n(n119, n118, n152);
    let n155: ZB = zb_or(n119, n153);
    let n156: ZN = zsel_n(n117, n113, n154);
    let n157: ZB = zb_or(n117, n155);
    let n158: ZN = zsel_n(n114, n113, n156);
    let n159: ZB = zb_or(n114, n157);
    let n160: ZN = zsel_n(n112, n108, n158);
    let n161: ZB = zb_or(n112, n159);
    let n162: ZN = zsel_n(n109, n108, n160);
    let n163: ZB = zb_or(n109, n161);
    let n164: ZN = zsel_n(n107, n103, n162);
    let n165: ZB = zb_or(n107, n163);
    let n166: ZN = zsel_n(n104, n103, n164);
    let n167: ZB = zb_or(n104, n165);
    let n168: ZN = zsel_n(n102, n71, n166);
    let n169: ZB = zb_or(n102, n167);
    let n170: ZN = zn_add(n71, n90);
    let n171: ZN = zsel_n(n69, n168, n170);
    let n172: ZB = zb_or(n91, n169);
    let n173: ZN = zn_add(n78, n80);
    let n174: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n173);
    let n175: ZN = zn_flr(n174);
    let n176: ZB = zn_gt(n175, zn_splat(P8::from_raw(0i32)));
    let n177: ZB = zn_lt(n175, zn_splat(P8::from_raw(0i32)));
    let n178: ZN = zsel_n(n177, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n179: ZN = zsel_n(n176, zn_splat(P8::from_raw(65536i32)), n178);
    let n180: ZN = zn_abs(n175);
    let n181: ZN = zn_add(n75, n171);
    let n182: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n181);
    let n183: ZN = zn_add(n100, n179);
    let n184: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n183, n74, n73, P8::from_raw(0i32));
    let n185: ZN = zn_add(n72, n179);
    let n186: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n180);
    let n187: ZN = zn_add(n76, n185);
    let n188: ZN = zn_add(n179, n187);
    let n189: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n188, n74, n73, P8::from_raw(0i32));
    let n190: ZN = zn_add(n179, n185);
    let n191: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n180);
    let n192: ZN = zn_add(n76, n190);
    let n193: ZN = zn_add(n179, n192);
    let n194: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n193, n74, n73, P8::from_raw(0i32));
    let n195: ZN = zn_add(n179, n190);
    let n196: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n180);
    let n197: ZN = zn_add(n76, n195);
    let n198: ZN = zn_add(n179, n197);
    let n199: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n198, n74, n73, P8::from_raw(0i32));
    let n200: ZN = zn_add(n179, n195);
    let n201: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n180);
    let n202: ZN = zn_add(n76, n200);
    let n203: ZN = zn_add(n179, n202);
    let n204: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n203, n74, n73, P8::from_raw(0i32));
    let n205: ZN = zn_add(n179, n200);
    let n206: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n180);
    let n207: ZN = zn_add(n76, n205);
    let n208: ZN = zn_add(n179, n207);
    let n209: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n208, n74, n73, P8::from_raw(0i32));
    let n210: ZN = zn_add(n179, n205);
    let n211: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n180);
    let n212: ZN = zn_add(n76, n210);
    let n213: ZN = zn_add(n179, n212);
    let n214: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n213, n74, n73, P8::from_raw(0i32));
    let n215: ZN = zn_add(n179, n210);
    let n216: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n180);
    let n217: ZN = zn_add(n76, n215);
    let n218: ZN = zn_add(n179, n217);
    let n219: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n182, n218, n74, n73, P8::from_raw(0i32));
    let n220: ZN = zn_add(n179, n215);
    let n221: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n180);
    let n222: ZB = zb_and(n172, n221);
    let n223: ZN = zsel_n(n219, n215, n220);
    let n224: ZB = zsel_b(n219, n172, n222);
    let n225: ZN = zsel_n(n216, n215, n223);
    let n226: ZB = zsel_b(n216, n172, n224);
    let n227: ZN = zsel_n(n214, n210, n225);
    let n228: ZB = zsel_b(n214, n172, n226);
    let n229: ZN = zsel_n(n211, n210, n227);
    let n230: ZB = zsel_b(n211, n172, n228);
    let n231: ZN = zsel_n(n209, n205, n229);
    let n232: ZB = zsel_b(n209, n172, n230);
    let n233: ZN = zsel_n(n206, n205, n231);
    let n234: ZB = zsel_b(n206, n172, n232);
    let n235: ZN = zsel_n(n204, n200, n233);
    let n236: ZB = zsel_b(n204, n172, n234);
    let n237: ZN = zsel_n(n201, n200, n235);
    let n238: ZB = zsel_b(n201, n172, n236);
    let n239: ZN = zsel_n(n199, n195, n237);
    let n240: ZB = zsel_b(n199, n172, n238);
    let n241: ZN = zsel_n(n196, n195, n239);
    let n242: ZB = zsel_b(n196, n172, n240);
    let n243: ZN = zsel_n(n194, n190, n241);
    let n244: ZB = zsel_b(n194, n172, n242);
    let n245: ZN = zsel_n(n191, n190, n243);
    let n246: ZB = zsel_b(n191, n172, n244);
    let n247: ZN = zsel_n(n189, n185, n245);
    let n248: ZB = zsel_b(n189, n172, n246);
    let n249: ZN = zsel_n(n186, n185, n247);
    let n250: ZB = zsel_b(n186, n172, n248);
    let n251: ZN = zsel_n(n184, n72, n249);
    let n252: ZB = zsel_b(n184, n172, n250);
    let n253: ZN = zn_add(n72, n175);
    let n254: ZN = zsel_n(n69, n251, n253);
    let n255: ZB = zsel_b(n69, n252, n172);
    let n256: ZN = zsel_n(n85, n171, n71);
    let n257: ZN = zsel_n(n85, n254, n72);
    let n258: ZB = zb_or(n86, n255);
    let n259: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n70);
    let n260: ZB = zb_not(n259);
    let n261: ZB = zb_and(n45, n260);
    let n262: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), n70);
    let n263: ZB = zb_not(n262);
    let n264: ZB = zb_and(n261, n263);
    let n265: ZN = zn_sub(n68, zn_splat(P8::from_raw(65536i32)));
    let n266: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), n70);
    let n267: ZB = zb_and(n264, n266);
    let n268: ZB = zn_lt(n265, zn_splat(P8::from_raw(0i32)));
    let n269: ZB = zb_and(n267, n268);
    let n270: ZB = zb_not(r_c43);
    let n271: ZB = zb_and(n269, n270);
    let n272: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n256);
    let n273: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n257);
    let n274: ZN = zn_div(n272, zn_splat(P8::from_raw(524288i32)));
    let n275: ZN = zn_flr(n274);
    let n276: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n275);
    let n277: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n272);
    let n278: ZN = zn_sub(n277, zn_splat(P8::from_raw(65536i32)));
    let n279: ZN = zn_div(n278, zn_splat(P8::from_raw(524288i32)));
    let n280: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n279);
    let n281: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n276);
    let n282: ZB = zn_le(n281, n280);
    let n283: ZB = zn_gt(n281, n280);
    let n284: ZB = zb_and(n271, n282);
    let n285: ZB = zb_and(n271, n283);
    let n286: ZN = zn_div(n273, zn_splat(P8::from_raw(524288i32)));
    let n287: ZN = zn_flr(n286);
    let n288: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n287);
    let n289: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n273);
    let n290: ZN = zn_sub(n289, zn_splat(P8::from_raw(65536i32)));
    let n291: ZN = zn_div(n290, zn_splat(P8::from_raw(524288i32)));
    let n292: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n291);
    let n293: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n288);
    let n294: ZB = zn_le(n293, n292);
    let n295: ZB = zn_gt(n293, n292);
    let n296: ZB = zb_and(n284, n294);
    let n297: ZB = zb_and(n284, n295);
    let n298: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n281);
    let n299: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n293);
    let n300: ZN = zn_mget(g.cart, n298, n299);
    let n301: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n300);
    let n302: ZB = zb_not(n301);
    let n303: ZB = zb_and(n296, n301);
    let n304: ZB = zb_and(n296, n302);
    let n305: ZN = zn_rem(n290, zn_splat(P8::from_raw(524288i32)));
    let n306: ZB = zn_ge(n305, zn_splat(P8::from_raw(393216i32)));
    let n307: ZB = zn_lt(n305, zn_splat(P8::from_raw(393216i32)));
    let n308: ZB = zb_and(n303, n307);
    let n309: ZB = zb_and(n303, n306);
    let n310: ZN = zn_mul(n293, zn_splat(P8::from_raw(524288i32)));
    let n311: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n310);
    let n312: ZB = zn_eq(n289, n311);
    let n313: ZB = zb_or(n308, n309);
    let n314: ZB = zb_or(n306, n312);
    let n315: ZB = zb_or(n304, n313);
    let n316: ZB = zb_and(n301, n314);
    let n317: ZB = zb_not(n316);
    let n318: ZB = zb_and(n315, n316);
    let n319: ZB = zb_and(n315, n317);
    let n320: ZB = zb_or(n318, n319);
    let n321: ZB = zb_and(n316, n320);
    let n322: ZB = zb_and(n317, n320);
    let n323: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n300);
    let n324: ZB = zb_not(n323);
    let n325: ZB = zb_and(n322, n323);
    let n326: ZB = zb_and(n322, n324);
    let n327: ZN = zn_rem(n273, zn_splat(P8::from_raw(524288i32)));
    let n328: ZB = zn_le(n327, zn_splat(P8::from_raw(131072i32)));
    let n329: ZB = zb_or(n325, n326);
    let n330: ZB = zb_and(n323, n328);
    let n331: ZB = zb_not(n330);
    let n332: ZB = zb_and(n329, n330);
    let n333: ZB = zb_and(n329, n331);
    let n334: ZB = zb_or(n332, n333);
    let n335: ZB = zb_and(n330, n334);
    let n336: ZB = zb_and(n331, n334);
    let n338: ZB = zn_eq(n300, zn_splat(P8::from_raw(2818048i32)));
    let n339: ZB = zb_not(n338);
    let n340: ZB = zb_and(n336, n338);
    let n341: ZB = zb_and(n336, n339);
    let n342: ZN = zn_rem(n272, zn_splat(P8::from_raw(524288i32)));
    let n343: ZB = zn_le(n342, zn_splat(P8::from_raw(131072i32)));
    let n344: ZB = zb_or(n340, n341);
    let n345: ZB = zb_and(n338, n343);
    let n346: ZB = zb_not(n345);
    let n347: ZB = zb_and(n344, n345);
    let n348: ZB = zb_and(n344, n346);
    let n349: ZB = zb_or(n347, n348);
    let n350: ZB = zb_and(n345, n349);
    let n351: ZB = zb_and(n346, n349);
    let n353: ZB = zn_eq(n300, zn_splat(P8::from_raw(3866624i32)));
    let n354: ZB = zb_not(n353);
    let n355: ZB = zb_and(n351, n353);
    let n356: ZB = zb_and(n351, n354);
    let n357: ZN = zn_rem(n278, zn_splat(P8::from_raw(524288i32)));
    let n358: ZB = zn_ge(n357, zn_splat(P8::from_raw(393216i32)));
    let n359: ZB = zn_lt(n357, zn_splat(P8::from_raw(393216i32)));
    let n360: ZB = zb_and(n355, n359);
    let n361: ZB = zb_and(n355, n358);
    let n362: ZN = zn_mul(n281, zn_splat(P8::from_raw(524288i32)));
    let n363: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n362);
    let n364: ZB = zn_eq(n277, n363);
    let n365: ZB = zb_or(n360, n361);
    let n366: ZB = zb_or(n358, n364);
    let n367: ZB = zb_or(n356, n365);
    let n368: ZB = zb_and(n353, n366);
    let n369: ZB = zb_not(n368);
    let n370: ZB = zb_and(n367, n368);
    let n371: ZB = zb_and(n367, n369);
    let n372: ZB = zb_or(n370, n371);
    let n373: ZB = zb_and(n368, n372);
    let n374: ZB = zb_and(n369, n372);
    let n375: ZB = zb_or(n350, n373);
    let n376: ZB = zb_or(n335, n375);
    let n377: ZB = zb_or(n321, n376);
    let n378: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n288);
    let n379: ZB = zn_le(n378, n292);
    let n380: ZB = zn_gt(n378, n292);
    let n381: ZB = zb_and(n374, n379);
    let n382: ZB = zb_and(n374, n380);
    let n383: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n378);
    let n384: ZN = zn_mget(g.cart, n298, n383);
    let n385: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n384);
    let n386: ZB = zb_not(n385);
    let n387: ZB = zb_and(n381, n385);
    let n388: ZB = zb_and(n381, n386);
    let n389: ZB = zb_and(n307, n387);
    let n390: ZB = zb_and(n306, n387);
    let n391: ZN = zn_mul(n378, zn_splat(P8::from_raw(524288i32)));
    let n392: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n391);
    let n393: ZB = zn_eq(n289, n392);
    let n394: ZB = zb_or(n389, n390);
    let n395: ZB = zb_or(n306, n393);
    let n396: ZB = zb_or(n388, n394);
    let n397: ZB = zb_and(n385, n395);
    let n398: ZB = zb_not(n397);
    let n399: ZB = zb_and(n396, n397);
    let n400: ZB = zb_and(n396, n398);
    let n401: ZB = zb_or(n399, n400);
    let n402: ZB = zb_and(n397, n401);
    let n403: ZB = zb_and(n398, n401);
    let n404: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n384);
    let n405: ZB = zb_not(n404);
    let n406: ZB = zb_and(n403, n404);
    let n407: ZB = zb_and(n403, n405);
    let n408: ZB = zb_or(n406, n407);
    let n409: ZB = zb_and(n328, n404);
    let n410: ZB = zb_not(n409);
    let n411: ZB = zb_and(n408, n409);
    let n412: ZB = zb_and(n408, n410);
    let n413: ZB = zb_or(n411, n412);
    let n414: ZB = zb_and(n409, n413);
    let n415: ZB = zb_and(n410, n413);
    let n416: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n384);
    let n417: ZB = zb_not(n416);
    let n418: ZB = zb_and(n415, n416);
    let n419: ZB = zb_and(n415, n417);
    let n420: ZB = zb_or(n418, n419);
    let n421: ZB = zb_and(n343, n416);
    let n422: ZB = zb_not(n421);
    let n423: ZB = zb_and(n420, n421);
    let n424: ZB = zb_and(n420, n422);
    let n425: ZB = zb_or(n423, n424);
    let n426: ZB = zb_and(n421, n425);
    let n427: ZB = zb_and(n422, n425);
    let n428: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n384);
    let n429: ZB = zb_not(n428);
    let n430: ZB = zb_and(n427, n428);
    let n431: ZB = zb_and(n427, n429);
    let n432: ZB = zb_and(n359, n430);
    let n433: ZB = zb_and(n358, n430);
    let n434: ZB = zb_or(n432, n433);
    let n435: ZB = zb_or(n431, n434);
    let n436: ZB = zb_and(n366, n428);
    let n437: ZB = zb_not(n436);
    let n438: ZB = zb_and(n435, n436);
    let n439: ZB = zb_and(n435, n437);
    let n440: ZB = zb_or(n438, n439);
    let n441: ZB = zb_and(n436, n440);
    let n442: ZB = zb_and(n437, n440);
    let n443: ZB = zb_or(n426, n441);
    let n444: ZB = zb_or(n414, n443);
    let n445: ZB = zb_or(n402, n444);
    let n446: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n288);
    let n447: ZB = zn_le(n446, n292);
    let n448: ZB = zn_gt(n446, n292);
    let n449: ZB = zb_and(n442, n447);
    let n450: ZB = zb_and(n442, n448);
    let n451: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n446);
    let n452: ZN = zn_mget(g.cart, n298, n451);
    let n453: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n452);
    let n454: ZB = zb_not(n453);
    let n455: ZB = zb_and(n449, n453);
    let n456: ZB = zb_and(n449, n454);
    let n457: ZB = zb_and(n307, n455);
    let n458: ZB = zb_and(n306, n455);
    let n459: ZN = zn_mul(n446, zn_splat(P8::from_raw(524288i32)));
    let n460: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n459);
    let n461: ZB = zn_eq(n289, n460);
    let n462: ZB = zb_or(n457, n458);
    let n463: ZB = zb_or(n306, n461);
    let n464: ZB = zb_or(n456, n462);
    let n465: ZB = zb_and(n453, n463);
    let n466: ZB = zb_not(n465);
    let n467: ZB = zb_and(n464, n465);
    let n468: ZB = zb_and(n464, n466);
    let n469: ZB = zb_or(n467, n468);
    let n470: ZB = zb_and(n465, n469);
    let n471: ZB = zb_and(n466, n469);
    let n472: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n452);
    let n473: ZB = zb_not(n472);
    let n474: ZB = zb_and(n471, n472);
    let n475: ZB = zb_and(n471, n473);
    let n476: ZB = zb_or(n474, n475);
    let n477: ZB = zb_and(n328, n472);
    let n478: ZB = zb_not(n477);
    let n479: ZB = zb_and(n476, n477);
    let n480: ZB = zb_and(n476, n478);
    let n481: ZB = zb_or(n479, n480);
    let n482: ZB = zb_and(n477, n481);
    let n483: ZB = zb_and(n478, n481);
    let n484: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n452);
    let n485: ZB = zb_not(n484);
    let n486: ZB = zb_and(n483, n484);
    let n487: ZB = zb_and(n483, n485);
    let n488: ZB = zb_or(n486, n487);
    let n489: ZB = zb_and(n343, n484);
    let n490: ZB = zb_not(n489);
    let n491: ZB = zb_and(n488, n489);
    let n492: ZB = zb_and(n488, n490);
    let n493: ZB = zb_or(n491, n492);
    let n494: ZB = zb_and(n489, n493);
    let n495: ZB = zb_and(n490, n493);
    let n496: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n452);
    let n497: ZB = zb_not(n496);
    let n498: ZB = zb_and(n495, n496);
    let n499: ZB = zb_and(n495, n497);
    let n500: ZB = zb_and(n359, n498);
    let n501: ZB = zb_and(n358, n498);
    let n502: ZB = zb_or(n500, n501);
    let n503: ZB = zb_or(n499, n502);
    let n504: ZB = zb_and(n366, n496);
    let n505: ZB = zb_not(n504);
    let n506: ZB = zb_and(n503, n504);
    let n507: ZB = zb_and(n503, n505);
    let n508: ZB = zb_or(n506, n507);
    let n509: ZB = zb_and(n504, n508);
    let n510: ZB = zb_and(n505, n508);
    let n511: ZB = zb_or(n494, n509);
    let n512: ZB = zb_or(n482, n511);
    let n513: ZB = zb_or(n470, n512);
    let n514: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n288);
    let n515: ZB = zn_gt(n514, n292);
    let n516: ZB = zb_and(n258, n515);
    let n517: ZB = zb_or(n450, n510);
    let n518: ZB = zsel_b(n448, n258, n516);
    let n519: ZB = zb_or(n445, n513);
    let n520: ZB = zb_or(n382, n517);
    let n521: ZB = zsel_b(n380, n258, n518);
    let n522: ZB = zb_or(n377, n519);
    let n523: ZB = zb_or(n297, n520);
    let n524: ZB = zsel_b(n295, n258, n521);
    let n525: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n276);
    let n526: ZB = zn_le(n525, n280);
    let n527: ZB = zn_gt(n525, n280);
    let n528: ZB = zb_and(n523, n526);
    let n529: ZB = zb_and(n523, n527);
    let n530: ZB = zb_and(n294, n528);
    let n531: ZB = zb_and(n295, n528);
    let n532: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n525);
    let n533: ZN = zn_mget(g.cart, n532, n299);
    let n534: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n533);
    let n535: ZB = zb_not(n534);
    let n536: ZB = zb_and(n530, n534);
    let n537: ZB = zb_and(n530, n535);
    let n538: ZB = zb_and(n307, n536);
    let n539: ZB = zb_and(n306, n536);
    let n540: ZB = zb_or(n538, n539);
    let n541: ZB = zb_or(n537, n540);
    let n542: ZB = zb_and(n314, n534);
    let n543: ZB = zb_not(n542);
    let n544: ZB = zb_and(n541, n542);
    let n545: ZB = zb_and(n541, n543);
    let n546: ZB = zb_or(n544, n545);
    let n547: ZB = zb_and(n542, n546);
    let n548: ZB = zb_and(n543, n546);
    let n549: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n533);
    let n550: ZB = zb_not(n549);
    let n551: ZB = zb_and(n548, n549);
    let n552: ZB = zb_and(n548, n550);
    let n553: ZB = zb_or(n551, n552);
    let n554: ZB = zb_and(n328, n549);
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n553, n554);
    let n557: ZB = zb_and(n553, n555);
    let n558: ZB = zb_or(n556, n557);
    let n559: ZB = zb_and(n554, n558);
    let n560: ZB = zb_and(n555, n558);
    let n561: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n533);
    let n562: ZB = zb_not(n561);
    let n563: ZB = zb_and(n560, n561);
    let n564: ZB = zb_and(n560, n562);
    let n565: ZB = zb_or(n563, n564);
    let n566: ZB = zb_and(n343, n561);
    let n567: ZB = zb_not(n566);
    let n568: ZB = zb_and(n565, n566);
    let n569: ZB = zb_and(n565, n567);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zb_and(n566, n570);
    let n572: ZB = zb_and(n567, n570);
    let n573: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n533);
    let n574: ZB = zb_not(n573);
    let n575: ZB = zb_and(n572, n573);
    let n576: ZB = zb_and(n572, n574);
    let n577: ZB = zb_and(n359, n575);
    let n578: ZB = zb_and(n358, n575);
    let n579: ZN = zn_mul(n525, zn_splat(P8::from_raw(524288i32)));
    let n580: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n579);
    let n581: ZB = zn_eq(n277, n580);
    let n582: ZB = zb_or(n577, n578);
    let n583: ZB = zb_or(n358, n581);
    let n584: ZB = zb_or(n576, n582);
    let n585: ZB = zb_and(n573, n583);
    let n586: ZB = zb_not(n585);
    let n587: ZB = zb_and(n584, n585);
    let n588: ZB = zb_and(n584, n586);
    let n589: ZB = zb_or(n587, n588);
    let n590: ZB = zb_and(n585, n589);
    let n591: ZB = zb_and(n586, n589);
    let n592: ZB = zb_or(n571, n590);
    let n593: ZB = zb_or(n559, n592);
    let n594: ZB = zb_or(n547, n593);
    let n595: ZB = zb_and(n379, n591);
    let n596: ZB = zb_and(n380, n591);
    let n597: ZN = zn_mget(g.cart, n532, n383);
    let n598: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n597);
    let n599: ZB = zb_not(n598);
    let n600: ZB = zb_and(n595, n598);
    let n601: ZB = zb_and(n595, n599);
    let n602: ZB = zb_and(n307, n600);
    let n603: ZB = zb_and(n306, n600);
    let n604: ZB = zb_or(n602, n603);
    let n605: ZB = zb_or(n601, n604);
    let n606: ZB = zb_and(n395, n598);
    let n607: ZB = zb_not(n606);
    let n608: ZB = zb_and(n605, n606);
    let n609: ZB = zb_and(n605, n607);
    let n610: ZB = zb_or(n608, n609);
    let n611: ZB = zb_and(n606, n610);
    let n612: ZB = zb_and(n607, n610);
    let n613: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n597);
    let n614: ZB = zb_not(n613);
    let n615: ZB = zb_and(n612, n613);
    let n616: ZB = zb_and(n612, n614);
    let n617: ZB = zb_or(n615, n616);
    let n618: ZB = zb_and(n328, n613);
    let n619: ZB = zb_not(n618);
    let n620: ZB = zb_and(n617, n618);
    let n621: ZB = zb_and(n617, n619);
    let n622: ZB = zb_or(n620, n621);
    let n623: ZB = zb_and(n618, n622);
    let n624: ZB = zb_and(n619, n622);
    let n625: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n597);
    let n626: ZB = zb_not(n625);
    let n627: ZB = zb_and(n624, n625);
    let n628: ZB = zb_and(n624, n626);
    let n629: ZB = zb_or(n627, n628);
    let n630: ZB = zb_and(n343, n625);
    let n631: ZB = zb_not(n630);
    let n632: ZB = zb_and(n629, n630);
    let n633: ZB = zb_and(n629, n631);
    let n634: ZB = zb_or(n632, n633);
    let n635: ZB = zb_and(n630, n634);
    let n636: ZB = zb_and(n631, n634);
    let n637: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n597);
    let n638: ZB = zb_not(n637);
    let n639: ZB = zb_and(n636, n637);
    let n640: ZB = zb_and(n636, n638);
    let n641: ZB = zb_and(n359, n639);
    let n642: ZB = zb_and(n358, n639);
    let n643: ZB = zb_or(n641, n642);
    let n644: ZB = zb_or(n640, n643);
    let n645: ZB = zb_and(n583, n637);
    let n646: ZB = zb_not(n645);
    let n647: ZB = zb_and(n644, n645);
    let n648: ZB = zb_and(n644, n646);
    let n649: ZB = zb_or(n647, n648);
    let n650: ZB = zb_and(n645, n649);
    let n651: ZB = zb_and(n646, n649);
    let n652: ZB = zb_or(n635, n650);
    let n653: ZB = zb_or(n623, n652);
    let n654: ZB = zb_or(n611, n653);
    let n655: ZB = zb_and(n447, n651);
    let n656: ZB = zb_and(n448, n651);
    let n657: ZN = zn_mget(g.cart, n532, n451);
    let n658: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n657);
    let n659: ZB = zb_not(n658);
    let n660: ZB = zb_and(n655, n658);
    let n661: ZB = zb_and(n655, n659);
    let n662: ZB = zb_and(n307, n660);
    let n663: ZB = zb_and(n306, n660);
    let n664: ZB = zb_or(n662, n663);
    let n665: ZB = zb_or(n661, n664);
    let n666: ZB = zb_and(n463, n658);
    let n667: ZB = zb_not(n666);
    let n668: ZB = zb_and(n665, n666);
    let n669: ZB = zb_and(n665, n667);
    let n670: ZB = zb_or(n668, n669);
    let n671: ZB = zb_and(n666, n670);
    let n672: ZB = zb_and(n667, n670);
    let n673: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n657);
    let n674: ZB = zb_not(n673);
    let n675: ZB = zb_and(n672, n673);
    let n676: ZB = zb_and(n672, n674);
    let n677: ZB = zb_or(n675, n676);
    let n678: ZB = zb_and(n328, n673);
    let n679: ZB = zb_not(n678);
    let n680: ZB = zb_and(n677, n678);
    let n681: ZB = zb_and(n677, n679);
    let n682: ZB = zb_or(n680, n681);
    let n683: ZB = zb_and(n678, n682);
    let n684: ZB = zb_and(n679, n682);
    let n685: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n657);
    let n686: ZB = zb_not(n685);
    let n687: ZB = zb_and(n684, n685);
    let n688: ZB = zb_and(n684, n686);
    let n689: ZB = zb_or(n687, n688);
    let n690: ZB = zb_and(n343, n685);
    let n691: ZB = zb_not(n690);
    let n692: ZB = zb_and(n689, n690);
    let n693: ZB = zb_and(n689, n691);
    let n694: ZB = zb_or(n692, n693);
    let n695: ZB = zb_and(n690, n694);
    let n696: ZB = zb_and(n691, n694);
    let n697: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n657);
    let n698: ZB = zb_not(n697);
    let n699: ZB = zb_and(n696, n697);
    let n700: ZB = zb_and(n696, n698);
    let n701: ZB = zb_and(n359, n699);
    let n702: ZB = zb_and(n358, n699);
    let n703: ZB = zb_or(n701, n702);
    let n704: ZB = zb_or(n700, n703);
    let n705: ZB = zb_and(n583, n697);
    let n706: ZB = zb_not(n705);
    let n707: ZB = zb_and(n704, n705);
    let n708: ZB = zb_and(n704, n706);
    let n709: ZB = zb_or(n707, n708);
    let n710: ZB = zb_and(n705, n709);
    let n711: ZB = zb_and(n706, n709);
    let n712: ZB = zb_or(n695, n710);
    let n713: ZB = zb_or(n683, n712);
    let n714: ZB = zb_or(n671, n713);
    let n715: ZB = zb_and(n515, n524);
    let n716: ZB = zb_or(n656, n711);
    let n717: ZB = zsel_b(n448, n524, n715);
    let n718: ZB = zb_or(n654, n714);
    let n719: ZB = zb_or(n596, n716);
    let n720: ZB = zsel_b(n380, n524, n717);
    let n721: ZB = zb_or(n594, n718);
    let n722: ZB = zb_or(n531, n719);
    let n723: ZB = zsel_b(n295, n524, n720);
    let n724: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n276);
    let n725: ZB = zn_le(n724, n280);
    let n726: ZB = zn_gt(n724, n280);
    let n727: ZB = zb_and(n722, n725);
    let n728: ZB = zb_and(n722, n726);
    let n729: ZB = zb_and(n294, n727);
    let n730: ZB = zb_and(n295, n727);
    let n731: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n724);
    let n732: ZN = zn_mget(g.cart, n731, n299);
    let n733: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n732);
    let n734: ZB = zb_not(n733);
    let n735: ZB = zb_and(n729, n733);
    let n736: ZB = zb_and(n729, n734);
    let n737: ZB = zb_and(n307, n735);
    let n738: ZB = zb_and(n306, n735);
    let n739: ZB = zb_or(n737, n738);
    let n740: ZB = zb_or(n736, n739);
    let n741: ZB = zb_and(n314, n733);
    let n742: ZB = zb_not(n741);
    let n743: ZB = zb_and(n740, n741);
    let n744: ZB = zb_and(n740, n742);
    let n745: ZB = zb_or(n743, n744);
    let n746: ZB = zb_and(n741, n745);
    let n747: ZB = zb_and(n742, n745);
    let n748: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n732);
    let n749: ZB = zb_not(n748);
    let n750: ZB = zb_and(n747, n748);
    let n751: ZB = zb_and(n747, n749);
    let n752: ZB = zb_or(n750, n751);
    let n753: ZB = zb_and(n328, n748);
    let n754: ZB = zb_not(n753);
    let n755: ZB = zb_and(n752, n753);
    let n756: ZB = zb_and(n752, n754);
    let n757: ZB = zb_or(n755, n756);
    let n758: ZB = zb_and(n753, n757);
    let n759: ZB = zb_and(n754, n757);
    let n760: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n732);
    let n761: ZB = zb_not(n760);
    let n762: ZB = zb_and(n759, n760);
    let n763: ZB = zb_and(n759, n761);
    let n764: ZB = zb_or(n762, n763);
    let n765: ZB = zb_and(n343, n760);
    let n766: ZB = zb_not(n765);
    let n767: ZB = zb_and(n764, n765);
    let n768: ZB = zb_and(n764, n766);
    let n769: ZB = zb_or(n767, n768);
    let n770: ZB = zb_and(n765, n769);
    let n771: ZB = zb_and(n766, n769);
    let n772: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n732);
    let n773: ZB = zb_not(n772);
    let n774: ZB = zb_and(n771, n772);
    let n775: ZB = zb_and(n771, n773);
    let n776: ZB = zb_and(n359, n774);
    let n777: ZB = zb_and(n358, n774);
    let n778: ZN = zn_mul(n724, zn_splat(P8::from_raw(524288i32)));
    let n779: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n778);
    let n780: ZB = zn_eq(n277, n779);
    let n781: ZB = zb_or(n776, n777);
    let n782: ZB = zb_or(n358, n780);
    let n783: ZB = zb_or(n775, n781);
    let n784: ZB = zb_and(n772, n782);
    let n785: ZB = zb_not(n784);
    let n786: ZB = zb_and(n783, n784);
    let n787: ZB = zb_and(n783, n785);
    let n788: ZB = zb_or(n786, n787);
    let n789: ZB = zb_and(n784, n788);
    let n790: ZB = zb_and(n785, n788);
    let n791: ZB = zb_or(n770, n789);
    let n792: ZB = zb_or(n758, n791);
    let n793: ZB = zb_or(n746, n792);
    let n794: ZB = zb_and(n379, n790);
    let n795: ZB = zb_and(n380, n790);
    let n796: ZN = zn_mget(g.cart, n731, n383);
    let n797: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n796);
    let n798: ZB = zb_not(n797);
    let n799: ZB = zb_and(n794, n797);
    let n800: ZB = zb_and(n794, n798);
    let n801: ZB = zb_and(n307, n799);
    let n802: ZB = zb_and(n306, n799);
    let n803: ZB = zb_or(n801, n802);
    let n804: ZB = zb_or(n800, n803);
    let n805: ZB = zb_and(n395, n797);
    let n806: ZB = zb_not(n805);
    let n807: ZB = zb_and(n804, n805);
    let n808: ZB = zb_and(n804, n806);
    let n809: ZB = zb_or(n807, n808);
    let n810: ZB = zb_and(n805, n809);
    let n811: ZB = zb_and(n806, n809);
    let n812: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n796);
    let n813: ZB = zb_not(n812);
    let n814: ZB = zb_and(n811, n812);
    let n815: ZB = zb_and(n811, n813);
    let n816: ZB = zb_or(n814, n815);
    let n817: ZB = zb_and(n328, n812);
    let n818: ZB = zb_not(n817);
    let n819: ZB = zb_and(n816, n817);
    let n820: ZB = zb_and(n816, n818);
    let n821: ZB = zb_or(n819, n820);
    let n822: ZB = zb_and(n817, n821);
    let n823: ZB = zb_and(n818, n821);
    let n824: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n796);
    let n825: ZB = zb_not(n824);
    let n826: ZB = zb_and(n823, n824);
    let n827: ZB = zb_and(n823, n825);
    let n828: ZB = zb_or(n826, n827);
    let n829: ZB = zb_and(n343, n824);
    let n830: ZB = zb_not(n829);
    let n831: ZB = zb_and(n828, n829);
    let n832: ZB = zb_and(n828, n830);
    let n833: ZB = zb_or(n831, n832);
    let n834: ZB = zb_and(n829, n833);
    let n835: ZB = zb_and(n830, n833);
    let n836: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n796);
    let n837: ZB = zb_not(n836);
    let n838: ZB = zb_and(n835, n836);
    let n839: ZB = zb_and(n835, n837);
    let n840: ZB = zb_and(n359, n838);
    let n841: ZB = zb_and(n358, n838);
    let n842: ZB = zb_or(n840, n841);
    let n843: ZB = zb_or(n839, n842);
    let n844: ZB = zb_and(n782, n836);
    let n845: ZB = zb_not(n844);
    let n846: ZB = zb_and(n843, n844);
    let n847: ZB = zb_and(n843, n845);
    let n848: ZB = zb_or(n846, n847);
    let n849: ZB = zb_and(n844, n848);
    let n850: ZB = zb_and(n845, n848);
    let n851: ZB = zb_or(n834, n849);
    let n852: ZB = zb_or(n822, n851);
    let n853: ZB = zb_or(n810, n852);
    let n854: ZB = zb_and(n447, n850);
    let n855: ZB = zb_and(n448, n850);
    let n856: ZN = zn_mget(g.cart, n731, n451);
    let n857: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n856);
    let n858: ZB = zb_not(n857);
    let n859: ZB = zb_and(n854, n857);
    let n860: ZB = zb_and(n854, n858);
    let n861: ZB = zb_and(n307, n859);
    let n862: ZB = zb_and(n306, n859);
    let n863: ZB = zb_or(n861, n862);
    let n864: ZB = zb_or(n860, n863);
    let n865: ZB = zb_and(n463, n857);
    let n866: ZB = zb_not(n865);
    let n867: ZB = zb_and(n864, n865);
    let n868: ZB = zb_and(n864, n866);
    let n869: ZB = zb_or(n867, n868);
    let n870: ZB = zb_and(n865, n869);
    let n871: ZB = zb_and(n866, n869);
    let n872: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n856);
    let n873: ZB = zb_not(n872);
    let n874: ZB = zb_and(n871, n872);
    let n875: ZB = zb_and(n871, n873);
    let n876: ZB = zb_or(n874, n875);
    let n877: ZB = zb_and(n328, n872);
    let n878: ZB = zb_not(n877);
    let n879: ZB = zb_and(n876, n877);
    let n880: ZB = zb_and(n876, n878);
    let n881: ZB = zb_or(n879, n880);
    let n882: ZB = zb_and(n877, n881);
    let n883: ZB = zb_and(n878, n881);
    let n884: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n856);
    let n885: ZB = zb_not(n884);
    let n886: ZB = zb_and(n883, n884);
    let n887: ZB = zb_and(n883, n885);
    let n888: ZB = zb_or(n886, n887);
    let n889: ZB = zb_and(n343, n884);
    let n890: ZB = zb_not(n889);
    let n891: ZB = zb_and(n888, n889);
    let n892: ZB = zb_and(n888, n890);
    let n893: ZB = zb_or(n891, n892);
    let n894: ZB = zb_and(n889, n893);
    let n895: ZB = zb_and(n890, n893);
    let n896: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n856);
    let n897: ZB = zb_not(n896);
    let n898: ZB = zb_and(n895, n896);
    let n899: ZB = zb_and(n895, n897);
    let n900: ZB = zb_and(n359, n898);
    let n901: ZB = zb_and(n358, n898);
    let n902: ZB = zb_or(n900, n901);
    let n903: ZB = zb_or(n899, n902);
    let n904: ZB = zb_and(n782, n896);
    let n905: ZB = zb_not(n904);
    let n906: ZB = zb_and(n903, n904);
    let n907: ZB = zb_and(n903, n905);
    let n908: ZB = zb_or(n906, n907);
    let n909: ZB = zb_and(n904, n908);
    let n910: ZB = zb_and(n905, n908);
    let n911: ZB = zb_or(n894, n909);
    let n912: ZB = zb_or(n882, n911);
    let n913: ZB = zb_or(n870, n912);
    let n914: ZB = zb_and(n515, n723);
    let n915: ZB = zb_or(n855, n910);
    let n916: ZB = zsel_b(n448, n723, n914);
    let n917: ZB = zb_or(n853, n913);
    let n918: ZB = zb_or(n795, n915);
    let n919: ZB = zsel_b(n380, n723, n916);
    let n920: ZB = zb_or(n793, n917);
    let n921: ZB = zb_or(n730, n918);
    let n922: ZB = zsel_b(n295, n723, n919);
    let n923: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n276);
    let n924: ZB = zn_gt(n923, n280);
    let n925: ZB = zb_and(n922, n924);
    let n926: ZB = zb_or(n721, n920);
    let n927: ZB = zsel_b(n721, n524, n723);
    let n928: ZB = zb_or(n728, n921);
    let n929: ZB = zsel_b(n726, n723, n925);
    let n930: ZB = zb_or(n522, n926);
    let n931: ZB = zsel_b(n522, n258, n927);
    let n932: ZB = zb_or(n529, n928);
    let n933: ZB = zsel_b(n527, n524, n929);
    let n934: ZB = zb_or(n285, n932);
    let n935: ZB = zsel_b(n283, n258, n933);
    let n936: ZB = zn_gt(n257, zn_splat(P8::from_raw(8388608i32)));
    let n937: ZB = zn_le(n257, zn_splat(P8::from_raw(8388608i32)));
    let n938: ZB = zb_and(n930, n936);
    let n939: ZB = zb_and(n930, n937);
    let n940: ZB = zb_or(n938, n939);
    let n941: ZB = zb_and(n934, n936);
    let n942: ZB = zb_or(n940, n941);
    let n943: ZB = zsel_b(n940, n931, n935);
    let n944: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n272);
    let n945: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n273);
    let n946: ZB = zn_tile_flag_at(g.cache, g.cart, n944, n945, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n947: ZB = zb_not(n946);
    let n948: ZB = zb_and(n942, n947);
    let n949: ZB = zb_and(n942, n946);
    let n950: ZB = zb_or(n948, n949);
    let n951: ZB = zb_and(n947, n950);
    let n952: ZB = zb_and(n946, n950);
    let n953: ZB = zb_or(n951, n952);
    let n954: ZB = zn_lt(r_c88, r_c88);
    let n955: ZB = zn_ge(r_c88, r_c88);
    let n956: ZN = zsel_n(n946, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n957: ZB = zb_and(n946, n953);
    let n958: ZB = zb_and(n947, n953);
    let n959: ZB = zb_and(n954, n957);
    let n960: ZB = zb_and(n955, n957);
    let n961: ZB = zb_or(n959, n960);
    let n962: ZB = zb_or(n958, n961);
    let n965: ZN = zsel_n(n947, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n966: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n965);
    let n967: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n965);
    let n968: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n273);
    let n969: ZB = zn_gt(n956, zn_splat(P8::from_raw(0i32)));
    let n970: ZB = zn_le(n956, zn_splat(P8::from_raw(0i32)));
    let n972: ZN = zn_add(n272, zn_splat(P8::from_raw(-196608i32)));
    let n973: ZB = zn_tile_flag_at(g.cache, g.cart, n972, n968, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n974: ZB = zb_not(n973);
    let n975: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n272);
    let n976: ZB = zn_tile_flag_at(g.cache, g.cart, n975, n968, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n977: ZB = zb_not(n976);
    let n978: ZN = zsel_n(n976, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n979: ZN = zsel_n(n973, zn_splat(P8::from_raw(-65536i32)), n978);
    let n980: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n979);
    let n981: ZB = zb_not(n980);
    let n983: ZB = zn_gt(r_c88, zn_splat(P8::from_raw(0i32)));
    let n984: ZB = zn_le(r_c88, zn_splat(P8::from_raw(0i32)));
    let n985: ZB = zb_and(n947, n962);
    let n986: ZB = zb_and(n946, n962);
    let n987: ZB = zb_or(n985, n986);
    let n988: ZB = zb_and(n947, n987);
    let n989: ZB = zb_and(n946, n987);
    let n990: ZB = zb_or(n988, n989);
    let n991: ZB = zb_and(n983, n990);
    let n992: ZB = zb_and(n984, n990);
    let n993: ZB = zb_or(n991, n992);
    let n994: ZB = zn_lt(n257, zn_splat(P8::from_raw(-262144i32)));
    let n995: ZB = zn_ge(n257, zn_splat(P8::from_raw(-262144i32)));
    let n996: ZB = zb_and(n993, n994);
    let n997: ZB = zb_and(n993, n995);
    let n998: ZB = zb_or(n996, n997);
    let n1000: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1002: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n966);
    let n1003: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n272);
    let n1004: ZB = zn_tile_flag_at(g.cache, g.cart, n1003, n968, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1005: ZB = zb_not(n1004);
    let n1008: ZB = zb_and(n987, n1005);
    let n1009: ZB = zb_and(n987, n1004);
    let n1010: ZB = zb_or(n1008, n1009);
    let n1011: ZB = zb_and(n1005, n1010);
    let n1012: ZB = zb_and(n1004, n1010);
    let n1013: ZB = zb_or(n1011, n1012);
    let n1014: ZB = zb_and(n1004, n1013);
    let n1015: ZB = zb_and(n1005, n1013);
    let n1016: ZB = zb_or(n1014, n1015);
    let n1017: ZB = zb_and(n1004, n1016);
    let n1018: ZB = zb_and(n1005, n1016);
    let n1019: ZB = zb_or(n1017, n1018);
    let n1020: ZB = zb_and(n947, n1019);
    let n1021: ZB = zb_and(n946, n1019);
    let n1022: ZB = zb_or(n1020, n1021);
    let n1023: ZB = zb_and(n983, n1022);
    let n1024: ZB = zb_and(n984, n1022);
    let n1025: ZB = zb_or(n1023, n1024);
    let n1026: ZB = zb_and(n994, n1025);
    let n1027: ZB = zb_and(n995, n1025);
    let n1028: ZB = zb_or(n1026, n1027);
    let n1031: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n967);
    let n1032: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n272);
    let n1033: ZB = zn_tile_flag_at(g.cache, g.cart, n1032, n968, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1034: ZB = zb_not(n1033);
    let n1035: ZB = zb_and(n987, n1034);
    let n1036: ZB = zb_and(n987, n1033);
    let n1037: ZB = zb_or(n1035, n1036);
    let n1038: ZB = zb_and(n1034, n1037);
    let n1039: ZB = zb_and(n1033, n1037);
    let n1040: ZB = zb_or(n1038, n1039);
    let n1041: ZB = zb_and(n1033, n1040);
    let n1042: ZB = zb_and(n1034, n1040);
    let n1043: ZB = zb_or(n1041, n1042);
    let n1044: ZB = zb_and(n1033, n1043);
    let n1045: ZB = zb_and(n1034, n1043);
    let n1046: ZB = zb_or(n1044, n1045);
    let n1047: ZB = zb_and(n947, n1046);
    let n1048: ZB = zb_and(n946, n1046);
    let n1049: ZB = zb_or(n1047, n1048);
    let n1050: ZB = zb_and(n983, n1049);
    let n1051: ZB = zb_and(n984, n1049);
    let n1052: ZB = zb_or(n1050, n1051);
    let n1053: ZB = zb_and(n994, n1052);
    let n1054: ZB = zb_and(n995, n1052);
    let n1055: ZB = zb_or(n1053, n1054);
    let n1058: ZB = zb_and(n969, n990);
    let n1059: ZB = zb_and(n970, n990);
    let n1060: ZB = zb_and(n974, n1059);
    let n1061: ZB = zb_and(n973, n1059);
    let n1062: ZB = zb_or(n1060, n1061);
    let n1063: ZB = zb_and(n974, n1062);
    let n1064: ZB = zb_and(n973, n1062);
    let n1065: ZB = zb_or(n1063, n1064);
    let n1066: ZB = zb_and(n973, n1065);
    let n1067: ZB = zb_and(n974, n1065);
    let n1068: ZB = zb_and(n977, n1067);
    let n1069: ZB = zb_and(n976, n1067);
    let n1070: ZB = zb_or(n1068, n1069);
    let n1071: ZB = zb_and(n977, n1070);
    let n1072: ZB = zb_and(n976, n1070);
    let n1073: ZB = zb_or(n1071, n1072);
    let n1074: ZB = zb_and(n976, n1073);
    let n1075: ZB = zb_and(n977, n1073);
    let n1076: ZB = zb_or(n1074, n1075);
    let n1077: ZB = zb_or(n1066, n1076);
    let n1078: ZB = zb_and(n981, n1077);
    let n1079: ZB = zb_and(n980, n1077);
    let n1080: ZB = zb_or(n1078, n1079);
    let n1081: ZB = zb_or(n1058, n1080);
    let n1082: ZB = zb_and(n983, n1081);
    let n1083: ZB = zb_and(n984, n1081);
    let n1084: ZB = zb_or(n1082, n1083);
    let n1085: ZB = zb_and(n994, n1084);
    let n1086: ZB = zb_and(n995, n1084);
    let n1087: ZB = zb_or(n1085, n1086);
    let n1090: ZB = zb_and(n969, n1022);
    let n1091: ZB = zb_and(n970, n1022);
    let n1092: ZB = zb_and(n974, n1091);
    let n1093: ZB = zb_and(n973, n1091);
    let n1094: ZB = zb_or(n1092, n1093);
    let n1095: ZB = zb_and(n974, n1094);
    let n1096: ZB = zb_and(n973, n1094);
    let n1097: ZB = zb_or(n1095, n1096);
    let n1098: ZB = zb_and(n973, n1097);
    let n1099: ZB = zb_and(n974, n1097);
    let n1100: ZB = zb_and(n977, n1099);
    let n1101: ZB = zb_and(n976, n1099);
    let n1102: ZB = zb_or(n1100, n1101);
    let n1103: ZB = zb_and(n977, n1102);
    let n1104: ZB = zb_and(n976, n1102);
    let n1105: ZB = zb_or(n1103, n1104);
    let n1106: ZB = zb_and(n976, n1105);
    let n1107: ZB = zb_and(n977, n1105);
    let n1108: ZB = zb_or(n1106, n1107);
    let n1109: ZB = zb_or(n1098, n1108);
    let n1110: ZB = zb_and(n981, n1109);
    let n1111: ZB = zb_and(n980, n1109);
    let n1112: ZB = zb_or(n1110, n1111);
    let n1113: ZB = zb_or(n1090, n1112);
    let n1114: ZB = zb_and(n983, n1113);
    let n1115: ZB = zb_and(n984, n1113);
    let n1116: ZB = zb_or(n1114, n1115);
    let n1117: ZB = zb_and(n994, n1116);
    let n1118: ZB = zb_and(n995, n1116);
    let n1119: ZB = zb_or(n1117, n1118);
    let n1122: ZB = zb_and(n969, n1049);
    let n1123: ZB = zb_and(n970, n1049);
    let n1124: ZB = zb_and(n974, n1123);
    let n1125: ZB = zb_and(n973, n1123);
    let n1126: ZB = zb_or(n1124, n1125);
    let n1127: ZB = zb_and(n974, n1126);
    let n1128: ZB = zb_and(n973, n1126);
    let n1129: ZB = zb_or(n1127, n1128);
    let n1130: ZB = zb_and(n973, n1129);
    let n1131: ZB = zb_and(n974, n1129);
    let n1132: ZB = zb_and(n977, n1131);
    let n1133: ZB = zb_and(n976, n1131);
    let n1134: ZB = zb_or(n1132, n1133);
    let n1135: ZB = zb_and(n977, n1134);
    let n1136: ZB = zb_and(n976, n1134);
    let n1137: ZB = zb_or(n1135, n1136);
    let n1138: ZB = zb_and(n976, n1137);
    let n1139: ZB = zb_and(n977, n1137);
    let n1140: ZB = zb_or(n1138, n1139);
    let n1141: ZB = zb_or(n1130, n1140);
    let n1142: ZB = zb_and(n981, n1141);
    let n1143: ZB = zb_and(n980, n1141);
    let n1144: ZB = zb_or(n1142, n1143);
    let n1145: ZB = zb_or(n1122, n1144);
    let n1146: ZB = zb_and(n983, n1145);
    let n1147: ZB = zb_and(n984, n1145);
    let n1148: ZB = zb_or(n1146, n1147);
    let n1149: ZB = zb_and(n994, n1148);
    let n1150: ZB = zb_and(n995, n1148);
    let n1151: ZB = zb_or(n1149, n1150);
    let n1154: ZN = zsel_n(n983, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1155: ZB = zb_or(n66, n983);
    let n1156: ZB = zb_and(n983, n993);
    let n1157: ZB = zb_and(n984, n993);
    let n1158: ZB = zb_or(n1156, n1157);
    let n1159: ZB = zb_and(n994, n1158);
    let n1160: ZB = zb_and(n995, n1158);
    let n1161: ZB = zb_or(n1159, n1160);
    let n1162: ZB = zb_and(n995, n1161);
    let n1163: ZB = zn_gt(n1154, zn_splat(P8::from_raw(0i32)));
    let n1164: ZB = zn_le(n1154, zn_splat(P8::from_raw(0i32)));
    let n1165: ZB = zb_and(n1162, n1163);
    let n1166: ZB = zb_and(n1162, n1164);
    let n1167: ZB = zb_or(n1165, n1166);
    let n1168: ZB = zb_and(n983, n1025);
    let n1169: ZB = zb_and(n984, n1025);
    let n1170: ZB = zb_or(n1168, n1169);
    let n1171: ZB = zb_and(n994, n1170);
    let n1172: ZB = zb_and(n995, n1170);
    let n1173: ZB = zb_or(n1171, n1172);
    let n1174: ZB = zb_and(n995, n1173);
    let n1175: ZB = zb_and(n1163, n1174);
    let n1176: ZB = zb_and(n1164, n1174);
    let n1177: ZB = zb_or(n1175, n1176);
    let n1178: ZB = zb_and(n983, n1052);
    let n1179: ZB = zb_and(n984, n1052);
    let n1180: ZB = zb_or(n1178, n1179);
    let n1181: ZB = zb_and(n994, n1180);
    let n1182: ZB = zb_and(n995, n1180);
    let n1183: ZB = zb_or(n1181, n1182);
    let n1184: ZB = zb_and(n995, n1183);
    let n1185: ZB = zb_and(n1163, n1184);
    let n1186: ZB = zb_and(n1164, n1184);
    let n1187: ZB = zb_or(n1185, n1186);
    let n1188: ZB = zb_and(n983, n1084);
    let n1189: ZB = zb_and(n984, n1084);
    let n1190: ZB = zb_or(n1188, n1189);
    let n1191: ZB = zb_and(n994, n1190);
    let n1192: ZB = zb_and(n995, n1190);
    let n1193: ZB = zb_or(n1191, n1192);
    let n1194: ZB = zb_and(n995, n1193);
    let n1195: ZB = zb_and(n1163, n1194);
    let n1196: ZB = zb_and(n1164, n1194);
    let n1197: ZB = zb_or(n1195, n1196);
    let n1198: ZB = zb_and(n983, n1116);
    let n1199: ZB = zb_and(n984, n1116);
    let n1200: ZB = zb_or(n1198, n1199);
    let n1201: ZB = zb_and(n994, n1200);
    let n1202: ZB = zb_and(n995, n1200);
    let n1203: ZB = zb_or(n1201, n1202);
    let n1204: ZB = zb_and(n995, n1203);
    let n1205: ZB = zb_and(n1163, n1204);
    let n1206: ZB = zb_and(n1164, n1204);
    let n1207: ZB = zb_or(n1205, n1206);
    let n1208: ZB = zb_and(n983, n1148);
    let n1209: ZB = zb_and(n984, n1148);
    let n1210: ZB = zb_or(n1208, n1209);
    let n1211: ZB = zb_and(n994, n1210);
    let n1212: ZB = zb_and(n995, n1210);
    let n1213: ZB = zb_or(n1211, n1212);
    let n1214: ZB = zb_and(n995, n1213);
    let n1215: ZB = zb_and(n1163, n1214);
    let n1216: ZB = zb_and(n1164, n1214);
    let n1217: ZB = zb_or(n1215, n1216);
    let n1222: ZB = zb_and(r_c38, n50);
    let n1223: ZN = zsel_n(n47, n48, r_c39);
    let n1224: ZB = zsel_b(n47, n1222, r_c38);
    let n1225: ZB = zb_and(n934, n937);
    let n1226: ZB = zb_and(n947, n1225);
    let n1227: ZB = zb_and(n946, n1225);
    let n1228: ZB = zb_or(n1226, n1227);
    let n1229: ZB = zb_and(n947, n1228);
    let n1230: ZB = zb_and(n946, n1228);
    let n1231: ZB = zb_or(n1229, n1230);
    let n1232: ZB = zb_and(n946, n1231);
    let n1233: ZB = zb_and(n947, n1231);
    let n1234: ZB = zb_and(n954, n1232);
    let n1235: ZB = zb_and(n955, n1232);
    let n1236: ZB = zb_or(n1234, n1235);
    let n1237: ZB = zb_or(n1233, n1236);
    let n1238: ZB = zb_and(n947, n1237);
    let n1239: ZB = zb_and(n946, n1237);
    let n1240: ZB = zb_or(n1238, n1239);
    let n1241: ZB = zb_and(n947, n1240);
    let n1242: ZB = zb_and(n946, n1240);
    let n1243: ZB = zb_or(n1241, n1242);
    let n1244: ZB = zb_and(n983, n1243);
    let n1245: ZB = zb_and(n984, n1243);
    let n1246: ZB = zb_or(n1244, n1245);
    let n1247: ZB = zb_and(n994, n1246);
    let n1248: ZB = zb_and(n995, n1246);
    let n1249: ZB = zb_or(n1247, n1248);
    let n1250: ZB = zb_and(n994, n1249);
    let n1251: ZB = zb_and(n994, n998);
    let n1252: ZN = zsel_n(n1250, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1253: ZB = zb_not(n1250);
    let n1254: ZB = zb_or(n1224, n1253);
    let n1255: ZB = zb_or(n1250, n1251);
    let n1256: ZB = zsel_b(n1250, n935, n943);
    let n1261: ZB = zb_and(n1005, n1240);
    let n1262: ZB = zb_and(n1004, n1240);
    let n1263: ZB = zb_or(n1261, n1262);
    let n1264: ZB = zb_and(n1005, n1263);
    let n1265: ZB = zb_and(n1004, n1263);
    let n1266: ZB = zb_or(n1264, n1265);
    let n1267: ZB = zb_and(n1004, n1266);
    let n1268: ZB = zb_and(n1005, n1266);
    let n1269: ZB = zb_or(n1267, n1268);
    let n1270: ZB = zb_and(n1004, n1269);
    let n1271: ZB = zb_and(n1005, n1269);
    let n1272: ZB = zb_or(n1270, n1271);
    let n1273: ZB = zb_and(n947, n1272);
    let n1274: ZB = zb_and(n946, n1272);
    let n1275: ZB = zb_or(n1273, n1274);
    let n1276: ZB = zb_and(n983, n1275);
    let n1277: ZB = zb_and(n984, n1275);
    let n1278: ZB = zb_or(n1276, n1277);
    let n1279: ZB = zb_and(n994, n1278);
    let n1280: ZB = zb_and(n995, n1278);
    let n1281: ZB = zb_or(n1279, n1280);
    let n1282: ZB = zb_and(n994, n1281);
    let n1283: ZB = zb_and(n994, n1028);
    let n1284: ZN = zsel_n(n1282, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1285: ZB = zb_not(n1282);
    let n1286: ZB = zb_or(n1224, n1285);
    let n1287: ZB = zb_or(n1282, n1283);
    let n1288: ZB = zsel_b(n1282, n935, n943);
    let n1290: ZB = zb_and(n1034, n1240);
    let n1291: ZB = zb_and(n1033, n1240);
    let n1292: ZB = zb_or(n1290, n1291);
    let n1293: ZB = zb_and(n1034, n1292);
    let n1294: ZB = zb_and(n1033, n1292);
    let n1295: ZB = zb_or(n1293, n1294);
    let n1296: ZB = zb_and(n1033, n1295);
    let n1297: ZB = zb_and(n1034, n1295);
    let n1298: ZB = zb_or(n1296, n1297);
    let n1299: ZB = zb_and(n1033, n1298);
    let n1300: ZB = zb_and(n1034, n1298);
    let n1301: ZB = zb_or(n1299, n1300);
    let n1302: ZB = zb_and(n947, n1301);
    let n1303: ZB = zb_and(n946, n1301);
    let n1304: ZB = zb_or(n1302, n1303);
    let n1305: ZB = zb_and(n983, n1304);
    let n1306: ZB = zb_and(n984, n1304);
    let n1307: ZB = zb_or(n1305, n1306);
    let n1308: ZB = zb_and(n994, n1307);
    let n1309: ZB = zb_and(n995, n1307);
    let n1310: ZB = zb_or(n1308, n1309);
    let n1311: ZB = zb_and(n994, n1310);
    let n1312: ZB = zb_and(n994, n1055);
    let n1313: ZN = zsel_n(n1311, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1314: ZB = zb_not(n1311);
    let n1315: ZB = zb_or(n1224, n1314);
    let n1316: ZB = zb_or(n1311, n1312);
    let n1317: ZB = zsel_b(n1311, n935, n943);
    let n1319: ZB = zb_and(n969, n1243);
    let n1320: ZB = zb_and(n970, n1243);
    let n1321: ZB = zb_and(n974, n1320);
    let n1322: ZB = zb_and(n973, n1320);
    let n1323: ZB = zb_or(n1321, n1322);
    let n1324: ZB = zb_and(n974, n1323);
    let n1325: ZB = zb_and(n973, n1323);
    let n1326: ZB = zb_or(n1324, n1325);
    let n1327: ZB = zb_and(n973, n1326);
    let n1328: ZB = zb_and(n974, n1326);
    let n1329: ZB = zb_and(n977, n1328);
    let n1330: ZB = zb_and(n976, n1328);
    let n1331: ZB = zb_or(n1329, n1330);
    let n1332: ZB = zb_and(n977, n1331);
    let n1333: ZB = zb_and(n976, n1331);
    let n1334: ZB = zb_or(n1332, n1333);
    let n1335: ZB = zb_and(n976, n1334);
    let n1336: ZB = zb_and(n977, n1334);
    let n1337: ZB = zb_or(n1335, n1336);
    let n1338: ZB = zb_or(n1327, n1337);
    let n1339: ZB = zb_and(n981, n1338);
    let n1340: ZB = zb_and(n980, n1338);
    let n1341: ZB = zb_or(n1339, n1340);
    let n1342: ZB = zb_or(n1319, n1341);
    let n1343: ZB = zb_and(n983, n1342);
    let n1344: ZB = zb_and(n984, n1342);
    let n1345: ZB = zb_or(n1343, n1344);
    let n1346: ZB = zb_and(n994, n1345);
    let n1347: ZB = zb_and(n995, n1345);
    let n1348: ZB = zb_or(n1346, n1347);
    let n1349: ZB = zb_and(n994, n1348);
    let n1350: ZB = zb_and(n994, n1087);
    let n1351: ZN = zsel_n(n1349, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1352: ZB = zb_not(n1349);
    let n1353: ZB = zb_or(n1224, n1352);
    let n1354: ZB = zb_or(n1349, n1350);
    let n1355: ZB = zsel_b(n1349, n935, n943);
    let n1357: ZB = zb_and(n969, n1275);
    let n1358: ZB = zb_and(n970, n1275);
    let n1359: ZB = zb_and(n974, n1358);
    let n1360: ZB = zb_and(n973, n1358);
    let n1361: ZB = zb_or(n1359, n1360);
    let n1362: ZB = zb_and(n974, n1361);
    let n1363: ZB = zb_and(n973, n1361);
    let n1364: ZB = zb_or(n1362, n1363);
    let n1365: ZB = zb_and(n973, n1364);
    let n1366: ZB = zb_and(n974, n1364);
    let n1367: ZB = zb_and(n977, n1366);
    let n1368: ZB = zb_and(n976, n1366);
    let n1369: ZB = zb_or(n1367, n1368);
    let n1370: ZB = zb_and(n977, n1369);
    let n1371: ZB = zb_and(n976, n1369);
    let n1372: ZB = zb_or(n1370, n1371);
    let n1373: ZB = zb_and(n976, n1372);
    let n1374: ZB = zb_and(n977, n1372);
    let n1375: ZB = zb_or(n1373, n1374);
    let n1376: ZB = zb_or(n1365, n1375);
    let n1377: ZB = zb_and(n981, n1376);
    let n1378: ZB = zb_and(n980, n1376);
    let n1379: ZB = zb_or(n1377, n1378);
    let n1380: ZB = zb_or(n1357, n1379);
    let n1381: ZB = zb_and(n983, n1380);
    let n1382: ZB = zb_and(n984, n1380);
    let n1383: ZB = zb_or(n1381, n1382);
    let n1384: ZB = zb_and(n994, n1383);
    let n1385: ZB = zb_and(n995, n1383);
    let n1386: ZB = zb_or(n1384, n1385);
    let n1387: ZB = zb_and(n994, n1386);
    let n1388: ZB = zb_and(n994, n1119);
    let n1389: ZN = zsel_n(n1387, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1390: ZB = zb_not(n1387);
    let n1391: ZB = zb_or(n1224, n1390);
    let n1392: ZB = zb_or(n1387, n1388);
    let n1393: ZB = zsel_b(n1387, n935, n943);
    let n1395: ZB = zb_and(n969, n1304);
    let n1396: ZB = zb_and(n970, n1304);
    let n1397: ZB = zb_and(n974, n1396);
    let n1398: ZB = zb_and(n973, n1396);
    let n1399: ZB = zb_or(n1397, n1398);
    let n1400: ZB = zb_and(n974, n1399);
    let n1401: ZB = zb_and(n973, n1399);
    let n1402: ZB = zb_or(n1400, n1401);
    let n1403: ZB = zb_and(n973, n1402);
    let n1404: ZB = zb_and(n974, n1402);
    let n1405: ZB = zb_and(n977, n1404);
    let n1406: ZB = zb_and(n976, n1404);
    let n1407: ZB = zb_or(n1405, n1406);
    let n1408: ZB = zb_and(n977, n1407);
    let n1409: ZB = zb_and(n976, n1407);
    let n1410: ZB = zb_or(n1408, n1409);
    let n1411: ZB = zb_and(n976, n1410);
    let n1412: ZB = zb_and(n977, n1410);
    let n1413: ZB = zb_or(n1411, n1412);
    let n1414: ZB = zb_or(n1403, n1413);
    let n1415: ZB = zb_and(n981, n1414);
    let n1416: ZB = zb_and(n980, n1414);
    let n1417: ZB = zb_or(n1415, n1416);
    let n1418: ZB = zb_or(n1395, n1417);
    let n1419: ZB = zb_and(n983, n1418);
    let n1420: ZB = zb_and(n984, n1418);
    let n1421: ZB = zb_or(n1419, n1420);
    let n1422: ZB = zb_and(n994, n1421);
    let n1423: ZB = zb_and(n995, n1421);
    let n1424: ZB = zb_or(n1422, n1423);
    let n1425: ZB = zb_and(n994, n1424);
    let n1426: ZB = zb_and(n994, n1151);
    let n1427: ZN = zsel_n(n1425, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1428: ZB = zb_not(n1425);
    let n1429: ZB = zb_or(n1224, n1428);
    let n1430: ZB = zb_or(n1425, n1426);
    let n1431: ZB = zsel_b(n1425, n935, n943);
    let n1433: ZB = zb_and(n983, n1246);
    let n1434: ZB = zb_and(n984, n1246);
    let n1435: ZB = zb_or(n1433, n1434);
    let n1436: ZB = zb_and(n994, n1435);
    let n1437: ZB = zb_and(n995, n1435);
    let n1438: ZB = zb_or(n1436, n1437);
    let n1439: ZB = zb_and(n994, n1438);
    let n1440: ZB = zb_and(n994, n1161);
    let n1441: ZN = zsel_n(n1439, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1442: ZB = zb_not(n1439);
    let n1443: ZB = zb_or(n1224, n1442);
    let n1444: ZB = zb_or(n1439, n1440);
    let n1445: ZB = zsel_b(n1439, n935, n943);
    let n1446: ZB = zb_and(n1163, n1444);
    let n1447: ZB = zb_and(n1164, n1444);
    let n1448: ZB = zb_or(n1446, n1447);
    let n1449: ZB = zb_and(n983, n1278);
    let n1450: ZB = zb_and(n984, n1278);
    let n1451: ZB = zb_or(n1449, n1450);
    let n1452: ZB = zb_and(n994, n1451);
    let n1453: ZB = zb_and(n995, n1451);
    let n1454: ZB = zb_or(n1452, n1453);
    let n1455: ZB = zb_and(n994, n1454);
    let n1456: ZB = zb_and(n994, n1173);
    let n1457: ZN = zsel_n(n1455, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1458: ZB = zb_not(n1455);
    let n1459: ZB = zb_or(n1224, n1458);
    let n1460: ZB = zb_or(n1455, n1456);
    let n1461: ZB = zsel_b(n1455, n935, n943);
    let n1462: ZB = zb_and(n1163, n1460);
    let n1463: ZB = zb_and(n1164, n1460);
    let n1464: ZB = zb_or(n1462, n1463);
    let n1465: ZB = zb_and(n983, n1307);
    let n1466: ZB = zb_and(n984, n1307);
    let n1467: ZB = zb_or(n1465, n1466);
    let n1468: ZB = zb_and(n994, n1467);
    let n1469: ZB = zb_and(n995, n1467);
    let n1470: ZB = zb_or(n1468, n1469);
    let n1471: ZB = zb_and(n994, n1470);
    let n1472: ZB = zb_and(n994, n1183);
    let n1473: ZN = zsel_n(n1471, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1474: ZB = zb_not(n1471);
    let n1475: ZB = zb_or(n1224, n1474);
    let n1476: ZB = zb_or(n1471, n1472);
    let n1477: ZB = zsel_b(n1471, n935, n943);
    let n1478: ZB = zb_and(n1163, n1476);
    let n1479: ZB = zb_and(n1164, n1476);
    let n1480: ZB = zb_or(n1478, n1479);
    let n1481: ZB = zb_and(n983, n1345);
    let n1482: ZB = zb_and(n984, n1345);
    let n1483: ZB = zb_or(n1481, n1482);
    let n1484: ZB = zb_and(n994, n1483);
    let n1485: ZB = zb_and(n995, n1483);
    let n1486: ZB = zb_or(n1484, n1485);
    let n1487: ZB = zb_and(n994, n1486);
    let n1488: ZB = zb_and(n994, n1193);
    let n1489: ZN = zsel_n(n1487, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1490: ZB = zb_not(n1487);
    let n1491: ZB = zb_or(n1224, n1490);
    let n1492: ZB = zb_or(n1487, n1488);
    let n1493: ZB = zsel_b(n1487, n935, n943);
    let n1494: ZB = zb_and(n1163, n1492);
    let n1495: ZB = zb_and(n1164, n1492);
    let n1496: ZB = zb_or(n1494, n1495);
    let n1497: ZB = zb_and(n983, n1383);
    let n1498: ZB = zb_and(n984, n1383);
    let n1499: ZB = zb_or(n1497, n1498);
    let n1500: ZB = zb_and(n994, n1499);
    let n1501: ZB = zb_and(n995, n1499);
    let n1502: ZB = zb_or(n1500, n1501);
    let n1503: ZB = zb_and(n994, n1502);
    let n1504: ZB = zb_and(n994, n1203);
    let n1505: ZN = zsel_n(n1503, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1506: ZB = zb_not(n1503);
    let n1507: ZB = zb_or(n1224, n1506);
    let n1508: ZB = zb_or(n1503, n1504);
    let n1509: ZB = zsel_b(n1503, n935, n943);
    let n1510: ZB = zb_and(n1163, n1508);
    let n1511: ZB = zb_and(n1164, n1508);
    let n1512: ZB = zb_or(n1510, n1511);
    let n1513: ZB = zb_and(n983, n1421);
    let n1514: ZB = zb_and(n984, n1421);
    let n1515: ZB = zb_or(n1513, n1514);
    let n1516: ZB = zb_and(n994, n1515);
    let n1517: ZB = zb_and(n995, n1515);
    let n1518: ZB = zb_or(n1516, n1517);
    let n1519: ZB = zb_and(n994, n1518);
    let n1520: ZB = zb_and(n994, n1213);
    let n1521: ZN = zsel_n(n1519, n1223, zn_splat(P8::from_raw(983040i32)));
    let n1522: ZB = zb_not(n1519);
    let n1523: ZB = zb_or(n1224, n1522);
    let n1524: ZB = zb_or(n1519, n1520);
    let n1525: ZB = zsel_b(n1519, n935, n943);
    let n1526: ZB = zb_and(n1163, n1524);
    let n1527: ZB = zb_and(n1164, n1524);
    let n1528: ZB = zb_or(n1526, n1527);
    let n1530: ZB = zb_and(r_c43, n269);
    let n1532: ZN = zsel_n(n947, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n1534: ZN = zn_neg(n979);
    let n1535: ZN = zn_mul(n1534, zn_splat(P8::from_raw(131072i32)));
    let n1536: ZN = zsel_n(n981, n1535, zn_splat(P8::from_raw(0i32)));
    let n1537: ZN = zsel_n(n981, zn_splat(P8::from_raw(-131072i32)), n1532);
    let n1538: ZN = zsel_n(n969, zn_splat(P8::from_raw(0i32)), n956);
    let n1539: ZN = zsel_n(n969, zn_splat(P8::from_raw(0i32)), n1536);
    let n1540: ZN = zsel_n(n969, zn_splat(P8::from_raw(-131072i32)), n1537);
    let n1541: ZN = zn_sub(r_c88, zn_splat(P8::from_raw(65536i32)));
    let n1544: ZB = zb_and(n995, n1249);
    let n1545: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n956);
    let n1546: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1532);
    let n1547: ZB = zb_or(n1530, n1544);
    let n1548: ZB = zsel_b(r_c43, n258, n935);
    let n1549: ZB = zb_and(n45, n1547);
    let n1550: ZB = zn_lt(n256, zn_splat(P8::from_raw(-65536i32)));
    let n1551: ZB = zn_ge(n256, zn_splat(P8::from_raw(-65536i32)));
    let n1552: ZB = zb_and(n1549, n1551);
    let n1553: ZB = zb_and(n1549, n1550);
    let n1555: ZB = zn_gt(n256, zn_splat(P8::from_raw(7929856i32)));
    let n1556: ZB = zb_or(n1552, n1553);
    let n1557: ZB = zb_or(n1550, n1555);
    let n1558: ZB = zb_not(n1557);
    let n1559: ZB = zb_and(n1556, n1557);
    let n1560: ZB = zb_and(n1556, n1558);
    let n1561: ZN = zn_min(n256, zn_splat(P8::from_raw(7929856i32)));
    let n1562: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1561);
    let n1563: ZN = zsel_n(n1557, n1562, n256);
    let n1564: ZB = zb_or(n1559, n1560);
    let n1565: ZN = zsel_n(n1000, n256, n1563);
    let n1567: ZN = zsel_n(n981, n1535, n1002);
    let n1568: ZN = zsel_n(n969, n1002, n1567);
    let n1569: ZB = zb_and(n995, n1281);
    let n1570: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1002);
    let n1571: ZB = zb_or(n1530, n1569);
    let n1572: ZB = zb_and(n45, n1571);
    let n1573: ZB = zb_and(n1551, n1572);
    let n1574: ZB = zb_and(n1550, n1572);
    let n1575: ZB = zb_or(n1573, n1574);
    let n1576: ZB = zb_and(n1557, n1575);
    let n1577: ZB = zb_and(n1558, n1575);
    let n1578: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1570);
    let n1579: ZB = zb_or(n1576, n1577);
    let n1580: ZN = zsel_n(n1000, n1570, n1578);
    let n1581: ZN = zsel_n(n981, n1535, n1031);
    let n1582: ZN = zsel_n(n969, n1031, n1581);
    let n1583: ZB = zb_and(n995, n1310);
    let n1584: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1031);
    let n1585: ZB = zb_or(n1530, n1583);
    let n1586: ZB = zb_and(n45, n1585);
    let n1587: ZB = zb_and(n1551, n1586);
    let n1588: ZB = zb_and(n1550, n1586);
    let n1589: ZB = zb_or(n1587, n1588);
    let n1590: ZB = zb_and(n1557, n1589);
    let n1591: ZB = zb_and(n1558, n1589);
    let n1592: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1584);
    let n1593: ZB = zb_or(n1590, n1591);
    let n1594: ZN = zsel_n(n1000, n1584, n1592);
    let n1595: ZB = zb_and(n995, n1348);
    let n1596: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1538);
    let n1597: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1539);
    let n1598: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1540);
    let n1599: ZB = zb_or(n1530, n1595);
    let n1600: ZB = zb_and(n45, n1599);
    let n1601: ZB = zb_and(n1551, n1600);
    let n1602: ZB = zb_and(n1550, n1600);
    let n1603: ZB = zb_or(n1601, n1602);
    let n1604: ZB = zb_and(n1557, n1603);
    let n1605: ZB = zb_and(n1558, n1603);
    let n1606: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1597);
    let n1607: ZB = zb_or(n1604, n1605);
    let n1608: ZN = zsel_n(n1000, n1597, n1606);
    let n1609: ZB = zb_and(n995, n1386);
    let n1610: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1568);
    let n1611: ZB = zb_or(n1530, n1609);
    let n1612: ZB = zb_and(n45, n1611);
    let n1613: ZB = zb_and(n1551, n1612);
    let n1614: ZB = zb_and(n1550, n1612);
    let n1615: ZB = zb_or(n1613, n1614);
    let n1616: ZB = zb_and(n1557, n1615);
    let n1617: ZB = zb_and(n1558, n1615);
    let n1618: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1610);
    let n1619: ZB = zb_or(n1616, n1617);
    let n1620: ZN = zsel_n(n1000, n1610, n1618);
    let n1621: ZB = zb_and(n995, n1424);
    let n1622: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1582);
    let n1623: ZB = zb_or(n1530, n1621);
    let n1624: ZB = zb_and(n45, n1623);
    let n1625: ZB = zb_and(n1551, n1624);
    let n1626: ZB = zb_and(n1550, n1624);
    let n1627: ZB = zb_or(n1625, n1626);
    let n1628: ZB = zb_and(n1557, n1627);
    let n1629: ZB = zb_and(n1558, n1627);
    let n1630: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1622);
    let n1631: ZB = zb_or(n1628, n1629);
    let n1632: ZN = zsel_n(n1000, n1622, n1630);
    let n1633: ZN = zsel_n(n983, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n1634: ZN = zsel_n(n983, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n1635: ZN = zsel_n(n983, n1541, r_c88);
    let n1636: ZN = zsel_n(n983, zn_splat(P8::from_raw(98304i32)), zn_splat(P8::from_raw(0i32)));
    let n1637: ZN = zsel_n(n983, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(0i32)));
    let n1638: ZN = zsel_n(n983, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1639: ZN = zsel_n(n983, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1640: ZN = zsel_n(n983, zn_splat(P8::from_raw(0i32)), n1532);
    let n1641: ZB = zb_and(n995, n1438);
    let n1642: ZN = zsel_n(r_c43, r_c20, n1154);
    let n1643: ZB = zsel_b(r_c43, n66, n1155);
    let n1644: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1633);
    let n1645: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1634);
    let n1646: ZN = zsel_n(r_c43, r_c88, n1635);
    let n1647: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1636);
    let n1648: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1637);
    let n1649: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1638);
    let n1650: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1639);
    let n1651: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1640);
    let n1652: ZB = zb_or(n1530, n1641);
    let n1653: ZB = zn_gt(n1642, zn_splat(P8::from_raw(0i32)));
    let n1654: ZB = zn_le(n1642, zn_splat(P8::from_raw(0i32)));
    let n1655: ZB = zb_and(n1652, n1653);
    let n1656: ZB = zb_and(n1652, n1654);
    let n1657: ZB = zb_and(n1551, n1656);
    let n1658: ZB = zb_and(n1550, n1656);
    let n1659: ZB = zb_or(n1657, n1658);
    let n1660: ZB = zb_and(n1557, n1659);
    let n1661: ZB = zb_and(n1558, n1659);
    let n1662: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1650);
    let n1663: ZB = zb_or(n1660, n1661);
    let n1664: ZN = zsel_n(n1653, n256, n1563);
    let n1665: ZN = zsel_n(n1653, n1650, n1662);
    let n1666: ZB = zb_or(n1655, n1663);
    let n1667: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1644);
    let n1668: ZN = zsel_n(n983, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1669: ZN = zsel_n(n983, zn_splat(P8::from_raw(-327680i32)), n1002);
    let n1670: ZB = zb_and(n995, n1454);
    let n1671: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1668);
    let n1672: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1669);
    let n1673: ZB = zb_or(n1530, n1670);
    let n1674: ZB = zb_and(n1653, n1673);
    let n1675: ZB = zb_and(n1654, n1673);
    let n1676: ZB = zb_and(n1551, n1675);
    let n1677: ZB = zb_and(n1550, n1675);
    let n1678: ZB = zb_or(n1676, n1677);
    let n1679: ZB = zb_and(n1557, n1678);
    let n1680: ZB = zb_and(n1558, n1678);
    let n1681: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1672);
    let n1682: ZB = zb_or(n1679, n1680);
    let n1683: ZN = zsel_n(n1653, n1672, n1681);
    let n1684: ZB = zb_or(n1674, n1682);
    let n1685: ZN = zsel_n(n983, zn_splat(P8::from_raw(327680i32)), n1031);
    let n1686: ZB = zb_and(n995, n1470);
    let n1687: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1685);
    let n1688: ZB = zb_or(n1530, n1686);
    let n1689: ZB = zb_and(n1653, n1688);
    let n1690: ZB = zb_and(n1654, n1688);
    let n1691: ZB = zb_and(n1551, n1690);
    let n1692: ZB = zb_and(n1550, n1690);
    let n1693: ZB = zb_or(n1691, n1692);
    let n1694: ZB = zb_and(n1557, n1693);
    let n1695: ZB = zb_and(n1558, n1693);
    let n1696: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1687);
    let n1697: ZB = zb_or(n1694, n1695);
    let n1698: ZN = zsel_n(n1653, n1687, n1696);
    let n1699: ZB = zb_or(n1689, n1697);
    let n1701: ZN = zsel_n(n983, zn_splat(P8::from_raw(-98304i32)), zn_splat(P8::from_raw(0i32)));
    let n1702: ZN = zsel_n(n983, zn_splat(P8::from_raw(-327680i32)), n1532);
    let n1703: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1701);
    let n1704: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1702);
    let n1705: ZN = zsel_n(n983, zn_splat(P8::from_raw(-231700i32)), n1002);
    let n1706: ZN = zsel_n(n983, zn_splat(P8::from_raw(-231700i32)), n1532);
    let n1707: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1705);
    let n1708: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1706);
    let n1709: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1707);
    let n1710: ZN = zsel_n(n1653, n1707, n1709);
    let n1711: ZN = zsel_n(n983, zn_splat(P8::from_raw(231700i32)), n1031);
    let n1712: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1711);
    let n1713: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1712);
    let n1714: ZN = zsel_n(n1653, n1712, n1713);
    let n1715: ZN = zsel_n(n983, zn_splat(P8::from_raw(327680i32)), n1532);
    let n1716: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1715);
    let n1717: ZN = zsel_n(n983, zn_splat(P8::from_raw(231700i32)), n1532);
    let n1718: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1717);
    let n1719: ZN = zsel_n(n983, zn_splat(P8::from_raw(65536i32)), n1539);
    let n1720: ZN = zsel_n(n983, zn_splat(P8::from_raw(0i32)), n1540);
    let n1721: ZB = zb_and(n995, n1486);
    let n1722: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1719);
    let n1723: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1720);
    let n1724: ZB = zb_or(n1530, n1721);
    let n1725: ZB = zb_and(n1653, n1724);
    let n1726: ZB = zb_and(n1654, n1724);
    let n1727: ZB = zb_and(n1551, n1726);
    let n1728: ZB = zb_and(n1550, n1726);
    let n1729: ZB = zb_or(n1727, n1728);
    let n1730: ZB = zb_and(n1557, n1729);
    let n1731: ZB = zb_and(n1558, n1729);
    let n1732: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1722);
    let n1733: ZB = zb_or(n1730, n1731);
    let n1734: ZN = zsel_n(n1653, n1722, n1732);
    let n1735: ZB = zb_or(n1725, n1733);
    let n1736: ZN = zsel_n(n983, zn_splat(P8::from_raw(-327680i32)), n1568);
    let n1737: ZB = zb_and(n995, n1502);
    let n1738: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1736);
    let n1739: ZB = zb_or(n1530, n1737);
    let n1740: ZB = zb_and(n1653, n1739);
    let n1741: ZB = zb_and(n1654, n1739);
    let n1742: ZB = zb_and(n1551, n1741);
    let n1743: ZB = zb_and(n1550, n1741);
    let n1744: ZB = zb_or(n1742, n1743);
    let n1745: ZB = zb_and(n1557, n1744);
    let n1746: ZB = zb_and(n1558, n1744);
    let n1747: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1738);
    let n1748: ZB = zb_or(n1745, n1746);
    let n1749: ZN = zsel_n(n1653, n1738, n1747);
    let n1750: ZB = zb_or(n1740, n1748);
    let n1751: ZN = zsel_n(n983, zn_splat(P8::from_raw(327680i32)), n1582);
    let n1752: ZB = zb_and(n995, n1518);
    let n1753: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1751);
    let n1754: ZB = zb_or(n1530, n1752);
    let n1755: ZB = zb_and(n1653, n1754);
    let n1756: ZB = zb_and(n1654, n1754);
    let n1757: ZB = zb_and(n1551, n1756);
    let n1758: ZB = zb_and(n1550, n1756);
    let n1759: ZB = zb_or(n1757, n1758);
    let n1760: ZB = zb_and(n1557, n1759);
    let n1761: ZB = zb_and(n1558, n1759);
    let n1762: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1753);
    let n1763: ZB = zb_or(n1760, n1761);
    let n1764: ZN = zsel_n(n1653, n1753, n1762);
    let n1765: ZB = zb_or(n1755, n1763);
    let n1766: ZN = zsel_n(n983, zn_splat(P8::from_raw(0i32)), n1539);
    let n1767: ZN = zsel_n(n983, zn_splat(P8::from_raw(-327680i32)), n1540);
    let n1768: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1766);
    let n1769: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1767);
    let n1770: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1768);
    let n1771: ZN = zsel_n(n1653, n1768, n1770);
    let n1772: ZN = zsel_n(n983, zn_splat(P8::from_raw(-231700i32)), n1568);
    let n1773: ZN = zsel_n(n983, zn_splat(P8::from_raw(-231700i32)), n1540);
    let n1774: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1772);
    let n1775: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1773);
    let n1776: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1774);
    let n1777: ZN = zsel_n(n1653, n1774, n1776);
    let n1778: ZN = zsel_n(n983, zn_splat(P8::from_raw(231700i32)), n1582);
    let n1779: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1778);
    let n1780: ZN = zsel_n(n1557, zn_splat(P8::from_raw(0i32)), n1779);
    let n1781: ZN = zsel_n(n1653, n1779, n1780);
    let n1782: ZN = zsel_n(n983, zn_splat(P8::from_raw(327680i32)), n1540);
    let n1783: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1782);
    let n1784: ZN = zsel_n(n983, zn_splat(P8::from_raw(231700i32)), n1540);
    let n1785: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1784);
    let n1792: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1793: ZB = zb_or(n49, r_c232);
    let n1794: ZN = zsel_n(n49, zn_splat(P8::from_raw(196608i32)), r_c244);
    let n1795: ZB = zb_and(n50, r_c262);
    let n1796: ZB = zb_and(n50, r_c263);
    let n1797: ZN = zsel_n(n49, zn_splat(P8::from_raw(524288i32)), r_c272);
    let n1798: ZN = zsel_n(n49, zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n1799: ZB = zsel_b(n47, n1793, r_c232);
    let n1800: ZN = zsel_n(n47, n1794, r_c244);
    let n1801: ZB = zsel_b(n47, n1795, r_c262);
    let n1802: ZB = zsel_b(n47, n1796, r_c263);
    let n1803: ZN = zsel_n(n47, n1797, r_c272);
    let n1804: ZN = zsel_n(n47, n1798, r_c273);
    let n1805: ZN = zn_sub(n89, zn_splat(P8::from_raw(32768i32)));
    let n1806: ZN = zn_sub(n1805, n90);
    let n1807: ZN = zsel_n(n137, zn_splat(P8::from_raw(0i32)), n1806);
    let n1808: ZN = zsel_n(n137, zn_splat(P8::from_raw(0i32)), n79);
    let n1809: ZN = zsel_n(n134, n1806, n1807);
    let n1810: ZN = zsel_n(n134, n79, n1808);
    let n1811: ZN = zsel_n(n132, zn_splat(P8::from_raw(0i32)), n1809);
    let n1812: ZN = zsel_n(n132, zn_splat(P8::from_raw(0i32)), n1810);
    let n1813: ZN = zsel_n(n129, n1806, n1811);
    let n1814: ZN = zsel_n(n129, n79, n1812);
    let n1815: ZN = zsel_n(n127, zn_splat(P8::from_raw(0i32)), n1813);
    let n1816: ZN = zsel_n(n127, zn_splat(P8::from_raw(0i32)), n1814);
    let n1817: ZN = zsel_n(n124, n1806, n1815);
    let n1818: ZN = zsel_n(n124, n79, n1816);
    let n1819: ZN = zsel_n(n122, zn_splat(P8::from_raw(0i32)), n1817);
    let n1820: ZN = zsel_n(n122, zn_splat(P8::from_raw(0i32)), n1818);
    let n1821: ZN = zsel_n(n119, n1806, n1819);
    let n1822: ZN = zsel_n(n119, n79, n1820);
    let n1823: ZN = zsel_n(n117, zn_splat(P8::from_raw(0i32)), n1821);
    let n1824: ZN = zsel_n(n117, zn_splat(P8::from_raw(0i32)), n1822);
    let n1825: ZN = zsel_n(n114, n1806, n1823);
    let n1826: ZN = zsel_n(n114, n79, n1824);
    let n1827: ZN = zsel_n(n112, zn_splat(P8::from_raw(0i32)), n1825);
    let n1828: ZN = zsel_n(n112, zn_splat(P8::from_raw(0i32)), n1826);
    let n1829: ZN = zsel_n(n109, n1806, n1827);
    let n1830: ZN = zsel_n(n109, n79, n1828);
    let n1831: ZN = zsel_n(n107, zn_splat(P8::from_raw(0i32)), n1829);
    let n1832: ZN = zsel_n(n107, zn_splat(P8::from_raw(0i32)), n1830);
    let n1833: ZN = zsel_n(n104, n1806, n1831);
    let n1834: ZN = zsel_n(n104, n79, n1832);
    let n1835: ZN = zsel_n(n102, zn_splat(P8::from_raw(0i32)), n1833);
    let n1836: ZN = zsel_n(n102, zn_splat(P8::from_raw(0i32)), n1834);
    let n1837: ZN = zsel_n(n69, n1835, n1806);
    let n1838: ZN = zsel_n(n69, n1836, n79);
    let n1839: ZN = zn_sub(n174, zn_splat(P8::from_raw(32768i32)));
    let n1840: ZN = zn_sub(n1839, n175);
    let n1841: ZN = zsel_n(n219, zn_splat(P8::from_raw(0i32)), n1840);
    let n1842: ZN = zsel_n(n219, zn_splat(P8::from_raw(0i32)), n80);
    let n1843: ZN = zsel_n(n216, n1840, n1841);
    let n1844: ZN = zsel_n(n216, n80, n1842);
    let n1845: ZN = zsel_n(n214, zn_splat(P8::from_raw(0i32)), n1843);
    let n1846: ZN = zsel_n(n214, zn_splat(P8::from_raw(0i32)), n1844);
    let n1847: ZN = zsel_n(n211, n1840, n1845);
    let n1848: ZN = zsel_n(n211, n80, n1846);
    let n1849: ZN = zsel_n(n209, zn_splat(P8::from_raw(0i32)), n1847);
    let n1850: ZN = zsel_n(n209, zn_splat(P8::from_raw(0i32)), n1848);
    let n1851: ZN = zsel_n(n206, n1840, n1849);
    let n1852: ZN = zsel_n(n206, n80, n1850);
    let n1853: ZN = zsel_n(n204, zn_splat(P8::from_raw(0i32)), n1851);
    let n1854: ZN = zsel_n(n204, zn_splat(P8::from_raw(0i32)), n1852);
    let n1855: ZN = zsel_n(n201, n1840, n1853);
    let n1856: ZN = zsel_n(n201, n80, n1854);
    let n1857: ZN = zsel_n(n199, zn_splat(P8::from_raw(0i32)), n1855);
    let n1858: ZN = zsel_n(n199, zn_splat(P8::from_raw(0i32)), n1856);
    let n1859: ZN = zsel_n(n196, n1840, n1857);
    let n1860: ZN = zsel_n(n196, n80, n1858);
    let n1861: ZN = zsel_n(n194, zn_splat(P8::from_raw(0i32)), n1859);
    let n1862: ZN = zsel_n(n194, zn_splat(P8::from_raw(0i32)), n1860);
    let n1863: ZN = zsel_n(n191, n1840, n1861);
    let n1864: ZN = zsel_n(n191, n80, n1862);
    let n1865: ZN = zsel_n(n189, zn_splat(P8::from_raw(0i32)), n1863);
    let n1866: ZN = zsel_n(n189, zn_splat(P8::from_raw(0i32)), n1864);
    let n1867: ZN = zsel_n(n186, n1840, n1865);
    let n1868: ZN = zsel_n(n186, n80, n1866);
    let n1869: ZN = zsel_n(n184, zn_splat(P8::from_raw(0i32)), n1867);
    let n1870: ZN = zsel_n(n184, zn_splat(P8::from_raw(0i32)), n1868);
    let n1871: ZN = zsel_n(n69, n1869, n1840);
    let n1872: ZN = zsel_n(n69, n1870, n80);
    let n1873: ZN = zsel_n(n85, n1837, n77);
    let n1874: ZN = zsel_n(n85, n1871, n78);
    let n1875: ZN = zsel_n(n85, n1838, n79);
    let n1876: ZN = zsel_n(n85, n1872, n80);
    let n1877: ZB = zb_and(n45, n259);
    let n1878: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n1804);
    let n1879: ZB = zn_lt(n257, n1878);
    let n1880: ZB = zn_ge(n257, n1878);
    let n1881: ZB = zb_and(n1877, n1879);
    let n1882: ZB = zb_and(n1877, n1880);
    let n1883: ZN = zsel_n(n1879, zn_splat(P8::from_raw(196608i32)), n68);
    let n1884: ZN = zsel_n(n1879, zn_splat(P8::from_raw(65536i32)), n70);
    let n1885: ZB = zb_or(n1881, n1882);
    let n1886: ZB = zb_and(n261, n262);
    let n1887: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1876);
    let n1888: ZB = zn_gt(n1887, zn_splat(P8::from_raw(0i32)));
    let n1889: ZB = zn_le(n1887, zn_splat(P8::from_raw(0i32)));
    let n1890: ZB = zb_and(n1886, n1888);
    let n1891: ZB = zb_and(n1886, n1889);
    let n1892: ZB = zn_gt(n68, zn_splat(P8::from_raw(0i32)));
    let n1893: ZB = zb_or(n1890, n1891);
    let n1894: ZB = zb_and(n1888, n1892);
    let n1895: ZB = zb_not(n1894);
    let n1896: ZB = zb_and(n1893, n1894);
    let n1897: ZB = zb_and(n1893, n1895);
    let n1898: ZN = zsel_n(n1894, n265, n68);
    let n1899: ZN = zsel_n(n1894, zn_splat(P8::from_raw(0i32)), n1887);
    let n1900: ZB = zb_or(n1896, n1897);
    let n1901: ZB = zn_gt(n1899, zn_splat(P8::from_raw(0i32)));
    let n1902: ZB = zn_le(n1899, zn_splat(P8::from_raw(0i32)));
    let n1903: ZB = zb_and(n1900, n1901);
    let n1904: ZB = zb_and(n1900, n1902);
    let n1905: ZB = zn_gt(n257, n1804);
    let n1906: ZB = zb_or(n1903, n1904);
    let n1907: ZB = zb_and(n1901, n1905);
    let n1908: ZB = zb_not(n1907);
    let n1909: ZB = zb_and(n1906, n1907);
    let n1910: ZB = zb_and(n1906, n1908);
    let n1911: ZN = zsel_n(n1907, zn_splat(P8::from_raw(327680i32)), n1898);
    let n1912: ZN = zsel_n(n1907, zn_splat(P8::from_raw(131072i32)), n70);
    let n1913: ZN = zsel_n(n1907, n1804, n257);
    let n1914: ZN = zsel_n(n1907, zn_splat(P8::from_raw(0i32)), n1875);
    let n1915: ZN = zsel_n(n1907, zn_splat(P8::from_raw(0i32)), n1899);
    let n1916: ZB = zb_or(n1909, n1910);
    let n1917: ZB = zb_not(n266);
    let n1918: ZB = zb_and(n264, n1917);
    let n1919: ZB = zn_ge(n265, zn_splat(P8::from_raw(0i32)));
    let n1920: ZB = zb_and(n267, n1919);
    let n1921: ZN = zsel_n(n266, n265, n68);
    let n1922: ZN = zsel_n(n266, zn_splat(P8::from_raw(393216i32)), n1800);
    let n1923: ZB = zb_or(n1918, n1920);
    let n1924: ZN = zsel_n(n262, n1911, n1921);
    let n1925: ZN = zsel_n(n262, n1800, n1922);
    let n1926: ZN = zsel_n(n262, n1912, n70);
    let n1927: ZN = zsel_n(n262, n1913, n257);
    let n1928: ZN = zsel_n(n262, n1914, n1875);
    let n1929: ZN = zsel_n(n262, n1915, n1876);
    let n1930: ZB = zb_or(n1916, n1923);
    let n1931: ZN = zsel_n(n259, n1883, n1924);
    let n1932: ZN = zsel_n(n259, n1800, n1925);
    let n1933: ZN = zsel_n(n259, n1884, n1926);
    let n1934: ZN = zsel_n(n259, n257, n1927);
    let n1935: ZN = zsel_n(n259, n1875, n1928);
    let n1936: ZN = zsel_n(n259, n1876, n1929);
    let n1937: ZB = zb_or(n1885, n1930);
    let n1938: ZN = zsel_n(n1000, r_c39, n1223);
    let n1939: ZN = zsel_n(n1000, n1792, r_c20);
    let n1940: ZB = zsel_b(n1000, r_c41, n66);
    let n1941: ZB = zsel_b(n1000, r_c42, n67);
    let n1942: ZB = zsel_b(n1000, r_c38, n1224);
    let n1943: ZB = zsel_b(n1000, r_c232, n1799);
    let n1944: ZN = zsel_n(n1000, r_c233, n1931);
    let n1945: ZB = zsel_b(n1000, r_c242, n69);
    let n1946: ZN = zsel_n(n1000, r_c244, n1932);
    let n1947: ZN = zsel_n(n1000, r_c245, n1933);
    let n1948: ZN = zsel_n(n1000, r_c248, n256);
    let n1949: ZN = zsel_n(n1000, r_c249, n1934);
    let n1950: ZB = zsel_b(n1000, r_c262, n1801);
    let n1951: ZB = zsel_b(n1000, r_c263, n1802);
    let n1952: ZN = zsel_n(n1000, zn_splat(u.c264), n73);
    let n1953: ZN = zsel_n(n1000, zn_splat(u.c265), n74);
    let n1954: ZN = zsel_n(n1000, zn_splat(u.c266), n75);
    let n1955: ZN = zsel_n(n1000, zn_splat(u.c267), n76);
    let n1956: ZN = zsel_n(n1000, r_c268, n1873);
    let n1957: ZN = zsel_n(n1000, r_c269, n1874);
    let n1958: ZN = zsel_n(n1000, r_c270, n1935);
    let n1959: ZN = zsel_n(n1000, r_c271, n1936);
    let n1960: ZN = zsel_n(n1000, r_c272, n1803);
    let n1961: ZN = zsel_n(n1000, r_c273, n1804);
    let n1962: ZB = zb_or(n1000, n1937);
    let n1963: ZB = zb_or(n258, n1000);
    let n1964: ZB = zn_gt(n1939, zn_splat(P8::from_raw(0i32)));
    let n1965: ZB = zn_le(n1939, zn_splat(P8::from_raw(0i32)));
    let n1966: ZB = zb_and(n1962, n1964);
    let n1967: ZB = zb_and(n1962, n1965);
    let n1968: ZB = zb_or(n1966, n1967);
    let n1971: ZW = zw_bits_b(n67);
    let n1972: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1971, 42u64);
    let n1973: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1971, 42u64);
    let n1974: ZW = zw_bits_b(r_c43);
    let n1975: ZW = zw_mix1(n1972, n1974, 43u64);
    let n1976: ZW = zw_mix2(n1973, n1974, 43u64);
    let n1977: ZW = zw_bits_n(r_c88);
    let n1978: ZW = zw_mix1(n1975, n1977, 88u64);
    let n1979: ZW = zw_mix2(n1976, n1977, 88u64);
    let n1980: ZW = zw_bits_n(r_c20);
    let n1981: ZW = zw_mix1(n1978, n1980, 20u64);
    let n1982: ZW = zw_mix2(n1979, n1980, 20u64);
    let n1983: ZW = zw_bits_b(n66);
    let n1984: ZW = zw_mix1(n1981, n1983, 41u64);
    let n1985: ZW = zw_mix2(n1982, n1983, 41u64);
    let n1986: ZW = zw_bits_n(n1154);
    let n1987: ZW = zw_mix1(n1978, n1986, 20u64);
    let n1988: ZW = zw_mix2(n1979, n1986, 20u64);
    let n1989: ZW = zw_bits_b(n1155);
    let n1990: ZW = zw_mix1(n1987, n1989, 41u64);
    let n1991: ZW = zw_mix2(n1988, n1989, 41u64);
    let n1992: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1974, 43u64);
    let n1993: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1974, 43u64);
    let n1994: ZW = zw_mix1(n1992, n1977, 88u64);
    let n1995: ZW = zw_mix2(n1993, n1977, 88u64);
    let n1996: ZW = zw_mix1(n1994, n1980, 20u64);
    let n1997: ZW = zw_mix2(n1995, n1980, 20u64);
    let n1998: ZW = zw_bits_b(n1254);
    let n1999: ZW = zw_mix1(n1996, n1998, 38u64);
    let n2000: ZW = zw_mix2(n1997, n1998, 38u64);
    let n2001: ZW = zw_bits_n(n1252);
    let n2002: ZW = zw_mix1(n1999, n2001, 39u64);
    let n2003: ZW = zw_mix2(n2000, n2001, 39u64);
    let n2004: ZW = zw_bits_b(n1286);
    let n2005: ZW = zw_mix1(n1996, n2004, 38u64);
    let n2006: ZW = zw_mix2(n1997, n2004, 38u64);
    let n2007: ZW = zw_bits_n(n1284);
    let n2008: ZW = zw_mix1(n2005, n2007, 39u64);
    let n2009: ZW = zw_mix2(n2006, n2007, 39u64);
    let n2010: ZW = zw_bits_b(n1315);
    let n2011: ZW = zw_mix1(n1996, n2010, 38u64);
    let n2012: ZW = zw_mix2(n1997, n2010, 38u64);
    let n2013: ZW = zw_bits_n(n1313);
    let n2014: ZW = zw_mix1(n2011, n2013, 39u64);
    let n2015: ZW = zw_mix2(n2012, n2013, 39u64);
    let n2016: ZW = zw_bits_b(n1353);
    let n2017: ZW = zw_mix1(n1996, n2016, 38u64);
    let n2018: ZW = zw_mix2(n1997, n2016, 38u64);
    let n2019: ZW = zw_bits_n(n1351);
    let n2020: ZW = zw_mix1(n2017, n2019, 39u64);
    let n2021: ZW = zw_mix2(n2018, n2019, 39u64);
    let n2022: ZW = zw_bits_b(n1391);
    let n2023: ZW = zw_mix1(n1996, n2022, 38u64);
    let n2024: ZW = zw_mix2(n1997, n2022, 38u64);
    let n2025: ZW = zw_bits_n(n1389);
    let n2026: ZW = zw_mix1(n2023, n2025, 39u64);
    let n2027: ZW = zw_mix2(n2024, n2025, 39u64);
    let n2028: ZW = zw_bits_b(n1429);
    let n2029: ZW = zw_mix1(n1996, n2028, 38u64);
    let n2030: ZW = zw_mix2(n1997, n2028, 38u64);
    let n2031: ZW = zw_bits_n(n1427);
    let n2032: ZW = zw_mix1(n2029, n2031, 39u64);
    let n2033: ZW = zw_mix2(n2030, n2031, 39u64);
    let n2034: ZW = zw_mix1(n1994, n1986, 20u64);
    let n2035: ZW = zw_mix2(n1995, n1986, 20u64);
    let n2036: ZW = zw_bits_b(n1443);
    let n2037: ZW = zw_mix1(n2034, n2036, 38u64);
    let n2038: ZW = zw_mix2(n2035, n2036, 38u64);
    let n2039: ZW = zw_bits_n(n1441);
    let n2040: ZW = zw_mix1(n2037, n2039, 39u64);
    let n2041: ZW = zw_mix2(n2038, n2039, 39u64);
    let n2042: ZW = zw_bits_b(n1459);
    let n2043: ZW = zw_mix1(n2034, n2042, 38u64);
    let n2044: ZW = zw_mix2(n2035, n2042, 38u64);
    let n2045: ZW = zw_bits_n(n1457);
    let n2046: ZW = zw_mix1(n2043, n2045, 39u64);
    let n2047: ZW = zw_mix2(n2044, n2045, 39u64);
    let n2048: ZW = zw_bits_b(n1475);
    let n2049: ZW = zw_mix1(n2034, n2048, 38u64);
    let n2050: ZW = zw_mix2(n2035, n2048, 38u64);
    let n2051: ZW = zw_bits_n(n1473);
    let n2052: ZW = zw_mix1(n2049, n2051, 39u64);
    let n2053: ZW = zw_mix2(n2050, n2051, 39u64);
    let n2054: ZW = zw_bits_b(n1491);
    let n2055: ZW = zw_mix1(n2034, n2054, 38u64);
    let n2056: ZW = zw_mix2(n2035, n2054, 38u64);
    let n2057: ZW = zw_bits_n(n1489);
    let n2058: ZW = zw_mix1(n2055, n2057, 39u64);
    let n2059: ZW = zw_mix2(n2056, n2057, 39u64);
    let n2060: ZW = zw_bits_b(n1507);
    let n2061: ZW = zw_mix1(n2034, n2060, 38u64);
    let n2062: ZW = zw_mix2(n2035, n2060, 38u64);
    let n2063: ZW = zw_bits_n(n1505);
    let n2064: ZW = zw_mix1(n2061, n2063, 39u64);
    let n2065: ZW = zw_mix2(n2062, n2063, 39u64);
    let n2066: ZW = zw_bits_b(n1523);
    let n2067: ZW = zw_mix1(n2034, n2066, 38u64);
    let n2068: ZW = zw_mix2(n2035, n2066, 38u64);
    let n2069: ZW = zw_bits_n(n1521);
    let n2070: ZW = zw_mix1(n2067, n2069, 39u64);
    let n2071: ZW = zw_mix2(n2068, n2069, 39u64);
    let n2072: ZW = zw_bits_b(n1224);
    let n2073: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2072, 38u64);
    let n2074: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2072, 38u64);
    let n2075: ZW = zw_bits_n(n1223);
    let n2076: ZW = zw_mix1(n2073, n2075, 39u64);
    let n2077: ZW = zw_mix2(n2074, n2075, 39u64);
    let n2078: ZW = zw_mix1(n2076, n1971, 42u64);
    let n2079: ZW = zw_mix2(n2077, n1971, 42u64);
    let n2080: ZW = zw_mix1(n2078, n1974, 43u64);
    let n2081: ZW = zw_mix2(n2079, n1974, 43u64);
    let n2082: ZW = zw_mix1(n2080, n1977, 88u64);
    let n2083: ZW = zw_mix2(n2081, n1977, 88u64);
    let n2084: ZW = zw_bits_n(n257);
    let n2085: ZW = zw_mix1(n2082, n2084, 254u64);
    let n2086: ZW = zw_mix2(n2083, n2084, 254u64);
    let n2087: ZW = zw_mix1(n2085, n1980, 20u64);
    let n2088: ZW = zw_mix2(n2086, n1980, 20u64);
    let n2089: ZW = zw_mix1(n2087, n1983, 41u64);
    let n2090: ZW = zw_mix2(n2088, n1983, 41u64);
    let n2091: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n2092: ZW = zw_mix1(n2089, zw_splat(n2091), 234u64);
    let n2093: ZW = zw_mix2(n2090, zw_splat(n2091), 234u64);
    let n2094: ZW = zw_mix1(n2092, zw_splat(n2091), 236u64);
    let n2095: ZW = zw_mix2(n2093, zw_splat(n2091), 236u64);
    let n2096: ZW = zw_mix1(n2094, n1977, 237u64);
    let n2097: ZW = zw_mix2(n2095, n1977, 237u64);
    let n2098: ZW = zw_bits_n(n1545);
    let n2099: ZW = zw_mix1(n2096, n2098, 239u64);
    let n2100: ZW = zw_mix2(n2097, n2098, 239u64);
    let n2101: u64 = false as u64;
    let n2102: ZW = zw_mix1(n2099, zw_splat(n2101), 246u64);
    let n2103: ZW = zw_mix2(n2100, zw_splat(n2101), 246u64);
    let n2104: ZW = zw_mix1(n2102, zw_splat(n2101), 247u64);
    let n2105: ZW = zw_mix2(n2103, zw_splat(n2101), 247u64);
    let n2106: ZW = zw_bits_n(n1565);
    let n2107: ZW = zw_mix1(n2104, n2106, 253u64);
    let n2108: ZW = zw_mix2(n2105, n2106, 253u64);
    let n2109: ZW = zw_mix1(n2107, zw_splat(n2091), 268u64);
    let n2110: ZW = zw_mix2(n2108, zw_splat(n2091), 268u64);
    let n2111: ZW = zw_mix1(n2109, zw_splat(n2091), 269u64);
    let n2112: ZW = zw_mix2(n2110, zw_splat(n2091), 269u64);
    let n2113: ZW = zw_mix1(n2111, zw_splat(n2091), 270u64);
    let n2114: ZW = zw_mix2(n2112, zw_splat(n2091), 270u64);
    let n2115: ZW = zw_mix1(n2113, zw_splat(n2091), 271u64);
    let n2116: ZW = zw_mix2(n2114, zw_splat(n2091), 271u64);
    let n2117: ZW = zw_mix1(n2115, zw_splat(n2101), 272u64);
    let n2118: ZW = zw_mix2(n2116, zw_splat(n2101), 272u64);
    let n2119: ZW = zw_mix1(n2117, zw_splat(n2091), 280u64);
    let n2120: ZW = zw_mix2(n2118, zw_splat(n2091), 280u64);
    let n2121: ZW = zw_bits_n(n1546);
    let n2122: ZW = zw_mix1(n2119, n2121, 281u64);
    let n2123: ZW = zw_mix2(n2120, n2121, 281u64);
    let n2124: ZW = zw_bits_b(n270);
    let n2125: ZW = zw_mix1(n2115, n2124, 272u64);
    let n2126: ZW = zw_mix2(n2116, n2124, 272u64);
    let n2127: ZW = zw_bits_n(n1580);
    let n2128: ZW = zw_mix1(n2125, n2127, 280u64);
    let n2129: ZW = zw_mix2(n2126, n2127, 280u64);
    let n2130: ZW = zw_mix1(n2128, n2121, 281u64);
    let n2131: ZW = zw_mix2(n2129, n2121, 281u64);
    let n2132: ZW = zw_bits_n(n1594);
    let n2133: ZW = zw_mix1(n2117, n2132, 280u64);
    let n2134: ZW = zw_mix2(n2118, n2132, 280u64);
    let n2135: ZW = zw_mix1(n2133, n2121, 281u64);
    let n2136: ZW = zw_mix2(n2134, n2121, 281u64);
    let n2137: ZW = zw_bits_n(n1596);
    let n2138: ZW = zw_mix1(n2096, n2137, 239u64);
    let n2139: ZW = zw_mix2(n2097, n2137, 239u64);
    let n2140: ZW = zw_mix1(n2138, zw_splat(n2101), 246u64);
    let n2141: ZW = zw_mix2(n2139, zw_splat(n2101), 246u64);
    let n2142: ZW = zw_mix1(n2140, n2124, 247u64);
    let n2143: ZW = zw_mix2(n2141, n2124, 247u64);
    let n2144: ZW = zw_mix1(n2142, n2106, 253u64);
    let n2145: ZW = zw_mix2(n2143, n2106, 253u64);
    let n2146: ZW = zw_mix1(n2144, zw_splat(n2091), 268u64);
    let n2147: ZW = zw_mix2(n2145, zw_splat(n2091), 268u64);
    let n2148: ZW = zw_mix1(n2146, zw_splat(n2091), 269u64);
    let n2149: ZW = zw_mix2(n2147, zw_splat(n2091), 269u64);
    let n2150: ZW = zw_mix1(n2148, zw_splat(n2091), 270u64);
    let n2151: ZW = zw_mix2(n2149, zw_splat(n2091), 270u64);
    let n2152: ZW = zw_mix1(n2150, zw_splat(n2091), 271u64);
    let n2153: ZW = zw_mix2(n2151, zw_splat(n2091), 271u64);
    let n2154: ZW = zw_mix1(n2152, zw_splat(n2101), 272u64);
    let n2155: ZW = zw_mix2(n2153, zw_splat(n2101), 272u64);
    let n2156: ZW = zw_bits_n(n1608);
    let n2157: ZW = zw_mix1(n2154, n2156, 280u64);
    let n2158: ZW = zw_mix2(n2155, n2156, 280u64);
    let n2159: ZW = zw_bits_n(n1598);
    let n2160: ZW = zw_mix1(n2157, n2159, 281u64);
    let n2161: ZW = zw_mix2(n2158, n2159, 281u64);
    let n2162: ZW = zw_mix1(n2152, n2124, 272u64);
    let n2163: ZW = zw_mix2(n2153, n2124, 272u64);
    let n2164: ZW = zw_bits_n(n1620);
    let n2165: ZW = zw_mix1(n2162, n2164, 280u64);
    let n2166: ZW = zw_mix2(n2163, n2164, 280u64);
    let n2167: ZW = zw_mix1(n2165, n2159, 281u64);
    let n2168: ZW = zw_mix2(n2166, n2159, 281u64);
    let n2169: ZW = zw_bits_n(n1632);
    let n2170: ZW = zw_mix1(n2154, n2169, 280u64);
    let n2171: ZW = zw_mix2(n2155, n2169, 280u64);
    let n2172: ZW = zw_mix1(n2170, n2159, 281u64);
    let n2173: ZW = zw_mix2(n2171, n2159, 281u64);
    let n2174: ZW = zw_bits_n(n1642);
    let n2175: ZW = zw_mix1(n2085, n2174, 20u64);
    let n2176: ZW = zw_mix2(n2086, n2174, 20u64);
    let n2177: ZW = zw_bits_b(n1643);
    let n2178: ZW = zw_mix1(n2175, n2177, 41u64);
    let n2179: ZW = zw_mix2(n2176, n2177, 41u64);
    let n2180: ZW = zw_bits_n(n1667);
    let n2181: ZW = zw_mix1(n2178, n2180, 234u64);
    let n2182: ZW = zw_mix2(n2179, n2180, 234u64);
    let n2183: ZW = zw_bits_n(n1645);
    let n2184: ZW = zw_mix1(n2181, n2183, 236u64);
    let n2185: ZW = zw_mix2(n2182, n2183, 236u64);
    let n2186: ZW = zw_bits_n(n1646);
    let n2187: ZW = zw_mix1(n2184, n2186, 237u64);
    let n2188: ZW = zw_mix2(n2185, n2186, 237u64);
    let n2189: ZW = zw_mix1(n2187, n2098, 239u64);
    let n2190: ZW = zw_mix2(n2188, n2098, 239u64);
    let n2191: ZW = zw_mix1(n2189, n2124, 246u64);
    let n2192: ZW = zw_mix2(n2190, n2124, 246u64);
    let n2193: ZW = zw_mix1(n2191, zw_splat(n2101), 247u64);
    let n2194: ZW = zw_mix2(n2192, zw_splat(n2101), 247u64);
    let n2195: ZW = zw_bits_n(n1664);
    let n2196: ZW = zw_mix1(n2193, n2195, 253u64);
    let n2197: ZW = zw_mix2(n2194, n2195, 253u64);
    let n2198: ZW = zw_bits_n(n1647);
    let n2199: ZW = zw_mix1(n2196, n2198, 268u64);
    let n2200: ZW = zw_mix2(n2197, n2198, 268u64);
    let n2201: ZW = zw_bits_n(n1648);
    let n2202: ZW = zw_mix1(n2199, n2201, 269u64);
    let n2203: ZW = zw_mix2(n2200, n2201, 269u64);
    let n2204: ZW = zw_bits_n(n1649);
    let n2205: ZW = zw_mix1(n2202, n2204, 270u64);
    let n2206: ZW = zw_mix2(n2203, n2204, 270u64);
    let n2207: ZW = zw_mix1(n2205, zw_splat(n2091), 271u64);
    let n2208: ZW = zw_mix2(n2206, zw_splat(n2091), 271u64);
    let n2209: ZW = zw_mix1(n2207, zw_splat(n2101), 272u64);
    let n2210: ZW = zw_mix2(n2208, zw_splat(n2101), 272u64);
    let n2211: ZW = zw_bits_n(n1665);
    let n2212: ZW = zw_mix1(n2209, n2211, 280u64);
    let n2213: ZW = zw_mix2(n2210, n2211, 280u64);
    let n2214: ZW = zw_bits_n(n1651);
    let n2215: ZW = zw_mix1(n2212, n2214, 281u64);
    let n2216: ZW = zw_mix2(n2213, n2214, 281u64);
    let n2217: ZW = zw_bits_n(n1671);
    let n2218: ZW = zw_mix1(n2202, n2217, 270u64);
    let n2219: ZW = zw_mix2(n2203, n2217, 270u64);
    let n2220: ZW = zw_mix1(n2218, zw_splat(n2091), 271u64);
    let n2221: ZW = zw_mix2(n2219, zw_splat(n2091), 271u64);
    let n2222: ZW = zw_mix1(n2220, n2124, 272u64);
    let n2223: ZW = zw_mix2(n2221, n2124, 272u64);
    let n2224: ZW = zw_bits_n(n1683);
    let n2225: ZW = zw_mix1(n2222, n2224, 280u64);
    let n2226: ZW = zw_mix2(n2223, n2224, 280u64);
    let n2227: ZW = zw_mix1(n2225, n2214, 281u64);
    let n2228: ZW = zw_mix2(n2226, n2214, 281u64);
    let n2229: ZW = zw_bits_n(n1698);
    let n2230: ZW = zw_mix1(n2209, n2229, 280u64);
    let n2231: ZW = zw_mix2(n2210, n2229, 280u64);
    let n2232: ZW = zw_mix1(n2230, n2214, 281u64);
    let n2233: ZW = zw_mix2(n2231, n2214, 281u64);
    let n2234: ZW = zw_mix1(n2196, n2201, 268u64);
    let n2235: ZW = zw_mix2(n2197, n2201, 268u64);
    let n2236: ZW = zw_mix1(n2234, n2198, 269u64);
    let n2237: ZW = zw_mix2(n2235, n2198, 269u64);
    let n2238: ZW = zw_mix1(n2236, zw_splat(n2091), 270u64);
    let n2239: ZW = zw_mix2(n2237, zw_splat(n2091), 270u64);
    let n2240: ZW = zw_bits_n(n1703);
    let n2241: ZW = zw_mix1(n2238, n2240, 271u64);
    let n2242: ZW = zw_mix2(n2239, n2240, 271u64);
    let n2243: ZW = zw_mix1(n2241, zw_splat(n2101), 272u64);
    let n2244: ZW = zw_mix2(n2242, zw_splat(n2101), 272u64);
    let n2245: ZW = zw_mix1(n2243, zw_splat(n2091), 280u64);
    let n2246: ZW = zw_mix2(n2244, zw_splat(n2091), 280u64);
    let n2247: ZW = zw_bits_n(n1704);
    let n2248: ZW = zw_mix1(n2245, n2247, 281u64);
    let n2249: ZW = zw_mix2(n2246, n2247, 281u64);
    let n2250: ZW = zw_mix1(n2234, n2201, 269u64);
    let n2251: ZW = zw_mix2(n2235, n2201, 269u64);
    let n2252: ZW = zw_mix1(n2250, n2217, 270u64);
    let n2253: ZW = zw_mix2(n2251, n2217, 270u64);
    let n2254: ZW = zw_mix1(n2252, n2240, 271u64);
    let n2255: ZW = zw_mix2(n2253, n2240, 271u64);
    let n2256: ZW = zw_mix1(n2254, n2124, 272u64);
    let n2257: ZW = zw_mix2(n2255, n2124, 272u64);
    let n2258: ZW = zw_bits_n(n1710);
    let n2259: ZW = zw_mix1(n2256, n2258, 280u64);
    let n2260: ZW = zw_mix2(n2257, n2258, 280u64);
    let n2261: ZW = zw_bits_n(n1708);
    let n2262: ZW = zw_mix1(n2259, n2261, 281u64);
    let n2263: ZW = zw_mix2(n2260, n2261, 281u64);
    let n2264: ZW = zw_mix1(n2250, n2204, 270u64);
    let n2265: ZW = zw_mix2(n2251, n2204, 270u64);
    let n2266: ZW = zw_mix1(n2264, n2240, 271u64);
    let n2267: ZW = zw_mix2(n2265, n2240, 271u64);
    let n2268: ZW = zw_mix1(n2266, zw_splat(n2101), 272u64);
    let n2269: ZW = zw_mix2(n2267, zw_splat(n2101), 272u64);
    let n2270: ZW = zw_bits_n(n1714);
    let n2271: ZW = zw_mix1(n2268, n2270, 280u64);
    let n2272: ZW = zw_mix2(n2269, n2270, 280u64);
    let n2273: ZW = zw_mix1(n2271, n2261, 281u64);
    let n2274: ZW = zw_mix2(n2272, n2261, 281u64);
    let n2275: ZW = zw_mix1(n2238, n2204, 271u64);
    let n2276: ZW = zw_mix2(n2239, n2204, 271u64);
    let n2277: ZW = zw_mix1(n2275, zw_splat(n2101), 272u64);
    let n2278: ZW = zw_mix2(n2276, zw_splat(n2101), 272u64);
    let n2279: ZW = zw_mix1(n2277, zw_splat(n2091), 280u64);
    let n2280: ZW = zw_mix2(n2278, zw_splat(n2091), 280u64);
    let n2281: ZW = zw_bits_n(n1716);
    let n2282: ZW = zw_mix1(n2279, n2281, 281u64);
    let n2283: ZW = zw_mix2(n2280, n2281, 281u64);
    let n2284: ZW = zw_mix1(n2252, n2204, 271u64);
    let n2285: ZW = zw_mix2(n2253, n2204, 271u64);
    let n2286: ZW = zw_mix1(n2284, n2124, 272u64);
    let n2287: ZW = zw_mix2(n2285, n2124, 272u64);
    let n2288: ZW = zw_mix1(n2286, n2258, 280u64);
    let n2289: ZW = zw_mix2(n2287, n2258, 280u64);
    let n2290: ZW = zw_bits_n(n1718);
    let n2291: ZW = zw_mix1(n2288, n2290, 281u64);
    let n2292: ZW = zw_mix2(n2289, n2290, 281u64);
    let n2293: ZW = zw_mix1(n2264, n2204, 271u64);
    let n2294: ZW = zw_mix2(n2265, n2204, 271u64);
    let n2295: ZW = zw_mix1(n2293, zw_splat(n2101), 272u64);
    let n2296: ZW = zw_mix2(n2294, zw_splat(n2101), 272u64);
    let n2297: ZW = zw_mix1(n2295, n2270, 280u64);
    let n2298: ZW = zw_mix2(n2296, n2270, 280u64);
    let n2299: ZW = zw_mix1(n2297, n2290, 281u64);
    let n2300: ZW = zw_mix2(n2298, n2290, 281u64);
    let n2301: ZW = zw_mix1(n2187, n2137, 239u64);
    let n2302: ZW = zw_mix2(n2188, n2137, 239u64);
    let n2303: ZW = zw_mix1(n2301, n2124, 246u64);
    let n2304: ZW = zw_mix2(n2302, n2124, 246u64);
    let n2305: ZW = zw_mix1(n2303, n2124, 247u64);
    let n2306: ZW = zw_mix2(n2304, n2124, 247u64);
    let n2307: ZW = zw_mix1(n2305, n2195, 253u64);
    let n2308: ZW = zw_mix2(n2306, n2195, 253u64);
    let n2309: ZW = zw_mix1(n2307, n2198, 268u64);
    let n2310: ZW = zw_mix2(n2308, n2198, 268u64);
    let n2311: ZW = zw_mix1(n2309, n2201, 269u64);
    let n2312: ZW = zw_mix2(n2310, n2201, 269u64);
    let n2313: ZW = zw_mix1(n2311, n2204, 270u64);
    let n2314: ZW = zw_mix2(n2312, n2204, 270u64);
    let n2315: ZW = zw_mix1(n2313, zw_splat(n2091), 271u64);
    let n2316: ZW = zw_mix2(n2314, zw_splat(n2091), 271u64);
    let n2317: ZW = zw_mix1(n2315, zw_splat(n2101), 272u64);
    let n2318: ZW = zw_mix2(n2316, zw_splat(n2101), 272u64);
    let n2319: ZW = zw_bits_n(n1734);
    let n2320: ZW = zw_mix1(n2317, n2319, 280u64);
    let n2321: ZW = zw_mix2(n2318, n2319, 280u64);
    let n2322: ZW = zw_bits_n(n1723);
    let n2323: ZW = zw_mix1(n2320, n2322, 281u64);
    let n2324: ZW = zw_mix2(n2321, n2322, 281u64);
    let n2325: ZW = zw_mix1(n2311, n2217, 270u64);
    let n2326: ZW = zw_mix2(n2312, n2217, 270u64);
    let n2327: ZW = zw_mix1(n2325, zw_splat(n2091), 271u64);
    let n2328: ZW = zw_mix2(n2326, zw_splat(n2091), 271u64);
    let n2329: ZW = zw_mix1(n2327, n2124, 272u64);
    let n2330: ZW = zw_mix2(n2328, n2124, 272u64);
    let n2331: ZW = zw_bits_n(n1749);
    let n2332: ZW = zw_mix1(n2329, n2331, 280u64);
    let n2333: ZW = zw_mix2(n2330, n2331, 280u64);
    let n2334: ZW = zw_mix1(n2332, n2322, 281u64);
    let n2335: ZW = zw_mix2(n2333, n2322, 281u64);
    let n2336: ZW = zw_bits_n(n1764);
    let n2337: ZW = zw_mix1(n2317, n2336, 280u64);
    let n2338: ZW = zw_mix2(n2318, n2336, 280u64);
    let n2339: ZW = zw_mix1(n2337, n2322, 281u64);
    let n2340: ZW = zw_mix2(n2338, n2322, 281u64);
    let n2341: ZW = zw_mix1(n2307, n2201, 268u64);
    let n2342: ZW = zw_mix2(n2308, n2201, 268u64);
    let n2343: ZW = zw_mix1(n2341, n2198, 269u64);
    let n2344: ZW = zw_mix2(n2342, n2198, 269u64);
    let n2345: ZW = zw_mix1(n2343, zw_splat(n2091), 270u64);
    let n2346: ZW = zw_mix2(n2344, zw_splat(n2091), 270u64);
    let n2347: ZW = zw_mix1(n2345, n2240, 271u64);
    let n2348: ZW = zw_mix2(n2346, n2240, 271u64);
    let n2349: ZW = zw_mix1(n2347, zw_splat(n2101), 272u64);
    let n2350: ZW = zw_mix2(n2348, zw_splat(n2101), 272u64);
    let n2351: ZW = zw_bits_n(n1771);
    let n2352: ZW = zw_mix1(n2349, n2351, 280u64);
    let n2353: ZW = zw_mix2(n2350, n2351, 280u64);
    let n2354: ZW = zw_bits_n(n1769);
    let n2355: ZW = zw_mix1(n2352, n2354, 281u64);
    let n2356: ZW = zw_mix2(n2353, n2354, 281u64);
    let n2357: ZW = zw_mix1(n2341, n2201, 269u64);
    let n2358: ZW = zw_mix2(n2342, n2201, 269u64);
    let n2359: ZW = zw_mix1(n2357, n2217, 270u64);
    let n2360: ZW = zw_mix2(n2358, n2217, 270u64);
    let n2361: ZW = zw_mix1(n2359, n2240, 271u64);
    let n2362: ZW = zw_mix2(n2360, n2240, 271u64);
    let n2363: ZW = zw_mix1(n2361, n2124, 272u64);
    let n2364: ZW = zw_mix2(n2362, n2124, 272u64);
    let n2365: ZW = zw_bits_n(n1777);
    let n2366: ZW = zw_mix1(n2363, n2365, 280u64);
    let n2367: ZW = zw_mix2(n2364, n2365, 280u64);
    let n2368: ZW = zw_bits_n(n1775);
    let n2369: ZW = zw_mix1(n2366, n2368, 281u64);
    let n2370: ZW = zw_mix2(n2367, n2368, 281u64);
    let n2371: ZW = zw_mix1(n2357, n2204, 270u64);
    let n2372: ZW = zw_mix2(n2358, n2204, 270u64);
    let n2373: ZW = zw_mix1(n2371, n2240, 271u64);
    let n2374: ZW = zw_mix2(n2372, n2240, 271u64);
    let n2375: ZW = zw_mix1(n2373, zw_splat(n2101), 272u64);
    let n2376: ZW = zw_mix2(n2374, zw_splat(n2101), 272u64);
    let n2377: ZW = zw_bits_n(n1781);
    let n2378: ZW = zw_mix1(n2375, n2377, 280u64);
    let n2379: ZW = zw_mix2(n2376, n2377, 280u64);
    let n2380: ZW = zw_mix1(n2378, n2368, 281u64);
    let n2381: ZW = zw_mix2(n2379, n2368, 281u64);
    let n2382: ZW = zw_mix1(n2345, n2204, 271u64);
    let n2383: ZW = zw_mix2(n2346, n2204, 271u64);
    let n2384: ZW = zw_mix1(n2382, zw_splat(n2101), 272u64);
    let n2385: ZW = zw_mix2(n2383, zw_splat(n2101), 272u64);
    let n2386: ZW = zw_mix1(n2384, n2351, 280u64);
    let n2387: ZW = zw_mix2(n2385, n2351, 280u64);
    let n2388: ZW = zw_bits_n(n1783);
    let n2389: ZW = zw_mix1(n2386, n2388, 281u64);
    let n2390: ZW = zw_mix2(n2387, n2388, 281u64);
    let n2391: ZW = zw_mix1(n2359, n2204, 271u64);
    let n2392: ZW = zw_mix2(n2360, n2204, 271u64);
    let n2393: ZW = zw_mix1(n2391, n2124, 272u64);
    let n2394: ZW = zw_mix2(n2392, n2124, 272u64);
    let n2395: ZW = zw_mix1(n2393, n2365, 280u64);
    let n2396: ZW = zw_mix2(n2394, n2365, 280u64);
    let n2397: ZW = zw_bits_n(n1785);
    let n2398: ZW = zw_mix1(n2395, n2397, 281u64);
    let n2399: ZW = zw_mix2(n2396, n2397, 281u64);
    let n2400: ZW = zw_mix1(n2371, n2204, 271u64);
    let n2401: ZW = zw_mix2(n2372, n2204, 271u64);
    let n2402: ZW = zw_mix1(n2400, zw_splat(n2101), 272u64);
    let n2403: ZW = zw_mix2(n2401, zw_splat(n2101), 272u64);
    let n2404: ZW = zw_mix1(n2402, n2377, 280u64);
    let n2405: ZW = zw_mix2(n2403, n2377, 280u64);
    let n2406: ZW = zw_mix1(n2404, n2397, 281u64);
    let n2407: ZW = zw_mix2(n2405, n2397, 281u64);
    let n2408: ZW = zw_bits_n(n1939);
    let n2409: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2408, 20u64);
    let n2410: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2408, 20u64);
    let n2411: ZW = zw_bits_b(n1942);
    let n2412: ZW = zw_mix1(n2409, n2411, 38u64);
    let n2413: ZW = zw_mix2(n2410, n2411, 38u64);
    let n2414: ZW = zw_bits_n(n1938);
    let n2415: ZW = zw_mix1(n2412, n2414, 39u64);
    let n2416: ZW = zw_mix2(n2413, n2414, 39u64);
    let n2417: ZW = zw_bits_b(n1940);
    let n2418: ZW = zw_mix1(n2415, n2417, 41u64);
    let n2419: ZW = zw_mix2(n2416, n2417, 41u64);
    let n2420: ZW = zw_bits_b(n1941);
    let n2421: ZW = zw_mix1(n2418, n2420, 42u64);
    let n2422: ZW = zw_mix2(n2419, n2420, 42u64);
    let n2423: ZW = zw_mix1(n2421, n1974, 43u64);
    let n2424: ZW = zw_mix2(n2422, n1974, 43u64);
    let n2425: ZW = zw_mix1(n2423, n1977, 88u64);
    let n2426: ZW = zw_mix2(n2424, n1977, 88u64);
    let n2427: ZW = zw_bits_b(n1943);
    let n2428: ZW = zw_mix1(n2425, n2427, 232u64);
    let n2429: ZW = zw_mix2(n2426, n2427, 232u64);
    let n2430: ZW = zw_bits_n(n1944);
    let n2431: ZW = zw_mix1(n2428, n2430, 233u64);
    let n2432: ZW = zw_mix2(n2429, n2430, 233u64);
    let n2433: ZW = zw_bits_b(n1945);
    let n2434: ZW = zw_mix1(n2431, n2433, 242u64);
    let n2435: ZW = zw_mix2(n2432, n2433, 242u64);
    let n2436: ZW = zw_bits_n(n1946);
    let n2437: ZW = zw_mix1(n2434, n2436, 244u64);
    let n2438: ZW = zw_mix2(n2435, n2436, 244u64);
    let n2439: ZW = zw_bits_n(n1947);
    let n2440: ZW = zw_mix1(n2437, n2439, 245u64);
    let n2441: ZW = zw_mix2(n2438, n2439, 245u64);
    let n2442: ZW = zw_bits_n(n1948);
    let n2443: ZW = zw_mix1(n2440, n2442, 248u64);
    let n2444: ZW = zw_mix2(n2441, n2442, 248u64);
    let n2445: ZW = zw_bits_n(n1949);
    let n2446: ZW = zw_mix1(n2443, n2445, 249u64);
    let n2447: ZW = zw_mix2(n2444, n2445, 249u64);
    let n2448: ZW = zw_bits_b(n1950);
    let n2449: ZW = zw_mix1(n2446, n2448, 262u64);
    let n2450: ZW = zw_mix2(n2447, n2448, 262u64);
    let n2451: ZW = zw_bits_b(n1951);
    let n2452: ZW = zw_mix1(n2449, n2451, 263u64);
    let n2453: ZW = zw_mix2(n2450, n2451, 263u64);
    let n2454: ZW = zw_bits_n(n1952);
    let n2455: ZW = zw_mix1(n2452, n2454, 264u64);
    let n2456: ZW = zw_mix2(n2453, n2454, 264u64);
    let n2457: ZW = zw_bits_n(n1953);
    let n2458: ZW = zw_mix1(n2455, n2457, 265u64);
    let n2459: ZW = zw_mix2(n2456, n2457, 265u64);
    let n2460: ZW = zw_bits_n(n1954);
    let n2461: ZW = zw_mix1(n2458, n2460, 266u64);
    let n2462: ZW = zw_mix2(n2459, n2460, 266u64);
    let n2463: ZW = zw_bits_n(n1955);
    let n2464: ZW = zw_mix1(n2461, n2463, 267u64);
    let n2465: ZW = zw_mix2(n2462, n2463, 267u64);
    let n2466: ZW = zw_bits_n(n1956);
    let n2467: ZW = zw_mix1(n2464, n2466, 268u64);
    let n2468: ZW = zw_mix2(n2465, n2466, 268u64);
    let n2469: ZW = zw_bits_n(n1957);
    let n2470: ZW = zw_mix1(n2467, n2469, 269u64);
    let n2471: ZW = zw_mix2(n2468, n2469, 269u64);
    let n2472: ZW = zw_bits_n(n1958);
    let n2473: ZW = zw_mix1(n2470, n2472, 270u64);
    let n2474: ZW = zw_mix2(n2471, n2472, 270u64);
    let n2475: ZW = zw_bits_n(n1959);
    let n2476: ZW = zw_mix1(n2473, n2475, 271u64);
    let n2477: ZW = zw_mix2(n2474, n2475, 271u64);
    let n2478: ZW = zw_bits_n(n1960);
    let n2479: ZW = zw_mix1(n2476, n2478, 272u64);
    let n2480: ZW = zw_mix2(n2477, n2478, 272u64);
    let n2481: ZW = zw_bits_n(n1961);
    let n2482: ZW = zw_mix1(n2479, n2481, 273u64);
    let n2483: ZW = zw_mix2(n2480, n2481, 273u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n943);
    let bd_v0_b0: bool = false;
    let live_v0_b0: u16 = ALL & zb_holds(n45) & zb_holds(n995) & zb_holds(n998);
    let ok_v1_b1: u16 = ALL & zb_holds(n943);
    let bd_v1_b1: bool = false;
    let live_v1_b1: u16 = ALL & zb_holds(n45) & zb_holds(n995) & zb_holds(n1028);
    let ok_v2_b2: u16 = ALL & zb_holds(n943);
    let bd_v2_b2: bool = false;
    let live_v2_b2: u16 = ALL & zb_holds(n45) & zb_holds(n995) & zb_holds(n1055);
    let ok_v16_b3: u16 = ALL & zb_holds(n943);
    let bd_v16_b3: bool = false;
    let live_v16_b3: u16 = ALL & zb_holds(n45) & zb_holds(n995) & zb_holds(n1087);
    let ok_v17_b4: u16 = ALL & zb_holds(n943);
    let bd_v17_b4: bool = false;
    let live_v17_b4: u16 = ALL & zb_holds(n45) & zb_holds(n995) & zb_holds(n1119);
    let ok_v18_b5: u16 = ALL & zb_holds(n943);
    let bd_v18_b5: bool = false;
    let live_v18_b5: u16 = ALL & zb_holds(n45) & zb_holds(n995) & zb_holds(n1151);
    let ok_v32_b6: u16 = ALL & zb_holds(n943);
    let bd_v32_b6: bool = false;
    let live_v32_b6: u16 = ALL & zb_holds(n1167);
    let ok_v33_b7: u16 = ALL & zb_holds(n943);
    let bd_v33_b7: bool = false;
    let live_v33_b7: u16 = ALL & zb_holds(n1177);
    let ok_v34_b8: u16 = ALL & zb_holds(n943);
    let bd_v34_b8: bool = false;
    let live_v34_b8: u16 = ALL & zb_holds(n1187);
    let ok_v48_b9: u16 = ALL & zb_holds(n943);
    let bd_v48_b9: bool = false;
    let live_v48_b9: u16 = ALL & zb_holds(n1197);
    let ok_v49_b10: u16 = ALL & zb_holds(n943);
    let bd_v49_b10: bool = false;
    let live_v49_b10: u16 = ALL & zb_holds(n1207);
    let ok_v50_b11: u16 = ALL & zb_holds(n943);
    let bd_v50_b11: bool = false;
    let live_v50_b11: u16 = ALL & zb_holds(n1217);
    let ok_v0_b12: u16 = ALL & zb_holds(n1256);
    let bd_v0_b12: bool = false;
    let live_v0_b12: u16 = ALL & zb_holds(n45) & zb_holds(n1255);
    let ok_v1_b13: u16 = ALL & zb_holds(n1288);
    let bd_v1_b13: bool = false;
    let live_v1_b13: u16 = ALL & zb_holds(n45) & zb_holds(n1287);
    let ok_v2_b14: u16 = ALL & zb_holds(n1317);
    let bd_v2_b14: bool = false;
    let live_v2_b14: u16 = ALL & zb_holds(n45) & zb_holds(n1316);
    let ok_v16_b15: u16 = ALL & zb_holds(n1355);
    let bd_v16_b15: bool = false;
    let live_v16_b15: u16 = ALL & zb_holds(n45) & zb_holds(n1354);
    let ok_v17_b16: u16 = ALL & zb_holds(n1393);
    let bd_v17_b16: bool = false;
    let live_v17_b16: u16 = ALL & zb_holds(n45) & zb_holds(n1392);
    let ok_v18_b17: u16 = ALL & zb_holds(n1431);
    let bd_v18_b17: bool = false;
    let live_v18_b17: u16 = ALL & zb_holds(n45) & zb_holds(n1430);
    let ok_v32_b18: u16 = ALL & zb_holds(n1445);
    let bd_v32_b18: bool = false;
    let live_v32_b18: u16 = ALL & zb_holds(n1448);
    let ok_v33_b19: u16 = ALL & zb_holds(n1461);
    let bd_v33_b19: bool = false;
    let live_v33_b19: u16 = ALL & zb_holds(n1464);
    let ok_v34_b20: u16 = ALL & zb_holds(n1477);
    let bd_v34_b20: bool = false;
    let live_v34_b20: u16 = ALL & zb_holds(n1480);
    let ok_v48_b21: u16 = ALL & zb_holds(n1493);
    let bd_v48_b21: bool = false;
    let live_v48_b21: u16 = ALL & zb_holds(n1496);
    let ok_v49_b22: u16 = ALL & zb_holds(n1509);
    let bd_v49_b22: bool = false;
    let live_v49_b22: u16 = ALL & zb_holds(n1512);
    let ok_v50_b23: u16 = ALL & zb_holds(n1525);
    let bd_v50_b23: bool = false;
    let live_v50_b23: u16 = ALL & zb_holds(n1528);
    let ok_v0_b24: u16 = ALL & zb_holds(n1548);
    let bd_v0_b24: bool = false;
    let live_v0_b24: u16 = ALL & zb_holds(n1564);
    let ok_v1_b25: u16 = ALL & zb_holds(n1548);
    let bd_v1_b25: bool = false;
    let live_v1_b25: u16 = ALL & zb_holds(n1579);
    let ok_v2_b26: u16 = ALL & zb_holds(n1548);
    let bd_v2_b26: bool = false;
    let live_v2_b26: u16 = ALL & zb_holds(n1593);
    let ok_v16_b27: u16 = ALL & zb_holds(n1548);
    let bd_v16_b27: bool = false;
    let live_v16_b27: u16 = ALL & zb_holds(n1607);
    let ok_v17_b28: u16 = ALL & zb_holds(n1548);
    let bd_v17_b28: bool = false;
    let live_v17_b28: u16 = ALL & zb_holds(n1619);
    let ok_v18_b29: u16 = ALL & zb_holds(n1548);
    let bd_v18_b29: bool = false;
    let live_v18_b29: u16 = ALL & zb_holds(n1631);
    let ok_v32_b30: u16 = ALL & zb_holds(n1548);
    let bd_v32_b30: bool = false;
    let live_v32_b30: u16 = ALL & zb_holds(n1666);
    let ok_v33_b31: u16 = ALL & zb_holds(n1548);
    let bd_v33_b31: bool = false;
    let live_v33_b31: u16 = ALL & zb_holds(n1684);
    let ok_v34_b32: u16 = ALL & zb_holds(n1548);
    let bd_v34_b32: bool = false;
    let live_v34_b32: u16 = ALL & zb_holds(n1699);
    let ok_v36_b33: u16 = ALL & zb_holds(n1548);
    let bd_v36_b33: bool = false;
    let live_v36_b33: u16 = ALL & zb_holds(n1666);
    let ok_v37_b34: u16 = ALL & zb_holds(n1548);
    let bd_v37_b34: bool = false;
    let live_v37_b34: u16 = ALL & zb_holds(n1684);
    let ok_v38_b35: u16 = ALL & zb_holds(n1548);
    let bd_v38_b35: bool = false;
    let live_v38_b35: u16 = ALL & zb_holds(n1699);
    let ok_v40_b36: u16 = ALL & zb_holds(n1548);
    let bd_v40_b36: bool = false;
    let live_v40_b36: u16 = ALL & zb_holds(n1666);
    let ok_v41_b37: u16 = ALL & zb_holds(n1548);
    let bd_v41_b37: bool = false;
    let live_v41_b37: u16 = ALL & zb_holds(n1684);
    let ok_v42_b38: u16 = ALL & zb_holds(n1548);
    let bd_v42_b38: bool = false;
    let live_v42_b38: u16 = ALL & zb_holds(n1699);
    let ok_v48_b39: u16 = ALL & zb_holds(n1548);
    let bd_v48_b39: bool = false;
    let live_v48_b39: u16 = ALL & zb_holds(n1735);
    let ok_v49_b40: u16 = ALL & zb_holds(n1548);
    let bd_v49_b40: bool = false;
    let live_v49_b40: u16 = ALL & zb_holds(n1750);
    let ok_v50_b41: u16 = ALL & zb_holds(n1548);
    let bd_v50_b41: bool = false;
    let live_v50_b41: u16 = ALL & zb_holds(n1765);
    let ok_v52_b42: u16 = ALL & zb_holds(n1548);
    let bd_v52_b42: bool = false;
    let live_v52_b42: u16 = ALL & zb_holds(n1735);
    let ok_v53_b43: u16 = ALL & zb_holds(n1548);
    let bd_v53_b43: bool = false;
    let live_v53_b43: u16 = ALL & zb_holds(n1750);
    let ok_v54_b44: u16 = ALL & zb_holds(n1548);
    let bd_v54_b44: bool = false;
    let live_v54_b44: u16 = ALL & zb_holds(n1765);
    let ok_v56_b45: u16 = ALL & zb_holds(n1548);
    let bd_v56_b45: bool = false;
    let live_v56_b45: u16 = ALL & zb_holds(n1735);
    let ok_v57_b46: u16 = ALL & zb_holds(n1548);
    let bd_v57_b46: bool = false;
    let live_v57_b46: u16 = ALL & zb_holds(n1750);
    let ok_v58_b47: u16 = ALL & zb_holds(n1548);
    let bd_v58_b47: bool = false;
    let live_v58_b47: u16 = ALL & zb_holds(n1765);
    let ok_v0_b48: u16 = ALL & zb_holds(n1963);
    let bd_v0_b48: bool = false;
    let live_v0_b48: u16 = ALL & zb_holds(n1968);
    let sh0 = KShared0 {
        c42: n67,
        c88: r_c88,
        c43: r_c43,
    };
    let sh1 = KShared1 {
        c88: r_c88,
        c43: r_c43,
    };
    let sh2 = KShared2 {
        c39: n1223,
        c42: n67,
        c88: r_c88,
        c254: n257,
        c43: r_c43,
        c38: n1224,
    };
    let sh3 = KShared3 {
        c39: n1938,
        c20: n1939,
        c41: n1940,
        c42: n1941,
        c88: r_c88,
        c232: n1943,
        c233: n1944,
        c262: n1950,
        c263: n1951,
        c264: n1952,
        c265: n1953,
        c266: n1954,
        c267: n1955,
        c268: n1956,
        c269: n1957,
        c242: n1945,
        c270: n1958,
        c271: n1959,
        c244: n1946,
        c245: n1947,
        c272: n1960,
        c273: n1961,
        c248: n1948,
        c249: n1949,
        c43: r_c43,
        c38: n1942,
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
    // 49 distinct button assignments; per outcome they fall
    // into [2, 12, 24, 1] groups that write identical values.
    declined |= live_v0_b0 & !ok_v0_b0;
    take_0_0 |= live_v0_b0 & ok_v0_b0;
    declined |= live_v1_b1 & !ok_v1_b1;
    take_0_0 |= live_v1_b1 & ok_v1_b1;
    declined |= live_v2_b2 & !ok_v2_b2;
    take_0_0 |= live_v2_b2 & ok_v2_b2;
    declined |= live_v16_b3 & !ok_v16_b3;
    take_0_0 |= live_v16_b3 & ok_v16_b3;
    declined |= live_v17_b4 & !ok_v17_b4;
    take_0_0 |= live_v17_b4 & ok_v17_b4;
    declined |= live_v18_b5 & !ok_v18_b5;
    take_0_0 |= live_v18_b5 & ok_v18_b5;
    let o0 = KOut0 {
        c20: r_c20,
        c41: n66,
        h1: n1984, h2: n1985,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_0, &sh0, &o0);
    declined |= live_v32_b6 & !ok_v32_b6;
    take_0_1 |= live_v32_b6 & ok_v32_b6;
    declined |= live_v33_b7 & !ok_v33_b7;
    take_0_1 |= live_v33_b7 & ok_v33_b7;
    declined |= live_v34_b8 & !ok_v34_b8;
    take_0_1 |= live_v34_b8 & ok_v34_b8;
    declined |= live_v48_b9 & !ok_v48_b9;
    take_0_1 |= live_v48_b9 & ok_v48_b9;
    declined |= live_v49_b10 & !ok_v49_b10;
    take_0_1 |= live_v49_b10 & ok_v49_b10;
    declined |= live_v50_b11 & !ok_v50_b11;
    take_0_1 |= live_v50_b11 & ok_v50_b11;
    let o0 = KOut0 {
        c20: n1154,
        c41: n1155,
        h1: n1990, h2: n1991,
    };
    // body 11: buttons 0x32, forks 0x0
    sink.o0(50, take_0_1, &sh0, &o0);
    declined |= live_v0_b12 & !ok_v0_b12;
    take_1_0 |= live_v0_b12 & ok_v0_b12;
    let o1 = KOut1 {
        c39: n1252,
        c20: r_c20,
        c38: n1254,
        h1: n2002, h2: n2003,
    };
    // body 12: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b13 & !ok_v1_b13;
    take_1_1 |= live_v1_b13 & ok_v1_b13;
    let o1 = KOut1 {
        c39: n1284,
        c20: r_c20,
        c38: n1286,
        h1: n2008, h2: n2009,
    };
    // body 13: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b14 & !ok_v2_b14;
    take_1_2 |= live_v2_b14 & ok_v2_b14;
    let o1 = KOut1 {
        c39: n1313,
        c20: r_c20,
        c38: n1315,
        h1: n2014, h2: n2015,
    };
    // body 14: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b15 & !ok_v16_b15;
    take_1_3 |= live_v16_b15 & ok_v16_b15;
    let o1 = KOut1 {
        c39: n1351,
        c20: r_c20,
        c38: n1353,
        h1: n2020, h2: n2021,
    };
    // body 15: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b16 & !ok_v17_b16;
    take_1_4 |= live_v17_b16 & ok_v17_b16;
    let o1 = KOut1 {
        c39: n1389,
        c20: r_c20,
        c38: n1391,
        h1: n2026, h2: n2027,
    };
    // body 16: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b17 & !ok_v18_b17;
    take_1_5 |= live_v18_b17 & ok_v18_b17;
    let o1 = KOut1 {
        c39: n1427,
        c20: r_c20,
        c38: n1429,
        h1: n2032, h2: n2033,
    };
    // body 17: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b18 & !ok_v32_b18;
    take_1_6 |= live_v32_b18 & ok_v32_b18;
    let o1 = KOut1 {
        c39: n1441,
        c20: n1154,
        c38: n1443,
        h1: n2040, h2: n2041,
    };
    // body 18: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b19 & !ok_v33_b19;
    take_1_7 |= live_v33_b19 & ok_v33_b19;
    let o1 = KOut1 {
        c39: n1457,
        c20: n1154,
        c38: n1459,
        h1: n2046, h2: n2047,
    };
    // body 19: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b20 & !ok_v34_b20;
    take_1_8 |= live_v34_b20 & ok_v34_b20;
    let o1 = KOut1 {
        c39: n1473,
        c20: n1154,
        c38: n1475,
        h1: n2052, h2: n2053,
    };
    // body 20: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v48_b21 & !ok_v48_b21;
    take_1_9 |= live_v48_b21 & ok_v48_b21;
    let o1 = KOut1 {
        c39: n1489,
        c20: n1154,
        c38: n1491,
        h1: n2058, h2: n2059,
    };
    // body 21: buttons 0x30, forks 0x0
    sink.o1(48, take_1_9, &sh1, &o1);
    declined |= live_v49_b22 & !ok_v49_b22;
    take_1_10 |= live_v49_b22 & ok_v49_b22;
    let o1 = KOut1 {
        c39: n1505,
        c20: n1154,
        c38: n1507,
        h1: n2064, h2: n2065,
    };
    // body 22: buttons 0x31, forks 0x0
    sink.o1(49, take_1_10, &sh1, &o1);
    declined |= live_v50_b23 & !ok_v50_b23;
    take_1_11 |= live_v50_b23 & ok_v50_b23;
    let o1 = KOut1 {
        c39: n1521,
        c20: n1154,
        c38: n1523,
        h1: n2070, h2: n2071,
    };
    // body 23: buttons 0x32, forks 0x0
    sink.o1(50, take_1_11, &sh1, &o1);
    declined |= live_v0_b24 & !ok_v0_b24;
    take_2_0 |= live_v0_b24 & ok_v0_b24;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n66,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1545,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n1546,
        c253: n1565,
        h1: n2122, h2: n2123,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b25 & !ok_v1_b25;
    take_2_1 |= live_v1_b25 & ok_v1_b25;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n66,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n270,
        c239: n1545,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1580,
        c281: n1546,
        c253: n1565,
        h1: n2130, h2: n2131,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b26 & !ok_v2_b26;
    take_2_2 |= live_v2_b26 & ok_v2_b26;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n66,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1545,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1594,
        c281: n1546,
        c253: n1565,
        h1: n2135, h2: n2136,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b27 & !ok_v16_b27;
    take_2_3 |= live_v16_b27 & ok_v16_b27;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n66,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1596,
        c246: zb_splat(false),
        c247: n270,
        c280: n1608,
        c281: n1598,
        c253: n1565,
        h1: n2160, h2: n2161,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b28 & !ok_v17_b28;
    take_2_4 |= live_v17_b28 & ok_v17_b28;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n66,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n270,
        c239: n1596,
        c246: zb_splat(false),
        c247: n270,
        c280: n1620,
        c281: n1598,
        c253: n1565,
        h1: n2167, h2: n2168,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b29 & !ok_v18_b29;
    take_2_5 |= live_v18_b29 & ok_v18_b29;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n66,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1596,
        c246: zb_splat(false),
        c247: n270,
        c280: n1632,
        c281: n1598,
        c253: n1565,
        h1: n2172, h2: n2173,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b30 & !ok_v32_b30;
    take_2_6 |= live_v32_b30 & ok_v32_b30;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1647,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1665,
        c281: n1651,
        c253: n1664,
        h1: n2215, h2: n2216,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b31 & !ok_v33_b31;
    take_2_7 |= live_v33_b31 & ok_v33_b31;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1647,
        c269: n1648,
        c234: n1667,
        c270: n1671,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1645,
        c237: n1646,
        c272: n270,
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1683,
        c281: n1651,
        c253: n1664,
        h1: n2227, h2: n2228,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b32 & !ok_v34_b32;
    take_2_8 |= live_v34_b32 & ok_v34_b32;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1647,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1698,
        c281: n1651,
        c253: n1664,
        h1: n2232, h2: n2233,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b33 & !ok_v36_b33;
    take_2_9 |= live_v36_b33 & ok_v36_b33;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1647,
        c234: n1667,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1703,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n1704,
        c253: n1664,
        h1: n2248, h2: n2249,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b34 & !ok_v37_b34;
    take_2_10 |= live_v37_b34 & ok_v37_b34;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1671,
        c271: n1703,
        c236: n1645,
        c237: n1646,
        c272: n270,
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1710,
        c281: n1708,
        c253: n1664,
        h1: n2262, h2: n2263,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b35 & !ok_v38_b35;
    take_2_11 |= live_v38_b35 & ok_v38_b35;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: n1703,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1714,
        c281: n1708,
        c253: n1664,
        h1: n2273, h2: n2274,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b36 & !ok_v40_b36;
    take_2_12 |= live_v40_b36 & ok_v40_b36;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1647,
        c234: n1667,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1649,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n1716,
        c253: n1664,
        h1: n2282, h2: n2283,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b37 & !ok_v41_b37;
    take_2_13 |= live_v41_b37 & ok_v41_b37;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1671,
        c271: n1649,
        c236: n1645,
        c237: n1646,
        c272: n270,
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1710,
        c281: n1718,
        c253: n1664,
        h1: n2291, h2: n2292,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b38 & !ok_v42_b38;
    take_2_14 |= live_v42_b38 & ok_v42_b38;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: n1649,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1545,
        c246: n270,
        c247: zb_splat(false),
        c280: n1714,
        c281: n1718,
        c253: n1664,
        h1: n2299, h2: n2300,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b39 & !ok_v48_b39;
    take_2_15 |= live_v48_b39 & ok_v48_b39;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1647,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1734,
        c281: n1723,
        c253: n1664,
        h1: n2323, h2: n2324,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b40 & !ok_v49_b40;
    take_2_16 |= live_v49_b40 & ok_v49_b40;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1647,
        c269: n1648,
        c234: n1667,
        c270: n1671,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1645,
        c237: n1646,
        c272: n270,
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1749,
        c281: n1723,
        c253: n1664,
        h1: n2334, h2: n2335,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b41 & !ok_v50_b41;
    take_2_17 |= live_v50_b41 & ok_v50_b41;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1647,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1764,
        c281: n1723,
        c253: n1664,
        h1: n2339, h2: n2340,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b42 & !ok_v52_b42;
    take_2_18 |= live_v52_b42 & ok_v52_b42;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1647,
        c234: n1667,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1703,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1771,
        c281: n1769,
        c253: n1664,
        h1: n2355, h2: n2356,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b43 & !ok_v53_b43;
    take_2_19 |= live_v53_b43 & ok_v53_b43;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1671,
        c271: n1703,
        c236: n1645,
        c237: n1646,
        c272: n270,
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1777,
        c281: n1775,
        c253: n1664,
        h1: n2369, h2: n2370,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b44 & !ok_v54_b44;
    take_2_20 |= live_v54_b44 & ok_v54_b44;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: n1703,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1781,
        c281: n1775,
        c253: n1664,
        h1: n2380, h2: n2381,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b45 & !ok_v56_b45;
    take_2_21 |= live_v56_b45 & ok_v56_b45;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1647,
        c234: n1667,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1649,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1771,
        c281: n1783,
        c253: n1664,
        h1: n2389, h2: n2390,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b46 & !ok_v57_b46;
    take_2_22 |= live_v57_b46 & ok_v57_b46;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1671,
        c271: n1649,
        c236: n1645,
        c237: n1646,
        c272: n270,
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1777,
        c281: n1785,
        c253: n1664,
        h1: n2398, h2: n2399,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b47 & !ok_v58_b47;
    take_2_23 |= live_v58_b47 & ok_v58_b47;
    let o2 = KOut2 {
        c20: n1642,
        c41: n1643,
        c268: n1648,
        c269: n1648,
        c234: n1667,
        c270: n1649,
        c271: n1649,
        c236: n1645,
        c237: n1646,
        c272: zb_splat(false),
        c239: n1596,
        c246: n270,
        c247: n270,
        c280: n1781,
        c281: n1785,
        c253: n1664,
        h1: n2406, h2: n2407,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined |= live_v0_b48 & !ok_v0_b48;
    take_3_0 |= live_v0_b48 & ok_v0_b48;
    let o3 = KOut3 {
        h1: n2482, h2: n2483,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
